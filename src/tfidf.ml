let prefix_stratgey s =
  let chars = String.to_seq s in
  let s =
    Seq.fold_left
      (fun (prev, acc) c ->
        let c = prev ^ String.make 1 c in
        (c, c :: acc))
      ("", []) chars
  in
  snd s |> List.rev

module M
    (Uid : Search_intf.Uid) (Doc : sig
      type t
    end) =
struct
  module TokenMap = Map.Make (String)
  module UidMap = Map.Make (Uid)

  type uid = Uid.t
  type key = Uid.t
  type doc = Doc.t

  type t = {
    mutable indexes : (Doc.t -> string) list;
    mutable documents : (Uid.t * Doc.t) list;
    mutable token_map : stats TokenMap.t;
    mutable cache : float TokenMap.t;
    strategy : string -> string list;
    tokeniser : string -> string list;
    santiser : string -> string;
  }

  and stats = {
    document_occurences : int;
    total_occurences : int;
    mutable uid_map : (doc * int) UidMap.t;
  }

  let pp_uid ppf (s, (_, i)) = Format.fprintf ppf "%s: %i" (Uid.to_string s) i

  let pp_stats ppf stats =
    let uids = UidMap.bindings stats.uid_map in
    Format.fprintf ppf "document: %i, total: %i, uid: %a"
      stats.document_occurences stats.total_occurences
      Format.(pp_print_list pp_uid)
      uids

  let pp_binding ppf (s, stats) = Format.fprintf ppf "%s: %a" s pp_stats stats

  let empty ?(santiser = String.lowercase_ascii) ?(strategy = prefix_stratgey)
      ?(tokeniser = String.split_on_char ' ') () =
    {
      indexes = [];
      documents = [];
      token_map = TokenMap.empty;
      cache = TokenMap.empty;
      strategy;
      santiser;
      tokeniser;
    }

  let get_documents t = t.documents
  let reset_documents t = t.documents <- []

  let pp ppf t =
    let b = TokenMap.bindings t.token_map in
    Format.fprintf ppf "[@.%a@.]@."
      Format.(pp_print_list ~pp_sep:pp_print_newline pp_binding)
      b

  let index t ~uid ~token doc =
    t.cache <- TokenMap.empty;
    let token_data =
      match TokenMap.find_opt token t.token_map with
      | Some stats ->
          { stats with total_occurences = stats.total_occurences + 1 }
      | None ->
          {
            document_occurences = 0;
            total_occurences = 1;
            uid_map = UidMap.empty;
          }
    in
    let token_data, uid_map =
      match UidMap.find_opt uid token_data.uid_map with
      | Some (doc, occurences) ->
          (token_data, UidMap.add uid (doc, occurences + 1) token_data.uid_map)
      | None ->
          ( {
              token_data with
              document_occurences = token_data.document_occurences + 1;
            },
            UidMap.add uid (doc, 1) token_data.uid_map )
    in
    token_data.uid_map <- uid_map;
    t.token_map <- TokenMap.add token token_data t.token_map

  let add_document t (uid : Uid.t) doc =
    t.documents <- (uid, doc) :: t.documents;
    let fields = List.fold_left (fun acc i -> i doc :: acc) [] t.indexes in
    List.iter
      (fun field ->
        List.iter
          (fun token -> index t ~uid ~token doc)
          (t.strategy (t.santiser field)))
      fields

  let add_index t index =
    t.indexes <- index :: t.indexes;
    let docs = t.documents in
    t.documents <- [];
    List.iter (fun (k, v) -> add_document t k v) docs

  let idf t token docs =
    match TokenMap.find_opt token t.cache with
    | Some i -> i
    | None ->
        let value =
          match TokenMap.find_opt token t.token_map with
          | Some stats -> float_of_int stats.document_occurences
          | None -> 0.
        in
        let i = 1. +. log (float_of_int (List.length docs) /. (1. +. value)) in
        t.cache <- TokenMap.add token i t.cache;
        i

  let tfidf t tokens docs doc =
    let f score token =
      let inverse = idf t token docs in
      let freq =
        match TokenMap.find_opt token t.token_map with
        | None -> 0.
        | Some stats -> (
            let uid = fst doc in
            match UidMap.find_opt uid stats.uid_map with
            | None -> 0.
            | Some (_, v) -> float_of_int v)
      in
      score +. (freq *. inverse)
    in
    List.fold_left f 0. tokens

  let score_cmp v = int_of_float v

  let search t token =
    let tokens = t.santiser token |> t.tokeniser in
    let documents = UidMap.empty in
    let i = ref (-1) in
    let document acc token =
      incr i;
      match TokenMap.find_opt token t.token_map with
      | None -> acc
      | Some stats ->
          let docs = if !i = 0 then stats.uid_map else documents in
          let map =
            UidMap.fold
              (fun uid (doc, _) docs ->
                if !i = 0 then UidMap.add uid doc docs
                else if not (UidMap.mem uid stats.uid_map) then
                  UidMap.remove uid docs
                else docs)
              docs documents
          in
          UidMap.union (fun _ v _ -> Some v) map acc
    in
    let uid_docs = List.fold_left document UidMap.empty tokens in
    let documents = UidMap.bindings uid_docs in
    let tf = tfidf t tokens t.documents in
    List.sort (fun d1 d2 -> score_cmp @@ (tf d2 -. tf d1)) documents
    |> List.map snd
end

(* Type identifiers.
   See http://alan.petitepomme.net/cwn/2015.03.24.html#1 and
   https://erratique.ch/repos/hmap/tree/src/hmap.ml *)
module G (U : Search_intf.Uid) = struct
  module TokenMap = Map.Make (String)
  module Witness = Witness

  module Uid = struct
    type 'a witness = { uid : U.t option; tid : 'a Witness.t }

    let tid v = v.tid

    let create () =
      let tid = Witness.make () in
      { uid = None; tid }

    type t = V : 'a witness -> t

    let hide_type k = V k

    let equal (V k0) (V k1) =
      U.compare (Option.get k0.uid) (Option.get k1.uid) = 0

    let compare (V k0) (V k1) =
      U.compare (Option.get k0.uid) (Option.get k1.uid)

    let to_string (V k) = U.to_string (Option.get k.uid)
  end

  type 'v uid = 'v Uid.witness
  type binding = KV : ('v uid * 'v) -> binding
  type doc = binding
  type key = U.t

  module UidMap = Map.Make (Uid)

  type t = {
    mutable indexes : (binding -> string option) list;
    mutable token_map : stats TokenMap.t;
    mutable cache : float TokenMap.t;
    mutable documents : binding UidMap.t;
    strategy : string -> string list;
    tokeniser : string -> string list;
    santiser : string -> string;
  }

  and stats = {
    document_occurences : int;
    total_occurences : int;
    mutable uid_map : (binding * int) UidMap.t;
  }

  let pp_uid ppf (s, (_, i)) = Format.fprintf ppf "%s: %i" (Uid.to_string s) i

  let pp_stats ppf stats =
    let uids = UidMap.bindings stats.uid_map in
    Format.fprintf ppf "document: %i, total: %i, uid: %a"
      stats.document_occurences stats.total_occurences
      Format.(pp_print_list pp_uid)
      uids

  let pp_binding ppf (s, stats) = Format.fprintf ppf "%s: %a" s pp_stats stats

  let empty ?(santiser = String.lowercase_ascii) ?(strategy = prefix_stratgey)
      ?(tokeniser = String.split_on_char ' ') () =
    {
      indexes = [];
      documents = UidMap.empty;
      token_map = TokenMap.empty;
      cache = TokenMap.empty;
      strategy;
      santiser;
      tokeniser;
    }

  let pp ppf t =
    let b = TokenMap.bindings t.token_map in
    Format.fprintf ppf "[@.%a@.]@."
      Format.(pp_print_list ~pp_sep:pp_print_newline pp_binding)
      b

  let index t ~uid ~token doc =
    t.cache <- TokenMap.empty;
    let token_data =
      match TokenMap.find_opt token t.token_map with
      | Some stats ->
          { stats with total_occurences = stats.total_occurences + 1 }
      | None ->
          {
            document_occurences = 0;
            total_occurences = 1;
            uid_map = UidMap.empty;
          }
    in
    let key = Uid.V uid in
    let token_data, uid_map =
      match UidMap.find_opt key token_data.uid_map with
      | Some (doc, occurences) ->
          (token_data, UidMap.add key (doc, occurences + 1) token_data.uid_map)
      | None ->
          ( {
              token_data with
              document_occurences = token_data.document_occurences + 1;
            },
            UidMap.add key (doc, 1) token_data.uid_map )
    in
    token_data.uid_map <- uid_map;
    t.token_map <- TokenMap.add token token_data t.token_map

  (* let apply (type v) (i : binding -> string) (doc : v) = *)

  let add_document t (type v) (uid : v uid) key (doc : v) =
    let uid = { uid with uid = Some key } in
    let uid_key = Uid.V uid in
    let binding = KV (uid, doc) in
    t.documents <- UidMap.add uid_key binding t.documents;
    let fields = List.fold_left (fun acc i -> i binding :: acc) [] t.indexes in
    List.iter
      (function
        | Some field ->
            List.iter
              (fun token -> index t ~uid ~token binding)
              (t.strategy (t.santiser field))
        | None -> ())
      fields

  let apply (type v) (k : v uid) ~default (f : v -> 'a) =
    let k = k.Uid.tid in
    fun (KV (k', v)) ->
      match Witness.eq k k'.tid with Some Teq -> f v | _ -> default

  let apply_exn (type v) (k : v uid) (f : v -> 'a) =
    let k = k.Uid.tid in
    fun (KV (k', v)) ->
      match Witness.eq k k'.tid with
      | Some Teq -> f v
      | _ -> failwith "Type witnesses were not equal."

  let add_index t k index =
    t.indexes <- apply ~default:None k (fun v -> Some (index v)) :: t.indexes;
    let docs = t.documents in
    t.documents <- UidMap.empty;
    UidMap.iter
      (fun _ (KV (k, v)) -> add_document t k (Option.get k.uid) v)
      docs

  type univ = Pack : 'a Witness.witness * 'a -> univ

  let int : int Witness.t = Witness.make ()

  let pack (type u) (module Witness : Witness.Tid with type t = u) x =
    Pack (Witness.Tid, x)

  let idf t token docs =
    match TokenMap.find_opt token t.cache with
    | Some i -> i
    | None ->
        let value =
          match TokenMap.find_opt token t.token_map with
          | Some stats -> float_of_int stats.document_occurences
          | None -> 0.
        in
        let i = 1. +. log (float_of_int (List.length docs) /. (1. +. value)) in
        t.cache <- TokenMap.add token i t.cache;
        i

  let tfidf t tokens docs doc =
    let f score token =
      let inverse = idf t token docs in
      let freq =
        match TokenMap.find_opt token t.token_map with
        | None -> 0.
        | Some stats -> (
            doc |> fun (KV (uid, _doc)) ->
            match UidMap.find_opt (Uid.V uid) stats.uid_map with
            | None -> 0.
            | Some (_, v) -> float_of_int v)
      in
      score +. (freq *. inverse)
    in
    List.fold_left f 0. tokens

  let score_cmp v = int_of_float v

  let search t token =
    let tokens = t.santiser token |> t.tokeniser in
    let documents = UidMap.empty in
    let i = ref (-1) in
    let document acc token =
      incr i;
      match TokenMap.find_opt token t.token_map with
      | None -> acc
      | Some stats ->
          let docs = if !i = 0 then stats.uid_map else documents in
          let map =
            UidMap.fold
              (fun uid (doc, _) docs ->
                if !i = 0 then UidMap.add uid doc docs
                else if not (UidMap.mem uid stats.uid_map) then
                  UidMap.remove uid docs
                else docs)
              docs documents
          in
          UidMap.union (fun _ v _ -> Some v) map acc
    in
    let uid_docs = List.fold_left document UidMap.empty tokens in
    let documents = UidMap.bindings uid_docs |> List.map snd in
    let tf = tfidf t tokens (UidMap.bindings t.documents) in
    List.sort (fun d1 d2 -> score_cmp @@ (tf d2 -. tf d1)) documents
end
