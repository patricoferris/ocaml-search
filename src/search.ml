include Search_intf

module Tfidf
    (Uid : Uid) (Doc : sig
      type t
    end) =
struct
  module TokenMap = Map.Make (String)
  module UidMap = Map.Make (Uid)

  type uid = Uid.t
  type doc = Doc.t

  type t = {
    mutable token_map : stats TokenMap.t;
    mutable cache : float TokenMap.t;
    uid : doc -> uid;
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
  let empty uid = { token_map = TokenMap.empty; cache = TokenMap.empty; uid }

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
            let uid = t.uid doc in
            match UidMap.find_opt uid stats.uid_map with
            | None -> 0.
            | Some (_, v) -> float_of_int v)
      in
      score +. (freq *. inverse)
    in
    List.fold_left f 0. tokens

  let score_cmp v = int_of_float v

  let search t tokens docs =
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
    let tf = tfidf t tokens docs in
    List.sort (fun d1 d2 -> score_cmp @@ (tf d2 -. tf d1)) documents
end

type ('uid, 'doc) t = {
  mutable documents : 'doc list;
  mutable indexes : ('doc -> string) list;
  uid : 'doc -> 'uid;
  index : ('uid, 'doc) index;
  strategy : string -> string list;
  tokeniser : string -> string list;
  santiser : string -> string;
}

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

let create_mono (type uid doc) ?(santiser = String.lowercase_ascii)
    ?(strategy = prefix_stratgey) ?(tokeniser = String.split_on_char ' ')
    (index : (uid, doc) index) (uid : doc -> uid) =
  { documents = []; indexes = []; uid; index; strategy; santiser; tokeniser }

let create_uid (type uid) ~(to_string : uid -> string)
    ~(cmp : uid -> uid -> int) =
  let module T = struct
    type t = uid

    let to_string = to_string
    let compare = cmp
  end in
  (module T : Uid with type t = uid)

let create (type uid doc) ?santiser ?strategy ?tokeniser (uid : doc -> uid)
    (module Uid : Uid with type t = uid) =
  let module T = struct
    include
      Tfidf
        (Uid)
        (struct
          type t = doc
        end)
  end in
  create_mono ?santiser ?strategy ?tokeniser
    (Mono
       ( (module T : Mono
           with type doc = doc
            and type t = T.t
            and type uid = uid),
         T.empty uid ))
    uid

let add_document (type uid doc) (t : (uid, doc) t) doc =
  t.documents <- doc :: t.documents;
  t.index |> function
  | Mono ((module I), index) ->
      let uid = t.uid doc in
      let fields = List.fold_left (fun acc i -> i doc :: acc) [] t.indexes in
      List.iter
        (fun field ->
          List.iter
            (fun token -> I.index index ~uid ~token doc)
            (t.strategy (t.santiser field)))
        fields
  | _ -> assert false

let add_index t index =
  t.indexes <- index :: t.indexes;
  let docs = t.documents in
  t.documents <- [];
  List.iter (add_document t) docs

let search (type uid doc) (t : (uid, doc) t) s =
  let tokens = t.tokeniser (t.santiser s) in
  t.index |> function
  | Mono ((module I), indexer) -> I.search indexer tokens t.documents
  | _ -> assert false

module Uids = struct
  module String = struct
    type t = string

    let to_string = Fun.id
    let compare = String.compare
  end

  module Int = struct
    type t = int

    let to_string = string_of_int
    let compare = Int.compare
  end
end
