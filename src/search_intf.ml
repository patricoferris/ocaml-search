module type Indexer_generalised = sig
  type ('k, 'a) uid
  type ('v, 'a) doc
  type ('k, 'v) t

  val index :
    ('k, 'v) t -> uid:('k, 'a) uid -> token:string -> ('v, 'a) doc -> unit
  (** [index t doc uid] indexes a given document [doc] in [t] with a
      unique identifier [uid]. *)

  val add_document : ('k, 'v) t -> ('k, 'a) uid -> ('v, 'a) doc -> unit
  (** Adds a new document to the indexer *)

  val add_index : ('k, 'v) t -> (('v, _) doc -> string) -> unit
  (** Adds a new index and re-indexes everything. *)

  val search : (_, 'doc) t -> string -> ('doc, 'a) doc list

  val empty :
    ?santiser:(string -> string) ->
    ?strategy:(string -> string list) ->
    ?tokeniser:(string -> string list) ->
    unit ->
    ('k, 'v) t

  val pp : Format.formatter -> (_, _) t -> unit
end

module type Mono = sig
  type doc
  type uid
  type t

  (** @inline *)
  include
    Indexer_generalised
      with type (_, 'v) t := t
       and type (_, _) uid := uid
       and type ('v, _) doc := doc
end

module type Uid = sig
  type t

  include Map.OrderedType with type t := t

  val to_string : t -> string
end

module type Generic = sig
  type t
  type key

  module Witness : sig
    type _ witness = ..

    module type Tid = sig
      type t
      type _ witness += Tid : t witness
    end

    type 'a t = (module Tid with type t = 'a)
  end

  module Uid_key : sig
    type 'v uid_key

    val create : unit -> 'v uid_key
    val tid : 'v uid_key -> 'v Witness.t

    type t

    val hide_type : 'v uid_key -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end

  type 'v uid = 'v Uid_key.uid_key
  type binding = KV : ('v uid * 'v) -> binding
  type doc = binding

  val index : t -> uid:'doc uid -> token:string -> doc -> unit
  (** [index t doc uid] indexes a given document [doc] in [t] with a
      unique identifier [uid]. *)

  val add_document : t -> 'doc uid -> key -> 'doc -> unit
  (** Adds a new document to the indexer *)

  val apply : 'v uid -> default:'a -> ('v -> 'a) -> doc -> 'a
  val apply_exn : 'v uid -> ('v -> 'a) -> doc -> 'a

  val add_index : t -> 'doc uid -> ('doc -> string) -> unit
  (** Adds a new index and re-indexes everything. *)

  val search : t -> string -> binding list

  val empty :
    ?santiser:(string -> string) ->
    ?strategy:(string -> string list) ->
    ?tokeniser:(string -> string list) ->
    unit ->
    t

  val pp : Format.formatter -> t -> unit
end

type ('uid, 'doc) mono_index =
  | Mono :
      (module Mono with type t = 'index and type doc = 'doc and type uid = 'uid)
      * 'index
      -> ('uid, 'doc) mono_index

type 'key generic_index =
  | General :
      (module Generic with type t = 'index and type key = 'key) * 'index
      -> 'key generic_index

module type S = sig
  module Tfidf : sig
    module M
        (Uid : Uid) (Doc : sig
          type t
        end) : Mono with type doc = Doc.t and type uid = Uid.t

    module G (Uid : Uid) : Generic with type key = Uid.t
  end

  val create_uid :
    to_string:('uid -> string) ->
    cmp:('uid -> 'uid -> int) ->
    (module Uid with type t = 'uid)

  (* module Mono : sig
       type ('uid, 'doc) t
       (** A search index for documents of type ['doc]. *)

       val search : ('uid, 'doc) t -> string -> 'doc list
       val add_index : ('uid, 'doc) t -> ('doc -> string) -> unit
       val add_document : ('uid, 'doc) t -> 'uid -> 'doc -> unit

       val create :
         ?santiser:(string -> string) ->
         ?strategy:(string -> string list) ->
         ?tokeniser:(string -> string list) ->
         ('uid, 'doc) mono_index ->
         ('doc -> 'uid) ->
         ('uid, 'doc) t

       val create_mono :
         ?santiser:(string -> string) ->
         ?strategy:(string -> string list) ->
         ?tokeniser:(string -> string list) ->
         ('doc -> 'uid) ->
         (module Uid with type t = 'uid) ->
         ('uid, 'doc) t
     end

     module Generic (Uid : Uid) : sig
       type 'key t
       (** A generic search index *)

       val create :
         ?santiser:(string -> string) ->
         ?strategy:(string -> string list) ->
         ?tokeniser:(string -> string list) ->
         (module Uid with type t = 'key) ->
         'key t

       val add_index : 'key t -> ('doc -> string) -> unit
     end *)

  module Uids : sig
    module String : Uid with type t = string
    module Int : Uid with type t = int
  end
end

module type Sigs = sig
  module type Uid = Uid
  module type S = S

  include S
end
