module type Indexer_generalised = sig
  type ('k, 'a) uid
  type ('v, 'a) doc
  type ('k, 'v) t

  val index :
    ('k, 'v) t -> uid:('k, 'a) uid -> token:string -> ('v, 'a) doc -> unit
  (** [index t doc uid] indexes a given document [doc] in [t] with a
      unique identifier [uid]. *)

  val search : (_, 'doc) t -> string list -> 'doc list -> ('doc, 'a) doc list
  val empty : (('v, 'a) doc -> ('k, 'a) uid) -> ('k, 'v) t
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

module type Generic = sig
  type t
  type 'v uid

  (** @inline *)
  include
    Indexer_generalised
      with type (_, _) t := t
       and type (_, 'a) uid := 'a uid
       and type (_, 'a) doc := 'a
end

module type Uid = sig
  type t

  include Map.OrderedType with type t := t

  val to_string : t -> string
end

type ('uid, 'doc) index =
  | Mono :
      (module Mono with type t = 'index and type doc = 'doc and type uid = 'uid)
      * 'index
      -> ('uid, 'doc) index
  | General :
      (module Generic with type t = 'index) * 'index
      -> ('uid, 'doc) index

module type S = sig
  module Tfidf
      (Uid : Uid) (Doc : sig
        type t
      end) : Mono with type doc = Doc.t and type uid = Uid.t

  type ('uid, 'doc) t
  (** A search index for documents of type ['doc]. *)

  val search : ('uid, 'doc) t -> string -> 'doc list
  val add_index : ('uid, 'doc) t -> ('doc -> string) -> unit
  val add_document : ('uid, 'doc) t -> 'doc -> unit

  val create_uid :
    to_string:('uid -> string) ->
    cmp:('uid -> 'uid -> int) ->
    (module Uid with type t = 'uid)

  val create_mono :
    ?santiser:(string -> string) ->
    ?strategy:(string -> string list) ->
    ?tokeniser:(string -> string list) ->
    ('uid, 'doc) index ->
    ('doc -> 'uid) ->
    ('uid, 'doc) t

  val create :
    ?santiser:(string -> string) ->
    ?strategy:(string -> string list) ->
    ?tokeniser:(string -> string list) ->
    ('doc -> 'uid) ->
    (module Uid with type t = 'uid) ->
    ('uid, 'doc) t

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
