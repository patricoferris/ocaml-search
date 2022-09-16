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

module type Mono_indexer = sig
  type doc
  type t

  (** @inline *)
  include
    Indexer_generalised
      with type (_, 'v) t := t
       and type (_, _) uid := string
       and type ('v, _) doc := doc
end

type 'doc mono_index =
  | Indexer :
      (module Mono_indexer with type t = 'index and type doc = 'doc) * 'index
      -> 'doc mono_index

module type S = sig
  module Tfidf (Arg : sig
    type t
  end) : Mono_indexer with type doc = Arg.t

  type 'doc t
  (** A search index for documents of type ['doc]. *)

  val search : 'doc t -> string -> 'doc list
  val add_index : 'doc t -> ('doc -> string) -> unit
  val add_document : 'doc t -> 'doc -> unit

  val create :
    ?santiser:(string -> string) ->
    ?strategy:(string -> string list) ->
    ?tokeniser:(string -> string list) ->
    ('doc -> string) ->
    'doc t
  (** [create uid] is a new search index using [uid] to uniquely identify documents.*)
end

module type Sigs = sig
  module type S = S

  include S
end
