module type Mono = sig
  type doc
  (** The type of documents we will search over *)

  type uid
  (** The type of unique identifiers we will use to identify distinct
      documents *)

  type t
  (** The search index *)

  val index : t -> uid:uid -> token:string -> doc -> unit
  (** [index t doc uid] indexes a given document [doc] in [t] with a
      unique identifier [uid]. *)

  val add_document : t -> uid -> doc -> unit
  (** Adds a new document to the indexer *)

  val add_index : t -> (doc -> string) -> unit
  (** Adds a new index and re-indexes everything. *)

  val add_indexes : t -> (doc -> string) list -> unit
  (** Same as {! add_index} but allows you to add multiple indexes
      at a time before re-indexing occurs. *)

  val search : t -> string -> doc list
  (** [search t k] searches for [t] using [k]. *)

  val empty :
    ?santiser:(string -> string) ->
    ?strategy:(string -> string list) ->
    ?tokeniser:(string -> string list) ->
    unit ->
    t
  (** Create a new empty search index.

     @param sanitiser Run on each token to normalise them, by default this is {! String.lowercase_ascii}
     @param strategy The indexing strategy, by default this is a prefixing strategy such that [abc] is indexed with [a], [ab] and [abc]
     @param tokeniser Turns your documents into tokens. *)

  val pp : Format.formatter -> t -> unit
  (** Dumps the index, mainly for debugging or testing. *)
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

  module Uid : sig
    type 'v witness

    val create : unit -> 'v witness
    val tid : 'v witness -> 'v Witness.t

    type t

    val hide_type : 'v witness -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end

  type 'v uid = 'v Uid.witness
  (** A value of type ['v uid] can be used to uniquely identify documents of type ['a]. *)

  type binding =
    | KV : ('v uid * 'v) -> binding
        (** A [binding] is returned when searching in a heterogeneous search index. *)

  type doc = binding
  (** Documents are bindings. *)

  val index : t -> uid:'doc uid -> token:string -> doc -> unit
  (** [index t doc uid] indexes a given document [doc] in [t] with a
      unique identifier [uid]. *)

  val add_document : t -> 'doc uid -> key -> 'doc -> unit
  (** Adds a new document to the indexer *)

  val apply : 'v uid -> default:'a -> ('v -> 'a) -> doc -> 'a
  (** [apply uid ~default fn doc] runs the function [fn] on [doc]
      if [uid] identifies the types as being the same, otherwise
      it returns [default]. *)

  val apply_exn : 'v uid -> ('v -> 'a) -> doc -> 'a
  (** Like {! apply} except without a default return value so it may raise
      [Invalid_argument _]. *)

  val add_index : t -> 'doc uid -> ('doc -> string) -> unit
  (** Adds a new index and re-indexes everything. *)

  val add_indexes : t -> 'doc uid -> ('doc -> string) list -> unit
  (** Same as {! add_index} but allows you to add multiple indexes
      at a time before re-indexing occurs. *)

  val search : t -> string -> binding list
  (** [search t k] searches the index [t] using [k] returning the possible bindings. *)

  val empty :
    ?santiser:(string -> string) ->
    ?strategy:(string -> string list) ->
    ?tokeniser:(string -> string list) ->
    unit ->
    t
  (** Create a new empty search index.

     @param sanitiser Run on each token to normalise them, by default this is {! String.lowercase_ascii}
     @param strategy The indexing strategy, by default this is a prefixing strategy such that [abc] is indexed with [a], [ab] and [abc]
     @param tokeniser Turns your documents into tokens. *)

  val pp : Format.formatter -> t -> unit
  (** Dumps the search index. *)
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

module type Sigs = sig
  (** {1 Unique Identifiers} *)

  module type Uid = Uid

  (** {1 Search Index Implementations} *)

  (** {2 Term Frequency Inverse Document Frequency}

        This search index uses the {{: https://en.wikipedia.org/wiki/Tf–idf} tf-idf}
        approach to searching.*)

  module Tfidf : sig
    (** A functor for building a Tfidf search index over one type of document. *)
    module Mono
        (Uid : Uid) (Doc : sig
          type t
        end) : Mono with type doc = Doc.t and type uid = Uid.t

    (** A functor for building a Tfidf search index over different types of document. *)
    module Generic (Uid : Uid) : Generic with type key = Uid.t
  end

  (** {1 Useful UID implementations} *)

  val create_uid :
    to_string:('uid -> string) ->
    cmp:('uid -> 'uid -> int) ->
    (module Uid with type t = 'uid)
  (** [create_uid ~to_string ~cmp] allows you to create a {! Uid} module
      from the provided functions. *)

  module Uids : sig
    module String : Uid with type t = string
    module Int : Uid with type t = int
  end

  module Private : sig
    module Witness = Witness
  end
end
