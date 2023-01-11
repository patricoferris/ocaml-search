# ocaml-search
--------------

A very simple, search library for OCaml heavily inspired by [js-search](https://github.com/bvaughn/js-search), originally by [Craigfe's posts](https://www.craigfe.io/posts/) and [Hmap](https://erratique.ch/repos/hmap).

- [Usage](#usage)
    - [Monomorphic Search Indexes](#monomorphic-search-indexes)
        - [Unique Identifiers](#unique-identifiers)
        - [Documents](#documents)
    - [Heterogeneous Search Indexes](#heterogeneous-search-indexes)
        - [Type Witness](#type-witness)
        - [Generic Interface](#generic-interface)
        - [Adding Indexes](#adding-indexes)
        - [Adding Documents](#adding-documents)
        - [Searching](#searching)

# Usage

The following is a quick guide on how to use this library. It is not particularly optimised, memory efficient or tested. Use with care!

```ocaml
# #require "search";;
```

## Monomorphic Search Indexes

[Monomorphism is the opposite of polymorphism](https://wiki.haskell.org/Monomorphism). Here, we mean that your search index will only work for one type of document. This is provided via a functor along with the unique identifier module.

### Unique Identifiers

Unique identifiers should uniquely identify a document amongst other documents.

```ocaml
# #show_module_type Search__Search_intf.Uid;;
module type Uid =
  sig type t val compare : t -> t -> int val to_string : t -> string end
```

The module only needs to provide a type `t` and a `compare` and `to_string` function. `Search.Uids` contains some common modules for your convenience.

### Documents

Every search index is specialised to some document type. For example, it could be a record representing people.

```ocaml
module Doc = struct
type t = {
  uid : string;
  name : string;
  nick : string;
  age : int;
}
let docs = [
{
    uid = "0";
    name = "Alice";
    nick = "";
    age = 10;
  };
  {
    uid = "1";
    name = "Alan";
    nick = "Al";
    age = 12;
  };
  {
    uid = "2";
    name = "William";
    nick = "Bob";
    age = 13;
  }
]
end

module M = Search.Tfidf.Mono (Search.Uids.String) (Doc)
```

Search indexes come with the `empty` function which creates a new index.

```ocaml
# M.empty;;
- : ?santiser:(string -> string) ->
    ?strategy:(string -> string list) ->
    ?tokeniser:(string -> string list) -> unit -> M.t
= <fun>
# let search = M.empty () ;;
val search : M.t = <abstr>
```

There are three optional functions you can add to change the how documents are treated. The `tokeniser` splits a string into tokens. By default this is just by whitespace. The `sanitiser` creates a uniform representation of strings, by default `String.lowercase_ascii`. Finally, `stratgey` is the indexing strategy. By default this is a prefixing strategy such that `abc` is indexed with `a`, `ab` and `abc`.

From here you add the indexes. These are the functions from your document to a string that will be used to search for documents matching some string later.

```ocaml
# M.add_index search (fun t -> t.name);;
- : unit = ()
```

After you've added all your indexes, you can add some documents.

```ocaml
# List.iter (fun d -> M.add_document search d.Doc.uid d) Doc.docs;;
- : unit = ()
```

At which point you are ready to search!

```ocaml
# M.search search "Al";;
- : M.doc list =
[{Doc.uid = "0"; name = "Alice"; nick = ""; age = 10};
 {Doc.uid = "1"; name = "Alan"; nick = "Al"; age = 12}]
```

Note that this implementation uses [TFIDF](https://en.wikipedia.org/wiki/Tf%E2%80%93idf), if we were to also add the `nick` as an index, then `"Alan"` will come to the top. Adding a new index after already adding documents causes the documents to be re-indexed!

```ocaml
# M.add_index search (fun t -> t.nick);
  M.search search "Al";;
- : M.doc list =
[{Doc.uid = "1"; name = "Alan"; nick = "Al"; age = 12};
 {Doc.uid = "0"; name = "Alice"; nick = ""; age = 10}]
```
## Heterogeneous Search Indexes

Heterogeneous search indexes allow you to store more than one type in the index. This is based on [Janestreet's Universal Map](https://github.com/janestreet/core/blob/master/core/src/univ_map.ml) and [Hmap](https://erratique.ch/repos/hmap).

### Type Witness

The main difference when programming with heterogeneous indexes is that you must provide a type witness when adding indexes and adding documents. A type witness is essentially a value that can be used to check the type of another value at runtime.

Search provides a low-level type witness module.

```ocaml
# module W = Search.Private.Witness;;
module W = Search.Private.Witness
# let int_witness : int W.t = W.make ();;
val int_witness : int W.t = <module>
# let float_witness : float W.t = W.make ();;
val float_witness : float W.t = <module>
```

Here we've constructed two witnesses, one for integers and one for floats.

```ocaml
# W.eq int_witness int_witness;;
- : (int, int) W.teq option = Some W.Teq
# W.eq int_witness float_witness;;
- : (int, float) W.teq option = None
```

### Generic Interface

The interface is very similar to that of the monomorphic search index. With the `Tfidf` implementation, we only need to provide a unique identifier for documents. The type witness will take care of differentiating the different kinds of documents.

```ocaml
module G = Search.Tfidf.Generic (Search.Uids.String)
module Cat = struct
  type t = { name : string; lives : int }
end
module Dog = struct
  type t = { name : string; kind : string }
end
```

Creating a new index is straightforward.


```ocaml
# let search = G.empty ();;
val search : G.t = <abstr>
```

Generic search indexes must wrap the user-supplied unique identifier (which differentiates documents) to also differentiate the different kinds of documents. The user must generate the type witnesses using `Generic.Uid`.

```ocaml
# let cat : Cat.t G.uid = G.Uid.create ();;
val cat : Cat.t G.uid = <abstr>
# let dog : Dog.t G.uid = G.Uid.create ();;
val dog : Dog.t G.uid = <abstr>
```

### Adding Indexes

To add an index, you must also specify the kind of document you wish the index to be used for.

```ocaml
# G.add_index;;
- : G.t -> 'doc G.uid -> ('doc -> string) -> unit = <fun>
```

This allows you to access the type from within your index.

```ocaml
# G.add_index search cat (fun c -> c.Cat.name);
  G.add_index search dog (fun c -> c.Dog.name);;
- : unit = ()
```

### Adding Documents

When adding documents you provide the type witness for the kind of document you are adding along with a unique identifier for that document.

```ocaml
let add_cat c = G.add_document search cat c.Cat.name c
let add_dog d = G.add_document search dog d.Dog.name d
```

With these helper functions we can add some new documents.

```ocaml
# add_cat Cat.{ name = "Alice"; lives = 9 };
  add_dog Dog.{ name = "Alan"; kind = "Irish Setter" };;
- : unit = ()
```

### Searching

Whenever you search for a collection of documents in a heterogenous search index you will get back a list of documents of different kinds. These are wrapped up in `binding` to hide the fact they are of different kinds.

```ocaml
# #show_type G.binding;;
type nonrec binding = G.doc = KV : ('v G.uid * 'v) -> G.doc
```

This means if you want to access a document you'll need to prove you know what kind it is first! There's a little helper function for doing that.

```ocaml
# G.apply;;
- : 'v G.uid -> default:'a -> ('v -> 'a) -> G.doc -> 'a = <fun>
```

`G.apply uid ~default f doc` will apply the function `f` to a document `doc` provided it of kind `uid`. If it is not that kind then the `default` value will be returned.

```ocaml
# let docs = G.search search "Al";;
val docs : G.doc list = [G.KV (<abstr>, <poly>); G.KV (<abstr>, <poly>)]
```

We'll use `G.apply` to get the names of the animals.

```ocaml
# List.filter_map
    (fun t ->
       G.apply
       cat
       ~default:(G.apply dog ~default:None (fun d -> Some (d.Dog.name ^ " (the dog)")) t)
       (fun c -> Some (c.Cat.name ^ " (the cat)")) t) docs;;
- : string list = ["Alan (the dog)"; "Alice (the cat)"]
```
