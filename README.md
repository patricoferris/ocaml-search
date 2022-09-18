ocaml-search
------------

A very simple, search library for OCaml heavily inspired by [js-search](https://github.com/bvaughn/js-search).

<!-- TOC -->

    - [ocaml-search](#ocaml-search)
- [Usage](#usage)
    - [Monomorphic Search Indexes](#monomorphic-search-indexes)
        - [Unique Identifiers](#unique-identifiers)
        - [Documents](#documents)
    - [Heterogeneous Search Indexes](#heterogeneous-search-indexes)

<!-- /TOC -->

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

The module only need to provide a type `t` and a `compare` and `to_string` function. `Search.Uids` contains some common modules for your convenience.

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

module M = Search.Tfidf.M (Search.Uids.String) (Doc)
```

Search indexes come with the `empty` function which creates a new index.

```ocaml
# let search = M.empty () ;;
val search : M.t = <abstr>
```

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

Note that this implementation uses TFIDF, if we were to also add the `nick` as an index, then `"Alan"` will come to the top. Adding a new index after already adding documents causes the documents to be re-indexed!

```ocaml
# M.add_index search (fun t -> t.nick);
  M.search search "Al";;
- : M.doc list =
[{Doc.uid = "1"; name = "Alan"; nick = "Al"; age = 12};
 {Doc.uid = "0"; name = "Alice"; nick = ""; age = 10}]
```
## Heterogeneous Search Indexes
