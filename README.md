ocaml-search
------------

A very simple, search library for OCaml heavily inspired by [js-search](https://github.com/bvaughn/js-search).

## Usage

The searcher is currently parameterised over three different, customisable components.

 - The tokeniser: how to tokenise a particular search field for your OCaml values. The default implementation being `String.split_on_char ' '`.
 - The sanitiser: how to make individual tokens and search queries more similar. By default this is `String.lowercase_ascii`.
 - The search index: how to actually perform the searching, by default this uses [JS-search's TFIDF implementation]().

### Default Search

```ocaml
# #require "search";;
```

Every search index is specialised to some document type. For example, it could be a record representing people.

```ocaml
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
```

Creating a search index only requires you to provide some function that uniquely identifies a document.

```ocaml
# let search = Search.create (fun t -> t.uid);;
val search : t Search.t = <abstr>
```

From here you add the indexes. These are the functions from your document to a string that will be used to search for documents matching some string later.

```ocaml
# Search.add_index search (fun t -> t.name);;
- : unit = ()
```

After you've added all your indexes, you can add some documents.

```ocaml
# List.iter (Search.add_document search) docs;;
- : unit = ()
```

At which point you are ready to search!

```ocaml
# Search.search search "Al";;
- : t list =
[{uid = "0"; name = "Alice"; nick = ""; age = 10};
 {uid = "1"; name = "Alan"; nick = "Al"; age = 12}]
```

Note that this implementation uses TFIDF, if we were to also add the `nick` as an index, then `"Alan"` will come to the top. Adding a new index after already adding documents causes the documents to be re-indexed!

```ocaml
# Search.add_index search (fun t -> t.nick);
  Search.search search "Al";;
- : t list =
[{uid = "1"; name = "Alan"; nick = "Al"; age = 12};
 {uid = "0"; name = "Alice"; nick = ""; age = 10}]
```
