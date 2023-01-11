# TFIDF Search

Setup some of the code.

```ocaml
module Book = struct
  type t = {
    title : string;
    author : string;
  }
end

let books : Book.t list =
  [
    { title = "Dune"; author = "Frank Herbet" };
    { title = "The day of the triffids"; author = "John Wyndham" };
    { title = "The Remains of the Day"; author = "Kazuo Ishiguro" }
  ]

module Mono = Search.Tfidf.Mono (Search.Uids.String) (Book)
let search = Mono.empty ()

let () =
  Mono.add_index search (fun t -> t.title);
  List.iter (fun doc -> Mono.add_document search doc.Book.title doc) books
```

Tokenisation works correctly.

```ocaml
# Mono.search search "day";;
- : Mono.doc list =
[{Book.title = "The Remains of the Day"; author = "Kazuo Ishiguro"};
 {Book.title = "The day of the triffids"; author = "John Wyndham"}]
# Mono.search search "of the";;
- : Mono.doc list =
[{Book.title = "The Remains of the Day"; author = "Kazuo Ishiguro"};
 {Book.title = "The day of the triffids"; author = "John Wyndham"}]
# Mono.search search "dUnE";;
- : Mono.doc list = [{Book.title = "Dune"; author = "Frank Herbet"}]
```
