module Doc = struct
  type t = { uid : string; name : string; nick : string; age : int }

  let pp ppf { uid; name; nick; age } =
    Format.fprintf ppf "uid: %s, name: %s, nick: %s, age: %i" uid name nick age

  let docs =
    [
      { uid = "0"; name = "Alice"; nick = ""; age = 10 };
      { uid = "1"; name = "Alan"; nick = "Al"; age = 12 };
      { uid = "2"; name = "William"; nick = "Bob"; age = 13 };
    ]
end

module Mono = Search.Tfidf.M (Search.Uids.String) (Doc)
module Generic = Search.Tfidf.G (Search.Uids.String)

let only_name () =
  let search = Mono.empty () in
  Mono.add_index search (fun t -> t.name);
  List.iter (fun doc -> Mono.add_document search doc.Doc.uid doc) Doc.docs;
  let docs = Mono.search search "Al" in
  Format.(pp_print_list Doc.pp) Format.std_formatter docs

let with_nick () =
  let search = Mono.empty () in
  Mono.add_index search (fun t -> t.name);
  Mono.add_index search (fun t -> t.nick);
  List.iter (fun doc -> Mono.add_document search doc.Doc.uid doc) Doc.docs;
  let docs = Mono.search search "Al" in
  Format.(pp_print_list Doc.pp) Format.std_formatter docs

let () =
  Format.printf "<><><> Name only <><><>\n";
  only_name ();
  Format.printf "\n<><><> Name and nick <><><>\n";
  with_nick ()

module Dog = struct
  type t = { name : string }

  let dogs = [ { name = "Fido" }; { name = "Alan (the dog)" } ]
end

let with_generic () =
  let document : Doc.t Generic.uid = Generic.Uid.create () in
  let dog : Dog.t Generic.uid = Generic.Uid.create () in
  let search = Generic.empty () in
  Generic.add_index search document (fun t -> t.Doc.name);
  Generic.add_index search document (fun t -> t.Doc.nick);
  Generic.add_index search dog (fun t -> t.Dog.name);
  List.iter (fun d -> Generic.add_document search document d.Doc.uid d) Doc.docs;
  List.iter (fun d -> Generic.add_document search dog d.Dog.name d) Dog.dogs;
  let docs = Generic.search search "Al" in
  List.filter_map
    (fun v ->
      Generic.apply document
        ~default:(Generic.apply dog ~default:None (fun t -> Some t.Dog.name) v)
        (fun v -> Some v.Doc.name)
        v)
    docs
  |> List.iter print_endline

let () =
  Format.printf "\n<><><> Name only <><><>\n";
  with_generic ()
