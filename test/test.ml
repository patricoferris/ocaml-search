type t = { uid : string; name : string; nick : string; age : int }

let pp ppf { uid; name; nick; age } =
  Format.fprintf ppf "uid: %s, name: %s, nick: %s, age: %i" uid name nick age

let docs =
  [
    { uid = "0"; name = "Alice"; nick = ""; age = 10 };
    { uid = "1"; name = "Alan"; nick = "Al"; age = 12 };
    { uid = "2"; name = "William"; nick = "Bob"; age = 13 };
  ]

let only_name () =
  let search = Search.create (fun t -> t.uid) in
  Search.add_index search (fun t -> t.name);
  List.iter (Search.add_document search) docs;
  let docs = Search.search search "Al" in
  Format.(pp_print_list pp) Format.std_formatter docs

let with_nick () =
  let search = Search.create (fun t -> t.uid) in
  Search.add_index search (fun t -> t.name);
  Search.add_index search (fun t -> t.nick);
  List.iter (Search.add_document search) docs;
  let docs = Search.search search "Al" in
  Format.(pp_print_list pp) Format.std_formatter docs

let () =
  Format.printf "<><><> Name only <><><>\n";
  only_name ();
  Format.printf "\n<><><> Name and nick <><><>\n";
  with_nick ()
