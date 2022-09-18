include Search_intf
module Tfidf = Tfidf

let create_uid (type uid) ~(to_string : uid -> string)
    ~(cmp : uid -> uid -> int) =
  let module T = struct
    type t = uid

    let to_string = to_string
    let compare = cmp
  end in
  (module T : Uid with type t = uid)

module Uids = struct
  module String = struct
    type t = string

    let to_string = Fun.id
    let compare = String.compare
  end

  module Int = struct
    type t = int

    let to_string = string_of_int
    let compare = Int.compare
  end
end
