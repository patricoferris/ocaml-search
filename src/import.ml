module Option = struct
  let get = function
    | Some v -> v
    | None -> raise (Invalid_argument "Expected Some value got None!")
end
