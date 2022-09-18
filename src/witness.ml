type _ witness = ..

module type Tid = sig
  type t
  type _ witness += Tid : t witness
end

type 'a t = (module Tid with type t = 'a)

let make () (type s) =
  let module M = struct
    type t = s
    type _ witness += Tid : t witness
  end in
  (module M : Tid with type t = s)

type ('a, 'b) teq = Teq : ('a, 'a) teq

let eq : type r s. r t -> s t -> (r, s) teq option =
 fun r s ->
  let module R = (val r : Tid with type t = r) in
  let module S = (val s : Tid with type t = s) in
  match R.Tid with S.Tid -> Some Teq | _ -> None
