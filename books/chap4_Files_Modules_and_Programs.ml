open Base
module Time = Core.Time
(* module <name> : <signature> = <implementation> *)

module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

module String_id = struct
  type t = string

  let of_string x = x
  let to_string x = x
  let ( = ) = String.( = )
end

module Username : ID = String_id
module Hostname : ID = String_id

type session_info = {
  user : Username.t;
  host : Hostname.t;
  when_started : Time.t;
}

(* let session_with_wrong_compare s1 s2 = Username.( = ) s1.user s2.host *)
let sessions_have_same_user s1 s2 = Username.( = ) s1.user s2.user

module M = struct
  let foo = 3
end

open M

let () = Caml.print_int foo

(* generally better to keep down the amount of code affected by an open *)
let average x y =
  let open Int64 in
  (x + y) / of_int 2

let average x y = Int64.((x + y) / of_int 2)

(* including module *)
module Interval = struct
  type t = Interval of int * int | Empty

  let create low high = if high < low then Empty else Interval (low, high)
end

(* module Extended_interval = struct
     include Interval

     let contains t x =
       match t with
       | Empty -> false
       | Interval (low,high) -> x >= low && x <= high
   end;;
   let () = Extended_interval.contains (Extended_interval.create 3 10) 4;;
*)

module Extended_interval = struct
  open Interval

  let contains t x =
    match t with
    | Empty -> false
    | Interval (low, high) -> x >= low && x <= high
end
(* unbounded *)
(* let () = Extended_interval.contains (Extended_interval.create 3 10) 4;; *)
