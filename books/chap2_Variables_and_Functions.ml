(* a variable with limited scope *)
open Core

let printInt (x : int) = Caml.print_endline (string_of_int x)
let printFloat (x : float) = Caml.print_endline (string_of_float x)
let languages = "OCaml,Perl,C++,C"

let dashed_languages =
  let language_list = String.split languages ~on:',' in
  String.concat ~sep:"-" language_list

let () = Caml.print_endline ""
let () = Caml.print_endline dashed_languages

(* use nested expression build larger computation *)
let area_of_ring inner_radius outer_radius =
  let pi = Float.pi in
  let area_of_circle r = pi *. r *. r in
  area_of_circle outer_radius -. area_of_circle inner_radius

let () = Caml.print_endline (string_of_float (area_of_ring 1. 3.))

(* pattern matching in both side *)
let ints, strings = List.unzip [ (1, "one"); (2, "two"); (3, "three") ]

let print_ints =
  List.iter ints ~f:(fun x -> Caml.print_string (string_of_int x ^ " "))

let () = Caml.print_endline ""
let print_strings = List.iter strings ~f:(fun x -> Caml.print_string (x ^ " "))
let () = Caml.print_endline ""

let upcase_first_entry line =
  match String.split ~on:',' line with
  | [] -> assert false (* String.split returns at least one element *)
  | first :: rest -> String.concat ~sep:"," (String.uppercase first :: rest)

let () = Caml.print_endline (upcase_first_entry "hello,world")

[@@@ocamlformat "disable"]

(* let and fun *)
let () = printInt ((fun x -> x + 1) 7)


let () =
  let x = 7 in
  x + 1 |> printInt

(* multiargument function *)
let abs_diff = (
  fun x -> (fun y -> abs (x - y))
)
[@@@ocamlformat "enable"]

let () = printInt (abs_diff 3 4)

(* currying *)
let abs_diff (x, y) = abs (x - y)
let () = printInt (abs_diff (3, 4))

(* recursive function *)
let rec is_even x = if x = 0 then true else is_odd (x - 1)
and is_odd x = if x = 0 then false else is_even (x - 1)

let () = Caml.print_endline "Even or Odd"
let x = List.map ~f:(fun x -> if is_even x then printInt x) [ 0; 1; 2; 3; 4; 5 ]
let x = List.map ~f:(fun x -> if is_odd x then printInt x) [ 0; 1; 2; 3; 4; 5 ]
(* prefix and infix *)

[@@@ocamlformat "disable"]
let () = Int.max 3 4 |> printInt
let () = 3 + 4 |> printInt
let () = 3 + 4 |> printInt 
let () = List.map ~f:(( + ) 3) [ 1; 2; 3; 4 ] |> List.iter ~f:printInt
[@@@ocamlformat "enable"]

(* difine or redifine operator *)
let ( +! ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let () =
  (1, 2) +! (3, 4) |> fun (x, y) ->
  Caml.print_endline (string_of_int x ^ "," ^ string_of_int y)

(* will be a comment *)
(* let (***) x y = (x **. y) **. y;; *)
let ( *** ) x y = (x **. y) **. y

(* reverse application operator *)
(* left associative *)
let ( |> ) x f = f x

open Stdio

let path = "/usr/bin:/usr/local/bin:/bin:/sbin"

let () =
  String.split ~on:':' path
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:print_endline

let x =
  let split_path = String.split ~on:':' path in
  let deduped_path = List.dedup_and_sort ~compare:String.compare split_path in
  List.iter ~f:print_endline deduped_path

(* right associative *)
(* https://v2.ocaml.org/api/Ocaml_operators.html *)
let ( ^> ) x f = f x

(* let () = String.split ~on:':' path ^> List.dedup_and_sort ~compare:String.compare ^> List.iter ~f:print_endline *)
let () =
  (String.split ~on:':' path ^> List.dedup_and_sort ~compare:String.compare)
  ^> List.iter ~f:print_endline

(* function *)
let some_or_zero = function Some x -> x | None -> 0
let some_or_zero num_opt = match num_opt with Some x -> x | None -> 0

(* label arguments *)
let ratio ~num ~denom = Float.of_int num /. Float.of_int denom
let () = printFloat (ratio ~num:3 ~denom:4)

(* argument label*)
let apply_to_tuple_2 f (first, second) = f ~second ~first
let divide ~first ~second = first / second

(* error disordered argument *)
(* apply_to_tuple_2 divide (4,2) *)
let apply_to_tuple f (first, second) = f ~first ~second
let x = apply_to_tuple divide (4, 2)

(* optional argument *)
let concat ?sep x y =
  let sep = match sep with None -> "" | Some s -> s in
  x ^ sep ^ y

let () = Caml.print_endline (concat "hello" "world")
let () = Caml.print_endline (concat ~sep:"," "hello" "world")
let concat ?(sep = "") x y = x ^ sep ^ y
let () = Caml.print_endline (concat ~sep:"," "hello" "world")
let () = Caml.print_endline (concat ?sep:(Some ",") "hello" "world")
