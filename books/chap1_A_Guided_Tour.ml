open Core

(* function define *)
let square x = x * x
let printInt (x : int) = Caml.print_endline (string_of_int x)
let printFloat (x : float) = Caml.print_endline (string_of_float x)
let () = printInt (square 2)
let () = printInt (square (square 2))

(* other than int -> int *)
let ratio x y = Float.of_int x /. Float.of_int y
let () = printFloat (ratio 4 7)

(* open module in let *)
let ratio x y =
  let open Float.O in
  of_int x / of_int y

let () = printFloat (ratio 4 7)

let ratio x y =
  let open Float.O in
  of_int x / of_int y

let () = printFloat (ratio 4 7)
let ratio x y = Float.O.(of_int x / of_int y)
let () = printFloat (ratio 4 7)

(* type inference example *)
let sum_if_true test first second =
  (if test first then first else 0) + if test second then second else 0

(* let sum_if_true (test : int -> bool) (x : int) (y : int) : int =
   (if test x then x else 0)
   + (if test y then y else 0);; *)

(* generic types *)
let first_if_true test x y = if test x then x else y

(* val first_if_true : ('a -> bool) -> 'a -> 'a -> 'a = <fun> *)

(* error example *)

(* let add_potato x =
   x + "potato";; *)
(* run time error if y <= 0 *)
let is_a_multiple x y = x % y = 0

(* tuple patter matching *)
let distance (x1, y1) (x2, y2) =
  Float.sqrt (((x1 -. x2) **. 2.) +. ((y1 -. y2) **. 2.))

(* diff operation in base and stdlib *)

(* exp1: exponentiation *)
(* stdlib *)
let _ = 1. **. 2.
let _ = 1. ** 2.

(* Base *)
let _ = Base.( ** ) 1 2
let _ = Base.( **. ) 1. 2.

(* comma for tuple, semicolon for list element *)
let _ : (int * int * int) list = [ 1, 2, 3 ] [@ocamlformat "disable"]
let _ : int list = [ 1; 2; 3 ]

(* String.rsplit2 split the string based on the rightmost period found in the string. *)
let downcase_extension filename =
  match String.rsplit2 filename ~on:'.' with
  | None -> filename
  | Some (base, ext) -> base ^ "." ^ String.lowercase ext

let () =
  Caml.print_endline
    (List.to_string
       ~f:(fun x -> x)
       (List.map ~f:downcase_extension
          [ "Hello_World.TXT"; "Hello_World.txt"; "Hello_World" ]))

(* Array *)
let printIntArray (a : int array) =
  let lst = Array.to_list a in
  match lst with
  | [] -> print_endline ""
  | l -> print_endline (List.to_string ~f:(fun x -> string_of_int x) l)

let numbers = [| 1; 2; 3; 4 |]
let () = printIntArray numbers
let () = numbers.(2) <- 4
let () = printIntArray numbers

(* mutable record field *)
type running_sum = {
  mutable sum : float;
  mutable sum_sq : float; (* sum of squares *)
  mutable samples : int;
}

let mean rsum = rsum.sum /. Float.of_int rsum.samples

let stdev rsum =
  Float.sqrt ((rsum.sum_sq /. Float.of_int rsum.samples) -. (mean rsum **. 2.))

let create () = { sum = 0.; sum_sq = 0.; samples = 0 }

let update rsum x =
  rsum.samples <- rsum.samples + 1;
  rsum.sum <- rsum.sum +. x;
  rsum.sum_sq <- rsum.sum_sq +. (x *. x)

let rsum = create ()
let () = List.iter [ 1.; 3.; 2.; -7.; 4.; 5. ] ~f:(fun x -> update rsum x)

(* ref *)
let x = { contents = 0 };;

x.contents <- x.contents + 1

let x = ref 0;;

x := !x + 1

(* implement of ref *)
type 'a ref = { mutable contents : 'a }

let ref x = { contents = x }
let ( ! ) r = r.contents
let ( := ) r x = r.contents <- x

(* for loop *)
let permute array =
  let length = Array.length array in
  for i = 0 to length - 2 do
    (* pick a j to swap with *)
    let j = i + Random.int (length - i) in
    (* Swap i and j *)
    let tmp = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- tmp
  done

let ar = Array.init 20 ~f:(fun i -> i);;

permute ar

let () = printIntArray ar

(* while loop *)
let find_first_negative_entry array =
  let pos = ref 0 in
  while
    let pos_is_good = !pos < Array.length array in
    let element_is_non_negative = array.(!pos) >= 0 in
    pos_is_good && element_is_non_negative
  do
    pos := !pos + 1
  done;
  if !pos = Array.length array then None else Some !pos
;;

find_first_negative_entry [| 1; 2; 0; -3 |]

(* type ctrl + d *)
let rec read_and_accumulate accum =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> read_and_accumulate (accum +. Float.of_string x)

let () = printf "Total: %F\n" (read_and_accumulate 0.)
