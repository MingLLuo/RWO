open Core
open Sexplib

(* find smallest and largest element on the list *)
let compute_bounds ~compare list =
  let sorted = List.sort ~compare list in
  match (List.hd sorted, List.last sorted) with
  | None, _ | _, None -> None
  | Some x, Some y -> Some (x, y)

let find_mismatches table1 table2 =
  Hashtbl.fold table1 ~init:[] ~f:(fun ~key ~data mismatches ->
      match Hashtbl.find table2 key with
      | Some data' when data' <> data -> key :: mismatches
      | _ -> mismatches)

module Result : sig
  type ('a, 'b) t = Ok of 'a | Error of 'b
end = struct
  type ('a, 'b) t = Ok of 'a | Error of 'b
end

let float_of_string s = Or_error.try_with (fun () -> Float.of_string s)
let x = float_of_string "a.bc"

let () =
  match x with
  | Ok x -> printf "x = %f\n" x
  | Error _ -> printf "x is not a float\n"
;;

Error.tag
  (Error.of_list
     [
       Error.of_string "Your tires were slashed";
       Error.of_string "Your windshield was smashed";
     ])
  ~tag:"over the weekend"

let a = "foo"
and b = ("foo", [ 3; 4 ])

let bind option ~f = match option with None -> None | Some x -> f x

let compute_bounds ~compare list =
  let sorted = List.sort ~compare list in
  Option.bind (List.hd sorted) ~f:(fun first ->
      Option.bind (List.last sorted) ~f:(fun last -> Some (first, last)))

exception Key_not_found of string

(* raise (Key_not_found "a") *)

let exceptions = [ Division_by_zero; Key_not_found "b" ]

let rec find_exn alist key =
  match alist with
  | [] -> raise (Key_not_found key)
  | (key', data) :: tl ->
      if String.( = ) key key' then data else find_exn tl key

type 'a bounds = { lower : 'a; upper : 'a } [@@deriving sexp]

let failwith msg = raise (Failure msg)

let merge_lists xs ys ~f =
  if List.length xs <> List.length ys then None
  else
    let rec loop xs ys =
      match (xs, ys) with
      | [], [] -> []
      | x :: xs, y :: ys -> f x y :: loop xs ys
      | _ -> assert false
    in
    Some (loop xs ys)

let merge_lists xs ys ~f =
  let rec loop xs ys =
    match (xs, ys) with
    | [], [] -> []
    | x :: xs, y :: ys -> f x y :: loop xs ys
    | _ -> assert false
  in
  loop xs ys

(* merge_lists [ 1; 2; 3 ] [ -1 ] ~f:( + ) *)

let parse_line line =
  String.split_on_chars ~on:[ ',' ] line |> List.map ~f:Float.of_string

(* might not close In_channel *)
let load filename =
  let inc = In_channel.create filename in
  let data = In_channel.input_lines inc |> List.map ~f:parse_line in
  In_channel.close inc;
  data

let load filename =
  let inc = In_channel.create filename in
  Exn.protect
    ~f:(fun () -> In_channel.input_lines inc |> List.map ~f:parse_line)
    ~finally:(fun () -> In_channel.close inc)

let load filename =
  In_channel.with_file filename ~f:(fun inc ->
      In_channel.input_lines inc |> List.map ~f:parse_line)

let lookup_weight ~compute_weight alist key =
  try
    let data = find_exn alist key in
    compute_weight data
  with Key_not_found _ -> 0.
;;

lookup_weight ~compute_weight:(fun _ -> raise (Key_not_found "foo")) ["a",3; "b",4] "a";;


let lookup_weight ~compute_weight alist key =
  match try Some (find_exn alist key) with _ -> None with
  | None -> 0.
  | Some data -> compute_weight data

let lookup_weight ~compute_weight alist key =
  match find_exn alist key with
  | exception _ -> 0.
  | data -> compute_weight data

let lookup_weight ~compute_weight alist key =
  match List.Assoc.find ~equal:String.equal alist key with
  | None -> 0.
  | Some data -> compute_weight data

(* backtraces *)
exception Empty_list

let list_max = function
  | [] -> raise Empty_list
  | hd :: tl -> List.fold tl ~init:hd ~f:(Int.max)

let () =
  printf "%d\n" (list_max [1;2;3]);
  printf "%d\n" (list_max [])
