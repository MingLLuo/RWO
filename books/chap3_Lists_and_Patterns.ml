open Core

let () = Caml.print_endline ""

(* match won't check value, just cause a new var to be bound, shadowing earlier definition *)
(* let rec drop_value l to_drop =
   match l with
   | [] -> []
   | to_drop :: tl -> drop_value tl to_drop
   | hd :: tl -> hd :: drop_value tl to_drop;; *)
let rec drop_value l to_drop =
  match l with
  | [] ->
      []
  | hd :: tl ->
      let new_tl = drop_value tl to_drop in
      if hd = to_drop then new_tl else hd :: new_tl

(* match benchmark in bench dir~ *)

(* use list module *)

(* List.map List.map2 *)
(* _exn: throw an exception *)
let x = List.map2_exn ~f:Int.max [1; 2; 3] [3; 2; 1]

(* List.fold *)
let x = List.fold ~init:0 ~f:( + ) [1; 2; 3]

(* compute max column width *)
let max_widths header rows =
  let lengths l = List.map ~f:String.length l in
  List.fold rows ~init:(lengths header) ~f:(fun acc row ->
      List.map2_exn ~f:Int.max acc (lengths row) )

let render_separator widths =
  let pieces = List.map widths ~f:(fun w -> String.make w '-') in
  "|-" ^ String.concat ~sep:"-+-" pieces ^ "-|"

let () = Caml.print_endline (render_separator [3; 5; 1])

(* ^ will allocate string of 2-7~, small amount don't care,
   but for large string, can cause a serious perfomance issue *)
let s = "." ^ "." ^ "." ^ "." ^ "." ^ "." ^ "."

let s = String.concat ~sep:"." ["."; "."; "."; "."; "."; "."; "."]

let pad s length = s ^ String.make (length - String.length s) ' '

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "| " ^ String.concat ~sep:" | " padded ^ " |"

let () = Caml.print_endline (render_row ["Hello"; "World"] [10; 15])

let render_table header rows =
  let widths = max_widths header rows in
  String.concat ~sep:"\n"
    ( render_row header widths :: render_separator widths
    :: List.map rows ~f:(fun row -> render_row row widths) )

let () =
  Stdio.print_endline
    (render_table
       ["language"; "architect"; "first release"]
       [ ["Lisp"; "John McCarthy"; "1958"]
       ; ["C"; "Dennis Ritchie"; "1969"]
       ; ["ML"; "Robin Milner"; "1973"]
       ; ["OCaml"; "Xavier Leroy"; "1996"] ] )

(* List.reduce won't use an explicit starting value *)
let x = List.reduce ~f:( + ) [1; 2; 3]

(* List.filter, List.filter_map *)
let extensions filenames =
  List.filter_map filenames ~f:(fun fname ->
      match String.rsplit2 ~on:'.' fname with
      | None | Some ("", _) ->
          None
      | Some (_, ext) ->
          Some ext )
  |> List.dedup_and_sort ~compare:String.compare

let x = extensions ["foo.c"; "foo.ml"; "bar.ml"; "bar.mli"]

let () = Caml.print_endline (String.concat ~sep:", " x)

(* List.partition_tf *)
let is_ocaml_source s =
  match String.rsplit2 s ~on:'.' with
  | Some (_, ("ml" | "mli")) ->
      true
  | _ ->
      false

let ml_files, other_files =
  List.partition_tf ["foo.c"; "foo.ml"; "bar.ml"; "bar.mli"] ~f:is_ocaml_source

(* List.append == @ *)

module Sys = Core.Sys
module Filename = Core.Filename

let rec ls_rec s =
  if Sys_unix.is_file_exn ~follow_symlinks:true s then [s]
  else
    Sys_unix.ls_dir s
    |> List.map ~f:(fun sub -> ls_rec (Filename.concat s sub))
    |> List.concat

let rec ls_rec s =
  if Sys_unix.is_file_exn ~follow_symlinks:true s then [s]
  else
    Sys_unix.ls_dir s
    |> List.concat_map ~f:(fun sub -> ls_rec (Filename.concat s sub))

(* Terser and Faster Patterns *)
let rec remove_sequential_duplicates list =
  match list with
  | [] ->
      []
  | [x] ->
      [x]
  | first :: second :: tl ->
      if first = second then remove_sequential_duplicates (second :: tl)
      else first :: remove_sequential_duplicates (second :: tl)

(* the way [x] -> [x] will allocate a new list element, use as pattern instead
   use function to remove an explicit match *)
let rec remove_sequential_duplicates = function
  | [] as l ->
      l
  | [_] as l ->
      l
  | first :: (second :: _ as tl) ->
      if first = second then remove_sequential_duplicates tl
      else first :: remove_sequential_duplicates tl

(* combine two cases *)
let rec remove_sequential_duplicates list =
  match list with
  | ([] | [_]) as l ->
      l
  | first :: (second :: _ as tl) ->
      if first = second then remove_sequential_duplicates tl
      else first :: remove_sequential_duplicates tl

(* use when to add extra precondition *)
let rec remove_sequential_duplicates list =
  match list with
  | ([] | [_]) as l ->
      l
  | first :: (second :: _ as tl) when first = second ->
      remove_sequential_duplicates tl
  | first :: tl ->
      first :: remove_sequential_duplicates tl

(* polymorphic compare *)
let x =
  let open Base.Poly in
  "foo" = "bar" && 1.1 = 2.2

let x = String.equal "foo" "bar" && Float.equal 1.1 2.2

(* use polymorphic compare to compare different types *)
open Base.Poly

let rec remove_sequential_duplicates list =
  match list with
  | ([] | [_]) as l ->
      l
  | first :: (second :: _ as tl) when first = second ->
      remove_sequential_duplicates tl
  | first :: tl ->
      first :: remove_sequential_duplicates tl

(* sometimes use when clauses may warning, though they work fine :(  *)
(* let rec count_some list =
  match list with
  | [] ->
      0
  | x :: tl when Option.is_none x ->
      count_some tl
  | x :: tl when Option.is_some x ->
      1 + count_some tl *)

(* let rec count_some list =
  match list with
  | [] ->
      0
  | x :: tl when Option.is_none x ->
      count_some tl
  | x :: tl when Option.is_some x ->
      1 + count_some tl
  | x :: tl ->
      -1 (*unreachable*) *)

(* better to done *)
let rec count_some list =
  match list with
  | [] ->
      0
  | x :: tl when Option.is_none x ->
      count_some tl
  | _ :: tl ->
      1 + count_some tl

(* best to done *)
let rec count_some list =
  match list with
  | [] ->
      0
  | None :: tl ->
      count_some tl
  | Some _ :: tl ->
      1 + count_some tl

let count_some l = List.count ~f:Option.is_some l
