open Base

let digit_alist =
  [
    (0, "zero");
    (1, "one");
    (2, "two");
    (3, "three");
    (4, "four");
    (5, "five");
    (6, "six");
    (7, "seven");
    (8, "eight");
    (9, "nine");
  ]
(*
   List.Assoc.find ~equal:Int.equal digit_alist 6;;
   List.Assoc.find ~equal:Int.equal digit_alist 22;;
   List.Assoc.add ~equal:Int.equal digit_alist 0 "zilch" 
*)

(* Maps *)
type t = (string, int, String.comparator_witness) Map.t

let empty = Map.empty (module String)
let to_list t = Map.to_alist t

let touch t s =
  let count = match Map.find t s with None -> 0 | Some x -> x in
  Map.set t ~key:s ~data:(count + 1)

let x = Set.of_list (module Int) [ 1; 2; 3 ] |> Set.to_list

let x =
  Set.union
    (Set.of_list (module Int) [ 1; 2; 3; 2 ])
    (Set.of_list (module Int) [ 3; 5; 1 ])

(* Modules and Comparators *)
let digit_map = Map.of_alist_exn (module Int) digit_alist

(* The function Map.of_alist_exn constructs a map from a provided association list, throwing an exception if a key is used more than once *)
let x = Map.find digit_map 3

(* The type Map.comparator is actually an alias for a first-class module type, representing any module that matches the signature Comparator.S *)

(* What if you want to satisfy this interface with a new module *)
(* module Book = struct
     type t = { title : string; isbn : string }

     let compare t1 t2 =
       let cmp_title = String.compare t1.title t2.title in
       if cmp_title <> 0 then cmp_title else String.compare t1.isbn t2.isbn

     let sexp_of_t t : Sexp.t = List [ Atom t.title; Atom t.isbn ]
   end

   let x = Map.empty (module Book) *)

module Book = struct
  module T = struct
    type t = { title : string; isbn : string }

    let compare t1 t2 =
      let cmp_title = String.compare t1.title t2.title in
      if cmp_title <> 0 then cmp_title else String.compare t1.isbn t2.isbn

    let sexp_of_t t : Sexp.t = List [ Atom t.title; Atom t.isbn ]
  end

  include T
  include Comparator.Make (T)
end

(* In order to satisfy the interface, we need to use the Comparator.Make functor to extend the module. Here, we use a common idiom where we create a submodule, called T containing the basic functionality for the type in question, and then include both that module and the result of applying a functor to that module. *)
let some_programming_books =
  Set.of_list
    (module Book)
    [
      { title = "Real World OCaml"; isbn = "978-1449323912" };
      {
        title = "Structure and Interpretation of Computer Programs";
        isbn = "978-0262510875";
      };
      { title = "The C Programming Language"; isbn = "978-0131101630" };
    ]

(* Map.symmetric_diff, which computes the difference between two maps. *)
let left =
  Map.of_alist_exn (module String) [ ("foo", 1); ("bar", 3); ("snoo", 0) ]

let right = Map.of_alist_exn (module String) [ ("foo", 0); ("snoo", 0) ]
let x = Map.symmetric_diff ~data_equal:Int.equal left right |> Sequence.to_list
(* the type of Map.symmetric_diff requires that the two maps it compares have the same comparator witness, in addition to the same key and value type. *)

module Reverse = struct
  module T = struct
    type t = string

    let sexp_of_t = String.sexp_of_t
    let t_of_sexp = String.t_of_sexp
    let compare x y = String.compare y x
  end

  include T
  include Comparator.Make (T)
end

let alist = [ ("foo", 0); ("snoo", 3) ]
let ord_map = Map.of_alist_exn (module String) alist
let rev_map = Map.of_alist_exn (module Reverse) alist

(* Map.min_elt ord_map;;
   - : (string * int) option = Some ("foo", 0)
   Map.min_elt rev_map;;
   - : (string * int) option = Some ("snoo", 3) *)

(* We don’t need to generate specialized comparators for every type we want to build a map on. We can instead build a map based on OCaml’s built-in polymorphic comparison function *)

(* error(diff type): let x = Map.symmetric_diff
   (Map.Poly.singleton 3 "three")
   (Map.singleton (module Int) 3 "four" );; *)

(* This comparison failed because polymorphic compare doesn’t work on functions, and maps store the comparison function they were created with. Happily, there’s a function, Map.Using_comparator.to_tree which exposes the underlying binary tree without the attached comparison function. We can use that to compare the underlying trees:
   Poly.(m1 = m2);;
   Exception: Invalid_argument "compare: functional value".
   Poly.((Map.Using_comparator.to_tree m1) = (Map.Using_comparator.to_tree m2));;
   - : bool = false
*)

(* use deriving speed up *)
module Book1 = struct
  module T = struct
    type t = { title : string; isbn : string } [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

(* =, ==, AND PHYS_EQUAL *)
(* If you don’t open Base, you’ll find that the == operator tests for physical equality, while the = operator is the polymorphic equality function. *)

(* Two values are considered physically equal if they are the same pointer in memory. Two data structures that have identical contents but are constructed separately will not be considered equal by ==. Polymorphic equality, on the other hand, is structural, which effectively means that it considers values to be equal if they have the same contents.s *)

(* It’s quite easy to mix up = and ==, and so Base deprecates == and provides phys_equal instead, a function with a clear and descriptive name.
     ref 1 == ref 1;;
   Line 1, characters 7-9:
   Alert deprecated: Base.==
   [2016-09] this element comes from the stdlib distributed with OCaml.
   Use [phys_equal] instead.
   - : bool = false
   phys_equal (ref 1) (ref 1);;
   - : bool = false
*)
(* type string_int_map = (string, int, String.comparator_witness) Map.t [@@deriving sexp] *)

type string_int_map = int Map.M(String).t [@@deriving sexp]

let m = Map.singleton (module String) "one" 1
let x = (m : int Map.M(String).t)

(* Trees *)

(* Sometimes, for space efficiency reasons, you want a version of the map data structure that doesn’t include the comparator. You can get such a representation with Map.Using_comparator.to_tree, which returns just the tree underlying the map, without the comparator. *)
let ord_tree = Map.Using_comparator.to_tree ord_map

(* Even though the tree doesn’t physically include a comparator, it does include the comparator in its type. This is what is known as a phantom type, because it reflects something about the logic of the value in question, even though it doesn’t correspond to any values directly represented in the underlying physical structure of the value. *)

(* Since the comparator isn’t included in the tree, we need to provide the comparator explicitly when we, say, search for a key, as shown below: *)
let x =
  Map.Using_comparator.Tree.find ~comparator:String.comparator ord_tree "snoo"

(* wrong type: let x = Map.Using_comparator.Tree.find ~comparator:Reverse.comparator ord_tree "snoo" *)

(* Hash Tables *)
let table = Hashtbl.create (module String)
let () = Hashtbl.set table ~key:"three" ~data:3
let x = Hashtbl.find table "three"

module Book2 = struct
  type t = { title : string; isbn : string } [@@deriving compare, sexp_of, hash]
end

let table = Hashtbl.create (module Book2)
let table = Hashtbl.Poly.create ()
let x () = Hashtbl.set table ~key:("foo", 3, [ 1; 2; 3 ]) ~data:"random data!"
let x = Hashtbl.find table ("foo", 3, [ 1; 2; 3 ])
