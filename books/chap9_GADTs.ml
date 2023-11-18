(* Generalized Algebraic Data Types *)

(* GADTs provide two extra features above and beyond ordinary variants:

   - They let the compiler learn more type information when you descend into a case of a pattern match.
   - They make it easy to use existential types, which let you work with data of a specific but unknown type. *)

open Base

type _ value = Int : int -> int value | Bool : bool -> bool value

type _ expr =
  | Value : 'a value -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr

(* OCaml by default isn’t willing to instantiate ordinary type variables in different ways in the body of the same function *)
(* We can fix that by adding a locally abstract type, which doesn’t have that restriction.   *)
let eval_value : type a. a value -> a = function Int x -> x | Bool x -> x

let eval_value (type a) (v : a value) : a =
  match v with Int x -> x | Bool x -> x

(* let rec eval : 'a. 'a expr -> 'a =
   fun (type a) (x : a expr) ->
    match x with
    | Value v -> eval_value v
    | If (c, t, e) -> if eval c then eval t else eval e
    | Eq (x, y) -> eval x = eval y
    | Plus (x, y) -> eval x + eval y *)

let rec eval : type a. a expr -> a = function
  | Value v -> eval_value v
  | If (c, t, e) -> if eval c then eval t else eval e
  | Eq (x, y) -> eval x = eval y
  | Plus (x, y) -> eval x + eval y

let i x = Value (Int x)
and b x = Value (Bool x)
and ( +: ) x y = Plus (x, y)

module If_not_found = struct
  type 'a t = Raise | Return_none | Default_to of 'a
end

let rec flexible_find list ~f (if_not_found : _ If_not_found.t) =
  match list with
  | hd :: tl -> if f hd then Some hd else flexible_find ~f tl if_not_found
  | [] -> (
      match if_not_found with
      | Raise -> failwith "Element not found"
      | Return_none -> None
      | Default_to x -> Some x)

module If_not_found1 = struct
  type (_, _) t =
    | Raise : ('a, 'a) t
    | Return_none : ('a, 'a option) t
    | Default_to : 'a -> ('a, 'a) t
end

let rec flexible_find :
    type a b. f:(a -> bool) -> a list -> (a, b) If_not_found1.t -> b =
 fun ~f list if_not_found ->
  match list with
  | [] -> (
      match if_not_found with
      | Raise -> failwith "No matching item found"
      | Return_none -> None
      | Default_to x -> x)
  | hd :: tl ->
      if f hd then
        match if_not_found with
        | Raise -> hd
        | Return_none -> Some hd
        | Default_to _ -> hd
      else flexible_find ~f tl if_not_found

type stringable =
  | Stringable : { value : 'a; to_string : 'a -> string } -> stringable

let print_stringable (Stringable s) = Stdio.print_endline (s.to_string s.value)

let stringables =
  let s value to_string = Stringable { to_string; value } in
  [ s 100 Int.to_string; s 12.3 Float.to_string; s "foo" Fn.id ]
;;

List.iter ~f:print_stringable stringables
(* let get_value (Stringable s) = s.value;;
   (* It’s worth spending a moment to decode this error message, and the meaning of the type variable $Stringable_'a in particular. You can think of this variable as having three parts:

   The $ marks the variable as an existential.
   Stringable is the name of the GADT tag that this variable came from.
   'a is the name of the type variable from inside that tag. *) *)
