open Base

let s =
  object
    val mutable v = [ 0; 2 ]

    method pop =
      match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None

    method push hd = v <- hd :: v
  end

(* The syntax for a method invocation uses the # character *)
(* s#pop;;
   s#push 4;;
   s#pop *)

let stack init =
  object
    val mutable v = init

    method pop =
      match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None

    method push hd = v <- hd :: v
  end

let s = stack [ 3; 2; 1 ]

(* s#pop *)

(* Object Polymorphism *)

let area sq = sq#width * sq#width
let minimize sq : unit = sq#resize 1
let limit sq = if area sq > 100 then minimize sq

(* The type system will complain if it sees incompatible uses of the same method: *)

(* let toggle sq b : unit = if b then sq#resize `Fullscreen else minimize sq *)

(* manually close *)
let area_closed (sq : < width : int >) = sq#width * sq#width

let sq =
  object
    method width = 30
    method name = "sq"
  end

(* wrong type: area_closed sq;; *)

(* The .. in an open object type is an elision, standing for “possibly more methods.” *)

let print_pop st = Option.iter ~f:(Stdio.printf "Popped: %d\n") st#pop
let () = print_pop (stack [ 5; 4; 3; 2; 1 ])

(* Immutable Objects *)
let imm_stack init =
  object
    val v = init
    method pop = match v with hd :: tl -> Some (hd, {<v = tl>}) | [] -> None
    method push hd = {<v = hd :: v>}
  end

let s = imm_stack [ 3; 2; 1 ]
let r = s#push 4

(* s#pop;;
   r#pop *)

(* Subtyping *)
(* Width Subtyping *)
type shape = < area : float >
type square = < area : float ; width : int >

let square w =
  object
    method area = Float.of_int (w * w)
    method width = w
  end

(* the coercion :> must be explicit: *)
(* (square 10 : shape);; *)
let x = (square 10 :> shape)
(* This form of object subtyping is called width subtyping. Width subtyping means that an object type A is a subtype of B, if A has all of the methods of B, and possibly more. A square is a subtype of shape because it implements all of the methods of shape, which in this case means the area method. *)

(* Depth Subtyping *)
(* We can also use depth subtyping with objects. Depth subtyping allows us to coerce an object if its individual methods could safely be coerced. So an object type < m: t1 > is a subtype of < m: t2 > if t1 is a subtype of t2.    *)

type circle = < area : float ; radius : int >

let circle r =
  object
    method area = 3.14 *. (Float.of_int r **. 2.0)
    method radius = r
  end

let coin =
  object
    method shape = circle 5
    method color = "silver"
  end

let map =
  object
    method shape = square 10
  end

type item = < shape : shape >

(* Both these objects have a shape method whose type is a subtype of the shape type, so they can both be coerced into the object type < shape : shape >: *)
let items = [ (coin :> item); (map :> item) ]

(* Subtyping can also be used to coerce a polymorphic variant into a larger polymorphic variant type. A polymorphic variant type A is a subtype of B, if the tags of A are a subset of the tags of B: *)

type num = [ `Int of int | `Float of float ]
type const = [ num | `String of string ]

let n : num = `Int 3
let c : const = (n :> const)

(* Variance *)
let squares : square list = [ square 10; square 20 ]
let shapes : shape list = (squares :> shape list)
let square_array : square array = [| square 10; square 20 |]

(* Note that this relies on lists being immutable. It would not be safe to treat a square array as a shape array because it would allow you to store non-square shapes into what should be an array of squares. OCaml recognizes this and does not allow the coercion: *)
(* let shape_array: shape array = (square_array :> shape array);; *)
let shape_to_string : shape -> string =
 fun s -> Printf.sprintf "Shape(%F)" s#area

let square_to_string : square -> string = (shape_to_string :> square -> string)

(* VARIANCE ANNOTATIONS : pass *)
