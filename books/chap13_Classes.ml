open Base

class istack =
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

let s = new istack

(* s#pop;;
   s#push 5;;
   s#pop *)

(* type istack = < pop : int option ; push : int -> unit > *)

(* make an OCaml iterator *)
type 'a iterator = < get : 'a ; has_value : bool ; next : unit >

class ['a] list_iterator init =
  object
    val mutable current : 'a list = init
    method has_value = not (List.is_empty current)

    method get =
      match current with
      | hd :: _ -> hd
      | [] -> raise (Invalid_argument "no value")

    method next =
      match current with
      | _ :: tl -> current <- tl
      | [] -> raise (Invalid_argument "no value")
  end

(* Class Parameters and Polymorphism *)
class ['a] stack init =
  object
    val mutable v : 'a list = init

    method pop =
      match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None

    method push hd = v <- hd :: v
    method iterator : 'a iterator = new list_iterator v

    method fold : 'b. ('b -> 'a -> 'b) -> 'b -> 'b =
      fun f init -> List.fold ~f ~init v
  end
(* The type quantifier 'b. can be read as “for all 'b.” Type quantifiers can only be used directly after the method name, which means that method parameters must be sexpressed using a fun or function expression. *)

let s = new stack []

(* s#push 5;;
   s#push 4 *)

let it = s#iterator

(* it#get;;
   it#next;;
   it#get;;
   it#next;;
   it#has_value *)

class sstack init =
  object
    inherit [string] stack init
    method print = List.iter ~f:Stdio.print_endline v
  end

(* override method *)
class double_stack init =
  object
    inherit [int] stack init as super
    method push hd = super#push (hd * 2)
  end

(* Open Recursion *)
type doc =
  | Heading of string
  | Paragraph of text_item list
  | Definition of string list_item list

and text_item =
  | Raw of string
  | Bold of text_item list
  | Enumerate of int list_item list
  | Quote of doc

and 'a list_item = { tag : 'a; text : text_item list }

(* The object (self) syntax binds self to the current object, allowing the doc, list_item, and text_item methods to call each other. *)
class ['a] folder =
  object (self)
    method doc acc =
      function
      | Heading _ -> acc
      | Paragraph text -> List.fold ~f:self#text_item ~init:acc text
      | Definition list -> List.fold ~f:self#list_item ~init:acc list

    method list_item : 'b. 'a -> 'b list_item -> 'a =
      fun acc { tag; text } -> List.fold ~f:self#text_item ~init:acc text

    method text_item acc =
      function
      | Raw _ -> acc
      | Bold text -> List.fold ~f:self#text_item ~init:acc text
      | Enumerate list -> List.fold ~f:self#list_item ~init:acc list
      | Quote doc -> self#doc acc doc
  end

(* Private mathod *)
class ['a] folder2 =
  object (self)
    method doc acc =
      function
      | Heading str -> self#heading acc str
      | Paragraph text -> self#paragraph acc text
      | Definition list -> self#definition acc list

    method list_item : 'b. 'a -> 'b list_item -> 'a =
      fun acc { tag; text } -> List.fold ~f:self#text_item ~init:acc text

    method text_item acc =
      function
      | Raw str -> self#raw acc str
      | Bold text -> self#bold acc text
      | Enumerate list -> self#enumerate acc list
      | Quote doc -> self#quote acc doc

    method private heading acc str = acc

    method private paragraph acc text =
      List.fold ~f:self#text_item ~init:acc text

    method private definition acc list =
      List.fold ~f:self#list_item ~init:acc list

    method private raw acc str = acc
    method private bold acc text = List.fold ~f:self#text_item ~init:acc text

    method private enumerate acc list =
      List.fold ~f:self#list_item ~init:acc list

    method private quote acc doc = self#doc acc doc
  end

let f :
    < doc : int -> doc -> int
    ; list_item : 'a. int -> 'a list_item -> int
    ; text_item : int -> text_item -> int > =
  new folder2

(* The private methods are part of the class type, but not part of the object type. This means, for example, that the object f has no method bold. However, the private methods are available to subclasses: we can use them to simplify our counter class: *)
class counter_with_private_method =
  object
    inherit [int] folder2 as super
    method list_item acc li = acc

    method private bold acc txt =
      let acc = super#bold acc txt in
      acc + 1
  end

(* The key property of private methods is that they are visible to subclasses, but not anywhere else. If you want the stronger guarantee that a method is really private, not even accessible in subclasses, you can use an explicit class type that omits the method. In the following code, the private methods are explicitly omitted from the class type of counter_with_sig and can’t be invoked in subclasses of counter_with_sig: *)
class counter_with_sig : object
  method doc : int -> doc -> int
  method list_item : int -> 'b list_item -> int
  method text_item : int -> text_item -> int
end =
  object
    inherit [int] folder2 as super
    method list_item acc li = acc

    method private bold acc txt =
      let acc = super#bold acc txt in
      acc + 1
  end

(* Binary Methods *)
(* A binary method is a method that takes an object of self type. *)
class square w =
  object (self : 'self)
    method width = w
    method area = Float.of_int (self#width * self#width)
    method equals (other : 'self) = other#width = self#width
  end

class circle r =
  object (self : 'self)
    method radius = r
    method area = 3.14 *. (Float.of_int self#radius **. 2.0)
    method equals (other : 'self) = other#radius = self#radius
  end

(* (new square 5)#equals (new square 5);;
   (new circle 10)#equals (new circle 7);; *)

(* the built-in polymorphic equality has very poor behavior when applied to objects: return false, not physically equal *)
(* Poly.( = )
   (object
      method area = 5
   end)
   (object
      method area = 5
   end) *)

type shape_repr = ..
type shape = < repr : shape_repr ; equals : shape -> bool ; area : float >
type shape_repr += Square of int

class square1 w =
  object (self)
    method width = w
    method area = Float.of_int (self#width * self#width)
    method repr = Square self#width

    method equals (other : shape) =
      match (self#repr, other#repr) with
      | Square x, Square x' -> Int.( = ) x x'
      | _ -> false
  end
