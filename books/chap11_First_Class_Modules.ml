open Core

module type X_int = sig
  val x : int
end

module Three : X_int = struct
  let x = 3
end

(* A first-class module is created by packaging up a module with a signature that it satisfies. This is done using the module keyword.
   (module <Module> : <Module_type>)
*)
let three = (module Three : X_int)

module Four = struct
  let x = 4
end

(* The module type doesn’t need to be part of the construction of a first-class module if it can be inferred. *)
let numbers = [ three; (module Four) ]

(* create a first-class module from an anonymous module *)
let numbers =
  [
    three;
    (module struct
      let x = 4
    end);
  ]

(* In order to access the contents of a first-class module, you need to unpack it into an ordinary module.
   (val <first_class_module> : <Module_type>)
*)
module New_three = (val three : X_int)

let x = New_three.x

let to_int m =
  let module M = (val m : X_int) in
  M.x

let plus m1 m2 =
  (module struct
    let x = to_int m1 + to_int m2
  end : X_int)

let to_int (module M : X_int) = M.x
let six = plus three three
let () = Caml.print_int (to_int (List.fold ~init:six ~f:plus [ three; three ]))

module type Bumpable = sig
  type t

  val bump : t -> t
end

module Int_bumper = struct
  type t = int

  let bump n = n + 1
end

module Float_bumper = struct
  type t = float

  let bump n = n +. 1.
end

(* convert to first class modules *)
let int_bumper = (module Int_bumper : Bumpable)

(* Error: This expression has type int but an expression was expected of type Bumper.t *)
(* let (module Bumper) = int_bumper in
   Bumper.bump 3 *)

let int_bumper = (module Int_bumper : Bumpable with type t = int)
let float_bumper = (module Float_bumper : Bumpable with type t = float)

(* Exposing types *)
let x =
  let (module Bumper1) = int_bumper in
  Bumper1.bump 3

let x =
  let (module Bumper1) = float_bumper in
  Bumper1.bump 3.5

let bump_list (type a) (module Bumper : Bumpable with type t = a) (l : a list) =
  List.map ~f:Bumper.bump l

(* bump_list int_bumper [ 1; 2; 3 ];;
   bump_list float_bumper [ 1.5; 2.5; 3.5 ] *)

(* ABSTRACT TYPES *)
let wrap_in_list (type b) (x : b) = [ x ]
let wrap_in_list x = [ x ]

module type Comparable = sig
  type t

  val compare : t -> t -> int
end

let create_comparable (type a) compare =
  (module struct
    type t = a

    let compare = compare
  end : Comparable
    with type t = a)

let x = create_comparable Int.compare
let x = create_comparable Float.compare
