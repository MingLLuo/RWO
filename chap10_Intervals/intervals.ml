open Base

module type Comparable = sig
  type t

  val compare : t -> t -> int
end

module Make_interval (Endpoint : Comparable) = struct
  type t = Interval of Endpoint.t * Endpoint.t | Empty

  (** [create low high] creates a new interval from [low] to
      [high].  If [low > high], then the interval is empty *)
  let create low high =
    if Endpoint.compare low high > 0 then Empty else Interval (low, high)

  (** Returns true iff the interval is empty *)
  let is_empty = function Empty -> true | Interval _ -> false

  (** [contains t x] returns true iff [x] is contained in the
      interval [t] *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) -> Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  (** [intersect t1 t2] returns the intersection of the two input
      intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match (t1, t2) with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) -> create (max l1 l2) (min h1 h2)
end

module Int_interval = Make_interval (struct
  type t = int

  let compare = Int.compare
end)

module Int_interval1 = Make_interval (Int)
module String_interval = Make_interval (String)

let i1 = Int_interval.create 3 8
let i2 = Int_interval.create 4 10
let x = Int_interval.intersect i1 i2

module Rev_int_interval = Make_interval (struct
  type t = int

  let compare x y = Int.compare y x
end)

let rev_interval = Rev_int_interval.create 4 3;;

(* illegal use of type t, they are different
    *)
(* Int_interval.contains rev_interval 3;; *)

(* wrong case *)
Int_interval.is_empty (* going through create *) (Int_interval.create 4 3);;
Int_interval.is_empty (* bypassing create *) (Int_interval.Interval (4, 3))

(* because Int_interval.t is not abstract, we can bypass create function *)
module type Interval_intf = sig
  type t
  type endpoint

  val create : endpoint -> endpoint -> t
  val is_empty : t -> bool
  val contains : t -> endpoint -> bool
  val intersect : t -> t -> t
end

module Make_interval1 (Endpoint : Comparable) : Interval_intf = struct
  type endpoint = Endpoint.t
  type t = Interval of Endpoint.t * Endpoint.t | Empty

  (** [create low high] creates a new interval from [low] to
      [high].  If [low > high], then the interval is empty *)
  let create low high =
    if Endpoint.compare low high > 0 then Empty else Interval (low, high)

  (** Returns true iff the interval is empty *)
  let is_empty = function Empty -> true | Interval _ -> false

  (** [contains t x] returns true iff [x] is contained in the
      interval [t] *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) -> Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  (** [intersect t1 t2] returns the intersection of the two input
      intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match (t1, t2) with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) -> create (max l1 l2) (min h1 h2)
end

module Int_interval2 = Make_interval1 (Int)

(* but this is too abstract *)
(* Int_interval2.create 3 4;; *)

(* we can share a constraint
   1. <Module_type> with type <type> = <type'>
   2. <Module_type> with type <type1> = <type1'> and type <type2> = <type2'>
*)
module type Int_interval_intf = Interval_intf with type endpoint = int

module Make_interval2 (Endpoint : Comparable) :
  Interval_intf with type endpoint = Endpoint.t = struct
  type endpoint = Endpoint.t
  type t = Interval of Endpoint.t * Endpoint.t | Empty

  (** [create low high] creates a new interval from [low] to
      [high].  If [low > high], then the interval is empty *)
  let create low high =
    if Endpoint.compare low high > 0 then Empty else Interval (low, high)

  (** Returns true iff the interval is empty *)
  let is_empty = function Empty -> true | Interval _ -> false

  (** [contains t x] returns true iff [x] is contained in the
      interval [t] *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) -> Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  (** [intersect t1 t2] returns the intersection of the two input
      intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match (t1, t2) with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) -> create (max l1 l2) (min h1 h2)
end

module Int_interval3 = Make_interval2 (Int)

let i = Int_interval3.create 3 4

(* destructive substitution *)
module type Int_interval_intf2 = Interval_intf with type endpoint := int

(* module type Int_interval_intf =
   sig
     type t
     val create : int -> int -> t
     val is_empty : t -> bool
     val contains : t -> int -> bool
     val intersect : t -> t -> t
   end *)

module Make_interval3 (Endpoint : Comparable) :
  Interval_intf with type endpoint := Endpoint.t = struct
  type t = Interval of Endpoint.t * Endpoint.t | Empty

  (** [create low high] creates a new interval from [low] to
      [high].  If [low > high], then the interval is empty *)
  let create low high =
    if Endpoint.compare low high > 0 then Empty else Interval (low, high)

  (** Returns true iff the interval is empty *)
  let is_empty = function Empty -> true | Interval _ -> false

  (** [contains t x] returns true iff [x] is contained in the
      interval [t] *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) -> Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  (** [intersect t1 t2] returns the intersection of the two input
      intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match (t1, t2) with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) -> create (max l1 l2) (min h1 h2)
end

module Int_interval4 = Make_interval3 (Int);;

Int_interval4.is_empty (Int_interval4.create 3 4)

(* Int_interval4.is_empty (Int_interval4.Interval (4, 3)) *)

module type Interval_intf_with_sexp = sig
  include Interval_intf
  include Sexpable.S with type t := t
end

module Make_interval5 (Endpoint : sig
  type t

  include Comparable with type t := t
  include Sexpable.S with type t := t
end) : Interval_intf_with_sexp with type endpoint := Endpoint.t = struct
  type t = Interval of Endpoint.t * Endpoint.t | Empty [@@deriving sexp]

  (** [create low high] creates a new interval from [low] to
    [high].  If [low > high], then the interval is empty *)
  let create low high =
    if Endpoint.compare low high > 0 then Empty else Interval (low, high)

  (* put a wrapper around the autogenerated [t_of_sexp] to enforce
     the invariants of the data structure *)
  let t_of_sexp sexp =
    match t_of_sexp sexp with Empty -> Empty | Interval (x, y) -> create x y

  (** Returns true iff the interval is empty *)
  let is_empty = function Empty -> true | Interval _ -> false

  (** [contains t x] returns true iff [x] is contained in the
    interval [t] *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) -> Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  (** [intersect t1 t2] returns the intersection of the two input
    intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match (t1, t2) with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) -> create (max l1 l2) (min h1 h2)
end

module Int_interval5 = Make_interval5 (Int);;

Int_interval5.sexp_of_t (Int_interval5.create 3 4);;
Int_interval5.sexp_of_t (Int_interval5.create 4 3)
