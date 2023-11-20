open Core

(* Pure code is the default in OCaml, and for good reason—it’s generally easier to reason about, less error prone and more composable. *)
let b = Bytes.of_string "foobar"
let () = Bytes.set b 0 (Char.uppercase (Bytes.get b 0))
let () = Caml.print_endline (Bytes.to_string b)
let x = ref 1
let ref x = { contents = x }
let ( ! ) r = r.contents
let ( := ) r x = r.contents <- x

let () =
  for i = 0 to 3 do
    printf "i = %d\n" i
  done

let rev_inplace ar =
  let i = ref 0 in
  let j = ref (Array.length ar - 1) in
  (* terminate when the upper and lower indices meet *)
  while !i < !j do
    (* swap the two elements *)
    let tmp = ar.(!i) in
    ar.(!i) <- ar.(!j);
    ar.(!j) <- tmp;
    (* bump the indices *)
    Int.incr i;
    Int.decr j
  done

let nums = [| 1; 2; 3; 4; 5 |]
let () = rev_inplace nums

(* lazy make the actual computation perform only once, only after force had been called *)
let v =
  lazy
    (print_endline "performing lazy computation";
     Float.sqrt 16.)

let x = Lazy.force v
let x = Lazy.force v
let () = Caml.print_float x

(* own lazy *)
type 'a lazy_state = Delayed of (unit -> 'a) | Value of 'a | Exn of exn
type 'a our_lazy = { mutable state : 'a lazy_state }

let our_lazy f = { state = Delayed f }

let v =
  our_lazy (fun () ->
      print_endline "performing lazy computation";
      Float.sqrt 16.)

let our_force l =
  match l.state with
  | Value x -> x
  | Exn e -> raise e
  | Delayed f -> (
      try
        let x = f () in
        l.state <- Value x;
        x
      with exn ->
        l.state <- Exn exn;
        raise exn)

let x = our_force v
let x = our_force v

(* Memoization and Dynamic Programming *)
let memoize m f =
  let memo_table = Hashtbl.create m in
  fun x -> Hashtbl.find_or_add memo_table x ~default:(fun () -> f x)

let rec edit_distance s t =
  match (String.length s, String.length t) with
  | 0, x | x, 0 -> x
  | len_s, len_t ->
      let s' = String.drop_suffix s 1 in
      let t' = String.drop_suffix t 1 in
      let cost_to_drop_both =
        if Char.( = ) s.[len_s - 1] t.[len_t - 1] then 0 else 1
      in
      List.reduce_exn ~f:Int.min
        [
          edit_distance s' t + 1;
          edit_distance s t' + 1;
          edit_distance s' t' + cost_to_drop_both;
        ]

let x = edit_distance "OCaml" "ocaml"

let time f =
  let open Core in
  let start = Time.now () in
  let x = f () in
  let stop = Time.now () in
  printf "Time: %F ms\n" (Time.diff stop start |> Time.Span.to_ms);
  x

let x = time (fun () -> edit_distance "OCaml" "ocaml")
let x = time (fun () -> edit_distance "OCaml 4.13" "ocaml 4.13")
let rec fib i = if i <= 1 then i else fib (i - 1) + fib (i - 2)
let x = time (fun () -> fib 20)
let x = time (fun () -> fib 40)
let fib = memoize (module Int) fib
let x = time (fun () -> fib 40)
let x = time (fun () -> fib 40)

(* unwind recursion *)
let fib_norec fib i = if i <= 1 then i else fib (i - 1) + fib (i - 2)
let rec fib i = fib_norec fib i
let x = fib 20

let make_rec f_norec =
  let rec f x = f_norec f x in
  f

let fib = make_rec fib_norec

let memo_rec m f_norec x =
  let fref = ref (fun _ -> assert false) in
  let f = memoize m (fun x -> f_norec !fref x) in
  fref := f;
  f x

let fib = memo_rec (module Int) fib_norec

(* time (fun () -> fib 40);;
   time (fun () -> fib 40) *)

let fib =
  memo_rec
    (module Int)
    (fun fib i -> if i <= 1 then 1 else fib (i - 1) + fib (i - 2))

(* Memoization is overkill for implementing Fibonacci, and indeed, the fib defined above is not especially efficient, allocating space linear in the number passed into fib. It’s easy enough to write a Fibonacci function that takes a constant amount of space. *)
module String_pair = struct
  type t = string * string [@@deriving sexp_of, hash, compare]
end

(* module String_pair :
   sig
     type t = string * string
     val sexp_of_t : t -> Sexp.t
     val hash_fold_t : Hash.state -> t -> Hash.state
     val hash : t -> int
     val compare : t -> t -> int
   end *)

let edit_distance =
  memo_rec
    (module String_pair)
    (fun edit_distance (s, t) ->
      match (String.length s, String.length t) with
      | 0, x | x, 0 -> x
      | len_s, len_t ->
          let s' = String.drop_suffix s 1 in
          let t' = String.drop_suffix t 1 in
          let cost_to_drop_both =
            if Char.( = ) s.[len_s - 1] t.[len_t - 1] then 0 else 1
          in
          List.reduce_exn ~f:Int.min
            [
              edit_distance (s', t) + 1;
              edit_distance (s, t') + 1;
              edit_distance (s', t') + cost_to_drop_both;
            ])

(* time (fun () -> edit_distance ("OCaml 4.09", "ocaml 4.09")) *)

(* let memo_rec m f_norec =
     let rec f = memoize m (fun x -> f_norec f x) in
     f;;
   let rec x = x + 1
   (* You could imagine it compiling down to an infinite loop, but x is of type int, and there’s no int that corresponds to an infinite loop. As such, this construct is effectively impossible to compile. *)
*)
let memo_rec m f_norec x =
  let fref = ref (fun _ -> assert false) in
  let f = memoize m (fun x -> f_norec !fref x) in
  fref := f;
  f x

(* may pass in lazy *)
let rec x = lazy (force x + 1)

(* undefined *)
(* force x *)

let lazy_memo_rec m f_norec x =
  let rec f = lazy (memoize m (fun x -> f_norec (force f) x)) in
  (force f) x

(* print *)
let fmt = "%i is an integer\n"

(* let () = Printf.printf fmt 1, wrong *)
open CamlinternalFormatBasics

let fmt : ('a, 'b, 'c) format = "%i is an integer\n"
let () = printf fmt 3

(* file I/O *)
let create_number_file filename numbers =
  let outc = Out_channel.create filename in
  List.iter numbers ~f:(fun x -> Out_channel.fprintf outc "%d\n" x);
  Out_channel.close outc

let sum_file filename =
  let file = In_channel.create filename in
  Exn.protect
    ~f:(fun () ->
      let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
      List.fold ~init:0 ~f:( + ) numbers)
    ~finally:(fun () -> In_channel.close file)

(* ignore(f x) --> f x; () *)
let () =
  for _ = 1 to 10000 do
    try ignore (sum_file "/etc/hosts" : int) with _ -> ()
  done

let () = try sum_file "numbers.txt" |> Caml.print_int with _ -> ()

let sum_file filename =
  In_channel.with_file filename ~f:(fun file ->
      let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
      List.fold ~init:0 ~f:( + ) numbers)

let sum_file filename =
  In_channel.with_file filename ~f:(fun file ->
      In_channel.fold_lines file ~init:0 ~f:(fun sum line ->
          sum + Int.of_string line))

(* order of evaluation *)
let m =
  let x = Float.sin 120. in
  let y = Float.sin 75. in
  let z = Float.sin 128. in
  List.exists ~f:(fun x -> Float.O.(x < 0.)) [ x; y; z ]

let m =
  let x = lazy (Float.sin 120.) in
  let y = lazy (Float.sin 75.) in
  let z = lazy (Float.sin 128.) in
  List.exists ~f:(fun x -> Float.O.(Lazy.force x < 0.)) [ x; y; z ]

let m =
  let x =
    lazy
      (printf "1\n";
       Float.sin 120.)
  in
  let y =
    lazy
      (printf "2\n";
       Float.sin 75.)
  in
  let z =
    lazy
      (printf "3\n";
       Float.sin 128.)
  in
  List.exists ~f:(fun x -> Float.O.(Lazy.force x < 0.)) [ x; y; z ]

let m =
  List.exists
    ~f:(fun x -> Float.O.(x < 0.))
    [
      (printf "1\n";
       Float.sin 120.);
      (printf "2\n";
       Float.sin 75.);
      (printf "3\n";
       Float.sin 128.);
    ]

(* Side Effects and Weak Polymorphism *)

let remember =
  let cache = ref None in
  fun x ->
    match !cache with
    | Some y -> y
    | None ->
        cache := Some x;
        x

let identity x = x

(* The underscore in the type variable '_weak1 tells us that the variable is only weakly polymorphic, which is to say that it can be used with any single type. That makes sense because, unlike identity, remember always returns the value it was passed on its first invocation, which means its return value must always have the same type.   *)

(* OCaml will convert a weakly polymorphic variable to a concrete type as soon as it gets a clue as to what concrete type it is to be used as: *)

let remember_three () = remember 3

(* remember "avocado";; *)

let list_init_10 = List.init 10

(* val list_init_10 : f:(int -> '_weak3) -> '_weak3 list = <fun> *)

module Concat_list : sig
  type 'a t

  val empty : 'a t
  val singleton : 'a -> 'a t
  val concat : 'a t -> 'a t -> 'a t (* constant time *)
  val to_list : 'a t -> 'a list (* linear time   *)
end = struct
  type 'a t = Empty | Singleton of 'a | Concat of 'a t * 'a t

  let empty = Empty
  let singleton x = Singleton x
  let concat x y = Concat (x, y)

  let rec to_list_with_tail t tail =
    match t with
    | Empty -> tail
    | Singleton x -> x :: tail
    | Concat (x, y) -> to_list_with_tail x (to_list_with_tail y tail)

  let to_list t = to_list_with_tail t []
end

let x = Concat_list.empty

(* - : 'a Concat_list.t = <abstr> *)
let x = identity Concat_list.empty

(* - : '_weak6 Concat_list.t = <abstr> *)
(* In particular, if we replace type 'a t in the interface with type +'a t, that will make it explicit in the interface that the data structure doesn’t contain any persistent references to values of type 'a, at which point, OCaml can infer polymorphic types for expressions of this type that are not simple values: *)

let x = identity Concat_list.empty
(* - : 'a Concat_list.t = <abstr> *)
