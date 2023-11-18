open Core

let sum_file_sizes () =
  Sys_unix.ls_dir "."
  |> List.filter ~f:Sys_unix.is_file_exn
  |> List.map ~f:(fun file_name -> (Core_unix.lstat file_name).st_size)
  |> List.sum (module Int) ~f:Int64.to_int_exn

(* This works well enough, but the advantage of a custom pipeline type is that it lets you build extra services beyond basic execution of the pipeline, e.g.:

   - Profiling, so that when you run a pipeline, you get a report of how long each step of the pipeline took.
   - Control over execution, like allowing users to pause the pipeline mid-execution, and restart it later.
   - Custom error handling, so, for example, you could build a pipeline that kept track of where it failed, and offered the possibility of restarting it. *)
module type Pipeline = sig
  type ('input, 'output) t

  val ( @> ) : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  val empty : ('a, 'a) t
end

module Example_pipeline (Pipeline : Pipeline) = struct
  open Pipeline

  let sum_file_sizes =
    (fun () -> Sys_unix.ls_dir ".")
    @> List.filter ~f:Sys_unix.is_file_exn
    @> List.map ~f:(fun file_name -> (Core_unix.lstat file_name).st_size)
    @> List.sum (module Int) ~f:Int64.to_int_exn
    @> empty
end

module Basic_pipeline : sig
  include Pipeline

  val exec : ('a, 'b) t -> 'a -> 'b
end = struct
  type ('input, 'output) t = 'input -> 'output

  let empty = Fn.id
  let ( @> ) f t input = t (f input)
  let exec t input = t input
end

type (_, _) pipeline =
  | Step : ('a -> 'b) * ('b, 'c) pipeline -> ('a, 'c) pipeline
  | Empty : ('a, 'a) pipeline

let ( @> ) f pipeline = Step (f, pipeline)
let empty = Empty

let rec exec : type a b. (a, b) pipeline -> a -> b =
 fun pipeline input ->
  match pipeline with Empty -> input | Step (f, tail) -> exec tail (f input)

let exec_with_profile pipeline input =
  let rec loop :
      type a b.
      (a, b) pipeline -> a -> Time_ns.Span.t list -> b * Time_ns.Span.t list =
   fun pipeline input rev_profile ->
    match pipeline with
    | Empty -> (input, rev_profile)
    | Step (f, tail) ->
        let start = Time_ns.now () in
        let output = f input in
        let elapsed = Time_ns.diff (Time_ns.now ()) start in
        loop tail output (elapsed :: rev_profile)
  in
  let output, rev_profile = loop pipeline input [] in
  (output, List.rev rev_profile)

(* The more abstract GADT approach for creating a little combinator library like this has several advantages over having combinators that build a more concrete computational machine:

   The core types are simpler, since they are typically built out of GADT tags that are just reflections of the types of the base combinators.

   The design is more modular, since your core types don’t need to contemplate every possible use you want to make of them.

   The code tends to be more efficient, since the more concrete approach typically involves allocating closures to wrap up the necessary functionality, and closures are more heavyweight than GADT tags. *)
