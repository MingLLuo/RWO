open Core
open Async
(* In Async, well-behaved functions never block. Instead, they return a value of type Deferred.t that acts as a placeholder that will eventually be filled in with the result. As an example, consider the signature of the Async equivalent of In_channel.read_all.   *)

(* #show Reader.file_contents;;
   val file_contents : string -> string Deferred.t *)

(* let contents = Reader.file_contents "test.txt";;
   val contents : string Deferred.t = <abstr>
   Deferred.peek contents;;
   - : string option = None
   contents;;
   - : string = "This is only a test."
*)

(* utop knows about Async and can start the scheduler automatically. More than that, utop knows about deferred values, and when you type in an expression of type Deferred.t, it will make sure the scheduler is running and block until the deferred is determined. *)

(* #show Deferred.bind;;
   val bind : 'a Deferred.t -> f:('a -> 'b Deferred.t) -> 'b Deferred.t *)

(* bind is effectively a way of sequencing concurrent computations. In particular, Deferred.bind d ~f causes f to be called after the value of d has been determined.  *)

let uppercase_file filename =
  Deferred.bind (Reader.file_contents filename) ~f:(fun text ->
      Writer.save filename ~contents:(String.uppercase text))
(* bind is acting as a sequencing operator, causing the file to be saved via the call to Writer.save only after the contents of the file were first read via Reader.file_contents. *)

let uppercase_file filename =
  Reader.file_contents filename >>= fun text ->
  Writer.save filename ~contents:(String.uppercase text)

(* return, a function provided by Async that takes an ordinary value and wraps it up in a deferred.   *)
let three = return 3
let three = Deferred.value_exn three

let count_lines filename =
  Reader.file_contents filename >>= fun text ->
  return (List.length (String.split text ~on:'\n'))

(* Calling bind and return together is a fairly common pattern, and as such there is a standard shortcut for it called Deferred.map, and comes with its own infix equivalent, >>|. *)

let count_lines filename =
  Reader.file_contents filename >>| fun text ->
  List.length (String.split text ~on:'\n')
(* let x = (count_lines "/etc/hosts") >>| Int.to_string |> Deferred.value_exn |> Caml.print_endline *)

let count_lines filename =
  let%bind text = Reader.file_contents filename in
  return (List.length (String.split text ~on:'\n'))

let count_lines filename =
  let%map text = Reader.file_contents filename in
  List.length (String.split text ~on:'\n')

(* Deferreds are usually built using combinations of bind, map and return, but sometimes you want to construct a deferred where you can programmatically decide when it gets filled in. This can be done using an ivar. (The term ivar dates back to a language called Concurrent ML that was developed by John Reppy in the early ’90s. The “i” in ivar stands for incremental.)    *)

let ivar = Ivar.create ()
let def = Ivar.read ivar
let x = Deferred.peek def
let () = Ivar.fill ivar "Hello"
let x = Deferred.peek def

(* scheduling a sequence of actions that would run after a fixed delay *)
module type Delayer_intf = sig
  type t

  val create : Time.Span.t -> t
  val schedule : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t
end

module Delayer : Delayer_intf = struct
  type t = { delay : Time.Span.t; jobs : (unit -> unit) Queue.t }

  let create delay = { delay; jobs = Queue.create () }

  (* upon schedules a callback to be executed when the deferred it is passed is determined; but unlike those calls, it doesn’t create a new deferred for this callback to fill. *)
  let schedule t thunk =
    let ivar = Ivar.create () in
    Queue.enqueue t.jobs (fun () -> upon (thunk ()) (fun x -> Ivar.fill ivar x));
    upon (after t.delay) (fun () ->
        let job = Queue.dequeue_exn t.jobs in
        job ());
    Ivar.read ivar
end

let my_bind d ~f =
  let i = Ivar.create () in
  upon d (fun x -> upon (f x) (fun y -> Ivar.fill i y));
  Ivar.read i

let do_stuff n =
  let x = 3 in
  if n > 0 then never_returns (Scheduler.go ());
  x + n

let maybe_raise =
  let should_fail = ref false in
  fun () ->
    let will_fail = !should_fail in
    should_fail := not will_fail;
    let%map () = after (Time.Span.of_sec 0.5) in
    if will_fail then raise Exit else ()

(* can't capture asynchronous error  *)
let handle_error () =
  try
    let%map () = maybe_raise () in
    "success"
  with _ -> return "failure"

let handle_error () =
  match%map try_with (fun () -> maybe_raise ()) with
  | Ok () -> "success"
  | Error _ -> "failure"
