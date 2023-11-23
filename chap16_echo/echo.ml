open Core
open Async

(* Copy data from the reader to the writer, using the provided buffer
   as scratch space *)
let rec copy_blocks buffer r w =
  match%bind Reader.read r buffer with
  | `Eof -> return ()
  | `Ok bytes_read ->
      Writer.write w (Bytes.to_string buffer) ~len:bytes_read;
      let%bind () = Writer.flushed w in
      copy_blocks buffer r w

(** Starts a TCP server, which listens on the specified port, invoking
    copy_blocks every time a client connects. *)
let run () =
  let host_and_port =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port 8765) (fun _addr r w ->
        let buffer = Bytes.create (16 * 1024) in
        copy_blocks buffer r w)
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

(* Call [run], and then start the scheduler *)
(* let () =
   run ();
   never_returns (Scheduler.go ()) *)
(* #show Scheduler.go;;
   val go : ?raise_unhandled_exn:bool -> unit -> never_returns *)
(* The point of never_returns is to create an explicit marker so the user knows that the function in question doesn’t return. *)

(* Improved *)

let run ~uppercase ~port =
  let host_and_port =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port) (fun _addr r w ->
        Pipe.transfer (Reader.pipe r) (Writer.pipe w)
          ~f:(if uppercase then String.uppercase else Fn.id))
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()

let () =
  Command.async ~summary:"Start an echo server"
    (let%map_open.Command uppercase =
       flag "-uppercase" no_arg ~doc:" Convert to uppercase before echoing back"
     and port =
       flag "-port"
         (optional_with_default 8765 int)
         ~doc:" Port to listen on (default 8765)"
     in
     fun () -> run ~uppercase ~port)
  |> Command_unix.run
