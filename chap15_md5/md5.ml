(* open Core
    (* without limit *)
   let do_hash file = Md5.digest_file_blocking file |> Md5.to_hex |> print_endline

   let command =
     Command.basic ~summary:"Generate an MD5 hash of the input data"
       ~readme:(fun () -> "More detailed information")
       Command.Param.(
         map (anon ("filename" %: string)) ~f:(fun filename () -> do_hash filename))

   let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command *)
(*
   open Core
   (* with length limited *)
   let do_hash hash_length filename =
     Md5.digest_file_blocking filename
     |> Md5.to_hex
     |> (fun s -> String.prefix s hash_length)
     |> print_endline

   (*
      let command =
        Command.basic ~summary:"Generate an MD5 hash of the input data"
          ~readme:(fun () -> "More detailed information")
          Command.Param.(
            map
              (both (anon ("hash_length" %: int)) (anon ("filename" %: string)))
              ~f:(fun (hash_length, filename) () -> do_hash hash_length filename)) *)

   (* let command =
      Command.basic ~summary:"Generate an MD5 hash of the input data"
        ~readme:(fun () -> "More detailed information")
        (let%map_open.Command hash_length = anon ("hash_length" %: int)
         and filename = anon ("filename" %: string) in
         fun () -> do_hash hash_length filename) *)
   (*
      let command =
        Command.basic ~summary:"Generate an MD5 hash of the input data"
          ~readme:(fun () -> "More detailed information")
          (let%map_open.Command hash_length = anon ("hash_length" %: int)
            and filename = anon ("filename" %: Filename_unix.arg_type) in
            fun () -> do_hash hash_length filename) *)

   (* open Core

      let do_hash file = Md5.digest_file_blocking file |> Md5.to_hex |> print_endline

      let regular_file =
        Command.Arg_type.create (fun filename ->
            match Sys_unix.is_file filename with
            | `Yes -> filename
            | `No -> failwith "Not a regular file"
            | `Unknown -> failwith "Could not determine if this was a regular file")

      let command =
        Command.basic ~summary:"Generate an MD5 hash of the input data"
          ~readme:(fun () -> "More detailed information")
          (let%map_open.Command filename = anon ("filename" %: regular_file) in
           fun () -> do_hash filename) *) *)
(* allow stdin *)
(*
open Core

let get_contents = function
  | "-" -> In_channel.input_all In_channel.stdin
  | filename -> In_channel.read_all filename

let do_hash filename =
  get_contents filename |> Md5.digest_string |> Md5.to_hex |> print_endline

let command =
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command filename =
       anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type))
     in
     fun () -> do_hash filename)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command *)
(*
   (* allow files *)
   open Core

   let get_contents = function
     | "-" -> In_channel.input_all In_channel.stdin
     | filename -> In_channel.read_all filename

   let do_hash filename =
     get_contents filename
     |> Md5.digest_string
     |> Md5.to_hex
     |> fun md5 -> printf "MD5 (%s) = %s\n" filename md5

   let command =
     Command.basic
       ~summary:"Generate an MD5 hash of the input data"
       ~readme:(fun () -> "More detailed information")
       (let%map_open.Command files =
          anon (sequence ("filename" %: Filename_unix.arg_type))
        in
        fun () ->
          match files with
          | [] -> do_hash "-"
          | _ -> List.iter files ~f:do_hash)

   let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command *)

(* with labeled flags *)
open Core

let checksum_from_string buf =
  Md5.digest_string buf |> Md5.to_hex |> print_endline

let checksum_from_file filename =
  let contents =
    match filename with
    | "-" -> In_channel.input_all In_channel.stdin
    | filename -> In_channel.read_all filename
  in
  Md5.digest_string contents |> Md5.to_hex |> print_endline

let command =
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    (let%map_open.Command use_string =
       flag "-s" (optional string) ~doc:"string Checksum the given string"
     and trial = flag "-t" no_arg ~doc:" run a built-in time trial"
     and filename =
       anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type))
     in
     fun () ->
       if trial then printf "Running time trial\n"
       else
         match use_string with
         | Some buf -> checksum_from_string buf
         | None -> checksum_from_file filename)

let () = Command_unix.run command
