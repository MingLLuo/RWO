open Core
open Async

(* Generate a DuckDuckGo search URI from a query string *)
(* let query_uri query =
   let base_uri = Uri.of_string "http://api.duckduckgo.com/?format=json" in
   Uri.add_query_param base_uri ("q", [ query ]) *)

(* Generate a DuckDuckGo search URI from a query string *)
let query_uri ~server query =
  let base_uri =
    Uri.of_string (String.concat [ "http://"; server; "/?format=json" ])
  in
  Uri.add_query_param base_uri ("q", [ query ])

(* Extract the "Definition" or "Abstract" field from the DuckDuckGo
   results *)
let get_definition_from_json json =
  match Yojson.Safe.from_string json with
  | `Assoc kv_list -> (
      let find key =
        match List.Assoc.find ~equal:String.equal kv_list key with
        | None | Some (`String "") -> None
        | Some s -> Some (Yojson.Safe.to_string s)
      in
      match find "Abstract" with Some _ as x -> x | None -> find "Definition")
  | _ -> None

(* Execute the DuckDuckGo search *)
(* let get_definition word =
   let%bind _, body = Cohttp_async.Client.get (query_uri word) in
   let%map string = Cohttp_async.Body.to_string body in
   (word, get_definition_from_json string) *)

(* Execute the DuckDuckGo search *)
let get_definition ~server word =
  match%map
    try_with (fun () ->
        let%bind _, body = Cohttp_async.Client.get (query_uri ~server word) in
        let%map string = Cohttp_async.Body.to_string body in
        (word, get_definition_from_json string))
  with
  | Ok (word, result) -> (word, Ok result)
  | Error _ -> (word, Error "Unexpected failure")

(* Print out a word/definition pair *)
let print_result (word, definition) =
  printf "%s\n%s\n\n%s\n\n" word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    (match definition with
    | Error s -> "DuckDuckGo query failed: " ^ s
    | Ok None -> "No definition found"
    | Ok (Some def) ->
        String.concat ~sep:"\n" (Wrapper.wrap (Wrapper.make 70) def))

(* Run many searches in parallel, printing out the results as you
   go *)
let search_and_print ~servers words =
  let servers = Array.of_list servers in
  Deferred.all
    (List.mapi words ~f:(fun i word ->
         let server = servers.(i mod Array.length servers) in
         get_definition ~server word))
  >>| fun results -> List.iter results ~f:print_result

let () =
  Command_unix.run
    (Command.async ~summary:"Retrieve definitions from duckduckgo search engine"
       (let open Command.Let_syntax in
        let%map_open servers =
          flag "servers" (listed string) ~doc:"SERVERS list of servers"
        and words = anon (sequence ("WORD" %: string)) in
        fun () -> search_and_print ~servers words))
