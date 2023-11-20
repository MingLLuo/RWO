open Re
open Core_unix
open Time_ns_unix
open Core

type service_info = { service_name : string; port : int; protocol : string }

let service_info_of_string line =
  let matches =
    let pat = "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)" in
    Re.exec (Re.Posix.compile_pat pat) line
  in
  {
    service_name = Re.Group.get matches 1;
    port = Int.of_string (Re.Group.get matches 2);
    protocol = Re.Group.get matches 3;
  }

let ssh = service_info_of_string "ssh 22/udp # SSH Remote Login Protocol"

type 'a with_line_num = { item : 'a; line_num : int }

let parse_lines parse file_contents =
  let lines = String.split ~on:'\n' file_contents in
  List.mapi lines ~f:(fun line_num line ->
      { item = parse line; line_num = line_num + 1 })

let x =
  parse_lines service_info_of_string
    "rtmp              1/ddp     # Routing Table Maintenance Protocol\n\
    \   tcpmux            1/udp     # TCP Port Service Multiplexer\n\
    \   tcpmux            1/tcp     # TCP Port Service Multiplexer"

let x = parse_lines Int.of_string "1\n10\n100\n1000"

let service_info_to_string { service_name = name; port; protocol = prot } =
  sprintf "%s %i/%s" name port prot

type service_info1 = {
  service_name : string;
  port : int;
  protocol : string;
  comment : string option;
}

let service_info_to_string { service_name = name; port; protocol = prot; _ } =
  sprintf "%s %i/%s" name port prot

(* when name of a variable coincide with the name of a record field, OCaml provide some shortcut, like field punning *)

let service_info_to_string { service_name; port; protocol; comment } =
  let base = sprintf "%s %i/%s" service_name port protocol in
  match comment with None -> base | Some text -> base ^ " #" ^ text

let service_info_of_string line =
  (* first, split off any comment *)
  let line, comment =
    match String.rsplit2 line ~on:'#' with
    | None -> (line, None)
    | Some (ordinary, comment) -> (ordinary, Some comment)
  in
  (* now, use a regular expression to break up the
     service definition *)
  let matches =
    Re.exec (Re.Posix.compile_pat "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)") line
  in
  let service_name = Re.Group.get matches 1 in
  let port = Int.of_string (Re.Group.get matches 2) in
  let protocol = Re.Group.get matches 3 in
  { service_name; port; protocol; comment }

let create_service_info ~service_name ~port ~protocol ~comment =
  { service_name; port; protocol; comment }

type log_entry = {
  session_id : string;
  time : Time_ns.t;
  important : bool;
  message : string;
}

type heartbeat = {
  session_id : string;
  time : Time_ns.t;
  status_message : string;
}

type logon = {
  session_id : string;
  time : Time_ns.t;
  user : string;
  credentials : string;
}

(* OCaml just pick the most recent definition of that record field *)
let get_session_id t = t.session_id

(* deal with annotation *)
let get_heartbeat_session_id (t : heartbeat) = t.session_id
let status_and_session t = (t.status_message, t.session_id)

(* will fail because of wrong inference *)
(* let session_and_status t = (t.session_id, t.status_message);; *)

module Log_entry = struct
  type t = {
    session_id : string;
    time : Time_ns.t;
    important : bool;
    message : string;
  }
end

module Heartbeat = struct
  type t = { session_id : string; time : Time_ns.t; status_message : string }
end

module Logon = struct
  type t = {
    session_id : string;
    time : Time_ns.t;
    user : string;
    credentials : string;
  }
  [@@deriving fields]
end

let create_log_entry ~session_id ~important message =
  {
    Log_entry.time = Time_ns.now ();
    Log_entry.session_id;
    Log_entry.important;
    Log_entry.message;
  }

let create_log_entry ~session_id ~important message : Log_entry.t =
  { time = Time_ns.now (); session_id; important; message }

let message_to_string { Log_entry.important; message; _ } =
  if important then String.uppercase message else message

let is_important t = t.Log_entry.important

type client_info = {
  addr : Core_unix.Inet_addr.t;
  port : int;
  user : string;
  credentials : string;
  last_heartbeat_time : Time_ns.t;
  last_heartbeat_status : string;
}

let register_heartbeat t hb =
  {
    addr = t.addr;
    port = t.port;
    user = t.user;
    credentials = t.credentials;
    last_heartbeat_time = hb.Heartbeat.time;
    last_heartbeat_status = hb.Heartbeat.status_message;
  }

(* use OCaml functional update syntax *)
let register_heartbeat t hb = { t with last_heartbeat_time = hb.Heartbeat.time }

type client_info1 = {
  addr : Core_unix.Inet_addr.t;
  port : int;
  user : string;
  credentials : string;
  mutable last_heartbeat_time : Time_ns.t;
  mutable last_heartbeat_status : string;
}

let register_heartbeat t (hb : Heartbeat.t) =
  t.last_heartbeat_time <- hb.time;
  t.last_heartbeat_status <- hb.status_message

let get_users logons =
  List.dedup_and_sort ~compare:String.compare
    (List.map logons ~f:(fun x -> x.Logon.user))

(* let get_users logons =
   List.dedup_and_sort ~compare:String.compare (List.map logons ~f:Logon.user) *)

let show_field field to_string record =
  let name = Field.name field in
  let field_string = to_string (Field.get field record) in
  name ^ ": " ^ field_string

let logon =
  {
    Logon.session_id = "26685";
    time = Time_ns.of_string "2017-07-21 10:11:45Z";
    user = "yminsky";
    credentials = "Xy2d9W";
  }

(* let () = print_endline ( show_field Logon.Fields.user Fn.id logon)
   let print_logon logon =
     let print to_string field =
       printf "%s\n" (show_field field to_string logon)
     in
     Logon.Fields.iter
       ~session_id:(print Fn.id)
       ~time:(print Time_ns.to_string)
       ~user:(print Fn.id)
       ~credentials:(print Fn.id);; *)
