open Base
open Stdio

type basic_color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

let basic_color_to_int = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7

(* let incomplete_color_to_int = function Black -> 0 | Red -> 1 | White -> 7 *)
let color_by_number number text =
  Printf.sprintf "\027[38;5;%dm%s\027[0m" number text

let blue = color_by_number (basic_color_to_int Blue) "Blue"
let () = printf "Hello %s World!\n" blue

type weight = Regular | Bold

type color =
  | Basic of basic_color * weight (* basic colors, regular and bold *)
  | RGB of int * int * int (* 6x6x6 color cube *)
  | Gray of int (* 24 grayscale levels *)

let color_to_int = function
  | Basic (basic_color, weight) ->
      let base = match weight with Bold -> 8 | Regular -> 0 in
      base + basic_color_to_int basic_color
  | RGB (r, g, b) -> 16 + b + (g * 6) + (r * 36)
  | Gray i -> 232 + i

let color_print color s = printf "%s\n" (color_by_number (color_to_int color) s)
let () = color_print (Basic (Red, Bold)) "A bold red!"
let () = color_print (Gray 4) "A muted gray..."

(* Variants with multiple arguments  *)
(* RGB (200, 200, 200) *)

let purple = (200, 0, 200)

(* RGB purple -> error, only one argu *)

(* The differences between a multi-argument variant and a variant containing a tuple are mostly about performance. A multi-argument variant is a single allocated block in memory, while a variant containing a tuple requires an extra heap-allocated block for the tuple. *)

(* record variants bind *)
module Time_ns = Core.Time_ns

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
end

type client_message =
  | Logon of Logon.t
  | Heartbeat of Heartbeat.t
  | Log_entry of Log_entry.t

let messages_for_user user messages =
  let user_messages, _ =
    List.fold messages
      ~init:([], Set.empty (module String))
      ~f:(fun ((messages, user_sessions) as acc) message ->
        match message with
        | Logon m ->
            if String.(m.user = user) then
              (message :: messages, Set.add user_sessions m.session_id)
            else acc
        | Heartbeat _ | Log_entry _ ->
            let session_id =
              match message with
              | Logon m -> m.session_id
              | Heartbeat m -> m.session_id
              | Log_entry m -> m.session_id
            in
            if Set.mem user_sessions session_id then
              (message :: messages, user_sessions)
            else acc)
  in
  List.rev user_messages

type details =
  | Logon of Logon.t
  | Heartbeat of Heartbeat.t
  | Log_entry of Log_entry.t

module Common = struct
  type t = { session_id : string; time : Time_ns.t }
end

let messages_for_user user (messages : (Common.t * details) list) =
  let user_messages, _ =
    List.fold messages
      ~init:([], Set.empty (module String))
      ~f:(fun
          ((messages, user_sessions) as acc) ((common, details) as message) ->
        match details with
        | Logon m ->
            if String.( = ) m.user user then
              (message :: messages, Set.add user_sessions common.session_id)
            else acc
        | Heartbeat _ | Log_entry _ ->
            if Set.mem user_sessions common.session_id then
              (message :: messages, user_sessions)
            else acc)
  in
  List.rev user_messages

(* type details =
   | Logon of { user : string; credentials : string }
   | Heartbeat of { status_message : string }
   | Log_entry of { important : bool; message : string } *)

let messages_for_user user (messages : (Common.t * details) list) =
  let user_messages, _ =
    List.fold messages
      ~init:([], Set.empty (module String))
      ~f:(fun
          ((messages, user_sessions) as acc) ((common, details) as message) ->
        match details with
        | Logon m ->
            if String.( = ) m.user user then
              (message :: messages, Set.add user_sessions common.session_id)
            else acc
        | Heartbeat _ | Log_entry _ ->
            if Set.mem user_sessions common.session_id then
              (message :: messages, user_sessions)
            else acc)
  in
  List.rev user_messages

(* an inline record can’t be treated as its own free-standing object *)
(* let get_logon_contents = function Logon m -> Some m | _ -> None *)

type 'a expr =
  | Base of 'a
  | Const of bool
  | And of 'a expr list
  | Or of 'a expr list
  | Not of 'a expr

type mail_field = To | From | CC | Date | Subject
type mail_predicate = { field : mail_field; contains : string }

let test field contains = Base { field; contains }

let rec eval expr base_eval =
  (* a shortcut, so we don't need to repeatedly pass [base_eval]
     explicitly to [eval] *)
  let eval' expr = eval expr base_eval in
  match expr with
  | Base base -> base_eval base
  | Const bool -> bool
  | And exprs -> List.for_all exprs ~f:eval'
  | Or exprs -> List.exists exprs ~f:eval'
  | Not expr -> not (eval' expr)

(* simplification *)
let and_ l =
  if List.exists l ~f:(function Const false -> true | _ -> false) then
    Const false
  else
    match List.filter l ~f:(function Const true -> false | _ -> true) with
    | [] -> Const true
    | [ x ] -> x
    | l -> And l

let or_ l =
  if List.exists l ~f:(function Const true -> true | _ -> false) then Const true
  else
    match List.filter l ~f:(function Const false -> false | _ -> true) with
    | [] -> Const false
    | [ x ] -> x
    | l -> Or l

let not_ = function
  | Const b -> Const (not b)
  | Not e -> e
  | (Base _ | And _ | Or _) as e -> Not e

let rec simplify = function
  | (Base _ | Const _) as x -> x
  | And l -> and_ (List.map ~f:simplify l)
  | Or l -> or_ (List.map ~f:simplify l)
  | Not e -> not_ (simplify e)

(* Polymorphic Variants *)
let three = `Int 3

(* val three : [> `Int of int ] = `Int 3 *)
let four = `Float 4.
let nan = `Not_a_number

(* [ three; four; nan ] *)

(* The > at the beginning of the variant types above is critical because it marks the types as being open to combination with other variant types. *)

let is_positive = function
  | `Int i -> i > 0
  | `Float f -> Float.(f > 0.)
  | `Not_a_number -> false

let exact = List.filter ~f:is_positive [ three; four ]
(* val exact : [ `Float of float | `Int of int ] list = [`Int 3; `Float 4.] *)
