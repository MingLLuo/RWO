(* Auto-generated from "github_org.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type org = Github_org_t.org = {
  login : string;
  id : int;
  url : string;
  name : string option;
  blog : string option;
  email : string option;
  public_repos : int;
}

val write_org : Buffer.t -> org -> unit
(** Output a JSON value of type {!type:org}. *)

val string_of_org : ?len:int -> org -> string
(** Serialize a value of type {!type:org}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_org : Yojson.Safe.lexer_state -> Lexing.lexbuf -> org
(** Input JSON data of type {!type:org}. *)

val org_of_string : string -> org
(** Deserialize JSON data of type {!type:org}. *)
