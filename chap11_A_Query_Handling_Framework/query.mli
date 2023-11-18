module type Query_handler = sig
  type config

  val sexp_of_config : config -> Sexplib0.Sexp.t
  val config_of_sexp : Sexplib0.Sexp.t -> config
  val name : string

  type t

  val create : config -> t
  val eval : t -> Sexplib0.Sexp.t -> Sexplib0.Sexp.t Core.Or_error.t
end

type u = { a : int; b : float }

val u_of_sexp : Sexplib0.Sexp.t -> u
val sexp_of_u : u -> Sexplib0.Sexp.t

module type M = sig
  type t

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module Unique : sig
  type config = int

  val config_of_sexp : Sexplib0.Sexp.t -> config
  val sexp_of_config : config -> Sexplib0.Sexp.t

  type t = { mutable next_id : config }

  val name : string
  val create : int -> t
  val eval : t -> Sexplib0.Sexp.t -> (Sexplib0.Sexp.t, Base.Error.t) result
end

val unique : Unique.t

module List_dir : sig
  type config = string

  val config_of_sexp : Sexplib0.Sexp.t -> config
  val sexp_of_config : config -> Sexplib0.Sexp.t

  type t = { cwd : config }

  val is_abs : string -> bool
  val name : string
  val create : string -> t
  val eval : t -> Sexplib0.Sexp.t -> (Sexplib0.Sexp.t, Base.Error.t) result
end

val list_dir : List_dir.t

module type Query_handler_instance = sig
  module Query_handler : Query_handler

  val this : Query_handler.t
end

val unique_instance : (module Query_handler_instance)

val build_instance :
  (module Query_handler with type config = 'a) ->
  'a ->
  (module Query_handler_instance)

val unique_instance : (module Query_handler_instance)
val list_dir_instance : (module Query_handler_instance)

val build_dispatch_table :
  (module Query_handler_instance) list ->
  (string, (module Query_handler_instance)) Base.Hashtbl.t

val dispatch :
  (string, (module Query_handler_instance)) Base.Hashtbl.t ->
  Sexplib0.Sexp.t ->
  Sexplib0.Sexp.t Core.Or_error.t

val cli : (string, (module Query_handler_instance)) Base.Hashtbl.t -> unit

module Loader : sig
  type config = (module Query_handler) list

  val config_of_sexp : Sexplib0.Sexp.t -> config
  val sexp_of_config : config -> Sexplib0.Sexp.t

  type t = {
    known : (module Query_handler) Core.String.Table.t;
    active : (module Query_handler_instance) Core.String.Table.t;
  }

  val name : string
  val create : (module Query_handler) list -> t
  val load : t -> string -> Sexplib0.Sexp.t -> Sexplib0.Sexp.t Core.Or_error.t
  val unload : t -> string -> Sexplib0.Sexp.t Core.Or_error.t

  type request =
    | Load of string * Sexplib0.Sexp.t
    | Unload of string
    | Known_services
    | Active_services

  val request_of_sexp : Sexplib0.Sexp.t -> request
  val sexp_of_request : request -> Sexplib0.Sexp.t
  val eval : t -> Sexplib0.Sexp.t -> (Sexplib0.Sexp.t, Base.Error.t) result
end
