open Base
(* The order of declarations in the mli doesn’t need to match the order of declarations in the ml. *)

type t
(** A collection of string frequency counts *)

val empty : t
(** The empty set of frequency counts *)

val touch : t -> string -> t
(** Bump the frequency count for the given string. *)

val to_list : t -> (string * int) list
(** Converts the set of frequency counts to an association list. A
    string shows up at most once, and the counts are >= 1. *)

(** Represents the median computed from a set of strings. In the case
    where there is an even number of choices, the one before and after
    the median is returned. *)
type median = Median of string | Before_and_after of string * string

val median : t -> median
