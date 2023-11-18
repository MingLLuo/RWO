val sum_file_sizes : unit -> int

module type Pipeline = sig
  type ('input, 'output) t

  val ( @> ) : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  val empty : ('a, 'a) t
end

module Example_pipeline : functor (Pipeline : Pipeline) -> sig
  val sum_file_sizes : (unit, int) Pipeline.t
end

module Basic_pipeline : sig
  type ('input, 'output) t

  val ( @> ) : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  val empty : ('a, 'a) t
  val exec : ('a, 'b) t -> 'a -> 'b
end

type (_, _) pipeline =
  | Step : ('a -> 'b) * ('b, 'c) pipeline -> ('a, 'c) pipeline
  | Empty : ('a, 'a) pipeline

val ( @> ) : ('a -> 'b) -> ('b, 'c) pipeline -> ('a, 'c) pipeline
val empty : ('a, 'a) pipeline
val exec : ('a, 'b) pipeline -> 'a -> 'b
val exec_with_profile : ('a, 'b) pipeline -> 'a -> 'b * Core.Time_ns.Span.t list
