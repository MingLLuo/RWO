(* Functors are, roughly speaking, functions from modules to modules, and they can be used to solve a variety of code-structuring problems
   Dependency injection
   Makes the implementations of some components of a system swappable. This is particularly useful when you want to mock up parts of your system for testing and simulation purposes.
   Autoextension of modules
   Functors give you a way of extending existing modules with new functionality in a standardized way. For example, you might want to add a slew of comparison operators derived from a base comparison function. To do this by hand would require a lot of repetitive code for each type, but functors let you write this logic just once and apply it to many different types.
   Instantiating modules with state
   Modules can contain mutable states, and that means that you’ll occasionally want to have multiple instantiations of a particular module, each with its own separate and independent mutable state. Functors let you automate the construction of such modules.
*)

open Base

module type X_int = sig
  val x : int
end

module Increment (M : X_int) : X_int = struct
  let x = M.x + 1
end

module Three = struct
  let x = 3
end

module Four = Increment (Three)

let x = Four.x - Three.x

(* the module type can omit some information available in the module, either by dropping fields or by leaving some fields abstract. *)
module Three_and_more = struct
  let x = 3
  let y = "three"
end

module Four1 = Increment (Three_and_more)
