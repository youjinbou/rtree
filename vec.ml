module type SCALAR =
sig
  type t
  val  zero   : t
  val  one    : t
  val  add    : t -> t -> t
  val  sub    : t -> t -> t
  val  mul    : t -> t -> t
  val  div    : t -> t -> t
  val  opp    : t -> t
  val  rand   : t -> t
  val  modulo : t -> t -> t
  val  epsilon: t
  val  abs    : t -> t
  val  sqrt   : t -> t
  val  to_string : t -> string
end

module type VEC =
sig

  module Scalar : SCALAR

  type t

  val size : int
    
  val null : unit -> t
  val one  : unit -> t

  val get     : t -> int -> Scalar.t
  val add     : t -> t -> t
  val sub     : t -> t -> t
  val scale   : t -> Scalar.t -> t
  val dot     : t -> t -> Scalar.t
  val copy    : t -> t -> unit
  val random  : t -> t
  val modulo  : t -> Scalar.t -> t
  val below_epsilon : t -> bool
  val for_all    : (Scalar.t -> bool) -> t -> bool
  val map        : (Scalar.t -> Scalar.t) -> t -> t
  val map2       : (Scalar.t -> Scalar.t -> Scalar.t) -> t -> t -> t
  val fold_left  : ('a -> Scalar.t -> 'a) -> 'a -> t -> 'a
  val fold_right : (Scalar.t -> 'a -> 'a) -> t -> 'a -> 'a
  val to_string  : t -> string

end
