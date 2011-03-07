(*
  a rtree library

  Copyright (C) 2010  Didier Cassirame

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

*)

module type SCALAR =
sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val opp : t -> t
  val rand : t -> t
  val modulo : t -> t -> t
  val epsilon : t
  val abs : t -> t
  val sqrt : t -> t
  val to_string : t -> string
end

module type T =
sig
  module Scalar : SCALAR
  type t
  type tuple_t
  val size : int
  val null : unit -> t
  val one : unit -> t
  val unit : int -> t
  val of_tuple : tuple_t -> t
  val to_tuple : t -> tuple_t
  val get : t -> int -> Scalar.t
  val set : t -> int -> Scalar.t -> unit
  val map : (Scalar.t -> Scalar.t) -> t -> t
  val map2 :
    (Scalar.t -> Scalar.t -> Scalar.t) -> t -> t -> t
  val map3 :
    (Scalar.t -> Scalar.t -> Scalar.t -> Scalar.t) ->
    t -> t -> t -> t
  val map4 :
    (Scalar.t ->
       Scalar.t -> Scalar.t -> Scalar.t -> Scalar.t) ->
    t -> t -> t -> t -> t
  val opp : t -> t
  val add : t -> t -> t
  val add3 : t -> t -> t -> t
  val add4 : t -> t -> t -> t -> t
  val sub : t -> t -> t
  val sub3 : t -> t -> t -> t
  val sub4 : t -> t -> t -> t -> t
  val scale : t -> Scalar.t -> t
  val muladd : t -> Scalar.t -> t -> t
  val dot : t -> t -> Scalar.t
  val cross : t -> t -> t
  val copy : t -> t -> unit
  val random : t -> t
  val modulo : t -> Scalar.t -> t
  val below_epsilon : t -> bool
  val for_all : (Scalar.t -> bool) -> t -> bool
  val fold_left :
    ('a -> Scalar.t -> 'a) -> 'a -> t -> 'a
  val fold_right :
    (Scalar.t -> 'a -> 'a) -> t -> 'a -> 'a
  val to_string : t -> string
end
