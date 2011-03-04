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
(* arithmetic modules for primitive types *)

module Float =
struct
  type t = float
  let  zero = 0.
  let  one  = 1.
  let  add  = ( +. )
  let  sub  = ( -. )
  let  mul  = ( *. )
  let  div  = ( /. )
  let  rand = Random.float
  let  opp x = -.x
  let  modulo x m = if x <= (-. m) then x +. m else if x > m then x -. m else x
  let  pi = acos(0.0) *. 2.0
  let  epsilon = epsilon_float
  let  acos    = acos
  let  asin    = asin
  let  cos     = cos
  let  sin     = sin
  let  power   = ( ** )
  let  abs     = abs_float
  let compare a b = let c = a -. b in if c < 0.0 then (-1) else if c > 0.0 then 1 else 0
  let  sqrt    = sqrt
  let  to_string = string_of_float
end

module Int   =
struct
  type t = int
  let  zero = 0
  let  one  = 1
  let  add  = ( + )
  let  sub  = ( - )
  let  mul  = ( * )
  let  div  = ( / )
  let  rand = Random.int
  let  opp x = -x
  let  modulo  = ( mod )
  let  epsilon = 0
  let  abs   = abs
  let  compare a b = a - b
  let  sqrt v  = int_of_float (sqrt ((float) v))
  let to_string = string_of_int
end
