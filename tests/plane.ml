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
open Primitives

module FVec3 = Vec3.Make(Float)

let ( + ) = Float.add
let ( - ) = Float.sub
let ( * ) = Float.mul
let ( / ) = Float.div
let opp   = Float.opp
let abs   = Float.abs

type t = FVec3.t * float * int

let signbits n =
  let signbit k = if k < 0.0 then 1 else 0
  in
  let x,y,z = FVec3.to_tuple n
  in
    (signbit x) lor ((signbit y) lsl 1) lor ((signbit z) lsl 2)

let make n p = 
  let k = FVec3.dot n p 
  and v = FVec3.clone n
  and s = signbits n
  in
    v, k, s

let pos (n,k,s) v = 
  (FVec3.dot n v) -. k

let inside pl v =
  pos pl v = 0.

let above pl v =
  pos pl v > 0.

let in_or_above pl v =
  pos pl v >= 0.

let below pl v =
  pos pl v < 0.


let check_bbox (n,k,s) (b,t) =
  let dot (x1,y1,z1) (x2,y2,z2) = x1 *. x2 +. y1 *. y2 +. z1 *. z2
  and xb,yb,zb = FVec3.to_tuple b
  and xt,yt,zt = FVec3.to_tuple t
  and xn,yn,zn = FVec3.to_tuple n
  in
  let d1, d2 = 
    match s with
	0 -> 
	  dot (xn,yn,zn) (xt,yt,zt),
	  dot (xn,yn,zn) (xb,yb,zb)
      | 1 ->
	  dot (xn,yn,zn) (xb,yt,zt),
	  dot (xn,yn,zn) (xt,yb,zb)
      | 2 ->
	  dot (xn,yn,zn) (xt,yb,zt),
	  dot (xn,yn,zn) (xb,yt,zb)
      | 3 ->
	  dot (xn,yn,zn) (xb,yb,zt),
	  dot (xn,yn,zn) (xt,yt,zb)
      | 4 ->
	  dot (xn,yn,zn) (xt,yt,zb),
	  dot (xn,yn,zn) (xb,yb,zt)
      | 5 ->
	  dot (xn,yn,zn) (xb,yt,zb),
	  dot (xn,yn,zn) (xt,yb,zt)
      | 6 ->
	  dot (xn,yn,zn) (xt,yb,zb),
	  dot (xn,yn,zn) (xb,yt,zt)
      | _ ->
	  dot (xn,yn,zn) (xb,yb,zb),
	  dot (xn,yn,zn) (xt,yt,zt)
  in
    (* 
       4 cases :
       - top below plane and bottom above plane (0) impossible
       - top above plane and bottom above plane (1)
       - top below plane and bottom below plane (2)
       - top above plane and bottom below plane (3)
       d1 >= k
       ->  top above plane
       d2 < k 
       ->  bottom below plane
       we want to avoid 2 for all planes
    *)
  let r = (if d1 >= k then 1 else 0) lor (if d2 < k then 2 else 0)
  in if r = 0 then invalid_arg "check_bbox : returning 0" else r
    
  

