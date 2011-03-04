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
(* very simple 3D vector module *)
module Make (T: Vec.SCALAR ) =
struct

  module Scalar = T
    
  type t = T.t array

  type tuple_t = T.t * T.t * T.t

  let ( + ) = T.add
  let ( - ) = T.sub
  let ( * ) = T.mul
  let ( / ) = T.div
  let abs   = T.abs

  let size = 3

  let get v i = v.(i)

  let set v i s = v.(i) <- s

  let map f v1 =
    [| f v1.(0) ; f v1.(1) ; f v1.(2) |]

  let map2 f v1 v2 = 
    [| f v1.(0) v2.(0) ; f v1.(1) v2.(1) ; f v1.(2) v2.(2) |]

  let map3 f v1 v2 v3 = 
    [| f v1.(0) v2.(0) v3.(0) ; f v1.(1) v2.(1) v3.(1) ; f v1.(2) v2.(2) v3.(2) |]

  let map4 f v1 v2 v3 v4 = 
    [| f v1.(0) v2.(0) v3.(0) v4.(0) ; f v1.(1) v2.(1) v3.(1) v4.(1) ; f v1.(2) v2.(2) v3.(2) v4.(2) |]

  let null () = Array.make size T.zero

  let unit i  = let u = null () in u.(pred i) <- T.one ; u

  let one  () = Array.make size T.one

  let opp v   = map (fun x -> T.opp x) v

  let add v1 v2 = 
    map2 T.add v1 v2

  let add3 v1 v2 v3 = 
    map3 (fun x y z -> x + y + z) v1 v2 v3

  let add4 v1 v2 v3 v4 = 
    map4 (fun x y z w -> x + y + z + w) v1 v2 v3 v4
      
  let sub v1 v2 = 
    map2 T.sub v1 v2

  let sub3 v1 v2 v3 = 
    map3 (fun x y z -> x - y - z) v1 v2 v3

  let sub4 v1 v2 v3 v4 = 
    map4 (fun x y z w -> x - y - z - w) v1 v2 v3 v4
      
  let scale v1 s = 
    map (fun x -> x * s) v1

  let muladd v1 s v2 =
    map2 (fun x y -> x * s + y) v1 v2

  let dot2d v1 v2 = v1.(0) * v2.(0) + v1.(1) * v2.(1)

  let dot3d v1 v2 = v1.(0) * v2.(0) + v1.(1) * v2.(1) + v1.(2) * v2.(2)

  let dot = dot3d

  let cross v1 v2 = [|
      (v1.(1) * v2.(2)) - (v1.(2) * v2.(1)) ;
      (v1.(2) * v2.(0)) - (v1.(0) * v2.(2)) ;
      (v1.(0) * v2.(1)) - (v1.(1) * v2.(0)) 
  |]

  let clone v = Array.copy v

  let copy v1 v2 = Array.blit v1 0 v2 0 size

  let to_tuple v = (v.(0),v.(1),v.(2))

  let of_tuple (x,y,z) = [| x; y; z |]

  let random v = map (fun x -> if x = T.zero then T.zero else T.rand x) v 

  let modulo v m = map (fun x -> T.modulo x m) v

  let below_epsilon v = Array.fold_left (fun acc x -> acc && (abs x < T.epsilon)) true v

  let fold_left  : ('a -> Scalar.t -> 'a) -> 'a -> t -> 'a = Array.fold_left

  let fold_right : (Scalar.t -> 'a -> 'a) -> t -> 'a -> 'a = Array.fold_right

  let for_all f v = Array.fold_left (fun acc x -> acc && (f x)) true v

  let min v1 v2 = map2 min v1 v2 

  let max v1 v2 = map2 max v1 v2 

  let normalize v =
    let n = Scalar.div Scalar.one (Scalar.sqrt (dot v v))
    in 
      scale v n

  let to_string v =
    let to_string = Scalar.to_string in
    "< "^(to_string v.(0))^" ; "^(to_string v.(1))^" ; "^(to_string v.(2))^" >"

end
