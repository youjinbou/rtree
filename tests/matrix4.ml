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
open Vec

module Make (V : VEC) = 
struct 

  module T = V.Scalar

  let  ( + )  = T.add
  let  ( - )  = T.sub
  let  ( ~- ) = T.opp
  let  ( * )  = T.mul
  let  ( / )  = T.div

  type t = T.t array array (* k² cells *)

  let size = V.size

  let get m i j =
    m.(i).(j)

  let set m i j v = 
    m.(i).(j) <- v


  let make v =
    Array.init size (fun _ -> Array.make size v)

  let null () = 
    make T.zero

  let identity () =
    let m = null () 
    in
      for i = 0 to (pred size) do
	m.(i).(i) <- T.one
      done;
      m

  let row m n =
    V.of_tuple (m.(0).(n), m.(1).(n), m.(2).(n), m.(3).(n))

  let col m n =
    V.of_tuple (m.(n).(0), m.(n).(1), m.(n).(2), m.(n).(3))

  (* matrix product
    | A | x | B | =

                         | b0  b1  b2  b3  |
                         | b4  b5  b6  b7  |
                         | b8  b9  b10 b11 |
                         | b12 b13 b14 b15 |

    | a0  a1  a2  a3  |  [ a0 * b0 + a1 * b4 + a2 * b8 + a3 * b12 ]
    | a4  a5  a6  a7  |
    | a8  a9  a10 a11 |
    | a12 a13 a14 a15 |
  *)

  (** multref
      compute the product of 2 matrices and store it in a 3rd matrix
      @m1, @m2 the matrices to multiply together
      @res the resulting matrix
  *)
  let multref m1 m2 res =
    for i = 0 to (pred size)
    do
      res.(i).(0) <- m1.(i).(0) * m2.(0).(0) + m1.(i).(1) * m2.(1).(0) + 
	m1.(i).(2) * m2.(2).(0) + m1.(i).(3) * m2.(3).(0);
      res.(i).(1) <- m1.(i).(0) * m2.(0).(1) + m1.(i).(1) * m2.(1).(1) + 
	m1.(i).(2) * m2.(2).(1) + m1.(i).(3) * m2.(3).(1);
      res.(i).(2) <- m1.(i).(0) * m2.(0).(2) + m1.(i).(1) * m2.(1).(2) +
	m1.(i).(2) * m2.(2).(2) + m1.(i).(3) * m2.(3).(2);
      res.(i).(3) <- m1.(i).(0) * m2.(0).(3) + m1.(i).(1) * m2.(1).(3) +
	m1.(i).(2) * m2.(2).(3) + m1.(i).(3) * m2.(3).(3);
    done 

  (** mult 
      compute the result of multiplying 2 matrices
      @m1, @m2 the matrices to multiply together
  *)
  let mult m1 m2 =
    let res = null ()
    in
      multref m1 m2 res; res

  (** apply
      apply a matrix transformation to a given 4 dim vector
      @m the transformation matrix to apply
      @x,@y,@z,@w the vector components to be transformed
  *)
  let apply m v = 
    let (x,y,z,w) = V.to_tuple v 
    in 
    let x = m.(0).(0) * x + m.(1).(0) * y + m.(2).(0) * y + m.(3).(0) * w 
    and y = m.(0).(1) * x + m.(1).(1) * y + m.(2).(1) * y + m.(3).(1) * w 
    and z = m.(0).(2) * x + m.(1).(2) * y + m.(2).(2) * y + m.(3).(2) * w 
    and w = m.(0).(3) * x + m.(1).(3) * y + m.(2).(3) * y + m.(3).(3) * w 
    in 
      V.of_tuple (x,y,z,w)


  let translation x y z =
    let m = make T.one
    in
    set m 0 3 (~- x);
    set m 1 3 (~- y);
    set m 2 3 (~- z)

  (* vector translation *)
  let translationv vt = 
    let x = V.get vt 0
    and y = V.get vt 1
    and z = V.get vt 2 
    in
    translation x y z

  (* rotation on 1 axis *)
  let rotation n theta =
    let m = null ()
    in
    set m (pred n) (pred n) T.one;
    set m 3 3 T.one;
    let n1 = succ (n mod 3) in
    let n2 = n1 mod 3 in
    let n1 = pred n1 in
    let c = T.cos theta 
    and s = T.sin theta in
    set m n1 n1 c;
    set m n1 n2 s;
    set m n2 n2 c;
    set m n2 n1 (~- s)

  (* rotation on 3 axes *)
  let rotationv vr = 
    let m1 = rotate 1 (V.get vr 0)
    and m2 = rotate 2 (V.get vr 1)
    and m3 = rotate 3 (V.get vr 2)
    in 
    mult m3 (mult m2 m1)

  let scale x y z =
    let m = null () in
    set m 0 0 x;
    set m 1 1 y;
    set m 2 2 z;
    set m 3 3 T.one

  let prepare vt vs vr =
    let s = scale vs
    and t = translation vt  
    and r = rotationv vr 
    in
    mult t (mult r s)

  let invprepare vt vs vr = 
    let flip v = 
      let v' = V.null () in
      let get = V.get v 
      and set = V.set v' in
      let x,y,z = get 0, get 1, get 2
      in set z, set y, set x; v'
    in
    let s = scale vs
    and t = translation vt  
    and r = rotationv (flip vr)
    in
    mult s (mult r t)

end
