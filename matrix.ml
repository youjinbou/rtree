(* column based matrices *)
module Make (V : Vec.T) = 
struct 

  module T = V.Scalar

  let ( + ) = T.add
  let ( - ) = T.sub
  let ( * ) = T.mul
  let ( / ) = T.div
  let opp   = T.opp
  let abs   = T.abs


  type t = V.t array (* k² cells *)

  let size = V.size

  let get m i j =
    V.get m.(i) j

  let set m i j v = 
    V.set m.(i) j v

  let null () = 
    Array.init size (fun _ -> V.null ())

  let identity () =
    let m = null () 
    in
      for i = 0 to (pred size) do
	set m i i T.one
      done;
      m

  let row m n =
    let v = V.null () 
    in
      for i = 0 to (pred size) do
	V.set v i (get m i n)
      done;
      v

  let col m n =
    m.(n)

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
    for i = 0 to (pred size) do
      for j = 0 to (pred size) do
	set res i j ((get res i j) + (get m1 i j) * (get m2 j i))
      done
    done
(*
      res.(i).(0) <- m1.(i).(0) *. m2.(0).(0) +. m1.(i).(1) *. m2.(1).(0) +. 
	m1.(i).(2) *. m2.(2).(0) +. m1.(i).(3) *. m2.(3).(0);
      res.(i).(1) <- m1.(i).(0) *. m2.(0).(1) +. m1.(i).(1) *. m2.(1).(1) +. 
	m1.(i).(2) *. m2.(2).(1) +. m1.(i).(3) *. m2.(3).(1);
      res.(i).(2) <- m1.(i).(0) *. m2.(0).(2) +. m1.(i).(1) *. m2.(1).(2) +.
	m1.(i).(2) *. m2.(2).(2) +. m1.(i).(3) *. m2.(3).(2);
      res.(i).(3) <- m1.(i).(0) *. m2.(0).(3) +. m1.(i).(1) *. m2.(1).(3) +.
	m1.(i).(2) *. m2.(2).(3) +. m1.(i).(3) *. m2.(3).(3);
*)

  (** mult 
      compute the result of multiplying 2 matrices
      @m1, @m2 the matrices to multiply together
  *)
  let mult m1 m2 =
    let res = null ()
    in
      multref m1 m2 res; 
      res

  (** apply
      apply a matrix transformation to a given size dim vector
      @m the transformation matrix to apply
      @x,@y,@z,@w the vector components to be transformed
  *)
  let apply m v = 
    let r = V.null ()
    in
      for i = 0 to (pred size) do
	V.set r i (V.dot (row m i) v)
      done;
      r

  let transpose m =
    let r = null () 
    in
      for i = 0 to (pred size) do
	for j = 0 to (pred size) do
	  set r i j (get m j i)
	done
      done
	

end
