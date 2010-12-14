(* a very simple 3D vector module *)

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
end

module Make( T: SCALAR) =
struct

  module type SCALAR = SCALAR

  module Scalar = T
    
  type t = T.t array

  let ( + ) = T.add
  let ( - ) = T.sub
  let ( * ) = T.mul
  let ( / ) = T.div
  let opp   = T.opp
  let abs   = T.abs

  let size = 3

  let get v i = v.(i)

  let null () = Array.make size T.zero

  let unit i  = let u = null () in u.(pred i) <- T.one ; u

  let one  () = Array.make size T.one

  let add v1 v2 = 
    Array.init size (fun i -> v1.(i) + v2.(i))
      
  let sub v1 v2 = 
    Array.init size (fun i -> v1.(i) - v2.(i))

  let scale v1 s = 
    Array.init 3 (fun i -> v1.(i) * s)

  let dot v1 v2 = v1.(0) * v2.(0) + v1.(1) * v2.(1) + v1.(2) * v2.(2)

  let copy v1 v2 = Array.blit v1 0 v2 0 size

  let to_tuple v = (v.(0),v.(1),v.(2))

  let of_tuple (x,y,z) = [| x; y; z |]

  let map = Array.map

  let map2 f v1 v2 = 
    [| f v1.(0) v2.(0) ; f v1.(1) v2.(1) ; f v1.(2) v2.(2) |]

  let random v = map (fun x -> if x = T.zero then T.zero else T.rand x) v 

  let modulo v m = map (fun x -> T.modulo x m) v

  let below_epsilon v = Array.fold_left (fun acc x -> acc && (abs x < T.epsilon)) true v

  let fold_left  : ('a -> Scalar.t -> 'a) -> 'a -> t -> 'a = Array.fold_left

  let fold_right : (Scalar.t -> 'a -> 'a) -> t -> 'a -> 'a = Array.fold_right

  let for_all f v = Array.fold_left (fun acc x -> acc && (f x)) true v

  let min v1 v2 = map2 min v1 v2 

  let max v1 v2 = map2 max v1 v2 

end
