(* Linear split algorithm for RTREE *)
module Make (Coord : Vec.T) (N : Splitnode.T with type scalar_t = Coord.Scalar.t) (Def : Rtreedef.T) : (Rtreesplit.T with type key_t = N.key_t and type node_t = N.node_t) = 
struct

  type key_t  = N.key_t
  type node_t = N.node_t

  let getkey        = N.getkey
  let area_increase = N.area_increase
  let expand        = N.expand

  exception Error of string

  let pickseeds l = 
    match l with 
	a::b::t -> a, b, t
      | _       -> raise (Error "pickseed : less than 2 elements")

  let rec fillup l1 (l2, r2) le =
    match le with
	[]    -> l1, (l2, r2)
      | e::le -> fillup l1 (e::l2, expand r2 (getkey e)) le

  let rec putnext (e1, l1, r1, i1) (e2, l2, r2, i2) le =
    (* check if we reached threshold on either list *)
    match i1, i2 with
	_,v when v > Def.minimum -> fillup (l2, r2) (l1, r1) le
      | v,_ when v > Def.minimum -> fillup (l1, r1) (l2, r2) le
      | _                        -> (
	  (* add e to the list l *)
	  let add e1 l r e i =
	    (e1, e::l, expand r (getkey e), succ i)
	  in
	    match le with
		[]    -> (l1, r1), (l2, r2)
	      | e::lr -> (
		  let re = getkey e in
		    if area_increase r1 re <= area_increase r2 re 
		    then putnext (add e1 l1 r1 e i1) (e2, l2, r2, i2) lr
		    else putnext (e1, l1, r1, i1) (add e2 l2 r2 e i2) lr
		)
	)

  let split l =
    let s1, s2, ll = pickseeds l
    in
      putnext (s1, [s1], getkey s1, 1) (s2, [s2], getkey s2, 1) ll

end
