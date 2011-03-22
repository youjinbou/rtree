open Rtree
(* module implementing Region.T *)
module Make =
  functor (Coord : M.Vec.T) ->
struct

  class c (p1 : Coord.t) (p2 : Coord.t) = 
    let b_ = Coord.map2 min p1 p2
    and t_ = Coord.map2 max p1 p2
    in
  object(self)

    method bottom = b_
    method top    = t_

    method private dims   = Coord.sub t_ b_

(*
    method center = 
      let half = Coord.Scalar.div Coord.Scalar.one (Coord.Scalar.add Coord.Scalar.one Coord.Scalar.one)
      in
      let d = (Coord.scale self#dims half)
      in 
	Coord.add b_ d
*)	
  
    (* volume covered by self *)
    val area_ = 
      let x = Coord.sub t_ b_ in
	Coord.dot x x

    (* whether the rbox rb overlaps with self *)
    method overlaps (rb : c) = 
      let segment_overlap s1 s2 e1 e2 =
	(s1 < e2) && (s2 < e1)
      in
      let b1 = b_ 
      and b2 = rb#bottom
      and t1 = t_ 
      and t2 = rb#top
      in
      let rec check_seg i =
	if i = -1 
	then true 
	else
	  let s1 = Coord.get b1 i
	  and s2 = Coord.get b2 i
	  and e1 = Coord.get t1 i
	  and e2 = Coord.get t2 i
	  in
	    (segment_overlap s1 s2 e1 e2) && check_seg (pred i)
      in
	check_seg (pred Coord.size)

    (* whether the rbox rb fits in self *)
    method includes (rb : c) =
      let segment_include s1 s2 e1 e2 =
	(s1 <= s2) && (e2 <= e1)
      in
      let b1 = b_ 
      and b2 = rb#bottom
      and t1 = t_ 
      and t2 = rb#top
      in
      let rec check_seg i =
	if i = -1 
	then true 
	else
	  let s1 = Coord.get b1 i
	  and s2 = Coord.get b2 i
	  and e1 = Coord.get t1 i
	  and e2 = Coord.get t2 i
	  in
	    (segment_include s1 s2 e1 e2) && check_seg (pred i)
      in
	check_seg (pred Coord.size)
	  
    method area = area_

    (*
    (* whether self surrounds the point p *)
    method surrounds p =
      let rec check f p1 p2 i =
	let c k = (f (Coord.get p1 k) (Coord.get p2 k)) 
	in
	  if i = 0
	  then c 0
	  else
	    (c i) && (check f p1 p2 (pred i))
      in
	(check (<) p t_ (pred Coord.size)) && (check (>) p b_ (pred Coord.size))
      *)

    (* create new rbox covering self and the rbox rb *)
    method expand (rb : c) =
      let bottom = Coord.map2 min rb#bottom b_
      and top    = Coord.map2 max rb#top t_
      in
	new c bottom top
	  
    method area_with (rb : c) = 
      (self#expand rb)#area 

    method to_string = string_of_int (Oo.id self)
      
  end

  type t = c
  type key_t = Coord.t
  type scalar_t = Coord.Scalar.t

(*
  let expand (v : t) (r : t) : t = v#expand r
  let area (v : t) : scalar_t = v#area
  let area_with (v : t) (r : t) : scalar_t = v#area_with r
  let overlaps (v : t) (r : t) : bool = v#overlaps r
  let includes (v : t) (r : t) : bool = v#includes r
  let to_string (v : t) : string = v#to_string
*)

  let expand v    = v#expand
  let area v      = v#area
  let area_with v = v#area_with
  let overlaps v  = v#overlaps
  let includes v  = v#includes
  let to_string v = v#to_string


end
