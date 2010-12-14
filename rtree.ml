open Debug

(*345678911234567892123456789312345678941234567895123456789612345678971234567898*)
module type VEC =
sig

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

end

(* module type for rtree data *)
module type RTREEDEF =
sig

  val minimum : int
  val maximum : int

  type t
    
end

module Make = functor (Coord: VEC) -> functor (Def : RTREEDEF) ->
struct

  open List

  exception Error of string

  (* some list manipulation helpers -------------------------------------- *)

  let rec extract f acc l lr =
      match l with 
	  []      -> (acc, lr)
	| hd::tl  -> 
	    let nacc, nr = f acc hd 
	    in 
	      extract f nacc tl (nr::lr)

  let rec fold_left_l f acc l =
    match l with
	[]   -> acc
      | h::t -> fold_left_l f (f acc h t) t
 
  (* Region Boxen -------------------------------------------------------- *)

  module Rbox = Rbox.Make(Coord)

  let covering l =
    let rec cover l r =
      match l with
	  []   -> r
	| a::b -> cover b (r#expand a#key)
    in
      match l with
	  []   -> invalid_arg "covering : empty list"
	| a::b -> cover l a#key

  let area_increase c1 c2 =
    let (-) = Coord.Scalar.sub in
      ((c1#area_with c2) 
       - (c1#area ())) - (c2#area ())

  (* --------------------------------------------------------------------- *)
    
  (* plain sum types to represent rtrees : same as btrees *)

  type key_t = Rbox.t

  type value_t = Def.t
      
  type cell_t = (key_t * value_t) 
      
  type node_t = 
      Leaf of key_t * cell_t list
    | Node of key_t * node_t list

  type t = { mutable root : node_t }

  let getkey node = match node with
      Leaf (k, _) -> k
    | Node (k, _) -> k

  (* ------------------------------------------------------------------------------- *)

  let rec find node key =
    match node with
	Leaf (k, cl) -> fold_left (fun l (k,b) -> if k#overlaps key then (k,b)::l else l) [] cl
      | Node (k, nl) -> if k#overlaps key 
	then fold_left (fun l nn -> append (find nn key) l) [] nl
	else []


  (* ------------------------------------------------------------------------------- *)

  module type SPLITNODE =
  sig
    type t
    val getkey : t -> key_t
  end

  module Split (N : SPLITNODE) = 
  struct
    
    let getkey = N.getkey

    let pickseeds l =
      let pickseed ((c1, c2, a), c3) c4 =
	let na = area_increase (getkey c3) (getkey c4)
	in
	  if na > a 
	  then ((c3,c4,a),c3)
	  else ((c1,c2,a),c3)
      in
      let e1, e2, ll = 
	match l with
	    s1::s2::ll -> s1, s2, ll
	  | _          -> raise (Error "pickseed: less than 3 elements")
      in
      let (ee2, _)    = 
	List.fold_left pickseed (fst (List.fold_left pickseed ((e1, e2, area_increase (getkey e1) (getkey e2)), e1) ll), e2) ll in
	(* then do the rest *)
      let (s1, s2, _) = 
	fold_left_l (fun acc h t -> fst (List.fold_left pickseed (acc,h) (h::t))) ee2 ll
      in 
	s1, s2

    let rec putnext (l1, r1) (l2, r2) le =
      (* add e to the list l *)
      let add l r e =
	(e::l, r#expand (getkey e))
      in
	match le with 
	    [] -> (l1, r1), (l2, r2)
	  | _  -> (
	      (* compute area increases for an entry on both nodes:
		 returns the entry, which node it should go in and 
		 the diff of increase *)
	      let compute_both e r1 r2 = 
		let (-) = Coord.Scalar.sub in
		let i1 = area_increase r1 (getkey e)
		and i2 = area_increase r2 (getkey e)
		in
		  if i1 < i2 then (e, 1, i2 - i1) else (e, 2, i1 - i2)
	      in
		(* compare acc with challenger, keep winner as new acc
		   and put loser in remaining list lr *)
	      let challenge ((e, i, d), lr) c =
		let (_, ci, cd) = compute_both c r1 r2 in
		  if cd > d then ((c, ci, cd), e::lr) else ((e, i, d), c::lr)
	      in
		(* compute the best entry for next inclusion, and the remaining 
		   list of entries *)
	      let ((e, i, _), lr) = 
		List.fold_left 
		  challenge 
		  (compute_both (List.hd le) r1 r2, [])
		  (List.tl le)
	      in 
		if i = 1 
		then putnext (add l1 r1 e) (l2,r2) lr
		else putnext (l1,r1) (add l2 r2 e) lr
	    )
	      
    let split l =
      let s1, s2 = pickseeds l
      in
	putnext ([s1], getkey s1) ([s2], getkey s2) l

  end
    
  module Cell =
  struct
    type t = cell_t
    let getkey = fst
  end

  module Node =
  struct 
    type t = node_t
    let getkey = getkey
  end

  module SplitLeaf = Split(Cell)
  module SplitNode = Split(Node)

  (* ------------------------------------------------------------------------------- *)

  let choose_leaf k nl =
    let least_area n1 n2 = 
      let a = (getkey n1)#area()
      and b = (getkey n2)#area() 
      in if a < b then -1 else if a > b then 1 else 0
    in
      sort least_area nl

  (* ------------------------------------------------------------------------------- *)

  let insert t key value =
  let rec insert n key value =
    match n with
	Leaf (k, cl) -> (
	  let c =  (key, value) 
	  in
	    if length cl > Def.maximum
	    then
	      let (l1, r1), (l2, r2) = SplitLeaf.split (c::cl)
	      in
		Leaf (r1, l1), Some (Leaf (r2, l2))
	    else
	      Leaf (k#expand key,c::cl), None
	)
      | Node (k, nl) -> (
	  match choose_leaf key nl with
	      []   -> (
		raise (Error "insert : choose_leaf returned []")
	      )
	    | h::l -> ( 
		let cl = match insert h key value with
		    (n1, None)    -> n1::l
		  | (n1, Some n2) -> n1::n2::l
		in
		  if length cl > Def.maximum 
		  then
		    let (l1, r1), (l2, r2) = SplitNode.split cl
		    in
		      Node (r1, l1), Some (Node (r2, l2))
		  else
		    Node (k#expand key,cl), None
	      )
	)
  in
    match insert t.root key value with
	n1, None    -> t.root <- n1
      | n1, Some n2 -> t.root <- Node ((getkey n1)#expand (getkey n2), [n2;n1])
	  

  let make k v = {root = Leaf (k,[(k,v)])}

  (* ------------------------------------------------------------------------------- *)

  let iter t f =
    let rec iter_node n f =
      match n with
	  Leaf (k, cl) -> List.iter (fun (k, v) -> f v) cl
	| Node (k, nl) -> iter_list nl f

    and iter_list l f =
      match l with 
	  []   -> ()
	| n::t -> (iter_node n f; iter_list t f)
	    
    in
      iter_node t.root f

end
