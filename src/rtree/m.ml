(*
  a rtree library

  Copyright (C) 2010-2011 Didier Cassirame

  This  program  is free software:  you can redistribute it and/or 
  modify  it  under  the  terms  of  the GNU Lesser General Public 
  License  as  published  by  the Free Software Foundation, either 
  version 3 of the License, or (at your option) any later version.

  This  program is distributed in the hope that it will be useful,
  but  WITHOUT  ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public 
  License along with this program. If not, see 
  <http://www.gnu.org/licenses/>.

*)

open Sig.M

(** rbox is an implementation of the region module *)
module Rbox (Coord: VEC) =
struct 

  type key_t    = Coord.t
  type scalar_t = Coord.Scalar.t


  type t = {
    bottom : Coord.t;
    top    : Coord.t;
    area   : Coord.Scalar.t Lazy.t;
  }

  (* some coord helpers ------------------------------------------------- *)
  let list_of_coord c =
    let rec blist i l = 
      if i < 0 then l else blist (pred i) ((Coord.get c i)::l)
    in
      blist Coord.size []

(*
  let string_of_coord c =
    let to_s = Coord.Scalar.to_string in
      match Coord.size with
	  1 -> "<"^(to_s (Coord.get c 1))^">"
	| n -> let l = (list_of_coord c)
	  in
	    "<"^(List.fold_left (fun acc x -> acc^";"^(to_s x)) (to_s (List.hd l)) (List.tl l))^">"
*)

  (* -------------------------------------------------------------------- *)

(*
  let to_string k =
    "["^(string_of_coord (k.top))^"-"^(string_of_coord (k.bottom))^"]"
*)

  (** returns the volume size between points b_ and t_ *)
  let area_ b_ t_ = 
    let x = Coord.sub t_ b_ in
      Coord.dot x x

  let make p1 p2 =
    let b_ = Coord.map2 min p1 p2
    and t_ = Coord.map2 max p1 p2
    in { 
	bottom = b_;
	top    = t_;
	area   = lazy (area_ b_ t_);
      }
	 
  let dims v = Coord.sub v.top v.bottom

  let center v = 
    let two = Coord.Scalar.add Coord.Scalar.one Coord.Scalar.one in
    let half = Coord.Scalar.div Coord.Scalar.one two in
    let d = (Coord.scale (dims v) half) in
    Coord.add v.bottom d

  let area v = Lazy.force v.area

  (** return true if the rbox rb overlaps with v *)
  let overlaps v rb = 
    let segment_overlap s1 s2 e1 e2 =
      (s1 < e2) && (s2 < e1)
    in
    let b1 = v.bottom 
    and b2 = rb.bottom
    and t1 = v.top 
    and t2 = rb.top
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

  (** returns true if the rbox rb fits in v *)
  let contains v rb =
    let segment_include s1 s2 e1 e2 =
      (s1 <= s2) && (e2 <= e1)
    in
    let b1 = v.bottom 
    and b2 = rb.bottom
    and t1 = v.top 
    and t2 = rb.top
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
	  
  (** returns true if v surrounds the point p *)
  let surrounds v p =
    let rec check f p1 p2 i =
      let c k = (f (Coord.get p1 k) (Coord.get p2 k)) 
      in
	if i = 0
	then c 0
	else
	  (c i) && (check f p1 p2 (pred i))
    in
      (check (<) p v.top (pred Coord.size)) && (check (>) p v.bottom (pred Coord.size))


  (** create new rbox covering self and the rbox rb *)
  let expand v rb =
    let bottom = Coord.map2 min rb.bottom v.bottom
    and top    = Coord.map2 max rb.top v.top
    in
      make bottom top
	
  let area_with v rb = 
    area (expand v rb)
      
end


(** Quadratic split algorithm for Rtree *)
module Qsplit
  (Coord : VEC) 
  (N     : NODE with type scalar_t = Coord.Scalar.t)
  (Def   : DEF) : 
  (SPLIT with type key_t = N.key_t and type node_t = N.node_t) = 
struct

  type key_t  = N.key_t
  type node_t = N.node_t
  
  let getkey        = N.getkey
  let area_increase = N.area_increase
  let expand        = N.expand
(*  let to_string     = N.to_string *)

  exception Error of string

  (* algorithm functions ------------------------------------------------- *)
 
   (* PS : Select 2 entries to be the first elements of the groups *)
   let pickseeds (l : node_t list) : node_t * node_t * node_t list =
     (* pick the entry in @l maximizing the area with @c1
	@e1 fixed entry against which we try every other entries of @l
	@e2 the current best pick in @l
	@a  the area covered by @e1,@e2
	@l  the list of entries we match @e1 with
	@l' the remaining entries of @l after we found @e2
     *)
     let rec pickbest e1 e2 a l l' =
       match l with
	   []    -> (
	     e2,a,l'
	   )
	 | x::xs -> (
	   let xa = area_increase (getkey e1) (getkey x) in
	   if xa > a
	   then pickbest e1 x xa xs (e2::l')
	   else pickbest e1 e2 a xs (x::l')
	 )
     in
     let rec pickseed c1 c2 a l l' =
       match l with
	 | []    -> (
	   c1, c2, l'
	 )
	 | x::xs -> (
	   let xa = area_increase (getkey c1) (getkey x) in
	   let x2, xa, xl = pickbest x c1 xa (c2::l') [] in
	   if xa > a
	   then pickseed x x2 xa xs xl
	   else pickseed c1 c2 a xs (x::l')
	 )
     in	     
     match l with
       | c1::c2::cs -> (
	 let a = area_increase (getkey c1) (getkey c2) in
	 let c2, a, cs = pickbest c1 c2 a cs [] in
	 pickseed c1 c2 a cs []
       )
       | _          -> raise (Error "pickseeds: less than 2 elements")
	 

  (* picknext:
     pick the least region increasing entry of le and put it in the corresponding
     list l1 or l2
     @l1, @l2 the output lists
     @r1, @r2 the covered regions for the output lists
     @le the list of remaining entries to be assigned
     Q: what if all the entries favor one list?
     A: just check the number of entries in either lists, and when (Def.minimum + 1)
     is reached for one list, fill up the other list with the rest, see fillup
  *)
  let rec picknext (l1, r1, i1) (l2, r2, i2) le =
    let rec fillup l1 (l2,r2) le =
      match le with
	  []    -> l1, (l2, r2)
	| e::le -> fillup l1 (e::l2, expand r2 (getkey e)) le
    in
    (* check if we reached threshold on either list *)
    match i1, i2 with
	_,v when v > Def.minimum -> fillup (l2, r2) (l1, r1) le
      | v,_ when v > Def.minimum -> fillup (l1, r1) (l2, r2) le
      | _                        -> (
	  (* add e to the list l *)
	  let add l r e i =
	    e::l, expand r (getkey e), succ i
	  in
	  match le with 
	      [] -> (l1, r1), (l2, r2)
	    | _  -> (
	      (* compute area increases for an entry on both nodes:
		 returns the entry, which node it should go in and 
		 the diff of increase *)
	      let compute_both e r1 r2 = 
		let (-) = Coord.Scalar.sub in
		let re = (getkey e) in
		let i1 = area_increase r1 re
		and i2 = area_increase r2 re
		in
		if i1 <= i2 then (e, 1, i2 - i1) else (e, 2, i1 - i2)
	      in
	      (* compare acc with challenger, keep winner as new acc
		 and put loser in remaining list lr *)
	      let challenge ((e, i, d), lr) c =
		let (_, ci, cd) = compute_both c r1 r2 in
		if cd > d then ((c, ci, cd), e::lr) else ((e, i, d), c::lr)
	      in
	      (* compute the best entry for next inclusion, and the 
		 remaining list of entries *)
	      let ((e, i, _), lr) = 
		List.fold_left 
		  challenge 
		  (compute_both (List.hd le) r1 r2, [])
		  (List.tl le)
	      in 
	      if i = 1 
	      then picknext (add l1 r1 e i1) (l2,r2,i2) lr
	      else picknext (l1,r1,i1) (add l2 r2 e i2) lr
	    )
      )

  (** algorithm entry point *)	  
  let split l =
    let s1, s2, ll = pickseeds l in
    picknext ([s1], getkey s1, 1) ([s2], getkey s2, 1) ll

end


(** Linear split algorithm for Rtree *)
module Lsplit
  (Coord : VEC)
  (N     : NODE with type scalar_t = Coord.Scalar.t)
  (Def   : DEF) :
  (SPLIT with type key_t = N.key_t and type node_t = N.node_t) = 
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

  let rec picknext (e1, l1, r1, i1) (e2, l2, r2, i2) le =
    let rec fillup l1 (l2, r2) le =
      match le with
	  []    -> l1, (l2, r2)
	| e::le -> fillup l1 (e::l2, expand r2 (getkey e)) le
    in
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
		    then picknext (add e1 l1 r1 e i1) (e2, l2, r2, i2) lr
		    else picknext (e1, l1, r1, i1) (add e2 l2 r2 e i2) lr
		)
	)

  (** algorithm entry point *)
  let split l =
    let s1, s2, ll = pickseeds l in
    let (l1,r1),(l2,r2) = picknext (s1, [s1], getkey s1, 1) (s2, [s2], getkey s2, 1) ll in
    assert (List.length l1 < Def.maximum);
    assert (List.length l2 < Def.maximum);
    (l1,r1),(l2,r2)

end



(** the functor used to build a rtree handling module *)

module Make
  (Split   : SPLIT_FUNCTOR)
  (Region  : REGION_FUNCTOR)
  (Coord   : VEC)
  (Def     : DEF) :
  (RTREE with type value_t = Def.t
	 and  type key_t   = Region(Coord).t) =
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

  (* Region Boxes -------------------------------------------------------- *)

  module R = Region(Coord)

  let area_increase c1 c2 =
    let (-) = Coord.Scalar.sub in
      ((R.area_with c1 c2) 
       - (R.area c1)) - (R.area c2)

  (* --------------------------------------------------------------------- *)
	
  (* plain sum types to represent rtrees : same as btrees *)

  type key_t = R.t

  type value_t = Def.t
      
  type cell_t = (key_t * value_t) 
      
  type node_t = 
      Leaf of key_t * cell_t list
    | Node of key_t * node_t list

  type t = { mutable root : node_t option }

  let getkey node = match node with
      Leaf (k, _) -> k
    | Node (k, _) -> k

  (* ------------------------------------------------------------------------------- *)

  (** returns the list of values in the tree which are filtered by f *)
  let filter tree (f : key_t -> bool) =
    let rec filter node f =
      match node with
	  Leaf (k, cl) -> fold_left (fun l (k,b) -> if f k then (k,b)::l else l) [] cl
	| Node (k, nl) -> 
	    if f k
	    then fold_left (fun l nn -> append (filter nn f) l) [] nl
	    else []
    in
      match tree.root with
	  None      -> []
	| Some tree -> filter tree f

  (* ------------------------------------------------------------------------------- *)

  module Cell : NODE with type scalar_t = Coord.Scalar.t and type node_t = cell_t and type key_t = key_t =
  struct
    type node_t   = cell_t
    type k_t      = key_t (* type alias to avoid a cycle *)
    type key_t    = k_t
    type scalar_t = Coord.Scalar.t
    let getkey        = fst
    let area_increase = area_increase
    let expand r1 r2  = R.expand r1 r2
  end

  module Node : NODE with type scalar_t = Coord.Scalar.t and type node_t = node_t and type key_t = key_t =
  struct 
    type t        = node_t
    type node_t   = t
    type k_t      = key_t (* type alias to avoid a cycle *)
    type key_t    = k_t
    type scalar_t = Coord.Scalar.t
    let getkey        = getkey
    let area_increase = area_increase
    let expand r1 r2  = R.expand r1 r2
  end

  module SplitLeaf = Split(Coord)(Cell)(Def)
  module SplitNode = Split(Coord)(Node)(Def)

  (* ------------------------------------------------------------------------------- *)

  let choose_leaf k nl =
    let least_area n1 n2 = 
      let a = R.area (getkey n1)
      and b = R.area (getkey n2)
      in if a < b then -1 else if a > b then 1 else 0
    in
      sort least_area nl

  (* ------------------------------------------------------------------------------- *)

  (** create an empty tree *)
  let empty = {root = None}

  (** create a tree containing the couple (key,value) *) 
  let make key value = {root = Some (Leaf (key,[(key,value)]))}


  (** find the first element in the tree which contains the key *)
  let find tree key = 
    let check_key key rank node =
      match node with
	| Leaf (k, _)
	| Node (k, _) -> R.contains k key 
    in
    let rec find nl key rank = 
      (* node regions may overlap, so we have to check each of them *)
      let lm = List.find_all (check_key key rank) nl in
      List.concat (List.map (function 
	| Leaf (_, cl) -> (
	  let f (k,v) = R.contains k key in
	  let r = List.find_all f cl in
	  List.map snd r
	)
	| Node (_, nl) -> (
	  find nl key (succ rank)
	)
      ) lm)
    in
    match tree.root with
	None       -> []
      | Some root  -> find [root] key 0
	

  (** insert the couple (key,value) in the tree *)
  let insert tree key value =
    let rec insert n key value =
      match n with
	  Leaf (k, cl) -> (
	    let c = (key, value) 
	    in
	      if length cl >= Def.maximum
	      then
		let (l1, r1), (l2, r2) = SplitLeaf.split (c::cl)
		in
		  Leaf (r1, l1), Some (Leaf (r2, l2))
	      else
		Leaf (R.expand k key,c::cl), None
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
		      Node (R.expand k key,cl), None
		)
	  )
    in
      match tree.root with 
	  None      -> tree.root <- Some (Leaf (key,[(key,value)]))
	| Some root ->
	    match insert root key value with
		n1, None    -> tree.root <- Some n1
	      | n1, Some n2 -> tree.root <- Some (Node ((R.expand (getkey n1)) (getkey n2), [n2;n1]))
	    



  (* ------------------------------------------------------------------------------- *)

  (** iterate over all the values in the tree *)
  let iter tree fnode fleaf =
    let rec iter_node n =
      match n with
	  Leaf (k, cl) -> fnode k; List.iter (fun (k, v) -> fleaf k v) cl
	| Node (k, nl) -> fnode k; iter_list nl

    and iter_list l =
      match l with 
	  []   -> ()
	| n::t -> (iter_node n; iter_list t)
    in
      match tree.root with
	  None      -> () 
	| Some root -> iter_node root

  (* ------------------------------------------------------------------------------- *)

  (** dump the tree to dotty format *)
  let dump string_of_key tree filename directory =
    let file = open_out (directory^"/"^filename^".dot") in
    let output_str s = output_string file s
    in
    let dump_link parent child =
      output_str (parent^":i"^child^" -> n_"^child^":n;\n")
    in
    let node_label k = 
      "n_"^(string_of_key k)
    in	
    let dump_cell (k,v) = 
      let sk = string_of_key k
      in
	output_str ("<i"^sk^"> "^sk^" ")
    and dump_child_key n =
      let sk = string_of_key (getkey n)
      in
	output_str ("<i"^sk^"> "^sk^" ")
    in
    let rec dump_ parent n =
      match n with
	  Leaf (k,l) -> dump_link parent (string_of_key k); dump_leaf k l
	| Node (k,l) -> dump_link parent (string_of_key k); dump_node k l
	    
    and dump_node k l = 
      (* dump node structure *)
      output_str (node_label k);
      output_str " [ label = \" ";
      dump_child_key (List.hd l);
      List.iter (fun x -> output_str " | "; dump_child_key x ) (List.tl l);
      output_str " \" ];\n";
      (* dump children *)
      List.iter (fun n -> dump_ (node_label k) n) l

    and dump_leaf k l =
      (* dump leaf structnure *)
      output_str (node_label k);
      output_str " [ label = \" ";
      dump_cell (List.hd l);
      List.iter (fun x -> output_str " | "; dump_cell x ) (List.tl l);
      output_str " \" ];\n"
	
    in
      output_str ("digraph "^filename^" {\n");
      output_str ("name = "^filename^";\n");
      output_str "node [shape=record];\n";
      (match tree.root with 
	  None       -> ()
	| Some root  ->
	    dump_ "root" root);
      output_str "}\n";
      close_out file

end

(** ready made rtrees *)
module Linear = Make(Lsplit)(Rbox)
module Quadratic = Make(Qsplit)(Rbox)

