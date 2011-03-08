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


(** Quadratic split algorithm for Rtree :
    This  is  a straight implementation of the algorithm described 
    in the original paper.
*)

module Make (* : Rtreesplit.Make *) = 
  functor (Scalar : Scalar.T)  ->
    functor (Def : Def.T)      -> 
struct

  exception Error of string

  (* some list manipulation helpers -------------------------------------- *)

  let rec fold_left_l f acc l =
    match l with
	[]   -> acc
      | h::t -> fold_left_l f (f acc h t) t

  (* algorithm functions ------------------------------------------------- *)

  (** algorithm class *)
  class ['node] t =
  object(self)

    constraint 'node = Scalar.t Node.node 
      
    method pickseeds (l : 'node list) : 'node * 'node =
      let pickseed ((c1, c2 , a), c3) c4 =
	let na = (c3#key)#area_increase c4#key
	in
	  if na > a 
	  then ((c3,c4,a),c3)
	  else ((c1,c2,a),c3)
      in
      let e1, e2, ll = 
	match l with
	    s1::s2::ll -> s1, s2, ll
	  | _          -> raise (Error "pickseed: less than 2 elements")
      in
      let (ee2, _)    = 
	List.fold_left pickseed (fst (List.fold_left pickseed ((e1, e2, e1#key#area_increase e2#key), e1) ll), e2) ll 
      in
	(* then do the rest *)
      let (s1, s2, _) = 
	fold_left_l (fun acc h t -> fst (List.fold_left pickseed (acc,h) (h::t))) ee2 ll
      in 
	s1, s2

	  
    method private fillup  =
      let rec fillup l1 (l2, r2) le =
	match le with
	    []    -> l1, (l2, r2)
	  | e::le -> fillup l1 (e::l2, r2#expand e#key) le
      in
	fillup
	  
	  
    (* putnext:
       pick the least region increasing entry of le and put it in the corresponding
       list l1 or l2
       @l1, @l2 the output lists
       @r1, @r2 the covered regions for the output lists
       @le the list of remaining entries to be assigned
       Q: what if all the entries favor one list?
       A: just check the number of entries in either lists, and when (Def.minimum + 1)
       is reached for one list, fill up the other list with the rest, see fillup
    *)
    method private putnext =
      let rec putnext (l1, r1, i1) (l2, r2, i2) le =
	(* check if we reached threshold on either list *)
	match i1, i2 with
	    _,v when v > Def.minimum -> self#fillup (l2, r2) (l1, r1) le
	  | v,_ when v > Def.minimum -> self#fillup (l1, r1) (l2, r2) le
	  | _                        -> (
	      (* add e to the list l *)
	      let add l r e i =
		(e::l, r#expand  e#key, succ i)
	      in
		match le with 
		    [] -> (l1, r1), (l2, r2)
		  | _  -> (
		      (* compute area increases for an entry on both nodes:
			 returns the entry, which node it should go in and 
			 the diff of increase *)
		      let compute_both e r1 r2 = 
			let (-) = Scalar.sub in
			let re = e#key in
			let i1 = r1#area_increase re
			and i2 = r2#area_increase re
			in
			  if i1 <= i2 then (e, 1, i2 - i1) else (e, 2, i1 - i2)
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
			then putnext (add l1 r1 e i1) (l2,r2,i2) lr
			else putnext (l1,r1,i1) (add l2 r2 e i2) lr
		    )
	    )
      in
	putnext

    (** split: the algorithm main entry point *)	  
    method split (l : 'node list) =
      let s1, s2 = self#pickseeds l
      in
      let ll = List.find_all (fun x -> x <> s1 && x <> s2) l
      in
	self#putnext ([s1], s1#key, 1) ([s2], s2#key, 1) ll

  end

 end

