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

(** Quadratic split algorithm for RTREE *)
module Make  
  (Coord : Vec.T) 
  (N     : Splitnode.T with type scalar_t = Coord.Scalar.t)
  (Def   : Rtreedef.T) : 
  (Rtreesplit.T with type key_t = N.key_t and type node_t = N.node_t) = 
struct

  type key_t  = N.key_t
  type node_t = N.node_t
  
  let getkey        = N.getkey
  let area_increase = N.area_increase
  let expand        = N.expand

  exception Error of string

  (* some list manipulation helpers -------------------------------------- *)

  let rec fold_left_l f acc l =
    match l with
	[]   -> acc
      | h::t -> fold_left_l f (f acc h t) t

  (* algorithm functions ------------------------------------------------- *)
 
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
	| _          -> raise (Error "pickseed: less than 2 elements")
    in
    let (ee2, _)    = 
      List.fold_left pickseed (fst (List.fold_left pickseed ((e1, e2, area_increase (getkey e1) (getkey e2)), e1) ll), e2) ll in
      (* then do the rest *)
    let (s1, s2, _) = 
      fold_left_l (fun acc h t -> fst (List.fold_left pickseed (acc,h) (h::t))) ee2 ll
    in 
      s1, s2

  let rec fillup l1 (l2, r2) le =
    match le with
	[]    -> l1, (l2, r2)
      | e::le -> fillup l1 (e::l2, expand r2 (getkey e)) le

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
  let rec putnext (l1, r1, i1) (l2, r2, i2) le =
    (* check if we reached threshold on either list *)
    match i1, i2 with
	_,v when v > Def.minimum -> fillup (l2, r2) (l1, r1) le
      | v,_ when v > Def.minimum -> fillup (l1, r1) (l2, r2) le
      | _                        -> (
	  (* add e to the list l *)
	  let add l r e i =
	    (e::l, expand r (getkey e), succ i)
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
	  
  let split l =
    let s1, s2 = pickseeds l
    in
    let ll = List.find_all (fun x -> x <> s1 && x <> s2) l
    in
      putnext ([s1], getkey s1, 1) ([s2], getkey s2, 1) ll

end
