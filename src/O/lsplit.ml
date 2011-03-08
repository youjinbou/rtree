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

(** Linear partition algorithm for RTREE *)

(** this module doesn't actually even require a formal definition 
    of  the  scalar  type  used  within, thanks to the comparison 
    operator polymorphy.
*)
module Make =
  functor (Scalar : Scalar.T)  ->
    functor (Def : Def.T)      -> 
struct

  (** exception raised by the module *)
  exception Error of string

  (* algorithm functions ------------------------------------------------- *)
    
  (** algorithm class *)
  class ['node, 'key] t =
  object(self)

    (** pickseeds : choose a couple of elements from the values to be partitioned *)
    method pickseeds (l : 'node list) = 
      match l with 
	  a::b::t -> a, b, t
	| _       -> raise (Error "pickseed : less than 2 elements")

    method private fillup =
      let rec fillup l1 (l2, r2) le =
	match le with
	    []    -> l1, (l2, r2)
	  | e::le -> fillup l1 (e::l2, r2#expand e#key) le
      in fillup

    method private putnext =
      let rec putnext (e1, l1, r1, i1) (e2, l2, r2, i2) le =
	(* check if we reached threshold on either list *)
	match i1, i2 with
	    _,v when v > Def.minimum -> self#fillup (l2, r2) (l1, r1) le
	  | v,_ when v > Def.minimum -> self#fillup (l1, r1) (l2, r2) le
	  | _                        -> (
	      (* add e to the list l *)
	      let add e1 l r e i =
		(e1, e::l, r#expand e#key, succ i)
	      in
		match le with
		    []    -> (l1, r1), (l2, r2)
		  | e::lr -> (
		      let re = e#key in
			if r1#area_increase re <= r2#area_increase re 
			then putnext (add e1 l1 r1 e i1) (e2, l2, r2, i2) lr
			else putnext (e1, l1, r1, i1) (add e2 l2 r2 e i2) lr
		    )
	    )
      in putnext

    (** split: the algorithm main entry point *)
    method split l =
      let s1, s2, ll = self#pickseeds l
      in
	self#putnext (s1, [s1], s1#key, 1) (s2, [s2], s2#key, 1) ll
	  
  end

end
