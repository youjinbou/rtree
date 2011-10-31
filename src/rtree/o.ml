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


(** Quadratic split algorithm for Rtree :
    This  is  a straight implementation of the algorithm described 
    in the original paper.
*)

open Sig.O


module Qsplit : SPLIT_FUNCTOR =
  functor (Scalar : SCALAR)  ->
    functor (Def : DEF)      -> 
struct

  exception Error of string
      
  class ['node, 'key] t =
  object(self)

    constraint 'node = Scalar.t #node 
    constraint 'key  = Scalar.t region

    method private pickseeds (l : 'node list) : 'node * 'node * 'node list = 
     (* pick the entry in @l maximizing the area with @c1
	@e1 fixed entry against which we try every other entries of @l
	@e2 the current best pick in @l
	@a  the area covered by @e1,@e2
	@l  the list of entries we match @e1 with
	@l' the remaining entries of @l after we found @e2
     *)
     let rec pickbest e1 e2 a l l' =
       match l with
	   []    -> e2,a,l'
	 | x::xs -> (
	   let xa = e1#key#area_increase x#key in
	   if xa > a
	   then pickbest e1 x xa xs (e2::l')
	   else pickbest e1 e2 a xs (x::l')
	 )
     in
     let rec pickseed c1 c2 a l l' =
       match l with
	 | []    -> c1, c2, l'
	 | x::xs -> (
	   let xa = c1#key#area_increase x#key in
	   let x2, xa, xl = pickbest x c1 xa (c2::l') [] in
	   if xa > a
	   then pickseed x x2 xa xs xl
	   else pickseed c1 c2 a xs (x::l')
	 )
     in	     
     match l with
       | c1::c2::cs -> (
	 let a = c1#key#area_increase c2#key in
	 let c2, a, cs = pickbest c1 c2 a cs [] in
	 pickseed c1 c2 a cs []
       )
       | _          -> raise (Error "pickseeds: less than 2 elements")
	

    method private fillup l1 (l2,r2) le =
      let rec fillup l1 (l2,r2) le =
	match le with
	    []    -> l1, (l2, r2)
	  | e::le -> fillup l1 (e::l2, r2#expand e#key) le
      in fillup l1 (l2,r2) le

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
    method private picknext (l1, r1, i1) (l2, r2, i2) le =
    (* check if we reached threshold on either list *)
      match i1, i2 with
	  _,v when v > Def.minimum -> self#fillup (l2, r2) (l1, r1) le
	| v,_ when v > Def.minimum -> self#fillup (l1, r1) (l2, r2) le
	| _                        -> (
	  (* add e to the list l *)
	  let add l r e i =
	    e::l, r#expand e#key, succ i
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
		  then self#picknext (add l1 r1 e i1) (l2,r2,i2) lr
		  else self#picknext (l1,r1,i1) (add l2 r2 e i2) lr
	    )
	)

    (** algorithm entry point *)	  
    method split (l : 'node list) : ('node list * 'key) * ('node list * 'key)  =
      let s1, s2, ll = self#pickseeds l in
      self#picknext ([s1], s1#key, 1) ([s2], s2#key, 1) ll

  end


end


(** this module doesn't actually even require a formal definition 
    of  the  scalar  type  used  within, thanks to the comparison 
    operator polymorphy.
*)
module LSplit : SPLIT_FUNCTOR =
  functor (Scalar : SCALAR)  ->
    functor (Def  : DEF)     -> 
struct

  (** exception raised by the module *)
  exception Error of string

  (* algorithm functions ------------------------------------------------- *)
    
  (** algorithm class *)
  class ['node, 'key] t =
  object(self)

    constraint 'node = Scalar.t #node 
    constraint 'key  = Scalar.t region

    (** pickseeds : choose a couple of elements from the values to be partitioned *)
    method private pickseeds (l : 'node list) = 
      match l with 
	  a::b::t -> a, b, t
	| _       -> raise (Error "pickseed : less than 2 elements")

    method private fillup =
      let rec fillup l1 (l2, r2) le =
	match le with
	    []    -> l1, (l2, r2)
	  | e::le -> fillup l1 (e::l2, r2#expand e#key) le
      in fillup

    method private putnext (v1 : 'node * 'node list * 'key * int) 
      (v2 : 'node * 'node list * 'key * int)
      (l : 'node list)
      : ('node list * 'key) * ('node list * 'key) =
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
      in putnext v1 v2 l

    (** split: the algorithm main entry point *)
    method split (l : 'node list) : ('node list * 'key) * ('node list * 'key)  =
      let s1, s2, ll = self#pickseeds l
      in
	self#putnext (s1, [s1], s1#key, 1) (s2, [s2], s2#key, 1) ll
	  
  end

end

module Make
  (Scalar  : SCALAR)
  (Split   : SPLIT_FUNCTOR)
  (Def     : DEF) : 
  RTREE with type value_t = Def.t
	and  type scalar_t = Scalar.t   =
struct

  open Debug
  open List

  type scalar_t = Scalar.t
  type value_t = Def.t
 

  (* splitting algorithm ------------------------------------------------- *)

  module CSplit = Split(Scalar)(Def)
  module NSplit = Split(Scalar)(Def)
    
  let csplitter = new CSplit.t
  let nsplitter = new NSplit.t

  (* node class interface ------------------------------------------------ *)

  class type ['a] node =
  object
    method key      : ('a region)
    method insert   : ('a region) -> value_t -> ('a node) * ('a node) option
    method iter     : (('a region) -> value_t -> unit) -> unit
    method filter   : ('a region -> bool) -> value_t list
    method find     : ('a region) -> value_t list
  end

  (* leaf class ---------------------------------------------------------- *)

  (** Module Leaf 
      leaf node class module 
      the class handles a list of Cell.t instances
  *)

  module Leaf =
  struct

    class ['key] cell key value =
    object
      method key    : 'key     = key
      method value  :  value_t = value
    end

    class ['key] t children_ key_ =
    object(self)

      constraint 'key  = (scalar_t) region

      method key : 'key = key_

      method insert (key : 'key) (value : value_t) = 
	let c = new cell key value in
	  if length children_ > Def.maximum
	  then
	    let (l1, r1), (l2, r2) = csplitter#split (c::children_)
	    in 
	      new t l1 r1, Some (new t l2 r2)
	  else 
	    (new t (c::children_) (key_#expand key)), None

      method iter f = 
	iter (fun c -> f c#key c#value) children_

      method filter filter =
	fold_left (fun l c -> if filter c#key then c#value::l else l) [] children_

      method find (key : 'key) =
	List.map (fun x -> x#value) (List.find_all (fun c -> c#key#contains key) children_)

    end

    let make key value = new t [new cell key value] key

  end

  (* node class ---------------------------------------------------------- *)

  (** Module Node
      inner node class module
      the class handles a list of Node.t/Leaf.t instances
  *)
  module Node =
  struct

    class ['key] t (children_ : 'node list) (key_ : 'key) = 
    object(self)

      constraint 'node = (scalar_t) #node
      constraint 'key = (scalar_t)  region

      (** choose_leaf
	  extract the entry from the children with closest match to key
	  used when inserting a value 
      *)
      method private choose_leaf key =
	let least_area l1 l2 = 
	  let a = (l1#key)#area
	  and b = (l2#key)#area
	  in if a < b then -1 else if a > b then 1 else 0
	in
	let l = sort least_area children_
	in (hd l, tl l)

      method key = key_

      (** insert
	  insert (key, value) in the leaf best covering key
	  algorithm: 
	  1) select a matching child
	  2) recursively call insert on it
	  the result is the new subtree(s) containing the child
	  3) put it/them back in the list
	  4) if overflow, split the node and return new trees
	  else return new self minus old entry and including new entry(/ies)
      *)
      method insert (key : 'key) (value : value_t) : ('node) * ('node option) =
	Debug.string "insert node" "";
	let n,l = self#choose_leaf key in
	let cl = match n#insert key value with
	    (n1, None)    -> n1::l
	  | (n1, Some n2) -> n1::n2::l
	in
	  if length cl > Def.maximum 
	  then
	    let (l1, r1), (l2, r2) = nsplitter#split cl
	    in
	      new t l1 r1, Some (new t l2 r2)
	  else
	    (new t cl (key_#expand key)), None

      method iter f = 
	iter (fun a -> a#iter f) children_
	  
      method filter filter =
	if filter key_
	then fold_left (fun l nn -> (nn#filter filter) @ l) [] children_
	else []

      method find key =
	List.concat (List.map (fun x -> x#find key) (List.find_all (fun c -> c#key#contains key) children_))

    end

  end

  (* main class ---------------------------------------------------------- *)

  class ['key] t (key_ : 'key) (value_ : value_t) =
  object

    constraint 'key = (scalar_t) region

    val mutable root : (scalar_t) node = Leaf.make key_ value_

    method key : 'key = root#key

    method insert (key : 'key) (value : value_t) =
      match root#insert key value with 
	  n1, None    -> root <- n1
	| n1, Some n2 -> root <- new Node.t [n2;n1] (n1#key#expand n2#key) 

    method filter filter = root#filter filter

    method find key = root#find key

  end

end
