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
open Debug


module Make = 
  functor (Scalar : Scalar.T)  ->
  functor (Split : Split.Make) ->
    functor (Def : Def.T)      ->
struct

  open List
  open Region

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


      constraint 'node = (scalar_t) node
      constraint 'key = (scalar_t) region

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

  end

end
