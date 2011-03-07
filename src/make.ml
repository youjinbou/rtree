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

open Debug

(** the functor used to build a rtree handling module *)

module Make = 
  functor (Split : Rtreesplit.Make) -> 
    functor (Region : Region.Make)  ->
      functor (Coord: Vec.T)        -> 
	functor (Def : Rtreedef.T)  -> 
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

  (* Region Boxen -------------------------------------------------------- *)

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

  type t = { mutable root : node_t }

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
      filter tree.root f

  (* ------------------------------------------------------------------------------- *)

  module Cell : Splitnode.T with type scalar_t = Coord.Scalar.t and type node_t = cell_t and type key_t = key_t =
  struct
    type node_t   = cell_t
    type k_t      = key_t (* redefine upper module type to avoid a cycle *)
    type key_t    = k_t
    type scalar_t = Coord.Scalar.t
    let getkey        = fst
    let area_increase = area_increase
    let expand r1 r2  = R.expand r1 r2
  end

  module Node : Splitnode.T with type scalar_t = Coord.Scalar.t and type node_t = node_t and type key_t = key_t =
  struct 
    type t        = node_t
    type node_t   = t
    type k_t      = key_t (* redefine upper module type to avoid a cycle *)
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
      match insert tree.root key value with
	  n1, None    -> tree.root <- n1
	| n1, Some n2 -> tree.root <- Node ((R.expand (getkey n1)) (getkey n2), [n2;n1])
	    

  (** create a tree containing the couple (key,value) *) 
  let make key value = {root = Leaf (key,[(key,value)])}

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
      iter_node tree.root

  (* ------------------------------------------------------------------------------- *)

  (** dump the tree to dotty format *)
  let dump tree filename directory =
    let file = open_out (directory^"/"^filename^".dot") in
    let output_str s = output_string file s
    in
    let dump_link parent child =
      output_str (parent^":i"^child^" -> n_"^child^":n;\n")
    and string_of_key k = 
      R.to_string k
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
      dump_ "root" tree.root;
      output_str "}\n";
      close_out file

end

(** rtrees with splitting algorithms already included *)
module Linear = Make(Lsplit.Make)(Rbox.Make)
module Quadratic = Make(Qsplit.Make)(Rbox.Make)

