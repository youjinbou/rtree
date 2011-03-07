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

(** rbox is an implementation of the region module *)

module Make (Coord: Vec.T) =
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

  let string_of_coord c =
    let to_s = Coord.Scalar.to_string in
      match Coord.size with
	  1 -> "<"^(to_s (Coord.get c 1))^">"
	| n -> let l = (list_of_coord c)
	  in
	    "<"^(List.fold_left (fun acc x -> acc^";"^(to_s x)) (to_s (List.hd l)) (List.tl l))^">"

  (* -------------------------------------------------------------------- *)

  let to_string k =
    "["^(string_of_coord (k.top))^"-"^(string_of_coord (k.bottom))^"]"



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
    let half = Coord.Scalar.div Coord.Scalar.one (Coord.Scalar.add Coord.Scalar.one Coord.Scalar.one)
    in
    let d = (Coord.scale (dims v) half)
    in 
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
  let includes v rb =
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
