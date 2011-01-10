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
(*345678911234567892123456789312345678941234567895123456789612345678971234567898*)

module Make (Coord: Vec.T) =
struct 
  class t (p1 : Coord.t) (p2 : Coord.t) = 
    let b_ = Coord.map2 min p1 p2
    and t_ = Coord.map2 max p1 p2
    in
  object(self)

    method bottom = b_
    method top    = t_

    method dims   = Coord.sub t_ b_

    method center = 
      let half = Coord.Scalar.div Coord.Scalar.one (Coord.Scalar.add Coord.Scalar.one Coord.Scalar.one)
      in
      let d = (Coord.scale self#dims half)
      in 
	Coord.add b_ d
	  
    (* volume covered by self *)
    val area_ = 
      let x = Coord.sub t_ b_ in
	Coord.dot x x

    (* whether the rbox rb overlaps with self *)
    method overlaps (rb : t) = 
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
    method includes (rb : t) =
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
	  
    method area () = area_

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

    (* create new rbox covering self and the rbox rb *)
    method expand (rb : t) =
      let bottom = Coord.map2 min rb#bottom b_
      and top    = Coord.map2 max rb#top t_
      in
	new t bottom top

    method area_with (rb : t) = 
      (self#expand rb)#area ()

  end
end
