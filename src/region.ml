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

module type T =
sig

  type t
  type key_t
  type scalar_t

  val expand    : t -> t -> t
  val area      : t -> scalar_t
  val area_with : t -> t -> scalar_t
  val overlaps  : t -> t -> bool
  val includes  : t -> t -> bool
  val to_string : t -> string

end


module type Make =
  functor (Coord: Vec.T) -> T with type scalar_t = Coord.Scalar.t and type key_t = Coord.t