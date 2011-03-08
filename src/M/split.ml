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

(** rtree node splitting algorithm functor signature *)
module type T =
sig
  type node_t
  type key_t
  val split : node_t list -> (node_t list * key_t) * (node_t list * key_t)
end

module type Make = 
  functor (Coord : Vec.T) ->
    functor (N : Node.T with type scalar_t = Coord.Scalar.t) -> 
      functor (Def : Def.T) -> T with type node_t = N.node_t and type key_t = N.key_t
