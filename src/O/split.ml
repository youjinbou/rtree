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
module type Make = 
  functor (Scalar : Scalar.T)  ->
    functor (Def : Def.T)      -> 
sig 

  class ['node] t :
  object 

    (* constraint 'node = Scalar.t Node.node *)

    method pickseeds : 'node list -> 'node * 'node
    method split     : 'node list -> ('node list * 'key) * ('node list * 'key)
  end

end
