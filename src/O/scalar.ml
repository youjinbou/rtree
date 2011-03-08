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

(** module signatures for scalar values *)

(** minimal scalar interface needed for the library *)
module type T =
sig
  type t
  val  zero      : t
  val  one       : t
  val  neg       : t -> t
  val  add       : t -> t -> t
  val  sub       : t -> t -> t
  val  mul       : t -> t -> t
  val  div       : t -> t -> t
  val  to_string : t -> string
end
