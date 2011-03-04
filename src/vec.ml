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

(** minimal scalar interface needed for the library *)

module type SCALAR =
sig
  type t
  val  one    : t
  val  add    : t -> t -> t
  val  sub    : t -> t -> t
  val  div    : t -> t -> t
end

(** minimal vector interface needed for the library *)

module type T =
sig

  module Scalar : SCALAR

    
  type t

  (** the vector size *)
  val size    : int

  (** function to get the nth element from a vector *)
  val get     : t -> int -> Scalar.t

  (** mapping a function to 2 vectors *)
  val map2    : (Scalar.t -> Scalar.t -> Scalar.t) -> t -> t -> t
    
  (** add operation on 2 vectors *)
  val add     : t -> t -> t

  (** sub operation on 2 vectors *)
  val sub     : t -> t -> t

  (** product of a vector by a scalar *)
  val scale   : t -> Scalar.t -> t

  (** the usual dot product on 2 vectors *)
  val dot     : t -> t -> Scalar.t
end
