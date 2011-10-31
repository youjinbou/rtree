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

module M =
struct

  (** this module defines the rtree configuration parameters  *)
  module type DEF =
  sig

    (** minimum number of elements held by a node *)
    val minimum : int

    (** maximum number of elements held by a node *)
    val maximum : int

    (* datatype handled by the tree - could be 'a ? *)
    type t

  end  

  (* ------------------------------------------------------------ *)

  (** minimal scalar interface needed for the library *)
  module type SCALAR =
  sig
    type t
    val  one       : t
    val  add       : t -> t -> t
    val  sub       : t -> t -> t
    val  div       : t -> t -> t
(*    val  to_string : t -> string *)
  end

  (** minimal vector interface needed for the library *)
  module type VEC =
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

  (* ------------------------------------------------------------ *)

  (** rtree node module signature used by the splitting functor *)
  module type NODE =
  sig 
    type node_t
    type key_t 
    type scalar_t
    val getkey        : node_t -> key_t
    val area_increase : key_t -> key_t -> scalar_t
    val expand        : key_t -> key_t -> key_t
(*    val to_string     : key_t -> string *)

  end

  (* ------------------------------------------------------------ *)

  (** this is the module signature for the region datatype used in
      the rtree to delimit subregions of the space.
  *)
  module type REGION =
  sig

    type t
    type key_t
    type scalar_t

    val expand    : t -> t -> t
    val area      : t -> scalar_t
    val area_with : t -> t -> scalar_t
    val contains  : t -> t -> bool
(*    val to_string : t -> string *)

  end

  (** the functor required by the library to set up a module of 
      signature defined above using the Coordinate system 
      (also required - see Vec).
  *)
  module type REGION_FUNCTOR =
    functor (Coord: VEC) -> REGION with type scalar_t = Coord.Scalar.t 
				   and  type key_t = Coord.t

  (* ------------------------------------------------------------ *)

  (** rtree node splitting algorithm functor signature *)
  module type SPLIT =
  sig
    type node_t
    type key_t
    val split : node_t list -> (node_t list * key_t) * (node_t list * key_t)
  end

  module type SPLIT_FUNCTOR = 
    functor (Coord : VEC) ->
      functor (N : NODE with type scalar_t = Coord.Scalar.t) -> 
	functor (Def : DEF) -> SPLIT with type node_t = N.node_t 
				     and type key_t = N.key_t



  (** rtree module *)
  module type RTREE = 
  sig

    type key_t
    type value_t
    type t

    (** returns the list of values in the tree which are filtered by f *)
    val filter : t -> (key_t -> bool) -> (key_t * value_t) list

    (** create an empty tree *)
    val empty : t

    (** create a tree containing the couple (key,value) *) 
    val make : key_t-> value_t -> t

    (** find the value corresponding to key in the tree *)
    val find : t -> key_t -> value_t list

    (** insert a value with its associated key in the tree *)
    val insert : t -> key_t -> value_t -> unit

    (** iterate over all the values in the tree *)
    val iter : t -> (key_t -> unit) -> (key_t -> value_t -> unit) -> unit

    (** dump a tree to dotty format *)
    val dump : (key_t -> string) -> t -> string -> string -> unit

  end


end

(* ------------------------------------------------------------ *)

module O =
struct

  (** module type for rtree configuration data *)
  module type DEF =
  sig

    (** minimum number of elements held a node *)
    val minimum : int

    (** maximum number of elements held by a node *)
    val maximum : int

    (** datatype handled by the tree - could be 'a ? *)
    type t

  end  

  (* ------------------------------------------------------------ *)

  (** minimal scalar interface needed for the library *)
  module type SCALAR =
  sig
    type t
    val  zero      : t
    val  one       : t
    val  neg       : t -> t
    val  add       : t -> t -> t
    val  sub       : t -> t -> t
    val  mul       : t -> t -> t
    val  div       : t -> t -> t
(*    val  to_string : t -> string *)
  end

  (* ------------------------------------------------------------ *)

  (** the region class full type *)
  class type ['scalar] region =
  object
    
    method expand    : 'scalar region -> 'scalar region
    method area      : 'scalar
    method area_with : 'scalar region -> 'scalar
    method area_increase : 'scalar region -> 'scalar
    method contains  : 'scalar region -> bool

  end

  (* ------------------------------------------------------------ *)

  (** rtree node class signature used by the splitting functor *)
  class type ['scalar] node =
  object
    method key    : ('scalar) region
  end

  (* ------------------------------------------------------------ *)

  (** rtree node splitting algorithm functor signature *)
  module type SPLIT_FUNCTOR = 
    functor (Scalar : SCALAR)  ->
      functor (Def  : DEF)     -> 
  sig 

    class ['node, 'key] t :
    object 

      constraint 'node = Scalar.t #node
      constraint 'key  = Scalar.t region
      method split     : 'node list -> ('node list * 'key) * ('node list * 'key)
    end

  end

  (* ------------------------------------------------------------ *)

  module type RTREE = 
  sig

    type value_t
    type scalar_t

    class ['key] t : 'key -> value_t ->
    object 
      
      constraint 'key = scalar_t region

      method key    : 'key
      method insert : 'key -> value_t -> unit
      method find   : 'key -> value_t list
      method filter : ('key -> bool) -> (value_t list)

    end

  end

end
