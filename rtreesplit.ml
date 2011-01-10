module type T =
sig
  type node_t
  type key_t
  val split : node_t list -> (node_t list * key_t) * (node_t list * key_t)
end

module type Make = 
  functor (Coord : Vec.T) ->
    functor (N : Splitnode.T with type scalar_t = Coord.Scalar.t) -> 
      functor (Def : Rtreedef.T) -> T with type node_t = N.node_t and type key_t = N.key_t
