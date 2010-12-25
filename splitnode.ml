module type T =
sig 
  type node_t
  type key_t 
  type scalar_t
  val getkey : node_t -> key_t
  val area_increase : key_t -> key_t -> scalar_t
  val expand : key_t -> key_t -> key_t

end
