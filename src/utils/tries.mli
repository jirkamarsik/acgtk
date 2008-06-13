module Tries :
sig
  exception Not_found
  exception Conflict
  type 'a t
  type key=string
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val fold : (key -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
end
    
