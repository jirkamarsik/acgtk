module type UnionFind =
  sig
    type 'a t
    type 'a content = Link_to of int | Value of 'a
    val create : ('a content) list -> 'a t
    val find : int -> 'a t -> (int * 'a t)
    val union : ?use_rank:bool -> int -> int -> 'a t -> 'a t
    val cyclic : int -> 'a t -> (bool*'a t)
end
    

module type Store =
sig
  type 'a t
  exception Not_found
  val empty : 'a t
  val get : int -> 'a t -> 'a
  val set : int -> 'a -> 'a t -> 'a t
end


module Make (S:Store) : UnionFind 
