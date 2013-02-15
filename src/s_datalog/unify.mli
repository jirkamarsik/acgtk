module type UnionFind = 
  sig
    type 'a t
    type 'a content = Link_to of int | Value of 'a
    exception Union_Failure
    val create : 'a content list -> 'a t
    val find : int -> 'a t -> ((int * 'a content) * 'a t)
    val union : int -> int -> 'a t -> 'a t
    val cyclic : int -> 'a t -> (bool * 'a t)
end
    

module type Store =
sig
  type 'a t
  exception Not_found
  val empty : int -> 'a t
  val get : int -> 'a t -> 'a
  val set : int -> 'a -> 'a t -> 'a t
end


(*module Make (S:Store) : UnionFind  *)
