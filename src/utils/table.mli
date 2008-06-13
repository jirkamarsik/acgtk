module type BASE =
  sig
    val b : int
  end


module type TABLE =
  sig
    exception Not_found
    exception Conflict
    type 'a t
    type key = int
(*    val create : unit -> 'a t
    val insert : int -> 'a -> 'a t -> 'a t
    val lookup : int -> 'a t -> 'a *)
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val fold : (key -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  end


module Make_table (Base : BASE) : TABLE 
