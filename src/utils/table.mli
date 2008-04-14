module type BASE =
  sig
    val b : int
  end


module type TABLE =
  sig
    exception Not_found
    type 'a t
    val create : unit -> 'a t
    val insert : int -> 'a -> 'a t -> 'a t
    val lookup : int -> 'a t -> 'a
  end


module Make_table (Base : BASE) : TABLE 
