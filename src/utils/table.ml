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


module Make_table (Base : BASE) : TABLE =

  struct

    exception Not_found

    type 'a option = None | Some of 'a

    type 'a t = 
        Nil
      | T of ('a option * 'a t) array

    let create () = T (Array.create Base.b (None, Nil))

    let insert n attr table = 
      let rec insert1 n table =
        match table with
          Nil  -> insert1 n (create ())
        | T ar -> let (r, i) = (n / Base.b, n mod Base.b) in
                  let (a, tb) = ar.(i) 
                  in 
                  if r = 0
                  then (ar.(i) <- (Some attr, tb); T ar)
                  else (ar.(i) <- (a, insert1 r tb);T ar)
      in
      insert1 n table

    let rec lookup n table =
      match table with
        Nil  -> raise Not_found
      | T ar -> let (r, i) = (n / Base.b, n mod Base.b) in
                let (a, tb) = ar.(i)
                in
                if r = 0
                then match a with
                       None   -> raise Not_found
                     | Some b -> b 
                else lookup r tb 

  end
