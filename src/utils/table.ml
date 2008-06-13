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

module Make_table (Base : BASE)=
  struct

    exception Not_found
    exception Conflict

(*    type 'a option = None | Some of 'a *)

    type 'a t = 
        Nil
      | T of ('a option * 'a t) array

    type key = int

    let create () = T (Array.create Base.b (None, Nil))

    let empty =  Nil

    let add n attr table = 
      let rec insert1 n table =
        match table with
          Nil  -> insert1 n (create ())
        | T ar -> let (r, i) = (n / Base.b, n mod Base.b) in
                  let (a, tb) = ar.(i) 
                  in 
                  if r = 0
                  then 
		    match a with
		      | None -> ar.(i) <- (Some attr, tb); T ar
		      | Some _ -> raise Conflict
                  else (ar.(i) <- (a, insert1 r tb);T ar)
      in
      insert1 n table

    let rec find n table =
      match table with
        Nil  -> raise Not_found
      | T ar -> let (r, i) = (n / Base.b, n mod Base.b) in
                let (a, tb) = ar.(i)
                in
                if r = 0
                then match a with
                       None   -> raise Not_found
                     | Some b -> b 
                else find r tb 

    let fold f acc table =
      let rec fold_aux q acc = function
	| Nil -> acc
	| T ar ->
	    let _,new_acc =
	      Array.fold_left
		(fun (i,acc) -> function
		   | Some v,_ -> i+1,f (q*Base.b+i) v acc
		   | None,_ -> i+1,acc)
		(0,acc)
		ar in
	      snd (Array.fold_left
		(fun (i,acc) (_,t) -> i+1,fold_aux (q+1) acc t)
		(0,new_acc)
		ar) in
	fold_aux 0 acc table
	      
		  

  end
