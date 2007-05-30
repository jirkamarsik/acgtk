module Tries =

  struct

    type 'a option = None | Some of 'a

    type 'a t = ST of 'a option * (char * 'a t) list

    exception Not_found

    exception Conflict
 
    let empty = ST (None, [])

    let explode str = 
      let rec explode_aux i ls =
        if i = -1 then ls
                  else explode_aux (i-1) ((String.get str i)::ls)
      in
      explode_aux (String.length str - 1) []

    let insert id attr smtb =
      let rec insert1 lts (ST (a, s)) =
        match lts with
          []    -> (match a with
                      None   -> ST (Some attr, s)
                    | Some b -> raise Conflict)
        | l::rm -> ST (a, insert2 l rm s)
      and     insert2 lt lts stls =
        match stls with
          []        -> [lt, insert1 lts empty]  
        | (l,i)::rm -> if lt = l 
                       then (lt, insert1 lts i)::rm
                       else if lt <= l
                            then (lt, insert1 lts empty)::stls
                            else (l,i)::(insert2 lt lts rm)
      in 
      insert1 (explode id) smtb

    let lookup w smtb =
      let rec lookup1 lts (ST (a, s)) = 
        match lts with
          []    -> (match a with 
                      None   -> raise Not_found
                    | Some b -> b)
        | l::rm -> lookup2 l rm s
      and     lookup2 lt lts stls =
        match stls with 
          []        -> raise Not_found
        | (l,i)::rm -> if lt = l 
                       then lookup1 lts i
                       else if lt <= l
                            then raise Not_found
                            else lookup2 lt lts rm
      in
      lookup1 (explode w) smtb

      let content tr =
        let rec tr_to_list tr ls =
          match tr with
            ST (None, [])          -> ls
          | ST (Some a, [])        -> a::ls
          | ST (None, (_,t)::rm)   -> trls_to_list rm (tr_to_list t ls)
          | ST (Some a, (_,t)::rm) -> trls_to_list rm (tr_to_list t (a::ls))
        and trls_to_list trls ls =
          match trls with
            []         -> ls
          | (_, t)::rm ->  trls_to_list rm (tr_to_list t ls)
      in
      List.rev (tr_to_list tr [])
  end
