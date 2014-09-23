module type Manager_sig =
  sig
    type t
    type elt
    val add_dependency : elt -> elt -> t -> t
    val dependencies : elt -> t -> elt list
  end


module Make(O:OrderedType) : Manager_sig with type elt=O.t
