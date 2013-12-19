open Datalog_AbstractSyntax
  
				     
module Make(Sg:Interface.Signature_sig with type  term = Lambda.Lambda.term and type stype = Lambda.Lambda.stype):
sig
  val generate_and_add_rule :
    abs_cst:(string*Lambda.Lambda.stype) ->
    obj_princ_type:Lambda.Lambda.stype  ->
    obj_typing_env:(Lambda.Lambda.term * Lambda.Lambda.stype) Utils.IntMap.t ->
    Datalog.Datalog.Program.program -> 
    abs_sig:Sg.t ->
    obj_sig:Sg.t ->
    (AbstractSyntax.Rule.rule * Datalog.Datalog.Program.program)
end
