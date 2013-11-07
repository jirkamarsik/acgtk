%{
  open IdGenerator
  open Datalog_AbstractSyntax
%}

%token <string> IDENT
%token <int> INT
%token LPAR RPAR COMMA DOT FROM EOI SLASH QUESTION_MARK


%start rule program
%type <  (Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table * IdGenerator.IntIdGen.t * Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIds.t*Datalog_AbstractSyntax.ConstGen.Table.table) -> (Datalog_AbstractSyntax.AbstractSyntax.Proto_Rule.t*(Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table * IdGenerator.IntIdGen.t * Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIds.t*Datalog_AbstractSyntax.ConstGen.Table.table))> rule
%type < Datalog_AbstractSyntax.AbstractSyntax.Proto_Rule.t list -> (Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table * IdGenerator.IntIdGen.t*Datalog_AbstractSyntax.ConstGen.Table.table) -> Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIds.t -> (Datalog_AbstractSyntax.AbstractSyntax.Proto_Rule.t list * Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table * Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIds.t*Datalog_AbstractSyntax.ConstGen.Table.table)> program
%type < Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table -> (Datalog_AbstractSyntax.VarGen.Table.table * Datalog_AbstractSyntax.ConstGen.Table.table) -> Datalog_AbstractSyntax.AbstractSyntax.Predicate.predicate * Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table * (Datalog_AbstractSyntax.VarGen.Table.table * Datalog_AbstractSyntax.ConstGen.Table.table)> query
   
%%
  
  program :
 | rule EOI { fun rules (pred_id_table,rule_id_gen,const_table) i_preds ->
   let rule,(new_pred_id_table,_,new_i_preds,new_const_table) = $1 (pred_id_table,rule_id_gen,i_preds,const_table) in
   ((rule::rules),new_pred_id_table,new_i_preds,new_const_table)
	    }
 | rule program { fun rules (pred_id_table,rule_id_gen,const_table) i_preds ->
   let rule,(new_pred_id_table,new_rule_id_gen,new_i_preds,new_const_table) = $1 (pred_id_table ,rule_id_gen,i_preds,const_table) in
   $2 (rule::rules) (new_pred_id_table,new_rule_id_gen,new_const_table) new_i_preds }
     
     
     rule :
 | predicate_with_arity DOT { fun (pred_id_table,rule_id_gen,i_preds,const_table) -> 
   let rule_id,new_rule_id_gen=IntIdGen.get_fresh_id rule_id_gen in
   let lhs,new_pred_id_table,(_,new_const_table)= $1 pred_id_table (VarGen.Table.empty,const_table) in
   {AbstractSyntax.Proto_Rule.proto_id=rule_id;
    AbstractSyntax.Proto_Rule.proto_lhs=lhs;
    AbstractSyntax.Proto_Rule.proto_rhs=[]},
   (new_pred_id_table,new_rule_id_gen,i_preds,new_const_table)}
     
 | predicate_with_arity FROM predicate_list DOT { fun (pred_id_table,rule_id_gen,i_preds,const_table) -> 
   let rule_id,new_rule_id_gen=IntIdGen.get_fresh_id rule_id_gen in
   let lhs,new_pred_id_table,new_tables=$1 pred_id_table (VarGen.Table.empty,const_table) in
   let rhs,new_pred_id_table',(_,new_const_table)=$3 new_pred_id_table new_tables in
   {AbstractSyntax.Proto_Rule.proto_id=rule_id;
    AbstractSyntax.Proto_Rule.proto_lhs=lhs;
    AbstractSyntax.Proto_Rule.proto_rhs=rhs},
   (new_pred_id_table',new_rule_id_gen,AbstractSyntax.Predicate.PredIds.add lhs.AbstractSyntax.Predicate.p_id i_preds,new_const_table)}
     
     
     predicate_list :
 | predicate_with_arity {fun pred_id_table tables -> 
   let predicate,new_pred_id_table,new_tables= $1 pred_id_table tables in
   [predicate],new_pred_id_table,new_tables }
 | predicate_with_arity COMMA predicate_list {fun pred_id_table tables ->
   let predicate,new_pred_id_table,new_tables= $1 pred_id_table tables in
   let remaining_pred,new_pred_id_table',new_tables'=$3 new_pred_id_table new_tables in
   predicate::remaining_pred,new_pred_id_table',new_tables' }

     predicate_with_arity :
 | IDENT SLASH INT LPAR parameters RPAR {fun pred_id_table tables ->
   let parameters,new_tables=$5 tables in
   let length=List.length parameters in
   if $3<>length then
     let () = Printf.fprintf stderr "The specified arity of predicate '%s/%d' does not match the actual number of arguments (%d)\n%!" $1 $3 length in
     raise Parsing.Parse_error
   else
     let new_sym = Printf.sprintf "%s/%d" $1 length in
     let pred_id,new_pred_id_table = AbstractSyntax.Predicate.PredIdTable.add_sym new_sym pred_id_table in
     {AbstractSyntax.Predicate.p_id=pred_id;
      AbstractSyntax.Predicate.arity=List.length parameters;
      AbstractSyntax.Predicate.arguments=parameters},new_pred_id_table,new_tables }
 | predicate {$1}

     
     predicate :
 | IDENT LPAR parameters RPAR {fun pred_id_table tables ->
   let parameters,new_tables=$3 tables in
   let new_sym = Printf.sprintf "%s/%d" $1 (List.length parameters) in
   let pred_id,new_pred_id_table = AbstractSyntax.Predicate.PredIdTable.add_sym new_sym pred_id_table in
   {AbstractSyntax.Predicate.p_id=pred_id;
    AbstractSyntax.Predicate.arity=List.length parameters;
    AbstractSyntax.Predicate.arguments=parameters},new_pred_id_table,new_tables }
     
     parameters:
 | parameter {fun tables ->
   let par,new_tables=$1 tables in
   [par],new_tables}
 | parameter COMMA parameters {fun tables -> 
   let par,new_tables=$1 tables in
   let other_parameters,new_tables'=$3 new_tables in
   par::other_parameters,new_tables'}
     
     parameter :
 | INT {fun (var_table,const_table) -> 
   let cst,new_const_table=ConstGen.Table.add_sym (string_of_int $1) const_table in
   AbstractSyntax.Predicate.Const cst,(var_table,new_const_table)}
 | IDENT {fun (var_table,const_table) -> 
   let var,new_var_table=VarGen.Table.add_sym $1 var_table in
   AbstractSyntax.Predicate.Var var,(new_var_table,const_table)}

     query:
 | predicate QUESTION_MARK {$1}
     
%%
