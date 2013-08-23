%{
  open IdGenerator
  open Rules
%}

%token <string> IDENT
%token <int> INT
%token LPAR RPAR COMMA DOT FROM EOI


%start rule program
%type <  (Rules.AbstractSyntax.Predicate.PredIdTable.table * IdGenerator.IntIdGen.t * Rules.AbstractSyntax.Predicate.PredIds.t) -> (Rules.AbstractSyntax.Rule.proto_rule*(Rules.AbstractSyntax.Predicate.PredIdTable.table * IdGenerator.IntIdGen.t * Rules.AbstractSyntax.Predicate.PredIds.t))> rule
%type < Rules.AbstractSyntax.Rule.proto_rule list -> (Rules.AbstractSyntax.Predicate.PredIdTable.table * IdGenerator.IntIdGen.t) -> Rules.AbstractSyntax.Predicate.PredIds.t -> (Rules.AbstractSyntax.Rule.proto_rule list * Rules.AbstractSyntax.Predicate.PredIdTable.table * Rules.AbstractSyntax.Predicate.PredIds.t)> program
   
%%
  
  program :
 | rule EOI { fun rules (pred_id_table,rule_id_gen) i_preds ->
   let rule,(new_pred_id_table,_,new_i_preds) = $1 (pred_id_table ,rule_id_gen,i_preds) in
   ((rule::rules),new_pred_id_table,new_i_preds)
	    }
 | rule program { fun rules (pred_id_table,rule_id_gen) i_preds ->
     let rule,(new_pred_id_table,new_rule_id_gen,new_i_preds) = $1 (pred_id_table ,rule_id_gen,i_preds) in
     $2 (rule::rules) (new_pred_id_table,new_rule_id_gen) new_i_preds }
 
       
       rule :
 | predicate DOT { fun (pred_id_table,rule_id_gen,i_preds) -> 
   let rule_id,new_rule_id_gen=IntIdGen.get_fresh_id rule_id_gen in
   let lhs,new_pred_id_table,_= $1 pred_id_table (VarGen.Table.empty,ConstGen.Table.empty) in
   {AbstractSyntax.Rule.proto_id=rule_id;
    AbstractSyntax.Rule.proto_lhs=lhs;
    AbstractSyntax.Rule.proto_rhs=[]},
   (new_pred_id_table,new_rule_id_gen,i_preds)}
     
 | predicate FROM predicate_list DOT { fun (pred_id_table,rule_id_gen,i_preds) -> 
   let rule_id,new_rule_id_gen=IntIdGen.get_fresh_id rule_id_gen in
   let lhs,new_pred_id_table,new_tables=$1 pred_id_table (VarGen.Table.empty,ConstGen.Table.empty) in
   let rhs,new_pred_id_table',_=$3 new_pred_id_table new_tables in
   {AbstractSyntax.Rule.proto_id=rule_id;
    AbstractSyntax.Rule.proto_lhs=lhs;
    AbstractSyntax.Rule.proto_rhs=rhs},
   (new_pred_id_table',new_rule_id_gen,AbstractSyntax.Predicate.PredIds.add lhs.AbstractSyntax.Predicate.p_id i_preds)}
     
     
     predicate_list :
 | predicate {fun pred_id_table tables -> 
   let predicate,new_pred_id_table,new_tables= $1 pred_id_table tables in
   [predicate],new_pred_id_table,new_tables }
 | predicate COMMA predicate_list {fun pred_id_table tables ->
   let predicate,new_pred_id_table,new_tables= $1 pred_id_table tables in
   let remaining_pred,new_pred_id_table',new_tables'=$3 new_pred_id_table new_tables in
   predicate::remaining_pred,new_pred_id_table',new_tables' }
     
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
     
%%
