%{
  open IdGenerator
  open Datalog_AbstractSyntax
%}

%token <string> IDENT
%token <int> INT
%token LPAR RPAR COMMA DOT FROM EOI SLASH QUESTION_MARK


%start rule program
%type <  Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t -> Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t > rule
%type <  Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t -> Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t > program
%type <  Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t -> Datalog_AbstractSyntax.AbstractSyntax.Predicate.predicate * Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t > query
   
%%
  
  program :
 | rule EOI { fun prog -> $1 prog }
 | rule program { fun prog ->
   let new_prog = $1 prog in
   $2 new_prog}
     
     
     rule :
 | predicate_with_arity DOT { fun prog -> 
   AbstractSyntax.Proto_Program.add_proto_rule ($1,fun t -> [],t) prog}
     
 | predicate_with_arity FROM predicate_list DOT { fun prog -> 
   AbstractSyntax.Proto_Program.add_proto_rule ($1,$3) prog }     
     
     predicate_list :
 | predicate_with_arity {fun (pred_id_table,tables) -> 
   let predicate,(new_pred_id_table,new_tables)= $1 (pred_id_table,tables) in
   [predicate],(new_pred_id_table,new_tables) }
 | predicate_with_arity COMMA predicate_list {fun (pred_id_table,tables) ->
   let predicate,(new_pred_id_table,new_tables)= $1 (pred_id_table,tables) in
   let remaining_pred,(new_pred_id_table',new_tables')=$3 (new_pred_id_table,new_tables) in
   predicate::remaining_pred,(new_pred_id_table',new_tables') }

     predicate_with_arity :
 | IDENT SLASH INT LPAR parameters RPAR {fun (pred_id_table,tables) ->
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
      AbstractSyntax.Predicate.arguments=parameters},(new_pred_id_table,new_tables)}
 | predicate {$1}

     
     predicate :
 | IDENT LPAR parameters RPAR {fun (pred_id_table,tables) ->
   let parameters,new_tables=$3 tables in
   let new_sym = Printf.sprintf "%s/%d" $1 (List.length parameters) in
   let pred_id,new_pred_id_table = AbstractSyntax.Predicate.PredIdTable.add_sym new_sym pred_id_table in
   {AbstractSyntax.Predicate.p_id=pred_id;
    AbstractSyntax.Predicate.arity=List.length parameters;
    AbstractSyntax.Predicate.arguments=parameters},(new_pred_id_table,new_tables) }
     
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
 | predicate QUESTION_MARK {fun prog ->
   let pred,(new_pred_table,(_,new_cst_table))=$1 AbstractSyntax.Proto_Program.(prog.pred_table,(VarGen.Table.empty,prog.const_table)) in
   pred,AbstractSyntax.Proto_Program.({prog with pred_table=new_pred_table ; const_table=new_cst_table})}
     
%%
