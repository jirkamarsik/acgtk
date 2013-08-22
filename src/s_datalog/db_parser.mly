%{
  open Table
  open Rules
%}

%token <string> IDENT
%token LPAR RPAR COMMA DOT FROM EOI


%start rule
%type < Rules.VarGen.id Rules.Pred_and_Rules.rule> rule

%%
rule :
 | predicate DOT { {Pred_and_Rules.id=0;
		    Pred_and_Rules.lhs=fst($1 VarGen.Corr.empty);
		    Pred_and_Rules.e_rhs=[];
		    Pred_and_Rules.i_rhs=[]}
		 }
 | predicate FROM predicate_list DOT {
   let lhs,new_table=$1 VarGen.Corr.empty in
   {Pred_and_Rules.id=0;
    Pred_and_Rules.lhs=lhs;
    Pred_and_Rules.e_rhs=[];
    Pred_and_Rules.i_rhs=fst ($3 new_table)}
 }
     
     
     predicate_list :
 | predicate {fun table -> 
   let predicate,new_table= $1 table in
   [predicate],new_table }
 | predicate COMMA predicate_list {fun table ->
   let predicate,new_table= $1 table in
   let remaining_pred,new_table'=$3 new_table in
   predicate::remaining_pred,new_table' }
     
     predicate :
 | IDENT LPAR parameters RPAR {fun table -> 
   let parameters,new_table=$3 table in
   {Pred_and_Rules.p_id=$1;
    Pred_and_Rules.arity=List.length parameters;
    Pred_and_Rules.components=parameters},new_table }
     
     parameters :
 | IDENT {fun table -> 
   let var,new_table=VarGen.Corr.add_sym $1 table in
   [Pred_and_Rules.Var var],new_table}
 | IDENT COMMA parameters {fun table -> 
   let var,new_table=VarGen.Corr.add_sym $1 table in
   let other_parameters,new_table'=$3 new_table in
   (Pred_and_Rules.Var var)::other_parameters,new_table'}

%%
