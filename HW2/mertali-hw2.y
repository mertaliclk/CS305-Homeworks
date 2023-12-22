%{
     #include <stdio.h>
     void yyerror(const char *msg){
     return;
     }
%}

%token tSTRING tCOMMA tLBRAC tRBRAC tGET tSET tFUNCTION tPRINT tIF tRETURN tADD tSUB tMUL tDIV tINC tGT tEQUALITY tDEC tLT tLEQ tGEQ tIDENT tNUM
%expect 5
%start jisp

%%

jisp : tLBRAC tRBRAC
     | tLBRAC statements tRBRAC    
;
statements : statements statements
           | set_statement
           | print_statement
           | inc_statement
           | dec_statement
           | return_statement
           | if_statement
           | expr
;
set_statement : tLBRAC tSET tCOMMA tIDENT tCOMMA expr tRBRAC
;
print_statement : tLBRAC tPRINT tCOMMA expr tRBRAC
;
inc_statement : tLBRAC tINC tCOMMA tIDENT tRBRAC
;
dec_statement : tLBRAC tDEC tCOMMA tIDENT tRBRAC
;
return_statement : tLBRAC tRETURN tRBRAC
                 | tLBRAC tRETURN tCOMMA expr tRBRAC
;
if_statement : tLBRAC tIF tCOMMA condition tCOMMA then_stat tRBRAC
             | tLBRAC tIF tCOMMA condition tCOMMA then_stat then_stat tRBRAC
;
then_stat : tLBRAC statements tRBRAC
          | tLBRAC tRBRAC
;
operators : tLT 
          | tLEQ
          | tEQUALITY
          | tGT
          | tGEQ
;
condition : tLBRAC operators tCOMMA expr tCOMMA expr tRBRAC
;
expr : tNUM
     | tSTRING
     | get_expr
     | f2_dec
     | operator_app
     | condition
;
listof_expr : expr
            | listof_expr tCOMMA listof_expr
;
exprlist : tLBRAC listof_expr tRBRAC
         | tLBRAC tRBRAC
;
get_expr : tLBRAC tGET tCOMMA tIDENT tRBRAC
         | tLBRAC tGET tCOMMA tIDENT tCOMMA exprlist tRBRAC
;
listof_prm : tIDENT
           | listof_prm tCOMMA listof_prm
;
declist : tLBRAC listof_prm tRBRAC
        | tLBRAC tRBRAC
;
f2_dec : tLBRAC tFUNCTION tCOMMA declist tCOMMA tLBRAC statements tRBRAC tRBRAC
       | tLBRAC tFUNCTION tCOMMA declist tCOMMA tLBRAC tRBRAC tRBRAC
;
calculators : tADD
            | tSUB
            | tMUL
            | tDIV
;
operator_app : tLBRAC calculators tCOMMA expr tCOMMA expr tRBRAC
;




%%

int main () {
   if (yyparse()) {
      // parse error
      printf("ERROR\n");
      return 1;
    } 
    else {
      // successful parsing
      printf("OK\n");
      return 0;
    }
} 