%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tutorial4.h"
void yyerror (const char *msg) /* Called by yyparse on error */ {
return; }

ExprNode * makeExpressionNodeFromIdent(IdentNode);
ExprNode * makeExpressionNodeFromNumber(NumberNode);
ExprNode * sumExpr(ExprNode *, ExprNode *, ExprNode *);
void assign(ExprNode *, ExprNode *);
int checkIdentifier(ExprNode *);

ExprNode ** expressions;
int expressionsSize = 100;
int exprIndex = 0;

int error = 0;

char ** errors;
int errorSize = 100;
int errorIndex = 0;

%}

%union{
    IdentNode identNode;
    NumberNode numberNode;
    ExprNode * exprNodePtr;
    int lineNum;
}

%token <lineNum> tASSIGN
%token <identNode> tIDENT
%token <numberNode> tNUMBER 

%token tPLUS
%start program

%type <exprNodePtr> expr

%%

program : calculationStatement program
        | assignmentStatement program
        |
;

calculationStatement : expr tASSIGN expr tPLUS expr{
                       sumExpr($1, $3, $5);
                    }
;

assignmentStatement : expr tASSIGN expr {
    assign($1, $3);
}
;

expr : tIDENT {
        $$ = makeExpressionNodeFromIdent($1);
    }
    | tNUMBER{
        $$ = makeExpressionNodeFromNumber($1);
    }
;

%%

ExprNode * makeExpressionNodeFromIdent(IdentNode ident){

    ExprNode * newNode = (ExprNode *)malloc(sizeof(ExprNode));
    newNode->identifier = ident.value;
    newNode->value = -1;
    newNode->lineNum = ident.lineNum;
    return newNode;
}

ExprNode * makeExpressionNodeFromNumber(NumberNode number){

    ExprNode * newNode = (ExprNode *)malloc(sizeof(ExprNode));
    newNode->identifier = NULL;
    newNode->value = atoi(number.value);
    newNode->lineNum = number.lineNum;
    return newNode;
}

ExprNode * sumExpr(ExprNode * expr, ExprNode * expr1, ExprNode * expr2){

    int expr_id1 = checkIdentifier(expr1);
    int expr_id2 = checkIdentifier(expr2);
    int result_id = checkIdentifier(expr);
    //printf("Result id %d\n", result_id);


    if(expr_id1 >= -1 && expr_id2 >= -1 ){
        if(expr_id1 >= 0){
            expr1 = expressions[expr_id1];
        }
        if(expr_id2 >= 0){
            expr2 = expressions[expr_id2];
        }
        if(result_id != -1){
            if(result_id >= 0){
                expr = expressions[result_id];
                expr->value = expr1->value + expr2->value;
            }
            else{
                expr->value = expr1->value + expr2->value;
                addExpressionToList(expr);
            }
        }
        else{
            error = 1;
            char * src =  "ERROR at line %d: Assigment to number is invalid\n";
            char * dest = (char *)malloc(strlen(src) + expr->lineNum + 10);
            sprintf(dest, src, expr->lineNum);
            if(errorIndex < errorSize){
                errors[errorIndex] = dest;
                errorIndex += 1;
            }
            else{
                errorSize = errorSize + errorSize;
                errors = realloc(errors, errorSize);
                errors[errorIndex] = dest;
                errorIndex += 1;
            }
        }
    }
    else{
        error = 1;
        if(expr_id1 == -2) {
            char * src =  "ERROR at line %d: %s is undefined\n";
            char * dest = (char *)malloc(strlen(src) + strlen(expr1->identifier) + expr1->lineNum + 10);
            sprintf(dest, src, expr1->lineNum, expr1->identifier);
            if(errorIndex < errorSize){
                errors[errorIndex] = dest;
                errorIndex += 1;
            }
            else{
                errorSize = errorSize + errorSize;
                errors = realloc(errors, errorSize);
                errors[errorIndex] = dest;
                errorIndex += 1;
            }
        }
        if(expr_id2 == -2) {
            char * src =  "ERROR at line %d: %s is undefined\n";
            char * dest = (char *)malloc(strlen(src) + strlen(expr2->identifier) + expr2->lineNum + 10);
            sprintf(dest, src, expr2->lineNum, expr2->identifier);
            if(errorIndex < errorSize){
                errors[errorIndex] = dest;
                errorIndex += 1;
            }
            else{
                errorSize = errorSize + errorSize;
                errors = realloc(errors, errorSize);
                errors[errorIndex] = dest;
                errorIndex += 1;
            }
        }
        return NULL;
    }
}


int checkIdentifier(ExprNode * expr){
    /* RETURNS:
    -1 -> Number
    -2 -> not found
    i >= 0 -> index of identifier
    */
    if(expr->identifier == NULL){
        return -1;
    }
    int i = 0;
    for(;i<exprIndex;i++){
        //printf("Expr 1 %s - Expr 2 %s\n", expr->identifier, expressions[i]->identifier);
        if(strcmp(expr->identifier, expressions[i]->identifier) == 0 ){
            //printf("Expr index %d identifier %s\n", i, expressions[i]->identifier);
            return i;
        }
    }
    return -2;
}

int addExpressionToList(ExprNode * newExpr){
    /* Add expression to expressions list */
        if(exprIndex < expressionsSize){
            expressions[exprIndex] = newExpr;
            exprIndex += 1;
        }
        else{
            expressionsSize = expressionsSize + expressionsSize;
            expressions = realloc(expressions, expressionsSize);
            expressions[exprIndex] = newExpr;
            exprIndex += 1;
        }
}

void assign(ExprNode * expr1, ExprNode * expr2){
    int expr_id1 = checkIdentifier(expr1);
    int expr_id2 = checkIdentifier(expr2);

    if(expr_id2 >= -1){
        if(expr_id1 >= 0){
            expr1 = expressions[expr_id1];
            if(expr_id2 >= 0){
                expr2 = expressions[expr_id2];
                expr1->value = expr2->value;
            }
            else{
                expr1->value = expr2->value;
            }
        }
        else if(expr_id1 == -1){
            error = 1;
            char * src =  "ERROR2 at line %d: Assigment to number is invalid\n";
            char * dest = (char *)malloc(strlen(src) + expr2->lineNum + 10);
            sprintf(dest, src, expr2->lineNum);
            if(errorIndex < errorSize){
                errors[errorIndex] = dest;
                errorIndex += 1;
            }
            else{
                errorSize = errorSize + errorSize;
                errors = realloc(errors, errorSize);
                errors[errorIndex] = dest;
                errorIndex += 1;
            }
        }
        else{
            if(expr_id2 >= 0){
                expr2 = expressions[expr_id2];
                expr1->value = expr2->value;
                addExpressionToList(expr1);
            }
            else if(expr_id2 == -1){
                expr1->value = expr2->value;
                addExpressionToList(expr1);
            }
            
        }
    }
    else{
        error = 1;
        char * src =  "ERROR2 at line %d: %s is undefined\n";
        char * dest = (char *)malloc(strlen(src) + strlen(expr2->identifier) + expr2->lineNum + 10);
        sprintf(dest, src, expr2->lineNum, expr2->identifier);
        if(errorIndex < errorSize){
            errors[errorIndex] = dest;
            errorIndex += 1;
        }
        else{
            errorSize = errorSize + errorSize;
            errors = realloc(errors, errorSize);
            errors[errorIndex] = dest;
            errorIndex += 1;
        }
    }

}

int main () 
{
    expressions = (ExprNode**)malloc(expressionsSize * sizeof(ExprNode*));
    errors = (char**)malloc(errorSize * sizeof(char*));

   if (yyparse())
   {
      // parse error
      printf("ERROR\n");
      return 1;
    } 
    else 
    {   
        if(error == 0){
            int i = 0;
            for(;i<exprIndex;i++){
                printf("Expression first defined at line %d with identifier %s with value %d\n", expressions[i]->lineNum, expressions[i]->identifier, expressions[i]->value );
            }
            // successful parsing
        }
        else{
            int i = 0;
            for(;i<errorIndex;i++){
                printf(errors[i]);
            }
        }
        //printf("OK\n");
        return 0;
    } 
}