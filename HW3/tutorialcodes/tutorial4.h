#ifndef __MS_H
#define __MS_H

typedef struct NumberNode
{
   char *value;
   int lineNum;
} NumberNode;

typedef struct IdentNode
{
    char *value;
    int lineNum;
} IdentNode;

typedef struct ExprNode
{
    char *identifier;
    int value;
    int lineNum;
} ExprNode;

#endif