/*

Copyright 2012 William Hart. All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are
permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of
      conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice, this list
      of conditions and the following disclaimer in the documentation and/or other materials
      provided with the distribution.

THIS SOFTWARE IS PROVIDED BY William Hart ``AS IS'' AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL William Hart OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

#include "ast.h"

ast_t * root;

ast_t * ast_nil;

ast_t * new_ast()
{
   ast_t * ast = GC_MALLOC(sizeof(ast_t));
      
   return ast;
}

void ast_init()
{
    ast_nil = new_ast();
    ast_nil->tag = T_NONE;
}

ast_t * ast0(tag_t tag)
{
   ast_t * ast = new_ast();
   ast->tag = tag;
   return ast;
}

ast_t * ast1(tag_t tag, ast_t * a1)
{
   ast_t * ast = new_ast();
   ast->tag = tag;
   ast->child = a1;
   return ast;
}

ast_t * ast2(tag_t tag, ast_t * a1, ast_t * a2)
{
   ast_t * ast = new_ast();
   ast->tag = tag;
   ast->child = a1;
   a1->next = a2;
   return ast;
}

ast_t * ast3(tag_t tag, ast_t * a1, ast_t * a2, ast_t * a3)
{
   ast_t * ast = new_ast();
   ast->tag = tag;
   ast->child = a1;
   a1->next = a2;
   a2->next = a3;
   return ast;
}

ast_t * ast4(tag_t tag, ast_t * a1, ast_t * a2, ast_t * a3, ast_t * a4)
{
   ast_t * ast = new_ast();
   ast->tag = tag;
   ast->child = a1;
   a1->next = a2;
   a2->next = a3;
   a3->next = a4;
   return ast;
}

ast_t * ast_binop(sym_t * sym, ast_t * a1, ast_t * a2)
{
   ast_t * ast = new_ast();
   ast->tag = T_BINOP;
   ast->child = a1;
   ast->child->next = a2;
   ast->sym = sym;
   return ast;
}

ast_t * ast_symbol(tag_t tag, sym_t * sym)
{
   ast_t * ast = new_ast();
   ast->tag = tag;
   ast->sym = sym;
}

void ast_print(ast_t * ast, int indent, int types)
{
   int i;
   ast_t * a;
   
   for (i = 0; i < indent; i++)
      printf(" ");
   printf("~ ");

   switch (ast->tag)
   {
      case T_NONE:
         printf("none\n");
         break;
      case T_INT:
      case T_UINT:
      case T_DOUBLE:
      case T_FLOAT:
      case T_CHAR:
      case T_STRING:
         printf("%s", ast->sym->name);
         if (types) printf(" : "), type_print(ast->type);
         printf("\n");
         break;
      case T_TUPLE:
         printf("tuple");
         if (types) printf(" : "), type_print(ast->type);
         printf("\n");
         break;
      case T_BINOP:
         printf("%s", ast->sym->name);
         if (types) printf(" : "), type_print(ast->type);
         printf("\n");
         ast_print(ast->child, indent + 3, types);
         ast_print(ast->child->next, indent + 3, types);
         break;
      case T_IDENT:
         printf("%s", ast->sym->name);
         if (types) printf(" : "), type_print(ast->type);
         printf("\n");
         break;
      case T_IF_ELSE_EXPR:
      case T_IF_ELSE_STMT:
         printf("if");
         if (types) printf(" : "), type_print(ast->type);
         printf("\n");
         ast_print(ast->child, indent + 3, types);
         ast_print(ast->child->next, indent, types);
         ast_print(ast->child->next->next, indent, types);
         break;
      case T_IF_STMT:
         printf("if");
         if (types) printf(" : "), type_print(ast->type);
         printf("\n");
         ast_print(ast->child, indent + 3, types);
         ast_print(ast->child->next, indent, types);
         break;
      case T_WHILE_STMT:
         printf("while");
         if (types) printf(" : "), type_print(ast->type);
         printf("\n");
         ast_print(ast->child, indent + 3, types);
         ast_print(ast->child->next, indent, types);
         break;
      case T_BREAK:
         printf("break\n");
         break;
      case T_BLOCK:
         printf("block\n");
         a = ast->child;
         while (a != NULL)
         {
            ast_print(a, indent + 3, types);
            a = a->next;
         }
         break;
      case T_THEN:
         printf("then\n");
         a = ast->child;
         while (a != NULL)
         {
            ast_print(a, indent + 3, types);
            a = a->next;
         }
         break;
      case T_ELSE:
         printf("else\n");
         a = ast->child;
         while (a != NULL)
         {
            ast_print(a, indent + 3, types);
            a = a->next;
         }
         break;
      case T_DO:
         printf("do\n");
         a = ast->child;
         while (a != NULL)
         {
            ast_print(a, indent + 3, types);
            a = a->next;
         }
         break;
      case T_TYPE_SLOT:
         printf("%s", ast->child->sym->name);
         if (types) printf(" : "), type_print(ast->type);
         printf("\n");
         break;
      case T_TYPENAME:
         if (types) type_print(ast->type);
         printf("\n");
         break;
      case T_TYPE_STMT:
         printf("type %s\n", ast->child->sym->name);
         a = ast->child->next->child;
         while (a != NULL)
         {
            ast_print(a, indent + 3, types);
            a = a->next;
         }
         break;
      case T_ASSIGN:
      case T_TUPLE_ASSIGN:
      case T_TUPLE_UNPACK:
         printf("assign");
         if (types) printf(" : "), type_print(ast->type);
         printf("\n");
         ast_print(ast->child, indent + 3, types);
         ast_print(ast->child->next, indent + 3, types);
         break;
      case T_APPL:
         printf("appl");
         if (types) printf(" : "), type_print(ast->type);
         printf("\n");
         a = ast->child->next;
         while (a != NULL)
         {
            ast_print(a, indent + 3, types);
            a = a->next;
         }
         break;
      case T_SLOT_NAME:
         printf(".%s\n", ast->sym->name);
         break;
      case T_SLOT:
      case T_LSLOT:
         printf("slot");
         if (types) printf(" : "), type_print(ast->type);
         printf("\n");
         ast_print(ast->child, indent + 3, types);
         ast_print(ast->child->next, indent + 3, types);
         break;
      case T_SLOT_ASSIGN:
         printf("assign");
         if (types) printf(" : "), type_print(ast->type);
         printf("\n");
         ast_print(ast->child, indent + 3, types);
         ast_print(ast->child->next, indent + 3, types);
         break;
      case T_PARAM:
         printf("%s", ast->child->sym->name);
         if (types) printf(" : "), type_print(ast->type);
         printf("\n");
         break;
      case T_FN_PROTO:
         printf("fn %s\n", ast->child->sym->name);
         a = ast->child->next->child;
         while (a != NULL)
         {
            ast_print(a, indent + 3, types);
            a = a->next;
         }
         a = ast->child->next->next;
         ast_print(a, indent + 3, types);
         break;
      case T_RETURN:
         printf("return\n");
         ast_print(ast->child, indent + 3, types);
         break;
      default:
         printf("%d\n", ast->tag);
         exception("invalid AST tag in ast_print\n");
   }
}