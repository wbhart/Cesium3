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

#include "inference.h"

int final_expression(ast_t * a)
{
   ast_t * s;
   
   switch (a->tag)
   {
   case T_IF_ELSE_STMT:
      if (final_expression(a->child->next)
       && final_expression(a->child->next->next))
      {
         a->tag = T_IF_ELSE_EXPR;
         return 1;
      } else
         return 0;
   case T_BLOCK:
      s = a->child;
      while (s->next != NULL)
         s = s->next;
      if (final_expression(s))
      {
         a->tag = T_IF_ELSE_EXPR;
         return 1;
      } else
         return 0;
   case T_INT:
   case T_BINOP:
      return 1;
   default:
      exception("Unknown AST tag in final_expression\n");
   }
}

void inference1(ast_t * a)
{
   bind_t * bind;
   type_t * t1, * t2;
   type_t ** args;
   int i, j;

   switch (a->tag)
   {
   case T_INT:
      a->type = t_int;
      break;
   case T_BINOP:
      inference1(a->child);
      inference1(a->child->next);
      bind = find_symbol(a->sym);
      t1 = bind->type;
      for (i = 0; i < t1->arity; i++)
      {
         t2 = t1->args[i];
         if (t2->args[0] == a->child->type
          && t2->args[1] == a->child->next->type)
         {
            a->type = t2->ret;
            break;
         }
      }
      if (i == t1->arity) /* didn't find an op with that prototype */
      {
         printf("Operator %s(", a->sym->name), type_print(a->child->type),
            printf(", "), type_print(a->child->next->type);
         exception(") not found in inference1\n");
      }
      break;
   default:
      exception("Unknown AST tag in inference1\n");
   }
}
