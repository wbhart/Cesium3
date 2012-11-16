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
   case T_THEN:
   case T_ELSE:
      s = a->child;
      while (s->next != NULL)
         s = s->next;
      return final_expression(s);
   case T_ASSIGN:
   case T_IF_STMT:
   case T_WHILE_STMT:
      return 0;
   case T_INT:
   case T_INT8:
   case T_INT16:
   case T_INT32:
   case T_INT64:
   case T_UINT:
   case T_UINT8:
   case T_UINT16:
   case T_UINT32:
   case T_UINT64:
   case T_DOUBLE:
   case T_FLOAT:
   case T_IDENT:
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
   ast_t * a1, * a2, * a3;
   int i, j;

   switch (a->tag)
   {
   case T_INT:
      a->type = t_int;
      break;
   case T_INT8:
      a->type = t_int8;
      break;
   case T_INT16:
      a->type = t_int16;
      break;
   case T_INT32:
      a->type = t_int32;
      break;
   case T_INT64:
      a->type = t_int64;
      break;
   case T_UINT:
      a->type = t_uint;
      break;
   case T_UINT8:
      a->type = t_uint8;
      break;
   case T_UINT16:
      a->type = t_uint16;
      break;
   case T_UINT32:
      a->type = t_uint32;
      break;
   case T_UINT64:
      a->type = t_uint64;
      break;
   case T_DOUBLE:
      a->type = t_double;
      break;
   case T_FLOAT:
      a->type = t_float;
      break;
   case T_IDENT:
      bind = find_symbol(a->sym);
      if (!bind)
         exception("Symbol not found in expression\n");
      a->type = bind->type;
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
   case T_BLOCK:
      scope_up();
      a->env = current_scope;
      a1 = a->child;
      while (a1->next != NULL)
      {
         inference1(a1);
         a1 = a1->next;
      }
      inference1(a1);
      a->type = a1->type;
      scope_down();
      break;
   case T_THEN:
   case T_ELSE:
   case T_DO:
      a1 = a->child;
      while (a1->next != NULL)
      {
         inference1(a1);
         a1 = a1->next;
      }
      inference1(a1);
      a->type = a1->type;
      break;
   case T_IF_ELSE_EXPR:
      a1 = a->child;
      a2 = a1->next;
      a3 = a2->next;
      inference1(a1);
      if (a1->type != t_bool)
         exception("Boolean expression expected in if..else expression\n");
      inference1(a2);
      inference1(a3);
      if (a2->type != a3->type)
         exception("Types not equal in branches of if..else expression\n");
      a->type = a2->type;
      break;
   case T_IF_ELSE_STMT:
      a1 = a->child;
      a2 = a1->next;
      a3 = a2->next;
      inference1(a1);
      if (a1->type != t_bool)
         exception("Boolean expression expected in if..else statement\n");
      inference1(a2);
      inference1(a3);
      a->type = t_nil;
      break;
   case T_IF_STMT:
      a1 = a->child;
      a2 = a1->next;
      inference1(a1);
      if (a1->type != t_bool)
         exception("Boolean expression expected in if statement\n");
      inference1(a2);
      a->type = t_nil;
      break;
   case T_WHILE_STMT:
      a1 = a->child;
      a2 = a1->next;
      inference1(a1);
      if (a1->type != t_bool)
         exception("Boolean expression expected in while statement\n");
      inference1(a2);
      a->type = t_nil;
      break;
   case T_ASSIGN:
      inference1(a->child->next);
      bind = find_symbol(a->child->sym);
      if (!bind) /* identifier doesn't exist */
      {
         a->child->type = a->child->next->type;
         bind_symbol(a->child->sym, a->child->type, NULL);
      } else
      {
         if (a->child->next->type != bind->type)
            exception("Incompatible types in assignment\n");
         a->child->type = bind->type;
      }
      a->type = t_nil;
      break;
   default:
      exception("Unknown AST tag in inference1\n");
   }
}
