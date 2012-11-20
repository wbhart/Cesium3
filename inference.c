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
   case T_TUPLE_ASSIGN:
   case T_SLOT_ASSIGN:
   case T_IF_STMT:
   case T_WHILE_STMT:
   case T_TYPE_STMT:
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
   case T_TUPLE:
   case T_CHAR:
   case T_STRING:
   case T_APPL:
   case T_SLOT:
      return 1;
   default:
      exception("Unknown AST tag in final_expression\n");
   }
}

void assign_inference1(ast_t * a, ast_t * b)
{
    int i, j;
    bind_t * bind;
   
    if (a->tag == T_IDENT)
    {
       bind = find_symbol(a->sym);
       if (!bind) /* identifier doesn't exist */
       {
          a->type = b->type;
          bind_symbol(a->sym, a->type, NULL);
       } else
       {
          if (b->type != bind->type)
             exception("Incompatible types in assignment\n");
          a->type = bind->type;
       }
    } else if (a->tag == T_LSLOT)
    {
       inference1(a);
       if (a->type != b->type)
          exception("Incompatible types in assignment\n");
    } else if (a->tag == T_TUPLE)
    {
       if (b->tag != T_TUPLE)
           exception("Attempt to assign non-tuple to tuple\n");
       ast_t * a1 = a->child;
       ast_t * b1 = b->child;
       i = j = 0;
       while (a1 != NULL)
       {
          a1 = a1->next;
          i++;
       }
       while (b1 != NULL)
       {
          b1 = b1->next;
          j++;
       }
       if (i != j)
          exception("Incorrect number of entries in tuple assignment\n");
       a1 = a->child;
       b1 = b->child;
       while (a1 != NULL)
       {
          assign_inference1(a1, b1);
          a1 = a1->next;
          b1 = b1->next;
       }
       a->type = b->type;
    } else
       exception("Invalid L-value in assignment\n");
}

type_t * resolve_inference1(type_t * t)
{
   int i;
   bind_t * bind;
   type_t * t1;

   switch (t->typ)
   {
   case RESOLVE:
      bind = find_symbol(t->sym);
      if (!bind)
         exception("Unable to resolve type in resolve_inference1\n");
      t1 = bind->type;
      if (t1->typ == TYPECONSTR)
         return t1->ret;
      else
         return t1;
   case NIL:
   case BOOL:
   case INT:
   case INT8:
   case INT16:
   case INT32:
   case INT64:
   case UINT:
   case UINT8:
   case UINT16:
   case UINT32:
   case UINT64:
   case DOUBLE:
   case FLOAT:
   case STRING:
   case CHAR:
   case DATATYPE:
   case TUPLE:
      return t;
   default:
      exception("Unknown type in resolve_inference1\n");
   }
}

void inference1(ast_t * a)
{
   bind_t * bind;
   type_t * t1, * t2, * f1;
   type_t ** args, ** fns;
   sym_t ** slots;
   ast_t * a1, * a2, * a3, * a4;
   int i, j, k;

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
   case T_CHAR:
      a->type = t_char;
      break;
   case T_STRING:
      a->type = t_string;
      break;
   case T_IDENT:
      bind = find_symbol(a->sym);
      if (!bind)
         exception("Symbol not found in expression\n");
      a->type = bind->type;
      break;
   case T_TUPLE:
      a1 = a->child;
      i = 0;
      while (a1 != NULL)
      {
         inference1(a1);
         a1 = a1->next;
         i++;
      }
      args = GC_MALLOC(i*sizeof(type_t *));
      i = 0;
      a1 = a->child;
      while (a1 != NULL)
      {
         args[i] = a1->type;
         a1 = a1->next;
         i++;
      }
      a->type = tuple_type(i, args);
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
   case T_TYPENAME:
      bind = find_symbol(a->sym);
      if (!bind)
         a->type = new_type(a->sym->name, RESOLVE);
      else if (bind->type->typ == TYPECONSTR)
         a->type = bind->type->ret;
      else
         a->type = bind->type;
      break;
   case T_TYPE_SLOT:
      a1 = a->child;
      a2 = a1->next;
      inference1(a2);
      a->type = a2->type;
      break;
   case T_TYPE_BODY:
      a1 = a->child;
      while (a1 != NULL)
      {
         inference1(a1);
         a1 = a1->next;
      }
      break;
   case T_TYPE_STMT:
      a1 = a->child;
      a2 = a1->next->child;
      i = 0;
      while (a2 != NULL)
      {
         a2 = a2->next;
         i++;
      }
      args = GC_MALLOC(i*sizeof(type_t *));
      slots = GC_MALLOC(i*sizeof(sym_t *));
      t1 = data_type(i, args, a1->sym, slots, 0, NULL);
      f1 = fn_type(t1, i, args);
      fns = GC_MALLOC(sizeof(type_t *));
      fns[0] = f1;
      t2 = typeconstr_type(a1->sym, t1, 1, fns);
      bind = bind_symbol(a1->sym, t2, NULL);
      inference1(a1->next);
      i = 0;
      a2 = a1->next->child;
      while (a2 != NULL)
      {
         f1->args[i] = t1->args[i] = a2->type;
         t1->slots[i] = a2->child->sym;
         a2 = a2->next;
         i++;
      }
      a->type = t_nil;
      break;
   case T_ASSIGN:
   case T_TUPLE_ASSIGN:
      a1 = a->child;
      a2 = a1->next;
      inference1(a2);
      assign_inference1(a1, a2);
      a->type = t_nil;
      break;
   case T_SLOT_ASSIGN:
      a1 = a->child;
      a2 = a1->next;
      inference1(a2);
      inference1(a1);
      a->type = t_nil;
      break;
   case T_TUPLE_TYPE:
      a1 = a->child;
      i = 0;
      while (a1 != NULL)
      {
         inference1(a1);
         a1 = a1->next;
         i++;
      }
      args = GC_MALLOC(i*sizeof(type_t *));
      i = 0;
      a1 = a->child;
      while (a1 != NULL)
      {
         args[i] = a1->type;
         a1 = a1->next;
         i++;
      }
      a->type = tuple_type(i, args);
      break;
   case T_APPL:
      a1 = a->child;
      a2 = a1->next;
      i = 0;
      while (a2 != NULL)
      {
         inference1(a2);
         a2 = a2->next;
         i++;
      }
      inference1(a1);
      t1 = a1->type;
      if (t1->typ != GENERIC && t1->typ != TYPECONSTR)
         exception("Invalid function or type constructor in application\n");
      for (j = 0; j < t1->arity; j++)
      {
         t2 = t1->args[j];
         if (t2->arity == i)
         {
            a2 = a1->next;
            for (k = 0; k < i; k++)
            {
               if (t2->args[i] != a2->type)
                  break;
               a2 = a2->next;
            }
            if (k == i)
               break;
         }
      }
      if (j != t1->arity)
         exception("Incorrect signature in application\n");
      t1 = t1->ret;
      for (i = 0; i < t1->arity; i++)
         t1->args[i] = resolve_inference1(t1->args[i]);
      a->type = t1;
      break;
   case T_LSLOT:
   case T_SLOT:
      a1 = a->child;
      a2 = a1->next;
      inference1(a1);
      t1 = a1->type;
      if (t1->typ != DATATYPE)
         exception("Datatype not known in slot evaluation\n");
      for (i = 0; i < t1->arity; i++)
         if (t1->slots[i] == a2->sym)
            break;
      if (i == t1->arity)
         exception("Slot not found in slot evaluation\n");
      a->type = t1->args[i];
      break;
   default:
      exception("Unknown AST tag in inference1\n");
   }
}
