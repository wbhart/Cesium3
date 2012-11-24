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

/*
   Determine whether the ast node corresponds to an expression 
   or statement
   Switch if statements to expressions if necessary
*/
int ast_expression(ast_t * a)
{
   ast_t * s;
   
   switch (a->tag)
   {
   case T_IF_ELSE_STMT:
      if (ast_expression(a->child->next)
       && ast_expression(a->child->next->next))
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
      return ast_expression(s);
   case T_ASSIGN:
   case T_TUPLE_ASSIGN:
   case T_SLOT_ASSIGN:
   case T_IF_STMT:
   case T_WHILE_STMT:
   case T_TYPE_STMT:
   case T_TUPLE_UNPACK:
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
      exception("Unknown AST tag in ast_expression\n");
   }
}

/*
   Annotate AST node for the left hand side of an assignment
   given the type being assigned on the RHS
*/
void assign_inference1(ast_t * a, type_t * b)
{
    int i, j;
    bind_t * bind;
   
    if (a->tag == T_IDENT)
    {
       bind = find_symbol(a->sym);
       if (!bind) /* identifier doesn't exist */
       {
          a->type = b;
          bind_symbol(a->sym, a->type, NULL);
       } else
       {
          if (b != bind->type)
             exception("Incompatible types in assignment\n");
          a->type = bind->type;
       }
    } else if (a->tag == T_LSLOT)
    {
       inference1(a);
       if (a->type != b)
          exception("Incompatible types in assignment\n");
    } else if (a->tag == T_TUPLE)
    {
       if (b->typ != TUPLE)
             exception("Attempt to assign non-tuple to tuple\n");
       ast_t * a1 = a->child;
       i = 0;
       while (a1 != NULL)
       {
          a1 = a1->next;
          i++;
       }
       if (i != b->arity)
          exception("Incorrect number of entries in tuple assignment\n");
       a1 = a->child;
       i = 0;
       while (a1 != NULL)
       {
          assign_inference1(a1, b->args[i]);
          a1 = a1->next;
          i++;
       }
       a->type = b;
    } else
       exception("Invalid L-value in assignment\n");
}

/*
   Resolve unresolved type
*/
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

/* 
   count the nodes in an AST list
*/
int ast_count(ast_t * a)
{
   int i = 0;

   while (a != NULL)
   {
      a = a->next;
      i++;
   }

   return i;   
}

/* 
   do inference1 for each node of an AST list
   and return the type of the final node
*/
type_t * list_inference1(ast_t * a)
{
   int i = 0;

   if (a == NULL)
      return t_nil;

   while (a->next != NULL)
   {
      inference1(a);
      a = a->next;
      i++;
   }
   inference1(a);
      
   return a->type;
}

/*
   Assign args array to types of AST nodes in list
*/
int assign_args(type_t ** args, ast_t * a)
{
   int i = 0;

   while (a != NULL)
   {
      args[i] = a->type;
      a = a->next;
      i++;
   }

   return i;
}

/*
   Assign args array to syms of children of AST nodes in list
*/
int assign_syms(sym_t ** args, ast_t * a)
{
   int i = 0;

   while (a != NULL)
   {
      args[i] = a->child->sym;
      a = a->next;
      i++;
   }

   return i;
}

/*
   Find prototype in generic type function list
*/
type_t * find_prototype(type_t * gen, ast_t * a)
{
   int i, j;

   for (i = 0; i < gen->arity; i++)
   {
      type_t * fn = gen->args[i];
      ast_t * a2 = a;

      int c = ast_count(a);

      if (fn->arity == c)
      {
         for (j = 0; j < c; j++)
         {
            if (fn->args[j] != a2->type)
               break;
            a2 = a2->next;
         }

         if (j == c)
            return fn->ret;
      }
   }
   
   return NULL; /* didn't find an op with that prototype */
}
     
/*
   Find the index of a slot in a data type by symbol name
*/
int find_slot(type_t * t, sym_t * sym)
{
   int i;
   
   for (i = 0; i < t->arity; i++)
      if (t->slots[i] == sym)
         break;

   return i;
}
  
/*
   Annotate an AST with known types
*/
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
      list_inference1(a1);
      i = ast_count(a1);
      args = GC_MALLOC(i*sizeof(type_t *));
      assign_args(args, a1);
      a->type = tuple_type(i, args);
      break;
   case T_BINOP:
      inference1(a->child);
      inference1(a->child->next);
      bind = find_symbol(a->sym);
      a->type = find_prototype(bind->type, a->child);
      if (!a->type) /* didn't find an op with that prototype */
      {
         printf("Operator %s(", a->sym->name), type_print(a->child->type),
            printf(", "), type_print(a->child->next->type);
         exception(") not found in inference1\n");
      }
      break;
   case T_BLOCK:
      scope_up();
      a->env = current_scope;
      a->type = list_inference1(a->child);
      scope_down();
      break;
   case T_THEN:
   case T_ELSE:
   case T_DO:
      a->type = list_inference1(a->child);
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
      list_inference1(a->child);
      break;
   case T_TYPE_STMT:
      a1 = a->child;
      a2 = a1->next->child;
      i = ast_count(a2);
      args = GC_MALLOC(i*sizeof(type_t *));
      slots = GC_MALLOC(i*sizeof(sym_t *));
      t1 = data_type(i, args, a1->sym, slots, 0, NULL);
      f1 = fn_type(t1, i, args);
      fns = GC_MALLOC(sizeof(type_t *));
      fns[0] = f1;
      t2 = typeconstr_type(a1->sym, t1, 1, fns);
      bind = bind_symbol(a1->sym, t2, NULL);
      inference1(a1->next);
      assign_args(f1->args, a2);
      assign_args(t1->args, a2);
      assign_syms(t1->slots, a2);
      a->type = t_nil;
      break;
   case T_ASSIGN:
   case T_TUPLE_ASSIGN:
   case T_TUPLE_UNPACK:
      a1 = a->child;
      a2 = a1->next;
      inference1(a2);
      assign_inference1(a1, a2->type);
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
      list_inference1(a1);
      i = ast_count(a1);
      args = GC_MALLOC(i*sizeof(type_t *));
      assign_args(args, a->child);
      a->type = tuple_type(i, args);
      break;
   case T_APPL:
      a1 = a->child;
      a2 = a1->next;
      list_inference1(a2);
      inference1(a1);
      t1 = a1->type;
      if (t1->typ != GENERIC && t1->typ != TYPECONSTR)
         exception("Invalid function or type constructor in application\n");
      a->type = find_prototype(t1, a2);
      if (!a->type)
         exception("Incorrect signature in application\n");
      for (i = 0; i < a->type->arity; i++)
         a->type->args[i] = resolve_inference1(a->type->args[i]);
      break;
   case T_LSLOT:
   case T_SLOT:
      a1 = a->child;
      a2 = a1->next;
      inference1(a1);
      t1 = a1->type;
      if (t1->typ != DATATYPE)
         exception("Unknown data type in dot operator\n");
      i = find_slot(t1, a2->sym);
      if (i == t1->arity)
         exception("Slot not found in slot evaluation\n");
      a->type = t1->args[i];
      break;
   default:
      exception("Unknown AST tag in inference1\n");
   }
}
