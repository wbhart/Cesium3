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

infer_t * infer_stack;
infer_t * deduce_stack;
infer_t * generic_stack;

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
       i = ast_count(a1);
       if (i != b->arity)
          exception("Incorrect number of entries in tuple assignment\n");
       for (j = 0; j < i; j++)
       {
          assign_inference1(a1, b->args[j]);
          a1 = a1->next;
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
   case UINT:
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
   Check if a list has types for each node or whether it
   includes some typevars
*/
int list_typed(ast_t * a)
{
   int typed = 1;

   while (a != NULL)
   {
      if (a->child->next->tag == T_TYPE_VAR)
      {
         typed = 0;
         break;
      }
      a = a->next;
   }

   return typed;
}

/* return 1 if the two types will unify, otherwise 0 */
int types_unify(type_t * t1, type_t * t2)
{
   int i;
   
   if (t1 == t2)
      return 1;

   if (t1->typ == TYPEVAR || t2->typ == TYPEVAR)
      return 1;

   if (t1->typ == FN || t2->typ == FN || t1->typ == TUPLE || t2->typ == TUPLE)
   {      
      /* make sure both are functions */
      if (t1->typ != t2->typ)
         return 0;

      /* make sure both have same arity */
      if (t1->arity != t2->arity)
         return 0;

      /* make sure return types unify */
      if (t1->typ == FN && !types_unify(t1->ret, t2->ret))
         return 0;

      for (i = 0; i < t1->arity; i++)
         if (!types_unify(t1->args[i], t2->args[i]))
            return 0;

      return 1;
   }

   return 0;
}

/*
   Find prototype in generic type function list
*/
type_t * find_prototype(type_t * t, ast_t * a)
{
   int i, j;
   int c = ast_count(a);
   ast_t * a2;

   if (t->typ == FN)
   {
      a2 = a;

      if (t->arity == c)
      {
         for (j = 0; j < c; j++)
         {
            if (t->args[j] != a2->type)
               break;
            a2 = a2->next;
         }

         if (j == c)
         return t;
      }
   } else /* generic or type constructor */
   {
      for (i = 0; i < t->arity; i++)
      {
         type_t * fn = t->args[i];
         a2 = a;

         if (fn->arity == c)
         {
            for (j = 0; j < c; j++)
            {
               if (fn->args[j] != a2->type)
                  break;
               a2 = a2->next;
            }

            if (j == c)
               return fn;
         }
      }
   }
   
   return NULL; /* didn't find an op with that prototype */
}

/* 
   return 1 if any of the function prototypes will unify with
   the types of the given ast list
*/
int exists_prototype(type_t * t, ast_t * a)
{
   int i, j, c = ast_count(a);
   ast_t * a2;

   if (t->typ == FN)
   {
      if (t->arity != c)
         return 0;

      a2 = a;

      for (j = 0; j < c; j++)
      {
         if (!types_unify(t->args[j], a2->type))
               return 0;
         a2 = a2->next;
      }

      return 1;
   } else /* generic or type constructor */
   {
      for (i = 0; i < t->arity; i++)
      {
         type_t * fn = t->args[i];
         a2 = a;

         if (fn->arity == c)
         {
            for (j = 0; j < c; j++)
            {
               if (!types_unify(fn->args[j], a2->type))
                  break;
               a2 = a2->next;
            }

            if (j == c)
               return 1;
         }
      }

      return 0;
   }
}

/* 
   Initialise the type inference and deduction
   stacks 
*/
void infer_stack_init(void)
{
    infer_stack = NULL;
    deduce_stack = NULL;
    generic_stack = NULL;
}

void infer_print(infer_t * st)
{
   while (st != NULL)
   {
      type_print(st->t1);
      printf(" : ");
      type_print(st->t2);
      printf("\n");
      st = st->next;
   }
}

/* Push a function inference relation onto the stack */
void push_generic(type_t * t1, type_t * t2)
{
   infer_t * t = (infer_t *) GC_MALLOC(sizeof(infer_t));
   t->t1 = t1;
   t->t2 = t2;
   t->next = generic_stack;
   generic_stack = t;
}

/* Push an inference relation onto the stack */
void push_inference(type_t * t1, type_t * t2)
{
   infer_t * t = (infer_t *) GC_MALLOC(sizeof(infer_t));
   t->t1 = t1;
   t->t2 = t2;
   t->next = infer_stack;
   infer_stack = t;
}

/* Push an deduction onto the deduction stack */
void push_deduction(infer_t * inf)
{
   inf->next = deduce_stack;
   deduce_stack = inf;
}

/* Pop an inference relation off the stack */
infer_t * pop_inference(void)
{
   if (infer_stack == NULL)
      exception("Attempt to pop empty inference stack!\n");

   infer_t * t = infer_stack;
   infer_stack = t->next;
   t->next = NULL;
   return t;
}

/* 
   Substitute a type according to an inference
   relation. We take a pointer to the type so
   that it can be replaced
*/
void infer_subst_type(type_t ** tin, infer_t * rel)
{
    type_t * t = *tin;
    int i;

    if (t == NULL)
        return;
    
    if (t->typ == TYPEVAR)
    {
        if (*tin == rel->t1)
            *tin = rel->t2;
    }
    else if (t->typ == FN)
    {
        for (i = 0; i < t->arity; i++)
            infer_subst_type(t->args + i, rel);
        if (t->ret != NULL)
            infer_subst_type(&(t->ret), rel);
    } else if (t->typ == TUPLE || t->typ == DATATYPE)
    {
        for (i = 0; i < t->arity; i++)
            infer_subst_type(t->args + i, rel);
    }
}

/* 
   Substitute types according to supplied inference
   relation on the inference stack and deduction stack
   according to which flags are set
*/
void infer_subst(infer_t * inf)
{
    infer_t * rel;
    
    rel = infer_stack;
       
    while (rel != NULL)
    {
       infer_subst_type(&(rel->t1), inf);
       infer_subst_type(&(rel->t2), inf);
       rel = rel->next;
    }

    rel = deduce_stack;
       
    while (rel != NULL)
    {
       infer_subst_type(&(rel->t2), inf);
       rel = rel->next;
    }

    rel = generic_stack;
       
    while (rel != NULL)
    {
       infer_subst_type(&(rel->t1), inf);
       rel = rel->next;
    }
}

/* 
   Substitute a type for its deduction. We pass a
   pointer to the type so it can be replaced
*/
void substitute_type(type_t ** tin)
{
    infer_t * rel = deduce_stack;
   
    while (rel != NULL)
    {
        infer_subst_type(tin, rel);
        rel = rel->next;
    }
}

/*
   Substitute types in an ast list
*/
void substitute_type_list(ast_t * a)
{
   while (a != NULL)
   {
      substitute_type(&a->type);
      a = a->next;
   }
}

/* 
   The heart of the Hindley-Milner type inference,
   the unification algorithm. Processes the inference
   stack and makes deductions, placing them on a
   deduction stack.
*/
void unify()
{
   int i, j;

   while (infer_stack != NULL)
   {
      while (infer_stack != NULL)
      {
         infer_t * rel = pop_inference();

         if (rel->t1 == rel->t2)
            continue;
         else if (rel->t1->typ == TYPEVAR)
         {
            infer_subst(rel);
            push_deduction(rel);
         } else if (rel->t2->typ == TYPEVAR)
            push_inference(rel->t2, rel->t1);
         else if (rel->t1->typ == FN)
         {
            if (rel->t2->typ != FN || rel->t1->arity != rel->t2->arity)
               exception("Type mismatch: function type not matched!\n");
            push_inference(rel->t1->ret, rel->t2->ret);
            for (i = 0; i < rel->t1->arity; i++)
               push_inference(rel->t1->args[i], rel->t2->args[i]);
         } else if (rel->t1->typ == TUPLE)
         {
            if (rel->t2->typ != TUPLE || rel->t1->arity != rel->t2->arity)
               exception("Type mismatch: tuple type not matched!\n");
            for (i = 0; i < rel->t1->arity; i++)
               push_inference(rel->t1->args[i], rel->t2->args[i]);
         } else if (rel->t1->typ != rel->t2->typ)
         {
            printf("%d %d\n", rel->t1->typ, rel->t2->typ);
            exception("Type mismatch!\n");
         }
      }

      { /* unify generics */
         infer_t * rel = generic_stack;
         infer_t * rel_old = NULL;
         type_t * t = NULL;
         
         while (rel != NULL)
         {
            for (i = 0; i < rel->t2->arity; i++)
            {
               if (types_unify(rel->t1, rel->t2->args[i]))
               {
                  if (t != NULL) /* more than one possibility */
                  {
                     t = NULL;
                     break;
                  } else
                     t = rel->t2->args[i];
               }
            }

            if (i == rel->t2->arity && t == NULL) /* no possibilities */
               exception("Unable to match generic function\n");

            if (t != NULL) /* we have found a single matching function type */
            {
               push_inference(rel->t1, t);
               if (rel_old != NULL) /* remove relation */
                  rel_old->next = rel->next;
               else
                  generic_stack = rel->next;

               t = NULL;
            }

            rel_old = rel;
            rel = rel->next;
         }
      }
   }
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
   case T_NONE:
      a->type = t_nil;
      break;
   case T_INT:
      a->type = t_int;
      break;
   case T_UINT:
      a->type = t_uint;
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
      a1 = a->child;
      list_inference1(a1);
      bind = find_symbol(a->sym);
      if (!exists_prototype(bind->type, a1)) /* check if any op has that prototype */
         exception("Operator not found in inference1\n");
      if ((t1 = find_prototype(bind->type, a1))) /* find exactly one op with that prototype */
         a->type = t1->ret;
      else /* prototype must be inferred */
      {
         a->type = new_typevar();
         i = 2;
         args = GC_MALLOC(i*sizeof(type_t *));
         f1 = fn_type(a->type, i, args);
         assign_args(f1->args, a1);
         push_generic(f1, bind->type);
      }
      break;
   case T_BLOCK:
      a->env = scope_up();
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
      if (!types_unify(a1->type, t_bool))
         exception("Boolean expression expected in if..else expression\n");
      if (a1->type->typ == TYPEVAR)
         push_inference(a1->type, t_bool);
      inference1(a2);
      inference1(a3);
      if (!types_unify(a2->type, a3->type))
         exception("Types not equal in branches of if..else expression\n");
      if (a2->type->typ == TYPEVAR)
         push_inference(a2->type, a3->type);
      else if (a3->type->typ == TYPEVAR)
         push_inference(a3->type, a2->type);
      a->type = a2->type;
      break;
   case T_IF_ELSE_STMT:
      a1 = a->child;
      a2 = a1->next;
      a3 = a2->next;
      inference1(a1);
      if (!types_unify(a1->type, t_bool))
         exception("Boolean expression expected in if..else statement\n");
      if (a1->type->typ == TYPEVAR)
         push_inference(a1->type, t_bool);
      inference1(a2);
      inference1(a3);
      a->type = t_nil;
      break;
   case T_IF_STMT:
      a1 = a->child;
      a2 = a1->next;
      inference1(a1);
      if (!types_unify(a1->type, t_bool))
         exception("Boolean expression expected in if statement\n");
      if (a1->type->typ == TYPEVAR)
         push_inference(a1->type, t_bool);
      inference1(a2);
      a->type = t_nil;
      break;
   case T_WHILE_STMT:
      a1 = a->child;
      a2 = a1->next;
      inference1(a1);
      if (!types_unify(a1->type, t_bool))
         exception("Boolean expression expected in while statement\n");
      if (a1->type->typ == TYPEVAR)
         push_inference(a1->type, t_bool);
      inference1(a2);
      a->type = t_nil;
      break;
   case T_BREAK:
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
      bind_symbol(a1->sym, t2, NULL);
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
      if (t1->typ != GENERIC && t1->typ != TYPECONSTR && t1->typ != FN)
         exception("Invalid function or type constructor in application\n");
      if (!exists_prototype(t1, a2))
         exception("Incorrect signature in application\n");
      if ((t2 = find_prototype(t1, a2)))
      {
         a->type = t2->ret;
         for (i = 0; i < a->type->arity; i++)
            a->type->args[i] = resolve_inference1(a->type->args[i]);
      } else
      {
         a->type = new_typevar();
         i = ast_count(a2);
         args = GC_MALLOC(i*sizeof(type_t *));
         f1 = fn_type(a->type, i, args);
         assign_args(f1->args, a2);
         if (t1->typ == GENERIC)
            push_generic(f1, t1);
         else
            push_inference(f1, t1);
      }
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
   case T_PARAM_BODY:
      list_inference1(a->child);
      break;
   case T_PARAM:
      a1 = a->child;
      a2 = a1->next;
      if (a2->tag == T_TYPE_VAR) a2->type = new_typevar();
      else inference1(a2);
      bind_symbol(a1->sym, a2->type, NULL);
      a->type = a2->type;
      break;
   case T_FN_PROTO:
      a1 = a->child; /* identifier */
      a2 = a1->next; /* param list */
      a3 = a2->next; /* return type */
      a->env = scope_up();
      inference1(a2);
      inference1(a3);
      bind_symbol(sym_lookup("return"), a3->type, NULL);
      i = ast_count(a2->child);
      args = GC_MALLOC(i*sizeof(type_t *));
      f1 = fn_type(a3->type, i, args);
      assign_args(f1->args, a2->child);
      f1->ast = a; /* store ast for jit'ing */
      scope_down();
      bind = find_symbol(a1->sym);
      if (list_typed(a2->child)) /* check if params all have types */
      {
         if (bind != NULL && bind->type->typ == GENERIC) /* insert fn into existing generic */
            generic_insert(bind->type, f1);
         else /* new generic */
         {
            fns = GC_MALLOC(sizeof(type_t *));
            fns[0] = f1;
            bind_generic(a1->sym, generic_type(1, fns));
         }  
      } else /* have a type inferred function */
         bind_symbol(a1->sym, f1, NULL);
      a->type = t_nil;
      break;
   case T_FN_STMT:
      a1 = a->child->next->next->next;
      current_scope = a->env;
      inference1(a1);
      scope_down();
      break;
   case T_RETURN:
      inference1(a->child);
      bind = find_symbol(sym_lookup("return"));
      if (bind == NULL)
         exception("Return outside of a function");
      if (!types_unify(bind->type, a->child->type))
         exception("Return type does not match prototype in function definition");
      else if (bind->type->typ == TYPEVAR)
         push_inference(bind->type, a->child->type);
      else if (a->child->type->typ == TYPEVAR)
         push_inference(a->child->type, bind->type);
      a->type = t_nil;
      break;
   default:
      exception("Unknown AST tag in inference1\n");
   }
}
