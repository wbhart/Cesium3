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

#include <stdarg.h>
#include "parser.h"

extern ast_t * ast_nil;

combinator_t * new_combinator()
{
    combinator_t * c = GC_MALLOC(sizeof(combinator_t));

    c->fn = NULL;
    c->args = NULL;

    return c;
}

ast_t * match_fn(input_t * in, void * args)
{
    char * str = ((match_args *) args)->str;

    int start = in->start;
    int i = 0, len = strlen(str);
   
    skip_whitespace(in);
   
    while (i < len && str[i] == read1(in)) i++;
   
    if (i != len)
    {
       in->start = start;
       return NULL;
    }

    return ast_nil;
}

combinator_t * match(char * str)
{
    match_args * args = GC_MALLOC(sizeof(match_args));
    args->str = str;
    
    combinator_t * comb = new_combinator();
    comb->fn = match_fn;
    comb->args = args;

    return comb;
}

ast_t * integer_fn(input_t * in, void * args)
{
   int start = in->start;
   char c;

   ast_t * ast = new_ast();

   skip_whitespace(in);

   c = read1(in);

   if (!isdigit(c))
   {
      in->start = start;
      return NULL;
   }

   if (c == '0')
   {
      ast->typ = T_INT;
      return ast;
   }

   while (isdigit(c = read1(in))) ;
   in->start--;

   ast->typ = T_INT;
   return ast;
}

combinator_t * integer()
{
    combinator_t * comb = new_combinator();
    comb->fn = integer_fn;
    comb->args = NULL;

    return comb;
}

seq_list * new_seq()
{
    return GC_MALLOC(sizeof(seq_list));
}

ast_t * seq_fn(input_t * in, void * args)
{
    seq_args * sa = (seq_args *) args;
    seq_list * seq = sa->list;
    
    ast_t * ret = new_ast();
    ret->typ = sa->typ;

    ast_t * ptr = ret;

    while (seq != NULL)
    {
        ast_t * a = parse(in, seq->comb);
        if (a == NULL)
           return NULL;

        if (a != ast_nil)
        {
            ptr->next = a;
            ptr = ptr->next;
        }
        
        seq = seq->next;
    }

    return ret;
}

combinator_t * seq(combinator_t * ret, tag_t typ, combinator_t * c1, ...)
{
    combinator_t * comb;
    seq_list * seq;
    seq_args * args;

    va_list ap;
    va_start(ap, c1);

    seq = new_seq();
    seq->comb = c1;

    args = GC_MALLOC(sizeof(seq_args));
    args->typ = typ;
    args->list = seq;
    ret->args = (void *) args;
    ret->fn = seq_fn;

    while ((comb = va_arg(ap, combinator_t *)) != NULL)
    {
        seq->next = new_seq();
        seq = seq->next;
        seq->comb = comb;
    }

    va_end(ap);

    seq->next = NULL;

    return ret;
}

ast_t * multi_fn(input_t * in, void * args)
{
    seq_list * seq = (seq_list *) args;
    
    while (seq != NULL)
    {
        ast_t * a = parse(in, seq->comb);
        if (a != NULL)
           return a;
        
        seq = seq->next;
    }

    return NULL;
}

combinator_t * multi(combinator_t * ret, combinator_t * c1, ...)
{
    combinator_t * comb;
    seq_list * seq;
    
    va_list ap;
    va_start(ap, c1);

    seq = new_seq();
    seq->comb = c1;

    ret->args = (void *) seq;
    ret->fn = multi_fn;

    while ((comb = va_arg(ap, combinator_t *)) != NULL)
    {
        seq->next = new_seq();
        seq = seq->next;
        seq->comb = comb;
    }

    va_end(ap);

    seq->next = NULL;

    return ret;
}

ast_t * expr_fn(input_t * in, void * args)
{
   int alt;
   tag_t tag;
   op_t * op;
   expr_list * list = (expr_list *) args;

   if (list->fix == EXPR_BASE)
      return parse(in, list->comb);

   if (list->fix == EXPR_INFIX)
   {
      if (list->assoc == ASSOC_LEFT)
      {
         ast_t * lhs = expr_fn(in, (void *) list->next);
         if (!lhs)
            return NULL;
         
         while (1)
         {
            ast_t * rhs;
            
            op = list->op;
            while (op)
            {
               if (parse(in, op->comb))
                  break;
               op = op->next;
            }
            if (!op) break;

            rhs = expr_fn(in, (void *) list->next);
            if (!rhs)
               exception("Expression expected!\n");

            lhs = ast2(op->tag, lhs, rhs);
         }

         return lhs;
      }
   }
}

combinator_t * expr(combinator_t * exp, combinator_t * base)
{
   expr_list * args = GC_MALLOC(sizeof(expr_list));
   args->next = NULL;
   args->fix = EXPR_BASE;
   args->comb = base;

   exp->fn = expr_fn;
   exp->args = args;

   return exp;
}

void expr_insert(combinator_t * expr, int prec, tag_t tag, expr_fix fix, 
                 expr_assoc assoc, combinator_t * comb)
{
   expr_list * list = (expr_list *) expr->args;
   int i;

   expr_list * node = GC_MALLOC(sizeof(expr_list));
   op_t * op = GC_MALLOC(sizeof(op_t));

   op->tag = tag;
   op->comb = comb;
   node->op = op;
   node->fix = fix;
   node->assoc = assoc;
   
   if (prec == 0)
   {
      node->next = list;
      expr->args = (void *) node;
      return;
   }
   
   for (i = 0; list != NULL && i < prec - 1; i++)
      list = list->next;

   if (list->fix == EXPR_BASE || list == NULL)
      exception("Invalid precedence for expression\n");

   node->next = list->next;
   list->next = node;
}

void expr_altern(combinator_t * expr, int prec, tag_t tag, combinator_t * comb)
{
   op_t * op = GC_MALLOC(sizeof(op_t));
   expr_list * list = (expr_list *) expr->args;
   int i;

   for (i = 0; list != NULL && i < prec; i++)
      list = list->next;

   if (list->fix == EXPR_BASE || list == NULL)
      exception("Invalid precedence for expression\n");

   op->tag = tag;
   op->comb = comb;
   op->next = list->op;
   list->op = op;
}

ast_t * parse(input_t * in, combinator_t * comb)
{
    return comb->fn(in, (void *)comb->args);
}
