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
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, O `R TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

#include "eval.h"

vals_t * values;

void value_store(sym_t * sym, long val)
{
   vals_t * value;
   vals_t * ptr = values;

   while (ptr != NULL)
   {
      if (ptr->sym == sym)
      {
         ptr->val = val;
         return;
      }

      ptr = ptr->next;
   }
   
   value = GC_MALLOC(sizeof(vals_t));
   value->sym = sym;
   value->val = val;
   value->next = values;
   values = value;
}

long value_get(sym_t * sym)
{
   vals_t * ptr = values;

   while (ptr != NULL)
   {
      if (ptr->sym == sym)
      {
         return ptr->val;
      }

      ptr = ptr->next;
   }
   
   exception("Undefined symbol\n");
}

void eval_init()
{
   values = NULL;
}

long eval(ast_t * ast)
{
    long t;
    
    switch (ast->typ)
    {
    case T_INT:
        return atol(ast->sym->name);
    case T_ADD:
        return eval(ast->child) + eval(ast->child->next);
    case T_SUB:
        return eval(ast->child) - eval(ast->child->next);
    case T_MUL:
        return eval(ast->child) * eval(ast->child->next);
    case T_DIV:
        return eval(ast->child) / eval(ast->child->next);
    case T_REM:
        return eval(ast->child) % eval(ast->child->next);
    case T_NEG:
        return -eval(ast->child);
    case T_ASSIGN:
        t = eval(ast->child->next);
        value_store(ast->child->sym, t);
        return t;
    case T_IDENT:
        return value_get(ast->sym);
    default:
        exception("Unknown ast tag in eval\n");
    }
}

