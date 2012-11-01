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

#include "types.h"

type_t * t_nil;
type_t * t_int;
type_t * t_bool;
type_t * t_double;
type_t * t_string;
type_t * t_char;

type_t * new_type(typ_t typ)
{
   type_t * t = (type_t *) GC_MALLOC(sizeof(type_t));
   t->typ = typ;
   return t;
}

void types_init(void)
{
   t_nil = new_type(NIL);
   t_int = new_type(INT);
   t_bool = new_type(BOOL);
   t_double = new_type(DOUBLE);
   t_string = new_type(STRING);
   t_char = new_type(CHAR);
}

type_t * fn_type(type_t * ret, int arity, type_t ** args)
{
   int i;
   
   type_t * t = (type_t *) GC_MALLOC(sizeof(type_t));
   t->typ = FN;
   t->args = (type_t **) GC_MALLOC(sizeof(type_t *)*arity);
   t->ret = ret;
   t->arity = arity;
   
   for (i = 0; i < arity; i++)
      t->args[i] = args[i];

   return t;
}

type_t * generic_type(int arity, type_t ** args)
{
   int i;
   
   type_t * t = (type_t *) GC_MALLOC(sizeof(type_t));
   t->typ = GENERIC;
   t->args = (type_t **) GC_MALLOC(sizeof(type_t *)*arity);
   t->arity = arity;
   
   for (i = 0; i < arity; i++)
      t->args[i] = args[i];

   return t;
}

type_t * tuple_type(int arity, type_t ** args)
{
   int i;
   
   type_t * t = (type_t *) GC_MALLOC(sizeof(type_t));
   t->typ = TUPLE;
   t->args = (type_t **) GC_MALLOC(sizeof(type_t *)*arity);
   t->arity = arity;

   for (i = 0; i < arity; i++)
      t->args[i] = args[i];

   return t;
}

type_t * data_type(int arity, type_t ** args, sym_t * sym, 
                       sym_t ** slots, int num_params, sym_t ** params)
{
   int i;
   
   type_t * t = (type_t *) GC_MALLOC(sizeof(type_t));
   t->typ = DATATYPE;
   t->args = (type_t **) GC_MALLOC(sizeof(type_t *)*arity);
   t->slots = (sym_t **) GC_MALLOC(sizeof(sym_t *)*arity);
   t->arity = arity;
   t->num_params = num_params;
   t->params = params;

   for (i = 0; i < arity; i++)
   {
       t->args[i] = args[i];
       t->slots[i] = slots[i];
   }
   
   t->sym = sym;

   return t;
}

type_t * array_type(type_t * el_type)
{
   type_t * t = (type_t *) GC_MALLOC(sizeof(type_t));
   t->typ = ARRAY;
   t->ret = el_type;
   
   return t;
}

type_t * fn_to_lambda_type(type_t * type)
{
    type = fn_type(type->ret, type->arity, type->args);
    type->typ = LAMBDA; 
    return type;
}

type_t * new_typevar(void)
{
    static long typevarnum = 0;
    type_t * t = new_type(TYPEVAR);
    t->arity = typevarnum++;
    return t;
}
