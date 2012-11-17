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
type_t * t_int8;
type_t * t_int16;
type_t * t_int32;
type_t * t_int64;
type_t * t_uint;
type_t * t_uint8;
type_t * t_uint16;
type_t * t_uint32;
type_t * t_uint64;
type_t * t_bool;
type_t * t_double;
type_t * t_float;
type_t * t_string;
type_t * t_char;

type_node_t * tuple_type_list;

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
   t_int8 = new_type(INT8);
   t_int16 = new_type(INT16);
   t_int32 = new_type(INT32);
   t_int64 = new_type(INT64);
   t_uint = new_type(UINT);
   t_uint8 = new_type(UINT8);
   t_uint16 = new_type(UINT16);
   t_uint32 = new_type(UINT32);
   t_uint64 = new_type(UINT64);
   t_bool = new_type(BOOL);
   t_double = new_type(DOUBLE);
   t_float = new_type(FLOAT);
   t_string = new_type(STRING);
   t_char = new_type(CHAR);

   tuple_type_list = NULL;
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

   type_node_t * s = tuple_type_list;

   while (s != NULL)
   {
      if (s->type->arity == arity)
      {
         for (i = 0; i < arity; i++)
            if (s->type->args[i] != t->args[i]) break;
         
         if (i == arity)
            return s->type;
      }
      s = s->next;
   }

   s = GC_MALLOC(sizeof(type_node_t));
   s->type = t;
   s->next = tuple_type_list;
   tuple_type_list = s;

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

void type_print(type_t * type)
{
   int i;
   
   switch (type->typ)
   {
   case INT:
      printf("int");
      break;
   case INT8:
      printf("int8");
      break;
   case INT16:
      printf("int16");
      break;
   case INT32:
      printf("int32");
      break;
   case INT64:
      printf("int64");
      break;
   case UINT:
      printf("uint");
      break;
   case UINT8:
      printf("uint8");
      break;
   case UINT16:
      printf("uint16");
      break;
   case UINT32:
      printf("uint32");
      break;
   case UINT64:
      printf("uint64");
      break;
   case DOUBLE:
      printf("double");
      break;
   case FLOAT:
      printf("float");
      break;
   case CHAR:
      printf("char");
      break;
   case STRING:
      printf("string");
      break;
   case BOOL:
      printf("bool");
      break;
   case TUPLE:
      printf("(");
      for (i = 0; i < type->arity - 1; i++)
         type_print(type->args[i]), printf(", ");
      type_print(type->args[i]), printf(")");
      break;
   case NIL:
      printf("nil");
      break;
   default:
      exception("Unknown type in type_print\n");
   }
}