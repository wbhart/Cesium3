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

#include <string.h>
#include <stdio.h>
#include "symbol.h"
#include "exception.h"
#include "gc.h"

#ifndef TYPES_H
#define TYPES_H

#ifdef __cplusplus
 extern "C" {
#endif

typedef enum
{
   NIL, BOOL, INT, DOUBLE, STRING, CHAR, 
   FN, LAMBDA, GENERIC, ARRAY, TUPLE, DATATYPE, TYPEVAR
} typ_t;

typedef struct type_t
{
   typ_t typ; /* kind of type */
   int arity; /* number of args */
   int num_params; /* number of type parameters */
   struct type_t ** args; /* arguments */
   struct type_t * ret; /* return type, for functions */
   sym_t ** params; /* type parameters */
   struct sym_t * sym; /* name of type */
   struct sym_t ** slots; /* names of type args/slots */
   int intrinsic; /* intrinsic function/operator? */
} type_t;

extern type_t * t_nil;
extern type_t * t_int;
extern type_t * t_bool;
extern type_t * t_double;
extern type_t * t_string;
extern type_t * t_char;

type_t * new_type(typ_t typ);

void types_init(void);

type_t * fn_type(type_t * ret, int arity, type_t ** args);

type_t * generic_type(int arity, type_t ** args);

type_t * tuple_type(int arity, type_t ** args);

type_t * data_type(int arity, type_t ** args, sym_t * sym, 
                     sym_t ** slots, int num_params, sym_t ** params);

type_t * array_type(type_t * el_type);

type_t * fn_to_lambda_type(type_t * type);

type_t * new_typevar(void);

void type_print(type_t * type);

#ifdef __cplusplus
}
#endif

#endif

