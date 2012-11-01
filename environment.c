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

#include "environment.h"

env_t * current_scope;

void scope_init(void)
{
   current_scope = (env_t *) GC_MALLOC(sizeof(env_t));
}

void intrinsics_init(void)
{
   type_t * int_op;
   
   type_t ** args = GC_MALLOC(2*sizeof(type_t *));
   
   args[0] = t_int;
   args[1] = t_int;

   int_op = fn_type(t_int, 2, args);
   int_op->intrinsic = 1;
   
   args[0] = int_op;
   
   bind_generic(sym_lookup("+"), generic_type(1, args));
   bind_generic(sym_lookup("-"), generic_type(1, args));
   bind_generic(sym_lookup("*"), generic_type(1, args));
   bind_generic(sym_lookup("/"), generic_type(1, args));
   bind_generic(sym_lookup("%"), generic_type(1, args));
}

bind_t * bind_generic(sym_t * sym, type_t * type)
{
   bind_t * scope = current_scope->scope;
   bind_t * b = (bind_t *) GC_MALLOC(sizeof(bind_t));
   b->sym = sym;
   b->type = type;
   b->next = scope;
   current_scope->scope = b;
   return b;
}
