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

#include "gc.h"

#include "symbol.h"
#include "types.h"

#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include <llvm-c/Core.h>  

#ifdef __cplusplus
 extern "C" {
#endif

typedef struct bind_t
{
   type_t * type;
   sym_t * sym;
   char * llvm;
   LLVMValueRef llvm_val;
   struct bind_t * next;
} bind_t;

typedef struct env_t
{
   bind_t * scope;
   struct env_t * next;
} env_t;

extern env_t * current_scope;

void scope_init(void);

void intrinsics_init(void);

bind_t * bind_generic(sym_t * sym, type_t * type);

bind_t * bind_symbol(sym_t * sym, type_t * type, char * llvm);

bind_t * find_symbol(sym_t * sym);

env_t * scope_up(void);

void scope_down(void);

int scope_is_global(bind_t * bind);

#ifdef __cplusplus
}
#endif

#endif