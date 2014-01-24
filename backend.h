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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "gc.h"
#include "exception.h"
#include "environment.h"
#include "inference.h"
#include "ast.h"
#include "serial.h"

#include <llvm-c/Core.h>  
#include <llvm-c/Analysis.h>  
#include <llvm-c/ExecutionEngine.h>  
#include <llvm-c/Target.h>  
#include <llvm-c/Transforms/Scalar.h> 

#ifndef BACKEND_H
#define BACKEND_H

#ifdef __cplusplus
 extern "C" {
#endif

#define TRACE 1 /* prints lots of ast and llvm trace info */

#define LOC_TAB_SIZE 10000 /* size of llvm locals hash table */

typedef struct loc_t {
   char * name;
   LLVMValueRef llvm_val;
} loc_t;

void loc_tab_init(void);

void loc_insert(const char * name, LLVMValueRef llvm_val);

LLVMValueRef loc_lookup(const char * name);

/* Are we on a 32 or 64 bit machine */
#if ULONG_MAX == 4294967295U
#define LLVMWordType() LLVMInt32Type()
#else
#define LLVMWordType() LLVMInt64Type()
#endif

typedef struct jit_t
{
    LLVMBuilderRef builder;
    LLVMValueRef function;
    LLVMExecutionEngineRef engine;  
    LLVMPassManagerRef pass;
    LLVMModuleRef module;
    LLVMBasicBlockRef breakto;
} jit_t;

typedef struct ret_t
{
    int closed;
    LLVMValueRef val;
} ret_t;

jit_t * llvm_init(void);

void llvm_reset(jit_t * jit);

void llvm_cleanup(jit_t * jit);

LLVMTypeRef type_to_llvm(jit_t * jit, type_t * type);

ret_t * exec_tuple_unpack_val(jit_t * jit, ast_t * ast1, LLVMValueRef val, type_t * type);

ret_t * exec_ast(jit_t * jit, ast_t * ast);

void exec_root(jit_t * jit, ast_t * ast);

void print_gen(jit_t * jit, type_t * type, LLVMGenericValueRef gen_val);

/* Set things up so we can begin jit'ing */
#define START_EXEC(ret_type) \
   LLVMBuilderRef __builder_save; \
   LLVMValueRef __function_save; \
   do { \
   __builder_save = jit->builder; \
   jit->builder = LLVMCreateBuilder(); \
   __function_save = jit->function; \
   LLVMTypeRef __args[] = { }; \
   LLVMTypeRef __retval = ret_type; \
   LLVMTypeRef __fn_type = LLVMFunctionType(__retval, __args, 0, 0); \
   jit->function = LLVMAddFunction(jit->module, "exec", __fn_type); \
   LLVMBasicBlockRef __entry = LLVMAppendBasicBlock(jit->function, "entry"); \
   LLVMPositionBuilderAtEnd(jit->builder, __entry); \
   } while (0)
   
/* Run the jit'd code */
#define END_EXEC(gen_val) \
   do { \
   LLVMRunFunctionPassManager(jit->pass, jit->function); \
   if (TRACE) \
      LLVMDumpModule(jit->module); \
   LLVMGenericValueRef __exec_args[] = {}; \
   gen_val = LLVMRunFunction(jit->engine, jit->function, 0, __exec_args); \
   LLVMDeleteFunction(jit->function); \
   LLVMDisposeBuilder(jit->builder); \
   jit->function = __function_save; \
   jit->builder = __builder_save; \
   } while (0)

#ifdef __cplusplus
}
#endif

#endif

