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

#include <llvm-c/Core.h>  
#include <llvm-c/Analysis.h>  
#include <llvm-c/ExecutionEngine.h>  
#include <llvm-c/Target.h>  
#include <llvm-c/Transforms/Scalar.h> 

#include "environment.h"
#include "symbol.h"
#include "types.h"

#ifndef AST_H
#define AST_H

#ifdef __cplusplus
 extern "C" {
#endif

typedef enum
{
   T_NONE, T_BOOL, T_INT, T_UINT, T_DOUBLE, 
   T_FLOAT, T_CHAR, T_STRING,
   T_BINOP, T_IDENT, T_BLOCK, T_IF_ELSE_EXPR, 
   T_IF_ELSE_STMT, T_IF_STMT, T_THEN, T_ELSE, 
   T_ASSIGN, T_WHILE_STMT, T_DO, T_BREAK,
   T_TUPLE, T_TUPLE_ASSIGN, T_TYPE_SLOT,
   T_SLOT, T_TYPE_STMT, T_TYPE_BODY, T_APPL,
   T_TUPLE_TYPE, T_TYPENAME, T_SLOT_NAME,
   T_PLACE_ASSIGN, T_LSLOT, T_TUPLE_UNPACK, T_LOCATION,
   T_FN_STMT, T_FN_PROTO, T_PARAM_BODY, T_PARAM, 
   T_RETURN, T_ARRAY, T_LLOCATION, T_LAPPL,
   T_TYPE_VAR
} tag_t;

typedef struct ast_t
{
   tag_t tag;
   struct ast_t * child;
   struct ast_t * next;
   type_t * type;
   sym_t * sym;
   env_t * env;
} ast_t;

extern ast_t * root;

extern ast_t * ast_nil;

ast_t * new_ast();

void ast_init();

void ast_print(ast_t * ast, int indent, int types);

ast_t * ast0(tag_t tag);

ast_t * ast1(tag_t tag, ast_t * a1);

ast_t * ast2(tag_t tag, ast_t * a1, ast_t * a2);

ast_t * ast3(tag_t tag, ast_t * a1, ast_t * a2, ast_t * a3);

ast_t * ast4(tag_t tag, ast_t * a1, ast_t * a2, ast_t * a3, ast_t * a4);

ast_t * ast_binop(sym_t * sym, ast_t * a1, ast_t * a2);

ast_t * ast_symbol(tag_t tag, sym_t * sym);

#ifdef __cplusplus
}
#endif

#endif