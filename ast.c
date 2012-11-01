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

#include "ast.h"

ast_t * root;

ast_t * ast_nil;

ast_t * new_ast()
{
   ast_t * ast = GC_MALLOC(sizeof(ast_t));
      
   return ast;
}

void ast_init()
{
    ast_nil = new_ast();
    ast_nil->typ = T_NONE;
}

ast_t * ast1(tag_t typ, ast_t * a1)
{
   ast_t * ast = new_ast();
   ast->typ = typ;
   ast->child = a1;
   return ast;
}

ast_t * ast2(tag_t typ, ast_t * a1, ast_t * a2)
{
   ast_t * ast = new_ast();
   ast->typ = typ;
   ast->child = a1;
   ast->child->next = a2;
   return ast;
}

ast_t * ast_symbol(tag_t typ, sym_t * sym)
{
   ast_t * ast = new_ast();
   ast->typ = typ;
   ast->sym = sym;
}