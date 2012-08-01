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
#include "input.h"
#include "exception.h"
#include "ast.h"
#include "gc.h"

#ifndef PARSER_H
#define PARSER_H

typedef ast_t * (*comb_fn)(input_t *, void *);
   
typedef struct
{
    comb_fn fn;
    void * args;
} combinator_t;

typedef struct
{
    char * str;
} match_args;

typedef struct seq_list
{
    combinator_t * comb;
    struct seq_list * next;
} seq_list;

typedef struct
{
    tag_t typ;
    seq_list * list;
} seq_args;

typedef enum
{
   EXPR_BASE, EXPR_INFIX, EXPR_PREFIX, EXPR_POSTFIX
} expr_fix;

typedef enum
{
   ASSOC_LEFT, ASSOC_RIGHT, ASSOC_FLAT, ASSOC_NONE
} expr_assoc;

typedef struct op_t
{
   tag_t tag;
   combinator_t * comb;
   struct op_t * next;
} op_t;

typedef struct expr_list
{
   op_t * op;
   expr_fix fix;
   expr_assoc assoc;
   combinator_t * comb;
   
   struct expr_list * next;
} expr_list;

combinator_t * new_combinator();

combinator_t * match(char * str);

combinator_t * integer();

combinator_t * seq(combinator_t * ret, tag_t typ, combinator_t * c1, ...);

combinator_t * multi(combinator_t * ret, combinator_t * c1, ...);

combinator_t * expr(combinator_t * exp, combinator_t * base);

ast_t * parse(input_t * in, combinator_t * comb);

#endif
