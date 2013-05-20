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

#include <stdio.h>
#include "gc.h"
#include "ast.h"
#include "types.h"
#include "environment.h"
#include "inference.h"
#include "backend.h"

#include "parser.c"

#define DEBUG1 0 /* print ast */
#define DEBUG2 1 /* print ast after inference1 */

extern jmp_buf exc;

int main(void)
{
   ast_t * a;
   int jval;
   jit_t * jit;

   GC_INIT();
   GREG g;
 
   ast_init();
   sym_tab_init();
   types_init();
   scope_init();
   intrinsics_init();
   jit = llvm_init();

   yyinit(&g);

   printf("Welcome to Cesium v0.3\n\n");
   printf("> ");

   while (1)
   {
      if (!(jval = setjmp(exc)))
      {
         if (!yyparse(&g))
         {
            printf("Error parsing\n");
            abort();
         } else if (root)
         {
#if DEBUG1
            printf("\n");
            ast_print(root, 0, 0);
#endif
            inference1(root);
#if DEBUG2
            printf("\n");
            ast_print(root, 0, 1);
#endif
            exec_root(jit, root);
            root = NULL;
         }
      } else if (jval == 1)
         root = NULL;
      else /* jval == 2 */
         break;
      
      printf("\n> ");
   }

   llvm_cleanup(jit);
   yydeinit(&g);
    
   printf("\n");

   return 0;

}
