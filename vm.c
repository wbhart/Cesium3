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
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, O `R TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

#include <stdio.h>

#include "vm.h"
#include "gc.h"

#define HALT      0
#define HALT_INT  1
#define SET_X     2
#define DEC_X     3
#define JUMP_X_NZ 4

#define STORE_ADDRESS(index, label) \
    data[index] = &&label

#define GOTO_IP goto **(ip++)

void execute(void ** return_val, void ** data, void ** stack, int run)
{
   int x = 0;
   void ** ip;

   if (!run)
      goto get_opcodes;

   ip = data;
   GOTO_IP;

op_halt:
   return;

op_halt_int_imm:
   *return_val = *((int*)(ip++));
   return;

op_set_x:
   x = *((int*)(ip++));
   GOTO_IP;

op_dec_x:
   --x;
   GOTO_IP;

op_jump_x_nz:
   {
     void * target = *(ip++);
     if (x) 
        ip = (void **) target;
     GOTO_IP;
   }

get_opcodes:
   STORE_ADDRESS(HALT, op_halt);
   STORE_ADDRESS(HALT_INT, op_halt_int);
   STORE_ADDRESS(SET_X, op_set_x);
   STORE_ADDRESS(DEC_X, op_dec_x);
   STORE_ADDRESS(JUMP_X_NZ, op_jump_x_nz);
}

void vm_init(void *** stack, void *** prog, void *** opcodes)
{
   *stack = GC_MALLOC(1000000*sizeof(void *));
   *prog  = GC_MALLOC(1000000*sizeof(void *));
   *opcodes  = GC_MALLOC(1000000*sizeof(void *));
   
   execute(*opcodes, *stack, 0);
}

/*int main()
{
  void * opcodes[10];
  void * program[20];
  double start_time, end_time;

  execute(opcodes, 0);

  program[0] = opcodes[SET_X];
  program[1] = (void *) 2000000000;
  program[2] = opcodes[DEC_X];
  program[3] = opcodes[JUMP_X_NZ];
  program[4] = &program[2];
  program[5] = opcodes[HALT];

  execute(program, 1);
  
  return 0;
}*/

