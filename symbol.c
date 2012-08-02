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

#include "symbol.h"

sym_t ** sym_tab;

void sym_tab_init(void)
{
    sym_tab = (sym_t **) GC_MALLOC(SYM_TAB_SIZE*sizeof(sym_t *));
}

sym_t * new_symbol(const char * name, int length)
{
   sym_t * sym = (sym_t *) GC_MALLOC(sizeof(sym_t));
   sym->name = (char *) GC_MALLOC(length + 1);
   strcpy(sym->name, name);
   return sym;
}

void print_sym_tab(void)
{
    int i;
    for (i = 0; i < SYM_TAB_SIZE; i++)
        if (sym_tab[i])
            printf("%s\n", sym_tab[i]->name);
}

int sym_hash(const char * name, int length)
{
    int hash = (int) name[0];
    int i;
    for (i = 1; i < length; i++)
        hash += (name[i] << ((3*i) % 15));
    return hash % SYM_TAB_SIZE;
}

sym_t * sym_lookup(const char * name)
{
   int length = strlen(name);
   int hash = sym_hash(name, length);
   sym_t * sym;

   while (sym_tab[hash])
   {
       if (strcmp(sym_tab[hash]->name, name) == 0)
           return sym_tab[hash];
       hash++;
       if (hash == SYM_TAB_SIZE)
           hash = 0;
   }

   sym = new_symbol(name, length);
   sym_tab[hash] = sym;
   return sym;
}

