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

