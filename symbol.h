#ifndef SYMBOL_H
#define SYMBOL_H

#include <string.h>
#include <stdio.h>
#include "gc.h"

#define SYM_TAB_SIZE 10000

typedef struct sym_t {
   char * name;
} sym_t;

void sym_tab_init(void);

void print_sym_tab(void);

sym_t * sym_lookup(const char * name);

#endif

