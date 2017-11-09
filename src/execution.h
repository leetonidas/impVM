#ifndef EXECUTION_H
#define EXECUTION_H

#include "imp.h"

#define user_def_funs 254

extern imp_prog prog;
extern cpu_state state;

int call_fun(size_t index);
void vm_getc();
void vm_putc();
void init_challenge();
void check_challenge();

#endif