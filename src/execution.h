#ifndef EXECUTION_H
#define EXECUTION_H

#include "imp.h"

extern imp_prog prog;
extern cpu_state state;

int call_fun(size_t index);

#endif