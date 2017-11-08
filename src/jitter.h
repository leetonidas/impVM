#ifndef JITTER_H
#define JITTER_H

#include <stdint.h>
#include <sys/types.h>

void init();
int jit_run_fun(size_t index);
int is_jitted(size_t index);
int evict(size_t index);
int jit(size_t index);

#endif