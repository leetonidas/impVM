#ifndef PROFILER_H
#define PROFILER_H

#include <stdint.h>
#include <sys/types.h>

void disable_jit();
int should_jit(size_t index);

#endif