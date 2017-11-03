#ifndef EMULATOR_H
#define EMULATOR_H

#include <sys/types.h>
#include <stdint.h>

#include "imp.h"
#include "execution.h"

int emu_run_fun(size_t index);

#endif