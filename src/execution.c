#include <string.h>

#include "execution.h"
#include "emulator.h"

imp_prog prog;
cpu_state state;

int call_fun(size_t index) {
    cpu_state save;
    int ret;
    if (index >= prog.fun_num)
        return -1;

    memcpy(&save, &state, sizeof(cpu_state));
    state.fp = index;
    state.ip = 0;
    ret = emu_run_fun(index);
    prog.stack[++save.sp] = prog.stack[state.sp];
    memcpy(&state, &save, sizeof(cpu_state));
    return ret;
}