#include <string.h>
#include <stdio.h>

#include "jitter.h"
#include "profiler.h"
#include "execution.h"
#include "emulator.h"

imp_prog prog;
cpu_state state;

int call_fun(size_t index) {
    cpu_state save;
    int ret = 0;

    switch (index) {
    case 255:
        vm_putc();
        break;
    case 254:
        vm_getc();
        break;
    default:
        if (index >= prog.fun_num) {
            ret = -1;
            break;
        }

        memcpy(&save, &state, sizeof(cpu_state));
        state.fp = index;
        state.ip = 0;
        if (!should_jit(index))
            ret = emu_run_fun(index);
        else
            ret = jit_run_fun(index);
        prog.stack[++save.sp] = prog.stack[state.sp];
        memcpy(&state, &save, sizeof(cpu_state));
        break;
    }
    
    return ret;
}

void vm_getc() {
    uint64_t val = (uint64_t) getchar();
    prog.stack[++state.sp] = val;
}

void vm_putc() {
    putchar((char) prog.stack[state.sp]);
}