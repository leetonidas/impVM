#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "utils.h"
#include "jitter.h"
#include "profiler.h"
#include "execution.h"
#include "emulator.h"

imp_prog prog;
cpu_state state;
int is_lockdown = 0;

char rev_flag[50];

void lockdown() {
    if(!is_lockdown) {
        if (init_seccomp() == 0) {
            memset(rev_flag, 0, sizeof(rev_flag));
            is_lockdown = 1;
        } else {
            printf("unable to init seccomp!\n");
            exit(1);
        }
    }
}

void init_challenge() {
    size_t n;
    disable_jit();
    int fd = open("rev_flag.txt", O_RDONLY);
    n = read(fd, rev_flag, 49);
    close(fd);
    rev_flag[n] = 0;
}

void check_challenge() {
    printf("%s", rev_flag);
}

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
    case 253:
        check_challenge();
        break;
    case 252:
        init_challenge();
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