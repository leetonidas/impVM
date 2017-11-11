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

uint64_t get_key_part(uint64_t *key, size_t index) {
    uint64_t k = key[index / 2];
    if (index % 2 == 1)
        return k & 0xFFFFFFFF;
    else
        return k >> 32; 
}

void xtea_encrypt(uint64_t *data, size_t dlen, uint64_t *key) {
    int i;
    size_t j;
    uint32_t sum = 0, tmp;
    for (i = 0; i < 32; i++) {
        for (j = 0; j < dlen; j++) {
            tmp = data[j] & 0xFFFFFFFF;
            tmp = (data[j] >> 32) + ((((tmp << 4) ^ (tmp >> 5)) + tmp) ^ (sum + get_key_part(key, sum & 3)));
            data[j] = (data[j] & 0xFFFFFFFF) | (((uint64_t) tmp) << 32);
        }
        sum += 0x9E3779B9;
        for (j = 0; j < dlen; j++) {
            tmp = data[j] >> 32;
            tmp = (data[j] + ((((tmp << 4) ^ (tmp >> 5)) + tmp) ^ (sum + get_key_part(key, (sum >> 11) & 3)))) & 0xFFFFFFFF;
            data[j] = ((data[j] >> 32) << 32) | tmp;
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
    fd = open("/dev/urandom", O_RDONLY);
    read(fd, prog.data + 4, 128 / 8);
    close(fd);
    memcpy(prog.data, rev_flag, sizeof(uint64_t) * 4);
    //memcpy(prog.data, "\x08\x07\x06\x05\x04\x03\x02\x01", 8);
    //memcpy(prog.data + 4, "\x78\x56\x34\x12\x67\x45\x23\x01\x9a\x78\x56\x34\x89\x67\x45\x23", 16);
    xtea_encrypt(prog.data, 4, prog.data + 4);
}

void check_challenge() {
    if (memcmp(rev_flag, prog.data, sizeof(uint64_t) * 4) == 0)
        printf("%s", rev_flag);
    else {
        printf("try harder next time\n");
    }
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