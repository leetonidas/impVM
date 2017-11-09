#include <stdio.h>
#include <sys/mman.h>

#include "profiler.h"
#include "jitter.h"
#include "loader.h"
#include "execution.h"

int main(int argc, char **argv) {
    int res;
    if (argc == 0) {
        printf("usage: ./impVM <file>\n");
        return -1;
    }

    if (argc == 1) {
        printf("usage: %s <file>\n", argv[0]);
        return -1;
    }

    if (argc == 3)
        disable_jit();

    if (load_prog_from_file(argv[1]) != 0)
        return -2;
    
    if (prog.fun_num > 254) {
        printf("WARNING: some user defined functions are shadowed by supervisor calls\n");
    }

    prog.stack = mmap(NULL, 0x10000, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, 0 , 0);
    init();
    res = call_fun(0);
    if (res == 0)
        return (uint8_t) prog.stack[state.sp];
    else
        return res;
}
