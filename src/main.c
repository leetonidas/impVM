#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <getopt.h>
#include <unistd.h>

#include "profiler.h"
#include "jitter.h"
#include "loader.h"
#include "execution.h"

char pwn_flag[] = "";
const char optstr[] = "inl";

const struct option opts[4] = {{"from-stdin", 0, NULL, 'i'},
    {"no-jit", 0, NULL, 'n'},
    {"lockdown", 0, NULL, 'l'},
    {NULL, 0, 0, 0}};

int main(int argc, char **argv) {
    int res;
    char *filename;
    int fromStdin = 0, ld = 0;

    while ((res = getopt_long(argc, argv, optstr, opts, NULL)) != -1) {
        switch (res) {
        case 'i':
            fromStdin = 1;
            break;
        case 'n':
            disable_jit();
            break;
        case 'l':
            ld = 1;
            break;
        }
    }

    if (argc == (optind + 1))
        filename = argv[optind];

    if (!fromStdin && load_prog_from_file(filename) != 0)
        return -2;
    
    if (fromStdin && load_prog_from_stdin() != 0)
        return -2;
    
    if (prog.fun_num > 254) {
        printf("WARNING: some user defined functions are shadowed by supervisor calls\n");
    }

    prog.stack = mmap(NULL, 0x10000, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, 0 , 0);
    init();

    printf("[+] program loaded\n");
    if (ld)
        lockdown();

    res = call_fun(0);
    if (res == 0)
        return (uint8_t) prog.stack[state.sp];
    else
        return res;
}
