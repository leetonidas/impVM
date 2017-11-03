#include <stdio.h>
#include <sys/mman.h>

#include "loader.h"
#include "execution.h"

int main(int argc, char **argv) {
    if (argc == 0) {
        printf("usage: ./impVM <file>\n");
        return -1;
    }

    if (argc == 1) {
        printf("usage: %s <file>\n", argv[0]);
        return -1;
    }

    if (load_prog_from_file(argv[1]) != 0)
        return -2;
    
    prog.stack = mmap(NULL, 0x10000, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, 0 , 0);
    return call_fun(0);
}