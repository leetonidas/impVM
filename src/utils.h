#include <stdint.h>
#include <sys/types.h>

uint64_t htonll(uint64_t);
uint64_t ntohll(uint64_t);

int readN(uint8_t *buf, size_t n);
int init_seccomp();