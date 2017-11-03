#ifndef LOADER_H
#define LOADER_H

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <netinet/in.h>

#include "imp.h"

typedef struct {
    uint64_t len;
    uint8_t code[];
} fun_fmt;

typedef struct {
    uint64_t flen;
    uint64_t mlen;
    uint64_t data[];
} data_fmt;

typedef struct {
    uint64_t fnum;
    uint64_t fst[];
} file_fmt;

int load_fun(uint8_t *buf, size_t start, imp_fun *ret);
int load_data(uint8_t *buf, size_t start);
int load_prog_from_file(char *const filename);

#endif