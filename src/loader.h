#ifndef LOADER_H
#define LOADER_H

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <netinet/in.h>

#include "imp.h"

struct fun_fmt_st{
    uint64_t len;
    uint8_t code[];
} __attribute__((packed));

typedef struct fun_fmt_st fun_fmt;

struct data_fmt_st{
    uint64_t flen;
    uint64_t mlen;
    uint64_t data[];
} __attribute__((packed));

typedef struct data_fmt_st data_fmt;

struct file_fmt_st {
    uint64_t fnum;
    uint64_t fst[];
} __attribute__((packed));

typedef struct file_fmt_st file_fmt;

int load_fun(uint8_t *buf, size_t start, imp_fun *ret);
int load_data(uint8_t *buf, size_t start);
int load_prog_from_file(char *const filename);

int load_prog_from_stdin();

#endif