#include <stdlib.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

#include "utils.h"
#include "loader.h"
#include "decoder.h"
#include "execution.h"

int load_fun(uint8_t *buf, size_t start, imp_fun *ret) {
    size_t dec_num, i;
    fun_fmt *fun = (fun_fmt *) (buf + start);
    ret->code_len = ntohll(fun->len);
    ret->code = calloc(sizeof(imp_ins_dec), ret->code_len);
    dec_num = decode(buf + start + 8, ret->code_len, ret->code);
    if (dec_num != ret->code_len) {
        free(ret->code);
        return 1;
    }

    ret->num_marks = 0;
    ret->marks = NULL;

    for (i = 0; i < dec_num; i++) {
        if (ret->code[i].mnemonic == IMP_MRK) {
            if (ret->code[i].operand + 1 > ret->num_marks) {
                ret->num_marks = ret->code[i].operand + 1;
                ret->marks = realloc(ret->marks, ret->num_marks * sizeof(size_t));
            }

            ret->marks[ret->code[i].operand] = i;
        }
    }

    return 0;
}

int load_data(uint8_t *buf, size_t start) {
    size_t i;
    data_fmt *dat = (data_fmt *) (buf + start);
    prog.data_len = ntohll(dat->mlen);
    if (prog.data_len < ntohll(dat->flen))
        return -1;
    prog.data = mmap(NULL,
                     ((prog.data_len * sizeof(uint64_t)) + 0xFFF) & (~0xFFFull),
                     PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS,0,0);
    for (i = 0; i < ntohll(dat->flen); i++) {
        prog.data[i] = ntohll(dat->data[i]);
    }
    return 0;
}

int load_prog_from_file(char *const filename) {
    size_t i;
    file_fmt *f;
    int fd = open(filename, O_RDONLY);
    struct stat sb;
    fstat(fd, &sb);
    uint8_t *buf = (uint8_t *) mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    f = (file_fmt*) buf;
    prog.fun_num = ntohll(f->fnum);
    prog.code = calloc(sizeof(imp_fun), prog.fun_num);
    for (i = 0; i < prog.fun_num; i++) {
        if (load_fun(buf, ntohll(f->fst[i]), prog.code + i) != 0)
            break;
    }
    if (i != prog.fun_num || load_data(buf, ntohll(f->fst[i])) != 0) {
        for (; i > 0; i--) {
            free(prog.code[i - 1].code);
        }
        free(prog.code);
        munmap(buf, sb.st_size);
        return -1;
    }

    munmap(buf, sb.st_size);
    close(fd);
    return 0;
}