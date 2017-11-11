#include <stdlib.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include "utils.h"
#include "loader.h"
#include "decoder.h"
#include "execution.h"

int load_fun(uint8_t *buf, size_t start, imp_fun *ret) {
    size_t dec_num, i;
    fun_fmt *fun = (fun_fmt *) (buf + start);
    ret->code_len = ntohll(fun->len);
    ret->code = calloc(sizeof(imp_ins_dec), ret->code_len);
    if (ret->code == NULL) {
        printf("unable to allocate memory for code");
        return 1;
    }
    dec_num = decode(buf + start + sizeof(uint64_t), ret->code_len, ret->code);
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
                if (ret->marks == NULL) {
                    printf("error reallocating space for marks\n");
                    free(ret->code);
                    return 1;
                }
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

    if (prog.data_len == 0) {
        prog.data = NULL;
        return 0;
    }

    prog.data = mmap(NULL,
                     ((prog.data_len * sizeof(uint64_t)) + 0xFFF) & (~0xFFFull),
                     PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS,0,0);
    if (prog.data == MAP_FAILED) {
        printf("unable to map memory for data\n");
        return 1;
    }

    for (i = 0; i < ntohll(dat->flen); i++) {
        prog.data[i] = ntohll(dat->data[i]);
    }
    return 0;
}

int load_prog_from_stdin() {
    uint64_t val, i;
    uint64_t *starts;
    uint8_t *buf;

    if(readN((uint8_t *) &val, sizeof(val)))
        return 1;

    prog.fun_num = ntohll(val);
    if (prog.fun_num > 256) {
        printf("this machine only supports up to 255 functions\n");
        return 1;
    }

    starts = calloc(sizeof(uint64_t), prog.fun_num + 1);
    prog.code = calloc(sizeof(imp_fun), prog.fun_num);
    
    if (prog.code == NULL || starts == NULL) {
        printf("unable to allocate function array or starts\n");
        return 1;
    }

    for (i = 0; i < prog.fun_num + 1; i++) {
        if (readN((uint8_t *) &val, sizeof(val))) {
            free (starts);
            free (prog.code);
            return 1;
        }
        starts[i] = ntohll(val); 
    }

    for (i = 0; i < prog.fun_num; i++) {
        val = starts[i + 1] - starts[i];
        buf = calloc(1, val);
        if (buf == NULL || readN(buf, val) || load_fun(buf, 0, prog.code + i)) {
            free(starts);
            free(prog.code);
            return 1;
        }
        free(buf);
    }
    free(starts);

    if (readN((uint8_t *) &val, sizeof(val)))
        return 1;
    i = ntohll(val);
    buf = calloc(8, i + 2);
    if (buf == NULL)
        return 1;
    memcpy(buf, &val, sizeof(val));
    if (readN(buf + 8, i * 8 + 8))
        return 1;
    load_data(buf, 0);
    free(buf);
    return 0;
}

int load_prog_from_file(char *const filename) {
    size_t i;
    file_fmt *f;
    int fd = open(filename, O_RDONLY);
    if (fd == -1) {
        if (errno == EACCES || errno == EFAULT)
            printf("unable to access file %s\n", filename);
        if (errno == ENOTDIR || errno == ENOENT) {
            printf("file %s does not exist\n", filename);
        }
        return 1;
    }

    struct stat sb;
    fstat(fd, &sb);
    if ((sb.st_mode & S_IFMT) != S_IFREG) {
        printf("%s is not a regular file!\n", filename);
        return 1;
    }

    uint8_t *buf = (uint8_t *) mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    close(fd);

    if (buf == MAP_FAILED) {
        printf("unable to map file into memory\n");
        return 1;
    }

    f = (file_fmt*) buf;
    prog.fun_num = ntohll(f->fnum);
    if (prog.fun_num > 256) {
        printf("this machine only supports up to 256 functions\n");
        munmap(buf, sb.st_size);
        return 1;
    }


    prog.code = calloc(sizeof(imp_fun), prog.fun_num);
    
    if (prog.code == NULL) {
        printf("unable to allocate function array\n");
        munmap(buf, sb.st_size);
        return 1;
    }

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
    return 0;
}