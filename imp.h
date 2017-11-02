#ifndef IMP_H
#define IMP_H

#include <stdint.h>
#include <sys/types.h>

// the highly effictient 6/11 Bit variable length instruction set

typedef enum {
                // 0123456789A
    IMP_JZ,     // 000TTTTTTTT
    IMP_JMP,    // 001TTTTTTTT
    IMP_CAL,    // 010TTTTTTTT
    IMP_MRK,    // 011TTTTTTTT
    IMP_LD,     // 1000AAAAAAA
    IMP_ST,     // 1001AAAAAAA
    IMP_LDR,    // 1010AAAAAAA
    IMP_STR,    // 1011AAAAAAA
    IMP_IMM,    // 110XXXXXXXX
    IMP_SHL,    // 11100RRRRRR
    IMP_SHR,    // 11101RRRRRR
    IMP_ADD,    // 111100-----
    IMP_AND,    // 111101-----
    IMP_OR,     // 111110-----
    IMP_GE,     // 111111-----
} imp_mne;

typedef struct {
    size_t fp;  // function pointer
    size_t ip;  // instruction pointer
    size_t sp;  // stack pointer
} cpu_state;

typedef struct {
    imp_mne mnemonic;
    size_t operand;
} imp_ins_dec;

typedef struct {
    imp_ins_dec *code;
    size_t code_len;
} imp_fun;

typedef struct {
    imp_fun *code;
    size_t fun_num;
    uint64_t *data;
    size_t data_len;
    uint64_t *stack;
} imp_prog;

#endif