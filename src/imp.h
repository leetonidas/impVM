#ifndef IMP_H
#define IMP_H

#include <stdint.h>
#include <sys/types.h>

// the highly effictient 6/11 Bit variable length instruction set

typedef enum {
                // 0123 4567 89A
    IMP_JZ,     // 000T TTTT TTT
    IMP_JMP,    // 001T TTTT TTT
    IMP_CAL,    // 010T TTTT TTT
    IMP_MRK,    // 011T TTTT TTT
    IMP_LD,     // 1000 AAAA AAA
    IMP_ST,     // 1001 AAAA AAA
    IMP_LDR,    // 1010 AAAA AAA
    IMP_STR,    // 1011 AAAA AAA
    IMP_IMM,    // 110X XXXX XXX
    IMP_SHL,    // 1110 0RRR RRR
    IMP_SHR,    // 1110 1RRR RRR
    IMP_NOT,    // 1111 00-- ---
    IMP_AND,    // 1111 01-- ---
    IMP_OR,     // 1111 10-- ---
    IMP_GE,     // 1111 11-- ---
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
    size_t *marks;
    size_t num_marks;
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