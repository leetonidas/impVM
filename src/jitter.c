#include <string.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <stdio.h>

#include "imp.h"
#include "jitter.h"
#include "execution.h"

#define max_jit_num 8

typedef struct {
    size_t index;
    size_t len;
    uint64_t *(*fun) (uint64_t*, uint64_t *, uint64_t *, uint64_t *(*) (size_t, uint64_t*));
} jit_entry;

uint64_t *call_fwd(size_t index, uint64_t *st) {
    state.sp = st - prog.stack;
    call_fun(index);
    return &prog.stack[state.sp];
}

size_t lut[user_def_funs];
jit_entry jitted_funs[max_jit_num];
size_t jit_num = 0;
size_t jit_index = 0;

void init() {
    size_t i;
    memset(jitted_funs, 0, sizeof(jitted_funs));
    for (i = 0; i < user_def_funs; i++)
        lut[i] = max_jit_num;
    for (i = 0; i < max_jit_num; i++)
        jitted_funs[i].index = user_def_funs;
}

int jit_run_fun(size_t index) {
    uint64_t *ret;
    if (index >= user_def_funs)
        return 1;
    
    if (lut[index] >= max_jit_num) {
        if (jit_num == max_jit_num) {
            evict(jitted_funs[jit_index].index);
        }
        if (jit(index) != 0)
            return 1;
    }

    ret = jitted_funs[lut[index]].fun(prog.data, &prog.stack[state.sp], &state.sp, call_fwd);
    state.sp = ret - prog.stack;
    return 0;
}

int is_jitted(size_t index) {
    return lut[index] < max_jit_num;
}

int evict(size_t index) {
    size_t jit_idx;

    if (lut[index] >= max_jit_num)
        return 1;
    
    jit_idx = lut[index];

    munmap ((void*) jitted_funs[jit_idx].fun, jitted_funs[jit_idx].len);
    lut[index] = max_jit_num;
    jitted_funs[jit_idx].len = 0;
    jitted_funs[jit_idx].fun = (uint64_t *(*)()) NULL;
    jit_num--;
    jit_index = jit_idx;

    return 0;
}

typedef struct {
    uint8_t opc[1];
    int32_t diff;
}__attribute__((packed)) jmp_rel4;

typedef struct {
    uint8_t opc[13];
    int32_t diff;
}__attribute__((packed)) jz_rel4;

typedef struct {
    uint8_t opc[7];
    uint32_t diff;
}__attribute__((packed)) ld_cmp;

typedef struct {
    uint8_t opc[10];
    uint32_t diff;
}__attribute__((packed)) ldr_cmp;

typedef struct {
    uint8_t opc[15];
    uint32_t diff;
}__attribute__((packed)) st_cmp;

typedef struct {
    uint8_t opc[18];
    uint32_t diff;
}__attribute__((packed)) str_cmp;

typedef struct {
    uint8_t opc[1];
    uint32_t val;
}__attribute__((packed)) imm_shr_shl_cmp;

typedef struct {
    uint8_t opc[4];
    uint32_t num;
}__attribute__((packed)) cal_rel4;

uint8_t cmp_init[6] = "\x48\x89\xd3\x48\x89\xc8";
uint8_t cmp_fini[4] = "\x48\x89\xf0\xc3";

size_t jit_len[15] = {17,5,16,0,14,19,17,22,12,14,14,3,10,10,23};
// pointer to top of stack in rsi
// pointer to data in rdi
// pointer to state.sp in rbx
uint8_t prec[15][24] = {
    "\x48\x8b\x16\x48\x83\xee\x08\x48\x83\xfa\x00\x0f\x84\x00\x00\x00\x00",
    "\xe9\x00\x00\x00\x00",
    "\x57\x53\x50\xbf\x00\x00\x00\x00\xff\xd0\x48\x89\xc6\x58\x5b\x5f",
    "",
    "\x48\x8b\x16\x48\x8b\x8c\xd7\x00\x00\x00\x00\x48\x89\x0e",
    "\x48\x8b\x16\x48\x8b\x4e\xf8\x48\x83\xee\x10\x48\x89\x8c\xd7\x00\x00\x00\x00",
    "\x48\x8b\x16\x48\xf7\xda\x48\x8b\x8c\xd6\x00\x00\x00\x00\x48\x89\x0e",
    "\x48\x8b\x16\x48\xf7\xda\x48\x8b\x4e\xf8\x48\x83\xee\x10\x48\x89\x8c\xd6\x00\x00\x00\x00",
    "\xba\x00\x00\x00\x00\x48\x83\xc6\x08\x48\x89\x16",
    "\xb9\x00\x00\x00\x00\x48\x8b\x16\x48\xd3\xe2\x48\x89\x16",
    "\xb9\x00\x00\x00\x00\x48\x8b\x16\x48\xd3\xea\x48\x89\x16",
    "\x48\xf7\x16",
    "\x48\x8b\x16\x48\x83\xee\x08\x48\x21\x16",
    "\x48\x8b\x16\x48\x83\xee\x08\x48\x09\x16",
    "\x48\x8b\x16\x48\x83\xee\x08\x48\x8b\x0e\x48\x29\xd1\x48\xc1\xf9\x3f\x48\xff\xc1\x48\x89\x0e"
};

int jit(size_t index) {
    size_t len, i, mark_pos[256];
    imp_fun *fun;
    uint8_t *buf;
    uint8_t *pos;
    jz_rel4 *jz;
    jmp_rel4 *jmp;
    cal_rel4 *cal;
    ld_cmp *ld;
    st_cmp *st;
    ldr_cmp *ldr;
    str_cmp *str;
    imm_shr_shl_cmp *imm_cmp;

    if (index >= prog.fun_num)
        return 1;

    fun = &prog.code[index];

    if (jit_num == max_jit_num)
        return 0;

    while (jitted_funs[jit_index].index < user_def_funs)
        jit_index = (jit_index + 1) % max_jit_num;

    len = 6;
    for (i = 0; i < fun->code_len; i++) {
        len += jit_len[fun->code[i].mnemonic];
        if (fun->code[i].mnemonic == IMP_MRK)
            mark_pos[fun->code[i].operand] = len;
    }
    len += 4;

    buf = mmap(NULL, len, PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, 0, 0);
    if (buf == MAP_FAILED) {
        printf("unable to allocate memory for jit\n");
        return 11;
    }

    pos = buf + 6;
    memcpy(buf, cmp_init, 6);
    for (i = 0; i < fun->code_len; i++) {
        memcpy(pos, prec[fun->code[i].mnemonic], jit_len[fun->code[i].mnemonic]);
        switch(fun->code[i].mnemonic) {
        case IMP_JZ:
            jz = (jz_rel4*) pos;
            jz->diff = ((int32_t) mark_pos[fun->code[i].operand]) - ((int32_t) (pos - buf + sizeof(jz_rel4)));
            break;
        case IMP_JMP:
            jmp = (jmp_rel4*) pos;
            jmp->diff = ((int32_t) mark_pos[fun->code[i].operand]) - ((int32_t) (pos - buf + sizeof(jmp_rel4)));
            break;
        case IMP_CAL:
            cal = (cal_rel4*) pos;
            cal->num = fun->code[i].operand;
            break;
        case IMP_LD:
            ld = (ld_cmp*) pos;
            ld->diff = fun->code[i].operand * 8;
            break;
        case IMP_ST:
            st = (st_cmp*) pos;
            st->diff = fun->code[i].operand * 8;
            break;
        case IMP_LDR:
            ldr = (ldr_cmp*) pos;
            ldr->diff = - (int32_t)(fun->code[i].operand * 8 + 8);
            break;
        case IMP_STR:
            str = (str_cmp*) pos;
            str->diff = - (int32_t)(fun->code[i].operand * 8);
            break;
        case IMP_IMM:
        case IMP_SHR:
        case IMP_SHL:
            imm_cmp = (imm_shr_shl_cmp*) pos;
            imm_cmp->val = fun->code[i].operand;
            break;
        default:
            break;
        }
        pos += jit_len[fun->code[i].mnemonic];
    }
    memcpy(pos,cmp_fini,4);
    
    if (((size_t)(pos - buf)) + 4 != len) {
        printf("ERROR: LENGTH DIFFERENT\n");
        exit(1);
    }
    mprotect(buf, len, PROT_READ | PROT_EXEC);


    jitted_funs[jit_index].index = index;
    jitted_funs[jit_index].len = len;
    jitted_funs[jit_index].fun = (uint64_t *(*) (uint64_t*, uint64_t*, uint64_t*, uint64_t *(*)(size_t, uint64_t *))) buf;
    lut[index] = jit_index;
    jit_index++;

    return 0;
}