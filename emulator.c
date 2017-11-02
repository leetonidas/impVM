#include <stdlib.h>

#include "emulator.h"

#include "imp.h"
#include "execution.h"

int single_step(imp_fun *fun, size_t *marks, size_t num_marks) {
    uint64_t top;
    uint64_t snd;
    imp_ins_dec *ins = &fun->code[state.ip++];

    switch(ins->mnemonic) {
    case IMP_JZ:
        if (state.sp == 0)
            return -1;
        if (ins->operand >= num_marks)
            return -1;
        top = prog.stack[state.sp--];
        if (top == 0)
            state.ip = marks[ins->operand];
        break;
    case IMP_JMP:
        if (ins->operand >= num_marks)
            return -1;
        state.ip = marks[ins->operand];
        break;
    case IMP_CAL:
        return call_fun(ins->operand);
        break;
    case IMP_LD:
        if (state.sp < 1)
            return -1;
        top = prog.stack[state.sp--] + ins->operand;
        if (top >= prog.data_len)
            return -1;
        prog.stack[++state.sp] = prog.data[top];
        break;
    case IMP_ST:
        if (state.sp < 2)
            return -1;
        top = prog.stack[state.sp--] + ins->operand;
        if (top >= prog.data_len)
            return -1;
        prog.data[top] = prog.stack[state.sp--];
        break;
    case IMP_LDR:
        if (state.sp < 1)
            return -1;
        top = prog.stack[state.sp--] + ins->operand;
        if (state.sp < top)
            return -1;
        state.sp++;
        prog.stack[state.sp] = prog.stack[state.sp - top - 1];
        break;
    case IMP_STR:
        if (state.sp < 1)
            return -1;
        top = prog.stack[state.sp--] + ins->operand + 1;
        if (state.sp < top)
            return -1;
        prog.stack[state.sp - top] = prog.stack[state.sp];
        state.sp--;
        break;
    case IMP_IMM:
        prog.stack[++state.sp] = ins->operand;
        break;
    case IMP_SHR:
        prog.stack[state.sp] = prog.stack[state.sp] >> ins->operand;
        break;
    case IMP_SHL:
        prog.stack[state.sp] = prog.stack[state.sp] << ins->operand;
        break;
    case IMP_ADD:
        if (state.sp < 2)
            return -1;
        top = prog.stack[state.sp--];
        prog.stack[state.sp] += top;
        break;
    case IMP_AND:
        if (state.sp < 2)
            return -1;
        top = prog.stack[state.sp--];
        prog.stack[state.sp] &= top;
        break;
    case IMP_OR:
        if (state.sp < 2)
            return -1;
        top = prog.stack[state.sp--];
        prog.stack[state.sp] |= top;
        break;
    case IMP_GE:
        if (state.sp < 2)
            return -1;
        top = prog.stack[state.sp--];
        snd = prog.stack[state.sp];
        prog.stack[state.sp] = (top >= snd) ? 1 : 0;
        break;
    case IMP_MRK:
        break;
    default:
        return -1;
    }
    return 0;
}

size_t get_marks(imp_fun *fun, size_t **array) {
    size_t i;
    size_t ret = 0;
    *array = NULL;

    for (i = 0; i < fun->code_len; i++){
        if (fun->code[i].mnemonic != IMP_MRK)
            continue;
        *array = (size_t *) realloc(*array, sizeof(size_t) * (ret + 1));
        *array[ret++] = i;
    }

    return ret;
}

int run_fun(size_t index) {
    imp_fun *fun;
    size_t *marks;
    size_t num_marks;

    if (index >= prog.fun_num)
        return -1;
    
    fun = &prog.code[index];
    state.ip = 0;
    num_marks = get_marks(fun, &marks);

    while (state.ip < fun->code_len) {
        if (single_step(fun, marks, num_marks) != 0)
            return -1;
    }

    return 0;
}