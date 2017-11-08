#include "profiler.h"
#include "jitter.h"
#include "execution.h"

#define num_call_max 256

size_t called[user_def_funs];
size_t called_total;
int disabled = 0;

void disable_jit() {
    disabled = 1;
}

void update_call_nums(size_t index) {
    called[index]++;
    called_total++;

    while (called_total > num_call_max) {
        size_t i;
        for (i = 0; i < user_def_funs; i++)
            called[i] = called[i] / 2;
        
        called_total = called_total / 2;
    }
}

int should_jit(size_t index) {
    if (disabled)
        return 0;
    
    if (index >= user_def_funs)
        return 0;

    update_call_nums(index);

    if (is_jitted(index))
        return 1;
    
    if (called[index] > (num_call_max / 8))
        return 1;
    else
        return 0;
}