#ifndef DECODER_H
#define DECODER_H

#include <stdint.h>
#include <sys/types.h>

#include "imp.h"

size_t decode(uint8_t *buf, size_t num, imp_ins_dec *dec_arr);

#endif