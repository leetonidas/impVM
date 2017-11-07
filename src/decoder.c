#include <netinet/in.h>

#include "decoder.h"

size_t decode(uint8_t *buf, size_t num, imp_ins_dec *dec) {
    size_t bitPos;
    size_t i;
    uint32_t tmp;

    bitPos = 0;
    for (i = 0; i < num; i++) {
        tmp = (htonl(*((uint32_t *) (&buf[bitPos / 8]))) << (bitPos % 8)) >> 21;
        switch (tmp >> 5) {
        case 0x0 ... 0x7:
            dec[i].mnemonic = IMP_JZ;
            dec[i].operand = tmp & 0xFF;
            break;
        case 0x8 ... 0xF:
            dec[i].mnemonic = IMP_JMP;
            dec[i].operand = tmp & 0xFF;
            break;
        case 0x10 ... 0x17:
            dec[i].mnemonic = IMP_CAL;
            dec[i].operand = tmp & 0xFF;
            break;
        case 0x18 ... 0x1F:
            dec[i].mnemonic = IMP_MRK;
            dec[i].operand = tmp & 0xFF;
            break;
        case 0x20 ... 0x23:
            dec[i].mnemonic = IMP_LD;
            dec[i].operand = tmp & 0x7F;
            break;
        case 0x24 ... 0x27:
            dec[i].mnemonic = IMP_ST;
            dec[i].operand = tmp & 0x7F;
            break;
        case 0x28 ... 0x2B:
            dec[i].mnemonic = IMP_LDR;
            dec[i].operand = tmp & 0x7F;
            break;
        case 0x2C ... 0x2F:
            dec[i].mnemonic = IMP_STR;
            dec[i].operand = tmp & 0x7F;
            break;
        case 0x30 ... 0x37:
            dec[i].mnemonic = IMP_IMM;
            dec[i].operand = tmp & 0xFF;
            break;
        case 0x38 ... 0x39:
            dec[i].mnemonic = IMP_SHL;
            dec[i].operand = tmp & 0x3F;
            break;
        case 0x3A ... 0x3B:
            dec[i].mnemonic = IMP_SHR;
            dec[i].operand = tmp & 0x3F;
            break;
        case 0x3C:
            dec[i].mnemonic = IMP_NOT;
            break;
        case 0x3D:
            dec[i].mnemonic = IMP_AND;
            break;
        case 0x3E:
            dec[i].mnemonic = IMP_OR;
            break;
        case 0x3F:
            dec[i].mnemonic = IMP_GE;
            break;
        default:
            return 0;
        }
        if ((tmp >> 5) >= 0x3C)
            bitPos += 6;
        else
            bitPos += 11;
    }
    return i;
}