#pragma once
/*
 * Emulator for stripped down Z80 inside GameBoy
 */

#include <stdbool.h>

#include "types.h"

typedef struct {
    // Registers
    union {
        u16 af;
        struct {
            u8 a;
            u8 f;
        };
    };

    union {
        u16 bc;
        struct {
            u8 b;
            u8 c;
        };
    };

    union {
        u16 de;
        struct {
            u8 d;
            u8 e;
        };
    };

    union {
        u16 hl;
        struct {
            u8 h;
            u8 l;
        };
    };

    u8 sp;
    u8 pc;
} Registers;
