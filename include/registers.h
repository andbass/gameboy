#pragma once
/*
 * Registers used by the stripped down Z80 inside the GameBoy
 */

#include "types.h"

#define ZERO_FLAG (1 << 7)
#define SUBTRACT_FLAG (1 << 6)
#define HALF_CARRY_FLAG (1 << 5)
#define CARRY_FLAG (1 << 4)

// Registers
typedef struct {
    union {
        u16 af;
        struct {
            u8 f;
            u8 a;
        };
    };

    union {
        u16 bc;
        struct {
            u8 c;
            u8 b;
        };
    };

    union {
        u16 de;
        struct {
            u8 e;
            u8 d;
        };
    };

    union {
        u16 hl;
        struct {
            u8 l;
            u8 h;
        };
    };

    u16 sp;
    u16 pc;
} Registers;
