#pragma once
/*
 * Registers used by the stripped down Z89 inside the GameBoy
 */

#include <stdbool.h>
#include "types.h"

#define ZERO_FLAG (1 << 6)
#define SUBTRACT_FLAG (1 << 5)
#define HALF_CARRY_FLAG (1 << 4)
#define CARRY_FLAG (1 << 3)

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

    u16 sp;
    u16 pc;
} Registers;
