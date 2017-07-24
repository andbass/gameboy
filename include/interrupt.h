#pragma once

typedef enum {
    VBLANK = 1 << 0,
    LCDC = 1 << 1,
    TIMER = 1 << 2,
    SERIAL = 1 << 3,
    INPUT = 1 << 4,
} Interrupt;
