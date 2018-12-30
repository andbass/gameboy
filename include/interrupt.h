#pragma once
/*
 * Constants and enums related to the various interrupts supported by the Gameboy
 */

#include "cpu.h"

#define CYCLES_PER_VBLANK (CYCLES_PER_SECOND / 60)

typedef enum Interrupt {
    VBlank = 1 << 0,
    LCDC = 1 << 1,
    Timer = 1 << 2,
    Serial = 1 << 3,
    Input = 1 << 4,
} Interrupt;


