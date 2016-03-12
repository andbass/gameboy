#pragma once
/*
 * Definition of GameBoy struct, which ties together all the components and defines the memory map
 */

#include "types.h"
#include "registers.h"

typedef struct {
    // Memory map
    union {
        struct {
            u8 rom_bank0[0x3FFF + 1];
            u8 rom_bankN[0x7FFF - 0x4000 + 1];

            u8 vram[0x9FFF - 0x8000 + 1];
            u8 ram[0xBFFF - 0xA000 + 1];

            u8 wram0[0xCFFF - 0xC000 + 1];
            u8 wram1[0xDFFF - 0xD000 + 1];

            u8 echo[0xFDFF - 0xE000 + 1];

            u8 spriteTable[0xFE9F - 0xFE00 + 1];
            u8 unused[0xFEFF -  0xFEA0 + 1]; // TODO save memory by removing padding

            u8 io[0xFF7F - 0xFF00 + 1];
            u8 hram[0xFFFE - 0xFF80 + 1];

            u8 interruptEnable;
        } map;

        u8 mem[0xFFFF + 1];
    };

    Registers reg;
} GameBoy;
