#pragma once

#include "types.h"
#include "registers.h"

typedef enum {
    Normal,
    LowPower,
    Stop,
} Mode;

typedef struct {
    union {
        struct {
            u8 rom_bank0    [0x3FFF - 0x0000 + 1];
            u8 rom_bankN    [0x7FFF - 0x4000 + 1];

            u8 vram         [0x9FFF - 0x8000 + 1];
            u8 external_ram [0xBFFF - 0xA000 + 1];

            u8 wram0        [0xCFFF - 0xC000 + 1];
            u8 wramN        [0xDFFF - 0xD000 + 1];

            u8 echo         [0xFDFF - 0xE000 + 1]; // TODO? implement echo memory, maybe.  prolly not
            u8 sprite_table [0xFE9F - 0xFE00 + 1];
            u8 unused       [0xFEFF - 0xFEA0 + 1];

            u8 io_reg       [0xFF7F - 0xFF00 + 1];
            u8 hram         [0xFFFE - 0xFF80 + 1];
            u8 interrupt_enable_flags;
        };

        u8 mem[0xFFFF + 1];
    };
    bool interrupt_master_enable;
    Registers reg;
    Mode mode;
} GameBoy;

void gb_init(GameBoy*);
