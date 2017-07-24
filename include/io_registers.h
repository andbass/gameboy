#pragma once
/*
 * Stores a number of pointers into the IO Registers buffer within the GameBoy's memory
 * Used to easily access the different IO Registers without manually indexing into the memory buffer
 *
 * These registers are controlled by a variety of subsystems, including `timer.c`, `lcd.c`, and others
 */

#include "types.h"

typedef struct IORegisters {
    u8* joypad_info; // P1: Button / DPad input

    u8* serial_data; // SB
    u8* serial_control; // SC

    u8* divider; // DIV

    u8* timer_counter; // TIMA
    u8* timer_modulo; // TMA: Data this is loaded into TIMA when it overflows
    u8* timer_control; // TAC: Controls timer behavior

    u8* interrupt_flag; // IF: Tells user which interrupt was requested

    // TODO add sound registers  

    u8* lcd_control; // LCDC: General graphics settings
    u8* lcd_status; // STAT: Tells user about LCD state and info

    u8* scroll_y; // SCY: Scroll background's Y position
    u8* scroll_x; // SCX: Scroll background's X position

    u8* lcdc_y; // LY: Indiciates vertical line to which the present data is transferred to the LCD driver
    u8* lcdc_y_compare; // LYC: When this value equals LY, the LCD's coincident flag is set

    u8* dma_transfer; // DMA: Used to initiate DMA into OAM and specify starting address

    u8* bg_window_palette; // BGP: BG and window palette, used to specify shades of grey
    u8* object_palette_0; // OBP0: First sprite palette 
    u8* object_palette_1; // OBP1: Second sprite palette

    u8* window_x; // WX
    u8* window_y; // WY
} IORegisters;

// Looks a bit funky here, had to forward declare to avoid circular dependency
struct GameBoy;
void io_registers_init(struct GameBoy*);
