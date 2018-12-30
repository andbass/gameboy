#pragma once
/*
 * Declares functions for running the GameBoy's CPU and fetching data
 * Also defines constants related to CPU
 */

#include <stdio.h>
#include "gb.h"

#define CYCLES_PER_SECOND 4194304

void cpu_init(GameBoy*);

// Dumps out CPU info
void cpu_info(GameBoy*, FILE*);

// Executes a single opcode, returns number of cycles used
u8 execute_opcode(GameBoy*);

// Executes a opcode that is prefixed by 0xCB, 
// It is invoked by `execute` when the prefix byte is encountered
u8 execute_cb_prefixed_opcode(GameBoy*);

// Advances the PC and returns the read byte
u8 next_u8(GameBoy*);

// Advances the PC twice and returns the read 2 byte number (little endianness)
u16 next_u16(GameBoy*);

// Writes a 2 byte number (in little endian format) to a given address
// There is no need for a `write_u8`, as a simple C assignment suffices
void write_u16(GameBoy*, u16 addr, u16 value);