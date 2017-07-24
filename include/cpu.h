#pragma once
/*
 * Declares functions for running the GameBoy's CPU and fetching data
 */

#include <stdio.h>
#include "gb.h"

void cpu_init(GameBoy*);

// Dumps out CPU info
void cpu_info(GameBoy*, FILE*);

// Executes a single opcode, returns number of cycles used
u8 execute_opcode(GameBoy*);

// Executes a opcode that is prefixed by 0xCB, 
// It is invoked by `execute` when the prefix byte is encountered
u8 execute_cb_prefixed_opcode(GameBoy*);

// Advances the PC and returns the read byte
u8 next_byte(GameBoy*);

// Advances the PC twice and returns the read word (2 bytes)
u16 next_word(GameBoy*);
