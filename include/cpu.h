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
u8 execute(GameBoy*);

// Advances the PC and returns the read byte
u8 next_byte(GameBoy*);

// Advances the PC twice and returns the read word (2 bytes)
u16 next_word(GameBoy*);
