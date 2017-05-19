#pragma once
/*
 * Declares functions to load in ROMs into memory
 */

#include <stdio.h>
#include "gb.h"

void load_rom(GameBoy* gb, FILE* rom_fp);
