#include <stdio.h>

#include "gb.h"
#include "rom.h"
#include "cpu.h"
#include "instr.h"

int main(int argc, char** argv) {
    if (argc < 2) {
        return 1;
    }

    char* rom_path = argv[1];
    FILE* rom_fp = fopen(rom_path, "r");

    if (rom_fp == NULL) {
        return 1;
    }

    GameBoy gb;
    gb_init(&gb);

    cpu_info(&gb, stdout);
    
    printf("HAHAH: %#lx\n", gb.io_reg_mem - gb.mem);

    return 0;
}
