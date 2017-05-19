#include "gb.h"
#include "cpu.h"

void gb_init(GameBoy* gb) {
    gb->mode = Normal;

    cpu_init(gb);
}
