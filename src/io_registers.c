#include "io_registers.h"
#include "gb.h"

void io_registers_init(GameBoy* gb) {
    gb->io_reg.joypad_info = gb->mem + 0xFF00; 

    gb->serial_data = gb->mem + 0xFF01;
    gb->serial_control = gb->mem + 0xFF02;

    gb->divider = gb->mem + 0xFF04;

    gb->timer_counter = gb->mem + 0xFF05;
    gb->timer_modulo = gb->mem + 0xFF06;
    gb->timer_control = gb->mem + 0xFF07;

    gb->interrupt_flag = gb->mem + 0xFF0F;
}
