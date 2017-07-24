#include "io_registers.h"
#include "gb.h"

void io_registers_init(GameBoy* gb) {
    gb->io_reg.joypad_info = gb->mem + 0xFF00; 

    gb->io_reg.serial_data = gb->mem + 0xFF01;
    gb->io_reg.serial_control = gb->mem + 0xFF02;

    gb->io_reg.divider = gb->mem + 0xFF04;

    gb->io_reg.timer_counter = gb->mem + 0xFF05;
    gb->io_reg.timer_modulo = gb->mem + 0xFF06;
    gb->io_reg.timer_control = gb->mem + 0xFF07;

    gb->io_reg.interrupt_flag = gb->mem + 0xFF0F;

    gb->io_reg.lcd_control = gb->mem + 0xFF40;
    gb->io_reg.lcd_status = gb->mem + 0xFF41;

    gb->io_reg.scroll_y = gb->mem + 0xFF42;
    gb->io_reg.scroll_x = gb->mem + 0xFF43;

    gb->io_reg.lcdc_y = gb->mem + 0xFF44;
    gb->io_reg.lcdc_y_compare = gb->mem + 0xFF45;

    gb->io_reg.dma_transfer = gb->mem + 0xFF46;

    gb->io_reg.bg_window_palette = gb->mem + 0xFF47;
    gb->io_reg.object_palette_0 = gb->mem + 0xFF48;
    gb->io_reg.object_palette_1 = gb->mem + 0xFF49;

    gb->io_reg.window_x = gb->mem + 0xFF4A;
    gb->io_reg.window_y = gb->mem + 0xFF4B;
}
