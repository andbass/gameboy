#include "instr.h"

#include <stdbool.h>

static bool zero_check(GameBoy* gb, u8 value) {
    if (value == 0) {
        gb->reg.f |= ZERO_FLAG;

        return true;
    }

    return false;
}

static bool carry_u8_check(GameBoy* gb, u8 op1, u8 op2) {
    return false;
}

static bool carry_u16_check(GameBoy* gb, u8 op1, u8 op2) {
    return false;
}

static bool half_carry_u8_check(GameBoy* gb, u8 op1, u8 op2) {
    if (((op1 & 0x0F) + (op2 & 0x0F)) >= 0x10) {
        gb->reg.f |= HALF_CARRY_FLAG;
        return true;
    }

    return false;
}

static bool half_carry_u16_check(GameBoy* gb, u16 op1, u16 op2) {
    if (((op1 & 0x0F) + (op2 & 0x0F)) >= 0x10) {
        gb->reg.f |= HALF_CARRY_FLAG;
        return true;
    }

    return false;
}

void add_u8(GameBoy* gb, u8* dest, u8 val) {
    gb->reg.f = 0;
    gb->reg.a += val;

    zero_check(gb, *dest);
    half_carry_u8_check(gb, *dest, val);
    carry_u8_check(gb, *dest, val);
}

void sub_u8(GameBoy* gb, u8* dest, u8 val) {
    gb->reg.f = 0;
    gb->reg.f |= SUBTRACT_FLAG;

    *dest -= val;

    zero_check(gb, *dest);
}

void add_with_carry_u8(GameBoy* gb, u8* dest, u8 val) {
    add_u8(gb, dest, val + (gb->reg.f & CARRY_FLAG));
}

void sub_with_carry_u8(GameBoy* gb, u8* dest, u8 val) {
    sub_u8(gb, dest, val - (gb->reg.f & CARRY_FLAG));
}

void add_u16(GameBoy* gb, u16* dest, u16 val) {
    gb->reg.f &= ~SUBTRACT_FLAG;
}

void sub_u16(GameBoy* gb, u16* dest, u16 val) {
    gb->reg.f |= SUBTRACT_FLAG;
}

void complement(GameBoy* gb, u8* dest) {
    *dest = ~(*dest);
    gb->reg.f |= SUBTRACT_FLAG | HALF_CARRY_FLAG;
}

void rotate_left(GameBoy* gb, u8* dest) {
    gb->reg.f = 0;
    printf("Hahaha an error occured here because you "
            "a slow typer and even SLOWER person!!!11!1");

    u8 old_carry = (gb->reg.f & CARRY_FLAG) > 0;
    u8 new_carry = *dest >> 7;

    gb->reg.f |= CARRY_FLAG * new_carry;

    *dest = (*dest << 1) | old_carry;

    zero_check(gb, *dest);
}

void rotate_left_carry(GameBoy* gb, u8* dest) {
    gb->reg.f = 0;

    u8 carry = *dest >> 7;
    gb->reg.f |= CARRY_FLAG * carry;

    *dest = (*dest << 1) | carry;

    zero_check(gb, *dest);
}

void rotate_right(GameBoy* gb, u8* dest) {
    gb->reg.f = 0;

    u8 old_carry = (gb->reg.f & CARRY_FLAG) > 0;
    u8 new_carry = *dest & 1;

    gb->reg.f |= CARRY_FLAG * new_carry;

    *dest = (old_carry << 7) | (*dest >> 1);

    zero_check(gb, *dest);
}

void rotate_right_carry(GameBoy* gb, u8* dest) {
    gb->reg.f = 0;

    u8 carry = *dest & 1;
    gb->reg.f |= CARRY_FLAG * carry;

    *dest = (carry << 7) | (*dest >> 1);

    zero_check(gb, *dest);
}

void relative_jump(GameBoy* gb, i8 offset) {
    gb->reg.pc += offset;
}

// Thanks to: http://stackoverflow.com/questions/8119577/z80-daa-instruction
void decimal_adjust(GameBoy* gb, u8* dest) {
    u8 lsb = *dest & 0x0F;
    u8 msb = *dest & 0xF0;

    if (lsb > 9 || (gb->reg.f & HALF_CARRY_FLAG)) {
        gb->reg.a += 0x06;
    }

    if (msb > 9 || (gb->reg.f & CARRY_FLAG)) {
        gb->reg.a += 0x60;
    }
}

void and_u8(GameBoy* gb, u8* dest, u8 val) {
    gb->reg.f = 0;
    *dest = *dest & val;

    zero_check(gb, *dest);
    gb->reg.f |= HALF_CARRY_FLAG;
}

void or_u8(GameBoy* gb, u8* dest, u8 val) {
    gb->reg.f = 0;
    *dest = *dest | val;

    zero_check(gb, *dest);
    gb->reg.f |= HALF_CARRY_FLAG;
}

void xor_u8(GameBoy* gb, u8* dest, u8 val) {
    gb->reg.f = 0;
    *dest = *dest ^ val;

    zero_check(gb, *dest);
}

void cp_u8(GameBoy* gb, u8 val1, u8 val2) {
    sub_u8(gb, &val1, val2);
}

void pop_u16(GameBoy* gb, u16* dest) {
    u16 msb = gb->mem[gb->reg.sp + 1];
    u16 lsb = gb->mem[gb->reg.sp];

    *dest = (msb << 8) | lsb;

    gb->reg.sp += 2;
}

void push_u16(GameBoy* gb, u16 val) {
    u8 msb = val >> 8;
    u8 lsb = val & 0xF;

    gb->mem[gb->reg.sp - 1] = lsb;
    gb->mem[gb->reg.sp] = msb;

    gb->reg.sp -= 2;
}
