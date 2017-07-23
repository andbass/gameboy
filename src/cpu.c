#include "gb.h"
#include "cpu.h"
#include "instr.h"

#include <string.h>

void cpu_init(GameBoy* gb) {
    memset(&gb->reg, 0, sizeof(gb->reg));
    gb->reg.pc = 0x100;
}

void cpu_info(GameBoy* gb, FILE* fp) {
    fprintf(fp, "Regs:\n");
    fprintf(fp, "\tA = 0x%x\n", gb->reg.a);
    fprintf(fp, "\tF = 0x%x\n", gb->reg.f);

    fprintf(fp, "\tB = 0x%x\n", gb->reg.b);
    fprintf(fp, "\tC = 0x%x\n", gb->reg.c);

    fprintf(fp, "\tD = 0x%x\n", gb->reg.d);
    fprintf(fp, "\tE = 0x%x\n", gb->reg.e);

    fprintf(fp, "\tH = 0x%x\n", gb->reg.h);
    fprintf(fp, "\tL = 0x%x\n", gb->reg.l);

    fprintf(fp, "\tSP = 0x%x\n", gb->reg.sp);
    fprintf(fp, "\tPC = 0x%x\n", gb->reg.pc);
}

u8 next_u8(GameBoy* gb) {
    u8 byte = gb->mem[gb->reg.pc];
    gb->reg.pc++;

    return byte;
}

u16 next_u16(GameBoy* gb) {
    u8 lo = next_u8(gb);
    u8 hi = next_u8(gb);

    return ((u16)hi << 8) | (u16)lo;
}

// Opcodes come from: http://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
// nn -> next 2 byte number
// n -> next byte
// s -> next signed byte
// ss -> next signed 2 byte number
u8 execute(GameBoy* gb) {
    u8 opcode = next_u8(gb);

    switch (opcode & 0xF0) {
    case 0x00:
        switch (opcode & 0x0F) {
        case 0x00: // NOP
            return 4;
        case 0x01: // LD BC, nn
            gb->reg.bc = next_u16(gb);
            return 12;
        case 0x02: // LD (BC), A
            gb->mem[gb->reg.bc] = gb->reg.a;
            return 8;
        case 0x03: // INC BC
            gb->reg.bc++;
            return 8;
        case 0x04: // INC B
            add_u8(gb, &gb->reg.b, 1);
            return 4;
        case 0x05: // DEC B
            sub_u8(gb, &gb->reg.b, 1);
            return 4;
        case 0x06: // LD B, n
            gb->reg.b = next_u8(gb);
            return 4;
        case 0x07: // RLCA
            rotate_left_carry(gb, &gb->reg.a);
            return 4;
        case 0x08: // LD (nn), SP
            gb->mem[next_u8(gb)] = gb->reg.sp;
            return 4;
        case 0x09: // ADD HL, BC
            add_u16(gb, &gb->reg.hl, gb->reg.bc);
            return 8;
        case 0x0A: // LD A, (BC)
            gb->reg.a = gb->mem[gb->reg.bc];
            return 8;
        case 0x0B: // DEC BC
            sub_u16(gb, &gb->reg.bc, 1);
            return 8;
        case 0x0C: // INC C
            add_u8(gb, &gb->reg.c, 1);
            return 4;
        case 0x0D: // DEC C
            sub_u8(gb, &gb->reg.c, 1);
            return 4;
        case 0x0E: // LD C, n
            gb->reg.c = next_u8(gb);
            return 8;
        case 0x0F: // RRCA
            rotate_right_carry(gb, &gb->reg.a);
            return 4;
        }
        break;
    case 0x10:
        switch (opcode & 0x0F) {
        case 0x00: // STOP 0
            // TODO do this
            return 4;
        case 0x01: // LD DE, d16
            gb->reg.de = next_u16(gb);
            return 12;
        case 0x02: // LD (DE), A
            gb->mem[gb->reg.de] = gb->reg.a;
            return 8;
        case 0x03: // INC DE
            gb->reg.de++;
            return 8;
        case 0x04: // INC D
            add_u8(gb, &gb->reg.d, 1);
            return 4;
        case 0x05: // DEC D
            sub_u8(gb, &gb->reg.d, 1);
            return 4;
        case 0x06: // LD D, n
            gb->reg.d = next_u8(gb);
            return 8;
        case 0x07: // RLA
            rotate_left(gb, &gb->reg.a);
            return 4;
        case 0x08: // JR s
            relative_jump(gb, next_u8(gb));
            return 12;
        case 0x09: // ADD HL, DE
            add_u16(gb, &gb->reg.hl, gb->reg.de);
            return 8;
        case 0x0A: // LD A, (DE)
            gb->reg.a = gb->mem[gb->reg.de];
            return 8;
        case 0x0B: // DEC DE
            gb->reg.de--;
            return 8;
        case 0x0C: // INC E
            add_u8(gb, &gb->reg.e, 1);
            return 4;
        case 0x0D: // DEC E
            sub_u8(gb, &gb->reg.e, 1);
            return 4;
        case 0x0E: // LD E, n
            gb->reg.e = next_u8(gb);
            return 8;
        case 0x0F: // RRA
            rotate_right(gb, &gb->reg.a);
            return 4;
        }
        break;
    case 0x20:
        switch (opcode & 0x0F) {
        case 0x00: // JR NZ, s
            if (!(gb->reg.f & ZERO_FLAG)) {
                relative_jump(gb, next_u8(gb));
                return 12;
            }

            return 8;
        case 0x01: // LD HL, nn
            gb->reg.hl = next_u16(gb);
            return 12;
        case 0x02: // LD (HL+), A
            gb->mem[gb->reg.hl] = gb->reg.a;
            gb->reg.hl++;
            return 8;
        case 0x03: // INC HL
            gb->reg.hl++;
            return 8;
        case 0x04: // INC H
            add_u8(gb, &gb->reg.h, 1);
            return 4;
        case 0x05: // DEC H
            sub_u8(gb, &gb->reg.h, 1);
            return 4;
        case 0x06: // LD H, n
            gb->reg.h = next_u8(gb);
            return 8;
        case 0x07: // DAA
            decimal_adjust(gb, &gb->reg.a);
            return 4;
        case 0x08: // JR Z, s
            if (gb->reg.f & ZERO_FLAG) {
                relative_jump(gb, next_u8(gb));
                return 12;
            }

            return 8;
        case 0x09: // ADD HL, HL
            add_u16(gb, &gb->reg.hl, gb->reg.hl);
            return 8;
        case 0x0A: // LD A, (HL+)
            gb->reg.a = gb->mem[gb->reg.hl++];
            return 8;
        case 0x0B: // DEC HL
            gb->reg.hl--;
            return 8;
        case 0x0C: // INC L
            add_u8(gb, &gb->reg.l, 1);
            return 4;
        case 0x0D: // DEC L
            sub_u8(gb, &gb->reg.l, 1);
            return 4;
        case 0x0E: // LD L, n
            gb->reg.l = next_u8(gb);
            return 8;
        case 0x0F: // CPL
            complement(gb, &gb->reg.a);
            return 4;
        }
        break;
    case 0x30:
        switch (opcode & 0x0F) {
        case 0x00: // JR NC, r
            if (!(gb->reg.f & CARRY_FLAG)) {
                relative_jump(gb, next_u8(gb));
                return 12;
            }

            return 8;
        case 0x01: // LD (SP), nn
            gb->reg.sp = next_u16(gb);
            return 12;
        case 0x02: // LD (HL-), A
            gb->mem[gb->reg.hl] = gb->reg.a;
            gb->reg.hl--;
            return 8;
        case 0x03: // INC SP
            gb->reg.sp++;
            return 8;
        case 0x04: // INC (HL)
            gb->mem[gb->reg.hl]++;
            return 12;
        case 0x05: // DEC (HL)
            gb->mem[gb->reg.hl]--;
            return 12;
        case 0x06: // LD (HL), n
            gb->mem[gb->reg.hl] = next_u8(gb);
            return 12;
        case 0x07: // SCF
            gb->reg.f |= CARRY_FLAG;
            return 0;
        case 0x08: // JR C, r
            if (gb->reg.f & CARRY_FLAG) {
                relative_jump(gb, next_u8(gb));
                return 12;
            }

            return 8;
        case 0x09: // ADD HL, SP
            add_u16(gb, &gb->reg.hl, gb->reg.sp);
            return 8;
        case 0x0A: // LD A, (HL-)
            gb->reg.a = gb->mem[gb->reg.hl];
            gb->reg.hl--;
            return 8;
        case 0x0B: // DEC SP
            gb->reg.sp--;
            return 8;
        case 0x0C: // INC A
            add_u8(gb, &gb->reg.a, 1);
            return 4;
        case 0x0D: // DEC A
            gb->reg.a--;
            return 4;
        case 0x0E: // LD A, n
            gb->reg.a = next_u8(gb);
            return 8;
        case 0x0F: // CCF
            gb->reg.f ^= CARRY_FLAG;
            return 4;
        }
        break;
    case 0x40:
        switch (opcode & 0x0F) {
        case 0x00: // LD B, B
            return 4;
        case 0x01: // LD B, C
            gb->reg.b = gb->reg.c;
            return 4;
        case 0x02: // LD B, D
            gb->reg.b = gb->reg.d;
            return 4;
        case 0x03: // LD B, E
            gb->reg.b = gb->reg.e;
            return 4;
        case 0x04: // LD B, H
            gb->reg.b = gb->reg.h;
            return 4;
        case 0x05: // LD B, L
            gb->reg.b = gb->reg.l;
            return 4;
        case 0x06: // LD B, (HL)
            gb->reg.b = gb->mem[gb->reg.hl];
            return 8;
        case 0x07: // LD B, A
            gb->reg.a = gb->reg.b;
            return 4;
        case 0x08: // LD C, B
            gb->reg.c = gb->reg.b;
            return 4;
        case 0x09: // LD C, C
            return 4;
        case 0x0A: // LD C, D
            gb->reg.c = gb->reg.d;
            return 4;
        case 0x0B: // LD C, E
            gb->reg.c = gb->reg.e;
            return 4;
        case 0x0C: // LD C, H
            gb->reg.c = gb->reg.h;
            return 4;
        case 0x0D: // LD C, L
            gb->reg.c = gb->reg.l;
            return 4;
        case 0x0E: // LD C, (HL)
            gb->reg.c = gb->mem[gb->reg.hl];
            return 8;
        case 0x0F: // LD C, A
            gb->reg.c = gb->reg.a;
            return 4;
        }
        break;
    case 0x50:
        switch (opcode & 0x0F) {
        case 0x00: // LD D, B
            gb->reg.d = gb->reg.b;
            return 4;
        case 0x01: // LD D, C
            gb->reg.d = gb->reg.c;
            return 4;
        case 0x02: // LD D, D
            return 4;
        case 0x03: // LD D, E
            gb->reg.d = gb->reg.e;
            return 4;
        case 0x04: // LD D, H
            gb->reg.d = gb->reg.h;
            return 4;
        case 0x05: // LD D, L
            gb->reg.d = gb->reg.l;
            return 4;
        case 0x06: // LD D, (HL)
            gb->reg.d = gb->mem[gb->reg.hl];
            return 12;
        case 0x07: // LD D, A
            gb->reg.d = gb->reg.a;
            return 4;
        case 0x08: // LD E, B
            gb->reg.e = gb->reg.b;
            return 4;
        case 0x09: // LD E, C
            gb->reg.e = gb->reg.c;
            return 4;
        case 0x0A: // LD E, D
            gb->reg.e = gb->reg.d;
            return 4;
        case 0x0B: // LD E, E
            return 4;
        case 0x0C: // LD E, H
            gb->reg.e = gb->reg.h;
            return 4;
        case 0x0D: // LD E, L
            gb->reg.e = gb->reg.l;
            return 4;
        case 0x0E: // LD E, (HL)
            gb->reg.e = gb->mem[gb->reg.hl];
            return 8;
        case 0x0F: // LD E, A
            gb->reg.e = gb->reg.a;
            return 4;
        }
        break;
    case 0x60:
        switch (opcode & 0x0F) {
        case 0x00: // LD H, B
            gb->reg.h = gb->reg.b;
            return 4;
        case 0x01: // LD H, C
            gb->reg.h = gb->reg.c;
            return 4;
        case 0x02: // LD H, D
            gb->reg.h = gb->reg.d;
            return 4;
        case 0x03: // LD H, E
            gb->reg.h = gb->reg.e;
            return 4;
        case 0x04: // LD H, H
            return 4;
        case 0x05: // LD H, L
            gb->reg.h = gb->reg.l;
            return 4;
        case 0x06: // LD H, (HL)
            gb->reg.h = gb->mem[gb->reg.hl];
            return 8;
        case 0x07: // LD H, A
            gb->reg.h = gb->reg.a;
            return 4;
        case 0x08: // LD L, B
            gb->reg.l = gb->reg.b;
            return 4;
        case 0x09: // LD L, C
            gb->reg.l = gb->reg.c;
            return 4;
        case 0x0A: // LD L, D
            gb->reg.l = gb->reg.d;
            return 4;
        case 0x0B: // LD L, E
            gb->reg.l = gb->reg.e;
            return 4;
        case 0x0C: // LD L, H
            gb->reg.l = gb->reg.h;
            return 4;
        case 0x0D: // LD L, L
            return 4;
        case 0x0E: // LD L, (HL)
            gb->reg.l = gb->mem[gb->reg.hl];
            return 8;
        case 0x0F: // LD L, A 
            gb->reg.l = gb->reg.a;
            return 4;
        }
        break;
    case 0x70:
        switch (opcode & 0x0F) {
        case 0x00: // LD (HL), B
            gb->mem[gb->reg.hl] = gb->reg.b;
            return 8;
        case 0x01: // LD (HL), C
            gb->mem[gb->reg.hl] = gb->reg.c;
            return 8;
        case 0x02: // LD (HL), D
            gb->mem[gb->reg.hl] = gb->reg.d;
            return 8;
        case 0x03: // LD (HL), E
            gb->mem[gb->reg.hl] = gb->reg.e;
            return 8;
        case 0x04: // LD (HL), H
            gb->mem[gb->reg.hl] = gb->reg.h;
            return 8;
        case 0x05: // LD (HL), L
            gb->mem[gb->reg.hl] = gb->reg.l;
            return 8;
        case 0x06: // HALT
            gb->mode = Stop;
            return 4;
        case 0x07: // LD (HL), A
            gb->mem[gb->reg.hl] = gb->reg.a;
            return 8;
        case 0x08: // LD A, B
            gb->reg.a = gb->reg.b;
            return 4;
        case 0x09: // LD A, C
            gb->reg.a = gb->reg.c;
            return 4;
        case 0x0A: // LD A, D
            gb->reg.a = gb->reg.d;
            return 4;
        case 0x0B: // LD A, E
            gb->reg.a = gb->reg.e;
            return 4;
        case 0x0C: // LD A, H
            gb->reg.a = gb->reg.h;
            return 4;
        case 0x0D: // LD A, L
            gb->reg.a = gb->reg.l;
            return 4;
        case 0x0E: // LD A, (HL)
            gb->reg.a = gb->mem[gb->reg.hl];
            return 8;
        case 0x0F: // LD A, A
            return 4;
        }
        break;
    case 0x80:
        switch (opcode & 0x0F) {
        case 0x00: // ADD A, B
            add_u8(gb, &gb->reg.a, gb->reg.b);
            return 4;
        case 0x01: // ADD A, C
            add_u8(gb, &gb->reg.a, gb->reg.c);
            return 4;
        case 0x02: // ADD A, D
            add_u8(gb, &gb->reg.a, gb->reg.d);
            return 4;
        case 0x03: // ADD A, E
            add_u8(gb, &gb->reg.a, gb->reg.e);
            return 4;
        case 0x04: // ADD A, H
            add_u8(gb, &gb->reg.a, gb->reg.h);
            return 4;
        case 0x05: // ADD A, L
            add_u8(gb, &gb->reg.a, gb->reg.l);
            return 4;
        case 0x06: // ADD A, (HL)
            add_u8(gb, &gb->reg.a, gb->mem[gb->reg.hl]);
            return 4;
        case 0x07: // ADD A, A
            add_u8(gb, &gb->reg.a, gb->reg.a);
            return 4;
        case 0x08: // ADC A, B
            add_with_carry_u8(gb, &gb->reg.a, gb->reg.b);
            return 4;
        case 0x09: // ADC A, C
            add_with_carry_u8(gb, &gb->reg.a, gb->reg.c);
            return 4;
        case 0x0A: // ADC A, D
            add_with_carry_u8(gb, &gb->reg.a, gb->reg.d);
            return 4;
        case 0x0B: // ADC A, E
            add_with_carry_u8(gb, &gb->reg.a, gb->reg.e);
            return 4;
        case 0x0C: // ADC A, H
            add_with_carry_u8(gb, &gb->reg.a, gb->reg.h);
            return 4;
        case 0x0D: // ADC A, L
            add_with_carry_u8(gb, &gb->reg.a, gb->reg.l);
            return 4;
        case 0x0E: // ADC A, (HL)
            add_with_carry_u8(gb, &gb->reg.a, gb->mem[gb->reg.hl]);
            return 8;
        case 0x0F: // ADC A, A
            add_with_carry_u8(gb, &gb->reg.a, gb->reg.a);
            return 4;
        }
        break;
    case 0x90:
        switch (opcode & 0x0F) {
        case 0x00: // SUB B
            sub_u8(gb, &gb->reg.a, gb->reg.b);
            return 4;
        case 0x01: // SUB C
            sub_u8(gb, &gb->reg.a, gb->reg.c);
            return 4;
        case 0x02: // SUB D
            sub_u8(gb, &gb->reg.a, gb->reg.d);
            return 4;
        case 0x03: // SUB E
            sub_u8(gb, &gb->reg.a, gb->reg.e);
            return 4;
        case 0x04: // SUB H
            sub_u8(gb, &gb->reg.a, gb->reg.h);
            return 4;
        case 0x05: // SUB L
            sub_u8(gb, &gb->reg.a, gb->reg.l);
            return 4;
        case 0x06: // SUB (HL)
            sub_u8(gb, &gb->reg.a, gb->mem[gb->reg.hl]);
            return 8;
        case 0x07: // SUB A
            sub_u8(gb, &gb->reg.a, gb->reg.a);
            return 4;
        case 0x08: // SBC A, B
            sub_with_carry_u8(gb, &gb->reg.a, gb->reg.b);
            return 4;
        case 0x09: // SBC A, C
            sub_with_carry_u8(gb, &gb->reg.a, gb->reg.c);
            return 4;
        case 0x0A: // SBC A, D
            sub_with_carry_u8(gb, &gb->reg.a, gb->reg.d);
            return 4;
        case 0x0B: // SBC A, E
            sub_with_carry_u8(gb, &gb->reg.a, gb->reg.e);
            return 4;
        case 0x0C: // SBC A, H
            sub_with_carry_u8(gb, &gb->reg.a, gb->reg.h);
            return 4;
        case 0x0D: // SBC A, L
            sub_with_carry_u8(gb, &gb->reg.a, gb->reg.l);
            return 4;
        case 0x0E: // SBC A, (HL)
            sub_with_carry_u8(gb, &gb->reg.a, gb->mem[gb->reg.hl]);
            return 8;
        case 0x0F: // SBC A, A
            sub_with_carry_u8(gb, &gb->reg.a, gb->reg.a);
            return 4;
        }
        return 4;
    case 0xA0:
        switch (opcode & 0x0F) {
        case 0x00: // AND B
            and_u8(gb, &gb->reg.a, gb->reg.b);
            return 4;
        case 0x01: // AND C
            and_u8(gb, &gb->reg.a, gb->reg.c);
            return 4;
        case 0x02: // AND D
            and_u8(gb, &gb->reg.a, gb->reg.d);
            return 4;
        case 0x03: // AND E
            and_u8(gb, &gb->reg.a, gb->reg.e);
            return 4;
        case 0x04: // AND H
            and_u8(gb, &gb->reg.a, gb->reg.h);
            return 4;
        case 0x05: // AND L
            and_u8(gb, &gb->reg.a, gb->reg.l);
            return 4;
        case 0x06: // AND (HL)
            and_u8(gb, &gb->reg.a, gb->mem[gb->reg.hl]);
            return 8;
        case 0x07: // AND A
            and_u8(gb, &gb->reg.a, gb->reg.a);
            return 4;
        case 0x08: // XOR B
            xor_u8(gb, &gb->reg.a, gb->reg.b);
            return 4;
        case 0x09: // XOR C
            xor_u8(gb, &gb->reg.a, gb->reg.c);
            return 4;
        case 0x0A: // XOR D
            xor_u8(gb, &gb->reg.a, gb->reg.d);
            return 4;
        case 0x0B: // XOR E
            xor_u8(gb, &gb->reg.a, gb->reg.e);
            return 4;
        case 0x0C: // XOR H
            xor_u8(gb, &gb->reg.a, gb->reg.h);
            return 4;
        case 0x0D: // XOR L
            xor_u8(gb, &gb->reg.a, gb->reg.l);
            return 4;
        case 0x0E: // XOR (HL)
            xor_u8(gb, &gb->reg.a, gb->mem[gb->reg.hl]);
            return 8;
        case 0x0F: // XOR A
            xor_u8(gb, &gb->reg.a, gb->reg.a);
            return 4;
        }
        return 4;
    case 0xB0:
        switch (opcode & 0x0F) {
        case 0x00: // OR B
            or_u8(gb, &gb->reg.a, gb->reg.b);
            return 4;
        case 0x01: // OR C
            or_u8(gb, &gb->reg.a, gb->reg.c);
            return 4;
        case 0x02: // OR D
            or_u8(gb, &gb->reg.a, gb->reg.d);
            return 4;
        case 0x03: // OR E
            or_u8(gb, &gb->reg.a, gb->reg.e);
            return 4;
        case 0x04: // OR H
            or_u8(gb, &gb->reg.a, gb->reg.h);
            return 4;
        case 0x05: // OR L
            or_u8(gb, &gb->reg.a, gb->reg.l);
            return 4;
        case 0x06: // OR (HL)
            or_u8(gb, &gb->reg.a, gb->mem[gb->reg.hl]);
            return 8;
        case 0x07: // OR A
            or_u8(gb, &gb->reg.a, gb->reg.a);
            return 4;
        case 0x08: // CP B
            cp_u8(gb, gb->reg.a, gb->reg.b);
            return 4;
        case 0x09: // CP C
            cp_u8(gb, gb->reg.a, gb->reg.c);
            return 4;
        case 0x0A: // CP D
            cp_u8(gb, gb->reg.a, gb->reg.d);
            return 4;
        case 0x0B: // CP E
            cp_u8(gb, gb->reg.a, gb->reg.e);
            return 4;
        case 0x0C: // CP H
            cp_u8(gb, gb->reg.a, gb->reg.h);
            return 4;
        case 0x0D: // CP L
            cp_u8(gb, gb->reg.a, gb->reg.l);
            return 4;
        case 0x0E: // CP (HL)
            cp_u8(gb, gb->reg.a, gb->mem[gb->reg.hl]);
            return 8;
        case 0x0F: // CP A
            cp_u8(gb, gb->reg.a, gb->reg.a);
            return 8;
        }
        break;
    case 0xC0:
        switch (opcode & 0x0F) {
        case 0x00: // RET NZ
            if (!(gb->reg.f & ZERO_FLAG)) {
                ret(gb);
                return 20;
            }

            return 8;
        case 0x01: // POP BC
            pop_u16(gb, &gb->reg.bc);
            return 12;
        case 0x02: // JP NZ, nn
            if (!(gb->reg.f & ZERO_FLAG)) {
                gb->reg.pc = next_u16(gb);
                return 16;
            }

            return 12;
        case 0x03: // JP nn
            gb->reg.pc = next_u16(gb);
            return 16;
        case 0x04: // CALL NZ, nn
            if (!(gb->reg.f & ZERO_FLAG)) {
                call(gb, next_u16(gb));
                return 24;
            }

            return 12;
        case 0x05: // PUSH BC
            push_u16(gb, gb->reg.bc);
            return 16;
        case 0x06: // ADD A, n
            add_u8(gb, &gb->reg.a, next_u8(gb));
            return 8;
        case 0x07: // RST 00h
            restart(gb, 0x0);
            return 16;
        case 0x08: // RET Z
            if (gb->reg.f & ZERO_FLAG) {
                ret(gb);
                return 20;
            }

            return 8;
        case 0x09: // RET
            ret(gb);
            return 16;
        case 0x0A: // JP Z, nn
            if (gb->reg.f & ZERO_FLAG) {
                gb->reg.pc = next_u16(gb);
            }

            return 12;
        case 0x0B: // Prefix byte
            return execute_cb_prefixed_opcode(gb);
        case 0x0C: // CALL Z, nn
            if (gb->reg.f & ZERO_FLAG) {
                call(gb, next_u16(gb));
                return 24;
            }

            return 12;
        case 0x0D: // CALL nn
            call(gb, next_u16(gb));
            return 24;
        }
        break;
    case 0xD0:
        switch (opcode & 0x0F) {

        }
        break;
    case 0xE0:
        switch (opcode & 0x0F) {

        }
        break;
    case 0xF0:
        switch (opcode & 0x0F) {

        }
        break;
    }

    return 0;
}

u8 execute_cb_prefixed_opcode(GameBoy* gb) {
    u8 opcode = next_u8(gb);

    switch (opcode) {
    case 0x00: // RLC B
        rotate_left_carry(gb, &gb->reg.b);
        return 8;
    case 0x01: // RLC C
        rotate_left_carry(gb, &gb->reg.c);
        return 8;
    case 0x02: // RLC D
        rotate_left_carry(gb, &gb->reg.d);
        return 8;
    case 0x03: // RLC E
        rotate_left_carry(gb, &gb->reg.e);
        return 8;
    case 0x04: // RLC H
        rotate_left_carry(gb, &gb->reg.h);
        return 8;
    case 0x05: // RLC L
        rotate_left_carry(gb, &gb->reg.l);
        return 8;
    case 0x06: // RLC (HL)
        rotate_left_carry(gb, &gb->mem[gb->reg.hl]);
        return 16;
    case 0x07: // RLC A
        rotate_left_carry(gb, &gb->reg.a);
        return 8;
    case 0x08: // RRC B
        rotate_right_carry(gb, &gb->reg.b);
        return 8;
    case 0x09: // RRC C
        rotate_right_carry(gb, &gb->reg.c);
        return 8;
    case 0x0A: // RRC D
        rotate_right_carry(gb, &gb->reg.d);
        return 8;
    case 0x0B: // RRC E
        rotate_right_carry(gb, &gb->reg.e);
        return 8;
    case 0x0C: // RRC H
        rotate_right_carry(gb, &gb->reg.h);
        return 8;
    case 0x0D: // RRC L
        rotate_right_carry(gb, &gb->reg.l);
        return 8;
    case 0x0E: // RRC (HL)
        rotate_right_carry(gb, &gb->mem[gb->reg.hl]);
        return 16;
    case 0x0F: // RRC A
        rotate_right_carry(gb, &gb->reg.a);
        return 8;
    case 0x10: // RL B
        rotate_left(gb, &gb->reg.b);
        return 8;
    case 0x11: // RL C
        rotate_left(gb, &gb->reg.c);
        return 8;
    case 0x12: // RL D
        rotate_left(gb, &gb->reg.d);
        return 8;
    case 0x13: // RL E
        rotate_left(gb, &gb->reg.e);
        return 8;
    case 0x14: // RL H
        rotate_left(gb, &gb->reg.h);
        return 8;
    case 0x15: // RL L
        rotate_left(gb, &gb->reg.l);
        return 8;
    case 0x16: // RL (HL)
        rotate_left(gb, &gb->mem[gb->reg.hl]);
        return 16;
    case 0x17: // RL A
        rotate_left(gb, &gb->reg.a);
        return 8;
    case 0x18: // RR B
        rotate_right(gb, &gb->reg.b);
        return 8;
    case 0x19: // RR C
        rotate_right(gb, &gb->reg.c);
        return 8;
    case 0x1A: // RR D
        rotate_right(gb, &gb->reg.d);
        return 8;
    case 0x1B: // RR E
        rotate_right(gb, &gb->reg.e);
        return 8;
    case 0x1C: // RR H
        rotate_right(gb, &gb->reg.h);
        return 8;
    case 0x1D: // RR L
        rotate_right(gb, &gb->reg.l);
        return 8;
    case 0x1F: // RR (HL)
        rotate_right(gb, &gb->mem[gb->reg.hl]);
        return 16;
    case 0x20: // SLA B
        shift_left_carry(gb, &gb->reg.b);
        return 8;
    case 0x21: // SLA C
        shift_left_carry(gb, &gb->reg.c);
        return 8;
    case 0x22: // SLA D
        shift_left_carry(gb, &gb->reg.d);
        return 8;
    case 0x23: // SLA E
        shift_left_carry(gb, &gb->reg.e);
        return 8;
    case 0x24: // SLA H
        shift_left_carry(gb, &gb->reg.h);
        return 8;
    case 0x25: // SLA L
        shift_left_carry(gb, &gb->reg.l);
        return 8;
    case 0x26: // SLA (HL)
        shift_left_carry(gb, &gb->mem[gb->reg.hl]);
        return 16;
    case 0x27: // SLA A
        shift_left_carry(gb, &gb->reg.a);
        return 8;
    case 0x28: // SRA B
        shift_right_carry_signed(gb, &gb->reg.b);
        return 8;
    case 0x29: // SRA C
        shift_right_carry_signed(gb, &gb->reg.c);
        return 8;
    case 0x2A: // SRA D
        shift_right_carry_signed(gb, &gb->reg.d);
        return 8;
    case 0x2B: // SRA E
        shift_right_carry_signed(gb, &gb->reg.e);
        return 8;
    case 0x2C: // SRA H
        shift_right_carry_signed(gb, &gb->reg.h);
        return 8;
    case 0x2D: // SRA L
        shift_right_carry_signed(gb, &gb->reg.l);
        return 8;
    case 0x2E: // SRA (HL)
        shift_right_carry_signed(gb, &gb->mem[gb->reg.hl]);
        return 16;
    case 0x2F: // SRA A
        shift_right_carry_signed(gb, &gb->reg.a);
        return 8;
    case 0x30: // SWAP B
        swap(gb, &gb->reg.b);
        return 8;
    case 0x31: // SWAP C
        swap(gb, &gb->reg.c);
        return 8;
    case 0x32: // SWAP D
        swap(gb, &gb->reg.d);
        return 8;
    case 0x33: // SWAP E
        swap(gb, &gb->reg.e);
        return 8;
    case 0x34: // SWAP H
        swap(gb, &gb->reg.h);
        return 8;
    case 0x35: // SWAP L
        swap(gb, &gb->reg.l);
        return 8;
    case 0x36: // SWAP (HL)
        swap(gb, &gb->mem[gb->reg.hl]);
        return 16;
    case 0x37: // SWAP A
        swap(gb, &gb->reg.a);
        return 8;
    case 0x38: // SRL B
        shift_right_carry(gb, &gb->reg.b); 
        return 8;
    case 0x39: // SRL C
        shift_right_carry(gb, &gb->reg.c); 
        return 8;
    case 0x3A: // SRL D
        shift_right_carry(gb, &gb->reg.d); 
        return 8;
    case 0x3B: // SRL E
        shift_right_carry(gb, &gb->reg.e); 
        return 8;
    case 0x3C: // SRL H
        shift_right_carry(gb, &gb->reg.h); 
        return 8;
    case 0x3D: // SRL L
        shift_right_carry(gb, &gb->reg.l); 
        return 8;
    case 0x3E: // SRL (HL)
        shift_right_carry(gb, &gb->mem[gb->reg.hl]); 
        return 16;
    case 0x3F: // SRL A
        shift_right_carry(gb, &gb->reg.a); 
        return 8;
    case 0x40: // BIT 0, B
        test_bit(gb, &gb->reg.b, 0);
        return 8;
    case 0x41: // BIT 0, C
        test_bit(gb, &gb->reg.c, 0);
        return 8;
    case 0x42: // BIT 0, D
        test_bit(gb, &gb->reg.d, 0);
        return 8;
    case 0x43: // BIT 0, E
        test_bit(gb, &gb->reg.e, 0);
        return 8;
    case 0x44: // BIT 0, H
        test_bit(gb, &gb->reg.h, 0);
        return 8;
    case 0x45: // BIT 0, L
        test_bit(gb, &gb->reg.l, 0);
        return 8;
    case 0x46: // BIT 0, (HL)
        test_bit(gb, &gb->mem[gb->reg.hl], 0);
        return 16;
    case 0x47: // BIT 0, A
        test_bit(gb, &gb->reg.a, 0);
        return 8;
    case 0x48: // BIT 1, B
        test_bit(gb, &gb->reg.b, 1);
        return 8;
    case 0x49: // BIT 1, C
        test_bit(gb, &gb->reg.c, 1);
        return 8;
    case 0x4A: // BIT 1, D
        test_bit(gb, &gb->reg.d, 1);
        return 8;
    case 0x4B: // BIT 1, E
        test_bit(gb, &gb->reg.e, 1);
        return 8;
    case 0x4C: // BIT 1, H
        test_bit(gb, &gb->reg.h, 1);
        return 8;
    case 0x4D: // BIT 1, L
        test_bit(gb, &gb->reg.l, 1);
        return 8;
    case 0x4E: // BIT 1, (HL)
        test_bit(gb, &gb->mem[gb->reg.hl], 1);
        return 16;
    case 0x4F: // BIT 1, A
        test_bit(gb, &gb->reg.a, 1);
        return 8;
    case 0x50: // BIT 2, B
        test_bit(gb, &gb->reg.b, 2);
        return 8;
    case 0x51: // BIT 2, C
        test_bit(gb, &gb->reg.c, 2);
        return 8;
    case 0x52: // BIT 2, D
        test_bit(gb, &gb->reg.d, 2);
        return 8;
    case 0x53: // BIT 2, E
        test_bit(gb, &gb->reg.e, 2);
        return 8;
    case 0x54: // BIT 2, H
        test_bit(gb, &gb->reg.h, 2);
        return 8;
    case 0x55: // BIT 2, L
        test_bit(gb, &gb->reg.l, 2);
        return 8;
    case 0x56: // BIT 2, (HL)
        test_bit(gb, &gb->mem[gb->reg.hl], 2);
        return 16;
    case 0x57: // BIT 2, A
        test_bit(gb, &gb->reg.a, 2);
        return 8;
    case 0x58: // BIT 3, B
        test_bit(gb, &gb->reg.b, 3);
        return 8;
    case 0x59: // BIT 3, C
        test_bit(gb, &gb->reg.c, 3);
        return 8;
    case 0x5A: // BIT 3, D
        test_bit(gb, &gb->reg.d, 3);
        return 8;
    case 0x5B: // BIT 3, E
        test_bit(gb, &gb->reg.e, 3);
        return 8;
    case 0x5C: // BIT 3, H
        test_bit(gb, &gb->reg.h, 3);
        return 8;
    case 0x5D: // BIT 3, L
        test_bit(gb, &gb->reg.l, 3);
        return 8;
    case 0x5E: // BIT 3, (HL)
        test_bit(gb, &gb->mem[gb->reg.hl], 3);
        return 16;
    case 0x5F: // BIT 3, A
        test_bit(gb, &gb->reg.a, 3);
        return 8;
    case 0x60: // BIT 4, B
        test_bit(gb, &gb->reg.b, 4);
        return 8;
    case 0x61: // BIT 4, C
        test_bit(gb, &gb->reg.c, 4);
        return 8;
    case 0x62: // BIT 4, D
        test_bit(gb, &gb->reg.d, 4);
        return 8;
    case 0x63: // BIT 4, E
        test_bit(gb, &gb->reg.e, 4);
        return 8;
    case 0x64: // BIT 4, H
        test_bit(gb, &gb->reg.h, 4);
        return 8;
    case 0x65: // BIT 4, L
        test_bit(gb, &gb->reg.l, 4);
        return 8;
    case 0x66: // BIT 4, (HL)
        test_bit(gb, &gb->mem[gb->reg.hl], 4);
        return 16;
    case 0x67: // BIT 4, A
        test_bit(gb, &gb->reg.a, 4);
        return 8;
    case 0x68: // BIT 5, B
        test_bit(gb, &gb->reg.b, 5);
        return 8;
    case 0x69: // BIT 5, C
        test_bit(gb, &gb->reg.c, 5);
        return 8;
    case 0x6A: // BIT 5, D
        test_bit(gb, &gb->reg.d, 5);
        return 8;
    case 0x6B: // BIT 5, E
        test_bit(gb, &gb->reg.e, 5);
        return 8;
    case 0x6C: // BIT 5, H
        test_bit(gb, &gb->reg.h, 5);
        return 8;
    case 0x6D: // BIT 5, L
        test_bit(gb, &gb->reg.l, 5);
        return 8;
    case 0x6E: // BIT 5, (HL)
        test_bit(gb, &gb->mem[gb->reg.hl], 5);
        return 16;
    case 0x6F: // BIT 5, A
        test_bit(gb, &gb->reg.a, 5);
        return 8;
    case 0x70: // BIT 6, B
        test_bit(gb, &gb->reg.b, 6);
        return 8;
    case 0x71: // BIT 6, C
        test_bit(gb, &gb->reg.c, 6);
        return 8;
    case 0x72: // BIT 6, D
        test_bit(gb, &gb->reg.d, 6);
        return 8;
    case 0x73: // BIT 6, E
        test_bit(gb, &gb->reg.e, 6);
        return 8;
    case 0x74: // BIT 6, H
        test_bit(gb, &gb->reg.h, 6);
        return 8;
    case 0x75: // BIT 6, L
        test_bit(gb, &gb->reg.l, 6);
        return 8;
    case 0x76: // BIT 6, (HL)
        test_bit(gb, &gb->mem[gb->reg.hl], 6);
        return 16;
    case 0x77: // BIT 6, A
        test_bit(gb, &gb->reg.a, 6);
        return 8;
    case 0x78: // BIT 7, B
        test_bit(gb, &gb->reg.b, 7);
        return 8;
    case 0x79: // BIT 7, C
        test_bit(gb, &gb->reg.c, 7);
        return 8;
    case 0x7A: // BIT 7, D
        test_bit(gb, &gb->reg.d, 7);
        return 8;
    case 0x7B: // BIT 7, E
        test_bit(gb, &gb->reg.e, 7);
        return 8;
    case 0x7C: // BIT 7, H
        test_bit(gb, &gb->reg.h, 7);
        return 8;
    case 0x7D: // BIT 7, L
        test_bit(gb, &gb->reg.l, 7);
        return 8;
    case 0x7E: // BIT 7, (HL)
        test_bit(gb, &gb->mem[gb->reg.hl], 7);
        return 16;
    case 0x7F: // BIT 7, A
        test_bit(gb, &gb->reg.a, 7);
        return 8;
    case 0x80: // RES 0, B
        reset_bit(gb, &gb->reg.b, 0);
        return 8;
    case 0x81: // RES 0, C
        reset_bit(gb, &gb->reg.c, 0);
        return 8;
    case 0x82: // RES 0, D
        reset_bit(gb, &gb->reg.d, 0);
        return 8;
    case 0x83: // RES 0, E
        reset_bit(gb, &gb->reg.e, 0);
        return 8;
    case 0x84: // RES 0, H
        reset_bit(gb, &gb->reg.h, 0);
        return 8;
    case 0x85: // RES 0, L
        reset_bit(gb, &gb->reg.l, 0);
        return 8;
    case 0x86: // RES 0, (HL)
        reset_bit(gb, &gb->mem[gb->reg.hl], 0);
        return 16;
    case 0x87: // RES 0, A
        reset_bit(gb, &gb->reg.a, 0);
        return 8;
    case 0x88: // RES 1, B
        reset_bit(gb, &gb->reg.b, 1);
        return 8;
    case 0x89: // RES 1, C
        reset_bit(gb, &gb->reg.c, 1);
        return 8;
    case 0x8A: // RES 1, D
        reset_bit(gb, &gb->reg.d, 1);
        return 8;
    case 0x8B: // RES 1, E
        reset_bit(gb, &gb->reg.e, 1);
        return 8;
    case 0x8C: // RES 1, H
        reset_bit(gb, &gb->reg.h, 1);
        return 8;
    case 0x8D: // RES 1, (HL)
        reset_bit(gb, &gb->mem[gb->reg.hl], 1);
        return 16;
    case 0x8F: // RES 1, A
        reset_bit(gb, &gb->reg.a, 1);
        return 8;
    case 0x90: // RES 2, B
        reset_bit(gb, &gb->reg.b, 2);
        return 8;
    case 0x91: // RES 2, C
        reset_bit(gb, &gb->reg.c, 2);
        return 8;
    case 0x92: // RES 2, D
        reset_bit(gb, &gb->reg.d, 2);
        return 8;
    case 0x93: // RES 2, E
        reset_bit(gb, &gb->reg.e, 2);
        return 8;
    case 0x94: // RES 2, H
        reset_bit(gb, &gb->reg.h, 2);
        return 8;
    case 0x95: // RES 2, L
        reset_bit(gb, &gb->reg.l, 2);
        return 8;
    case 0x96: // RES 2, (HL)
        reset_bit(gb, &gb->mem[gb->reg.hl], 2);
        return 16;
    case 0x97: // RES 2, A
        reset_bit(gb, &gb->reg.a, 2);
        return 8;
    case 0x98: // RES 3, B
        reset_bit(gb, &gb->reg.b, 3);
        return 8;
    case 0x99: // RES 3, C
        reset_bit(gb, &gb->reg.c, 3);
        return 8;
    case 0x9A: // RES 3, D
        reset_bit(gb, &gb->reg.d, 3);
        return 8;
    case 0x9B: // RES 3, E
        reset_bit(gb, &gb->reg.e, 3);
        return 8;
    case 0x9C: // RES 3, H
        reset_bit(gb, &gb->reg.h, 3);
        return 8;
    case 0x9D: // RES 3, L
        reset_bit(gb, &gb->reg.l, 3);
        return 8;
    case 0x9E: // RES 3, (HL)
        reset_bit(gb, &gb->mem[gb->reg.hl], 3);
        return 16;
    case 0x9F: // RES 3, A
        reset_bit(gb, &gb->reg.a, 3);
        return 8;
    case 0xA0: // RES 4, B
        reset_bit(gb, &gb->reg.b, 4);
        return 8;
    case 0xA1: // RES 4, C
        reset_bit(gb, &gb->reg.c, 4);
        return 8;
    case 0xA2: // RES 4, D
        reset_bit(gb, &gb->reg.d, 4);
        return 8;
    case 0xA3: // RES 4, E
        reset_bit(gb, &gb->reg.e, 4);
        return 8;
    case 0xA4: // RES 4, H
        reset_bit(gb, &gb->reg.h, 4);
        return 8;
    case 0xA5: // RES 4, L
        reset_bit(gb, &gb->reg.l, 4);
        return 8;
    case 0xA6: // RES 4, (HL)
        reset_bit(gb, &gb->mem[gb->reg.hl], 4);
        return 16;
    case 0xA7: // RES 4, A
        reset_bit(gb, &gb->reg.a, 4);
        return 8;
    case 0xA8: // RES 5, B
        reset_bit(gb, &gb->reg.b, 5);
        return 8;
    case 0xA9: // RES 5, C
        reset_bit(gb, &gb->reg.c, 5);
        return 8;
    case 0xAA: // RES 5, D
        reset_bit(gb, &gb->reg.d, 5);
        return 8;
    case 0xAB: // RES 5, E
        reset_bit(gb, &gb->reg.e, 5);
        return 8;
    case 0xAC: // RES 5, H
        reset_bit(gb, &gb->reg.h, 5);
        return 8;
    case 0xAD: // RES 5, L
        reset_bit(gb, &gb->reg.l, 5);
        return 8;
    case 0xAE: // RES 5, (HL)
        reset_bit(gb, &gb->mem[gb->reg.hl], 5);
        return 16;
    case 0xAF: // RES 5, A
        reset_bit(gb, &gb->reg.a, 5);
        return 8;
    case 0xB0: // RES 6, B
        reset_bit(gb, &gb->reg.b, 6);
        return 8;
    case 0xB1: // RES 6, C
        reset_bit(gb, &gb->reg.c, 6);
        return 8;
    case 0xB2: // RES 6, D
        reset_bit(gb, &gb->reg.d, 6);
        return 8;
    case 0xB3: // RES 6, E
        reset_bit(gb, &gb->reg.e, 6);
        return 8;
    case 0xB4: // RES 6, H
        reset_bit(gb, &gb->reg.h, 6);
        return 8;
    case 0xB5: // RES 6, L
        reset_bit(gb, &gb->reg.l, 6);
        return 8;
    case 0xB6: // RES 6, (HL)
        reset_bit(gb, &gb->mem[gb->reg.hl], 6);
        return 16;
    case 0xB7: // RES 6, A
        reset_bit(gb, &gb->reg.a, 6);
        return 8;
    case 0xB8: // RES 7, B
        reset_bit(gb, &gb->reg.b, 7);
        return 8;
    case 0xB9: // RES 7, C
        reset_bit(gb, &gb->reg.c, 7);
        return 8;
    case 0xBA: // RES 7, D
        reset_bit(gb, &gb->reg.d, 7);
        return 8;
    case 0xBB: // RES 7, E
        reset_bit(gb, &gb->reg.e, 7);
        return 8;
    case 0xBC: // RES 7, H
        reset_bit(gb, &gb->reg.h, 7);
        return 8;
    case 0xBD: // RES 7, L
        reset_bit(gb, &gb->reg.l, 7);
        return 8;
    case 0xBE: // RES 7, (HL)
        reset_bit(gb, &gb->mem[gb->reg.hl], 7);
        return 16;
    case 0xBF: // RES 7, A
        reset_bit(gb, &gb->reg.a, 7);
        return 8;
    case 0xC0: // SET 0, B
        set_bit(gb, &gb->reg.b, 0);
        return 8;
    case 0xC1: // SET 0, C
        set_bit(gb, &gb->reg.c, 0);
        return 8;
    case 0xC2: // SET 0, D
        set_bit(gb, &gb->reg.d, 0);
        return 8;
    case 0xC3: // SET 0, E
        set_bit(gb, &gb->reg.e, 0);
        return 8;
    case 0xC4: // SET 0, H
        set_bit(gb, &gb->reg.h, 0);
        return 8;
    case 0xC5: // SET 0, L
        set_bit(gb, &gb->reg.l, 0);
        return 8;
    case 0xC6: // SET 0, (HL)
        set_bit(gb, &gb->mem[gb->reg.hl], 0);
        return 8;
    case 0xC7: // SET 0, A
        set_bit(gb, &gb->reg.a, 0);
        return 8;
    case 0xC8: // SET 1, B
        set_bit(gb, &gb->reg.b, 1);
        return 8;
    case 0xC9: // SET 1, C
        set_bit(gb, &gb->reg.c, 1);
        return 8;
    case 0xCA: // SET 1, D
        set_bit(gb, &gb->reg.d, 1);
        return 8;
    case 0xCB: // SET 1, E
        set_bit(gb, &gb->reg.e, 1);
        return 8;
    case 0xCC: // SET 1, H
        set_bit(gb, &gb->reg.h, 1);
        return 8;
    case 0xCD: // SET 1, L
        set_bit(gb, &gb->reg.l, 1);
        return 8;
    case 0xCE: // SET 1, (HL)
        set_bit(gb, &gb->mem[gb->reg.hl], 1);
        return 16;
    case 0xCF: // SET 1, A
        set_bit(gb, &gb->reg.a, 1);
        return 8;
    case 0xD0: // SET 2, B
        set_bit(gb, &gb->reg.b, 2);
        return 8;
    case 0xD1: // SET 2, C
        set_bit(gb, &gb->reg.c, 2);
        return 8;
    case 0xD2: // SET 2, D
        set_bit(gb, &gb->reg.d, 2);
        return 8;
    case 0xD3: // SET 2, E
        set_bit(gb, &gb->reg.e, 2);
        return 8;
    case 0xD4: // SET 2, H
        set_bit(gb, &gb->reg.h, 2);
        return 8;
    case 0xD5: // SET 2, L
        set_bit(gb, &gb->reg.l, 2);
        return 8;
    case 0xD6: // SET 2, (HL)
        set_bit(gb, &gb->mem[gb->reg.hl], 2);
        return 16;
    case 0xD7: // SET 2, A
        set_bit(gb, &gb->reg.a, 2);
        return 8;
    case 0xD8: // SET 3, B
        set_bit(gb, &gb->reg.b, 3);
        return 8;
    case 0xD9: // SET 3, C
        set_bit(gb, &gb->reg.c, 3);
        return 8;
    case 0xDA: // SET 3, D
        set_bit(gb, &gb->reg.d, 3);
        return 8;
    case 0xDB: // SET 3, E
        set_bit(gb, &gb->reg.e, 3);
        return 8;
    case 0xDC: // SET 3, H
        set_bit(gb, &gb->reg.h, 3);
        return 8;
    case 0xDD: // SET 3, L
        set_bit(gb, &gb->reg.l, 3);
        return 8;
    case 0xDE: // SET 3, (HL)
        set_bit(gb, &gb->mem[gb->reg.hl], 3);
        return 16;
    case 0xDF: // SET 3, A
        set_bit(gb, &gb->reg.a, 3);
        return 8;
    case 0xE0: // SET 4, B
        set_bit(gb, &gb->reg.b, 4);
        return 8;
    case 0xE1: // SET 4, C
        set_bit(gb, &gb->reg.c, 4);
        return 8;
    case 0xE2: // SET 4, D
        set_bit(gb, &gb->reg.d, 4);
        return 8;
    case 0xE3: // SET 4, E
        set_bit(gb, &gb->reg.e, 4);
        return 8;
    case 0xE4: // SET 4, H
        set_bit(gb, &gb->reg.h, 4);
        return 8;
    case 0xE5: // SET 4, L
        set_bit(gb, &gb->reg.l, 4);
        return 8;
    case 0xE6: // SET 4, (HL)
        set_bit(gb, &gb->mem[gb->reg.hl], 4);
        return 16;
    case 0xE7: // SET 4, A
        set_bit(gb, &gb->reg.a, 4);
        return 8;
    case 0xE8: // SET 5, B
        set_bit(gb, &gb->reg.b, 5);
        return 8;
    case 0xE9: // SET 5, C
        set_bit(gb, &gb->reg.c, 5);
        return 8;
    case 0xEA: // SET 5, D
        set_bit(gb, &gb->reg.d, 5);
        return 8;
    case 0xEB: // SET 5, E
        set_bit(gb, &gb->reg.e, 5);
        return 8;
    case 0xEC: // SET 5, H
        set_bit(gb, &gb->reg.h, 5);
        return 8;
    case 0xED: // SET 5, L
        set_bit(gb, &gb->reg.l, 5);
        return 8;
    case 0xEE: // SET 5, (HL)
        set_bit(gb, &gb->mem[gb->reg.hl], 5);
        return 16;
    case 0xEF: // SET 5, A
        set_bit(gb, &gb->reg.a, 5);
        return 8;
    case 0xF0: // SET 6, B
        set_bit(gb, &gb->reg.b, 6);
        return 8;
    case 0xF1: // SET 6, C
        set_bit(gb, &gb->reg.c, 6);
        return 8;
    case 0xF2: // SET 6, D
        set_bit(gb, &gb->reg.d, 6);
        return 8;
    case 0xF3: // SET 6, E
        set_bit(gb, &gb->reg.e, 6);
        return 8;
    case 0xF4: // SET 6, H
        set_bit(gb, &gb->reg.h, 6);
        return 8;
    case 0xF5: // SET 6, L
        set_bit(gb, &gb->reg.l, 6);
        return 8;
    case 0xF6: // SET 6, (HL)
        set_bit(gb, &gb->mem[gb->reg.hl], 6);
        return 16;
    case 0xF7: // SET 6, A
        set_bit(gb, &gb->reg.a, 6);
        return 8;
    case 0xF8: // SET 7, B
        set_bit(gb, &gb->reg.b, 7);
        return 8;
    case 0xF9: // SET 7, C
        set_bit(gb, &gb->reg.c, 7);
        return 8;
    case 0xFA: // SET 7, D
        set_bit(gb, &gb->reg.d, 7);
        return 8;
    case 0xFB: // SET 7, E
        set_bit(gb, &gb->reg.e, 7);
        return 8;
    case 0xFC: // SET 7, H
        set_bit(gb, &gb->reg.h, 7);
        return 8;
    case 0xFD: // SET 7, L
        set_bit(gb, &gb->reg.l, 7);
        return 8;
    case 0xFE: // SET 7, (HL)
        set_bit(gb, &gb->mem[gb->reg.hl], 7);
        return 16;
    case 0xFF: // SET 7, A
        set_bit(gb, &gb->reg.a, 7);
        return 8;
    }

    return 0;
}
