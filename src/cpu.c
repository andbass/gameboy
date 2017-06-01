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
// nn -> next word (2 bytes)
// n -> next byte
// s -> next signed byte
// ss -> next signed word
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
            return 8;

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
