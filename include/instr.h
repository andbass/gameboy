#pragma once
/*
 * Generic definitions for the various functionalities of the GameBoy's opcodes
 * Also provides utility functions for dealing with flags
 *
 * Opcode functions are used by `execute` in "cpu.h"
 */

#include "gb.h"

// Handles INC and ADD
void add_u8(GameBoy*, u8* dest, u8 val);

// Handles SUB
void sub_u8(GameBoy*, u8* dest, u8 val);

// Handles SBC
void sub_with_carry_u8(GameBoy*, u8* dest, u8 val);

// Handles ADD with 16 bit registers
void add_u16(GameBoy*, u16* dest, u16 val);

// Handles ADC
void add_with_carry_u8(GameBoy*, u8* dest, u8 val);

// Handles SUB with 16 bit registers
void sub_u16(GameBoy*, u16* dest, u16 val);

// Handles CPL (flips all bits)
void complement(GameBoy*, u8* dest);

// Handles RL, RLA
void rotate_left(GameBoy*, u8* dest);

// Handles RLC, RLCA
void rotate_left_carry(GameBoy*, u8* dest);

// Handles RR, RRA
void rotate_right(GameBoy*, u8* dest);

// Handles RRC, RRCA
void rotate_right_carry(GameBoy*, u8* dest);

// Handles SLA
void shift_left_carry(GameBoy*, u8* dest);

// Handles SRA
void shift_right_carry_signed(GameBoy*, u8* dest);

// Handles SRL
void shift_right_carry(GameBoy* gb, u8* dest);

// Handles JR and company
void relative_jump(GameBoy*, i8 offset);

// Handles DAA
void decimal_adjust(GameBoy*, u8* dest);

// Handles AND
void and_u8(GameBoy*, u8* dest, u8 val);

// Handles OR
void or_u8(GameBoy*, u8* dest, u8 val);

// Handles XOR
void xor_u8(GameBoy*, u8* dest, u8 val);

// Handles CP
void cp_u8(GameBoy*, u8 val1, u8 val2);

// Handles POP, RET
void pop_u16(GameBoy*, u16* dest);

// Handles PUSH
void push_u16(GameBoy*, u16 val);

// Handles SWAP
void swap(GameBoy*, u8* dest);

// Handles BIT
void test_bit(GameBoy*, u8* dest, u8 bit);

// Handles CALL
void call(GameBoy*, u16 addr);

// Handles RET
void ret(GameBoy*);

// Handles RST
void restart(GameBoy*, u8 offset);
