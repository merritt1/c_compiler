#ifndef _OPTIMIZE_H
#define _OPTIMIZE_H

#include <stdio.h>
#include <stdbool.h>

void optimize_program(struct ir_section *program_ir, int optimization_level);
void optimize_program_level_one(struct ir_section *program_ir);
void optimize_program_level_two(struct ir_section *program_ir);

/* Optimization Functions: Level 1 */
void swap_constint_addressof(struct ir_instruction *instruction);
int remove_duplicate_address_of(struct ir_instruction *instruction);
int remove_duplicate_load(struct ir_instruction *instruction);
int remove_arithmetic_identities(struct ir_instruction *instruction);
int remove_redundant_load_store(struct ir_instruction *instruction);
int remove_goto_label(struct ir_instruction *instruction);

/* Optimization Functions: Level 2 */
void decompose_mults_to_shift(struct ir_instruction *instruction);
int constant_folding(struct ir_instruction *instruction);

/* PATTERNS */
struct ir_section *single_const_arithmetic_pattern(enum ir_instruction_kind kind);
struct ir_section *single_const_bitwise_and_pattern(enum ir_instruction_kind kind);
struct ir_section *double_const_arithmetic_pattern(enum ir_instruction_kind kind);
struct ir_section *unary_const_arithmetic_pattern(enum ir_instruction_kind kind);
struct ir_section *duplicate_pattern_generate(enum ir_instruction_kind kind);
struct ir_section *load_store_pattern(void);
struct ir_section *const_int_address_of_pattern(void);
struct ir_section *goto_label_pattern(void);

// Helper functions
bool pattern_match(struct ir_section *pattern, struct ir_instruction *current_instr);
void update_register_references(struct ir_instruction *instruction, int val);

#endif
