#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "node.h"
#include "symbol.h"
#include "type.h"
#include "type_check.h"
#include "ir.h"
#include "optimize.h"

void optimize_program(struct ir_section *program_ir, int optimization_level)  {
  if (optimization_level >= 1) {
    optimize_program_level_one(program_ir);
  }
  if (optimization_level >= 2) {
    optimize_program_level_two(program_ir);
  }
}

void optimize_program_level_one(struct ir_section *program_ir) {
  struct ir_instruction *instruction = program_ir->first;
  swap_constint_addressof(instruction);
  int num_modifications;
  do {
  	num_modifications = 0;
	  num_modifications += remove_duplicate_address_of(instruction);
	  num_modifications += remove_arithmetic_identities(instruction);
	  num_modifications += remove_redundant_load_store(instruction);
	  num_modifications += remove_duplicate_load(instruction);
	  num_modifications += remove_goto_label(instruction);
  } while (num_modifications > 0);
}

void optimize_program_level_two(struct ir_section *program_ir) {
  struct ir_instruction *instruction = program_ir->first;
  decompose_mults_to_shift(instruction);
  int num_modifications;
  do {
		num_modifications = 0;
	  num_modifications += constant_folding(instruction);
  } while (num_modifications > 0);
}

/**********************************
 * LEVEL 1 OPTIMIZATION FUNCTIONS *
 **********************************/

// If two adjacent instructions load the address of the same variable into different registers,
// remove one of them. Correct later references to register assigned to by removed instruction
int remove_duplicate_address_of(struct ir_instruction *instruction) {
	int num_modifications = 0;
  while (instruction) {
    struct ir_section *duplicate_pattern = duplicate_pattern_generate(IR_addressOf);
    if (pattern_match(duplicate_pattern, instruction)) {
      if (!(instruction->operands[1].kind == OPERAND_IDENTIFIER &&
            instruction->next->operands[1].kind == OPERAND_IDENTIFIER)) {
        instruction = instruction->next;
        continue;
      }
      struct symbol *sym1 = instruction->operands[1].data.symbol;
      struct symbol *sym2 = instruction->next->operands[1].data.symbol;
      if (!(sym1 == sym2)) {
        instruction = instruction->next;
        continue; // Not the same identifier
      }

      // Unnecessary addressOf IR instruction; remove.
      int reg_to_replace = instruction->next->operands[0].data.temporary;
      update_register_references(instruction->next->next, reg_to_replace);
      ir_remove_instruction(instruction->next); // Remove superfluous addressOf
      ++num_modifications;
      continue;
    }
    instruction = instruction->next;
  }
  return num_modifications;
}

// If two adjacent instructions load a value from the same address into different registers,
// remove one of them. Correct later references to register assigned to by removed instruction.
int remove_duplicate_load(struct ir_instruction *instruction) {
	int num_modifications = 0;
  while (instruction) {
    struct ir_section *duplicate_pattern = duplicate_pattern_generate(IR_loadWord);
    if (pattern_match(duplicate_pattern, instruction)) {
      int first_src  = instruction->operands[1].data.temporary;
      int second_src = instruction->next->operands[1].data.temporary;
      if (!(first_src == second_src)) {
        instruction = instruction->next;
        continue;
      }

      // Unnecessary loadWord IR instruction; remove.
      int reg_to_replace = instruction->next->operands[0].data.temporary;
      update_register_references(instruction->next->next, reg_to_replace);
      ir_remove_instruction(instruction->next);
      ++num_modifications;
      continue;
    }
    instruction = instruction->next;
  }
  return num_modifications;
}


// If a goto instruction immediately precedes its associated label, it can be removed.
int remove_goto_label(struct ir_instruction *instruction) {
	int num_modifications = 0;
  while (instruction) {
    struct ir_section *pattern = goto_label_pattern();
    if (pattern_match(pattern, instruction)) {
	    struct ir_instruction *branch = instruction;
	    struct ir_instruction *label = instruction->next;
	    if ((branch->operands[0].kind == label->operands[0].kind) && 
	    		 (branch->operands[0].kind == OPERAND_GENERATED_LABEL) &&
	    		 (branch->operands[0].data.temporary == label->operands[0].data.temporary)) {
				// Remove goto
				ir_remove_instruction(branch);
	    	instruction = instruction->next->next;
	    	++num_modifications;
	    	continue;
  		}
	  }
    instruction = instruction->next;
  }
  return num_modifications;
}


// Helper function for arithmetic identity removal. Whenever a constInt address is followed by
// an addressOf instruction, this function swaps those instructions in the IR. This gives
// the expression "a+0" the same pattern in the IR as the insruction "0+a", i.e. a constInt
// instruction, followed by a loadWord, followed by an arithmetic instruction. Due to SSA
// these instructions will always modify different registers, so swapping them doesn't alter
// program semantics.
void swap_constint_addressof(struct ir_instruction *instruction) {
  while (instruction) {
    struct ir_section *pattern = const_int_address_of_pattern();
    if (pattern_match(pattern, instruction)) {
      // Swap instructions
      ir_swap_instructions(instruction, instruction->next);
    }
    instruction = instruction->next;
  }
}

// Arithmetic identities (e.g. a+0, a-0, a/1, etc.) are removed from the IR.
int remove_arithmetic_identities(struct ir_instruction *instruction) {
	int num_modifications = 0;
  while (instruction) {
    struct ir_section *add_pattern = single_const_arithmetic_pattern(IR_addSignedWord);
    struct ir_section *addu_pattern = single_const_arithmetic_pattern(IR_addUnsignedWord);
    struct ir_section *sub_pattern = single_const_arithmetic_pattern(IR_subSignedWord);
	  struct ir_section *subu_pattern = single_const_arithmetic_pattern(IR_subUnsignedWord);

	  struct ir_section *mult_pattern = single_const_arithmetic_pattern(IR_multSignedWord);
	  struct ir_section *multu_pattern = single_const_arithmetic_pattern(IR_multUnsignedWord);
    struct ir_section *div_pattern = single_const_arithmetic_pattern(IR_divSignedWord);
    struct ir_section *divu_pattern = single_const_arithmetic_pattern(IR_divUnsignedWord);

	  struct ir_section *lshift_pattern = single_const_arithmetic_pattern(IR_leftShiftWord);
	  struct ir_section *rshift_pattern = single_const_arithmetic_pattern(IR_rightShiftSignedWord);
	  struct ir_section *rshiftu_pattern = single_const_arithmetic_pattern(IR_rightShiftUnsignedWord);

	  struct ir_section *bitwise_or_pattern = single_const_arithmetic_pattern(IR_bitwiseOrWord);
	  struct ir_section *bitwise_xor_pattern = single_const_arithmetic_pattern(IR_bitwiseXorWord);

	  struct ir_section *bitwise_and_pattern = single_const_bitwise_and_pattern(IR_bitwiseAndWord);

    bool identity_found = false;
    if (pattern_match(mult_pattern, instruction) || 
	    	pattern_match(multu_pattern, instruction) ||
	    	pattern_match(div_pattern, instruction) ||
    		pattern_match(divu_pattern, instruction)) {
      if (instruction->operands[1].data.number == 1) {
      	identity_found = true;
      }
    }
    if (pattern_match(add_pattern, instruction) || 
    		pattern_match(addu_pattern, instruction) ||
        pattern_match(sub_pattern, instruction) ||
        pattern_match(subu_pattern, instruction) ||
        pattern_match(lshift_pattern, instruction) ||
        pattern_match(rshift_pattern, instruction) ||
        pattern_match(rshiftu_pattern, instruction) ||
        pattern_match(bitwise_or_pattern, instruction) ||
        pattern_match(bitwise_xor_pattern, instruction)) {
      if (instruction->operands[1].data.number == 0) {
      	identity_found = true;
      }
    }
    if (pattern_match(bitwise_and_pattern, instruction)) {
      if (instruction->operands[1].data.number == 1) {
      	identity_found = true;
      	//Remove unaryMinus IR instruction
      	int reg_to_replace = instruction->next->operands[0].data.temporary;
	      ir_remove_instruction(instruction->next);
	   		update_register_references(instruction->next, reg_to_replace);
      }
    }
    if (identity_found) {
      // Remove constInt
      int reg_to_remove = instruction->operands[0].data.temporary;
      update_register_references(instruction->next, reg_to_remove);
      struct ir_instruction *arithmetic_instruction = instruction->next->next;
      ir_remove_instruction(instruction);

      // Remove arithmetic IR instruction
      reg_to_remove = arithmetic_instruction->operands[0].data.temporary;
      update_register_references(arithmetic_instruction->next, reg_to_remove);
      ir_remove_instruction(arithmetic_instruction);
      ++num_modifications;
    }
    instruction = instruction->next;
  }
  return num_modifications;
}

// If a value is loaded from an address and then immediately stored back to that
// address, remove both the load and store instructions.
int remove_redundant_load_store(struct ir_instruction *instruction) {
	int num_modifications = 0;
  while (instruction) {
    struct ir_section *ls_pattern = load_store_pattern();
    if (pattern_match(ls_pattern, instruction)) {
    	struct ir_instruction *load = instruction;
    	struct ir_instruction *store = instruction->next;
      if ((load->operands[0].data.temporary == store->operands[1].data.temporary) &&
      		(load->operands[1].data.temporary == store->operands[0].data.temporary)) {

      	// Redundant load/store. Remove.
      	instruction = load->next->next;
        ir_remove_instruction(load);
        ir_remove_instruction(store);

    		// Update register references
    		int reg_to_replace = load->operands[0].data.temporary;
	      update_register_references(instruction, reg_to_replace);

        ++num_modifications;
        continue;
      }	
    }
    instruction = instruction->next;
  }
  return num_modifications;
}

/**********************************
 * LEVEL 2 OPTIMIZATION FUNCTIONS *
 **********************************/

// Decompose multiplication by a constant into a single bitshift (or multiple bitshifts 
// followed by adds if constant is not an even power of 2). Constant operand must be > 0.
void decompose_mults_to_shift(struct ir_instruction *instruction) {
  while (instruction) {
    struct ir_section *mult_pattern = single_const_arithmetic_pattern(IR_multSignedWord);
    if (pattern_match(mult_pattern, instruction)) {
        int multiplicand = instruction->operands[1].data.number;
        if (multiplicand < 1) {
          instruction = instruction->next;
          continue;
        }

        int power_2 = (multiplicand & (~(multiplicand - 1)));
        int bits_to_shift = 0;

        int temp = power_2;
        while (temp > 1) {
          ++bits_to_shift;
          temp >>= 1;
        }

        instruction->operands[1].data.number = bits_to_shift;
        struct ir_instruction *mul_op = instruction->next->next;
        mul_op->kind = IR_leftShiftWord; // Replace multiplication operation with lshift

        if (!(multiplicand - power_2)) break; // No remainder, const was power of 2

        // Handle remainder
        struct ir_instruction *new = ir_insert_section_after(mul_op, mult_pattern);
        ir_operand_copy(new, 0, &(instruction->operands[0]));
        ir_operand_number_literal(new, 1, multiplicand - power_2);

        ir_operand_copy(new->next, 0, &(instruction->next->operands[0]));
        ir_operand_copy(new->next, 1, &(instruction->next->operands[1]));

        ir_operand_copy(new->next->next, 0, &(instruction->next->next->operands[0]));
        new->next->next->operands[0].data.temporary++;
        ir_operand_copy(new->next->next, 1, &(instruction->next->next->operands[1]));
        ir_operand_copy(new->next->next, 2, &(instruction->next->next->operands[2]));

        // Sum remainder product and result of lshift. Add sum to IR.
        struct ir_instruction *sum = ir_instruction(IR_addSignedWord);
        ir_operand_copy(sum, 0, &(instruction->next->next->operands[0]));
        ir_operand_copy(sum, 1, &(instruction->next->next->operands[0]));
        ir_operand_copy(sum, 2, &(new->next->next->operands[0]));
        ir_insert_instruction_after(new->next->next, sum);

    }
    instruction = instruction->next;
  }
}

// Evaluate arithmetic operations with 2 constInt operands at compile time.
// Operands may be positive, negative, or zero.
int constant_folding(struct ir_instruction *instruction) {
	int num_modifications = 0;
  while (instruction) {
  	// Operands will always be signed
    struct ir_section *add_pattern = double_const_arithmetic_pattern(IR_addSignedWord);
    struct ir_section *sub_pattern = double_const_arithmetic_pattern(IR_subSignedWord);

	  struct ir_section *mult_pattern = double_const_arithmetic_pattern(IR_multSignedWord);
    struct ir_section *div_pattern = double_const_arithmetic_pattern(IR_divSignedWord);
    struct ir_section *rem_pattern = double_const_arithmetic_pattern(IR_remSignedWord);

    struct ir_section *lshift_pattern = double_const_arithmetic_pattern(IR_leftShiftWord);
    struct ir_section *rshift_pattern = double_const_arithmetic_pattern(IR_rightShiftSignedWord);

    struct ir_section *bitwise_and_pattern = double_const_arithmetic_pattern(IR_bitwiseAndWord);
    struct ir_section *bitwise_xor_pattern = double_const_arithmetic_pattern(IR_bitwiseXorWord);
    struct ir_section *bitwise_or_pattern = double_const_arithmetic_pattern(IR_bitwiseOrWord);

    struct ir_section *unary_minus_pattern = unary_const_arithmetic_pattern(IR_unaryMinus);
    struct ir_section *unary_b_neg_pattern = unary_const_arithmetic_pattern(IR_unaryBitwiseNegation);

    bool match_found = false;
    bool binary_operation = false;
    int result = 0;
    if (pattern_match(add_pattern, instruction)) {
    	result = instruction->operands[1].data.number + instruction->next->operands[1].data.number;
    	match_found = true;
    	binary_operation = true;
    }
    if (pattern_match(sub_pattern, instruction)) {
    	result = instruction->operands[1].data.number - instruction->next->operands[1].data.number;
    	match_found = true;
    	binary_operation = true;
    }

    if (pattern_match(mult_pattern, instruction)) {
    	result = instruction->operands[1].data.number * instruction->next->operands[1].data.number;
    	match_found = true;
    	binary_operation = true;
    }
    if (pattern_match(div_pattern, instruction)) {
    	result = instruction->operands[1].data.number / instruction->next->operands[1].data.number;
    	match_found = true;
    	binary_operation = true;
    }
    if (pattern_match(rem_pattern, instruction)) {
    	result = instruction->operands[1].data.number % instruction->next->operands[1].data.number;
    	match_found = true;
    	binary_operation = true;
    }

    if (pattern_match(lshift_pattern, instruction)) {
    	result = instruction->operands[1].data.number << instruction->next->operands[1].data.number;
    	match_found = true;
     	binary_operation = true;
    }
    if (pattern_match(rshift_pattern, instruction)) {
    	result = instruction->operands[1].data.number >> instruction->next->operands[1].data.number;
    	match_found = true;
    	binary_operation = true;
    }

    if (pattern_match(bitwise_and_pattern, instruction)) {
    	result = instruction->operands[1].data.number & instruction->next->operands[1].data.number;
    	match_found = true;
    	binary_operation = true;
    }
    if (pattern_match(bitwise_xor_pattern, instruction)) {
    	result = instruction->operands[1].data.number ^ instruction->next->operands[1].data.number;
    	match_found = true;
     	binary_operation = true;
    }
    if (pattern_match(bitwise_or_pattern, instruction)) {
    	result = instruction->operands[1].data.number | instruction->next->operands[1].data.number;
    	match_found = true;
    	binary_operation = true;
    }

    if (pattern_match(unary_minus_pattern, instruction)) {
			result = -(instruction->operands[1].data.number);
    	match_found = true;
    }
    if (pattern_match(unary_b_neg_pattern, instruction)) {
    	result = ~(instruction->operands[1].data.number);
    	match_found = true;
 		}

    if (match_found) {
    	struct ir_instruction *second_constInt = instruction->next;
    	struct ir_instruction *arithmetic_instruction = instruction->next->next;

      // Remove first constInt
      int reg_to_remove = instruction->operands[0].data.temporary;
      update_register_references(instruction->next, reg_to_remove);
      ir_remove_instruction(instruction);

      // Remove second constInt (for binary operations)
      if (binary_operation) {
	      reg_to_remove = second_constInt->operands[0].data.temporary;
	      update_register_references(second_constInt->next, reg_to_remove);
	      ir_remove_instruction(second_constInt);
    	} else {
    		arithmetic_instruction = arithmetic_instruction->prev;
    	}

      // Change arithmetic operand to constInt
      arithmetic_instruction->kind = IR_constInt;
      ir_operand_number_literal(arithmetic_instruction, 1, result);

      ++num_modifications;
      instruction = arithmetic_instruction;
      continue;
    }
    instruction = instruction->next;
  }
  return num_modifications;
}

/***************************************
 * PATTERNS FOR PEEPHOLE OPTIMIZATIONS *
 ***************************************/
// All patterns correspond to a sequence of two or more IR instructions.

struct ir_section *single_const_arithmetic_pattern(enum ir_instruction_kind kind) {
  struct ir_section *pattern = NULL;
  struct ir_instruction *instruction;

  instruction = ir_instruction(IR_constInt);
  pattern = ir_append(pattern, instruction);

  instruction = ir_instruction(IR_loadWord);
  pattern = ir_append(pattern, instruction);

  instruction = ir_instruction(kind);
  pattern = ir_append(pattern, instruction);

  return pattern;
}

// Used to remove identity "a&-1"
struct ir_section *single_const_bitwise_and_pattern(enum ir_instruction_kind kind) {
  struct ir_section *pattern = NULL;
  struct ir_instruction *instruction;

  instruction = ir_instruction(IR_constInt);
  pattern = ir_append(pattern, instruction);

  instruction = ir_instruction(IR_unaryMinus);
  pattern = ir_append(pattern, instruction);

  instruction = ir_instruction(IR_loadWord);
  pattern = ir_append(pattern, instruction);

  instruction = ir_instruction(kind);
  pattern = ir_append(pattern, instruction);

  return pattern;
}

// Arithmetic expression with two constant operands
struct ir_section *double_const_arithmetic_pattern(enum ir_instruction_kind kind) {
  struct ir_section *pattern = NULL;
  struct ir_instruction *instruction;

  instruction = ir_instruction(IR_constInt);
  pattern = ir_append(pattern, instruction);

  instruction = ir_instruction(IR_constInt);
  pattern = ir_append(pattern, instruction);

  instruction = ir_instruction(kind);
  pattern = ir_append(pattern, instruction);

  return pattern;
}

struct ir_section *unary_const_arithmetic_pattern(enum ir_instruction_kind kind) {
  struct ir_section *pattern = NULL;
  struct ir_instruction *instruction;

  instruction = ir_instruction(IR_constInt);
  pattern = ir_append(pattern, instruction);

  instruction = ir_instruction(kind);
  pattern = ir_append(pattern, instruction);

  return pattern;
}

struct ir_section *goto_label_pattern(void) {
  struct ir_section *pattern = NULL;
  struct ir_instruction *instruction;

  instruction = ir_instruction(IR_goto);
  pattern = ir_append(pattern, instruction);

  instruction = ir_instruction(IR_label);
  pattern = ir_append(pattern, instruction);

  return pattern;
}

struct ir_section *duplicate_pattern_generate(enum ir_instruction_kind kind) {
  struct ir_section *pattern = NULL;
  struct ir_instruction *instruction;

  instruction = ir_instruction(kind);
  pattern = ir_append(pattern, instruction);

  instruction = ir_instruction(kind);
  pattern = ir_append(pattern, instruction);
  return pattern;
}

struct ir_section *load_store_pattern(void) {
  struct ir_section *pattern = NULL;
  struct ir_instruction *instruction;

  instruction = ir_instruction(IR_loadWord);
  pattern = ir_append(pattern, instruction);

  instruction = ir_instruction(IR_storeWord);
  pattern = ir_append(pattern, instruction);
  return pattern;
}

struct ir_section *const_int_address_of_pattern(void) {
  struct ir_section *pattern = NULL;
  struct ir_instruction *instruction;

  instruction = ir_instruction(IR_constInt);
  pattern = ir_append(pattern, instruction);

  instruction = ir_instruction(IR_addressOf);
  pattern = ir_append(pattern, instruction);
  return pattern;
}


/*********************************
 * OPTIMIZATION HELPER FUNCTIONS *
 *********************************/

bool pattern_match(struct ir_section *pattern, struct ir_instruction *current_instr) {
  struct ir_instruction *p = pattern->first;
  while (p && current_instr) {
    if (p->kind != current_instr->kind) {
      return false;
    }
    p = p->next;
    current_instr = current_instr->next;
  }
  if (p) return false; //Ensure full pattern has been matched
  return true;
}

// If instruction storing a value to register x is deleted, all references to registers 
// with value >(x+1) are decremented.
void update_register_references(struct ir_instruction *instruction, int val) {
  while (instruction) {
    for (int i = 0; i < 3; ++i) {
      if (instruction->operands[i].kind == OPERAND_TEMPORARY) {
        if (instruction->operands[i].data.temporary >= val) {
          --(instruction->operands[i].data.temporary);
        }
      }
    }
    instruction = instruction->next;
  }
}
