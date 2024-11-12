#ifndef _IR_H
#define _IR_H

#include <stdio.h>
#include <stdbool.h>

struct node;
struct symbol;
struct symbol_table;

enum ir_operand_kind {
  OPERAND_NUMBER,
  OPERAND_IDENTIFIER,
  OPERAND_TEMPORARY,
  OPERAND_FN_IDENTIFIER,
  OPERAND_USER_LABEL,
  OPERAND_GENERATED_LABEL,
  OPERAND_STRING_LABEL
};
struct ir_operand {
  enum ir_operand_kind kind;

  union {
    struct symbol *symbol;
    struct label_symbols {
      struct symbol *fn_symbol;
      struct symbol *lbl_symbol;
      char *lbl_name; //for goto
    } label_symbols;
    signed long number;
    int temporary;
    char *string_label;
  } data;
};

enum ir_instruction_kind {
  IR_addressOf,
  IR_loadWord,
  IR_loadHalfWord,
  IR_loadSignedHalfWord,
  IR_loadByte,
  IR_loadSignedByte,
  IR_storeWord,
  IR_storeHalfWord,
  IR_storeByte,
  IR_multSignedWord,
  IR_multUnsignedWord,
  IR_divSignedWord,
  IR_divUnsignedWord,
  IR_remSignedWord,
  IR_remUnsignedWord,
  IR_addSignedWord,
  IR_addUnsignedWord,
  IR_subSignedWord,
  IR_subUnsignedWord,
  IR_leftShiftWord,
  IR_rightShiftSignedWord,
  IR_rightShiftUnsignedWord,
  IR_ltSignedWord,
  IR_ltUnsignedWord,
  IR_leSignedWord,
  IR_leUnsignedWord,
  IR_geSignedWord,
  IR_geUnsignedWord,
  IR_gtSignedWord,
  IR_gtUnsignedWord,
  IR_eqWord,
  IR_neWord,
  IR_bitwiseAndWord,
  IR_bitwiseXorWord,
  IR_bitwiseOrWord,
  IR_unaryMinus,
  IR_unaryLogicalNegation,
  IR_unaryBitwiseNegation,
  IR_constInt,

  //casting opcodes
  IR_castWordToHalfWord,
  IR_castWordToByte,
  IR_castHalfWordToByte,

  IR_castUnsignedHalfWordToWord,
  IR_castSignedHalfWordToWord,
  IR_castUnsignedByteToHalfWord,
  IR_castSignedByteToHalfWord,
  IR_castUnsignedByteToWord,
  IR_castSignedByteToWord,

  //opcodes for produre/function definitions
  IR_procBegin,
  IR_procEnd,
  IR_returnWord,
  IR_returnHalfWord,
  IR_returnByte,

  //opcodes for procedure/function references
  IR_call,
  IR_parameter,
  IR_resultWord,
  IR_resultHalfWord,
  IR_resultByte,
  IR_label,
  IR_goto,
  IR_gotoIfFalse,
  IR_gotoIfTrue,

  // Retained for compatibility with mips.c. NOT USED in ir.c
  IR_NO_OPERATION,
  IR_MULTIPLY,
  IR_DIVIDE,
  IR_ADD,
  IR_SUBTRACT,
  IR_LOAD_IMMEDIATE,
  IR_COPY,
  IR_PRINT_NUMBER
};
struct ir_instruction {
  enum ir_instruction_kind kind;
  struct ir_instruction *prev, *next;
  struct ir_operand operands[3];
};

struct ir_section {
  struct ir_instruction *first, *last;
};

//Constructors
struct ir_section *ir_section(struct ir_instruction *first, struct ir_instruction *last);
struct ir_instruction *ir_instruction(enum ir_instruction_kind kind);

// Functions for manipulating IR (some also used in optimization phase)
static struct ir_section *ir_concatenate(struct ir_section *before, struct ir_section *after);
struct ir_section *ir_append(struct ir_section *section, struct ir_instruction *instruction);
void ir_insert_instruction_after(struct ir_instruction *existing, struct ir_instruction *new);
struct ir_instruction *ir_insert_section_after(struct ir_instruction *existing, struct ir_section *new);
void ir_remove_instruction(struct ir_instruction *to_remove);
void ir_swap_instructions(struct ir_instruction *first, struct ir_instruction *second);

// IR operand constructors used in optimization phase
void ir_operand_copy(struct ir_instruction *instruction, int position, struct ir_operand *operand);
void ir_operand_number_literal(struct ir_instruction *instruction, int position, int number);

// Helper functions for generating IR
void ir_lvalue_to_rvalue(struct ir_section **ir, struct node *lvalue);
enum ir_instruction_kind ir_store_type(struct type_tree *ttree);

void ir_generate_for_translation_unit_list(struct node *translation_unit_list);
void ir_generate_top_level_decl(struct node *top_level_decl);
void ir_generate_fn_def(struct node *fn_def);

void ir_generate_decl_or_statement_list(struct node *decl_or_statement_list);
void ir_generate_decl(struct node *decl);
void ir_generate_statement(struct node *stmt);

// Expressions
void ir_generate_binary_operation(struct node *binop);
void ir_generate_ternary_operation(struct node *ternop);
void ir_generate_unary_prefix_op(struct node *unary_prefix_op);
void ir_generate_postfix_expression(struct node *postfix_expr);
void ir_generate_conditional_expr(struct node *conditional_expr);
void ir_generate_for_expr(struct node *for_expr);
void ir_generate_cast_operation(struct node *cast_op);
void ir_generate_expression_list(struct node *expression_list);

// Basic types
void ir_generate_identifier(struct node *identifier);
void ir_generate_number(struct node *number);
void ir_generate_string(struct node *string);

// Print funtions
void ir_print_section(FILE *output, struct ir_section *section);
void ir_print_operand(FILE *output, struct ir_operand *operand);

#endif
