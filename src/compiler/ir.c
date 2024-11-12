#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "node.h"
#include "symbol.h"
#include "type.h"
#include "type_check.h"
#include "ir.h"

struct ir_section *global_ir;

// Forward declaration
static void ir_print_instruction(FILE *output, struct ir_instruction *instruction);

/****************
 * CONSTRUCTORS *
 ****************/

// An IR section is just a list of IR instructions. Each node has an associated
// IR section if any code is required to implement it.
struct ir_section *ir_section(struct ir_instruction *first, struct ir_instruction *last) {
  struct ir_section *code;
  code = malloc(sizeof(struct ir_section));
  assert(NULL != code);

  code->first = first;
  code->last = last;
  return code;
}

// An IR instruction represents a single 3-address statement.
 struct ir_instruction *ir_instruction(enum ir_instruction_kind kind) {
  struct ir_instruction *instruction;

  instruction = malloc(sizeof(struct ir_instruction));
  assert(NULL != instruction);

  instruction->kind = kind;

  instruction->next = NULL;
  instruction->prev = NULL;

  return instruction;
}

/*******************
 * IR MANIPULATION *
 *******************/

 // This joins two IR sections together into a new IR section.
 static struct ir_section *ir_concatenate(struct ir_section *before, struct ir_section *after) {
  /* patch the two sections together */
  if (!after) return before;
  if (!before) return after;
  before->last->next = after->first;
  after->first->prev = before->last;

  return ir_section(before->first, after->last);
}

// Appends IR instruction to provided IR section.
struct ir_section *ir_append(struct ir_section *section,
                                    struct ir_instruction *instruction) {
  if (NULL == section) {
    section = ir_section(instruction, instruction);

  } else if (NULL == section->first || NULL == section->last) {
    assert(NULL == section->first && NULL == section->last);
    section->first = instruction;
    section->last = instruction;
    instruction->prev = NULL;
    instruction->next = NULL;

  } else {
    instruction->next = section->last->next;
    if (NULL != instruction->next) {
      instruction->next->prev = instruction;
    }
    section->last->next = instruction;

    instruction->prev = section->last;
    section->last = instruction;
  }
  return section;
}

void ir_insert_instruction_after(struct ir_instruction *existing, struct ir_instruction *new) {
  struct ir_instruction *temp = existing->next;
  existing->next = new;
  new->prev = existing;
  new->next = temp;
  temp->prev = new;
}

struct ir_instruction *ir_insert_section_after(struct ir_instruction *existing, struct ir_section *new) {
  struct ir_instruction *temp = existing->next;
  existing->next = new->first;
  new->first->prev = existing;
  new->last->next = temp;
  temp->prev = new->last;
  return new->first;
}

void ir_remove_instruction(struct ir_instruction *to_remove) {
  to_remove->prev->next = to_remove->next;
  to_remove->next->prev = to_remove->prev;
}

void ir_swap_instructions(struct ir_instruction *first, struct ir_instruction *second) {
  first->prev->next = second;
  struct ir_instruction *temp = first->prev;
  first->prev = second;
  first->next = second->next;

  second->next->prev = first;
  second->prev = temp;
  second->next = first;
}


/***************************
 * IR OPERAND CONSTRUCTORS *
 ***************************/

static void ir_operand_number(struct ir_instruction *instruction, int position, struct node *number) {
  instruction->operands[position].kind = OPERAND_NUMBER;
  instruction->operands[position].data.number = number->data.number.value;
}

// Non-static function; used in optimization phase.
void ir_operand_number_literal(struct ir_instruction *instruction, int position, int number) {
  instruction->operands[position].kind = OPERAND_NUMBER;
  instruction->operands[position].data.number = number;
}

static void ir_operand_identifier(struct ir_instruction *instruction, int position, struct node *identifier) {
  assert(identifier->kind == NODE_IDENTIFIER);
  instruction->operands[position].kind = OPERAND_IDENTIFIER;
  instruction->operands[position].data.symbol = identifier->data.identifier.symbol;
}

static void ir_operand_fn_identifier(struct ir_instruction *instruction, int position, struct node *fn_identifier) {
  assert(fn_identifier->kind == NODE_IDENTIFIER);
  instruction->operands[position].kind = OPERAND_FN_IDENTIFIER;
  instruction->operands[position].data.symbol = fn_identifier->data.identifier.symbol;
}

static void ir_operand_user_label(struct ir_instruction *instruction, int position,
                                  struct node *fn_identifier, struct node *user_label) {
  assert(fn_identifier->kind == NODE_IDENTIFIER);
  assert(user_label->kind == NODE_IDENTIFIER);
  instruction->operands[position].kind = OPERAND_USER_LABEL;
  instruction->operands[position].data.label_symbols.fn_symbol = fn_identifier->data.identifier.symbol;
  instruction->operands[position].data.label_symbols.lbl_symbol = user_label->data.identifier.symbol;
  instruction->operands[position].data.label_symbols.lbl_name = user_label->data.identifier.name;
}

static void ir_operand_generated_label(struct ir_instruction *instruction, int position) {
  static int next_generated_label;
  instruction->operands[position].kind = OPERAND_GENERATED_LABEL;
  instruction->operands[position].data.temporary = ++next_generated_label;
}

static void ir_operand_string_label(struct ir_instruction *instruction, int position, char *str_lbl) {
  instruction->operands[position].kind = OPERAND_STRING_LABEL;
  instruction->operands[position].data.string_label = str_lbl;
}

static void ir_operand_temporary(struct ir_instruction *instruction, int position) {
  static int next_temporary;
  instruction->operands[position].kind = OPERAND_TEMPORARY;
  instruction->operands[position].data.temporary = next_temporary++;
}

// Non-static function; used in optimization phase.
void ir_operand_copy(struct ir_instruction *instruction, int position, struct ir_operand *operand) {
  instruction->operands[position] = *operand;
}

/**********************************************
 * Helper functions used during IR generation *
 **********************************************/

// Converts node correponding to lvalue operand into an rvalue
void ir_lvalue_to_rvalue(struct ir_section **ir, struct node *lvalue) {
  struct ir_instruction *instruction;

  // "func() converted to rvalue by resultWord (or similar) instruction
  if ((lvalue->kind == NODE_POSTFIX_EXPR && 
       lvalue->data.postfix_expr.postfix_expr_type == POSTFIX_FN_CALL) || 
      (lvalue->kind == NODE_CONDITIONAL_EXPR && 
       lvalue->data.conditional_expr.conditional_expr->kind == NODE_POSTFIX_EXPR && 
       lvalue->data.conditional_expr.conditional_expr->data.postfix_expr.postfix_expr_type == POSTFIX_FN_CALL)) {
    
    struct type_tree *ttree = lvalue->type_tree;
    if (ttree->type->data.basic.datatype == TYPE_BASIC_CHAR) {
      instruction = ir_instruction(IR_resultByte);
    } else if (ttree->type->data.basic.datatype == TYPE_BASIC_SHORT) {
      instruction = ir_instruction(IR_resultHalfWord);
    } else {
      instruction = ir_instruction(IR_resultWord);
    }

    ir_operand_temporary(instruction, 0);
    *ir = ir_append(*ir, instruction);
    lvalue->ir_operand = &(instruction->operands[0]);
    return;
  }

  struct type_tree *ttree = lvalue->type_tree;
  if (!isBasicType(ttree)) {
    instruction = ir_instruction(IR_loadWord);
  } else if (ttree->type->data.basic.datatype == TYPE_BASIC_CHAR) {
    if (ttree->type->data.basic.is_unsigned) {
      instruction = ir_instruction(IR_loadByte);
    } else {
       instruction = ir_instruction(IR_loadSignedByte);
    }
  } else if (ttree->type->data.basic.datatype == TYPE_BASIC_SHORT) {
    if (ttree->type->data.basic.is_unsigned) {
      instruction = ir_instruction(IR_loadHalfWord);
    } else {
      instruction = ir_instruction(IR_loadSignedHalfWord);
    }
  } else {
    instruction = ir_instruction(IR_loadWord);
  }
  // Retrieve address
  ir_operand_temporary(instruction, 0);
  ir_operand_copy(instruction, 1, lvalue->ir_operand);
  *ir = ir_append(*ir, instruction);
  lvalue->ir_operand = &(instruction->operands[0]);
  lvalue->is_rvalue = true;
}

// Generates store instruction based on the size of the object being stored
enum ir_instruction_kind ir_store_type(struct type_tree *ttree) {
  if (!isBasicType(ttree)) return IR_storeWord;
  if (ttree->type->data.basic.datatype == TYPE_BASIC_CHAR) {
    return IR_storeByte;
  } else if (ttree->type->data.basic.datatype == TYPE_BASIC_SHORT) {
    return IR_storeHalfWord;
  }
  return IR_storeWord;
}


/*******************************
 * GENERATE IR FOR EXPRESSIONS *
 *******************************/

int parameter_num;
struct node *current_function_identifier;
struct ir_operand *escape_label_break;
struct ir_operand *escape_label_continue;
struct ir_operand *escape_label_return;

void ir_generate_for_translation_unit_list(struct node *translation_unit_list) {
  assert(NODE_TRANSLATION_UNIT_LIST == translation_unit_list->kind);
  if (NULL != translation_unit_list->data.translation_unit_list.init) {
    ir_generate_for_translation_unit_list(translation_unit_list->data.translation_unit_list.init);
  }
  if (NULL != translation_unit_list->data.translation_unit_list.translation_unit) {
    ir_generate_top_level_decl(translation_unit_list->data.translation_unit_list.translation_unit);
  }
  translation_unit_list->ir = translation_unit_list->data.translation_unit_list.translation_unit->ir;
  if (translation_unit_list->ir) {
    //ir_print_section(stdout, translation_unit_list->ir);
    global_ir = ir_concatenate(global_ir, translation_unit_list->ir);
  }
}

void ir_generate_top_level_decl(struct node *top_level_decl) {
  assert(NODE_DECL == top_level_decl->kind || NODE_FN_DEF == top_level_decl->kind);
  top_level_decl->ir_generate(top_level_decl);
}

void ir_generate_fn_def(struct node *fn_def) {
  assert(NODE_FN_DEF == fn_def->kind);
  struct ir_instruction *instruction;
  escape_label_break = NULL;
  escape_label_continue = NULL;
  escape_label_return = NULL;

  // Extract identifier that names function

  struct node *fn_id;
  fn_id = fn_def->data.function_definition.fn_def_specifier;
  fn_id = fn_id->data.parameter_decl.declarator_opt_abstract;
  while (fn_id->kind == NODE_DECLARATOR) fn_id = fn_id->data.declarator.declarator;
  fn_id = fn_id->data.function_declarator.direct_declarator;
  fn_id = fn_id->data.declarator.declarator;
  current_function_identifier = fn_id;

  instruction = ir_instruction(IR_procBegin);
  ir_operand_fn_identifier(instruction, 0, fn_id);
  fn_def->ir = ir_append(fn_def->ir, instruction);

  if (fn_def->data.function_definition.compound_statement) {
    fn_def->data.function_definition.compound_statement -> ir_generate(fn_def->data.function_definition.compound_statement);
    fn_def->ir = ir_concatenate(fn_def->ir, fn_def->data.function_definition.compound_statement->ir);
  }

  // Add goto for return statements (to skip any statements after return)
  if (escape_label_return) {
    instruction = ir_instruction(IR_label);
    ir_operand_copy(instruction, 0, escape_label_return);
    fn_def->ir = ir_append(fn_def->ir, instruction);
  }


  instruction = ir_instruction(IR_procEnd);
  ir_operand_fn_identifier(instruction, 0, fn_id);
  fn_def->ir = ir_append(fn_def->ir, instruction);
}

void ir_generate_decl_or_statement_list(struct node *decl_or_statement_list) {
  assert(NODE_DECL_OR_STATEMENT_LIST == decl_or_statement_list->kind);
  struct node *init = decl_or_statement_list->data.decl_or_statement_list.init;
  if (init) {
    init->ir_generate(init);
    decl_or_statement_list->ir = init->ir;
  }

  struct node *decl_or_statement = decl_or_statement_list->data.decl_or_statement_list.decl_or_statement;
  if (decl_or_statement) {
    decl_or_statement->ir_generate(decl_or_statement);
    decl_or_statement_list->ir = ir_concatenate(decl_or_statement_list->ir, decl_or_statement->ir);

  }
}

void ir_generate_decl(struct node *decl) {
  assert(NODE_DECL == decl-> kind);
  decl->ir = NULL;
}

void ir_generate_statement(struct node *statement) {
  assert(NODE_STATEMENT == statement-> kind);
  struct node *expr, *stmt, *stmt2;
  struct ir_instruction *instruction, *temp, *temp2;
  struct ir_operand *condition;
  struct ir_operand *label, *label2;

  //Save global labels for restoration after statement IR is generated
  //(For nested looping statements)
  struct ir_operand *temp_escape_label_break;
  struct ir_operand *temp_escape_label_continue;
  temp_escape_label_break = escape_label_break;
  temp_escape_label_continue = escape_label_continue;

  switch (statement->data.statement.statement_type) {
  case STATEMENT_EXPRESSION:
    expr = statement->data.statement.expr;
    expr->ir_generate(expr);
    statement->ir = expr->ir;
      
    // Generate loadWord for identifier expression
    if (expr->kind == NODE_CONDITIONAL_EXPR && 
        expr->data.conditional_expr.conditional_expr->kind == NODE_IDENTIFIER) {
      ir_lvalue_to_rvalue(&(statement->ir), expr);
    }
    break;
  case STATEMENT_RETURN:
    expr = statement->data.statement.expr;
    if (!expr) break;
    expr->ir_generate(expr);
    statement->ir = expr->ir;
    if (!expr->is_rvalue) {
      ir_lvalue_to_rvalue(&(statement->ir), expr);
    }
    if (isBasicType(expr->type_tree) && expr->type_tree->type->data.basic.datatype == TYPE_BASIC_CHAR) {
      instruction = ir_instruction(IR_returnByte);
    } else if (isBasicType(expr->type_tree) && expr->type_tree->type->data.basic.datatype == TYPE_BASIC_SHORT) {
      instruction = ir_instruction(IR_returnHalfWord);
    } else {
      instruction = ir_instruction(IR_returnWord);
    }
    if (statement->ir->last->operands[0].kind == OPERAND_TEMPORARY) {
      ir_operand_copy(instruction, 0, &(statement->ir->last->operands[0]));
    } else {
      ir_operand_copy(instruction, 0, expr->ir_operand);
    }
    statement->ir = ir_append(statement->ir, instruction);

    // Label for return statements (to skip any statements after return)
    instruction = ir_instruction(IR_goto);
    if (escape_label_return) {
      ir_operand_copy(instruction, 0, escape_label_return);
    } else {
      ir_operand_generated_label(instruction, 0);
      escape_label_return = &(instruction->operands[0]);
    }
    statement->ir = ir_append(statement->ir, instruction);
    break;
  case STATEMENT_LABELED:
    instruction = ir_instruction(IR_label);
    ir_operand_user_label(instruction, 0, current_function_identifier, statement->data.statement.expr);
    statement->ir = ir_append(statement->ir, instruction);

    stmt = statement->data.statement.stmt;
    if (stmt) {
      stmt->ir_generate(stmt);
      statement->ir = ir_concatenate(statement->ir, stmt->ir);
    }
    break;
  case STATEMENT_GOTO:
    instruction = ir_instruction(IR_goto);
    ir_operand_user_label(instruction, 0, current_function_identifier, statement->data.statement.expr);
    statement->ir = ir_append(statement->ir, instruction);
    break;
  case STATEMENT_CONDITIONAL:
    expr = statement->data.statement.expr;
    expr->ir_generate(expr);
    statement->ir = expr->ir;
    if (!expr->is_rvalue) {
      ir_lvalue_to_rvalue(&(statement->ir), expr);
      condition = &(statement->ir->last->operands[0]);
    } else {
      condition = expr->ir_operand;
    }

    instruction = ir_instruction(IR_gotoIfFalse);
    ir_operand_copy(instruction, 0, condition);
    ir_operand_generated_label(instruction, 1);
    label = &(instruction->operands[1]);
    statement->ir = ir_append(statement->ir, instruction);

    //handle body of if statement
    stmt = statement->data.statement.stmt;
    stmt->ir_generate(stmt);
    statement->ir = ir_concatenate(statement->ir, stmt->ir);

    instruction = ir_instruction(IR_label);
    ir_operand_copy(instruction, 0, label);
    statement->ir = ir_append(statement->ir, instruction);

    // hand if-else
    stmt2 = statement->data.statement.stmt2;
    if (stmt2) { 
      instruction = ir_instruction(IR_gotoIfTrue);
      ir_operand_copy(instruction, 0, condition);
      ir_operand_generated_label(instruction, 1);
      label = &(instruction->operands[1]);
      statement->ir = ir_append(statement->ir, instruction);

      stmt2->ir_generate(stmt2);
      statement->ir = ir_concatenate(statement->ir, stmt2->ir);

      instruction = ir_instruction(IR_label);
      ir_operand_copy(instruction, 0, label);
      statement->ir = ir_append(statement->ir, instruction);
    }
    break;
  case STATEMENT_COMPOUND:
    stmt = statement->data.statement.expr;
    if (!stmt) break;
    stmt->ir_generate(stmt);
    statement->ir = stmt->ir;
    break;
  case STATEMENT_ITERATIVE_WHILE:
    instruction = ir_instruction(IR_label);
    ir_operand_generated_label(instruction, 0);
    label = &(instruction->operands[0]);
    escape_label_continue = label;
    statement->ir = ir_append(statement->ir, instruction);

    instruction = ir_instruction(IR_label);
    ir_operand_generated_label(instruction, 0);
    label2 = &(instruction->operands[0]);
    escape_label_break = label2;
    temp = instruction; //wait to append

    expr = statement->data.statement.expr;
    expr->ir_generate(expr);
    statement->ir = ir_concatenate(statement->ir, expr->ir);
    if (!expr->is_rvalue) {
      ir_lvalue_to_rvalue(&(statement->ir), expr);
      condition = &(statement->ir->last->operands[0]);
    } else {
      condition = expr->ir_operand;
    }

    instruction = ir_instruction(IR_gotoIfFalse);
    ir_operand_copy(instruction, 0, condition);
    ir_operand_copy(instruction, 1, label2);
    statement->ir = ir_append(statement->ir, instruction);

    //handle body of while statement
    stmt = statement->data.statement.stmt;
    stmt->ir_generate(stmt);
    statement->ir = ir_concatenate(statement->ir, stmt->ir);

    instruction = ir_instruction(IR_goto);
    ir_operand_copy(instruction, 0, label);
    statement->ir = ir_append(statement->ir, instruction);

    statement->ir = ir_append(statement->ir, temp);
    escape_label_break = NULL;
    escape_label_continue = NULL;
    break;
  case STATEMENT_ITERATIVE_DO:
    instruction = ir_instruction(IR_label);
    ir_operand_generated_label(instruction, 0);
    label = &(instruction->operands[0]);
    statement->ir = ir_append(statement->ir, instruction);

    // Label for continue statements
    instruction = ir_instruction(IR_label);
    ir_operand_generated_label(instruction, 0);
    escape_label_continue = &(instruction->operands[0]);
    temp = instruction; //wait to append

    // Label for break statements
    instruction = ir_instruction(IR_label);
    ir_operand_generated_label(instruction, 0);
    escape_label_break = &(instruction->operands[0]);
    temp2 = instruction; //wait to append

    //handle body of do-while statement
    stmt = statement->data.statement.stmt;
    stmt->ir_generate(stmt);
    statement->ir = ir_concatenate(statement->ir, stmt->ir);

    statement->ir = ir_append(statement->ir, temp);

    expr = statement->data.statement.expr;
    expr->ir_generate(expr);
    statement->ir = ir_concatenate(statement->ir, expr->ir);
    if (!expr->is_rvalue) {
      ir_lvalue_to_rvalue(&(statement->ir), expr);
      condition = &(statement->ir->last->operands[0]);
    } else {
      condition = expr->ir_operand;
    }

    instruction = ir_instruction(IR_gotoIfTrue);
    ir_operand_copy(instruction, 0, condition);
    ir_operand_copy(instruction, 1, label);
    statement->ir = ir_append(statement->ir, instruction);

    statement->ir = ir_append(statement->ir, temp2);
    escape_label_break = NULL;
    escape_label_continue = NULL;
    break;
  case STATEMENT_ITERATIVE_FOR:
    // Label for continue statements
    instruction = ir_instruction(IR_label);
    ir_operand_generated_label(instruction, 0);
    escape_label_continue = &(instruction->operands[0]);

    // Label for break statements
    instruction = ir_instruction(IR_label);
    ir_operand_generated_label(instruction, 0);
    escape_label_break = &(instruction->operands[0]);
    temp = instruction;

    expr = statement->data.statement.expr;
    if (expr) expr->ir_generate(expr);
    statement->ir = ir_concatenate(statement->ir, expr->ir);

    stmt = statement->data.statement.stmt;
    if (stmt) {
      stmt->ir_generate(stmt);
      statement->ir = ir_concatenate(statement->ir, stmt->ir);
    }

    struct node *update = NULL;
    if (expr) update = expr->data.for_expr.update;
    if (update) {
      update->ir_generate(update);
      statement->ir = ir_concatenate(statement->ir, update->ir);
    }

    instruction = ir_instruction(IR_goto);
    ir_operand_copy(instruction, 0, escape_label_continue);
    statement->ir = ir_append(statement->ir, instruction);
    statement->ir = ir_append(statement->ir, temp);
    escape_label_break = NULL;
    escape_label_continue = NULL;
    break;
  case STATEMENT_CONTINUE:
    if (!escape_label_continue) {
      printf("\n\n***ERROR: 'continue' statement must be within looping construct.***\n\n");
    } else {
      instruction = ir_instruction(IR_goto);
      ir_operand_copy(instruction, 0, escape_label_continue);
      statement->ir = ir_append(statement->ir, instruction);
    }
    break;
  case STATEMENT_BREAK:
    if (!escape_label_break) {
      printf("\n\n***ERROR: 'continue' statement must be within looping construct.***\n\n");
    } else {
      instruction = ir_instruction(IR_goto);
      ir_operand_copy(instruction, 0, escape_label_break);
      statement->ir = ir_append(statement->ir, instruction);
    }
    break;
  default:
    break;
  }

  //Restore global labels around looping constructs
  if ((statement->data.statement.statement_type == STATEMENT_ITERATIVE_DO) ||
      (statement->data.statement.statement_type == STATEMENT_ITERATIVE_FOR) ||
      (statement->data.statement.statement_type == STATEMENT_ITERATIVE_WHILE)) {
    escape_label_break = temp_escape_label_break;
    escape_label_continue = temp_escape_label_continue;
  }
}

void ir_generate_for_expr(struct node *for_expr) {
  assert(NODE_FOR_EXPR == for_expr-> kind);
  struct node *init, *condition;
  struct ir_instruction *instruction;

  init = for_expr->data.for_expr.init;
  condition = for_expr->data.for_expr.condition;

  if (init) {
    init->ir_generate(init);
    for_expr->ir = ir_concatenate(for_expr->ir, init->ir);
  } 
  instruction = ir_instruction(IR_label);
  ir_operand_copy(instruction, 0, escape_label_continue);
  for_expr->ir = ir_append(for_expr->ir, instruction);

  if (condition) {
    condition->ir_generate(condition);
    for_expr->ir = ir_concatenate(for_expr->ir, condition->ir);

    instruction = ir_instruction(IR_gotoIfFalse);
    if (condition->is_rvalue) {
      ir_operand_copy(instruction, 0, condition->ir_operand);
    } else {
      ir_lvalue_to_rvalue(&(for_expr->ir), condition);
      ir_operand_copy(instruction, 0, &(for_expr->ir->last->operands[0]));
    }
    ir_operand_copy(instruction, 1, escape_label_break);
    for_expr->ir = ir_append(for_expr->ir, instruction);
  }

}

void ir_generate_binary_operation(struct node *binop) {
  assert(NODE_BINARY_OPERATION == binop-> kind);

  struct node *l_op = binop->data.binary_operation.left_operand;
  struct node *r_op = binop->data.binary_operation.right_operand;
  l_op->ir_generate(l_op);
  binop->ir = ir_concatenate(binop->ir, l_op->ir);

  // Preserve "short-circuit" behavior for logical operators
  if (binop->data.binary_operation.operation != BINOP_LOGICAL_OR &&
      binop->data.binary_operation.operation != BINOP_LOGICAL_AND) {
    r_op->ir_generate(r_op);
    binop->ir = ir_concatenate(binop->ir, r_op->ir);
  }

  struct ir_instruction *instruction, *instruction2;
  struct ir_operand *label, *label2, *result_reg;
  switch(binop->data.binary_operation.operation) {
  case BINOP_ADDITION:
  case BINOP_MULTIPLICATION:
  case BINOP_DIVISION:
  case BINOP_SUBTRACTION:
  case BINOP_MODDIV:
  case BINOP_LEFT_SHIFT:
  case BINOP_RIGHT_SHIFT:
  case BINOP_LESS:
  case BINOP_LESS_EQUAL:
  case BINOP_GREATER:
  case BINOP_GREATER_EQUAL:
  case BINOP_EQUALITY:
  case BINOP_INEQUALITY:
  case BINOP_BITWISE_AND:
  case BINOP_BITWISE_OR:
  case BINOP_BITWISE_XOR:
    if (!l_op->is_rvalue) {
      ir_lvalue_to_rvalue(&(binop->ir), l_op);
    }
    if (!r_op->is_rvalue) {
      ir_lvalue_to_rvalue(&(binop->ir), r_op);
    }

    switch(binop->data.binary_operation.operation) {
    case BINOP_ADDITION:
      binop->is_rvalue = true;

      //Handle pointer addition, left operand is pointer
      if (isPointerType(l_op->type_tree)) {
        char pointerSize[10];
        sprintf(pointerSize, "%d", pointerSizeOf(l_op->type_tree->child));
        instruction = ir_instruction(IR_constInt);
        ir_operand_temporary(instruction, 0);
        ir_operand_number(instruction, 1, node_number(l_op->location, pointerSize));
        binop->ir = ir_append(binop->ir, instruction);

        instruction = ir_instruction(IR_multSignedWord);
        ir_operand_temporary(instruction, 0);
        ir_operand_copy(instruction, 1, r_op->ir_operand);
        ir_operand_copy(instruction, 2, &(binop->ir->last->operands[0])); //constInt
        binop->ir = ir_append(binop->ir, instruction);

        r_op->ir_operand = &(binop->ir->last->operands[0]); //multSignedWord
        binop->is_rvalue = false;
      }

      //Handle pointer addition, right operand is pointer
      if (isPointerType(r_op->type_tree)) {
        char pointerSize[10];
        sprintf(pointerSize, "%d", pointerSizeOf(l_op->type_tree->child));
        instruction = ir_instruction(IR_constInt);
        ir_operand_temporary(instruction, 0);
        ir_operand_number(instruction, 1, node_number(r_op->location, pointerSize));
        binop->ir = ir_append(binop->ir, instruction);

        instruction = ir_instruction(IR_multSignedWord);
        ir_operand_temporary(instruction, 0);
        ir_operand_copy(instruction, 1, l_op->ir_operand);
        ir_operand_copy(instruction, 2, &(binop->ir->last->operands[0])); //constInt
        binop->ir = ir_append(binop->ir, instruction);

        l_op->ir_operand = &(binop->ir->last->operands[0]); //multSignedWord
        binop->is_rvalue = false;
      }

      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_addUnsignedWord);
      } else {
        instruction = ir_instruction(IR_addSignedWord);
      }
      break;
    case BINOP_MULTIPLICATION:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_multUnsignedWord);
      } else {
        instruction = ir_instruction(IR_multSignedWord);
      }
      binop->is_rvalue = true;
      break;
    case BINOP_DIVISION:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_divUnsignedWord);
      } else {
        instruction = ir_instruction(IR_divSignedWord);
      }
      binop->is_rvalue = true;
      break;
    case BINOP_SUBTRACTION:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_subUnsignedWord);
      } else {
        instruction = ir_instruction(IR_subSignedWord);
      }

      binop->is_rvalue = true;
      break;
    case BINOP_MODDIV:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_remUnsignedWord);
      } else {
        instruction = ir_instruction(IR_remSignedWord);
      }
      binop->is_rvalue = true;
      break;
    case BINOP_LEFT_SHIFT:
      instruction = ir_instruction(IR_leftShiftWord);
      binop->is_rvalue = true;
      break;
    case BINOP_RIGHT_SHIFT:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_rightShiftUnsignedWord);
      } else {
        instruction = ir_instruction(IR_rightShiftSignedWord);
      }
      binop->is_rvalue = true;
      break;
    case BINOP_LESS:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_ltUnsignedWord);
      } else {
        instruction = ir_instruction(IR_ltSignedWord);
      }
      binop->is_rvalue = true;
      break;
    case BINOP_LESS_EQUAL:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_leUnsignedWord);
      } else {
        instruction = ir_instruction(IR_leSignedWord);
      }
      binop->is_rvalue = true;
      break;
    case BINOP_GREATER:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_gtUnsignedWord);
      } else {
        instruction = ir_instruction(IR_gtSignedWord);
      }
      binop->is_rvalue = true;
      break;
    case BINOP_GREATER_EQUAL:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_geUnsignedWord);
      } else {
        instruction = ir_instruction(IR_geSignedWord);
      }
      binop->is_rvalue = true;
      break;
    case BINOP_EQUALITY:
      instruction = ir_instruction(IR_eqWord);
      binop->is_rvalue = true;
      break;
    case BINOP_INEQUALITY:
      instruction = ir_instruction(IR_neWord);
      binop->is_rvalue = true;
      break;
    case BINOP_BITWISE_AND:
      instruction = ir_instruction(IR_bitwiseAndWord);
      binop->is_rvalue = true;
      break;
    case BINOP_BITWISE_OR:
      instruction = ir_instruction(IR_bitwiseOrWord);
      binop->is_rvalue = true;
      break;
    case BINOP_BITWISE_XOR:
      instruction = ir_instruction(IR_bitwiseXorWord);
      binop->is_rvalue = true;
      break;
    default:
      exit(1); //Not possible
    }

    ir_operand_temporary(instruction, 0);
    ir_operand_copy(instruction, 1, l_op->ir_operand);
    ir_operand_copy(instruction, 2, r_op->ir_operand);
    binop->ir = ir_append(binop->ir, instruction);
    binop->ir_operand = &(instruction->operands[0]);

    //Add scaling for subtraction of like pointers
    if (binop->data.binary_operation.operation == BINOP_SUBTRACTION &&
      isPointerType(l_op->type_tree) && isPointerType(r_op->type_tree)) {
      //Calculate sizeof pointer, load constInt with that value
      instruction = ir_instruction(IR_constInt);
      char size[10];
      sprintf(size, "%d", pointerSizeOf(l_op->type_tree->child));
      ir_operand_temporary(instruction, 0);
      ir_operand_number(instruction, 1, node_number(binop->location, size));
      binop->ir = ir_append(binop->ir, instruction);

      instruction = ir_instruction(IR_divSignedWord);
      ir_operand_temporary(instruction, 0);
      ir_operand_copy(instruction, 1, binop->ir_operand);
      ir_operand_copy(instruction, 2, &(binop->ir->last->operands[0])); //constInt above
      binop->ir = ir_append(binop->ir, instruction);
      binop->ir_operand = &(instruction->operands[0]);
    }
    break;    
  case BINOP_ASSIGN:
    instruction = ir_instruction(ir_store_type(l_op->type_tree));
    if (!r_op->is_rvalue) {
      ir_lvalue_to_rvalue(&(binop->ir), r_op);
    }

    ir_operand_copy(instruction, 0, l_op->ir_operand);
    ir_operand_copy(instruction, 1, r_op->ir_operand);
    binop->ir = ir_append(binop->ir, instruction);
    binop->ir_operand = &(instruction->operands[0]);
    binop->is_rvalue = false;
    break;
  case BINOP_ASSIGN_ADDITION:
  case BINOP_ASSIGN_SUBTRACTION:
  case BINOP_ASSIGN_MULTIPLICATION:
  case BINOP_ASSIGN_DIVISION:
  case BINOP_ASSIGN_MODDIV:
  case BINOP_ASSIGN_LSHIFT:
  case BINOP_ASSIGN_RSHIFT:
  case BINOP_ASSIGN_BITWISE_AND:
  case BINOP_ASSIGN_BITWISE_XOR:
  case BINOP_ASSIGN_BITWISE_OR:
    if (binop->data.binary_operation.operation == BINOP_ASSIGN_ADDITION || 
        binop->data.binary_operation.operation == BINOP_ASSIGN_SUBTRACTION) {

      //Handle pointer addition/subtraction
      if (isPointerType(l_op->type_tree)) {
        char pointerSize[10];
        sprintf(pointerSize, "%d", pointerSizeOf(l_op->type_tree->child));
        instruction = ir_instruction(IR_constInt);
        ir_operand_temporary(instruction, 0);
        ir_operand_number(instruction, 1, node_number(l_op->location, pointerSize));
        binop->ir = ir_append(binop->ir, instruction);

        instruction = ir_instruction(IR_multSignedWord);
        ir_operand_temporary(instruction, 0);
        ir_operand_copy(instruction, 1, r_op->ir_operand);
        ir_operand_copy(instruction, 2, &(binop->ir->last->operands[0])); //constInt
        binop->ir = ir_append(binop->ir, instruction);

        r_op->ir_operand = &(binop->ir->last->operands[0]); //multSignedWord
        binop->is_rvalue = false;
      }

    }

    // Generate store instruction (to be appended later, after arithmetic instruction)
    instruction = ir_instruction(ir_store_type(l_op->type_tree));
    ir_operand_copy(instruction, 0, l_op->ir_operand);

    if (!r_op->is_rvalue) {
      ir_lvalue_to_rvalue(&(binop->ir), r_op);
    }
    ir_lvalue_to_rvalue(&(binop->ir), l_op);

    // Generate instruction for arithmetic operation
    switch(binop->data.binary_operation.operation) {
    case BINOP_ASSIGN_ADDITION:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction2 = ir_instruction(IR_addUnsignedWord);
      } else {
        instruction2 = ir_instruction(IR_addSignedWord);
      }
      break;
    case BINOP_ASSIGN_MULTIPLICATION:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction2 = ir_instruction(IR_multUnsignedWord);
      } else {
        instruction2 = ir_instruction(IR_multSignedWord);
      }
      break;
    case BINOP_ASSIGN_DIVISION:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction2 = ir_instruction(IR_divUnsignedWord);
      } else {
        instruction2 = ir_instruction(IR_divSignedWord);
      }
      break;
    case BINOP_ASSIGN_SUBTRACTION:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction2 = ir_instruction(IR_subUnsignedWord);
      } else {
        instruction2 = ir_instruction(IR_subSignedWord);
      }
      break;
    case BINOP_ASSIGN_MODDIV:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction2 = ir_instruction(IR_remUnsignedWord);
      } else {
        instruction2 = ir_instruction(IR_remSignedWord);
      }
      break;
    case BINOP_ASSIGN_LSHIFT:
      instruction2 = ir_instruction(IR_leftShiftWord);
      break;
    case BINOP_ASSIGN_RSHIFT:
      if (isBasicType(l_op->type_tree) && l_op->type_tree->type->data.basic.is_unsigned) {
        instruction2 = ir_instruction(IR_rightShiftUnsignedWord);
      } else {
        instruction2 = ir_instruction(IR_rightShiftSignedWord);
      }
      break;
    case BINOP_ASSIGN_BITWISE_AND:
      instruction2 = ir_instruction(IR_bitwiseAndWord);
      break;
    case BINOP_ASSIGN_BITWISE_OR:
      instruction2 = ir_instruction(IR_bitwiseOrWord);
      break;
    case BINOP_ASSIGN_BITWISE_XOR:
      instruction2 = ir_instruction(IR_bitwiseXorWord);
      break;
    default:
      exit(1); //Not possible
    }
    ir_operand_temporary(instruction2, 0);
    ir_operand_copy(instruction2, 1, l_op->ir_operand);
    ir_operand_copy(instruction2, 2, r_op->ir_operand);
    binop->ir = ir_append(binop->ir, instruction2);

    // Append store instruction
    ir_operand_copy(instruction, 1, &(instruction2->operands[0]));
    binop->ir = ir_append(binop->ir, instruction);

    binop->ir_operand = &(instruction->operands[0]);
    binop->is_rvalue = false;
    break;
  case BINOP_LOGICAL_AND:
    // Logical AND
    instruction = ir_instruction(IR_gotoIfFalse);
    if (!l_op->is_rvalue) {
      ir_lvalue_to_rvalue(&(binop->ir), l_op);
      ir_operand_copy(instruction, 0, &(binop->ir->last->operands[0]));
    } else {
      ir_operand_copy(instruction, 0, l_op->ir_operand);
    }  
    ir_operand_generated_label(instruction, 1);
    label = &(instruction->operands[1]);
    binop->ir = ir_append(binop->ir, instruction);

    // Evaluate second operand
    r_op->ir_generate(r_op);
    binop->ir = ir_concatenate(binop->ir, r_op->ir);
    instruction = ir_instruction(IR_gotoIfFalse);
    if (!r_op->is_rvalue) {
      ir_lvalue_to_rvalue(&(binop->ir), r_op);
      ir_operand_copy(instruction, 0, &(binop->ir->last->operands[0]));
    } else {
      ir_operand_copy(instruction, 0, r_op->ir_operand);
    }  
    ir_operand_copy(instruction, 1, label);
    binop->ir = ir_append(binop->ir, instruction);

    // All operands have evaluated to true
    instruction = ir_instruction(IR_constInt);
    ir_operand_temporary(instruction, 0);
    result_reg = &(instruction->operands[0]);
    ir_operand_number(instruction, 1, node_number(binop->location, "1"));
    binop->ir = ir_append(binop->ir, instruction);

    // Add goto
    instruction = ir_instruction(IR_goto);
    ir_operand_generated_label(instruction, 0);
    label2 = &(instruction->operands[0]);
    binop->ir = ir_append(binop->ir, instruction);

    // Add label
    instruction = ir_instruction(IR_label);
    ir_operand_copy(instruction, 0, label);
    binop->ir = ir_append(binop->ir, instruction);

    // Only reached if at least one operand evaluated to false.
    instruction = ir_instruction(IR_constInt);
    ir_operand_copy(instruction, 0, result_reg);
    ir_operand_number(instruction, 1, node_number(binop->location, "0"));
    binop->ir = ir_append(binop->ir, instruction);

    // Add label
    instruction = ir_instruction(IR_label);
    ir_operand_copy(instruction, 0, label2);
    binop->ir = ir_append(binop->ir, instruction);
    
    binop->ir_operand = result_reg;
    binop->is_rvalue = true;
    break;
  case BINOP_LOGICAL_OR:
      // Logical OR
    instruction = ir_instruction(IR_gotoIfTrue);
    if (!l_op->is_rvalue) {
      ir_lvalue_to_rvalue(&(binop->ir), l_op);
      ir_operand_copy(instruction, 0, &(binop->ir->last->operands[0]));
    } else {
      ir_operand_copy(instruction, 0, l_op->ir_operand);
    }  
    ir_operand_generated_label(instruction, 1);
    label = &(instruction->operands[1]);
    binop->ir = ir_append(binop->ir, instruction);

    // Evaluate second operand
    r_op->ir_generate(r_op);
    binop->ir = ir_concatenate(binop->ir, r_op->ir);
    instruction = ir_instruction(IR_gotoIfTrue);
    if (!r_op->is_rvalue) {
      ir_lvalue_to_rvalue(&(binop->ir), r_op);
      ir_operand_copy(instruction, 0, &(binop->ir->last->operands[0]));
    } else {
      ir_operand_copy(instruction, 0, r_op->ir_operand);
    }  
    ir_operand_copy(instruction, 1, label);
    binop->ir = ir_append(binop->ir, instruction);

    // No operand evaluated to true.
    instruction = ir_instruction(IR_constInt);
    ir_operand_temporary(instruction, 0);
    result_reg = &(instruction->operands[0]);
    ir_operand_number(instruction, 1, node_number(binop->location, "0"));
    binop->ir = ir_append(binop->ir, instruction);

    // Add goto
    instruction = ir_instruction(IR_goto);
    ir_operand_generated_label(instruction, 0);
    label2 = &(instruction->operands[0]);
    binop->ir = ir_append(binop->ir, instruction);

    // Add label
    instruction = ir_instruction(IR_label);
    ir_operand_copy(instruction, 0, label);
    binop->ir = ir_append(binop->ir, instruction);

    // At least one operand evaluated to true.
    instruction = ir_instruction(IR_constInt);
    ir_operand_copy(instruction, 0, result_reg);
    ir_operand_number(instruction, 1, node_number(binop->location, "1"));
    binop->ir = ir_append(binop->ir, instruction);

    // Add label
    instruction = ir_instruction(IR_label);
    ir_operand_copy(instruction, 0, label2);
    binop->ir = ir_append(binop->ir, instruction);
    
    binop->ir_operand = result_reg;
    binop->is_rvalue = true;
    break;
  case BINOP_COMMA:
    if (!l_op->is_rvalue) {
      ir_lvalue_to_rvalue(&(binop->ir), l_op);
    }
    if (!r_op->is_rvalue) {
      ir_lvalue_to_rvalue(&(binop->ir), r_op);
      binop->ir_operand = &(binop->ir->last->operands[0]);
    } else {
      binop->ir_operand = l_op->ir_operand;
    }
    binop->is_rvalue = true;
    break;
  default:
    break;
  }
}

void ir_generate_ternary_operation(struct node *ternop) {
  assert(NODE_TERNARY_OPERATION == ternop-> kind);
  struct ir_instruction *instruction;
  struct ir_operand *condition, *label, *result_reg;
  struct node *first_op = ternop->data.ternary_operation.first_op;
  struct node *second_op = ternop->data.ternary_operation.second_op;
  struct node *third_op = ternop->data.ternary_operation.third_op;

  // Initialize register that will be used to store result
  instruction = ir_instruction(IR_constInt);
  ir_operand_temporary(instruction, 0);
  result_reg = &(instruction->operands[0]);
  ir_operand_number(instruction, 1, node_number(ternop->location, "0"));
  ternop->ir = ir_append(ternop->ir, instruction);

  // evaluate condition (first operand)
  first_op->ir_generate(first_op);
  ternop->ir = ir_concatenate(ternop->ir, first_op->ir);
  if (!first_op->is_rvalue) {
    ir_lvalue_to_rvalue(&(ternop->ir), first_op);
    condition = &(ternop->ir->last->operands[0]);
  } else {
    condition = first_op->ir_operand;
  }

  // Jump to IR code for third operand if condition is false
  instruction = ir_instruction(IR_gotoIfFalse);
  ir_operand_copy(instruction, 0, condition);
  ir_operand_generated_label(instruction, 1);
  label = &(instruction->operands[1]);
  ternop->ir = ir_append(ternop->ir, instruction);

  // Handle body of if statement
  second_op->ir_generate(second_op);
  ternop->ir = ir_concatenate(ternop->ir, second_op->ir);

  // Set value of result register equal to zero
  instruction = ir_instruction(IR_constInt);
  ir_operand_copy(instruction, 0, result_reg);
  ir_operand_number(instruction, 1, node_number(ternop->location, "0"));
  ternop->ir = ir_append(ternop->ir, instruction);

  // Or result with result register
  instruction = ir_instruction(IR_bitwiseOrWord);
  ir_operand_copy(instruction, 0, result_reg);
  if (!second_op->is_rvalue) {
    ir_lvalue_to_rvalue(&(ternop->ir), second_op);
    ir_operand_copy(instruction, 1, &(ternop->ir->last->operands[0]));
  } else {
    ir_operand_copy(instruction, 1, second_op->ir_operand);
  }  
  ir_operand_copy(instruction, 2, result_reg);
  ternop->ir = ir_append(ternop->ir, instruction);

  instruction = ir_instruction(IR_label);
  ir_operand_copy(instruction, 0, label);
  ternop->ir = ir_append(ternop->ir, instruction);

  // IR code for third operand
  instruction = ir_instruction(IR_gotoIfTrue);
  ir_operand_copy(instruction, 0, condition);
  ir_operand_generated_label(instruction, 1);
  label = &(instruction->operands[1]);
  ternop->ir = ir_append(ternop->ir, instruction);
  third_op->ir_generate(third_op);
  ternop->ir = ir_concatenate(ternop->ir, third_op->ir);

  // Set value of result register equal to zero
  instruction = ir_instruction(IR_constInt);
  ir_operand_copy(instruction, 0, result_reg);
  ir_operand_number(instruction, 1, node_number(ternop->location, "0"));
  ternop->ir = ir_append(ternop->ir, instruction);

  // or result with result register
  instruction = ir_instruction(IR_bitwiseOrWord);
  ir_operand_copy(instruction, 0, result_reg);
  if (!third_op->is_rvalue) {
    ir_lvalue_to_rvalue(&(ternop->ir), third_op);
    ir_operand_copy(instruction, 1, &(ternop->ir->last->operands[0]));
  } else {
    ir_operand_copy(instruction, 1, third_op->ir_operand);
  }
  ir_operand_copy(instruction, 2, result_reg);
  ternop->ir = ir_append(ternop->ir, instruction);

  instruction = ir_instruction(IR_label);
  ir_operand_copy(instruction, 0, label);
  ternop->ir = ir_append(ternop->ir, instruction);
  
  ternop->ir_operand = result_reg;
  ternop->is_rvalue = true;
}

void ir_generate_unary_prefix_op(struct node *unary_prefix_op) {
  assert(NODE_UNARY_PREFIX_OPERATION == unary_prefix_op-> kind);
  struct node *operand = unary_prefix_op->data.unary_prefix_operation.operand;
  operand->ir_generate(operand);
  unary_prefix_op->ir = ir_concatenate(unary_prefix_op->ir, operand->ir);

  struct ir_instruction *instruction;
  enum node_unary_prefix_operation op = unary_prefix_op->data.unary_prefix_operation.operation;
  switch(op) {
  case UNARY_PREINCREMENT:
  case UNARY_PREDECREMENT:
    instruction = ir_instruction(IR_constInt);
    ir_operand_temporary(instruction, 0);
    // sizeOf
    int val = isBasicType(operand->type_tree) ? 1 : pointerSizeOf(operand->type_tree->child);
    char s[10];
    sprintf(s, "%d", val);
    ir_operand_number(instruction, 1, node_number(unary_prefix_op->location, s));
    unary_prefix_op->ir = ir_append(unary_prefix_op->ir, instruction);

    // do addition or subtraction operation
    switch(op) {
    case UNARY_PREINCREMENT:
      if (isBasicType(operand->type_tree) && operand->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_addUnsignedWord);
      } else {
        instruction = ir_instruction(IR_addSignedWord);
      }
      break;
    case UNARY_PREDECREMENT:
      if (isBasicType(operand->type_tree) && operand->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_subUnsignedWord);
      } else {
        instruction = ir_instruction(IR_subSignedWord);
      }
      break;
    default:
      exit(1); //not possible
    }
    ir_lvalue_to_rvalue(&(unary_prefix_op->ir), operand); //operand guaranteed to be lvalue
    ir_operand_temporary(instruction, 0);
    ir_operand_copy(instruction, 1, &(unary_prefix_op->ir->last->operands[0]));
    ir_operand_copy(instruction, 2, &(unary_prefix_op->ir->last->prev->operands[0]));
    unary_prefix_op->ir = ir_append(unary_prefix_op->ir, instruction);;

    // store result
    instruction = ir_instruction(ir_store_type(operand->type_tree));
    ir_operand_copy(instruction, 0, &(unary_prefix_op->ir->last->prev->operands[1]));
    ir_operand_copy(instruction, 1, &(unary_prefix_op->ir->last->operands[0]));
    unary_prefix_op->ir = ir_append(unary_prefix_op->ir, instruction);

    unary_prefix_op->ir_operand = &(unary_prefix_op->ir->last->prev->prev->operands[1]);
    unary_prefix_op->is_rvalue = false;
    break;
  case UNARY_ADDRESS_EXPR:
    unary_prefix_op->ir_operand = operand->ir_operand;
    unary_prefix_op->is_rvalue = true;
    break;
  case UNARY_INDIRECTION_EXPR:
    unary_prefix_op->ir_operand = operand->ir_operand;
    if (isArrayType(operand->type_tree->child)) {
      unary_prefix_op->is_rvalue = true;
    } else {
      unary_prefix_op->is_rvalue = false;
    }

    if (unary_prefix_op->data.unary_prefix_operation.operand->kind == NODE_IDENTIFIER) {
      ir_lvalue_to_rvalue(&(unary_prefix_op->ir), operand);
      unary_prefix_op->ir_operand = &(unary_prefix_op->ir->last->operands[0]);
      unary_prefix_op->is_rvalue = false;
    }
    break;
  case UNARY_MINUS:
  case UNARY_PLUS:
  case UNARY_BITWISE_NEGATION:
  case UNARY_LOGICAL_NEGATION:

    switch(op) {
    case UNARY_MINUS:
      instruction = ir_instruction(IR_unaryMinus);
      break;
    case UNARY_BITWISE_NEGATION:
      instruction = ir_instruction(IR_unaryBitwiseNegation);
      break;
    case UNARY_LOGICAL_NEGATION:
      instruction = ir_instruction(IR_unaryLogicalNegation);
      break;
    case UNARY_PLUS:
      if (!operand->is_rvalue) {
        ir_lvalue_to_rvalue(&(unary_prefix_op->ir), operand);
        unary_prefix_op->ir_operand = &(unary_prefix_op->ir->last->operands[0]);
      } else {
        unary_prefix_op->ir_operand = operand->ir_operand;
      }
      unary_prefix_op->is_rvalue = true;
      return;
    default:
      exit(1); //not possible
    }

    if (!operand->is_rvalue) {
      ir_lvalue_to_rvalue(&(unary_prefix_op->ir), operand);
      ir_operand_copy(instruction, 1, &(unary_prefix_op->ir->last->operands[0]));
    }  else {
      ir_operand_copy(instruction, 1, operand->ir_operand);
    }
    ir_operand_temporary(instruction, 0);
    unary_prefix_op->ir = ir_append(unary_prefix_op->ir, instruction);
    unary_prefix_op->ir_operand = &(instruction->operands[0]);
    unary_prefix_op->is_rvalue = true;
    break;
  default: 
    break;
  }
}

void ir_generate_postfix_expression(struct node *postfix_expr) {
  assert(NODE_POSTFIX_EXPR == postfix_expr->kind);
  struct node *operand = postfix_expr->data.postfix_expr.operand;
  struct node *expression_list; //for postfix function call
  struct ir_instruction *instruction;
  enum node_postfix_expr op = postfix_expr->data.postfix_expr.postfix_expr_type;
  switch(op) {
  case POSTFIX_FN_CALL:
    // handle parameters, if any
    parameter_num = 0;
    expression_list = postfix_expr->data.postfix_expr.expression_list;
    if (expression_list) {
      expression_list->ir_generate(expression_list);
      postfix_expr->ir = ir_concatenate(postfix_expr->ir, expression_list->ir);
    }

    instruction = ir_instruction(IR_call);
    ir_operand_fn_identifier(instruction, 0, postfix_expr->data.postfix_expr.operand);
    postfix_expr->ir = ir_append(postfix_expr->ir, instruction);
    postfix_expr->is_rvalue = false;
    break;
  case POSTINCREMENT:
  case POSTDECREMENT:
    operand->ir_generate(operand);
    postfix_expr->ir = ir_concatenate(postfix_expr->ir, operand->ir);
  
    // store value of appropriate constant in register
    instruction = ir_instruction(IR_constInt);
    ir_operand_temporary(instruction, 0);
    int val = isBasicType(operand->type_tree) ? 1 : pointerSizeOf(operand->type_tree->child);
    char s[10];
    sprintf(s, "%d", val);
    ir_operand_number(instruction, 1, node_number(postfix_expr->location, s));
    postfix_expr->ir = ir_append(postfix_expr->ir, instruction);

    // do addition or subtraction operation
    switch(op) {
    case POSTINCREMENT:
      if (isBasicType(operand->type_tree) && operand->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_addUnsignedWord);
      } else {
        instruction = ir_instruction(IR_addSignedWord);
      }
      break;
    case POSTDECREMENT:
      if (isBasicType(operand->type_tree) && operand->type_tree->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_subUnsignedWord);
      } else {
        instruction = ir_instruction(IR_subSignedWord);
      }
      break;
    default:
      exit(1); //not possible
    }

    ir_lvalue_to_rvalue(&(postfix_expr->ir), operand); //operand guaranteed to be lvalue
    ir_operand_temporary(instruction, 0);
    ir_operand_copy(instruction, 1, &(postfix_expr->ir->last->operands[0]));
    ir_operand_copy(instruction, 2, &(postfix_expr->ir->last->prev->operands[0]));
    postfix_expr->ir = ir_append(postfix_expr->ir, instruction);;

    // store result
    instruction = ir_instruction(ir_store_type(operand->type_tree));
    ir_operand_copy(instruction, 0, &(postfix_expr->ir->last->prev->operands[1]));
    ir_operand_copy(instruction, 1, &(postfix_expr->ir->last->operands[0]));
    postfix_expr->ir = ir_append(postfix_expr->ir, instruction);


    postfix_expr->ir_operand = &(postfix_expr->ir->last->prev->prev->operands[0]);
    postfix_expr->is_rvalue = true;
    break;
  default:
    break;
  }
}

void ir_generate_expression_list(struct node *expression_list) {
  assert(NODE_EXPRESSION_LIST == expression_list->kind);
  struct node *init = expression_list->data.expression_list.init;
  if (init) {
    init->ir_generate(init);
    expression_list->ir = ir_concatenate(expression_list->ir, init->ir);
  }
  struct node *param = expression_list->data.expression_list.assignment_expr;
  param->ir_generate(param);
  expression_list->ir = ir_concatenate(expression_list->ir, param->ir);

  struct ir_instruction *instruction = ir_instruction(IR_parameter);
  ir_operand_number_literal(instruction, 0, parameter_num++);
  if (!param->is_rvalue) {
    ir_lvalue_to_rvalue(&(expression_list->ir), param);
    ir_operand_copy(instruction, 1, &(expression_list->ir->last->operands[0]));
  } else {
    ir_operand_copy(instruction, 1, param->ir_operand);
  }
  expression_list->ir = ir_append(expression_list->ir, instruction);
}

void ir_generate_cast_operation(struct node *cast_op) {
  assert(NODE_CAST_OPERATION == cast_op-> kind);
  struct node *operand = cast_op->data.cast_operation.operand;
  operand->ir_generate(operand);
  cast_op->ir = ir_concatenate(cast_op->ir, operand->ir);

  if (!operand->is_rvalue) {
    ir_lvalue_to_rvalue(&(cast_op->ir), operand);
  }
  struct ir_instruction *instruction = NULL;
  struct type_tree *dest_type = cast_op->type_tree;
  struct type_tree *source_type = operand->type_tree;

  //casts to word
  if (!isBasicType(dest_type) || dest_type->type->data.basic.datatype == TYPE_BASIC_INT || 
                                 dest_type->type->data.basic.datatype == TYPE_BASIC_LONG) {
    if (isBasicType(source_type) && source_type->type->data.basic.datatype == TYPE_BASIC_CHAR) {
      if (source_type->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_castUnsignedByteToWord);
      } else {
        instruction = ir_instruction(IR_castSignedByteToWord);
      }
    }
    if (isBasicType(source_type) && source_type->type->data.basic.datatype == TYPE_BASIC_SHORT) {
      if (source_type->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_castUnsignedHalfWordToWord);
      } else {
        instruction = ir_instruction(IR_castSignedHalfWordToWord);
      }
    }
  }

  //casts to half word
  if (isBasicType(dest_type) && dest_type->type->data.basic.datatype == TYPE_BASIC_SHORT){
    if (isBasicType(source_type) && source_type->type->data.basic.datatype == TYPE_BASIC_CHAR) {
      if (source_type->type->data.basic.is_unsigned) {
        instruction = ir_instruction(IR_castUnsignedByteToHalfWord);
      } else {
        instruction = ir_instruction(IR_castSignedByteToHalfWord);
      }
    }
  }

  //narrowing conversions
  if (!isBasicType(source_type) || source_type->type->data.basic.datatype == TYPE_BASIC_INT || 
                                 source_type->type->data.basic.datatype == TYPE_BASIC_LONG) {
    if (isBasicType(dest_type) && dest_type->type->data.basic.datatype == TYPE_BASIC_SHORT) {
      instruction = ir_instruction(IR_castWordToHalfWord);
    }
    if (isBasicType(dest_type) && dest_type->type->data.basic.datatype == TYPE_BASIC_CHAR) {
      instruction = ir_instruction(IR_castWordToByte);
    } 
  }
  if (isBasicType(source_type) && source_type->type->data.basic.datatype == TYPE_BASIC_SHORT) {
    if (isBasicType(dest_type) && dest_type->type->data.basic.datatype == TYPE_BASIC_CHAR) {
      instruction = ir_instruction(IR_castHalfWordToByte);
    }
  }

  if (instruction) {
    ir_operand_temporary(instruction, 0);
    ir_operand_copy(instruction, 1, operand->ir_operand);
    cast_op->ir = ir_append(cast_op->ir, instruction);
    cast_op->ir_operand = &(instruction->operands[0]);
    cast_op->is_rvalue = true;
  } else {
    cast_op->ir_operand = operand->ir_operand;
    cast_op->is_rvalue = true;
  }
}

void ir_generate_conditional_expr(struct node *conditional_expr) {
  assert(NODE_CONDITIONAL_EXPR == conditional_expr-> kind);
  struct node *c = conditional_expr->data.conditional_expr.conditional_expr;
  if (c) {
    c->ir_generate(c);
    conditional_expr->ir = ir_concatenate(conditional_expr->ir, c->ir);
  }
  conditional_expr->is_rvalue = c->is_rvalue;
  conditional_expr->ir_operand = c->ir_operand;
}

void ir_generate_identifier(struct node *identifier) {
  assert(NODE_IDENTIFIER == identifier-> kind);
  struct ir_instruction *instruction;

  instruction = ir_instruction(IR_addressOf);
  ir_operand_temporary(instruction, 0);
  ir_operand_identifier(instruction, 1, identifier);

  identifier->ir = ir_section(instruction, instruction);
  identifier->ir_operand = &(instruction->operands[0]);

  if (isArrayType(identifier->data.identifier.symbol->type_tree)) {
    identifier-> is_rvalue = true; 
  } else {
    identifier-> is_rvalue = false;
  }
}

void ir_generate_number(struct node *number) {
  assert(NODE_NUMBER == number-> kind);
  struct ir_instruction *instruction;

  instruction = ir_instruction(IR_constInt);
  ir_operand_temporary(instruction, 0);
  ir_operand_number(instruction, 1, number);
  number->ir = ir_section(instruction, instruction);
  number->ir_operand = &(instruction->operands[0]);
  number-> is_rvalue = true;

}

void ir_generate_string(struct node *string) {
  assert(NODE_STRING == string-> kind);
  struct ir_instruction *instruction;

  instruction = ir_instruction(IR_addressOf);
  ir_operand_temporary(instruction, 0);
  ir_operand_string_label(instruction, 1, string->data.string.string_literal_ptr->id);
  string->ir = ir_section(instruction, instruction);
  string->ir_operand = &(instruction->operands[0]);
  string-> is_rvalue = true;
}


/***********************
 * PRINT IR STRUCTURES *
 ***********************/

static void ir_print_opcode(FILE *output, enum ir_instruction_kind kind) {
  static char const * const instruction_names[] = {
    "addressOf",
    "loadWord",
    "loadHalfWord",
    "loadSignedHalfWord",
    "loadByte",
    "loadSignedByte",
    "storeWord",
    "storeHalfWord",
    "storeByte",
    "multSignedWord",
    "multUnsignedWord",
    "divSignedWord",
    "divUnsignedWord",
    "remSignedWord",
    "remUnsignedWord",
    "addSignedWord",
    "addUnsignedWord",
    "subSignedWord",
    "subUnsignedWord",
    "leftShiftWord",
    "rightShiftSignedWord",
    "rightShiftUnsignedWord",
    "ltSignedWord",
    "ltUnsignedWord",
    "leSignedWord",
    "leUnsignedWord",
    "geSignedWord",
    "geUnsignedWord",
    "gtSignedWord",
    "gtUnsignedWord",
    "eqWord",
    "neWord",
    "bitwiseAndWord",
    "bitwiseXorWord",
    "bitwiseOrWord",
    "unaryMinus",
    "unaryLogicalNegation",
    "unaryBitwiseNegation",
    "constInt",
    "castWordToHalfWord",
    "castWordToByte",
    "castHalfWordToByte",
    "castUnsignedHalfWordToWord",
    "castSignedHalfWordToWord",
    "castUnsignedByteToHalfWord",
    "castSignedByteToHalfWord",
    "castUnsignedByteToWord",
    "castSignedByteToWord",
    "procBegin",
    "procEnd",
    "returnWord",
    "returnHalfWord",
    "returnByte",
    "call",
    "parameter",
    "resultWord",
    "resultHalfWord",
    "resultByte",
    "label",
    "goto",
    "gotoIfFalse",
    "gotoIfTrue",

    NULL
  };

  fprintf(output, "%-8s", instruction_names[kind]);
}

void ir_print_operand(FILE *output, struct ir_operand *operand) {
  switch (operand->kind) {
    case OPERAND_NUMBER:
      fprintf(output, "%10d", (signed int)operand->data.number);
      break;

    case OPERAND_TEMPORARY:
      fprintf(output, "     r%d", operand->data.temporary);
      break;

    case OPERAND_IDENTIFIER:
      fprintf(output, "     %s", operand->data.symbol->name);
      break;

    case OPERAND_FN_IDENTIFIER:
      if (0 == strcmp(operand->data.symbol->name, "main")) {
        fprintf(output, "  %s", operand->data.symbol->name);
      }
      else {
        fprintf(output, "  _Global_%s", operand->data.symbol->name);
      }
      break;

    case OPERAND_USER_LABEL:
      if (operand->data.label_symbols.lbl_symbol) {
        fprintf(output, "  _UserLabel_%s_%s", operand->data.label_symbols.fn_symbol->name, 
                                              operand->data.label_symbols.lbl_symbol->name);

      } else {
        fprintf(output, "  _UserLabel_%s_%s", operand->data.label_symbols.fn_symbol->name, 
                                              operand->data.label_symbols.lbl_name);
      }
      break;

    case OPERAND_GENERATED_LABEL:
      fprintf(output, "  _GeneratedLabel_%d", operand->data.temporary);
      break;

    case OPERAND_STRING_LABEL:
      fprintf(output, "  %s", operand->data.string_label);
      break;
  }
}
static void ir_print_instruction(FILE *output, struct ir_instruction *instruction) {
  fprintf(output, "(");
  ir_print_opcode(output, instruction->kind);
  fprintf(output, ",");

  switch (instruction->kind) {
  case IR_addressOf:
    ir_print_operand(output, &instruction->operands[0]);
    fprintf(output, ", ");
    ir_print_operand(output, &instruction->operands[1]);
    break;
  case IR_loadByte:
  case IR_loadSignedByte:
  case IR_loadHalfWord:
  case IR_loadSignedHalfWord:
  case IR_loadWord:
  case IR_storeWord:
  case IR_storeByte:
  case IR_storeHalfWord:
  case IR_constInt:
  case IR_castWordToHalfWord:
  case IR_castWordToByte:
  case IR_castHalfWordToByte:
  case IR_castUnsignedHalfWordToWord:
  case IR_castSignedHalfWordToWord:
  case IR_castUnsignedByteToHalfWord:
  case IR_castSignedByteToHalfWord:
  case IR_castUnsignedByteToWord:
  case IR_castSignedByteToWord:
  case IR_unaryMinus:
  case IR_unaryBitwiseNegation:
  case IR_unaryLogicalNegation:
  case IR_parameter:
  case IR_gotoIfFalse:
  case IR_gotoIfTrue:
    ir_print_operand(output, &instruction->operands[0]);
    fprintf(output, ", ");
    ir_print_operand(output, &instruction->operands[1]);
    break;
  case IR_addSignedWord:
  case IR_addUnsignedWord:
  case IR_multSignedWord:
  case IR_multUnsignedWord:
  case IR_divSignedWord:
  case IR_divUnsignedWord:
  case IR_remSignedWord:
  case IR_remUnsignedWord:
  case IR_subSignedWord:
  case IR_subUnsignedWord:
  case IR_leftShiftWord:
  case IR_rightShiftSignedWord:
  case IR_rightShiftUnsignedWord:
  case IR_ltSignedWord:
  case IR_ltUnsignedWord:
  case IR_leSignedWord:
  case IR_leUnsignedWord:
  case IR_gtSignedWord:
  case IR_gtUnsignedWord:
  case IR_geSignedWord:
  case IR_geUnsignedWord:
  case IR_eqWord:
  case IR_neWord:
  case IR_bitwiseAndWord:
  case IR_bitwiseOrWord:
  case IR_bitwiseXorWord:
    ir_print_operand(output, &instruction->operands[0]);
    fprintf(output, ", ");
    ir_print_operand(output, &instruction->operands[1]);
    fprintf(output, ", ");
    ir_print_operand(output, &instruction->operands[2]);
    break;
  case IR_call:
  case IR_resultWord:
  case IR_resultHalfWord:
  case IR_resultByte:
  case IR_procBegin:
  case IR_procEnd:
  case IR_returnByte:
  case IR_returnHalfWord:
  case IR_returnWord:
  case IR_label:
  case IR_goto:
    ir_print_operand(output, &instruction->operands[0]);
    break;
  default:
    break;
  }

  fprintf(output, ")");
}

void ir_print_section(FILE *output, struct ir_section *section) {
  struct ir_instruction *iter = section->first;
  struct ir_instruction *prev = NULL;
  while (NULL != iter && section->last != prev) {
    //fprintf(output, "%5d     ", i++);
    ir_print_instruction(output, iter);
    fprintf(output, "\n");

    iter = iter->next;
  }
}
