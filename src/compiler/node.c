#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdbool.h>

#include "node.h"
#include "symbol.h"
#include "type.h"
#include "ir.h"

extern bool annotate;
struct string_literal_table string_literal_table;


/***************************
 * CREATE PARSE TREE NODES *
 ***************************/

/* Allocate and initialize a generic node. */
static struct node *node_create(enum node_kind kind, YYLTYPE location) {
  struct node *n;
  n = malloc(sizeof(struct node));
  assert(NULL != n);

  n->kind = kind;
  n->location = location;

  n->nr.val = 0;
  n->ir = NULL;
  n->ir_operand = NULL;
  return n;
}

/*
 * node_number - allocate a node to represent a number
 */
struct node *node_number(YYLTYPE location, char *text)
{
  struct node *node = node_create(NODE_NUMBER, location);
  errno = 0;
  node->data.number.value = strtoul(text, NULL, 10);
  if (node->data.number.value == ULONG_MAX && ERANGE == errno) {
    /* Strtoul indicated overflow. */
    node->data.number.overflow = true;
    node->data.number.result.type = type_basic(false, TYPE_BASIC_LONG);
  } else if (node->data.number.value > 0xFFFFFFFFul) {
    /* Value is too large for 32-bit unsigned long type. */
    node->data.number.overflow = true;
    node->data.number.result.type = type_basic(false, TYPE_BASIC_LONG);
  } else if (node->data.number.value > 0x7FFFFFFFul) {
    /* value too large to be stored in anything but an unsigned long */
    node->data.number.overflow = false;
    node->data.number.result.type = type_basic(true, TYPE_BASIC_LONG);
  } else {
    node->data.number.overflow = false;
    node->data.number.result.type = type_basic(false, TYPE_BASIC_INT);
  }

  node->data.number.result.ir_operand = NULL;
  node->nr.val = strtoul(text, NULL, 10);

  node->print_function = &node_print_number;
  node->generate_symbols = &add_no_symbols;
  node->check_types = &check_types_number;
  node->ir_generate = &ir_generate_number;

  return node;
}

/*
 * node_identifier - allocate a node to represent an identifier
 */
struct node *node_identifier(YYLTYPE location, char *text, int length)
{
  struct node *node = node_create(NODE_IDENTIFIER, location);
  memset(node->data.identifier.name, 0, IDENTIFIER_MAX + 1);
  strncpy(node->data.identifier.name, text, length);
  node->data.identifier.symbol_table = NULL;
  node->data.identifier.symbol = NULL;

  node->print_function = &node_print_identifier;
  node->generate_symbols = &add_symbols_from_identifier;
  node->check_types = &check_types_identifier;
  node->ir_generate = &ir_generate_identifier;

  node->nr.val = 0;
  return node;
}

/*
 * node_string - allocate a node to represent an string literal
 */
struct node *node_string(YYLTYPE location, char *text, int length)
{
  struct node *node = node_create(NODE_STRING, location);
  memset(node->data.string.val, 0, STRING_MAX + 1);
  strncpy(node->data.string.val, text + 1, length-2);
  node->data.string.string_literal_ptr = string_table_add(&string_literal_table, text);

  node->print_function = &node_print_string;
  node->generate_symbols = &add_no_symbols;
  node->check_types = &check_types_string;
  node->ir_generate = &ir_generate_string;
  return node;
}


/*
 * node_decl - allocate a node to represent a decl
 */
struct node *node_decl(YYLTYPE location, struct node *type_specifiers,
                       struct node *initialized_declarator_list)
{
  struct node *node = node_create(NODE_DECL, location);
  node->data.decl.initialized_declarator_list = initialized_declarator_list;
  node->data.decl.type_specifiers = type_specifiers;

  node->print_function = &node_print_decl;
  node->generate_symbols = &add_symbols_from_decl;
  node->check_types = &check_types_decl;
  node->ir_generate = &ir_generate_decl;
  return node;
}

/*
 * node_fn_def - allocate a node to represent a function definition
 */
struct node *node_fn_def(YYLTYPE location, struct node *fn_def_specifier,
                         struct node *compound_statement)
{
  struct node *node = node_create(NODE_FN_DEF, location);
  node->data.function_definition.fn_def_specifier = fn_def_specifier;
  node->data.function_definition.compound_statement = compound_statement;

  node->print_function = &node_print_fn_def;
  node->generate_symbols = &add_symbols_from_fn_def;
  node->check_types = &check_types_fn_def;
  node->ir_generate = &ir_generate_fn_def;
  return node;
}

/*
 * node_parameter_decl - allocate a node to represent a parameter decl
 */
struct node *node_parameter_decl(YYLTYPE location, struct node *type_specifiers,
                                 struct node *declarator_opt_abstract)
{
  struct node *node = node_create(NODE_PARAMETER_DECL, location);
  node->data.parameter_decl.type_specifiers = type_specifiers;
  node->data.parameter_decl.declarator_opt_abstract = declarator_opt_abstract;

  node->print_function = &node_print_parameter_decl;
  node->generate_symbols = &add_symbols_from_parameter_decl;
  return node;
}

/*
 * node_declarator - allocate a node to represent a single
 * initialized declarator
 */
struct node *node_declarator(YYLTYPE location,
                                 struct node *declarator,
                                 struct node *add_spec,
                                 enum declarator_type d)
{
  struct node *node = node_create(NODE_DECLARATOR, location);
  node->data.declarator.declarator = declarator;
  node->data.declarator.declarator_type = d;
  node->data.declarator.add_spec = add_spec;

  node->print_function = &node_print_declarator;
  node->generate_symbols = &add_symbols_from_declarator;
  return node;
}

/*
 * node_abstract_declarator - allocate a node to represent a single
 * abstract declarator
 */
struct node *node_abstract_declarator(YYLTYPE location,
                                 struct node *pointer,
                                 struct node *dir_abs_declarator)
{
  struct node *node = node_create(NODE_ABSTRACT_DECLARATOR, location);
  node->data.abstract_declarator.pointer = pointer;
  node->data.abstract_declarator.dir_abs_declarator = dir_abs_declarator;

  node->print_function = &node_print_abstract_declarator;
  node->generate_symbols = &add_symbols_from_abstract_declarator;
  return node;
}


/*
 * node_type_specifier - allocate a node to represent a type specifier
 */
struct node *node_type_specifier(YYLTYPE location, enum type_specifier typeid)
{
  struct node *node = node_create(NODE_TYPE_SPECIFIER, location);
  node->data.type_specifier.typeid = typeid;

  node->print_function = &node_print_type_specifier;
  node->generate_symbols = &add_symbols_from_type_specifier;
  return node;
}

/*
 * node_function_declarator - allocate a node to represent a function
 * declarator
 */
struct node *node_function_declarator(YYLTYPE location,
                                 struct node *direct_declarator,
                                 struct node *parameter_type_list)
{
  struct node *node = node_create(NODE_FUNCTION_DECLARATOR, location);
  node->data.function_declarator.direct_declarator = direct_declarator;
  node->data.function_declarator.parameter_type_list = parameter_type_list;

  node->print_function = &node_print_function_declarator;
  node->generate_symbols = &add_symbols_from_function_declarator;
  return node;
}

/*
 * node_statement - allocate a node to represent a statement
 */
struct node *node_statement(YYLTYPE location, enum statement_type statement_type,
                            struct node *expr, struct node *stmt, struct node *stmt2)
{
  struct node *node = node_create(NODE_STATEMENT, location);
  node->data.statement.statement_type = statement_type;
  node->data.statement.expr = expr;
  node->data.statement.stmt = stmt;
  node->data.statement.stmt2 = stmt2;

  node->print_function = &node_print_statement;
  node->generate_symbols = &add_symbols_from_statement;
  node->check_types = &check_types_statement;
  node->ir_generate = &ir_generate_statement;
  return node;
}

/*
 * node_for_expr - allocate a node to represent a for_expr
 */
struct node *node_for_expr(YYLTYPE location, struct node *init,
                            struct node *condition, struct node *update)
{
  struct node *node = node_create(NODE_FOR_EXPR, location);
  node->data.for_expr.init = init;
  node->data.for_expr.condition = condition;
  node->data.for_expr.update = update;

  node->print_function = &node_print_for_expr;
  node->generate_symbols = &add_symbols_from_for_expr;
  node->check_types = &check_types_for_expr;
  node->ir_generate = &ir_generate_for_expr;

  return node;
}

/***************************
 *  OPERATOR CONSTRUCTORS  *
 ***************************/

/*
 * node_binary_operation - allocate a node to represent a binary operation
 */
struct node *node_binary_operation(YYLTYPE location,
                                   enum node_binary_operation operation,
                                   struct node *left_operand,
                                   struct node *right_operand)
{
  struct node *node = node_create(NODE_BINARY_OPERATION, location);
  node->data.binary_operation.operation = operation;
  node->data.binary_operation.left_operand = left_operand;
  node->data.binary_operation.right_operand = right_operand;
  node->data.binary_operation.op_type_spec[0] = '\0';
  node->data.binary_operation.res_type_spec[0] = '\0';

  node->print_function = &node_print_binary_operation;
  node->generate_symbols = &add_symbols_from_binary_operation;
  node->check_types = &check_types_binary_operation;
  node->ir_generate = &ir_generate_binary_operation;

  // compute result of non-assignment binary operation
  // (needed for placing arrays in symbol table)
  switch (operation) {
  case BINOP_MULTIPLICATION:
    node->nr.val = left_operand->nr.val * right_operand->nr.val;
    break;
  case BINOP_DIVISION:
    if (right_operand->nr.val) {
      node->nr.val = left_operand->nr.val / right_operand->nr.val;
    }
    break;
  case BINOP_ADDITION:
    node->nr.val = left_operand->nr.val + right_operand->nr.val;
    break;
  case BINOP_SUBTRACTION:
    node->nr.val = left_operand->nr.val - right_operand->nr.val;
    break;
  case BINOP_MODDIV:
    if (right_operand->nr.val) {
      node->nr.val = left_operand->nr.val % right_operand->nr.val;
    }
    break;
  case BINOP_LOGICAL_OR:
    node->nr.val = left_operand->nr.val || right_operand->nr.val;
    break;
  case BINOP_LOGICAL_AND:
    node->nr.val = left_operand->nr.val && right_operand->nr.val;
    break;
  case BINOP_BITWISE_OR:
    node->nr.val = left_operand->nr.val | right_operand->nr.val;
    break;
  case BINOP_BITWISE_AND:
    node->nr.val = left_operand->nr.val & right_operand->nr.val;
    break;
  case BINOP_BITWISE_XOR:
    node->nr.val = left_operand->nr.val ^ right_operand->nr.val;
    break;
  case BINOP_EQUALITY:
    node->nr.val = left_operand->nr.val == right_operand->nr.val;
    break;
  case BINOP_INEQUALITY:
    node->nr.val = left_operand->nr.val != right_operand->nr.val;
    break;
  case BINOP_LESS:
    node->nr.val = left_operand->nr.val < right_operand->nr.val;
    break;
  case BINOP_LESS_EQUAL:
    node->nr.val = left_operand->nr.val <= right_operand->nr.val;
    break;
  case BINOP_GREATER:
    node->nr.val = left_operand->nr.val > right_operand->nr.val;
    break;
  case BINOP_GREATER_EQUAL:
    node->nr.val = left_operand->nr.val >= right_operand->nr.val;
    break;
  case BINOP_LEFT_SHIFT:
    node->nr.val = left_operand->nr.val << right_operand->nr.val;
    break;
  case BINOP_RIGHT_SHIFT:
    node->nr.val = left_operand->nr.val >> right_operand->nr.val;
    break;
  case BINOP_COMMA:
    node->nr.val = right_operand->nr.val;
    break;
  default:
    break;
  }

  return node;
}

/*
 * node_ternary_operation - allocate a node to represent a statement
 * using the '?' operator.
 */
struct node *node_ternary_operation(YYLTYPE location,
                                   struct node *first_op,
                                   struct node *second_op,
                                   struct node *third_op)
{
  struct node *node = node_create(NODE_TERNARY_OPERATION, location);
  node->data.ternary_operation.first_op = first_op;
  node->data.ternary_operation.second_op = second_op;
  node->data.ternary_operation.third_op = third_op;
  if (first_op->nr.val) {
    node->nr.val = second_op->nr.val;
  } else {
    node->nr.val = third_op->nr.val;
  }

  node->print_function = &node_print_ternary_operation;
  node->generate_symbols = &add_symbols_from_ternary_operation;
  node->check_types = &check_types_ternary_operation;
  node->ir_generate = &ir_generate_ternary_operation;
  return node;
}

/*
 * node_cast_operation - allocate a node to represent a cast expression
 */
struct node *node_cast_operation(YYLTYPE location,
                                   struct node *type_name,
                                   struct node *operand)
{
  struct node *node = node_create(NODE_CAST_OPERATION, location);
  node->data.cast_operation.type_name = type_name;
  node->data.cast_operation.operand = operand;

  node->print_function = &node_print_cast_operation;
  node->generate_symbols = &add_symbols_from_cast_operation;
  node->check_types = &check_types_cast_operation;
  node->ir_generate = &ir_generate_cast_operation;
  return node;
}

/*
 * node_unary_prefix_operation - allocate a node to represent a unary prefix op
 */
struct node *node_unary_prefix_operation(YYLTYPE location,
                                   enum node_unary_prefix_operation operation,
                                   struct node *operand)
{
  struct node *node = node_create(NODE_UNARY_PREFIX_OPERATION, location);
  node->data.unary_prefix_operation.operation = operation;
  node->data.unary_prefix_operation.op_type_spec[0] = '\0';
  node->data.unary_prefix_operation.res_type_spec[0] = '\0';
  node->data.unary_prefix_operation.operand = operand;

  node->print_function = &node_print_unary_prefix_operation;
  node->generate_symbols = &add_symbols_from_unary_prefix_operation;
  node->check_types = &check_types_unary_prefix_operation;
  node->ir_generate = &ir_generate_unary_prefix_op;

  switch(operation) {
  case UNARY_MINUS:
    node->nr.val = 0 - operand->nr.val;
    break;
  case UNARY_PLUS:
    node->nr.val = operand->nr.val;
    break;
  case UNARY_LOGICAL_NEGATION:
    node->nr.val = !(operand->nr.val);
    break;
  case UNARY_BITWISE_NEGATION:
    node->nr.val = ~(operand->nr.val);
    break;
  default:
    break;
  }

  return node;
}

/*
 * node_unary_postfix_op - allocate a node to represent a unary postfix op
 */
struct node *node_postfix_expr(YYLTYPE location,
                              enum node_postfix_expr postfix_expr_type,
                              struct node *operand,
                              struct node *expression_list)
{
  struct node *node = node_create(NODE_POSTFIX_EXPR, location);
  node->data.postfix_expr.postfix_expr_type = postfix_expr_type;
  node->data.postfix_expr.op_type_spec[0] = '\0';
  node->data.postfix_expr.res_type_spec[0] = '\0';
  node->data.postfix_expr.operand = operand;
  node->data.postfix_expr.expression_list = expression_list;

  node->print_function = &node_print_postfix_expr;
  node->generate_symbols = &add_symbols_from_postfix_expr;
  node->check_types = &check_types_postfix_expression;
  node->ir_generate = &ir_generate_postfix_expression;

  return node;
}

/*
 * node_conditional_expr - allocate a node to represent a conditional expression
 */
struct node *node_conditional_expr(YYLTYPE location, struct node *c_expr)
{
  struct node *node = node_create(NODE_CONDITIONAL_EXPR, location);
  node->data.conditional_expr.conditional_expr = c_expr;
  node->nr = c_expr->nr;

  node->print_function = &node_print_conditional_expr;
  node->generate_symbols = &add_symbols_from_conditional_expr;
  node->check_types = &check_types_conditional_expr;
  node->ir_generate = &ir_generate_conditional_expr;

  return node;
}

/*********************************
 *  LIST-TYPE NODE CONSTRUCTORS  *
 *********************************/

/*
 * node_translation_unit_list - allocate a node to represent a list of
 * translation units; this list represents the entire input program
 */
struct node *node_translation_unit_list(YYLTYPE location,
                                 struct node *init,
                                 struct node *translation_unit)
{
  struct node *node = node_create(NODE_TRANSLATION_UNIT_LIST, location);
  node->data.translation_unit_list.init = init;
  node->data.translation_unit_list.translation_unit = translation_unit;
  return node;
}

/*
 * node_initialized_declarator_list - allocate a node to represent a list of
 * initialized declarators
 */
struct node *node_initialized_declarator_list(YYLTYPE location,
                                 struct node *init,
                                 struct node *initialized_declarator)
{
  struct node *node = node_create(NODE_INITIALIZED_DECLARATOR_LIST, location);
  node->data.initialized_declarator_list.init = init;
  node->data.initialized_declarator_list.initialized_declarator = initialized_declarator;

  node->print_function = &node_print_initialized_declarator_list;
  node->generate_symbols = &add_symbols_from_initialized_declarator_list;
  return node;
}

/*
 * node_direct_abstract_declarator_list - allocate a node to represent a list of
 */
struct node *node_direct_abstract_declarator_list(YYLTYPE location,
                                 struct node *init,
                                 struct node *dir_abs_declarator)
{
  struct node *node = node_create(NODE_DIRECT_ABSTRACT_DECLARATOR_LIST, location);
  node->data.direct_abstract_declarator_list.init = init;
  node->data.direct_abstract_declarator_list.dir_abs_declarator = dir_abs_declarator;

  node->print_function = &node_print_direct_abstract_declarator_list;
  node->generate_symbols = &add_symbols_from_direct_abstract_declarator_list;
  return node;
}


/*
 * node_parameter_list - allocate a node to represent a list of
 * parameters in a function declaration
 */
struct node *node_parameter_list(YYLTYPE location,
                                 struct node *init,
                                 struct node *parameter_decl)
{
  struct node *node = node_create(NODE_PARAMETER_LIST, location);
  node->data.parameter_list.init = init;
  node->data.parameter_list.parameter_decl = parameter_decl;

  node->print_function = &node_print_parameter_list;
  node->generate_symbols = &add_symbols_from_parameter_list;
  return node;
}

/*
 * node_expression_list - allocate a node to represent a list of
 * expressions in an abstract declarator
 */
struct node *node_expression_list(YYLTYPE location,
                                 struct node *init,
                                 struct node *assignment_expr)
{
  struct node *node = node_create(NODE_EXPRESSION_LIST, location);
  node->data.expression_list.init = init;
  node->data.expression_list.assignment_expr = assignment_expr;

  node->print_function = &node_print_expression_list;
  node->generate_symbols = &add_symbols_from_expression_list;

  node->check_types = &check_types_expression_list;
  node->ir_generate = &ir_generate_expression_list;

  return node;
}


/*
 * node_decl_or_statement_list - allocate a node to represent a list of
 * declarations or statements
 */
struct node *node_decl_or_statement_list(YYLTYPE location,
                                 struct node *init,
                                 struct node *decl_or_statement)
{
  struct node *node = node_create(NODE_DECL_OR_STATEMENT_LIST, location);
  node->data.decl_or_statement_list.init = init;
  node->data.decl_or_statement_list.decl_or_statement = decl_or_statement;

  node->print_function = &node_print_decl_or_statement_list;
  node->generate_symbols = &add_symbols_from_decl_or_statement_list;
  node->check_types = &check_types_decl_or_statement_list;
  node->ir_generate = &ir_generate_decl_or_statement_list;
  return node;
}

/*
 * node_pointer_list - allocate a node to represent any number of sequential
 * indirection operators
 */
struct node *node_pointer_list(YYLTYPE location,
                                 struct node *current)
{
  if (current == NULL) {
    struct node *node = node_create(NODE_POINTER_LIST, location);
    node->data.pointer_list.ptr_count = 1;

    node->print_function = &node_print_pointer_list;
    node->generate_symbols = &add_symbols_from_pointer_list;
    return node;
  }

  ++(current->data.pointer_list.ptr_count);
  return current;
}




/**************************
 * PRINT PARSE TREE NODES *
 **************************/

static void node_print_number(FILE *output, struct node *number) {
  assert(NODE_NUMBER == number->kind);

  fprintf(output, "%lu", number->data.number.value);
}

static void node_print_identifier(FILE *output, struct node *identifier) {
  assert(NODE_IDENTIFIER == identifier->kind);

  if (annotate) {
    if (!(identifier->data.identifier.symbol_table) || !(identifier->data.identifier.symbol)) {
      // Identifier not in any symbol table, print name from AST
      fprintf(output, "%s/*NONE*/",
              identifier->data.identifier.name);
      return;
    }
    fprintf(output, "%s/*%d*/", identifier->data.identifier.symbol->name,
            identifier->data.identifier.symbol_table->id);
    return;
  }

  // If identifier is in a symbol table, print name from symbol table,
  // else (if undefined identifier is referenced) print name directly from AST
  if (identifier->data.identifier.symbol) {
    fprintf(output, "%s", identifier->data.identifier.symbol->name);
  } else {
    fprintf(output, "%s", identifier->data.identifier.name);
  }
}

static void node_print_string(FILE *output, struct node *string) {
  assert(NODE_STRING == string->kind);

  if (string->data.string.string_literal_ptr && annotate) {
    fprintf(output, "\"%s\" /* %s */", string->data.string.val,
            string->data.string.string_literal_ptr->id);
  } else {
    fprintf(output, "\"%s\"", string->data.string.val);
  }
}

static void node_print_decl(FILE *output, struct node *decl) {
  assert(NODE_DECL == decl-> kind);
  decl->data.decl.type_specifiers->print_function(output, decl->data.decl.type_specifiers);
  fprintf(output, " ");
  struct node *idl = decl->data.decl.initialized_declarator_list;
  idl->print_function(output, idl);
  fprintf(output, ";\n");
}

static void node_print_fn_def(FILE *output, struct node *fn_def) {
  assert(NODE_FN_DEF == fn_def->kind);

  struct node *specifier = fn_def->data.function_definition.fn_def_specifier;
  specifier->print_function(output, specifier);

  struct node *fd = fn_def->data.function_definition.compound_statement;
  fprintf(output, " {\n");
  if (fd) fd->print_function(output, fd);
  fprintf(output, "}\n");
}

void node_print_parameter_decl(FILE *output, struct node *parameter_decl) {
  assert(NODE_PARAMETER_DECL == parameter_decl-> kind);

  struct node *t_spec = parameter_decl->data.parameter_decl.type_specifiers;
  t_spec->print_function(output, t_spec);

  struct node *dcl_opt_abs = parameter_decl->data.parameter_decl.declarator_opt_abstract;
  if (dcl_opt_abs != NULL) {
      fprintf(output, " (");
      dcl_opt_abs->print_function(output, dcl_opt_abs);
      fprintf(output, ")");
  }
}

void node_print_declarator(FILE *output, struct node *declarator) {
  assert(NODE_DECLARATOR == declarator->kind);

  struct node *d = declarator->data.declarator.declarator;
  switch (declarator->data.declarator.declarator_type) {
  case SIMPLE_DECLARATOR:
    d->print_function(output, d);
    break;
  case ARRAY_DECLARATOR:
    d->print_function(output, d);
    fprintf(output, "[");
    if (declarator->data.declarator.add_spec != NULL) {
      declarator->data.declarator.add_spec->print_function(output, declarator->data.declarator.add_spec);
    }
    fprintf(output, "]");
    break;
  case POINTER_DECLARATOR:
    declarator->data.declarator.add_spec->print_function(output, declarator->data.declarator.add_spec);
    fprintf(output, "(");
    d->print_function(output, d);
    fprintf(output, ")");
  default:
    break;
  }
}

static void node_print_abstract_declarator(FILE *output, struct node *abstract_declarator) {
  assert(NODE_ABSTRACT_DECLARATOR == abstract_declarator->kind);

  struct node *p = abstract_declarator->data.abstract_declarator.pointer;
  struct node *dir_abs_declarator = abstract_declarator->data.abstract_declarator.dir_abs_declarator;
  if (p) p->print_function(output, p);
  fprintf(output, "(");
  if (dir_abs_declarator) dir_abs_declarator->print_function(output, dir_abs_declarator);
  fprintf(output, ")");
}

static void node_print_type_specifier(FILE *output, struct node *type_specifier) {
  assert(NODE_TYPE_SPECIFIER == type_specifier->kind);
  switch (type_specifier->data.type_specifier.typeid) {
  case SIGNED_CHAR:
    fprintf(output, "signed char");
    break;
  case UNSIGNED_CHAR:
    fprintf(output, "unsigned char");
    break;
  case SIGNED_SHORT_INT:
    fprintf(output, "signed short int");
    break;
  case UNSIGNED_SHORT_INT:
    fprintf(output, "unsigned short int");
    break;
  case SIGNED_INT:
    fprintf(output, "signed int");
    break;
  case UNSIGNED_INT:
    fprintf(output, "unsigned int");
    break;
  case SIGNED_LONG_INT:
    fprintf(output, "signed long int");
    break;
  case UNSIGNED_LONG_INT:
    fprintf(output, "unsigned long int");
    break;
  case VOID_T:
    fprintf(output, "void");
    break;
  }
}

static void node_print_function_declarator(FILE *output, struct node *function_declarator) {
  assert(NODE_FUNCTION_DECLARATOR == function_declarator->kind);
  
  struct node *dd = function_declarator->data.function_declarator.direct_declarator;
  dd->print_function(output, dd);

  fprintf(output, "(");
  struct node *ptl = function_declarator->data.function_declarator.parameter_type_list;
  ptl->print_function(output, ptl);
  fprintf(output, ")");
}


static void node_print_statement(FILE *output, struct node *statement) {
  assert(NODE_STATEMENT == statement->kind);

  int s_type = statement->data.statement.statement_type;
  struct node *expr = statement->data.statement.expr;
  struct node *stmt = statement->data.statement.stmt;
  struct node *stmt2 = statement->data.statement.stmt2;
  
  switch(s_type) {
  case STATEMENT_EXPRESSION:
    expr->print_function(output, expr);
    fprintf(output, ";\n");
    break;
  case STATEMENT_LABELED:
    expr->print_function(output, expr);
    fprintf(output, ":\n    ");
    stmt->print_function(output, stmt);
    break;
  case STATEMENT_COMPOUND:
    fprintf(output, "{\n");
    if (expr) expr->print_function(output, expr);
    fprintf(output, "}\n");
    break;
  case STATEMENT_CONDITIONAL:
    fprintf(output, "if (");
    if (expr) expr->print_function(output, expr);
    fprintf(output, ") ");
    if (stmt) stmt->print_function(output, stmt);
    if (stmt2) {
      fprintf(output, "else ");
      stmt2->print_function(output, stmt2);
    }
    break;
  case STATEMENT_ITERATIVE_WHILE:
    fprintf(output, "while (");
    if (expr) expr->print_function(output, expr);
    fprintf(output, ") ");
    if (stmt) stmt->print_function(output, stmt);
    break;
  case STATEMENT_ITERATIVE_DO:
    fprintf(output, "do ");
    if (stmt) stmt->print_function(output, stmt);
    fprintf(output, "while (");
    if (expr) expr->print_function(output, expr);
    fprintf(output, ");\n");
    break;
  case STATEMENT_ITERATIVE_FOR:
    fprintf(output, "for ");
    if (expr) expr->print_function(output, expr);
    if (stmt) stmt->print_function(output, stmt);
    break;
  case STATEMENT_BREAK:
    fprintf(output, "break;\n");
    break;
  case STATEMENT_CONTINUE:
    fprintf(output, "continue;\n");
    break;
  case STATEMENT_RETURN:
    fprintf(output, "return");
    if (expr) {
      fprintf(output, " ");
      expr->print_function(output, expr);
    }
    fprintf(output, ";\n");
    break;
  case STATEMENT_GOTO:
    fprintf(output, "goto ");
    expr->print_function(output, expr);
    fprintf(output, ";\n");
    break;
  case STATEMENT_NULL:
    fprintf(output, ";\n");
    break;
  }
}

static void node_print_for_expr(FILE *output, struct node *for_expr) {
  assert(NODE_FOR_EXPR == for_expr->kind);
  struct node *i = for_expr->data.for_expr.init;
  struct node *c = for_expr->data.for_expr.condition;
  struct node *u = for_expr->data.for_expr.update;

  fprintf(output, "(");
  if (i) i->print_function(output, i);
  fprintf(output, "; ");
  if (c) c->print_function(output, c);
  fprintf(output, "; ");
  if (u) u->print_function(output, u);
  fprintf(output, ") ");
}

/******************************
 *  OPERATOR PRINT FUNCTIONS  *
 ******************************/

static void node_print_binary_operation(FILE *output, struct node *binary_operation) {
  static const char *binary_operators[] = {
    "*",    /*  0 = BINOP_MULTIPLICATION */
    "/",    /*  1 = BINOP_DIVISION */
    "+",    /*  2 = BINOP_ADDITION */
    "-",    /*  3 = BINOP_SUBTRACTION */
    "%",    /*  4 = BINOP_MODDIV */
    "||",   /*  5 = BINOP_LOGICAL_OR */
    "&&",   /*  6 = BINOP_LOGICAL_AND */
    "|",    /*  7 = BINOP_BITWISE_OR */
    "&",    /*  8 = BINOP_BITWISE_AND */
    "^",    /*  9 = BINOP_BITWISE_XOR */
    "==",   /*  10 = BINOP_EQUALITY */
    "!=",   /*  11 = BINOP_INEQUALITY */
    "<",    /*  12 = BINOP_LESS */
    "<=",   /*  13 = BINOP_LESS_EQUAL */
    ">",    /*  14 = BINOP_GREATER */
    ">=",   /*  15 = BINOP_GREATER_EQUAL */
    "<<",   /*  16 = BINOP_LEFT_SHIFT */
    ">>",   /*  17 = BINOP_RIGHT_SHIFT */
    ",",    /*  18 = BINOP_COMMA */

    "=",    /*  19 = BINOP_ASSIGN */
    "+=",   /*  20 = BINOP_ASSIGN_ADDITION */
    "-=",   /*  21 = BINOP_ASSIGN_SUBTRACTION */
    "*=",   /*  22 = BINOP_ASSIGN_MULTIPLICATION */
    "/=",   /*  23 = BINOP_ASSIGN_DIVISION */
    "%=",   /*  24 = BINOP_ASSIGN_MODDIV */
    "<<=",  /*  25 = BINOP_ASSIGN_LSHIFT */
    ">>=",  /*  26 = BINOP_ASSIGN_RSHIFT */
    "&=",   /*  27 = BINOP_ASSIGN_BITWISE_AND */
    "^=",   /*  28 = BINOP_ASSIGN_BITWISE_XOR */
    "|=",   /*  29 = BINOP_ASSIGN_BITWISE_OR */

    NULL
  };

  assert(NODE_BINARY_OPERATION == binary_operation->kind);

  fputs("(", output);


  if (strlen(binary_operation->data.binary_operation.res_type_spec)) {
    //fprintf(output, "/*result:(%s)*/", binary_operation->data.binary_operation.res_type_spec);
  }
  if (strlen(binary_operation->data.binary_operation.op_type_spec)) {
    //fprintf(output, "/*operand:(%s)*/", binary_operation->data.binary_operation.op_type_spec);
  }
  struct node *l_op = binary_operation->data.binary_operation.left_operand;
  l_op->print_function(output, l_op);

  int operator = binary_operation->data.binary_operation.operation;
  fputs(binary_operators[operator], output);

  struct node *r_op = binary_operation->data.binary_operation.right_operand;
  r_op->print_function(output, r_op);
  fputs(")", output);
}

static void node_print_ternary_operation(FILE *output, struct node *ternary_operation) {
  assert(NODE_TERNARY_OPERATION == ternary_operation->kind);

  struct node *first_op = ternary_operation->data.ternary_operation.first_op;
  first_op->print_function(output, first_op);

  fprintf(output, " ? ");

  struct node *second_op = ternary_operation->data.ternary_operation.second_op;
  second_op->print_function(output, second_op);

  fprintf(output, " : ");

  struct node *third_op = ternary_operation->data.ternary_operation.third_op;
  third_op->print_function(output, third_op);
}

static void node_print_cast_operation(FILE *output, struct node *cast_operation) {
  assert(NODE_CAST_OPERATION == cast_operation->kind);

  struct node *type_name = cast_operation->data.cast_operation.type_name;
  fprintf(output, "(");
  type_name->print_function(output, type_name);
  fprintf(output, ") ");

  struct node *operand = cast_operation->data.cast_operation.operand;
  operand->print_function(output, operand);
}

static void node_print_conditional_expr(FILE *output, struct node *conditional_expr) {
  assert(NODE_CONDITIONAL_EXPR == conditional_expr->kind); 

  struct node *c_expr = conditional_expr->data.conditional_expr.conditional_expr;
  assert(c_expr != NULL);
  c_expr->print_function(output, c_expr);
}

static void node_print_unary_prefix_operation(FILE *output, struct node *unary_prefix_operation) {
  static const char *unary_prefix_operators[] = {
    "-",    /*  0 = UNARY_MINUS */
    "+",    /*  1 = UNARY_PLUS */
    "!",    /*  2 = UNARY_LOGICAL_NEGATION */
    "~",    /*  3 = UNARY_BITWISE_NEGATION */
    "&",    /*  4 = UNARY_ADDRESS_EXPR */
    "*",    /*  5 = UNARY_INDIRECTION_EXPR */
    "++",   /*  6 = UNARY_PREINCREMENT */
    "--",    /*  7 = UNARY_PREDECREMENT */
    NULL
  };

  assert(NODE_UNARY_PREFIX_OPERATION == unary_prefix_operation->kind);

  fputs("(", output);
  fputs(unary_prefix_operators[unary_prefix_operation->data.unary_prefix_operation.operation], output);
  struct node *op = unary_prefix_operation->data.unary_prefix_operation.operand;
  if (strlen(unary_prefix_operation->data.unary_prefix_operation.res_type_spec)) {
    fprintf(output, "/*result:(%s)*/", unary_prefix_operation->data.unary_prefix_operation.res_type_spec);
  }
  if (strlen(unary_prefix_operation->data.unary_prefix_operation.op_type_spec)) {
    fprintf(output, "/*operand:(%s)*/", unary_prefix_operation->data.unary_prefix_operation.op_type_spec);
  }
  op->print_function(output, op);
  fputs(")", output);
}

static void node_print_postfix_expr(FILE *output, struct node *postfix_expr) {
  assert(NODE_POSTFIX_EXPR == postfix_expr->kind);

  fputs("(", output);

  struct node *op = postfix_expr->data.postfix_expr.operand;
  struct node *expr_list = postfix_expr->data.postfix_expr.expression_list;
  
  switch (postfix_expr->data.postfix_expr.postfix_expr_type) {
  case POSTFIX_FN_CALL:
    op->print_function(output, op);
    fprintf(output, "(");
    if (expr_list) expr_list->print_function(output, expr_list);
    fprintf(output, ")");
    break;
  case POSTINCREMENT:
    if (0 && strlen(postfix_expr->data.postfix_expr.res_type_spec)) {
      fprintf(output, "/*result:(%s)*/", postfix_expr->data.postfix_expr.res_type_spec);
    }
    if (0 && strlen(postfix_expr->data.postfix_expr.op_type_spec)) {
      fprintf(output, "/*operand:(%s)*/", postfix_expr->data.postfix_expr.op_type_spec);
    }

    op->print_function(output, op);
    fprintf(output, "++");
    break;
  case POSTDECREMENT:
    if (0 && strlen(postfix_expr->data.postfix_expr.res_type_spec)) {
      fprintf(output, "/*result:(%s)*/", postfix_expr->data.postfix_expr.res_type_spec);
    }
    if (0 && strlen(postfix_expr->data.postfix_expr.op_type_spec)) {
      fprintf(output, "/*operand:(%s)*/", postfix_expr->data.postfix_expr.op_type_spec);
    }

    op->print_function(output, op);
    fprintf(output, "--");
    break;
  }

  fputs(")", output);
}


/************************************
 *  LIST-TYPE NODE PRINT FUNCTIONS  *
 ************************************/

void node_print_translation_unit_list(FILE *output, struct node *translation_unit_list) {
  assert(NODE_TRANSLATION_UNIT_LIST == translation_unit_list->kind);

  if (NULL != translation_unit_list->data.translation_unit_list.init) {
    node_print_translation_unit_list(output, translation_unit_list->data.translation_unit_list.init);
  }
  if (NULL != translation_unit_list->data.translation_unit_list.translation_unit) {
    node_print_top_level_decl(output, translation_unit_list->data.translation_unit_list.translation_unit);
  }
}

static void node_print_initialized_declarator_list(FILE *output, struct node *initialized_declarator_list) {
  assert(NODE_INITIALIZED_DECLARATOR_LIST == initialized_declarator_list->kind);

  if (NULL != initialized_declarator_list->data.initialized_declarator_list.init) {
    struct node *i = initialized_declarator_list->data.initialized_declarator_list.init;
    i->print_function(output, i);
    fprintf(output, ", ");
  }

  fprintf(output, "(");
  struct node *i = initialized_declarator_list->data.initialized_declarator_list.initialized_declarator;
  i->print_function(output, i);
  fprintf(output, ")");
}

static void node_print_direct_abstract_declarator_list(FILE *output, struct node *dir_abs_declarator) {
  assert(NODE_DIRECT_ABSTRACT_DECLARATOR_LIST == dir_abs_declarator->kind);

  if (NULL != dir_abs_declarator->data.direct_abstract_declarator_list.init) {
    struct node *i = dir_abs_declarator->data.direct_abstract_declarator_list.init;
    i->print_function(output, i);
  }

  fprintf(output, "[");
  struct node *i = dir_abs_declarator->data.direct_abstract_declarator_list.dir_abs_declarator;
  if (i) i->print_function(output, i);
  fprintf(output, "]");
}

static void node_print_parameter_list(FILE *output, struct node *parameter_list) {
  assert(NODE_PARAMETER_LIST == parameter_list->kind);

  if (NULL != parameter_list->data.parameter_list.init) {
    struct node *p = parameter_list->data.parameter_list.init;
    p->print_function(output, p);
    fprintf(output, ", ");
  }

  struct node *p = parameter_list->data.parameter_list.parameter_decl;
  p->print_function(output, p);
}

static void node_print_expression_list(FILE *output, struct node *expression_list) {
  assert(NODE_EXPRESSION_LIST == expression_list->kind);

  if (NULL != expression_list->data.expression_list.init) {
    struct node *p = expression_list->data.expression_list.init;
    p->print_function(output, p);
    fprintf(output, ", ");
  }

  struct node *p = expression_list->data.expression_list.assignment_expr;
  p->print_function(output, p);
}

static void node_print_decl_or_statement_list(FILE *output, struct node *decl_or_statement_list) {
  assert(NODE_DECL_OR_STATEMENT_LIST == decl_or_statement_list->kind);

  if (NULL != decl_or_statement_list->data.decl_or_statement_list.init) {
    struct node *ds = decl_or_statement_list->data.decl_or_statement_list.init;
    ds->print_function(output, ds);
  }

  struct node *ds = decl_or_statement_list->data.decl_or_statement_list.decl_or_statement;
  if (ds) {
    ds->print_function(output, ds);
  }
}

static void node_print_pointer_list(FILE *output, struct node *ptr_list) {
  assert(NODE_POINTER_LIST == ptr_list->kind);
  for (int i = 0; i < ptr_list->data.pointer_list.ptr_count; ++i) {
    fprintf(output, "*");
  }
}

static void node_print_top_level_decl(FILE *output, struct node *top_level_decl) {
  assert(NODE_DECL == top_level_decl->kind || NODE_FN_DEF == top_level_decl->kind);
  top_level_decl->print_function(output, top_level_decl);
}



// NOT USED: retained for compatibility with other components of the sample compiler.
struct result *node_get_result(struct node *expression) {
  switch (expression->kind) {
    case NODE_NUMBER:
      return &expression->data.number.result;
    case NODE_IDENTIFIER:
      return &expression->data.identifier.symbol->result;
    default:
      assert(0);
      return NULL;
  }
}
