#ifndef _NODE_H
#define _NODE_H

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

#include <stdio.h>
#include <stdbool.h>

#include "compiler.h"
#include "symbol.h"
#include "parser.tab.h"
#include "type.h"
#include "type_check.h"
#include "ir.h"

enum type_specifier {
  SIGNED_CHAR,
  UNSIGNED_CHAR,
  SIGNED_SHORT_INT,
  UNSIGNED_SHORT_INT,
  SIGNED_INT,
  UNSIGNED_INT,
  SIGNED_LONG_INT,
  UNSIGNED_LONG_INT,
  VOID_T
};

enum declarator_type {
  FUNCTION_DECLARATOR,
  SIMPLE_DECLARATOR,
  PAREN_DECLARATOR,
  ARRAY_DECLARATOR,
  POINTER_DECLARATOR
};

enum statement_type {
  STATEMENT_EXPRESSION,
  STATEMENT_LABELED,
  STATEMENT_COMPOUND,
  STATEMENT_CONDITIONAL,
  STATEMENT_ITERATIVE_WHILE,
  STATEMENT_ITERATIVE_DO,
  STATEMENT_ITERATIVE_FOR,
  STATEMENT_BREAK,
  STATEMENT_CONTINUE,
  STATEMENT_RETURN,
  STATEMENT_GOTO,
  STATEMENT_NULL
};

enum node_binary_operation {
  /* mathematical and logical binary operators */
  BINOP_MULTIPLICATION,
  BINOP_DIVISION,
  BINOP_ADDITION,
  BINOP_SUBTRACTION,
  BINOP_MODDIV,
  BINOP_LOGICAL_OR,
  BINOP_LOGICAL_AND,
  BINOP_BITWISE_OR,
  BINOP_BITWISE_AND,
  BINOP_BITWISE_XOR,
  BINOP_EQUALITY,
  BINOP_INEQUALITY,
  BINOP_LESS,
  BINOP_LESS_EQUAL,
  BINOP_GREATER,
  BINOP_GREATER_EQUAL,
  BINOP_LEFT_SHIFT,
  BINOP_RIGHT_SHIFT,
  BINOP_COMMA,

  /* assignment and compount assignment operators */
  BINOP_ASSIGN,
  BINOP_ASSIGN_ADDITION,
  BINOP_ASSIGN_SUBTRACTION,
  BINOP_ASSIGN_MULTIPLICATION,
  BINOP_ASSIGN_DIVISION,
  BINOP_ASSIGN_MODDIV,
  BINOP_ASSIGN_LSHIFT,
  BINOP_ASSIGN_RSHIFT,
  BINOP_ASSIGN_BITWISE_AND,
  BINOP_ASSIGN_BITWISE_XOR,
  BINOP_ASSIGN_BITWISE_OR
};

enum node_unary_prefix_operation {
  UNARY_MINUS,
  UNARY_PLUS,
  UNARY_LOGICAL_NEGATION,
  UNARY_BITWISE_NEGATION,
  UNARY_ADDRESS_EXPR,
  UNARY_INDIRECTION_EXPR,
  UNARY_PREINCREMENT,
  UNARY_PREDECREMENT
};

enum node_postfix_expr {
  POSTFIX_FN_CALL,
  POSTINCREMENT,
  POSTDECREMENT
};

struct numeric_result {
  unsigned int val;
};

enum node_kind {
  NODE_NUMBER,
  NODE_IDENTIFIER,
  NODE_STRING,

  NODE_DECL,
  NODE_FN_DEF,
  NODE_PARAMETER_DECL,
  NODE_DECLARATOR,
  NODE_ABSTRACT_DECLARATOR,
  NODE_TYPE_SPECIFIER,
  NODE_FUNCTION_DECLARATOR,
  NODE_STATEMENT,
  NODE_FOR_EXPR,

  //operators
  NODE_BINARY_OPERATION,
  NODE_TERNARY_OPERATION,
  NODE_CAST_OPERATION,
  NODE_UNARY_PREFIX_OPERATION,
  NODE_POSTFIX_EXPR,
  NODE_CONDITIONAL_EXPR,

  //list-type nodes
  NODE_TRANSLATION_UNIT_LIST,
  NODE_INITIALIZED_DECLARATOR_LIST,
  NODE_DIRECT_ABSTRACT_DECLARATOR_LIST,
  NODE_PARAMETER_LIST,
  NODE_EXPRESSION_LIST,
  NODE_DECL_OR_STATEMENT_LIST,
  NODE_POINTER_LIST,

  //NOT USED: retained for compatibility with other components
  //  of the sample compiler. To be deleted.
  NODE_NULL_STATEMENT,
  NODE_STATEMENT_LIST,
  NODE_EXPRESSION_STATEMENT
};
struct node {
  enum node_kind kind;
  struct location location;
  struct numeric_result nr;

  // Used for type checking
  bool lvalue_possible;
  bool modifiable;
  struct type_tree *type_tree;

  // Used for IR generation
  struct ir_operand *ir_operand;
  struct ir_section *ir;
  bool is_rvalue;

  void (*print_function)(FILE*, struct node *);
  void (*generate_symbols)(struct symbol_table *, struct type_tree **, struct node *);
  void (*check_types)(struct node *);
  void (*ir_generate)(struct node *);

  union {
    struct {
      unsigned long value;
      bool overflow;
      struct result result;
    } number;
    struct {
      char name[IDENTIFIER_MAX + 1];
      struct symbol_table *symbol_table;
      struct symbol *symbol;
    } identifier;
    struct {
      char val[STRING_MAX + 1];
      struct string_literal *string_literal_ptr;
    } string;

    struct {
      struct node *type_specifiers;
      struct node *initialized_declarator_list;
    } decl;
    struct {
      struct node *fn_def_specifier;
      struct node *compound_statement;
    } function_definition;
    struct {
      struct node *type_specifiers;
      struct node *declarator_opt_abstract;
    } parameter_decl;
    struct {
      struct node *declarator;
      struct node *add_spec;
      enum declarator_type declarator_type;
    } declarator;
    struct {
      struct node *pointer;
      struct node *dir_abs_declarator;
    } abstract_declarator;
    struct {
      enum type_specifier typeid;
    } type_specifier;
    struct {
      struct node *direct_declarator;
      struct node *parameter_type_list;
    } function_declarator;
    struct {
      enum statement_type statement_type;
      struct node *expr;
      struct node *stmt;
      struct node *stmt2;
    } statement;
    struct {
      struct node *init;
      struct node *condition;
      struct node *update;
    } for_expr;

    //operators
    struct {
      int operation;
      struct node *left_operand;
      struct node *right_operand;
      struct result result;
      char op_type_spec[TYPE_MAX + 1];
      char res_type_spec[TYPE_MAX + 1];
    } binary_operation;
    struct {
      struct node *first_op;
      struct node *second_op;
      struct node *third_op;
      struct result result;
    } ternary_operation;
    struct {
      struct node *type_name;
      struct node *operand;
    } cast_operation;
    struct {
      int operation;
      char op_type_spec[TYPE_MAX + 1];
      char res_type_spec[TYPE_MAX + 1];
      struct node *operand;
    } unary_prefix_operation;
    struct {
      int postfix_expr_type;
      char op_type_spec[TYPE_MAX + 1];
      char res_type_spec[TYPE_MAX + 1];
      struct node *operand;
      struct node *expression_list; //for postfix function call
    } postfix_expr;
    struct {
      struct node *conditional_expr;
    } conditional_expr;

    //list type nodes
    struct {
      struct node *init;
      struct node *translation_unit;
    } translation_unit_list;
    struct {
      struct node *init;
      struct node *initialized_declarator;
    } initialized_declarator_list;
    struct {
      struct node *init;
      struct node *dir_abs_declarator;
    } direct_abstract_declarator_list;
    struct {
      struct node *init;
      struct node *parameter_decl;
    } parameter_list;
    struct {
      struct node *init;
      struct node *assignment_expr;
    } expression_list;
    struct {
      struct node *init;
      struct node *decl_or_statement;
    } decl_or_statement_list;
    struct {
      int ptr_count;
    } pointer_list;


    //NOT USED: retained for compatibility with other parts of
    //  the sample compiler
    struct {
      struct node *expression;
    } expression_statement;
    struct {
      struct node *init;
      struct node *statement;
    } statement_list;
  } data;
};


/* Constructors */
struct node *node_number(YYLTYPE location, char *text);
struct node *node_identifier(YYLTYPE location, char *text, int length);
struct node *node_string(YYLTYPE location, char *text, int length);

struct node *node_decl(YYLTYPE location, struct node *type_specifiers, struct node *initialized_declarator_list);
struct node *node_fn_def(YYLTYPE location, struct node *fn_def_specifier, struct node *decl_or_statement_list);
struct node *node_parameter_decl(YYLTYPE location, struct node *type_specifiers, struct node *declarator_opt_abstract);
struct node *node_declarator(YYLTYPE location, struct node *declarator, struct node *add_spec, enum declarator_type d);
struct node *node_abstract_declarator(YYLTYPE location, struct node *pointer, struct node *dir_abs_declarator);
struct node *node_type_specifier(YYLTYPE location, enum type_specifier);
struct node *node_function_declarator(YYLTYPE location, struct node *direct_declarator, struct node *parameter_type_list);
struct node *node_statement(YYLTYPE location, enum statement_type statement_type,
                            struct node *expr, struct node *stmt, struct node *stmt2);
struct node *node_for_expr(YYLTYPE location, struct node *init,
                            struct node *condition, struct node *udpate);

//operators
struct node *node_binary_operation(YYLTYPE location, enum node_binary_operation operation,
                                   struct node *left_operand, struct node *right_operand);
struct node *node_ternary_operation(YYLTYPE location, struct node *first_op,
                                   struct node *second_op, struct node *third_op);
struct node *node_cast_operation(YYLTYPE location, struct node *type_name,
                                   struct node *operand);
struct node *node_unary_prefix_operation(YYLTYPE location,
                                   enum node_unary_prefix_operation operation,
                                   struct node *operand);
struct node *node_postfix_expr(YYLTYPE location, enum node_postfix_expr postfix_expr_type,
                              struct node *operand, struct node *expression_list);

struct node *node_conditional_expr(YYLTYPE location, struct node *c_expr);

//list-type nodes
struct node *node_translation_unit_list(YYLTYPE location, struct node *init, struct node *statement);
struct node *node_initialized_declarator_list(YYLTYPE location, struct node *init, struct node *initialized_declarator);
struct node *node_direct_abstract_declarator_list(YYLTYPE location, struct node *init, struct node *dir_abs_declarator);
struct node *node_parameter_list(YYLTYPE location, struct node *init, struct node *parameter_decl);
struct node *node_expression_list(YYLTYPE location, struct node *init, struct node *assignment_expr);
struct node *node_decl_or_statement_list(YYLTYPE location, struct node *init, struct node *decl_or_statement);
struct node *node_pointer_list(YYLTYPE location, struct node *current);


/* Print Functions */
static void node_print_number(FILE *output, struct node *number);
static void node_print_identifier(FILE *output, struct node *identifier);
static void node_print_string(FILE *output, struct node *string);

static void node_print_decl(FILE *output, struct node *decl);
static void node_print_fn_def(FILE *output, struct node *fn_def);
void node_print_parameter_decl(FILE *output, struct node *parameter_decl);
void node_print_declarator(FILE *output, struct node *declarator);
static void node_print_abstract_declarator(FILE *output, struct node *abstract_declarator);
static void node_print_type_specifier(FILE *output, struct node *type_specifier);
static void node_print_function_declarator(FILE *output, struct node *function_declarator);
static void node_print_statement(FILE *output, struct node *statement);
static void node_print_for_expr(FILE *output, struct node *for_expr);

//operators
static void node_print_binary_operation(FILE *output, struct node *binary_operation);
static void node_print_ternary_operation(FILE *output, struct node *ternary_operation);
static void node_print_cast_operation(FILE *output, struct node *cast_operation);
static void node_print_conditional_expr(FILE *output, struct node *conditional_expr);
static void node_print_unary_prefix_operation(FILE *output, struct node *unary_prefix_operation);
static void node_print_postfix_expr(FILE *output, struct node *postfix_expr);

//list-type nodes
void node_print_translation_unit_list(FILE *output, struct node *translation_unit_list);
static void node_print_initialized_declarator_list(FILE *output, struct node *initialized_declarator_list);
static void node_print_direct_abstract_declarator_list(FILE *output, struct node *dir_abs_declarator);
static void node_print_parameter_list(FILE *output, struct node *parameter_list);
static void node_print_expression_list(FILE *output, struct node *expression_list);
static void node_print_decl_or_statement_list(FILE *output, struct node *decl_or_statement_list);
static void node_print_pointer_list(FILE *output, struct node *ptr_list);

static void node_print_top_level_decl(FILE *output, struct node *top_level_decl);


// NOT USED: retained for compatibility with other components of the sample compiler.
struct node *node_null_statement(YYLTYPE location);
struct result *node_get_result(struct node *expression);
static void node_print_expression_statement(FILE *output, struct node *expression_statement);


#endif
