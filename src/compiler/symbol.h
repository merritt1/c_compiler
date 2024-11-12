#ifndef _SYMBOL_H
#define _SYMBOL_H

#include <stdio.h>
#include <stdlib.h>

#include "compiler.h"
#include "type.h"

struct node;
struct type;


/*****************
 * SYMBOL TABLES *
 *****************/

struct symbol {
  char type[TYPE_MAX + 1];
  char name[IDENTIFIER_MAX + 1];
  struct type_tree *type_tree;
  int stack_offset;         // For storing position of variable in stack frame
  int fn_symbol_table_id;   // For functions
  struct result result;     // Not used at this time. Retained for
                            // compability with sample compiler.
};

struct symbol_list {
  struct symbol symbol;
  bool isDefined;           // false when function is declared but not defined,
                            // true otherwise.
  struct symbol_list *next;
};

struct symbol_table {
  bool isDeclaration; // Used to determine whether identifier should be annotated.
  unsigned int id;

  struct symbol_table *labels; // Only used at function scope

  struct symbol_list *head; // List of symbols in the table

  struct symbol_table *parent;
  struct symbol_table *child;
  struct symbol_table *sibling;
};

/*************************
 * STRING LITERAL TABLES *
 *************************/

struct string_literal {
  char val[STRING_MAX + 1];
  char id[17]; // Format _StringLabel_x, where 0 < x < 999;
};

struct string_literal_list {
  struct string_literal str;
  struct string_literal_list *next;
};

struct string_literal_table {
  struct string_literal_list *head;
};

// Functions that manipulate and search within symbol tables
struct symbol_table *create_table(struct symbol_table *);
struct symbol_list *create_symbol(char *, char *, struct type_tree *, bool isDefined);
struct symbol_list *add_symbol(struct symbol_table *, struct symbol_list *, bool redeclarationPermitted);
void add_child(struct symbol_table *, struct symbol_table *);
struct symbol_table *youngest_child(struct symbol_table *);
struct symbol_table *find_identifier(struct symbol_table *, char *);
struct symbol_table *get_labels_table(struct symbol_table *);
struct symbol_list *find_symbol(struct symbol_table *, char *);
void validate_label_references(struct string_literal_list *, struct symbol_table *);

// Function to manipulate string literal table
struct string_literal *string_table_add(struct string_literal_table *, char *);

// Generate symbols for declarations
void add_symbols_from_translation_unit_list(struct node *, struct symbol_table *);
void add_symbols_from_top_level_decl(struct node *, struct symbol_table *);
void add_symbols_from_type_specifier(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_decl(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_initialized_declarator_list(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_declarator(struct symbol_table *, struct type_tree **, struct node *);
void add_array_to_type_tree(unsigned int, struct type_tree **);
void add_symbols_from_pointer_list(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_function_declarator(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_parameter_list(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_parameter_decl(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_abstract_declarator(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_direct_abstract_declarator_list(struct symbol_table *, struct type_tree **, struct node *);

// Generate symbols for function definitions and expressions
void add_symbols_from_fn_def(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_decl_or_statement_list(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_statement(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_conditional_expr(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_identifier(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_postfix_expr(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_unary_prefix_operation(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_cast_operation(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_binary_operation(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_ternary_operation(struct symbol_table *, struct type_tree **, struct node *);
void add_symbols_from_expression_list(struct symbol_table *, struct type_tree **, struct node *); 
void add_symbols_from_for_expr(struct symbol_table *, struct type_tree **, struct node *);

// Used for nodes that never interact with symbol table (e.g. string, number)
void add_no_symbols(struct symbol_table *, struct type_tree **, struct node *);

// Print functions
void print_symbol_table(FILE *, struct symbol_table *);
void print_string_literal_table(FILE*, struct string_literal_table *);
void print_string_literals(FILE *, struct string_literal_list *);
void print_symbol_table(FILE *, struct symbol_table *);

// Helper functions used in later phases of compiler
int symbol_table_bytes(struct symbol_table *symbol_table, bool set_bytes);
struct symbol_table *get_symbol_table_by_id(struct symbol_table *table, int id);
void update_stack_offsets(struct symbol_table *symbol_table, int frame_size);

#endif /* _SYMBOL_H */
