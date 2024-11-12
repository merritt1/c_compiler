#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "node.h"
#include "symbol.h"
#include "type.h"

unsigned int nextTableId;
unsigned int nextStringID;
unsigned int nextSymbolID;
struct string_literal_table label_references;

/*****************
 * SYMBOL TABLES *
 *****************/

struct symbol_table *create_table(struct symbol_table *parent) {
  struct symbol_table *table = malloc(sizeof(struct symbol_table));
  table->head = NULL;
  table->child = NULL;
  table->sibling = NULL;
  table->labels = NULL;
  table->parent = parent;
  table->id = nextTableId++;
  return table;
}

struct symbol_list *create_symbol(char *type, char *name, struct type_tree *type_tree,
                                  bool isDefined) {
  struct symbol_list *sym = malloc(sizeof(struct symbol_list));
  memset(sym->symbol.type, '\0', TYPE_MAX + 1);
  memset(sym->symbol.name, '\0', IDENTIFIER_MAX + 1);
  sym->symbol.stack_offset = 0;
  sym->symbol.fn_symbol_table_id = 0;
  assert(sym != NULL);

  strcpy(sym->symbol.type, type);
  strcpy(sym->symbol.name, name);
  sym->symbol.type_tree = type_tree;
  sym->next = NULL;
  sym->isDefined = isDefined;
  return sym;
}

bool symbol_match(struct symbol_list *first, struct symbol_list *second) {
  return (!(strcmp(first->symbol.name, second->symbol.name)));
}

struct symbol_list *add_symbol(struct symbol_table *symbol_table, 
                struct symbol_list *symbol, bool redeclarationPermitted) {
  if (!(symbol_table)) return NULL;
  if (!(symbol_table->head)) {
    symbol_table->head = symbol;
    return symbol;
  }

  struct symbol_list *temp = symbol_table->head;
  struct symbol_list *temp_prev;
  while (temp) {
    // Check if identifier is already in symbol table
    if (symbol_match(temp, symbol)) {
      // Allow multiple declarations of the same function
      if (redeclarationPermitted) {
        if (!(strcmp(temp->symbol.type, symbol->symbol.type))) {
          return temp;
        } else {
          printf("\n\n\n***ERROR: Incompatible declarations of function '%s'\n\n\n", 
                 symbol->symbol.name);
          return NULL;
        }
      }
      // Allow function to be defined once and only once
      if (!(temp->isDefined)) {
        if (!(strcmp(temp->symbol.type, symbol->symbol.type))) {
          temp->isDefined = true;
          temp->symbol.fn_symbol_table_id = symbol->symbol.fn_symbol_table_id;
          return temp;
        } else {
          printf("\n\n\n***ERROR: Redefinition of function identifier '%s'\n\n\n", 
                 symbol->symbol.name);
          return NULL;
        }      
      }

      printf("\n\n\n***ERROR: Redefinition of identifier '%s'\n\n\n", 
             symbol->symbol.name);
      return NULL;
    }
    temp_prev = temp;
    temp = temp->next;
  }
  temp_prev->next = symbol;
  return symbol;
}

// Add string literal to global string table
struct string_literal *string_table_add(struct string_literal_table *sl_tbl, char *text) {

  // search string table for string matching str, if found return corresponding entry
  struct string_literal_list *start = sl_tbl->head;
  while (start) {
    if (!(strcmp(start->str.val, text))) {
      return &start->str;
    }
    start = start->next;
  }

  // match not found; add string literal to table
  struct string_literal_list *s;

  s = malloc(sizeof(struct string_literal_list));
  assert(NULL != s);

  char buf[17];
  snprintf(buf, 17, "%s%d", "_StringLabel_", nextStringID++);
  strncpy(s->str.id, buf, 17);
  strncpy(s->str.val, text, STRING_MAX);

  s->next = sl_tbl->head;
  sl_tbl->head = s;
  return &s->str;
}


void add_child(struct symbol_table *parent, struct symbol_table *child) {
  if (!(parent->child)) {
    parent->child = child;
    return;
  }
  struct symbol_table *temp = parent->child;
  while (temp->sibling) {
    temp = temp->sibling;
  }
  temp->sibling = child;
}

struct symbol_table *youngest_child(struct symbol_table *parent) {
  if (!(parent->child)) return NULL;
  struct symbol_table *temp = parent->child;
  while (temp->sibling) {
    temp = temp->sibling;
  }
  return temp;
}

// Searches for identifier in provided symbol table, then in each enclosing scope.
// If identifier is not found, returns NULL.
struct symbol_table *find_identifier(struct symbol_table *current, char *name) {
  if (!(current)) return NULL;
  struct symbol_list *iter;
  for (iter = current->head; NULL != iter; iter = iter->next) {
    if (!(strcmp(iter->symbol.name, name))) {
     return current;
    }
  }
  if (!(current->parent)) return NULL;
  return find_identifier(current->parent, name);
}

// Searches for identifier in only the provided symbol table.
// If identifier is found, returns pointer to symbol.
// If identifier is not found, returns NULL.
struct symbol_list *find_symbol(struct symbol_table *table, char *name) {
  if (!(table)) return NULL;
  struct symbol_list *iter;
  for (iter = table->head; NULL != iter; iter = iter->next) {
    if (!(strcmp(iter->symbol.name, name))) {
     return iter;
    }
  }
  return NULL;
}

// Ascends symbol table tree if necessary to reach the appropriate
// procedure scope, returns pointer to the symbol table for labels
struct symbol_table *get_labels_table(struct symbol_table *current) {
  if (current->labels) {
    return current->labels;
  }
  if (current->parent) {
    return get_labels_table(current->parent);
  }
  return NULL;
}


// Prints errors if there are any references to undefined labels
void validate_label_references(struct string_literal_list *references,
                               struct symbol_table *labels) {
  if (!(references)) return;
  while (references) {
    // Print error if referenced label is not found in the symbol table
    if (find_identifier(labels, references->str.val) == NULL) {
      printf("\n\n\n***ERROR: Reference to undefined label '%s' -- searched in Table %d***\n\n\n", 
             references->str.val, labels->id);
    }
    references = references->next;
  }
}

/******************************************
 * TRAVERSE AST AND CREATE SYMBOL TABLES: *
 *              DECLARATIONS              *
 ******************************************/

void add_symbols_from_translation_unit_list(struct node *translation_unit_list, 
                                            struct symbol_table *symbol_table) {
  if (NULL != translation_unit_list->data.translation_unit_list.init) {
    add_symbols_from_translation_unit_list(translation_unit_list->data.translation_unit_list.init, symbol_table);
  }
  if (NULL != translation_unit_list->data.translation_unit_list.translation_unit) {
    add_symbols_from_top_level_decl(translation_unit_list->data.translation_unit_list.translation_unit, symbol_table);
  }
}

void add_symbols_from_top_level_decl(struct node *top_level_decl,
                                     struct symbol_table *symbol_table) {
  assert(NODE_DECL == top_level_decl->kind || NODE_FN_DEF == top_level_decl->kind);\
  if (NODE_DECL == top_level_decl->kind) {
    symbol_table->isDeclaration = true;
  } else if (NODE_FN_DEF == top_level_decl->kind) {
    symbol_table->isDeclaration = false;
  }
  top_level_decl->generate_symbols(symbol_table, NULL, top_level_decl);
}

void add_symbols_from_type_specifier(struct symbol_table *symbol_table,
                                     struct type_tree **type_tree,
                                     struct node *type_specifier) {
  assert(symbol_table || !symbol_table); // silence unused parameter warning
  assert(NODE_TYPE_SPECIFIER == type_specifier->kind);
  struct type *base_type = NULL;
  switch (type_specifier->data.type_specifier.typeid) {
  case SIGNED_CHAR:
    base_type = type_basic(false, TYPE_BASIC_CHAR);
    break;
  case UNSIGNED_CHAR:
    base_type = type_basic(true, TYPE_BASIC_CHAR);
    break;
  case SIGNED_SHORT_INT:
    base_type = type_basic(false, TYPE_BASIC_SHORT);
    break;
  case UNSIGNED_SHORT_INT:
    base_type = type_basic(true, TYPE_BASIC_SHORT);
    break;
  case SIGNED_INT:
    base_type = type_basic(false, TYPE_BASIC_INT);
    break;
  case UNSIGNED_INT:
    base_type = type_basic(true, TYPE_BASIC_INT);
    break;
  case SIGNED_LONG_INT:
    base_type = type_basic(false, TYPE_BASIC_LONG);
    break;
  case UNSIGNED_LONG_INT:
    base_type = type_basic(true, TYPE_BASIC_LONG);
    break;
  case VOID_T:
    base_type = type_void();
    break;
  }
  assert(base_type != NULL);
  struct type_tree *new_type_tree = type_tree_add(NULL, base_type);
  *type_tree = new_type_tree;
}

void add_symbols_from_decl(struct symbol_table *symbol_table,
                           struct type_tree **type_tree,
                           struct node *decl) {
  assert(NODE_DECL == decl->kind && type_tree == NULL);
  struct type_tree *base = malloc(sizeof(struct type_tree));

  struct node *ts = decl->data.decl.type_specifiers;
  ts->generate_symbols(symbol_table, &base, ts);

  struct node *idl = decl->data.decl.initialized_declarator_list;
  idl->generate_symbols(symbol_table, &base, idl);
}

void add_symbols_from_initialized_declarator_list(struct symbol_table *symbol_table,
                                                  struct type_tree **type_tree, 
                                                  struct node *initialized_declarator_list) {
  assert(NODE_INITIALIZED_DECLARATOR_LIST == initialized_declarator_list->kind);

  if (NULL != initialized_declarator_list->data.initialized_declarator_list.init) {
    struct node *i = initialized_declarator_list->data.initialized_declarator_list.init;
    struct type_tree **ntt = malloc(sizeof(struct type_tree *));
    *ntt = *type_tree;
    i->generate_symbols(symbol_table, ntt, i);
  }

  struct node *i = initialized_declarator_list->data.initialized_declarator_list.initialized_declarator;
  i->generate_symbols(symbol_table, type_tree, i);
}

void add_symbols_from_declarator(struct symbol_table *symbol_table,
                                 struct type_tree **type_tree,
                                 struct node *declarator) {
  assert(NODE_DECLARATOR == declarator->kind);
  struct node *d = declarator->data.declarator.declarator;
  struct node *ptr;
  char *type_string;
  bool redeclarationPermitted;
  bool isDefined;
  switch (declarator->data.declarator.declarator_type) {
    case SIMPLE_DECLARATOR:
      // Get name of identifier, and representation of it's type as a string
      type_string = malloc(sizeof(char) * (TYPE_MAX + 1));
      memset(type_string, '\0', TYPE_MAX + 1);
      type_print_string(type_string, *type_tree);
      char *identifier_name = declarator->data.declarator.declarator->data.identifier.name;
      
      // Allow functions to be declared multiple times, but defined only once.
      // All other declarations are also definitions.

      if ((*type_tree)->type->kind == TYPE_FUNCTION && symbol_table && symbol_table->isDeclaration) {
        redeclarationPermitted = true;
      } else {
        redeclarationPermitted = false;
      }
      if (symbol_table && symbol_table->isDeclaration && (*type_tree)->type->kind == TYPE_FUNCTION) {
        isDefined = false;
      } else {
        isDefined = true;
      }

      // Add identifier, type (string representation), and type tree to the symbol table
      struct symbol_list *symbol_to_add = create_symbol(type_string, identifier_name, *type_tree, isDefined);
      if ((*type_tree)->type->kind == TYPE_FUNCTION) {
        symbol_to_add->symbol.fn_symbol_table_id = nextTableId - 2;
      }
      symbol_to_add = add_symbol(symbol_table, symbol_to_add, redeclarationPermitted);

      // Update identifier node in AST
      if (symbol_to_add) {
        declarator->data.declarator.declarator->data.identifier.symbol_table = symbol_table;
        declarator->data.declarator.declarator->data.identifier.symbol = &(symbol_to_add->symbol);
      }

      if (!(symbol_table)) break;     // Identifier is named formal parameter in fn declaration

      break;
    case ARRAY_DECLARATOR:
      // handle array without bounds in parameter declarator (after identifier name)
      if (!(declarator->data.declarator.add_spec)) {
        *type_tree = type_tree_add(*type_tree, type_pointer());
        break;
      }
      // const_expr has been evaluated to a number
      add_array_to_type_tree(declarator->data.declarator.add_spec->nr.val, type_tree);
      d->generate_symbols(symbol_table, type_tree, d);
      break;
    case POINTER_DECLARATOR:
      ptr = declarator->data.declarator.add_spec;
      ptr->generate_symbols(symbol_table, type_tree, ptr);
      d->generate_symbols(symbol_table, type_tree, d);
      break;
    default:
      break;
  }
}

// helper function for adding array to type tree
void add_array_to_type_tree(unsigned int dim, struct type_tree **type_tree) {
    struct type *array = type_array(dim);
    struct type_tree *new_type_tree = type_tree_add(*type_tree, array);
    *type_tree = new_type_tree;
}

void add_symbols_from_pointer_list(struct symbol_table *symbol_table,
                                   struct type_tree **type_tree,
                                   struct node *ptr_list) {
  assert(symbol_table || !symbol_table); // silence unused parameter warning
  assert(NODE_POINTER_LIST == ptr_list->kind);
  if ((*type_tree)->type->kind == TYPE_VOID) {
    printf("\n\n\n***ERROR: Void pointers are not permitted.\n\n\n");
    return;
  }
  for (int i = 0; i < ptr_list->data.pointer_list.ptr_count; ++i) {
    *type_tree = type_tree_add(*type_tree, type_pointer());
  }
}

void add_symbols_from_function_declarator(struct symbol_table *symbol_table, 
                                          struct type_tree **type_tree,
                                          struct node *fn_declarator) {

  // array to hold parameter type trees
  struct type_tree **parameter_types_reversed = malloc(sizeof(struct type_tree *) * 127);
  struct type_tree **parameter_types = malloc(sizeof(struct type_tree *) * 127); // used later

  // initialize all pointers to NULL
  struct type_tree **temp1 = parameter_types_reversed;
  struct type_tree **temp2 = parameter_types;
  for (size_t i = 0; i < 127; i++) {
    *temp1 = NULL;
    *temp2 = NULL;
    ++temp1;
    ++temp2;
  }

  struct node *param_list = fn_declarator->data.function_declarator.parameter_type_list;
  if (symbol_table->isDeclaration) {
    // parameter names in function declaration are not added to any symbol table
    param_list->generate_symbols(NULL, parameter_types_reversed, param_list);
  } else {
    struct symbol_table *child = create_table(symbol_table);
    struct symbol_table *labels = create_table(NULL);
    child->labels = labels;
    add_child(symbol_table, child);
    param_list->generate_symbols(child, parameter_types_reversed, param_list);
  }

  /* Reverse parameter type list */
  int counter = 0;
  struct type_tree **loop = parameter_types_reversed;
  while (*loop) {
    ++loop;
    ++counter;
  }
  --counter;
  --loop;

  bool multipleParameters = (bool) counter;
  struct type_tree **loop2 = parameter_types;
  while (counter >= 0) {
    if ((*loop)->type->kind == TYPE_VOID && multipleParameters) {
      printf("\n\n\n***ERROR: \"Void\" cannot be mixed with other parameters.\n\n\n");
    }
    *loop2 = *loop;
    ++loop2;
    --counter;
    --loop;
  }
  /* End reverse parameter type list */

  // Make new function type node, add to top of type tree
  struct type *fn = type_function(parameter_types);
  *type_tree = type_tree_add(*type_tree, fn);

  // continue AST descent
  struct node *next = fn_declarator->data.function_declarator.direct_declarator;
  next->generate_symbols(symbol_table, type_tree, next);

}

void add_symbols_from_parameter_list(struct symbol_table *symbol_table,
                                     struct type_tree **type_tree,
                                     struct node *parameter_list) {

  if (NULL != parameter_list->data.parameter_list.init) {
    struct node *i = parameter_list->data.parameter_list.init;
    i->generate_symbols(symbol_table, type_tree + 1, i);
  }

  struct node *p = parameter_list->data.parameter_list.parameter_decl;
  p->generate_symbols(symbol_table, type_tree, p);
}

void add_symbols_from_parameter_decl(struct symbol_table *symbol_table, 
                                     struct type_tree **type_tree,
                                     struct node *parameter_decl) {
  struct type_tree *base = malloc(sizeof(struct type_tree));
  struct node *type_spec = parameter_decl->data.parameter_decl.type_specifiers;
  type_spec->generate_symbols(symbol_table, &base, type_spec);

  bool isVoid = (parameter_decl->data.parameter_decl.type_specifiers->data.type_specifier.typeid == VOID_T);
  struct node *opt_declarator = parameter_decl->data.parameter_decl.declarator_opt_abstract;
  if (symbol_table && !symbol_table->isDeclaration && !opt_declarator && !isVoid) {
    printf("\n\n\n***ERROR: Unnamed parameter in function definition.\n\n\n");
  }
  if (opt_declarator) {
    opt_declarator->generate_symbols(symbol_table, &base, opt_declarator);
  }
  *type_tree = base;
}

void add_symbols_from_abstract_declarator(struct symbol_table *symbol_table, 
                                          struct type_tree **type_tree,
                                          struct node *abstract_declarator) {
  struct node *p = abstract_declarator->data.abstract_declarator.pointer;
  p->generate_symbols(symbol_table, type_tree, p);

  struct node *dir_abs_declarator = abstract_declarator->data.abstract_declarator.dir_abs_declarator;
  dir_abs_declarator->generate_symbols(symbol_table, type_tree, dir_abs_declarator);
}

void add_symbols_from_direct_abstract_declarator_list(struct symbol_table *symbol_table, 
                                                      struct type_tree **type_tree,
                                                      struct node *dir_abs_declarator_list) {
  struct node *init = dir_abs_declarator_list->data.direct_abstract_declarator_list.init;
  if (init) {
    struct node *d = dir_abs_declarator_list->data.direct_abstract_declarator_list.dir_abs_declarator;
    add_array_to_type_tree(d->nr.val, type_tree);
    init->generate_symbols(symbol_table, type_tree, init);
  } else {
  // turn leftmost array specifier into pointer, add to type tree
  *type_tree = type_tree_add(*type_tree, type_pointer());
  return;
  }
}

/*******************************************
 * TRAVERSE AST AND CREATE SYMBOL TABLES:  *
 *  FUNCTION DEFINITIONS AND EXPRESSION    *
 *******************************************/

void add_symbols_from_fn_def(struct symbol_table *symbol_table,
                             struct type_tree **type_tree,
                             struct node *fn_def) {
  assert(type_tree || !type_tree); // silence unused parameter warning
  struct type_tree *base = malloc(sizeof(struct type_tree));
  struct node *fn_def_specifier = fn_def->data.function_definition.fn_def_specifier;
  fn_def_specifier->generate_symbols(symbol_table, &base, fn_def_specifier);

  // Initialize list of label references
  label_references.head = NULL;
  // Handle function body
  struct node *body = fn_def->data.function_definition.compound_statement;
  if (body) {
    body->generate_symbols(youngest_child(symbol_table), NULL, body);
  }
  
  // Validate
  validate_label_references(label_references.head, get_labels_table(youngest_child(symbol_table)));

}

void add_symbols_from_decl_or_statement_list(struct symbol_table *symbol_table, 
                                             struct type_tree **type_tree,
                                             struct node *dsl) {
  assert(type_tree || !type_tree); // silence unused parameter warning
  struct node *init = dsl->data.decl_or_statement_list.init;
  if (init) {
    init->generate_symbols(symbol_table, NULL, init);
  }

  struct node *d_or_s = dsl->data.decl_or_statement_list.decl_or_statement;
  d_or_s->generate_symbols(symbol_table, NULL, d_or_s);
}

void add_symbols_from_statement(struct symbol_table *symbol_table, 
                                struct type_tree **type_tree,
                                struct node *statement) {
  struct node *n;
  struct symbol_table *child;

  struct symbol_list *sym;
  struct symbol_table *references_table;
  char *label_name;

  switch (statement->data.statement.statement_type) {
  case STATEMENT_NULL:
  case STATEMENT_RETURN:
    n = statement->data.statement.expr;
    if (n) n->generate_symbols(symbol_table, type_tree, n);

    // Ensure function does not return an array or a function
    if (n && n->kind == NODE_CONDITIONAL_EXPR && 
        n->data.conditional_expr.conditional_expr->kind == NODE_IDENTIFIER &&
        n->data.conditional_expr.conditional_expr->data.identifier.symbol &&
        (isArrayType(n->data.conditional_expr.conditional_expr->data.identifier.symbol->type_tree) ||
        isFunctionType(n->data.conditional_expr.conditional_expr->data.identifier.symbol->type_tree))) {
      printf("\n\n\n***ERROR: Function may not return an array or function.\n\n\n");
    }

    break;
  case STATEMENT_CONTINUE:
  case STATEMENT_BREAK:
    break;
  case STATEMENT_ITERATIVE_FOR:
    n = statement->data.statement.expr;
    if (n) n->generate_symbols(symbol_table, type_tree, n);

    child = create_table(symbol_table);
    add_child(symbol_table, child);
    n = statement->data.statement.stmt;
    if (n) n->generate_symbols(child, NULL, n);
    break;
  case STATEMENT_ITERATIVE_WHILE:
    n = statement->data.statement.expr;
    n->generate_symbols(symbol_table, type_tree, n);

    child = create_table(symbol_table);
    add_child(symbol_table, child);
    n = statement->data.statement.stmt;
    n->generate_symbols(child, NULL, n);
    break;
  case STATEMENT_ITERATIVE_DO:
    n = statement->data.statement.expr;
    n->generate_symbols(symbol_table, type_tree, n);

    child = create_table(symbol_table);
    add_child(symbol_table, child);
    n = statement->data.statement.stmt;
    n->generate_symbols(child, NULL, n);
    break;
  case STATEMENT_CONDITIONAL:
    n = statement->data.statement.expr;
    n->generate_symbols(symbol_table, type_tree, n);

    child = create_table(symbol_table);
    add_child(symbol_table, child);
    n = statement->data.statement.stmt;
    n->generate_symbols(child, NULL, n);

    // Handle else case
    n = statement->data.statement.stmt2;
    if (!n) break;
    child = create_table(symbol_table);
    add_child(symbol_table, child);
    n->generate_symbols(child, NULL, n);
    break;
  case STATEMENT_COMPOUND:
    n = statement->data.statement.expr;
    child = create_table(symbol_table);
    add_child(symbol_table, child);
    if (n) n->generate_symbols(child, NULL, n);
    break;
  case STATEMENT_EXPRESSION:
    n = statement->data.statement.expr;
    n->generate_symbols(symbol_table, type_tree, n);
    break;
  case STATEMENT_LABELED:
    label_name = statement->data.statement.expr->data.identifier.name;
    sym = create_symbol("label", label_name, NULL, true);
    references_table = get_labels_table(symbol_table);
    add_symbol(references_table, sym, false);

    // Update identifier node in AST
    statement->data.statement.expr->data.identifier.symbol_table = references_table;
    statement->data.statement.expr->data.identifier.symbol = &(sym->symbol);

    // Continue descending AST
    n = statement->data.statement.stmt;
    n->generate_symbols(symbol_table, type_tree, n);
    break;
  case STATEMENT_GOTO:
    label_name = statement->data.statement.expr->data.identifier.name;
    string_table_add(&label_references, label_name);
    break;
  default:
    break;
  }
}

void add_symbols_from_conditional_expr(struct symbol_table *symbol_table, 
                                       struct type_tree **type_tree,
                                       struct node *c_expr) {
  struct node *n = c_expr->data.conditional_expr.conditional_expr;
  n->generate_symbols(symbol_table, type_tree, n);
}

void add_symbols_from_identifier(struct symbol_table *symbol_table, 
                                 struct type_tree **type_tree,
                                 struct node *identifier) {
  assert(type_tree || !type_tree); // silence unused parameter warning
  char *name = identifier->data.identifier.name;
  // Get pointers to relevant symbol table and symbol
  struct symbol_table *st = find_identifier(symbol_table, name);
  struct symbol_list *sym = find_symbol(st, name);

  // Print error if undeclared identifier is referenced
  //   Note: If identifier is name of formal parameter in fn declaration,
  //   symbol_table will be NULL.
  if (symbol_table && !sym) {
    printf("\n\n\n***ERROR: Reference to undefined identifier '%s'\n\n\n", 
           identifier->data.identifier.name);
  }

  // Update AST
  identifier->data.identifier.symbol_table = st;
  sym ? identifier->data.identifier.symbol = &(sym->symbol) : NULL;
}

void add_symbols_from_postfix_expr(struct symbol_table *symbol_table, 
                                   struct type_tree **type_tree,
                                   struct node *postfix_expr) {
  struct node *n = postfix_expr->data.postfix_expr.operand;
  n->generate_symbols(symbol_table, type_tree, n);

  n = postfix_expr->data.postfix_expr.expression_list;
  if (n) {
      n->generate_symbols(symbol_table, type_tree, n);
  }
}

void add_symbols_from_unary_prefix_operation(struct symbol_table *symbol_table, 
                                             struct type_tree **type_tree,
                                             struct node *unary_prefix) {
  struct node *n = unary_prefix->data.unary_prefix_operation.operand;
  n->generate_symbols(symbol_table, type_tree, n);
}

void add_symbols_from_cast_operation(struct symbol_table *symbol_table, 
                                     struct type_tree **type_tree,
                                     struct node *cast_operation) {
  struct node *n = cast_operation->data.cast_operation.operand;
  n->generate_symbols(symbol_table, type_tree, n);
}

void add_symbols_from_binary_operation(struct symbol_table *symbol_table, 
                                       struct type_tree **type_tree,
                                       struct node *binary_operation) {
  struct node *l = binary_operation->data.binary_operation.left_operand;
  struct node *r = binary_operation->data.binary_operation.right_operand;

  l->generate_symbols(symbol_table, type_tree, l);
  r->generate_symbols(symbol_table, type_tree, r);
}

void add_symbols_from_ternary_operation(struct symbol_table *symbol_table, 
                                        struct type_tree **type_tree,
                                        struct node *ternary_operation) {
  struct node *first = ternary_operation->data.ternary_operation.first_op;
  struct node *second = ternary_operation->data.ternary_operation.second_op;
  struct node *third = ternary_operation->data.ternary_operation.third_op;

  first->generate_symbols(symbol_table, type_tree, first);
  second->generate_symbols(symbol_table, type_tree, second);
  third->generate_symbols(symbol_table, type_tree, third);
}

void add_symbols_from_expression_list(struct symbol_table *symbol_table, 
                                      struct type_tree **type_tree,
                                      struct node *expression_list) {
  struct node *init = expression_list->data.expression_list.init;
  if (init) {
    init->generate_symbols(symbol_table, type_tree, init);
  }
  struct node *n = expression_list->data.expression_list.assignment_expr;
  n->generate_symbols(symbol_table, type_tree, n);
}

void add_symbols_from_for_expr(struct symbol_table *symbol_table, 
                                 struct type_tree **type_tree,
                                 struct node *for_expr) {
  struct node *init = for_expr->data.for_expr.init;
  if (init) {
    init->generate_symbols(symbol_table, type_tree, init);
  }
  
  struct node *condition = for_expr->data.for_expr.condition;
  if (condition) {
    condition->generate_symbols(symbol_table, type_tree, condition);
  }

  struct node *update = for_expr->data.for_expr.update;
  if (update) {
    update->generate_symbols(symbol_table, type_tree, update);
  }
}

// Used for nodes that never interact with symbol table (e.g. string, number)
void add_no_symbols(struct symbol_table *symbol_table,
                    struct type_tree **type_tree,
                    struct node *expression_list) {
  assert(type_tree || !type_tree); // silence unused parameter warning
  assert(symbol_table || !symbol_table); // silence unused parameter warning
  assert(expression_list || !expression_list); // silence unused parameter warning
  return;
}


/*********************************
 * PRINT FUNCTION: SYMBOL TABLES *
 *********************************/

void print_symbol_table(FILE *output, struct symbol_table *table) {
  if (table->head) {
    fprintf(output, "Symbol Table ID: %d\n", table->id);

    struct symbol_list *iter;
    for (iter = table->head; NULL != iter; iter = iter->next) {
      fprintf(output, "  %s -> %s\n", iter->symbol.name, iter->symbol.type);
    }
    fputs("\n", output);
  }
  if (table->labels) {
    print_symbol_table(output, table->labels);
  }
  if (table->child) {
    print_symbol_table(output, table->child);
  }
  if (table->sibling) {
    print_symbol_table(output, table->sibling);
  }
}

void print_string_literal_table(FILE *output, struct string_literal_table *sl_table) {
  fprintf(output, "String Literal Table:\n");
  print_string_literals(output, sl_table->head);
  fprintf(output, "\n");
}

void print_string_literals(FILE *output, struct string_literal_list *current) {
  if (current->next) {
    print_string_literals(output, current->next);
  }
  fprintf(output, "  %s\t%s\n", current->str.id, current->str.val);
}

/*****************************************************
 * Helper functions used in later phases of compiler *
 *****************************************************/

// Returns the number of bytes required to store the contents of the specified
// symbol table in memory. Enforces memory alignment rules, and includes variables
// in any enclosed scope. (If there are multiple enclosed scopes, variables in them
// will share memory.) Stores the position of each object relative to the start
// of the local variable section of the stack frame as the stack_offset
// attribute of the associated symbol table entry.
int symbol_table_bytes(struct symbol_table *symbol_table, bool set_bytes) {
  int byte_count = 0;
  int child_byte_count = 0;
  int sibling_byte_count = 0;
  if (symbol_table->head) {
    struct symbol_list *iter;
    for (iter = symbol_table->head; NULL != iter; iter = iter->next) {
      struct symbol *sym = &(iter->symbol);
      struct type_tree *ttree = iter->symbol.type_tree;
      if (isBasicType(ttree)) {
        if (ttree->type->data.basic.datatype == TYPE_BASIC_CHAR) {
          if (set_bytes) sym->stack_offset = byte_count;
          byte_count += 1;
        } else if (ttree->type->data.basic.datatype == TYPE_BASIC_SHORT) {
          if (byte_count % 2) byte_count += (2 - (byte_count % 2)); //Alignment
          if (set_bytes) sym->stack_offset = byte_count;
          byte_count += 2;
        } else if (ttree->type->data.basic.datatype == TYPE_BASIC_INT) {
          if (byte_count % 4) byte_count += (4 - (byte_count % 4)); //Alignment
          if (set_bytes) sym->stack_offset = byte_count;
          byte_count += 4;
        } else if (ttree->type->data.basic.datatype == TYPE_BASIC_LONG) {
          if (byte_count % 4) byte_count += (4 - (byte_count % 4)); //Alignment
          if (set_bytes) sym->stack_offset = byte_count;
          byte_count += 4;
        }
      } else if (isPointerType(ttree)) {
        if (byte_count % 4) byte_count += (4 - (byte_count % 4)); //Alignment
        if (set_bytes) sym->stack_offset = byte_count;
        byte_count += 4;
      } else if (isArrayType(ttree)) {
        if (byte_count % 4) byte_count += (4 - (byte_count % 4)); //Alignment
        if (set_bytes) sym->stack_offset = byte_count;
        byte_count += arraySizeBytes(ttree);
      }
    }
  }  
  if (!set_bytes) return byte_count;
  if (symbol_table->child) {
    child_byte_count = symbol_table_bytes(symbol_table->child, set_bytes);
    struct symbol_table *sibling = symbol_table->child->sibling;
    while (sibling) {
      sibling_byte_count = symbol_table_bytes(sibling, set_bytes);
      if (sibling_byte_count > child_byte_count) {
        child_byte_count = sibling_byte_count;
      }
      sibling = sibling->sibling;
    } 
  }
  return byte_count + child_byte_count;
}

// Returns pointer to symbol table with specified numeric id.
struct symbol_table *get_symbol_table_by_id(struct symbol_table *table, int id) {
  if ((int)table->id == id) return table;

  struct symbol_table *s;
  if (table->labels) {
    s = get_symbol_table_by_id(table->labels, id);
    if (s) return s;
  }
  if (table->child) {
    s = get_symbol_table_by_id(table->child, id);
    if (s) return s;
  }
  if (table->sibling) {
    s = get_symbol_table_by_id(table->sibling, id);
    if (s) return s;
  }
  return NULL;
}

// Adds the size of a stack frame the stack_offset value associated with each identifier.
void update_stack_offsets(struct symbol_table *symbol_table, int frame_size) {
  if (symbol_table->head) {
    struct symbol_list *iter;
    for (iter = symbol_table->head; NULL != iter; iter = iter->next) {
      struct symbol *sym = &(iter->symbol);
      struct type_tree *ttree = iter->symbol.type_tree;
      if (isBasicType(ttree) || isPointerType(ttree) || isArrayType(ttree)) {
        sym->stack_offset += frame_size;
      }
    }
  }

  if (symbol_table->child) {
    update_stack_offsets(symbol_table->child, frame_size + symbol_table_bytes(symbol_table, false));
    struct symbol_table *sibling = symbol_table->child->sibling;
    while (sibling) {
      update_stack_offsets(sibling, frame_size + symbol_table_bytes(symbol_table, false));
      sibling = sibling->sibling;
    }
  }
}
