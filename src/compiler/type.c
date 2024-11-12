#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "node.h"
#include "symbol.h"
#include "type.h"

/***************************
 * CREATE TYPE EXPRESSIONS *
 ***************************/

struct type *type_basic(bool is_unsigned, enum type_basic_kind datatype) {
  struct type *basic;

  basic = malloc(sizeof(struct type));
  assert(NULL != basic);

  basic->kind = TYPE_BASIC;
  basic->data.basic.is_unsigned = is_unsigned;
  basic->data.basic.datatype = datatype;
  return basic;
}

struct type *type_array(unsigned int dim) {
  struct type *array;

  array = malloc(sizeof(struct type));
  assert(NULL != array);

  array->kind = TYPE_ARRAY;
  array->data.array.dim = dim;
  return array;
}

struct type *type_void(void) {
  struct type *v;

  v = malloc(sizeof(struct type));
  assert(NULL != v);

  v->kind = TYPE_VOID;
  return v;
}

struct type *type_pointer(void) {
  struct type *ptr;

  ptr = malloc(sizeof(struct type));
  assert(NULL != ptr);

  ptr->kind = TYPE_POINTER;
  return ptr;
}

struct type *type_function(struct type_tree **parameter_list) {
  struct type *fn;

  fn = malloc(sizeof(struct type));
  assert(NULL != fn);

  fn->kind = TYPE_FUNCTION;
  fn->data.function.parameter_types_list = parameter_list;
  return fn;
}

// Adds node to top of type tree
struct type_tree *type_tree_add(struct type_tree *tree, struct type *type) {
  struct type_tree *new_node = malloc(sizeof(struct type_tree));
  new_node->type = type;
  new_node->child = tree;
  return new_node;
}

/**************************
 * PRINT TYPE EXPRESSIONS *
 **************************/

void type_print_string(char *output, struct type_tree *ttree) {
  struct type *t = ttree->type;
  assert(NULL != t);

  char *buf;
  switch (t->kind) {
    case TYPE_BASIC:
      type_print_basic_string(output, t);
      break;
    case TYPE_POINTER:
      strcat(output, "pointer(");
      type_print_string(output, ttree->child);
      strcat(output, ")");
      break;
    case TYPE_ARRAY:
      buf = malloc(sizeof(char) * TYPE_MAX + 1);
      memset(buf, '\0', TYPE_MAX + 1);
      sprintf(buf, "array(%d, ", t->data.array.dim);
      strcat(output, buf);
      type_print_string(output, ttree->child);
      strcat(output, ")");
      break;
    case TYPE_FUNCTION:
      strcat(output, "function(");
      type_print_string(output, ttree->child);
      strcat(output, ", [");

      struct type_tree **ptl = t->data.function.parameter_types_list;

      // Print parameter list
      bool first_param_printed = false;
      while (*ptl) {
        if (first_param_printed) {
          strcat(output, ", ");
        }
        type_print_string(output, *ptl);
        first_param_printed = true;
        ++ptl;
      }
      strcat(output, "])");
      break;
    case TYPE_VOID:
      strcat(output, "void");
      break;
    default:
      assert(0);
      break;
  }
}

void type_print_basic_string(char *output, struct type *basic) {
  assert(TYPE_BASIC == basic->kind);

  if (basic->data.basic.is_unsigned) {
    strcat(output, "unsigned ");
  } else {
    strcat(output, "signed ");
  }

  switch (basic->data.basic.datatype) {
    case TYPE_BASIC_INT:
      strcat(output, "int");
      break;
    case TYPE_BASIC_LONG:
      strcat(output, "long int");
      break;
    case TYPE_BASIC_CHAR:
      strcat(output, "char");
      break;
    case TYPE_BASIC_SHORT:
      strcat(output, "short int");
      break;
    default:
      assert(0);
      break;
  }
}


/* 
   RETAINED FOR COMPATIBILITY WITH SCANNER CODE WRITTEN BY COURSE STAFF.
   (SPECIFICALLY, CODE IN FILE 'scanner.lex') NOT USED ELSEWHERE.
*/
static void type_print_basic(FILE *output, struct type *basic) {
  assert(TYPE_BASIC == basic->kind);

  if (basic->data.basic.is_unsigned) {
    fputs("unsigned", output);
  } else {
    fputs("  signed", output);
  }

  switch (basic->data.basic.datatype) {
    case TYPE_BASIC_INT:
      fputs("  int", output);
      break;
    case TYPE_BASIC_LONG:
      fputs(" long", output);
      break;
    default:
      assert(0);
      break;
  }
}

void type_print(FILE *output, struct type *kind) {
  assert(NULL != kind);

  switch (kind->kind) {
    case TYPE_BASIC:
      type_print_basic(output, kind);
      break;
    default:
      assert(0);
      break;
  }
}
