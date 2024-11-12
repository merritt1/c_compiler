#ifndef _TYPE_H
#define _TYPE_H

#include <stdio.h>
#include <stdbool.h>

struct node;

enum type_kind {
  TYPE_BASIC,
  TYPE_VOID,
  TYPE_POINTER,
  TYPE_ARRAY,
  TYPE_FUNCTION,
  TYPE_LABEL
};

enum type_basic_kind {
  TYPE_BASIC_CHAR,
  TYPE_BASIC_SHORT,
  TYPE_BASIC_INT,
  TYPE_BASIC_LONG
};

struct type {
  enum type_kind kind;
  union {
    struct {
      bool is_unsigned;
      enum type_basic_kind datatype;
    } basic;
    struct {
      unsigned int dim;
    } array;
    struct {
      struct type_tree** parameter_types_list;
    } function;
  } data;
};

struct type_tree {
  struct type *type;
  struct type_tree *child;
};

// Type constructors
struct type *type_basic(bool is_unsigned, enum type_basic_kind datatype);
struct type *type_pointer(void);
struct type *type_function(struct type_tree **);
struct type *type_array(unsigned int dim);
struct type *type_void(void);

// Add node to top of type tree
struct type_tree *type_tree_add(struct type_tree *, struct type*);

// Print type expressions
void type_print_basic_string(char *output, struct type *basic);
void type_print_string(char *output, struct type_tree *ttree);

/* RETAINED FOR COMPATIBILITY WITH SCANNER CODE WRITTEN BY COURSE STAFF*/
static void type_print_basic(FILE *output, struct type *basic);
void type_print(FILE *output, struct type *type);

#endif /* _TYPE_H */
