#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "type_check.h"
#include "node.h"
#include "symbol.h"

struct type_tree **current_param;
extern bool print_type_errors;

/****************************************
 * RETRIEVE INFORMATION ABOUT TYPE TREE *
 ****************************************/
bool isPointerType(struct type_tree *ttree) {
  if (!ttree) return false;
  if (ttree->type && (ttree->type->kind == TYPE_POINTER)) return true;
  return false;
}

bool isArrayType(struct type_tree *ttree) {
  if (!ttree) return false;
  if (ttree->type && (ttree->type->kind == TYPE_ARRAY)) return true;
  return false;
}

bool isBasicType(struct type_tree *ttree) {
  if (!ttree) return false;
  if (ttree->type && (ttree->type->kind == TYPE_BASIC)) return true;
  return false;
}

bool isFunctionType(struct type_tree *ttree) {
  if (!ttree) return false;
  if (ttree->type && (ttree->type->kind == TYPE_FUNCTION)) return true;
  return false;
}

enum type_specifier baseTypeFromTree(struct type_tree *input_tree) {
  if (!(isBasicType(input_tree))) return VOID_T; // Not arithmetic type
  bool is_unsigned = input_tree->type->data.basic.is_unsigned;
  if (is_unsigned) {
    switch (input_tree->type->data.basic.datatype) {
    case TYPE_BASIC_CHAR:
      return UNSIGNED_CHAR;
    case TYPE_BASIC_SHORT:
      return UNSIGNED_SHORT_INT;
    case TYPE_BASIC_INT:
      return UNSIGNED_INT;
    case TYPE_BASIC_LONG:
      return UNSIGNED_LONG_INT;
    }
  } else {
    switch (input_tree->type->data.basic.datatype) {
    case TYPE_BASIC_CHAR:
      return SIGNED_CHAR;
    case TYPE_BASIC_SHORT:
      return SIGNED_SHORT_INT;
    case TYPE_BASIC_INT:
      return SIGNED_INT;
    case TYPE_BASIC_LONG:
      return SIGNED_LONG_INT;
    }
  }

  return VOID_T; //Error
}

/***********************************************
 * RETURN INFORMATION ABOUT TOP TYPE TREE NODE *
 *          (FOR INTEGER TYPES ONLY)           *
 ***********************************************/
bool isUnsigned(struct type_tree *input_tree) {
  if (!(isBasicType(input_tree))) return false;
  return (input_tree->type->data.basic.is_unsigned);
}
int typeRank(struct type_tree *input_tree) {
  if (!(isBasicType(input_tree))) return -1; // Not arithmetic type
  switch (input_tree->type->data.basic.datatype) {
  case TYPE_BASIC_CHAR:
    return 10;
  case TYPE_BASIC_SHORT:
    return 30;
  case TYPE_BASIC_INT:
    return 40;
  case TYPE_BASIC_LONG:
    return 50;
  }
  return -1; // Error
}

/***********************************************
 * COMPATIBILITY CHECKER FOR WHOLE TYPE TREES  *
 ***********************************************/
bool typesIdentical(struct type_tree *ttree1, struct type_tree *ttree2) {
  while (ttree1 && ttree2) {
    if (ttree1->type->kind == ttree2->type->kind) {
      if (ttree1->type->kind == TYPE_BASIC) {
        if ((ttree1->type->data.basic.is_unsigned == ttree2->type->data.basic.is_unsigned) &&
            (ttree1->type->data.basic.datatype == ttree2->type->data.basic.datatype)) {
          ttree1 = ttree1->child;
          ttree2 = ttree2->child;
          continue;
        }
        return false;
      }
      ttree1 = ttree1->child;
      ttree2 = ttree2->child;
      continue;
    }
    return false;
  }
  if (ttree1 || ttree2) return false;
  return true;
}

void baseTypeToString(char *type_string, enum type_specifier type) {
  switch(type) {
  case SIGNED_CHAR:
    strcpy(type_string, "signed char");
    break;
  case UNSIGNED_CHAR:
    strcpy(type_string, "unsigned char");
    break;
  case SIGNED_SHORT_INT:
    strcpy(type_string, "signed short int");
    break;
  case UNSIGNED_SHORT_INT:
    strcpy(type_string, "unsigned short int");
    break;
  case SIGNED_INT:
    strcpy(type_string, "signed int");
    break;
  case UNSIGNED_INT:
    strcpy(type_string, "unsigned int");
    break;
  case SIGNED_LONG_INT:
    strcpy(type_string, "signed long int");
    break;
  case UNSIGNED_LONG_INT:
    strcpy(type_string, "unsigned long int");
    break;
  default:
    break;  //Error
  }
}

bool canRepresentAllUnsigned(enum type_specifier signed_type, enum type_specifier unsigned_type) {
  switch(signed_type) {
  case SIGNED_CHAR:
    return false;
  case SIGNED_SHORT_INT: 
    if (unsigned_type == UNSIGNED_CHAR) {
      return true;
    }
    return false;
  case SIGNED_INT:
  case SIGNED_LONG_INT:
    if (unsigned_type == UNSIGNED_CHAR || unsigned_type == UNSIGNED_SHORT_INT) {
      return true;
    }
    return false;
  default:
    break;
  }

  return false; //Error
}

bool isMathematicalLogical(enum node_binary_operation op) {
  if ((op == BINOP_MULTIPLICATION) ||
      (op == BINOP_DIVISION) ||
      (op == BINOP_ADDITION) ||
      (op == BINOP_SUBTRACTION) ||
      (op == BINOP_MODDIV) ||
      (op == BINOP_LOGICAL_OR) ||
      (op == BINOP_LOGICAL_AND) ||
      (op == BINOP_BITWISE_OR) ||
      (op == BINOP_BITWISE_AND) ||
      (op == BINOP_BITWISE_XOR) ||
      (op == BINOP_EQUALITY) ||
      (op == BINOP_INEQUALITY) ||
      (op == BINOP_LESS) ||
      (op == BINOP_LESS_EQUAL) ||
      (op == BINOP_GREATER) ||
      (op == BINOP_GREATER_EQUAL) ||
      (op == BINOP_LEFT_SHIFT) ||
      (op == BINOP_RIGHT_SHIFT) ||
      (op == BINOP_COMMA)) {
    return true;
  }
  return false;
}

enum type_basic_kind basicKindFromTypeSpecifier(enum type_specifier t) {
  if (t == SIGNED_CHAR || t == UNSIGNED_CHAR) return TYPE_BASIC_CHAR;
  if (t == SIGNED_SHORT_INT || t == UNSIGNED_SHORT_INT) return TYPE_BASIC_SHORT;
  if (t == SIGNED_INT || t == UNSIGNED_INT) return TYPE_BASIC_INT;
  if (t == SIGNED_LONG_INT || t == UNSIGNED_LONG_INT) return TYPE_BASIC_LONG;

  return 0; //Error
}

enum type_specifier unsignedVersion(enum type_specifier signed_type) {
  switch(signed_type) {
  case SIGNED_CHAR:
    return UNSIGNED_CHAR;
  case SIGNED_SHORT_INT:
    return UNSIGNED_SHORT_INT;
  case SIGNED_INT:
    return UNSIGNED_INT;
  case SIGNED_LONG_INT:
    return UNSIGNED_LONG_INT;
  default:
    break;
  }
  return VOID_T; //Error
}


/**********************************************************
 * FUNCTIONS TO INSERT EXPLICIT CAST NODES WHERE REQUIRED *
 **********************************************************/
void insertCastNodeBasic(struct node **parent, struct node *child, 
                               enum type_specifier type) {
  struct node *type_node = node_type_specifier(child->location, type);
  struct node *cast_node;
  if (child->kind == NODE_CAST_OPERATION) {
    cast_node = node_cast_operation(child->location, type_node, child->data.cast_operation.operand);
  } else {
    cast_node = node_cast_operation(child->location, type_node, child);
  }
  cast_node->check_types(cast_node);
  (*parent) = cast_node;
  return;
}

void insertCastNodeCompound(struct node **parent, struct node *child, 
                               struct type_tree *ttree) {
 struct node *type_node = NULL;
 struct node *ptr_list = NULL;
  while (ttree) {
    if (ttree->type->kind == TYPE_POINTER) {
      ptr_list = node_pointer_list(child->location, ptr_list); 
    }
    if (ttree->type->kind == TYPE_BASIC) {
      type_node = node_type_specifier(child->location, baseTypeFromTree(ttree));
      if (ptr_list) {
        type_node = node_abstract_declarator(child->location, type_node, ptr_list);
      }
    }
    ttree = ttree->child;
  }
  struct node *cast_node;
  if (child->kind == NODE_CAST_OPERATION) {
    cast_node = node_cast_operation(child->location, type_node, child->data.cast_operation.operand);
  } else {
    cast_node = node_cast_operation(child->location, type_node, child);
  }
  cast_node->check_types(cast_node);
  (*parent) = cast_node;
  return;
}

struct type_tree *simulateBinop(struct node *left_operand, 
                                            struct node *right_operand) {
  if (right_operand) {
    struct node *binop = node_binary_operation(left_operand->location, BINOP_ADDITION, left_operand, right_operand);
    binop->check_types(binop);
    return binop->type_tree;
  } else {
    struct node *number = node_number(left_operand->location, "1");
    number->check_types(number);
    struct node *binop = node_binary_operation(left_operand->location, BINOP_ADDITION, left_operand, number);
    binop->check_types(binop);
    return binop->type_tree;
  }
}


/*********************
 * USUAL CONVERSIONS *
 *********************/
void usualUnaryConversions(struct node **parent, struct node *child) {
  if (isBasicType(child->type_tree)) {
    int op_rank = typeRank(child->type_tree);
    if (op_rank >= 40) return;
    if (op_rank < 40 && (!isUnsigned(child->type_tree) || 
               (canRepresentAllUnsigned(SIGNED_INT, baseTypeFromTree(child->type_tree))))) {
      insertCastNodeBasic(parent, child, SIGNED_INT);
    } else {
      insertCastNodeBasic(parent, child, UNSIGNED_INT);
    }
  } else if (isArrayType(child->type_tree)) {
    (*parent)->type_tree = type_tree_add(child->type_tree->child, type_pointer());
  }
}

void usualAssignmentConversions(struct node **parent, struct node *child,
                                  struct type_tree *type_to_match) {
  if (typesIdentical(child->type_tree, type_to_match)) return;
  insertCastNodeCompound(parent, child, type_to_match);
}

/**********************************
 * TRAVERSE AST AND CHECK TYPES:  *
 *          EXPRESSIONS           *
 **********************************/


/***************
 * BASIC TYPES *
 ***************/
void check_types_identifier(struct node *identifier) {
  assert(NODE_IDENTIFIER == identifier->kind);
  identifier->lvalue_possible = true;

  if (identifier->data.identifier.symbol) {
    identifier->type_tree = identifier->data.identifier.symbol->type_tree;
  }

  if (identifier->data.identifier.symbol && isArrayType(identifier->data.identifier.symbol->type_tree)) {
    identifier->modifiable = false;
  } else {
    identifier->modifiable = true;
  }
  return;
}

void check_types_number(struct node *number) {
  assert(NODE_NUMBER == number->kind);
  number->lvalue_possible = false;

  number->type_tree = type_tree_add(NULL, type_basic(false, TYPE_BASIC_INT));
  return;
}

void check_types_string(struct node *string) {
  // String gets type tree representing type pointer(unsigned char)
  assert(NODE_STRING == string->kind);
  string->lvalue_possible = false;

  struct type_tree *ntt = type_tree_add(NULL, type_basic(false, TYPE_BASIC_CHAR));
  string->type_tree = type_tree_add(ntt, type_pointer());
  return;
}

/*******************
 * END BASIC TYPES *
 *******************/



/********************
 * UNARY OPERATIONS *
 ********************/

void check_types_unary_prefix_operation(struct node *unary_prefix_op) {

  assert(NODE_UNARY_PREFIX_OPERATION == unary_prefix_op->kind);
  unary_prefix_op->lvalue_possible = false;

  int operation = unary_prefix_op->data.unary_prefix_operation.operation;
  struct node *op = unary_prefix_op->data.unary_prefix_operation.operand;
  struct node **to_modify = &unary_prefix_op->data.unary_prefix_operation.operand;

  op->check_types(op);
  unary_prefix_op->type_tree = op->type_tree;

  // For unary preincrement and predecrement
  struct type_tree *operand_type_tree;
  char op_type_string[TYPE_MAX + 1];
  char res_type_string[TYPE_MAX + 1] = "";

  if (operation == UNARY_MINUS || operation == UNARY_PLUS) {
    if (!isBasicType(op->type_tree)) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Operand of unary '+' or '-' "
               "must be an arithmetic type.\n");
      }
      return;    
    }
    usualUnaryConversions(to_modify, op);
  }

  if (operation == UNARY_LOGICAL_NEGATION) {
    if (!isBasicType(op->type_tree) && !isPointerType(op->type_tree)) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Operand of unary '!' "
               "must be a scalar type.\n");      
      }
      return;    
    }
    usualUnaryConversions(to_modify, op);
  }

  if (operation == UNARY_BITWISE_NEGATION) {
    if (!isBasicType(op->type_tree)) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Operand of unary '~' "
                 "must be an integer type.\n");
      }
      return;    
    }
    usualUnaryConversions(to_modify, op);
  }

  if (operation == UNARY_ADDRESS_EXPR) {
    if (!(op->lvalue_possible)) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Operand of operator '&' "
               "must be an lvalue.\n");
      }
      return;    
    }
    unary_prefix_op->type_tree = type_tree_add(unary_prefix_op->type_tree, type_pointer());
  }

  if (operation == UNARY_INDIRECTION_EXPR) {
    if (!(isPointerType(op->type_tree))) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Operand of indirection operator '*' "
               "must be a pointer.\n");
      }
      return;    
    }
    unary_prefix_op->type_tree = unary_prefix_op->type_tree->child;
    unary_prefix_op->lvalue_possible = true;
  }

  if (operation == UNARY_PREINCREMENT || operation == UNARY_PREDECREMENT) {
    if (!(op->lvalue_possible)) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Operand of unary increment/decrement "
               "operator must be an lval.\n");
      }
      return;
    }
    // Simulate usual binary operation conversions
    operand_type_tree = simulateBinop(op, NULL);
    type_print_string(op_type_string, operand_type_tree);
    strcpy(unary_prefix_op->data.unary_prefix_operation.op_type_spec, op_type_string);
    type_print_string(res_type_string, op->type_tree);
    strcpy(unary_prefix_op->data.unary_prefix_operation.res_type_spec, res_type_string);
    return;
  }
}

void check_types_postfix_expression(struct node *postfix_expr) {
  assert(NODE_POSTFIX_EXPR == postfix_expr->kind);

  enum node_postfix_expr expr_type = postfix_expr->data.postfix_expr.postfix_expr_type;
  struct node *op = postfix_expr->data.postfix_expr.operand;
  op->check_types(op);

  struct type_tree *operand_type_tree;
  char op_type_string[TYPE_MAX + 1] = "";
  char res_type_string[TYPE_MAX + 1] = "";

  switch(expr_type) {
  case POSTFIX_FN_CALL:
    // Extract return type from type tree and label node so return type propagates
    op = postfix_expr->data.postfix_expr.operand;
    postfix_expr->type_tree = op->data.identifier.symbol->type_tree->child;
    current_param = op->data.identifier.symbol->type_tree->type->data.function.parameter_types_list;

    // Check compatibility of actual parameters
    op = postfix_expr->data.postfix_expr.expression_list;
    if (op) op->check_types(op);
    return;
  case POSTINCREMENT:
  case POSTDECREMENT:
    if (!(op->lvalue_possible)) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Operand of unary increment/decrement "
               "operator must be an lval.\n");
      }
      return;
    }
    // Simulate usual binary operation conversions
    operand_type_tree = simulateBinop(op, NULL);
    type_print_string(op_type_string, operand_type_tree);
    strcpy(postfix_expr->data.postfix_expr.op_type_spec, op_type_string);
    type_print_string(res_type_string, op->type_tree);
    strcpy(postfix_expr->data.postfix_expr.res_type_spec, res_type_string);

    // Tag postfix expression node with appropriate type
    postfix_expr->type_tree = op->type_tree;
    return;
  default:
    break;
  }
  postfix_expr->lvalue_possible = false;
}
/************************
 * END UNARY OPERATIONS *
 ************************/



/*********************
 * BINARY OPERATIONS *
 *********************/
void check_types_binary_operation(struct node *binary_operation) {
  assert(NODE_BINARY_OPERATION == binary_operation->kind);
  binary_operation->lvalue_possible = false;

  struct node *l_op = binary_operation->data.binary_operation.left_operand;
  struct node **l_op_modify = &(binary_operation->data.binary_operation.left_operand);
  l_op->check_types(l_op);

  struct node *r_op = binary_operation->data.binary_operation.right_operand;
  struct node **r_op_modify = &(binary_operation->data.binary_operation.right_operand);
  r_op->check_types(r_op);

  enum node_binary_operation operation = binary_operation->data.binary_operation.operation;

  if (isMathematicalLogical(operation)) {
    // Perform usual unary conversions on each operand
    usualUnaryConversions(l_op_modify, l_op);
    usualUnaryConversions(r_op_modify, r_op);
    l_op = binary_operation->data.binary_operation.left_operand;
    r_op = binary_operation->data.binary_operation.right_operand;
  }

  int l_op_rank, r_op_rank;
  bool l_op_unsigned, r_op_unsigned;
  int cast_type;
    
  // MULTIPLICATIVE OPERATORS
  if (operation == BINOP_MULTIPLICATION) {
    if (!(isBasicType(r_op->type_tree) && isBasicType(l_op->type_tree))) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Both operands of operator '*' must be arithmetic types.\n");
      }
    } 
  }
  if (operation == BINOP_DIVISION) {
    if (!(isBasicType(r_op->type_tree) && isBasicType(l_op->type_tree))) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Both operands of operator '/' must be arithmetic types.\n");
      }
    } 
  }
  if (operation == BINOP_MODDIV) {
    if (!(isBasicType(r_op->type_tree) && isBasicType(l_op->type_tree))) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Both operands of operator '%%' must be integer types.\n");
      }
      return;
    } 
  }

  // ADDITIVE OPERATORS
  if (operation == BINOP_ADDITION) {
    // Left operand pointer, right operand integer
    if (isPointerType(l_op->type_tree)) {
      if (isBasicType(r_op->type_tree) && 
                     (r_op->type_tree->type->data.basic.datatype == TYPE_BASIC_INT)) {
        // Mark binop node with pointer type and return
        binary_operation->type_tree = l_op->type_tree;
        return;
      } else {
        if (print_type_errors) {
          printf("\n\n\n***ERROR: If one operand to '+' is a pointer, the other must be an int.\n");
        }
        return;
      }
    }
    // Right operand pointer, left operand integer
    if (isPointerType(r_op->type_tree)) {
      if (isBasicType(l_op->type_tree) && 
                     (l_op->type_tree->type->data.basic.datatype == TYPE_BASIC_INT)) {
        // Mark binop node with pointer type and return
        binary_operation->type_tree = r_op->type_tree;
        binary_operation->lvalue_possible = false;
        return;
      } else {
        if (print_type_errors) {
          printf("\n\n\n***ERROR: If one operand to '+' is a pointer, the other must be an int.\n"); 
        }
        return;
      }
    }
    if (!(isBasicType(l_op->type_tree) && isBasicType(r_op->type_tree))) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Invalid operands to operator '+'.\n");
      }
      return; 
    }
  }
  if (operation == BINOP_SUBTRACTION) {
    // Left operand pointer, right operand integer
    if (isPointerType(l_op->type_tree)) {
      if (isBasicType(r_op->type_tree) && 
                     (r_op->type_tree->type->data.basic.datatype == TYPE_BASIC_INT)) {
        // Mark binop node with pointer type and return
        binary_operation->type_tree = l_op->type_tree;
        binary_operation->lvalue_possible = false;
        return;
      } else if (isPointerType(r_op->type_tree)) {
        // Mark binop node with pointer type and return
        binary_operation->type_tree = l_op->type_tree;
        binary_operation->lvalue_possible = false;
        return;
      } else {
        if (print_type_errors) {
          printf("\n\n\n***ERROR: If left operand to 'i' is a pointer,"
                 " the other must be an int or compatible pointer.\n");
        }
        return;
      }
    }
    // Right operand pointer, left operand integer
    if (isPointerType(r_op->type_tree)) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Right operand of '-' cannot"
                 " be a pointer.\n");
      }
      return;
    }
    if (!(isBasicType(l_op->type_tree) && isBasicType(r_op->type_tree))) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Invalid operands to operator '-'.\n");
      }
      return; 
    }
  }

  if (operation == BINOP_BITWISE_OR || operation == BINOP_BITWISE_AND ||
      operation == BINOP_BITWISE_XOR) {
    if (!(isBasicType(r_op->type_tree) && isBasicType(l_op->type_tree))) {
      if (print_type_errors)  {
        printf("\n\n\n***ERROR: Both operands of bitwise binary operators"
               " must be integer types.\n");
      }
      return;
    } 
  }

  // SIMPLE ASSIGNMENT
  if (operation == BINOP_ASSIGN) {
    if (!(l_op->lvalue_possible)) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Left operand of asssignment "
               "expression must be an lval.\n");   
      }
      return; 
    }
    if (!(l_op->modifiable)) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Left operand of asssignment "
               "expression must be modifiable.\n");   
      }
      return; 
    }

    usualAssignmentConversions(r_op_modify, r_op, l_op->type_tree);
    binary_operation->type_tree = l_op->type_tree;
    return;
  }

  // EQUALITY OPERATORS
  if (operation == BINOP_EQUALITY || operation == BINOP_INEQUALITY) {
    if (isPointerType(l_op->type_tree)) {
      if (isPointerType(r_op->type_tree)) {
        if (!(typesIdentical(l_op->type_tree, r_op->type_tree))) {
          if (print_type_errors) {
            printf("\n\n\n***ERROR: Pointer operands to '==' or '!=' "
                   "operator are not compatible.\n");           
          }
          return;
        }
        return;
      }
      if (r_op->kind == NODE_NUMBER && (r_op->data.number.value == 0)) {
        insertCastNodeCompound(r_op_modify, r_op, l_op->type_tree);
        return;
      }
    }
    if (isPointerType(r_op->type_tree)) {
      if (l_op->kind == NODE_NUMBER && (l_op->data.number.value == 0)) {
        insertCastNodeCompound(l_op_modify, l_op, r_op->type_tree);
        return;
      }
    }
  }

  // RELATIONAL OPERATORS
  if (operation == BINOP_LESS || operation == BINOP_LESS_EQUAL ||
      operation == BINOP_GREATER || operation == BINOP_GREATER_EQUAL) {
    if (isPointerType(l_op->type_tree)) {
      if (isPointerType(r_op->type_tree)) {
        if (typesIdentical(l_op->type_tree, r_op->type_tree)) { 
          return;         
        } else {
          if (print_type_errors) {
            printf("\n\n\n***ERROR: Pointer operands to relational "
                   "operator are not compatible.\n");  
          }
          return;
        }
      }
    if (print_type_errors) {
      printf("\n\n\n***ERROR: If left operand in relational expression "
             "is a pointer, right operand must be pointer with compatible type.\n");    
    }
    return;
    }
    if (isPointerType(r_op->type_tree)) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: If right operand in relational expression "
               "is a pointer, left operand must be pointer with compatible type.\n");     
      }   
      return;   
    }
  }

  //SHIFT OPERATORS
  if (operation == BINOP_LEFT_SHIFT || operation == BINOP_RIGHT_SHIFT) {
    if (!isBasicType(r_op->type_tree) || !isBasicType(l_op->type_tree)) {
      if (print_type_errors) {
        printf("\n\n\n***ERROR: Both operands of shift operator "
               "must be integer types.\n");
      }
      return;
    }
  }

  //For compound assignment operators
  struct type_tree *operand_type_tree;
  char op_type_string[TYPE_MAX + 1];
  char res_type_string[TYPE_MAX + 1];

  if (operation == BINOP_ASSIGN_ADDITION ||
      operation == BINOP_ASSIGN_SUBTRACTION ||
      operation == BINOP_ASSIGN_MULTIPLICATION ||
      operation == BINOP_ASSIGN_DIVISION ||
      operation == BINOP_ASSIGN_MODDIV ||
      operation == BINOP_ASSIGN_LSHIFT ||
      operation == BINOP_ASSIGN_RSHIFT ||
      operation == BINOP_ASSIGN_BITWISE_AND ||
      operation == BINOP_ASSIGN_BITWISE_XOR ||
      operation == BINOP_ASSIGN_BITWISE_OR) {
    operand_type_tree = simulateBinop(l_op, r_op);
    type_print_string(op_type_string, operand_type_tree);
    strcpy(binary_operation->data.binary_operation.op_type_spec, op_type_string);
    type_print_string(res_type_string, l_op->type_tree);
    strcpy(binary_operation->data.binary_operation.res_type_spec, res_type_string);
    return;
  }

  if (operation == BINOP_COMMA) {
    binary_operation->type_tree = l_op->type_tree;
    return;
  }

  /*
    Proceed with usual binary conversions for two arithmetic operands.
  */

  l_op_rank = typeRank(l_op->type_tree);
  l_op_unsigned = isUnsigned(l_op->type_tree);

  r_op_rank = typeRank(r_op->type_tree);
  r_op_unsigned = isUnsigned(r_op->type_tree);

  if (typesIdentical(l_op->type_tree, r_op->type_tree)) {
    // Types identical; tag binop node with result type and return
    binary_operation->type_tree = l_op->type_tree;
    return; 
  }

  // Both operands are signed, or both operands are unsigned
  if ((l_op_unsigned && r_op_unsigned) || (!(l_op_unsigned) && !(r_op_unsigned))) {
    if (l_op_rank > r_op_rank) {
      cast_type = baseTypeFromTree(l_op->type_tree);
      insertCastNodeBasic(r_op_modify, r_op, cast_type);

      // Tag binop node with result type
      binary_operation->type_tree = l_op->type_tree;
    } else {
      cast_type = baseTypeFromTree(r_op->type_tree);
      insertCastNodeBasic(l_op_modify, l_op, cast_type);

      // Tag binop node with result type
      binary_operation->type_tree = r_op->type_tree;
    }
  }

  // One operand is signed, the other is unsigned. The type of the signed operand has a lower
  // or equal rank when compared to the type of the unsigned operand.
  if ((l_op_unsigned && !r_op_unsigned) && (r_op_rank <= l_op_rank)) {
      cast_type = baseTypeFromTree(l_op->type_tree);
      insertCastNodeBasic(r_op_modify, r_op, cast_type);

      // Tag binop node with result type
      binary_operation->type_tree = l_op->type_tree;
  }
  if ((r_op_unsigned && !l_op_unsigned) && (l_op_rank <= r_op_rank)) {
      cast_type = baseTypeFromTree(r_op->type_tree);
      insertCastNodeBasic(l_op_modify, l_op, cast_type);

      // Tag binop node with result type
      binary_operation->type_tree = r_op->type_tree;
  }

  // One operand is signed, the other is unsigned. The type of the signed operand has a higher
  // rank than the type of the unsigned operand.
  if ((l_op_unsigned && !r_op_unsigned) && (r_op_rank >= l_op_rank)) {
    if (canRepresentAllUnsigned(baseTypeFromTree(r_op->type_tree), 
        baseTypeFromTree(l_op->type_tree))) {
      cast_type = baseTypeFromTree(r_op->type_tree);
      insertCastNodeBasic(l_op_modify, l_op, cast_type);

      // Tag binop node with result type
      binary_operation->type_tree = r_op->type_tree;
    } else {
      enum type_specifier t = unsignedVersion(baseTypeFromTree(r_op->type_tree));
      // Cast BOTH nodes to the unsigned version of the signed type
      cast_type = t;
      insertCastNodeBasic(l_op_modify, l_op, cast_type);
      insertCastNodeBasic(r_op_modify, r_op, cast_type);

      // Tag binop node with result type. Requires creation of new type tree.
      enum type_basic_kind basic_kind = basicKindFromTypeSpecifier(t);
      binary_operation->type_tree = type_tree_add(NULL, type_basic(true, basic_kind));
    }
  }
  if ((r_op_unsigned && !l_op_unsigned) && (l_op_rank >= r_op_rank)) {
    if (canRepresentAllUnsigned(baseTypeFromTree(l_op->type_tree), 
        baseTypeFromTree(r_op->type_tree))) {
      cast_type = baseTypeFromTree(l_op->type_tree);
      insertCastNodeBasic(r_op_modify, r_op, cast_type);

      // Tag binop node with result type
      binary_operation->type_tree = l_op->type_tree;
    } else {
      enum type_specifier t = unsignedVersion(baseTypeFromTree(l_op->type_tree));
      // Cast BOTH nodes to the unsigned version of the signed type
      cast_type = t;
      insertCastNodeBasic(l_op_modify, l_op, cast_type);
      insertCastNodeBasic(r_op_modify, r_op, cast_type);

      // Tag binop node with result type. Requires creation of new type tree.
      enum type_basic_kind basic_kind = basicKindFromTypeSpecifier(t);
      binary_operation->type_tree = type_tree_add(NULL, type_basic(true, basic_kind));
    }
  }
  return;
}
/*************************
 * END BINARY OPERATIONS *
 *************************/

void check_types_conditional_expr(struct node *conditional_expr) {
  assert(NODE_CONDITIONAL_EXPR == conditional_expr->kind);
  struct node *c_expr = conditional_expr->data.conditional_expr.conditional_expr;
  c_expr->check_types(c_expr);

  // Mark node with appropriate type
  conditional_expr->type_tree = c_expr->type_tree;
  conditional_expr->lvalue_possible = false;
  return;
}



void check_types_cast_operation(struct node *cast_op) {

  assert(NODE_CAST_OPERATION == cast_op->kind);

  struct node *operand = cast_op->data.cast_operation.operand;
  operand->check_types(operand);
  // Generate appropriate type tree
  struct type_tree *ntt = malloc(sizeof(struct type_tree));
  struct node *type = cast_op->data.cast_operation.type_name;
  type->generate_symbols(NULL, &ntt, type);
  cast_op->type_tree = ntt;
  cast_op->lvalue_possible = false;
  return;
}

void check_types_expression_list(struct node *expression_list) {
  struct node *init = expression_list->data.expression_list.init;
  if (init) init->check_types(init);

  struct node *arg = expression_list->data.expression_list.assignment_expr;
  struct node **arg_to_modify = &(expression_list->data.expression_list.assignment_expr);
  arg->check_types(arg);

  char expected_type[TYPE_MAX + 1] = "";
  char received_type[TYPE_MAX + 1] = "";
  if (!(*current_param)) {
    if (print_type_errors) {
      printf("\n\n\n***ERROR: Too many parameters provided in function call.\n");
    }
    return;
  }
  
  usualAssignmentConversions(arg_to_modify, arg, (*current_param));
  arg = expression_list->data.expression_list.assignment_expr;
  
  if (!typesIdentical((*current_param), arg->type_tree)) {
    type_print_string(expected_type, *current_param);
    type_print_string(received_type, arg->type_tree);
    if (print_type_errors) {
      printf("\n\n\n***ERROR: Incompatible parameter type in function call.\n"
             "   Expected %s, received %s.***\n", expected_type, received_type);
    }
    return;
  }
  ++current_param;
}

/*********************************************************
 * NODES THAT CANNOT APPEAR BELOW AN expr IN PARSE TREE: *
 *           NO TYPE CHECKING REQUIRED                   *
 *********************************************************/

void check_types_translation_unit_list(struct node *translation_unit_list) {
  assert(NODE_TRANSLATION_UNIT_LIST == translation_unit_list->kind);

  if (NULL != translation_unit_list->data.translation_unit_list.init) {
    check_types_translation_unit_list(translation_unit_list->data.translation_unit_list.init);
  }
  if (NULL != translation_unit_list->data.translation_unit_list.translation_unit) {
    check_types_top_level_decl(translation_unit_list->data.translation_unit_list.translation_unit);
  }
}

void check_types_top_level_decl(struct node *top_level_decl) {
  assert(NODE_DECL == top_level_decl->kind || NODE_FN_DEF == top_level_decl->kind);

  if (top_level_decl->kind == NODE_FN_DEF) {
   top_level_decl->check_types(top_level_decl);
  }
}

void check_types_fn_def(struct node *fn_def) {
  assert(NODE_FN_DEF == fn_def->kind);

  struct node *fd = fn_def->data.function_definition.compound_statement;
  if (fd)  {
    fd->check_types(fd);
  }
}

void check_types_decl_or_statement_list(struct node *decl_or_statement_list) {
  assert(NODE_DECL_OR_STATEMENT_LIST == decl_or_statement_list->kind);

  if (NULL != decl_or_statement_list->data.decl_or_statement_list.init) {
    struct node *ds_list = decl_or_statement_list->data.decl_or_statement_list.init;
    ds_list->check_types(ds_list);
  }

  struct node *ds = decl_or_statement_list->data.decl_or_statement_list.decl_or_statement;
  if (ds) {
    ds->check_types(ds);
  }
}

void check_types_decl(struct node *decl) {
  assert(NODE_DECL == decl->kind);
  return;
}

void check_types_statement(struct node *statement) {
  assert(NODE_STATEMENT == statement->kind);

  struct node *expr = statement->data.statement.expr;
  struct node *stmt = statement->data.statement.stmt;
  struct node *stmt2 = statement->data.statement.stmt2;
  
  if (expr) expr->check_types(expr);
  if (stmt) stmt->check_types(stmt);
  if (stmt2) stmt2->check_types(stmt2);

  return;
}

void check_types_ternary_operation(struct node *tern_op) {
  assert(NODE_TERNARY_OPERATION == tern_op->kind);

  struct node *first_op = tern_op->data.ternary_operation.first_op;
  struct node *second_op = tern_op->data.ternary_operation.second_op;
  struct node *third_op = tern_op->data.ternary_operation.third_op;
  
  if (first_op) first_op->check_types(first_op);
  if (second_op) second_op->check_types(second_op);
  if (third_op) third_op->check_types(third_op);

  return;
}

void check_types_for_expr(struct node *for_expr) {
  assert(NODE_FOR_EXPR == for_expr->kind);

  struct node *init = for_expr->data.for_expr.init;
  struct node *condition = for_expr->data.for_expr.condition;
  struct node *update = for_expr->data.for_expr.update;
  
  if (init) init->check_types(init);
  if (condition) condition->check_types(condition);
  if (update) update->check_types(update);

  return;
}

/*****************************************************
 * Helper functions used in later phases of compiler *
 *****************************************************/

int arraySizeBytes(struct type_tree *ttree) {
  if (isArrayType(ttree)) {
    return ttree->type->data.array.dim * arraySizeBytes(ttree->child);
  }
  if (isPointerType(ttree)) {
    return 4;
  }
  if (isBasicType(ttree)) {
    if (ttree->type->data.basic.datatype == TYPE_BASIC_CHAR) {
      return 1;
    } else if (ttree->type->data.basic.datatype == TYPE_BASIC_SHORT) {
      return 2;
    } else if (ttree->type->data.basic.datatype == TYPE_BASIC_INT) {
      return 4;
    } else if (ttree->type->data.basic.datatype == TYPE_BASIC_LONG) {
      return 4;
    }
  }
  return 0;
}

int pointerSizeOf(struct type_tree *ttree) {
  if (isArrayType(ttree)) {
    return ttree->type->data.array.dim *  pointerSizeOf(ttree->child);
  }
  if (isPointerType(ttree)) {
    return 4;
  }
  if (isBasicType(ttree)) {
    return basicSizeOf(ttree);
  }
  return 0;
}

int basicSizeOf(struct type_tree *ttree) {
  assert(isBasicType(ttree));
  if (ttree->type->data.basic.datatype == TYPE_BASIC_CHAR) {
    return 1;
  } else if (ttree->type->data.basic.datatype == TYPE_BASIC_SHORT) {
    return 2;
  } else if (ttree->type->data.basic.datatype == TYPE_BASIC_INT) {
    return 4;
  } else if (ttree->type->data.basic.datatype == TYPE_BASIC_LONG) {
    return 4;
  }
  return 0;
}
