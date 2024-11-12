#ifndef _TYPE_CHECK_H
#define _TYPE_CHECK_H

#include <stdio.h>
#include <stdlib.h>

#include "node.h"

enum operand_to_modify {
	CAST_LEFT_OP,
	CAST_RIGHT_OP
};

// Functions that retrieve information about type tree
bool isBasicType(struct type_tree *ttree);
bool isPointerType(struct type_tree *ttree);
bool isArrayType(struct type_tree *ttree);
bool isFunctionType(struct type_tree *ttree);
bool isUnsigned(struct type_tree *input_tree);
int typeRank(struct type_tree *input_tree);

// Traverse AST during type checking: declarations only
void check_types_translation_unit_list(struct node *);
void check_types_top_level_decl(struct node *);
void check_types_fn_def(struct node *);
void check_types_decl_or_statement_list(struct node *);
void check_types_decl(struct node *);
void check_types_statement(struct node *);
void check_types_conditional_expr(struct node *);
void check_types_unary_prefix_operation(struct node *);
void check_types_postfix_expression(struct node *);
void check_types_binary_operation(struct node *);
void check_types_identifier(struct node *);
void check_types_number(struct node *);
void check_types_string(struct node *);
void check_types_cast_operation(struct node *);
void check_types_expression_list(struct node *); 
void check_types_ternary_operation(struct node *);
void check_types_for_expr(struct node *);

// Helper functions used in later phases of compiler
int arraySizeBytes(struct type_tree *ttree);
int pointerSizeOf(struct type_tree *ttree);
int basicSizeOf(struct type_tree *ttree);

#endif /* _TYPE_CHECK_H */
