%verbose
%debug
%defines
%locations
%define api.pure
%lex-param {yyscan_t scanner}
%parse-param {YYSTYPE *root}
%parse-param {int *error_count}
%parse-param {yyscan_t scanner}
%token-table

%{

  #ifndef YY_TYPEDEF_YY_SCANNER_T
  #define YY_TYPEDEF_YY_SCANNER_T
  typedef void* yyscan_t;
  #endif

  #include <stdio.h>

  #include "compiler.h"
  #include "parser.tab.h"
  #include "scanner.yy.h"
  #include "node.h"

  #define DEBUG 2
  #define YYERROR_VERBOSE
  static void yyerror(YYLTYPE *loc, YYSTYPE *root,
                      int *error_count, yyscan_t scanner,
                      char const *s);
%}

%token IDENTIFIER NUMBER STRING

%token BREAK CHAR CONTINUE DO ELSE FOR GOTO IF
%token INT LONG RETURN SHORT SIGNED UNSIGNED VOID WHILE

%token LEFT_PAREN RIGHT_PAREN LEFT_SQUARE RIGHT_SQUARE LEFT_CURLY RIGHT_CURLY

%token AMPERSAND ASTERISK CARET COLON COMMA EQUAL EXCLAMATION GREATER
%token LESS MINUS PERCENT PLUS SEMICOLON SLASH QUESTION TILDE VBAR

%token AMPERSAND_AMPERSAND AMPERSAND_EQUAL ASTERISK_EQUAL CARET_EQUAL
%token EQUAL_EQUAL EXCLAMATION_EQUAL GREATER_EQUAL GREATER_GREATER
%token GREATER_GREATER_EQUAL LESS_EQUAL LESS_LESS LESS_LESS_EQUAL
%token MINUS_EQUAL MINUS_MINUS PERCENT_EQUAL PLUS_EQUAL PLUS_PLUS
%token SLASH_EQUAL VBAR_EQUAL VBAR_VBAR

%start program

%%

simple_declarator
  : identifier
;

    identifier
      : IDENTIFIER
    ;

declarator
  : pointer_declarator

  | direct_declarator
;

    pointer_declarator
      : pointer direct_declarator
        { $$ = node_declarator(yyloc, $2, $1, POINTER_DECLARATOR); }
    ;

        pointer
          : ASTERISK
            { $$ = node_pointer_list(yyloc, NULL); }
          | ASTERISK pointer
            { $$ = node_pointer_list(yyloc, $2); }
        ;

    direct_declarator
      : simple_declarator
          { $$ = node_declarator(yyloc, $1, NULL, SIMPLE_DECLARATOR); }

      | LEFT_PAREN declarator RIGHT_PAREN
          { $$ = $2; }
      | function_declarator

      | array_declarator

    ;

      function_declarator
        : direct_declarator LEFT_PAREN parameter_type_list RIGHT_PAREN
          { $$ = node_function_declarator(yyloc, $1, $3); }
      ;

      parameter_type_list
        : parameter_list
      ;

        parameter_list
          : parameter_decl
            { $$ = node_parameter_list(yyloc, NULL, $1); }
          | parameter_list COMMA parameter_decl
            { $$ = node_parameter_list(yyloc, $1, $3); }
        ;

        parameter_decl
          : declaration_specifiers declarator
            { $$ = node_parameter_decl(yyloc, $1, $2); }
          | declaration_specifiers abstract_declarator
            { $$ = node_parameter_decl(yyloc, $1, $2); }
          | declaration_specifiers
            { $$ = node_parameter_decl(yyloc, $1, NULL); }
        ;

      array_declarator
        : direct_declarator LEFT_SQUARE constant_expr RIGHT_SQUARE
          { $$ = node_declarator(yyloc, $1, $3, ARRAY_DECLARATOR); }
        | direct_declarator LEFT_SQUARE RIGHT_SQUARE
          { $$ = node_declarator(yyloc, $1, NULL, ARRAY_DECLARATOR); }

      ;

declaration_specifiers
  : type_specifier
;

type_specifier
  : integer_type_specifier

  | void_type_specifier
;

    integer_type_specifier
      : signed_type_specifier

      | unsigned_type_specifier

      | character_type_specifier
    ;

        signed_type_specifier

          : SHORT
            { $$ = node_type_specifier(yyloc, SIGNED_SHORT_INT); }

          | SHORT INT
            { $$ = node_type_specifier(yyloc, SIGNED_SHORT_INT); }

          | SIGNED SHORT
            { $$ = node_type_specifier(yyloc, SIGNED_SHORT_INT); }

          | SIGNED SHORT INT
            { $$ = node_type_specifier(yyloc, SIGNED_SHORT_INT); }

          | INT
            { $$ = node_type_specifier(yyloc, SIGNED_INT); }

          | SIGNED INT
            { $$ = node_type_specifier(yyloc, SIGNED_INT); }

          | SIGNED
            { $$ = node_type_specifier(yyloc, SIGNED_INT); }

          | LONG
            { $$ = node_type_specifier(yyloc, SIGNED_LONG_INT); }

          | LONG INT
            { $$ = node_type_specifier(yyloc, SIGNED_LONG_INT); }

          | SIGNED LONG
            { $$ = node_type_specifier(yyloc, SIGNED_LONG_INT); }

          | SIGNED LONG INT
            { $$ = node_type_specifier(yyloc, SIGNED_LONG_INT); }

        ;

        unsigned_type_specifier

          : UNSIGNED SHORT INT
            { $$ = node_type_specifier(yyloc, UNSIGNED_SHORT_INT); }

          | UNSIGNED
            { $$ = node_type_specifier(yyloc, UNSIGNED_INT); }

          | UNSIGNED SHORT
            { $$ = node_type_specifier(yyloc, UNSIGNED_SHORT_INT); }

          | UNSIGNED INT
            { $$ = node_type_specifier(yyloc, UNSIGNED_INT); }

          | UNSIGNED LONG
            { $$ = node_type_specifier(yyloc, UNSIGNED_LONG_INT); }

          | UNSIGNED LONG INT
            { $$ = node_type_specifier(yyloc, UNSIGNED_LONG_INT); }

        ;

        character_type_specifier

          : CHAR
            { $$ = node_type_specifier(yyloc, SIGNED_CHAR); }

          | SIGNED CHAR
            { $$ = node_type_specifier(yyloc, SIGNED_CHAR); }

          | UNSIGNED CHAR
            { $$ = node_type_specifier(yyloc, UNSIGNED_CHAR); }

        ;

    void_type_specifier

      : VOID
        { $$ = node_type_specifier(yyloc, VOID_T); }
    ;

decl
  : declaration_specifiers initialized_declarator_list SEMICOLON
    { $$ = node_decl(yyloc, $1, $2); }

  | error SEMICOLON
    { $$ = NULL; }
;

top_level_decl
  : decl

  | function_definition
;

function_definition
  : function_def_specifier LEFT_CURLY RIGHT_CURLY
    { $$ = node_fn_def(yyloc, $1, NULL); }

  | function_def_specifier LEFT_CURLY declaration_or_statement_list RIGHT_CURLY
      { $$ = node_fn_def(yyloc, $1, $3); }

;

function_def_specifier
  : declaration_specifiers declarator
    { $$ = node_parameter_decl(yyloc, $1, $2); }

;

translation_unit
  : top_level_decl
    { $$ = node_translation_unit_list(yyloc, NULL, $1); }
  | translation_unit top_level_decl
    { $$ = node_translation_unit_list(yyloc, $1, $2); }
;

initialized_declarator_list
  : initialized_declarator
    { $$ = node_initialized_declarator_list(yyloc, NULL, $1); }
  | initialized_declarator_list COMMA initialized_declarator
    { $$ = node_initialized_declarator_list(yyloc, $1, $3); }

;

initialized_declarator
  : declarator
;

assignment_expr
  : conditional_expr

  | unary_expr EQUAL assignment_expr
    { $$ = node_binary_operation(yyloc, BINOP_ASSIGN, $1, $3); }

  | unary_expr PLUS_EQUAL assignment_expr
    { $$ = node_binary_operation(yyloc, BINOP_ASSIGN_ADDITION, $1, $3); }

  | unary_expr MINUS_EQUAL assignment_expr
    { $$ = node_binary_operation(yyloc, BINOP_ASSIGN_SUBTRACTION, $1, $3); }

  | unary_expr ASTERISK_EQUAL assignment_expr
    { $$ = node_binary_operation(yyloc, BINOP_ASSIGN_MULTIPLICATION, $1, $3); }

  | unary_expr SLASH_EQUAL assignment_expr
    { $$ = node_binary_operation(yyloc, BINOP_ASSIGN_DIVISION, $1, $3); }

  | unary_expr PERCENT_EQUAL assignment_expr
    { $$ = node_binary_operation(yyloc, BINOP_ASSIGN_MODDIV, $1, $3); }

  | unary_expr LESS_LESS_EQUAL assignment_expr
    { $$ = node_binary_operation(yyloc, BINOP_ASSIGN_LSHIFT, $1, $3); }

  | unary_expr GREATER_GREATER_EQUAL assignment_expr
    { $$ = node_binary_operation(yyloc, BINOP_ASSIGN_RSHIFT, $1, $3); }

  | unary_expr AMPERSAND_EQUAL assignment_expr
    { $$ = node_binary_operation(yyloc, BINOP_ASSIGN_BITWISE_AND, $1, $3); }

  | unary_expr CARET_EQUAL assignment_expr
    { $$ = node_binary_operation(yyloc, BINOP_ASSIGN_BITWISE_XOR, $1, $3); }

  | unary_expr VBAR_EQUAL assignment_expr
    { $$ = node_binary_operation(yyloc, BINOP_ASSIGN_BITWISE_OR, $1, $3); }


;


conditional_expr
  : logical_or_expr
    { $$ = node_conditional_expr(yyloc, $1); }

  | logical_or_expr QUESTION expr COLON conditional_expr
    { struct node *tern_op = node_ternary_operation(yyloc, $1, $3, $5); 
      $$ = node_conditional_expr(yyloc, tern_op); }
;

expr
  : comma_expr
;

comma_expr
  : assignment_expr

  | comma_expr COMMA assignment_expr
    { $$ = node_binary_operation(yyloc, BINOP_COMMA, $1, $3); }
;

logical_or_expr
  : logical_and_expr

  | logical_or_expr VBAR_VBAR logical_and_expr
    { $$ = node_binary_operation(yyloc, BINOP_LOGICAL_OR, $1, $3); }
;

logical_and_expr
  : bitwise_or_expr

  |logical_and_expr AMPERSAND_AMPERSAND bitwise_or_expr
    { $$ = node_binary_operation(yyloc, BINOP_LOGICAL_AND, $1, $3); }
;

bitwise_or_expr
  : bitwise_xor_expr

  | bitwise_or_expr VBAR bitwise_xor_expr
    { $$ = node_binary_operation(yyloc, BINOP_BITWISE_OR, $1, $3); }
;

bitwise_xor_expr
  : bitwise_and_expr

  | bitwise_xor_expr CARET bitwise_and_expr
    { $$ = node_binary_operation(yyloc, BINOP_BITWISE_XOR, $1, $3); }
;

bitwise_and_expr
  : equality_expr

  | bitwise_and_expr AMPERSAND equality_expr
    { $$ = node_binary_operation(yyloc, BINOP_BITWISE_AND, $1, $3); }
;

equality_expr
  : relational_expr

  | equality_expr EQUAL_EQUAL relational_expr
    { $$ = node_binary_operation(yyloc, BINOP_EQUALITY, $1, $3); }

  | equality_expr EXCLAMATION_EQUAL relational_expr
    { $$ = node_binary_operation(yyloc, BINOP_INEQUALITY, $1, $3); }

;

relational_expr
  : shift_expr

  | relational_expr LESS shift_expr
    { $$ = node_binary_operation(yyloc, BINOP_LESS, $1, $3); }

  | relational_expr LESS_EQUAL shift_expr
    { $$ = node_binary_operation(yyloc, BINOP_LESS_EQUAL, $1, $3); }

  | relational_expr GREATER shift_expr
    { $$ = node_binary_operation(yyloc, BINOP_GREATER, $1, $3); }

  | relational_expr GREATER_EQUAL shift_expr
    { $$ = node_binary_operation(yyloc, BINOP_GREATER_EQUAL, $1, $3); }

;


shift_expr
  : additive_expr

  | shift_expr LESS_LESS additive_expr
    { $$ = node_binary_operation(yyloc, BINOP_LEFT_SHIFT, $1, $3); }

  | shift_expr GREATER_GREATER additive_expr
    { $$ = node_binary_operation(yyloc, BINOP_RIGHT_SHIFT, $1, $3); }

;

additive_expr
  : multiplicative_expr

  | additive_expr PLUS multiplicative_expr
    { $$ = node_binary_operation(yyloc, BINOP_ADDITION, $1, $3); }

  | additive_expr MINUS multiplicative_expr
    { $$ = node_binary_operation(yyloc, BINOP_SUBTRACTION, $1, $3); }
;

multiplicative_expr
  : cast_expr

  | multiplicative_expr ASTERISK cast_expr
    { $$ = node_binary_operation(yyloc, BINOP_MULTIPLICATION, $1, $3); }

  | multiplicative_expr SLASH cast_expr
    { $$ = node_binary_operation(yyloc, BINOP_DIVISION, $1, $3); }

  | multiplicative_expr PERCENT cast_expr
    { $$ = node_binary_operation(yyloc, BINOP_MODDIV, $1, $3); }
;

cast_expr
  : unary_expr

  | LEFT_PAREN type_name RIGHT_PAREN cast_expr
    { $$ = node_cast_operation(yyloc, $2, $4); }
;

unary_expr
  : postfix_expr

  | MINUS cast_expr
    { $$ = node_unary_prefix_operation(yyloc, UNARY_MINUS, $2); }

  | PLUS cast_expr
    { $$ = node_unary_prefix_operation(yyloc, UNARY_PLUS, $2); }

  | EXCLAMATION cast_expr
    { $$ = node_unary_prefix_operation(yyloc, UNARY_LOGICAL_NEGATION, $2); }

  | TILDE cast_expr
    { $$ = node_unary_prefix_operation(yyloc, UNARY_BITWISE_NEGATION, $2); }

  | AMPERSAND cast_expr
    { $$ = node_unary_prefix_operation(yyloc, UNARY_ADDRESS_EXPR, $2); }

  | ASTERISK cast_expr
    { $$ = node_unary_prefix_operation(yyloc, UNARY_INDIRECTION_EXPR, $2); }

  | PLUS_PLUS cast_expr
    { $$ = node_unary_prefix_operation(yyloc, UNARY_PREINCREMENT, $2); }

  | MINUS_MINUS cast_expr
      { $$ = node_unary_prefix_operation(yyloc, UNARY_PREDECREMENT, $2); }

;

    postfix_expr
      : primary_expr

      | subscript_expr

      | function_call

      | postfix_expr PLUS_PLUS
        { $$ = node_postfix_expr(yyloc, POSTINCREMENT, $1, NULL); }

      | postfix_expr MINUS_MINUS
        { $$ = node_postfix_expr(yyloc, POSTDECREMENT, $1, NULL); }

    ;

        primary_expr
          : IDENTIFIER

          | constant

          | parenthesized_expr
        ;

            constant
              : NUMBER

              | STRING
            ;

            parenthesized_expr
              : LEFT_PAREN expr RIGHT_PAREN
                { $$ = $2; }
            ;

        subscript_expr
          : postfix_expr LEFT_SQUARE expr RIGHT_SQUARE
            { struct node *add = node_binary_operation(yyloc, BINOP_ADDITION, $1, $3); 
              $$ = node_unary_prefix_operation(yyloc, UNARY_INDIRECTION_EXPR, add); }
        ;

        function_call
          : postfix_expr LEFT_PAREN expression_list RIGHT_PAREN
            { $$ = node_postfix_expr(yyloc, POSTFIX_FN_CALL, $1, $3); }

          | postfix_expr LEFT_PAREN RIGHT_PAREN
            { $$ = node_postfix_expr(yyloc, POSTFIX_FN_CALL, $1, NULL); }
        ;

          expression_list
            : assignment_expr
              { $$ = node_expression_list(yyloc, NULL, $1); }

            | expression_list COMMA assignment_expr
              { $$ = node_expression_list(yyloc, $1, $3); }
          ;

type_name 
  : declaration_specifiers

  | declaration_specifiers abstract_declarator
    { $$ = node_abstract_declarator(yyloc, $1, $2); }
;

abstract_declarator
  : pointer

  | direct_abstract_declarator

  | pointer direct_abstract_declarator
    { $$ = node_abstract_declarator(yyloc, $1, $2); }
;

direct_abstract_declarator
  : LEFT_PAREN abstract_declarator RIGHT_PAREN
    { $$ = $2; }

  | LEFT_SQUARE constant_expr RIGHT_SQUARE
    { $$ = node_direct_abstract_declarator_list(yyloc, NULL, $2); }

  | LEFT_SQUARE RIGHT_SQUARE
    { $$ = node_direct_abstract_declarator_list(yyloc, NULL, NULL);}

  | direct_abstract_declarator LEFT_SQUARE constant_expr RIGHT_SQUARE
    { $$ = node_direct_abstract_declarator_list(yyloc, $1, $3); }
;

constant_expr
  : conditional_expr
;

program
  : translation_unit
    { *root = $1; }
;

statement
  : expr SEMICOLON
    { $$ = node_statement(yyloc, STATEMENT_EXPRESSION, $1, NULL, NULL); }

  | IDENTIFIER COLON statement
    { $$ = node_statement(yyloc, STATEMENT_LABELED, $1, $3, NULL); }

  | LEFT_CURLY RIGHT_CURLY
    { $$ = node_statement(yyloc, STATEMENT_COMPOUND, NULL, NULL, NULL); }

  | LEFT_CURLY error RIGHT_CURLY
    { $$ = node_statement(yyloc, STATEMENT_COMPOUND, NULL, NULL, NULL); }

  | LEFT_CURLY declaration_or_statement_list RIGHT_CURLY
    { $$ = node_statement(yyloc, STATEMENT_COMPOUND, $2, NULL, NULL); }

  | IF LEFT_PAREN expr RIGHT_PAREN statement
    { $$ = node_statement(yyloc, STATEMENT_CONDITIONAL, $3, $5, NULL); }

  | IF LEFT_PAREN expr RIGHT_PAREN statement ELSE statement
    { $$ = node_statement(yyloc, STATEMENT_CONDITIONAL, $3, $5, $7); }

  | WHILE LEFT_PAREN expr RIGHT_PAREN statement
    { $$ = node_statement(yyloc, STATEMENT_ITERATIVE_WHILE, $3, $5, NULL); }

  | DO statement WHILE LEFT_PAREN expr RIGHT_PAREN SEMICOLON
    { $$ = node_statement(yyloc, STATEMENT_ITERATIVE_DO, $5, $2, NULL); }

  | FOR for_expr statement
    { $$ = node_statement(yyloc, STATEMENT_ITERATIVE_FOR, $2, $3, NULL); }

  | BREAK SEMICOLON
    { $$ = node_statement(yyloc, STATEMENT_BREAK, NULL, NULL, NULL); }

  | CONTINUE SEMICOLON
    { $$ = node_statement(yyloc, STATEMENT_CONTINUE, NULL, NULL, NULL); }

  | RETURN expr SEMICOLON
    { $$ = node_statement(yyloc, STATEMENT_RETURN, $2, NULL, NULL); }

  | RETURN SEMICOLON
    { $$ = node_statement(yyloc, STATEMENT_RETURN, NULL, NULL, NULL); }

  | GOTO IDENTIFIER SEMICOLON
    { $$ = node_statement(yyloc, STATEMENT_GOTO, $2, NULL, NULL); }

  | SEMICOLON
    { $$ = node_statement(yyloc, STATEMENT_NULL, NULL, NULL, NULL); }
;

for_expr
  : LEFT_PAREN expr SEMICOLON expr SEMICOLON expr RIGHT_PAREN
    { $$ = node_for_expr(yyloc, $2, $4, $6); }

  | LEFT_PAREN SEMICOLON expr SEMICOLON expr RIGHT_PAREN
    { $$ = node_for_expr(yyloc, NULL, $3, $5); }

  | LEFT_PAREN expr SEMICOLON SEMICOLON expr RIGHT_PAREN
    { $$ = node_for_expr(yyloc, $2, NULL, $5); }

  | LEFT_PAREN expr SEMICOLON expr SEMICOLON RIGHT_PAREN
    { $$ = node_for_expr(yyloc, $2, $4, NULL); }

  | LEFT_PAREN SEMICOLON SEMICOLON expr RIGHT_PAREN
    { $$ = node_for_expr(yyloc, NULL, NULL, $4); }

  | LEFT_PAREN expr SEMICOLON SEMICOLON RIGHT_PAREN
    { $$ = node_for_expr(yyloc, $2, NULL, NULL); }

  | LEFT_PAREN SEMICOLON expr SEMICOLON RIGHT_PAREN
    { $$ = node_for_expr(yyloc, NULL, $3, NULL); }

  | LEFT_PAREN SEMICOLON SEMICOLON RIGHT_PAREN
    { $$ = node_for_expr(yyloc, NULL, NULL, NULL); }

;

declaration_or_statement_list
  : declaration_or_statement
    { $$ = node_decl_or_statement_list(yyloc, NULL, $1); }

  | declaration_or_statement_list declaration_or_statement
    { $$ = node_decl_or_statement_list(yyloc, $1, $2); }
;

declaration_or_statement
  : decl

  | statement
;


%%

static void yyerror(YYLTYPE *loc,
                    YYSTYPE *root __attribute__((unused)),
                    int *error_count,
                    yyscan_t scanner __attribute__((unused)),
                    char const *s)
{
  compiler_print_error(*loc, s);
  (*error_count)++;
}

struct node *parser_create_tree(int *error_count, yyscan_t scanner) {
  struct node *parse_tree;
  int result = yyparse(&parse_tree, error_count, scanner);
  if (result == 1) {
    return NULL;
  } else if (result == 2) {
    fprintf(stdout, "Parser ran out of memory.\n");
    return NULL;
  } else {
    return parse_tree;
  }
}

char const *parser_token_name(int token) {
  return yytname[token - 255];
}

