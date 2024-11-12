#ifndef _COMPILER_H
#define _COMPILER_H

#define IDENTIFIER_MAX 31
#define STRING_MAX 509
#define TYPE_MAX 1024

struct result {
  struct type *type;
  struct ir_operand *ir_operand;
};

typedef struct location {
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
#define YYLTYPE struct location

typedef struct node *YYSTYPE;
#define YYSTYPE struct node *

char const *parser_token_name(int token);

void compiler_print_error(YYLTYPE location, const char *format, ...);

#endif
