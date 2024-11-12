%option yylineno
%option reentrant
%option bison-bridge
%option bison-locations
%option nounput
%option noyyalloc
%option noyyrealloc
%option noyyfree
%{
/*
 * scanner.lex
 *
 * This file contains the specification for the Flex generated scanner
 * for the CSCI E-95 sample language.
 *
 */

  #include <stdlib.h>
  #include <errno.h>
  #include <string.h>

  /* Suppress compiler warnings about unused variables and functions. */
  #define YY_EXIT_FAILURE ((void)yyscanner, 2)
  #define YY_NO_INPUT

  /* Track locations. */
  #define YY_EXTRA_TYPE int
  #define YY_USER_ACTION { yylloc->first_line = yylloc->last_line = yylineno; \
                           yylloc->first_column = yyextra; \
                           yylloc->last_column = yyextra + yyleng - 1; \
                           yyextra += yyleng; }

  #include "compiler.h"
  #include "parser.tab.h"
  #include "node.h"
  #include "type.h"
  #include "interpret_char.h"
%}

%x comment

newline         \n
ws              [ \t\v\f]

digit           [[:digit:]]
nonzero_digit   [123456789]

letter          [[:alpha:]_]
char_escape     (a|n|t|b|r|f|v|\\|\'|\"|\?)
octal_escape    ([0-3][0-7]{2}|[0-7]{1,2})

id              {letter}({letter}|{digit})*
number          (0|{nonzero_digit}{digit}*)
string          \"(\\({char_escape}|[01234567])|[^"\n\\]){0,509}\"
str_overflow    \"(\\({char_escape}|[01234567])|[^"\n\\]){510,}\"
char            \'(\\{char_escape}|\\{octal_escape}|.)\'

%%

<INITIAL>{
"/*"              BEGIN(comment);
}
<comment>{
"*/"      BEGIN(INITIAL);
[^*\n]+   // eat comment in chunks
"*"       // eat the lone star
\n        yyextra = 1;
}

{newline}   yyextra = 1;
{ws}        /* do nothing */

  /* reserved keywords begin */
break       return BREAK;
char        return CHAR;
continue    return CONTINUE;
do          return DO;
else        return ELSE;
for         return FOR;
goto        return GOTO;
if          return IF;
int         return INT;
long        return LONG;
return      return RETURN;
short       return SHORT;
signed      return SIGNED;
unsigned    return UNSIGNED;
void        return VOID;
while       return WHILE;
  /* reserved keywords end */

  /* simple operators begin */
\!          return EXCLAMATION;
\%          return PERCENT;
\^          return CARET;
\&          return AMPERSAND;
\*          return ASTERISK;
-           return MINUS;
\+          return PLUS;
\=          return EQUAL;
~           return TILDE;
\|          return VBAR;
\<          return LESS;
>           return GREATER;
\/          return SLASH;
\?          return QUESTION;
  /* simple operators end */

  /* commpound assignment operators begin */
\+=         return PLUS_EQUAL;
-=          return MINUS_EQUAL;
\*=         return ASTERISK_EQUAL;
\/=         return SLASH_EQUAL;
%=          return PERCENT_EQUAL;

\<<=         return LESS_LESS_EQUAL;
>>=         return GREATER_GREATER_EQUAL;
\&=          return AMPERSAND_EQUAL;
\^=          return CARET_EQUAL;
\|=          return VBAR_EQUAL;
  /* compound assignment operators end */

  /* other compound operators begin */
\++         return PLUS_PLUS;
\--         return MINUS_MINUS;
\<<         return LESS_LESS;
>>          return GREATER_GREATER;

\<=         return LESS_EQUAL;
>=          return GREATER_EQUAL;
\==         return EQUAL_EQUAL;
\!=         return EXCLAMATION_EQUAL;
\&&         return AMPERSAND_AMPERSAND;
\|\|         return VBAR_VBAR;

  /* other compound operators end */

  /* separators begin */
\(          return LEFT_PAREN;
\)          return RIGHT_PAREN;
;           return SEMICOLON;
\{          return LEFT_CURLY;
\}          return RIGHT_CURLY;
\[          return LEFT_SQUARE;
\]          return RIGHT_SQUARE;
:           return COLON;
,           return COMMA;
  /* separators end */

  /* constants begin */
{number}          *yylval = node_number(*yylloc, yytext); return NUMBER;
{char}            *yylval = node_number(*yylloc, itoa_wrap(to_ascii(yytext))); return NUMBER;
{string}          *yylval = node_string(*yylloc, yytext, yyleng); return STRING;
{str_overflow}    return -1;

  /* constants end */

  /* identifiers */
{id}        *yylval = node_identifier(*yylloc, yytext, yyleng); return IDENTIFIER;
.           return -1;

%%

void scanner_initialize(yyscan_t *scanner, FILE *input) {
  yylex_init(scanner);
  yyset_in(input, *scanner);
  yyset_extra(1, *scanner);
}

void scanner_destroy(yyscan_t *scanner) {
  yylex_destroy(*scanner);
  scanner = NULL;
}

void scanner_print_tokens(FILE *output, int *error_count, yyscan_t scanner) {
  YYSTYPE val;
  YYLTYPE loc;
  int token;

  token = yylex(&val, &loc, scanner);
  while (0 != token) {
    /*
     * Print the line number. Use printf formatting and tabs to keep columns
     * lined up.
     */
    fprintf(output, "loc = %04d:%04d-%04d:%04d",
            loc.first_line, loc.first_column, loc.last_line, loc.last_column);

    /*
     * Print the scanned text. Try to use formatting but give up instead of
     * truncating if the text is too long.
     */
    if (yyget_leng(scanner) <= 20) {
      fprintf(output, "     text = %-20s", yyget_text(scanner));
    } else {
      fprintf(output, "     text = %s", yyget_text(scanner));
    }

    if (token <= 0) {
      fputs("     token = ERROR", output);
      (*error_count)++;
    } else {
      fprintf(output, "     token = [%-20s]", parser_token_name(token));

      switch (token) {
        case NUMBER:
          /* Print the type and value. */
          fputs("     type = ", output);
          type_print(output, val->data.number.result.type);
          fprintf(output, "     value = [%-10lu]", val->data.number.value);
          if (val->data.number.overflow) {
            fputs("     OVERFLOW", output);
            (*error_count)++;
          }
          break;

        case IDENTIFIER:
          fprintf(output, "     name = %s", val->data.identifier.name);
          break;

        case STRING:
          fprintf(output, "     val = %s", val->data.string.val);
          break;
      }
    }
    fputs("\n", output);
    token = yylex(&val, &loc, scanner);
  }
}

/* Suppress compiler warnings about unused parameters. */
void *yyalloc (yy_size_t size, yyscan_t yyscanner __attribute__((unused)))
{
  return (void *)malloc(size);
}

/* Suppress compiler warnings about unused parameters. */
void *yyrealloc  (void *ptr, yy_size_t size, yyscan_t yyscanner __attribute__((unused)))
{
  return (void *)realloc((char *)ptr, size );
}

/* Suppress compiler warnings about unused parameters. */
void yyfree (void *ptr, yyscan_t yyscanner __attribute__((unused)))
{
  free((char *)ptr); /* see yyrealloc() for (char *) cast */
}
