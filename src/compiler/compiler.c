#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>

#include "compiler.h"
#include "parser.h"
#include "scanner.h"
#include "node.h"
#include "symbol.h"
#include "type.h"
#include "ir.h"
#include "mips.h"
#include "type_check.h"
#include "optimize.h"

extern int errno;
extern struct string_literal_table string_literal_table;
extern struct ir_section *global_ir;
bool print_type_errors;
bool optimize_mips;
bool annotate;

void compiler_print_error(YYLTYPE location, const char *format, ...) {
  va_list ap;
  fprintf(stdout, "Error (%d, %d) to (%d, %d): ",
          location.first_line, location.first_column,
          location.last_line, location.last_column);
  va_start(ap, format);
  vfprintf(stdout, format, ap);
  va_end(ap);
  fputc('\n', stdout);
}

static void print_errors_from_pass(FILE *output, char *pass, int error_count) {
  fprintf(output, "%s encountered %d %s.\n",
          pass, error_count, (error_count == 1 ? "error" : "errors"));
}

/**
 * Launches the compiler.
 * 
 * The following describes the arguments to the program:
 * compiler [-s (scanner|parser)] [-o outputfile] [inputfile|stdin] [-O (1|2|3)]
 *
 * -s : the name of the stage to stop after. Defaults to
 *      runs all implemented stages.
 * -o : the name of the output file. Defaults to "output.s"  
 * 
 * -O: Optimization level selected. Only works with "ir" and "mips" stages.    
 *
 * You should pass the name of the file to process or redirect stdin.
 */

int main(int argc, char **argv) {
  FILE *output  = stdout;
  print_type_errors = false;
  char *stage, output_name[NAME_MAX + 1];
  int opt;
  yyscan_t scanner;
  struct node *parse_tree;
  struct symbol_table *symbol_table = create_table(NULL);
  int error_count;
  annotate = false;
  optimize_mips = false;
  int optimization_level = 0;

  strncpy(output_name, "output.s", NAME_MAX + 1);
  stage = "mips";                                    // last stage implemented thus far
  while (-1 != (opt = getopt(argc, argv, "o:s:O:a"))) {
    switch (opt) {
      case 'o':
        strncpy(output_name, optarg, NAME_MAX);
          output = fopen(output_name, "w");
            if (NULL == output) {
              fprintf(stdout, "Could not open output file %s: %s", optarg, strerror(errno));
              return 1;
            }
        break;
      case 's':
        stage = optarg;
        break;
      case 'a':
        annotate = true;
        break;
      case 'O':
        optimization_level = atoi(optarg);
        if (optimization_level == 3) { optimize_mips = true; }
        if (optimization_level < 0 || optimization_level >3) {
          fprintf(stdout, "Error -- optimization level must be '0', '1', '2', or '3'\n");
          exit(1);
        }
        break;
    }
  }

  /* Figure out whether we're using stdin/stdout or file in/file out. */
  if (optind >= argc) {
    scanner_initialize(&scanner, stdin);
  } else if (optind == argc - 1) {
    scanner_initialize(&scanner, fopen(argv[optind], "r"));
  } else {
    fprintf(stdout, "Expected 1 input file, found %d.\n", argc - optind);
    return 1;
  }

  if (0 == strcmp("scanner", stage)) {
    error_count = 0;
    scanner_print_tokens(output, &error_count, scanner);
    scanner_destroy(&scanner);
    if (error_count > 0) {
      print_errors_from_pass(output, "Scanner", error_count);
      return 1;
    } else {
      return 0;
    }
  }

  error_count = 0;
  parse_tree = parser_create_tree(&error_count, scanner);
  scanner_destroy(&scanner);
  if (NULL == parse_tree) {
    print_errors_from_pass(output,"Parser", error_count);
    return 1;
  }

  if (0 == strcmp("parser", stage)) {
    node_print_translation_unit_list(output, parse_tree);
    return 0;
  }

  if (error_count > 0) {
    print_errors_from_pass(output, "Symbol table", error_count);
    return 1;
  }
  if (0 == strcmp("symbol", stage)) {
    add_symbols_from_translation_unit_list(parse_tree, symbol_table);
    fprintf(output, "=============== PARSE TREE ===============\n");
    node_print_translation_unit_list(output, parse_tree);
    fprintf(output, "================= SYMBOLS ================\n");
    print_symbol_table(output, symbol_table);
    if (string_literal_table.head) {
      print_string_literal_table(output, &string_literal_table);
    }
    return 0;
  }

  if (0 == strcmp("type", stage)) {
    print_type_errors = true;
    add_symbols_from_translation_unit_list(parse_tree, symbol_table);
    check_types_translation_unit_list(parse_tree);
    fprintf(output, "=============== PARSE TREE ===============\n");
    node_print_translation_unit_list(output, parse_tree);
    fprintf(output, "================= SYMBOLS ================\n");
    print_symbol_table(output, symbol_table);
    if (string_literal_table.head) {
      print_string_literal_table(output, &string_literal_table);
    }
  }

  if (0 == strcmp("ir", stage)) {
    add_symbols_from_translation_unit_list(parse_tree, symbol_table);
    check_types_translation_unit_list(parse_tree);
    ir_generate_for_translation_unit_list(parse_tree);
    if (optimization_level) optimize_program(global_ir, optimization_level);
    if (global_ir) ir_print_section(output, global_ir);
  } 

  if (0 == strcmp("mips", stage)) {
    add_symbols_from_translation_unit_list(parse_tree, symbol_table);
    check_types_translation_unit_list(parse_tree);
    ir_generate_for_translation_unit_list(parse_tree);
    if (optimization_level) optimize_program(global_ir, optimization_level);
    mips_print_program(output, global_ir, symbol_table);
  }

  return 0;
}
