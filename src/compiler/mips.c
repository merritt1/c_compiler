#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "type.h"
#include "symbol.h"
#include "ir.h"
#include "mips.h"
#include "type_check.h"

#define NUM_REGISTERS         32
#define STACK_FRAME_OVERHEAD  96


/****************************
 * MIPS TEXT SECTION OUTPUT *
 ****************************/

// Globals used for allocating MIPS registers and calculating stack frame size
int local_variables_bytes;
int highest_IR_register_seen;
int IR_register_basis;

extern struct string_literal_table string_literal_table;
// Flag set to true when compiler is invoked with "-O 3"
extern bool optimize_mips;

// Calculate double-word alignment for stack frames.
int double_word_align(int local_variables_bytes) {
  int bytes = local_variables_bytes + STACK_FRAME_OVERHEAD;
  bytes += (8 - ((local_variables_bytes + STACK_FRAME_OVERHEAD) % 8));
  return bytes;
}

char *mips_assign_register(int IR_reg) {
  int mips_reg = IR_reg - IR_register_basis;

  if (mips_reg < 0) {
    mips_reg -= (mips_reg * 2);
    mips_reg = mips_reg % 18;
  }

  if (highest_IR_register_seen < IR_reg) {
    highest_IR_register_seen = IR_reg;
  }

  switch (mips_reg) {
  case 0:
    return "$s0";
  case 1:
    return "$s1";
  case 2:
    return "$s2";
  case 3:
    return "$s3";
  case 4:
    return "$s4";
  case 5:
    return "$s5";
  case 6:
    return "$s6";
  case 7:
    return "$s7";
  case 8:
    return "$t0";
  case 9:
    return "$t1";
  case 10:
    return "$t2";
  case 11:
    return "$t3";
  case 12:
    return "$t4";
  case 13:
    return "$t5";
  case 14:
    return "$t6";
  case 15:
    return "$t7";
  case 16:
    return "$t8";
  case 17:
    return "$t9";
  default:
    return mips_assign_register(IR_reg % 18);
  }
}

// Takes an IR instruction and prints the corresponding MIPS instruction(s)
void mips_print_instruction(FILE *output,
                            struct ir_instruction *instruction,
                            struct symbol_table *symbol_table) {
  switch (instruction->kind) {
  case IR_addressOf:
    if (instruction->operands[1].kind == OPERAND_STRING_LABEL) {
      // Load address of string literal
      fprintf(output, "\tla %s, %s\n",
              mips_assign_register(instruction->operands[0].data.temporary),
              instruction->operands[1].data.string_label);
    } else if (instruction->operands[1].data.symbol->stack_offset) {
      // Load address of local variable
      fprintf(output, "\tla %s, %d($fp)\n",
              mips_assign_register(instruction->operands[0].data.temporary),
              instruction->operands[1].data.symbol->stack_offset);
    } else {
      //Load address of global variable
      fprintf(output, "\tla %s, _Global_%s\n",
              mips_assign_register(instruction->operands[0].data.temporary),
              instruction->operands[1].data.symbol->name);
    }
    break;
  case IR_loadWord:
    fprintf(output, "\tlw %s, 0(%s)\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary));
    break;
  case IR_loadHalfWord:
    fprintf(output, "\tlhu %s, 0(%s)\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary));
    break;
  case IR_loadSignedHalfWord:
    fprintf(output, "\tlh %s, 0(%s)\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary));
    break;
  case IR_loadByte:
    fprintf(output, "\tlbu %s, 0(%s)\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary));
    break;
  case IR_loadSignedByte:
    fprintf(output, "\tlb %s, 0(%s)\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary));
    break;
  case IR_storeWord:
    fprintf(output, "\tsw %s, 0(%s)\n",
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[0].data.temporary));
    break;
  case IR_storeHalfWord:
    fprintf(output, "\tsh %s, 0(%s)\n",
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[0].data.temporary));
    break;
  case IR_storeByte:
    fprintf(output, "\tsb %s, 0(%s)\n", 
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[0].data.temporary));
    break;
  case IR_multSignedWord:
  case IR_multUnsignedWord:
    fprintf(output, "\tmul %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_divSignedWord:
  case IR_divUnsignedWord:
    fprintf(output, "\tdivu %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_remSignedWord:
    fprintf(output, "\trem %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_remUnsignedWord:
    fprintf(output, "\tremu %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_addSignedWord:
  case IR_addUnsignedWord:
    fprintf(output, "\taddu %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_subSignedWord:
  case IR_subUnsignedWord:
    fprintf(output, "\tsubu %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_leftShiftWord:
    fprintf(output, "\tsllv %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_rightShiftSignedWord:
    fprintf(output, "\tsrav %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_rightShiftUnsignedWord:
    fprintf(output, "\tsrlv %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_ltSignedWord:
    fprintf(output, "\tslt %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_ltUnsignedWord:
    fprintf(output, "\tsltu %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_leSignedWord:
    fprintf(output, "\tsle %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_leUnsignedWord:
    fprintf(output, "\tsleu %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_geSignedWord:
    fprintf(output, "\tsge %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_geUnsignedWord:
    fprintf(output, "\tsgeu %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_gtSignedWord:
    fprintf(output, "\tsgt %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_gtUnsignedWord:
    fprintf(output, "\tsgtu %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_eqWord:
    fprintf(output, "\tseq %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_neWord:
    fprintf(output, "\tsne %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_bitwiseAndWord:
    fprintf(output, "\tand %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_bitwiseXorWord:
    fprintf(output, "\txor %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_bitwiseOrWord:
    fprintf(output, "\tor %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[2].data.temporary));
    break;
  case IR_unaryMinus:
    fprintf(output, "\tsubu %s, $0, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary));
    break;
  case IR_unaryLogicalNegation:
    fprintf(output, "\tnor %s, %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary));  
    break;
  case IR_unaryBitwiseNegation:
    fprintf(output, "\tnot %s, %s\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary));
    break;
  case IR_constInt:
    fprintf(output, "\tli %s, %d\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            (int)instruction->operands[1].data.number);
    break;

  // Narrowing Casts
  case IR_castWordToHalfWord:
  case IR_castWordToByte:
  case IR_castHalfWordToByte:
    fprintf(output, "\tor %s, %s, $0\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            mips_assign_register(instruction->operands[1].data.temporary));
    break;

  // Widening Casts
  case IR_castUnsignedHalfWordToWord:
      fprintf(output, "\tandi %s, %s, 0xffff\n",
              mips_assign_register(instruction->operands[0].data.temporary),
              mips_assign_register(instruction->operands[1].data.temporary));
    break;
  case IR_castUnsignedByteToHalfWord:
  case IR_castUnsignedByteToWord:
      fprintf(output, "\tandi %s, %s, 0xff\n",
              mips_assign_register(instruction->operands[0].data.temporary),
              mips_assign_register(instruction->operands[1].data.temporary));
    break;
  case IR_castSignedHalfWordToWord:
      // Clear 16 high-order bits
      fprintf(output, "\tsll %s, %s, 16\n",
              mips_assign_register(instruction->operands[0].data.temporary),
              mips_assign_register(instruction->operands[1].data.temporary));
      fprintf(output, "\tsra %s, %s, 16\n",
              mips_assign_register(instruction->operands[0].data.temporary),
              mips_assign_register(instruction->operands[0].data.temporary));
    break;
  case IR_castSignedByteToHalfWord:
  case IR_castSignedByteToWord:
      // Clear 24 high-order bits
      fprintf(output, "\tsll %s, %s, 24\n",
              mips_assign_register(instruction->operands[0].data.temporary),
              mips_assign_register(instruction->operands[1].data.temporary));
      fprintf(output, "\tsra %s, %s, 24\n",
              mips_assign_register(instruction->operands[0].data.temporary),
              mips_assign_register(instruction->operands[0].data.temporary));
    break;

  // Produre/function definitions
  case IR_procBegin:
    IR_register_basis = highest_IR_register_seen;
    if (0 == strcmp(instruction->operands[0].data.symbol->name, "main")) {
      fprintf(output, "main:\n");
    } else {
      fprintf(output, "_Global_%s:\n", instruction->operands[0].data.symbol->name);
    }

    int table_id = instruction->operands[0].data.symbol->fn_symbol_table_id;
    assert(table_id);
    struct symbol_table *s = get_symbol_table_by_id(symbol_table, table_id);
    local_variables_bytes = symbol_table_bytes(s, true);
    update_stack_offsets(s, STACK_FRAME_OVERHEAD);


    // Create new stack frame
    fprintf(output, "\taddiu $sp, $sp, -%d\n", double_word_align(local_variables_bytes)); 
          // ^^^ push space for stack frame onto stack
    fprintf(output, "\tsw $fp, 48($sp)\n"); //save old frame pointer
    fprintf(output, "\tor $fp, $sp, $0\n"); //fp->stack frame
    fprintf(output, "\tsw $ra, 52($fp)\n"); //save return address

    // Save actual parameters
    fprintf(output, "\tsw $a0, 0($fp)\n"); 
    fprintf(output, "\tsw $a1, 4($fp)\n"); 
    fprintf(output, "\tsw $a2, 8($fp)\n"); 
    fprintf(output, "\tsw $a3, 12($fp)\n"); 

    // Save contents of s registers so they can be restored before return
    fprintf(output, "\tsw $s0, 16($fp)\n");
    fprintf(output, "\tsw $s1, 20($fp)\n"); 
    fprintf(output, "\tsw $s2, 24($fp)\n"); 
    fprintf(output, "\tsw $s3, 28($fp)\n"); 
    fprintf(output, "\tsw $s4, 32($fp)\n"); 
    fprintf(output, "\tsw $s5, 36($fp)\n");
    fprintf(output, "\tsw $s6, 40($fp)\n");
    fprintf(output, "\tsw $s7, 44($fp)\n");

    // Move actual parameters to local variable region of stack
    int param_reg = 0;
    struct symbol_list *sl = s->head;

    while (sl && param_reg <= 3) { 
      int size = pointerSizeOf(sl->symbol.type_tree);
      if (size == 4) {
        fprintf(output, "\tsw $a%d, %d($fp)\n", param_reg, sl->symbol.stack_offset);
      } else if (size == 2) {
        fprintf(output, "\tsh $a%d, %d($fp)\n", param_reg, sl->symbol.stack_offset);
      } else if (size == 1) {
        fprintf(output, "\tsb $a%d, %d($fp)\n", param_reg, sl->symbol.stack_offset);
      }
      ++param_reg;
      sl = sl->next;
    }
    break;
  case IR_procEnd:
    if (0 == strcmp(instruction->operands[0].data.symbol->name, "main")) {
      fprintf(output, "\tli $v0,10\n\tsyscall\n"); //exit program
    } else {
      // Restore s registers
      fprintf(output, "\tlw $s7, 44($fp)\n");
      fprintf(output, "\tlw $s6, 40($fp)\n"); 
      fprintf(output, "\tlw $s5, 36($fp)\n"); 
      fprintf(output, "\tlw $s4, 32($fp)\n"); 
      fprintf(output, "\tlw $s3, 28($fp)\n"); 
      fprintf(output, "\tlw $s2, 24($fp)\n");
      fprintf(output, "\tlw $s1, 20($fp)\n");
      fprintf(output, "\tlw $s0, 16($fp)\n");

      // Restore old return address and frame pointer
      fprintf(output, "\tlw $ra, 52($fp)\n");
      fprintf(output, "\tlw $fp, 48($fp)\n");

      // Pops stack frame and return to caller
      fprintf(output, "\taddiu $sp, $sp, %d\n", double_word_align(local_variables_bytes));
      fprintf(output, "\tjr $ra\n");
    }
    break;
  case IR_returnWord:
      fprintf(output, "\tor $v0, %s, $0\n",
              mips_assign_register(instruction->operands[0].data.temporary));
    break;
  case IR_returnHalfWord:
      fprintf(output, "\tor $v0, %s, $0\n",
              mips_assign_register(instruction->operands[0].data.temporary));
    break;
  case IR_returnByte:
      fprintf(output, "\tor $v0, %s, $0\n",
              mips_assign_register(instruction->operands[0].data.temporary));
    break;
  case IR_call:
    //Handle basic syscalls
    if (0 == strcmp(instruction->operands[0].data.symbol->name, "syscall_print_int")) {
      fprintf(output, "\tli $v0,1\n"); //sycall code for print_int
      fprintf(output, "\tsyscall\n"); 
    } else if (0 == strcmp(instruction->operands[0].data.symbol->name, "syscall_print_string")) {
      fprintf(output, "\tli $v0,4\n"); //sycall code for print_string
      fprintf(output, "\tsyscall\n");
    } else if (0 == strcmp(instruction->operands[0].data.symbol->name, "syscall_read_int")) {
      fprintf(output, "\tli $v0,5\n"); //sycall code for read_int
      fprintf(output, "\tsyscall\n"); 
    } else if (0 == strcmp(instruction->operands[0].data.symbol->name, "syscall_read_string")) {
      fprintf(output, "\tli $v0,8\n"); //sycall code for read_string
      fprintf(output, "\tsyscall\n"); 
    } else if (0 == strcmp(instruction->operands[0].data.symbol->name, "syscall_exit")) {
      fprintf(output, "\tli $v0,10\n"); //sycall code for exit
      fprintf(output, "\tsyscall\n"); 
    } else {
      // If optimizations are turned on and no t registers have become live, the contents of the
      // t registers are not saved and restored across the function call boundary.
      if (optimize_mips && ((highest_IR_register_seen - IR_register_basis) < 8)) {
        //Jump and link
        fprintf(output, "\tjal _Global_%s\n", instruction->operands[0].data.symbol->name);
        break;
      }

      // Save contents of t registers so they can be restored before return
      fprintf(output, "\tsw $t0, 56($fp)\n");
      fprintf(output, "\tsw $t1, 60($fp)\n"); 
      fprintf(output, "\tsw $t2, 64($fp)\n"); 
      fprintf(output, "\tsw $t3, 68($fp)\n"); 
      fprintf(output, "\tsw $t4, 72($fp)\n"); 
      fprintf(output, "\tsw $t5, 76($fp)\n");
      fprintf(output, "\tsw $t6, 80($fp)\n");
      fprintf(output, "\tsw $t7, 84($fp)\n");
      fprintf(output, "\tsw $t8, 88($fp)\n");
      fprintf(output, "\tsw $t8, 92($fp)\n");

      // Jump and link
      fprintf(output, "\tjal _Global_%s\n", instruction->operands[0].data.symbol->name);

      // Restore contents of t registers 
      fprintf(output, "\tlw $t9, 92($fp)\n");
      fprintf(output, "\tlw $t8, 88($fp)\n"); 
      fprintf(output, "\tlw $t7, 84($fp)\n"); 
      fprintf(output, "\tlw $t6, 80($fp)\n"); 
      fprintf(output, "\tlw $t5, 76($fp)\n"); 
      fprintf(output, "\tlw $t4, 72($fp)\n");
      fprintf(output, "\tlw $t3, 68($fp)\n");
      fprintf(output, "\tlw $t2, 64($fp)\n");
      fprintf(output, "\tlw $t1, 60($fp)\n");
      fprintf(output, "\tlw $t0, 56($fp)\n");
    }
    break;
  case IR_parameter:
    fprintf(output, "\tor $a%lu, %s, $0\n", instruction->operands[0].data.number,
            mips_assign_register(instruction->operands[1].data.temporary));
    break;
  case IR_resultWord:
      fprintf(output, "\tor %s, $v0, $0\n",
              mips_assign_register(instruction->operands[0].data.temporary));
    break;
  case IR_resultHalfWord:
      fprintf(output, "\tor %s, $v0, $0\n",
              mips_assign_register(instruction->operands[0].data.temporary));
    break;
  case IR_resultByte:
      fprintf(output, "\tor %s, $v0, $0\n",
              mips_assign_register(instruction->operands[0].data.temporary));
    break;
  case IR_label:
    if (instruction->operands[0].kind == OPERAND_USER_LABEL) {
      if (instruction->operands[0].data.label_symbols.lbl_symbol) {
        fprintf(output, "_UserLabel_%s_%s:\n", 
                instruction->operands[0].data.label_symbols.fn_symbol->name, 
                instruction->operands[0].data.label_symbols.lbl_symbol->name);

      } else {
        fprintf(output, "_UserLabel_%s_%s:\n", 
                instruction->operands[0].data.label_symbols.fn_symbol->name, 
                instruction->operands[0].data.label_symbols.lbl_name);
      }
    } else if (instruction->operands[0].kind == OPERAND_GENERATED_LABEL) {
      fprintf(output, "_GeneratedLabel_%d:\n", instruction->operands[0].data.temporary);
    }
    break;
  case IR_goto:
    if (instruction->operands[0].kind == OPERAND_USER_LABEL) {
      if (instruction->operands[0].data.label_symbols.lbl_symbol) {
        fprintf(output, "\tb _UserLabel_%s_%s\n", 
                instruction->operands[0].data.label_symbols.fn_symbol->name, 
                instruction->operands[0].data.label_symbols.lbl_symbol->name);

      } else {
        fprintf(output, "\tb _UserLabel_%s_%s\n",
                instruction->operands[0].data.label_symbols.fn_symbol->name, 
                instruction->operands[0].data.label_symbols.lbl_name);
      }
    } else if (instruction->operands[0].kind == OPERAND_GENERATED_LABEL) {
      fprintf(output, "\tb _GeneratedLabel_%d\n",
                                          instruction->operands[0].data.temporary);
    }
    break;
  case IR_gotoIfFalse:
    fprintf(output, "\tbeqz %s, _GeneratedLabel_%d\n", 
            mips_assign_register(instruction->operands[0].data.temporary),
            instruction->operands[1].data.temporary);
    break;
  case IR_gotoIfTrue:
    fprintf(output, "\tbnez %s, _GeneratedLabel_%d\n",
            mips_assign_register(instruction->operands[0].data.temporary),
            instruction->operands[1].data.temporary);
    break;
  default:
    assert(0);
    break;
  }
}

/*******************************
 *  MIPS CODE PRINT FUNCTIONS  *
 *******************************/

void mips_print_text_section(FILE *output, 
                             struct ir_section *section,
                             struct symbol_table *symbol_table) {
  struct ir_instruction *instruction = section->first;

  fputs("\n\t.text\n\t.globl main\n", output);

  // Print functions from ir
  for (;instruction != section->last->next; instruction = instruction->next) {
    mips_print_instruction(output, instruction, symbol_table);
  }
}

void mips_print_data_section(FILE *output,
                             struct symbol_table *symbol_table) {
  fputs("\t.data\n", output);

  // Add global variables to data section
  struct symbol_list *sl = symbol_table->head;
  while (sl) {
    if (isBasicType(sl->symbol.type_tree)) {
      fprintf(output, "_Global_%s: ", sl->symbol.name);

      switch(sl->symbol.type_tree->type->data.basic.datatype) {
      case TYPE_BASIC_CHAR:
        fprintf(output, ".byte ");
        break;
      case TYPE_BASIC_SHORT:
        fprintf(output, ".half ");
        break;
      case TYPE_BASIC_INT:
      case TYPE_BASIC_LONG:
        fprintf(output, ".word ");
        break;
      }

      // In C all global variables must be initialized to zero.
      fprintf(output, "0\n"); 
    } else if (isArrayType(sl->symbol.type_tree)) {
      fprintf(output, "_Global_%s: .space %d\n", sl->symbol.name,
              arraySizeBytes(sl->symbol.type_tree));
    } else if (isPointerType(sl->symbol.type_tree)) {
      fprintf(output, "_Global_%s: .word 0\n", sl->symbol.name);
    }

    sl = sl->next;
  }

  // Add string literals to data section
  struct string_literal_list *sl2 = string_literal_table.head;
  while (sl2) {
    fprintf(output, "%s: .asciiz %s\n", sl2->str.id, sl2->str.val);
    sl2 = sl2->next;
  }
}

void mips_print_program(FILE *output,
                        struct ir_section *section,
                        struct symbol_table *symbol_table) {
  mips_print_data_section(output, symbol_table);
  mips_print_text_section(output, section, symbol_table);
}
