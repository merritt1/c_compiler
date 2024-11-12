#include <ctype.h>
#include "interpret_char.h"

int to_ascii(char *input) {
	if (input[1] != 92) {  //not an octal or character escape sequence
		return input[1];
	} else if (input[1] == 92) {
		switch(input[2])
		{
		case 'a':
			return '\a';
		case 'n':
			return '\n';
		case 't':
			return '\t';
		case 'b':
			return '\b';
		case 'r':
			return '\r';
		case 'f':
			return '\f';
		case 'v':
			return '\v';
		case '\\':
			return '\\';
		case '\?':
			return '\?';
		case '\'':
			return '\'';
		case '\"':
			return '\"';
		}

		//handle escaped octal sequence
		int octal_val = input[2] - '0';
		if (isdigit(input[3])) {
			octal_val *= 8;
			octal_val += input[3] - '0';

			if (isdigit(input[4])) {
				octal_val *= 8;
				octal_val += input[4] - '0';

				if (octal_val > 255) return -1;
				/* handle sign extension if necessary;
				   see Harbison & Steele 5e Section 2.7.3 
				if (octal_val > 127) {
					octal_val = -128 + (octal_val - 128);
				}
				*/
			}
		}
		return octal_val;
	}
	return -1;
}

char *itoa_wrap(int ascii) {
	char* buffer = malloc(5);
	sprintf(buffer, "%d", ascii);
	return buffer;
}
