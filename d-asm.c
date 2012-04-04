/* http://0x10c.com/doc/dcpu-16.txt
 * Program: BasicInstruction+
 * BasicInstruction: Label? (SET|ADD) Arg , Arg
 * Label: ':'name (lowercase)
 * Arg: Register | Value | RegRef | ValRef | Label
 * Register: (A|B|C|X|Y|Z|I|J|PC|SP|O);
 * Value: HexValue | DecimalValue
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <assert.h>

typedef struct {
	unsigned char val; /* 6-bit "spec" value or internal value */
	unsigned int rest; /* additional word */
	char *label; /* label reference */
} arg;

typedef struct b_ins {
	struct b_ins *next;
	arg a;
	arg b;
	unsigned char opcode;
	size_t size; /* calculated during output */
} b_ins; /* basic instruction */

typedef struct lbl {
	struct lbl *next;
	char *name;
	off_t offset;
} lbl;

char *opcode_names[] = {NULL,
	"SET",
	"ADD",
	"SUB",
	"MUL",
	"DIV",
	"MOD",
	"SHL",
	"SHR",
	"AND",
	"BOR",
	"XOR",
	"IFE",
	"IFN",
	"IFG",
	"IFB"};

char *extended_opcode_names[] = {"JSR"};

size_t parse_jsr(off_t, b_ins *);
void write_jsr(b_ins *);
size_t (*extended_opcode_parsers[])(off_t, b_ins *) = {parse_jsr};
void (*extended_opcode_output[])(b_ins *) = {write_jsr};

char *reg2_names[] = {"POP", "PEEK", "PUSH", "SP", "PC", "O"};

char source[12345];
lbl *labels;
b_ins *instructions;

/*************************************************************/
/*            Parsing Code                                   */
/*************************************************************/

size_t whitespace(off_t loc) {
	size_t size = 0;
	while (1) {
		switch (source[loc + size]) {
		case ' ':
		case '\t':
		case '\n':
			size++;
			continue;
		}
		break;
	}
	return size;
}

size_t label(off_t loc) {
	lbl *l = malloc(sizeof(lbl));
	b_ins *j = instructions;
	size_t len = 0;
	memset(l, 0, sizeof(lbl));
	for (; j->next; j = j->next, l->offset++);
	l->offset++;
	for (; islower(source[loc+len]); len++);
	l->name = strndup(source+loc, len);
	if (labels == NULL) {
		labels = l;
	} else {
		lbl *k = labels;
		for (; k->next; k = k->next);
		k->next = l;
	}
	fprintf(stderr, "Label '%s' found at instruction %d\n", l->name, l->offset);
	return len;
}

size_t label_ref(off_t loc, arg *argp) {
	size_t len = 0;
	argp->val = 0xfe; /* interval value, label ref will be resolved during output */
	for (; islower(source[loc+len]); len++);
	argp->label = strndup(source+loc, len);
	return len;
}

unsigned char simple_reg(off_t loc) {
	switch (source[loc]) {
	case 'A':
		return 0;
	case 'B':
		return 1;
	case 'C':
		return 2;
	case 'X':
		return 3;
	case 'Y':
		return 4;
	case 'Z':
		return 5;
	case 'I':
		return 6;
	case 'J':
		return 7;
	}
	return 0xff; /* not recognized */
}

size_t literal(off_t loc, unsigned int *litval) {
	size_t size = 0;

	/* hex */
	if (source[loc] == '0' && source[loc+1] == 'x') {
		size = 2;
		*litval = 0;
		while (1) {
			char x = source[loc+size];
			if (x >= '0' && x <= '9') {
				x -= '0';
			} else if (x >= 'a' && x <= 'f') {
				x = 10 + (x - 'a');
			} else if (x >= 'A' && x <= 'F') {
				x = 10 + (x - 'A');
			} else {
				break; /* 0x just means 0 */
			}
			*litval <<= 4;
			*litval += x;
			size++;
		}
		return size;
	}

	/* decimal */
	while (1) {
		char x = source[loc+size];
		if (x >= '0' && x <= '9') {
			x -= '0';
		} else {
			break;
		}
		*litval *= 10;
		*litval += x;
		size++;
	}
	return size;
}

size_t argument(off_t loc, arg *argp) {
	size_t size = 0;
	int i;

	/* simple register */
	argp->val = simple_reg(loc);
	if (argp->val != 0xff)
		return 1; /* 0x00-0x07: register */

	/* other register or directive (see reg2_names[]) */
	for (i = 0; i < sizeof(reg2_names) / sizeof(char *); ++i) {
		size_t s = strlen(reg2_names[i]);
		if (!strncmp(source+loc, reg2_names[i], s)) {
			argp->val = 0x18 + i;
			return s; /* 0x18: POP , etc.. */
		}
	}

	/* label */
	if (source[loc] >= 'a' && source[loc] <= 'z') {
		return label_ref(loc, argp);
	}

	/* address */
	if (source[loc] == '[') {
		size++;
		size += whitespace(loc);
		argp->val = simple_reg(loc+size);
		if (argp->val != 0xff) {
			size++;
			size += whitespace(loc);
			argp->val += 8;
			if (source[loc+size] != ']') {
				printf("Invalid address syntax at '%.*s'\n", 10, source+loc);
				exit(1);
			}
			size++;
			return size; /* 0x08-0x0f: [register] */
		}

		size += literal(loc+size, &argp->rest);
		size += whitespace(loc);
		if (source[loc+size] == '+') {
			size++;
			size += whitespace(loc+size);
			argp->val = simple_reg(loc+size);
			size++;
			if (argp->val == 0xff) {
				printf("Expected register (A,B,C,X,Y,Z,I,J) after '+' at '%.*s'\n",
						10, source+loc);
				exit(1);
			}
			argp->val += 0x10;
		} else {
			argp->val = 0x1e;
		}
		if (source[loc+size] != ']') {
			printf("Invalid address syntax at '%.*s'\n", 15, source+loc);
			exit(1);
		}
		size++;
		return size; /* 0x10-0x17: [next word + register]
						or 0x1e: [next word] */
	}

	/* literal */
	size = literal(loc, &argp->rest);
	if (argp->rest <= 0x1f)
		argp->val = 0x20 + argp->rest;
	else
		argp->val = 0x1f;
	return size;
}

void push_ins(b_ins *i) {
	b_ins *j;
	if (instructions == NULL) {
		instructions = i;
		return;
	}
	j = instructions;
	for (; j->next; j = j->next);
	j->next = i;
}

size_t parse_jsr(off_t loc, b_ins *i) {
	size_t size = 3;
	size += whitespace(loc+size);
	i->opcode = 0x80 + 0; /* JSR = 0, 0x80 is the internal offset */
	size += argument(loc+size, &i->a);
	push_ins(i);
	return size;
}

size_t instruction(off_t loc) {
	size_t size = 3;
	b_ins *i = malloc(sizeof(b_ins));
	memset(i, 0, sizeof(b_ins));

	/* lookup opcode name */
	i->opcode = 1;
	while (i->opcode < sizeof(opcode_names) / sizeof(char *) &&
			strncmp(opcode_names[i->opcode], source+loc, 3))
		i->opcode++;
	if (i->opcode >= sizeof(opcode_names) / sizeof(char *)) {
		i->opcode = 0;
		/* check to pass off to extended opcodes (assume they're still 3 byte names) */
		while (i->opcode < sizeof(extended_opcode_names) / sizeof(char *) &&
				strncmp(extended_opcode_names[i->opcode], source+loc, 3))
			i->opcode++;
		if (i->opcode < sizeof(extended_opcode_names) / sizeof(char *)) {
			return extended_opcode_parsers[i->opcode](loc, i);
		}
		printf("Invalid opcode: '%.*s'\n", 3, source+loc);
		exit(1);
	}
	/*
	printf("Opcode %s\n", opcode_names[i->opcode]);
	*/

	/* parameters */
	size += whitespace(loc+size);
	size += argument(loc+size, &i->a);
	size += whitespace(loc+size);
	if (source[loc+size] != ',') {
		printf("Invalid instruction format at '%.*s'\n", 10, source+loc);
		exit(1);
	}
	size++;
	size += whitespace(loc+size);
	size += argument(loc+size, &i->b);

	push_ins(i);
	/*
	printf("a = %x\n", i->a.val);
	printf("ar = %x\n", i->a.rest);
	printf("b = %x\n", i->b.val);
	printf("br = %x\n", i->b.rest);
	*/
	return size;
}

/*************************************************************/
/*            Output  Code                                   */
/*************************************************************/

size_t opcode_size(unsigned char opcode) {
	/* basic instruction */
	if (opcode <= sizeof(opcode_names) / sizeof(char *))
		return 1;
	else if (opcode >= 0x80 &&
			opcode < 0x80 + (sizeof(extended_opcode_names) / sizeof(char *)))
		return 1;
	printf("Unknown opcode %x\n", opcode);
	exit(1);
}

/* extra words needed for arguments */
size_t extra_size(arg *argp) {
	if (argp->val >= 0x10 && argp->val <= 0x17)
		return 1;
	if (argp->val == 0x1e || argp->val == 0x1f)
		return 1;
	/* label ref, use extra word for now to be the same as the example code (see [*]) */
	if (argp->val == 0xfe)
		return 1;
	return 0;
}

void calculate_instruction_sizes() {
	b_ins *i = instructions;
	for (; i; i = i->next) {
		/* size in WORDS */
		i->size = opcode_size(i->opcode);
		i->size += extra_size(&i->a);
		i->size += extra_size(&i->b);
		fprintf(stderr, "Size is %d\n", i->size);
	}
}

void calculate_label_offsets() {
	lbl *l = labels;
	b_ins *i;
	off_t tmp;
	for (; l; l = l->next) {
		tmp = 0;
		for (i = instructions;
				l->offset > 0; l->offset--, tmp += i->size, i = i->next);
		l->offset = tmp;
		fprintf(stderr, "Label '%s' offset is %x\n", l->name, l->offset);
	}
}

void resolve_label_ref(arg *argp) {
	lbl *l = labels;
	for (; strcmp(l->name, argp->label) && l->next; l = l->next);
	/* see [*] in example code for why "short form" is not used */
	argp->val = 0x1f;
	argp->rest = l->offset;
}

void resolve_label_refs() {
	b_ins *i = instructions;
	for (; i; i = i->next) {
		if (i->a.val == 0xfe)
			resolve_label_ref(&i->a);
		if (i->b.val == 0xfe)
			resolve_label_ref(&i->b);
	}
}

void write_arg_extra_word(arg *argp) {
	unsigned char out[2];
	if (extra_size(argp) == 0)
		return;
	out[0] = argp->rest >> 8;
	out[1] = argp->rest & 0xff;
	fwrite(out, 1, 2, stdout);
}

void write_basic_instruction(b_ins *i) {
	unsigned int w1; /* word */
	unsigned char out[2];
	w1 = i->opcode + ((i->a.val & 0x3f) << 4) + ((i->b.val & 0x3f) << 10);
	out[0] = (w1 >> 8) & 0xff;
	out[1] = w1 & 0xff;
	fwrite(out, 1, 2, stdout);
	write_arg_extra_word(&i->a);
	write_arg_extra_word(&i->b);
}

void write_jsr(b_ins *i) {
	/* similar to basic with 0000 as the opcode bits */
	unsigned int w1; /* word */
	unsigned char out[2];
	int opcode = 0;
	w1 = ((opcode & 0x3f) << 4) + ((i->a.val & 0x3f) << 10);
	out[0] = (w1 >> 8) & 0xff;
	out[1] = w1 & 0xff;
	fwrite(out, 1, 2, stdout);
	write_arg_extra_word(&i->a);
}

void write_output() {
	b_ins *i = instructions;
	for (; i; i = i->next) {
		/* basic instruction */
		if (i->opcode <= sizeof(opcode_names) / sizeof(char *)) {
			write_basic_instruction(i);
		} else {
			extended_opcode_output[i->opcode - 0x80](i);
		}
	}
}

void write_out() {
	calculate_instruction_sizes();
	calculate_label_offsets();
	resolve_label_refs();
	write_output();
}

int main(int argc, char **argv) {
	/* parse the input code */
	off_t loc = 0;
	fread(source, 1, 12345, stdin);
	while (1) {
		loc += whitespace(loc);
		switch (source[loc]) {
		case ';': /* comment until end of line */
			while (source[++loc] != '\n');
			continue;
		case ':':
			loc++;
			loc += label(loc);
			continue;
		}
		if (source[loc] == 0)
			break;
		loc += instruction(loc);
	}

	/* write the output code */
	write_out();

	return 0;
}

