#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>

typedef int8_t i8;
typedef uint8_t u8;
typedef int16_t i16;
typedef uint16_t u16;
typedef int32_t i32;
typedef uint32_t u32;

#define ARRLEN(X) ((i32)(sizeof(X) / sizeof(*X)))

#define COG_LONGS  512
#define COG_REGS    16
#define COG_MAX       (COG_LONGS - COG_REGS)

typedef struct
{
	char *Name;
	i32 Type;
	u32 Val;
} Keyword;

enum
{
	T_NULL,
	T_LABEL,
	T_EFFECT,
	T_COND,
	T_DIRECTIVE,
	T_INSTR2,
	T_INSTRK,
	T_INSTRH,
	T_INSTRC,
	T_INSTRN,
	T_INSTRJ,
	T_INSTRR,
	T_NUMBER,
	T_COMMA,
	T_LPAREN,
	T_RPAREN,
	T_ADD,
	T_SUB,
	T_MUL,
	T_DIV,
	T_MOD,
	T_SHL,
	T_SHR,
	T_INV,
	T_AND,
	T_OR,
	T_XOR,
	T_HASH,
	T_AT,
	T_EQ,
	T_U_MINUS
};

enum
{
	TC_OPERAND,
	TC_UNARY,
	TC_BINARY,
	TC_COUNT
};

typedef struct
{
	i32 Type;
	char *Ptr;
	i32 Len;
	i32 Val;
} Token;

static Token t_null = { .Type = T_NULL, .Ptr = NULL, .Len = 0, .Val = 0 };

#define IMM_BIT    22
#define COND_SHIFT 18
#define IF_ALWAYS  0b1111

typedef struct
{
	char *Ptr;
	i32 Len;
	i32 Val;
	i32 Offset;
} Label;

enum
{
	E_WR,
	E_NR,
	E_WC,
	E_WZ
};

enum
{
	D_ORG,
	D_FIT,
	D_RES,
	D_LONG,
	D_WORD,
	D_BYTE
};

static Keyword keywords[] =
{
	/* Effects */
	{ .Name = "WR",       .Type = T_EFFECT,    .Val = E_WR },
	{ .Name = "NR",       .Type = T_EFFECT,    .Val = E_NR },
	{ .Name = "WC",       .Type = T_EFFECT,    .Val = E_WC },
	{ .Name = "WZ",       .Type = T_EFFECT,    .Val = E_WZ },

	/* Directives */
	{ .Name = "ORG",      .Type = T_DIRECTIVE, .Val = D_ORG  },
	{ .Name = "FIT",      .Type = T_DIRECTIVE, .Val = D_FIT  },
	{ .Name = "RES",      .Type = T_DIRECTIVE, .Val = D_RES  },
	{ .Name = "LONG",     .Type = T_DIRECTIVE, .Val = D_LONG },
	{ .Name = "WORD",     .Type = T_DIRECTIVE, .Val = D_WORD },
	{ .Name = "BYTE",     .Type = T_DIRECTIVE, .Val = D_BYTE },

	/* Instructions */
	{ .Name = "WRBYTE",  .Type = T_INSTR2, .Val = 0b00000000001111000000000000000000 },
	{ .Name = "RDBYTE",  .Type = T_INSTR2, .Val = 0b00000000101111000000000000000000 },
	{ .Name = "WRWORD",  .Type = T_INSTR2, .Val = 0b00000100001111000000000000000000 },
	{ .Name = "RDWORD",  .Type = T_INSTR2, .Val = 0b00000100101111000000000000000000 },
	{ .Name = "WRLONG",  .Type = T_INSTR2, .Val = 0b00001000001111000000000000000000 },
	{ .Name = "RDLONG",  .Type = T_INSTR2, .Val = 0b00001000101111000000000000000000 },
	{ .Name = "HUBOP",   .Type = T_INSTR2, .Val = 0b00001100001111000000000000000000 },
	{ .Name = "CLKSET",  .Type = T_INSTRH, .Val = 0b00001100011111000000000000000000 },
	{ .Name = "COGID",   .Type = T_INSTRH, .Val = 0b00001100111111000000000000000001 },
	{ .Name = "COGINIT", .Type = T_INSTRH, .Val = 0b00001100011111000000000000000010 },
	{ .Name = "COGSTOP", .Type = T_INSTRH, .Val = 0b00001100011111000000000000000011 },
	{ .Name = "LOCKNEW", .Type = T_INSTRH, .Val = 0b00001100111111000000000000000100 },
	{ .Name = "LOCKRET", .Type = T_INSTRH, .Val = 0b00001100011111000000000000000101 },
	{ .Name = "LOCKSET", .Type = T_INSTRH, .Val = 0b00001100011111000000000000000110 },
	{ .Name = "LOCKCLR", .Type = T_INSTRH, .Val = 0b00001100011111000000000000000111 },
	{ .Name = "ROR",     .Type = T_INSTR2, .Val = 0b00100000101111000000000000000000 },
	{ .Name = "ROL",     .Type = T_INSTR2, .Val = 0b00100100101111000000000000000000 },
	{ .Name = "SHR",     .Type = T_INSTR2, .Val = 0b00101000101111000000000000000000 },
	{ .Name = "SHL",     .Type = T_INSTR2, .Val = 0b00101100101111000000000000000000 },
	{ .Name = "RCR",     .Type = T_INSTR2, .Val = 0b00110000101111000000000000000000 },
	{ .Name = "RCL",     .Type = T_INSTR2, .Val = 0b00110100101111000000000000000000 },
	{ .Name = "SAR",     .Type = T_INSTR2, .Val = 0b00111000101111000000000000000000 },
	{ .Name = "REV",     .Type = T_INSTR2, .Val = 0b00111100101111000000000000000000 },
	{ .Name = "MINS",    .Type = T_INSTR2, .Val = 0b01000000101111000000000000000000 },
	{ .Name = "MAXS",    .Type = T_INSTR2, .Val = 0b01000100101111000000000000000000 },
	{ .Name = "MIN",     .Type = T_INSTR2, .Val = 0b01001000101111000000000000000000 },
	{ .Name = "MAX",     .Type = T_INSTR2, .Val = 0b01001100101111000000000000000000 },
	{ .Name = "MOVS",    .Type = T_INSTR2, .Val = 0b01010000101111000000000000000000 },
	{ .Name = "MOVD",    .Type = T_INSTR2, .Val = 0b01010100101111000000000000000000 },
	{ .Name = "MOVI",    .Type = T_INSTR2, .Val = 0b01011000101111000000000000000000 },
	{ .Name = "JMPRET",  .Type = T_INSTR2, .Val = 0b01011100101111000000000000000000 },
	{ .Name = "JMP",     .Type = T_INSTRJ, .Val = 0b01011100001111000000000000000000 },
	{ .Name = "CALL",    .Type = T_INSTRC, .Val = 0b01011100111111000000000000000000 },
	{ .Name = "RET",     .Type = T_INSTRR, .Val = 0b01011100011111000000000000000000 },
	{ .Name = "TEST",    .Type = T_INSTR2, .Val = 0b01100000001111000000000000000000 },
	{ .Name = "TESTN",   .Type = T_INSTR2, .Val = 0b01100100001111000000000000000000 },
	{ .Name = "AND",     .Type = T_INSTR2, .Val = 0b01100000101111000000000000000000 },
	{ .Name = "ANDN",    .Type = T_INSTR2, .Val = 0b01100100101111000000000000000000 },
	{ .Name = "OR",      .Type = T_INSTR2, .Val = 0b01101000101111000000000000000000 },
	{ .Name = "XOR",     .Type = T_INSTR2, .Val = 0b01101100101111000000000000000000 },
	{ .Name = "MUXC",    .Type = T_INSTR2, .Val = 0b01110000101111000000000000000000 },
	{ .Name = "MUXNC",   .Type = T_INSTR2, .Val = 0b01110100101111000000000000000000 },
	{ .Name = "MUXZ",    .Type = T_INSTR2, .Val = 0b01111000101111000000000000000000 },
	{ .Name = "MUXNZ",   .Type = T_INSTR2, .Val = 0b01111100101111000000000000000000 },
	{ .Name = "ADD",     .Type = T_INSTR2, .Val = 0b10000000101111000000000000000000 },
	{ .Name = "SUB",     .Type = T_INSTR2, .Val = 0b10000100101111000000000000000000 },
	{ .Name = "CMP",     .Type = T_INSTR2, .Val = 0b10000100001111000000000000000000 },
	{ .Name = "ADDABS",  .Type = T_INSTR2, .Val = 0b10001000101111000000000000000000 },
	{ .Name = "SUBABS",  .Type = T_INSTR2, .Val = 0b10001100101111000000000000000000 },
	{ .Name = "SUMC",    .Type = T_INSTR2, .Val = 0b10010000101111000000000000000000 },
	{ .Name = "SUMNC",   .Type = T_INSTR2, .Val = 0b10010100101111000000000000000000 },
	{ .Name = "SUMZ",    .Type = T_INSTR2, .Val = 0b10011000101111000000000000000000 },
	{ .Name = "SUMNZ",   .Type = T_INSTR2, .Val = 0b10011100101111000000000000000000 },
	{ .Name = "MOV",     .Type = T_INSTR2, .Val = 0b10100000101111000000000000000000 },
	{ .Name = "NEG",     .Type = T_INSTR2, .Val = 0b10100100101111000000000000000000 },
	{ .Name = "ABS",     .Type = T_INSTR2, .Val = 0b10101000101111000000000000000000 },
	{ .Name = "ABSNEG",  .Type = T_INSTR2, .Val = 0b10101100101111000000000000000000 },
	{ .Name = "NEGC",    .Type = T_INSTR2, .Val = 0b10110000101111000000000000000000 },
	{ .Name = "NEGNC",   .Type = T_INSTR2, .Val = 0b10110100101111000000000000000000 },
	{ .Name = "NEGZ",    .Type = T_INSTR2, .Val = 0b10111000101111000000000000000000 },
	{ .Name = "NEGNZ",   .Type = T_INSTR2, .Val = 0b10111100101111000000000000000000 },
	{ .Name = "CMPS",    .Type = T_INSTR2, .Val = 0b11000000001111000000000000000000 },
	{ .Name = "CMPSX",   .Type = T_INSTR2, .Val = 0b11000100001111000000000000000000 },
	{ .Name = "ADDX",    .Type = T_INSTR2, .Val = 0b11001000101111000000000000000000 },
	{ .Name = "SUBX",    .Type = T_INSTR2, .Val = 0b11001100101111000000000000000000 },
	{ .Name = "CMPX",    .Type = T_INSTR2, .Val = 0b11001100001111000000000000000000 },
	{ .Name = "ADDS",    .Type = T_INSTR2, .Val = 0b11010000101111000000000000000000 },
	{ .Name = "SUBS",    .Type = T_INSTR2, .Val = 0b11010100101111000000000000000000 },
	{ .Name = "ADDSX",   .Type = T_INSTR2, .Val = 0b11011000101111000000000000000000 },
	{ .Name = "SUBSX",   .Type = T_INSTR2, .Val = 0b11011100101111000000000000000000 },
	{ .Name = "CMPSUB",  .Type = T_INSTR2, .Val = 0b11100000101111000000000000000000 },
	{ .Name = "DJNZ",    .Type = T_INSTRK, .Val = 0b11100100101111000000000000000000 },
	{ .Name = "TJNZ",    .Type = T_INSTRK, .Val = 0b11101000001111000000000000000000 },
	{ .Name = "TJZ",     .Type = T_INSTRK, .Val = 0b11101100001111000000000000000000 },
	{ .Name = "WAITPEQ", .Type = T_INSTR2, .Val = 0b11110000001111000000000000000000 },
	{ .Name = "WAITPNE", .Type = T_INSTR2, .Val = 0b11110100001111000000000000000000 },
	{ .Name = "WAITCNT", .Type = T_INSTR2, .Val = 0b11111000101111000000000000000000 },
	{ .Name = "WAITVID", .Type = T_INSTR2, .Val = 0b11111100001111000000000000000000 },
	{ .Name = "NOP",     .Type = T_INSTRN, .Val = 0b10000000110000000000000000000000 },

	/* Conditions */
	{ .Name = "IF_ALWAYS",    .Type = T_COND, .Val = 0b1111 },
	{ .Name = "IF_NEVER",     .Type = T_COND, .Val = 0b0000 },
	{ .Name = "IF_E",         .Type = T_COND, .Val = 0b1010 },
	{ .Name = "IF_NE",        .Type = T_COND, .Val = 0b0101 },
	{ .Name = "IF_A",         .Type = T_COND, .Val = 0b0001 },
	{ .Name = "IF_B",         .Type = T_COND, .Val = 0b1100 },
	{ .Name = "IF_AE",        .Type = T_COND, .Val = 0b0011 },
	{ .Name = "IF_BE",        .Type = T_COND, .Val = 0b1110 },
	{ .Name = "IF_C",         .Type = T_COND, .Val = 0b1100 },
	{ .Name = "IF_NC",        .Type = T_COND, .Val = 0b0011 },
	{ .Name = "IF_Z",         .Type = T_COND, .Val = 0b1010 },
	{ .Name = "IF_NZ",        .Type = T_COND, .Val = 0b0101 },
	{ .Name = "IF_C_EQ_Z",    .Type = T_COND, .Val = 0b1001 },
	{ .Name = "IF_C_NE_Z",    .Type = T_COND, .Val = 0b0110 },
	{ .Name = "IF_C_AND_Z",   .Type = T_COND, .Val = 0b1000 },
	{ .Name = "IF_C_AND_NZ",  .Type = T_COND, .Val = 0b0100 },
	{ .Name = "IF_NC_AND_Z",  .Type = T_COND, .Val = 0b0010 },
	{ .Name = "IF_NC_AND_NZ", .Type = T_COND, .Val = 0b0001 },
	{ .Name = "IF_C_OR_Z",    .Type = T_COND, .Val = 0b1110 },
	{ .Name = "IF_C_OR_NZ",   .Type = T_COND, .Val = 0b1101 },
	{ .Name = "IF_NC_OR_Z",   .Type = T_COND, .Val = 0b1011 },
	{ .Name = "IF_NC_OR_NZ",  .Type = T_COND, .Val = 0b0111 },
	{ .Name = "IF_Z_EQ_C",    .Type = T_COND, .Val = 0b1001 },
	{ .Name = "IF_Z_NE_C",    .Type = T_COND, .Val = 0b0110 },
	{ .Name = "IF_Z_AND_C",   .Type = T_COND, .Val = 0b1000 },
	{ .Name = "IF_Z_AND_NC",  .Type = T_COND, .Val = 0b0010 },
	{ .Name = "IF_NZ_AND_C",  .Type = T_COND, .Val = 0b0100 },
	{ .Name = "IF_NZ_AND_NC", .Type = T_COND, .Val = 0b0001 },
	{ .Name = "IF_Z_OR_C",    .Type = T_COND, .Val = 0b1110 },
	{ .Name = "IF_Z_OR_NC",   .Type = T_COND, .Val = 0b1011 },
	{ .Name = "IF_NZ_OR_C",   .Type = T_COND, .Val = 0b1101 },
	{ .Name = "IF_NZ_OR_NC",  .Type = T_COND, .Val = 0b0111 },
};

#define LABEL_INITIAL 16

static i32 label_cnt = LABEL_INITIAL;
static i32 label_save = LABEL_INITIAL;
static Label labels[512] =
{
	{ .Ptr = "PAR",  .Len = 3, .Val = 0x1F0, .Offset = -1 },
	{ .Ptr = "CNT",  .Len = 3, .Val = 0x1F1, .Offset = -1 },
	{ .Ptr = "INA",  .Len = 3, .Val = 0x1F2, .Offset = -1 },
	{ .Ptr = "INB",  .Len = 3, .Val = 0x1F3, .Offset = -1 },
	{ .Ptr = "OUTA", .Len = 4, .Val = 0x1F4, .Offset = -1 },
	{ .Ptr = "OUTB", .Len = 4, .Val = 0x1F5, .Offset = -1 },
	{ .Ptr = "DIRA", .Len = 4, .Val = 0x1F6, .Offset = -1 },
	{ .Ptr = "DIRB", .Len = 4, .Val = 0x1F7, .Offset = -1 },
	{ .Ptr = "CTRA", .Len = 4, .Val = 0x1F8, .Offset = -1 },
	{ .Ptr = "CTRB", .Len = 4, .Val = 0x1F9, .Offset = -1 },
	{ .Ptr = "FRQA", .Len = 4, .Val = 0x1FA, .Offset = -1 },
	{ .Ptr = "FRQB", .Len = 4, .Val = 0x1FB, .Offset = -1 },
	{ .Ptr = "PHSA", .Len = 4, .Val = 0x1FC, .Offset = -1 },
	{ .Ptr = "PHSB", .Len = 4, .Val = 0x1FD, .Offset = -1 },
	{ .Ptr = "VCFG", .Len = 4, .Val = 0x1FE, .Offset = -1 },
	{ .Ptr = "VSCL", .Len = 4, .Val = 0x1FF, .Offset = -1 }
};

static i32 op_top;
static u8 op_stack[32];
static i32 n_top;
static i32 n_stack[32];

static i32 token_cnt;
static i32 token_idx;
static Token tokens[256];

static i32 byteoffset;
static i32 lnr;
static i32 org;
static char input[32 * 1024];
static char *input_ptr;
static char *input_end;
static i32 input_len;
static u8 output[16 * 1024];
static size_t output_len;
static i32 pass;

static i32 is_digit(i32 c)
{
	return c >= '0' && c <= '9';
}

static i32 is_lower(i32 c)
{
	return c >= 'a' && c <= 'z';
}

static i32 is_upper(i32 c)
{
	return c >= 'A' && c <= 'Z';
}

static i32 is_alpha(i32 c)
{
	return is_upper(c) || is_lower(c);
}

static i32 is_alnum(i32 c)
{
	return is_digit(c) || is_alpha(c);
}

static i32 is_oct(i32 c)
{
	return c >= '0' && c <= '7';
}

static i32 is_hex(i32 c)
{
	return is_digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

static i32 is_bin(i32 c)
{
	return c == '0' || c == '1';
}

static i32 to_upper(i32 c)
{
	if(is_lower(c))
	{
		return c - 'a' + 'A';
	}

	return c;
}

static i32 acc_hex(i32 n, i32 c)
{
	return n * 16 + (c & 15) + (c >= 'A' ? 9 : 0);
}

static i32 acc_dec(i32 n, i32 c)
{
	return n * 10 + (c - '0');
}

static i32 acc_bin(i32 n, i32 c)
{
	return n * 2 + (c - '0');
}

static i32 acc_oct(i32 n, i32 c)
{
	return n * 8 + (c - '0');
}

static i32 is_x(i32 c)
{
	return c == 'X' || c == 'x';
}

static i32 is_b(i32 c)
{
	return c == 'B' || c == 'b';
}

static void report_error(char *msg, ...)
{
	va_list args;
	printf("Line %d: ", lnr);
	va_start(args, msg);
	vprintf(msg, args);
	va_end(args);
	printf("\n");
	exit(1);
}

static u32 instr_set_imm(u32 instr)
{
	return instr | (1 << IMM_BIT);
}

static u32 instr_set_d(u32 instr, u32 d)
{
	if(d >= 512)
	{
		report_error("Constant (0x%08X) does not fit in 9-bit D-field", d);
	}

	instr &= ~(0x1FF << 9);
	return instr | (d << 9);
}

static u32 instr_set_s(u32 instr, u32 s)
{
	if(s >= 512)
	{
		report_error("Constant (0x%08X) does not fit in 9-bit S-field", s);
	}

	instr &= ~0x1FF;
	return instr | s;
}

static u32 instr_set_cond(u32 instr, u32 cond)
{
	instr &= ~(0x0F << COND_SHIFT);
	instr |= (cond << COND_SHIFT);
	return instr;
}

static void org_add(i32 count, i32 size)
{
	org = ((org + size - 1) / size) * size;
	org += count * size;
}

static i32 org_get(void)
{
	if(org % 4 == 0)
	{
		return org / 4;
	}

	return org / 4 + 1;
}

static void token_add(i32 type, char *p, i32 len, i32 value)
{
	if(token_cnt >= ARRLEN(tokens))
	{
		report_error("Maximum number of tokens (%d) exceeded",
			ARRLEN(tokens));
	}

	Token *t = tokens + token_cnt;
	t->Type = type;
	t->Ptr = p;
	t->Len = len;
	t->Val = value;
	++token_cnt;
}

static void t_add(i32 type)
{
	token_add(type, NULL, 0, 0);
}

static void output_push_byte(u8 v)
{
	if(output_len >= ARRLEN(output))
	{
		report_error("Maximum output size (%d bytes) exceeded",
			ARRLEN(output));
	}

	output[output_len++] = v;
}

static void output_push_word(u16 v)
{
	while((output_len & 0x1) != 0) { output_push_byte(0); }
	output_push_byte(v & 0xFF);
	output_push_byte((v >> 8) & 0xFF);
}

static void output_push_long(u32 v)
{
	while((output_len & 0x3) != 0) { output_push_byte(0); }
	output_push_byte(v & 0xFF);
	output_push_byte((v >> 8) & 0xFF);
	output_push_byte((v >> 16) & 0xFF);
	output_push_byte((v >> 24) & 0xFF);
}

static u32 instr_build(i32 opcode, i32 zcri, i32 con, i32 dest, i32 src)
{
	return (opcode << 26) |
		(zcri << 22) |
		(con << 18) |
		(dest << 9) |
		src;
}

static void output_push_instr(i32 opcode, i32 zcri, i32 con, i32 dest, i32 src)
{
	output_push_long(instr_build(opcode, zcri, con, dest, src));
}

static i32 is_ident_start(i32 c)
{
	return is_alpha(c) || c == '_' || c == ':';
}

static i32 is_ident(i32 c)
{
	return is_alnum(c) || c == '_';
}

static i32 label_cmp(Label *a, Label *b)
{
	if(a->Len != b->Len)
	{
		return 0;
	}

	for(i32 i = 0; i < a->Len; ++i)
	{
		if(a->Ptr[i] != b->Ptr[i])
		{
			return 0;
		}
	}

	return 1;
}

static Label *label_find(Label *label)
{
	for(i32 i = 0; i < label_cnt; ++i)
	{
		Label *cur = labels + i;
		if(label_cmp(label, cur))
		{
			return cur;
		}
	}

	return NULL;
}

static void label_add(char *ptr, i32 len, i32 val, i32 byteoffset)
{
	if(label_cnt >= ARRLEN(labels))
	{
		report_error("Maximum number of labels (%d) exceeded",
			ARRLEN(labels));
	}

	Label ident = { ptr, len, val, byteoffset };
	Label *dup = label_find(&ident);
	if(dup)
	{
		report_error("Duplicate label: \"%.*s\"", dup->Len, dup->Ptr);
	}

	if(ident.Ptr[0] != ':')
	{
		label_cnt = label_save;
		++label_save;
	}

	labels[label_cnt++] = ident;
}

static void labels_print(void)
{
	printf("Labels:\n");
	for(i32 i = 0; i < label_cnt; ++i)
	{
		Label *label = labels + i;
		printf("%.*s - %d %d\n", label->Len, label->Ptr, label->Val, label->Offset);
	}
}

static void token_print(Token *t)
{
	if(!t)
	{
		printf(" [NULL] ");
		return;
	}

	switch(t->Type)
	{
	case T_NULL:      printf(" [NULL] ");         break;
	case T_LABEL:     printf(" [LABEL] ");        break;
	case T_EFFECT:    printf(" [EFFECT] ");       break;
	case T_COND:      printf(" [COND] ");         break;
	case T_DIRECTIVE: printf(" [DIRECTIVE] ");    break;
	case T_INSTR2:    printf(" [INSTR2] ");       break;
	case T_INSTRK:    printf(" [INSTRK] ");       break;
	case T_INSTRH:    printf(" [INSTRH] ");       break;
	case T_INSTRC:    printf(" [INSTRC] ");       break;
	case T_INSTRN:    printf(" [INSTRN] ");       break;
	case T_INSTRJ:    printf(" [INSTRJ] ");       break;
	case T_INSTRR:    printf(" [INSTRR] ");       break;
	case T_NUMBER:    printf(" [%d] ", t->Val);   break;
	case T_COMMA:     printf(" [,] ");            break;
	case T_LPAREN:    printf(" [(] ");            break;
	case T_RPAREN:    printf(" [)] ");            break;
	case T_ADD:       printf(" [+] ");            break;
	case T_SUB:       printf(" [-] ");            break;
	case T_MUL:       printf(" [*] ");            break;
	case T_DIV:       printf(" [/] ");            break;
	case T_MOD:       printf(" [%%] ");           break;
	case T_SHL:       printf(" [<<] ");           break;
	case T_SHR:       printf(" [>>] ");           break;
	case T_INV:       printf(" [~] ");            break;
	case T_AND:       printf(" [&] ");            break;
	case T_OR:        printf(" [|] ");            break;
	case T_XOR:       printf(" [^] ");            break;
	case T_EQ:        printf(" [=] ");            break;
	case T_HASH:      printf(" [#] ");            break;
	case T_AT:        printf(" [@] ");            break;
	default:          printf(" [UNK] ");          break;
	}
}

static void t_next(void)
{
	if(token_idx < token_cnt)
	{
		++token_idx;
	}
}

static Token *t_get(i32 i)
{
	if((token_idx + i) < token_cnt)
	{
		return tokens + token_idx + i;
	}

	return &t_null;
}

static i32 tt_get(i32 i)
{
	return t_get(i)->Type;
}

static void t_expect(i32 tt)
{
	if(tt_get(0) != tt)
	{
		report_error("Unexpected token");
	}
}

static const char *token_class_tostring(i32 token_class)
{
	switch(token_class)
	{
	case TC_OPERAND: return "TC_OPERAND";
	case TC_BINARY:  return "TC_BINARY";
	case TC_UNARY:   return "TC_UNARY";
	}

	return "";
}

static i32 precedence_get(i32 tt)
{
	switch(tt)
	{
	case T_OR:      return 9;
	case T_XOR:     return 8;
	case T_AND:     return 7;
	case T_SHL:     return 4;
	case T_SHR:     return 4;
	case T_ADD:     return 3;
	case T_SUB:     return 3;
	case T_MUL:     return 2;
	case T_DIV:     return 2;
	case T_MOD:     return 2;
	case T_INV:     return 1;
	case T_U_MINUS: return 1;
	}

	return 0;
}

static void check_prev(i32 prev_class, i32 cur_class)
{
	if((prev_class == TC_OPERAND && cur_class == TC_OPERAND) ||
		(prev_class == TC_OPERAND && cur_class == TC_UNARY) ||
		(prev_class == TC_BINARY && cur_class == TC_BINARY) ||
		(prev_class == TC_UNARY && cur_class == TC_BINARY) ||
		(prev_class == TC_UNARY && cur_class == TC_UNARY))
	{
		report_error("Unexpected token %s after token %s in expression",
			token_class_tostring(cur_class),
			token_class_tostring(prev_class));
	}
}

static i32 token_class(i32 tt)
{
	if(tt == T_NUMBER || tt == T_LABEL || tt == T_AT || tt == T_LPAREN)
	{
		return TC_OPERAND;
	}
	else if(tt == T_U_MINUS || tt == T_INV)
	{
		return TC_UNARY;
	}

	return TC_BINARY;
}

static void n_push(i32 n)
{
	if(n_top >= ARRLEN(n_stack))
	{
		report_error("Operand stack overflow");
	}

	n_stack[n_top++] = n;
}

static i32 n_pop(void)
{
	if(n_top <= 0)
	{
		report_error("Operand stack underflow");
	}

	return n_stack[--n_top];
}

static void expr_evaluate(i32 tt)
{
	i32 tc = token_class(tt);
	i32 v = 0;
	if(tc == TC_BINARY)
	{
		i32 b = n_pop();
		i32 a = n_pop();
		switch(tt)
		{
		case T_ADD: v = a + b;  break;
		case T_SUB: v = a - b;  break;
		case T_MUL: v = a * b;  break;
		case T_DIV: v = a / b;  break;
		case T_MOD: v = a % b;  break;
		case T_OR:  v = a | b;  break;
		case T_AND: v = a & b;  break;
		case T_XOR: v = a ^ b;  break;
		case T_SHL: v = a << b; break;
		case T_SHR: v = a >> b; break;
		}
	}
	else if(tc == TC_UNARY)
	{
		i32 a = n_pop();
		switch(tt)
		{
		case T_INV:     v = ~a; break;
		case T_U_MINUS: v = -a; break;
		}
	}

	n_push(v);
}

static i32 parse_expr(void)
{
	i32 local_top = op_top;
	i32 tt = T_NULL;
	for(;;)
	{
		i32 precedence = 0;
		i32 prev_class = token_class(tt);
		tt = tt_get(0);
		if(tt == T_SUB && prev_class == TC_BINARY)
		{
			tt = T_U_MINUS;
		}

		check_prev(prev_class, token_class(tt));
		if(tt == T_NUMBER)
		{
			n_push(t_get(0)->Val);
		}
		else if(tt == T_LABEL || tt == T_AT)
		{
			Token *cur = t_get(0);
			Label label = { cur->Ptr, cur->Len, 0, 0 };
			Label *found = label_find(&label);
			if(!found)
			{
				report_error("Undefined reference to \"%.*s\"",
					label.Len, label.Ptr);
			}

			if(tt == T_LABEL)
			{
				n_push(found->Val);
			}
			else
			{
				if(found->Offset < 0)
				{
					report_error("Cannot take byte address of \"%.*s\"",
						label.Len, label.Ptr);
				}

				n_push(found->Offset);
			}
		}
		else if(tt == T_LPAREN)
		{
			t_next();
			n_push(parse_expr());
			t_expect(T_RPAREN);
		}
		else if((precedence = precedence_get(tt)) > 0)
		{
			while(op_top > local_top &&
				precedence_get(op_stack[op_top - 1]) <= precedence)
			{
				--op_top;
				expr_evaluate(op_stack[op_top]);
			}

			op_stack[op_top++] = tt;
		}
		else
		{
			break;
		}

		t_next();
	}

	while(op_top > local_top)
	{
		--op_top;
		expr_evaluate(op_stack[op_top]);
	}

	return n_pop();
}

static void token_prints(void)
{
	printf("Line %d: ", lnr);
	for(i32 i = 0; i < token_cnt; ++i)
	{
		token_print(tokens + i);
	}

	printf("\n");
}

static u32 instr_modify_bit(u32 instr, i32 bit, i32 state)
{
	return state ? (instr | (1 << bit)) : (instr & ~(1 << bit));
}

static u32 instr_effects(u32 instr)
{
	i32 mask = 0;
	i32 tt = tt_get(0);
	if(tt == T_NULL)
	{
		return instr;
	}

	for(;;)
	{
		t_expect(T_EFFECT);

		Token *t = t_get(0);
		i32 v = t->Val;
		i32 bit = 0;
		i32 state = 0;

		switch(v)
		{
		case E_NR:
			bit = (1 << 23);
			state = 0;
			break;

		case E_WR:
			bit = (1 << 23);
			state = 1;
			break;

		case E_WC:
			bit = (1 << 24);
			state = 1;
			break;

		case E_WZ:
			bit = (1 << 25);
			state = 1;
			break;
		}

		if(mask & (1 << v))
		{
			report_error("Duplicate effect");
		}

		mask |= (1 << v);
		instr = instr_modify_bit(instr, bit, state);
		t_next();
		if(tt_get(0) == T_NULL)
		{
			return instr;
		}

		t_expect(T_COMMA);
		t_next();
	}
}

static void instr_normal(u32 cond)
{
	u32 instr = t_get(0)->Val;
	instr = instr_set_cond(instr, cond);
	t_next();
	i32 d = parse_expr();
	t_expect(T_COMMA);
	t_next();
	if(tt_get(0) == T_HASH)
	{
		instr = instr_set_imm(instr);
		t_next();
	}

	i32 s = parse_expr();
	instr = instr_effects(instr);
	instr = instr_set_d(instr, d);
	instr = instr_set_s(instr, s);
	output_push_long(instr);
}

static void instr_jmp2(u32 cond)
{
	u32 instr = t_get(0)->Val;
	instr = instr_set_cond(instr, cond);
	t_next();
	i32 d = parse_expr();
	t_expect(T_COMMA);
	t_next();
	if(tt_get(0) == T_HASH)
	{
		instr = instr_set_imm(instr);
		t_next();
	}
	else
	{
		printf("Line %d: note: indirect jump instruction, did you forget the '#'?\n", lnr);
	}

	i32 s = parse_expr();
	instr = instr_effects(instr);
	instr = instr_set_d(instr, d);
	instr = instr_set_s(instr, s);
	output_push_long(instr);
}

static void instr_hub(u32 cond)
{
	u32 instr = t_get(0)->Val;
	instr = instr_set_cond(instr, cond);
	t_next();
	i32 s = parse_expr();
	instr = instr_set_s(instr, s);
	instr = instr_effects(instr);
	output_push_long(instr);
}

static void instr_call(u32 cond)
{
	u32 instr = t_get(0)->Val;
	instr = instr_set_cond(instr, cond);
	t_next();
	t_expect(T_HASH);
	t_next();
	t_expect(T_LABEL);
	Token *t_label = t_get(0);
	t_next();
	instr = instr_effects(instr);

	char buf[256];
	int len = snprintf(buf, sizeof(buf), "%.*s_ret", t_label->Len, t_label->Ptr);

	Label l_call = { t_label->Ptr, t_label->Len, 0, 0 };
	Label l_ret = { buf, len, 0, 0 };

	Label *res_call = label_find(&l_call);
	if(!res_call)
	{
		report_error("Undefined reference to `%.*s`\n", l_call.Len, l_call.Ptr);
	}

	instr = instr_set_s(instr, res_call->Val);

	Label *res_ret = label_find(&l_ret);
	if(!res_ret)
	{
		report_error("Undefined reference to `%.*s`\n", l_ret.Len, l_ret.Ptr);
	}

	instr = instr_set_d(instr, res_ret->Val);
	output_push_long(instr);
}

static void instr_zero(u32 cond)
{
	u32 instr = t_get(0)->Val;
	instr = instr_set_cond(instr, cond);
	t_next();
	instr = instr_effects(instr);
	output_push_long(instr);
}

static void instr_jmp(u32 cond)
{
	u32 instr = t_get(0)->Val;
	instr = instr_set_cond(instr, cond);
	t_next();
	if(tt_get(0) == T_HASH)
	{
		instr = instr_set_imm(instr);
		t_next();
	}
	else
	{
		printf("Line %d: note: indirect jump instruction, did you forget the '#'?\n", lnr);
	}

	i32 s = parse_expr();
	instr = instr_set_s(instr, s);
	instr = instr_effects(instr);
	output_push_long(instr);
}

static i32 try_parse_instr(u32 cond)
{
	/* (COND) INSTR DEST, SRC (EFFECT) */
	switch(tt_get(0))
	{
	case T_INSTR2:
		instr_normal(cond);
		return 1;

	case T_INSTRK:
		instr_jmp2(cond);
		return 1;

	case T_INSTRC:
		instr_call(cond);
		return 1;

	case T_INSTRH:
		instr_hub(cond);
		return 1;

	case T_INSTRJ:
		instr_jmp(cond);
		return 1;

	case T_INSTRN:
		instr_zero(cond);
		return 1;

	case T_INSTRR:
		instr_zero(cond);
		return 1;
	}

	return 0;
}

static void handle_org(void)
{
	t_next();
	t_expect(T_NUMBER);
	org = 4 * t_get(0)->Val;
	t_next();
	t_expect(T_NULL);
}

static void handle_res(void)
{
	t_next();
	t_expect(T_NUMBER);
	org_add(t_get(0)->Val, 4);
}

static void elements_store(i32 size)
{
	t_next();
	if(tt_get(0) == T_NULL)
	{
		return;
	}

	for(;;)
	{
		i32 v = parse_expr();

		if(size == D_BYTE)
		{
			if(v < -128 || v > 255)
			{
				printf("Line %d: warning: byte range overflow (%d)\n", lnr, v);
			}

			output_push_byte(v);
		}
		else if(size == D_WORD)
		{
			if(v < -32768 || v > 65535)
			{
				printf("Line %d: warning: word range overflow (%d)\n", lnr, v);
			}

			output_push_word(v);
		}
		else
		{
			output_push_long(v);
		}

		if(tt_get(0) == T_NULL)
		{
			break;
		}

		t_expect(T_COMMA);
		t_next();
	}
}

static void parse_directive(void)
{
	switch(t_get(0)->Val)
	{
	case D_BYTE:
		elements_store(D_BYTE);
		break;

	case D_WORD:
		elements_store(D_WORD);
		break;

	case D_LONG:
		elements_store(D_LONG);
		break;

	case D_ORG:
		break;

	case D_FIT:
		if(org_get() > COG_MAX)
		{
			report_error("Code does not fit in one cog");
		}
		break;

	case D_RES:
		break;
	}
}

static void parse_first(void)
{
	i32 tt = tt_get(0);
	if(tt == T_NULL)
	{
		return;
	}

	if(tt == T_COND)
	{
		Token *t_cond = t_get(0);
		t_next();
		if(!try_parse_instr(t_cond->Val))
		{
			report_error("Expected instruction after condition");
		}

		return;
	}

	if(tt == T_DIRECTIVE)
	{
		parse_directive();
		return;
	}

	if(!try_parse_instr(IF_ALWAYS))
	{
		report_error("Expected instruction");
	}
}

static void count_elements(i32 size)
{
	i32 t, cnt = 1;

	t_next();
	if(tt_get(0) == T_NULL)
	{
		return;
	}

	while((t = tt_get(0)) != T_NULL)
	{
		if(t == T_COMMA)
		{
			++cnt;
		}

		t_next();
	}

	byteoffset += cnt * size;
	org_add(cnt, size);
}

static void pass0(void)
{
	token_prints();
	if(tt_get(0) == T_LABEL)
	{
		Token *t = t_get(0);
		if(tt_get(1) == T_EQ)
		{
			return;
		}

		label_add(t->Ptr, t->Len, org_get(), byteoffset);
		t_next();
	}

	switch(tt_get(0))
	{
	case T_COND:
	case T_INSTR2:
	case T_INSTRK:
	case T_INSTRC:
	case T_INSTRH:
	case T_INSTRJ:
	case T_INSTRN:
	case T_INSTRR:
		org_add(1, 4);
		byteoffset += 4;
		break;

	case T_DIRECTIVE:
		switch(t_get(0)->Val)
		{
		case D_BYTE:
			count_elements(1);
			break;

		case D_WORD:
			count_elements(2);
			break;

		case D_LONG:
			count_elements(4);
			break;

		case D_ORG:
			handle_org();
			break;

		case D_RES:
			handle_res();
			break;
		}
		break;
	}
}

static void pass1(void)
{
	if(tt_get(0) == T_LABEL)
	{
		Token *t = t_get(0);
		t_next();
		if(tt_get(0) == T_EQ)
		{
			t_next();
			i32 v = parse_expr();
			label_add(t->Ptr, t->Len, v, -1);
			return;
		}
	}

	parse_first();
}

static void parse_line(void)
{
	token_idx = 0;
	switch(pass)
	{
	case 0: pass0(); break;
	case 1: pass1(); break;
	}

	token_cnt = 0;
}

static void lexer_advance(void)
{
	++input_ptr;
	if(*input_ptr == '\n')
	{
		++lnr;
	}
}

static i32 matches(char *nt, char *s, size_t len)
{
	size_t i = 0;
	for(; i < len; ++i)
	{
		if(nt[i] == '\0')
		{
			return 0;
		}

		if(nt[i] != to_upper(s[i]))
		{
			return 0;
		}
	}

	if(nt[i] != '\0')
	{
		return 0;
	}

	return 1;
}

static i32 try_parse_identifier(int type)
{
	if(is_ident_start(*input_ptr))
	{
		char *name = input_ptr;
		lexer_advance();
		while(is_ident(*input_ptr))
		{
			lexer_advance();
		}

		size_t name_len = input_ptr - name;
		if(type == T_LABEL)
		{
			for(i32 i = 0; i < ARRLEN(keywords); ++i)
			{
				if(matches(keywords[i].Name, name, name_len))
				{
					token_add(keywords[i].Type, name, name_len, keywords[i].Val);
					return 1;
				}
			}

			token_add(type, name, name_len, 0);
		}
		else
		{
			token_add(type, name, name_len, 0);
		}
		return 1;
	}

	return 0;
}

static i32 parse_hex(void)
{
	i32 n = 0;
	lexer_advance();
	if(!is_hex(*input_ptr))
	{
		report_error("Expected hex digit");
	}

	while(is_hex(*input_ptr) || *input_ptr == '_')
	{
		if(*input_ptr != '_')
		{
			n = acc_hex(n, *input_ptr);
		}

		lexer_advance();
	}

	return n;
}

static i32 parse_bin(void)
{
	i32 n = 0;
	lexer_advance();
	if(!is_bin(*input_ptr))
	{
		report_error("Expected bin digit");
	}

	while(is_bin(*input_ptr) || *input_ptr == '_')
	{
		if(*input_ptr != '_')
		{
			n = acc_bin(n, *input_ptr);
		}

		lexer_advance();
	}

	return n;
}

static i32 try_parse_number(void)
{
	if(is_digit(*input_ptr))
	{
		i32 n = *input_ptr - '0';
		lexer_advance();
		if(n > 0)
		{
			while(is_digit(*input_ptr) || *input_ptr == '_')
			{
				if(*input_ptr != '_')
				{
					n = acc_dec(n, *input_ptr);
				}

				lexer_advance();
			}
		}
		else
		{
			if(is_x(*input_ptr))
			{
				n = parse_hex();
			}
			else if(is_b(*input_ptr))
			{
				n = parse_bin();
			}
			else
			{
				while(is_oct(*input_ptr) || *input_ptr == '_')
				{
					if(*input_ptr != '_')
					{
						n = acc_oct(n, *input_ptr);
					}

					lexer_advance();
				}
			}
		}

		token_add(T_NUMBER, NULL, 0, n);
		return 1;
	}
	else if(*input_ptr == '$')
	{
		i32 n = parse_hex();
		token_add(T_NUMBER, NULL, 0, n);
		return 1;
	}
	else if(*input_ptr == '%')
	{
		i32 n = parse_bin();
		token_add(T_NUMBER, NULL, 0, n);
		return 1;
	}

	return 0;
}

static i32 try_parse_string(void)
{
	if(*input_ptr == '\"')
	{
		lexer_advance();
		for(;;)
		{
			i32 c = *input_ptr;
			i32 v = c;
			if(c == '\"')
			{
				break;
			}
			else if(c == '\\')
			{
				lexer_advance();
				switch(*input_ptr)
				{
					case '\\': v = '\\'; break;
					case '\"': v = '\"'; break;
					case 't':  v = '\t'; break;
					case 'v':  v = '\v'; break;
					case 'n':  v = '\n'; break;
					case 'r':  v = '\r'; break;
					case 'b':  v = '\b'; break;
					case '0':  v = '\0'; break;
					default:
						report_error("Invalid escape sequence");
				}
			}
			else if(c < 32 || c > 126)
			{
				report_error("Unexpected character in string");
			}

			token_add(T_NUMBER, NULL, 0, v);
			lexer_advance();
			if(*input_ptr != '\"')
			{
				t_add(T_COMMA);
			}
		}

		lexer_advance();
		return 1;
	}

	return 0;
}

static void parse_input(void)
{
	lnr = 0;
	input_ptr = input;
	while(input_ptr < input_end)
	{
		switch(*input_ptr)
		{
		case '\n':
			parse_line();
			lexer_advance();
			break;

		case '/':
			t_add(T_DIV);
			lexer_advance();
			break;

		case '=':
			t_add(T_EQ);
			lexer_advance();
			break;

		case '\t':
		case ' ':
			lexer_advance();
			break;

		case '\'':
			while(input_ptr < input_end && *input_ptr != '\n')
			{
				lexer_advance();
			}
			break;

		case '{':
			while(input_ptr < input_end && *input_ptr != '}')
			{
				lexer_advance();
			}

			lexer_advance();
			break;

		case '+':
			t_add(T_ADD);
			lexer_advance();
			break;

		case '-':
			t_add(T_SUB);
			lexer_advance();
			break;

		case '<':
			lexer_advance();
			if(*input_ptr == '<')
			{
				t_add(T_SHL);
				lexer_advance();
			}
			else
			{
				report_error("Unexpected character <");
			}
			break;

		case '>':
			lexer_advance();
			if(*input_ptr == '>')
			{
				t_add(T_SHR);
				lexer_advance();
			}
			else
			{
				report_error("Unexpected character >");
			}
			break;

		case '|':
			t_add(T_OR);
			lexer_advance();
			break;

		case '&':
			t_add(T_AND);
			lexer_advance();
			break;

		case '^':
			t_add(T_XOR);
			lexer_advance();
			break;

		case '%':
			t_add(T_MOD);
			lexer_advance();
			break;

		case '*':
			t_add(T_MUL);
			lexer_advance();
			break;

		case '(':
			t_add(T_LPAREN);
			lexer_advance();
			break;

		case ')':
			t_add(T_RPAREN);
			lexer_advance();
			break;

		case '~':
			t_add(T_INV);
			lexer_advance();
			break;

		case ',':
			t_add(T_COMMA);
			lexer_advance();
			break;

		case '#':
			t_add(T_HASH);
			lexer_advance();
			break;

		case '@':
			lexer_advance();
			if(!try_parse_identifier(T_AT))
			{
				report_error("Expected identifier");
			}
			break;

		default:
			if(try_parse_identifier(T_LABEL)) { break; }
			if(try_parse_number()) { break; }
			if(try_parse_string()) { break; }
			report_error("Unexpected character");
			break;
		}
	}

	parse_line();
}

static void load_input(char *filename_input)
{
	FILE *file_input = fopen(filename_input, "r");
	if(!file_input)
	{
		fprintf(stderr, "Failed to open input file \"%s\"\n", filename_input);
		exit(1);
	}

	size_t r = fread(input, 1, sizeof(input), file_input);
	if(!r)
	{
		fprintf(stderr, "Empty file\n");
		fclose(file_input);
		exit(1);
	}

	if(ferror(file_input))
	{
		fprintf(stderr, "IO Error\n");
		fclose(file_input);
		exit(1);
	}

	if(!feof(file_input))
	{
		fprintf(stderr, "Maximum input file size (%d bytes) exceeded\n",
			ARRLEN(input));
		fclose(file_input);
		exit(1);
	}

	input_len = r;
	input_end = input + input_len;
	fclose(file_input);
}

static void write_output(char *filename_output)
{
	FILE *file_output = fopen(filename_output, "wb");
	if(!file_output)
	{
		fprintf(stderr, "Failed to open output file \"%s\"\n", filename_output);
		exit(1);
	}

	if(fwrite(output, 1, output_len, file_output) != output_len)
	{
		fprintf(stderr, "Failed to write output file\n");
		fclose(file_output);
		exit(1);
	}

	fclose(file_output);
	printf("Wrote %d bytes to output file `%s`\n", (int)output_len, filename_output);
}

int main(int argc, char **argv)
{
	if(argc != 3)
	{
		fprintf(stderr, "Usage: ./assembler input-file output-file\n");
		return 1;
	}

	char *filename_input = argv[1];
	char *filename_output = argv[2];
	load_input(filename_input);
	parse_input();
	labels_print();
	pass = 1;
	parse_input();
	labels_print();
	write_output(filename_output);
	return 0;
}
