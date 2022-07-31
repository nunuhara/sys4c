#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "system4.h"
#include "system4_ain.h"
#include "system4_buffer.h"
#include "system4_instructions.h"
#include "system4_string.h"
#include "system4_utfsjis.h"
#include "little_endian.h"

/*
 * XXX: Private API for bindings internal use. Client code should never
 *      touch pointers to ain_* types.
 */

static char *conv(const char *str)
{
	return sjis2utf(str, 0);
}

struct ain *_ain_open(const char *path, int *error)
{
	return ain_open_conv(path, conv, error);
}

int _ain_version(struct ain *ain)
{
	return ain->version;
}

int _ain_minor_version(struct ain *ain)
{
	return ain->minor_version;
}

bool _ain_version_gte(struct ain *ain, int major, int minor)
{
	return AIN_VERSION_GTE(ain, major, minor);
}

struct ain_type *_ain_alloc_type(int n)
{
	return xcalloc(n, sizeof(struct ain_type));
}

char *_ain_string(struct ain *ain, int i)
{
	return (i < 0 || i >= ain->nr_strings) ? NULL : ain->strings[i]->text;
}

char *_ain_message(struct ain *ain, int i)
{
	return (i < 0 || i >= ain->nr_messages) ? NULL : ain->messages[i]->text;
}

void _ain_variable_set_name(struct ain_variable *v, const char *name)
{
	v->name = xstrdup(name);
}

void _ain_variable_set_name2(struct ain_variable *v, const char *name)
{
	v->name2 = xstrdup(name);
}

// functions {{{

int _ain_nr_functions(struct ain *ain)
{
	return ain->nr_functions;
}

struct ain_function *_ain_functions(struct ain *ain)
{
	return ain->functions;
}

struct ain_function *_ain_function(struct ain *ain, int i)
{
	return (i < 0 || i >= ain->nr_functions) ? NULL : &ain->functions[i];
}

void _ain_function_realloc_vars(struct ain_function *f, int nr_vars)
{
	ain_free_variables(f->vars, f->nr_vars);
	f->vars = xcalloc(nr_vars, sizeof(struct ain_variable));
	f->nr_vars = nr_vars;
}

// functions }}}
// globals {{{

int _ain_nr_globals(struct ain *ain)
{
	return ain->nr_globals;
}

struct ain_variable *_ain_globals(struct ain *ain)
{
	return ain->globals;
}

struct ain_variable *_ain_global(struct ain *ain, int i)
{
	return (i < 0 || i >= ain->nr_globals) ? NULL : &ain->globals[i];
}

// globals }}}
// structures {{{

int _ain_nr_structures(struct ain *ain)
{
	return ain->nr_structures;
}

struct ain_struct *_ain_structures(struct ain *ain)
{
	return ain->structures;
}

struct ain_struct *_ain_struct(struct ain *ain, int i)
{
	return (i < 0 || i >= ain->nr_structures) ? NULL : &ain->structures[i];
}

void _ain_struct_realloc_members(struct ain_struct *s, int nr_members)
{
	ain_free_variables(s->members, s->nr_members);
	s->members = xcalloc(nr_members, sizeof(struct ain_variable));
	s->nr_members = nr_members;
}

// structures }}}
// function types {{{

int _ain_nr_functypes(struct ain *ain)
{
	return ain->nr_function_types;
}

struct ain_function_type *_ain_functypes(struct ain *ain)
{
	return ain->function_types;
}

struct ain_function_type *_ain_functype(struct ain *ain, int i)
{
	return (i < 0 || i >= ain->nr_function_types) ? NULL : &ain->function_types[i];
}

void _ain_functype_realloc_vars(struct ain_function_type *f, int nr_vars)
{
	ain_free_variables(f->variables, f->nr_variables);
	f->variables = xcalloc(nr_vars, sizeof(struct ain_variable));
	f->nr_variables = nr_vars;
}

// function types }}}
// libraries {{{

int _ain_nr_libraries(struct ain *ain)
{
	return ain->nr_libraries;
}

struct ain_hll_function *_ain_library_function(struct ain *ain, int lib_no, int fun_no)
{
	if (lib_no < 0 || lib_no >= ain->nr_libraries)
		return NULL;
	if (fun_no < 0 || fun_no >= ain->libraries[lib_no].nr_functions)
		return NULL;
	return &ain->libraries[lib_no].functions[fun_no];
}

struct ain_library *_ain_library(struct ain *ain, int i)
{
	return (i < 0 || i >= ain->nr_libraries) ? NULL : &ain->libraries[i];
}

void _ain_library_realloc_functions(struct ain_library *lib, int nr_functions)
{
	for (int i = 0; i < lib->nr_functions; i++) {
		ain_free_hll_function(&lib->functions[i]);
	}
	free(lib->functions);
	lib->functions = xcalloc(nr_functions, sizeof(struct ain_hll_function));
	lib->nr_functions = nr_functions;
}

void _ain_hll_function_realloc_args(struct ain_hll_function *f, int nr_args)
{
	for (int i = 0; i < f->nr_arguments; i++) {
		ain_free_hll_argument(&f->arguments[i]);
	}
	free(f->arguments);
	f->arguments = xcalloc(nr_args, sizeof(struct ain_hll_argument));
	f->nr_arguments = nr_args;
}

void _ain_set_hll_function_name(struct ain_hll_function *f, const char *name)
{
	f->name = xstrdup(name);
}

void _ain_set_hll_argument_name(struct ain_hll_argument *a, const char *name)
{
	a->name = xstrdup(name);
}

// libraries }}}
// code {{{

struct dasm {
	struct ain *ain;
	size_t addr;
	const struct instruction *instr;
};

void dasm_jump(struct dasm *dasm, uint32_t addr);

struct dasm *dasm_open(struct ain *ain)
{
	struct dasm *dasm = xcalloc(1, sizeof(struct dasm));
	dasm->ain = ain;
	dasm_jump(dasm, 0);
	return dasm;
}

void dasm_close(struct dasm *dasm)
{
	free(dasm);
}

bool dasm_eof(struct dasm *dasm)
{
	return dasm->addr >= dasm->ain->code_size;
}

int32_t dasm_addr(struct dasm *dasm)
{
	return dasm->addr;
}

static const struct instruction *dasm_get_instruction(struct dasm *dasm)
{
	uint16_t opcode = LittleEndian_getW(dasm->ain->code, dasm->addr);
	return &instructions[opcode];
}

void dasm_jump(struct dasm *dasm, uint32_t addr)
{
	dasm->addr = addr;
	dasm->instr = dasm_eof(dasm) ? &instructions[0] : dasm_get_instruction(dasm);
}

void dasm_next(struct dasm *dasm)
{
	dasm->addr += instruction_width(dasm->instr->opcode);
	dasm->instr = dasm_eof(dasm) ? &instructions[0] : dasm_get_instruction(dasm);
}

int dasm_peek(struct dasm *dasm)
{
	int width = instruction_width(dasm->instr->opcode);
	if (dasm->addr+width >= dasm->ain->code_size)
		return -1;
	return LittleEndian_getW(dasm->ain->code, dasm->addr+width);
}

int dasm_opcode(struct dasm *dasm)
{
	return dasm->instr->opcode;
}

int dasm_nr_args(struct dasm *dasm)
{
	return dasm->instr->nr_args;
}

int32_t dasm_arg(struct dasm *dasm, int n)
{
	if (n < 0 || n >= dasm->instr->nr_args)
		return 0;
	return LittleEndian_getDW(dasm->ain->code, dasm->addr + 2 + 4*n);
}

int dasm_arg_type(struct dasm *dasm, int n)
{
	if (n < 0 || n >= dasm->instr->nr_args)
		return 0;
	return dasm->instr->args[n];
}

void _ain_append_bytecode(struct ain *ain, struct buffer *b)
{
	ain->code = xrealloc(ain->code, ain->code_size + b->index);
	memcpy(ain->code + ain->code_size, b->buf, b->index);
	ain->code_size += b->index;
}

int _ain_code_size(struct ain *ain)
{
	return ain->code_size;
}

void _ain_set_main_function(struct ain *ain, int no)
{
	ain->main = no;
}

void _ain_set_message_function(struct ain *ain, int no)
{
	ain->msgf = no;
}

// code }}}
