#include <string.h>
#include "system4.h"
#include "system4_ain.h"
#include "system4_buffer.h"
#include "system4_utfsjis.h"

static char *conv(const char *str)
{
	return sjis2utf(str, 0);
}

struct ain *_ain_open(const char *path, int *error)
{
	return ain_open_conv(path, conv, error);
}

/*
 * XXX: Private API for bindings internal use. Client code should never
 *      touch pointers to ain_* types.
 */

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

struct ain_variable *_ain_global(struct ain *ain, int i)
{
	return (i < 0 || i >= ain->nr_globals) ? NULL : &ain->globals[i];
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

struct ain_library *_ain_library(struct ain *ain, int i)
{
	return (i < 0 || i >= ain->nr_libraries) ? NULL : &ain->libraries[i];
}

struct ain_hll_function *_ain_library_function(struct ain *ain, int lib_no, int fun_no)
{
	if (lib_no < 0 || lib_no >= ain->nr_libraries)
		return NULL;
	if (fun_no < 0 || fun_no >= ain->libraries[lib_no].nr_functions)
		return NULL;
	return &ain->libraries[lib_no].functions[fun_no];
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

struct ain_type *_ain_alloc_type(int n)
{
	return xcalloc(n, sizeof(struct ain_type));
}
