#include <string.h>
#include "libsys4/include/system4.h"
#include "libsys4/include/system4/ain.h"
#include "libsys4/include/system4/buffer.h"

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
