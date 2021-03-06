#include "libsys4/include/system4.h"
#include "libsys4/include/system4/ain.h"

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
