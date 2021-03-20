#include "libsys4/include/system4/ain.h"

/*
 * XXX: Private API for bindings internal use. Client code should never
 *      touch pointers to ain_* types.
 */

struct ain_function *_ain_function(struct ain *ain, int i)
{
	return (i < 0 || i >= ain->nr_functions) ? NULL : &ain->functions[i];
}

struct ain_variable *_ain_global(struct ain *ain, int i)
{
	return (i < 0 || i >= ain->nr_globals) ? NULL : &ain->globals[i];
}
