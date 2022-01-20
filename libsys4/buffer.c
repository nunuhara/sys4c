#include <stdlib.h>
#include "system4.h"
#include "system4_buffer.h"

struct buffer *_buffer_create(size_t initial_size)
{
	struct buffer *b = xmalloc(sizeof(struct buffer));
	uint8_t *data = xmalloc(initial_size);
	buffer_init(b, data, initial_size);
	return b;
}

void _buffer_free(struct buffer *b)
{
	free(b->buf);
	free(b);
}

void _buffer_clear(struct buffer *b)
{
	b->index = 0;
}
