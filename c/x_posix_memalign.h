#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static void* x_posix_memalign(size_t alignment, size_t size) {
    void *p=NULL; /* huh, avoid uninitialized warning */
    int err= posix_memalign(p, alignment, size);
    if (err) {
	fprintf(stderr, "fatal: posix_memalign: %s\n", strerror(err));
	exit(1);
    }
    return p;
}

