#include <time.h>

struct nstime {
    clockid_t clkid;
    struct timespec ts;
};

void
nstime_init(struct nstime* t);

void
x_nstime_gettime(struct nstime* t);

void
x_nstime_print_resolution (struct nstime *t);

void
x_nstime_print_diff(struct nstime *t0, struct nstime *t1);
