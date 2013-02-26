/*
  nano-second real time timing

  Linux implementation

  man clock_gettime

  Link with -lrt.
*/


#include <time.h>
#include <stdio.h>
#include <stdlib.h> /* abort */

#define XNSTIME(expr) { int ___err= expr; if (___err) { perror(#expr); abort(); }}


struct nstime {
    clockid_t clkid;
    struct timespec ts;
};

STATIC void
nstime_init(struct nstime* t) {
    t->clkid= 0; /* XXX what should this be? man page doesn't say, sigh */
}

void
x_nstime_getres(struct nstime* t) {
    XNSTIME(clock_getres(t->clkid, &(t->ts)));
}

STATIC void
x_nstime_gettime(struct nstime* t) {
    XNSTIME(clock_gettime(t->clkid, &(t->ts)));
}


STATIC void
x_nstime(struct nstime *res) {
    nstime_init(res);
    x_nstime_gettime(res);
}

STATIC void
nstime_diff(struct timespec *res, struct nstime *a, struct nstime *b) {
    res->tv_sec = b->ts.tv_sec - a->ts.tv_sec;
    res->tv_nsec = b->ts.tv_nsec - a->ts.tv_nsec;
}


void
x_nstime_print_resolution (struct nstime *t) {
    struct timespec r;
    XNSTIME(clock_getres(t->clkid, &r));
    printf("nstime resolution: %ld.%9ld seconds\n", r.tv_sec, r.tv_nsec);
}

