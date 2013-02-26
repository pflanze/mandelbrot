/*
  nano-second real time timing

  Linux implementation

  man clock_gettime

  Link with -lrt.
*/

#include "nstime.h"

#include <stdio.h>
#include <stdlib.h> /* abort */
#include <assert.h>

#define XNSTIME(expr) { int ___err= expr; if (___err) { perror(#expr); abort(); }}

#define STATIC static

void
nstime_init(struct nstime* t) {
    t->clkid= CLOCK_MONOTONIC_RAW; /* XX hm? */
}

/*
STATIC void
x_nstime_getres(struct nstime* t) {
    XNSTIME(clock_getres(t->clkid, &(t->ts)));
}
*/

void
x_nstime_gettime(struct nstime* t) {
    XNSTIME(clock_gettime(t->clkid, &(t->ts)));
}

/*
STATIC void
x_nstime(struct nstime *res) {
    nstime_init(res);
    x_nstime_gettime(res);
}
*/

STATIC void
nstime_diff(struct timespec *res, struct nstime *a, struct nstime *b) {
    long ndiff;
    assert(a->clkid == b->clkid);
    ndiff = b->ts.tv_nsec - a->ts.tv_nsec;
    res->tv_sec = b->ts.tv_sec - a->ts.tv_sec + (ndiff < 0 ? -1 : 0);
    res->tv_nsec = (ndiff < 0 ? ndiff + 1000000000 : ndiff);
}


void
x_nstime_print_resolution (struct nstime *t) {
    struct timespec r;
    XNSTIME(clock_getres(t->clkid, &r));
    printf("nstime resolution: %ld.%09ld seconds\n", r.tv_sec, r.tv_nsec);
}

void
x_nstime_print_diff(struct nstime *t0, struct nstime *t1) {
    struct timespec r;
    nstime_diff(&r, t0, t1);
    printf("nstime diff: %ld.%09ld seconds\n", r.tv_sec, r.tv_nsec);
}
