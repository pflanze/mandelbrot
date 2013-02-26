#include <assert.h>
#include <unistd.h>
#include <stdlib.h>

#include "nstime.h"
#include "mandelbrot.h"

#define STATIC static

#define CAST(to,expr) (to)(expr)

// for SIMD:
typedef double v2_double __attribute__ ((vector_size (16)));

#define Cr(x) x[0]
#define Ci(x) x[1]
// SIMD


struct complex_double {
    double r;
    double i;
};
typedef struct complex_double complex_double; // bad?

// myIterateUntil :: (a -> Bool) -> Int -> (a -> a) -> a -> (Int, a)
// but (Int, a) is passed as *res, *z; *z doubling as start value
#define ITERATE_UNTIL(res, pred, maxdepth, fn, fn_first_arg, z)	\
    {								\
    int __d= maxdepth;						\
    while (1) {							\
        if (__d == 0) { res= maxdepth; break; }			\
	if (pred(&z)) { res= maxdepth-__d; break; }		\
	__d--; fn(&z, fn_first_arg, &z);			\
    }								\
    }


STATIC void
magnitudesquare (double*res, complex_double*x) {
    // *res= r*r + i*i;
    v2_double *xv= CAST(v2_double*,x);
    v2_double xv2 = *xv * *xv;
    *res= Cr(xv2) + Ci(xv2);
}

STATIC void
complex_double_square(complex_double*res, complex_double*x) {
    v2_double *xv= CAST(v2_double*,x);
    double r= x->r;
    double i= x->i;
    {
	v2_double xv2 = *xv * *xv;
	res->r = Cr(xv2) - Ci(xv2);
    }
    res->i = 2*r*i;
}

STATIC void
complex_double_add(complex_double*res, complex_double*a, complex_double*b) {
    res->r = a->r + b->r;
    res->i = a->i + b->i;
}

//-- Mandelbrot series

STATIC void
pIter(complex_double*res, complex_double*c, complex_double*z) {
    complex_double_square(res, z);
    complex_double_add(res, res, c);
}

//-- and its presentation

// isDiverged !x = (magnitudesquare x) > (1e10**2)
STATIC int
isDiverged(complex_double*x) {
    double tmp;
    magnitudesquare(&tmp, x);
    return (tmp > 1e20);
}

STATIC int
mandelbrotDepth(int maxdepth, complex_double*p) {
    int res;
    complex_double zero= { 0.0, 0.0 };
    ITERATE_UNTIL(res, isDiverged, maxdepth, pIter, p, zero);
    return res;
}

STATIC double
inscreen (int from, int to, int i, double fromr, double tor) {
    double idiff= (i - from);
    double ddiff= (to - from);
    return fromr + (tor - fromr) * idiff / ddiff;
}

STATIC void
setPoint(struct pb_context* ctx,
	 int x, int y, int r, int g, int b) {
    guchar *pixels= ctx->pixels;
    int p = y * ctx->rowstride + x * ctx->nChannels;
    pixels[p] = r;
    pixels[p+1] = g;
    pixels[p+2] = b;
}

STATIC
int depth = 200;

void
mandelbrot_render(struct pb_context *ctx, gint w, gint h) {
    struct nstime t0,t1;
    {
	nstime_init(&t0);
	nstime_init(&t1);
	x_nstime_print_resolution(&t0);
	x_nstime_gettime(&t0);
	{    
	    int _x, _y;
	    for (_x=0; _x<w; _x++) {
		for (_y=0; _y<h; _y++) {
		    complex_double p;
		    p.r= inscreen(0,w,_x,-2.0,1.0);
		    p.i= inscreen(0,h,_y,-1.0,1.0);
		    {
			int d= mandelbrotDepth(depth, &p);
			unsigned char l= d * 255 / depth;
			setPoint(ctx, _x, _y, l,l,l);
		    }
		}
	    }
	}
	x_nstime_gettime(&t1);
	x_nstime_print_diff(&t0,&t1);
    }
}

