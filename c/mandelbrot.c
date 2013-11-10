#define USE_SIMD
//#define USE_SIMD_off
#define USE_SIMD2
//XX my makefile setup doesn't let me pass that through hu

#include <assert.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

//which one? for int64_t
#include <stdint.h>
#include <inttypes.h> 

// HACK, sgh, to avoid having to include all that stuff. ok?
typedef int gint;
typedef unsigned char guchar;

#include "nstime.h"
#include "mandelbrot.h"

#include "util.h"

//#include "x_posix_memalign.h"


#define STATIC static

/* Complex numbers */

#ifdef USE_SIMD
typedef double v2_double __attribute__ ((vector_size (16), aligned (16)));
//typedef unsigned long v2_ulong __attribute__ ((vector_size (16), aligned (16)));
typedef int64_t v2_long __attribute__ ((vector_size (16), aligned (16)));

#define SIMD __attribute__ ((aligned (16)))

#define Cr(x) x[0]
#define Ci(x) x[1]

#else

#define SIMD

#endif


struct complex_double {
    double r;
    double i;
} SIMD;
typedef struct complex_double complex_double SIMD;


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


#ifdef USE_SIMD
#define V2(var) v2_double *v##var= CAST(v2_double*,var)
#endif


#ifdef USE_SIMD2

//struct complex_double2 { complex_double val[2]; } SIMD;
//typedef struct complex_double2 complex_double2 SIMD;
//XXX for now.
struct complex_double2 {
    double r0,r1;
    double i0,i1;
    // (and packed?)
} SIMD;
typedef struct complex_double2 complex_double2 SIMD;

/*
#define C2_r(x) (CAST(v2_double*,(x)))
#define C2_i(x) (CAST(v2_double*,&((x)->i0)))
type check, please..:
*/
STATIC inline v2_double*
C2_r(complex_double2 *x) {
    return CAST(v2_double*,x);
}
STATIC inline v2_double*
C2_i(complex_double2 *x) {
    return CAST(v2_double*,&(x->i0));
}

#include "mandelbrot2.h"

STATIC void
inscreen2 (v2_double *res,
	   int to,
	   int i0,
	   int i1,
	   double fromr,
	   double tor) {
    int from= 0;

    v2_double idiff;
    idiff[0]= (i0 - from);
    idiff[1]= (i1 - from);

    double ddiff= (to - from);

    (*res)= fromr + (tor - fromr) * idiff / ddiff;
}

#undef DEBUG
//#else
#endif


#define DEBUG(x)


#if 1


/* Helper functions */

STATIC void
magnitudesquare (double*res, complex_double*x) {
#ifdef USE_SIMD_off
    v2_double *xv= CAST(v2_double*,x);
    v2_double xv2 = *xv * *xv;
    *res= Cr(xv2) + Ci(xv2);
#else
    double r= x->r;
    double i= x->i;
    *res= r*r + i*i;
#endif
}


STATIC void
complex_double_square(complex_double*res, complex_double*x) {
    double r= x->r;
    double i= x->i;
#ifdef USE_SIMD_off
    v2_double *xv= CAST(v2_double*,x);
    {
	v2_double xv2 = *xv * *xv;
	res->r = Cr(xv2) - Ci(xv2);
    }
#else
    res->r = r*r - i*i;
#endif
    res->i = 2*r*i;
}


STATIC void
complex_double_add(complex_double*res, complex_double*a, complex_double*b) {
#ifdef USE_SIMD_off
    V2(a);
    V2(b);
    V2(res);
    *vres= *va + *vb;
#else
    res->r = a->r + b->r;
    res->i = a->i + b->i;
#endif
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

#endif

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


void
mandelbrot_render(struct pb_context *ctx, gint w, gint h,
		  double xcenter, double ycenter, double size, int depth) {
    struct nstime t0,t1;
    {
	nstime_init(&t0);
	nstime_init(&t1);
	x_nstime_print_resolution(&t0);
	x_nstime_gettime(&t0);
	{
	    /* assuming square pixels */
	    double aspect= CAST(double,h)/CAST(double,w);
	    /* assume size is really width? */
	    double width= size;
	    double fromx= xcenter - width;
	    double tox= xcenter + width;
	    double height= width * aspect;
	    double fromy= ycenter - height;
	    double toy= ycenter + height;

	    printf("  fromx=%.15lf\n",fromx);
	    printf("    tox=%.15lf\n",tox);
	    printf("  fromy=%.15lf\n",fromy);
	    printf("    toy=%.15lf\n",toy);
	    printf("  depth=%d\n",depth);
	    
	    int _x, _y;
#pragma omp parallel for					\
    shared(w,h,fromx,tox,fromy,toy) private(_x,_y)		\
    schedule(dynamic,20)
	    for (_y=0; _y<h; _y++) {

#ifdef USE_SIMD2

#               define MEMSIZ 64 /* ...(see MEMSIZ 32 below) */
		char mem[MEMSIZ];
		// XXX unsigned long, hm. WORD, please.
		void *memaligned = CAST(void*, CAST(intptr_t,mem) & ~ 15);
		complex_double2 *p = memaligned;
		//printf("mem=%p, p=%p\n",mem,p);

		p->i0= inscreen(0,h,_y, fromy, toy);
		p->i1= inscreen(0,h,_y, fromy, toy);

		for (_x=0; _x<(w-1) /*XXXhack to stay within bounds*/; _x+=2) {
		    inscreen2(C2_r(p),
			      w, _x, _x+1, fromx, tox);
		    {
			int d[2];
			mandelbrotDepth2(d, depth, p);
			unsigned char l0= d[0] * 255 / depth;
			unsigned char l1= d[1] * 255 / depth;
			setPoint(ctx, _x, _y, l0,l0,l0);
			setPoint(ctx, _x+1, _y, l1,l1,l1);
			DEBUG(printf("((%.14e):+(%.14e)) -> %i\n",
				     p->r0, p->i0, l0));
			DEBUG(printf("((%.14e):+(%.14e)) -> %i\n",
				     p->r1, p->i1, l1));
		    }
		}

#else

		for (_x=0; _x<w; _x++) {
#define MEMSIZ 32 /* 2*sizeof(complex_double) but calculating manually right now */
		    char mem[MEMSIZ];
		    // XXX unsigned long, hm. WORD, please.
		    void *memaligned = CAST(void*, CAST(intptr_t,mem) & ~ 15);
		    complex_double *p = memaligned;
		    //printf("mem=%p, p=%p\n",mem,p);
		    p->r= inscreen(0,w,_x, fromx, tox);
		    p->i= inscreen(0,h,_y, fromy, toy);
		    {
			int d= mandelbrotDepth(depth, p);
			unsigned char l= d * 255 / depth;
			setPoint(ctx, _x, _y, l,l,l);
		    }
		}
#endif

	    }
	}
	x_nstime_gettime(&t1);
	x_nstime_print_diff(&t0,&t1);
    }
}


