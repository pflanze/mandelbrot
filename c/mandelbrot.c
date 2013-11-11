/* Configuration.  XX my makefile setup doesn't let me pass that through */

/* Whether to use SIMD at all */
#define USE_SIMD

#ifdef USE_SIMD

/* Whether to use SIMD in complex number library. Not worth it */
//#define USE_SIMD_off

/* Whether to use SIMD to calculate 2 points in parallel using
   mandelbrot2.h.  Worth it */
#define USE_SIMD2

#endif /* USE_SIMD */

/* ------------------------------------------------------------------ */

#include <assert.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

//which one? for int64_t
#include <stdint.h>
#include <inttypes.h> 

// HACK to avoid having to include glib or gtk headers
typedef int gint;
typedef unsigned char guchar;

#include "nstime.h"
#include "mandelbrot.h"

#include "util.h"


#define STATIC static


#ifdef USE_SIMD
typedef double v2_double __attribute__ ((vector_size (16), aligned (16)));
typedef float v4_float __attribute__ ((vector_size (16), aligned (16)));
typedef int64_t v2_long __attribute__ ((vector_size (16), aligned (16)));
typedef int32_t v4_int __attribute__ ((vector_size (16), aligned (16)));
typedef int32_t v2_int __attribute__ ((vector_size (8), aligned (16)));

#define SIMD __attribute__ ((aligned (16)))

#define Cr(x) x[0]
#define Ci(x) x[1]

#define V2(var) v2_double *v##var= CAST(v2_double*,var)

#else

#define SIMD

#endif /* USE_SIMD */


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



#ifdef USE_SIMD2

struct complex_double2 {
    double r0,r1;
    double i0,i1;
    // (and packed?)
} SIMD;
typedef struct complex_double2 complex_double2 SIMD;

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

#else  /*! USE_SIMD2 */

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

#endif /* !USE_SIMD2 */

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


#define DEBUG(x)


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

		complex_double2 p1,p2;

		p1.i0= inscreen(0,h,_y, fromy, toy);
		p1.i1= inscreen(0,h,_y, fromy, toy);
		p2.i0= inscreen(0,h,_y, fromy, toy);
		p2.i1= inscreen(0,h,_y, fromy, toy);

		double fdepth= depth;
		double fratio= 255.0 / fdepth;
		v4_float v4depth;
		v4depth[0]= fratio;
		v4depth[1]= fratio;
		v4depth[2]= fratio;
		v4depth[3]= fratio;

		for (_x=0; _x<(w-3) /*XX hack to stay within bounds*/; _x+=4) {
		    inscreen2(C2_r(&p1),
			      w, _x, _x+1, fromx, tox);
		    inscreen2(C2_r(&p2),
			      w, _x+2, _x+3, fromx, tox);
		    {
			v4_int d;
			mandelbrotDepth4(&d, depth, &p1, &p2);
			float d0= d[0];
			float d1= d[1];
			float d2= d[2];
			float d3= d[3];
			v4_float _d;
			_d[0]=d0;
			_d[1]=d1;
			_d[2]=d2;
			_d[3]=d3;
			v4_float _l = _d * v4depth;
			unsigned char l0= _mm_cvtt_ss2si(_mm_load_ss(&(_l[0])));
			unsigned char l1= _mm_cvtt_ss2si(_mm_load_ss(&(_l[1])));
			unsigned char l2= _mm_cvtt_ss2si(_mm_load_ss(&(_l[2])));
			unsigned char l3= _mm_cvtt_ss2si(_mm_load_ss(&(_l[3])));
			setPoint(ctx, _x, _y, l0,l0,l0);
			setPoint(ctx, _x+1, _y, l1,l1,l1);
			setPoint(ctx, _x+2, _y, l2,l2,l2);
			setPoint(ctx, _x+3, _y, l3,l3,l3);

			DEBUG(printf("((%.14e):+(%.14e)) -> %i\n",
				     p1.r0, p1.i0, l0));
			DEBUG(printf("((%.14e):+(%.14e)) -> %i\n",
				     p1.r1, p1.i1, l1));
			DEBUG(printf("((%.14e):+(%.14e)) -> %i\n",
				     p2.r0, p2.i0, l0));
			DEBUG(printf("((%.14e):+(%.14e)) -> %i\n",
				     p2.r1, p2.i1, l1));
		    }
		}

#else
		for (_x=0; _x<w; _x++) {
		    complex_double p;
		    p.r= inscreen(0,w,_x, fromx, tox);
		    p.i= inscreen(0,h,_y, fromy, toy);
		    {
			int d= mandelbrotDepth(depth, &p);
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


