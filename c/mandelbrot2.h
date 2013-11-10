#define DEBUG(x)

/*
   calculate 2 mandelbrot points in parallel with SIMD: 
   calculate all complex math serially, but instead do everything 2 times in parallel.

   Means, there will be waste of some depth calculation since we can
   only stop iterating if *both* points have diverged.
   (Crazy optim?: immediately restart the finished channel with a new point?)
*/


STATIC void
pIter2(complex_double2 *res, complex_double2 *c, complex_double2 *z) {
    /*
      res = z^2 + c
      = z * z + c
      = ((z.r * z.r - z.i * z.i) + (2 * z.r * z.i)*i) + c
    */
    // need to store this value as res==z !!
    v2_double rz= *C2_r(z);
    *C2_r(res) =
	((*C2_r(z) * *C2_r(z))
	 -
	 (*C2_i(z) * *C2_i(z)))
	+ *C2_r(c);
    *C2_i(res) = (2 * rz * *C2_i(z))
	+ *C2_i(c);
}

STATIC void
magnitudesquare2 (v2_double *res, complex_double2 *x) {
    /*
      res= x.r * x.r + x.i * x.i;
    */
    *res = *C2_r(x) * *C2_r(x) + *C2_i(x) * *C2_i(x);
}


#include <xmmintrin.h>

// isDiverged !x = (magnitudesquare x) > (1e10**2)
STATIC void
isDiverged2(v2_long *res, complex_double2 *x) {
    v2_double tmp;
    magnitudesquare2(&tmp, x);
    /*
      tmp = tmp - 1e20;
      (*res)[0]= (tmp[0] > 0);
      (*res)[1]= (tmp[1] > 0);
      is slower
    */
    (*res)[0]= -(tmp[0] > 1e20);
    (*res)[1]= -(tmp[1] > 1e20);
    /*
      int i;
      for (i=0; i<2; i++) {
         (*res)[i]= (tmp[i] > 0);
      }
      does not work, gcc bug?
    */
    /*
      v2_double lim= { 1e20, 1e20 };
      *res = _mm_cmpgt_epf64(tmp, lim);
      does not exist
    */
}

// debugging
STATIC complex_double*
new_complex_double(double r, double i) {
    complex_double *p= malloc(sizeof(complex_double));
    p->r=r;
    p->i=i;
    return p;
}


STATIC void
mandelbrotDepth2(int *res, int maxdepth, complex_double2 *p) {
    complex_double2 z= { 0.0, 0.0, 0.0, 0.0 };

    res[0]= -1;
    res[1]= -1;

    DEBUG(printf("mandelbrotDepth2( ((%g) :+ (%g)), ((%g) :+ (%g)) )\n",
		 p->r0, p->i0,  p->r1, p->i1));
    
    // ITERATE_UNTIL(res, isDiverged, maxdepth, pIter, p, zero);
    // #define ITERATE_UNTIL(res, pred, maxdepth, fn, fn_first_arg, z)	..:
    {
	int d= maxdepth;
	int channels=2;
	while (1) {
	    v2_long isdiverged;
	    isDiverged2(&isdiverged, &z);
	    DEBUG(printf("(%d,%d)= isDiverged2(%g,%g,%g,%g)\n",
			 isdiverged[0], 
			 isdiverged[1],
			 z.r0,z.r1,z.i0,z.i1));
	    /* if (isdiverged[0] || isdiverged[1]) { */
	    /* 	complex_double *man= new_complex_double (-4919165.4427832961, -48408484422782.219); */
	    /* 	abort();// */
	    /* } */
	    if (isdiverged[0] && res[0]==-1) {
		res[0] = maxdepth-d;
		channels--;
	    }
	    if (isdiverged[1] && res[1]==-1) {
		res[1] = maxdepth-d;
		channels--;
	    }
	    if (!channels)
		return; // hu, why did break not work?
	    d--;
	    if (d == 0) {
		if (res[0]==-1)
		    res[0] = maxdepth;
		if (res[1]==-1)
		    res[1] = maxdepth;
		return;
	    }
	    pIter2(&z, p, &z);
	}
    }
}

