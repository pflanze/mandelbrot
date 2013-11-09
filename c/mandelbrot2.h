
/*
   calculate 2 mandelbrot points in parallel with SIMD: 
   calculate all complex math serially, but instead do everything 2 times in parallel.

   Means, there will be waste of some depth calculation since we can
   only stop iterating if *both* points have diverged.
   (Crazy optim?: immediately restart the finished channel with a new point?)
*/

STATIC void
mandelbrotDepth2(int res[2], int maxdepth, complex_double*ps) {
    complex_double z[2]= {{ 0.0, 0.0 }, { 0.0, 0.0 }};
    //hmmm uf.  lay out as   z0r z1r   z0c z1c  ?
    res[0]= 0;
    res[1]= 0;
    
    // ITERATE_UNTIL(res, isDiverged, maxdepth, pIter, p, zero);
    // #define ITERATE_UNTIL(res, pred, maxdepth, fn, fn_first_arg, z)	..:
    {
	int *remainder_res;
	complex_double *remainder_z;
	complex_double *remainder_p;

	int __d= maxdepth;
	while (1) {
	    if (__d == 0) {
		res[0] = maxdepth;
		res[1] = maxdepth;
		return;
	    }
	    if (isDiverged(&(z[0]))) {
		res[0] = maxdepth-__d;
		remainder_res= &(res[1]);
		remainder_z= &(z[1]);
		remainder_p= &(ps[1]);
		goto remainder_HACK;
	    }
	    if (isDiverged(&(z[1]))) {
		res[1] = maxdepth-__d;
		remainder_res= &(res[0]);
		remainder_z= &(z[0]);
		remainder_p= &(ps[0]);
		goto remainder_HACK;
	    }
	    __d--;
	    pIter(&(z[0]), &(ps[0]), &(z[0]));
	    pIter(&(z[1]), &(ps[1]), &(z[1]));
	}
	//remainder:
	while (1) {
	    if (__d == 0) {
		*remainder_res= maxdepth;
		return;
	    }
	remainder_HACK:
	    if (isDiverged(remainder_z)) {
		*remainder_res = maxdepth-__d;
		break;
	    }
	    __d--;
	    pIter(remainder_z, remainder_p, remainder_z);
	}
    }
}

