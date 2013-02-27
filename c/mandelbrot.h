

struct pb_context {
    guchar *pixels;
    int rowstride;
    int nChannels;
};


void
mandelbrot_render(struct pb_context *ctx, gint w, gint h,
		  double xcenter, double ycenter, double size, int depth);
