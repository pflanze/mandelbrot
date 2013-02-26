
// HACK, sgh, to avoid having to include all that stuff. ok?
typedef int gint;
typedef unsigned char guchar;


struct pb_context {
    guchar *pixels;
    int rowstride;
    int nChannels;
};


void
mandelbrot_render(struct pb_context *ctx, gint w, gint h);
