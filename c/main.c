#include <assert.h>
#include <unistd.h>
#include <stdlib.h>

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>


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


void
magnitudesquare (double*res, complex_double*x) {
    double r= x->r;
    double i= x->i;
    *res= r*r + i*i;
}

void
complex_double_square(complex_double*res, complex_double*x) {
    double r= x->r;
    double i= x->i;
    res->r = r*r - i*i;
    res->i = 2*r*i;
}

void
complex_double_add(complex_double*res, complex_double*a, complex_double*b) {
    res->r = a->r + b->r;
    res->i = a->i + b->i;
}

//-- Mandelbrot series

void
pIter(complex_double*res, complex_double*c, complex_double*z) {
    complex_double_square(res, z);
    complex_double_add(res, res, c);
}

//-- and its presentation

// isDiverged !x = (magnitudesquare x) > (1e10**2)
int
isDiverged(complex_double*x) {
    double tmp;
    magnitudesquare(&tmp, x);
    return (tmp > 1e20);
}

int
mandelbrotDepth(int maxdepth, complex_double*p) {
    int res;
    complex_double zero= { 0.0, 0.0 };
    ITERATE_UNTIL(res, isDiverged, maxdepth, pIter, p, zero);
    return res;
}

double
inscreen (int from, int to, int i, double fromr, double tor) {
    double idiff= (i - from);
    double ddiff= (to - from);
    return fromr + (tor - fromr) * idiff / ddiff;
}

struct pb_context {
    guchar *pixels;
    int rowstride;
    int nChannels;
};

void
pb_get_context(struct pb_context* ctx, GdkPixbuf *pb) {
    ctx->pixels= gdk_pixbuf_get_pixels(pb);
    ctx->rowstride= gdk_pixbuf_get_rowstride(pb);
    ctx->nChannels= gdk_pixbuf_get_n_channels(pb);
}


void
setPoint(struct pb_context* ctx,
	 int x, int y, int r, int g, int b) {
    guchar *pixels= ctx->pixels;
    int p = y * ctx->rowstride + x * ctx->nChannels;
    pixels[p] = r;
    pixels[p+1] = g;
    pixels[p+2] = b;
}


int depth = 200;

GtkWidget *__HACK_drawing;

int
renderScene (GtkWidget *d, GdkEventExpose *ev, gpointer data) {
    GtkWidget *dw= __HACK_drawing;
    gint w,h;
    w=200; h=200; //XXX gtk_window_get_size(d, &w, &h);
    
    // what is gc for, if never changed???      gc     <- gcNew dw
    //-- pixbuf
    assert(dw);
    assert(w>0);
    assert(h>0);
    {
	GdkPixbuf *pb= gdk_pixbuf_new(GDK_COLORSPACE_RGB, 0, 8, w, h);
	assert(pb);
	{
	    int _x, _y;
	    struct pb_context ctx;
	    pb_get_context(&ctx, pb);
	    for (_x=0; _x<w; _x++) {
		for (_y=0; _y<h; _y++) {
		    complex_double p;
		    p.r= inscreen(0,w,_x,-2.0,1.0);
		    p.i= inscreen(0,h,_y,-1.0,1.0);
		    {
			int d= mandelbrotDepth(depth, &p);
			unsigned char l= d * 255 / depth;
			setPoint(&ctx, _x, _y, l,l,l);
		    }
		}
	    }
	}
	gdk_draw_pixbuf(dw, NULL, pb, 0, 0, 0, 0, w, h, GDK_RGB_DITHER_NONE,0,0);
    }
    return 0;
}


// ----- Gtk wrappers

void
onExpose(GtkWidget *drawing,
	 int(proc)(GtkWidget *d, GdkEventExpose *ev, gpointer data)) {
    g_signal_connect(drawing, "expose-event",
		     G_CALLBACK(proc), NULL);
}

void
onDestroy(GtkWidget *window, // ?
	  /* eh odd: different than above (how would that work without
	     casting?), but that's what the hello world example
	     uses: */
	  int(proc)(GtkWidget *d, gpointer data)) {
    g_signal_connect(window, "destroy",
		     G_CALLBACK(proc), NULL);
}

int
mainQuit (GtkWidget *window, gpointer data) {
    // what is mainQuit from haskell lib doing?
    exit(0); // well.
}

// -----


int
main ( int   argc,
       char *argv[] ) {
    GtkWidget *window;
    GtkWidget *drawing;
    gtk_init (&argc, &argv); // initGUI();
    window= gtk_window_new(GTK_WINDOW_TOPLEVEL);
    drawing= gtk_drawing_area_new();
    assert(window);
    assert(drawing);
    __HACK_drawing= drawing;
    gtk_window_set_title(window, "Mandelbrot");
    gtk_container_add (GTK_CONTAINER (window), drawing);
    {
	/* widgetModifyBg drawing StateNormal bg */
	GdkColor bg= {
	    0,
	    0,
	    0,
	    0
	};
	gtk_widget_modify_bg(drawing, GTK_STATE_NORMAL, &bg); 
    }
    onExpose(drawing, renderScene); /* onExpose drawing (renderScene drawing) */
    onDestroy(window, mainQuit); /* onDestroy window mainQuit */
    gtk_window_set_default_size(window, 800, 600); /* windowSetDefaultSize window 800 600 */
    gtk_window_set_position(window, GTK_WIN_POS_CENTER); /* windowSetPosition window WinPosCenter */
    /* widgetShowAll window */ //hmm don't have?
    gtk_widget_show(drawing);
    gtk_widget_show(window);
    gtk_main(); /* mainGUI */

    return 0;
}


