#include <assert.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#define STATIC static

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

#include "mandelbrot.h"

/* HACK to make it compile on Debian Lenny; XXX: does that override
   the original gtk_widget_get_window in all cases now? */
#ifndef gtk_widget_get_window
#define gtk_widget_get_window(d) (d)->window
#endif


STATIC void
pb_get_context(struct pb_context* ctx, GdkPixbuf *pb) {
    ctx->pixels= gdk_pixbuf_get_pixels(pb);
    ctx->rowstride= gdk_pixbuf_get_rowstride(pb);
    ctx->nChannels= gdk_pixbuf_get_n_channels(pb);
}


STATIC double xcenter= 0.0;
STATIC double ycenter= 0.0;
STATIC double size= 2.0;
STATIC int depth= 200;


STATIC int
renderScene (GtkWidget *d, GdkEventExpose *ev, gpointer data) {
    GtkWidget *dw= gtk_widget_get_window(d);
    gint w,h;
    assert(dw);
    //gtk_window_get_size(dw, &w, &h); // XXX should be d ?
    //gtk_widget_size_request(d, &w, &h);
    /* gtk_window_get_size(dw, &w, &h);//UWHYNOT */
    
    /* // what is gc for, if never changed???      gc     <- gcNew dw */
    /* //-- pixbuf */
    /* assert(w>0); */
    /* assert(h>0); */
    /* // WTH. 256 times bigger than anticipated  wth. exact. wth.wll. */
    /* w= w/256; */
    /* h= h/256; */
    /* assert(w>0); */
    /* assert(h>0); */
    /* // AND W T HELL  swapped   why   doc is as above  wt . */
    /* { */
    /* 	gint tmp=w; */
    /* 	w=h; */
    /* 	h=tmp; */
    /* } */
	 //XXX hard coded size, get from window instead.
    w=1246;
    h=998;
	
    {
	GdkPixbuf *pb= gdk_pixbuf_new(GDK_COLORSPACE_RGB, 0, 8, w, h);
	assert(pb);
	{
	    struct pb_context ctx;
	    pb_get_context(&ctx, pb);
	    mandelbrot_render(&ctx, w, h, xcenter, ycenter, size, depth);
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
    gtk_main_quit();
    return 0; //?
}

// -----


int
main ( int   argc,
       char *argv[] ) {
    gtk_init (&argc, &argv); // initGUI();

    if (argc==1) {
	/* leave at defaults */
    } else if (argc==5) {
# define CHK(expr) if ((expr)!=1) { goto usage; }
	CHK(sscanf(argv[1],"%lf",&xcenter));
	CHK(sscanf(argv[2],"%lf",&ycenter));
	CHK(sscanf(argv[3],"%lf",&size));
	CHK(sscanf(argv[4],"%d",&depth));
    } else {
    usage:
	printf("usage: %s [xcenter ycenter size depth]\n", argv[0]);
	return 1;
    }
    printf("xcenter=%lf\n",xcenter);
    printf("ycenter=%lf\n",ycenter);
    printf("   size=%lf\n",size);
    printf("  depth=%d\n",depth);
    {
	GtkWidget *window= gtk_window_new(GTK_WINDOW_TOPLEVEL);
	GtkWidget *drawing= gtk_drawing_area_new();
	assert(window);
	assert(drawing);
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
    }
    return 0;
}


