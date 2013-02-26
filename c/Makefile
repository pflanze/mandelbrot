
CC=gcc
CFLAGS=-Wall -g -pthread `pkg-config --cflags gtk+-2.0`
LDFLAGS=`pkg-config --libs gtk+-2.0` -lrt
#LD=gcc  hm how to specify cc for linking?

objects= nstime.o mandelbrot.o main.o


all: main


nstime.o: nstime.c nstime.h

mandelbrot.o: mandelbrot.c mandelbrot.h nstime.h

main.o: main.c mandelbrot.h


main: $(objects)
	cc -o main $(objects) $(LDFLAGS)

#	gcc -Wall -g -o main main.c $(CCFLAGS) $(LDFLAGS)
#	gcc -Wall -g -O3 -o opt main.c $(CCFLAGS) $(LDFLAGS)
# TODO: how to compile different obj files?
opt: $(objects)
	cc -o opt $(objects) $(LDFLAGS)

run: opt
	bash -c 'time ./opt'


.PHONY: clean
clean:
	rm -f main opt $(objects)






