
CCFLAGS=-pthread `pkg-config --cflags gtk+-2.0` `pkg-config --libs gtk+-2.0`

main: main.c
	gcc -Wall -g -o main main.c $(CCFLAGS)

opt: main.c
	gcc -Wall -g -O3 -o opt main.c $(CCFLAGS)

clean:
	rm -f main opt

run: opt
	bash -c 'time ./opt'
