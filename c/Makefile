
CCFLAGS=-pthread `pkg-config --cflags gtk+-2.0`
LDFLAGS=`pkg-config --libs gtk+-2.0` -lrt


main: main.c nstime.c
	gcc -Wall -g -o main main.c $(CCFLAGS) $(LDFLAGS)

opt: main.c nstime.c
	gcc -Wall -g -O3 -o opt main.c $(CCFLAGS) $(LDFLAGS)

clean:
	rm -f main opt

run: opt
	bash -c 'time ./opt'
