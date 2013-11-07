
default: .make/Makefile
	make -C .make

run: .make/Makefile
	make -C .make run

rungdb: .make/Makefile
	make -C .make rungdb

opt: .opt/Makefile
	make -C .opt

optrun: .opt/Makefile
	make -C .opt run

all: opt default

clean:
	make -C .make clean || true
	make -C .opt clean || true
	rm -f .make/Makefile .opt/Makefile

.make/Makefile: genmake Makefile
	./genmake .make ""

.opt/Makefile: genmake Makefile
	./genmake .opt "-O3 -mtune=core2 -fomit-frame-pointer -funroll-loops " " "

