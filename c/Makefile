
default: .make/Makefile
	make -C .make run

opt: .opt/Makefile
	make -C .opt run

all: opt default

.make/Makefile: genmake
	./genmake .make ""

.opt/Makefile: genmake
	./genmake .opt "-O3 "

