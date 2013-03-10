run: all
	bash -c 'time ./main +RTS -N4 -s'

all:
	ghc --make -Odph -rtsopts -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 main.hs
