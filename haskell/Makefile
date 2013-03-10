run: main
	bash -c 'time ./main +RTS -N4 -s'

main: main.hs Makefile
	ghc --make -Odph -rtsopts -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 main.hs
