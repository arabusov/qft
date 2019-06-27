all:
	ghc test.hs -o test
love:
	@echo Not war
clean:
	rm -rf *.o *.hi
