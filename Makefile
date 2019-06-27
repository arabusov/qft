all:
	ghc test.hs -o test
love:
	@echo Not war
clean:
	rm -rf *.o *.hi
	rm -rf doc

doc:
	mkdir -p doc
	haddock --html -o doc Lorentz.hs
