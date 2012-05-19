cppf : Main.hs Cppf.hs
	ghc $^ -o $@

Cppf.hs : Cppf.y
	happy -gac $^ -o $@

.PHONY : test
test : Test.hs Cppf.hs
	runhaskell Test.hs

.PHONY: clean
clean :
	rm -rf *.o *.hi cppf Cppf.hs
