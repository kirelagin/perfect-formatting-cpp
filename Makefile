Cppf.hs : Cppf.y
	happy Cppf.y -o $@

.PHONY : test
test : Test.hs Cppf.hs
	runhaskell Test.hs
