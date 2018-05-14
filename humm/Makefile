all:
	bnfc -haskell latte.cf
	happy -gca ParLatte.y
	alex -g LexLatte.x
	ghc Interpreter.hs -o interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

