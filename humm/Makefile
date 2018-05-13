all:
	happy -gca ParLatte.y
	alex -g LexLatte.x
	ghc --make TestLatte.hs -o TestLatte
	ghc Interpreter.hs -o interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

