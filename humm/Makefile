all:
	bnfc -haskell latte.cf
	happy -gca ParLatte.y
	alex -g LexLatte.x
	ghc Interpreter.hs -o interpreter


clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f AbsLatte.hs DocLatte.txt ErrM.hs LexLatte.hs LexLatte.x ParLatte.hs ParLatte.y PrintLatte.hs SkelLatte.hs TestLatte.hs interpreter

