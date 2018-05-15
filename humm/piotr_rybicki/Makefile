export PATH := /home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin:$(PATH)
DEPS = Main.hs StaticCheck.hs Common.hs

all: sandbox interpreter

sandbox:
		cabal sandbox init
		cabal install --only-dependencies

interpreter: $(DEPS)
		cabal build
		cp ./dist/build/interpreter/interpreter ./interpreter

clean:
		cabal clean
		cabal sandbox delete
		rm ./interpreter
