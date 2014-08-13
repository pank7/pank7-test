.PHONY: all clean test

GHC = ghc
GHCFLAGS = 
TARGETS = Main

all: ${TARGETS}

Main: Main.hs
	${GHC} ${GHCFLAGS} $^ -o $@

clean:
	-rm -rf *.o *.hi ${TARGETS}
