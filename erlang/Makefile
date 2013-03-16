TARGETS	= tut.beam useless.beam fac.beam len.beam fib.beam \
	  recursive.beam tree.beam

ERLC 	= erlc

.PHONY: all clean

all: ${TARGETS}

clean:
	-rm *.beam

%.beam: %.erl
	${ERLC} $^

