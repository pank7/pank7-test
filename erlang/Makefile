TARGETS	= tut.beam useless.beam fac.beam len.beam fib.beam \
	  recursive.beam tree.beam road.beam

ERLC 	= erlc

.PHONY: all clean

all: ${TARGETS}

clean:
	-rm *.beam

%.beam: %.erl
	${ERLC} $^

run_fib: fib.beam
	erl -noshell -run fib main 300
