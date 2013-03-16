ERLC = erlc

.PHONY: all clean

all: tut.beam useless.beam

clean:
	-rm *.beam

%.beam: %.erl
	${ERLC} $^

