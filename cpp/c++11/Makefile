CC = gcc-4.8
CPP = g++-4.8
CFLAGS = -std=c++11 -Wall -c
OFLAGS =
TARGETS = test.out

all: ${TARGETS}

%.o: %.c
	${CC} ${CFLAGS} -o $@ $^

%.o: %.cpp
	${CPP} ${CFLAGS} -o $@ $^

test.out: test.o
	${CPP} ${OFLAGS} -o $@ $^

clean:
	rm -f *.o
	rm ${TARGETS}
