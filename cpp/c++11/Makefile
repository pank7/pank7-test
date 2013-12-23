.PHONY: all clean

CC = gcc-4.8
CPP = g++-4.8
CFLAGS = -std=c++11 -Wall -g -c
OFLAGS =
OBJECTS =
TARGETS = test.out test2.out test3.out

all: ${TARGETS}

%.o: %.c
	${CC} ${CFLAGS} -o $@ $^

%.o: %.cpp
	${CPP} ${CFLAGS} -o $@ $^

%.out: %.o
	${CPP} ${OFLAGS} -o $@ $^

clean:
	rm -f *.o ${OBJECTS}
	rm -f ${TARGETS}
