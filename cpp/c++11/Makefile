.PHONY: all clean

CC = clang
CXX = clang++
CFLAGS = -Wall -g -c
CXXFLAGS = -std=c++11 -Wall -g -c
OFLAGS =  -O0
OBJECTS =
TARGETS = test.out test2.out test3.out test4.out test5.out \
	  test6.out test7.out test8.out test9.out test10.out \
	  test11.out test12.out

all: ${TARGETS}

%.o: %.c
	${CC} ${CFLAGS} -o $@ $^

%.o: %.cpp
	${CXX} ${CXXFLAGS} -o $@ $^

%.out: %.o
	${CXX} ${OFLAGS} -o $@ $^

test6.out: test6.o
	${CXX} ${OFLAGS} -lpthread -o $@ $^

test11.out: test11.o
	${CXX} ${OFLAGS} -lssl -lcrypto -o $@ $^

clean:
	rm -f *.o ${OBJECTS}
	rm -f ${TARGETS}
