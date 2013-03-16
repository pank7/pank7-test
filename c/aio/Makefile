TARGETS		= test_aio.out

CC		= clang
#CFLAGS		= -Wall -O3 -c
CFLAGS		= -Wall -g -c
OFLAGS		= -L.
AR		= ar
RANLIB		= ranlib
AWK		= gawk

%.o: %.c
	${CC} ${CFLAGS} $^ -o $@

all: ${TARGETS}

clean:
	rm -rf ${TARGETS} *.o

tags:
	ctags -R

test_aio.out: test_aio.o
	${CC} ${OFLAGS} $^ -o $@
