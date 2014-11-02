.PHONY: all clean tags

TARGETS 	= test.out testpub.out testsub.out

HOMEPATH = $(shell cd; pwd)

CC		= gcc
INCPATHS	= -I. -I${HOMEPATH}/include -I/usr/local/include
LIBPATHS	= -L. -L${HOMEPATH}/lib -L/usr/local/lib
LINKLIBS	= -lzmq -lczmq -lpthread
#CFLAGS		= -Wall -O3 -g -c
CFLAGS		= ${INCPATHS} -Wall -g -c
OFLAGS		= ${LIBPATHS} -Wl,-rpath -Wl,.:${HOMEPATH}/lib:/usr/local/lib ${LINKLIBS}
AWK		= gawk

all: ${TARGETS}

%.o: %.c
	${CC} ${CFLAGS} $^ -o $@

%.out: %.o
	${CC} ${OFLAGS} $^ -o $@

clean:
	rm -rf ${TARGETS}

tags:
	ctags -R
