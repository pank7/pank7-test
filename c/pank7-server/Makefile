TARGETS 	= pank7-server.out rot13-select.out rot13-libevent.out \
		  rot13-libevent+.out test-libev.out pank7-svc.out
OBJECTS		= pank7-server.o rot13-select.o rot13-libevent.o rot13-libevent+.o \
		  test-libev.o pank7-svc.o

CC		= clang
INCPATHS	= -I../../http-parser
LIBPATHS	= -L../../http-parser
LINKLIBS	= -lev -lhttp_parser
#CFLAGS		= -Wall -O3 -g -c
CFLAGS		= ${INCPATHS} -Wall -g -c
OFLAGS		= ${LIBPATHS}
AWK		= gawk

%.o: %.c
	${CC} ${CFLAGS} $^ -o $@

all: ${TARGETS}

clean:
	rm -rf ${TARGETS} ${OBJECTS}

tags:
	ctags -R

pank7-svc.out: pank7-svc.o
	${CC} ${OFLAGS} -o $@ $^ -lev -lhttp_parser

test-libev.out: test-libev.o
	${CC} ${OFLAGS} -lev $^ -o $@

pank7-server.out: pank7-server.o
	${CC} ${OFLAGS} -levent $^ -o $@

rot13-select.out: rot13-select.o
	${CC} ${OFLAGS} $^ -o $@

rot13-libevent.out: rot13-libevent.o
	${CC} ${OFLAGS} -levent $^ -o $@

rot13-libevent+.out: rot13-libevent+.o
	${CC} ${OFLAGS} -levent $^ -o $@
