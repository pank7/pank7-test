TARGETS 	= pank7-server rot13-select rot13-libevent rot13-libevent+ \
		  test-libev
OBJECTS		= pank7-server.o rot13-select.o rot13-libevent.o rot13-libevent+.o \
		  test-libev.o

CC		= clang
#CFLAGS		= -Wall -O3 -g -c
CFLAGS		= -Wall -g -c
OFLAGS		=
AWK		= gawk

%.o: %.c
	${CC} ${CFLAGS} $^ -o $@

all: ${TARGETS}

clean:
	rm -rf ${TARGETS} ${OBJECTS}

tags:
	ctags -R

test-libev: test-libev.o
	${CC} ${OFLAGS} -lev $^ -o $@

pank7-server: pank7-server.o
	${CC} ${OFLAGS} -levent $^ -o $@

rot13-select: rot13-select.o
	${CC} ${OFLAGS} $^ -o $@

rot13-libevent: rot13-libevent.o
	${CC} ${OFLAGS} -levent $^ -o $@

rot13-libevent+: rot13-libevent+.o
	${CC} ${OFLAGS} -levent $^ -o $@
