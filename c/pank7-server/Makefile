TARGETS 	= pank7-server rot13-select rot13-libevent rot13-libevent+
OBJECTS		= pank7-server.o rot13-select.o rot13-libevent.o rot13-libevent+.o

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

pank7-server: pank7-server.o
	${CC} ${OFLAGS} -levent $^ -o $@

rot13-select: rot13-select.o
	${CC} ${OFLAGS} $^ -o $@

rot13-libevent: rot13-libevent.o
	${CC} ${OFLAGS} -levent $^ -o $@

rot13-libevent+: rot13-libevent+.o
	${CC} ${OFLAGS} -levent $^ -o $@
