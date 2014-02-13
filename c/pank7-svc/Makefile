TARGETS 	= pank7-svc.out
OBJECTS		= pank7-svc.o

CC		= clang
INCPATHS	= -I../../http-parser
LIBPATHS	= -L../../http-parser
LINKLIBS	= -lev -lhttp_parser -lpthread
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
	${CC} ${OFLAGS} -o $@ $^ -lev -lhttp_parser -lpthread
