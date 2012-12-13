TARGETS 	= test_list.out test_rbtree.out test_list_rbtree.out \
		hello_world.out
OBJECTS		= rbtree.o test_list.o test_rbtree.o

CC		= clang
#CFLAGS		= -Wall -O3 -c
CFLAGS		= -Wall -g -c
OFLAGS		=
AWK		= gawk

%.o: %.c
	${CC} ${CFLAGS} $^ -o $@

all: ${TARGETS}

clean:
	rm -rf ${TARGETS} ${OBJECTS} *.o

tags:
	ctags -R

hello_world.out: hello_world.o
	${CC} ${OFLAGS} $^ -o $@

test_list.out: test_list.o
	${CC} ${OFLAGS} $^ -o $@

run_test_list: test_list.out
	${AWK} 'BEGIN{srand(systime()); for (i = 0; i < 5; ++i) print int(1000 * rand())}' | ./$^

test_rbtree.out: test_rbtree.o rbtree.o
	${CC} ${OFLAGS} $^ -o $@

test_list_rbtree.out: test_list_rbtree.o rbtree.o
	${CC} ${OFLAGS} $^ -o $@
