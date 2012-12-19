#include        <stdio.h>
#include        "rbtree.h"

typedef struct
{
  int                   number;
  struct rb_node        hook;
} data_type;

data_type *
search(struct rb_root *t, int num);

data_type *
insert(struct rb_root *t, data_type *d);

data_type *
delete(struct rb_root *t, data_type *d);

int
main(int argc, char *argv[])
{
  int                   tmp = -1;
  struct rb_root        tree = RB_ROOT;
  struct rb_node        node = NULL;
  data_type             *data = NULL;
  FILE                  *fin = stdin;

  if (argc > 1) {
    fin = fopen(argv[1], "r");
  }
  if (fin == NULL) {
    perror(argv[1]);
    return 1;
  }

  while (fscanf(fin, "%d", &tmp) != EOF) {
    data = (data_type *)malloc(sizeof(data_type));
    RB_CLEAR_NODE(&data->hook);
    data->number = tmp;
    insert(tree, data);
  }

  fprintf(stdout, "search & delete:\n");
  while (fscanf(fin, "%d", &tmp) != EOF) {
    data = search(tree, tmp);
    if (data == NULL) {
      fprintf(stdout, "not found!\n");
    } else {
      fprintf(stdout, "found: %d!\n", data->number);
      data = delete(tree, data);
      free((void *)data);
    }
  }

  fprintf(stdout, "the rest:\n");
  node = rb_first(tree);
  while (node != NULL) {
    data = rb_entry(node, data_type, hook);
    fprintf(stdout, "%d\n", data->number);
  }

  while (!RB_EMPTY_ROOT(tree)) {
    node = (*tree)->rb_left;
    node
  }

  return 0;
}
