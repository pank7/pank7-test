#ifndef __INT_NODE
#define __INT_NODE

#include        "list.h"

typedef struct int_node_type {
  int           val;
  list_head     hook;
} int_node_type;

int_node_type *
int_node_new()
{
  int_node_type         *in = NULL;

  in = (int_node_type *)malloc(sizeof(int_node_type));
  in->val = 0;
  INIT_LIST_HEAD(&in->hook);

  return in;
}

void
int_node_del(int_node_type *in)
{
  free((void *)in);
}

#endif  /* __INT_NODE */
