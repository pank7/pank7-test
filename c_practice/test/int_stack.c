#include        <stdlib.h>
#include        "int_stack.h"
#include        "int_node.h"

int_stack_type *
int_stack_new()
{
  int_stack_type        *is = NULL;

  is = (int_stack_type *)malloc(sizeof(int_stack_type));
  is->size = 0;
  INIT_LIST_HEAD(&is->head);

  return is;
}

void
int_stack_del(int_stack_type *is)
{
  int_node_type         *in = NULL, *n = NULL;

  list_for_each_entry_safe(in, n, &is->head, hook) {
    list_del(&in->hook);
    int_node_del((void *)in);
  }

  free((void *)is);
}

int
int_stack_empty(int_stack_type *is)
{
  return list_empty(&is->head);
}

int
int_stack_head(int_stack_type *is)
{
  int_node_type         *in = NULL;

  in = list_entry(is->head.next, int_node_type, hook);

  return in->val;
}

int
int_stack_push(int_stack_type *is, int val)
{
  int_node_type         *in = NULL;

  in = int_node_new();
  in->val = val;
  list_add(&in->hook, &is->head);
  ++is->size;

  return val;
}

int
int_stack_pop(int_stack_type *is)
{
  int_node_type         *in = NULL;
  int                   val;

  in = list_entry(is->head.next, int_node_type, hook);
  val = in->val;
  list_del(is->head.next);
  int_node_del(in);
  --is->size;

  return val;
}

