#include        <stdlib.h>
#include        "int_set.h"
#include        "int_node.h"

int_set_type *
int_set_new()
{
  int_set_type  *is = NULL;

  is = (int_set_type *)malloc(sizeof(int_set_type));
  is->size = 0;
  is->head = RB_ROOT;

  return is;
}

void
int_set_del(int_set_type *is)
{
  struct rb_node        *node = NULL;
  int_setnode_type      *isn = NULL;

  while (!RB_EMPTY_ROOT(&is->head)) {
    node = is->head.rb_node;
    isn = rb_entry(node, int_setnode_type, hook);
    rb_erase(node, &is->head);
    int_setnode_del(isn);
  }

  free((void *)is);
}

int
int_set_empty(int_set_type *is)
{
  return RB_EMPTY_ROOT(&is->head);
}

static int_setnode_type *
__int_set_search(int_set_type *is, int val)
{
  struct rb_node        *node = is->head.rb_node;
  int_setnode_type      *data = NULL;

  while (node) {
    data = rb_entry(node, int_setnode_type, hook);
    if (val < data->val) {
      node = node->rb_left;
    } else if (val > data->val){
      node = node->rb_right;
    } else {
      return data;
    }
  }

  return NULL;
}

int
int_set_search(int_set_type *is, int val)
{
  int_setnode_type      *data = NULL;

  data = __int_set_search(is, val);

  return data != NULL;
}

int
int_set_insert(int_set_type *is, int val)
{
  struct rb_node        **node = &is->head.rb_node;
  struct rb_node        *parent = NULL;
  int_setnode_type      *data = NULL;

  while (*node) {
    parent = *node;
    data = rb_entry(parent, int_setnode_type, hook);
    if (val < data->val) {
      node = &parent->rb_left;
    } else if (val > data->val) {
      node = &parent->rb_right;
    } else {
      return 0;
    }
  }
  rb_link_node(&data->hook, parent, node);
  rb_insert_color(&data->hook, &is->head);
  ++is->size;

  return 1;
}

int
int_set_remove(int_set_type *is, int val)
{
  int_setnode_type      *data = NULL;

  data = __int_set_search(is, val);
  if (data == NULL) return 0;
  rb_erase(&data->hook, &is->head);
  --is->size;

  return 1;
}

