#ifndef __INT_NODE
#define __INT_NODE

#include        "list.h"
#include        "rbtree.h"

#ifdef  __cplusplus
extern "C" {
#endif

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

  typedef struct int_setnode_type {
    int                 val;
    struct rb_node      hook;
  } int_setnode_type;

  int_setnode_type *
  int_setnode_new()
  {
    int_setnode_type    *isn = NULL;

    isn = (int_setnode_type *)malloc(sizeof(int_setnode_type));
    isn->val = 0;
    RB_CLEAR_NODE(&isn->hook);

    return isn;
  }

  void
  int_setnode_del(int_setnode_type *isn)
  {
    free((void *)isn);
  }

#endif  /* __INT_NODE */
