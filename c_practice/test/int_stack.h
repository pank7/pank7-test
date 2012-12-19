#include        "list.h"

#ifdef  __cplusplus
extern "C" {
#endif

  typedef struct int_stack_type {
    int         size;
    list_head   head;
  } int_stack_type;

  int_stack_type *
  int_stack_new();

  void
  int_stack_del(int_stack_type *is);

  int
  int_stack_empty(int_stack_type *is);

  int
  int_stack_push(int_stack_type *is, int val);

  int
  int_stack_pop(int_stack_type *is);
#ifdef  __cplusplus
}
#endif
