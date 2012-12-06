#include        <stdio.h>
#include        <stdlib.h>
#include        "list.h"

#define         BUFFSIZE        1024

typedef struct
{
  int           number;
  list_head     hook;
} data_type;

int
main(int argc, char *argv[])
{
  list_head     *head = NULL;
  char          tmp[BUFFSIZE];

  while (fgets(tmp, BUFFSIZE, stdin)) {
    fprintf(stdout, "%s", tmp);
  }

  return 0;
}
