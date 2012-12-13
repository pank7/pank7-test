#include        <stdio.h>
#include        <stdlib.h>
#include        <string.h>
#include        <errno.h>
#include        "list.h"

#define         BUFFSIZE        1024

typedef struct
{
  int           number;
  char          name[BUFFSIZE];
  list_head     hook;
} data_type;

int
main(int argc, char *argv[])
{
  char          tmp[BUFFSIZE];
  FILE          *fin = stdin;

  if (argc > 1) {
    fin = fopen(argv[1], "r");
  }
  if (fin == NULL) {
    perror(argv[1]);
    exit(1);
  }

  while (fgets(tmp, BUFFSIZE, fin)) {
    fprintf(stdout, "%4ld: %s", strlen(tmp), tmp);
  }

  fclose(fin);

  return 0;
}
