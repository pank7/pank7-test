#include	<stdio.h>
#include	<string.h>
#include	<ctype.h>

int
main (int argc, char *argv[])
{
  if (argc < 3) {
    fprintf (stderr, "WHAT?!\n");
    return -1;
  }
  int           i = 0, j = 0, len = strlen (argv[1]);
  unsigned int  array;

  if ((len < 1) || (len > 8)) {
    fprintf (stderr, "Invalid bit array!\n");
    fprintf (stdout, "00000000\n");
    return -1;
  }

  for (i = 0; i < 8; ++i) {
    if (!isxdigit (argv[1][i])) {
      fprintf (stderr, "Invalid bit array!\n");
      fprintf (stdout, "00000000\n");
      return -1;
    }
  }
  sscanf (argv[1], "%x", &array);
  sscanf (argv[2], "%d", &i);
  array = (array & (0x00000001 << (i - 1)));

  fprintf (stdout, "%08X\n", array);

  return 0;
}
