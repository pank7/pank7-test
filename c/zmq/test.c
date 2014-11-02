#include    "czmq.h"

int
main(int argc, char *argv[])
{
  zsys_set_logident("test");
  zsys_info("test");

  exit(EXIT_SUCCESS);
}
