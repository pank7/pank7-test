#include    "czmq.h"

int
main(int argc, char *argv[])
{
  zsys_set_logident("test");

  ziflist_t     *iflist = ziflist_new();
  const char    *i = ziflist_first(iflist);

  zsys_info("Interfaces on this machine:");
  while (i) {
    zsys_info("  %s", i);
    i = ziflist_next(iflist);
  }

  ziflist_destroy(&iflist);

  exit(EXIT_SUCCESS);
}
