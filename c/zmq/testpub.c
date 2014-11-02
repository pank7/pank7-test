#include    "czmq.h"

int
main(int argc, char *argv[])
{
  zsys_set_logident("testpub");
  zsys_info("Create publisher");
  zsock_t       *p = zsock_new_pub("tcp://*:7777");
  if (!p) {
    zsys_error("Error creating publisher: %s",
               strerror(zmq_errno()));
    exit(EXIT_FAILURE);
  }
  int           sec = 10;
  if (argc > 1) sec = atoi(argv[1]);
  int           rc = 0, i = 0;
  for (i = 0; i < sec * 10; ++i) {
    rc = zstr_sendf(p, "test %s", argc > 1 ? argv[1] : "hoge");
    if (rc == -1) {
      zsys_error("Error publishing: %s",
                 strerror(zmq_errno()));
      break;
    }
    zclock_sleep(100);
  }

  zsys_info("Close publisher");
  zsock_destroy(&p);

  exit(EXIT_SUCCESS);
}
