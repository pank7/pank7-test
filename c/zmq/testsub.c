#include    "czmq.h"

int
main(int argc, char *argv[])
{
  zsys_set_logident(argc > 1 ? argv[1] : "testsub");
  zsys_info("Create subscriber: %s", argc > 1 ? argv[1] : "testsub");
  zsock_t       *s = zsock_new_sub("tcp://localhost:7777", "test");
  // zsock_t       *s = zsock_new_sub("epgm://en1;239.192.1.1:7777", "test");
  // zsock_t       *s = zsock_new(ZMQ_SUB);
  // zsock_connect(s, "epgm://en1;239.192.1.1:7777");
  if (!s) {
    zsys_error("Error creating subscriber: %s",
               strerror(zmq_errno()));
    exit(EXIT_FAILURE);
  }
  zsock_set_subscribe(s, "test");

  unsigned int  i = 0;
  char          *r = NULL;
  while(true) {
    r = zstr_recv(s);
    if (!r) {
      zsys_info("Interrupted");
      break;
    }
    zsys_info("Subscriber received(%u): %s", ++i, r);
    zstr_free(&r);
  }

  zsys_info("Close subscriber");
  zsock_destroy(&s);

  exit(EXIT_SUCCESS);
}
