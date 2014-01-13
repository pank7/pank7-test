#include        <stdlib.h>
#include        <unistd.h>
#include        <stdio.h>
#include        <string.h>

/* For sockaddr_in */
#include        <netinet/in.h>
/* For socket functions */
#include        <sys/socket.h>
/* For fcntl */
#include        <fcntl.h>

#include        <event2/event.h>
#include        <event2/buffer.h>
#include        <event2/bufferevent.h>
#include        <event2/thread.h>

#define SHORT_STRING_LENGTH     256

struct pank7_server_settings
{
  char                  name[SHORT_STRING_LENGTH];
#define FALSE   0
#define TRUE    1
  short int             libevent_debug_mode;
  unsigned int          thread_num;
};

int
check_version_match(void)
{
  ev_uint32_t   v_compile, v_run;
  v_compile = LIBEVENT_VERSION_NUMBER;
  v_run = event_get_version_number();
  if ((v_compile & 0xffff0000) != (v_run & 0xffff0000)) {
    printf("Running with a Libevent version (%s) very different from the "
           "one we were built with (%s).\n", event_get_version(),
           LIBEVENT_VERSION);
    return -1;
  }
  return 0;
}

void
default_pank7_server_settings(struct pank7_server_settings *st)
{
  char          default_name[] = "pank7-server";
  long          nc = sysconf(_SC_NPROCESSORS_ONLN);

  strcpy(st->name, default_name);
#ifdef _DEBUG
  st->libevent_debug_mode = TRUE;
#else  /* _DEBUG */
  st->libevent_debug_mode = FALSE;
#endif /* _DEBUG */
  st->thread_num = nc;

  return;
}

void
print_help(int argc, char *argv[])
{
  fprintf(stderr, "usage: %s [-hi]\n", argv[0]);
  fprintf(stderr, "\th\tprint this help\n");
  fprintf(stderr, "\ti\tprint some system information\n");

  return;
}

void
print_sys_info(int argc, char *argv[])
{
  int           i;
  const char    **methods = event_get_supported_methods();
  fprintf(stdout, "Libevent version: %s\n", event_get_version());
  fprintf(stdout, "Available libevent methods are:");
  for (i = 0; methods[i] != NULL; ++i) {
    printf(" %s", methods[i]);
  }  
  fprintf(stdout, "\n");
  long          nc = sysconf(_SC_NPROCESSORS_ONLN);
  fprintf(stdout, "Number of CPU core(s): %ld\n", nc);
  check_version_match();

  struct event_base *base;
  enum event_method_feature f;

  base = event_base_new();
  if (!base) {
    puts("Couldn't get an event_base!");
  } else {
    printf("Using Libevent with backend method %s.",
           event_base_get_method(base));
    f = event_base_get_features(base);
    if ((f & EV_FEATURE_ET))
      printf("  Edge-triggered events are supported.");
    if ((f & EV_FEATURE_O1))
      printf("  O(1) event notification is supported.");
    if ((f & EV_FEATURE_FDS))
      printf("  All FD types are supported.");
    puts("");
  }
  event_base_free(base);

  return;
}

int
parse_args(int argc, char *argv[])
{
  int           ch;

  while ((ch = getopt(argc, argv, "hid")) != -1) {
    switch (ch) {
    case 'h':
      print_help(argc, argv);
      exit(0);
      break;
    case 'i':
      print_sys_info(argc, argv);
      exit(0);
      break;
    case 'd':
      break;
    default:
      return 1;
    }
  }

  return 0;
}

int
pank7_server_init(struct pank7_server_settings *st)
{
  if (st->libevent_debug_mode == TRUE) event_enable_debug_mode();

  struct event_config   *config = event_config_new();
  event_config_require_features(config, EV_FEATURE_ET | EV_FEATURE_O1 | EV_FEATURE_FDS);
  event_config_free(config);

  return 0;
}

void
pank7_server_run(struct pank7_server_settings *st)
{
  return;
}

int
main(int argc, char *argv[], char *env[])
{
  struct pank7_server_settings          st;

  default_pank7_server_settings(&st);

  if (parse_args(argc, argv) != 0) {
    print_help(argc, argv);
    return 1;
  }

  pank7_server_init(&st);

  pank7_server_run(&st);
          
  return 0;
}
