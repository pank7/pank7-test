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
#define MAX_LINE                16384

struct pank7_server_settings
{
  char                  name[SHORT_STRING_LENGTH];
#define FALSE   0
#define TRUE    1
  short int             libevent_debug_mode;
  unsigned int          thread_num;
  struct event_base     *base;
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
  st->base = NULL;

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
  event_base_dump_events(base, stdout);
  event_base_free(base);

  return;
}

int
parse_args(struct pank7_server_settings *st, int argc, char *argv[])
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

void
pank7_server_exit_callback()
{
  
}

void
pank7_server_error_callback(struct bufferevent *bev, short error, void *ctx)
{
    if (error & BEV_EVENT_EOF) {
        /* connection has been closed, do any clean up here */
        /* ... */
    } else if (error & BEV_EVENT_ERROR) {
        /* check errno to see what error occurred */
        /* ... */
    } else if (error & BEV_EVENT_TIMEOUT) {
        /* must be a timeout event handle, handle it */
        /* ... */
    }
    bufferevent_free(bev);
}

char
rot13_char(char c)
{
    /* We don't want to use isalpha here; setting the locale would change
     * which characters are considered alphabetical. */
    if ((c >= 'a' && c <= 'm') || (c >= 'A' && c <= 'M'))
        return c + 13;
    else if ((c >= 'n' && c <= 'z') || (c >= 'N' && c <= 'Z'))
        return c - 13;
    else
        return c;
}

void
pank7_server_read_callback(struct bufferevent *bev, void *ctx)
{
  struct evbuffer       *input, *output;
  char                  *line;
  size_t                n;
  int                   i;
  input = bufferevent_get_input(bev);
  output = bufferevent_get_output(bev);

  while ((line = evbuffer_readln(input, &n, EVBUFFER_EOL_LF))) {
    for (i = 0; i < n; ++i) line[i] = rot13_char(line[i]);
    evbuffer_add(output, line, n);
    evbuffer_add(output, "\n", 1);
    free(line);
  }

  if (evbuffer_get_length(input) >= MAX_LINE) {
    /* Too long; just process what there is and go on so that the buffer
     * doesn't grow infinitely long. */
    char buf[1024];
    while (evbuffer_get_length(input)) {
      int n = evbuffer_remove(input, buf, sizeof(buf));
      for (i = 0; i < n; ++i)
        buf[i] = rot13_char(buf[i]);
      evbuffer_add(output, buf, n);
    }
    evbuffer_add(output, "\n", 1);
  }
}

void
pank7_server_accept_callback(evutil_socket_t listener, short event, void *arg)
{
  struct pank7_server_settings  *st = arg;
  struct event_base             *base = st->base;
  struct sockaddr_storage       ss;
  socklen_t                     slen = sizeof(ss);
  int                           fd;
  fd = accept(listener, (struct sockaddr*)&ss, &slen);
  if (fd < 0) {
    perror("accept");
  } else if (fd > FD_SETSIZE) {
    close(fd);
  } else {
    struct bufferevent *bev;
    evutil_make_socket_nonblocking(fd);
    bev = bufferevent_socket_new(base, fd, BEV_OPT_CLOSE_ON_FREE);
    bufferevent_setcb(bev, pank7_server_read_callback, NULL,
                      pank7_server_error_callback, NULL);
    bufferevent_setwatermark(bev, EV_READ, 0, MAX_LINE);
    bufferevent_enable(bev, EV_READ|EV_WRITE);
  }
}

int
pank7_listener_event_init(struct pank7_server_settings *st,
                          struct event **listener_event)
{
  evutil_socket_t       listener;
  struct sockaddr_in    sin;

  sin.sin_family = AF_INET;
  sin.sin_addr.s_addr = 0;
  sin.sin_port = htons(7777);

  listener = socket(AF_INET, SOCK_STREAM, 0);
  evutil_make_socket_nonblocking(listener);

  if (bind(listener, (struct sockaddr *)&sin, sizeof(sin)) < 0) {
    /* error message */
    perror("bind");
    return 1;
  }

  if (listen(listener, 16) < 0) {
    /* error message */
    perror("listen");
    return 1;
  }

  *listener_event = event_new(st->base, listener, EV_READ | EV_PERSIST,
                              pank7_server_accept_callback, (void *)st);

  return 0;
}

int
pank7_server_init(struct pank7_server_settings *st)
{
  /* debug mode? */
  if (st->libevent_debug_mode == TRUE) event_enable_debug_mode();

  /* event_config & event_base */
  struct event_config   *config = event_config_new();
  event_config_require_features(config, EV_FEATURE_ET | EV_FEATURE_O1 | EV_FEATURE_FDS);
  st->base = event_base_new_with_config(config);
  event_config_free(config);

  /* listener event */
  struct event          *listener_event;
  if (pank7_listener_event_init(st, &listener_event) != 0) {
    /* error message here */
    return 1;
  }
  event_add(listener_event, NULL);
  
  return 0;
}

void
pank7_server_run(struct pank7_server_settings *st)
{
  event_base_dump_events(st->base, stdout);
  event_base_dispatch(st->base);

  return;
}

int
main(int argc, char *argv[], char *env[])
{
  struct pank7_server_settings          st;

  default_pank7_server_settings(&st);

  if (parse_args(&st, argc, argv) != 0) {
    print_help(argc, argv);
    return 1;
  }

  pank7_server_init(&st);

  pank7_server_run(&st);
          
  return 0;
}
