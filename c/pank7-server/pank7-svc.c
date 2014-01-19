#include        <stdlib.h>
#include        <unistd.h>
#include        <signal.h>
#include        <stdio.h>
#include        <string.h>
#include        <stdbool.h>

#include        <fcntl.h>
#include        <errno.h>

#include        <sys/types.h>
#include        <sys/stat.h>

/* For sockaddr_in */
#include        <netinet/in.h>
/* For socket functions */
#include        <sys/socket.h>
#include        <netdb.h>

#include        <ev.h>

#define SHORT_STRING_LENGTH     256
#define MEDIUM_STRING_LENGTH    1024

struct pank7_connection
{
  int                   fd;
  ev_io                 watcher;
};

struct pank7_server_settings
{
  char                  name[SHORT_STRING_LENGTH];
  bool                  daemon_mode;
  bool                  debug_mode;
  bool                  udp_listen_on;
  unsigned int          thread_num;
  char                  fifo_path[MEDIUM_STRING_LENGTH];
  struct ev_loop        *loop;
  char                  listen_host[SHORT_STRING_LENGTH];
  in_port_t             listen_port;
  int                   listen_socket;
  ev_io                 listen_watcher;
  ev_tstamp             period;
  ev_periodic           period_watcher;
  ev_signal             sigterm_watcher;
  ev_signal             sigquit_watcher;
  ev_signal             sigint_watcher;
  
};

static struct pank7_server_settings     settings;

void
default_pank7_server_settings(struct pank7_server_settings *st)
{
  char          default_name[] = "pank7-server";
  long          nc = sysconf(_SC_NPROCESSORS_ONLN);

  strcpy(st->name, default_name);
#ifdef _DEBUG
  st->debug_mode = true;
#else  /* _DEBUG */
  st->debug_mode = false;
#endif /* _DEBUG */
  st->thread_num = nc;
  st->fifo_path[0] = '\0';
  strcpy(st->listen_host, "localhost");
  st->listen_port = 7777;
  st->loop = NULL;

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
  struct ev_loop        *loop = EV_DEFAULT;
  unsigned int          sb = ev_backend(loop);

  fprintf(stdout, "default backend(%08X): ", sb);
  if (sb & EVBACKEND_SELECT) fprintf(stdout, "SELECT");
  if (sb & EVBACKEND_EPOLL) fprintf(stdout, "EPOLL");
  if (sb & EVBACKEND_KQUEUE) fprintf(stdout, "KQUEUE");
  if (sb & EVBACKEND_POLL) fprintf(stdout, "POLL");
  fprintf(stdout, "\n");

  return;
}

int
parse_args(struct pank7_server_settings *st, int argc, char *argv[])
{
  int           ch;

  while ((ch = getopt(argc, argv,
                      "h"       /* print help information */
                      "i"       /* print system information */
                      "n:"      /* service name */
                      "d"       /* daemon mode on */
                      "D"       /* debug mode on */
                      "I:"      /* fifo path on */
                      "H::"     /* host to listen */
                      "p::"     /* port to listen */
                      "u"       /* udp listen on */
                      )) != -1) {
    switch (ch) {
    case 'h':
      print_help(argc, argv);
      exit(0);
      break;
    case 'i':
      print_sys_info(argc, argv);
      exit(0);
      break;
    case 'n':
      strncpy(st->name, optarg, SHORT_STRING_LENGTH - 1);
      break;
    case 'd':
      st->daemon_mode = true;
      break;
    case 'D':
      st->debug_mode = true;
      break;
    case 'H':
      strncpy(st->listen_host, optarg, SHORT_STRING_LENGTH - 1);
      break;
    case 'p':
      {
        unsigned long   port = strtoul(optarg, NULL, 10);
        st->listen_port = port;
      }
      break;
    case 'I':
      strncpy(st->fifo_path, optarg, MEDIUM_STRING_LENGTH - 1);
      break;
    case 'u':
      break;
    default:
      return 1;
    }
  }

  return 0;
}

static void
period_callback(EV_P_ ev_periodic *w, int revents)
{
  fprintf(stdout, "loop count: %d ", ev_iteration(EV_A));
  fprintf(stdout, "event depth: %d ", ev_depth(EV_A));
  fprintf(stdout, "pending count: %d", ev_pending_count(EV_A));
  fprintf(stdout, "\n");
}

void
pank7_server_exit_callback()
{
}

void
pank7_server_error_callback()
{
}

void
pank7_server_read_callback(EV_P_ ev_io *w, int revents)
{
  char          buf[MEDIUM_STRING_LENGTH];
  ssize_t       ret;

  buf[MEDIUM_STRING_LENGTH - 1] = '\0';
  while (true) {
    ret = recv(w->fd, buf, MEDIUM_STRING_LENGTH - 1, 0);
    if (ret <= 0) break;
    buf[ret] = '\0';
    fprintf(stdout, "recv(%ld): %s", ret, buf);
  }

  if (ret == 0) {
    ev_io_stop(EV_A_ w);
    free(w);
  } else if (ret < 0) {
    if (errno == EAGAIN) // XXXX use evutil macro
      return;
    perror("recv");
    ev_io_stop(EV_A_ w);
  }
}

void
pank7_server_accept_callback(EV_P_ ev_io *w, int revents)
{
  fprintf(stdout, "incoming!\n");

  struct sockaddr_storage       ss;
  socklen_t                     slen = sizeof(ss);
  int                           infd;

  infd = accept(w->fd, (struct sockaddr *)&ss, &slen);
  if (infd < 0) {
    /* error message */
    perror("accept");
  } else if (infd > FD_SETSIZE) {
    perror("FD_SETSIZE");
    close(infd);
  } else {
    struct ev_io                *watcher = NULL;
    watcher = (struct ev_io *)malloc(sizeof(struct ev_io));
    ev_io_init(watcher, pank7_server_read_callback,
               infd, EV_READ);
    ev_io_start(EV_A_ watcher);
  }

  return;
}

static int
setup_nonblocking_socket(int s)
{
  int                   flags;

  /* If they have O_NONBLOCK, use the Posix way to do it */
#if defined(O_NONBLOCK)
  /* Fixme: O_NONBLOCK is defined but broken on SunOS 4.1.x and AIX 3.2.5. */
  if ((flags = fcntl(s, F_GETFL, 0)) == -1)
    flags = 0;
  return fcntl(s, F_SETFL, flags | O_NONBLOCK);
#else
  /* Otherwise, use the old way of doing it */
  flags = 1;
  return ioctl(s, FIOBIO, &flags);
#endif

  return 0;
}

static int
new_tcp_socket(const char *host)
{
  int                   s;
  struct protoent       *proto = getprotobyname("ip");

  if ((s = socket(PF_INET, SOCK_STREAM, proto->p_proto)) == -1) {
    /* error message */
    perror("socket");
    return -1;
  }

  setup_nonblocking_socket(s);

  return s;
}

int
pank7_listener_event_init(struct pank7_server_settings *st)
{
  st->listen_socket = new_tcp_socket(st->listen_host);
  if (st->listen_socket == -1) {
    return 1;
  }

  int           flags = -1;

  setsockopt(st->listen_socket, SOL_SOCKET, SO_REUSEADDR,
             (void *)&flags, sizeof(flags));

  struct sockaddr_in    sin;
  memset(&sin, 0, sizeof(sin));
  sin.sin_family = AF_INET;
  sin.sin_addr.s_addr = htonl(INADDR_ANY);
  sin.sin_port = htons(st->listen_port);
  if (bind(st->listen_socket, (struct sockaddr *)&sin, sizeof(sin)) < 0) {
    /* error message */
    perror("bind");
    return -1;
  }

  if (listen(st->listen_socket, 16) < 0) {
    /* error message */
    perror("listen");
    return -1;
  }

  return 0;
}

static void
signal_callback(EV_P_ ev_signal *w, int revents)
{
  fprintf(stdout, "signal caught: %d\n", w->signum);

  ev_break(settings.loop, EVBREAK_ONE);

  return;
}

int
pank7_server_signal_handler_regitster(struct pank7_server_settings *st)
{
  ev_signal_init(&st->sigterm_watcher, signal_callback, SIGTERM);
  ev_signal_start(st->loop, &st->sigterm_watcher);

  ev_signal_init(&st->sigquit_watcher, signal_callback, SIGQUIT);
  ev_signal_start(st->loop, &st->sigquit_watcher);

  ev_signal_init(&st->sigint_watcher, signal_callback, SIGINT);
  ev_signal_start(st->loop, &st->sigint_watcher);

  return 0;
}

int
pank7_server_atexit_handler_regitster()
{
  return 0;
}

int
pank7_server_init(struct pank7_server_settings *st)
{
  
  /* atexit handlers */
  if (pank7_server_atexit_handler_regitster() != 0) {
    return 1;
  }

  /* get the default event loop from libev */
  // st->loop = EV_DEFAULT;
  st->loop = ev_default_loop(EVBACKEND_KQUEUE);

  /* signal handlers */
  if (pank7_server_signal_handler_regitster(st) != 0) {
    return 1;
  }
  
  /* debug mode? */
  if (st->debug_mode == true) {};

  if (pank7_listener_event_init(st) != 0) {
    /* error message here */
    return 1;
  }

  ev_io_init(&st->listen_watcher, pank7_server_accept_callback,
             st->listen_socket, EV_READ);
  ev_io_start(st->loop, &st->listen_watcher);

  ev_periodic_init(&st->period_watcher, period_callback, 0.0, 2.5, NULL);
  ev_periodic_start(st->loop, &st->period_watcher);
  
  return 0;
}

void
pank7_server_run(struct pank7_server_settings *st)
{
  ev_run(st->loop, 0);

  return;
}

int
main(int argc, char *argv[], char *env[])
{
  default_pank7_server_settings(&settings);

  if (parse_args(&settings, argc, argv) != 0) {
    print_help(argc, argv);
    return 1;
  }

  if (pank7_server_init(&settings) != 0) {
    return 1;
  }

  pank7_server_run(&settings);
          
  return 0;
}
