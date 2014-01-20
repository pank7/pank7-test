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

#include        <ev.h>

#define SHORT_STRING_LENGTH     256
#define MEDIUM_STRING_LENGTH    1024

struct pank7_server_settings
{
  char                  name[SHORT_STRING_LENGTH];
  bool                  daemon_mode;
  bool                  debug_mode;
  bool                  udp_listen_on;
  unsigned int          thread_num;
  char                  fifo_path[MEDIUM_STRING_LENGTH];
  struct ev_loop        *loop;
  int                   loop_flags;
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

void
default_pank7_server_settings(struct pank7_server_settings *st)
{
  char          default_name[] = "pank7-server";
  long          nc = sysconf(_SC_NPROCESSORS_ONLN);

  strcpy(st->name, default_name);
  st->daemon_mode = false;
#ifdef _DEBUG
  st->debug_mode = true;
#else  /* _DEBUG */
  st->debug_mode = false;
#endif /* _DEBUG */
  st->thread_num = nc;
  st->fifo_path[0] = '\0';
  st->loop = NULL;
  st->loop_flags = 0;
  strcpy(st->listen_host, "localhost");
  st->listen_port = 7777;
  st->period = 0.0;

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
  if (sb & EVBACKEND_POLL) fprintf(stdout, "POLL");
  if (sb & EVBACKEND_EPOLL) fprintf(stdout, "EPOLL");
  if (sb & EVBACKEND_KQUEUE) fprintf(stdout, "KQUEUE");
  if (sb & EVBACKEND_DEVPOLL) fprintf(stdout, "DEVPOLL");
  if (sb & EVBACKEND_PORT) fprintf(stdout, "PORT");
  fprintf(stdout, "\n");

  ev_verify(loop);

  ev_loop_destroy(loop);

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
                      "P::"     /* add a periodic statistic watcher */
                      "I:"      /* fifo path on */
                      "H:"      /* host to listen */
                      "p:"      /* port to listen */
                      "u"       /* udp listen on */
                      "e"       /* EVFLAG_NOENV for loop */
                      "o"       /* EVFLAG_NOINOTIFY for loop */
                      "s"       /* EVFLAG_SIGNALFD for loop */
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
    case 'P':
      if (optarg != NULL) {
        ev_tstamp       period = strtod(optarg, NULL);
        st->period = period;
      } else {
        st->period = 10.0;
      }
      fprintf(stdout, "will add a period(%.2fs) event\n", st->period);
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
    case 'e':
      st->loop_flags |= EVFLAG_NOENV;
      break;
    case 'o':
      st->loop_flags |= EVFLAG_NOINOTIFY;
      break;
    case 's':
      st->loop_flags |= EVFLAG_SIGNALFD;
      break;
    default:
      return 1;
    }
  }

  return 0;
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

static void
pank7_server_period_callback(EV_P_ ev_periodic *w, int revents)
{
  fprintf(stdout, "loop count: %d, ", ev_iteration(EV_A));
  fprintf(stdout, "event depth: %d, ", ev_depth(EV_A));
  fprintf(stdout, "pending count: %d", ev_pending_count(EV_A));
  fprintf(stdout, "\n");
}

void
pank7_server_exit_callback()
{
}

#define DEFAULT_SENT_DATA \
  "HTTP/1.1 200 OK\r\n" \
  "Connection: close\r\n" \
  "Content-Type: text/html; charset=UTF-8\r\n" \
  "Content-Length: 82\r\n" \
  "\r\n" \
  "<html><head><title>pank7-svc</title></head><body><h1>pank7-svc</h1></body></html>\n"

void
pank7_server_write_callback(EV_P_ ev_io *w, int revents)
{
  struct pank7_server_settings  *st;
  ssize_t                       ret;
  st = (struct pank7_server_settings *)ev_userdata(EV_A);
  char                          send_data[] = DEFAULT_SENT_DATA;
  char                          *ptr = send_data;
  size_t                        len = strlen(send_data);

  while (true) {
    ret = send(w->fd, ptr, len, 0);
    if (ret < 0) break;
    if (st->debug_mode == true)
      fprintf(stderr, "send(%d:%ld)\n", w->fd, ret);
    if (ret == len) break;
    ptr += ret;
    len -= ret;
  }

  if (ret < 0) {
    if (errno == EAGAIN || errno == EWOULDBLOCK) return;
    /* error message */
    perror("send");
  }
  close(w->fd);
  ev_io_stop(EV_A_ w);
  free(w);
}

void
pank7_server_read_callback(EV_P_ ev_io *w, int revents)
{
  struct pank7_server_settings  *st;
  char                          buf[MEDIUM_STRING_LENGTH];
  ssize_t                       ret;

  st = (struct pank7_server_settings *)ev_userdata(EV_A);

  buf[MEDIUM_STRING_LENGTH - 1] = '\0';
  while (true) {
    ret = recv(w->fd, buf, MEDIUM_STRING_LENGTH - 1, 0);
    if (ret <= 0) break;
    buf[ret] = '\0';
    if (st->debug_mode == true)
      fprintf(stderr, "recv(%d:%ld): %s", w->fd, ret, buf);
  }

  if (ret == 0) {
    if (st->debug_mode == true)
      fprintf(stderr, "connection(%d) closed by peer\n", w->fd);
  } else if (ret < 0) {
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      struct ev_io              *watcher = NULL;
      watcher = (struct ev_io *)malloc(sizeof(struct ev_io));
      ev_io_init(watcher, pank7_server_write_callback,
                 w->fd, EV_WRITE);
      ev_io_start(EV_A_ watcher);
    } else {
      if (st->debug_mode == true)
        fprintf(stderr, "(%d)", w->fd);
      perror("recv");
      close(w->fd);
    }
  }
  ev_io_stop(EV_A_ w);
  free(w);
}

void
pank7_server_accept_callback(EV_P_ ev_io *w, int revents)
{
  struct pank7_server_settings  *st;
  st = (struct pank7_server_settings *)ev_userdata(EV_A);

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
    setup_nonblocking_socket(infd);
    ev_io_init(watcher, pank7_server_read_callback,
               infd, EV_READ);
    ev_io_start(EV_A_ watcher);
  }

  if (st->debug_mode == true)
    fprintf(stderr, "incoming connection(%d)\n", infd);

  return;
}

static int
new_tcp_socket(const char *host)
{
  int                   s;

  if ((s = socket(PF_INET, SOCK_STREAM, 0)) == -1) {
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
pank7_server_close_io_callback(EV_P_ int type, void *w) __attribute__((unused));
static void
pank7_server_close_io_callback(EV_P_ int type, void *w)
{
}

static void
pank7_server_signal_callback(EV_P_ ev_signal *w, int revents)
{
  struct pank7_server_settings  *st;
  st = (struct pank7_server_settings *)ev_userdata(EV_A);

  if (st->debug_mode)
    fprintf(stderr, "signal(%d) caught, quit\n", w->signum);

  ev_io_stop(EV_A_ &st->listen_watcher);
  ev_signal_stop(EV_A_ &st->sigterm_watcher);
  ev_signal_stop(EV_A_ &st->sigquit_watcher);
  ev_signal_stop(EV_A_ &st->sigint_watcher);

  if (st->period > 0.0) {
    ev_periodic_stop(EV_A_ &st->period_watcher);
  }

  ev_break(EV_A_ EVBREAK_ALL);

  return;
}

int
pank7_server_signal_handler_register(struct pank7_server_settings *st)
{
  ev_signal_init(&st->sigterm_watcher, pank7_server_signal_callback, SIGTERM);
  ev_signal_start(st->loop, &st->sigterm_watcher);

  ev_signal_init(&st->sigquit_watcher, pank7_server_signal_callback, SIGQUIT);
  ev_signal_start(st->loop, &st->sigquit_watcher);

  ev_signal_init(&st->sigint_watcher, pank7_server_signal_callback, SIGINT);
  ev_signal_start(st->loop, &st->sigint_watcher);

  return 0;
}

int
pank7_server_atexit_handler_register()
{
  return 0;
}

int
pank7_server_init(struct pank7_server_settings *st)
{
  /* atexit handlers */
  if (pank7_server_atexit_handler_register() != 0) {
    return 1;
  }

  /* get the default event loop from libev */
  st->loop = ev_default_loop(st->loop_flags);
  ev_set_userdata(st->loop, (void *)st);

  if (pank7_listener_event_init(st) != 0) {
    /* error message here */
    return 1;
  }

  ev_io_init(&st->listen_watcher, pank7_server_accept_callback,
             st->listen_socket, EV_READ);
  ev_io_start(st->loop, &st->listen_watcher);

  if (st->period > 0.0) {
    ev_periodic_init(&st->period_watcher, pank7_server_period_callback,
                     0.0, st->period, NULL);
    ev_periodic_start(st->loop, &st->period_watcher);
  }

  /* signal handlers */
  if (pank7_server_signal_handler_register(st) != 0) {
    return 1;
  }
  
  return 0;
}

void
pank7_server_run(struct pank7_server_settings *st)
{
  ev_run(st->loop, 0);

  ev_loop_destroy(st->loop);

  return;
}

int
main(int argc, char *argv[], char *env[])
{
  struct pank7_server_settings  settings;

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
