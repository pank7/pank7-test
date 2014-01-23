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
#include        <sys/resource.h>

#include        <netinet/in.h>
#include        <netinet/tcp.h>
#include        <sys/socket.h>

#include        <ev.h>

#define SHORT_STRING_LENGTH     256
#define MEDIUM_STRING_LENGTH    1024

struct pank7_svc
{
  char                  name[SHORT_STRING_LENGTH];
  bool                  daemon_mode;
  bool                  debug_mode;
  bool                  udp_listen_on;
  unsigned int          thread_num;
  unsigned int          max_conns;
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
  ev_signal             sigpipe_watcher;
  size_t                current_conns;
};

static void
default_pank7_svc(struct pank7_svc *svc)
{
  char          default_name[] = "pank7-svc";
  long          nc = sysconf(_SC_NPROCESSORS_ONLN);

  strcpy(svc->name, default_name);
  svc->daemon_mode = false;
#ifdef _DEBUG
  svc->debug_mode = true;
#else  /* _DEBUG */
  svc->debug_mode = false;
#endif /* _DEBUG */
  svc->thread_num = nc;
  svc->fifo_path[0] = '\0';
  svc->loop = NULL;
  svc->loop_flags = 0;
  strcpy(svc->listen_host, "localhost");
  svc->listen_port = 7777;
  svc->period = 0.0;
  svc->max_conns = 5000;
  svc->current_conns = 0;

  return;
}

static void
print_help(int argc, char *argv[])
{
  fprintf(stderr, "usage: %s [-hi]\n", argv[0]);
  fprintf(stderr, "\th\tprint this help\n");
  fprintf(stderr, "\ti\tprint some system information\n");

  return;
}

static void
print_sys_info(int argc, char *argv[], struct pank7_svc *svc)
{
  struct ev_loop        *loop = EV_DEFAULT;
  unsigned int          sb = ev_backend(loop);

  fprintf(stdout, "name: %s\n", svc->name);
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

static int
parse_args(struct pank7_svc *svc, int argc, char *argv[])
{
  int           ch;

  while ((ch = getopt(argc, argv,
                      "h"       /* print help information */
                      "i"       /* print system information */
                      "n:"      /* service name */
                      "c:"      /* max connections */
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
                      "k"       /* force KQUEUE for loop */
                      )) != -1) {
    switch (ch) {
    case 'h':
      print_help(argc, argv);
      exit(0);
      break;
    case 'i':
      print_sys_info(argc, argv, svc);
      exit(0);
      break;
    case 'n':
      strncpy(svc->name, optarg, SHORT_STRING_LENGTH - 1);
      break;
    case 'd':
      svc->daemon_mode = true;
      break;
    case 'D': 
      svc->debug_mode = true;
      break;
    case 'c':
      svc->max_conns = strtol(optarg, NULL, 10);
      break;
    case 'P':
      if (optarg != NULL) {
        ev_tstamp       period = strtod(optarg, NULL);
        svc->period = period;
      } else {
        svc->period = 10.0;
      }
      fprintf(stderr, "will add a period(%.2fs) event\n", svc->period);
      break;
    case 'H':
      strncpy(svc->listen_host, optarg, SHORT_STRING_LENGTH - 1);
      break;
    case 'p':
      {
        unsigned long   port = strtoul(optarg, NULL, 10);
        svc->listen_port = port;
      }
      break;
    case 'I':
      strncpy(svc->fifo_path, optarg, MEDIUM_STRING_LENGTH - 1);
      break;
    case 'u':
      break;
    case 'e':
      svc->loop_flags |= EVFLAG_NOENV;
      break;
    case 'o':
      svc->loop_flags |= EVFLAG_NOINOTIFY;
      break;
    case 's':
      svc->loop_flags |= EVFLAG_SIGNALFD;
      break;
    case 'k':
      svc->loop_flags |= EVBACKEND_KQUEUE;
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
pank7_svc_period_callback(EV_P_ ev_periodic *w, int revents)
{
  struct pank7_svc      *svc;
  unsigned int          sb = ev_backend(EV_A);

  svc = (struct pank7_svc *)ev_userdata(EV_A);

  fprintf(stderr, "loop count: %d, ", ev_iteration(EV_A));
  fprintf(stderr, "event depth: %d, ", ev_depth(EV_A));
  fprintf(stderr, "pending count: %d, ", ev_pending_count(EV_A));

  fprintf(stderr, "backend: ");
  if (sb & EVBACKEND_SELECT) fprintf(stderr, "SELECT");
  if (sb & EVBACKEND_POLL) fprintf(stderr, "POLL");
  if (sb & EVBACKEND_EPOLL) fprintf(stderr, "EPOLL");
  if (sb & EVBACKEND_KQUEUE) fprintf(stderr, "KQUEUE");
  if (sb & EVBACKEND_DEVPOLL) fprintf(stderr, "DEVPOLL");
  if (sb & EVBACKEND_PORT) fprintf(stderr, "PORT");
  fprintf(stderr, ", ");

  fprintf(stderr, "current conns: %lu", svc->current_conns);
  fprintf(stderr, "\n");
}

#define DEFAULT_SEND_DATA \
  "HTTP/1.1 200 OK\r\n" \
  "Connection: close\r\n" \
  "Content-Type: text/html; charset=UTF-8\r\n" \
  "Content-Length: 82\r\n" \
  "\r\n" \
  "<html><head><title>pank7-svc</title></head><body><h1>pank7-svc</h1></body></html>\n"

static void
pank7_svc_write_callback(EV_P_ ev_io *w, int revents)
{
  struct pank7_svc      *svc;
  ssize_t               ret;
  svc = (struct pank7_svc *)ev_userdata(EV_A);
  char                  send_data[] = DEFAULT_SEND_DATA;
  char                  *ptr = send_data;
  size_t                len = strlen(send_data);

  while (true) {
    ret = send(w->fd, ptr, len, 0);
    if (ret < 0) break;
    if (svc->debug_mode == true)
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
  --svc->current_conns;
  ev_io_stop(EV_A_ w);
  free(w);
}

static void
pank7_svc_read_callback(EV_P_ ev_io *w, int revents)
{
  struct pank7_svc      *svc;
  char                  buf[MEDIUM_STRING_LENGTH];
  ssize_t               ret;
  // socklen_t                     slen = 0;

  svc = (struct pank7_svc *)ev_userdata(EV_A);

  buf[MEDIUM_STRING_LENGTH - 1] = '\0';
  while (true) {
    ret = recv(w->fd, buf, MEDIUM_STRING_LENGTH - 1, 0);
    // ret = read(w->fd, buf, MEDIUM_STRING_LENGTH - 1);
    // ret = recvfrom(w->fd, buf, MEDIUM_STRING_LENGTH - 1, 0, NULL, &slen);
    if (ret <= 0) break;
    buf[ret] = '\0';
    if (svc->debug_mode == true)
      fprintf(stderr, "recv(%d:%ld): %s", w->fd, ret, buf);
  }

  if (ret == 0) {
    if (svc->debug_mode == true)
      fprintf(stderr, "connection(%d) closed by peer\n", w->fd);
    ev_io_stop(EV_A_ w);
    close(w->fd);
    free(w);
    --svc->current_conns;
  } else if (ret < 0) {
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      ev_io_stop(EV_A_ w);
      ev_io_init(w, pank7_svc_write_callback, w->fd, EV_WRITE);
      ev_io_start(EV_A_ w);
    } else {
      if (svc->debug_mode == true)
        fprintf(stderr, "(%d)", w->fd);
      perror("recv");
      ev_io_stop(EV_A_ w);
      close(w->fd);
      free(w);
      --svc->current_conns;
    }
  }
}

static void
pank7_svc_accept_callback(EV_P_ ev_io *w, int revents)
{
  struct pank7_svc              *svc;
  svc = (struct pank7_svc *)ev_userdata(EV_A);
  struct sockaddr_storage       ss;
  socklen_t                     slen = sizeof(ss);
  int                           infd;

  infd = accept(w->fd, (struct sockaddr *)&ss, &slen);
  if (infd < 0) {
    /* error message */
    perror("accept");
  } else {
    struct ev_io                *watcher = NULL;
    watcher = (struct ev_io *)malloc(sizeof(struct ev_io));
    setup_nonblocking_socket(infd);
    ev_io_init(watcher, pank7_svc_read_callback,
               infd, EV_READ);
    ev_io_start(EV_A_ watcher);
    ++svc->current_conns;
  }

  if (svc->debug_mode == true)
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

static int
pank7_listener_event_init(struct pank7_svc *svc)
{
  svc->listen_socket = new_tcp_socket(svc->listen_host);
  if (svc->listen_socket == -1) {
    return 1;
  }

  int           flags = -1;
  struct linger ling = {0, 0};

  setsockopt(svc->listen_socket, SOL_SOCKET, SO_REUSEADDR,
             (void *)&flags, sizeof(flags));
  setsockopt(svc->listen_socket, SOL_SOCKET, SO_KEEPALIVE,
             (void *)&flags, sizeof(flags));
  setsockopt(svc->listen_socket, SOL_SOCKET, SO_LINGER,
             (void *)&ling, sizeof(ling));
  setsockopt(svc->listen_socket, IPPROTO_TCP, TCP_NODELAY,
             (void *)&flags, sizeof(flags));

  struct sockaddr_in    sin;
  memset(&sin, 0, sizeof(sin));
  sin.sin_family = AF_INET;
  sin.sin_addr.s_addr = htonl(INADDR_ANY);
  sin.sin_port = htons(svc->listen_port);
  if (bind(svc->listen_socket, (struct sockaddr *)&sin, sizeof(sin)) < 0) {
    /* error message */
    perror("bind");
    return -1;
  }

  if (listen(svc->listen_socket, 16) < 0) {
    /* error message */
    perror("listen");
    return -1;
  }

  return 0;
}

static void
pank7_svc_signal_callback(EV_P_ ev_signal *w, int revents)
{
  struct pank7_svc      *svc;
  svc = (struct pank7_svc *)ev_userdata(EV_A);

  if (svc->debug_mode)
    fprintf(stderr, "signal(%d) caught, quit\n", w->signum);

  ev_io_stop(EV_A_ &svc->listen_watcher);
  ev_signal_stop(EV_A_ &svc->sigterm_watcher);
  ev_signal_stop(EV_A_ &svc->sigquit_watcher);
  ev_signal_stop(EV_A_ &svc->sigint_watcher);
  ev_signal_stop(EV_A_ &svc->sigpipe_watcher);

  if (svc->period > 0.0) {
    ev_periodic_stop(EV_A_ &svc->period_watcher);
  }

  ev_break(EV_A_ EVBREAK_ALL);

  return;
}

static int
pank7_svc_signal_handler_register(struct pank7_svc *svc)
{
  ev_signal_init(&svc->sigterm_watcher, pank7_svc_signal_callback, SIGTERM);
  ev_signal_start(svc->loop, &svc->sigterm_watcher);

  ev_signal_init(&svc->sigquit_watcher, pank7_svc_signal_callback, SIGQUIT);
  ev_signal_start(svc->loop, &svc->sigquit_watcher);

  ev_signal_init(&svc->sigint_watcher, pank7_svc_signal_callback, SIGINT);
  ev_signal_start(svc->loop, &svc->sigint_watcher);

  ev_signal_init(&svc->sigpipe_watcher, pank7_svc_signal_callback, SIGPIPE);
  ev_signal_start(svc->loop, &svc->sigpipe_watcher);

  return 0;
}

static int
pank7_svc_atexit_handler_register()
{
  return 0;
}

static int
pank7_svc_set_limits(struct pank7_svc *svc)
{
  struct rlimit rlim;
  /*
   * If needed, increase rlimits to allow as many connections
   * as needed.
   */
  if (getrlimit(RLIMIT_NOFILE, &rlim) != 0) {
    fprintf(stderr, "failed to getrlimit number of files\n");
    perror("getrlimit");
    return 1;
  } else {
    int maxfiles = svc->max_conns;
    if (rlim.rlim_cur < maxfiles)
      rlim.rlim_cur = maxfiles + 3;
    if (rlim.rlim_max < rlim.rlim_cur)
      rlim.rlim_max = rlim.rlim_cur;
    if (setrlimit(RLIMIT_NOFILE, &rlim) != 0) {
      fprintf(stderr, "failed to set rlimit for open files. Try running as root or requesting smaller maxconns value.\n");
      perror("setrlimit");
      return 1;
    }
  }

  return 0;
}

static int
pank7_svc_init(struct pank7_svc *svc)
{
  /* atexit handlers */
  if (pank7_svc_atexit_handler_register() != 0) {
    return 1;
  }
  /* set limits */
  if (pank7_svc_set_limits(svc) != 0) {
    return 1;
  }

  /* get the default event loop from libev */
  svc->loop = ev_default_loop(svc->loop_flags);
  ev_set_userdata(svc->loop, (void *)svc);

  if (pank7_listener_event_init(svc) != 0) {
    /* error message here */
    return 1;
  }

  ev_io_init(&svc->listen_watcher, pank7_svc_accept_callback,
             svc->listen_socket, EV_READ);
  ev_io_start(svc->loop, &svc->listen_watcher);

  if (svc->period > 0.0) {
    ev_periodic_init(&svc->period_watcher, pank7_svc_period_callback,
                     0.0, svc->period, NULL);
    ev_periodic_start(svc->loop, &svc->period_watcher);
  }

  /* signal handlers */
  if (pank7_svc_signal_handler_register(svc) != 0) {
    return 1;
  }
  
  return 0;
}

static void
pank7_svc_run(struct pank7_svc *svc)
{
  ev_run(svc->loop, 0);

  ev_loop_destroy(svc->loop);

  return;
}

int
main(int argc, char *argv[], char *env[])
{
  struct pank7_svc      svc;

  default_pank7_svc(&svc);

  if (parse_args(&svc, argc, argv) != 0) {
    print_help(argc, argv);
    return 1;
  }

  if (pank7_svc_init(&svc) != 0) {
    return 1;
  }

  pank7_svc_run(&svc);
          
  return 0;
}
