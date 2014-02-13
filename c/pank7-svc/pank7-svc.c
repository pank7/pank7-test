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

#include        <pthread.h>

#include        <ev.h>

#include        "http_parser.h"

#define SHORT_STRING_LENGTH     256
#define MEDIUM_STRING_LENGTH    1024

struct pank7_svc;

struct pank7_thread
{
  struct pank7_svc      *svc;
  struct ev_loop        *loop;
  pthread_t             thread_id;
  pthread_attr_t        thread_attr;
  ev_async              work_watcher;
  ev_async              stop_watcher;
};

struct pank7_http_connection
{
  ev_io                 w;
  http_parser           hp;
  bool                  fin;
  void                  *data;
};

#define WUD(w, n) struct pank7_http_connection *n = (struct pank7_http_connection *)(w)->data
#define HPUD(hp, n) struct pank7_http_connection *n = (struct pank7_http_connection *)(hp)->data

static int
pank7_http_connection_init(struct pank7_http_connection *c,
                           int s, int ev, void (*cb)(EV_P_ ev_io *, int),
                           struct pank7_svc *svc)
{
  ev_io_init(&c->w, cb, s, ev);
  c->w.data = (void *)c;
  http_parser_init(&c->hp, HTTP_REQUEST);
  c->hp.data = (void *)c;
  c->fin = true;
  c->fin = false;
  c->data = (void *)svc;

  return 0;
}

struct pank7_svc
{
  char                  name[SHORT_STRING_LENGTH];
  bool                  daemon_mode;
  bool                  debug_mode;
  bool                  udp_listen_on;
  unsigned int          thread_num;
  struct pank7_thread   *threads;
  unsigned int          max_conns;
  size_t                current_conns;
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
  ev_cleanup            cleanup_watcher;
  http_parser_settings  hps;
};

#define EVUD(n) struct pank7_svc *n = (struct pank7_svc *)ev_userdata(EV_A)
#define HCUD(hc, n) struct pank7_svc *n = (struct pank7_svc *)(hc)->data

static int
http_url_callback(http_parser *hp, const char *u, size_t l)
{
  HPUD(hp, conn);
  HCUD(conn, svc);
  struct http_parser_url        hpu;
  int                           ret;

  ret = http_parser_parse_url(u, l, 0, &hpu);

  if (svc->debug_mode == true) {
    fprintf(stderr, "http url: %.*s\n", (int)l, u);
    fprintf(stderr, "\tport: %u\n", hpu.port);
    if (hpu.field_set & (1 << UF_SCHEMA))
      fprintf(stderr, "\tschema: %.*s\n", hpu.field_data[UF_SCHEMA].len,
              u + hpu.field_data[UF_SCHEMA].off);
    if (hpu.field_set & (1 << UF_HOST))
      fprintf(stderr, "\thost: %.*s\n", hpu.field_data[UF_HOST].len,
              u + hpu.field_data[UF_HOST].off);
    if (hpu.field_set & (1 << UF_PORT))
      fprintf(stderr, "\tport: %.*s\n", hpu.field_data[UF_PORT].len,
              u + hpu.field_data[UF_PORT].off);
    if (hpu.field_set & (1 << UF_PATH))
      fprintf(stderr, "\tpath: %.*s\n", hpu.field_data[UF_PATH].len,
              u + hpu.field_data[UF_PATH].off);
    if (hpu.field_set & (1 << UF_QUERY))
      fprintf(stderr, "\tquery: %.*s\n", hpu.field_data[UF_QUERY].len,
              u + hpu.field_data[UF_QUERY].off);
    if (hpu.field_set & (1 << UF_FRAGMENT))
      fprintf(stderr, "\tfragment: %.*s\n", hpu.field_data[UF_FRAGMENT].len,
              u + hpu.field_data[UF_FRAGMENT].off);
    if (hpu.field_set & (1 << UF_USERINFO))
      fprintf(stderr, "\tuserinfo: %.*s\n", hpu.field_data[UF_USERINFO].len,
              u + hpu.field_data[UF_USERINFO].off);
  }

  return ret;
}

static int
http_header_field_callback(http_parser *hp, const char *f, size_t l)
{
  HPUD(hp, conn);
  HCUD(conn, svc);

  if (svc->debug_mode == true) {
    fprintf(stderr, "http header field: %.*s\n", (int)l, f);
  }

  return 0;
}

static int
http_header_value_callback(http_parser *hp, const char *v, size_t l)
{
  HPUD(hp, conn);
  HCUD(conn, svc);

  if (svc->debug_mode == true) {
    fprintf(stderr, "http header value: %.*s\n", (int)l, v);
  }

  return 0;
}

static int
http_headers_complete_callback(http_parser *hp)
{
  HPUD(hp, conn);
  HCUD(conn, svc);

  if (svc->debug_mode == true) {
    fprintf(stderr, "http headers complate! (HTTP/%u.%u %s %s %s)\n",
            hp->http_major, hp->http_minor, http_method_str(hp->method),
            (http_should_keep_alive(hp) != 0)  ? "keep-alive" : "close",
            (hp->upgrade == 1) ? "upgrade" : "no-upgrade");
  }

  return 0;
}

static int
http_message_complete_callback(http_parser *hp)
{
  HPUD(hp, conn);
  HCUD(conn, svc);

  if (svc->debug_mode == true)
    fprintf(stderr, "http message complate!\n");

  conn->fin = true;

  return 0;
}

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
  svc->threads = NULL;
  svc->fifo_path[0] = '\0';
  svc->loop = NULL;
  svc->loop_flags = 0;
  strcpy(svc->listen_host, "localhost");
  svc->listen_port = 7777;
  svc->period = 0.0;
  svc->max_conns = 5000;
  svc->current_conns = 0;
  memset((void *)&svc->hps, 0, sizeof(svc->hps));
  svc->hps.on_message_complete = http_message_complete_callback;
  svc->hps.on_headers_complete = http_headers_complete_callback;
  svc->hps.on_header_field = http_header_field_callback;
  svc->hps.on_header_value = http_header_value_callback;
  svc->hps.on_url = http_url_callback;

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
                      "t:"      /* worker thread number */
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
    case 't':
      svc->thread_num = strtol(optarg, NULL, 10);
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
  EVUD(svc);
  unsigned int          sb = ev_backend(EV_A);

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
  "Connection: Keep-Alive\r\n" \
  "Content-Type: text/html; charset=UTF-8\r\n" \
  "Content-Length: 82\r\n" \
  "\r\n" \
  "<html><head><title>pank7-svc</title></head><body><h1>pank7-svc</h1></body></html>\n"

static void
pank7_svc_read_callback(EV_P_ ev_io *w, int revents);

static void
pank7_svc_write_callback(EV_P_ ev_io *w, int revents);

static void
pank7_svc_write_callback(EV_P_ ev_io *w, int revents)
{
  EVUD(svc);
  WUD(w, conn);
  ssize_t               ret;
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

  if (http_should_keep_alive(&conn->hp) != 0) {
    if (svc->debug_mode == true)
      fprintf(stderr, "should keep alive (%d)\n", w->fd);
    conn->fin = false;
    ev_io_stop(EV_A_ w);
    pank7_http_connection_init(conn, w->fd, EV_READ, pank7_svc_read_callback, svc);
    ev_io_start(EV_A_ w);
  } else {
    if (svc->debug_mode == true)
      fprintf(stderr, "connection close (%d)\n", w->fd);
    close(w->fd);
    --svc->current_conns;
    ev_io_stop(EV_A_ w);
    free(conn);
  }
}

static void
pank7_svc_read_callback(EV_P_ ev_io *w, int revents)
{
  EVUD(svc);
  WUD(w, conn);
  char                  buf[MEDIUM_STRING_LENGTH];
  ssize_t               ret;
  size_t                len = 0;
  bool                  close_conn = false;
  // socklen_t             slen = 0;

  buf[MEDIUM_STRING_LENGTH - 1] = '\0';
  while (true) {
    ret = recv(w->fd, buf, MEDIUM_STRING_LENGTH - 1, 0);
    // ret = read(w->fd, buf, MEDIUM_STRING_LENGTH - 1);
    // ret = recvfrom(w->fd, buf, MEDIUM_STRING_LENGTH - 1, 0, NULL, &slen);
    if (ret <= 0) break;
    len += ret;
    buf[ret] = '\0';
    if (svc->debug_mode == true) {
      fprintf(stderr, "recv(%d:%ld): %s", w->fd, ret, buf);
      char              *p = buf;
      while (*p++) {
        fprintf(stderr, " %02X", (unsigned int)*p);
        if ((p - buf) % 16 == 0) fprintf(stderr, "\n");
      }
      fprintf(stderr, "\n");
    }
  }

  if (ret == 0) {
    if (svc->debug_mode == true)
      fprintf(stderr, "connection(%d) closed by peer\n", w->fd);
    close_conn = true;
  } else if (ret < 0) {
    if (errno != EAGAIN && errno != EWOULDBLOCK) {
      if (svc->debug_mode == true)
        fprintf(stderr, "(%d)", w->fd);
      perror("recv");
      close_conn = true;
    }
  }

  if (close_conn) {
    ev_io_stop(EV_A_ w);
    close(w->fd);
    free(conn);
    --svc->current_conns;
    return;
  }

  /* OK, let's take a look into the message */
  size_t        nparsed;
  nparsed = http_parser_execute(&conn->hp, &svc->hps, buf, len);
  if (nparsed != len) {
    if (svc->debug_mode == true)
      fprintf(stderr, "HTTP request parse error: %s (%s)\n",
              http_errno_description(HTTP_PARSER_ERRNO(&conn->hp)),
              http_errno_name(HTTP_PARSER_ERRNO(&conn->hp)));
    close_conn = true;
  } else {
    if (conn->hp.upgrade == 1) {
      close_conn = true;
    } else if (conn->fin == true) {
      ev_io_stop(EV_A_ w);
      ev_io_init(w, pank7_svc_write_callback, w->fd, EV_WRITE);
      ev_io_start(EV_A_ w);
    }
  }

  if (close_conn) {
    ev_io_stop(EV_A_ w);
    close(w->fd);
    free(conn);
    --svc->current_conns;
    return;
  }
}

static void
pank7_svc_accept_callback(EV_P_ ev_io *w, int revents)
{
  EVUD(svc);
  struct sockaddr_storage       ss;
  socklen_t                     slen = sizeof(ss);
  int                           infd;

  infd = accept(w->fd, (struct sockaddr *)&ss, &slen);
  if (infd < 0) {
    /* error message */
    perror("accept");
  } else {
    struct pank7_http_connection        *conn = NULL;
    conn = (struct pank7_http_connection *)malloc(sizeof(struct pank7_http_connection));
    setup_nonblocking_socket(infd);
    pank7_http_connection_init(conn, infd, EV_READ, pank7_svc_read_callback, svc);
    ev_io_start(EV_A_ &conn->w);
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
  EVUD(svc);

  if (svc->debug_mode)
    fprintf(stderr, "signal(%d) caught\n", w->signum);

  ev_break(EV_A_ EVBREAK_ALL);

  return;
}

static void
pank7_svc_cleanup_callback(EV_P_ ev_cleanup *w, int revents)
{
  EVUD(svc);

  if (svc->debug_mode)
    fprintf(stderr, "cleanup\n");

  ev_io_stop(EV_A_ &svc->listen_watcher);
  ev_signal_stop(EV_A_ &svc->sigterm_watcher);
  ev_signal_stop(EV_A_ &svc->sigquit_watcher);
  ev_signal_stop(EV_A_ &svc->sigint_watcher);
  ev_signal_stop(EV_A_ &svc->sigpipe_watcher);

  if (svc->period > 0.0) {
    ev_periodic_stop(EV_A_ &svc->period_watcher);
  }

  int   i;
  for (i = 0; i < svc->thread_num; ++i) {
    ev_async_stop(svc->threads[i].loop, &svc->threads[i].work_watcher);
    ev_async_stop(svc->threads[i].loop, &svc->threads[i].stop_watcher);
    ev_loop_destroy(svc->threads[i].loop);
  }

  free(svc->threads);

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

static void
pank7_svc_atexit_handler(void)
{
  fprintf(stdout, "quit\n");

  return;
}

static int
pank7_svc_atexit_handler_register()
{
  atexit(pank7_svc_atexit_handler);

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

static void
pank7_svc_thread_work_callback(EV_P_ ev_async *w, int revents)
{
  EVUD(svc);

  if (svc->debug_mode == true)
    fprintf(stderr, "thread %lu: incoming work!\n", pthread_self());

  return;
}


static void
pank7_svc_thread_stop_callback(EV_P_ ev_async *w, int revents)
{
  EVUD(svc);
  //  struct pank7_thread   *thread = (struct pank7_thread *)w->data;

  if (svc->debug_mode == true)
    fprintf(stderr, "thread %lu: stop!\n", pthread_self());

  ev_break(EV_A_ EVBREAK_ALL);

  return;
}

static void *
pank7_svc_worker_thread(void *arg)
{
  struct pank7_thread   *thread = (struct pank7_thread *)arg;
  struct pank7_svc      *svc = thread->svc;

  if (svc->debug_mode == true)
    fprintf(stderr, "thread %lu: start!\n", pthread_self());

  ev_run(thread->loop, 0);

  return NULL;
}

static int
pank7_svc_worker_threads_init(struct pank7_svc *svc)
{
  int           i;

  svc->threads = (struct pank7_thread *)malloc(sizeof(struct pank7_thread) * svc->thread_num);

  for (i = 0; i < svc->thread_num; ++i) {
    svc->threads[i].svc = svc;
    svc->threads[i].loop = ev_loop_new(svc->loop_flags);
    ev_set_userdata(svc->threads[i].loop, (void *)svc);
    ev_async_init(&svc->threads[i].work_watcher, pank7_svc_thread_work_callback);
    svc->threads[i].work_watcher.data = (void *)&svc->threads[i];
    ev_async_init(&svc->threads[i].stop_watcher, pank7_svc_thread_stop_callback);
    svc->threads[i].stop_watcher.data = (void *)&svc->threads[i];
    pthread_attr_init(&svc->threads[i].thread_attr);
    pthread_create(&svc->threads[i].thread_id, &svc->threads[i].thread_attr,
                   pank7_svc_worker_thread, &svc->threads[i]);
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

  ev_cleanup_init(&svc->cleanup_watcher, pank7_svc_cleanup_callback);
  ev_cleanup_start(svc->loop, &svc->cleanup_watcher);

  if (pank7_listener_event_init(svc) != 0) {
    /* error message here */
    ev_loop_destroy(svc->loop);
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
    ev_loop_destroy(svc->loop);
    return 1;
  }
  
  /* worker threads */
  if (pank7_svc_worker_threads_init(svc) != 0) {
    ev_loop_destroy(svc->loop);
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
