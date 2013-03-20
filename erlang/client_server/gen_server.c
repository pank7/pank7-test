/* 
 * Parallel generic server.
 * Adapted from Stevens - Unix Network Programming.
 * Author: Joe Armstrong <joe@sics.se>
 * Date: 2003-10-02
 */ 

#include	<stdio.h>
#include	<sys/socket.h>
#include	<arpa/inet.h>
#include        <varargs.h>

extern char buf[25536];

/*VARARGS1*/
err_quit(va_alist)
va_dcl
{
  va_list         args;
  char            *fmt;
  
  va_start(args);
  fmt = va_arg(args, char *);
  vfprintf(stderr, fmt, args);
  fputc('\n', stderr);
  va_end(args);
  
  exit(1);
}
/*
 * Read "n" bytes from a descriptor.
 * Use in place of read() when fd is a stream socket.
 */

int readn(int fd, char *ptr, int nbytes)
{
  int	nleft, nread;
  
  nleft = nbytes;
  while (nleft > 0) {
    nread = read(fd, ptr, nleft);
    if (nread < 0)
      return(nread);		/* error, return < 0 */
    else if (nread == 0)
      break;			/* EOF */
    nleft -= nread;
    ptr   += nread;
  }
  return(nbytes - nleft);		/* return >= 0 */
}

/*
 * Write "n" bytes to a descriptor.
 * Use in place of write() when fd is a stream socket.
 */

int writen(int fd, char *ptr, int nbytes)
{
  int nleft, nwritten;
  
  nleft = nbytes;
  while (nleft > 0) {
    nwritten = write(fd, ptr, nleft);
    if (nwritten <= 0)
      return(nwritten);		/* error */
    nleft -= nwritten;
    ptr   += nwritten;
  }
  return(nbytes - nleft);
}

gen_server(int port, int (*handler)(int, int, int))
{
  int			sockfd, newsockfd, clilen, childpid, err;
  struct sockaddr_in	cli_addr, serv_addr;
  
  if ( (sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    err_quit("server: can't open stream socket");

  printf("opening port %d\n", port);

  bzero((char *) &serv_addr, sizeof(serv_addr));
  serv_addr.sin_family      = AF_INET;
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  serv_addr.sin_port        = htons(port);
  
  if ((err = bind(sockfd, (struct sockaddr *) &serv_addr, 
		  sizeof(serv_addr))) < 0)
    err_quit("server: can't bind local address %d", err);
  listen(sockfd, 5);
  for ( ; ; ) {
    clilen = sizeof(cli_addr);
    newsockfd = accept(sockfd, (struct sockaddr *) &cli_addr, &clilen);
    if (newsockfd < 0)
      err_quit("server: accept error");
    if ( (childpid = fork()) < 0)
      err_quit("server: fork error");
    else if (childpid == 0) {
      close(sockfd);		
      (*handler)(1, newsockfd, 0);
      loop(newsockfd, handler);	
      /* printf("closing connection in process %d\n", getpid()); */
      (*handler)(3, newsockfd, 0);
      exit(0);
    }
    
    close(newsockfd);
  }
}

/* the socket
 * protocol is 2 byte length then the data
 */

loop(int fd, int (*handler)(int, int, int))
{
  char *p;
  int again, n, i;

  while(1){
    p = buf;
    i = readn(fd, buf, 2);
    if(i==0)
      /* socket closed */
      break;
    else if (i == 2){
      n = *buf *256 + *(buf+1);
      i = readn(fd, buf, n);
      if (i != n)
	break;
      (*handler)(2, fd, n);
    } else {
      /* protocol error */
      break;
    }
  }
}

gen_reply(int fd, char *p, int n)
{
  char out[2];
  out[0] = n >> 8;
  out[1] = n & 0xff;
  writen(fd, out, 2);
  writen(fd, p, n);
}

