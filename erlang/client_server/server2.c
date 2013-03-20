/* 
 * Parallel socket server.
 * Author: Joe Armstrong <joe@sics.se>
 * Date: 2003-10-02
 *
 * Usage:
 *   server2 <port>
 */ 

#include	<stdio.h>

/* this buffer is used to communicate with the client */

char buf[65536];

/* handle will be called every time something happens
 *   phase = 1 means a connection is starting
 *         = 2 means the client has sent a message to the
 *	       server. The length of the message is n
 *	       and the data is in buf[0..n-1]
 *	       The client *must* reply by calling
 *	          gen_reply(int fd, char *p, int m)
 *	       this returns the data in p[0..m-1] to the client
 *	   = 3 means the client has disconnected
 */

my_handler(int phase, int fd, int n)
{
  int i;
  switch (phase)
    {
    case 1:
      printf("handle %d starting\n", getpid());
      break;
    case 2:
      printf("handle %d received %d bytes:", getpid(), n);
      for(i=0;i<n;i++)
	putchar(buf[i]);
      printf("\r\n");
      /* just for fun crash if buf[0] = 42 */
      if(buf[0] == 42)
	exit(1);
      strcpy(buf, "ack");
      gen_reply(fd, buf, 3);
      printf("handle %d sending ack\n", getpid());
      break;
    case 3:
      printf("handle %d stopping\n", getpid());
      break;
    }
}

main(int argc, char *argv[])
{
  int port;

  if (argc != 2)
    err_quit("usage: server <port>\n");
  
  port = atoi(argv[1]);
  gen_server(port, my_handler);

}



