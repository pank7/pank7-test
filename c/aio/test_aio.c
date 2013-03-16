#include        <stdio.h>
#include        <fcntl.h>
#include        <unistd.h>
#include        <errno.h>
#include        <sys/types.h>
#include        <sys/aio.h>

int
main(int argc, char *argv[])
{
  int           fd;
  char          c[21];
  off_t         o;
  ssize_t       s;

  fd = open("/dev/disk3s1", O_RDWR);
  if (fd == -1) {
    perror("failed to popen the file");
    return 1;
  }

  o = lseek(fd, 0, SEEK_SET);

  s = write(fd, "09876543211234567890", 20);

  o = lseek(fd, 0, SEEK_SET);

  s = read(fd, c, 20);
  c[20] = '\0';

  fprintf(stdout, "I just wrote: %s\n", c);

  return 0;
}
