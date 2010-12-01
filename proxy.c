#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>

int main(int argc, char* argv[]) {
  pid_t pid;
  int fd_r_child[2], fd_w_child[2];
  char buf[256] = "";

  pipe(fd_r_child);
  pipe(fd_w_child);

  pid = fork();

  if (pid == 0) {               /* Child */
    /* Magic? */
    close(fd_w_child[1]);
    close(fd_r_child[0]);
    dup2(fd_w_child[0], 0);
    dup2(fd_r_child[1], 1);
    close(fd_w_child[0]);
    close(fd_r_child[1]);

    /* execlp("gnugo", "gnugo", "--mode",  "gtp", NULL); */
    execlp("seq", "seq", "1", "100", NULL);
  } else {                      /* Parent */
    close(fd_w_child[0]);
    close(fd_r_child[1]);

    char *command = "1 help\n";
    write(fd_w_child[1], command, strlen(command));

    int chars = 1;
    while (chars != 0) {
      chars = read(fd_r_child[0], buf, 256);
      fprintf(stdout, "%s", buf);
    }
    /* Handle io translation between js and gnugo */

    int status;
    wait(&status);
  }

  return 0;
}
