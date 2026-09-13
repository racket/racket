#include <stdio.h>
#include <sys/socket.h>

int main(void) {
  printf("%d %d\n", SOL_SOCKET, SO_LINGER);
  return 0;
}
