#include <stdio.h>

int main() {
#if defined(_M_ARM64)
  printf("MACH=tarm64nt\n");
#elif defined(_WIN64)
  printf("MACH=ta6nt\n");
#else
  printf("MACH=ti3nt\n");
#endif
  return 0;
}
