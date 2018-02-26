#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

/* 65535 characters should be enough for any string --- or so says
   MSVC. Convert "startup.inc" to a character array. */

int main(int argc, char **argv) {
  struct _stat s1, s2;
  FILE *in, *out;
  int c, col = 0;

  if (_stat(argv[1], &s1) == 0) {
    if (_stat(argv[2], &s2) == 0) {
      if (s2.st_mtime > s1.st_mtime) {
	printf("Generated file is already newer than source\n");
	return 0;
      }
    }
  }

  in = fopen(argv[1], "r");
  out = fopen(argv[2], "w");

  fprintf(out, "#define EVAL_STARTUP EVAL_ONE_STR((char *)startup_source)\n");
  fprintf(out, "static unsigned char startup_source[] = {\n");
  
  while (1) {
    while (1) {
      c = fgetc(in);
      if (c == '"')
        break;

      if (c == EOF) {
        fprintf(out, "\n 0 };\n");
        return 0;
      }
    }

    while (1) {
      c = fgetc(in);
      if (c == '"')
        break;
      if (c == '\\')
        c = fgetc(in);
      fprintf(out, "%d,", c);
      col++;
      if (col == 20) {
        fprintf(out, "\n");
        col = 0;
      }
    }
  }

  return 0;
}
