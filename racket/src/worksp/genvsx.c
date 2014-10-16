#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* Generate racketX.sln from racket.sln, etc., to match the 
   current VS tool chain */

static const char *solutions[] = { "gracket/gracket.sln",
                                   "mzstart/mzstart.sln",
                                   "mrstart/mrstart.sln",
                                   "racket/racket.sln",
                                   "mzcom/mzcom.sln",
                                   NULL };

static const char *projects[] = { "gracket/gracket.vcxproj",
                                  "libracket/libracket.vcxproj",
                                  "mzstart/mzstart.vcxproj",
                                  "libffi/libffi.vcxproj",
                                  "mrstart/mrstart.vcxproj",
                                  "racket/racket.vcxproj",
                                  "libmzgc/libmzgc.vcxproj",
                                  "mzcom/mzcom.vcxproj",
                                  "sgc/sgc.vcxproj",
                                  NULL };

static const char *tool_prefix = "<PlatformToolset>v";
static const char *filename_suffix = ".vcxproj";

static void adjust_file(const char *fn, const char *vers) {
  char *new_fn;
  int i, j, tool_pos, filename_pos;
  FILE *f, *new_f;

  new_fn = malloc(strlen(fn) + 2);
  for (i = 0, j = 0; fn[i]; i++, j++) {
    if (fn[i] == '.')
      new_fn[j++] = 'X';
    new_fn[j] = fn[i];
  }
  new_fn[j] = 0;

  f = fopen(fn, "r");
  new_f = fopen(new_fn, "w");

  tool_pos = 0;
  filename_pos = 0;
  while (1) {
    i = fgetc(f);

    if (i == EOF)
      break;

    if (i == tool_prefix[tool_pos])
      tool_pos++;
    else
      tool_pos = 0;

    if (i && (i == filename_suffix[filename_pos])) {
      /* don't write potential suffix until we know whether it matches... */
      filename_pos++;
    } else {
      if (filename_pos) {
        if (!filename_suffix[filename_pos]) {
          /* found matching suffix, so add "X" before: */
          fwrite("X", 1, 1, new_f);
        }
        fwrite(filename_suffix, 1, filename_pos, new_f);
        filename_pos = 0;
      }
      fputc(i, new_f);
    }

    if (!tool_prefix[tool_pos]) {
      /* found matching tool prefix, so adjust version */
      fwrite(vers, 1, strlen(vers), new_f);
      tool_pos = 0;
      fgetc(f); fgetc(f); fgetc(f); /* = "100" */
    }
  }

  fclose(f);
  fclose(new_f);
}

int main() {
  const char *vers = "100";
  int i;

#if _MSC_VER >= 1800
  /* VS 2013 */
  vers = "120";
#elif _MSC_VER >= 1700
  /* VS 2012 */
  vers = "110";
#else
  /* VS 2010 or earlier */
  return 0;
#endif

  for (i = 0; solutions[i]; i++) {
    adjust_file(solutions[i], vers);
  }
  for (i = 0; projects[i]; i++) {
    adjust_file(projects[i], vers);
  }

  return 1;
}
