#ifndef _MSC_VER
# include <unistd.h>
#endif
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef _MSC_VER
# include <Windows.h>
# define DOS_FILE_SYSTEM
static int scheme_utf8_encode(unsigned int *path, int zero_offset, int len,
			      char *dest, int dest_len, int get_utf16);
#endif
#include "boot.h"

#define MZ_CHEZ_SCHEME
#ifndef INITIAL_BIN_TYPE
# define INITIAL_BIN_TYPE "zi"
#endif
#ifndef RACKET_IS_GUI
# define RACKET_IS_GUI 0
#endif

#include "../../start/config.inc"

char *boot_file_data = "BooT FilE OffsetS:xxxxyyyyyzzzz";
static int boot_file_offset = 18;

#ifdef OS_X
# include <mach-o/dyld.h>
static char *get_self_path()
{
  char buf[1024], *s;
  uint32_t size = sizeof(buf);
  int r;
  
  r = _NSGetExecutablePath(buf, &size);
  if (!r)
    return strdup(buf);
  else {
    s = malloc(size);
    r = _NSGetExecutablePath(s, &size);
    if (!r)
      return s;
    fprintf(stderr, "failed to get self\n");
    exit(1);
  }
}
#endif

#if defined(__linux__)
# include <errno.h>
static char *get_self_path()
{
  char buf[256], *s = buf;
  ssize_t len, blen = sizeof(buf);

  while (1) {
    len = readlink("/proc/self/exe", s, blen-1);
    if (len == (blen-1)) {
      if (s != buf) free(s);
      blen *= 2;
      s = malloc(blen);
    } else if (len < 0) {
      fprintf(stderr, "failed to get self (%d)\n", errno);
      exit(1);
    } else
      break;
  }
  buf[len] = 0;
  return strdup(buf);
}
#endif

#ifdef _MSC_VER
static char *get_self_path()
{
  wchar_t *p = get_self_executable_path();
  char *r;
  int len;

  len = WideCharToMultiByte(CP_UTF8, 0, p, -1, NULL, 0, NULL, NULL);
  r = malloc(len);
  len = WideCharToMultiByte(CP_UTF8, 0, p, -1, r, len, NULL, NULL);

  return r;
}

static int scheme_utf8_encode(unsigned int *path, int zero_offset, int len,
			      char *dest, int dest_len, int get_utf16)
{
  return WideCharToMultiByte(CP_UTF8, 0, (wchar_t *)path, len, dest, dest_len, NULL, NULL);
}
#endif

#ifdef NO_GET_SEGMENT_OFFSET
static long get_segment_offset()
{
  return 0;
}
#endif

#ifndef do_pre_filter_cmdline_arguments
# define do_pre_filter_cmdline_arguments(argc, argv) /* empty */
#endif

int main(int argc, char **argv)
{
  char *self, *prog = argv[0], *sprog = NULL;
  int pos1, pos2, pos3;
  long segment_offset;

  do_pre_filter_cmdline_arguments(&argc, &argv);

  argc--;
  argv++;

  extract_built_in_arguments(&prog, &sprog, &argc, &argv);
  segment_offset = get_segment_offset();

  self = get_self_path();

  memcpy(&pos1, boot_file_data + boot_file_offset, sizeof(pos1));
  memcpy(&pos2, boot_file_data + boot_file_offset + 4, sizeof(pos2));
  memcpy(&pos3, boot_file_data + boot_file_offset + 8, sizeof(pos2));

  racket_boot(argc, argv, self, segment_offset,
              extract_coldir(), extract_configdir(),
              pos1, pos2, pos3,
              RACKET_IS_GUI);
  
  return 0;
}
