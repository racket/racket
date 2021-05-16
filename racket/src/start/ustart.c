
/* "Embedding" program for Unix/X11, to be used as
   an alternative to embedding in the actual Racket
   or GRacket binary. */

#include <sys/types.h>
#include <sys/uio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>

#if defined(__GNUC__)
# define PRESERVE_IN_EXECUTABLE __attribute__((used))
#else
# define PRESERVE_IN_EXECUTABLE /* empty */
#endif

/* The config string after : is replaced with ! or * followed by a sequence
   of little-endian 4-byte ints:
    start - offset into the binary
    prog_end - offset; start to prog_end is the program region
    decl_end - offset; prog_end to decl_end is the module-command region
    end - offset; prog_end to end is the complete command region
    count - number of cmdline args in command region
    x11? - non-zero => launches GRacket for X

    In the command region, the format is a sequence of NUL-terminated strings:
     exe_path - program to start (relative is w.r.t. executable)
     dll_path - DLL directory if non-empty (relative is w.r.t. executable)
     cmdline_arg ...

   A * instead of ! at the start means that `-E` should be skipped,
   so that `(find-system-path 'exec-file)` refers to the started
   executable instaed of this starter.

   For ELF binaries, the absolute values of `start', `decl_end', `prog_end',
   and `end' are ignored if a ".rackcmdl" (starter) or ".rackprog"
   (embedding) section is found. The `start' value is set to match the
   section offset, and `decl_end', `prog_end', and `end' are correspondingly
   adjusted. Using a section offset allows linking tools (such as
   `strip') to move the data in the executable.
*/
PRESERVE_IN_EXECUTABLE
char *config = "cOnFiG:[***************************";

PRESERVE_IN_EXECUTABLE
char *binary_type_hack = "bINARy tYPe:ezic";

/* This path list is used instead of the one in the Racket/GRacket
   binary. That way, the same Racket/GRacket binary can be shared
   among embedding exectuables that have different collection
   paths. */
PRESERVE_IN_EXECUTABLE
char *_coldir = "coLLECTs dIRECTORy:" /* <- this tag stays, so we can find it again */
                "../collects"
                "\0\0" /* <- 1st nul terminates path, 2nd terminates path list */
                /* Pad with at least 1024 bytes: */
                "****************************************************************"
		"****************************************************************"
		"****************************************************************"
		"****************************************************************"
		"****************************************************************"
		"****************************************************************"
		"****************************************************************"
		"****************************************************************"
		"****************************************************************"
		"****************************************************************"
		"****************************************************************"
		"****************************************************************"
		"****************************************************************"
		"****************************************************************"
		"****************************************************************"
		"****************************************************************";
static int _coldir_offset = 19; /* Skip permanent tag */

PRESERVE_IN_EXECUTABLE
char * volatile _configdir = "coNFIg dIRECTORy:" /* <- this tag stays, so we can find it again */
                       "../etc"
                       "\0"
                       /* Pad with at least 1024 bytes: */
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************";
static int _configdir_offset = 17; /* Skip permanent tag */

#define XFORM_SKIP_PROC /* empty */
#define USE_EXE_LOOKUP_VIA_PATH
#include "self_exe.inc"

typedef struct {
  char *flag;
  int arg_count;
} X_flag_entry;

static X_flag_entry X_flags[] = {
  { "-display", 1 },
  { "-geometry", 1 },
  { "-bg", 1 },
  { "-background", 1 },
  { "-fg", 1 },
  { "-foreground", 1 },
  { "-fn", 1 },
  { "-font", 1 },
  { "-iconic", 0 },
  { "-name", 1 },
  { "-rv", 0 },
  { "-reverse", 0 },
  { "+rv", 0 },
  { "-selectionTimeout", 1 },
  { "-synchronous", 0 },
  { "-title", 1 },
  { "-xnllanguage", 1 },
  { "-xrm", 1 },
  { "-singleInstance", 0 },
  { NULL, 0 }
};

static int is_x_flag(char *s)
{
  X_flag_entry *x = X_flags;

  while (x->flag) {
    if (!strcmp(x->flag, s))
      return x->arg_count + 1;
    x++;
  }

  return 0;
}

static int write_str(int fd, char *s)
{
  return write(fd, s, strlen(s));
}

static char *num_to_string(int n)
{
  if (!n)
    return "0";
  else {
    char *d = (char *)malloc(20) + 19;
    *d = 0;
    while (n) {
      d--;
      *d = (n % 10) + '0';
      n = n / 10;
    }
    return d;
  }
}

static char *string_append(char *s1, char *s2)
{
  int l1, l2;
  char *s;

  l1 = strlen(s1);
  l2 = strlen(s2);

  s  = (char *)malloc(l1 + l2 + 1);

  memcpy(s, s1, l1);
  memcpy(s + l1, s2, l2);
  s[l1 + l2] = 0;

  return s;
}

static int as_int(char *_c)
{
  unsigned char *c = (unsigned char *)_c;
  return c[0] | ((int)c[1] << 8) | ((int)c[2] << 16)  | ((int)c[3] << 24);
}

char *absolutize(char *p, char *d)
{
  int l1;

  if (!p[0])
    return p;

  if (p[0] == '/')
    return p;
  
  /* Strip filename off d: */
  l1 = strlen(d);
  while (l1 && (d[l1- 1] != '/')) {
    l1--;
  }
  if (l1)
    return do_path_append(d, l1, p);
  else
    return p;
}

static char *next_string(char *s)
{
  return s + strlen(s) + 1;
}

static int try_section_shift(const char *me, int *_start, int *_decl_end, int *_prog_end, int *_end)
{
  int start = 0, end = 0;
  int is_prog;
  
#ifdef OS_X
  {
    long len = 0;
    start = find_mach_o_segment("__PLTSCHEME", &len);
    end = start + len;
    is_prog = 1;
  }
#else
  is_prog = find_elf_section_offset(me, ".rackprog", &start, &end);
#endif

  if (start != 0) {
    *_decl_end = (*_decl_end - *_start) + start;
    *_prog_end = (*_prog_end - *_start) + start;
    *_start = start;
    *_end = end;
  }

  return is_prog;
}

int main(int argc, char **argv)
{
  char *me = argv[0], *embedding_me, *data, **new_argv;
  char *exe_path, *lib_path, *dll_path;
  int start, decl_end, prog_end, end, count, fd, v, en, x11;
  int argpos, inpos, collcount = 1, fix_argv;
  int bufsize = 127;

  if (config[7] == '[') {
    write_str(2, argv[0]);
    write_str(2, ": this is an unconfigured starter\n");
    return 1;
  }

  me = lookup_exe_via_path(me);
  
  /* me is now an absolute path to the binary */

  /* resolve soft links */
  while (1) {
    int len;
    char *buf;
    buf = (char *)malloc(bufsize + 1);
    len = readlink(me, buf, bufsize);
    if (len < 0) {
      if (errno == ENAMETOOLONG) {
	/* Increase buffer size and try again: */
	bufsize *= 2;
      } else
	break;
    } else {
      /* Resolve buf relative to me: */
      buf[len] = 0;
      buf = absolutize(buf, me);
      me = buf;
    }
  }

  /* use `me` for `-k`, unless we have a way to more directly get the
     executable file that contains embedded code; if we do, then
     argv[0] doesn't have to match the executable */
  embedding_me = get_self_path(me);

  start = as_int(config + 8);
  decl_end = as_int(config + 12);
  prog_end = as_int(config + 16);
  end = as_int(config + 20);
  count = as_int(config + 24);
  x11 = as_int(config + 28);

  try_section_shift(embedding_me, &start, &decl_end, &prog_end, &end);

  {
    int offset, len;
    offset = _coldir_offset;
    while (1) {
      len = strlen(_coldir + offset);
      offset += len + 1;
      if (!_coldir[offset])
	break;
      collcount++;
    }
  }

  data = (char *)malloc(end - prog_end);
  new_argv = (char **)malloc((count + argc + (2 * collcount) + 15) * sizeof(char*));

  fd = open(embedding_me, O_RDONLY, 0);
  lseek(fd, prog_end, SEEK_SET);
  {
    int expected_length = end - prog_end;
    if (expected_length != read(fd, data, expected_length)) {
      printf("read failed to read all %i bytes from file %s at offset %d\n", expected_length, embedding_me, prog_end);
      abort();
    }
  }
  close(fd);
  
  exe_path = data;
  data = next_string(data);

  lib_path = data;
  data = next_string(data);

  exe_path = absolutize(exe_path, me);
  lib_path = absolutize(lib_path, me);

# ifdef OS_X
#  define LD_LIB_PATH "DYLD_LIBRARY_PATH"
# else
#  define LD_LIB_PATH "LD_LIBRARY_PATH"
# endif

  if (*lib_path) {
    dll_path = getenv(LD_LIB_PATH);
    if (!dll_path) {
      dll_path = "";
    }
    dll_path = string_append(dll_path, ":");
    dll_path = string_append(dll_path, lib_path);
    dll_path = string_append(LD_LIB_PATH "=", dll_path);
    putenv(dll_path);
  }

  new_argv[0] = exe_path;

  argpos = 1;
  inpos = 1;

  /* Keep all X11 flags to the front: */
  if (x11) {
    int n;
    while (inpos < argc) {
      n = is_x_flag(argv[inpos]);
      if (!n)
	break;
      if (inpos + n > argc) {
	write_str(2, argv[0]);
	write_str(2, ": missing an argument for ");
	write_str(2, argv[inpos]);
	write_str(2, "\n");
	return 1;
      }
      while (n--) {
	new_argv[argpos++] = argv[inpos++];
      }
    }
  }

  if (config[7] != '*') {
    /* Add -E flag; we can't just put `me` in `argv[0]`, because some
       OSes (well, just OpenBSD) cannot find the executable path of a
       process, and the actual executable may be needed to find embedded
       boot images. */
    new_argv[argpos++] = "-E";
    new_argv[argpos++] = me;
  }
  new_argv[argpos++] = "-N";
  new_argv[argpos++] = me;

  /* Add -X and -S flags */
  {
    int offset, len;
    offset = _coldir_offset;
    new_argv[argpos++] = "-X";
    new_argv[argpos++] = absolutize(_coldir + offset, me);
    while (1) {
      len = strlen(_coldir + offset);
      offset += len + 1;
      if (!_coldir[offset])
	break;
      new_argv[argpos++] = "-S";
      new_argv[argpos++] = absolutize(_coldir + offset, me);
    }
  }

  /* Add -G flag */
  new_argv[argpos++] = "-G";
  new_argv[argpos++] = absolutize(_configdir + _configdir_offset, me);

  if (count && !strcmp(data, "-k")) {
    /* next four args are "-k" and numbers; leave room to insert the
       filename in place of "-k", and fix the numbers to match start,
       decl_end, and prog_end */
    new_argv[argpos++] = "-Y";
    fix_argv = argpos;
  } else
    fix_argv = 0;

  /* Add built-in flags: */
  while (count--) {
    new_argv[argpos++] = data;
    data = next_string(data);
  }

  /* Propagate new flags (after the X11 flags) */
  while (inpos < argc) {
    new_argv[argpos++] = argv[inpos++];
  }

  new_argv[argpos] = NULL;

  if (fix_argv) {
    new_argv[fix_argv] = embedding_me;
    new_argv[fix_argv+1] = num_to_string(start);
    new_argv[fix_argv+2] = num_to_string(decl_end);
    new_argv[fix_argv+3] = num_to_string(prog_end);
  }

  /* Execute the original binary: */

  v = execv(exe_path, new_argv);
  en = errno;

  write_str(2, argv[0]);
  write_str(2, ": failed to start ");
  write_str(2, exe_path);
  write_str(2, " (");
  write_str(2, strerror(en));
  write_str(2, ")\n");
  if (*lib_path) {
    write_str(2, " used library path ");
    write_str(2, lib_path);
    write_str(2, "\n");
  }

  return v;
}
