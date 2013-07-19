
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

/* The config string after : is replaced with ! followed by a sequence
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

   For ELF binaries, the absolute values of `start', `decl_end', `prog_end',
   and `end' are ignored if a ".rackcmdl" (starter) or ".rackprog"
   (embedding) section is found. The `start' value is set to match the
   section offset, and `decl_end', `prog_end', and `end' are correspondingly
   adjusted. Using a seciton offset allows linking tools (such as
   `strip') to move the data in the executable.
*/
char *config = "cOnFiG:[***************************";

char *binary_type_hack = "bINARy tYPe:ezic";

/* This path list is used instead of the one in the Racket/GRacket
   binary. That way, the same Racket/GRacket binary can be shared
   among embedding exectuables that have different collection
   paths. */
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

static char *copy_string(char *s1)
{
  int l1;
  char *s;

  if (!s1) return NULL;

  l1 = strlen(s1);

  s  = (char *)malloc(l1 + 1);

  memcpy(s, s1, l1 + 1);

  return s;
}

static char *do_path_append(char *s1, int l1, char *s2)
{
  int l2;
  char *s;

  l2 = strlen(s2);

  s  = (char *)malloc(l1 + l2 + 2);

  memcpy(s, s1, l1);
  if (s[l1 - 1] != '/') {
    s[l1++] = '/';
  }

  memcpy(s + l1, s2, l2);
  s[l1 + l2] = 0;

  return s;
}

static char *path_append(char *s1, char *s2)
{
  return do_path_append(s1, strlen(s1), s2);
}

static int executable_exists(char *path)
{
  return (access(path, X_OK) == 0);
}

static int as_int(char *_c)
{
  unsigned char *c = (unsigned char *)_c;
  return c[0] | ((int)c[1] << 8) | ((int)c[2] << 16)  | ((int)c[3] << 24);
}

static int has_slash(char *s)
{
  while (*s) {
    if (s[0] == '/')
      return 1;
    s++;
  }
  return 0;
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

typedef unsigned short ELF__Half;
typedef unsigned int ELF__Word;
typedef unsigned long ELF__Xword;
typedef unsigned long ELF__Addr;
typedef unsigned long ELF__Off;

typedef struct { 
  unsigned char e_ident[16]; 
  ELF__Half e_type; 
  ELF__Half e_machine; 
  ELF__Word e_version; 
  ELF__Addr e_entry; 
  ELF__Off e_phoff; 
  ELF__Off e_shoff; 
  ELF__Word e_flags; 
  ELF__Half e_ehsize; 
  ELF__Half e_phentsize; 
  ELF__Half e_phnum; 
  ELF__Half e_shentsize; 
  ELF__Half e_shnum;
  ELF__Half e_shstrndx;
} ELF__Header;

typedef struct
{
  ELF__Word sh_name;
  ELF__Word sh_type;
  ELF__Xword sh_flags;
  ELF__Addr sh_addr;
  ELF__Off sh_offset;
  ELF__Xword sh_size;
  ELF__Word sh_link;
  ELF__Word sh_info;
  ELF__Xword sh_addralign;
  ELF__Xword sh_entsize;
} Elf__Shdr;

static int try_elf_section(const char *me, int *_start, int *_decl_end, int *_prog_end, int *_end)
{
  int fd, i;
  ELF__Header e;
  Elf__Shdr s;
  char *strs;

  fd = open(me, O_RDONLY, 0);
  if (fd == -1) return 0;

  if (read(fd, &e, sizeof(e)) == sizeof(e)) {
    if ((e.e_ident[0] == 0x7F)
	&& (e.e_ident[1] == 'E')
	&& (e.e_ident[2] == 'L')
	&& (e.e_ident[3] == 'F')) {

      lseek(fd, e.e_shoff + (e.e_shstrndx * e.e_shentsize), SEEK_SET);
      if (read(fd, &s, sizeof(s)) != sizeof(s)) {
	close(fd);
	return 0;
      }

      strs = (char *)malloc(s.sh_size);
      lseek(fd, s.sh_offset, SEEK_SET);
      if (read(fd, strs, s.sh_size) != s.sh_size) {
	close(fd);
	return 0;
      }

      for (i = 0; i < e.e_shnum; i++) {
	lseek(fd, e.e_shoff + (i * e.e_shentsize), SEEK_SET);
	if (read(fd, &s, sizeof(s)) != sizeof(s)) {
	  close(fd);
	  return 0;
	}
	if (!strcmp(strs + s.sh_name, ".rackcmdl")
	    || !strcmp(strs + s.sh_name, ".rackprog")) {
	  *_decl_end = (*_decl_end - *_start) + s.sh_offset;
	  *_prog_end = (*_prog_end - *_start) + s.sh_offset;
	  *_start = s.sh_offset;
	  *_end = s.sh_offset + s.sh_size;
	  close(fd);
	  return !strcmp(strs + s.sh_name, ".rackprog");
	}
      }
    }
  }

  close(fd);
  return 0;
}

int main(int argc, char **argv)
{
  char *me = argv[0], *data, **new_argv;
  char *exe_path, *lib_path, *dll_path;
  int start, decl_end, prog_end, end, count, fd, v, en, x11;
  int argpos, inpos, collcount = 1, fix_argv;

  if (config[7] == '[') {
    write_str(2, argv[0]);
    write_str(2, ": this is an unconfigured starter\n");
    return 1;
  }

  if (me[0] == '/') {
    /* Absolute path */
  } else if (has_slash(me)) {
    /* Relative path with a directory: */
    char *buf;
    long buflen = 4096;
    buf = (char *)malloc(buflen);
    me = path_append(getcwd(buf, buflen), me);
  } else {
    /* We have to find the executable by searching PATH: */
    char *path = copy_string(getenv("PATH")), *p, *m;
    int more;

    if (!path) {
      path = "";
    }

    while (1) {
      /* Try each element of path: */
      for (p = path; *p && (*p != ':'); p++) { }
      if (*p) {
	*p = 0;
	more = 1;
      } else
	more = 0;

      if (!*path)
	break;

      m = path_append(path, me);

      if (executable_exists(m)) {
	if (m[0] != '/')
	  m = path_append(getcwd(NULL, 0), m);
	me = m;
	break;
      }
      free(m);

      if (more)
	path = p + 1;
      else
	break;
    }
  }
  
  /* me is now an absolute path to the binary */

  /* resolve soft links */
  while (1) {
    int len, bufsize = 127;
    char *buf;
    buf = (char *)malloc(bufsize + 1);
    len = readlink(me, buf, bufsize);
    if (len < 0) {
      if (errno == ENAMETOOLONG) {
	/* Increase buffer size and try again: */
	bufsize *= 2;
	buf = (char *)malloc(bufsize + 1);
      } else
	break;
    } else {
      /* Resolve buf relative to me: */
      buf[len] = 0;
      buf = absolutize(buf, me);
      me = buf;
      buf = (char *)malloc(bufsize + 1);
    }
  }

  start = as_int(config + 8);
  decl_end = as_int(config + 12);
  prog_end = as_int(config + 16);
  end = as_int(config + 20);
  count = as_int(config + 24);
  x11 = as_int(config + 28);

  fix_argv = try_elf_section(me, &start, &decl_end, &prog_end, &end);

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
  new_argv = (char **)malloc((count + argc + (2 * collcount) + 10) * sizeof(char*));

  fd = open(me, O_RDONLY, 0);
  lseek(fd, prog_end, SEEK_SET);
  {
    int expected_length = end - prog_end;
    if (expected_length != read(fd, data, expected_length)) {
      printf("read failed to read all %i bytes from file %s\n", expected_length, me);
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
    dll_path = string_append(lib_path, dll_path);
    dll_path = string_append(LD_LIB_PATH "=", dll_path);
    putenv(dll_path);
  }

  new_argv[0] = me;

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

  if (fix_argv) {
    /* next three args are "-k" and numbers; fix 
       the numbers to match start, decl_end, and prog_end */
    fix_argv = argpos + 1;
  }

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
    new_argv[fix_argv] = num_to_string(start);
    new_argv[fix_argv+1] = num_to_string(decl_end);
    new_argv[fix_argv+2] = num_to_string(prog_end);
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
