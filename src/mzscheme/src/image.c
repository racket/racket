/*
  MzScheme
  Copyright (c) 2004-2005 PLT Scheme, Inc.
  Copyright (c) 1995-2001 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"

#ifdef UNIX_IMAGE_DUMPS
#include "schmach.h"
#ifdef FILES_HAVE_FDS
# include <sys/types.h>
# include <sys/time.h>
# ifdef SELECT_INCLUDE
#  include <sys/select.h>
# endif
#endif
#ifndef NO_USER_BREAK_HANDLER
# include <signal.h>
#endif
#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#ifdef UNIX_IMAGE_DUMPS
# include <ctype.h>
# include "schgc.h"
#endif

extern void *GC_get_stack_base();
#endif

MZ_DLLSPEC int (*scheme_actual_main)(int argc, char **argv);

#ifdef UNIX_IMAGE_DUMPS
static Scheme_Object *(*scheme_dump_heap)(char *filename) = NULL;
static Scheme_Object *(*scheme_load_heap)(char *filename, Scheme_Object *argvec) = NULL;
#endif

static char *no_dumps;
extern int scheme_file_open_count;
#ifdef UNIX_PROCESSES
extern void *scheme_system_children;
#endif

static Scheme_Object *dump_heap(int argc, Scheme_Object **argv);
static Scheme_Object *load_heap(int argc, Scheme_Object **argv);

void scheme_init_image(Scheme_Env *env)
{
  scheme_add_global_constant("write-image-to-file", 
			     scheme_make_prim_w_arity(dump_heap,
						      "write-image-to-file",
						      1, 2),
			     env);
  scheme_add_global_constant("read-image-from-file", 
			     scheme_make_prim_w_arity(load_heap,
						      "read-image-from-file",
						      2, 2),
			     env);
}

void scheme_no_dumps(char *why)
{
  if (why && !no_dumps)
    no_dumps = why;
}

static Scheme_Object *dump_heap(int argc, Scheme_Object **argv)
{
  char *filename;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("write-image-to-file", SCHEME_PATH_STRING_STR, 0, argc, argv);
  if (argc > 1)
    if (!SCHEME_FALSEP(argv[1]))
      scheme_check_proc_arity("write-image-to-file", 0,
			      1, argc, argv);

  filename = scheme_expand_string_filename(argv[0], 
					   "write-image-to-file", NULL,
					   SCHEME_GUARD_FILE_WRITE);

#ifdef UNIX_IMAGE_DUMPS
  if (scheme_dump_heap) {
    if (no_dumps) {
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "write-image-to-file: image cannot be saved; %s", 
		       no_dumps);
      return NULL;
    } else if (scheme_file_open_count) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "write-image-to-file: a file, process, or TCP port is open (%d)",
		       scheme_file_open_count);
      return NULL;
#ifdef UNIX_PROCESSES
    } else if (scheme_system_children) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "write-image-to-file: a subprocess is still active");
      return NULL;
#endif
    } else {
      Scheme_Object *v;
      v = scheme_dump_heap(filename);
      if (!v) {
	if (argc > 1) {
	  if (SCHEME_FALSEP(argv[1]))
	    exit(0);
	  else
	    return _scheme_tail_apply(argv[1], 0, NULL);
	} else
	  return scheme_void;
      } else
	return v;
    }
  }
#endif
  
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "write-image-to-file: not supported");
  return NULL;
}

static Scheme_Object *load_heap(int argc, Scheme_Object **argv)
{
  char *filename;
  int bad = 0;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("read-image-from-file", SCHEME_PATH_STRING_STR, 0, argc, argv);

  if (SCHEME_VECTORP(argv[1])) {
    Scheme_Object **a;
    int i;

    a = SCHEME_VEC_ELS(argv[1]);
    for (i = SCHEME_VEC_SIZE(argv[1]); i--; ) {
      if (!SCHEME_BYTE_STRINGP(a[i])) {
	bad = 1;
	break;
      }
    }
  } else
    bad = 1;
      
  if (bad)
    scheme_wrong_type("read-image-from-file", "vector of strings", 0, argc, argv);

  filename = scheme_expand_string_filename(argv[0],
					   "read-image-from-file", NULL,
					   SCHEME_GUARD_FILE_READ);

#ifdef UNIX_IMAGE_DUMPS
  if (scheme_load_heap)
    scheme_load_heap(filename, argv[1]);
#endif

  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "read-image-from-file: not supported");
  return NULL;
}

/******************************************************************************/

#ifdef UNIX_IMAGE_DUMPS

/* XXX_SKIP_PLT makes images sortof work for dynamically-linked programs */

#if defined(sun) && defined(ECHRNG)
# define SOLARIS_SKIP_PLT
#endif

#if 0 && defined(__FreeBSD__)
# define FREEBSD_SKIP_PLT
#endif

static char *restore_brk;

static unsigned long stack_base;
static unsigned long orig_brk;
static int data_count;
static unsigned long *data_starts;
static unsigned long *data_ends;
typedef unsigned long ptr_t;
typedef unsigned long word;

static Scheme_Jumpup_Buf **restore_launch_ubuf;

extern char **environ;

#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/param.h>

#include "../gc/include/private/gcconfig.h"

static void die_now(char *phase, char *file)
{
  printf("Restore from \"%s\" failed at %s (%d)\n", file, phase, errno);
  exit(-1);
}

#if defined(linux) || defined(__FreeBSD__)
# define FIND_WRITEABLE_SECTION
#endif

#ifdef FIND_WRITEABLE_SECTION
static unsigned long current_value;
static mz_jmp_buf goback;

static void bus_error(int ignore)
{
  scheme_longjmp(goback, 1);
}
#endif

static void  do_restore_env_argv(long orig_len, long len, 
				 char *start, char *carry, mz_jmp_buf b)
{
  char buffer[1024];

  if (len < 0) {
    long l, i;
    Scheme_Object *v;

    FLUSH_REGISTER_WINDOWS;

    if ((unsigned long)carry < (unsigned long)start)
      start = carry;

    memcpy(start, restore_brk + sizeof(long), orig_len);
    brk(restore_brk);

    l = *(long *)start;
    start += sizeof(long);
    environ = malloc(sizeof(char *) * (l + 1));
    environ[l] = NULL;
    for (i = 0; i < l; i++) {
      int l = strlen(start);
      environ[i] = malloc(l + 1);
      memcpy(environ[i], start, l + 1);
      start += l + 1;
    }

    /* align for long */
    if ((long)start & (sizeof(long) - 1))
      start += (sizeof(long) - ((long)start & (sizeof(long) - 1)));

    l = *(long *)start;
    start += sizeof(long);
    v = scheme_make_vector(l, scheme_null);
    for (i = 0; i < l; i++) {
      SCHEME_VEC_ELS(v)[i] = scheme_make_string(start);
      start += strlen(start) + 1;
    }

    scheme_longjmp(b, (long)v);
  } else
    do_restore_env_argv(orig_len, len - 1024, 
			start ? start : buffer, buffer, b);
}

static Scheme_Object *restore_env_argv(long len)
{
  mz_jmp_buf buf;
  Scheme_Object *v;

  /* We're going to trash the stack, so we'll need to escape */
  v = (Scheme_Object *)scheme_setjmp(buf);
  
  if (!v)
    do_restore_env_argv(len, len, NULL, NULL, buf);

  return v;
}

static Scheme_Object *dump_image(char *filename)
{
  Scheme_Jumpup_Buf *buf;
  Scheme_Object *v;

  buf = (Scheme_Jumpup_Buf *)scheme_malloc(sizeof(Scheme_Jumpup_Buf));
  scheme_init_jmpup_buf(buf);

  if (!scheme_setjmpup(buf, buf, (void *)stack_base)) {
    unsigned long current_brk = (unsigned long)sbrk(0);
    int fd;
    
    do {
      fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    } while ((fd == -1) && (errno == EINTR));

    if (fd == -1) {
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "write-image-to-file: couldn't write file \"%q\"", 
		       filename);
    } else {
      int i;
      const char *machine = scheme_system_library_subpath();
      unsigned char len = strlen(machine);

      write(fd, (char *)&len, sizeof(char));
      write(fd, (char *)machine, len);

      len = strlen(MZSCHEME_VERSION);
      write(fd, (char *)&len, sizeof(char));
      write(fd, (char *)MZSCHEME_VERSION, len);
      
      write(fd, (char *)&data_count, sizeof(int));
      for (i = 0; i < data_count; i++) {
	write(fd, (char *)&(data_starts[i]), sizeof(unsigned long));
	write(fd, (char *)&(data_ends[i]), sizeof(unsigned long));
      }
      write(fd, (char *)&orig_brk, sizeof(unsigned long));
      write(fd, (char *)&stack_base, sizeof(unsigned long));
      
      for (i = 0; i < data_count; i++) {
	write(fd, (char *)data_starts[i], data_ends[i] - data_starts[i]);
      }

      write(fd, (char *)&current_brk, sizeof(unsigned long));
      write(fd, (char *)orig_brk, current_brk - orig_brk);
      write(fd, (char *)&buf, sizeof(Scheme_Jumpup_Buf *));
  
      close(fd);
    }

    v = NULL;
  } else {
    /* We've been restored. environ contains a pointer to environment + argv. */
    long len;

    len = *(long *)restore_brk;
    v = restore_env_argv(len);
  }

  /* zero it back out for GC */
  scheme_init_jmpup_buf(buf);

  return v;
}

static void mismatch(int which, unsigned long a, unsigned long b, char *file, void (*die)(char *c1, char *c2))
{
  char buffer[256];
  errno = which;
  sprintf(buffer, "setup mismatch [%lx vs %lx]", a, b);
  die(buffer, file);
}

static void do_restore_image(char *file, int argc, char **argv, 
			     Scheme_Jumpup_Buf **ubuf,
			     void (*die)(char *c1, char *c2),
			     long env_len, char *env_space1, char *env_space2)
{
# define MACHVERS_MAX_LEN 100
  int fd;
  int i, count;
  long argv_len;
  unsigned long current_brk;
  unsigned long saved_data_start, saved_data_end, saved_orig_brk, saved_stack_base;
  int saved_data_count;
  const char *machine = scheme_system_library_subpath();
  char machvers[MACHVERS_MAX_LEN];
  unsigned char len;
  char **save_environ;

  do {
    fd = open(file, O_RDONLY);
  } while ((fd == -1) && (errno == EINTR));

  if (fd == -1)
    die("open", file);

  if (read(fd, (char *)&len, sizeof(char)) != sizeof(char)) {
    close(fd);
    die("getting machine type", file);
  }
  if (len >= MACHVERS_MAX_LEN) {
    close(fd);
    die("getting machine type", file);
  }
  if (read(fd, (char *)machvers, len) != len) {
    close(fd);
    die("getting machine type", file);
  }
  machvers[len] = 0;
  if (strcmp(machvers, machine)) {
    char buffer[256];
    sprintf(buffer, "machine type: image is %s, this is %s", machvers, machine);
    die(buffer, file);
  }

  
  if (read(fd, (char *)&len, sizeof(char)) != sizeof(char)) {
    close(fd);
    die("getting version", file);
  }
  if (len >= MACHVERS_MAX_LEN) {
    close(fd);
    die("getting version", file);
  }
  if (read(fd, (char *)machvers, len) != len) {
    close(fd);
    die("getting version", file);
  }
  machvers[len] = 0;
  if (strcmp(machvers, MZSCHEME_VERSION)) {
    char buffer[256];
    sprintf(buffer, "version: image is %s, this is %s", machvers, machine);
    die(buffer, file);
  }

  if (read(fd, (char *)&saved_data_count, sizeof(int)) != sizeof(int)) {
    close(fd);
    die("setup", file);
  }
  if (saved_data_count != data_count) {
    close(fd);
    mismatch(-1, saved_data_count, data_count, file, die);
  }
  for (i = 0; i < data_count; i++) {
    if (read(fd, (char *)&saved_data_start, sizeof(unsigned long)) != sizeof(unsigned long)
	|| read(fd, (char *)&saved_data_end, sizeof(unsigned long)) != sizeof(unsigned long)) {
      close(fd);
      die("setup", file);
    }
    if (saved_data_start != data_starts[i]) {
      close(fd);
      mismatch(-10 * (i + 1), saved_data_start, data_starts[i], file, die);
    }
    if (saved_data_end != data_ends[i]) {
      close(fd);
      mismatch(-1000 + -10 * (i + 1), saved_data_end, data_ends[i], file, die);
    }
  }

  if (read(fd, (char *)&saved_orig_brk, sizeof(unsigned long)) != sizeof(unsigned long)
      || read(fd, (char *)&saved_stack_base, sizeof(unsigned long)) != sizeof(unsigned long)) {
    close(fd);
    die("setup", file);
  }

  if (saved_orig_brk != orig_brk) {
    close(fd);
    mismatch(-3, saved_orig_brk, orig_brk, file, die);
  }
  if (saved_stack_base != stack_base) {
    close(fd);
    mismatch(-4, saved_stack_base, stack_base, file, die);
  }

   if ((unsigned long)env_space1 > (unsigned long)env_space2)
     env_space1 = env_space2;

  save_environ = (char **)env_space1;
  for (i = 0; environ[i]; i++) {}
  env_space1 += i * sizeof(char *);
  for (i = 0; environ[i]; i++) {
    int l;
    save_environ[i] = env_space1;
    l = strlen(environ[i]);
    memcpy(save_environ[i], environ[i], l + 1);
    env_space1 += l + 1;
  }
  count = i;

  for (i = 0; i < data_count; i++) {
    if (read(fd, (char *)data_starts[i], data_ends[i] - data_starts[i]) != (data_ends[i] - data_starts[i])) 
      die_now("data", file);
  }
  if (read(fd, (char *)&current_brk, sizeof(unsigned long)) != sizeof(unsigned long)) 
    die_now("data position", file);
  brk((void *)current_brk);
  if (read(fd, (char *)orig_brk, current_brk - orig_brk) != (current_brk - orig_brk))
    die_now("dynamic data", file);
  if (read(fd, (char *)ubuf, sizeof(Scheme_Jumpup_Buf *)) != sizeof(Scheme_Jumpup_Buf *)) 
    die_now("stack", file);

  close(fd);

  /* Use sbrk to make room for env and argv; this will have to be restored
     in a tricky way. */

  argv_len = sizeof(long);
  for (i = 0; i < argc; i++) {
    argv_len += strlen(argv[i]) + 1;
  }

  env_len += sizeof(long);

  restore_brk = sbrk(env_len + argv_len + 2 * sizeof(long));

  env_space1 = restore_brk;
  *(long *)env_space1 = env_len + argv_len + sizeof(long);
  env_space1 += sizeof(long);

  *(long *)env_space1 = count;
  env_space1 += sizeof(long);
  for (i = 0; i < count; i++) {
    int l = strlen(save_environ[i]);
    memcpy(env_space1, save_environ[i], l + 1);
    env_space1 += l + 1;
  }

  /* align for long */
  if ((long)env_space1 & (sizeof(long) - 1))
    env_space1 += (sizeof(long) - ((long)env_space1 & (sizeof(long) - 1)));

  *(long *)env_space1 = argc;
  env_space1 += sizeof(long);
  for (i = 0; i < argc; i++) {
    int l = strlen(argv[i]);
    memcpy(env_space1, argv[i], l + 1);
    env_space1 += l + 1;
  }

  scheme_longjmpup(*ubuf);
}

static void do_restore_image_with_space(char *file, int argc, char **argv, 
					Scheme_Jumpup_Buf **ubuf,
					void (*die)(char *c1, char *c2),
					long orig_len, long len, void *start, void *carry)
{
  char buffer[1024];

  if (len < 1024)
    do_restore_image(file, argc, argv, ubuf, die, orig_len, (char *)start, buffer);
  else
    do_restore_image_with_space(file, argc, argv, ubuf, die, orig_len, len - 1024, start, buffer);
}

static void restore_image(char *file, int argc, char **argv, 
			  Scheme_Jumpup_Buf **ubuf,
			  void (*die)(char *c1, char *c2))
{
  int i;
  long len = 0;

  for (i = 0; environ[i]; i++) {
    len += strlen(environ[i]) + 1;
  }

  len += sizeof(char **) * (i + 1);

  do_restore_image_with_space(file, argc, argv, ubuf, die, len, len, &len, &len);
}


static void read_image_exn(char *phase, char *file)
{
  scheme_raise_exn(MZEXN_FAIL,
		   "read-image-from-file: restore from \"%q\" failed at %s (%d).", 
		   file, phase, errno);
}

static Scheme_Object *load_image(char *filename, Scheme_Object *argvec)
{
# define MAX_ARGV 20
# define MAX_ARGLEN 2048
  char *argv[MAX_ARGV], argspace[MAX_ARGLEN + MAX_ARGV], *s;
  int i, count;
  long l;
  Scheme_Object **a;
  
  count = SCHEME_VEC_SIZE(argvec);
  if (count > MAX_ARGV)
    scheme_raise_exn(MZEXN_FAIL,
		     "read-image-from-file: too many string arguments; "
		     "maximum is %d", MAX_ARGV);

  l = 0;

  a = SCHEME_VEC_ELS(argvec);
  for (i = count; i--; ) {
    l += SCHEME_STRTAG_VAL(a[i]);
  }
  a = NULL;

  if (l > MAX_ARGLEN)
    scheme_raise_exn(MZEXN_FAIL,
		     "read-image-from-file: string arguments too long; "
		     "maximum total length is %d", MAX_ARGLEN);

  s = argspace;
  for (i = 0; i < count; i++) {
    l = SCHEME_STRTAG_VAL(a[i]);
    memcpy(s, SCHEME_STR_VAL(a[i]), l + 1);
    argv[i] = s;
    s += l + 1;
  }

  restore_image(filename, count, argv, restore_launch_ubuf, read_image_exn);

  return NULL;
}

#ifdef SOLARIS_SKIP_PLT
#include <sys/link.h>
unsigned long plt_start, plt_end;
void find_plt()
{
  extern Elf32_Dyn _DYNAMIC;
  Elf32_Dyn *dp;
  int tag;

  for (dp = ((Elf32_Dyn *)(&_DYNAMIC)); (tag = dp->d_tag) != DT_NULL; dp++) {
    if (tag == DT_PLTGOT) {
      plt_start = (unsigned long)dp->d_un.d_val;
    } else if (tag == DT_PLTRELSZ) {
      plt_end = (unsigned long)dp->d_un.d_val;
    }
  }

  plt_end += plt_start;
}
#endif

#ifdef FREEBSD_SKIP_PLT
#include <sys/types.h>
#include <nlist.h>
#include <link.h>
static unsigned long plt_start, plt_end;
extern struct _dynamic _DYNAMIC;
void find_plt()
{
  plt_start = _DYNAMIC.d_un.d_sdt->sdt_plt;
  plt_end = plt_start + _DYNAMIC.d_un.d_sdt->sdt_plt_sz;
}
#endif

int scheme_image_main(int argc, char **argv)
{
  Scheme_Jumpup_Buf *buf;

  data_count = 1;
  data_starts = malloc(sizeof(unsigned long));
  data_ends = malloc(sizeof(unsigned long));

  stack_base = (unsigned long)GC_get_stack_base();
  data_starts[0] = (unsigned long)DATASTART;
  data_ends[0] = (unsigned long)DATAEND;
  orig_brk = data_ends[0];

#if defined(SOLARIS_SKIP_PLT) || defined(FREEBSD_SKIP_PLT)
  {
    unsigned long ds, de;

    ds = data_starts[0];
    de = data_ends[0];
    data_starts = malloc(2 * sizeof(unsigned long));
    data_ends = malloc(2 * sizeof(unsigned long));
    data_count = 2;

    find_plt();
    
    data_starts[0] = ds;
    data_ends[0] = plt_start;
    data_starts[1] = plt_end;
    data_ends[1] = de;
  }
#endif

  scheme_dump_heap = dump_image;
  scheme_load_heap = load_image;

#ifdef FIND_WRITEABLE_SECTION
  /* Find writeable section of text segment: */
  {
    unsigned long ds;

    MZ_SIGSET(SIGBUS, bus_error);
    MZ_SIGSET(SIGSEGV, bus_error);

    ds = data_starts[0];

    if (!scheme_setjmp(goback)) {
      for (current_value = data_ends[0]; (current_value -= sizeof(long)) > ds; ) {
	*(unsigned long *)current_value = *(unsigned long *)current_value;
      }
    }
    data_starts[0] = current_value + sizeof(long);
    MZ_SIGSET(SIGBUS, SIG_DFL);
    MZ_SIGSET(SIGSEGV, SIG_DFL);
  }
#endif

  if ((argc > 1) && (!strcmp(argv[1], "--restore")
		     || !scheme_strncmp(argv[1], "-R", 2))) {
    char *file, *exfile;
    int startargs;
    
    if (argv[1][1] == 'R') {
      file = argv[1] + 2;
      if (!*file) {
	printf("%s: Missing file name for -R.\n", argv[0]);
	printf("Use the --help or -h flag for help.\n");
	return -1;
      }
      startargs = 2;
    } else if (argc < 3) {
      printf("%s: Missing file name for --restore.\n", argv[0]);
      printf("Use the --help or -h flag for help.\n");
      return -1;
    } else {
      file = argv[2];
      startargs = 3;
    }

    exfile = file; /* scheme_expand_filename(file, strlen(file), NULL, NULL); */
    if (!exfile)
      die_now("bad filename", file);

    restore_image(exfile, argc - startargs, argv + startargs, &buf, die_now);

    die_now("stack jump", file);
  }

  restore_launch_ubuf = &buf;

  {
    /* Make a copy of argv so that it's definitely in the normal heap: */
    char **naya_argv;
    int i;
    
    naya_argv = scheme_malloc(argc * sizeof(char *));
    for (i = 0; i < argc; i++) {
      naya_argv[i] = scheme_strdup(argv[i]);
    }

    /* doesn't support atexit(): */
    _exit(scheme_actual_main(argc, naya_argv));
  }

  return -1;
}

#endif

/******************************************************************************/

#ifndef UNIX_IMAGE_DUMPS

int scheme_image_main(int argc, char **argv)
{
  return scheme_actual_main(argc, argv);
}

#endif
