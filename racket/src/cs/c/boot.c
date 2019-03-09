#ifndef WIN32
# include <unistd.h>
#endif
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include "scheme.h"
#include "rktio.h"

#ifdef WIN32
# define BOOT_EXTERN __declspec(dllexport)
#else
# define BOOT_EXTERN extern
#endif
#include "boot.h"

#define RACKET_AS_BOOT

#if defined(_MSC_VER) || defined(__MINGW32__)
# define BOOT_O_BINARY O_BINARY
#endif

#ifndef BOOT_O_BINARY
# define BOOT_O_BINARY 0
#endif

#if defined(OS_X) && !defined(RACKET_XONX)

# include <mach-o/dyld.h>
# define RACKET_USE_FRAMEWORK

const char *get_framework_path() {
  int i, c, len;
  const char *s;
  
  c = _dyld_image_count();
  for (i = 0; i < c; i++) {
    s = _dyld_get_image_name(i);
    len = strlen(s);
    if ((len > 7) && !strcmp("/Racket", s + len - 7)) {
      char *s2;
      s2 = strdup(s);
      strcpy(s2 + len - 6, "boot");
      return s2;
    }
  }

  return "???";
}

char *path_append(const char *p1, char *p2) {
  int l1, l2;
  char *s;
  l1 = strlen(p1);
  l2 = strlen(p2);
  s = malloc(l1 + l2 + 2);
  memcpy(s, p1, l1);
  s[l1] = '/';
  memcpy(s + l1 + 1, p2, l2);
  s[l1+l2+1] = 0;
  return s;
}

#endif

static ptr Sbytevector(char *s)
{
  iptr len = strlen(s);
  ptr bv;
  bv = Smake_bytevector(len, 0);
  memcpy(Sbytevector_data(bv), s, len);
  return bv;
}

static ptr parse_coldirs(char *s)
{
  iptr len = strlen(s);

  if (!len || !s[len+1]) {
    /* empty string or only one string */
    return Sbytevector(s);
  }

  /* multiple collects paths; put into a reversed list */
  {
    ptr rev = Snil;
    iptr delta = 0;

    while (s[delta]) {
      len = strlen(s + delta);
      rev = Scons(Sbytevector(s+delta), rev);
      delta += len + 1;
    }

    return rev;
  }
}

static void run_cross_server(char **argv)
{
  ptr c, a;
  const char *target_machine = argv[1];
  const char *cross_server_patch_file = argv[2];
  const char *cross_server_library_file = argv[3];

  c = Stop_level_value(Sstring_to_symbol("load")); /* original `load` */
  a = Sstring(cross_server_patch_file);
  (void)Scall1(c, a);

  c = Stop_level_value(Sstring_to_symbol("load")); /* this is the patched `load` */
  a = Sstring(cross_server_library_file);
  (void)Scall1(c, a);
  c = Stop_level_value(Sstring_to_symbol("serve-cross-compile"));

  a = Sstring(target_machine);
  (void)Scall1(c, a);
}

static void racket_exit(int v)
{
  exit(v);
}

static int racket_errno()
{
  return errno;
}

static void init_foreign()
{
# include "rktio.inc"
  Sforeign_symbol("racket_exit", (void *)racket_exit);
  Sforeign_symbol("racket_errno", (void *)racket_errno);
}

void racket_boot(int argc, char **argv, char *exec_file, char *run_file,
		 char *boot_exe, long segment_offset,
                 char *coldir, char *configdir, /* wchar_t * */void *dlldir,
                 int pos1, int pos2, int pos3,
                 int cs_compiled_subdir, int is_gui,
		 int wm_is_gracket_or_x11_arg_count,
                 char *gracket_guid_or_x11_args,
		 void *dll_open, void *dll_find_object)
/* exe argument already stripped from argv */
{
#if !defined(RACKET_USE_FRAMEWORK) || !defined(RACKET_AS_BOOT)
  int fd;
#endif
#ifdef RACKET_AS_BOOT
  int skip_racket_boot = 0;
#endif
#ifdef RACKET_USE_FRAMEWORK
  const char *fw_path;
#endif
  int cross_server = 0;

#ifdef WIN32
  if (dlldir)
    rktio_set_dll_path((wchar_t *)dlldir);
  if (dll_open)
    rktio_set_dll_procs(dll_open, dll_find_object);
#endif

  Sscheme_init(NULL);

  if ((argc == 4) && !strcmp(argv[0], "--cross-server")) {
    cross_server = 1;
#ifdef RACKET_AS_BOOT
    skip_racket_boot = 1;
#endif
  }

#ifdef RACKET_USE_FRAMEWORK
  fw_path = get_framework_path();
  Sregister_boot_file(path_append(fw_path, "petite.boot"));
  Sregister_boot_file(path_append(fw_path, "scheme.boot"));
# ifdef RACKET_AS_BOOT
  if (!skip_racket_boot)
    Sregister_boot_file(path_append(fw_path, "racket.boot"));
# endif
#else
  fd = open(boot_exe, O_RDONLY | BOOT_O_BINARY);

  {
    int fd1, fd2;

    fd1 = dup(fd);
    lseek(fd1, pos1, SEEK_SET);    
    Sregister_boot_file_fd("petite", fd1);
    
    fd2 = open(boot_exe, O_RDONLY | BOOT_O_BINARY);
    lseek(fd2, pos2, SEEK_SET);
    Sregister_boot_file_fd("scheme", fd2);

# ifdef RACKET_AS_BOOT
    if (!skip_racket_boot) {
      fd = open(boot_exe, O_RDONLY | BOOT_O_BINARY);
      lseek(fd, pos3, SEEK_SET);
      Sregister_boot_file_fd("racket", fd);
    }
# endif
  }
#endif

  Sbuild_heap(NULL, init_foreign);

  if (cross_server) {
    /* Don't run Racket as usual. Instead, load the patch
       file and run `serve-cross-compile` */
    run_cross_server(argv);
    racket_exit(0);
  }

  {
    ptr l = Snil;
    int i;
    char segment_offset_s[32], wm_is_gracket_s[32];

    for (i = argc; i--; ) {
      l = Scons(Sbytevector(argv[i]), l);
    }
    l = Scons(Sbytevector(gracket_guid_or_x11_args), l);
    sprintf(wm_is_gracket_s, "%d", wm_is_gracket_or_x11_arg_count);
    l = Scons(Sbytevector(wm_is_gracket_s), l);
    l = Scons(Sbytevector(is_gui ? "true" : "false"), l);
    l = Scons(Sbytevector(cs_compiled_subdir ? "true" : "false"), l);
    sprintf(segment_offset_s, "%ld", segment_offset);
    l = Scons(Sbytevector(segment_offset_s), l);
    l = Scons(Sbytevector(configdir), l);
    l = Scons(parse_coldirs(coldir), l);
    l = Scons(Sbytevector(run_file), l);
    l = Scons(Sbytevector(exec_file), l);

#ifdef RACKET_AS_BOOT
    {
      ptr c, start, apply;
      c = Stop_level_value(Sstring_to_symbol("scheme-start"));
      start = Scall0(c);
      apply = Stop_level_value(Sstring_to_symbol("apply"));
      Scall2(apply, start, l);
    }
#else
    Sset_top_level_value(Sstring_to_symbol("bytes-command-line-arguments"), l);
#endif
  }

#ifndef RACKET_AS_BOOT
# ifdef RACKET_USE_FRAMEWORK
  fd = open(path_append(fw_path, "racket.so"), O_RDONLY);
  pos3 = 0;
# endif
  
  {
    ptr c, p;

    if (pos3) lseek(fd, pos3, SEEK_SET);
    c = Stop_level_value(Sstring_to_symbol("open-fd-input-port"));
    p = Scall1(c, Sfixnum(fd));
    Slock_object(p);
    c = Stop_level_value(Sstring_to_symbol("port-file-compressed!"));
    Scall1(c, p);
    Sunlock_object(p);
    c = Stop_level_value(Sstring_to_symbol("load-compiled-from-port"));
    Scall1(c, p);
  }
#endif
}
