#ifndef _MSC_VER
# include <unistd.h>
#endif
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include "scheme.h"
#include "rktio.h"
#include "boot.h"

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
    if ((len > 9) && !strcmp("CS/Racket", s + len - 9)) {
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

static void racket_exit(int v)
{
  exit(v);
}

void racket_boot(int argc, char **argv, char *self, long segment_offset,
                 char *coldir, char *configdir,
                 int pos1, int pos2, int pos3,
                 int is_gui)
/* exe argument already stripped from argv */
{
  int fd; 
#ifdef RACKET_USE_FRAMEWORK
  const char *fw_path;
#endif
 
  Sscheme_init(NULL);

#ifdef RACKET_USE_FRAMEWORK
  fw_path = get_framework_path();
  Sregister_boot_file(path_append(fw_path, "petite.boot"));
  Sregister_boot_file(path_append(fw_path, "scheme.boot"));
#else
  fd = open(self, O_RDONLY | O_BINARY);

  {
    int fd1, fd2;

    fd1 = dup(fd);
    lseek(fd1, pos1, SEEK_SET);    
    Sregister_boot_file_fd("petite", fd1);
    
    fd2 = open(self, O_RDONLY | O_BINARY);
    lseek(fd2, pos2, SEEK_SET);
    Sregister_boot_file_fd("scheme", fd2);
  }
#endif
  
  Sbuild_heap(NULL, NULL);

# include "rktio.inc"
  Sforeign_symbol("racket_exit", (void *)racket_exit);
  
  {
    ptr l = Snil;
    int i;
    char segment_offset_s[32];

    for (i = argc; i--; ) {
      l = Scons(Sbytevector(argv[i]), l);
    }
    l = Scons(Sbytevector(is_gui ? "true" : "false"), l);
    sprintf(segment_offset_s, "%ld", segment_offset);
    l = Scons(Sbytevector(segment_offset_s), l);
    l = Scons(Sbytevector(configdir), l);
    l = Scons(Sbytevector(coldir), l);
    l = Scons(Sbytevector(self), l);
    Sset_top_level_value(Sstring_to_symbol("bytes-command-line-arguments"), l);
  }

#ifdef RACKET_USE_FRAMEWORK
  fd = open(path_append(fw_path, "racket.so"), O_RDONLY);
  pos3 = 0;
#endif
  
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
}
