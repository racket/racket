#define USE_GENERIC_GET_SELF_PATH

#if defined(__linux__)
# include <errno.h>
static char *get_self_path(char *exec_file)
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
# undef USE_GENERIC_GET_SELF_PATH
#endif

#if defined(__FreeBSD__) || defined(__NetBSD__)
# include <sys/sysctl.h>
# include <errno.h>
static char *get_self_path(char *exec_file)
{
  int mib[4];
  char *s;
  size_t len;
  int r;

  mib[0] = CTL_KERN;
#if defined(__NetBSD__)
  mib[1] = KERN_PROC_ARGS;
  mib[2] = getpid();
  mib[3] = KERN_PROC_PATHNAME;
#else
  mib[1] = KERN_PROC;
  mib[2] = KERN_PROC_PATHNAME;
  mib[3] = -1;
#endif

  r = sysctl(mib, 4, NULL, &len, NULL, 0);
  if (r < 0) {
      fprintf(stderr, "failed to get self (%d)\n", errno);
      exit(1);
  }
  s = malloc(len);
  r = sysctl(mib, 4, s, &len, NULL, 0);
  if (r < 0) {
      fprintf(stderr, "failed to get self (%d)\n", errno);
      exit(1);
  }
  return s;
}
# undef USE_GENERIC_GET_SELF_PATH
#endif

#if defined(__APPLE__) && defined(__MACH__)
# include <mach-o/dyld.h>
static char *get_self_path(char *exec_file)
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
# undef USE_GENERIC_GET_SELF_PATH
#endif
