#include "cs_config.h"

#ifndef WIN32
# include <unistd.h>
#endif
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef WIN32
# include <windows.h>
# define DOS_FILE_SYSTEM
static int scheme_utf8_encode(unsigned int *path, int zero_offset, int len,
			      char *dest, int dest_len, int get_utf16);
#endif
#define BOOT_EXTERN extern
#include "boot.h"

#define MZ_CHEZ_SCHEME
#ifndef INITIAL_BIN_TYPE
# define INITIAL_BIN_TYPE "zi"
#endif
#ifndef RACKET_IS_GUI
# define RACKET_IS_GUI 0
#endif

#ifndef CS_COMPILED_SUBDIR
# define CS_COMPILED_SUBDIR 0
#endif

#define XFORM_SKIP_PROC /* empty */

#include "../../start/config.inc"

#ifdef WIN32
typedef void *(*scheme_dll_open_proc)(const char *name, int as_global);
typedef void *(*scheme_dll_find_object_proc)(void *h, const char *name);
typedef void (*scheme_dll_close_proc)(void *h);
static scheme_dll_open_proc embedded_dll_open;
static scheme_dll_find_object_proc scheme_dll_find_object;
static scheme_dll_close_proc embedded_dll_close;
static void scheme_set_dll_procs(scheme_dll_open_proc open,
                                 scheme_dll_find_object_proc find,
                                 scheme_dll_close_proc close)
{
  embedded_dll_open = open;
  scheme_dll_find_object = find;
  embedded_dll_close = close;
}
# include "../../start/embedded_dll.inc"
#else
# define embedded_dll_open NULL
# define scheme_dll_find_object NULL
# define embedded_dll_close NULL
#endif

PRESERVE_IN_EXECUTABLE
char *boot_file_data = "BooT FilE OffsetS:\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
static int boot_file_offset = 18;

#define USE_GENERIC_GET_SELF_PATH

#ifdef OS_X
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

static long find_rktboot_section(char *me)
{
  const struct mach_header *mh;
  const struct load_command *lc;
  int i;

  mh = _dyld_get_image_header(0);

  lc = (void *)((char *)mh + ((mh->magic == 0xfeedfacf) ? sizeof(struct mach_header_64) : sizeof(struct mach_header)));

  for (i = 0; i < mh->ncmds; i++) {
    if (lc->cmd == LC_SEGMENT) {
      const struct segment_command *sc = (struct segment_command *)lc;
      if (!strcmp(sc->segname, "__RKTBOOT"))
        return sc->fileoff;
    } else if (lc->cmd == LC_SEGMENT_64) {
      const struct segment_command_64 *sc = (struct segment_command_64 *)lc;
      if (!strcmp(sc->segname, "__RKTBOOT"))
        return sc->fileoff;
    }
    lc = (void *)((char *)lc + lc->cmdsize);
  }

  return 0;
}
#endif

#if defined(OS_X) && !defined(RACKET_XONX)

# include <mach-o/dyld.h>
# define RACKET_USE_FRAMEWORK 1

static const char *get_framework_path() {
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

  return NULL;
}

static char *path_append(const char *p1, char *p2) {
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

#if defined(__FreeBSD__)
# include <sys/sysctl.h>
# include <errno.h>
static char *get_self_path(char *exec_file)
{
  int mib[4];
  char *s;
  size_t len;
  int r;

  mib[0] = CTL_KERN;
  mib[1] = KERN_PROC;
  mib[2] = KERN_PROC_PATHNAME;
  mib[3] = -1;

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

#ifdef ELF_FIND_BOOT_SECTION
# include <elf.h>
# include <fcntl.h>

static long find_boot_section(char *me)
{
  int fd, i;
#if SIZEOF_VOID_P == 4
  Elf32_Ehdr e;
  Elf32_Shdr s;
#else
  Elf64_Ehdr e;
  Elf64_Shdr s;
#endif
  char *strs;

  fd = open(me, O_RDONLY, 0);
  if (fd == -1) return 0;

  if (read(fd, &e, sizeof(e)) == sizeof(e)) {
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
      if (!strcmp(strs + s.sh_name, ".rackboot")) {
        close(fd);
        return s.sh_offset;
      }
    }
  }

  close(fd);
  return 0;
}
#endif

#ifdef WIN32
static char *string_to_utf8(wchar_t *p)
{
  char *r;
  int len;

  len = WideCharToMultiByte(CP_UTF8, 0, p, -1, NULL, 0, NULL, NULL);
  r = malloc(len);
  len = WideCharToMultiByte(CP_UTF8, 0, p, -1, r, len, NULL, NULL);

  return r;
}

static int scheme_utf8_encode(unsigned int *path, int zero_offset, int len,
			      char *dest, int offset, int get_utf16)
{
  int dest_len = 0;
  if (dest) {
    dest_len = WideCharToMultiByte(CP_UTF8, 0, (wchar_t *)path, len, NULL, 0, NULL, NULL);
  }
  return WideCharToMultiByte(CP_UTF8, 0, (wchar_t *)path, len, dest, dest_len, NULL, NULL);
}

# include "../start/cmdl_to_argv.inc"

# undef USE_GENERIC_GET_SELF_PATH
#endif

#ifdef USE_GENERIC_GET_SELF_PATH
/* Get executable path via argv[0] and the `PATH` encironment variable */

static int has_slash(char *s)
{
  while (*s) {
    if (s[0] == '/')
      return 1;
    s++;
  }
  return 0;
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

static int executable_exists(char *path)
{
  return (access(path, X_OK) == 0);
}

static char *get_self_path(char *exec_file)
{
  if (exec_file[0] == '/') {
    /* Absolute path */
    return exec_file;
  } else if (has_slash(exec_file)) {
    /* Relative path with a directory: */
    char *buf;
    long buflen = 4096;
    buf = (char *)malloc(buflen);
    return path_append(getcwd(buf, buflen), exec_file);
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

      m = path_append(path, exec_file);

      if (executable_exists(m)) {
	if (m[0] != '/')
	  m = path_append(getcwd(NULL, 0), m);
	return m;
      }
      free(m);

      if (more)
	path = p + 1;
      else
	break;
    }

    return exec_file;
  }
}
#endif

#ifdef NO_GET_SEGMENT_OFFSET
static long get_segment_offset()
{
  return 0;
}
#endif

#ifndef WIN32
static void *extract_dlldir()
{
  return NULL;
}
#endif

static char *path_replace(const char *s, const char *new_file)
{
  int len1 = strlen(s), len2 = strlen(new_file);
  char *r;

  while ((len1 > 0) && (s[len1-1] != '/') && (s[len1-1] != '\\'))
    len1--;

  r = malloc(len1+len2+1);
  memcpy(r, (void *)s, len1);
  memcpy(r+len1, (void *)new_file, len2+1);

  return r;
}

#ifndef do_pre_filter_cmdline_arguments
# define do_pre_filter_cmdline_arguments(argc, argv) /* empty */
#endif

static int bytes_main(int argc, char **argv,
		      /* for Windows and X11 GUI modes */
		      int wm_is_gracket_or_x11_arg_count, char *gracket_guid_or_x11_args)
{
  char *boot_exe;
  char *exec_file = argv[0], *run_file = NULL;
  char *boot1_path, *boot2_path, *boot3_path;
  int boot1_offset, boot2_offset, boot3_offset, boot_end_offset;
#ifdef OS_X
  int boot_images_in_exe = 1;
#endif
  long boot_offset;
  long segment_offset;
#ifdef WIN32
  wchar_t *dll_path;
  racket_boot_t racket_boot_p;
  long boot_rsrc_offset = 0;
#endif

  if (argc) {
    argc--;
    argv++;
  }

  extract_built_in_arguments(&exec_file, &run_file, &argc, &argv);
  if (!run_file)
    run_file = exec_file;

  segment_offset = get_segment_offset();

  memcpy(&boot1_offset, boot_file_data + boot_file_offset, sizeof(boot1_offset));
  memcpy(&boot2_offset, boot_file_data + boot_file_offset + 4, sizeof(boot2_offset));
  memcpy(&boot3_offset, boot_file_data + boot_file_offset + 8, sizeof(boot3_offset));
  memcpy(&boot_end_offset, boot_file_data + boot_file_offset + 12, sizeof(boot_end_offset));

#ifdef WIN32
  parse_embedded_dlls();
  register_embedded_dll_hooks();
  if (embedded_dll_open) {
    void *dll;
    dll = embedded_dll_open("libracketcsxxxxxxx.dll", 1);
    boot_rsrc_offset = in_memory_get_offset("libracketcsxxxxxxx.dll");
    racket_boot_p = (racket_boot_t)scheme_dll_find_object(dll, "racket_boot");
    dll_path = get_self_executable_path();
  } else {
    HMODULE dll;
    dll_path = load_delayed_dll_x(NULL, "libracketcsxxxxxxx.dll", &dll);
    racket_boot_p = (racket_boot_t)GetProcAddress(dll, "racket_boot");
  }
  boot_exe = string_to_utf8(dll_path);
# define racket_boot racket_boot_p
#else
  boot_exe = get_self_path(exec_file);
#endif

#ifdef ELF_FIND_BOOT_SECTION
  boot_offset = find_boot_section(boot_exe);
#elif defined(OS_X)
  boot_offset = find_rktboot_section(boot_exe);
  if (!boot_offset) boot_images_in_exe = 0;
#elif WIN32
  boot_offset = find_resource_offset(dll_path, 259, boot_rsrc_offset);
#else
  boot_offset = 0;
#endif

  boot1_offset += boot_offset;
  boot2_offset += boot_offset;
  boot3_offset += boot_offset;
  boot_end_offset += boot_offset;

  boot1_path = boot2_path = boot3_path = boot_exe;

#if defined(OS_X) && !defined(RACKET_XONX)
  if (!boot_images_in_exe) {
    const char *fw_path = get_framework_path();
    if (fw_path) {
      boot1_path = path_append(fw_path, "petite.boot");
      boot2_path = path_append(fw_path, "scheme.boot");
      boot3_path = path_append(fw_path, "racket.boot");
      boot1_offset = boot2_offset = boot3_offset = boot_end_offset = 0;
    }
  }
#endif

  if ((boot1_offset == 0)
      && (boot2_offset == 0)
      && (boot3_offset == 0)
      && (boot1_path == boot2_path)
      && (boot1_path == boot3_path)) {
    /* No offsets have been set, so we must be trying to run
       something like `raw_racketcs` during the build process.
       Look for boot files adjacent to the executable. */
    boot1_path = path_replace(boot_exe, "petite-v.boot");
    boot2_path = path_replace(boot_exe, "scheme-v.boot");
    boot3_path = path_replace(boot_exe, "racket-v.boot");
  }

  {
    racket_boot_arguments_t ba;

    memset(&ba, 0, sizeof(ba));
    
    ba.boot1_path = boot1_path;
    ba.boot1_offset = boot1_offset;
    ba.boot1_len = boot2_offset - boot1_offset;
    ba.boot2_path = boot2_path;
    ba.boot2_offset = boot2_offset;
    ba.boot2_len = boot3_offset - boot2_offset;
    ba.boot3_path = boot3_path;
    ba.boot3_offset = boot3_offset;
    ba.boot3_len = boot_end_offset - boot3_offset;

    ba.argc = argc;
    ba.argv = argv;
    ba.exec_file = exec_file;
    ba.run_file = run_file;
    ba.collects_dir = extract_coldir();
    ba.config_dir = extract_configdir();
    ba.dll_dir = extract_dlldir();

    ba.cs_compiled_subdir = CS_COMPILED_SUBDIR;

    ba.segment_offset = segment_offset;

    ba.dll_open = embedded_dll_open;
    ba.dll_find_object = scheme_dll_find_object;
    ba.dll_close = embedded_dll_close;

    ba.exit_after = 1;

    ba.is_gui = RACKET_IS_GUI;
    ba.wm_is_gracket_or_x11_arg_count = wm_is_gracket_or_x11_arg_count;
    ba.gracket_guid_or_x11_args = gracket_guid_or_x11_args;

    racket_boot(&ba);
  }

  return 0;
}

#if defined(WIN32) && (defined(CHECK_SINGLE_INSTANCE) || defined(__MINGW32__))
int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR ignored, int nCmdShow)
{
  int argc;
  char **argv;
  char *normalized_path;
  int wm = 0;
  char *guid = "";

  argv = cmdline_to_argv(&argc, &normalized_path);

#ifdef CHECK_SINGLE_INSTANCE
  if (CheckSingleInstance(normalized_path, argv))
    return 0;
  wm = wm_is_gracket;
  guid = GRACKET_GUID;
#endif
  
  return bytes_main(argc, argv, wm, guid);
}
#elif defined(WIN32)
int wmain(int argc, wchar_t **wargv)
{
  int i;
  char **argv = malloc(argc * sizeof(char*));

  for (i = 0; i < argc; i++) {
    argv[i] = string_to_utf8(wargv[i]);
  }

  return bytes_main(argc, argv, 0, "");
}
#else
static int x11_arg_count = 0;
static char *x11_args = "0x0";

int main(int argc, char **argv) {
  do_pre_filter_cmdline_arguments(&argc, &argv);
  return bytes_main(argc, argv, x11_arg_count, x11_args);
}
#endif
