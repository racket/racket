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
#include "path_replace.inc"

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

#ifdef WIN32
PRESERVE_IN_EXECUTABLE
char *racket_dll_name = "libracketcsxxxxxxx.dll";
#endif

#ifdef OS_X
# include <mach-o/dyld.h>
static long find_rktboot_section(char *me)
{
  return find_mach_o_segment("__RKTBOOT", NULL);
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

static char *path_append(const char *p1, const char *p2) {
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

#if !defined(WIN32) && !defined(OS_X)
static long find_boot_section(const char *me)
{
  int start = 0, end = 0;
  
  find_elf_section_offset(me, ".rackboot", &start, &end);

  return start;
}
#endif

#ifdef WIN32
# include "../../start/cmdl_to_argv.inc"
#endif

#ifndef WIN32
static void *extract_dlldir()
{
  return NULL;
}
#endif

#ifndef do_pre_filter_cmdline_arguments
# define do_pre_filter_cmdline_arguments(argc, argv) /* empty */
#endif

static int bytes_main(int argc, char **argv,
		      /* for Windows and X11 GUI modes */
		      int wm_is_gracket_or_x11_arg_count, char *gracket_guid_or_x11_args)
{
  char *boot_exe;
  self_exe_t self_exe;
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

  self_exe = get_self_path(exec_file);

  extract_built_in_arguments(self_exe, &exec_file, &run_file, &argc, &argv);
  if (!run_file)
    run_file = exec_file;

  memcpy(&boot1_offset, boot_file_data + boot_file_offset, sizeof(boot1_offset));
  memcpy(&boot2_offset, boot_file_data + boot_file_offset + 4, sizeof(boot2_offset));
  memcpy(&boot3_offset, boot_file_data + boot_file_offset + 8, sizeof(boot3_offset));
  memcpy(&boot_end_offset, boot_file_data + boot_file_offset + 12, sizeof(boot_end_offset));

#ifdef WIN32
  parse_embedded_dlls();
  register_embedded_dll_hooks();
  if (embedded_dll_open) {
    void *dll;
    dll = embedded_dll_open(racket_dll_name, 1);
    boot_rsrc_offset = in_memory_get_offset(racket_dll_name);
    racket_boot_p = (racket_boot_t)scheme_dll_find_object(dll, "racket_boot");
    dll_path = self_exe;
  } else {
    HMODULE dll = NULL;
    dll_path = load_delayed_dll_x(NULL, racket_dll_name, &dll);
    racket_boot_p = (racket_boot_t)GetProcAddress(dll, "racket_boot");
  }
  boot_exe = string_to_utf8(dll_path);
# define racket_boot racket_boot_p
#else
  boot_exe = self_exe;
#endif

  /* segment_offset is for embedded bytecode (not boot files) */
  segment_offset = get_segment_offset(self_exe);

#if defined(OS_X)
  boot_offset = find_rktboot_section(boot_exe);
  if (!boot_offset) boot_images_in_exe = 0;
#elif WIN32
  boot_offset = find_resource_offset(dll_path, 259, boot_rsrc_offset);
#else
  boot_offset = find_boot_section(boot_exe);
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
    /* No offsets have been set, so we must be trying< to run
       something like `raw_racketcs` during the build process.
       Look for boot files adjacent to the executable. */
    boot1_path = path_replace_filename(boot_exe, "petite-v.boot");
    boot2_path = path_replace_filename(boot_exe, "scheme-v.boot");
    boot3_path = path_replace_filename(boot_exe, "racket-v.boot");
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
    ba.k_file = SELF_PATH_TO_BYTES(self_exe);

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
