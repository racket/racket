/* Used by "ustart.c" and "config.inc".

   Defines

     self_exe_t get_process_executable_path(const char *argv0);

   usually via "../ChezScheme/c/self-exe.c". See that file for more
   information.

   On Mac (even XonX), `find_mach_o_segment` is also defined,
   unless `SELF_EXE_NO_EXTRAS` is defined.

   On Unix (not Mac OS, not even XonX), `find_elf_section`
   is also defined. 

*/

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

#if defined(DOS_FILE_SYSTEM) && !defined(SELF_EXE_WINDOWS_AS_UTF8)
typedef wchar_t *self_exe_t;
static self_exe_t get_self_path(const char *exec_file)
{
  wchar_t *path;
  DWORD r, sz = 1024;

  while (1) {
    path = (wchar_t *)malloc(sz * sizeof(wchar_t));
    r = GetModuleFileNameW(NULL, path, sz);
    if ((r == sz)
        && (GetLastError() == ERROR_INSUFFICIENT_BUFFER)) {
      free(path);
      sz = 2 * sz;
    } else
      break;
  }

  return path;
}
#else
/* declare as static */
typedef char *self_exe_t;
static char *S_get_process_executable_path(const char *exec_file);
# include "../ChezScheme/c/self-exe.c"
static self_exe_t get_self_path(const char *exec_file) {
  self_exe_t p;
  p = S_get_process_executable_path(exec_file);
  if (p == NULL)
    return strdup(exec_file);
  else
    return p;
}
#endif

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

#if defined(__APPLE__) && defined(__MACH__)
# include <mach-o/getsect.h>
# include <mach-o/dyld.h>
# ifndef SELF_EXE_NO_EXTRAS
static long find_mach_o_segment(const char *name, long *_len)
{
#  if defined(__x86_64__) || defined(__arm64__)
  const struct segment_command_64 *seg;
#  else
  const struct segment_command *seg;
#  endif
  seg = getsegbyname(name);
  if (seg) {
    if (_len)
      *_len = seg->filesize;
    return seg->fileoff;
  } else
    return 0;
}
# endif
#endif

#if !defined(OS_X) && !defined(DOS_FILE_SYSTEM) && !defined(SELF_EXE_NO_EXTRAS)

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

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

static int find_elf_section_offset(const char *filename,
                                   const char *name,
                                   int *_start, int *_end) XFORM_SKIP_PROC
{
  int fd, i;
  ELF__Header e;
  Elf__Shdr s;
  char *strs;

  fd = open(filename, O_RDONLY, 0);
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
        free(strs);
	return 0;
      }

      for (i = 0; i < e.e_shnum; i++) {
	lseek(fd, e.e_shoff + (i * e.e_shentsize), SEEK_SET);
	if (read(fd, &s, sizeof(s)) != sizeof(s)) {
	  close(fd);
	  return 0;
	}
	if (!strcmp(strs + s.sh_name, name)) {
	  *_start = s.sh_offset;
          *_end = s.sh_offset + s.sh_size;
	  close(fd);
          free(strs);
	  return 1;
	}
      }

      free(strs);
    }
  }

  close(fd);
  return 0;
}

#endif
