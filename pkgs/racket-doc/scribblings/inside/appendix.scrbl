#lang scribble/base
@(require "utils.rkt")

@title[#:style '(grouper toc) #:tag "appendix"]{Appendices}

@local-table-of-contents[]

@; ----------------------------------------

@section[#:tag "src-build"]{Building Racket from Source}

The normal Racket distribution includes @filepath{.rkt} sources for
collection-based libraries. After modifying library files, run
@exec{raco setup} (see @secref[#:doc '(lib
"scribblings/raco/raco.scrbl") "setup"]) to rebuild installed
libraries.

The normal Racket distribution does not include the C sources for
Racket's run-time system. To build Racket from scratch, download a
source distribution from @url{http://download.racket-lang.org}.
Detailed build instructions are in the @filepath{README.txt} file in
the top-level @filepath{src} directory. You can also get the latest
sources from the @tt{git} repository at
@url{https://github.com/racket/racket}, but beware that the repository
is one step away from a normal source distribution, and it provides
build modes that are more suitable for developing Racket itself; see
@filepath{build.md} in the @tt{git} repository for more information.

@; ----------------------------------------

@section[#:tag "ios-cross-compilation"]{Cross-compiling Racket Sources for iOS}

Everything in this section can be adapted to other cross-compilation
targets, but iOS is used to give concrete examples.

After cross-compiling Racket CS for iOS according to the documentation
in the source distribution's @filepath{src/README.txt} file, you can
use that build of Racket in conjunction with the host build it was
compiled by to cross-compile Racket modules for iOS by passing the
following set of flags to the host executable:

@verbatim[#:indent 2]{
racket \
  --compile-any \
  --compiled 'compiled_host:tarm64osx' \
  --cross \
  --cross-compiler tarm64osx /path/to/ios/racket/lib \
  --config /path/to/ios/racket/etc \
  --collects /path/to/ios/racket/collects
}

The above command runs the host Racket REPL with support for
outputting compiled code for both the host machine and for the
@tt{tarm64osx} target.  The second path to @exec{--compiled} may be
any relative path, but @filepath{tarm64osx} is what the cross build
uses to set up its installation so it is convenient to re-use it.

Furthermore, you can instruct the host Racket to run library code by
passing the @exec{-l} flag.  For example, you can setup the target
Racket's installation with the following command:

@verbatim[#:indent 2]{
racket \
  --compile-any \
  --compiled 'compiled_host:tarm64osx' \
  --cross \
  --cross-compiler tarm64osx /path/to/ios/racket/lib \
  --config /path/to/ios/racket/etc \
  --collects /path/to/ios/racket/collects \
  -l- \
  raco setup
}

Finally, you can package up a Racket module and its dependencies for
use with @cppi{racket_embedded_load_file} (after installing
@filepath{compiler-lib} and @filepath{cext-lib} for the target Racket)
with:

@verbatim[#:indent 2]{
racket \
  --compile-any \
  --compiled 'compiled_host:tarm64osx' \
  --cross \
  --cross-compiler tarm64osx /path/to/ios/racket/lib \
  --config /path/to/ios/racket/etc \
  --collects /path/to/ios/racket/collects \
  -l- \
  raco ctool --mods application.zo src/application.rkt
}

@; ----------------------------------------

@section[#:tag "segment-ideas"]{Embedding Files in Executable Sections}

Locating external files on startup, such as the boot files needed for
Racket CS, can be troublesome. An alternative to having separate files
is to embed the files in an ELF or Mach-O executable as data segments
or in a Windows executable as a resource. Embedding files in that way
requires using OS-specific linking steps and runtime libraries.

@; ============================================================

@subsection{Accessing ELF Sections on Linux}

On Linux and other ELF-based systems, you can add sections to an
executable using @exec{objcopy}. For example, the following command
copies @filepath{pre_run} to @cpp{run} while adding boot files as
sections:

@verbatim[#:indent 2]{
objcopy --add-section .csboot1=petite.boot \
        --set-section-flags .csboot1=noload,readonly \
        --add-section .csboot2=scheme.boot \
        --set-section-flags .csboot2=noload,readonly \
        --add-section .csboot3=racket.boot \
        --set-section-flags .csboot3=noload,readonly \
        ./pre_run ./run
}

Here's an implementation for @filepath{pre_run} like the one in
@secref["cs-embedding"], but where boot files are loaded from
sections:

@filebox["main.c"]{
@verbatim[#:indent 2]{
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <elf.h>
#include <fcntl.h>

#include "chezscheme.h"
#include "racketcs.h"

#include "run.c"

static char *get_self_path()
{
  ssize_t len, blen = 256;
  char *s = malloc(blen);

  while (1) {
    len = readlink("/proc/self/exe", s, blen-1);
    if (len == (blen-1)) {
      free(s);
      blen *= 2;
      s = malloc(blen);
    } else if (len < 0) {
      fprintf(stderr, "failed to get self (%d)\n", errno);
      exit(1);
    } else
      return s;
  }
}

static long find_section(const char *exe, const char *sectname)
{
  int fd, i;
  Elf64_Ehdr e;
  Elf64_Shdr s;
  char *strs;

  fd = open(exe, O_RDONLY, 0);
  if (fd != -1) {
    if (read(fd, &e, sizeof(e)) == sizeof(e)) {
      lseek(fd, e.e_shoff + (e.e_shstrndx * e.e_shentsize), SEEK_SET);
      if (read(fd, &s, sizeof(s)) == sizeof(s)) {
	strs = (char *)malloc(s.sh_size);
	lseek(fd, s.sh_offset, SEEK_SET);
	if (read(fd, strs, s.sh_size) == s.sh_size) {
	  for (i = 0; i < e.e_shnum; i++) {
	    lseek(fd, e.e_shoff + (i * e.e_shentsize), SEEK_SET);
	    if (read(fd, &s, sizeof(s)) != sizeof(s))
	      break;
	    if (!strcmp(strs + s.sh_name, sectname)) {
	      close(fd);
	      return s.sh_offset;
	    }
	  }
	}
      }
    }
    close(fd);
  }

  fprintf(stderr, "could not find section %s\n", sectname);
  return -1;
}

int main(int argc, char *argv[])
{
  racket_boot_arguments_t ba;

  memset(&ba, 0, sizeof(ba));

  ba.boot1_path = get_self_path();
  ba.boot2_path = ba.boot1_path;
  ba.boot3_path = ba.boot1_path;

  ba.boot1_offset = find_section(ba.boot1_path, ".csboot1");
  ba.boot2_offset = find_section(ba.boot2_path, ".csboot2");
  ba.boot3_offset = find_section(ba.boot3_path, ".csboot3");

  ba.exec_file = argv[0];

  racket_boot(&ba);

  declare_modules();

  ptr mod = Scons(Sstring_to_symbol("quote"),
                  Scons(Sstring_to_symbol("run"),
                        Snil));

  racket_dynamic_require(mod, Sfalse);

  return 0;
}
}}

@; ============================================================

@subsection{Accessing Mac OS Sections}

On Mac OS, sections can be added to a Mach-O executable using the
@Flag{sectcreate} compiler flag. If @filepath{main.c} is compiled and
linked with

@verbatim[#:indent 2]{
  gcc main.c libracketcs.a -Ipath/to/racket/include \
      -liconv -lncurses -framework CoreFoundation \
      -sectcreate __DATA __rktboot1 petite.boot \
      -sectcreate __DATA __rktboot2 scheme.boot \
      -sectcreate __DATA __rktboot2 racket.boot
}

then the executable can access is own path using
@cpp{_NSGetExecutablePath}, and it can locate sections using
@cpp{getsectbyname}. Here's an example like the one in
@secref["cs-embedding"]:

@filebox["main.c"]{
@verbatim[#:indent 2]{
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "chezscheme.h"
#include "racketcs.h"

#include "run.c"

#include <mach-o/dyld.h>
#include <mach-o/getsect.h>

static char *get_self_path()
{
  char *s;
  uint32_t size = 0;
  int r;

  r = _NSGetExecutablePath(NULL, &size);
  s = malloc(size+1);
  r = _NSGetExecutablePath(s, &size);
  if (!r)
    return s;

  fprintf(stderr, "could not get executable path\n");
  exit(1);
}

static long find_section(char *segname, char *sectname)
{
  const struct section_64 *s = getsectbyname(segname, sectname);
  if (s)
    return s->offset;

  fprintf(stderr, "could not find segment %s section %s\n",
          segname, sectname);
  exit(1);
}

#endif

int main(int argc, char **argv)
{
  racket_boot_arguments_t ba;

  memset(&ba, 0, sizeof(ba));

  ba.boot1_path = get_self_path();
  ba.boot2_path = ba.boot1_path;
  ba.boot3_path = ba.boot1_path;

  ba.boot1_offset = find_section("__DATA", "__rktboot1");
  ba.boot2_offset = find_section("__DATA", "__rktboot2");
  ba.boot3_offset = find_section("__DATA", "__rktboot3");

  ba.exec_file = argv[0];
  ba.run_file = argv[0];

  racket_boot(&ba);

  declare_modules(); /* defined by "run.c" */

  ptr mod = Scons(Sstring_to_symbol("quote"),
                  Scons(Sstring_to_symbol("run"),
                        Snil));

  racket_dynamic_require(mod, Sfalse);

  return 0;
}
}}

@; ============================================================

@subsection{Accessing Windows Resources}

On Windows, data is most readily added to an executable as a resource.
The following code demonstrates how to find the path to the current
executable and how to find a resource in the executable by identifying
number, type (usually @cpp{1}) and encoding (usual @cpp{1033}):

@filebox["main.c"]{
@verbatim[#:indent 2]{
/* forward declaration for internal helper */
static DWORD find_by_id(HANDLE fd, DWORD rsrcs, DWORD pos, int id);

static wchar_t *get_self_executable_path()
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

static long find_resource_offset(wchar_t *path, int id, int type, int encoding)
{
  /* Find the resource of type `id` */
  HANDLE fd;

  fd = CreateFileW(path, GENERIC_READ,
                   FILE_SHARE_READ | FILE_SHARE_WRITE,
                   NULL,
                   OPEN_EXISTING,
                   0,
                   NULL);

  if (fd == INVALID_HANDLE_VALUE)
    return 0;
  else {
    DWORD val, got, sec_pos, virtual_addr, rsrcs, pos;
    WORD num_sections, head_size;
    char name[8];

    SetFilePointer(fd, 60, 0, FILE_BEGIN);
    ReadFile(fd, &val, 4, &got, NULL);
    SetFilePointer(fd, val+4+2, 0, FILE_BEGIN); /* Skip "PE\0\0" tag and machine */
    ReadFile(fd, &num_sections, 2, &got, NULL);
    SetFilePointer(fd, 12, 0, FILE_CURRENT); /* time stamp + symbol table */
    ReadFile(fd, &head_size, 2, &got, NULL);

    sec_pos = val+4+20+head_size;
    while (num_sections--) {
      SetFilePointer(fd, sec_pos, 0, FILE_BEGIN);
      ReadFile(fd, &name, 8, &got, NULL);
      if ((name[0] == '.')
          && (name[1] == 'r')
          && (name[2] == 's')
          && (name[3] == 'r')
          && (name[4] == 'c')
          && (name[5] == 0)) {
        SetFilePointer(fd, 4, 0, FILE_CURRENT); /* skip virtual size */
        ReadFile(fd, &virtual_addr, 4, &got, NULL);
        SetFilePointer(fd, 4, 0, FILE_CURRENT); /* skip file size */
        ReadFile(fd, &rsrcs, 4, &got, NULL);
        SetFilePointer(fd, rsrcs, 0, FILE_BEGIN);

        /* We're at the resource table; step through 3 layers */
        pos = find_by_id(fd, rsrcs, rsrcs, id);
	if (pos) {
	  pos = find_by_id(fd, rsrcs, pos, type);
	  if (pos) {
	    pos = find_by_id(fd, rsrcs, pos, encoding);

	    if (pos) {
	      /* pos is the reource data entry */
	      SetFilePointer(fd, pos, 0, FILE_BEGIN);
	      ReadFile(fd, &val, 4, &got, NULL);
	      pos = val - virtual_addr + rsrcs;

	      CloseHandle(fd);

	      return pos;
	    }
	  }
	}

	break;
      }
      sec_pos += 40;
    }

    /* something went wrong */
    CloseHandle(fd);
    return -1;
  }
}

/* internal helper function */
static DWORD find_by_id(HANDLE fd, DWORD rsrcs, DWORD pos, int id)
{
  DWORD got, val;
  WORD name_count, id_count;

  SetFilePointer(fd, pos + 12, 0, FILE_BEGIN);
  ReadFile(fd, &name_count, 2, &got, NULL);
  ReadFile(fd, &id_count, 2, &got, NULL);

  pos += 16 + (name_count * 8);
  while (id_count--) {
    ReadFile(fd, &val, 4, &got, NULL);
    if (val == id) {
      ReadFile(fd, &val, 4, &got, NULL);
      return rsrcs + (val & 0x7FFFFFF);
    } else {
      ReadFile(fd, &val, 4, &got, NULL);
    }
  }

  return 0;
}
}}
