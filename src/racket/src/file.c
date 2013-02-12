/*
  Racket
  Copyright (c) 2004-2013 PLT Design Inc.
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
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* Most of this source file is pathname manipulation functions.  At
   the bottom, a path is just a byte string, not necessarily
   normalized in any way, except that it doesn't contain a nul
   character. The advantage of this representation is that it keeps
   paths the way the user wrote them. The tremendous disadvantage of
   this representation is that all of the operations must manipulate
   strings.

   A major complication is the complex syntax of Windows paths. Again,
   since all the operations work on the string, the code in this file
   parses and re-parses constantly.

 */

#include "schpriv.h"
#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#ifndef NO_STAT_PROC
# ifdef NO_SYS_INCLUDE_SUBDIR
#  include <stat.h>
# else
#  include <sys/types.h>
#  include <sys/stat.h>
# endif
#endif
#ifdef EXPAND_FILENAME_TILDE
# include <pwd.h>
#endif
#ifndef NO_FILE_SYSTEM_UTILS
# include <ctype.h>
# ifndef NO_READDIR
#  include <dirent.h>
# endif
#endif
#ifdef DIR_INCLUDE
# include <dir.h>
#endif
#ifdef DIRECT_INCLUDE
# include <direct.h>
#endif
#ifdef IO_INCLUDE
# include <io.h>
#endif
#if defined(MACINTOSH_EVENTS)
# ifdef OS_X
#  include <Carbon/Carbon.h>
# else
#  include <Carbon.h>
# endif
#endif
#ifdef UNIX_FILE_SYSTEM
# include <fcntl.h>
# include <grp.h>
# include <pwd.h>
# include <utime.h>
#endif
#ifdef DOS_FILE_SYSTEM
# include <windows.h>
# include <shlobj.h>
# ifdef __BORLANDC__
#  include <utime.h>
# else
#  include <sys/utime.h>
# endif
#endif
#ifdef NO_ERRNO_GLOBAL
# define errno -1
#else
# include <errno.h>
#endif

#if defined(S_IFDIR) && !defined(S_ISDIR)
# define S_ISDIR(m) ((m) & S_IFDIR)
#endif
#if defined(S_IFREG) && !defined(S_ISREG)
# define S_ISREG(m) ((m) & S_IFREG)
#endif
#if defined(S_IFLNK) && !defined(S_ISLNK)
# define S_ISLNK(m) ((m) & S_IFLNK)
#endif
#if defined(_S_IFDIR) && !defined(S_ISDIR)
# define S_ISDIR(m) ((m) & _S_IFDIR)
#endif
#if defined(_S_IFREG) && !defined(S_ISREG)
# define S_ISREG(m) ((m) & _S_IFREG)
#endif

#if defined(CARBON_FILE_SYSTEM)
long scheme_creator_id = 'MzSc';
#endif

#define UNIX_FN_SEP '/'
#define IS_A_UNIX_SEP(x) ((x) == '/')
#define IS_A_UNIX_PRIM_SEP(x) IS_A_UNIX_SEP(x)

#define DOS_FN_SEP '\\'
#define IS_A_DOS_SEP(x) (((x) == '/') || ((x) == '\\'))
#define IS_A_DOS_PRIM_SEP(x) ((x) == '\\')
#define IS_A_DOS_X_SEP(prim, x) (prim ? IS_A_DOS_PRIM_SEP(x) : IS_A_DOS_SEP(x))

#define FN_SEP(kind) ((kind == SCHEME_UNIX_PATH_KIND) ? UNIX_FN_SEP : DOS_FN_SEP)
#define IS_A_SEP(kind, x) ((kind == SCHEME_UNIX_PATH_KIND) ? IS_A_UNIX_SEP(x) : IS_A_DOS_SEP(x))
#define IS_A_PRIM_SEP(kind, x) ((kind == SCHEME_UNIX_PATH_KIND) ? IS_A_UNIX_PRIM_SEP(x) : IS_A_DOS_PRIM_SEP(x))

SHARED_OK int scheme_ignore_user_paths;
void scheme_set_ignore_user_paths(int v) { scheme_ignore_user_paths = v; }

SHARED_OK int scheme_ignore_link_paths;
void scheme_set_ignore_link_paths(int v) { scheme_ignore_link_paths = v; }

#define CURRENT_WD() scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_DIRECTORY)

#define TO_PATH(x) (SCHEME_GENERAL_PATHP(x) ? x : scheme_char_string_to_path(x))

#ifdef DOS_FILE_SYSTEM
extern int scheme_stupid_windows_machine;
#endif

/* Define TILDE_IS_ABSOLUTE to get pre-v4.0 handling of "~". */

#ifdef TILDE_IS_ABSOLUTE
# define WHEN_TILDE_IS_ABSOLUTE(x) (x)
#else
# define WHEN_TILDE_IS_ABSOLUTE(x) 0
#endif

static int check_dos_slashslash_drive(const char *next, int delta, int len, 
				      int *drive_end, int exact, int no_fw);
static int check_dos_slashslash_qm(const char *next, int len, int *drive_end, 
				   int *clean_start, int *add_sep);

#define is_drive_letter(c) (((unsigned char)c < 128) && isalpha((unsigned char)c))

/* local */
static Scheme_Object *path_p(int argc, Scheme_Object **argv);
static Scheme_Object *general_path_p(int argc, Scheme_Object **argv);
static Scheme_Object *path_to_string(int argc, Scheme_Object **argv);
static Scheme_Object *path_to_bytes(int argc, Scheme_Object **argv);
static Scheme_Object *path_element_to_bytes(int argc, Scheme_Object **argv);
static Scheme_Object *path_element_to_string(int argc, Scheme_Object **argv);
static Scheme_Object *string_to_path(int argc, Scheme_Object **argv);
static Scheme_Object *bytes_to_path(int argc, Scheme_Object **argv);
static Scheme_Object *bytes_to_path_element(int argc, Scheme_Object **argv);
static Scheme_Object *string_to_path_element(int argc, Scheme_Object **argv);
static Scheme_Object *path_kind(int argc, Scheme_Object **argv);
static Scheme_Object *platform_path_kind(int argc, Scheme_Object **argv);

static Scheme_Object *file_exists(int argc, Scheme_Object **argv);
static Scheme_Object *directory_exists(int argc, Scheme_Object **argv);
static Scheme_Object *link_exists(int argc, Scheme_Object **argv);

#ifndef NO_FILE_SYSTEM_UTILS
static Scheme_Object *build_path_kind(int argc, Scheme_Object **argv);
static Scheme_Object *delete_file(int argc, Scheme_Object **argv);
static Scheme_Object *rename_file(int argc, Scheme_Object **argv);
static Scheme_Object *copy_file(int argc, Scheme_Object **argv);
static Scheme_Object *path_to_directory_path(int argc, Scheme_Object *argv[]);
static Scheme_Object *directory_list(int argc, Scheme_Object *argv[]);
static Scheme_Object *filesystem_root_list(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_directory(int argc, Scheme_Object *argv[]);
static Scheme_Object *delete_directory(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_link(int argc, Scheme_Object *argv[]);
static Scheme_Object *split_path(int argc, Scheme_Object **argv);
static Scheme_Object *relative_path_p(int argc, Scheme_Object **argv);
static Scheme_Object *absolute_path_p(int argc, Scheme_Object **argv);
static Scheme_Object *complete_path_p(int argc, Scheme_Object **argv);
static Scheme_Object *path_to_complete_path(int argc, Scheme_Object **argv);
static Scheme_Object *resolve_path(int argc, Scheme_Object *argv[]);
static Scheme_Object *cleanse_path(int argc, Scheme_Object *argv[]);
static Scheme_Object *expand_user_path(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_drive(int argc, Scheme_Object *argv[]);
static Scheme_Object *file_modify_seconds(int argc, Scheme_Object *argv[]);
static Scheme_Object *file_or_dir_permissions(int argc, Scheme_Object *argv[]);
static Scheme_Object *file_identity(int argc, Scheme_Object *argv[]);
static Scheme_Object *file_size(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_library_collection_paths(int argc, Scheme_Object *argv[]);
static Scheme_Object *use_compiled_kind(int, Scheme_Object *[]);
static Scheme_Object *compiled_file_roots(int, Scheme_Object *[]);
static Scheme_Object *use_user_paths(int, Scheme_Object *[]);
static Scheme_Object *use_link_paths(int, Scheme_Object *[]);
static Scheme_Object *find_system_path(int argc, Scheme_Object **argv);
#endif

#ifdef DIR_FUNCTION
static Scheme_Object *current_directory(int argc, Scheme_Object *argv[]);
#endif

static int has_null(const char *s, intptr_t l);
static void raise_null_error(const char *name, Scheme_Object *path, const char *mod);

static char *do_path_to_complete_path(char *filename, intptr_t ilen, const char *wrt, intptr_t wlen, int kind);
static Scheme_Object *do_simplify_path(Scheme_Object *path, Scheme_Object *cycle_check, int skip, int use_filesystem, int force_rel_up, int kind);
static char *do_normal_path_seps(char *si, int *_len, int delta, int strip_trail, int kind, int *_did);
static char *remove_redundant_slashes(char *filename, int *l, int delta, int *expanded, int kind);
static Scheme_Object *do_path_to_directory_path(char *s, intptr_t offset, intptr_t len, Scheme_Object *p, int just_check, int kind);

READ_ONLY static Scheme_Object *up_symbol;
READ_ONLY static Scheme_Object *relative_symbol;
READ_ONLY static Scheme_Object *same_symbol;
#ifndef NO_FILE_SYSTEM_UTILS
READ_ONLY static Scheme_Object *read_symbol, *write_symbol, *execute_symbol;

READ_ONLY static Scheme_Object *temp_dir_symbol, *home_dir_symbol, *pref_dir_symbol;
READ_ONLY static Scheme_Object *doc_dir_symbol, *desk_dir_symbol;
READ_ONLY static Scheme_Object *init_dir_symbol, *init_file_symbol, *sys_dir_symbol;
READ_ONLY static Scheme_Object *exec_file_symbol, *run_file_symbol, *collects_dir_symbol;
READ_ONLY static Scheme_Object *pref_file_symbol, *orig_dir_symbol, *addon_dir_symbol;
READ_ONLY static Scheme_Object *links_file_symbol;

SHARED_OK static Scheme_Object *exec_cmd;
SHARED_OK static Scheme_Object *run_cmd;
SHARED_OK static Scheme_Object *collects_path;
THREAD_LOCAL_DECL(static Scheme_Object *original_pwd);
SHARED_OK static Scheme_Object *addon_dir;
SHARED_OK static Scheme_Object *links_file;
THREAD_LOCAL_DECL(static Scheme_Object *inst_links_path);

#endif
READ_ONLY static Scheme_Object *windows_symbol, *unix_symbol;

#if defined(UNIX_FILE_SYSTEM) && !defined(NO_UNIX_USERS)

# define GROUP_CACHE_SIZE 10
THREAD_LOCAL_DECL(static Scheme_Object *group_member_cache);

SHARED_OK static int have_user_ids = 0;
SHARED_OK static uid_t uid;
SHARED_OK static uid_t euid;
SHARED_OK static gid_t gid;
SHARED_OK static gid_t egid;
#endif

void scheme_init_file(Scheme_Env *env)
{
  Scheme_Object *p;

  REGISTER_SO(up_symbol);
  REGISTER_SO(relative_symbol);
  REGISTER_SO(same_symbol);
#ifndef NO_FILE_SYSTEM_UTILS
  REGISTER_SO(read_symbol);
  REGISTER_SO(write_symbol);
  REGISTER_SO(execute_symbol);
  
  REGISTER_SO(temp_dir_symbol);
  REGISTER_SO(home_dir_symbol);
  REGISTER_SO(pref_dir_symbol);
  REGISTER_SO(doc_dir_symbol);
  REGISTER_SO(desk_dir_symbol);
  REGISTER_SO(init_dir_symbol);
  REGISTER_SO(init_file_symbol);
  REGISTER_SO(sys_dir_symbol);
  REGISTER_SO(pref_file_symbol);
  REGISTER_SO(exec_file_symbol);
  REGISTER_SO(run_file_symbol);
  REGISTER_SO(collects_dir_symbol);
  REGISTER_SO(orig_dir_symbol);
  REGISTER_SO(addon_dir_symbol);
  REGISTER_SO(links_file_symbol);
#endif
  REGISTER_SO(windows_symbol);
  REGISTER_SO(unix_symbol);

  up_symbol = scheme_intern_symbol("up");
  relative_symbol = scheme_intern_symbol("relative");
  same_symbol = scheme_intern_symbol("same");
  
#ifndef NO_FILE_SYSTEM_UTILS
  read_symbol = scheme_intern_symbol("read");
  write_symbol = scheme_intern_symbol("write");
  execute_symbol = scheme_intern_symbol("execute");
  
  temp_dir_symbol = scheme_intern_symbol("temp-dir");
  home_dir_symbol = scheme_intern_symbol("home-dir");
  doc_dir_symbol = scheme_intern_symbol("doc-dir");
  desk_dir_symbol = scheme_intern_symbol("desk-dir");
  pref_dir_symbol = scheme_intern_symbol("pref-dir");
  init_dir_symbol = scheme_intern_symbol("init-dir");
  init_file_symbol = scheme_intern_symbol("init-file");
  sys_dir_symbol = scheme_intern_symbol("sys-dir");
  pref_file_symbol = scheme_intern_symbol("pref-file");
  exec_file_symbol = scheme_intern_symbol("exec-file");
  run_file_symbol = scheme_intern_symbol("run-file");
  collects_dir_symbol = scheme_intern_symbol("collects-dir");
  orig_dir_symbol = scheme_intern_symbol("orig-dir");
  addon_dir_symbol = scheme_intern_symbol("addon-dir");
  links_file_symbol = scheme_intern_symbol("links-file");
#endif

  windows_symbol = scheme_intern_symbol("windows");
  unix_symbol = scheme_intern_symbol("unix");

  p = scheme_make_prim_w_arity(path_p, "path?", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_add_global_constant("path?", p, env);

  scheme_add_global_constant("path-for-some-system?", 
			     scheme_make_folding_prim(general_path_p, 
                                                      "path-for-some-system?", 
                                                      1, 1, 1), 
			     env);
  scheme_add_global_constant("path-convention-type", 
			     scheme_make_folding_prim(path_kind, 
                                                      "path-convention-type", 
                                                      1, 1, 1), 
			     env);
  scheme_add_global_constant("system-path-convention-type", 
			     scheme_make_prim_w_arity(platform_path_kind, 
                                                      "system-path-convention-type", 
                                                      0, 0),
			     env);
  scheme_add_global_constant("path->string", 
			     scheme_make_prim_w_arity(path_to_string, 
						      "path->string", 
						      1, 1), 
			     env);
  scheme_add_global_constant("path->bytes", 
			     scheme_make_prim_w_arity(path_to_bytes, 
						      "path->bytes", 
						      1, 1), 
			     env);
  scheme_add_global_constant("path-element->bytes", 
			     scheme_make_prim_w_arity(path_element_to_bytes, 
						      "path-element->bytes", 
						      1, 1), 
			     env);
  scheme_add_global_constant("path-element->string", 
			     scheme_make_prim_w_arity(path_element_to_string, 
						      "path-element->string", 
						      1, 1), 
			     env);
  scheme_add_global_constant("string->path", 
			     scheme_make_prim_w_arity(string_to_path, 
						      "string->path", 
						      1, 1), 
			     env);
  scheme_add_global_constant("bytes->path", 
			     scheme_make_prim_w_arity(bytes_to_path, 
						      "bytes->path", 
						      1, 2), 
			     env);
  scheme_add_global_constant("bytes->path-element", 
			     scheme_make_prim_w_arity(bytes_to_path_element, 
						      "bytes->path-element", 
						      1, 2), 
			     env);
  scheme_add_global_constant("string->path-element", 
			     scheme_make_prim_w_arity(string_to_path_element, 
						      "string->path-element", 
						      1, 1), 
			     env);

  scheme_add_global_constant("file-exists?", 
			     scheme_make_prim_w_arity(file_exists, 
						      "file-exists?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("directory-exists?", 
			     scheme_make_prim_w_arity(directory_exists, 
						      "directory-exists?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("link-exists?", 
			     scheme_make_prim_w_arity(link_exists, 
						      "link-exists?", 
						      1, 1), 
			     env);
#ifndef NO_FILE_SYSTEM_UTILS
  scheme_add_global_constant("delete-file", 
			     scheme_make_prim_w_arity(delete_file, 
						      "delete-file", 
						      1, 1), 
			     env);
  scheme_add_global_constant("rename-file-or-directory", 
			     scheme_make_prim_w_arity(rename_file, 
						      "rename-file-or-directory", 
						      2, 3), 
			     env);
  scheme_add_global_constant("copy-file", 
			     scheme_make_prim_w_arity(copy_file, 
						      "copy-file", 
						      2, 3), 
			     env);
  scheme_add_global_constant("build-path", 
			     scheme_make_prim_w_arity(scheme_build_path,
						      "build-path", 
						      1, -1), 
			     env);
  scheme_add_global_constant("build-path/convention-type", 
			     scheme_make_prim_w_arity(build_path_kind,
						      "build-path/convention-type", 
						      2, -1), 
			     env);
  scheme_add_global_constant("path->directory-path",
			     scheme_make_prim_w_arity(path_to_directory_path,
						      "path->directory-path",
						      1, 1), 
			     env);
  scheme_add_global_constant("split-path", 
			     scheme_make_prim_w_arity2(split_path,
						       "split-path",
						       1, 1,
						       3, 3), 
			     env);
  scheme_add_global_constant("relative-path?", 
			     scheme_make_prim_w_arity(relative_path_p,
						      "relative-path?",
						      1, 1), 
			     env);
  scheme_add_global_constant("absolute-path?", 
			     scheme_make_prim_w_arity(absolute_path_p,
						      "absolute-path?",
						      1, 1), 
			     env);
  scheme_add_global_constant("complete-path?", 
			     scheme_make_prim_w_arity(complete_path_p,
						      "complete-path?",
						      1, 1), 
			     env);
  scheme_add_global_constant("path->complete-path",
			     scheme_make_prim_w_arity(path_to_complete_path,
						      "path->complete-path",
						      1, 2), 
			     env);
  scheme_add_global_constant("resolve-path",
			     scheme_make_prim_w_arity(resolve_path,
						      "resolve-path",
						      1, 1), 
			     env);
  scheme_add_global_constant("simplify-path",
			     scheme_make_prim_w_arity(scheme_simplify_path,
						      "simplify-path",
						      1, 2), 
			     env);
  scheme_add_global_constant("cleanse-path",
			     scheme_make_prim_w_arity(cleanse_path,
						      "cleanse-path",
						      1, 1), 
			     env);
  scheme_add_global_constant("expand-user-path",
			     scheme_make_prim_w_arity(expand_user_path,
						      "expand-user-path",
						      1, 1), 
			     env);
  scheme_add_global_constant("directory-list",
			     scheme_make_prim_w_arity(directory_list,
						      "directory-list",
						      0, 1), 
			     env);
  scheme_add_global_constant("filesystem-root-list",
			     scheme_make_prim_w_arity(filesystem_root_list,
						      "filesystem-root-list",
						      0, 0), 
			     env);
  scheme_add_global_constant("make-directory",
			     scheme_make_prim_w_arity(make_directory,
						      "make-directory",
						      1, 1), 
			     env);
  scheme_add_global_constant("delete-directory",
			     scheme_make_prim_w_arity(delete_directory,
						      "delete-directory",
						      1, 1), 
			     env);
  scheme_add_global_constant("make-file-or-directory-link",
			     scheme_make_prim_w_arity(make_link,
						      "make-file-or-directory-link",
						      2, 2), 
			     env);
  scheme_add_global_constant("file-or-directory-modify-seconds",
			     scheme_make_prim_w_arity(file_modify_seconds,
						      "file-or-directory-modify-seconds",
						      1, 3), 
			     env);
  scheme_add_global_constant("file-or-directory-permissions",
			     scheme_make_prim_w_arity(file_or_dir_permissions,
						      "file-or-directory-permissions",
						      1, 2), 
			     env);
  scheme_add_global_constant("file-or-directory-identity",
			     scheme_make_prim_w_arity(file_identity,
						      "file-or-directory-identity",
						      1, 2), 
			     env);
  scheme_add_global_constant("file-size",
			     scheme_make_prim_w_arity(file_size,
						      "file-size",
						      1, 1), 
			     env);

  scheme_add_global_constant("current-drive", 
			     scheme_make_prim_w_arity(current_drive, 
						      "current-drive", 
						      0, 0), 
			     env);

  scheme_add_global_constant("find-system-path", 
			     scheme_make_prim_w_arity(find_system_path, 
						      "find-system-path", 
						      1, 1), 
			     env);

#endif

#ifdef DIR_FUNCTION
  scheme_add_global_constant("current-directory",
			     scheme_register_parameter(current_directory,
						       "current-directory", 
						       MZCONFIG_CURRENT_DIRECTORY),
			     env);
#endif

#ifndef NO_FILE_SYSTEM_UTILS
  scheme_add_global_constant("current-library-collection-paths",
			     scheme_register_parameter(current_library_collection_paths,
						       "current-library-collection-paths",
						       MZCONFIG_COLLECTION_PATHS),
			     env);
#endif
  scheme_add_global_constant("use-compiled-file-paths",
			     scheme_register_parameter(use_compiled_kind,
						       "use-compiled-file-paths",
						       MZCONFIG_USE_COMPILED_KIND),
			     env);
  scheme_add_global_constant("current-compiled-file-roots",
			     scheme_register_parameter(compiled_file_roots,
						       "current-compiled-file-roots",
						       MZCONFIG_USE_COMPILED_ROOTS),
			     env);
  scheme_add_global_constant("use-user-specific-search-paths",
			     scheme_register_parameter(use_user_paths,
						       "use-user-specific-search-paths",
						       MZCONFIG_USE_USER_PATHS),
			     env);
  scheme_add_global_constant("use-collection-link-paths",
			     scheme_register_parameter(use_link_paths,
						       "use-collection-link-paths",
						       MZCONFIG_USE_LINK_PATHS),
			     env);
}

void scheme_init_file_places()
{
#ifndef NO_FILE_SYSTEM_UTILS
  REGISTER_SO(original_pwd);
#endif
}

/**********************************************************************/
/*                             paths                                  */
/**********************************************************************/

Scheme_Object *scheme_make_sized_offset_kind_path(char *chars, intptr_t d, intptr_t len, int copy, int kind)
{
  Scheme_Object *s;
  s = scheme_make_sized_offset_byte_string(chars, d, len, copy);
  s->type = kind;
  return s;
}

Scheme_Object *scheme_make_sized_offset_path(char *chars, intptr_t d, intptr_t len, int copy)
{
  return scheme_make_sized_offset_kind_path(chars, d, len, copy, SCHEME_PLATFORM_PATH_KIND);
}

# define IS_SPEC_CHAR(x) (IS_A_DOS_SEP(x) || ((x) == '"') || ((x) == '|') || ((x) == ':') || ((x) == '<') || ((x) == '>'))
static int is_special_filename(const char *_f, int offset, int len, int not_nul, int immediate);

static Scheme_Object *make_protected_sized_offset_path(int protect, char *chars, 
						       intptr_t d, intptr_t len, int copy,
						       int just_check, int kind)
     /* just_check == 2 => just check, and only for the case
	that it's the last element of a path */
{
  if (kind == SCHEME_WINDOWS_PATH_KIND) {
    if (protect) {
      int i;

      protect = 0;

      if (!protect) {
        int at_end = 1;
        for (i = len; i--; ) {
          if ((just_check != 2)
              && ((chars[i + d] == '.')
                  || (chars[i + d] == ' '))) {
            if (at_end) {
              protect = 1;
              break;
            }
          } else {
            at_end = 0;
            if ((chars[i + d] == '/')
                || (IS_SPEC_CHAR(chars[i + d]))) {
              protect = 1;
              break;
            }
          }
        }
      }

      if (!protect && (len == 1) && (chars[d] == '.'))
        protect = 1;

      if (!protect && (len == 2) && (chars[d] == '.') && (chars[d+1] == '.'))
        protect = 1;

      if (!protect) 
        protect = is_special_filename(chars, d, len, 0, 1);

      if (protect) {
        char *s2;
        if (just_check)
          return scheme_true;
        s2 = (char *)scheme_malloc_atomic(len + 9 + 1);
        memcpy(s2, "\\\\?\\REL\\\\", 9);
        memcpy(s2 + 9, chars + d, len);
        s2[9 + len] = 0;
        return scheme_make_sized_offset_kind_path(s2, 0, len + 9, 0, SCHEME_WINDOWS_PATH_KIND);
      }
    }
  } else {
#ifdef TILDE_IS_ABSOLUTE
    if (protect) {
      if (chars[d] == '~') {
        char *nm;
        if (just_check)
          return scheme_true;
        nm = (char *)scheme_malloc_atomic(len + 3);
        memcpy(nm XFORM_OK_PLUS 2, chars XFORM_OK_PLUS d, len);
        nm[0] = '.';
        nm[1] = '/';
        nm[len + 2] = 0;
        return scheme_make_sized_offset_kind_path(nm, 0, len + 2, 0, kind);
      }
    }
#endif
  }

  if (just_check)
    return scheme_false;

  return scheme_make_sized_offset_kind_path(chars, d, len, copy, kind);
}

#ifdef DOS_FILE_SYSTEM
static Scheme_Object *make_protected_path(char *chars)
{
  return make_protected_sized_offset_path(1, chars, 0, strlen(chars), 1, 0, SCHEME_WINDOWS_PATH_KIND);
}
#endif

Scheme_Object *make_exposed_sized_offset_path(int already_protected, 
					      char *chars, intptr_t d, intptr_t len, int copy,
                                              int kind)
  /* Called to make a directory path where the end has been removed.
     We may need to remove a redundant separator.
     Under Windows, if the resulting last element has spaces or is a 
     special file, then we need to protect it with "\\?\". */
{
  if (kind == SCHEME_WINDOWS_PATH_KIND) {
    if (!already_protected) {
      int i, name_end;
      int non_dot = 0, trailing_dots = 0, protect = 0;
      /* Skip trailing seps: */
      for (i = d + len - 1; (i > d) && IS_A_DOS_SEP(chars[i]); --i) {
      }
      name_end = i+1;
      for (; (i > d) && !IS_A_DOS_SEP(chars[i]); --i) {
        if ((chars[i] != ' ') && (chars[i] != '.'))
          non_dot = 1;
        else if (!non_dot)
          trailing_dots = 1;
      }
      if (non_dot && trailing_dots)
        protect = 1;
      else if (name_end == (d + len))
        protect = is_special_filename(chars, i+1, name_end, 0, 1);

      if (protect) {
        Scheme_Object *first, *last, *a[2];
        char *s2;
        int l;
        l = name_end - (i+1);
        s2 = (char *)scheme_malloc_atomic(l + 9 + 1);
        memcpy(s2, "\\\\?\\REL\\\\", 9);
        memcpy(s2+9, chars + i + 1, l);
        s2[l + 9] = 0;
        last = scheme_make_sized_offset_kind_path(s2, 0, l+9, 0, SCHEME_WINDOWS_PATH_KIND);
        first = make_exposed_sized_offset_path(0, chars, d, i-d+1, 1, SCHEME_WINDOWS_PATH_KIND);
        a[0] = first;
        a[1] = last;
        return scheme_build_path(2, a);
      }
    }
  }

  /* We may need to remove a redundant separator from the directory
     path. Try removing it, and see if anyone would care: */
  if (do_path_to_directory_path(chars, d, len - 1, scheme_true, 1, kind)) {
    /* Actually, don't remove a separator after a drive, although it's
       technically redundant. */
    if ((kind != SCHEME_WINDOWS_PATH_KIND)
        || !((len == 3) && is_drive_letter(chars[d]) && (chars[d+1] == ':'))) {
      len--;
      copy = 1;
    }
  }

  return scheme_make_sized_offset_kind_path(chars, d, len, copy, kind);
}

Scheme_Object *scheme_make_path(const char *chars)
{
  return scheme_make_sized_offset_path((char *)chars, 0, -1, 1);
}

Scheme_Object *scheme_make_sized_path(char *chars, intptr_t len, int copy)
{
  return scheme_make_sized_offset_path(chars, 0, len, copy);
}

Scheme_Object *scheme_make_path_without_copying(char *chars)
{
  return scheme_make_sized_offset_path(chars, 0, -1, 0);
}

static Scheme_Object *append_path(Scheme_Object *a, Scheme_Object *b)
{
  Scheme_Object *s;
  s = scheme_append_byte_string(a, b);
  s->type = SCHEME_PLATFORM_PATH_KIND;
  return s;
}

Scheme_Object *scheme_char_string_to_path(Scheme_Object *p)
{
  p = scheme_char_string_to_byte_string_locale(p);
  p->type = SCHEME_PLATFORM_PATH_KIND;
  return p;
}


static Scheme_Object *path_p(int argc, Scheme_Object **argv)
{
  return (SCHEME_PATHP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *general_path_p(int argc, Scheme_Object **argv)
{
  return (SCHEME_GENERAL_PATHP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *path_kind(int argc, Scheme_Object **argv)
{
  if (SCHEME_GENERAL_PATHP(argv[0])) {
    switch (SCHEME_PATH_KIND(argv[0])) {
    case SCHEME_WINDOWS_PATH_KIND:
      return windows_symbol;
      break;
    default:
    case SCHEME_UNIX_PATH_KIND:
      return unix_symbol;
      break;
    }
  } else {
    scheme_wrong_contract("path-system-type", "path-for-some-system?", 0, argc, argv);
    return NULL;
  }
}

static Scheme_Object *platform_path_kind(int argc, Scheme_Object **argv)
{
  switch (SCHEME_PLATFORM_PATH_KIND) {
  case SCHEME_WINDOWS_PATH_KIND:
    return windows_symbol;
    break;
  default:
  case SCHEME_UNIX_PATH_KIND:
    return unix_symbol;
    break;
  }
}

static Scheme_Object *drop_rel_prefix(Scheme_Object *p)
/* Drop \\?\REL\ prefix */
{
  int drive_end;
  if (check_dos_slashslash_qm(SCHEME_PATH_VAL(p),
                              SCHEME_PATH_LEN(p),
                              &drive_end, NULL, NULL)) {
    if (drive_end < 0) {
      /* \\?\REL\ */
      int delta;
      if (SCHEME_PATH_VAL(p)[8] == '\\')
        delta = 9;
      else
        delta = 8;
      p = scheme_make_sized_offset_kind_path(SCHEME_BYTE_STR_VAL(p),
                                             delta,
                                             SCHEME_BYTE_STRLEN_VAL(p) - delta,
                                             1,
                                             SCHEME_WINDOWS_PATH_KIND);
    }
  }

  return p;
}

Scheme_Object *scheme_path_to_char_string(Scheme_Object *p)
{
  Scheme_Object *s;

  s = scheme_byte_string_to_char_string_locale(p);

  if (!SCHEME_CHAR_STRLEN_VAL(s))
    return scheme_make_utf8_string("?");
  else
    return s;
}

static Scheme_Object *path_to_string(int argc, Scheme_Object **argv)
{
  if (!SCHEME_PATHP(argv[0]))
    scheme_wrong_contract("path->string", "path?", 0, argc, argv);

  return scheme_path_to_char_string(argv[0]);
}

static Scheme_Object *path_to_bytes(int argc, Scheme_Object **argv)
{
  if (!SCHEME_GENERAL_PATHP(argv[0]))
    scheme_wrong_contract("path->bytes", "path?", 0, argc, argv);

  return scheme_make_sized_byte_string(SCHEME_PATH_VAL(argv[0]),
				       SCHEME_PATH_LEN(argv[0]),
				       1);
}

static Scheme_Object *is_path_element(Scheme_Object *p)
{
  Scheme_Object *base, *fn;
  int isdir;

  fn = scheme_split_path(SCHEME_PATH_VAL(p), 
                         SCHEME_PATH_LEN(p), 
                         &base, 
                         &isdir,
                         SCHEME_PATH_KIND(p));

  if (SCHEME_SYMBOLP(base)
      && SCHEME_GENERAL_PATHP(fn))
    return fn;
  return NULL;
}

static Scheme_Object *do_path_element_to_bytes(const char *name, int argc, Scheme_Object **argv)
{
  Scheme_Object *p = argv[0], *pe;
  int kind;

  if (!SCHEME_GENERAL_PATHP(p))
    scheme_wrong_contract(name, "path?", 0, argc, argv);
  
  pe = is_path_element(p);

  if (!pe)
    scheme_contract_error(name,
                          "path can be split or is not relative",
                          "path", 1, p,
                          NULL);

  if (SCHEME_SYMBOLP(pe)) {
    scheme_contract_error(name,
                          (SAME_OBJ(pe, up_symbol)
                           ? "path is an up-directory indicator"
                           : "path is a same-directory indicator"),
                          "path", 1, p,
                          NULL);
  }

  p = pe;

  kind = SCHEME_PATH_KIND(p);

  if (kind == SCHEME_UNIX_PATH_KIND) {
#ifdef TILDE_IS_ABSOLUTE
    /* Drop ./ of ./~ prefix */
    if ((SCHEME_PATH_VAL(p)[0] == '.')
        && (SCHEME_PATH_VAL(p)[1] == '/')
        && (SCHEME_PATH_VAL(p)[2] == '~')) {
      p = scheme_make_sized_offset_byte_string(SCHEME_PATH_VAL(p), 
                                               2, 
                                               SCHEME_PATH_LEN(p) - 2, 
                                               1);
    }
#endif
  }
  if (kind == SCHEME_WINDOWS_PATH_KIND) {
    p = drop_rel_prefix(p);
  }

  return scheme_make_sized_byte_string(SCHEME_PATH_VAL(p),
				       SCHEME_PATH_LEN(p),
				       1);
}

static Scheme_Object *path_element_to_bytes(int argc, Scheme_Object **argv)
{
  return do_path_element_to_bytes("path-element->bytes", argc, argv);
}

static Scheme_Object *path_element_to_string(int argc, Scheme_Object **argv)
{
  Scheme_Object *b;
  b = do_path_element_to_bytes("path-element->string", argc, argv);
  return scheme_byte_string_to_char_string_locale(b);
}

static void check_path_ok(const char *who, Scheme_Object *p, Scheme_Object *o)
{
  if (has_null(SCHEME_PATH_VAL(p), SCHEME_PATH_LEN(p))) {
    raise_null_error(who, o, "");
  }
}

static Scheme_Object *string_to_path(int argc, Scheme_Object **argv)
{
  Scheme_Object *p;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("string->path", "string?", 0, argc, argv);

  p = scheme_char_string_to_path(argv[0]);
  
  check_path_ok("string->path", p, argv[0]);

  return p;
}

static int extract_path_kind(const char *who, int which, int argc, Scheme_Object **argv)
{
  if (which >= argc)
    return SCHEME_PLATFORM_PATH_KIND;
  
  if (SAME_OBJ(argv[which], windows_symbol))
    return SCHEME_WINDOWS_PATH_KIND;
  if (SAME_OBJ(argv[which], unix_symbol))
    return SCHEME_UNIX_PATH_KIND;

  scheme_wrong_contract(who, "(or/c 'unix 'windows)", which, argc, argv);
  return 0;
}

static Scheme_Object *bytes_to_path(int argc, Scheme_Object **argv)
{
  Scheme_Object *s;
  int kind;

  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_contract("bytes->path", "bytes?", 0, argc, argv);
  kind = extract_path_kind("bytes->path", 1, argc, argv);

  s = scheme_make_sized_byte_string(SCHEME_BYTE_STR_VAL(argv[0]),
				    SCHEME_BYTE_STRLEN_VAL(argv[0]),
				    SCHEME_MUTABLEP(argv[0]));
  s->type = kind;

  check_path_ok("bytes->path", s, argv[0]);

  return s;
}

static Scheme_Object *do_bytes_to_path_element(const char *name, Scheme_Object *s, int argc, Scheme_Object **argv)
{
  Scheme_Object *p;
  intptr_t i, len;
  int kind;

  if (!SCHEME_BYTE_STRINGP(s))
    scheme_wrong_contract(name, "bytes?", 0, argc, argv);
  kind = extract_path_kind(name, 1, argc, argv);

  len = SCHEME_BYTE_STRLEN_VAL(s);
  for (i = 0; i < len; i++) {
    if (IS_A_PRIM_SEP(kind, SCHEME_BYTE_STR_VAL(s)[i])) {
      break;
    }
  }

  if (i >= len)
    p = make_protected_sized_offset_path(1, SCHEME_BYTE_STR_VAL(s),
                                         0, len,
                                         SCHEME_MUTABLEP(s), 0,
                                         kind);
  else
    p = NULL;
  
  if (!p || !is_path_element(p))
    scheme_contract_error(name,
                          "cannot be converted to a path element",
                          "path", 1, argv[0],
                          "explanation", 0, "path can be split, is not relative, or names a special element",
                          NULL);

  return p;
}

static Scheme_Object *bytes_to_path_element(int argc, Scheme_Object **argv)
{
  return do_bytes_to_path_element("bytes->path-element", argv[0], argc, argv);
}

static Scheme_Object *string_to_path_element(int argc, Scheme_Object **argv)
{
  Scheme_Object *b;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("string->path-element", "string?", 0, argc, argv);

  b = scheme_char_string_to_byte_string_locale(argv[0]);
  
  return do_bytes_to_path_element("string->path-element", b, argc, argv);
}

/**********************************************************************/
/*                                                                    */
/**********************************************************************/


#ifdef DOS_FILE_SYSTEM
static char *mz_getcwd(char *s, int l)
{
 int need_l, bl = 256;
 wchar_t buffer[256], *wbuf;

 wbuf = buffer;
 while (1) {
   need_l = GetCurrentDirectoryW(bl, wbuf);
   if (need_l > bl) {
     wbuf = (wchar_t *)scheme_malloc_atomic(need_l * sizeof(wchar_t));
     bl = need_l;
   } else
     break;
 }

 if (!need_l)
   return NULL;
 
 bl = scheme_utf8_encode((unsigned int *)wbuf, 0, need_l, NULL, 0, 1 /*UTF-16*/);
 if (bl + 1 > l) {
   s = (char *)scheme_malloc_atomic(bl + 1);
 }
 bl = scheme_utf8_encode((unsigned int *)wbuf, 0, need_l, s, 0, 1 /*UTF-16*/);
 s[bl] = 0;

 return s;
}
#else
# define mz_getcwd MSC_IZE(getcwd)
#endif

char *scheme_os_getcwd(char *buf, int buflen, int *actlen, int noexn)
{
# define GETCWD_BUFSIZE 1024
  char buffer[GETCWD_BUFSIZE], *r, *gbuf;
  int obuflen = buflen;

  if (buflen < GETCWD_BUFSIZE) {
    gbuf = buffer;
    buflen = GETCWD_BUFSIZE;
  } else
    gbuf = buf;

  r = mz_getcwd(gbuf, buflen - 1);
  if (!r) {
    char *r2;

    r = mz_getcwd(NULL, 0);
    if (!r) {
      /* Something bad happened! */
      if (noexn) {
        /* We need to invent some complete path. */
#ifdef DOS_FILE_SYSTEM
        r = "C:\\";
#else
        r = "/";
#endif        
	if (actlen)
	  *actlen = strlen(r);

	if (buf) {
          strcpy(buf, r);
	  return buf;
	} else {
	  return r;
	}
      }
	
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM, 
		       "current-directory: unknown failure\n"
                       "  system error: %e", 
                       errno);
    }

    buflen = strlen(r) + 1;
    r2 = (char *)scheme_malloc_atomic(buflen);
    memcpy(r2, r, buflen);
    r2[buflen] = 0;
    free(r);
    r = r2;

    if (actlen)
      *actlen = buflen;
  } else {
    int slen = strlen(r) + 1;

    if (actlen)
      *actlen = slen;

    if (obuflen < slen)
      r = scheme_strdup(r);
    else if (r != buf) {
      memcpy(buf, r, slen);
      r = buf;
    }
  }
     
  return r;
}

int scheme_os_setcwd(char *expanded, int noexn)
{
  int err;

  while (1) {
    err = MSC_W_IZE(chdir)(MSC_WIDE_PATH(expanded));
    if (!err || (errno != EINTR))
      break;
  }

  if (err && !noexn)
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "current-directory: unable to switch to directory\n"
                       "  path: %q",
		       expanded);

  return !err;
}

#ifdef DOS_FILE_SYSTEM
#define WC_BUFFER_SIZE 1024
THREAD_LOCAL_DECL(static void *file_path_wc_buffer);

static int wc_strlen(const wchar_t *ws)
{
  int l;
  for (l =0; ws[l]; l++) { }
  return l;
}

wchar_t *scheme_convert_to_wchar(const char *s, int do_copy)
     /* This function uses '\t' in place of invalid UTF-8 encoding
	bytes, because '\t' is not a legal filename under Windows. */
{
  intptr_t len, l;
  wchar_t *ws;

  l = strlen(s);
  len = scheme_utf8_decode(s, 0, l,
			   NULL, 0, -1,
			   NULL, 1/*UTF-16*/, '\t');

  if (!do_copy && (len < (WC_BUFFER_SIZE-1))) {
    if (!file_path_wc_buffer) {
      REGISTER_SO(file_path_wc_buffer);
      file_path_wc_buffer = scheme_malloc_atomic(sizeof(wchar_t) * WC_BUFFER_SIZE);
    }
    ws = (wchar_t *)file_path_wc_buffer;
  } else
    ws = (wchar_t *)scheme_malloc_atomic(sizeof(wchar_t) * (len + 1));
  scheme_utf8_decode(s, 0, l,
		     (unsigned int *)ws, 0, -1,
		     NULL, 1/*UTF-16*/, '\t');
  ws[len] = 0;
  return ws;
}

char *scheme_convert_from_wchar(const wchar_t *ws)
{
  intptr_t len, l;
  char *s;

  l = wc_strlen(ws);
  len = scheme_utf8_encode((unsigned int *)ws, 0, l,
			   NULL, 0,
			   1/*UTF-16*/);
  s = (char *)scheme_malloc_atomic(len + 1);
  scheme_utf8_encode((unsigned int *)ws, 0, l,
		     s, 0,
		     1/*UTF-16*/);
  s[len] = 0;
  return s;
}
#endif


Scheme_Object *scheme_get_file_directory(const char *filename)
{
  int isdir;
  Scheme_Object *base;
  
  scheme_split_path(filename, strlen(filename), &base, &isdir, SCHEME_PLATFORM_PATH_KIND);
  
  return base;
}

Scheme_Object *scheme_remove_current_directory_prefix(Scheme_Object *fn)
{
  Scheme_Object *cwd;
  intptr_t len;

  cwd = scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_DIRECTORY);

  fn = TO_PATH(fn);

  len = SCHEME_PATH_LEN(cwd);
  if ((len < SCHEME_PATH_LEN(fn))
      && !scheme_strncmp(SCHEME_PATH_VAL(cwd), SCHEME_PATH_VAL(fn), len)) {
    /* Skip over path separators: */
    while (IS_A_SEP(SCHEME_PLATFORM_PATH_KIND, SCHEME_PATH_VAL(fn)[len])) {
      len++;
    }

    return scheme_make_sized_offset_path(SCHEME_PATH_VAL(fn), len, SCHEME_PATH_LEN(fn) - len, 1);
  }

  return fn;
}

static int has_null(const char *s, intptr_t l)
{
  if (!l)
    return 1;

  while (l--) {
    if (!s[l])
      return 1;
  }

  return 0;
}

static void raise_null_error(const char *name, Scheme_Object *path, const char *mod)
{
  if (!(SCHEME_CHAR_STRINGP(path) ? SCHEME_CHAR_STRTAG_VAL(path) : SCHEME_PATH_LEN(path)))
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: path string%s is empty", 
		     name, mod);
  else
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: path string%s contains a null character\n"
                     "  path string: %Q", 
		     name, mod, 
		     path);
}

static int check_dos_slashslash_qm(const char *next, int len, 
				   int *drive_end, int *clean_start, int *add_sep)
/* Check starting with exactly \\?\, which prefixes an absolute path
   to be passed on to the filesystem without changes.

   If it's a \\?\ path, then drive_end is set to the first character
   after the root specification. For example, if the drive is
   terminated by \\\ (a weird "root"), then drive_end is set to after
   the third \.  If the drive is \\?\C:\, then drive_end is after the
   last slash, unless thre's one extra slash, in which case drive_end
   is after that slash, too. In the case of \\?\UNC\..., drive_end
   is after the UNC part as in check_dos_slashslash_drive(). If
   it's a \\?\REL\ or \\?\RED\ path, then drive_end is set to -1(!)
   or -2(!)!; use get_slashslash_qm_dot_ups_end() to get more information.

   clean_start is the position where it's ok to start removing
   extra slahes. It's usually set to the same thing as drive_end. In the
   case of a \\?\UNC\ path, clean_start is set to 7 (i.e., just after
   that prefix). In the case of a \\?\REL\ or \\?\RED\ path, clean_start
   is the end of the string.

   If add_sep is set, it points to a place where an extra separator
   might need to be inserted.
*/
{
  if ((len >= 4)
      && (next[0] == '\\')
      && (next[1] == '\\')
      && (next[2] == '?')
      && (next[3] == '\\')) {
    int base;
    if (!drive_end && !clean_start && !add_sep)
      return 1;
    if (next[4] == '\\')
      base = 5;
    else
      base = 4;
    /* If there's two backslashes in a row at the end, count everything
       as the drive. There are two exceptions: two backslashes are ok
       at the end in the form \\?\C:\\, and \\?\\\ is \\?\ */
    if ((len > 5)
	&& (next[len - 1] == '\\')
	&& (next[len - 2] == '\\')) {
      if (len == 6) {
	/* \\?\ is the root */
      } else if ((len != 8)
	  || !is_drive_letter(next[base])
	  || (next[base+1] != ':')) {
	if (drive_end)
	  *drive_end = len;
	if (clean_start)
	  *clean_start = len;
	if (add_sep)
	  *add_sep = len;
	return 1;
      }
    }
    /* If there's three backslashes in a row, count everything
       up to the slashes as the drive. */
    if (len > 6) {
      int i;
      for (i = len; --i > 5; ) {
	if ((next[i] == '\\')
	    && (next[i-1] == '\\')
	    && (next[i-2] == '\\')) {
	  if (drive_end)
	    *drive_end = i+1;
	  if (clean_start)
	    *clean_start = i+1;
	  return 1;
	}
      }
    }

    if ((len > 6)
	&& is_drive_letter(next[base])
	&& next[base+1] == ':'
	&& next[base+2] == '\\') {
      if (clean_start)
	*clean_start = base+2;
      if (drive_end) {
	if ((len > base+3) && next[base+3] == '\\')
	  *drive_end = base+4;
	else
	  *drive_end = base+3;
      }
    } else if ((len > base+3)
	       && ((next[base] == 'U') || (next[base] == 'u'))
	       && ((next[base+1] == 'N') || (next[base+1] == 'n'))
	       && ((next[base+2] == 'C') || (next[base+2] == 'c'))
	       && (next[base+3] == '\\')
	       && check_dos_slashslash_drive(next, 
					     (((len > (base+4)) && (next[base+4] == '\\'))
					      ? base+5
					      : base+4),
					     len, drive_end, 0, 1)) {
      /* drive_end set by check_dos_slashslash_drive */
      if (clean_start)
	*clean_start = base+3;
    } else if ((base == 4) 
	       && (len > 8)
	       && (next[4] == 'R')
	       && (next[5] == 'E')
	       && ((next[6] == 'L') || (next[6] == 'D'))
	       && (next[7] == '\\')
	       && ((next[8] != '\\')
		   || (len > 9))) { 
      if (drive_end)
	*drive_end = ((next[6] == 'L') ? -1 : -2);
      if (clean_start)
	*clean_start = len; /* caller will have to use get_slashslash_qm_dot_ups_end */
    } else {
      if (drive_end)
	*drive_end = 4;
      if (clean_start) {
	if (((len == 5) && (next[4] == '\\'))
	    || ((len == 6) && (next[4] == '\\') && (next[5] == '\\')))
	  *clean_start = 3;
	else
	  *clean_start = 4;
      }
      if (add_sep)
	*add_sep = 4;
    }
    return 1;
  }
  return 0;
}

static int check_dos_slashslash_drive(const char *next, int delta, int len, 
				      int *drive_end, int exact, int no_fw)
/* Returns 1 if this path is a UNC path, 0 otherwise.
   (It starts by checking for \\?\ paths, so they won't be
   treated as UNC. Unless delta is non-0, in which case the
   check isn't necessary, presumably because the original
   `next' already started with \\?\UNC\.)
   For a 1 result, drive_end (if not NULL) is set to point to the
   byte after the \\server\vol; so, drive_end points to either
   a separator or NUL char.
   If exact is 1, then a 1 is returned only if `next' is just the
   drive; that is, only if 1 would be returned and only slashes are
   in `next' starting with `*drive_end'.
   If `no_fw' is set, then only backslashes are recognized.
*/
{
  int j;
  int is_drive = 0;

  if (drive_end)
    *drive_end = len;

  if (!delta && check_dos_slashslash_qm(next, len, NULL, NULL, NULL))
    return 0;

#define IS_X_SEP(c) (no_fw ? (c == '\\') : IS_A_DOS_SEP(c))

  if (delta || (IS_A_DOS_SEP(next[0]) && IS_A_DOS_SEP(next[1]))) {
    /* Found two separators... */
    /* Check for a drive form: //x/y */
    j = delta ? delta : 2;
    if (!IS_X_SEP(next[j])) {
      /* Found non-sep; skip over more */
      for (; j < len; j++) {
	if (IS_X_SEP(next[j])) {
	  /* Found sep again, so we have //x/: */
	  j++;
	  if (no_fw && (j < len) && IS_X_SEP(next[j]))
	    j++; /* two backslashes ok in \\?\UNC mode */
	  if ((j == (delta ? (delta + 2) : 4))
	      && (next[j - 2] == '?')) {
	    /* We have //?/, with up to 2 backslashes.
	       This doesn't count as UNC, to avoid confusion with \\?\. */
	  } else if ((j < len) && !IS_X_SEP(next[j])) {
	    /* Found non-sep again; this is UNC */
	    for (; j < len; j++) {
	      if (IS_X_SEP(next[j])) {
		/* Found sep again. */
		if (drive_end)
		  *drive_end = j;
		if (exact) {
		  for (; j < len; j++) {
		    if (!IS_X_SEP(next[j])) {
		      /* Found non-sep again 
			 - not a drive (it's absolute path) */
		      break;
		    }
		  }
		} else
		  is_drive = 1;
		break;
	      }
	    }
	    if (j >= len)
	      is_drive = 1;
	    break;
	  }
	  break;
	} else if (IS_A_DOS_SEP(next[j])) {
	  /* Found / when only \ is allowed as separator */
	  break;
	}
      }
    }
  }

  return is_drive;
}

static int get_slashslash_qm_dot_ups_end(const char *s, int len, int *_lit_start)
  /* If str is of the form \\?\REL\..\..\.., returns the index just
     past the last "\..". This might be the first "\" of a "\\"
     separator, the "\" before a non-".." element, or the end of the
     string. For a \\?\RED\ path, it's as if there are no ".."s
     (because ".." is not special in "RED" paths).  The _lit_start
     value is filled with the starting index of the literal part of
     the path (i.e., after one or two slashes). */
{
  int pos = -1, j = 7; /* \\?\REL\ or \\?\RED\ */

  if (s[6] == 'L') {
    while (1) {
      if (j + 3 > len) {
        break;
      } else if ((s[j] == '\\') && (s[j+1] == '.') && (s[j+2] == '.')
                 && ((j + 3 == len) || (s[j+3] == '\\'))) {
        pos = j + 3;
        j += 3;
      } else {
        break;
      }
    }
  }

  if (pos > 0) {
    if (pos == len) 
      *_lit_start = len;
    else if ((pos + 2 < len)
	     && s[pos+1] == '\\') {
      *_lit_start = pos + 2;
    } else {
      *_lit_start = pos + 1;
    }
  } else if (len > 8) {
    if (s[8] == '\\')
      *_lit_start = 9;
    else
      *_lit_start = 8;
  } else
    *_lit_start = len;

  return pos;
}

static char *convert_to_backslashbackslash_qm(char *cleaned, int *_clen, char *str, int *_alloc, int len)
     /* cleaned (bad name) is input; str must be NULL or at least
	*_clen + 10; alloc is size of str; result maybe goes into str,
	but new srt may be returned, and result length is in
	*_clen. len is amount extract expected to be useful in str. */
{
  int clen = *_clen, pos;
  int alloc = *_alloc;

  if (!str) {
    alloc = clen + 10;
    str = scheme_malloc_atomic(alloc);
  }

  {
    int cde = 0;
    if (!check_dos_slashslash_drive(cleaned, 0, clen, &cde, 0, 0))
      cde = 0;
    cleaned = remove_redundant_slashes(cleaned, &clen, cde, NULL, SCHEME_WINDOWS_PATH_KIND);
  }
  cleaned = do_normal_path_seps(cleaned, &clen, 0, 1, SCHEME_WINDOWS_PATH_KIND, NULL);
  if (scheme_is_relative_path(cleaned, clen, SCHEME_WINDOWS_PATH_KIND)) {
    memcpy(str, "\\\\?\\REL\\", 8);
    memcpy(str + 8, cleaned, clen);
    pos = clen + 8;
  } else {
    int plen, xdel = 0;
    if (cleaned[0] == '\\') {
      if (cleaned[1] == '\\') {
        /* UNC */
        xdel = 1;
        plen = 7;
        pos = 0; /* reset below */
      } else {
        /* Drive-relative absolute. */
        memcpy(str, "\\\\?\\RED\\", 8);
        memcpy(str + 8, cleaned, clen);
        pos = clen + 8;
        plen = 0;
      }
    } else {
      plen = 4;
      pos = 0; /* reset below */
    }
    if (plen) {
      memcpy(str, "\\\\?\\UNC", plen);
      memcpy(str + plen, cleaned + xdel, clen - xdel);
      pos = clen + plen - xdel;
    }
  }

  *_alloc = alloc;
  *_clen = pos;
  return str;
}

static char *get_drive_part(const char *wds, int wdlen)
{
  int dend, dstart = 0;
  char *naya;

  if (check_dos_slashslash_qm(wds, wdlen, &dend, NULL, NULL)) {
    /* dend can't be < 0, because that's a relative path */
  } else if (!check_dos_slashslash_drive(wds, 0, wdlen, &dend, 0, 0))
    dend = 3;

  naya = scheme_malloc_atomic(dend + 1);
  memcpy(naya + dstart, wds, dend);
  naya[dend] = 0;

  return naya;
}

char *scheme_getdrive()
{
  scheme_security_check_file("current-drive", NULL, SCHEME_GUARD_FILE_EXISTS);
#ifdef DOS_FILE_SYSTEM
  {
    Scheme_Object *wd;
    wd = CURRENT_WD();
    return get_drive_part(SCHEME_PATH_VAL(wd), SCHEME_PATH_LEN(wd));
  }
#else
  return "";
#endif
}

char *strip_trailing_spaces(const char *s, int *_len, int delta, int in_place)
  /* Strips trailing dots, too */
{
  int len, skip_end = 0;

  if (_len)
    len = *_len;
  else
    len = strlen(s);

  /* Keep separators that are at the very end: */
  if ((len - skip_end > delta) && IS_A_DOS_SEP(s[len - 1 - skip_end])) {
    skip_end++;
  }

  if ((len - skip_end > delta) 
      && ((s[len - 1 - skip_end] == ' ') || (s[len - 1 - skip_end] == '.'))) {
    char *t;
    int orig_len = len;

    while ((len - skip_end > delta) 
	   && ((s[len - 1 - skip_end] == ' ') || (s[len - 1 - skip_end] == '.'))) {
      len--;
    }

    /* If the path element doesn't contain any non-space non-. chars, don't
       strip them after all. */
    if ((len - skip_end > delta) && !IS_A_DOS_SEP(s[len - 1 - skip_end])) {
      if (in_place)
	t = (char *)s;
      else {
	t = (char *)scheme_malloc_atomic(len + 1);
	memcpy(t, s, len - skip_end);
      }
      memcpy(t + len - skip_end, t + orig_len - skip_end, skip_end);
      t[len] = 0;
      
      if (_len)
	*_len = len;
      
      return t;
    }
  }

  return (char *)s;
}

/* Watch out for special device names. Could we do better than hardwiring this list? */
READ_ONLY static const char *special_filenames[] = { "NUL", "CON", "PRN", "AUX", /* NT only: "CLOCK$", */
                                     "COM1", "COM2", "COM3", "COM4", "COM5", 
                                     "COM6", "COM7", "COM8", "COM9",
                                     "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", 
                                     "LPT6", "LPT7", "LPT8", "LPT9", NULL };

static int is_special_filename(const char *f, int offset, int len, int not_nul, int immediate)
{
  int i, j, delta;

  /* Skip over path: */
  if (!len)
    return 0;
  if (!immediate) {
    delta = len;
    if (check_dos_slashslash_qm(f, delta, NULL, NULL, NULL))
      return 0;
    delta -= 1;
    while (delta && !IS_A_DOS_SEP(f[delta])) {
      --delta;
    }
    if (!delta && is_drive_letter(f[0]) && f[1] == ':') {
      delta = 2;
    } else if (IS_A_DOS_SEP(f[delta]))
      delta++;
  } else
    delta = offset;

  for (i = not_nul; special_filenames[i]; i++) {
    const char *sf = special_filenames[i];
    for (j = 0; sf[j] && f[delta + j]; j++) {
      if (scheme_toupper((mzchar)(unsigned char)f[delta + j]) != sf[j])
	break;
    }
    if (j && !sf[j]) {
      j += delta;
      if ((j >= (len + offset))
	  || (f[j] == '.')
	  || (f[j] == ':'))
	return i + 1;
      while ((j < (len + offset))
	     && ((f[j] == ' ')
		 || (f[j] == '.'))) {
	j++;
      }
      if (j >= (len + offset))
	return i + 1;

      return 0;
    }
  }

  return 0;
}

#ifdef DOS_FILE_SYSTEM
int scheme_is_special_filename(const char *f, int not_nul)
{
  return is_special_filename(f, 0, strlen(f), not_nul, 0);
}
#endif

static char *remove_redundant_slashes(char *filename, int *l, int delta, int *expanded, int kind)
{
  int extra = 0, i, ilen = *l;
  
  for (i = ilen; --i > delta; ) {
    if (IS_A_SEP(kind, filename[i])) {
      if (IS_A_SEP(kind, filename[i - 1])) {
        extra++;
      }
    }
  }

  if (extra) {
    char *naya;
    naya = (char *)scheme_malloc_atomic(ilen + 1 - extra);
    extra = 0;
    for (i = delta; i < ilen; i++) {
      if (IS_A_SEP(kind, filename[i])
          && IS_A_SEP(kind, filename[i + 1])) {
        /* Skip */
        extra++;
      } else {
        naya[i - extra] = filename[i];
      }
    }
    memcpy(naya, filename, delta);
    ilen -= extra;
    naya[ilen] = 0;
    filename = naya;
    if (expanded)
      *expanded = 1;
  }
  
  *l = ilen;
  return filename;
}

static char *do_expand_filename(Scheme_Object *o, char* filename, int ilen, const char *errorin, 
				int *expanded,
				int report_bad_user, int fullpath,
				int guards, int kind, int expand_user)
{
  if (expanded)
    *expanded = 0;

  if (o) {
    o = TO_PATH(o);
    filename = SCHEME_PATH_VAL(o);
    ilen = SCHEME_PATH_LEN(o);
  }

  if (guards)
    scheme_security_check_file(errorin, filename, guards);

  if (ilen < 0)
    ilen = strlen(filename);
  else  {
    if (has_null(filename, ilen)) {
      if (errorin)
	raise_null_error(errorin, scheme_make_sized_path(filename, ilen, 1), "");
      else 
	return NULL;
    }
  }

  if (kind == SCHEME_UNIX_PATH_KIND) {
    /* User home lookup strategy taken from wxWindows: */

#ifdef UNIX_FILE_SYSTEM
    if (expand_user && (filename[0] == '~')) {
      char user[256], *home = NULL, *naya;
      struct passwd *who = NULL;
      int u, f, len, flen;
    
      for (u = 0, f = 1; 
           u < 255 && filename[f] && filename[f] != '/'; 
           u++, f++) {
        user[u] = filename[f];
      }

      if (filename[f] && filename[f] != '/') {
        if (errorin && report_bad_user)
          scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                           "%s: bad username in path\n"
                           "  path: %q", 
                           errorin, filename);
        return NULL;
      }
      user[u] = 0;

      if (!user[0]) {
        if (!(home = getenv("HOME"))) {
          char *ptr;

          ptr = getenv("USER");
          if (!ptr)
            ptr = getenv("LOGNAME");

          who = ptr ? getpwnam(ptr) : NULL;

          if (!who)
            who = getpwuid(getuid());
        }
      } else
        who = getpwnam(user);

      if (!home && who)
        home = who->pw_dir;

      if (!home) {
        if (errorin && report_bad_user)
          scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                           "%s: bad username in path\n"
                           "  path: %q", 
                           errorin, filename);
        return NULL;
      }

      len = strlen(home);
      if (f < ilen) 
        flen = ilen - f - 1;
      else
        flen = 0;
      naya = (char *)scheme_malloc_atomic(len + flen + 2);
      memcpy(naya, home, len);
      naya[len] = '/';
      memcpy(naya + len + 1, filename + f + 1, flen);
      naya[len + flen + 1] = 0;

      if (expanded)
        *expanded = 1;
  
      filename = naya;
      ilen = len + flen + 1;
    }
#endif

    /* Remove redundant slashes */
    {
      int l = ilen;
      filename = remove_redundant_slashes(filename, &l, 0, expanded, SCHEME_PLATFORM_PATH_KIND);
      ilen = l;
    }
  } else {
    /* SCHEME_WINDOWS_PATH_KIND */
    int drive_end, clean_start;
    int fixit = 0, i, insert_initial_sep = 0;

    if (!check_dos_slashslash_qm(filename, ilen, &drive_end, &clean_start, NULL))
      drive_end = 0;
    else if (drive_end < 0) {
      /* For \\?\REL\, only remove extra backslashes after 
	 unprotected ..s, so count the start of that area
	 as the drive end. */
      get_slashslash_qm_dot_ups_end(filename, ilen, &drive_end);
      /* Make sure that backslashes are doubled after dots. */
      if ((drive_end != ilen) && (filename[drive_end-2] != '\\')) {
	insert_initial_sep = 1;
	fixit = 1;
      }
    } else if (drive_end == 8) {
      /* For \\?\c:\\ path, start clean up after colon. */
      if (is_drive_letter(filename[4])
	  && (filename[5] == ':'))
	drive_end = 6;
    } else if (drive_end == 9) {
      /* For \\?\\c:\\ path, start clean up after colon. */
      if ((filename[4] == '\\')
	  && is_drive_letter(filename[5])
	  && (filename[6] == ':'))
	drive_end = 7;
    } else {
      drive_end = clean_start;
    }

    /* Check whether to clean up the name, removing mulitple // and
       adding "/" after "c:" if necessary */
    if (!drive_end 
	&& is_drive_letter(filename[0])
	&& (filename[1] == ':') 
	&& !IS_A_DOS_SEP(filename[2])) {
      drive_end = 2;
      insert_initial_sep = 1;
      fixit = 1;
    } else {
      int found_slash = 0, prim_only = drive_end;
      
      for (i = ilen; i-- > drive_end; ) {
	if (IS_A_DOS_X_SEP(prim_only, filename[i])) {
	  if (IS_A_DOS_X_SEP(prim_only, filename[i - 1])) {
	    if ((i > 1) || !found_slash)
	      fixit = 1;
	    break;
	  }
	  found_slash = 1;
	}
      }
    }

    if (fixit) {
      int pos, prim_only = drive_end;
      char *naya;
      
      if (expanded)
	*expanded = 1;
      
      if (!drive_end) {
	/* Allow // at start? */
	if (check_dos_slashslash_drive(filename, 0, ilen, NULL, 0, 0))
	  drive_end = 2;
      }

      naya = (char *)scheme_malloc_atomic(ilen + 2);

      memcpy(naya, filename, drive_end);
      pos = i = drive_end;
      if (insert_initial_sep) {
	naya[pos++] = '\\';
      }
      
      while (i < ilen) {
	if (IS_A_DOS_X_SEP(prim_only, filename[i])
            && ((i + 1) < ilen)
	    && IS_A_DOS_X_SEP(prim_only, filename[i + 1])) {
	  i++;
	} else
	  naya[pos++] = filename[i++];
      }
      
      naya[pos] = 0;
      filename = naya;
      ilen = pos;

      if (drive_end == 4) {
	/* If the root was \\?\, there's a chance that we removed a
	   backslash and changed the root. In that case, add two \\s after \\?\: */
	check_dos_slashslash_qm(filename, ilen, &drive_end, NULL, NULL);
	if (drive_end != 4) {
	  /* There's room to expand, because insert_initial_sep couldn't be -1. */
	  if (filename[4] == '\\') {
	    /* Need one more */
	    memmove(filename + 5, filename + 4, ilen - 3);
	    filename[4] = '\\'; /* Actually, this is redundant. */
	    ilen += 1;
	  } else {
	    /* Need two more */
	    memmove(filename + 6, filename + 4, ilen - 3);
	    filename[4] = '\\'; /* Actually, this is redundant. */
	    filename[5] = '\\';
	    ilen += 2;
	  }
	}
      }
    }
  }

  if (fullpath) {
    if (!scheme_is_complete_path(filename, ilen, kind)) {
      if (expanded)
	*expanded = 1;
      filename = do_path_to_complete_path(filename, ilen, NULL, 0, kind);
      ilen = strlen(filename);
    }
    if (kind == SCHEME_WINDOWS_PATH_KIND) {
      if (ilen > ((fullpath > 1) ? fullpath : 259)) {
        if (!check_dos_slashslash_qm(filename, ilen, NULL, NULL, NULL)) {
          /* Convert to \\?\ to avoid length limit. */
          int l = ilen, a = ilen + 1;
          Scheme_Object *p;

          p = scheme_make_sized_path(filename, ilen, 0);
          p = do_simplify_path(p, scheme_null, 0, 1, 0, SCHEME_WINDOWS_PATH_KIND);
          filename = SCHEME_PATH_VAL(p);
          ilen = SCHEME_PATH_LEN(p);

          filename = convert_to_backslashbackslash_qm(filename, &l, filename, &a, 0);
          filename[l] = 0;
        }
      }
    }
  }

  return filename;
}

char *scheme_expand_filename(char* filename, int ilen, const char *errorin, int *expanded, int guards)
{
  return do_expand_filename(NULL, filename, ilen, errorin, expanded, 1, 1, guards, SCHEME_PLATFORM_PATH_KIND, 0);
}

char *scheme_expand_user_filename(char* filename, int ilen, const char *errorin, int *expanded, int guards)
{
  return do_expand_filename(NULL, filename, ilen, errorin, expanded, 1, 1, guards, SCHEME_PLATFORM_PATH_KIND, 1);
}

char *scheme_expand_string_filename(Scheme_Object *o, const char *errorin, int *expanded, int guards)
{
  return do_expand_filename(o, NULL, 0, errorin, expanded, 1, 1, guards, SCHEME_PLATFORM_PATH_KIND, 0);
}

#ifdef DOS_FILE_SYSTEM
# define FIND_FIRST FindFirstFileW
# define FIND_NEXT FindNextFileW
# define FIND_CLOSE FindClose
# define FF_TYPE WIN32_FIND_DATAW
# define FF_HANDLE_TYPE HANDLE
# define FIND_FAILED(h) (h == INVALID_HANDLE_VALUE)
# define FF_A_RDONLY FILE_ATTRIBUTE_READONLY
# define FF_A_DIR FILE_ATTRIBUTE_DIRECTORY
# define GET_FF_ATTRIBS(fd) (fd.dwFileAttributes)
# define GET_FF_MODDATE(fd) convert_date(&fd.ftLastWriteTime)
# define GET_FF_NAME(fd) fd.cFileName
static time_t convert_date(const FILETIME *ft)
{
  LONGLONG l, delta;
  FILETIME ft2;
  SYSTEMTIME st;
  TIME_ZONE_INFORMATION tz;

  /* FindFirstFile incorrectly shifts for daylight saving. It
     subtracts an hour to get to UTC when daylight saving is in effect
     now, even when daylight saving was not in effect when the file
     was saved.  Counteract the difference. There's a race condition
     here, because we might cross the daylight-saving boundary between
     the time that FindFirstFile runs and GetTimeZoneInformation
     runs. Cross your fingers... */
  FileTimeToLocalFileTime(ft, &ft2);
  FileTimeToSystemTime(&ft2, &st);
  
  delta = 0;
  if (GetTimeZoneInformation(&tz) == TIME_ZONE_ID_DAYLIGHT) {
    /* Daylight saving is in effect now, so there may be a bad
       shift. Check the file's date. */
    int start_day_of_month, end_day_of_month, first_day_of_week, diff, end_shift;

    /* Valid only when the months match: */
    first_day_of_week = (st.wDayOfWeek - (st.wDay - 1 - (((st.wDay - 1) / 7) * 7)));
    if (first_day_of_week < 0)
      first_day_of_week += 7;

    diff = (tz.DaylightDate.wDayOfWeek - first_day_of_week);
    if (diff < 0)
      diff += 7;
    start_day_of_month = 1 + (((tz.DaylightDate.wDay - 1) * 7)
			      + diff);
	
    diff = (tz.StandardDate.wDayOfWeek - first_day_of_week);
    if (diff < 0)
      diff += 7;
    end_day_of_month = 1 + (((tz.StandardDate.wDay - 1) * 7)
			    + diff);

    /* Count ambigious range (when the clock goes back) as
       in standard time. We assume that subtracting the 
       ambiguous range does not go back into the previous day,
       and that the shift is a multiple of an hour. */
    end_shift = ((tz.StandardBias - tz.DaylightBias) / 60);

    if ((st.wMonth < tz.DaylightDate.wMonth)
	|| ((st.wMonth == tz.DaylightDate.wMonth)
	    && ((st.wDay < start_day_of_month)
		|| ((st.wDay == start_day_of_month)
		    && (st.wHour < tz.DaylightDate.wHour))))) {
      /* Daylight saving had not yet started. */
      delta = ((tz.StandardBias - tz.DaylightBias) * 60);
    } else if ((st.wMonth > tz.StandardDate.wMonth)
	       || ((st.wMonth == tz.StandardDate.wMonth)
		   && ((st.wDay > end_day_of_month)
		       || ((st.wDay == end_day_of_month)
			   && (st.wHour >= (tz.StandardDate.wHour
					    - end_shift)))))) {
      /* Daylight saving was already over. */
      delta = ((tz.StandardBias - tz.DaylightBias) * 60);
    }
  }

  l = ((((LONGLONG)ft->dwHighDateTime << 32) | ft->dwLowDateTime)
       - (((LONGLONG)0x019DB1DE << 32) | 0xD53E8000));
  l /= 10000000;
  l += delta;

  return (time_t)l;
}
#endif

#ifdef DOS_FILE_SYSTEM
# define MZ_UNC_READ 0x1
# define MZ_UNC_WRITE 0x2
# define MZ_UNC_EXEC 0x4

static int UNC_stat(char *dirname, int len, int *flags, int *isdir, Scheme_Object **date,
		    mzlonglong *filesize, int set_flags)
  /* dirname must be absolute */
{
  /* Note: stat() doesn't work with UNC "drive" names or \\?\ paths.
     Also, stat() doesn't distinguish between the ability to
     list a directory's content and whether the directory exists. 
     So, we use GetFileAttributesExW(). */
  char *copy;
  WIN32_FILE_ATTRIBUTE_DATA fd;
  int must_be_dir = 0;

  if (isdir)
    *isdir = 0;
  if (date)
    *date = scheme_false;

  copy = scheme_malloc_atomic(len + 14);
  if (check_dos_slashslash_qm(dirname, len, NULL, NULL, NULL)) {
    memcpy(copy, dirname, len + 1);
  } else {
    memcpy(copy, dirname, len + 1);
    while (IS_A_DOS_SEP(copy[len - 1])) {
      --len;
      copy[len] = 0;
      must_be_dir = 1;
    }
  }
  /* If we ended up with "\\?\X:", then drop the "\\?\" */
  if ((copy[0] == '\\')&& (copy[1] == '\\') && (copy[2] == '?') && (copy[3] == '\\') 
      && is_drive_letter(copy[4]) && (copy[5] == ':') && !copy[6]) {
    memmove(copy, copy + 4, len - 4);
    len -= 4;
    copy[len] = 0;
  }
  /* If we ended up with "\\?\X:", then drop the "\\?\\" */
  if ((copy[0] == '\\') && (copy[1] == '\\') && (copy[2] == '?') && (copy[3] == '\\') 
      && (copy[4] == '\\') && is_drive_letter(copy[5]) && (copy[6] == ':') && !copy[7]) {
    memmove(copy, copy + 5, len - 5);
    len -= 5;
    copy[len] = 0;
  }
  if (!GetFileAttributesExW(WIDE_PATH(copy), GetFileExInfoStandard, &fd)) {
    errno = -1;
    return 0;
  } else {
    if (set_flags != -1) {
      DWORD attrs = GET_FF_ATTRIBS(fd);

      if (!(set_flags & MZ_UNC_WRITE))
        attrs |= FF_A_RDONLY;
      else if (attrs & FF_A_RDONLY)
        attrs -= FF_A_RDONLY;
      
      if (!SetFileAttributesW(WIDE_PATH(copy), attrs)) {
        errno = -1;
        return 0;
      }
    } else {
      if (must_be_dir && !(GET_FF_ATTRIBS(fd) & FF_A_DIR))
        return 0;
      if (flags)
        *flags = MZ_UNC_READ | MZ_UNC_EXEC | ((GET_FF_ATTRIBS(fd) & FF_A_RDONLY) ? 0 : MZ_UNC_WRITE);
      if (date) {
        Scheme_Object *dt;
        time_t mdt;
        mdt = GET_FF_MODDATE(fd);
        dt = scheme_make_integer_value_from_time(mdt);
        *date = dt;
      }
      if (isdir) {
        *isdir = (GET_FF_ATTRIBS(fd) & FF_A_DIR);
      }
      if (filesize) {
        *filesize = ((mzlonglong)fd.nFileSizeHigh << 32) | fd.nFileSizeLow;
      }
    }
    return 1;
  }
}
#endif

int scheme_file_exists(char *filename)
{
# ifdef NO_STAT_PROC
  FILE *fp;

  fp = fopen(filename, "r");
  if (fp) {
    fclose(fp);
    return 1;
  } else
    return 0;
# else
#  ifdef DOS_FILE_SYSTEM
  /* Claim that all special files exist: */
  if (scheme_is_special_filename(filename, 0))
    return 1;

  {
    int isdir;
    return (UNC_stat(filename, strlen(filename), NULL, &isdir, NULL, NULL, -1)
	    && !isdir);
  }
#  else
  struct MSC_IZE(stat) buf;
  int ok;

  do {
    ok = MSC_W_IZE(stat)(MSC_WIDE_PATH(filename), &buf);
  } while ((ok == -1) && (errno == EINTR));

  return !ok && !S_ISDIR(buf.st_mode);
#  endif
# endif
}

int scheme_directory_exists(char *dirname)
{
# ifdef NO_STAT_PROC
  return 0;
# else
#  ifdef DOS_FILE_SYSTEM
  int isdir;

  return (UNC_stat(dirname, strlen(dirname), NULL, &isdir, NULL, NULL, -1)
	  && isdir);
#  else
  struct MSC_IZE(stat) buf;

  while (1) {
    if (!MSC_IZE(stat)(dirname, &buf))
      break;
    else if (errno != EINTR)
      return 0;
  }

  return S_ISDIR(buf.st_mode);
#  endif
# endif
}

int scheme_is_regular_file(char *filename)
{
# ifdef NO_STAT_PROC
  return 0;
# else
  struct MSC_IZE(stat) buf;

#  ifdef DOS_FILE_SYSTEM
  if (scheme_is_special_filename(filename, 1))
    return 0;
#  endif

  while (1) {
    if (!MSC_W_IZE(stat)(MSC_WIDE_PATH(filename), &buf))
      break;
    else if (errno != EINTR)
      return 0;
  }

  return S_ISREG(buf.st_mode);
# endif  
}

static Scheme_Object *file_exists(int argc, Scheme_Object **argv)
{
  char *f;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("file-exists?", "path-string?", 0, argc, argv);

  f = do_expand_filename(argv[0],
			 NULL,
			 0,
			 "file-exists?",
			 NULL,
			 0, 1,
			 SCHEME_GUARD_FILE_EXISTS,
                         SCHEME_PLATFORM_PATH_KIND,
                         0);

  return (f && scheme_file_exists(f)) ? scheme_true : scheme_false;
}

static Scheme_Object *directory_exists(int argc, Scheme_Object **argv)
{
  char *f;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("directory-exists?", "path-string?", 0, argc, argv);

  f = do_expand_filename(argv[0],
			 NULL,
			 0,
			 "directory-exists?",
			 NULL,
			 0, 1,
			 SCHEME_GUARD_FILE_EXISTS,
                         SCHEME_PLATFORM_PATH_KIND,
                         0);

  return (f && scheme_directory_exists(f)) ? scheme_true : scheme_false;
}

static Scheme_Object *link_exists(int argc, Scheme_Object **argv)
{
  char *filename;
#ifndef UNIX_FILE_SYSTEM
  Scheme_Object *bs;
#endif

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("link-exists?", "path-string?", 0, argc, argv);


#ifndef UNIX_FILE_SYSTEM
  /* DOS or MAC: expand isn't called, so check the form now */
  bs = TO_PATH(argv[0]);
  filename = SCHEME_PATH_VAL(bs);
  if (has_null(filename, SCHEME_PATH_LEN(bs))) {
    raise_null_error("link-exists?", bs, "");
    return NULL;
  }
#endif

#ifdef DOS_FILE_SYSTEM
  scheme_security_check_file("link-exists?", filename, SCHEME_GUARD_FILE_EXISTS);

  return scheme_false;
#endif
#ifdef UNIX_FILE_SYSTEM
  {
    struct MSC_IZE(stat) buf;

    filename = do_expand_filename(argv[0],
				  NULL,
				  0,
				  "link-exists?",
				  NULL,
				  0, 1,
				  SCHEME_GUARD_FILE_EXISTS, 
                                  SCHEME_PLATFORM_PATH_KIND,
                                  0);
    while (1) {
      if (!MSC_W_IZE(lstat)(MSC_WIDE_PATH(filename), &buf))
	break;
      else if (errno != EINTR)
	return scheme_false;
    }

    if (S_ISLNK(buf.st_mode))
      return scheme_true;
    else
      return scheme_false;
  }
#endif
}

Scheme_Object *scheme_get_fd_identity(Scheme_Object *port, intptr_t fd, char *path)
/* If path is supplied, then fd is 0 for stat, 1 for lstat */
{
  int errid = 0;
  uintptr_t devi = 0, inoi = 0, inoi2 = 0;
  int shift = 0, shift2 = -1;
  Scheme_Object *devn, *inon, *a[2];

#ifdef FILES_HAVE_FDS
  struct MSC_IZE(stat) buf;

  while (1) {
    if (!path && !MSC_IZE(fstat)(fd, &buf))
      break;
    else if (path && !fd && !MSC_IZE(stat)(path, &buf))
      break;
    else if (path && fd && !MSC_IZE(lstat)(path, &buf))
      break;
    else if (errno != EINTR) {
      errid = errno;
      break;
    }
  }
  
  if (!errid) {
    /* Warning: we assume that dev_t and ino_t fit in a long. */
    devi = (uintptr_t)buf.st_dev;
    inoi = (uintptr_t)buf.st_ino;
    shift = sizeof(dev_t);
  }
#endif
#ifdef WINDOWS_FILE_HANDLES
  BY_HANDLE_FILE_INFORMATION info;
  HANDLE fdh = (HANDLE)fd;

  errid = 0;
  if (path) {
    fdh = CreateFileW(WIDE_PATH(path),
                      0, /* not even read access => just get info */
                      FILE_SHARE_READ | FILE_SHARE_WRITE,
                      NULL,
                      OPEN_EXISTING,
                      FILE_FLAG_BACKUP_SEMANTICS,
                      NULL);
    if (fdh == INVALID_HANDLE_VALUE) {
      errid = GetLastError();
    }
  }

  if (fdh == INVALID_HANDLE_VALUE) {
    /* errid is set */
  } else {
    if (!GetFileInformationByHandle(fdh, &info))
      errid = GetLastError();
    
    if (path)
      CloseHandle(fdh);
  }

  if (!errid) {
    devi = info.dwVolumeSerialNumber;
    inoi = info.nFileIndexLow;
    inoi2 = info.nFileIndexHigh;
    shift = sizeof(DWORD);
    shift2 = 2 * sizeof(DWORD);
  }
#endif

  if (!errid) {
    devn = scheme_make_integer_value_from_unsigned(devi);
    inon = scheme_make_integer_value_from_unsigned(inoi);
    
    a[0] = inon;
    a[1] = scheme_make_integer(shift);
    inon = scheme_bitwise_shift(2, a);
    
    if (shift2 > -1) {
      a[0] = scheme_make_integer_value_from_unsigned(inoi2);
      a[1] = scheme_make_integer(shift2);
      inon = scheme_bin_plus(inon, scheme_bitwise_shift(2, a));
    }

    return scheme_bin_plus(devn, inon);
  }

  if (!path) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                     "port-file-identity: error obtaining identity\n"
                     "  system error: %)",
                     errid);
  } else {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                     "file-or-directory-identity: error obtaining identity for path\n"
                     "  path: %q\n"
                     "  system error: %E", 
                     path, 
                     errid);
  }

  return NULL;
}

static int path_is_simple_dir_without_sep(Scheme_Object *path)
{
  int len;

  len = SCHEME_PATH_LEN(path);
  if (IS_A_SEP(SCHEME_PATH_VAL(path)[len - 1], SCHEME_PATH_KIND(path)))
    return 0;

  /* The simple thing to do here is to use split_path, but that's
     a lot of extra computation. */

  if (SCHEME_PATH_VAL(path)[len - 1] == '.') {
    if (len == 1)
      return 1;
    if (IS_A_SEP(SCHEME_PATH_VAL(path)[len - 2], SCHEME_PATH_KIND(path)))
      return 1;
    if (SCHEME_PATH_VAL(path)[len - 2] == '.') {
      if (len == 2)
        return 1;
      if (IS_A_SEP(SCHEME_PATH_VAL(path)[len - 3], SCHEME_PATH_KIND(path)))
        return 1;
    }
  }

#ifdef TILDE_IS_ABSOLUTE
  if (SCHEME_PATH_KIND(path) == SCHEME_UNIX_PATH_KIND) {
    if (SCHEME_PATH_VAL(path)[0] == '~') {
      int i;
      for (i = 1; i < len; i++) {
        if (IS_A_UNIX_SEP(SCHEME_PATH_VAL(path)[i]))
          break;
      }
      if (i == len)
        return 1;
    }
  }
#endif

  if (SCHEME_PATH_KIND(path) == SCHEME_WINDOWS_PATH_KIND) {
    int drive_end;
    if (check_dos_slashslash_drive(SCHEME_PATH_VAL(path), 0, len, &drive_end, 1, 0))
      return 1; /* exactly a UNC drive */
    if (len == 2
        && (is_drive_letter(SCHEME_PATH_VAL(path)[0]))
        && (SCHEME_PATH_VAL(path)[1] == ':'))
      return 1; /* a c: path */
  }

  return 0;
}

static Scheme_Object *do_path_to_directory_path(char *s, intptr_t offset, intptr_t len, Scheme_Object *p, int just_check,
                                                int kind)
/* Although this function accepts an offset, the Windows part assumes that
   `offset' is always 0. */
{
  char *s2;
#if DROP_REDUNDANT_SLASHES
  int not_a_sep = 0;
#endif

  if (kind == SCHEME_WINDOWS_PATH_KIND) {
    int slash_dir_sep = 1;

    {
      int drive_end;

      if (offset) {
        scheme_signal_error("path->directory-path currently assumes a 0 offset");
      }

      if (check_dos_slashslash_qm(s, len, &drive_end, NULL, NULL)) {
#if DROP_REDUNDANT_SLASHES
        if (drive_end < 0) {
          /* It's a \\?\REL\ or \\?\RED\ path. */
          int litpos;
          drive_end = get_slashslash_qm_dot_ups_end(s, len, &litpos);
          /* If there's no path after the ..s, then nothing more is needed. */
          if (litpos >= len)
            return p;
        } else {
          /* If s is just a drive, then nothing more is needed. */
          if (drive_end == len)
            return p;
        }
#endif

        /* In \\?\, / can be part of a name, and it is never a separator. */
        slash_dir_sep = 0;
        /* Any "." or ".." at the end is a literal path element,
           not an up- or same-directory indicator: */
#if DROP_REDUNDANT_SLASHES
        not_a_sep = 1;
#endif
      } else {
#if DROP_REDUNDANT_SLASHES
        /* A slash after C: is not strictly necessary: */
        if ((len == 2)
            && is_drive_letter(s[offset])
            && (s[offset+1] == ':'))
          return p;
#endif
      }
    }
    {
      int cs = s[offset + len - 1];
      if (slash_dir_sep ? IS_A_DOS_SEP(cs) : (cs == '\\'))
        return p;
    }
  } else {
    if (IS_A_UNIX_SEP(s[offset + len - 1]))
      return p;
  }

#if DROP_REDUNDANT_SLASHES
  if (!not_a_sep
      && (((len > 1) && (s[offset + len - 1] == '.') && IS_A_SEP(kind, s[offset + len - 2]))
          || ((len == 1) && (s[offset] == '.'))))
    return p;
  if (!not_a_sep
      && (((len > 2) 
           && (s[offset + len - 1] == '.') 
           && (s[offset + len - 2] == '.') 
           && IS_A_SEP(kind, s[offset + len - 3]))
          || ((len == 2) && (s[offset] == '.') && (s[offset + 1] == '.'))))
    return p;
  
# ifdef TILDE_IS_ABSOLUTE
  if (kind == SCHEME_UNIX_PATH_KIND) {
    if (s[offset] == '~') {
      intptr_t i;
      for (i = 1; i < len; i++) {
        if (IS_A_UNIX_SEP(s[offset + i]))
          break;
      }
      if (i >= len)
        return p;
    }
  }
# endif
#endif

  if (just_check)
    return NULL;

  s2 = (char *)scheme_malloc_atomic(len + 2);
  memcpy(s2, s XFORM_OK_PLUS offset, len);
  s2[len] = FN_SEP(kind);
  s2[len+1] = 0;

  return scheme_make_sized_offset_kind_path(s2, 0, len + 1, 0, kind);
}

Scheme_Object *scheme_path_to_directory_path(Scheme_Object *p)
{
  return do_path_to_directory_path(SCHEME_PATH_VAL(p), 0, SCHEME_PATH_LEN(p), p, 0, 
                                   SCHEME_PATH_KIND(p));
}

static char *do_normal_path_seps(char *si, int *_len, int delta, int strip_trail, int kind, int *_did)
{
  if (kind == SCHEME_UNIX_PATH_KIND) {
    return si;
  } else {
    int i;
    unsigned char *s;
    int len = *_len;
    
    if (kind == SCHEME_WINDOWS_PATH_KIND) {
      if (!delta && check_dos_slashslash_qm(si, len, NULL, NULL, NULL))
        return si;
    }
    
    s = (unsigned char *)MALLOC_N_ATOMIC(char, len + 1);
    memcpy(s, si, len + 1);
    
    if (kind == SCHEME_WINDOWS_PATH_KIND) {
      for (i = delta; i < len; i++) {
        if (s[i] == '/') {
          if (_did)
            *_did = 1;
          s[i] = '\\';
        }
      }
      if (strip_trail)
        s = (unsigned char *)strip_trailing_spaces((char *)s, _len, delta, 1);
    }
    
    return (char *)s;
  }
}

char *scheme_normal_path_seps(char *si, int *_len, int delta)
{
  return do_normal_path_seps(si, _len, delta, 1, SCHEME_PLATFORM_PATH_KIND, NULL);
}

#define PATH_EXTRA_SPACE 4

static Scheme_Object *do_build_path(int argc, Scheme_Object **argv, int idelta, int no_final_simplify, int kind)
/* Originally, it made sense to just perform build operations
   directly on string representations, because it was simple enough.
   Over the years, though, as we refined the path syntax for Windows
   to deal with all of its idiosyncrasies, this has gotten completely 
   out of hand. */
{
#define PN_BUF_LEN 256
  int pos, i, len, no_sep;
  int alloc = PN_BUF_LEN;
  char buffer[PN_BUF_LEN], *str, *next;
  int rel, next_off;
  int first_was_drive = 0;
  int first_len = 0;
  int needs_extra_slash = 0;
  int pre_unc = 0;
  int pre_qm = 0;
  const char *who = (idelta ? "build-path/convention-type" : "build-path");

  str = buffer;
  pos = 0;

  no_sep = 0; /* This is actually initialized after we know whether
		 it's relative or not. */

  for (i = 0 ; i < argc; i++) {
    if (SCHEME_GENERAL_PATH_STRINGP(argv[i+idelta])
	|| (SCHEME_SYMBOLP(argv[i+idelta]) 
	    && (SAME_OBJ(argv[i+idelta], up_symbol)
		|| SAME_OBJ(argv[i+idelta], same_symbol)))) {
      next_off = 0;
      if (SAME_OBJ(argv[i+idelta], up_symbol)) {
        next = "..";
        len = 2;
      } else if (SAME_OBJ(argv[i+idelta], same_symbol)) {
	next = ".";
	len = 1;
      } else {
	Scheme_Object *bs;

        if (SCHEME_CHAR_STRINGP(argv[i+idelta])) {
          if (kind != SCHEME_PLATFORM_PATH_KIND) {
            scheme_contract_error(who,
                                  (idelta
                                   ? "specified convention incompatible with string path element"
                                   : "preceding path's convention incompatible with string path element"),
                                  "path element", 1, argv[i+idelta],
                                  (idelta ? "convention" : "preceding path's convention"),
                                  0,
                                  (kind == scheme_unix_path_type) ? "'unix" : "'windows",
                                  NULL); 
          }
        }

	bs = TO_PATH(argv[i+idelta]);

        if (kind != SCHEME_PATH_KIND(bs)) {
          scheme_contract_error(who,
                                (idelta
                                 ? "specified convention incompatible with given path element"
                                 : "preceding path's convention incompatible with given path element"),
                                "path element", 1, argv[i+idelta],
                                (idelta ? "convention" : "preceding path's convention"),
                                0,
                                (kind == scheme_unix_path_type) ? "'unix" : "'windows",
                                NULL); 
        }

	next = SCHEME_PATH_VAL(bs);
	len = SCHEME_PATH_LEN(bs);
	if (!len) {
	  char *astr;
	  intptr_t alen;

	  astr = scheme_make_arg_lines_string("   ", i+idelta, argc, argv, &alen);
	  scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			   "%s: path element is an empty string\n"
                           "  argument position: %d%s\n"
                           "  other arguments...:%t", 
                           who,
			   i + 1,
			   scheme_number_suffix(i + 1),
			   astr, alen);
	  return scheme_false;
	}

	if (has_null(next, len)) {
	  raise_null_error(who, argv[i+idelta], " element");
	  return NULL;
	}
      }

      if (kind == SCHEME_WINDOWS_PATH_KIND) {
	/* Strip trailing spaces before we add more path parts,
	   because trailing spaces originally don't count for the base
	   path, and they'll start counting if we add more without
	   removing the spaces first. first_len points after anything
	   that should be saved due to a \\?\ prefix. */
	int p = pos;
	strip_trailing_spaces(str, &p, first_len, 1);
	pos = p;
      }

      /* +3: null term, leading sep, and trailing sep (if up & Mac) */
      if (pos + len + PATH_EXTRA_SPACE >= alloc) {
	char *naya;
	int newalloc;

	newalloc = 2 * alloc + len + 1;
	naya = (char *)scheme_malloc_atomic(newalloc);
	memcpy(naya, str, pos);
	alloc = newalloc;
	
	str = naya;
      }

      if (kind == SCHEME_UNIX_PATH_KIND) {
        if (next[0] == '/') {
          rel = 0;
          if (i) {
            scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                             "%s: absolute path cannot be added to a path\n"
                             "  absolute path: %q",
                             who,
                             next);
            return scheme_false;
          }
        } else {
          rel = 1;
#ifdef TILDE_IS_ABSOLUTE
          if (i && (next[0] == '.') && (next[1] == '/') && (next[2] == '~')) {
            /* Strip the "./" prefix */
            next_off += 2;
            len -= 2;
          }
#endif
        }
      } else {
        /* SCHEME_WINDOWS_PATH_KIND: */
	int is_drive;

	needs_extra_slash = 0;
	
	if (IS_A_DOS_SEP(next[0])) {
	  int drive_end, plus_sep = 0;
	  rel = 0;
	  if (check_dos_slashslash_qm(next, len, &drive_end, NULL, &plus_sep)) {
	    if (drive_end < 0) {
	      /* \\?\REL\ or \\?\RED\ path */
	      rel = 1;
	      is_drive = 0;
	      if (i) {
		int dots_end, lit_start;
		int new_rel_base, need_simplify;
		int base_is_here = 0;

		/* If the current base is not a \\?\ path, turn it into one. */
		if (!check_dos_slashslash_qm(str, pos, &drive_end, NULL, NULL)) {
		  Scheme_Object *simp;

		  str[pos] = 0;
		  simp = do_simplify_path(scheme_make_sized_offset_kind_path(str, 0, pos, 0,
                                                                             SCHEME_WINDOWS_PATH_KIND),
					  scheme_null, first_len, 0, 0,
                                          SCHEME_WINDOWS_PATH_KIND);
		  if (SCHEME_FALSEP(simp)) {
		    /* Base path is just relative "here". We can ignore it. */
		    pos = 0;
		    first_len = len;
		    if (next[len] != '\\')
		      first_len++;
		    no_sep = 1;
		    new_rel_base = 0;
		  } else {
		    char *cleaned;
		    int clen;
		    int al = alloc;

		    clen = SCHEME_PATH_LEN(simp); 
		    cleaned = SCHEME_PATH_VAL(simp);

		    str = convert_to_backslashbackslash_qm(cleaned, &clen, str, &al, 
							   len + PATH_EXTRA_SPACE);

		    pos = clen;
		    alloc = al;
		    
		    if ((pos > 5)
			&& (str[4] == 'R')
			&& (str[5] == 'E'))
		      new_rel_base = 1;
		    else
		      new_rel_base = 0;

		    if (str[pos - 1] != '\\')
		      str[pos++] = '\\';
		    no_sep = 1;
		    first_len = pos;
		  }
		  need_simplify = 0;
		} else {
		  new_rel_base = (drive_end < 0);
		  need_simplify = 1;
		}
		
		if (!pos) {
		  /* Base was relative "here", so we can use next directly */
		} else {
		  dots_end = get_slashslash_qm_dot_ups_end(next, len, &lit_start);
		  
		  if (dots_end > 0) {
		    /* Add dots part of this addition, then simplify again: */
		    if (!no_sep)
		      str[pos++] = '\\';
		    memcpy(str + pos, next + 8, dots_end - 8);
		    pos += dots_end - 8;
		    str[pos] = 0;
		    need_simplify = 1;
		  }

		  if (need_simplify) {
                    /* Simplify the base path to build on: */
		    Scheme_Object *simp;

		    simp = do_simplify_path(scheme_make_sized_offset_kind_path(str, 0, pos, 0,
                                                                               SCHEME_WINDOWS_PATH_KIND),
					    scheme_null, first_len, 0, 1,
                                            SCHEME_WINDOWS_PATH_KIND);
		    if (SCHEME_FALSEP(simp)) {
                      /* Note: if root turns out to be relative, then we couldn't
                         have had a \\?\RED\ path. */
		      memcpy(str, "\\\\?\\REL\\\\", 9);
		      pos = 9;
		      no_sep = 1;
		      base_is_here = 1;
		    } else {
		      pos = SCHEME_PATH_LEN(simp);
		      memcpy(str, SCHEME_PATH_VAL(simp), pos);
		      no_sep = (str[pos - 1] == '\\');
		    }
		  }

		  /* At this point, we may have dots only in a \\?\REL
		     path in str, or we might have something without a 
		     \\ to prevent later .. from being parsed as 'up.
		     So, add a backslash if needed. */
		  if (new_rel_base && (lit_start < len)) {
		    int ls;
		    dots_end = get_slashslash_qm_dot_ups_end(str, pos, &ls);
		    if (dots_end > 0) {
		      if (ls == pos) {
			if (dots_end + 2 > pos) {
			  if (dots_end + 1 > pos)
			    str[pos++] = '\\';
			  str[pos++] = '\\';
			  no_sep = 1;
			}
		      }
		    } else if (ls == 8) {
		      memmove(str + 9, str + 8, pos - 8);
		      str[8] = '\\';
		      pos++;
		      no_sep = 1;
		    } 
		  }

		  /* Set offset into next to get only literal part, and
		     set first_len to indicate that the result will be
		     literal */
		  next_off = lit_start;
		  len -= next_off;
		  if (!len) {
		    if (base_is_here) {
		      /* Special case: base is "here" and path to add is
			 "here". Make sure result is just ".". */
		      pos = 0;
		      no_sep = 1;
		      next = ".";
		      len = 1;
		      next_off = 0;
		    } else
		      no_sep = 1;
		  } else {
		    /* One last possibility: str is \\?\ (which counts as a bizaare
		       root). We need two extra slashes. */
		    if (!new_rel_base && (pos == 4)) {
		      str[pos++] = '\\';
		      str[pos++] = '\\';
		    }
		  }
		  first_len = pos + len;
		  if (next[next_off + len] != '\\')
		    first_len++;
		}
	      } else {
		first_len = len;
	      }
	    } else {
	      /* non-REL/RED \\?\ path */
              is_drive = (drive_end == len);
	      needs_extra_slash = plus_sep;
	      if (!i) {
		first_len = len;
		if (next[first_len - 1] != '\\')
		  first_len++;
	      }
	    }
	  } else
	    is_drive = check_dos_slashslash_drive(next, 0, len, NULL, 1, 0);
	} else if ((len >= 2) 
		   && is_drive_letter(next[0])
		   && (next[1] == ':')) {
	  int j;
	  rel = 0;
	  for (j = 2; j < len; j++) {
	    if (!IS_A_DOS_SEP(next[j]))
	      break;
	  }
	  is_drive = (j >= len);
	} else {
	  rel = 1;
	  is_drive = 0;
	}

	if (!rel) {
	  if (i && (!first_was_drive || (i > 1) || is_drive)) {
	    if (pos > 30) {
	      str[27] = '.';
	      str[28] = '.';
	      str[29] = '.';
	      str[30] = 0;
	    } else
	      str[pos] = 0;
	    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			     "%s: %s cannot be added to a base path\n"
                             "  %s: %s\n"
                             "  base path: %q",
                             who,
			     is_drive ? "drive" : "absolute path",
			     is_drive ? "drive" : "absolute path",
			     next, str);
	    return scheme_false;
	  }

	  if (i == 1) {
	    /* Absolute path onto a drive: skip separator(s) */
	    while (len && IS_A_DOS_SEP(next[next_off])) {
	      next_off++;
	      len--;
	    }
	  }
	}

	if (!i)
	  first_was_drive = is_drive;
      }

      if (!i) {
	no_sep = 1;
      }
      
      if (kind == SCHEME_WINDOWS_PATH_KIND) {
        if (i) {
          pre_unc = check_dos_slashslash_drive(str, 0, pos, NULL, 0, 0);
	  if (!pre_unc) {
	    int de;
	    if (check_dos_slashslash_qm(str, pos, &de, NULL, NULL)) {
	      if (de == 4) /* \\?\ */
		pre_qm = 1;
	    }
	  } else
	    pre_qm = 0;
        } else {
          pre_unc = 1;
	  pre_qm = 0;
	}

        if (no_final_simplify
            && (len == 2) 
            && (next[next_off] == '.')
            && (next[next_off+1] == '.')
            && (first_len < pos + 2)) {
          /* Adding ".." ... */
          int de;
          if (check_dos_slashslash_qm(str, pos, &de, NULL, NULL)) {
            if (de < 0) {
              /* ... to a \\?\REL\ or \\?\RED\ path. Unless the \\?\REL\ path
                 is only dots, we need to remove a path element
                 here, instead of waiting for simplify, because simplify
                 will just push the job back here. */
              int ls, dots_end;
              dots_end = get_slashslash_qm_dot_ups_end(str, pos, &ls);
              if (ls == pos) {
                /* It's ok to add "..". Make sure we don't
                   append to "..\\" by setting pos to no more
                   than dots_end + 1. */
                if (dots_end < ls)
                  pos = dots_end + 1;
              } else {
                int q;
                for (q = pos; q-- > ls; ) {
                  if (str[q] == '\\') {
                    break;
                  }
                }
                pos = q;
                first_len = pos;
                len = 0;
                while (q && (str[q-1] == '\\')) {
                  q--;
                }
                if (q == 7) {
                  /* All we have left is \\?\REL or \\?\RED (plus a slash or two).
                     We should only get here when called by scheme_simplify. */
                  if (i + 1 == argc) {
                    /* Since we were called by scheme_simplify, use #f to mean
                       the empty path. */
                    return scheme_false;
                  }
                  /* Shouldn't ever get here, but just in case... */
                  str[0] = '.';
                  pos = 1;
                  no_sep = 1;
                  first_len = 0;
                }
              }
            }
          }
        }
      }

      if (!no_sep)
	str[pos++] = FN_SEP(kind);

      memcpy(str + pos, next + next_off, len);
      pos += len;

      if (kind == SCHEME_WINDOWS_PATH_KIND) {
        if (!pre_unc
            && check_dos_slashslash_drive(str, 0, pos, NULL, 0, 0)) {
          /* Added to //x to get something that looks like UNC. Remove the
             first [back]slash. */
          memmove(str, str+1, pos - 1);
          --pos;
        }
	if (pre_qm) {
	  int de;

	  /* Normalize path separators for the addition: */
	  {
	    int i;
	    for (i = first_len; i < pos; i++) {
	      if (str[i] == '/') {
		str[i] = '\\';
	      }
	    }
	  }

	  /* check the \\?\ parsing */
	  check_dos_slashslash_qm(str, pos, &de, NULL, NULL);
	  if (de != 4) {
	    /* Added to \\?\ to get something that now looks like 
	       a \\?\UNC path. Insert a backslash or two. */
	    int amt = ((str[4] == '\\') ? 1 : 2);
	
	    if (pos + amt >= alloc) {
	      char *naya;
	      int newalloc;
	      
	      newalloc = 2 * alloc;
	      naya = (char *)scheme_malloc_atomic(newalloc);
	      memcpy(naya, str, pos);
	      alloc = newalloc;
	      
	      str = naya;
	    }
	    memmove(str + 4 + amt, str + 4, pos - 4);
	    str[4] = '\\';
	    if (amt == 2)
	      str[5] = '\\';
	    pos += amt;
	    first_len += amt;
	  }
	}

        if (needs_extra_slash) {
          if (needs_extra_slash >= pos)
            str[pos++] = '\\';
          else if (str[needs_extra_slash] != '\\') {
            memmove(str + needs_extra_slash + 1, str + needs_extra_slash, pos - needs_extra_slash);
            str[needs_extra_slash] = '\\';
            pos++;
          }
        }
      }

      /* If last path elem ends in a separator, don't add one: */
      if (len) {
	no_sep = IS_A_SEP(kind, next[next_off + len - 1]);
      } else {
	no_sep = 0;
      }
    } else {
      scheme_wrong_contract(who, "(or/c path-for-some-system? path-string? 'up 'same)", i + idelta, argc, argv);
      return scheme_false;
    }
  }

  str[pos] = 0;

  if (kind == SCHEME_WINDOWS_PATH_KIND) {
    if (check_dos_slashslash_qm(str, pos, NULL, NULL, NULL) && !no_final_simplify) {
      /* Clean up additions to \\?\ path */
      int p;
      Scheme_Object *simp;
      p = pos;
      str = do_normal_path_seps(str, &p, first_len, 1, SCHEME_WINDOWS_PATH_KIND, NULL);
      str = remove_redundant_slashes(str, &p, first_len, NULL, SCHEME_WINDOWS_PATH_KIND);
      simp = do_simplify_path(scheme_make_sized_offset_kind_path(str, 0, p, 0, SCHEME_WINDOWS_PATH_KIND),
                              scheme_null, first_len, 0, 1, SCHEME_WINDOWS_PATH_KIND);
      if (SCHEME_FALSEP(simp))
        return scheme_make_sized_offset_kind_path(".\\", 0, 1, 0, SCHEME_WINDOWS_PATH_KIND);
      else
        return simp;
    }
  }

  return scheme_make_sized_offset_kind_path(str, 0, pos, alloc == PN_BUF_LEN, kind);
}

Scheme_Object *scheme_build_path(int argc, Scheme_Object **argv)
{
  int kind = SCHEME_PLATFORM_PATH_KIND, i;

  for (i = 0; i < argc; i++) {
    if (SCHEME_GENERAL_PATHP(argv[i])) {
      kind = SCHEME_PATH_KIND(argv[i]);
      break;
    } else if (SCHEME_CHAR_STRINGP(argv[i])) {
      kind = SCHEME_PLATFORM_PATH_KIND;
      break;
    }
  }
  
  return do_build_path(argc, argv, 0, 0, kind);
}

static Scheme_Object *build_path_kind(int argc, Scheme_Object **argv)
{ 
  int kind;

  kind = extract_path_kind("build-path/convention-type", 0, argc, argv);
  return do_build_path(argc - 1, argv, 1, 0, kind);
}

static Scheme_Object *path_to_directory_path(int argc, Scheme_Object **argv)
{
  Scheme_Object *inpath;

  inpath = argv[0];

  if (!SCHEME_GENERAL_PATH_STRINGP(inpath))
    scheme_wrong_contract("path->directory-path", "(or/c path-for-some-system? path-string?)", 0, argc, argv);

  inpath = TO_PATH(inpath);

  return scheme_path_to_directory_path(inpath);
}

static Scheme_Object *do_split_path(const char *path, int len, Scheme_Object **base_out, int *id_out,
                                    int *cleaned_slashes, int kind)
{
  char *s;
  int p, last_was_sep = 0, is_dir, no_up = 0, not_same;
  Scheme_Object *file;
  int allow_double_before = 0, drive_end, no_slash_sep = 0;

#define MAKE_SPLIT(x, y, z) (*base_out = x, *id_out = z, y)

  s = (char *)path;

  if (kind == SCHEME_WINDOWS_PATH_KIND) {
    if ((len > 2) && IS_A_DOS_SEP(s[0]) && IS_A_DOS_SEP(s[1])) {
      if (check_dos_slashslash_qm(s, len, &drive_end, NULL, NULL)) {
        allow_double_before = drive_end;
        no_slash_sep = 1;
        if (drive_end < 0) {
          /* \\?\REL\ or \\?\RED\ path. Handle it directly as a special case. */
          int p, lit_start, dots_end;
          is_dir = 0;
          if (s[len - 1] == '\\') {
            --len;
            is_dir = 1;
          }
          dots_end = get_slashslash_qm_dot_ups_end(s, len, &lit_start);
          if (lit_start < len) {
            /* There's at least one literal path. */
            for (p = len; --p >= ((dots_end > 0) ? lit_start - 1 : lit_start); ) {
              if (s[p] == '\\') {
                /* Prefix path element with \\?\REL\\: */
                {
                  int len2, nsep;
                  char *s2;
                  Scheme_Object *dir;
                  len2 = len - p - 1 + 9;
                  s2 = scheme_malloc_atomic(len2 + 1);
                  memcpy(s2, "\\\\?\\REL\\\\", 9);
                  memcpy(s2 + 9, s + p + 1, len - p - 1);
                  s2[len2] = 0;
                  if ((dots_end == p) || (dots_end == p - 1)) {
                    /* stripping the only element: drop reundant separator(s) after .. */
                    nsep = ((dots_end == p) ? 0 : -1);
                  } else {
                    if (s[6] == 'L') {
                      /* preserve separator */
                      nsep = 1;
                    } else {
                      /* preserve one separator, but not two */
                      if (s[p - 1] == '\\')
                        nsep = 0;
                      else
                        nsep = 1;
                    }
                  }
                  dir = scheme_make_sized_offset_kind_path(s, 0, p + nsep, 1, SCHEME_WINDOWS_PATH_KIND);
                  file = scheme_make_sized_offset_kind_path(s2, 0, len2, 0, SCHEME_WINDOWS_PATH_KIND);
                  return MAKE_SPLIT(dir, file, is_dir);
                }
              }
            }
          }
          /* Either no literal path elements, or only one element and no dots */
          if (dots_end > 0) {
            /* There are dots (so no literals) */
            if (dots_end - 3 > 8) {
              file = scheme_make_sized_offset_kind_path(s, 0, dots_end - 3, 1, SCHEME_WINDOWS_PATH_KIND);
              return MAKE_SPLIT(file, up_symbol, 1);
            } else
              return MAKE_SPLIT(relative_symbol, up_symbol, 1);
          } else {
            /* No dots, so there must be one element. */
            if (s[6] == 'L') {
              /* keep \\?\REL\ on path, and report 'relative as base */
              return MAKE_SPLIT(relative_symbol, 
                                scheme_make_sized_offset_kind_path(s, 0, len, 1,
                                                                   SCHEME_WINDOWS_PATH_KIND), 
                                is_dir);
            } else {
              /* Switch "D" to "L", and simplify base to just "\\" */
              char *naya;
              Scheme_Object *dir;
              naya = (char *)scheme_malloc_atomic(len + 2);
              memcpy(naya, s, len + 2);
              naya[6] = 'L';
              if (naya[8] != '\\') {
                /* Make sure REL is followed by \\, just in case the element is
                   ".." (i.e., we had \\?\RED\..). */
                memmove(naya + 9, naya + 8, len + 1 - 8);
                naya[8] = '\\';
                len++;
              }
              dir = scheme_make_sized_offset_kind_path("\\", 0, 1, 0,
                                                       SCHEME_WINDOWS_PATH_KIND);
              return MAKE_SPLIT(dir, 
                                scheme_make_sized_offset_kind_path(naya, 0, len, 0,
                                                                   SCHEME_WINDOWS_PATH_KIND), 
                                is_dir);
            }
          }
        } else {
          no_up = 1;
          if ((drive_end < len) && s[drive_end] == '\\') {
            /* Happens with \\?\c:\\, for example. */
            drive_end++;
          }
        }
      } else if (check_dos_slashslash_drive(s, 0, len, &drive_end, 0, 0)) {
        allow_double_before = 1;
        if ((drive_end < len) && IS_A_DOS_SEP(s[drive_end]))
          drive_end++;
      } else
        drive_end = 0;
    } else if ((len > 1) && is_drive_letter(s[0]) && (s[1] == ':')) {
      drive_end = 2;
      if ((drive_end < len) && IS_A_DOS_SEP(s[drive_end]))
        drive_end++;
    } else
      drive_end = 0;
  } else {
    drive_end = 0;
  }

  /* Look for confusing repeated separators (e.g. "x//y") */
  for (p = len; p--; ) {
    if (p > allow_double_before) {
      if (IS_A_SEP(kind, s[p]) && IS_A_SEP(kind, s[p - 1])) {
	/* Found it; copy without repeats */
	int q;
	char *old = s;

        if (cleaned_slashes)
          *cleaned_slashes = 1;

	s = (char *)scheme_malloc_atomic(len);
	--len;

	for (p = 0, q = 0; p < allow_double_before; p++) {
	  s[q++] = old[p];
	}

	for (; p < len; p++) {
	  if (!IS_A_SEP(kind, old[p]) || !IS_A_SEP(kind, old[p + 1]))
	    s[q++] = old[p];
	}
	s[q++] = old[len];
	len = q;
	break;
      }
    }
  }

# define IS_A_SPLIT_SEP(x) (((kind == SCHEME_WINDOWS_PATH_KIND) && no_slash_sep) ? (x == '\\') : IS_A_SEP(kind, x))

  if ((kind == SCHEME_WINDOWS_PATH_KIND) && (len <= drive_end))
    p = -1;
  else {
    for (p = len; p--; ) {
      if (IS_A_SPLIT_SEP(s[p])) {
        if (p != len - 1)
          break;
        else
          last_was_sep = 1;
      }
      if (kind == SCHEME_WINDOWS_PATH_KIND) {
	if (p < drive_end)
	  break;
      }
    }
  }
  
  if (kind == SCHEME_UNIX_PATH_KIND) {
#ifdef TILDE_IS_ABSOLUTE
    /* "./~..." can't be split at the beginning. */
    if ((p == 1)
        && s[0] == '.'
        && s[p + 1] == '~') {
      not_same = 1;
      p -= 2;
    } else
#endif
      not_same = 0;
  } else
    not_same = 0;

  if (p < 0) {
    Scheme_Object *dir;

    /* No splitting available. 
       For Unx & DOS, it was relative or exactly root.
       For Mac, it is relative or root with trailing sep. */
    if (kind == SCHEME_UNIX_PATH_KIND) {
      if (s[0] == '/')
        return MAKE_SPLIT(scheme_false, scheme_make_sized_offset_kind_path(s, 0, len, 1, kind), 1);
#ifdef TILDE_IS_ABSOLUTE
      if (s[0] == '~') {
        /* Strip ending slashes, if any. */
        while (IS_A_UNIX_SEP(s[len - 1])) {
          --len;
        }
        return MAKE_SPLIT(scheme_false, scheme_make_sized_offset_kind_path(s, 0, len, 1, kind), 1);
      }
#endif
    } else {
      if (IS_A_DOS_SEP(s[0]) || drive_end)
        return MAKE_SPLIT(scheme_false, scheme_make_sized_offset_kind_path(s, 0, len, 1, kind), 1);
    }

    dir = relative_symbol;

    /* Check for 'up: */
    if (!no_up && (s[0] == '.') && (s[1] == '.')
	&& (2 >= len || IS_A_SEP(kind, s[2]))) {
      file = up_symbol;
      is_dir = 1;
    } else if (!no_up && !not_same && (s[0] == '.') && (1 >= len || IS_A_SEP(kind, s[1]))) {
      file = same_symbol;
      is_dir = 1;
    } else {
      int delta;
      is_dir = last_was_sep;
      delta = 0;
      file = make_protected_sized_offset_path(no_up || is_dir, 
					      s, 0, len - last_was_sep + delta, 1, 0,
                                              kind);
    }
    
    return MAKE_SPLIT(dir, file, is_dir);
  }
  
  /* Check for 'up and 'same: */
  if (!no_up && (s[p + 1] == '.') && (s[p + 2] == '.')
      && (p + 3 >= len || IS_A_SEP(kind, s[p + 3]))) {
    file = up_symbol;
    is_dir = 1;
  } else if (!no_up && (s[p + 1] == '.') && (p + 2 >= len || IS_A_SEP(kind, s[p + 2]))) {
    file = same_symbol;
    is_dir = 1;
  } else {
    int protected;
    if (kind == SCHEME_WINDOWS_PATH_KIND) {
      protected = no_up || last_was_sep;
    } else  {
      protected = 1;
    }
    file = make_protected_sized_offset_path(protected,
					    s,
					    p + 1, 
					    len - p - last_was_sep - 1, 
					    1, 0, kind);
    is_dir = last_was_sep;
  }
  
  /* Check directory */
  if (p > 0) {
    Scheme_Object *ss;
    ss = make_exposed_sized_offset_path(no_up, s, 0, p + 1, 1, kind);
    return MAKE_SPLIT(ss, 
		      file, 
		      is_dir);
  }
	
  /* p = 0; this means root dir. */
  {
    Scheme_Object *ss;
    ss = scheme_make_sized_offset_kind_path(s, 0, 1, 1, kind);
    return MAKE_SPLIT(ss, file, is_dir);
  }
}

Scheme_Object *scheme_split_path(const char *path, int len, Scheme_Object **base_out, int *id_out, int kind)
{
  return do_split_path(path, len, base_out, id_out, NULL, kind);
}

#ifndef NO_FILE_SYSTEM_UTILS
static Scheme_Object *split_path(int argc, Scheme_Object **argv)
{
  char *s;
  int is_dir, len;
  Scheme_Object *three[3], *inpath;

  inpath = argv[0];

  if (!SCHEME_GENERAL_PATH_STRINGP(inpath))
    scheme_wrong_contract("split-path", "(or/c path-for-some-system? path-string?)", 0, argc, argv);

  inpath = TO_PATH(inpath);

  s = SCHEME_PATH_VAL(inpath);
  len = SCHEME_PATH_LEN(inpath);

  if (!len) {
    scheme_contract_error("split-path",
                          "path is an empty string",
                          NULL);
  }

  if (has_null(s, len))
    raise_null_error("split-path", inpath, "");

  three[1] = scheme_split_path(s, len, &three[0], &is_dir, SCHEME_PATH_KIND(inpath));

  three[2] = is_dir ? scheme_true : scheme_false;

  return scheme_values(3, three);
}
#endif

int scheme_is_relative_path(const char *s, intptr_t len, int kind)
{
  if (!len)
    return 0;

  if (kind == SCHEME_UNIX_PATH_KIND) {
    return !((s[0] == '/') || WHEN_TILDE_IS_ABSOLUTE(s[0] == '~'));
  } else {
    int dlen;
    if (check_dos_slashslash_qm(s, len, &dlen, NULL, NULL)
	&& (dlen < 0)) {
      if (dlen == -1)
        return 1; /* It's a \\?\REL\ path */
      else
        return 0; /* It's a \\?\RED\ path */
    }

    if (IS_A_DOS_SEP(s[0])
        || ((len >= 2) 
            && is_drive_letter(s[0])
            && (s[1] == ':')))
      return 0;
    else
      return 1;
  }
}

int scheme_is_complete_path(const char *s, intptr_t len, int kind)
{
  if (!len)
    return 0;

  if (!kind)
    kind = SCHEME_PLATFORM_PATH_KIND;

  if (!scheme_is_relative_path(s, len, kind)) {
    if (kind == SCHEME_WINDOWS_PATH_KIND) {
      if (IS_A_DOS_SEP(s[0]) && IS_A_DOS_SEP(s[1])) {
        int dlen;
        if (check_dos_slashslash_qm(s, len, &dlen, NULL, NULL)) { /* not relative */
          return (dlen >= 0);
        } else if (check_dos_slashslash_drive(s, 0, len, NULL, 0, 0))
          return 1;
        else
          return 0;
      } else if ((len >= 2) 
                 && is_drive_letter(s[0])
                 && (s[1] == ':')) {
        return 1;
      } else
        return 0;
    } else
      return 1;
  } else 
    return 0;
}

static char *do_path_to_complete_path(char *filename, intptr_t ilen, const char *wrt, intptr_t wlen, int kind)
{
  if (!scheme_is_complete_path(filename, ilen, kind)) {
    char *naya;
    int skip_sep = 0;

    if (!wrt) {
      Scheme_Object *wd;
      if (scheme_current_thread) {
        wd = CURRENT_WD();
        wrt = SCHEME_PATH_VAL(wd);
        wlen = SCHEME_PATH_LEN(wd);
        scheme_security_check_file("path->complete-path", NULL, SCHEME_GUARD_FILE_EXISTS);
      } else {
        int actlen;
        wrt = scheme_os_getcwd(NULL, 0, &actlen, 1);
        wlen = actlen - 1;
      }
    }

    if (kind == SCHEME_WINDOWS_PATH_KIND) {
      if (!scheme_is_relative_path(filename, ilen, kind)) {
        /* Absolute, not complete. Fill in the disk */
        wrt = get_drive_part(wrt, wlen);
        wlen = strlen(wrt);
        /* drop trailing separator */
        if (IS_A_DOS_SEP(wrt[wlen - 1]) 
            && !check_dos_slashslash_qm(wrt, wlen, NULL, NULL, NULL)) {
          wlen--;
        }
        skip_sep = 1;
      }

      if (check_dos_slashslash_qm(wrt, wlen, NULL, NULL, NULL) /* wrt is never relative */
          || check_dos_slashslash_qm(filename, ilen, NULL, NULL, NULL)) { /* filename might be \\?\REL\ */
        /* For \\?\, give up on fast path and use build-path */
        Scheme_Object *a[2], *p;
        p = scheme_make_sized_offset_kind_path((char *)wrt, 0, wlen, 1, SCHEME_WINDOWS_PATH_KIND);
        a[0] = p;
        p = scheme_make_sized_offset_kind_path(filename, 0, ilen, 1, SCHEME_WINDOWS_PATH_KIND);
        a[1] = p;
        p = do_build_path(2, a, 0, 0, SCHEME_WINDOWS_PATH_KIND);
        return SCHEME_PATH_VAL(p);
      }
    }

    naya = (char *)scheme_malloc_atomic(ilen + wlen + 2);
    memcpy(naya, wrt, wlen);
    if (!skip_sep)
      if (!IS_A_SEP(kind, naya[wlen - 1]))
	naya[wlen++] = FN_SEP(kind);
    if (kind == SCHEME_WINDOWS_PATH_KIND) {
      int w = wlen;
      strip_trailing_spaces(naya, &w, 0, 1);
      wlen = w;
    }
    memcpy(naya + wlen, filename, ilen);
    naya[wlen + ilen] = 0;

    return naya;
  }

  return filename;
}

static Scheme_Object *path_to_complete_path(int argc, Scheme_Object **argv)
{
  Scheme_Object *p, *wrt;
  char *s;
  int len, kind;

  p = argv[0];
  if (!SCHEME_GENERAL_PATH_STRINGP(p))
    scheme_wrong_contract("path->complete-path", "(or/c path-for-some-system? path-string?)", 0, argc, argv);
  p = TO_PATH(p);
  if (argc > 1) {
    wrt = argv[1];
    if (!SCHEME_GENERAL_PATH_STRINGP(wrt))
      scheme_wrong_contract("path->complete-path", "(or/c path-for-some-system? path-string?)", 1, argc, argv);
    wrt = TO_PATH(wrt);
  } else
    wrt = NULL;

  kind = SCHEME_PATH_KIND(p);
  if (wrt) {
    if (SCHEME_PATH_KIND(wrt) != kind) {
      scheme_contract_error("path->complete-path",
                            "convention of first path incompatible with convention of second path",
                            "first path", 1, argv[0],
                            "second path", 1, argv[1],
                            NULL);
    }
  } else if (kind != SCHEME_PLATFORM_PATH_KIND) {
    scheme_contract_error("path->complete-path",
                          "no second path supplied, and given path is not for the current platform",
                          "given path", 1, argv[0],
                          NULL);
  }

  s = SCHEME_PATH_VAL(p);
  len = SCHEME_PATH_LEN(p);

  if (has_null(s, len))
    raise_null_error("path->complete-path", p, "");

  if (wrt) {
    char *ws;
    int wlen;

    ws = SCHEME_PATH_VAL(wrt);
    wlen = SCHEME_PATH_LEN(wrt);
    
    if (has_null(ws, wlen))
      raise_null_error("path->complete-path", p, "");

    if (!scheme_is_complete_path(ws, wlen, kind))
      scheme_contract_error("path->complete-path", "second argument is not a complete path",
                            "first argument", 1, p,
                            "second argument", 1, wrt,
                            NULL);
    
    if (!scheme_is_complete_path(s, len, kind)) {
      s = do_path_to_complete_path(s, len, ws, wlen, kind);
      return scheme_make_sized_offset_kind_path(s, 0, strlen(s), 0, kind);
    }
  } else if (!scheme_is_complete_path(s, len, kind)) {
    s = do_path_to_complete_path(s, len, NULL, 0, kind);

    return scheme_make_sized_offset_kind_path(s, 0, strlen(s), 0, kind);
  }
   
  return p;
}

Scheme_Object *scheme_path_to_complete_path(Scheme_Object *path, Scheme_Object *relto_path)
{
  Scheme_Object *a[2];
  a[0] = path;
  a[1] = relto_path;
  return path_to_complete_path(relto_path ? 2 : 1, a);
}

#ifndef NO_FILE_SYSTEM_UTILS

static char *filename_for_error(Scheme_Object *p)
{
  return do_expand_filename(p, NULL, 0,
			    NULL,
			    NULL,
			    1, 1,
			    0, SCHEME_PLATFORM_PATH_KIND,
                            0);
}

static Scheme_Object *delete_file(int argc, Scheme_Object **argv)
{
  int errid;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("delete-file", "path-string?", 0, argc, argv);

  while (1) {
    if (!MSC_W_IZE(unlink)(MSC_WIDE_PATH(scheme_expand_string_filename(argv[0],
								       "delete-file",
								       NULL,
								       SCHEME_GUARD_FILE_DELETE))))
      return scheme_void;
    else if (errno != EINTR)
      break;
  }
  errid = errno;
  
  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM, 
		   "delete-file: cannot delete file\n"
                   "  path: %q\n"
                   "  system error: %e",
		   filename_for_error(argv[0]),
		   errid);

  return NULL;
}

static Scheme_Object *rename_file(int argc, Scheme_Object **argv)
{
  int exists_ok = 0;
  char *src, *dest;
  Scheme_Object *bss, *bsd;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("rename-file-or-directory", "path-string?", 0, argc, argv);
  if (!SCHEME_PATH_STRINGP(argv[1]))
    scheme_wrong_contract("rename-file-or-directory", "path-string?", 1, argc, argv);
  if (argc > 2)
    exists_ok = SCHEME_TRUEP(argv[2]);

  bss = argv[0];
  bsd = argv[1];

  src = scheme_expand_string_filename(bss,
				      "rename-file-or-directory",
				      NULL,
				      SCHEME_GUARD_FILE_READ);
  dest = scheme_expand_string_filename(bsd,
				       "rename-file-or-directory",
				       NULL,
				       SCHEME_GUARD_FILE_WRITE);

# ifdef DOS_FILE_SYSTEM
  if (MoveFileExW(WIDE_PATH_COPY(src), WIDE_PATH(dest), (exists_ok ? MOVEFILE_REPLACE_EXISTING : 0)))
    return scheme_void;
  
  {
    int errid;
    errid = GetLastError();
    errno = errid;
  }

  if (errno == ERROR_CALL_NOT_IMPLEMENTED) {
    /* Then we have the great misfortune of running in Windows 9x. If
       exists_ok, then do something no less stupid than the OS
       itself: */
    int errid;
    if (exists_ok)
      MSC_W_IZE(unlink)(MSC_WIDE_PATH(dest));
    if (MoveFileW(WIDE_PATH_COPY(src), WIDE_PATH(dest)))
      return scheme_void;
    errid = GetLastError();
    errno = errid;
  } else if (errno == ERROR_ALREADY_EXISTS) {
    exists_ok = -1;
  }

# define MOVE_ERRNO_FORMAT "%E"
# else
  if (!exists_ok && (scheme_file_exists(dest) || scheme_directory_exists(dest))) {
    exists_ok = -1;
    errno = EEXIST;
    goto failed;
  }
  
  while (1) {
    if (!rename(src, dest))
      return scheme_void;
    else if (errno != EINTR)
      break;
  }
# define MOVE_ERRNO_FORMAT "%e"
# endif

#ifndef DOS_FILE_SYSTEM
failed:
#endif
  scheme_raise_exn((exists_ok < 0) ? MZEXN_FAIL_FILESYSTEM_EXISTS : MZEXN_FAIL_FILESYSTEM, 
		   "rename-file-or-directory: cannot rename file or directory\n"
                   "  source path: %q\n"
                   "  dest path: %q\n"
                   "  system error: " MOVE_ERRNO_FORMAT,
		   filename_for_error(argv[0]),
		   filename_for_error(argv[1]),
		   errno);
  
  return NULL;
}

static Scheme_Object *copy_file(int argc, Scheme_Object **argv)
{
  char *reason = NULL;
#ifdef DOS_FILE_SYSTEM
  char *src, *dest;
#endif 
  int pre_exists = 0, has_err_val = 0, err_val = 0, exists_ok = 0;
  Scheme_Object *bss, *bsd;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("copy-file", "path-string?", 0, argc, argv);
  if (!SCHEME_PATH_STRINGP(argv[1]))
    scheme_wrong_contract("copy-file", "path-string?", 1, argc, argv);

  bss = argv[0];
  bsd = argv[1];
  exists_ok = ((argc > 2) && SCHEME_TRUEP(argv[2]));

#ifdef DOS_FILE_SYSTEM
  src = 
#endif 
    scheme_expand_string_filename(bss,
                                  "copy-file",
                                  NULL,
                                  SCHEME_GUARD_FILE_READ);

#ifdef DOS_FILE_SYSTEM
  dest = 
#endif 
    scheme_expand_string_filename(bsd,
                                  "copy-file",
                                  NULL, 
                                  SCHEME_GUARD_FILE_WRITE | SCHEME_GUARD_FILE_DELETE);

#ifdef UNIX_FILE_SYSTEM
  {
# define COPY_BUFFER_SIZE 2048
    char b[COPY_BUFFER_SIZE];
    intptr_t len;
    int ok;
    struct stat buf;
    mz_jmp_buf newbuf, * volatile savebuf;
    int a_cnt;
    Scheme_Object *a[2], * volatile in, * volatile out;

    reason = NULL;
    in = scheme_do_open_input_file("copy-file", 0, 1, argv, 1, &reason, &err_val);
    if (!in) {
      has_err_val = !!err_val;
      goto failed;
    }

    do {
      ok = fstat(scheme_get_port_fd(in), &buf);
    } while ((ok == -1) && (errno == EINTR));
    if (ok || S_ISDIR(buf.st_mode)) {
      reason = "error getting mode";
      err_val = errno;
      has_err_val = 1;
      goto failed;
    }

    a[0] = argv[1];
    if (exists_ok) {
      a_cnt = 2;
      a[1] = scheme_intern_symbol("truncate");
    } else
      a_cnt = 1;
    out = scheme_do_open_output_file("copy-file", 0, a_cnt, a, 0, 1, &reason, &err_val);
    if (!out) {
      scheme_close_input_port(in);
      has_err_val = !!err_val;
      pre_exists = (err_val == EEXIST);
      goto failed;
    }

    /* catch errors or breaks during read and write to close ports: */
    savebuf = scheme_current_thread->error_buf;
    scheme_current_thread->error_buf = &newbuf;
    if (scheme_setjmp(newbuf)) {
      scheme_close_input_port(in);
      scheme_close_output_port(out);
      scheme_current_thread->error_buf = savebuf;
      scheme_longjmp(*savebuf, 1);
      return NULL;
    } else {
      ok = 1;
      while ((len = scheme_get_byte_string("copy-file", in, b, 0, COPY_BUFFER_SIZE, 0, 0, NULL))) {
        if (len == -1)
          break;
        if (scheme_put_byte_string("copy-file", out, b, 0, len, 0) != len) {
          ok = 0;
          break;
        }
      }
    }
    scheme_current_thread->error_buf = savebuf;

    if (ok) {
      do {
        err_val = fchmod(scheme_get_port_fd(out), buf.st_mode);
      } while ((err_val == -1) && (errno != EINTR));
      if (err_val) {
        err_val = errno;
        has_err_val = 0;
        reason = "cannot set destination's mode";
        ok = 0;
      }
    } else
      reason = "read or write failed";

    scheme_close_input_port(in);
    scheme_close_output_port(out);

    if (ok)
      return scheme_void;
  }
 failed:
#endif
#ifdef DOS_FILE_SYSTEM
  if (CopyFileW(WIDE_PATH_COPY(src), WIDE_PATH(dest), !exists_ok))
    return scheme_void;
  
  reason = "copy failed";
  err_val = GetLastError();
  if ((err_val == ERROR_FILE_EXISTS)
      || (err_val == ERROR_ALREADY_EXISTS))
    pre_exists = 1;
  has_err_val = 1;
#endif

  scheme_raise_exn(pre_exists ? MZEXN_FAIL_FILESYSTEM_EXISTS : MZEXN_FAIL_FILESYSTEM, 
		   "copy-file: %s\n"
                   "  source path: %q\n"
                   "  destination path: %q"
                   "%s%M",
		   reason,
		   filename_for_error(argv[0]),
		   filename_for_error(argv[1]),
		   has_err_val ? "\n  system error: " : "",
		   has_err_val,
		   err_val);

  return NULL;
}

static Scheme_Object *relative_path_p(int argc, Scheme_Object **argv)
{
  char *s;
  int len;
  Scheme_Object *bs;

  if (!SCHEME_GENERAL_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("relative-path?", "(or/c path-for-some-system? path-string?)", 0, argc, argv);

  bs = TO_PATH(argv[0]);

  s = SCHEME_PATH_VAL(bs);
  len = SCHEME_PATH_LEN(bs);

  if (has_null(s, len))
    return scheme_false;

  return (scheme_is_relative_path(s, len, SCHEME_PATH_KIND(bs))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *complete_path_p(int argc, Scheme_Object **argv)
{
  char *s;
  int len;
  Scheme_Object *bs;

  if (!SCHEME_GENERAL_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("complete-path?", "(or/c path-for-some-system? path-string?)", 0, argc, argv);

  bs = TO_PATH(argv[0]);

  s = SCHEME_PATH_VAL(bs);
  len = SCHEME_PATH_LEN(bs);

  if (has_null(s, len))
    return scheme_false;

  return (scheme_is_complete_path(s, len, SCHEME_PATH_KIND(bs))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *absolute_path_p(int argc, Scheme_Object **argv)
{
  char *s;
  int len;
  Scheme_Object *bs;

  if (!SCHEME_GENERAL_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("absolute-path?", "(or/c path-for-some-system? path-string?)", 0, argc, argv);

  bs = TO_PATH(argv[0]);

  s = SCHEME_PATH_VAL(bs);
  len = SCHEME_PATH_LEN(bs);

  if (has_null(s, len))
    return scheme_false;

  return (!scheme_is_relative_path(s, len, SCHEME_PATH_KIND(bs))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *resolve_path(int argc, Scheme_Object *argv[])
{
#ifndef NO_READLINK
#define SL_NAME_MAX 2048
  char buffer[SL_NAME_MAX];
#endif
#ifndef NO_READLINK
  intptr_t len;
  int copied = 0;
#endif
  char *filename;
  int expanded;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("resolve-path", "path-string?", 0, argc, argv);

  filename = do_expand_filename(argv[0],
				NULL,
				0,
				"resolve-path",
				&expanded,
				1, 0,
				SCHEME_GUARD_FILE_EXISTS,
                                SCHEME_PLATFORM_PATH_KIND,
                                0);

#ifndef NO_READLINK
  {
    char *fullfilename = filename;

    len = strlen(fullfilename);
    if (!scheme_is_complete_path(fullfilename, len, SCHEME_PLATFORM_PATH_KIND)) {
      fullfilename = do_path_to_complete_path(fullfilename, len, NULL, 0, SCHEME_PLATFORM_PATH_KIND);
      copied = 1;
    }

    /* Make sure path doesn't have trailing separator: */
    len = strlen(fullfilename);
    while (len && IS_A_SEP(SCHEME_PLATFORM_PATH_KIND, fullfilename[len - 1])) {
      if (!expanded && !copied) {
	fullfilename = scheme_strdup(fullfilename);
	copied = 1;
      }
      fullfilename[--len] = 0;
    }

    while (1) {
      len = readlink(fullfilename, buffer, SL_NAME_MAX);
      if (len == -1) {
	if (errno != EINTR)
	  break;
      } else
	break;
    }

    if (len > 0)
      return scheme_make_sized_path(buffer, len, 1);
  }
#endif

  if (!expanded)
    return argv[0];
  else
    return scheme_make_sized_path(filename, strlen(filename), 1);
}

static Scheme_Object *convert_literal_relative(Scheme_Object *file)
{
  int ln;
  char *f;
  f = SCHEME_PATH_VAL(file);
  ln = SCHEME_PATH_LEN(file);
  if ((ln == 11) && !strcmp(f, "\\\\?\\REL\\\\.."))
    return up_symbol;
  else if ((ln == 10) && !strcmp(f, "\\\\?\\REL\\\\."))
    return same_symbol;
  return file;
}

static Scheme_Object *simplify_qm_path(Scheme_Object *path)
{
  /* path is already expanded, so the only remaining
     clean-ups are dropping a trailing separator,
     and getting rid of \\?\ if it's not actually needed. */
  char *s = SCHEME_PATH_VAL(path);
  int drive_end, clean_start, len = SCHEME_PATH_LEN(path), fixed = 0, i;
  int drop_extra_slash = -1, set_slash = -1, element_start;
  int found_bad = 0, start_special_check = 0, is_dir = 0, norm_unc = 0, drop_ss_slash = 0;

  if ((s[len - 1] == '\\')
      && (s[len - 2] != '\\')
      && do_path_to_directory_path(s, 0, len - 1, scheme_true, 1, SCHEME_WINDOWS_PATH_KIND)) {
    --len;
    fixed = 1;
  }

  check_dos_slashslash_qm(s, len, &drive_end, &clean_start, NULL);
  if ((drive_end == 7)
      && is_drive_letter(s[4])
      && (s[5] == ':')) {
    /* Maybe don't need \\?\ for \\?\C:\... */
    start_special_check = 7;
    drive_end = 4;
  } else if ((drive_end == 8)
	     && (s[4] == '\\')
	     && is_drive_letter(s[5])
	     && (s[6] == ':')) {
    /* Maybe don't need \\?\\ for \\?\\C:\... */
    start_special_check = 8;
    drive_end = 5;
    drop_ss_slash = 1;
  } else if (drive_end == -2) {
    /* \\?\RED\ */
    int lit_start;
    get_slashslash_qm_dot_ups_end(s, len, &lit_start);
    start_special_check = lit_start;
    drive_end = lit_start - 1;
  } else if (drive_end < 0) {
    int lit_start, dots_end;
    dots_end = get_slashslash_qm_dot_ups_end(s, len, &lit_start);
    if (lit_start == len) {
      /* just keep the dots */
      return scheme_path_to_directory_path(scheme_make_sized_offset_kind_path(s, 8, dots_end - 8, 1, SCHEME_WINDOWS_PATH_KIND));
    }
    start_special_check = lit_start;
    if (dots_end < 9)
      drive_end = lit_start; /* no dots, so just keep the literal part */
    else {
      drive_end = 8; /* \\?\REL\..\, and we keep the .. */
      drop_extra_slash = dots_end;
      is_dir = 1;
    }
  } else if ((clean_start == 7) 
	     && ((s[4] == 'U') || (s[4] == 'u'))
	     && ((s[5] == 'N') || (s[5] == 'n'))
	     && ((s[6] == 'C') || (s[6] == 'c'))) {
    if (drive_end == len) {
      is_dir = 1;
    }
    drive_end = 6;
    start_special_check = 7; /* \\?\UNC */
    set_slash = 6;
    norm_unc = 1;
  } else if ((clean_start == 8) 
	     && (s[4] == '\\')
	     && ((s[5] == 'U') || (s[5] == 'u'))
	     && ((s[6] == 'N') || (s[6] == 'n'))
	     && ((s[7] == 'C') || (s[7] == 'c'))) {
    if (drive_end == len) {
      is_dir = 1;
    }
    drive_end = 7;
    start_special_check = 8; /* \\?\\UNC */
    set_slash = 7;
    norm_unc = 1;
    drop_ss_slash = 1;
  } else {
    /* We have a weird root. Give up. */
    found_bad = 1;
    start_special_check = len;
  }

  if (!found_bad) {
    element_start = start_special_check;
    for (i = element_start; 1; i++) {
      if ((i == len) || (s[i] == '\\')) {
	if (element_start <= i - 1) {
	  /* Need the protection? */
	  Scheme_Object *v;
	  int any_more = 0, j;
	
	  for (j = i+1; j < len; j++) {
	    if (s[j] != '\\') {
	      any_more = 1;
	      break;
	    }
	  }
	
	  v = make_protected_sized_offset_path(1, 
					       s, element_start, i - element_start,
					       1, 
					       (any_more ? 2 : 1),
                                               SCHEME_WINDOWS_PATH_KIND);
	  if (SCHEME_TRUEP(v)) {
	    found_bad = 1;
	    break;
	  }
	}
	if (i == len)
	  break;
	element_start = i + 1;
      }
    }
  }

  if (found_bad) {
    if (norm_unc) {
      if ((s[4 + drop_ss_slash] == 'U')
	  && (s[5 + drop_ss_slash] == 'N')
	  && (s[6 + drop_ss_slash] == 'C'))
	norm_unc = 0;
    }
    if (norm_unc || drop_ss_slash) {
      if (!fixed) {
	char *naya;
	naya = (char *)scheme_malloc_atomic(len);
	memcpy(naya, s, len);
	s = naya;
	fixed = 1;
      }
      if (drop_ss_slash) {
	memmove(s + 3, s + 4, len - 4);
	len--;
      }
      if (norm_unc) {
	s[4] = 'U';
	s[5] = 'N';
	s[6] = 'C';
      }
    }
    if (fixed)
      path = scheme_make_sized_offset_kind_path(s, 0, len, 1, SCHEME_WINDOWS_PATH_KIND);
    return path;
  } else {
    if (drop_extra_slash > -1) {
      char *naya;
      naya = (char *)scheme_malloc_atomic(len);
      memcpy(naya, s, drop_extra_slash);
      memcpy(naya + drop_extra_slash, s + drop_extra_slash + 1, len - drop_extra_slash - 1);
      s = naya;
      --len;
    }
    if (set_slash > -1) {
      char *naya;
      naya = (char *)scheme_malloc_atomic(len);
      memcpy(naya, s, len);
      naya[set_slash] = '\\';
      s = naya;
    }
    path = scheme_make_sized_offset_kind_path(s, drive_end, len - drive_end, 1, SCHEME_WINDOWS_PATH_KIND);
    if (is_dir)
      path = scheme_path_to_directory_path(path);
    return path;
  }
}

static Scheme_Object *do_simplify_path(Scheme_Object *path, Scheme_Object *cycle_check, int skip, 
				       int use_filesystem, 
                                       int force_rel_up,
                                       int kind)
     /* When !use_filesystem, the result can be #f for an empty relative
	path, and it can contain leading ".."s, or ".."s after an initial
        "~" path with "~" paths are absolute.
	When force_rel_up under Windows, "\\?\REL\.." from split-path is
	treated like 'up. */
{
  int isdir, cleaned_slashes = 0, must_be_dir = 0, last_was_dir = 0, did_first = 0;
  Scheme_Object *file = scheme_false, *base;

  /* cleanse-path doesn't touch the filesystem. Always start with
     that, to get things basically tidy. */
  if (kind == SCHEME_WINDOWS_PATH_KIND) {
    char *s;
    int expanded, add_sep = 0;
    s = do_expand_filename(path, SCHEME_PATH_VAL(path), SCHEME_PATH_LEN(path),
                           NULL, &expanded, 0, 0, 0, kind, 0);
    {
      int slen;
      if (expanded)
        slen = strlen(s);
      else
        slen = SCHEME_PATH_LEN(path);
      s = do_normal_path_seps(s, &slen, 0, 0, SCHEME_WINDOWS_PATH_KIND, &expanded);
    }
    if (expanded) {
      path = scheme_make_sized_offset_kind_path(s, 0, -1, 0, SCHEME_WINDOWS_PATH_KIND);
    }
    if (!check_dos_slashslash_qm(SCHEME_PATH_VAL(path), SCHEME_PATH_LEN(path), NULL, NULL, &add_sep)) {
      int len = SCHEME_PATH_LEN(path);
      s = strip_trailing_spaces(SCHEME_PATH_VAL(path), &len, 0, 0);
      if (s != SCHEME_PATH_VAL(path))
        path = scheme_make_sized_offset_kind_path(s, 0, -1, 0, SCHEME_WINDOWS_PATH_KIND);
    } else if (add_sep) {
      int len = SCHEME_PATH_LEN(path);
      if ((add_sep < len) && (s[add_sep] != '\\')) {
        /* Add two \, as in \\?\c -> \\?\\\c */
        char *naya;
        naya = (char *)scheme_malloc_atomic(len + 3);
        memcpy(naya, s, add_sep);
        naya[add_sep] = '\\';
        naya[add_sep+1] = '\\';
        memcpy(naya + add_sep + 2, s + add_sep, len + 1 - add_sep);
        len += 2;
        path = scheme_make_sized_offset_kind_path(naya, 0, len, 0, SCHEME_WINDOWS_PATH_KIND);
      } else if (((add_sep + 1) < len) && (s[add_sep] == '\\') && (s[add_sep+1] != '\\')) {
        /* Add \, as in \\?\\c -> \\?\\\c */
        char *naya;
        naya = (char *)scheme_malloc_atomic(len + 2);
        memcpy(naya, s, add_sep);
        naya[add_sep] = '\\';
        memcpy(naya + add_sep + 1, s + add_sep, len + 1 - add_sep);
        len++;
        path = scheme_make_sized_offset_kind_path(naya, 0, len, 0, SCHEME_WINDOWS_PATH_KIND);
      }
    }
  }

  /* Fast check; avoids split operations, if possible.
     Also responsible for determining whether there's a
     redundant or missing trailing slash in the case that
     the path is just a root. */
  {
    char *s;
    int len, i, saw_dot = 0;
    s = SCHEME_PATH_VAL(path);
    len = SCHEME_PATH_LEN(path);

    if (kind == SCHEME_WINDOWS_PATH_KIND) {
      if (!skip && check_dos_slashslash_qm(s, len, NULL, NULL, NULL)) {
        if (!force_rel_up) {
	  int drive_end;
          path = simplify_qm_path(path);
	  len = SCHEME_PATH_LEN(path);
	  if (check_dos_slashslash_qm(SCHEME_PATH_VAL(path), len, &drive_end, NULL, NULL)) {
	    /* If it's a drive... */
	    if (drive_end == len) {
	      /* Make it a directory path. */
	      path = scheme_path_to_directory_path(path);
	    }
	  }
	  return path;
        } else {
	  /* force_rel_up means that we want a directory: */
          return scheme_path_to_directory_path(path);
	}
      }
      if (!skip && check_dos_slashslash_drive(s, 0, len, NULL, 1, 0)) {
        /* A UNC drive (with no further elements). 
	   Remove extra trailing slashes, if any... */
        for (i = len; IS_A_DOS_SEP(s[i-1]); i--) { }
        if (i < len - 1) {
          path = scheme_make_sized_offset_kind_path(s, 0, i, 1, SCHEME_WINDOWS_PATH_KIND);
        }
	/* ... but make it a directory path. */
        path = scheme_path_to_directory_path(path);
      }

      if (skip) {
        while (s[skip] == '\\') {
          skip++;
        }
      }
    }

    i = skip;
    if (kind == SCHEME_WINDOWS_PATH_KIND) {
      if (!i && (len >= 2) && is_drive_letter(s[0]) && s[1] == ':') {
        i = 2;
      } else if (!i) {
        int drive_end;
        if (check_dos_slashslash_drive(s, 0, len, &drive_end, 0, 0)) {
          i = drive_end;
        }
      }
    }

    for (; i < len; i++) {
      if (s[i] == '.')
	saw_dot++;
      else if (IS_A_SEP(kind, s[i])) {
	if ((saw_dot == 1) || (saw_dot == 2))
	  break;
        if ((i + 1 < len) && (IS_A_SEP(kind, s[i]))) {
          /* Double slash to clean up... */
          break;
        }
	saw_dot = 0;
      } else
	saw_dot = 3;
    }

    if (i == len) {
      if ((saw_dot != 1) && (saw_dot != 2)) {
        /* Still may need to add trailing separator if it's syntactically a directory. */
        if (path_is_simple_dir_without_sep(path))
          path = scheme_path_to_directory_path(path);
        return path;
      }
    }
    /* There's a ., .., or // in the path... */
  }

  /* Check whether it can be simplified: */
  if (!cleaned_slashes) {
    base = path;
    do {
      char *s;
      int len;
      s = SCHEME_PATH_VAL(base);
      len = SCHEME_PATH_LEN(base);
      if (len <= skip)
        break;
      file = do_split_path(s, len, &base, &isdir, &cleaned_slashes, kind);
      if (kind == SCHEME_WINDOWS_PATH_KIND) {
        if (force_rel_up) {
          file = convert_literal_relative(file);
        }
      }
      if (SCHEME_SYMBOLP(file) || cleaned_slashes)
        break;
    } while (SCHEME_GENERAL_PATHP(base));
  } else
    file = scheme_false;

  if (SCHEME_SYMBOLP(file) || cleaned_slashes) {
    /* It can be simplified: */
    char *s;
    int len;
    Scheme_Object *accum = scheme_null, *result;

    s = SCHEME_PATH_VAL(path);
    len = SCHEME_PATH_LEN(path);

    if (use_filesystem
	&& !scheme_is_complete_path(s, len, kind)) {
      /* Make it absolute */
      s = scheme_expand_string_filename(path,
					"simplify-path", NULL,
					SCHEME_GUARD_FILE_EXISTS);
      len = strlen(s);
    }

    /* Check for cycles: */
    if (use_filesystem) {
      {
	Scheme_Object *l = cycle_check;
	while (!SCHEME_NULLP(l)) {
	  Scheme_Object *p = SCHEME_CAR(l);
	  if ((len == SCHEME_PATH_LEN(p))
	      && !strcmp(s, SCHEME_PATH_VAL(p))) {
	    /* Cycle of links detected */
	    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			     "simplify-path: cycle detected at link\n"
                             "  link path: %q",
			     s);
	  }
	  l = SCHEME_CDR(l);
	}
      }
    
      cycle_check = scheme_make_pair(scheme_make_sized_path(s, len, 0), 
				     cycle_check);
    }

    /* Split the path into a list. */
    while (1) {
      if (len <= skip) {
	accum = scheme_make_pair(scheme_make_sized_offset_kind_path(s, 0, len, 0, kind), accum);
	break;
      }

      file = scheme_split_path(s, len, &base, &isdir, kind);
      if (kind == SCHEME_WINDOWS_PATH_KIND) {
        if (force_rel_up) {
          file = convert_literal_relative(file);
          if (SCHEME_SYMBOLP(file))
            isdir = 1;
        }
      }

      if (!did_first) {
        must_be_dir = isdir;
        did_first = 1;
      }

      if (SAME_OBJ(file, same_symbol)) {
	/* Drop it */
      } else
	accum = scheme_make_pair(file, accum);
      
      if (SCHEME_GENERAL_PATHP(base)) {
	s = SCHEME_PATH_VAL(base);
	len = SCHEME_PATH_LEN(base);
      } else {
	if (use_filesystem) {
	  accum = scheme_make_pair(file, SCHEME_CDR(accum));
	}
	break;
      }
    }

    /* Now assemble the result */
    if (SCHEME_NULLP(accum)) {
      /* Only happens when !use_filesystem */
      result = scheme_false;
    } else {
      result = SCHEME_CAR(accum);
      if (SAME_OBJ(result, up_symbol)) {
	/* Only happens when !use_filesystem */
	result = scheme_false;
      } else
	accum = SCHEME_CDR(accum);
    }

    /* Build up path, watching for links just before a ..: */
    while (!SCHEME_NULLP(accum)) {
      if (SAME_OBJ(SCHEME_CAR(accum), up_symbol)) {
	if (use_filesystem) {
	  /* Look for symlink in result-so-far. */
	  Scheme_Object *new_result, *a[1];

	  while (1) {
	    a[0] = result;
	    new_result = resolve_path(1, a);
	
	    /* Was it a link? */
	    if (result != new_result) {
	      /* It was a link. Is the new result relative? */
	      if (!scheme_is_complete_path(SCHEME_PATH_VAL(new_result),
					   SCHEME_PATH_LEN(new_result),
                                           kind)) {
		Scheme_Object *aa[2], *result_base;
		/* Yes - resolve it relative to result's base: */
		scheme_split_path(SCHEME_PATH_VAL(result),
				  SCHEME_PATH_LEN(result),
				  &result_base,
				  &isdir,
                                  kind);
		aa[0] = result_base;
		aa[1] = new_result;
		new_result = do_build_path(2, aa, 0, 0, SCHEME_PLATFORM_PATH_KIND);
	      }
	    
	      /* Simplify the new result */
	      result = do_simplify_path(new_result, cycle_check, skip, 
					use_filesystem, force_rel_up, kind);
	      cycle_check = scheme_make_pair(new_result, cycle_check);
	    } else
	      break;
	  }
	}
	
	/* Do one 'up: */
	{
	  accum = SCHEME_CDR(accum);
	  if (SCHEME_FALSEP(result)) {
	    /* Empty relative path so far */
	    if (skip) /* => input was a \\?\ path, and it must be relative */
	      result = scheme_make_sized_offset_kind_path("\\\\?\\REL\\..", 0, 10, 0, SCHEME_WINDOWS_PATH_KIND);
	    else
	      result = scheme_make_sized_offset_kind_path("..", 0, 2, 0, kind);
	  } else {
	    Scheme_Object *next, *to_go;
	    to_go = scheme_split_path(SCHEME_PATH_VAL(result),
				      SCHEME_PATH_LEN(result),
				      &next,
				      &isdir,
                                      kind);
	    if (SAME_OBJ(to_go, up_symbol)) {
	      /* We're building a sequence of ups... */
	      Scheme_Object *a[2];
	      a[0] = result;
	      a[1] = up_symbol;
	      result = do_build_path(2, a, 0, 1, kind);
#ifdef TILDE_IS_ABSOLUTE
	    } else if ((kind == SCHEME_UNIX_PATH_KIND)
                       && SCHEME_FALSEP(next)
                       && SCHEME_GENERAL_PATHP(to_go)
                       && SCHEME_PATH_VAL(to_go)[0] == '~') {
	      /* Can't delete a leading ~ for .. */
	      Scheme_Object *a[2];
	      a[0] = result;
	      a[1] = up_symbol;
	      result = do_build_path(2, a, 0, 1, kind);
#endif
	    } else if (!SCHEME_GENERAL_PATH_STRINGP(next)) {
	      if (SCHEME_FALSEP(next)) {
		/* Result is already a root, so we just drop the .. */
	      } else {
		/* Result is empty relative path */
		result = scheme_false;
	      }
	    } else
	      result = next;
	  }
	}

        last_was_dir = 1;
      } else {
	/* Add path element onto the result: */
	if (SCHEME_FALSEP(result))
	  result = SCHEME_CAR(accum);
	else {
	  Scheme_Object *a[2];
	  a[0] = result;
	  a[1] = SCHEME_CAR(accum);
	  result = do_build_path(2, a, 0, 0, kind);
	}
	accum = SCHEME_CDR(accum);
        last_was_dir = 0;
      }
    }

    if ((must_be_dir || last_was_dir) && !SCHEME_FALSEP(result)) {
      result = scheme_path_to_directory_path(result);
    }

    return result;
  } else
    return path;
}

Scheme_Object *scheme_simplify_path(int argc, Scheme_Object *argv[])
{
  char *s;
  int len, use_fs, kind;
  Scheme_Object *bs, *r;

  if (!SCHEME_GENERAL_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("simplify-path", "(or/c path-for-some-system? path-string?)", 0, argc, argv);

  bs = TO_PATH(argv[0]);

  s = SCHEME_PATH_VAL(bs);
  len = SCHEME_PATH_LEN(bs);

  if (has_null(s, len))
    raise_null_error("simplify-path", argv[0], "");

  use_fs = ((argc <= 1) || SCHEME_TRUEP(argv[1]));
  kind = SCHEME_PATH_KIND(bs);

  if (use_fs && (kind != SCHEME_PLATFORM_PATH_KIND)) {
    scheme_contract_error("simplify-path",
                          "in use-filesystem mode, path is not for the current platform",
                          "path", 1, argv[0],
                          NULL);
  }
  
  r = do_simplify_path(bs, scheme_null, 0, use_fs, 0, kind);

  if (SCHEME_FALSEP(r)) {
    /* Input was just 'same: */
    return scheme_make_sized_offset_kind_path((kind == SCHEME_WINDOWS_PATH_KIND) ? ".\\" : "./", 0, 2, 0, kind);
  }

  return r;
}

static Scheme_Object *current_drive(int argc, Scheme_Object *argv[])
{
#ifdef DOS_FILE_SYSTEM
  char *drive;

  drive = scheme_getdrive();

  return scheme_make_sized_path(drive, strlen(drive), 0);
#else
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED, "current-drive: " NOT_SUPPORTED_STR);
  return NULL;
#endif
}

static Scheme_Object *cleanse_path(int argc, Scheme_Object *argv[])
{
  char *filename;
  int expanded, kind;

  if (!SCHEME_GENERAL_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("cleanse-path", "(or/c path-for-some-system? path-string?)", 0, argc, argv);

  if (SCHEME_GENERAL_PATHP(argv[0]))
    kind = SCHEME_PATH_KIND(argv[0]);
  else
    kind = SCHEME_PLATFORM_PATH_KIND;

  filename = do_expand_filename(argv[0],
				NULL,
				0,
				"cleanse-path",
				&expanded,
				1, 0,
				0, /* no security check, since the filesystem is not used */ 
                                kind,
                                0);
  
  if (!expanded && SCHEME_GENERAL_PATHP(argv[0]))
    return argv[0];
  else
    return scheme_make_sized_offset_kind_path(filename, 0, strlen(filename), 1, kind);
}

static Scheme_Object *expand_user_path(int argc, Scheme_Object *argv[])
{
  char *filename;
  int expanded;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("expand-user-path", "path-string?", 0, argc, argv);

  filename = do_expand_filename(argv[0],
				NULL,
				0,
				"expand-user-path",
				&expanded,
				1, 0,
				SCHEME_GUARD_FILE_EXISTS, 
                                SCHEME_PLATFORM_PATH_KIND,
                                1);

  if (!expanded && SCHEME_PATHP(argv[0]))
    return argv[0];
  else
    return scheme_make_sized_path(filename, strlen(filename), 1);
}

#ifdef USE_FINDFIRST
void do_find_close(void *p) 
{
  FIND_CLOSE(*(FF_HANDLE_TYPE *)p);
}
#endif

static Scheme_Object *do_directory_list(int break_ok, int argc, Scheme_Object *argv[])
{
#if !defined(NO_READDIR) || defined(USE_FINDFIRST)
  char *filename;
  Scheme_Object * volatile first = scheme_null, * volatile last = NULL, *n, *elem;
#endif
#ifndef NO_READDIR
  DIR *dir;
  int nlen;
  struct dirent *e;
#endif
#ifdef USE_FINDFIRST
  char *pattern;
  int len;
  FF_HANDLE_TYPE hfile, *hfile_ptr = NULL;
  FF_TYPE info;
#endif
  volatile int counter = 0;

  if (argc && !SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("directory-list", "path-string?", 0, argc, argv);

#if defined(NO_READDIR) && !defined(USE_FINDFIRST)
  return scheme_null;
#else

  if (argc) {
    Scheme_Object *path = argv[0];
# ifdef USE_FINDFIRST
    while (1) {
# endif
      filename = do_expand_filename(path, NULL, 0, 
				    break_ok ? "directory-list" : NULL, 
				    NULL, 1, 259 - 4 /* leave room for \*.* in Windows */, 
				    break_ok ? SCHEME_GUARD_FILE_READ : 0, 
                                    SCHEME_PLATFORM_PATH_KIND,
                                    0);
      if (!filename)
	return NULL;
# ifdef USE_FINDFIRST
      /* Eliminate "." and "..": */
      if (SAME_OBJ(path, argv[0])) {
	Scheme_Object *old;
	old = scheme_make_path(filename);
	path = do_simplify_path(old, scheme_null, 0, 1, 0, SCHEME_WINDOWS_PATH_KIND);
	if (SAME_OBJ(path, old))
	  break;
      } else
	break;
    }
# endif
  } else {
    filename = SCHEME_PATH_VAL(CURRENT_WD());
    if (break_ok) {
      scheme_security_check_file("directory-list", NULL, SCHEME_GUARD_FILE_EXISTS);
      scheme_security_check_file("directory-list", filename, SCHEME_GUARD_FILE_READ);
    }
  }

# ifdef USE_FINDFIRST

  if (!filename)
    pattern = "*.*";
  else {
    char *nf;
    int is_unc = 0, d, nd;
    len = strlen(filename);
    if ((len > 1) && IS_A_DOS_SEP(filename[0]) && check_dos_slashslash_drive(filename, 0, len, NULL, 0, 0))
      is_unc = 1;
    nf = scheme_normal_path_seps(filename, &len, 0);
    pattern = (char *)scheme_malloc_atomic(len + 14);
    
    if ((scheme_stupid_windows_machine > 0)
	|| check_dos_slashslash_qm(filename, len, NULL, NULL, NULL)) {
      d = 0;
      nd = 0;
    } else {
      pattern[0] = '\\';
      pattern[1] = '\\';
      pattern[2] = '?';
      pattern[3] = '\\';
      if (is_unc) {
	pattern[4] = 'U';
	pattern[5] = 'N';
	pattern[6] = 'C';
	pattern[7] = '\\';
	d = 8;
	nd = 2;
      } else {
	d = 4;
	nd = 0;
      }
    }
    memcpy(pattern + d, nf + nd, len - nd);
    len += (d - nd);
    if (len && !IS_A_DOS_SEP(pattern[len - 1]))
      pattern[len++] = '\\';      
    memcpy(pattern + len, "*.*", 4);
  }

  hfile = FIND_FIRST(WIDE_PATH(pattern), &info);
  if (FIND_FAILED(hfile)) {
    if (!filename)
      return scheme_null;
    if (break_ok)
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                       "directory-list: could not open directory\n"
                       "  path: %q\n"
                       "  system error: %E",
                       filename,
                       GetLastError());  
    return NULL;
  }

  do {
    if ((GET_FF_NAME(info)[0] == '.')
	&& (!GET_FF_NAME(info)[1] || ((GET_FF_NAME(info)[1] == '.')
				      && !GET_FF_NAME(info)[2]))) {
      /* skip . and .. */
    } else {
      n = make_protected_path(NARROW_PATH(info.cFileName));
      elem = scheme_make_pair(n, scheme_null);
      if (last)
	SCHEME_CDR(last) = elem;
      else
	first = elem;
      last = elem;
    }
    counter++;
    if (break_ok && !(counter & 0x15)) {
      if (!hfile_ptr) {
	hfile_ptr = (FF_HANDLE_TYPE *)scheme_malloc_atomic(sizeof(FF_HANDLE_TYPE));
	*hfile_ptr = hfile;
      }
      BEGIN_ESCAPEABLE(do_find_close, hfile_ptr);
      scheme_thread_block(0);
      END_ESCAPEABLE();
      scheme_current_thread->ran_some = 1;
    }
  } while (FIND_NEXT(hfile, &info));
  
  FIND_CLOSE(hfile);

  return first;
# else
  
  dir = opendir(filename ? filename : ".");
  if (!dir) {
    if (!filename)
      return scheme_null;
    if (break_ok)
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                       "directory-list: could not open directory\n"
                       "  path: %q\n"
                       "  system error: %e",
                       filename,
                       errno);
    return NULL;
  }
  
  while ((e = readdir(dir))) {
#  ifdef DIRENT_NO_NAMLEN
    nlen = strlen(e->d_name);
#  elif defined(__QNX__) || defined(__QNXNTO__)
    nlen = e->d_namelen;
#  else
    nlen = e->d_namlen;
#  endif
#  if defined(UNIX_FILE_SYSTEM) || defined(DOS_FILE_SYSTEM)
    if (nlen == 1 && e->d_name[0] == '.')
      continue;
    if (nlen == 2 && e->d_name[0] == '.' && e->d_name[1] == '.')
      continue;
#  endif
    n = make_protected_sized_offset_path(1, e->d_name, 0, nlen, 1, 0, SCHEME_PLATFORM_PATH_KIND);
    elem = scheme_make_pair(n, scheme_null);
    if (last)
      SCHEME_CDR(last) = elem;
    else
      first = elem;
    last = elem;

    counter++;
    if (break_ok && !(counter & 0xF)) {
      BEGIN_ESCAPEABLE(closedir, dir);
      scheme_thread_block(0);
      END_ESCAPEABLE();
      scheme_current_thread->ran_some = 1;
    }
  }
  
  closedir(dir);

  return first;
# endif
#endif
}

static Scheme_Object *directory_list(int argc, Scheme_Object *argv[])
{
  return do_directory_list(1, argc, argv);
}

char *scheme_find_completion(char *fn)
{
  int len;
  Scheme_Object *p, *l, *a[2], *f, *matches, *fst;
  int isdir, max_match;
  Scheme_Object *base;
  
  len = strlen(fn);

  if (!len)
    return NULL;
  
  f = scheme_split_path(fn, len, &base, &isdir, SCHEME_PLATFORM_PATH_KIND);
  if (isdir) {
    /* Look for single file/prefix in directory: */
    base = scheme_make_sized_path(fn, len, 0);
    f = scheme_make_sized_path("", 0, 0);
  } else {
    if (!SCHEME_PATHP(base))
      return NULL;
  }

  a[0] = base;
  l = do_directory_list(0, 1, a);
  if (!l)
    return NULL;

  matches = scheme_null;
  while (SCHEME_PAIRP(l)) {
    p = SCHEME_CAR(l);
    if ((SCHEME_PATH_LEN(p) >= SCHEME_PATH_LEN(f))
	&& !memcmp(SCHEME_PATH_VAL(f), SCHEME_PATH_VAL(p), SCHEME_PATH_LEN(f))) {
      matches = scheme_make_pair(p, matches);
    }
    l = SCHEME_CDR(l);
  }

  if (SCHEME_NULLP(matches))
    return NULL;

  if (SCHEME_NULLP(SCHEME_CDR(matches))) {
    /* One match */
    a[0] = base;
    a[1] = SCHEME_CAR(matches);
    p = scheme_build_path(2, a);
    a[0] = p;
    if (SCHEME_TRUEP(directory_exists(1, a))) {
      /* Add trailing separator if one is not there */
      fn = SCHEME_PATH_VAL(p);
      len = SCHEME_PATH_LEN(p);
      if (!IS_A_SEP(SCHEME_PLATFORM_PATH_KIND, fn[len-1])) {
	char *naya;
	naya = (char *)scheme_malloc_atomic(len + 2);
	memcpy(naya, fn, len);
	naya[len++] = FN_SEP(SCHEME_PLATFORM_PATH_KIND);
	naya[len] = 0;
	fn = naya;
      }
    } else
      fn = SCHEME_PATH_VAL(p);
    return fn;
  }

  fst = SCHEME_CAR(matches);
  max_match = SCHEME_PATH_LEN(fst);
  for (l = SCHEME_CDR(matches); SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    int i, l2;
    p = SCHEME_CAR(l);
    l2 = SCHEME_PATH_LEN(p);
    if (max_match < l2)
      l2 = max_match;
    else if (l2 < max_match)
      max_match = l2;
    for (i = 0; i < l2; i++) {
      if (SCHEME_PATH_VAL(fst)[i] != SCHEME_PATH_VAL(p)[i]) {
	max_match = i;
	break;
      }
    }
  }

  if (max_match <= SCHEME_PATH_LEN(f)) 
    /* No longer match available: */
    return NULL;

  /* Build match */
  a[0] = base;
  a[1] = scheme_make_sized_path(SCHEME_PATH_VAL(fst), max_match, 0);
  f = scheme_build_path(2, a);  

  return SCHEME_PATH_VAL(f);
}

static Scheme_Object *explode_path(Scheme_Object *p)
{
  Scheme_Object *l = scheme_null, *base, *name;
  int isdir;

  while (1) {
    name = scheme_split_path(SCHEME_PATH_VAL(p), SCHEME_PATH_LEN(p), &base, &isdir, SCHEME_PATH_KIND(p));
    l = scheme_make_pair(name, l);

    if (!SCHEME_PATHP(base)) {
      l = scheme_make_pair(base, l);
      return l;
    }
    p = base;
  }
}

Scheme_Object *scheme_extract_relative_to(Scheme_Object *obj, Scheme_Object *dir)
{
  Scheme_Object *de, *be, *oe;

  if (SCHEME_PAIRP(dir)) {
    be = explode_path(SCHEME_CAR(dir));
    de = explode_path(SCHEME_CDR(dir));
  } else {
    be = explode_path(dir);
    de = be;
  }
  oe = explode_path(obj);

  while (SCHEME_PAIRP(de)
	 && SCHEME_PAIRP(oe)) {
    if (!scheme_equal(SCHEME_CAR(de), SCHEME_CAR(oe)))
      return obj;
    de = SCHEME_CDR(de);
    oe = SCHEME_CDR(oe);
    be = SCHEME_CDR(be);
  }

  if (SCHEME_NULLP(de)) {
    Scheme_Object *a[2];

    while (SCHEME_PAIRP(be)
           && SCHEME_PAIRP(oe)
           && scheme_equal(SCHEME_CAR(be), SCHEME_CAR(oe))) {
      oe = SCHEME_CDR(oe);
      be = SCHEME_CDR(be);
    }

    if (SCHEME_NULLP(oe)) {
      a[0] = same_symbol;
      obj = scheme_build_path(1, a);
    } else {
      obj = SCHEME_CAR(oe);
      oe = SCHEME_CDR(oe);
    }

    while (SCHEME_PAIRP(oe)) {
      a[0] = obj;
      a[1] = SCHEME_CAR(oe);
      obj = scheme_build_path(2, a);
      oe = SCHEME_CDR(oe);
    }

    while (!SCHEME_NULLP(be)) {
      a[0] = up_symbol;
      a[1] = obj;
      obj = scheme_build_path(2, a);
      be = SCHEME_CDR(be);
    }
  }

  return obj;
}

static Scheme_Object *filesystem_root_list(int argc, Scheme_Object *argv[])
{
  Scheme_Object *first = scheme_null;
#if defined(DOS_FILE_SYSTEM)
  Scheme_Object *last = NULL, *v;
#endif

  scheme_security_check_file("filesystem-root-list", NULL, SCHEME_GUARD_FILE_EXISTS);

#ifdef UNIX_FILE_SYSTEM 
  first = scheme_make_pair(scheme_make_path("/"), scheme_null);
#endif
#ifdef DOS_FILE_SYSTEM
  {
#   define DRIVE_BUF_SIZE 1024
    char drives[DRIVE_BUF_SIZE], *s;
    intptr_t len, ds;
    UINT oldmode;

    len = GetLogicalDriveStrings(DRIVE_BUF_SIZE, drives);
    if (len <= DRIVE_BUF_SIZE)
      s = drives;
    else {
      s = scheme_malloc_atomic(len + 1);
      GetLogicalDriveStrings(len + 1, s);
    }

    ds = 0;
    oldmode = SetErrorMode(SEM_FAILCRITICALERRORS);      
    while (s[ds]) {
      DWORD a, b, c, d;
      /* GetDiskFreeSpace effectively checks whether we can read the disk: */
      if (GetDiskFreeSpace(s XFORM_OK_PLUS ds, &a, &b, &c, &d)) {
	v = scheme_make_pair(scheme_make_sized_offset_path(s, ds, -1, 1), scheme_null);
	if (last)
	  SCHEME_CDR(last) = v;
	else
	  first = v;
	last = v;
      }
      ds += strlen(s XFORM_OK_PLUS ds) + 1;
    }
    SetErrorMode(oldmode);
  }
#endif

  return first;
}

static Scheme_Object *make_directory(int argc, Scheme_Object *argv[])
{
#ifdef NO_MKDIR
  return scheme_false;
#else
  char *filename;
  int exists_already = 0;
  int len, copied;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("make-directory", "path-string?", 0, argc, argv);

  filename = scheme_expand_string_filename(argv[0],
					   "make-directory",
					   &copied,
					   SCHEME_GUARD_FILE_WRITE);
  
  /* Make sure path doesn't have trailing separator: */
  len = strlen(filename);
  while (len && IS_A_SEP(SCHEME_PLATFORM_PATH_KIND, filename[len - 1])) {
    if (!copied) {
      filename = scheme_strdup(filename);
      copied = 1;
    }
    filename[--len] = 0;
  }

  while (1) {
    if (!MSC_W_IZE(mkdir)(MSC_WIDE_PATH(filename)
#  ifndef MKDIR_NO_MODE_FLAG
			  , 0777
# endif
			  ))
      return scheme_void;
    else if (errno != EINTR)
      break;
  }

  exists_already = (errno == EEXIST);
# define MKDIR_EXN_TYPE "%e"

  scheme_raise_exn(exists_already ? MZEXN_FAIL_FILESYSTEM_EXISTS : MZEXN_FAIL_FILESYSTEM,
		   "make-directory: cannot make directory\n"
                   "  path: %q\n"
                   "  system error: " MKDIR_EXN_TYPE,
		   filename_for_error(argv[0]),
		   errno);
  return NULL;
#endif
}

static Scheme_Object *delete_directory(int argc, Scheme_Object *argv[])
{
#ifdef NO_RMDIR
  return scheme_false;
#else
# ifdef DOS_FILE_SYSTEM
  int tried_cwd = 0;
# endif
  char *filename;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("delete-directory", "path-string?", 0, argc, argv);

  filename = scheme_expand_string_filename(argv[0],
					   "delete-directory",
					   NULL,
					   SCHEME_GUARD_FILE_DELETE);

  while (1) {
    if (!MSC_W_IZE(rmdir)(MSC_WIDE_PATH(filename)))
      return scheme_void;
# ifdef DOS_FILE_SYSTEM
    else if ((errno == EACCES) && !tried_cwd) {
      /* Maybe we're using the target directory. Try a real setcwd. */
      Scheme_Object *tcd;
      tcd = scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_DIRECTORY);
      scheme_os_setcwd(SCHEME_PATH_VAL(tcd), 0);
      tried_cwd = 1;
    }
# endif
    else if (errno != EINTR)
      break;
  }

  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		   "delete-directory: cannot delete directory\n"
                   "  path: %q\n"
                   "  system error: %e",
		   filename_for_error(argv[0]),
		   errno);
  return NULL;
#endif
}

static Scheme_Object *make_link(int argc, Scheme_Object *argv[])
{
  char *src;
  Scheme_Object *dest;
  int copied;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("make-file-or-directory-link", "path-string?", 0, argc, argv);
  if (!SCHEME_PATH_STRINGP(argv[1]))
    scheme_wrong_contract("make-file-or-directory-link", "path-string?", 0, argc, argv);

  dest = argv[0];
  /* dest does not get expanded, but we need to make sure it's a path */
  dest = TO_PATH(dest);
  if (has_null(SCHEME_PATH_VAL(dest), SCHEME_PATH_LEN(dest))) {
    raise_null_error("make-file-or-directory-link", dest, "");
    return NULL;
  }

  src = scheme_expand_string_filename(argv[1],
				      "make-file-or-directory-link",
				      &copied,
				      SCHEME_GUARD_FILE_WRITE);

  scheme_security_check_file_link("make-file-or-directory-link", 
				  src, 
				  SCHEME_PATH_VAL(dest));

#if defined(DOS_FILE_SYSTEM)
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "make-file-or-directory-link: " NOT_SUPPORTED_STR ";\n"
		   " cannot create link\n"
                   "  path: %Q",
		   argv[1]);
#else
  while (1) {
    if (!symlink(SCHEME_PATH_VAL(dest), src))
      return scheme_void;
    else if (errno != EINTR)
      break;
  }

  scheme_raise_exn((errno == EEXIST) ? MZEXN_FAIL_FILESYSTEM_EXISTS : MZEXN_FAIL_FILESYSTEM,
		   "make-file-or-directory-link: cannot make link\n"
                   "  path: %q\n"
                   "  system error: %e",
		   filename_for_error(argv[1]),
		   errno);
#endif

  return NULL;
}

static Scheme_Object *file_modify_seconds(int argc, Scheme_Object **argv)
{
  char *file;
  int set_time = 0;
  UNBUNDLE_TIME_TYPE mtime;
  struct MSC_IZE(stat) buf;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("file-or-directory-modify-seconds", "path-string?", 0, argc, argv);

  set_time = ((argc > 1) && SCHEME_TRUEP(argv[1]));

  file = scheme_expand_string_filename(argv[0],
				       "file-or-directory-modify-seconds",
				       NULL,
				       (set_time
					? SCHEME_GUARD_FILE_WRITE
					: SCHEME_GUARD_FILE_READ));
  
  if (set_time) {
    if (!SCHEME_INTP(argv[1]) && !SCHEME_BIGNUMP(argv[1])) {
      scheme_wrong_contract("file-or-directory-modify-seconds", "(or/c exact-integer? #f)", 1, argc, argv);
      return NULL;
    }
    if (!scheme_get_time_val(argv[1], &mtime)) {
      scheme_contract_error("file-or-directory-modify-seconds",
                            "integer value is out-of-range",
                            "value", 1, argv[1],
                            NULL);
      return NULL;
    }
  } else
    mtime = 0;

  if (argc > 2) {
    scheme_check_proc_arity("file-or-directory-modify-seconds", 0, 2, argc, argv);
  }

# ifdef DOS_FILE_SYSTEM
  if (!set_time) {
    int len = strlen(file);
    Scheme_Object *secs;

    if (UNC_stat(file, len, NULL, NULL, &secs, NULL, -1))
      return secs;
  } else 
# endif
    {
      while (1) {
	if (set_time) {
	  struct MSC_IZE(utimbuf) ut;
	  ut.actime = mtime;
	  ut.modtime = mtime;
	  if (!MSC_W_IZE(utime)(MSC_WIDE_PATH(file), &ut))
	    return scheme_void;
	} else {
	  if (!MSC_W_IZE(stat)(MSC_WIDE_PATH(file), &buf))
	    return scheme_make_integer_value_from_time(buf.st_mtime);
	}
	if (errno != EINTR)
	  break;
      }
    }

  if (argc > 2) {
    return _scheme_tail_apply(argv[2], 0, NULL);
  }

  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		   "file-or-directory-modify-seconds: error %s file/directory time\n"
                   "  path: %q\n"
                   "  system error: %e",
		   set_time ? "setting" : "getting",
		   filename_for_error(argv[0]),
		   errno);
  return NULL;
}

#if defined(UNIX_FILE_SYSTEM) && !defined(NO_UNIX_USERS)
static int user_in_group(uid_t uid, gid_t gid)
{
  struct group *g;
  struct passwd *pw;
  int i, in;

  if (!group_member_cache) {
    group_member_cache = scheme_make_vector(2 * GROUP_CACHE_SIZE, scheme_false);
    REGISTER_SO(group_member_cache);
  }

  for (i = 0; i < GROUP_CACHE_SIZE; i++) {
    Scheme_Object *gid_e;
    gid_e = SCHEME_VEC_ELS(group_member_cache)[2*i];
    if (!SCHEME_FALSEP(gid_e) && (SCHEME_INT_VAL(gid_e) == gid))
      return SCHEME_FALSEP(SCHEME_VEC_ELS(group_member_cache)[2*i+1]) ? 0 : 1;
  }

  pw = getpwuid(uid);
  if (!pw)
    return 0;

  g = getgrgid(gid);
  if (!g)
    return 0;

  for (i = 0; g->gr_mem[i]; i++) {
    if (!strcmp(g->gr_mem[i], pw->pw_name))
      break;
  }

  in = !!(g->gr_mem[i]);

  for (i = 0; i < GROUP_CACHE_SIZE; i++) {
    if (SCHEME_FALSEP(SCHEME_VEC_ELS(group_member_cache)[2*i])) {
      SCHEME_VEC_ELS(group_member_cache)[2*i]   = scheme_make_integer(gid);
      SCHEME_VEC_ELS(group_member_cache)[2*i+1] = in ? scheme_true : scheme_false;
      return in;
    }
  }

  return in;
}
#endif

static Scheme_Object *file_or_dir_permissions(int argc, Scheme_Object *argv[])
{
  Scheme_Object *l = scheme_null;
  char *filename;
  int as_bits = 0, set_bits = 0, new_bits = 0;
  int err_val = 0;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("file-or-directory-permissions", "path-string?", 0, argc, argv);
  if (argc > 1) {
    l = argv[1];
    if (SCHEME_FALSEP(l)) {
    } else if (SCHEME_SYMBOLP(l) && !SCHEME_SYM_WEIRDP(l)
        && !strcmp("bits", SCHEME_SYM_VAL(l))) {
      as_bits = 1;
    } else {
      as_bits = -1;
      l = argv[1];
      if (SCHEME_INTP(l) 
          && (SCHEME_INT_VAL(l) >= 0) 
          && (SCHEME_INT_VAL(l) <= 0xFFFF)) {
        set_bits = 1;
        new_bits = SCHEME_INT_VAL(l);
      } else
        scheme_wrong_contract("file-or-directory-permissions", 
                              "(or/c #f 'bits (integer-in 0 65535)",
                              1, argc, argv);
    }
  }

  filename = scheme_expand_string_filename(argv[0],
					   "file-or-directory-permissions",
					   NULL,
					   (set_bits
                                            ? SCHEME_GUARD_FILE_WRITE
                                            : SCHEME_GUARD_FILE_READ));

# ifdef NO_STAT_PROC
  return scheme_null;
# else
#  ifdef UNIX_FILE_SYSTEM
  /* General strategy for permissions (to deal with setuid)
     taken from euidaccess() in coreutils... */
#   ifndef NO_UNIX_USERS
  if (have_user_ids == 0) {
    have_user_ids = 1;
    uid = getuid();
    gid = getgid();
    euid = geteuid();
    egid = getegid();
  }

  if (!as_bits && (uid == euid) && (gid == egid)) {
    /* Not setuid; use access() */
    int read, write, execute, ok;
    
    do {
      ok = access(filename, R_OK);
    } while ((ok == -1) && (errno == EINTR));
    read = !ok;

    if (ok && (errno != EACCES))
      l = NULL;
    else {
      do {
	ok = access(filename, W_OK);
      } while ((ok == -1) && (errno == EINTR));
      write = !ok;
      
      if (ok && (errno != EACCES))
	l = NULL;
      else {
	do {
	  ok = access(filename, X_OK);
	} while ((ok == -1) && (errno == EINTR));
	execute = !ok;
      
        /* Don't fail at the exec step if the user is the
           superuser and errno is EPERM; under Mac OS X,
           at least, such a failure simply means tha the
           file is not executable. */
	if (ok && (errno != EACCES) 
            && (uid || gid || (errno != EPERM))) {
	  l = NULL;
	} else {
	  if (read)
	    l = scheme_make_pair(read_symbol, l);
	  if (write)
	    l = scheme_make_pair(write_symbol, l);
	  if (execute)
	    l = scheme_make_pair(execute_symbol, l);
	}
      }
    }
  } else 
#  endif
    {
      /* Use stat, because setuid, or because or no user info available */
      struct stat buf;
      int ok, read, write, execute;

      do {
        ok = stat(filename, &buf);
      } while ((ok == -1) && (errno == EINTR));

      if (ok)
	l = NULL;
      else {
        if (as_bits) {
          if (set_bits) {
            do {
              ok = chmod(filename, new_bits);
            } while ((ok == -1) && (errno == EINTR));
            if (ok)
              l = NULL;
            else
              l = scheme_void;
          } else {
            int bits = buf.st_mode;
#   ifdef S_IFMT
            bits -= (bits & S_IFMT);
#   endif
            l = scheme_make_integer(bits);
          }
        } else {
#   ifndef NO_UNIX_USERS
          if (euid == 0) {
            /* Super-user can read/write anything, and can
               execute anything that someone can execute */
            read = 1;
            write = 1;
            execute = !!(buf.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH));
          } else if (buf.st_uid == euid) {
            read = !!(buf.st_mode & S_IRUSR);
            write = !!(buf.st_mode & S_IWUSR);
            execute = !!(buf.st_mode & S_IXUSR);
          } else if ((egid == buf.st_gid) || user_in_group(euid, buf.st_gid)) {
            read = !!(buf.st_mode & S_IRGRP);
            write = !!(buf.st_mode & S_IWGRP);
            execute = !!(buf.st_mode & S_IXGRP);
          } else {
            read = !!(buf.st_mode & S_IROTH);
            write = !!(buf.st_mode & S_IWOTH);
            execute = !!(buf.st_mode & S_IXOTH);
          }
#   else
          read = !!(buf.st_mode & (S_IRUSR | S_IRGRP | S_IROTH));
          write = !!(buf.st_mode & (S_IWUSR | S_IWGRP | S_IWOTH));
          execute = !!(buf.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH));
#   endif
	
          if (read)
            l = scheme_make_pair(read_symbol, l);
          if (write)
            l = scheme_make_pair(write_symbol, l);
          if (execute)
            l = scheme_make_pair(execute_symbol, l);
        }
      }
    }
  if (!l)
    err_val = errno;
#  endif  
#  ifdef DOS_FILE_SYSTEM
  {
    int len = strlen(filename);
    int flags;

    if (set_bits) {
      int ALWAYS_SET_BITS = ((MZ_UNC_READ | MZ_UNC_EXEC)
                             | ((MZ_UNC_READ | MZ_UNC_EXEC) << 3)
                             | ((MZ_UNC_READ | MZ_UNC_EXEC) << 6));
      if (((new_bits & ALWAYS_SET_BITS) != ALWAYS_SET_BITS)
          || ((new_bits & MZ_UNC_WRITE) != ((new_bits & (MZ_UNC_WRITE << 3)) >> 3))
          || ((new_bits & MZ_UNC_WRITE) != ((new_bits & (MZ_UNC_WRITE << 6)) >> 6))
          || (new_bits >= (1 << 9)))
        scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                         "file-or-directory-permissions: update failed due to"
                         " unsupported bit combination\n"
                         "  path: %s\n"
                         "  permission value: %d",
                         filename_for_error(argv[0]),
                         new_bits);
      l = scheme_void;
    } else
      new_bits = -1;
    
    if (UNC_stat(filename, len, &flags, NULL, NULL, NULL, new_bits)) {
      if (set_bits)
	l = scheme_void;
      else if (as_bits)
        l = scheme_make_integer(flags | (flags << 3) | (flags << 6));
      else {
        if (flags & MZ_UNC_READ)
          l = scheme_make_pair(read_symbol, l);
        if (flags & MZ_UNC_WRITE)
          l = scheme_make_pair(write_symbol, l);
        if (flags & MZ_UNC_EXEC)
          l = scheme_make_pair(execute_symbol, l);
      }
    } else {
      l = NULL;
      err_val = GetLastError();
    }
  }
#  endif
# endif
  
  if (!l) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		     "file-or-directory-permissions: %s failed\n"
                     "  path: %q\n"
                     "  system error: %e",
                     set_bits ? "update" : "access",
		     filename_for_error(argv[0]),
                     err_val);
  }

  return l;
}

static Scheme_Object *file_identity(int argc, Scheme_Object *argv[])
{
  char *filename;
  int as_link = 0;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("file-or-directory-identity", "path-string?", 0, argc, argv);

  filename = scheme_expand_string_filename(argv[0],
					   "file-or-directory-identity",
					   NULL,
					   SCHEME_GUARD_FILE_EXISTS);

  if (argc > 1)
    as_link = SCHEME_TRUEP(argv[1]);

  return scheme_get_fd_identity(NULL, as_link, filename);
}

static Scheme_Object *file_size(int argc, Scheme_Object *argv[])
{
  char *filename;
  mzlonglong len = 0;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("file-size", "path-string?", 0, argc, argv);

  filename = scheme_expand_string_filename(argv[0],
					   "file-size",
					   NULL,
					   SCHEME_GUARD_FILE_READ);

#ifdef DOS_FILE_SYSTEM
 {
   if (UNC_stat(filename, strlen(filename), NULL, NULL, NULL, &len, -1)) {
     return scheme_make_integer_value_from_long_long(len);
   }
 }
#else
  {
    struct BIG_OFF_T_IZE(stat) buf;

    while (1) {
      if (!BIG_OFF_T_IZE(stat)(MSC_WIDE_PATH(filename), &buf))
	break;
      else if (errno != EINTR)
	goto failed;
    }

    if (S_ISDIR(buf.st_mode))
      goto failed;

    len = buf.st_size;
  }

  return scheme_make_integer_value_from_long_long(len);

 failed:
#endif

  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		   "file-size: file not found\n"
                   "  path: %q",
		   filename_for_error(argv[0]));
  return NULL;
}

#endif

#ifdef DIR_FUNCTION

static Scheme_Object *cwd_check(int argc, Scheme_Object **argv)
{
  if (!SCHEME_PATH_STRINGP(argv[0])) {
    return NULL;
  } else {
    char *expanded;
    Scheme_Object *ed;

    expanded = scheme_expand_string_filename(argv[0], "current-directory", NULL, SCHEME_GUARD_FILE_EXISTS);
    ed = scheme_make_sized_path(expanded, strlen(expanded), 1);

# ifndef NO_FILE_SYSTEM_UTILS
    ed = do_simplify_path(ed, scheme_null, 0, 1, 0, SCHEME_PLATFORM_PATH_KIND);
# endif

    ed = scheme_path_to_directory_path(ed);

    return ed;
  }
}

static Scheme_Object *current_directory(int argc, Scheme_Object **argv)
{
  if (!argc)
    scheme_security_check_file("current-directory", NULL, SCHEME_GUARD_FILE_EXISTS);

  return scheme_param_config("current-directory",
			     scheme_make_integer(MZCONFIG_CURRENT_DIRECTORY),
			     argc, argv,
			     -1, cwd_check, 
			     "complete path or string", 1);
}

#endif

static Scheme_Object *collpaths_gen_p(int argc, Scheme_Object **argv, int rel_ok, int abs_ok, int sym_ok)
{
  Scheme_Object *v = argv[0];

  if (scheme_proper_list_length(v) < 0)
    return NULL;

  if (SCHEME_NULLP(v))
    return v;

  while (SCHEME_PAIRP(v)) {
    Scheme_Object *s;
    s = SCHEME_CAR(v);
    if (sym_ok && SAME_OBJ(s, same_symbol)) {
      /* ok */
    } else {
      if (!SCHEME_PATH_STRINGP(s))
        return NULL;
      s = TO_PATH(s);
      if (!abs_ok && !scheme_is_relative_path(SCHEME_PATH_VAL(s),
                                              SCHEME_PATH_LEN(s),
                                              SCHEME_PLATFORM_PATH_KIND))
        return NULL;
      if (!rel_ok && !scheme_is_complete_path(SCHEME_PATH_VAL(s),
                                              SCHEME_PATH_LEN(s),
                                              SCHEME_PLATFORM_PATH_KIND))
        return NULL;
    }
    v = SCHEME_CDR(v);
  }

  if (!SCHEME_NULLP(v))
    return NULL;

  /* Convert to list of paths: */
  {
    Scheme_Object *last = NULL, *first = NULL, *p, *s;
    v = argv[0];
    while (SCHEME_PAIRP(v)) {
      s = SCHEME_CAR(v);
      if (!SCHEME_SYMBOLP(s))
        s = TO_PATH(s);
      
      p = scheme_make_pair(s, scheme_null);
      if (!first)
	first = p;
      else
	SCHEME_CDR(last) = p;
      last = p;

      v = SCHEME_CDR(v);
    }

    return first;
  }
}

#ifndef NO_FILE_SYSTEM_UTILS

static Scheme_Object *collpaths_p(int argc, Scheme_Object **argv)
{
  return collpaths_gen_p(argc, argv, 0, 1, 0);
}

Scheme_Object *scheme_current_library_collection_paths(int argc, Scheme_Object *argv[]) {
  return current_library_collection_paths(argc, argv);
}

static Scheme_Object *current_library_collection_paths(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-library-collection-paths", 
			     scheme_make_integer(MZCONFIG_COLLECTION_PATHS),
			     argc, argv,
			     -1, collpaths_p, "list of complete paths and strings", 1);
}

#endif

static Scheme_Object *compiled_kind_p(int argc, Scheme_Object **argv)
{
  return collpaths_gen_p(argc, argv, 1, 0, 0);
}

static Scheme_Object *use_compiled_kind(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("use-compiled-file-paths",
			     scheme_make_integer(MZCONFIG_USE_COMPILED_KIND),
			     argc, argv,
			     -1, compiled_kind_p, "list of relative paths and strings", 1);
}

static Scheme_Object *compiled_roots_p(int argc, Scheme_Object **argv)
{
  return collpaths_gen_p(argc, argv, 1, 1, 1);
}

Scheme_Object *scheme_compiled_file_roots(int argc, Scheme_Object *argv[])
{
  return compiled_file_roots(argc, argv);
}

static Scheme_Object *compiled_file_roots(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-compiled-file-roots",
			     scheme_make_integer(MZCONFIG_USE_COMPILED_ROOTS),
			     argc, argv,
			     -1, compiled_roots_p, "list of paths, string, and 'same", 1);
}

static Scheme_Object *use_user_paths(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("use-user-specific-search-paths", 
			     scheme_make_integer(MZCONFIG_USE_USER_PATHS),
			     argc, argv,
			     -1, NULL, NULL, 1);
}

static Scheme_Object *use_link_paths(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("use-collection-link-paths", 
			     scheme_make_integer(MZCONFIG_USE_LINK_PATHS),
			     argc, argv,
			     -1, NULL, NULL, 1);
}

/********************************************************************************/

#ifndef NO_FILE_SYSTEM_UTILS

enum {
  id_temp_dir,
  id_home_dir,
  id_doc_dir,
  id_desk_dir,
  id_pref_dir,
  id_pref_file,
  id_init_dir,
  id_init_file,
  id_sys_dir,
  id_addon_dir,
  id_links_file
};

Scheme_Object *scheme_get_run_cmd(void)
{
  if (!run_cmd) {
    return scheme_make_path("racket");
  }
  return run_cmd;
}

static Scheme_Object *
find_system_path(int argc, Scheme_Object **argv)
{
  int which;

  if (argv[0] == temp_dir_symbol)
    which = id_temp_dir;
  else if (argv[0] == home_dir_symbol)
    which = id_home_dir;
  else if (argv[0] == doc_dir_symbol)
    which = id_doc_dir;
  else if (argv[0] == desk_dir_symbol)
    which = id_desk_dir;
  else if (argv[0] == pref_dir_symbol)
    which = id_pref_dir;
  else if (argv[0] == init_dir_symbol)
    which = id_init_dir;
  else if (argv[0] == pref_file_symbol)
    which = id_pref_file;
  else if (argv[0] == init_file_symbol)
    which = id_init_file;
  else if (argv[0] == sys_dir_symbol)
    which = id_sys_dir;
  else if (argv[0] == exec_file_symbol) {
    if (!exec_cmd) {
       return scheme_make_path("racket");
    }
    return exec_cmd;
  } else if (argv[0] == run_file_symbol) {
    return scheme_get_run_cmd();
  } else if (argv[0] == collects_dir_symbol) {
    if (!collects_path) {
      return scheme_make_path("collects");
    }
    return collects_path;
  } else if (argv[0] == orig_dir_symbol) {
    return original_pwd;
  } else if (argv[0] == addon_dir_symbol) {
    if (addon_dir) return addon_dir;
    which = id_addon_dir;
  } else if (argv[0] == links_file_symbol) {
    if (links_file) return links_file;
    if (addon_dir) {
      Scheme_Object *pa[2];
      pa[0] = addon_dir;
      pa[1] = scheme_make_path("links.rktd");
      return scheme_build_path(2, pa);
    }
    which = id_links_file;
  } else {
    scheme_wrong_contract("find-system-path", 
                          "(or/c 'home-dir 'pref-dir 'pref-file 'temp-dir\n"
                          "       'init-dir 'init-file 'links-file 'addon-dir\n"
                          "       'doc-dir 'desk-dir 'sys-dir 'exec-file 'run-file\n"
                          "       'collects-dir 'orig-dir)",
                          0, argc, argv);
    return NULL;
  }

  scheme_security_check_file("find-system-path", NULL, SCHEME_GUARD_FILE_EXISTS);

#ifdef UNIX_FILE_SYSTEM
  if (which == id_sys_dir) {
    return scheme_make_path("/");
  }

  if (which == id_temp_dir) {
    char *p;
    
    if ((p = getenv("TMPDIR"))) {
      p = scheme_expand_filename(p, -1, NULL, NULL, 0);
      if (p && scheme_directory_exists(p))
	return scheme_make_path(p);
    }

    if (scheme_directory_exists("/var/tmp"))
      return scheme_make_path("/var/tmp");

    if (scheme_directory_exists("/usr/tmp"))
      return scheme_make_path("/usr/tmp");

    if (scheme_directory_exists("/tmp"))
      return scheme_make_path("/tmp");

    return CURRENT_WD();
  }
  
  {
    /* Everything else is in ~: */
    Scheme_Object *home;
    char *home_str, *ex_home;
    int ends_in_slash;

    if ((which == id_pref_dir) 
	|| (which == id_pref_file)
	|| (which == id_addon_dir)
	|| (which == id_links_file)) {
#if defined(OS_X) && !defined(XONX)
      if ((which == id_addon_dir)
          || (which == id_links_file))
	home_str = "~/Library/Racket/";
      else
	home_str = "~/Library/Preferences/";
#else
      home_str = "~/.racket/";
#endif 
    } else {
#if defined(OS_X) && !defined(XONX)
      if (which == id_desk_dir)
	home_str = "~/Desktop/";
      else if (which == id_doc_dir)
	home_str = "~/Documents/";
      else
#endif
        home_str = "~/";
    }
    
    ex_home = do_expand_filename(NULL, home_str, strlen(home_str), NULL,
                                 NULL,
                                 0, 1,
                                 0, SCHEME_UNIX_PATH_KIND, 
                                 1);

    if (!ex_home) {
      /* Something went wrong with the user lookup. Just drop "~'. */
      home = scheme_make_sized_offset_path(home_str, 1, -1, 1);
    } else
      home = scheme_make_path(ex_home);

    
    if ((which == id_pref_dir) || (which == id_init_dir) 
	|| (which == id_home_dir) || (which == id_addon_dir)
	|| (which == id_desk_dir) || (which == id_doc_dir))
      return home;

    ends_in_slash = (SCHEME_PATH_VAL(home))[SCHEME_PATH_LEN(home) - 1] == '/';
    
    if (which == id_init_file)
      return append_path(home, scheme_make_path("/.racketrc" + ends_in_slash));
    if (which == id_pref_file) {
#if defined(OS_X) && !defined(XONX)
      return append_path(home, scheme_make_path("/org.racket-lang.prefs.rktd" + ends_in_slash));
#else      
      return append_path(home, scheme_make_path("/racket-prefs.rktd" + ends_in_slash));
#endif
    }
    if (which == id_links_file)
      return append_path(home, scheme_make_path("/links.rktd" + ends_in_slash));
  }
#endif

#ifdef DOS_FILE_SYSTEM
  if (which == id_sys_dir) {
    int size;
    wchar_t *s;
    size = GetSystemDirectoryW(NULL, 0);
    s = (wchar_t *)scheme_malloc_atomic((size + 1) * sizeof(wchar_t));
    GetSystemDirectoryW(s, size + 1);
    return scheme_make_path(NARROW_PATH(s));
  }

  {
    char *d, *p;
    Scheme_Object *home;
    int ends_in_slash;
    
    if (which == id_temp_dir) {
      if ((p = getenv("TMP")) || (p = getenv("TEMP"))) {
	p = scheme_expand_filename(p, -1, NULL, NULL, 0);
	if (p && scheme_directory_exists(p))
	  return scheme_make_path(p);
      }
      
      return CURRENT_WD();
    }

    home = NULL;

    {
      /* Try to get Application Data directory: */
      LPITEMIDLIST items;
      int which_folder;

      if ((which == id_addon_dir)
	  || (which == id_pref_dir)
	  || (which == id_pref_file)
	  || (which == id_links_file)) 
	which_folder = CSIDL_APPDATA;
      else if (which == id_doc_dir) {
#       ifndef CSIDL_PERSONAL
#         define CSIDL_PERSONAL 0x0005
#       endif
	which_folder = CSIDL_PERSONAL;
      } else if (which == id_desk_dir)	
	which_folder = CSIDL_DESKTOPDIRECTORY;
      else {
#       ifndef CSIDL_PROFILE
#         define CSIDL_PROFILE 0x0028
#       endif
	which_folder = CSIDL_PROFILE;
      }

      if (SHGetSpecialFolderLocation(NULL, which_folder, &items) == S_OK) {
	int ok;
	IMalloc *mi;
	wchar_t *buf;

	buf = (wchar_t *)scheme_malloc_atomic(MAX_PATH * sizeof(wchar_t));
	ok = SHGetPathFromIDListW(items, buf);

	SHGetMalloc(&mi);
	mi->lpVtbl->Free(mi, items);
	mi->lpVtbl->Release(mi);

	if (ok) {
	  home = scheme_make_path_without_copying(NARROW_PATH(buf));
	}
      }
    }

    if (!home) {
      /* Back-up: try USERPROFILE environment variable */
      d = getenv("USERPROFILE");
      if (d) {
	if (scheme_directory_exists(d))
	  home = scheme_make_path_without_copying(d);
      }
    }

    if (!home) {
    /* Last-ditch effort: try HOMEDRIVE+HOMEPATH */
      d = getenv("HOMEDRIVE");
      p = getenv("HOMEPATH");

      if (d && p) {
	char *s;
	s = scheme_malloc_atomic(strlen(d) + strlen(p) + 1);
	strcpy(s, d);
	strcat(s, p);
      
	if (scheme_directory_exists(s))
	  home = scheme_make_path_without_copying(s);
	else
	  home = NULL;
      } else 
	home = NULL;
    
      if (!home) {
	wchar_t name[1024];
      
	if (!GetModuleFileNameW(NULL, name, 1024)) {
	  /* Disaster. Use CWD. */
	  home = CURRENT_WD();
	} else {
	  int i;
	  wchar_t *s;
	
	  s = name;
	
	  i = wc_strlen(s) - 1;
	
	  while (i && (s[i] != '\\')) {
	    --i;
	  }
	  s[i] = 0;
	  home = scheme_make_path(NARROW_PATH(s));
	}
      }
    }
    
    if ((which == id_init_dir)
	|| (which == id_home_dir)
	|| (which == id_doc_dir)
	|| (which == id_desk_dir))
      return home;

    ends_in_slash = (SCHEME_PATH_VAL(home))[SCHEME_PATH_LEN(home) - 1];
    ends_in_slash = ((ends_in_slash == '/') || (ends_in_slash == '\\'));

    if ((which == id_addon_dir)
	|| (which == id_pref_dir)
	|| (which == id_pref_file)) {
      home = append_path(home, scheme_make_path("\\Racket" + ends_in_slash));
      ends_in_slash = 0;
    }

    if (which == id_init_file)
      return append_path(home, scheme_make_path("\\racketrc.rktl" + ends_in_slash));
    if (which == id_pref_file)
      return append_path(home, scheme_make_path("\\racket-prefs.rktd" + ends_in_slash));
    if (which == id_links_file)
      return append_path(home, scheme_make_path("\\links.rktd" + ends_in_slash));
    return home;
  }
#endif

  /* Something went wrong if we get here. */
  return scheme_void;
}

#endif

/* should only called from main */
Scheme_Object *scheme_set_exec_cmd(char *s)
{
#ifndef NO_FILE_SYSTEM_UTILS
  if (!exec_cmd) {
    REGISTER_SO(exec_cmd);
    exec_cmd = scheme_make_path(s);
  }

  return exec_cmd;
#endif
}

/* should only called from main */
Scheme_Object *scheme_set_run_cmd(char *s)
{
#ifndef NO_FILE_SYSTEM_UTILS
  if (!run_cmd) {
    REGISTER_SO(run_cmd);
    run_cmd = scheme_make_path(s);
  }

  return run_cmd;
#endif
}

char *scheme_get_exec_path(void)
{
  if (exec_cmd)
    return SCHEME_PATH_VAL(exec_cmd);
  else
    return NULL;
}

/* should only called from main */
void scheme_set_collects_path(Scheme_Object *p)
{
  if (!collects_path) {
    REGISTER_SO(collects_path);
  }
  collects_path = p;
}


void scheme_set_original_dir(Scheme_Object *d)
{
  original_pwd = d;
}

/* should only called from main */
void scheme_set_addon_dir(Scheme_Object *p)
{
  if (!addon_dir) {
    REGISTER_SO(addon_dir);
  }
  addon_dir = p;
}

/* should only called from main */
void scheme_set_links_file(Scheme_Object *p)
{
  if (!links_file) {
    REGISTER_SO(links_file);
  }
  links_file = p;
}

Scheme_Object *scheme_find_links_path(int argc, Scheme_Object *argv[])
{
  if (inst_links_path)
    return inst_links_path;

  REGISTER_SO(inst_links_path);
  inst_links_path = scheme_apply(argv[0], 0, NULL);

  return inst_links_path;
}

/********************************************************************************/

#ifdef DOS_FILE_SYSTEM

static wchar_t *dlldir;

__declspec(dllexport) wchar_t *scheme_get_dll_path(wchar_t *s);
__declspec(dllexport) void scheme_set_dll_path(wchar_t *p);

wchar_t *scheme_get_dll_path(wchar_t *s)
{
  if (dlldir) {
    int len1, len2;
    wchar_t *p;
    len1 = wc_strlen(dlldir);
    len2 = wc_strlen(s);
    p = (wchar_t *)scheme_malloc_atomic((len1 + len2 + 2) * sizeof(wchar_t));
    memcpy(p, dlldir, len1 * sizeof(wchar_t));
    if (p[len1 - 1] != '\\') {
      p[len1++] = '\\';
    }
    memcpy(p + len1, s, (len2 + 1) * sizeof(wchar_t));
    return p;
  } else
    return s;
}

void scheme_set_dll_path(wchar_t *p)
{
  dlldir = p;
}

#endif
