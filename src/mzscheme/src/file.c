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
#ifdef USE_MAC_FILE_TOOLBOX
# include <Files.h>
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

#if defined(MAC_FILE_SYSTEM) || defined(CARBON_FILE_SYSTEM)
long scheme_creator_id = 'MzSc';
#endif

#ifdef UNIX_FILE_SYSTEM
# define FN_SEP '/'
# define IS_A_SEP(x) ((x) == '/')
#endif
#ifdef DOS_FILE_SYSTEM
# define FN_SEP '\\'
# define IS_A_SEP(x) (((x) == '/') || ((x) == '\\'))
#endif
#ifdef MAC_FILE_SYSTEM
# define FN_SEP ':'
# define IS_A_SEP(x) ((x) == ':')
#endif
#ifdef PALMOS_STUFF
# define FN_SEP 0
# define IS_A_SEP(x) (!(x))
#endif

#define CURRENT_WD() scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_DIRECTORY)

#define TO_PATH(x) (SCHEME_PATHP(x) ? x : scheme_char_string_to_path(x))

#ifdef DOS_FILE_SYSTEM
extern int scheme_stupid_windows_machine;
#endif

/* local */
static Scheme_Object *path_p(int argc, Scheme_Object **argv);
static Scheme_Object *path_to_string(int argc, Scheme_Object **argv);
static Scheme_Object *path_to_bytes(int argc, Scheme_Object **argv);
static Scheme_Object *string_to_path(int argc, Scheme_Object **argv);
static Scheme_Object *bytes_to_path(int argc, Scheme_Object **argv);

static Scheme_Object *file_exists(int argc, Scheme_Object **argv);
static Scheme_Object *directory_exists(int argc, Scheme_Object **argv);
static Scheme_Object *link_exists(int argc, Scheme_Object **argv);

#ifndef NO_FILE_SYSTEM_UTILS
static Scheme_Object *delete_file(int argc, Scheme_Object **argv);
static Scheme_Object *rename_file(int argc, Scheme_Object **argv);
static Scheme_Object *copy_file(int argc, Scheme_Object **argv);
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
static Scheme_Object *simplify_path(int argc, Scheme_Object *argv[]);
static Scheme_Object *expand_path(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_drive(int argc, Scheme_Object *argv[]);
static Scheme_Object *file_modify_seconds(int argc, Scheme_Object *argv[]);
static Scheme_Object *file_or_dir_permissions(int argc, Scheme_Object *argv[]);
static Scheme_Object *file_size(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_library_collection_paths(int argc, Scheme_Object *argv[]);
static Scheme_Object *use_compiled_kind(int, Scheme_Object *[]);
static Scheme_Object *find_system_path(int argc, Scheme_Object **argv);
#endif

#ifdef DIR_FUNCTION
static Scheme_Object *current_directory(int argc, Scheme_Object *argv[]);
#endif

static int has_null(const char *s, long l);
static void raise_null_error(const char *name, Scheme_Object *path, const char *mod);

static char *do_path_to_complete_path(char *filename, long ilen, const char *wrt, long wlen);
static Scheme_Object *do_simplify_path(Scheme_Object *path, Scheme_Object *cycle_check);

static Scheme_Object *up_symbol, *relative_symbol;
static Scheme_Object *same_symbol;
#ifndef NO_FILE_SYSTEM_UTILS
static Scheme_Object *read_symbol, *write_symbol, *execute_symbol;

static Scheme_Object *temp_dir_symbol, *home_dir_symbol, *pref_dir_symbol;
static Scheme_Object *doc_dir_symbol, *desk_dir_symbol;
static Scheme_Object *init_dir_symbol, *init_file_symbol, *sys_dir_symbol;
static Scheme_Object *exec_file_symbol, *pref_file_symbol, *addon_dir_symbol;

static Scheme_Object *exec_cmd;
#endif

void scheme_init_file(Scheme_Env *env)
{
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
  REGISTER_SO(addon_dir_symbol);
#endif

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
  addon_dir_symbol = scheme_intern_symbol("addon-dir");
#endif

  scheme_add_global_constant("path?", 
			     scheme_make_prim_w_arity(path_p, 
						      "path?", 
						      1, 1), 
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
  scheme_add_global_constant("string->path", 
			     scheme_make_prim_w_arity(string_to_path, 
						      "string->path", 
						      1, 1), 
			     env);
  scheme_add_global_constant("bytes->path", 
			     scheme_make_prim_w_arity(bytes_to_path, 
						      "bytes->path", 
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
						      2, 2), 
			     env);
  scheme_add_global_constant("build-path", 
			     scheme_make_prim_w_arity(scheme_build_path,
						      "build-path", 
						      1, -1), 
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
			     scheme_make_prim_w_arity(simplify_path,
						      "simplify-path",
						      1, 1), 
			     env);
  scheme_add_global_constant("expand-path",
			     scheme_make_prim_w_arity(expand_path,
						      "expand-path",
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
						      1, 1), 
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
}

/**********************************************************************/
/*                             paths                                  */
/**********************************************************************/

Scheme_Object *scheme_make_sized_offset_path(char *chars, long d, long len, int copy)
{
  Scheme_Object *s;
  s = scheme_make_sized_offset_byte_string(chars, d, len, copy);
  s->type = scheme_path_type;
  return s;
}

Scheme_Object *scheme_make_path(const char *chars)
{
  return scheme_make_sized_offset_path((char *)chars, 0, -1, 1);
}

Scheme_Object *scheme_make_sized_path(char *chars, long len, int copy)
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
  s->type = scheme_path_type;
  return s;
}

Scheme_Object *scheme_char_string_to_path(Scheme_Object *p)
{
  p = scheme_char_string_to_byte_string_locale(p);
  p->type = scheme_path_type;
  return p;
}


static Scheme_Object *path_p(int argc, Scheme_Object **argv)
{
  return (SCHEME_PATHP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *path_to_string(int argc, Scheme_Object **argv)
{
  Scheme_Object *s;

  if (!SCHEME_PATHP(argv[0]))
    scheme_wrong_type("path->string", "path", 0, argc, argv);

  s = scheme_byte_string_to_char_string_locale(argv[0]);

  if (!SCHEME_CHAR_STRLEN_VAL(s))
    return scheme_make_utf8_string("?");
  else
    return s;
}

static Scheme_Object *path_to_bytes(int argc, Scheme_Object **argv)
{
  if (!SCHEME_PATHP(argv[0]))
    scheme_wrong_type("path->bytes", "path", 0, argc, argv);

  return scheme_make_sized_byte_string(SCHEME_PATH_VAL(argv[0]),
				       SCHEME_PATH_LEN(argv[0]),
				       1);
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
    scheme_wrong_type("string->path", "string", 0, argc, argv);

  p = scheme_char_string_to_path(argv[0]);
  
  check_path_ok("string->path", p, argv[0]);

  return p;
}

static Scheme_Object *bytes_to_path(int argc, Scheme_Object **argv)
{
  Scheme_Object *s;

  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_type("bytes->path", "byte string", 0, argc, argv);

  s = scheme_make_sized_byte_string(SCHEME_BYTE_STR_VAL(argv[0]),
				    SCHEME_BYTE_STRLEN_VAL(argv[0]),
				    SCHEME_MUTABLEP(argv[0]));
  s->type = scheme_path_type;

  check_path_ok("bytes->path", s, argv[0]);

  return s;
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
#ifdef USE_MAC_FILE_TOOLBOX
    char *dir;
    char nbuf[64];
    WDPBRec rec;
    FSSpec spec;
    OSErr err;
    
    rec.ioNamePtr = (StringPtr)nbuf;
    PBHGetVol(&rec, 0);
    
    err = FSMakeFSSpec(rec.ioVRefNum,rec.ioWDDirID,NULL,&spec);
    
    if (err != noErr) {
      return NULL;
    }
    
    dir = scheme_mac_spec_to_path(&spec);

    if (actlen)
      *actlen = strlen(dir) + 1;

    return dir;
#else
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
	  if (actlen)
	    *actlen = 0;

	  if (buf) {
	    buf[0] = 0;
	    return buf;
	  } else {
	    return ".";
	  }
	}
	
	scheme_raise_exn(MZEXN_FAIL_FILESYSTEM, 
			 "current-directory: unknown failure (%e)", errno);
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
#endif
}

#ifdef USE_MAC_FILE_TOOLBOX
static int find_mac_file(const char *filename, int use_real_cwd,
			 FSSpec *spec, int finddir, int findfile,
			 int *dealiased, int *wasdir, int *exists,
			 long *filedate, int *flags, 
			 long *type, unsigned long *size,
			 FInfo *finfo, int set_time); 
#endif

int scheme_os_setcwd(char *expanded, int noexn)
{
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec spec;
#endif
  int err;

#ifdef USE_MAC_FILE_TOOLBOX
  if (find_mac_file(expanded, 1, &spec, 1, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0)) {
    WDPBRec rec;
    
    rec.ioNamePtr = NULL;
    rec.ioVRefNum = spec.vRefNum;
    rec.ioWDDirID = spec.parID;
    
    err = PBHSetVol(&rec, 0);
  } else
    err = 1;
#else
  while (1) {
    err = MSC_W_IZE(chdir)(MSC_WIDE_PATH(expanded));
    if (!err || (errno != EINTR))
      break;
  }
#endif

  if (err && !noexn)
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "current-directory: unable to switch to directory: \"%q\"",
		       expanded);

  return !err;
}

#ifdef DOS_FILE_SYSTEM
#define WC_BUFFER_SIZE 1024
static wchar_t wc_buffer[WC_BUFFER_SIZE];

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
  long len, l;
  wchar_t *ws;

  l = strlen(s);
  len = scheme_utf8_decode(s, 0, l,
			   NULL, 0, -1,
			   NULL, 1/*UTF-16*/, '\t');

  if (!do_copy && (len < (WC_BUFFER_SIZE-1)))
    ws = wc_buffer;
  else
    ws = (wchar_t *)scheme_malloc_atomic(sizeof(wchar_t) * (len + 1));
  scheme_utf8_decode(s, 0, l,
		     (unsigned int *)ws, 0, -1,
		     NULL, 1/*UTF-16*/, '\t');
  ws[len] = 0;
  return ws;
}

char *scheme_convert_from_wchar(const wchar_t *ws)
{
  long len, l;
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
  
  scheme_split_path(filename, strlen(filename), &base, &isdir);
  
  return base;
}

Scheme_Object *scheme_remove_current_directory_prefix(Scheme_Object *fn)
{
  Scheme_Object *cwd;
  long len;

  cwd = scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_DIRECTORY);

  fn = TO_PATH(fn);

  len = SCHEME_PATH_LEN(cwd);
  if ((len < SCHEME_PATH_LEN(fn))
      && !scheme_strncmp(SCHEME_PATH_VAL(cwd), SCHEME_PATH_VAL(fn), len)) {
    /* Skip over path separators: */
    while (IS_A_SEP(SCHEME_PATH_VAL(fn)[len])) {
      len++;
    }

    return scheme_make_sized_offset_path(SCHEME_PATH_VAL(fn), len, SCHEME_PATH_LEN(fn) - len, 1);
  }

  return fn;
}

#ifdef USE_MAC_FILE_TOOLBOX
static int find_mac_file(const char *filename, int use_real_cwd,
			 FSSpec *spec, int finddir, int findfile,
			 int *dealiased, int *wasdir, int *exists,
			 long *filedate, int *flags, 
			 long *type, unsigned long *size,
			 FInfo *finfo, int set_time) 
/* finddir:   0 => don't care if dir is found (but set *wasdir)
              1 => must find a dir
   findfile:  0 => don't care if file is found (but unset *wasdir)
              1 => must find a file
             -1 => must find a link
             -2 => must find a file or link
             -3 => don't care if file or link is found (but unset *wasdir)
   wasdir and exists are always filled in, unless they're null; *exists
    is set to 2 if a link is found
   filedate, flags, type, size, and finfo are only filled in
   when findfile >= 0 (and when they're non-null)
 */
{
  WDPBRec  wdrec;
  CInfoPBRec pbrec;
  char buf[256];
  short find_vref;
  long find_dir_id;
  int need_filedate = 0;

  if (dealiased)
    *dealiased = 0;
  if (wasdir)
    *wasdir = 1;
  if (exists)
    *exists = 0;
  if (flags)
    *flags = 0;
  if (type)
    *type = 0;
  if (size)
    *size = 0;
  if (filedate)
    *filedate = 0;

  if (use_real_cwd) {
    wdrec.ioNamePtr = (StringPtr)buf;
    if (PBHGetVol(&wdrec, 0))
      return 0;
    
    find_vref = wdrec.ioWDVRefNum;
    find_dir_id = wdrec.ioWDDirID;
  } else {
    int len = filename ? strlen(filename) : 0;
    find_vref = -1;
    find_dir_id = 0;
    if (!filename || !scheme_is_complete_path(filename, len))
      filename = do_path_to_complete_path((char *)filename, len, NULL, 0);
  }
  
  /* filename is empty => Local directory */
  if (!*filename) {
    if (findfile && (findfile != -3))
      return 0;
    if (exists)
      *exists = 1;
    need_filedate = 1;
  } else {
    const char *p;
    int has_colon;
    
    for (p = filename; *p && (*p != ':'); p++) {}
    has_colon = (*p == ':');
    
    p = filename;    
    if (*p == ':') {
      p++;
    } else if (has_colon) {
      char vbuf[256];
      int len = 0;
      HParamBlockRec hrec;
      /* It's an absolute path: get the Volume Name and vRefNum */
      while (*p && *p != ':') {
	vbuf[len++] = *(p++);
	if (len > 32)
	  return 0;
      }
      vbuf[len] = 0;
      if (*p)
	p++;
      
      strcat(vbuf, ":");
      {
	/* C to Pascal string: */
	int clen = strlen(vbuf);
	memmove(vbuf + 1, vbuf, clen);
	vbuf[0] = clen;
      }
      hrec.volumeParam.ioNamePtr = (unsigned char *)vbuf;
      hrec.volumeParam.ioVRefNum = 0;
      hrec.volumeParam.ioVolIndex = -1;
      if (PBHGetVInfo(&hrec, 0))
	return 0;
      find_vref = hrec.volumeParam.ioVRefNum;
      find_dir_id = 0;
    }
    
    if (!*p) {
      if (findfile && (findfile != -3))
        return 0;
      if (exists)
        *exists = 1;
      need_filedate = 1;
    }

    while (p && *p) {
      const char *next = p;
      int len = 0;
      Boolean isFolder, wasAlias;
      
      while (*next && *next != ':') {
	buf[len++] = *(next++);
	if (len > 32)
	  return 0;
      }
      buf[len] = 0;
      
      if (*next)
	next++;
      if (!*next)
	next = NULL;
      
      if (len > 32)
	return 0;
      
      if (!len) {
	/* Parent directory */
	pbrec.hFileInfo.ioNamePtr = spec->name;
	pbrec.hFileInfo.ioVRefNum = find_vref;
	pbrec.hFileInfo.ioDirID = find_dir_id;
	pbrec.hFileInfo.ioFDirIndex = -1;
	if (PBGetCatInfo(&pbrec, 0))
	  return 0;	
	find_dir_id = pbrec.dirInfo.ioDrParID;
	if (!next) {
	  if (findfile && (findfile != -3))
	    return 0;
	  if (wasdir)
	    *wasdir = 1;
	  if (exists)
	    *exists = 1;
	  need_filedate = 1;
	}
      } else {
	spec->vRefNum = find_vref;
	spec->parID = find_dir_id;
	memcpy((void *)spec->name, buf, len);
	spec->name[len] = 0;
	{
	  /* C to Pascal string: */
	  int clen = strlen((char *)spec->name);
	  memmove(spec->name + 1, spec->name, clen);
	  spec->name[0] = clen;
	}
	
	pbrec.hFileInfo.ioNamePtr = spec->name;
	pbrec.hFileInfo.ioVRefNum = spec->vRefNum;
	pbrec.hFileInfo.ioDirID = spec->parID;
	pbrec.hFileInfo.ioFDirIndex = 0;
	if (PBGetCatInfo(&pbrec, 0)) {
	  if (finddir || (findfile && (findfile != -3)) || next)
	    return 0;
	  if (wasdir)
	    *wasdir = 0;
	  break;
	} else {
	  /* If it's a file, it could be an alias: */
	  if (!(pbrec.hFileInfo.ioFlAttrib & 0x10)) {
	    Str32 origname;
	    memcpy(origname, spec->name, 32);
	    ResolveAliasFile(spec, 1, &isFolder, &wasAlias);
	    if (!next && (findfile < 0)) {
	      if (wasAlias) {
	        spec->vRefNum = find_vref;
	        spec->parID = find_dir_id;
	        memcpy((void *)spec->name, origname, 32);
	      }
		  if (findfile <= -2) {
	        if (wasdir)
	         *wasdir = 0;
			if (exists)
			 *exists = wasAlias ? 2 : 1;
	        return 1;
	      }
	      if (wasdir)
	        *wasdir = isFolder;
	      if (wasAlias && exists)
	        *exists = 2;
	      return wasAlias ? 1 : 0;
	    }
	    if (wasAlias && dealiased)
	      *dealiased = 1;
	    
	    if (wasAlias) {
	      /* Look it up again: */
	      find_vref = spec->vRefNum;
	      find_dir_id = spec->parID;
	      pbrec.hFileInfo.ioNamePtr = spec->name;
	      pbrec.hFileInfo.ioVRefNum = spec->vRefNum;
	      pbrec.hFileInfo.ioDirID = spec->parID;
	      pbrec.hFileInfo.ioFDirIndex = 0;
	      if (PBGetCatInfo(&pbrec, 0)) {
	        if (finddir || (findfile && (findfile != -3)) || next)
	          return 0;
	        if (wasdir)
	          *wasdir = 0;
	        if (exists)
	          *exists = 0;
	        break;
	      }
	    }
	  }
	 
	  if (!(pbrec.hFileInfo.ioFlAttrib & 0x10)) {
	    if (finddir || next)
	      return 0; /* Not a directory */
	    if (wasdir)
	      *wasdir = 0;
	    if (exists)
	      *exists = 1;
	    if (filedate)
	      *filedate = pbrec.hFileInfo.ioFlMdDat;
	    if (type)
	      *type = pbrec.hFileInfo.ioFlFndrInfo.fdType;
	    if (flags)
	      *flags = pbrec.hFileInfo.ioFlAttrib;
	    if (size)
	      *size = pbrec.hFileInfo.ioFlLgLen;
	    if (finfo)
	      memcpy(finfo, &pbrec.hFileInfo.ioFlFndrInfo, sizeof(FInfo));
	  } else {
	    if (findfile && (findfile != -3) && !next)
	      return 0;
	    find_vref = spec->vRefNum;
	    find_dir_id = pbrec.hFileInfo.ioDirID;
	    if (!next) {
	      if (flags)
	        *flags = pbrec.hFileInfo.ioFlAttrib;
	      if (filedate)
	        *filedate = pbrec.dirInfo.ioDrMdDat;
	      if (exists)
	        *exists = 1;
	     }
	  }
        }  
      }
      
      p = next;
    }
  }
  
  if (need_filedate && filedate) {
    Str255 buffer;
    pbrec.hFileInfo.ioNamePtr = buffer;
    pbrec.hFileInfo.ioVRefNum = find_vref;
    pbrec.hFileInfo.ioDirID = find_dir_id;
    pbrec.hFileInfo.ioFDirIndex = -1;
    if (!PBGetCatInfo(&pbrec, 0))
      *filedate = pbrec.dirInfo.ioDrMdDat;
  }
  
  spec->vRefNum = find_vref;
  spec->parID = find_dir_id;
  
  return 1;
}

char *scheme_mac_spec_to_path(FSSpec *spec)
{
#define QUICK_BUF_SIZE 256

  CInfoPBRec pbrec;
  char buf[256], qbuf[QUICK_BUF_SIZE], *s;
  int i, j, size = 0, alloced = QUICK_BUF_SIZE - 1;
  int vref = spec->vRefNum;
  long dirID = spec->parID;

  s = qbuf;
  for (i = spec->name[0]; i; i--) {
    s[size++] = spec->name[i];
  }

  while (1)  {
    pbrec.hFileInfo.ioNamePtr = (unsigned char *)buf;
    pbrec.hFileInfo.ioVRefNum = vref;
    pbrec.hFileInfo.ioDirID = dirID;
    pbrec.hFileInfo.ioFDirIndex = -1;
	if (PBGetCatInfo(&pbrec, 0))
	  return NULL;
	
	if (size + buf[0] + 1 > alloced) {
	   char *old;
	   
	   alloced *= 2;
	   old = (char *)scheme_malloc_atomic(alloced + 1);
	   memcpy(old, s, size);
	}

    s[size++] = ':';
    for (i = buf[0]; i; i--) {
      s[size++] = buf[i];
    }
	  
    dirID = pbrec.dirInfo.ioDrParID;
    if (dirID == 1)
      break;
  }
  
  if (alloced < QUICK_BUF_SIZE) {
    s = (char *)scheme_malloc_atomic(size + 1);
    for (j = 0, i = size; i--; j++) {
      s[j] = qbuf[i];
    }
  } else {
    int save;
    
    for (i = 0, j = size - 1; i < j; i++, j--) {
      save = s[i];
      s[i] = s[j];
      s[j] = save;
    }
  }
  
  s[size] = 0;
  return s;
}

void scheme_file_create_hook(char *filename)
{
  FSSpec spec;
    
  if (find_mac_file(filename, 1, &spec, 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0)) {
    FInfo info;

    FSpGetFInfo(&spec, &info);
    info.fdCreator = scheme_creator_id;
    info.fdType = 'TEXT';
    FSpSetFInfo(&spec, &info);
  }
}

int scheme_mac_path_to_spec(const char *filename, FSSpec *spec)
{
  int wasdir;
  
  if (find_mac_file(filename, 1, spec, 0, 0, NULL, &wasdir, NULL, NULL, NULL, NULL, NULL, NULL, 0)) {
    if (wasdir) {
      CInfoPBRec pb;
      
      pb.hFileInfo.ioNamePtr = spec->name;
      pb.hFileInfo.ioVRefNum = spec->vRefNum;
      pb.hFileInfo.ioFDirIndex = -1;
      pb.hFileInfo.ioDirID = spec->parID;
      if (PBGetCatInfo(&pb, 0)) {
	/* Should never happen; the record should exist, because it
	   was used to create the spec. But maybe one day the
	   directory could disappear, in a truly multithreaded
	   MacOS. */
	return 0;
      }           
      spec->parID = pb.dirInfo.ioDrParID;
    }

    return 1;
  } else
    return 0;
}
#endif

static int has_null(const char *s, long l)
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
		     "%s: path string%s contains a null character: %Q", 
		     name, mod, 
		     path);
}

#ifdef DOS_FILE_SYSTEM
static int check_dos_slashslash_drive(const char *next, int len, 
				      int *drive_end, int exact)
{
  int j;
  int is_drive = 0;

  if (drive_end)
    *drive_end = len;

  if (IS_A_SEP(next[1])) {
    /* Check for a drive form: //x/y */
    for (j = 2; j < len; j++) {
      if (!IS_A_SEP(next[j])) {
	/* Found non-sep */
	for (; j < len; j++) {
	  if (IS_A_SEP(next[j])) {
	    /* Found sep again: */
	    for (; j < len; j++) {
	      if (!IS_A_SEP(next[j])) {
		/* Found non-sep again */
		for (; j < len; j++) {
		  if (IS_A_SEP(next[j])) {
		    /* Found sep again... */
		    if (drive_end)
		      *drive_end = j;
		    if (exact) {
		      for (; j < len; j++) {
			if (!IS_A_SEP(next[j])) {
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
	    }
	    break;
	  }
	}
	break;
      }
    }
  }

  return is_drive;
}
#endif

#ifdef DOS_FILE_SYSTEM
static char *get_drive_part(const char *wds, int wdlen)
{
  int dend;
  char *naya;

  if (!check_dos_slashslash_drive(wds, wdlen, &dend, 0))
    dend = 3;

  naya = scheme_malloc_atomic(dend + 1);
  memcpy(naya, wds, dend);
  naya[dend] = 0;

  return naya;
}
#endif

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

#ifdef DOS_FILE_SYSTEM

char *strip_trailing_spaces(const char *s, int *_len)
{
  int len;

  if (_len)
    len = *_len;
  else
    len = strlen(s);

  if (len && (s[len - 1] == ' ')) {
    char *t;

    while (len && (s[len - 1] == ' ')) {
      len--;
    }

    t = (char *)scheme_malloc_atomic(len + 1);
    memcpy(t, s, len);
    t[len] = 0;

    if (_len)
      *_len = len;

    return t;
  }

  return (char *)s;
}

/* Watch out for special device names. Could we do better than hardwiring this list? */
static unsigned char *special_filenames[] = { "NUL", "CON", "PRN", "AUX", "CLOCK$",
					      "COM1", "COM2", "COM3", "COM4", "COM5", 
					      "COM6", "COM7", "COM8", "COM9",
					      "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", 
					      "LPT6", "LPT7", "LPT8", "LPT9", NULL };

#define IS_SPEC_CHAR(x) (IS_A_SEP(x) || ((x) == '"') || ((x) == '|') || ((x) == ':') || ((x) == '<') || ((x) == '>'))

int scheme_is_special_filename(const char *_f, int not_nul)
{
  int i, j, delta;
  const unsigned char *f = (const unsigned char *)_f;

  /* Skip over path: */
  delta = strlen(_f);
  if (!delta) return 0;
  delta -= 1;
  while (delta && !IS_A_SEP(f[delta])) {
    --delta;
  }
  if (!delta && isalpha(f[0]) && f[1] == ':') {
    delta = 2;
  } else if (IS_A_SEP(f[delta]))
    delta++;
  
  for (i = not_nul; special_filenames[i]; i++) {
    unsigned char *sf = special_filenames[i];
    for (j = 0; sf[j] && f[delta + j]; j++) {
      if (toupper(f[delta + j]) != sf[j])
	break;
    }
    if (j && !sf[j]) {
      j += delta;
      f = strip_trailing_spaces(f, NULL);

      if (!f[j])
	return i + 1;
      if ((f[j] == ':') && !(f[j+1]))
	return i + 1;
      /* Look for extension: */
      if (f[j] == '.') {
	j++;
	if (!f[j])
	  return i + 1;
	else {
	  if ((f[j] == '.') || IS_SPEC_CHAR(f[j]) || !isprint(f[j]))
	    return 0;
	  j++;
	  if (f[j]) {
	    if ((f[j] == '.') || IS_SPEC_CHAR(f[j]) || !isprint(f[j]))
	      return 0;
	    j++;
	    if (f[j]) {
	      if ((f[j] == '.') || IS_SPEC_CHAR(f[j]) || !isprint(f[j]))
		return 0;
	      if (!f[j+1])
		return i + 1;
	    }
	  }
	}
      }

      return 0;
    }
  }

  return 0;
}
#endif

static char *do_expand_filename(Scheme_Object *o, char* filename, int ilen, const char *errorin, 
				int *expanded,
				int report_bad_user, int fullpath,
				int guards)
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

#ifdef EXPAND_FILENAME_TILDE
  /* User home lookup strategy taken from wxWindows: */

  if (filename[0] == '~') {
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
			 "%s: bad username in path: \"%q\"", 
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
			 "%s: bad username in path: \"%q\"", 
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

  /* Remove redundant slashes */
 {
   int extra = 0, i;
   
   for (i = ilen; i-- > 1; ) {
     if (IS_A_SEP(filename[i])) {
       if (IS_A_SEP(filename[i - 1])) {
	 extra++;
       }
     }
   }

   if (extra) {
     char *naya;
     naya = (char *)scheme_malloc_atomic(ilen + 1 - extra);
     extra = 0;
     for (i = 0; i < ilen; i++) {
       if (IS_A_SEP(filename[i])
	   && IS_A_SEP(filename[i + 1])) {
	 /* Skip */
	 extra++;
       } else {
	 naya[i - extra] = filename[i];
       }
     }
     ilen -= extra;
     naya[ilen] = 0;
     filename = naya;
     if (expanded)
       *expanded = 1;
   }
 }
#endif
#ifdef DOS_FILE_SYSTEM
  {
    int fixit = 0, i;

    /* Clean up the name, removing mulitple // and
       adding "/" after "c:" if necessary */
    if (isalpha(((unsigned char)filename[0]))
	&& (filename[1] == ':') && !IS_A_SEP(filename[2]))
      fixit = -1;
    else {
      int found_slash = 0;

      for (i = ilen; i-- > 1; ) {
	if (IS_A_SEP(filename[i])) {
	  if (IS_A_SEP(filename[i - 1])) {
	    if ((i > 1) || !found_slash)
	      fixit = 1;
	    break;
	  }
	  found_slash = 1;
	}
      }
    }

    if (fixit) {
      int allow_leading, pos;
      char *naya;
      
      if (expanded)
	*expanded = 1;

      /* Allow // at start? */
      allow_leading = 0;
      if (IS_A_SEP(filename[0]) && IS_A_SEP(filename[1])) {
	for (i = 2; i < ilen; i++) {
	  if (!IS_A_SEP(filename[i])) {
	    /* Found non-sep */
	    for (; i < ilen; i++) {
	      if (IS_A_SEP(filename[i])) {
		/* Found sep */
		for (; i < ilen; i++) {
		  if (!IS_A_SEP(filename[i])) {
		    /* Found non-sep; allow leading */
		    allow_leading = 1;
		    break;
		  }
		}
		break;
	      }
	    }
	    break;
	  }
	}
      }

      pos = i = 0;
      naya = (char *)scheme_malloc_atomic(ilen + 2);

      if (allow_leading) {
	naya[0] = filename[0];
	naya[1] = filename[1];
	pos = i = 2;
      } else if (fixit == -1) {
	naya[0] = filename[0];
	naya[1] = ':';
	naya[2] = '\\';
	pos = 3;
	i = 2;
      }

      while (i < ilen) {
	if (IS_A_SEP(filename[i])
	    && IS_A_SEP(filename[i + 1])) {
	  i++;
	} else
	  naya[pos++] = filename[i++];
      }

      naya[pos] = 0;
      filename = naya;
      ilen = pos;
    }
  }
#endif
#ifdef USE_MAC_FILE_TOOLBOX
  {
    FSSpec spec;
    int dealiased, wasdir;
    
    if (find_mac_file(filename, 0, &spec, 0, 0, &dealiased, &wasdir, NULL, NULL, NULL, NULL, NULL, NULL, 0)) {
      if (wasdir) {
      	// clean up broken FSSpec:
      	FSMakeFSSpec(spec.vRefNum,spec.parID,NULL,&spec);
      }
      if (dealiased) {
        char *s;
        s = scheme_mac_spec_to_path(&spec);
	if (s) {
	  if (expanded)
	    *expanded = 1;
	  return s;
        }
      }
    }
  }
#endif

  if (fullpath) {
    if (!scheme_is_complete_path(filename, ilen)) {
      if (expanded)
	*expanded = 1;
      return do_path_to_complete_path(filename, ilen, NULL, 0);
    }
  }

  return filename;
}

char *scheme_expand_filename(char* filename, int ilen, const char *errorin, int *expanded, int guards)
{
  return do_expand_filename(NULL, filename, ilen, errorin, expanded, 1, 1, guards);
}

char *scheme_expand_string_filename(Scheme_Object *o, const char *errorin, int *expanded, int guards)
{
  return do_expand_filename(o, NULL, 0, errorin, expanded, 1, 1, guards);
}

int scheme_file_exists(char *filename)
{
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec spec;
  
  if (!find_mac_file(filename, 1, &spec, 0, 1, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0))
    return 0;
  return 1;

#else
#ifdef NO_STAT_PROC
  FILE *fp;

  fp = fopen(filename, "r");
  if (fp) {
    fclose(fp);
    return 1;
  } else
    return 0;
#else
  struct MSC_IZE(stat) buf;
  int ok;

#  ifdef DOS_FILE_SYSTEM
  /* Claim that all special files exist: */
  if (scheme_is_special_filename(filename, 0))
    return 1;
#  endif

  do {
    ok = MSC_W_IZE(stat)(MSC_WIDE_PATH(filename), &buf);
  } while ((ok == -1) && (errno == EINTR));

  return !ok && !S_ISDIR(buf.st_mode);
#endif
#endif
}

#ifdef DOS_FILE_SYSTEM
# define FIND_FIRST FindFirstFileW
# define FIND_NEXT FindNextFileW
# define FIND_CLOSE FindClose
# define FF_TYPE WIN32_FIND_DATAW
# define FF_HANDLE_TYPE HANDLE
# define FIND_FAILED(h) (h == INVALID_HANDLE_VALUE)
# define FF_A_RDONLY FILE_ATTRIBUTE_READONLY
# define GET_FF_ATTRIBS(fd) (fd.dwFileAttributes)
# define GET_FF_MODDATE(fd) convert_date(&fd.ftLastWriteTime)
# define GET_FF_NAME(fd) fd.cFileName
static time_t convert_date(const FILETIME *ft)
{
  LONGLONG l;

  l = ((((LONGLONG)ft->dwHighDateTime << 32) | ft->dwLowDateTime)
       - (((LONGLONG)0x019DB1DE << 32) | 0xD53E8000));
  l /= 10000000;

  return (time_t)l;
}
#endif

#ifdef DOS_FILE_SYSTEM
# define MZ_UNC_READ 0x1
# define MZ_UNC_WRITE 0x2
# define MZ_UNC_EXEC 0x4

static int UNC_stat(char *dirname, int len, int *flags, int *isdir, Scheme_Object **date)
{
  int strip_end, strip_char;
  struct MSC_IZE(stat) buf;
  int v;

  if (isdir)
    *isdir = 0;
  if (date)
    *date = scheme_false;

  if ((len > 1) && IS_A_SEP(dirname[0]) && check_dos_slashslash_drive(dirname, len, NULL, 1)) {
    /* stat doesn't work with UNC "drive" names */
    char *copy;
    FF_TYPE fd;
    FF_HANDLE_TYPE fh;
    
    if (isdir)
      *isdir = 1;

    copy = scheme_malloc_atomic(len + 14);
    if (scheme_stupid_windows_machine) {
      memcpy(copy, dirname, len);
    } else {
      copy[0] = '\\';
      copy[1] = '\\';
      copy[2] = '?';
      copy[3] = '\\';
      copy[4] = 'U';
      copy[5] = 'N';
      copy[6] = 'C';
      copy[7] = '\\';
      memcpy(copy + 8, dirname + 2, len - 2);
      len += 6;
    }
    if (!IS_A_SEP(copy[len - 1])) {
      copy[len] = '\\';
      len++;
    }
    memcpy(copy + len, "*.*", 4);
    fh = FIND_FIRST(WIDE_PATH(copy), &fd);
    if (FIND_FAILED(fh)) {
      errno = -1;
      return 0;
    } else {
      if (flags)
	*flags = MZ_UNC_READ | MZ_UNC_EXEC | ((GET_FF_ATTRIBS(fd) & FF_A_RDONLY) ? 0 : MZ_UNC_WRITE);
      if (date) {
	Scheme_Object *dt;
	time_t mdt;
	mdt = GET_FF_MODDATE(fd);
	dt = scheme_make_integer_value_from_time(mdt);
	*date = dt;
      }
      FIND_CLOSE(fh);
      return 1;
    }
  } else if ((len > 1) && (dirname[len - 1] == '\\' || dirname[len - 1] == '/')
	     && (dirname[len - 2] != ':')) {
    strip_end = len - 1;
    strip_char = dirname[strip_end];
    dirname[strip_end] = 0;
    if (isdir)
      *isdir = 1;
  } else
    strip_end = strip_char = 0;

  while (1) {
    v = !MSC_W_IZE(stat)(MSC_WIDE_PATH(dirname), &buf);
    if (v || (errno != EINTR))
      break;
  }

  if (v) {
    if (isdir && S_ISDIR(buf.st_mode))
      *isdir = 1;
    if (date) {
      Scheme_Object *dt;
      dt = scheme_make_integer_value_from_time(buf.st_mtime);
      (*date) = dt;
    }
    if (flags) {
      if (buf.st_mode & MSC_IZE(S_IREAD))
	*flags |= MZ_UNC_READ | MZ_UNC_EXEC;
      if (buf.st_mode & MSC_IZE(S_IWRITE))
	*flags |= MZ_UNC_WRITE;
    }
  }

  if (strip_end)
    dirname[strip_end] = strip_char;

  return v;
}
#endif

int scheme_directory_exists(char *dirname)
{
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec spec;
  
  if (!find_mac_file(dirname, 1, &spec, 1, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0))
    return 0;

  return 1;
#else
# ifdef NO_STAT_PROC
  return 0;
# else
#  ifdef DOS_FILE_SYSTEM
  int isdir;

  return (UNC_stat(dirname, strlen(dirname), NULL, &isdir, NULL)
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
#endif
}

int scheme_is_regular_file(char *filename)
{
#ifdef USE_MAC_FILE_TOOLBOX
  return 1;
#else
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
#endif
}

static Scheme_Object *file_exists(int argc, Scheme_Object **argv)
{
  char *f;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("file-exists?", SCHEME_PATH_STRING_STR, 0, argc, argv);

  f = do_expand_filename(argv[0],
			 NULL,
			 0,
			 "file-exists?",
			 NULL,
			 0, 1,
			 SCHEME_GUARD_FILE_EXISTS);

  return (f && scheme_file_exists(f)) ? scheme_true : scheme_false;
}

static Scheme_Object *directory_exists(int argc, Scheme_Object **argv)
{
  char *f;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("directory-exists?", SCHEME_PATH_STRING_STR, 0, argc, argv);

  f = do_expand_filename(argv[0],
			 NULL,
			 0,
			 "directory-exists?",
			 NULL,
			 0, 1,
			 SCHEME_GUARD_FILE_EXISTS);

  return (f && scheme_directory_exists(f)) ? scheme_true : scheme_false;
}

static Scheme_Object *link_exists(int argc, Scheme_Object **argv)
{
  char *filename;
#ifndef UNIX_FILE_SYSTEM
  Scheme_Object *bs;
#endif

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("link-exists?", SCHEME_PATH_STRING_STR, 0, argc, argv);


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
#ifdef USE_MAC_FILE_TOOLBOX
  {
    FSSpec spec;
  
    scheme_security_check_file("link-exists?", filename, SCHEME_GUARD_FILE_EXISTS);

    if (!find_mac_file(filename, 0, &spec, 0, -1, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0))
      return scheme_false;

    return scheme_true;
  }
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
				  SCHEME_GUARD_FILE_EXISTS);
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

Scheme_Object *scheme_get_fd_identity(Scheme_Object *port, long fd)
{
  int errid = 0;
  unsigned long devi = 0, inoi = 0, inoi2 = 0;
  int shift = 0, shift2 = -1;
  Scheme_Object *devn, *inon, *a[2];

#ifdef FILES_HAVE_FDS
  struct MSC_IZE(stat) buf;

  while (1) {
    if (!MSC_IZE(fstat)(fd, &buf))
      break;
    else if (errno != EINTR) {
      errid = errno;
      break;
    }
  }
  
  if (!errid) {
    /* Warning: we assume that dev_t and ino_t fit in a long. */
    devi = (unsigned long)buf.st_dev;
    inoi = (unsigned long)buf.st_ino;
    shift = sizeof(dev_t);
  }
#endif
#ifdef WINDOWS_FILE_HANDLES
  BY_HANDLE_FILE_INFORMATION info;

  if (GetFileInformationByHandle((HANDLE)fd, &info))
    errid = 0;
  else
    errid = GetLastError();

  if (!errid) {
    devi = info.dwVolumeSerialNumber;
    inoi = info.nFileIndexLow;
    inoi2 = info.nFileIndexHigh;
    shift = sizeof(DWORD);
    shift2 = 2 * sizeof(DWORD);
  }
#endif
#ifdef MAC_FILE_SYSTEM
  FCBPBRec rec;
  OSErr err;

  rec.ioNamePtr = NULL;
  rec.ioFCBIndx = 0;
  rec.ioRefNum = fd;

  err = PBGetFCBInfo(&rec, FALSE);

  if (err == noErr) {
    devi = (unsigned short)rec.ioFCBVRefNum;
    inoi = (unsigned short)rec.ioFCBVRefNum;
    shift = sizeof(short);
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

  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		   "port-file-identity: error obtaining identity (%E)",
		   errid);
  return NULL;
}

char *scheme_normal_path_seps(char *si, int *_len)
{
#ifdef PALMOS_STUFF
  return si;
#else
# ifndef UNIX_FILE_SYSTEM
  int i;
  unsigned char *s;
  int len = *_len;

  s = (unsigned char *)MALLOC_N_ATOMIC(char, len + 1);
  memcpy(s, si, len + 1);

  for (i = 0; i < len; i++) {
#  ifdef DOS_FILE_SYSTEM
    if (s[i] == '/')
      s[i] = '\\';
#  endif
  }

#  ifdef DOS_FILE_SYSTEM
  s = (unsigned char *)strip_trailing_spaces((char *)s, _len);
#  endif

  return (char *)s;
# else
  return si;
# endif
#endif
}

Scheme_Object *scheme_build_path(int argc, Scheme_Object **argv)
{
#define PN_BUF_LEN 256
  int pos, i, len, no_sep;
  int alloc = PN_BUF_LEN;
  char buffer[PN_BUF_LEN], *str, *next;
  int rel, next_off;
#ifdef MAC_FILE_SYSTEM
  int prev_no_sep;
#endif
#ifdef DOS_FILE_SYSTEM
  int first_was_drive = 0;
#endif

  str = buffer;
  pos = 0;

  no_sep = 0; /* This is actually initialized after we know whether
		 it's relative or not. */
  
  for (i = 0 ; i < argc; i++) {
    if (SCHEME_PATH_STRINGP(argv[i])
	|| (SCHEME_SYMBOLP(argv[i]) 
	    && (SAME_OBJ(argv[i], up_symbol)
		|| SAME_OBJ(argv[i], same_symbol)))) {
      next_off = 0;
      if (SAME_OBJ(argv[i], up_symbol)) {
#ifdef UNIX_FILE_SYSTEM
	next = "..";
	len = 2;
#endif
#ifdef DOS_FILE_SYSTEM
	next = "..";
	len = 2;
#endif
#ifdef MAC_FILE_SYSTEM
	next = "";
	len = 0;
#endif
      } else if (SAME_OBJ(argv[i], same_symbol)) {
#ifdef UNIX_FILE_SYSTEM
	next = ".";
	len = 1;
#endif
#ifdef DOS_FILE_SYSTEM
	next = ".";
	len = 1;
#endif
#ifdef MAC_FILE_SYSTEM
	next = "";
	len = 0;
#endif
      } else {
	Scheme_Object *bs;
	bs = TO_PATH(argv[i]);
	next = SCHEME_PATH_VAL(bs);
	len = SCHEME_PATH_LEN(bs);
#ifdef MAC_FILE_SYSTEM
	/* ":" is not a legal particle */
	if ((len == 1) && (next[0] == ':'))
	  len = 0;
#endif	
	if (!len) {
	  char *astr;
	  long alen;

	  astr = scheme_make_args_string("other ", i, argc, argv, &alen);
	  scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			   "build-path: %d%s path element is an empty string%t", 
			   i + 1,
			   scheme_number_suffix(i + 1),
			   astr, alen); 
	  return scheme_false;
	}

	if (has_null(next, len)) {
	  raise_null_error("build-path", argv[i], " element");
	  return NULL;
	}
      }

      /* +3: null term, leading sep, and trailing sep (if up & Mac) */
      if (pos + len + 3 >= alloc) {
	char *naya;
	int newalloc;

	newalloc = 2 * alloc + len + 1;
	naya = (char *)scheme_malloc_atomic(newalloc);
	memcpy(naya, str, pos);
	alloc = newalloc;
	
	str = naya;
      }

#ifdef UNIX_FILE_SYSTEM
      if (next[0] == '/') {
	rel = 0;
	if (i) {
	  scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			   "build-path: absolute path \"%q\" cannot be"
			   " added to a path",
			   next);
	  return scheme_false;
	}
      } else
	rel = 1;
#endif
#ifdef DOS_FILE_SYSTEM
      {
	int is_drive;
	
	if (IS_A_SEP(next[0])) {
	  rel = 0;
	  is_drive = check_dos_slashslash_drive(next, len, NULL, 1);
	} else if ((len >= 2) 
		   && isalpha(((unsigned char)next[0]))
		   && (next[1] == ':')) {
	  int j;
	  rel = 0;
	  for (j = 2; j < len; j++) {
	    if (!IS_A_SEP(next[j]))
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
			     "build-path: %s \"%s\" cannot be"
			     " added to the path \"%q\"",
			     is_drive ? "drive" : "absolute path",
			     next, str);
	    return scheme_false;
	  }

	  if (i == 1) {
	    /* Absolute path onto a drive: skip separator(s) */
	    while (len && IS_A_SEP(next[next_off])) {
	      next_off++;
	      len--;
	    }
	  }
	}

	if (!i)
	  first_was_drive = is_drive;
      }
#endif
#ifdef MAC_FILE_SYSTEM
      /* If we're appending a relative path, strip leading sep; otherwise,
         check for embedded colons */
      if (next[next_off] == FN_SEP) {
	rel = 1;
	if (i) {
	  next_off++;
	  --len;
	}
      } else {
	/* Except for first, ignore a colon at the end. */
	int last = i ? len - 1 : len, j;
	rel = 1;
	for (j = 0; j < last; j++) {
	  if (next[j + next_off] == ':') {
	    if (i) {
	      char *nstr;
#ifdef MZ_PRECISE_GC
	      /* Can't pass unaligned pointer to scheme_raise_exn: */
	      if (next_off & 0x1) {
		nstr = MALLOC_N_ATOMIC(char, len+1);
		memcpy(nstr, next + next_off, len);
		nstr[len] = 0;
	      } else
#endif
		nstr = next + next_off;

	      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			       "build-path: absolute path \"%q\" cannot be"
			       " added to a path",
			       nstr);
	      return scheme_false;
	    } else {
	      rel = 0;
	      break;
	    }
	  }
	}
      }
#endif

      if (!i) {
#ifdef MAC_FILE_SYSTEM
	no_sep = !rel || (next[next_off] == ':');
#else
	no_sep = 1;
#endif
      }
      
#ifdef MAC_FILE_SYSTEM
      if (SAME_OBJ(argv[i], same_symbol)) {
	prev_no_sep = no_sep;
	no_sep = 1;
      }
#endif

      if (!no_sep)
	str[pos++] = FN_SEP;

      memcpy(str + pos, next + next_off, len);
      pos += len;

      /* If last path elem ends in a separator, don't add one: */
      if (len) {
	no_sep = IS_A_SEP(next[next_off + len - 1]);
#ifdef MAC_FILE_SYSTEM
	if (!len)
	  no_sep = 0;
#endif
      } else {
#ifdef MAC_FILE_SYSTEM
	if (SAME_OBJ(argv[i], same_symbol))
	  no_sep = prev_no_sep;
	else
	  no_sep = 0;
#else
	no_sep = 0;
#endif
      }
    } else {
      scheme_wrong_type("build-path", "path, string, 'up, 'same", i, argc, argv);
      return scheme_false;
    }
  }

#ifdef MAC_FILE_SYSTEM
  if (argc && SAME_OBJ(argv[argc - 1], up_symbol))
    str[pos++] = ':';
#endif

  str[pos] = 0;

  return scheme_make_sized_path(str, pos, alloc == PN_BUF_LEN);
}

Scheme_Object *scheme_split_path(const char *path, int len, Scheme_Object **base_out, int *id_out)
{
  char *s;
  int p, last_was_sep = 0, is_dir;
  Scheme_Object *file;
#ifdef DOS_FILE_SYSTEM
  int allow_double_before, drive_end;
#endif

  s = (char *)path;

#ifdef DOS_FILE_SYSTEM
  allow_double_before = 0;
  if ((len > 2) && IS_A_SEP(s[0]) && IS_A_SEP(s[1])) {
    if (check_dos_slashslash_drive(s, len, &drive_end, 0))
      allow_double_before = 1;
    else
      drive_end = 0;
  } else if ((len > 1) && isalpha(((unsigned char)s[0])) && (s[1] == ':'))
    drive_end = 2;
  else
    drive_end = 0;

  if (drive_end && IS_A_SEP(s[drive_end]))
    drive_end++;
#endif

#ifndef MAC_FILE_SYSTEM
#ifdef DOS_FILE_SYSTEM
# define ALLOW_DOUBLE_BEFORE allow_double_before
#else
# define ALLOW_DOUBLE_BEFORE 0
#endif
  /* Look for confusing repeated separators (e.g. "x//y") */
  for (p = len; p--; ) {
    if (p > ALLOW_DOUBLE_BEFORE) {
      if (IS_A_SEP(s[p]) && IS_A_SEP(s[p - 1])) {
	/* Found it; copy without repeats */
	int q;
	char *old = s;

	s = (char *)scheme_malloc_atomic(len);
	--len;

	for (p = 0, q = 0; p < ALLOW_DOUBLE_BEFORE; p++) {
	  s[q++] = old[p];
	}

	for (; p < len; p++) {
	  if (!IS_A_SEP(old[p]) || !IS_A_SEP(old[p + 1]))
	    s[q++] = old[p];
	}
	s[q++] = old[len];
	len = q;
	break;
      }
    }
  }
#endif

#ifdef DOS_FILE_SYSTEM
  if (len <= drive_end)
    p = -1;
  else
#endif
    {
      for (p = len; p--; ) {
	if (IS_A_SEP(s[p])) {
	  if (p != len - 1)
	    break;
	  else
	    last_was_sep = 1;
	}
#ifdef DOS_FILE_SYSTEM
	if (p < drive_end)
	  break;
#endif
      }
    }

#define MAKE_SPLIT(x, y, z) (*base_out = x, *id_out = z, y)

  if (p < 0) {
    Scheme_Object *dir;

    /* No splitting available. 
       For Unx & DOS, it was relative or exactly root.
       For Mac, it is relative or root with trailing sep. */
#ifdef UNIX_FILE_SYSTEM
    if (s[0] == '/')
      return MAKE_SPLIT(scheme_false, scheme_make_sized_path(s, len, 1), 1);
#endif
#ifdef DOS_FILE_SYSTEM
    if (IS_A_SEP(s[0]) || drive_end)
      return MAKE_SPLIT(scheme_false, scheme_make_sized_path(s, len, 1), 1);
#endif

#ifdef MAC_FILE_SYSTEM
    dir = last_was_sep ? scheme_false : relative_symbol;
#else
    dir = relative_symbol;
#endif

  /* Check for 'up: */
#ifndef MAC_FILE_SYSTEM
  if ((s[0] == '.') && (s[1] == '.')
      && (2 >= len || IS_A_SEP(s[2])))
    {
      file = up_symbol;
      is_dir = 1;
    } 
  else if ((s[0] == '.') && (1 >= len || IS_A_SEP(s[1])))
    {
      file = same_symbol;
      is_dir = 1;
    } 
  else
#endif
    {
      int delta;
      is_dir = last_was_sep;
#ifdef MAC_FILE_SYSTEM
      /* We need to keep the trailing separator for a root: */
      delta = last_was_sep;
#else
      delta = 0;
#endif
      file = scheme_make_sized_path(s, len - last_was_sep + delta, 1);
    }

    return MAKE_SPLIT(dir, file, is_dir);
  }
		 
  /* Check for 'up and 'same: */
#if defined(UNIX_FILE_SYSTEM) || defined(DOS_FILE_SYSTEM) || defined(PALMOS_STUFF)
  if ((s[p + 1] == '.') && (s[p + 2] == '.')
      && (p + 3 >= len || IS_A_SEP(s[p + 3])))
#endif
#ifdef MAC_FILE_SYSTEM
  if ((p == len - 2) && s[p + 1] == ':')
#endif  
    {
      file = up_symbol;
      is_dir = 1;
    } 
#ifndef MAC_FILE_SYSTEM
  else if ((s[p + 1] == '.') && (p + 2 >= len || IS_A_SEP(s[p + 2])))
    {
      file = same_symbol;
      is_dir = 1;
    }
#endif
  else 
    {
      file = scheme_make_sized_offset_path(s,
					   p + 1, 
					   len - p - last_was_sep - 1, 
					   1);
      is_dir = last_was_sep;
    }

  /* Check directory */
  if (p > 0) {
    Scheme_Object *ss;
    ss = scheme_make_sized_path(s, p + 1, 1);
    return MAKE_SPLIT(ss, 
		      file, 
		      is_dir);
  }
	
  /* p = 0; for Unix & Dos, this means root dir. For Mac, this means
     it was relative. */
#ifdef MAC_FILE_SYSTEM
  return MAKE_SPLIT(relative_symbol, file, is_dir);
#else
  {
    Scheme_Object *ss;
    ss = scheme_make_sized_path(s, 1, 1);
    return MAKE_SPLIT(ss, file, is_dir);
  }
#endif  
}

#ifndef NO_FILE_SYSTEM_UTILS
static Scheme_Object *split_path(int argc, Scheme_Object **argv)
{
  char *s;
  int is_dir, len;
  Scheme_Object *three[3], *inpath;

  inpath = argv[0];

  if (!SCHEME_PATH_STRINGP(inpath))
    scheme_wrong_type("split-path", SCHEME_PATH_STRING_STR, 0, argc, argv);

  inpath = TO_PATH(inpath);

  s = SCHEME_PATH_VAL(inpath);
  len = SCHEME_PATH_LEN(inpath);

  if (!len) {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "split-path: path is an empty string");
  }

  if (has_null(s, len))
    raise_null_error("split-path", inpath, "");

  three[1] = scheme_split_path(s, len, &three[0], &is_dir);

  three[2] = is_dir ? scheme_true : scheme_false;

  return scheme_values(3, three);
}
#endif

int scheme_is_relative_path(const char *s, long len)
{
#ifdef MAC_FILE_SYSTEM
  int i;
#endif

  if (!len)
    return 0;

#ifdef UNIX_FILE_SYSTEM
  return !((s[0] == '/') || (s[0] == '~'));
#endif
#ifdef DOS_FILE_SYSTEM
  if (IS_A_SEP(s[0])
      || ((len >= 2) 
	  && isalpha(((unsigned char)s[0]))
	  && (s[1] == ':')))
    return 0;
  else
    return 1;
#endif
#ifdef MAC_FILE_SYSTEM
  if (s[0] != ':')
    for (i = 1; i < len; i++) {
      if (s[i] == ':')
	return 0;
    }

  return 1;
#endif
}

int scheme_is_complete_path(const char *s, long len)
{
  if (!len)
    return 0;

  if (!scheme_is_relative_path(s, len)) {
#ifdef DOS_FILE_SYSTEM
    if (IS_A_SEP(s[0]) && IS_A_SEP(s[1])) {
      if (check_dos_slashslash_drive(s, len, NULL, 0))
	return 1;
      else
	return 0;
    } else if ((len >= 2) 
	       && isalpha(((unsigned char)s[0]))
	       && (s[1] == ':')) {
      return 1;
    } else
      return 0;
#else
    return 1;
#endif
  } else 
    return 0;
}

static char *do_path_to_complete_path(char *filename, long ilen, const char *wrt, long wlen)
{
  if (!scheme_is_complete_path(filename, ilen)) {
    char *naya;
    int skip_sep = 0;

    if (!wrt) {
      Scheme_Object *wd;
      wd = CURRENT_WD();
      wrt = SCHEME_PATH_VAL(wd);
      wlen = SCHEME_PATH_LEN(wd);
      scheme_security_check_file("path->complete-path", NULL, SCHEME_GUARD_FILE_EXISTS);
    }

#ifdef DOS_FILE_SYSTEM
    if (!scheme_is_relative_path(filename, ilen)) {
      /* Absolute, not complete. Fill in the disk */
      wrt = get_drive_part(wrt, wlen);
      wlen = strlen(wrt);
      /* drop trailing separator */
      if (IS_A_SEP(wrt[wlen - 1])) {
	wlen--;
	skip_sep = 1;
      }
    }
#endif

    naya = (char *)scheme_malloc_atomic(ilen + wlen + 2);
    memcpy(naya, wrt, wlen);
    if (!skip_sep)
      if (!IS_A_SEP(naya[wlen - 1]))
	naya[wlen++] = FN_SEP;
#ifdef MAC_FILE_SYSTEM
    if (IS_A_SEP(filename[0])) {
      filename++;
      ilen--;
    }
#endif
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
  int len;

  p = argv[0];
  if (!SCHEME_PATH_STRINGP(p))
    scheme_wrong_type("path->complete-path", SCHEME_PATH_STRING_STR, 0, argc, argv);
  p = TO_PATH(p);
  if (argc > 1) {
    wrt = argv[1];
    if (!SCHEME_PATH_STRINGP(wrt))
      scheme_wrong_type("path->complete-path", SCHEME_PATH_STRING_STR, 1, argc, argv);
    wrt = TO_PATH(wrt);
  } else
    wrt = NULL;

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

    if (!scheme_is_complete_path(ws, wlen))
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "path->complete-path: second argument is not a complete path: \"%q\"",
		       ws);

    if (!scheme_is_complete_path(s, len)) {
      s = do_path_to_complete_path(s, len, ws, wlen);
      return scheme_make_sized_path(s, strlen(s), 0);
    }
  } else if (!scheme_is_complete_path(s, len)) {
    s = do_path_to_complete_path(s, len, NULL, 0);

    return scheme_make_sized_path(s, strlen(s), 0);
  }
   
  return p;
}

#ifndef NO_FILE_SYSTEM_UTILS

static char *filename_for_error(Scheme_Object *p)
{
  return do_expand_filename(p, NULL, 0,
			    NULL,
			    NULL,
			    1, 1,
			    0);
}

static Scheme_Object *delete_file(int argc, Scheme_Object **argv)
{
  int errid;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("delete-file", SCHEME_PATH_STRING_STR, 0, argc, argv);

#ifdef USE_MAC_FILE_TOOLBOX
  {
    FSSpec spec;
    char *file;
    int glen;
    Scheme_Object *bs;

    bs = TO_PATH(argv[0]);
    
    file = SCHEME_PATH_VAL(bs);
    flen = SCHEME_PATH_LEN(bs);
    if (has_null(file, flen))
      raise_null_error("delete-file", argv[0], "");
    
    scheme_security_check_file("delete-file", file, SCHEME_GUARD_FILE_DELETE);
 
    if (find_mac_file(file, 0, &spec, 0, -2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0)) {
      errid = FSpDelete(&spec);
      if (!errid)
        return scheme_void;
    }
  }
#else
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
#endif
  
  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM, 
		   "delete-file: cannot delete file: \"%q\" (%e)",
		   filename_for_error(argv[0]),
		   errid);

  return NULL;
}

static Scheme_Object *rename_file(int argc, Scheme_Object **argv)
{
  int exists_ok = 0;
  char *src, *dest;
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec srcspec, destspec;
  int swas_dir, sexists, dexists;
#endif
  Scheme_Object *bss, *bsd;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("rename-file-or-directory", SCHEME_PATH_STRING_STR, 0, argc, argv);
  if (!SCHEME_PATH_STRINGP(argv[1]))
    scheme_wrong_type("rename-file-or-directory", SCHEME_PATH_STRING_STR, 1, argc, argv);
  if (argc > 2)
    exists_ok = SCHEME_TRUEP(argv[2]);

  bss = argv[0];
  bsd = argv[1];

#ifdef USE_MAC_FILE_TOOLBOX
  {
    int srcl, destl;
    bss = TO_PATH(bss);
    bsd = TO_PATH(bsd);
    src = SCHEME_PATH_VAL(bss);
    srcl = SCHEME_PATH_LEN(bss);
    if (has_null(src, srcl))
      raise_null_error("rename-file-or-directory", argv[0], "");
    dest = SCHEME_PATH_VAL(bsd);
    destl = SCHEME_PATH_LEN(bsd);
    if (has_null(dest, destl))
      raise_null_error("rename-file-or-directory", argv[1], "");
  }
  scheme_security_check_file("rename-file-or-directory", src, SCHEME_GUARD_FILE_READ);
  scheme_security_check_file("rename-file-or-directory", dest, SCHEME_GUARD_FILE_WRITE);
#else
  src = scheme_expand_string_filename(bss,
				      "rename-file-or-directory",
				      NULL,
				      SCHEME_GUARD_FILE_READ);
  dest = scheme_expand_string_filename(bsd,
				       "rename-file-or-directory",
				       NULL,
				       SCHEME_GUARD_FILE_WRITE);
#endif

#ifdef USE_MAC_FILE_TOOLBOX
  if (find_mac_file(src, 0, &srcspec, 0, -3, NULL, &swas_dir, &sexists, NULL, NULL, NULL, NULL, NULL, 0)
      && sexists) {
    if (find_mac_file(dest, 0, &destspec, 0, 0, NULL, NULL, &dexists, NULL, NULL, NULL, NULL, NULL, 0)) {
      /* Already exists or different volumes => failure */
      if ((exists_ok || !dexists) && (srcspec.vRefNum == destspec.vRefNum)) {
        int rename;
        
        if (swas_dir) {
          /* Get parent of directory to be moved: */
          CInfoPBRec pb;
          
          pb.hFileInfo.ioNamePtr = srcspec.name;
          pb.hFileInfo.ioVRefNum = srcspec.vRefNum;
          pb.hFileInfo.ioFDirIndex = -1;
          pb.hFileInfo.ioDirID = srcspec.parID;
          if ((errno = PBGetCatInfo(&pb, 0))) {
            goto failed;
	  }
            
          srcspec.parID = pb.dirInfo.ioDrParID;
        }
        
        rename = ((destspec.name[0] != srcspec.name[0])
                  || memcmp(destspec.name, srcspec.name, destspec.name[0] + 1));
        
	if (dexists && exists_ok) {
	  if ((errno = FSpDelete(&destspec)))
	    goto failed;
	}

        if (srcspec.parID != destspec.parID) {
          CMovePBRec mv;
          mv.ioNamePtr = srcspec.name;
          mv.ioVRefNum = srcspec.vRefNum;
          mv.ioDirID = srcspec.parID;
          mv.ioNewName = NULL;
          mv.ioNewDirID = destspec.parID;
          if ((errno = PBCatMove(&mv, 0))) {
            goto failed;
	  }
        }
        
        if (rename) {
          srcspec.parID = destspec.parID;
          if ((errno = FSpRename(&srcspec, destspec.name))) {
	    goto failed;
	  }
      	}
      	
        return scheme_void;
      } else if (!exists_ok && dexists)
	exists_ok = -1;
    }
  }
  errno = -1;
# define MOVE_ERRNO_FORMAT "%e"
#else
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
#endif

#if defined (USE_MAC_FILE_TOOLBOX) || ! defined (DOS_FILE_SYSTEM)
failed:
#endif
  scheme_raise_exn((exists_ok < 0) ? MZEXN_FAIL_FILESYSTEM_EXISTS : MZEXN_FAIL_FILESYSTEM, 
		   "rename-file-or-directory: cannot rename file or directory: %q to: %q (" MOVE_ERRNO_FORMAT ")",
		   filename_for_error(argv[0]),
		   filename_for_error(argv[1]),
		   errno);
  
  return NULL;
}

#ifdef MAC_FILE_SYSTEM
static int xCopyFile(short dest, short src) {
  long i, j;
  char buffer[256]; 
  
  do {
    i = 256;
    FSRead(src, &i, buffer);
    j = i;
    FSWrite(dest, &i, buffer);
    if (j != i) return 0;
  } while (i);
  
  return 1;
}
#endif

static Scheme_Object *copy_file(int argc, Scheme_Object **argv)
{
  char *src, *dest, *reason = NULL;
  int pre_exists = 0;
  Scheme_Object *bss, *bsd;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("copy-file", SCHEME_PATH_STRING_STR, 0, argc, argv);
  if (!SCHEME_PATH_STRINGP(argv[1]))
    scheme_wrong_type("copy-file", SCHEME_PATH_STRING_STR, 1, argc, argv);

  bss = argv[0];
  bsd = argv[1];

#ifdef MAC_FILE_SYSTEM
  bss = TO_PATH(bss);
  bsd = TO_PATH(bsd);
  src = SCHEME_PATH_VAL(bass);
  if (has_null(src, SCHEME_PATH_LEN(bass)))
    raise_null_error("copy-file", bass, "");
  dest = SCHEME_PATH_VAL(bsd);
  if (has_null(dest, SCHEME_PATH_LEN(bsd)))
    raise_null_error("copy-file", bsd, "");
  scheme_security_check_file("copy-file", src, SCHEME_GUARD_FILE_READ);
  scheme_security_check_file("copy-file", dest, SCHEME_GUARD_FILE_WRITE | SCHEME_GUARD_FILE_DELETE);
#else
  src = scheme_expand_string_filename(bss,
				      "copy-file",
				      NULL,
				      SCHEME_GUARD_FILE_READ);
  dest = scheme_expand_string_filename(bsd,
				       "copy-file",
				       NULL, 
				       SCHEME_GUARD_FILE_WRITE | SCHEME_GUARD_FILE_DELETE);
#endif

#ifdef UNIX_FILE_SYSTEM
  {
# define COPY_BUFFER_SIZE 2048
    FILE *s, *d;
    char b[COPY_BUFFER_SIZE];
    long len;
    int ok;
    struct stat buf;


    do {
      ok = stat(src, &buf);
    } while ((ok == -1) && (errno == EINTR));

    if (ok || S_ISDIR(buf.st_mode)) {
      reason = "source file does not exist";
      goto failed;
    }

    do {
      ok = stat(dest, &buf);
    } while ((ok == -1) && (errno == EINTR));

    if (!ok) {
      reason = "destination already exists";
      pre_exists = 1;
      goto failed;
    }

    s = fopen(src, "rb");
    if (!s) {
      reason = "cannot open source file";
      goto failed;
    }

    d = fopen(dest, "wb");
    if (!d) {
      fclose(s);
      reason = "cannot open destination file";
      goto failed;
    }
    
    ok = 1;
    while ((len = fread(b, 1, COPY_BUFFER_SIZE, s))) {
      if (fwrite(b, 1, len, d) != len) {
	ok = 0;
	break;
      }
    }
    if (!feof(s))
      ok = 0;

    fclose(s);
    fclose(d);

    if (ok) {
      while (1) {
	if (!chmod(dest, buf.st_mode))
	  return scheme_void;
	else if (errno != EINTR)
	  break;
      }
      reason = "cannot set destination's mode";
    } else
      reason = "read or write failed";
  }
 failed:
#endif
#ifdef DOS_FILE_SYSTEM
  if (CopyFileW(WIDE_PATH_COPY(src), WIDE_PATH(dest), TRUE))
    return scheme_void;
  
  reason = "copy failed";
  if (GetLastError() == ERROR_ALREADY_EXISTS)
    pre_exists = 1;
#endif
#ifdef MAC_FILE_SYSTEM
  { 
    FInfo finfo;
    FSSpec srcspec, destspec;
    int exists = 0;
    static OSErr en;
   
    if (find_mac_file(src, 0, &srcspec, 0, 1, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &finfo, 0)) {
      if (find_mac_file(dest, 0, &destspec, 0, 0, NULL, NULL, &exists, NULL, NULL, NULL, NULL, NULL, 0)
      	  && !exists) {
        CopyParam rec;
        
        /* Try CopyFile first: */
        rec.ioVRefNum = srcspec.vRefNum;
        rec.ioDstVRefNum = destspec.vRefNum;
        rec.ioDirID = srcspec.parID;
        rec.ioNewDirID = destspec.parID;
        rec.ioNamePtr = srcspec.name;
        rec.ioNewName = destspec.name;
        rec.ioCopyName = NULL;
        if (!(en = PBHCopyFileSync((HParmBlkPtr)&rec)))
          return scheme_void;
        else {
          /* Try the old-fashioned way: */
          short sdf, srf, ddf, drf;
          if (!FSpOpenDF(&srcspec, fsRdPerm, &sdf)) {
            if (!FSpOpenRF(&srcspec, fsRdPerm, &srf)) {
              if (!FSpCreate(&destspec, finfo.fdCreator, finfo.fdType, smSystemScript)) {
                if (!FSpOpenDF(&destspec, fsWrPerm, &ddf)) {
                  if (!FSpOpenRF(&destspec, fsWrPerm, &drf)) {
                    if (xCopyFile(ddf, sdf)) {
                      if (xCopyFile(drf, srf)) {
	                    FSClose(drf);
	                    FSClose(ddf);
	                    FSClose(srf);
	                    FSClose(sdf);
	                    return scheme_void;
	                  } else
			    reason = "read or write error on resource fork";
	                } else
			  reason = "read or write error on data fork";
	                FSClose(drf);
                  }  else
		    reason = "cannot open destination file resource fork";
                  FSClose(ddf);
                } else
		  reason = "cannot open destination file data fork";
                FSpDelete(&destspec);
              } else
		reason = "cannot create destination file";
              FSClose(srf);
            } else
	      reason = "cannot open source file resource fork";
            FSClose(sdf);
          } else
	    reason = "cannot open source file data fork";
        }
      } else {
	if (exists) {
	  reason = "destination already exists";
	  pre_exists = 1;
	} else
	  reason = "bad destination path";
      }
    } else
      reason = "source file does not exist";
  }
#endif

  scheme_raise_exn(pre_exists ? MZEXN_FAIL_FILESYSTEM_EXISTS : MZEXN_FAIL_FILESYSTEM, 
		   "copy-file: %s; cannot copy: %q to: %q",
		   reason,
		   filename_for_error(argv[0]),
		   filename_for_error(argv[1]));
  return NULL;
}

static Scheme_Object *relative_path_p(int argc, Scheme_Object **argv)
{
  char *s;
  int len;
  Scheme_Object *bs;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("relative-path?", SCHEME_PATH_STRING_STR, 0, argc, argv);

  bs = TO_PATH(argv[0]);

  s = SCHEME_PATH_VAL(bs);
  len = SCHEME_PATH_LEN(bs);

  if (has_null(s, len))
    return scheme_false;

  return (scheme_is_relative_path(s, len)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *complete_path_p(int argc, Scheme_Object **argv)
{
  char *s;
  int len;
  Scheme_Object *bs;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("complete-path?", SCHEME_PATH_STRING_STR, 0, argc, argv);

  bs = TO_PATH(argv[0]);

  s = SCHEME_PATH_VAL(bs);
  len = SCHEME_PATH_LEN(bs);

  if (has_null(s, len))
    return scheme_false;

  return (scheme_is_complete_path(s, len)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *absolute_path_p(int argc, Scheme_Object **argv)
{
  char *s;
  int len;
  Scheme_Object *bs;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("absolute-path?", SCHEME_PATH_STRING_STR, 0, argc, argv);

  bs = TO_PATH(argv[0]);

  s = SCHEME_PATH_VAL(bs);
  len = SCHEME_PATH_LEN(bs);

  if (has_null(s, len))
    return scheme_false;

  return (!scheme_is_relative_path(s, len)
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
  long len;
  int copied = 0;
#endif
  char *filename;
  int expanded;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("resolve-path", SCHEME_PATH_STRING_STR, 0, argc, argv);

  filename = do_expand_filename(argv[0],
				NULL,
				0,
				"resolve-path",
				&expanded,
				1, 0,
				SCHEME_GUARD_FILE_EXISTS);

#ifndef NO_READLINK
  {
    char *fullfilename = filename;

    len = strlen(fullfilename);
    if (!scheme_is_complete_path(fullfilename, len)) {
      fullfilename = do_path_to_complete_path(fullfilename, len, NULL, 0);
      copied = 1;
    }

    /* Make sure path doesn't have trailing separator: */
    len = strlen(fullfilename);
    while (len && IS_A_SEP(fullfilename[len - 1])) {
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

static Scheme_Object *do_simplify_path(Scheme_Object *path, Scheme_Object *cycle_check)
{
  int isdir;
  Scheme_Object *file, *base;

#if defined(UNIX_FILE_SYSTEM) || defined(DOS_FILE_SYSTEM)
  /* Fast check; avoids split operations, if possible */
  {
    char *s;
    int len, i, saw_dot = 0;
    s = SCHEME_PATH_VAL(path);
    len = SCHEME_PATH_LEN(path);

    for (i = 0; i < len; i++) {
      if (s[i] == '.')
	saw_dot++;
      else if (IS_A_SEP(s[i])) {
	if ((saw_dot == 1) || (saw_dot == 2))
	  break;
	saw_dot = 0;
      } else
	saw_dot = 3;
    }

    if (i == len) {
      if ((saw_dot != 1) && (saw_dot != 2))
	return path;
    }
    /* There's a . or .. in the path. Switch to 
       slower (but reliable across platforms) mode */
  }
#endif

  /* Check whether it can be simplified: */
  base = path;
  do {
    char *s;
    int len;
    s = SCHEME_PATH_VAL(base);
    len = SCHEME_PATH_LEN(base);
    file = scheme_split_path(s, len, &base, &isdir);
    if (SCHEME_SYMBOLP(file))
      break;
  } while(SCHEME_PATHP(base));

  if (SCHEME_SYMBOLP(file)) {
    /* It can be simplified: */
    char *s;
    int len;
    Scheme_Object *accum = scheme_null, *result;

    /* Make it absolute */
    s = scheme_expand_string_filename(path,
				      "simplify-path", NULL,
				      SCHEME_GUARD_FILE_EXISTS);
    len = strlen(s);

    /* Check for cycles: */
    {
      Scheme_Object *l = cycle_check;
      while (!SCHEME_NULLP(l)) {
	Scheme_Object *p = SCHEME_CAR(l);
	if ((len == SCHEME_PATH_LEN(p))
	    && !strcmp(s, SCHEME_PATH_VAL(p))) {
	  /* Cycle of links detected */
	  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			   "simplify-path: cycle detected at link: \"%q\"",
			   s);
	}
	l = SCHEME_CDR(l);
      }
    }

    cycle_check = scheme_make_pair(scheme_make_sized_path(s, len, 0), 
				   cycle_check);

    /* Split the path into a list. */
    while (1) {
      file = scheme_split_path(s, len, &base, &isdir);
    
      if (SAME_OBJ(file, same_symbol)) {
	/* Drop it */
      } else
	accum = scheme_make_pair(file, accum);
      
      if (SCHEME_PATHP(base)) {
	s = SCHEME_PATH_VAL(base);
	len = SCHEME_PATH_LEN(base);
      } else {
      	/* Start list with root path, not root name.
      	   This is crucial for MacOS */
      	accum = scheme_make_pair(scheme_make_sized_path(s, len, 0),
      				 SCHEME_CDR(accum));
	break;
      }
    }

    /* Now assemble the result */
    result = SCHEME_CAR(accum);
    accum = SCHEME_CDR(accum);
    /* Build up path, watching for links just before a ..: */
    while (!SCHEME_NULLP(accum)) {
      if (SAME_OBJ(SCHEME_CAR(accum), up_symbol)) {
	/* Look for symlink in result-so-far. */
	Scheme_Object *new_result, *a[1];

	while (1) {
	  a[0] = result;
	  new_result = resolve_path(1, a);
	
	  /* Was it a link? */
	  if (result != new_result) {
	    /* It was a link. Is the new result relative? */
	    if (!scheme_is_complete_path(SCHEME_PATH_VAL(new_result),
					 SCHEME_PATH_LEN(new_result))) {
	      Scheme_Object *aa[2], *result_base;
	      /* Yes - resolve it relative to result's base: */
	      scheme_split_path(SCHEME_PATH_VAL(result),
				    SCHEME_PATH_LEN(result),
				    &result_base,
				    &isdir);
	      aa[0] = result_base;
	      aa[1] = new_result;
	      new_result = scheme_build_path(2, aa);
	    }
	    
	    /* Simplify the new result */
	    result = do_simplify_path(new_result, cycle_check);
	    cycle_check = scheme_make_pair(new_result, cycle_check);
	  } else
	    break;
	}
	
	/* Do one 'up: */
	{
	  Scheme_Object *next;
	  accum = SCHEME_CDR(accum);
	  scheme_split_path(SCHEME_PATH_VAL(result),
				SCHEME_PATH_LEN(result),
				&next,
				&isdir);
	  if (!SCHEME_PATH_STRINGP(next)) {
	    /* If result is already a root, we just drop the .. */
	  } else
	    result = next;
	}
      } else {
	/* Add path element onto the result: */
	Scheme_Object *a[2];
	a[0] = result;
	a[1] = SCHEME_CAR(accum);
	result = scheme_build_path(2, a);
	accum = SCHEME_CDR(accum);
      }
    }

    return result;
  } else
    return path;
}

static Scheme_Object *simplify_path(int argc, Scheme_Object *argv[])
{
  char *s;
  int len;
  Scheme_Object *bs;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("simplify-path", SCHEME_PATH_STRING_STR, 0, argc, argv);

  bs = TO_PATH(argv[0]);

  s = SCHEME_PATH_VAL(bs);
  len = SCHEME_PATH_LEN(bs);

  if (has_null(s, len))
    raise_null_error("simplify-path", argv[0], "");

  return do_simplify_path(bs, scheme_null);
}

static Scheme_Object *current_drive(int argc, Scheme_Object *argv[])
{
#ifdef DOS_FILE_SYSTEM
  char *drive;

  drive = scheme_getdrive();

  return scheme_make_sized_path(drive, strlen(drive), 0);
#else
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED, "current-drive: not supported");
  return NULL;
#endif
}

static Scheme_Object *expand_path(int argc, Scheme_Object *argv[])
{
  char *filename;
  int expanded;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("expand-path", SCHEME_PATH_STRING_STR, 0, argc, argv);

  filename = do_expand_filename(argv[0],
				NULL,
				0,
				"expand-path",
				&expanded,
				1, 0,
				SCHEME_GUARD_FILE_EXISTS);

  if (!expanded && SCHEME_PATHP(argv[0]))
    return argv[0];
  else
    return scheme_make_sized_path(filename, strlen(filename), 1);
}

static Scheme_Object *directory_list(int argc, Scheme_Object *argv[])
{
#if !defined(NO_READDIR) || defined(USE_MAC_FILE_TOOLBOX) || defined(USE_FINDFIRST)
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
  FF_HANDLE_TYPE hfile;
  FF_TYPE info;
#endif
#ifdef USE_MAC_FILE_TOOLBOX
  CInfoPBRec pbrec;
  char buf[256];
  FSSpec dir;
  short find_position = 0;
#else
  volatile int counter = 0;
#endif

  if (argc && !SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("directory-list", SCHEME_PATH_STRING_STR, 0, argc, argv);

#if defined(NO_READDIR) && !defined(USE_MAC_FILE_TOOLBOX) && !defined(USE_FINDFIRST)
  return scheme_null;
#else

#ifndef USE_MAC_FILE_TOOLBOX
  if (argc) {
    Scheme_Object *path = argv[0];
# ifdef USE_FINDFIRST
    while (1) {
# endif
      filename = scheme_expand_string_filename(path,
					       "directory-list",
					       NULL,
					       SCHEME_GUARD_FILE_READ);
#ifdef USE_FINDFIRST
      /* Eliminate "." and "..": */
      if (SAME_OBJ(path, argv[0])) {
	Scheme_Object *old;
	old = scheme_make_path(filename);
	path = do_simplify_path(old, scheme_null);
	if (SAME_OBJ(path, old))
	  break;
      } else
	break;
    }
#endif
  } else {
    filename = SCHEME_PATH_VAL(CURRENT_WD());
    scheme_security_check_file("directory-list", NULL, SCHEME_GUARD_FILE_EXISTS);
    scheme_security_check_file("directory-list", filename, SCHEME_GUARD_FILE_READ);
  }

  if (filename && !scheme_directory_exists(filename))
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		     "directory-list: could not open \"%q\"",
		     filename);
#endif

#ifdef USE_MAC_FILE_TOOLBOX
  if (argc) {
    Scheme_Object *bs;
    bs = TO_PATH(argv[0]);
    bs = BYTE_PATH(bs);
    filename = SCHEME_PATH_VAL(bs);
    if (has_null(filename, SCHEME_PATH_LEN(bs)))
      raise_null_error("directory-list", argv[0], "");
  } else {
    filename = SCHEME_PATH_VAL(CURRENT_WD());
    scheme_security_check_file("directory-list", NULL, SCHEME_GUARD_FILE_EXISTS);
  }
  scheme_security_check_file("directory-list", filename, SCHEME_GUARD_FILE_READ);

  if (!find_mac_file(filename, 0, &dir, 1, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0)) {
    if (argc) {
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "directory-list: could not open \"%q\"",
		       filename);
    }
    return scheme_null;
  }
  
  while (1) {
    pbrec.hFileInfo.ioVRefNum = dir.vRefNum;
    pbrec.hFileInfo.ioDirID = dir.parID;
    pbrec.hFileInfo.ioFDirIndex = find_position + 1;
    pbrec.hFileInfo.ioNamePtr = (unsigned char *)buf;
    if (PBGetCatInfo(&pbrec, 0) || !*buf)
      break;
    
    find_position++;

    if (!(find_position & 0x15)) {
      scheme_thread_block(0);
      scheme_current_thread->ran_some = 1;
    }
    
    n = scheme_make_sized_offset_path(buf, 1, buf[0], 1);
    elem = scheme_make_pair(n, scheme_null);
    if (last)
      SCHEME_CDR(last) = elem;
    else
      first = elem;
    last = elem;
  }
  
  return first;
#else
#ifdef USE_FINDFIRST

  if (!filename)
    pattern = "*.*";
  else {
    char *nf;
    int is_unc = 0, d, nd;
    len = strlen(filename);
    if ((len > 1) && IS_A_SEP(filename[0]) && check_dos_slashslash_drive(filename, len, NULL, 0))
      is_unc = 1;
    nf = scheme_normal_path_seps(filename, &len);
    pattern = (char *)scheme_malloc_atomic(len + 14);
    
    if (scheme_stupid_windows_machine) {
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
    if (len && !IS_A_SEP(pattern[len - 1]))
      pattern[len++] = '\\';      
    memcpy(pattern + len, "*.*", 4);
  }

  hfile = FIND_FIRST(WIDE_PATH(pattern), &info);
  if (FIND_FAILED(hfile))
    return scheme_null;

  do {
    if ((GET_FF_NAME(info)[0] == '.')
	&& (!GET_FF_NAME(info)[1] || ((GET_FF_NAME(info)[1] == '.')
				      && !GET_FF_NAME(info)[2]))) {
      /* skip . and .. */
    } else {
      n = scheme_make_path(NARROW_PATH(info.cFileName));
      elem = scheme_make_pair(n, scheme_null);
      if (last)
	SCHEME_CDR(last) = elem;
      else
	first = elem;
      last = elem;
    }
    counter++;
    if (!(counter & 0x15)) {
      BEGIN_ESCAPEABLE(FIND_CLOSE, hfile);
      scheme_thread_block(0);
      END_ESCAPEABLE();
      scheme_current_thread->ran_some = 1;
    }
  } while (FIND_NEXT(hfile, &info));
  
  FIND_CLOSE(hfile);

  return first;
#else
  
  dir = opendir(filename ? filename : ".");
  if (!dir)
    return scheme_null;
  
  while ((e = readdir(dir))) {
#ifdef DIRENT_NO_NAMLEN
    nlen = strlen(e->d_name);
#else
    nlen = e->d_namlen;
#endif
#if defined(UNIX_FILE_SYSTEM) || defined(DOS_FILE_SYSTEM)
    if (nlen == 1 && e->d_name[0] == '.')
      continue;
    if (nlen == 2 && e->d_name[0] == '.' && e->d_name[1] == '.')
      continue;
#endif
    n = scheme_make_sized_path(e->d_name, nlen, 1);
    elem = scheme_make_pair(n, scheme_null);
    if (last)
      SCHEME_CDR(last) = elem;
    else
      first = elem;
    last = elem;

    counter++;
    if (!(counter & 0xF)) {
      BEGIN_ESCAPEABLE(closedir, dir);
      scheme_thread_block(0);
      END_ESCAPEABLE();
      scheme_current_thread->ran_some = 1;
    }
  }
  
  closedir(dir);

  return first;
#endif
#endif
#endif
}

static Scheme_Object *filesystem_root_list(int argc, Scheme_Object *argv[])
{
  Scheme_Object *first = scheme_null;
#if defined(DOS_FILE_SYSTEM) || defined(USE_MAC_FILE_TOOLBOX)
  Scheme_Object *last = NULL, *v;
#endif
#ifdef USE_MAC_FILE_TOOLBOX
  HParamBlockRec rec;
  int i;
#endif

  scheme_security_check_file("filesystem-root-list", NULL, SCHEME_GUARD_FILE_EXISTS);

#ifdef UNIX_FILE_SYSTEM 
  first = scheme_make_pair(scheme_make_path("/"), scheme_null);
#endif
#ifdef DOS_FILE_SYSTEM
  {
#   define DRIVE_BUF_SIZE 1024
    char drives[DRIVE_BUF_SIZE], *s;
    long len, ds;
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
#ifdef USE_MAC_FILE_TOOLBOX
  i = 1;
  while (1) {
    Str255 name;
    Scheme_Object *v;
    
    rec.volumeParam.ioVolIndex = i;
    rec.volumeParam.ioNamePtr = name;
    
    if (PBHGetVInfo(&rec, 0))
      break;
    
    name[name[0] + 1] = ':';
    v = scheme_make_pair(scheme_make_sized_offset_path((char *)name, 1, 
						       name[0] + 1, 1), 
    	                 scheme_null);
    if (last)
      SCHEME_CDR(last) = v;
    else
      first = v;
    last = v;
    i++;
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

# ifdef USE_MAC_FILE_TOOLBOX	  
  FSSpec spec;
  Scheme_Object *bs;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("make-directory", SCHEME_PATH_STRING_STR, 0, argc, argv);

  bs = TO_PATH(argv[0]);

  filename = SCHEME_PATH_VAL(argv[0]);
  if (has_null(filename, SCHEME_PATH_LEN(argv[0])))
    raise_null_error("make-directory", argv[0], "");

  scheme_security_check_file("make-directory", filename, SCHEME_GUARD_FILE_WRITE);

  if (find_mac_file(filename, 0, &spec, 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0)) {
    SInt32 created;
    errno = FSpDirCreate(&spec, smSystemScript, &created);
    if (!created)
      errno = dupFNErr;
    if (!errno)
      return scheme_void;
    exists_already = (errno == dupFNErr);
  }
# define MKDIR_EXN_TYPE "%E"
# else
  int len, copied;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("make-directory", SCHEME_PATH_STRING_STR, 0, argc, argv);

  filename = scheme_expand_string_filename(argv[0],
					   "make-directory",
					   &copied,
					   SCHEME_GUARD_FILE_WRITE);
  
  /* Make sure path doesn't have trailing separator: */
  len = strlen(filename);
  while (len && IS_A_SEP(filename[len - 1])) {
    if (!copied) {
      filename = scheme_strdup(filename);
      copied = 1;
    }
    filename[--len] = 0;
  }


  while (1) {
    if (!MSC_W_IZE(mkdir)(MSC_WIDE_PATH(filename)
#  ifndef MKDIR_NO_MODE_FLAG
			  , 0xFFFF
# endif
			  ))
      return scheme_void;
    else if (errno != EINTR)
      break;
  }

  exists_already = (errno == EEXIST);
# define MKDIR_EXN_TYPE "%e"
# endif

  scheme_raise_exn(exists_already ? MZEXN_FAIL_FILESYSTEM_EXISTS : MZEXN_FAIL_FILESYSTEM,
		   "make-directory: cannot make directory: %q (" MKDIR_EXN_TYPE ")",
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

# ifdef USE_MAC_FILE_TOOLBOX	  
  FSSpec spec;
  Scheme_Object *bs;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("delete-directory", SCHEME_PATH_STRING_STR, 0, argc, argv);

  bs = TO_PATH(argv[0]);

  filename = SCHEME_PATH_VAL(bs);
  if (has_null(filename, SCHEME_PATH_LEN(bs)))
    raise_null_error("delete-directory", argv[0], "");
  
  scheme_security_check_file("delete-directory", filename, SCHEME_GUARD_FILE_DELETE);

  if (find_mac_file(filename, 0, &spec, 1, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0)) {
    errno = FSpDelete(&spec);
    if (!errno)
      return scheme_void;
  }  
# else
  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("delete-directory", SCHEME_PATH_STRING_STR, 0, argc, argv);

  filename = scheme_expand_string_filename(argv[0],
					   "delete-directory",
					   NULL,
					   SCHEME_GUARD_FILE_DELETE);

  while (1) {
    if (!MSC_W_IZE(rmdir)(MSC_WIDE_PATH(filename)))
      return scheme_void;
#  ifdef DOS_FILE_SYSTEM
    else if ((errno == EACCES) && !tried_cwd) {
      /* Maybe we're using the target directory. Try a real setcwd. */
      Scheme_Object *tcd;
      tcd = scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_DIRECTORY);
      scheme_os_setcwd(SCHEME_PATH_VAL(tcd), 0);
      tried_cwd = 1;
    }
#  endif
    else if (errno != EINTR)
      break;
  }
# endif

  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		   "delete-directory: cannot delete directory: %q (%e)",
		   filename_for_error(argv[0]),
		   errno);
  return NULL;
#endif
}

static Scheme_Object *make_link(int argc, Scheme_Object *argv[])
{
  char *src, *dest;
  int copied;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("make-file-or-directory-link", SCHEME_PATH_STRING_STR, 0, argc, argv);
  if (!SCHEME_PATH_STRINGP(argv[1]))
    scheme_wrong_type("make-file-or-directory-link", SCHEME_PATH_STRING_STR, 0, argc, argv);

  dest = scheme_expand_string_filename(argv[0],
				       "make-file-or-directory-link",
				       &copied,
				       SCHEME_GUARD_FILE_EXISTS);
  src = scheme_expand_string_filename(argv[1],
				      "make-file-or-directory-link",
				      &copied,
				      SCHEME_GUARD_FILE_WRITE);

#if defined(USE_MAC_FILE_TOOLBOX) || defined(DOS_FILE_SYSTEM)
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "make-file-or-directory-link: link creation not supported on this platform; "
		   "cannot create link: %Q",
		   argv[1]);
#else
  while (1) {
    if (!MSC_W_IZE(symlink)(MSC_WIDE_PATH(dest), src))
      return scheme_void;
    else if (errno != EINTR)
      break;
  }

  scheme_raise_exn((errno == EEXIST) ? MZEXN_FAIL_FILESYSTEM_EXISTS : MZEXN_FAIL_FILESYSTEM,
		   "make-file-or-directory-link: cannot make link: %q (%e)",
		   filename_for_error(argv[1]),
		   errno);
#endif

  return NULL;
}

static Scheme_Object *file_modify_seconds(int argc, Scheme_Object **argv)
{
  char *file;
  int set_time = 0;
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec spec;
  long mtime;
  int exists;
  Scheme_Object *bs;
#else
  UNBUNDLE_TIME_TYPE mtime;
  struct MSC_IZE(stat) buf;
#endif

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("file-or-directory-modify-seconds", SCHEME_PATH_STRING_STR, 0, argc, argv);

  set_time = ((argc > 1) && SCHEME_TRUEP(argv[1]));

#ifdef USE_MAC_FILE_TOOLBOX	  
  bs = TO_PATH(argv[0]);
  file = SCHEME_PATH_VAL(bs);
  if (has_null(file, SCHEME_PATH_LEN(bs)))
    raise_null_error("file-or-directory-modify-seconds", argv[0], "");
#else
  file = scheme_expand_string_filename(argv[0],
				       "file-or-directory-modify-seconds",
				       NULL,
				       (set_time
					? SCHEME_GUARD_FILE_WRITE
					: SCHEME_GUARD_FILE_READ));
#endif
  
  if (set_time) {
    if (!SCHEME_INTP(argv[1]) && !SCHEME_BIGNUMP(argv[1])) {
      scheme_wrong_type("file-or-directory-modify-seconds", "exact integer or #f", 1, argc, argv);
      return NULL;
    }
    if (!scheme_get_time_val(argv[1], &mtime)) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "file-or-directory-modify-seconds: integer %s is out-of-range",
		       scheme_make_provided_string(argv[1], 0, NULL));
      return NULL;
    }
  } else
    mtime = 0;

  if (argc > 2) {
    scheme_check_proc_arity("file-or-directory-modify-seconds", 0, 2, argc, argv);
  }

#ifdef USE_MAC_FILE_TOOLBOX	  
  scheme_security_check_file("file-or-directory-modify-seconds", file, 
			     (set_time
			      ? SCHEME_GUARD_FILE_READ
			      : SCHEME_GUARD_FILE_WRITE));

  if (!find_mac_file(file, 0, &spec, 0, 0, NULL, NULL, &exists, &mtime, NULL, NULL, NULL, NULL, set_time)
      || !exists) {
    /* Failed */
  } else if (set_time)
    return scheme_void;
  else
    return scheme_make_integer_value_from_time(mtime);
#else
# ifdef DOS_FILE_SYSTEM
  if (!set_time) {
    int len = strlen(file);
    Scheme_Object *secs;

    if (UNC_stat(file, len, NULL, NULL, &secs))
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
#endif

  if (argc > 2) {
    return _scheme_tail_apply(argv[2], 0, NULL);
  }

  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		   "file-or-directory-modify-seconds: error %s file/directory time: %q (%e)",
		   set_time ? "setting" : "getting",
		   filename_for_error(argv[0]),
		   errno);
  return NULL;
}

#if defined(UNIX_FILE_SYSTEM) && !defined(NO_UNIX_USERS)
# define GROUP_CACHE_SIZE 10
typedef struct {
  gid_t gid;
  char set, in;
} Group_Mem_Cache;
static Group_Mem_Cache group_mem_cache[GROUP_CACHE_SIZE];
static int user_in_group(gid_t gid)
{
  struct group *g;
  struct passwd *pw;
  int i, in;

  for (i = 0; i < GROUP_CACHE_SIZE; i++) {
    if (group_mem_cache[i].set && (group_mem_cache[i].gid == gid))
      return group_mem_cache[i].in;
  }

  pw = getpwuid(getuid());
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
    if (!group_mem_cache[i].set) {
      group_mem_cache[i].set = 1;
      group_mem_cache[i].gid = gid;
      group_mem_cache[i].in = in;
    }
  }

  return in;
}
#endif

static Scheme_Object *file_or_dir_permissions(int argc, Scheme_Object *argv[])
{
  Scheme_Object *l = scheme_null;
  char *filename;
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec spec;
  int flags, isdir, exists;
  long type;
  Scheme_Object *bs;
#endif

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("file-or-directory-permissions", SCHEME_PATH_STRING_STR, 0, argc, argv);

#ifdef USE_MAC_FILE_TOOLBOX	  
  bs = TO_PATH(argv[0]);
  filename = SCHEME_PATH_VAL(bs);
  if (has_null(filename, SCHEME_PATH_LEN(bs)))
    raise_null_error("file-or-directory-permissions", argv[0], "");
#else
  filename = scheme_expand_string_filename(argv[0],
					   "file-or-directory-permissions",
					   NULL,
					   SCHEME_GUARD_FILE_READ);
#endif

#ifdef USE_MAC_FILE_TOOLBOX
  scheme_security_check_file("file-or-directory-permissions", filename, SCHEME_GUARD_FILE_READ);

  if (!find_mac_file(filename, 0, &spec, 0, 0, NULL, &isdir, &exists, NULL, &flags, &type, NULL, NULL, 0)
      || !exists)
    return l = NULL;
  else {
    l = scheme_make_pair(read_symbol, l);
    
    if (type == 'APPL')
      l = scheme_make_pair(execute_symbol, l);
    
    if (!(flags & 0x1))
      l = scheme_make_pair(write_symbol, l);
  }
#else
# ifdef NO_STAT_PROC
  return scheme_null;
# else
#  ifdef UNIX_FILE_SYSTEM
  {
    struct stat buf;
    int read, write, execute;

    if (stat(filename, &buf))
      l = NULL;
    else {
#ifndef NO_UNIX_USERS
      if (buf.st_uid == getuid()) {
	read = !!(buf.st_mode & S_IRUSR);
	write = !!(buf.st_mode & S_IWUSR);
	execute = !!(buf.st_mode & S_IXUSR);
      } else if (user_in_group(buf.st_gid)) {
	read = !!(buf.st_mode & S_IRGRP);
	write = !!(buf.st_mode & S_IWGRP);
	execute = !!(buf.st_mode & S_IXGRP);
      } else {
	read = !!(buf.st_mode & S_IROTH);
	write = !!(buf.st_mode & S_IWOTH);
	execute = !!(buf.st_mode & S_IXOTH);
      }
#else
      read = !!(buf.st_mode & (S_IRUSR | S_IRGRP | S_IROTH));
      write = !!(buf.st_mode & (S_IWUSR | S_IWGRP | S_IWOTH));
      execute = !!(buf.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH));
#endif
      
      if (read)
	l = scheme_make_pair(read_symbol, l);
      if (write)
	l = scheme_make_pair(write_symbol, l);
      if (execute)
	l = scheme_make_pair(execute_symbol, l);
    }
  }
#  endif  
#  ifdef DOS_FILE_SYSTEM
  {
    int len = strlen(filename);
    int flags;
    
    if (UNC_stat(filename, len, &flags, NULL, NULL)) {
      if (flags & MZ_UNC_READ)
	l = scheme_make_pair(read_symbol, l);
      if (flags & MZ_UNC_WRITE)
	l = scheme_make_pair(write_symbol, l);
      if (flags & MZ_UNC_EXEC)
	l = scheme_make_pair(execute_symbol, l);
    } else
      l = NULL;
  }
#  endif
# endif
#endif
  
  if (!l) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		     "file-or-directory-permissions: file or directory not found: \"%q\"",
		     filename_for_error(argv[0]));
  }

  return l;
}

static Scheme_Object *file_size(int argc, Scheme_Object *argv[])
{
  char *filename;
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec spec;
  Scheme_Object *bs;
#endif
  unsigned long len = 0;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("file-size", SCHEME_PATH_STRING_STR, 0, argc, argv);

#ifdef USE_MAC_FILE_TOOLBOX	  
  bs = TO_PATH(argv[0]);
  filename = SCHEME_PATH_VAL(bs);
  if (has_null(filename, SCHEME_PATH_LEN(bs)))
    raise_null_error("file-size", argv[0], "");
#else
  filename = scheme_expand_string_filename(argv[0],
					   "file-size",
					   NULL,
					   SCHEME_GUARD_FILE_READ);
#endif

#ifdef USE_MAC_FILE_TOOLBOX
  scheme_security_check_file("file-size", filename, SCHEME_GUARD_FILE_READ);

  if (!find_mac_file(filename, 0, &spec, 0, 1, NULL, NULL, NULL, NULL, NULL, NULL, &len, NULL, 0))
    goto failed;
#endif
#if defined(UNIX_FILE_SYSTEM) || defined(DOS_FILE_SYSTEM)
  {
    struct MSC_IZE(stat) buf;

    while (1) {
      if (!MSC_W_IZE(stat)(MSC_WIDE_PATH(filename), &buf))
	break;
      else if (errno != EINTR)
	goto failed;
    }

    if (S_ISDIR(buf.st_mode))
      goto failed;

    len = buf.st_size;
  }
#endif

  return scheme_make_integer_value_from_unsigned(len);

failed:
  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		   "file-size: file not found: \"%q\"",
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
    ed = do_simplify_path(ed, scheme_null);
# endif

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

static Scheme_Object *collpaths_gen_p(int argc, Scheme_Object **argv, int rel)
{
  Scheme_Object *v = argv[0];

  if (scheme_proper_list_length(v) < 0)
    return NULL;

  if (SCHEME_NULLP(v))
    return v;

  while (SCHEME_PAIRP(v)) {
    Scheme_Object *s;
    s = SCHEME_CAR(v);
    if (!SCHEME_PATH_STRINGP(s))
      return NULL;
    s = TO_PATH(s);
    if (rel && !scheme_is_relative_path(SCHEME_PATH_VAL(s),
					SCHEME_PATH_LEN(s)))
      return NULL;
    if (!rel && !scheme_is_complete_path(SCHEME_PATH_VAL(s),
					 SCHEME_PATH_LEN(s)))
      return NULL;
    v = SCHEME_CDR(v);
  }

  if (!SCHEME_NULLP(v))
    return NULL;

  /* Convert to immutable list of paths: */
  {
    Scheme_Object *last = NULL, *first = NULL, *p, *s;
    v = argv[0];
    while (SCHEME_PAIRP(v)) {
      s = SCHEME_CAR(v);
      s = TO_PATH(s);
      
      p = scheme_make_pair(s, scheme_null);
      SCHEME_SET_PAIR_IMMUTABLE(p);
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
  return collpaths_gen_p(argc, argv, 0);
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
  return collpaths_gen_p(argc, argv, 1);
}

static Scheme_Object *use_compiled_kind(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("use-compiled-file-paths",
			     scheme_make_integer(MZCONFIG_USE_COMPILED_KIND),
			     argc, argv,
			     -1, compiled_kind_p, "list of relative paths and strings", 1);
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
  id_addon_dir
};

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
      REGISTER_SO(exec_cmd);
      exec_cmd = scheme_make_path("mzscheme");
    }
    return exec_cmd;
  } else if (argv[0] == addon_dir_symbol) {
    which = id_addon_dir;
  } else {
    scheme_wrong_type("find-system-path", "system-path-symbol",
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

    if (scheme_directory_exists("/usr/tmp"))
      return scheme_make_path("/usr/tmp");

    if (scheme_directory_exists("/tmp"))
      return scheme_make_path("/tmp");

    return CURRENT_WD();
  }
  
  {
    /* Everything else is in ~: */
    Scheme_Object *home;
    int ends_in_slash;

    if ((which == id_pref_dir) 
	|| (which == id_pref_file)
	|| (which == id_addon_dir)) {
#if defined(OS_X) && !defined(XONX)
      if (which == id_addon_dir)
	home = scheme_make_path(scheme_expand_filename("~/Library/PLT Scheme/", -1, NULL, NULL, 0));
      else
	home = scheme_make_path(scheme_expand_filename("~/Library/Preferences/", -1, NULL, NULL, 0));
#else
      home = scheme_make_path(scheme_expand_filename("~/.plt-scheme/", -1, NULL, NULL, 0));
#endif 
    } else {
#if defined(OS_X) && !defined(XONX)
      if (which == id_desk_dir)
	home = scheme_make_path(scheme_expand_filename("~/Desktop/", 10, NULL, NULL, 0));
      else
#endif
	home = scheme_make_path(scheme_expand_filename("~/", 2, NULL, NULL, 0));
    }
    
    if ((which == id_pref_dir) || (which == id_init_dir) 
	|| (which == id_home_dir) || (which == id_addon_dir)
	|| (which == id_desk_dir) || (which == id_doc_dir))
      return home;

    ends_in_slash = (SCHEME_PATH_VAL(home))[SCHEME_PATH_LEN(home) - 1] == '/';
    
    if (which == id_init_file)
      return append_path(home, scheme_make_path("/.mzschemerc" + ends_in_slash));
    if (which == id_pref_file) {
#if defined(OS_X) && !defined(XONX)
      return append_path(home, scheme_make_path("/org.plt-scheme.prefs.ss" + ends_in_slash));
#else      
      return append_path(home, scheme_make_path("/plt-prefs.ss" + ends_in_slash));
#endif
    }
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
	  || (which == id_pref_file)) 
	which_folder = CSIDL_APPDATA;
      else if (which == id_doc_dir) {
#       ifndef CSIDL_MYDOCUMENTS
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
      home = append_path(home, scheme_make_path("\\PLT Scheme" + ends_in_slash));
      ends_in_slash = 0;
    }

    if (which == id_init_file)
      return append_path(home, scheme_make_path("\\mzschemerc.ss" + ends_in_slash));
    if (which == id_pref_file)
      return append_path(home, scheme_make_path("\\plt-prefs.ss" + ends_in_slash));
    return home;
  }
#endif

#ifdef MAC_FILE_SYSTEM
  {
    OSType	t;
    FSSpec	spec;
    Scheme_Object *home;
    int		ends_in_colon;
    SInt16	vRefNum;
    SInt32	dirID;
    OSErr	err;

    switch (which) {
    case id_doc_dir:
    case id_desk_dir:
      t = 'desk';
      break;
    case id_home_dir:
    case id_pref_dir:
    case id_pref_file:
    case id_init_dir:
    case id_init_file:
    case id_addon_dir:
      t = 'pref';
      break;
    case id_sys_dir:
      t = 'macs';
      break;
    case id_temp_dir:
    default:
      t = 'temp';
      break;
    }

    err = FindFolder(kOnSystemDisk, t, kCreateFolder, &vRefNum, &dirID);
    
    if (err == noErr) {
      FSMakeFSSpec(vRefNum,dirID,NULL,&spec);
      home = scheme_make_path(scheme_mac_spec_to_path(&spec));
    }
    else {
      if (which == id_temp_dir)
	home = CURRENT_WD();
      else {
	/* Everything else uses system current directory if there's no prefs folder */
	home = scheme_make_path(scheme_os_getcwd(NULL, 0, NULL, 1));
	if (!home)
	  /* disaster strikes; use Scheme CWD */
	  home = CURRENT_WD();
      }
    }
  
    if ((which == id_home_dir) 
	|| (which == id_doc_dir) 
	|| (which == id_desk_dir) 
	|| (which == id_temp_dir) 
	|| (which == id_init_dir) 
	|| (which == id_sys_dir))
      return home;
    
    ends_in_colon = (SCHEME_PATH_VAL(home))[SCHEME_PATH_LEN(home) - 1] == ':';

    if ((which == id_addon_dir)
	|| (which == id_pref_dir)
	|| (which == id_pref_file))
      home = append_path(home, scheme_make_path(":PLT Scheme AddOns" + ends_in_colon));
      ends_in_colon = 0;
    }

    if (which == id_init_file)
      return append_path(home, scheme_make_path(":mzschemerc.ss" + ends_in_colon));
    if (which == id_pref_file)
      return append_path(home, scheme_make_path(":org.plt-scheme.prefs.ss" + ends_in_colon));

    return home;
  }
#endif

  /* Something went wrong if we get here. */
  return scheme_void;
}

#endif

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

char *scheme_get_exec_path(void)
{
  if (exec_cmd)
    return SCHEME_PATH_VAL(exec_cmd);
  else
    return NULL;
}

/********************************************************************************/

#ifdef MAC_CLASSIC_PROCESS_CONTROL

static long check_four(char *name, int which, int argc, Scheme_Object **argv)
{
  Scheme_Object *o = argv[which];
  
  if (!SCHEME_BYTE_STRINGP(o))
    scheme_wrong_type(name, "MacOS type/creator 4-character byte string", which, argc, argv);

  if (SCHEME_BYTE_STRTAG_VAL(o) != 4) {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: string is not a 4-character type or creator signature: %V",
		     name,
		     o);
  }
  
  return *(long *)SCHEME_BYTE_STR_VAL(o);
}

static int appl_name_to_spec(char *name, int find_path, Scheme_Object *o, FSSpec *spec)
{
  if (find_path) {
    HVolumeParam volPB;
    HIOParam paramPB;
    GetVolParmsInfoBuffer volinfo;
    DTPBRec rec;
    Str255 nm;
    short vrefnum;
    long junk;
    long creator = check_four(name, 0, 1, &o);

    /* try current volume: */
    scheme_os_setcwd(SCHEME_PATH_VAL(scheme_get_param(scheme_current_config(), 
						      MZCONFIG_CURRENT_DIRECTORY)),
		     0);
    if (HGetVol(nm, &vrefnum, &junk) == noErr) {
      rec.ioNamePtr = NULL;
      rec.ioVRefNum = vrefnum;
      
      if (PBDTGetPath(&rec)) {
	rec.ioIndex = 0;
	rec.ioNamePtr = nm;
	rec.ioFileCreator = creator;

	if (PBDTGetAPPL(&rec, 0)) {
	  memcpy(spec->name, nm, 32);
	  spec->vRefNum = vrefnum;
	  spec->parID = rec.ioAPPLParID;
	  
	  return 1;
	}
      }
    }

    volPB.ioNamePtr = NULL;
    paramPB.ioNamePtr = NULL;
    paramPB.ioBuffer = (Ptr)&volinfo;
    paramPB.ioReqCount = sizeof(volinfo);

    /* Loop over all volumes: */
    for (volPB.ioVolIndex = 1; PBHGetVInfoSync ((HParmBlkPtr)&volPB) == noErr; volPB.ioVolIndex++) {
      /* Call PBHGetVolParms call to ensure the volume is a local volume. */
      paramPB.ioVRefNum = volPB.ioVRefNum;

      if (PBHGetVolParmsSync ((HParmBlkPtr)&paramPB) == noErr && volinfo.vMServerAdr == 0) {
	rec.ioNamePtr = NULL;
	rec.ioVRefNum = volPB.ioVRefNum;
	
	if (PBDTGetPath(&rec))
	  break;

	rec.ioIndex = 0;
	rec.ioNamePtr = nm;
	rec.ioFileCreator = creator;

	if (PBDTGetAPPL(&rec, 0))
	  break;
      
	memcpy(spec->name, nm, 32);
	spec->vRefNum = vrefnum;
	spec->parID = rec.ioAPPLParID;

	return 1;
      }
    }
    return 0;
  } else {
    char *s;
    
    if (!SCHEME_PATH_STRINGP(o))
      scheme_wrong_type(name, SCHEME_PATH_STRING_STR, 0, 1, &o);
  
    s = scheme_expand_string_filename(o,
				      name,
				      NULL,
				      0);

    if (!find_mac_file(s, 0, spec, 0, 1, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0))
      return 0;
  }
  
  return 1;
}

int scheme_mac_start_app(char *name, int find_path, Scheme_Object *o)
{
  FSSpec spec;
  LaunchParamBlockRec rec;

  if (!appl_name_to_spec(name, find_path, o, &spec))
    return 0;

  rec.launchBlockID = extendedBlock;
  rec.launchEPBLength = extendedBlockLen;
  rec.launchFileFlags = 0;
  rec.launchControlFlags = launchContinue | launchNoFileFlags;
  rec.launchAppSpec = &spec;
  rec.launchAppParameters = NULL;

  return !LaunchApplication(&rec);
}

#endif
