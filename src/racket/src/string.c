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

#include "schpriv.h"
#include "schvers.h"
#include <string.h>
#include <ctype.h>
#ifndef DONT_USE_LOCALE
# include <locale.h>
# ifdef MZ_NO_ICONV
#  define USE_ICONV_DLL
# endif
# ifndef USE_ICONV_DLL
#  include <iconv.h>
#  include <langinfo.h>
# endif
# include <wchar.h>
# include <wctype.h>
# include <errno.h>
# ifdef MACOS_UNICODE_SUPPORT
#  include <CoreFoundation/CFString.h>
#  include <CoreFoundation/CFLocale.h>
# endif
# ifdef WINDOWS_UNICODE_SUPPORT
#  include <windows.h>
# endif
#endif

#ifndef SCHEME_PLATFORM_LIBRARY_SUBPATH
# include "schsys.h"
#endif

#include "schustr.inc"

#ifdef USE_ICONV_DLL
typedef intptr_t iconv_t;
typedef int *(*errno_proc_t)();
typedef size_t (*iconv_proc_t)(iconv_t cd,
			       char **inbuf, size_t *inbytesleft,
			       char **outbuf, size_t *outbytesleft);
typedef iconv_t (*iconv_open_proc_t)(const char *tocode, const char *fromcode);
typedef void (*iconv_close_proc_t)(iconv_t cd);
typedef char *(*locale_charset_proc_t)();
static errno_proc_t iconv_errno;
static iconv_proc_t iconv;
static iconv_open_proc_t iconv_open;
static iconv_close_proc_t iconv_close;
static locale_charset_proc_t locale_charset; /* Not used, currently */
#define mzCHK_PROC(x) x
static int get_iconv_errno(void)
{
  int *a;
  a = iconv_errno();
  return *a;
}
# undef HAVE_CODESET
# define HAVE_CODESET 1
# define CODESET 0
# define ICONV_errno get_iconv_errno()
extern wchar_t *scheme_get_dll_path(wchar_t *s);
static int iconv_ready = 0;
static void init_iconv()
{
# ifdef MZ_NO_ICONV
# else
  HMODULE m;
  m = LoadLibraryW(scheme_get_dll_path(L"iconv.dll"));
  if (!m)
    m = LoadLibraryW(scheme_get_dll_path(L"libiconv.dll"));
  if (!m)
    m = LoadLibraryW(scheme_get_dll_path(L"libiconv-2.dll"));
  if (!m)
    m = LoadLibrary("iconv.dll");
  if (!m)
    m = LoadLibrary("libiconv.dll");
  if (!m)
    m = LoadLibrary("libiconv-2.dll");
  if (m) {
    iconv = (iconv_proc_t)GetProcAddress(m, "libiconv");
    iconv_open = (iconv_open_proc_t)GetProcAddress(m, "libiconv_open");
    iconv_close = (iconv_close_proc_t)GetProcAddress(m, "libiconv_close");
    locale_charset = (locale_charset_proc_t)GetProcAddress(m, "locale_charset");
    /* Make sure we have all of them or none: */
    if (!iconv || !iconv_open || !iconv_close) {
      iconv = NULL;
      iconv_open = NULL;
      iconv_close = NULL;
    }
  }
  if (iconv) {
    iconv_errno = (errno_proc_t)GetProcAddress(m, "_errno");
    if (!iconv_errno) {
      /* The iconv.dll distributed with Racket links to msvcrt.dll.
	 It's a slighly dangerous assumption that whatever iconv we
	 found also uses msvcrt.dll. */
      m = LoadLibrary("msvcrt.dll");
      if (m) {
	iconv_errno = (errno_proc_t)GetProcAddress(m, "_errno");
	if (!iconv_errno) {
	  iconv = NULL;
	  iconv_open = NULL;
	  iconv_close = NULL;
	}
      }
    }
  }
# endif
  iconv_ready = 1;
}
#else
# define ICONV_errno errno
# define iconv_ready 1
# define mzCHK_PROC(x) 1
static void init_iconv() { }
#endif

#ifdef MACOS_UNICODE_SUPPORT
# define mzLOCALE_IS_UTF_8(s) (!s || !(*s))
#endif
#ifdef WINDOWS_UNICODE_SUPPORT
# define mzLOCALE_IS_UTF_8(s) (!s || !(*s))
#endif
#ifndef mzLOCALE_IS_UTF_8
# define mzLOCALE_IS_UTF_8(s) !mzCHK_PROC(iconv_open)
#endif

#define mzICONV_KIND 0
#define mzUTF8_KIND 1
#define mzUTF8_TO_UTF16_KIND 2
#define mzUTF16_TO_UTF8_KIND 3

typedef struct Scheme_Converter {
  Scheme_Object so;
  short closed;
  short kind;
  iconv_t cd;
  int permissive;
  Scheme_Custodian_Reference *mref;
} Scheme_Converter;

/* locals */

/* These two locale variables are only valid when reset_locale()
   is called after continuation marks (and hence parameterization)
   may have changed. Similarly, setlocale() is only up-to-date
   when reset_locale() has been called. */
THREAD_LOCAL_DECL(static int locale_on);
THREAD_LOCAL_DECL(static void *current_locale_name_ptr);
static void reset_locale(void);

#define current_locale_name ((const mzchar *)current_locale_name_ptr)

#ifdef USE_ICONV_DLL
static char *nl_langinfo(int which)
{
  int i;

  reset_locale();
  if (!current_locale_name)
    current_locale_name_ptr = "\0\0\0\0";

  if ((current_locale_name[0] == 'C')
      && !current_locale_name[1])
    return "US-ASCII";

  for (i = 0; current_locale_name[i]; i++) {
    if (current_locale_name[i] == '.') {
      if (current_locale_name[i + 1]) {
	int len, j = 0;
	char *enc;
	i++;
	len = scheme_char_strlen(current_locale_name) - i;
	enc = (char *)scheme_malloc_atomic(len + 1);
	while (current_locale_name[i]) {
	  if (current_locale_name[i] > 127)
	    return "UTF-8";
	  enc[j++] = current_locale_name[i++];
	}
	enc[j] = 0;
	return enc;
      }
    }
  }

  return "UTF-8";
}
#endif

#ifdef DONT_USE_LOCALE
# define mz_iconv_nl_langinfo() ""
#else
static char *mz_iconv_nl_langinfo(){
  char *s;
# if HAVE_CODESET
  s = nl_langinfo(CODESET);
# else
  s = NULL;
# endif
  if (!s)
    return "";
  else
    return s;
}
#endif

static Scheme_Object *make_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_ci_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_ci_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_ci_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_upcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_downcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_titlecase (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_foldcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_upcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_downcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *substring (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_append (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_list (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_to_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_copy (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_copy_bang (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_fill (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_immutable (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_normalize_c (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_normalize_kc (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_normalize_d (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_normalize_kd (int argc, Scheme_Object *argv[]);

static Scheme_Object *make_shared_byte_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *shared_byte_string (int argc, Scheme_Object *argv[]);

static Scheme_Object *make_byte_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_substring (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_append (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_to_list (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_to_byte_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_copy (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_copy_bang (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_fill (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_to_immutable (int argc, Scheme_Object *argv[]);

static Scheme_Object *byte_string_utf8_index (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_utf8_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_utf8_length (int argc, Scheme_Object *argv[]);

static Scheme_Object *byte_string_to_char_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_to_char_string_locale (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_to_char_string_latin1 (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_string_to_byte_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_string_to_byte_string_locale (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_string_to_byte_string_latin1 (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_string_utf8_length (int argc, Scheme_Object *argv[]);

static Scheme_Object *version(int argc, Scheme_Object *argv[]);
static Scheme_Object *format(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_printf(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_eprintf(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_fprintf(int argc, Scheme_Object *argv[]);
static Scheme_Object *banner(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_getenv(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_putenv(int argc, Scheme_Object *argv[]);
static Scheme_Object *system_type(int argc, Scheme_Object *argv[]);
static Scheme_Object *system_library_subpath(int argc, Scheme_Object *argv[]);
static Scheme_Object *cmdline_args(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_locale(int argc, Scheme_Object *argv[]);
static Scheme_Object *locale_string_encoding(int argc, Scheme_Object *argv[]);
static Scheme_Object *system_language_country(int argc, Scheme_Object *argv[]);

static Scheme_Object *byte_string_open_converter(int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_close_converter(int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_convert(int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_convert_end(int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_converter_p(int argc, Scheme_Object *argv[]);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static int mz_char_strcmp(const char *who, const mzchar *str1, intptr_t l1, const mzchar *str2, intptr_t l2, int locale, int size_shortcut);
static int mz_char_strcmp_ci(const char *who, const mzchar *str1, intptr_t l1, const mzchar *str2, intptr_t l2, int locale, int size_shortcut);
static int mz_strcmp(const char *who, unsigned char *str1, intptr_t l1, unsigned char *str2, intptr_t l2);

XFORM_NONGCING static intptr_t utf8_decode_x(const unsigned char *s, intptr_t start, intptr_t end,
					unsigned int *us, intptr_t dstart, intptr_t dend,
					intptr_t *ipos, intptr_t *jpos,
					char compact, char utf16,
					int *state, int might_continue, int permissive);
XFORM_NONGCING static intptr_t utf8_encode_x(const unsigned int *us, intptr_t start, intptr_t end,
					unsigned char *s, intptr_t dstart, intptr_t dend,
					intptr_t *_ipos, intptr_t *_opos, char utf16);

static char *string_to_from_locale(int to_bytes,
				   char *in, intptr_t delta, intptr_t len,
				   intptr_t *olen, int perm,
				   int *no_cvt);

#define portable_isspace(x) (((x) < 128) && isspace(x))

ROSYM static Scheme_Object *sys_symbol;
ROSYM static Scheme_Object *platform_3m_path, *platform_cgc_path;
READ_ONLY static Scheme_Object *zero_length_char_string;
READ_ONLY static Scheme_Object *zero_length_byte_string;

SHARED_OK static Scheme_Hash_Table *putenv_str_table;

SHARED_OK static char *embedding_banner;
SHARED_OK static Scheme_Object *vers_str;
SHARED_OK static Scheme_Object *banner_str;

READ_ONLY static Scheme_Object *complete_symbol, *continues_symbol, *aborts_symbol, *error_symbol;

void
scheme_init_string (Scheme_Env *env)
{
  Scheme_Object *p;

  REGISTER_SO(sys_symbol);
  sys_symbol = scheme_intern_symbol(SYSTEM_TYPE_NAME);

  REGISTER_SO(zero_length_char_string);
  REGISTER_SO(zero_length_byte_string);
  zero_length_char_string = scheme_alloc_char_string(0, 0);
  zero_length_byte_string = scheme_alloc_byte_string(0, 0);

  REGISTER_SO(complete_symbol);
  REGISTER_SO(continues_symbol);
  REGISTER_SO(aborts_symbol);
  REGISTER_SO(error_symbol);
  complete_symbol = scheme_intern_symbol("complete");
  continues_symbol = scheme_intern_symbol("continues");
  aborts_symbol = scheme_intern_symbol("aborts");
  error_symbol = scheme_intern_symbol("error");

  REGISTER_SO(platform_3m_path);
#ifdef UNIX_FILE_SYSTEM
# define MZ3M_SUBDIR "/3m"
#else
# ifdef DOS_FILE_SYSTEM
#  define MZ3M_SUBDIR "\\3m"
# else
#  define MZ3M_SUBDIR ":3m"
# endif
#endif
  REGISTER_SO(platform_3m_path);
  REGISTER_SO(platform_cgc_path);
  platform_cgc_path = scheme_make_path(SCHEME_PLATFORM_LIBRARY_SUBPATH);
  platform_3m_path = scheme_make_path(SCHEME_PLATFORM_LIBRARY_SUBPATH MZ3M_SUBDIR);

  REGISTER_SO(putenv_str_table);

  REGISTER_SO(embedding_banner);
  REGISTER_SO(vers_str);
  REGISTER_SO(banner_str);

  vers_str = scheme_make_utf8_string(scheme_version());
  SCHEME_SET_CHAR_STRING_IMMUTABLE(vers_str);
  banner_str = scheme_make_utf8_string(scheme_banner());
  SCHEME_SET_CHAR_STRING_IMMUTABLE(banner_str);

  p = scheme_make_folding_prim(string_p, "string?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_add_global_constant("string?", p, env);

  scheme_add_global_constant("make-string",
			     scheme_make_immed_prim(make_string,
						    "make-string",
						    1, 2),
			     env);
  scheme_add_global_constant("string",
			     scheme_make_immed_prim(string,
						    "string",
						    0, -1),
			     env);
  
  p = scheme_make_folding_prim(string_length, "string-length", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("string-length", p,
			     env);

  p = scheme_make_immed_prim(scheme_checked_string_ref, "string-ref", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_add_global_constant("string-ref", p, env);


  p = scheme_make_immed_prim(scheme_checked_string_set, "string-set!", 3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_add_global_constant("string-set!", p, env);

  scheme_add_global_constant("string=?",
			     scheme_make_immed_prim(string_eq,
						    "string=?",
						    2, -1),
			     env);
  scheme_add_global_constant("string-locale=?",
			     scheme_make_immed_prim(string_locale_eq,
						    "string-locale=?",
						    2, -1),
			     env);
  scheme_add_global_constant("string-ci=?",
			     scheme_make_immed_prim(string_ci_eq,
						    "string-ci=?",
						    2, -1),
			     env);
  scheme_add_global_constant("string-locale-ci=?",
			     scheme_make_immed_prim(string_locale_ci_eq,
						    "string-locale-ci=?",
						    2, -1),
			     env);
  scheme_add_global_constant("string<?",
			     scheme_make_immed_prim(string_lt,
						    "string<?",
						    2, -1),
			     env);
  scheme_add_global_constant("string-locale<?",
			     scheme_make_immed_prim(string_locale_lt,
						    "string-locale<?",
						    2, -1),
			     env);
  scheme_add_global_constant("string>?",
			     scheme_make_immed_prim(string_gt,
						    "string>?",
						    2, -1),
			     env);
  scheme_add_global_constant("string-locale>?",
			     scheme_make_immed_prim(string_locale_gt,
						    "string-locale>?",
						    2, -1),
			     env);
  scheme_add_global_constant("string<=?",
			     scheme_make_immed_prim(string_lt_eq,
						    "string<=?",
						    2, -1),
			     env);
  scheme_add_global_constant("string>=?",
			     scheme_make_immed_prim(string_gt_eq,
						    "string>=?",
						    2, -1),
			     env);
  scheme_add_global_constant("string-ci<?",
			     scheme_make_immed_prim(string_ci_lt,
						    "string-ci<?",
						    2, -1),
			     env);
  scheme_add_global_constant("string-locale-ci<?",
			     scheme_make_immed_prim(string_locale_ci_lt,
						    "string-locale-ci<?",
						    2, -1),
			     env);
  scheme_add_global_constant("string-ci>?",
			     scheme_make_immed_prim(string_ci_gt,
						    "string-ci>?",
						    2, -1),
			     env);
  scheme_add_global_constant("string-locale-ci>?",
			     scheme_make_immed_prim(string_locale_ci_gt,
						    "string-locale-ci>?",
						    2, -1),
			     env);
  scheme_add_global_constant("string-ci<=?",
			     scheme_make_immed_prim(string_ci_lt_eq,
						    "string-ci<=?",
						    2, -1),
			     env);
  scheme_add_global_constant("string-ci>=?",
			     scheme_make_immed_prim(string_ci_gt_eq,
						    "string-ci>=?",
						    2, -1),
			     env);

  scheme_add_global_constant("substring",
			     scheme_make_immed_prim(substring,
						    "substring",
						    2, 3),
			     env);
  scheme_add_global_constant("string-append",
			     scheme_make_immed_prim(string_append,
						    "string-append",
						    0, -1),
			     env);
  scheme_add_global_constant("string->list",
			     scheme_make_immed_prim(string_to_list,
						    "string->list",
						    1, 1),
			     env);
  scheme_add_global_constant("list->string",
			     scheme_make_immed_prim(list_to_string,
						    "list->string",
						    1, 1),
			     env);
  scheme_add_global_constant("string-copy",
			     scheme_make_immed_prim(string_copy,
						    "string-copy",
						    1, 1),
			     env);
  scheme_add_global_constant("string-copy!",
			     scheme_make_immed_prim(string_copy_bang,
						    "string-copy!",
						    3, 5),
			     env);
  scheme_add_global_constant("string-fill!",
			     scheme_make_immed_prim(string_fill,
						    "string-fill!",
						    2, 2),
			     env);
  scheme_add_global_constant("string->immutable-string",
			     scheme_make_immed_prim(string_to_immutable,
						    "string->immutable-string",
						    1, 1),
			     env);
  scheme_add_global_constant("string-normalize-nfc",
			     scheme_make_immed_prim(string_normalize_c,
						    "string-normalize-nfc",
						    1, 1),
			     env);
  scheme_add_global_constant("string-normalize-nfkc",
			     scheme_make_immed_prim(string_normalize_kc,
						    "string-normalize-nfkc",
						    1, 1),
			     env);
  scheme_add_global_constant("string-normalize-nfd",
			     scheme_make_immed_prim(string_normalize_d,
						    "string-normalize-nfd",
						    1, 1),
			     env);
  scheme_add_global_constant("string-normalize-nfkd",
			     scheme_make_immed_prim(string_normalize_kd,
						    "string-normalize-nfkd",
						    1, 1),
			     env);

  scheme_add_global_constant("string-upcase",
			     scheme_make_immed_prim(string_upcase,
						    "string-upcase",
						    1, 1),
			     env);
  scheme_add_global_constant("string-downcase",
			     scheme_make_immed_prim(string_downcase,
						    "string-downcase",
						    1, 1),
			     env);
  scheme_add_global_constant("string-titlecase",
			     scheme_make_immed_prim(string_titlecase,
						    "string-titlecase",
						    1, 1),
			     env);
  scheme_add_global_constant("string-foldcase",
			     scheme_make_immed_prim(string_foldcase,
						    "string-foldcase",
						    1, 1),
			     env);

  scheme_add_global_constant("string-locale-upcase",
			     scheme_make_immed_prim(string_locale_upcase,
						    "string-locale-upcase",
						    1, 1),
			     env);
  scheme_add_global_constant("string-locale-downcase",
			     scheme_make_immed_prim(string_locale_downcase,
						    "string-locale-downcase",
						    1, 1),
			     env);

  scheme_add_global_constant("current-locale",
			     scheme_register_parameter(current_locale,
						       "current-locale",
						       MZCONFIG_LOCALE),
			     env);
  scheme_add_global_constant("locale-string-encoding",
			     scheme_make_immed_prim(locale_string_encoding,
						    "locale-string-encoding",
						    0, 0),
			     env);
  scheme_add_global_constant("system-language+country",
			     scheme_make_immed_prim(system_language_country,
						    "system-language+country",
						    0, 0),
			     env);

  scheme_add_global_constant("bytes-converter?",
			     scheme_make_immed_prim(byte_converter_p,
						    "bytes-converter?",
						    1, 1),
			     env);
  scheme_add_global_constant("bytes-convert",
			     scheme_make_prim_w_arity2(byte_string_convert,
						       "bytes-convert",
						       1, 7,
						       3, 3),
			     env);
  scheme_add_global_constant("bytes-convert-end",
			     scheme_make_prim_w_arity2(byte_string_convert_end,
						       "bytes-convert-end",
						       0, 3,
						       2, 2),
			     env);
  scheme_add_global_constant("bytes-open-converter",
			     scheme_make_immed_prim(byte_string_open_converter,
						    "bytes-open-converter",
						    2, 2),
			     env);
  scheme_add_global_constant("bytes-close-converter",
			     scheme_make_immed_prim(byte_string_close_converter,
						    "bytes-close-converter",
						    1, 1),
			     env);

  scheme_add_global_constant("format",
			     scheme_make_noncm_prim(format,
                                                    "format",
                                                    1, -1),
			     env);
  scheme_add_global_constant("printf",
			     scheme_make_noncm_prim(sch_printf,
                                                    "printf",
                                                    1, -1),
			     env);
  scheme_add_global_constant("eprintf",
			     scheme_make_noncm_prim(sch_eprintf,
                                                    "eprintf",
                                                    1, -1),
			     env);
  scheme_add_global_constant("fprintf",
			     scheme_make_noncm_prim(sch_fprintf,
                                                    "fprintf",
                                                    2, -1),
			     env);

  scheme_add_global_constant("byte?",
			     scheme_make_folding_prim(byte_p,
						      "byte?",
						      1, 1, 1),
			     env);

  p = scheme_make_folding_prim(byte_string_p, "bytes?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_add_global_constant("bytes?", p, env);

  scheme_add_global_constant("make-bytes",
			     scheme_make_immed_prim(make_byte_string,
						    "make-bytes",
						    1, 2),
			     env);
  scheme_add_global_constant("bytes",
			     scheme_make_immed_prim(byte_string,
						    "bytes",
						    0, -1),
			     env);

  GLOBAL_PRIM_W_ARITY("make-shared-bytes", make_shared_byte_string, 1, 2, env);
  GLOBAL_PRIM_W_ARITY("shared-bytes", shared_byte_string, 0, -1, env);

  p = scheme_make_folding_prim(byte_string_length, "bytes-length", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("bytes-length", p, env);

  p = scheme_make_immed_prim(scheme_checked_byte_string_ref, "bytes-ref", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("bytes-ref", p, env);

  p = scheme_make_immed_prim(scheme_checked_byte_string_set, "bytes-set!", 3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_add_global_constant("bytes-set!", p, env);

  scheme_add_global_constant("bytes=?",
			     scheme_make_immed_prim(byte_string_eq,
						    "bytes=?",
						    2, -1),
			     env);
  scheme_add_global_constant("bytes<?",
			     scheme_make_immed_prim(byte_string_lt,
						    "bytes<?",
						    2, -1),
			     env);
  scheme_add_global_constant("bytes>?",
			     scheme_make_immed_prim(byte_string_gt,
						    "bytes>?",
						    2, -1),
			     env);

  scheme_add_global_constant("subbytes",
			     scheme_make_immed_prim(byte_substring,
						    "subbytes",
						    2, 3),
			     env);
  scheme_add_global_constant("bytes-append",
			     scheme_make_immed_prim(byte_string_append,
						    "bytes-append",
						    0, -1),
			     env);
  scheme_add_global_constant("bytes->list",
			     scheme_make_immed_prim(byte_string_to_list,
						    "bytes->list",
						    1, 1),
			     env);
  scheme_add_global_constant("list->bytes",
			     scheme_make_immed_prim(list_to_byte_string,
						    "list->bytes",
						    1, 1),
			     env);
  scheme_add_global_constant("bytes-copy",
			     scheme_make_immed_prim(byte_string_copy,
						    "bytes-copy",
						    1, 1),
			     env);
  scheme_add_global_constant("bytes-copy!",
			     scheme_make_immed_prim(byte_string_copy_bang,
						    "bytes-copy!",
						    3, 5),
			     env);
  scheme_add_global_constant("bytes-fill!",
			     scheme_make_immed_prim(byte_string_fill,
						    "bytes-fill!",
						    2, 2),
			     env);
  scheme_add_global_constant("bytes->immutable-bytes",
			     scheme_make_immed_prim(byte_string_to_immutable,
						    "bytes->immutable-bytes",
						    1, 1),
			     env);

  p = scheme_make_immed_prim(byte_string_utf8_index, "bytes-utf-8-index", 2, 4);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("bytes-utf-8-index", p, env);

  p = scheme_make_immed_prim(byte_string_utf8_length, "bytes-utf-8-length", 1, 4);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("bytes-utf-8-length", p, env);

  scheme_add_global_constant("bytes-utf-8-ref",
			     scheme_make_immed_prim(byte_string_utf8_ref,
						    "bytes-utf-8-ref",
						    2, 4),
			     env);

  scheme_add_global_constant("bytes->string/utf-8",
			     scheme_make_immed_prim(byte_string_to_char_string,
						    "bytes->string/utf-8",
						    1, 4),
			     env);
  scheme_add_global_constant("bytes->string/locale",
			     scheme_make_immed_prim(byte_string_to_char_string_locale,
						    "bytes->string/locale",
						    1, 4),
			     env);
  scheme_add_global_constant("bytes->string/latin-1",
			     scheme_make_immed_prim(byte_string_to_char_string_latin1,
						    "bytes->string/latin-1",
						    1, 4),
			     env);
  scheme_add_global_constant("string->bytes/utf-8",
			     scheme_make_immed_prim(char_string_to_byte_string,
						    "string->bytes/utf-8",
						    1, 4),
			     env);
  scheme_add_global_constant("string->bytes/locale",
			     scheme_make_immed_prim(char_string_to_byte_string_locale,
						    "string->bytes/locale",
						    1, 4),
			     env);
  scheme_add_global_constant("string->bytes/latin-1",
			     scheme_make_immed_prim(char_string_to_byte_string_latin1,
						    "string->bytes/latin-1",
						    1, 4),
			     env);

  scheme_add_global_constant("string-utf-8-length",
			     scheme_make_immed_prim(char_string_utf8_length,
						    "string-utf-8-length",
						    1, 3),
			     env);


  /* In principle, `version' could be foldable, but it invites
     more problems than it solves... */

  scheme_add_global_constant("version",
			     scheme_make_immed_prim(version,
						    "version",
						    0, 0),
			     env);
  scheme_add_global_constant("banner",
			     scheme_make_immed_prim(banner,
						    "banner",
						    0, 0),
			     env);

  scheme_add_global_constant("getenv",
			     scheme_make_immed_prim(sch_getenv,
						    "getenv",
						    1, 1),
			     env);
  scheme_add_global_constant("putenv",
			     scheme_make_immed_prim(sch_putenv,
						    "putenv",
						    2, 2),
			     env);

  /* Don't make these folding, since they're platform-specific: */

  scheme_add_global_constant("system-type",
			     scheme_make_immed_prim(system_type,
						    "system-type",
						    0, 1),
			     env);
  scheme_add_global_constant("system-library-subpath",
			     scheme_make_immed_prim(system_library_subpath,
						    "system-library-subpath",
						    0, 1),
			     env);

  scheme_add_global_constant("current-command-line-arguments",
			     scheme_register_parameter(cmdline_args,
						       "current-command-line-arguments",
						       MZCONFIG_CMDLINE_ARGS),
			     env);

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

void scheme_init_string_places(void) {
  REGISTER_SO(current_locale_name_ptr);
  current_locale_name_ptr = "xxxx\0\0\0\0";
}

/**********************************************************************/
/*                     UTF-8 char constructors                        */
/**********************************************************************/

Scheme_Object *scheme_make_sized_offset_utf8_string(char *chars, intptr_t d, intptr_t len)
{
  intptr_t ulen;
  mzchar *us;

  if (len) {
    ulen = scheme_utf8_decode((unsigned char *)chars, d, d + len,
			      NULL, 0, -1,
			      NULL, 0 /* not UTF-16 */, 0xFFFD);
    us = scheme_malloc_atomic(sizeof(mzchar) * (ulen + 1));
    scheme_utf8_decode((unsigned char *)chars, d, d + len,
                       us, 0, -1,
                       NULL, 0 /* not UTF-16 */, 0xFFFD);
    us[ulen] = 0;
  } else {
    us = (mzchar *)"\0\0\0";
    ulen = 0;
  }
  return scheme_make_sized_offset_char_string(us, 0, ulen, 0);
}

Scheme_Object *
scheme_make_sized_utf8_string(char *chars, intptr_t len)
{
  return scheme_make_sized_offset_utf8_string(chars, 0, len);
}

Scheme_Object *
scheme_make_immutable_sized_utf8_string(char *chars, intptr_t len)
{
  Scheme_Object *s;

  s = scheme_make_sized_offset_utf8_string(chars, 0, len);
  if (len)
    SCHEME_SET_CHAR_STRING_IMMUTABLE(s);

  return s;
}

Scheme_Object *
scheme_make_utf8_string(const char *chars)
{
  return scheme_make_sized_offset_utf8_string((char *)chars, 0, -1);
}

Scheme_Object *
scheme_make_locale_string(const char *chars)
{
  return scheme_byte_string_to_char_string_locale(scheme_make_byte_string((char *)chars));
}

/**********************************************************************/
/*                         index helpers                              */
/**********************************************************************/

intptr_t scheme_extract_index(const char *name, int pos, int argc, Scheme_Object **argv, intptr_t top, int false_ok)
{
  intptr_t i;
  int is_top = 0;

  if (SCHEME_INTP(argv[pos])) {
    i = SCHEME_INT_VAL(argv[pos]);
  } else if (SCHEME_BIGNUMP(argv[pos])) {
    if (SCHEME_BIGPOS(argv[pos])) {
      i = top; /* out-of-bounds */
      is_top = 1;
    } else
      i = -1; /* negative */
  } else
    i = -1;

  if (!is_top && (i < 0))
    scheme_wrong_contract(name,
                          (false_ok ? "(or/c exact-nonnegative-integer? #f)" : "exact-nonnegative-integer?"),
                          pos, argc, argv);

  return i;
}

void scheme_get_substring_indices(const char *name, Scheme_Object *str,
                                  int argc, Scheme_Object **argv,
                                  int spos, int fpos, intptr_t *_start, intptr_t *_finish)
{
  intptr_t len;
  intptr_t start, finish;

  if (SCHEME_CHAPERONE_VECTORP(str))
    len = SCHEME_VEC_SIZE(str);
  else if (SCHEME_CHAR_STRINGP(str))
    len = SCHEME_CHAR_STRTAG_VAL(str);
  else
    len = SCHEME_BYTE_STRTAG_VAL(str);

  if (argc > spos)
    start = scheme_extract_index(name, spos, argc, argv, len + 1, 0);
  else
    start = 0;
  if (argc > fpos)
    finish = scheme_extract_index(name, fpos, argc, argv, len + 1, 0);
  else
    finish = len;

  if (!(start <= len)) {
    scheme_out_of_range(name, NULL, (fpos < 100) ? "starting " : "", argv[spos], str, 0, len);
  }
  if (!(finish >= start && finish <= len)) {
    scheme_out_of_range(name, NULL, "ending ", argv[fpos], str, start, len);
  }

  *_start = start;
  *_finish = finish;
}

void scheme_do_get_substring_indices(const char *name, Scheme_Object *str,
                                     int argc, Scheme_Object **argv,
                                     int spos, int fpos, intptr_t *_start, intptr_t *_finish, intptr_t len)
{
  if (argc > spos) {
    if (SCHEME_INTP(argv[spos])) {
      intptr_t start = SCHEME_INT_VAL(argv[spos]);
      if ((start >= 0) && (start < len)) {
        *_start = start;
        if (argc > fpos) {
          intptr_t finish = SCHEME_INT_VAL(argv[fpos]);
          if ((finish >= start) && (finish <= len)) {
            *_finish = finish;
            return;
          }
        } else {
          *_finish = len;
          return;
        }
      }
    }
  } else {
    *_start = 0;
    *_finish = len;
    return;
  }

  scheme_get_substring_indices(name, str, argc, argv, spos, fpos, _start, _finish);
}

/**********************************************************************/
/*                          char strings                              */
/**********************************************************************/

#define SCHEME_X_STR_VAL(x) SCHEME_CHAR_STR_VAL(x)
#define SCHEME_X_STRTAG_VAL(x) SCHEME_CHAR_STRTAG_VAL(x)
#define SCHEME_X_STRINGP(x) SCHEME_CHAR_STRINGP(x)
#define SCHEME_MUTABLE_X_STRINGP(x) SCHEME_MUTABLE_CHAR_STRINGP(x)
#define SCHEME_SET_X_STRING_IMMUTABLE(x) SCHEME_SET_CHAR_STRING_IMMUTABLE(x)
#define scheme_x_string_type scheme_char_string_type
#define X(a, b) a##_char##b
#define X_(a, b) a##_##b
#define X__(a) a
#define EMPTY (mzchar *)"\0\0\0"
#define Xchar mzchar
#define uXchar mzchar
#define XSTR ""
#define IS_STR "string?"
#define XSTRINGSTR "string"
#define SUBXSTR "substring"
#define CHARP(x) SCHEME_CHARP(x)
#define CHAR_VAL(x) SCHEME_CHAR_VAL(x)
#define CHAR_STR "char?"
#define MAKE_CHAR(x) _scheme_make_char(x)
#define xstrlen scheme_char_strlen
#include "strops.inc"

#define GEN_STRING_COMP(name, scheme_name, comp, op, ul, size_shortcut)     \
static Scheme_Object * name (int argc, Scheme_Object *argv[]) \
{  mzchar *s, *prev; int i, sl, pl; int falz = 0;\
   if (!SCHEME_CHAR_STRINGP(argv[0])) \
    scheme_wrong_contract(scheme_name, "string?", 0, argc, argv); \
   prev = SCHEME_CHAR_STR_VAL(argv[0]); pl = SCHEME_CHAR_STRTAG_VAL(argv[0]); \
   for (i = 1; i < argc; i++) { \
     if (!SCHEME_CHAR_STRINGP(argv[i])) \
      scheme_wrong_contract(scheme_name, "string?", i, argc, argv); \
     s = SCHEME_CHAR_STR_VAL(argv[i]); sl = SCHEME_CHAR_STRTAG_VAL(argv[i]); \
     if (!falz) if (!(comp(scheme_name, \
                           prev, pl, \
                           s, sl, ul, size_shortcut) op 0)) falz = 1; \
     prev = s; pl = sl; \
  } \
  return falz ? scheme_false : scheme_true; \
}

GEN_STRING_COMP(string_eq, "string=?", mz_char_strcmp, ==, 0, 1)
GEN_STRING_COMP(string_lt, "string<?", mz_char_strcmp, <, 0, 0)
GEN_STRING_COMP(string_gt, "string>?", mz_char_strcmp, >, 0, 0)
GEN_STRING_COMP(string_lt_eq, "string<=?", mz_char_strcmp, <=, 0, 0)
GEN_STRING_COMP(string_gt_eq, "string>=?", mz_char_strcmp, >=, 0, 0)

GEN_STRING_COMP(string_ci_eq, "string-ci=?", mz_char_strcmp_ci, ==, 0, 0)
GEN_STRING_COMP(string_ci_lt, "string-ci<?", mz_char_strcmp_ci, <, 0, 0)
GEN_STRING_COMP(string_ci_gt, "string-ci>?", mz_char_strcmp_ci, >, 0, 0)
GEN_STRING_COMP(string_ci_lt_eq, "string-ci<=?", mz_char_strcmp_ci, <=, 0, 0)
GEN_STRING_COMP(string_ci_gt_eq, "string-ci>=?", mz_char_strcmp_ci, >=, 0, 0)

GEN_STRING_COMP(string_locale_eq, "string-locale=?", mz_char_strcmp, ==, 1, 0)
GEN_STRING_COMP(string_locale_lt, "string-locale<?", mz_char_strcmp, <, 1, 0)
GEN_STRING_COMP(string_locale_gt, "string-locale>?", mz_char_strcmp, >, 1, 0)
GEN_STRING_COMP(string_locale_ci_eq, "string-locale-ci=?", mz_char_strcmp_ci, ==, 1, 0)
GEN_STRING_COMP(string_locale_ci_lt, "string-locale-ci<?", mz_char_strcmp_ci, <, 1, 0)
GEN_STRING_COMP(string_locale_ci_gt, "string-locale-ci>?", mz_char_strcmp_ci, >, 1, 0)

/**********************************************************************/
/*                         byte strings                               */
/**********************************************************************/

#define SCHEME_BYTEP(x) ((SCHEME_INTP(x)) && (SCHEME_INT_VAL(x) >= 0) && (SCHEME_INT_VAL(x) <= 255))

static Scheme_Object *
byte_p(int argc, Scheme_Object *argv[])
{
  return (SCHEME_BYTEP(argv[0]) ? scheme_true : scheme_false);
}

#define SCHEME_X_STR_VAL(x) SCHEME_BYTE_STR_VAL(x)
#define SCHEME_X_STRTAG_VAL(x) SCHEME_BYTE_STRTAG_VAL(x)
#define SCHEME_X_STRINGP(x) SCHEME_BYTE_STRINGP(x)
#define SCHEME_MUTABLE_X_STRINGP(x) SCHEME_MUTABLE_BYTE_STRINGP(x)
#define SCHEME_SET_X_STRING_IMMUTABLE(x) SCHEME_SET_BYTE_STRING_IMMUTABLE(x)
#define scheme_x_string_type scheme_byte_string_type
#define X(a, b) a##_byte##b
#define X_(a, b) a##_byte_##b
#define X__(a) byte_##a
#define EMPTY ""
#define Xchar char
#define uXchar unsigned char
#define XSTR "byte "
#define IS_STR "bytes?"
#define XSTRINGSTR "bytes"
#define SUBXSTR "subbytes"
#define CHARP(x) SCHEME_BYTEP(x)
#define CHAR_VAL(x) SCHEME_INT_VAL(x)
#define CHAR_STR "byte?"
#define MAKE_CHAR(x) scheme_make_integer_value(x)
#define xstrlen strlen
#define GENERATING_BYTE
#include "strops.inc"
#undef GENERATING_BYTE

/* comparisons */

#define GEN_BYTE_STRING_COMP(name, scheme_name, comp, op) \
static Scheme_Object * name (int argc, Scheme_Object *argv[]) \
{  char *s, *prev; int i, sl, pl; int falz = 0;\
   if (!SCHEME_BYTE_STRINGP(argv[0])) \
    scheme_wrong_contract(scheme_name, "bytes?", 0, argc, argv); \
   prev = SCHEME_BYTE_STR_VAL(argv[0]); pl = SCHEME_BYTE_STRTAG_VAL(argv[0]); \
   for (i = 1; i < argc; i++) { \
     if (!SCHEME_BYTE_STRINGP(argv[i])) \
      scheme_wrong_contract(scheme_name, "bytes?", i, argc, argv); \
     s = SCHEME_BYTE_STR_VAL(argv[i]); sl = SCHEME_BYTE_STRTAG_VAL(argv[i]); \
     if (!falz) if (!(comp(scheme_name, \
                           (unsigned char *)prev, pl, \
                           (unsigned char *)s, sl) op 0)) falz = 1; \
     prev = s; pl = sl; \
  } \
  return falz ? scheme_false : scheme_true; \
}

GEN_BYTE_STRING_COMP(byte_string_eq, "bytes=?", mz_strcmp, ==)
GEN_BYTE_STRING_COMP(byte_string_lt, "bytes<?", mz_strcmp, <)
GEN_BYTE_STRING_COMP(byte_string_gt, "bytes>?", mz_strcmp, >)

/**********************************************************************/
/*                   byte string <-> char string                      */
/**********************************************************************/

/************************* bytes->string *************************/

static Scheme_Object *
do_byte_string_to_char_string(const char *who,
			      Scheme_Object *bstr,
			      intptr_t istart, intptr_t ifinish,
			      int perm, int as_locale)
{
  int i, ulen;
  char *chars;
  unsigned int *v;

  chars = SCHEME_BYTE_STR_VAL(bstr);

  ulen = utf8_decode_x((unsigned char *)chars, istart, ifinish,
		       NULL, 0, -1,
		       NULL, NULL, 0, 0,
		       NULL, 0, 
		       (perm > -1) ? 0xD800 : 0);
  if (ulen < 0) {
    scheme_contract_error(who,
                          "string is not a well-formed UTF-8 encoding",
                          "string", 1, bstr,
                          NULL);
  }

  v = (unsigned int *)scheme_malloc_atomic((ulen + 1) * sizeof(unsigned int));
  utf8_decode_x((unsigned char *)chars, istart, ifinish,
		v, 0, -1,
		NULL, NULL, 0, 0,
		NULL, 0, 
		(perm > -1) ? 0xD800 : 0);
  
  if (perm > -1) {
    for (i = 0; i < ulen; i++) {
      if (v[i] == 0xD800)
	v[i] = perm;
    }
  }
  v[ulen] = 0;

  return scheme_make_sized_char_string(v, ulen, 0);
}

static Scheme_Object *
do_byte_string_to_char_string_locale(const char *who,
				     Scheme_Object *bstr,
				     intptr_t istart, intptr_t ifinish,
				     int perm)
{
  char *us;
  intptr_t olen;

  reset_locale();
  if (!iconv_ready) init_iconv();

  if (mzLOCALE_IS_UTF_8(current_locale_name) || !locale_on || !mzCHK_PROC(iconv_open))
    return do_byte_string_to_char_string(who, bstr, istart, ifinish, perm, 1);

  if (istart < ifinish) {
    int no_cvt;

    us = string_to_from_locale(0, SCHEME_BYTE_STR_VAL(bstr),
			       istart, ifinish - istart,
			       &olen, perm, &no_cvt);

    if (!us) {
      if (no_cvt) {
	return do_byte_string_to_char_string(who, bstr, istart, ifinish, perm, 1);
      } else {
	scheme_contract_error(who,
                              "byte string is not a valid encoding for the current locale",
                              "byte string", 1, bstr,
                              NULL);
      }
    }
    ((mzchar *)us)[olen] = 0;
  } else {
    us = "\0\0\0";
    olen = 0;
  }

  return scheme_make_sized_char_string((mzchar *)us, olen, 0);
}

static Scheme_Object *
do_string_to_vector(const char *who, int mode, int argc, Scheme_Object *argv[])
{
  int permc;
  intptr_t istart, ifinish;

  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_contract(who, "bytes?", 0, argc, argv);

  if ((argc < 2) || SCHEME_FALSEP(argv[1]))
    permc = -1;
  else {
    if (!SCHEME_CHARP(argv[1]))
      scheme_wrong_contract(who, "(or/c char? #f)", 1, argc, argv);
    permc = SCHEME_CHAR_VAL(argv[1]);
  }

  scheme_get_substring_indices(who, argv[0], argc, argv,
			       2, 3,
			       &istart, &ifinish);

  if (mode == 0)
    return do_byte_string_to_char_string(who, argv[0], istart, ifinish, permc, 0);
  else if (mode == 1)
    return do_byte_string_to_char_string_locale(who, argv[0], istart, ifinish, permc);
  else {
    /* Latin-1 */
    mzchar *us;
    unsigned char *s;
    intptr_t i, len;
    len = ifinish - istart;
    s = (unsigned char *)SCHEME_BYTE_STR_VAL(argv[0]);
    us = (mzchar *)scheme_malloc_atomic((len + 1) * sizeof(mzchar));
    for (i = istart; i < ifinish; i++) {
      us[i - istart] = s[i];
    }
    us[len] = 0;

    return scheme_make_sized_char_string(us, len, 0);
  }
}


static Scheme_Object *
byte_string_to_char_string (int argc, Scheme_Object *argv[])
{
  return do_string_to_vector("bytes->string/utf-8", 0, argc, argv);
}

static Scheme_Object *
byte_string_to_char_string_locale (int argc, Scheme_Object *argv[])
{
  return do_string_to_vector("bytes->string/locale", 1, argc, argv);
}

static Scheme_Object *
byte_string_to_char_string_latin1 (int argc, Scheme_Object *argv[])
{
  return do_string_to_vector("bytes->string/latin-1", 2, argc, argv);
}

Scheme_Object *scheme_byte_string_to_char_string(Scheme_Object *o)
{
  return do_byte_string_to_char_string("s->s", o, 0, SCHEME_BYTE_STRLEN_VAL(o), 0xFFFD, 0);
}

Scheme_Object *scheme_byte_string_to_char_string_locale(Scheme_Object *o)
{
  return do_byte_string_to_char_string_locale("s->s", o, 0, SCHEME_BYTE_STRLEN_VAL(o), 0xFFFD);
}

/************************* string->bytes *************************/

static Scheme_Object *do_char_string_to_byte_string(Scheme_Object *s, intptr_t istart, intptr_t ifinish, 
						    int as_locale)
{
  char *bs;
  int slen;

  slen = scheme_utf8_encode(SCHEME_CHAR_STR_VAL(s), istart, ifinish,
			    NULL, 0,
			    0 /* UTF-16 */);
  bs = (char *)scheme_malloc_atomic(slen + 1);
  scheme_utf8_encode(SCHEME_CHAR_STR_VAL(s), istart, ifinish,
		     (unsigned char *)bs, 0,
		     0 /* UTF-16 */);
  bs[slen] = 0;

  return scheme_make_sized_byte_string(bs, slen, 0);
}

static Scheme_Object *
do_char_string_to_byte_string_locale(const char *who,
				     Scheme_Object *cstr,
				     intptr_t istart, intptr_t ifinish,
				     int perm)
{
  char *s;
  intptr_t olen;

  reset_locale();
  if (!iconv_ready) init_iconv();

  if (mzLOCALE_IS_UTF_8(current_locale_name) || !locale_on || !mzCHK_PROC(iconv_open))
    return do_char_string_to_byte_string(cstr, istart, ifinish, 1);

  if (istart < ifinish) {
    int no_cvt;

    s = string_to_from_locale(1, (char *)SCHEME_CHAR_STR_VAL(cstr),
			      istart, ifinish - istart,
			      &olen, perm, &no_cvt);

    if (!s) {
      if (no_cvt) {
	return do_char_string_to_byte_string(cstr, istart, ifinish, 1);
      } else {
	scheme_contract_error(who,
                              "string cannot be encoded for the current locale",
                              "string", 1, cstr, 
                              NULL);
      }
    }
    s[olen] = 0;
  } else {
    s = "";
    olen = 0;
  }

  return scheme_make_sized_byte_string(s, olen, 0);
}


Scheme_Object *scheme_char_string_to_byte_string(Scheme_Object *s)
{
  return do_char_string_to_byte_string(s, 0, SCHEME_CHAR_STRLEN_VAL(s), 0);
}

Scheme_Object *scheme_char_string_to_byte_string_locale(Scheme_Object *s)
{
  return do_char_string_to_byte_string_locale("s->s", s, 0, SCHEME_CHAR_STRLEN_VAL(s), '?');
}

static Scheme_Object *do_chars_to_bytes(const char *who, int mode,
					int argc, Scheme_Object *argv[])
{
  intptr_t istart, ifinish;
  int permc;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract(who, "string?", 0, argc, argv);

  if ((argc < 2) || SCHEME_FALSEP(argv[1]))
    permc = -1;
  else {
    if (!SCHEME_BYTEP(argv[1]))
      scheme_wrong_contract(who, "(or/c byte? #f)", 1, argc, argv);
    permc = SCHEME_INT_VAL(argv[1]);
  }

  scheme_get_substring_indices(who, argv[0], argc, argv,
			       2, 3, &istart, &ifinish);

  if (mode == 1)
    return do_char_string_to_byte_string_locale(who, argv[0], istart, ifinish, permc);
  else if (mode == 0)
    return do_char_string_to_byte_string(argv[0], istart, ifinish, 0);
  else {
    /* Latin-1 */
    mzchar *us;
    unsigned char *s;
    intptr_t i, len;
    len = ifinish - istart;
    us = SCHEME_CHAR_STR_VAL(argv[0]);
    s = (unsigned char *)scheme_malloc_atomic(len + 1);
    for (i = istart; i < ifinish; i++) {
      if (us[i] < 256)
	s[i - istart] = us[i];
      else if (permc >= 0) {
	s[i - istart] = permc;
      } else {
	scheme_contract_error(who,
                              "string cannot be encoded in Latin-1",
                              "string", 1, argv[0],
                              NULL);
      }
    }
    s[len] = 0;

    return scheme_make_sized_byte_string((char *)s, len, 0);
  }
}

static Scheme_Object *char_string_to_byte_string(int argc, Scheme_Object *argv[])
{
  return do_chars_to_bytes("string->bytes/utf-8", 0, argc, argv);
}

static Scheme_Object *char_string_to_byte_string_locale(int argc, Scheme_Object *argv[])
{
  return do_chars_to_bytes("string->bytes/locale", 1, argc, argv);
}

static Scheme_Object *char_string_to_byte_string_latin1(int argc, Scheme_Object *argv[])
{
  return do_chars_to_bytes("string->bytes/latin-1", 2, argc, argv);
}

/************************* Other *************************/

static Scheme_Object *char_string_utf8_length (int argc, Scheme_Object *argv[])
{
  intptr_t istart, ifinish, len;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("string-utf-8-length", "string?", 0, argc, argv);

  scheme_get_substring_indices("string-utf-8-length", argv[0], argc, argv,
			       1, 2, &istart, &ifinish);

  len = scheme_utf8_encode(SCHEME_CHAR_STR_VAL(argv[0]), istart, ifinish,
			   NULL, 0, 0);

  return scheme_make_integer(len);
}

static Scheme_Object *
byte_string_utf8_length (int argc, Scheme_Object *argv[])
{
  int len, perm;
  intptr_t istart, ifinish;
  char *chars;

  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_contract("bytes-utf-8-length", "string?", 0, argc, argv);

  chars = SCHEME_BYTE_STR_VAL(argv[0]);

  if ((argc > 1) && !SCHEME_FALSEP(argv[1])) {
    if (!SCHEME_CHARP(argv[1]))
      scheme_wrong_contract("bytes-utf-8-length", "(or/c char? #f)", 1, argc, argv);
    perm = 1;
  } else
    perm = 0;

  scheme_get_substring_indices("bytes-utf-8-length", argv[0], argc, argv,
			       2, 3,
			       &istart, &ifinish);

  len = scheme_utf8_decode((unsigned char *)chars, istart, ifinish,
			   NULL, 0, -1,
			   NULL, 0, perm);

  if (len < 0)
    return scheme_false;
  else
    return scheme_make_integer(len);
}

static Scheme_Object *
byte_string_utf8_index(int argc, Scheme_Object *argv[])
{
  intptr_t istart, ifinish, pos = -1, opos, ipos;
  int result, perm;
  char *chars;

  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_contract("bytes-utf-8-index", "bytes?", 0, argc, argv);

  chars = SCHEME_BYTE_STR_VAL(argv[0]);

  if (SCHEME_INTP(argv[1])) {
    pos = SCHEME_INT_VAL(argv[1]);
  } else if (SCHEME_BIGNUMP(argv[1])) {
    if (SCHEME_BIGPOS(argv[1]))
      pos = 0x7FFFFFFF;
  }

  if (pos < 0) {
    scheme_wrong_contract("bytes-utf-8-index", "exact-nonnegative-integer?", 1, argc, argv);
  }

  if ((argc > 2) && !SCHEME_FALSEP(argv[2])) {
    if (!SCHEME_CHARP(argv[2]))
      scheme_wrong_contract("bytes-utf-8-index", "(or/c char? #f)", 1, argc, argv);
    perm = 1;
  } else
    perm = 0;

  scheme_get_substring_indices("bytes-utf-8-index", argv[0], argc, argv,
			       3, 4,
			       &istart, &ifinish);

  result = utf8_decode_x((unsigned char *)chars, istart, ifinish,
			 NULL, 0, pos,
			 &ipos, &opos,
			 0, 0, NULL, 0, perm ? 1 : 0);

  if (((result < 0) && (result != -3))
      || ((ipos == ifinish) && (opos <= pos)))
    return scheme_false;
  else
    return scheme_make_integer(ipos);
}

static Scheme_Object *
byte_string_utf8_ref(int argc, Scheme_Object *argv[])
{
  intptr_t istart, ifinish, pos = -1, opos, ipos;
  char *chars;
  unsigned int us[1];
  Scheme_Object *perm;

  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_contract("bytes-utf-8-ref", "bytes?", 0, argc, argv);

  chars = SCHEME_BYTE_STR_VAL(argv[0]);

  if (SCHEME_INTP(argv[1])) {
    pos = SCHEME_INT_VAL(argv[1]);
  } else if (SCHEME_BIGNUMP(argv[1])) {
    if (SCHEME_BIGPOS(argv[1]))
      pos = 0x7FFFFFFF;
  }

  if (pos < 0) {
    scheme_wrong_contract("bytes-utf-8-ref", "exact-nonnegative-integer?", 1, argc, argv);
  }

  if ((argc > 2) && !SCHEME_FALSEP(argv[2])) {
    if (!SCHEME_CHARP(argv[2]))
      scheme_wrong_contract("bytes-utf-8-ref", "(or/c char? #f)", 1, argc, argv);
    perm = argv[2];
  } else
    perm = 0;

  scheme_get_substring_indices("bytes-utf-8-ref", argv[0], argc, argv,
			       3, 4,
			       &istart, &ifinish);

  if (pos > 0) {
    utf8_decode_x((unsigned char *)chars, istart, ifinish,
		  NULL, 0, pos,
		  &ipos, &opos,
		  0, 0, NULL, 0, perm ? 1 : 0);
    if (opos < pos)
      return scheme_false;
    istart = ipos;
  }

  utf8_decode_x((unsigned char *)chars, istart, ifinish,
		us, 0, 1,
		&ipos, &opos,
		0, 0, NULL, 0, perm ? 0xFFFF : 0);

  if (opos < 1)
    return scheme_false;
  else if (us[0] == 0xFFFF)
    return perm;
  else
    return scheme_make_character(us[0]);
}

/********************************************************************/
/*                            format                                */
/********************************************************************/

void scheme_do_format(const char *procname, Scheme_Object *port,
		      const mzchar *format, int flen,
		      int fpos, int offset, int argc, Scheme_Object **argv)
{
  int i, start, end;
  int used = offset;
  int num_err = 0, char_err = 0, end_ok = 0;
  Scheme_Object *a[2];

  if (!format) {
    if (!SCHEME_CHAR_STRINGP(argv[fpos])) {
      scheme_wrong_contract(procname, "string?", fpos, argc, argv);
      return;
    }
    format = SCHEME_CHAR_STR_VAL(argv[fpos]);
    flen = SCHEME_CHAR_STRTAG_VAL(argv[fpos]);
  } else if (flen == -1)
    flen = strlen((char *)format);

  /* Check string first: */
  end = flen - 1;
  for (i = 0; i < end; i++) {
    if (format[i] == '~') {
      i++;
      if (scheme_isspace(format[i])) {
	/* skip spaces... */
      } else switch (format[i]) {
      case '~':
	if (i == end)
	  end_ok = 1;
	break;
      case '%':
      case 'n':
      case 'N':
	break;
      case 'a':
      case 'A':
      case 's':
      case 'S':
      case 'v':
      case 'V':
      case 'e':
      case 'E':
	used++;
	break;
      case '.':
        switch (format[i+1]) {
        case 'a':
        case 'A':
        case 's':
        case 'S':
        case 'v':
        case 'V':
          break;
        default:
	  scheme_contract_error(procname, 
                                "ill-formed pattern string",
                                "explanation", 0, "tag `~.' not followed by `a', `s', or `v'",
                                "pattern string", 1, argv[fpos],
                                NULL);
          break;
        }
        used++;
        break;
      case 'x':
      case 'X':
      case 'o':
      case 'O':
      case 'b':
      case 'B':
	if (!num_err && !char_err && (used < argc)) {
	  Scheme_Object *o = argv[used];
	  if (!SCHEME_EXACT_REALP(o)
	      && (!SCHEME_COMPLEXP(o)
		  || !SCHEME_EXACT_REALP(scheme_complex_real_part(o))))
	    num_err = used + 1;
	}
	used++;
	break;
      case 'c':
      case 'C':
	if (!num_err && !char_err && (used < argc)) {
	  if (!SCHEME_CHARP(argv[used]))
	    char_err = used + 1;
	}
	used++;
	break;
      default:
	{
	  char buffer[64];
	  sprintf(buffer, "tag `~%c' not allowed", format[i]);          
	  scheme_contract_error(procname, 
                                "ill-formed pattern string",
                                "explanation", 0, buffer,
                                "pattern string", 1, argv[fpos],
                                NULL);
	  return;
	}
      }
    }
  }
  if ((format[end] == '~') && !end_ok) {
    scheme_contract_error(procname, 
                          "ill-formed pattern string",
                          "explanation", 0, "cannot end in `~'",
                          "pattern string", 1, argv[fpos],
                          NULL);
    return;
  }
  if (used != argc) {
    char *args;
    intptr_t alen;

    args = scheme_make_args_string("", -1, argc, argv, &alen);

    if (used > argc) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: format string requires %d arguments, given %d%t",
		       procname, used - offset, argc - offset, args, alen);
    } else {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: format string requires %d arguments, given %d%t",
		       procname, used - offset, argc - offset, args, alen);
    }
    return;
  }
  if (num_err || char_err) {
    int pos = (num_err ? num_err : char_err) - 1;
    char *args, *bstr;
    intptr_t alen;
    intptr_t blen;
    char *type = (num_err ? "exact-number" : "character");
    Scheme_Object *bad = argv[pos];

    args = scheme_make_args_string("other ", pos, argc, argv, &alen);
    bstr = scheme_make_provided_string(bad, 1, &blen);
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: format string requires argument of type <%s>, given %t%t",
		     procname, type,
		     bstr, blen,
		     args, alen);
    return;
  }

  for (used = offset, i = start = 0; i < flen; i++) {
    if (format[i] == '~') {
      if (start < i) {
	(void)scheme_put_char_string(procname, port, format, start, i - start);
      }
      i++;
      if (scheme_isspace(format[i])) {
	/* skip spaces (at most one newline) */
	do {
	  if ((format[i] == '\n') || (format[i] == '\r')) {
	    /* got one */
	    if ((format[i] == '\r') && (format[i + 1] == '\n'))
	      i++; /* Windows-style CR-NL */
	    i++;
	    while (portable_isspace(format[i])
		   && !((format[i] == '\n') || (format[i] == '\r'))) {
	      i++;
	    }
	    break;
	  } else
	    i++;
	} while (scheme_isspace(format[i]));
	--i; /* back up over something */
      } else switch (format[i]) {
      case '~':
	scheme_write_byte_string("~", 1, port);
	break;
      case '%':
      case 'n':
      case 'N':
	scheme_write_byte_string("\n", 1, port);
	break;
      case 'c':
      case 'C':
      case 'a':
      case 'A':
	a[0] = argv[used++];
	a[1] = port;
	_scheme_apply(scheme_display_proc, 2, a);
	break;
      case 's':
      case 'S':
	a[0] = argv[used++];
	a[1] = port;
	_scheme_apply(scheme_write_proc, 2, a);
	break;
      case 'v':
      case 'V':
	a[0] = argv[used++];
	a[1] = port;
	_scheme_apply(scheme_print_proc, 2, a);
	break;
      case 'e':
      case 'E':
	{
	  intptr_t len;
	  char *s;
	  s = scheme_make_provided_string(argv[used++], 0, &len);
	  scheme_write_byte_string(s, len, port);
	}
	break;
      case '.':
	{
	  intptr_t len;
	  char *s;
          len = scheme_get_print_width();
          i++;
          switch (format[i]) {
          case 'a':
          case 'A':
            s = scheme_display_to_string_w_max(argv[used++], &len, len);
            break;
          case 's':
          case 'S':
            s = scheme_write_to_string_w_max(argv[used++], &len, len);
            break;
          case 'v':
          case 'V':
            s = scheme_print_to_string_w_max(argv[used++], &len, len);
            break;
          default:
            s = "???";
            len = 3;
          }
	  scheme_write_byte_string(s, len, port);
	}
	break;
      case 'x':
      case 'X':
      case 'o':
      case 'O':
      case 'b':
      case 'B':
	{
	  char *s;
	  int radix;

	  switch(format[i]) {
	  case 'x':
	  case 'X':
	    radix = 16;
	    break;
	  case 'o':
	  case 'O':
	    radix = 8;
	    break;
	  default:
	  case 'b':
	  case 'B':
	    radix = 2;
	    break;
	  }
	  s = scheme_number_to_string(radix, argv[used++]);

	  scheme_write_byte_string(s, strlen(s), port);
	}
	break;
      }
      SCHEME_USE_FUEL(1);
      start = i + 1;
    }
  }

  SCHEME_USE_FUEL(flen);

  if (start < i) {
    (void)scheme_put_char_string(procname, port, format, start, i - start);
  }
}

char *scheme_format(mzchar *format, int flen, int argc, Scheme_Object **argv, intptr_t *rlen)
{
  Scheme_Object *port;
  port = scheme_make_byte_string_output_port();
  scheme_do_format("format", port, format, flen, 0, 0, argc, argv);
  return scheme_get_sized_byte_string_output(port, rlen);
}

void scheme_printf(mzchar *format, int flen, int argc, Scheme_Object **argv)
{
  scheme_do_format("printf", scheme_get_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT),
		   format, flen, 0, 0, argc, argv);
}

char *scheme_format_utf8(char *format, int flen, int argc, Scheme_Object **argv, intptr_t *rlen)
{
  mzchar *s;
  intptr_t srlen;
  if (flen == -1)
    flen = strlen(format);
  s = scheme_utf8_decode_to_buffer_len((unsigned char *)format, flen, NULL, 0, &srlen);
  if (s)
    return scheme_format(s, srlen, argc, argv, rlen);
  else
    return "";
}

void scheme_printf_utf8(char *format, int flen, int argc, Scheme_Object **argv)
{
  mzchar *s;
  intptr_t srlen;
  if (flen == -1)
    flen = strlen(format);
  s = scheme_utf8_decode_to_buffer_len((unsigned char *)format, flen, NULL, 0, &srlen);
  if (s)
    scheme_printf(s, srlen, argc, argv);
}


static Scheme_Object *
format(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  char *s;
  intptr_t len;

  port = scheme_make_byte_string_output_port();

  scheme_do_format("format", port, NULL, 0, 0, 1, argc, argv);

  s = scheme_get_sized_byte_string_output(port, &len);
  return scheme_make_sized_utf8_string(s, len);
}

#ifdef INSTRUMENT_PRIMITIVES
extern int g_print_prims;
#endif

static Scheme_Object *
sch_printf(int argc, Scheme_Object *argv[])
{
  scheme_do_format("printf", scheme_get_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT),
		   NULL, 0, 0, 1, argc, argv);
  return scheme_void;
}

static Scheme_Object *
sch_eprintf(int argc, Scheme_Object *argv[])
{
  scheme_do_format("eprintf", scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_PORT),
		   NULL, 0, 0, 1, argc, argv);
  return scheme_void;
}

static Scheme_Object *
sch_fprintf(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("fprintf", "output-port?", 0, argc, argv);

  scheme_do_format("fprintf", argv[0], NULL, 0, 1, 2, argc, argv);
  return scheme_void;
}

/********************************************************************/
/*                              misc                                */
/********************************************************************/

static Scheme_Object *
version(int argc, Scheme_Object *argv[])
{
  return vers_str;
}

static Scheme_Object *
banner(int argc, Scheme_Object *argv[])
{
  return banner_str;
}

char *scheme_version(void)
{
  return MZSCHEME_VERSION;
}

#ifdef MZ_PRECISE_GC
/* don't print " [3m]", which is the default: */
# define VERSION_SUFFIX ""
#else
# ifdef USE_SENORA_GC
#  define VERSION_SUFFIX " [cgc~]"
# else
#  define VERSION_SUFFIX " [cgc]"
# endif
#endif

char *scheme_banner(void)
{
  if (embedding_banner)
    return embedding_banner;
  else
    return ("Welcome to Racket"
            " v" MZSCHEME_VERSION VERSION_SUFFIX
            ".\n");
}

void scheme_set_banner(char *s)
{
  embedding_banner = s;
}

int scheme_byte_string_has_null(Scheme_Object *o)
{
  const char *s = SCHEME_BYTE_STR_VAL(o);
  int i = SCHEME_BYTE_STRTAG_VAL(o);
  while (i--) {
    if (!s[i])
      return 1;
  }
  return 0;
}

int scheme_any_string_has_null(Scheme_Object *o)
{
  if (SCHEME_BYTE_STRINGP(o))
    return scheme_byte_string_has_null(o);
  else {
    const mzchar *s = SCHEME_CHAR_STR_VAL(o);
    int i = SCHEME_CHAR_STRTAG_VAL(o);
    while (i--) {
      if (!s[i])
	return 1;
    }
    return 0;
  }
}

/***********************************************************************/
/* Environment Variables                                               */
/***********************************************************************/

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
static char* clone_str_with_gc(const char* buffer) {
  int length;
  char *newbuffer;
  length = strlen(buffer);
  newbuffer = scheme_malloc_atomic(length+1);
  memcpy(newbuffer, buffer, length+1);
  return newbuffer;
}
#endif

static void create_putenv_str_table_if_needed() {
  if (!putenv_str_table) {
    putenv_str_table = scheme_make_hash_table(SCHEME_hash_string);
  }
}

#ifndef DOS_FILE_SYSTEM
static void putenv_str_table_put_name(Scheme_Object *name, Scheme_Object *value) {
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  void *original_gc;
  Scheme_Object *name_copy;
  original_gc = GC_switch_to_master_gc();
  scheme_start_atomic();

  name_copy = (Scheme_Object *) clone_str_with_gc((const char *) name);
  create_putenv_str_table_if_needed();
  scheme_hash_set(putenv_str_table, name_copy, value);

  scheme_end_atomic_no_swap();
  GC_switch_back_from_master(original_gc);
#else
  create_putenv_str_table_if_needed();
  scheme_hash_set(putenv_str_table, name, value);
#endif
}
#endif

#ifndef GETENV_FUNCTION
static void putenv_str_table_put_name_value(Scheme_Object *name, Scheme_Object *value) {
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  void *original_gc;
  Scheme_Object *name_copy;
  Scheme_Object *value_copy;
  original_gc = GC_switch_to_master_gc();
  scheme_start_atomic();

  name_copy = (Scheme_Object *) clone_str_with_gc((const char *) name);
  value_copy = (Scheme_Object *) clone_str_with_gc((const char *) value);
  create_putenv_str_table_if_needed();
  scheme_hash_set(putenv_str_table, name_copy, value_copy);

  scheme_end_atomic_no_swap();
  GC_switch_back_from_master(original_gc);
#else
  create_putenv_str_table_if_needed();
  scheme_hash_set(putenv_str_table, name, value);
#endif
}
#endif

#if !defined(GETENV_FUNCTION) || defined(MZ_PRECISE_GC)
static Scheme_Object *putenv_str_table_get(Scheme_Object *name) {
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  void *original_gc;
  Scheme_Object *value; 
  original_gc = GC_switch_to_master_gc();
  scheme_start_atomic();

  create_putenv_str_table_if_needed();
  value = scheme_hash_get(putenv_str_table, name);

  scheme_end_atomic_no_swap();
  GC_switch_back_from_master(original_gc);
  return value;
#else
  create_putenv_str_table_if_needed();
  return scheme_hash_get(putenv_str_table, name);
#endif
}
#endif


static int sch_bool_getenv(const char* name);

void
scheme_init_getenv(void)
{
#ifndef GETENV_FUNCTION
  FILE *f = fopen("Environment", "r");
  if (f) {
    Scheme_Object *p = scheme_make_file_input_port(f);
    mz_jmp_buf *savebuf, newbuf;
    savebuf = scheme_current_thread->error_buf;
    scheme_current_thread->error_buf = &newbuf;
    if (!scheme_setjmp(newbuf)) {
      while (1) {
        Scheme_Object *v = scheme_read(p);
        if (SCHEME_EOFP(v))
          break;

        if (SCHEME_PAIRP(v) && SCHEME_PAIRP(SCHEME_CDR(v))
            && SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(v)))) {
          Scheme_Object *key = SCHEME_CAR(v);
          Scheme_Object *val = SCHEME_CADR(v);
          if (SCHEME_STRINGP(key) && SCHEME_STRINGP(val)) {
            Scheme_Object *a[2];
            a[0] = key;
            a[1] = val;
            sch_putenv(2, a);
            v = NULL;
          }
        }

        if (v)
          scheme_signal_error("bad environment specification: %V", v);
      }
    }
    scheme_current_thread->error_buf = savebuf;
    scheme_close_input_port(p);
  }
#endif
  if (sch_bool_getenv("PLTNOMZJIT")) {
      scheme_set_startup_use_jit(0);
  }
}

#ifdef DOS_FILE_SYSTEM
# include <windows.h>
static char *dos_win_getenv(const char *name) {
  int value_size;
  value_size = GetEnvironmentVariable(name, NULL, 0);
  if (value_size) {
    char *value;
    int got;
    value = scheme_malloc_atomic(value_size);
    got = GetEnvironmentVariable(name, value, value_size);
    if (got < value_size)
      value[got] = 0;
    return value;
  }
  return NULL;
}
#endif

static int sch_bool_getenv(const char* name) {
  int rc = 0;
#ifdef GETENV_FUNCTION
# ifdef DOS_FILE_SYSTEM
  if (GetEnvironmentVariable(name, NULL, 0)) rc = 1;
# else
  if (getenv(name)) rc = 1;
# endif
#else
  if (putenv_str_table_get(name))  rc = 1;
#endif
  return rc;
}

static Scheme_Object *sch_getenv(int argc, Scheme_Object *argv[])
{
  char *name;
  char *value;
  Scheme_Object *bs;

  if (!SCHEME_CHAR_STRINGP(argv[0]) || scheme_any_string_has_null(argv[0]))
    scheme_wrong_contract("getenv", CHAR_STRING_W_NO_NULLS, 0, argc, argv);

  bs = scheme_char_string_to_byte_string_locale(argv[0]);
  name = SCHEME_BYTE_STR_VAL(bs);

#ifdef GETENV_FUNCTION
# ifdef DOS_FILE_SYSTEM
  value = dos_win_getenv(name);
# else
  value = getenv(name);
# endif
#else
  {
    Scheme_Object *hash_value;
    hash_value = putenv_str_table_get(name); 
    return hash_value ? hash_value : scheme_false;
  }
#endif

  return value ? scheme_make_locale_string(value) : scheme_false;
}

#ifndef DOS_FILE_SYSTEM
static int sch_unix_putenv(const char *var, const char *val, const intptr_t varlen, const intptr_t vallen) {
  char *buffer;
  intptr_t total_length;
  total_length = varlen + vallen + 2;

#ifdef MZ_PRECISE_GC
  /* Can't put moveable string into array. */
  buffer = malloc(total_length);
#else
  buffer = (char *)scheme_malloc_atomic(total_length);
#endif
  memcpy(buffer, var, varlen);
  buffer[varlen] = '=';
  memcpy(buffer + varlen + 1, val, vallen + 1);

#ifdef MZ_PRECISE_GC
  {
    /* Free old, if in table: */
    char *oldbuffer;
    oldbuffer = (char *)putenv_str_table_get((Scheme_Object *)var);
    if (oldbuffer)
      free(oldbuffer);
  }
#endif

  /* if precise the buffer needs to be remembered so it can be freed */
  /* if not precise the buffer needs to be rooted so it doesn't get collected prematurely */
  putenv_str_table_put_name((Scheme_Object *)var, (Scheme_Object *)buffer);
  return putenv(buffer);
} 
#endif

static Scheme_Object *sch_putenv(int argc, Scheme_Object *argv[])
{
  Scheme_Object *varbs;
  Scheme_Object *valbs;
  char *var;
  char *val;
  int rc = 0;

  if (!SCHEME_CHAR_STRINGP(argv[0]) || scheme_any_string_has_null(argv[0]))
    scheme_wrong_contract("putenv", CHAR_STRING_W_NO_NULLS, 0, argc, argv);
  if (!SCHEME_CHAR_STRINGP(argv[1]) || scheme_any_string_has_null(argv[1]))
    scheme_wrong_contract("putenv", CHAR_STRING_W_NO_NULLS, 1, argc, argv);

  varbs = scheme_char_string_to_byte_string_locale(argv[0]);
  var = SCHEME_BYTE_STR_VAL(varbs);

  valbs = scheme_char_string_to_byte_string_locale(argv[1]);
  val = SCHEME_BYTE_STR_VAL(valbs);

#ifdef GETENV_FUNCTION
# ifdef DOS_FILE_SYSTEM
  rc = !SetEnvironmentVariable(var, val);
# else
  rc = sch_unix_putenv(var, val, SCHEME_BYTE_STRLEN_VAL(varbs), SCHEME_BYTE_STRLEN_VAL(valbs));
# endif
#else
  putenv_str_table_put_name_value(argv[0], argv[1]);
#endif
  return rc ? scheme_false : scheme_true;
}

/***********************************************************************/
/* End Environment Variables                                           */
/***********************************************************************/

static void machine_details(char *s);

static Scheme_Object *system_type(int argc, Scheme_Object *argv[])
{
  if (argc) {
    Scheme_Object *sym;
    sym = scheme_intern_symbol("link");
    if (SAME_OBJ(argv[0], sym)) {
#if defined(OS_X) && !defined(XONX)
      return scheme_intern_symbol("framework");
#else
# ifdef DOS_FILE_SYSTEM
      return scheme_intern_symbol("dll");
# else
#  ifdef MZ_USES_SHARED_LIB
      return scheme_intern_symbol("shared");
#  else
      return scheme_intern_symbol("static");
#  endif
# endif
#endif
    }

    sym = scheme_intern_symbol("machine");
    if (SAME_OBJ(argv[0], sym)) {
      char buff[1024];
      
      machine_details(buff);
    
      return scheme_make_utf8_string(buff);
    }

    sym = scheme_intern_symbol("gc");
    if (SAME_OBJ(argv[0], sym)) {
#ifdef MZ_PRECISE_GC
      return scheme_intern_symbol("3m");
#else
      return scheme_intern_symbol("cgc");
#endif
    }

    sym = scheme_intern_symbol("so-suffix");
    if (SAME_OBJ(argv[0], sym)) {
#ifdef DOS_FILE_SYSTEM
      return scheme_make_byte_string(".dll");
#else
# ifdef OS_X
      return scheme_make_byte_string(".dylib");
# else
#  ifdef USE_CYGWIN_SO_SUFFIX
      return scheme_make_byte_string(".dll");
#  else
      return scheme_make_byte_string(".so");
#  endif
# endif
#endif
    }

    sym = scheme_intern_symbol("word");
    if (SAME_OBJ(argv[0], sym)) {
      return scheme_make_integer(sizeof(void*)*8);
    }

    sym = scheme_intern_symbol("os");
    if (!SAME_OBJ(argv[0], sym)) {
      scheme_wrong_contract("system-type", "(or/c 'os 'word 'link 'machine 'gc 'so-suffix 'word)", 0, argc, argv);
      return NULL;
    }
  }

  return sys_symbol;
}

static Scheme_Object *system_library_subpath(int argc, Scheme_Object *argv[])
{
  if (argc > 0) {
    Scheme_Object *sym;

    if (SCHEME_FALSEP(argv[0]))
      return platform_cgc_path;
    
    sym = scheme_intern_symbol("cgc");
    if (SAME_OBJ(sym, argv[0]))
      return platform_cgc_path;

    sym = scheme_intern_symbol("3m");
    if (SAME_OBJ(sym, argv[0]))
      return platform_3m_path;

    scheme_wrong_contract("system-library-subpath", "(or/c 'cgc '3m #f)", 0, argc, argv);
    return NULL;
  } else {
#ifdef MZ_PRECISE_GC
    return platform_3m_path;
#else
    return platform_cgc_path;
#endif
  }
}

const char *scheme_system_library_subpath()
{
  return SCHEME_PLATFORM_LIBRARY_SUBPATH;
}

/* Our own strncpy - which would be really stupid, except the one for
   the implementation in Solaris 2.6 is broken (it doesn't always stop
   at the null terminator). */
int scheme_strncmp(const char *a, const char *b, int len)
{
  while (len-- && (*a == *b) && *a) {
    a++;
    b++;
  }

  if (len < 0)
    return 0;
  else
    return *a - *b;
}

static Scheme_Object *ok_cmdline(int argc, Scheme_Object **argv)
{
  if (SCHEME_CHAPERONE_VECTORP(argv[0])) {
    Scheme_Object *vec = argv[0], *vec2, *str;
    int i, size = SCHEME_VEC_SIZE(vec);


    if (!size)
      return vec;

    for (i = 0; i < size; i++) {
      if (!SCHEME_CHAR_STRINGP(SCHEME_VEC_ELS(vec)[i]))
	return NULL;
    }

    /* Make sure vector and strings are immutable: */
    vec2 = scheme_make_vector(size, NULL);
    if (size)
      SCHEME_SET_VECTOR_IMMUTABLE(vec2);
    for (i = 0; i < size; i++) {
      str = SCHEME_VEC_ELS(vec)[i];
      if (!SCHEME_IMMUTABLE_CHAR_STRINGP(str)) {
	str = scheme_make_sized_char_string(SCHEME_CHAR_STR_VAL(str), SCHEME_CHAR_STRLEN_VAL(str), 0);
	SCHEME_SET_CHAR_STRING_IMMUTABLE(str);
      }
      SCHEME_VEC_ELS(vec2)[i] = str;
    }

    return vec2;
  }

  return NULL;
}

static Scheme_Object *cmdline_args(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-command-line-arguments",
			     scheme_make_integer(MZCONFIG_CMDLINE_ARGS),
			     argc, argv,
			     -1, ok_cmdline, "vector of strings", 1);
}

/**********************************************************************/
/*                           locale ops                               */
/**********************************************************************/

static Scheme_Object *ok_locale(int argc, Scheme_Object **argv)
{
  if (SCHEME_FALSEP(argv[0]))
    return argv[0];
  else if (SCHEME_CHAR_STRINGP(argv[0])) {
    if (SCHEME_IMMUTABLEP(argv[0]))
      return argv[0];
    else {
      Scheme_Object *str = argv[0];
      str = scheme_make_immutable_sized_char_string(SCHEME_CHAR_STR_VAL(str), SCHEME_CHAR_STRLEN_VAL(str), 1);
      return str;
    }
  }

  return NULL;
}

static Scheme_Object *current_locale(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  v = scheme_param_config("current-locale",
			  scheme_make_integer(MZCONFIG_LOCALE),
			  argc, argv,
			  -1, ok_locale, "#f or string", 1);

  return v;
}

static Scheme_Object *locale_string_encoding(int argc, Scheme_Object *argv[])
{
  reset_locale();
  if (mzLOCALE_IS_UTF_8(current_locale_name) || !locale_on)
    return scheme_make_utf8_string("UTF-8");
  
#if HAVE_CODESET
  return scheme_make_utf8_string(nl_langinfo(CODESET));
#else
  /* nl_langinfo doesn't work, so just make up something */
  return scheme_make_utf8_string("UTF-8");
#endif
}

static Scheme_Object *system_language_country(int argc, Scheme_Object *argv[])
{
#ifdef MACOS_UNICODE_SUPPORT
  /* Mac OS X */
  CFLocaleRef l;
  CFStringRef s;
  int len;
  char *r;

  l = CFLocaleCopyCurrent();
  s = CFLocaleGetIdentifier(l);

  len = CFStringGetLength(s);
  r = (char *)scheme_malloc_atomic(len * 6 + 1);
  CFStringGetCString(s, r, len * 6 + 1, kCFStringEncodingUTF8);

  CFRelease(l);

  return scheme_make_sized_utf8_string(r, 5);
#else
# ifdef WINDOWS_UNICODE_SUPPORT
  /* Windows */
  LCID l;
  int llen, clen;
  char *lang, *country, *s;
  l = GetUserDefaultLCID();

  llen = GetLocaleInfo(l, LOCALE_SENGLANGUAGE, NULL, 0);
  lang = (char *)scheme_malloc_atomic(llen);
  GetLocaleInfo(l, LOCALE_SENGLANGUAGE, lang, llen);
  if (llen)
    llen -= 1; /* drop nul terminator */

  clen = GetLocaleInfo(l, LOCALE_SENGCOUNTRY, NULL, 0);
  country = (char *)scheme_malloc_atomic(clen);
  GetLocaleInfo(l, LOCALE_SENGCOUNTRY, country, clen);
  if (clen)
    clen -= 1; /* drop nul terminator */

  s = (char *)scheme_malloc_atomic(clen + llen + 1);
  memcpy(s, lang, llen);
  memcpy(s + 1 + llen, country, clen);
  s[llen] = '_';
  
  return scheme_make_sized_utf8_string(s, llen + 1 + clen);
# else
  /* Unix */
  char *s;
  
  s = getenv("LC_ALL");
  if (!s)
    s = getenv("LC_CTYPE");
  if (!s)
    s = getenv("LANG");
  
  if (s) {
    /* Check that the environment variable has the form
       xx_XX[.ENC] */
    if ((s[0] >= 'a') && (s[0] <= 'z')
	&& (s[1] >= 'a') && (s[1] <= 'z')
	&& (s[2] == '_')
	&& (s[3] >= 'A') && (s[3] <= 'Z')
	&& (s[4] >= 'A') && (s[4] <= 'Z')
	&& (!s[5] || s[5] == '.')) {
      /* Good */
    } else
      s = NULL;
  }
  
  if (!s)
    s = "en_US";
  
  return scheme_make_sized_utf8_string(s, 5);
# endif
#endif
}

#ifndef DONT_USE_LOCALE

#define ICONV_ARG_CAST /* empty */

static char *do_convert(iconv_t cd,
			/* if cd == -1 and either from_e or to_e can be NULL, then
			   reset_locale() must have been called */
			const char *from_e, const char *to_e,
			/* 1 => UCS-4 -> UTF-8; 2 => UTF-8 -> UCS-4; 0 => other */
			int to_from_utf8,
			/* in can be NULL to output just a shift; in that case,
			   id should be 0, too */
			char *in, int id, int iilen,
			char *out, int od, int iolen,
			/* if grow, then reallocate when out isn't big enough */
			int grow,
			/* if add_end_shift, add a shift sequence to the end;
			   not useful if in is already NULL to indicate a shift */
			int add_end_shift,
			/* extra specifies the length of a terminator,
			   not included in iolen or *oolen */
			int extra,
			/* these two report actual read/wrote sizes: */
			intptr_t *oilen, intptr_t *oolen,
			/* status is set to
			   0 for complete,
			   -1 for partial input,
			   -2 for error,
			   1 for more avail */
			int *status)
{
  int dip, dop, close_it = 0, mz_utf8 = 0;
  size_t il, ol, r;
  GC_CAN_IGNORE char *ip, *op;

  /* Defaults: */
  *status = -1;
  if (oilen)
    *oilen = 0;
  *oolen = 0;

  if (cd == (iconv_t)-1) {
    if (!iconv_ready) init_iconv();
    if (mzCHK_PROC(iconv_open)) {
      if (!from_e)
	from_e = mz_iconv_nl_langinfo();
      if (!to_e)
	to_e = mz_iconv_nl_langinfo();
      cd = iconv_open(to_e, from_e);
      close_it = 1;
    } else if (to_from_utf8) {
      /* Assume UTF-8 */
      mz_utf8 = 1;
    }
  }

  if ((cd == (iconv_t)-1) && !mz_utf8) {
    if (out) {
      while (extra--) {
	out[extra] = 0;
      }
    }
    return out;
  }

  /* The converter is ready. Allocate out space, if necessary */

  if (!out) {
    if (iolen <= 0)
      iolen = iilen;
    out = (char *)scheme_malloc_atomic(iolen + extra);
    od = 0;
  }

  /* il and ol are the number of available chars */
  il = iilen;
  ol = iolen;
  /* dip and dop are the number of characters read so far;
     we use these and NULL out the ip and op pointers
     for the sake of precise GC */
  dip = 0;
  dop = 0;
  if (!in)
    add_end_shift = 0;

  while (1) {
    int icerr;

    if (mz_utf8) {
      /* Use our UTF-8 routines as if they were iconv */
      if (to_from_utf8 == 1) {
	/* UCS-4 -> UTF-8 */
	/* We assume that in + id and iilen are mzchar-aligned */
	int opos, uid, uilen;
	uid = (id + dip) >> 2;
	uilen = (iilen - dip) >> 2;
	opos = scheme_utf8_encode((const unsigned int *)in, uid, uilen,
				  NULL, 0,
				  0);
	if (opos <= iolen) {
	  opos = scheme_utf8_encode((const unsigned int *)in, uid, uilen,
				    (unsigned char *)out, od + dop,
				    0);
	  dop += opos;
	  dip += iilen;
	  icerr = 0;
	  r = (size_t)opos;
	} else {
	  icerr = E2BIG;
	  r = (size_t)-1;
	}
      } else {
	/* UTF-8 -> UCS-4 */
	/* We assume that out + od is mzchar-aligned */
	intptr_t ipos, opos;

	r = utf8_decode_x((unsigned char *)in, id + dip, iilen,
			  (unsigned int *)out, (od + dop) >> 2, iolen >> 2,
			  &ipos, &opos,
			  0, 0, NULL, 0, 0);
	
	opos <<= 2;
	dop = (opos - od);
	dip = (ipos - id);

	if ((r == -1) || (r == -2)) {
	  r = (size_t)-1;
	  icerr = EILSEQ;
	} else if (r == -3) {
	  icerr = E2BIG;
	  r = (size_t)-1;
	} else
	  icerr = 0;
      }
    } else  {
      ip = in XFORM_OK_PLUS id + dip;
      op = out XFORM_OK_PLUS od + dop;
      r = iconv(cd, ICONV_ARG_CAST &ip, &il, &op, &ol);
      dip = ip - (in XFORM_OK_PLUS id);
      dop = op - (out XFORM_OK_PLUS od);
      ip = op = NULL;
      icerr = ICONV_errno;
    }

    /* Record how many chars processed, now */
    if (oilen)
      *oilen = dip;
    *oolen = dop;

    /* Got all the chars? */
    if (r == (size_t)-1) {
      if (icerr == E2BIG) {
	if (grow) {
	  /* Double the string size and try again */
	  char *naya;
	  naya = (char *)scheme_malloc_atomic((iolen * 2) + extra);
	  memcpy(naya, out + od, *oolen);
	  ol += iolen;
	  iolen += iolen;
	  out = naya;
	  od = 0;
	} else {
	  *status = 1;
	  if (close_it)
	    iconv_close(cd);
	  while (extra--) {
	    out[od + dop + extra] = 0;
	  }
	  return out;
	}
      } else {
	/* Either EINVAL (premature end) or EILSEQ (bad sequence) */
	if (icerr == EILSEQ)
	  *status = -2;
	if (close_it)
	  iconv_close(cd);
	while (extra--) {
	  out[od + dop + extra] = 0;
	}
	return out;
      }
    } else {
      /* All done... */
      if (add_end_shift) {
	add_end_shift = 0;
	in = NULL;
	dip = 0;
	id = 0;
	il = 0; /* should be redundant */
	oilen = NULL; /* so it doesn't get set to 0 */
      } else {
	*status = 0;
	if (close_it)
	  iconv_close(cd);
	while (extra--) {
	  out[od + dop + extra] = 0;
	}
	return out;
      }
    }
  }
}

#define MZ_SC_BUF_SIZE 32

static char *string_to_from_locale(int to_bytes,
				   char *in, intptr_t delta, intptr_t len,
				   intptr_t *olen, int perm,
				   int *no_cvt)
     /* Call this function only when iconv is available, and only when
	reset_locale() has been called */
{
  Scheme_Object *parts = scheme_null, *one;
  char *c;
  intptr_t clen, used;
  int status;
  iconv_t cd;

  if (!iconv_ready) init_iconv();

  if (to_bytes)
    cd = iconv_open(mz_iconv_nl_langinfo(), MZ_UCS4_NAME);
  else
    cd = iconv_open(MZ_UCS4_NAME, mz_iconv_nl_langinfo());
  if (cd == (iconv_t)-1) {
    *no_cvt = 1;
    return NULL;
  }
  *no_cvt = 0;

  while (len) {
    /* We might have conversion errors... */
    c = do_convert(cd, NULL, NULL, 0,
		   (char *)in, (to_bytes ? 4 : 1) * delta, (to_bytes ? 4 : 1) * len,
		   NULL, 0, (to_bytes ? 1 : 4) * (len + 1),
		   1 /* grow */, 1, (to_bytes ? 1 : 4) /* terminator size */,
		   &used, &clen,
		   &status);

    if (to_bytes)
      used >>= 2;

    if ((perm < 0) && (used < len)) {
      iconv_close(cd);
      return NULL;
    }

    delta += used;
    len -= used;

    if (!len && SCHEME_NULLP(parts)) {
      if (to_bytes) {
	*olen = clen;
	c[*olen] = 0;
      } else {
	*olen = (clen >> 2);
	((mzchar *)c)[*olen] = 0;
      }
      iconv_close(cd);
      return c;
    }

    /* We can get here if there was some conversion error at some
       point. We're building up a list of parts. */

    if (to_bytes) {
      one = scheme_make_sized_byte_string(c, clen, 0);
    } else {
      one = scheme_make_sized_char_string((mzchar *)c, clen >> 2, 0);
    }

    parts = scheme_make_pair(one, parts);

    if (len) {
      /* Conversion error, so skip one char. */
      if (to_bytes) {
	char bc[1];
	bc[0] = perm;
	one = scheme_make_sized_byte_string(bc, 1, 1);
      } else {
	mzchar bc[1];
	bc[0] = perm;
	one = scheme_make_sized_char_string(bc, 1, 1);
      }
      parts = scheme_make_pair(one, parts);
      delta += 1;
      len -= 1;
    }
  }

  iconv_close(cd);

  if (to_bytes) {
    parts = append_all_byte_strings_backwards(parts);
    *olen = SCHEME_BYTE_STRTAG_VAL(parts);

    return SCHEME_BYTE_STR_VAL(parts);
  } else {
    parts = append_all_strings_backwards(parts);
    *olen = SCHEME_CHAR_STRTAG_VAL(parts);

    return (char *)SCHEME_CHAR_STR_VAL(parts);
  }
}

static char *locale_recase(int to_up,
			   /* in must be null-terminated, iilen doesn't include it */
			   char *in, int id, int iilen,
			   /* iolen, in contrast, includes the terminator */
			   char *out, int od, int iolen,
			   intptr_t *oolen)
     /* Assumes that reset_locale() has been called */
{
#ifdef NO_MBTOWC_FUNCTIONS
  /* No wide-char functions...
     The C library's toupper and tolower is supposed to be
     locale-sensitive. It can't be right for characters that are
     encoded in multiple bytes, but probably it will do the right
     thing in common cases. */
  int i;

  /* First, copy "in" to "out" */
  if (iilen + 1 >= iolen) {
    out = (char *)scheme_malloc_atomic(iilen + 1);
    od = 0;
  }
  memcpy(out + od, in + id, iilen);
  out[od + iilen] = 0;
  *oolen = iilen;

  /* Re-case chars in "out" */
  for (i = 0; i < iilen; i++) {
    char t;
    t = (to_up) ? toupper(out[od+i]) : tolower(out[od+i]);
    out[od+i] = t;
  }

  return out;
#else
  /* To change the case, convert the string to multibyte, re-case the
     multibyte, then convert back. */
# define MZ_WC_BUF_SIZE 32
  GC_CAN_IGNORE mbstate_t state;
  size_t wl, ml;
  wchar_t *wc, *ws, wcbuf[MZ_WC_BUF_SIZE], cwc;
  const char *s;
  unsigned int j;
  /* The "n" versions are apparently not too standard: */
# define mz_mbsnrtowcs(t, f, fl, tl, s) mbsrtowcs(t, f, tl, s)
# define mz_wcsnrtombs(t, f, fl, tl, s) wcsrtombs(t, f, tl, s)

  /* ----- to wide char ---- */

  /* Get length */
  memset(&state, 0, sizeof(mbstate_t));
  s = in XFORM_OK_PLUS id;
  wl = mz_mbsnrtowcs(NULL, &s, iilen, 0, &state);
  s = NULL;

  /* Allocate space */
  if (wl < MZ_WC_BUF_SIZE) {
    wc = wcbuf;
  } else {
    wc = (wchar_t *)scheme_malloc_atomic(sizeof(wchar_t) * (wl + 1));
  }

  /* Convert */
  memset(&state, 0, sizeof(mbstate_t));
  s = in XFORM_OK_PLUS id;
  (void)mz_mbsnrtowcs(wc, &s, iilen, wl + 1, &state);
  s = NULL;

  wc[wl] = 0; /* just in case */

  /* ---- re-case ---- */

  if (to_up) {
    for (j = 0; j < wl; j++) {
      cwc = towupper(wc[j]);
      wc[j] = cwc;
    }
  } else {
    for (j = 0; j < wl; j++) {
      cwc = towlower(wc[j]);
      wc[j] = cwc;
    }
  }

  /* ---- back to multibyte ---- */

  /* Measure */
  memset(&state, 0, sizeof(mbstate_t));
  ws = wc;
  ml = mz_wcsnrtombs(NULL, (const wchar_t **)&ws, wl, 0, &state);
  ws = NULL;

  /* Allocate space */
  *oolen = ml;
  if (ml + 1 >= (unsigned int)iolen) {
    out = (char *)scheme_malloc_atomic(ml + 1);
    od = 0;
  }

  /* Convert */
  memset(&state, 0, sizeof(mbstate_t));
  ws = wc;
  (void)mz_wcsnrtombs(out + od, (const wchar_t **)&ws, wl, ml + 1, &state);
  ws = NULL;

  out[od + ml] = 0;

  return out;
#endif
}

int mz_locale_strcoll(char *s1, int d1, int l1, char *s2, int d2, int l2, int cvt_case)
     /* The s1 and s2 arguments are actually UCS-4.
        Assumes that reset_locale() has been called. */
{
  intptr_t clen1, clen2, used1, used2, origl1, origl2;
  char *c1, *c2, buf1[MZ_SC_BUF_SIZE], buf2[MZ_SC_BUF_SIZE];
  char case_buf1[MZ_SC_BUF_SIZE], case_buf2[MZ_SC_BUF_SIZE];
  int status, got_more;

  /* First, convert UCS-4 to locale-specific encoding. If some
     characters don't fit into the encoding, then we'll have leftover
     characters. Count unconvertable charc as greater than anything
     that can be converted */

  origl1 = l1;
  origl2 = l2;

  /* Loop to check both convertable and unconvertable parts */
  while (1) {
    if (!origl1 && !origl2)
      return 0;
    if (!origl1)
      return -1;
    if (!origl2)
      return 1;

    /* Loop to get consistent parts of the wto strings, in case
       a conversion fails. */
    got_more = 0;
    l1 = origl1;
    l2 = origl2;
    while (1) {
      c1 = do_convert((iconv_t)-1, MZ_UCS4_NAME, NULL, 1,
		      s1, d1 * 4, 4 * l1,
		      buf1, 0, MZ_SC_BUF_SIZE - 1,
		      1 /* grow */, 0, 1 /* terminator size */,
		      &used1, &clen1,
		      &status);
      c2 = do_convert((iconv_t)-1, MZ_UCS4_NAME, NULL, 1,
		      s2, d2 * 4, 4 * l2,
		      buf2, 0, MZ_SC_BUF_SIZE - 1,
		      1 /* grow */, 0, 1 /* terminator size */,
		      &used2, &clen2,
		      &status);

      if ((used1 < 4 * l1) || (used2 < 4 * l2)) {
	if (got_more) {
	  /* Something went wrong. We've already tried to
	     even out the parts that work. Let's give up
	     on the first characters */
	  clen1 = clen2 = 0;
	  break;
	} else if (used1 == used2) {
	  /* Not everything, but both ended at the same point */
	  break;
	} else {
	  /* Pick the smallest */
	  if (used2 < used1) {
	    used1 = used2;
	    got_more = 1;
	  } else
	    got_more = 2;
	  l2 = (used1 >> 2);
	  l1 = (used1 >> 2);

	  if (!l1) {
	    /* Nothing to get this time. */
	    clen1 = clen2 = 0;
	    c1 = c2 = "";
	    used1 = used2 = 0;
	    break;
	  }
	}
      } else
	/* Got all that we wanted */
	break;
    }

    if (cvt_case) {
      if (clen1)
	c1 = locale_recase(0, c1, 0, clen1,
			   case_buf1, 0, MZ_SC_BUF_SIZE - 1,
			   &clen1);
      else
	c1 = NULL;
      if (clen2)
	c2 = locale_recase(0, c2, 0, clen2,
			   case_buf2, 0, MZ_SC_BUF_SIZE - 1,
			   &clen2);
      else
	c2 = NULL;
      /* There shouldn't have been conversion errors, but just in
	 case, care of NULL. */
      if (!c1) c1 = "";
      if (!c2) c2 = "";
    }

    /* Collate, finally. */
    status = strcoll(c1, c2);

    /* If one is bigger than the other, we're done. */
    if (status)
      return status;

    /* Otherwise, is there more to check? */
    origl1 -= (used1 >> 2);
    origl2 -= (used2 >> 2);
    d1 += (used1 >> 2);
    d2 += (used2 >> 2);
    if (!origl1 && !origl2)
      return 0;

    /* There's more. It must be that the next character wasn't
       convertable in one of the encodings. */
    if (got_more)
      return ((got_more == 2) ? 1 : -1);

    if (!origl1)
      return -1;

    /* Compare an unconverable character directly. No case conversions
       if it's outside the locale. */
    if (((unsigned int *)s1)[d1] > ((unsigned int *)s2)[d2])
      return 1;
    else if (((unsigned int *)s1)[d1] < ((unsigned int *)s2)[d2])
      return -1;
    else {
      /* We've skipped one unconvertable char, and they still look the
	 same.  Now try again. */
      origl1 -= 1;
      origl2 -= 1;
      d1 += 1;
      d2 += 1;
    }
  }
}

#ifdef MACOS_UNICODE_SUPPORT
int mz_native_strcoll(char *s1, int d1, int l1, char *s2, int d2, int l2, int cvt_case)
     /* The s1 and s2 arguments are actually UTF-16. */
{
  CFStringRef str1, str2;
  CFComparisonResult r;

  str1 = CFStringCreateWithBytes(NULL, (unsigned char *)s1 XFORM_OK_PLUS (d1 * 2), (l1 * 2), 
				 kCFStringEncodingUnicode, FALSE);
  str2 = CFStringCreateWithBytes(NULL, (unsigned char *)s2 XFORM_OK_PLUS (d2 * 2), (l2 * 2), 
				 kCFStringEncodingUnicode, FALSE);

  r = CFStringCompare(str1, str2, (kCFCompareLocalized
				   | (cvt_case ? kCFCompareCaseInsensitive : 0)));

  CFRelease(str1);
  CFRelease(str2);

  return (int)r;
}
#endif

#ifdef WINDOWS_UNICODE_SUPPORT
int mz_native_strcoll(char *s1, int d1, int l1, char *s2, int d2, int l2, int cvt_case)
     /* The s1 and s2 arguments are actually UTF-16. */
{
  int r;

  r = CompareStringW(LOCALE_USER_DEFAULT,
		     ((cvt_case ? NORM_IGNORECASE : 0)
		      | NORM_IGNOREKANATYPE
		      | NORM_IGNOREWIDTH),
		     (wchar_t *)s1 + d1, l1, (wchar_t *)s2 + d2, l2);

  return r - 2;
}
#endif

typedef int (*strcoll_proc)(char *s1, int d1, int l1, char *s2, int d2, int l2, int cvt_case);

int do_locale_comp(const char *who, const mzchar *us1, intptr_t ul1, const mzchar *us2, intptr_t ul2, int cvt_case)
{
  int xl1;
  int v, endres, utf16 = 0;
  GC_CAN_IGNORE strcoll_proc mz_strcoll = mz_locale_strcoll;

#if defined(MACOS_UNICODE_SUPPORT) || defined(WINDOWS_UNICODE_SUPPORT)
  if (current_locale_name && !*current_locale_name) {
    utf16 = 1;
    mz_strcoll = mz_native_strcoll;
  }
#endif

  if (utf16) {
    us1 = (mzchar *)scheme_ucs4_to_utf16(us1, 0, ul1, NULL, 0, &ul1, 1);
    us2 = (mzchar *)scheme_ucs4_to_utf16(us2, 0, ul2, NULL, 0, &ul2, 1);
    ((short *)us1)[ul1] = 0;
    ((short *)us2)[ul2] = 0;
  }

  if (ul1 > ul2) {
    ul1 = ul2;
    endres = 1;
  } else {
    if (ul2 > ul1)
      endres = -1;
    else
      endres = 0;
  }

  /* Walk back through the strings looking for nul characters. If we
     find one, compare the part after the null character to update
     endres, then continue. Unfortunately, we do too much work if an
     earlier part of the string (tested later) determines the result,
     but hopefully nul characters are rare. */

  xl1 = 0;
  while (ul1--) {
    if ((utf16 && (!(((short *)us1)[ul1]) || !(((short *)us2)[ul1])))
	|| (!utf16 && (!(us1[ul1]) || !(us2[ul1])))) {
      if (utf16) {
	if (((short *)us1)[ul1])
	  endres = 1;
	else if (((short *)us2)[ul1])
	  endres = -1;
      } else {
	if (us1[ul1])
	  endres = 1;
	else if (us2[ul1])
	  endres = -1;
      }

      if (xl1)
	v = mz_strcoll((char *)us1, ul1 + 1, xl1, (char *)us2, ul1 + 1, xl1, cvt_case);
      else
	v = 0;

      if (v)
	endres = v;
      xl1 = 0;
    } else {
      xl1++;
    }
  }

  v = mz_strcoll((char *)us1, 0, xl1, (char *)us2, 0, xl1, cvt_case);
  if (v)
    endres = v;

  return endres;
}


mzchar *do_locale_recase(int to_up, mzchar *in, int delta, int len, intptr_t *olen)
{
  Scheme_Object *parts = scheme_null;
  char *c, buf[MZ_SC_BUF_SIZE], case_buf[MZ_SC_BUF_SIZE];
  intptr_t clen, used;
  int status;

  while (len) {
    /* We might have conversion errors... */
    c = do_convert((iconv_t)-1, MZ_UCS4_NAME, NULL, 1,
		   (char *)in, 4 * delta, 4 * len,
		   buf, 0, MZ_SC_BUF_SIZE - 1,
		   1 /* grow */, 0, 1 /* terminator size */,
		   &used, &clen,
		   &status);

    used >>= 2;
    delta += used;
    len -= used;

    c = locale_recase(to_up, c, 0, clen,
		      case_buf, 0, MZ_SC_BUF_SIZE - 1,
		      &clen);
    if (!c)
      clen = 0;

    c = do_convert((iconv_t)-1, NULL, MZ_UCS4_NAME, 2,
		   c, 0, clen,
		   NULL, 0, 0,
		   1 /* grow */, 0, sizeof(mzchar) /* terminator size */,
		   &used, &clen,
		   &status);

    if (!len && SCHEME_NULLP(parts)) {
      *olen = (clen >> 2);
      ((mzchar *)c)[*olen] = 0;
      return (mzchar *)c;
    }

    /* We can get here if there was some conversion error at some
       point. We're building up a list of parts. */

    parts = scheme_make_pair(scheme_make_sized_char_string((mzchar *)c, clen >> 2, 0),
			     parts);

    if (len) {
      /* Conversion error, so skip one char. */
      parts = scheme_make_pair(scheme_make_sized_offset_char_string(in, delta, 1, 1),
			       parts);
      delta += 1;
      len -= 1;
    }
  }

  parts = append_all_strings_backwards(parts);
  *olen = SCHEME_CHAR_STRTAG_VAL(parts);

  return SCHEME_CHAR_STR_VAL(parts);
}

#ifdef MACOS_UNICODE_SUPPORT
mzchar *do_native_recase(int to_up, mzchar *in, int delta, int len, intptr_t *olen)
     /* The in argument is actually UTF-16. */
{
  CFMutableStringRef mstr;
  CFStringRef str;
  GC_CAN_IGNORE CFRange rng;
  char *result;

  str = CFStringCreateWithBytes(NULL, ((unsigned char *)in) XFORM_OK_PLUS (delta * 2), (len * 2), 
				kCFStringEncodingUnicode, FALSE);
  mstr = CFStringCreateMutableCopy(NULL, 0, str);
  CFRelease(str);

  if (to_up)
    CFStringUppercase(mstr, NULL);
  else
    CFStringLowercase(mstr, NULL);

  len = CFStringGetLength(mstr);
  *olen = len;

  result = (char *)scheme_malloc_atomic((len + 1) * 2);

  rng = CFRangeMake(0, len);
  CFStringGetCharacters(mstr, rng, (UniChar *)result);
  CFRelease(mstr);

  ((UniChar *)result)[len] = 0;

  return (mzchar *)result;
}
#endif

#ifdef WINDOWS_UNICODE_SUPPORT
mzchar *do_native_recase(int to_up, mzchar *in, int delta, int len, intptr_t *olen)
     /* The in argument is actually UTF-16. */
{
  char *result;

  result = (char *)scheme_malloc_atomic((len + 1) * 2);
  memcpy(result, ((char *)in) + (2 * delta), len * 2);
  ((wchar_t*)result)[len] = 0;

  if (to_up)
    CharUpperBuffW((wchar_t *)result, len);
  else
    CharLowerBuffW((wchar_t *)result, len);

  *olen = len;
  return (mzchar *)result;
}
#endif

typedef mzchar *(*recase_proc)(int to_up, mzchar *in, int delta, int len, intptr_t *olen);

static Scheme_Object *mz_recase(const char *who, int to_up, mzchar *us, intptr_t ulen)
{
  intptr_t ulen1;
  int utf16 = 0, i, delta = 0;
  mzchar *us1;
  recase_proc mz_do_recase = do_locale_recase;
  Scheme_Object *s, *parts = scheme_null;

  reset_locale();

#if defined(MACOS_UNICODE_SUPPORT) || defined(WINDOWS_UNICODE_SUPPORT)
  if (current_locale_name && !*current_locale_name) {
    utf16 = 1;
    mz_do_recase = do_native_recase;
  }
#endif

  if (utf16) {
    us = (mzchar *)scheme_ucs4_to_utf16(us, 0, ulen, NULL, 0, &ulen, 1);
    ((short *)us)[ulen] = 0;
  }

  /* If there are nulls in the string, then we have to make multiple
     calls to mz_do_recase */
  i = 0;
  while (1) {
    for (; i < ulen; i++) {
      if (utf16) {
	if (!((short *)us)[i])
	  break;
      } else if (!us[i])
	break;
    }

    us1 = mz_do_recase(to_up, us, delta, i - delta, &ulen1);

    if (utf16) {
      us1 = scheme_utf16_to_ucs4((unsigned short *)us1, 0, ulen1, NULL, 0, &ulen1, 1);
      us1[ulen1] = 0;
    }

    s = scheme_make_sized_char_string((mzchar *)us1, ulen1, 0);

    if (SCHEME_NULLP(parts) && (i == ulen))
      return s;

    parts = scheme_make_pair(s, parts);

    if (i == ulen)
      break;

    /* upcasing and encoding a nul char is easy: */
    s = scheme_make_sized_char_string((mzchar *)"\0\0\0\0", 1, 0);
    parts = scheme_make_pair(s, parts);
    i++;
    delta = i;

    if (i == ulen)
      break;
  }

  return append_all_strings_backwards(parts);
}

#endif

static Scheme_Object *
unicode_recase(const char *who, int to_up, int argc, Scheme_Object *argv[])
{
  intptr_t len;
  mzchar *chars;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract(who, "string?", 0, argc, argv);

  chars = SCHEME_CHAR_STR_VAL(argv[0]);
  len = SCHEME_CHAR_STRTAG_VAL(argv[0]);

  return mz_recase(who, to_up, chars, len);
}

static Scheme_Object *
string_locale_upcase(int argc, Scheme_Object *argv[])
{
  return unicode_recase("string-locale-upcase", 1, argc, argv);
}

static Scheme_Object *
string_locale_downcase(int argc, Scheme_Object *argv[])
{
  return unicode_recase("string-locale-downcase", 0, argc, argv);
}

static void reset_locale(void)
{
  Scheme_Object *v;
  const mzchar *name;

  /* This function needs to work before threads are set up: */
  if (scheme_current_thread) {
    v = scheme_get_param(scheme_current_config(), MZCONFIG_LOCALE);
  } else {
    v = scheme_make_immutable_sized_utf8_string("", 0);
  }
  locale_on = SCHEME_TRUEP(v);

  if (locale_on) {
    name = SCHEME_CHAR_STR_VAL(v);
#ifndef DONT_USE_LOCALE
    if ((current_locale_name != name)
        && (!current_locale_name
            || mz_char_strcmp("result-locale",
                              current_locale_name, scheme_char_strlen(current_locale_name),
                              name, SCHEME_CHAR_STRLEN_VAL(v),
                              0, 1))) {
      /* We only need CTYPE and COLLATE; two calls seem to be much
	 faster than one call with ALL */
      char *n, buf[32];

      n = scheme_utf8_encode_to_buffer(name, SCHEME_CHAR_STRLEN_VAL(v), buf, 32);

      if (!setlocale(LC_CTYPE, n))
	setlocale(LC_CTYPE, "C");
      if (!setlocale(LC_COLLATE, n))
	setlocale(LC_COLLATE, "C");
    }
#endif
    current_locale_name_ptr = (void *)name;
  }
}

char *scheme_push_c_numeric_locale()
{
#ifndef DONT_USE_LOCALE
  GC_CAN_IGNORE char *prev;
  prev = setlocale(LC_NUMERIC, NULL);
  if (!strcmp(prev, "C"))
    return NULL;
  else
    return setlocale(LC_NUMERIC, "C");
#endif  
}

void scheme_pop_c_numeric_locale(char *prev)
{
#ifndef DONT_USE_LOCALE
  if (prev)
    setlocale(LC_NUMERIC, prev);
#endif  
}

static int find_special_casing(int ch)
{
  /* Binary search */
  int i, lo, hi, j;

  i = NUM_SPECIAL_CASINGS >> 1;
  lo = i;
  hi = NUM_SPECIAL_CASINGS - i - 1;

  while (1) {
    if (uchar_special_casings[i * 10] == ch)
      return i * 10;
    if (uchar_special_casings[i * 10] > ch) {
      j = i - lo;
      i = j + (lo >> 1);
      hi = lo - (i - j) - 1;
      lo = i - j;
    } else {
      j = i + 1;
      i = j + (hi >> 1);
      lo = i - j;
      hi = hi - (i - j) - 1;
    }
  }
}

static int is_final_sigma(int mode, mzchar *s, int d, int i, int len)
{
  int j;

  if (mode == 3)
    return 1;
  
  /* find a cased char before, skipping case-ignorable: */
  for (j = i - 1; j >= d; j--) {
    if (!scheme_iscaseignorable(s[j])) {
      if (scheme_iscased(s[j]))
	break;
      else
	return 0;
    }
  }
  if (j < d)
    return 0;

  /* next non-case-ignorable must not be cased: */
  for (j = i + 1; j < d + len; j++) {
    if (!scheme_iscaseignorable(s[j])) {
      return !scheme_iscased(s[j]);
    }
  }

  return 1;
}

mzchar *scheme_string_recase(mzchar *s, int d, int len, int mode, int inplace, int *_len)
{
  mzchar *t;
  int i, extra = 0, pos, special = 0, td, prev_was_cased = 0, xmode = mode;

  for (i = 0; i < len; i++) {
    if (scheme_isspecialcasing(s[d+i])) {
      pos = find_special_casing(s[d+i]);
      if (!uchar_special_casings[pos + 9] || is_final_sigma(xmode, s, d, i, len)) {
	special = 1;
	extra += (uchar_special_casings[pos + 1 + (xmode << 1)] - 1);
      }
    }
    if (mode == 2) {
      if (!scheme_iscaseignorable(s[d+i]))
	prev_was_cased = scheme_iscased(s[d+i]);
      xmode = (prev_was_cased ? 0 : 2);
    }
  }

  if (_len)
    *_len = len + extra;

  if (!extra && inplace) {
    t = s;
    td = d;
  } else {
    t = scheme_malloc_atomic(sizeof(mzchar) * (len + extra + 1));
    td = 0;
  }

  if (!special) {
    if (mode == 0) {
      for (i = 0; i < len; i++) {
	t[i+td] = scheme_tolower(s[i+d]);
      }
    } else if (mode == 1) {
      for (i = 0; i < len; i++) {
	t[i+td] = scheme_toupper(s[i+d]);
      }
    } else if (mode == 2) {
      prev_was_cased = 0;
      for (i = 0; i < len; i++) {
	if (!prev_was_cased)
	  t[i+td] = scheme_totitle(s[i+d]);
	else
	  t[i+td] = scheme_tolower(s[i+d]);
	if (!scheme_iscaseignorable(s[i+d]))
	  prev_was_cased = scheme_iscased(s[i+d]);
      }
    } else /* if (mode == 3) */ {
      for (i = 0; i < len; i++) {
	t[i+td] = scheme_tofold(s[i+d]);
      }
    }
  } else {
    int j = 0, c;
    prev_was_cased = 0;
    for (i = 0; i < len; i++) {
      if (mode == 0) {
	t[j+td] = scheme_tolower(s[i+d]);
      } else if (mode == 1) {
	t[j+td] = scheme_toupper(s[i+d]);
      } else if (mode == 2) {
	if (!prev_was_cased) {
	  xmode = 2;
	  t[j+td] = scheme_totitle(s[i+d]);
	} else {
	  xmode = 0;
	  t[j+td] = scheme_tolower(s[i+d]);
	}
	if (!scheme_iscaseignorable(s[i+d]))
	  prev_was_cased = scheme_iscased(s[i+d]);
      } else /* if (mode == 3) */ {
	t[j+td] = scheme_tofold(s[i+d]);
      }

      if (scheme_isspecialcasing(s[i+d])) {
	pos = find_special_casing(s[i+d]);
	if (!uchar_special_casings[pos + 9] || is_final_sigma(xmode, s, d, i, len)) {
	  c = uchar_special_casings[pos + 1 + (xmode << 1)];
	  pos = uchar_special_casings[pos + 2 + (xmode << 1)];
	  while (c--) {
	    t[(j++)+td] = uchar_special_casing_data[pos++];
	  }
	} else
	  j++;
      } else
	j++;
    }
  }
  t[len+extra+td] = 0;

  return t;
}

static Scheme_Object *string_recase (const char *name, int argc, Scheme_Object *argv[], int mode)
{
  mzchar *s;
  int len;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract(name, "string?", 0, argc, argv);
  
  s = SCHEME_CHAR_STR_VAL(argv[0]);
  len = SCHEME_CHAR_STRLEN_VAL(argv[0]);

  s = scheme_string_recase(s, 0, len, mode, 0, &len);

  return scheme_make_sized_char_string(s, len, 0);
}

static Scheme_Object *string_upcase (int argc, Scheme_Object *argv[])
{
  return string_recase("string-upcase", argc, argv, 1);
}

static Scheme_Object *string_downcase (int argc, Scheme_Object *argv[])
{
  return string_recase("string-downcase", argc, argv, 0);
}

static Scheme_Object *string_titlecase (int argc, Scheme_Object *argv[])
{
  return string_recase("string-titlecase", argc, argv, 2);
}

static Scheme_Object *string_foldcase (int argc, Scheme_Object *argv[])
{
  return string_recase("string-foldcase", argc, argv, 3);
}

/**********************************************************************/
/*                          normalization                             */
/**********************************************************************/

#define MZ_JAMO_INITIAL_CONSONANT_START  0x1100
#define MZ_JAMO_INITIAL_CONSONANT_COUNT  19
#define MZ_JAMO_INITIAL_CONSONANT_END    (MZ_JAMO_INITIAL_CONSONANT_START + MZ_JAMO_INITIAL_CONSONANT_COUNT - 1)

#define MZ_JAMO_VOWEL_START              0x1161
#define MZ_JAMO_VOWEL_COUNT              21
#define MZ_JAMO_VOWEL_END                (MZ_JAMO_VOWEL_START + MZ_JAMO_VOWEL_COUNT - 1)

/* First in this range is not actually a consonant, but a placeholder for "no consonant" */
#define MZ_JAMO_TRAILING_CONSONANT_START 0x11A7
#define MZ_JAMO_TRAILING_CONSONANT_COUNT 28
#define MZ_JAMO_TRAILING_CONSONANT_END   (MZ_JAMO_TRAILING_CONSONANT_START + MZ_JAMO_TRAILING_CONSONANT_COUNT - 1)

#define MZ_JAMO_SYLLABLE_START           0xAC00
#define MZ_JAMO_SYLLABLE_END             (MZ_JAMO_SYLLABLE_START + 11171)

static mzchar get_composition(mzchar a, mzchar b)
{
  uintptr_t key = (a << 16) | b;
  int pos = (COMPOSE_TABLE_SIZE >> 1), new_pos;
  int below_len = pos;
  int above_len = (COMPOSE_TABLE_SIZE - pos - 1);
  
  if (a > 0xFFFF) return 0;

  /* Binary search: */
  while (key != utable_compose_pairs[pos]) {
    if (key > utable_compose_pairs[pos]) {
      if (!above_len)
	return 0;
      new_pos = pos + (above_len >> 1) + 1;
      below_len = (new_pos - pos - 1);
      above_len = (above_len - below_len - 1);
      pos = new_pos;
    } else if (key < utable_compose_pairs[pos]) {
      if (!below_len)
	return 0;
      new_pos = pos - ((below_len >> 1) + 1);
      above_len = (pos - new_pos - 1);
      below_len = (below_len - above_len - 1);
      pos = new_pos;
    }
  }

  return utable_compose_result[pos];
}

mzchar get_canon_decomposition(mzchar key, mzchar *b)
{
  int pos = (DECOMPOSE_TABLE_SIZE >> 1), new_pos;
  int below_len = pos;
  int above_len = (DECOMPOSE_TABLE_SIZE - pos - 1);

  /* Binary search: */
  while (key != utable_decomp_keys[pos]) {
    if (key > utable_decomp_keys[pos]) {
      if (!above_len)
	return 0;
      new_pos = pos + (above_len >> 1) + 1;
      below_len = (new_pos - pos - 1);
      above_len = (above_len - below_len - 1);
      pos = new_pos;
    } else if (key < utable_decomp_keys[pos]) {
      if (!below_len)
	return 0;
      new_pos = pos - ((below_len >> 1) + 1);
      above_len = (pos - new_pos - 1);
      below_len = (below_len - above_len - 1);
      pos = new_pos;
    }
  }

  pos = utable_decomp_indices[pos];
  if (pos < 0) {
    pos = -(pos + 1);
    pos <<= 1;
    *b = utable_compose_long_pairs[pos + 1];
    return utable_compose_long_pairs[pos];
  } else {
    key = utable_compose_pairs[pos];
    *b = (key & 0xFFFF);
    return (key >> 16);
  }
}

int get_kompat_decomposition(mzchar key, unsigned short **chars)
{
  int pos = (KOMPAT_DECOMPOSE_TABLE_SIZE >> 1), new_pos;
  int below_len = pos;
  int above_len = (KOMPAT_DECOMPOSE_TABLE_SIZE - pos - 1);

  /* Binary search: */
  while (key != utable_kompat_decomp_keys[pos]) {
    if (key > utable_kompat_decomp_keys[pos]) {
      if (!above_len)
	return 0;
      new_pos = pos + (above_len >> 1) + 1;
      below_len = (new_pos - pos - 1);
      above_len = (above_len - below_len - 1);
      pos = new_pos;
    } else if (key < utable_kompat_decomp_keys[pos]) {
      if (!below_len)
	return 0;
      new_pos = pos - ((below_len >> 1) + 1);
      above_len = (pos - new_pos - 1);
      below_len = (below_len - above_len - 1);
      pos = new_pos;
    }
  }

  *chars = utable_kompat_decomp_strs XFORM_OK_PLUS utable_kompat_decomp_indices[pos];
  return utable_kompat_decomp_lens[pos];
}

static Scheme_Object *normalize_c(Scheme_Object *o)
/* Assumes then given string is in normal form D */
{
  mzchar *s, *s2, tmp, last_c0 = 0;
  int len, i, j = 0, last_c0_pos = 0, last_cc = 0;

  s = SCHEME_CHAR_STR_VAL(o);
  len = SCHEME_CHAR_STRLEN_VAL(o);

  s2 = (mzchar *)scheme_malloc_atomic((len + 1) * sizeof(mzchar));
  memcpy(s2, s, len * sizeof(mzchar));
  
  for (i = 0; i < len; i++) {
    if ((i + 1 < len)
	&& (s2[i] >= MZ_JAMO_INITIAL_CONSONANT_START)
	&& (s2[i] <= MZ_JAMO_INITIAL_CONSONANT_END)
	&& (s2[i+1] >= MZ_JAMO_VOWEL_START)
	&& (s2[i+1] <= MZ_JAMO_VOWEL_END)) {
      /* Need Hangul composition */
      if ((i + 2 < len)
	  && (s2[i+2] > MZ_JAMO_TRAILING_CONSONANT_START)
	  && (s2[i+2] <= MZ_JAMO_TRAILING_CONSONANT_END)) {
	/* 3-char composition */
	tmp = (MZ_JAMO_SYLLABLE_START
	       + ((s2[i] - MZ_JAMO_INITIAL_CONSONANT_START) 
		  * MZ_JAMO_VOWEL_COUNT * MZ_JAMO_TRAILING_CONSONANT_COUNT)
	       + ((s2[i+1] - MZ_JAMO_VOWEL_START)
		  * MZ_JAMO_TRAILING_CONSONANT_COUNT)
	       + (s2[i+2] - MZ_JAMO_TRAILING_CONSONANT_START));
	i += 2;
      } else {
	/* 2-char composition */
	tmp = (MZ_JAMO_SYLLABLE_START
	       + ((s2[i] - MZ_JAMO_INITIAL_CONSONANT_START) 
		  * MZ_JAMO_VOWEL_COUNT * MZ_JAMO_TRAILING_CONSONANT_COUNT)
	       + ((s2[i+1] - MZ_JAMO_VOWEL_START)
		  * MZ_JAMO_TRAILING_CONSONANT_COUNT));
	i++;
      }
      last_c0 = tmp;
      last_c0_pos = j;
      last_cc = 0;
      s2[j++] = tmp;
    } else {
      int cc;
      
      cc = scheme_combining_class(s2[i]);
      if (last_c0 && (cc > last_cc))
	tmp = get_composition(last_c0, s2[i]);
      else
	tmp = 0;

      if (tmp) {
	/* Need to compose */
	s2[last_c0_pos] = tmp;
	last_c0 = tmp;
      } else if (!cc) {
	/* Reset last_c0... */
	tmp = s2[i];
	if (scheme_needs_maybe_compose(tmp)) {
	  last_c0 = tmp;
	  last_c0_pos = j;
	} else {
	  last_c0 = 0;
	}
	last_cc = -1;
	s2[j++] = tmp;
      } else {
	s2[j++] = s2[i];
	last_cc = cc;
      }
    }
  }

  s2[j] = 0;
  if (len - j > 16) {
    s = (mzchar *)scheme_malloc_atomic((j + 1) * sizeof(mzchar));
    memcpy(s, s2, (j + 1) * sizeof(mzchar));
    s2 = s;
  }

  return scheme_make_sized_char_string(s2, j, 0);
}

static Scheme_Object *normalize_d(Scheme_Object *o, int kompat)
{
  mzchar *s, tmp, *s2;
  int len, i, delta, j, swapped;

  s = SCHEME_CHAR_STR_VAL(o);
  len = SCHEME_CHAR_STRLEN_VAL(o);

  /* Run through string list to predict expansion: */
  delta = 0;
  for (i = 0; i < len; i++) {
    if (scheme_needs_decompose(s[i])) {
      int klen;
      mzchar snd;
      GC_CAN_IGNORE unsigned short *start;

      tmp = s[i];
      while (scheme_needs_decompose(tmp)) {
	if (kompat)
	  klen = get_kompat_decomposition(tmp, &start);
	else
	  klen = 0;
	if (klen) {
	  delta += (klen - 1);
	  break;
	} else {
	  tmp = get_canon_decomposition(tmp, &snd);
	  if (tmp) {
	    if (snd) {
	      delta++;
	      if (kompat) {
		klen = get_kompat_decomposition(snd, &start);
		if (klen)
		  delta += (klen - 1);
	      }
	    }
	  } else
	    break;
	}
      }
    } else if ((s[i] >= MZ_JAMO_SYLLABLE_START)
	       && (s[i] <= MZ_JAMO_SYLLABLE_END)) {
      tmp = s[i];
      tmp -= MZ_JAMO_SYLLABLE_START;
      if (tmp % MZ_JAMO_TRAILING_CONSONANT_COUNT)
	delta += 2;
      else
	delta += 1;
    }
  }

  s2 = (mzchar *)scheme_malloc_atomic((len + delta + 1) * sizeof(mzchar));

  j = 0;
  for (i = 0; i < len; i++) {
    if (scheme_needs_decompose(s[i])) {
      mzchar snd, tmp2;
      int snds = 0, klen = 0, k;
      GC_CAN_IGNORE unsigned short*start;

      tmp = s[i];
      while (scheme_needs_decompose(tmp)) {
	if (kompat)
	  klen = get_kompat_decomposition(tmp, &start);
	else
	  klen = 0;
	if (klen) {
	  for (k = 0; k < klen; k++) {
	    s2[j++] = start[k];
	  }
	  break;
	} else {
	  tmp2 = get_canon_decomposition(tmp, &snd);
	  if (tmp2) {
	    tmp = tmp2;
	    if (snd) {
	      if (kompat)
		klen = get_kompat_decomposition(snd, &start);
	      else
		klen = 0;
	      if (klen) {
		snds += klen;
		for (k = 0; k < klen; k++) {
		  s2[len + delta - snds + k] = start[k];
		}
		klen = 0;
	      } else {
		snds++;
		s2[len + delta - snds] = snd;
	      }
	    }
	  } else 
	    break;
	}
      }
      if (!klen)
	s2[j++] = tmp;
      memcpy(s2 + j, s2 + len + delta - snds, snds * sizeof(mzchar));
      j += snds;
    } else if ((s[i] >= MZ_JAMO_SYLLABLE_START)
	       && (s[i] <= MZ_JAMO_SYLLABLE_END)) {
      int l, v, t;
      tmp = s[i];
      tmp -= MZ_JAMO_SYLLABLE_START;
      l = tmp / (MZ_JAMO_VOWEL_COUNT * MZ_JAMO_TRAILING_CONSONANT_COUNT);
      v = (tmp % (MZ_JAMO_VOWEL_COUNT * MZ_JAMO_TRAILING_CONSONANT_COUNT)) / MZ_JAMO_TRAILING_CONSONANT_COUNT;
      t = tmp % MZ_JAMO_TRAILING_CONSONANT_COUNT;
      s2[j++] = MZ_JAMO_INITIAL_CONSONANT_START + l;
      s2[j++] = MZ_JAMO_VOWEL_START + v;
      if (t) {
	s2[j++] = MZ_JAMO_TRAILING_CONSONANT_START + t;
      }
    } else {
      s2[j++] = s[i];
    }
  }
  s2[j] = 0;
  len += delta;

  /* Reorder pass: */
  do {
    swapped = 0;
    for (i = 0; i < len; i++) {
      if ((i + 1 < len)
	  && scheme_combining_class(s2[i])
	  && scheme_combining_class(s2[i+1])
	  && (scheme_combining_class(s2[i+1]) < scheme_combining_class(s2[i]))) {
	/* Reorder and try again: */
	tmp = s2[i + 1];
	s2[i + 1] = s2[i];
	s2[i] = tmp;
	i--;
	swapped = 1;
      }
    }
  } while (swapped);

  return scheme_make_sized_char_string(s2, len, 0);
}

static Scheme_Object *do_string_normalize_c (const char *who, int argc, Scheme_Object *argv[], int kompat)
{
  Scheme_Object *o;
  mzchar *s, last_c0 = 0, snd;
  int len, i, last_cc = 0;

  o = argv[0];
  if (!SCHEME_CHAR_STRINGP(o))
    scheme_wrong_contract(who, "string?", 0, argc, argv);

  s = SCHEME_CHAR_STR_VAL(o);
  len = SCHEME_CHAR_STRLEN_VAL(o);

  for (i = 0; i < len; i++) {
    if (scheme_needs_decompose(s[i])
	&& (kompat || get_canon_decomposition(s[i], &snd))) {
      /* Decomposition may expose a different composition */
      break;
    } else if ((i + 1 < len)
	&& scheme_combining_class(s[i])
	&& scheme_combining_class(s[i+1])
	&& (scheme_combining_class(s[i+1]) < scheme_combining_class(s[i]))) {
      /* Need to reorder */
      break;
    } else if ((s[i] >= MZ_JAMO_INITIAL_CONSONANT_START)
	       && (s[i] <= MZ_JAMO_INITIAL_CONSONANT_END)
	       && (s[i+1] >= MZ_JAMO_VOWEL_START)
	       && (s[i+1] <= MZ_JAMO_VOWEL_END)) {
      /* Need Hangul composition */
      break;
    } else if (last_c0 
	       && get_composition(last_c0, s[i])
	       && (scheme_combining_class(s[i]) > last_cc)) {
      /* Need to compose */
      break;
    } else {
      int cc;

      cc = scheme_combining_class(s[i]);

      if (!cc) {
	if (scheme_needs_maybe_compose(s[i]))
	  last_c0 = s[i];
	else
	  last_c0 = 0;
	last_cc = -1;
      } else
	last_cc = cc;
    }
  }

  if (i < len) {
    o = normalize_c(normalize_d(o, kompat));
  }

  return o;
}

static Scheme_Object *string_normalize_c (int argc, Scheme_Object *argv[])
{
  return do_string_normalize_c("string-normalize-nfc", argc, argv, 0);
}

static Scheme_Object *string_normalize_kc (int argc, Scheme_Object *argv[])
{
  return do_string_normalize_c("string-normalize-nfkc", argc, argv, 1);
}

static Scheme_Object *do_string_normalize_d (const char *who, int argc, Scheme_Object *argv[], int kompat)
{
  Scheme_Object *o;
  mzchar *s;
  int len, i;

  o = argv[0];
  if (!SCHEME_CHAR_STRINGP(o))
    scheme_wrong_contract(who, "string?", 0, argc, argv);

  s = SCHEME_CHAR_STR_VAL(o);
  len = SCHEME_CHAR_STRLEN_VAL(o);

  for (i = len; i--; ) {
    if (scheme_needs_decompose(s[i])) {
      /* Need to decompose */
      mzchar snd;
      if (kompat || get_canon_decomposition(s[i], &snd))
	break;
    } else if ((i + 1 < len)
	       && scheme_combining_class(s[i])
	       && scheme_combining_class(s[i+1])
	       && (scheme_combining_class(s[i+1]) < scheme_combining_class(s[i]))) {
      /* Need to reorder */
      break;
    } else if ((s[i] >= MZ_JAMO_SYLLABLE_START)
	       && (s[i] <= MZ_JAMO_SYLLABLE_END)) {
      /* Need Hangul decomposition */
      break;
    }
  }

  if (i >= 0) {
    o = normalize_d(o, kompat);
  }

  return o;
}

static Scheme_Object *string_normalize_d (int argc, Scheme_Object *argv[])
{
  return do_string_normalize_d("string-normalize-nfd", argc, argv, 0);
}

static Scheme_Object *string_normalize_kd (int argc, Scheme_Object *argv[])
{
  return do_string_normalize_d("string-normalize-nfkd", argc, argv, 1);
}

/**********************************************************************/
/*                            strcmps                                 */
/**********************************************************************/

intptr_t scheme_char_strlen(const mzchar *s)
{
  intptr_t i;
  for (i = 0; s[i]; i++) {
  }
  return i;
}

static int mz_char_strcmp(const char *who, const mzchar *str1, intptr_t l1, const mzchar *str2, intptr_t l2, 
			  int use_locale, int size_shortcut)
{
  intptr_t endres;

  if (size_shortcut && (l1 != l2))
    return 1;

#ifndef DONT_USE_LOCALE
  if (use_locale) {
    reset_locale();
    if (locale_on) {
      return do_locale_comp(who, str1, l1, str2, l2, 0);
    }
  }
#endif

  if (l1 > l2) {
    l1 = l2;
    endres = 1;
  } else {
    if (l2 > l1)
      endres = -1;
    else
      endres = 0;
  }

  while (l1--) {
    unsigned int a, b;

    a = *(str1++);
    b = *(str2++);

    a = a - b;
    if (a)
      return a;
  }

  return endres;
}

static int mz_char_strcmp_ci(const char *who, const mzchar *str1, intptr_t l1, const mzchar *str2, intptr_t l2, 
			     int use_locale, int size_shortcut)
{
  intptr_t p1, p2, sp1, sp2, a, b;
  mzchar spec1[SPECIAL_CASE_FOLD_MAX], spec2[SPECIAL_CASE_FOLD_MAX];

  if (size_shortcut && (l1 != l2))
    return 1;

#ifndef DONT_USE_LOCALE
  if (use_locale) {
    reset_locale();
    if (locale_on) {
      return do_locale_comp(who, str1, l1, str2, l2, 1);
    }
  }
#endif

  p1 = sp1 = 0;
  p2 = sp2 = 0;

  while (((p1 < l1) || sp1) && ((p2 < l2) || sp2)) {
    if (sp1) {
      a = spec1[--sp1];
    } else {
      a = str1[p1];
      if (scheme_isspecialcasing(a)) {
	int pos, i;
	pos = find_special_casing(a);
	sp1 = uchar_special_casings[pos + 7];
	pos = uchar_special_casings[pos + 8];
	for (i = sp1; i--; pos++) {
	  spec1[i] = uchar_special_casing_data[pos];
	}
	a = spec1[--sp1];
      } else {
	a = scheme_tofold(a);
      }
      p1++;
    }

    if (sp2) {
      b = spec2[--sp2];
    } else {
      b = str2[p2];
      if (scheme_isspecialcasing(b)) {
	int pos, i;
	pos = find_special_casing(b);
	sp2 = uchar_special_casings[pos + 7];
	pos = uchar_special_casings[pos + 8];
	for (i = sp2; i--; pos++) {
	  spec2[i] = uchar_special_casing_data[pos];
	}
	b = spec2[--sp2];
      } else {
	b = scheme_tofold(b);
      }
      p2++;
    }

    a = a - b;
    if (a)
      return a;
  }

  return ((p1 < l1) || sp1) - ((p2 < l2) || sp2);
}

static int mz_strcmp(const char *who, unsigned char *str1, intptr_t l1, unsigned char *str2, intptr_t l2)
{
  intptr_t endres;

  if (l1 > l2) {
    l1 = l2;
    endres = 1;
  } else {
    if (l2 > l1)
      endres = -1;
    else
      endres = 0;
  }

  while (l1--) {
    unsigned int a, b;

    a = *(str1++);
    b = *(str2++);

    a = a - b;
    if (a)
      return a;
  }

  return endres;
}

/**********************************************************************/
/*                  byte string conversion                            */
/**********************************************************************/

static void close_converter(Scheme_Object *o, void *data)
{
  Scheme_Converter *c = (Scheme_Converter *)o;

  if (!c->closed) {
    c->closed = 1;
    if (c->kind == mzICONV_KIND) {
      iconv_close(c->cd);
      c->cd = (iconv_t)-1;
    }
    if (c->mref) {
      scheme_remove_managed(c->mref, (Scheme_Object *)c);
      c->mref = NULL;
    }
  }
}

Scheme_Object *scheme_open_converter(const char *from_e, const char *to_e)
{
  Scheme_Converter *c;
  iconv_t cd;
  int kind;
  int permissive;
  int need_regis = 1;
  Scheme_Custodian_Reference *mref;

  if (!iconv_ready) init_iconv();

  if (!*to_e || !*from_e)
    reset_locale();

  if ((!strcmp(from_e, "UTF-8")
       || !strcmp(from_e, "UTF-8-permissive")
       || (!*from_e && mzLOCALE_IS_UTF_8(current_locale_name)))
      && (!strcmp(to_e, "UTF-8")
	  || (!*to_e && mzLOCALE_IS_UTF_8(current_locale_name)))) {
    /* Use the built-in UTF-8<->UTF-8 converter: */
    kind = mzUTF8_KIND;
    if (!strcmp(from_e, "UTF-8-permissive"))
      permissive = 0xFFFD;
    else
      permissive = 0;
    cd = (iconv_t)-1;
    need_regis = (*to_e && *from_e);
  } else if ((!strcmp(from_e, "platform-UTF-8")
	      || !strcmp(from_e, "platform-UTF-8-permissive"))
	     && !strcmp(to_e, "platform-UTF-16")) {
    kind = mzUTF8_TO_UTF16_KIND;
    if (!strcmp(from_e, "platform-UTF-8-permissive"))
      permissive = 0xFFFD;
    else
      permissive = 0;
    cd = (iconv_t)-1;
    need_regis = 0;
  } else if (!strcmp(from_e, "platform-UTF-16")
	     && !strcmp(to_e, "platform-UTF-8")) {
    kind = mzUTF16_TO_UTF8_KIND;
    permissive = 0;
    cd = (iconv_t)-1;
    need_regis = 0;
  } else {
    if (!iconv_ready) init_iconv();

    if (!mzCHK_PROC(iconv_open))
      return scheme_false;

    if (!*from_e || !*to_e)
      reset_locale();

    if (!*from_e)
      from_e = mz_iconv_nl_langinfo();
    if (!*to_e)
      to_e = mz_iconv_nl_langinfo();
    cd = iconv_open(to_e, from_e);

    if (cd == (iconv_t)-1)
      return scheme_false;

    kind = mzICONV_KIND;
    permissive = 0;
  }

  c = MALLOC_ONE_TAGGED(Scheme_Converter);
  c->so.type = scheme_string_converter_type;
  c->closed = 0;
  c->kind = kind;
  c->permissive = permissive;
  c->cd = cd;
  if (!need_regis)
    mref = NULL;
  else
    mref = scheme_add_managed(NULL,
			      (Scheme_Object *)c,
			      close_converter,
			      NULL, 1);
  c->mref = mref;

  return (Scheme_Object *)c;
}

static Scheme_Object *byte_string_open_converter(int argc, Scheme_Object **argv)
{
  Scheme_Object *s1, *s2;
  char *from_e, *to_e;
  
  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("bytes-open-converter", "bytes?", 0, argc, argv);
  if (!SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_contract("bytes-open-converter", "bytes?", 1, argc, argv);

  scheme_custodian_check_available(NULL, "bytes-open-converter", "converter");

  s1 = scheme_char_string_to_byte_string(argv[0]);
  s2 = scheme_char_string_to_byte_string(argv[1]);

  if (scheme_byte_string_has_null(s1))
    return scheme_false;
  if (scheme_byte_string_has_null(s2))
    return scheme_false;

  from_e = SCHEME_BYTE_STR_VAL(s1);
  to_e = SCHEME_BYTE_STR_VAL(s2);

  return scheme_open_converter(from_e, to_e);
}

static Scheme_Object *convert_one(const char *who, int opos, int argc, Scheme_Object *argv[])
{
  char *r, *instr;
  int status;
  intptr_t amt_read, amt_wrote;
  intptr_t istart, ifinish, ostart, ofinish;
  Scheme_Object *a[3], *status_sym;
  Scheme_Converter *c;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_string_converter_type))
    scheme_wrong_contract(who, "bytes-converter?", 0, argc, argv);

  if (opos > 1) {
    if (!SCHEME_BYTE_STRINGP(argv[1]))
      scheme_wrong_contract(who, "bytes?", 1, argc, argv);
    scheme_get_substring_indices(who, argv[1], argc, argv, 2, 3, &istart, &ifinish);
  } else {
    istart = 0;
    ifinish = 4; /* This is really a guess about how much space we need for a shift terminator */
  }

  if (argc > opos) {
    if (SCHEME_TRUEP(argv[opos])) {
      if (!SCHEME_MUTABLE_BYTE_STRINGP(argv[opos]))
	scheme_wrong_contract(who, "(and/c bytes? (not/c immutable?))", opos, argc, argv);
      r = SCHEME_BYTE_STR_VAL(argv[opos]);
      scheme_get_substring_indices(who, argv[opos], argc, argv, opos + 1, opos + 2, &ostart, &ofinish);
    } else {
      int ip;
      r = NULL;
      for (ip = opos + 1; ip <= opos + 2; ip++) {
	if (argc > ip) {
	  int ok = 0;
	  if (SCHEME_INTP(argv[ip]))
	    ok = SCHEME_INT_VAL(argv[ip]) >= 0;
	  else if (SCHEME_BIGNUMP(argv[ip]))
	    ok = SCHEME_BIGPOS(argv[ip]);
	  else if ((ip == opos + 2) && SCHEME_FALSEP(argv[ip]))
	    ok = 1;
	  if (!ok)
	    scheme_wrong_contract(who,
                                  ((ip == opos + 2)
                                   ? "(or/c exact-nonnegative-integer? #f)"
                                   : "exact-nonnegative-integer?"),
                                  ip, argc, argv);
	}
      }
      if ((argc > opos + 2) && SCHEME_TRUEP(argv[opos + 2])) {
	Scheme_Object *delta;
	if (scheme_bin_lt(argv[opos + 2], argv[opos + 1])) {
	  scheme_contract_error(who,
                                "ending index is less than the starting index",
                                "staring index", 1, argv[opos + 1],
                                "ending index", 1, argv[opos + 2],
                                NULL);
	}
	delta = scheme_bin_minus(argv[opos + 2], argv[opos + 1]);
	if (SCHEME_BIGNUMP(delta))
	  ofinish = -1;
	else
	  ofinish = SCHEME_INT_VAL(delta);
	ostart = 0;
      } else {
	ostart = 0;
	ofinish = -1;
      }
    }
  } else {
    r = NULL;
    ostart = 0;
    ofinish = -1;
  }

  c = (Scheme_Converter *)argv[0];
  if (c->closed)
    scheme_contract_error(who, "converter is closed", 
                          "converter", 1, argv[0],
                          NULL);

  instr = ((opos > 1) ? SCHEME_BYTE_STR_VAL(argv[1]) : NULL);

  if (c->kind == mzUTF16_TO_UTF8_KIND) {
    if (istart & 0x1) {
      /* Copy to word-align */
      char *c2;
      c2 = (char *)scheme_malloc_atomic(ifinish - istart);
      memcpy(c2, instr XFORM_OK_PLUS istart, ifinish - istart);
      ifinish = ifinish - istart;
      istart = 0;
      instr = c2;
    }

    status = utf8_encode_x((const unsigned int *)instr, istart >> 1, ifinish >> 1,
			   (unsigned char *)r, ostart, ofinish,
			   &amt_read, &amt_wrote, 1);
    
    amt_read -= (istart >> 1);

    if (amt_read) {
      if (!r) {
	/* Need to allocate, then do it again: */
	r = (char *)scheme_malloc_atomic(amt_wrote + 1);
	utf8_encode_x((const unsigned int *)instr, istart >> 1, ifinish >> 1,
		      (unsigned char *)r, ostart, ofinish,
		      NULL, NULL, 1);
	r[amt_wrote] = 0;
      }
      amt_read <<= 1;
    }

    /* We might get a -1 result because the input has an odd number of
       bytes, and 2nd+next-to-last bytes form an unpaired
       surrogate. In that case, the transformer normally needs one
       more byte: Windows is little-endian, so we need the byte to
       tell whether the surrogate is paired, and for all other
       platforms (where we assume that surrogates are paired), we need
       the byte to generate output. Technically, on a big-endian
       non-Windows machine, we could generate the first byte of UTF-8
       output and keep the byte as state, but we don't. */

    if (status != -1) {
      if (amt_read < ((ifinish - istart) & ~0x1)) {
	/* Must have run out of output space */
	status = 1;
      } else {
	/* Read all of input --- but it wasn't really all if there
	   was an odd number of bytes. */
	if ((ifinish - istart) & 0x1)
	  status = -1;
	else
	  status = 0;
      }
    }
  } else if (c->kind != mzICONV_KIND) {
    /* UTF-8 -> UTF-{8,16} "identity" converter, but maybe permissive */
    if (instr) {
      intptr_t _ostart, _ofinish;
      int utf16;

      if (c->kind == mzUTF8_TO_UTF16_KIND) {
	_ostart = ostart;
	_ofinish = ofinish;
	if (_ostart & 0x1)
	  _ostart++;
	_ostart >>= 1;
	if (_ofinish > 0)
	  _ofinish >>= 1;
	utf16 = 1;
      } else {
	_ostart = ostart;
	_ofinish = ofinish;
	utf16 = 0;
      }

      status = utf8_decode_x((unsigned char *)instr, istart, ifinish,
			     (unsigned int *)r, _ostart, _ofinish,
			     &amt_read, &amt_wrote,
			     1, utf16, NULL, 1, c->permissive);
      
      if (utf16) {
	_ostart <<= 1;
	amt_wrote <<= 1;
	if ((ostart & 0x1) && (amt_wrote > _ostart)) {
	  /* Shift down one byte: */
	  memmove(r XFORM_OK_PLUS ostart, r XFORM_OK_PLUS _ostart, amt_wrote - _ostart);
	}
      }

      amt_read -= istart;
      amt_wrote -= _ostart;
      if (status == -3) {
	/* r is not NULL; ran out of room */
	status = 1;
      } else {
	if (amt_wrote) {
	  if (!r) {
	    /* Need to allocate, then do it again: */
	    r = (char *)scheme_malloc_atomic(amt_wrote + 1);
	    utf8_decode_x((unsigned char *)instr, istart, ifinish,
			  (unsigned int *)r, ostart, _ofinish,
			  NULL, NULL,
			  1, utf16, NULL, 1, c->permissive);
	    r[amt_wrote] = 0;
	  }
	} else if (!r)
	  r = "";
	if (status > 0)
	  status = 0;
      }
    } else {
      r = "";
      status = 0;
      amt_read = 0;
      amt_wrote = 0;
    }
  } else {
    r = do_convert(c->cd, NULL, NULL, 0,
		   instr, istart, ifinish-istart,
		   r, ostart, ofinish-ostart,
		   !r, /* grow? */
		   0,
		   (r ? 0 : 1), /* terminator */
		   &amt_read, &amt_wrote,
		   &status);
  }

  if (status == 0) {
    /* Converted all input without error */
    status_sym = complete_symbol;
  } else if (status == 1) {
    /* Filled output, more input ready */
    status_sym = continues_symbol;
  } else if (status == -1) {
    /* Input ends in the middle of an encoding */
    status_sym = aborts_symbol;
  } else {
    /* Assert: status == -2 */
    /* Input has error (that won't be fixed by
       adding more characters */
    status_sym = error_symbol;
  }

  if (argc <= opos) {
    a[0] = scheme_make_sized_byte_string(r, amt_wrote, 0);
  } else {
    a[0] = scheme_make_integer(amt_wrote);
  }
  if (opos > 1) {
    a[1] = scheme_make_integer(amt_read);
    a[2] = status_sym;
    return scheme_values(3, a);
  } else {
    a[1] = status_sym;
    return scheme_values(2, a);
  }
}

static Scheme_Object *byte_string_convert(int argc, Scheme_Object *argv[])
{
  return convert_one("bytes-convert", 4, argc, argv);
}

static Scheme_Object *byte_string_convert_end(int argc, Scheme_Object *argv[])
{
  return convert_one("bytes-convert-end", 1, argc, argv);
}

void scheme_close_converter(Scheme_Object *conv)
{
  close_converter(conv, NULL);
}

static Scheme_Object *byte_string_close_converter(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_string_converter_type))
    scheme_wrong_contract("bytes-close-converter", "bytes-converter?", 0, argc, argv);

  scheme_close_converter(argv[0]);

  return scheme_void;
}

static Scheme_Object *
byte_converter_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_string_converter_type)
	  ? scheme_true
	  : scheme_false);
}

/**********************************************************************/
/*                         utf8 converter                             */
/**********************************************************************/

static intptr_t utf8_decode_x(const unsigned char *s, intptr_t start, intptr_t end,
                              unsigned int *us, intptr_t dstart, intptr_t dend,
                              intptr_t *ipos, intptr_t *jpos,
                              char compact, char utf16, int *_state,
                              int might_continue, int permissive)
     /* Results:
	non-negative => translation complete, = number of produced chars
	-1 => input ended in middle of encoding (only if might_continue)
	-2 => encoding error (only if permissive is 0)
	-3 => not enough output room

	ipos & jpos are filled with ending positions (between [d]start
	and [d]end) before return, unless they are NULL.

	compact => UTF-8 to UTF-8 or UTF-16 --- the latter if utf16
	for Windows for utf16, decode extended UTF-8 that allows surrogates

	_state provides initial state and is filled with ending state;
	when it's not NULL, the us must be NULL

	might_continue => allows -1 result without consuming characters

	permissive is non-zero => use permissive as value for bad byte
	sequences. When generating UTF-8, this must be an ASCII character
        or U+FFFD. */

{
  intptr_t i, j, oki;
  int failmode = -3, state;
  int init_doki;
  int nextbits, v;
  unsigned int sc;
# ifdef WINDOWS_UNICODE_SUPPORT
  int pending_surrogate = 0;
# endif

  if (_state) {
    state = (*_state) & 0x7;
    init_doki = (((*_state) >> 3) & 0x7);
    nextbits = ((((*_state) >> 6) & 0xF) << 2);
    /* Need v to detect 0xD800 through 0xDFFF
       Note that we have 22 bits to work with, which is
       is enough to detect > 0x10FFFF */
    v = ((*_state) >> 10);
  } else {
    state = 0;
    init_doki = 0;
    nextbits = 0;
    v = 0;
  }

  /* In non-permissive mode, a negative result means ill-formed input.
     Permissive mode accepts anything and tries to convert it.  In
     that case, the strategy for illegal sequences is to convert
     anything bad to the given "permissive" value. */

  if (end < 0)
    end = strlen((char *)s);
  if (dend < 0)
    dend = 0x7FFFFFFF;

# define ENCFAIL i = oki; failmode = -2; break

  oki = start;
  j = dstart;
  i = start;
  if (j < dend) {
    while (i < end) {
      sc = s[i];
      if (sc < 0x80) {
	if (state) {
	  /* In a sequence, but didn't continue */
	  state = 0;
	  nextbits = 0;
	  if (permissive) {
	    v = permissive;
	    i = oki;
	    j += init_doki;
	  } else {
	    ENCFAIL;
	  }
	} else {
	  v = sc;
	}
      } else if ((sc & 0xC0) == 0x80) {
	/* Continues a sequence ... */
	if (state) {
	  /* ... and we're in one ... */
	  if (!nextbits || (sc & nextbits)) {
	    /* and we have required bits. */
	    v = (v << 6) + (sc & 0x3F);
	    nextbits = 0;
	    --state;
	    if (state) {
	      i++;
	      continue;
	    }
	    /* We finished. One last check: */
	    if ((((v >= 0xD800) && (v <= 0xDFFF))
		 || (v > 0x10FFFF))
# ifdef WINDOWS_UNICODE_SUPPORT
		&& (!utf16
		    /* If UTF-16 for Windows, just apply upper-limit check */
		    || (v > 0x10FFFF))
# endif
		) {
	      /* UTF-16 surrogates or other illegal code units */
	      if (permissive) {
		v = permissive;
		j += init_doki;
		i = oki;
	      } else {
		ENCFAIL;
	      }
	    }
	  } else {
	    /* ... but we're missing required bits. */
	    state = 0;
	    nextbits = 0;
	    if (permissive) {
	      v = permissive;
	      j += init_doki;
	      i = oki;
	    } else {
	      ENCFAIL;
	    }
	  }
	} else {
	  /* ... but we're not in one */
	  if (permissive) {
	    v = permissive;
	  } else {
	    ENCFAIL;
	  }
	}
      } else if (state) {
	/* bad: already in a sequence */
	state = 0;
	if (permissive) {
	  v = permissive;
	  i = oki;
	  j += init_doki;
	} else {
	  ENCFAIL;
	}
      } else {
	if ((sc & 0xE0) == 0xC0) {
	  if (sc & 0x1E) {
	    state = 1;
	    v = (sc & 0x1F);
	    i++;
	    continue;
	  }
	  /* else too small */
	} else if ((sc & 0xF0) == 0xE0) {
	  state = 2;
	  v = (sc & 0xF);
	  if (!v)
	    nextbits = 0x20;
	  i++;
	  continue;
	} else if ((sc & 0xF8) == 0xF0) {
	  v = (sc & 0x7);
	  if (v <= 4) {
	    state = 3;
	    if (!v)
	      nextbits = 0x30;
	    i++;
	    continue;
	  } 
	  /* Else will be larger than 0x10FFFF, so fail */
	}
	/* Too small, or 0xFF or 0xFe, or start of a 5- or 6-byte sequence */
	if (permissive) {
	  v = permissive;
	} else {
	  ENCFAIL;
	}
      }

      /* If we get here, we're supposed to output v */

      if (compact) {
	if (utf16) {
	  if (v > 0xFFFF) {
# ifdef WINDOWS_UNICODE_SUPPORT
	    if (pending_surrogate) {
	      if (us)
		((unsigned short *)us)[j] = pending_surrogate;
	      j++; /* Accept previously written unpaired surrogate */
	      pending_surrogate = 0;
	    }
# endif
	    if (j + 1 >= dend)
	      break;
	    if (us) {
	      v -= 0x10000;
	      ((unsigned short *)us)[j] = 0xD800 | ((v >> 10) & 0x3FF);
	      ((unsigned short *)us)[j+1] = 0xDC00 | (v & 0x3FF);
	    }
	    j++;
	  } else {
# ifdef WINDOWS_UNICODE_SUPPORT
	    /* We allow a surrogate by itself, but don't allow
	       a 0xDC00 after a 0xD800, otherwise multiple encodings can
	       map to the same thing. */
	    if ((v >= 0xD800) && (v <= 0xDFFF)) {
	      if (pending_surrogate && ((v & 0xDC00) == 0xDC00)) {
		/* This looks like a surrogate pair, so disallow it. */
		if (permissive) {
		  /* We need to fill in 6 permissive substitutions,
		     one for each input byte. If we can't put all 6,
		     then don't use any input. */
		  if (j + 5 >= dend) {
		    break;
		  } else {
		    int p;
		    if (us) {
		      for (p = 0; p < 5; p++) {
			if (j + p >= dend)
			  break;
			((unsigned short *)us)[j+p] = permissive;
		      }
		    }
		    j += 5;
		    v = permissive;
		  }
		} else {
		  ENCFAIL;
		}
		pending_surrogate = 0;
	      } else {
		if (pending_surrogate) {
		  if (us)
		    ((unsigned short *)us)[j] = pending_surrogate;
		  j++; /* Accept previousy written unpaired surrogate */
		  pending_surrogate = 0;
		  if (j >= dend)
		    break;
		}
		if ((v & 0xDC00) == 0xD800)
		  pending_surrogate = v;
		else
		  pending_surrogate = 0;
	      }
	    } else {
	      if (pending_surrogate) {
		if (us)
		  ((unsigned short *)us)[j] = pending_surrogate;
		j++; /* Accept previousy written unpaired surrogate */
		pending_surrogate = 0;
		if (j >= dend)
		  break;
	      }
	    }

	    if (pending_surrogate)
	      --j; /* don't accept unpaired surrogate, yet */
	    else if (us)
	      ((unsigned short *)us)[j] = v;
# else
	    if (us)
	      ((unsigned short *)us)[j] = v;
# endif
	  }
	} else {
	  intptr_t delta;
	  delta = (i - oki);
	  if (delta) {
	    if (j + delta + 1 < dend) {
	      if (us)
		memcpy(((char *)us) + j, s + oki, delta + 1);
	      j += delta;
	    } else
	      break;
	  } else if (v == 0xFFFD) {
            if (j + 3 < dend) {
              if (us) {
                ((unsigned char *)us)[j] = 0xEF;
                ((unsigned char *)us)[j+1] = 0xBF;
                ((unsigned char *)us)[j+2] = 0xBD;
              }
              j += 2;
            } else
              break;
          } else if (us) {
            ((unsigned char *)us)[j] = v;
          }
	}
      } else if (us) {
	us[j] = v;
      }
      j++;
      i++;
      oki = i;
      init_doki = 0;
      if (j >= dend)
	break;
    }
  }

  if (_state) {
    if (!state)
      *_state = 0;
    else
      *_state = (state 
		 | (((end - oki) + init_doki) << 3)
		 | ((nextbits >> 2) << 6)
		 | (v << 10));
  } else if (state) {
    if (might_continue || !permissive) {
      failmode = -1;
      i = end - 1; /* to ensure that failmode is returned */
    } else if (permissive) {
      for (i = oki; i < end; i++) {
	if (j < dend) {
	  if (us) {
	    if (compact) {
	      if (utf16)
		((unsigned short *)us)[j] = permissive;
	      else
		((unsigned char *)us)[j] = permissive;
	    } else
	      us[j] = permissive;
	  }
	  j++;
	} else
	  break;
      }
      oki = i;
    }
  }

# ifdef WINDOWS_UNICODE_SUPPORT
  if (pending_surrogate)
    oki -= 3;
#endif

  if (ipos)
    *ipos = oki;
  if (jpos)
    *jpos = j;

  if (i < end)
    return failmode;

# ifdef WINDOWS_UNICODE_SUPPORT
  if (pending_surrogate) {
    /* input must have ended right after surrogate */
    return -1;
  }
#endif

  return j - dstart;
}

intptr_t scheme_utf8_decode(const unsigned char *s, intptr_t start, intptr_t end,
                            unsigned int *us, intptr_t dstart, intptr_t dend,
                            intptr_t *ipos, char utf16, int permissive)
{
  return utf8_decode_x(s, start, end, us, dstart, dend,
		       ipos, NULL, utf16, utf16, NULL, 0, permissive);
}

intptr_t scheme_utf8_decode_as_prefix(const unsigned char *s, intptr_t start, intptr_t end,
                                      unsigned int *us, intptr_t dstart, intptr_t dend,
                                      intptr_t *ipos, char utf16, int permissive)
     /* Always returns number of read characters, not error codes. */
{
  intptr_t opos;
  utf8_decode_x(s, start, end, us, dstart, dend,
		ipos, &opos, utf16, utf16, NULL, 1, permissive);
  return opos - dstart;
}

intptr_t scheme_utf8_decode_all(const unsigned char *s, intptr_t len, unsigned int *us, int permissive)
{
  return utf8_decode_x(s, 0, len, us, 0, -1, NULL, NULL, 0, 0, NULL, 0, permissive);
}

intptr_t scheme_utf8_decode_prefix(const unsigned char *s, intptr_t len, unsigned int *us, int permissive)
     /* us != NULL */
{
  {
    /* Try fast path (all ASCII) */
    intptr_t i;
    for (i = 0; i < len; i++) {
      if (s[i] < 128)
	us[i] = s[i];
      else
	break;
    }
    if (i == len)
      return len;
  }

  return utf8_decode_x(s, 0, len, us, 0, -1, NULL, NULL, 0, 0, NULL, 1, permissive);
}

mzchar *scheme_utf8_decode_to_buffer_len(const unsigned char *s, intptr_t len,
					 mzchar *buf, intptr_t blen, intptr_t *_ulen)
{
  intptr_t ulen;

  ulen = utf8_decode_x(s, 0, len, NULL, 0, -1,
		       NULL, NULL, 0, 0,
		       NULL, 0, 0);
  if (ulen < 0)
    return NULL;
  if (ulen + 1 > blen) {
    buf = (mzchar *)scheme_malloc_atomic((ulen + 1) * sizeof(mzchar));
  }
  utf8_decode_x(s, 0, len, buf, 0, -1,
		NULL, NULL, 0, 0,
		NULL, 0, 0);
  buf[ulen] = 0;
  *_ulen = ulen;
  return buf;
}

mzchar *scheme_utf8_decode_to_buffer(const unsigned char *s, intptr_t len,
				     mzchar *buf, intptr_t blen)
{
  intptr_t ulen;
  return scheme_utf8_decode_to_buffer_len(s, len, buf, blen, &ulen);
}

intptr_t scheme_utf8_decode_count(const unsigned char *s, intptr_t start, intptr_t end,
			     int *_state, int might_continue, int permissive)
{
  intptr_t pos = 0;

  if (!_state || !*_state) {
    /* Try fast path (all ASCII): */
    intptr_t i;
    for (i = start; i < end; i++) {
      if (s[i] > 127)
	break;
    }
    if (i == end)
      return end - start;
  }

  utf8_decode_x(s, start, end,
		NULL, 0, -1,
		NULL, &pos,
		0, 0, _state,
		might_continue, permissive);

  return pos;
}

static intptr_t utf8_encode_x(const unsigned int *us, intptr_t start, intptr_t end,
			 unsigned char *s, intptr_t dstart, intptr_t dend,
			 intptr_t *_ipos, intptr_t *_opos, char utf16)
  /* Results:
        -1 => input ended in the middle of an encoding - only when utf16 and _opos
	non-negative => reports number of bytes/code-units produced */
{
  intptr_t i, j, done = start;

  if (dend < 0)
    dend = 0x7FFFFFFF;

  if (!s) {
    unsigned int wc;
    j = 0;
    for (i = start; i < end; i++) {
      if (utf16) {
	wc = ((unsigned short *)us)[i];
	if ((wc & 0xF800) == 0xD800) {
	  /* Unparse surrogates. We assume that the surrogates are
	     well formed, unless this is Windows or if we're at the
             end and _opos is 0. */
# ifdef WINDOWS_UNICODE_SUPPORT
#  define UNPAIRED_MASK 0xFC00
# else
#  define UNPAIRED_MASK 0xF800
# endif
	  if (((i + 1) == end) && ((wc & UNPAIRED_MASK) == 0xD800) && _opos) {
	    /* Ended in the middle of a surrogate pair */
	    *_opos = j;
	    if (_ipos)
	      *_ipos = i;
	    return -1;
	  }
# ifdef WINDOWS_UNICODE_SUPPORT
	  if ((wc & 0xFC00) != 0xD800) {
	    /* Count as one */
	  } else if ((i + 1 >= end)
		     || (((((unsigned short *)us)[i+1]) & 0xFC00) != 0xDC00)) {
	  } else 
# endif
	    {
	      i++;
	      wc = ((wc & 0x3FF) << 10) + ((((unsigned short *)us)[i]) & 0x3FF);
	      wc += 0x10000;
	    }
	}
      } else {
	wc = us[i];
      }
      if (wc < 0x80) {
	j += 1;
      } else if (wc < 0x800) {
	j += 2;
      } else if (wc < 0x10000) {
	j += 3;
      } else if (wc < 0x200000) {
	j += 4;
      } else if (wc < 0x4000000) {
	j += 5;
      } else {
	j += 6;
      }
    }
    if (_ipos)
      *_ipos = i;
    if (_opos)
      *_opos = j + dstart;
    return j;
  } else {
    unsigned int wc;
    j = dstart;
    for (i = start; i < end; i++) {
      if (utf16) {
	wc = ((unsigned short *)us)[i];
	if ((wc & 0xF800) == 0xD800) {
	  /* Unparse surrogates. We assume that the surrogates are
	     well formed on non-Windows platforms, but when _opos,
	     we detect ending in the middle of an surrogate pair. */
	  if (((i + 1) == end) && ((wc & UNPAIRED_MASK) == 0xD800) && _opos) {
	    /* Ended in the middle of a surrogate pair */
	    *_opos = j;
	    if (_ipos)
	      *_ipos = i;
	    return -1;
	  }
# ifdef WINDOWS_UNICODE_SUPPORT
	  if ((wc & 0xFC00) != 0xD800) {
	    /* Let the misplaced surrogate through */
	  } else if ((i + 1 >= end)
		     || (((((unsigned short *)us)[i+1]) & 0xFC00) != 0xDC00)) {
	    /* Let the misplaced surrogate through */
	  } else
# endif
	    {
	      i++;
	      wc = ((wc & 0x3FF) << 10) + ((((unsigned short *)us)[i]) & 0x3FF);
	      wc += 0x10000;
	    }
	}
      } else {
	wc = us[i];
      }

      if (wc < 0x80) {
	if (j + 1 > dend)
	  break;
	s[j++] = wc;
      } else if (wc < 0x800) {
	if (j + 2 > dend)
	  break;
	s[j++] = 0xC0 | ((wc & 0x7C0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      } else if (wc < 0x10000) {
	if (j + 3 > dend)
	  break;
	s[j++] = 0xE0 | ((wc & 0xF000) >> 12);
	s[j++] = 0x80 | ((wc & 0x0FC0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      } else if (wc < 0x200000) {
	if (j + 4 > dend)
	  break;
	s[j++] = 0xF0 | ((wc & 0x1C0000) >> 18);
	s[j++] = 0x80 | ((wc & 0x03F000) >> 12);
	s[j++] = 0x80 | ((wc & 0x000FC0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      } else if (wc < 0x4000000) {
	if (j + 5 > dend)
	  break;
	s[j++] = 0xF8 | ((wc & 0x3000000) >> 24);
	s[j++] = 0x80 | ((wc & 0x0FC0000) >> 18);
	s[j++] = 0x80 | ((wc & 0x003F000) >> 12);
	s[j++] = 0x80 | ((wc & 0x0000FC0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      } else {
	if (j + 6 > dend)
	  break;
	s[j++] = 0xFC | ((wc & 0x40000000) >> 30);
	s[j++] = 0x80 | ((wc & 0x3F000000) >> 24);
	s[j++] = 0x80 | ((wc & 0x00FC0000) >> 18);
	s[j++] = 0x80 | ((wc & 0x0003F000) >> 12);
	s[j++] = 0x80 | ((wc & 0x00000FC0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      }
      done = i;
    }
    if (_ipos)
      *_ipos = done;
    if (_opos)
      *_opos = j;
    return j - dstart;
  }
}

intptr_t scheme_utf8_encode(const unsigned int *us, intptr_t start, intptr_t end,
		       unsigned char *s, intptr_t dstart,
		       char utf16)
{
  return utf8_encode_x(us, start, end,
		       s, dstart, -1,
		       NULL, NULL, utf16);
}

intptr_t scheme_utf8_encode_all(const unsigned int *us, intptr_t len, unsigned char *s)
{
  return utf8_encode_x(us, 0, len, s, 0, -1, NULL, NULL, 0 /* utf16 */);
}

char *scheme_utf8_encode_to_buffer_len(const mzchar *s, intptr_t len,
				       char *buf, intptr_t blen,
				       intptr_t *_slen)
{
  intptr_t slen;

  /* ASCII with len < blen is a common case: */
  if (len < blen) {
    for (slen = 0; slen < len; slen++) {
      if (s[slen] > 127)
        break;
      else
        buf[slen] = s[slen];
    }
    if (slen == len) {
      buf[slen] = 0;
      *_slen = slen;
      return buf;
    }
  }

  slen = utf8_encode_x(s, 0, len, NULL, 0, -1, NULL, NULL, 0);
  if (slen + 1 > blen) {
    buf = (char *)scheme_malloc_atomic(slen + 1);
  }
  utf8_encode_x(s, 0, len, (unsigned char *)buf, 0, -1, NULL, NULL, 0);
  buf[slen] = 0;
  *_slen = slen;
  return buf;
}

char *scheme_utf8_encode_to_buffer(const mzchar *s, intptr_t len,
				   char *buf, intptr_t blen)
{
  intptr_t slen;
  return scheme_utf8_encode_to_buffer_len(s, len, buf, blen, &slen);
}

unsigned short *scheme_ucs4_to_utf16(const mzchar *text, intptr_t start, intptr_t end,
				     unsigned short *buf, intptr_t bufsize,
				     intptr_t *ulen, intptr_t term_size)
{
  mzchar v;
  intptr_t extra, i, j;
  unsigned short *utf16;

  /* Count characters that fall outside UCS-2: */
  for (i = start, extra = 0; i < end; i++) {
    if (text[i] > 0xFFFF)
      extra++;
  }

  if ((end - start) + extra + term_size < bufsize)
    utf16 = buf;
  else
    utf16 = (unsigned short *)scheme_malloc_atomic(sizeof(unsigned short) * ((end - start) + extra + term_size));

  for (i = start, j = 0; i < end; i++) {
    v = text[i];
    if (v > 0xFFFF) {
      utf16[j++] = 0xD800 | ((v >> 10) & 0x3FF);
      utf16[j++] = 0xDC00 | (v & 0x3FF);
    } else
      utf16[j++] = v;
  }

  *ulen = j;

  return utf16;
}

mzchar *scheme_utf16_to_ucs4(const unsigned short *text, intptr_t start, intptr_t end,
			     mzchar *buf, intptr_t bufsize,
			     intptr_t *ulen, intptr_t term_size)
{
  int wc;
  intptr_t i, j;

  for (i = start, j = 0; i < end; i++) {
    wc = text[i];
    if ((wc & 0xF800) == 0xD800) {
      i++;
    }
    j++;
  }

  if (j + term_size >= bufsize)
    buf = (mzchar *)scheme_malloc_atomic((j + term_size) * sizeof(mzchar));

  for (i = start, j = 0; i < end; i++) {
    wc = text[i];
    if ((wc & 0xF800) == 0xD800) {
      i++;
      wc = ((wc & 0x3FF) << 10) + ((((unsigned short *)text)[i]) & 0x3FF);
      wc += 0x10000;
    }
    buf[j++] = wc;
  }

  *ulen = j;

  return buf;
}

/**********************************************************************/
/*                     machine type details                           */
/**********************************************************************/

/**************************** MacOS ***********************************/

#if defined(MACINTOSH_EVENTS) && !defined(OS_X)
# include <Gestalt.h>
extern intptr_t scheme_this_ip(void);
static void machine_details(char *s)
{
   OSErr err;
   intptr_t lng;
   char sysvers[30];
   char machine_name[256];

   err = Gestalt(gestaltSystemVersion, &lng);
   if (err != noErr) {
     strcpy(sysvers, "<unknown system>");
   } else {
     int i;
     sprintf(sysvers, "%X.%X",
	     (lng >> 8) & 0xff,
	     lng & 0xff);
     /* remove trailing zeros, put dot before something else */
     i = strlen(sysvers);
     if (i > 1) {
       if (sysvers[i-1] != '.') {
	 if (sysvers[i-1] == '0') {
	   sysvers[i-1] = 0;
	   i--;
	 } else {
	   sysvers[i] = sysvers[i-1];
	   sysvers[i-1] = '.';
	   i++;
	   sysvers[i] = 0;
	 }
       }
     }
   }

   err = Gestalt(gestaltMachineType, &lng);
   if (err != noErr) {
     strcpy(machine_name, "<unknown machine>");
   } else {
   	 Str255 machine_name_pascal;

   	 GetIndString(machine_name_pascal, kMachineNameStrID, lng);
	 CopyPascalStringToC(machine_name_pascal, machine_name);
   }

   lng = scheme_this_ip();

   sprintf(s, "%s %s %d.%d.%d.%d", sysvers, machine_name,
	   ((unsigned char *)&lng)[0],
	   ((unsigned char *)&lng)[1],
	   ((unsigned char *)&lng)[2],
	   ((unsigned char *)&lng)[3]);
}
#endif

/*************************** Windows **********************************/

#ifdef DOS_FILE_SYSTEM
# include <windows.h>
void machine_details(char *buff)
{
  OSVERSIONINFO info;
  BOOL hasInfo;
  char *p;

  info.dwOSVersionInfoSize = sizeof(info);

  GetVersionEx(&info);

  hasInfo = FALSE;

  p = info.szCSDVersion;

  while (p < info.szCSDVersion + sizeof(info.szCSDVersion) &&
	 *p) {
    if (*p != ' ') {
      hasInfo = TRUE;
      break;
    }
    p = p XFORM_OK_PLUS 1;
  }

  sprintf(buff,"Windows %s %ld.%ld (Build %ld)%s%s",
	  (info.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS) ?
	  "9x" :
	  (info.dwPlatformId == VER_PLATFORM_WIN32_NT) ?
	  "NT" : "Unknown platform",
	  info.dwMajorVersion,info.dwMinorVersion,
	  (info.dwPlatformId == VER_PLATFORM_WIN32_NT) ?
	  info.dwBuildNumber :
	  info.dwBuildNumber & 0xFFFF,
	  hasInfo ? " " : "",hasInfo ? info.szCSDVersion : "");
}
#endif

/***************************** OSKit **********************************/

#ifdef USE_OSKIT_CONSOLE
void machine_details(char *buff)
{
  strcpy(buff, "OSKit");
}
#endif

/***************************** Unix ***********************************/

#if (!defined(MACINTOSH_EVENTS) || defined(OS_X)) && !defined(DOS_FILE_SYSTEM) && !defined(USE_OSKIT_CONSOLE)
READ_ONLY static char *uname_locations[] = { "/bin/uname",
				   "/usr/bin/uname",
				   /* The above should cover everything, but
				      just in case... */
				   "/sbin/uname",
				   "/usr/sbin/uname",
				   "/usr/local/bin/uname",
				   "/usr/local/uname",
				   NULL };

static int try_subproc(Scheme_Object *subprocess_proc, char *prog)
{
  Scheme_Object *a[5];
  mz_jmp_buf * volatile savebuf, newbuf;

  savebuf = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;

  if (!scheme_setjmp(newbuf)) {
    a[0] = scheme_false;
    a[1] = scheme_false;
    a[2] = scheme_false;
    a[3] = scheme_make_locale_string(prog);
    a[4] = scheme_make_locale_string("-a");
    _scheme_apply_multi(subprocess_proc, 5, a);
    scheme_current_thread->error_buf = savebuf;
    return 1;
  } else {
    scheme_clear_escape();
    scheme_current_thread->error_buf = savebuf;
    return 0;
  }
}

void machine_details(char *buff)
{
  Scheme_Object *subprocess_proc;
  int i;

  subprocess_proc = scheme_builtin_value("subprocess");

  for (i = 0; uname_locations[i]; i++) {
    if (scheme_file_exists(uname_locations[i])) {
      /* Try running it. */
      if (try_subproc(subprocess_proc, uname_locations[i])) {
	Scheme_Object *sout, *sin, *serr;
	intptr_t c;

	sout = scheme_current_thread->ku.multiple.array[1];
	sin = scheme_current_thread->ku.multiple.array[2];
	serr = scheme_current_thread->ku.multiple.array[3];

	scheme_close_output_port(sin);
	scheme_close_input_port(serr);

	/* Read result: */
	strcpy(buff, "<unknown machine>");
	c = scheme_get_bytes(sout, 1023, buff, 0);
	buff[c] = 0;

	scheme_close_input_port(sout);

	/* Remove trailing whitespace (especially newlines) */
	while (c && portable_isspace(((unsigned char *)buff)[c - 1])) {
	  buff[--c] = 0;
	}

	return;
      }
    }
  }

  strcpy(buff, "<unknown machine>");
}
#endif


/**********************************************************************/
/*                           Precise GC                               */
/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_string.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_string_converter_type, mark_string_convert);
}

END_XFORM_SKIP;

#endif
