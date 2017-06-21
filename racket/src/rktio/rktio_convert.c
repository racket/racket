#include "rktio.h"
#include "rktio_private.h"
#include <string.h>
#include <stdlib.h>

#ifdef OS_X
# define MACOS_UNICODE_SUPPORT
#endif

#include <locale.h>
#include <errno.h>
#if !defined(RKTIO_SYSTEM_WINDOWS) && !defined(RKTIO_NO_ICONV)
# include <iconv.h>
# include <langinfo.h>
#endif
#include <wchar.h>
#include <wctype.h>
#ifdef MACOS_UNICODE_SUPPORT
# include <CoreFoundation/CFString.h>
# include <CoreFoundation/CFLocale.h>
#endif

/*============================================================*/
/* Using iconv via a DLL                                      */
/*============================================================*/

#ifdef RKTIO_NO_ICONV

# define HAVE_CODESET 0
# define ICONV_errno 0
# define RKTIO_CHK_PROC(x) 0
# define iconv_ready 1

typedef intptr_t iconv_t;
static size_t iconv(iconv_t cd, char **in, size_t *in_left, char **out, size_t *out_left) { return (size_t)-1; }
static iconv_t iconv_open(const char *to, const char *from) { return -1; }
static void iconv_close(iconv_t cd) { }
static void init_iconv() { }

#elif defined(RKTIO_SYSTEM_WINDOWS)

static wchar_t *dlldir;

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

static int get_iconv_errno(void)
{
  int *a;
  a = iconv_errno();
  return *a;
}

# define HAVE_CODESET 1
# define CODESET 0
# define ICONV_errno get_iconv_errno()
# define RKTIO_CHK_PROC(x) x
static int iconv_ready = 0;

static void init_iconv()
{
  HMODULE m;
  wchar_t *p;
  
  p = rktio_get_dll_path(L"iconv.dll");
  if (p) {
    m = LoadLibraryW(p);
    free(p);
  } else
    m = NULL;
  
  if (!m) {
    p = rktio_get_dll_path(L"libiconv.dll");
    if (p) {
      m = LoadLibraryW(p);
      free(p);
    } else
      m = NULL;
  }
  
  if (!m) {
    p = rktio_get_dll_path(L"libiconv-2.dll");
    if (p) {
      m = LoadLibraryW(p);
      free(p);
    } else
      m = NULL;
  }
  
  if (!m)
    m = LoadLibraryW(L"iconv.dll");
  if (!m)
    m = LoadLibraryW(L"libiconv.dll");
  if (!m)
    m = LoadLibraryW(L"libiconv-2.dll");
  
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
      m = LoadLibraryW(L"msvcrt.dll");
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

  iconv_ready = 1;
}

rktio_char16_t *rktio_get_dll_path(rktio_char16_t *s)
{
  if (dlldir) {
    int len1, len2;
    wchar_t *p;
    len1 = wcslen(dlldir);
    len2 = wcslen(s);
    p = malloc((len1 + len2 + 2) * sizeof(wchar_t));
    memcpy(p, dlldir, len1 * sizeof(wchar_t));
    if (p[len1 - 1] != '\\') {
      p[len1++] = '\\';
    }
    memcpy(p + len1, s, (len2 + 1) * sizeof(wchar_t));
    return p;
  } else
    return NULL;
}

void rktio_set_dll_path(rktio_char16_t *p)
{
  dlldir = p;
}

#else

# ifdef RKTIO_HAVE_CODESET
#  define HAVE_CODESET 1
# else
#  define HAVE_CODESET 0
# endif

# include <errno.h>
# define ICONV_errno errno
# define iconv_ready 1
# define RKTIO_CHK_PROC(x) 1
static void init_iconv() { }

void rktio_set_dll_path(rktio_char16_t *p) { }
rktio_char16_t *rktio_get_dll_path(rktio_char16_t *s) { return NULL; }

#endif

/*============================================================*/
/* Properties                                                 */
/*============================================================*/

int rktio_convert_properties(rktio_t *rktio)
{
  int flags = 0;

  if (!iconv_ready) init_iconv();

  if (RKTIO_CHK_PROC(iconv_errno))
    flags = RKTIO_CONVERTER_SUPPORTED;
      
#if defined(MACOS_UNICODE_SUPPORT) || defined(RKTIO_SYSTEM_WINDOWS)
  flags |= (RKTIO_CONVERT_STRCOLL_UTF16 | RKTIO_CONVERT_RECASE_UTF16);
#endif

  return flags;
}

/*============================================================*/
/* Current locale                                             */
/*============================================================*/

void rktio_set_locale(rktio_t *rktio, char *name)
{
  /* We only need CTYPE and COLLATE; two calls seem to be much
     faster than one call with ALL */
  if (name) {
    if (!setlocale(LC_CTYPE, name))
      setlocale(LC_CTYPE, "C");
    if (!setlocale(LC_COLLATE, name))
      setlocale(LC_COLLATE, "C");
  } else {
    setlocale(LC_CTYPE, "C");
    setlocale(LC_COLLATE, "C");
  }
}

char *rktio_push_c_numeric_locale(rktio_t *rktio)
{
  char *prev;
  prev = setlocale(LC_NUMERIC, NULL);
  if (!prev || !strcmp(prev, "C"))
    return NULL;
  else
    return setlocale(LC_NUMERIC, "C");
}

void rktio_pop_c_numeric_locale(rktio_t *rktio, char *prev)
{
  if (prev)
    setlocale(LC_NUMERIC, prev);
}

/*============================================================*/
/* Current locale's encoding                                  */
/*============================================================*/

#ifdef RKTIO_SYSTEM_WINDOWS

static char *nl_langinfo_dup()
{
  int i;
  char *current_locale_name;

  current_locale_name = setlocale(LC_NUMERIC, NULL);
  if (!current_locale_name)
    current_locale_name = "";

  if ((current_locale_name[0] == 'C')
      && !current_locale_name[1])
    return MSC_IZE(strdup)("US-ASCII");

  for (i = 0; current_locale_name[i]; i++) {
    if (current_locale_name[i] == '.') {
      if (current_locale_name[i + 1]) {
	int len, j;
	char *enc;
	i++;
	len = scheme_char_strlen(current_locale_name) - i;
	enc = malloc(2 + len + 1);

        /* Check whether the encoding is numeric, in which case
           we add "CP" in front to make it an encoding name */
        for (j = i; current_locale_name[j]; j++) {
          if (current_locale_name[j] > 127)
            break;
          if (!isdigit(current_locale_name[j]))
            break;
        }
        if (!current_locale_name[j]) {
          j = 2;
          memcpy(enc, "CP", j);
        } else {
          j = 0;
        }

	while (current_locale_name[i]) {
	  if (current_locale_name[i] > 127) {
            free(enc);
	    return MSC_IZE(strdup)("UTF-8");
          }
	  enc[j++] = current_locale_name[i++];
	}
	enc[j] = 0;
	return enc;
      }
    }
  }

  return MSC_IZE(strdup)("UTF-8");
}

#else

static char *nl_langinfo_dup()
{
  char *s;
# if HAVE_CODESET
  s = nl_langinfo(CODESET);
# else
  /* nl_langinfo doesn't work, so just make up something */
  s = "UTF-8";
# endif

  return MSC_IZE(strdup)(s);
}

#endif

char *rktio_locale_encoding(rktio_t *rktio)
{
  return nl_langinfo_dup();
}

/*============================================================*/

char *rktio_system_language_country(rktio_t *rktio)
{
#ifdef MACOS_UNICODE_SUPPORT
  /* Mac OS */
  CFLocaleRef l;
  CFStringRef s;
  int len;
  char *r;

  l = CFLocaleCopyCurrent();
  s = CFLocaleGetIdentifier(l);

  len = CFStringGetLength(s);
  r = malloc(len * 6 + 1);
  CFStringGetCString(s, r, len * 6 + 1, kCFStringEncodingUTF8);

  CFRelease(l);

  r[5] = 0;

  return r;
#elif defined(RKTIO_SYSTEM_WINDOWS)
  /* Windows */
  LCID l;
  int llen, clen;
  wchar_t *lang, *country, *s;
  l = GetUserDefaultLCID();

  llen = GetLocaleInfoW(l, LOCALE_SENGLANGUAGE, NULL, 0);
  lang = malloc(llen * sizeof(wchar_t));
  GetLocaleInfo(l, LOCALE_SENGLANGUAGE, lang, llen);
  if (llen)
    llen -= 1; /* drop nul terminator */

  clen = GetLocaleInfoW(l, LOCALE_SENGCOUNTRY, NULL, 0);
  country = malloc(clen * sizeof(wchar_t));
  GetLocaleInfoW(l, LOCALE_SENGCOUNTRY, country, clen);
  if (clen)
    clen -= 1; /* drop nul terminator */

  s = malloc((clen + llen + 2) * sizeof(wchar_t));
  memcpy(s, lang, llen * sizeof(wchar_t));
  memcpy(s + 1 + llen, country, clen * sizeof(wchar_t));
  s[llen] = '_';
  s[clen + llen + 1] = 0;

  free(lang);
  free(country);
  
  return NARROW_PATH_copy_then_free(s);
#else
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
  
  return MSC_IZE(strdup)(s);
#endif
}

/*============================================================*/
/* Converters                                                 */
/*============================================================*/

struct rktio_converter_t {
  iconv_t cd;
};

rktio_converter_t *rktio_converter_open(rktio_t *rktio, const char *to_enc, const char *from_enc)
{
  iconv_t cd;
  rktio_converter_t *cvt;

  if (!iconv_ready) init_iconv();

  cd = iconv_open(to_enc, from_enc);
  if (cd == (iconv_t)-1) {
    errno = ICONV_errno;
    get_posix_error();
    return NULL;
  }

  cvt = malloc(sizeof(rktio_converter_t));
  cvt->cd = cd;
  return cvt;
}

void rktio_converter_close(rktio_t *rktio, rktio_converter_t *cvt)
{
  iconv_close(cvt->cd);
  free(cvt);
}

intptr_t rktio_convert(rktio_t *rktio,
                       rktio_converter_t *cvt,
                       char **in, intptr_t *in_left,
                       char **out, intptr_t *out_left)
{
  size_t il = *in_left, ol = *out_left, r;
  int icerr;

  r = iconv(cvt->cd, in, &il, out, &ol);

  *in_left = il;
  *out_left = ol;

  if (r == (size_t)-1) {
    icerr = ICONV_errno;

    if (icerr == E2BIG)
      set_racket_error(RKTIO_ERROR_CONVERT_NOT_ENOUGH_SPACE);
    else if (icerr == EILSEQ)
      set_racket_error(RKTIO_ERROR_CONVERT_BAD_SEQUENCE);
    else if (icerr == EINVAL)
      set_racket_error(RKTIO_ERROR_CONVERT_PREMATURE_END);
    else
      set_racket_error(RKTIO_ERROR_CONVERT_OTHER);
    return RKTIO_CONVERT_ERROR;
  }

  return (intptr_t)r;
}

/*============================================================*/
/* Case conversion                                            */
/*============================================================*/

char *rktio_locale_recase(rktio_t *rktio,
                          rktio_bool_t to_up,
                          char *in)
{
  char *out;

#ifdef NO_MBTOWC_FUNCTIONS
  /* No wide-char functions...
     The C library's toupper and tolower is supposed to be
     locale-sensitive. It can't be right for characters that are
     encoded in multiple bytes, but probably it will do the right
     thing in common cases. */
  intptr_t iilen = strlen(in);
  int i;

  /* First, copy "in" to "out" */
  out = malloc(iilen + 1);
  memcpy(out, in, iilen);
  out[iilen] = 0;

  /* Re-case chars in "out" */
  for (i = 0; i < iilen; i++) {
    char t;
    t = (to_up) ? toupper(out[i]) : tolower(out[i]);
    out[i] = t;
  }

  return out;
#else
  /* To change the case, convert the string to multibyte, re-case the
     multibyte, then convert back. */
# define RKTIO_WC_BUF_SIZE 32
  mbstate_t state;
  size_t wl, ml;
  wchar_t *wc, *ws, wcbuf[RKTIO_WC_BUF_SIZE], cwc;
  const char *s;
  unsigned int j;
  /* The "n" versions are apparently not too standard: */
# define mz_mbsnrtowcs(t, f, fl, tl, s) mbsrtowcs(t, f, tl, s)
# define mz_wcsnrtombs(t, f, fl, tl, s) wcsrtombs(t, f, tl, s)

  /* ----- to wide char ---- */

  /* Get length */
  memset(&state, 0, sizeof(mbstate_t));
  s = in;
  wl = mz_mbsnrtowcs(NULL, &s, iilen, 0, &state);
  s = NULL;

  /* Allocate space */
  if (wl < RKTIO_WC_BUF_SIZE) {
    wc = wcbuf;
  } else {
    wc = malloc(sizeof(wchar_t) * (wl + 1));
  }

  /* Convert */
  memset(&state, 0, sizeof(mbstate_t));
  s = in;
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
  out = malloc(ml + 1);

  /* Convert */
  memset(&state, 0, sizeof(mbstate_t));
  ws = wc;
  (void)mz_wcsnrtombs(out, (const wchar_t **)&ws, wl, ml + 1, &state);
  ws = NULL;

  out[ml] = 0;

  if (wc != wcbuf) free(wc);

  return out;
#endif
}

rktio_char16_t *rktio_recase_utf16(rktio_t *rktio, rktio_bool_t to_up, rktio_char16_t *s1, intptr_t l1, intptr_t *olen)
{
#ifdef MACOS_UNICODE_SUPPORT
  CFMutableStringRef mstr;
  CFStringRef str;
  CFRange rng;
  rktio_char16_t *result;
  intptr_t len;

  str = CFStringCreateWithBytes(NULL, (unsigned char *)s1, (l1 * sizeof(rktio_char16_t)), kCFStringEncodingUnicode, FALSE);
  mstr = CFStringCreateMutableCopy(NULL, 0, str);
  CFRelease(str);

  if (to_up)
    CFStringUppercase(mstr, NULL);
  else
    CFStringLowercase(mstr, NULL);

  len = CFStringGetLength(mstr);

  result = malloc((len + 1) * sizeof(rktio_char16_t));

  rng = CFRangeMake(0, len);
  CFStringGetCharacters(mstr, rng, (UniChar *)result);
  CFRelease(mstr);

  result[len] = 0;

  if (olen)
    *olen = len;

  return result;
#elif defined(RKTIO_SYSTEM_WINDOWS)
  rktio_char16_t *result;
  
  result = malloc((l1 + 1) * sizeof(rktio_char16_t));
  memcpy(result, s1, l1 * sizeof(rktio_char16_t));
  result[l1] = 0;
  
  if (to_up)
    CharUpperBuffW((wchar_t *)result, l1);
  else
    CharLowerBuffW((wchar_t *)result, l1);

  if (olen)
    *olen = l1;

  return result;
#else
  return NULL;
#endif
}

/*============================================================*/
/* Native string comparison                                   */
/*============================================================*/

int rktio_locale_strcoll(rktio_t *rktio, char *s1, char *s2)
{
  return strcoll(s1, s2);
}

int rktio_strcoll_utf16(rktio_t *rktio,
                        rktio_char16_t *s1, intptr_t l1,
                        rktio_char16_t *s2, intptr_t l2,
                        rktio_bool_t cvt_case)
{
#ifdef MACOS_UNICODE_SUPPORT
  CFStringRef str1, str2;
  CFComparisonResult r;

  str1 = CFStringCreateWithBytes(NULL, (unsigned char *)s1, (l1 * sizeof(rktio_char16_t)), 
				 kCFStringEncodingUnicode, FALSE);
  str2 = CFStringCreateWithBytes(NULL, (unsigned char *)s2, (l2 * sizeof(rktio_char16_t)), 
				 kCFStringEncodingUnicode, FALSE);

  r = CFStringCompare(str1, str2, (kCFCompareLocalized
				   | (cvt_case ? kCFCompareCaseInsensitive : 0)));

  CFRelease(str1);
  CFRelease(str2);

  return (int)r;
#elif defined(RKTIO_SYSTEM_WINDOWS)
  int r;
  r = CompareStringW(LOCALE_USER_DEFAULT,
		     ((cvt_case ? NORM_IGNORECASE : 0)
		      | NORM_IGNOREKANATYPE
		      | NORM_IGNOREWIDTH),
		     (wchar_t *)s1, l1, (wchar_t *)s2, l2);
  
  return r - 2;
#else
  return 0;
#endif
}


