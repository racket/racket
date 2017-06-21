#include "rktio.h"
#include "rktio_private.h"
#include <stdlib.h>
#include <string.h>

/* For converting byte strings to and from "wide" strings on Windows. */

#ifdef RKTIO_SYSTEM_UNIX
void rktio_init_wide(rktio_t *rktio) { }

rktio_char16_t *rktio_path_to_wide_path(rktio_t *rktio, const char *p)
{
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  return NULL;
}

char *rktio_wide_path_to_path(rktio_t *rktio, const rktio_char16_t *wp)
{
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  return NULL;
}

#endif

#ifdef RKTIO_SYSTEM_WINDOWS

#include <string.h>
#include <stdlib.h>

void rktio_init_wide(rktio_t *rktio)
{
  OSVERSIONINFO info;
  info.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx(&info);
  if (info.dwPlatformId == VER_PLATFORM_WIN32_NT)
    rktio->windows_nt_or_later = 1;
  else
    rktio->windows_nt_or_later = 0;
}

/* A UTF-8 to UTF-16 conversion, but accepts an extended variant of
   UTF-16 that accommodates unpaired surrogates, so that all 16-byte
   sequences are accessible. */
static intptr_t utf8ish_to_utf16ish(const unsigned char *s, intptr_t end, unsigned short *us)
{
  intptr_t i, j, oki;
  int state;
  int init_doki;
  int nextbits, v;
  unsigned int sc;
  int pending_surrogate = 0;

  state = 0;
  init_doki = 0;
  nextbits = 0;
  v = 0;

  oki = 0;
  j = 0;
  i = 0;
  while (i < end) {
    sc = s[i];
    if (sc < 0x80) {
      if (state) {
        /* In a sequence, but didn't continue */
        return -1;
      }
      v = sc;
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
          if (v > 0x10FFFF) {
            /* illegal code units */
            return -1;
          }
        } else {
          /* ... but we're missing required bits. */
          return -1;
        }
      } else {
        /* ... but we're not in one */
        return -1;
      }
    } else if (state) {
      /* bad: already in a sequence */
      return -1;
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
      return -1;
    }

    /* If we get here, we're supposed to output v */

    if (v > 0xFFFF) {
      if (pending_surrogate) {
        /* Accept previously written unpaired surrogate */
        if (us)
          us[j] = pending_surrogate;
        j++;
        pending_surrogate = 0;
      }

      if (us) {
        v -= 0x10000;
        us[j] = 0xD800 | ((v >> 10) & 0x3FF);
        us[j+1] = 0xDC00 | (v & 0x3FF);
      }
      j++;
    } else {
      /* We allow a surrogate by itself, but don't allow a UTF-8ish a
         0xDC00 after a UTF-8ish 0xD800, otherwise multiple encodings
         can map to the same thing. */
      if ((v >= 0xD800) && (v <= 0xDFFF)) {
        if (pending_surrogate && ((v & 0xDC00) == 0xDC00)) {
          /* This would look like a surrogate pair... */
          return -1;
        } else {
          if (pending_surrogate) {
            if (us)
              us[j] = pending_surrogate;
            j++; /* Accept previousy written unpaired surrogate */
            pending_surrogate = 0;
          }
          if ((v & 0xDC00) == 0xD800)
            pending_surrogate = v;
          else
            pending_surrogate = 0;
        }
      } else {
        if (pending_surrogate) {
          if (us)
            us[j] = pending_surrogate;
          j++; /* Accept previousy written unpaired surrogate */
          pending_surrogate = 0;
        }
      }
        
      if (pending_surrogate)
        --j; /* don't accept unpaired surrogate, yet */
      else if (us)
        us[j] = v;
    }
    j++;
    i++;
    oki = i;
    init_doki = 0;
  }

  if (pending_surrogate) {
    if (us)
      us[j] = pending_surrogate;
    j++;
  }

  if (state)
    return -1;

  return j;
}

/* A UTF-16 to UTF-8 conversion, but unpaired surrogates map to an
   extended variant of UTF-18, so all 16-byte sequences are
   encodable. */
static intptr_t utf16ish_to_utf8ish(const unsigned short *us, intptr_t end, unsigned char *s)
{
  intptr_t i, j;
  unsigned int wc;
  
  j = 0;
  for (i = 0; i < end; i++) {
    wc = us[i];
    if ((wc & 0xF800) == 0xD800) {
      /* Unparse surrogates. */
      if ((wc & 0xFC00) != 0xD800) {
        /* Count as one */
      } else if ((i + 1 >= end)
                 || (((us[i+1]) & 0xFC00) != 0xDC00)) {
        /* Let the misplaced surrogate through */
      } else {
        i++;
        wc = ((wc & 0x3FF) << 10) + ((us[i]) & 0x3FF);
        wc += 0x10000;
      }
    }
    if (!s) {
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
    } else {
      if (wc < 0x80) {
        s[j++] = wc;
      } else if (wc < 0x800) {
        s[j++] = 0xC0 | ((wc & 0x7C0) >> 6);
        s[j++] = 0x80 | (wc & 0x3F);
      } else if (wc < 0x10000) {
        s[j++] = 0xE0 | ((wc & 0xF000) >> 12);
        s[j++] = 0x80 | ((wc & 0x0FC0) >> 6);
        s[j++] = 0x80 | (wc & 0x3F);
      } else if (wc < 0x200000) {
        s[j++] = 0xF0 | ((wc & 0x1C0000) >> 18);
        s[j++] = 0x80 | ((wc & 0x03F000) >> 12);
        s[j++] = 0x80 | ((wc & 0x000FC0) >> 6);
        s[j++] = 0x80 | (wc & 0x3F);
      } else if (wc < 0x4000000) {
        s[j++] = 0xF8 | ((wc & 0x3000000) >> 24);
        s[j++] = 0x80 | ((wc & 0x0FC0000) >> 18);
        s[j++] = 0x80 | ((wc & 0x003F000) >> 12);
        s[j++] = 0x80 | ((wc & 0x0000FC0) >> 6);
        s[j++] = 0x80 | (wc & 0x3F);
      } else {
        s[j++] = 0xFC | ((wc & 0x40000000) >> 30);
        s[j++] = 0x80 | ((wc & 0x3F000000) >> 24);
        s[j++] = 0x80 | ((wc & 0x00FC0000) >> 18);
        s[j++] = 0x80 | ((wc & 0x0003F000) >> 12);
        s[j++] = 0x80 | ((wc & 0x00000FC0) >> 6);
        s[j++] = 0x80 | (wc & 0x3F);
      }  
    }
  }

  return j;
}

#define RKTIO_MAX_IDEAL_BUFFER_SIZE 4096

wchar_t *rktio_convert_to_wchar(rktio_t *rktio, const char *s, int do_copy)
{
  intptr_t len, l;
  wchar_t *ws;

  l = strlen(s)+1; /* add nul terminator */

  len = utf8ish_to_utf16ish((unsigned char *)s, l, NULL);
  if (len < 0) {
    set_racket_error(RKTIO_ERROR_INVALID_PATH);
    return NULL;
  }
  
  if (do_copy)
    ws = malloc(sizeof(wchar_t) * len);
  else if (len > rktio->wide_buffer_size) {
    free(rktio->wide_buffer);
    rktio->wide_buffer_size = len;
    ws = malloc(sizeof(wchar_t) * len);
    rktio->wide_buffer = ws;
  } else if ((len < RKTIO_MAX_IDEAL_BUFFER_SIZE)
             && (rktio->wide_buffer_size > RKTIO_MAX_IDEAL_BUFFER_SIZE)) {
    free(rktio->wide_buffer);
    rktio->wide_buffer_size = RKTIO_MAX_IDEAL_BUFFER_SIZE;
    ws = malloc(sizeof(wchar_t) * RKTIO_MAX_IDEAL_BUFFER_SIZE);
    rktio->wide_buffer = ws;
  } else
    ws = rktio->wide_buffer;

  (void)utf8ish_to_utf16ish((unsigned char *)s, l, (unsigned short*)ws);

  return ws;
}

char *rktio_convert_from_wchar(const wchar_t *ws, int free_given)
{
  intptr_t len, l;
  char *s;

  l = wcslen(ws) + 1; /* add nul terminator */

  len = utf16ish_to_utf8ish((unsigned short *)ws, l, NULL);

  s = malloc(len);

  len = utf16ish_to_utf8ish((unsigned short *)ws, l, (unsigned char *)s);

  if (free_given)
    free((void *)ws);

  return s;
}

rktio_char16_t *rktio_path_to_wide_path(rktio_t *rktio, const char *p)
{
  return WIDE_PATH_copy(p);
}

char *rktio_wide_path_to_path(rktio_t *rktio, const rktio_char16_t *wp)
{
  return NARROW_PATH_copy(wp);
}

#endif

/*============================================================*/

/* The same as strndup(), but sometimes strndup() is missing */
char *rktio_strndup(char *s, intptr_t len)
{
  char *s2;
  s2 = malloc(len + 1);
  memcpy(s2, s, len);
  s2[len] = 0;
  return s2;
}
