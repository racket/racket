/*								-*- C++ -*-
 *
 * Purpose: string copy and conversion
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2005 PLT Scheme, Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#define  Uses_wxObject
#include "wx.h"

#include <string.h>

#ifdef _AIX41
# include <strings.h>
#endif

char *copystring(const char *s)
{
  size_t len;
  char *news;

  if (s == NULL) s = "";
  len = strlen (s) + 1;
  news = new WXGC_ATOMIC char[len];
  memcpy(news, s, len);        // Should be the fastest
  return news;
}

#ifdef MZ_PRECISE_GC
char *copystring_to_aligned(const char *s, int d)
{
  size_t len;
  char *news;

  len = strlen(s XFORM_OK_PLUS d);
  news = new WXGC_ATOMIC char[len + 1];
  memcpy(news, s + d, len + 1);
  return news;
}
#endif

void wxGetLabelAndKey(char *label, char **clean_label, char **clean_key)
{
    char *key, *s;

    s = copystring(label); // make private copy
    *clean_label = s;
#if 0
    char *amp;
    if ((amp = strchr(*clean_label, '&'))) { // is there an ampersand? -> erase
	memmove(amp, amp+1, strlen(amp+1) + 1);
    }
#endif
    if ((key=strchr(*clean_label, '\t'))) // is there a key binding? -> split
	*key++ ='\0';
    if (clean_key)
      *clean_key = key; // point to key binding in private copy
}

char *wxStripMenuCodes(char *in, char *out)
{
  char *tmpOut;

  if (!in)
    return NULL;
  if (!out) {
	out = copystring(in);
  }
  tmpOut = out;
  
  while (*in)  {
    if (*in == '&') {
      // Check && -> &, &x -> x
      if (*++in == '&')
	*out++ = *in++;
    } else if (*in == '\t') {
      // Remove all stuff after \t in X mode, and let the stuff as is
      // in Windows mode.
      // Accelerators are handled in wx_item.cc for Motif, and are not
      // YET supported in XView
      break;
    } else
      *out++ = *in++;
  }

  *out = '\0';
  return tmpOut;
}
