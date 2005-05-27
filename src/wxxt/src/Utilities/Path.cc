/*								-*- C++ -*-
 *
 * Purpose: path- and filename manipulations
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

#include "wx.h"

#include <string.h>

char *wxFileNameFromPath(char *path)
{
    if (path) {
      int tcp, slen;
      
      tcp = slen = strlen(path);
      while (--tcp >= 0) {
	if (path[tcp] == '/') {
	  char *naya;
	  tcp++;
	  naya = new char[slen + 1 - tcp];
	  memcpy(naya, path + tcp, slen + 1 - tcp);
	  return naya;
	}
      }
    }

    return path;
}

char *wxPathOnly(char *path)
{
  if (path) {
    int p, last_slash = 0;
    char *buf;

    buf = new char[strlen(path) + 1];
    
    // copy path and keep the last slash or baskslash in mind
    for (p = 0; path[p]; p++) {
      buf[p] = path[p];
      if (buf[p] == '/')
	last_slash = p;
    }
    if (last_slash) {
      buf[last_slash] = 0;
      return buf;
    }
  }

  return NULL;
}
