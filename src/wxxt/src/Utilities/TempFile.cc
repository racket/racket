/*								-*- C++ -*-
 *
 * Purpose: filename for temporary files
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

#include <unistd.h>
#include <string.h>

char *wxGetTempFileName(char *prefix, char *dest)
{
    static short last_temp = 0;	// cache last to speed things a bit
    // At most 1000 temp files to a process! We use a ring count.
    char buf[64];
    short suffix;

    for (suffix = last_temp + 1; suffix != last_temp; ++suffix %= 1000) {
      int pid;
      pid = (int)getpid();
      sprintf (buf, "/tmp/%s%d.%03x", prefix, pid, (int)suffix);
      if (!wxFileExists(buf)) {
	// Touch the file to create it (reserve name)
	FILE *fd;
	fd = fopen (buf, "w");
	if (fd)
	  fclose (fd);
	last_temp = suffix;
	if (dest)
	  strcpy(dest, buf);
	else {
	  dest = copystring(buf);
	}
	return dest;
	}
    }
    
    wxError("wxWindows: error finding temporary file name.");
    if (dest) dest[0] = 0;

    return NULL;
}

