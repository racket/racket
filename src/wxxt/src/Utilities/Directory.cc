/*								-*- C++ -*-
 *
 * Purpose: basic file and directory handling
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

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

Bool wxDirExists(char *dir)
{
    struct stat sbuf;
    return ((stat(dir, &sbuf) != -1) && S_ISDIR(sbuf.st_mode) ? TRUE : FALSE);
}

Bool wxFileExists(char *filename)
{
    struct stat stbuf;

    return (filename && !stat((char *)filename, &stbuf));
}

Bool wxRemoveFile(char *file)
{
    return !unlink(file);
}
