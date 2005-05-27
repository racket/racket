/*								-*- C++ -*-
 *
 * Purpose: get user home info
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

#include <pwd.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

char *wxGetUserHome(const char *user)
{
    struct passwd *who = NULL;

    if (user == NULL || *user == '\0') {
	register char *ptr;

	if ((ptr = getenv("HOME")) != NULL) 
	    return ptr;
	if ((ptr = getenv("USER")) != NULL
	|| (ptr = getenv("LOGNAME")) != NULL) {
	    who = getpwnam(ptr);
	}
	// We now make sure the the user exists!
	if (who == NULL)
	    who = getpwuid(getuid());
    } else
	who = getpwnam (user);
    return who ? who->pw_dir : (char*)NULL;
}
