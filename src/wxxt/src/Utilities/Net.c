/*								-*- C++ -*-
 *
 * Purpose: host and user net info
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

#include "gc.h"
#include "Net.h"

#include <pwd.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#if !defined(SVR4) || defined(__sgi)
# define WX_USE_GETHOSTBYNAME
# include <netdb.h>
#endif

int wxGetHostName(char *buf, int sz)
{
#ifndef WX_USE_GETHOSTBYNAME
  return (sysinfo(SI_HOSTNAME, buf, maxSize) != -1);
#else /* BSD Sockets */
  char name[255];
  /* Get hostname */
  if ((gethostname(name, sizeof(name)/sizeof(char)-1)) == -1)
    return 0;
  strncpy(buf, name, sz-1);
  buf[sz-1] = 0;
  return 1;
#endif
}

int wxGetEmailAddress(char *address, int maxSize)
{
  char host[65];
  char user[65];
  char tmp[130];

  if (wxGetHostName(host, 64) == 0)
    return 0;
  if (wxGetUserId(user, 64) == 0)
    return 0;

  strcpy(tmp, user);
  strcat(tmp, "@");
  strcat(tmp, host);
  strncpy(address, tmp, maxSize - 1);
  address[maxSize-1] = '\0';
  return 1;
}

int wxGetUserId(char *buf, int sz)
{
  struct passwd *who;
  
  if ((who = getpwuid(getuid ())) != NULL) {
    strncpy (buf, who->pw_name, sz-1);
    buf[sz - 1]= 0;
    return 1;
  }
  return 0;
}

int wxGetUserName(char *buf, int sz)
{
  struct passwd *who;
  
  if ((who = getpwuid (getuid ())) != NULL) {
    strncpy (buf, who->pw_gecos, sz - 1);
    buf[sz - 1]= 0;
    return 1;
  }
  return 0;
}
