/*
 * File:        wb_utils.cc
 * Purpose:     Miscellaneous utilities
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * Copyright:   (c) 2004-2005 PLT Scheme, Inc.
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

char *copystring (const char *s, long offset)
{
  char *news;
  size_t len;

  if (s == NULL) s = "";
  len = strlen(s XFORM_OK_PLUS offset) + 1;

  news = new char[len];
  memcpy(news, s + offset, len);

  return news;
}

Bool 
wxFileExists (const char *filename)
{
  struct stat stbuf;

  // (char *) cast necessary for VMS
  if (filename && stat ((char *)filename, &stbuf) == 0)
    return TRUE;
  return FALSE;
}

/*
 * Strip off any extension (dot something) from end of file,
 * IF one exists. Inserts zero into buffer.
 *
 */
 
void wxStripExtension(char *buffer)
{
  int len, i;
  
  len = strlen(buffer);
  i = len-1;
  while (i > 0)
  {
    if (buffer[i] == '.')
    {
      buffer[i] = 0;
      break;
    }
    i --;
  }
}

// Return just the filename, not the path
// (basename)
char *
wxFileNameFromPath (char *path)
{
  if (path) {
    char *tcp = path;
    int tcpd;
    
    tcpd = strlen(path);
    while (--tcpd >= 0) {
      if ((tcp[tcpd] == '/') || (tcp[tcpd] == '\\'))
	return copystring(tcp, tcpd + 1);
    }
    if (isalpha(path[0]) && path[1] == ':')
      return copystring(path, 2);
  }

  return path;
}

// Return just the directory, or NULL if no directory
char *
wxPathOnly(char *path)
{
  if (path && *path) {
    char *buf;
    int i, l;
    Bool done = FALSE;

    buf = copystring(path);

    l = strlen(path);

    i = l - 1;

    // Search backward for a backward or forward slash
    while (!done && i > -1) {
      if (path[i] == '/' || path[i] == '\\') {
	done = TRUE;
	buf[i] = 0;
	return buf;
      } else 
	i --;
    }

    // Try Drive specifier
    if (isalpha (buf[0]) && buf[1] == ':') {
      // A:junk --> A:. (since A:.\junk Not A:\junk)
      buf[2] = '.';
      buf[3] = '\0';
      return buf;
    }
  }
  
  return NULL;
}

char *wxNow( void )
{
  time_t now;
  char *date;

  now = time(NULL);
  date = ctime(&now); 
  date[24] = '\0';
  return date;
}

/* Get Full RFC822 style email address */
Bool wxGetEmailAddress (char *address, int maxSize)
{
  char host[65];
  char user[65];
  char tmp[130];

  if (wxGetHostName(host, 64) == FALSE)
    return FALSE;
  if (wxGetUserId(user, 64) == FALSE)
    return FALSE;

  strcpy(tmp, user);
  strcat(tmp, "@");
  strcat(tmp, host);

  strncpy(address, tmp, maxSize - 1);
  address[maxSize-1] = '\0';
  return TRUE;
}

/*
 * Strip out any menu codes
 */

char *wxStripMenuCodes (char *in, char *out)
{
  int inp, outp;

  if (!in)
    return NULL;
    
  if (!out)
    out = copystring(in);

  inp = outp = 0;

  while (in[inp]) {
    if (in[inp] == '&') {
      // Check && -> &, &x -> x
      if (in[++inp] == '&')
	out[outp++] = in[inp++];
    } else if (in[inp] == '\t') {
      // Remove all stuff after \t in X mode, and let the stuff as is
      // in Windows mode.
      // Accelerators are handled in wx_item.cc for Motif, and are not
      // YET supported in XView
      break;
    } else
      out[outp++] = in[inp++];
  }

  out[outp] = '\0';
  
  return out;
}
