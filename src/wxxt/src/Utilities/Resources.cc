/*								-*- C++ -*-
 *
 * Purpose: read/write .Xdefaults
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

#define  Uses_XLib
#define  Uses_wxList
#include "wx.h"

#include <ctype.h>
#include <string.h>
#include <unistd.h>

#if defined(SVR4) && !defined(__sgi)
# include <sys/systeminfo.h>
#endif

//-----------------------------------------------------------------------------
// We have a cache for writing different resource files,
// which will only get flushed when we call wxFlushResources().
// Build up a list of resource databases waiting to be written.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// utility functions for get/write resources
//-----------------------------------------------------------------------------

static char *GetIniFile(char *dest, const char *filename)
{
    char *home = NULL;
    if (filename) {
      strcpy(dest, filename);
    } else if ((home = wxGetUserHome(NULL)) != NULL) {
      strcpy(dest, home);
      if (dest[strlen(dest) - 1] != '/')
	strcat(dest, "/");
      strcat(dest, ".mred.resources");
    } else {
      dest[0] = '\0';    
    }
    return dest;
}

static XrmDatabase wxXrmGetFileDatabase(const char *s)
{
  /* because directory names crash XrmGetFileDatabase */
  if (!wxDirExists((char *)s))
    return XrmGetFileDatabase(s);
  else
    return NULL;
}

static void wxXMergeDatabases(void)
{
    XrmDatabase homeDB, serverDB, applicationDB, userDB;
    char filenamebuf[1024];
    char *filename = &filenamebuf[0];
    char *environment;
    char *classname = wxAPP_CLASS;
    char name[256];
    char *home, *dest;

    (void)strcpy(name, "/usr/lib/X11/app-defaults/");
    strcat(name, classname ? classname : "wxWindows");

    // Get application defaults file, if any 
    if ((applicationDB = wxXrmGetFileDatabase(name)))
      XrmMergeDatabases(applicationDB, &wxResourceDatabase);

    // Merge server defaults, created by xrdb, loaded as a property of the root
    // window when the server initializes and loaded into the display
    // structure on XOpenDisplay;
    // if not defined, use .Xdefaults
    if (XResourceManagerString(wxAPP_DISPLAY) != NULL) {
	serverDB = XrmGetStringDatabase(XResourceManagerString(wxAPP_DISPLAY));
    } else {
      // Get X defaults file, if any 
      home = wxGetUserHome(NULL);
      if (home) {
	dest = new char[strlen(home) + 20];
	
	strcpy(dest, home);
	if (dest[strlen(dest) - 1] != '/')
	  strcat(dest, "/");
	strcat(dest, ".Xdefaults");
	
	serverDB = wxXrmGetFileDatabase(dest);
      } else
	serverDB = NULL;
    }
    if (serverDB)
      XrmMergeDatabases(serverDB, &wxResourceDatabase);

    // Open XENVIRONMENT file, or if not defined, the .Xdefaults,
    // and merge into existing database

    if ((environment = getenv("XENVIRONMENT")) == NULL) {
      size_t len;
      environment = GetIniFile(filename, NULL);
      len = strlen(environment);
#if !defined(SVR4) || defined(__sgi)
      gethostname(environment XFORM_OK_PLUS len, 1024 - len);
#else
      sysinfo(SI_HOSTNAME, environment XFORM_OK_PLUS len, 1024 - len);
#endif
    }
    if ((homeDB = wxXrmGetFileDatabase(environment)))
      XrmMergeDatabases(homeDB, &wxResourceDatabase);


    // Get user defaults file, if any 
    home = wxGetUserHome(NULL);
    if (home) {
      dest = new char[strlen(home) + 20];
      
      strcpy(dest, home);
      if (dest[strlen(dest) - 1] != '/')
	strcat(dest, "/");
      strcat(dest, ".mred.resources");
      
      if ((userDB = wxXrmGetFileDatabase(dest)))
	(void)XrmMergeDatabases(userDB, &wxResourceDatabase);
    }
}

//-----------------------------------------------------------------------------
// write resource functions
//-----------------------------------------------------------------------------
Bool wxWriteResource(const char *section, const char *entry, char *value,
		     const char *file)
{
    char buffer[500];
    char resName[300];
    int isnull;
    XrmDatabase database;
    wxNode *node;

    if (!entry)
      return FALSE;

    (void)GetIniFile(buffer, file);

    node = wxResourceCache->Find(buffer);
    if (node)
      database = (XrmDatabase)node->Data();
    else {
      database = wxXrmGetFileDatabase(buffer);
      node = wxResourceCache->Append(buffer, (wxObject *)database);
    }
    strcpy(resName, section ? section : "wxWindows");
    strcat(resName, ".");
    strcat(resName, entry);

    isnull = !database;
    XrmPutStringResource(&database, resName, value);
    if (isnull) {
      if (node)
	wxResourceCache->DeleteNode(node);
      wxResourceCache->Append(buffer, (wxObject *)database);
    }

    XrmPutFileDatabase(database, buffer);

    return TRUE;
}

Bool wxWriteResource(const char *section, const char *entry, float value,
		     const char *file)
{
    char buf[50];
    sprintf(buf, "%.4f", value);
    return wxWriteResource(section, entry, buf, file);
}

Bool wxWriteResource(const char *section, const char *entry, long value,
		     const char *file)
{
    char buf[50];
    sprintf(buf, "%ld", value);
    return wxWriteResource(section, entry, buf, file);
}

Bool wxWriteResource(const char *section, const char *entry, int value,
		     const char *file)
{
    char buf[50];
    sprintf(buf, "%d", value);
    return wxWriteResource(section, entry, buf, file);
}

//-----------------------------------------------------------------------------
// get resource functions
//-----------------------------------------------------------------------------
Bool wxGetResource(const char *section, const char *entry, char **value,
		   const char *file)
{
    XrmValue xvalue;
    char *str_type[20];
    char buf[150];
    Bool success;
    XrmDatabase database;

    if (!wxResourceDatabase)
	wxXMergeDatabases();

    if (file) {
      char buffer[500];
      wxNode *node;

      (void)GetIniFile(buffer, file);
      
      node = wxResourceCache->Find(buffer);
      if (node)
	database = (XrmDatabase)node->Data();
      else {
	database = wxXrmGetFileDatabase(buffer);
	wxResourceCache->Append(buffer, (wxObject *)database);
      }
    } else
      database = wxResourceDatabase;
    
    strcpy(buf, section);
    strcat(buf, ".");
    strcat(buf, entry);

    success = XrmGetResource(database, buf, "*", str_type, &xvalue);
    if (success) {
      char *v;
      v = new char[xvalue.size + 1];
      *value = v;
      strncpy(*value, xvalue.addr, (int)xvalue.size);
      return TRUE;
    }
    return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, float *value,
		   const char *file)
{
  char *s = NULL;
  Bool succ;
  succ = wxGetResource(section, entry, &s, file);
  if (succ) {
    float v;
    v = (float)strtod(s, NULL);
    *value = v;
    return TRUE;
  } else
    return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, long *value,
		   const char *file)
{
  char *s = NULL;
  Bool succ;
  succ = wxGetResource(section, entry, &s, file);
  if (succ) {
    long v;
    v = strtol(s, NULL, 10);
    *value = v;
    return TRUE;
  } else
    return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, int *value,
		   const char *file)
{
    char *s = NULL;
    Bool succ;
    succ = wxGetResource(section, entry, &s, file);
    if (succ) {
	// Handle True, False here 
	// True, Yes, Enables, Set or  Activated 
	if (*s == 'T' || *s == 'Y' || *s == 'E' || *s == 'S' || *s == 'A')
	    *value = TRUE;
	// False, No, Disabled, Reset, Cleared, Deactivated
	else if (*s == 'F' || *s == 'N' || *s == 'D' || *s == 'R' || *s == 'C')
	    *value = FALSE;
	// Handle as Integer
	else {
	  int v;
	  v = (int)strtol(s, NULL, 10);
	  *value = v;
	}
	return TRUE;
    } else
	return FALSE;
}
