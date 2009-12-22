/*
 * File:	wx_utils.h
 * Purpose:	Miscellaneous utilities
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_utilsh
#define wxb_utilsh

#include "wx_setup.h"
#include "wx_obj.h"
#include "wx_list.h"
#include "wx_win.h"

#include <stdio.h>

// Return a string with the current date/time
char *wxNow(void);

// Make a copy of this string using 'new'
char *copystring(const char *s, long offset = 0);

// Useful buffer
extern char *wxBuffer;

// Some file utilities

Bool wxFileExists(const char *filename);
#define FileExists wxFileExists

Bool wxDirExists(const char *dir);
#define DirExists wxDirExists

// Get filename
char *wxFileNameFromPath(char *path);
#define FileNameFromPath wxFileNameFromPath

// Get directory
char *wxPathOnly(char *path);
#define PathOnly wxPathOnly

// Strip the extension, in situ
void wxStripExtension(char *buffer);

// Get a temporary filename, opening and closing the file.
char *wxGetTempFileName(const char *prefix, char *buf = NULL);

// Remove file
Bool wxRemoveFile(const char *file);

// Consume all events until no more left
void wxFlushEvents(void);

/*
 * Network and username functions.
 *
 */

// Get eMail address
Bool wxGetEmailAddress(char *buf, int maxSize);

// Get hostname.
Bool wxGetHostName(char *buf, int maxSize);

// Get user ID e.g. jacs
Bool wxGetUserId(char *buf, int maxSize);

// Get user name e.g. Julian Smart
Bool wxGetUserName(char *buf, int maxSize);

/*
 * Strip out any menu codes
 */
char *wxStripMenuCodes(char *in, char *out = NULL);

#if (!defined(__MINMAX_DEFINED) && !defined(max))
# define max(a,b)            (((a) > (b)) ? (a) : (b))
# define min(a,b)            (((a) < (b)) ? (a) : (b))
# define __MINMAX_DEFINED 1
#endif

// Yield to other apps/messages
Bool wxYield(void);

// Format a message on the standard error (UNIX) or the debugging
// stream (Windows)
void wxDebugMsg(const char *fmt ...);
 
// Sound the bell
void wxBell(void);
  
// Get OS version
int wxGetOsVersion(int *majorVsn=NULL,int *minorVsn=NULL);
 
// Set the cursor to the busy cursor for all windows
class wxCursor;
extern wxCursor *wxHOURGLASS_CURSOR;
void wxBeginBusyCursor(wxCursor *cursor = wxHOURGLASS_CURSOR);
 
extern int wxGetBusyState(void);
extern void wxSetBusyState(int);

// Restore cursor to normal
void wxEndBusyCursor(void);
 
// TRUE if we're between the above two calls
Bool wxIsBusy(void);

void wxHideCursor(void);
void wxUnhideCursor(void);
void wxCanHideCursor(void);
void wxCannotHideCursor(void);
  
/* Error message functions used by wxWindows */

// Non-fatal error (continues) 
void wxError(const char *msg, const char *title = "wxWindows Internal Error");

// Fatal error (exits)
void wxFatalError(const char *msg, const char *title = "wxWindows Fatal Error");

// Reading and writing resources (eg WIN.INI, .Xdefaults)
Bool wxWriteResource(const char *section, const char *entry, char *value, const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, float value, const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, long value, const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, int value, const char *file = NULL);

Bool wxGetResource(const char *section, const char *entry, char **value, const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, float *value, const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, long *value, const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, int *value, const char *file = NULL);

// Get the user's home dir (caller must copy--- volatile)
// returns NULL is no HOME dir is known
char *wxGetUserHome(const char *user = NULL);

int wx_wstrlen(wchar_t *ws);
wchar_t *wx_convert_to_wchar(char *s, int do_copy);
char *wx_convert_from_wchar(wchar_t *ws);
#define wxWIDE_STRING(s) wx_convert_to_wchar(s, 0)
#define wxWIDE_STRING_COPY(s) wx_convert_to_wchar(s, 1)
#define wxNARROW_STRING(ws) wx_convert_from_wchar(ws) 

#endif // wxb_utilsh
