/*
 * File:	wx_utils.h
 * Purpose:	Miscellaneous utilities
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	Oct 7, 1995 - Cecil Coupe, added wxRmdir() prototype
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_utilsh
#define wxb_utilsh

#include "wx_setup.h"
#include "wx_obj.h"
#include "wx_list.h"
#include "wx_win.h"

#include <stdio.h>

// Forward declaration
class wxFrame;

// Stupid ASCII macros
#define   wxToUpper(C)      (((C) >= 'a' && (C) <= 'z')? (C) - 'a' + 'A': (C))
#define   wxToLower(C)      (((C) >= 'A' && (C) <= 'Z')? (C) - 'A' + 'a': (C))

// Return a string with the current date/time
char *wxNow(void);

// Make a copy of this string using 'new'
char *copystring(const char *s);
#ifdef MZ_PRECISE_GC
char *copystring_to_aligned(const char *s, int d);
#endif

// Generate a unique ID
long wxNewId(void);
#define NewId wxNewId

// Ensure subsequent IDs don't clash with this one
void wxRegisterId(long id);
#define RegisterId wxRegisterId

// Return the current ID
long wxGetCurrentId(void);

// Useful buffer
extern char *wxBuffer;

// Various conversions
void StringToFloat(char *s, float *number);
char *FloatToString(float number);
void StringToDouble(char *s, double *number);
char *DoubleToString(double number);
void StringToInt(char *s, int *number);
void StringToLong(char *s, long *number);
char *IntToString(int number);
char *LongToString(long number);

// Some file utilities

// Get filename
char *wxFileNameFromPath(char *path);
#define FileNameFromPath wxFileNameFromPath

// Get directory
char *wxPathOnly(char *path);
#define PathOnly wxPathOnly

void wxDos2UnixFilename(char *s);
#define Dos2UnixFilename wxDos2UnixFilename

void wxUnix2DosFilename(char *s);
#define Unix2DosFilename wxUnix2DosFilename

// Get a temporary filename, opening and closing the file.
char *wxGetTempFileName(const char *prefix, char *buf = NULL);

void wxRemoveFile(char *filename);

// Sleep for nSecs seconds under UNIX, do nothing under Windows
void wxSleep(int nSecs);

// Get free memory in bytes, or -1 if cannot determine amount (e.g. on UNIX)
long wxGetFreeMemory(void);

// Consume all events until no more left
void wxFlushEvents(void);
void wxFlushResources(void);

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
char *wxStripMenuCodes(char *in, char *out);

// Find the window/widget with the given title or label.
// Pass a parent to begin the search from, or NULL to look through
// all windows.
wxWindow *wxFindWindowByLabel(char *title, wxWindow *parent = NULL);

// Find window by name, and if that fails, by label.
wxWindow *wxFindWindowByName(char *name, wxWindow *parent = NULL);

// Returns menu item id or -1 if none.
int wxFindMenuItemId(wxFrame *frame, char *menuString, char *itemString);

// Format a message on the standard error (UNIX) or the debugging
// stream (Windows)
void wxDebugMsg(const char *fmt ...) ;
 
// Sound the bell
void wxBell(void) ;
  
// Get OS version
int wxGetOsVersion(int *majorVsn=NULL,int *minorVsn=NULL) ;
 
// Set the cursor to the busy cursor for all windows
class wxCursor;
extern wxCursor *wxHOURGLASS_CURSOR;
void wxBeginBusyCursor(wxCursor *cursor = wxHOURGLASS_CURSOR);
 
// Restore cursor to normal
void wxEndBusyCursor(void);
 
// TRUE if we're between the above two calls
Bool wxIsBusy(void);
  
void wxHideCursor(void);
void wxUnhideCursor(void);

/* Error message functions used by wxWindows */

// Non-fatal error (continues) 
void wxError(const char *msg, const char *title = "wxWindows Internal Error");

// Fatal error (exits)
void wxFatalError(const char *msg, const char *title = "wxWindows Fatal Error");

Bool wxDirExists(char *f);

// Reading and writing resources (eg WIN.INI, .Xdefaults)
#if USE_RESOURCES
Bool wxWriteResource(const char *section, const char *entry, char *value, const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, float value, const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, long value, const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, int value, const char *file = NULL);

Bool wxGetResource(const char *section, const char *entry, char **value, const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, float *value, const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, long *value, const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, int *value, const char *file = NULL);
#endif // USE_RESOURCES

// converting an FSRef to a UNIX path:
char *wxFSRefToPath(FSRef fsref);

CFStringRef wxCFString(const char *s);

#endif // wxb_utilsh
