/*								-*- C++ -*-
 *
 * Purpose: common utilities
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

#ifndef wxUtilities_h
#define wxUtilities_h

#ifdef __GNUG__
#pragma interface
#endif

#undef  min
#define min(a,b) ((a)<(b)?(a):(b))
#undef  max
#define max(a,b) ((a)>(b)?(a):(b))
#undef  wxNumberOf
#define wxNumberOf(arr) ((int)(sizeof(arr)/sizeof(arr[0])))

// application utilities
void wxExit(void);
void wxFlushEvents(void);
Bool wxYield(void);

// File functions
Bool wxDirExists(char *dirname);
Bool wxFileExists(char *filename);
Bool wxRemoveFile(char *filename);

char *wxFileNameFromPath(char *path);
char *wxPathOnly(char *path);
char *wxGetUserHome(const char *user);
char *wxGetTempFileName(char *prefix, char *dest);

#define DirExists wxDirExists
#define FileExists wxFileExists
#define FileNameFromPath wxFileNameFromPath
#define PathOnly wxPathOnly

// Network functions
extern "C" {
#include "Net.h"
}

// String functions
char *copystring(const char *s);
#ifdef MZ_PRECISE_GC
char *copystring_to_aligned(const char *s, int d);
#endif
void wxGetLabelAndKey(char *label, char **clean_label, char **clean_key);
char *wxStripMenuCodes(char *in, char *out);

// GDI functions
class wxCursor;
Bool wxColourDisplay(void);
int  wxDisplayDepth(void);
void wxDisplaySize(int *width, int *height, int flags = 0);
void wxDisplayOrigin(int *x, int *y);

// busy cursor
class wxCursor;
void wxBeginBusyCursor(wxCursor *cursor = wxHOURGLASS_CURSOR);
void wxEndBusyCursor(void);
Bool wxIsBusy(void);

void wxHideCursor(void);
void wxUnhideCursor(void);
int wxCheckHiddenCursors();
int wxUnhideCursorInFrame(wxObject *o, int busy);

// Miscellaneous
void wxDebugMsg(const char *fmt, ...);
void wxError(const char *msg, const char *title="wxWindows Error");
Bool wxExecute(char **argv, Bool Async = FALSE);
Bool wxExecute(const char *command, Bool Async = FALSE);
void wxFatalError(const char *msg, const char *title="wxWindows Fatal Error");
char *wxNow(void);

// read/write resources
Bool wxGetResource(const char *section, const char *entry, char **value,
		   const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, float *value,
		   const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, long *value,
		   const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, int *value,
		   const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, char *value,
		     const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, float value,
		     const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, long value,
		     const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, int value,
		     const char *file = NULL);

#endif // wxUtilities_h
