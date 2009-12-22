/*
 * File:	wx_utils.cc
 * Purpose:	Various utilities
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include <ctype.h>
#include <direct.h>

#include <dos.h>

#define WX_USE_GLOBAL_SLEEP 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

extern "C" {
  int scheme_utf8_decode(const unsigned char *s, int start, int len, 
			 unsigned int *us, int dstart, int dlen,
			 long *ipos, char utf16, int permissive);
  int scheme_utf8_encode(const unsigned int *us, int start, int len, 
			 unsigned char *s, int dstart,
			 char utf16);
};

// In the WIN.INI file
static const char WX_SECTION[] = "wxWindows";
static const char eHOSTNAME[]  = "HostName";
static const char eUSERID[]    = "UserId";
static const char eUSERNAME[]  = "UserName";

// For the following functions we SHOULD fill in support
// for Windows-NT (which I don't know) as I assume it begin
// a POSIX Unix (so claims MS) that it has some special
// functions beyond those provided by WinSock

// Get full hostname (eg. DoDo.BSn-Germany.crg.de)
Bool wxGetHostName(char *buf, int maxSize)
{
  char *sysname;
  const char *default_host = "noname";

  if ((sysname = getenv("SYSTEM_NAME")) == NULL) {
     GetProfileString(WX_SECTION, eHOSTNAME, default_host, buf, maxSize - 1);
  } else
    strncpy(buf, sysname, maxSize - 1);
  buf[maxSize] = '\0';
  return *buf ? TRUE : FALSE;
}

// Get user ID e.g. jacs
Bool wxGetUserId(char *buf, int maxSize)
{
  char *user;
  const char *default_id = "anonymous";

  // Can't assume we have NIS (PC-NFS) or some other ID daemon
  // So we ...
  if (	(user = getenv("USER")) == NULL &&
	(user = getenv("LOGNAME")) == NULL ) {
     // Use wxWindows configuration data (comming soon)
     GetProfileString(WX_SECTION, eUSERID, default_id, buf, maxSize - 1);
  } else
    strncpy(buf, user, maxSize - 1);
  return *buf ? TRUE : FALSE;
}

// Get user name e.g. Julian Smart
Bool wxGetUserName(char *buf, int maxSize)
{
  const char *default_name = "Unknown User";

  // Could use NIS, MS-Mail or other site specific programs
  // Use wxWindows configuration data 
  GetProfileString(WX_SECTION, eUSERNAME, default_name, buf, maxSize - 1);

  return *buf ? TRUE : FALSE;
}

Bool wxRemoveFile(const char *file)
{
  int flag;
  flag = remove(file);
  if (flag == 0) return TRUE;
  return FALSE;
}

// Get a temporary filename, opening and closing the file.
char *wxGetTempFileName(const char *prefix, char *buf)
{
  char tmp[MAX_PATH];
  char tmpPath[MAX_PATH];
  ::GetTempPath(MAX_PATH, tmpPath);
  ::GetTempFileName(tmpPath, prefix, 0, tmp);

  if (buf) strcpy(buf, tmp);
  else buf = copystring(tmp);
  return buf;
}

// Consume all events until no more left
void wxFlushEvents(void)
{
}

// Output a debug mess., in a system dependent fashion.
void wxDebugMsg(const char *fmt ...)
{
}

// Non-fatal error: pop up message box and (possibly) continue
void wxError(const char *msg, const char *title)
{
  int msAns;

  sprintf(wxBuffer, "%s\nContinue?", msg);

  msAns = MessageBox(NULL, (LPCSTR)wxBuffer, (LPCSTR)title,
		     MB_ICONSTOP | MB_YESNO);
  
  if (msAns == IDNO)
    wxExit();
}

// Fatal error: pop up message box and abort
void wxFatalError(const char *msg, const char *title)
{
  sprintf(wxBuffer, "%s: %s", title, msg);
  FatalAppExit(0, (LPCSTR)wxBuffer);
}

// Emit a beeeeeep
void wxBell(void)
{
  MessageBeep(MB_OK);
}

static char *wxUserResourceFile;

void wxInitUserResource(char *s)
{
   wxUserResourceFile = s;
}

static int wxDoResource(HKEY key, const char *entry, char **value_get, char *value_set) 
{
  int key_needs_close = 0, elen, sep_pos, success = 0;
  unsigned long rlen;
  
  /* Split the value name from the registry entry: */
  elen = strlen(entry);
  for (sep_pos = elen - 1; (sep_pos >= 0) && (entry[sep_pos] != '\\'); sep_pos -= 1) {
  }
  if (sep_pos >= 0) {
    HKEY hKey;
    char *new_entry;

    new_entry = new char[sep_pos + 1];
    memcpy(new_entry, entry, sep_pos);
    new_entry[sep_pos] = 0;
      
    /* Get a key for the entry: */
    if (RegOpenKeyEx(key, new_entry, 0, value_get ? KEY_QUERY_VALUE : KEY_SET_VALUE, &hKey)
	== ERROR_SUCCESS) {
      key = hKey;
      key_needs_close = 1;
    } else
      return FALSE;
    /* name starts after the separator */
    sep_pos++;
  } else
    sep_pos = 0;

  if (value_get) {
    /* Get the value. Start by finding out how big it is: */
    rlen = 0;
    if (RegQueryValueEx(key, entry XFORM_OK_PLUS sep_pos, NULL, NULL, NULL, 
			&rlen) == ERROR_SUCCESS) {
      char *res;
      res = new char[rlen + 1];
      if (RegQueryValueEx(key, entry XFORM_OK_PLUS sep_pos, NULL, NULL, 
			  (unsigned char *)res, &rlen) == ERROR_SUCCESS) {
	res[rlen + 1] = 0;
	*value_get = res;
	success = 1;
      }
    }
  } else {
    if (RegSetValueEx(key, entry XFORM_OK_PLUS sep_pos, 0,
		      REG_SZ, (BYTE *)value_set, strlen(value_set))
	== ERROR_SUCCESS) {
      success = 1;
    } else
      success = 0;
  }

  if (key_needs_close)
    RegCloseKey(key);
    
  return success;
}

Bool wxWriteResource(const char *section, const char *entry, char *value, const char *file)
{
  HKEY key;

#define TRY_HKEY(name) if (!strcmp(section, #name)) key = name;

  TRY_HKEY(HKEY_CLASSES_ROOT);
  TRY_HKEY(HKEY_CURRENT_CONFIG);
  TRY_HKEY(HKEY_CURRENT_USER);
  TRY_HKEY(HKEY_LOCAL_MACHINE);
  TRY_HKEY(HKEY_USERS);

#undef TRY_HKEY

  if (key) {
    return wxDoResource(key, entry, NULL, value);
  } else {
    if (file) {
      char *naya;
      int len, i;

      len = strlen(file);
      
      naya = new char[len + 1];
      
      memcpy(naya, file, len + 1);
      
      for (i = 0; i < len; i++) {
	naya[i] = tolower(naya[i]);
	if (naya[i] == '/')
	  naya[i] = '\\';
      }
      while (len && naya[len - 1] == ' ') {
	--len;
	naya[len] = 0;
      }
      
      file = naya;
    } else {
      file = wxUserResourceFile;
    } 
    
    return WritePrivateProfileString((LPCSTR)section, (LPCSTR)entry, (LPCSTR)value, (LPCSTR)file);
  }
}

Bool wxWriteResource(const char *section, const char *entry, float value, const char *file)
{
  char buf[50];
  sprintf(buf, "%.4f", value);
  return wxWriteResource(section, entry, (char *)buf, file);
}

Bool wxWriteResource(const char *section, const char *entry, long value, const char *file)
{
  char buf[50];
  sprintf(buf, "%ld", value);
  return wxWriteResource(section, entry, (char *)buf, file);
}

Bool wxWriteResource(const char *section, const char *entry, int value, const char *file)
{
  char buf[50];
  sprintf(buf, "%d", value);
  return wxWriteResource(section, entry, (char *)buf, file);
}

Bool wxGetResource(const char *section, const char *entry, char **value, const char *file)
{
  HKEY key;

#define TRY_HKEY(name) if (!strcmp(section, #name)) key = name;

  TRY_HKEY(HKEY_CLASSES_ROOT);
  TRY_HKEY(HKEY_CURRENT_CONFIG);
  TRY_HKEY(HKEY_CURRENT_USER);
  TRY_HKEY(HKEY_LOCAL_MACHINE);
  TRY_HKEY(HKEY_USERS);

#undef TRY_HKEY

  if (key) {
    return wxDoResource(key, entry, value, NULL);
  } else {
    static const char defunkt[] = "$$default";
    int no_file = !file;
    char *v;
    
    if (!file)
      file = wxUserResourceFile;
    
    wxBuffer[0] = 0;
    
    if (file) {
      int n;
      n = GetPrivateProfileString((LPCSTR)section, (LPCSTR)entry, (LPCSTR)defunkt,
				  (LPSTR)wxBuffer, 1000, (LPCSTR)file);
      if (n == 0 || strcmp(wxBuffer, defunkt) == 0)
	return FALSE;
    }
    
    v = copystring(wxBuffer);
    *value = v;
    return TRUE;
  }
}

Bool wxGetResource(const char *section, const char *entry, float *value, const char *file)
{
  char *s = NULL;
  Bool succ;
  succ = wxGetResource(section, entry, (char **)&s, file);
  if (succ)
  {
    float v;
    v = (float)strtod(s, NULL);
    *value = v;
    return TRUE;
  }
  else return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, long *value, const char *file)
{
  char *s = NULL;
  Bool succ;
  succ = wxGetResource(section, entry, (char **)&s, file);
  if (succ)
  {
    long v;
    v = strtol(s, NULL, 10);
    *value = v;
    return TRUE;
  }
  else return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, int *value, const char *file)
{
  char *s = NULL;
  Bool succ;
  succ = wxGetResource(section, entry, (char **)&s, file);
  if (succ)
  {
    int v;
    v = (int)strtol(s, NULL, 10);
    *value = v;
    return TRUE;
  }
  else return FALSE;
}

extern void wxResetCurrentCursor(void);

extern HCURSOR wxMSWSetCursor(HCURSOR c);

// Set the cursor to the busy cursor for all windows
void wxBeginBusyCursor(wxCursor *cursor)
{
  int wxBusyCursorCount;
  wxBusyCursorCount = wxGetBusyState();
  wxBusyCursorCount++;
  wxSetBusyState(wxBusyCursorCount);

  if (wxBusyCursorCount == 1)
    wxMSWSetCursor(cursor->ms_cursor);
  else
    (void)wxMSWSetCursor(cursor->ms_cursor);
}

// Restore cursor to normal
void wxEndBusyCursor(void)
{
  int wxBusyCursorCount;
  wxBusyCursorCount = wxGetBusyState();
  if (wxBusyCursorCount == 0)
    return;
  --wxBusyCursorCount;
  wxSetBusyState(wxBusyCursorCount);
    
  if (wxBusyCursorCount == 0) {
    wxResetCurrentCursor();
  }
}

// TRUE if we're between the above two calls
Bool wxIsBusy(void)
{
  return (wxGetBusyState() > 0);
}    

static int hidden = 0, hide_not_ok = 0;

/* The hide-cursor functions are used in wx_pdf.cxx from
   a non-main thread, so make sure there are no GC annotations. */
#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

void wxHideCursor(void)
{
  if (!hidden && !hide_not_ok) {
    hidden = 1;
    ShowCursor(FALSE);
  }
}

void wxUnhideCursor(void)
{
  if (hidden) {
    hidden = 0;
    ShowCursor(TRUE);
  }
}

void wxCannotHideCursor(void)
{
  hide_not_ok++;
  wxUnhideCursor();
}

void wxCanHideCursor(void)
{
  --hide_not_ok;
}
   
#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

/********************************************************************************/

#define WC_BUFFER_SIZE 1024
static wchar_t wc_buffer[WC_BUFFER_SIZE];

int wx_wstrlen(wchar_t *ws)
{
  int l;
  for (l =0; ws[l]; l++) { }
  return l;
}

wchar_t *wx_convert_to_wchar(char *s, int do_copy)
{
  long len, l;
  wchar_t *ws;

  if (!s) return NULL;

  l = strlen(s);
  len = scheme_utf8_decode((unsigned char *)s, 0, l,
			   NULL, 0, -1,
			   NULL, 1/*UTF-16*/, 1);
  if (!do_copy && (len < (WC_BUFFER_SIZE-1)))
    ws = wc_buffer;
  else
    ws = (wchar_t *)GC_malloc_atomic(sizeof(wchar_t) * (len + 1));
  scheme_utf8_decode((unsigned char *)s, 0, l,
		     (unsigned int *)ws, 0, -1,
		     NULL, 1/*UTF-16*/, 1);
  ws[len] = 0;
  return ws;
}

extern "C" {
  wchar_t *_wx_convert_to_wchar(char *s, int do_copy)
  {
    return wx_convert_to_wchar(s, do_copy);
  }
}

char *wx_convert_from_wchar(wchar_t *ws)
{
  long len, l;
  char *s;

  if (!ws) return NULL;

  l = wx_wstrlen(ws);
  len = scheme_utf8_encode((unsigned int *)ws, 0, l,
			   NULL, 0,
			   1/*UTF-16*/);
  s = new char[len + 1];
  scheme_utf8_encode((unsigned int *)ws, 0, l,
		     (unsigned char *)s, 0,
		     1/*UTF-16*/);
  s[len] = 0;
  return s;
}
