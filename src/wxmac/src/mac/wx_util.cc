#include "wx.h"
#include "wx_utils.h"
#include "wx_list.h"
#include "wx_main.h"
#include <stdarg.h>
#include <ctype.h>
#ifdef OS_X
# include <sys/types.h>
# include <unistd.h>
#endif
#ifndef WX_CARBON
# include <Strings.h>
# include <Gestalt.h>
# include <Files.h>
# include <Sound.h>
# include <Folders.h>
# include <PPCToolbox.h>
#endif
#ifndef OS_X
extern "C" long atol(char *);
extern "C" int atoi(char *);
#endif

#ifndef OS_X
extern "C" {
#endif
  extern char *scheme_mac_spec_to_path(FSSpec *spec);
#ifndef OS_X
}
#endif

#ifndef OS_X
extern "C" {
  extern int scheme_file_exists(char *filename);
}
#define stat(name, ignored) !scheme_file_exists(name)
#else
# include <sys/stat.h>
#endif

// Get a temporary filename, opening and closing the file.
char *wxGetTempFileName (const char *prefix, char *dest)
{
  static char *temp_folder;
  static int temp_len;
  static short last_temp = 0;	// cache last to speed things a bit
  // At most 1000 temp files to a process! We use a ring count.
  char *buf;

  if (!temp_folder) {
    FSSpec spec;
    SInt16 vRefNum;
    SInt32 dirID;
    const Str255 fileName = "\p";
    
    if (FindFolder(kOnSystemDisk, 'temp', kCreateFolder, &vRefNum, &dirID) == noErr) {
      wxREGGLOB(temp_folder);
      FSMakeFSSpec(vRefNum,dirID,fileName,&spec);
      temp_folder = scheme_mac_spec_to_path(&spec);
    }
    else
      temp_folder = "";
    temp_len = strlen(temp_folder);
  }
  
  if (!prefix)
    prefix = "";
  else {
    int i;
    for (i = 0; prefix[i]; i++) {
      if (prefix[i] == ':') {
        prefix = "";
        break;
      }
    }
    if (i > 15)
      prefix = "";
  }
  
  buf = new char[temp_len + strlen(prefix) + 20];

  for (short suffix = last_temp + 1; suffix != last_temp; ++suffix %= 1000) {
#ifdef OS_X
    struct stat stbuf;
#endif
    sprintf (buf, "%s_%s%d", temp_folder, prefix, (int) suffix);
    if (stat ((char *)buf, &stbuf) != 0) {
      // Touch the file to create it (reserve name)
      FILE *fd;
      fd = fopen (buf, "w");
      if (fd) {
	fclose (fd);
	last_temp = suffix;
	if (dest)
	  strcpy(dest, buf);
	else
	  dest = copystring(buf);
	return dest;
      }
    }
  }
  if (dest) dest[0] = 0;
  return dest;
}

#ifndef OS_X
extern "C" int scheme_mac_path_to_spec(const char *filename, FSSpec *spec);
#endif

void wxRemoveFile(char *filename)
{
#if OS_X
  unlink(filename);
#else
  FSSpec sp;

  if (scheme_mac_path_to_spec(filename, &sp)) {
    FSpDelete(&sp);
  }
#endif
}

// Get free memory in bytes, or -1 if cannot determine amount (e.g. on UNIX)
long 
wxGetFreeMemory (void)	// CJC FIXME this could be Macintized.
{
  return -1;
}

// Sleep for nSecs seconds.
// XView implementation according to the Heller manual
void 
wxSleep (int nSecs)
{
  // sleep (nSecs);
}

/****************************************/

extern "C" {
  int scheme_utf8_decode_all(const unsigned char *s, int len, unsigned int *us, 
			     int permissive);
  int scheme_utf8_encode_all(const unsigned int *us, int len, unsigned char *s);
}

CFStringRef wxCFString(const char *s)
{
  int l, ul, i;
  unsigned int *us;

  /* Look for a character that is beyond the official Unicode
     range, which ends at 0x10FFFDL. */
  l = strlen(s);
  us = new WXGC_ATOMIC unsigned[l + 1];
  ul = scheme_utf8_decode_all((unsigned char *)s, l, us, 0);
  for (i = 0; i < ul; i++) {
    if (us[i] > 0x10FFFD) {
      us[i] = '?';
      s = NULL;
    }
  }

  if (!s) {
    /* Found a too-large char. Convert back to utf-8.
       (The result will be smaller than the old l.) */
    s = new WXGC_ATOMIC char[l + 1];
    l = scheme_utf8_encode_all(us, ul, (unsigned char *)s);
    ((char *)s)[l] = 0;
  }

  return CFStringCreateWithCString(NULL, s, kCFStringEncodingUTF8);
}

/****************************************/

// Old cursor
extern int wxGetBusyState();
extern void wxSetBusyState(int);

// Set the cursor to the busy cursor for all windows
void 
wxBeginBusyCursor (wxCursor * cursor)
{
  int s;
  s = wxGetBusyState();
  wxSetBusyState(s + 1);

  if (!s) {
    wxChildList *tlw;
    wxChildNode *node;
    wxFrame *f;

    tlw = wxTopLevelWindows(NULL);
    for (node = tlw->First(); node; node = node->Next()) {
      f = (wxFrame *)node->Data();
      f->cBusyCursor = 1;
    }

    wxTheApp->AdjustCursor();
  }
}

// Restore cursor to normal
void 
wxEndBusyCursor (void)
{
  int s;

  s = wxGetBusyState();
  wxSetBusyState(s - 1);

  if (s == 1) {
    wxChildList *tlw;
    wxChildNode *node;
    wxFrame *f;

    tlw = wxTopLevelWindows(NULL);
    for (node = tlw->First(); node; node = node->Next()) {
      f = (wxFrame *)node->Data();
      f->cBusyCursor = 0;
    }
    
    wxTheApp->AdjustCursor();
  }
}

// TRUE if we're between the above two calls
Bool 
wxIsBusy (void)
{
  return wxGetBusyState() > 0;
}

static int hidden = 0;

void wxHideCursor(void)
{
  if (!hidden) {
    hidden = 1;
    HideCursor();
  }
}

void wxUnhideCursor(void)
{
  if (hidden) {
    hidden = 0;
    ShowCursor();
  }
}

Bool wxWriteResource(const char *section, const char *entry, char *value, const char *file)
{
  return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, float value, const char *file)
{
  return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, long value, const char *file)
{
  return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, int value, const char *file)
{
  return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, char **value, const char *file)
{
  return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, float *value, const char *file)
{
  return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, long *value, const char *file)
{
  return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, int *value, const char *file)
{
  return FALSE;
}

void wxBell()
{
  SysBeep(0);
}

int wxGetOsVersion(int *a, int *b)
{
  long systemVersion;
  
  ::Gestalt(gestaltSystemVersion,&systemVersion);

  *a = 10 * ((systemVersion >> 12) & 0xF) +
    ((systemVersion >>  8) & 0xF);
  *b =      ((systemVersion >>  4) & 0xF);
  // sub-version num (bottom four bits) is ignored
  
  return wxMACINTOSH;
}

void 
wxDebugMsg (const char *fmt...)
{
  va_list ap;
  char buffer[BUFSIZ];

  if (!wxTheApp->wantDebugOutput)
    return ;

  va_start (ap, fmt);

  vsprintf (buffer, fmt, ap);

  va_end (ap);
}


// Get hostname.
Bool wxGetHostName(char *buf, int maxSize)
{	
  Bool good = FALSE;

 if (maxSize>9) {
   strcpy(buf,"Macintosh");
   good = TRUE;
 }
 return good;
}

// Get user ID e.g. jacs
#ifdef OS_X
Bool wxGetUserId(char *buf, int maxSize)
{
  CFStringRef username;
  
  username = CSCopyUserName(true);
  return CFStringGetCString(username, buf, maxSize, kCFStringEncodingUTF8);
  
}
#else
Bool wxGetUserId(char *buf, int maxSize)
{
  return wxGetUserName(buf,maxSize);
}
#endif

// Get user name e.g. Julian Smart
#ifdef OS_X
Bool wxGetUserName(char *buf, int maxSize)
{
  CFStringRef username;
  
  username = CSCopyUserName(false);
  return CFStringGetCString(username, buf, maxSize, kCFStringEncodingUTF8);
}
#else    
Bool wxGetUserName(char *buf, int maxSize)
{	
  return FALSE;
}
#endif

#ifdef WX_CARBON

/* wxFSRefToPath converts an FSRef to a path.
 */
char *wxFSRefToPath(FSRef fsref)
{
  int longEnough = FALSE;
  OSErr err;
  int strLen = 256;
  char *str;
  
  str = new WXGC_ATOMIC char[strLen];
  
  while (! longEnough) {
    err = FSRefMakePath(&fsref,(unsigned char *)str,strLen);
    if (err == pathTooLongErr) {
      strLen *= 2;
      str = new WXGC_ATOMIC char[strLen];
    } else if (err == noErr) {
      longEnough = TRUE;
    } else {
      char b[20];
      sprintf(b + 1, "%d", err);
      b[0] = strlen(b + 1);
      DebugStr((unsigned char *)b);
      return NULL;
    }
  }
  
  return str;
}

#endif
