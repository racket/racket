/*
 * File:	wx_cmdlg.cc
 * Purpose:	Common dialogs: MS Windows implementation
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1995, Julian Smart
 *
 * Renovated by Matthew for MrEd, 1995-2000
 * Corrected and improved by Noel Welsh.
 */

#include "wx.h"
#include "wx_pdf.h"
#include <objbase.h>
#include <objidl.h>
#include <shlobj.h>

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

#define FILEBUF_SIZE 4096


/*
 * Utility functions
 */


/**
 * Gets the directory portion of an multiple file OPENFILENAME file buffer.  
 * Returns length of said directory portion
 */
static int GetDirectoryPart(wchar_t* fileBuffer, wchar_t* directory) 
{
  int length;
  
  /* The only confusion is that the directory portion may or may not
     end with a \.  If the file is, e.g., c:\file.dat, it will.  If
     the file is c:\somedir\file.dat, it won't so we must append the \ */

  length = wx_wstrlen(fileBuffer);
  memcpy(directory, fileBuffer, (length + 1) * sizeof(wchar_t));

  if(! (directory[length - 1] == '\\') ) {
    directory[length] = '\\';
    directory[length + 1] = '\0';
    length = length + 1;
  }

  return length;
}


/**
 * Returns the length of the buffer than will be need for the
 * transformed multiple file OPENFILENAME file buffer 
 */
static int GetTotalLength(wchar_t* fileBuffer, int directoryLength)
{
  wchar_t* currentFile;
  int currentLength = 0;
  int totalLength = 0;
  
  currentFile = fileBuffer;
  
  /* Skip the directory part */      
  currentLength = wx_wstrlen(currentFile);
  currentFile = currentFile XFORM_OK_PLUS (currentLength + 1);

  while (*currentFile) {
    currentLength = wx_wstrlen(currentFile);
    totalLength += directoryLength + currentLength + 8;
    currentFile = currentFile XFORM_OK_PLUS (currentLength + 1);
  }

  return totalLength;
}

extern "C" {
 int scheme_utf8_encode(const unsigned int *us, int start, int end, 
			unsigned char *s, int dstart,
			char utf16);
};

static int len_in_bytes(const wchar_t *s, int start, int len)
{
  return scheme_utf8_encode((const unsigned int *)s,
			    start, start + len,
			    NULL, 0,
			    1);
}

/**
 * If a dialog box allows multiple files to be selected, and the user
 * selects multiple files, the lpstrFile buffer contains the current
 * directory followed by the filenames of the selected files. For
 * Explorer-style dialog boxes, the directory and filename strings are
 * NULL separated, with an extra NULL character after the last
 * filename.
 *
 * The format we want is a simple string containing of the form:
 * "<length> <full-file-name>...", where
 *
 * <length> is the length of filename
 * <full-file-name> is concatenation of that path and the filename
 *
 * This function does the conversion from the Windows format to the
 * format we want
 */
static char* ExtractMultipleFileNames(OPENFILENAMEW* of, wchar_t* wFileBuffer) 
{
  /* Check for multiple file names, indicated by a null character
     preceding nFileOffset */
  if (of->nFileOffset && !wFileBuffer[of->nFileOffset - 1]) {
    wchar_t directory[FILEBUF_SIZE];
    wchar_t *result;
    char num_buf[8];
    int directoryLength, directoryByteLength;
    int currentFile;
    int currentFileLength = 0, currentFileByteLength;
    int currentTotal = 0;
    int totalLength = 0;
        
    directoryLength = GetDirectoryPart(wFileBuffer, directory);
    totalLength = GetTotalLength(wFileBuffer, directoryLength);
    
    directoryByteLength = len_in_bytes(wFileBuffer, 0, directoryLength);

    result = (wchar_t *)GC_malloc_atomic(sizeof(wchar_t) * totalLength);

    /* Skip the directory part */
    currentFileLength = wx_wstrlen(wFileBuffer);
    currentFile = currentFileLength + 1;

    /* Copy the concatentation of the directory and file */
    while (wFileBuffer[currentFile]) {
      currentFileLength = wx_wstrlen(wFileBuffer XFORM_OK_PLUS currentFile);
      currentFileByteLength = len_in_bytes(wFileBuffer, currentFile, currentFileLength);

      sprintf(num_buf, "%5d ", currentFileByteLength + directoryByteLength);
      {
	int i;
	for (i = 0; i < 6; i++) {
	  result[currentTotal + i] = num_buf[i];
	}
      }
      memcpy(result + currentTotal + 6, directory, directoryLength * sizeof(wchar_t));
      memcpy(result + currentTotal + 6 + directoryLength,
	     wFileBuffer + currentFile, currentFileLength * sizeof(wchar_t));

      currentFile += currentFileLength + 1;
      currentTotal += currentFileLength + directoryLength + 6;
    }
    result[currentTotal] = 0;

    return wxNARROW_STRING(result);
  }
  /* Only a single file name so we can simply copy it */
  else {
    int length;
    char* result;
    char *fileBuffer;

    fileBuffer = wxNARROW_STRING(wFileBuffer);
    length = strlen(fileBuffer);

    result = new WXGC_ATOMIC char[length + 7];
    sprintf(result, "%5d ", length);
    memcpy(result + 6, fileBuffer, length + 1);

	return result;
  }
}



/*
 * Common dialogs
 *
 */
 
// Pop up a message box
int wxMessageBox(char *message, char *caption, long type,
                 wxWindow *parent, int x, int y)
{
  return wxbMessageBox(message, caption, type, parent, x, y);
}


#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static BOOL DoGetSaveFileName(OPENFILENAMEW *of, HWND parent)
{
  if (!of->hwndOwner)
    of->hwndOwner = parent;
  return GetSaveFileNameW(of);
}

static BOOL DoGetOpenFileName(OPENFILENAMEW *of, HWND parent)
{
  if (!of->hwndOwner)
    of->hwndOwner = parent;
  return GetOpenFileNameW(of);
}

static BOOL DoGetDir(BROWSEINFOW *b, HWND parent)
{
  ITEMIDLIST *r;

  if (!b->hwndOwner)
    b->hwndOwner = parent;

  CoInitialize(NULL);

  r = SHBrowseForFolderW(b);

  if (r) {
    IMalloc *mi;
    int ok;

    ok = SHGetPathFromIDListW(r, b->pszDisplayName);

    SHGetMalloc(&mi);
    mi->Free(r);
    mi->Release();
    return ok;
  } else
    return 0;
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

static int set_init_dir;
extern void MrEdSyncCurrentDir(void);

char *wxFileSelector(char *message,
                     char *default_path, char *default_filename, 
                     char *default_extension, char *wildcard, int flags,
                     wxWindow *parent, int x, int y)
{
  wxWnd *wnd = NULL;
  HWND hwnd = NULL;
  wchar_t *file_buffer;
  wchar_t *filter_buffer;
  wchar_t *def_path, *def_ext, *msg;
  OPENFILENAMEW *of;
  long msw_flags = 0;
  Bool success;

  if (x < 0) x = wxDIALOG_DEFAULT_X;
  if (y < 0) y = wxDIALOG_DEFAULT_Y;

  if (parent) {
    wnd = (wxWnd *)parent->handle;
    hwnd = wnd->handle;
  }

  if (!message)
    message = "";

  if (flags & wxGETDIR) {
    BROWSEINFOW *b;
    wchar_t *result, *_result;
    int ok;

    _result = (wchar_t *)malloc(sizeof(wchar_t) * (MAX_PATH + 1));
    
    b = (BROWSEINFOW *)malloc(sizeof(BROWSEINFOW));
    memset(b, 0, sizeof(BROWSEINFOW));

#ifndef BIF_NEWDIALOGSTYLE
# define BIF_NEWDIALOGSTYLE	0x0040
#endif

    b->pidlRoot = NULL;
    b->pszDisplayName = _result;
    {
      int len;
      wchar_t *ws;
      ws = wxWIDE_STRING_COPY(message);
      len = wx_wstrlen(ws);
      msg = (wchar_t *)malloc(sizeof(wchar_t) * (len + 1));
      memcpy(msg, ws, sizeof(wchar_t) * (len + 1));
      b->lpszTitle = msg;
    }
    b->ulFlags = (BIF_NEWDIALOGSTYLE | BIF_RETURNONLYFSDIRS);

    ok = wxPrimitiveDialog((wxPDF)DoGetDir, b, 1);
    free(b);

    if (ok) {
      result = (wchar_t *)GC_malloc_atomic(sizeof(wchar_t) * (MAX_PATH + 1));
      memcpy(result, _result, sizeof(wchar_t) * (MAX_PATH + 1));
    } else
      result = NULL;

    free(_result);
    free(msg);

    return wxNARROW_STRING(result);
  }

  file_buffer = (wchar_t *)malloc(sizeof(wchar_t *) * FILEBUF_SIZE);

  if (default_filename) {
    wcsncpy(file_buffer, wxWIDE_STRING(default_filename), FILEBUF_SIZE);
    file_buffer[FILEBUF_SIZE-1] = 0;
  } else 
    file_buffer[0] = 0;

  if (!wildcard)
    wildcard = "*.*";
  else if (strlen(wildcard) > 50) {
    char *s;
    s = new WXGC_ATOMIC char[51];
    memcpy(s, wildcard, 50);
    s[50] = 0;
    wildcard = s;
  }

  /* In wxFileSelector you can put, instead of a single wild_card, pairs of
     strings separated by '|'. The first string is a description, and the
     second is the wild card. You can put any number of pairs.
     
     eg.  "description1 (*.ex1)|*.ex1|description2 (*.ex2)|*.ex2"
  */
  
  if (wildcard) {
    int i, len;
    wchar_t *ww;

    filter_buffer = (wchar_t *)malloc(200 * sizeof(wchar_t));

    ww = wxWIDE_STRING(wildcard);
    len = wx_wstrlen(ww);
  
    if (!strchr(wildcard, '|')) {
      memcpy(filter_buffer, L"Files (", sizeof(wchar_t) * 7);
      memcpy(filter_buffer + 7, ww, sizeof(wchar_t) * len);
      memcpy(filter_buffer + 7 + len, L")|", sizeof(wchar_t) * 2);
      memcpy(filter_buffer + 9 + len, ww, sizeof(wchar_t) * len);
      filter_buffer[9 + (2*len)] = 0;
    } else {
      memcpy(filter_buffer, ww, sizeof(wchar_t) * len);
      filter_buffer[len] = 0;
    }

    len = wx_wstrlen(filter_buffer);
	 
    for (i = 0; i < len; i++) {
      if (filter_buffer[i] == '|')
	filter_buffer[i] = 0;
    }

    /* Extra terminator: */
    filter_buffer[len+1] = 0;
  } else
    filter_buffer = NULL;

  if (!set_init_dir) {
    set_init_dir = 1;
    MrEdSyncCurrentDir();
  }

  of = (OPENFILENAMEW *)malloc(sizeof(OPENFILENAMEW));

  memset(of, 0, sizeof(OPENFILENAMEW));

  of->lStructSize = sizeof(OPENFILENAMEW);
  of->hwndOwner = hwnd;

  if (default_path) {
    int len, alen;
    wchar_t *dp;
    dp = wxWIDE_STRING_COPY(default_path);
    len = wx_wstrlen(dp);
    alen = min(MAX_PATH, len);
    def_path = (wchar_t *)malloc((alen + 1) * sizeof(wchar_t));
    memcpy(def_path, dp, (len + 1) * sizeof(wchar_t));
    def_path[alen] = 0;

    // Picky, picky. Need backslashes.
    {
      int i;
      for (i = 0; def_path[i]; i++) {
	if (def_path[i] == '/')
	  def_path[i] = '\\';
      }
    }
  } else
    def_path = NULL;

  if (default_extension) {
    int len, fl;
    wchar_t *de;
    de = wxWIDE_STRING_COPY(default_extension);
    len = wx_wstrlen(de);
    fl = min(50, len);
    def_ext = (wchar_t *)malloc(sizeof(wchar_t) * (fl + 1));
    memcpy(def_ext, de, fl * sizeof(wchar_t));
    def_ext[fl] = 0;
  } else
    def_ext = NULL;
      
  if (wildcard) {
    of->lpstrFilter = (LPWSTR)filter_buffer;
    of->nFilterIndex = 1L;
  } else {
    of->lpstrFilter = NULL;
    of->nFilterIndex = 0L;
  }
  of->lpstrCustomFilter = NULL;
  of->nMaxCustFilter = 0L;
  of->lpstrFile = file_buffer;
  of->nMaxFile = FILEBUF_SIZE;
  of->lpstrFileTitle = NULL;
  of->nMaxFileTitle = 0;
  of->lpstrInitialDir = def_path;
  {
    int len;
    wchar_t *ws;
    ws = wxWIDE_STRING_COPY(message);
    len = wx_wstrlen(ws);
    msg = (wchar_t *)malloc(sizeof(wchar_t) * (len + 1));
    memcpy(msg, ws, sizeof(wchar_t) * (len + 1));
    of->lpstrTitle = msg;
  }
  of->nFileOffset = 0;
  of->nFileExtension = 0;
  of->lpstrDefExt = def_ext;

  msw_flags = OFN_HIDEREADONLY;
  if (flags & wxSAVE)
    msw_flags |= OFN_OVERWRITEPROMPT;
  if (flags & wxMULTIOPEN)
    msw_flags |= OFN_ALLOWMULTISELECT | OFN_EXPLORER;
  if (flags & wxHIDE_READONLY)
    msw_flags |= OFN_HIDEREADONLY;
  if (default_path)
    msw_flags |= OFN_NOCHANGEDIR;
  of->Flags = msw_flags;

  if (flags & wxSAVE)
    success = wxPrimitiveDialog((wxPDF)DoGetSaveFileName, of, 1);
  else
    success = wxPrimitiveDialog((wxPDF)DoGetOpenFileName, of, 1);

  if (success && !*file_buffer)
    success = 0;

  if (def_path) free(def_path);
  if (def_ext) free(def_ext);
  if (filter_buffer) free(filter_buffer);
  free(msg);

  {
    char *s;
    if (success && (flags & wxMULTIOPEN)) {
      s = ExtractMultipleFileNames(of, file_buffer);
    } else if (success) {
      s = wxNARROW_STRING(file_buffer);
    } else
      s = NULL;
    
    free(of);
    free(file_buffer);

    return s;
  }
}
