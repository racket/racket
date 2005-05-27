/*
 * File:	wx_clipb.h
 * Purpose:	Clipboard functionality
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wx_clipbh
#define wx_clipbh

#include "common.h"
#include "wx_setup.h"
#include "wx_gdi.h"

#if USE_CLIPBOARD

#include "wx_obj.h"
#include "wx_list.h"

#ifndef IN_CPROTO
Bool wxOpenClipboard(void);
Bool wxCloseClipboard(void);
Bool wxEmptyClipboard(void);
Bool wxIsClipboardFormatAvailable(int dataFormat);
Bool wxSetClipboardData(int dataFormat, wxObject *obj, int width = 0, int height = 0);
wxObject *wxGetClipboardData(int dataFormat, long *size = NULL);
int  wxEnumClipboardFormats(int dataFormat);
int  wxRegisterClipboardFormat(char *formatName);
Bool wxGetClipboardFormatName(int dataFormat, char *formatName, int maxCount);

/* A clipboard client holds data belonging to the clipboard.
   For plain text, a client is not necessary. */
class wxClipboardClient : public wxObject
{
 public:
  wxStringList *formats;
  /* This list should be filled in with strings indicating the formats
     this client can provide. Almost all clients will provide "TEXT".
     Format names should be 4 characters long, so things will work
     out on the Macintosh */

  void *context; /* eventspace */

  wxClipboardClient();

  virtual void BeingReplaced(void) = 0;
  /* This method is called when the client is losing the selection. */
  virtual char *GetData(char *format, long *size) = 0;
  /* This method is called when someone wants the data this client is
     supplying to the clipboard. "format" is a string indicating the
     format of the data - one of the strings from the "formats"
     list. "*size" should be filled with the size of the resulting
     data. In the case of text, "*size" does not count the
     NULL terminator. */
};

/* ONE instance of this class: */
class wxClipboard : public wxObject
{
 public:
  wxClipboardClient *clipOwner;
  char *cbString, *sentString, *receivedString;
  void *receivedTargets;
  long receivedLength;
#ifdef wx_xview
  long sel_owner;
#endif

  wxClipboard();
  ~wxClipboard();

  void SetClipboardClient(wxClipboardClient *, long time);
  /* Set the clipboard data owner. "time" comes from the event record. */
  void SetClipboardString(char *, long time);
  /* Set the clipboard string; does not require a client. */

  void SetClipboardBitmap(wxBitmap *, long time);
  /* Set the clipboard bitmap; does not require a client. */
  char *GetClipboardString(long time);
  /* Get data from the clipboard in the format "TEXT". */
  wxBitmap *GetClipboardBitmap(long time);
  /* Get picture from the clipboard. */
  char *GetClipboardData(char *format, long *length, long time);
  /* Get data from the clipboard */
  wxClipboardClient *GetClipboardClient(void);
  /* Get the clipboard client directly. Will be NULL if clipboard data
     is a string, or if some other application owns the clipboard. 
     This can be useful for shortcutting data translation, if the
     clipboard user can check for a specific client. (This is used
     by the wxMediaEdit class.) */
};

void wxInitClipboard(void);
/* Initialize wxTheClipboard. Can be called repeatedly */

extern wxClipboard *wxTheClipboard;
/* The clipboard */

char *wxsGetDataInEventspace(wxClipboardClient *clipOwner, char *format, long *length);

#endif // IN_CPROTO
#endif // USE_CLIPBOARD
#endif // wx_clipbh
