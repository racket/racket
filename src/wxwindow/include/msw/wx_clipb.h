/*
 * File:	wx_clipb.h
 * Purpose:	Clipboard functionality
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_clipbh
#define wx_clipbh

#include "common.h"
#include "wx_setup.h"
#include "wx_list.h"

Bool wxOpenClipboard(void);
Bool wxCloseClipboard(void);
Bool wxEmptyClipboard(void);
Bool wxIsClipboardFormatAvailable(int dataFormat);
Bool wxSetClipboardData(int dataFormat, wxObject *obj, int width = 0, int height = 0);
wxObject *wxGetClipboardData(int dataFormat, long *len = NULL);
int  wxEnumClipboardFormats(int dataFormat);
int  wxRegisterClipboardFormat(char *formatName);
Bool wxGetClipboardFormatName(int dataFormat, char *formatName, int maxCount);

/* The following is Matthew Flatt's implementation of the MSW
 * side of generic clipboard functionality.
 */

/* A clipboard client holds data belonging to the clipboard.
   For plain text, a client is not necessary. */
class wxClipboardClient : public wxObject
{
 public:
  /* This list should be filled in with strings indicating the formats
     this client can provide. Almost all clients will provide "TEXT".
     Format names should be 4 characters long, so things will work
     out on the Macintosh */
  wxStringList *formats;

  void *context; /* eventspace */

  wxClipboardClient();

  /* This method is called when the client is losing the selection. */
  virtual void BeingReplaced(void) = 0;

  /* This method is called when someone wants the data this client is
     supplying to the clipboard. "format" is a string indicating the
     format of the data - one of the strings from the "formats"
     list. "*size" should be filled with the size of the resulting
     data. In the case of text, "*size" does not count the
     NULL terminator. */
  virtual char *GetData(char *format, long *size) = 0;
};

/* ONE instance of this class: */
class wxClipboard : public wxObject
{
 public:
  wxClipboardClient *clipOwner;
  char *cbString, *sentString, *receivedString;
  void *receivedTargets;
  long receivedLength;

  wxClipboard();
  ~wxClipboard();

  /* Set the clipboard data owner. "time" comes from the event record. */
  void SetClipboardClient(wxClipboardClient *, long time);

  /* Set the clipboard string; does not require a client. */
  void SetClipboardString(char *, long time);

  /* Get data from the clipboard in the format "TEXT". */
  char *GetClipboardString(long time);

  /* Set the clipboard bitmap; does not require a client, or
     keep the bitmap. */
  void SetClipboardBitmap(wxBitmap *, long time);

  /* Get bitmap data from the clipboard. */
  wxBitmap *GetClipboardBitmap(long time);

  /* Get data from the clipboard */
  char *GetClipboardData(char *format, long *length, long time);

  /* Get the clipboard client directly. Will be NULL if clipboard data
     is a string, or if some other application owns the clipboard. 
     This can be useful for shortcutting data translation, if the
     clipboard user can check for a specific client. (This is used
     by the wxMediaEdit class.) */
  wxClipboardClient *GetClipboardClient(void);
};

/* Initialize wxTheClipboard. Can be called repeatedly */
void wxInitClipboard(void);

/* The clipboard */
extern wxClipboard *wxTheClipboard;

extern char *wxsGetDataInEventspace(wxClipboardClient *clipOwner, char *format, long *length);
extern void MrEdQueueBeingReplaced(wxClipboardClient *clipOwner);

#endif // wx_clipbh
