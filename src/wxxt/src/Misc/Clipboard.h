/* wx_clipb.h */
/* Header file for clipboard class. */

#ifndef __WX_CLIPBOARD__
#define __WX_CLIPBOARD__

class wxFrame;

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

  void *context; /* eventspace for the client */

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
  Bool is_sel, in_progress;
  void *saferef;
  double start_time;
  wxFrame *frame;

  wxClipboard();
  ~wxClipboard();

  /* Set the clipboard data owner. "time" comes from the event record. */
  void SetClipboardClient(wxClipboardClient *, long time);

  /* Set the clipboard string; does not require a client. */
  void SetClipboardString(char *, long time);

  /* Get data from the clipboard in the format "TEXT". */
  char *GetClipboardString(long time);


  void SetClipboardBitmap(wxBitmap *, long time);
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

/* The clipboard and selection: */
extern wxClipboard *wxTheClipboard;
extern wxClipboard *wxTheSelection;

char *wxsGetDataInEventspace(wxClipboardClient *clipOwner, char *format, long *length);

#endif

