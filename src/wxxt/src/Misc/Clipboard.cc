/*
 * File:        wx_clipb.cc
 * Purpose:     Clipboard implementation.
 * Author:      Julian Smart and Matthew Flatt
 * Created:     1993
 * Updated:	August 1994
 * Copyright:   (c) 2004-2010 PLT Scheme Inc.
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#pragma GCC diagnostic ignored "-Wwrite-strings"
#endif

#define Uses_wxApp
#define Uses_wxFrame
#define Uses_wxClipboard
#define Uses_XtIntrinsic
#define Uses_XtIntrinsicP
#define Uses_XLib
#include "wx.h"
#include <X11/Xatom.h>
#define  Uses_ShellWidget
#include "widgets.h"
#include "scheme.h"

wxClipboard *wxTheSelection;
wxClipboard *wxTheClipboard;

static Widget getClipWindow;
/* These two are used by mredx.cxx: */
Widget wx_clipWindow, wx_selWindow;

extern void MrEdQueueBeingReplaced(wxClipboardClient *clipOwner);

#ifdef MZ_PRECISE_GC
Atom ATOM(char *atom) 
{
  Widget wgt;
  wgt = wxAPP_TOPLEVEL;
  return XInternAtom(XtDisplay(wgt), atom, FALSE);
}
#else
# define ATOM(atom) XInternAtom(XtDisplay(wxAPP_TOPLEVEL), atom, FALSE)
#endif
#define VALUE_TYPE void*

Atom xa_utf8, xa_text, xa_targets, xa_clipboard;

static wxFrame *clipboard_frame, *selection_frame;
static wxFrame *get_clipboard_frame;

void wxInitClipboard(void)
{
  if (!wx_clipWindow) {
    /* Hack: We need a window for clipboard stuff */
    wxWindow_Xintern *fh;
    wxREGGLOB(clipboard_frame);
    wxREGGLOB(selection_frame);
    wxREGGLOB(get_clipboard_frame);
    clipboard_frame = new wxFrame(NULL, "clipboard", 0, 0, 10, 10);
    selection_frame = new wxFrame(NULL, "selection", 0, 0, 10, 10);
    get_clipboard_frame = new wxFrame(NULL, "get clipboard", 0, 0, 10, 10);

    fh = clipboard_frame->GetHandle();
    wx_clipWindow = fh->frame;
    XtRealizeWidget(wx_clipWindow);

    fh = selection_frame->GetHandle();
    wx_selWindow = fh->frame;
    XtRealizeWidget(wx_selWindow);

    fh = get_clipboard_frame->GetHandle();
    getClipWindow = fh->frame;
    XtRealizeWidget(getClipWindow);

    /* Initially not in any specific context. */
    clipboard_frame->context = NULL;
    selection_frame->context = NULL;
    /* Not in any specific context. */
    get_clipboard_frame->context = NULL;
  }

  if (!wxTheClipboard) {
    int cts;
    
    wxREGGLOB(wxTheClipboard);
    wxREGGLOB(wxTheSelection);

    wxTheSelection = new wxClipboard;
    wxTheSelection->is_sel = 1;
    wxTheSelection->frame = selection_frame;

    if (!wxGetBoolPreference("selectionAsClipboard", &cts))
      cts = 0;

    if (cts) {
      wxTheClipboard = wxTheSelection;
    } else {
      wxTheClipboard = new wxClipboard;
      wxTheClipboard->frame = clipboard_frame;      
    }
  }

  xa_utf8 = ATOM("UTF8_STRING");
  xa_text = ATOM("TEXT");
  xa_targets = ATOM("TARGETS");
  xa_clipboard = ATOM("CLIPBOARD");
}

Window wxAddClipboardWindowProperty(Atom prop)
{
  unsigned char data[1] = { 'm' };
  XChangeProperty(XtDisplay(wx_clipWindow), XtWindow(wx_clipWindow),
		  prop, prop, 8, PropModeReplace, data, 1);
  return XtWindow(wx_clipWindow);
}

static void AddClipboardFrame(wxClipboard *cb, int on)
{
  if (!on)
    cb->frame->context = NULL;
}

wxClipboardClient::wxClipboardClient()
{
  formats = new wxStringList();
}

wxClipboard::wxClipboard()
{
  clipOwner = NULL;
  cbString = NULL;

  {
    void *v;
    v = MALLOC_SAFEREF();
    saferef = v;
    SET_SAFEREF(saferef, this);
  }
}

wxClipboard::~wxClipboard()
{
}

static Boolean doConvertClipboard(wxClipboard *cb,
				  Widget WXUNUSED(w), Atom *WXUNUSED(selection), Atom *target,
				  Atom *type_return, XtPointer *value_return,
				  unsigned long *length_return,
				  int *format_return)
{
  Atom xa;
  char **formats = NULL;
  int i = 0, count, extra;

  if (*target == xa_targets) {
    if (cb->clipOwner) {
      count = cb->clipOwner->formats->Number();
      extra = (cb->clipOwner->formats->Member("TEXT")) ? 2 : 0;
      cb->receivedTargets = new WXGC_ATOMIC Atom[count + extra];
      formats = cb->clipOwner->formats->ListToArray(FALSE);
      for (i = 0; i < count; i++) {
	Atom atm;
	atm = ATOM(formats[i]);
	((Atom *)cb->receivedTargets)[i] = atm;
      }
      if (extra) {
	((Atom *)cb->receivedTargets)[count] = xa_utf8;
	((Atom *)cb->receivedTargets)[count+1] = XA_STRING;
      }
    } else {
      count = 3;
      cb->receivedTargets = new WXGC_ATOMIC Atom[3];
      ((Atom *)cb->receivedTargets)[0] = xa_utf8;
      ((Atom *)cb->receivedTargets)[1] = XA_STRING;
      ((Atom *)cb->receivedTargets)[2] = xa_text;
      extra = 0;
    }

    *value_return = (VALUE_TYPE)cb->receivedTargets;
    *type_return = XA_ATOM;
    if (sizeof(Atom) > 4) {
      *format_return = 32;
      *length_return = (count + extra) * (sizeof(Atom) / 4);
    } else {
      *format_return = 8 * sizeof(Atom);
      *length_return = count + extra;
    }

    cb->sentString = NULL;

    return TRUE;
  } 
  
  cb->receivedTargets = NULL;

  if (cb->clipOwner) {
    formats = cb->clipOwner->formats->ListToArray(FALSE);
    for (i = cb->clipOwner->formats->Number(); i--; ) {
      xa = ATOM(formats[i]);
      if (xa == *target)
	break;
      if (xa == xa_text && (*target == xa_utf8 || *target == XA_STRING))
	break;
    }
    if (i < 0)
      return FALSE;
  } else if (*target != xa_text && *target != xa_utf8 && *target != XA_STRING)
    return FALSE;

  *type_return = xa_utf8;
  *format_return = 8;
  if (cb->clipOwner) {
    long sz = 0;
    char *s;
    s = cb->clipOwner->GetData(formats[i], &sz);
    cb->sentString = s;
    *length_return = sz;
    *value_return = (VALUE_TYPE)cb->sentString;
  } else {
    *value_return = (VALUE_TYPE)cb->cbString;
    *length_return = strlen(cb->cbString);
  }

  return TRUE;
}

static Boolean wxConvertClipboard(Widget w, Atom *selection, Atom *target,
				  Atom *type_return, XtPointer *value_return,
				  unsigned long *length_return,
				  int *format_return)
{
  return doConvertClipboard(wxTheClipboard, w, selection, target, type_return, value_return,
			    length_return, format_return);
}

static Boolean wxConvertSelection(Widget w, Atom *selection, Atom *target,
				 Atom *type_return, XtPointer *value_return,
				 unsigned long *length_return,
				 int *format_return)
{
  return doConvertClipboard(wxTheSelection, w, selection, target, type_return, value_return,
			    length_return, format_return);
}

static void doSelectionDone(wxClipboard *cb, Widget WXUNUSED(w), Atom *WXUNUSED(selection), Atom *WXUNUSED(target))
{
  cb->sentString = NULL;
  cb->receivedTargets = NULL;
}

static void wxClipboardDone(Widget w, Atom *selection, Atom *target)
{
  doSelectionDone(wxTheClipboard, w, selection, target);
}

static void wxSelectionDone(Widget w, Atom *selection, Atom *target)
{
  doSelectionDone(wxTheSelection, w, selection, target);
}

static void wxStringClipboardDone(Widget WXUNUSED(w), Atom *WXUNUSED(selection), Atom *WXUNUSED(target))
{
  /* do nothing */
}

static void wxStringSelectionDone(Widget WXUNUSED(w), Atom *WXUNUSED(selection), Atom *WXUNUSED(target))
{
  /* do nothing */
}

static void doLoseClipboard(wxClipboard *cb, Widget WXUNUSED(w), Atom *WXUNUSED(selection))
{
  if (cb->clipOwner) {
    MrEdQueueBeingReplaced(cb->clipOwner);
    cb->clipOwner = NULL;
    AddClipboardFrame(cb, 0);
  }
  cb->cbString = NULL;
}

static void wxLoseClipboard(Widget w, Atom *selection)
{
  doLoseClipboard(wxTheClipboard, w, selection);
}

static void wxLoseSelection(Widget w, Atom *selection)
{
  doLoseClipboard(wxTheSelection, w, selection);
}

void wxClipboard::SetClipboardClient(wxClipboardClient *client, long time)
{
  Bool got_selection;

  if (clipOwner) {
    MrEdQueueBeingReplaced(clipOwner);
    clipOwner = NULL;
    AddClipboardFrame(this, 0);
  }
  cbString = NULL;
  
  /* Merely setting the context for a frame would not
     normally redirect events to a different eventspace.
     But there's a hack in mredx.cxx to help complete
     the redirection. */
  clipOwner = client;
  client->context = wxGetContextForFrame();
  frame->context = client->context;
  AddClipboardFrame(this, 1);

  if (is_sel) {
    got_selection = XtOwnSelection(wx_selWindow, XA_PRIMARY, time,
				   wxConvertSelection, wxLoseSelection, 
				   wxSelectionDone);
  } else {
    got_selection = XtOwnSelection(wx_clipWindow, xa_clipboard, time,
				   wxConvertClipboard, wxLoseClipboard, 
				   wxClipboardDone);
  }

  if (!got_selection) {
    MrEdQueueBeingReplaced(clipOwner);
    clipOwner = NULL;
    AddClipboardFrame(this, 0);
  }
}

wxClipboardClient *wxClipboard::GetClipboardClient()
{
  return clipOwner;
}

void wxClipboard::SetClipboardString(char *str, long time)
{
  Bool got_selection;

  if (clipOwner) {
    MrEdQueueBeingReplaced(clipOwner);
    clipOwner = NULL;
    AddClipboardFrame(this, 0);
  }

  cbString = str;

  if (is_sel) {
    got_selection = XtOwnSelection(wx_selWindow, XA_PRIMARY, time,
				   wxConvertSelection, wxLoseSelection, 
				   wxStringSelectionDone);
  } else {
    got_selection = XtOwnSelection(wx_clipWindow, xa_clipboard, time,
				   wxConvertClipboard, wxLoseClipboard, 
				   wxStringClipboardDone);
  }
  
  if (!got_selection) {
    cbString = NULL;
  }
}

void wxClipboard::SetClipboardBitmap(wxBitmap *bm, long time)
{
  if (clipOwner) {
    MrEdQueueBeingReplaced(clipOwner);
    AddClipboardFrame(this, 0);
    clipOwner = NULL;
  }

  cbString = NULL;

  /* Don't know how to put a bitmap into the clipboard. */
}

wxBitmap *wxClipboard::GetClipboardBitmap(long time)
{
  return NULL;
}

static void wxGetTargets(Widget WXUNUSED(w), XtPointer _cb, Atom *WXUNUSED(sel), Atom *WXUNUSED(type),
			 XtPointer value, unsigned long *len, int *WXUNUSED(format))
{
  wxClipboard *cb = (wxClipboard *)GET_SAFEREF(_cb);

  if (cb->in_progress < 0) {
    cb->in_progress = 0;
  } else {
    if (*len <= 0) {
      cb->receivedTargets = (void *)1; /* To break the waiting loop */
      cb->receivedLength = 0;
    } else {
      cb->receivedTargets = new WXGC_ATOMIC Atom[*len];
      memcpy(cb->receivedTargets, value, *len * sizeof(Atom));
      cb->receivedLength = *len;
    }
  }
}

static void wxGetSelection(Widget WXUNUSED(w), XtPointer _cb, Atom *WXUNUSED(sel), Atom *WXUNUSED(type),
			   XtPointer value, unsigned long *len, int *WXUNUSED(format))
{
  wxClipboard *cb = (wxClipboard *)GET_SAFEREF(_cb);

  if (cb->in_progress < 0) {
    cb->in_progress = 0;
  } else {
    cb->receivedString = new WXGC_ATOMIC char[(*len) + 1];
    memcpy(cb->receivedString, value, *len);
    cb->receivedString[*len] = 0;
    cb->receivedLength = *len;
  }
}

char *wxClipboard::GetClipboardString(long time)
{
  char *str;
  long length;

  str = GetClipboardData("TEXT", &length, time);
  if (!str)
    str = "";

  return str;
}

extern void wxBlockUntil(int (*)(void *), void *);
extern void wxBlockUntilTimeout(int (*)(void *), void *, float);
extern double scheme_get_inexact_milliseconds(void);

static int clip_timeout;

static int CheckNotInProgress(void *_cb)
{
  wxClipboard *cb = (wxClipboard *)GET_SAFEREF(_cb);
  return !cb->in_progress;
}

static int CheckReadyTarget(void *_cb)
{
  wxClipboard *cb = (wxClipboard *)GET_SAFEREF(_cb);
  double now;
  now = scheme_get_inexact_milliseconds();
  if (now > cb->start_time + clip_timeout)
    return 1;
  return !!cb->receivedTargets;
}

static int CheckReadyString(void *_cb)
{
  wxClipboard *cb = (wxClipboard *)GET_SAFEREF(_cb);
  double now;
  now = scheme_get_inexact_milliseconds();
  if (now > cb->start_time + clip_timeout)
    return 1;
  return !!cb->receivedString;
}

static void abandoned_clip(void *_cb)
{
  wxClipboard *cb = (wxClipboard *)GET_SAFEREF(_cb);

  if (cb->in_progress)
    cb->in_progress = -1;
}

char *wxClipboard::GetClipboardData(char *format, long *length, long time, int alt_sel)
{
  if (clipOwner && !alt_sel)  {
    if (clipOwner->formats->Member(format))
      return wxsGetDataInEventspace(clipOwner, format, length);
    else
      return NULL;
  } else if (cbString && !alt_sel) {
    if (!strcmp(format, "TEXT"))
      return copystring(cbString);
    else
      return NULL;
  } else {
    Atom xa;
    long i;

    if (!clip_timeout)
      clip_timeout = XtAppGetSelectionTimeout(wxAPP_CONTEXT) + 1;

    /* Need to make sure that only one thread is here at a time. */
    wxBlockUntil(CheckNotInProgress, saferef);
    
    in_progress = 1;

    receivedString = NULL;
    receivedTargets = NULL;

    XtGetSelectionValue(getClipWindow, alt_sel ? alt_sel : (is_sel ? XA_PRIMARY : xa_clipboard),
			xa_targets, wxGetTargets, (XtPointer)saferef, time);

    start_time = scheme_get_inexact_milliseconds();
    BEGIN_ESCAPEABLE(abandoned_clip, saferef);
    wxBlockUntilTimeout(CheckReadyTarget, saferef, clip_timeout);
    END_ESCAPEABLE();

    if (!receivedTargets) {
      /* Timeout */
      in_progress = 0;
      return NULL;
    }

    xa = ATOM(format);

    for (i = 0; i < receivedLength; i++) {
      if (((Atom *)receivedTargets)[i] == xa)
	break;
      else if ((((Atom *)receivedTargets)[i] == xa_utf8
		|| ((Atom *)receivedTargets)[i] == XA_STRING)
	       && xa == xa_text) {
	xa = ((Atom *)receivedTargets)[i];
	break;
      }
    }

    if (receivedLength)
      receivedTargets = NULL;

    if (i >= receivedLength) {
      in_progress = 0;
      return NULL;
    }

    XtGetSelectionValue(getClipWindow, alt_sel ? alt_sel : (is_sel ? XA_PRIMARY : xa_clipboard),
			xa, wxGetSelection, (XtPointer)saferef, 0);
    
    start_time = scheme_get_inexact_milliseconds();
    BEGIN_ESCAPEABLE(abandoned_clip, saferef);
    wxBlockUntilTimeout(CheckReadyString, saferef, clip_timeout);
    END_ESCAPEABLE();

    if (!receivedString) {
      /* Timeout */
      in_progress = 0;
      return NULL;
    }
    
    *length = receivedLength;

    in_progress = 0;

    if (xa == XA_STRING) {
      /* Really should encode... */
    }

    return receivedString;
  }
}
