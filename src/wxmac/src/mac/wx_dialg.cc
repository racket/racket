///////////////////////////////////////////////////////////////////////////////
// File:	wx_dialg.cc
// Purpose:	wxDialogBox (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

//-----------------------------------------------------------------------------
// Dialog box - like panel but doesn't need a frame, and is modal or non-modal
//-----------------------------------------------------------------------------

#include "wx_dialg.h"
#include "wx_panel.h"
#include "wx_utils.h"
#include "wx_messg.h"
#include "wx_main.h"
#include "wx_buttn.h"
#include "wx_mac_utils.h"
#include "wx_macevents.h"

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

extern wxApp* wxTheApp;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

static int IsUnshown(void *data)
{
  return !((wxDialogBox *)data)->IsShown();
}

//-----------------------------------------------------------------------------
void wxDialogBox::Show(Bool show)
{
  cFrame->Show(show);

  if (show) {
    if (!cCloseRequested) {
      wxPushModalWindow(ContextWindow(), cFrame);
      cCloseRequested = TRUE;
    }
    
    wxDispatchEventsUntil(IsUnshown, (void *)this);
  } else {
    if (cCloseRequested) {
      cCloseRequested = FALSE;
      wxPopModalWindow(ContextWindow(), cFrame);
    }
  }
}

Bool wxDialogBox::IsShown(void)
{
  return cFrame->IsShown();
}

void wxDialogBox::SetSize(int x, int y, int width, int height, int flags)
{
  if (!(flags & 0x70)) {
    int w, h, cw, ch;
    
    if ((width > -1) && (height > -1)) {
      cFrame->GetSize(&w, &h);
      cFrame->GetClientSize(&cw, &ch);
    } else
      w = h = cw = ch = 0;
    
    cFrame->SetSize(x, y, width + (w - cw), height + (h - ch), flags);
  } else
    wxWindow::SetSize(x, y, width, height, flags);
}

void wxDialogBox::EnforceSize(int minw, int minh, int maxw, int maxh, int incw, int inch)
{
  cFrame->EnforceSize(minw, minh, maxw, maxh, incw, inch);
}

// Default resizing behaviour - if only ONE subwindow,
// resize to client rectangle size
void wxDialogBox::OnSize(int x, int y)
{
  // Search for a child which is a subwindow, not another frame.
  wxWindow *child = NULL;
  // Count the number of _subwindow_ children
  int noChildren = 0;
  wxChildNode *node;
  wxChildList *cl;
  wxWindow *win;
  int client_x, client_y;

  cl = GetChildren();
  
  for (node = cl->First(); node; node = node->Next()) {
    WXTYPE winType;
    
    win = (wxWindow *)(node->Data());
    winType = win->__type;
    
    if (wxSubType(winType, wxTYPE_PANEL) ||
	wxSubType(winType, wxTYPE_TEXT_WINDOW) ||
	wxSubType(winType, wxTYPE_CANVAS)) {
      child = win;
      noChildren ++;
    }
  }
  if (!child || (noChildren > 1))
    return;

  GetClientSize(&client_x, &client_y);
  child->SetSize(0, 0, client_x, client_y, 0x70);
}

//-----------------------------------------------------------------------------
Bool wxDialogBox::IsModal(void)
{
  return cFrame->IsModal();
}

//-----------------------------------------------------------------------------
void wxDialogBox::ShowModal(void)
{
  Show(TRUE);
  if (!cFrame->IsModal()) {
    while (IsShown()) {
      wxTheApp->MainLoop();
    }
  }
}

void wxDialogBox::Fit(void)
{
  int x, y;

  wxPanel::Fit();
  wxPanel::GetSize(&x, &y);
  // cFrame->SetClientSize(x, y);
}

//-----------------------------------------------------------------------------
Bool wxDialogBox::OnClose(void)
{
  Bool result;
  if (IsModal()) {
    Show(FALSE);
    result = FALSE; // don't want dialog frame deleted
  } else {
    result = TRUE;
  }

  if (result)
    return cFrame->OnClose();

  return result;
}

//=============================================================================
// Public constructors
//=============================================================================

# define DIALOG_BORDER_STYLE wxMAXIMIZE

static wxFrame *make_dlog_frame(wxWindow *parentFrame, char *windowTitle, int x, int y, int width, int height, int style, char *windowName, WXTYPE objectType)
{
  return new wxFrame((wxFrame *)parentFrame, windowTitle, 
		     x, y,
		     width, height, 
		     (style | wxMDI_CHILD 
		      | ((style & DIALOG_BORDER_STYLE) 
			 ? 0
			 : wxNO_RESIZE_BORDER)), 
		     windowName, objectType);
}

//-----------------------------------------------------------------------------
wxDialogBox::wxDialogBox // Constructor (for dialog window)
(
 wxWindow*	parentFrame,		// this is ignored, used to be wxFrame*
 char*		windowTitle,
 Bool		modal,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
 wxPanel (make_dlog_frame(parentFrame, windowTitle, x, y, width, height, style, windowName, objectType),
	  0, 0, width, height)
{
  int w, h;

  cCloseRequested = 0;

  WXGC_IGNORE(this, cFrame);

  {
    wxWindow *parent;
    parent = GetParent();
    cFrame = (wxFrame *)parent;
  }
  cFrame->cDialogPanel = this;
  cFrame->MakeModal(modal);

  /* Set dialog panel to frame's client size: */
  cFrame->GetClientSize(&w, &h);
  SetSize(-1, -1, w, h, 0x70);
  
  __type = wxTYPE_DIALOG_BOX;
  
  wx_cursor = wxSTANDARD_CURSOR;
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxDialogBox::~wxDialogBox()
{
  if (cFrame) {
    wxChildList *tlw;
    tlw = wxTopLevelWindows(ContextWindow());
    tlw->DeleteObject(cFrame);
    cFrame = NULL;
  }
}

extern int wxsMessageBox(char *message, char *caption, long style, wxWindow *parent);

//-----------------------------------------------------------------------------
// Pop up a message box
//-----------------------------------------------------------------------------
int wxMessageBox(char* message, char* caption, long style,
                 wxWindow* parent, int x, int y)
{

  return wxsMessageBox(message, caption, style, parent);
}

#ifndef OS_X
extern "C" {
#endif
  extern char *scheme_mac_spec_to_path(FSSpec *f);
  extern int scheme_mac_path_to_spec(const char *filename, FSSpec *spec);
#ifndef OS_X
}
#endif

//= T.P. ==============================================================================

static int navinited = 0;

static int log_base_10(int i)
{
  if (i < 10) { 
    return 1;
  } else {
    return 1 + log_base_10(i / 10);
  }
}

#ifdef OS_X
# define PATH_SEPARATOR "/"
#else
// Result from the dialog already has a separator:
# define PATH_SEPARATOR ""
#endif

class wxCallbackInfo {
public:
  char *initial_directory;
  char *force_extension;
};

static void ExtensionCallback(NavEventCallbackMessage callBackSelector, 
			      NavCBRecPtr callBackParms, 
			      void *callBackUD)
{
  wxCallbackInfo *cbi = (wxCallbackInfo *)callBackUD;

  switch (callBackSelector) {
  case kNavCBStart:
    if (cbi->initial_directory) {
      FSSpec spec;
      if (scheme_mac_path_to_spec(cbi->initial_directory, &spec)) {
	AEDesc desc;
	AECreateDesc (typeFSS, &spec, sizeof(FSSpec), &desc);
	NavCustomControl(callBackParms->context, kNavCtlSetLocation, &desc);
	AEDisposeDesc(&desc);
      }
    }
    break;
  case kNavCBAccept:
    if (cbi->force_extension) {
      Str255 sv;
      StringPtr s;
      char *c, *suffix = cbi->force_extension;
      int i, sl;

      s = sv;
      NavCustomControl(callBackParms->context, kNavCtlGetEditFileName, s);

      c = wxP2C(s);
      for (i = s[0]; i--; ) {
	if (c[i] == '.')
	  break;
      }
      if (i < 0)
	i = 0;
      if (strcmp(c + i, suffix)) {
	unsigned char *s2;
	sl = strlen(suffix);
	s2 = (unsigned char *)(new WXGC_ATOMIC char[s[0] + sl + 1]);
	memcpy(s2 + 1, s + 1, s[0]);
	memcpy(s2 + 1 + s[0], suffix, sl);
	s2[0] = s[0] + sl;
	NavCustomControl(callBackParms->context, kNavCtlSetEditFileName, s2);
	s = s2;
      }

      {
	AEDesc here, there;
	FSRef fsref;
	OSErr err;
	char *dir = NULL;

	NavCustomControl(callBackParms->context, kNavCtlGetLocation, &here);

	err = AECoerceDesc(&here, typeFSRef, &there);
	if (err != noErr) {
	  if (err == errAECoercionFail) {
	    /* Try FSSpec: */
	    FSSpec spec;

	    err = AECoerceDesc(&here, typeFSRef, &there);
	    if (err == noErr) {	    
	      err = AEGetDescData(&there, &spec, sizeof(FSSpec));
	      if (err == noErr)
		dir = scheme_mac_spec_to_path(&spec);
	      AEDisposeDesc(&there);
	    }
	  }
	} else {
	  err = AEGetDescData(&there, &fsref, sizeof(fsref));
	  if (err == noErr)
	    dir = wxFSRefToPath(fsref);
	  AEDisposeDesc(&there);
	}

	if (!dir)
	  printf("NoDir! %d\n", err);

	if (dir) {
	  AlertStdAlertParamRec rec;
	  SInt16 which;

	  rec.movable = FALSE;
	  rec.helpButton = FALSE;
	  rec.filterProc = NULL;
	  rec.defaultText = "\pReplace";
	  rec.cancelText = (ConstStringPtr)kAlertDefaultCancelText;
	  rec.otherText = NULL;
	  rec.defaultButton = kAlertStdAlertCancelButton;
	  rec.cancelButton = 0;
	  rec.position = kWindowAlertPositionParentWindowScreen;

	  which = kAlertStdAlertCancelButton;

	  err = StandardAlert(kAlertCautionAlert,
			      "\pReally replace?",
			      NULL,
			      &rec,
			      &which);

	  if (which == kAlertStdAlertCancelButton) {
	    /* ??????? */
	  }
	}
      }
    }
  case kNavCBCancel: /* ^^^^^^^^^^ FALLTHROUGH ^^^^^^^^^^^^ */
    QuitAppModalLoopForWindow(callBackParms->window);
    break;
  }
}

static char *GetNthPath(NavReplyRecord *reply, int index)
{
  AEKeyword   theKeyword;
  DescType    actualType;
  Size        actualSize;
  FSRef	      fsref;
  OSErr       err;

  err = AEGetNthPtr(&(reply->selection), index, typeFSRef, &theKeyword, &actualType, 
		    &fsref, sizeof(fsref), &actualSize);
  if (err != noErr) {
    if (err == errAECoercionFail) {
      /* Try FSSpec: */
      FSSpec spec;

      err = AEGetNthPtr(&(reply->selection), index, typeFSS, &theKeyword, &actualType, 
			&spec, sizeof(FSSpec), &actualSize);
      if (err == noErr) {
	return scheme_mac_spec_to_path(&spec);
      }
    }
    return NULL;
  }

  return wxFSRefToPath(fsref);
}

static CFStringRef clientName = CFSTR("MrEd");
static NavEventUPP extProc = NewNavEventUPP((NavEventProcPtr)ExtensionCallback);

char *wxFileSelector(char *message, char *default_path,
                     char *default_filename, char *default_extension,
                     char *wildcard, int flags,
                     wxWindow *parent, int x, int y)
{
  if ((navinited >= 0) && (navinited || NavServicesAvailable())) {
    NavDialogRef outDialog;
    OSErr derr;
    NavDialogCreationOptions dialogOptions;
    wxCallbackInfo *cbi;
    NavUserAction action;
    NavReplyRecord *reply;
    char *temp;

    if (!navinited) {
      if (!NavLoad()) {
	navinited = 1;
      } else {
	navinited = -1;
	return wxFileSelector(message, default_path, default_filename,
			      default_extension, wildcard, flags,
			      parent, x, y);
      } 
    }

    cbi = new wxCallbackInfo();
    cbi->initial_directory = default_path;
    cbi->force_extension = NULL;

    NavGetDefaultDialogCreationOptions(&dialogOptions);
    if (default_filename)  {
      dialogOptions.saveFileName = CFStringCreateWithCString(NULL,default_filename,CFStringGetSystemEncoding());
    }
    if (message) {
      dialogOptions.message = CFStringCreateWithCString(NULL,message,CFStringGetSystemEncoding());
    }
    dialogOptions.modality = kWindowModalityAppModal;

    if (flags & wxBUNDLES_ENTER)
      dialogOptions.optionFlags |= (kNavSupportPackages | kNavAllowOpenPackages);
    if (flags & wxBUNDLES_OK)
      dialogOptions.optionFlags |= kNavSupportPackages;
    if (!(flags & wxMULTIOPEN))
      dialogOptions.optionFlags -= (dialogOptions.optionFlags & kNavAllowMultipleFiles);

    if (cbi->force_extension)
      dialogOptions.optionFlags |= kNavDontConfirmReplacement;

#ifdef OS_X
    if (parent) {
      wxFrame *f;

      if (wxSubType(parent->__type, wxTYPE_FRAME)) {
	f = (wxFrame *)parent;
      } else if (wxSubType(parent->__type, wxTYPE_DIALOG_BOX)) {
	f = (wxFrame *)parent->GetParent();
      } else
	f = NULL;

      if (f)
	f = f->GetSheetParent();

      if (f) {
	CGrafPtr graf;
	wxMacDC *mdc;
	mdc = f->MacDC();
	graf = mdc->macGrafPort();
	dialogOptions.parentWindow = GetWindowFromPort(graf);
	dialogOptions.modality = kWindowModalityWindowModal;
      }
    }
#endif

    // create the dialog:
    if (flags & wxGETDIR) {
      derr = NavCreateChooseFolderDialog(&dialogOptions,
					 extProc, NULL, cbi, 
					 &outDialog);
    } else if ((flags == 0) || (flags & wxOPEN) || (flags & wxMULTIOPEN)) {
      derr = NavCreateGetFileDialog(&dialogOptions, NULL,
				    extProc, NULL, NULL, cbi, 
				    &outDialog);
    } else {
      derr = NavCreatePutFileDialog(&dialogOptions, 'TEXT', 'mReD',
				    extProc, cbi,
				    &outDialog);
    }

    if (derr != noErr) {
      if (default_filename) 
	CFRelease(dialogOptions.saveFileName);
      if (message)
	CFRelease(dialogOptions.message);
      return NULL;
    }

    wxPrimDialogSetUp();

    // run the dialog (AppModal doesn't return until user closes dialog):
    if (NavDialogRun(outDialog) != noErr) {
      if (default_filename)
	CFRelease(dialogOptions.saveFileName);
      if (message)
	CFRelease(dialogOptions.message);
      NavDialogDispose(outDialog);
      wxTheApp->AdjustCursor();
      return NULL;
    }
    
    if (dialogOptions.modality != kWindowModalityAppModal) {
      RunAppModalLoopForWindow(NavDialogGetWindow(outDialog));
    }

    wxPrimDialogCleanUp();
    
    // dump those strings:
    if (default_filename)
      CFRelease(dialogOptions.saveFileName);
    if (message)
      CFRelease(dialogOptions.message);
    
    // did the user cancel?:
    action = NavDialogGetUserAction(outDialog);
    if ((action == kNavUserActionCancel) || (action == kNavUserActionNone)) {
      NavDialogDispose(outDialog);
      return NULL;
    }
    
    // get the user's reply:
#ifdef MZ_PRECISE_GC
    reply = (NavReplyRecord *)GC_malloc_atomic(sizeof(NavReplyRecord));
#else
    reply = new NavReplyRecord;
#endif
    if (NavDialogGetReply(outDialog,reply) != noErr) {
      NavDialogDispose(outDialog);
      return NULL;
    }
    NavDialogDispose(outDialog);
    if (! reply->validRecord) {
      NavDisposeReply(reply);
      return NULL;
    }
    
    if (flags & wxMULTIOPEN) {
      long count, index;
      char *newpath, *aggregate = "";
      OSErr err;
      
      err = AECountItems(&(reply->selection),&count);
      if (err != noErr) {
        NavDisposeReply(reply);
        return NULL;
      }
      
      for (index=1; index<=count; index++) {
	temp = GetNthPath(reply, index);
        if (temp != NULL) {
	  long size;
	  size = (strlen(aggregate) 
		  + strlen(temp)
		  + log_base_10(strlen(temp)) + 3);
          newpath = new WXGC_ATOMIC char[size];
          sprintf(newpath,"%s %ld %s",aggregate,strlen(temp),temp);
          aggregate = newpath;
        }
      }
      
      NavDisposeReply(reply);
      
      return aggregate;
    } else if ((flags & wxOPEN) || (flags & wxGETDIR)) {
      char *path;

      path = GetNthPath(reply, 1);
      NavDisposeReply(reply);
      return path;
    } else { // saving file
      int strLen;
      char *filename;
      char *path, *wholepath;

      strLen = (6 * CFStringGetLength(reply->saveFileName)) + 1;
      filename = new char[strLen];

      if (CFStringGetCString(reply->saveFileName,filename,strLen,kCFStringEncodingUTF8) == FALSE) {
	// Unable to convert string
	NavDisposeReply(reply);
	return NULL;
      }

      path = GetNthPath(reply, 1);
      
      if (path == NULL) {
	NavDisposeReply(reply);
	return NULL;
      }
      
      wholepath = new WXGC_ATOMIC char[strlen(path) + strlen(filename) + 2];
      
      sprintf(wholepath,"%s" PATH_SEPARATOR "%s",path,filename);
      
      NavDisposeReply(reply);
      
      return wholepath;
    }
  } else
    return NULL;
}
