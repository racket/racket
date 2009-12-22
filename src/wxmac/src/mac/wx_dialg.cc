///////////////////////////////////////////////////////////////////////////////
// File:	wx_dialg.cc
// Purpose:	wxDialogBox (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
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
  return new WXGC_PTRS wxFrame((wxFrame *)parentFrame, windowTitle, 
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
 wxWindow*	parentFrame,
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

//****************************************************************************
// File selector
//****************************************************************************

extern char *scheme_mac_spec_to_path(FSSpec *f);
extern int scheme_mac_path_to_spec(const char *filename, FSSpec *spec);
extern "C" {
  extern char *scheme_expand_filename(char* filename, int ilen, const char *errorin, int *ex, int guards);
  extern int scheme_is_complete_path(const char *s, long len, int kind);
  extern int scheme_file_exists(const char *s);
  extern char *scheme_find_completion(char *fn);
}

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
  NavDialogRef dialog;
  int has_parent, is_put;
  int need_show_select;
  char *initial_directory;
};

//-----------------------------------------------------------------------------
// Text path dialog
//-----------------------------------------------------------------------------
/* Cocoa's navigation dialog let's the user type "/" to get a dialog
   for entering an aboslute path in text, and tab implements path
   completion. Carbon's navigation dialog doesn't do that, so we roll
   our own. */

class wxCallbackCallbackInfo {
 public:
  WindowRef dialog;
  ControlRef txt;
  wxCallbackInfo *cbi;
};

static char *extract_string(wxCallbackCallbackInfo *ccbi)
{
  int len;
  char *result;
  CFStringRef str;
  Size sz;

  ::GetControlData(ccbi->txt, kControlEntireControl, 
		   kControlEditTextCFStringTag, sizeof(CFStringRef), &str, &sz);
  
  len = ::CFStringGetLength(str);
  result = new WXGC_ATOMIC char[len * 6 + 1];
  ::CFStringGetCString(str, result, len * 6 + 1, kCFStringEncodingUTF8);
  ::CFRelease(str);

  return result;
}

static OSStatus ok_evt_handler(EventHandlerCallRef inHandlerCallRef, 
			       EventRef inEvent, 
			       void *inUserData)
{
  char *result;
  FSSpec spec;
  wxCallbackCallbackInfo *ccbi = (wxCallbackCallbackInfo *)GET_SAFEREF(inUserData);
  WindowRef dialog = ccbi->dialog;

  result = extract_string(ccbi);
  
  result = scheme_expand_filename(result, -1, NULL, NULL, 0);

  if (result && scheme_mac_path_to_spec(result, &spec)) {
    AEDesc desc;
    AECreateDesc (typeFSS, &spec, sizeof(FSSpec), &desc);
    if (scheme_file_exists(result)) {
      NavCustomControl(ccbi->cbi->dialog, kNavCtlSetLocation, &desc); /* Leopard */
      NavCustomControl(ccbi->cbi->dialog, kNavCtlSetSelection, &desc); /* Tiger */
      if (ccbi->cbi->is_put) {
	NavCustomControl(ccbi->cbi->dialog, kNavCtlSetEditFileName, spec.name);
      }
    } else
      NavCustomControl(ccbi->cbi->dialog, kNavCtlSetLocation, &desc);
    ccbi->cbi->need_show_select = 1;
    AEDisposeDesc(&desc);
    if (!ccbi->cbi->has_parent) {
      ::HideSheetWindow(dialog);
    } else {
      ::HideWindow(dialog);
    }
    ::QuitAppModalLoopForWindow(dialog);
  } else
    wxBell();

  return noErr;
}

static OSStatus cancel_evt_handler(EventHandlerCallRef inHandlerCallRef, 
				   EventRef inEvent, 
				   void *inUserData)
{
  wxCallbackCallbackInfo *cbbi = (wxCallbackCallbackInfo *)GET_SAFEREF(inUserData);
  WindowRef dialog = cbbi->dialog;

  ::HideWindow(dialog);
  ::QuitAppModalLoopForWindow(dialog);

  return noErr;
}

static OSStatus key_evt_handler(EventHandlerCallRef inHandlerCallRef, 
				EventRef inEvent, 
				void *inUserData)
{
  char c;

  GetEventParameter(inEvent, kEventParamKeyMacCharCodes, typeChar, 
		    NULL, sizeof(c), NULL, &c);

  if (c == 13) {
    return ok_evt_handler(inHandlerCallRef, NULL, inUserData);
  } else if (c == 27) {
    return cancel_evt_handler(inHandlerCallRef, NULL, inUserData);
  } else
    return eventNotHandledErr;
}

static OSStatus tab_evt_handler(EventHandlerCallRef inHandlerCallRef, 
				EventRef inEvent, 
				void *inUserData)
{
  char c;

  GetEventParameter(inEvent, kEventParamKeyMacCharCodes, typeChar, 
		    NULL, sizeof(c), NULL, &c);

  if (c == 9) {
    char *result;
    int len;
    wxCallbackCallbackInfo *ccbi = (wxCallbackCallbackInfo *)GET_SAFEREF(inUserData);
    
    result = extract_string(ccbi);
    len = strlen(result);

    if (scheme_is_complete_path(result, len, 0)) {
      result = scheme_find_completion(result);
    } else {
      result = NULL;
    }

    if (result) {
      CFStringRef str;
      str = wxCFString(result);
      ::SetControlData(ccbi->txt, kControlEntireControl, 
		       kControlEditTextCFStringTag, sizeof(CFStringRef), &str);
      ::CFRelease(str);
      ::Draw1Control(ccbi->txt);
    } else
      wxBell();
    return noErr;
  } else
    return eventNotHandledErr;
}

static char *extract_current_dir(NavDialogRef context)
{
  AEDesc here, there;
  FSRef fsref;
  OSErr err;
  char *dir = NULL;
  
  NavCustomControl(context, kNavCtlGetLocation, &here);
  
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

  return dir;
}

static void do_text_path_dialog(wxCallbackInfo *cbi)
{
  int width = 500;
  WindowRef parent, dialog;
  ControlRef ok, lbl, txt, cancel;
  EventTypeSpec spec[1];
  Rect r, pr;
  char byteFlag = 1, *init;
  ControlFontStyleRec style;
  ControlEditTextSelectionRec sel;
  wxCallbackCallbackInfo *info;
  CFStringRef str;
  void *info_sr;

  info = new WXGC_PTRS wxCallbackCallbackInfo;
  info_sr = WRAP_SAFEREF(info);

  init = extract_current_dir(cbi->dialog);
  if (!init)
    init = "/";

  parent = ::NavDialogGetWindow(cbi->dialog);
  GetWindowBounds(parent, kWindowContentRgn, &pr);
 
  ::SetRect(&r, 0, 0, width, 90);
  r.top += ((pr.top + pr.bottom) - r.bottom) / 2;
  r.bottom += r.top;
  r.left = ((pr.left + pr.right) - r.right) / 2;
  r.right += r.left;

  ::CreateNewWindow(cbi->has_parent ? kMovableModalWindowClass : kSheetWindowClass,
		    kWindowCompositingAttribute, &r, &dialog);
  ::InstallStandardEventHandler(GetWindowEventTarget(dialog));
  spec[0].eventClass = kEventClassKeyboard;
  spec[0].eventKind = kEventRawKeyDown;
  info->dialog = dialog;
  info->cbi = cbi;
  ::InstallEventHandler(GetWindowEventTarget(dialog), key_evt_handler, 1, spec, info_sr, NULL);

  style.flags = kControlUseFontMask;
  style.font = kControlFontBigSystemFont;
      
  ::SetRect(&r, 10, 10, width - 10, 26);
  ::CreateStaticTextControl(dialog, &r, CFSTR("Go to the folder:"), &style, &lbl);
  ::ShowControl(lbl);

  ::SetRect(&r, 10, 32, width - 10, 48);
  str = wxCFString(init);
  ::CreateEditTextControl(dialog, &r, str, 0, 0, &style, &txt);
  :: CFRelease(str);
  spec[0].eventClass = kEventClassKeyboard;
  spec[0].eventKind = kEventRawKeyDown;
  ::InstallEventHandler(GetControlEventTarget(txt), tab_evt_handler, 1, spec, info_sr, NULL);
  ::ShowControl(txt);

  info->txt = txt;

  ::SetRect(&r, width - 75, 60, width - 10, 80);
  ::CreatePushButtonControl(dialog, &r, CFSTR("Goto"), &ok);
  ::ShowControl(ok);
  spec[0].eventClass = kEventClassControl;
  spec[0].eventKind = kEventControlHit;
  ::InstallEventHandler(GetControlEventTarget(ok), ok_evt_handler, 1, spec, info_sr, NULL);
  ::SetControlData(ok, kControlEntireControl, kControlPushButtonDefaultTag, 1, &byteFlag);

  ::SetRect(&r, width - 150, 60, width - 85, 80);
  ::CreatePushButtonControl(dialog, &r, CFSTR("Cancel"), &cancel);
  ::ShowControl(cancel);
  ::InstallEventHandler(GetControlEventTarget(cancel), cancel_evt_handler, 1, spec, info_sr, NULL);

  ::SetKeyboardFocus(dialog, txt, kControlFocusNextPart);
  sel.selStart = 1;
  sel.selEnd = 1000;
  ::SetControlData(txt, kControlEntireControl, kControlEditTextSelectionTag, sizeof(sel), &sel);
      
  if (!cbi->has_parent) {
    ::ShowSheetWindow(dialog, parent);
  } else {
    ::ShowWindow(dialog);
  }

  ::RunAppModalLoopForWindow(dialog);

  ::DisposeControl(lbl);
  ::DisposeControl(txt);
  ::DisposeControl(ok);
  ::DisposeControl(cancel);
  ::DisposeWindow(dialog);

  FREE_SAFEREF(info_sr);
  info_sr = NULL;
}

static OSStatus slash_key_evt_handler(EventHandlerCallRef inHandlerCallRef, 
				      EventRef inEvent, 
				      void *inUserData)
{
  char c;

  GetEventParameter(inEvent, kEventParamKeyMacCharCodes, typeChar, 
		    NULL, sizeof(c), NULL, &c);

  if (c == '/') {
    wxCallbackInfo *cbi = (wxCallbackInfo *)GET_SAFEREF(inUserData);
    do_text_path_dialog(cbi);
    return noErr;
  } else
    return eventNotHandledErr;
}

//-----------------------------------------------------------------------------
// File-selector callback
//-----------------------------------------------------------------------------
/* Sets the right initial directory, if one is supplied, and
   redirects '/' to open the text path dialog. */


static void ExtensionCallback(NavEventCallbackMessage callBackSelector, 
			      NavCBRecPtr callBackParms, 
			      void *callBackUD)
{
  wxCallbackInfo *cbi = (wxCallbackInfo *)GET_SAFEREF(callBackUD);

  switch (callBackSelector) {
  case kNavCBEvent:
    if (cbi->need_show_select) {
      cbi->need_show_select = 0;
      NavCustomControl(cbi->dialog, kNavCtlShowSelection, NULL);
    }
    break;
  case kNavCBStart:
    if (0) {
      /* No longer needed */
      EventTypeSpec spec[1];
      spec[0].eventClass = kEventClassKeyboard;
      spec[0].eventKind = kEventRawKeyDown;
      ::InstallEventHandler(GetWindowEventTarget(NavDialogGetWindow(callBackParms->context)), 
			    slash_key_evt_handler, 1, spec, callBackUD, NULL);
    }
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

static NavEventUPP extProc = NewNavEventUPP((NavEventProcPtr)ExtensionCallback);


static WindowPtr extract_sheet_parent(wxWindow *parent)
{
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
      WindowPtr win;
      mdc = f->MacDC();
      graf = mdc->macGrafPort();
      win = GetWindowFromPort(graf);
      if (IsWindowVisible(win))
        return win;
    }
  }

  return NULL;
}
	

extern "C" void wx_set_nav_file_types(NavDialogRef dlg, int cnt, char **exts, char *def_ext);

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
    void *cbi_sr;
    NavUserAction action;
    NavReplyRecord *reply;
    char *temp;
    char **acceptable_extensions = NULL;
    int num_acceptable = 0, single_type = 0;

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

    cbi = new WXGC_PTRS wxCallbackInfo();
    cbi->initial_directory = default_path;

    NavGetDefaultDialogCreationOptions(&dialogOptions);
    if (message) {
      dialogOptions.message = wxCFString(message);
    }
    dialogOptions.modality = kWindowModalityAppModal;

    if (flags & wxBUNDLES_ENTER)
      dialogOptions.optionFlags |= (kNavSupportPackages | kNavAllowOpenPackages);
    if (flags & wxBUNDLES_OK)
      dialogOptions.optionFlags |= kNavSupportPackages;
    if (!(flags & wxMULTIOPEN))
      dialogOptions.optionFlags -= (dialogOptions.optionFlags & kNavAllowMultipleFiles);

    /* Check whether to enforce a particular suffix */
    if (default_extension && wildcard && flags && !(flags & (wxOPEN | wxMULTIOPEN | wxGETDIR))) {
      GC_CAN_IGNORE char *s1, *s2;

      s1 = strchr(wildcard, '|');
      if (s1) {
	s1++;
	s2 = strchr(s1, '|');
	if (s2) {
	  int len, flen;
	  len = strlen(default_extension);
          if ((s1[0] == '*')
	      && (s1[1] == '.')
	      && ((s2 - s1) == (len + 2))
	      && !strncmp(default_extension, s1+2, len)
              && (!s1[len+2]
                  || ((s1[len+2] == '|')
                      && !s1[len+3]))) {
            single_type = 1;
	    dialogOptions.optionFlags |= kNavPreserveSaveFileExtension;
	    /* Make sure initial name has specified extension: */
	    if (!default_filename)
	      default_filename = "?";
	    flen = strlen(default_filename);
	    if ((flen < len + 1)
		|| (default_filename[flen -len - 1] != '.')
		|| strcmp(default_extension, default_filename + flen - len)) {
	      /* Need to add extension */
	      char *naya;
	      naya = new WXGC_ATOMIC char[flen + len +2];
	      memcpy(naya, default_filename, flen);
	      memcpy(naya + flen + 1, default_extension, len + 1);
	      naya[flen] = '.';
	      default_filename  = naya;
	    }
	  }
	}
      }
      
      if (!single_type) {
        /* Extract defaults */
        int cnt = 0;
        char **a, *ext;
        s1 = wildcard;
        while (s1) {
          s1 = strchr(s1, '|');
          if (s1) {
            if ((s1[1] == '*')
                && (s1[2] == '.')) {
              cnt++;
              s1 = strchr(s1 + 1, '|');
              if (s1) s1++;
            } else
              s1 = 0;
          }
        }
        if (cnt) {
          int i;
          a = new WXGC_PTRS char*[cnt];
          s1 = wildcard;
          for (i = 0; i < cnt; i++) {
            s1 = strchr(s1, '|');
            s1 += 3;
            s2 = strchr(s1, '|');
            if (!s2)
              s2 = s1 + strlen(s1);
            ext = new WXGC_ATOMIC char[s2 - s1 + 1];
            memcpy(ext, s1, s2 - s1);
            ext[s2 - s1] = 0;
            a[i] = ext;
            s1 = s2 + 1;
          }
          acceptable_extensions = a;
          num_acceptable = cnt;
        }
      }
    }

    if (default_filename)  {
      dialogOptions.saveFileName = wxCFString(default_filename);
    }

    cbi->has_parent = 1;

    if (parent) {
      WindowPtr win;
      win = extract_sheet_parent(parent);
      
      if (win) {
        dialogOptions.parentWindow = win;
        dialogOptions.modality = kWindowModalityWindowModal;
        cbi->has_parent = 1;
      }
    }

    cbi_sr = WRAP_SAFEREF(cbi);
    cbi->is_put = 0;

    // create the dialog:
    if (flags & wxGETDIR) {
      derr = NavCreateChooseFolderDialog(&dialogOptions,
					 extProc, NULL, cbi_sr, 
					 &outDialog);
    } else if ((flags == 0) || (flags & wxOPEN) || (flags & wxMULTIOPEN)) {
      derr = NavCreateGetFileDialog(&dialogOptions, NULL,
				    extProc, NULL, NULL, cbi_sr, 
				    &outDialog);
    } else {
      derr = NavCreatePutFileDialog(&dialogOptions, 'TEXT', 'mReD',
				    extProc, cbi_sr,
				    &outDialog);
      cbi->is_put = 1;
      if (derr == noErr)
        wx_set_nav_file_types(outDialog, num_acceptable, acceptable_extensions,
                              default_extension);
    }

    cbi->dialog = outDialog;
    cbi->need_show_select = 0;

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
      FREE_SAFEREF(cbi_sr);
      cbi_sr = NULL;
      return NULL;
    }
    
    if (dialogOptions.modality != kWindowModalityAppModal) {
      RunAppModalLoopForWindow(NavDialogGetWindow(outDialog));
    }

    wxPrimDialogCleanUp();
    
    FREE_SAFEREF(cbi_sr);
    cbi_sr = NULL;

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
    reply = new WXGC_ATOMIC NavReplyRecord;
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
      filename = new WXGC_ATOMIC char[strLen];

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
