///////////////////////////////////////////////////////////////////////////////
// File:	wx_frame.cc
// Purpose:	wxFrame implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_frame.h"
#include "wx_menu.h"
#include "wx_mnuit.h"
#include "wx_gdi.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_panel.h"
#include "wx_area.h"
#include "wx_screen.h"
#include "wx_mac_utils.h"
#include "wx_main.h"
#include "wx_messg.h"
#include "wx_canvs.h"
#include "wx_utils.h"
#include "wx_het.h"

static wxMenuBar *close_menu_bar;

static wxFrame *current_focus_window;

extern int mred_current_thread_is_handler(void *ctx);
extern int mred_in_restricted_context();
extern int wxMenuBarHeight;
extern int wx_activate_anyway;

extern void MrEdQueuePaint(wxWindow *wx_window);
extern void MrEdQueueClose(wxWindow *wx_window);
extern void MrEdQueueZoom(wxWindow *wx_window);
extern void MrEdQueueToolbar(wxWindow *wx_window);
extern void MrEdQueueUnfocus(wxWindow *wx_window);
extern void MrEdQueueDrop(wxWindow *wx_window, char *s);

extern int wxsIsContextShutdown(void *cx);

static OSStatus window_evt_handler(EventHandlerCallRef inHandlerCallRef, 
				   EventRef inEvent, 
				   void *inUserData);

extern char *scheme_mac_spec_to_path(FSSpec *spec);

static int os_x_post_tiger;
static int zoom_window_hack = 0;

//=============================================================================
// Public constructors
//=============================================================================

static pascal void userPaneDrawFunction(ControlRef controlRef, SInt16 thePart);
static ControlUserPaneDrawUPP userPaneDrawFunctionUPP = NewControlUserPaneDrawUPP(userPaneDrawFunction); 

//-----------------------------------------------------------------------------
wxFrame::wxFrame // Constructor (for frame window)
(
 wxFrame*	parentFrame,
 char*		windowTitle,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
 wxbFrame (windowName, wxScreen::gScreenWindow,
	   x, y, 
	   (cStyle & wxNO_CAPTION) ? width : ((width < 30) ? 30 : width), 
	   (cStyle & wxNO_CAPTION) ? height : ((height < 40) ? 40 : height), style)
{
  int X, Y, theMacX, theMacY, theMacWidth, theMacHeight;
  Rect theBoundsRect;
  OSErr result;
  WindowClass windowClass;
  WindowAttributes windowAttributes;
  Rect theStrucRect;
  Rect theContRect;
  ControlRef rootControl;
  wxMargin contentAreaMargin;
  WindowPtr theMacWindow;
  wxArea *carea, *parea;
  wxMargin pam;
  int metal = (style & wxMETAL);
  
  InitDefaults();

  cActive = FALSE;

  SetEraser(wxCONTROL_BACKGROUND_BRUSH);

  X = cWindowX;
  Y = cWindowY;

  ::SetRect(&theBoundsRect, X, Y, X + cWindowWidth, Y + cWindowHeight);

  cUserHidden = TRUE;

  if (parentFrame)
    if (wxSubType(parentFrame->__type, wxTYPE_DIALOG_BOX))
      parentFrame = (wxFrame *)parentFrame->GetParent();
  
  if (cStyle & wxMDI_CHILD) { // hack : MDI_CHILD means dialog box
#ifdef OS_X
    if (parentFrame && parentFrame->GetSheetParent()
	&& !(cStyle & wxNOT_AS_SHEET)) {
      WXGC_IGNORE(this, cSheetParent);
      cSheetParent = parentFrame->GetSheetParent();
      windowClass = kSheetWindowClass;
    } else
#endif
      {
	if (cStyle & wxNO_CAPTION)
	  windowClass = kPlainWindowClass;
	else
	  windowClass = kDocumentWindowClass;  /* kMovableModalWindowClass => OS X does modality, which we don't want */
      }
    if (cStyle & wxNO_RESIZE_BORDER) {
      windowAttributes = kWindowNoAttributes;
    } else {
      windowAttributes = kWindowResizableAttribute;
    }
  } else {
    if (cStyle & wxFLOAT_FRAME) {
      if (cStyle & wxNO_CAPTION) {
	metal = 0;
	windowClass = kToolbarWindowClass;
	windowAttributes = kWindowNoAttributes;
      } else {
	windowClass = kUtilityWindowClass; /* or kFloatingWindowClass? */
	windowAttributes = kWindowCloseBoxAttribute;
	if (!(cStyle & wxNO_RESIZE_BORDER)) {
	  windowAttributes |= kWindowFullZoomAttribute | kWindowResizableAttribute;
	}
      }
    } else if (cStyle & wxNO_CAPTION) {
      metal = 0;
      windowClass = kPlainWindowClass;
      if (cStyle & wxNO_RESIZE_BORDER)
	windowAttributes = kWindowNoAttributes;
      else {
	/* Can't support resizable, though */
	windowAttributes = kWindowNoAttributes;
      }
    } else {
      windowClass = kDocumentWindowClass;
      if (cStyle & wxNO_RESIZE_BORDER)
	windowAttributes = kWindowStandardFloatingAttributes;
      else
	windowAttributes = kWindowStandardDocumentAttributes;
    }
  }

  windowAttributes |= (metal ? kWindowMetalAttribute : 0) | kWindowCompositingAttribute;

#ifdef OS_X
  if (cStyle & wxTOOLBAR_BUTTON)
    windowAttributes |= kWindowToolbarButtonAttribute;
#endif

  result = ::CreateNewWindow(windowClass, windowAttributes, &theBoundsRect, &theMacWindow);  
  
  if (result != noErr) {
    char error[256];
    sprintf(error,"wxFrameConstructor: Attempt to create window failed with error: %d.",
	    result);
    wxFatalError(error);
  }

  if (windowTitle) {
    CFStringRef wtitle;
    wtitle = wxCFString(windowTitle);
    SetWindowTitleWithCFString(theMacWindow, wtitle);
    CFRelease(wtitle);
  }
  
  {
    void *rc;
    rc = WRAP_SAFEREF(this);
    refcon = rc;
    SetWRefCon(theMacWindow, (long)refcon);
  }
  
  CheckMemOK(theMacWindow);

  cMacDC = new WXGC_PTRS wxMacDC(GetWindowPort(theMacWindow));

  // Calculate the platformArea size
  GetWindowBounds(theMacWindow,kWindowStructureRgn,&theStrucRect);
  GetWindowBounds(theMacWindow,kWindowContentRgn,&theContRect);
  wxMargin platformMargin;
  platformMargin.SetMargin(theContRect.left - theStrucRect.left, wxLeft);
  platformMargin.SetMargin(theContRect.top - theStrucRect.top, wxTop);
  platformMargin.SetMargin(theStrucRect.right - theContRect.right, wxRight);
  platformMargin.SetMargin(theStrucRect.bottom - theContRect.bottom, wxBottom);
  parea = PlatformArea();
  parea->SetMargin(platformMargin);

  // The client has the requested window position, not the window: must move
  SetCurrentDC();
  carea = ContentArea();
  contentAreaMargin = carea->Margin(wxScreen::gScreenWindow);
  theMacX = contentAreaMargin.Offset(wxLeft);
  theMacY = contentAreaMargin.Offset(wxTop);
  MoveWindow(theMacWindow, theMacX, theMacY, FALSE);

  // The client has the requested window size, not the window: must resize
  parea = PlatformArea();
  pam = parea->Margin();
  theMacWidth = cWindowWidth - pam.Offset(wxHorizontal);
  theMacHeight = cWindowHeight - pam.Offset(wxVertical);
  SizeWindow(theMacWindow, theMacWidth, theMacHeight, FALSE);
  
  windowStyle |= ((style & wxHIDE_MENUBAR) | (style & wxMETAL));

  wx_cursor = wxSTANDARD_CURSOR;
  
  if (wxIsBusy())
    cBusyCursor = 1;

  ::GetRootControl(theMacWindow,&rootControl);
  if (!rootControl) {
    // create a root control, to enable control embedding
    ::CreateRootControl(theMacWindow,&rootControl);
  }
  cMacControl = rootControl;

  if (!metal) {
    Rect r = {0, 0, 6000, 6000};
    ControlRef ctl;
    CreateUserPaneControl(theMacWindow, &r, 0, &ctl);
    SetControlData(ctl, kControlEntireControl, kControlUserPaneDrawProcTag, 
		   sizeof(userPaneDrawFunctionUPP), (Ptr)&userPaneDrawFunctionUPP);
    ::EmbedControl(ctl, cMacControl);
    bgControl = ctl;    
  }

  EnforceSize(-1, -1, -1, -1, 1, 1);

  if (!os_x_post_tiger) {
    SInt32 res;
    Gestalt(gestaltSystemVersion, &res);
    if ((res & 0xFFFF) >= ((1 << 12) | (0 << 8) | (4 << 4)))
      os_x_post_tiger = 1;
    else
      os_x_post_tiger = -1;
  }


  {
    /* Handle some events. */
    EventTypeSpec spec[4];
    spec[0].eventClass = kEventClassWindow;
    spec[0].eventKind = kEventWindowClose;
    spec[1].eventClass = kEventClassWindow;
    spec[1].eventKind = kEventWindowZoom;
    spec[2].eventClass = kEventClassWindow;
    spec[2].eventKind = kEventWindowBoundsChanging;
    spec[3].eventClass = kEventClassWindow;
    spec[3].eventKind = kEventWindowToolbarSwitchMode;
    InstallEventHandler(GetWindowEventTarget(theMacWindow), window_evt_handler, 4, spec, refcon, NULL);
  }

  {
    /* In case we need to recognize MrEd windows: */
    UInt32 val = 1;
    SetWindowProperty (theMacWindow, 'mReD', 'Ello', sizeof(UInt32), &val);
  }
}

static void userPaneDrawFunction(ControlRef controlRef, SInt16 thePart)
{
  Rect itemRect;
  int depth;
  ThemeDrawingState s;

  GetThemeDrawingState(&s);

  depth = wxDisplayDepth();
  SetThemeBackground(kThemeBrushDialogBackgroundActive, depth, depth > 1);
  GetControlBounds(controlRef, &itemRect);
  EraseRect(&itemRect);

  SetThemeDrawingState(s, TRUE);
} 

static OSStatus window_evt_handler(EventHandlerCallRef inHandlerCallRef, 
				   EventRef inEvent, 
				   void *inUserData)
{
  wxFrame *f;
  
  f = (wxFrame*)GET_SAFEREF(inUserData);
  switch (GetEventKind(inEvent)) {
  case kEventWindowClose:
    MrEdQueueClose(f);
    break;
  case kEventWindowZoom:
    MrEdQueueZoom(f);
    break;
  case kEventWindowToolbarSwitchMode:
    MrEdQueueToolbar(f);
    break;
  case kEventWindowBoundsChanging:
    if (os_x_post_tiger > 0) {
      UInt32 a;
      
      GetEventParameter(inEvent, kEventParamAttributes, typeUInt32, 
			NULL, sizeof(a), NULL, &a);
      if (a & kWindowBoundsChangeUserDrag) {
	/* DragWindow is somehow confused by MrEd. It seems to confuse the struct
	   and content regions, causing current bounds's height to become
	   different from the original bound's height. To compensate,
	   10.3 seems to ignore the height, but 10.4 doesn't. So we
	   explicitly request the old size. */
	Rect o, n;
	GetEventParameter(inEvent, kEventParamPreviousBounds, typeQDRectangle, 
			  NULL, sizeof(Rect), NULL, &o);
	GetEventParameter(inEvent, kEventParamCurrentBounds, typeQDRectangle, 
			  NULL, sizeof(Rect), NULL, &n);
	n.bottom = n.top + (o.bottom - o.top);
	n.right = n.left + (o.right - o.left);
	SetEventParameter(inEvent, kEventParamCurrentBounds, typeQDRectangle, 
			  sizeof(Rect), &n);
      } else if (!(a & (kWindowBoundsChangeUserResize | (zoom_window_hack ? 0 : kWindowBoundsChangeZoom)))) {
	Rect n, s, c;
	WindowRef w;

	GetEventParameter(inEvent, kEventParamDirectObject, typeWindowRef,
			  NULL, sizeof(WindowRef), NULL, &w);

	GetWindowBounds(w, kWindowStructureRgn, &s);
	GetWindowBounds(w, kWindowContentRgn, &c);

	if (((c.bottom - c.top) == (s.bottom - s.top))
	    && ((c.right - c.left) == (s.right - s.left))) {
	  /* Struct size == content size? This is when the window
	     manager is confused by MrEd in 10.4. Subtract out the difference
	     between the real content and structure regions. */
	  int h, w, hdelta, wdelta;

          GetEventParameter(inEvent, kEventParamCurrentBounds, typeQDRectangle, 
			    NULL, sizeof(Rect), NULL, &n);
	  
          if (a & kWindowBoundsChangeZoom) {
            h = n.bottom - n.top;
            w = n.right - n.left;
          } else {
            w = f->Width();
            h = f->Height();
          }
            
	  {
	    wxArea *parea;
	    wxMargin pam;
	    parea = f->PlatformArea();
	    pam = parea->Margin();
	    wdelta = pam.Offset(wxHorizontal);
	    hdelta = pam.Offset(wxVertical);
	  }

	  n.bottom = n.top + h - hdelta;
	  n.right = n.left + w - wdelta;
	  SetEventParameter(inEvent, kEventParamCurrentBounds, typeQDRectangle, 
			    sizeof(Rect), &n);
	}
      }
    }
    break;
  }

  return noErr;
}

//=============================================================================
// Public destructor
//=============================================================================

wxFrame::~wxFrame(void)
{
  CWindowPtr theMacWindow;

  cFocusWindow = NULL; /* don't try to de-focus while hiding */

  if (IsVisible()) 
    Show(FALSE);

  // Kludge needed here:
  // Bad:  DisposeWindow deletes the mac controls, but not their wxItems.
  // Good: DestroyChildren deletes the wxItems and their associated mac controls.
  if (cDialogPanel)
    cDialogPanel = NULL;
  DestroyChildren();

  if (bgControl) { 
    ::DisposeControl(bgControl);
    bgControl = NULL;
  }

  theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());
  ::DisposeWindow(theMacWindow);
  DELETE_OBJ cMacDC;
  if (wx_menu_bar)
    DELETE_OBJ wx_menu_bar;
  if (cControlArea)
    DELETE_OBJ cControlArea;
  if (cContentArea)
    DELETE_OBJ cContentArea;
  if (cPlatformArea)
    DELETE_OBJ cPlatformArea;
}

//=============================================================================
// Paint callback when showing
//=============================================================================

static int DoPaint(void *_c)
{
  wxCanvas *c = (wxCanvas *)_c;
  
  c->DoPaint();

  return 0;
}

void wxCallDoPaintOrQueue(wxCanvas *win)
{
  wxFrame *f;

  f = win->GetRootFrame();

  /* we can all OnPaint only when we've set up a trampoline around whatever
     triggers the update.  The trampoline is necessary because OnPaint()
     might invoke arbitrary Scheme code, and because the system called
     us, we can't allow thread swaps (which would copy a part of the
     stack that the system owns). See the use of wxHETShowWindow
     below. */

  if (f->cCanUpdateOnCallback) {
    while (f->cCanUpdateOnCallback) {
      --f->cCanUpdateOnCallback;
      /* wxHETYield doesn't actually wxYield(). It calls our DoPaint
	 proc. This might run some Scheme code and then time out
	 for a thread swap. We give the thread up to 10 times its
	 normal alotment to finish drawing the frame. */
      if (!wxHETYield(f, DoPaint, win))
	break;
    }
  } else
    MrEdQueuePaint(win);
}

//=============================================================================
// Root frame
//=============================================================================

wxFrame *wxRootFrame = NULL;

void wxFrame::DesignateRootFrame(void)
{
  if (!wxRootFrame) {
    CGrafPtr graf;
    WindowRef win;

    graf = cMacDC->macGrafPort();
    win = GetWindowFromPort(graf);

    cWindowWidth = 0;
    cWindowHeight = 0;
    cWindowX = 32000;
    cWindowY = 32000;
    ::MoveWindow(win, 32000, 32000, FALSE);
    ::SizeWindow(win, 0, 0, FALSE);

    wxREGGLOB(wxRootFrame);
    wxRootFrame = this;
    Show(TRUE);
    
    if (!FrontNonFloatingWindow()) {
      ::ShowWindow(win);
    }
  }
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxFrame::InitDefaults(void)
{
  cMaximized = FALSE;
  cIsModal = FALSE;
  cBusyCursor = 0;
  cFocusWindow = NULL;
  cStatusPanel = NULL;
  cStatusText = NULL;
  cDialogPanel = NULL;
  cControlArea = new WXGC_PTRS wxArea(this);
  cContentArea = new WXGC_PTRS wxArea(this);
  cPlatformArea = new WXGC_PTRS wxArea(this);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Geometry methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxArea* wxFrame::PlatformArea(void) { return cPlatformArea; } // mac platform only

//-----------------------------------------------------------------------------
wxArea* wxFrame::ContentArea(void) { return cContentArea; } // mac platform only

//-----------------------------------------------------------------------------
wxArea* wxFrame::ControlArea(void) { return cControlArea; } // mac platform only

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#define max(x, y) ((x > y) ? x : y)

//-----------------------------------------------------------------------------
// Note: Can't set width < width of borders + etc.
// Note: Can't set height < height of borders + statusLine + etc.
// Note: Mac platform can't set width = width of borders + etc.
// Note: Mac platform can't set height = height of borders + statusLine + etc.
//-----------------------------------------------------------------------------
void wxFrame::DoSetSize(int x, int y, int width, int height)
{
  Bool xIsChanged, yIsChanged, widthIsChanged, heightIsChanged;
  WindowPtr theMacWindow;
  int dw, dh;

  if (width==-1) 
    width = cWindowWidth;
  if (height==-1) 
    height = cWindowHeight;
  
  xIsChanged = (x != cWindowX);
  yIsChanged = (y != cWindowY);
  widthIsChanged = (width != cWindowWidth);
  heightIsChanged = (height != cWindowHeight);

  dw = width - cWindowWidth;
  dh = height - cWindowHeight;

  if (xIsChanged) cWindowX = x;
  if (yIsChanged) cWindowY = y;
  if (widthIsChanged) cWindowWidth = width;
  if (heightIsChanged) cWindowHeight = height;

  SetCurrentDC();
  theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());

  if (xIsChanged || yIsChanged)
    {
      wxMargin contentAreaMargin;
      int theMacX, theMacY;
      wxArea *carea;
      carea = ContentArea();
      contentAreaMargin = carea->Margin(wxScreen::gScreenWindow);
      theMacX = contentAreaMargin.Offset(wxLeft);
      theMacY = contentAreaMargin.Offset(wxTop);
      ::MoveWindow(theMacWindow, theMacX, theMacY, FALSE);
    }

  if (widthIsChanged || heightIsChanged)
    {
      int theMacWidth, theMacHeight;
      wxArea *parea;
      wxMargin pam;

      parea = PlatformArea();
      pam = parea->Margin();
      theMacWidth = cWindowWidth - pam.Offset(wxHorizontal);
      theMacHeight = cWindowHeight - pam.Offset(wxVertical);

      if (!dw && (dh == 22) && (os_x_post_tiger < 0)) {
	/* Defeat OS X bug that incorrectly shortcuts when the target
	   client size matches the structure size */
	::SizeWindow(theMacWindow, theMacWidth, theMacHeight+1, TRUE);
      }
      ::SizeWindow(theMacWindow, theMacWidth, theMacHeight, TRUE);
      // Resizing puts windows into the unzoomed state
      cMaximized = FALSE;

      if (cStatusPanel) {
	int w, h;

	cStatusPanel->SetSize(0, theMacHeight - cStatusPanel->Height(),
			      theMacWidth, -1);
	cStatusPanel->GetClientSize(&w, &h);
	cStatusText->SetSize(-1, -1, w, -1);
	cStatusText->MaybeMoveControls();
      }
      
      // Call OnSize handler
      if (this != wxRootFrame) {
	OnSize(width, height);
      }
    }
}

//-----------------------------------------------------------------------------
void wxFrame::Maximize(Bool maximize)
{
  if (maximize == 2) {
    maximize = !cMaximized;
  }

  if (cMaximized != maximize)
    {
      int oldWindowX = cWindowX;
      int oldWindowY = cWindowY;
      int oldWindowWidth = cWindowWidth;
      int oldWindowHeight = cWindowHeight;
      int pam_dh, pam_dv;
      CGrafPtr theMacGrafPort;
      WindowPtr theMacWindow;
      Point size;
  
      theMacGrafPort = cMacDC->macGrafPort();
      theMacWindow = GetWindowFromPort(theMacGrafPort);

      {
        wxArea *parea;
	wxMargin pam;

	parea = PlatformArea();
	pam = parea->Margin();

        pam_dh = pam.Offset(wxHorizontal);
        pam_dv = pam.Offset(wxVertical);
      }

      if (maximize) {
	Rect rect;

	IsWindowInStandardState(theMacWindow, NULL, &rect);
	size.h = (rect.right - rect.left);
	size.v = (rect.bottom - rect.top);
	
        if ((size.v + pam_dv == cWindowHeight)
            && (size.h + pam_dh == cWindowWidth)) {
          zoom_window_hack = 1;
        }
      } else {
        Rect r;
	GetWindowIdealUserState(theMacWindow, &r);
        if (((r.bottom - r.top + pam_dv) == cWindowHeight)
            && ((r.right - r.left + pam_dh) == cWindowWidth)) {
          zoom_window_hack = 1;
        }
      }

      ::ZoomWindowIdeal(theMacWindow, maximize ? inZoomOut : inZoomIn, &size);

      if (maximize) {
	/* MrEd somehow confuses Carbon about structure region versus content region.
	   Subtract out the difference. */
	Rect r;
	GetWindowIdealUserState(theMacWindow, &r);
	r.right -= pam_dh;
	r.bottom -= pam_dv;
	SetWindowIdealUserState(theMacWindow, &r);
      }

      zoom_window_hack = 0;

      wxMacRecalcNewSize();
      
      if (cStatusPanel) {
	int theMacWidth, theMacHeight;
	int w, h;
	wxArea *parea;
	wxMargin pam;

	parea = PlatformArea();
	pam = parea->Margin();

	theMacWidth = cWindowWidth - pam.Offset(wxHorizontal);
	theMacHeight = cWindowHeight - pam.Offset(wxVertical);
	
	cStatusPanel->SetSize(0, theMacHeight - cStatusPanel->Height(),
			      theMacWidth, -1);
	cStatusPanel->GetClientSize(&w, &h);
	cStatusText->SetSize(-1, -1, w, -1);
	cStatusText->MaybeMoveControls();
      }
      
      {
	int dW = cWindowWidth - oldWindowWidth;
	int dH = cWindowHeight - oldWindowHeight;
	int dX = cWindowX - oldWindowX;
	int dY = cWindowY - oldWindowY;
	OnWindowDSize(dW, dH, dX, dY);
	OnSize(cWindowWidth, cWindowHeight);
      }

      cMaximized = maximize;
    }
}

Bool wxFrame::IsMaximized()
{
  return cMaximized;
}

void wxFrame::EnforceSize(int minw, int minh, int maxw, int maxh, int incw, int inch)
{
  RgnHandle screen;
  Rect bounds;

  if (minw < 0)
    minw = 1;
  if (minh < 0)
    minh = 1;

  screen = GetGrayRgn();
  GetRegionBounds(screen, &bounds);

  if (maxh < 0)
    maxh = bounds.bottom - bounds.top;
  if (maxw < 0)
    maxw = bounds.right - bounds.left;

  size_limits.left = minw;
  size_limits.top = minh;
  size_limits.right = maxw;
  size_limits.bottom = maxh;  
}

void wxFrame::GetSizeLimits(Rect *r)
{
  wxArea *parea;
  wxMargin pam;
  int dh, dv;

  memcpy(r, &size_limits, sizeof(Rect));

  parea = PlatformArea();
  pam = parea->Margin();
  dh = pam.Offset(wxHorizontal);
  dv = pam.Offset(wxVertical);

  r->left -= dh;
  r->top -= dv;
  r->right -= dh;
  r->bottom -= dv;

  if (r->left < 1)
    r->left = 1;
  if (r->top < 1)
    r->top = 1;
  if (r->right < r->left)
    r->right = r->left;
  if (r->bottom < r->top)
    r->bottom = r->top;
}

//-----------------------------------------------------------------------------
// Mac platform only; internal use only.
//-----------------------------------------------------------------------------
void wxFrame::wxMacRecalcNewSize(Bool resize)
{
  Rect theStrucRect;
  int mbh;

  GetWindowBounds(GetWindowFromPort(cMacDC->macGrafPort()),kWindowStructureRgn,&theStrucRect);

  cWindowX = theStrucRect.left;
  
  mbh = wxMenuBarHeight;
  cWindowY = theStrucRect.top - mbh;
  if (resize) {
    cWindowWidth = theStrucRect.right - theStrucRect.left;
    cWindowHeight = theStrucRect.bottom - theStrucRect.top;
  }

  if (sheet)
    sheet->wxMacRecalcNewSize(FALSE);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Status line methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxFrame::CreateStatusLine(int number, char* name)
{
  wxMargin clientAreaMargin;
  int statusLineHeight;
  int clientWidth, clientHeight;
  wxArea *carea;

  if (status_line_exists) return;

  nb_status = number;
  status_line_exists = TRUE;
  cStatusPanel = new WXGC_PTRS wxPanel(ControlArea());
  cStatusText = new WXGC_PTRS wxMessage(cStatusPanel, "");
  cStatusPanel->SetEraser(cEraser);
  cStatusText->SetEraser(cEraser);
  statusLineHeight = (int)(cStatusText->GetCharHeight() * nb_status);
  GetClientSize(&clientWidth, &clientHeight);
  cStatusText->SetWidthHeight(clientWidth, statusLineHeight);
  cStatusPanel->Fit();
#if 1
  // it is a hack to put the line down..
  cStatusPanel->SetSize(0, clientHeight - cStatusPanel->Height(),
			cStatusPanel->Width(), cStatusPanel->Height()); // tom!!
  // tom: here the Statuspanel is placed over the controlArea!!
  cControlArea->SetMargin(cStatusPanel->Height() + 1, wxBottom);
#else
  cStatusPanel->SetSize(0, 0, cStatusPanel->Width(), cStatusPanel->Height());
  cControlArea->SetMargin(cStatusPanel->Height() + 1, wxTop);
#endif
  // cStatusPanel->SetJustify(wxLeft);

  carea = ClientArea();

  clientAreaMargin = carea->Margin(wxScreen::gScreenWindow);
  {
    int o;
    o = clientAreaMargin.Offset(wxBottom);
    clientAreaMargin.SetMargin(o + statusLineHeight - 1, wxBottom);
  }
  carea->SetMargin(clientAreaMargin);
  OnSize(cWindowWidth, cWindowHeight);
}

//-----------------------------------------------------------------------------
void wxFrame::SetStatusText(char* text, int number)
{
  if (!status_line_exists) 
    return;
  cStatusText->SetLabel(text);
}

// tom: perhaps this could be done otherwise:
//      get statusline from Frame
//		do statusline->SetEraser
void wxFrame::SetStatusEraser(wxBrush* b)
{
  if (!status_line_exists) 
    return;
  cStatusPanel->SetEraser(b);
  cStatusText->SetEraser(b);
  
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Menubar methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxFrame::SetMenuBar(wxMenuBar* menu_bar)
{
  WindowPtr theMacWindow,front;
  wxMenuBar* oldMenuBar;
  CGrafPtr graf;
  
  // if this menu bar is already in use, give up
  if (menu_bar && menu_bar->menu_bar_frame)
    return;

  graf = cMacDC->macGrafPort();
  theMacWindow = GetWindowFromPort(graf);
  oldMenuBar = wx_menu_bar;
  if (oldMenuBar) {
    wx_menu_bar = NULL;
    oldMenuBar->menu_bar_frame = NULL;
    // must correct each menu->menu_bar and menuItem->menuBar
  }

  menu_bar->handle = NULL; // menu_bar->handle is not used
  if (menu_bar) menu_bar->menu_bar_frame = this;
  wx_menu_bar = menu_bar;

  front = FrontNonFloatingWindow();
  if (front == theMacWindow) {
    NowFront(TRUE);
  }
}

//-----------------------------------------------------------------------------
void wxFrame::Command(int id)
{ // Call this to simulate a menu command
  wxMenuBar* menuBar;

  menuBar = GetMenuBar();
  if (menuBar) {
    wxMenuItem* item;
    item = menuBar->FindItemForId(id);
    if (item) {
      wxMenu* theParentMenu;
      theParentMenu = item->ParentMenu();
      if (theParentMenu) {
	wxPrepareMenuDraw();
	HiliteMenu(theParentMenu->cMacMenuId); // hilite the menu
	wxDoneMenuDraw(TRUE);
	ProcessCommand(id);
      }
    }
  }
}

void wxFrame::OnMenuClick()
     /* Called when the user clicks on the menu bar */
{
  /* Do nothing; MrEd overrides it. */
}

//-----------------------------------------------------------------------------
void wxFrame::ProcessCommand(int id)
{
  OnMenuCommand(id);
  wxPrepareMenuDraw();
  // HiliteMenu(0); // calling wxPrepareMenuDraw unhlites the menu
  wxDoneMenuDraw();
}

//-----------------------------------------------------------------------------
void wxFrame::NowFront(Bool flag) // mac platform only
{
  // Show the menuBar for this frame on becoming active
  if (flag)
    {
      wxWindow::gMouseWindow = NULL; // If the frame changes, force capture off
      
      if (wx_menu_bar)
	wx_menu_bar->Install(this);
      else {
	if (!close_menu_bar) {
	  wxREGGLOB(close_menu_bar);
	  close_menu_bar = new WXGC_PTRS wxMenuBar;
#ifndef OS_X
	  /* When a frame doesn't have a menubar, doMacInMenuBar
	     assumes that any menulelection is the close item. */
	  wxMenu *file = new WXGC_PTRS wxMenu();
	  file->Append(1, "Close\tCmd+W");
	  close_menu_bar->Append(file, "File");
#endif
	}
	close_menu_bar->Install(this);
      }
    }
}

void wxFrame::ShowAsActive(Bool flag)
{
  /* Find child to accept focus, if none already has the focus: */
  if (flag && !cFocusWindow && children) {
    wxChildNode *node;
    wxWindow *win;
    node = children->First();
    while (node) {
      win = (wxWindow *)(node->Data());
      if (win->WantsFocus() && win->CanAcceptEvent()) {
	cFocusWindow = win;
	break;
      }
      node = node->Next();
    }
  }

  /* Notify child with focus: */
  if (cFocusWindow) {
    if (flag) {
      if (!(cStyle & wxFLOAT_FRAME)
	  || !(cStyle & wxNO_CAPTION)) {
	TakeoverFocus();
	if (cFocusWindow)
	  cFocusWindow->OnSetFocus();
      }
    } else {
      ReleaseFocus();
      if (cFocusWindow)
	cFocusWindow->OnKillFocus();
    }
  }
}

void wxFrame::OnToolbarButton(void)
{
}

void wxFrame::SetFrameModified(Bool is_modified)
{
  SetWindowModified(GetWindowFromPort(cMacDC->macGrafPort()), is_modified);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Icon methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

void wxFrame::SetIcon(wxBitmap* wx_icon, wxBitmap* mask, int kind) { }		// not implemented

void wxFrame::Iconize(Bool iconize) {
  CollapseWindow(GetWindowFromPort(cMacDC->macGrafPort()), iconize);
}

Bool wxFrame::Iconized(void) { 
  return IsWindowCollapsed(GetWindowFromPort(cMacDC->macGrafPort()));
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Platform methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
WindowPtr wxFrame::macWindow(void)
{
  return cMacDC ? GetWindowFromPort(cMacDC->macGrafPort()) : NULL;
}

//-----------------------------------------------------------------------------
Bool wxFrame::IsMacWindow(void) { return TRUE; }

//-----------------------------------------------------------------------------
Bool wxFrame::IsVisible(void)
{
  if (cMacDC)
    return IsWindowVisible(GetWindowFromPort(cMacDC->macGrafPort()));
  else
    return FALSE;
}

//-----------------------------------------------------------------------------
void wxFrame::MacUpdateWindow(void)
{
}

//-----------------------------------------------------------------------------
void wxFrame::MacDrawGrowIcon(void)
{
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
char* wxFrame::GetTitle(void) // WCH: return type should be "const char*"
{
  Str255 theTitle;
  WindowPtr theMacWindow;

  theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());
  ::GetWTitle(theMacWindow, theTitle);
  return wxP2C(theTitle);
}

//-----------------------------------------------------------------------------
void wxFrame::SetTitle(char* title)
{
  WindowPtr theMacWindow;
  theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());
  {
    CFStringRef wtitle;
    wtitle = wxCFString(title);
    SetWindowTitleWithCFString(theMacWindow, wtitle);
    CFRelease(wtitle);
  }
}

void wxCheckRootFrame(WindowPtr w)
{
  if (w) {
    /* Hide root frame, if any, in case it's shown */
    if (wxRootFrame) {
      wxMacDC *dc;
      WindowPtr theMacWindow;
      dc = wxRootFrame->MacDC();
      theMacWindow = GetWindowFromPort(dc->macGrafPort());
      if (w != theMacWindow)
	::HideWindow(theMacWindow);
    }
  } else {
    /* If all frames are closed/minimized, it's time for the root window (if any) */
    if (wxRootFrame && !FrontNonFloatingWindow()) {
      wxMacDC *dc;
      WindowPtr theMacWindow;
      dc = wxRootFrame->MacDC();
      theMacWindow = GetWindowFromPort(dc->macGrafPort());
      ::ShowWindow(theMacWindow);
    }
  }
}

//-----------------------------------------------------------------------------
void wxFrame::Show(Bool show)
{
  WindowPtr theMacWindow;
  wxChildList *tlw;

  /* WARNING: Show(FALSE) might called when the frame is being destroyed.
     Don't issue any callbacks at all in that case, because it may be
     that a custodian is shutting down an eventspace, etc. */

  if (!show == cUserHidden) {
    if (show) {
      CGrafPtr graf;
      graf = cMacDC->macGrafPort();
      ::SelectWindow(GetWindowFromPort(graf));
    }
    return;
  }
  
  cUserHidden = !show;

  /* Don't try to adjust the screen's window list, because it
     doesn't keep a list of children. */
  if (0) {
    if (window_parent) {
      wxChildList *cl;
      cl = window_parent->GetChildren();
      cl->Show(this, show);
    }
    if (cParentArea) {
      wxChildList *cl;
      cl = cParentArea->Windows();
      cl->Show(this, show);
    }
  }
  tlw = wxTopLevelWindows(ContextWindow());
  tlw->Show(this, show);

  if (wxRootFrame == this)
    return;

  theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());
  if (show) {
    // If we're foremost, then activate before we show,
    // instead of waiting for the activate event. Otherwise,
    // showing an inactive window looks strange.
    {
      ProcessSerialNumber us, front;
      Boolean r = 0;
      GetCurrentProcess(&us);
      GetFrontProcess(&front);
      SameProcess(&us, &front, &r);
      if (r) {
	wx_activate_anyway = 1;
	Activate(TRUE);
      }
    }
#ifdef OS_X
    if (cSheetParent && !cSheetParent->sheet
	&& !(cSheetParent->GetWindowStyleFlag() & wxHIDE_MENUBAR)) {
      WindowPtr pwin;
      CGrafPtr graf;

      graf = cSheetParent->cMacDC->macGrafPort();
      pwin = GetWindowFromPort(graf);
      if (mred_current_thread_is_handler(context)
	  && !mred_in_restricted_context()) {
	/* Enable the paint callback, which must run in
	   a special trampoline mode; see update_if_in_handler
	   above. */
	cCanUpdateOnCallback = 10;
	wxHETShowSheetWindow(theMacWindow, pwin);
	cCanUpdateOnCallback = 0;
      } else {
	ShowSheetWindow(theMacWindow, pwin);
	Refresh();
      }
      cSheetParent->sheet = this;
      ChangeWindowAttributes(pwin, 0, kWindowCloseBoxAttribute);
      wxMacRecalcNewSize(FALSE); // recalc new position only
    } else { 
      if (mred_current_thread_is_handler(context)
	  && !mred_in_restricted_context()) {
	/* Enable the paint callback, which must run in
	   a special trampoline mode; see update_if_in_handler
	   above. */
	cCanUpdateOnCallback = 10;
	wxHETShowWindow(theMacWindow);
	cCanUpdateOnCallback = 0;
      } else {
	ShowWindow(theMacWindow);
	Refresh();
      }
    }
#else
    ShowWindow(theMacWindow);
#endif
      
    ::SelectWindow(theMacWindow); 

    if (cMacDC->currentUser() == this)
      /* b/c may be optimized for hidden: */
      cMacDC->setCurrentUser(NULL);

    wxCheckRootFrame(theMacWindow);
  } else {
    if (cFocusWindow) {
      ReleaseFocus();
      if (!wxsIsContextShutdown(context))
	cFocusWindow->OnKillFocus();
      cFocusWindow = NULL;
    }
#ifdef OS_X
    if (cSheetParent && (cSheetParent->sheet == this)) {
      WindowPtr pwin;
      CGrafPtr graf;

      ::HideSheetWindow(theMacWindow);
      cSheetParent->sheet = NULL;

      graf = cSheetParent->cMacDC->macGrafPort();
      pwin = GetWindowFromPort(graf);
      ChangeWindowAttributes(pwin, kWindowCloseBoxAttribute, 0);
    } else
#endif
      {
	::HideWindow(theMacWindow);
      }

    wxCheckRootFrame(NULL);
  }
}

wxFrame *wxFrame::GetSheetParent()
{
#ifdef OS_X
  if (cSheetParent)
    return NULL; /* No nested sheets */
  else if (cStyle & wxNO_CAPTION)
    return NULL; /* Sheets need a title bar */
  else if (cStyle & wxFLOAT_FRAME)
    return NULL; /* Don't use sheets on floating windows */
  else
#endif
    return this;
}

wxFrame *wxFrame::GetSheetChild()
{
#ifdef OS_X
  return sheet;
#else
  return NULL;
#endif
}

//-----------------------------------------------------------------------------
Bool wxFrame::IsFrontWindow(void)
{
  if (cStyle & wxFLOAT_FRAME) {
    return FALSE;
  } else {
    WindowPtr theMacWindow, front;
    theMacWindow = macWindow();
    if (theMacWindow) {
      front = FrontNonFloatingWindow();
      return (theMacWindow == front);
    } else
      return FALSE;
  }
}

//-----------------------------------------------------------------------------
Bool wxFrame::IsModal(void) { return cIsModal; } //cjc, mflatt

//-----------------------------------------------------------------------------
void wxFrame::MakeModal(Bool modal)
{
  cIsModal = modal;
  // wxbWindow::MakeModal(modal);
}

//-----------------------------------------------------------------------------
wxWindow* wxFrame::GetFocusWindow(void) { return cFocusWindow; }

//-----------------------------------------------------------------------------
void wxFrame::SetFocusWindow(wxWindow* window)
{
  if (window != cFocusWindow) {
    if (cFocusWindow && cActive) {
      ReleaseFocus();
      cFocusWindow->OnKillFocus();
    }
    cFocusWindow = window;
    if (window && cActive) {
      TakeoverFocus();
      window->OnSetFocus();
    }
  } else if (cFocusWindow && cActive && (current_focus_window != this)) {
    TakeoverFocus();
    cFocusWindow->OnSetFocus();
  }
}

void wxRegisterFocusWindow()
{
  wxREGGLOB(current_focus_window);
}

void wxFrame::TakeoverFocus()
{
  if (current_focus_window) {
    if (current_focus_window->context == context) {
      current_focus_window->Unfocus();
    } else {
      MrEdQueueUnfocus(current_focus_window);
      current_focus_window = NULL;
    }
  }
  current_focus_window = this;
}

void wxFrame::ReleaseFocus()
{
  if (current_focus_window == this)
    current_focus_window = NULL;
}

void wxFrame::Unfocus()
{
  ReleaseFocus();
  if (cFocusWindow && cActive) {
    cFocusWindow->OnKillFocus();
    if (cStyle & wxFLOAT_FRAME)
      cFocusWindow = NULL;
  }
}

wxFrame *wxGetFocusFrame()
{
  return current_focus_window;
}

//-----------------------------------------------------------------------------

void wxFrame::LoadAccelerators(char* table) { } // Not Applicable for Mac platform

//-----------------------------------------------------------------------------
void wxFrame::Paint(void)
{
  CWindowPtr w;
  w = GetWindowFromPort(cMacDC->macGrafPort());
  DrawControls(w);
}

RgnHandle wxFrame::GetCoveredRegion(int x, int y, int w, int h)
{
  return NULL;
}

//-----------------------------------------------------------------------------
void wxFrame::OnChar(wxKeyEvent *event)
{
  if (cFocusWindow) {
    if (cFocusWindow != this) // kludge to prevent infinite loop
      cFocusWindow->OnChar(event);
  }
}

void wxFrame::OnEvent(wxMouseEvent *event)
{
  if (cStyle & wxMETAL) {
    if (event->ButtonDown(1)) {
      int x = event->x, y = event->y;
      Point start;
      start.h = x;
      start.v = y;
      SetCurrentDC();
      LocalToGlobal(&start);
      DragFrame(start);
      return;
    }
  }
  wxbFrame::OnEvent(event);
}

void wxFrame::DragFrame(Point where)
{
  RgnHandle screen;
  Rect dragBoundsRect;
  CWindowPtr window;

  window = GetWindowFromPort(cMacDC->macGrafPort());
  
  screen = GetGrayRgn();
  GetRegionBounds(screen, &dragBoundsRect);

  wxTracking();
  
  DragWindow(window, where, &dragBoundsRect);
  
  wxMacRecalcNewSize(FALSE);
}

//-----------------------------------------------------------------------------
void wxFrame::Enable(Bool enable)
{
  wxWindow::Enable(enable);
  // Enable/disbale menubar
  if (wx_menu_bar)
    wx_menu_bar->Install(this);	
}


wxFrame *wxFrame::GetRootFrame()
{
  return this;
}

ControlHandle wxFrame::GetRootControl(void)
{
  return cMacControl;
}

//-----------------------------------------------------------------------------

OSErr wxFrame::OnDrag(DragRef theDrag)
{
  Bool accepted = 0;
  UInt16 n, i;
  int x, y, w, h;
  DragItemRef ref;
  Rect bounds;
  wxChildNode *node;
  wxWindow *win;

  if (CountDragItems(theDrag, &n) == noErr) {
    for (i = 0; i < n; i++) {
      if (GetDragItemReferenceNumber(theDrag, i + 1, &ref) == noErr) {
	GetDragItemBounds(theDrag, ref, &bounds);

	node = drag_targets->First();
	while (node) {
	  int hit;
	  win = (wxWindow *)(node->Data());
	  if (win->CanAcceptEvent()) {
	    if (win == this)
	      hit = 1;
	    else {
	      win->GetPosition(&x, &y);
	      win->GetSize(&w, &h);
	      hit = !((x + w < bounds.left) || (x > bounds.right)
		      || (y + h < bounds.top) || (y > bounds.bottom));
	    }
	    if (hit) {
	      HFSFlavor hfs;
	      Size sz;
	      sz = sizeof(HFSFlavor);
	      if (GetFlavorData(theDrag, ref, 'hfs ', &hfs, &sz, 0) == noErr) {
		char *f;
		f = scheme_mac_spec_to_path(&hfs.fileSpec);
		MrEdQueueDrop(win, f);
		accepted = 1;
	      }
	      node = NULL;
	    } else
	      node = node->Next();
	  } else
	    node = node->Next();
	}
      }
    }
  }

  if (!accepted)
    return dragNotAcceptedErr;
  else
    return noErr;
}

static OSErr receiveHandler(WindowRef theWindow,  
			    void *handlerRefCon,  
			    DragRef theDrag)
{
  wxFrame *f;
  long refcon;

  refcon = GetWRefCon(theWindow);
  
  f = (wxFrame*)GET_SAFEREF(refcon);

  return f->OnDrag(theDrag);
}

void wxFrame::AddDragAccept(wxWindow *target, Bool on)
{
  CGrafPtr graf;
  WindowRef win;
  
  graf = cMacDC->macGrafPort();
  win = GetWindowFromPort(graf);
  
  if (!drag_targets) {
    if (!on)
      return;

    InstallReceiveHandler(receiveHandler, win, NULL);

    drag_targets = new WXGC_PTRS wxChildList();
  }

  if (!on) {
    drag_targets->DeleteObject(target);
    if (!drag_targets->Number()) {
      drag_targets = NULL;
      RemoveReceiveHandler(receiveHandler, win);
    }
  } else {
    drag_targets->Append(target);
    drag_targets->Show(target, FALSE); /* make link weak */
  }
}

long wxFrame::GetWindowHandle()
{
  CGrafPtr graf;
  WindowRef win;
  
  graf = cMacDC->macGrafPort();
  win = GetWindowFromPort(graf);

  return (long)win;
}

void wxFrame::OnMDIActivate(Bool flag)
{
}
