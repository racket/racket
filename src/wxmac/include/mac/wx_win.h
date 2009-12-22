/////////////////////////////////////////////////////////////////////////////// 
// File:	wx_win.h 
// Purpose:	wxWindow class declaration (Macintosh version). 
// Author:	Bill Hale 
// Created:	1994 
// Updated:	 
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved. 
/////////////////////////////////////////////////////////////////////////////// 

#ifndef wx_winh
#define wx_winh


#include "wb_win.h"
/*#include "wx_screen.h" */
#include "wxDirection.h"
#include "wxScrollData.h"
#if defined(WX_CARBON)
# ifdef OS_X
#  include <Carbon/Carbon.h>
# else
#  include <Carbon.h>
# endif
#else
#	include <Menus.h>
#	include <QuickDraw.h>
#	include <Fonts.h>
#	include <Events.h>
#	include <Dialogs.h>
#	include <TextEdit.h>
#	include	<Scrap.h>
#	include	<ToolUtils.h>
#endif
/*
 * Base class for frame, panel, canvas, panel items, dialog box.
 * and screen (for the mac)
 */

#ifdef IN_CPROTO
typedef       void* wxWindow ;
#else

extern int SetOriginX;
extern int SetOriginY;

class wxMacDC;
class wxFrame;
class wxBrush;
class wxArea;
class wxMargin;
class wxPanel;
class wxScroll;
class wxScreen;

extern void wxTracking();

class wxWindow: public wxbWindow
{
  /*============================================================================= */
  /* Public variables */
  /*============================================================================= */
 public:

  static wxWindow* gMouseWindow; /* mac platform only (window that has captured mouse) */

  void *refcon;

  /*============================================================================= */
  /* Protected variables */
  /*============================================================================= */
 protected:

  Bool 		cColour;	/* use colour for this window? */
  Bool		cActive;	/* active window is "highlighted" (mac platform only) */
  Bool		cEnable;	/* enabled window accepts mouse/keyboard events */
  int           internal_gray; /* how many enclosing windows are disabled? */
  Bool		cHidden;	/* Hidden? */
  Bool		cUserHidden;	/* Hidden because user asked (not just inherited)? */
  Bool        cGrandcursor;	/* Skip parent for retriving the effective cursor */
  long		cStyle;		/* mac platform only */
  Direction	cGravitate;	/* mac platform only */
  Direction	cJustify;	/* mac platform only */
  wxWindow* 	window_parent; 	/* Each window always knows its parent */
  wxChildList* children;	/* Window's children */
  wxScroll* 	cScroll;	/* for scroll data and scroll synchronization */
  wxMacDC*	cMacDC;		/* mac platform only */
  wxBrush* 	cBrush;		/* foreground brush */
  wxBrush* 	cEraser;	/* background brush */
  Pattern 	cMacPattern; 	/* mac platform only (temp work pattern) */

  /* For window rectangle */
  int 		cWindowX;
  int 		cWindowY;
  int 		cWindowHeight;
  int 		cWindowWidth;
 	
  /* For window area */
  wxArea* 	cParentArea; 	/* mac platform only */
  wxList* 		cAreas;	/* mac platform only */
  wxArea* 	cClientArea; 	/* mac platform only */

  ControlHandle cMacControl;

  ControlHandle cPaintControl;
  int           control_inset_extent, control_opaque;

  /*============================================================================= */
  /* Public methods */
  /*============================================================================= */
 public:

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Constructors */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  wxWindow(void);

  wxWindow			/* Constructor (for screen window) */
    (
     char*		windowName,
     int 		x,
     int			y,
     int			width,
     int			height,
     long		style
     );

  wxWindow			/* Constructor (given parentScreen; i.e., this is frame) */
    (
     char*		windowName,
     wxScreen*	parentScreen,
     int 		x,
     int			y,
     int			width,
     int			height,
     long		style
     );

  wxWindow			/* Constructor (given parentArea) */
    (
     char*		windowName,
     wxArea*		parentArea,
     int 		x,
     int			y,
     int			width,
     int			height,
     long		style
     );

  wxWindow			/* Constructor (given parentWindow) */
    (
     char*		windowName,
     wxWindow*	parentWindow,
     int 		x,
     int			y,
     int			width,
     int			height,
     long		style
     );

  wxWindow			/* Constructor (given objectType; i.e., menu or menuBar) */
    (
     char*		windowName
     );

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Destructor */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  ~wxWindow(void);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Geometry methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  int Width(void);		/* mac platform only */
  int Height(void);		/* mac platform only */
  wxMargin Margin(wxArea* outerArea); /* mac platform only */
  wxMargin Margin(wxWindow* outerWindow); /* mac platform only */
  virtual void GetPosition(int* windowX, int* windowY);
  virtual void GetSize(int* width, int* height);
  virtual void GetClientSize(int* width, int* height); /* Size client can use */
  virtual void ClientToScreen(int* x, int* y);
  virtual void ScreenToClient(int* x, int* y);
  virtual void ClientToLogical(int* x, int* y); /* mac platform only; testing */

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Sizing methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  void SetWidthHeight(int width, int height); /* mac platform only */
  virtual void SetSize(int x, int y, int width, int height, int flags = wxSIZE_AUTO); /* mac platform only */
  virtual void DoSetSize(int x, int y, int width, int height); /* mac platform only */
  virtual void SetClientSize(int newClientWidth, int newClientHeight);
  void GravitateJustify(Direction gravitate, Direction justify,
			int left, int top, int right, int bottom); /* mac platform only */
  virtual void Fit(void);	/* mac platform only */
  virtual void SetPhantomSize(int w, int h);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Device context methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  wxMacDC* MacDC(void);		/* mac platform only */

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Tree (windows and areas) methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  wxArea* ParentArea(void);	/*mac platform only */
  wxList* Areas(void);		/*mac platform only */
  virtual wxArea* ClientArea(void); /* mac platform only */
  virtual wxWindow* GetParent(void);
  virtual wxChildList* GetChildren(void);
  virtual wxWindow* GetGrandParent(void);
  virtual wxFrame* GetRootFrame(void); /* mac platform only */

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Scroll methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  virtual void AddChildScrollWindow(wxWindow* childScrollWindow); /* mac platform only */
  wxScroll* GetScroll(void);	/* mac platform only */
  virtual void SetScrollData	/* Must override if window scrolls */
    (
     wxScrollData*		scrollData,
     wxWhatScrollData	whatScrollData,
     wxScrollEvent*		e
     );

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Mouse methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  void CaptureMouse(void);
  void ReleaseMouse(void);

  int MaybeMetalDrag(wxMouseEvent *evt);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Keyboard methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  virtual void SetFocus(void);
  virtual void OnSetFocus(void);
  virtual void OnKillFocus(void);
  virtual Bool WantsFocus(void);
  virtual Bool AcceptsExplicitFocus(void);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Quill methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  virtual void SetFont(wxFont* theFont); /* mac platform only */
  double GetCharHeight(void);
  double GetCharWidth(void);
  void GetTextExtent(const char* string, double* x, double* y, double* descent = NULL,
		     double* externalLeading = NULL, wxFont *thefont = NULL,
		     Bool use16 = FALSE);
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Other methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  Bool PopupMenu(wxMenu* menu, double x, double y);
  wxCursor* SetCursor(wxCursor* cursor);
  void SetColourMap(wxColourMap* cmap);
  virtual Bool IsMacWindow(void); /* mac platform only */
  virtual void DoPeriodicAction(void); /* mac platform only */
  /*+++++ Begin Macintosh Platform only +++++ */
  virtual void Enable(Bool Flag);
  Bool IsEnable(void);
  Bool CanAcceptEvent(void);
  Direction GetGravitate(void);
  void SetGravitate(Direction direction);
  Direction GetJustify(void);
  void SetJustify(Direction direction);
  virtual void DragAcceptFiles(Bool accept);
  void SetEraser(wxBrush* eraser);
  inline wxBrush *GetEraser() { return cEraser; }
  virtual void DestroyFocus();	/* tom (fettig@dfki.uni-sb.de) */
  /*+++++ End Macintosh Platform only +++++ */

  Bool		OS_Active(void); /* should this window be shown as enabled right now? */
  void InitInternalGray(void);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Sizing methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  virtual void OnWindowDSize(int dW, int dH, int dX = 0, int dY = 0); /* mac platform only */
  virtual void OnAreaDSize(int dW, int dH, int dX = 0, int dY = 0); /* mac platform only */
  virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY); /* mac platform only */

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Device context methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  int SetCurrentMacDCNoMargin(void); /* mac platform only */
  int SetCurrentMacDC(void);	/* mac platform only */
  int SetCurrentDC(void);	/* mac platform only */
  virtual void ReleaseCurrentDC(int really = 0);
  virtual void MacSetBackground(void); /* mac platform only */
  virtual void SetForeground(void); /* mac platform only */
  virtual void SetTextInfo(void); /* mac platform only */
  virtual void GetClipRect(wxArea* area, Rect* clipRect); /* mac platform only */
  virtual RgnHandle GetCoveredRegion(int x, int y, int w, int h);

  void GetWinOrigin(int *x, int *y);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Tree (windows and areas) methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  virtual void AddChild(wxObject* child); /* Adds reference to the child object */
  virtual void OnDeleteChildWindow(wxWindow* childWindow); /* mac platform only */
  virtual void OnDeleteChildArea(wxArea* childArea); /* mac platform only */
  virtual void DestroyChildren(void); /* Removes and destroys all children */

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Activate methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  virtual void Activate(Bool flag); /* mac platform only */
  virtual void ShowAsActive(Bool flag); /* mac platform only */
  virtual void OnActivate(Bool flag); /* mac platform only */

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Mouse methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  virtual Bool SeekMouseEventArea(wxMouseEvent *mouseEvent, int *metal_drag_ok);
  virtual Bool AdjustCursor(int mouseX, int mouseY);
  virtual void AdjustMetalDragOk(int *metal_drag_ok);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Other methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  virtual void Paint(void);
  virtual void PaintRgn(RgnHandle rgn);
  virtual void Refresh(void);
  virtual void RefreshIfUpdating(void);
	
  virtual Bool PreOnEvent(wxWindow *win, wxMouseEvent *event);
  virtual Bool PreOnChar(wxWindow *win, wxKeyEvent *event);

  virtual void OnEventCheckMetal(wxMouseEvent *event, int metal_drag_ok);

  virtual void Show(Bool);

  virtual void DoShow(Bool);
  Bool CanShow(Bool);
	
  inline Bool IsHidden(void) { return cHidden; }
  inline Bool IsUserHidden(void) { return cUserHidden; }
	
  virtual Bool IsShown(void) { return !IsUserHidden(); }
	
  /* InternalGray is part of the command hierarchy: calling InternalGray
   * increases the internal_gray index of this and all enclosed windows.
   * Only when the internal_gray index is zero is a window OS_activated.
   */
  void InternalGray(int gray_amt);
  Bool IsGray(void);
	
  /* The Highlight method is used to make an item look "pressed." This is
   * the case, for instance, when there's been a mouse-down over a button, 
   * but no mouse-up yet.
   */
  virtual void Highlight(Bool flag);
  /* Track is used to track the mouse between a mouse-down and a mouse-up.
   * Track is used only when there is no mac ControlHandle, and thus the
   * OS tracker cannot be used.
   */
  int Track(Point start);

  void ForEach(void (*foreach)(wxWindow *w, void *data), void *data);
	
  virtual wxCursor *GetEffectiveCursor(void);
	
  Bool GetsFocus();

  virtual void MaybeMoveControls();

  virtual wxWindow *EnterLeaveTarget();

  virtual ControlHandle GetRootControl(void);

  void FlushDisplay(void);

  void CreatePaintControl(int inset_extent = -1, Bool opaque = FALSE);
  void GetPaintControlRegion(RgnHandle rgn, Bool opaquePart);

  virtual long GetWindowHandle();
	
 protected:
  /* ChildrenInternalGray is a local abstraction which calls
   * InternalGray for each of the children of this window.
   */
  virtual void ChildrenInternalGray(int gray_amt);
  /* ChangeToGray is the procedure which physically changes
   * the window gray, OS_deactivating it if necessary.
   */
  virtual void ChangeToGray(Bool Gray);
	
  /* MaybeMoveControl is used by panels to notify sub-panels and controls
   * that a super-panel's absolute position has changed.  This is necessary
   * on the mac because the OS controls are located relative to the frame, and
   * the OS needs to be notified whenever they move relative to the frame,
   * even if they haven't moved relative to their enclosing panel.
   *
   * Needed Invariant: Panels contain only controls and other panels.
   */

  /*============================================================================= */
  /* Private methods */
  /*============================================================================= */
 private:

  void InitDefaults(void);	/* used by constructors */
  void InitWindowPostion(int x, int y); /* used by constructors */

  /*============================================================================= */
  /* Friend classes */
  /*============================================================================= */
 private:

  friend class wxArea;
};

void wxFlushMacDisplay(void);

#endif /* IN_CPROTO */
#endif
