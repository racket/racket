/////////////////////////////////////////////////////////////////////////////// 
// File:	wx_frame.h 
// Purpose:	wxFrame declaration (Macintosh version) 
// Author:	Bill Hale 
// Created:	1994 
// Updated:	 
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved. 
/////////////////////////////////////////////////////////////////////////////// 

#ifndef wx_frameh
#define wx_frameh

#include "wb_frame.h"

#ifdef IN_CPROTO
typedef       void* wxFrame;
#else

class wxMenuBar;
class wxCommandEvent;
class wxPanel;
class wxMessage;
class wxDialogBox;

class wxFrame: public wxbFrame
{
  /*============================================================================= */
  /* Protected variables */
  /*============================================================================= */
 protected:

  Bool		cMaximized;
  wxPanel*	cStatusPanel;
  wxMessage*	cStatusText;
  wxArea* 	cPlatformArea;
  wxArea* 	cContentArea;
  wxArea* 	cControlArea;
  wxWindow*	cFocusWindow;
  Bool		cIsModal;

  Rect          size_limits;

  wxFrame*      cSheetParent;
  wxFrame*      sheet;	/* child sheet */

  wxChildList *drag_targets;

  ControlHandle bgControl;

  /*============================================================================= */
  /* Public methods */
  /*============================================================================= */
 public:
  int         cBusyCursor;
  Bool        is_in_update;
  Bool	      cCanUpdateOnCallback;

  wxDialogBox* cDialogPanel;

  void NowFront(Bool on);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Constructors */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  wxFrame			/* Constructor (for frame window) */
    (
     wxFrame*	parentFrame,	/* this is ignored */
     char*		windowTitle,
     int 		x = wxDEFAULT_POSITION,
     int			y = wxDEFAULT_POSITION,
     int			width = -1,
     int			height = -1,
     long		style = wxSDI,
     char*		windowName = "frame",
     WXTYPE		objectType = wxTYPE_FRAME
     );

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Destructor */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  ~wxFrame(void);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Geometry methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  wxArea* PlatformArea(void);
  wxArea* ContentArea(void);
  wxArea* ControlArea(void);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Sizing methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  virtual void DoSetSize(int x, int y, int width, int height);
  void Maximize(Bool maximize);
  Bool IsMaximized();

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Status line methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  void CreateStatusLine(int number = 1, char* name = "status_line");
  void SetStatusText(char* text, int number = 0);
  void SetStatusEraser(wxBrush* b); /* tom */

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Menubar methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  void SetMenuBar(wxMenuBar* menu_bar);
  void Command(int id);		/* Call this to simulate a menu command */
  virtual void OnMenuClick(void);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Icon methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  void SetIcon(wxBitmap* icon, wxBitmap *mask = NULL, int kind = 0);
  void Iconize(Bool iconize);
  Bool Iconized(void);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Platform methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  Bool IsVisible(void);
  void MacUpdateWindow(void);
  void MacDrawGrowIcon(void);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Other methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  char* GetTitle(void);
  void SetTitle(char* title);
  void Show(Bool show);
  Bool IsFrontWindow(void);
  virtual Bool IsModal(void);
  void MakeModal(Bool on);
  wxWindow* GetFocusWindow(void);
  void SetFocusWindow(wxWindow* window);
  void LoadAccelerators(char* table);
  void Enable(Bool on);
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Other methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  virtual void Paint(void);
  virtual void OnEvent(wxMouseEvent *event);
  virtual void OnChar(wxKeyEvent *event);
  virtual void OnCommandEvent(wxCommandEvent *event) {};

  virtual void OnMDIActivate(Bool flag);
      
  void DragFrame(Point startpt);

  virtual RgnHandle GetCoveredRegion(int x, int y, int w, int h);

  virtual wxFrame* GetRootFrame(void); /* mac platform only */

  virtual ControlHandle GetRootControl(void);

  WindowPtr macWindow(void);

  wxFrame *GetSheetParent(); /* NULL or self */
  wxFrame *GetSheetChild(); /* NULL or child frame  */

  void DesignateRootFrame(void);
  virtual void OnToolbarButton(void);
  void SetFrameModified(Bool is_modified);

  void EnforceSize(int minw, int minh, int maxw, int maxh, int incw=1, int inch=1);
  void GetSizeLimits(Rect *r);

  void Unfocus();

  OSErr OnDrag(DragRef d);
  void AddDragAccept(wxWindow *target, Bool on);

  virtual long GetWindowHandle();

  /*============================================================================= */
  /* Protected methods */
  /*============================================================================= */
 protected:

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Sizing methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  void wxMacRecalcNewSize(Bool resize = TRUE);

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Menubar methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  void ProcessCommand(int id);
  virtual void ShowAsActive(Bool flag); /* mac platform only */

  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  /* Platform methods */
  /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  virtual Bool IsMacWindow(void);
  void wxMacStartDrawing(CGrafPtr  * oldPort, GDHandle * oldGD);
  void wxMacStopDrawing(CGrafPtr oldPort, GDHandle oldGD);
  Rect wxMacGetContRect(void);
  Rect wxMacGetStrucRect(void);
  inline wxDialogBox* wxMacGetDialog(void) {return cDialogPanel;} 

  void TakeoverFocus();
  void ReleaseFocus();

  /*============================================================================= */
  /* Private methods */
  /*============================================================================= */
 private:

  void InitDefaults(void);	/* used by constructors */

  /*============================================================================= */
  /* Friend classes */
  /*============================================================================= */
 private:

  friend class wxApp;
  friend class wxDialogBox;
};

extern wxFrame *wxRootFrame;
extern wxFrame *wxGetFocusFrame();

#endif /* IN_CPROTO */
#endif /* wx_frameh */
