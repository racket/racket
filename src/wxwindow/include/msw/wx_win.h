/*
 * File:	wx_win.h
 * Purpose:	
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

/*
 * Purpose:  wxWindow class declaration. Base class for all windows and
 *           panel items.
 */


#ifndef wx_winh
#define wx_winh

#include "wb_win.h"

#include <windows.h>

/*
 * Base class for frame, panel, canvas, panel items, dialog box.
 *
 */

class wxWindow: public wxbWindow
{
 public:
  HANDLE ms_handle;                   // For menus and hwnds: using 'handle'
                                      // causes too many compiler messages
  int wxWinType;                      // For knowing how to delete the object
  int windows_id;
  Bool winEnabled;
  short internal_gray_disabled;

  RECT updateRect;             // Bounding box for screen damage area
  HRGN updateRgn;                  // NT allows access to the rectangle list

  wxWindow *focusWindow;

  virtual BOOL MSWCommand(UINT param, WORD id);
  wxWindow *FindItem(int id);
  wxWindow *FindItemByHWND(HWND hWnd);
  virtual void PreDelete(HDC dc);              // Allows system cleanup
  HWND GetHWND(void);

  // Constructors/Destructors
  wxWindow(void);
  virtual ~wxWindow(void);

  virtual Bool Show(Bool show);
  virtual wxCursor *SetCursor(wxCursor *cursor);

  virtual void GetTextExtent(const char *string, double *x, double *y,
			     double *descent = NULL, double *externalLeading = NULL, 
			     wxFont *theFont = NULL, Bool use16bit = FALSE);

  virtual void GetSize(int *width, int *height);
  void GetPosition(int *x, int *y);
  void GetClientSize(int *width, int *height); // Size client can use
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetSize(int width, int height) { wxbWindow::SetSize(width, height); }
  void SetClientSize(int width, int size);
  void ClientToScreen(int *x, int *y);
  void ScreenToClient(int *x, int *y);
  void SetFocus(void);
  void CaptureMouse(void);
  void ReleaseMouse(void);
  void Enable(Bool enable);
  void DragAcceptFiles(Bool accept);
  inline void SetTitle(char *WXUNUSED(title)) {};
  inline void Fit(void) {};
  inline void Centre(int WXUNUSED(direction)) {};
  Bool PopupMenu(wxMenu *menu, double x, double y);

  void InternalEnable(Bool enable, Bool gray = FALSE);
  Bool IsGray(void);
  virtual void ChangeToGray(Bool gray);
  void InternalGrayChildren(Bool gray);

  void Refresh(void);

  void OnScroll(wxScrollEvent *event);
  void SetScrollPos(int orient, int pos);
  void SetScrollRange(int orient, int range);
  void SetScrollPage(int orient, int page);
  int GetScrollPos(int orient);
  int GetScrollRange(int orient);
  int GetScrollPage(int orient);

  virtual void OnCalcScroll();

  wxWindow *GetTopLevel();
  void DoEnableWindow(int on);

  // The default implementation sets scroll ranges, if any
  void OnSize(int w, int h);

  // Internal function to update scrollbars
  void DoScroll(wxScrollEvent *event);

  // Calculate scroll increment
  int CalcScrollInc(wxScrollEvent *event);

  Bool CallPreOnEvent(wxWindow *, wxMouseEvent *);
  Bool CallPreOnChar(wxWindow *, wxKeyEvent *);  

  virtual Bool PreOnEvent(wxWindow *, wxMouseEvent *);
  virtual Bool PreOnChar(wxWindow *, wxKeyEvent *);

  virtual wxWindow *PreWindow();

  virtual wxWindow *FindFocusWindow();

  void InitEnable();
};

// Window specific (so far)
wxWindow *wxGetActiveWindow(void);

int wxCharCodeMSWToWX(int keySym);
int wxCharCodeWXToMSW(int id, Bool *IsVirtual);

int wxEventTrampoline(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam, 
		      LRESULT *res, WNDPROC proc);

#endif
