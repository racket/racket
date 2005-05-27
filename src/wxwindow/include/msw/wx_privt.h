/*
 * File:	wx_privt.h
 * Purpose:	Private class declarations.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_privth
#define wx_privth

#include "common.h"
#include "wx_obj.h"


class wxWindow;

class wxWnd : public wxObject
{
public:
    UINT last_msg;
    WPARAM last_wparam;
    LPARAM last_lparam;
    char x_scrolling_enabled, y_scrolling_enabled, x_scroll_visible, y_scroll_visible;
    Bool calcScrolledOffset; // If TRUE, wxCanvasDC uses scrolled offsets

    int xscroll_pixels_per_line;
    int yscroll_pixels_per_line;
    int xscroll_lines;
    int yscroll_lines;
    int xscroll_lines_per_page;
    int yscroll_lines_per_page;
    int xscroll_position;
    int yscroll_position;
    int last_x_pos;
    int last_y_pos;
    int last_event;
    HWND handle;
    HMENU hMenu; // Menu, if any

    Bool mouse_in_window;
    Bool is_canvas;
    Bool is_panel;
    Bool is_dialog;
    HDC cdc;

    wxWindow *wx_window;

    HDC ldc;
    int dc_count;

    wxWnd(void);
    ~wxWnd(void);

    HDC GetHDC(void);
    void ReleaseHDC(void);

    void Create(wxWnd *parent, char *wclass, wxWindow *wx_win, char *title,
               int x, int y, int width, int height,
               DWORD style, char *dialog_template = NULL,
               DWORD exendedStyle = 0);

    // Calculates the position of a point on the window
    // taking into account the position of scrollbars.
    // Windows doesn't automatically reflect the position of the
    // scrollbars - (0, 0) is always the top left of the visible window,
    // whereas in XView, (0, 0) moves according to scrollbar positions.
    virtual void CalcScrolledPosition(int x, int y, int *xx, int *yy);

    // Actually defined in wx_canvs.cc since requires wxCanvas declaration
    void DeviceToLogical(double *x, double *y);

    // Calculate logical (scroll-bar/scaling aware) position from
    // device (pixel) position
    virtual void CalcUnscrolledPosition(int x, int y, double *xx, double *yy);

    // Handlers
    virtual void OnCreate(LPCREATESTRUCT cs);
    virtual BOOL OnPaint(void);
    virtual HICON OnQueryDragIcon(void) { return 0; }
    virtual void OnSize(int x, int y, UINT flag);
    virtual void OnHScroll(WORD nSBCode, WORD pos, HWND control);
    virtual void OnVScroll(WORD nSBCode, WORD pos, HWND control);
    virtual BOOL OnCommand(WORD id, WORD cmd, HWND control);
    virtual BOOL OnEraseBkgnd(HDC pDC);
    virtual void OnMenuSelect(WORD item, WORD flags, HMENU sysmenu);
    virtual void OnMenuClick(WPARAM mnu);
    virtual BOOL OnClose(void);
    virtual BOOL OnDestroy(void);
    virtual BOOL OnSetFocus(HWND wnd);
    virtual BOOL OnKillFocus(HWND wnd);
    virtual void OnDropFiles(WPARAM wParam);

    virtual int OnButton(int x, int y, UINT flags, int evttype, int for_nc = 0);
    virtual int OnMouseMove(int x, int y, UINT flags, int for_nc = 0);

    virtual void OnMouseEnter(int x, int y, UINT flags);
    virtual void OnMouseLeave(int x, int y, UINT flags);

    virtual void OnChar(WORD wParam, LPARAM lParam, Bool isASCII = FALSE, Bool isRelease = FALSE);

    virtual BOOL OnActivate(BOOL flag, BOOL minimized, HWND activate);
    virtual BOOL OnMDIActivate(BOOL flag, HWND activate, HWND deactivate);

    virtual LONG DefWindowProc(UINT nMsg, WPARAM wParam, LPARAM lParam);
    virtual LONG Propagate(UINT nMsg, WPARAM wParam, LPARAM lParam);
    virtual BOOL ProcessMessage(MSG* pMsg);
    virtual void DestroyWindow(void);

    virtual BOOL NCPaint(WPARAM wParam, LPARAM lParam, LONG *result);
    virtual void OnWinThemeChange();

    virtual void GetMinMaxInfo(MINMAXINFO *mmi);

    // Detach "Window" menu from menu bar so it doesn't get deleted
    void DetachWindowMenu(void);
};


/*
 * This is a Windows 3 subwindow - panel or canvas
 */

class wxSubWnd : public wxWnd
{
public:
    wxSubWnd(wxWnd *parent, char *wclass, wxWindow *wx_win,
             int x, int y, int width, int height,
             DWORD style, char *dialog_template = NULL,
             DWORD exendedStyle = 0);
    ~wxSubWnd(void);

    // Handlers
    BOOL OnPaint(void);
    void OnSize(int x, int y, UINT flag);
    BOOL OnCommand(WORD id, WORD cmd, HWND control);

    // Canvas-type events
    void OnHScroll(WORD nSBCode, WORD pos, HWND control);
    void OnVScroll(WORD nSBCode, WORD pos, HWND control);
};

class wxCanvasWnd : public wxSubWnd
{
public:
  HANDLE control_theme;

  wxCanvasWnd(wxWnd *parent, wxWindow *wx_win,
              int x, int y, int width, int height,
              DWORD style, DWORD ex_style);

  // Handlers
  BOOL OnEraseBkgnd(HDC pDC);
  BOOL OnPaint(void);

  BOOL NCPaint(WPARAM wParam, LPARAM lParam, LONG *result);
  void OnWinThemeChange();
};

class wxFrameWnd : public wxWnd
{
public:
    Bool iconized;
    HICON icon;
    HICON bigIcon;
    HICON defaultIcon;
    MINMAXINFO mmi;

    wxFrameWnd(void);
    wxFrameWnd(wxWnd *parent, char *wclass, wxWindow *wx_win, char *title,
                   int x, int y, int width, int height,
                   long style);
    ~wxFrameWnd(void);

    // Handlers
    BOOL OnPaint(void);
    HICON OnQueryDragIcon(void);
    void OnSize(int x, int y, UINT flag);
    BOOL OnCommand(WORD id, WORD cmd, HWND control);
    BOOL OnClose(void);
    void OnMenuSelect(WORD item, WORD flags, HMENU sysmenu);
    void OnMenuClick(WPARAM mnu);
    BOOL ProcessMessage(MSG *msg);
    virtual void GetMinMaxInfo(MINMAXINFO *mmi);
};

class wxStatusWnd : public wxWnd
{
public:
    char *status_text;
    int height;
    HBRUSH light_grey_brush;

    wxStatusWnd(wxFrameWnd *parent, int the_height);
    ~wxStatusWnd(void);

    BOOL OnPaint(void);
};

class wxMDIFrame : public wxFrameWnd
{
public:
    HWND client_hwnd;
    wxWnd *current_child;
    HMENU window_menu;
    Bool parent_frame_active; // TRUE if MDI Frame is intercepting
                              // commands, not child

    wxMDIFrame(wxWnd *parent, wxWindow *wx_win, char *title=NULL,
                int x=-1, int y=-1, int width=-1, int height=-1, long style = 0);
    ~wxMDIFrame(void);

    void OnCreate(LPCREATESTRUCT cs);
    void OnSize(int x, int y, UINT);
    BOOL OnCommand(WORD id, WORD cmd, HWND control);
    void OnMenuSelect(WORD, WORD, HMENU);
    long DefWindowProc(UINT message, WPARAM wParam, LPARAM lParam);
    long Propagate(UINT nMsg, WPARAM wParam, LPARAM lParam);
    BOOL ProcessMessage(MSG *msg);
    BOOL OnEraseBkgnd(HDC pDC);
    BOOL OnDestroy(void);
};

class wxMDIChild : public wxFrameWnd
{
public:
    Bool active;
    
    wxMDIChild(wxMDIFrame *parent, wxWindow *wx_win, char *title=NULL,
                int x=-1, int y=-1, int width=-1, int height=-1, long style = 0);
    ~wxMDIChild(void);

    BOOL OnMDIActivate(BOOL bActivate, HWND, HWND);
    BOOL OnClose(void);
    void OnSize(int x, int y, UINT);
    BOOL OnCommand(WORD id, WORD cmd, HWND control);
    long DefWindowProc(UINT message, WPARAM wParam, LPARAM lParam);
    long Propagate(UINT nMsg, WPARAM wParam, LPARAM lParam);
    BOOL ProcessMessage(MSG *msg);
    void DestroyWindow(void);
};

#define         wxTYPE_XWND              1
#define         wxTYPE_HWND              2
#define         wxTYPE_HMENU             3
#define         wxTYPE_MDICHILD          4
#define VIEWPORT_EXTENT 1000

class wxFont;

void wxGetCharSize(HWND wnd, int *x, int *y,wxFont *the_font);
void wxSliderEvent(HWND control, WORD wParam, WORD pos);
wxWnd *wxFindWinFromHandle(HWND hWnd);
void wxScrollBarEvent(HWND hbar, WORD wParam, WORD pos);

extern HICON wxSTD_FRAME_ICON;
extern HICON wxSTD_MDIPARENTFRAME_ICON;
extern HICON wxSTD_MDICHILDFRAME_ICON;
extern HICON wxDEFAULT_FRAME_ICON;
extern HICON wxDEFAULT_MDIPARENTFRAME_ICON;
extern HICON wxDEFAULT_MDICHILDFRAME_ICON;
extern HFONT wxSTATUS_LINE_FONT;

#endif // wx_privth

