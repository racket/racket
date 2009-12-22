/*
 * File:	wx_messg.cc
 * Purpose:	Message item implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

static int icon_w, icon_h;
static HICON icons[3];

wxMessage::wxMessage(wxPanel *panel, char *label, int x, int y, long style, wxFont *_font, char *name):
  wxbMessage(panel, label, x, y, style, name)
{
  SetFont(_font);
  Create(panel, label, NULL, 0, x, y, style);
}

wxMessage::wxMessage(wxPanel *panel, wxBitmap *image, int x, int y, long style, wxFont *_font, char *name):
  wxbMessage(panel, image, x, y, style, name)
{
  SetFont(_font);
  Create(panel, NULL, image, 0, x, y, style);
}
  
wxMessage::wxMessage(wxPanel *panel, int iconID, int x, int y, long style, wxFont *_font, char *name):
  wxbMessage(panel, "<icon>", x, y, style, name)
{
  SetFont(_font);
  Create(panel, NULL, NULL, iconID, x, y, style);
}
  
Bool wxMessage::Create(wxPanel *panel, char *label, wxBitmap *image, int iconID, int x, int y, long style)
{
  wxWnd *cparent;
  HWND static_item;
  HICON icn = NULL;

  if (image) {
    if (!image->Ok() || (image->selectedIntoDC < 0))
      return Create(panel, "<bad-image>", NULL, 0, x, y, style);
    
    image->selectedIntoDC++;
    bm_label = image;
  } else if (iconID) {
    if (!icon_w) {
      wchar_t *name;

      name = new WXGC_ATOMIC wchar_t[1024];

      icon_w = GetSystemMetrics(SM_CXICON);
      icon_h = GetSystemMetrics(SM_CYICON);

      ::GetModuleFileNameW(NULL, name, 1023);
      icn = ExtractIconW(NULL, name, 0);
      icons[wxMSGICON_APP - 1] = (icn ? icn : LoadIcon(NULL, IDI_APPLICATION));
      icons[wxMSGICON_WARNING - 1] = LoadIcon(NULL, IDI_WARNING);
      icons[wxMSGICON_ERROR - 1] = LoadIcon(NULL, IDI_ERROR);
    }

    icn = icons[iconID - 1];
    if (icn)
      is_icon = TRUE;
    else {
      return Create(panel, "<bad-icon>", NULL, 0, x, y, style);
    }
  }

  panel->AddChild(this);
  wxWinType = wxTYPE_HWND;
  windowStyle = style;
  cparent = (wxWnd *)(panel->handle);

  if (image) {
    HBITMAP lbm;
    int nid;
    nid = NewId(this);
    static_item = wxwmCreateWindowEx(0, FafaStat, NULL,
				     FS_BITMAP | FS_X2 | FS_Y2 | WS_CHILD 
				     | WS_GROUP | WS_CLIPSIBLINGS
				     | ((style & wxINVISIBLE) ? 0 : WS_VISIBLE),
				     0, 0, 0, 0, cparent->handle, (HMENU)nid,
				     wxhInstance, NULL);
    
    lbm = image->GetLabelBitmap(1);
    SetBitmapDimensionEx(lbm,
			 image->GetWidth(),
			 image->GetHeight(),
			 NULL);
    SendMessage((HWND)static_item, WM_CHANGEBITMAP,
                  (WPARAM)0xFFFF,
                  (LPARAM)lbm);
  } else if (is_icon) {
    int nid;
    nid = NewId(this);
    static_item = wxwmCreateWindowEx(0, FafaStat, NULL,
				     FS_BITMAP | FS_X2 | FS_Y2 | WS_CHILD 
				     | WS_GROUP | WS_CLIPSIBLINGS
				     | ((style & wxINVISIBLE) ? 0 : WS_VISIBLE),
				     0, 0, 0, 0, cparent->handle, (HMENU)nid,
				     wxhInstance, NULL);
    
    SendMessage((HWND)static_item, WM_CHANGEICON,
		(WPARAM)0xFFFF,
		(LPARAM)icn);
  } else {
    int nid;
    nid = NewId(this);
    static_item = CreateWindowExW(0, L"wxSTATIC", wxWIDE_STRING(label),
				  STATIC_FLAGS | WS_CLIPSIBLINGS
				  | ((style & wxINVISIBLE) ? 0 : WS_VISIBLE),
				  0, 0, 0, 0, cparent->handle, (HMENU)nid,
				  wxhInstance, NULL);
  }

  ms_handle = (HANDLE)static_item;

  SubclassControl(static_item);

  if (!image)
    wxSetWinFont(font, ms_handle);

  panel->GetValidPosition(&x, &y);

  {
    int iw, ih;
    iw = (image ? image->GetWidth() : -1);
    ih = (image ? image->GetHeight() : -1);
    SetSize(x, y, 
	    (is_icon ? icon_w : iw), 
	    (is_icon ? icon_h : ih));
  }
  panel->AdvanceCursor(this);

  if (style & wxINVISIBLE)
    Show(FALSE);

  return TRUE;
}

wxMessage::~wxMessage(void)
{
  if (bm_label) {
    --bm_label->selectedIntoDC;
    bm_label->ReleaseLabel();
    bm_label = NULL;
  }
}

void wxMessage::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  int currentX, currentY;
  int actualWidth = width;
  int actualHeight = height;
  wchar_t buf[300];
  double current_width;
  double cyf;
  int ww, hh;

  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  GetWindowTextW((HWND)ms_handle, buf, 300);
  GetLabelExtent(wxStripMenuCodes(wxNARROW_STRING(buf)), &current_width, &cyf);

  GetSize(&ww, &hh);

  // If we're prepared to use the existing width, then...
  if (width == -1 && ((sizeFlags & wxSIZE_AUTO_WIDTH) != wxSIZE_AUTO_WIDTH))
    actualWidth = ww;
  else if (width == -1)
    actualWidth = (int)current_width;

  // If we're prepared to use the existing height, then...
  if (height == -1 && ((sizeFlags & wxSIZE_AUTO_HEIGHT) != wxSIZE_AUTO_HEIGHT))
    actualHeight = hh;
  else if (height == -1)
    actualHeight = (int)cyf;

  MoveWindow((HWND)ms_handle, x, y, actualWidth, actualHeight, TRUE);

  if (!((width == -1) && (height == -1)))
    OnSize(actualWidth, actualHeight);
}

void wxMessage::SetLabel(char *label)
{
  RECT rect;
  wxWindow *parent;
  POINT point;

  if (bm_label || is_icon)
    return;

  parent = GetParent();
  GetWindowRect((HWND)ms_handle, &rect);

  // Since we now have the absolute screen coords,
  // if there's a parent we must subtract its top left corner
  point.x = rect.left;
  point.y = rect.top;
  if (parent) {
    wxWnd *cparent = (wxWnd *)(parent->handle);
    ::ScreenToClient(cparent->handle, &point);
  }

  SetWindowTextW((HWND)ms_handle, wxWIDE_STRING(label));
}

void wxMessage::SetLabel(wxBitmap *bitmap)
{
  int x, y;
  int w, h;
  RECT rect;
  wxWindow *par;
  HBITMAP lbm;

  if (!bm_label || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;

  --bm_label->selectedIntoDC;
  bm_label->ReleaseLabel();
  bm_label = bitmap;
  bm_label->selectedIntoDC++;

  GetPosition(&x, &y);
  GetSize(&w, &h);
  rect.left = x; rect.top = y; rect.right = x + w; rect.bottom = y + h;

  lbm = bitmap->GetLabelBitmap(1);
  SetBitmapDimensionEx(lbm,
		       bitmap->GetWidth(),
		       bitmap->GetHeight(),
		       NULL);
  SendMessage((HWND)ms_handle, WM_CHANGEBITMAP,
	      (WPARAM)0xFFFF /*((bitmap->GetHeight()<<8)+bitmap->GetWidth())*/,
	      (LPARAM)lbm);
  
  par = GetParent();
  InvalidateRect(par->GetHWND(), &rect, TRUE);
}
