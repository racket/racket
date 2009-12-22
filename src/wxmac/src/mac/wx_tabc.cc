///////////////////////////////////////////////////////////////////////////////
// File:	wx_tabc.cc
// Purpose:	Panel item tab choice implementation (Macintosh version)
// Author:	Matthew
// Created:	2002
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 2002, PLT
///////////////////////////////////////////////////////////////////////////////

#include "wx_tabc.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_area.h"
#include "wx_panel.h"

static int OS_103 = -1;

/* constants from Aqua interface guidelines */
#define TAB_TOP_SPACE (OS_103 ? 7 : 12)
#define TAB_CONTROL_HEIGHT (OS_103 ? 25 : 30)
#define TAB_CONTENT_MARGIN 2
#define TAB_BOTTOM_EXTRA_MARGIN 3
#define TAB_TITLE_SPACE 24
#define TAB_BASE_SIDE_SPACE 16

static ControlHandle MakeTabs(CGrafPtr theMacGrafPort, int N, char **Choices, Rect *boundsRect)
{
  ControlTabEntry *array;
  ControlHandle cMacControl;
  int i;

  if (OS_103 < 0) { 
    long r;
    Gestalt(gestaltSystemVersion, &r);
    OS_103 = (((r >> 4) & 0xF) > 2);
  }

#ifdef MZ_PRECISE_GC
  array = (ControlTabEntry *)GC_malloc_atomic(sizeof(ControlTabEntry) * N);
#else
  array = new WXGC_ATOMIC ControlTabEntry[N];
#endif
  for (i = 0; i < N; i++) {
    CFStringRef cfstr;
    array[i].icon = NULL;
    cfstr = wxCFString(wxItemStripLabel(Choices[i]));
    array[i].name = cfstr;
    array[i].enabled = TRUE;
  }

  cMacControl = NULL;
  CreateTabsControl(GetWindowFromPort(theMacGrafPort), boundsRect, 
		    kControlTabSizeLarge, kControlTabDirectionNorth,
		    N, array, &cMacControl);
  for (i = 0; i < N; i++) {
    CFRelease(array[i].name);
  }

  return cMacControl;
}

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxTabChoice::wxTabChoice(wxPanel *panel, wxFunction function, char *label, 
			 int N, char **Choices, int style, wxFont *_font)
 : wxItem (panel, -1, -1, -1, -1, style,  "tab-choice")
{
  int i, tch;
  CGrafPtr theMacGrafPort;
  Rect boundsRect = {0, 0, 10, 10};

  SetFont(_font, 13);

  Callback(function);

  tab_count = N;
  tab_labels = Choices;

  SetCurrentMacDC();
  theMacGrafPort = cMacDC->macGrafPort();
  OffsetRect(&boundsRect, SetOriginX, SetOriginY + TAB_TOP_SPACE);

  cMacControl = MakeTabs(theMacGrafPort, N, Choices, &boundsRect);
   
  CheckMemOK(cMacControl);

  wxSetControlFont(cMacControl, font);

  focused_button = -1;

  tch = TAB_CONTROL_HEIGHT + (font->GetPointSize() - 13);

#if 0
  /* #^%$^&!!! GetBestControlRect doesn't work for tab widgets.
     And why should it? That would be entriely too helpful. */
  Rect r = {0,0,0,0};
  SInt16 baselineOffset; // ignored
  OSErr err;
  err = ::GetBestControlRect(cMacControl,&r,&baselineOffset);

  cWindowWidth = r.right - r.left;
  cWindowHeight = r.bottom - r.top;
#else
  cWindowHeight = (TAB_TOP_SPACE + tch + TAB_CONTENT_MARGIN + TAB_BOTTOM_EXTRA_MARGIN + 5);
  cWindowWidth = TAB_TITLE_SPACE + TAB_BASE_SIDE_SPACE;
  for (i = 0; i < N; i++) {
    double x, y;
    font->GetTextExtent(wxItemStripLabel(Choices[i]), 0, -1, &x, &y, NULL, NULL, TRUE);
    cWindowWidth += TAB_TITLE_SPACE + (int)x;
  }
  padTop = TAB_TOP_SPACE;
#endif

  padLeft = padRight = padBottom = 2;

  phantom_height = -1;

  ::SizeControl(cMacControl, 
		cWindowWidth - (padLeft + padRight), 
		(style & wxBORDER) ? (cWindowHeight - padBottom) : tch);

  ::EmbedControl(cMacControl, GetRootControl());

  IgnoreKeyboardEvents();
  
  {
    wxWindow*p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (style & wxINVISIBLE)
    Show(FALSE);
  InitInternalGray();
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxTabChoice::~wxTabChoice(void)
{
  if (cMacControl) {
    ::DisposeControl(cMacControl);
    cMacControl = NULL;
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Item methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


//-----------------------------------------------------------------------------
void wxTabChoice::SetSelection(Bool value)
{
  if (cMacControl) {
    ::SetControlValue(cMacControl, value + 1);
  }
}

//-----------------------------------------------------------------------------
Bool wxTabChoice::GetSelection(void)
{
  if (cMacControl) {
    short value;
    value = ::GetControlValue(cMacControl);
    return value - 1;
  } else
    return -1;
}


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxTabChoice::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{
  if (!cMacControl)
    return;
  
  if (dW || dH) {
    int clientWidth, clientHeight;

    GetClientSize(&clientWidth, &clientHeight);

    if (cStyle & wxBORDER) {
      wxWindow *parent;
      int pw, ph;

      if (phantom_height > 0) {
	ph = phantom_height;
	pw = 0;
      } else {
	parent = GetParent();
	parent->GetClientSize(&pw, &ph);
      }
      ::SizeControl(cMacControl, clientWidth - (padLeft + padRight), 
		    ph - (padTop + padBottom));
    } else {
      int tch;
      tch = TAB_CONTROL_HEIGHT + (font->GetPointSize() - 13);
      ::SizeControl(cMacControl, clientWidth - (padLeft + padRight), tch);
    }
  }

  if (dX || dY) {
    MaybeMoveControls();
  }
}

void wxTabChoice::MaybeMoveControls(void)
{
  wxItem::MaybeMoveControls();
}

void wxTabChoice::Refresh(void)
{
  wxItem::Refresh();
}

void wxTabChoice::SetPhantomSize(int w, int h)
{
  if (phantom_height != h) {
    phantom_height = h;
    OnClientAreaDSize(0, 1, 0, 0);
  }
}

extern Bool wx_propagate_key;

static void move_it(ControlRef c, int delta)
{
  EventRef evt;
  char ch = 0;
  UInt32 zero = 0, code = ((delta > 0) ? 0x7c : 0x7b), kb = 0;

  wx_propagate_key = 1;

  CreateEvent(NULL, kEventClassKeyboard, kEventRawKeyDown, 0, 0, &evt);
  SetEventParameter(evt, kEventParamKeyMacCharCodes, typeChar, 1, &ch);
  SetEventParameter(evt, kEventParamKeyModifiers, typeUInt32, sizeof(UInt32), &zero);
  SetEventParameter(evt, kEventParamKeyCode, typeUInt32, sizeof(UInt32), &code);
  SetEventParameter(evt, kEventParamKeyboardType, typeUInt32, sizeof(UInt32), &kb);
  SendEventToEventTarget(evt, GetControlEventTarget(c));
  ReleaseEvent(evt);
  
  CreateEvent(NULL, kEventClassKeyboard, kEventRawKeyUp, 0, 0, &evt);
  SetEventParameter(evt, kEventParamKeyMacCharCodes, typeChar, 1, &ch);
  SetEventParameter(evt, kEventParamKeyModifiers, typeUInt32, sizeof(UInt32), &zero);
  SetEventParameter(evt, kEventParamKeyCode, typeUInt32, sizeof(UInt32), &code);
  SetEventParameter(evt, kEventParamKeyboardType, typeUInt32, sizeof(UInt32), &kb);
  SendEventToEventTarget(evt, GetControlEventTarget(c));
  ReleaseEvent(evt);

  wx_propagate_key = 0;
}

int wxTabChoice::ButtonFocus(int n)
{
  if (n < 0) {
    if (focused_button < 0)
      return (-focused_button) - 1;
    else
      return focused_button - 1;
  } else {
    int i;

    if (focused_button < 0) {
      SetFocus();
    }

    if (n > (focused_button - 1)) {
      for (i = focused_button; i <= n; i++) {
	move_it(cMacControl, 1);
      }
    } else {
      for (i = focused_button - 1; i > n; --i) {
	move_it(cMacControl, -1);
      }
    }

    focused_button = n + 1;

    return n;
  }
}

void wxTabChoice::OnSetFocus()
{
  if (focused_button < 0) {
    wxItem::OnSetFocus();
    focused_button = GetSelection();
    focused_button += 1;
  }
}

void wxTabChoice::OnKillFocus()
{
  if (focused_button > 0) {
    wxItem::OnKillFocus();
    focused_button = -focused_button;
  }
}


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxTabChoice::Paint(void)
{
}

//-----------------------------------------------------------------------------
void wxTabChoice::DoShow(Bool show)
{
  if (!CanShow(show)) return;

  if (show) {
    ::ShowControl(cMacControl);
  } else {
    ::HideControl(cMacControl);
  }
  wxItem::DoShow(show);
}

void wxTabChoice::ChangeToGray(Bool gray)
{
  wxItem::ChangeToGray(gray);
}

void wxTabChoice::Activate(Bool on)
{
  wxItem::Activate(on);
}

//-----------------------------------------------------------------------------
void wxTabChoice::OnEvent(wxMouseEvent *event)
{
  if (event->LeftDown()) {
    int startH;
    int startV;
    Point startPt;
    int trackResult;

    SetCurrentDC();
      
    event->Position(&startH, &startV); // client c.s.
      
    startPt.v = startV - padTop;
    startPt.h = startH - padLeft;

    wxTracking();
    if (cMacControl)
      trackResult = ::TrackControl(cMacControl, startPt, NULL);
    else
      trackResult = Track(startPt);

    if (trackResult) {
      wxCommandEvent *commandEvent;
      commandEvent = new WXGC_PTRS wxCommandEvent(wxEVENT_TYPE_TAB_CHOICE_COMMAND);
      ProcessCommand(commandEvent);
    }
  }
}


int wxTabChoice::Number(void) { 
  return tab_count;
}

void wxTabChoice::Append(char *s, int new_sel)
{
  char **new_choices;
  int i;  
  Rect r;
  ControlHandle naya;
  int ox, oy;

  if (new_sel < 0)
    new_sel = GetSelection();

  if (s) {
    new_choices = new WXGC_PTRS char*[tab_count + 1];
    for (i = 0; i < tab_count; i++) {
      new_choices[i] = tab_labels[i];
    }
    new_choices[i] = s;
    tab_labels = new_choices;
    tab_count++;
  }

  GetWinOrigin(&ox, &oy);
  
  r.top = padTop + ox;
  r.bottom = r.top + TAB_CONTROL_HEIGHT + (font->GetPointSize() - 13);
  r.left = oy + padLeft;
  r.right = r.left + cWindowWidth - (padLeft + padRight);

  naya = MakeTabs(cMacDC->macGrafPort(), tab_count, tab_labels, &r);
  wxSetControlFont(naya, font);

  if (cMacControl) {
    HIViewRef prev;
    prev = HIViewGetPreviousView(cMacControl);
    if (prev)
      ::HIViewSetZOrder(naya, kHIViewZOrderBelow, prev);
    ::DisposeControl(cMacControl);
  }
  cMacControl = naya;

  ::EmbedControl(cMacControl, GetRootControl());

  IgnoreKeyboardEvents();

  if (cHidden) {
    ::HideControl(cMacControl);
  }
  if (!cActive)
    DeactivateControl(cMacControl);
  if (!OS_Active()) {
#ifdef OS_X
    DisableControl(cMacControl);
#else
    HiliteControl(cMacControl, 255);
#endif
  }

  if (new_sel >= 0)
    SetSelection(new_sel);

  OnClientAreaDSize(1, 1, 1, 1);
}

void wxTabChoice::Delete(int i)
{
  if ((i >= 0) && (i < tab_count)) {
    int sel;

    sel = GetSelection();
    sel = ((sel <= i) ? sel : (sel ? sel - 1 : 0));

    for (i++; i < tab_count; i++) {
      tab_labels[i - 1] = tab_labels[i];
    }
    --tab_count;

    Append(NULL, sel); /* refreshes the control */
  }
}

void wxTabChoice::SetLabel(int i, char *s)
{
  if ((i >= 0) && (i < tab_count)) {
    tab_labels[i] = s;
    Append(NULL); /* refreshes the control */
  }
}

char *wxTabChoice::GetLabel()
{
  return "tab choice";
}

void wxTabChoice::Set(int N, char **Choices)
{
  int sel;
  sel = GetSelection();
  tab_count = N;
  tab_labels = Choices;
  if (sel >= N)
    sel = N - 1;
  Append(NULL, sel); /* refreshes the control */
}
