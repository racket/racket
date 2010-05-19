/*								-*- C++ -*-
 *
 * Purpose: message panel item
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2010 PLT Scheme Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */

#ifdef __GNUG__
#pragma implementation "Message.h"
#pragma GCC diagnostic ignored "-Wwrite-strings"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxMessage
#define  Uses_wxBitmap
#include "wx.h"
#define  Uses_EnforcerWidget
#define  Uses_LabelWidget
#include "widgets.h"

static char * info_xpm[] = {
"32 32 17 1",
" 	c None",
".	c #000000",
"+	c #800000",
"@	c #008000",
"#	c #808000",
"$	c #000080",
"%	c #800080",
"&	c #008080",
"*	c #808080",
"=	c #D6D6D6",
"-	c #FF0000",
";	c #00FF00",
">	c #FFFF00",
",	c #0000FF",
"'	c #FF00FF",
")	c #00FFFF",
"!	c #FFFFFF",
"================================",
"================================",
"=============,,,,,,,============",
"==========,,,,,,,,,,,,,=========",
"=========,,,,,,,,,,,,,,,========",
"=======,,,,,,,,,,,,,,,,,,,======",
"======,,,,,,,,,,,,,,,,,,,,,=====",
"======,,,,,,,,,,,,,,,,,,,,,=====",
"=====,,,,,,,,,,,,,,,,,,,,,,,====",
"====,,,,,,,,,,,!!!,,,,,,,,,,,===",
"====,,,,,,,,,,,!!!,,,,,,,,,,,===",
"====,,,,,,,,,,,,,,,,,,,,,,,,,===",
"===,,,,,,,,,,,,,,,,,,,,,,,,,,,==",
"===,,,,,,,,,,,!!!!,,,,,,,,,,,,==",
"===,,,,,,,,,,,,!!!,,,,,,,,,,,,==",
"===,,,,,,,,,,,,!!!,,,,,,,,,,,,==",
"===,,,,,,,,,,,,!!!,,,,,,,,,,,,==",
"===,,,,,,,,,,,,!!!,,,,,,,,,,,,==",
"===,,,,,,,,,,,,!!!,,,,,,,,,,,,==",
"====,,,,,,,,,,,!!!,,,,,,,,,,,===",
"====,,,,,,,,,,!!!!!,,,,,,,,,,===",
"====,,,,,,,,,,,,,,,,,,,,,,,,,===",
"=====,,,,,,,,,,,,,,,,,,,,,,,====",
"======,,,,,,,,,,,,,,,,,,,,,=====",
"======,,,,,,,,,,,,,,,,,,,,,=====",
"=======,,,,,,,,,,,,,,,,,,,======",
"=========,,,,,,,,,,,,,,,========",
"==========,,,,,,,,,,,,,=========",
"=============,,,,,,,============",
"================================",
"================================",
"================================"};

static char * caution_xpm[] = {
"32 32 17 1",
" 	c None",
".	c #000000",
"+	c #800000",
"@	c #008000",
"#	c #808000",
"$	c #000080",
"%	c #800080",
"&	c #008080",
"*	c #808080",
"=	c #D6D6D6",
"-	c #FF0000",
";	c #00FF00",
">	c #FFFF00",
",	c #0000FF",
"'	c #FF00FF",
")	c #00FFFF",
"!	c #FFFFFF",
"================================",
"================================",
"================================",
"================>===============",
"===============>>>==============",
"===============>>>==============",
"==============>>>>>=============",
"==============>>>>>=============",
"=============>>>>>>>============",
"=============>>>>>>>============",
"============>>>..>>>>===========",
"============>>....>>>===========",
"===========>>>....>>>>==========",
"===========>>>....>>>>==========",
"==========>>>>....>>>>>=========",
"==========>>>>....>>>>>=========",
"=========>>>>>....>>>>>>========",
"=========>>>>>>..>>>>>>>========",
"========>>>>>>>..>>>>>>>>=======",
"========>>>>>>>..>>>>>>>>=======",
"=======>>>>>>>>..>>>>>>>>>======",
"=======>>>>>>>>>>>>>>>>>>>======",
"======>>>>>>>>>>>>>>>>>>>>>=====",
"======>>>>>>>>>>>>>>>>>>>>>=====",
"=====>>>>>>>>>>..>>>>>>>>>>>====",
"=====>>>>>>>>>>..>>>>>>>>>>>====",
"====>>>>>>>>>>>>>>>>>>>>>>>>>===",
"====>>>>>>>>>>>>>>>>>>>>>>>>>===",
"================================",
"================================",
"================================",
"================================"};

static char * stop_xpm[] = {
"32 32 17 1",
" 	c None",
".	c #000000",
"+	c #800000",
"@	c #008000",
"#	c #808000",
"$	c #000080",
"%	c #800080",
"&	c #008080",
"*	c #808080",
"=	c #D6D6D6",
"-	c #FF0000",
";	c #00FF00",
">	c #FFFF00",
",	c #0000FF",
"'	c #FF00FF",
")	c #00FFFF",
"!	c #FFFFFF",
"================================",
"================================",
"==========-----------===========",
"=========-------------==========",
"========---------------=========",
"=======-----------------========",
"======-------------------=======",
"=====---------------------======",
"====-----------------------=====",
"===-------------------------====",
"==----------!-----!----------===",
"==---------!!!---!!!---------===",
"==----------!!!-!!!----------===",
"==-----------!!!!!-----------===",
"==------------!!!------------===",
"==------------!!!------------===",
"==-----------!!!!!-----------===",
"==----------!!!-!!!----------===",
"==---------!!!---!!!---------===",
"==----------!-----!----------===",
"==---------------------------===",
"===-------------------------====",
"====-----------------------=====",
"=====---------------------======",
"======-------------------=======",
"=======-----------------========",
"========---------------=========",
"=========-------------==========",
"==========-----------===========",
"================================",
"================================",
"================================"};

static int icons_loaded;
static wxBitmap *icons[3];

//-----------------------------------------------------------------------------
// create and destroy message
//-----------------------------------------------------------------------------

wxMessage::wxMessage(wxPanel *panel, char *message,
		   int x, int y, long style, wxFont *_font, char *name) : wxItem(_font)
{
    __type = wxTYPE_MESSAGE;
    Create(panel, message, x, y, style, name);
}

wxMessage::wxMessage(wxPanel *panel, wxBitmap *bitmap,
		   int x, int y, long style, wxFont *_font, char *name) : wxItem(_font)
{
    __type = wxTYPE_MESSAGE;
    Create(panel, bitmap, x, y, style, name);
}

wxMessage::wxMessage(wxPanel *panel, int iconId,
		   int x, int y, long style, wxFont *_font, char *name) : wxItem(_font)
{
    __type = wxTYPE_MESSAGE;
    Create(panel, NULL, NULL, iconId, x, y, style, name);
}

static void do_nothing()
{
}

Bool wxMessage::Create(wxPanel *panel, char *message,
		      int x, int y, long style, char *name)
{
  return Create(panel, message, NULL, 0, x, y, style, name);
}

Bool wxMessage::Create(wxPanel *panel, wxBitmap *bitmap,
		      int x, int y, long style, char *name)
{
  return Create(panel, NULL, bitmap, 0, x, y, style, name);
}

Bool wxMessage::Create(wxPanel *panel, 
		       char *label, wxBitmap *bitmap, int iconID, 
		       int x, int y,
		       long style, char *name)
{
    wxWindow_Xintern *ph;
    Widget wgt;
    const char *lblT;
    void *lblV, *maskmap;

    if (iconID) {
      if (!icons_loaded) {
	icons_loaded = 1;
	wxREGGLOB(icons);
	icons[0] = new wxBitmap(info_xpm, NULL);
	icons[1] = new wxBitmap(caution_xpm, NULL);
	icons[2] = new wxBitmap(stop_xpm, NULL);
      }

      bitmap = icons[iconID - 1];
      if (!bitmap)
	label = "<bad-icon>";
    }

    if (bitmap) {
      if (!bitmap->Ok() || (bitmap->selectedIntoDC < 0)) {
	label = "<bad-image>";
	bitmap = NULL;
      } else {
	bitmap->selectedIntoDC++;
	bm_label = bitmap;
      }
    }
    
    if (!bitmap)
      bm_label = NULL;

    bm_label_mask = CheckMask(bm_label);

    ChainToPanel(panel, style, name);

    ph = parent->GetHandle();

    // create frame
    wgt = XtVaCreateWidget
	(name, xfwfEnforcerWidgetClass, ph->handle,
	 XtNbackground,  wxGREY_PIXEL,
	 XtNforeground,  wxBLACK_PIXEL,
	 XtNfont,        font->GetInternalFont(),
#ifdef WX_USE_XFT
	 XtNxfont,       font->GetInternalAAFont(),
#endif
	 XtNshrinkToFit, TRUE,
	 XtNhighlightThickness, 0, XtNtraversalOn, FALSE,
	 NULL);
    X->frame = wgt;

    if (!(style & wxINVISIBLE))
      XtManageChild(X->frame);
    else
      XtRealizeWidget(wgt);
    // create widget
    if (bitmap) {
      lblT = XtNpixmap;
      lblV = bitmap->GetLabelPixmap();
    } else {
      lblT = XtNlabel;
      lblV = label;
    }

    if (bm_label_mask)
      maskmap = (void *)GETPIXMAP(bm_label_mask);
    else
      maskmap = 0;

    // create widget
    wgt = XtVaCreateManagedWidget
	("message", xfwfLabelWidgetClass, X->frame,
	 lblT,           lblV,
	 XtNmaskmap,     maskmap,
	 XtNbackground,  wxGREY_PIXEL,
	 XtNforeground,  wxBLACK_PIXEL,
	 XtNfont,        font->GetInternalFont(),
#ifdef WX_USE_XFT
	 XtNxfont,       font->GetInternalAAFont(),
#endif
	 XtNalignment,   wxALIGN_LEFT,
	 XtNshrinkToFit, TRUE,
	 XtNhighlightThickness, 0,
	 ( !(style & wxBORDER) ) ?
	     NULL :
	     XtNouterOffset, 0, 
	     XtNinnerOffset, 1,
	     XtNframeWidth,  0,
	     NULL);

    X->handle = wgt;

    panel->PositionItem(this, x, y, -1, -1);
    AddEventHandlers();

    /* This just turns on KeyPress events in the widget so that PreOnChar() works. */
    XtAddEventHandler(X->frame, KeyPressMask, FALSE, (XtEventHandler)do_nothing, NULL);
    XtAddEventHandler(X->handle, KeyPressMask, FALSE, (XtEventHandler)do_nothing, NULL);
    
    AllowResize(FALSE);

    if (style & wxINVISIBLE)
      Show(FALSE);

    return TRUE;
}

wxMessage::~wxMessage()
{
  if (bm_label) {
    --bm_label->selectedIntoDC;
    bm_label->ReleaseLabel();
    XtVaSetValues(X->handle, XtNpixmap, NULL, XtNmaskmap, NULL, NULL);
  }
  if (bm_label_mask) {
    --bm_label_mask->selectedIntoDC;
  }
}

//-----------------------------------------------------------------------------
// alternate SetLabel for changing bitmap
//-----------------------------------------------------------------------------

void wxMessage::AllowResize(Bool allow)
{
    XtVaSetValues(X->handle, XtNshrinkToFit, allow, NULL);
}

void wxMessage::SetAlignment(long alignment)
{
    XtVaSetValues(X->handle, XtNalignment, alignment, NULL);
}

void wxMessage::SetLabel(char *message)
{
  message = wxGetCtlLabel(message);
  if (!bm_label)
    XtVaSetValues(X->handle, XtNlabel, message, XtNbitmap, None, NULL);
}

void wxMessage::SetLabel(wxBitmap *bitmap)
{
  {
    /* Don't reset the label if this is an icon message: */
    int i;
    for (i = 0; i < 3; i++) {
      if (bm_label == icons[i])
	return;
    }
  }

  if (bm_label && bitmap && bitmap->Ok() && (bitmap->selectedIntoDC >= 0)
      && (bitmap->GetDepth()==1 || bitmap->GetDepth()==wxDisplayDepth())) {
    Pixmap pm, mpm;

    --bm_label->selectedIntoDC;
    bm_label->ReleaseLabel();
    if (bm_label_mask) {
      --bm_label_mask->selectedIntoDC;
      bm_label_mask = NULL;
    }

    bm_label = bitmap;
    bm_label->selectedIntoDC++;

    bm_label_mask = CheckMask(bm_label);

    pm = (Pixmap)bitmap->GetLabelPixmap();
    if (bm_label_mask)
      mpm = GETPIXMAP(bm_label_mask);
    else
      mpm = 0;
    XtVaSetValues(X->handle, 
		  XtNlabel, NULL, 
		  XtNpixmap, pm, 
		  XtNmaskmap, mpm, NULL);
  }
}

char *wxMessage::GetLabel(void)
{
  char *label;

  if (!X->handle) // forbid, if no widget associated
    return NULL;
  
  label = NULL;
  XtVaGetValues(X->handle, XtNlabel, &label, NULL);
  return label;
}
