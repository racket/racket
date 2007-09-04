///////////////////////////////////////////////////////////////////////////////
// File:	wx_dcpr1.cc
// Purpose:	Print Canvas device context implementation (Macintosh version) (part 1)
// Author:	Lou Birk (copied from wx_dccan)
// Created:	1995
// Updated:	
///////////////////////////////////////////////////////////////////////////////

#include "common.h"
#include "wx_dcpr.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wx_area.h"
#include "wx_dcps.h"

//-----------------------------------------------------------------------------
// Default constructor
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
wxPrinterDC::wxPrinterDC(wxPrintData *printData, Bool interactive) : wxCanvasDC()
{
  PMRect pageRect;
  
  ok = true;

  __type = wxTYPE_DC_PRINTER;

  if (!printData) {
    wxPrintSetupData *ps;
    ps = wxGetThePrintSetupData();
    if (ps->native)
      printData = ps->native->copy();
    else {
      printData = new WXGC_PTRS wxPrintData();
      if (ps->GetPrinterOrientation() == PS_LANDSCAPE)
	printData->SetLandscape(TRUE);
      {
        double sx, sy;
        ps->GetPrinterScaling(&sx, &sy);
        printData->SetScale(sy);
      }
    }
  }

  cPrintData = printData;
  if (interactive) {
    wxPrintDialog *dialog;

    dialog = new WXGC_PTRS wxPrintDialog(NULL, printData);
    dialog->ShowSetupDialog(TRUE);
    ok = dialog->UseIt();
    DELETE_OBJ dialog;
  }

  cMacDoingDrawing = FALSE;

  clipping = FALSE;
  selected_pixmap = NULL;

  current_reg = NULL ;
  onpaint_reg = NULL ;

  min_x = 0; min_y = 0;
  max_x = 0; max_y = 0;

  PMGetAdjustedPageRect(cPrintData->cPageFormat,&pageRect);
  pixmapWidth = (int)(pageRect.right - pageRect.left);
  pixmapHeight = (int)(pageRect.bottom - pageRect.top);

  device = wxDEVICE_CANVAS;
  font = wxNORMAL_FONT;

  logical_origin_x = 0;
  logical_origin_y = 0;

  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  mapping_mode = MM_TEXT;

  title = NULL;

  current_pen_join = -1 ;
  current_pen_cap = -1 ;
  current_pen_nb_dash = -1 ;
  current_pen_dash = NULL ;
  current_stipple = NULL ;

  Colour = wxColourDisplay();

  current_pen = NULL;
  current_brush = NULL;
  current_text_foreground = new WXGC_PTRS wxColour(wxBLACK);
  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);
}

//-----------------------------------------------------------------------------
wxPrinterDC::~wxPrinterDC(void)
{
  if (current_phase == 2)
    EndPage();
  if (current_phase == 1)
    EndDoc();
}

//-----------------------------------------------------------------------------
Bool wxPrinterDC::StartDoc(char *message) 
{ 
  OSErr rlt;

  if (current_phase != 0)
   return FALSE;

  rlt = PMSessionBeginDocumentNoDialog(cPrintData->cPrintSession, cPrintData->cPrintSettings,cPrintData->cPageFormat);
  if (rlt != noErr) {
    ok = false;
    return FALSE;
  } else
    current_phase = 1;

  return TRUE;
}

//-----------------------------------------------------------------------------
void wxPrinterDC::EndDoc(void)
{
  if (current_phase != 1)
    return;

  PMSessionEndDocument(cPrintData->cPrintSession);
}

//-----------------------------------------------------------------------------
void wxPrinterDC::StartPage(void)
{
  if (current_phase != 1)
    return;

  if (ok) {
    CGrafPtr theGrafPtr;

    if (cMacDC)
      cMacDC->EndCG();

    if (PMSessionBeginPage(cPrintData->cPrintSession,
			   cPrintData->cPageFormat,
			   NULL) != noErr) {
      PMSessionEndDocument(cPrintData->cPrintSession);
      current_phase = 0;
      return;
    } else {
      if (PMSessionGetGraphicsContext(cPrintData->cPrintSession, NULL, (void **)&theGrafPtr) != noErr) {
	ok = false;
	PMSessionEndPage(cPrintData->cPrintSession);
	PMSessionEndDocument(cPrintData->cPrintSession);
	current_phase = 0;
	return;
      }
    }
    
    current_phase = 2;
    cMacDC = new WXGC_PTRS wxMacDC(theGrafPtr);
  }
}

//-----------------------------------------------------------------------------
void wxPrinterDC::EndPage(void)
{
  if (current_phase != 2)
    return;
  if (cMacDC)
    cMacDC->EndCG();
  PMSessionEndPage(cPrintData->cPrintSession);
  cMacDC = NULL;
  current_phase = 1;
}
