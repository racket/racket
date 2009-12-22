///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan.h
// Purpose:	Canvas device context declaration (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_dcprh
#define wx_dcprh

#include "wx_gdi.h"
#include "wb_dccan.h"
#include "wx_print.h"

#ifdef IN_CPROTO
typedef       void* wxPrinterDC ;
#else

//class wxCanvas;
class wxPrinterDC: public wxCanvasDC
{
 public:

  wxPrintData *cPrintData;
  int current_phase;

  wxPrinterDC(wxPrintData * data = NULL, Bool interactive = 1); // Create a DC corresponding to a canvas

  ~wxPrinterDC(void);

  Bool StartDoc(char *);
  void EndDoc(void);
  void StartPage(void);
  void EndPage(void);
};

#endif // IN_CPROTO
#endif // wx_dccanh
