/*
 * File:	wx_print.h
 * Purpose:	Printing-related classes
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_printh
#define wx_printh

#include "common.h"
#include "wx_canvs.h"
#include "wx_dialg.h"
#include "wx_panel.h"
#include "wx_panel.h"
#include "wx_buttn.h"
#include "wx_messg.h"
#include "wx_choic.h"
#include "wx_rbox.h"
#include "wx_check.h"
#include "wx_dcmem.h"
#include "wx_dcps.h"

class wxDC;
class wxPrintout;
class wxPrinter;
class wxPrintDialog;

/*
 * wxPrintData
 * Encapsulates information displayed and edited in the printer dialog box.
 */
 
class wxPrintData: public wxObject
{
 public:
  void *printData;

  wxPrintData(void);
  ~wxPrintData(void);

  int GetFromPage(void);
  int GetToPage(void);
  int GetMinPage(void);
  int GetMaxPage(void);
  int GetNoCopies(void);
  Bool GetAllPages(void);
  Bool GetSetupDialog(void);

  void SetFromPage(int);
  void SetToPage(int);
  void SetMinPage(int);
  void SetMaxPage(int);
  void SetNoCopies(int);
  void SetSetupDialog(Bool);

  void EnablePageNumbers(Bool);
};

/*
 * wxPrinterDialog
 * The common dialog for printing.
 */
 
class wxPrintDialog
{
 private:
  wxPrintData *printData;
  wxDC *printerDC;
  Bool destroyDC;
 public:
  wxPrintDialog(wxWindow *parent, wxPrintData *data = NULL);
  ~wxPrintDialog(void);

  Bool Run();
  wxDC *GetPrintDC(void);
};

/*
 * Represents the printer: manages printing a wxPrintout object
 */
 
class wxPrinter: public wxObject
{
 private:
  wxPrintData *printData;
  wxPrintout *currentPrintout;
  FARPROC lpAbortProc;
 public:
  static wxWindow *abortWindow;
  static Bool abortIt;

  wxPrinter();
  ~wxPrinter(void);

  virtual Bool Print(wxWindow *parent, wxPrintout *printout, Bool prompt = TRUE);
  virtual wxWindow *CreateAbortWindow(wxWindow *parent, wxPrintout *printout);
  virtual void ReportError(wxWindow *parent, wxPrintout *printout, char *message);
  virtual inline Bool Abort(void) { return abortIt; }
};

/*
 * wxPrintout
 * Represents an object via which a document may be printed.
 * The programmer derives from this, overrides (at least) OnPrintPage,
 * and passes it to a wxPrinter object for printing, or a wxPrintPreview
 * object for previewing.
 */
 
class wxPrintout: public wxObject
{
 private:
   char *printoutTitle;
   wxDC *printoutDC;
 public:
  wxPrintout(char *title = "Printout");
  ~wxPrintout(void);

  virtual Bool OnBeginDocument(int startPage, int endPage);
  virtual void OnEndDocument(void);
  virtual void OnBeginPrinting(void);
  virtual void OnEndPrinting(void);

  // Guaranteed to be before any other functions are called
  inline virtual void OnPreparePrinting(void) { }

  virtual Bool HasPage(int page);
  virtual Bool OnPrintPage(int page) = 0;
  virtual void GetPageInfo(int *minPage, int *maxPage, int *pageFrom, int *pageTo);

  inline virtual char *GetTitle(void) { return printoutTitle; }

  inline wxDC *GetDC(void) { return printoutDC; }
  inline void SetDC(wxDC *dc) { printoutDC = dc; }
};

#endif // wx_printh
