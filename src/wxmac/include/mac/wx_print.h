/*
 * File:	wx_print.h
 * Purpose:	Printing-related classes
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 */

/*
 * In order to port this stuff to OS X, I'm trying to fathom exactly what
 * happens in wx_print.cc and wx_dcpr1.cc.  Here's my best guess at what
 * each class does, and some rough stabs at invariants.  It occurs to 
 * me at this point that UML-type diagrams would be _very_ helpful here.
 *
 * classes in this file:
 *
 * wxPrintData : this is pretty straightforward: this class encapsulates
 *   the persistent settings for a mac print session.  Under OS 8/9, this
 *   means a TPrint structure.  Under carbon, this means a PMPrintSettings
 *   and a PMPageFormat.  This class's constructor ensures that it always
 *   starts with valid settings.  I don't know whether they can become in-
 *   valid during execution.  There are a bunch of accessor functions which
 *   provide access to the elements of these structures.  However, the 
 *   encapsulated data is also public, so these accessors are for convenience
 *   only.  The wxPrintData class does not refer to any other wx classes.
 * 
 * wxPrintDialog : this class represents the dialog.  It refers to a 
 *   wxPrintData and a wxWindow (the parent).  It lives only as long as the
 *   dialog is up.
 *
 * wxPrinter:  
 *
 *  JBC, 2001-07-03
 */

#ifndef wx_printh
#define wx_printh

#include "common.h"
#include "wx_canvs.h"
#include "wx_panel.h"
#include "wx_frame.h"
#include "wx_dialg.h"
#include "wx_choic.h"

#ifdef wx_mac
#ifndef WX_CARBON
#include <Printing.h>
#endif
#endif

#ifdef IN_CPROTO
typedef       void    *wxPrintDialog ;
typedef       void    *wxPrinter ;
typedef       void    *wxPrintout ;
#else

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
  PMPrintSession cPrintSession;
  PMPrintSettings cPrintSettings;
  PMPageFormat cPageFormat;

  wxPrintData(void);
  ~wxPrintData(void);

  void SetAbortFlag(void); // set mac abort flag
  int GetFromPage(void);
  int GetToPage(void);
  int GetMinPage(void);
  int GetMaxPage(void);
  int GetNoCopies(void);
  Bool GetAllPages(void);
  Bool GetCollate(void);

  void SetFromPage(int);
  void SetToPage(int);
  void SetMinPage(int);
  void SetMaxPage(int);
  void SetNoCopies(int);
  void SetAllPages(Bool);
  void SetCollate(Bool);
  void SetPrintToFile(Bool);

  void EnablePrintToFile(Bool);
  void EnableSelection(Bool);
  void EnablePageNumbers(Bool);
  void EnableHelp(Bool);

  void SetLandscape(Bool);
  Bool GetLandscape();
  
  void SetScale(double s);
  double GetScale();

  wxPrintData *copy();
};

/*
 * wxPrinterDialog
 * The common dialog for printing.
 */
 
class wxPrintDialog: public wxDialogBox
{
 private:
  wxPrintData *printData;
  wxWindow *dialogParent;
  
  Bool cShowSetupDialog;
 public:
  wxPrintDialog(wxWindow *parent, wxPrintData *data);
  ~wxPrintDialog(void);

  virtual Bool UseIt();
  
  virtual void Show(Bool flag);
  
  virtual void ShowSetupDialog(Bool flag);

  virtual wxPrintData *GetPrintData(void) { return printData; }
};

/*
 * Represents the printer: manages printing a wxPrintout object
 */
 
class wxPrinter: public wxObject
{
 private:
  wxPrintData *printData;
  wxPrintout *currentPrintout;
#ifndef wx_mac
  FARPROC lpAbortProc;
#endif
 public:
  static wxWindow *abortWindow;
  static Bool abortIt;

  wxPrinter();
  ~wxPrinter(void);

  virtual Bool Print(wxWindow *parent, wxPrintout *printout, Bool prompt = TRUE);
  virtual Bool PrintDialog(wxWindow *parent);
#ifndef WX_CARBON  
  virtual wxWindow *CreateAbortWindow(wxWindow *parent, wxPrintout *printout);
#endif  
  virtual Bool Setup(wxWindow *parent);
  virtual void ReportError(wxWindow *parent, wxPrintout *printout, char *message);
  virtual wxPrintData *GetPrintData(void);
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

   int pageWidthPixels;
   int pageHeightPixels;

   int pageWidthMM;
   int pageHeightMM;

   int PPIScreenX;
   int PPIScreenY;
   int PPIPrinterX;
   int PPIPrinterY;

   Bool isPreview;
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
  inline void SetPageSizePixels(int w, int  h) { pageWidthPixels = w; pageHeightPixels = h; }
  inline void GetPageSizePixels(int *w, int  *h) { *w = pageWidthPixels; *h = pageHeightPixels; }
  inline void SetPageSizeMM(int w, int  h) { pageWidthMM = w; pageHeightMM = h; }
  inline void GetPageSizeMM(int *w, int  *h) { *w = pageWidthMM; *h = pageHeightMM; }

  inline void SetPPIScreen(int x, int y) { PPIScreenX = x; PPIScreenY = y; }
  inline void GetPPIScreen(int *x, int *y) { *x = PPIScreenX; *y = PPIScreenY; }
  inline void SetPPIPrinter(int x, int y) { PPIPrinterX = x; PPIPrinterY = y; }
  inline void GetPPIPrinter(int *x, int *y) { *x = PPIPrinterX; *y = PPIPrinterY; }

  inline virtual Bool IsPreview(void) { return isPreview; }

  inline virtual void SetIsPreview(Bool p) { isPreview = p; }
};

void wxPrOpen(void);
void wxPrClose(void);

#endif // IN_CPROTO
#endif // wx_printh
