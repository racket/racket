/*
 * File:        wx_print.cc
 * Purpose:     Printer implementation (mac)
 * Author:      Lj Birk (original msw by Julian Smart
 * Created:     1995
 * Updated:	October 1995
 * Copyright:   (c) 2004-2010 PLT Scheme Inc.
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#include "common.h"

#ifdef OS_X
# include <ApplicationServices/ApplicationServices.h>
#else
# include <ApplicationServices.h>
#endif

#if USE_PRINTING_ARCHITECTURE
#if USE_COMMON_DIALOGS

#include "wx_utils.h"
#include "wx_print.h"
#include "wx_dc.h"
#include "wx_dcpr.h"
#include "wx_main.h"
#include "wx_frame.h"
#include "wx_buttn.h"
#include "wx_dcmem.h"
#include "wx_messg.h"

#include <stdlib.h>
#include "wx_dcps.h"

/* The abortIt declaration confises xform.ss. */
#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

int wxPrinter::abortIt = 0;

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

wxPrintDialog::wxPrintDialog(wxWindow *p, wxPrintData *data):
  wxDialogBox((wxFrame *)p, "Printer Dialog")
{
  dialogParent = p;
  cShowSetupDialog = FALSE;
  printData = data;
}

wxPrintDialog::~wxPrintDialog(void)
{
}

void wxPrintDialog::ShowSetupDialog(Bool flag)
{
  cShowSetupDialog = flag;
}

void wxPrintDialog::Show(Bool flag)
{
}

Bool wxPrintDialog::UseIt(void)
{
  Boolean prtJob = FALSE;

  wxPrimDialogSetUp();

  if (cShowSetupDialog)
    PMSessionPrintDialog(printData->cPrintSession,
			 printData->cPrintSettings,
			 printData->cPageFormat,
			 &prtJob);
  else
    PMSessionPageSetupDialog(printData->cPrintSession,
			     printData->cPageFormat,
			     &prtJob);

  if (prtJob) {
    Boolean x;
    PMSessionValidatePageFormat(printData->cPrintSession, 
				printData->cPageFormat,
				&x);
  }

  wxPrimDialogCleanUp();

  return prtJob;
}

/*
 * Print data
 */

wxPrintData::wxPrintData(void)
{
  if (PMCreatePrintSettings(&cPrintSettings) != noErr)
    return;

  if (PMCreatePageFormat(&cPageFormat) != noErr) {
    PMRelease(cPrintSettings);
    cPrintSettings = NULL;
    return;
  }

  PMCreateSession(&cPrintSession);

  PMSessionDefaultPrintSettings(cPrintSession, cPrintSettings);
  PMSessionDefaultPageFormat(cPrintSession, cPageFormat);
}

wxPrintData::~wxPrintData(void)
{
  if (cPrintSettings) {
    PMRelease(cPrintSettings);
    cPrintSettings = NULL;
  }

  if (cPageFormat) {
    PMRelease(cPageFormat);
    cPageFormat = NULL;
  }

  if (cPrintSession) {
    PMRelease(cPrintSession);
    cPrintSession = NULL;
  }
}

void wxPrintData::SetAbortFlag()
{
}

int wxPrintData::GetFromPage(void)
{
  UInt32 result;
  PMGetFirstPage(cPrintSettings,&result);
  return result;
}

int wxPrintData::GetToPage(void)
{
  UInt32 result;
  PMGetLastPage(cPrintSettings,&result);
  return result;
}

int wxPrintData::GetMinPage(void)
{
  return GetFromPage();
}

int wxPrintData::GetMaxPage(void)
{
  return GetToPage();
}

int wxPrintData::GetNoCopies(void)
{
  UInt32 result;
  PMGetCopies(cPrintSettings,&result);
  return result;
}

Bool wxPrintData::GetAllPages(void)
{
  // here's the original (nonsensical) line:
  // return ( GetMaxPage() == (**macPrData).prJob.iLstPage);
  return TRUE;
}

Bool wxPrintData::GetCollate(void)
{
  return FALSE;
}

/*
wxDC *wxPrintData::GetDC(void)
{
  if (printerDC)
  {
    destroyDC = FALSE;
    return printerDC;
  }
  else
    return NULL;
}
*/

void wxPrintData::SetFromPage(int p)
{
  PMSetFirstPage(cPrintSettings,p,false);
}

void wxPrintData::SetToPage(int p)
{
  PMSetLastPage(cPrintSettings,p,false);
}

void wxPrintData::SetMinPage(int p)
{
  UInt32 low, high;
  PMGetPageRange(cPrintSettings, &low, &high);
  PMSetPageRange(cPrintSettings, p, high);
}

void wxPrintData::SetMaxPage(int p)
{
  UInt32 low, high;
  PMGetPageRange(cPrintSettings, &low, &high);
  PMSetPageRange(cPrintSettings, low, p);
}

void wxPrintData::SetNoCopies(int c)
{
  PMSetCopies(cPrintSettings,c,false);
}

void wxPrintData::SetAllPages(Bool flag)
{
  PMSetPageRange(cPrintSettings, 1, (unsigned long)kPMPrintAllPages);
}

void wxPrintData::SetCollate(Bool flag)
{
}

void wxPrintData::SetPrintToFile(Bool flag)
{
}

void wxPrintData::EnablePrintToFile(Bool flag)
{
}

void wxPrintData::EnableSelection(Bool flag)
{
}

void wxPrintData::EnablePageNumbers(Bool flag)
{
}

void wxPrintData::EnableHelp(Bool flag)
{
}

void wxPrintData::SetLandscape(Bool flag)
{
  PMSetOrientation(cPageFormat, flag ? kPMLandscape : kPMPortrait, 0);
}

Bool wxPrintData::GetLandscape()
{
  PMOrientation o;
  PMGetOrientation(cPageFormat, &o);
  return ((o == kPMLandscape) || (o == kPMReverseLandscape));
}

void wxPrintData::SetScale(double s)
{
  PMSetScale(cPageFormat, s * 100);
}

double wxPrintData::GetScale()
{
  double s;
  PMGetScale(cPageFormat, &s);
  return s / 100;
}

wxPrintData *wxPrintData::copy(void)
{
  wxPrintData *pd;

  pd = new WXGC_PTRS wxPrintData();
  PMCopyPageFormat(cPageFormat, pd->cPageFormat);
  PMCopyPrintSettings(cPrintSettings, pd->cPrintSettings);
  return pd;
}

/*
 * Printer
 */
 
wxPrinter::wxPrinter()
{
  currentPrintout = NULL;
  //lpAbortProc = MakeProcInstance((FARPROC) wxAbortProc, wxhInstance);
}

void wxRegisterAbortWindow();

void wxRegisterAbortWindow()
{
}

wxPrinter::~wxPrinter(void)
{
  //FreeProcInstance(lpAbortProc);
}

Bool wxPrinter::Print(wxWindow *parent, wxPrintout *printout, Bool prompt)
{
  int fromPage, toPage;
  int minPage, maxPage;
  GDHandle gThisGDevice;
  int logPPIScreenX, logPPIScreenY;
  PMResolution res;
  Bool keepGoing = TRUE;
  int copyCount;
  double w, h;
  wxDC* dc;
  wxPrintSetupData *ps;

  if (!printout)
    return FALSE;

  ps = wxGetThePrintSetupData();
  if (ps->native) {
    printData = ps->native->copy();
  } else {
    printData = new WXGC_PTRS wxPrintData();
    if (ps->GetPrinterOrientation() == PS_LANDSCAPE)
      printData->SetLandscape(TRUE);
    {
      double sx, sy;
      ps->GetPrinterScaling(&sx, &sy);
      printData->SetScale(sy);
    }
  }

  printout->SetIsPreview(FALSE);
  printout->OnPreparePrinting();

  // Get some parameters from the printout, if defined
  printout->GetPageInfo(&minPage, &maxPage, &fromPage, &toPage);

  if (maxPage == 0)
    return FALSE;

  printData->SetMinPage(minPage);
  printData->SetMaxPage(maxPage);
  if (fromPage != 0)
    printData->SetFromPage(fromPage);
  if (toPage != 0)
    printData->SetToPage(toPage);

  if (minPage != 0) {
    printData->EnablePageNumbers(TRUE);
    if (printData->GetFromPage() < printData->GetMinPage())
      printData->SetFromPage(printData->GetMinPage());
    else if (printData->GetFromPage() > printData->GetMaxPage())
      printData->SetFromPage(printData->GetMaxPage());
    if (printData->GetToPage() > printData->GetMaxPage())
      printData->SetToPage(printData->GetMaxPage());
    else if (printData->GetToPage() < printData->GetMinPage())
      printData->SetToPage(printData->GetMinPage());
  } else
    printData->EnablePageNumbers(FALSE);
  
  if (prompt) {
    Bool goAhead;
    wxPrintDialog *dialog;

    dialog = new WXGC_PTRS wxPrintDialog(parent, printData);
    dialog->ShowSetupDialog(TRUE);
    goAhead = dialog->UseIt();
    if (goAhead == FALSE) 
      return FALSE;
    DELETE_OBJ dialog;
  }

  // sanity check  
  if (printData->GetFromPage() <= 0 || 
      printData->GetToPage() <= 0) {
    return FALSE;
  }
  
  // Create a suitable device context  
  dc = new WXGC_PTRS wxPrinterDC(printData, 0); 

  if (!dc->Ok()) {
    if (dc) DELETE_OBJ dc; // PrSetError
    return FALSE;
  }

  gThisGDevice = GetMainDevice();
  logPPIScreenX = (int)Fix2Long((**(**gThisGDevice).gdPMap).hRes);
  logPPIScreenY = (int)Fix2Long((**(**gThisGDevice).gdPMap).vRes);

  printout->SetPPIScreen(logPPIScreenX, logPPIScreenY);

  PMGetResolution(printData->cPageFormat,&res);
  printout->SetPPIPrinter((int)res.hRes,(int)res.vRes);

  // Set printout parameters  
  printout->SetDC(dc);

  ///// TODO figure the equivalent
  dc->GetSize(&w, &h);
  printout->SetPageSizePixels((int)w, (int)h);
  //dc->GetSizeMM(&w, &h);
  dc->GetSize(&w, &h);
  printout->SetPageSizeMM((int)w, (int)h);

  printout->OnBeginPrinting();
  
  for (copyCount = 1; copyCount <= printData->GetNoCopies(); copyCount ++) {
    int pn;
    if (!printout->OnBeginDocument(printData->GetFromPage(), printData->GetToPage())) {
      //wxEndBusyCursor();
      wxMessageBox("Could not start printing.", "Print Error");
      break;
    }
    for (pn = printData->GetFromPage(); 
	 keepGoing && 
	 (pn <= printData->GetToPage()) && printout->HasPage(pn);
	 pn++) {
      dc->StartPage();
      printout->OnPrintPage(pn);
      dc->EndPage();
    }
    printout->OnEndDocument();
  }
  
  printout->OnEndPrinting();

  //wxEndBusyCursor();

  DELETE_OBJ dc;
  
  DELETE_OBJ printData;

  return TRUE;
}

Bool wxPrinter::PrintDialog(wxWindow *parent)
{
  wxPrintDialog *dialog;
  dialog = new WXGC_PTRS wxPrintDialog(parent, printData);
  dialog->Show(TRUE);
  DELETE_OBJ dialog;
  return 0;
}

Bool wxPrinter::Setup(wxWindow *parent)
{
  wxPrintDialog *dialog;
  dialog = new WXGC_PTRS wxPrintDialog(parent, printData);
  dialog->ShowSetupDialog(TRUE);
  dialog->Show(TRUE);
  DELETE_OBJ dialog;
  return 0;
}

void wxPrinter::ReportError(wxWindow *parent, wxPrintout *printout, char *message)
{
  wxMessageBox(message, "Printing Error", wxOK, (wxFrame *)parent);
}

wxPrintData *wxPrinter::GetPrintData(void)
{
  return printData;
}

/*
 * Printout class
 */
 
wxPrintout::wxPrintout(char *title)
{
  if (title) {
    printoutTitle = copystring(title);
  }
  printoutDC = NULL;
  pageWidthMM = 0;
  pageHeightMM = 0;
  pageWidthPixels = 0;
  pageHeightPixels = 0;
  PPIScreenX = 0;
  PPIScreenY = 0;
  PPIPrinterX = 0;
  PPIPrinterY = 0;
  isPreview = FALSE;
}

wxPrintout::~wxPrintout(void)
{
}

Bool wxPrintout::OnBeginDocument(int startPage, int endPage)
{
  return GetDC()->StartDoc("Printing");
}

void wxPrintout::OnEndDocument(void)
{
  GetDC()->EndDoc();
}

void wxPrintout::OnBeginPrinting(void)
{
}

void wxPrintout::OnEndPrinting(void)
{
}

Bool wxPrintout::HasPage(int page)
{
  return (page == 1);
}

void wxPrintout::GetPageInfo(int *minPage, int *maxPage, int *fromPage, int *toPage)
{
  *minPage = 1;
  *maxPage = kPMPrintAllPages;
  *fromPage = 0;
  *toPage = 0;
}

/****************************************************************************

    FUNCTION: wxAbortProc()

    PURPOSE:  Processes messages for the Abort Dialog box

****************************************************************************/

#endif
// USE_COMMON_DIALOGS
#endif
// End USE_PRINTING_ARCHITECTURE


