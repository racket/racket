/*
 * File:        wx_print.cc
 * Purpose:     Printer implementation (MSW)
 * Author:      Julian Smart
 * Created:     1995
 * Updated:	April 1995
 * Copyright:   (c) 2004-2005 PLT Scheme, Inc.
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "wx_print.h"

#include <stdlib.h>

#include "wx_pdf.h"

#include <commdlg.h>

LONG APIENTRY wxAbortProc(HDC hPr, int Code);

wxPrintPaperDatabase *wxThePrintPaperDatabase = NULL;

wxPrintDialog::wxPrintDialog(wxWindow *p, wxPrintData *data)
{
  printerDC = NULL;
  destroyDC = TRUE;

  printData = data;

  {
    HWND h;
    if (p) {
      h = p->GetHWND();
    } else
      h = NULL;
    
    ((PRINTDLG *)printData->printData)->hwndOwner = h;
  }
}

wxPrintDialog::~wxPrintDialog(void)
{
  if (destroyDC && printerDC)
    delete printerDC;
}

static BOOL do_print(void *data, HWND parent)
{
  PRINTDLG *p = (PRINTDLG *)data;
  p->hwndOwner = parent;

  return PrintDlg(p);
}

Bool wxPrintDialog::Run()
{
  if (wxPrimitiveDialog(do_print, printData->printData , 0)) {
    wxPrinterDC *pdc;
    pdc = new wxPrinterDC(((PRINTDLG *)printData->printData)->hDC);
    printerDC = pdc;
    return TRUE;
  } else
    return FALSE;
}

wxDC *wxPrintDialog::GetPrintDC(void)
{
  if (printerDC) {
    destroyDC = FALSE;
    return printerDC;
  } else
    return NULL;
}

/*
 * Print data
 */

wxPrintData::wxPrintData(void)
{
  PRINTDLG *pd;
  pd = (PRINTDLG *)malloc(sizeof(PRINTDLG));
  printData = (void *)pd;

  memset(pd, 0, sizeof(PRINTDLG));
  pd->Flags = PD_RETURNDC | PD_NOSELECTION | PD_NOPAGENUMS;
  pd->lStructSize = sizeof(PRINTDLG);
  pd->hwndOwner = (HWND)NULL;
  pd->hDevMode = (HANDLE)NULL;
  pd->hDevNames = (HANDLE)NULL;
  pd->nFromPage = 0;
  pd->nToPage = 0;
  pd->nMinPage = 0;
  pd->nMaxPage = 0;
  pd->nCopies = 1;
  pd->hInstance=(HINSTANCE)NULL;
}

wxPrintData::~wxPrintData(void)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  free(pd);
}

int wxPrintData::GetFromPage(void)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  if (!(pd->Flags & (PD_PAGENUMS | PD_SELECTION)))
    return 1;
  else
    return pd->nFromPage;
}

int wxPrintData::GetToPage(void)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  if (!(pd->Flags & (PD_PAGENUMS | PD_SELECTION)))
    return 32000;
  else
    return pd->nToPage;
}

int wxPrintData::GetMinPage(void)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  return pd->nMinPage;
}

int wxPrintData::GetMaxPage(void)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  return pd->nMaxPage;
}

int wxPrintData::GetNoCopies(void)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  return pd->nCopies;
}

Bool wxPrintData::GetAllPages(void)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  return ((pd->Flags & PD_ALLPAGES) == PD_ALLPAGES);
}


void wxPrintData::SetFromPage(int p)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  pd->nFromPage = (UINT)p;
}

void wxPrintData::SetToPage(int p)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  pd->nToPage = (UINT)p;
}

void wxPrintData::SetMinPage(int p)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  pd->nMinPage = (UINT)p;
}

void wxPrintData::SetMaxPage(int p)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  pd->nMaxPage = (UINT)p;
}

void wxPrintData::SetNoCopies(int c)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  pd->nCopies = (UINT)c;
}

void wxPrintData::EnablePageNumbers(Bool flag)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  if (flag) {
    if (pd->Flags & PD_NOPAGENUMS)
      pd->Flags -= PD_NOPAGENUMS;
  } else {
    pd->Flags |= PD_NOPAGENUMS;
  }
}

void wxPrintData::SetSetupDialog(Bool flag)
{
  PRINTDLG *pd = (PRINTDLG *)printData;
  if (flag) {
    pd->Flags |= PD_PRINTSETUP;
  } else if (pd->Flags & PD_PRINTSETUP) {
    pd->Flags -= PD_PRINTSETUP;
  }
}

/*
 * Printer
 */

wxPrinter::wxPrinter()
{
  currentPrintout = NULL;
  abortIt = FALSE;
  lpAbortProc = MakeProcInstance((FARPROC) wxAbortProc, wxhInstance);

  printData = new wxPrintData();
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

wxWindow *wxPrinter::abortWindow = NULL;
int aw_registered = 0;

Bool wxPrinter::abortIt = FALSE;

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

wxPrinter::~wxPrinter(void)
{
  FreeProcInstance(lpAbortProc);
}

Bool wxPrinter::Print(wxWindow *parent, wxPrintout *printout, Bool prompt)
{
  int fromPage, toPage;
  int minPage, maxPage;
  wxDC *dc = NULL;
  wxWindow *win;
  Bool keepGoing;
  int copyCount;

  abortIt = FALSE;
  abortWindow = NULL;

  if (!printout)
    return FALSE;
    
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

  if (1) {
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
  
  // Create a suitable device context  
  if (prompt) {
    wxPrintDialog  *dialog;
    dialog = new wxPrintDialog(parent, printData);
    if (dialog->Run())
      dc = dialog->GetPrintDC();
  } else {
    dc = new wxPrinterDC(NULL, NULL, NULL, NULL, FALSE);
  }

  // May have pressed cancel.
  if (!dc || !dc->Ok()) {
    if (dc) delete dc;
    return FALSE;
  }
  
  // Set printout parameters  
  printout->SetDC(dc);

  // Create an abort window
  wxBeginBusyCursor();

  win = CreateAbortWindow(parent, printout);
  wxYield();
  ::SetAbortProc(dc->cdc, (ABORTPROC)lpAbortProc);
  
  if (!win) {
    wxEndBusyCursor();
    wxMessageBox("Sorry, could not create an abort dialog.", "Print Error", wxOK, parent);
    delete dc;
  }

  if (!aw_registered) {
    wxREGGLOB(abortWindow);
    aw_registered = 1;
  }
  abortWindow = win;
  abortWindow->Show(TRUE);
  wxYield();

  printout->OnBeginPrinting();
  
  keepGoing = TRUE;

  for (copyCount = 1; copyCount <= printData->GetNoCopies(); copyCount ++) {
    int pn;
    int is_down;
    int endpage;

    if (!printout->OnBeginDocument(printData->GetFromPage(), printData->GetToPage())) {
      wxEndBusyCursor();
      wxMessageBox("Could not start printing.", "Print Error", wxOK, parent);
      break;
    }
    
    if (abortIt)
      break;

    is_down = (printData->GetFromPage() > printData->GetToPage());
    endpage = printData->GetToPage() + (is_down ? -1 : 1);
    for (pn = printData->GetFromPage(); keepGoing && (pn != endpage);
	 pn = (is_down ? pn - 1 : pn + 1)) {
      if (!printout->HasPage(pn)) {
	if (!is_down)
	  break;
      } else {
	if (abortIt) {
	  keepGoing = FALSE;
	  break;
	} else {
	  dc->StartPage();
	  printout->OnPrintPage(pn);
	  dc->EndPage();
	  if (abortWindow)
	    wxYield();
	}
      }
    }
    printout->OnEndDocument();
  }
  
  printout->OnEndPrinting();

  if (abortWindow) {
    abortWindow->Show(FALSE);
    delete abortWindow;
    abortWindow = NULL;
  }

  wxEndBusyCursor();

  delete dc;
  
  return TRUE;
}

static void wxAbortWindowCancel(wxButton *WXUNUSED(but), wxCommandEvent *WXUNUSED(event))
{
  wxPrinter::abortIt = TRUE;
  wxPrinter::abortWindow->Show(FALSE);
  delete (wxPrinter::abortWindow);
  wxPrinter::abortWindow = NULL;
}

wxWindow *wxPrinter::CreateAbortWindow(wxWindow *parent, wxPrintout *WXUNUSED(printout))
{
  wxDialogBox *dialog;
  wxButton *button;

  dialog = new wxDialogBox(parent, "Printing", 0, 0, 400, 400);
  new wxMessage(dialog, "Please wait, printing...");
  dialog->NewLine();
  button = new wxButton(dialog, (wxFunction) wxAbortWindowCancel, "Cancel");
  
  dialog->Fit();
  button->Centre(wxHORIZONTAL);

  dialog->Centre();
  return dialog;
}

void wxPrinter::ReportError(wxWindow *parent, wxPrintout *WXUNUSED(printout), char *message)
{
  wxMessageBox(message, "Printing Error", wxOK, parent);
}

/*
 * Printout class
 */
 
wxPrintout::wxPrintout(char *title)
{
  if (title) {
    printoutTitle = copystring(title);
  } else
    printoutTitle = (char *)NULL;
  printoutDC = NULL;
}

wxPrintout::~wxPrintout(void)
{
}

Bool wxPrintout::OnBeginDocument(int WXUNUSED(startPage), int WXUNUSED(endPage))
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
  OSVERSIONINFO info;
  int is_win95;

  info.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx(&info);
  is_win95 = (info.dwPlatformId != VER_PLATFORM_WIN32_NT);

  if (is_win95) {
    *minPage = 1;
    *maxPage = 0xFFFF;
    *fromPage = 0xFFFF;
    *toPage = 0xFFFF;
  } else {
    *minPage = 1;
    *maxPage = 0xFFFF;
    *fromPage = 1;
    *toPage = 1;
  }
}

/****************************************************************************

    FUNCTION: wxAbortProc()

    PURPOSE:  Processes messages for the Abort Dialog box

****************************************************************************/

LONG APIENTRY wxAbortProc(HDC WXUNUSED(hPr), int WXUNUSED(Code))
{
  if (!wxPrinter::abortWindow)              /* If the abort dialog isn't up yet */
    return(TRUE);
  
  /* Don't yield here, because we might change threads,
     which causes Windows-owned portions of the stack to
     be copied, etc. */
  /* wxYield(); */
  
  /* bAbort is TRUE (return is FALSE) if the user has aborted */
  
  return !wxPrinter::abortIt;
}
