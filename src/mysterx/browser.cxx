// browser.cxx

#include "stdafx.h"

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>
#include <process.h>

#include "escheme.h"

#include "bstr.h"
#include "myspage.h"
#include "myssink.h"

#include "mysterx.h"

HWND browserHwnd;

/* we don't worry about overflow, since
   Windows can't even create this many windows */
unsigned long browserCount;
static BOOL noBrowsersCache = TRUE;

BROWSER_WINDOW_STYLE_OPTION styleOptions[6] = {

  // keep alphabetic for bsearch()

  // { symbol,Win32 constant,TRUE=add/FALSE=remove }

  { "iconize",WS_ICONIC,TRUE },
  { "maximize",WS_MAXIMIZE,TRUE },
  { "no-system-menu",WS_CAPTION | WS_SYSMENU,FALSE },
  { "no-thick-border",WS_THICKFRAME,FALSE },
  { "scrollbars",WS_HSCROLL | WS_VSCROLL,TRUE },
};

int cmpBwso(char *key,BROWSER_WINDOW_STYLE_OPTION *bwso) {
  return strcmp(key,bwso->name);
}

void assignIntOrDefault(int *pVal,Scheme_Object **argv,int argc,int ndx) {
  if (SCHEME_SYMBOLP(argv[ndx])) {
    *pVal = CW_USEDEFAULT;
    if (strcmpi(SCHEME_SYM_VAL(argv[ndx]),"default") == 0) {
      *pVal = CW_USEDEFAULT;
    }
    else {
      scheme_wrong_type("make-document","int",ndx+1,argc,argv);
    }
  }
  else if (SCHEME_INTP(argv[ndx]) == FALSE) {
    scheme_wrong_type("make-browser","int",ndx+1,argc,argv);
  }
  else {
    *pVal = SCHEME_INT_VAL(argv[ndx]);
  }
}

Scheme_Object *mx_make_browser(int argc,Scheme_Object **argv) {
  HRESULT hr;
  MX_Browser_Object *browser;
  IUnknown *pIUnknown;
  IConnectionPointContainer *pIConnectionPointContainer;
  IConnectionPoint *pIConnectionPoint;
  ISink *pISink;
  IDHTMLPage *pIDHTMLPage;
  IStream *pIStream,*pBrowserStream;
  IWebBrowser2 *pIWebBrowser2;
  IEventQueue *pIEventQueue;
  Scheme_Object *pSyms,*currSym;
  char *currStyleOption;
  BROWSER_WINDOW_INIT browserWindowInit;
  BROWSER_WINDOW_STYLE_OPTION *pBwso;
  DWORD cookie;

  browserWindowInit.browserWindow.label = schemeToMultiByte (GUARANTEE_STRSYM ("make-browser", 0));

  assignIntOrDefault(&browserWindowInit.browserWindow.width,argv,argc,1);
  assignIntOrDefault(&browserWindowInit.browserWindow.height,argv,argc,2);
  assignIntOrDefault(&browserWindowInit.browserWindow.x,argv,argc,3);
  assignIntOrDefault(&browserWindowInit.browserWindow.y,argv,argc,4);

  if (SCHEME_PAIRP(argv[5]) == FALSE && argv[5] != scheme_null) {
    scheme_wrong_type("make-browser","list of symbols",5,argc,argv);
  }

  pSyms = argv[5];
  browserWindowInit.browserWindow.style = WS_OVERLAPPEDWINDOW;

  while (pSyms != scheme_null) {

    currSym = SCHEME_CAR(pSyms);

    if (SCHEME_SYMBOLP(currSym) == FALSE) {
      scheme_wrong_type("make-browser","list of symbols",5,argc,argv);
    }

    currStyleOption = SCHEME_SYM_VAL(currSym);

    pBwso = (BROWSER_WINDOW_STYLE_OPTION *)
              bsearch(currStyleOption,
		      styleOptions,
		      sizeray(styleOptions),
		      sizeof(styleOptions[0]),
		      (int (*)(const void *,const void *))cmpBwso);

    if (pBwso == NULL) {
      scheme_signal_error("Invalid browser window style option: %s",
			  currStyleOption);
    }

    if (pBwso->enable) {
      browserWindowInit.browserWindow.style |= pBwso->bits;
    }
    else {
      browserWindowInit.browserWindow.style &= ~(pBwso->bits);
    }

    pSyms = SCHEME_CDR(pSyms);
  }

  // mutex to protect association between new window and pIUnknown pointer to DHTML control

  WaitForSingleObject(browserHwndMutex,INFINITE);

  browserWindowInit.ppIStream = &pBrowserStream;

  browser = (MX_Browser_Object *)scheme_malloc(sizeof(MX_Browser_Object));
  browser->type = mx_browser_type;
  browserWindowInit.browserObject = browser;

  // use _beginthread instead of CreateThread
  // because the use of HTMLHelp requires the use of
  // multithreaded C library

  _beginthread(browserHwndMsgLoop,0,(void *)&browserWindowInit);

  // wait until the window is created

  WaitForSingleObject(createHwndSem,INFINITE);

  browser->destroy = FALSE;
  browser->hwnd = browserHwnd;

  hr = CoGetInterfaceAndReleaseStream(pBrowserStream,IID_IUnknown,(void **)&pIUnknown);

  ReleaseSemaphore(browserHwndMutex,1,NULL);

  if (hr != S_OK || pIUnknown == NULL) {
    DestroyWindow(browserHwnd);
    codedComError("make-browser: Can't get browser IUnknown interface",hr);
  }

  pIUnknown->QueryInterface(IID_IDHTMLPage,(void **)&pIDHTMLPage);

  pIUnknown->Release();

  if (pIDHTMLPage == NULL) {
    scheme_signal_error("make-browser: Can't get IDHTMLPage interface");
  }

  // workaround for inability to use exdisp.idl or mshtml.idl

  pIStream = NULL;
  pIDHTMLPage->marshalWebBrowserToStream(&pIStream);

  if (pIStream == NULL) {
    scheme_signal_error("make-browser: Can't get pIStream interface for browser");
  }

  hr = CoGetInterfaceAndReleaseStream(pIStream,IID_IWebBrowser2,(void **)&pIWebBrowser2);

  if (hr != S_OK || pIWebBrowser2 == NULL) {
    codedComError("make-browser: Can't get IWebBrowser2 interface",hr);
  }

  pIStream = NULL;
  pIDHTMLPage->marshalEventQueueToStream(&pIStream);

  pIDHTMLPage->Release();

  if (pIStream == NULL) {
    scheme_signal_error("make-browser: Can't get IStream interface for event queue");
  }

  hr = CoGetInterfaceAndReleaseStream(pIStream,IID_IEventQueue,(void **)&pIEventQueue);

  if (hr != S_OK || pIEventQueue == NULL) {
    codedComError("make-browser: Can't get event queue interface",hr);
  }

  pIEventQueue->GetReaderSemaphore((long *)(&browser->readSem));

  if (browser->readSem == 0) {
    scheme_signal_error("make-browser: Error retrieving browser event read semaphore");
  }

  // setup event sink for browser

  hr = pIWebBrowser2->QueryInterface(IID_IConnectionPointContainer,(void **)&pIConnectionPointContainer);

  if (hr != S_OK || pIConnectionPointContainer == NULL) {
    signalCodedEventSinkError("make-browser: Unable to get browser connection point container",hr);
  }

  hr = pIConnectionPointContainer->FindConnectionPoint(DIID_DWebBrowserEvents2,
						       &pIConnectionPoint);

  if (hr != S_OK || pIConnectionPoint == NULL) {
    signalCodedEventSinkError("make-browser: Unable to get browser connection point",hr);
  }

  pIConnectionPointContainer->Release();

  hr = CoCreateInstance(CLSID_Sink,NULL,
			CLSCTX_LOCAL_SERVER | CLSCTX_INPROC_SERVER,
			IID_IUnknown,(void **)&pIUnknown);

  if (hr != S_OK || pIUnknown == NULL) {
    signalCodedEventSinkError("make-browser: Unable to create sink object",hr);
  }

  hr = pIUnknown->QueryInterface(IID_ISink,(void **)&pISink);

  if (hr != S_OK || pISink == NULL) {
    signalCodedEventSinkError("make-browser: Unable to find sink interface",hr);
  }

  pISink->set_myssink_table(&myssink_table);

  hr = pIConnectionPoint->Advise(pIUnknown,&cookie);

  pIUnknown->Release();

  if (hr != S_OK) {
    signalCodedEventSinkError("make-browser: Unable to connect sink to connection point",hr);
  }

  browser->pIWebBrowser2 = pIWebBrowser2;
  browser->pISink = pISink;
  browser->pIEventQueue = pIEventQueue;

  scheme_add_managed((Scheme_Custodian *)scheme_get_param(scheme_current_config(),MZCONFIG_CUSTODIAN),
		     (Scheme_Object *)browser,
		     (Scheme_Close_Custodian_Client *)scheme_release_browser,
		     (void *)TRUE,0);

  scheme_register_finalizer(browser,scheme_release_browser,NULL,NULL,NULL);

  ++browserCount;
  noBrowsersCache = FALSE;

  return (Scheme_Object *)browser;
}

int browserExists(Scheme_Object *v) {
  if (noBrowsersCache == TRUE) {
    return TRUE;
  }

  if (browserCount == 0) {
    noBrowsersCache = TRUE;
    return TRUE;
  }

  return FALSE;
}

Scheme_Object *mx_block_while_browsers(int argc,Scheme_Object **argv) {
  scheme_block_until(browserExists,NULL,NULL,1.0);
  return scheme_void;
}

Scheme_Object *mx_navigate(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IWebBrowser2 *pIWebBrowser2;
  BSTR url;
  VARIANT vars[4];

  pIWebBrowser2 = MX_BROWSER_VAL (GUARANTEE_BROWSER ("navigate", 0));

  url = schemeToBSTR (GUARANTEE_STRSYM ("navigate", 1));

  memset(vars,0,sizeof(vars));

  hr = pIWebBrowser2->Navigate(url,vars,vars+1,vars+2,vars+3);

  SysFreeString(url);

  return (hr == S_OK) ? scheme_true : scheme_false;
}

Scheme_Object *mx_go_back(int argc,Scheme_Object **argv) {
  IWebBrowser2 *pIWebBrowser2;

  pIWebBrowser2 = MX_BROWSER_VAL (GUARANTEE_BROWSER ("go-back", 0));

  return (pIWebBrowser2->GoBack() == S_OK) ? scheme_true : scheme_false;
}

Scheme_Object *mx_go_forward(int argc,Scheme_Object **argv) {
  IWebBrowser2 *pIWebBrowser2;

  pIWebBrowser2 = MX_BROWSER_VAL (GUARANTEE_BROWSER ("go-forward", 0));

  return (pIWebBrowser2->GoForward() == S_OK) ? scheme_true : scheme_false;
}

Scheme_Object *mx_refresh(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IWebBrowser2 *pIWebBrowser2;

  pIWebBrowser2 = MX_BROWSER_VAL (GUARANTEE_BROWSER ("refresh", 0));

  hr = pIWebBrowser2->Refresh();

  return (hr == S_OK) ? scheme_true : scheme_false;
}

Scheme_Object *mx_show_browser_window(int argc,Scheme_Object **argv,
				      int cmd,char *s) {
  HWND hwnd;

  hwnd = MX_BROWSER_HWND (GUARANTEE_BROWSER (s, 0));

  if (hwnd == NULL) {
    scheme_signal_error("Browser has NULL window handle");
  }

  ShowWindow(hwnd,cmd);

  return scheme_void;
}

Scheme_Object *mx_browser_show(int argc,Scheme_Object **argv) {
  BOOL noShow;
  noShow = (argv[1] == scheme_false);
  browserCount +=  noShow ? -1 : 1;
  return mx_show_browser_window(argc,argv,
				noShow ? SW_HIDE : SW_SHOW,
				"show");
}

Scheme_Object *mx_iconize(int argc,Scheme_Object **argv) {
  return mx_show_browser_window(argc,argv,SW_MINIMIZE,"iconize");
}

Scheme_Object *mx_restore(int argc,Scheme_Object **argv) {
  return mx_show_browser_window(argc,argv,SW_SHOWNORMAL,"restore");
}

Scheme_Object *mx_register_navigate_handler(int argc,Scheme_Object **argv) {
  ISink *pISink;

  pISink = MX_BROWSER_SINK (GUARANTEE_BROWSER ("register-navigate-handler", 0));

  // register handler for NavigateComplete2 event (memID = 259)

  pISink->register_handler(259,argv[1]);

  return scheme_void;
}

IHTMLDocument2 *IHTMLDocument2FromBrowser(Scheme_Object *obj) {
  HRESULT hr;
  IWebBrowser2 *pIWebBrowser2;
  IHTMLDocument2 *pIHTMLDocument2;
  IDispatch *pIDispatch;

  pIWebBrowser2 = MX_BROWSER_VAL(obj);

  hr = pIWebBrowser2->get_Document(&pIDispatch);

  if (hr != S_OK || pIDispatch == NULL) {
    scheme_signal_error("Error retrieving DHTML dispatch interface");
  }

  hr = pIDispatch->QueryInterface(IID_IHTMLDocument2,(void **)&pIHTMLDocument2);

  pIDispatch->Release();

  if (hr != S_OK || pIHTMLDocument2 == NULL) {
    codedComError("Error retrieving DHTML document2 interface",hr);
  }

  return pIHTMLDocument2;
}

Scheme_Object *mx_current_document(int argc,Scheme_Object **argv) {
  IHTMLDocument2 *pIHTMLDocument2;
  MX_Document_Object *doc;

  pIHTMLDocument2 = IHTMLDocument2FromBrowser (GUARANTEE_BROWSER ("current-document", 0));

  doc = (MX_Document_Object *)scheme_malloc(sizeof(MX_Document_Object));
  doc->type = mx_document_type;
  doc->pIHTMLDocument2 = pIHTMLDocument2;

  scheme_add_managed((Scheme_Custodian *)scheme_get_param(scheme_current_config(),MZCONFIG_CUSTODIAN),
		     (Scheme_Object *)doc,
		     (Scheme_Close_Custodian_Client *)scheme_release_document,
		     NULL,0);

  scheme_register_finalizer(doc,scheme_release_document,NULL,NULL,NULL);

  return (Scheme_Object *)doc;
}

Scheme_Object *mx_print(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IWebBrowser2 *pIWebBrowser2;
  VARIANT varIn, varOut;

  pIWebBrowser2 = MX_BROWSER_VAL (GUARANTEE_BROWSER ("print", 0));

  VariantInit(&varIn);
  VariantInit(&varOut);

  hr = pIWebBrowser2->ExecWB(OLECMDID_PRINT,
			     OLECMDEXECOPT_DONTPROMPTUSER,
			     &varIn,&varOut);

  if (hr != S_OK) {
    codedComError("print: Error printing",hr);
  }

  return scheme_void;
}

Scheme_Object *mx_current_url(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IWebBrowser2 *pIWebBrowser2;
  IHTMLDocument2 *pIHTMLDocument2;
  BSTR url;
  Scheme_Object *retval;

  pIWebBrowser2 = MX_BROWSER_VAL (GUARANTEE_BROWSER ("current-url", 0));

  pIHTMLDocument2 = IHTMLDocument2FromBrowser(argv[0]);

  hr = pIHTMLDocument2->get_URL(&url);

  pIHTMLDocument2->Release();

  if (hr != S_OK) {
    codedComError("current-url: Error retrieving URL",hr);
  }

  if (url == NULL) {
    scheme_signal_error("current-url: NULL URL");
  }

  retval = BSTRToSchemeString(url);

  SysFreeString(url);

  return retval;
}
