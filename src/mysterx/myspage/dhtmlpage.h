// DHTMLPage.h : Declaration of the CDHTMLPage

#ifndef __DHTMLPAGE_H_
#define __DHTMLPAGE_H_

#define _ATL_NO_DOCHOSTUIHANDLER

#include "resource.h"       // main symbols
#include <atlctl.h>
#include <stdio.h>
#include <stdlib.h>
#include <mshtml.h>

#define sizeray(a) (sizeof(a)/sizeof(*a))

void failureBox(const char *s);

typedef struct _event_map_ {
  WCHAR *name;
  EVENT_TYPE eventType;
} EVENT_MAP;

extern EVENT_MAP eventMap[11];

/////////////////////////////////////////////////////////////////////////////
// CDHTMLPage
class ATL_NO_VTABLE CDHTMLPage : 
public CComObjectRootEx<CComSingleThreadModel>,
  public IDispatchImpl<IDHTMLPage, &IID_IDHTMLPage, &LIBID_MYSPAGELib>,
  public IDispatchImpl<IDHTMLPageUI, &IID_IDHTMLPageUI, &LIBID_MYSPAGELib>,
	public CComControl<CDHTMLPage>,
	public IPersistStreamInitImpl<CDHTMLPage>,
	public IOleControlImpl<CDHTMLPage>,
	public IOleObjectImpl<CDHTMLPage>,
	public IOleInPlaceActiveObjectImpl<CDHTMLPage>,
	public IViewObjectExImpl<CDHTMLPage>,
	public IOleInPlaceObjectWindowlessImpl<CDHTMLPage>,
	public IPersistStorageImpl<CDHTMLPage>,
	public ISpecifyPropertyPagesImpl<CDHTMLPage>,
	public IQuickActivateImpl<CDHTMLPage>,
	public IDataObjectImpl<CDHTMLPage>,
  public IProvideClassInfo2Impl<&CLSID_DHTMLPage, NULL, &LIBID_MYSPAGELib>,
	public CComCoClass<CDHTMLPage, &CLSID_DHTMLPage>
{

private:
  IHTMLWindow2 *pTopWindow;

public:
	CDHTMLPage()
	{
	  m_bWindowOnly = TRUE;
	  pTopWindow = NULL;
	}

DECLARE_REGISTRY_RESOURCEID(IDR_DHTMLPAGE)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CDHTMLPage)
	COM_INTERFACE_ENTRY(IDHTMLPage)
	COM_INTERFACE_ENTRY(IDHTMLPageUI)
	COM_INTERFACE_ENTRY2(IDispatch, IDHTMLPage)
	COM_INTERFACE_ENTRY(IViewObjectEx)
	COM_INTERFACE_ENTRY(IViewObject2)
	COM_INTERFACE_ENTRY(IViewObject)
	COM_INTERFACE_ENTRY(IOleInPlaceObjectWindowless)
	COM_INTERFACE_ENTRY(IOleInPlaceObject)
	COM_INTERFACE_ENTRY2(IOleWindow, IOleInPlaceObjectWindowless)
	COM_INTERFACE_ENTRY(IOleInPlaceActiveObject)
	COM_INTERFACE_ENTRY(IOleControl)
	COM_INTERFACE_ENTRY(IOleObject)
	COM_INTERFACE_ENTRY(IPersistStreamInit)
	COM_INTERFACE_ENTRY2(IPersist, IPersistStreamInit)
	COM_INTERFACE_ENTRY(ISpecifyPropertyPages)
	COM_INTERFACE_ENTRY(IQuickActivate)
	COM_INTERFACE_ENTRY(IPersistStorage)
	COM_INTERFACE_ENTRY(IDataObject)
	COM_INTERFACE_ENTRY(IProvideClassInfo)
	COM_INTERFACE_ENTRY(IProvideClassInfo2)
END_COM_MAP()

BEGIN_PROP_MAP(CDHTMLPage)
	PROP_DATA_ENTRY("_cx", m_sizeExtent.cx, VT_UI4)
	PROP_DATA_ENTRY("_cy", m_sizeExtent.cy, VT_UI4)
	// Example entries
	// PROP_ENTRY("Property Description", dispid, clsid)
	// PROP_PAGE(CLSID_StockColorPage)
END_PROP_MAP()

BEGIN_MSG_MAP(CDHTMLPage)
	MESSAGE_HANDLER(WM_CREATE, OnCreate)
	CHAIN_MSG_MAP(CComControl<CDHTMLPage>)
END_MSG_MAP()
// Handler prototypes:
//  LRESULT MessageHandler(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
//  LRESULT CommandHandler(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled);
//  LRESULT NotifyHandler(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);

// IViewObjectEx
	DECLARE_VIEW_STATUS(0)

// IDHTMLPage
public:

// IDHTMLPageUI

public:
 STDMETHOD(marshalEventQueueToStream)(IStream **);
 STDMETHOD(marshalWebBrowserToStream)(IStream **);
 CComPtr<IWebBrowser2> m_spBrowser;
 IEventQueue *pIEventQueue;
 LRESULT OnCreate(UINT,WPARAM,LPARAM,BOOL&);
 STDMETHOD(AtAnyEvent)(void);
 STDMETHOD(SuppressCtxMenu)(IDispatch *);
};

#endif //__DHTMLPAGE_H_
