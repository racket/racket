// TestControl.h : Declaration of the CTestControl

#ifndef __TESTCONTROL_H_
#define __TESTCONTROL_H_

#include "resource.h"       // main symbols
#include <windowsx.h>
#include <atlctl.h>
#include "testobjectCP.h"


/////////////////////////////////////////////////////////////////////////////
// CTestControl
class ATL_NO_VTABLE CTestControl :
	public CComObjectRootEx<CComSingleThreadModel>,
	public CStockPropImpl<CTestControl, ITestControl, &IID_ITestControl, &LIBID_TESTOBJECTLib>,
	public CComControl<CTestControl>,
	public IPersistStreamInitImpl<CTestControl>,
	public IOleControlImpl<CTestControl>,
	public IOleObjectImpl<CTestControl>,
	public IOleInPlaceActiveObjectImpl<CTestControl>,
	public IViewObjectExImpl<CTestControl>,
	public IOleInPlaceObjectWindowlessImpl<CTestControl>,
	public IConnectionPointContainerImpl<CTestControl>,
	public IPersistStorageImpl<CTestControl>,
	public ISpecifyPropertyPagesImpl<CTestControl>,
	public IQuickActivateImpl<CTestControl>,
	public IDataObjectImpl<CTestControl>,
	public IProvideClassInfo2Impl<&CLSID_TestControl, &DIID__ITestControlEvents, &LIBID_TESTOBJECTLib>,
	public IPropertyNotifySinkCP<CTestControl>,
	public CComCoClass<CTestControl, &CLSID_TestControl>,
	public CProxy_ITestControlEvents< CTestControl >
{
private:
  long the_value;
public:
	CTestControl()
	{
	  the_value = 0L;
	}

DECLARE_REGISTRY_RESOURCEID(IDR_TESTCONTROL)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CTestControl)
	COM_INTERFACE_ENTRY(ITestControl)
	COM_INTERFACE_ENTRY(IDispatch)
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
	COM_INTERFACE_ENTRY(IConnectionPointContainer)
	COM_INTERFACE_ENTRY(ISpecifyPropertyPages)
	COM_INTERFACE_ENTRY(IQuickActivate)
	COM_INTERFACE_ENTRY(IPersistStorage)
	COM_INTERFACE_ENTRY(IDataObject)
	COM_INTERFACE_ENTRY(IProvideClassInfo)
	COM_INTERFACE_ENTRY(IProvideClassInfo2)
	COM_INTERFACE_ENTRY_IMPL(IConnectionPointContainer)
END_COM_MAP()

BEGIN_PROP_MAP(CTestControl)
	PROP_DATA_ENTRY("_cx", m_sizeExtent.cx, VT_UI4)
	PROP_DATA_ENTRY("_cy", m_sizeExtent.cy, VT_UI4)
	PROP_ENTRY("Caption", DISPID_CAPTION, CLSID_NULL)
	// Example entries
	// PROP_ENTRY("Property Description", dispid, clsid)
	// PROP_PAGE(CLSID_StockColorPage)
END_PROP_MAP()

BEGIN_CONNECTION_POINT_MAP(CTestControl)
	CONNECTION_POINT_ENTRY(IID_IPropertyNotifySink)
	CONNECTION_POINT_ENTRY(DIID__ITestControlEvents)
END_CONNECTION_POINT_MAP()

BEGIN_MSG_MAP(CTestControl)
	CHAIN_MSG_MAP(CComControl<CTestControl>)
	DEFAULT_REFLECTION_HANDLER()
	MESSAGE_HANDLER(WM_LBUTTONDOWN, OnLButtonDown)
	MESSAGE_HANDLER(WM_LBUTTONUP, OnLButtonUp)
	MESSAGE_HANDLER(WM_MBUTTONDOWN, OnMButtonDown)
	MESSAGE_HANDLER(WM_MBUTTONUP, OnMButtonUp)
	MESSAGE_HANDLER(WM_RBUTTONDOWN, OnRButtonDown)
	MESSAGE_HANDLER(WM_RBUTTONUP, OnRButtonUp)
	MESSAGE_HANDLER(WM_MOUSEMOVE, OnMouseMove)
END_MSG_MAP()
// Handler prototypes:
//  LRESULT MessageHandler(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
//  LRESULT CommandHandler(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled);
//  LRESULT NotifyHandler(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);



// IViewObjectEx
	DECLARE_VIEW_STATUS(VIEWSTATUS_SOLIDBKGND | VIEWSTATUS_OPAQUE)

// ITestControl
public:
	STDMETHOD(FloatTest)(double n1,double n2,/*[out,retval]*/double *n3);
	STDMETHOD(UnsignedTest)(unsigned n1,unsigned n2,/*[out,retval]*/unsigned *n3);
	STDMETHOD(ShortTest)(short int n1,short int n2,/*[out,retval]*/short int *n3);
	STDMETHOD(StringTest)(BSTR s1,BSTR s2,/*[out,retval]*/BSTR *s3);
	STDMETHOD(AddTest)(long n1,long *n2,/*[out,retval]*/long *n3);
	STDMETHOD(get_Numprop)(long ndx,long *retVal);
	STDMETHOD(put_Numprop)(long ndx,long newVal);

	HRESULT OnDraw (ATL_DRAWINFO& di)
	{
		RECT& rc = *(RECT*)di.prcBounds;
		Rectangle(di.hdcDraw, rc.left, rc.top, rc.right, rc.bottom);

		SetTextAlign(di.hdcDraw, TA_CENTER|TA_BASELINE);
		LPCTSTR pszText = _T("MysterX Test Control");
		TextOut(di.hdcDraw,
			(rc.left + rc.right) / 2,
			(rc.top + rc.bottom) / 2,
			pszText,
			lstrlen(pszText));

		return S_OK;
	}

	CComBSTR m_bstrCaption;

	LRESULT OnLButtonDown(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
	  Fire_MouseDown (0x1, wParam, GET_X_LPARAM (lParam), GET_Y_LPARAM (lParam));
	  Fire_Click();
	  return DefWindowProc (uMsg, wParam, lParam);
	}

	LRESULT OnLButtonUp(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
	  Fire_MouseUp (0x1, wParam, GET_X_LPARAM (lParam), GET_Y_LPARAM (lParam));
	  return DefWindowProc (uMsg, wParam, lParam);
	}

	LRESULT OnMButtonDown (UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
	  Fire_MouseDown (0x4, wParam, GET_X_LPARAM (lParam), GET_Y_LPARAM (lParam));
	  Fire_Click();
	  return DefWindowProc (uMsg, wParam, lParam);
	}

	LRESULT OnMButtonUp (UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
	  Fire_MouseUp (0x4, wParam, GET_X_LPARAM (lParam), GET_Y_LPARAM (lParam));
	  return DefWindowProc (uMsg, wParam, lParam);
	}

	LRESULT OnRButtonDown (UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
	  Fire_MouseDown (0x2, wParam, GET_X_LPARAM (lParam), GET_Y_LPARAM (lParam));
	  Fire_Click();
	  return DefWindowProc(uMsg, wParam, lParam);
	}

	LRESULT OnRButtonUp (UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
	  Fire_MouseUp (0x2, wParam, GET_X_LPARAM (lParam), GET_Y_LPARAM (lParam));
	  return DefWindowProc (uMsg, wParam, lParam);
	}

	LRESULT OnMouseMove (UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
	  short button;
	  short shift;

	  button = wParam & (MK_LBUTTON | MK_MBUTTON | MK_RBUTTON);
	  shift = wParam & (MK_CONTROL | MK_SHIFT);

	  Fire_MouseMove (button, shift, GET_X_LPARAM (lParam), GET_Y_LPARAM (lParam));

	  return DefWindowProc (uMsg, wParam, lParam);
	}
};

#endif //__TESTCONTROL_H_
