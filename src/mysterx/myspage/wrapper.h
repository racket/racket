#pragma once
#ifndef __WRAPPERS_H__
#define __WRAPPERS_H__

// based on Chris Sells' code at www.sellsbrothers.com/tools

class CWrapperDispatch :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CWrapperDispatch>,
    public IDispatch
{
public:
    void SetDispatch(IDispatch* pdisp) {
        m_spdisp = pdisp;
    }

DECLARE_NO_REGISTRY()
BEGIN_COM_MAP(CWrapperDispatch)
    COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()

// IDispatch

    STDMETHODIMP GetTypeInfoCount (UINT* pctinfo)
    {
      return m_spdisp != NULL
	  ? m_spdisp->GetTypeInfoCount (pctinfo)
	  : E_UNEXPECTED;
    }

    STDMETHODIMP GetTypeInfo (UINT itinfo, LCID lcid, ITypeInfo** pptinfo)
    {
      return pptinfo != NULL
	  ? ((* pptinfo = 0, m_spdisp != NULL)
	     ? m_spdisp->GetTypeInfo (itinfo, lcid, pptinfo)
	     : E_UNEXPECTED)
	  : E_POINTER;
    }

    STDMETHODIMP GetIDsOfNames (REFIID riid, LPOLESTR* rgszNames, UINT cNames,
				LCID lcid, DISPID* rgdispid)
    {
      return m_spdisp != NULL
	  ? m_spdisp->GetIDsOfNames (riid, rgszNames, cNames, lcid, rgdispid)
	  : E_UNEXPECTED;
    }

    STDMETHODIMP Invoke (DISPID dispidMember, REFIID riid, LCID lcid,
			 WORD wFlags, DISPPARAMS* pdispparams,
			 VARIANT* pvarResult, EXCEPINFO* pexcepinfo,
			 UINT* puArgErr)
    {
      return m_spdisp != NULL
	  ? m_spdisp->Invoke (dispidMember, riid, lcid,
			      wFlags, pdispparams,
			      pvarResult, pexcepinfo,
			      puArgErr)
	  : E_UNEXPECTED;
    }

private:
    CComPtr<IDispatch>  m_spdisp;
};

// this class override IDocHostUIHandler, to block context menus

class CWrapperUIHandler :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CWrapperUIHandler>,
    public IDocHostUIHandler
{
public:
    void SetUIHandler(IDocHostUIHandler* pHandler) {
        m_spHandler = pHandler;
    }

DECLARE_NO_REGISTRY()
BEGIN_COM_MAP(CWrapperUIHandler)
    COM_INTERFACE_ENTRY(IDocHostUIHandler)
END_COM_MAP()

// IDocHostUIHandler

  STDMETHODIMP ShowContextMenu (DWORD, POINT *,
			       IUnknown *,
			       IDispatch *)
  {
    return m_spHandler != NULL
	? S_OK // overrides default menu
	: E_UNEXPECTED;

  }

    STDMETHODIMP GetHostInfo (DOCHOSTUIINFO *pInfo)
    {
      return m_spHandler != NULL
	  ? m_spHandler->GetHostInfo (pInfo)
	  : E_UNEXPECTED;
    }

    STDMETHODIMP ShowUI (DWORD dwID,
			 IOleInPlaceActiveObject *pActiveObject,
			 IOleCommandTarget *pCommandTarget,
			 IOleInPlaceFrame *pFrame,
			 IOleInPlaceUIWindow *pDoc)
    {
      return m_spHandler != NULL
	  ? m_spHandler->ShowUI (dwID, pActiveObject,
				 pCommandTarget, pFrame, pDoc)
	  : E_UNEXPECTED;
    }

    STDMETHODIMP HideUI (void)
    {
      return m_spHandler != NULL
	  ? m_spHandler->HideUI()
	  : E_UNEXPECTED;
    }

    STDMETHODIMP UpdateUI (void)
    {
      return m_spHandler != NULL
	  ? m_spHandler->UpdateUI()
	  : E_UNEXPECTED;
    }

    STDMETHODIMP EnableModeless (BOOL fEnable)
    {
      return m_spHandler != NULL
	  ? m_spHandler->EnableModeless (fEnable)
	  : E_UNEXPECTED;
    }

    STDMETHODIMP OnDocWindowActivate (BOOL fActivate)
    {
      return m_spHandler != NULL
	  ? m_spHandler->OnDocWindowActivate (fActivate)
	  : E_UNEXPECTED;
    }

    STDMETHODIMP OnFrameWindowActivate (BOOL fActivate)
    {
      return m_spHandler != NULL
	  ? m_spHandler->OnFrameWindowActivate (fActivate)
	  : E_UNEXPECTED;
    }

    STDMETHODIMP ResizeBorder (LPCRECT prcBorder,
			       IOleInPlaceUIWindow *pUIWindow, BOOL fFrameWindow)
    {
      return m_spHandler != NULL
	  ? m_spHandler->ResizeBorder (prcBorder, pUIWindow, fFrameWindow)
	  : E_UNEXPECTED;
    }

    STDMETHODIMP TranslateAccelerator (LPMSG lpMsg,
				       const GUID __RPC_FAR *pguidCmdGroup,
				       DWORD nCmdID)
    {
      return m_spHandler != NULL
	  ? m_spHandler->TranslateAccelerator (lpMsg, pguidCmdGroup, nCmdID)
	  : E_UNEXPECTED;
    }

    STDMETHODIMP GetOptionKeyPath (LPOLESTR *pchKey, DWORD dw)
    {
      return m_spHandler != NULL
	  ? m_spHandler->GetOptionKeyPath (pchKey,dw)
	  : E_UNEXPECTED;
    }

    STDMETHODIMP GetDropTarget (IDropTarget *pDropTarget, IDropTarget **ppDropTarget)
    {
      return m_spHandler != NULL
	  ? m_spHandler->GetDropTarget (pDropTarget, ppDropTarget)
	  : E_UNEXPECTED;
    }

    STDMETHODIMP GetExternal (IDispatch **ppDispatch)
    {
      return m_spHandler != NULL
	  ? m_spHandler->GetExternal (ppDispatch)
	  : E_UNEXPECTED;
 }

    STDMETHODIMP TranslateUrl (DWORD dwTranslate,
			       OLECHAR  *pchURLIn,
			       OLECHAR  **ppchURLOut)
    {
      return m_spHandler != NULL
	  ? m_spHandler->TranslateUrl (dwTranslate, pchURLIn, ppchURLOut)
	  : E_UNEXPECTED;
    }

    STDMETHODIMP FilterDataObject (IDataObject *pDO,
				   IDataObject **ppDORet)
    {
      return m_spHandler != NULL
	  ? m_spHandler->FilterDataObject (pDO,ppDORet)
	  : E_UNEXPECTED;
    }

private:
  CComPtr<IDocHostUIHandler> m_spHandler;
};

#endif	// __WRAPPERS_H__


