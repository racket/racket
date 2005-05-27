// Event.h : Declaration of the CEvent

#ifndef __EVENT_H_
#define __EVENT_H_

#include "resource.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CEvent
class ATL_NO_VTABLE CEvent : 
	public CComObjectRootEx<CComSingleThreadModel>,
	public CComCoClass<CEvent, &CLSID_Event>,
	public IDispatchImpl<IEvent, &IID_IEvent, &LIBID_MYSPAGELib>
{

private:

  EVENT_TYPE eventType;
  BSTR srcIdName,srcTagName;
  BSTR fromIdName,fromTagName;
  BSTR toIdName,toTagName;
  VARIANT_BOOL altPressed,ctrlPressed,shiftPressed;
  MOUSE_BUTTON mouseButton;
  long keyCode;
  long x,y;

public:
  CEvent(void);
  
DECLARE_REGISTRY_RESOURCEID(IDR_EVENT)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CEvent)
	COM_INTERFACE_ENTRY(IEvent)
	COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()

// IEvent
public:
	STDMETHOD(get_y)(/*[out, retval]*/ long *pVal);
	STDMETHOD(put_y)(/*[in]*/ long newVal);
	STDMETHOD(get_x)(/*[out, retval]*/ long *pVal);
	STDMETHOD(put_x)(/*[in]*/ long newVal);
	STDMETHOD(get_mouseButton)(/*[out, retval]*/ MOUSE_BUTTON *pVal);
	STDMETHOD(put_mouseButton)(/*[in]*/ MOUSE_BUTTON newVal);
	STDMETHOD(get_keyCode)(/*[out, retval]*/ long *pVal);
	STDMETHOD(put_keyCode)(/*[in]*/ long newVal);
	STDMETHOD(get_shiftPressed)(/*[out, retval]*/ VARIANT_BOOL *pVal);
	STDMETHOD(put_shiftPressed)(/*[in]*/ VARIANT_BOOL newVal);
	STDMETHOD(get_ctrlPressed)(/*[out, retval]*/ VARIANT_BOOL *pVal);
	STDMETHOD(put_ctrlPressed)(/*[in]*/ VARIANT_BOOL newVal);
	STDMETHOD(get_altPressed)(/*[out, retval]*/ VARIANT_BOOL *pVal);
	STDMETHOD(put_altPressed)(/*[in]*/ VARIANT_BOOL newVal);
	STDMETHOD(get_toId)(/*[out, retval]*/ BSTR *pVal);
	STDMETHOD(put_toId)(/*[in]*/ BSTR newVal);
	STDMETHOD(get_toTag)(/*[out, retval]*/ BSTR *pVal);
	STDMETHOD(put_toTag)(/*[in]*/ BSTR newVal);
	STDMETHOD(get_fromId)(/*[out, retval]*/ BSTR *pVal);
	STDMETHOD(put_fromId)(/*[in]*/ BSTR newVal);
	STDMETHOD(get_fromTag)(/*[out, retval]*/ BSTR *pVal);
	STDMETHOD(put_fromTag)(/*[in]*/ BSTR newVal);
	STDMETHOD(get_srcId)(/*[out, retval]*/ BSTR *pVal);
	STDMETHOD(put_srcId)(/*[in]*/ BSTR newVal);
	STDMETHOD(get_srcTag)(/*[out, retval]*/BSTR *pVal);
	STDMETHOD(put_srcTag)(/*[in]*/BSTR newVal);
	STDMETHOD(get_eventType)(/*[out, retval]*/ EVENT_TYPE *pVal);
	STDMETHOD(put_eventType)(/*[in]*/ EVENT_TYPE newVal);
};

#endif //__EVENT_H_
