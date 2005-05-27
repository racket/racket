// Event.cxx : Implementation of CEvent
#include "stdafx.h"
#include "myspage.h"
#include "Event.h"

/////////////////////////////////////////////////////////////////////////////
// CEvent

CEvent::CEvent(void) { 
  srcIdName = NULL; 
  srcTagName = NULL; 
  fromIdName = NULL; 
  fromTagName = NULL; 
  toIdName = NULL; 
  toTagName = NULL; 
}

STDMETHODIMP CEvent::get_eventType(EVENT_TYPE *pVal) {
  *pVal = eventType;
  return S_OK;
}

STDMETHODIMP CEvent::put_eventType(EVENT_TYPE newVal) {
  eventType = newVal;
  return S_OK;
}

STDMETHODIMP CEvent::get_srcTag(BSTR *pVal) {
  *pVal = srcTagName;
  
  return S_OK;
}

STDMETHODIMP CEvent::put_srcTag(BSTR newVal) {
  if (srcTagName) {
    SysFreeString(srcTagName);
  }
  
  srcTagName = SysAllocString(newVal);
  
  return S_OK;
}

STDMETHODIMP CEvent::get_srcId(BSTR *pVal)
{
  
  *pVal = srcIdName;
  
  return S_OK;
}

STDMETHODIMP CEvent::put_srcId(BSTR newVal)
{
  if (srcIdName) {
    SysFreeString(srcTagName);
  }
  
  srcIdName = SysAllocString(newVal);
  
  return S_OK;
}

STDMETHODIMP CEvent::get_fromTag(BSTR *pVal) {
  
  *pVal = fromTagName;
  
  return S_OK;
}

STDMETHODIMP CEvent::put_fromTag(BSTR newVal) {
  
  if (fromTagName) {
    SysFreeString(fromTagName);
  }
  
  fromTagName = SysAllocString(newVal);
  
  return S_OK;
}

STDMETHODIMP CEvent::get_fromId(BSTR *pVal) {
  
  *pVal = fromIdName;
  
  return S_OK;
}

STDMETHODIMP CEvent::put_fromId(BSTR newVal) {
  
  if (fromIdName) {
    SysFreeString(fromIdName);
  }
  
  fromIdName = SysAllocString(newVal);
  
  return S_OK;
}

STDMETHODIMP CEvent::get_toTag(BSTR *pVal) {
  
  *pVal = toTagName;
  
  return S_OK;
}

STDMETHODIMP CEvent::put_toTag(BSTR newVal) {
  
  if (toTagName) {
    SysFreeString(toTagName);
  }
  
  toTagName = SysAllocString(newVal);
  
  return S_OK;
}

STDMETHODIMP CEvent::get_toId(BSTR *pVal) {
  
  *pVal = toIdName;
  
  return S_OK;
}

STDMETHODIMP CEvent::put_toId(BSTR newVal) {
  
  if (toIdName) {
    SysFreeString(toIdName);
  }
  
  toIdName = SysAllocString(newVal);
  
  return S_OK;
}

STDMETHODIMP CEvent::get_keyCode(long *pVal) {
  
  *pVal = keyCode;
  
  return S_OK;
}

STDMETHODIMP CEvent::put_keyCode(long newVal) {
  
  keyCode = newVal;
  
  return S_OK;
}

STDMETHODIMP CEvent::get_altPressed(VARIANT_BOOL *pVal) {
  
  *pVal = altPressed;
  
  return S_OK;
}

STDMETHODIMP CEvent::put_altPressed(VARIANT_BOOL newVal) {
  
  altPressed = newVal;
  
  return S_OK;
}

STDMETHODIMP CEvent::get_ctrlPressed(VARIANT_BOOL *pVal) {
  
  *pVal = ctrlPressed;
  
  return S_OK;
}

STDMETHODIMP CEvent::put_ctrlPressed(VARIANT_BOOL newVal) {
  
  ctrlPressed = newVal;
  
  return S_OK;
}

STDMETHODIMP CEvent::get_shiftPressed(VARIANT_BOOL *pVal) {
  
  *pVal = shiftPressed;
  
  return S_OK;
}

STDMETHODIMP CEvent::put_shiftPressed(VARIANT_BOOL newVal) {
  
  shiftPressed = newVal;
  
  return S_OK;
}

STDMETHODIMP CEvent::get_mouseButton(MOUSE_BUTTON *pVal) {
  
  *pVal = mouseButton;
  
  return S_OK;
}

STDMETHODIMP CEvent::put_mouseButton(MOUSE_BUTTON newVal) {
  
  mouseButton = newVal;
  
  return S_OK;
}

STDMETHODIMP CEvent::get_x(long *pVal) {
  
  *pVal = x;
  
  return S_OK;
}

STDMETHODIMP CEvent::put_x(long newVal) {
  
  x = newVal;
  
  return S_OK;
}

STDMETHODIMP CEvent::get_y(long *pVal) {
  
  *pVal = y;
  
  return S_OK;
}

STDMETHODIMP CEvent::put_y(long newVal) {
  
  y = newVal;
  
  return S_OK;
}
