#ifndef _COM_GLUE_H_
#define _COM_GLUE_H_

#ifdef FOR_GLUE

#include <initguid.h>

// {A604CB9C-2AB5-11D4-B6D3-0060089002FE}
DEFINE_GUID(CLSID_TypeLib, 0xA604CB9C, 0x2ab5, 0x11d4, 0xb6, 0xd3, 0x00, 0x60, 0x08, 0x90, 0x02, 0xfe);

// {A3B0AF9E-2AB0-11D4-B6D2-0060089002FE}
DEFINE_GUID(CLSID_IMzObj, 0xA3B0AF9E, 0x2ab0, 0x11d4, 0xb6, 0xd2, 0x00, 0x60, 0x08, 0x90, 0x02, 0xfe);

// {A604CBA8-2AB5-11D4-B6D3-0060089002FE}
DEFINE_GUID(IID_IMzObj, 0xA604CBA8, 0x2ab5, 0x11d4, 0xb6, 0xd3, 0x00, 0x60, 0x08, 0x90, 0x02, 0xfe);

#undef  INTERFACE
#define INTERFACE IMzObj
DECLARE_INTERFACE_ (INTERFACE, IDispatch)
{
	// IUnknown functions
	STDMETHOD  (QueryInterface)		(THIS_ REFIID, void **) PURE;
	STDMETHOD_ (ULONG, AddRef)		(THIS) PURE;
	STDMETHOD_ (ULONG, Release)		(THIS) PURE;
	// IDispatch functions
	STDMETHOD_ (ULONG, GetTypeInfoCount)(THIS_ UINT *) PURE;
	STDMETHOD_ (ULONG, GetTypeInfo)		(THIS_ UINT, LCID, ITypeInfo **) PURE;
	STDMETHOD_ (ULONG, GetIDsOfNames)	(THIS_ REFIID, LPOLESTR *, UINT, LCID, DISPID *) PURE;
	STDMETHOD_ (ULONG, Invoke)			(THIS_ DISPID, REFIID, LCID, WORD, DISPPARAMS *, VARIANT *, EXCEPINFO *, UINT *) PURE;
	// Extra functions
	STDMETHOD  (Eval)			(THIS_ BSTR, BSTR *) PURE;
	STDMETHOD  (About)			(THIS) PURE;
	STDMETHOD  (Reset)			(THIS) PURE;
};

// {A604CBA9-2AB5-11D4-B6D3-0060089002FE}
DEFINE_GUID(DIID_IMzObjEvents, 0xA604CBA9, 0x2ab5, 0x11d4, 0xb6, 0xd3, 0x00, 0x60, 0x08, 0x90, 0x02, 0xfe);

#undef  INTERFACE
#define INTERFACE IMzObjEvents
DECLARE_INTERFACE_ (INTERFACE, IDispatch)
{
	// IUnknown functions
	STDMETHOD  (QueryInterface)		(THIS_ REFIID, void **) PURE;
	STDMETHOD_ (ULONG, AddRef)		(THIS) PURE;
	STDMETHOD_ (ULONG, Release)		(THIS) PURE;
	// IDispatch functions
	STDMETHOD_ (ULONG, GetTypeInfoCount)(THIS_ UINT *) PURE;
	STDMETHOD_ (ULONG, GetTypeInfo)		(THIS_ UINT, LCID, ITypeInfo **) PURE;
	STDMETHOD_ (ULONG, GetIDsOfNames)	(THIS_ REFIID, LPOLESTR *, UINT, LCID, DISPID *) PURE;
	STDMETHOD_ (ULONG, Invoke)			(THIS_ DISPID, REFIID, LCID, WORD, DISPPARAMS *, VARIANT *, EXCEPINFO *, UINT *) PURE;
	// Extra functions
	STDMETHOD  (SchemeError)                (THIS_ BSTR *) PURE;
};

#else

typedef struct IMzObj { int dummy; } IMzObj;

#endif

extern HRESULT com_register();
extern int com_unregister();
extern int com_can_unregister();
extern const GUID com_get_class_iid();

extern void *new_mzobj(IMzObj*);
extern void delete_mzobj(void*);
extern HRESULT mzobj_about(void*);
extern HRESULT mzobj_reset(void*);
extern HRESULT mzobj_eval(void*, BSTR, BSTR*);

extern VOID Fire_SchemeError(IMzObj *com_obj, BSTR description);

#endif // _COM_GLUE_H_
