// Sink.h : Declaration of the CSink

#ifndef __SINK_H_
#define __SINK_H_

#include "resource.h"       // main symbols
#include "sinktbl.h"

#define EVENT_HANDLER_TBL_SIZE (93)

typedef struct _event_handler_entry_ { // entry in hash table
  DISPID dispId;
  void *handler;
  _event_handler_entry_ *next;
} EVENT_HANDLER_ENTRY;

/////////////////////////////////////////////////////////////////////////////
// CSink
class ATL_NO_VTABLE CSink :
        public CComObjectRootEx<CComSingleThreadModel>,
	public CComCoClass<CSink, &CLSID_Sink>,
	public IDispatchImpl<ISink, &IID_ISink, &LIBID_MYSSINKLib>
{

private:
  MYSSINK_TABLE *myssink_table;

  void *variantToSchemeObject(VARIANTARG *);
  void unmarshalSchemeObject(void *,VARIANTARG *);
  void handlerUpdateError(char *);

  unsigned int getHashValue(DISPID);
  EVENT_HANDLER_ENTRY *newEventHandlerEntry(DISPID,void *);
  EVENT_HANDLER_ENTRY *lookupHandler(DISPID);

  EVENT_HANDLER_ENTRY eventHandlerTable[EVENT_HANDLER_TBL_SIZE];

public:
  CSink();
  ~CSink();

DECLARE_REGISTRY_RESOURCEID(IDR_SINK)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CSink)
  COM_INTERFACE_ENTRY(ISink)
  COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()

// ISink
public:
 STDMETHOD(set_myssink_table)(void *);
 STDMETHOD(register_handler)(DISPID,void *);
 STDMETHOD(unregister_handler)(DISPID);

 //override ATL implementations

 STDMETHOD(Invoke)(DISPID,REFIID,LCID,WORD,
		   DISPPARAMS*,VARIANT*,EXCEPINFO*,UINT*);

 STDMETHOD(InternalQueryInterface)(void *, const _ATL_INTMAP_ENTRY* pEntries,REFIID,void **);
};

#endif //__SINK_H_

