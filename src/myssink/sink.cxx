// Sink.cxx : Implementation of CSink

#include "stdafx.h"
#include <stdio.h>
#include <process.h>

#include "myssink.h"
#include "sink.h"

/////////////////////////////////////////////////////////////////////////////
// CSink

// private methods

unsigned int CSink::getHashValue(DISPID dispId) {

  // casting dispId guarantees positive result

  return (unsigned int)((ULONG)dispId % EVENT_HANDLER_TBL_SIZE);
}

EVENT_HANDLER_ENTRY *CSink::newEventHandlerEntry(DISPID dispId, void *handler) {
  EVENT_HANDLER_ENTRY *p;
  p = (EVENT_HANDLER_ENTRY *)malloc(sizeof(EVENT_HANDLER_ENTRY));
  p->dispId = dispId;
  p->handler = handler;
  p->next = NULL;

  return p;
}

EVENT_HANDLER_ENTRY *CSink::lookupHandler(DISPID dispId) {
  unsigned int hashVal;
  EVENT_HANDLER_ENTRY *p;

  hashVal = getHashValue(dispId);

  p = &eventHandlerTable[hashVal];

  while (p) {
    if (p->dispId == dispId) {
      return p;
    }
    p = p->next;
  }

  return NULL;
}

// constructor

CSink::CSink(void) {
  memset(eventHandlerTable,0,sizeof(eventHandlerTable));
}

// destructor

CSink::~CSink(void) {
  EVENT_HANDLER_ENTRY *p,*psave;
  int i;

  for (i = 0; i < EVENT_HANDLER_TBL_SIZE; i++) {
    p = &eventHandlerTable[i];
    while (p != NULL) {
      if (p->handler) {
	sink_release_handler(p->handler);
	p->handler = NULL;
      }
      psave = p;
      p = p->next;
      free(psave);
    }
  }
}

STDMETHODIMP CSink::set_myssink_table(void * p) {
  myssink_table = (MYSSINK_TABLE *)p;
  return S_OK;
}

STDMETHODIMP CSink::register_handler(DISPID dispId,void * handler) {
  unsigned int hashVal;
  EVENT_HANDLER_ENTRY *p;

  hashVal = getHashValue(dispId);

  p = &eventHandlerTable[hashVal];

  if (p->dispId == (DISPID)0) {
    p->dispId = dispId;
    p->handler = handler;
    p->next = NULL;
  }
  else {

    while (p != NULL) {

      if (p->dispId == dispId) { // update existing entry
	if (p->handler)
	  sink_release_handler(p->handler);
	p->handler = handler;
	return S_OK;
      }

      p = p->next;
    }

    p->next = newEventHandlerEntry(dispId, handler);
  }

  return S_OK;
}

STDMETHODIMP CSink::unregister_handler(DISPID dispId) {
  unsigned int hashVal;
  EVENT_HANDLER_ENTRY *p;

  hashVal = getHashValue(dispId);

  p = &eventHandlerTable[hashVal];

  if (p->dispId == (DISPID)0) { // no handler installed
    return S_OK;
  }

  while (p != NULL) {
    if (p->dispId == dispId) { // set existing entry to NULL
      if (p->handler) {
	sink_release_handler(p->handler);
	p->handler = NULL;
      }
      return S_OK;
    }

    p = p->next;
  }

  return S_OK;
}

void *CSink::variantToSchemeObject(VARIANTARG *pVariantArg) {
  return sink_variant_to_scheme(pVariantArg);
}

void CSink::unmarshalSchemeObject(void *obj,VARIANTARG *pVariantArg) {
  sink_unmarshal_scheme(obj, pVariantArg);
}

// effectively, override default implementation of IDispatch::QueryInterface

HRESULT CSink::InternalQueryInterface(void *pThis,
				      const _ATL_INTMAP_ENTRY* pEntries,
				      REFIID riid,
				      void **ppVoid) {

  // marshalling IID's end in 0000-0000-C0000-000000000046
  // these seem to be requested by VB and VC++/ATL event sources
  // the sink doesn't implement those, and doesn't need to

  if (riid != IID_IUnknown && riid != IID_IDispatch && riid != IID_ISink) {
    LPOLESTR str;
    BOOL isSystemIID;

    StringFromIID(riid,&str);

    str[37] = L'\0';

    isSystemIID = (_wcsicmp(str + 10,L"0000-0000-C000-000000000046") == 0);

    CoTaskMemFree(str);

    if (isSystemIID) {
      return E_NOINTERFACE;
    }
  }

  // Use IUnknown pointer for IUnknown, ISink, and the outbound interface

  return CComObjectRootEx<CComSingleThreadModel>::InternalQueryInterface(pThis,pEntries,IID_IUnknown,ppVoid);

}

// override default implementation of IDispatch::Invoke

typedef struct _named_args_ {
  DISPID dispId;
  VARIANTARG *pVariantArg;
  unsigned int index;
} NAMEDARG;

int cmpNamedArgs(NAMEDARG *p1,NAMEDARG *p2) {
  return (int)p1->dispId - (int)p2->dispId;
}

HRESULT CSink::Invoke(DISPID dispId, REFIID, LCID, WORD,
                      DISPPARAMS* pDispParams,
                      VARIANT*, EXCEPINFO*, UINT*) {

  void *handler;
  EVENT_HANDLER_ENTRY *p;
  VARIANTARG *pCurrArg;
  NAMEDARG namedArgs[MAXINVOKEARGS];
  UINT numParams,actualParams,positionalParams,namedParams;
  void *argv[MAXINVOKEARGS];
  UINT i;
  UINT j;

  p = lookupHandler(dispId);

  if (p == NULL) { // nothing registered
    return S_OK;
  }

  handler = p->handler;

  if (handler == NULL) { // handler was unregistered
    return S_OK;
  }

  numParams = pDispParams->cArgs;

  if (numParams > MAXINVOKEARGS) {
    return DISP_E_TYPEMISMATCH;
  }

  namedParams = pDispParams->cNamedArgs;

  if (namedParams > 0) {
    for (i = 0; i < namedParams; i++) {
      namedArgs[i].dispId = pDispParams->rgdispidNamedArgs[i];
      namedArgs[i].pVariantArg = &pDispParams->rgvarg[i];
    }

    qsort(namedArgs,namedParams,sizeof(NAMEDARG),
	  (int (*)(const void *,const void *))cmpNamedArgs);
  }

  /* memory layout of rgvargs:

    ---------------------------------
   | named params | required params  |
    ---------------------------------

     these are in reverse order from the order
     given to Racket

  */

  actualParams = 0;

  positionalParams = numParams - namedParams;

  for (i = 0; i < positionalParams; i++) {
    pCurrArg = &pDispParams->rgvarg[numParams - 1 - i];
    argv[i] = variantToSchemeObject(pCurrArg);
    if (!argv[i]) {
      for (i = 0; i < actualParams; i++) {
	sink_release_arg(argv[i]);
      }
      return S_OK;
    }
    actualParams++;
  }

  int ii = positionalParams;
  j = 0;

  while (j < namedParams) {

    if (ii >= MAXINVOKEARGS) {
      return DISP_E_TYPEMISMATCH;
    }

    while(ii < namedArgs[j].dispId) {
      if (ii >= MAXINVOKEARGS) {
	return DISP_E_TYPEMISMATCH;
      }

      argv[ii] = make_scode(DISP_E_PARAMNOTFOUND);
      ii++,actualParams++;
    }

    argv[ii] = variantToSchemeObject(namedArgs[j].pVariantArg);
    if (!argv[ii]) {
      for (i = 0; i < actualParams; i++) {
	sink_release_arg(argv[i]);
      }
      return S_OK;
    }
    namedArgs[j].index = ii;
    ii++,j++,actualParams++;
  }

  (void)sink_apply(handler,actualParams,argv);

  // updating of boxes needs to be reflected in BYREF parameters

  for (i = 0; i < positionalParams; i++) {
    pCurrArg = &pDispParams->rgvarg[numParams - 1 - i];
    unmarshalSchemeObject(argv[i],pCurrArg);
  }

  for (i = 0; i < namedParams; i++) {
    pCurrArg = namedArgs[i].pVariantArg;
    j = namedArgs[i].index;
    unmarshalSchemeObject(argv[j],pCurrArg);
  }

  return S_OK;
}

