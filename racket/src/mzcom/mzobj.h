// MzObj.h : Declaration of the CMzObj

#ifndef __MZOBJ_H_
#define __MZOBJ_H_

#include "resource.h"       // main symbols

typedef struct {
  BSTR **ppInput;
  BSTR *pOutput;
  HRESULT *pHr;
  HANDLE readSem;
  HANDLE writeSem;
  HANDLE resetSem;
  HANDLE resetDoneSem;
  BOOL *pErrorState;
  BOOL *pResetFlag;
} THREAD_GLOBALS;

extern HINSTANCE globHinst;

/////////////////////////////////////////////////////////////////////////////
// CMzObj

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

class CMzObj
{

  private:

    void *com_obj;

    HRESULT hr;
    HANDLE inputMutex;
    HANDLE readSem;
    HANDLE writeSem;
    HANDLE resetSem;
    HANDLE resetDoneSem;
    HANDLE evalDoneSems[2];
    BSTR *globInput;
    BSTR globOutput;
    HANDLE threadHandle;
    BOOL errorState;
    
    void RaiseError(const OLECHAR *);
    BOOL testThread(void);
    void startMzThread(void);
    void killMzThread(void);

  public:

    CMzObj(void* com_obj);
    ~CMzObj(void);

// IMzObj
public:
 HRESULT Reset(void);
 HRESULT About(void);
 HRESULT Eval(BSTR input,/*[out,retval]*/BSTR *output);
};

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

#endif //__MZOBJ_H_

