// mzobj.cxx : Implementation of CMzObj

#ifdef MZCOM_3M
/* xform.rkt converts this file to mzobj3m.cxx: */
# define i64 /* ??? why does expansion produce i64? */
# include "mzobj3m.cxx"
#else

#include "scheme.h"

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

#include "resource.h"

#include <process.h>

#include <objbase.h>
extern "C" {
#include "com_glue.h"
};

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

#include "mzobj.h"

#ifndef MZ_PRECISE_GC
# define GC_CAN_IGNORE /* empty */
#endif

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static void ErrorBox(char *s) {
  ::MessageBox(NULL,s,"MzCOM",MB_OK);
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

static THREAD_GLOBALS tg;

static Scheme_Env *env;

static BOOL *pErrorState;
static OLECHAR *wideError;

static HANDLE evalLoopSems[2];
static HANDLE exitSem;

static Scheme_Object *exn_catching_apply;
static Scheme_Object *exn_p;
static Scheme_Object *exn_message;

static Scheme_At_Exit_Callback_Proc at_exit_callback;

/* This indirection lets us delayload libmzsch.dll: */
#define scheme_false (scheme_make_false())

static Scheme_Object *_apply_thunk_catch_exceptions(Scheme_Object *f,
                                                    Scheme_Object **exn) {
  Scheme_Object *v;

  v = _scheme_apply(exn_catching_apply,1,&f);

  /* v is a pair: (cons #t value) or (cons #f exn) */

  if (SCHEME_TRUEP(SCHEME_CAR(v))) {
    return SCHEME_CDR(v);
  }
  else {
    *exn = SCHEME_CDR(v);
    return NULL;
  }
}

static Scheme_Object *extract_exn_message(Scheme_Object *v) {
  if (SCHEME_TRUEP(_scheme_apply(exn_p,1,&v)))
    return _scheme_apply(exn_message,1,&v);
  else
    return NULL; /* Not an exn structure */
}

static Scheme_Object *do_eval(void *s,int,Scheme_Object **) {
  return scheme_eval_string_all((char *)s,env,TRUE);
}

static Scheme_Object *eval_string_or_get_exn_message(char *s) {
  Scheme_Object *v;
  Scheme_Object *exn;

  v = _apply_thunk_catch_exceptions(scheme_make_closed_prim(do_eval,s),&exn);
  /* value */
  if (v) {
    *pErrorState = FALSE;
    return v;
  }

  v = extract_exn_message(exn);
  /* exn */
  if (v) {
    *pErrorState = TRUE;
    return v;
  }

  /* `raise' was called on some arbitrary value */
  return exn;
}

OLECHAR *wideStringFromSchemeObj(Scheme_Object *obj,char *fmt,int fmtlen) {
  char *s;
  OLECHAR *wideString;
  int len;

  s = scheme_format_utf8(fmt,fmtlen,1,&obj,NULL);
  len = strlen(s);
  wideString = (OLECHAR *)scheme_malloc((len + 1) * sizeof(OLECHAR));
  MultiByteToWideChar(CP_ACP,(DWORD)0,s,len,wideString,len + 1);
  wideString[len] = 0;
  return wideString;
}

void exitHandler(int) {
  if (at_exit_callback) at_exit_callback();
  ReleaseSemaphore(exitSem,1,NULL);
  _endthreadex(0);
}

void setupSchemeEnv(Scheme_Env *in_env)
{
  char *wrapper;
  char exeBuff[260];
  HMODULE mod;
  static BOOL registered;

  if (!registered) {
    scheme_register_static(&env,sizeof(env));
    scheme_register_static(&exn_catching_apply,sizeof(exn_catching_apply));
    scheme_register_static(&exn_p,sizeof(exn_p));
    scheme_register_static(&exn_message,sizeof(exn_message));
    registered = TRUE;
  }

  env = in_env;

  if (env == NULL) {
    ErrorBox("Can't create Racket environment");
    _endthreadex(0);
  }

  // set up collection paths, based on Racket startup

  mod = GetModuleHandle("mzcom.exe");
  GetModuleFileName(mod,exeBuff,sizeof(exeBuff));

  scheme_add_global("mzcom-exe",scheme_make_utf8_string(exeBuff),env);
  scheme_set_exec_cmd(exeBuff);
  scheme_set_collects_path(scheme_make_path("../collects"));
  scheme_set_config_path(scheme_make_path("../etc"));
  scheme_init_collection_paths(env, scheme_make_null());

  // initialize namespace

  scheme_namespace_require(scheme_intern_symbol("racket"));

  // set up exception trapping

  wrapper =
    "(lambda (thunk) "
    "(with-handlers ([void (lambda (exn) (cons #f exn))]) "
    "(cons #t (thunk))))";

  exn_catching_apply = scheme_eval_string(wrapper,env);
  exn_p = scheme_builtin_value("exn?");
  exn_message = scheme_builtin_value("exn-message");
}

static int do_evalLoop(Scheme_Env *env, int argc, char **_args)
{
  LPVOID args = (LPVOID)_args;
  HRESULT *pHr;
  BOOL doEval;
  UINT len;
  DWORD waitVal;
  char *narrowInput;
  Scheme_Object *outputObj;
  Scheme_Object *sleepFun;
  OLECHAR *outputBuffer;
  THREAD_GLOBALS *pTg;
  HANDLE readSem;
  HANDLE writeSem;
  HANDLE resetSem;
  HANDLE resetDoneSem;
  BSTR **ppInput;
  BSTR *pOutput, po;
  MSG msg;

  // make sure all Racket calls are in this thread

  setupSchemeEnv(env);

  scheme_set_exit(exitHandler);
  sleepFun = scheme_builtin_value("sleep");

  pTg = (THREAD_GLOBALS *)args;

  ppInput = pTg->ppInput;
  pOutput = pTg->pOutput;
  pHr = pTg->pHr;
  readSem = pTg->readSem;
  writeSem = pTg->writeSem;
  resetSem = pTg->resetSem;
  resetDoneSem = pTg->resetDoneSem;
  pErrorState = pTg->pErrorState;

  while (1) {

    doEval = FALSE;

    while (doEval == FALSE) {
      waitVal = MsgWaitForMultipleObjects(2,evalLoopSems,FALSE,
                                          5,QS_ALLINPUT);

      switch (waitVal) {

      case WAIT_TIMEOUT :

        scheme_apply(sleepFun,0,NULL);
        break;

      case WAIT_OBJECT_0 + 1:

        // reset semaphore signaled

        setupSchemeEnv(scheme_basic_env());
        ReleaseSemaphore(resetDoneSem,1,NULL);

        break;

      case WAIT_OBJECT_0 + 2:

        // Windows msg

        while (PeekMessage(&msg,NULL,0x400,0x400,PM_REMOVE)) {
          TranslateMessage(&msg);
          DispatchMessage(&msg);
        }

        scheme_apply(sleepFun,0,NULL);

        break;

      default :

        // got string to eval

        doEval = TRUE;

        break;
      }
    }

    len = SysStringLen(**ppInput);

    narrowInput = (char *)scheme_malloc(len + 1);

    scheme_dont_gc_ptr(narrowInput);

    WideCharToMultiByte(CP_ACP,(DWORD)0,
                        **ppInput,len,
                        narrowInput,len + 1,
                        NULL,NULL);

    narrowInput[len] = '\0';

    outputObj = eval_string_or_get_exn_message(narrowInput);

    scheme_gc_ptr_ok(narrowInput);

    if (*pErrorState) {
      wideError = wideStringFromSchemeObj(outputObj,"Racket error: ~a",18);
      po = SysAllocString(L"");
      *pOutput = po;
      *pHr = E_FAIL;
    }
    else {
      outputBuffer = wideStringFromSchemeObj(outputObj,"~s",2);
      po = SysAllocString(outputBuffer);
      *pOutput = po;
      *pHr = S_OK;
    }

    ReleaseSemaphore(writeSem,1,NULL);

  }

  return 0;
}

static void record_at_exit(Scheme_At_Exit_Callback_Proc p) XFORM_SKIP_PROC
{
  at_exit_callback = p;
}

static __declspec(thread) void *tls_space;

static unsigned WINAPI evalLoop(void *args) XFORM_SKIP_PROC {
#ifndef _WIN64
  scheme_register_tls_space(&tls_space, 0);
#endif
  scheme_set_atexit(record_at_exit);

  return scheme_main_setup(1, do_evalLoop, 0, (char **)args);
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

void CMzObj::startMzThread(void) {
  tg.pHr = &hr;
  tg.ppInput = &globInput;
  tg.pOutput = &globOutput;
  tg.readSem = readSem;
  tg.writeSem = writeSem;
  tg.resetSem = resetSem;
  tg.resetDoneSem = resetDoneSem;
  tg.pErrorState = &errorState;

  threadHandle = (HANDLE)_beginthreadex(NULL, 0, evalLoop, &tg, 0, NULL);
}


CMzObj::CMzObj(void *_com_obj) {

  com_obj = _com_obj;

  inputMutex = NULL;
  readSem = NULL;
  threadHandle = NULL;

  inputMutex = CreateSemaphore(NULL,1,1,NULL);
  if (inputMutex == NULL) {
    ErrorBox("Can't create input mutex");
    return;
  }

  readSem = CreateSemaphore(NULL,0,1,NULL);

  if (readSem == NULL) {
    ErrorBox("Can't create read semaphore");
    return;
  }

  writeSem = CreateSemaphore(NULL,0,1,NULL);

  if (writeSem == NULL) {
    ErrorBox("Can't create write semaphore");
    return;
  }

  exitSem = CreateSemaphore(NULL,0,1,NULL);

  if (exitSem == NULL) {
    ErrorBox("Can't create exit semaphore");
    return;
  }

  resetSem = CreateSemaphore(NULL,0,1,NULL);

  if (resetSem == NULL) {
    ErrorBox("Can't create reset semaphore");
    return;
  }

  resetDoneSem = CreateSemaphore(NULL,0,1,NULL);

  if (resetSem == NULL) {
    ErrorBox("Can't create reset-done semaphore");
    return;
  }

  evalLoopSems[0] = readSem;
  evalLoopSems[1] = resetSem;
  evalDoneSems[0] = writeSem;
  evalDoneSems[1] = exitSem;

  startMzThread();

}

void CMzObj::killMzThread(void) {
  if (threadHandle) {
    DWORD threadStatus;

    GetExitCodeThread(threadHandle,&threadStatus);

    if (threadStatus == STILL_ACTIVE) {
      TerminateThread(threadHandle,0);
    }

    CloseHandle(threadHandle);

    threadHandle = NULL;
  }
}

CMzObj::~CMzObj(void) {

  killMzThread();

  if (readSem) {
    CloseHandle(readSem);
  }

  if (writeSem) {
    CloseHandle(writeSem);
  }

  if (exitSem) {
    CloseHandle(exitSem);
  }

  if (inputMutex) {
    CloseHandle(inputMutex);
  }
}

void CMzObj::RaiseError(const OLECHAR *msg) {
  BSTR bstr;
  ICreateErrorInfo *pICreateErrorInfo;
  IErrorInfo *pIErrorInfo;

  bstr = SysAllocString(msg);

  if (CreateErrorInfo(&pICreateErrorInfo) == S_OK &&
      pICreateErrorInfo != NULL) {
    pICreateErrorInfo->SetGUID(com_get_class_iid());
    pICreateErrorInfo->SetDescription((LPOLESTR)msg);
    pICreateErrorInfo->SetSource((LPOLESTR)L"MzCOM.MzObj");
    if (pICreateErrorInfo->QueryInterface(IID_IErrorInfo,
                                          (void **)&pIErrorInfo) == S_OK &&
        pIErrorInfo != NULL) {
      SetErrorInfo(0,pIErrorInfo);
    }
  }

  Fire_SchemeError((IMzObj *)com_obj, bstr);
  SysFreeString(bstr);
}

BOOL CMzObj::testThread(void) {
  DWORD threadStatus;

  if (threadHandle == NULL) {
    RaiseError(L"No evaluator");
    return FALSE;
  }

  if (GetExitCodeThread(threadHandle,&threadStatus) == 0) {
    RaiseError(L"Evaluator may be terminated");
  }

  if (threadStatus != STILL_ACTIVE) {
    RaiseError(L"Evaluator terminated");
    return FALSE;
  }

  return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CMzObj

HRESULT CMzObj::Eval(BSTR input, BSTR *output) {
  if (!testThread()) {
    return E_ABORT;
  }

  WaitForSingleObject(inputMutex,INFINITE);
  globInput = &input;
  // allow evaluator to read
  ReleaseSemaphore(readSem,1,NULL);

  // wait until evaluator done or eval thread terminated
  if (WaitForMultipleObjects(2,evalDoneSems,FALSE,INFINITE) ==
      WAIT_OBJECT_0 + 1) {
    RaiseError(L"Racket terminated evaluator");
    return E_FAIL;
  }

  *output = globOutput;
  ReleaseSemaphore(inputMutex,1,NULL);

  if (errorState) {
    RaiseError(wideError);
  }

  return hr;
}

INT_PTR WINAPI dlgProc(HWND hDlg,UINT msg,WPARAM wParam,LPARAM) {
  switch(msg) {
  case WM_INITDIALOG :
    SetDlgItemText(hDlg,MZCOM_URL,
                   "http://www.cs.rice.edu/CS/PLT/packages/mzcom/");
    return TRUE;
  case WM_COMMAND :
    switch (LOWORD(wParam)) {
    case IDOK :
    case IDCANCEL :
      EndDialog(hDlg,0);
      return FALSE;
    }
  default :
    return FALSE;
  }
}

HRESULT CMzObj::About() {
  DialogBox(globHinst,MAKEINTRESOURCE(ABOUTBOX),NULL,dlgProc);
  return S_OK;
}

HRESULT CMzObj::Reset() {
  if (!testThread()) {
    return E_ABORT;
  }

  ReleaseSemaphore(resetSem,1,NULL);
  WaitForSingleObject(resetDoneSem,INFINITE);
  return S_OK;
}

void *new_mzobj(IMzObj *com_obj)
{
  return new CMzObj(com_obj);
}

void delete_mzobj(void *o)
{
  delete (CMzObj *)o;
}

HRESULT mzobj_about(void *o)
{
  return ((CMzObj *)o)->About();
}

HRESULT mzobj_reset(void *o)
{
  return ((CMzObj *)o)->Reset();
}

HRESULT mzobj_eval(void *o, BSTR s, BSTR *r)
{
  return ((CMzObj *)o)->Eval(s, r);
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

#endif // MZCOM_3M
