// mzcom.cxx : Implementation of WinMain

// This file is not xformed for 3m. There's just one
// bit of conditional compilation on MZCOM_3M.

#include "stdafx.h"
#include "resource.h"
#include <initguid.h>
#include "mzcom.h"

#include "mzcom_i.c"
#include "mzobj.h"

// time for EXE to be idle before shutting down
#define dwTimeOut (5000)
// time to wait for threads to finish up
#define dwPause (1000)

HINSTANCE globHinst;

// Passed to CreateThread to monitor the shutdown event
static DWORD WINAPI MonitorProc(void* pv)
{
    CExeModule* p = (CExeModule*)pv;
    p->MonitorShutdown();
    return 0;
}

LONG CExeModule::Unlock()
{
    LONG l = CComModule::Unlock();
    if (l == 0)
    {
        bActivity = true;
        SetEvent(hEventShutdown);
    }
    return l;
}

// Monitors the shutdown event
void CExeModule::MonitorShutdown()
{
    while (1)
    {
        WaitForSingleObject(hEventShutdown, INFINITE);
        DWORD dwWait=0;
        do
        {
            bActivity = false;
            dwWait = WaitForSingleObject(hEventShutdown, dwTimeOut);
        } while (dwWait == WAIT_OBJECT_0);
        // timed out
        if (!bActivity && m_nLockCnt == 0) // if no activity let's really bail
        {
#if _WIN32_WINNT >= 0x0400 & defined(_ATL_FREE_THREADED)
            CoSuspendClassObjects();
            if (!bActivity && m_nLockCnt == 0)
#endif
                break;
        }
    }
    CloseHandle(hEventShutdown);
    PostThreadMessage(dwThreadID, WM_QUIT, 0, 0);
}

bool CExeModule::StartMonitor()
{
    hEventShutdown = CreateEvent(NULL, false, false, NULL);
    if (hEventShutdown == NULL)
        return false;
    DWORD dwThreadID;
    HANDLE h = CreateThread(NULL, 0, MonitorProc, this, 0, &dwThreadID);
    return (h != NULL);
}

CExeModule _Module;

BEGIN_OBJECT_MAP(ObjectMap)
OBJECT_ENTRY(CLSID_MzObj, CMzObj)
END_OBJECT_MAP()

LPCTSTR FindOneOf(LPCTSTR p1, LPCTSTR p2)
{
    while (p1 != NULL && *p1 != NULL)
    {
        LPCTSTR p = p2;
        while (p != NULL && *p != NULL)
        {
            if (*p1 == *p)
                return CharNext(p1);
            p = CharNext(p);
        }
        p1 = CharNext(p1);
    }
    return NULL;
}

#define DLL_RELATIVE_PATH L"lib"
#include "../racket/delayed.inc"

/////////////////////////////////////////////////////////////////////////////
//
extern "C" int WINAPI _tWinMain(HINSTANCE hInstance, 
				HINSTANCE /*hPrevInstance*/, LPTSTR lpCmdLine, int /*nShowCmd*/) {

  globHinst = hInstance;

  lpCmdLine = GetCommandLine(); //this line necessary for _ATL_MIN_CRT

#ifdef MZCOM_3M
  load_delayed_dll(hInstance, "libmzsch3mxxxxxxx.dll");
#else
  load_delayed_dll(hInstance, "libmzgcxxxxxxx.dll");
  load_delayed_dll(hInstance, "libmzschxxxxxxx.dll");
#endif

#if _WIN32_WINNT >= 0x0400 & defined(_ATL_FREE_THREADED)
  HRESULT hRes = CoInitializeEx(NULL, COINIT_MULTITHREADED);
#else
  HRESULT hRes = CoInitialize(NULL);
#endif
  _ASSERTE(SUCCEEDED(hRes));
  _Module.Init(ObjectMap, hInstance, &LIBID_MZCOMLib);
  _Module.dwThreadID = GetCurrentThreadId();
  TCHAR szTokens[] = _T("-/");

  int nRet = 0;
  BOOL bRun = TRUE;
  LPCTSTR lpszToken = FindOneOf(lpCmdLine, szTokens);
  while (lpszToken != NULL)
    {
      if (lstrcmpi(lpszToken, _T("UnregServer"))==0)
        {
	  _Module.UpdateRegistryFromResource(IDR_MZCOM, FALSE);
          nRet = _Module.UnregisterServer(TRUE);
	  bRun = FALSE;
	  break;
        }
      if (lstrcmpi(lpszToken, _T("RegServer"))==0)
        {
	  _Module.UpdateRegistryFromResource(IDR_MZCOM, TRUE);
	  nRet = _Module.RegisterServer(TRUE);
	  bRun = FALSE;
	  break;
        }
      lpszToken = FindOneOf(lpszToken, szTokens);
    }
  
  if (bRun)
    {
        _Module.StartMonitor();

#if _WIN32_WINNT >= 0x0400 & defined(_ATL_FREE_THREADED)
        hRes = _Module.RegisterClassObjects(CLSCTX_LOCAL_SERVER, 
					    REGCLS_SINGLEUSE | REGCLS_SUSPENDED);
    // was:  REGCLS_MULTIPLEUSE | REGCLS_SUSPENDED);


        _ASSERTE(SUCCEEDED(hRes));
        hRes = CoResumeClassObjects();
#else
        hRes = _Module.RegisterClassObjects(CLSCTX_LOCAL_SERVER, 
					    // was REGCLS_MULTIPLEUSE);
					    REGCLS_SINGLEUSE);
#endif
        _ASSERTE(SUCCEEDED(hRes));

        MSG msg;
        while (GetMessage(&msg, 0, 0, 0))
            DispatchMessage(&msg);

        _Module.RevokeClassObjects();
	Sleep(dwPause); //wait for any threads to finish
    }

    _Module.Term();
    CoUninitialize();
    return nRet;
}
