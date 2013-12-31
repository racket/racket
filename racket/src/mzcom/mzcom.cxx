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

int IsFlag(LPCTSTR cmd, LPCTSTR flag)
{
  if ((*cmd == '-') || (*cmd == '/'))
    cmd++;
  else
    return 0;

  while (*flag) {
    if (toupper(*cmd) != toupper(*flag))
      return 0;
    cmd++;
    flag++;
  }
  if (!*cmd || (*cmd == ' '))
    return 1;
  return 0;
}

#define DLL_RELATIVE_PATH L"."
#include "../racket/delayed.inc"

#define ASSUME_ASCII_COMMAND_LINE
#define GC_CAN_IGNORE
#include "../racket/parse_cmdl.inc"

/////////////////////////////////////////////////////////////////////////////
//
extern "C" int WINAPI _tWinMain(HINSTANCE hInstance, 
				HINSTANCE /*hPrevInstance*/, LPTSTR lpCmdLine, int /*nShowCmd*/) {

  globHinst = hInstance;

  lpCmdLine = GetCommandLine(); //this line necessary for _ATL_MIN_CRT

#ifdef MZCOM_3M
  load_delayed_dll(hInstance, "libracket3mxxxxxxx.dll");
#else
  load_delayed_dll(hInstance, "libmzgcxxxxxxx.dll");
  load_delayed_dll(hInstance, "libracketxxxxxxx.dll");
#endif

#if _WIN32_WINNT >= 0x0400 & defined(_ATL_FREE_THREADED)
  HRESULT hRes = CoInitializeEx(NULL, COINIT_MULTITHREADED);
#else
  HRESULT hRes = CoInitialize(NULL);
#endif
  _ASSERTE(SUCCEEDED(hRes));
  _Module.Init(ObjectMap, hInstance, &LIBID_MZCOMLib);
  _Module.dwThreadID = GetCurrentThreadId();

  int argc, i;
  char **argv, *normalized_path;

  argv = cmdline_to_argv(&argc, &normalized_path);

  int nRet = 0, verbose = 0;
  BOOL bRun = TRUE;
  LPCTSTR lpszToken;
  for (i = 1; i < argc; i++)
    {
      lpszToken = argv[i];
      if (IsFlag(lpszToken, _T("UnregServer")))
        {
	  if (!nRet) {
	    _Module.UpdateRegistryFromResource(IDR_MZCOM, FALSE);
	    nRet = _Module.UnregisterServer(TRUE);
	    bRun = FALSE;
	  }
        }
      else if (IsFlag(lpszToken, _T("RegServer")))
        {
	  if (!nRet) {
	    _Module.UpdateRegistryFromResource(IDR_MZCOM, TRUE);
	    nRet = _Module.RegisterServer(TRUE);
	    bRun = FALSE;
	  }
        }
      else if (IsFlag(lpszToken, _T("v")))
	{
	  verbose = 1;
	}
      else if (IsFlag(lpszToken, _T("?")))
        {
	  MessageBox(NULL,
		     _T("/RegServer - register\n"
			"/UnregServer - unregister\n"
			"/Embedding - ignored\n"
			"/v - report failures\n"
			"/? - show this help"),
		     _T("Help"),
		     MB_OK);
	  bRun = FALSE;
        }
      else if (IsFlag(lpszToken, _T("Embedding")))
	{
	  /* ??? */
	}
      else
	{
          if (verbose)
            MessageBox(NULL, lpszToken, _T("Unknown Flag"), MB_OK);
	  bRun = FALSE;
	  break;
	}
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

  if (verbose && (nRet != 0)) {
    wchar_t *res;
    FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER
		   | FORMAT_MESSAGE_FROM_SYSTEM,
		   NULL,
		   nRet,
		   0,
		   (wchar_t *)&res,
		   0,
		   0);
    MessageBoxW(NULL, res, L"Registration Failed", MB_OK);
  }

    _Module.Term();
    CoUninitialize();
    return nRet;
}
