// mzcom.cxx : Implementation of WinMain

// This file is not xformed for 3m. There's just one
// bit of conditional compilation on MZCOM_3M.

#include "../racket/src/schvers.h"
#include "resource.h"

#include <objbase.h>
extern "C" {
#include "com_glue.h"
};

// time to wait for threads to finish up
#define dwPause (1000)

HINSTANCE globHinst;

/* A monitor thread might be a good idea to make sure the process
   terminates if it's somehow started and not used. It also creates
   a race condition, though, so it's disabled for now. */

#if 0

// time for EXE to be idle before shutting down
#define dwTimeOut (5000)

static HANDLE hEventShutdown;
static DWORD dwThreadID;

// Polls for idle state
static DWORD WINAPI MonitorProc(void* pv)
{
  while (1) {
    DWORD dwWait=0;
    do
      {
        dwWait = WaitForSingleObject(hEventShutdown, dwTimeOut);
      } while (dwWait == WAIT_OBJECT_0);

    if (com_can_unregister())
      break;
  }
  CloseHandle(hEventShutdown);
  PostThreadMessage(dwThreadID, WM_QUIT, 0, 0);

  return 0;
}

static bool StartMonitor()
{
  dwThreadID = GetCurrentThreadId();
  hEventShutdown = CreateEvent(NULL, false, false, NULL);
  if (hEventShutdown == NULL)
    return false;

  DWORD subThreadID;
  HANDLE h = CreateThread(NULL, 0, MonitorProc, NULL, 0, &subThreadID);
  return (h != NULL);
}

#else

static bool StartMonitor() { return TRUE; }

#endif

static int set_reg_string(HKEY sub, const char *name, const char *s)
{
  return RegSetValueExA(sub, name, 0, REG_SZ, (const BYTE *)s, strlen(s));
}

static int set_reg_sub_string(HKEY sub, const char *name, const char *s)
{
  HKEY sub2;
  int nRet;

  nRet = RegCreateKeyExA(sub, name, 0, NULL, 0, KEY_SET_VALUE, NULL, &sub2, NULL);
  if (!nRet) {
    nRet |= set_reg_string(sub2, NULL, s);
    nRet |= RegCloseKey(sub2);
  }

  return nRet;
}

LPCTSTR FindOneOf(LPCTSTR p1, LPCTSTR p2)
{
  while (p1 != NULL && *p1 != 0)
    {
      LPCTSTR p = p2;
      while (p != NULL && *p != 0)
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
extern "C" int WINAPI WinMain(HINSTANCE hInstance, 
                              HINSTANCE /*hPrevInstance*/, LPTSTR lpCmdLine, int /*nShowCmd*/) {

  globHinst = hInstance;

  lpCmdLine = GetCommandLine(); //this line necessary for _ATL_MIN_CRT

#ifdef MZCOM_3M
  load_delayed_dll(hInstance, "libracket3mxxxxxxx.dll");
#else
  load_delayed_dll(hInstance, "libmzgcxxxxxxx.dll");
  load_delayed_dll(hInstance, "libracketxxxxxxx.dll");
#endif

  HRESULT nRet = CoInitialize(NULL);

  int argc, i;
  char **argv, *normalized_path;

  argv = cmdline_to_argv(&argc, &normalized_path);

  int verbose = 0;
  BOOL bRun = TRUE;
  LPCTSTR lpszToken;
  for (i = 1; i < argc; i++)
    {
      lpszToken = argv[i];
      if (IsFlag(lpszToken, "UnregServer"))
        {
	  if (!nRet) {
            HKEY sub;

            nRet |= RegDeleteKeyA(HKEY_CLASSES_ROOT, "MzCOM.MzObj");
            nRet |= RegDeleteKeyA(HKEY_CLASSES_ROOT, "MzCOM.MzObj." MZSCHEME_VERSION);

            if (!nRet) {
              nRet = RegCreateKeyExA(HKEY_CLASSES_ROOT, "CLSID", 0, NULL, 0, KEY_SET_VALUE, NULL, &sub, NULL);
              if (!nRet) {
                nRet |= RegDeleteKeyA(sub, "{A3B0AF9E-2AB0-11D4-B6D2-0060089002FE}");
                nRet |= RegCloseKey(sub);
              }
            }
            
	    if (!nRet) {
              nRet = RegCreateKeyExA(HKEY_CLASSES_ROOT, "AppID", 0, NULL, 0, KEY_SET_VALUE, NULL, &sub, NULL);
              if (!nRet) {
                nRet |= RegDeleteKeyA(sub, "{A604CB9D-2AB5-11D4-B6D3-0060089002FE}");
                nRet |= RegCloseKey(sub);
              }
            }
            
	    bRun = FALSE;
	  }
        }
      else if (IsFlag(lpszToken, "RegServer"))
        {
	  if (!nRet) {
            HKEY sub, sub2;

            nRet |= RegCreateKeyExA(HKEY_CLASSES_ROOT, "MzCOM.MzObj", 0, NULL, 0, KEY_SET_VALUE, NULL, &sub, NULL);

            if (!nRet) {
              nRet |= set_reg_string(sub, NULL, "MzObj Class");
              nRet |= set_reg_sub_string(sub, "CLSID", "{A3B0AF9E-2AB0-11D4-B6D2-0060089002FE}");
              nRet |= set_reg_sub_string(sub, "CurVer", "MzCOM.MzObj." MZSCHEME_VERSION);
              nRet |= RegCloseKey(sub);
            }

            if (!nRet) {
              nRet = RegCreateKeyExA(HKEY_CLASSES_ROOT, "MzCOM.MzObj." MZSCHEME_VERSION, 0, NULL, 0, KEY_SET_VALUE, NULL, &sub, NULL);
              if (!nRet) {
                nRet |= set_reg_string(sub, NULL, "MzObj Class");
                nRet |= set_reg_sub_string(sub, "CLSID", "{A3B0AF9E-2AB0-11D4-B6D2-0060089002FE}");
                nRet |= RegCloseKey(sub);
              }
            }

            if (!nRet) {
              nRet = RegCreateKeyExA(HKEY_CLASSES_ROOT, "CLSID", 0, NULL, 0, KEY_SET_VALUE, NULL, &sub, NULL);
              if (!nRet) {
                nRet = RegCreateKeyExA(sub, "{A3B0AF9E-2AB0-11D4-B6D2-0060089002FE}", 0, NULL, 0, KEY_SET_VALUE, NULL, &sub2, NULL);
                if (!nRet) {
                  nRet |= set_reg_string(sub2, NULL, "MzObj Class");
                  nRet |= set_reg_string(sub2, "AppId", "{A604CB9D-2AB5-11D4-B6D3-0060089002FE}");
                  nRet |= set_reg_sub_string(sub2, "ProgID", "MzCOM.MzObj." MZSCHEME_VERSION);
                  nRet |= set_reg_sub_string(sub2, "VersionIndependentProgID", "MzCOM.MzObj");
                  nRet |= set_reg_sub_string(sub2, "Programmable", "");

                  char *path;
                  path = (char *)malloc(1024 * sizeof(wchar_t));
                  GetModuleFileNameA(NULL, path, 1024);
                  nRet |= set_reg_sub_string(sub2, "LocalServer32", path);
                  free(path);

                  nRet |= set_reg_sub_string(sub2, "TypeLib", "{A604CB9C-2AB5-11D4-B6D3-0060089002FE}");
                  nRet |= RegCloseKey(sub2);
                }
                nRet |= RegCloseKey(sub);
              }
            }

            if (!nRet) {
              nRet = RegCreateKeyExA(HKEY_CLASSES_ROOT, "AppID", 0, NULL, 0, KEY_SET_VALUE, NULL, &sub, NULL);
              if (!nRet) {
                nRet = RegCreateKeyExA(sub, "{A604CB9D-2AB5-11D4-B6D3-0060089002FE}", 0, NULL, 0, KEY_SET_VALUE, NULL, &sub2, NULL);
                if (!nRet) {
                  nRet |= set_reg_string(sub2, NULL, "MzCOM");
                  nRet |= RegCloseKey(sub2);
                }
              }
              if (!nRet) {
                nRet = RegCreateKeyExA(sub, "MzCOM.EXE", 0, NULL, 0, KEY_SET_VALUE, NULL, &sub2, NULL);
                if (!nRet) {
                  nRet |= set_reg_string(sub2, "AppID", "{A604CB9D-2AB5-11D4-B6D3-0060089002FE}");
                  nRet |= RegCloseKey(sub2);
                }
              }
              nRet |= RegCloseKey(sub);
            }

	    bRun = FALSE;
	  }
        }
      else if (IsFlag(lpszToken, "v"))
	{
	  verbose = 1;
	}
      else if (IsFlag(lpszToken, "?"))
        {
	  MessageBox(NULL,
		     "/RegServer - register\n"
                     "/UnregServer - unregister\n"
                     "/Embedding - ignored\n"
                     "/v - report failures\n"
                     "/? - show this help",
                     "Help",
		     MB_OK);
	  bRun = FALSE;
        }
      else if (IsFlag(lpszToken, "Embedding"))
	{
	  /* ??? */
	}
      else
	{
          if (verbose)
            MessageBox(NULL, lpszToken, "Unknown Flag", MB_OK);
	  bRun = FALSE;
	  break;
	}
    }
  
  if (bRun) {
    StartMonitor();

    nRet = com_register();

    if (!nRet) {
      MSG msg;
      while (GetMessage(&msg, 0, 0, 0))
        DispatchMessage(&msg);
      
      while (!com_unregister()) {
        Sleep(dwPause); // wait for any objects to finish
      }
    }
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

  CoUninitialize();

  return nRet;
}
