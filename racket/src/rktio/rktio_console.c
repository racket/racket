#include "rktio.h"
#include "rktio_private.h"

#ifdef RKTIO_SYSTEM_UNIX

void rktio_create_console()
{
}

#endif

#ifdef RKTIO_SYSTEM_WINDOWS

static int has_console;
static HWND console_hwnd;
static HANDLE waiting_sema;
typedef HWND (WINAPI* gcw_proc)();

void rktio_console_ctl_c()
{
  if (waiting_sema)
    ReleaseSemaphore(waiting_sema, 1, NULL);
}

static void WaitOnConsole()
{
  DWORD wrote;

  if (!has_console)
    return;

  waiting_sema = CreateSemaphore(NULL, 0, 1, NULL);

  if (console_hwnd) {
    AppendMenu(GetSystemMenu(console_hwnd, FALSE),
	       MF_STRING,
	       SC_CLOSE,
	       "Close");
    /* Un-gray the close box: */
    RedrawWindow(console_hwnd, NULL, NULL,
		 RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW);
  }

  WriteConsoleW(GetStdHandle(STD_OUTPUT_HANDLE),
		L"\n[Exited. Close box or Ctrl-C closes the console.]\n",
		51,
		&wrote,
		NULL);

  WaitForSingleObject(waiting_sema, INFINITE);

  has_console = 0;
}

void rktio_create_console()
{
  if (!has_console) {
    HMODULE hm;
    gcw_proc gcw;

    AllocConsole();

    rktio_set_console_handler();

    hm = LoadLibraryW(L"kernel32.dll");
    if (hm)
      gcw = (gcw_proc)GetProcAddress(hm, "GetConsoleWindow");
    else
      gcw = NULL;

    if (gcw)
      console_hwnd = gcw();

    if (console_hwnd) {
      EnableMenuItem(GetSystemMenu(console_hwnd, FALSE), SC_CLOSE,
		     MF_BYCOMMAND | MF_GRAYED);
      RemoveMenu(GetSystemMenu(console_hwnd, FALSE), SC_CLOSE, MF_BYCOMMAND);
    }

    has_console = 1;

    atexit(WaitOnConsole);
  }
}

#endif
