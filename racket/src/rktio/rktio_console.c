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

}

static void WaitOnConsole()
{
  DWORD wrote;

  if (!has_console)
    return;

  FreeConsole();
  has_console = 0;
}

void rktio_create_console()
{
  if (!has_console) {
    
    AllocConsole();
    rktio_set_console_handler();
    HANDLE console_input = GetStdHandle(STD_INPUT_HANDLE);
    SetConsoleMode(console_input, ENABLE_PROCESSED_INPUT | ENABLE_VIRTUAL_TERMINAL_INPUT);

    HANDLE console_output = GetStdHandle(STD_OUTPUT_HANDLE);
    SetConsoleMode(console_output, ENABLE_PROCESSED_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
  
    has_console = 1;

    HANDLE console_wnd = GetConsoleWindow();
    ShowWindow(console_wnd, SW_HIDE);
    
    atexit(WaitOnConsole);
  }
}

#endif
