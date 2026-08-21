#include "rktio.h"
#include "rktio_private.h"

#ifdef RKTIO_SYSTEM_UNIX

void rktio_create_console()
{
}

#endif

#ifdef RKTIO_SYSTEM_WINDOWS

static BOOL has_console;

void rktio_console_ctl_c()
{
    FreeConsole();
}

void rktio_create_console()
{
    if (has_console)
      return;

    // If this fails, we already have a console. 
    if (!AllocConsole()) {
      has_console = TRUE;
      return;
    }

    has_console = TRUE;

    rktio_set_console_handler();

    HANDLE console_in = GetStdHandle(STD_INPUT_HANDLE);
    SetConsoleMode(console_in, ENABLE_VIRTUAL_TERMINAL_INPUT);

    HANDLE console_out = GetStdHandle(STD_OUTPUT_HANDLE);
    SetConsoleMode(console_out, ENABLE_PROCESSED_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING);

    ShowWindow(GetConsoleWindow(), SW_HIDE);

    has_console = 1;
    atexit(rktio_console_ctl_c);
}

#endif
