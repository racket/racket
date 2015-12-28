#include <Windows.h>

/* Doesn't attempt to parse the command line in the usual way.
   Instead, the whole command-line is used as a path of a
   sound file to play. */

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR ignored, int nCmdShow)
{
  return PlaySoundW(GetCommandLineW(), NULL, SND_SYNC);
}
