
#include <windows.h>
#include <stdio.h>
#include "../../mzscheme/src/schvers.h"

HMODULE LoadUnicowsProc(void)
{
  /* Try version-mangled name, and if that doesn't work, try xxxxxxx name */
  HMODULE m;
  char s[40];
  sprintf(s, "uniplt_%d%d_000000", MZSCHEME_VERSION_MAJOR, MZSCHEME_VERSION_MINOR);
  s[14] = '.';
  s[15] = 'd';
  s[16] = 'l';
  s[17] = 'l';
  s[18] = 0;
  m = LoadLibraryA(s);
  if (!m)
    m = LoadLibraryA("uniplt_xxxxxxx.dll");
  return m;
}

extern FARPROC _PfnLoadUnicows = (FARPROC) &LoadUnicowsProc;
