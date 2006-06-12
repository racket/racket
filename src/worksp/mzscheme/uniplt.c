
#include <windows.h>

static int warned;

HMODULE LoadUnicowsProc(void)
{
  /* We can't use Unicode functions, so we can't
     use the library path as returned by scheme_get_dll_path().
     Instead, just search for UnicoWS.dll in the standard
     place. */
  HMODULE h;
  char *name;
  int i;

  h = LoadLibrary("UnicoWS.dll");
  if (h) return h;
  
  name = (char *)GlobalAlloc(GMEM_FIXED, 1050);
  GetModuleFileName(NULL, name, 1024);
  name[1023] = 0;
  for (i = 0; name[i]; i++) { }
  --i;
  while (i && (name[i] != '\\')) {
    --i;
  }
  memcpy(name + i, "\\lib\\UnicoWS.dll", 17);

  h = LoadLibrary(name);
  if (h) return h;
  
  if (!warned) {
    warned = 1;
    MessageBox(NULL, name, "Can't load UnicoWS", MB_OK);
  }

  return NULL;
}

extern FARPROC _PfnLoadUnicows = (FARPROC) &LoadUnicowsProc;
