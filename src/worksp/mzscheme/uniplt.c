
#include <windows.h>
#include <stdio.h>

#ifdef LIBMZ_EXPORTS
# define MZ_EXTERN extern __declspec(dllexport)
#else
# define MZ_EXTERN extern __declspec(dllimport)
#endif

MZ_EXTERN char *scheme_get_dll_path(char *);

HMODULE LoadUnicowsProc(void)
{
  char *s;

  /* Versioning should replace the "xxxxxxx" */
  s = scheme_get_dll_path("uniplt_xxxxxxx.dll");

  return LoadLibrary(s);
}

extern FARPROC _PfnLoadUnicows = (FARPROC) &LoadUnicowsProc;
