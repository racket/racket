
# ifdef MZ_PRECISE_GC
#  define DLL_3M_SUFFIX "3m"
# else
#  define DLL_3M_SUFFIX ""
# endif
static wchar_t *_dlldir = L"dLl dIRECTORy:" /* <- this tag stays, so we can find it again */
                          DLL_RELATIVE_PATH L"\0"
                          /* Pad with 512 characters: */
                          L"****************************************************************"
                          L"****************************************************************"
                          L"****************************************************************"
                          L"****************************************************************"
                          L"****************************************************************"
                          L"****************************************************************"
                          L"****************************************************************"
                          L"****************************************************************";
static int _dlldir_offset = 14; /* Skip permanent tag */

# ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
# endif

static void load_delayed_dll(HINSTANCE me, const char *lib) 
{
  /* Don't use the C library here! */
  const wchar_t *dlldir = _dlldir + _dlldir_offset;

  if (dlldir[0] != '<') {
    if ((dlldir[0] == '\\')
	|| ((((dlldir[0] >= 'a') && (dlldir[0] <= 'z'))
	     || ((dlldir[0] >= 'A') && (dlldir[0] <= 'Z')))
	    && (dlldir[1] == ':'))) {
      /* Absolute path */
    } else {
      /* Make it absolute, relative to this module */
      wchar_t *name, *s;
      int j, i;
      name = (wchar_t *)GlobalAlloc(GMEM_FIXED, 1024 * sizeof(wchar_t));
      GetModuleFileNameW(me, name, 1024);
      name[1023] = 0;
      s = (wchar_t *)GlobalAlloc(GMEM_FIXED, 2048 * sizeof(wchar_t));
      for (i = 0; name[i]; i++) { }
      --i;
      while (i && (name[i] != '\\')) {
	--i;
      }
      name[i+1] = 0;
      for (i = 0; name[i]; i++) {
	s[i] = name[i];
      }
      for (j = 0; dlldir[j]; j++, i++) {
	s[i] = dlldir[j];
      }
      s[i] = 0;
      dlldir = s;
      _dlldir = s;
      _dlldir_offset = 0;
    }

    {
      wchar_t *t;
      int j, i;

      t = (wchar_t *)GlobalAlloc(GMEM_FIXED, 2048 * sizeof(wchar_t));
      for (i = 0; dlldir[i]; i++) {
	t[i] = dlldir[i];
      }
      if (t[i-1] != '\\')
	t[i++] = '\\';
      for (j = 0; lib[j]; j++, i++) {
	t[i] = lib[j];
      }
      t[i] = 0;

      if (!LoadLibraryW(t)) {
	MessageBoxW(NULL, t, L"Failure: cannot load DLL", MB_OK);
	ExitProcess(1);
      }
    }
  }
}
