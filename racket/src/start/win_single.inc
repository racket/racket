/* ---------------------------------------- */
/*        single-instance detection         */
/* ---------------------------------------- */

static char *CreateUniqueName()
{
  char desktop[MAX_PATH], session[32], *together;
  int dlen, slen;

  {
    // Name should be desktop unique, so add current desktop name
    HDESK hDesk;
    ULONG cchDesk = MAX_PATH - 1;

    hDesk = GetThreadDesktop(GetCurrentThreadId());
    
    if (!GetUserObjectInformation( hDesk, UOI_NAME, desktop, cchDesk, &cchDesk))
      desktop[0] = 0;
    else
      desktop[MAX_PATH - 1]  = 0;
  }

  {
    // Name should be session unique, so add current session id
    HANDLE hToken = NULL;
    // Try to open the token (fails on Win9x) and check necessary buffer size
    if (OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &hToken)) {
      DWORD cbBytes = 0;
      
      if(!GetTokenInformation( hToken, TokenStatistics, NULL, cbBytes, &cbBytes ) 
	 && GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
	  PTOKEN_STATISTICS pTS;

	  pTS = (PTOKEN_STATISTICS)malloc(cbBytes);
	  
	  if(GetTokenInformation(hToken, TokenStatistics, (LPVOID)pTS, cbBytes, &cbBytes)) {
	    sprintf(session, "-%08x%08x-",
		    pTS->AuthenticationId.HighPart, 
		    pTS->AuthenticationId.LowPart);
	  } else
	    session[0] = 0;
	  free(pTS);
      } else {
	session[0] = 0;
      }
    } else
      session[0] = 0;
  }

  dlen = strlen(desktop);
  slen =  strlen(session);
  together = (char *)malloc(slen + dlen + 1);
  memcpy(together, desktop, dlen);
  memcpy(together + dlen, session, slen);
  together[dlen + slen] = 0;
  
  return together;
}

#define GRACKET_GUID "B2261834-D535-44dd-8511-A26FC8F97DD0"

static int wm_is_gracket;

static BOOL CALLBACK CheckWindow(HWND wnd, LPARAM param)
{
  int i, len, gl;
  DWORD w;
  char **argv, *v;
  COPYDATASTRUCT cd;
  DWORD_PTR result;
  LRESULT ok;

  ok = SendMessageTimeout(wnd, wm_is_gracket,
			  0, 0, 
			  SMTO_BLOCK |
			  SMTO_ABORTIFHUNG,
			  200,
			  &result);

  printf("try %p result %d\n", wnd, result);

  if (ok == 0)
    return TRUE; /* ignore and continue */
  if ((intptr_t)result == 79) {
    /* found it */
  } else
    return TRUE; /* continue search */

  /* wnd is owned by another instance of this application */

  SetForegroundWindow(wnd);
  if (IsIconic(wnd)) 
    ShowWindow(wnd, SW_RESTORE);

  argv = (char **)param;
  
  len = gl = strlen(GRACKET_GUID);
  len += 4 + sizeof(DWORD);
  for (i = 1; argv[i]; i++) {
    len += sizeof(DWORD) + strlen(argv[i]);
  }
  w = i - 1;

  v = (char *)malloc(len);
  memcpy(v, GRACKET_GUID, gl);
  memcpy(v + gl, "OPEN", 4);
  memcpy(v + gl + 4, &w, sizeof(DWORD));
  len = gl + 4 + sizeof(DWORD);
  for (i = 1; argv[i]; i++) {
    w = strlen(argv[i]);
    memcpy(v + len, &w, sizeof(DWORD));
    len += sizeof(DWORD);
    memcpy(v + len, argv[i], w);
    len += w;
  }

  cd.dwData = 79;
  cd.cbData = len;
  cd.lpData = v;

  SendMessage(wnd, WM_COPYDATA, (WPARAM)wnd, (LPARAM)&cd);

  free(v);

  return FALSE;
}

static int CheckSingleInstance(char *normalized_path, char **argv)
{
  /* Check for an existing instance: */
  if (check_for_another[0] != 'n') {
    int alreadyrunning;
    HANDLE mutex;
    int j, l, i;
    char *a, *b;

    /* This mutex creation synchronizes multiple instances of
       the application that may have been started. */
    j = strlen(normalized_path);
    
    b = CreateUniqueName();
    l = strlen(b);
    a = (char *)malloc(j + l + 50);
    memcpy(a, normalized_path, j);
    for (i = 0; i < j; i++) {
      /* backslashes are not allowed in mutex names */
      if (a[i] == '\\') a[i] = '/';
    }
    memcpy(a + j, b, l);
    memcpy(a + j + l, "GRacket-" GRACKET_GUID, strlen(GRACKET_GUID) + 9);
    mutex = CreateMutex(NULL, FALSE, a);
    alreadyrunning = (GetLastError() == ERROR_ALREADY_EXISTS || 
		      GetLastError() == ERROR_ACCESS_DENIED);
    /* The call fails with ERROR_ACCESS_DENIED if the Mutex was 
       created in a different users session because of passing
       NULL for the SECURITY_ATTRIBUTES on Mutex creation. */
    wm_is_gracket = RegisterWindowMessage(a);
    free(a);

    if (alreadyrunning) {
      /* If another instance has been started, try to find it. */
      if (!EnumWindows((WNDENUMPROC)CheckWindow, (LPARAM)argv)) {
	return 1;
      }
    }
  }

  return 0;
}
