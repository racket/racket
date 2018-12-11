#include "MemoryModule.h"

#define noisy_embedded 0

typedef struct embedded_dll_entry_t {
  char *name;
  long pos;
  HMEMORYMODULE loaded_h;
} embedded_dll_entry_t;

static embedded_dll_entry_t *embedded_dlls;

void *__pfnDliNotifyHook2 = NULL;
static FARPROC WINAPI delayHook(unsigned dliNotify, void *pdli);
static HCUSTOMMODULE LoadLibraryHook(LPCSTR, void *);
static FARPROC GetProcAddressHook(HCUSTOMMODULE, LPCSTR, void *);

static HANDLE open_self()
{
  wchar_t *path;
  HANDLE fd;
  
  path = get_self_executable_path();

  fd = CreateFileW(path, GENERIC_READ,
		   FILE_SHARE_READ | FILE_SHARE_WRITE,
		   NULL,
		   OPEN_EXISTING,
		   0,
		   NULL);

  free(path);

  return fd;
}

static void parse_embedded_dlls()
{
  long rsrc_pos;

  rsrc_pos = find_resource_offset(get_self_executable_path(), 258, 0);
  if (rsrc_pos) {
    HANDLE fd = open_self();
    
    if (fd != INVALID_HANDLE_VALUE) {
      long pos;
      DWORD count, got, i;
      short name_len;
      char *name;
      
      SetFilePointer(fd, rsrc_pos, 0, FILE_BEGIN);
      ReadFile(fd, &count, sizeof(DWORD), &got, NULL);

      embedded_dlls = malloc(sizeof(embedded_dll_entry_t) * (count + 1));
      for (i = 0; i < count; i++) {
	ReadFile(fd, &name_len, sizeof(short), &got, NULL);
	name = malloc(name_len + 1);
	ReadFile(fd, name, name_len, &got, NULL);
	name[name_len] = 0;
	embedded_dlls[i].name = name;
	if (noisy_embedded)
	  fprintf(stderr, "embedded %s\n", name);
      }
      embedded_dlls[count].name = NULL;

      for (i = 0; i < count+1; i++) {
	ReadFile(fd, &pos, sizeof(pos), &got, NULL);
	embedded_dlls[i].pos = pos + rsrc_pos;
	embedded_dlls[i].loaded_h = NULL;
      }
      
      CloseHandle(fd);

      __pfnDliNotifyHook2 = delayHook;
    }
  }
}

static void *in_memory_open(const char *name, int as_global)
{
  int i;

  for (i = 0; embedded_dlls[i].name; i++) {
    if (!_stricmp(embedded_dlls[i].name, name)) {
      HMEMORYMODULE loaded_h = (void *)embedded_dlls[i].loaded_h;
      if (!loaded_h) {
	HANDLE fd = open_self();
	
	if (fd != INVALID_HANDLE_VALUE) {
	  void *p;
	  DWORD got;
	  long len = embedded_dlls[i+1].pos - embedded_dlls[i].pos;
	  
	  SetFilePointer(fd, embedded_dlls[i].pos, 0, FILE_BEGIN);
	  p = malloc(len);
	  ReadFile(fd, p, len, &got, NULL);
	  CloseHandle(fd);

	  if (got != len)
	    fprintf(stderr, "partial load %d vs %ld\n", got, len);

	  loaded_h = MemoryLoadLibraryEx(p, len,
					 MemoryDefaultAlloc, MemoryDefaultFree,
					 LoadLibraryHook, GetProcAddressHook,
					 MemoryDefaultFreeLibrary, NULL);
	  if (noisy_embedded) {
	    if (!loaded_h) {
	      fprintf(stderr, "failed %s %d\n", name, GetLastError());
	    } else
	      fprintf(stderr, "ok %s\n", name);
	  }
	  
	  free(p);

	  embedded_dlls[i].loaded_h = loaded_h;
	}
      }
      return (void *)loaded_h;
    }
  }
  
  return NULL;
}

static void *in_memory_find_object(void *h, const char *name)
{
  if (h)
    return MemoryGetProcAddress((HMEMORYMODULE)h, name);
  else {
    /* Search all loaded DLLs */
    int i;
    for (i = 0; embedded_dlls[i].name; i++) {
      if (embedded_dlls[i].loaded_h) {
	void *v = MemoryGetProcAddress((HMEMORYMODULE)embedded_dlls[i].loaded_h, name);
	if (v)
	  return v;
      }
    }
    return NULL;
  }
}

static long in_memory_get_offset(const char *name)
{
  int i;

  for (i = 0; embedded_dlls[i].name; i++) {
    if (!_stricmp(embedded_dlls[i].name, name)) {
      return embedded_dlls[i].pos;
    }
  }
  
  return 0;
}

static void register_embedded_dll_hooks()
{
  if (embedded_dlls) {
    scheme_set_dll_procs(in_memory_open, in_memory_find_object);
  }
}

/**************************************************************/

typedef struct custom_module_t {
  int hooked;
  void *h;
} custom_module_t;

static HCUSTOMMODULE LoadLibraryHook(LPCSTR name, void *data)
{
  void *h;
  int hooked = 1;
  custom_module_t *m;

  h = (HANDLE)in_memory_open(name, 0);
  if (h)
    hooked = 1;
  else {
    h = MemoryDefaultLoadLibrary(name, data);
    hooked = 0;
  }

  if (!h)
    return NULL;

  m = malloc(sizeof(custom_module_t));
  m->hooked = hooked;
  m->h = h;

  return (HCUSTOMMODULE)m;
}

static FARPROC GetProcAddressHook(HCUSTOMMODULE _m, LPCSTR name, void *data)
{
  custom_module_t *m = (custom_module_t *)_m;

  if (m->hooked)
    return in_memory_find_object(m->h, name);
  else
    return MemoryDefaultGetProcAddress(m->h, name, data);
}

/*************************************************************/

/* Set a delay-load hook to potentially redirect to an embedded DLL */

/* From Microsoft docs: */

typedef struct mz_DelayLoadProc {  
  BOOL                fImportByName;  
  union {  
    LPCSTR          szProcName;  
    DWORD           dwOrdinal;  
  };  
} mz_DelayLoadProc;  

typedef struct Mz_DelayLoadInfo {  
  DWORD           cb;         // size of structure  
  void           *pidd;       // raw form of data (everything is there)  
  FARPROC *       ppfn;       // points to address of function to load  
  LPCSTR          szDll;      // name of dll  
  mz_DelayLoadProc dlp;        // name or ordinal of procedure  
  HMODULE         hmodCur;    // the hInstance of the library we have loaded  
  FARPROC         pfnCur;     // the actual function that will be called  
  DWORD           dwLastError;// error received (if an error notification)  
} mz_DelayLoadInfo;

# define mz_dliNotePreLoadLibrary 1
# define mz_dliNotePreGetProcAddress 2

FARPROC WINAPI delayHook(unsigned dliNotify, void *_dli)
{
  mz_DelayLoadInfo *dli = (mz_DelayLoadInfo *)_dli;

  switch (dliNotify) {  
  case mz_dliNotePreLoadLibrary:
    return in_memory_open(dli->szDll, 0);
    break;
  case mz_dliNotePreGetProcAddress:
    {
      void *h;
      h = in_memory_open(dli->szDll, 0);
      if (h) {
	if (dli->dlp.fImportByName)
	  return in_memory_find_object(h, dli->dlp.szProcName);
	else
	  return in_memory_find_object(h, (char *)(intptr_t)dli->dlp.dwOrdinal);
      }
    }
  default:
    break;
  }  

  return NULL;  
}
