#include "rktio.h"
#include "rktio_private.h"
#include <stdlib.h>
#include <string.h>

typedef struct rktio_dll_object_t rktio_dll_object_t;

#ifdef RKTIO_SYSTEM_UNIX
static void get_dl_error(rktio_t *rktio);
#endif

/*========================================================================*/
/* Opening a DLL                                                          */
/*========================================================================*/

#ifdef RKTIO_SYSTEM_UNIX
#include <dlfcn.h>
typedef void *dll_handle_t;
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
typedef HANDLE dll_handle_t;
static dll_open_proc LoadLibraryHook;
static dll_find_object_proc GetProcAddressHook;
#endif

struct rktio_dll_t {
  void *handle;
#ifdef RKTIO_SYSTEM_WINDOWS
  int hook_handle;
#endif
  char *name;
  rktio_hash_t *objects_by_name;
  rktio_dll_object_t *all_objects;
  int search_exe;
  rktio_dll_t *all_next; /* chain for all DLLs */
  rktio_dll_t *hash_next; /* chain for hash collisions */
};

rktio_dll_t *rktio_dll_open(rktio_t *rktio, rktio_const_string_t name, rktio_bool_t as_global)
{
  rktio_dll_t *dll, *dlls;
  intptr_t key;
  dll_handle_t handle;
  int null_ok = 0;
#ifdef RKTIO_SYSTEM_WINDOWS
  int hook_handle = 0;
#endif
  
  if (!rktio->dlls_by_name)
    rktio->dlls_by_name = rktio_hash_new();
  
  if (name)
    key = rktio_hash_string(name);
  else
    key = 0;

  dll = dlls = rktio_hash_get(rktio->dlls_by_name, key);
  
  while (dll) {
    if (!name) {
      if (!dll->name)
        break;
    } else if (!strcmp(dll->name, name)) {
      break;
    }
    dll = dll->hash_next;
  }

  if (dll)
    return dll;

#ifdef RKTIO_SYSTEM_UNIX
# if defined(__ANDROID__)
  if (!name) handle = RTLD_DEFAULT; else
# elif defined(__CYGWIN32__)
  if (!name) { handle = RTLD_DEFAULT; null_ok = 1; } else
# endif
    handle = dlopen(name, RTLD_NOW | (as_global ? RTLD_GLOBAL : RTLD_LOCAL));

  if (!handle && !null_ok)
    get_dl_error(rktio);
#endif
  
#ifdef RKTIO_SYSTEM_WINDOWS
  if (!name) {
    /* opening the executable is marked by a NULL handle */
    handle = NULL;
    null_ok = 1;
  } else {
    if (LoadLibraryHook)
      handle = LoadLibraryHook(name, as_global);
    else
      handle = NULL;
    if (handle) {
      hook_handle = 1;
    } else {
      handle = LoadLibraryW(WIDE_PATH_temp(name));
      if (!handle)
	get_windows_error();
    }
  }
#endif

  if (!handle && !null_ok)
    return NULL;

  dll = malloc(sizeof(rktio_dll_t));
  dll->handle = handle;
#ifdef RKTIO_SYSTEM_WINDOWS
  dll->hook_handle = hook_handle;
#endif
  dll->name = (name ? MSC_IZE(strdup)(name) : NULL);
  dll->objects_by_name = rktio_hash_new();
  dll->all_objects = NULL;
  dll->search_exe = (name == NULL);
  
  dll->all_next = rktio->all_dlls;
  rktio->all_dlls = dll;

  dll->hash_next = dlls;
  rktio_hash_set(rktio->dlls_by_name, key, dll);

  return dll;
}

/*========================================================================*/
/* Searching all DLLs on Windows                                          */
/*========================================================================*/

#ifdef RKTIO_SYSTEM_WINDOWS

/* We'd like to use EnumProcessModules to find all loaded DLLs, but it's
   only available in NT 4.0 and later. The alternative, Module32{First,Next},
   is available *except* for NT 4.0! So we try EnumProcessModules first. */

int epm_tried = 0;
typedef BOOL (WINAPI *EnumProcessModules_t)(HANDLE hProcess,
                                            HMODULE* lphModule,
                                            DWORD cb,
                                            LPDWORD lpcbNeeded);
EnumProcessModules_t _EnumProcessModules;
#include <tlhelp32.h>

static BOOL do_EnumProcessModules(HANDLE hProcess, HMODULE* lphModule,
                                  DWORD cb, LPDWORD lpcbNeeded)
{
  if (!epm_tried) {
    HMODULE hm;
    hm = LoadLibraryW(L"psapi.dll");
    if (hm)
      _EnumProcessModules = (EnumProcessModules_t)GetProcAddress(hm, "EnumProcessModules");
    if (!_EnumProcessModules) {
      hm = LoadLibraryW(L"kernel32.dll");
      if (hm)
        _EnumProcessModules = (EnumProcessModules_t)GetProcAddress(hm, "EnumProcessModules");
    }
    epm_tried = 1;
  }

  if (_EnumProcessModules)
    return _EnumProcessModules(hProcess, lphModule, cb, lpcbNeeded);
  else {
    HANDLE snapshot;
    MODULEENTRY32 mod;
    int i, ok;

    snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,
                                        GetCurrentProcessId());
    if (snapshot == INVALID_HANDLE_VALUE)
      return FALSE;

    for (i = 0; 1; i++) {
      mod.dwSize = sizeof(mod);
      if (!i)
        ok = Module32First(snapshot, &mod);
      else
        ok = Module32Next(snapshot, &mod);
      if (!ok)
        break;
      if (cb >= sizeof(HMODULE)) {
        lphModule[i] = mod.hModule;
        cb -= sizeof(HMODULE);
      }
    }

    CloseHandle(snapshot);
    *lpcbNeeded = i * sizeof(HMODULE);
    return GetLastError() == ERROR_NO_MORE_FILES;
  }
}

#endif

/*========================================================================*/
/* Searching for an object in a DLL                                       */
/*========================================================================*/

#ifdef __ANDROID__
static int adjustment_set;
static uintptr_t adjustment;
#endif

struct rktio_dll_object_t {
  char *name;
  void *address;
  rktio_dll_object_t *all_next; /* chain for all object */
  rktio_dll_object_t *hash_next; /* chain for hash collisions */
};

void *rktio_dll_find_object(rktio_t *rktio, rktio_dll_t *dll, rktio_const_string_t name)
{
  rktio_dll_object_t *objs, *obj;
  intptr_t key;
  void *address;

  key = rktio_hash_string(name);
  objs = rktio_hash_get(dll->objects_by_name, key);
  
  for (obj = objs; obj; obj = obj->hash_next)
    if (!strcmp(name, obj->name))
      break;

  if (obj)
    return obj->address;

#ifdef RKTIO_SYSTEM_UNIX
  address = dlsym(dll->handle, name);

# ifdef __ANDROID__
  if (address && (dll->handle == RTLD_DEFAULT)) {
    /* Compensate for a bug in dlsym() that gets the address wrong by
       an offset (incorrect use of `link_bias'?): */
    if (!adjustment_set) {
      adjustment = ((uintptr_t)scheme_start_atomic_no_break
                    - (uintptr_t)dlsym(RTLD_DEFAULT, "rktio_dll_find_object"));
      adjustment_set = 1;
    }
    address = (char *)address + adjustment;
  }
# endif
  
  if (!address && dll->search_exe) {
    /* Try every handle in the table of opened libraries. */
    for (dll = rktio->all_dlls; dll; dll = dll->all_next) {
      address = dlsym(dll->handle, name);
      if (address) break;
    }
  }

  if (!address) {
    get_dl_error(rktio);
    return NULL;
  }
#endif
  
#ifdef RKTIO_SYSTEM_WINDOWS
  if (dll->handle) {
    if (dll->hook_handle)
      address = GetProcAddressHook(dll->handle, name);
    else
      address = GetProcAddress(dll->handle, name);
  } else {
    /* this is for the executable-open case, which was marked by a NULL
     * handle; deal with it by searching all current modules */
#   define NUM_QUICK_MODS 16
    HMODULE *mods, me, quick_mods[NUM_QUICK_MODS];
    DWORD cnt = NUM_QUICK_MODS * sizeof(HMODULE), actual_cnt, i;
    me = GetCurrentProcess();
    mods = quick_mods;
    if (do_EnumProcessModules(me, mods, cnt, &actual_cnt)) {
      if (actual_cnt > cnt) {
        cnt = actual_cnt;
        mods = malloc(cnt);
        if (!do_EnumProcessModules(me, mods, cnt, &actual_cnt))
          mods = NULL;
      } else
        cnt = actual_cnt;
    } else
      mods = NULL;
    if (mods) {
      cnt /= sizeof(HMODULE);
      for (i = 0; i < cnt; i++) {
        address = GetProcAddress(mods[i], name);
        if (address) break;
      }
    } else
      address = NULL;
    if (mods != quick_mods)
      free(mods);
    if (!address && GetProcAddressHook) 
      address = GetProcAddressHook(NULL, name);
  }

  if (!address) {
    get_windows_error();
    return NULL;
  }
#endif

  obj = malloc(sizeof(rktio_dll_object_t));
  obj->name = MSC_IZE(strdup)(name);
  obj->address = address;

  obj->hash_next = objs;
  rktio_hash_set(dll->objects_by_name, key, obj);

  obj->all_next = dll->all_objects;
  dll->all_objects = obj;

  return address;
}

/*========================================================================*/
/* Errors                                                                 */
/*========================================================================*/

#ifdef RKTIO_SYSTEM_UNIX
static void get_dl_error(rktio_t *rktio)
{
  char *s = dlerror();

  if (rktio->dll_error)
    free(rktio->dll_error);

  if (s)
    rktio->dll_error = strdup(s);
  else
    rktio->dll_error = strdup("unknown error");

  set_racket_error(RKTIO_ERROR_DLL);
}
#endif

RKTIO_EXTERN char *rktio_dll_get_error(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_UNIX
  char *s = rktio->dll_error;
  rktio->dll_error = NULL;
  return s;
#else
  return NULL;
#endif
}

/*========================================================================*/
/* Windows hooks                                                          */
/*========================================================================*/

/* Support in-memory DLLs and similar by allowing the application to
   install replacements for LoadLibrary and GetProcAddress. */

void rktio_set_dll_procs(dll_open_proc dll_open, dll_find_object_proc dll_find_object)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  LoadLibraryHook = dll_open;
  GetProcAddressHook = dll_find_object;
#endif
}

#ifdef RKTIO_SYSTEM_WINDOWS
HANDLE rktio_load_library(rktio_const_string_t name)
{
  if (!LoadLibraryHook) return NULL;
  return (HANDLE)LoadLibraryHook(name, 1);
}

void *rktio_get_proc_address(HANDLE m, rktio_const_string_t name)
{
  return GetProcAddressHook((void *)m, name);
}
#endif

/*========================================================================*/
/* Clean up                                                               */
/*========================================================================*/

void rktio_dll_clean(rktio_t *rktio)
{
  rktio_dll_t *dll, *next_dll;
  rktio_dll_object_t *obj, *next_obj;

  for (dll = rktio->all_dlls; dll; dll = next_dll) {
    next_dll = dll->all_next;
    for (obj = dll->all_objects; obj; obj = next_obj) {
      next_obj = obj->all_next;
      free(obj->name);
      free(obj);
    }
    if (dll->name)
      free(dll->name); 
    if (dll->objects_by_name)
      rktio_hash_free(dll->objects_by_name, 0);
    free(dll);
  }

  if (rktio->dlls_by_name)
    rktio_hash_free(rktio->dlls_by_name, 0);

#ifdef RKTIO_SYSTEM_UNIX
  if (rktio->dll_error)
    free(rktio->dll_error);
#endif
}
