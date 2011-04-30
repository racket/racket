/* 
   Provides:
      allocator
      determine_max_heap_size()
   Requires:
   Optional:
      DONT_NEED_MAX_HEAP_SIZE --- to disable a provide
*/

/* Cache doesn't seem to help in Windows: */
#define CACHE_SLOTS 0

#if CACHE_SLOTS
typedef struct {
  size_t len;
  void *page;
} alloc_cache_entry;

/* First dimension is age: */
static alloc_cache_entry cache[2][CACHE_SLOTS];
#endif

static void *os_alloc_pages(MMU *mmu, size_t len, size_t alignment, int dirty_ok)
{
#if CACHE_SLOTS
  {
    int i, j;

    for (j = 0; j < 2; j++) {
      for (i = 0; i < CACHE_SLOTS; i++) {
        if (cache[j][i].len == len) {
          if (cache[j][i].page) {
            void *result = cache[j][i].page;
            cache[j][i].page = *(void **)result;
            memset(result, 0, len);
            return result;
          }
          break;
        }
      }
    }
  }
#endif

  /* VirtualAlloc MEM_COMMIT always zeros memory */
  return (void *)VirtualAlloc(NULL, len, 
			      MEM_COMMIT | MEM_RESERVE, 
			      PAGE_READWRITE);
}

static void os_free_pages(MMU *mmu, void *p, size_t len)
{

#if CACHE_SLOTS
  {
    int i;

    for (i = 0; i < CACHE_SLOTS; i++) {
      if (!cache[0][i].len)
        cache[0][i].len = len;
      if (cache[0][i].len == len) {
        *(void **)p = cache[0][i].page;
        cache[0][i].page = p;
        return;
      }
    }
  }
#endif

  VirtualFree(p, 0, MEM_RELEASE);
}

static void os_flush_freed_pages(MMU *mmu)
{
#if CACHE_SLOTS
  int i;
  void *p, *next;

  for (i = 0; i < CACHE_SLOTS; i++) {
    if (cache[1][i].len) {
      for (p = cache[1][i].page; p; p = next) {
        next = *(void **)p;
        mmu_memory_allocated_dec(mmu, cache[i].len);
        VirtualFree(p, 0, MEM_RELEASE);
      }
    }
    cache[1][i].len = cache[0][i].len;
    cache[1][i].page = cache[0][i].page;
    cache[0][i].len = 0;
    cache[0][i].page = NULL;
  }
#endif
}

static void os_protect_pages(void *p, size_t len, int writeable)
{
  DWORD old;
  VirtualProtect(p, len, (writeable ? PAGE_READWRITE : PAGE_READONLY), &old);
}

#ifndef DONT_NEED_MAX_HEAP_SIZE
typedef uintptr_t size_type;

typedef BOOL (WINAPI * QueryInformationJobObject_Proc)(HANDLE hJob,
    JOBOBJECTINFOCLASS JobObjectInfoClass,
    LPVOID lpJobObjectInfo,
    DWORD cbJobObjectInfoLength,
    LPDWORD lpReturnLength);

static size_type determine_max_heap_size(void)
{
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION info;
  HMODULE hm;
  SYSTEM_INFO si;

  hm = LoadLibrary("kernel32.dll");
  if (hm) {
    DWORD size;
    QueryInformationJobObject_Proc qijo = NULL;
    qijo = (QueryInformationJobObject_Proc)GetProcAddress(hm, "QueryInformationJobObject");
    if (qijo) {
      if (qijo(NULL, JobObjectExtendedLimitInformation, &info, sizeof(info), &size)) {
        if (info.BasicLimitInformation.LimitFlags & JOB_OBJECT_LIMIT_PROCESS_MEMORY) {
          return info.ProcessMemoryLimit;
        }
      }
    }
  }

  GetSystemInfo(&si);
  return (size_type)si.lpMaximumApplicationAddress - (size_type)si.lpMinimumApplicationAddress;
}
#endif
