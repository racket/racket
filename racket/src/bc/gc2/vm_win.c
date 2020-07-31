/* 
   Provides:
      allocator
      determine_max_heap_size()
   Requires:
   Optional:
      DONT_NEED_MAX_HEAP_SIZE --- to disable a provide
*/

static void *os_alloc_pages(size_t len)
{
  /* VirtualAlloc MEM_COMMIT always zeros memory */
  return (void *)VirtualAlloc(NULL, len, 
			      MEM_COMMIT | MEM_RESERVE, 
			      PAGE_READWRITE);
}

static void os_free_pages(void *p, size_t len)
{
  VirtualFree(p, 0, MEM_RELEASE);
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
