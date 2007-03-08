/* 
   Provides:
      allocator
      determine_max_heap_size()
   Requires:
      LOGICALLY_ALLOCATING_PAGES(len)
      ACTUALLY_ALLOCATING_PAGES(len)
      LOGICALLY_FREEING_PAGES(len)
      ACTUALLY_FREEING_PAGES(len)
   Optional:
      CHECK_USED_AGAINST_MAX(len)
      GCPRINT
      GCOUTF
      DONT_NEED_MAX_HEAP_SIZE --- to disable a provide
*/

#ifndef GCPRINT
# define GCPRINT fprintf
# define GCOUTF stderr
#endif
#ifndef CHECK_USED_AGAINST_MAX
# define CHECK_USED_AGAINST_MAX(x) /* empty */
#endif

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

static void *malloc_pages(size_t len, size_t alignment)
{
  CHECK_USED_AGAINST_MAX(len);
  LOGICALLY_ALLOCATING_PAGES(len);

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

  ACTUALLY_ALLOCATING_PAGES(len);

  return (void *)VirtualAlloc(NULL, len, 
			      MEM_COMMIT | MEM_RESERVE, 
			      PAGE_READWRITE);
}

#define malloc_dirty_pages(size,align) malloc_pages(size,align)

static void free_pages(void *p, size_t len)
{
  LOGICALLY_FREEING_PAGES(len);

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

  ACTUALLY_FREEING_PAGES(len);

  VirtualFree(p, 0, MEM_RELEASE);
}

static void flush_freed_pages(void)
{
#if CACHE_SLOTS
  int i;
  void *p, *next;

  for (i = 0; i < CACHE_SLOTS; i++) {
    if (cache[1][i].len) {
      for (p = cache[1][i].page; p; p = next) {
	next = *(void **)p;
	ACTUALLY_FREEING_PAGES(cache[i].len);
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

static void protect_pages(void *p, size_t len, int writeable)
{
  DWORD old;
  VirtualProtect(p, len, (writeable ? PAGE_READWRITE : PAGE_READONLY), &old);
}

#ifndef DONT_NEED_MAX_HEAP_SIZE
typedef unsigned long size_type;

typedef BOOL (WINAPI * QueryInformationJobObject_Proc)(HANDLE hJob,
						       JOBOBJECTINFOCLASS JobObjectInfoClass,
						       LPVOID lpJobObjectInfo,
						       DWORD cbJobObjectInfoLength,
						       LPDWORD lpReturnLength);
static size_type determine_max_heap_size(void)
{
  QueryInformationJobObject_Proc qijo;
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION info;
  HMODULE hm;
  SYSTEM_INFO si;

  hm = LoadLibrary("kernel32.dll");
  if (hm)
    qijo = (QueryInformationJobObject_Proc)GetProcAddress(hm, "QueryInformationJobObject");
  else
    qijo = NULL;

  if (qijo) {
    DWORD size;
    if (qijo(NULL, JobObjectExtendedLimitInformation, &info, sizeof(info), &size)) {
      if (info.BasicLimitInformation.LimitFlags & JOB_OBJECT_LIMIT_PROCESS_MEMORY) {
	return info.ProcessMemoryLimit;
      }
    }
  }

  GetSystemInfo(&si);
  return (size_type)si.lpMaximumApplicationAddress - (size_type)si.lpMinimumApplicationAddress;
}
#endif
