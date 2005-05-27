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

static void *malloc_pages(size_t len, size_t alignment)
{
  CHECK_USED_AGAINST_MAX(len);
  ACTUALLY_ALLOCATING_PAGES(len);
  LOGICALLY_ALLOCATING_PAGES(len);

  return (void *)VirtualAlloc(NULL, len, 
			      MEM_COMMIT | MEM_RESERVE, 
			      PAGE_READWRITE);
}

static void free_pages(void *p, size_t len)
{
  VirtualFree(p, 0, MEM_RELEASE);

  LOGICALLY_FREEING_PAGES(len);
  ACTUALLY_FREEING_PAGES(len);
}

static void flush_freed_pages(void)
{
}

static void protect_pages(void *p, size_t len, int writeable)
{
  DWORD old;
  VirtualProtect(p, len, (writeable ? PAGE_READWRITE : PAGE_READONLY), &old);
}

#ifndef DONT_NEED_MAX_HEAP_SIZE
typedef unsigned long size_type;

static size_type determine_max_heap_size(void)
{
  GCPRINT(GCOUTF, 
	  "Don't know how to get heap size for Windows: assuming 1GB\n");
  return (1 * 1024 * 1024 * 1024);
}
#endif
