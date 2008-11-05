/* 
   Provides:
      allocator
      determine_max_heap_size()
   Requires:
      ACTUALLY_ALLOCATING_PAGES(len)
      ACTUALLY_FREEING_PAGES(len)
   Optional:
      DONT_NEED_MAX_HEAP_SIZE --- to disable a provide
*/

#include <oskit/c/malloc.h>

inline static void *vm_malloc_pages(size_t len, size_t alignment, int dirty_ok)
{
  void *p;

  p = smemalign(alignment, len);
  
  if (!dirty_ok)
  memset(p, 0, len);

  ACTUALLY_ALLOCATING_PAGES(len);

  return p;
}

static void vm_free_pages(void *p, size_t len)
{
  sfree(p, len);
  ACTUALLY_FREEING_PAGES(len);
}

static void vm_flush_freed_pages(void)
{
}

#ifndef DONT_NEED_MAX_HEAP_SIZE
static unsigned long determine_max_heap_size(void)
{
  GCPRINT(GCOUTF, "Don't know how to get heap size for OSKit: assuming 1GB\n");
  return (1 * 1024 * 1024 * 1024);
}
#endif
