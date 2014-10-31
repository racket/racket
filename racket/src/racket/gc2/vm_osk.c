/* 
   Provides:
      allocator
      determine_max_heap_size()
   Requires:
   Optional:
      DONT_NEED_MAX_HEAP_SIZE --- to disable a provide
*/

#include <oskit/c/malloc.h>

inline static void *os_alloc_aligned_pages(size_t len, size_t alignment, int dirty_ok)
{
  void *p;

  p = smemalign(alignment, len);
  
  if (!dirty_ok)
    memset(p, 0, len);

  return p;
}

static void os_free_pages(MMU *mmu, void *p, size_t len)
{
  sfree(p, len);
}

#ifndef DONT_NEED_MAX_HEAP_SIZE
static unsigned long determine_max_heap_size(void)
{
  GCPRINT(GCOUTF, "Don't know how to get heap size for OSKit: assuming 1GB\n");
  return (1 * 1024 * 1024 * 1024);
}
#endif
