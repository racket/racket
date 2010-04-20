/* 
   Provides:
      allocator
      determine_max_heap_size()
   Requires:
   Optional:
      DONT_NEED_MAX_HEAP_SIZE --- to disable a provide
*/

#include <oskit/c/malloc.h>

inline static void *vm_malloc_pages(VM *vm, size_t len, size_t alignment, int dirty_ok)
{
  void *p;

  p = smemalign(alignment, len);
  
  if (!dirty_ok)
  memset(p, 0, len);

  vm_memory_allocated_inc(vm, len);

  return p;
}

static void vm_free_pages(VM *vm, void *p, size_t len)
{
  vm_memory_allocated_dec(vm, len);
  sfree(p, len);
}

static void vm_flush_freed_pages(VM *vm)
{
}

#ifndef DONT_NEED_MAX_HEAP_SIZE
static unsigned long determine_max_heap_size(void)
{
  GCPRINT(GCOUTF, "Don't know how to get heap size for OSKit: assuming 1GB\n");
  return (1 * 1024 * 1024 * 1024);
}
#endif
