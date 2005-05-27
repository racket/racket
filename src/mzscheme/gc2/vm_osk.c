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

#include <oskit/c/malloc.h>

#ifndef GCPRINT
# define GCPRINT fprintf
# define GCOUTF stderr
#endif
#ifndef CHECK_USED_AGAINST_MAX
# define CHECK_USED_AGAINST_MAX(x) /* empty */
#endif

inline static void *malloc_pages(size_t len, size_t alignment)
{
  void *p;

  CHECK_USED_AGAINST_MAX(len);

  p = smemalign(alignment, len);
  memset(p, 0, len);

  ACTUALLY_ALLOCATING_PAGES(len);
  LOGICALLY_ALLOCATING_PAGES(len);

  return p;
}

static void free_pages(void *p, size_t len)
{
  free_used_pages(len);
  sfree(p, len);

  LOGICALLY_FREEING_PAGES(len);
  ACTUALLY_FREEING_PAGES(len);
}

static void flush_freed_pages(void)
{
}

#ifndef DONT_NEED_MAX_HEAP_SIZE
typedef unsigned long size_type;

static size_type determine_max_heap_size(void)
{
  GCPRINT(GCOUTF, 
	  "Don't know how to get heap size for OSKit: assuming 1GB\n");
  return (1 * 1024 * 1024 * 1024);
}
#endif
