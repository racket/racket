/* 
   Provides:
      posix_memalign-based allocator
      determine_max_heap_size() (uses rlimit_heapsize.c)
   Requires:
   Optional:
      DONT_NEED_MAX_HEAP_SIZE --- to disable a provide
*/

#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <errno.h>

static int page_size; /* OS page size */

static void *os_malloc_pages(VM *vm, size_t len, size_t alignment, int dirty_ok)
{
  void *r;

  if (!page_size)
    page_size = getpagesize();

  /* Round up to nearest page: */
  if (len & (page_size - 1))
    len += page_size - (len & (page_size - 1));

  if (posix_memalign(&r, alignment, len)) {
    if (errno == EINVAL)
      printf("Invalid request\n");
    return NULL;
  }

  vm_memory_allocated_inc(vm, len);

  if(!dirty_ok)
    memset(p, 0, len);
  return r;
}

static void os_free_pages(VM *vm, void *p, size_t len)
{
  vm_memory_allocated_dec(vm, len);
  free(p);
}

static void os_protect_pages(void *p, size_t len, int writeable)
{
  if (len & (page_size - 1)) {
    len += page_size - (len & (page_size - 1));
  }

  mprotect(p, len, (writeable ? (PROT_READ | PROT_WRITE) : PROT_READ));
}

/*************************************************************/

# include "rlimit_heapsize.c"
