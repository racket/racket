/* Provides: */
/*
static void os_vm_free_pages(void *p, size_t len);
static void *os_vm_alloc_pages(size_t len);
static void os_protect_pages(void *p, size_t len, int writeable);
*/
/* Requires: */
/* Optional:
      DONT_NEED_MAX_HEAP_SIZE --- to disable a provide
*/

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <errno.h>

static void os_free_pages(void *p, size_t len)
{
  if (munmap(p, len)) {
    GCPRINT(GCOUTF, "unmap failed: %lx, %ld, %d\n", (long)p, (long)len, errno);
  }
}

static void *os_alloc_pages(size_t len)
{
  void *r;

#ifndef MAP_ANON
  static int fd;
  static int fd_created;

  if (!fd_created) {
    fd_created = 1;
    fd = open("/dev/zero", O_RDWR);
  }
#endif

#ifdef MAP_ANON
  r = mmap(NULL, len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
#else
  r = mmap(NULL, len, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
#endif

  if (r  == (void *)-1)
    return NULL;

  return r;
}


static void os_protect_pages(void *p, size_t len, int writeable)
{
  if (mprotect(p, len, (writeable ? (PROT_READ | PROT_WRITE) : PROT_READ)))
    GCPRINT(GCOUTF, "mprotect failed: %lx, %ld, %d, %d\n", (long)p, (long)len, writeable, errno);
}

#include "rlimit_heapsize.c"
