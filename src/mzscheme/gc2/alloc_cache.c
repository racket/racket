/* 
   Provides:
      find_cached_pages --- same interface as malloc_pages
      vm_free_pages --- usual interface
      vm_flush_freed_pages --- usual interface
   Requires (defined earlier):
      page_size --- in bytes
      my_qsort --- possibly from my_qsort.c
      ACTUALLY_ALLOCATING_PAGES(len)
      ACTUALLY_FREEING_PAGES(len)
*/

/* interface to GC */
/*
static void *malloc_pages(size_t len, size_t alignment);
static void *malloc_dirty_pages(size_t len, size_t alignment);
static void free_pages(void *p, size_t len);
static void flush_freed_pages(void);
static void protect_pages(void *p, size_t len, int writable);
*/

/* interface to OS */
/*
static void os_vm_free_pages(void *p, size_t len);
static void *os_vm_alloc_pages(size_t len);
*/
#define BLOCKFREE_UNMAP_AGE 1

static int compare_free_block(const void *a, const void *b)
{
  if ((unsigned long)((Free_Block *)a)->start < (unsigned long)((Free_Block *)b)->start)
    return -1;
  else
    return 1;
}

static void collapse_adjacent_pages(void)
{
  int i, j;
  Free_Block *blockfree = GC->blockfree;

  /* collapse adjacent: */
  my_qsort(blockfree, BLOCKFREE_CACHE_SIZE, sizeof(Free_Block), compare_free_block);
  j = 0;
  for (i = 1; i < BLOCKFREE_CACHE_SIZE; i++) {
    if ((blockfree[j].start + blockfree[j].len) == blockfree[i].start) {
      blockfree[j].len += blockfree[i].len;
      blockfree[i].start = NULL;
      blockfree[i].len = 0;
      if (!blockfree[i].zeroed)
        blockfree[j].zeroed = 0;
    } else
      j = i;
  }
}

inline static void *find_cached_pages(size_t len, size_t alignment, int dirty_ok)
{
  int i;
  void *r;
  Free_Block *blockfree = GC->blockfree;

  /* Try an exact fit: */
  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (blockfree[i].len == len) {
      r = blockfree[i].start;
      if (!alignment || !((unsigned long)r & (alignment - 1))) {
        blockfree[i].start = NULL;
        blockfree[i].len = 0;
        if (!blockfree[i].zeroed && !dirty_ok)
          memset(r, 0, len);
        return r;
      }
    }
  }

  /* Try a first fit: */
  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (blockfree[i].len > len) {
      /* Align at start? */
      r = blockfree[i].start;
      if (!alignment || !((unsigned long)r & (alignment - 1))) {
        blockfree[i].start += len;
        blockfree[i].len -= len;
        if (!blockfree[i].zeroed && !dirty_ok)
          memset(r, 0, len);
        return r;
      }

      /* Align at end? */
      r = blockfree[i].start + (blockfree[i].len - len);
      if (!((unsigned long)r & (alignment - 1))) {
        blockfree[i].len -= len;
        if (!blockfree[i].zeroed && !dirty_ok)
          memset(r, 0, len);
        return r;
      }

      /* We don't try a middle alignment, because that would
         split the block into three. */
    }
  }

  /* Nothing useable in the cache... */
  return NULL;
}

static void return_mem_to_cache(void *p, size_t len, int zeroed)
{
  int i;
  Free_Block *blockfree = GC->blockfree;

  /* Round up to nearest page: */
  if (len & (page_size - 1))
    len += page_size - (len & (page_size - 1));

  /* Try to free pages in larger blocks, since the OS may be slow. */

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++)
    if(blockfree[i].start && (blockfree[i].len < (1024 * 1024))) {
      if (p == blockfree[i].start + blockfree[i].len) {
        blockfree[i].len += len;
        if (!zeroed)
          blockfree[i].zeroed = 0;
        return;
      }
      if (p + len == blockfree[i].start) {
        blockfree[i].start = p;
        blockfree[i].len += len;
        if (!zeroed)
          blockfree[i].zeroed = 0;
        return;
      }
    }

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (!blockfree[i].start) {
      blockfree[i].start = p;
      blockfree[i].len = len;
      blockfree[i].age = 0;
      blockfree[i].zeroed = zeroed;
      return;
    }
  }

  /* Might help next time around: */
  collapse_adjacent_pages();

  os_vm_free_pages(p, len);

  ACTUALLY_FREEING_PAGES(len);
}

static void vm_free_pages(void *p, size_t len)
{
  return_mem_to_cache(p, len, 0);
}

static void vm_flush_freed_pages(void)
{
  int i;
  Free_Block *blockfree = GC->blockfree;

  collapse_adjacent_pages();

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (blockfree[i].start) {
      if (blockfree[i].age == BLOCKFREE_UNMAP_AGE) {
        os_vm_free_pages(blockfree[i].start, blockfree[i].len);
        ACTUALLY_FREEING_PAGES(blockfree[i].len);
        blockfree[i].start = NULL;
        blockfree[i].len = 0;
      } else
        blockfree[i].age++;
    }
  }
}

/* Instead of immediately freeing pages with munmap---only to mmap
   them again---we cache BLOCKFREE_CACHE_SIZE freed pages. A page is
   cached unused for at most BLOCKFREE_UNMAP_AGE cycles of the
   collector. (A max age of 1 seems useful, anything more seems
   dangerous.) 

   The cache is small enough that we don't need an elaborate search
   mechanism, but we do a bit of work to collapse adjacent pages in
   the cache. */

static void *vm_malloc_pages(size_t len, size_t alignment, int dirty_ok)
{
  void *r;

  if (!page_size)
    page_size = getpagesize();

  /* Round up to nearest page: */
  if (len & (page_size - 1))
    len += page_size - (len & (page_size - 1));

  /* Something from the cache, perhaps? */
  r = find_cached_pages(len, alignment, dirty_ok);
  if(!r) {
    /* attempt to allocate from OS */
    r = os_vm_alloc_pages(len + alignment);
    if(r == (void *)-1) { return NULL; }

    if (alignment) {
      /* We allocated too large so we can choose the alignment. */
      size_t extra    = alignment;
      void *real_r    = (void *)(((unsigned long)r + (alignment - 1)) & (~(alignment - 1)));
      long pre_extra  = real_r - r;

      /* in front extra */
      if (pre_extra) { os_vm_free_pages(r, pre_extra); }
      /* in back extra exists */
      if (pre_extra < extra) {
        if (pre_extra == 0) {
          /* Instead of actually unmapping, put it in the cache, and there's
             a good chance we can use it next time: */
          ACTUALLY_ALLOCATING_PAGES(extra);
          return_mem_to_cache(real_r + len, extra, 1);
        } 
        else { os_vm_free_pages(real_r + len, extra - pre_extra); }
      }
      r = real_r;
    }

    ACTUALLY_ALLOCATING_PAGES(len);
  }

  return r;
}
