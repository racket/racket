/* 
   Provides:
      static intptr_t alloc_cache_free_page(AllocCacheBlock *blockfree, char *p, size_t len, int dirty, int originated_here)
      static intptr_t void alloc_cache_flush_freed_pages(AllocCacheBlock *blockfree)
      static void *alloc_cache_alloc_page(AllocCacheBlock *blockfree,  size_t len, size_t alignment, int dirty_ok, intptr_t *size_diff)
   Requires (defined earlier):
      my_qsort --- possibly from my_qsort.c
      static void os_free_pages(void *p, size_t len);
      static void *os_alloc_pages(size_t len);
      static void *ofm_malloc_zero(size_t len);
      APAGE_SIZE (for cache heuristic)
*/

/* Controls how often freed pages are actually returned to OS: */
#define BLOCKFREE_UNMAP_AGE 3

/* Controls size of the cache */
#define BLOCKFREE_CACHE_SIZE 96

/* Controls how many extra pages are requested from OS at a time: */
#define CACHE_SEED_PAGES 16

typedef struct AllocCacheBlock {
  char *start;
  intptr_t len;
  short age;
  short zeroed;
} AllocCacheBlock;

static AllocCacheBlock *alloc_cache_create() {
  return ofm_malloc_zero(sizeof(AllocCacheBlock) * BLOCKFREE_CACHE_SIZE); 
}

#ifndef NO_ALLOC_CACHE_FREE
static intptr_t alloc_cache_free_all_pages(AllocCacheBlock *blockfree);
static intptr_t alloc_cache_free(AllocCacheBlock *ac) {
  if (ac) {
    intptr_t s = alloc_cache_free_all_pages(ac);
    free(ac);
    return s;
  }
  return 0;
}
#endif

static int alloc_cache_block_compare(const void *a, const void *b)
{
  if ((uintptr_t)((AllocCacheBlock *)a)->start < (uintptr_t)((AllocCacheBlock *)b)->start)
    return -1;
  else
    return 1;
}

static void alloc_cache_collapse_pages(AllocCacheBlock *blockfree)
{
  int i;
  int j;

  /* sort by AllocCacheBlock->start */
  my_qsort(blockfree, BLOCKFREE_CACHE_SIZE, sizeof(AllocCacheBlock), alloc_cache_block_compare);

  /* collapse adjacent: */
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

inline static void *alloc_cache_find_pages(AllocCacheBlock *blockfree, size_t len, size_t alignment, int dirty_ok)
{
  int i;
  void *r;

  /* Try an exact fit: */
  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (blockfree[i].len == len) {
      r = blockfree[i].start;
      if (!alignment || !((uintptr_t)r & (alignment - 1))) {
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
      if (!alignment || !((uintptr_t)r & (alignment - 1))) {
        blockfree[i].start += len;
        blockfree[i].len -= len;
        if (!blockfree[i].zeroed && !dirty_ok)
          memset(r, 0, len);
        return r;
      }

      /* Align at end? */
      r = blockfree[i].start + (blockfree[i].len - len);
      if (!((uintptr_t)r & (alignment - 1))) {
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

static intptr_t alloc_cache_free_page(AllocCacheBlock *blockfree, char *p, size_t len, int dirty, int originated_here)
{
  int i;

  /* Try to free pages in larger blocks, since the OS may be slow. */

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++)
    if(blockfree[i].start && (blockfree[i].len < (1024 * 1024))) {
      if (p == blockfree[i].start + blockfree[i].len) {
        blockfree[i].len += len;
        if (dirty)
          blockfree[i].zeroed = 0;
        return (originated_here ? 0 : len);
      }
      if (p + len == blockfree[i].start) {
        blockfree[i].start = p;
        blockfree[i].len += len;
        if (dirty)
          blockfree[i].zeroed = 0;
        return (originated_here ? 0 : len);
      }
    }

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (!blockfree[i].start) {
      blockfree[i].start = p;
      blockfree[i].len = len;
      blockfree[i].age = 0;
      blockfree[i].zeroed = !dirty;
      return (originated_here ? 0 : len);
    }
  }

  /* Might help next time around: */
  alloc_cache_collapse_pages(blockfree);

  os_free_pages(p, len);
  return (originated_here ? -len : 0);
}

static intptr_t alloc_cache_flush_freed_pages(AllocCacheBlock *blockfree)
{
  int i;
  intptr_t freed = 0;
  alloc_cache_collapse_pages(blockfree);

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (blockfree[i].start) {
      if (blockfree[i].age == BLOCKFREE_UNMAP_AGE) {
        os_free_pages(blockfree[i].start, blockfree[i].len);
        freed -= blockfree[i].len;
        blockfree[i].start = NULL;
        blockfree[i].len = 0;
      } else
        blockfree[i].age++;
    }
  }
  return freed;
}

#ifndef NO_ALLOC_CACHE_FREE
static intptr_t alloc_cache_free_all_pages(AllocCacheBlock *blockfree)
{
  int i;
  intptr_t freed = 0;
  alloc_cache_collapse_pages(blockfree);

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (blockfree[i].start) {
        os_free_pages(blockfree[i].start, blockfree[i].len);
        freed -= blockfree[i].len;
        blockfree[i].start = NULL;
        blockfree[i].len = 0;
    }
  }
  return freed;
}
#endif

/* Instead of immediately freeing pages with munmap---only to mmap
   them again---we cache BLOCKFREE_CACHE_SIZE freed pages. A page is
   cached unused for at most BLOCKFREE_UNMAP_AGE cycles of the
   collector. (A max age of 1 seems useful, anything more seems
   dangerous.) 

   The cache is small enough that we don't need an elaborate search
   mechanism, but we do a bit of work to collapse adjacent pages in
   the cache. */

static void *alloc_cache_alloc_page(AllocCacheBlock *blockfree,  size_t len, size_t alignment, int dirty_ok, intptr_t *size_diff)
{
  char *r;

  /* Something from the cache, perhaps? */
  r = alloc_cache_find_pages(blockfree, len, alignment, dirty_ok);
  if(!r) {
    /* attempt to allocate from OS */
    size_t extra = (alignment ? (alignment + CACHE_SEED_PAGES * APAGE_SIZE) : 0);
    r = os_alloc_pages(len + extra);
    if(r == (void *)-1) { return NULL; }

    if (alignment) {
      /* We allocated too large so we can choose the alignment. */
      char *real_r    = (char*)(((uintptr_t)r + (alignment - 1)) & (~(alignment - 1)));
      intptr_t pre_extra  = real_r - r;

      /* in front extra */
      if (pre_extra) { 
        /* printf("FREEING FRONT %p %lx\n", r, pre_extra); */
        os_free_pages(r, pre_extra); }
      /* in back extra exists */
      if (pre_extra < extra) {
        if (pre_extra == 0) {
          /* Instead of actually unmapping, put it in the cache, and there's
             a good chance we can use it next time: */
          (*size_diff) += extra;
          (*size_diff) += alloc_cache_free_page(blockfree, real_r + len, extra, 1, 1);
        } else { 
          os_free_pages(real_r + len, extra - pre_extra);
        }
      }
      r = real_r;
    }

    (*size_diff) += len;
  }

  return r;
}
