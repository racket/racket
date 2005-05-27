/* 
   Provides:
      find_cached_pages --- same interface as malloc_pages
      free_pages --- usual interface
      flush_freed_pages --- usual interface
   Requires (defined earlier):
      system_free_pages --- called with len already rounded up to page size
      page_size --- in bytes
      my_qsort --- possibyl from my_qsort.c
      LOGICALLY_ALLOCATING_PAGES(len)
      ACTUALLY_ALLOCATING_PAGES(len)
      LOGICALLY_FREEING_PAGES(len)
      ACTUALLY_FREEING_PAGES(len)
*/

typedef struct {
  void *start;
  long len;
  int age;
} Free_Block;

#define BLOCKFREE_UNMAP_AGE 1
#define BLOCKFREE_CACHE_SIZE 96
static Free_Block blockfree[BLOCKFREE_CACHE_SIZE];

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

  /* collapse adjacent: */
  my_qsort(blockfree, BLOCKFREE_CACHE_SIZE, sizeof(Free_Block), compare_free_block);
  j = 0;
  for (i = 1; i < BLOCKFREE_CACHE_SIZE; i++) {
    if ((blockfree[j].start + blockfree[j].len) ==blockfree[i].start) {
      blockfree[j].len += blockfree[i].len;
      blockfree[i].start = NULL;
      blockfree[i].len = 0;
    } else
      j = i;
  }
}

inline static void *find_cached_pages(size_t len, size_t alignment)
{
  int i;
  void *r;

  /* Try an exact fit: */
  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (blockfree[i].len == len) {
      r = blockfree[i].start;
      if (!alignment || !((unsigned long)r & (alignment - 1))) {
	blockfree[i].start = NULL;
	blockfree[i].len = 0;
	memset(r, 0, len);
	LOGICALLY_ALLOCATING_PAGES(len);
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
	memset(r, 0, len);
	LOGICALLY_ALLOCATING_PAGES(len);
	return r;
      }

      /* Align at end? */
      r = blockfree[i].start + (blockfree[i].len - len);
      if (!((unsigned long)r & (alignment - 1))) {
	blockfree[i].len -= len;
	memset(r, 0, len);
	LOGICALLY_ALLOCATING_PAGES(len);
	return r;
      }

      /* We don't try a middle alignment, because that would
	 split the block into three. */
    }
  }

  /* Nothing useable in the cache... */
  return NULL;
}

static void free_pages(void *p, size_t len)
{
  int i;

  /* Round up to nearest page: */
  if (len & (page_size - 1))
    len += page_size - (len & (page_size - 1));

  LOGICALLY_FREEING_PAGES(len);

  /* Try to free pages in larger blocks, since the OS may be slow. */

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++)
    if(blockfree[i].start && (blockfree[i].len < (1024 * 1024))) {
      if (p == blockfree[i].start + blockfree[i].len) {
	blockfree[i].len += len;
	return;
      }
      if (p + len == blockfree[i].start) {
	blockfree[i].start = p;
	blockfree[i].len += len;
	return;
      }
    }

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (!blockfree[i].start) {
      blockfree[i].start = p;
      blockfree[i].len = len;
      blockfree[i].age = 0;
      return;
    }
  }

  /* Might help next time around: */
  collapse_adjacent_pages();

  system_free_pages(p, len);

  ACTUALLY_FREEING_PAGES(len);
}

static void flush_freed_pages(void)
{
  int i;

  collapse_adjacent_pages();

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (blockfree[i].start) {
      if (blockfree[i].age == BLOCKFREE_UNMAP_AGE) {
	system_free_pages(blockfree[i].start, blockfree[i].len);
	ACTUALLY_FREEING_PAGES(blockfree[i].len);
	blockfree[i].start = NULL;
	blockfree[i].len = 0;
      } else
	blockfree[i].age++;
    }
  }
}
