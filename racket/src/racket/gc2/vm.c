/******************************************************************************/
/*                     OS-specific low-level allocator                        */
/******************************************************************************/

/* TODO
OSKIT and WINDOWS hard code os_pagesize to APAGE_SIZE
*/

#ifndef GCPRINT
# define GCPRINT fprintf
# define GCOUTF stderr
#endif

enum {
  MMU_WRITE_PROTECTED = 0,
  MMU_WRITABLE        = 1,
};

#if defined(_WIN32) || defined(__CYGWIN32__)
/* No block cache or alloc cache; relies on APAGE_SIZE matching allocator's alignment */
#elif defined(OSKIT)
# define OS_ALLOCATOR_NEEDS_ALIGNMENT
#elif defined(MZ_USE_PLACES) || defined(PREFER_MMAP_LARGE_BLOCKS)
# define USE_BLOCK_CACHE
#else
# define USE_ALLOC_CACHE
#endif

#ifdef USE_BLOCK_CACHE
# define USE_ALLOC_CACHE
# define QUEUED_MPROTECT_INFECTS_SMALL 1
#else
# define QUEUED_MPROTECT_INFECTS_SMALL 0
#endif
#define QUEUED_MPROTECT_INFECTS_MED 0

/* Either USE_ALLOC_CACHE or OS_ALLOCATOR_NEEDS_ALIGNMENT must be
   enabled, unless the lower-level allocator's alignment matches
   APAGE_SIZE. */

struct AllocCacheBlock;
struct BlockCache;
typedef struct MMU {
#ifdef USE_BLOCK_CACHE
  struct BlockCache *block_cache;
#elif defined(USE_ALLOC_CACHE)
  struct AllocCacheBlock *alloc_caches[2];
  Page_Range *page_range;
#endif
  intptr_t memory_allocated;
  size_t os_pagesize;
  NewGC *gc;
} MMU;

#ifdef OS_ALLOCATOR_NEEDS_ALIGNMENT
static void *os_alloc_aligned_pages(size_t len, size_t alignment, int dirty_ok)
#else
static void  *os_alloc_pages(size_t len);
#endif
static void   os_free_pages(void *p, size_t len);
static void   os_protect_pages(void *p, size_t len, int writable);

/* provides */
static inline size_t mmu_get_os_page_size(MMU *mmu) { return mmu->os_pagesize; }
static size_t mmu_memory_allocated(MMU *mmu);

static inline size_t align_up(const size_t len, const size_t boundary) {
  const size_t modulo = (len & (boundary - 1));
  if (modulo) 
    return len + (boundary - modulo);
  return len;
}
static inline void* align_up_ptr(const void *p, const size_t boundary) {
  return (void*) align_up((size_t) p, boundary);
}

static inline size_t align_up_to_gc_pagesize(size_t len) {
  const size_t page_size = APAGE_SIZE;
  return align_up(len, page_size);
}

static inline size_t mmu_round_up_to_os_page_size(MMU *mmu, size_t len) {
  const size_t page_size = mmu->os_pagesize;
  return align_up(len, page_size);
}

static inline void mmu_assert_os_page_aligned(MMU *mmu, size_t p) {
  if (p & (mmu->os_pagesize - 1)) {
    GCPRINT(GCOUTF, "address or size is not page-aligned\n");
    abort();
  }
}

#ifdef USE_BLOCK_CACHE
# include "block_cache.c"
#endif
#ifdef USE_ALLOC_CACHE
# include "alloc_cache.c"
# include "page_range.c"
# include <unistd.h>
#endif

static MMU *mmu_create(NewGC *gc) {
  MMU *mmu = ofm_malloc_zero(sizeof(MMU));
  mmu->gc = gc;

#ifdef USE_ALLOC_CACHE
# ifdef USE_BLOCK_CACHE
  mmu->block_cache = block_cache_create(mmu);
# else
  /* initialization of page_range */
  mmu->page_range = page_range_create();

  /* initialization of alloc_cache */
  mmu->alloc_caches[0] = alloc_cache_create();
  mmu->alloc_caches[1] = alloc_cache_create();
# endif

  mmu->os_pagesize = getpagesize();
#else
  mmu->os_pagesize = APAGE_SIZE;
#endif
  
  return mmu;
}

static void mmu_free(MMU *mmu) {
  /* printf("MMU ALLOCATED PRE  %li\n", mmu->memory_allocated); */
#ifdef USE_BLOCK_CACHE
  mmu->memory_allocated += block_cache_free(mmu->block_cache);
#elif defined(USE_ALLOC_CACHE)
  page_range_free(mmu->page_range);
  mmu->memory_allocated += alloc_cache_free(mmu->alloc_caches[0]);
  mmu->memory_allocated += alloc_cache_free(mmu->alloc_caches[1]);
#endif
  /* printf("MMU ALLOCATED POST %li\n", mmu->memory_allocated); */
  free(mmu);
}

static void *mmu_alloc_page(MMU* mmu, size_t len, size_t alignment, int dirty, int type, int expect_mprotect, void **src_block) {
  mmu_assert_os_page_aligned(mmu, len);
#ifdef USE_BLOCK_CACHE
  return block_cache_alloc_page(mmu->block_cache, len, alignment, dirty, type, expect_mprotect, src_block, &mmu->memory_allocated);
#else
  *src_block = NULL;
# if defined(USE_ALLOC_CACHE)
  /* len = mmu_round_up_to_os_page_size(mmu, len); */
  {
    AllocCacheBlock *alloc_cache = mmu->alloc_caches[!!expect_mprotect];
    return alloc_cache_alloc_page(alloc_cache, len, alignment, dirty, &mmu->memory_allocated);
  }
# else
  mmu->memory_allocated += len;
#  ifdef OS_ALLOCATOR_NEEDS_ALIGNMENT
  return os_alloc_aligned_pages(len, alignment, dirty);
#  else
  return os_alloc_pages(len);
#  endif
# endif
#endif
}

static void mmu_free_page(MMU* mmu, void *p, size_t len, int type, int expect_mprotect, void **src_block,
                          int originated_here) {
  mmu_assert_os_page_aligned(mmu, (size_t)p);
  mmu_assert_os_page_aligned(mmu, len);
#ifdef USE_BLOCK_CACHE
  mmu->memory_allocated += block_cache_free_page(mmu->block_cache, p, len, type, expect_mprotect, src_block,
                                                 originated_here);
#elif defined(USE_ALLOC_CACHE)
  /* len = mmu_round_up_to_os_page_size(mmu, len); */
  {
    AllocCacheBlock *alloc_cache = mmu->alloc_caches[!!expect_mprotect];
    mmu->memory_allocated += alloc_cache_free_page(alloc_cache, p, len, MMU_DIRTY, originated_here);
  }
#else
  if (originated_here) mmu->memory_allocated -= len;
  os_free_pages(p, len);
#endif
}

static void mmu_flush_freed_pages(MMU *mmu) {
#ifdef USE_BLOCK_CACHE
  mmu->memory_allocated += block_cache_flush_freed_pages(mmu->block_cache);
#elif defined(USE_ALLOC_CACHE)
  mmu->memory_allocated += alloc_cache_flush_freed_pages(mmu->alloc_caches[0]);
  mmu->memory_allocated += alloc_cache_flush_freed_pages(mmu->alloc_caches[1]);
#endif  
}

static void mmu_prep_for_compaction(MMU *mmu) {
#ifdef USE_BLOCK_CACHE
  block_cache_prep_for_compaction(mmu->block_cache);
#endif
}

static int mmu_should_compact_page(MMU *mmu, void **src_block) {
#ifdef USE_BLOCK_CACHE
  return block_cache_compact(src_block);
#endif
  return 0;
}

static void mmu_write_unprotect_page(MMU *mmu, void *p, size_t len, int type, void **src_block) {
  mmu_assert_os_page_aligned(mmu, (size_t)p);
  mmu_assert_os_page_aligned(mmu, len);
#ifdef USE_BLOCK_CACHE
  block_cache_protect_one_page(mmu->block_cache, p, len, type, 1, src_block);
#else
  os_protect_pages(p, len, 1);
#endif
}

static void mmu_queue_protect_range(MMU *mmu, void *p, size_t len, int type, int writeable, void **src_block) {
  mmu_assert_os_page_aligned(mmu, (size_t)p);
  mmu_assert_os_page_aligned(mmu, len);
#ifdef USE_BLOCK_CACHE
  block_cache_queue_protect_range(mmu->block_cache, p, len, type, writeable, src_block);
#elif defined(USE_ALLOC_CACHE)
  page_range_add(mmu->page_range, p, len, writeable);
#else
  os_protect_pages(p, len, writeable);
#endif  
}

static void mmu_queue_write_protect_range(MMU *mmu, void *p, size_t len, int type, void **src_block) {
  mmu_queue_protect_range(mmu, p, len, type, MMU_WRITE_PROTECTED, src_block);
}
    
static void mmu_queue_write_unprotect_range(MMU *mmu, void *p, size_t len, int type, void **src_block) {
  mmu_queue_protect_range(mmu, p, len, type, MMU_WRITABLE, src_block);
}

static void mmu_flush_write_protect_ranges(MMU *mmu) {
#ifdef USE_BLOCK_CACHE
  block_cache_flush_protect_ranges(mmu->block_cache, MMU_WRITE_PROTECTED);
#elif defined(USE_ALLOC_CACHE)
  page_range_flush(mmu->page_range, MMU_WRITE_PROTECTED);
#endif  
}

static void mmu_flush_write_unprotect_ranges(MMU *mmu) {
#ifdef USE_BLOCK_CACHE
  block_cache_flush_protect_ranges(mmu->block_cache, MMU_WRITABLE);
#elif defined(USE_ALLOC_CACHE)
  page_range_flush(mmu->page_range, MMU_WRITABLE);
#endif  
}

static size_t mmu_memory_allocated(MMU *mmu) {
  return mmu->memory_allocated;
}


/* _WIN32 and OSKIT use these functions
   On OSX and Linux the block and alloc caches 
   manipulate mmu->memory_allocated directly.
  
   The gc calls mmu_memory_allocated_dec for
   pages that are going to be orphaned sent to a different place.
   The gc calls mmu_memory_allocated_inc for 
   pages that are going to be adopted from another place.
*/
static void mmu_memory_allocated_inc(MMU *mmu, intptr_t amt) {
  mmu->memory_allocated += amt;
}
static void mmu_memory_allocated_dec(MMU *mmu, intptr_t amt) {
  mmu->memory_allocated -= amt;
}

#if _WIN32            /* Windows */
# include "vm_win.c"
#elif defined(OSKIT)  /* OSKit */
# include "vm_osk.c"
#elif defined(OS_X)   /* OS X */
# include "vm_osx.c"
#else                 /* Default: mmap, linux, unix, freebsd*/
# include "vm_mmap.c"
#endif
