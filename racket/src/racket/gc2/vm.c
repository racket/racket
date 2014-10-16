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

#if (defined(MZ_USE_PLACES) && !defined(_WIN32)) || defined(PREFER_MMAP_LARGE_BLOCKS)
# define USE_BLOCK_CACHE
#endif

struct AllocCacheBlock;
struct BlockCache;
typedef struct MMU {
#ifdef USE_BLOCK_CACHE
  struct BlockCache *block_cache;
#elif !( defined(_WIN32) || defined(OSKIT) )
  struct AllocCacheBlock *alloc_caches[2];
  Page_Range *page_range;
#endif
  intptr_t memory_allocated;
  size_t os_pagesize;
  NewGC *gc;
} MMU;

#if !( defined(_WIN32) || defined(OSKIT) )
static void  *os_alloc_pages(size_t len);
static void   os_free_pages(void *p, size_t len);
static void   os_protect_pages(void *p, size_t len, int writable);
#else
static void  *os_alloc_pages(MMU *mmu, size_t len, size_t alignment, int dirty);
static void   os_free_pages(MMU *mmu, void *p, size_t len);
static void   os_protect_pages(void *p, size_t len, int writable);
static void   os_flush_freed_pages(MMU *mmu);
#endif

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
    printf("address or size is not OS PAGE ALIGNED!!!!");
    abort();
  }
}

#ifdef USE_BLOCK_CACHE
#include "block_cache.c"
#include "alloc_cache.c"
#include "page_range.c"
#include <unistd.h>
#elif !( defined(_WIN32) || defined(OSKIT) )
#include "alloc_cache.c"
#include "page_range.c"
#include <unistd.h>
#endif


static MMU *mmu_create(NewGC *gc) {
  MMU *mmu = ofm_malloc_zero(sizeof(MMU));
  mmu->gc = gc;

#if !( defined(_WIN32) || defined(OSKIT) )
#ifdef USE_BLOCK_CACHE
  mmu->block_cache = block_cache_create(mmu);
#else
  /* initialization of page_range */
  mmu->page_range = page_range_create();

  /* initialization of alloc_cache */
  mmu->alloc_caches[0] = alloc_cache_create();
  mmu->alloc_caches[1] = alloc_cache_create();
#endif

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
#elif !( defined(_WIN32) || defined(OSKIT) )
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
#elif !( defined(_WIN32) || defined(OSKIT) )
  /* len = mmu_round_up_to_os_page_size(mmu, len); */
  {
    AllocCacheBlock *alloc_cache = mmu->alloc_caches[!!expect_mprotect];
    return alloc_cache_alloc_page(alloc_cache, len, alignment, dirty, &mmu->memory_allocated);
  }
#else
  mmu->memory_allocated += len;
  return os_alloc_pages(mmu, len, alignment, dirty);
#endif
}

static void mmu_free_page(MMU* mmu, void *p, size_t len, int type, int expect_mprotect, void **src_block,
                          int originated_here) {
  mmu_assert_os_page_aligned(mmu, (size_t)p);
  mmu_assert_os_page_aligned(mmu, len);
#ifdef USE_BLOCK_CACHE
  mmu->memory_allocated += block_cache_free_page(mmu->block_cache, p, len, type, expect_mprotect, src_block,
                                                 originated_here);
#elif !( defined(_WIN32) || defined(OSKIT) )
  /* len = mmu_round_up_to_os_page_size(mmu, len); */
  {
    AllocCacheBlock *alloc_cache = mmu->alloc_caches[!!expect_mprotect];
    mmu->memory_allocated += alloc_cache_free_page(alloc_cache, p, len, MMU_DIRTY, originated_here);
  }
#else
  if (originated_here) mmu->memory_allocated -= len;
  os_free_pages(mmu, p, len);
#endif
}

static void mmu_flush_freed_pages(MMU *mmu) {
#ifdef USE_BLOCK_CACHE
  mmu->memory_allocated += block_cache_flush_freed_pages(mmu->block_cache);
#elif !( defined(_WIN32) || defined(OSKIT) )
  mmu->memory_allocated += alloc_cache_flush_freed_pages(mmu->alloc_caches[0]);
  mmu->memory_allocated += alloc_cache_flush_freed_pages(mmu->alloc_caches[1]);
#elif defined(_WIN32)
  os_flush_freed_pages(mmu);
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

static void mmu_write_unprotect_page(MMU *mmu, void *p, size_t len) {
  mmu_assert_os_page_aligned(mmu, (size_t)p);
  mmu_assert_os_page_aligned(mmu, len);
  os_protect_pages(p, len, 1);
}

static void mmu_queue_protect_range(MMU *mmu, void *p, size_t len, int type, int writeable, void **src_block) {
  mmu_assert_os_page_aligned(mmu, (size_t)p);
  mmu_assert_os_page_aligned(mmu, len);
#ifdef USE_BLOCK_CACHE
  block_cache_queue_protect_range(mmu->block_cache, p, len, type, writeable, src_block);
#elif !( defined(_WIN32) || defined(OSKIT) )
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
#elif !( defined(_WIN32) || defined(OSKIT) )
  page_range_flush(mmu->page_range, MMU_WRITE_PROTECTED);
#endif  
}

static void mmu_flush_write_unprotect_ranges(MMU *mmu) {
#ifdef USE_BLOCK_CACHE
  block_cache_flush_protect_ranges(mmu->block_cache, MMU_WRITABLE);
#elif !( defined(_WIN32) || defined(OSKIT) )
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
