/******************************************************************************/
/*                     OS-specific low-level allocator                        */
/******************************************************************************/
#include "gclist.h"
/* requires: */
static void  *os_alloc_pages(size_t len);
static void   os_free_pages(void *p, size_t len);
static void   os_protect_pages(void *p, size_t len, int writable);

#define BC_STARTING_BLOCK_SIZE (1 << 21)  /* 2 MB */
#define BC_MAX_BLOCK_SIZE (1 << 24)  /* 16 MB */

struct block_desc;
static AllocCacheBlock *alloc_cache_create();
static ssize_t alloc_cache_free(AllocCacheBlock *);
static ssize_t alloc_cache_free_page(AllocCacheBlock *blockfree, char *p, size_t len, int dirty, int originated_here);
static ssize_t alloc_cache_flush_freed_pages(AllocCacheBlock *blockfree);
static void *alloc_cache_alloc_page(AllocCacheBlock *blockfree,  size_t len, size_t alignment, int dirty_ok, ssize_t *size_diff);

static Page_Range *page_range_create();
static void page_range_free(Page_Range *pr);
static void page_range_flush(Page_Range *pr, int writeable);
static void page_range_add(Page_Range *pr, void *_start, uintptr_t len, int writeable);


#ifdef BC_ASSERTS
static int block_cache_chain_stat(GCList *head, int *bcnt);
#endif

struct block_group;
typedef struct block_desc {
  GCList gclist;
  void *block;
  void *free;
  intptr_t size;
  intptr_t used;
  intptr_t totalcnt;
  intptr_t freecnt;
  struct block_group *group;
  int in_queue;
} block_desc;

typedef struct block_group {
  GCList full;
  GCList free;
  int atomic;
  int block_size;
} block_group;

typedef struct BlockCache {
  block_group atomic;
  block_group non_atomic;
  AllocCacheBlock *bigBlockCache;
  Page_Range *page_range;
  MMU *mmu;
} BlockCache;

typedef struct pfree_list {
  void *next;
  int   dirty;
} pfree_list;

static BlockCache* block_cache_create(MMU *mmu) {
  BlockCache *bc = ofm_malloc_zero(sizeof(BlockCache));
  gclist_init(&bc->atomic.full);
  gclist_init(&bc->atomic.free);
  bc->atomic.atomic = 1;
  bc->atomic.block_size = BC_STARTING_BLOCK_SIZE;
  gclist_init(&bc->non_atomic.full);
  gclist_init(&bc->non_atomic.free);
  bc->non_atomic.atomic = 0;
  bc->non_atomic.block_size = BC_STARTING_BLOCK_SIZE;
  bc->bigBlockCache = alloc_cache_create();
  bc->page_range = page_range_create();
  bc->mmu = mmu;
  return bc;
}

static ssize_t block_cache_free(BlockCache* bc) {
  ssize_t acf = alloc_cache_free(bc->bigBlockCache);
  page_range_free(bc->page_range);
  free(bc);
  return acf;
}

static block_desc *bc_alloc_std_block(block_group *bg) {
  int this_block_size = bg->block_size;
  void *r = os_alloc_pages(this_block_size);
  block_desc *bd;
  void *ps;

  if (!r) return NULL;

  ps = align_up_ptr(r, APAGE_SIZE);

  if (this_block_size < BC_MAX_BLOCK_SIZE) {
    bg->block_size <<= 1;
  }

  bd = (block_desc*) ofm_malloc(sizeof(block_desc));
  if (!bd) {
    os_free_pages(r, this_block_size);
    return NULL;
  }
  bd->block = r;
  bd->free = ps;
  bd->size = this_block_size;
  bd->used = 0;
  bd->group = bg;
  gclist_init(&bd->gclist);

  /* printf("ALLOC BLOCK %p-%p size %li %li %li %p\n", bd->block, bd->block + bd->size, bd->size, APAGE_SIZE, bd->size / APAGE_SIZE, bd->free); */
  /* free unaligned portion */
  {
    intptr_t diff = ps -r;
    if (diff) {
      intptr_t enddiff = APAGE_SIZE - diff;
      os_free_pages(r, diff);
      os_free_pages(r + this_block_size - enddiff, enddiff);
      bd->block = ps;
      bd->size  = this_block_size - APAGE_SIZE;
      /* printf("UNALIGNED FROM OS %p %li %li\n", r, diff, enddiff); */
    }
  }


  /* setup free list of APAGE_SIZE sized pages inside block */
  { 
    int i = 0;
    pfree_list *pe = (bd->block + bd->size);
    pfree_list *p = ps;
    pfree_list *n;
    while(p < pe) {
      n = ((void*) p) + APAGE_SIZE;
      p->next = n;
      p->dirty = 0;
      p = n;
      i++;
    }
    bd->totalcnt = i;
    bd->freecnt  = i;
    if (p > pe) { p = (p - (2 * APAGE_SIZE)); }
    else        { p = (p - APAGE_SIZE); }
    p->next = NULL;
    /* printf("ENDUP %p %p %p %i\n", n, p, p->next, i); */
  }
  
  return bd;
}

static void *bc_alloc_std_page(BlockCache *bc, int dirty_ok, int expect_mprotect, void **src_block, ssize_t *size_diff) {
  block_group *bg = (expect_mprotect ? &bc->non_atomic : &bc->atomic);
  GCList *free_head = &bg->free;
#if BC_ASSERTS
  int newbl = 0;
#endif

  tryagain:
  if (!gclist_is_empty(free_head)) {
    if (!gclist_first_item(free_head, block_desc*, gclist)->free) {
      gclist_move(free_head->next, &bg->full);
      goto tryagain;
    }
  }
  else {
    block_desc *bd;
#if BC_ASSERTS
    newbl = 1;
#endif
    bd = bc_alloc_std_block(bg);
    if (!bd) return NULL;
    gclist_add(free_head, &(bd->gclist));
    (*size_diff) += bd->size;
    /* printf("ALLOC BLOCK %i %p %p-%p size %li %p\n", expect_mprotect, bg, bd->block, bd->block + bd->size, bd->size, bd->free); */
  }
  
  {
    block_desc *bd = gclist_first_item(free_head, block_desc*, gclist);
    pfree_list *fl = bd->free;
    void *p = fl;
    bd->free = fl->next;
    bd->freecnt--;

    *src_block = bd;
    if (expect_mprotect) {
      GC_MP_CNT_INC(mp_alloc_med_big_cnt);
      os_protect_pages(p, APAGE_SIZE, 1);
    }

    if (!dirty_ok) {
      if (fl->dirty)
        memset(p, 0, APAGE_SIZE);
      else
        fl->next = 0;
    }

#if BC_ASSERTS
    assert(p >= bd->block);
    assert(p+APAGE_SIZE <= bd->block + bd->size);
    if (!bg->atomic)
    {
      int afub = 0;
      int afrb  = 0;
      int nafub = 0;
      int nafrb = 0;
      int afu = block_cache_chain_stat(&bc->atomic.full, &afub);
      int afr = block_cache_chain_stat(&bc->atomic.free, &afrb);
      int nafu = block_cache_chain_stat(&bc->non_atomic.full, &nafub);
      int nafr = block_cache_chain_stat(&bc->non_atomic.free, &nafrb);
      printf("ALLOC PAGE %i %p %p-%p %03i %03i %04i %04i : %03i %03i %03i %03i %09i %s\n", expect_mprotect, bg, p, p + APAGE_SIZE, afu, afr, nafu, nafr, afub, afrb, nafub, nafrb, mmu_memory_allocated(bc->mmu), (newbl ? "NEW " : ""));
    }
#endif
    return p;
  }
}

static ssize_t bc_free_std_block(block_desc *b) {
  ssize_t size_diff = 0;
  gclist_del(&b->gclist);
  os_free_pages(b->block, b->size);
  size_diff -= b->size;
  free(b);
  return size_diff;
}
  
static void *block_cache_alloc_page(BlockCache* bc, size_t len, size_t alignment, int dirty, int type, int expect_mprotect, void **src_block, ssize_t *size_diff) {
  switch(type) {
    case MMU_SMALL_GEN1:
      return bc_alloc_std_page(bc, dirty, expect_mprotect, src_block, size_diff);
      break;
    default:
      *(char**)src_block = (char*) ~0x0;
      return alloc_cache_alloc_page(bc->bigBlockCache, len, APAGE_SIZE, dirty, size_diff);
      break;
  }
}

#if BC_ASSERTS
static int find_addr_in_bd(GCList *head, void *p, char* msg) {
  block_desc *b;
  gclist_each_item(b, head, block_desc*, gclist) {
    if (p >= b->block && p < b->block + b->size) {
      return 1;
    }
  }
  return 0;
}
#endif

static ssize_t block_cache_free_page(BlockCache* bc, void *p, size_t len, int type, int expect_mprotect, void **src_block,
                                     int originated_here) {
  switch(type) {
    case MMU_SMALL_GEN1:
      {
        GCList *free_head = &((expect_mprotect ? &bc->non_atomic : &bc->atomic)->free);
        block_desc *b = (block_desc*)(*src_block);
        pfree_list *fl = p;
        fl->next = b->free;
        fl->dirty = 1;
        b->free = fl;
#if BC_ASSERTS
        assert(*src_block != (char*)~0x0);
        assert(b->group == bg);
#endif
        gclist_move(&b->gclist, free_head);
        b->freecnt++;
#if BC_ASSERTS
        if (!bg->atomic)
        {
          int afub = 0;
          int afrb  = 0;
          int nafub = 0;
          int nafrb = 0;
          int afu = block_cache_chain_stat(&bc->atomic.full, &afub);
          int afr = block_cache_chain_stat(&bc->atomic.free, &afrb);
          int nafu = block_cache_chain_stat(&bc->non_atomic.full, &nafub);
          int nafr = block_cache_chain_stat(&bc->non_atomic.free, &nafrb);
          printf("FREE  PAGE %i %p %p-%p %03i %03i %04i %04i : %03i %03i %03i %03i %09i\n", expect_mprotect, bg, p, p + APAGE_SIZE, afu, afr, nafu, nafr, afub, afrb, nafub, nafrb, mmu_memory_allocated(bc->mmu));
        }
#endif
        return (originated_here ? 0 : len);
      }
      break;
    default:
#if BC_ASSERTS
      assert(!(find_addr_in_bd(&bc->atomic.full, p, "atomic full") ||
               find_addr_in_bd(&bc->atomic.free, p, "atomic freeblock") ||
               find_addr_in_bd(&bc->non_atomic.full, p, "non_atomic full") ||
               find_addr_in_bd(&bc->non_atomic.free, p, "non_atomic freeblock")));
      assert(*src_block == (char*)~0x0);
#endif
      return alloc_cache_free_page(bc->bigBlockCache, p, len, MMU_DIRTY, originated_here);
      break;
  }
}

static int sort_full_to_empty(void *priv, GCList *a, GCList *b) {
  block_desc *ba = gclist_item(a, block_desc*, gclist);
  block_desc *bb = gclist_item(b, block_desc*, gclist);

  if ((ba->freecnt) <= (bb->freecnt)) {
    return -1;
  }
  return 1;
}

static void block_cache_prep_for_compaction(BlockCache* bc) {
  gclist_sort(NULL, &bc->atomic.free, sort_full_to_empty);
  gclist_sort(NULL, &bc->non_atomic.free, sort_full_to_empty);
#if 0
  {
    block_desc *b;
    gclist_each_item(b, &bc->atomic.free, block_desc*, gclist) {
      printf("   ATOMIC %05li %03li %p\n", b->freecnt, b->totalcnt, b); }
    gclist_each_item(b, &bc->non_atomic.free, block_desc*, gclist) {
      printf("NONATOMIC %03li %03li %p\n", b->freecnt, b->totalcnt, b);
    }
  }
#endif
}

static int block_cache_compact(void **src_block) {
  block_desc *b = *src_block;
  if (b->freecnt > (b->totalcnt/2)) {
    return 1;
  }
  return 0;
}

static ssize_t block_cache_flush_freed_pages(BlockCache* bc) {
  block_desc *b;
  block_desc *bn;
  ssize_t size_diff = 0;
  ssize_t alloc_cache_size_diff = 0;
  
  gclist_each_item_safe(b, bn, &bc->atomic.free, block_desc*, gclist) {
    if (b->freecnt == b->totalcnt) { size_diff += bc_free_std_block(b); }
  }
  gclist_each_item_safe(b, bn, &bc->non_atomic.free, block_desc*, gclist) {
    if (b->freecnt == b->totalcnt) { size_diff += bc_free_std_block(b); }
  }
  alloc_cache_size_diff = alloc_cache_flush_freed_pages(bc->bigBlockCache);

#ifdef GC_MP_CNT 
  mp_bc_freed = -size_diff;
  mp_ac_freed = -alloc_cache_size_diff;
#endif
  
  return size_diff + alloc_cache_size_diff;
}

static void block_cache_queue_protect_range(BlockCache* bc, void *p, size_t len, int type, int writeable, void **src_block) {
  switch(type) {
    case MMU_SMALL_GEN1:
#if BC_ASSERTS
      assert(!(find_addr_in_bd(&bc->atomic.full, p, "atomic full") ||
               find_addr_in_bd(&bc->atomic.free, p, "atomic freeblock")));
      assert(find_addr_in_bd(&bc->non_atomic.full, p, "non_atomic full") ||
             find_addr_in_bd(&bc->non_atomic.free, p, "non_atomic freeblock"));
      assert(*src_block != (char*)~0x0);
#endif
      {
        block_desc *b = (block_desc *)*src_block;
        b->in_queue = 1;
      }
      return;
      break;
    default:
#if BC_ASSERTS 
      assert(*src_block == (char*)~0x0);
#endif
      page_range_add(bc->page_range, p, len, writeable);
      return;
      break;
  }
}

static void block_cache_flush_protect_ranges(BlockCache* bc, int writeable) {
  block_group *bg = &bc->non_atomic;
  block_desc *b;
  gclist_each_item(b, &bg->full, block_desc*, gclist) {
    if (b->in_queue) {
      b->in_queue = 0;
      page_range_add(bc->page_range, b->block, b->size, writeable);
    }
  }
  gclist_each_item(b, &bg->free, block_desc*, gclist) {
    if (b->in_queue) {
      b->in_queue = 0;
      page_range_add(bc->page_range, b->block, b->size, writeable);
    }
  }

  page_range_flush(bc->page_range, writeable);
}

#if BC_ASSERTS
static int block_cache_chain_stat(GCList *head, int *blcnt) {
  block_desc *b;
  int freecnt = 0;
  gclist_each_item(b, head, block_desc*, gclist) {
    pfree_list *fl;
    int lfcnt = 0;
    for (fl = b->free; fl; fl = fl->next) {
      lfcnt++;
    }
    freecnt += lfcnt;
    (*blcnt)++;
  }
  return freecnt;
}
#endif
