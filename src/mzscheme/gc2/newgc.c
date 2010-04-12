/* A new accouting precise GC for MzScheme
   Copyright (C) 2001, 2002 Matthew Flatt and Adam Wick
   All rights reserved.

   Please see full copyright in the documentation
   Search for "FIXME" for known improvement points 

   IF YOU'RE NOT ADAM (AND PROBABLY IF YOU ARE) READ THIS FIRST:

   This is now a hybrid copying/mark-compact collector. The nursery
   (generation 0) is copied into the old generation (generation 1),
   but the old generation compacts. This yields a nice combination
   of performance, scalability and memory efficiency.

   The following page map invariants are required:

   Outside of collection, only pages in the older generation should
   be in the gc->page_maps.

   During the mark phase of collection, only pages which contain
   objects which may be marked should be in the page map. This means
   that during minor collections, only pages in the nursery should
   be in the map.

   During the rest of collection, only pages which contain the past
   locations of moved data should be in the page map. This means only
   the nursery and pages being compacted.
*/

#define MZ_PRECISE_GC /* required for mz includes to work right */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "platforms.h"
#include "../src/schpriv.h"
#include "gc2.h"
#include "gc2_dump.h" 

/* the number of tags to use for tagged objects */
#define NUMBER_OF_TAGS 512

#ifdef SIXTY_FOUR_BIT_INTEGERS
#define PAGEMAP64_LEVEL1_SIZE (1 << 16)
#define PAGEMAP64_LEVEL2_SIZE (1 << 16)
#define PAGEMAP64_LEVEL3_SIZE (1 << (32 - LOG_APAGE_SIZE))
#define PAGEMAP64_LEVEL1_BITS(p) (((unsigned long)(p)) >> 48)
#define PAGEMAP64_LEVEL2_BITS(p) ((((unsigned long)(p)) >> 32) & ((PAGEMAP64_LEVEL2_SIZE) - 1))
#define PAGEMAP64_LEVEL3_BITS(p) ((((unsigned long)(p)) >> LOG_APAGE_SIZE) & ((PAGEMAP64_LEVEL3_SIZE) - 1))
#else
#define PAGEMAP32_SIZE (1 << (32 - LOG_APAGE_SIZE))
#define PAGEMAP32_BITS(x) (NUM(x) >> LOG_APAGE_SIZE)
#endif

#if 0
# define GC_ASSERT(x) assert(x)
#else
# define GC_ASSERT(x) /* empty */
#endif

#if 0
# define GC_LOCK_DEBUG(args) printf(args)
#else
# define GC_LOCK_DEBUG(args) /* empty */
#endif

/* the page type constants */
enum {
  PAGE_TAGGED   = 0,
  PAGE_ATOMIC   = 1,
  PAGE_ARRAY    = 2,
  PAGE_TARRAY   = 3,
  PAGE_XTAGGED  = 4,
  PAGE_BIG      = 5,
  /* the number of page types. */
  PAGE_TYPES    = 6,
};

enum {
  SIZE_CLASS_SMALL_PAGE      = 0,
  SIZE_CLASS_MED_PAGE        = 1,
  SIZE_CLASS_BIG_PAGE        = 2,
  SIZE_CLASS_BIG_PAGE_MARKED = 3,
};

static const char *type_name[PAGE_TYPES] = { 
  "tagged", 
  "atomic", 
  "array",
  "tagged array", 
  "xtagged",
  "big" 
};


#include "newgc.h"

THREAD_LOCAL_DECL(static NewGC *GC_instance);
#define GCTYPE NewGC
#define GC_get_GC() (GC_instance)
#define GC_set_GC(gc) (GC_instance = gc)

#ifdef MZ_USE_PLACES
static NewGC *MASTERGC;
static NewGCMasterInfo *MASTERGCINFO;
inline static int premaster_or_master_gc(NewGC *gc) {
  return (!MASTERGC || gc == MASTERGC);
}
inline static int premaster_or_place_gc(NewGC *gc) {
  return (!MASTERGC || gc != MASTERGC);
}
inline static int postmaster_and_master_gc(NewGC *gc) {
  return (MASTERGC && gc == MASTERGC);
}
inline static int postmaster_and_place_gc(NewGC *gc) {
  return (MASTERGC && gc != MASTERGC);
}
static void master_collect_initiate();
#endif

#if defined(MZ_USE_PLACES)
/*
# define DEBUG_GC_PAGES
# define MASTER_ALLOC_DEBUG
# define KILLING_DEBUG
*/
#endif

#if defined(MZ_USE_PLACES) && defined(DEBUG_GC_PAGES)
inline static size_t real_page_size(mpage* page);

static FILE *GCVERBOSEFH;
static FILE* gcdebugOUT() {
  if (GCVERBOSEFH) { fflush(GCVERBOSEFH); }
  else { GCVERBOSEFH = fopen("GCDEBUGOUT", "w"); }
  return GCVERBOSEFH;
}

static void GCVERBOSEprintf(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(gcdebugOUT(), fmt, ap);
    va_end(ap);
}

static void GCVERBOSEPAGE(const char *msg, mpage* page) {
  NewGC *gc = GC_get_GC();
  if(postmaster_and_master_gc(gc)) {
    GCVERBOSEprintf("%s %p %p %p\n", msg, page, page->addr, (void*)((long)page->addr + real_page_size(page)));
  }
}
# ifdef KILLING_DEBUG
static void killing_debug(NewGC *gc, void *info);
# endif
#else
# define GCVERBOSEPAGE(msg, page) /* EMPTY */
#endif



#include "msgprint.c"

/*****************************************************************************/
/* Collector selection. Change the definitions of these to set or unset the  */
/* particular collector you want.                                            */
/*****************************************************************************/

/* This turns on automatic memory accounting */
/* #define NEWGC_BTC_ACCOUNT */
/* #undef NEWGC_BTC_ACCOUNT */

/* This turns on memory tracing */
/* #define NEWGC_MEMORY_TRACE */

/* This turns on support for heap debugging (FIXME: NOT IMPLEMENTED YET) */
/* #define NEWGC_HEAP_DEBUGGING */

/* This turns on some internal debugging logs. Don't turn this on unless you
   don't care about performance and you're hacking the collector */
/* #define NEWGC_INTERNAL_DEBUGGING */

/* The initial size of generation 0. This will grow and shrink a bit as time
   goes on */
#define GEN0_INITIAL_SIZE (1 * 1024 * 1024)
#define GEN0_SIZE_FACTOR 0.5
#define GEN0_SIZE_ADDITION (512 * 1024)
#define GEN0_MAX_SIZE (32 * 1024 * 1024)
#define GEN0_PAGE_SIZE (1 * 1024 * 1024)

/* This is the log base 2 of the size of one word, given in bytes */
#ifdef SIXTY_FOUR_BIT_INTEGERS
# define LOG_WORD_SIZE 3
#else
# define LOG_WORD_SIZE 2
#endif


/* the size of a page we use for the internal mark stack */
#define STACK_PART_SIZE (1 * 1024 * 1024)


/* # define LOG_APAGE_SIZE ... see gc2_obj.h */
/* These are computed from the previous settings. You shouldn't mess with 
   them */
#define PTR(x) ((void*)(x))
#define PPTR(x) ((void**)(x))
#define NUM(x) ((unsigned long)(x))
#define WORD_SIZE (1 << LOG_WORD_SIZE)
#define WORD_BITS (8 * WORD_SIZE)
#define APAGE_SIZE (1 << LOG_APAGE_SIZE)
#define HALF_PAGE_SIZE (1 << (LOG_APAGE_SIZE - 1))
#define GENERATIONS 1

/* the externals */
void (*GC_out_of_memory)(void);
void (*GC_report_out_of_memory)(void);
void (*GC_mark_xtagged)(void *obj);
void (*GC_fixup_xtagged)(void *obj);

GC_collect_start_callback_Proc GC_set_collect_start_callback(GC_collect_start_callback_Proc func) {
  NewGC *gc = GC_get_GC();
  GC_collect_start_callback_Proc old;
  old = gc->GC_collect_start_callback;
  gc->GC_collect_start_callback = func;
  return old;
}
GC_collect_end_callback_Proc GC_set_collect_end_callback(GC_collect_end_callback_Proc func) {
  NewGC *gc = GC_get_GC();
  GC_collect_end_callback_Proc old;
  old = gc->GC_collect_end_callback;
  gc->GC_collect_end_callback = func;
  return old;
}
void GC_set_collect_inform_callback(void (*func)(int major_gc, long pre_used, long post_used)) {
  NewGC *gc = GC_get_GC();
  gc->GC_collect_inform_callback = func;
}


#include "my_qsort.c"

/*****************************************************************************/
/* OS-Level Memory Management Routines                                       */
/*****************************************************************************/
static void garbage_collect(NewGC*, int, int);

static void out_of_memory()
{
  if (GC_report_out_of_memory)
    GC_report_out_of_memory();
  GCPRINT(GCOUTF, "The system has run out of memory!\n");
  abort();
}

inline static void out_of_memory_gc(NewGC* gc) {
  out_of_memory();
}

static void *ofm_malloc(size_t size) {
  void *ptr = malloc(size);
  if (!ptr) out_of_memory();
  return ptr;
}

static void *ofm_malloc_zero(size_t size) {
  void *ptr;
  ptr = ofm_malloc(size);
  memset(ptr, 0, size);
  return ptr;
}

inline static void check_used_against_max(NewGC *gc, size_t len) 
{
  long delta;

  delta = (len / APAGE_SIZE) + (((len % APAGE_SIZE) == 0) ? 0 : 1);
  gc->used_pages += delta;

  if(gc->in_unsafe_allocation_mode) {
    if(gc->used_pages > gc->max_pages_in_heap)
      gc->unsafe_allocation_abort(gc);
  } else {
    if(gc->used_pages > gc->max_pages_for_use) {
      garbage_collect(gc, 0, 0); /* hopefully this will free enough space */
      if(gc->used_pages > gc->max_pages_for_use) {
        garbage_collect(gc, 1, 0); /* hopefully *this* will free enough space */
        if(gc->used_pages > gc->max_pages_for_use) {
          /* too much memory allocated. 
           * Inform the thunk and then die semi-gracefully */
          if(GC_out_of_memory) {
            gc->used_pages -= delta;
            GC_out_of_memory();
          }
          out_of_memory();
        }
      }
    }
  }
}

#include "page_range.c"

#include "vm.c"

#include "protect_range.c"

static void *malloc_pages(NewGC *gc, size_t len, size_t alignment)
{
  void *ptr;
  check_used_against_max(gc, len);
  ptr = vm_malloc_pages(gc->vm, len, alignment, 0);
  if (!ptr) out_of_memory();
  return ptr;
}

static void *malloc_dirty_pages(NewGC *gc, size_t len, size_t alignment)
{
  void *ptr;
  check_used_against_max(gc, len);
  ptr = vm_malloc_pages(gc->vm, len, alignment, 1);
  if (!ptr) out_of_memory();
  return ptr;
}

static void free_pages(NewGC *gc, void *p, size_t len)
{
  gc->used_pages -= (len / APAGE_SIZE) + (((len % APAGE_SIZE) == 0) ? 0 : 1);
  vm_free_pages(gc->vm, p, len);
}


/*****************************************************************************/
/* Memory Tracing, Part 1                                                    */
/*****************************************************************************/
#ifdef NEWGC_MEMORY_TRACE
# error "memory tracing not implemented in this particular revision \
  please revert to early versions of this collector, and then nag \
Adam (awick@cs.utah.edu) to put this stuff back in"
#endif

int GC_mtrace_new_id(void *f)
{
  return 0;
}

int GC_mtrace_union_current_with(int newval)
{
  return 0;
}

/*****************************************************************************/
/* Page Map Routines                                                         */
/*****************************************************************************/
inline static void free_page_maps(PageMap page_maps1) {
#ifdef SIXTY_FOUR_BIT_INTEGERS
  unsigned long i;
  unsigned long j;
  mpage ***page_maps2;
  mpage **page_maps3;

  for (i=0; i<PAGEMAP64_LEVEL1_SIZE; i++) {
    page_maps2 = page_maps1[i];
    if (page_maps2) {
      for (j=0; j<PAGEMAP64_LEVEL2_SIZE; j++) {
        page_maps3 = page_maps2[j];
        if (page_maps3) {
          free(page_maps3);
        }
      }
      free(page_maps2);
    }
  }
  free(page_maps1);
#else
  free(page_maps1);
#endif
}

/* the page map makes a nice mapping from addresses to pages, allowing
   fairly fast lookup. this is useful. */
inline static void pagemap_set(PageMap page_maps1, void *p, mpage *value) {
#ifdef SIXTY_FOUR_BIT_INTEGERS
  unsigned long pos;
  mpage ***page_maps2;
  mpage **page_maps3;

  pos = PAGEMAP64_LEVEL1_BITS(p);
  page_maps2 = page_maps1[pos];
  if (!page_maps2) {
    page_maps2 = (mpage ***)calloc(PAGEMAP64_LEVEL2_SIZE, sizeof(mpage **));
    page_maps1[pos] = page_maps2;
  }
  pos = PAGEMAP64_LEVEL2_BITS(p);
  page_maps3 = page_maps2[pos];
  if (!page_maps3) {
    page_maps3 = (mpage **)calloc(PAGEMAP64_LEVEL3_SIZE, sizeof(mpage *));
    page_maps2[pos] = page_maps3;
  }
  page_maps3[PAGEMAP64_LEVEL3_BITS(p)] = value;
#else
  page_maps1[PAGEMAP32_BITS(p)] = value;
#endif
}

inline static mpage *pagemap_find_page(PageMap page_maps1, void *p) {
#ifdef SIXTY_FOUR_BIT_INTEGERS
  mpage ***page_maps2;
  mpage **page_maps3;

  page_maps2 = page_maps1[PAGEMAP64_LEVEL1_BITS(p)];
  if (!page_maps2) return NULL;
  page_maps3 = page_maps2[PAGEMAP64_LEVEL2_BITS(p)];
  if (!page_maps3) return NULL;
  return page_maps3[PAGEMAP64_LEVEL3_BITS(p)];
#else
  return page_maps1[PAGEMAP32_BITS(p)];
#endif
}

/* These procedures modify or use the page map. The page map provides us very
   fast mappings from pointers to the page the reside on, if any. The page 
   map itself serves two important purposes:

   Between collections, it maps pointers to write-protected pages, so that 
   the write-barrier can identify what page a write has happened to and
   mark it as potentially containing pointers from gen 1 to gen 0. 

   During collections, it maps pointers to "from" pages. */

/* pagemap_modify_with_size could be optimized more for the 64 bit case
   repeatedly calling pagemap_set for the 64 bit case is not optimal */
inline static void pagemap_modify_with_size(PageMap pagemap, mpage *page, long size, mpage *val) {
  void *p = page->addr;

  while(size > 0) {
    pagemap_set(pagemap, p, val);
    size -= APAGE_SIZE;
    p = (char *)p + APAGE_SIZE;
  }
}

inline static void pagemap_modify(PageMap pagemap, mpage *page, mpage *val) {
  long size = (page->size_class > 1) ? page->size : APAGE_SIZE;
  pagemap_modify_with_size(pagemap, page, size, val);
}

inline static void pagemap_add(PageMap pagemap, mpage *page)
{
  pagemap_modify(pagemap, page, page);
}

inline static void pagemap_add_with_size(PageMap pagemap, mpage *page, long size)
{
  pagemap_modify_with_size(pagemap, page, size, page);
}

inline static void pagemap_remove(PageMap pagemap, mpage *page)
{
  pagemap_modify(pagemap, page, NULL);
}

inline static void pagemap_remove_with_size(PageMap pagemap, mpage *page, long size)
{
  pagemap_modify_with_size(pagemap, page, size, NULL);
}

int GC_is_allocated(void *p)
{
  NewGC *gc = GC_get_GC();
  return !!pagemap_find_page(gc->page_maps, p);
}

#ifdef GC2_PLACES_TESTING
#include "testing.c"
#endif

/*****************************************************************************/
/* Allocation                                                                */
/*****************************************************************************/

/* struct objhead is defined in gc2_obj.h */
/* Make sure alloction starts out double-word aligned. 
   The header on each allocated object is one word, so to make
   the content double-word aligned, we may need a prefix. */
#ifdef GC_ALIGN_SIXTEEN
# ifdef SIXTY_FOUR_BIT_INTEGERS
#  define PREFIX_WSIZE 1
# else
#  define PREFIX_WSIZE 3
# endif
#elif defined(GC_ALIGN_EIGHT) 
# if defined(SIXTY_FOUR_BIT_INTEGERS)
#  define PREFIX_WSIZE 0
# else
#  define PREFIX_WSIZE 1
# endif
#else /* GC_ALIGHT_FOUR or byte aligned */
# define PREFIX_WSIZE 0
#endif
#define PREFIX_SIZE (PREFIX_WSIZE * WORD_SIZE)

#define MED_OBJHEAD(p, bytesize) ((objhead *)(PTR(((((NUM(p) & (APAGE_SIZE - 1)) - PREFIX_SIZE) / bytesize) * bytesize) \
                                                         + (NUM(p) & (~(APAGE_SIZE - 1))) + PREFIX_SIZE)))

/* this is the maximum size of an object that will fit on a page, in words.
   the "- 3" is basically used as a fudge/safety factor, and has no real, 
   important meaning. */
#define MAX_OBJECT_SIZE  (APAGE_SIZE - ((PREFIX_WSIZE + 3) * WORD_SIZE))

#define ASSERT_TAG(tag) GC_ASSERT((tag) >= 0 && (tag) <= NUMBER_OF_TAGS)
#define ASSERT_VALID_OBJPTR(objptr) GC_ASSERT(!((long)(objptr) & (0x3)))

/* Generation 0. Generation 0 is a set of very large pages in a list(gc->gen0.pages),
   plus a set of smaller bigpages in a separate list(gc->gen0.big_pages). 
   The former is purely an optimization, saving us from constantly deallocating 
   and allocating the entire nursery on every GC. The latter is useful because it simplifies
   the allocation process (which is also a speed hack, come to think of it) 

   gc->gen0.pages             is the list of very large nursery pages.
   gc->gen0.curr_alloc_page   is the member of this list we are currently allocating on.
   The size count helps us trigger collection quickly when we're running out of space; see
   the test in allocate_big. 
*/
THREAD_LOCAL_DECL(unsigned long GC_gen0_alloc_page_ptr = 0);
THREAD_LOCAL_DECL(unsigned long GC_gen0_alloc_page_end = 0);

/* miscellaneous variables */
static const char *zero_sized[4]; /* all 0-sized allocs get this */

static size_t round_to_apage_size(size_t sizeb)
{  
  sizeb += APAGE_SIZE - 1;
  sizeb -= sizeb & (APAGE_SIZE - 1);
  return sizeb;
}

static mpage *malloc_mpage()
{
  mpage *page;
  page = ofm_malloc_zero(sizeof(mpage));
  return page;
}

static void free_mpage(mpage *page)
{
  free(page);
}

#ifdef NEWGC_BTC_ACCOUNT
static inline int BTC_single_allocation_limit(NewGC *gc, size_t sizeb);
#endif

/* ALIGN_BYTES_SIZE DOES NOT assume that the argument is already word-aligned. */
/* INSET_WORDS is how many words in a tagged array can be padding, plus one; it
   must also be no more than the minimum size of a tagged element. */
#ifdef GC_ALIGN_SIXTEEN
# ifdef SIXTY_FOUR_BIT_INTEGERS
#  define ALIGN_SIZE(sizew) (((sizew) & 0x1) ? ((sizew) + 1) : (sizew))
#  define ALIGN_BYTES_SIZE(sizeb) (((sizeb) & ((2 * WORD_SIZE) -1)) ? ((sizeb) + ((2 * WORD_SIZE) - ((sizeb) & ((2 * WORD_SIZE) - 1)))) : (sizeb))
#  define INSET_WORDS 1
# else
#  define ALIGN_SIZE(sizew) (((sizew) & 0x3) ? ((sizew) + (4 - ((sizew) & 0x3))) : (sizew))
#  define ALIGN_BYTES_SIZE(sizeb) (((sizeb) & ((4 * WORD_SIZE) - 1)) ? ((sizeb) + ((4 * WORD_SIZE) - ((sizeb) & ((4 * WORD_SIZE) - 1)))) : (sizeb))
#  define INSET_WORDS 3
# endif
#else
# ifdef GC_ALIGN_EIGHT
#  ifdef SIXTY_FOUR_BIT_INTEGERS
#   define ALIGN_SIZE(sizew) (sizew)
#   define ALIGN_BYTES_SIZE(sizeb) (((sizeb) & (WORD_SIZE -1)) ? ((sizeb) + (WORD_SIZE - ((sizeb) & (WORD_SIZE - 1)))) : (sizeb))
#   define INSET_WORDS 0
#  else
#   define ALIGN_SIZE(sizew) (((sizew) & 0x1) ? ((sizew) + 1) : (sizew))
#   define ALIGN_BYTES_SIZE(sizeb) (((sizeb) & ((2 * WORD_SIZE) -1)) ? ((sizeb) + ((2 * WORD_SIZE) - ((sizeb) & ((2 * WORD_SIZE) - 1)))) : (sizeb))
#   define INSET_WORDS 1
#  endif
# else
#  define ALIGN_SIZE(sizew) (sizew)
#  define ALIGN_BYTES_SIZE(sizeb) (((sizeb) & (3)) ? ((sizeb) + (4 - ((sizeb) & (3)))) : (sizeb))
#  define INSET_WORDS 0
# endif
#endif

#define COMPUTE_ALLOC_SIZE_FOR_OBJECT_SIZE(s) (ALIGN_BYTES_SIZE((s) + OBJHEAD_SIZE))
#define COMPUTE_ALLOC_SIZE_FOR_BIG_PAGE_SIZE(s) (ALIGN_BYTES_SIZE((s) + OBJHEAD_SIZE + PREFIX_SIZE))
#define BIG_PAGE_TO_OBJECT(big_page) ((void *) (((char *)((big_page)->addr)) + OBJHEAD_SIZE + PREFIX_SIZE))
#define BIG_PAGE_TO_OBJHEAD(big_page) ((objhead*) (((char *)((big_page)->addr)) + PREFIX_SIZE))
#define PAGE_TO_OBJHEAD(page) ((objhead*) (((char *)((page)->addr)) + PREFIX_SIZE))
#define PAGE_START_VSS(page) ((void**) (((char *)((page)->addr)) + PREFIX_SIZE))
#define PAGE_END_VSS(page) ((void**) (((char *)((page)->addr)) + ((page)->size)))
#define MED_OBJHEAD_TO_OBJECT(ptr, page_size) ((void*) (((char *)MED_OBJHEAD((ptr), (page_size))) + OBJHEAD_SIZE));

static inline void* TAG_AS_BIG_PAGE_PTR(void *p) {
  return ((void *)(((unsigned long) p)|1));
}

static inline int IS_BIG_PAGE_PTR(void *p) {
  return (((unsigned long) p) & ((unsigned long) 1));
}

static inline void* REMOVE_BIG_PAGE_PTR_TAG(void *p) {
  return ((void *)((~((unsigned long) 1)) & ((unsigned long) p)));
}

void GC_check_master_gc_request() {
#ifdef MZ_USE_PLACES 
  if (MASTERGC && MASTERGC->major_places_gc == 1) {
    GC_gcollect();
  }
#endif
}

static inline void gc_if_needed_account_alloc_size(NewGC *gc, size_t allocate_size) {
  if((gc->gen0.current_size + allocate_size) >= gc->gen0.max_size) {
#ifdef MZ_USE_PLACES 
    if (postmaster_and_master_gc(gc)) {
      master_collect_initiate();
    }
    else {
#endif
    if (!gc->dumping_avoid_collection)
      garbage_collect(gc, 0, 0);
#ifdef MZ_USE_PLACES 
    }
#endif
  }
  gc->gen0.current_size += allocate_size;
}

/* the core allocation functions */
static void *allocate_big(const size_t request_size_bytes, int type)
{
  NewGC *gc = GC_get_GC();
  mpage *bpage;
  size_t allocate_size;
  void *addr;

#ifdef NEWGC_BTC_ACCOUNT
  if(GC_out_of_memory) {
#ifdef MZ_USE_PLACES 
  if (premaster_or_place_gc(gc)) {
#endif
    if (BTC_single_allocation_limit(gc, request_size_bytes)) {
      /* We're allowed to fail. Check for allocations that exceed a single-time
         limit. Otherwise, the limit doesn't work as intended, because
         a program can allocate a large block that nearly exhausts memory,
         and then a subsequent allocation can fail. As long as the limit
         is much smaller than the actual available memory, and as long as
         GC_out_of_memory protects any user-requested allocation whose size
         is independent of any existing object, then we can enforce the limit. */
      GC_out_of_memory();
    }
#ifdef MZ_USE_PLACES 
  }
#endif
  }
#endif

  /* the actual size of this is the size, ceilinged to the next largest word,
     plus one word for the object header.
     This last serves many purposes, including making sure the object is 
     aligned for Sparcs. */
  allocate_size = COMPUTE_ALLOC_SIZE_FOR_BIG_PAGE_SIZE(request_size_bytes);

  gc_if_needed_account_alloc_size(gc, allocate_size);

  /* The following allocations may fail and escape if GC_out_of_memory is set.
     We not only need APAGE_SIZE alignment, we 
     need everything consisently mapped within an APAGE_SIZE
     segment. So round up. */
  if (type == PAGE_ATOMIC)
    addr = malloc_dirty_pages(gc, round_to_apage_size(allocate_size), APAGE_SIZE);
  else
    addr = malloc_pages(gc, round_to_apage_size(allocate_size), APAGE_SIZE);

  bpage = malloc_mpage();
  bpage->addr = addr;
  bpage->size = allocate_size;
  bpage->size_class = 2;
  bpage->page_type = type;
  GCVERBOSEPAGE("NEW BIG PAGE", bpage);

  /* push new bpage onto GC->gen0.big_pages */
  bpage->next = gc->gen0.big_pages;
  if(bpage->next) bpage->next->prev = bpage;
  gc->gen0.big_pages = bpage;
  pagemap_add(gc->page_maps, bpage);

  {
    void * objptr = BIG_PAGE_TO_OBJECT(bpage);
    ASSERT_VALID_OBJPTR(objptr);
    return objptr;
  }
}

inline static mpage *create_new_medium_page(NewGC *gc, const int sz, const int pos) {
  mpage *page;
  int n;

  page = malloc_mpage();
  page->addr = malloc_pages(gc, APAGE_SIZE, APAGE_SIZE);
  page->size = sz;
  page->size_class = 1;
  page->page_type = PAGE_BIG;
  page->previous_size = PREFIX_SIZE;
  page->live_size = sz;
  GCVERBOSEPAGE("NEW MED PAGE", page);

  for (n = page->previous_size; ((n + sz) <= APAGE_SIZE); n += sz) {
    objhead *info = (objhead *)PTR(NUM(page->addr) + n);
    info->dead = 1;
    info->size = gcBYTES_TO_WORDS(sz);
  }

  /* push page onto linked list */
  page->next = gc->med_pages[pos];
  if (page->next)
    page->next->prev = page;
  gc->med_pages[pos] = page;
  gc->med_freelist_pages[pos] = page;

  pagemap_add(gc->page_maps, page);
  return page;
}

inline static void *medium_page_realloc_dead_slot(NewGC *gc, const int sz, const int pos, const int type) {
  int n;
  mpage *page;

  for (page = gc->med_freelist_pages[pos]; page; page = gc->med_freelist_pages[pos] = page->prev) {
    for (n = page->previous_size; ((n + sz) <= APAGE_SIZE); n += sz) {
      objhead * info = (objhead *)PTR(NUM(page->addr) + n);
      if (info->dead) {
        void *p;

        page->previous_size = (n + sz);
        page->live_size += sz;

        info->dead = 0;
        info->type = type;
        p = OBJHEAD_TO_OBJPTR(info);
        memset(p, 0, sz - OBJHEAD_SIZE);
        return p;
      }
    }
  }
  return 0;
}
#if defined(linux)
#if defined(MASTER_ALLOC_DEBUG)
#include <execinfo.h>
#include <stdio.h>
#include <stdlib.h>

/* Obtain a backtrace and print it to stdout. */
void print_libc_backtrace (FILE *file)
{
  void *array[100];
  size_t size;
  char **strings;
  size_t i;

  size = backtrace (array, 100);
  strings = backtrace_symbols (array, size);
  for (i = 0; i < size; i++)
    fprintf(file, "%s\n", strings[i]);
  free (strings);
}
#endif
#endif

static void *allocate_medium(const size_t request_size_bytes, const int type)
{
  int sz = 8;
  int pos = 0;

  if (request_size_bytes > HALF_PAGE_SIZE)
    return allocate_big(request_size_bytes, type);
 
  while (sz < request_size_bytes) {
    sz <<= 1;
    pos++;
  }

  sz += WORD_SIZE; /* add trailing word, in case pointer is to end */
  sz += OBJHEAD_SIZE; /* room for objhead */
  sz = ALIGN_BYTES_SIZE(sz);

  {
    NewGC *gc = GC_get_GC();
    void *objptr;

    gc_if_needed_account_alloc_size(gc, sz);

    objptr = medium_page_realloc_dead_slot(gc, sz, pos, type);
    if (!objptr) {
      mpage *page;
      objhead *info;

      page = create_new_medium_page(gc, sz, pos);
      info = (objhead *)PTR(NUM(page->addr) + page->previous_size);

      info->dead = 0;
      info->type = type;

      objptr = OBJHEAD_TO_OBJPTR(info);
    }
#if defined(DEBUG_GC_PAGES) && defined(MASTER_ALLOC_DEBUG)
  if (postmaster_and_master_gc(gc)) {
    GCVERBOSEprintf("MASTERGC_allocate_medium %zi %i %i %i %i %p\n", request_size_bytes, type, sz, pos, 1 << (pos +3), objptr);
    /* print_libc_backtrace(gcdebugOUT()); */
  }
#endif

    ASSERT_VALID_OBJPTR(objptr);
    return objptr;
  }
}

inline static mpage *gen0_create_new_nursery_mpage(NewGC *gc, const size_t page_size) {
  mpage *newmpage;

  newmpage = malloc_mpage();
  newmpage->addr = malloc_dirty_pages(gc, page_size, APAGE_SIZE);
  newmpage->size_class = 0;
  newmpage->size = PREFIX_SIZE;
  newmpage->previous_size = page_size;
  pagemap_add_with_size(gc->page_maps, newmpage, page_size);
  GCVERBOSEPAGE("NEW gen0", newmpage);

  return newmpage;
}

inline static void gen0_free_nursery_mpage(NewGC *gc, mpage *page, size_t page_size) {
  pagemap_remove_with_size(gc->page_maps, page, page_size);
  free_pages(gc, page->addr, page_size);
  free_mpage(page);
}

/* Needs to be consistent with GC_alloc_alignment(): */
#define THREAD_LOCAL_PAGE_SIZE APAGE_SIZE

unsigned long GC_make_jit_nursery_page(int count) {
  NewGC *gc = GC_get_GC();
  mpage *new_mpage;
  long size = count * THREAD_LOCAL_PAGE_SIZE;

  if((gc->gen0.current_size + size) >= gc->gen0.max_size) {
    if (!gc->dumping_avoid_collection)
      garbage_collect(gc, 0, 0);
  }
  gc->gen0.current_size += size;

  {
    new_mpage = gen0_create_new_nursery_mpage(gc, size);

    /* push page */
    new_mpage->next = gc->thread_local_pages;
    if (new_mpage->next)
      new_mpage->next->prev = new_mpage;
    gc->thread_local_pages = new_mpage;
  }

  if (!new_mpage->size) {
    /* To avoid roundoff problems, the JIT needs the
       result to be not a multiple of THREAD_LOCAL_PAGE_SIZE,
       so add a prefix if alignment didn't force one. */
#if defined(GC_ALIGN_SIXTEEN)
    new_mpage->size = 16;
#elif defined(GC_ALIGN_EIGHT)
    new_mpage->size = 8;
#else
    new_mpage->size = WORD_SIZE;
#endif
  }
  return (NUM(new_mpage->addr) + new_mpage->size);
}

inline static void gen0_free_jit_nursery_page(NewGC *gc, mpage *page) {
  gen0_free_nursery_mpage(gc, page, page->previous_size);
}

inline static mpage *gen0_create_new_mpage(NewGC *gc) {
  return gen0_create_new_nursery_mpage(gc, GEN0_PAGE_SIZE);
}

inline static void gen0_free_mpage(NewGC *gc, mpage *page) {
  gen0_free_nursery_mpage(gc, page, GEN0_PAGE_SIZE);
}

//#define OVERFLOWS_GEN0(ptr) ((ptr) > (NUM(gc->gen0.curr_alloc_page->addr) + GEN0_PAGE_SIZE))
#define OVERFLOWS_GEN0(ptr) ((ptr) > GC_gen0_alloc_page_end)

inline static size_t gen0_size_in_use(NewGC *gc) {
  return (gc->gen0.current_size + ((GC_gen0_alloc_page_ptr - NUM(gc->gen0.curr_alloc_page->addr)) - PREFIX_SIZE));
}

#define BYTES_MULTIPLE_OF_WORD_TO_WORDS(sizeb) ((sizeb) >> gcLOG_WORD_SIZE)

inline static void *allocate(const size_t request_size, const int type)
{
  size_t allocate_size;
  unsigned long newptr;

  if(request_size == 0) return zero_sized;
  
  allocate_size = COMPUTE_ALLOC_SIZE_FOR_OBJECT_SIZE(request_size);
  if(allocate_size > MAX_OBJECT_SIZE)  return allocate_big(request_size, type);

  /* ensure that allocation will fit in a gen0 page */
  newptr = GC_gen0_alloc_page_ptr + allocate_size;
  ASSERT_VALID_OBJPTR(newptr);

  /* master always overflows and uses allocate_medium because master allocations can't move */
  while (OVERFLOWS_GEN0(newptr)) {
    NewGC *gc = GC_get_GC();
#ifdef MZ_USE_PLACES
    if (postmaster_and_master_gc(gc)) {
      return allocate_medium(request_size, type);
    }
#endif
    /* bring page size used up to date */
    gc->gen0.curr_alloc_page->size = GC_gen0_alloc_page_ptr - NUM(gc->gen0.curr_alloc_page->addr);
    gc->gen0.current_size += gc->gen0.curr_alloc_page->size;

    /* try next nursery page if present */
    if(gc->gen0.curr_alloc_page->next) { 
      gc->gen0.curr_alloc_page  = gc->gen0.curr_alloc_page->next;
      GC_gen0_alloc_page_ptr    = NUM(gc->gen0.curr_alloc_page->addr) + gc->gen0.curr_alloc_page->size;
      ASSERT_VALID_OBJPTR(GC_gen0_alloc_page_ptr);
      GC_gen0_alloc_page_end    = NUM(gc->gen0.curr_alloc_page->addr) + GEN0_PAGE_SIZE;
    }
    /* WARNING: tries to avoid a collection but
     * malloc_pages can cause a collection due to check_used_against_max */
    else if (gc->dumping_avoid_collection) {
      mpage *new_mpage = gen0_create_new_mpage(gc);

      /* push page */
      new_mpage->next = gc->gen0.curr_alloc_page;
      new_mpage->next->prev = new_mpage;

      gc->gen0.curr_alloc_page  = new_mpage;
      GC_gen0_alloc_page_ptr    = NUM(new_mpage->addr);
      ASSERT_VALID_OBJPTR(GC_gen0_alloc_page_ptr);
      GC_gen0_alloc_page_end    = NUM(new_mpage->addr) + GEN0_PAGE_SIZE;
    }
    else {
#ifdef INSTRUMENT_PRIMITIVES 
      LOG_PRIM_START(((void*)garbage_collect));			
#endif
      
      garbage_collect(gc, 0, 0);

#ifdef INSTRUMENT_PRIMITIVES 
      LOG_PRIM_END(((void*)garbage_collect));
#endif
    }
    newptr = GC_gen0_alloc_page_ptr + allocate_size;
    ASSERT_VALID_OBJPTR(newptr);
  } 

  /* actual Allocation */
  {
    objhead *info = (objhead *)PTR(GC_gen0_alloc_page_ptr);

    GC_gen0_alloc_page_ptr = newptr;

    if (type == PAGE_ATOMIC)
      memset(info, 0, sizeof(objhead)); /* init objhead */
    else
      bzero(info, allocate_size);

    info->type = type;
    info->size = BYTES_MULTIPLE_OF_WORD_TO_WORDS(allocate_size); /* ALIGN_BYTES_SIZE bumbed us up to the next word boundary */
    {
      /* NewGC *gc = GC_get_GC(); */
      void * objptr = OBJHEAD_TO_OBJPTR(info);
      /* GCVERBOSEprintf("ALLOCATE page %p %zi %i %p\n", gc->gen0.curr_alloc_page->addr, request_size, type,  objptr); */
      ASSERT_VALID_OBJPTR(objptr);
      return objptr;
    }
  }
}


inline static void *fast_malloc_one_small_tagged(size_t request_size, int dirty)
{
  unsigned long newptr;
  const size_t allocate_size = COMPUTE_ALLOC_SIZE_FOR_OBJECT_SIZE(request_size);

  newptr = GC_gen0_alloc_page_ptr + allocate_size;
  ASSERT_VALID_OBJPTR(newptr);

  if(OVERFLOWS_GEN0(newptr)) {
    return GC_malloc_one_tagged(request_size);
  } else {
    objhead *info = (objhead *)PTR(GC_gen0_alloc_page_ptr);

    GC_gen0_alloc_page_ptr = newptr;

    if (dirty)
      memset(info, 0, sizeof(objhead)); /* init objhead */
    else
      bzero(info, allocate_size);

    info->size = BYTES_MULTIPLE_OF_WORD_TO_WORDS(allocate_size); /* ALIGN_BYTES_SIZE bumbed us up to the next word boundary */

    {
      void * objptr = OBJHEAD_TO_OBJPTR(info);
      ASSERT_VALID_OBJPTR(objptr);
      return objptr;
    }
  }
}

#define PAIR_SIZE_IN_BYTES ALIGN_BYTES_SIZE(sizeof(Scheme_Simple_Object) + OBJHEAD_SIZE)

void *GC_malloc_pair(void *car, void *cdr)
{
  unsigned long newptr;
  void *pair;
  const size_t allocate_size = PAIR_SIZE_IN_BYTES;

  newptr = GC_gen0_alloc_page_ptr + allocate_size;
  ASSERT_VALID_OBJPTR(newptr);

  if(OVERFLOWS_GEN0(newptr)) {
    NewGC *gc = GC_get_GC();
    gc->park[0] = car;
    gc->park[1] = cdr;
    pair = GC_malloc_one_tagged(sizeof(Scheme_Simple_Object));
    car = gc->park[0];
    cdr = gc->park[1];
    gc->park[0] = NULL;
    gc->park[1] = NULL;
  }
  else {
    objhead *info = (objhead *) PTR(GC_gen0_alloc_page_ptr);
    GC_gen0_alloc_page_ptr = newptr;

    memset(info, 0, sizeof(objhead)); /* init objhead */

    /* info->type = type; */ /* We know that the type field is already 0 */
    info->size = BYTES_MULTIPLE_OF_WORD_TO_WORDS(allocate_size); /* ALIGN_BYTES_SIZE bumbed us up to the next word boundary */

    pair = OBJHEAD_TO_OBJPTR(info);
    ASSERT_VALID_OBJPTR(pair);
  }
  
  /* initialize pair */
  {
    Scheme_Simple_Object *obj = (Scheme_Simple_Object *) pair;
    obj->iso.so.type = scheme_pair_type;
    obj->iso.so.keyex = 0; /* init first word of SchemeObject to 0 */
    obj->u.pair_val.car = car;
    obj->u.pair_val.cdr = cdr;
  }

  return pair;
}

/* the allocation mechanism we present to the outside world */
void *GC_malloc(size_t s)                         { return allocate(s, PAGE_ARRAY); }
void *GC_malloc_one_tagged(size_t s)              { return allocate(s, PAGE_TAGGED); }
void *GC_malloc_one_xtagged(size_t s)             { return allocate(s, PAGE_XTAGGED); }
void *GC_malloc_array_tagged(size_t s)            { return allocate(s, PAGE_TARRAY); }
void *GC_malloc_atomic(size_t s)                  { return allocate(s, PAGE_ATOMIC); }
void *GC_malloc_atomic_uncollectable(size_t s)    { return ofm_malloc_zero(s); }
void *GC_malloc_allow_interior(size_t s)          { return allocate_medium(s, PAGE_ARRAY); }
void *GC_malloc_atomic_allow_interior(size_t s)   { return allocate_big(s, PAGE_ATOMIC); }
void *GC_malloc_tagged_allow_interior(size_t s)   { return allocate_medium(s, PAGE_TAGGED); }
void *GC_malloc_one_small_dirty_tagged(size_t s)  { return fast_malloc_one_small_tagged(s, 1); }
void *GC_malloc_one_small_tagged(size_t s)        { return fast_malloc_one_small_tagged(s, 0); }
void GC_free(void *p) {}

long GC_compute_alloc_size(long sizeb)
{
  return COMPUTE_ALLOC_SIZE_FOR_OBJECT_SIZE(sizeb);
}

long GC_initial_word(int request_size)
{
  long w = 0;
  objhead info;

  const size_t allocate_size = COMPUTE_ALLOC_SIZE_FOR_OBJECT_SIZE(request_size);

  memset(&info, 0, sizeof(objhead));

  info.size = BYTES_MULTIPLE_OF_WORD_TO_WORDS(allocate_size); /* ALIGN_BYTES_SIZE bumped us up to the next word boundary */
  memcpy(&w, &info, sizeof(objhead));

  return w;
}

long GC_alloc_alignment()
{
  return APAGE_SIZE;
}

long GC_malloc_stays_put_threshold() { return MAX_OBJECT_SIZE; }

/* this function resizes generation 0 to the closest it can get (erring high)
   to the size we've computed as ideal */
inline static void resize_gen0(NewGC *gc, unsigned long new_size)
{

  mpage *work = gc->gen0.pages;
  mpage *prev = NULL;
  unsigned long alloced_size = 0;


  /* first, make sure the big pages pointer is clean */
  GC_ASSERT(gc->gen0.big_pages == NULL);

  /* reset any parts of gen0 we're keeping */
  while(work && (alloced_size < new_size)) {
    alloced_size += GEN0_PAGE_SIZE;
    work->size = PREFIX_SIZE;
    prev = work;
    work = work->next;
  }

  /* if we're short, add more */
  while(alloced_size < new_size) {
    mpage *newpage = gen0_create_new_mpage(gc);

    if(prev)
      prev->next = newpage;
    else gc->gen0.pages = newpage;
    prev = newpage;

    alloced_size += GEN0_PAGE_SIZE;
  }

  /* deallocate any parts left over */
  if (work) {
    prev->next = NULL;

    /* remove the excess pages */
    while(work) {
      mpage *next = work->next;
      gen0_free_mpage(gc, work);
      work = next;
    }
  }

  /* we're going to allocate onto the first page now */
  gc->gen0.curr_alloc_page = gc->gen0.pages;
  GC_gen0_alloc_page_ptr = NUM(gc->gen0.curr_alloc_page->addr) + gc->gen0.curr_alloc_page->size;
  ASSERT_VALID_OBJPTR(GC_gen0_alloc_page_ptr);
  GC_gen0_alloc_page_end = NUM(gc->gen0.curr_alloc_page->addr) + GEN0_PAGE_SIZE;

  /* set the two size variables */
  gc->gen0.max_size = alloced_size;
  gc->gen0.current_size = 0;

  {
    mpage *work = gc->thread_local_pages;
    while(work) {
      mpage *next = work->next;
      gen0_free_jit_nursery_page(gc, work);
      work = next;
    }

    gc->thread_local_pages = NULL;
  }
}

#ifdef MZ_USE_PLACES
inline static void master_set_max_size(NewGC *gc)
{
  gc->gen0.max_size = gc->gen0.current_size + GEN0_INITIAL_SIZE;
}
#endif

inline static void reset_nursery(NewGC *gc)
{
  unsigned long new_gen0_size; 
  new_gen0_size = NUM((GEN0_SIZE_FACTOR * (float)gc->memory_in_use) + GEN0_SIZE_ADDITION);
  if(new_gen0_size > GEN0_MAX_SIZE)
    new_gen0_size = GEN0_MAX_SIZE;

  resize_gen0(gc, new_gen0_size);
}

/* This procedure fundamentally returns true if a pointer is marked, and
   false if it isn't. This function assumes that you're talking, at this
   point, purely about the mark field of the object. It ignores things like
   the object not being one of our GC heap objects, being in a higher gen
   than we're collecting, not being a pointer at all, etc. */
inline static int marked(NewGC *gc, void *p)
{
  mpage *page;

  if(!p) return 0;
  if(!(page = pagemap_find_page(gc->page_maps, p))) return 1;
  switch(page->size_class) {
    case SIZE_CLASS_BIG_PAGE_MARKED:
      return 1;
    case SIZE_CLASS_SMALL_PAGE:
      if (page->generation) {
        if((NUM(page->addr) + page->previous_size) > NUM(p)) 
          return 1;
      }
      /* else FALLTHROUGH */
    case SIZE_CLASS_MED_PAGE: /* FALLTHROUGH */
    case SIZE_CLASS_BIG_PAGE:
      return OBJPTR_TO_OBJHEAD(p)->mark;
      break;
    default:
      fprintf(stderr, "ABORTING! INVALID SIZE_CLASS %i\n", page->size_class);
      exit(EXIT_FAILURE);
  }
}

/*****************************************************************************/
/* Internal Debugging Routines                                               */
/*****************************************************************************/
#ifdef NEWGC_INTERNAL_DEBUGGING
static FILE *dump;
static int collections = 0;

static void init_debug_file(void) 
{
  /*
    char filename_buf[20];
    snprintf(filename_buf, 20, "gclog%d%d", (collections / 10), (collections % 10));
    dump = fopen(filename_buf, "a");
    collections += 1;
  */

  char *filename = ofm_malloc(8 * sizeof(char));

  filename[0] = 'g'; filename[1] = 'c'; filename[2] = 'l';
  filename[3] = 'o'; filename[4] = 'g';
  filename[5] = '0' + (collections / 10);
  filename[6] = '0' + (collections % 10);
  filename[7] = 0;

  dump = fopen(filename, "a");
  collections += 1;
}

static void close_debug_file(void)
{
  fclose(dump);
}

static void dump_region(void **start, void **end)
{
  while(start < end) {
    fprintf(dump, "%.8lx: %.8lx %.8lx %.8lx %.8lx %.8lx %.8lx %.8lx %.8lx\n", 
            NUM(start), NUM(*start), NUM(*(start + 1)), NUM(*(start + 2)),
            NUM(*(start + 3)), NUM(*(start + 4)), NUM(*(start + 5)), 
            NUM(*(start + 6)), NUM(*(start + 7)));
    start += 8;
  }
  fprintf(dump, "\n\n");
}

static void dump_heap(NewGC *gc)
{
  mpage *page;
  short i;

  if(collections >= 0) {
    for(page = gc->gen0.pages; page; page = page->next) {
      fprintf(dump, "Generation 0 Page (%p:%p - %p, size %i):\n", 
              page, page->addr, PTR(NUM(page->addr) + GEN0_PAGE_SIZE), page->size);
      dump_region(PAGE_START_VSS(page), PAGE_END_VSS(page));
    }
    for(page = gc->gen0.big_pages; page; page = page->next) {
      fprintf(dump, "Page %p:%p (gen %i, type %i, big %i, back %i, size %i)\n",
              page, page->addr, page->generation, page->page_type, page->big_page,
              page->back_pointers, page->size);
      dump_region(PAGE_START_VSS(page), PAGE_END_VSS(page));
    }
    for(i = 0; i < PAGE_TYPES; i++)
      for(page = gc->gen1_pages[i]; page; page = page->next) {
        fprintf(dump, "Page %p:%p (gen %i, type %i, big %i, back %i, size %i)\n",
                page, page->addr, page->generation, page->page_type, page->big_page,
                page->back_pointers, page->size);
        dump_region(PAGE_START_VSS(page), PAGE_END_VSS(page));
      }
    fprintf(dump, "STACK:\n");
    dump_region((void*)(NUM(&i) & 0xfffffff0), (void*)(get_stack_base() & 0xfffffff0)); 
    fflush(dump);
  }
}
#endif

#ifdef NEWGC_INTERNAL_DEBUGGING
# define INIT_DEBUG_FILE() init_debug_file()
# define CLOSE_DEBUG_FILE() close_debug_file()
# define DUMP_HEAP() dump_heap()
# define DEBUGOUTF dump
# define GCDEBUG(args) { GCPRINT args; GCFLUSHOUT(); }
#else
# define INIT_DEBUG_FILE() /* */
# define CLOSE_DEBUG_FILE() /* */
# define DUMP_HEAP() /* */
# define GCDEBUG(args) /* */
#endif

#define GCWARN(args) { GCPRINT args; GCFLUSHOUT(); }
#define GCERR(args) { GCPRINT args; GCFLUSHOUT(); abort(); }

/*****************************************************************************/
/* Backtrace                                                                 */
/*****************************************************************************/

#if MZ_GC_BACKTRACE

static void backtrace_new_page(NewGC *gc, mpage *page)
{
  /* This is a little wastefull for big pages, because we'll
     only use the first few words: */
  page->backtrace = (void **)malloc_pages(gc, APAGE_SIZE, APAGE_SIZE);
}

# define backtrace_new_page_if_needed(gc, page) if (!page->backtrace) backtrace_new_page(gc, page)

static void free_backtrace(mpage *page)
{
  if (page->backtrace)
    free_pages(GC, page->backtrace, APAGE_SIZE);
}

static void *bt_source;
static int bt_type;

static void set_backtrace_source(void *source, int type)
{
  bt_source = source;
  bt_type = type;
}

static void record_backtrace(mpage *page, void *ptr)
/* ptr is after objhead */
{
  unsigned long delta;

  delta = PPTR(ptr) - PPTR(page->addr);
  page->backtrace[delta - 1] = bt_source;
  ((long *)page->backtrace)[delta] = bt_type;
}

static void copy_backtrace_source(mpage *to_page, void *to_ptr,
                                  mpage *from_page, void *from_ptr)
/* ptrs are at objhead */
{
  unsigned long to_delta, from_delta;

  to_delta = PPTR(to_ptr) - PPTR(to_page->addr);
  from_delta = PPTR(from_ptr) - PPTR(from_page->addr);

  to_page->backtrace[to_delta] = from_page->backtrace[from_delta];
  to_page->backtrace[to_delta+1] = from_page->backtrace[from_delta+1];
}

static void *get_backtrace(mpage *page, void *ptr)
/* ptr is after objhead */
{
  unsigned long delta;

  if (page->size_class) {
    if (page->size_class > 1)
      ptr = BIG_PAGE_TO_OBJECT(page);
    else
      ptr = MED_OBJHEAD_TO_OBJECT(ptr, page->size);
  }

  delta = PPTR(ptr) - PPTR(page->addr);
  return page->backtrace[delta - 1];
}


# define BT_STACK      (PAGE_TYPES + 0)
# define BT_ROOT       (PAGE_TYPES + 1)
# define BT_FINALIZER  (PAGE_TYPES + 2)
# define BT_WEAKLINK   (PAGE_TYPES + 3)
# define BT_IMMOBILE   (PAGE_TYPES + 4)

#else
# define backtrace_new_page(gc, page) /* */
# define backtrace_new_page_if_needed(gc, page) /* */
# define free_backtrace(page) /* */
# define set_backtrace_source(ptr, type) /* */
# define record_backtrace(page, ptr) /* */
# define copy_backtrace_source(to_page, to_ptr, from_page, from_ptr) /* */
#endif

#define two_arg_no_op(a, b) /* */

/*****************************************************************************/
/* Routines dealing with various runtime execution stacks                    */
/*                                                                           */
/* With the exception of the "traverse" macro and resultant simplification,  */
/* this code is entirely lifted from compact.c                               */
/*****************************************************************************/
THREAD_LOCAL_DECL(void **GC_variable_stack);

void **GC_get_variable_stack()
{ 
  return GC_variable_stack;
}

void GC_set_variable_stack(void **p)
{
  GC_variable_stack = p;
}

void GC_set_stack_base(void *base) 
{
  NewGC *gc = GC_get_GC();
  gc->stack_base = (unsigned long)base;
}

unsigned long GC_get_stack_base() 
{
  NewGC *gc = GC_get_GC();
  return gc->stack_base;
}

void GC_set_get_thread_stack_base(unsigned long (*func)(void)) {
  NewGC *gc = GC_get_GC();
  gc->GC_get_thread_stack_base = func;
}

static inline void *get_stack_base(NewGC *gc) {
  if (gc->GC_get_thread_stack_base) return (void*) gc->GC_get_thread_stack_base();
  return (void*) gc->stack_base;
}

#include "stack_comp.c"

#define GC_X_variable_stack GC_mark2_variable_stack
#define gcX2(a, gc) gcMARK2(*a, gc)
#define X_source(stk, p) set_backtrace_source((stk ? stk : p), BT_STACK)
#include "var_stack.c"
#undef GC_X_variable_stack
#undef gcX2
#undef X_source

#define GC_X_variable_stack GC_fixup2_variable_stack
#define gcX2(a, gc) gcFIXUP2(*a, gc)
#define X_source(stk, p) /* */
#include "var_stack.c"
#undef GC_X_variable_stack
#undef gcX2
#undef X_source

void GC_mark_variable_stack(void **var_stack,
                            long delta,
                            void *limit,
                            void *stack_mem)
{
  GC_mark2_variable_stack(var_stack, delta, limit, stack_mem, GC_get_GC());
}

void GC_fixup_variable_stack(void **var_stack,
                             long delta,
                             void *limit,
                             void *stack_mem)
{
  GC_fixup2_variable_stack(var_stack, delta, limit, stack_mem, GC_get_GC());
}

/*****************************************************************************/
/* Routines for root sets                                                    */
/*****************************************************************************/

#include "roots.c"

#define traverse_roots(gcMUCK, set_bt_src) {    \
    unsigned long j;                            \
    Roots *roots = &gc->roots;                  \
    if(roots->roots) {                          \
      sort_and_merge_roots(roots);              \
      for(j = 0; j < roots->count; j += 2) {    \
        void **start = (void**)roots->roots[j]; \
        void **end = (void**)roots->roots[j+1]; \
        while(start < end) {                    \
          set_bt_src(start, BT_ROOT);           \
          gcMUCK(*start++);                     \
        }                                       \
      }                                         \
    }                                           \
  }

inline static void mark_roots(NewGC *gc) 
{
  traverse_roots(gcMARK, set_backtrace_source);
}

inline static void repair_roots(NewGC *gc)
{
  traverse_roots(gcFIXUP, two_arg_no_op);
}

#include "immobile_boxes.c"

/*****************************************************************************/
/* finalizers                                                                */
/*****************************************************************************/

static int is_finalizable_page(NewGC *gc, void *p)
{
  return !!pagemap_find_page(gc->page_maps, p);
}

#include "fnls.c"

inline static void mark_finalizer_structs(NewGC *gc)
{
  Fnl *fnl;

  for(fnl = GC_resolve(gc->finalizers); fnl; fnl = GC_resolve(fnl->next)) { 
    set_backtrace_source(fnl, BT_FINALIZER);
    gcMARK2(fnl->data, gc); 
    set_backtrace_source(&gc->finalizers, BT_ROOT);
    gcMARK2(fnl, gc);
  }
  for(fnl = gc->run_queue; fnl; fnl = fnl->next) {
    set_backtrace_source(fnl, BT_FINALIZER);
    gcMARK2(fnl->data, gc);
    gcMARK2(fnl->p, gc);
    set_backtrace_source(&gc->run_queue, BT_ROOT);
    gcMARK2(fnl, gc);
  }
}  

inline static void repair_finalizer_structs(NewGC *gc)
{
  Fnl *fnl;

  /* repair the base parts of the list */
  gcFIXUP2(gc->finalizers, gc); gcFIXUP2(gc->run_queue, gc);
  /* then repair the stuff inside them */
  for(fnl = gc->finalizers; fnl; fnl = fnl->next) {
    gcFIXUP2(fnl->data, gc);
    gcFIXUP2(fnl->p, gc);
    gcFIXUP2(fnl->next, gc);
  }
  for(fnl = gc->run_queue; fnl; fnl = fnl->next) {
    gcFIXUP2(fnl->data, gc);
    gcFIXUP2(fnl->p, gc);
    gcFIXUP2(fnl->next, gc);
  }
}

inline static void check_finalizers(NewGC *gc, int level)
{
  Fnl *work = GC_resolve(gc->finalizers);
  Fnl *prev = NULL;

  GCDEBUG((DEBUGOUTF, "CFNL: Checking level %i finalizers\n", level));
  while(work) {
    if((work->eager_level == level) && !marked(gc, work->p)) {
      struct finalizer *next = GC_resolve(work->next);

      GCDEBUG((DEBUGOUTF, 
               "CFNL: Level %i finalizer %p on %p queued for finalization.\n",
               work->eager_level, work, work->p));
      set_backtrace_source(work, BT_FINALIZER);
      gcMARK2(work->p, gc);
      if(prev) prev->next = next;
      if(!prev) gc->finalizers = next;
      if(gc->last_in_queue) gc->last_in_queue = gc->last_in_queue->next = work;
      if(!gc->last_in_queue) gc->run_queue = gc->last_in_queue = work;
      work->next = NULL;
      --gc->num_fnls;

      work = next;
    } else { 
      GCDEBUG((DEBUGOUTF, "CFNL: Not finalizing %p (level %i on %p): %p / %i\n",
               work, work->eager_level, work->p, pagemap_find_page(gc->page_maps, work->p),
               marked(work->p)));
      prev = work; 
      work = GC_resolve(work->next); 
    }
  }
}

inline static void do_ordered_level3(NewGC *gc)
{
  struct finalizer *temp;
  Mark2_Proc *mark_table = gc->mark_table;

  for(temp = GC_resolve(gc->finalizers); temp; temp = GC_resolve(temp->next))
    if(!marked(gc, temp->p)) {
      GCDEBUG((DEBUGOUTF,
               "LVL3: %p is not marked. Marking payload (%p)\n", 
               temp, temp->p));
      set_backtrace_source(temp, BT_FINALIZER);
      if(temp->tagged) mark_table[*(unsigned short*)temp->p](temp->p, gc);
      if(!temp->tagged) GC_mark_xtagged(temp->p);
    }
}

void GC_finalization_weak_ptr(void **p, int offset)
{
  NewGC *gc = GC_get_GC();
  Weak_Finalizer *wfnl;

  gc->park[0] = p; wfnl = GC_malloc_atomic(sizeof(Weak_Finalizer));
  p = gc->park[0]; gc->park[0] = NULL;
  wfnl->p = p; wfnl->offset = offset * sizeof(void*); wfnl->saved = NULL;
  wfnl->next = gc->weak_finalizers; gc->weak_finalizers = wfnl;
}

inline static void mark_weak_finalizer_structs(NewGC *gc)
{
  Weak_Finalizer *work;

  GCDEBUG((DEBUGOUTF, "MARKING WEAK FINALIZERS.\n"));
  for(work = gc->weak_finalizers; work; work = work->next) {
    set_backtrace_source(&gc->weak_finalizers, BT_ROOT);
    gcMARK2(work, gc);
  }
}

inline static void repair_weak_finalizer_structs(NewGC *gc)
{
  Weak_Finalizer *work;
  Weak_Finalizer *prev;

  gcFIXUP2(gc->weak_finalizers, gc);
  work = gc->weak_finalizers; prev = NULL;
  while(work) {
    gcFIXUP2(work->next, gc);
    if(!marked(gc, work->p)) {
      if(prev) prev->next = work->next;
      if(!prev) gc->weak_finalizers = work->next;
      work = GC_resolve(work->next);
    } else {
      gcFIXUP2(work->p, gc);
      prev = work;
      work = work->next;
    }
  }
}

inline static void zero_weak_finalizers(NewGC *gc)
{
  Weak_Finalizer *wfnl;

  for(wfnl = GC_resolve(gc->weak_finalizers); wfnl; wfnl = GC_resolve(wfnl->next)) {
    wfnl->saved = *(void**)(NUM(GC_resolve(wfnl->p)) + wfnl->offset);
    *(void**)(NUM(GC_resolve(wfnl->p)) + wfnl->offset) = NULL;
  }
}

inline static void reset_weak_finalizers(NewGC *gc)
{
  Weak_Finalizer *wfnl;

  for(wfnl = GC_resolve(gc->weak_finalizers); wfnl; wfnl = GC_resolve(wfnl->next)) {
    if(marked(gc, wfnl->p)) {
      set_backtrace_source(wfnl, BT_WEAKLINK);
      gcMARK2(wfnl->saved, gc); 
    }
    *(void**)(NUM(GC_resolve(wfnl->p)) + wfnl->offset) = wfnl->saved;
    wfnl->saved = NULL;
  }
}

/*****************************************************************************/
/* weak boxes and arrays                                                     */
/*****************************************************************************/

#define is_marked(gc, p) marked(gc, p)
#define weak_box_resolve(p) GC_resolve(p)
#include "weak.c"
#undef is_marked
#undef weak_box_resolve

/*****************************************************************************/
/* Internal Stack Routines                                                   */
/*****************************************************************************/

/* This is the code we use to implement the mark stack. We can't, sadly, use
   the standard C stack because we'll blow it; propagation makes for a *very*
   deep stack. So we use this instead. */

#define MARK_STACK_START(ms) ((void **)(void *)&ms[1])
#define MARK_STACK_END(ms) ((void **)((char *)ms + STACK_PART_SIZE))

inline static MarkSegment* mark_stack_create_frame() {
  MarkSegment *mark_frame = (MarkSegment*)ofm_malloc(STACK_PART_SIZE);
  mark_frame->next = NULL;
  mark_frame->top  = MARK_STACK_START(mark_frame);
  return mark_frame;
}

inline static void mark_stack_initialize(NewGC *gc) {
  /* This happens at the very beginning */
  if(!gc->mark_stack) {
    gc->mark_stack = mark_stack_create_frame();
    gc->mark_stack->prev = NULL;
  }
}

inline static void push_ptr(NewGC *gc, void *ptr)
{
  /* This happens during propagation if we go past the end of this MarkSegment*/
  if(gc->mark_stack->top == MARK_STACK_END(gc->mark_stack)) {
    /* test to see if we already have another stack page ready */
    if(gc->mark_stack->next) {
      /* we do, so just use it */
      gc->mark_stack = gc->mark_stack->next;
      gc->mark_stack->top = MARK_STACK_START(gc->mark_stack);
    } else {
      /* we don't, so we need to allocate one */
      gc->mark_stack->next = mark_stack_create_frame();
      gc->mark_stack->next->prev = gc->mark_stack;
      gc->mark_stack = gc->mark_stack->next;
    }
  }

  /* at this point, we're guaranteed to be good to push pointers */
  *(gc->mark_stack->top++) = ptr;
}

inline static int pop_ptr(NewGC *gc, void **ptr)
{
  if(gc->mark_stack->top == MARK_STACK_START(gc->mark_stack)) {
    if(gc->mark_stack->prev) {
      /* if there is a previous page, go to it */
      gc->mark_stack = gc->mark_stack->prev;
    } else {
      /* if there isn't a previous page, then we've hit the bottom of the stack */
      return 0;
    }
  }

  /* if we get here, we're guaranteed to have data */
  *ptr = *(--gc->mark_stack->top);
  return 1;
}

inline static void clear_stack_pages(NewGC *gc)
{
  if(gc->mark_stack) {
    MarkSegment *temp;
    MarkSegment *base;
    int keep = 2;

    /* go to the head of the list */
    for(; gc->mark_stack->prev; gc->mark_stack = gc->mark_stack->prev) {}
    /* then go through and clear them out */
    base = gc->mark_stack;
    for(; gc->mark_stack; gc->mark_stack = temp) {
      temp = gc->mark_stack->next;
      if(keep) { 
        keep--; 
        if (!keep)
          gc->mark_stack->next = NULL;
      } else 
        free(gc->mark_stack);
    }
    gc->mark_stack = base;
    gc->mark_stack->top = MARK_STACK_START(gc->mark_stack);
  }
}

inline static void reset_pointer_stack(NewGC *gc)
{
  /* go to the head of the list */
  for(; gc->mark_stack->prev; gc->mark_stack = gc->mark_stack->prev) {}
  /* reset the stack */
  gc->mark_stack->top = MARK_STACK_START(gc->mark_stack);
}

static inline void propagate_marks_worker(NewGC *gc, Mark2_Proc *mark_table, void *p);

/*****************************************************************************/
/* MEMORY ACCOUNTING                                                         */
/*****************************************************************************/

#ifdef NEWGC_BTC_ACCOUNT
# include "mem_account.c"
#else
# define clean_up_thread_list() /* */
#endif

void GC_register_root_custodian(void *c)
{
#ifdef NEWGC_BTC_ACCOUNT
  BTC_register_root_custodian(c);
#endif
}

int GC_set_account_hook(int type, void *c1, unsigned long b, void *c2)
{
#ifdef NEWGC_BTC_ACCOUNT
  BTC_add_account_hook(type, c1, c2, b); 
  return 1;
#else
  return 0;
#endif
}

void GC_register_thread(void *t, void *c)
{
#ifdef NEWGC_BTC_ACCOUNT
  BTC_register_thread(t, c);
#endif
}
void GC_register_new_thread(void *t, void *c)
{
#ifdef NEWGC_BTC_ACCOUNT
  BTC_register_new_thread(t, c);
#endif
}

int GC_merely_accounting()
{
  NewGC *gc = GC_get_GC();
  return gc->doing_memory_accounting;
}

/*****************************************************************************/
/* administration / initialization                                           */
/*****************************************************************************/

static inline size_t real_page_size(mpage *page) {
  return (page->size_class > 1) ? round_to_apage_size(page->size) : APAGE_SIZE;
}

static int designate_modified_gc(NewGC *gc, void *p)
{
  mpage *page = pagemap_find_page(gc->page_maps, p);

  if (gc->no_further_modifications) {
    GCPRINT(GCOUTF, "Seg fault (internal error during gc) at %p\n", p);
    return 0;
  }

  if(page) {
    if (!page->back_pointers) {
      page->mprotected = 0;
      vm_protect_pages(page->addr, real_page_size(page), 1);
      page->back_pointers = 1;
    }
    /* For a single mutator thread, we shouldn't get here
       (and a `return 1' in the braces above would make more
       sense). With multiple mutators, though, two threads might
       hit the same page at effectively the same time, and only
       the first one of them will handle the signal. */
    return 1;
  } else {
    if (gc->primoridal_gc) {
      return designate_modified_gc(gc->primoridal_gc, p);
    }
    GCPRINT(GCOUTF, "Seg fault (internal error) at %p\n", p);
  }
  return 0;
}

static int designate_modified(void *p) {
  NewGC *gc = GC_get_GC();
  return designate_modified_gc(gc, p);
}


void GC_write_barrier(void *p) 
{
  (void)designate_modified(p);
}

#include "sighand.c"

#ifdef MZ_USE_PLACES
static void NewGCMasterInfo_initialize() {
  MASTERGCINFO = ofm_malloc_zero(sizeof(NewGCMasterInfo));
  mzrt_rwlock_create(&MASTERGCINFO->cangc);
  mzrt_sema_create(&MASTERGCINFO->wait_sema, 0);
}

static void NewGCMasterInfo_cleanup() {
  mzrt_rwlock_destroy(MASTERGCINFO->cangc);
  free(MASTERGCINFO);
  MASTERGCINFO = NULL;
}

static void NewGCMasterInfo_set_have_collected(NewGC *gc) {
  MASTERGCINFO->have_collected[gc->place_id] = 1;
}


/* signals every place to do a full gc at then end of 
   garbage_collect the places will call 
   wait_if_master_in_progress and
   rendezvous for a master gc */
static void master_collect_initiate() {
  if (MASTERGC->major_places_gc == 0) {
    int i = 0;
    int maxid = MASTERGCINFO->next_GC_id;
    MASTERGC->major_places_gc = 1;

    for (i=1; i < maxid; i++) {
      void *signal_fd = MASTERGCINFO->signal_fds[i];
      MASTERGCINFO->have_collected[i] = -1;
      if (signal_fd >= 0 ) { 
        scheme_signal_received_at(signal_fd);
      }
#if defined(DEBUG_GC_PAGES)
      printf("%i SIGNALED BUT NOT COLLECTED\n", i);
      GCVERBOSEprintf("%i SIGNALED BUT NOT COLLECTED\n", i);
#endif
    }
  }
}


static void wait_if_master_in_progress(NewGC *gc) {
  if (MASTERGC->major_places_gc == 1) {
    int last_one_here = 1;

    mzrt_rwlock_wrlock(MASTERGCINFO->cangc);
    GC_LOCK_DEBUG("MGCLOCK GC_switch_to_master_gc\n");
    {
      int i = 0;
      int maxid = MASTERGCINFO->next_GC_id;

      NewGCMasterInfo_set_have_collected(gc);
      for (i=1; i < maxid; i++) {
        int have_collected = MASTERGCINFO->have_collected[i];
        if (have_collected == 1) {
#if defined(DEBUG_GC_PAGES)
            printf("%i READY\n", i);
            GCVERBOSEprintf("%i READY\n", i);
#endif
        }
        else {
#if defined(DEBUG_GC_PAGES)
          printf("%i SIGNALED BUT NOT COLLECTED\n", i);
          GCVERBOSEprintf("%i SIGNALED BUT NOT COLLECTED\n", i);
#endif
          last_one_here = 0; 
        }
      }
    }
    if (last_one_here) {
      int i = 0;
      int maxid = MASTERGCINFO->next_GC_id;
      NewGC *saved_gc;
      
      GC_LOCK_DEBUG("UNMGCLOCK GC_switch_to_master_gc\n");
      mzrt_rwlock_unlock(MASTERGCINFO->cangc);


      saved_gc = GC_switch_to_master_gc();
      {
#if defined(DEBUG_GC_PAGES)
        printf("START MASTER COLLECTION\n");
        GCVERBOSEprintf("START MASTER COLLECTION\n");
#endif
        MASTERGC->major_places_gc = 0;
        garbage_collect(MASTERGC, 1, 0);
#if defined(DEBUG_GC_PAGES)
        printf("END MASTER COLLECTION\n");
        GCVERBOSEprintf("END MASTER COLLECTION\n");
#endif

        /* wake everyone back up */  
        for (i=2; i < maxid; i++) {
          mzrt_sema_post(MASTERGCINFO->wait_sema);
        }
      }
      GC_switch_back_from_master(saved_gc);
    }
    else {
      GC_LOCK_DEBUG("UNMGCLOCK GC_switch_to_master_gc\n");
      mzrt_rwlock_unlock(MASTERGCINFO->cangc);

      /* wait on semaphonre */
      mzrt_sema_wait(MASTERGCINFO->wait_sema);
    }
  }
}

static void NewGCMasterInfo_get_next_id(NewGC *newgc) {
  int newid;
  /* this could just be an atomic op if we had those */
  /* waiting for other threads to finish a possible concurrent GC is not optimal*/
  mzrt_rwlock_wrlock(MASTERGCINFO->cangc);
  newid = MASTERGCINFO->next_GC_id++;
  newgc->place_id = newid;
  /* printf("ALLOCATED GC OID %li\n", newgc->place_id); */
  MASTERGCINFO->have_collected = realloc(MASTERGCINFO->have_collected, sizeof(char) * MASTERGCINFO->next_GC_id);
  MASTERGCINFO->signal_fds = realloc(MASTERGCINFO->signal_fds, sizeof(void*) * MASTERGCINFO->next_GC_id);
  MASTERGCINFO->have_collected[newid] = 0;
  MASTERGCINFO->signal_fds[newid] = (void *)-1;
  mzrt_rwlock_unlock(MASTERGCINFO->cangc);
}

void GC_set_put_external_event_fd(void *fd) {
  NewGC *gc = GC_get_GC();
  mzrt_rwlock_wrlock(MASTERGCINFO->cangc);
  MASTERGCINFO->signal_fds[gc->place_id] = fd;
  mzrt_rwlock_unlock(MASTERGCINFO->cangc);
}
#endif

static void NewGC_initialize(NewGC *newgc, NewGC *parentgc) {
  if (parentgc) {
    newgc->mark_table  = parentgc->mark_table;
    newgc->fixup_table = parentgc->fixup_table;
  }
  else {

#ifdef MZ_USE_PLACES
    NewGCMasterInfo_initialize();
#endif
    newgc->mark_table  = ofm_malloc_zero(NUMBER_OF_TAGS * sizeof (Mark2_Proc)); 
    newgc->fixup_table = ofm_malloc_zero(NUMBER_OF_TAGS * sizeof (Fixup2_Proc)); 
#ifdef NEWGC_BTC_ACCOUNT
    BTC_initialize_mark_table(newgc);
#endif
  }

#ifdef MZ_USE_PLACES
  NewGCMasterInfo_get_next_id(newgc);
#endif

  mark_stack_initialize(newgc);

#ifdef SIXTY_FOUR_BIT_INTEGERS
  newgc->page_maps = ofm_malloc_zero(PAGEMAP64_LEVEL1_SIZE * sizeof (mpage***)); 
#else
  newgc->page_maps = ofm_malloc_zero(PAGEMAP32_SIZE * sizeof (mpage*)); 
#endif

  newgc->vm = vm_create();
  newgc->protect_range = ofm_malloc_zero(sizeof(Page_Range));
  
  newgc->generations_available = 1;
  newgc->last_full_mem_use = (20 * 1024 * 1024);
  newgc->new_btc_mark = 1;
}

/* NOTE This method sets the constructed GC as the new Thread Specific GC. */
static NewGC *init_type_tags_worker(NewGC *parentgc, int count, int pair, int mutable_pair, int weakbox, int ephemeron, int weakarray, int custbox)
{
  NewGC *gc;

  gc = ofm_malloc_zero(sizeof(NewGC));
  /* NOTE sets the constructed GC as the new Thread Specific GC. */
  GC_set_GC(gc);

  gc->weak_box_tag    = weakbox;
  gc->ephemeron_tag   = ephemeron;
  gc->weak_array_tag  = weakarray;
# ifdef NEWGC_BTC_ACCOUNT
  gc->cust_box_tag    = custbox;
# endif

  NewGC_initialize(gc, parentgc);


  /* Our best guess at what the OS will let us allocate: */
  gc->max_pages_in_heap = determine_max_heap_size() / APAGE_SIZE;
  /* Not all of that memory is available for allocating GCable
     objects.  There's the memory used by the stack, code,
     malloc()/free()ed memory, etc., and there's also the
     administrative structures for the GC itself. */
  gc->max_pages_for_use = gc->max_pages_in_heap / 2;

  resize_gen0(gc, GEN0_INITIAL_SIZE);

  if (!parentgc) {
    GC_register_traversers2(gc->weak_box_tag, size_weak_box, mark_weak_box, fixup_weak_box, 0, 0);
    GC_register_traversers2(gc->ephemeron_tag, size_ephemeron, mark_ephemeron, fixup_ephemeron, 0, 0);
    GC_register_traversers2(gc->weak_array_tag, size_weak_array, mark_weak_array, fixup_weak_array, 0, 0);
  }
  initialize_signal_handler(gc);
  GC_add_roots(&gc->park, (char *)&gc->park + sizeof(gc->park) + 1);
  GC_add_roots(&gc->park_save, (char *)&gc->park_save + sizeof(gc->park_save) + 1);

  initialize_protect_page_ranges(gc->protect_range, malloc_dirty_pages(gc, APAGE_SIZE, APAGE_SIZE), APAGE_SIZE);

  return gc;
}

void GC_init_type_tags(int count, int pair, int mutable_pair, int weakbox, int ephemeron, int weakarray, int custbox)
{
  static int initialized = 0;

  if(!initialized) {
    initialized = 1;
    init_type_tags_worker(NULL, count, pair, mutable_pair, weakbox, ephemeron, weakarray, custbox);
  }
  else {
    GCPRINT(GCOUTF, "GC_init_type_tags should only be called once!\n");
    abort();
  }
}

#ifdef MZ_USE_PLACES
void GC_construct_child_gc() {
  NewGC *gc = MASTERGC;
  NewGC *newgc = init_type_tags_worker(gc, 0, 0, 0, gc->weak_box_tag, gc->ephemeron_tag, gc->weak_array_tag, gc->cust_box_tag);
  newgc->primoridal_gc = MASTERGC;
}

static inline void save_globals_to_gc(NewGC *gc) {
  gc->saved_GC_variable_stack       = GC_variable_stack;
  gc->saved_GC_gen0_alloc_page_ptr  = GC_gen0_alloc_page_ptr;
  gc->saved_GC_gen0_alloc_page_end  = GC_gen0_alloc_page_end;
}

static inline void restore_globals_from_gc(NewGC *gc) {
  GC_variable_stack       = gc->saved_GC_variable_stack;
  GC_gen0_alloc_page_ptr  = gc->saved_GC_gen0_alloc_page_ptr;
  GC_gen0_alloc_page_end  = gc->saved_GC_gen0_alloc_page_end;
}

void GC_switch_out_master_gc() {
  static int initialized = 0;

  if(!initialized) {
    NewGC *gc = GC_get_GC();
    initialized = 1;
    garbage_collect(gc, 1, 1);

#ifdef MZ_USE_PLACES
    GC_gen0_alloc_page_ptr = 2;
    GC_gen0_alloc_page_end = 1;
#endif
 
    MASTERGC = gc;
    MASTERGC->dumping_avoid_collection = 1;
    save_globals_to_gc(MASTERGC);
    GC_construct_child_gc();
  }
  else {
    GCPRINT(GCOUTF, "GC_switch_out_master_gc should only be called once!\n");
    abort();
  }
}

/*used in scheme_master_fast_path*/
void *GC_switch_to_master_gc() {
  NewGC *gc = GC_get_GC();
  /* return if MASTERGC hasn't been constructed yet, allow recursive locking */
  if (premaster_or_master_gc(gc)) { return MASTERGC; }

  save_globals_to_gc(gc);

  /*obtain exclusive access to MASTERGC*/
  mzrt_rwlock_wrlock(MASTERGCINFO->cangc);
  GC_LOCK_DEBUG("MGCLOCK GC_switch_to_master_gc\n");

  GC_set_GC(MASTERGC);
  restore_globals_from_gc(MASTERGC);
  return gc;
}

void GC_switch_back_from_master(void *gc) {
  /* return if MASTERGC hasn't been constructed yet, allow recursive locking */
  if (premaster_or_master_gc(gc)) { return; }
  save_globals_to_gc(MASTERGC);

  /*release exclusive access to MASTERGC*/
  GC_LOCK_DEBUG("UNMGCLOCK GC_switch_to_master_gc\n");
  mzrt_rwlock_unlock(MASTERGCINFO->cangc);

  GC_set_GC(gc);
  restore_globals_from_gc(gc);
}


#endif

void GC_gcollect(void)
{
  NewGC *gc = GC_get_GC();
  garbage_collect(gc, 1, 0);
}

static inline int atomic_mark(void *p) { return 0; }
void GC_register_traversers2(short tag, Size2_Proc size, Mark2_Proc mark,
                             Fixup2_Proc fixup, int constant_Size, int atomic)
{
  NewGC *gc = GC_get_GC();

  int mark_tag = tag;

#ifdef NEWGC_BTC_ACCOUNT
  mark_tag = BTC_get_redirect_tag(gc, mark_tag);
#endif

#if MZ_GC_BACKTRACE
  /* Keep tagged objects in tagged space: */
  atomic = 0;
#endif

  gc->mark_table[mark_tag]  = atomic ? (Mark2_Proc)PAGE_ATOMIC : mark;
  gc->fixup_table[tag]      = fixup;
}

void GC_register_traversers(short tag, Size_Proc size, Mark_Proc mark,
                            Fixup_Proc fixup, int constant_Size, int atomic)
{
  GC_register_traversers2(tag, (Size2_Proc)size, (Mark2_Proc)mark,
                          (Fixup2_Proc)fixup, constant_Size, atomic);
}

long GC_get_memory_use(void *o) 
{
  NewGC *gc = GC_get_GC();
#ifdef NEWGC_BTC_ACCOUNT
  if(o) {
    return BTC_get_memory_use(gc, o);
  }
#endif
  return gen0_size_in_use(gc) + gc->memory_in_use;
}

/*****************************************************************************/
/* Garbage collection proper ... and all the mess therein                    */
/*****************************************************************************/

/* We use two mark routines to handle propagation. Why two? The first is the
   one that we export out, and it does a metric crapload of work. The second
   we use internally, and it doesn't do nearly as much. */

/* This is the first mark routine. It's a bit complicated. */
void GC_mark2(const void *const_p, struct NewGC *gc)
{
  mpage *page;
  void *p = (void*)const_p;

  if(!p || (NUM(p) & 0x1)) {
    GCDEBUG((DEBUGOUTF, "Not marking %p (bad ptr)\n", p));
    return;
  }

  if(!(page = pagemap_find_page(gc->page_maps, p))) {
#ifdef MZ_USE_PLACES
    if (!MASTERGC || !MASTERGC->major_places_gc || !(page = pagemap_find_page(MASTERGC->page_maps, p)))
#endif
    {
      GCDEBUG((DEBUGOUTF,"Not marking %p (no page)\n",p));
      return;
    }
  }

  /* toss this over to the BTC mark routine if we're doing accounting */
  if(gc->doing_memory_accounting) { 
#ifdef NEWGC_BTC_ACCOUNT
    BTC_memory_account_mark(gc, page, p); return; 
#endif
  }

  if(page->size_class) {
    if(page->size_class > 1) {
      /* This is a bigpage. The first thing we do is see if its been marked
         previously */
      if(page->size_class != 2) {
        GCDEBUG((DEBUGOUTF, "Not marking %p on big %p (already marked)\n", p, page));
        return;
      }
      /* in this case, it has not. So we want to mark it, first off. */
      page->size_class = 3;

      /* if this is in the nursery, we want to move it out of the nursery */
      if(!page->generation) {
        page->generation = 1;

        /* remove page */
        if(page->prev) page->prev->next = page->next; else
          gc->gen0.big_pages = page->next;
        if(page->next) page->next->prev = page->prev;
        
        GCVERBOSEPAGE("MOVING BIG PAGE TO GEN1", page);

        backtrace_new_page(gc, page);

        /* add to gen1 */
        page->next = gc->gen1_pages[PAGE_BIG]; 
        page->prev = NULL;
        if(page->next) page->next->prev = page;
        gc->gen1_pages[PAGE_BIG] = page;

        /* if we're doing memory accounting, then we need to make sure the
           btc_mark is right */
#ifdef NEWGC_BTC_ACCOUNT
        BTC_set_btc_mark(gc, BIG_PAGE_TO_OBJHEAD(page));
#endif
      }

      page->marked_on = 1;
      record_backtrace(page, BIG_PAGE_TO_OBJECT(page));
      GCDEBUG((DEBUGOUTF, "Marking %p on big page %p\n", p, page));
      /* Finally, we want to add this to our mark queue, so we can 
         propagate its pointers */
      push_ptr(gc, TAG_AS_BIG_PAGE_PTR(p));
    } else {
      /* A medium page. */
      objhead *info = MED_OBJHEAD(p, page->size);
      if (info->mark) {
        GCDEBUG((DEBUGOUTF,"Not marking %p (already marked)\n", p));
        return;
      }
      info->mark = 1;
      page->marked_on = 1;
      p = OBJHEAD_TO_OBJPTR(info);
      backtrace_new_page_if_needed(gc, page);
      record_backtrace(page, p);
      push_ptr(gc, p);
    }
  } else {
    objhead *ohead = OBJPTR_TO_OBJHEAD(p);

    if(ohead->mark) {
      GCDEBUG((DEBUGOUTF,"Not marking %p (already marked)\n", p));
      return;
    }

    /* what we do next depends on whether this is a gen0 or gen1 
       object */
    if(page->generation) {
      /* this is a generation 1 object. This means we are not going
         to move it, we don't have to check to see if it's an atomic
         object masquerading as a tagged object, etc. So all we do
         is add the pointer to the mark queue and note on the page
         that we marked something on it*/
      if((NUM(page->addr) + page->previous_size) <= NUM(p)) {
        GCDEBUG((DEBUGOUTF, "Marking %p (leaving alone)\n", p));
        ohead->mark = 1;
        page->marked_on = 1;
        page->previous_size = PREFIX_SIZE;
        page->live_size += ohead->size;
        record_backtrace(page, p);
        push_ptr(gc, p);
      } else GCDEBUG((DEBUGOUTF, "Not marking %p (it's old; %p / %i)\n",
                      p, page, page->previous_size));
    } else {
      /* this is a generation 0 object. This means that we do have
         to do all of the above. Fun, fun, fun. */
      unsigned short type = ohead->type;
      mpage *work;
      size_t size;
      objhead *newplace;

      /* first check to see if this is an atomic object masquerading
         as a tagged object; if it is, then convert it */
      if(type == PAGE_TAGGED) {
        if((unsigned long)gc->mark_table[*(unsigned short*)p] < PAGE_TYPES)
          type = ohead->type = (int)(unsigned long)gc->mark_table[*(unsigned short*)p];
      }

      /* now set us up for the search for where to put this thing */
      work = gc->gen1_pages[type];
      size = gcWORDS_TO_BYTES(ohead->size);

      /* search for a page with the space to spare */
      if (work && ((work->size + size) >= APAGE_SIZE))
        work = NULL;

      /* now either fetch where we're going to put this object or make
         a new page if we couldn't find a page with space to spare */
      if(work) {
        if (!work->added) {
          pagemap_add(gc->page_maps, work);
          work->added = 1;
        }
        work->marked_on = 1;
        if (work->mprotected) {
          work->mprotected = 0;
          vm_protect_pages(work->addr, APAGE_SIZE, 1);
        }
        newplace = PTR(NUM(work->addr) + work->size);
      } else {
        /* Allocate and prep the page */
        work = malloc_mpage();
        work->addr = malloc_dirty_pages(gc, APAGE_SIZE, APAGE_SIZE);
        work->generation = 1; 
        work->page_type = type;
        work->size = work->previous_size = PREFIX_SIZE;
        work->marked_on = 1;
        backtrace_new_page(gc, work);
        work->next = gc->gen1_pages[type];
        work->prev = NULL;
        if(work->next)
          work->next->prev = work;
        pagemap_add(gc->page_maps, work);
        work->added = 1;
        gc->gen1_pages[type] = work;
        newplace = PAGE_TO_OBJHEAD(work);
      }

      /* update the size */
      work->size += size;
      work->has_new = 1;

      /* transfer the object */
      ohead->mark = 1; /* mark is copied to newplace, too */
      if (size == PAIR_SIZE_IN_BYTES) 
        /* pairs are common, and compiler tends to inline constant-size memcpys */
        memcpy(newplace, ohead, PAIR_SIZE_IN_BYTES);
      else
        memcpy(newplace, ohead, size);
      /* mark the old location as marked and moved, and the new location
         as marked */
      ohead->moved = 1;
      /* if we're doing memory accounting, then we need the btc_mark
         to be set properly */
#ifdef NEWGC_BTC_ACCOUNT
      BTC_set_btc_mark(gc, newplace);
#endif
      
      {
        /* drop the new location of the object into the forwarding space
           and into the mark queue */
        void *newp = OBJHEAD_TO_OBJPTR(newplace);
        /* record why we marked this one (if enabled) */
        record_backtrace(work, newp);
        /* set forwarding pointer */
        GCDEBUG((DEBUGOUTF,"Marking %p (moved to %p on page %p)\n", p, newp, work));
        *(void**)p = newp;
        push_ptr(gc, newp);
      }
    }
  }
}

void GC_mark(const void *const_p)
{
  GC_mark2(const_p, GC_get_GC());
}

/* this is the second mark routine. It's not quite as complicated. */
/* this is what actually does mark propagation */
static inline void propagate_marks_worker(NewGC *gc, Mark2_Proc *mark_table, void *pp)
{
  void **start, **end;
  int alloc_type;
  void *p;

  /* we can assume a lot here -- like it's a valid pointer with a page --
     because we vet bad cases out in GC_mark, above */
  if (IS_BIG_PAGE_PTR(pp)) {
    mpage *page;
    p = REMOVE_BIG_PAGE_PTR_TAG(pp);
    page = pagemap_find_page(gc->page_maps, p);
#ifdef MZ_USE_PLACES
    if (!page && MASTERGC && MASTERGC->major_places_gc) {
      page = pagemap_find_page(MASTERGC->page_maps, p);
    }
#endif
    start = PPTR(BIG_PAGE_TO_OBJECT(page));
    alloc_type = page->page_type;
    end = PAGE_END_VSS(page);
  } else {
    objhead *info;
    p = pp;
    info = OBJPTR_TO_OBJHEAD(p);
    start = p;
    alloc_type = info->type;
    end = PPTR(info) + info->size;
  }

  set_backtrace_source(start, alloc_type);

  switch(alloc_type) {
    case PAGE_TAGGED: 
      {
        const unsigned short tag = *(unsigned short*)start;
        Mark2_Proc markproc;
        ASSERT_TAG(tag);
        markproc = mark_table[tag];
        if(((unsigned long) markproc) >= PAGE_TYPES) {
          GC_ASSERT(markproc);
          markproc(start, gc);
        }
        break;
      }
    case PAGE_ATOMIC: 
      break;
    case PAGE_ARRAY: 
      {
        while(start < end) gcMARK2(*start++, gc); break;
      }
    case PAGE_TARRAY: 
      {
        const unsigned short tag = *(unsigned short *)start;
        ASSERT_TAG(tag);
        end -= INSET_WORDS;
        while(start < end) {
          GC_ASSERT(mark_table[tag]);
          start += mark_table[tag](start, gc);
        }
        break;
      }
    case PAGE_XTAGGED: 
      GC_mark_xtagged(start); break;
  }
}

static void propagate_marks(NewGC *gc) 
{
  void *p;
  Mark2_Proc *mark_table = gc->mark_table;

  while(pop_ptr(gc, &p)) {
    GCDEBUG((DEBUGOUTF, "Popped pointer %p\n", p));
    propagate_marks_worker(gc, mark_table, p);
  }
}

void *GC_resolve(void *p)
{
  NewGC *gc = GC_get_GC();
  mpage *page = pagemap_find_page(gc->page_maps, p);
  objhead *info;

  if(!page || page->size_class)
    return p;

  info = OBJPTR_TO_OBJHEAD(p);
  if(info->mark && info->moved)
    return *(void**)p;
  else 
    return p;
}

void *GC_fixup_self(void *p)
{
  return p;
}

void GC_fixup2(void *pp, struct NewGC *gc)
{
  mpage *page;
  void *p = *(void**)pp;

  if(!p || (NUM(p) & 0x1))
    return;

  if((page = pagemap_find_page(gc->page_maps, p))) {
    objhead *info;

    if(page->size_class) return;
    info = OBJPTR_TO_OBJHEAD(p);
    if(info->mark && info->moved) 
      *(void**)pp = *(void**)p;
    else GCDEBUG((DEBUGOUTF, "Not repairing %p from %p (not moved)\n",p,pp));
  } else GCDEBUG((DEBUGOUTF, "Not repairing %p from %p (no page)\n", p, pp));
}

void GC_fixup(void *pp)
{
  GC_fixup2(pp, GC_get_GC());
}

/*****************************************************************************/
/* memory stats and traces                                                   */
/*****************************************************************************/

#ifdef MZ_GC_BACKTRACE
# define trace_page_t mpage
# define trace_page_type(page) (page)->page_type
static void *trace_pointer_start(mpage *page, void *p) { 
  if (page->size_class) {
    if (page->size_class > 1)
      return BIG_PAGE_TO_OBJECT(page);
    else
      return MED_OBJHEAD_TO_OBJECT(p, page->size);
  } else 
    return p; 
}
# define TRACE_PAGE_TAGGED PAGE_TAGGED
# define TRACE_PAGE_ARRAY PAGE_ARRAY
# define TRACE_PAGE_TAGGED_ARRAY PAGE_TARRAY
# define TRACE_PAGE_ATOMIC PAGE_ATOMIC
# define TRACE_PAGE_XTAGGED PAGE_XTAGGED
# define TRACE_PAGE_MALLOCFREE PAGE_TYPES
# define TRACE_PAGE_BAD PAGE_TYPES
# define trace_page_is_big(page) (page)->size_class
# define trace_backpointer get_backtrace
# include "backtrace.c"
#else
# define reset_object_traces() /* */
# define register_traced_object(p) /* */
# define print_traced_objects(x, y, q, z) /* */
#endif

#define MAX_DUMP_TAG 256

void GC_dump_with_traces(int flags,
                         GC_get_type_name_proc get_type_name,
                         GC_get_xtagged_name_proc get_xtagged_name,
                         GC_for_each_found_proc for_each_found,
                         short trace_for_tag,
                         GC_print_tagged_value_proc print_tagged_value,
                         int path_length_limit)
{
  NewGC *gc = GC_get_GC();
  mpage *page;
  int i;
  static unsigned long counts[MAX_DUMP_TAG], sizes[MAX_DUMP_TAG];

  reset_object_traces();
  if (for_each_found)
    gc->dumping_avoid_collection++;

  /* Traverse tagged pages to count objects: */
  for (i = 0; i < MAX_DUMP_TAG; i++) {
    counts[i] = sizes[i] = 0;
  }
  for (page = gc->gen1_pages[PAGE_TAGGED]; page; page = page->next) {
    void **start = PAGE_START_VSS(page);
    void **end = PAGE_END_VSS(page);

    while(start < end) {
      objhead *info = (objhead *)start;
      if(!info->dead) {
        void *obj_start = OBJHEAD_TO_OBJPTR(start);
        unsigned short tag = *(unsigned short *)obj_start;
        ASSERT_TAG(tag);
        if (tag < MAX_DUMP_TAG) {
          counts[tag]++;
          sizes[tag] += info->size;
        }
        if (tag == trace_for_tag) {
          register_traced_object(obj_start);
          if (for_each_found)
            for_each_found(obj_start);
        }
      }
      start += info->size;
    }
  }
  for (page = gc->gen1_pages[PAGE_BIG]; page; page = page->next) {
    if (page->page_type == PAGE_TAGGED) {
      void **start = PAGE_START_VSS(page);
      void *obj_start = OBJHEAD_TO_OBJPTR(start);
      unsigned short tag = *(unsigned short *)obj_start;
      ASSERT_TAG(tag);
      if (tag < MAX_DUMP_TAG) {
        counts[tag]++;
        sizes[tag] += gcBYTES_TO_WORDS(page->size);
      }
      if ((tag == trace_for_tag)
          || (tag == -trace_for_tag)) {
        register_traced_object(obj_start);
        if (for_each_found)
          for_each_found(obj_start);
      }
    }
  }
  for (i = 0; i < NUM_MED_PAGE_SIZES; i++) {
    for (page = gc->med_pages[i]; page; page = page->next) {
      void **start = PPTR(NUM(page->addr) + PREFIX_SIZE);
      void **end = PPTR(NUM(page->addr) + APAGE_SIZE - page->size);
      
      while(start <= end) {
        objhead *info = (objhead *)start;
        if (!info->dead) {
          if (info->type == PAGE_TAGGED) {
            void *obj_start = OBJHEAD_TO_OBJPTR(start);
            unsigned short tag = *(unsigned short *)obj_start;
            ASSERT_TAG(tag);
            if (tag < MAX_DUMP_TAG) {
              counts[tag]++;
              sizes[tag] += info->size;
            }
            if (tag == trace_for_tag) {
              register_traced_object(obj_start);
              if (for_each_found)
                for_each_found(obj_start);
            }
          }
        }
        start += info->size;
      }
    }
  }

  GCPRINT(GCOUTF, "Begin MzScheme3m\n");
  for (i = 0; i < MAX_DUMP_TAG; i++) {
    if (counts[i]) {
      char *tn, buf[256];
      if (get_type_name)
        tn = get_type_name((Type_Tag)i);
      else
        tn = NULL;
      if (!tn) {
        sprintf(buf, "unknown,%d", i);
        tn = buf;
      }
      GCPRINT(GCOUTF, "  %20.20s: %10ld %10ld\n", tn, counts[i], gcWORDS_TO_BYTES(sizes[i]));
    }
  }
  GCPRINT(GCOUTF, "End MzScheme3m\n");

  GCWARN((GCOUTF, "Generation 0: %lu of %li bytes used\n", (unsigned long) gen0_size_in_use(gc), gc->gen0.max_size));

  for(i = 0; i < PAGE_TYPES; i++) {
    unsigned long total_use = 0, count = 0;

    for(page = gc->gen1_pages[i]; page; page = page->next) {
      total_use += page->size;
      count++;
    }
    GCWARN((GCOUTF, "Generation 1 [%s]: %li bytes used in %li pages\n", 
            type_name[i], total_use, count));
  }

  GCWARN((GCOUTF, "Generation 1 [medium]:"));
  for (i = 0; i < NUM_MED_PAGE_SIZES; i++) {
    if (gc->med_pages[i]) {
      long count = 0, page_count = 0;
      for (page = gc->med_pages[i]; page; page = page->next) {
        void **start = PPTR(NUM(page->addr) + PREFIX_SIZE);
        void **end = PPTR(NUM(page->addr) + APAGE_SIZE - page->size);
        
        page_count++;
        
        while(start <= end) {
          objhead *info = (objhead *)start;
          if (!info->dead) {
            count += info->size;
          }
          start += info->size;
        }
      }
      GCWARN((GCOUTF, " %li [%li/%li]", count, page_count, gc->med_pages[i]->size));
    }
  }
  GCWARN((GCOUTF, "\n"));


  GCWARN((GCOUTF,"\n"));
  GCWARN((GCOUTF,"Current memory use: %li\n", GC_get_memory_use(NULL)));
  GCWARN((GCOUTF,"Peak memory use after a collection: %li\n", gc->peak_memory_use));
  GCWARN((GCOUTF,"Allocated (+reserved) page sizes: %li (+%li)\n", 
          gc->used_pages * APAGE_SIZE, 
          vm_memory_allocated(gc->vm) - (gc->used_pages * APAGE_SIZE)));
  GCWARN((GCOUTF,"# of major collections: %li\n", gc->num_major_collects));
  GCWARN((GCOUTF,"# of minor collections: %li\n", gc->num_minor_collects));
  GCWARN((GCOUTF,"# of installed finalizers: %i\n", gc->num_fnls));
  GCWARN((GCOUTF,"# of traced ephemerons: %i\n", gc->num_last_seen_ephemerons));

  if (flags & GC_DUMP_SHOW_TRACE) {
    print_traced_objects(path_length_limit, get_type_name, get_xtagged_name, print_tagged_value);
  }

  if (for_each_found)
    --gc->dumping_avoid_collection;
}

void GC_dump(void)
{
  GC_dump_with_traces(0, NULL, NULL, NULL, 0, NULL, 0);
}

#ifdef MZ_GC_BACKTRACE

int GC_is_tagged(void *p)
{
  NewGC *gc = GC_get_GC();
  mpage *page;
  page = pagemap_find_page(gc->page_maps, p);
  return page && (page->page_type == PAGE_TAGGED);
}

int GC_is_tagged_start(void *p)
{
  return 0;
}

void *GC_next_tagged_start(void *p)
{
  return NULL;
}

#endif

/*****************************************************************************/
/* garbage collection                                                        */
/*****************************************************************************/

static void reset_gen1_page(NewGC *gc, mpage *work)
{
  if (gc->generations_available && work->mprotected) {
    work->mprotected = 0;
    add_protect_page_range(gc->protect_range, work->addr, real_page_size(work), APAGE_SIZE, 1);
  }
}

static void reset_gen1_pages_live_and_previous_sizes(NewGC *gc)
{
  mpage *work;
  int i;

  GCDEBUG((DEBUGOUTF, "MAJOR COLLECTION - PREPPING PAGES - reset live_size, reset previous_size, unprotect.\n"));
  /* we need to make sure that previous_size for every page is reset, so
     we don't accidentally screw up the mark routine */

  for(i = 0; i < PAGE_TYPES; i++) {
    for(work = gc->gen1_pages[i]; work; work = work->next) {
      reset_gen1_page(gc, work);
      work->live_size = 0;
      work->previous_size = PREFIX_SIZE;
    }
  }

  for (i = 0; i < NUM_MED_PAGE_SIZES; i++) {
    for (work = gc->med_pages[i]; work; work = work->next) {
      if (work->generation) {
        reset_gen1_page(gc, work);
      }
    }
  }

  flush_protect_page_ranges(gc->protect_range, 1);
}

static void remove_gen1_page_from_pagemap(NewGC *gc, mpage *work)
{
  if (gc->generations_available && work->back_pointers && work->mprotected) {
    work->mprotected = 0;
    add_protect_page_range(gc->protect_range, work->addr, real_page_size(work), APAGE_SIZE, 1);
  }
  pagemap_remove(gc->page_maps, work);
  work->added = 0;
}

static void remove_all_gen1_pages_from_pagemap(NewGC *gc)
{
  mpage *work;
  int i;

  GCDEBUG((DEBUGOUTF, "MINOR COLLECTION - PREPPING PAGES - remove all gen1 pages from pagemap.\n"));

  /* if we're not doing a major collection, then we need to remove all the
     pages in gc->gen1_pages[] from the page map */

  for(i = 0; i < PAGE_TYPES; i++) {
    for(work = gc->gen1_pages[i]; work; work = work->next) {
      remove_gen1_page_from_pagemap(gc, work);
    }
  }

  for (i = 0; i < NUM_MED_PAGE_SIZES; i++) {
    for (work = gc->med_pages[i]; work; work = work->next) {
      if (work->generation) {
        remove_gen1_page_from_pagemap(gc, work);
      }
    }
  }

  flush_protect_page_ranges(gc->protect_range, 1);
}

static void mark_backpointers(NewGC *gc)
{
  if(!gc->gc_full) {
    mpage *work;
    int i;
    PageMap pagemap = gc->page_maps;

    /* if this is not a full collection, then we need to mark any pointers
       that point backwards into generation 0, since they're roots. */
    for(i = 0; i < PAGE_TYPES; i++) {
      for(work = gc->gen1_pages[i]; work; work = work->next) {
        if(work->back_pointers) {
          /* these pages are guaranteed not to be write protected, because
             if they were, they wouldn't have this bit set */
          work->marked_on = 1;
          work->previous_size = PREFIX_SIZE;
          pagemap_add(pagemap, work);
          if(work->size_class) {
            /* must be a big page */
            work->size_class = 3;
            push_ptr(gc, TAG_AS_BIG_PAGE_PTR(BIG_PAGE_TO_OBJECT(work)));
          } else {
            if(work->page_type != PAGE_ATOMIC) {
              void **start = PAGE_START_VSS(work);
              void **end = PAGE_END_VSS(work);

              while(start < end) {
                objhead *info = (objhead *)start;
                if(!info->dead) {
                  info->mark = 1;
                  /* This must be a push_ptr, and not a direct call to
                     internal_mark. This is because we need every object
                     in the older heap to be marked out of and noted as
                     marked before we do anything else */
                  push_ptr(gc, OBJHEAD_TO_OBJPTR(start));
                }
                start += info->size;
              }
            }
          }
          work->previous_size = PREFIX_SIZE;
        } else {
          GCDEBUG((DEBUGOUTF,"Setting previous_size on %p to %i\n", work,
                   work->size));
          work->previous_size = work->size;
        }
      }
    }

    for (i = 0; i < NUM_MED_PAGE_SIZES; i++) {
      for (work = gc->med_pages[i]; work; work = work->next) {
        if(work->back_pointers) {
          void **start = PPTR(NUM(work->addr) + PREFIX_SIZE);
          void **end = PPTR(NUM(work->addr) + APAGE_SIZE - work->size);
          
          work->marked_on = 1;
          pagemap_add(pagemap, work);

          while(start <= end) {
            objhead *info = (objhead *)start;
            if(!info->dead) {
              info->mark = 1;
              /* This must be a push_ptr (see above) */
              push_ptr(gc, OBJHEAD_TO_OBJPTR(info));
            }
            start += info->size;
          }
        }
      }
    }
  }
}

mpage *allocate_compact_target(NewGC *gc, mpage *work)
{
  mpage *npage;

  npage = malloc_mpage();
  npage->addr = malloc_dirty_pages(gc, APAGE_SIZE, APAGE_SIZE);
  npage->previous_size = npage->size = PREFIX_SIZE;
  npage->generation = 1;
  npage->back_pointers = 0;
  npage->size_class = 0;
  npage->page_type = work->page_type;
  npage->marked_on = 1;
  backtrace_new_page(gc, npage);
  GCVERBOSEPAGE("NEW COMPACT PAGE", npage);
  /* Link in this new replacement page */
  npage->prev = work;
  npage->next = work->next;
  work->next = npage;
  if (npage->next)
    npage->next->prev = npage;

  return npage;
}

/* Compact when 1/4 of the space between objects is unused: */
#define should_compact_page(lsize,tsize) (lsize < (tsize - PREFIX_SIZE - (APAGE_SIZE >> 2)))

inline static void do_heap_compact(NewGC *gc)
{
  int i;
  PageMap pagemap = gc->page_maps;

  for(i = 0; i < PAGE_BIG; i++) {
    mpage *work = gc->gen1_pages[i], *prev, *npage;

    /* Start from the end: */
    if (work) {
      while (work->next)
        work = work->next;
    }
    npage = work;

    while(work) {
      if(work->marked_on && !work->has_new) {
        /* then determine if we actually want to do compaction */
        if(should_compact_page(gcWORDS_TO_BYTES(work->live_size),work->size)) {
          void **start = PAGE_START_VSS(work);
          void **end = PAGE_END_VSS(work);
          void **newplace;
          unsigned long avail;

          GCDEBUG((DEBUGOUTF, "Compacting page %p: new version at %p\n", 
                   work, npage));

          if (npage == work) {
            /* Need to insert a page: */
            npage = allocate_compact_target(gc, work);
          }
          avail = gcBYTES_TO_WORDS(APAGE_SIZE - npage->size);
          newplace = PPTR(NUM(npage->addr) + npage->size);

          while(start < end) {
            objhead *info = (objhead *)start;

            if(info->mark) {
              while (avail <= info->size) {
                npage->size = NUM(newplace) - NUM(npage->addr);
                do {
                  npage = npage->prev;
                } while (!npage->marked_on || npage->has_new);
                if (npage == work)
                  npage = allocate_compact_target(gc, work);
                avail = gcBYTES_TO_WORDS(APAGE_SIZE - npage->size);
                newplace = PPTR(NUM(npage->addr) + npage->size);
              }

              if (npage->mprotected) {
                npage->mprotected = 0;
                vm_protect_pages(npage->addr, APAGE_SIZE, 1);
              }

              GCDEBUG((DEBUGOUTF,"Moving size %i object from %p to %p\n",
                       gcWORDS_TO_BYTES(info->size), start+1, newplace+1));
              memcpy(newplace, start, gcWORDS_TO_BYTES(info->size));
              info->moved = 1;
              *(PPTR(OBJHEAD_TO_OBJPTR(start))) = OBJHEAD_TO_OBJPTR(newplace);
              copy_backtrace_source(npage, newplace, work, start);
              newplace += info->size;
              avail -= info->size;
            }
            start += info->size;	    
          }
          npage->size = NUM(newplace) - NUM(npage->addr);

          prev = work->prev;

          if(prev) prev->next = work->next; else gc->gen1_pages[i] = work->next;
          if(work->next) work->next->prev = prev;

          /* push work onto gc->release_pages */
          work->next = gc->release_pages;
          gc->release_pages = work;

          /* add the old page to the page map so fixups can find forwards */
          pagemap_add(pagemap, work);

          work = prev;
        } else { 
          work = work->prev;
        }
      } else {
        if (npage == work)
          npage = npage->prev;
        work = work->prev;
      }
    }
  }
}

#ifdef KILLING_DEBUG
#include <ctype.h>
static void fprintf_buffer(FILE* file, char* buf, int l) {
  int i;
  for (i=0; i < l; i++ ) { fprintf(file, "%02hhx",buf[i]); }
  fprintf(file, "\n"); 
  for (i=0; i < l; i++ ) {
    unsigned char c = buf[i];
    if(isprint(c)) { fprintf(file, "%c ", c); }
    else           { fprintf(file, "  "); }
  }
  fprintf(file, "\n"); 
}

static void fprintf_debug(NewGC *gc, const char *msg, objhead *info, FILE* file, int isgc) {
  if (!isgc || postmaster_and_master_gc(gc)) {
    Scheme_Object *obj = OBJHEAD_TO_OBJPTR(info);
    fprintf(file, "%s %p ot %i it %i im %i is %i is >> 3 %i\n", msg, obj, obj->type, info->type, info->mark, info->size, info->size >> 3);
    switch (obj->type) {
      case scheme_unix_path_type:
        if (pagemap_find_page(gc->page_maps, SCHEME_PATH_VAL(obj))) {
          fprintf_buffer(file, SCHEME_PATH_VAL(obj), SCHEME_PATH_LEN(obj));
        }
        else {
          fprintf(file, "%p already freed and out of bounds\n", SCHEME_PATH_VAL(obj));
        }
        break;
      case scheme_symbol_type:
        fprintf_buffer(file, SCHEME_SYM_VAL(obj), SCHEME_SYM_LEN(obj));
        break;
      case scheme_resolved_module_path_type:
        if (pagemap_find_page(gc->page_maps, SCHEME_PTR_VAL(obj))) {
          fprintf_debug(gc, "RMP ", OBJPTR_TO_OBJHEAD(SCHEME_PTR_VAL(obj)), file, isgc);
        }
        else {
          fprintf(file, "RMP %p already freed and out of bounds\n", SCHEME_PATH_VAL(obj));
        }
      default:
        fprintf_buffer(file, ((char *)obj), (info->size * WORD_SIZE) - sizeof(objhead));
        break;
    }
  }
}
static void killing_debug(NewGC *gc, void *info) {
  fprintf_debug(gc, "killing", (objhead *) info, gcdebugOUT(), 1);
}
static void marking_rmp_debug(NewGC *gc, void *info) {
  fprintf_debug(gc, "marking rmp", (objhead *) info, gcdebugOUT(), 0);
}
#endif

static void repair_heap(NewGC *gc)
{
  mpage *page;
  int i;
  Fixup2_Proc *fixup_table = gc->fixup_table;
#ifdef MZ_USE_PLACES
  int master_has_switched = postmaster_and_master_gc(gc);
#endif

  
  for(i = 0; i < PAGE_TYPES; i++) {
    for(page = gc->gen1_pages[i]; page; page = page->next) {
#ifdef MZ_USE_PLACES
      if (master_has_switched || page->marked_on)
#else
      if (page->marked_on)
#endif
      {
        page->has_new = 0;
        /* these are guaranteed not to be protected */
        if(page->size_class)  {
          /* since we get here via gen1_pages, it's a big page */
          void **start = PPTR(BIG_PAGE_TO_OBJECT(page));
          void **end = PAGE_END_VSS(page);
#ifdef MZ_USE_PLACES
          objhead *info = BIG_PAGE_TO_OBJHEAD(page);
          if (page->marked_on || info->mark) {
          page->marked_on = 1;
#endif
          GCDEBUG((DEBUGOUTF, "Cleaning objs on page %p, starting with %p\n",
                   page, start));
          page->size_class = 2; /* remove the mark */
          switch(page->page_type) {
          case PAGE_TAGGED: 
            fixup_table[*(unsigned short*)start](start, gc); 
            break;
          case PAGE_ATOMIC: break;
          case PAGE_ARRAY: 
            while(start < end) gcFIXUP2(*(start++), gc); 
            break;
          case PAGE_XTAGGED: 
            GC_fixup_xtagged(start); 
            break;
          case PAGE_TARRAY: {
            unsigned short tag = *(unsigned short *)start;
            ASSERT_TAG(tag);
            end -= INSET_WORDS;
            while(start < end) start += fixup_table[tag](start, gc);
            break;
          }
          }
#ifdef MZ_USE_PLACES
          }
#endif
        } else {
          void **start = PPTR(NUM(page->addr) + page->previous_size);
          void **end = PAGE_END_VSS(page);

          GCDEBUG((DEBUGOUTF, "Cleaning objs on page %p, starting with %p\n",
                page, start));
          switch(page->page_type) {
            case PAGE_TAGGED: 
              while(start < end) {
                objhead *info = (objhead *)start;

                if(info->mark) {
                  void *obj_start = OBJHEAD_TO_OBJPTR(start);
                  unsigned short tag = *(unsigned short *)obj_start;
                  ASSERT_TAG(tag);
                  info->mark = 0;
                  fixup_table[tag](obj_start, gc);
                } else {
                  info->dead = 1;
                }
                start += info->size;
              }
              break;
            case PAGE_ATOMIC:
              while(start < end) {
                objhead *info = (objhead *)start;
                if(info->mark) {
                  info->mark = 0;
                } else info->dead = 1;
                start += info->size;
              }
              break;
            case PAGE_ARRAY: 
              while(start < end) {
                objhead *info = (objhead *)start;
                size_t size = info->size;
                if(info->mark) {
                  void **tempend = PPTR(info) + info->size;
                  start = OBJHEAD_TO_OBJPTR(start);
                  while(start < tempend) gcFIXUP2(*start++, gc);
                  info->mark = 0;
                } else { 
                  info->dead = 1;
                  start += size;
                }
              }
              break;
            case PAGE_TARRAY:
              while(start < end) {
                objhead *info = (objhead *)start;
                size_t size = info->size;
                if(info->mark) {
                  void **tempend = PPTR(info) + (info->size - INSET_WORDS);
                  unsigned short tag;
                  start = OBJHEAD_TO_OBJPTR(start);
                  tag = *(unsigned short*)start;
                  ASSERT_TAG(tag);
                  while(start < tempend)
                    start += fixup_table[tag](start, gc);
                  info->mark = 0;
                  start = PPTR(info) + size;
                } else {
                  info->dead = 1;
                  start += size;
                }
              }
              break;
            case PAGE_XTAGGED:
              while(start < end) {
                objhead *info = (objhead *)start;
                if(info->mark) {
                  GC_fixup_xtagged(OBJHEAD_TO_OBJPTR(start));
                  info->mark = 0;
                } else info->dead = 1;
                start += info->size;
              }
          }
        }
      } else GCDEBUG((DEBUGOUTF,"Not Cleaning page %p\n", page));
    }
  }

  for (i = 0; i < NUM_MED_PAGE_SIZES; i++) {
    for (page = gc->med_pages[i]; page; page = page->next) {
#ifdef MZ_USE_PLACES
      if (master_has_switched || page->marked_on)
#else
      if (page->marked_on)
#endif
      {
        void **start = PPTR(NUM(page->addr) + PREFIX_SIZE);
        void **end = PPTR(NUM(page->addr) + APAGE_SIZE - page->size);
        
        while(start <= end) {
          objhead *info = (objhead *)start;
          if(info->mark) {
            switch(info->type) {
            case PAGE_ARRAY:
              {
                void **tempend = PPTR(info) + info->size;
                start = OBJHEAD_TO_OBJPTR(start);
                while(start < tempend) gcFIXUP2(*start++, gc);
              }
              break;
            case PAGE_TAGGED:
              {
                void *obj_start = OBJHEAD_TO_OBJPTR(start);
                unsigned short tag = *(unsigned short *)obj_start;
                ASSERT_TAG(tag);
                fixup_table[tag](obj_start, gc);
                start += info->size;
              }
              break;
            case PAGE_ATOMIC:
              start += info->size;
            }
            info->mark = 0;
#ifdef MZ_USE_PLACES
            page->marked_on = 1;
#endif
          } else {
#ifdef KILLING_DEBUG
            killing_debug(gc, info);
#endif
            info->dead = 1;
            start += info->size;
          }
        }
      }
    }
  }
}

static inline void gen1_free_mpage(PageMap pagemap, mpage *page) {
  pagemap_remove(pagemap, page);
  free_backtrace(page);
  free_pages(GC_instance, page->addr, real_page_size(page));
  free_mpage(page);
}

static inline void cleanup_vacated_pages(NewGC *gc) {
  mpage *pages = gc->release_pages;
  PageMap pagemap = gc->page_maps;

  /* Free pages vacated by compaction: */
  while (pages) {
    mpage *next = pages->next;
    GCVERBOSEPAGE("Cleaning up vacated", pages);
    gen1_free_mpage(pagemap, pages);
    pages = next;
  }
  gc->release_pages = NULL;
}

inline static void gen0_free_big_pages(NewGC *gc) {
  mpage *work;
  mpage *next;
  PageMap pagemap = gc->page_maps;

  for(work = gc->gen0.big_pages; work; work = next) {
    GCVERBOSEPAGE("FREEING BIG PAGE", work);
 
    next = work->next;
    pagemap_remove(pagemap, work);
    free_pages(gc, work->addr, round_to_apage_size(work->size));
    free_mpage(work);
  }

  /* They are all gone, set the pointer to NULL */
  gc->gen0.big_pages = NULL; 
}

static void clean_up_heap(NewGC *gc)
{
  int i;
  size_t memory_in_use = 0;
  PageMap pagemap = gc->page_maps;

  gen0_free_big_pages(gc);

  for(i = 0; i < PAGE_TYPES; i++) {
    if(gc->gc_full) {
      mpage *work = gc->gen1_pages[i];
      mpage *prev = NULL;
      while(work) {
        mpage *next = work->next;
        if(!work->marked_on) {
          /* remove work from list */
          if(prev) prev->next = next; else gc->gen1_pages[i] = next;
          if(next) work->next->prev = prev;
          GCVERBOSEPAGE("Cleaning up BIGPAGE", work);
          gen1_free_mpage(pagemap, work);
        } else {
          GCVERBOSEPAGE("clean_up_heap BIG PAGE ALIVE", work);
          pagemap_add(pagemap, work);
          work->back_pointers = work->marked_on = 0;
          memory_in_use += work->size;
          prev = work; 
        }
        work = next;
      }
    } else {
      mpage *work;
      for(work = gc->gen1_pages[i]; work; work = work->next) {
        pagemap_add(pagemap, work);
        work->back_pointers = work->marked_on = 0;
        memory_in_use += work->size;
      }
    }
  }

  for (i = 0; i < NUM_MED_PAGE_SIZES; i++) {
    mpage *work;
    mpage *prev = NULL, *next;

    for (work = gc->med_pages[i]; work; work = next) {
      if (work->marked_on) {
        void **start = PPTR(NUM(work->addr) + PREFIX_SIZE);
        void **end = PPTR(NUM(work->addr) + APAGE_SIZE - work->size);
        int non_dead = 0;

        while(start <= end) {
          objhead *info = (objhead *)start;
          if (!info->dead) {
            non_dead++;
          }
          start += info->size;
        }

        next = work->next;
        if (non_dead) {
          work->live_size = (work->size * non_dead);
          memory_in_use += work->live_size;
          work->previous_size = PREFIX_SIZE;
          work->back_pointers = work->marked_on = 0;
          work->generation = 1;
          pagemap_add(pagemap, work);
          prev = work;
        } else {
          /* free the page */
          if(prev) prev->next = next; else gc->med_pages[i] = next;
          if(next) work->next->prev = prev;
          GCVERBOSEPAGE("Cleaning up MED PAGE NO OBJ", work);
          gen1_free_mpage(pagemap, work);
        }
      } else if (gc->gc_full || !work->generation) {
        /* Page wasn't touched in full GC, or gen-0 not touched,
           so we can free it. */
        next = work->next;
        if(prev) prev->next = next; else gc->med_pages[i] = next;
        if(next) work->next->prev = prev;
        GCVERBOSEPAGE("Cleaning up MED NO MARKEDON", work);
        gen1_free_mpage(pagemap, work);
      } else {
        /* not touched during minor gc */
        memory_in_use += work->live_size;
        work->previous_size = PREFIX_SIZE;
        next = work->next;
        prev = work;
        work->back_pointers = 0;
        pagemap_add(pagemap, work);
      }
    }
    gc->med_freelist_pages[i] = prev;
  }

  gc->memory_in_use = memory_in_use;
  cleanup_vacated_pages(gc);
}

#ifdef MZ_USE_PLACES
static void unprotect_old_pages(NewGC *gc)
{
  Page_Range *protect_range = gc->protect_range;
  mpage *page;
  int i;

  for(i = 0; i < PAGE_TYPES; i++) {
    if(i != PAGE_ATOMIC)
      for(page = gc->gen1_pages[i]; page; page = page->next)
        if(page->page_type != PAGE_ATOMIC)  {
          if (page->mprotected) {
            page->mprotected = 0;
            add_protect_page_range(protect_range, page->addr, page->size, APAGE_SIZE, 1);
          }
        }
  }

  for (i = 0; i < NUM_MED_PAGE_SIZES; i++) {
    for (page = gc->med_pages[i]; page; page = page->next) {
      if (page->mprotected) {
        page->mprotected = 0;
        add_protect_page_range(protect_range, page->addr, page->size, APAGE_SIZE, 1);
      }
    }
  }

  flush_protect_page_ranges(protect_range, 0);
}
#endif

static void protect_old_pages(NewGC *gc)
{
  Page_Range *protect_range = gc->protect_range;
  mpage *page;
  int i;

  for(i = 0; i < PAGE_TYPES; i++) {
    if(i != PAGE_ATOMIC)
      for(page = gc->gen1_pages[i]; page; page = page->next)
        if(page->page_type != PAGE_ATOMIC)  {
          if (!page->mprotected) {
            page->mprotected = 1;
            add_protect_page_range(protect_range, page->addr, page->size, APAGE_SIZE, 0);
          }
        }
  }

  for (i = 0; i < NUM_MED_PAGE_SIZES; i++) {
    for (page = gc->med_pages[i]; page; page = page->next) {
      if (!page->mprotected) {
        page->mprotected = 1;
        add_protect_page_range(protect_range, page->addr, APAGE_SIZE, APAGE_SIZE, 0);
      }
    }
  }

  flush_protect_page_ranges(protect_range, 0);
}

#if 0
extern double scheme_get_inexact_milliseconds(void);
# define TIME_DECLS() double start, task_start
# define TIME_INIT() start = task_start = scheme_get_inexact_milliseconds(); fprintf(stderr, "GC (%d):\n", gc->gc_full)
# define TIME_STEP(task) fprintf(stderr, "  %s: %lf\n", task, scheme_get_inexact_milliseconds() - task_start); \
  task_start = scheme_get_inexact_milliseconds()
# define TIME_DONE() fprintf(stderr, " Total: %lf\n", scheme_get_inexact_milliseconds() - start)
#else
# define TIME_DECLS() /**/
# define TIME_INIT() /**/
# define TIME_STEP(task) /**/
# define TIME_DONE() /**/
#endif

/* Full GCs trigger finalization. Finalization releases data
   in the old generation. So one more full GC is needed to
   really clean up. The full_needed_for_finalization flag triggers 
   the second full GC. */

static void garbage_collect(NewGC *gc, int force_full, int switching_master)
{
  unsigned long old_mem_use;
  unsigned long old_gen0;

  int next_gc_full;

#ifdef MZ_USE_PLACES
  if (postmaster_and_place_gc(gc)) {
    mzrt_rwlock_rdlock(MASTERGCINFO->cangc);
    /* printf("RD MGCLOCK garbage_collect %i\n", gc->place_id); */
  }
#endif

  old_mem_use = gc->memory_in_use;
  old_gen0    = gc->gen0.current_size;

  TIME_DECLS();

  /* determine if this should be a full collection or not */
  gc->gc_full = force_full || !gc->generations_available 
    || (gc->since_last_full > 100) || (gc->memory_in_use > (2 * gc->last_full_mem_use));
#if 0
  printf("Collection %li (full = %i): %i / %i / %i / %i  %ld\n", number_of_gc_runs, 
      gc->gc_full, force_full, !generations_available,
      (gc->since_last_full > 100), (gc->memory_in_use > (2 * gc->last_full_mem_use)),
      gc->last_full_mem_use);
#endif

  next_gc_full = gc->gc_full;

  if (gc->full_needed_for_finalization) {
    gc->full_needed_for_finalization= 0;
    gc->gc_full = 1;
  }
#ifdef DEBUG_GC_PAGES
  if (gc->gc_full == 1) {
    GCVERBOSEprintf("GC_FULL gc: %p MASTER: %p\n", gc, MASTERGC);
  }
#endif
  gc->number_of_gc_runs++; 
  INIT_DEBUG_FILE(); DUMP_HEAP();

  /* we don't want the low-level allocator freaking because we've gone past
     half the available memory */
  gc->in_unsafe_allocation_mode = 1;
  gc->unsafe_allocation_abort = out_of_memory_gc;

  TIME_INIT();

  /* inform the system (if it wants us to) that we're starting collection */
  if(gc->GC_collect_start_callback)
    gc->GC_collect_start_callback();

  TIME_STEP("started");

  gc->no_further_modifications = 1;

  if (gc->gc_full)
    reset_gen1_pages_live_and_previous_sizes(gc);
  else /* minor collection */
    remove_all_gen1_pages_from_pagemap(gc);

  init_weak_boxes(gc);
  init_weak_arrays(gc);
  init_ephemerons(gc);

  /* at this point, the page map should only include pages that contain
     collectable objects */

  TIME_STEP("prepared");

  /* mark and repair the roots for collection */
  mark_backpointers(gc);
  TIME_STEP("backpointered");
  mark_finalizer_structs(gc);
  mark_weak_finalizer_structs(gc);
  TIME_STEP("pre-rooted");
  mark_roots(gc);
  mark_immobiles(gc);
  TIME_STEP("rooted");
#ifdef MZ_USE_PLACES
  if (premaster_or_place_gc(gc))
#endif
    GC_mark_variable_stack(GC_variable_stack, 0, get_stack_base(gc), NULL);

  TIME_STEP("stacked");

  /* now propagate/repair the marks we got from these roots, and do the
     finalizer passes */
  propagate_marks(gc);
  mark_ready_ephemerons(gc); 
  propagate_marks(gc); 

  check_finalizers(gc, 1);
  mark_ready_ephemerons(gc);
  propagate_marks(gc);

  check_finalizers(gc, 2);
  mark_ready_ephemerons(gc);
  propagate_marks(gc);

  if(gc->gc_full) zero_weak_finalizers(gc);
  do_ordered_level3(gc); propagate_marks(gc);
  check_finalizers(gc, 3); propagate_marks(gc);
  if(gc->gc_full) {
    reset_weak_finalizers(gc); 
    propagate_marks(gc);
  }
#ifndef NEWGC_BTC_ACCOUNT
  /* we need to clear out the stack pages. If we're doing memory accounting,
     though, we might as well leave them up for now and let the accounting
     system clear them later. Better then freeing them, at least. If we're
     not doing accounting, though, there is no "later" where they'll get
     removed */
  clear_stack_pages(gc);  
#endif

  TIME_STEP("marked");

  zero_weak_boxes(gc); 
  zero_weak_arrays(gc);
  zero_remaining_ephemerons(gc);

  TIME_STEP("zeroed");

  if(gc->gc_full)
#ifdef MZ_USE_PLACES
  if (!MASTERGC)
#endif
    do_heap_compact(gc);
  TIME_STEP("compacted");

  /* do some cleanup structures that either change state based on the
     heap state after collection or that become useless based on changes
     in state after collection */
#ifdef NEWGC_BTC_ACCOUNT
  BTC_clean_up(gc);
#endif
  TIME_STEP("cleaned");
  repair_finalizer_structs(gc);
  repair_weak_finalizer_structs(gc);
  repair_roots(gc);
  repair_immobiles(gc);
#ifdef MZ_USE_PLACES
  if (premaster_or_place_gc(gc))
#endif
    GC_fixup_variable_stack(GC_variable_stack, 0, get_stack_base(gc), NULL);
  TIME_STEP("reparied roots");
  repair_heap(gc);
  TIME_STEP("repaired");
  clean_up_heap(gc);
  TIME_STEP("cleaned heap");
#ifdef MZ_USE_PLACES
  if (postmaster_and_master_gc(gc) && !switching_master) {
    master_set_max_size(gc); 
  }
  if (premaster_or_place_gc(gc) && !switching_master)
#endif
    reset_nursery(gc);
  TIME_STEP("reset nursurey");
#ifdef NEWGC_BTC_ACCOUNT
  if (gc->gc_full)
    BTC_do_accounting(gc);
#endif
  TIME_STEP("accounted");
  if (gc->generations_available) {
#ifdef MZ_USE_PLACES
    if (postmaster_and_master_gc(gc) || switching_master) {
      unprotect_old_pages(gc);
    }
    else {
      protect_old_pages(gc);
    }
#else
    protect_old_pages(gc);
#endif
}
  TIME_STEP("protect");
  if (gc->gc_full)
    vm_flush_freed_pages(gc->vm);
  reset_finalizer_tree(gc);

  TIME_STEP("reset");

  /* now we do want the allocator freaking if we go over half */
  gc->in_unsafe_allocation_mode = 0;

  gc->no_further_modifications = 0;

  /* If we have too many idle pages, flush: */
  if (vm_memory_allocated(gc->vm) > ((gc->used_pages << (LOG_APAGE_SIZE + 1)))) {
    vm_flush_freed_pages(gc->vm);
  }

  /* update some statistics */
  if(gc->gc_full) gc->num_major_collects++; else gc->num_minor_collects++;
  if(gc->peak_memory_use < gc->memory_in_use) gc->peak_memory_use = gc->memory_in_use;
  if(gc->gc_full)
    gc->since_last_full = 0;
  else if((float)(gc->memory_in_use - old_mem_use) < (0.1 * (float)old_mem_use))
    gc->since_last_full += 1;
  else if((float)(gc->memory_in_use - old_mem_use) < (0.4 * (float)old_mem_use))
    gc->since_last_full += 5;
  else 
    gc->since_last_full += 10;
  if(gc->gc_full)
    gc->last_full_mem_use = gc->memory_in_use;

  /* inform the system (if it wants us to) that we're done with collection */
  if (gc->GC_collect_end_callback)
    gc->GC_collect_end_callback();
  if (gc->GC_collect_inform_callback)
    gc->GC_collect_inform_callback(gc->gc_full, old_mem_use + old_gen0, gc->memory_in_use);

  TIME_STEP("ended");

  TIME_DONE();

  if (!gc->run_queue)
    next_gc_full = 0;

  /* run any queued finalizers, EXCEPT in the case where this collection was
     triggered by the execution of a finalizer. The outside world needs this
     invariant in some corner case I don't have a reference for. In any case,
     if we run a finalizer after collection, and it triggers a collection,
     we should not run the next finalizer in the queue until the "current"
     finalizer completes its execution */
  if(!gc->running_finalizers) {
    gc->running_finalizers = 1;

    /* Finalization might allocate, which might need park: */
    gc->park_save[0] = gc->park[0];
    gc->park_save[1] = gc->park[1];
    gc->park[0] = NULL;
    gc->park[1] = NULL;

    while(gc->run_queue) {
      struct finalizer *f;
      void **saved_gc_variable_stack;

      f = gc->run_queue; gc->run_queue = gc->run_queue->next;
      if(!gc->run_queue) gc->last_in_queue = NULL;

      GCDEBUG((DEBUGOUTF, "Running finalizers %p for pointer %p (lvl %i)\n", f, f->p, f->eager_level));
      saved_gc_variable_stack = GC_variable_stack;
      f->f(f->p, f->data);
      GC_variable_stack = saved_gc_variable_stack;
    }
#ifdef NEWGC_BTC_ACCOUNT
    BTC_run_account_hooks(gc);
#endif
    gc->running_finalizers = 0;

    gc->park[0] = gc->park_save[0];
    gc->park[1] = gc->park_save[1];
    gc->park_save[0] = NULL;
    gc->park_save[1] = NULL;
  }

  DUMP_HEAP(); CLOSE_DEBUG_FILE();

  if (next_gc_full)
    gc->full_needed_for_finalization = 1;

#ifdef MZ_USE_PLACES
  if (postmaster_and_place_gc(gc)) {
    /* printf("UN RD MGCLOCK garbage_collect %i\n", gc->place_id); */
    mzrt_rwlock_unlock(MASTERGCINFO->cangc);
    if (gc->gc_full) { 
       wait_if_master_in_progress(gc);
    }
  }
#endif
}

#if MZ_GC_BACKTRACE

static GC_get_type_name_proc stack_get_type_name;
static GC_get_xtagged_name_proc stack_get_xtagged_name;
static GC_print_tagged_value_proc stack_print_tagged_value;

static void dump_stack_pos(void *a) 
{
  GCPRINT(GCOUTF, " @%p: ", a);
  print_out_pointer("", *(void **)a, stack_get_type_name, stack_get_xtagged_name, stack_print_tagged_value);
}

# define GC_X_variable_stack GC_do_dump_variable_stack
# define gcX2(a, gc) dump_stack_pos(a)
# define X_source(stk, p) /* */
# include "var_stack.c"
# undef GC_X_variable_stack
# undef gcX
# undef X_source

void GC_dump_variable_stack(void **var_stack,
    long delta,
    void *limit,
    void *stack_mem,
    GC_get_type_name_proc get_type_name,
    GC_get_xtagged_name_proc get_xtagged_name,
    GC_print_tagged_value_proc print_tagged_value)
{
  stack_get_type_name = get_type_name;
  stack_get_xtagged_name = get_xtagged_name;
  stack_print_tagged_value = print_tagged_value;
  GC_do_dump_variable_stack(var_stack, delta, limit, stack_mem);
}

#endif

/******************************************************************************/
/*                              GC free all                                   */
/******************************************************************************/

void GC_free_all(void)
{
  NewGC *gc = GC_get_GC();
  int i;
  mpage *work;
  mpage *next;
  PageMap pagemap = gc->page_maps;

  remove_signal_handler(gc);

  gen0_free_big_pages(gc);

  for(i = 0; i < PAGE_TYPES; i++) {
    for (work = gc->gen1_pages[i]; work; work = next) {
      next = work->next;

      if (work->mprotected)
        vm_protect_pages(work->addr, real_page_size(work), 1);
      GCVERBOSEPAGE("Cleaning up GC DYING", work);
      gen1_free_mpage(pagemap, work);
    }
  }

  free(gc->mark_table);
  free(gc->fixup_table);

  free_page_maps(gc->page_maps);

  free(gc->protect_range);

  vm_flush_freed_pages(gc->vm);
  vm_free(gc->vm);
  free(gc);
}
