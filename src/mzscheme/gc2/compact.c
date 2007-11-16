/*
  Precise GC for MzScheme
  Copyright (c) 2004-2007 PLT Scheme Inc.
  Copyright (c) 1999 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/**************** Configuration ****************/

#define GROW_FACTOR 1.5
#define GROW_ADDITION 500000

#define GENERATIONS 1

/* Platform-specific disablers (`and'ed with GENERATIONS): */
#define OS_X_GENERATIONS 1
#define WIN32_GENERATIONS 1


#ifdef NO_GC_SIGNALS
# undef GENERATIONS
# define GENERATIONS 0
#endif

#ifdef OS_X
# if GENERATIONS
#  undef GENERATIONS
#  define GENERATIONS OS_X_GENERATIONS
# endif
#endif

#ifdef _WIN32
# if GENERATIONS
#  undef GENERATIONS
#  define GENERATIONS WIN32_GENERATIONS
/* Under Windows, setting the unhandled-exception handler doesn't work
   within Windows callbacks. Perhaps in the future we can fix all
   callbacks to insert an appropriate wrapper. For now, we use
   AddVectoredExceptionHandler, but that's only available starting
   with XP. We detect the presence of AddVectoredExceptionHandler
   dynamically (and disable generations if it's not present), but we
   also make generations easy to disable entirely above. */
# endif
#endif

#define USE_FREELIST 0

/* When USE_FREELIST is on: */
#define COMPACTING SELECTIVELY_COMPACT
# define ALWAYS_COMPACT      2
# define SELECTIVELY_COMPACT 1
# define NEVER_COMPACT       0
#define COMPACT_THRESHOLD 0.2

#ifdef _WIN32
# include <windows.h>
# define gcINLINE _inline
#endif
#ifdef OSKIT
# undef GENERATIONS
# define GENERATIONS 0
#endif

#if defined(sparc) || defined(__sparc) || defined(__sparc__)
# define ALIGN_DOUBLES 1
#else
# define ALIGN_DOUBLES 0
#endif

#define LOG_WORD_SIZE 2
#define WORD_SIZE (1 << LOG_WORD_SIZE)
#define WORD_BIT_COUNT (WORD_SIZE << 3)

#define INCREMENT_CYCLE_COUNT_GROWTH 1048576

typedef short Type_Tag;

#include "gc2.h"
#include "gc2_dump.h"

#define BYTEPTR(x) ((char *)x)

/* Debugging and performance tools: */
#define TIME 0
#define SEARCH 0
#define CHECKS 0
#define CHECK_STACK_PTRS 0
#define NOISY 0
#define MARK_STATS 0
#define ALLOC_GC_PHASE 0
#define SKIP_FORCED_GC 0
#define RECORD_MARK_SRC 0
#define KEEP_BACKPOINTERS 0
#define DEFINE_MALLOC_FREE 0

#ifdef MZ_GC_BACKTRACE
# undef KEEP_BACKPOINTERS
# define KEEP_BACKPOINTERS 1
#endif

#if TIME
# include <sys/time.h>
# include <sys/resource.h>
# include <unistd.h>
#endif

#include "msgprint.c"

/**************** Stack for mark phase ****************/
#define MARK_STACK_MAX 4096
static void *mark_stack[MARK_STACK_MAX];
static unsigned short mark_stack_type[MARK_STACK_MAX];
static long mark_stack_pos = 0;

#if KEEP_BACKPOINTERS
# undef RECORD_MARK_SRC
# define RECORD_MARK_SRC 1
/* Disabled generations, since old-page ifxups would be wrong,
   and even if that were fixed, the results would be confusing. */
# undef GENERATIONS
# define GENERATIONS 0
#endif

#if RECORD_MARK_SRC
static void *mark_src;
static int mark_type;
static void *mark_src_stack[MARK_STACK_MAX];
static int mark_src_type[MARK_STACK_MAX];

static void *current_mark_src;
static int current_mark_type;

#define MTYPE_ROOT      6
#define MTYPE_STACK     7
#define MTYPE_FINALIZER 8
#define MTYPE_WEAKLINK  9
#define MTYPE_WEAKLINKX 10
#define MTYPE_IMMOBILE  11
#endif

/********************* Client hooks *********************/
void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_out_of_memory)(void);
unsigned long (*GC_get_thread_stack_base)(void);

void (*GC_mark_xtagged)(void *obj);
void (*GC_fixup_xtagged)(void *obj);

void **GC_variable_stack;

void **GC_get_variable_stack() { return GC_variable_stack; }
void GC_set_variable_stack(void **p) { GC_variable_stack = p; }

/********************* Type tags *********************/
Type_Tag pair_tag = 42; /* set by client */
Type_Tag weak_box_tag = 42; /* set by client */
Type_Tag ephemeron_tag = 42; /* set by client */
Type_Tag weak_array_tag  = 42; /* set by client */

#define gc_on_free_list_tag 511

#define _num_tags_ 512

Size_Proc size_table[_num_tags_];
Mark_Proc mark_table[_num_tags_];
Fixup_Proc fixup_table[_num_tags_];

/****************** Memory Pages ******************/

/* An MPage (as opposed to the OS's page) is an allocation region
   for a particular kind of object (tagged, atomic, array, etc.).
   It's also the granluarity of memory-mapping (i.e., taking an 
   arbitrary pointer an determining whether it's in the GC's
   domain.

   It has an associated offset table, which is mainly used for
   updating pointers during the fixup phase.
*/

#if ALIGN_DOUBLES || DEFINE_MALLOC_FREE
# define SQUASH_OFFSETS 0
#else
# define SQUASH_OFFSETS 1
#endif
/* Offsets must fit into 14 bits, saving 2 bits for tags.  But since
   the minimum size of an allocation is two words (unless
   ALIGN_DOUBLES), we can squash the index array into half as much
   space as we might otherwise. For example, let **** and #### be the
   offsets for indexes 0 and 3, respectively:

     ---- ---- ---- ---- ----
    |****|    |    |####|    |  Unsquashed representation
     ---- ---- ---- ---- ----
     -- -- -- -- --
    |**|**|  |##|##|  Squashed representation
     -- -- -- -- -- 
*/

typedef unsigned short OffsetTy;
#if SQUASH_OFFSETS
typedef unsigned char OffsetArrTy;
#else
typedef unsigned short OffsetArrTy;
#endif

typedef unsigned char mtype_t;  /* object type */
typedef unsigned char mflags_t; /* mark state, etc. */

typedef struct MPage {
  mtype_t type;       /* object type */
  mflags_t flags;     /* mark state, etc. */
  short alloc_boundary;
  short compact_boundary;
  short age, refs_age, compact_to_age;
  union {
    OffsetArrTy *offsets;  /* for small objects */
    long size;             /* for one big object */
  } u;
  union {
    void **compact_to;     /* for small objects */
    void *bigblock_start;  /* for one big object */
  } o;
  void *block_start;       /* start of memory in this page */
  struct MPage *next, *prev; /* for linked list of pages */

  /* For mark-stack overflow, or slowing mark categories: */
  OffsetTy gray_start, gray_end; 
  struct MPage *gray_next;

#if KEEP_BACKPOINTERS
  void **backpointer_page;
#endif
} MPage;

/* Linked list of allocated pages: */
static MPage *first, *last;

/* For mark-stack overflow, or slowish mark categories. */
static MPage *gray_first;

/* For memory-mapping: */
MPage **mpage_maps;

/* MPage size: */
#define LOG_MPAGE_SIZE 14
#define MPAGE_SIZE (1 << LOG_MPAGE_SIZE)
#define MPAGE_WORDS (1 << (LOG_MPAGE_SIZE - LOG_WORD_SIZE))
#define MPAGE_MASK ((1 << LOG_MPAGE_SIZE) - 1)
#define MPAGE_START ~MPAGE_MASK

#define BIGBLOCK_MIN_SIZE (1 << (LOG_MPAGE_SIZE - 2))
#define FREE_LIST_ARRAY_SIZE (BIGBLOCK_MIN_SIZE >> LOG_WORD_SIZE)

/* Offset-page size: */
#define LOG_OPAGE_SIZE (LOG_MPAGE_SIZE - LOG_WORD_SIZE - SQUASH_OFFSETS)
#define OPAGE_SIZE (sizeof(OffsetTy) << LOG_OPAGE_SIZE)

/* We use a two-level table to map the universe. The MAP_SIZE is the
   size of the outer table, so LOG_MAP_SIZE is the number of high-order
   bits used to index the table. */
#define LOG_MAP_SIZE 9
#define LOG_MAPS_SIZE (WORD_BIT_COUNT - LOG_MAP_SIZE - LOG_MPAGE_SIZE)
#define MAP_SIZE (1 << LOG_MAP_SIZE)
#define MAPS_SIZE (1 << LOG_MAPS_SIZE)

/* MASK_MASK followed by MAP_SHIFT gives the 2nd-page index. */
#define MAPS_SHIFT (WORD_BIT_COUNT - LOG_MAPS_SIZE)
#define MAP_MASK ((1 << (LOG_MAP_SIZE + LOG_MPAGE_SIZE)) - 1)
#define MAP_SHIFT LOG_MPAGE_SIZE

/* Allocation (MPage) types */
#define MTYPE_NONE         0
#define MTYPE_TAGGED       1
#define MTYPE_ATOMIC       2
#define MTYPE_TAGGED_ARRAY 3
#define MTYPE_ARRAY        4
#define MTYPE_XTAGGED      5
#define MTYPE_MALLOCFREE   6

/* Allocation flags */

#define COLOR_MASK         0x3

#define MFLAG_GRAY         0x1
#define MFLAG_BLACK        0x2

#define NONCOLOR_MASK      0xFC

#define MFLAG_BIGBLOCK     0x4
#define MFLAG_CONTINUED    0x8

#define MFLAG_OLD          0x10
#define MFLAG_MODIFIED     0x20
#define MFLAG_INITED       0x40
#define MFLAG_MARK         0x80

/* Offset table manipulations */

#define OFFSET_COLOR_UNMASKED(offsets, pos) (offsets[pos])
#define OFFSET_COLOR(offsets, pos) (offsets[pos] & COLOR_MASK)
#define OFFSET_SET_COLOR_UNMASKED(offsets, pos, c) (offsets[pos] = c)

#if SQUASH_OFFSETS
# define OFFSET_HI_MASK 0xFC
# define OFFSET_LO_MASK 0xFF
# define OFFSET_HI_SHIFT 6
# define OFFSET_SIZE(offsets, pos) (((OffsetTy)(offsets[pos] & OFFSET_HI_MASK) << OFFSET_HI_SHIFT) | (offsets[(pos)+1]))
# define OFFSET_SET_SIZE_UNMASKED(offsets, pos, s) (offsets[pos] = (((s) >> OFFSET_HI_SHIFT) & OFFSET_HI_MASK), offsets[(pos)+1] = ((s) & OFFSET_LO_MASK))
#else
# define OFFSET_SHIFT 2
# define OFFSET_SIZE(offsets, pos) ((offsets[pos]) >> OFFSET_SHIFT)
# define OFFSET_SET_SIZE_UNMASKED(offsets, pos, s) (offsets[pos] = ((s) << OFFSET_SHIFT))
#endif

/* Special tags */

#define SKIP ((Type_Tag)0x7000)
#define TAGGED_EOM ((Type_Tag)0x6000)
#define UNTAGGED_EOM   (MPAGE_SIZE + 1)

/* One MSet for every type of MPage: */

typedef struct {
  void **low, **high;
  MPage *malloc_page, *compact_page;
  void **compact_to;
  OffsetTy compact_to_offset;
#if USE_FREELIST
  void *free_lists[FREE_LIST_ARRAY_SIZE];
#endif
} MSet;

#define NUM_SETS 5
#define NUM_TAGGED_SETS 1
#define NUM_NONATOMIC_SETS 4
static MSet tagged, atomic, array, tagged_array, xtagged;
static MSet *sets[NUM_SETS]; /* First one is tagged, last one is atomic */

/********************* Statistics *********************/
static long page_allocations = 0;
static long page_reservations = 0;
#define LOGICALLY_ALLOCATING_PAGES(len) (page_allocations += len)
#define ACTUALLY_ALLOCATING_PAGES(len) (page_reservations += len)
#define LOGICALLY_FREEING_PAGES(len) (page_allocations -= len)
#define ACTUALLY_FREEING_PAGES(len) (page_reservations -= len)

static long memory_in_use, gc_threshold = GROW_ADDITION, max_memory_use;
static int prev_memory_in_use, memory_use_growth;
#if USE_FREELIST
static long on_free_list;
# define FREE_LIST_DELTA (on_free_list << LOG_WORD_SIZE)
#else
# define FREE_LIST_DELTA 0
#endif

#if GENERATIONS
static int generations_available = 1;
static long num_seg_faults;
#endif

static int cycle_count = 0, compact_count = 0, gc_count = 0;
static int skipped_pages, scanned_pages, young_pages, inited_pages;

static long iterations;

#if TIME
static long mark_stackoflw;
#endif

static int fnl_weak_link_count;

static int ran_final;
static int running_finals;

/******************** Misc ********************/

/* The answer for all 0-byte requests: */
static char zero_sized[4];

/* Temporary pointer-holder used by routines that allocate */
static void *park[2];

static int during_gc, avoid_collection;

static int resolve_for_fixup = 0;

static MPage *find_page(void *p);

#if CHECKS
static void CRASH(int where)
{
  GCPRINT(GCOUTF, "crash @%d\n", where);
  GCFLUSHOUT();
#ifdef _WIN32
  DebugBreak();
#endif
  abort();
}

#if DEFINE_MALLOC_FREE
static void check_not_freed(MPage *page, const void *p);
#endif

static int just_checking, the_size;
#endif

#include "my_qsort.c"

/******************************************************************************/
/*                     OS-specific low-level allocator                        */
/******************************************************************************/

#define DONT_NEED_MAX_HEAP_SIZE
#include "vm.c"

/******************************************************************************/
/*                              client setup                                  */
/******************************************************************************/

static unsigned long stack_base;

void GC_set_stack_base(void *base)
{
  stack_base = (unsigned long)base;
}

void GC_init_type_tags(int count, int pair, int weakbox, int ephemeron, int weakarray, int custbox)
{
  pair_tag = pair;
  weak_box_tag = weakbox;
  ephemeron_tag = ephemeron;
  weak_array_tag = weakarray;
}

void GC_register_traversers(Type_Tag tag, Size_Proc size, Mark_Proc mark, Fixup_Proc fixup, 
			    int is_constant_size, int is_atomic)
{
  if (is_constant_size) {
    long v;
    v = size(NULL);
    if (v < 100)
      size = (Size_Proc)v;
  }

  size_table[tag] = size;
  mark_table[tag] = mark;
  fixup_table[tag] = fixup;
}

/******************************************************************************/
/*                               root table                                   */
/******************************************************************************/

#include "roots.c"

void GC_register_thread(void *p, void *c)
{
}
void GC_register_new_thread(void *p, void *c)
{
}

/******************************************************************************/
/*                             immobile box                                   */
/******************************************************************************/

/* The ImmobileBox struct is an internal view, only. To a GC client,
   an immobile box starts with a longword for a pointer, and the rest
   is undefined. */
typedef struct ImmobileBox {
  void *p; /* must be first in the record */
  struct ImmobileBox *next, *prev;
} ImmobileBox;

static ImmobileBox *immobile;

void **GC_malloc_immobile_box(void *p)
{
  ImmobileBox *ib;

  ib = (ImmobileBox *)malloc(sizeof(ImmobileBox));
  ib->p = p;
  ib->next = immobile;
  if (immobile)
    immobile->prev = ib;
  ib->prev = NULL;

  immobile = ib;

  return (void **)ib;
}

void GC_free_immobile_box(void **b)
{
  ImmobileBox *ib = (ImmobileBox *)b;

  if (!ib)
    return;

  if (ib->prev)
    ib->prev->next = ib->next;
  else
    immobile = ib->next;
  if (ib->next)
    ib->next->prev = ib->prev;

  free(ib);
}

/******************************************************************************/
/*                             free list element                              */
/******************************************************************************/

#if USE_FREELIST

static int size_on_free_list(void *p)
{
  return ((OffsetTy *)p)[1];
}

#endif

/******************************************************************************/
/*                           weak arrays and boxes                            */
/******************************************************************************/

static int is_marked(void *p);
#define weak_box_resolve(p) (p)

#include "weak.c"

/******************************************************************************/
/*                             finalization                                   */
/******************************************************************************/

static int is_finalizable_page(void *p)
{
  MPage *page;
  page = find_page(p);
  return page && page->type;
}

#include "fnls.c"

static Fnl *run_queue, *last_in_queue;

static void mark_finalizer(Fnl *fnl)
{
  gcMARK(fnl->next);
  gcMARK(fnl->data);
  /* !eager_level => queued for run: */
  if (!fnl->eager_level) {
    gcMARK(fnl->p);
  }
#if CHECKS
  if (!fnl->tagged && fnl->size < BIGBLOCK_MIN_SIZE) {
    if (((long *)fnl->p)[-1] != fnl->size)
      CRASH(2);
  }
#endif
}

static void fixup_finalizer(Fnl *fnl)
{
#if CHECKS
  static void *old_fnl_p;
  static MPage *old_fnl_page;
  
  old_fnl_p = fnl->p;
  old_fnl_page = find_page(fnl->p);
#endif
  
  gcFIXUP(fnl->next);
  gcFIXUP(fnl->data);
  gcFIXUP(fnl->p);

#if CHECKS
  if (!fnl->tagged && fnl->size < BIGBLOCK_MIN_SIZE) {
    if (!(((long)fnl->p) & MPAGE_MASK))
      CRASH(3);
  }
#endif
}

typedef struct Fnl_Weak_Link {
  void *p;
  int offset;
  void *saved;
  struct Fnl_Weak_Link *next;
} Fnl_Weak_Link;

static Fnl_Weak_Link *fnl_weaks;

static void mark_finalizer_weak_link(Fnl_Weak_Link *wl)
{
  gcMARK(wl->next);
}

static void fixup_finalizer_weak_link(Fnl_Weak_Link *wl)
{
  gcFIXUP(wl->next);
  gcFIXUP(wl->p);
}

void GC_finalization_weak_ptr(void **p, int offset)
{
  Fnl_Weak_Link *wl;

#if CHECKS
  if (offset < 0)
    CRASH(6);
#endif

  /* Allcation might trigger GC, so we use park: */
  park[0] = p;

  wl = (Fnl_Weak_Link *)GC_malloc_atomic(sizeof(Fnl_Weak_Link));

  p = park[0];
  park[0] = NULL;

  wl->p = p;
  wl->next = fnl_weaks;
  wl->offset = offset * sizeof(void*);

  fnl_weaks = wl;

  fnl_weak_link_count++;
}

/******************************************************************************/
/*                             alloc state info                               */
/******************************************************************************/

/* Works anytime: */
static MPage *find_page(void *p) 
{
  unsigned long g = ((unsigned long)p >> MAPS_SHIFT);
  MPage *map;

  map = mpage_maps[g];
  if (map) {
    unsigned long addr = (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);
    MPage *page;

    page = map + addr;
    return page;
  }
  
  return NULL;  
}

/* Works only during GC: */
static int is_marked(void *p)
{
  unsigned long g = ((unsigned long)p >> MAPS_SHIFT);
  MPage *map;

  map = mpage_maps[g];
  if (map) {
    MPage *page;

    page = map + (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);
#if DEFINE_MALLOC_FREE
    if (page->type == MTYPE_MALLOCFREE)
      return 1;
#endif
    if (page->flags & MFLAG_BIGBLOCK) {
      if (page->flags & MFLAG_CONTINUED)
	return is_marked(page->o.bigblock_start);
      else
	return (page->flags & (COLOR_MASK | MFLAG_OLD));
    } else {
      if (page->flags & MFLAG_OLD)
	return 1;
      else if (page->flags & COLOR_MASK) {
	long offset = ((long)p & MPAGE_MASK) >> LOG_WORD_SIZE;

 	if (page->type > MTYPE_TAGGED)
 	  offset -= 1;

	return OFFSET_COLOR(page->u.offsets, offset);
      } else if ((long)p & 0x1)
	return 1;
      else
	return 0;
    }
  }
  
  return 1;
}

#if SEARCH
void *search_for, *search_mark = 0x7;
long search_size;

void stop()
{
  GCPRINT(GCOUTF, "stopped\n");
}
#endif

/******************************************************************************/
/*                               init phase                                   */
/******************************************************************************/

/* Init: set color to white and install offsets (to indicate the
   offset to the start of and allocation block) for marking. */

#if CHECKS
static void **prev_ptr, **prev_prev_ptr, **prev_prev_prev_ptr;
static void **prev_prev_prev_prev_ptr, **prev_prev_prev_prev_prev_ptr;
static void **prev_var_stack;
#endif

static void init_tagged_mpage(void **p, MPage *page)
{
  OffsetTy offset = 0;
  OffsetArrTy *offsets;
  void **top;

  page->flags = (page->flags & NONCOLOR_MASK);
  offsets = page->u.offsets;
  top = p + MPAGE_WORDS;

  page->alloc_boundary = MPAGE_WORDS;
  
  while (p < top) {
    Type_Tag tag;
    long size;

    tag = *(Type_Tag *)p;

    if (tag == TAGGED_EOM) {
      /* Remember empty space for prop and compact:  */
      page->alloc_boundary = offset;
      break;
    }

#if ALIGN_DOUBLES
    if (tag == SKIP) {
      OFFSET_SET_SIZE_UNMASKED(offsets, offset, 1);
      offset++;
      p++;
    } else {
#endif

#if CHECKS
      if ((tag < 0) || (tag >= _num_tags_) || !size_table[tag]) {
	GCPRINT(GCOUTF, "bad tag: %d at %lx\n", tag, (long)p);
	GCFLUSHOUT();
	CRASH(7);
      }
      prev_prev_prev_prev_prev_ptr = prev_prev_prev_prev_ptr;
      prev_prev_prev_prev_ptr = prev_prev_prev_ptr;
      prev_prev_prev_ptr = prev_prev_ptr;
      prev_prev_ptr = prev_ptr;
      prev_ptr = p;
      prev_var_stack = GC_variable_stack;
#endif

      {
	Size_Proc size_proc;
	
	size_proc = size_table[tag];
	if (((long)size_proc) < 100)
	  size = (long)size_proc;
	else
	  size = size_proc(p);
      }

#if CHECKS
      if (size < 1) {
	CRASH(57);
      }
#endif

      OFFSET_SET_SIZE_UNMASKED(offsets, offset, size);
      offset += size;

#if CHECKS
      if (prev_var_stack != GC_variable_stack) {
	CRASH(8);
      }
#endif
      
      p += size;
#if ALIGN_DOUBLES
    }
#endif
  }

  inited_pages++;
}

static void init_untagged_mpage(void **p, MPage *page)
{
  OffsetTy offset = 0;
  OffsetArrTy *offsets;
  void **top;

  page->flags = (page->flags & NONCOLOR_MASK);
  offsets = page->u.offsets;
  page->alloc_boundary = MPAGE_WORDS;

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;

    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM) {
      /* Remember empty space for prop:  */
      page->alloc_boundary = offset;
      
      break;
    }

#if CHECKS
    if (0 && page->type == MTYPE_XTAGGED) {
      just_checking = 1;
      GC_mark_xtagged(p + 1);
      just_checking = 0;
    }

    the_size = size;
#endif

    OFFSET_SET_SIZE_UNMASKED(offsets, offset, 0);
    offset += size;

    p += size;
  } 

  inited_pages++;
}

static void init_all_mpages(int young)
{
  MPage *page;

  for (page = first; page; page = page->next) {
    int is_old = (page->age > young);
#if GENERATIONS
    void *p = page->block_start;
#endif
	
    if (!is_old && !(page->flags & MFLAG_MODIFIED)) {
#if GENERATIONS
      if (generations_available) {
	if (page->flags & MFLAG_BIGBLOCK)
	  protect_pages((void *)p, page->u.size, 1);
	else
	  protect_pages((void *)p, MPAGE_SIZE, 1);
      }
#endif
      page->flags |= MFLAG_MODIFIED;
    }

    if (is_old) {
      page->flags -= (page->flags & MFLAG_MARK);
      page->flags |= MFLAG_OLD;
    } else {
      page->flags -= (page->flags & MFLAG_OLD);
      page->flags |= MFLAG_MARK;
      young_pages++;
    }
      
    if (!(page->flags & MFLAG_INITED)) {
      void *p = page->block_start;

      if (page->flags & MFLAG_BIGBLOCK) {
	page->flags = (page->flags & NONCOLOR_MASK);
	page->flags |= MFLAG_INITED;
      } else {
	if (is_old) {
	  if (page->type <= MTYPE_TAGGED)
	    init_tagged_mpage((void **)p, page);
	  else
	    init_untagged_mpage((void **)p, page);
	  page->flags |= MFLAG_INITED;
	} else {
	  /* Young pages: initialize lazily as needed by `mark'.
	     Not initialized means full page is garbage. */
	  page->flags = (page->flags & NONCOLOR_MASK);
	}

	if (is_old) {
	  skipped_pages++;
	}
      }
    } else {
       if (is_old) 
	 skipped_pages++;
      /* Clear color flags: */
      page->flags = (page->flags & NONCOLOR_MASK);
    }

    if (is_old
	&& ((page->refs_age <= young)
	    || (page->flags & MFLAG_MODIFIED))
	&& (page->type != MTYPE_ATOMIC)) {
      /* Offsets inited; need to set gray flag */
      page->flags |= MFLAG_GRAY;
      
      page->gray_next = gray_first;
      gray_first = page;
      
      page->gray_start = 0;
      page->gray_end = page->alloc_boundary - 2;

      if (!(page->flags & MFLAG_MODIFIED)) {
#if GENERATIONS
	if (generations_available) {
	  if (page->flags & MFLAG_BIGBLOCK)
	    protect_pages((void *)p, page->u.size, 1);
	  else
	    protect_pages((void *)p, MPAGE_SIZE, 1);
	}
#endif
	page->flags |= MFLAG_MODIFIED;
      }

      scanned_pages++;
    }
  }
}

/******************************************************************************/
/*                               mark phase                                   */
/******************************************************************************/

/* Mark: mark a block as reachable. */

#if MARK_STATS
long mark_calls, mark_hits, mark_recalls, mark_colors, mark_many, mark_slow;
#endif

void GC_mark(const void *p)
{
  unsigned long g;
  MPage *map;

#if CHECKS
  if (just_checking) {
    return;
  }
#endif
#if MARK_STATS
  mark_calls++;
#endif

  if ((long)p & 0x1) return;
  g = ((unsigned long)p >> MAPS_SHIFT);

  map = mpage_maps[g];
  if (map) {
    MPage *page;
    mflags_t flags;

    page = map + (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);

#if SEARCH
    if (p == search_mark) {
      stop();
    }
#endif

#if DEFINE_MALLOC_FREE
    if (page->type == MTYPE_MALLOCFREE) {
#if CHECKS
      check_not_freed(page, p);
#endif
      return;
    }
#endif

    flags = page->flags;
    if (flags & (MFLAG_MARK | MFLAG_CONTINUED)) {
#if MARK_STATS
      mark_hits++;
#endif

      if (flags & MFLAG_BIGBLOCK) {
	if (flags & MFLAG_CONTINUED) {
	  void *p2;
	  unsigned long g2;
#if MARK_STATS
	  mark_recalls++;
#endif
	  p2 = page->o.bigblock_start;
	  g2 = ((unsigned long)p2 >> MAPS_SHIFT);
	  page = mpage_maps[g2] + (((unsigned long)p2 & MAP_MASK) >> MAP_SHIFT);
	  flags = page->flags;

	  if (!(flags & MFLAG_MARK))
	    return;
	}

	if (!(flags & COLOR_MASK)) {
#if MARK_STATS
	  mark_colors++;
#endif
	  page->flags = (flags | MFLAG_GRAY);
	  
	  if (page->type != MTYPE_ATOMIC) {
	    page->gray_next = gray_first;
	    gray_first = page;
	  }

#if KEEP_BACKPOINTERS
	  page->backpointer_page = mark_src;
#endif
	}
      } else {
	long offset;
	OffsetArrTy v;
	mtype_t type;
	
	type = page->type;

	/* Check for lazy initialization: */
	if (!(flags & MFLAG_INITED)) {
	  if (type <= MTYPE_TAGGED)
	    init_tagged_mpage((void **)page->block_start, page);
	  else
	    init_untagged_mpage((void **)page->block_start, page);
	  flags |= MFLAG_INITED;
	  page->flags = flags;
	}

	if (type > MTYPE_TAGGED) {
#if CHECKS
	  if (!((long)p & MPAGE_MASK)) {
	    /* Can't point to beginning of non-tagged block! */
	    CRASH(9);
	  }
#endif
	  p = BYTEPTR(p) - WORD_SIZE;
	}

	offset = ((long)p & MPAGE_MASK) >> LOG_WORD_SIZE;

#if CHECKS
	if (offset >= page->alloc_boundary) {
	  /* Past allocation region. */
	  CRASH(10);
	}
#endif

	v = OFFSET_COLOR_UNMASKED(page->u.offsets, offset);
	if (!(v & COLOR_MASK)) {
#if MARK_STATS
	  mark_colors++;
#endif

	  switch(type) {
	  case MTYPE_ATOMIC:
	    OFFSET_SET_COLOR_UNMASKED(page->u.offsets, offset, v | MFLAG_BLACK);
	    if (!(flags & MFLAG_BLACK)) {
	      page->flags = (flags | MFLAG_BLACK);
	    }
#if KEEP_BACKPOINTERS
	    page->backpointer_page[offset] = mark_src;
#endif
	    break;
	  case MTYPE_TAGGED:
#if CHECKS
	    {
	      Type_Tag tag = *(Type_Tag *)p;
	      if ((tag < 0) || (tag >= _num_tags_) || !size_table[tag]) {
		GCPRINT(GCOUTF, "bad tag: %d at %lx\n", tag, (long)p);
		CRASH(11);
	      }
	    }
#endif
	  case MTYPE_XTAGGED:
	  case MTYPE_ARRAY:
	    if (mark_stack_pos < MARK_STACK_MAX) {
	      page->flags = (flags | MFLAG_BLACK);
	      OFFSET_SET_COLOR_UNMASKED(page->u.offsets, offset, v | MFLAG_BLACK); /* black can mean on stack */
# if RECORD_MARK_SRC
#  if CHECKS
	      if ((long)mark_src & 0x1) CRASH(12);
#  endif
	      mark_src_stack[mark_stack_pos] = mark_src;
	      mark_src_type[mark_stack_pos] = mark_type;
# endif
	      mark_stack[mark_stack_pos] = (void *)p;
	      mark_stack_type[mark_stack_pos++] = type;
#if KEEP_BACKPOINTERS
	      page->backpointer_page[offset] = mark_src;
#endif
	      break;
	    }
	  default: /* ^^^ fallthrough */
	    OFFSET_SET_COLOR_UNMASKED(page->u.offsets, offset, v | MFLAG_GRAY);
#if TIME
	    mark_stackoflw++;
#endif
#if KEEP_BACKPOINTERS
	    page->backpointer_page[offset] = mark_src;
#endif
	    if (!(flags & MFLAG_GRAY)) {
	      page->flags = (flags | MFLAG_GRAY);
	      
	      page->gray_next = gray_first;
	      gray_first = page;
	      
	      page->gray_start = offset;
	      page->gray_end = offset;
	    } else {
	      if (page->gray_start > offset)
		page->gray_start = offset;
	      if (page->gray_end < offset)
		page->gray_end = offset;
	    }
	  }
	} else {
#if CHECKS
	  if (!(flags & COLOR_MASK)) {
	    CRASH(13);
	  }
#endif
	}
      }
    }
  }
}

/******************************************************************************/
/*                               prop phase                                   */
/******************************************************************************/

/* Propoagate: for each marked object, mark objects it
   reaches... until fixpoint. */

static void propagate_tagged_mpage(void **bottom, MPage *page)
{
  OffsetTy offset;
  OffsetArrTy *offsets;
  void **p, **graytop;

  offsets = page->u.offsets;

  offset = page->gray_start;
  p = bottom + offset;
  graytop = bottom + page->gray_end;
  
  while (p <= graytop) {
    OffsetArrTy v;
    Type_Tag tag;
    long size;
    
    tag = *(Type_Tag *)p;

#if ALIGN_DOUBLES
    if (tag != SKIP) {
#endif

#if RECORD_MARK_SRC
      mark_src = p;
      mark_type = MTYPE_TAGGED;
#endif
      
      v = OFFSET_COLOR_UNMASKED(offsets, offset);
      size = OFFSET_SIZE(offsets, offset);
      if (v & MFLAG_GRAY) {
	v -= MFLAG_GRAY;
	v |= MFLAG_BLACK;
	OFFSET_SET_COLOR_UNMASKED(offsets, offset, v);
	mark_table[tag](p);
      }
	
#if ALIGN_DOUBLES
    } else
      size = 1;
#endif

    p += size;
    offset += size;
  }

#if MARK_STATS
  mark_many++;
#endif

#if MARK_STATS
  if (page->flags & MFLAG_GRAY) {
    mark_slow++;
  }
#endif
}

static void propagate_tagged_whole_mpage(void **p, MPage *page)
{
  void **top;

  top = p + MPAGE_WORDS;
  
  while (p < top) {
    Type_Tag tag;
    long size;

    tag = *(Type_Tag *)p;

    if (tag == TAGGED_EOM) {
      break;
    }

#if ALIGN_DOUBLES
    if (tag == SKIP) {
      p++;
    } else {
#endif

#if RECORD_MARK_SRC
      mark_src = p;
      mark_type = MTYPE_TAGGED;
#endif

      size = mark_table[tag](p);

      p += size;

#if ALIGN_DOUBLES
    }
#endif
  }
}

static void propagate_array_mpage(void **bottom, MPage *page)
{
  OffsetTy offset;
  OffsetArrTy *offsets;
  void **p, **top;

  offset = page->gray_start;
  p = bottom + offset;
  top = bottom + page->gray_end;
  offsets = page->u.offsets;

  while (p <= top) {
    OffsetArrTy v;
    long size;

    size = *(long *)p + 1;
	
#if CHECKS
    if ((size < 2) || (size > MPAGE_WORDS)) {
      CRASH(14);
    }
    prev_ptr = p;
#endif

    v = OFFSET_COLOR_UNMASKED(offsets, offset);
    if (v & MFLAG_GRAY) {
      int i;

#if RECORD_MARK_SRC
      mark_src = p + 1;
      mark_type = MTYPE_ARRAY;
#endif

      v -= MFLAG_GRAY;
      v |= MFLAG_BLACK;
      OFFSET_SET_COLOR_UNMASKED(offsets, offset, v);
      
      for (i = 1; i < size; i++) {
	gcMARK(p[i]);
      }
    }
    
    p += size;
    offset += size;

#if CHECKS
    if ((p > bottom + MPAGE_WORDS + 1) || (p < bottom)) {
      CRASH(15);
    }
#endif
  }
}

static void propagate_array_whole_mpage(void **p, MPage *page)
{
  void **top;

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size, i;

    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM) {
      break;
    }

#if RECORD_MARK_SRC
    mark_src = p + 1;
    mark_type = MTYPE_ARRAY;
#endif

    for (i = 1; i < size; i++) {
      gcMARK(p[i]);
    }

    p += size;
  } 
}

static void propagate_tagged_array_mpage(void **bottom, MPage *page)
{
  OffsetTy offset;
  OffsetArrTy *offsets;
  void **p, **top;

  offset = page->gray_start;
  p = bottom + offset;
  top = bottom + page->gray_end;
  offsets = page->u.offsets;

  while (p <= top) {
    OffsetArrTy v;
    int size;
    
    size = *(long *)p + 1;

    v = OFFSET_COLOR_UNMASKED(offsets, offset);
    if (v & MFLAG_GRAY) {
      v -= MFLAG_GRAY;
      v |= MFLAG_BLACK;
      OFFSET_SET_COLOR_UNMASKED(offsets, offset, v);

      {
	int i, elem_size;
	void **mp = p + 1;
	Type_Tag tag;
	Mark_Proc traverse;
	
#if RECORD_MARK_SRC
	mark_src = mp;
	mark_type = MTYPE_TAGGED_ARRAY;
#endif

	size--;
	tag = *(Type_Tag *)mp;

	traverse = mark_table[tag];
	elem_size = traverse(mp);
	mp += elem_size;
	for (i = elem_size; i < size; i += elem_size, mp += elem_size)
	  traverse(mp);

	size++;
      }
    }
    
    p += size;
    offset += size;
  }
}

static void propagate_tagged_array_whole_mpage(void **p, MPage *page)
{
  void **top;

  top = p + MPAGE_WORDS;

  while (p < top) {
    int i, elem_size, size;
    void **mp;
    Type_Tag tag;
    Mark_Proc traverse;
    
    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM)
      break;
      
    mp = p + 1;
    p += size;
    size--;

    tag = *(Type_Tag *)mp;
      
#if RECORD_MARK_SRC
    mark_src = mp;
    mark_type = MTYPE_TAGGED_ARRAY;
#endif

    traverse = mark_table[tag];
    elem_size = traverse(mp);
    mp += elem_size;
    for (i = elem_size; i < size; i += elem_size, mp += elem_size)
      traverse(mp);
  }
}

static void propagate_xtagged_mpage(void **bottom, MPage *page)
{
  OffsetTy offset;
  OffsetArrTy *offsets;
  void **p, **top;

  offset = page->gray_start;
  p = bottom + offset;
  top = bottom + page->gray_end;
  offsets = page->u.offsets;

  while (p <= top) {
    OffsetArrTy v;
    long size;

    size = *(long *)p + 1;
	
#if ALIGN_DOUBLES
    if (size > 1) {
#endif

      v = OFFSET_COLOR_UNMASKED(offsets, offset);
      if (v & MFLAG_GRAY) {
	v -= MFLAG_GRAY;
	v |= MFLAG_BLACK;
	OFFSET_SET_COLOR_UNMASKED(offsets, offset, v);
	
#if RECORD_MARK_SRC
	mark_src = p + 1;
	mark_type = MTYPE_XTAGGED;
#endif

	GC_mark_xtagged(p + 1);
      }

#if ALIGN_DOUBLES
    }
#endif
    
    p += size;
    offset += size;
  }
}

static void propagate_xtagged_whole_mpage(void **p, MPage *page)
{
  void **top;

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;

    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM) {
      break;
    }

#if RECORD_MARK_SRC
    mark_src = p + 1;
    mark_type = MTYPE_XTAGGED;
#endif

#if ALIGN_DOUBLES
    if (size > 1) {
#endif

      GC_mark_xtagged(p + 1);

#if ALIGN_DOUBLES
    }
#endif

    p += size;
  } 
}

static void do_bigblock(void **p, MPage *page, int fixup)
{
  switch (page->type) {
  case MTYPE_ATOMIC:
    return;

  case MTYPE_TAGGED:
    {
      Type_Tag tag;

      tag = *(Type_Tag *)p;

#if CHECKS
      if ((tag < 0) || (tag >= _num_tags_) || !size_table[tag]) {
	CRASH(16);
      }
      prev_var_stack = GC_variable_stack;
#endif
#if RECORD_MARK_SRC
      mark_src = p;
      mark_type = MTYPE_TAGGED;
#endif

      if (fixup)
	fixup_table[tag](p);
      else
	mark_table[tag](p);

#if CHECKS
      if (prev_var_stack != GC_variable_stack) {
	CRASH(17);
      }
#endif
    
      return;
    }

  case MTYPE_TAGGED_ARRAY:
    {
      int i, elem_size, size;
      void **mp = p;
      Type_Tag tag;
      Mark_Proc mark;

      size = page->u.size >> LOG_WORD_SIZE;
      tag = *(Type_Tag *)mp;

#if RECORD_MARK_SRC
      mark_src = mp;
      mark_type = MTYPE_TAGGED_ARRAY;
#endif

      if (fixup)
	mark = fixup_table[tag];
      else
	mark = mark_table[tag];
      elem_size = mark(mp);      
      mp += elem_size;
      for (i = elem_size; i < size; i += elem_size, mp += elem_size)
	mark(mp);

      return;
    }

  case MTYPE_ARRAY:
    {
      int i;
      long size = page->u.size >> LOG_WORD_SIZE;
    
      if (fixup) {
	for (i = 0; i < size; i++, p++) {
	  if (*p)
	    gcFIXUP(*p);
	}
      } else {
#if RECORD_MARK_SRC
	mark_src = p;
	mark_type = MTYPE_ARRAY;
#endif
	for (i = 0; i < size; i++, p++) {
	  if (*p)
	    gcMARK(*p);
	}
      }

      return;
    }

  case MTYPE_XTAGGED:
  default:
#if RECORD_MARK_SRC
    mark_src = p;
    mark_type = MTYPE_XTAGGED;
#endif
   if (fixup)
     GC_fixup_xtagged(p);
   else
     GC_mark_xtagged(p);
   return;
  }
}

static int old_tag;
static void *old_p;

static void propagate_all_mpages()
{
  MPage *page;
  void *p;
  
  while (gray_first || mark_stack_pos) {
    iterations++;

    while (mark_stack_pos) {
      mtype_t type;
      
      p = mark_stack[--mark_stack_pos];
      type = mark_stack_type[mark_stack_pos];
# if RECORD_MARK_SRC
      current_mark_src = mark_src_stack[mark_stack_pos];
      current_mark_type = mark_src_type[mark_stack_pos];
# endif

      switch (type) {
      case MTYPE_TAGGED:
	{
	  Type_Tag tag;
	  tag = *(Type_Tag *)p;
	
#if ALIGN_DOUBLES
	  if (tag != SKIP) {
#endif
	  
#if CHECKS
	    if ((tag < 0) || (tag >= _num_tags_) || !size_table[tag]) {
	      CRASH(18);
	    }
#endif
#if RECORD_MARK_SRC
	    mark_src = p;
	    mark_type = MTYPE_TAGGED;
#endif

	    old_tag = tag;
	    old_p = p;
	    mark_table[tag](p);
	  
#if ALIGN_DOUBLES
	  }
#endif
	}
	break;

      case MTYPE_XTAGGED:
#if RECORD_MARK_SRC
	mark_src = (void **)p + 1;
	mark_type = MTYPE_XTAGGED;
#endif
	GC_mark_xtagged((void **)p + 1);
	break;

      default: /* MTYPE_ARRAY */
	{
	  long size, i;
	  
	  size = ((long *)p)[0];
	  
#if RECORD_MARK_SRC
	  mark_src = (void **)p + 1;
	  mark_type = MTYPE_ARRAY;
#endif

	  for (i = 1; i <= size; i++) {
	    gcMARK(((void **)p)[i]);
	  }
	}
      }
    }

    if (gray_first) {
      page = gray_first;
      gray_first = page->gray_next;
      
      page->flags = ((page->flags & NONCOLOR_MASK) | MFLAG_BLACK);
      p = page->block_start;
      
      if (page->flags & MFLAG_BIGBLOCK) {
	if (!(page->flags & MFLAG_CONTINUED))
	  do_bigblock((void **)p, page, 0);
      } else {
	switch (page->type) {
	case MTYPE_ATOMIC:
	  break;
	case MTYPE_TAGGED:
	  if (page->flags & MFLAG_OLD)
	    propagate_tagged_whole_mpage((void **)p, page);
	  else
	    propagate_tagged_mpage((void **)p, page);
	  break;
	case MTYPE_TAGGED_ARRAY:
	  if (page->flags & MFLAG_OLD)
	    propagate_tagged_array_whole_mpage((void **)p, page);
	  else
	    propagate_tagged_array_mpage((void **)p, page);
	  break;
	case MTYPE_XTAGGED:
	  if (page->flags & MFLAG_OLD)
	    propagate_xtagged_whole_mpage((void **)p, page);
	  else
	    propagate_xtagged_mpage((void **)p, page);
	  break;
	case MTYPE_ARRAY:
	default:
	  if (page->flags & MFLAG_OLD)
	    propagate_array_whole_mpage((void **)p, page);
	  else
	    propagate_array_mpage((void **)p, page);
	  break;
	}
      }
    }      
  }
}

/******************************************************************************/
/*                             compact phase                                  */
/******************************************************************************/

/* Compact: compact objects, setting page color to white if all
   objects are moved elsewhere */

static void compact_tagged_mpage(void **p, MPage *page)
{
  int to_near = 0, set_age = 0;
  OffsetTy offset, dest_offset, dest_start_offset;
  OffsetArrTy *offsets;
  void **dest, **startp;
  void **top;
  MSet *set;

  offsets = page->u.offsets;

  top = p + page->alloc_boundary;

  startp = p;
  switch (page->type) {
  case MTYPE_TAGGED:
  default:
    set = &tagged;
    break;
  }
  dest = set->compact_to;
  dest_start_offset = set->compact_to_offset;
  dest_offset = dest_start_offset;
  offset = 0;

  page->o.compact_to = dest;
  page->compact_boundary = MPAGE_WORDS;
  
  while (p < top) {
    long size;

    size = OFFSET_SIZE(offsets, offset);

#if CHECKS
    if (!size) {
      CRASH(19);
    }
    prev_prev_prev_ptr = prev_prev_ptr;
    prev_prev_ptr = prev_ptr;
    prev_ptr = p;
#endif

    if (OFFSET_COLOR(offsets, offset)) {
#if ALIGN_DOUBLES
#define PLUS_ALIGNMENT + alignment
      long alignment;
      if (!(size & 0x1) && (dest_offset & 0x1))
	alignment = 1;
      else
	alignment = 0;
#else
# define PLUS_ALIGNMENT /**/      
#endif
      
      if (dest_offset + size PLUS_ALIGNMENT > MPAGE_WORDS) {
	/* Set end of allocation area in previous page: */
	if (dest_offset < MPAGE_WORDS)
	  *(Type_Tag *)(dest + dest_offset) = TAGGED_EOM;
	
#if NOISY
	GCPRINT(GCOUTF, "t: %lx [0,%d] -> %lx [%d,%d]\n", 
		(long)startp, offset,
		(long)dest, dest_start_offset, dest_offset);
#endif

	dest_offset = 0;
	dest = startp;
	to_near = 1;
	if (set_age) {
	  page->compact_boundary = offset;
	  set->compact_page->age = page->age;
	  set->compact_page->refs_age = page->age;
	} else
	  /* Haven't moved anything; set boundary to 0 to indicate this */
	  page->compact_boundary = 0;
      } else {
	set_age = 1;
#if ALIGN_DOUBLES
	if (alignment) {
	  *(Type_Tag *)(dest + dest_offset) = SKIP;
	  dest_offset++;
	}
#endif
      }
      
      if (!to_near || (dest_offset != offset)) {
	memmove(dest + dest_offset, p, size << LOG_WORD_SIZE);
#if KEEP_BACKPOINTERS
	if (to_near)
	  page->backpointer_page[dest_offset] = page->backpointer_page[offset];
	else
	  set->compact_page->backpointer_page[dest_offset] = page->backpointer_page[offset];
#endif
      }
      
      OFFSET_SET_SIZE_UNMASKED(offsets, offset, dest_offset);
      offset += size;
      dest_offset += size;
 
      p += size;
    } else {
      p += size;
      offset += size;
    }
  }

  if (to_near)
    set->compact_page = page;
  set->compact_to = dest;
  set->compact_to_offset = dest_offset;

  if (!to_near) {
    /* Nothing left in here. Reset color to white: */
    page->flags = (page->flags & NONCOLOR_MASK);
#if NOISY
    GCPRINT(GCOUTF, "t: %lx [all=%d] -> %lx [%d,%d]\n", 
	    (long)startp, offset,
	    (long)dest, dest_start_offset, dest_offset);
#endif
  }
}

static void compact_untagged_mpage(void **p, MPage *page)
{
  int to_near = 0, set_age = 0;
  OffsetTy offset = 0, dest_offset;
  OffsetArrTy *offsets;
  void **dest, **startp, **top;
  MSet *set;

  offsets = page->u.offsets;

  startp = p;
  switch (page->type) {
  case MTYPE_TAGGED_ARRAY:
    set = &tagged_array;
    break;
  case MTYPE_ATOMIC:
    set = &atomic;
    break;
  case MTYPE_XTAGGED:
    set = &xtagged;
    break;
  default:
    set = &array;
    break;
  }

  dest = set->compact_to;
  dest_offset = set->compact_to_offset;

  page->o.compact_to = dest;
  page->compact_boundary = MPAGE_WORDS;

  top = p + MPAGE_WORDS;

#if CHECKS
  if (dest == startp) {
    if (dest_offset < MPAGE_WORDS) {
      /* Can't compact to self! */
      CRASH(20);
    }
  }
#endif

  while (p < top) {
    long size;
      
    size = *(long *)p + 1;
    
    if (size == UNTAGGED_EOM) {
#if CHECKS
      if (p < startp + page->alloc_boundary) {
	/* Premature end */
	CRASH(21);
      }
#endif
      break;
    }

#if CHECKS
    if (size >= BIGBLOCK_MIN_SIZE) {
      CRASH(22);
    }
#endif

    if (OFFSET_COLOR(offsets, offset)) {
#if ALIGN_DOUBLES
      long alignment;
      if ((size & 0x1) && !(dest_offset & 0x1))
	alignment = 1;
      else
	alignment = 0;
#endif
	
      if ((long)dest_offset + size PLUS_ALIGNMENT > MPAGE_WORDS) {
	/* Set end of allocation area in previous page: */
	if (dest_offset < MPAGE_WORDS)
	  *(long *)(dest + dest_offset) = UNTAGGED_EOM - 1;
	
#if NOISY
	GCPRINT(GCOUTF, "u: %lx -> %lx [%d]\n", (long)startp, (long)dest, offset);
#endif

	dest_offset = 0;
	dest = startp;
	to_near = 1;
#if ALIGN_DOUBLES
	if (size & 0x1) {
	  dest[0] = 0;
	  dest_offset++;
	}
#endif

	if (set_age) {
	  page->compact_boundary = offset;
	  set->compact_page->age = page->age;
	  set->compact_page->refs_age = page->age;
	} else
	  /* Haven't moved anything; set boundary to 0 to indicate this */
	  page->compact_boundary = 0;
      } else {
	set_age = 1;
#if ALIGN_DOUBLES
	if (alignment) {
	  dest[dest_offset] = 0;
	  dest_offset++;
	}
#endif
      }

      if (!to_near || (dest_offset != offset)) {
	memmove(dest + dest_offset, p, size << LOG_WORD_SIZE);
#if KEEP_BACKPOINTERS
	if (to_near)
	  page->backpointer_page[dest_offset] = page->backpointer_page[offset];
	else
	  set->compact_page->backpointer_page[dest_offset] = page->backpointer_page[offset];
#endif
      }
      
      OFFSET_SET_SIZE_UNMASKED(offsets, offset, dest_offset+1);
#if CHECKS
      if (!offsets[offset] && !offsets[offset+1])
	CRASH(23);
#endif
      offset += size;
      dest_offset += size;
 
      p += size;
    } else {
      p += size;
      offset += size;
    }
  }

  set->compact_to = dest;
  set->compact_to_offset = dest_offset;
  if (to_near)
    set->compact_page = page;

  if (!to_near) {
    /* Nothing left in here. Reset color to white: */
    page->flags = (page->flags & NONCOLOR_MASK);
#if NOISY
    GCPRINT(GCOUTF, "u: %lx -> %lx [all]\n", (long)startp, (long)dest);
#endif
  }
}

static void compact_all_mpages()
{
  MPage *page;
  int i;

  for (i = 0; i < NUM_SETS; i++)
    sets[i]->compact_to_offset = MPAGE_WORDS;

  for (page = first; page; page = page->next) {
    if (!(page->flags & (MFLAG_BIGBLOCK | MFLAG_OLD))) {
      if (page->flags & COLOR_MASK) {
	void *p;
	
	page->flags -= (page->flags & MFLAG_INITED);
	p = page->block_start;
	
	if (page->type <= MTYPE_TAGGED)
	  compact_tagged_mpage((void **)p, page);
	else
	  compact_untagged_mpage((void **)p, page);
      } else {
	/* Set compact_boundar to 0 to indicate no moves: */
	page->compact_boundary = 0;
#if NOISY
	GCPRINT(GCOUTF, "x: %lx\n", (long)page->block_start);
#endif
      }
    }
  }

  for (i = 0; i < NUM_TAGGED_SETS; i++) {
    if (sets[i]->compact_to_offset < MPAGE_WORDS)
      *(Type_Tag *)(sets[i]->compact_to + sets[i]->compact_to_offset) = TAGGED_EOM;
  }
  for (i = NUM_TAGGED_SETS; i < NUM_SETS; i++) {
    if (sets[i]->compact_to_offset < MPAGE_WORDS)
      *(long *)(sets[i]->compact_to + sets[i]->compact_to_offset) = UNTAGGED_EOM - 1;
  }
}

/******************************************************************************/
/*                             freelist phase                                 */
/******************************************************************************/

/* Freelist: put unmarked blocks onto the free list */

#if USE_FREELIST

static void freelist_tagged_mpage(void **p, MPage *page)
{
  OffsetTy offset;
  OffsetArrTy *offsets;
  void **top;
  void **free_lists;
  long on_at_start = on_free_list;

  offsets = page->u.offsets;

  top = p + page->alloc_boundary;

  offset = 0;

  switch (page->type) {
  case MTYPE_TAGGED:
  default:
    free_lists = tagged.free_lists;
    break;
  }

  while (p < top) {
    long size;
    OffsetArrTy v;

    size = OFFSET_SIZE(offsets, offset);

    v = OFFSET_COLOR_UNMASKED(offsets, offset);
    if (!(v & COLOR_MASK)) {
#if ALIGN_DOUBLES
      if (size > 1) {
#endif
	/* HACK! This relies on both Type_Tag and OffsetTy being `short' */
	((Type_Tag *)p)[0] = gc_on_free_list_tag;
	((Type_Tag *)p)[1] = size;
	p[1] = free_lists[size];
	free_lists[size] = (void *)p;
	on_free_list += size;
#if ALIGN_DOUBLES
      }
#endif
    } else {
      /* Remove color: */
      v -= (v & (MFLAG_GRAY | MFLAG_BLACK));
      OFFSET_SET_COLOR_UNMASKED(offsets, offset, v);
    }

    p += size;
    offset += size;
  }

  if (on_at_start != on_free_list)
    page->age = page->refs_age = -1;  /* will be promoted to 0 */
}

static void freelist_untagged_mpage(void **p, MPage *page)
{
  OffsetTy offset = 0;
  OffsetArrTy *offsets;
  void **free_lists, **top;
  long on_at_start = on_free_list;

  switch (page->type) {
  case MTYPE_TAGGED_ARRAY:
    free_lists = tagged_array.free_lists;
    break;
  case MTYPE_ATOMIC:
    free_lists = atomic.free_lists;
    break;
  case MTYPE_XTAGGED:
    free_lists = xtagged.free_lists;
    break;
  default:
    free_lists = array.free_lists;
    break;
  }

  offsets = page->u.offsets;
  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;
    OffsetArrTy v;

    size = *(long *)p + 1;
    
    if (size == UNTAGGED_EOM)
      break;

#if CHECKS
    if (size >= BIGBLOCK_MIN_SIZE) {
      CRASH(24);
    }
#endif

    v = OFFSET_COLOR_UNMASKED(offsets, offset);
    if (!(v & COLOR_MASK)) {
#if ALIGN_DOUBLES
      if (size > 1) {
#endif
	p[1] = free_lists[size-1];
	free_lists[size-1] = (void *)(p + 1);
	on_free_list += (size-1);
#if ALIGN_DOUBLES
      }
#endif
    } else {
      /* Remove color: */
      v -= (v & (MFLAG_GRAY | MFLAG_BLACK));
      OFFSET_SET_COLOR_UNMASKED(offsets, offset, v);
    }
     
    p += size;
    offset += size;
  }

  if (on_at_start != on_free_list)
    page->age = page->refs_age = -1; /* will be promoted to 0 */
}

static void freelist_all_mpages(int young)
{
  MPage *page;

  for (page = first; page; page = page->next) {
    if (page->flags & COLOR_MASK) {
      if (page->refs_age <= young)
	page->refs_age = -1; /* best we can assume */
      if (!(page->flags & (MFLAG_BIGBLOCK | MFLAG_OLD))) {
	void *p;
	
	p = page->block_start;
	
	if (page->type <= MTYPE_TAGGED)
	  freelist_tagged_mpage((void **)p, page);
	else
	  freelist_untagged_mpage((void **)p, page);
      }
    }
  }
}

#endif

/******************************************************************************/
/*                              fixup phase                                   */
/******************************************************************************/

/* Fixup: translate an old address to a new one, and note age of
   youngest referenced page */

static int min_referenced_page_age;
#if CHECKS
static void *bad_dest_addr;
#endif

void GC_fixup(void *pp)
{
  void *p = *(void **)pp;
  unsigned long g;
  MPage *map;

  if ((long)p & 0x1) return;
  g = ((unsigned long)p >> MAPS_SHIFT);

  map = mpage_maps[g];
  if (map) {
    unsigned long addr = (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);
    MPage *page;

    page = map + addr;

#if DEFINE_MALLOC_FREE
    if (page->type == MTYPE_MALLOCFREE)
      return;
#endif

    if (page->type) {
      if (page->compact_to_age < min_referenced_page_age)
	min_referenced_page_age = page->compact_to_age;

      if (!(page->flags & (MFLAG_OLD | MFLAG_BIGBLOCK))) {
	long offset = ((long)p & MPAGE_MASK) >> LOG_WORD_SIZE;
	OffsetTy v;
	void *r;

	if (page->type > MTYPE_TAGGED) {
#if CHECKS
	  if (!offset) {
	    /* Can't point to beginning of non-tagged block! */
	    CRASH(25);
	  }
#endif
	  offset--;
	}

	v = OFFSET_SIZE(page->u.offsets, offset);
#if CHECKS
	if (page->type > MTYPE_TAGGED) {
	  if (!v) {
	    /* Can't point to beginning of non-tagged block! */
	    CRASH(26);
	  }
	}
#endif
	
	if (offset < page->compact_boundary)
	  r = (void *)(page->o.compact_to + v);
	else
	  r = (void *)(((long)p & MPAGE_START) + ((long)v << LOG_WORD_SIZE));

#if SEARCH
	if (r == search_for)
	  stop();
#endif

#if CHECKS
	if (!(find_page(r)->flags & COLOR_MASK)) {
	  bad_dest_addr = r;
	  CRASH(27);
	}
#endif

	if (r != p)
	  *(void **)pp = r;
      }
    }
  }
}

/**********************************************************************/

/* set compact_to_age field of a page: */

void reverse_propagate_new_age(void)
{
  MPage *page;

  for (page = first; page; page = page->next) {
    if (!(page->flags & (MFLAG_BIGBLOCK | MFLAG_OLD))) {
      if (page->compact_boundary > 0) {
	MPage *page_to;
	page_to = find_page(page->o.compact_to);
	if (page_to->age < page->age)
	  page->compact_to_age = page_to->age;
	else
	  page->compact_to_age = page->age;
      } else
	page->compact_to_age = page->age;
    } else
      page->compact_to_age = page->age;
  }
}

/**********************************************************************/

/* Fixup: fixup addresses in all readable objects */

static void fixup_tagged_mpage(void **p, MPage *page)
{
  void **top;
#if KEEP_BACKPOINTERS
  long bp_delta = page->backpointer_page - p;
#endif

  top = p + MPAGE_WORDS;

  while (p < top) {
    Type_Tag tag;
    long size;

    tag = *(Type_Tag *)p;

    if (tag == TAGGED_EOM)
      break;

#if ALIGN_DOUBLES
    if (tag == SKIP) {
      p++;
    } else {
#endif

#if CHECKS
      if ((tag < 0) || (tag >= _num_tags_) || !size_table[tag]) {
	GCFLUSHOUT();
	CRASH(28);
      }
      prev_var_stack = prev_ptr;
      prev_ptr = p;
#endif

      size = fixup_table[tag](p);

#if KEEP_BACKPOINTERS
      GC_fixup((void *)(p + bp_delta));
#endif

      p += size;

#if ALIGN_DOUBLES
    }
#endif
  }
}

static void fixup_array_mpage(void **p, MPage *page)
{
  void **top;
#if KEEP_BACKPOINTERS
  long bp_delta = page->backpointer_page - p;
#endif

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;

    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM)
      break;

#if CHECKS
    if (size >= BIGBLOCK_MIN_SIZE) {
      CRASH(29);
    }
#endif

#if KEEP_BACKPOINTERS
    GC_fixup((void *)(p + bp_delta));
#endif

    for (p++; --size; p++) {
      gcFIXUP(*p);
    }
  }
}

static void fixup_tagged_array_mpage(void **p, MPage *page)
{
  void **top;
#if KEEP_BACKPOINTERS
  long bp_delta = page->backpointer_page - p;
#endif

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;
    int i, elem_size;
    void **mp;
    Type_Tag tag;
    Fixup_Proc traverse;

    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM)
      break;

    mp = p + 1;
    p += size;
    size--;

#if ALIGN_DOUBLES
    if (size) {
#endif
#if KEEP_BACKPOINTERS
      GC_fixup((void *)(mp - 1 + bp_delta));
#endif
      tag = *(Type_Tag *)mp;

      traverse = fixup_table[tag];
      elem_size = traverse(mp);
      mp += elem_size;
      for (i = elem_size; i < size; i += elem_size, mp += elem_size)
	traverse(mp);

#if ALIGN_DOUBLES
    }
#endif
  }
}

static void fixup_xtagged_mpage(void **p, MPage *page)
{
  void **top;
#if KEEP_BACKPOINTERS
  long bp_delta = page->backpointer_page - p;
#endif

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;

    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM)
      break;

#if CHECKS
    if (size >= BIGBLOCK_MIN_SIZE) {
      CRASH(30);
    }
#endif

#if ALIGN_DOUBLES
    if (size > 1) {
#endif
      GC_fixup_xtagged(p + 1);
#if KEEP_BACKPOINTERS
      GC_fixup((void *)(p + bp_delta));
#endif
#if ALIGN_DOUBLES
    }
#endif

    p += size;
  }
}

static void fixup_all_mpages()
{
  MPage *page;

  for (page = first; page; page = page->next) {
    if (page->flags & COLOR_MASK) {
      if (page->type != MTYPE_ATOMIC) {
	void *p;

	scanned_pages++;
	min_referenced_page_age = page->age;
	p = page->block_start;

#if NOISY
	GCPRINT(GCOUTF, "Fixup %lx\n", (long)p);
#endif

	if (page->flags & MFLAG_BIGBLOCK) {
	  do_bigblock((void **)p, page, 1);
#if KEEP_BACKPOINTERS
	  GC_fixup((void *)&(page->backpointer_page));
#endif
	} else {
	  switch (page->type) {
	  case MTYPE_TAGGED:
	    fixup_tagged_mpage((void **)p, page);
	    break;
	  case MTYPE_TAGGED_ARRAY:
	    fixup_tagged_array_mpage((void **)p, page);
	    break;
	  case MTYPE_XTAGGED:
	    fixup_xtagged_mpage((void **)p, page);
	    break;
	  default:
	    fixup_array_mpage((void **)p, page);
	  }
	}

	page->refs_age = min_referenced_page_age;
      }
    }
  }
}

/******************************************************************************/
/*                               free phase                                   */
/******************************************************************************/

/* Free: release unused pages. */

static void free_unused_mpages()
{
  MPage *page, *next;
  memory_in_use = 0;

  for (page = first; page; page = next) {
    next = page->next;
    if (!(page->flags & (COLOR_MASK | MFLAG_OLD))) {
      void *p;
      p = page->block_start;

      if (page->prev)
	page->prev->next = page->next;
      else
	first = page->next;
      if (page->next)
	page->next->prev = page->prev;
      else
	last = page->prev;

      if (page->flags & MFLAG_BIGBLOCK) {
#if NOISY
	GCPRINT(GCOUTF, "Free %lx - %lx\n", (long)p,
		(long)p + page->u.size);
#endif

	free_pages((void *)p, page->u.size);
	
	{
	 long s = page->u.size;
	 unsigned long i = ((unsigned long)p >> MAPS_SHIFT);
	 unsigned long j = (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);
	 while (s > MPAGE_SIZE) {
	   s -= MPAGE_SIZE;
	   j++;
	   if (j == MAP_SIZE) {
	     j = 0;
	     i++;
	   }
	   mpage_maps[i][j].type = 0;
	   mpage_maps[i][j].flags = 0;
	 }
	}
      } else {
#if NOISY
	GCPRINT(GCOUTF, "Free %lx\n", (long)p);
#endif
	free_pages((void *)p, MPAGE_SIZE);
	free_pages(page->u.offsets, OPAGE_SIZE);
#if KEEP_BACKPOINTERS
	free_pages(page->backpointer_page, MPAGE_SIZE);
#endif
      }
      
      if (page->flags & MFLAG_INITED)
	scanned_pages++;
      
      page->type = 0;
      page->flags = 0;
      skipped_pages++;
    } else {
      if (page->flags & MFLAG_BIGBLOCK) {
	if (!(page->flags & MFLAG_CONTINUED))
	  memory_in_use += page->u.size;
      } else
	memory_in_use += MPAGE_SIZE;
    }
  }

  flush_freed_pages();
}

void promote_all_ages()
{
  MPage *page;

  for (page = first; page; page = page->next) {
    if (page->age < 15)
      page->age++;
    if (page->refs_age < 15)
      page->refs_age++;
  }
}


void protect_old_mpages()
{
#if GENERATIONS
  MPage *page;

  if (generations_available) {
    for (page = first; page; page = page->next) {
      if (page->age && (page->type != MTYPE_ATOMIC)) {
	void *p;
      
	if (page->flags & MFLAG_MODIFIED) {
	  page->flags -= MFLAG_MODIFIED;
      
	  p = page->block_start;
	  if (page->flags & MFLAG_BIGBLOCK)
	    protect_pages((void *)p, page->u.size, 0);
	  else 
	    protect_pages((void *)p, MPAGE_SIZE, 0);
	}
      }
    }
  }
#endif
}

/******************************************************************************/
/*                         modification tracking                              */
/******************************************************************************/

#if GENERATIONS

static int designate_modified(void *p);

static int designate_modified_maybe(void *p, int no_barrier_ok)
{
  unsigned long g = ((unsigned long)p >> MAPS_SHIFT);
  MPage *map;

#if CHECKS
  if (during_gc)
    CRASH(31);
#endif

  map = mpage_maps[g];
  if (map) {
    unsigned long addr = (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);
    MPage *page;

    page = map + addr;
    if (page->type) {
      if (page->flags & MFLAG_CONTINUED) {
	designate_modified(page->o.bigblock_start);
	num_seg_faults++;
	return 1;
      } else if (page->age) {
        if (page->flags & MFLAG_MODIFIED) {
          if (no_barrier_ok)
            return 0;
        } else {
          page->flags |= MFLAG_MODIFIED;
          p = (void *)((long)p & MPAGE_START);
          if (page->flags & MFLAG_BIGBLOCK)
            protect_pages(p, page->u.size, 1);
          else
            protect_pages(p, MPAGE_SIZE, 1);
          num_seg_faults++;
          return 1;
        }
      } else if (no_barrier_ok) {
        return 0;
      }

      GCPRINT(GCOUTF, "Seg fault (internal error) at %lx [%ld]\n", 
	      (long)p, num_seg_faults);
      return 0;
    }
  }

  
  GCPRINT(GCOUTF, "Access on unmapped page at %lx [%ld]\n", 
	  (long)p, num_seg_faults);

#if defined(_WIN32) && defined(CHECKS)
  DebugBreak();
#endif
  return 0;
}

static int designate_modified(void *p)
{
  return designate_modified_maybe(p, 0);
}

void GC_write_barrier(void *p)
{
  designate_modified_maybe(p, 1);
}

/* The platform-specific signal handlers, and initialization function: */
# include "sighand.c"

#endif /* GENERATIONS */

/******************************************************************************/
/*                              stack walking                                 */
/******************************************************************************/

#if CHECKS
static void **o_var_stack, **oo_var_stack;
#endif
#if TIME
static int stack_depth;
#endif
#if RECORD_MARK_SRC
static int record_stack_source = 0;
#endif

#include "stack_comp.c"

#define GC_X_variable_stack GC_mark_variable_stack
#if RECORD_MARK_SRC
# define X_source(stk, p) if (record_stack_source) { mark_src = (stk ? stk : p); mark_type = MTYPE_STACK; }
#else
# define X_source(stk, p) /* */
#endif
#define gcX(a) gcMARK(*a)
#include "var_stack.c"
#undef GC_X_variable_stack
#undef gcX
#undef X_source

#define GC_X_variable_stack GC_fixup_variable_stack
#define gcX(a) gcFIXUP(*a)
#define X_source(stk, p) /* */
#include "var_stack.c"
#undef GC_X_variable_stack
#undef gcX
#undef X_source

#if CHECKS
# if CHECK_STACK_PTRS
static void check_ptr(void **a)
{
  void *p = *a;
  MPage *page;

  if (!mpage_maps) return;

  if ((long)p & 0x1) return;

  page = find_page(p);
  if (page) {
    if ((page->type == MTYPE_TAGGED)
        && !(page->flags & MFLAG_BIGBLOCK)) {
      Type_Tag tag;

      tag = *(Type_Tag *)p;
      if ((tag < 0) || (tag >= _num_tags_) 
	  || (!size_table[tag] 
	      && (tag != weak_box_tag)
	      && (tag != ephemeron_tag)
	      && (tag != weak_array_tag)
	      && (tag != gc_on_free_list_tag))) {
	GCPRINT(GCOUTF, "bad tag: %d at %lx, references from %lx\n", tag, (long)p, (long)a);
	GCFLUSHOUT();
	CRASH(70);
      }

    }
#if DEFINE_MALLOC_FREE
    else if (page->type == MTYPE_MALLOCFREE) {
      check_not_freed(page, p);
    }
#endif
  }
}

#define GC_X_variable_stack GC_do_check_variable_stack
#define gcX(a) check_ptr(a)
#define X_source(stk, p) /* */
#include "var_stack.c"
#undef GC_X_variable_stack
#undef gcX
#undef X_source

# endif

void GC_check_variable_stack()
{
# if CHECK_STACK_PTRS
  GC_do_check_variable_stack(GC_variable_stack,
                             0,
                             (void **)(GC_get_thread_stack_base
                                       ? GC_get_thread_stack_base()
                                       : stack_base),
                             NULL);
# endif
}
#endif

/******************************************************************************/
/*                             main GC driver                                 */
/******************************************************************************/

static void set_ending_tags(void)
{
  int i;

  for (i = 0; i < NUM_TAGGED_SETS; i++) {
    if (sets[i]->low < sets[i]->high)
      *(Type_Tag *)sets[i]->low = TAGGED_EOM;
  }
  for (i = NUM_TAGGED_SETS; i < NUM_SETS; i++) {
    if (sets[i]->low < sets[i]->high)
      *(long *)sets[i]->low = UNTAGGED_EOM - 1;
  }
}

static int initialized;

static void init(void)
{
  if (!initialized) {
    GC_register_traversers(weak_box_tag, size_weak_box, mark_weak_box, fixup_weak_box, 1, 0);
    GC_register_traversers(ephemeron_tag, size_ephemeron, mark_ephemeron, fixup_ephemeron, 1, 0);
    GC_register_traversers(weak_array_tag, size_weak_array, mark_weak_array, fixup_weak_array, 0, 0);
#if USE_FREELIST
    GC_register_traversers(gc_on_free_list_tag, size_on_free_list, size_on_free_list, size_on_free_list, 0, 0);
#endif
    GC_add_roots(&finalizers, (char *)&finalizers + sizeof(finalizers) + 1);
    GC_add_roots(&fnl_weaks, (char *)&fnl_weaks + sizeof(fnl_weaks) + 1);
    GC_add_roots(&run_queue, (char *)&run_queue + sizeof(run_queue) + 1);
    GC_add_roots(&last_in_queue, (char *)&last_in_queue + sizeof(last_in_queue) + 1);
    GC_add_roots(&park, (char *)&park + sizeof(park) + 1);

    sets[0] = &tagged;
    sets[1] = &array;
    sets[2] = &tagged_array;
    sets[3] = &xtagged;
    sets[4] = &atomic;

    initialized = 1;

#if GENERATIONS
    initialize_signal_handler();
#endif
  }
}

#if 0
# define GETTIME() ((long)scheme_get_milliseconds())
#else
extern long scheme_get_process_milliseconds();
# define GETTIME() ((long)scheme_get_process_milliseconds())
#endif

#if TIME
# define PRINTTIME(x) GCPRINT x
# define STDERR GCOUTF
static long started, rightnow, old;
# define INITTIME() (started = GETTIME())
# define GETTIMEREL() (rightnow = GETTIME(), old = started, started = rightnow, rightnow - old)
#else
# define INITTIME() /* empty */
# define PRINTTIME(x) /* empty */
#endif

static void do_roots(int fixup)
{
  ImmobileBox *ib;
  int i;

  for (i = 0; i < roots_count; i += 2) {
    void **s = (void **)roots[i];
    void **e = (void **)roots[i + 1];
    
    while (s < e) {
      if (fixup) {
	gcFIXUP(*s);
      } else {
#if RECORD_MARK_SRC
	mark_src = s;
	mark_type = MTYPE_ROOT;
#endif
	gcMARK(*s);
      }
      s++;
    }
  }

  if (fixup)
    GC_fixup_variable_stack(GC_variable_stack,
			    0,
			    (void *)(GC_get_thread_stack_base
				     ? GC_get_thread_stack_base()
				     : stack_base),
                            NULL);
  else {
#if RECORD_MARK_SRC
    record_stack_source = 1;
#endif
    GC_mark_variable_stack(GC_variable_stack,
			   0,
			   (void *)(GC_get_thread_stack_base
				    ? GC_get_thread_stack_base()
				    : stack_base),
                           NULL);
#if RECORD_MARK_SRC
    record_stack_source = 0;
#endif
  }

  /* Do immobiles: */
  for (ib = immobile; ib; ib = ib->next) {
    if (fixup) {
      gcFIXUP(ib->p);
    } else {
#if RECORD_MARK_SRC
      mark_src = ib;
      mark_type = MTYPE_IMMOBILE;
#endif
      gcMARK(ib->p);
    }
  }
}

static void gcollect(int full)
{
  int did_fnls;
#if TIME
  struct rusage pre, post;
#endif
  int young;
  int compact;
  int i;

  INITTIME();
  PRINTTIME((STDERR, "gc: << start with %ld [%d]: %ld\n", 
	     memory_in_use, cycle_count, GETTIMEREL()));

  if (memory_in_use > max_memory_use)
    max_memory_use = memory_in_use;

  init();

  set_ending_tags();

  init_weak_boxes();
  init_ephemerons();
  init_weak_arrays();

  did_fnls = 0;

  gray_first = NULL;

  if (GC_collect_start_callback)
    GC_collect_start_callback();

#if TIME
  getrusage(RUSAGE_SELF, &pre);
#endif

  sort_and_merge_roots();

  during_gc = 1;

  /******************** Init ****************************/

  skipped_pages = 0;
  scanned_pages = 0;
  young_pages = 0;
  inited_pages = 0;

  if (full)
    young = 15;
  else if ((cycle_count & 0xF) == 0xF)
    young = 15;
  else if ((cycle_count & 0x7) == 0x7)
    young = 7;
  else if ((cycle_count & 0x3) == 0x3)
    young = 3;
  else if ((cycle_count & 0x1) == 0x1)
    young = 1;
  else
    young = 0;

#if !GENERATIONS
  young = 15;
#else
  if (!generations_available)
    young = 15;
#endif

#if USE_FREELIST && (COMPACTING == SELECTIVELY_COMPACT)
  if (full)
    compact = 1;
  else {
    /* Remaining free list items few enough? */
    if (((float)(on_free_list << LOG_WORD_SIZE) / memory_in_use) < COMPACT_THRESHOLD)
      compact = 0;
    else
      compact = 1;
  }
#else
# if (COMPACTING == ALWAYS_COMPACT) || !USE_FREELIST
  compact = 1;
# endif
# if (COMPACTING == NEVER_COMPACT)
  compact = 0;
# endif
#endif

  if (compact)
    compact_count++;

  init_all_mpages(young);

  PRINTTIME((STDERR, "gc: init %s [freelist=%f] (young:%d skip:%d scan:%d init:%d): %ld\n", 
	     compact ? "cmpct" : "frlst", (double)(FREE_LIST_DELTA << LOG_WORD_SIZE) / memory_in_use,
	     young_pages, skipped_pages, scanned_pages, inited_pages,
	     GETTIMEREL()));

  /************* Mark and Propagate *********************/

  inited_pages = 0;
#if TIME
  mark_stackoflw = 0;
#endif

#if MARK_STATS
  mark_calls = mark_hits = mark_recalls = mark_colors = mark_many = mark_slow = 0;
#endif

  do_roots(0);

  {
    Fnl *f;
    for (f = finalizers; f; f = f->next) {
#if RECORD_MARK_SRC
      mark_src = f;
      mark_type = MTYPE_FINALIZER;
#endif
      mark_finalizer(f);
    }
    for (f = run_queue; f; f = f->next) {
#if RECORD_MARK_SRC
      mark_src = f;
      mark_type = MTYPE_FINALIZER;
#endif
      mark_finalizer(f);
    }
  }

  {
    Fnl_Weak_Link *wl;
    for (wl = fnl_weaks; wl; wl = wl->next) {
#if RECORD_MARK_SRC
      mark_src = wl;
      mark_type = MTYPE_WEAKLINK;
#endif
      mark_finalizer_weak_link(wl);
    }
  }

#if TIME
  getrusage(RUSAGE_SELF, &post);
#endif

#if MARK_STATS
# define STATS_FORMAT " {c=%ld h=%ld c=%ld r=%ld m=%ld s=%ld}"
# define STATS_ARGS mark_calls, mark_hits, mark_colors, mark_recalls, mark_many, mark_slow,
#else
# define STATS_FORMAT
# define STATS_ARGS
#endif

  PRINTTIME((STDERR, "gc: roots (init:%d deep:%d)"
	     STATS_FORMAT
	     " [%ld/%ld faults]: %ld\n", 
	     inited_pages, stack_depth, 
	     STATS_ARGS
	     post.ru_minflt - pre.ru_minflt, post.ru_majflt - pre.ru_majflt,
	     GETTIMEREL()));

  iterations = 0;

  /* Propagate, mark ready ephemerons */
  propagate_all_mpages();
  mark_ready_ephemerons();

  /* Propagate, loop to do finalization */
  while (1) { 

    /* Propagate all marks. */
    propagate_all_mpages();
    
    if ((did_fnls >= 3) || !finalizers) {
      if (did_fnls == 3) {
	/* Finish up ordered finalization */
	Fnl *f, *next, *prev;
	Fnl_Weak_Link *wl;

	/* Enqueue and mark level 3 finalizers that still haven't been marked. */
	/* (Recursive marking is already done, though.) */
	prev = NULL;
	for (f = finalizers; f; f = next) {
	  next = f->next;
	  if (f->eager_level == 3) {
	    if (!is_marked(f->p)) {
	      /* Not yet marked. Mark it and enqueue it. */
#if RECORD_MARK_SRC
	      mark_src = f;
	      mark_type = MTYPE_FINALIZER;
#endif
	      gcMARK(f->p);

	      if (prev)
		prev->next = next;
	      else
		finalizers = next;
	      
	      f->eager_level = 0; /* indicates queued */
	      
	      f->next = NULL;
	      if (last_in_queue) {
		last_in_queue->next = f;
		last_in_queue = f;
	      } else {
		run_queue = last_in_queue = f;
	      }
	    } else {
	      prev = f;
	    }
	  } else {
	    prev = f;
	  }
	}

	if (young == 15) {
	  /* Restore zeroed out weak links, marking as we go: */	
	  for (wl = fnl_weaks; wl; wl = wl->next) {
	    void *wp = (void *)wl->p;
	    int markit;
	    markit = is_marked(wp);
	    if (markit) {
#if RECORD_MARK_SRC
	      mark_src = wp;
	      mark_type = MTYPE_WEAKLINKX;
#endif
	      gcMARK(wl->saved);
	    }
	    *(void **)(BYTEPTR(wp) + wl->offset) = wl->saved;
	  }
	}
	
	/* We have to mark one more time, because restoring a weak
           link may have made something reachable. */

	did_fnls++;
      } else
	break;
    } else {
      int eager_level = did_fnls + 1;
      
      if (eager_level == 3) {
	/* Ordered finalization */
	Fnl *f;
	Fnl_Weak_Link *wl;

	/* If full collect, zero out weak links for ordered finalization. */
	/* (Only done on full collect to avoid modifying old pages.) */
	if (young == 15) {
	  for (wl = fnl_weaks; wl; wl = wl->next) {
	    void *wp = (void *)wl->p;
	    wl->saved = *(void **)(BYTEPTR(wp) + wl->offset);
	    *(void **)(BYTEPTR(wp) + wl->offset) = NULL;
	  }
	}

	/* Mark content of not-yet-marked finalized objects,
	   but don't mark the finalized objects themselves. */	
	for (f = finalizers; f; f = f->next) {
	  if (f->eager_level == 3) {
#if RECORD_MARK_SRC
	    mark_src = f;
	    mark_type = MTYPE_TAGGED;
#endif
	    if (!is_marked(f->p)) {
	      /* Not yet marked. Mark content. */
	      if (f->tagged) {
		Type_Tag tag = *(Type_Tag *)f->p;
#if CHECKS
		if ((tag < 0) || (tag >= _num_tags_) || !size_table[tag]) {
		  CRASH(34);
		}
#endif
		mark_table[tag](f->p);
	      } else {
		GC_mark_xtagged(f->p);
	      }
	    }
	  }
	}
      } else {
	/* Unordered finalization */
	Fnl *f, *prev, *queue;

	f = finalizers;
	prev = NULL;
	queue = NULL;
	
	while (f) {
	  if (f->eager_level == eager_level) {
	    if (!is_marked(f->p)) {
	      /* Not yet marked. Move finalization to run queue. */
	      Fnl *next = f->next;

	      if (prev)
		prev->next = next;
	      else
		finalizers = next;
	      
	      f->eager_level = 0; /* indicates queued */
	      
	      f->next = NULL;
	      if (last_in_queue) {
		last_in_queue->next = f;
		last_in_queue = f;
	      } else {
		run_queue = last_in_queue = f;
	      }
	      if (!queue)
		queue = f;

	      f = next;
	    } else {
	      prev = f;
	      f = f->next;
	    }
	  } else {
	    prev = f;
	    f = f->next;
	  }
	}
	
	/* Mark items added to run queue: */
	f = queue;
	while (f) {
#if RECORD_MARK_SRC
	  mark_src = f;
	  mark_type = MTYPE_FINALIZER;
#endif
	  gcMARK(f->p);
	  f = f->next;
	}

	mark_ready_ephemerons();
      }
	
      did_fnls++;
    }
  }

#if CHECKS
  {
    Fnl *f;
    /* All finalized objects must be marked at this point. */
    for (f = finalizers; f; f = f->next) {
      if (!is_marked(f->p))
	CRASH(35);
    }
    for (f = run_queue; f; f = f->next) {
      if (!is_marked(f->p))
	CRASH(36);
    }
  }
#endif

#if TIME
  getrusage(RUSAGE_SELF, &post);
#endif

  PRINTTIME((STDERR, "gc: mark (init:%d cycle:%ld stkoflw:%ld)"
	     STATS_FORMAT
	     " [%ld/%ld faults]: %ld\n", 
	     inited_pages, iterations, 
	     mark_stackoflw,
	     STATS_ARGS
	     post.ru_minflt - pre.ru_minflt, post.ru_majflt - pre.ru_majflt,
	     GETTIMEREL()));

  /******************************************************/

  zero_remaining_ephemerons();
  zero_weak_boxes();
  zero_weak_arrays();

  /* Cleanup weak finalization links: */
  {
    Fnl_Weak_Link *wl, *prev, *next;

    prev = NULL;
    for (wl = fnl_weaks; wl; wl = next) {
      next = wl->next;
      if (!is_marked(wl->p)) {
	/* Will be collected. Removed this link. */
	wl->p = NULL;
	if (prev)
	  prev->next = next;
	else
	  fnl_weaks = next;
	--fnl_weak_link_count;
      } else {
	prev = wl;
      }
    }
  }

  PRINTTIME((STDERR, "gc: weak: %ld\n", GETTIMEREL()));

  /******************************************************/

#if USE_FREELIST
  {
    int j;

    for (j = 0; j < NUM_SETS; j++) {
      void **free_lists = sets[j]->free_lists;
      for (i = 0; i < FREE_LIST_ARRAY_SIZE; i++)
	free_lists[i] = NULL;
    }

    on_free_list = 0;
  }
#endif

  if (compact)
    compact_all_mpages();
#if USE_FREELIST
  else
    freelist_all_mpages(young);
#endif

#if TIME
  getrusage(RUSAGE_SELF, &post);
#endif

  PRINTTIME((STDERR, "gc: %s [%ld/%ld faults]: %ld\n", 
	     compact ? "compact" : "freelist",
	     post.ru_minflt - pre.ru_minflt, post.ru_majflt - pre.ru_majflt,
	     GETTIMEREL()));

  /******************************************************/

  promote_all_ages();

  if (compact) {
    for (i = 0; i < NUM_SETS; i++) {
      sets[i]->malloc_page = sets[i]->compact_page;
      sets[i]->low = sets[i]->compact_to + sets[i]->compact_to_offset;
      sets[i]->high = sets[i]->compact_to + MPAGE_WORDS;
      if (sets[i]->compact_to_offset < MPAGE_WORDS) {
	sets[i]->compact_page->age = 0;
	sets[i]->compact_page->refs_age = 0;
	sets[i]->compact_page->flags |= MFLAG_MODIFIED;
      }
    }

    reverse_propagate_new_age();
  } else {
    for (i = 0; i < NUM_SETS; i++) {
      if (sets[i]->malloc_page) {
	if (!(sets[i]->malloc_page->flags & COLOR_MASK)) {
	  sets[i]->malloc_page= NULL;
	  sets[i]->low = sets[i]->high = (void **)0;
	} else
	  sets[i]->malloc_page->flags -= (sets[i]->malloc_page->flags & MFLAG_INITED);
      }
    }
  }

  /******************************************************/

  resolve_for_fixup = 1;

  if (compact) {
#if CHECKS
    int fnl_count = 0;
#endif

    scanned_pages = 0;
    
    do_roots(1);

    {
      Fnl *f;
      for (f = finalizers; f; f = f->next) {
#if CHECKS
	fnl_count++;
#endif
	fixup_finalizer(f);
      }
      for (f = run_queue; f; f = f->next) {
#if CHECKS
	fnl_count++;
#endif
	fixup_finalizer(f);
      }
#if CHECKS
      if (fnl_count != num_fnls)
	CRASH(38);
#endif
    }
    
    {
      Fnl_Weak_Link *wl;
      for (wl = fnl_weaks; wl; wl = wl->next)
	fixup_finalizer_weak_link(wl);
    }

    fixup_all_mpages();
    
#if TIME
    getrusage(RUSAGE_SELF, &post);
#endif
    
    PRINTTIME((STDERR, "gc: fixup (%d) [%ld/%ld faults]: %ld\n", 
	       scanned_pages,
	       post.ru_minflt - pre.ru_minflt, post.ru_majflt - pre.ru_majflt,
	       GETTIMEREL()));
  }

  resolve_for_fixup = 0;

  /******************************************************/
  
  skipped_pages = scanned_pages = 0;

  free_unused_mpages();

  protect_old_mpages();

  reset_finalizer_tree();

#if (COMPACTING == NEVER_COMPACT)
# define THRESH_FREE_LIST_DELTA (FREE_LIST_DELTA >> LOG_WORD_SIZE)
#else
# define THRESH_FREE_LIST_DELTA FREE_LIST_DELTA
#endif

  gc_threshold = (long)((GROW_FACTOR * (memory_in_use - THRESH_FREE_LIST_DELTA))
			+ GROW_ADDITION);

  if (compact) {
    for (i = 0; i < NUM_NONATOMIC_SETS; i++) {
      if (sets[i]->compact_to_offset < MPAGE_WORDS)
	memset(sets[i]->low, 0, (sets[i]->high - sets[i]->low) << LOG_WORD_SIZE);
    }
  }

#if TIME
  getrusage(RUSAGE_SELF, &post);
#endif

  memory_use_growth += (memory_in_use - prev_memory_in_use);
  prev_memory_in_use = memory_in_use;

  PRINTTIME((STDERR, "gc: done with %ld delta=%ld (free:%d cheap:%d) [%ld/%ld faults]: %ld >>\n",
	     memory_in_use, memory_use_growth, skipped_pages, scanned_pages,
	     post.ru_minflt - pre.ru_minflt, post.ru_majflt - pre.ru_majflt,
	     GETTIMEREL()));

  during_gc = 0;

  if (young == 15) {
    cycle_count = 0;
    memory_use_growth = 0;
  } else {
    if ((cycle_count & 0x1) 
	|| (memory_use_growth > INCREMENT_CYCLE_COUNT_GROWTH))
      cycle_count++;
  }
  gc_count++;

  if (GC_collect_start_callback)
    GC_collect_end_callback();

  /**********************************************************************/

  /* Run Finalizations. Collections may happen */

  ran_final = 0;

  if (!running_finals) {
    running_finals = 1;

    while (run_queue) {
      Fnl *f;
      void **gcs;
      
      ran_final++;
      
      f = run_queue;
      run_queue = run_queue->next;
      if (!run_queue)
	last_in_queue = NULL;
      --num_fnls;

      gcs = GC_variable_stack;
      f->f(f->p, f->data);
      GC_variable_stack = gcs;
    }

    running_finals = 0;
  }
}

void *GC_resolve(void *p)
{
  if (resolve_for_fixup) {
    GC_fixup(&p);
    return p;
  } else
    return p;
}

void *GC_fixup_self(void *p)
{
  return p;
}

/******************************************************************************/
/*                               allocators                                   */
/******************************************************************************/

void *malloc_pages_try_hard(size_t len, size_t alignment)
{
  void *m;
  int i = 5;

  ran_final = 1;

  while (i--) {
    m = malloc_pages(len, alignment);
    if (m)
      return m;
    if (!ran_final)
      break;
    else
      gcollect(1);
  }

  if (GC_out_of_memory)
    GC_out_of_memory();

  GCPRINT(GCOUTF, "Out of memory\n");
  abort();
}

/**********************************************************************/

static MPage *get_page_rec(void *p, mtype_t mtype, mflags_t flags)
{
  unsigned long g, addr;
  MPage *map;

  g = ((unsigned long)p >> MAPS_SHIFT);
  
  if (!mpage_maps) {
    int i;
    mpage_maps = (MPage **)malloc_pages(sizeof(MPage *) * MAPS_SIZE, 0);
    if (!mpage_maps) {
      GCPRINT(GCOUTF, "Can't allocate map list\n");
      abort();
    }
    for (i = 0; i < MAPS_SIZE; i++)
      mpage_maps[i] = NULL;
  }

  map = mpage_maps[g];
  if (!map) {
    int i;

    map = (MPage *)malloc_pages_try_hard(sizeof(MPage) * MAP_SIZE, 0);
    for (i = 0; i < MAP_SIZE; i++) {
      map[i].type = 0;
      map[i].flags = 0;
    }

    mpage_maps[g] = map;
  }

  addr = (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);

#if NOISY
  {
    int c;
    if (!mtype)
      c = '.';
    else {
      if (mtype <= MTYPE_TAGGED)
	c = 't';
      else if (mtype == MTYPE_ATOMIC)
	c = 'a';
      else if (mtype == MTYPE_TAGGED_ARRAY)
	c = 'g';
      else
	c = 'v';

      if (flags & MFLAG_BIGBLOCK)
	c = c - ('a' - 'A');
    }
    GCPRINT(GCOUTF, "%c p = %lx, g = %lx, addr = %lx\n", c, (long)p, g, addr);
  }
#endif

  return map + addr;
}

static void new_page(mtype_t mtype, mflags_t mflags, MSet *set, int link)
{
  void *p;
  MPage *map;
  OffsetArrTy *offsets;

  if ((memory_in_use > gc_threshold) && link && !avoid_collection) {
    gcollect(0);
    return;
  }
  
  p = (void *)malloc_pages_try_hard(MPAGE_SIZE, MPAGE_SIZE);
  offsets = (OffsetArrTy *)malloc_pages_try_hard(OPAGE_SIZE, 0);

  memory_in_use += MPAGE_SIZE;

  map = get_page_rec(p, mtype, mflags);

  map->type = mtype;
  map->flags = (mflags | MFLAG_MODIFIED);
  map->u.offsets = offsets;
  map->block_start = p;
  map->age = 0;
  map->refs_age = 0;

  if (link) {
    map->next = NULL;
    map->prev = last;
    if (last)
      last->next = map;
    else
      first = map;
    last = map;
  } else {
    map->next = map->prev = NULL;
  }

  set->malloc_page = map;

  set->low = (void **)p;
  set->high = (void **)(BYTEPTR(p) + MPAGE_SIZE);

#if KEEP_BACKPOINTERS
  map->backpointer_page = (void **)malloc_pages_try_hard(MPAGE_SIZE, 0);
#endif
}

static void *malloc_bigblock(long size_in_bytes, mtype_t mtype, int link)
{
  void *p, *mp;
  MPage *map;
  long s;

#if SEARCH
  if (size_in_bytes == search_size)
    stop();
#endif

  if ((memory_in_use > gc_threshold) && link && !avoid_collection) {
    gcollect(0);
    return malloc_bigblock(size_in_bytes, mtype, 1);
  }
  
  p = (void *)malloc_pages_try_hard(size_in_bytes, MPAGE_SIZE);

  memory_in_use += size_in_bytes;

  map = get_page_rec(p, mtype, MFLAG_BIGBLOCK);
  
  map->type = mtype;
  map->flags = (MFLAG_BIGBLOCK | MFLAG_MODIFIED);
  map->u.size = size_in_bytes;
  map->block_start = p;
  map->age = 0;
  map->refs_age = 0;

  if (link) {
    map->next = NULL;
    map->prev = last;
    if (last)
      last->next = map;
    else
      first = map;
    last = map;
  } else {
    map->next = NULL;
    map->prev = NULL;
  }

  s = size_in_bytes;
  mp = p;
  while (s > MPAGE_SIZE) {
    mp = BYTEPTR(mp) + MPAGE_SIZE;
    s -= MPAGE_SIZE;
    map = get_page_rec(mp, 0, MFLAG_CONTINUED | MFLAG_BIGBLOCK);
    map->type = mtype;
    map->flags = MFLAG_CONTINUED | MFLAG_BIGBLOCK;
    map->o.bigblock_start = p;
  }

#if SEARCH
  if (p == search_for) {
    stop();
  }
#endif

  return p;
}

void *GC_malloc_one_tagged(size_t size_in_bytes)
{
  size_t size_in_words;
  void **m, **naya;

#if CHECKS
  GC_check_variable_stack();
#endif

  size_in_words = ((size_in_bytes + 3) >> LOG_WORD_SIZE);

#if CHECKS
  if (size_in_words < 2)
    CRASH(37);
#endif

  if (size_in_words >= (BIGBLOCK_MIN_SIZE >> LOG_WORD_SIZE)) {
    return malloc_bigblock(size_in_words << LOG_WORD_SIZE, MTYPE_TAGGED, 1);
  }

#if USE_FREELIST
  m = (void *)tagged.free_lists[size_in_words];
  if (m) {
    int i;

    tagged.free_lists[size_in_words] = m[1];

    for (i = 0; i < size_in_words; i++)
      m[i] = NULL;

    on_free_list -= size_in_words;
    
    return m;
  }
#endif

#if ALIGN_DOUBLES
  if (!(size_in_words & 0x1)) {
    /* Make sure memory is 8-aligned */
    if (((long)tagged.low & 0x4)) {
      if (tagged.low == tagged.high) {
	new_page(MTYPE_TAGGED, 0, &tagged, 1);
	return GC_malloc_one_tagged(size_in_words << LOG_WORD_SIZE);
      }
      ((Type_Tag *)tagged.low)[0] = SKIP;
      tagged.low += 1;
    }
  }
#endif

#if SEARCH
  if (size_in_bytes == search_size)
    stop();
#endif

  m = tagged.low;
  naya = tagged.low + size_in_words;
  if (naya >= tagged.high) {
    if (tagged.low < tagged.high)
      *(Type_Tag *)tagged.low = TAGGED_EOM;
    new_page(MTYPE_TAGGED, 0, &tagged, 1);
    return GC_malloc_one_tagged(size_in_words << LOG_WORD_SIZE);
  }
  tagged.low = naya;

#if SEARCH
  if (m == search_for) {
    stop();
  }
#endif

  return m;
}

void *GC_malloc_one_small_tagged(size_t size_in_bytes)
{
  return GC_malloc_one_tagged(size_in_bytes);
}

void *GC_malloc_one_small_dirty_tagged(size_t size_in_bytes)
{
  return GC_malloc_one_tagged(size_in_bytes);
}

void *GC_malloc_pair(void *a, void *b)
{
  void *p;

  park[0] = a;
  park[1] = b;
  p = GC_malloc_one_tagged(3 << LOG_WORD_SIZE);
  a = park[0];
  b = park[1];

  ((Type_Tag *)p)[0] = pair_tag;
  ((void **)p)[1] = a;
  ((void **)p)[2] = b;

  return p;
}

#ifndef gcINLINE
# define gcINLINE inline
#endif

static gcINLINE void *malloc_untagged(size_t size_in_bytes, mtype_t mtype, MSet *set)
{
  size_t size_in_words;
  void **m, **naya;

#if CHECKS
  GC_check_variable_stack();
#endif

  if (!size_in_bytes)
    return zero_sized;

  size_in_words = ((size_in_bytes + 3) >> LOG_WORD_SIZE);

  if (size_in_words >= (BIGBLOCK_MIN_SIZE >> LOG_WORD_SIZE)) {
    return malloc_bigblock(size_in_words << LOG_WORD_SIZE, mtype, 1);
  }

#if USE_FREELIST
  m = (void *)set->free_lists[size_in_words];
  if (m) {
    int i;

    set->free_lists[size_in_words] = m[0];

    if (mtype != MTYPE_ATOMIC)
      memset(m, 0, size_in_words << LOG_WORD_SIZE);

    on_free_list -= size_in_words;
    
    return m;
  }
#endif

#if ALIGN_DOUBLES
  if (!(size_in_words & 0x1)) {
    /* Make sure memory is 8-aligned */
    if (!((long)set->low & 0x4)) {
      if (set->low == set->high) {
	new_page(mtype, 0, set, 1);
	return malloc_untagged(size_in_words << LOG_WORD_SIZE, mtype, set);
      }
      (set->low)[0] = 0;
      set->low += 1;
    }
  }
#endif

#if SEARCH
  if (size_in_bytes == search_size)
    stop();
#endif

  m = set->low;
  naya = set->low + size_in_words + 1;
  if (naya >= set->high) {
    if (set->low < set->high)
      *(long *)set->low = UNTAGGED_EOM - 1;
    new_page(mtype, 0, set, 1);
    return malloc_untagged(size_in_words << LOG_WORD_SIZE, mtype, set);
  }
  set->low = naya;

#if SEARCH
  if ((m + 1) == search_for) {
    stop();
  }
#endif

  *(long *)m = size_in_words;

  return m + 1;
}

/* Array of pointers: */
void *GC_malloc(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, MTYPE_ARRAY, &array);
}

void *GC_malloc_allow_interior(size_t size_in_bytes)
{
  return malloc_bigblock(size_in_bytes, MTYPE_ARRAY, 1);
}

void *GC_malloc_atomic_allow_interior(size_t size_in_bytes)
{
  return malloc_bigblock(size_in_bytes, MTYPE_ATOMIC, 1);
}

void *GC_malloc_tagged_allow_interior(size_t size_in_bytes)
{
  return malloc_bigblock(size_in_bytes, MTYPE_TAGGED, 1);
}

void *GC_malloc_array_tagged(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, MTYPE_TAGGED_ARRAY, &tagged_array);
}

void *GC_malloc_one_xtagged(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, MTYPE_XTAGGED, &xtagged);
}

/* Pointerless */
void *GC_malloc_atomic(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, MTYPE_ATOMIC, &atomic);
}

/* Plain malloc: */
void *GC_malloc_atomic_uncollectable(size_t size_in_bytes)
{
  return malloc(size_in_bytes);
}

/******************************************************************************/
/*                                  misc                                      */
/******************************************************************************/

static void free_bigpage(MPage *page)
{
  long s;
  unsigned long i, j;

  page->type = 0;
  page->flags = 0;

  free_pages(page->block_start, page->u.size);
	
  s = page->u.size;
  i = ((unsigned long)page->block_start >> MAPS_SHIFT);
  j = (((unsigned long)page->block_start & MAP_MASK) >> MAP_SHIFT);
  while (s > MPAGE_SIZE) {
    s -= MPAGE_SIZE;
    j++;
    if (j == MAP_SIZE) {
      j = 0;
      i++;
    }
    mpage_maps[i][j].type = 0;
    mpage_maps[i][j].flags = 0;
  }
}

void GC_free(void *p)
{
  MPage *page;

  page = find_page(p);

  if ((page->flags & MFLAG_BIGBLOCK)
      && !(page->flags & MFLAG_CONTINUED)
      && (p == page->block_start)) {
    memory_in_use -= page->u.size;

    if (page->prev)
      page->prev->next = page->next;
    else
      first = page->next;
    if (page->next)
      page->next->prev = page->prev;
    else
      last = page->prev;

    free_bigpage(page);
  }
}

long GC_malloc_stays_put_threshold() 
{ 
  return BIGBLOCK_MIN_SIZE;
}

void GC_gcollect()
{
  gcollect(1);
}

long GC_get_memory_use(void *c)
{
  return memory_in_use;
}

int GC_set_account_hook(int type, void *cust, unsigned long b, void *f) 
{
  return 0;
}

int GC_mtrace_new_id(void *f)
{
  return 0;
}

int GC_mtrace_union_current_with(int newval)
{
  return 0;
}

unsigned long GC_get_stack_base(void)
{
  return stack_base;
}

/******************************************************************************/
/*                        malloc and free replacements                        */
/******************************************************************************/

#if DEFINE_MALLOC_FREE

# define MALLOC_MIDDLE_SIZE (BIGBLOCK_MIN_SIZE >> LOG_WORD_SIZE)

static MSet mallocfree_set;

void *mallocfree_freelists[FREE_LIST_ARRAY_SIZE];

void *malloc(size_t size)
{
  void **m, **naya;
  long size_in_words = (size +  (WORD_SIZE - 1)) >> LOG_WORD_SIZE;
  int pos;

  if (size_in_words < 2)
    size_in_words = 2; /* need at least 2 for freelist */

  if (size_in_words >= (BIGBLOCK_MIN_SIZE >> LOG_WORD_SIZE)) {
    return malloc_bigblock(size_in_words << LOG_WORD_SIZE, MTYPE_MALLOCFREE, 0);
  }

#if ALIGN_DOUBLES
  if (size_in_words & 0x1)
    size_in_words++;
#endif

  if (mallocfree_freelists[size_in_words]) {
    MPage *page;

    m = mallocfree_freelists[size_in_words];
    mallocfree_freelists[size_in_words] = ((void **)m)[1];

    page = find_page(m);
    pos = m - (void **)((long)m & MPAGE_START);

    OFFSET_SET_SIZE_UNMASKED(page->u.offsets, pos, size_in_words);
    while (--size_in_words) {
      pos++;
      OFFSET_SET_SIZE_UNMASKED(page->u.offsets, pos, MALLOC_MIDDLE_SIZE);
    }

    return m;
  }

  m = mallocfree_set.low;
  naya = m + size_in_words;
  if (naya >= mallocfree_set.high) {
    new_page(MTYPE_MALLOCFREE, 0, &mallocfree_set, 0);
    return malloc(size);
  }
  mallocfree_set.low = naya;

  pos = m - (void **)mallocfree_set.malloc_page->block_start;

  OFFSET_SET_SIZE_UNMASKED(mallocfree_set.malloc_page->u.offsets, pos, size_in_words);
  while (--size_in_words) {
    pos++;
    OFFSET_SET_SIZE_UNMASKED(mallocfree_set.malloc_page->u.offsets, pos, MALLOC_MIDDLE_SIZE);
  }

  return m;
}

void free(void *p)
{
  MPage *page;
  int pos;
  long sz;

  if (!p)
    return;

  page = find_page(p);
  if (!page || (page->type != MTYPE_MALLOCFREE)) {
    GCPRINT(GCOUTF, "Free of non-malloced pointer! %p\n", p);
    return;
  }
  
  if (page->flags & MFLAG_BIGBLOCK) {
    if ((page->flags & MFLAG_CONTINUED) || (p != page->block_start)) {
      GCPRINT(GCOUTF, "Free of in the middle of large malloced pointer! %p\n", p);
      return;
    }

    free_bigpage(page);

    return;
  }

  pos = (void **)p - (void **)page->block_start;

  sz = OFFSET_SIZE(page->u.offsets, pos);

  if (!sz) {
    GCPRINT(GCOUTF, "Free of non-malloced to already-freed pointer! %p\n", p);
    return;
  }

  if (sz == MALLOC_MIDDLE_SIZE) {
    GCPRINT(GCOUTF, "Free in middle of malloced pointer! %p\n", p);
    return;
  }

  OFFSET_SET_SIZE_UNMASKED(page->u.offsets, pos, 0);

  ((int *)p)[0] = sz;
  ((void **)p)[1] = mallocfree_freelists[sz];
  mallocfree_freelists[sz] = p;

  while (--sz) {
    pos++;
    OFFSET_SET_SIZE_UNMASKED(page->u.offsets, pos, 0);
  }
}

void *realloc(void *p, size_t size)
{
  void *naya;
  size_t oldsize;

  if (p) {
    MPage *page;
    page = find_page(p);
    if (!page || (page->type != MTYPE_MALLOCFREE)) {
      GCPRINT(GCOUTF, "Realloc of non-malloced pointer! %p\n", p);
      oldsize = 0;
    } else {
      if (page->flags & MFLAG_BIGBLOCK) {
	if ((page->flags & MFLAG_CONTINUED) || (p != page->block_start)) {
	  GCPRINT(GCOUTF, "Realloc of in the middle of large malloced pointer! %p\n", p);
	  oldsize = 0;
	} else
	  oldsize = page->u.size;
      } else {
	int pos;
	pos = (void **)p - (void **)page->block_start;
	oldsize = OFFSET_SIZE(page->u.offsets, pos);
	if (oldsize == MALLOC_MIDDLE_SIZE) {
	  GCPRINT(GCOUTF, "Realloc in middle of malloced pointer! %p\n", p);
	  oldsize = 0;
	}
      }
    }
  } else
    oldsize = 0;

  oldsize <<= LOG_WORD_SIZE;

  naya = malloc(size);
  if (oldsize > size)
    oldsize = size;
  memcpy(naya, p, oldsize);
  if (p)
    free(p);

  return naya;
}

void *calloc(size_t n, size_t size)
{
  void *p;
  long c;

  c = n * size;
  p = malloc(c);
  memset(p, 0, c);

  return p;
}

# if CHECKS
static void check_not_freed(MPage *page, const void *p)
{
  if (page->flags & MFLAG_BIGBLOCK) {
    /* Ok */
  } else {
    int pos;
    pos = (void **)p - (void **)page->block_start;
    if (!OFFSET_SIZE(page->u.offsets, pos)) {
      GCPRINT(GCOUTF, "Mark of previously malloced? (now freed) pointer: %p\n", p);
      CRASH(77);
    }
  }
}
# endif

#endif

/******************************************************************************/
/*                             GC stat dump                                   */
/******************************************************************************/

static long dump_info_array[BIGBLOCK_MIN_SIZE];

#if KEEP_BACKPOINTERS

static void *trace_backpointer(MPage *page, void *p)
{

  if (page->flags & MFLAG_BIGBLOCK)
    return (void *)page->backpointer_page;
  else {
    int offset;
    offset = ((char *)p - (char *)page->block_start) >> LOG_WORD_SIZE;
    if (page->type != MTYPE_TAGGED)
      offset -= 1;
    if (offset > 0)
      return page->backpointer_page[offset];
    else
      return NULL; /* This shouldn't happen */
  }
}

# define trace_page_t MPage
# define trace_page_type(page) (page)->type
static void *trace_pointer_start(struct mpage *page, void *p) { 
  if (page->flags & MFLAG_BIGBLOCK) 
    return page->block_start;
  else 
    return p;
}
# define TRACE_PAGE_TAGGED MTYPE_TAGGED
# define TRACE_PAGE_ARRAY MTYPE_ARRAY
# define TRACE_PAGE_TAGGED_ARRAY MTYPE_TAGGED_ARRAY
# define TRACE_PAGE_ATOMIC MTYPE_ATOMIC
# define TRACE_PAGE_XTAGGED MTYPE_XTAGGED
# define TRACE_PAGE_MALLOCFREE MTYPE_MALLOCFREE
# define TRACE_PAGE_BAD 0
# define trace_page_is_big(page) ((page)->flags & MFLAG_BIGBLOCK)

# include "backtrace.c"

#endif

static long scan_tagged_mpage(void **p, MPage *page, short trace_for_tag,
			      GC_for_each_found_proc for_each_found)
{
  void **top, **bottom = p;

  top = p + MPAGE_WORDS;
  
  while (p < top) {
    Type_Tag tag;
    long size;

    tag = *(Type_Tag *)p;

    if (tag == TAGGED_EOM) {
      return (p - bottom);
    }

#if ALIGN_DOUBLES
    if (tag == SKIP) {
      p++;
    } else {
#endif      
      {
	Size_Proc size_proc;
	
	size_proc = size_table[tag];
	if (((long)size_proc) < 100)
	  size = (long)size_proc;
	else
	  size = size_proc(p);
      }

      dump_info_array[tag]++;
      dump_info_array[tag + _num_tags_] += size;

      if (tag == trace_for_tag) {
#if KEEP_BACKPOINTERS
	register_traced_object(p);
#endif
	if (for_each_found)
	  for_each_found(p);
      }

      p += size;
#if ALIGN_DOUBLES
    }
#endif
  }

  return MPAGE_WORDS;
}

static long scan_untagged_mpage(void **p, MPage *page)
{
  void **top, **bottom = p;

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;

    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM) {
      return (p - bottom);
    }

    dump_info_array[size - 1] += 1;

    p += size;
  } 

  return MPAGE_WORDS;
}

#if KEEP_BACKPOINTERS

int GC_is_tagged(void *p)
{
  MPage *page;
  page = find_page(p);
  return page && (page->type == MTYPE_TAGGED);
}

static void *next_tagged_start(void *p, int stop_at_p)
{
  MPage *page;
  void **p2, **top;
  int prev_was_p = 0;

  page = find_page(p);
  if (page && (page->type == MTYPE_TAGGED)) {
    p2 = (void **)page->block_start;

    if (page->flags & MFLAG_CONTINUED)
      return NULL;
    if (page->flags & MFLAG_BIGBLOCK) {
      if (p == (void *)p2) {
	if (stop_at_p)
	  return p;
      }
      return NULL;
    }

    top = p2 + MPAGE_WORDS;
    
    while (p2 < top) {
      Type_Tag tag;
      long size;

      if (stop_at_p) {
	if ((void *)p2 == p)
	  return p;
	if ((unsigned long)p2 > (unsigned long)p)
	  break;
      }
      
      tag = *(Type_Tag *)p2;

      if (tag == TAGGED_EOM)
	break;

#if ALIGN_DOUBLES
      if (tag == SKIP) {
	p2++;
      } else {
#endif
	if (prev_was_p)
	  return (void *)p2;

	{
	  Size_Proc size_proc;
	
	  size_proc = size_table[tag];
	  if (((long)size_proc) < 100)
	    size = (long)size_proc;
	  else
	    size = size_proc(p2);
	}

	prev_was_p = (p == p2);
      
	p2 += size;
#if ALIGN_DOUBLES
      }
#endif
    }
  }
  
  return NULL;
}

int GC_is_tagged_start(void *p)
{
  if (next_tagged_start(p, 1))
    return 1;
  else
    return 0;
}

void *GC_next_tagged_start(void *p)
{
  void *p2;

  while (1) {
    p2 = next_tagged_start(p, 0);
    if (p2)
      return p2;

    p = (void *)(((long)p & MPAGE_START) + MPAGE_SIZE);
    if (!p)
      return NULL;
  }
}

#endif

void GC_dump_with_traces(int flags,
			 GC_get_type_name_proc get_type_name,
			 GC_get_xtagged_name_proc get_xtagged_name,
			 GC_for_each_found_proc for_each_found,
			 short trace_for_tag,
			 GC_print_tagged_value_proc print_tagged_value,
			 int path_length_limit)
{
  int i;
  long waste = 0;

  if (!(flags & GC_DUMP_SHOW_TRACE))
    trace_for_tag = -1;
#if KEEP_BACKPOINTERS
  reset_object_traces();
#endif
  if (for_each_found)
    avoid_collection++;

  if (flags & GC_DUMP_SHOW_DETAILS) {
    GCPRINT(GCOUTF, "t=tagged a=atomic v=array x=xtagged g=tagarray\n");
    GCPRINT(GCOUTF, "mpagesize=%ld  opagesize=%ld\n", (long)MPAGE_SIZE, (long)OPAGE_SIZE);
    GCPRINT(GCOUTF, "[");
    for (i = 0; i < MAPS_SIZE; i++) {
      if (i && !(i & 63))
	GCPRINT(GCOUTF, "\n ");

      if (mpage_maps[i])
	GCPRINT(GCOUTF, "*");
      else
	GCPRINT(GCOUTF, "-");
    }
    GCPRINT(GCOUTF, "]\n");
    for (i = 0; i < MAPS_SIZE; i++) {
      MPage *maps = mpage_maps[i];
      if (maps) {
	int j;
	GCPRINT(GCOUTF, "%.2x:\n ", i);
	for (j = 0; j < MAP_SIZE; j++) {
	  if (j && !(j & 63))
	    GCPRINT(GCOUTF, "\n ");

	  if (maps[j].type
#if DEFINE_MALLOC_FREE
	      && (maps[j].type != MTYPE_MALLOCFREE)
#endif
	      ) {
	    int c;

	    if (maps[j].flags & MFLAG_CONTINUED) 
	      c = '.';
	    else {
	      if (maps[j].type <= MTYPE_TAGGED)
		c = 't';
	      else if (maps[j].type == MTYPE_TAGGED_ARRAY)
		c = 'g';
	      else if (maps[j].type == MTYPE_ATOMIC)
		c = 'a';
	      else if (maps[j].type == MTYPE_XTAGGED)
		c = 'x';
	      else
		c = 'v';
	    
	      if (maps[j].flags & MFLAG_BIGBLOCK)
		c = c - ('a' - 'A');
	    }

	    GCPRINT(GCOUTF, "%c", c);
	  } else {
	    GCPRINT(GCOUTF, "-");
	  }
	}
	GCPRINT(GCOUTF, "\n");
      }
    }

    {
      MPage *page;

      GCPRINT(GCOUTF, "Block info: [type][modified?][age][refs-age]\n");
      for (page = first, i = 0; page; page = page->next, i++) {
	int c;

	if (page->flags & MFLAG_CONTINUED) 
	  c = '.';
	else {
	  if (page->type <= MTYPE_TAGGED)
	    c = 't';
	  else if (page->type == MTYPE_TAGGED_ARRAY)
	    c = 'g';
	  else if (page->type == MTYPE_ATOMIC)
	    c = 'a';
	  else if (page->type == MTYPE_XTAGGED)
	    c = 'x';
	  else
	    c = 'v';
	 
	  if (page->flags & MFLAG_BIGBLOCK)
	    c = c - ('a' - 'A');
	}
       
	GCPRINT(GCOUTF, " %c%c%c%c",
		c,
		((page->flags & MFLAG_MODIFIED)
		 ? 'M'
		 : '_'),
		((page->age < 10)
		 ? (page->age + '0')
		 : (page->age + 'a' - 10)),
		((page->type == MTYPE_ATOMIC)
		 ? '-'
		 : ((page->refs_age < 10)
		    ? (page->refs_age + '0')
		    : (page->refs_age + 'a' - 10))));
	if ((i % 10) == 9)
	  GCPRINT(GCOUTF, "\n");
      }
      GCPRINT(GCOUTF, "\n");
    }
  }

  {
    int j;

    init();
    set_ending_tags();

    for (j = 0; j < NUM_SETS; j++) {
      int kind, i;
      char *name;
      MPage *page;
      long used, total;

      switch (j) {
      case 1: kind = MTYPE_ARRAY; name = "array"; break;
      case 2: kind = MTYPE_ATOMIC; name = "atomic"; break;
      case 3: kind = MTYPE_XTAGGED; name = "xtagged"; break;
      case 4: kind = MTYPE_TAGGED_ARRAY; name = "tagarray"; break;
      default: kind = MTYPE_TAGGED; name = "tagged"; break;
      }

      for (i = 0; i < (BIGBLOCK_MIN_SIZE >> LOG_WORD_SIZE); i++)
	dump_info_array[i] = 0;

      total = 0;

      for (page = first; page; page = page->next) {
	if ((page->type == kind) && !(page->flags & MFLAG_BIGBLOCK)) {
	  if (j >= NUM_TAGGED_SETS)
	    used = scan_untagged_mpage(page->block_start, page); /* gets size counts */
	  else
	    used = scan_tagged_mpage(page->block_start, page, 
				     trace_for_tag, for_each_found); /* gets tag counts */

	  total += used;
	  waste += (MPAGE_WORDS - used);
	}
	if ((page->flags & MFLAG_BIGBLOCK)
	    && (page->type == kind)
	    && (((trace_for_tag >= (BIGBLOCK_MIN_SIZE >> LOG_WORD_SIZE))
		 && (page->u.size > trace_for_tag))
		|| (page->u.size == -trace_for_tag))) {
#if KEEP_BACKPOINTERS
	  register_traced_object(page->block_start);
#endif
	  if (for_each_found)
	    for_each_found(page->block_start);
	}
      }

      if (j >= NUM_TAGGED_SETS) {
	int k = 0;
	if (flags & GC_DUMP_SHOW_DETAILS) {
	  GCPRINT(GCOUTF, "%s counts: ", name);
	  for (i = 0; i < (BIGBLOCK_MIN_SIZE >> LOG_WORD_SIZE); i++) {
	    if (dump_info_array[i]) {
	      k++;
	      if (k == 10) {
		GCPRINT(GCOUTF, "\n    ");
		k = 0;
	      }
	      GCPRINT(GCOUTF, " [%d:%ld]", i << LOG_WORD_SIZE, dump_info_array[i]);
	    }
	  }
	  GCPRINT(GCOUTF, "\n");
	}
      } else {
	GCPRINT(GCOUTF, "Tag counts and sizes:\n");
	GCPRINT(GCOUTF, "Begin MzScheme3m\n");
	for (i = 0; i < _num_tags_; i++) {
	  if (dump_info_array[i]) {
	    char *tn, buf[256];
	    switch(i) {
	    case gc_on_free_list_tag: tn = "freelist-elem"; break;
	    default:
	      if (i == weak_array_tag)
		tn = "weak-array";
	      else if (get_type_name)
		tn = get_type_name((Type_Tag)i);
	      else
		tn = NULL;
	      if (!tn) {
		sprintf(buf, "unknown,%d", i);
		tn = buf;
	      }
	      break;
	    }
	    GCPRINT(GCOUTF, "  %20.20s: %10ld %10ld\n", tn, dump_info_array[i], (dump_info_array[i + _num_tags_]) << LOG_WORD_SIZE);
	  }
	}
	GCPRINT(GCOUTF, "End MzScheme3m\n");
      }

      if (flags & GC_DUMP_SHOW_DETAILS) {
	int did_big = 0;
	for (page = first; page; page = page->next) {
	  if ((page->type == kind) && (page->flags & MFLAG_BIGBLOCK) && !(page->flags & MFLAG_CONTINUED)) {
	    if (!did_big) {
	      GCPRINT(GCOUTF, "    ");
	      did_big = 1;
	    }
	    if (j >= NUM_TAGGED_SETS)
	      GCPRINT(GCOUTF, " [+%ld]", page->u.size);
	    else
	      GCPRINT(GCOUTF, " %d:[+%ld]", (int)*(Type_Tag *)(page->block_start), page->u.size);
	    
	    total += (page->u.size >> LOG_WORD_SIZE);
	    waste += ((page->u.size >> LOG_WORD_SIZE)  & (MPAGE_WORDS - 1));
	  }
	}
	if (did_big)
	  GCPRINT(GCOUTF, "\n");
      }

      GCPRINT(GCOUTF, " Total %s: %ld\n", name, total << LOG_WORD_SIZE);
    }
  }

  GCPRINT(GCOUTF, "Active fnls: %d\n", num_fnls);
  GCPRINT(GCOUTF, "Active fnl weak links: %d\n", fnl_weak_link_count);

  if (memory_in_use > max_memory_use)
    max_memory_use = memory_in_use;
  
  GCPRINT(GCOUTF, "Number of collections: %d  (%d compacting)\n", gc_count, compact_count);
  GCPRINT(GCOUTF, "Memory high point: %ld\n", max_memory_use);

  GCPRINT(GCOUTF, "Memory use: %ld\n", memory_in_use - FREE_LIST_DELTA);
  GCPRINT(GCOUTF, "Memory wasted: %ld (%.2f%%)\n", waste << LOG_WORD_SIZE, 
	  (100.0 * (waste << LOG_WORD_SIZE)) / memory_in_use);
  GCPRINT(GCOUTF, "Memory overhead: %ld (%.2f%%)   %ld (%.2f%%) on free list\n", 
	  page_allocations - memory_in_use + FREE_LIST_DELTA,
	  (100.0 * ((double)page_allocations - memory_in_use)) / memory_in_use,
	  (long)FREE_LIST_DELTA,
	  (100.0 * FREE_LIST_DELTA) / memory_in_use);
  GCPRINT(GCOUTF, "Mmap overhead: %ld (%.2f%%)\n", 
	  page_reservations - memory_in_use + FREE_LIST_DELTA,
	  (100.0 * ((double)page_reservations - memory_in_use)) / memory_in_use);

#if KEEP_BACKPOINTERS
  if (flags & GC_DUMP_SHOW_TRACE) {
    print_traced_objects(path_length_limit, get_type_name, get_xtagged_name, print_tagged_value);
  }
  if (flags & GC_DUMP_SHOW_FINALS) {
    Fnl *f;
    avoid_collection++;
    GCPRINT(GCOUTF, "Begin Finalizations\n");
    for (f = finalizers; f; f = f->next) {
      print_out_pointer("==@ ", f->p, get_type_name, get_xtagged_name, print_tagged_value);
    }
    GCPRINT(GCOUTF, "End Finalizations\n");
    --avoid_collection;
  }
#endif
  if (for_each_found)
    --avoid_collection;
}

void GC_dump(void)
{
  GC_dump_with_traces(0, NULL, NULL, NULL, 0, NULL, 0);
}

