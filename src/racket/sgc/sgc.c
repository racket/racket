/*
  SenoraGC, a relatively portable conservative GC for a slightly
    cooperative environment
  Copyright (c) 2004-2013 PLT Design Inc.
  Copyright (c) 1996-98 Matthew Flatt
  All rights reserved.

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

  After Boehm et al.

  Note: this implementation is probably still a little hardwired for
  32-bit addresses.

 */

#include <stdlib.h>
#include <setjmp.h>
#include <stdio.h>
#include <string.h>
#include "../sconfig.h"
#include "mzconfig.h"
#include "sgc.h"

#ifdef _WIN64
# define SIZEOF_LONG 8
# ifdef _MSC_VER
#  define inline _inline
# endif
#endif

#ifdef SIZEOF_LONG
# if SIZEOF_LONG == 8
#  define SIXTY_FOUR_BIT_INTEGERS
# endif
#endif

/****************************************************************************/
/* Option bundles                                                           */
/****************************************************************************/

#ifndef SGC_STD_DEBUGGING
# define SGC_STD_DEBUGGING 0
#endif

#ifdef WIN32
# define SGC_STD_DEBUGGING_UNIX 0
# define SGC_STD_DEBUGGING_WINDOWS SGC_STD_DEBUGGING
#else
# define SGC_STD_DEBUGGING_UNIX SGC_STD_DEBUGGING
# define SGC_STD_DEBUGGING_WINDOWS 0
#endif

#ifndef SGC_AUTO_ROOTS
# define SGC_AUTO_ROOTS 0
#endif

/****************************************************************************/
/* Options and debugging flags                                              */
/****************************************************************************/

#define NO_COLLECTIONS 0
/* Disable all collections */

#define NO_DISAPPEARING 0
/* Never perform disappearing link */

#define NO_FINALIZING 0
/* Never invoke queued finalizers */

#define NO_ATOMIC 0
/* Never treat allocated data as atomic */

#define KEEP_BLOCKS_FOREVER 0
/* Number of block sector assignments to keep indefinitely, rather
   than recycling them immediately. This speeds up collection
   and allocation, but it also costs memory via fragmentation. */

#define NO_STACK_OFFBYONE 0
/* Don't notice a stack-based pointer just past the end of an object */

#define WATCH_FOR_FINALIZATION_CYCLES 0
/* Report cycles in finalizable chunks */

#define USE_GC_FREE_SPACE_DIVISOR 1
/* Grow stack in a way similar to Boehm's GC using the global
   GC_free_space_divisor. 0 => GC after allocating twice the
   active size from the last GC. */

#define PROVIDE_GC_FREE 0
/* Provide GC_free; automatically implies DISTINGUISH_FREE_FROM_UNMARKED */

#define PROVIDE_CHUNK_GC_FREE 1
/* Provide GC_free that only works on chunks (i.e., large allocation
   blocks); frees on small blacks are ignored */

#define PROVIDE_MALLOC_AND_FREE 0
/* Defines malloc(), realloc(), calloc(), and  free() in terms of 
   the collector. Platform-specific allocation routines (e.g., sbrk())
   must then be used for low-level allocations by the collector;
   turn on one of: GET_MEM_VIA_SBRK or GET_MEM_VIA_MMAP.
   This will not work if fprintf uses malloc of free. Turning on
   FPRINTF_USE_PRIM_STRINGOUT can solve this problem.
   Automatically implies PROVIDE_GC_FREE, but adds extra checks to
   CHECK_FREES if PROVIDE_GC_FREE was not otherwise on */

#define GET_MEM_VIA_SBRK 0
/* Instead of calling malloc() to get low-level memory, use
   sbrk() directly. (Unix) */

#define GET_MEM_VIA_MMAP SGC_STD_DEBUGGING_UNIX
/* Instead of calling malloc() to get low-level memory, use
   mmap() directly. (Unix) */

#define GET_MEM_VIA_VIRTUAL_ALLOC SGC_STD_DEBUGGING_WINDOWS
/* Instead of calling malloc() to get low-level memory, use
   VirtualAlloc() directly. (Win32) */

#define RELEASE_UNUSED_SECTORS 1
/* Instead of managing a list of unused sectors, they are
   given back to the OS. This only works with mmap(). */

#define DISTINGUISH_FREE_FROM_UNMARKED 0
/* Don't let conservatism resurrect a previously-collected block */

#define TERSE_MEMORY_TRACING 0
/* Automatically implies ALLOW_TRACE_COUNT, ALLOW_TRACE_PATH,
   ALLOW_SET_FINALIZER, CHECK_SKIP_MARK_AT_FIRST, and CHECK_WATCH_FOR_PTR_ALLOC */

#define STD_MEMORY_TRACING SGC_STD_DEBUGGING
/* Automatically implies TERSE_MEMORY_TRACING, DUMP_BLOCK_COUNTS,
   and DUMP_SECTOR_MAP */

#define DETAIL_MEMORY_TRACING 0
/* Automatically implies STD_MEMORY_TRACING, DUMP_BLOCK_MAPS, 
   STAMP_AND_REMEMBER_SOURCE, and KEEP_DETAIL_PATH */

#define STAMP_AND_REMEMBER_SOURCE 0
/* Keep timestamps and source of marking from collection */

#define ALLOW_TRACE_COUNT 0
/* Support collection-based trace count callbacks */

#define ALLOW_TRACE_PATH 0
/* Support collection-based trace path callbacks */

#define KEEP_DETAIL_PATH SGC_STD_DEBUGGING
/* Keep source offsets for path traces */

#define CHECK_SKIP_MARK_AT_FIRST 0
/* Enables skipping certain marks during collection from the inital
   root supplied as GC_initial_trace_root */

#define ALLOW_SET_LOCKING 0
/* Enables locking out collections for a specific set. */

#define ALLOW_SET_FINALIZER 0
/* Support the per-set custom "de-allocation" callback. */

#define CHECK_WATCH_FOR_PTR_ALLOC SGC_STD_DEBUGGING
/* Set GC_watch_for_ptr to be ~(ptr value);
   there are 3 places where the ptr is checked, 
   unless USE_WATCH_FOUND_FUNC is on */

#define USE_WATCH_FOUND_FUNC SGC_STD_DEBUGGING
/* Calls GC_found_watch when the watch-for ptr is found. */

#define PAD_BOUNDARY_BYTES SGC_STD_DEBUGGING
/* Put a known padding pattern around every allocated
   block to test for array overflow/underflow.
   Pad-testing is performed at the beginning of every GC.
   Automatically implies CHECK_SIMPLE_INTERIOR_POINTERS */

#define CHECK_SIMPLE_INTERIOR_POINTERS 0
/* Recognize pointers into the middle of an allocated
   block, (but do not recognize pointers just past the
   end of an allocated block, as is generally performed
   for stack-based pointers). */

#define DUMP_BLOCK_COUNTS 1
/* GC_dump prints detail information about block and
   set size contents. */

#define DUMP_SECTOR_MAP 1
/* GC_dump prints detail information about existing
   sectors. */
#ifdef SIXTY_FOUR_BIT_INTEGERS
# undef DUMP_SECTOR_MAP
# define DUMP_SECTOR_MAP 0
#endif

#define DUMP_BLOCK_MAPS 1 /* 0 */
/* GC_dump prints detail information about block and
   set address contents. Automatically implies
   DUMP_BLOCK_COUNTS. */

#define CHECK_FREES SGC_STD_DEBUGGING
/* Print an error for redundant frees by calling free_error */

#define FPRINTF_USE_PRIM_STRINGOUT SGC_STD_DEBUGGING_WINDOWS
/* Avoids using fprintf while the GC is running, printing
   messages instead as pure strings passed to
    void GC_prim_stringout(char *s, int len);
   which must be provided at link time, or one of
   PRIM_STRINGOUT_AS_FWRITE or PRIM_STRINGOUT_AS_WINDOWS_CONSOLE */

#define PRIM_STRINGOUT_AS_FWRITE 0
/* Implements GC_prim_stringout using fwrite. Not likely to
   solve any problems, but useful for debugging FPRINTF. */

#define PRIM_STRINGOUT_AS_WINDOWS_CONSOLE SGC_STD_DEBUGGING_WINDOWS
/* Implements GC_prim_stringout using Windows console
   functions. */

#define AUTO_STATIC_ROOTS_IF_POSSIBLE SGC_AUTO_ROOTS
/* Automatically registers static C variables as roots if
   platform-specific code is porvided */

#define PRINT_INFO_PER_GC SGC_STD_DEBUGGING
/* Writes to stderr before an after each GC, summarizing
   the state of memory and current system time at each point. */

#define SHOW_SECTOR_MAPS_AT_GC 0
/* Write a sector map before and after each GC. This is helpful for
   noticing unusual memory pattens, such as allocations of large
   blocks or unusually repetitive allocations. Only works with
   PRINT_INFO_PER_GC. */

/****************************************************************************/
/* Parameters and platform-specific settings                                */
/****************************************************************************/

/* GC frequency: MEM_USE_FACTOR is max factor between current
   allocated bytes and alocated bytes after last GC. */
#ifdef SMALL_HASH_TABLES
# define FIRST_GC_LIMIT 20000
# define MEM_USE_FACTOR 1.40
#else
# define FIRST_GC_LIMIT 100000
# define MEM_USE_FACTOR 3
#endif

#ifdef DOS_FAR_POINTERS
# include <dos.h>
# include <alloc.h>
# define PTR_TO_INT(v) (((((intptr_t)FP_SEG(v)) & 0xFFFF) << 4) + FP_OFF(v))
# define INT_TO_PTR(v) ((void *)((((v) >> 4) << 16) + ((v) & 0xF)))
# if !PROVIDE_MALLOC_AND_FREE
#  define MALLOC farmalloc
#  define FREE farfree
# endif
#else
# define PTR_TO_INT(v) ((uintptr_t)(v))
# define INT_TO_PTR(v) ((void *)(v))
# if !PROVIDE_MALLOC_AND_FREE
#  define MALLOC malloc
#  define FREE free
# endif
#endif

#if GET_MEM_VIA_SBRK
# include <unistd.h>
#endif
#if GET_MEM_VIA_MMAP
# include <unistd.h>
# include <sys/mman.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <fcntl.h>
#endif
#ifdef WIN32
# include <windows.h>
#endif

#if !GET_MEM_VIA_MMAP
# undef RELEASE_UNUSED_SECTORS
# define RELEASE_UNUSED_SECTORS 0
#endif

/* System-specific alignment of pointers. */
#ifdef SIXTY_FOUR_BIT_INTEGERS
# define PTR_ALIGNMENT 8
# define LOG_PTR_SIZE 3
# define LOW_32_BITS(x) (x & 0xFFFFFFFF)
#else
# define PTR_ALIGNMENT 4
# define LOG_PTR_SIZE 2
# define LOW_32_BITS(x) x
#endif
#define PTR_SIZE (1 << LOG_PTR_SIZE)

#ifdef _WIN64
# define ALLOC_ALIGNMENT 16
#else
# define ALLOC_ALIGNMENT PTR_SIZE
#endif

#define DOUBLE_SIZE sizeof(double)

/* SECTOR_SEGMENT_SIZE determines the alignment of collector blocks.
   Since it should be a power of 2, LOG_SECTOR_SEGMENT_SIZE is
   specified directly. A larger block size speeds up GC, but wastes
   more unallocated bytes in same-size buckets. */
#define LOG_SECTOR_SEGMENT_SIZE 12
#define SECTOR_SEGMENT_SIZE (1 << LOG_SECTOR_SEGMENT_SIZE)
#define SECTOR_SEGMENT_MASK (~(SECTOR_SEGMENT_SIZE-1))

/* MAX_COMMON_SIZE is maximum size of allocated blocks
   using relatively efficient memory layouts. */
#define MAX_COMMON_SIZE (SECTOR_SEGMENT_SIZE >> 2)

#define NUM_COMMON_SIZE ((2 * LOG_SECTOR_SEGMENT_SIZE) + 8)

/* Number of sector segments to be allocated at once with
   malloc() to avoid waste when obtaining the proper alignment. */
#define SECTOR_SEGMENT_GROUP_SIZE 32

/* Number of bits used in 32-bit level table for checking existence of
   a sector. Creates a table of (1 << SECTOR_LOOKUP_SHIFT) pointers
   to individual page tables of size SECTOR_LOOKUP_PAGESIZE. */
#define SECTOR_LOOKUP_PAGESETBITS 12

#define LOG_MAP_PTR_SIZE 2
#define MAP_PTR_SIZE 4

#define SECTOR_LOOKUP_SHIFT ((MAP_PTR_SIZE*8) - SECTOR_LOOKUP_PAGESETBITS)
#define LOG_SECTOR_LOOKUP_PAGESIZE ((MAP_PTR_SIZE*8) - SECTOR_LOOKUP_PAGESETBITS - LOG_SECTOR_SEGMENT_SIZE)
#define SECTOR_LOOKUP_PAGESIZE (1 << LOG_SECTOR_LOOKUP_PAGESIZE)
#define SECTOR_LOOKUP_PAGEMASK (SECTOR_LOOKUP_PAGESIZE - 1)

#define SECTOR_LOOKUP_PAGETABLE(x) (LOW_32_BITS(x) >> SECTOR_LOOKUP_SHIFT)
#define SECTOR_LOOKUP_PAGEPOS(x) ((LOW_32_BITS(x) >> LOG_SECTOR_SEGMENT_SIZE) & SECTOR_LOOKUP_PAGEMASK)

#define LOG_SECTOR_PAGEREC_SIZE (LOG_PTR_SIZE + 1)

/***************************************************************************/

/* Implementation Terminology:
    "sector" - A low-level block of memory. Given an arbitrary
               pointer value, whether it is contained in a sector and
	       the starting point of the sector can be determined in
	       constant time.
    "segment" - A portion of a sector, aligned on SECTOR_SEGMENT_SIZE 
               boundaries.
    "page" - part of a table for a (partial) mapping from addresses to 
               segments
    "block" or "common block" - A block for small memory allocations. Blocks
               provide allocation by partitioning a sector.
    "chunk" - A block of memory too large to be a common block. Each chunk
               is allocated in its own sector.
    "set" - A collection of blocks & chunks asscoaited with a particular
               name, "deallocation" function, trace function, etc.
*/

/***************************************************************************/

/* Debugging the collector: */
#define CHECK 0
#define PRINT 0
#define TIME 0
#define ALWAYS_TRACE 0
#define CHECK_COLLECTING 0
#define MARK_STATS 0
#define ALLOC_STATS 0
#define FINISH_STATS 0

#if DETAIL_MEMORY_TRACING
# undef STD_MEMORY_TRACING
# undef STAMP_AND_REMEMBER_SOURCE
# undef DUMP_BLOCK_MAPS
# undef KEEP_DETAIL_PATH
# define STD_MEMORY_TRACING 1
# define STAMP_AND_REMEMBER_SOURCE 1
# define DUMP_BLOCK_MAPS 1
# define KEEP_DETAIL_PATH 1
#endif

#if STD_MEMORY_TRACING
# undef TERSE_MEMORY_TRACING
# undef DUMP_BLOCK_COUNTS
# define TERSE_MEMORY_TRACING 1
# define DUMP_BLOCK_COUNTS 1
#endif

#if TERSE_MEMORY_TRACING
# undef ALLOW_TRACE_COUNT
# undef ALLOW_TRACE_PATH
# undef ALLOW_SET_FINALIZER
# undef CHECK_WATCH_FOR_PTR_ALLOC
# undef CHECK_SKIP_MARK_AT_FIRST
# define ALLOW_TRACE_COUNT 1
# define ALLOW_TRACE_PATH 1
# define ALLOW_SET_FINALIZER 1
# define CHECK_WATCH_FOR_PTR_ALLOC 1
# define CHECK_SKIP_MARK_AT_FIRST 1
#endif

#if PAD_BOUNDARY_BYTES
# undef CHECK_SIMPLE_INTERIOR_POINTERS
# define CHECK_SIMPLE_INTERIOR_POINTERS 1
#endif

#if DUMP_BLOCK_MAPS
# undef DUMP_BLOCK_COUNTS
# define DUMP_BLOCK_COUNTS 1
#endif

#if PROVIDE_MALLOC_AND_FREE
# if !PROVIDE_GC_FREE
#  define EXTRA_FREE_CHECKS 1
# endif
# undef PROVIDE_GC_FREE
# define PROVIDE_GC_FREE 1
#else
# define EXTRA_FREE_CHECKS 0
#endif

#if PROVIDE_GC_FREE
# undef DISTINGUISH_FREE_FROM_UNMARKED
# define DISTINGUISH_FREE_FROM_UNMARKED 1
# undef PROVIDE_CHUNK_GC_FREE
# define PROVIDE_CHUNK_GC_FREE 1
#endif

#if ALLOW_TRACE_COUNT || ALLOW_TRACE_PATH || PROVIDE_GC_FREE
# define KEEP_SET_NO 1
# define KEEP_CHUNK_SET_NO 1
#else
# define KEEP_SET_NO 0
# if PROVIDE_CHUNK_GC_FREE
#  define KEEP_CHUNK_SET_NO 1
# endif
#endif

#if PROVIDE_CHUNK_GC_FREE
# define KEEP_PREV_PTR 1
#else
# define KEEP_PREV_PTR 0
#endif

#ifndef NULL
# define NULL 0L
#endif

#if PAD_BOUNDARY_BYTES
/* Pad start & end must be a multiple of DOUBLE_SIZE */
# define PAD_START_SIZE (2 * sizeof(intptr_t))
# define PAD_END_SIZE PAD_START_SIZE
# define PAD_PATTERN 0x7c8c9cAc
# define PAD_FILL_PATTERN 0xc7
# define SET_PAD(p, s, os) set_pad(p, s, os)
# define PAD_FORWARD(p) ((void *)(((char *)p) + PAD_START_SIZE))
# define PAD_BACKWARD(p) ((void *)(((char *)p) - PAD_START_SIZE))
#else
# define PAD_FORWARD(p) (p)
# define PAD_BACKWARD(p) (p)
#endif

/* A root to start with, when non-zero. Useful for tracing from a
   particular object. The skip function is used when
   CHECK_SKIP_MARK_AT_FIRST to skip certain objects when marking from
   this root (when the function return 1) */
void *GC_initial_trace_root;
int (*GC_inital_root_skip)(void *, size_t);

void (*GC_out_of_memory)(void);

/* Sector types: */
enum {
  sector_kind_block,
  sector_kind_free,
#if !RELEASE_UNUSED_SECTORS
  sector_kind_freed,
#else
# define sector_kind_freed sector_kind_free
#endif
  sector_kind_chunk,
  sector_kind_managed,
  sector_kind_other
};

typedef struct BlockOfMemory {
  struct Finalizer *finalizers;
  uintptr_t start;
  uintptr_t end;
  uintptr_t top;
  short size;
  short atomic;
  short elem_per_block;
  short free_search_start, free_search_bit, free_search_offset;
#if KEEP_SET_NO
  short set_no;
#endif
  int *positions; /* maps displacement in ptrs => position in objects */
  struct BlockOfMemory *next;
#if STAMP_AND_REMEMBER_SOURCE
  intptr_t make_time;
  intptr_t use_time;
  uintptr_t low_marker;
  uintptr_t high_marker;
#endif
  unsigned char free[1];
} BlockOfMemory;

#if DISTINGUISH_FREE_FROM_UNMARKED

# define FREE_BIT_PER_ELEM 4
# define LOG_FREE_BIT_PER_ELEM 2
# define FREE_BIT_SIZE (8 >> LOG_FREE_BIT_PER_ELEM)
# define FREE_BIT_START 0x2 
# define UNMARK_BIT_START 0x1

# define POS_TO_FREE_INDEX(p) (p >> LOG_FREE_BIT_PER_ELEM)
# define POS_TO_UNMARK_INDEX(p) (p >> LOG_FREE_BIT_PER_ELEM)
# define POS_TO_FREE_BIT(p) (FREE_BIT_START << ((p & (FREE_BIT_PER_ELEM - 1)) << 1))
# define POS_TO_UNMARK_BIT(p) (UNMARK_BIT_START << ((p & (FREE_BIT_PER_ELEM - 1)) << 1))

# define ALL_UNMARKED 0x55
# define ALL_FREE 0xAA

# define _NOT_FREE(x) NOT_FREE(x)

# define SHIFT_UNMARK_TO_FREE(x) ((x & ALL_UNMARKED) << 1)
# define SHIFT_COPY_FREE_TO_UNMARKED(x) ((x & ALL_FREE) | ((x & ALL_FREE) >> 1))

#else /* !DISTINGUISH_FREE_FROM_UNMARKED */

# define FREE_BIT_PER_ELEM 8
# define LOG_FREE_BIT_PER_ELEM 3
# define FREE_BIT_SIZE (8 >> LOG_FREE_BIT_PER_ELEM)
# define FREE_BIT_START 0x1
# define UNMARK_BIT_START 0x1

# define POS_TO_FREE_INDEX(p) (p >> LOG_FREE_BIT_PER_ELEM)
# define POS_TO_UNMARK_INDEX(p) (p >> LOG_FREE_BIT_PER_ELEM)
# define POS_TO_FREE_BIT(p) (FREE_BIT_START << (p & (FREE_BIT_PER_ELEM - 1)))
# define POS_TO_UNMARK_BIT(p) (UNMARK_BIT_START << (p & (FREE_BIT_PER_ELEM - 1)))

# define ALL_UNMARKED 0xFF

# define _NOT_FREE(x) 1

#endif /* DISTINGUISH_FREE_FROM_UNMARKED */

#define NOT_FREE(x) (!(x))
#define IS_FREE(x) (x)
#define NOT_MARKED(x) (x)
#define IS_MARKED(x) (!(x))

#define ELEM_PER_BLOCK(b) b->elem_per_block

typedef struct MemoryChunk {
  struct Finalizer *finalizers;
  uintptr_t start;
  uintptr_t end;
  struct MemoryChunk *next;
#if KEEP_PREV_PTR
  struct MemoryChunk **prev_ptr;
#endif
  short atomic;
  short marked;
#if STAMP_AND_REMEMBER_SOURCE
  intptr_t make_time;
  uintptr_t marker;
#endif
#if KEEP_CHUNK_SET_NO
  int set_no;
#endif
  char data[1];
} MemoryChunk;

/* If this changes size from 2 ptrs, change LOG_SECTOR_PAGEREC_SIZE */
typedef struct {
  intptr_t kind;  /* sector_kind_other, etc. */
  uintptr_t start; /* Sector start; may not be accurate if the segment
                          is deallocated, but 0 => not in any sector */
} SectorPage;

#if !RELEASE_UNUSED_SECTORS
# include "../utils/splay.c"

typedef struct SectorFreepage {
  intptr_t size; 
  uintptr_t start; /* Sector start */
  uintptr_t end; /* start of next */
  Tree by_start;
  Tree by_end;
  Tree by_start_per_size;
  Tree by_size;
  Tree *same_size;
} SectorFreepage;

static Tree *sector_freepage_by_start;
static Tree *sector_freepage_by_end;
static Tree *sector_freepage_by_size;

#define TREE_FP(t) ((SectorFreepage *)(t->data))

static Tree *next(Tree *node)
{
  node = node->right;
  if (node) {
    while (node->left) {
      node = node->left;
    }
    return node;
  } else
    return NULL;
}

static void remove_freepage(SectorFreepage *fp)
{
  /* Remove fp from freelists: */
  sector_freepage_by_start = splay_delete(fp->start, sector_freepage_by_start);
  sector_freepage_by_end = splay_delete(fp->end, sector_freepage_by_end);
  sector_freepage_by_size = splay(fp->size, sector_freepage_by_size);
  if (TREE_FP(sector_freepage_by_size) == fp) {
    /* This was the representative for its size; remove it. */
    sector_freepage_by_size = splay_delete(fp->size, sector_freepage_by_size);
    if (fp->same_size) {
      SectorFreepage *same;
      same = TREE_FP(fp->same_size);
      same->same_size = splay_delete(same->start, fp->same_size);
      sector_freepage_by_size = splay_insert(same->size, &same->by_size, sector_freepage_by_size);
    }
  } else {
    /* Not the top-level representative; remove it from the representative's
       same_size tree */
    SectorFreepage *same;
    same = TREE_FP(sector_freepage_by_size);
    same->same_size = splay_delete(fp->start, same->same_size);
  }
}

static void add_freepage(SectorFreepage *naya)
{
  naya->by_start.data = (void *)naya;
  sector_freepage_by_start = splay_insert(naya->start, &naya->by_start, sector_freepage_by_start);
  naya->by_end.data = (void *)naya;
  sector_freepage_by_end = splay_insert(naya->end, &naya->by_end, sector_freepage_by_end);
  naya->by_size.data = (void *)naya;
  sector_freepage_by_size = splay_insert(naya->size, &naya->by_size, sector_freepage_by_size);
  if (TREE_FP(sector_freepage_by_size) != naya) {
    /* This size was already in the tree; add it to the next_size list, instead */
    SectorFreepage *already = TREE_FP(sector_freepage_by_size);
    naya->by_start_per_size.data = (void *)naya;
    already->same_size = splay_insert(naya->start, &naya->by_start_per_size, already->same_size);
  } else
    naya->same_size = NULL;
}
#endif /* !RELEASE_UNUSED_SECTORS */

#define TABLE_HI_SHIFT LOG_SECTOR_SEGMENT_SIZE
#define TABLE_LO_MASK (SECTOR_SEGMENT_SIZE-1)
#define EACH_TABLE_COUNT (1 << (LOG_SECTOR_SEGMENT_SIZE - LOG_PTR_SIZE))

typedef struct GC_Set {
  short atomic, uncollectable;
#if ALLOW_SET_LOCKING
  short locked;
#endif
  char *name;
  BlockOfMemory **blocks;
  BlockOfMemory **block_ends;
  MemoryChunk **othersptr;
#if DUMP_BLOCK_COUNTS
  uintptr_t total;
#endif
#if ALLOW_TRACE_COUNT
  GC_count_tracer count_tracer;
#endif
#if ALLOW_TRACE_PATH
  GC_path_tracer path_tracer;
#endif
#if ALLOW_TRACE_COUNT || ALLOW_TRACE_PATH
  GC_trace_init trace_init;
  GC_trace_done trace_done;
#endif
#if KEEP_SET_NO || KEEP_CHUNK_SET_NO
  int no;
#endif
#if ALLOW_SET_FINALIZER
  GC_set_elem_finalizer finalizer;
#endif
} GC_Set;

typedef struct GC_SetWithOthers {
  GC_Set c;
  MemoryChunk *others;
} GC_SetWithOthers;

static GC_Set **common_sets;
static int num_common_sets;

static BlockOfMemory *common[2 * NUM_COMMON_SIZE]; /* second half is `ends' array */
static BlockOfMemory *atomic_common[2 * NUM_COMMON_SIZE];
static BlockOfMemory *uncollectable_common[2 * NUM_COMMON_SIZE];
static BlockOfMemory *uncollectable_atomic_common[2 * NUM_COMMON_SIZE];
static MemoryChunk *others, *atomic_others;
static MemoryChunk *uncollectable_others, *uncollectable_atomic_others;

static int *common_positionses[NUM_COMMON_SIZE];

#if PROVIDE_MALLOC_AND_FREE
static BlockOfMemory *sys_malloc[2 * NUM_COMMON_SIZE];
static MemoryChunk *sys_malloc_others;
#endif

#define do_malloc_ATOMIC 0x1
#define do_malloc_UNCOLLECTABLE 0x2
#if NO_ATOMIC
# define do_malloc_ATOMIC_UNLESS_DISABLED 0
#else
# define do_malloc_ATOMIC_UNLESS_DISABLED do_malloc_ATOMIC
#endif

int GC_dl_entries;
int GC_fo_entries;

int GC_dont_gc;

void (*GC_push_last_roots)(void);
void (*GC_push_last_roots_again)(void);

enum {
  dl_normal,
  dl_restored,
  dl_late
};

typedef struct DisappearingLink {
  short kind;
  void *watch;
  void **disappear;
  void *saved_value;
  struct DisappearingLink *prev, *next;
} DisappearingLink;

typedef struct Finalizer {
  union {
    void *watch; /* pointer to finalized block; used after queued */
    int pos;     /* position within common block; used before queued */
  } u;
  short eager_level;
  short ignore_self;
  void *data;
  void (*f)(void *p, void *data);
  struct Finalizer *prev, *next; /* also not needed for chunks */
} Finalizer;

static DisappearingLink *disappearing, *late_disappearing;
static Finalizer *queued_finalizers, *last_queued_finalizer;
static int num_queued_finalizers;

static uintptr_t sector_low_plausible, sector_high_plausible;
static uintptr_t low_plausible, high_plausible;

void *GC_stackbottom;

static intptr_t mem_use, mem_limit = FIRST_GC_LIMIT;
#if USE_GC_FREE_SPACE_DIVISOR
int GC_free_space_divisor = 4;
#endif

static intptr_t mem_real_use, mem_uncollectable_use;

static intptr_t sector_mem_use, sector_admin_mem_use, sector_free_mem_use;
static intptr_t manage_mem_use, manage_real_mem_use;

static intptr_t collect_mem_use;

static intptr_t num_sector_allocs, num_sector_frees;

#if ALLOW_TRACE_COUNT
static intptr_t mem_traced;
#endif

static intptr_t num_chunks;
static intptr_t num_blocks;

GC_collect_start_callback_Proc GC_collect_start_callback;
GC_collect_end_callback_Proc GC_collect_end_callback;
void (*GC_custom_finalize)(void);

GC_collect_start_callback_Proc GC_set_collect_start_callback(GC_collect_start_callback_Proc func) {
  GC_collect_start_callback_Proc old;
  old = GC_collect_start_callback;
  GC_collect_start_callback = func;
  return old;
}
GC_collect_end_callback_Proc GC_set_collect_end_callback(GC_collect_end_callback_Proc func) {
  GC_collect_end_callback_Proc old;
  old = GC_collect_end_callback;
  GC_collect_end_callback = func;
  return old;
}


static intptr_t roots_count;
static intptr_t roots_size;
static uintptr_t *roots;

#if STAMP_AND_REMEMBER_SOURCE
static intptr_t stamp_clock = 0;
#endif

static intptr_t *size_index_map; /* (1/PTR_SIZE)th of requested size to alloc index */
static intptr_t *size_map; /* alloc index to alloc size */

#if CHECK_COLLECTING
static int collecting_now;
#endif

#if FPRINTF_USE_PRIM_STRINGOUT
static void sgc_fprintf(int, const char *, ...);
# define FPRINTF sgc_fprintf
# define STDERR 0
#else
# define FPRINTF fprintf
# define STDERR stderr
#endif

#if CHECK_FREES
static void free_error(const char *msg)
{
  FPRINTF(STDERR, msg);
}
#endif

/*************************************************************/
/* Page mapping: */

#ifdef SIXTY_FOUR_BIT_INTEGERS
static SectorPage ***sector_pagetablesss[1 << 16];
# define DECL_SECTOR_PAGETABLES SectorPage **sector_pagetables;
# define GET_SECTOR_PAGETABLES(p) sector_pagetables = create_sector_pagetables(p)
# define FIND_SECTOR_PAGETABLES(p) sector_pagetables = get_sector_pagetables(p)
static void *malloc_plain_sector(int count);
inline static SectorPage **create_sector_pagetables(uintptr_t p) {
  uintptr_t pos;
  SectorPage ***sector_pagetabless, **sector_pagetables;
  pos = ((uintptr_t)p >> 48) & ((1 << 16) - 1);
  sector_pagetabless = sector_pagetablesss[pos];
  if (!sector_pagetabless) {
    int c = (sizeof(SectorPage **) << 16) >> LOG_SECTOR_SEGMENT_SIZE;
    sector_pagetabless = (SectorPage ***)malloc_plain_sector(c);
    memset(sector_pagetabless, 0, c << LOG_SECTOR_SEGMENT_SIZE);
    sector_pagetablesss[pos] = sector_pagetabless;
    sector_admin_mem_use += (c << LOG_SECTOR_SEGMENT_SIZE);
  }
  pos = ((uintptr_t)p >> 32) & ((1 << 16) - 1);
  sector_pagetables = sector_pagetabless[pos];
  if (!sector_pagetables) {
    int c = (SECTOR_LOOKUP_PAGESETBITS + LOG_PTR_SIZE) - LOG_SECTOR_SEGMENT_SIZE;
    if (c < 0)
      c = 0;
    c = 1 << c;
    sector_pagetables = (SectorPage **)malloc_plain_sector(c);
    memset(sector_pagetables, 0, c << LOG_SECTOR_SEGMENT_SIZE);
    sector_admin_mem_use += (c << LOG_SECTOR_SEGMENT_SIZE);
    sector_pagetabless[pos] = sector_pagetables;
  }
  return sector_pagetables;
}
inline static SectorPage **get_sector_pagetables(uintptr_t p) {
  uintptr_t pos;
  SectorPage ***sector_pagetabless;
  pos = ((uintptr_t)p >> 48) & ((1 << 16) - 1);
  sector_pagetabless = sector_pagetablesss[pos];
  if (!sector_pagetabless)
    return NULL;
  pos = ((uintptr_t)p >> 32) & ((1 << 16) - 1);
  return sector_pagetabless[pos];
}
#else
static SectorPage **sector_pagetables;
# define DECL_SECTOR_PAGETABLES /**/
# define GET_SECTOR_PAGETABLES(p) /**/
# define FIND_SECTOR_PAGETABLES(p) /**/
#endif

/*************************************************************/

/* 
   The kinds of allocation:
   
     malloc_sector = returns new SECTOR_SEGMENT_SIZE-aligned memory;
                     relies on nothing else; the memeory blocks must
		     be explicitly freed with free_sector; all GC
		     allocation is perfomed via sectors
     
     malloc_managed = malloc "atomic" block used by GC implementation
                      itself; no GCing should occur during the malloc;
		      the block is freed with free_managed

     realloc_collect_temp = temporary structures used during gc;
                            no other allocation can take place
			    during gc, and all memory will be freed
                            when GC is done with free_collect_temp
*/

#if GET_MEM_VIA_SBRK
static void *platform_plain_sector(int count)
{
  caddr_t cur_brk = (caddr_t)sbrk(0);
  intptr_t lsbs = (uintptr_t)cur_brk & TABLE_LO_MASK;
  void *result;
    
  if (lsbs != 0) {
    if ((caddr_t)sbrk(SECTOR_SEGMENT_SIZE - lsbs) == (caddr_t)(-1)) 
      return 0;
  }

  result = (caddr_t)sbrk((count << LOG_SECTOR_SEGMENT_SIZE));

  if (result == (caddr_t)(-1)) 
    return 0;

  return result;
}
#endif
#if GET_MEM_VIA_MMAP
static void *platform_plain_sector(int count)
{
#ifdef MAP_ANON
  return mmap(NULL, count << LOG_SECTOR_SEGMENT_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
#else
  static int fd;

  if (!fd) {
    fd = open("/dev/zero", O_RDWR);
  }
  
  return mmap(0, count << LOG_SECTOR_SEGMENT_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
#endif
}

static void free_plain_sector(void *p, int count)
{
  munmap(p, count << LOG_SECTOR_SEGMENT_SIZE);
}
#endif
#if GET_MEM_VIA_VIRTUAL_ALLOC
static void *platform_plain_sector(int count)
{
  /* Since 64k blocks are used up by each call to VirtualAlloc,
     use roughly the same trick as in the malloc-based alloc to
     avoid wasting the address space. */

  static int prealloced;
  static void *preallocptr;
  
  if (!prealloced && (count < SECTOR_SEGMENT_GROUP_SIZE)) {
    prealloced = SECTOR_SEGMENT_GROUP_SIZE;
    preallocptr = VirtualAlloc(NULL, prealloced << LOG_SECTOR_SEGMENT_SIZE,
			       MEM_COMMIT | MEM_RESERVE,
			       PAGE_READWRITE);		
  }
  
  if (count <= prealloced) {
    void *result = preallocptr;
    preallocptr = ((char *)preallocptr) + (count << LOG_SECTOR_SEGMENT_SIZE);
    prealloced -= count;
    return result;
  }
  
  return VirtualAlloc(NULL, count << LOG_SECTOR_SEGMENT_SIZE,
		      MEM_COMMIT | MEM_RESERVE,
		      PAGE_READWRITE);
}
#endif

#if !GET_MEM_VIA_SBRK && !GET_MEM_VIA_MMAP && !GET_MEM_VIA_VIRTUAL_ALLOC
# if PROVIDE_MALLOC_AND_FREE
  >> 
  Error: you must pick a platform-specific allocation mechanism
  to support malloc() and free() 
  <<
# endif

static void *platform_plain_sector(int count)
{
  static int prealloced;
  static void *preallocptr;

  if (!prealloced) {
    uintptr_t d;

    if (count <= (SECTOR_SEGMENT_GROUP_SIZE-1))
      prealloced = SECTOR_SEGMENT_GROUP_SIZE-1;
    else
      prealloced = count;

    preallocptr = MALLOC((prealloced + 1) << LOG_SECTOR_SEGMENT_SIZE);

    d = ((uintptr_t)preallocptr) & TABLE_LO_MASK;
    if (d)
      preallocptr = ((char *)preallocptr) + (SECTOR_SEGMENT_SIZE - d);
  }

  if (prealloced >= count) {
    void *r = preallocptr;

    prealloced -= count;
    preallocptr = ((char *)preallocptr) + (count << LOG_SECTOR_SEGMENT_SIZE);
    
    return r;
  }

  {
    uintptr_t d;
    void *r;

    r = MALLOC((count + 1) << LOG_SECTOR_SEGMENT_SIZE);

    d = ((uintptr_t)r) & TABLE_LO_MASK;
    if (d)
      r = ((char *)r) + (SECTOR_SEGMENT_SIZE - d);

    return r;
  }
}
#endif

static void *malloc_plain_sector(int count)
{
  void *m;

  m = platform_plain_sector(count);

  if (!m) {
    if (GC_out_of_memory)
      GC_out_of_memory();
    FPRINTF(STDERR, "out of memory\n");
    exit(-1);
  }

  return m;
}

static void register_sector(void *naya, int need, intptr_t kind)
{
  uintptr_t ns, orig_ns;
  int pagetableindex, pageindex, i;
  SectorPage *pagetable;
  DECL_SECTOR_PAGETABLES;

  orig_ns = ns = PTR_TO_INT(naya);
  if (!sector_low_plausible || (ns < sector_low_plausible))
    sector_low_plausible = ns;
  if (!sector_high_plausible 
      || (ns + (need << LOG_SECTOR_SEGMENT_SIZE) > sector_high_plausible))
    sector_high_plausible = ns + (need << LOG_SECTOR_SEGMENT_SIZE);

  /* Register pages as existing: */
  for (i = need; i--; ns += SECTOR_SEGMENT_SIZE) {
    GET_SECTOR_PAGETABLES(ns);
    pagetableindex = SECTOR_LOOKUP_PAGETABLE(ns);
    pagetable = sector_pagetables[pagetableindex];
    if (!pagetable) {
      int j, c = (LOG_SECTOR_LOOKUP_PAGESIZE + LOG_SECTOR_PAGEREC_SIZE) - LOG_SECTOR_SEGMENT_SIZE;
      if (c < 0)
        c = 0;
      c = 1 << c;
      pagetable = (SectorPage *)malloc_plain_sector(c);
      sector_pagetables[pagetableindex] = pagetable;
      sector_admin_mem_use += (c << LOG_SECTOR_SEGMENT_SIZE);
      for (j = 0; j < SECTOR_LOOKUP_PAGESIZE; j++) {
       pagetable[j].start = 0; 
       pagetable[j].kind = sector_kind_free;
      }
    }

    pageindex = SECTOR_LOOKUP_PAGEPOS(ns);
    pagetable[pageindex].kind = kind;
    pagetable[pageindex].start = orig_ns;
  }
}

static void *malloc_sector(intptr_t size, intptr_t kind, int no_new)
{
  intptr_t need;
  void *naya;
#if !RELEASE_UNUSED_SECTORS
  SectorFreepage *fp;
#endif

#if CHECK_COLLECTING
  if (collecting_now) {
    free_error("alloc while collecting\n");
    return NULL;
  }
#endif

  num_sector_allocs++;

#ifndef SIXTY_FOUR_BIT_INTEGERS
  if (!sector_pagetables) {
    int i, c = (SECTOR_LOOKUP_PAGESETBITS + LOG_PTR_SIZE) - LOG_SECTOR_SEGMENT_SIZE;
    if (c < 0)
      c = 0;
    c = 1 << c;
    sector_pagetables = (SectorPage **)malloc_plain_sector(c);
    sector_admin_mem_use += (c << LOG_SECTOR_SEGMENT_SIZE);
    for (i = 0; i < (1 << SECTOR_LOOKUP_PAGESETBITS); i++)
      sector_pagetables[i] = NULL;
  }
#endif

  need = (size + SECTOR_SEGMENT_SIZE - 1) >> LOG_SECTOR_SEGMENT_SIZE;

#if !RELEASE_UNUSED_SECTORS
  if (sector_freepage_by_size) {
    sector_freepage_by_size = splay(need, sector_freepage_by_size);
    if (TREE_FP(sector_freepage_by_size)->size < need) {
      /* No exact match, so find the next size up */
      Tree *node;
      node = next(sector_freepage_by_size);
      if (node)
	fp = TREE_FP(node);
      else
	fp = NULL;
    } else 
      fp = TREE_FP(sector_freepage_by_size);
  } else
    fp = NULL;
  
  if (fp) {
    remove_freepage(fp);

    naya = INT_TO_PTR(fp->start);
    register_sector(naya, need, kind);
    if (fp->size > need) {
      /* Move freepage info and shrink */
      SectorFreepage *naya;
      uintptr_t nfp;
      nfp = fp->start + (need << LOG_SECTOR_SEGMENT_SIZE);
      naya = (SectorFreepage *)INT_TO_PTR(nfp);
      naya->size = fp->size - need;
      naya->start = nfp;
      naya->end = fp->end;

      add_freepage(naya);
    }

    sector_free_mem_use -= (need << LOG_SECTOR_SEGMENT_SIZE);
    return naya;
  }
#endif

  if (no_new)
    return NULL;

  naya = malloc_plain_sector(need);
  sector_mem_use += (need << LOG_SECTOR_SEGMENT_SIZE);
  register_sector(naya, need, kind);

  return naya;
}

static void free_sector(void *p)
{
  uintptr_t s = PTR_TO_INT(p), t;
  int c = 0;
#if !RELEASE_UNUSED_SECTORS
  SectorFreepage *fp, *ifp;
#endif

  num_sector_frees++;
  
  /* Determine the size: */
  t = s;
  while(1) {
    intptr_t pagetableindex = SECTOR_LOOKUP_PAGETABLE(t);
    intptr_t pageindex = SECTOR_LOOKUP_PAGEPOS(t);
    DECL_SECTOR_PAGETABLES;
    GET_SECTOR_PAGETABLES(t);
    if (sector_pagetables[pagetableindex]
	&& (sector_pagetables[pagetableindex][pageindex].start == s)) {
      sector_pagetables[pagetableindex][pageindex].kind = sector_kind_freed;
      sector_pagetables[pagetableindex][pageindex].start = 0;
      c++;
      t += SECTOR_SEGMENT_SIZE;
    } else
      break;
  }

#if CHECK_FREES
  if (!c) {
    free_error("bad sector free!\n");
    return;
  }
#endif

#if RELEASE_UNUSED_SECTORS
  free_plain_sector(p, c);
  sector_mem_use -= (c << LOG_SECTOR_SEGMENT_SIZE);
#else
  sector_free_mem_use += (c << LOG_SECTOR_SEGMENT_SIZE);
  if (sector_freepage_by_end) {
    /* Try merge with a predecessor: */
    sector_freepage_by_end = splay(s, sector_freepage_by_end);
    ifp = TREE_FP(sector_freepage_by_end);
    if (ifp->end == s) {
      remove_freepage(ifp);
      c += ifp->size;
      s = ifp->start;
    }
    
    if (sector_freepage_by_start) {
      /* Try merge with a successor: */
      sector_freepage_by_start = splay(t, sector_freepage_by_start);
      ifp = TREE_FP(sector_freepage_by_start);
      if (ifp->start == t) {
	remove_freepage(ifp);
	c += ifp->size;
	t = ifp->end;
      }
    }
  }
  
  fp = (SectorFreepage *)p;
  fp->start = s;
  fp->end = t;
  fp->size = c;
  add_freepage(fp);
#endif
}

#ifdef WIN32
static int is_sector_segment(void *p)
{
  uintptr_t s = PTR_TO_INT(p);
  intptr_t pagetableindex = SECTOR_LOOKUP_PAGETABLE(s);
  intptr_t pageindex = SECTOR_LOOKUP_PAGEPOS(s);
  DECL_SECTOR_PAGETABLES;

  FIND_SECTOR_PAGETABLES(p);
  if (!sector_pagetables) return 0;

  return (sector_pagetables[pagetableindex]
          && sector_pagetables[pagetableindex][pageindex].start);
}
#endif

#if GET_MEM_VIA_SBRK
static int c_refcount;
static char *save_brk;
#endif

static void prepare_collect_temp()
{
#if GET_MEM_VIA_SBRK
  save_brk = (char *)sbrk(0);
#else
  collect_mem_use = 0;
#endif
}

static void *realloc_collect_temp(void *v, intptr_t oldsize, intptr_t newsize)
{
#if GET_MEM_VIA_SBRK
  void *naya;

  naya = (void *)sbrk(newsize);
  memcpy(naya, v, oldsize);
  if (!v)
    c_refcount++;
  return naya;
#elif GET_MEM_VIA_MMAP
  void *naya;
  
  naya = platform_plain_sector((newsize + SECTOR_SEGMENT_SIZE - 1) >> LOG_SECTOR_SEGMENT_SIZE);
  memcpy(naya, v, oldsize);
  if (v)
    munmap(v, (oldsize + SECTOR_SEGMENT_SIZE - 1) >> LOG_SECTOR_SEGMENT_SIZE);

  return naya;
#elif GET_MEM_VIA_VIRTUAL_ALLOC
  void *naya;

  naya = VirtualAlloc(NULL, newsize, 
		      MEM_COMMIT | MEM_RESERVE,
		      PAGE_READWRITE);
  memcpy(naya, v, oldsize);
  if (v)
    VirtualFree(v, 0, MEM_RELEASE);

  return naya;
#else
  void *naya;

  naya = MALLOC(newsize);
  memcpy(naya, v, oldsize);
  FREE(v);
  collect_mem_use += newsize;
  return naya;
#endif
}

static void free_collect_temp(void *v, intptr_t oldsize)
{
#if GET_MEM_VIA_SBRK
  if (!(--c_refcount)) {
    collect_mem_use = (uintptr_t)(sbrk(0)) - (uintptr_t)save_brk;
    brk(save_brk);
  }
#elif GET_MEM_VIA_MMAP
  munmap(v, (oldsize + SECTOR_SEGMENT_SIZE - 1) >> LOG_SECTOR_SEGMENT_SIZE);
#elif GET_MEM_VIA_VIRTUAL_ALLOC
  VirtualFree(v, 0, MEM_RELEASE);
#else
  FREE(v);
#endif
}

typedef struct {
  struct ManagedBlock *next;
  struct ManagedBlock *prev;
  intptr_t count;
  intptr_t size; /* Use size to find bucket */
  uintptr_t end;
} ManagedBlockHeader;

typedef struct ManagedBlock {
  ManagedBlockHeader head;
  char free[1];
} ManagedBlock;

typedef struct {
  intptr_t size;
  intptr_t perblock;
  intptr_t offset;
  ManagedBlock *block;
} ManagedBucket;

typedef struct {
  int num_buckets;
  ManagedBucket buckets[1];
} Managed;

static Managed *managed;

static void *malloc_managed(intptr_t size)
{
  /* A naive strategy is sufficient here.
     There will be many disappearing links, many
     finalizations, and very little of anything else. */
  int i, j;
  intptr_t perblock, offset;
  ManagedBlock *mb;
  
  if (size & PTR_SIZE)
    size += PTR_SIZE - (size & PTR_SIZE);

  if (!managed) {
    managed = (Managed *)malloc_sector(SECTOR_SEGMENT_SIZE, sector_kind_other, 0);
    managed->num_buckets = 0;
    manage_real_mem_use += SECTOR_SEGMENT_SIZE;
  }

  for (i = 0; i < managed->num_buckets; i++) {
    if (managed->buckets[i].size == size)
      break;
  }

  if (i >= managed->num_buckets) {
    managed->num_buckets++;
    managed->buckets[i].size = size;
    if (size < MAX_COMMON_SIZE) {
      int c;

      mb = (ManagedBlock *)malloc_sector(SECTOR_SEGMENT_SIZE, sector_kind_managed, 0);
      manage_real_mem_use += SECTOR_SEGMENT_SIZE;
      managed->buckets[i].block = mb;

      c = (SECTOR_SEGMENT_SIZE - sizeof(ManagedBlockHeader)) / size;
      if (c & (PTR_SIZE - 1))
	c += (PTR_SIZE - (c & (PTR_SIZE - 1)));
      managed->buckets[i].perblock = (SECTOR_SEGMENT_SIZE - sizeof(ManagedBlockHeader) - c) / size;
      managed->buckets[i].offset = c + sizeof(ManagedBlockHeader);
    } else {
      intptr_t l = size + sizeof(ManagedBlockHeader) + PTR_SIZE;
      mb = (ManagedBlock *)malloc_sector(l, sector_kind_managed, 0);
      manage_real_mem_use += l;
      managed->buckets[i].block = mb;
      managed->buckets[i].perblock = 1;
      managed->buckets[i].offset = sizeof(ManagedBlockHeader) + PTR_SIZE;
    }
    mb->head.count = 0;
    mb->head.size = size;
    mb->head.next = NULL;
    mb->head.prev = NULL;
    perblock = managed->buckets[i].perblock;
    for (j = perblock; j--; )
      mb->free[j] = 1;
    mb->head.end = PTR_TO_INT(mb) + managed->buckets[i].offset + size * perblock;
  }

  perblock = managed->buckets[i].perblock;
  offset = managed->buckets[i].offset;
  mb = managed->buckets[i].block;
  while ((mb->head.count == perblock) && mb->head.next)
    mb = mb->head.next;
  if (mb->head.count == perblock) {
    intptr_t l = offset + size * perblock;
    mb->head.next = (ManagedBlock *)malloc_sector(l, sector_kind_managed, 0);
    manage_real_mem_use += l;
    mb->head.next->head.prev = mb;
    mb = mb->head.next;
    mb->head.count = 0;
    mb->head.size = size;
    mb->head.next = NULL;
    for (j = perblock; j--; )
      mb->free[j] = 1;
    mb->head.end = PTR_TO_INT(mb) + offset + size * perblock;
  }

  manage_mem_use += size;

  mb->head.count++;
  for (j = perblock; j--; )
    if (mb->free[j]) {
      mb->free[j] = 0;
      return (((char *)mb) + offset) + size * j;
    }

  FPRINTF(STDERR, "error allocating managed\n");
  return NULL;
}

static void free_managed(void *s)
{
  int i;
  uintptr_t p;
  ManagedBucket *bucket;
  ManagedBlock *mb;

  p = PTR_TO_INT(s);

  /* Assume that s really is an allocated managed pointer: */
  mb = (ManagedBlock *)INT_TO_PTR((p & SECTOR_SEGMENT_MASK));
  
  for (i = 0; i < managed->num_buckets; i++) {
    bucket = managed->buckets + i;
    if (bucket->size == mb->head.size) {
      /* Found bucket */
      int which;
      which = (p - PTR_TO_INT(mb) - bucket->offset) / bucket->size;
      if ((which >= 0) && (which < bucket->perblock)) {
	if (mb->free[which]) {
	  FPRINTF(STDERR, "error freeing managed\n");
	  return;
	}
	mb->free[which] = 1;
	--mb->head.count;
	manage_mem_use -= bucket->size;
	if (!mb->head.count) {
	  if (mb->head.prev) {
	    if (mb->head.next)
	      mb->head.next->head.prev = mb->head.prev;
	    mb->head.prev->head.next = mb->head.next;
	  } else {
	    if (mb->head.next) {
	      bucket->block = mb->head.next;
	      bucket->block->head.prev = NULL;
	    } else {
	      /* Empty bucket */
	      int j;
	      --managed->num_buckets;
	      for (j = i; j < managed->num_buckets; j++)
		memcpy(&(managed->buckets[j]), &(managed->buckets[j + 1]), sizeof(ManagedBucket));
	    }
	  }

	  manage_real_mem_use -= (bucket->offset + bucket->size * bucket->perblock);

	  free_sector(mb);
	}
	return;
      }
    }
  }
  
  FPRINTF(STDERR, "error freeing managed\n");
}

/*************************************************************/


static intptr_t size_align(intptr_t s) {
  if (s & (ALLOC_ALIGNMENT-1))
    return s + (ALLOC_ALIGNMENT - (s & (ALLOC_ALIGNMENT-1)));
  else
    return s;
}

static void init_size_map()
{
  int i, j, find_half;
  intptr_t k, next;

  size_index_map = (intptr_t *)malloc_sector(MAX_COMMON_SIZE, sector_kind_other, 0);
  size_map = (intptr_t *)malloc_sector(NUM_COMMON_SIZE * sizeof(intptr_t), sector_kind_other, 0);

  /* This is two loops instead of one to avoid a gcc 2.92.2 -O2 x86 bug: */
  for (i = 0; i < 8; i++) {
    size_index_map[i] = i;
  }
  for (i = 0; i < 8; i++) {
    size_map[i] = size_align((i + 1) * PTR_SIZE);
  }
  /* i's final value is used below... */

  k = 8;
  next = 12;
  j = i;
  find_half = 1;
  while (j < (MAX_COMMON_SIZE >> 2)) {
    size_index_map[j] = i;
    if ((j + 1) == next) {
      size_map[i] = size_align(next * PTR_SIZE);
      i++;
      if (find_half) {
	next = 2 * k;
      } else {
	next = 3 * k;
	k = 2 * k;
      }
      find_half = !find_half;
    }
    j++;
  }
  if (i < NUM_COMMON_SIZE)
    size_map[i] = size_align(next * PTR_SIZE);

#if 0
  FPRINTF(STDERR, "max: %d  num: %d\n", MAX_COMMON_SIZE, NUM_COMMON_SIZE);
  for (i = 0; i < (MAX_COMMON_SIZE >> 2); i++) {
    FPRINTF(STDERR, "%d->%d=%d;", i, 
	    size_index_map[i], 
	    size_map[size_index_map[i]]);
  }
  FPRINTF(STDERR, "\n");
#endif
}

/*************************************************************/

void GC_add_roots(void *start, void *end)
{
  if (roots_count >= roots_size) {
    uintptr_t *naya;

    mem_real_use -= (sizeof(uintptr_t) * roots_size);

    roots_size = roots_size ? 2 * roots_size : 500;
    naya = (uintptr_t *)malloc_managed(sizeof(uintptr_t) * (roots_size + 1));

    mem_real_use += (sizeof(uintptr_t) * roots_size);

    memcpy((void *)naya, (void *)roots, 
	   sizeof(uintptr_t) * roots_count);

    if (roots)
      free_managed(roots);

    roots = naya;
  }

  roots[roots_count++] = PTR_TO_INT(start);
  roots[roots_count++] = PTR_TO_INT(end) - PTR_ALIGNMENT;
}

#if AUTO_STATIC_ROOTS_IF_POSSIBLE
# include "autostat.inc"
#endif

static int statics_setup = 0;

static void init_static_variables(void)
{
#if AUTO_STATIC_ROOTS_IF_POSSIBLE
# if USE_DATASTARTEND
  GC_add_roots(DATASTART, DATAEND);
# endif
# ifdef WIN32
  register_static_variables();
# endif
#endif

  statics_setup = 1;
}

static int initialized = 0;

static void GC_initialize(void)
{
  int i;

#if PROVIDE_MALLOC_AND_FREE
  num_common_sets = 5;
#else
  num_common_sets = 4;
#endif
  common_sets = (GC_Set **)malloc_managed(sizeof(GC_Set*) * num_common_sets);

  common_sets[0] = (GC_Set *)malloc_managed(sizeof(GC_Set));
  common_sets[0]->atomic = 0;
  common_sets[0]->uncollectable = 0;
  common_sets[0]->blocks = common;
  common_sets[0]->block_ends = common + NUM_COMMON_SIZE;
  common_sets[0]->othersptr = &others;

  common_sets[1] = (GC_Set *)malloc_managed(sizeof(GC_Set));
  common_sets[1]->atomic = !NO_ATOMIC;
  common_sets[1]->uncollectable = 0;
  common_sets[1]->blocks = atomic_common;
  common_sets[1]->block_ends = atomic_common + NUM_COMMON_SIZE;
  common_sets[1]->othersptr = &atomic_others;

  common_sets[2] = (GC_Set *)malloc_managed(sizeof(GC_Set));
  common_sets[2]->atomic = 0;
  common_sets[2]->uncollectable = 1;
  common_sets[2]->blocks = uncollectable_common;
  common_sets[2]->block_ends = uncollectable_common + NUM_COMMON_SIZE;
  common_sets[2]->othersptr = &uncollectable_others;

  common_sets[3] = (GC_Set *)malloc_managed(sizeof(GC_Set));
  common_sets[3]->atomic = !NO_ATOMIC;
  common_sets[3]->uncollectable = 1;
  common_sets[3]->blocks = uncollectable_atomic_common;
  common_sets[3]->block_ends = uncollectable_atomic_common + NUM_COMMON_SIZE;
  common_sets[3]->othersptr = &uncollectable_atomic_others;

#if PROVIDE_MALLOC_AND_FREE
  common_sets[4] = (GC_Set *)malloc_managed(sizeof(GC_Set));
  common_sets[4]->atomic = 1;
  common_sets[4]->uncollectable = 1;
  common_sets[4]->blocks = sys_malloc;
  common_sets[4]->block_ends = sys_malloc + NUM_COMMON_SIZE;
  common_sets[4]->othersptr = &sys_malloc_others;
#endif

  for (i = 0; i < num_common_sets; i++) {
    common_sets[i]->name = "Basic";
#if ALLOW_SET_LOCKING
    common_sets[i]->locked = 0;
#endif
#if KEEP_SET_NO || KEEP_CHUNK_SET_NO
    common_sets[i]->no = i;
#endif
#if ALLOW_TRACE_COUNT
    common_sets[i]->count_tracer = NULL;
#endif
#if ALLOW_TRACE_PATH
    common_sets[i]->path_tracer = NULL;
#endif
#if ALLOW_TRACE_COUNT || ALLOW_TRACE_PATH
    common_sets[i]->trace_init = NULL;
    common_sets[i]->trace_done = NULL;
#endif
#if ALLOW_SET_FINALIZER
    common_sets[i]->finalizer = NULL;
#endif    
  }

#if PROVIDE_MALLOC_AND_FREE
  common_sets[4]->name = "Sysmalloc";
#endif

  initialized = 1;
}

void GC_set_stack_base(void *base)
{
  GC_stackbottom = base;
}

void *GC_get_stack_base(void)
{
  return GC_stackbottom;
}

static void *find_ptr(void *d, int *_size,
		      BlockOfMemory **_block, int *_pos,
		      MemoryChunk **_chunk,
		      int find_anyway)
{
  uintptr_t p = PTR_TO_INT(d);
  DECL_SECTOR_PAGETABLES;

  FIND_SECTOR_PAGETABLES(p);

  if (!sector_pagetables)
    return NULL;

  if (p >= low_plausible && p < high_plausible) {
    SectorPage *pagetable = sector_pagetables[SECTOR_LOOKUP_PAGETABLE(p)];
    if (pagetable) {
      SectorPage *page = pagetable + SECTOR_LOOKUP_PAGEPOS(p);
      intptr_t kind = page->kind;

      if (kind == sector_kind_block) {
	/* Found common block: */
	BlockOfMemory *block = (BlockOfMemory *)INT_TO_PTR(page->start);
	if (p >= block->start && p < block->top) {
	  int size = block->size;
	  int diff = p - block->start;
	  int pos = (diff / size), apos;
	  int bit;
	  uintptr_t result;
	  
	  apos = POS_TO_UNMARK_INDEX(pos);
	  bit = POS_TO_UNMARK_BIT(pos);
	  
	  if (_size)
	    *_size = size;
	  
	  if (NOT_MARKED(block->free[apos] & bit) && !find_anyway)
	    return NULL;
	  
	  result = block->start + (pos * size);
	  
	  if (_block)
	    *_block = block;
	  if (_pos)
	    *_pos = pos;
	  
	  return INT_TO_PTR(result);
	}
      } else if (kind == sector_kind_chunk) {
	MemoryChunk *c = (MemoryChunk *)INT_TO_PTR(page->start);
	if ((p >= c->start) && (p < c->end)) {
	  if (_size)
	    *_size = (c->end - c->start);
	  if (c->marked || find_anyway) {
	    if (_chunk)
	      *_chunk = c;
	    return INT_TO_PTR(c->start);
	  } else
	    return NULL;
	}
      }
    }
  }

  return NULL;
}

void *GC_base(void *d)
{
  void *p;

  p = find_ptr(d, NULL, NULL, NULL, NULL, 0);

#if PAD_BOUNDARY_BYTES
  if (p)
    p = PAD_FORWARD(p);
#endif
  
  return p;
}

int GC_size(void *d);
int GC_is_atomic(void *d);
int GC_orig_size(void *d);
void *GC_orig_base(void *d);

int GC_size(void *d)
{
  int size;
  
  if (find_ptr(d, &size, NULL, NULL, NULL, 0)) {
#if PAD_BOUNDARY_BYTES
    size -= PAD_START_SIZE + PAD_END_SIZE;
#endif
    return size;
  } else
    return 0;
}

int GC_is_atomic(void *d)
{
  BlockOfMemory *block = NULL;
  MemoryChunk *chunk = NULL;
  
  if (find_ptr(d, NULL, &block, NULL, &chunk, 0)) {
    if (block)
      return block->atomic;
    else
      return chunk->atomic;
  } else
    return 0;
}

int GC_orig_size(void *d)
{
  int size = 0;
  
  find_ptr(d, &size, NULL, NULL, NULL, 0);
  return size;
}

void *GC_orig_base(void *d)
{
  return find_ptr(d, NULL, NULL, NULL, NULL, 1);
}

GC_Set *GC_set(void *d)
{
#if KEEP_SET_NO
  BlockOfMemory *block = NULL;
  MemoryChunk *chunk = NULL;
  
  if (!initialized)
    GC_initialize();

  if (find_ptr(d, NULL, &block, NULL, &chunk, 0)) {
    int set_no;
    if (block)
      set_no = block->set_no;
    else
      set_no = chunk->set_no;

    return common_sets[set_no];
  } else
    return NULL;
#else
  return NULL;
#endif
}

#if DUMP_BLOCK_MAPS
static uintptr_t trace_stack_start, trace_stack_end, trace_reg_start, trace_reg_end;
#endif

#if DUMP_SECTOR_MAP
static void dump_sector_map(char *prefix)
{
  FPRINTF(STDERR, "%sBegin Sectors\n"
	  "%sO0:free; ,.:block; =-:chunk; mn:other; \"':other; %d each\n%s",
	  prefix, prefix, SECTOR_SEGMENT_SIZE, prefix);
  {
    int i, j;
    int c = 0;
    uintptr_t was_sec = 0;
    int was_kind = 0;

    for (i = 0; i < (1 << SECTOR_LOOKUP_PAGESETBITS); i++) {
      SectorPage *pagetable;
      pagetable = sector_pagetables[i];
      if (pagetable) {
	for (j = 0; j < SECTOR_LOOKUP_PAGESIZE; j++) {
	  intptr_t kind;
	  kind = pagetable[j].kind;
	  if (kind != sector_kind_free) {
	    char *same_sec, *diff_sec;

	    if (c++ > 40) {
	      FPRINTF(STDERR, "\n%s", prefix);
	      c = 1;
	    }

	    switch(kind) {
#if !RELEASE_UNUSED_SECTORS
	    case sector_kind_freed:
	      same_sec = "0";
	      diff_sec = "O";
	      break;
#endif
	    case sector_kind_block:
	      same_sec = ".";
	      diff_sec = ",";
	      break;
	    case sector_kind_chunk:
	      same_sec = "-";
	      diff_sec = "=";
	      break;
	    case sector_kind_managed:
	      same_sec = "n";
	      diff_sec = "m";
	      break;
	    case sector_kind_other:
	      same_sec = "'";
	      diff_sec = "\"";
	      break;
	    default:
	      same_sec = "?";
	      diff_sec = "?";
	      break;
	    }

	    if ((was_kind != kind) || (was_sec != pagetable[j].start))
	      same_sec = diff_sec;

	    FPRINTF(STDERR, "%s", same_sec);
	    
	    was_kind = kind;
	    was_sec = pagetable[j].start;
	  }
	}
      }
    }
  }
  FPRINTF(STDERR, "\n%sEnd Sectors\n", prefix);
}
#endif

void GC_dump(void)
{
  FPRINTF(STDERR, "Begin Map\n");

  FPRINTF(STDERR,
	  "allocated: %ld  collectable: %ld  uncollectable: %ld\n"
	  "including known overhead: %ld  scheduled gc: %ld  last collect depth: %ld\n"
	  "managed: %ld  managed including overhead: %ld\n"
	  "sector used: %ld  sector free: %ld  sector total: %ld\n"
	  "sector range: %ld  sector administration: %ld\n"
	  "num sector allocs: %ld  num sector frees: %ld\n"
	  "num disappearing links: %d  num finalizations: %d  queued: %d\n"
#if STAMP_AND_REMEMBER_SOURCE
	  "current clock: %ld\n"
#endif
	  , mem_use + mem_uncollectable_use, mem_use, mem_uncollectable_use, 
	  mem_real_use, mem_limit, collect_mem_use,
	  manage_mem_use, manage_real_mem_use,
	  sector_mem_use - sector_free_mem_use, sector_free_mem_use, sector_mem_use,
	  sector_high_plausible - sector_low_plausible,
	  sector_admin_mem_use,
	  num_sector_allocs, num_sector_frees,
	  GC_dl_entries, GC_fo_entries, num_queued_finalizers
#if STAMP_AND_REMEMBER_SOURCE
	  , stamp_clock
#endif
	  );

#if DUMP_SECTOR_MAP
  dump_sector_map("");
#endif

#if DUMP_BLOCK_COUNTS
  {
    int i, j;
    uintptr_t total;
    
#if DUMP_BLOCK_MAPS
    FPRINTF(STDERR, "roots: ======================================\n");
    for (i = 0; i < roots_count; i += 2)
      FPRINTF(STDERR, ">%lx-%lx", roots[i], roots[i + 1]);
    FPRINTF(STDERR, "\n");

    FPRINTF(STDERR, "stack: ======================================\n");
    FPRINTF(STDERR, ">%lx-%lx>%lx-%lx\n",
	    trace_stack_start, trace_stack_end, trace_reg_start, trace_reg_end);
#endif

    for (j = 0; j < num_common_sets; j++) {
      GC_Set *cs = common_sets[j];

      total = 0;

      FPRINTF(STDERR,
	      "Set: %s [%s/%s]: ======================================\n", 
	      cs->name,
	      cs->atomic ? "atomic" : "pointerful",
	      cs->uncollectable ? "eternal" : "collectable");

      for (i = 0; i < NUM_COMMON_SIZE; i++) {
	BlockOfMemory *block;
	int counter = 0;

	block = (cs)->blocks[i];

	if (block) {
	  FPRINTF(STDERR, "%d:", block->size);

#if DUMP_BLOCK_MAPS
	  FPRINTF(STDERR, "[%lx]", block->start - (uintptr_t)block);
#endif

	  while (block) {
	    int k, size = block->size;

#if DUMP_BLOCK_MAPS
	    counter = 0;
#endif

	    for (k = (block->top - block->start) / block->size; k-- ; ) {
	      int bit = POS_TO_UNMARK_BIT(k);
	      int pos = POS_TO_UNMARK_INDEX(k);
	      
	      if (IS_MARKED(block->free[pos] & bit)) {
		total += size;
		counter++;
	      }
	    }

#if DUMP_BLOCK_MAPS
	    FPRINTF(STDERR,
		    ">%lxx%d"
#if STAMP_AND_REMEMBER_SOURCE
		    "@%ld-%ld:%lx-%lx" 
#endif
		    , (uintptr_t)block, counter
#if STAMP_AND_REMEMBER_SOURCE
		    , block->make_time, 
		    block->use_time,
		    block->low_marker,
		    block->high_marker
#endif
		    );
#endif
	    block = block->next;
	  }
#if DUMP_BLOCK_MAPS
	  FPRINTF(STDERR, "\n");
#else
	  FPRINTF(STDERR, "%d;", counter);
#endif
	}
      }

      /* Print chunks, "sorting" so that same size are printed together: */
      {
	MemoryChunk *c, *cnext, *first = NULL, *last = NULL, *t, *next, *prev;
	int counter = 0;
	
	for (c = *(cs->othersptr); c; c = cnext) {
	  uintptr_t size = c->end - c->start;
	  FPRINTF(STDERR, "%ld:", size);

#if DUMP_BLOCK_MAPS
	  FPRINTF(STDERR, "[%lx]", c->start - (uintptr_t)c);
#endif
	  
	  cnext = c->next;
	  
	  prev = NULL;
	  for (t = c; t; t = next) {
	    next = t->next;
	    
	    if (size == (t->end - t->start)) {
#if DUMP_BLOCK_MAPS
	      FPRINTF(STDERR,
		      ">%lx"
#if STAMP_AND_REMEMBER_SOURCE
		      "@%ld:%lx" 
#endif
		      , (uintptr_t)t
#if STAMP_AND_REMEMBER_SOURCE
		      , t->make_time,
		      t->marker
#endif
		      );
#endif
	      
	      counter++;

	      if (last)
		last->next = t;
	      else
		first = t;
	      last = t;
	      if (prev)
		prev->next = t->next;
	      if (t == cnext)
		cnext = t->next;

	      total += size;
	    } else
	      prev = t;
	  }
#if DUMP_BLOCK_MAPS
	  FPRINTF(STDERR, "\n");
#else
	  FPRINTF(STDERR, "%d;", counter);
	  counter = 0;
#endif
	}
	
	if (last)
	  last->next = NULL;
	*(cs->othersptr) = first;
      }
      cs->total = total;

#if KEEP_PREV_PTR
      /* reset prev pointers: */
      {
	MemoryChunk *c, **prev_ptr = (cs->othersptr);
	for (c = *(cs->othersptr); c; c = c->next) {
	  c->prev_ptr = prev_ptr;
	  prev_ptr = &c->next;
	}
      }
#endif
      
      FPRINTF(STDERR, "total size: %ld\n", total);
    }

    FPRINTF(STDERR, "summary: ======================================\n");
    total = 0;
    for (j = 0; j < num_common_sets; j++) {
      GC_Set *cs = common_sets[j];
      FPRINTF(STDERR,
	      "%12s: %10ld  [%s/%s]\n",
	      cs->name, cs->total,
	      cs->atomic ? "atomic" : "pointerful",
	      cs->uncollectable ? "eternal" : "collectable");
      total += cs->total;
    }
    FPRINTF(STDERR, "%12s: %10ld\n", "total", total);
  }
#endif
  FPRINTF(STDERR, "End Map\n");
}

long GC_get_memory_use()
{
  /* returns a `long' instead of `intptr_t' for compatibility
     with the Boehm GC */
  return (long)mem_real_use;
}

void GC_end_stubborn_change(void *p)
{
  /* stubbornness is not exploited */
}

static void *zero_ptr;

#if CHECK_WATCH_FOR_PTR_ALLOC
void *GC_watch_for_ptr = NULL;
#define UNHIDE_WATCH(p) ((void *)~((uintptr_t)p))
static int findings;

#if USE_WATCH_FOUND_FUNC
void GC_found_watch()
{
  FPRINTF(STDERR, "found\n");
  findings++;
}
#endif
#endif

#if PAD_BOUNDARY_BYTES
static void set_pad(void *p, intptr_t s, intptr_t os)
{
  intptr_t diff;

  /* Set start & end pad: */
  *(intptr_t*)p = PAD_PATTERN;
  *(intptr_t*)(((char *)p) + s - PAD_END_SIZE) = PAD_PATTERN;
  *(intptr_t*)(((char *)p) + s - PAD_END_SIZE + sizeof(intptr_t)) = PAD_PATTERN;
  
  /* Keep difference of given - requested: */
  diff = (s - os - PAD_START_SIZE - PAD_END_SIZE);
  ((intptr_t *)p)[1] = diff;
  
  if (diff) {
    unsigned char *ps = ((unsigned char *)p) + os + PAD_START_SIZE;
    while (diff--)
      *(ps++) = PAD_FILL_PATTERN;
  }
}
#endif

static void init_positions(int cpos, int size, int num_elems)
{
  int num_positions = num_elems << LOG_FREE_BIT_PER_ELEM;
  int block_size = size * num_positions;
  int num_offsets = block_size >> LOG_PTR_SIZE;
  int size_in_ptrs = size >> LOG_PTR_SIZE;
  int i, j, pos;
  int *positions;

  positions = (int *)malloc_sector(num_offsets * sizeof(int), sector_kind_other, 0);

  for (i = 0, pos = 0, j = 0; i < num_offsets; ) {
    positions[i++] = pos;
    if (++j == size_in_ptrs) {
      pos++;
      j = 0;
    }
  }

  common_positionses[cpos] = positions;
}

#if ALLOC_STATS
# define ALLOC_STATISTIC(x) x
static int num_allocs_stat;
static int num_nonzero_allocs_stat;
static int num_common_allocs_stat;
static int num_block_alloc_checks_stat;
static int num_block_alloc_nexts_stat;
static int num_block_alloc_second_checks_stat;
static int num_chunk_allocs_stat;
static int num_newblock_allocs_stat;
#else
# define ALLOC_STATISTIC(x) /* empty */
#endif

#if KEEP_SET_NO || KEEP_CHUNK_SET_NO
#define SET_NO_BACKINFO int set_no,
#define KEEP_SET_INFO_ARG(x) x, 
#else
#define SET_NO_BACKINFO /* empty */
#define KEEP_SET_INFO_ARG(x) /* empty */
#endif

static void *do_malloc(SET_NO_BACKINFO
		       uintptr_t size, 
		       BlockOfMemory **common,
		       MemoryChunk **othersptr,
		       int flags)
{
  BlockOfMemory **find, *block;
  BlockOfMemory **common_ends;
  void *s;
  intptr_t c;
  uintptr_t p;
  intptr_t sizeElemBit;
  int i, cpos, elem_per_block, extra_alignment;
#if PAD_BOUNDARY_BYTES
  intptr_t origsize;
#endif

#if CHECK_COLLECTING
  if (collecting_now) {
    exit(-1);
  }
#endif

  ALLOC_STATISTIC(num_allocs_stat++);

  if (!size)
    return (void *)&zero_ptr;

  ALLOC_STATISTIC(num_nonzero_allocs_stat++);

#if PAD_BOUNDARY_BYTES
  origsize = size;
  size += PAD_START_SIZE + PAD_END_SIZE;
#endif

  if (size < (MAX_COMMON_SIZE - PTR_SIZE + 1)) {
    ALLOC_STATISTIC(num_common_allocs_stat++);

    if (!size_map)
      init_size_map();

    cpos = size_index_map[((size + PTR_SIZE - 1) >> LOG_PTR_SIZE) - 1];
#if 0
    if (size > size_map[cpos]) {
      FPRINTF(STDERR, "map error: %d < %d\n", size_map[cpos], size);
    }
#endif
    size = size_map[cpos];

    block = common[cpos + NUM_COMMON_SIZE];
    find = NULL;

    while (block) {
      int search_bit, search_offset;

      if (block->top < block->end)
	goto block_top;

      ALLOC_STATISTIC(num_block_alloc_checks_stat++);

      search_bit = block->free_search_bit;
      search_offset = block->free_search_offset;

      for (i = block->free_search_start; i >= 0; i--)
	if (block->free[i]) {
	  char *zp;
	  int v = block->free[i];
	  
	  while (IS_MARKED(v & search_bit)) {
	    search_bit = search_bit << FREE_BIT_SIZE;
	    search_offset++;
	  }
	  block->free[i] -= search_bit;
	  block->free_search_start = i;
	  block->free_search_bit = search_bit << FREE_BIT_SIZE;
	  block->free_search_offset = search_offset + 1;

	  c = (i << LOG_FREE_BIT_PER_ELEM) + search_offset;
	  
	  if (flags & do_malloc_UNCOLLECTABLE)
	    mem_uncollectable_use += size;
	  else
	    mem_use += size;
	  
	  p = block->start + c * size;

	  zp = INT_TO_PTR(p);

	  if (!(flags & do_malloc_ATOMIC)) {
	    void **p = (void **)zp;
	    uintptr_t sz = size >> LOG_PTR_SIZE;
	    for (; sz--; p++)
	      *p = 0;
	  }

#if CHECK_WATCH_FOR_PTR_ALLOC
	  if (zp == UNHIDE_WATCH(GC_watch_for_ptr)) {
#if USE_WATCH_FOUND_FUNC
	    GC_found_watch();
#else
	    findings++;
#endif
	  }
#endif

#if PAD_BOUNDARY_BYTES
	  SET_PAD(zp, size, origsize);
	  zp = PAD_FORWARD(zp);
#endif

	  return zp;
	} else {
	  search_bit = (FREE_BIT_START | UNMARK_BIT_START);
	  search_offset = 0;
	}

      find = &block->next;

      block = block->next;
      common[cpos + NUM_COMMON_SIZE] = block;

      ALLOC_STATISTIC(num_block_alloc_nexts_stat++);
      ALLOC_STATISTIC(if (block) num_block_alloc_second_checks_stat++);
    }

  } else {
    void *a;
    MemoryChunk *c;

    /* Round up to aligned size: */
    if (size & (ALLOC_ALIGNMENT-1))
      size += ALLOC_ALIGNMENT - (size & (ALLOC_ALIGNMENT-1));

    ALLOC_STATISTIC(num_chunk_allocs_stat++);

    cpos = 0;

    a = malloc_sector(size + size_align(sizeof(MemoryChunk)), sector_kind_chunk, 1);
    if (!a) {
      if (mem_use >= mem_limit)
	GC_gcollect();
      
      a = malloc_sector(size + size_align(sizeof(MemoryChunk)), sector_kind_chunk, 0);
    }

    c = (MemoryChunk *)a;
    
    c->finalizers = NULL;
    c->marked = 1;

#if STAMP_AND_REMEMBER_SOURCE
    c->make_time = stamp_clock;
#endif
#if KEEP_CHUNK_SET_NO
    c->set_no = set_no;
#endif

    c->next = *othersptr;
#if CHECK_FREES
    if (PTR_TO_INT(c->next) & (SECTOR_SEGMENT_SIZE - 1))
      free_error("bad next\n");
#endif
    *othersptr = c;
#if KEEP_PREV_PTR
    c->prev_ptr = othersptr;
    if (c->next)
      c->next->prev_ptr = &c->next;
#endif
    
    c->start = size_align(PTR_TO_INT(&c->data));
    c->end = c->start + size;
    c->atomic = flags & do_malloc_ATOMIC;

    if (flags & do_malloc_UNCOLLECTABLE)
      mem_uncollectable_use += size;
    else
      mem_use += size;
    mem_real_use += (size + sizeof(MemoryChunk));
    num_chunks++;

    if (!low_plausible || (c->start < low_plausible))
      low_plausible = c->start;
    if (!high_plausible || (c->end > high_plausible))
      high_plausible = c->end;	

    if (!(flags & do_malloc_ATOMIC)) {
      void **p = (void **)&c->data;
      uintptr_t sz = size >> LOG_PTR_SIZE;
      for (; sz--; p++)
	*p = 0;
    }

#if CHECK_WATCH_FOR_PTR_ALLOC
    if ((&c->data) == UNHIDE_WATCH(GC_watch_for_ptr)) {
#if USE_WATCH_FOUND_FUNC
      GC_found_watch();
#else
      findings++;
#endif
    }
#endif

    s = (void *)&c->data;

#if PAD_BOUNDARY_BYTES
    SET_PAD(s, size, origsize);
    s = PAD_FORWARD(s);
#endif

    return s;
  }

  ALLOC_STATISTIC(num_newblock_allocs_stat++);

  sizeElemBit = size << LOG_FREE_BIT_PER_ELEM;
  
#if PAD_BOUNDARY_BYTES
  /* Assume alignment */
  extra_alignment = (DOUBLE_SIZE - PTR_SIZE);
#else
  extra_alignment = (size & (DOUBLE_SIZE - 1)) ? 0 : (DOUBLE_SIZE - PTR_SIZE);
#endif

  /* upper bound: */
  elem_per_block = (SECTOR_SEGMENT_SIZE - size_align(sizeof(BlockOfMemory))) / sizeElemBit;
  /*                ^- mem area size      ^- block record */
  /* use this one: */
  elem_per_block = ((SECTOR_SEGMENT_SIZE - size_align(sizeof(BlockOfMemory)) - elem_per_block
  /*                ^- mem area size      ^- block record       ^- elems     */
		     - (extra_alignment + PTR_SIZE - 2)) / sizeElemBit);
  /*                     ^- possible elem padding, -2 since BlockOfMemory has free[1] */
  if (elem_per_block) {
    /* Small enough to fit into one segment */
    c = SECTOR_SEGMENT_SIZE;
  } else {
    elem_per_block = 1;
    /* Add (PTR_SIZE - 1) to ensure enough room after alignment: */
    c = size_align(sizeof(BlockOfMemory)) + (PTR_SIZE - 1) + sizeElemBit;
  }

  block = (BlockOfMemory *)malloc_sector(c, sector_kind_block, 1);
  if (!block) {
    if ((mem_use >= mem_limit) && !GC_dont_gc) {
      GC_gcollect();
      return do_malloc(KEEP_SET_INFO_ARG(set_no)
		       size, common, othersptr, flags);
    } else
      block = (BlockOfMemory *)malloc_sector(c, sector_kind_block, 0);
  }

  
  block->elem_per_block = elem_per_block;

  block->finalizers = NULL;

#if STAMP_AND_REMEMBER_SOURCE
  block->make_time = stamp_clock;
#endif
#if KEEP_SET_NO
  block->set_no = set_no;
#endif

  /* offset for data (ptr aligned): */
  c = size_align(sizeof(BlockOfMemory) + (elem_per_block - 1));
  if (c & (PTR_SIZE - 1))
    c += (PTR_SIZE - (c & (PTR_SIZE - 1)));
#if !PAD_BOUNDARY_BYTES
  if (!(size & (DOUBLE_SIZE - 1))) /* Even more alignment for doubles: */
#else
    /* Assume alignment */
#endif
    if (c & (DOUBLE_SIZE - 1))
      c += (DOUBLE_SIZE - (c & (DOUBLE_SIZE - 1)));
  p = PTR_TO_INT(block) + c;

  common_ends = common + NUM_COMMON_SIZE;

  if (common_ends[cpos] || (find && !common[cpos])) {
    /* hey! - GC happened and reset stuff. find may not be alive anymore,
       so find it again. */
    find = &common_ends[cpos];
    while (*find)
      find = &(*find)->next;
  }

  if (find)
    *find = block;
  else if (!common[cpos])
    common[cpos] = block;

  if (!common_ends[cpos])
    common_ends[cpos] = block;

  num_blocks++;

  for (i = ELEM_PER_BLOCK(block); i-- ; )
    block->free[i] = 0;
  block->free_search_start = -1; /* a free search will not yield results until a GC */

  block->start = block->top = p;
  block->end = block->start + (elem_per_block * sizeElemBit);
  block->size = (short)size;
  block->next = NULL;
  block->atomic = flags & do_malloc_ATOMIC;
  if (!common_positionses[cpos])
    init_positions(cpos, size, elem_per_block);
  block->positions = common_positionses[cpos];

  if (!low_plausible || (block->start < low_plausible))
    low_plausible = block->start;
  if (!high_plausible || (block->end > high_plausible))
    high_plausible = block->end;	

  mem_real_use += SECTOR_SEGMENT_SIZE;

 block_top:

#if STAMP_AND_REMEMBER_SOURCE
  block->use_time = stamp_clock;
#endif

#if CHECK
  if (block->end < block->start
      || block->top < block->start
      || block->top > block->end)
    FPRINTF(STDERR,
	    "bad block: %ld %ld %ld %ld\n",
	    size, block->start, block->top, block->end);
#endif      

  s = INT_TO_PTR(block->top);
  block->top = block->top + size;

  if (flags & do_malloc_UNCOLLECTABLE)
    mem_uncollectable_use += size;
  else
    mem_use += size;

  if (!(flags & do_malloc_ATOMIC)) {
    void **p = (void **)s;
    uintptr_t sz = size >> LOG_PTR_SIZE;
    for (; sz--; p++)
      *p = 0;
  }

#if CHECK_WATCH_FOR_PTR_ALLOC
  if (s == UNHIDE_WATCH(GC_watch_for_ptr)) {
#if USE_WATCH_FOUND_FUNC
    GC_found_watch();
#else
    findings++;
#endif
  }
#endif

#if PAD_BOUNDARY_BYTES
    SET_PAD(s, size, origsize);
    s = PAD_FORWARD(s);
#endif

  return s;
}

GC_Set *GC_new_set(char *name, 
		   GC_trace_init trace_init,
		   GC_trace_done trace_done,
		   GC_count_tracer count_tracer,
		   GC_path_tracer path_tracer,
		   GC_set_elem_finalizer final,
		   int flags)
{
  GC_Set *c, **naya;
  int i;

  if (!initialized)
    GC_initialize();

  c = (GC_Set *)malloc_managed(sizeof(GC_SetWithOthers));

  naya = (GC_Set **)malloc_managed(sizeof(GC_Set *) * (num_common_sets + 1));
  for (i = 0; i < num_common_sets; i++)
    naya[i] = common_sets[i];
  
#if KEEP_SET_NO || KEEP_CHUNK_SET_NO
  c->no = num_common_sets;
#endif
#if ALLOW_TRACE_COUNT
  c->count_tracer = count_tracer;
#endif
#if ALLOW_TRACE_PATH
  c->path_tracer = path_tracer;
#endif
#if ALLOW_TRACE_COUNT || ALLOW_TRACE_PATH
  c->trace_init = trace_init;
  c->trace_done = trace_done;
#endif
#if ALLOW_SET_FINALIZER
  c->finalizer = final;
#endif    

  naya[num_common_sets++] = c;
  c->atomic = !!(flags & SGC_ATOMIC_SET);
  c->uncollectable = !!(flags & SGC_UNCOLLECTABLE_SET);
#if ALLOW_SET_LOCKING
  c->locked = 0;
#endif
  c->name = name;
  c->blocks = (BlockOfMemory **)malloc_managed(sizeof(BlockOfMemory*) * 2 * NUM_COMMON_SIZE);
  memset(c->blocks, 0, sizeof(BlockOfMemory*) * NUM_COMMON_SIZE);
  c->block_ends = c->blocks + NUM_COMMON_SIZE;
  memset(c->block_ends, 0, sizeof(BlockOfMemory*) * NUM_COMMON_SIZE);

  ((GC_SetWithOthers *)c)->others = NULL;
  c->othersptr = &((GC_SetWithOthers *)c)->others;

  free_managed(common_sets);
  common_sets = naya;

  return c;
}

void *GC_malloc(size_t size)
{
  return do_malloc(KEEP_SET_INFO_ARG(0)
		   size, common, &others, 
		   0);
}

void *GC_malloc_atomic(size_t size)
{
  return do_malloc(KEEP_SET_INFO_ARG(1)
		   size, atomic_common, 
		   &atomic_others, 
		   do_malloc_ATOMIC_UNLESS_DISABLED);
}

void *GC_malloc_uncollectable(size_t size)
{
  return do_malloc(KEEP_SET_INFO_ARG(2)
		   size, uncollectable_common, 
		   &uncollectable_others, 
		   do_malloc_UNCOLLECTABLE);
}

void *GC_malloc_atomic_uncollectable(size_t size)
{
  return do_malloc(KEEP_SET_INFO_ARG(3)
		   size, uncollectable_atomic_common, 
		   &uncollectable_atomic_others, 
		   do_malloc_ATOMIC_UNLESS_DISABLED | do_malloc_UNCOLLECTABLE);
}

void *GC_malloc_specific(size_t size, GC_Set *set)
{
  return do_malloc(KEEP_SET_INFO_ARG(set->no)
		   size, set->blocks, set->othersptr,
		   ((set->atomic ? do_malloc_ATOMIC_UNLESS_DISABLED : 0)
		    | (set->uncollectable ? do_malloc_UNCOLLECTABLE : 0)));
}

void *GC_malloc_stubborn(size_t size)
{
  return GC_malloc(size);
}

#if PROVIDE_MALLOC_AND_FREE
void *malloc(size_t size)
{
  return do_malloc(KEEP_SET_INFO_ARG(4)
		   size, sys_malloc, 
		   &sys_malloc_others, 
		   do_malloc_ATOMIC | do_malloc_UNCOLLECTABLE);
}

void *realloc(void *p, size_t size)
{
  void *naya;
  size_t oldsize;

  if (p) {
    oldsize = (size_t)GC_size(p);
    if (!oldsize)
      FPRINTF(STDERR, "illegal realloc\n");
  } else
    oldsize = 0;
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
  intptr_t c;

  c = n * size;
  p = malloc(c);
  memset(p, 0, c);

  return p;
}

void free(void *p)
{
  if (p)
    GC_free(p);
}

# ifdef WIN32
size_t _msize(void *p)
{
  return GC_size(p);
}
# endif
#endif

static void register_disappearing_link(void **p, void *a, int late)
{
  DisappearingLink *dl;
    
  dl = (DisappearingLink *)malloc_managed(sizeof(DisappearingLink));
  dl->kind = late ? dl_late : (a ? dl_normal : dl_restored);
  dl->watch = a;
  dl->disappear = p;
  dl->saved_value = NULL;
  dl->prev = NULL;
  dl->next = late ? late_disappearing : disappearing;
  if (dl->next)
    dl->next->prev = dl;
  if (late)
    late_disappearing = dl;
  else
    disappearing = dl;

  GC_dl_entries++;

  mem_real_use += sizeof(DisappearingLink);
}

void GC_register_indirect_disappearing_link(void **p, void *a)
{
  register_disappearing_link(p, a, 0);
}

void GC_register_late_disappearing_link(void **p, void *a)
{
  register_disappearing_link(p, a, 1);
}

void GC_unregister_disappearing_link(void **p)
{
  /* We'll do it later */
}

#if 0
DisappearingLink *GC_find_dl(void *p)
{
  DisappearingLink *dl;

  for (dl = disappearing; dl; dl = dl->next)
    if ((dl->watch == p) || (!dl->watch && (*dl->disappear == p)))
      return dl;

  for (dl = late_disappearing; dl; dl = dl->next)
    if ((dl->watch == p) || (!dl->watch && (*dl->disappear == p)))
      return dl;

  return NULL;
}
#endif

static void register_finalizer(void *p, void (*f)(void *p, void *data), 
			       void *data, void (**oldf)(void *p, void *data), 
			       void **olddata, int eager_level, int ignore_self)
{
  BlockOfMemory *block = NULL;
  MemoryChunk *chunk = NULL;
  int pos;

  if ((p = find_ptr(p, NULL, &block, &pos, &chunk, 0))) {
    Finalizer *fn;

    if (block) {
      fn = block->finalizers;
      while (fn && (fn->u.pos != pos))
	fn = fn->next;
      if (fn && !f) {
	if (fn->prev)
	  fn->prev->next = fn->next;
	else
	  block->finalizers = fn->next;
	if (fn->next)
	  fn->next->prev = fn->prev;
      }
    } else {
      fn = chunk->finalizers;
      if (fn && !f)
	chunk->finalizers = NULL;
    }

    if (oldf)
      *oldf = (fn ? fn->f : NULL);
    if (olddata)
      *olddata = (fn ? fn->data : NULL);
    
    if (f) {
      int isnaya = !fn;

      if (!fn) {
	fn = (Finalizer *)malloc_managed(sizeof(Finalizer));
	mem_real_use += sizeof(Finalizer);
	GC_fo_entries++;
      }

      fn->u.pos = pos;
      fn->f = f;
      fn->data = data;
      fn->eager_level = eager_level;
      fn->ignore_self = ignore_self;
      
      if (isnaya) {
	fn->prev = NULL;
	if (block) {
	  fn->next = block->finalizers;
	  if (fn->next)
	    fn->next->prev = fn;
	  block->finalizers = fn;
	} else {
	  chunk->finalizers = fn;
	  fn->next = NULL;
	}
      }
    } else if (fn) {
      mem_real_use -= sizeof(Finalizer);
      free_managed(fn);
      --GC_fo_entries;
    }
  }
}

void GC_register_finalizer(void *p, void (*f)(void *p, void *data), 
			   void *data, void (**oldf)(void *p, void *data), 
			   void **olddata)
{
  register_finalizer(PAD_BACKWARD(p), f, data, oldf, olddata, 0, 0);
}

void GC_register_eager_finalizer(void *p, int level, void (*f)(void *p, void *data), 
				 void *data, void (**oldf)(void *p, void *data), 
				 void **olddata)
{
  register_finalizer(PAD_BACKWARD(p), f, data, oldf, olddata, level, 0);
}

void GC_register_finalizer_ignore_self(void *p, void (*f)(void *p, void *data), 
				       void *data, void (**oldf)(void *p, void *data), 
				       void **olddata)
{
  register_finalizer(PAD_BACKWARD(p), f, data, oldf, olddata, 0, 1);
}

/******************************************************************/

void GC_for_each_element(GC_Set *set,
			 void (*f)(void *p, int size, void *data),
			 void *data)
{
  int i;
  BlockOfMemory **blocks = set->blocks;
  MemoryChunk *c = *(set->othersptr);

#if ALLOW_SET_LOCKING
  if (!set->uncollectable)
    set->locked++;
#endif

  for (i = 0; i < NUM_COMMON_SIZE; i++) {
    BlockOfMemory **prev = &blocks[i];
    BlockOfMemory *block = *prev;

    while (block) {
      int j;

      j = (block->top - block->start) / block->size;
      
      while (j--) {
	int bit = POS_TO_FREE_BIT(j);
	int pos = POS_TO_FREE_INDEX(j);
	
	if (IS_MARKED(block->free[pos] & bit)) {
	  uintptr_t p;
	  void *s;
	  
	  p = block->start + (block->size * j);
	  s = INT_TO_PTR(p);
	  
#if PAD_BOUNDARY_BYTES
	  s = PAD_FORWARD(s);
#endif
	  
	  f(s, block->size, data);
	}
      }
      block = block->next;
    }
  }

  for (; c; c = c->next) {
    void *s;

    s = INT_TO_PTR(c->start);

#if PAD_BOUNDARY_BYTES
    s = PAD_FORWARD(s);
#endif

    f(s, c->end - c->start, data);
  }

#if ALLOW_SET_LOCKING
  if (!set->uncollectable)
    --set->locked;
#endif
}

/******************************************************************/

static void free_chunk(MemoryChunk *k, MemoryChunk **prev, GC_Set *set)
{
  MemoryChunk *next;
  
#if ALLOW_SET_FINALIZER
  if (set->finalizer) {
    void *s = INT_TO_PTR(k->start);
#if PAD_BOUNDARY_BYTES
    s = PAD_FORWARD(s);
#endif
    set->finalizer(s);
  }
#endif
  
  mem_real_use -= (k->end - k->start + sizeof(MemoryChunk));
  
#if PRINT && 0
  FPRINTF(STDERR, "free chunk: %ld (%ld) %d %d\n", 
	  (uintptr_t)k, k->end - k->start,
	  set->atomic, set->uncollectable);
#endif
  
  next = k->next;

#if KEEP_PREV_PTR
  if (next)
    next->prev_ptr = k->prev_ptr;
#endif

#if CHECK_FREES
  if (PTR_TO_INT(next) & (SECTOR_SEGMENT_SIZE - 1))
    free_error("bad next\n");
#endif

  *prev = next;

  free_sector(k);
  --num_chunks;
}

void GC_free(void *p) 
{
#if PROVIDE_GC_FREE || PROVIDE_CHUNK_GC_FREE
  BlockOfMemory *block = NULL;
  MemoryChunk *chunk = NULL;
  int fpos;
  void *found;
  GC_Set *set;

# if CHECK_COLLECTING && CHECK_FREES
  if (collecting_now)
    free_error("GC_free during collection\n");
# endif

  found = find_ptr(p, NULL, &block, &fpos, &chunk, 1);
  if (!found) {
# if CHECK_FREES
    char b[256];
    sprintf(b, "GC_free failed! %lx\n", (intptr_t)p);
    free_error(b);
# endif
    return;
  }

  if (PAD_FORWARD(found) == p) {
    if (block) {
# if PROVIDE_GC_FREE
      int i;
      int pos = POS_TO_FREE_INDEX(fpos);
      int fbit = POS_TO_FREE_BIT(fpos);
      int ubit = POS_TO_UNMARK_BIT(fpos);

# if CHECK_FREES
      if (block->free[pos] & fbit) {
	char b[256];
	sprintf(b, "Block element already free! %lx\n", (intptr_t)p);
	return;
      }
#   if EXTRA_FREE_CHECKS
      if (block->set_no != 4) {
	char b[256];
	sprintf(b, "GC_free on ptr from wrong block! %lx\n", (intptr_t)p);
	free_error(b);
	return;
      }
#   endif
#  endif

      block->free[pos] |= (fbit | ubit);
      if (block->free_search_start <= pos) {
	block->free_search_start = pos;
	block->free_search_bit = (FREE_BIT_START | UNMARK_BIT_START);
	block->free_search_offset = 0;
      }

      if (!initialized)
	GC_initialize();

      set = common_sets[block->set_no];
      
#  if ALLOW_SET_FINALIZER
      if (set->finalizer)
	set->finalizer(p);
#  endif

      {
	int size;
#  if PAD_BOUNDARY_BYTES
	size = block->size - PAD_START_SIZE - PAD_END_SIZE;
	((intptr_t *)found)[1] = 0; /* 0 extra */
#  else
	size = block->size;
#  endif

	/* Clear, since collection scans whole block. */
	memset(p, 0, size);
      }

      /* Make sure this block is reachable from block_ends: */
      i = size_index_map[(block->size >> LOG_PTR_SIZE) - 1];
      if (set->block_ends[i] != block)
	set->block_ends[i] = set->blocks[i];

      if (set->uncollectable)
	mem_uncollectable_use -= block->size;
# endif
    } else {
      if (!initialized)
	GC_initialize();

# if CHECK_FREES && EXTRA_FREE_CHECKS
      if (chunk->set_no != 4) {
	char b[256];
	sprintf(b, "GC_free on ptr from wrong block! %lx\n", (intptr_t)p);
	free_error(b);
	return;
      }
# endif
      set = common_sets[chunk->set_no];
      if (set->uncollectable)
	mem_uncollectable_use -= (chunk->end - chunk->start);
      free_chunk(chunk, chunk->prev_ptr, set);
    }
  }
# if CHECK_FREES
  else {
    char b[256];
    sprintf(b, "GC_free on block interior! %lx != %lx\n", 
	    (intptr_t)p, (intptr_t)PAD_FORWARD(found));
    free_error(b);
  }
# endif
#endif
}

/******************************************************************/

#if CHECK
static intptr_t cmn_count, chk_count;
#endif

#if ALLOW_TRACE_COUNT
static int collecting_with_trace_count;

#define TRACE_COLLECT_SWITCH !collecting_with_trace_count
#else
#define TRACE_COLLECT_SWITCH 1
#endif

#if ALLOW_TRACE_PATH
static int collecting_with_trace_path;
static char *current_trace_source;

/* Buffer used to store paths, since allocation is not allowed: */
# define TRACE_PATH_BUFFER_SIZE 1048576
static void *trace_path_buffer[TRACE_PATH_BUFFER_SIZE];
static int trace_path_buffer_pos;
#endif

#if PAD_BOUNDARY_BYTES
static void bad_pad(char *where, void *s, int type, intptr_t sz, intptr_t diff, intptr_t offset, 
		    intptr_t pd, intptr_t expect)
{
  FPRINTF(STDERR,
	  "pad %s violation at %lx <%d>, len %ld (diff %ld+%ld): %lx != %lx\n", 
	  where, (uintptr_t)s, type, sz, diff, offset, pd, expect);
}
#endif

static void collect_init_chunk(MemoryChunk *c, int uncollectable, int ty)
{
  for (; c; c = c->next) {
    if (uncollectable && TRACE_COLLECT_SWITCH)
      c->marked = 1;
    else
      c->marked = 0;

#if PAD_BOUNDARY_BYTES
    /* Test padding: */
    {
      void *s = INT_TO_PTR(c->start);
      intptr_t pd, sz, diff;
      sz = c->end - c->start;
      diff = ((intptr_t *)s)[1];
      pd = *(intptr_t *)s;
      if (pd != PAD_PATTERN)
	bad_pad("start", s, ty, sz, diff, 0, pd, PAD_PATTERN);
      pd = *(intptr_t *)INT_TO_PTR(c->end - PAD_END_SIZE);
      if (pd != PAD_PATTERN)
	bad_pad("end1", s, ty, sz, diff, 0, pd, PAD_PATTERN);
      pd = *(intptr_t *)INT_TO_PTR(c->end - PAD_END_SIZE + sizeof(intptr_t));
      if (pd != PAD_PATTERN)
	bad_pad("end2", s, ty, sz, diff, 0, pd, PAD_PATTERN);
      if (diff) {
	/* Given was bigger than requested; check extra bytes: */
	unsigned char *ps = ((unsigned char *)s) + sz - PAD_END_SIZE - diff;
	intptr_t d = 0;
	while (d < diff) {
	  if (*ps != PAD_FILL_PATTERN) {
	    bad_pad("extra", s, ty, sz, diff, d, *ps, PAD_FILL_PATTERN);
	  }
	  ps++;
	  d++;
	}
      }
    }
#endif

#if CHECK
    chk_count++;
    if ((!low_plausible || (c->start < low_plausible))
	|| (!high_plausible || (c->end > high_plausible)))
      FPRINTF(STDERR, "implausible chunk!\n");
#endif
  }
}

#if FINISH_STATS
# define FINISH_STATISTIC(x) x
static int num_finish_chunk_stat;
static int num_finish_chunkkeep_stat;
static int num_finish_chunkfree_stat;
static int num_finish_block_stat;
static int num_finish_blockkeep_stat;
static int num_finish_blockfree_stat;
static int num_finish_blockadjust_stat;
static int num_finish_blockfiltercycles_stat;
static int num_finishes_stat;
#else
# define FINISH_STATISTIC(x)
#endif

static void collect_finish_chunk(MemoryChunk **c, GC_Set *set)
{
  uintptr_t local_low_plausible;
  uintptr_t local_high_plausible;

  local_low_plausible = low_plausible;
  local_high_plausible = high_plausible;

  while (*c) {
    MemoryChunk *k = *c;

    FINISH_STATISTIC(num_finish_chunk_stat++);

    if (k->marked) {
      c = &k->next;

      FINISH_STATISTIC(num_finish_chunkkeep_stat++);

      if (!local_low_plausible || (k->start < local_low_plausible))
	local_low_plausible = k->start;
      if (!local_high_plausible || (k->end > local_high_plausible))
	local_high_plausible = k->end;	
    } else {
      FINISH_STATISTIC(num_finish_chunkfree_stat++);

      free_chunk(k, c, set);
    }
  }

  low_plausible = local_low_plausible;
  high_plausible = local_high_plausible;
}

static void collect_init_common(BlockOfMemory **blocks, int uncollectable, int ty)
{
  int i, j;
  int boundary, boundary_val = 0;

  for (i = 0; i < NUM_COMMON_SIZE; i++) {
    BlockOfMemory *block = blocks[i];

    while (block) {
#if CHECK
      cmn_count++;
      if ((!low_plausible || (block->start < low_plausible))
	  || (!high_plausible || (block->end > high_plausible)))
	FPRINTF(STDERR, "implausible block!\n");
#endif

#if STAMP_AND_REMEMBER_SOURCE
      block->low_marker = block->high_marker = 0;
#endif

#if PAD_BOUNDARY_BYTES
      /* Test padding: */
      {
	uintptr_t p;
	intptr_t size = size_map[i];
	
	for (p = block->start; p < block->top; p += size) {
	  void *s = INT_TO_PTR(p);
	  intptr_t pd, diff;
	  pd = *(intptr_t *)s;
	  diff = ((intptr_t *)s)[1];
	  if (pd != PAD_PATTERN)
	    bad_pad("start", s, ty, size, diff, 0, pd, PAD_PATTERN);
	  pd = *(intptr_t *)INT_TO_PTR(p + size - PAD_END_SIZE);
	  if (pd != PAD_PATTERN)
	    bad_pad("end1", s, ty, size, diff, 0, pd, PAD_PATTERN);
	  pd = *(intptr_t *)INT_TO_PTR(p + size - PAD_END_SIZE + sizeof(intptr_t));
	  if (pd != PAD_PATTERN)
	    bad_pad("end2", s, ty, size, diff, 0, pd, PAD_PATTERN);
	  if (diff) {
	    /* Given was bigger than requested; check extra bytes: */
	    unsigned char *ps = ((unsigned char *)s) + size - PAD_END_SIZE - diff;
	    intptr_t d = 0;
	    while (d < diff) {
	      if (*ps != PAD_FILL_PATTERN) {
		bad_pad("extra", s, ty, size, diff, d, *ps, PAD_FILL_PATTERN);
	      }
	      ps++;
	      d++;
	    }
	  }
	}
      }
#endif

      if (uncollectable && TRACE_COLLECT_SWITCH) {
	for (j = ELEM_PER_BLOCK(block); j-- ; ) {
#if DISTINGUISH_FREE_FROM_UNMARKED
	  block->free[j] = SHIFT_COPY_FREE_TO_UNMARKED(block->free[j]);
#else
	  block->free[j] = 0;
#endif
	}
      } else {
	if (block->top < block->end) {
	  int pos = block->positions[(block->top - block->start) >> LOG_PTR_SIZE];
	  boundary = POS_TO_UNMARK_INDEX(pos);
	  boundary_val = (POS_TO_UNMARK_BIT(pos) - 1) & ALL_UNMARKED;
	} else {
	  boundary = ELEM_PER_BLOCK(block);
	}

	for (j = ELEM_PER_BLOCK(block); j-- ; ) {
	  if (j < boundary)
	    block->free[j] |= ALL_UNMARKED;
	  else if (j == boundary)
	    block->free[j] = boundary_val;
	  else
	    block->free[j] = 0;
	}
      }

      block = block->next;
    }
  }
}

static void collect_finish_common(BlockOfMemory **blocks, 
				  BlockOfMemory **block_ends, 
				  GC_Set *set)
{
  int i;
#if KEEP_BLOCKS_FOREVER
  int kept;
#endif
  uintptr_t local_low_plausible;
  uintptr_t local_high_plausible;

  local_low_plausible = low_plausible;
  local_high_plausible = high_plausible;

  for (i = 0; i < NUM_COMMON_SIZE; i++) {
    BlockOfMemory **prev = &blocks[i];
    BlockOfMemory *block = *prev;
#if CHECK
    intptr_t size = size_map[i];
#endif

#if KEEP_BLOCKS_FOREVER
    kept = 0;
#endif

    while (block) {
      int unfree;

      FINISH_STATISTIC(num_finish_block_stat++);
      
#if CHECK
      if (block->end < block->start
	  || block->top < block->start
	  || block->top > block->end)
	FPRINTF(STDERR,
		"bad block: %ld %ld %ld %ld\n",
		size, block->start, block->top, block->end);
#endif

#if ALLOW_SET_FINALIZER
      if (set->finalizer) {
	uintptr_t s;
	int j;
	for (j = 0, s = block->start; s < block->top; s += block->size, j++) {
	  int pos = POS_TO_UNMARK_INDEX(j);
	  int bit = POS_TO_UNMARK_BIT(j);

	  if (NOT_MARKED(block->free[pos] & bit)) {
	    void *p = INT_TO_PTR(s);
#if PAD_BOUNDARY_BYTES
	    p = PAD_FORWARD(p);
#endif
	    set->finalizer(p);
	  }
	}
      }
#endif

      unfree = 0;
      {
	int j;
	for (j = ELEM_PER_BLOCK(block); j-- ; ) {
	  FINISH_STATISTIC(num_finish_blockfiltercycles_stat++);
	  if ((block->free[j] & ALL_UNMARKED) != ALL_UNMARKED) {
	    unfree = j + 1;
	    break;
	  }
	}
      }

#if KEEP_BLOCKS_FOREVER
      if (!unfree && (kept < KEEP_BLOCKS_FOREVER)) {
	int j;
	block->top = block->start;
	for (j = ELEM_PER_BLOCK(block); j-- ; )
	  block->free[j] = 0;
	kept++;
	unfree = 1;
      }
#endif

      if (!unfree) {
	FINISH_STATISTIC(num_finish_blockfree_stat++);

	--num_blocks;

	*prev = block->next;
	free_sector(block);
	mem_real_use -= SECTOR_SEGMENT_SIZE;
	block = *prev;
      } else {
#if DISTINGUISH_FREE_FROM_UNMARKED
	/* If it's unmarked, free it: */
	int j;

	for (j = ELEM_PER_BLOCK(block); j-- ; )
	  block->free[j] |= SHIFT_UNMARK_TO_FREE(block->free[j]);
#endif

	/* Push down block->top if it's easy */
	{
	  uintptr_t dt = (unfree << LOG_FREE_BIT_PER_ELEM) * (uintptr_t)block->size;
	  if (block->top > block->start + dt) {
	    int k;
	    FINISH_STATISTIC(num_finish_blockadjust_stat++);
	    block->top = block->start + dt;
	    for (k = ELEM_PER_BLOCK(block); --k >= unfree; ) {
	      block->free[k] = 0;
	    }
	  }
	}
	
	block->free_search_start = unfree - 1;
	block->free_search_bit = (FREE_BIT_START | UNMARK_BIT_START);
	block->free_search_offset = 0;

	FINISH_STATISTIC(num_finish_blockkeep_stat++);

	if (!local_low_plausible || (block->start < local_low_plausible))
	  local_low_plausible = block->start;
	if (!local_high_plausible || (block->end > local_high_plausible))
	  local_high_plausible = block->end;

	prev = &block->next;
	block = block->next;
      }
    }

    block_ends[i] = blocks[i];
  }

  low_plausible = local_low_plausible;
  high_plausible = local_high_plausible;
}

static int collect_stack_count;
static int collect_stack_size;
static uintptr_t *collect_stack;

#define INITIAL_COLLECT_STACK_SIZE 8192

#if KEEP_DETAIL_PATH
# define PUSH_SRC(src) collect_stack[collect_stack_count++] = src;
# define LOCAL_PUSH_SRC(src) local_collect_stack[local_collect_stack_count++] = src;
# define COLLECT_STACK_FRAME_SIZE 3
#else
# define PUSH_SRC(src) /*empty*/
# define LOCAL_PUSH_SRC(src) /*empty*/
# define COLLECT_STACK_FRAME_SIZE 2
#endif

static void push_collect(uintptr_t start, uintptr_t end, uintptr_t src)
{
  if (collect_stack_count >= collect_stack_size) {
    intptr_t oldsize;

    if (collect_stack)
      oldsize = sizeof(uintptr_t) * (collect_stack_size + (COLLECT_STACK_FRAME_SIZE - 1));
    else
      oldsize = 0;

    collect_stack_size = collect_stack_size ? 2 * collect_stack_size : 500;
    collect_stack = (uintptr_t *)realloc_collect_temp(collect_stack, 
							  oldsize, 
							  sizeof(uintptr_t) 
							  * (collect_stack_size + (COLLECT_STACK_FRAME_SIZE - 1)));
    /* fprintf(stderr, "grow push stack: %d\n", collect_stack_size); */
  }

  collect_stack[collect_stack_count++] = start;
  collect_stack[collect_stack_count++] = end;
  PUSH_SRC(src)
}

#define PUSH_COLLECT(s, e, src) \
  if (collect_stack_count < collect_stack_size) { \
    collect_stack[collect_stack_count++] = s; \
    collect_stack[collect_stack_count++] = e + 1 - PTR_ALIGNMENT; \
    PUSH_SRC(src) \
  } else \
    push_collect(s, e + 1 - PTR_ALIGNMENT, src);

#define LOCAL_PUSH_COLLECT(s, e, src) \
  if (local_collect_stack_count < local_collect_stack_size) { \
    local_collect_stack[local_collect_stack_count++] = s; \
    local_collect_stack[local_collect_stack_count++] = e + 1 - PTR_ALIGNMENT; \
    LOCAL_PUSH_SRC(src) \
  } else { \
    collect_stack_count = local_collect_stack_count; \
    push_collect(s, e + 1 - PTR_ALIGNMENT, src); \
    local_collect_stack = collect_stack; \
    local_collect_stack_count = collect_stack_count; \
    local_collect_stack_size = collect_stack_size; \
  }

#if ALLOW_TRACE_COUNT || ALLOW_TRACE_PATH

typedef struct {
  int count, size;
  uintptr_t *stack;
} TraceStack;

static void init_trace_stack(TraceStack *s)
{
  s->size = s->count = 0;
  s->stack = NULL;
}

static void done_trace_stack(TraceStack *s)
{
  if (s->stack)
    free_collect_temp(s->stack, sizeof(uintptr_t)*(s->size + 1));
}

static void push_trace_stack(uintptr_t v, TraceStack *s)
{
  if (s->count >= s->size) {
    intptr_t oldsize;

    if (s->stack)
      oldsize = sizeof(uintptr_t)*(s->size + 1);
    else
      oldsize = 0;

    s->size = s->size ? 2 * s->size : 500;
    s->stack = (uintptr_t *)realloc_collect_temp(s->stack,
						     oldsize,
						     sizeof(uintptr_t)*(s->size + 1));
  }

  s->stack[s->count++] = v;
}

#define PUSH_TS(v, s) \
  if (s.count < s.size) { \
    s.stack[s.count++] = (uintptr_t)(v); \
  } else \
    push_trace_stack((uintptr_t)(v), &s);

#define POP_TS(s) (s.stack[--s.count])

#endif

#if ALLOW_TRACE_COUNT

TraceStack collect_trace_stack, collect_wait_trace_stack;

static int collect_start_tracing;
static int collect_end_tracing;
static int collect_trace_count;

#define PUSH_TRACE(v) PUSH_TS(v, collect_trace_stack)
#define PUSH_WAIT_TRACE(v) PUSH_TS(v, collect_wait_trace_stack)

#define POP_TRACE() POP_TS(collect_trace_stack)
#define POP_WAIT_TRACE() POP_TS(collect_wait_trace_stack)

#endif

#if ALLOW_TRACE_PATH

TraceStack collect_trace_path_stack;

static int collect_end_path_elem;

#define PUSH_PATH_ELEM(v) PUSH_TS(v, collect_trace_path_stack)

#define POP_PATH_ELEM() POP_TS(collect_trace_path_stack)

#define PATH_ELEM_STACK_NONEMPTY() (collect_trace_path_stack.count)

#endif

#if CHECK_SKIP_MARK_AT_FIRST
static int collect_start_disable_mark_skip;
int (*skip_mark_at_first)(void *, size_t);
#endif

#if MARK_STATS
static int num_pairs_stat;
static int num_checks_stat;
static int num_interior_checks_stat;
static int num_plausibles_stat;
static int num_pages_stat;
static int num_blocks_stat;
static int num_blockallocs_stat;
static int num_blockaligns_stat;
static int num_blockmarks_stat;
static int num_blockpushes_stat;
static int num_blockpushes_tail_stat;
static int num_chunks_stat;
static int num_chunkmarks_stat;
#endif

#define COLLECT semi_collect_stack
#define STACK_TRACE
#include "collect.inc"

static void prepare_stack_collect()
{
  uintptr_t s, e;
  uintptr_t source;
#if KEEP_DETAIL_PATH
  source = collect_stack[--collect_stack_count];
#else
  source = 0;
#endif
  e = collect_stack[--collect_stack_count];
  s = collect_stack[--collect_stack_count];
  e += PTR_ALIGNMENT - 1;

  PUSH_COLLECT(s, e, source);
  semi_collect_stack(0);

#if !NO_STACK_OFFBYONE
  PUSH_COLLECT(s, e, source);
  semi_collect_stack(-PTR_ALIGNMENT);
  /* Note: this nested-semi preparation can create trace paths of
     the form X->X->Y->Z->... */
#endif
}

#define COLLECT collect
#if CHECK_SIMPLE_INTERIOR_POINTERS
# define FOLLOW_INTERIOR
#endif
# include "collect.inc"

static jmp_buf buf;

/* Sparc fix borrowed from SCM, so here's the copyright:  */
/* Scheme implementation intended for JACAL.
   Copyright (C) 1990, 1991, 1992, 1993, 1994 Aubrey Jaffer. */
/* James Clark came up with this neat one instruction fix for
   continuations on the SPARC.  It flushes the register windows so
   that all the state of the process is contained in the stack. */
#ifdef sparc
#define FLUSH_REGISTER_WINDOWS asm("ta 3")
#else
#define FLUSH_REGISTER_WINDOWS /* empty */
#endif

static void push_stack(void *stack_now)
{
  uintptr_t start, end;

  start = PTR_TO_INT(GC_stackbottom);
  end = PTR_TO_INT(stack_now);

#if PRINT && STAMP_AND_REMEMBER_SOURCE
  FPRINTF(STDERR, "stack in [%lx, %lx]\n", start, end);
#endif

  if (start < end) {
    PUSH_COLLECT(start, end, 0);
  } else {
    PUSH_COLLECT(end, start, 0);
  }

#if DUMP_BLOCK_MAPS
  trace_stack_start = collect_stack[collect_stack_count - COLLECT_STACK_FRAME_SIZE];
  trace_stack_end = collect_stack[collect_stack_count - (COLLECT_STACK_FRAME_SIZE - 1)];
#endif

  prepare_stack_collect();

  start = PTR_TO_INT((void *)&buf);
  end = start + sizeof(buf);
  PUSH_COLLECT(start, end, 0);

#if DUMP_BLOCK_MAPS
  trace_reg_start = collect_stack[collect_stack_count - COLLECT_STACK_FRAME_SIZE];
  trace_reg_end = collect_stack[collect_stack_count - (COLLECT_STACK_FRAME_SIZE - 1)];
#endif

  prepare_stack_collect();

#if PRINT && STAMP_AND_REMEMBER_SOURCE
  FPRINTF(STDERR, "jmpbuf in [%lx, %lx]\n", start, end);
#endif
}

#if ALLOW_SET_LOCKING
static void push_locked_chunk(MemoryChunk *c, int atomic)
{
  for (; c; c = c->next) {
    uintptr_t size = (c->end - c->start);
    mem_use += size;
    collect_trace_count += size;
    if (!atomic) {
      PUSH_COLLECT(c->start, c->end, 0);
    }
  }
}

static void push_locked_common(BlockOfMemory **blocks, int atomic)
{
  int i;

  for (i = 0; i < NUM_COMMON_SIZE; i++) {
    BlockOfMemory *block = blocks[i];
    
    for (; block; block = block->next) {
      uintptr_t size = block->size;
      uintptr_t start = block->start;
      uintptr_t top = block->top;
      int j;
      
      for (j = 0; start < top; start += size, j++) {
	int bit = POS_TO_UNMARK_BIT(j);
	int pos = POS_TO_UNMARK_INDEX(j);
	if (IS_MARKED(block->free[pos] & bit)) {
	  if (!atomic) {
	    PUSH_COLLECT(start, start + size, 0);
	  }
	  mem_use += size;
	  collect_trace_count += size;
	}
      }
    }
  }
}

#endif

static void push_uncollectable_chunk(MemoryChunk *c, GC_Set *set)
{
#if ALLOW_TRACE_COUNT
  if (!collecting_with_trace_count
      || !c
      || !set->count_tracer) {
#endif
    for (; c; c = c->next) {
#if ALLOW_TRACE_COUNT
      if (!c->marked) {
	if (collecting_with_trace_count) {
	  c->marked = 1;
	  collect_trace_count += (c->end - c->start);
	}
	if (!set->atomic) {
#endif      
	  PUSH_COLLECT(c->start, c->end, 0);
#if ALLOW_TRACE_COUNT
	}
      } else {
	/* It got marked the normal way; deduct the size. */
	mem_use -= (c->end - c->start);
      }
#endif
    }
#if ALLOW_TRACE_COUNT
  } else {
    int save_count = collect_trace_count;
    for (; c; c = c->next) {
      if (!c->marked) {
	void *s;
	c->marked = 1;
	collect_trace_count = 0;
	if (!c->atomic) {
	  PUSH_COLLECT(c->start, c->end, 0);
	  collect();
	}
	collect_trace_count += (c->end - c->start);

	s = INT_TO_PTR(c->start);
#if PAD_BOUNDARY_BYTES
	s = PAD_FORWARD(s);
#endif
	set->count_tracer(s, collect_trace_count);
	mem_traced += collect_trace_count;
      } else {
	/* It got marked the normal way; deduct the size. */
	mem_use -= (c->end - c->start);
      }
    }
    collect_trace_count = save_count;
  }
#endif
}

static void push_uncollectable_common(BlockOfMemory **blocks, GC_Set *set)
{
  int i;

#if ALLOW_TRACE_COUNT
  if (!collecting_with_trace_count) {
#endif
    for (i = 0; i < NUM_COMMON_SIZE; i++) {
      BlockOfMemory *block = blocks[i];
      
      while (block) {
	PUSH_COLLECT(block->start, block->top, 0);
	block = block->next;
      }
    }
#if ALLOW_TRACE_COUNT
  } else {
    int save_count = collect_trace_count;

    for (i = 0; i < NUM_COMMON_SIZE; i++) {
      BlockOfMemory *block = blocks[i];
      
      while (block) {
	uintptr_t size = block->size;
	uintptr_t start = block->start;
	uintptr_t top = block->top;
	int j;
	
	for (j = 0; start < top; start += size, j++) {
	  int bit;
	  int pos;
	  int fbit;

	  pos = POS_TO_UNMARK_INDEX(j);
	  bit = POS_TO_UNMARK_BIT(j);
	  fbit = POS_TO_FREE_BIT(j);

	  if (NOT_MARKED(block->free[pos] & bit)
	      && _NOT_FREE(block->free[pos] & fbit)) {
	    block->free[pos] -= bit;
	    if (set->count_tracer)
	      collect_trace_count = 0;
	    else
	      collect_trace_count += size;
	    if (!block->atomic) {
	      PUSH_COLLECT(start, start + size, 0);
	      collect();
	    }
	    if (set->count_tracer) {
	      void *s;
	      collect_trace_count += size;
	      s = INT_TO_PTR(start);
#if PAD_BOUNDARY_BYTES
	      s = PAD_FORWARD(s);
#endif
	      set->count_tracer(s, collect_trace_count);
	      mem_traced += collect_trace_count;
	    }
	  } else {
	    /* It got marked the normal way; deduct the size. */
	    mem_use -= size;
	  }
	}

	block = block->next;
      }
    }

    if (set->count_tracer)
      collect_trace_count = save_count;
  }
#endif
}


static void push_collect_ignore(uintptr_t s, uintptr_t e, 
				uintptr_t a)
/* Like PUSH_COLLECT, but immediate references to `a' are avoided */
{
  uintptr_t push_from = s;

#if PAD_BOUNDARY_BYTES
  a = PTR_TO_INT(PAD_FORWARD(INT_TO_PTR(a)));
#endif

  for (; s < e; s += PTR_ALIGNMENT) {
    void *d = *(void **)INT_TO_PTR(s);
    uintptr_t p = PTR_TO_INT(d);

    if (p == a) {
      if (push_from != s) {
	PUSH_COLLECT(push_from, s, a);
      }
      push_from = s + PTR_ALIGNMENT;
    }
  }

  if (push_from != s) {
    PUSH_COLLECT(push_from, s, a);
  }
}

static void mark_chunks_for_finalizations(MemoryChunk *c)
{
  for (; c; c = c->next) {
    Finalizer *fn = c->finalizers;

    if (fn) {
      /* Always mark data associated with finalization: */
      uintptr_t p = PTR_TO_INT(&fn->data);
      PUSH_COLLECT(p, p + PTR_SIZE, 0);

      /* If not eager, mark data reachable from finalized block: */
      if (!fn->eager_level && !c->marked && !c->atomic) {
	if (fn->ignore_self)
	  push_collect_ignore(c->start, c->end, c->start);
	else {
	  PUSH_COLLECT(c->start, c->end, 0);
	}
      }
    }
  }

  collect();
}

static void mark_common_for_finalizations(BlockOfMemory **blocks, int atomic)
{
  int i;

  for (i = 0; i < NUM_COMMON_SIZE; i++) {
    BlockOfMemory *block = blocks[i];
    for (; block; block = block->next) {
      Finalizer *fn = block->finalizers;
      for (; fn ; fn = fn->next) {
	uintptr_t p;
	  
	/* Always mark data associated with finalization: */
	p = PTR_TO_INT(&fn->data);
	PUSH_COLLECT(p, p + PTR_SIZE, 0);

	/* If not eager, mark data reachable from finalized block: */
	if (!fn->eager_level) {
	  int pos, apos;
	  int bit, fbit;

	  pos = fn->u.pos;
	  apos = POS_TO_UNMARK_INDEX(pos);
	  bit = POS_TO_UNMARK_BIT(pos);
	  fbit = POS_TO_FREE_BIT(pos);
	  
	  if (NOT_MARKED(block->free[apos] & bit)
	      && _NOT_FREE(block->free[apos] & fbit)) {
	    int size = block->size;
	    
	    if (!atomic) {
	      p = block->start + (pos * size);
	      if (fn->ignore_self)
		push_collect_ignore(p, p + size, p);
	      else {
		PUSH_COLLECT(p, p + size, 0);
	      }

#if WATCH_FOR_FINALIZATION_CYCLES
	      collect();
	      if (IS_MARKED(block->free[apos] & bit))
		FPRINTF(STDERR, "cycle: %lx\n", p);
#endif
	    }
	  }
	}
      }
    }
  }

  collect();
}

static void enqueue_fn(Finalizer *fn)
{
  /* DO NOT COLLECT FROM collect_stack DURING THIS PROCEDURE */

  uintptr_t p;

  num_queued_finalizers++;

  if (last_queued_finalizer) {
    fn->prev = last_queued_finalizer;
    fn->prev->next = fn;
    fn->next = NULL;
  } else {
    fn->next = queued_finalizers;
    if (fn->next)
      fn->next->prev = fn;
    queued_finalizers = fn;
  }
  last_queued_finalizer = fn;

  /* Need to mark watched as in-use, now: */
  /* (if this finalizer is eager, block contents are now marked too) */
  p = PTR_TO_INT(&fn->u.watch);
  PUSH_COLLECT(p, p + PTR_SIZE, 0);
}

static void queue_chunk_finalizeable(MemoryChunk *c, int eager_level)
{
  /* DO NOT COLLECT FROM collect_stack DURING THIS PROCEDURE */

  for (; c; c = c->next) {
    if (c->finalizers && !c->marked) {
      Finalizer *fn = c->finalizers;

      if (fn->eager_level == eager_level) {
	c->finalizers = NULL;

	fn->u.watch = INT_TO_PTR(c->start);
	enqueue_fn(fn);

	if (eager_level) {
	  /* Always mark data associated with finalization: */
	  uintptr_t p = PTR_TO_INT(&fn->data);
	  PUSH_COLLECT(p, p + PTR_SIZE, 0);
	}
      }
    }
  }
}

static void queue_common_finalizeable(BlockOfMemory **blocks, int eager_level)
{
  /* DO NOT COLLECT FROM collect_stack DURING THIS PROCEDURE */

  int i;
  
  for (i = 0; i < NUM_COMMON_SIZE; i++) {
    BlockOfMemory *block = blocks[i];
    for (; block; block = block->next) {
      Finalizer *fn = block->finalizers, *next;
      
      for (; fn; fn = next) {
	int pos, apos;
	int bit;
	  
	next = fn->next;

	pos = fn->u.pos;
	apos = POS_TO_UNMARK_INDEX(pos);
	bit = POS_TO_UNMARK_BIT(pos);
	
	if (NOT_MARKED(block->free[apos] & bit)) {
	  uintptr_t p;
	
	  if (fn->eager_level == eager_level) {
	    if (fn->prev)
	      fn->prev->next = fn->next;
	    else
	      block->finalizers = fn->next;
	    if (fn->next)
	      fn->next->prev = fn->prev;
	    
	    p = block->start + (pos * block->size);
	    fn->u.watch = INT_TO_PTR(p);
	    enqueue_fn(fn);

	    if (eager_level) {
	      /* Always mark data associated with finalization: */
	      p = PTR_TO_INT(&fn->data);
	      PUSH_COLLECT(p, p + PTR_SIZE, 0);
	    }
	  }
	}
      }
    }
  }
}

static void do_disappearing(DisappearingLink **disappearing_ptr)
{
  DisappearingLink *dl, *next, *disappearing;
  void *watch;
  int size;

  disappearing = *disappearing_ptr;

  for (dl = disappearing; dl; dl = next) {
    next = dl->next;
      
    watch = (dl->watch ? dl->watch : *dl->disappear);
    
    size = 0;
    if (watch && !find_ptr(watch, &size, NULL, NULL, NULL, 0)) {
      /* was the pointer allocated at all? */
      if (size) {
	/* It was allocated, and now it's gone: */
	if (dl->kind != dl_restored) {
	  *dl->disappear = NULL;
	  /* disappear is done */
	  if (dl->prev)
	    dl->prev->next = dl->next;
	  else
	    disappearing = dl->next;
	  if (dl->next)
	    dl->next->prev = dl->prev;
	  
	  mem_real_use -= sizeof(DisappearingLink);
	  free_managed(dl);
	  --GC_dl_entries;
	} else {
	  /* We'll need to restore this one: */
	  dl->saved_value = *dl->disappear;
	  *dl->disappear = NULL;
	}
      }
    }
  }

  *disappearing_ptr = disappearing;
}

static void trim_disappearing(DisappearingLink **disappearing_ptr)
{
  DisappearingLink *dl, *next, *disappearing;

  disappearing = *disappearing_ptr;

  for (dl = disappearing; dl; dl = next) {
    int size;

    next = dl->next;
    
    size = 0;
    if (!find_ptr(dl->disappear, &size, NULL, NULL, NULL, 0) && size) {
      /* Found it, but it was unmarked. Deregister disappearing. */
      if (dl->prev)
	dl->prev->next = dl->next;
      else
	disappearing = dl->next;
      if (dl->next)
	dl->next->prev = dl->prev;

      mem_real_use -= sizeof(DisappearingLink);
      free_managed(dl);
      --GC_dl_entries;
    }
  }

  *disappearing_ptr = disappearing;
}

static void do_disappear_and_finals()
{
  DisappearingLink *dl, *next;
  Finalizer *fn;
  int j;

  /* Mark data in (not-yet-finalized) queued finalizable */
  for (fn = queued_finalizers; fn; fn = fn->next) {
    uintptr_t p;

    p = PTR_TO_INT(&fn->u.watch);
    PUSH_COLLECT(p, p + PTR_SIZE, 0);

    p = PTR_TO_INT(&fn->data);
    PUSH_COLLECT(p, p + PTR_SIZE, 0);
  }
  collect();
  if (GC_push_last_roots_again) { GC_push_last_roots_again(); collect(); }

#if !NO_DISAPPEARING
  /* Do disappearing: */
  do_disappearing(&disappearing);
#endif

  /* Queue unreachable eager finalizable, level 1: */  
  /* DO NOT COLLECT FROM collect_stack UNTIL AFTER THIS LOOP */
  /* (Otherwise, some ready eager finalizations may not be queued.) */
  for (j = 0; j < num_common_sets; j++) {
    queue_chunk_finalizeable(*(common_sets[j]->othersptr), 1);
    queue_common_finalizeable(common_sets[j]->blocks, 1);
  }
  collect();
  if (GC_push_last_roots_again) { GC_push_last_roots_again(); collect(); }

  /* Queue unreachable eager finalizable, level 2: */  
  /* DO NOT COLLECT FROM collect_stack UNTIL AFTER THIS LOOP */
  for (j = 0; j < num_common_sets; j++) {
    queue_chunk_finalizeable(*(common_sets[j]->othersptr), 2);
    queue_common_finalizeable(common_sets[j]->blocks, 2);
  }
  collect();
  if (GC_push_last_roots_again) { GC_push_last_roots_again(); collect(); }

  /* Mark reachable from (non-eager) finalized blocks: */
  for (j = 0; j < num_common_sets; j++) {
    mark_chunks_for_finalizations(*(common_sets[j]->othersptr));
    mark_common_for_finalizations(common_sets[j]->blocks, common_sets[j]->atomic);
  }

  /* Queue unreachable (non-eager) finalizable: */  
  for (j = 0; j < num_common_sets; j++) {
    queue_chunk_finalizeable(*(common_sets[j]->othersptr), 0);
    queue_common_finalizeable(common_sets[j]->blocks, 0);
  }
  collect();

  /* Restore disappeared links where watch value is NULL: */
  for (dl = disappearing; dl; dl = next) {
    next = dl->next;
    if ((dl->kind == dl_restored) && dl->saved_value) {
      /* Restore disappearing value and deregister */
      *dl->disappear = dl->saved_value;
      dl->saved_value = NULL;
    }
  }

  if (GC_push_last_roots_again) { GC_push_last_roots_again(); collect(); }

  /* Deregister dangling disappearings: */
  trim_disappearing(&disappearing);
  trim_disappearing(&late_disappearing);

#if !NO_DISAPPEARING
  /* Do late disappearing: */
  do_disappearing(&late_disappearing);
#endif

  if (GC_custom_finalize)
    GC_custom_finalize();
}

static int compare_roots(const void *a, const void *b)
{
  if (*(uintptr_t *)a < *(uintptr_t *)b)
    return -1;
  else
    return 1;
}

static void sort_and_merge_roots()
{
  static int counter = 0;
  int i, offset, top;

  if (roots_count < 4)
    return;

  /* Only try this every 5 collections or so: */
  if (counter--)
    return;
  counter = 5;

  qsort(roots, roots_count >> 1, 2 * sizeof(uintptr_t), compare_roots);
  offset = 0;
  top = roots_count;
  for (i = 2; i < top; i += 2) {
    if ((roots[i - 2 - offset] <= roots[i])
	&& ((roots[i - 1 - offset] + (PTR_ALIGNMENT - 1)) >= roots[i])) {
      /* merge: */
      if (roots[i + 1] > roots[i - 1 - offset])
	roots[i - 1 - offset] = roots[i + 1];
      offset += 2;
      roots_count -= 2;
    } else if (offset) {
      /* compact: */
      roots[i - offset] = roots[i];
      roots[i + 1 - offset] = roots[i + 1];
    }
  }
}

static void run_finalizers(void)
{
  static int doing = 0;
  Finalizer *fn;
  void *s;

  /* don't allow nested finalizations */
  if (doing)
    return;
  doing++;

#if !NO_FINALIZING
  while (queued_finalizers) {
    fn = queued_finalizers;
    queued_finalizers = fn->next;
    if (!fn->next)
      last_queued_finalizer = NULL;

    --num_queued_finalizers;

    s = fn->u.watch;
    
#if PAD_BOUNDARY_BYTES
    s = PAD_FORWARD(s);
#endif

    fn->f(s, fn->data);

    mem_real_use -= sizeof(Finalizer);
    free_managed(fn);
    --GC_fo_entries;
  }
#endif

  doing--;
}

#if ALLOW_TRACE_COUNT
static int traced_from_roots, traced_from_stack, traced_from_uncollectable, traced_from_finals;
#endif

#ifdef WIN32
# define GETTIME() 0
#else
# if 0
extern intptr_t scheme_get_milliseconds(void);
#  define GETTIME() scheme_get_milliseconds()
# else
extern intptr_t scheme_get_process_milliseconds(void);
#  define GETTIME() scheme_get_process_milliseconds()
# endif
#endif

#if TIME
# define PRINTTIME(x) FPRINTF x
static intptr_t started, rightnow, old;
# define INITTIME() (started = GETTIME())
# define GETTIMEREL() (rightnow = GETTIME(), old = started, started = rightnow, rightnow - old)
#else
# define INITTIME() /* empty */
# define PRINTTIME(x) /* empty */
#endif

/* Immitate Boehm's private GC call; used by Racket */
void GC_push_all_stack(void *sp, void *ep)
{
  uintptr_t s, e;

  s = PTR_TO_INT(sp);
  e = PTR_TO_INT(ep);

  PUSH_COLLECT(s, e, 0);

  prepare_stack_collect();
}

void GC_flush_mark_stack()
{
  collect();  
}

#if PRINT_INFO_PER_GC
static intptr_t last_gc_end;
#endif

static void do_GC_gcollect(void *stack_now)
{
  intptr_t root_marked;
  int j;

#if PRINT_INFO_PER_GC
  intptr_t orig_mem_use = mem_use;
  intptr_t start_time;
  start_time = GETTIME();
  FPRINTF(STDERR, "gc at %ld (%ld): %ld after %ld msecs\n",
	  mem_use, sector_mem_use, 
# if GET_MEM_VIA_SBRK
	  (intptr_t)sbrk(0),
# elif defined(WIN32) && AUTO_STATIC_ROOTS_IF_POSSIBLE
	  total_memory_use(),
# else
	  (intptr_t)0,
# endif
	  start_time - last_gc_end);
# if SHOW_SECTOR_MAPS_AT_GC
  dump_sector_map("");
# endif
#endif

  if (!GC_stackbottom) {
    /* Stack position not yet initialized; delay collection */
    if (mem_use)
      mem_limit = MEM_USE_FACTOR * mem_use;
    return;
  }

  if (!initialized)
    GC_initialize();

  if (!statics_setup)
    init_static_variables();

  if (GC_collect_start_callback)
    GC_collect_start_callback();

#if CHECK_COLLECTING
  collecting_now = 1;
#endif

#if !NO_COLLECTIONS

# if ALWAYS_TRACE && ALLOW_TRACE_COUNT
  collecting_with_trace_count = 1;
# endif

# if CHECK
  cmn_count = chk_count = 0;
# endif

  INITTIME();
  PRINTTIME((STDERR, "gc: init start: %ld\n", GETTIMEREL()));

  for (j = 0; j < num_common_sets; j++) {
# if ALLOW_SET_LOCKING
    if (!common_sets[j]->locked) {
# endif
      collect_init_chunk(*(common_sets[j]->othersptr),
			 common_sets[j]->uncollectable,
			 j);
      collect_init_common(common_sets[j]->blocks,
			  common_sets[j]->uncollectable,
			  j);
# if ALLOW_SET_LOCKING
    }
# endif
  }

# if CHECK
  if (num_chunks != chk_count) {
    FPRINTF(STDERR, "bad chunk count: %ld != %ld\n", num_chunks, chk_count);
  }

  if (num_blocks != cmn_count) {
    FPRINTF(STDERR, "bad block count: %ld != %ld\n", num_blocks, cmn_count);
  }
# endif

# if PRINT
  FPRINTF(STDERR, "gc at %ld (%ld)\n", mem_use, mem_real_use);
  FPRINTF(STDERR,
	  "low: %lx hi: %lx blocks: %ld chunks: %ld\n", 
	  low_plausible, high_plausible, 
	  num_blocks, num_chunks);
# endif

  mem_use = 0;

  sort_and_merge_roots();

# if ALLOW_TRACE_COUNT
  init_trace_stack(&collect_trace_stack);
  init_trace_stack(&collect_wait_trace_stack);
  collect_start_tracing = 0;
  collect_end_tracing = -1;
# endif
# if ALLOW_TRACE_PATH
  init_trace_stack(&collect_trace_path_stack);
# endif

  prepare_collect_temp();

  /*** Mark from roots ***/
  collect_stack_size = roots_count ? COLLECT_STACK_FRAME_SIZE * roots_count : 10;
  if (collect_stack_size < INITIAL_COLLECT_STACK_SIZE)
    collect_stack_size = INITIAL_COLLECT_STACK_SIZE;
  collect_stack_count = 0;
  collect_stack = (uintptr_t *)realloc_collect_temp(NULL,
							0,
							sizeof(uintptr_t) 
							* (collect_stack_size + 2));

  for (j = 0; j < roots_count; j += 2) {
    collect_stack[collect_stack_count++] = roots[j];
    collect_stack[collect_stack_count++] = roots[j + 1];
# if KEEP_DETAIL_PATH
    collect_stack[collect_stack_count++] = 0;
# endif
  }

  if (GC_initial_trace_root) {
# if CHECK_SKIP_MARK_AT_FIRST
    collect_start_disable_mark_skip = collect_stack_count;
    skip_mark_at_first = GC_inital_root_skip;
# endif
    collect_stack[collect_stack_count++] = (uintptr_t)&GC_initial_trace_root;
    collect_stack[collect_stack_count++] = ((uintptr_t)&GC_initial_trace_root) + 1;
# if KEEP_DETAIL_PATH
    collect_stack[collect_stack_count++] = 0;
# endif
  }

  PRINTTIME((STDERR, "gc: root collect start: %ld\n", GETTIMEREL()));

# if ALLOW_TRACE_COUNT
  collect_trace_count = 0;
  mem_traced = 0;
# endif

# if ALLOW_TRACE_PATH
  current_trace_source = "root";
# endif

  collect();

# if ALLOW_SET_LOCKING
  for (j = 0; j < num_common_sets; j++) {
    if (common_sets[j]->locked) {
      int a = common_sets[j]->atomic;
      push_locked_chunk(*(common_sets[j]->othersptr), a);
      push_locked_common(common_sets[j]->blocks, a);
    }
  }

  collect();
# endif

# if ALLOW_TRACE_COUNT
  traced_from_roots = collect_trace_count;
  collect_trace_count = 0;
# endif

  root_marked = mem_use;

  PRINTTIME((STDERR, "gc: stack push start: %ld\n", GETTIMEREL()));

  /*** Mark from stack ***/
  push_stack(stack_now);
  
# if PRINT && 0
  FPRINTF(STDERR, "stack until: %ld\n", collect_end_stackbased);
# endif

# if ALLOW_TRACE_PATH
  current_trace_source = "stack";
# endif

  PRINTTIME((STDERR, "gc: stack collect start: %ld\n", GETTIMEREL()));

  collect();

# if ALLOW_TRACE_COUNT
  traced_from_stack = collect_trace_count;
  collect_trace_count = 0;
# endif

  PRINTTIME((STDERR, "gc: uncollectable start: %ld\n", GETTIMEREL()));

  /*** Uncollectable and pointerful ***/
  for (j = 0; j < num_common_sets; j++)
    if (common_sets[j]->uncollectable)
      if (!common_sets[j]->atomic
# if ALLOW_TRACE_COUNT
	  || collecting_with_trace_count
# endif
	  ) {
	push_uncollectable_chunk(*(common_sets[j]->othersptr), common_sets[j]);
	push_uncollectable_common(common_sets[j]->blocks, common_sets[j]);
      }

# if ALLOW_TRACE_PATH
  current_trace_source = "uncollectable";
# endif

  collect();

# if ALLOW_TRACE_COUNT
  traced_from_uncollectable = collect_trace_count;
  collect_trace_count = 0;
# endif

# if ALLOW_TRACE_PATH
  /* External stacks may collect eagerly: */
  current_trace_source = "xstack";
# endif

  if (GC_push_last_roots) {
    PRINTTIME((STDERR, "gc: last roots push start: %ld\n", GETTIMEREL()));
    /*** ``Last'' roots external hook ***/
    GC_push_last_roots();
    PRINTTIME((STDERR, "gc: last roots start: %ld\n", GETTIMEREL()));
  }

# if ALLOW_TRACE_PATH
  current_trace_source = "xstack";
# endif

  collect();
  
# if ALLOW_TRACE_COUNT
  /* Count this as stack tracing */
  traced_from_stack += collect_trace_count;
  collect_trace_count = 0;
# endif
  
  PRINTTIME((STDERR, "gc: queue finalize start: %ld\n", GETTIMEREL()));

# if ALLOW_TRACE_PATH
  current_trace_source = "finalization";
# endif

  /*** Disappearing Links and Finalization ***/
  do_disappear_and_finals();

# if ALLOW_TRACE_COUNT
  traced_from_finals = collect_trace_count;
# endif

  PRINTTIME((STDERR, "gc: finish start: %ld\n", GETTIMEREL()));

  low_plausible = high_plausible = 0;

  for (j = 0; j < num_common_sets; j++) {
    FINISH_STATISTIC(num_finishes_stat++);
    collect_finish_chunk(common_sets[j]->othersptr, common_sets[j]);
    collect_finish_common(common_sets[j]->blocks, 
			  common_sets[j]->block_ends,
			  common_sets[j]);
  }

  PRINTTIME((STDERR, "gc: all done: %ld\n", GETTIMEREL()));

# if PRINT
  FPRINTF(STDERR,
	  "done %ld (%ld), %ld from stack\n", mem_use, mem_real_use,
	  mem_use - root_marked);
# endif

  if (mem_use) {
# if USE_GC_FREE_SPACE_DIVISOR
    intptr_t root_size;

    if (roots_count)
      root_size = roots[1] - roots[0];
    else
      root_size = 0;

    mem_limit = mem_use + ((sector_mem_use + root_size) / GC_free_space_divisor);
# else
    mem_limit = MEM_USE_FACTOR * mem_use;
# endif
  }

  free_collect_temp(collect_stack, sizeof(uintptr_t) * (collect_stack_size + 1));

# if ALLOW_TRACE_COUNT
  done_trace_stack(&collect_trace_stack);
  done_trace_stack(&collect_wait_trace_stack);
# endif
# if ALLOW_TRACE_PATH
  done_trace_stack(&collect_trace_path_stack);
# endif

#else
  if (mem_use)
    mem_limit = MEM_USE_FACTOR * mem_use;
#endif

#if PRINT_INFO_PER_GC
  FPRINTF(STDERR, "done  %ld (%ld); recovered %ld in %ld msecs\n",
	  mem_use, sector_mem_use, orig_mem_use - mem_use,
	  (intptr_t)GETTIME() - start_time);
# if SHOW_SECTOR_MAPS_AT_GC
  dump_sector_map("                            ");
# endif
  last_gc_end = GETTIME();
#endif

#if STAMP_AND_REMEMBER_SOURCE
  stamp_clock++;
#endif

#if CHECK_COLLECTING
  collecting_now = 0;
#endif

  if (GC_collect_end_callback)
    GC_collect_end_callback();

  /* Run queued finalizers. Garbage collections may happen: */
  PRINTTIME((STDERR, "gc: finalize start: %ld\n", GETTIMEREL()));
  run_finalizers();
  PRINTTIME((STDERR, "gc: finalize end: %ld\n", GETTIMEREL()));

#if MARK_STATS
  fprintf(STDERR, 
	  "mark stats:\n"
	  " %d pairs\n"
	  " %d lookups\n"
	  "   %d interior\n"
	  "   %d plausible\n"
	  "     %d paged\n"
	  "       %d block page\n"
	  "         %d block\n"
	  "           %d block aligned\n"
	  "             %d block mark\n"
	  "               %d block pushes\n"
	  "                 %d block tail pushes\n"
	  "       %d chunk page\n"
	  "         %d chunk mark\n",
	  num_pairs_stat,
	  num_checks_stat,
	  num_interior_checks_stat,
	  num_plausibles_stat,
	  num_pages_stat,
	  num_blocks_stat,
	  num_blockallocs_stat,
	  num_blockaligns_stat,
	  num_blockmarks_stat,
	  num_blockpushes_stat,
	  num_blockpushes_tail_stat,
	  num_chunks_stat,
	  num_chunkmarks_stat);
#endif
#if ALLOC_STATS
  fprintf(STDERR, 
	  "alloc stats:\n"
	  " %d allocs\n"
	  "   %d nonzero allocs\n"
	  "   %d common allocs\n"
	  "     %d common tries\n"
	  "     %d common fails\n"
	  "     %d common second tries\n"
	  "     %d common newblocks\n"
	  "   %d chunk allocs\n",
	  num_allocs_stat,
	  num_nonzero_allocs_stat,
	  num_common_allocs_stat,
	  num_block_alloc_checks_stat,
	  num_block_alloc_nexts_stat,
	  num_block_alloc_second_checks_stat,
	  num_newblock_allocs_stat,
	  num_chunk_allocs_stat);
#endif
#if FINISH_STATS
  fprintf(STDERR,
	  "finish stats:\n"
	  " %d finishes\n"
	  "  %d chunk finishes\n"
	  "   %d chunk keep finishes\n"
	  "   %d chunk free finishes\n"
	  "  %d block finishes\n"
	  "   %d block filter steps\n"
	  "   %d block keep finishes\n"
	  "   %d block free finishes\n"
	  "   %d block adjust finishes\n",
	  num_finishes_stat,
	  num_finish_chunk_stat,
	  num_finish_chunkkeep_stat,
	  num_finish_chunkfree_stat,
	  num_finish_block_stat,
	  num_finish_blockfiltercycles_stat,
	  num_finish_blockkeep_stat,
	  num_finish_blockfree_stat,
	  num_finish_blockadjust_stat);
#endif
}

void GC_gcollect(void)
{
  intptr_t dummy;

  if (!sector_mem_use)
    return;

  if (GC_dont_gc)
    return;

  FLUSH_REGISTER_WINDOWS;
  if (!setjmp(buf))
    do_GC_gcollect((void *)&dummy);
}

int GC_trace_count(int *stack, int *roots, int *uncollectable, int *final)
{
#if ALLOW_TRACE_COUNT
  int j;

  if (!sector_mem_use)
    return 0;

  for (j = 0; j < num_common_sets; j++) {
    if (common_sets[j]->trace_init)
      common_sets[j]->trace_init();
  }

  collecting_with_trace_count = 1;
  GC_gcollect();
  collecting_with_trace_count = 0;

  if (stack)
    *stack = traced_from_stack;
  if (roots)
    *roots = traced_from_roots;
  if (uncollectable)
    *uncollectable = traced_from_uncollectable;
  if (final)
    *final = traced_from_finals;

  for (j = 0; j < num_common_sets; j++) {
    if (common_sets[j]->trace_done)
      common_sets[j]->trace_done();
  }

  return mem_traced;
#else
  return 0;
#endif
}

void GC_trace_path(void)
{
#if ALLOW_TRACE_PATH
  int j;

  if (!sector_mem_use)
    return;

  for (j = 0; j < num_common_sets; j++) {
    if (common_sets[j]->trace_init)
      common_sets[j]->trace_init();
  }

  trace_path_buffer_pos = 0;

  collecting_with_trace_path = 1;
  GC_gcollect();
  collecting_with_trace_path = 0;

  for (j = 0; j < num_common_sets; j++) {
    if (common_sets[j]->trace_done)
      common_sets[j]->trace_done();
  }
#endif
}

void GC_store_path(void *v, uintptr_t src, void *path_data)
{
  /* Note: a trace path of the form X->X->Y->Z->... (with two Xs)
     indicates an off-by-one stack source. */
#if ALLOW_TRACE_PATH
  TraceStack *s = (TraceStack *)path_data;
  int len, i;

  if (trace_path_buffer_pos < 0)
    return;

  len = s->count / 3;
  if (len * 2 + 3 > (TRACE_PATH_BUFFER_SIZE - trace_path_buffer_pos - 7)) {
    trace_path_buffer[trace_path_buffer_pos++] = (void *)2;
    trace_path_buffer[trace_path_buffer_pos++] = "truncated";
    trace_path_buffer[trace_path_buffer_pos++] = 0;
    trace_path_buffer[trace_path_buffer_pos++] = v; /* already padded */
    trace_path_buffer[trace_path_buffer_pos++] = 0;
    trace_path_buffer[trace_path_buffer_pos] = 0;
    trace_path_buffer_pos = -1;
    return;
  }

  if (len) {
    uintptr_t prev = 0;

    trace_path_buffer[trace_path_buffer_pos++] = (void *)(len + 2);
    trace_path_buffer[trace_path_buffer_pos++] = current_trace_source;
    trace_path_buffer[trace_path_buffer_pos++] = 0;
    for (i = 1; len--; i += 3) {
      trace_path_buffer[trace_path_buffer_pos++] = (void *)PAD_FORWARD(s->stack[i]);
      trace_path_buffer[trace_path_buffer_pos++] = 0; /* reset on next iteration */

      if (i > 1) {
	/* See if we have offset information in the original trace info.
	   (It might be missing because KEEP_DETAIL might be turned off, or
            PUSH_COLLECT had 0 for its third argument.) */
	uintptr_t diff;
	if (s->stack[i + 1])
	  diff = ((uintptr_t)s->stack[i + 1]) - prev;
	else
	  diff = 0;
	trace_path_buffer[trace_path_buffer_pos - 3] = (void *)diff;
      }
      prev = (uintptr_t)s->stack[i];
    }

    trace_path_buffer[trace_path_buffer_pos - 1] = (void *)(src - prev);

    trace_path_buffer[trace_path_buffer_pos++] = v; /* already padded */
    trace_path_buffer[trace_path_buffer_pos++] = 0;
    trace_path_buffer[trace_path_buffer_pos] = 0;
  }
#endif
}

void **GC_get_next_path(void **prev, int *len)
{
#if ALLOW_TRACE_PATH
  void **p;

  if (!prev)
    p = trace_path_buffer;
  else
    p = prev + (2 * (((intptr_t *)prev)[-1]));
    
  *len = *(intptr_t *)p;
  if (!*len)
    return NULL;

  return p + 1;
#else
  return NULL;
#endif
}

void GC_clear_paths(void)
{
#if ALLOW_TRACE_PATH
  int i;

  for (i = 0; i < TRACE_PATH_BUFFER_SIZE; i++)
    trace_path_buffer[i] = NULL;
#endif
}

/**********************************************************************/

#if FPRINTF_USE_PRIM_STRINGOUT

#if PRIM_STRINGOUT_AS_FWRITE
void GC_prim_stringout(char *s, int len)
{
  fwrite(s, len, 1, stderr);
}
#else
# if PRIM_STRINGOUT_AS_WINDOWS_CONSOLE
void GC_prim_stringout(char *s, int len)
{
  static HANDLE console;
  DWORD wrote;

  if (!console) {
	COORD size;
    AllocConsole();
    console = GetStdHandle(STD_OUTPUT_HANDLE);
	size.X = 90;
	size.Y = 500;
	SetConsoleScreenBufferSize(console, size);
  }

  WriteConsoleA(console, s, len, &wrote, NULL);
}
# else
extern void GC_prim_stringout(char *s, int len);
# endif
#endif

#include <stdarg.h>
#include <ctype.h>

#define NP_BUFSIZE 512

/* Non-allocating printf. */
static void sgc_fprintf(int ignored, const char *c, ...)
{
  char buffer[NP_BUFSIZE];
  int pos;
  va_list args;

  va_start(args, c);

  pos = 0;
  while (*c) {
    if (*c == '%') {
      int len = -1, slen;
      int islong = 0;
      char *s;

      if (pos) {
	GC_prim_stringout(buffer, pos);
	pos = 0;
      }

      c++;
      if (isdigit(*c)) {
	len = 0;
	while (isdigit(*c)) {
	  len = (len * 10) + (*c - '0');
	  c++;
	}
      }

      if (*c == 'l') {
	islong = 1;
	c++;
      }
      
      switch (*c) {
      case 'd':
      case 'x':
	{
	  intptr_t v;
	  int d, i;

	  if (islong) {
	    v = va_arg(args, intptr_t);
	  } else {
	    v = va_arg(args, int);
	  }
	  
	  if (!v) {
	    s = "0";
	    slen = 1;
	  } else {
	    int neg = 0;

	    i = NP_BUFSIZE - 2;
	    
	    if (v < 0) {
	      neg = 1;
	      v = -v;
	    }

	    d = (((*c) == 'd') ? 10 : 16);
	    while (v) {
	      int digit = (v % d);
	      if (digit < 10)
		digit += '0';
	      else
		digit += 'a' - 10;
	      buffer[i--] = digit;
	      v = v / d;
	    }
	    if (neg)
	      buffer[i--] = '-';

	    s = buffer + i + 1;
	    slen = (NP_BUFSIZE - 2) - i;
	  }
	}
	break;
      case 's':
	s = va_arg(args, char*);
	slen = strlen(s);
	break;
      default:
	s = "???";
	slen = 3;
	break;
      }

      c++;

      if (len != -1) {
	if (slen > len)
	  slen = len;
	else {
	  int i;
	  for (i = slen; i < len; i++)
	    GC_prim_stringout(" ", 1);
	}
      }
      
      if (slen)
	GC_prim_stringout(s, slen);
    } else {
      if (pos == (NP_BUFSIZE - 1)) {
	GC_prim_stringout(buffer, pos);
	pos = 0;
      }
      buffer[pos++] = *(c++);
    }
  }

  if (pos)
    GC_prim_stringout(buffer, pos);

  /* Suggest a flush: */
  GC_prim_stringout(NULL, 0);

  va_end(args);
}

#endif


