#include "commongc_internal.h"

#if defined(MZ_PRECISE_GC) && !defined(USE_COMPACT_3M_GC)

/* This is the log base 2 of the standard memory page size. 14 means 2^14,
   which is 16k. This seems to be a good size for most platforms.
   Under Windows as of 2008, however, the allocation granularity is 64k. */
#ifdef _WIN32
# define LOG_APAGE_SIZE 16
#else
# define LOG_APAGE_SIZE 14
#endif

#ifdef SIXTY_FOUR_BIT_INTEGERS
# define OBJH_WORD_SIZE 8
#else
# define OBJH_WORD_SIZE 4
#endif

struct objhead {
  unsigned long hash      : ((8*OBJH_WORD_SIZE) - (4+3+LOG_APAGE_SIZE));
  /* the type and size of the object */
  unsigned long type      : 3;
  /* these are the various mark bits we use */
  unsigned long mark      : 1;
  unsigned long btc_mark  : 1;
  /* these are used for compaction et al*/
  unsigned long moved     : 1;
  unsigned long dead      : 1;
  unsigned long size      : LOG_APAGE_SIZE;
};

XFORM_NONGCING extern int GC_is_allocated(void *p);

#define OBJHEAD_HAS_HASH_BITS
#define OBJHEAD_HASH_BITS(p) ((struct objhead *)((void **)p - 1))->hash

#endif

typedef struct mpage {
  struct mpage *next;
  struct mpage *prev;
  void *addr;
  unsigned long previous_size;
  unsigned long size;
  unsigned char generation;
/*
  unsigned char back_pointers :1;
  unsigned char big_page      :2;
  unsigned char page_type     :3; 
  unsigned char marked_on     :1;
  unsigned char has_new       :1;
  unsigned char mprotected    :1;
*/
  unsigned char back_pointers ;
  unsigned char big_page      ;
  unsigned char page_type     ; 
  unsigned char marked_on     ;
  unsigned char has_new       ;
  unsigned char mprotected    ;
  unsigned short live_size;
  void **backtrace;
} mpage;

typedef struct Gen0 {
 struct mpage *curr_alloc_page;
 struct mpage *pages;
 struct mpage *big_pages;
 unsigned long GC_gen0_alloc_page_ptr;
 unsigned long current_size;
 unsigned long max_size;
} Gen0;

typedef struct Weak_Finalizer {
  void *p;
  int offset;
  void *saved;
  struct Weak_Finalizer *next;
} Weak_Finalizer;

typedef struct GC_Thread_Info {
  void *thread;
  int owner;
  struct GC_Thread_Info *next;
} GC_Thread_Info;


typedef struct NewGC {
  Gen0 gen0;
  Mark_Proc  *mark_table;   /* the table of mark procs */
  Fixup_Proc *fixup_table;  /* the table of repair procs */
#ifdef SIXTY_FOUR_BIT_INTEGERS
  mpage ****page_mapss;
#else
  mpage **page_map;
#endif

  /* Finalization */
  Fnl *run_queue;
  Fnl *last_in_queue;
  Weak_Finalizer *weak_finalizers;

  struct NewGC *primoridal_gc;
  unsigned long max_heap_size;
  unsigned long max_pages_in_heap;
  unsigned long max_pages_for_use;
  unsigned long used_pages;
  unsigned long actual_pages_size;
  unsigned long in_unsafe_allocation_mode :1;
  void (*unsafe_allocation_abort)();
  unsigned long memory_in_use; /* the amount of memory in use */

  /* blame the child saved off Mark_Proc pointers */
  Mark_Proc normal_thread_mark;
  Mark_Proc normal_custodian_mark;
  Mark_Proc normal_cust_box_mark;
  GC_Thread_Info *thread_infos;

  mpage *release_pages;
  unsigned long stack_base;
  int dumping_avoid_collection; /* dumping coutner flag */

  int generations_available;
  unsigned char full_needed_for_finalization :1;
  unsigned char no_further_modifications     :1;
  unsigned char gc_full                      :1; /* a flag saying if this is a full/major collection */


  /* These collect information about memory usage, for use in GC_dump. */
  unsigned long peak_memory_use;
  unsigned long num_minor_collects;
  unsigned long num_major_collects;

  GC_Immobile_Box *immobile_boxes;

  /* Common with CompactGC */
  void *park[2];
  void *park_save[2];

  unsigned short weak_array_tag;
  unsigned short weak_box_tag;
  unsigned short ephemeron_tag;
  unsigned short cust_box_tag;

  Roots roots;
  GC_Weak_Array *weak_arrays;
  GC_Weak_Box   *weak_boxes;
  GC_Ephemeron  *ephemerons;
  int num_last_seen_ephemerons;
} NewGC;

void NewGC_initialize(NewGC *newgc) {
  memset(newgc, 0, sizeof(NewGC));
  newgc->mark_table = malloc(NUMBER_OF_TAGS * sizeof (Mark_Proc)); 
  newgc->fixup_table = malloc(NUMBER_OF_TAGS * sizeof (Fixup_Proc)); 
#ifdef SIXTY_FOUR_BIT_INTEGERS
  newgc->page_mapss = malloc(PAGEMAP64_LEVEL1_SIZE * sizeof (mpage***)); 
#else
  newgc->page_map = malloc(PAGEMAP32_SIZE * sizeof (mpage*)); 
#endif

  newgc->primoridal_gc              = NULL;
  newgc->max_heap_size              = 0;
  newgc->max_pages_in_heap          = 0;
  newgc->max_pages_for_use          = 0;
  newgc->used_pages                 = 0;
  newgc->actual_pages_size          = 0;
  newgc->in_unsafe_allocation_mode  = 0;
  newgc->unsafe_allocation_abort    = NULL;

  newgc->roots.count = 0;
  newgc->roots.size = 0;
  newgc->roots.roots = NULL;
  newgc->roots.nothing_new = 0;

  newgc->weak_arrays = NULL;
  newgc->weak_boxes = NULL;
  newgc->ephemerons = NULL;
  newgc->num_last_seen_ephemerons = 0;
  newgc->generations_available = 1;
}
