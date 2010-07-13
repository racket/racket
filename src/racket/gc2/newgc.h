#include "commongc_internal.h"
#include "gc2_obj.h"

#if defined(MZ_USE_PLACES)
/*
# define GC_DEBUG_PAGES
# define MASTER_ALLOC_DEBUG
# define KILLING_DEBUG
*/
#endif


typedef struct mpage {
  struct mpage *next;
  struct mpage *prev;
  void *addr;
  unsigned long previous_size; /* for med page, place to search for available block; for jit nursery, allocated size */
  unsigned long size; /* big page size, med page element size, or nursery starting point */
/*
  unsigned char generation    :1;
  unsigned char back_pointers :1;
  unsigned char size_cless    :2;
  unsigned char page_type     :3; 
  unsigned char marked_on     :1;
  unsigned char has_new       :1;
  unsigned char mprotected    :1;
  unsigned char added         :1;
*/
  unsigned char generation    ;
  unsigned char back_pointers ;
  unsigned char size_class    ; /* 0 => small; 1 => med; 2 => big; 3 => big marked */
  unsigned char page_type     ; 
  unsigned char marked_on     ;
  unsigned char has_new       ;
  unsigned char mprotected    ;
  unsigned char added         ;
  unsigned short live_size;
#ifdef MZ_GC_BACKTRACE
  void **backtrace;
  void *backtrace_page_src;
#endif
  void *mmu_src_block;
} mpage;

typedef struct Gen0 {
 struct mpage *curr_alloc_page;
 struct mpage *pages;
 struct mpage *big_pages;
 unsigned long current_size;
 unsigned long max_size;
} Gen0;

typedef struct MarkSegment {
  struct MarkSegment *prev;
  struct MarkSegment *next;
  void **top;
} MarkSegment;

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

typedef struct AccountHook {
  int type;
  void *c1;
  void *c2;
  unsigned long amount;
  struct AccountHook *next;
} AccountHook;

typedef struct OTEntry {
  void *originator;
  void **members;
  unsigned long memory_use;
  unsigned long single_time_limit;
  unsigned long super_required;
  char limit_set;
  char required_set;
} OTEntry;

typedef struct Range {
  unsigned long start, len;
  struct Range *left, *right, *prev, *next;
} Range;

typedef struct Page_Range {
  Range *range_root;
  Range *range_start;
  void *range_alloc_block;
  unsigned long range_alloc_size;
  unsigned long range_alloc_used;
} Page_Range;

typedef struct {
  char *start;
  long len;
  short age;
  short zeroed;
} AllocCacheBlock;

#ifdef MZ_USE_PLACES
typedef struct NewGCMasterInfo {
  unsigned long size;
  unsigned long alive;
  unsigned long ready;
  void **signal_fds;
  mzrt_rwlock *cangc;
  mzrt_sema *wait_sema;
} NewGCMasterInfo;
#endif

#ifdef SIXTY_FOUR_BIT_INTEGERS
typedef mpage ****PageMap;
#else
typedef mpage **PageMap;
#endif

#define NUM_MED_PAGE_SIZES (((LOG_APAGE_SIZE - 1) - 3) + 1)

typedef struct NewGC {
  Gen0 gen0;
  Mark2_Proc  *mark_table;   /* the table of mark procs */
  Fixup2_Proc *fixup_table;  /* the table of repair procs */
  PageMap page_maps;
  /* All non-gen0 pages are held in the following structure. */
  struct mpage *gen1_pages[PAGE_TYPES];

  struct mpage *med_pages[NUM_MED_PAGE_SIZES];
  struct mpage *med_freelist_pages[NUM_MED_PAGE_SIZES];

  MarkSegment *mark_stack;

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
  void (*unsafe_allocation_abort)(struct NewGC *);
  unsigned long memory_in_use; /* the amount of memory in use */

  /* blame the child thread infos */
  GC_Thread_Info *thread_infos;

  mpage *release_pages;
  unsigned long stack_base;
  int dumping_avoid_collection; /* dumping coutner flag */

  unsigned char generations_available        :1;
  unsigned char in_unsafe_allocation_mode    :1;
  unsigned char full_needed_for_finalization :1;
  unsigned char no_further_modifications     :1;
  unsigned char gc_full                      :1; /* a flag saying if this is a full/major collection */
  unsigned char running_finalizers           :1;

  /* blame the child */
  unsigned int doing_memory_accounting        :1;
  unsigned int really_doing_accounting        :1;
  unsigned int old_btc_mark                   :1;
  unsigned int new_btc_mark                   :1;
  unsigned int reset_limits                   :1;
  unsigned int reset_required                 :1;
  unsigned int kill_propagation_loop          :1;
  unsigned int current_mark_owner;
  OTEntry **owner_table;
  unsigned int owner_table_size;
  AccountHook *hooks;


  unsigned long number_of_gc_runs;
  unsigned int since_last_full;
  unsigned long last_full_mem_use;

  /* These collect information about memory usage, for use in GC_dump. */
  unsigned long peak_memory_use;
  unsigned long num_minor_collects;
  unsigned long num_major_collects;
  
  /* THREAD_LOCAL variables that need to be saved off */
  MarkSegment  *saved_mark_stack;
  void         *saved_GC_variable_stack;
  unsigned long saved_GC_gen0_alloc_page_ptr;
  unsigned long saved_GC_gen0_alloc_page_end;
  /* Distributed GC over places info */
#ifdef MZ_USE_PLACES
  int           place_id;
  int           major_places_gc;   /* :1; */
  int           dont_master_gc_until_child_registers;   /* :1: */
#endif

 struct mpage *thread_local_pages;

  /* Callbacks */
  void (*GC_collect_start_callback)(void);
  void (*GC_collect_end_callback)(void);
  void (*GC_collect_inform_callback)(int major_gc, long pre_used, long post_used);
  unsigned long (*GC_get_thread_stack_base)(void);

  GC_Immobile_Box *immobile_boxes;

  /* Common with CompactGC */
  Fnl *finalizers;
  Fnl *splayed_finalizers;
  int num_fnls;

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
  struct MMU     *mmu;

#if defined(GC_DEBUG_PAGES)
  FILE *GCVERBOSEFH;
#endif

} NewGC;
