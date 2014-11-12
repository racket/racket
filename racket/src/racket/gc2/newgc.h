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
  uintptr_t previous_size; /* for med page, place to search for available block; for jit nursery, allocated size */
  uintptr_t size; /* big page size, med page element size, or nursery starting point */
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
  uintptr_t current_size;
  uintptr_t max_size;
  uintptr_t page_alloc_size;
} Gen0;

typedef struct MsgMemory {
  struct mpage *pages;
  struct mpage *big_pages;
  uintptr_t size;
} MsgMemory;

typedef struct Allocator {
  Gen0 savedGen0;
  uintptr_t saved_alloc_page_ptr;
  uintptr_t saved_alloc_page_end;
} Allocator;

typedef struct MarkSegment {
  struct MarkSegment *prev;
  struct MarkSegment *next;
  void **top;
} MarkSegment;

typedef struct GC_Thread_Info {
  void *thread;
  int owner;
  struct GC_Thread_Info *next;
} GC_Thread_Info;

typedef struct AccountHook {
  int type;
  void *c1;
  void *c2;
  uintptr_t amount;
  struct AccountHook *next;
} AccountHook;

typedef struct OTEntry {
  void *originator;
  void **members;
  uintptr_t memory_use;
  uintptr_t master_memory_use;
  uintptr_t single_time_limit;
  uintptr_t super_required;
  char limit_set;
  char required_set;
} OTEntry;

typedef struct Range {
  uintptr_t start, len;
  struct Range *left, *right, *prev, *next;
} Range;

typedef struct Page_Range {
  Range *range_root;
  Range *range_start;
  void *range_alloc_block;
  uintptr_t range_alloc_size;
  uintptr_t range_alloc_used;
} Page_Range;

#ifdef MZ_USE_PLACES
typedef struct NewGCMasterInfo {
  uintptr_t size;
  uintptr_t alive;
  uintptr_t ready;
  void **signal_fds;
  mzrt_rwlock *cangc;
  mzrt_sema *wait_go_sema;
  mzrt_sema *wait_done_sema;
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

  struct mpage *med_pages[MED_PAGE_TYPES][NUM_MED_PAGE_SIZES];
  struct mpage *med_freelist_pages[MED_PAGE_TYPES][NUM_MED_PAGE_SIZES];

  MarkSegment *mark_stack;

  /* Finalization */
  Fnl *run_queue;
  Fnl *last_in_queue;

  struct NewGC *primoridal_gc;
  uintptr_t max_heap_size;
  uintptr_t max_pages_in_heap;
  uintptr_t max_pages_for_use;
  uintptr_t used_pages;
  uintptr_t actual_pages_size;
  void (*unsafe_allocation_abort)(struct NewGC *);
  uintptr_t memory_in_use; /* the amount of memory in use */
  uintptr_t pending_msg_size; /* set in master, only */
  uintptr_t prev_pending_msg_size; /* set in master, only */

  /* blame the child thread infos */
  GC_Thread_Info *thread_infos;

  mpage *release_pages;
  uintptr_t stack_base;
  int avoid_collection;

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

  uintptr_t number_of_gc_runs;
  unsigned int since_last_full;
  uintptr_t last_full_mem_use;

  /* These collect information about memory usage, for use in GC_dump. */
  uintptr_t peak_memory_use;
  uintptr_t num_minor_collects;
  uintptr_t num_major_collects;
  
  /* THREAD_LOCAL variables that need to be saved off */
  MarkSegment  *saved_mark_stack;
  void         *saved_GC_variable_stack;
  uintptr_t saved_GC_gen0_alloc_page_ptr;
  uintptr_t saved_GC_gen0_alloc_page_end;
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
  GC_collect_inform_callback_Proc GC_collect_inform_callback;
  uintptr_t (*GC_get_thread_stack_base)(void);
  GC_Post_Propagate_Hook_Proc GC_post_propagate_hook;

  GC_Immobile_Box *immobile_boxes;

  /* Common with CompactGC */
  Fnl *finalizers;
  Fnl *splayed_finalizers;
  int num_fnls;

  void *park[2];
  void *park_fsave[2];
  void *park_isave[2];

  unsigned short weak_array_tag;
  unsigned short weak_box_tag;
  unsigned short ephemeron_tag;
  unsigned short cust_box_tag;
  unsigned short phantom_tag;

  uintptr_t phantom_count;

  Roots roots;
  GC_Weak_Array *weak_arrays;
  GC_Weak_Box   *weak_boxes[2];
  GC_Ephemeron  *ephemerons;
  int num_last_seen_ephemerons;
  struct MMU     *mmu;

  Allocator *saved_allocator;

#ifdef MZ_USE_PLACES
  struct NewGC *parent_gc; /* parent for the purpose of reporting memory use */
  intptr_t previously_reported_total; /* how much we previously reported to the parent */
  mzrt_mutex *child_total_lock; /* lock on `child_gc_total' */
#endif
  uintptr_t child_gc_total;

  uintptr_t place_memory_limit; /* set to propagate a custodian limit from a parent place */  

#if MZ_GC_BACKTRACE
  void *bt_source;
  int bt_type;
#endif

#if defined(GC_DEBUG_PAGES)
  FILE *GCVERBOSEFH;
#endif

} NewGC;
