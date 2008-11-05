#include "commongc_internal.h"

typedef struct Gen0 {
 struct mpage *curr_alloc_page;
 struct mpage *pages;
 struct mpage *big_pages;
 unsigned long GC_gen0_alloc_page_ptr;
 unsigned long current_size;
 unsigned long max_size;
} Gen0;

typedef struct NewGC {
  Gen0 gen0;
  struct NewGC *primoridal_gc;
  unsigned long max_heap_size;
  unsigned long max_pages_in_heap;
  unsigned long max_pages_for_use;
  unsigned long used_pages;
  unsigned long actual_pages_size;
  unsigned long in_unsafe_allocation_mode :1;
  void (*unsafe_allocation_abort)();

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
}
