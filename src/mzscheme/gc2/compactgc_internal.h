#include "commongc_internal.h"

typedef struct CompactGC {
 /* Common with NewGC */
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
  Free_Block    *blockfree;
} CompactGC;

void CompactGC_initialize(CompactGC *gc) {
  memset(gc, 0, sizeof(CompactGC));
  gc->blockfree = malloc(sizeof(Free_Block) *BLOCKFREE_CACHE_SIZE);
}
