
/* 
   Provides:
      GC_malloc_weak_array
      size_weak_array, mark_weak_array, fixup_weak_array
      init_weak_arrays zero_weak_arrays
      GC_malloc_weak_box
      size_weak_box, mark_weak_box, fixup_weak_box
      init_weak_boxes zero_weak_boxes
      GC_malloc_ephemeron
      size_ephemeron, mark_ephemeron, fixup_ephemeron
      BTC_ephemeron_mark [ifdef NEW_BTC_ACCOUNT]
      init_ephemerons mark_ready_ephemerons zero_remaining_ephemerons
      num_last_seen_ephemerons
   Requires:
      weak_array_tag
      weak_box_tag
      ephemeron_tag
      is_marked(p)
      is_in_generation_half(p)
      Type_Tag
*/

#define WEAK_INCREMENTAL_DONE_1 ((void *)0x1)
#define WEAK_INCREMENTAL_DONE_2 ((void *)0x3)

/******************************************************************************/
/*                               weak arrays                                  */
/******************************************************************************/

static int size_weak_array(void *p, struct NewGC *gc)
{
  GC_Weak_Array *a = (GC_Weak_Array *)p;

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Array) 
			  + ((a->count - 1 + 1) * sizeof(void *)));
}

static int mark_weak_array(void *p, struct NewGC *gc)
{
  GC_Weak_Array *a = (GC_Weak_Array *)p;

  gcMARK2(a->replace_val, gc);

  if (gc->doing_memory_accounting) {
    /* skip */
  } else if (gc->inc_gen1) {
    /* inc_next field is at the end of the `data` array: */
    a->data[a->count] = gc->inc_weak_arrays;
    gc->inc_weak_arrays = a;
  } else if (gc->during_backpointer) {
    if (!gc->gc_full
        || (gc->started_incremental
            /* `a` must have been marked and must be in the old
               generation, or we wouldn't get here; `a` may have been
               fully processed in incremental mode, though */
            && (a->data[a->count] == gc->weak_incremental_done))) {
      /* Keep backpointered weak arrays separate, because we
         should not merge them to the incremental list
         in incremental mode. */
      a->next = gc->bp_weak_arrays;
      gc->bp_weak_arrays = a;
    }
  } else {
    a->next = gc->weak_arrays;
    gc->weak_arrays = a;
    if (gc->gc_full)
      a->data[a->count] = NULL; /* ensure not a future weak_incremental_done */
  }

#if CHECKS
  /* For now, weak arrays only used for symbols, keywords, and falses: */
  {
    void **data;
    int i;
    data = a->data;
    for (i = a->count; i--; ) {
      if (data[i] 
	  && (*(short *)(data[i]) != 48)
	  && (*(short *)(data[i]) != 49)
	  && (*(short *)(data[i]) != 58)) {
	CRASH(1);
      }
    }
  }
#endif

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Array) 
			  + ((a->count - 1 + 1) * sizeof(void *)));
}

static int fixup_weak_array(void *p, struct NewGC *gc)
{
  GC_Weak_Array *a = (GC_Weak_Array *)p;
  int i;
  void **data;

  gcFIXUP2(a->replace_val, gc);

  data = a->data;
  for (i = a->count; i--; ) {
    if (data[i]) {
      gcFIXUP2(data[i], gc);
    }
  }

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Array) 
			  + ((a->count - 1) * sizeof(void *)));
}

void *GC_malloc_weak_array(size_t size_in_bytes, void *replace_val)
{
  GCTYPE *gc = GC_get_GC();
  GC_Weak_Array *w;

  /* Allcation might trigger GC, so we use park: */
  CHECK_PARK_UNUSED(gc);
  gc->park[0] = replace_val;

  w = (GC_Weak_Array *)GC_malloc_one_tagged(size_in_bytes 
					    + sizeof(GC_Weak_Array) 
					    - sizeof(void *)
					    + sizeof(GC_Weak_Array *));

  replace_val = gc->park[0];
  gc->park[0] = NULL;

  w->type = gc->weak_array_tag;
  w->replace_val = replace_val;
  w->count = (size_in_bytes >> LOG_WORD_SIZE);
  
  return w;
}

static void init_weak_arrays(GCTYPE *gc)
{
  GC_ASSERT(!gc->bp_weak_arrays);
  gc->weak_arrays = NULL;
}

static GC_Weak_Array *append_weak_arrays(GC_Weak_Array *wa, GC_Weak_Array *bp_wa, int *_num_gen0)
{
  *_num_gen0 = 0;

  if (wa) {
    GC_Weak_Array *last_wa = wa;
    while (last_wa->next) {
      (*_num_gen0)++;
      last_wa = last_wa->next;
    }
    (*_num_gen0)++;
    last_wa->next = bp_wa;
    return wa;
  } else
    return bp_wa;
}

static int zero_weak_arrays(GCTYPE *gc, int force_zero, int from_inc, int need_resolve, int fuel)
{
  GC_Weak_Array *wa;
  int i, num_gen0;

  if (!fuel) return 0;

  if (from_inc) {
    wa = gc->inc_weak_arrays;
    num_gen0 = 0;
  } else
    wa = append_weak_arrays(gc->weak_arrays, gc->bp_weak_arrays, &num_gen0);

  if (gc->gc_full || !gc->started_incremental)
    num_gen0 = 0;

  while (wa) {
    void **data;

    data = wa->data;
    for (i = wa->count; i--; ) {
      void *p = data[i];
      if (p && (force_zero || !is_marked(gc, p)))
        data[i] = wa->replace_val;
      else if (need_resolve)
        data[i] = GC_resolve2(p, gc);
    }
    if (fuel > 0) {
      fuel -= (4 * wa->count);
      if (fuel < 0) fuel = 0;
    }

    if (num_gen0 > 0) {
      if (!is_in_generation_half(gc, wa)) {
        if (!gc->all_marked_incremental) {
          /* For incremental mode, preserve this weak array
             in the incremental list for re-checking later. */
          wa->data[wa->count] = gc->inc_weak_arrays;
          gc->inc_weak_arrays = wa;
        } else {
          /* Count as incremental-done: */
          wa->data[wa->count] = gc->weak_incremental_done;
        }
      }
    }

    if (from_inc) {
      GC_Weak_Array *next;
      next = (GC_Weak_Array *)wa->data[wa->count];
      wa->data[wa->count] = gc->weak_incremental_done;
      wa = next;
    } else
      wa = wa->next;
    num_gen0--;
  }
  if (from_inc)
    gc->inc_weak_arrays = NULL;
  else {
    gc->weak_arrays = NULL;
    gc->bp_weak_arrays = NULL;
  }

  return fuel;
}

/******************************************************************************/
/*                                weak boxes                                  */
/******************************************************************************/

#if 0
static void check_weak_box_not_already_in_inc_chain(GC_Weak_Box *wb, GC_Weak_Box *wbc)
{
  while (wbc) {
    GC_ASSERT(wb != wbc);
    wbc = wbc->inc_next;
  }
}
static void check_weak_box_not_already_in_chain(GC_Weak_Box *wb, GC_Weak_Box *wbc)
{
  while (wbc) {
    GC_ASSERT(wb != wbc);
    wbc = wbc->next;
  }
}
#else
static void check_weak_box_not_already_in_inc_chain(GC_Weak_Box *wb, GC_Weak_Box *wbc) { }
static void check_weak_box_not_already_in_chain(GC_Weak_Box *wb, GC_Weak_Box *wbc) { }
#endif

static int size_weak_box(void *p, struct NewGC *gc)
{
  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Box));
}

static int mark_weak_box(void *p, struct NewGC *gc)
{
  GC_Weak_Box *wb = (GC_Weak_Box *)p;

  gcMARK2(wb->secondary_erase, gc);

  if (gc->doing_memory_accounting) {
    /* skip */
  } else if (gc->inc_gen1) {
    check_weak_box_not_already_in_inc_chain(wb, gc->inc_weak_boxes[wb->is_late]);
    wb->inc_next = gc->inc_weak_boxes[wb->is_late];
    gc->inc_weak_boxes[wb->is_late] = wb;
  } else if (gc->during_backpointer) {
    if ((!gc->gc_full
         || (gc->started_incremental
             /* see note with `gc->weak_incremental_done` for weak arrays */
             && (wb->inc_next == gc->weak_incremental_done)
             && wb->val))
        && (wb->val || gc->started_incremental)) {
      /* Keep backpointered weak arrays separate, because we
         should not merge them to the incremental list
         in incremental mode. */
      check_weak_box_not_already_in_chain(wb, gc->bp_weak_boxes[wb->is_late]);
      check_weak_box_not_already_in_chain(wb, gc->weak_boxes[wb->is_late]);
      wb->next = gc->bp_weak_boxes[wb->is_late];
      gc->bp_weak_boxes[wb->is_late] = wb;
    }
  } else if (wb->val || gc->started_incremental) {
    check_weak_box_not_already_in_chain(wb, gc->weak_boxes[wb->is_late]);
    check_weak_box_not_already_in_chain(wb, gc->bp_weak_boxes[wb->is_late]);
    wb->next = gc->weak_boxes[wb->is_late];
    gc->weak_boxes[wb->is_late] = wb;
    if (gc->gc_full)
      wb->inc_next = NULL; /* ensure not a future weak_incremental_done */
  }

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Box));
}

static int fixup_weak_box(void *p, struct NewGC *gc)
{
  GC_Weak_Box *wb = (GC_Weak_Box *)p;
    
  gcFIXUP2(wb->secondary_erase, gc);
  gcFIXUP2(wb->val, gc);

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Box));
}

void *GC_malloc_weak_box(void *p, void **secondary, int soffset, int is_late)
{
  GCTYPE *gc = GC_get_GC();
  GC_Weak_Box *w;

  /* Allcation might trigger GC, so we use park: */
  CHECK_PARK_UNUSED(gc);
  gc->park[0] = p;
  gc->park[1] = secondary;

  w = (GC_Weak_Box *)GC_malloc_one_tagged(sizeof(GC_Weak_Box));

  /* Future-local allocation may fail: */
  if (!w) return NULL;

  p = gc->park[0];
  secondary = (void **)gc->park[1];
  gc->park[0] = NULL;
  gc->park[1] = NULL;
  
  w->type = gc->weak_box_tag;
  w->val = p;
  w->secondary_erase = secondary;
  w->is_late = is_late;
  w->soffset = soffset;

  return w;
}

static void init_weak_boxes(GCTYPE *gc)
{
  GC_ASSERT(!gc->bp_weak_boxes[0]);
  GC_ASSERT(!gc->bp_weak_boxes[1]);
  gc->weak_boxes[0] = NULL;
  gc->weak_boxes[1] = NULL;
}

static GC_Weak_Box *append_weak_boxes(GC_Weak_Box *wb, GC_Weak_Box *bp_wb, int *_num_gen0)
{
  *_num_gen0 = 0;

  if (wb) {
    GC_Weak_Box *last_wb = wb;
    while (last_wb->next) {
      (*_num_gen0)++;
      last_wb = last_wb->next;
    }
    (*_num_gen0)++;
    last_wb->next = bp_wb;
    return wb;
  } else
    return bp_wb;
}

static int zero_weak_boxes(GCTYPE *gc, int is_late, int force_zero, int from_inc, int need_resolve, int fuel)
{
  GC_Weak_Box *wb;
  int num_gen0;

  if (!fuel) return 0;

  if (from_inc) {
    wb = gc->inc_weak_boxes[is_late];
    num_gen0 = 0;
  } else {
    wb = append_weak_boxes(gc->weak_boxes[is_late],
                           gc->bp_weak_boxes[is_late],
                           &num_gen0);
    if (gc->gc_full || !gc->started_incremental)
      num_gen0 = 0;
  }

  while (wb) {
    GC_ASSERT(is_marked(gc, wb));
    if (!wb->val) {
      /* nothing to do */
    } else if (force_zero || !is_marked(gc, wb->val)) {
      wb->val = NULL;
      if (wb->secondary_erase) {
        void **p;
        mpage *page;

        /* it's possible for the secondary to be in an old generation
           and therefore on an mprotected page: */
        page = pagemap_find_page(gc->page_maps, wb->secondary_erase);
        if (page->mprotected) {
          page->mprotected = 0;
          mmu_write_unprotect_page(gc->mmu, page->addr, APAGE_SIZE, page_mmu_type(page), &page->mmu_src_block);
          page->reprotect_next = gc->reprotect_next;
          gc->reprotect_next = page;
          page->reprotect = 1;
        }
        p = (void **)GC_resolve2(wb->secondary_erase, gc);
        *(p + wb->soffset) = NULL;
        wb->secondary_erase = NULL;
      }
    } else if (need_resolve)
      wb->val = GC_resolve2(wb->val, gc);

    if (num_gen0 > 0) {
      if (!is_in_generation_half(gc, wb)) {
        if (!gc->all_marked_incremental) {
          /* For incremental mode, preserve this weak box
             in the incremental list for re-checking later. */
          check_weak_box_not_already_in_inc_chain(wb, gc->inc_weak_boxes[wb->is_late]);
          wb->inc_next = gc->inc_weak_boxes[is_late];
          gc->inc_weak_boxes[is_late] = wb;
        } else {
          /* Count as incremental-done: */
          wb->inc_next = gc->weak_incremental_done;
        }
      }
    }

    if (from_inc) {
      GC_Weak_Box *next;
      next = wb->inc_next;
      wb->inc_next = gc->weak_incremental_done;
      wb = next;
    } else
      wb = wb->next;

    num_gen0--;

    if (fuel >= 0) {
      if (fuel > 0) {
        if (gc->unprotected_page) {
          fuel -= 100;
          gc->unprotected_page = 0;
        } else
          fuel -= 4;
        if (fuel < 0) fuel = 0;
      } else {
        GC_ASSERT(from_inc);
        gc->inc_weak_boxes[is_late] = wb;
        return 0;
      }
    }
  }

  /* reset, in case we have a second round */
  if (from_inc) {
    gc->inc_weak_boxes[is_late] = NULL;
  } else {
    gc->weak_boxes[is_late] = NULL;
    gc->bp_weak_boxes[is_late] = NULL;
  }

  return fuel;
}

/******************************************************************************/
/*                                 ephemeron                                  */
/******************************************************************************/

static int size_ephemeron(void *p, struct NewGC *gc)
{
  return gcBYTES_TO_WORDS(sizeof(GC_Ephemeron));
}

static int mark_ephemeron(void *p, struct NewGC *gc)
{
  GC_Ephemeron *eph = (GC_Ephemeron *)p;

  if (eph->val) {
    GC_ASSERT(!gc->doing_memory_accounting);
    if (gc->inc_gen1) {
      eph->inc_next = gc->inc_ephemerons;
      gc->inc_ephemerons = eph;
    } else if (gc->during_backpointer) {
      if (!gc->gc_full
          /* If this old-generation object is not yet marked
             and we're finishing an incremental pass, then
             it won't get marked (and it can only refer to
             other old-generation objects), so ignore in that case */
          && (gc->mark_gen1
              || !gc->started_incremental
              || !gc->all_marked_incremental)) {
        eph->next = gc->bp_ephemerons;
        gc->bp_ephemerons = eph;
      }
    } else {
      eph->next = gc->ephemerons;
      gc->ephemerons = eph;
    }
  }

  return gcBYTES_TO_WORDS(sizeof(GC_Ephemeron));
}

#ifdef NEWGC_BTC_ACCOUNT
static int BTC_ephemeron_mark(void *p, struct NewGC *gc)
{
  if (gc->doing_memory_accounting) {

    GC_Ephemeron *eph = (GC_Ephemeron *)p;

    gcMARK2(eph->key, gc);
    gcMARK2(eph->val, gc);

    return gcBYTES_TO_WORDS(sizeof(GC_Ephemeron));
  }
  return mark_ephemeron(p, gc);
}
#endif


static int fixup_ephemeron(void *p, struct NewGC *gc)
{
  GC_Ephemeron *eph = (GC_Ephemeron *)p;
    
  gcFIXUP2(eph->key, gc);
  gcFIXUP2(eph->val, gc);

  return gcBYTES_TO_WORDS(sizeof(GC_Ephemeron));
}

void *GC_malloc_ephemeron(void *k, void *v)
{
  GCTYPE *gc = GC_get_GC();
  GC_Ephemeron *eph;

  /* Allcation might trigger GC, so we use park: */
  CHECK_PARK_UNUSED(gc);
  gc->park[0] = k;
  gc->park[1] = v;

  eph = (GC_Ephemeron *)GC_malloc_one_tagged(sizeof(GC_Ephemeron));

  k = gc->park[0];
  v = gc->park[1];
  gc->park[0] = NULL;
  gc->park[1] = NULL;
  
  eph->type = gc->ephemeron_tag;
  eph->key = k;
  eph->val = v;

  return eph;
}

void init_ephemerons(GCTYPE *gc) {
  GC_ASSERT(!gc->bp_ephemerons);
  gc->ephemerons = NULL;
  gc->bp_ephemerons = NULL;
  gc->num_last_seen_ephemerons = 0;
}

static int mark_ready_ephemerons(GCTYPE *gc, int inc_gen1)
{
  GC_Ephemeron *waiting, *next, *eph;
  int did_one = 0, j;

  GC_mark_no_recur(gc, 1);

  for (j = 0; j < (inc_gen1 ? 1 : (gc->gc_full ? 3 : 2)); j++) {
    waiting = NULL;

    if (inc_gen1)
      eph = gc->inc_ephemerons;
    else if (j == 0)
      eph = gc->ephemerons;
    else if (j == 1)
      eph = gc->bp_ephemerons;
    else {
      eph = gc->inc_ephemerons;
      gc->inc_ephemerons = NULL;
      waiting = gc->ephemerons;
    }
    
    for (; eph; eph = next) {
      if (inc_gen1 || (j == 2))
        next = eph->inc_next;
      else
        next = eph->next;
      if (is_marked(gc, eph->key)) {
        if (!inc_gen1)
          eph->key = GC_resolve2(eph->key, gc);
        gcMARK2(eph->val, gc);
        gc->num_last_seen_ephemerons++;
        did_one = 1;
        if (!inc_gen1 && (j == 0) && !gc->gc_full
            && gc->started_incremental && !gc->all_marked_incremental) {
          /* Need to preserve the ephemeron in the incremental list,
             unless it's kept in generation 1/2 instead of promoted to
             generation 1. */
          if (!is_in_generation_half(gc, eph)) {
            eph->inc_next = gc->inc_ephemerons;
            gc->inc_ephemerons = eph;
          }
        }
      } else {
        if (inc_gen1) {
          /* Ensure that we can write to the page containing the emphemeron: */
          check_incremental_unprotect(gc, pagemap_find_page(gc->page_maps, eph));
          eph->inc_next = waiting;
        } else
          eph->next = waiting;
        waiting = eph;
      }
    }

    if (inc_gen1)
      gc->inc_ephemerons = waiting;
    else if ((j == 0)|| (j == 2))
      gc->ephemerons = waiting;
    else
      gc->bp_ephemerons = waiting;
  }

  GC_mark_no_recur(gc, 0);

  return did_one;
}

static void zero_remaining_ephemerons(GCTYPE *gc, int from_inc)
{
  GC_Ephemeron *eph;

  GC_ASSERT(from_inc || !gc->gc_full || !gc->inc_ephemerons);

  /* After level-1 finalization, any remaining ephemerons
     should be zeroed. */
  if (from_inc) {
    for (eph = gc->inc_ephemerons; eph; eph = eph->inc_next) {
      eph->key = NULL;
      eph->val = NULL;
    }
    gc->inc_ephemerons = NULL;
  } else {
    for (eph = gc->ephemerons; eph; eph = eph->next) {
      eph->key = NULL;
      eph->val = NULL;
    }
    gc->ephemerons = NULL;
  }
}
