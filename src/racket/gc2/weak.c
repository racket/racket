
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
      Type_Tag
*/


/******************************************************************************/
/*                               weak arrays                                  */
/******************************************************************************/

static int size_weak_array(void *p, struct NewGC *gc)
{
  GC_Weak_Array *a = (GC_Weak_Array *)p;

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Array) 
			  + ((a->count - 1) * sizeof(void *)));
}

static int mark_weak_array(void *p, struct NewGC *gc)
{
  GC_Weak_Array *a = (GC_Weak_Array *)p;

  gcMARK2(a->replace_val, gc);

  a->next = gc->weak_arrays;
  gc->weak_arrays = a;

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
			  + ((a->count - 1) * sizeof(void *)));
}

static int fixup_weak_array(void *p, struct NewGC *gc)
{
  GC_Weak_Array *a = (GC_Weak_Array *)p;
  int i;
  void **data;

  gcFIXUP2(a->replace_val, gc);

  data = a->data;
  for (i = a->count; i--; ) {
    if (data[i])
      gcFIXUP2(data[i], gc);
  }

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Array) 
			  + ((a->count - 1) * sizeof(void *)));
}

void *GC_malloc_weak_array(size_t size_in_bytes, void *replace_val)
{
  GCTYPE *gc = GC_get_GC();
  GC_Weak_Array *w;

  /* Allcation might trigger GC, so we use park: */
  gc->park[0] = replace_val;

  w = (GC_Weak_Array *)GC_malloc_one_tagged(size_in_bytes 
					    + sizeof(GC_Weak_Array) 
					    - sizeof(void *));

  replace_val = gc->park[0];
  gc->park[0] = NULL;

  w->type = gc->weak_array_tag;
  w->replace_val = replace_val;
  w->count = (size_in_bytes >> LOG_WORD_SIZE);
  
  return w;
}

static void init_weak_arrays(GCTYPE *gc) {
  gc->weak_arrays = NULL;
}

static void zero_weak_arrays(GCTYPE *gc)
{
  GC_Weak_Array *wa;
  int i;

  wa = gc->weak_arrays;
  while (wa) {
    void **data;

    data = wa->data;
    for (i = wa->count; i--; ) {
      void *p = data[i];
      if (p && !is_marked(gc, p))
        data[i] = wa->replace_val;
    }

    wa = wa->next;
  }
}

/******************************************************************************/
/*                                weak boxes                                  */
/******************************************************************************/

static int size_weak_box(void *p, struct NewGC *gc)
{
  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Box));
}

static int mark_weak_box(void *p, struct NewGC *gc)
{
  GC_Weak_Box *wb = (GC_Weak_Box *)p;
    
  gcMARK2(wb->secondary_erase, gc);

  if (wb->val) {
    wb->next = gc->weak_boxes;
    gc->weak_boxes = wb;
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

void *GC_malloc_weak_box(void *p, void **secondary, int soffset)
{
  GCTYPE *gc = GC_get_GC();
  GC_Weak_Box *w;

  /* Allcation might trigger GC, so we use park: */
  gc->park[0] = p;
  gc->park[1] = secondary;

  w = (GC_Weak_Box *)GC_malloc_one_tagged(sizeof(GC_Weak_Box));

  p = gc->park[0];
  secondary = (void **)gc->park[1];
  gc->park[0] = NULL;
  gc->park[1] = NULL;
  
  w->type = gc->weak_box_tag;
  w->val = p;
  w->secondary_erase = secondary;
  w->soffset = soffset;

  return w;
}

static void init_weak_boxes(GCTYPE *gc) {
  gc->weak_boxes = NULL;
}

static void zero_weak_boxes(GCTYPE *gc)
{
  GC_Weak_Box *wb;

  wb = gc->weak_boxes;
  while (wb) {
    if (!is_marked(gc, wb->val)) {
      wb->val = NULL;
      if (wb->secondary_erase) {
        void **p;
        p = (void **)GC_resolve(wb->secondary_erase);
        *(p + wb->soffset) = NULL;
        wb->secondary_erase = NULL;
      }
    }
    wb = wb->next;
  }
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
    eph->next = gc->ephemerons;
    gc->ephemerons = eph;
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
  gc->ephemerons = NULL;
  gc->num_last_seen_ephemerons = 0;
}

static void mark_ready_ephemerons(GCTYPE *gc)
{
  GC_Ephemeron *waiting = NULL, *next, *eph;

  for (eph = gc->ephemerons; eph; eph = next) {
    next = eph->next;
    if (is_marked(gc, eph->key)) {
      gcMARK2(eph->val, gc);
      gc->num_last_seen_ephemerons++;
    } else {
      eph->next = waiting;
      waiting = eph;
    }
  }
  gc->ephemerons = waiting;
}

static void zero_remaining_ephemerons(GCTYPE *gc)
{
  GC_Ephemeron *eph;

  /* After unordered finalization, any remaining ephemerons
     should be zeroed. */
  for (eph = gc->ephemerons; eph; eph = eph->next) {
    eph->key = NULL;
    eph->val = NULL;
  }
}
