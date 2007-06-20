
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
      btc_mark_ephemeron [ifdef NEW_BTC_ACCOUNT]
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

/* The GC_Weak_Array structure is not externally visible, but
   clients expect a specific structure. See README for more
   information. */
typedef struct GC_Weak_Array {
  Type_Tag type;
  short keyex;
  long count;
  void *replace_val;
  struct GC_Weak_Array *next;
  void *data[1]; /* must be the 5th longword! */
} GC_Weak_Array;

static GC_Weak_Array *weak_arrays;

static int size_weak_array(void *p)
{
  GC_Weak_Array *a = (GC_Weak_Array *)p;

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Array) 
			  + ((a->count - 1) * sizeof(void *)));
}

static int mark_weak_array(void *p)
{
  GC_Weak_Array *a = (GC_Weak_Array *)p;

  gcMARK(a->replace_val);

  a->next = weak_arrays;
  weak_arrays = a;

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

static int fixup_weak_array(void *p)
{
  GC_Weak_Array *a = (GC_Weak_Array *)p;
  int i;
  void **data;

  gcFIXUP(a->replace_val);

  data = a->data;
  for (i = a->count; i--; ) {
    if (data[i])
      gcFIXUP(data[i]);
  }

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Array) 
			  + ((a->count - 1) * sizeof(void *)));
}

void *GC_malloc_weak_array(size_t size_in_bytes, void *replace_val)
{
  GC_Weak_Array *w;

  /* Allcation might trigger GC, so we use park: */
  park[0] = replace_val;

  w = (GC_Weak_Array *)GC_malloc_one_tagged(size_in_bytes 
					    + sizeof(GC_Weak_Array) 
					    - sizeof(void *));

  replace_val = park[0];
  park[0] = NULL;

  w->type = weak_array_tag;
  w->replace_val = replace_val;
  w->count = (size_in_bytes >> LOG_WORD_SIZE);
  
  return w;
}

void init_weak_arrays() {
  weak_arrays = NULL;
}

static void zero_weak_arrays()
{
  GC_Weak_Array *wa;
  int i;

  wa = weak_arrays;
  while (wa) {
    void **data;
    
    data = wa->data;
    for (i = wa->count; i--; ) {
      void *p = data[i];
      if (p && !is_marked(p))
	data[i] = wa->replace_val;
    }
    
    wa = wa->next;
  }
}

/******************************************************************************/
/*                                weak boxes                                  */
/******************************************************************************/

/* The GC_Weak_Box struct is not externally visible, but
   first three fields are mandated by the GC interface */
typedef struct GC_Weak_Box {
  Type_Tag type;
  short keyex;
  void *val;
  /* The rest is up to us: */
  void **secondary_erase;
  int soffset;
  struct GC_Weak_Box *next;
} GC_Weak_Box;

static GC_Weak_Box *weak_boxes;

static int size_weak_box(void *p)
{
  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Box));
}

static int mark_weak_box(void *p)
{
  GC_Weak_Box *wb = (GC_Weak_Box *)p;
    
  gcMARK(wb->secondary_erase);

  if (wb->val) {
    wb->next = weak_boxes;
    weak_boxes = wb;
  }

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Box));
}

static int fixup_weak_box(void *p)
{
  GC_Weak_Box *wb = (GC_Weak_Box *)p;
    
  gcFIXUP(wb->secondary_erase);
  gcFIXUP(wb->val);

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Box));
}

void *GC_malloc_weak_box(void *p, void **secondary, int soffset)
{
  GC_Weak_Box *w;

  /* Allcation might trigger GC, so we use park: */
  park[0] = p;
  park[1] = secondary;

  w = (GC_Weak_Box *)GC_malloc_one_tagged(sizeof(GC_Weak_Box));

  p = park[0];
  park[0] = NULL;
  secondary = (void **)park[1];
  park[1] = NULL;
  
  w->type = weak_box_tag;
  w->val = p;
  w->secondary_erase = secondary;
  w->soffset = soffset;

  return w;
}

void init_weak_boxes() {
  weak_boxes = NULL;
}

static void zero_weak_boxes()
{
  GC_Weak_Box *wb;

  wb = weak_boxes;
  while (wb) {
    if (!is_marked(wb->val)) {
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

/* The GC_Ephemeron struct is not externally visible, but
   first three fields are mandated by the GC interface */
typedef struct GC_Ephemeron {
  Type_Tag type;
  short keyex;
  void *key;
  void *val;
  /* The rest is up to us: */
  struct GC_Ephemeron *next;
} GC_Ephemeron;

static GC_Ephemeron *ephemerons;

static int num_last_seen_ephemerons = 0;

static int size_ephemeron(void *p)
{
  return gcBYTES_TO_WORDS(sizeof(GC_Ephemeron));
}

static int mark_ephemeron(void *p)
{
  GC_Ephemeron *eph = (GC_Ephemeron *)p;

  if (eph->val) {
    eph->next = ephemerons;
    ephemerons = eph;
  }

  return gcBYTES_TO_WORDS(sizeof(GC_Ephemeron));
}

#ifdef NEWGC_BTC_ACCOUNT
static int btc_mark_ephemeron(void *p)
{
  GC_Ephemeron *eph = (GC_Ephemeron *)p;
  
  gcMARK(eph->key);
  gcMARK(eph->val);

  return gcBYTES_TO_WORDS(sizeof(GC_Ephemeron));
}
#endif


static int fixup_ephemeron(void *p)
{
  GC_Ephemeron *eph = (GC_Ephemeron *)p;
    
  gcFIXUP(eph->key);
  gcFIXUP(eph->val);

  return gcBYTES_TO_WORDS(sizeof(GC_Ephemeron));
}

void *GC_malloc_ephemeron(void *k, void *v)
{
  GC_Ephemeron *eph;

  /* Allcation might trigger GC, so we use park: */
  park[0] = k;
  park[1] = v;

  eph = (GC_Ephemeron *)GC_malloc_one_tagged(sizeof(GC_Ephemeron));

  k = park[0];
  park[0] = NULL;
  v = park[1];
  park[1] = NULL;
  
  eph->type = ephemeron_tag;
  eph->key = k;
  eph->val = v;

  return eph;
}

void init_ephemerons() {
  ephemerons = NULL;
  num_last_seen_ephemerons = 0;
}

static void mark_ready_ephemerons()
{
  GC_Ephemeron *waiting = NULL, *next, *eph;

  for (eph = ephemerons; eph; eph = next) {
    next = eph->next;
    if (is_marked(eph->key)) {
      gcMARK(eph->val);
      num_last_seen_ephemerons++;
    } else {
      eph->next = waiting;
      waiting = eph;
    }
  }
  ephemerons = waiting;
}

static void zero_remaining_ephemerons()
{
  GC_Ephemeron *eph;

  /* After unordered finalization, any remaining ephemerons
     should be zeroed. */
  for (eph = ephemerons; eph; eph = eph->next) {
    eph->key = NULL;
    eph->val = NULL;
  }
}
