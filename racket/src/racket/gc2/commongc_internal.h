typedef short Type_Tag;

typedef struct Roots {
  intptr_t count;
  intptr_t size;
  /* roots is a array of longs, logically grouped into start and end pairs.
   * [ start0, end0, start1, end1, ... ]
   */ 
  uintptr_t *roots;
  int nothing_new;
} Roots;

/* The GC_Weak_Array structure is not externally visible, but
   clients expect a specific structure. See README for more
   information. */
typedef struct GC_Weak_Array {
  Type_Tag type;
  short keyex;
  intptr_t count;
  void *replace_val;
  struct GC_Weak_Array *next;
  void *data[1]; /* must be the 5th longword! */
  /* inc_next is after the array */
} GC_Weak_Array;

/* The GC_Weak_Box struct is not externally visible, but
   first three fields are mandated by the GC interface */
typedef struct GC_Weak_Box {
  Type_Tag type;
  short keyex;
  void *val;
  /* The rest is up to us: */
  void **secondary_erase;
  int soffset, is_late;
  struct GC_Weak_Box *next;
  struct GC_Weak_Box *inc_next;
} GC_Weak_Box;

/* The GC_Ephemeron struct is not externally visible, but
   first three fields are mandated by the GC interface */
typedef struct GC_Ephemeron {
  Type_Tag type;
  short keyex;
  void *key;
  void *val;
  /* The rest is up to us: */
  struct GC_Ephemeron *next;
  struct GC_Ephemeron *inc_next;
} GC_Ephemeron;

typedef struct GC_Immobile_Box {
  void *p; /* this must be first or mred dies */
  struct GC_Immobile_Box *next;
  struct GC_Immobile_Box *prev;
} GC_Immobile_Box;

typedef struct finalizer {
  char eager_level;
  char tagged;
  void *p;
  GC_finalization_proc f;
  void *data;
#if CHECKS
  intptr_t size;
#endif
  struct finalizer *next;
  /* Patched after GC: */
  struct finalizer *prev;
  struct finalizer *left;
  struct finalizer *right;
} Fnl;
