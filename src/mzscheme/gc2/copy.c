/*
  Precise GC for MzScheme
  Copyright (c) 2004-2005 PLT Scheme, Inc.
  Copyright (c) 1999 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.
*/

/* This implementation is currently hard-wired for 4-byte words */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define USE_MMAP 1
#define GROW_FACTOR 1.5

#if defined(sparc) || defined(__sparc) || defined(__sparc__)
# define ALIGN_DOUBLES 1
#else
# define ALIGN_DOUBLES 0
#endif

#if USE_MMAP
/* For mmap: */
# include <fcntl.h>
# include <sys/types.h>
# include <sys/mman.h>
# include <errno.h>
#endif

typedef short Type_Tag;

#include "gc2.h"

#define TIME 0
#define SEARCH 1
#define SAFETY 1
#define RECYCLE_HEAP 0
#define KEEP_FROM_PTR 0

#define GC_EVERY_ALLOC 0
#define ALLOC_GC_PHASE 0
#define SKIP_FORCED_GC 0

#define CHECK_STACK_EVERY 5
#define CHECK_STACK_START -1

void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_out_of_memory)(void);
unsigned long (*GC_get_thread_stack_base)(void);

void (*GC_mark_xtagged)(void *obj);
void (*GC_fixup_xtagged)(void *obj);

void **GC_variable_stack;

Type_Tag weak_box_tag;

#define gc_finalization_tag 256
#define gc_finalization_weak_link_tag 257
#define gc_weak_array_tag 258

#define _num_tags_ 259

Size_Proc size_table[_num_tags_];
Mark_Proc mark_table[_num_tags_];
Fixup_Proc fixup_table[_num_tags_];

#define STARTING_PLACE ((void *)0x400000)

void *GC_alloc_space = STARTING_PLACE, *GC_alloc_top;
static long alloc_size, heap_size = 32000;
static void **tagged_high = STARTING_PLACE, **untagged_low = STARTING_PLACE;
static void **new_tagged_high, **new_untagged_low;

static void *old_space;
static long old_size;

static char *alloc_bitmap;

static char zero_sized[4];

static void *park[2];

static int cycle_count = 0;
#if GC_EVERY_ALLOC
static int alloc_cycle = ALLOC_GC_PHASE;
static int skipped_first = !SKIP_FORCED_GC;
#endif

#if KEEP_FROM_PTR
static void *mark_source;
# define FROM_STACK ((void *)0xAAAA1)
# define FROM_ROOT ((void *)0xAAAA3)
# define FROM_FNL ((void *)0xAAAA5)
# define FROM_NEW ((void *)0xAAAA7)
# define FROM_IMM ((void *)0xAAAA7)
#endif

/******************************************************************************/

#if USE_MMAP

int fd, fd_created;

#define PAGE_SIZE 4096

void *malloc_pages(size_t len)
{
  void *r;

  if (!fd_created) {
    fd_created = 1;
    fd = open("/dev/zero", O_RDWR);
  }

  if (len & (PAGE_SIZE - 1)) {
    len += PAGE_SIZE - (len & (PAGE_SIZE - 1));
  }

  r = mmap(NULL, len, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);

  if (r  == (void *)-1) {
    printf("mmap failed: %s\n", strerror(errno));
    exit(-1);
  }

  return r;
}

void free_pages(void *p, size_t len)
{
  munmap(p, len);
}

#endif

/******************************************************************************/

#if !USE_MMAP

void *malloc_pages(size_t len)
{
  return malloc(len);
}

void free_pages(void *p, size_t len)
{
  free(p);
}

#endif

/******************************************************************************/

#define PTR_ALIGNMENT 4
#define PTR_TO_INT(x) ((unsigned long)x)
#define INT_TO_PTR(x) ((void *)x)

static long roots_count;
static long roots_size;
static unsigned long *roots;

static int compare_roots(const void *a, const void *b)
{
  if (*(unsigned long *)a < *(unsigned long *)b)
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

  qsort(roots, roots_count >> 1, 2 * sizeof(unsigned long), compare_roots);
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
    } else if (roots[i] == roots[i + 1]) {
      /* Remove empty range: */
      offset += 2;
      roots_count -= 2;
    } else if (offset) {
      /* compact: */
      roots[i - offset] = roots[i];
      roots[i + 1 - offset] = roots[i + 1];
    }
  }
}

void GC_add_roots(void *start, void *end)
{
  if (roots_count >= roots_size) {
    unsigned long *naya;

    roots_size = roots_size ? 2 * roots_size : 500;
    naya = (unsigned long *)malloc(sizeof(unsigned long) * (roots_size + 1));

    memcpy((void *)naya, (void *)roots, 
	   sizeof(unsigned long) * roots_count);

    if (roots)
      free(roots);

    roots = naya;
  }

  roots[roots_count++] = PTR_TO_INT(start);
  roots[roots_count++] = PTR_TO_INT(end) - PTR_ALIGNMENT;
}

typedef struct ImmobileBox {
  void *p;
  struct ImmobileBox *next, *prev;
} ImmobileBox;

static ImmobileBox *immobile;

void *GC_malloc_immobile_box(void *p)
{
  ImmobileBox *ib;

  ib = (ImmobileBox *)malloc(sizeof(ImmobileBox));
  ib->p = p;
  ib->next = immobile;
  if (immobile)
    immobile->prev = ib;
  ib->prev = NULL;

  immobile = ib;

  return ib;
}

void GC_free_immobile_box(void *b)
{
  ImmobileBox *ib = (ImmobileBox *)b;

  if (!ib)
    return;

  if (ib->prev)
    ib->prev->next = ib->next;
  else
    immobile = ib->next;
  if (ib->next)
    ib->next->prev = ib->prev;

  free(ib);
}

/******************************************************************************/

typedef struct GC_Weak_Array {
  Type_Tag type;
  short keyex;
  long count;
  void *replace_val;
  struct GC_Weak_Array *next;
  void *data[1];
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
  /* Not used */
  return size_weak_array(p);
}

static int fixup_weak_array(void *p)
{
  GC_Weak_Array *a = (GC_Weak_Array *)p;

  gcFIXUP(a->replace_val);
    
  a->next = weak_arrays;
  weak_arrays = a;

  return size_weak_array(p);
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

  w->type = gc_weak_array_tag;
  w->replace_val = replace_val;
  w->count = (size_in_bytes >> 2);
  
  return w;
}

typedef struct GC_Weak_Box {
  /* The first three fields are mandated by the GC spec: */
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
  /* Not used */
  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Box));
}

static int fixup_weak_box(void *p)
{
  GC_Weak_Box *wb = (GC_Weak_Box *)p;
  
  gcFIXUP(wb->secondary_erase);
  if (wb->val) {
    wb->next = weak_boxes;
    weak_boxes = wb;
  }

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

/******************************************************************************/

typedef struct Fnl {
  Type_Tag type;
  short eager_level;
  void *p;
  void (*f)(void *p, void *data);
  void *data;
  struct Fnl *next;
} Fnl;

static Fnl *fnls, *run_queue, *last_in_queue;

static int size_finalizer(void *p)
{
  return gcBYTES_TO_WORDS(sizeof(Fnl));
}

static int mark_finalizer(void *p)
{
  /* Not used */
  return gcBYTES_TO_WORDS(sizeof(Fnl));
}

static int fixup_finalizer(void *p)
{
  Fnl *fnl = (Fnl *)p;
  
  gcFIXUP(fnl->next);
  gcFIXUP(fnl->data);
  if (!fnl->eager_level) {
    /* Queued for run: */
    gcFIXUP(fnl->p);
  }

  return gcBYTES_TO_WORDS(sizeof(Fnl));
}

void GC_set_finalizer(void *p, int tagged, int level, void (*f)(void *p, void *data), 
		      void *data, void (**oldf)(void *p, void *data), 
		      void **olddata)
{
  Fnl *fnl, *prev;

  if (((long)p & 0x1) || (p < GC_alloc_space) || (p > GC_alloc_top)) {
    /* Never collected. Don't finalize it. */
    if (oldf) *oldf = NULL;
    if (olddata) *olddata = NULL;
    return;
  }

  fnl = fnls;
  prev = NULL;
  while (fnl) {
    if (fnl->p == p) {
      if (oldf) *oldf = fnl->f;
      if (olddata) *olddata = fnl->data;
      if (f) {
	fnl->f = f;
	fnl->data = data;
	fnl->eager_level = level;
      } else {
	if (prev)
	  prev->next = fnl->next;
	else
	  fnls = fnl->next;
	return;
      }
      return;
    } else {
      prev = fnl;
      fnl = fnl->next;
    }
  }
  
  if (oldf) *oldf = NULL;
  if (olddata) *olddata = NULL;

  if (!f)
    return;

  /* Allcation might trigger GC, so we use park: */
  park[0] = p;
  park[1] = data;

  fnl = GC_malloc_one_tagged(sizeof(Fnl));

  p = park[0];
  park[0] = NULL;
  data = park[1];
  park[1] = NULL;

  fnl->type = gc_finalization_tag;
  fnl->next = fnls;
  fnl->p = p;
  fnl->f = f;
  fnl->data = data;
  fnl->eager_level = level;

  fnls = fnl;
}

typedef struct Fnl_Weak_Link {
  Type_Tag type;
  void *p;
  long offset; /* offset from beginning of block */
  void *saved;
  struct Fnl_Weak_Link *next;
} Fnl_Weak_Link;

static Fnl_Weak_Link *fnl_weaks;

static int size_finalizer_weak_link(void *p)
{
  return gcBYTES_TO_WORDS(sizeof(Fnl_Weak_Link));
}

static int mark_finalizer_weak_link(void *p)
{
  /* Not used */
  return gcBYTES_TO_WORDS(sizeof(Fnl_Weak_Link));
}

static int fixup_finalizer_weak_link(void *p)
{
  Fnl_Weak_Link *wl = (Fnl_Weak_Link *)p;
  
  gcFIXUP(wl->next);

  return gcBYTES_TO_WORDS(sizeof(Fnl_Weak_Link));
}

void GC_finalization_weak_ptr(void **p, int offset)
{
  Fnl_Weak_Link *wl;

#ifdef SAFETY
  if (((void *)p < GC_alloc_space) || (p >= GC_alloc_top)) {
    *(int *)0x0 = 1;
  }
#endif

  /* Allcation might trigger GC, so we use park: */
  park[0] = p;

  wl = (Fnl_Weak_Link *)GC_malloc_one_tagged(sizeof(Fnl_Weak_Link));

  p = park[0];
  park[0] = NULL;

  wl->type = gc_finalization_weak_link_tag;
  wl->p = p;
  wl->offset = offset * sizeof(void*);
  wl->next = fnl_weaks;

  fnl_weaks = wl;
}

/******************************************************************************/

static unsigned long stack_base;

void GC_set_stack_base(void *base)
{
  stack_base = (unsigned long)base;
}

unsigned long GC_get_stack_base(void)
{
  return stack_base;
}

void GC_dump(void)
{
  fprintf(stderr, "Memory use: %ld\n", GC_get_memory_use());
}

long GC_get_memory_use()
{
  return (alloc_size - ((untagged_low - tagged_high) << 2));
}

void GC_init_type_tags(int count, int weakbox)
{
  weak_box_tag = weakbox;
}

#define SKIP ((Type_Tag)0x7000)
#define MOVED ((Type_Tag)0x3000)

#if SEARCH
void *search_for, *search_mark;
long search_size;
int detail_cycle;
int atomic_detail_cycle;
#endif

#if SEARCH
void stop()
{
  printf("stopped\n");
}
#endif

/* Only works during GC: */
void *find_start(void *p)
{
  long diff = ((char *)p - (char *)GC_alloc_space) >> 2;

  if (((long)p & 0x3) || !(alloc_bitmap[diff >> 3] & (1 << (diff & 0x7)))) {
    while (!(alloc_bitmap[diff >> 3] & (1 << (diff & 0x7)))) {
      diff--;
    }
      
    diff <<= 2;

    return (void *)((char *)GC_alloc_space + diff);
  } else
    return p;
}

#ifdef SAFETY
static void middle(unsigned long p, long delta, unsigned long where)
{
  fprintf(stderr, "Middle!: 0x%lx d: %ld at 0x%lx\n", p, delta, where);
}

static int check_count = CHECK_STACK_START;

static void check_interior_pointer(void **pp)
{
  void *p = *pp;

  if (check_count--)
    return;
  else
    check_count = CHECK_STACK_EVERY;

  if (!((long)p & 0x1)
      && (p >= GC_alloc_space)
      && (p <= GC_alloc_top)) {
    long diff = ((char *)p - (char *)GC_alloc_space) >> 2;
    
    if (((long)p & 0x3) || !(alloc_bitmap[diff >> 3] & (1 << (diff & 0x7)))) {
      long diff1 = ((char *)p - (char *)GC_alloc_space);
      
      while (!(alloc_bitmap[diff >> 3] & (1 << (diff & 0x7)))) {
	diff--;
      }
      
      diff <<= 2;
      
      if (((diff + (char *)GC_alloc_space) > (char *)tagged_high)
	  && ((*(long *)(diff + (char *)GC_alloc_space - 4) & 0x20000000))) {
	/* Middle is ok. */
      } else {
	middle((unsigned long)p, diff1 - diff, (unsigned long)pp);
      }
    }
  }
}
#endif

static void *mark(void *p)
{
  long diff = ((char *)p - (char *)GC_alloc_space) >> 2;

#if SEARCH
  if (p == search_mark)
    stop();
#endif

  if (((long)p & 0x3) || !(alloc_bitmap[diff >> 3] & (1 << (diff & 0x7)))) {
    long diff1 = ((char *)p - (char *)GC_alloc_space);
      
    while (!(alloc_bitmap[diff >> 3] & (1 << (diff & 0x7)))) {
      diff--;
    }
      
    diff <<= 2;

#ifdef SAFETY
    if (((diff + (char *)GC_alloc_space) > (char *)tagged_high)
	&& ((*(long *)(diff + (char *)GC_alloc_space - 4) & 0x20000000)
	    || (!(*(long *)(diff + (char *)GC_alloc_space - 4))
		&& (*(long **)(diff + (char *)GC_alloc_space))[-1] & 0x20000000))) {
      /* Middle is ok. */
    } else {
      middle((unsigned long)p, diff1 - diff, 0);
    }
#endif

    return (void *)((char *)mark(diff + (char *)GC_alloc_space) + (diff1 - diff));
  } else {
    if (p < (void *)tagged_high) {
      Type_Tag tag = *(Type_Tag *)p;
      long size;
      void *naya;
	
      if (tag == MOVED)
	return ((void **)p)[1];

#if SAFETY	
      if ((tag < 0) || (tag >= _num_tags_) || !size_table[tag]) {
	*(int *)0x0 = 1;
      }
#endif
	
      size = size_table[tag](p);
#if ALIGN_DOUBLES
      if (!(size & 0x1)) {
	if ((long)new_tagged_high & 0x4) {
	  ((Type_Tag *)new_tagged_high)[0] = SKIP;
	  new_tagged_high += 1;
	}
      }
#endif
	
#if KEEP_FROM_PTR
      *new_tagged_high = mark_source;
      new_tagged_high++;
#endif

      {
	int i;
	long *a, *b;
	a = (long *)new_tagged_high;
	b = (void *)p;
	for (i = size; i--; )
	  *(a++) = *(b++);
      }
	
      naya = new_tagged_high;
      ((Type_Tag *)p)[0] = MOVED;
      ((void **)p)[1] = naya;
	
      new_tagged_high += size;
#if SEARCH
      if (naya == search_for) {
	stop();
      }
#endif
      return naya;
    } else {
      long size;
	
      p -= 4;
      size = ((*(long *)p) & 0x0FFFFFFF);
	
      if (!size)
	return ((void **)p)[1];

#if ALIGN_DOUBLES
      if (!(size & 1)) {
	if (!((long)new_untagged_low & 0x4)) {
	  new_untagged_low--;
	  *(long *)new_untagged_low = 0;
	}
      }
#endif
      size++;

      new_untagged_low -= size;

#if SAFETY
      if ((unsigned long)new_untagged_low < (unsigned long)new_tagged_high) {
	*(int *)0x0 = 1;
      }
#endif	

      {
	int i;
	long *a, *b;
	a = (long *)new_untagged_low;
	b = (void *)p;
	for (i = size; i--; )
	  *(a++) = *(b++);
      }
      ((void **)p)[1] = new_untagged_low + 1;
      ((long *)p)[0] = 0;
	
#if SEARCH
      if ((new_untagged_low + 1) == search_for) {
	stop();
      }
#endif

#if SEARCH
      if (atomic_detail_cycle == cycle_count) {
	printf("%ld at %lx\n", size, (long)new_untagged_low);
      }
#endif

#if KEEP_FROM_PTR
      --new_untagged_low;
      *new_untagged_low = mark_source;
      return new_untagged_low + 2;
#else
      return new_untagged_low + 1;
#endif
    }
  }
}

void GC_mark(const void *p)
{
  /* Not used. */
}

void GC_fixup(void *_p)
{
  void *p;

  p = *(void **)_p;

  if (!((long)p & 0x1)
      && (p >= GC_alloc_space)
      && (p <= GC_alloc_top))
    *(void **)_p = mark(p);
}

static void **o_var_stack, **oo_var_stack;

void GC_mark_variable_stack(void **var_stack,
			    long delta,
			    void *limit)
{
  /* Not used. */
}

void GC_trace_variable_stack(void **var_stack,
			     long delta,
			     void *limit,
			     int just_check)
{
  int stack_depth;

  stack_depth = 0;
  while (var_stack) {
    long size;
    void ***p;

    var_stack = (void **)((char *)var_stack + delta);
    if (var_stack == limit)
      return;

    size = *(long *)(var_stack + 1);

    oo_var_stack = o_var_stack;
    o_var_stack = var_stack;

    p = (void ***)(var_stack + 2);
    
    while (size--) {
      if (!*p) {
	/* Array */
	long count = ((long *)p)[2];
	void **a = ((void ***)p)[1];
	p += 2;
	size -= 2;
	a = (void **)((char *)a + delta);
	while (count--) {
#ifdef SAFETY
	  if (just_check) {
	    check_interior_pointer(a);
	  } else 
#endif
	    { gcFIXUP(*a); }
	  a++;
	}
      } else {
	void **a = *p;
	a = (void **)((char *)a + delta);
#ifdef SAFETY
	if (just_check) {
	  check_interior_pointer(a);
	} else
#endif
	  { gcFIXUP(*a); }
      }
      p++;
    }

#if SAFETY
    if (*var_stack && ((unsigned long)*var_stack <= (unsigned long)var_stack))
      *(int *)0x0 = 1;
#endif

    var_stack = *var_stack;
    stack_depth++;
  }
}

void GC_fixup_variable_stack(void **var_stack,
			     long delta,
			     void *limit)
{
  GC_trace_variable_stack(var_stack, delta, limit, 0);
}

#if SAFETY
void check_variable_stack()
{
  void **limit, **var_stack;

  if (!alloc_bitmap)
    return;

  limit = (void **)(GC_get_thread_stack_base
		    ? GC_get_thread_stack_base()
		    : stack_base);

  var_stack = GC_variable_stack;

  GC_trace_variable_stack(var_stack, 0, limit, 1);
}
#endif

#if 0
# define GETTIME() ((long)scheme_get_milliseconds())
#else
# define GETTIME() ((long)scheme_get_process_milliseconds())
#endif

#if TIME
# define PRINTTIME(x) fprintf x
# define STDERR stderr
static long started, rightnow, old;
# define INITTIME() (started = GETTIME())
# define GETTIMEREL() (rightnow = GETTIME(), old = started, started = rightnow, rightnow - old)
#else
# define INITTIME() /* empty */
# define PRINTTIME(x) /* empty */
#endif

static int initialized;
#if SAFETY
static long *prev_ptr;
static void **prev_var_stack;
#endif

void gcollect(int needsize)
{
  /* Check old: */
  long *p, *top;
  void *new_space;
  long new_size;
  void **tagged_mark, **untagged_mark;
  char *bitmap;
  int i, did_fnls;
  long diff, iterations;
  ImmobileBox *ib;
  GC_Weak_Box *wb;
  GC_Weak_Array *wa;

  INITTIME();
  PRINTTIME((STDERR, "gc: start: %ld\n", GETTIMEREL()));

  cycle_count++;

  if (!initialized) {
    GC_register_traversers(weak_box_tag, size_weak_box, mark_weak_box, fixup_weak_box);
    GC_register_traversers(gc_weak_array_tag, size_weak_array, mark_weak_array, fixup_weak_array);
    GC_register_traversers(gc_finalization_tag, size_finalizer, mark_finalizer, fixup_finalizer);
    GC_register_traversers(gc_finalization_weak_link_tag, size_finalizer_weak_link, mark_finalizer_weak_link, fixup_finalizer_weak_link);

    GC_add_roots(&fnls, (char *)&fnls + sizeof(fnls) + 1);
    GC_add_roots(&fnl_weaks, (char *)&fnl_weaks + sizeof(fnl_weaks) + 1);
    GC_add_roots(&run_queue, (char *)&run_queue + sizeof(run_queue) + 1);
    GC_add_roots(&last_in_queue, (char *)&last_in_queue + sizeof(last_in_queue) + 1);
    GC_add_roots(&park, (char *)&park + sizeof(park) + 1);
    initialized = 1;
  }

  weak_boxes = NULL;
  weak_arrays = NULL;
  did_fnls = 0;

  if (GC_collect_start_callback)
    GC_collect_start_callback();

  sort_and_merge_roots();

  new_size = (heap_size * GROW_FACTOR);
  if (new_size < alloc_size)
    new_size = alloc_size;

  new_size += needsize;

  /* word-aligned: */
  new_size = (new_size + 3) & 0xFFFFFFFC;
  
  if (old_size >= new_size) {
    new_size = old_size;
    new_space = old_space;
  } else {
    if (old_size) {
      free_pages(old_space, old_size);
      old_size = 0;
    }

    new_space = malloc_pages(new_size + 4);

    if (!new_space) {
      printf("Out of memory");
      abort();
    }
  }

  /******************** Mark/Copy ****************************/

  tagged_mark = new_tagged_high = (void **)new_space;
  untagged_mark = new_untagged_low = (void **)(new_space + new_size);

#if KEEP_FROM_PTR
  mark_source = FROM_STACK;
#endif

  GC_fixup_variable_stack(GC_variable_stack,
			  0,
			  (void *)(GC_get_thread_stack_base
				   ? GC_get_thread_stack_base()
				   : stack_base));

  PRINTTIME((STDERR, "gc: stack: %ld\n", GETTIMEREL()));

#if KEEP_FROM_PTR
  mark_source = FROM_ROOT;
#endif

  for (i = 0; i < roots_count; i += 2) {
    void **s = (void **)roots[i];
    void **e = (void **)roots[i + 1];
    
    while (s < e) {
      gcFIXUP(*s);
      s++;
    }
  }

#if KEEP_FROM_PTR
  mark_source = FROM_IMM;
#endif

  /* Do immobiles: */
  for (ib = immobile; ib; ib = ib->next) {
    gcFIXUP(ib->p);
  }

  PRINTTIME((STDERR, "gc: roots: %ld\n", GETTIMEREL()));

  iterations = 0;

  while (1) { /* Loop to do finalization */

    while ((tagged_mark < new_tagged_high)
	   || (untagged_mark > new_untagged_low)) {
      
      iterations++;
      
      while (tagged_mark < new_tagged_high) {
	Type_Tag tag;
	long size;
	
#if KEEP_FROM_PTR
	tagged_mark++;
#endif

	tag = *(Type_Tag *)tagged_mark;

#if ALIGN_DOUBLES
	if (tag == SKIP)
	  tagged_mark++;
	else {
#endif
	  
#if SAFETY
	  if ((tag < 0) || (tag >= _num_tags_) || !size_table[tag]) {
	    *(int *)0x0 = 1;
	  }
#endif
	  
#if KEEP_FROM_PTR
	  mark_source = tagged_mark;
#endif

	  size = size_table[tag](tagged_mark);
	  fixup_table[tag](tagged_mark);
	  
#if SAFETY
	  if (size <= 1) {
	    *(int *)0x0 = 1;
	  }
#endif
	  
	  tagged_mark += size;
	  
#if SAFETY
	  if ((void *)tagged_mark < new_space) {
	    *(int *)0x0 = 1;
	  }
#endif
#if ALIGN_DOUBLES
	}
#endif
      }

      while (untagged_mark > new_untagged_low) {
	void **mp, **started;
	
	mp = started = new_untagged_low;
	while (mp < untagged_mark) {
	  long v, size;
#if KEEP_FROM_PTR
	  mp++;
#endif
	  v = *(long *)mp;
	  size = (v & 0x0FFFFFFF);
	  
	  if (v & 0xF0000000) {
#if KEEP_FROM_PTR
	    mark_source = mp;
#endif
	    mp++;	    
	    if (v & 0x80000000) {
	      /* Array of pointers */
	      int i;
	      /* printf("parray: %d %lx\n", size, (long)mp); */
	      for (i = size; i--; mp++) {
		gcFIXUP(*mp);
	      }
	    } else if (v & 0x10000000) {
	      /* xtagged */
	      GC_fixup_xtagged(mp);
	      mp += size;
	    } else {
	      /* Array of tagged */
	      int i, elem_size;
	      Type_Tag tag = *(Type_Tag *)mp;
	      
	      elem_size = size_table[tag](mp);
	      fixup_table[tag](mp);
	      mp += elem_size;
	      for (i = elem_size; i < size; i += elem_size, mp += elem_size)
		fixup_table[tag](mp);
	    }
	  } else
	    mp += v + 1;
	}
	untagged_mark = started;
      }
    }
      
    if ((did_fnls >= 3) || !fnls) {
      if (did_fnls == 3) {
	/* Finish up ordered finalization */
	Fnl *f, *next, *prev;
	Fnl_Weak_Link *wl;

	/* Enqueue and mark level 3 finalizers that still haven't been marked. */
	/* (Recursive marking is already done, though.) */
	prev = NULL;
	for (f = fnls; f; f = next) {
	  next = f->next;
	  if (f->eager_level == 3) {
	    void *v;

	    v = GC_resolve(f->p);

	    if (v == f->p) {
	      /* Not yet marked. Mark it and enqueue it. */
#if KEEP_FROM_PTR
	      mark_source = f;
#endif
	      gcFIXUP(f->p);

	      if (prev)
		prev->next = next;
	      else
		fnls = next;
	      
	      f->eager_level = 0; /* indicates queued */
	      
	      f->next = NULL;
	      if (last_in_queue) {
		last_in_queue->next = f;
		last_in_queue = f;
	      } else {
		run_queue = last_in_queue = f;
	      }
	    } else {
	      f->p = v;
	      prev = f;
	    }
	  }
	}

	/* Restore zeroed out weak links, marking as we go: */	
	for (wl = fnl_weaks; wl; wl = wl->next) {
	  void *wp = (void *)GC_resolve(wl->p);
	  int markit;
	  markit = (wp != wl->p);
	  wp = (wp + wl->offset);
	  if (markit)
	    gcFIXUP(wl->saved);
	  *(void **)wp = wl->saved;
	}
	
	/* We have to mark one more time, because restoring a weak
           link may have made something reachable. */

	did_fnls++;
      } else
	break;
    } else {
      int eager_level = did_fnls + 1;
      
      if (eager_level == 3) {
	/* Ordered finalization */
	Fnl *f;
	Fnl_Weak_Link *wl;

	/* Zero out weak links for ordered finalization */
	for (wl = fnl_weaks; wl; wl = wl->next) {
	  void *wp = (void *)GC_resolve(wl->p);
	  wl->saved = *(void **)(wp + wl->offset);
	  *(void **)(wp + wl->offset) = NULL;
	}

	/* Mark content of not-yet-marked finalized objects,
	   but don't mark the finalized objects themselves. */	
	for (f = fnls; f; f = f->next) {
	  if (f->eager_level == 3) {
	    void *v;

	    v = GC_resolve(f->p);

	    if (v == f->p) {
	      /* Not yet marked. Do content. */
	      Type_Tag tag = *(Type_Tag *)v;
#if SAFETY
	      if ((tag < 0) || (tag >= _num_tags_) || !fixup_table[tag]) {
		*(int *)0x0 = 1;
	      }
#endif
#if KEEP_FROM_PTR
	      mark_source = FROM_FNL;
#endif
	      fixup_table[tag](v);
	    }
	  }
	}
      } else {
	/* Unordered finalization */
	Fnl *f, *prev, *queue;

	f = fnls;
	prev = NULL;
	queue = NULL;
	
	while (f) {
	  if (f->eager_level == eager_level) {
	    void *v;

	    v = GC_resolve(f->p);

	    if (v == f->p) {
	      /* Not yet marked. Move finalization to run queue. */
	      Fnl *next = f->next;

	      if (prev)
		prev->next = next;
	      else
		fnls = next;
	      
	      f->eager_level = 0; /* indicates queued */
	      
	      f->next = NULL;
	      if (last_in_queue) {
		last_in_queue->next = f;
		last_in_queue = f;
	      } else {
		run_queue = last_in_queue = f;
	      }
	      if (!queue)
		queue = f;

	      f = next;
	    } else {
	      f->p = v;
	      prev = f;
	      f = f->next;
	    }
	  } else {
	    prev = f;
	    f = f->next;
	  }
	}
	
	/* Mark items added to run queue: */
	f = queue;
	while (f) {
#if KEEP_FROM_PTR
	  mark_source = f;
#endif
	  gcFIXUP(f->p);
	  f = f->next;
	}
      }
	
      did_fnls++;
    }

  }

  PRINTTIME((STDERR, "gc: mark/copy (%d): %ld\n", iterations, GETTIMEREL()));

  /******************************************************/

  /* Do weak boxes: */
  wb = weak_boxes;
  while (wb) {
    if (!((long)wb->val & 0x1) && ((void *)wb->val >= GC_alloc_space) && ((void *)wb->val <= GC_alloc_top)) {
      void *v;
      v = GC_resolve(wb->val);
      if (v == wb->val) {
	wb->val = NULL;
	if (wb->secondary_erase) {
	  *(wb->secondary_erase + wb->soffset) = NULL;
	  wb->secondary_erase = NULL;
	}
      } else
	wb->val = v;
    } /* else not collectable */

    wb = wb->next;
  }

  /* Do weak arrays: */
  wa = weak_arrays;
  while (wa) {
    int i;

    for (i = wa->count; i--; ) {
      void *p = wa->data[i];
      if (!((long)p & 0x1) && (p >= GC_alloc_space) && (p <= GC_alloc_top)) {
	void *v;    
	v = GC_resolve(p);
	if (v == p)
	  wa->data[i] = wa->replace_val;
	else
	  wa->data[i] = v;
      } /* else not collectable */
    }

    wa = wa->next;
  }

  /* Cleanup weak finalization links: */
  {
    Fnl_Weak_Link *wl, *prev, *next;

    prev = NULL;
    for (wl = fnl_weaks; wl; wl = next) {
      void *wp;
      next = wl->next;
      wp = (void *)GC_resolve(wl->p);
      if (wp == wl->p) {
	/* Collectable. Removed this link. */
	if (prev)
	  prev->next = next;
	else
	  fnl_weaks = next;
      } else {
	wl->p = wp;
	prev = wl;
      }
    }
  }

  /******************************************************/
  
#if RECYCLE_HEAP
  old_space = GC_alloc_space;
  old_size = alloc_size;
#else
  if (alloc_size)
    free_pages(GC_alloc_space, alloc_size + 4);
#endif

  free(alloc_bitmap);

  PRINTTIME((STDERR, "gc: free: %ld\n", GETTIMEREL()));

  if (new_untagged_low < new_tagged_high) {
    printf("Ouch: Tagged area collided with untagged area.\n");
    abort();
  }

  alloc_size = new_size;
  GC_alloc_space = new_space;
  GC_alloc_top = GC_alloc_space + alloc_size;
  tagged_high = new_tagged_high;
  untagged_low = new_untagged_low;

  heap_size = new_size - ((untagged_low - tagged_high) << 2);
  
  {
    long *p = (long *)untagged_low;
    while (p-- > (long *)tagged_high)
      *p = 0;
  }

  PRINTTIME((STDERR, "gc: restored: %ld\n", GETTIMEREL()));

  /******************** Make initial bitmap image: ****************************/

  {
    alloc_bitmap = bitmap = (char *)malloc((alloc_size >> 5) + 1);
    memset(bitmap, 0, (alloc_size >> 5) + 1);
  }

  p = (long *)untagged_low;
  diff = (((char *)p - (char *)GC_alloc_space) + 4) >> 2;
  top = (long *)GC_alloc_top;
  while (p < top) {
    long size;

#if KEEP_FROM_PTR
    diff++;
    p++;
#endif

    size = (*p & 0x0FFFFFFF) + 1;

    bitmap[diff >> 3] |= (1 << (diff & 0x7));

    p += size;
    diff += size;
  }

  p = ((long *)GC_alloc_space);
  diff = ((char *)p - (char *)GC_alloc_space) >> 2;
  while (p < (long *)tagged_high) {
    Type_Tag tag;
    long size;
      
#if KEEP_FROM_PTR
    diff++;
    p++;
#endif
    tag = *(Type_Tag *)p;

#if ALIGN_DOUBLES
    if (tag == SKIP) {
      p++;
      diff++;
    } else {
#endif
      bitmap[diff >> 3] |= (1 << (diff & 0x7));

#if SEARCH
      if (cycle_count == detail_cycle)
	printf("tag: %lx =  %d\n", (long)p, tag);
#endif

#if SAFETY
      if ((tag < 0) || (tag >= _num_tags_) || !size_table[tag]) {
	fflush(NULL);
	*(int *)0x0 = 1;
      }
      prev_ptr = p;
      prev_var_stack = GC_variable_stack;
#endif
      size = size_table[tag](p);
#if SAFETY
      if (prev_var_stack != GC_variable_stack) {
	*(int *)0x0 = 1;
      }
#endif
      
      p += size;
      diff += size;
#if ALIGN_DOUBLES
    }
#endif
  }

  PRINTTIME((STDERR, "gc: done (t=%d, u=%d): %ld\n", 
	     (long)((void *)tagged_high - GC_alloc_space),
	     (long)(GC_alloc_top - (void *)untagged_low),
	     GETTIMEREL()));

  if (GC_collect_start_callback)
    GC_collect_end_callback();

  /**********************************************************************/

  /* Run Finalizations. Collections may happen */

  while (run_queue) {
    Fnl *f;
    void **gcs;

    f = run_queue;
    run_queue = run_queue->next;
    if (!run_queue)
      last_in_queue = NULL;

    gcs = GC_variable_stack;
    f->f(f->p, f->data);
    GC_variable_stack = gcs;
  }
}

void *GC_resolve(void *p)
{
  if (!((long)p & 0x1) && (p >= GC_alloc_space) && (p <= GC_alloc_top)) {  
    if (p < (void *)tagged_high) {
      Type_Tag tag = *(Type_Tag *)p;

      if (tag == MOVED)
	return ((void **)p)[1];
      else
	return p;
    } else {
      long size;
      
      p -= 4;
      size = ((*(long *)p) & 0x0FFFFFFF);
      
      if (!size)
	return ((void **)p)[1];
      else
	return p + 4;
    }
  } else
    return p;
}

static void *malloc_tagged(size_t size_in_bytes)
{
  void **m, **naya;

#if SAFETY
  check_variable_stack();
#endif

#if GC_EVERY_ALLOC
# if SKIP_FORCED_GC
  if (!skipped_first) {
    alloc_cycle++;
    if (alloc_cycle >= SKIP_FORCED_GC) {
      alloc_cycle = 0;
      skipped_first = 1;
    }
  }
# endif
  if (skipped_first) {
    alloc_cycle++;
    if (alloc_cycle >= GC_EVERY_ALLOC) {
      alloc_cycle = 0;
      gcollect(size_in_bytes);
    }
  }
#endif

#if KEEP_FROM_PTR
  size_in_bytes += 4;
#endif

  size_in_bytes = ((size_in_bytes + 3) & 0xFFFFFFFC);
#if ALIGN_DOUBLES
  if (!(size_in_bytes & 0x4)) {
    /* Make sure memory is 8-aligned */
    if (((long)tagged_high & 0x4)) {
      if (tagged_high == untagged_low) {
	gcollect(size_in_bytes);
#if KEEP_FROM_PTR
	size_in_bytes -= 4;
#endif
	return malloc_tagged(size_in_bytes);
      }
      ((Type_Tag *)tagged_high)[0] = SKIP;
      tagged_high += 1;
    }
  }
#endif

#if SEARCH
  if (size_in_bytes == search_size)
    stop();
#endif

  m = tagged_high;
  naya = tagged_high + (size_in_bytes >> 2);
  if (naya > untagged_low) {
    gcollect(size_in_bytes);
#if KEEP_FROM_PTR
    size_in_bytes -= 4;
#endif
    return malloc_tagged(size_in_bytes);
  }
  tagged_high = naya;

#if KEEP_FROM_PTR
  *m = FROM_NEW;
  m++;
#endif

#if SEARCH
  if (m == search_for) {
    stop();
  }
#endif

  {
    long diff = ((char *)m - (char *)GC_alloc_space) >> 2;

    alloc_bitmap[diff >> 3] |= (1 << (diff & 0x7));
  }

  return m;
}

static void *malloc_untagged(size_t size_in_bytes, unsigned long nonatomic)
{
  void **naya;

#if SAFETY
  check_variable_stack();
#endif

#if GC_EVERY_ALLOC
# if SKIP_FORCED_GC
  if (!skipped_first) {
    alloc_cycle++;
    if (alloc_cycle >= SKIP_FORCED_GC) {
      alloc_cycle = 0;
      skipped_first = 1;
    }
  }
# endif
  if (skipped_first) {
    alloc_cycle++;
    if (alloc_cycle >= GC_EVERY_ALLOC) {
      alloc_cycle = 0;
      gcollect(size_in_bytes);
    }
  }
#endif

  if (!size_in_bytes)
    return zero_sized;

#if KEEP_FROM_PTR
  size_in_bytes += 4;
#endif

  size_in_bytes = ((size_in_bytes + 3) & 0xFFFFFFFC);
#if ALIGN_DOUBLES
  if (!(size_in_bytes & 0x4)) {
    /* Make sure memory is 8-aligned */
    if ((long)untagged_low & 0x4) {
      if (untagged_low == tagged_high) {
#if KEEP_FROM_PTR
	size_in_bytes -= 4;
#endif
	gcollect(size_in_bytes);
	return malloc_untagged(size_in_bytes, nonatomic);
      }
      untagged_low -= 1;
      ((long *)untagged_low)[0] = 0;
    }
  }
#endif

#if SEARCH
  if (size_in_bytes == search_size)
    stop();
#endif

  naya = untagged_low - ((size_in_bytes >> 2) + 1);
  if (naya < tagged_high) {
    gcollect(size_in_bytes);
#if KEEP_FROM_PTR
    size_in_bytes -= 4;
#endif
    return malloc_untagged(size_in_bytes, nonatomic);
  }
  untagged_low = naya;

#if KEEP_FROM_PTR
  *naya = FROM_NEW;
  naya++;
  size_in_bytes -= 4;
#endif

  ((long *)naya)[0] = (size_in_bytes >> 2) | nonatomic;
  
#if SEARCH
  if ((naya + 1) == search_for) {
    stop();
  }
#endif

  {
    long diff = ((char *)(naya + 1) - (char *)GC_alloc_space) >> 2;

    alloc_bitmap[diff >> 3] |= (1 << (diff & 0x7));
  }

  return naya + 1;
}

/* Array of pointers: */
void *GC_malloc(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, 0x80000000);
}

/* Tagged item: */
void *GC_malloc_one_tagged(size_t size_in_bytes)
{
  return malloc_tagged(size_in_bytes);
}

void *GC_malloc_one_xtagged(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, 0x10000000);
}

void *GC_malloc_array_tagged(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, 0x40000000);
}

/* Pointerless */
void *GC_malloc_atomic(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, 0x00000000);
}

/* Plain malloc: */
void *GC_malloc_atomic_uncollectable(size_t size_in_bytes)
{
  return malloc(size_in_bytes);
}

/* Array of pointers: */
void *GC_malloc_allow_interior(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, 0xA0000000);
}

void GC_free(void *s) /* noop */
{
}

void GC_register_traversers(Type_Tag tag, Size_Proc size, Mark_Proc mark, Fixup_Proc fixup)
{
  size_table[tag] = size;
  mark_table[tag] = mark;
  fixup_table[tag] = fixup;
}

void GC_gcollect()
{
  gcollect(0);
}

/*************************************************************/

#if KEEP_FROM_PTR

void GC_print_back_trace(void *p)
{
  while ((p > GC_alloc_space) && (p < GC_alloc_top)) {
    if (p < (void *)tagged_high) {
      printf("%lx = tagged: %d\n", (long)p, *(short *)p);
      p = ((void **)p)[-1];
    } else if (p > (void *)untagged_low) {
      printf("%lx = untagged: %lx\n", (long)p, *(long *)p);
      p = ((void **)p)[-1];
    } else
      break;
  }

  if (p == FROM_STACK)
    printf("stack\n");
  if (p == FROM_ROOT)
    printf("root\n");
  if (p == FROM_FNL)
    printf("fnl\n");
  if (p == FROM_IMM)
    printf("immobile\n");
}
#endif
