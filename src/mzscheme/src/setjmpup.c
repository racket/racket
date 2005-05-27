/*
  MzScheme
  Copyright (c) 2004-2005 PLT Scheme, Inc.
  Copyright (c) 1995-2001 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Some copilers don't like re-def of GC_malloc in schemef.h: */
#ifndef MZ_PRECISE_GC
# define SCHEME_NO_GC_PROTO
#endif

#include "schpriv.h"
#include "schmach.h"
#include "schgc.h"

#ifdef STACK_GROWS_UP
#define DEEPPOS(b) ((unsigned long)(b)->stack_from+(unsigned long)(b)->stack_size)
#else
#ifdef STACK_GROWS_DOWN
#define DEEPPOS(b) ((unsigned long)(b)->stack_from)
#else
#define DEEPPOS(b) ((unsigned long)(b)->stack_from+ \
                 (scheme_stack_grows_up ? (unsigned long)(b)->stack_size : 0))
#endif
#endif

#ifdef memcpy
#undef memcpy
#endif

#ifdef MZ_PRECISE_GC
void *(*scheme_get_external_stack_val)(void);
void (*scheme_set_external_stack_val)(void *);
#endif

#ifndef MZ_PRECISE_GC

/**********************************************************************/

/* When we copy the stack, we must set up GC to specially traverse the
   stack copy to account for pointers to the interior of collectable
   objects. */     

extern MZ_DLLIMPORT void GC_push_all_stack(void *, void *);
extern MZ_DLLIMPORT void GC_flush_mark_stack(void);
extern MZ_DLLIMPORT void (*GC_push_last_roots)(void);
extern MZ_DLLIMPORT void (*GC_push_last_roots_again)(void);
/* GC_push_last_roots_again is called after marking eager
   finalizations (once at each stage). We rely on the fact that no
   copied stack will be referenced by (or affected the ordering of)
   anything non-eagerly finalized.*/

#ifdef USE_SENORA_GC
# define GC_is_marked(p) GC_base(p)
# define GC_did_mark_stack_overflow() 0
#else
extern MZ_DLLIMPORT int GC_is_marked(void *);
extern MZ_DLLIMPORT int GC_did_mark_stack_overflow(void);
#endif

#define get_copy(s_c) (((CopiedStack *)s_c)->_stack_copy)

#define MALLOC_LINK() MALLOC_ONE_WEAK(CopiedStack*)
#ifdef USE_TAGGED_ALLOCATION
extern void *scheme_malloc_stack(size_t);
# define MALLOC_STACK(size) scheme_malloc_stack(size)
#else
# define MALLOC_STACK(size) scheme_malloc_atomic(size)
#endif

typedef struct CopiedStack {
  void *_stack_copy; /* The actual data */
  long size;
  int pushed;
  struct CopiedStack **next, **prev;
} CopiedStack;

static CopiedStack **first_copied_stack;
int scheme_num_copied_stacks = 0;

static void push_copied_stacks(int init)
{
  /* This is called after everything else is marked.
     Mark from those stacks that are still reachable. If
     we mark from a stack, we need to go back though the list
     all over to check the previously unmarked stacks. */
  CopiedStack *cs;
  int pushed_one;

  if (init) {
    for (cs = *first_copied_stack; cs; cs = *cs->next) {
      if (get_copy(cs))
	cs->pushed = 0;
      else
	cs->pushed = 1;
    }
  }

  GC_flush_mark_stack();

  do {
    pushed_one = 0;
    for (cs = *first_copied_stack; cs; cs = *cs->next) {
      if (!cs->pushed && GC_is_marked(get_copy(cs))) {
	pushed_one = 1;
	cs->pushed = 1;
	GC_push_all_stack(get_copy(cs), (char *)get_copy(cs) + cs->size);
	if (GC_did_mark_stack_overflow()) {
	  /* printf("mark stack overflow\n"); */
	  return;
	} else {
	  GC_flush_mark_stack();
	  if (GC_did_mark_stack_overflow()) {
	    /* printf("mark stack overflow (late)\n"); */
	    return;
	  }
	}
      }
    }
  } while (pushed_one);
}

static void init_push_copied_stacks(void)
{
  push_copied_stacks(1);
}

static void update_push_copied_stacks(void)
{
  push_copied_stacks(0);
}

void scheme_init_setjumpup(void)
{
  if (scheme_starting_up) {
    REGISTER_SO(first_copied_stack);
  }
  first_copied_stack = MALLOC_LINK();
  *first_copied_stack = NULL;

  GC_push_last_roots = init_push_copied_stacks;
  GC_push_last_roots_again = update_push_copied_stacks;
}

static void remove_cs(void *_cs, void *unused)
{
  CopiedStack *cs = (CopiedStack *)_cs;

  if (*cs->prev)
    *(*cs->prev)->next = *cs->next;
  else
    *first_copied_stack = *cs->next;

  if (*cs->next)
    *(*cs->next)->prev = *cs->prev;

  if (cs->_stack_copy) {
#ifndef SGC_STD_DEBUGGING
    GC_free(cs->_stack_copy);
#else
    memset(cs->_stack_copy, 0, cs->size);
#endif
    cs->_stack_copy = NULL;
  }

  --scheme_num_copied_stacks;
}

static void *make_stack_copy_rec(long size)
{
  CopiedStack *cs, **lk;

  cs = MALLOC_ONE(CopiedStack);
  cs->size = size;
  lk = MALLOC_LINK();
  cs->next = lk;
  lk = MALLOC_LINK();
  cs->prev = lk;

  *cs->next = *first_copied_stack;
  if (*first_copied_stack)
    *(*first_copied_stack)->prev = cs;
  *cs->prev = NULL;
  *first_copied_stack = cs;

  GC_register_finalizer(cs, remove_cs, NULL, NULL, NULL);

  scheme_num_copied_stacks++;

  return (void *)cs;
}

static void set_copy(void *s_c, void *c)
{
  CopiedStack *cs = (CopiedStack *)s_c;

  cs->_stack_copy = c;
}

/**********************************************************************/

#else

/* Precise GC: */
# define MALLOC_STACK(size) scheme_malloc_atomic(size)
# define get_copy(s_c) (s_c)
# define set_copy(s_c, c) s_c = c

#define STACK_COPY_CACHE_SIZE 10
static void *stack_copy_cache[STACK_COPY_CACHE_SIZE];
static long stack_copy_size_cache[STACK_COPY_CACHE_SIZE];
static int scc_pos;
#define SCC_OK_EXTRA_AMT 100

START_XFORM_SKIP;

void scheme_flush_stack_copy_cache(void)
{
  int i;
  for (i = 0; i < STACK_COPY_CACHE_SIZE; i++) {
    stack_copy_cache[i] = NULL;
    stack_copy_size_cache[i] = 0;
  }
}

END_XFORM_SKIP;

#endif

/**********************************************************************/

#define memcpy(dd, ss, ll) \
{  stack_val *d, *s; long l; \
   l = ll / sizeof(stack_val); d = (stack_val *)dd; s = (stack_val *)ss; \
   while (l--) { *(d++) = *(s++);} }

#ifdef MZ_PRECISE_GC
# define GC_VAR_STACK_ARG_DECL , void *gc_var_stack_in
# define GC_VAR_STACK_ARG      , __gc_var_stack__
#else
# define GC_VAR_STACK_ARG_DECL /* empty */
# define GC_VAR_STACK_ARG      /* empty */
#endif

static void copy_stack(Scheme_Jumpup_Buf *b, void *base, void *start GC_VAR_STACK_ARG_DECL)
{
  long size, msize;
  void *here;

  here = &size;

  size = (long)here XFORM_OK_MINUS (long)start;
  if (scheme_stack_grows_up) {
    b->stack_from = start;
  } else {
    size = -size;
    b->stack_from = here;
  }

  if (size < 0)
    size = 0;

  msize = size;

  if (b->stack_max_size < size) {
    /* printf("Stack size: %d\n", size); */
    void *copy;
#ifndef MZ_PRECISE_GC
    copy = make_stack_copy_rec(size);
    b->stack_copy = copy;
    set_copy(b->stack_copy, MALLOC_STACK(size));
#else
    /* b is a pointer into the middle of `base'; bad for precise gc: */
    unsigned long diff;
    diff = (unsigned long)b XFORM_OK_MINUS (unsigned long)base;
    b = NULL;

    copy = NULL;
    /* Look for a reusable freed block: */
    {
      int i;
      for (i = 0; i < STACK_COPY_CACHE_SIZE; i++) {
	if ((stack_copy_size_cache[i] >= size)
	    && (stack_copy_size_cache[i] < (size + SCC_OK_EXTRA_AMT))) {
	  /* Found one */
	  copy = stack_copy_cache[i];
	  msize = stack_copy_size_cache[i];
	  stack_copy_cache[i] = NULL;
	  stack_copy_size_cache[i] = 0;
	  break;
	}
      }
    }
    if (!copy) {
      /* No reusable block found */
      copy = MALLOC_STACK(size);
    }

    /* Restore b: */
    b = (Scheme_Jumpup_Buf *)(((char *)base) XFORM_OK_PLUS diff);

    set_copy(b->stack_copy, copy);
#endif
    b->stack_max_size = msize;
  }
  b->stack_size = size;

#ifdef MZ_PRECISE_GC
  b->gc_var_stack = gc_var_stack_in;
  if (scheme_get_external_stack_val) {
    void *es;
    es = scheme_get_external_stack_val();
    b->external_stack = es;
  }
#endif
  
  memcpy(get_copy(b->stack_copy),
	 b->stack_from,
	 size);
}

static void uncopy_stack(int ok, Scheme_Jumpup_Buf *b, long *prev)
{
  Scheme_Jumpup_Buf *c;

  if (!ok) {
    unsigned long z;
    long junk[200];

    z = (unsigned long)&junk[0];

    uncopy_stack(STK_COMP(z, DEEPPOS(b)), b, junk);
  }

  {
    int i;
    for (i = 0; i < 200; i++) {
      prev[i] = 0;
    }
  }

  FLUSH_REGISTER_WINDOWS;

  START_XFORM_SKIP;
  c = b;
  while (c) {
    memcpy(c->stack_from,
	   get_copy(c->stack_copy),
	   c->stack_size);
    c = c->cont;
  }
  END_XFORM_SKIP;

#ifdef MZ_PRECISE_GC
  GC_variable_stack = b->gc_var_stack;
  if (scheme_set_external_stack_val)
    scheme_set_external_stack_val(b->external_stack);
#endif

  scheme_longjmp(b->buf, 1);
}

int scheme_setjmpup_relative(Scheme_Jumpup_Buf *b, void *base,
			     void * volatile start, Scheme_Jumpup_Buf *c)
{
  int local;
  long disguised_b;

  FLUSH_REGISTER_WINDOWS;

  if (STK_COMP((unsigned long)start, (unsigned long)&local))
    start = (void *)&local;

  if (!(local = scheme_setjmp(b->buf))) {
    if (c) {
      b->cont = c;
      if (scheme_stack_grows_up) {
	start = (void *)((char *)c->stack_from + c->stack_size);
      } else {
	start = c->stack_from;
      }
    } else
      b->cont = NULL;


    /* b is a pointer into the middle of `base', which bad for precise
     gc, so we hide it. */
    disguised_b = (long)b;
    b = NULL;

    copy_stack((Scheme_Jumpup_Buf *)disguised_b, base, start GC_VAR_STACK_ARG);

    /* Precise GC: ensure that this frame is pushed. */
    if (0) {
      base = scheme_malloc(0);
    }

    return 0;
  }

  return local;
}

void scheme_longjmpup(Scheme_Jumpup_Buf *b)
{
  long z;
  long junk[200];

  uncopy_stack(STK_COMP((unsigned long)&z, DEEPPOS(b)), b, junk);
}

void scheme_init_jmpup_buf(Scheme_Jumpup_Buf *b)
{
  b->stack_size = b->stack_max_size = 0;
  b->stack_from = b->stack_copy = NULL;
}

void scheme_reset_jmpup_buf(Scheme_Jumpup_Buf *b)
{
  if (b->stack_copy) {
#ifdef MZ_PRECISE_GC
    /* "Free" the stack copy by putting it into a cache.
       (We clear the cache before a GC.) */
    stack_copy_cache[scc_pos] = b->stack_copy;
    stack_copy_size_cache[scc_pos] = b->stack_max_size;
    scc_pos++;
    if (scc_pos == STACK_COPY_CACHE_SIZE)
      scc_pos = 0;
#else
    /* Drop the copy of the stack, */
    /* remove the finalizer, */
    /* and explicitly call the finalization proc */
    GC_register_finalizer(b->stack_copy, NULL, NULL, NULL, NULL);
    remove_cs(b->stack_copy, NULL);
#endif

    scheme_init_jmpup_buf(b);
  }

  memset(&b->buf, 0, sizeof(mz_jmp_buf));
}

void scheme_ensure_stack_start(Scheme_Thread *p, void *d)
{
  if (!p->stack_start 
      || (STK_COMP((unsigned long)p->stack_start, (unsigned long)d)))
    p->stack_start = d;
}

#ifdef USE_MZ_CYGWIN_SETJMP
/* We have to define setjmp & longjmp to remain compatible
   with MSVC-compiled extensions. It's the mostly same code 
   as mzsj86.c, just in a slightly different syntax, and it
   probably only works with -O2. */

int scheme_mz_setjmp(mz_jmp_buf b)
{
  asm("mov 4(%EBP), %ECX"); /* return address */
  asm("mov 8(%EBP), %EAX"); /* jmp_buf ptr */
  asm("mov (%EBP), %EDX");  /* old EBP */
  asm("mov %EDX, (%EAX)");
  asm("mov %EBX, 4(%EAX)");
  asm("mov %EDI, 8(%EAX)");
  asm("mov %ESI, 12(%EAX)");
  asm("mov %ESP, 16(%EAX)");
  asm("mov %ECX, 20(%EAX)");
 
  return 0;
}

void scheme_mz_longjmp(mz_jmp_buf b, int v)
{
  asm("mov 12(%EBP), %EAX"); /* return value */
  asm("mov 8(%EBP), %ECX");  /* jmp_buf */
  asm("mov 16(%ECX), %ESP"); /* restore stack pointer */
  asm("mov (%ECX), %EBP");   /* old EBP */
  asm("mov %EBP, (%ESP)");
  asm("mov %ESP, %EBP");
  asm("mov 4(%ECX), %EBX");
  asm("mov 8(%ECX), %EDI");
  asm("mov 12(%ECX), %ESI");
  asm("mov 20(%ECX), %ECX"); /* return address */
  asm("mov %ECX, 4(%EBP)");
}

#endif
