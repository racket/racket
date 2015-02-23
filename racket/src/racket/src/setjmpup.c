/*
  Racket
  Copyright (c) 2004-2014 PLT Design Inc.
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
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.
*/

/* Some copilers don't like re-def of GC_malloc in schemef.h: */
#ifndef MZ_PRECISE_GC
# define SCHEME_NO_GC_PROTO
#endif

#include "schpriv.h"
#include "schmach.h"
#include "schgc.h"

#ifdef STACK_GROWS_UP
# define DEEPPOS(b) ((uintptr_t)(b)->stack_from+(uintptr_t)(b)->stack_size)
#else
# define DEEPPOS(b) ((uintptr_t)(b)->stack_from)
#endif

#ifdef MZ_PRECISE_GC
HOOK_SHARED_OK void *(*scheme_get_external_stack_val)(void);
HOOK_SHARED_OK void (*scheme_set_external_stack_val)(void *);
#endif

#ifndef MZ_PRECISE_GC

/**********************************************************************/

/* When we copy the stack, we must set up GC to specially traverse the
   stack copy to account for pointers to the interior of collectable
   objects. */     

extern MZGC_DLLIMPORT void GC_push_all_stack(void *, void *);
extern MZGC_DLLIMPORT void GC_flush_mark_stack(void);
extern MZGC_DLLIMPORT void (*GC_push_last_roots)(void);
extern MZGC_DLLIMPORT void (*GC_push_last_roots_again)(void);
/* GC_push_last_roots_again is called after marking eager
   finalizations (once at each stage). We rely on the fact that no
   copied stack will be referenced by (or affected the ordering of)
   anything non-eagerly finalized.*/

#ifdef USE_SENORA_GC
# define GC_is_marked(p) GC_base(p)
# define GC_did_mark_stack_overflow() 0
#else
extern MZGC_DLLIMPORT int GC_is_marked(void *);
extern MZGC_DLLIMPORT int GC_did_mark_stack_overflow(void);
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
  intptr_t size;
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
  do {
    push_copied_stacks(0);
  } while (scheme_propagate_ephemeron_marks());
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

static void *make_stack_copy_rec(intptr_t size)
{
  CopiedStack *cs, **lk;

  cs = MALLOC_ONE(CopiedStack);
  cs->size = size;
  lk = MALLOC_LINK();
  cs->next = lk;
  lk = MALLOC_LINK();
  cs->prev = lk;


  /* double linked list push */
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

THREAD_LOCAL_DECL(static void *stack_copy_cache[STACK_COPY_CACHE_SIZE]);
THREAD_LOCAL_DECL(static intptr_t stack_copy_size_cache[STACK_COPY_CACHE_SIZE]);
THREAD_LOCAL_DECL(static int scc_pos);
#define SCC_OK_EXTRA_AMT 100

void scheme_flush_stack_copy_cache(void)
  XFORM_SKIP_PROC
{
  int i;
  for (i = 0; i < STACK_COPY_CACHE_SIZE; i++) {
    stack_copy_cache[i] = NULL;
    stack_copy_size_cache[i] = 0;
  }
}

#endif

/**********************************************************************/

#ifdef MZ_PRECISE_GC
# define GC_VAR_STACK_ARG_DECL , void *gc_var_stack_in
# define GC_VAR_STACK_ARG      , __gc_var_stack__
#else
# define GC_VAR_STACK_ARG_DECL /* empty */
# define GC_VAR_STACK_ARG      /* empty */
#endif

/* This function must not be inlined! */
void MZ_NO_INLINE scheme_copy_stack(Scheme_Jumpup_Buf *b, void *base, void *start GC_VAR_STACK_ARG_DECL)
{
  intptr_t size, msize;
  void *here;

  here = &size;

  size = (intptr_t)here XFORM_OK_MINUS (intptr_t)start;
#ifdef STACK_GROWS_UP
  b->stack_from = start;
#else
  size = -size;
  b->stack_from = here;
#endif

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
    uintptr_t diff;
    diff = (uintptr_t)b XFORM_OK_MINUS (uintptr_t)base;
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

MZ_DO_NOT_INLINE(void scheme_uncopy_stack(int ok, Scheme_Jumpup_Buf *b, intptr_t *prev));

void scheme_uncopy_stack(int ok, Scheme_Jumpup_Buf *b, intptr_t *prev)
{
  GC_CAN_IGNORE Scheme_Jumpup_Buf *c;
  intptr_t top_delta = 0, bottom_delta = 0, size;
  void *cfrom, *cto;

  if (!ok) {
    uintptr_t z;
    intptr_t junk[200];

    z = (uintptr_t)&junk[0];

    scheme_uncopy_stack(STK_COMP(z, DEEPPOS(b)), b, junk);
  }

  /* Vague attempt to prevent the compiler from optimizing away `prev': */
  prev[199] = 0;

  FLUSH_REGISTER_WINDOWS;

  START_XFORM_SKIP;
  c = b;
  while (c) {
    size = c->stack_size - top_delta;
    cto = (char *)c->stack_from + bottom_delta;
    cfrom = (char *)get_copy(c->stack_copy) + bottom_delta;

    memcpy(cto, cfrom, size);

    if (c->cont) {
#ifdef STACK_GROWS_UP
      top_delta = (((uintptr_t)c->cont->buf_ptr->buf.stack_from
		    + c->cont->buf.stack_size)
		   - (uintptr_t)c->stack_from);
#else
      bottom_delta = ((uintptr_t)c->stack_from 
		      + c->stack_size
		      - (uintptr_t)c->cont->buf_ptr->buf.stack_from);
      top_delta = bottom_delta;
#endif
      c = &c->cont->buf_ptr->buf;
    } else
      c = NULL;
  }
  END_XFORM_SKIP;

#ifdef MZ_PRECISE_GC
  GC_variable_stack = b->gc_var_stack;
  if (scheme_set_external_stack_val)
    scheme_set_external_stack_val(b->external_stack);
#endif

  scheme_longjmp(b->buf, 1);
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static intptr_t find_same(char *p, char *low, intptr_t max_size)
{
  intptr_t cnt = 0;

  /* We assume a max possible amount of the current stack that should
     not be shared with the saved stack. This is ok (or not) in the same
     sense as assuming that STACK_SAFETY_MARGIN is enough wiggle room to
     prevent stack overflow. */
# define MAX_STACK_DIFF 4096

#ifdef SIXTY_FOUR_BIT_INTEGERS
# define SHARED_STACK_ALIGNMENT 8
#else
# define SHARED_STACK_ALIGNMENT 4
#endif

  if (max_size > MAX_STACK_DIFF) {
    cnt = max_size - MAX_STACK_DIFF;
    max_size = MAX_STACK_DIFF;
  }

#ifdef STACK_GROWS_UP
  while (max_size--) {
    if (p[cnt] != low[cnt])
      break;
    cnt++;
  }
#else
  if (!((intptr_t)p & (sizeof(intptr_t)-1))
      && !((intptr_t)low & (sizeof(intptr_t)-1))) {
    /* common case of aligned addresses: compare `intptr_t`s at a time */
    max_size /= sizeof(intptr_t);
    while (max_size--) {
      if (((intptr_t *)p)[max_size] != ((intptr_t *)low)[max_size])
        break;
      cnt += sizeof(intptr_t);
    }
  } else {
    /* general case: compare bytes */
    while (max_size--) {
      if (p[max_size] != low[max_size])
        break;
      cnt++;
    }
  }
#endif

  if (cnt & (SHARED_STACK_ALIGNMENT - 1)) {
    cnt -= (cnt & (SHARED_STACK_ALIGNMENT - 1));
  }

  return cnt;
}

#ifdef MZ_PRECISE_GC
static void *align_var_stack(void **vs, void *s)
{
  void **nvs, **next;
  intptr_t i, cnt;
  void *a;
  
  while (STK_COMP((uintptr_t)vs, (uintptr_t)s)) {
    vs = (void **)(*vs);
  }

  s = (void *)vs;

  /* Check next few frames to see whether they refer to variables
     before s. This can happen due to inlining, so that an older
     frame is shallower in the stack. It shouldn't happen much,
     though. */
  nvs = *vs;
  while (nvs) {
    next = NULL;
    cnt = ((intptr_t *)nvs)[1];
    for (i = 0; i < cnt; i++) {
      a = nvs[i+2];
      if (!a) {
	a = nvs[i+3];
	i += 2;
      }
      if (STK_COMP((uintptr_t)a, (uintptr_t)s)) {
	/* We need nvs to update part of copied stack! */
	vs = nvs;
	s = (void *)vs;
	next = *nvs;
	break;
      }
    }
    nvs = next;
  }

  return s;
}
#define ALIGN_VAR_STACK(vs, s) s = align_var_stack(vs, s)

static void *shift_var_stack(void *s, intptr_t delta)
{
#ifdef STACK_GROWS_UP
  return s;
#else
  void **vs = (void **)((char *)s + delta);
  intptr_t cnt;
  
  /* Set s past end of vs: */
  cnt = ((intptr_t *)vs)[1];
  return (void *)((void **)s + cnt + 2);
#endif
}
#define PAST_VAR_STACK(s) s = shift_var_stack(s, 0);
#define PAST_VAR_STACK_DELTA(s, d) s = shift_var_stack(s, d);
END_XFORM_SKIP;
#else
# define ALIGN_VAR_STACK(vs, s) /* empty */
# define PAST_VAR_STACK(s) /* empty */
# define PAST_VAR_STACK_DELTA(s, d) /* empty */
#endif

int scheme_setjmpup_relative(Scheme_Jumpup_Buf *b, void *base,
			     void * volatile start, struct Scheme_Cont *c)
{
  int local;
  intptr_t disguised_b;

#ifdef MZ_USE_JIT
  scheme_flush_stack_cache();
#endif

  FLUSH_REGISTER_WINDOWS;

  if (!(local = scheme_setjmp(b->buf))) {
    if (c) {
      /* We'd like to re-use the stack copied for a continuation
	 that encloses the current one --- but we dont' know exactly
	 how much the stack is supposed to be shared, since call/cc
	 is implemented with a trampoline; certainly, the shallowest
	 bit of the old continuation is not right for this one. So,
	 we just start from the deepest part of the stack and find
	 how many bytes match (using find_same)
	 For chains of continuations C1 < C2 < C3, we assume that the 
	 discovered-safe part of C1 to be used for C2 is also valid
	 for C3, so checking for C3 starts with the fresh part in C2,
	 and that's where asymptotic benefits start to kick in. 
         Unfortunately, I can't quite convince myself that this
         assumption is definitely correct. I think it's likely correct,
         but watch out. */
      intptr_t same_size;
      START_XFORM_SKIP;
      same_size = find_same(get_copy(c->buf_ptr->buf.stack_copy), 
                            c->buf_ptr->buf.stack_from, 
                            c->buf_ptr->buf.stack_size);
      b->cont = c;
#ifdef STACK_GROWS_UP
      start = (void *)((char *)c->buf_ptr->buf.stack_from + same_size);
#else
      start = (void *)((char *)c->buf_ptr->buf.stack_from 
                       + (c->buf_ptr->buf.stack_size - same_size));
#endif
      /* In 3m-mode, we need `start' on a var-stack boundary: */
      ALIGN_VAR_STACK(__gc_var_stack__, start);
      END_XFORM_SKIP;
    } else
      b->cont = NULL;

    /* In 3m-mode, we need `start' at the end of the frame */
    PAST_VAR_STACK(start);

    /* b is a pointer into the middle of `base', which bad for precise
     gc, so we hide it. */
    disguised_b = (intptr_t)b;
    b = NULL;

    scheme_copy_stack((Scheme_Jumpup_Buf *)disguised_b, base, start GC_VAR_STACK_ARG);

    /* Precise GC: ensure that this frame is pushed. */
    if (0) {
      base = scheme_malloc(0);
    }

    return 0;
  }

  return local;
}

struct Scheme_Overflow_Jmp *scheme_prune_jmpup(struct Scheme_Overflow_Jmp *jmp, void *stack_boundary)
{
  void *cur_end;

  PAST_VAR_STACK_DELTA(stack_boundary,  (char *)get_copy(jmp->cont.stack_copy) - (char *)jmp->cont.stack_from);

#ifdef STACK_GROWS_UP
  cur_end = (void *)jmp->cont.stack_from;
#else
  cur_end = (void *)((char *)jmp->cont.stack_from + jmp->cont.stack_size);
#endif

  if (stack_boundary != cur_end) {
    intptr_t new_size, delta;
    Scheme_Overflow_Jmp *naya;
    void *copy, *base;

# ifdef STACK_GROWS_UP
    delta = (char *)stack_boundary - (char *)jmp->cont.stack_from;
    new_size = jmp->cont.stack_size - delta;
    base = (char *)stack_boundary;
# else
    delta = 0;
    new_size = (intptr_t)stack_boundary - (intptr_t)jmp->cont.stack_from;
    base = jmp->cont.stack_from;
# endif

    if ((new_size < 0) || (new_size > jmp->cont.stack_size))
      scheme_signal_error("bad C-stack pruigin size: %ld vs. %ld", new_size, jmp->cont.stack_size);

    naya = MALLOC_ONE_RT(Scheme_Overflow_Jmp);
    memcpy(naya, jmp, sizeof(Scheme_Overflow_Jmp));
    scheme_init_jmpup_buf(&naya->cont);
    
#ifndef MZ_PRECISE_GC
    copy = make_stack_copy_rec(new_size);
    naya->cont.stack_copy = copy;
    set_copy(naya->cont.stack_copy, MALLOC_STACK(new_size));
#else
    copy = MALLOC_STACK(new_size);
    set_copy(naya->cont.stack_copy, copy);
#endif
    
    memcpy(get_copy(copy), 
           (char *)get_copy(jmp->cont.stack_copy) XFORM_OK_PLUS delta,
           new_size);

    naya->cont.stack_size = naya->cont.stack_max_size = new_size;
    naya->cont.stack_from = base;

    return naya;
  }

  return NULL;
}

void scheme_longjmpup(Scheme_Jumpup_Buf *b)
{
  intptr_t z;
  intptr_t junk[200];

#ifdef MZ_USE_JIT
  scheme_flush_stack_cache();
#endif

  scheme_uncopy_stack(STK_COMP((uintptr_t)&z, DEEPPOS(b)), b, junk);
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

#ifdef USE_MZ_CYGWIN_SETJMP
/* We have to define setjmp & longjmp to remain compatible
   with MSVC-compiled extensions. It's the mostly same code 
   as mzsj86.c, just in a slightly different syntax. This code
   is fragile, because it's not well defined whether the compiler
   will generate frame-pointer setup; use mzsj86g.S, instead. */

#if (__OPTIMIZE__ > 0) || defined(MZ_XFORM)
# define NEED_STACK_FRAME_SETUP
#endif

MZ_DO_NOT_INLINE(int scheme_mz_setjmp(mz_pre_jmp_buf b));
MZ_DO_NOT_INLINE(void scheme_mz_longjmp(mz_pre_jmp_buf b, int v));

int scheme_mz_setjmp(mz_pre_jmp_buf b)
{
#ifdef NEED_STACK_FRAME_SETUP
  asm("push %EBP");
  asm("mov %ESP, %EBP");
#endif

  asm("mov 4(%EBP), %ECX"); /* return address */
  asm("mov 8(%EBP), %EAX"); /* jmp_buf ptr */
  asm("mov (%EBP), %EDX");  /* old EBP */
  asm("mov %EDX, (%EAX)");
  asm("mov %EBX, 4(%EAX)");
  asm("mov %EDI, 8(%EAX)");
  asm("mov %ESI, 12(%EAX)");
  asm("mov %ESP, 16(%EAX)");
  asm("mov %ECX, 20(%EAX)");

#ifdef NEED_STACK_FRAME_SETUP
  asm("pop %EBP");
#endif

  return 0;
}

void scheme_mz_longjmp(mz_pre_jmp_buf b, int v)
{
#ifdef NEED_STACK_FRAME_SETUP
  asm("push %EBP");
  asm("mov %ESP, %EBP");
#endif

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

#ifdef NEED_STACK_FRAME_SETUP
  asm("pop %EBP");
#endif
}

#endif


#ifndef USE_MZ_SETJMP_INDIRECT
Scheme_Setjmp_Proc scheme_get_mz_setjmp(void)
{
  scheme_log_abort("internal error: setjmp was indirect?");
  abort();
  return NULL;
}
#endif
