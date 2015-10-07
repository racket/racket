/*
  Racket
  Copyright (c) 2006-2015 PLT Design Inc.

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

#include "schpriv.h"
#include "schmach.h"
#ifdef MZ_USE_DWARF_LIBUNWIND
# include "unwind/libunwind.h"
#endif
#include "future.h"

#ifdef MZ_USE_JIT

#include "jit.h"

#include "codetab.inc"

/* The Stack_Cache_Elem structure type (define in schthread.h)
   must have a size of  4 words. */

THREAD_LOCAL_DECL(static Stack_Cache_Elem stack_cache_stack[STACK_CACHE_SIZE]);
THREAD_LOCAL_DECL(static intptr_t stack_cache_stack_pos = 0);

void *scheme_decrement_cache_stack_pos(void *p)
{
  Stack_Cache_Elem *r;
  r = stack_cache_stack + stack_cache_stack_pos;
  stack_cache_stack_pos--;
  r->orig_result = p;
  return r;
}

void scheme_register_stack_cache_stack(void)
{
  REGISTER_SO(stack_cache_stack);
}

/*========================================================================*/
/*                              stack trace                               */
/*========================================================================*/

typedef void *(*Get_Stack_Proc)();

#ifdef MZ_USE_JIT_PPC
# define NEXT_FRAME_OFFSET 0
# ifdef _CALL_DARWIN
#  define RETURN_ADDRESS_OFFSET 2
# else
#  define RETURN_ADDRESS_OFFSET 1
# endif
#endif
#ifdef MZ_USE_JIT_I386
# define NEXT_FRAME_OFFSET 0
# define RETURN_ADDRESS_OFFSET 1
#endif
#ifdef MZ_USE_JIT_ARM
# define NEXT_FRAME_OFFSET JIT_NEXT_FP_OFFSET
# define RETURN_ADDRESS_OFFSET (JIT_NEXT_FP_OFFSET+1)
#endif

#define CACHE_STACK_MIN_TRIGGER 128

/* Normally, caching of a native trace on the stack ensures that
   `current-continuation-marks' is effectively constant-time. 
   It's possible to have a deep stack section with no recognizable anchors
   for caching, however, in which case `current-continuation-marks'
   can become O(n); avoid that pathological case by limiting the
   number of non-anchor frames in a row that we're willing to traverse. */
#define UNKNOWN_FRAME_LIMIT 64

#define USE_STACK_CHECK 0

#if USE_STACK_CHECK
static void check_stack(void)
{
  void *p, *q;
  uintptr_t stack_end;
  int pos = stack_cache_stack_pos;
  Get_Stack_Proc gs;

  gs = (Get_Stack_Proc)get_stack_pointer_code;
  p = gs();

  stack_end = (uintptr_t)(scheme_current_thread->next 
			      ? scheme_current_thread->stack_start 
			      : scheme_current_thread->o_start);

  while (STK_COMP((uintptr_t)p, stack_end)) {
    q = ((void **)p)[RETURN_ADDRESS_OFFSET];

    if (q == stack_cache_pop_code) {
      if (!pos)
	abort();
      else {
	if (stack_cache_stack[pos].stack_frame != (void *)(((void **)p) + RETURN_ADDRESS_OFFSET)) {
	  abort();
	}
	--pos;
      }
    }

    q = ((void **)p)[NEXT_FRAME_OFFSET];
    if (STK_COMP((uintptr_t)q, (uintptr_t)p))
      break;
    p = q;
  }
}
#endif

MZ_DO_NOT_INLINE(uintptr_t scheme_approx_sp());
uintptr_t scheme_approx_sp()
{
  uintptr_t p;
  p = (uintptr_t)&p;
  return p;
}

#if defined( _WIN64) && !defined(__MINGW32__)
# define USE_WIN64_UNWIND
# ifndef UNWIND_HISTORY_TABLE_SIZE
extern PRUNTIME_FUNCTION WINAPI RtlLookupFunctionEntry(ULONG64, ULONG64*, void*);
extern PVOID WINAPI RtlVirtualUnwind(DWORD, DWORD64, DWORD64, PRUNTIME_FUNCTION,
				     PCONTEXT, PVOID, PDWORD64, PVOID);
# endif
#endif

static void set_cache(void *p, Scheme_Object *last)
{
  int pos;

  if (stack_cache_stack_pos >= (STACK_CACHE_SIZE - 1)) {
    /* Make room on the stack */
    void **z;
    z = (void **)stack_cache_stack[stack_cache_stack_pos].stack_frame;
    *z = stack_cache_stack[stack_cache_stack_pos].orig_return_address;
    --stack_cache_stack_pos;
  }

  pos = (int)++stack_cache_stack_pos;
  stack_cache_stack[pos].orig_return_address = ((void **)p)[RETURN_ADDRESS_OFFSET];
  stack_cache_stack[pos].stack_frame = (void *)(((void **)p) + RETURN_ADDRESS_OFFSET);
  stack_cache_stack[pos].cache = last;
  ((void **)p)[RETURN_ADDRESS_OFFSET] = sjc.stack_cache_pop_code;
}

Scheme_Object *scheme_native_stack_trace(void)
{
  void *p, *q, *cache_frame_p;
  uintptr_t stack_end, real_stack_end, stack_start, halfway;
  Scheme_Object *name, *last = NULL, *first = NULL, *tail;
  int cache_had_name = 0;
#ifdef MZ_USE_DWARF_LIBUNWIND
  unw_context_t cx;
  unw_cursor_t c;
  int manual_unw = 0;
  unw_word_t stack_addr;
#else
  Get_Stack_Proc gs;
#endif
  int use_unw = 0;
  int shift_cache_to_next = 0;
  int added_list_elem;
  int unsuccess = 0;

  if (!sjc.get_stack_pointer_code)
    return NULL;

#if USE_STACK_CHECK
  check_stack();
#endif

  stack_start = scheme_approx_sp();

  real_stack_end = (uintptr_t)scheme_current_thread->stack_start;
  if (stack_cache_stack_pos) {
    stack_end = (uintptr_t)stack_cache_stack[stack_cache_stack_pos].stack_frame;
    stack_end -= (RETURN_ADDRESS_OFFSET << JIT_LOG_WORD_SIZE);
    tail = stack_cache_stack[stack_cache_stack_pos].cache;
  } else {
    stack_end = real_stack_end;
    tail = scheme_null;
  }

#ifdef MZ_USE_DWARF_LIBUNWIND
  unw_getcontext(&cx);
  unw_init_local(&c, &cx);
  unw_set_safe_pointer_range(&c, stack_start, stack_end);
  use_unw = 1;
  p = NULL;
#else
  gs = (Get_Stack_Proc)sjc.get_stack_pointer_code;
  p = gs();
#endif

  halfway = STK_DIFF(stack_end, (uintptr_t)stack_start) / 2;
  if (halfway < CACHE_STACK_MIN_TRIGGER)
    halfway = stack_end;
  else {
#ifdef STACK_GROWS_DOWN
    halfway += (uintptr_t)stack_start;
#else
    halfway += stack_end;
#endif
  }

#ifdef USE_WIN64_UNWIND
  {
    CONTEXT ctx;
    PRUNTIME_FUNCTION rf;
    ULONG64 base, ef;
    void *data, *cache_sp = NULL;

    RtlCaptureContext(&ctx);
    while (unsuccess < UNKNOWN_FRAME_LIMIT) {
      name = find_symbol((uintptr_t)ctx.Rip);
      if (name) {
	/* Unwind manually */
	uintptr_t *fp = (uintptr_t *)ctx.Rbp;
        if (!(STK_COMP((uintptr_t)fp, stack_end)
              && STK_COMP(stack_start, (uintptr_t)fp))) {
          /* out-of-range frame pointer; give up */
          break;
        } else if (SCHEME_FALSEP(name) || SCHEME_VOIDP(name)) {
	  /* "quick" call convention */
	  if (SCHEME_VOIDP(name)) {
	    /* JIT_LOCAL2 has the next return address */
	    ctx.Rip = fp[JIT_LOCAL2 >> JIT_LOG_WORD_SIZE];
	  } else {
	    /* Push after local stack of return-address proc
	       may have the next return address */
	    ctx.Rip = fp[-(3 + LOCAL_FRAME_SIZE + 1)];
	  }
	  name = find_symbol((uintptr_t)ctx.Rip);
	} else {
	  /* normal JIT function convention */
	}

	cache_sp = (void *)fp;

	if (SCHEME_EOFP(name)) {
	  /* JIT_LOCAL2 has the name to use */
	  name = *(Scheme_Object **)fp[JIT_LOCAL2 >> JIT_LOG_WORD_SIZE];
	}

	ctx.Rsp = ctx.Rbp + (2 * sizeof(void*));
# ifdef NEED_LOCAL4
	ctx.R14 = fp[-JIT_LOCAL4_OFFSET];
# endif
	ctx.Rbp = fp[0];
	ctx.Rbx = fp[-1];
	ctx.Rsi = fp[-2];
	ctx.Rdi = fp[-3];
	ctx.Rip = fp[1];

	if (SCHEME_NULLP(name))
	  name = NULL;
      } else {
	unsuccess++;
	rf = RtlLookupFunctionEntry(ctx.Rip, &base, NULL);
	if (rf) {
	  RtlVirtualUnwind(0x0, base, ctx.Rip,
			   rf, &ctx, &data, &ef, NULL);
	} else {
	  break;
	}
      }

      if (name) {
	name = scheme_make_pair(name, scheme_null);
	if (last)
	  SCHEME_CDR(last) = name;
	else
	  first = name;
	last = name;
	if (shift_cache_to_next) {
	  stack_cache_stack[stack_cache_stack_pos].cache = last;
	  shift_cache_to_next = 0;
	}
      }

      if (cache_sp) {
	if (STK_COMP((uintptr_t)halfway, (uintptr_t)cache_sp)) {
	  set_cache(cache_sp, last);
	  if (!name)
	    shift_cache_to_next = 1;
	  halfway = stack_end;
	  unsuccess = -100000; /* if we got halfway, no need to bail out later */
	}
	cache_sp = NULL;
      }

      if (!(STK_COMP((uintptr_t)ctx.Rsp, stack_end)
	    && STK_COMP(stack_start, (uintptr_t)ctx.Rsp))) {
	/* out of stack range */
	break;
      }
    }

    if (shift_cache_to_next)
      stack_cache_stack[stack_cache_stack_pos].cache = tail;

    if (last)
      SCHEME_CDR(last) = tail;
    else
      first = tail;
    
    if (SCHEME_NULLP(first))
      return NULL;
    else
      return first;
  }
#endif

  cache_frame_p = NULL;

  while (unsuccess < UNKNOWN_FRAME_LIMIT) {
#ifdef MZ_USE_DWARF_LIBUNWIND
    if (use_unw)
      q = (void *)unw_get_ip(&c);
    else
      q = NULL;
#endif

    if (!use_unw) {
      if (!(STK_COMP((uintptr_t)p, stack_end)
	    && STK_COMP(stack_start, (uintptr_t)p)))
	break;
      q = ((void **)p)[RETURN_ADDRESS_OFFSET];
      /* Except on PPC, p is the frame pointer for the function called
	 by q, not for q. */
    }

    name = find_symbol((uintptr_t)q);
#ifdef MZ_USE_DWARF_LIBUNWIND
    if (name && !manual_unw)
      manual_unw = 1;
#endif

    if (SCHEME_FALSEP(name) || SCHEME_VOIDP(name)) {
      /* The function `q' was reached through a lighter-weight
         internal ABI; we want the name of the function that
         called `q' */
      void *np;
#ifdef MZ_USE_JIT_PPC
      np = p;
#else
      /* Since `p' is the frame pointer for the function called
         by `q', so we need to step one more frame to look
         at the frame for `q': */
# ifdef MZ_USE_DWARF_LIBUNWIND
      if (use_unw) {
	np = (void *)unw_get_frame_pointer(&c);
      } else
# endif
	np = ((void **)p)[NEXT_FRAME_OFFSET];
      /* `np' is now the frame pointer for `q',
	 so we can find the actual `q' */
#endif

      if (STK_COMP((uintptr_t)np, real_stack_end)
	  && STK_COMP(stack_start, (uintptr_t)np)) {
	if (SCHEME_VOIDP(name)) {
	  /* JIT_LOCAL2 has the next return address (always) */
	  q = ((void **)np)[JIT_LOCAL2 >> JIT_LOG_WORD_SIZE];
	} else {
#ifdef MZ_USE_JIT_I386
	  /* Push after local stack of return-address proc
	     has the next return address */
	  q = ((void **)np)[-(3 + LOCAL_FRAME_SIZE + 1)];
#else
          /* JIT_LOCAL2 has the next return address */
          q = ((void **)np)[JIT_LOCAL2 >> JIT_LOG_WORD_SIZE];
#endif
	}
      } else {
	q = NULL;
      }
      name = find_symbol((uintptr_t)q);
    } else if (SCHEME_EOFP(name)) {
      /* Stub (to mark start of running a module body, for example);
         JIT_LOCAL2 has the name to use. */
      void *np;
#ifdef MZ_USE_JIT_PPC
      np = p;
#else
      /* Need to step a frame, as above. */
# ifdef MZ_USE_DWARF_LIBUNWIND
      if (use_unw) {
	np = (void *)unw_get_frame_pointer(&c);
      } else
# endif
	np = ((void **)p)[NEXT_FRAME_OFFSET];
#endif

      if (STK_COMP((uintptr_t)np, real_stack_end)
	  && STK_COMP(stack_start, (uintptr_t)np)) {
        name = *(Scheme_Object **)((void **)np)[JIT_LOCAL2 >> JIT_LOG_WORD_SIZE];
      } else
        name = NULL;
    }

#ifdef MZ_USE_DWARF_LIBUNWIND
    if (use_unw) {
      if (manual_unw) {
	cache_frame_p = (void **)unw_get_frame_pointer(&c);
	if (!(STK_COMP((uintptr_t)cache_frame_p, stack_end)
	      && STK_COMP(stack_start, (uintptr_t)cache_frame_p)))
	  break;
	cache_had_name = name && (last || !SCHEME_NULLP(name));
      } else {
	cache_frame_p = NULL;
	cache_had_name = 0;
      }
    }
#endif

    if (name && !SCHEME_NULLP(name)) { /* null is used to help unwind without a true name */
      name = scheme_make_pair(name, scheme_null);
      if (last)
	SCHEME_CDR(last) = name;
      else
	first = name;
      last = name;
      if (shift_cache_to_next) {
        stack_cache_stack[stack_cache_stack_pos].cache = last;
        shift_cache_to_next = 0;
      }
      added_list_elem = 1;
    } else
      added_list_elem = 0;

    if (!name)
      unsuccess++;
    else
      unsuccess = 0;

    /* Cache the result halfway up the stack, if possible. Only cache
       on frames where the previous frame had a return address with a
       name, because an arbitrary frame's return address on the stack
       might not be used (depending on how the C compiler optimized the
       code); any frame whose procedure has a name is JITted code, so
       it will use the return address from the stack. */
    if (STK_COMP((uintptr_t)halfway, (uintptr_t)cache_frame_p)
	&& cache_had_name) {
      set_cache(cache_frame_p, last);
      if (!added_list_elem)
	shift_cache_to_next = 1;
      halfway = stack_end;
      unsuccess = -8 * UNKNOWN_FRAME_LIMIT; /* if we got halfway, less likely to bail out later */
    }

    if (!use_unw)
      cache_had_name = !!name;
    
#ifdef MZ_USE_DWARF_LIBUNWIND
    if (use_unw) {
      if (manual_unw) {
        /* A JIT-generated function, so we unwind ourselves... */
	void **pp;
	pp = (void **)unw_get_frame_pointer(&c);
	if (!(STK_COMP((uintptr_t)pp, stack_end)
	      && STK_COMP(stack_start, (uintptr_t)pp)))
	  break;
# ifdef MZ_USE_JIT_ARM
        stack_addr = (unw_word_t)&(pp[JIT_NEXT_FP_OFFSET+2]);
	unw_manual_step(&c, 
                        &pp[RETURN_ADDRESS_OFFSET], &stack_addr,
                        &pp[0], &pp[1], &pp[2], &pp[3],
                        &pp[4], &pp[5], &pp[6], &pp[7],
                        &pp[NEXT_FRAME_OFFSET]);
# else
	stack_addr = (unw_word_t)&(pp[RETURN_ADDRESS_OFFSET+1]);
	unw_manual_step(&c, &pp[RETURN_ADDRESS_OFFSET], &pp[0],
			&stack_addr, &pp[-1], &pp[-2], &pp[-3]);
# endif
	manual_unw = 0;
      } else {
        unw_step(&c);
        q = (void *)unw_get_ip(&c);
        if (unw_reset_bad_ptr_flag(&c))
          break;
      }
    }
#endif

    if (!use_unw) {
      q = ((void **)p)[NEXT_FRAME_OFFSET];
      if (STK_COMP((uintptr_t)q, (uintptr_t)p))
        break;
      p = q;
      cache_frame_p = p;
    }
  }

  if (shift_cache_to_next)
    stack_cache_stack[stack_cache_stack_pos].cache = tail;

#ifdef MZ_USE_DWARF_LIBUNWIND
  unw_destroy_local(&c);
#endif

  if (last)
    SCHEME_CDR(last) = tail;
  else
    first = tail;

  if (SCHEME_NULLP(first))
    return NULL;

  return first;
}

#if 0
/* Sometimes useful for debugging Racket: */
void scheme_dump_stack_trace(void)
{
  void *p, *q;
  uintptr_t stack_end, stack_start;
  Get_Stack_Proc gs;
  Scheme_Object *name;

  gs = (Get_Stack_Proc)sjc.get_stack_pointer_code;
  p = gs();
  stack_start = scheme_approx_sp();

  stack_end = (uintptr_t)scheme_current_thread->stack_start;

  while (STK_COMP((uintptr_t)p, stack_end)
         && STK_COMP(stack_start, (uintptr_t)p)) {
    name = find_symbol((uintptr_t)q);
    if (SCHEME_FALSEP(name)) {
      /* Code uses special calling convention */
#ifdef MZ_USE_JIT_PPC
      /* JIT_LOCAL2 has the next return address */
      q = ((void **)p)[JIT_LOCAL2 >> JIT_LOG_WORD_SIZE];
#endif
#ifdef MZ_USE_JIT_I386
      /* Push after local stack of return-address proc
	 has the next return address */
      q = *(void **)p;
      q = ((void **)q)[-(3 + LOCAL_FRAME_SIZE + 1)];
#endif
      name = find_symbol((uintptr_t)q);
    }

    if (name) {
      printf(" scheme\n");
    } else {
      printf(" %p\n", q);
    }

    q = *(void **)p;
    if (STK_COMP((uintptr_t)q, (uintptr_t)p))
      break;
    p = q;
  }
}
#endif

void scheme_flush_stack_cache()
  XFORM_SKIP_PROC
{
  void **p;

  while (stack_cache_stack_pos) {
    p = (void **)stack_cache_stack[stack_cache_stack_pos].stack_frame;
    *p = stack_cache_stack[stack_cache_stack_pos].orig_return_address;
    --stack_cache_stack_pos;
  }
}

void scheme_jit_longjmp(mz_jit_jmp_buf b, int v)
  XFORM_SKIP_PROC
{
  uintptr_t limit;
  void **p;

  limit = b->stack_frame;

  while (stack_cache_stack_pos
	 && STK_COMP((uintptr_t)stack_cache_stack[stack_cache_stack_pos].stack_frame,
		     limit)) {
    p = (void **)stack_cache_stack[stack_cache_stack_pos].stack_frame;
    *p = stack_cache_stack[stack_cache_stack_pos].orig_return_address;
    --stack_cache_stack_pos;
  }

  scheme_mz_longjmp(b->jb, v);
}

void scheme_jit_setjmp_prepare(mz_jit_jmp_buf b)
  XFORM_SKIP_PROC
{
  void *p;
  p = &p;
  b->stack_frame = (uintptr_t)p;
}

void scheme_clean_native_symtab(void)
{
  clear_symbols_for_collected();
#ifndef MZ_PRECISE_GC
  jit_notify_freed_code();
#endif
}

#ifdef MZ_PRECISE_GC
void scheme_jit_release_native_code(void *fnlized, void *p)
{
  Scheme_Object *len;

  len = SCHEME_BOX_VAL(fnlized);

  scheme_jit_malloced -= SCHEME_INT_VAL(len);

# if !defined(PLT_DUMP_JIT_RANGES)
  /* Remove name mapping: */
  scheme_jit_add_symbol((uintptr_t)p, (uintptr_t)p + SCHEME_INT_VAL(len), NULL, 1);
  /* Free memory: */
  scheme_free_code(p);
# endif

  jit_notify_freed_code();
}
#endif

void* scheme_jit_find_code_end(void *_p)
{
  uintptr_t p = (uintptr_t)_p;
  uintptr_t hi, lo, mid;
  void *n;

  n = find_symbol(p);
  if (n) {
    /* find overesitinate of ending point: */
    hi = 1;
    while (find_symbol(p+hi) == n) {
      hi = hi*2;
      if (p + hi < p) {
        /* this shouldn't happen, but if something has gone really wrong,
           we don't want to loop forever */
        return NULL;
      }
    }
    /* binary search for precise ending point: */
    lo = hi / 2;
    while (lo+1 < hi) {
      mid = lo + (((hi - lo) + 1) / 2);
      if (find_symbol(p+mid) == n)
        lo = mid;
      else
        hi = mid;
    }
    return (void *)(p+hi);
  } else
    return NULL;
}

void scheme_jit_now(Scheme_Object *f)
{
  if (SAME_TYPE(SCHEME_TYPE(f), scheme_native_closure_type)) {
    Scheme_Native_Closure *nc;
    Scheme_Native_Closure_Data *ncd;

    nc = (Scheme_Native_Closure*)f;
    ncd = nc->code;
    if (ncd->start_code == scheme_on_demand_jit_code)
      scheme_on_demand_generate_lambda(nc, 0, NULL, 0);
  }
}


typedef void *(*Module_Run_Proc)(Scheme_Env *menv, Scheme_Env *env, Scheme_Object **name);
typedef void *(*Module_Exprun_Proc)(Scheme_Env *menv, int set_ns, Scheme_Object **name);
typedef void *(*Module_Start_Proc)(struct Start_Module_Args *a, Scheme_Object **name);
typedef void (*Thread_Start_Child_Proc)(Scheme_Thread *child, Scheme_Object *child_thunk);

void *scheme_module_run_start(Scheme_Env *menv, Scheme_Env *env, Scheme_Object *name)
{
  Module_Run_Proc proc = (Module_Run_Proc)sjc.module_run_start_code;
  if (proc)
    return proc(menv, env, &name);
  else
    return scheme_module_run_finish(menv, env);
}

void *scheme_module_exprun_start(Scheme_Env *menv, int set_ns, Scheme_Object *name)
{
  Module_Exprun_Proc proc = (Module_Exprun_Proc)sjc.module_exprun_start_code;
  if (proc)
    return proc(menv, set_ns, &name);
  else
    return scheme_module_exprun_finish(menv, set_ns);
}

void *scheme_module_start_start(struct Start_Module_Args *a, Scheme_Object *name)
{
  Module_Start_Proc proc = (Module_Start_Proc)sjc.module_start_start_code;
  if (proc)
    return proc(a, &name);
  else
    return scheme_module_start_finish(a);
}

void scheme_thread_start_child(Scheme_Thread *child, Scheme_Object *child_thunk)
  XFORM_SKIP_PROC
{
  Thread_Start_Child_Proc proc = (Thread_Start_Child_Proc)sjc.thread_start_child_code;
  if (proc)
    proc(child, child_thunk);
  else
    scheme_do_thread_start_child(child, child_thunk);
}


#else

void* scheme_jit_find_code_end(void *p) { return NULL; }

void scheme_thread_start_child(Scheme_Thread *child, Scheme_Object *child_thunk)
{
  return scheme_do_thread_start_child(child, child_thunk);
}

#endif
