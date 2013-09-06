/*
  Racket
  Copyright (c) 2006-2013 PLT Design Inc.

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
#include "future.h"

#ifdef MZ_USE_JIT

#include "jit.h"

/* Used by vector-set-performance-stats!: */
int scheme_jit_malloced;

/*========================================================================*/
/*                              JIT buffer                                */
/*========================================================================*/

#define JIT_CACHE_SIZE_LIMIT 65536
#define JIT_BUFFER_INIT_SIZE 256

#define JIT_INIT_MAPPINGS_SIZE 32

THREAD_LOCAL_DECL(static void *jit_buffer_cache);
THREAD_LOCAL_DECL(static intptr_t jit_buffer_cache_size);
THREAD_LOCAL_DECL(static int jit_buffer_cache_registered);

#ifdef SET_DEFAULT_LONG_JUMPS
static int default_long_jumps;
static volatile uintptr_t code_low, code_high;
#endif

static void *get_end_pointer(mz_jit_state *jitter)
{
  return jit_unadjust_ip(jit_get_ip());
}

int scheme_mz_retain_it(mz_jit_state *jitter, void *v)
{
  if (jitter->retain_start) {
    jitter->retain_start[jitter->retained] = v;
#ifdef JIT_PRECISE_GC
    /* We just change an array that is marked indirectly for GC
       via a Scheme_Native_Closure_Data. Write to that record
       so that a minor GC will trace it and therefore trace
       the reatined array: */
    if (jitter->retaining_data) {
      jitter->retaining_data->retained = jitter->retain_start;
    }
#endif
  }
  jitter->retained++;
  return jitter->retained;
}

void scheme_mz_load_retained(mz_jit_state *jitter, int rs, void *obj)
/* obj is a pointer, but not necesarily tagged (in CGC) */
{
  if (!SCHEME_INTP((Scheme_Object *)obj)
      && !SAME_OBJ((Scheme_Object *)obj, scheme_true)
      && !SAME_OBJ((Scheme_Object *)obj, scheme_false)
      && !SAME_OBJ((Scheme_Object *)obj, scheme_void)
      && !SAME_OBJ((Scheme_Object *)obj, scheme_null)) {
#ifdef JIT_PRECISE_GC
    int retptr;
    void *p;
    retptr = mz_retain(obj);
    p = jitter->retain_start + retptr - 1;
    (void)jit_patchable_movi_p(rs, p);
    jit_ldr_p(rs, rs);
#else
    mz_retain(obj);
    (void)jit_patchable_movi_p(rs, obj);
#endif
  } else {
    (void)jit_movi_p(rs, obj);
  }
}

#if defined(MZ_USE_JIT_I386)
double *scheme_mz_retain_double(mz_jit_state *jitter, double d)
{
  void *p;
  if (jitter->retain_start)
    jitter->retain_double_start[jitter->retained_double] = d;
  p = jitter->retain_double_start + jitter->retained_double;
  jitter->retained_double++;
  return p;
}
#endif

#ifdef MZ_LONG_DOUBLE
long_double *scheme_mz_retain_long_double(mz_jit_state *jitter, long_double ld)
{
  /* Save a long double into two cells of double */
  void *p;
  if (jitter->retain_start)
    memcpy(&jitter->retain_double_start[jitter->retained_double], &ld, sizeof(long_double));
  p = jitter->retain_double_start + jitter->retained_double;
  jitter->retained_double++;
  jitter->retained_double++;
  return p;
}
#endif

#ifdef SET_DEFAULT_LONG_JUMPS
static int check_long_mode(uintptr_t low, uintptr_t size) 
{
  uintptr_t high = low + size;

  if (default_long_jumps)
    return 1;
  
  if (!code_low)
    code_low = low;
  else if (low < code_low) {
#ifdef MZ_USE_PLACES
    while (!mzrt_cas(&code_low, code_low, low)) {
      if (low >= code_low)
        break;
    }
#else
    code_low = low;
#endif    
  }

  if (high > code_high) {
#ifdef MZ_USE_PLACES
    while (!mzrt_cas(&code_high, code_high, high)) {
      if (high <= code_high)
        break;
    }
#else
    code_high = high;
#endif
  }

  if ((code_high - code_low) >= ((uintptr_t)1 << 31)) {
    if (!default_long_jumps) {
      scheme_log_warning("warning: JIT switching to long-jump mode");
      default_long_jumps = 1;
    }
    return 1;
  }

  return 0;
}
#endif


void *scheme_generate_one(mz_jit_state *old_jitter, 
			  Generate_Proc generate,
			  void *data,
			  int gcable,
			  void *save_ptr,
			  Scheme_Native_Closure_Data *ndata)
{
  mz_jit_state _jitter;
  mz_jit_state *jitter = &_jitter;
  void *buffer;
  int mappings_buffer[JIT_INIT_MAPPINGS_SIZE];
  int *mappings = mappings_buffer;
  intptr_t size = JIT_BUFFER_INIT_SIZE, known_size = 0;
  intptr_t size_pre_retained = 0, size_pre_retained_double = 0, num_retained = 0, num_retained_double = 0, padding;
  int mappings_size = JIT_INIT_MAPPINGS_SIZE;
  int ok, max_extra_pushed = 0;
#ifdef SET_DEFAULT_LONG_JUMPS
  int use_long_jumps = default_long_jumps;
#endif
#ifdef MZ_PRECISE_GC
  Scheme_Object *fnl_obj;

  if (ndata) {
    /* When fnl_obj becomes inaccessible, code generated
       here can be freed. */
    fnl_obj = scheme_box(scheme_false);
  } else
    fnl_obj = NULL;
#endif

  if (!jit_buffer_cache_registered) {
    jit_buffer_cache_registered = 1;
    REGISTER_SO(jit_buffer_cache);
    scheme_register_stack_cache_stack();
#ifdef MZ_PRECISE_GC
    scheme_jit_register_traversers();
#endif
  }

#ifdef MZ_USE_JIT_ARM
  jit_get_cpu();
#endif

  while (1) {
    memset(jitter, 0, sizeof(_jitter));
#ifdef SET_DEFAULT_LONG_JUMPS
    _jitl.long_jumps_default = use_long_jumps;
#endif
#ifdef NEED_LONG_JUMPS
    _jitl.long_jumps = LONG_JUMPS_DEFAULT(_jitl);
#endif
#ifdef USE_TINY_JUMPS
    _jitl.tiny_jumps = 0;
#endif
    padding = JIT_BUFFER_PAD_SIZE;
    if (known_size) {
      size_pre_retained_double = known_size;
      size_pre_retained = size_pre_retained_double + (num_retained_double * sizeof(double));
      size = size_pre_retained + WORDS_TO_BYTES(num_retained);
      padding = 0;
      if (gcable) {
#ifdef MZ_PRECISE_GC
	buffer = scheme_malloc_code(size);
        scheme_jit_malloced += size_pre_retained_double;
#else
	buffer = scheme_malloc_gcable_code(size);
#endif
      } else {
        buffer = scheme_malloc_permanent_code(size);
      }
      RECORD_CODE_SIZE(size);
    } else if (old_jitter) {
      /* this is a recursive generate, so use leftover space in
	 old_jitter's buffer */
      buffer = get_end_pointer(old_jitter);
      size = ((char *)old_jitter->limit - (char *)buffer);
      if (size < JIT_BUFFER_INIT_SIZE) {
	old_jitter = NULL;
	buffer = NULL;
	size = JIT_BUFFER_INIT_SIZE;
      } else {
	size_pre_retained_double = size;
	size_pre_retained = size;
      }
    } else
      buffer = NULL;

    if (!buffer) {
      if (jit_buffer_cache && (jit_buffer_cache_size >= size)) {
	buffer = jit_buffer_cache;
	size = jit_buffer_cache_size;
	jit_buffer_cache = NULL;
      } else {
#ifdef MZ_PRECISE_GC
	intptr_t minsz;
	minsz = GC_malloc_stays_put_threshold();
	if (size < minsz)
	  size = minsz;
	buffer = (char *)scheme_malloc_atomic(size);
#else
	buffer = scheme_malloc(size);
#endif
      }
      size_pre_retained = size;
      size_pre_retained_double = size;
    }

#ifdef SET_DEFAULT_LONG_JUMPS
    if (!use_long_jumps) {
      /* In the case that we start allocating so much that the address
         moves beyond the 32-bit half where code normally resides,
         then switch over to long-jump mode. */
      if (check_long_mode((uintptr_t)buffer, size)) {
        /* start over */
        known_size = 0;
        use_long_jumps = 1;
        continue;
      }
    }
#endif

    (void)jit_set_ip(buffer);
    jitter->limit = (char *)buffer + size_pre_retained_double - padding;
    if (known_size) {
      jitter->retain_double_start = (double *)jitter->limit;
      jitter->retain_start = (void *)(jitter->limit + num_retained_double * sizeof(double));
#ifdef MZ_PRECISE_GC
      if (ndata) {
	memset(jitter->retain_start, 0, num_retained * sizeof(void*));
        if (num_retained) {
          jitter->retaining_data = ndata;
          ndata->retained = jitter->retain_start;
        } else
          ndata->retained = NULL;
	SCHEME_BOX_VAL(fnl_obj) = scheme_make_integer(size_pre_retained_double);
	GC_set_finalizer(fnl_obj, 1, 3,
			 scheme_jit_release_native_code, buffer,
			 NULL, NULL);
      }
#endif
    } else {
      jitter->retain_start = NULL;
      jitter->retain_double_start = (double *)buffer;
    }

    jitter->mappings = mappings;
    jitter->num_mappings = 0;
    jitter->mappings_size = mappings_size;
    mappings[0] = 0;
    jitter->max_extra_pushed = max_extra_pushed;
    jitter->self_pos = 1; /* beyond end of stack */
    jitter->self_toplevel_pos = -1;
    jitter->status_at_ptr = NULL;

    /* Leave room for retained size on first pass, 
       install it if needed) on second pass:*/
    if (!known_size || num_retained)
      scheme_mz_retain_it(jitter, (void *)scheme_make_integer(num_retained));

    ok = generate(jitter, data);

#ifdef SET_DEFAULT_LONG_JUMPS
    /* Check again after generate, because we may have
       generated new code blocks along the way. */
    if (!use_long_jumps) {
      if (check_long_mode((uintptr_t)buffer, size)) {
        /* start over */
        known_size = 0;
        use_long_jumps = 1;
        continue;
      }
    }
#endif

    if (save_ptr) {
      scheme_mz_retain_it(jitter, save_ptr);
    }
#ifdef MZ_PRECISE_GC
    if (fnl_obj) {
      scheme_mz_retain_it(jitter, fnl_obj);
    }
#endif

    jitter->limit = (char *)jitter->limit + padding;
    if (PAST_LIMIT() || (jitter->retain_start
			 && (jitter->retained > num_retained))) {
      scheme_console_printf("JIT buffer overflow: %p [%p,%p] (%d)!!\n", 
			    jit_get_ip(), 
			    buffer, jitter->limit,
			    !!jitter->retain_start);
      abort();
    }

    mappings_size = jitter->mappings_size;
    mappings = jitter->mappings;
    max_extra_pushed = jitter->max_extra_pushed;

    if (ok) {
      /* That was big enough: */
      if (jitter->unbox || jitter->unbox_depth)
	scheme_signal_error("internal error: ended with unbox or depth");
      if (MZ_LONG_DOUBLE_AND(jitter->unbox_extflonum))
	scheme_signal_error("internal error: ended with unbox_extflonum");
      if (known_size) {
	/* That was in the permanent area, so return: */
	jit_flush_code(buffer, jit_get_raw_ip());
	return buffer;
      } else {
	/* Allocate permanent area and jit again: */
	known_size = ((uintptr_t)jit_get_raw_ip()) - (uintptr_t)buffer;
        /* Make sure room for pointers is aligned: */
	if (known_size & (JIT_WORD_SIZE - 1)) {
	  known_size += (JIT_WORD_SIZE - (known_size & (JIT_WORD_SIZE - 1)));
	}
        if (jitter->retained_double) {
          /* even stronger: `double'-aligned: */
          if (known_size & (JIT_DOUBLE_SIZE - 1)) {
            known_size += (JIT_DOUBLE_SIZE - (known_size & (JIT_DOUBLE_SIZE - 1)));
          }
        }
	num_retained = jitter->retained;
        if (num_retained == 1) num_retained = 0;
	num_retained_double = jitter->retained_double;
	/* Keep this buffer? Don't if it's too big, or if it's
	   a part of old_jitter, or if there's already a bigger
	   cache. */
	if ((jit_buffer_cache_size < JIT_CACHE_SIZE_LIMIT)
	    && !old_jitter
	    && (!jit_buffer_cache
		|| (jit_buffer_cache_size > size))) {
	  jit_buffer_cache = buffer;
	  jit_buffer_cache_size = size;
	}
      }
      /* looping to try again... */
    } else {
      /* Need more room to try again: */
      size = size * 2;
      old_jitter = NULL;
    }
  }
}


mz_jit_state *scheme_clone_jitter(mz_jit_state *jitter) {
  mz_jit_state *jitter_copy;

  jitter_copy = MALLOC_ONE_RT(mz_jit_state);
  memcpy(jitter_copy, jitter, sizeof(mz_jit_state));
#ifdef MZTAG_REQUIRED
  jitter_copy->type = scheme_rt_jitter_data;
#endif

  return jitter_copy;
}


void scheme_unclone_jitter(mz_jit_state *jitter, mz_jit_state *jitter_copy) {
  memcpy(jitter, jitter_copy, sizeof(mz_jit_state));
}

/*========================================================================*/
/*                           code-gen utils                               */
/*========================================================================*/

static void new_mapping(mz_jit_state *jitter)
{
  jitter->num_mappings++;
  if (jitter->num_mappings >= jitter->mappings_size) {
    int *a;
    a = (int *)scheme_malloc_atomic(jitter->mappings_size * 2 * sizeof(int));
    memcpy(a, jitter->mappings, jitter->mappings_size * sizeof(int));
    jitter->mappings = a;
    jitter->mappings_size *= 2;
  }
  jitter->mappings[jitter->num_mappings] = 0;
}

void scheme_mz_pushr_p_it(mz_jit_state *jitter, int reg) 
/* de-sync's rs */
{
  int v;

  jitter->extra_pushed++;
  if (jitter->extra_pushed > jitter->max_extra_pushed)
    jitter->max_extra_pushed = jitter->extra_pushed;

  if (!(jitter->mappings[jitter->num_mappings] & 0x1)
      || (jitter->mappings[jitter->num_mappings] & 0x2)
      || (jitter->mappings[jitter->num_mappings] < 0)) {
    new_mapping(jitter);
  }
  v = (jitter->mappings[jitter->num_mappings]) >> 2;
  v++;
  jitter->mappings[jitter->num_mappings] = ((v << 2) | 0x1);
  
  mz_rs_dec(1);
  CHECK_RUNSTACK_OVERFLOW_NOCL();
  mz_rs_str(reg);

  jitter->need_set_rs = 1;
}

void scheme_mz_popr_p_it(mz_jit_state *jitter, int reg, int discard) 
/* de-sync's rs */
{
  int v;

  jitter->extra_pushed--;

  JIT_ASSERT(jitter->mappings[jitter->num_mappings] & 0x1);
  JIT_ASSERT(!(jitter->mappings[jitter->num_mappings] & 0x2));
  v = jitter->mappings[jitter->num_mappings] >> 2;
  v--;
  if (!v)
    --jitter->num_mappings;
  else
    jitter->mappings[jitter->num_mappings] = ((v << 2) | 0x1);

  if (!discard)
    mz_rs_ldr(reg);
  mz_rs_inc(1);

  jitter->need_set_rs = 1;
}

void scheme_mz_need_space(mz_jit_state *jitter, int need_extra)
{
  if (jitter->extra_pushed + need_extra > jitter->max_extra_pushed)
    jitter->max_extra_pushed = jitter->extra_pushed + need_extra;
}

void scheme_mz_runstack_skipped(mz_jit_state *jitter, int n) 
{
  int v;

  if (!(jitter->mappings[jitter->num_mappings] & 0x1)
      || (jitter->mappings[jitter->num_mappings] & 0x2)
      || (jitter->mappings[jitter->num_mappings] > 0)) {
    new_mapping(jitter);
  }
  v = (jitter->mappings[jitter->num_mappings]) >> 2;
  JIT_ASSERT(v <= 0);
  v -= n;
  jitter->mappings[jitter->num_mappings] = ((v << 2) | 0x1);
  jitter->self_pos += n;
}

void scheme_mz_runstack_unskipped(mz_jit_state *jitter, int n) 
{
  int v;

  JIT_ASSERT(jitter->mappings[jitter->num_mappings] & 0x1);
  JIT_ASSERT(!(jitter->mappings[jitter->num_mappings] & 0x2));
  v = (jitter->mappings[jitter->num_mappings]) >> 2;
  JIT_ASSERT(v + n <= 0);
  v += n;
  if (!v)
    --jitter->num_mappings;
  else
    jitter->mappings[jitter->num_mappings] = ((v << 2) | 0x1);
  jitter->self_pos -= n;
}

void scheme_mz_runstack_pushed(mz_jit_state *jitter, int n)
{
  jitter->depth += n;
  if (jitter->depth > jitter->max_depth)
    jitter->max_depth = jitter->depth;
  jitter->self_pos += n;
  if (!jitter->mappings[jitter->num_mappings]
      || (jitter->mappings[jitter->num_mappings] & 0x3)) {
    new_mapping(jitter);
  }
  jitter->mappings[jitter->num_mappings] += (n << 2);
  jitter->need_set_rs = 1;
}

void scheme_mz_runstack_closure_pushed(mz_jit_state *jitter, int a, int flags)
{
  jitter->depth += 1;
  if (jitter->depth > jitter->max_depth)
    jitter->max_depth = jitter->depth;
  jitter->self_pos += 1;
  new_mapping(jitter);
  jitter->mappings[jitter->num_mappings] = (a << 4) | (flags << 2) | 0x2;
  jitter->need_set_rs = 1;
  /* closures are never popped; they go away due to returns or tail calls */
}

#ifdef USE_FLONUM_UNBOXING
void scheme_mz_runstack_flonum_pushed(mz_jit_state *jitter, int pos)
{
  jitter->depth += 1;
  if (jitter->depth > jitter->max_depth)
    jitter->max_depth = jitter->depth;
  jitter->self_pos += 1;
  new_mapping(jitter);
  jitter->mappings[jitter->num_mappings] = (pos << 2) | 0x3;
  jitter->need_set_rs = 1;
  /* flonums are never popped; they go away due to returns or tail calls */
}
#endif

void scheme_mz_runstack_popped(mz_jit_state *jitter, int n)
{
  int v;
  jitter->depth -= n;
  jitter->self_pos -= n;

  v = jitter->mappings[jitter->num_mappings];
  JIT_ASSERT(!(v & 0x1));
  /* non-procedure slot */
  v = v >> 2;
  JIT_ASSERT(v >= n);
  v -= n;
  if (!v)
    --jitter->num_mappings;
  else
    jitter->mappings[jitter->num_mappings] = (v << 2);
  jitter->need_set_rs = 1;
}

int scheme_mz_try_runstack_pop(mz_jit_state *jitter, int n)
{
  if (jitter->mappings[jitter->num_mappings] & 0x3)
    return 0;
  if ((jitter->mappings[jitter->num_mappings] >> 2) < n)
    return 0;
  mz_runstack_popped(jitter, n);
  return 1;
}

void scheme_mz_runstack_saved(mz_jit_state *jitter)
{
  new_mapping(jitter);
  /* 0 slot means "saved here" */
}

int scheme_mz_compute_runstack_restored(mz_jit_state *jitter, int adj, int skip)
{
  /* pop down to 0 slot */
  int amt = 0, c, num_mappings;
  
  num_mappings = jitter->num_mappings;
  while (1) {
    c = jitter->mappings[num_mappings];
    if (!c) {
      if (skip)
        --skip;
      else
        break;
    } else if (c & 0x1) {
      if (c & 0x2) {
        /* single flonum */
        amt++;
        if (adj) jitter->self_pos--;
      } else {
        /* native push or skip */
        c >>= 2;
        if (c > 0)
          amt += c;
        else {
          if (adj) jitter->self_pos += c;
        }
      }
    } else if (c & 0x2) {
      /* single procedure */
      amt++;
      if (adj) jitter->self_pos--;
    } else {
      /* pushed N */
      c = (c >> 2);
      amt += c;
      if (adj) jitter->self_pos -= c;
    }
    --num_mappings;
  }
  --num_mappings;
  if (adj) {
    jitter->num_mappings = num_mappings;
    if (amt)
      jitter->need_set_rs = 1;
    jitter->depth -= amt;
  }
  return amt;
}

int scheme_mz_runstack_restored(mz_jit_state *jitter)
{
  return scheme_mz_compute_runstack_restored(jitter, 1, 0);
}

int scheme_mz_flostack_save(mz_jit_state *jitter, int *pos)
{
  *pos = jitter->flostack_offset;
  return jitter->flostack_space;
}

void scheme_mz_flostack_restore(mz_jit_state *jitter, int space, int pos, int gen, int adj)
{
  if (space != jitter->flostack_space) {
    if (gen) {
      int delta = jitter->flostack_space - space;
      jit_addi_p(JIT_SP, JIT_SP, delta);
    }
    if (adj) jitter->flostack_space = space;
  }
  if (adj) jitter->flostack_offset = pos;
}

int scheme_mz_remap_it(mz_jit_state *jitter, int i)
{
  int j = i, p = jitter->num_mappings, c;
  while (p && (j >= 0)) {
    c = jitter->mappings[p];
    if (c & 0x1) {
      if (c & 0x2) {
        /* single flonum */
        j--;
      } else {
        /* native push or skip */
        c >>= 2;
        i += c;
        if (c < 0)
          j += c;
      }
    } else if (c & 0x2) {
      /* single procedure */
      j--;
    } else {
      /* pushed N */
      j -= (c >> 2);
    }
    --p;
  }
  return i;
}

int scheme_mz_is_closure(mz_jit_state *jitter, int i, int arity, int *_flags)
{
  int j = i, p = jitter->num_mappings, c;
  while (p && (j >= 0)) {
    c = jitter->mappings[p];
    if (c & 0x1) {
      if (c & 0x2) {
        /* single flonum */
        j--;
      } else {
        /* native push or skip */
        c >>= 2;
        if (c < 0)
          j += c;
      }
    } else if (c & 0x2) {
      /* procedure */
      if (!j) {
        /* the one we're looking for */
        if ((arity == (c >> 4)) || (arity == -1)) {
          *_flags = (c >> 2) & 0x3;
          return 1;
        }
      }
      j--;
    } else {
      /* pushed N */
      j -= (c >> 2);
    }
    --p;
  }
  return 0;
}

#ifdef USE_FLONUM_UNBOXING
int scheme_mz_flostack_pos(mz_jit_state *jitter, int i)
{
  int j = i, p = jitter->num_mappings, c;
  while (p && (j >= 0)) {
    c = jitter->mappings[p];
    if (c & 0x1) {
      if (c & 0x2) {
        /* single flonum */
        if (!j) {
          /* the one we're looking for */
          return c >> 2;
        }
        j--;
      } else {
        /* native push or skip */
        c >>= 2;
        if (c < 0)
          j += c;
      }
    } else if (c & 0x2) {
      /* single procedure */
      j--;
    } else {
      /* pushed N */
      j -= (c >> 2);
    }
    --p;
  }
  scheme_signal_error("internal error: flonum position not found");
  return 0;
}
#endif

int scheme_stack_safety(mz_jit_state *jitter, int cnt, int offset)
/* de-sync'd rs ok */
{
  /* To preserve space safety, we must initialize any stack room
     that we make, so that whatever happens to be there isn't
     traversed in case of a GC. the value of JIT_RUNSTACK is
     handy to use as a "clear" value. */
  int i, valid;

  valid = mz_CURRENT_REG_STATUS_VALID();

  for (i = 0; i < cnt; i++) {
    mz_rs_stxi(i+offset, JIT_RUNSTACK);
    CHECK_LIMIT();
  }

  if (valid) mz_SET_REG_STATUS_VALID(1);

  return 1;
}

void scheme_mz_unbox_save(mz_jit_state *jitter, mz_jit_unbox_state *r)
{
  r->unbox = jitter->unbox;
  jitter->unbox = 0;

#ifdef MZ_LONG_DOUBLE
  r->unbox_extflonum = jitter->unbox_extflonum;
  jitter->unbox_extflonum = 0;
#endif
  }

void scheme_mz_unbox_restore(mz_jit_state *jitter, mz_jit_unbox_state *r)
{
  jitter->unbox = r->unbox;
#ifdef MZ_LONG_DOUBLE
  jitter->unbox_extflonum = r->unbox_extflonum;
#endif  
}

#endif
