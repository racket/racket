/*
  MzScheme
  Copyright (c) 2006 PLT Scheme Inc.

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

#include "schpriv.h"
#include "schmach.h"

#ifdef MZ_USE_JIT

#ifdef __APPLE__
# define _CALL_DARWIN
#endif

#include "lightning/lightning.h"

#define JIT_LOG_WORD_SIZE 2
#define WORDS_TO_BYTES(x) ((x) << JIT_LOG_WORD_SIZE)
#define JIT_WORD_SIZE (1 << JIT_LOG_WORD_SIZE)

#define JIT_NOT_RET JIT_R1
#if JIT_NOT_RET == JIT_RET
Fix me! See use.
#endif

#define MAX_SHARED_CALL_RANDS 25
static void *shared_tail_code[3][MAX_SHARED_CALL_RANDS];
static void *shared_non_tail_code[3][MAX_SHARED_CALL_RANDS][2];

#define MAX_SHARED_ARITY_CHECK 25
static void *shared_arity_check[MAX_SHARED_ARITY_CHECK][2][2];

static void *jump_to_native_code;
static void *jump_to_native_arity_code;
static void *bad_result_arity_code;
static void *unbound_global_code;
static void *quote_syntax_code;
static void *call_original_unary_arith_code;
static void *call_original_binary_arith_code;
static void *call_original_binary_rev_arith_code;
static void *call_original_unary_arith_for_branch_code;
static void *call_original_binary_arith_for_branch_code;
static void *call_original_binary_rev_arith_for_branch_code;
static void *bad_car_code, *bad_cdr_code;
static void *vector_ref_code, *vector_ref_check_index_code;
static void *syntax_e_code;
static void *on_demand_jit_code;
static void *on_demand_jit_arity_code;
static void *get_stack_pointer_code;
static void *stack_cache_pop_code;

typedef struct {
  jit_state js;
  char *limit;
  int extra_pushed, max_extra_pushed;
  int depth, max_depth;
  int *mappings; /* For each element,
		    case 0x1 bit:
		    . 0 -> case 0x2 bit:
                    .        0 -> case rest bits:
                    .               0 -> save point
                    .               1 -> shift >>2 to get orig pushed count
                    .        1 -> shift >>2 to get arity for single orig pushed
		    . 1 -> shift >>1 to get new (native) pushed */
  int num_mappings, mappings_size;
  int retained;
  int need_set_rs;
  void **retain_start;
  int log_depth;
} mz_jit_state;

typedef int (*Native_Check_Arity_Proc)(Scheme_Object *o, int argc);
typedef Scheme_Object *(*Native_Get_Arity_Proc)(Scheme_Object *o);
static Native_Check_Arity_Proc check_arity_code;
static Native_Get_Arity_Proc get_arity_code;

static int generate_non_tail(Scheme_Object *obj, mz_jit_state *jitter, int multi_ok, int need_ends);
static int generate(Scheme_Object *obj, mz_jit_state *jitter, int tail_ok, int multi_ok);
static void *generate_top_simple_arity_check(int num_params, int has_rest, int is_method, int permanent);
static void generate_top_case_lambda_dispatch(Scheme_Case_Lambda *c, Scheme_Native_Closure_Data *ndata, 
					      int is_method);
static void on_demand();
static int generate_non_tail_mark_pos_prefix(mz_jit_state *jitter);
static void generate_non_tail_mark_pos_suffix(mz_jit_state *jitter);

/* Tracking statistics: */
#if 0
# define NUM_CATEGORIES 23
int jit_sizes[NUM_CATEGORIES];
int jit_counts[NUM_CATEGORIES];
int jit_code_size;
# define START_JIT_DATA() void *__pos = jit_get_ip().ptr; unsigned long __total = 0
# define END_JIT_DATA(where) if (jitter->retain_start) { \
                              jit_sizes[where] += __total + ((unsigned long)jit_get_ip().ptr - (unsigned long)__pos); \
                              jit_counts[where]++; }
# define PAUSE_JIT_DATA() __total += ((unsigned long)jit_get_ip().ptr - (unsigned long)__pos)
# define RESUME_JIT_DATA() __pos = jit_get_ip().ptr
# define RECORD_CODE_SIZE(s) jit_code_size += s
#else
# define START_JIT_DATA() /* empty */
# define END_JIT_DATA(where) /* empty */
# define PAUSE_JIT_DATA() /* empty */
# define RESUME_JIT_DATA() /* empty */
# define RECORD_CODE_SIZE(s) /* empty */
#endif

typedef struct {
  Scheme_Native_Closure_Data nc;
  Scheme_Native_Closure_Data *case_lam;
} Scheme_Native_Closure_Data_Plus_Case;

/* This structure must be 4 words: */
typedef struct {
  void *orig_return_address;
  void *stack_frame;
  Scheme_Object *cache;
  void *filler;
} Stack_Cache_Elem;

#define STACK_CACHE_SIZE 32
static Stack_Cache_Elem stack_cache_stack[STACK_CACHE_SIZE];
int stack_cache_stack_pos = 0;

#define IS_NAMED_PRIM(p, nm) (!strcmp(((Scheme_Primitive_Proc *)p)->name, nm))

#include "codetab.inc"

/*========================================================================*/
/*                              JIT buffer                                */
/*========================================================================*/

#define _jit (jitter->js)
#define PAST_LIMIT() (jit_get_ip().ptr > jitter->limit)
#define CHECK_LIMIT() if (PAST_LIMIT()) return 0;

#define JIT_CACHE_SIZE_LIMIT 65536
#define JIT_BUFFER_INIT_SIZE 256

#define JIT_INIT_MAPPINGS_SIZE 32

static void *jit_buffer_cache;
static long jit_buffer_cache_size;
static int jit_buffer_cache_registered;

typedef int (*Generate_Proc)(mz_jit_state *j, void *data);

static void *get_end_pointer(mz_jit_state *jitter)
{
  return jit_get_ip().ptr;
}

static void mz_retain_it(mz_jit_state *jitter, void *v)
{
  if (jitter->retain_start) {
    jitter->retain_start[jitter->retained] = v;
  }
  jitter->retained++;
}

static void *generate_one(mz_jit_state *old_jitter, 
			  Generate_Proc generate,
			  void *data,
			  int gcable,
			  void *save_ptr)
{
  mz_jit_state _jitter;
  mz_jit_state *jitter = &_jitter;
  void *buffer;
  int mappings_buffer[JIT_INIT_MAPPINGS_SIZE];
  int *mappings = mappings_buffer;
  long size = JIT_BUFFER_INIT_SIZE, known_size = 0, size_pre_retained = 0, num_retained = 0, padding;
  int mappings_size = JIT_INIT_MAPPINGS_SIZE;
  int ok, max_extra_pushed = 0;

  if (!jit_buffer_cache_registered) {
    jit_buffer_cache_registered = 1;
    REGISTER_SO(jit_buffer_cache);
    REGISTER_SO(stack_cache_stack);
    /* printf("zap!\n"); */
  }

  while (1) {
    memset(jitter, 0, sizeof(_jitter));
#ifdef MZ_USE_JIT_PPC
    _jitl.long_jumps = 1;
#endif
    padding = 100;
    if (known_size) {
      size_pre_retained = known_size;
      size = size_pre_retained + WORDS_TO_BYTES(num_retained);
      padding = 0;
      if (gcable) {
	buffer = scheme_malloc(size);
      } else {
	buffer = malloc(size);
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
	buffer = scheme_malloc(size);
      }
      size_pre_retained = size;
    }
      
    (void)jit_set_ip(buffer).ptr;
    jitter->limit = (char *)buffer + size_pre_retained - padding;
    if (known_size)
      jitter->retain_start = (void *)jitter->limit;
    else
      jitter->retain_start = NULL;

    jitter->mappings = mappings;
    jitter->num_mappings = 0;
    jitter->mappings_size = mappings_size;
    mappings[0] = 0;
    jitter->max_extra_pushed = max_extra_pushed;

    ok = generate(jitter, data);

    if (save_ptr) {
      mz_retain_it(jitter, save_ptr);
    }

    jitter->limit = (char *)jitter->limit + padding;
    if (PAST_LIMIT() || (jitter->retain_start
			 && (jitter->retained > num_retained))) {
      scheme_console_printf("JIT buffer overflow!!\n");
      abort();
    }

    mappings_size = jitter->mappings_size;
    mappings = jitter->mappings;
    max_extra_pushed = jitter->max_extra_pushed;

    if (ok) {
      /* That was big enough: */
      if (known_size) {
	/* That was in the permanent area, so return: */
	jit_flush_code(buffer, jit_get_ip().ptr);
	return buffer;
      } else {
	/* Allocate permanent area and jit again: */
	known_size = ((unsigned long)jit_get_ip().ptr) - (unsigned long)buffer;
	if (known_size & 0x3) {
	  known_size += (4 - (known_size & 0x3));
	}
	num_retained = jitter->retained;
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

#if 0
# define FOR_LOG(x) x
# define LOG_IT(args) if (jitter->retain_start) { emit_indentation(jitter); printf args; }
static void emit_indentation(mz_jit_state *jitter)
{
  int i = jitter->log_depth;
  while (i--) {
    printf("  ");
  }
}
#else
# define FOR_LOG(x) /* empty */
# define LOG_IT(args) /* empty */
#endif

/*========================================================================*/
/*                               run time                                 */
/*========================================================================*/

static 
#ifndef NO_INLINE_KEYWORD
MSC_IZE(inline) 
#endif
  Scheme_Object *do_make_native_closure(Scheme_Native_Closure_Data *code, int size)
{
  Scheme_Native_Closure *o;

  o = (Scheme_Native_Closure *)scheme_malloc_tagged(sizeof(Scheme_Native_Closure)
						    + ((size - 1) * sizeof(Scheme_Object *)));
  o->so.type = scheme_native_closure_type;
  o->code = code;

  return (Scheme_Object *)o;
}

Scheme_Object *scheme_make_native_closure(Scheme_Native_Closure_Data *code)
{
  return do_make_native_closure(code, code->closure_size);
}

Scheme_Object *scheme_make_native_case_closure(Scheme_Native_Closure_Data *code)
{
  return do_make_native_closure(code, -(code->closure_size + 1));
}

static void box_multiple_array_element(int pos)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object **naya, **a;
  int i;

  naya = MALLOC_N(Scheme_Object *, p->ku.multiple.count);
  a = p->ku.multiple.array;
  
  for (i = p->ku.multiple.count; i--; ) {
    naya[i] = a[i];
  }
  {
    Scheme_Object *eb;
    eb = scheme_make_envunbox(naya[pos]);
    naya[pos] = eb;
  }
  
  p->ku.multiple.array = naya;
}

static void thread_block()
{
  scheme_thread_block(0);
  scheme_current_thread->ran_some = 1;
}

/*========================================================================*/
/*                           code-gen utils                               */
/*========================================================================*/

#define JIT_RUNSTACK JIT_V0
#define JIT_RUNSTACK_BASE JIT_V2

#ifdef MZ_USE_JIT_PPC
# define JIT_STACK 1
# define JIT_STACK_FRAME 1
#else
# define JIT_STACK JIT_SP
# define JIT_STACK_FRAME JIT_FP
#endif

#define JIT_UPDATE_THREAD_RSPTR() jit_sti_p(&MZ_RUNSTACK, JIT_RUNSTACK)
#define JIT_UPDATE_THREAD_RSPTR_IF_NEEDED() \
    if (jitter->need_set_rs) {   \
      JIT_UPDATE_THREAD_RSPTR(); \
      jitter->need_set_rs = 0;   \
    }
#define JIT_UPDATE_THREAD_RSPTR_FOR_BRANCH_IF_NEEDED() \
    if (jitter->need_set_rs) {   \
      JIT_UPDATE_THREAD_RSPTR(); \
    }

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

static void mz_pushr_p_it(mz_jit_state *jitter, int reg) 
{
  int v;

  jitter->extra_pushed++;
  if (jitter->extra_pushed > jitter->max_extra_pushed)
    jitter->max_extra_pushed = jitter->extra_pushed;

  if (!(jitter->mappings[jitter->num_mappings] & 0x1)
      || (jitter->mappings[jitter->num_mappings] < 0)) {
    new_mapping(jitter);
  }
  v = (jitter->mappings[jitter->num_mappings]) >> 1;
  v++;
  jitter->mappings[jitter->num_mappings] = ((v << 1) | 0x1);
  
  jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
  jit_str_p(JIT_RUNSTACK, reg);

  jitter->need_set_rs = 1;
}

static void mz_popr_p_it(mz_jit_state *jitter, int reg) 
{
  int v;

  jitter->extra_pushed--;

  v = jitter->mappings[jitter->num_mappings] >> 1;
  v--;
  if (!v)
    --jitter->num_mappings;
  else
    jitter->mappings[jitter->num_mappings] = ((v << 1) | 0x1);

  jit_ldr_p(reg, JIT_RUNSTACK);
  jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));

  jitter->need_set_rs = 1;
}

static void mz_runstack_skipped(mz_jit_state *jitter, int n) 
{
  int v;

  if (!(jitter->mappings[jitter->num_mappings] & 0x1)
      || (jitter->mappings[jitter->num_mappings] > 0)) {
    new_mapping(jitter);
  }
  v = (jitter->mappings[jitter->num_mappings]) >> 1;
  v -= n;
  jitter->mappings[jitter->num_mappings] = ((v << 1) | 0x1);
}

static void mz_runstack_unskipped(mz_jit_state *jitter, int n) 
{
  int v;

  v = (jitter->mappings[jitter->num_mappings]) >> 1;
  v += n;
  if (!v)
    --jitter->num_mappings;
  else
    jitter->mappings[jitter->num_mappings] = ((v << 1) | 0x1);
}

static void mz_runstack_pushed(mz_jit_state *jitter, int n)
{
  jitter->depth += n;
  if (jitter->depth > jitter->max_depth)
    jitter->max_depth = jitter->depth;
  if (!jitter->mappings[jitter->num_mappings]
      || (jitter->mappings[jitter->num_mappings] & 0x3)) {
    new_mapping(jitter);
  }
  jitter->mappings[jitter->num_mappings] += (n << 2);
  jitter->need_set_rs = 1;
}

static void mz_runstack_closure_pushed(mz_jit_state *jitter, int a)
{
  jitter->depth += 1;
  if (jitter->depth > jitter->max_depth)
    jitter->max_depth = jitter->depth;
  new_mapping(jitter);
  jitter->mappings[jitter->num_mappings] = (a << 2) | 0x2;
  jitter->need_set_rs = 1;
}

static void mz_runstack_popped(mz_jit_state *jitter, int n)
{
  int v;
  jitter->depth -= n;
  v = (jitter->mappings[jitter->num_mappings]) >> 2;
  v -= n;
  if (!v)
    --jitter->num_mappings;
  else
    jitter->mappings[jitter->num_mappings] = (v << 2);
  jitter->need_set_rs = 1;
}

static void mz_runstack_saved(mz_jit_state *jitter)
{
  new_mapping(jitter);
  /* 0 slot means "saved here" */
}

static int mz_runstack_restored(mz_jit_state *jitter)
{
  /* pop down to 0 slot */
  int amt = 0, c;
  while ((c = jitter->mappings[jitter->num_mappings])) {
    if (c & 0x1) {
      c >>= 1;
      if (c > 0)
	amt += c;
    } else if (c & 0x2) {
      amt++;
    } else
      amt += (c >> 2);
    --jitter->num_mappings;
  }
  --jitter->num_mappings;
  if (amt)
    jitter->need_set_rs = 1;
  jitter->depth -= amt;
  return amt;
}

static int mz_remap_it(mz_jit_state *jitter, int i)
{
  int j = i, p = jitter->num_mappings, c;
  while (p && (j >= 0)) {
    c = jitter->mappings[p];
    if (c & 0x1) {
      c >>= 1;
      i += c;
      if (c < 0)
	j += c;
    } else if (c & 0x2) {
      j--;
    } else {
      j -= (c >> 2);
    }
    --p;
  }
  return i;
}

static int mz_is_closure(mz_jit_state *jitter, int i, int arity)
{
  int j = i, p = jitter->num_mappings, c;
  while (p && (j >= 0)) {
    c = jitter->mappings[p];
    if (c & 0x1) {
      c >>= 1;
      if (c < 0)
	j += c;
    } else if (c & 0x2) {
      if (!j) {
	if (arity == (c >> 2))
	  return 1;
      }
      j--;
    } else {
      j -= (c >> 2);
    }
    --p;
  }
  return 0;
}

#define mz_pushr_p(x) mz_pushr_p_it(jitter, x)
#define mz_popr_p(x) mz_popr_p_it(jitter, x)

#define mz_prepare(x) jit_prepare(x)
#define mz_finish(x) jit_finish(x)
#define mz_finishr(x) jit_finishr(x)

#define mz_retain(x) mz_retain_it(jitter, x)
#define mz_remap(x) mz_remap_it(jitter, x)

#ifdef MZ_USE_JIT_PPC
/* JIT_LOCAL1 and JIT_LOCAL2 are offsets in the stack frame.
   We use the last two slots reserved for parameters to calless,
   because we never call with more than 6 arguments. */
# define JIT_LOCAL1 56
# define JIT_LOCAL2 60
# define JIT_LOCAL3 64
# define mz_push_local_p(x, l) jit_stxi_p(l, 1, x)
# define mz_pop_local_p(x, l) jit_ldxi_p(x, 1, l)
# define mz_patch_branch_at(a, v) (_jitl.long_jumps ? (void)jit_patch_movei(a-4, a-3, v) : (void)jit_patch_branch(a-1, v))
# define mz_patch_ucbranch_at(a, v) (_jitl.long_jumps ? (void)jit_patch_movei(a-4, a-3, v) : (void)jit_patch_ucbranch(a-1, v))
# define mz_patch_branch(a) mz_patch_branch_at(a, (_jit.x.pc))
# define mz_patch_ucbranch(a) mz_patch_ucbranch_at(a, (_jit.x.pc))
# define mz_prolog(x) (MFLRr(x), mz_push_local_p(x, JIT_LOCAL2))
# define mz_epilog(x) (mz_pop_local_p(x, JIT_LOCAL2), jit_jmpr(x))
#else
# define mz_push_local_p(x, l) jit_pushr_p(x)
# define mz_pop_local_p(x, l) jit_popr_p(x)
# define mz_patch_branch_at(a, v) jit_patch_at(a, v)
# define mz_patch_ucbranch_at(a, v) jit_patch_at(a, v)
# define mz_patch_branch(a) jit_patch(a)
# define mz_patch_ucbranch(a) jit_patch(a)
# define mz_prolog(x) /* empty */
# define mz_epilog(x) RET_()
#endif

#ifdef MZ_USE_JIT_PPC
# define __START_SHORT_JUMPS__(cond) if (cond) { _jitl.long_jumps = 0; }
# define __END_SHORT_JUMPS__(cond) if (cond) { _jitl.long_jumps = 1; }
#else
# define __START_SHORT_JUMPS__(cond) /* empty */
# define __END_SHORT_JUMPS__(cond) /* empty */
#endif

/* Note: Things like

      refm = jit_jmpi(jit_forward());
      jit_patch_at(refm, jump_to_native_code);

   appear in the code because the generated instructions can depend on
   the actual value supplied to jit_jmpi, and it can depend on the
   relative location between the instruction address and the actual
   value.  Using jit_patch ensures that the generated instructions
   always have the same size. */

/*========================================================================*/
/*                         bytecode properties                            */
/*========================================================================*/

#ifdef MZ_USE_JIT_PPC
static int is_short(Scheme_Object *obj, int fuel)
{
  Scheme_Type t;

  if (fuel <= 0)
    return fuel;

  t = SCHEME_TYPE(obj);

  switch (t) {
  case scheme_syntax_type:
    {
      int t;
      t = SCHEME_PINT_VAL(obj); 
      if ((t == CASE_LAMBDA_EXPD)
	  || (t == QUOTE_SYNTAX_EXPD))
	return fuel - 1;
      else
	return 0;
    }
    break;
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)obj;
      int i;

      fuel -= app->num_args;
      for (i = app->num_args + 1; i--; ) {
	fuel = is_short(app->args[i], fuel);
      }
      return fuel;
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)obj;
      fuel -= 2;
      fuel = is_short(app->rator, fuel);
      return is_short(app->rand, fuel);
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)obj;
      fuel -= 3;
      fuel = is_short(app->rator, fuel);
      fuel = is_short(app->rand1, fuel);
      return is_short(app->rand2, fuel);
    }
  case scheme_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)obj;
      int i;

      fuel -= seq->count;
      for (i = seq->count; i--; ) {
	fuel = is_short(seq->array[i], fuel);
      }
      return fuel;
    }
    break;
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *branch = (Scheme_Branch_Rec *)obj;
      fuel -= 3;
      fuel = is_short(branch->test, fuel);
      fuel = is_short(branch->tbranch, fuel);
      return is_short(branch->fbranch, fuel);
    }
  case scheme_toplevel_type:
  case scheme_local_type:
  case scheme_local_unbox_type:
  case scheme_unclosed_procedure_type:
    return fuel - 1;
  default:
    if (t > _scheme_values_types_)
      return fuel - 1;
    else
      return 0;
  }
}
#endif

static int inlined_unary_prim(Scheme_Object *o, Scheme_Object *_app)
{
  return (SCHEME_PRIMP(o)
	  && (SCHEME_PRIM_PROC_FLAGS(o) & SCHEME_PRIM_IS_UNARY_INLINED));
}

static int inlined_binary_prim(Scheme_Object *o, Scheme_Object *_app)
{
  return (SCHEME_PRIMP(o)
	  && (SCHEME_PRIM_PROC_FLAGS(o) & SCHEME_PRIM_IS_BINARY_INLINED));
}

static int is_noncm(Scheme_Object *a)
{
  if (SCHEME_PRIMP(a)) {
    if (((Scheme_Prim_Proc_Header *)a)->flags & SCHEME_PRIM_IS_NONCM)
      return 1;
  }
  return 0;
}

#define INIT_SIMPLE_DEPTH 10

static int is_simple(Scheme_Object *obj, int depth, int just_markless)
{
  /* Return 1 if evaluating `obj' doesn't change the runstack or cont-mark stack ---
     or, if just_markless is 1, doesn't use the cont-mark stack.
     If a form doesn't itself change/use the stack, then check all
     expressions in tail position, up to some depth. The conservative
     answer is always 0. */
  Scheme_Type type;

  type = SCHEME_TYPE(obj);

  switch (type) {
  case scheme_syntax_type:
    {
      int t;
      t = SCHEME_PINT_VAL(obj); 
      return ((t == CASE_LAMBDA_EXPD)
	      || (t == QUOTE_SYNTAX_EXPD));
    }
    break;

  case scheme_branch_type:
    if (depth) {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)obj;
      return (is_simple(b->tbranch, depth - 1, just_markless)
	      && is_simple(b->fbranch, depth - 1, just_markless));
    }
    break;
    
  case scheme_let_value_type:
    if (depth) {
      return is_simple(((Scheme_Let_Value *)obj)->body, depth - 1, just_markless);
    }
    break;
  case scheme_let_one_type:
    if (just_markless && depth) {
      return is_simple(((Scheme_Let_One *)obj)->body, depth - 1, just_markless);
    }
    break;
  case scheme_let_void_type:
    if (just_markless && depth) {
      return is_simple(((Scheme_Let_Void *)obj)->body, depth - 1, just_markless);
    }
    break;
  case scheme_letrec_type:
    if (just_markless && depth) {
      return is_simple(((Scheme_Letrec *)obj)->body, depth - 1, just_markless);
    }
    break;

  case scheme_application_type:
    if (just_markless) {
      return is_noncm(((Scheme_App_Rec *)obj)->args[0]);
    }
    break;
  case scheme_application2_type:
    if (inlined_unary_prim(((Scheme_App2_Rec *)obj)->rator, obj))
      return 1;
    else if (just_markless) {
      return is_noncm(((Scheme_App2_Rec *)obj)->rator);
    }
    break;
  case scheme_application3_type:
    if (inlined_binary_prim(((Scheme_App2_Rec *)obj)->rator, obj))
      return 1;
    else if (just_markless) {
      return is_noncm(((Scheme_App3_Rec *)obj)->rator);
    }
    break;
    
  case scheme_toplevel_type:
  case scheme_local_type:
  case scheme_local_unbox_type:
  case scheme_unclosed_procedure_type:
    return 1;
    break;
  }

  return (type > _scheme_values_types_);
}

/*========================================================================*/
/*                         application codegen                            */
/*========================================================================*/

static int generate_direct_prim_tail_call(mz_jit_state *jitter, int num_rands)
{
  /* JIT_V1 must have the target function pointer.
     Also, scheme_current_runstack must be up-to-date...
     unless num-rands == 1, in which case JIT_R0 must
     have the argument. */
  if (num_rands == 1) {
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    JIT_UPDATE_THREAD_RSPTR();
  }
  jit_movi_i(JIT_R1, num_rands);
  mz_prepare(2);
  CHECK_LIMIT();
  jit_pusharg_p(JIT_RUNSTACK);
  jit_pusharg_i(JIT_R1);
  mz_finishr(JIT_V1);
  CHECK_LIMIT();
  /* Pop saved runstack val and return: */
  mz_pop_local_p(JIT_NOT_RET, JIT_LOCAL1);
  jit_sti_p(&scheme_current_runstack, JIT_NOT_RET);
  jit_ret();

  return 1;
}

static int generate_tail_call(mz_jit_state *jitter, int num_rands, int direct_native, int need_set_rs)
{
  int i;
  GC_CAN_IGNORE jit_insn *ref, *ref2, *ref4, *ref5;

  __START_SHORT_JUMPS__(num_rands < 100);

  /* First, try fast direct jump to native code: */
  if (!direct_native) {
    ref = jit_bmsi_ul(jit_forward(), JIT_V1, 0x1);
    jit_ldr_s(JIT_R1, JIT_V1);
    ref2 = jit_bnei_p(jit_forward(), JIT_R1, scheme_native_closure_type);
    CHECK_LIMIT();
  } else {
    ref = ref2 = NULL;
  }
  /* Right kind of function. Extract data and check stack depth: */
  jit_ldxi_p(JIT_R0, JIT_V1, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_i(JIT_R2, JIT_R0, &((Scheme_Native_Closure_Data *)0x0)->max_let_depth);
  jit_ldi_p(JIT_R1, &MZ_RUNSTACK_START);
  jit_subr_ul(JIT_R1, JIT_RUNSTACK, JIT_R1);
  ref4 = jit_bltr_ul(jit_forward(), JIT_R1, JIT_R2);
  CHECK_LIMIT();

  /* Fast jump ok (proc will check argc).
     At this point, JIT_V1 and JIT_R0 are set up. */

  /* Check for thread swap: */
  (void)jit_movi_p(JIT_R1, &scheme_fuel_counter);
  jit_ldr_i(JIT_R2, JIT_R1);
  ref5 = jit_bgti_i(jit_forward(), JIT_R2, 0);
  if (need_set_rs) {
    JIT_UPDATE_THREAD_RSPTR();
  }
  /* FIXME 3m: need to move JIT_V1 and JIT_R0 to GC-visible place */
  mz_push_local_p(JIT_R0, JIT_LOCAL2);
  (void)jit_calli(thread_block);
  mz_pop_local_p(JIT_R0, JIT_LOCAL2);
  mz_patch_branch(ref5);
#ifndef FUEL_AUTODECEREMENTS
  jit_subi_p(JIT_R2, JIT_R2, 0x1);
  jit_str_i(JIT_R1, JIT_R2);
#endif

  /* Copy args to runstack base: */
  jit_subi_p(JIT_R2, JIT_RUNSTACK_BASE, WORDS_TO_BYTES(num_rands)); 
  for (i = num_rands; i--; ) {
    jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(i));
    jit_stxi_p(WORDS_TO_BYTES(i), JIT_R2, JIT_R1);
    CHECK_LIMIT();
  }
  jit_movr_p(JIT_RUNSTACK, JIT_R2);

  /* Extract function and data: */
  jit_movr_p(JIT_R2, JIT_V1);
  if (direct_native) {
    jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure_Data *)0x0)->u.tail_code);
  } else {
    jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure_Data *)0x0)->arity_code);
  }
  /* Set up arguments; JIT_RUNSTACK and JIT_RUNSTACK_BASE must also be ready */
  jit_movr_p(JIT_R0, JIT_R2);
  jit_movi_i(JIT_R1, num_rands);
  jit_movr_p(JIT_R2, JIT_RUNSTACK);
  /* Now jump: */
  jit_jmpr(JIT_V1);
  CHECK_LIMIT();

  /* The slow way: */
  /*  JIT_R0, JIT_V1, and JIT_RUNSTACK must be intact! */
  if (!direct_native) {
    mz_patch_branch(ref);
    mz_patch_branch(ref2);
  }
  mz_patch_branch(ref4);
  CHECK_LIMIT();
  if (need_set_rs) {
    JIT_UPDATE_THREAD_RSPTR();
  }
  jit_movi_i(JIT_R0, num_rands);
  mz_prepare(3);
  CHECK_LIMIT();
  jit_pusharg_p(JIT_RUNSTACK);
  jit_pusharg_i(JIT_R0);
  jit_pusharg_p(JIT_V1);
  (void)mz_finish(_scheme_tail_apply_from_native);
  /* Pop saved runstack val and return: */
  mz_pop_local_p(JIT_NOT_RET, JIT_LOCAL1);
  jit_sti_p(&scheme_current_runstack, JIT_NOT_RET);
  jit_ret();

  __END_SHORT_JUMPS__(num_rands < 100);

  return 1;
}

static int generate_direct_prim_non_tail_call(mz_jit_state *jitter, int num_rands, int multi_ok, int pop_and_jump)
{
  /* See generate_prim_non_tail_call for assumptions. */

  if (pop_and_jump) {
    mz_prolog(JIT_R1);
  }

  if (num_rands == 1) {
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    JIT_UPDATE_THREAD_RSPTR();
  }

  jit_movi_i(JIT_R1, num_rands);
  mz_prepare(2);
  CHECK_LIMIT();
  jit_pusharg_p(JIT_RUNSTACK);
  jit_pusharg_i(JIT_R1);
  mz_finishr(JIT_V1);
  CHECK_LIMIT();
  jit_retval(JIT_R0);
#if 0
  /* No need to check for multi values or tail-call, because
     we only use this for noncm primitives. */
  jit_insn *ref;
  if (!multi_ok) {
    jit_insn *refm;
    refm = jit_beqi_p(jit_forward(), JIT_R0, SCHEME_MULTIPLE_VALUES);
    mz_patch_branch_at(refm, bad_result_arity_code);
  }
  ref = jit_bnei_p(jit_forward(), JIT_R0, SCHEME_TAIL_CALL_WAITING);
  CHECK_LIMIT();
  mz_prepare(1);
  jit_pusharg_p(JIT_R0);
  if (multi_ok) {
    (void)mz_finish(scheme_force_value_same_mark);
  } else {
    (void)mz_finish(scheme_force_one_value_same_mark);
  }
  jit_retval(JIT_R0);
  mz_patch_branch(ref);
#endif

  if (num_rands == 1) {
    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
  }

  if (pop_and_jump) {
    mz_epilog(JIT_V1);
  }

  return 1;
}

static int generate_non_tail_call(mz_jit_state *jitter, int num_rands, int direct_native, int need_set_rs, 
				  int multi_ok, int pop_and_jump)
{
  /* Non-tail call: */
  GC_CAN_IGNORE jit_insn *ref, *ref2, *ref4, *ref5, *ref6, *ref7, *ref8, *ref9;
  GC_CAN_IGNORE jit_insn *ref10;

  __START_SHORT_JUMPS__(num_rands < 100);

  if (pop_and_jump) {
    mz_prolog(JIT_R1);
  }

  /* Check for inlined prim types */
  if (!direct_native) {
    ref = jit_bmsi_ul(jit_forward(), JIT_V1, 0x1);
    jit_ldr_s(JIT_R1, JIT_V1);
    ref2 = jit_bnei_i(jit_forward(), JIT_R1, scheme_native_closure_type);
    CHECK_LIMIT();
  } else {
    ref = ref2 = NULL;
  }

  /* Before inlined native, check max let depth */
  jit_ldxi_p(JIT_R0, JIT_V1, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_i(JIT_R2, JIT_R0, &((Scheme_Native_Closure_Data *)0x0)->max_let_depth);
  jit_ldi_p(JIT_R1, &MZ_RUNSTACK_START);
  jit_subr_ul(JIT_R1, JIT_RUNSTACK, JIT_R1);
  ref4 = jit_bltr_ul(jit_forward(), JIT_R1, JIT_R2);
  CHECK_LIMIT();

  /* Before inlined native, check stack depth: */
  (void)jit_movi_p(JIT_R1, &scheme_stack_boundary);
  jit_ldr_i(JIT_R1, JIT_R1);
  ref9 = jit_bltr_ul(jit_forward(), JIT_STACK, JIT_R1);
  CHECK_LIMIT();
  
  /* Fast inlined-native jump ok (proc will check argc); */
  /* extract function and data: */
  mz_prepare(3);
  jit_pusharg_p(JIT_RUNSTACK);
  jit_movi_i(JIT_R1, num_rands);
  jit_pusharg_i(JIT_R1);
  jit_pusharg_p(JIT_V1);
  if (direct_native) {
    (void)mz_finish(jump_to_native_code);
  } else {
    (void)mz_finish(jump_to_native_arity_code);
  }
  CHECK_LIMIT();
  jit_retval(JIT_R0);
  if (!multi_ok) {
    jit_insn *refm;
    __END_SHORT_JUMPS__(num_rands < 100);
    refm = jit_beqi_p(jit_forward(), JIT_R0, SCHEME_MULTIPLE_VALUES);
    mz_patch_branch_at(refm, bad_result_arity_code);
    __START_SHORT_JUMPS__(num_rands < 100);
  }
  ref6 = jit_bnei_p(jit_forward(), JIT_R0, SCHEME_TAIL_CALL_WAITING);
  if (need_set_rs) {
    JIT_UPDATE_THREAD_RSPTR();
  }
  mz_prepare(1);
  jit_pusharg_p(JIT_R0);
  if (multi_ok) {
    (void)mz_finish(scheme_force_value_same_mark);
  } else {
    (void)mz_finish(scheme_force_one_value_same_mark);
  }
  ref5 = jit_jmpi(jit_forward());
  CHECK_LIMIT();

  /* Maybe it's a prim? */
  if (!direct_native) {
    mz_patch_branch(ref2);
    ref2 = jit_bnei_i(jit_forward(), JIT_R1, scheme_prim_type);
    /* It's a prim. Arity check... fast path when exactly equal to min, only: */
    jit_ldxi_i(JIT_R0, JIT_V1, &((Scheme_Primitive_Proc *)0x0)->mina);
    ref7 = jit_bnei_i(jit_forward(), JIT_R0, num_rands);
    /* Fast prim application */
    jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Primitive_Proc *)0x0)->prim_val);
    if (need_set_rs) {
      JIT_UPDATE_THREAD_RSPTR();
    }
    mz_prepare(2);
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_R0);
    (void)mz_finishr(JIT_V1);
    CHECK_LIMIT();
    jit_retval(JIT_R0);
    if (!multi_ok) {
      jit_insn *refm;
      __END_SHORT_JUMPS__(num_rands < 100);
      refm = jit_beqi_p(jit_forward(), JIT_R0, SCHEME_MULTIPLE_VALUES);
      mz_patch_branch_at(refm, bad_result_arity_code);
      __START_SHORT_JUMPS__(num_rands < 100);
    }
    ref10 = jit_bnei_p(jit_forward(), JIT_R0, SCHEME_TAIL_CALL_WAITING);
    mz_prepare(1);
    jit_pusharg_p(JIT_R0);
    if (multi_ok) {
      (void)mz_finish(scheme_force_value_same_mark);
    } else {
      (void)mz_finish(scheme_force_one_value_same_mark);
    }
    CHECK_LIMIT();
    ref8 = jit_jmpi(jit_forward());
  } else {
    ref2 = ref7 = ref8 = ref10 = NULL;
  }

  /* The slow way: */
  if (!direct_native) {
    mz_patch_branch(ref);
    mz_patch_branch(ref2);
    mz_patch_branch(ref7);
  }
  mz_patch_branch(ref4);
  mz_patch_branch(ref9);
  if (need_set_rs) {
    JIT_UPDATE_THREAD_RSPTR();
  }
  jit_movi_i(JIT_R0, num_rands);
  mz_prepare(3);
  CHECK_LIMIT();
  jit_pusharg_p(JIT_RUNSTACK);
  jit_pusharg_i(JIT_R0);
  jit_pusharg_p(JIT_V1);
  if (multi_ok) {
    (void)mz_finish(_scheme_apply_multi_from_native);
  } else {
    (void)mz_finish(_scheme_apply_from_native);
  }
  mz_patch_ucbranch(ref5);
  if (!direct_native) {
    mz_patch_ucbranch(ref8);
  }
  jit_retval(JIT_R0);
  mz_patch_branch(ref6);
  if (!direct_native) {
    mz_patch_branch(ref10);
  }
  if (pop_and_jump) {
    mz_epilog(JIT_V1);
  }

  __END_SHORT_JUMPS__(num_rands < 100);

  return 1;
}

typedef struct {
  int num_rands;
  mz_jit_state *old_jitter;
  int multi_ok;
  int is_tail;
  int direct_prim, direct_native;
} Generate_Call_Data;

int do_generate_shared_call(mz_jit_state *jitter, void *_data)
{
  Generate_Call_Data *data = (Generate_Call_Data *)_data;
  
#ifdef MZ_USE_JIT_PPC
  jitter->js.jitl.nbArgs = data->old_jitter->js.jitl.nbArgs;
#endif

  if (data->is_tail) {
    if (data->direct_prim)
      return generate_direct_prim_tail_call(jitter, data->num_rands);
    else
      return generate_tail_call(jitter, data->num_rands, data->direct_native, 1);
  } else {
    int ok;
    void *code, *code_end;

    code = jit_get_ip().ptr;

    if (data->direct_prim)
      ok = generate_direct_prim_non_tail_call(jitter, data->num_rands, data->multi_ok, 1);
    else
      ok = generate_non_tail_call(jitter, data->num_rands, data->direct_native, 1, data->multi_ok, 1);

    code_end = jit_get_ip().ptr;
    if (jitter->retain_start)
      add_symbol((unsigned long)code, (unsigned long)code_end - 1, scheme_false, 0);

    return ok;
  }
}

static void *generate_shared_call(int num_rands, mz_jit_state *old_jitter, int multi_ok, int is_tail, 
				  int direct_prim, int direct_native)
{
  Generate_Call_Data data;

  data.num_rands = num_rands;
  data.old_jitter = old_jitter;
  data.multi_ok = multi_ok;
  data.is_tail = is_tail;
  data.direct_prim = direct_prim;
  data.direct_native = direct_native;

  return generate_one(old_jitter, do_generate_shared_call, &data, 0, NULL);
}

static int generate_app(Scheme_App_Rec *app, Scheme_Object **alt_rands, int num_rands, 
			mz_jit_state *jitter, int is_tail, int multi_ok)
{
  int i, direct_prim = 0, need_non_tail = 0, direct_native = 0, offset;
  Scheme_Object *rator, *v;
  int reorder_ok = 0;
  START_JIT_DATA();

  rator = (alt_rands ? alt_rands[0] : app->args[0]);

  if (SCHEME_PRIMP(rator)) {
    if ((num_rands >= ((Scheme_Primitive_Proc *)rator)->mina)
	&& ((num_rands <= ((Scheme_Primitive_Proc *)rator)->maxa)
	    || (((Scheme_Primitive_Proc *)rator)->maxa < 0))
	&& is_noncm(rator))
      direct_prim = 1;
  } else {
    Scheme_Type t;
    t = SCHEME_TYPE(rator);
    if ((t == scheme_local_type) || (t > _scheme_values_types_)) {
      /* We can re-order evaluation. */
      reorder_ok = 1;
    }
  }

  if (num_rands) {
    if (!direct_prim || (num_rands > 1)) {
      jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(num_rands));
      mz_runstack_pushed(jitter, num_rands);
    } else {
      mz_runstack_skipped(jitter, 1);
    }
  }

  for (i = 0; i <= num_rands; i++) {
    v = (alt_rands ? alt_rands[i] : app->args[i]);
    if (!is_simple(v, INIT_SIMPLE_DEPTH, 1)) {
      need_non_tail = 1;
      break;
    }
  }

  if (need_non_tail) {
    offset = generate_non_tail_mark_pos_prefix(jitter);
    CHECK_LIMIT();
  } else
    offset = 0;

  if (!direct_prim && !reorder_ok) {
    generate_non_tail(rator, jitter, 0, !need_non_tail);
    CHECK_LIMIT();

    if (num_rands) {
      /* Save rator where GC can see it */
      jit_stxi_p(WORDS_TO_BYTES(num_rands - 1 + offset), JIT_RUNSTACK, JIT_R0);
    } else {
      jit_movr_p(JIT_V1, JIT_R0);
    }
  }

  for (i = 0; i < num_rands; i++) {
    PAUSE_JIT_DATA();
    generate_non_tail(alt_rands ? alt_rands[i+1] : app->args[i+1], jitter, 0, !need_non_tail);
    RESUME_JIT_DATA();
    CHECK_LIMIT();
    if ((i == num_rands - 1) && !direct_prim && !reorder_ok) {
      /* Move rator back to register: */
      jit_ldxi_p(JIT_V1, JIT_RUNSTACK, WORDS_TO_BYTES(i + offset));
    }
    if (!direct_prim || (num_rands > 1)) {
      jit_stxi_p(WORDS_TO_BYTES(i + offset), JIT_RUNSTACK, JIT_R0);
    }
  }

  if (need_non_tail) {
    /* Uses JIT_R2: */
    generate_non_tail_mark_pos_suffix(jitter);
    CHECK_LIMIT();
  }

  if (direct_prim) {
    (void)jit_movi_p(JIT_V1, ((Scheme_Primitive_Proc *)rator)->prim_val);
    if (num_rands == 1) {
      mz_runstack_unskipped(jitter, 1);
    } else {
      JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
    }
    LOG_IT(("direct: %s\n", ((Scheme_Primitive_Proc *)rator)->name));
  }

  if (reorder_ok) {
    generate(rator, jitter, 0, 0);
    CHECK_LIMIT();
    jit_movr_p(JIT_V1, JIT_R0);
  }

  END_JIT_DATA(20);

  if (SAME_TYPE(SCHEME_TYPE(rator), scheme_local_type)) {
    if (mz_is_closure(jitter, SCHEME_LOCAL_POS(rator), num_rands)) {
      direct_native = 1;
    }
  }

  if (num_rands >= MAX_SHARED_CALL_RANDS) {
    if (is_tail) {
      if (direct_prim)
	generate_direct_prim_tail_call(jitter, num_rands);
      else
	generate_tail_call(jitter, num_rands, direct_native, jitter->need_set_rs);
    } else {
      if (direct_prim)
	generate_direct_prim_non_tail_call(jitter, num_rands, multi_ok, 0);
      else
	generate_non_tail_call(jitter, num_rands, direct_native, jitter->need_set_rs, multi_ok, 0);
    }
  } else {
    /* Jump to code to implement a tail call for num_rands arguments */
    void *code;
    int dp = (direct_prim ? 1 : (direct_native ? 2 : 0));
    if (is_tail) {
      jit_insn *refm;
      if (!shared_tail_code[dp][num_rands]) {
	code = generate_shared_call(num_rands, jitter, multi_ok, is_tail, direct_prim, direct_native);
	shared_tail_code[dp][num_rands] = code;
      }
      code = shared_tail_code[dp][num_rands];
      refm = jit_jmpi(jit_forward());
      mz_patch_ucbranch_at(refm, code);
    } else {
      int mo = (multi_ok ? 1 : 0);

      if (!shared_non_tail_code[dp][num_rands][mo]) {
	code = generate_shared_call(num_rands, jitter, multi_ok, is_tail, direct_prim, direct_native);
	shared_non_tail_code[dp][num_rands][mo] = code;
      }
      code = shared_non_tail_code[dp][num_rands][mo];

      (void)jit_calli(code);

      /* Whether we call a prim, a native, or something else,
	 scheme_current_runstack is up-to-date. */
      jitter->need_set_rs = 0;
    }
  }

  END_JIT_DATA(need_non_tail ? 22 : 4);
    
  return is_tail ? 2 : 1;
}

static void generate_arith_slow_path(mz_jit_state *jitter, Scheme_Object *rator, 
				     jit_insn **_ref, jit_insn **_ref4,
				     jit_insn **for_branch, 
				     int orig_args, int reversed, int use_v, int v)
{
  jit_insn *ref, *ref4;

  (void)jit_movi_p(JIT_R2, ((Scheme_Primitive_Proc *)rator)->prim_val);
  if (for_branch) {
    ref4 = jit_movi_p(JIT_V1, jit_forward());
    mz_push_local_p(JIT_V1, JIT_LOCAL2);
  } else
    ref4 = NULL;
  ref = jit_movi_p(JIT_V1, jit_forward());

  if (orig_args == 1) {
    if (for_branch) {
      (void)jit_jmpi(call_original_unary_arith_for_branch_code);
    } else {
      (void)jit_jmpi(call_original_unary_arith_code);
    }
  } else {
    if (use_v) {
      (void)jit_movi_p(JIT_R1, scheme_make_integer(v));
      reversed = !reversed;
    }

    if (for_branch) {
      if (reversed) {
	(void)jit_jmpi(call_original_binary_rev_arith_for_branch_code);
      } else {
	(void)jit_jmpi(call_original_binary_arith_for_branch_code);
      }
    } else {
      if (reversed) {
	(void)jit_jmpi(call_original_binary_rev_arith_code);
      } else {
	(void)jit_jmpi(call_original_binary_arith_code);
      }
    }
  }

  *_ref = ref;
  *_ref4 = ref4;
}

static int generate_arith(mz_jit_state *jitter, Scheme_Object *rator, Scheme_Object *rand, Scheme_Object *rand2, 
			  int orig_args, int arith, int cmp, int v, jit_insn **for_branch, int branch_short)
{
  GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3, *ref4, *refslow;
  int skipped, simple_rand, reversed = 0;

  LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)rator)->name));

  if (rand2) {
    if (SCHEME_INTP(rand2)) {
      v = SCHEME_INT_VAL(rand2);
      rand2 = NULL;
    } else if (SCHEME_INTP(rand) && (arith != -1)) {
      v = SCHEME_INT_VAL(rand);
      cmp = -cmp;
      rand = rand2;
      rand2 = NULL;
      reversed = 1;
    } else if ((arith != -1)
	       && SAME_TYPE(SCHEME_TYPE(rand2), scheme_local_type)
	       && !SAME_TYPE(SCHEME_TYPE(rand), scheme_local_type)) {
      Scheme_Object *t = rand2;
      rand2 = rand;
      rand = t;
      cmp = -cmp;
      reversed = 1;
    }
  }

  if (rand2) {
    simple_rand = SAME_TYPE(SCHEME_TYPE(rand), scheme_local_type);
  } else
    simple_rand = 0;

  if (rand2 && !simple_rand)
    skipped = orig_args - 1;    
  else
    skipped = orig_args;

  mz_runstack_skipped(jitter, skipped);

  if (rand2 && !simple_rand) {
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    mz_runstack_pushed(jitter, 1);
    generate_non_tail(rand, jitter, 0, 1);
    CHECK_LIMIT();
    jit_str_p(JIT_RUNSTACK, JIT_R0);
  }

  generate_non_tail(rand2 ? rand2 : rand, jitter, 0, 1);
  CHECK_LIMIT();

  if (simple_rand) {
    int pos;
    pos = mz_remap(SCHEME_LOCAL_POS(rand));
    jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(pos));

    jit_andr_ul(JIT_R2, JIT_R0, JIT_R1);
    __START_SHORT_JUMPS__(1);
    ref2 = jit_bmsi_ul(jit_forward(), JIT_R2, 0x1);
    __END_SHORT_JUMPS__(1);

    /* Slow path */
    refslow = _jit.x.pc;
    generate_arith_slow_path(jitter, rator, &ref, &ref4, for_branch, orig_args, reversed, 0, 0);

    __START_SHORT_JUMPS__(1);
    mz_patch_branch(ref2);
    __END_SHORT_JUMPS__(1);
    CHECK_LIMIT();
  } else if (rand2) {
    jit_ldr_p(JIT_R1, JIT_RUNSTACK);
    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    mz_runstack_popped(jitter, 1);

    jit_andr_ul(JIT_R2, JIT_R0, JIT_R1);
    __START_SHORT_JUMPS__(1);
    ref2 = jit_bmsi_ul(jit_forward(), JIT_R2, 0x1);
    __END_SHORT_JUMPS__(1);

    /* Slow path */
    refslow = _jit.x.pc;
    generate_arith_slow_path(jitter, rator, &ref, &ref4, for_branch, orig_args, reversed, 0, 0);

    __START_SHORT_JUMPS__(1);
    mz_patch_branch(ref2);
    __END_SHORT_JUMPS__(1);
    CHECK_LIMIT();
  } else {
    __START_SHORT_JUMPS__(1);
    ref2 = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
    __END_SHORT_JUMPS__(1);

    /* Slow path */
    refslow = _jit.x.pc;
    generate_arith_slow_path(jitter, rator, &ref, &ref4, for_branch, orig_args, reversed, 1, v);

    __START_SHORT_JUMPS__(1);
    mz_patch_branch(ref2);
    __END_SHORT_JUMPS__(1);
  }

  mz_runstack_unskipped(jitter, skipped);

  __START_SHORT_JUMPS__(branch_short);

  if (arith) {
    if (rand2) {
      if (arith > 0) {
	jit_andi_ul(JIT_R2, JIT_R1, (~0x1));
	(void)jit_boaddr_i(refslow, JIT_R2, JIT_R0);
	jit_movr_p(JIT_R0, JIT_R2);
      } else {
	jit_movr_p(JIT_R2, JIT_R1);
	(void)jit_bosubr_i(refslow, JIT_R2, JIT_R0);
	jit_ori_ul(JIT_R0, JIT_R2, 0x1);
      }
    } else {
      jit_movr_p(JIT_R2, JIT_R0);
      if (arith > 0) {
	(void)jit_boaddi_i(refslow, JIT_R2, v << 1);
      } else {
	(void)jit_bosubi_i(refslow, JIT_R2, v << 1);
      }
      jit_movr_p(JIT_R0, JIT_R2);
    }
    jit_patch_movi(ref, (_jit.x.pc));
  } else {
    switch (cmp) {
    case -2:
      if (rand2) {
	ref3 = jit_bger_i(jit_forward(), JIT_R1, JIT_R0);
      } else {
	ref3 = jit_bgei_i(jit_forward(), JIT_R0, scheme_make_integer(v));
      }
      break;
    case -1:
      if (rand2) {
	ref3 = jit_bgtr_i(jit_forward(), JIT_R1, JIT_R0);
      } else {
	ref3 = jit_bgti_i(jit_forward(), JIT_R0, scheme_make_integer(v));
      }
      break;
    case 0:
      if (rand2) {
	ref3 = jit_bner_i(jit_forward(), JIT_R1, JIT_R0);
      } else {
	ref3 = jit_bnei_i(jit_forward(), JIT_R0, scheme_make_integer(v));
      }
      break;
    case 1:
      if (rand2) {
	ref3 = jit_bltr_i(jit_forward(), JIT_R1, JIT_R0);
      } else {
	ref3 = jit_blti_i(jit_forward(), JIT_R0, scheme_make_integer(v));
      }
      break;
    case 2:
    default:
      if (rand2) {
	ref3 = jit_bler_i(jit_forward(), JIT_R1, JIT_R0);
      } else {
	ref3 = jit_blei_i(jit_forward(), JIT_R0, scheme_make_integer(v));
      }
      break;
    }

    if (for_branch) {
      for_branch[0] = ref3;
      for_branch[2] = ref;
      jit_patch_movi(ref4, (_jit.x.pc));
    } else {
      (void)jit_movi_p(JIT_R0, scheme_true);
      ref2 = jit_jmpi(jit_forward());
      mz_patch_branch(ref3);
      (void)jit_movi_p(JIT_R0, scheme_false);
      mz_patch_ucbranch(ref2);
      jit_patch_movi(ref, (_jit.x.pc));
    }
  }

  __END_SHORT_JUMPS__(branch_short);

  return 1;  
}

static int generate_inlined_constant_test(mz_jit_state *jitter, Scheme_App2_Rec *app,
					  Scheme_Object *cnst, jit_insn **for_branch, int branch_short)
{
  GC_CAN_IGNORE jit_insn *ref, *ref2;

  LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)app->rator)->name));

  mz_runstack_skipped(jitter, 1);

  generate_non_tail(app->rand, jitter, 0, 1);
  CHECK_LIMIT();

  mz_runstack_unskipped(jitter, 1);

  __START_SHORT_JUMPS__(branch_short);

  ref = jit_bnei_p(jit_forward(), JIT_R0, cnst);
  if (for_branch) {
    for_branch[0] = ref;
  } else {
    (void)jit_movi_p(JIT_R0, scheme_true);
    ref2 = jit_jmpi(jit_forward());
    mz_patch_branch(ref);
    (void)jit_movi_p(JIT_R0, scheme_false);
    mz_patch_ucbranch(ref2);
  }

  __END_SHORT_JUMPS__(branch_short);

  return 1;
}

static int generate_inlined_type_test(mz_jit_state *jitter, Scheme_App2_Rec *app,
				      Scheme_Type ty, jit_insn **for_branch, int branch_short)
{
  GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3;

  LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)app->rator)->name));

  mz_runstack_skipped(jitter, 1);

  generate_non_tail(app->rand, jitter, 0, 1);
  CHECK_LIMIT();

  mz_runstack_unskipped(jitter, 1);

  __START_SHORT_JUMPS__(branch_short);

  ref = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
  jit_ldxi_s(JIT_R0, JIT_R0, &((Scheme_Object *)0x0)->type);
  ref3 = jit_bnei_p(jit_forward(), JIT_R0, ty);
  if (for_branch) {
    for_branch[0] = ref;
    for_branch[1] = ref3;
  } else {
    (void)jit_movi_p(JIT_R0, scheme_true);
    ref2 = jit_jmpi(jit_forward());
    mz_patch_branch(ref);
    mz_patch_branch(ref3);
    (void)jit_movi_p(JIT_R0, scheme_false);
    mz_patch_ucbranch(ref2);
  }

  __END_SHORT_JUMPS__(branch_short);

  return 1;
}

static int generate_inlined_unary(mz_jit_state *jitter, Scheme_App2_Rec *app, int is_tail, int multi_ok, 
				  jit_insn **for_branch, int branch_short)
{
  Scheme_Object *rator = app->rator;

  if (!SCHEME_PRIMP(rator))
    return 0;

  if (!(SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_IS_UNARY_INLINED))
    return 0;

  if (IS_NAMED_PRIM(rator, "not")) {
    generate_inlined_constant_test(jitter, app, scheme_false, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "null?")) {
    generate_inlined_constant_test(jitter, app, scheme_null, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "pair?")) {
    generate_inlined_type_test(jitter, app, scheme_pair_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "symbol?")) {
    generate_inlined_type_test(jitter, app, scheme_symbol_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "syntax?")) {
    generate_inlined_type_test(jitter, app, scheme_stx_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "char?")) {
    generate_inlined_type_test(jitter, app, scheme_char_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "zero?")) {
    generate_arith(jitter, rator, app->rand, NULL, 1, 0, 0, 0, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "negative?")) {
    generate_arith(jitter, rator, app->rand, NULL, 1, 0, -2, 0, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "positive?")) {
    generate_arith(jitter, rator, app->rand, NULL, 1, 0, 2, 0, for_branch, branch_short);
    return 1;
  } else if (!for_branch) {
    if (IS_NAMED_PRIM(rator, "car")
	|| IS_NAMED_PRIM(rator, "cdr")) {
      GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3;
      int is_car;

      LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)rator)->name));

      is_car = IS_NAMED_PRIM(rator, "car");

      mz_runstack_skipped(jitter, 1);

      generate_non_tail(app->rand, jitter, 0, 1);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      __START_SHORT_JUMPS__(1);

      ref = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
      jit_ldxi_s(JIT_R1, JIT_R0, &((Scheme_Object *)0x0)->type);
      ref3 = jit_bnei_p(jit_forward(), JIT_R1, scheme_pair_type);
      if (is_car) {
	(void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Simple_Object *)0x0)->u.pair_val.car);
      } else {
	(void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Simple_Object *)0x0)->u.pair_val.cdr);
      }
      ref2 = jit_jmpi(jit_forward());
      mz_patch_branch(ref);
      mz_patch_branch(ref3);
      __END_SHORT_JUMPS__(1);

      if (is_car) {
	(void)jit_jmpi(bad_car_code);
      } else {
	(void)jit_jmpi(bad_cdr_code);
      }

      __START_SHORT_JUMPS__(1);
      mz_patch_ucbranch(ref2);
      __END_SHORT_JUMPS__(1);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "syntax-e")) {
      LOG_IT(("inlined syntax-e\n"));

      mz_runstack_skipped(jitter, 1);

      generate_non_tail(app->rand, jitter, 0, 1);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      (void)jit_calli(syntax_e_code);
      
      return 1;
    } else if (IS_NAMED_PRIM(rator, "add1")) {
      generate_arith(jitter, rator, app->rand, NULL, 1, 1, 0, 1, NULL, 1);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "sub1")) {
      generate_arith(jitter, rator, app->rand, NULL, 1, -1, 0, 1, NULL, 1);
      return 1;
    }
  }

  if (!for_branch) {
    scheme_console_printf("Inlining expected.\n");
    abort();
  }

  return 0;
}

static int generate_inlined_binary(mz_jit_state *jitter, Scheme_App3_Rec *app, int is_tail, int multi_ok, 
				   jit_insn **for_branch, int branch_short)
{
  Scheme_Object *rator = app->rator;

  if (!SCHEME_PRIMP(rator))
    return 0;

  if (!(SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_IS_BINARY_INLINED))
    return 0;

  if (IS_NAMED_PRIM(rator, "eq?")) {
    Scheme_Object *a1, *a2;
    GC_CAN_IGNORE jit_insn *ref, *ref2;

    LOG_IT(("inlined eq?\n"));
    
    a1 = app->rand1;
    if (SCHEME_TYPE(a1) > _scheme_values_types_) {
      a2 = app->rand2;
    } else {
      a1 = app->rand2;
      a2 = app->rand1;
    }

    if (SCHEME_TYPE(a1) > _scheme_values_types_) {
      /* Compare to constant: */
      mz_runstack_skipped(jitter, 2);

      generate_non_tail(a2, jitter, 0, 1);
      CHECK_LIMIT();
      
      mz_runstack_unskipped(jitter, 2);

      if (!SCHEME_INTP(a1))
	mz_retain(a1);
      
      __START_SHORT_JUMPS__(branch_short);
      
      ref = jit_bnei_p(jit_forward(), JIT_R0, a1);
      if (for_branch) {
	for_branch[0] = ref;
      } else {
	(void)jit_movi_p(JIT_R0, scheme_true);
	ref2 = jit_jmpi(jit_forward());
	mz_patch_branch(ref);
	(void)jit_movi_p(JIT_R0, scheme_false);
	mz_patch_ucbranch(ref2);
      }
      
      __END_SHORT_JUMPS__(branch_short);
    } else {
      /* Two complex expressions: */
      mz_runstack_skipped(jitter, 1);

      jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
      mz_runstack_pushed(jitter, 1);
      
      generate_non_tail(a2, jitter, 0, 1);
      CHECK_LIMIT();
      jit_str_p(JIT_RUNSTACK, JIT_R0);
      generate_non_tail(a1, jitter, 0, 1);
      CHECK_LIMIT();
      jit_ldr_p(JIT_R1, JIT_RUNSTACK);

      jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
      mz_runstack_popped(jitter, 1);
      
      mz_runstack_unskipped(jitter, 1);
      
      __START_SHORT_JUMPS__(branch_short);
      
      ref = jit_bner_p(jit_forward(), JIT_R0, JIT_R1);
      if (for_branch) {
	for_branch[0] = ref;
      } else {
	(void)jit_movi_p(JIT_R0, scheme_true);
	ref2 = jit_jmpi(jit_forward());
	mz_patch_branch(ref);
	(void)jit_movi_p(JIT_R0, scheme_false);
	mz_patch_ucbranch(ref2);
      }
      
      __END_SHORT_JUMPS__(branch_short);      
    }
    
    return 1;
  } else if (IS_NAMED_PRIM(rator, "=")) {
    generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, 0, 0, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "<=")) {
    generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, -1, 0, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "<")) {
    generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, -2, 0, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, ">=")) {
    generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, 1, 0, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, ">")) {
    generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, 2, 0, for_branch, branch_short);
    return 1;
  } else if (!for_branch) {
    if (IS_NAMED_PRIM(rator, "+")) {
      generate_arith(jitter, rator, app->rand1, app->rand2, 2, 1, 0, 0, NULL, 1);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "-")) {
      generate_arith(jitter, rator, app->rand1, app->rand2, 2, -1, 0, 0, NULL, 1);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "vector-ref")) {
      int simple;

      LOG_IT(("inlined vector-ref?\n"));

      simple = (SCHEME_INTP(app->rand2)
		&& (SCHEME_INT_VAL(app->rand2) >= 0));
      
      if (simple)
	mz_runstack_skipped(jitter, 2);
      else
	mz_runstack_skipped(jitter, 1);

      if (!simple) {
	jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
	mz_runstack_pushed(jitter, 1);
      }

      generate_non_tail(app->rand1, jitter, 0, 1);
      CHECK_LIMIT();

      if (!simple) {
	jit_str_p(JIT_RUNSTACK, JIT_R0);

	generate_non_tail(app->rand2, jitter, 0, 1);
	CHECK_LIMIT();

	jit_movr_p(JIT_R1, JIT_R0);
	jit_ldr_p(JIT_R0, JIT_RUNSTACK);

	jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
	mz_runstack_popped(jitter, 1);
	
	(void)jit_calli(vector_ref_check_index_code);
      } else {
	int offset;
	offset = SCHEME_INT_VAL(app->rand2);
	(void)jit_movi_p(JIT_R1, offset);
	offset = ((int)&SCHEME_VEC_ELS(0x0)) + WORDS_TO_BYTES(SCHEME_INT_VAL(app->rand2));
	jit_movi_i(JIT_V1, offset);
	(void)jit_calli(vector_ref_code);
      }

      if (simple)
	mz_runstack_unskipped(jitter, 2);
      else
	mz_runstack_unskipped(jitter, 1);

      return 1;
    }
  }

  if (!for_branch) {
    scheme_console_printf("Inlining expected.\n");
    abort();
  }

  return 0;
}

int generate_inlined_test(mz_jit_state *jitter, Scheme_Object *obj, int branch_short, jit_insn **refs)
{
  switch (SCHEME_TYPE(obj)) {
  case scheme_application2_type:
    return generate_inlined_unary(jitter, (Scheme_App2_Rec *)obj, 0, 0, refs, branch_short);
  case scheme_application3_type:
    return generate_inlined_binary(jitter, (Scheme_App3_Rec *)obj, 0, 0, refs, branch_short);
  }

  return 0;
}


/*========================================================================*/
/*                           lambda codegen                               */
/*========================================================================*/

static void ensure_closure_native(Scheme_Closure_Data *data, 
				  Scheme_Native_Closure_Data *case_lam)
{
  if (!data->native_code || SCHEME_FALSEP((Scheme_Object *)data->native_code)) {
    Scheme_Native_Closure_Data *code;
    code = scheme_generate_lambda(data, 0, case_lam);
    data->native_code = code;
  }
}

static int generate_closure(Scheme_Closure_Data *data, 
			    mz_jit_state *jitter)
{
  Scheme_Native_Closure_Data *code;
  
  ensure_closure_native(data, NULL);
  code = data->native_code;

  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
  mz_prepare(1);
  mz_retain(code);
  (void)jit_movi_p(JIT_R0, code); /* !! */
  jit_pusharg_p(JIT_R0);
  (void)mz_finish(scheme_make_native_closure);
  jit_retval(JIT_R0);

  return 1;
}

static int generate_closure_fill(Scheme_Closure_Data *data, 
				 mz_jit_state *jitter)
{
  /* Fill in closure */
  int j, size, pos;
  mzshort *map;
  size = data->closure_size;
  map = data->closure_map;
  jit_addi_p(JIT_R2, JIT_R0, &((Scheme_Native_Closure *)0x0)->vals);
  for (j = 0; j < size; j++) {
    CHECK_LIMIT();
    pos = mz_remap(map[j]);
    jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
    jit_stxi_p(WORDS_TO_BYTES(j), JIT_R2, JIT_R1);
  }
  return 1;
}

Scheme_Native_Closure_Data *scheme_generate_case_lambda(Scheme_Case_Lambda *c)
{
  Scheme_Closure_Data *data;
  Scheme_Native_Closure_Data *ndata;
  Scheme_Object *name, *o;
  int max_let_depth = 0, i, count, is_method = 0;

  ndata = MALLOC_ONE_RT(Scheme_Native_Closure_Data);
  name = c->name;
  if (name && SCHEME_BOXP(name)) {
    name = SCHEME_BOX_VAL(name);
    is_method = 1;
  }
  ndata->u2.name = name;
  count = c->count;
  for (i = 0; i < count; i++) {
    o = c->array[i];
    if (SCHEME_PROCP(o))
      o = (Scheme_Object *)((Scheme_Closure *)o)->code;
    data = (Scheme_Closure_Data *)o;
    ensure_closure_native(data, ndata);
    if (data->native_code->max_let_depth > max_let_depth)
      max_let_depth = data->native_code->max_let_depth;
  }
  ndata->max_let_depth = max_let_depth;
  ndata->closure_size = -(count + 1); /* Indicates case-lambda */

  if (count) {
    o = c->array[0];
    if (SCHEME_PROCP(o))
      o = (Scheme_Object *)((Scheme_Closure *)o)->code;
    data = (Scheme_Closure_Data *)o;
    is_method = ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_IS_METHOD) ? 1 : 0);
  }

  generate_top_case_lambda_dispatch(c, ndata, is_method);

  return ndata;
}

static void ensure_case_closure_native(Scheme_Case_Lambda *c)
{
  if (!c->native_code || SCHEME_FALSEP((Scheme_Object *)c->native_code)) {
    Scheme_Native_Closure_Data *ndata;
    ndata = scheme_generate_case_lambda(c);
    c->native_code = ndata;
  }
}

static int generate_case_lambda(Scheme_Object *obj, mz_jit_state *jitter)
{
  Scheme_Case_Lambda *c = (Scheme_Case_Lambda *)obj;
  Scheme_Native_Closure_Data *ndata;
  Scheme_Closure_Data *data;
  Scheme_Object *o;
  int i, offset, count;

  ensure_case_closure_native(c);
  ndata = c->native_code;

  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
  mz_prepare(1);
  mz_retain(ndata);
  (void)jit_movi_p(JIT_R0, ndata); /* !! */
  jit_pusharg_p(JIT_R0);
  (void)mz_finish(scheme_make_native_case_closure);
  jit_retval(JIT_R1);
  CHECK_LIMIT();

  count = c->count;
  
  for (i = 0; i < count; i++) {
    o = c->array[i];
    if (SCHEME_PROCP(o))
      o = (Scheme_Object *)((Scheme_Closure *)o)->code;
    data = (Scheme_Closure_Data *)o;
    mz_pushr_p(JIT_R1); /* !!!!!!! */  
    generate_closure(data, jitter);
    CHECK_LIMIT();
    generate_closure_fill(data, jitter);
    CHECK_LIMIT();
    mz_popr_p(JIT_R1);
    offset = WORDS_TO_BYTES(i) + (unsigned long)&((Scheme_Native_Closure *)0x0)->vals;
    jit_stxi_p(offset, JIT_R1, JIT_R0);
    CHECK_LIMIT();
  }
  jit_movr_p(JIT_R0, JIT_R1);
  
  return 1;
}

/*========================================================================*/
/*                          non-tail codegen                              */
/*========================================================================*/

static int generate_non_tail_mark_pos_prefix(mz_jit_state *jitter)
{
  /* This part of a non-tail setup can be done once for a sequence
     of non-tail calls. In that case, pass 0 for the `mark_pos_ends'
     argument to generate_non_tail(), so that it can skip this prefix
     and suffix. In case this prefix needs to adjust the runstack,
     the result indicates the number of pushed values. */
  jit_ldi_i(JIT_R2, &scheme_current_cont_mark_pos);
  jit_addi_i(JIT_R2, JIT_R2, 2);
  jit_sti_i(&scheme_current_cont_mark_pos, JIT_R2);
  return 0 /* = number of pushed items */;
}

static void generate_non_tail_mark_pos_suffix(mz_jit_state *jitter)
{
  jit_ldi_i(JIT_R2, &scheme_current_cont_mark_pos);
  jit_subi_i(JIT_R2, JIT_R2, 2);
  jit_sti_i(&scheme_current_cont_mark_pos, JIT_R2);
}

static int generate_non_tail(Scheme_Object *obj, mz_jit_state *jitter, int multi_ok, int mark_pos_ends)
{
  if (is_simple(obj, INIT_SIMPLE_DEPTH, 0)) {
    /* Simple; doesn't change the stack or set marks: */
    int v;
    FOR_LOG(jitter->log_depth++);
    v = generate(obj, jitter, 0, multi_ok);
    FOR_LOG(--jitter->log_depth);
    return v;
  }

  {
    int amt, need_ends = 1;
    START_JIT_DATA();

    /* Might change the stack or marks: */
    if (is_simple(obj, INIT_SIMPLE_DEPTH, 1)) {
      need_ends = 0;
    } else {
      if (mark_pos_ends)
	generate_non_tail_mark_pos_prefix(jitter);
      jit_ldi_p(JIT_R2, &scheme_current_cont_mark_stack);
      mz_pushr_p(JIT_R2);
      CHECK_LIMIT();
    }
    mz_runstack_saved(jitter);
    CHECK_LIMIT();
    
    PAUSE_JIT_DATA();
    FOR_LOG(jitter->log_depth++);

    generate(obj, jitter, 0, multi_ok);

    FOR_LOG(--jitter->log_depth);
    RESUME_JIT_DATA();
    CHECK_LIMIT();

    amt = mz_runstack_restored(jitter);
    if (amt) {
      jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(amt));
    }
    if (need_ends) {
      mz_popr_p(JIT_R2);
      jit_sti_p(&scheme_current_cont_mark_stack, JIT_R2);
      if (mark_pos_ends)
	generate_non_tail_mark_pos_suffix(jitter);
      CHECK_LIMIT();
    }

    END_JIT_DATA(21);
  }
    
  return 1;
}

/*========================================================================*/
/*                          expression codegen                            */
/*========================================================================*/

static Scheme_Object *generate_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *obj = (Scheme_Object *)p->ku.k.p1;
  mz_jit_state *jitter = (mz_jit_state *)p->ku.k.p2;
  int v;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  v = generate(obj, jitter, p->ku.k.i1, p->ku.k.i2);

  return scheme_make_integer(v);
}

static int generate(Scheme_Object *obj, mz_jit_state *jitter, int is_tail, int multi_ok)
/* result goes to JIT_R0 */
{
  Scheme_Type type;

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Object *ok;
    Scheme_Thread *p = scheme_current_thread;
    mz_jit_state *jitter_copy;

    /* 3m FIXME: need precise handling of this copy: */
    jitter_copy = (mz_jit_state *)scheme_malloc(sizeof(jitter_copy));
    memcpy(jitter_copy, jitter, sizeof(mz_jit_state));

    p->ku.k.p1 = (void *)obj;
    p->ku.k.p2 = (void *)jitter_copy;
    p->ku.k.i1 = is_tail;
    p->ku.k.i2 = multi_ok;

    ok = scheme_handle_stack_overflow(generate_k);

    memcpy(jitter, jitter_copy, sizeof(mz_jit_state));

    return SCHEME_INT_VAL(ok);
  }
#endif
  
  type = SCHEME_TYPE(obj);
  switch (type) {
  case scheme_toplevel_type:
    {
      int pos;
      START_JIT_DATA();
      LOG_IT(("top-level\n"));
      /* Load global array: */
      pos = mz_remap(SCHEME_TOPLEVEL_DEPTH(obj));
      jit_ldxi_p(JIT_R2, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
      /* Load bucket: */
      pos = SCHEME_TOPLEVEL_POS(obj);
      jit_ldxi_p(JIT_R2, JIT_R2, WORDS_TO_BYTES(pos));
      /* Extract bucket value */
      jit_ldxi_p(JIT_R0, JIT_R2, &(SCHEME_VAR_BUCKET(0x0)->val));
      CHECK_LIMIT();
      /* Is it NULL? */
      (void)jit_beqi_p(unbound_global_code, JIT_R0, 0);
      END_JIT_DATA(0);
      return 1;
    }
  case scheme_local_type:
    {
      int pos;
      START_JIT_DATA();
      LOG_IT(("local\n"));
      pos = mz_remap(SCHEME_LOCAL_POS(obj));
      jit_ldxi_p(JIT_R0, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
      END_JIT_DATA(2);
      return 1;
    }
  case scheme_local_unbox_type:
    {
      int pos;
      START_JIT_DATA();
      LOG_IT(("unbox local\n"));

      pos = mz_remap(SCHEME_LOCAL_POS(obj));
      jit_ldxi_p(JIT_R0, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
      jit_ldr_p(JIT_R0, JIT_R0);

      END_JIT_DATA(3);
      return 1;
    }
  case scheme_syntax_type:
    {
      int pos;
      pos = SCHEME_PINT_VAL(obj);
      switch (pos) {
      case CASE_LAMBDA_EXPD:
	{
	  START_JIT_DATA();
	  LOG_IT(("case-lambda\n"));
	  /* case-lambda */
	  generate_case_lambda(SCHEME_IPTR_VAL(obj), jitter);
	  END_JIT_DATA(5);
	} 
	break;
      case BEGIN0_EXPD:
	{
	  Scheme_Sequence *seq;
	  jit_insn *ref, *ref2;
	  int i;
	  START_JIT_DATA();

	  LOG_IT(("begin0\n"));

	  seq = (Scheme_Sequence *)SCHEME_IPTR_VAL(obj);
	
	  /* Evaluate first expression, and for consistency with bytecode
	     evaluation, allow multiple values. */
	  generate_non_tail(seq->array[0], jitter, 1, 1);
	  CHECK_LIMIT();

	  /* Save value(s) */
	  jit_movr_p(JIT_V1, JIT_R0);
	  mz_pushr_p(JIT_V1);
	  mz_pushr_p(JIT_V1);
	  mz_pushr_p(JIT_V1); /* !!!!!!!! */
	  __START_SHORT_JUMPS__(1);
	  ref = jit_bnei_p(jit_forward(), JIT_R0, SCHEME_MULTIPLE_VALUES);
	  CHECK_LIMIT();
	  /* Save away multiple values */
	  mz_popr_p(JIT_V1);
	  mz_popr_p(JIT_V1);
	  mz_popr_p(JIT_V1);
	  jit_ldi_p(JIT_R0, &scheme_current_thread);
	  CHECK_LIMIT();
	  jit_ldxi_i(JIT_V1, JIT_R0, &((Scheme_Thread *)0x0)->ku.multiple.count);
	  jit_lshi_i(JIT_V1, JIT_V1, 0x1);
	  jit_ori_i(JIT_V1, JIT_V1, 0x1);
	  mz_pushr_p(JIT_V1);
	  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Thread *)0x0)->ku.multiple.array);
	  mz_pushr_p(JIT_V1); /* !!!!!!!! */
	  CHECK_LIMIT();
	  (void)jit_movi_p(JIT_R1, 0x0);
	  mz_pushr_p(JIT_R1); /* pushing 0 indicates that multi-array follows */
	  /* If multi-value array is values buffer, zero out values buffer */
	  jit_ldxi_i(JIT_R2, JIT_R0, &((Scheme_Thread *)0x0)->values_buffer);
	  ref2 = jit_bner_p(jit_forward(), JIT_V1, JIT_R2);
	  jit_stxi_i(&((Scheme_Thread *)0x0)->values_buffer, JIT_R0, JIT_R1);
	  CHECK_LIMIT();

	  /* evaluate remaining expressions */
	  mz_patch_branch(ref);
	  mz_patch_branch(ref2);
	  __END_SHORT_JUMPS__(1);
	  for (i = 1; i < seq->count; i++) {
	    generate_non_tail(seq->array[i], jitter, 1, 1);
	    CHECK_LIMIT();
	  }

	  /* Restore values, if necessary */
	  mz_popr_p(JIT_R0);
	  mz_popr_p(JIT_R1);
	  mz_popr_p(JIT_R2);
	  CHECK_LIMIT();
	  __START_SHORT_JUMPS__(1);
	  ref = jit_bnei_p(jit_forward(), JIT_R0, 0x0);
	  CHECK_LIMIT();
	  jit_ldi_p(JIT_R0, &scheme_current_thread);
	  jit_stxi_p(&((Scheme_Thread *)0x0)->ku.multiple.array, JIT_R0, JIT_R1);
	  jit_rshi_ul(JIT_R2, JIT_R2, 0x1);
	  jit_stxi_p(&((Scheme_Thread *)0x0)->ku.multiple.count, JIT_R0, JIT_R2);
	  (void)jit_movi_p(JIT_R0, SCHEME_MULTIPLE_VALUES);

	  mz_patch_branch(ref);
	  __END_SHORT_JUMPS__(1);

	  END_JIT_DATA(6);
	}
	break;
      case SET_EXPD:
	{
	  Scheme_Object *p, *v;
	  int pos, set_undef;
	  START_JIT_DATA();
	
	  LOG_IT(("set!\n"));

	  p = SCHEME_IPTR_VAL(obj);
	  v = SCHEME_CAR(p);
	  set_undef = SCHEME_TRUEP(v);
	  p = SCHEME_CDR(p);
	  v = SCHEME_CAR(p);
	  p = SCHEME_CDR(p);

	  generate_non_tail(p, jitter, 0, 1);
	  CHECK_LIMIT();

	  /* Load global+stx array: */
	  pos = mz_remap(SCHEME_TOPLEVEL_DEPTH(v));
	  jit_ldxi_p(JIT_R2, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
	  /* Try already-renamed stx: */
	  pos = SCHEME_TOPLEVEL_POS(v);
	  jit_ldxi_p(JIT_R2, JIT_R2, WORDS_TO_BYTES(pos));
	  CHECK_LIMIT();
	
	  /* R0 has values, R2 has pos */
	  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
	  mz_prepare(4);
	  (void)jit_movi_i(JIT_R1, set_undef);
	  jit_pusharg_p(JIT_R1);
	  jit_pusharg_p(JIT_R0);
	  jit_pusharg_p(JIT_R2);
	  CHECK_LIMIT();
	  (void)jit_movi_i(JIT_R1, "set!");
	  jit_pusharg_p(JIT_R1);
	  (void)mz_finish(scheme_set_global_bucket);
	  CHECK_LIMIT();
	  (void)jit_movi_p(JIT_R0, scheme_void);
	  END_JIT_DATA(7);
	}
	break;
      case BOXENV_EXPD:
	{
	  Scheme_Object *p, *v;
	  int pos;
	  START_JIT_DATA();

	  LOG_IT(("boxenv\n"));

	  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();

	  p = (Scheme_Object *)SCHEME_IPTR_VAL(obj);
	  v = SCHEME_CAR(p);
	  pos = mz_remap(SCHEME_INT_VAL(v));
	  p = SCHEME_CDR(p);

	  jit_ldxi_p(JIT_R2, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
	  mz_prepare(1);
	  jit_pusharg_p(JIT_R2);
	  (void)mz_finish(scheme_make_envunbox);
	  jit_retval(JIT_R0);
	  jit_stxi_p(WORDS_TO_BYTES(pos), JIT_RUNSTACK, JIT_R0);
	  CHECK_LIMIT();

	  generate(p, jitter, is_tail, multi_ok);

	  END_JIT_DATA(8);
	}
	break;
      case BOXVAL_EXPD:
	{
	  Scheme_Object *p, *v;
	  int pos, cnt;
	  START_JIT_DATA();

	  LOG_IT(("boxval\n"));

	  p = (Scheme_Object *)SCHEME_IPTR_VAL(obj);
	  v = SCHEME_CAR(p);
	  pos = SCHEME_INT_VAL(v);
	  p = SCHEME_CDR(p);
	  v = SCHEME_CAR(p);
	  cnt = SCHEME_INT_VAL(v);
	  p = SCHEME_CDR(p);

	  /* cnt is expected number of returns, and it will be
	     consistent with multi_ok; do something only if the actual
	     count is the same as cnt */

	  generate_non_tail(p, jitter, cnt != 1, 1);
	  CHECK_LIMIT();

	  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();

	  if (cnt != 1) {
	    jit_insn *ref, *ref2, *ref3;

	    __START_SHORT_JUMPS__(1);
	    ref = jit_bnei_p(jit_forward(), JIT_R0, SCHEME_MULTIPLE_VALUES);
	    /* Handle multiple values: */
	    jit_ldi_p(JIT_R2, &scheme_current_thread);
	    jit_ldxi_i(JIT_R1, JIT_R2, &((Scheme_Thread *)0x0)->ku.multiple.count);	  
	    ref3 = jit_bnei_p(jit_forward(), JIT_R1, cnt);
	    CHECK_LIMIT();
	    /* Received results match expected results */
	    (void)jit_movi_i(JIT_R0, pos);
	    mz_prepare(1);
	    jit_pusharg_p(JIT_R0);
	    (void)mz_finish(box_multiple_array_element);
	    CHECK_LIMIT();
	    (void)jit_movi_p(JIT_R0, SCHEME_MULTIPLE_VALUES);

	    /* Jump over single-value handling: */
	    ref2 = jit_jmpi(jit_forward());
	    CHECK_LIMIT();

	    /* Handle single value: */
	    mz_patch_branch(ref);
	    mz_prepare(1);
	    jit_pusharg_p(JIT_R0);
	    (void)mz_finish(scheme_make_envunbox);
	    CHECK_LIMIT();
	    jit_retval(JIT_R0);
	    mz_patch_ucbranch(ref2);
	    mz_patch_branch(ref3);
	    CHECK_LIMIT();
	    __END_SHORT_JUMPS__(1);
	  } else {
	    mz_prepare(1);
	    jit_pusharg_p(JIT_R0);
	    (void)mz_finish(scheme_make_envunbox);
	    jit_retval(JIT_R0);
	  }

	  END_JIT_DATA(9);
	}
	break;
      case QUOTE_SYNTAX_EXPD:
	{
	    int i, c, p;
	    START_JIT_DATA();

	    LOG_IT(("quote-syntax\n"));

	    obj = SCHEME_IPTR_VAL(obj);
	    i = SCHEME_INT_VAL(SCHEME_CAR(obj));
	    c = mz_remap(SCHEME_INT_VAL(SCHEME_CADR(obj)));
	    p = SCHEME_INT_VAL(SCHEME_CDDR(obj));
	    
	    jit_movi_i(JIT_R0, WORDS_TO_BYTES(c));
	    jit_movi_i(JIT_R1, WORDS_TO_BYTES(i + p + 1));
	    jit_movi_i(JIT_R2, WORDS_TO_BYTES(p));
	    (void)jit_calli(quote_syntax_code);

	    END_JIT_DATA(10);
	}
	break;
      default:
	{
	  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
	  obj = SCHEME_IPTR_VAL(obj);
	  (void)jit_movi_p(JIT_R2, obj); /* !! */
	  CHECK_LIMIT();
	  mz_prepare(1);
	  jit_pusharg_p(JIT_R2);
	  (void)mz_finish(scheme_syntax_executers[pos]);
	  CHECK_LIMIT();
	  jit_retval(JIT_R0);
	}
      }
      return 1;
    }
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)obj;

      LOG_IT(("app %d\n", app->num_args));

      return generate_app(app, NULL, app->num_args, jitter, is_tail, multi_ok);
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)obj;
      Scheme_Object *args[2];
      int r;

      r = generate_inlined_unary(jitter, app, is_tail, multi_ok, NULL, 1);
      if (r)
	return r;

      LOG_IT(("app 2\n"));

      CHECK_LIMIT();
      
      args[0] = app->rator;
      args[1] = app->rand;
      
      return generate_app(NULL, args, 1, jitter, is_tail, multi_ok);
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)obj;
      Scheme_Object *args[3];
      int r;

      r = generate_inlined_binary(jitter, app, is_tail, multi_ok, NULL, 1);
      if (r)
	return r;

      LOG_IT(("app 3\n"));

      CHECK_LIMIT();
      
      args[0] = app->rator;
      args[1] = app->rand1;
      args[2] = app->rand2;

      return generate_app(NULL, args, 2, jitter, is_tail, multi_ok);
    }
  case scheme_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)obj;
      int cnt = seq->count, i;
      START_JIT_DATA();

      LOG_IT(("begin\n"));

      for (i = 0; i < cnt - 1; i++) {
	generate_non_tail(seq->array[i], jitter, 1, 1);
	CHECK_LIMIT();
      }

      END_JIT_DATA(11);

      return generate(seq->array[cnt - 1], jitter, is_tail, multi_ok);
    }
  case scheme_branch_type:
    {	
      Scheme_Branch_Rec *branch = (Scheme_Branch_Rec *)obj;
      jit_insn *refs[3], *ref2;
      int nsrs, nsrs1, g1, g2, amt;
#ifdef MZ_USE_JIT_PPC
      int then_short_ok, else_short_ok;
#else
      int then_short_ok = 1;
#endif
	  START_JIT_DATA();

#ifdef MZ_USE_JIT_PPC
      /* It's possible that the code for a then
	 or else branch will be so large that we might
	 need a long jump. Conservatively analyze the
	 `then' and `else' expressions. */
      then_short_ok = (is_short(branch->tbranch, 32) > 0);
      else_short_ok = (is_short(branch->fbranch, 32) > 0);
#endif

      LOG_IT(("if...\n"));

      refs[1] = NULL;
      refs[2] = NULL;

      if (!generate_inlined_test(jitter, branch->test, then_short_ok, refs)) {
	CHECK_LIMIT();
	generate_non_tail(branch->test, jitter, 0, 1);
	CHECK_LIMIT();
	__START_SHORT_JUMPS__(then_short_ok);
	refs[0] = jit_beqi_p(jit_forward(), JIT_R0, scheme_false);
	__END_SHORT_JUMPS__(then_short_ok);
      }
      CHECK_LIMIT();

      /* True branch */
      mz_runstack_saved(jitter);
      nsrs = jitter->need_set_rs;
      PAUSE_JIT_DATA();
      LOG_IT(("...then...\n"));
      g1 = generate(branch->tbranch, jitter, is_tail, multi_ok);
      RESUME_JIT_DATA();
      CHECK_LIMIT();
      amt = mz_runstack_restored(jitter);
      if (g1 != 2) {
	if (amt && !is_tail) {
	  jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(amt));
	}
	__START_SHORT_JUMPS__(else_short_ok);
	ref2 = jit_jmpi(jit_forward());
	__END_SHORT_JUMPS__(else_short_ok);
	nsrs1 = jitter->need_set_rs;
      } else {
	ref2 = 0;
	nsrs1 = 0;
      }
      jitter->need_set_rs = nsrs;
      
      /* False branch */
      mz_runstack_saved(jitter);
      __START_SHORT_JUMPS__(then_short_ok);
      mz_patch_branch(refs[0]);
      if (refs[1]) {
	mz_patch_branch(refs[1]);
      }
      if (refs[2]) {
	jit_patch_movi(refs[2], (_jit.x.pc));
      }
      __END_SHORT_JUMPS__(then_short_ok);
      PAUSE_JIT_DATA();
      LOG_IT(("...else\n"));
      g2 = generate(branch->fbranch, jitter, is_tail, multi_ok);
      RESUME_JIT_DATA();
      CHECK_LIMIT();
      amt = mz_runstack_restored(jitter);
      if (g2 != 2) {
	if (amt && !is_tail) {
	  jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(amt));
	}
      } else {
	jitter->need_set_rs = 0;
      }
      if (g1 != 2) {
	__START_SHORT_JUMPS__(else_short_ok);
	mz_patch_ucbranch(ref2);
	__END_SHORT_JUMPS__(else_short_ok);
      }

      END_JIT_DATA(12);

      /* Return result */

      if ((g1 == 2) && (g2 == 2))
	return 2;

      if (nsrs1)
	jitter->need_set_rs = 1;

      return 1;
    }
  case scheme_unclosed_procedure_type:
    {
      Scheme_Closure_Data *data = (Scheme_Closure_Data *)obj;
      START_JIT_DATA();

      LOG_IT(("lambda\n"));
      
      /* Allocate closure */
      generate_closure(data, jitter);
      CHECK_LIMIT();

      generate_closure_fill(data, jitter);

      END_JIT_DATA(13);
      return 0;
    }
  case scheme_let_value_type:
    {
      Scheme_Let_Value *lv = (Scheme_Let_Value *)obj;
      int ab = SCHEME_LET_AUTOBOX(lv), i, pos;
      START_JIT_DATA();

      LOG_IT(("let...\n"));

      if (lv->count == 1) {
	/* Expect one result: */
	generate_non_tail(lv->value, jitter, 0, 1);
	if (ab) {
	  pos = mz_remap(lv->position);
	  jit_ldxi_p(JIT_R2, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
	  jit_str_p(JIT_R2, JIT_R0);
	} else {
	  pos = mz_remap(lv->position);
	  jit_stxi_p(WORDS_TO_BYTES(pos), JIT_RUNSTACK, JIT_R0);
	}
	CHECK_LIMIT();
      } else {
	/* Expect multiple results: */
	jit_insn *ref, *ref2, *ref3;

	generate_non_tail(lv->value, jitter, 1, 1);
	CHECK_LIMIT();
    
	__START_SHORT_JUMPS__(1);

	/* Did we get multiple results? If not, go to error: */
	ref = jit_bnei_p(jit_forward(), JIT_R0, SCHEME_MULTIPLE_VALUES);
	/* Load count and result array: */
	jit_ldi_p(JIT_R2, &scheme_current_thread);
	jit_ldxi_i(JIT_R1, JIT_R2, &((Scheme_Thread *)0x0)->ku.multiple.count);
	jit_ldxi_i(JIT_R2, JIT_R2, &((Scheme_Thread *)0x0)->ku.multiple.array);
	CHECK_LIMIT();
	/* If we got the expected count, jump to installing values: */
	ref2 = jit_beqi_i(jit_forward(), JIT_R1, lv->count);
	/* Otherwise, jump to error: */
	ref3 = jit_jmpi(jit_forward());
	CHECK_LIMIT();

	/* Jump here when we didn't get multiple values. Set count to 1
	   and "array" to single value: */
	mz_patch_branch(ref);
	jit_movi_i(JIT_R1, 1);
	jit_movr_i(JIT_R2, JIT_R0);
	CHECK_LIMIT();
	  
	/* Error starts here: */
	mz_patch_ucbranch(ref3);
	JIT_UPDATE_THREAD_RSPTR_FOR_BRANCH_IF_NEEDED();
	mz_prepare(5);
	(void)jit_movi_p(JIT_V1, "lexical binding");
	jit_pusharg_p(JIT_V1);
	jit_pusharg_p(JIT_R2);
	jit_pusharg_i(JIT_R1);
	CHECK_LIMIT();
	jit_movi_i(JIT_V1, lv->count);
	jit_pusharg_i(JIT_V1);
	(void)jit_movi_p(JIT_V1, NULL);
	jit_pusharg_i(JIT_V1);
	(void)mz_finish(scheme_wrong_return_arity);
	CHECK_LIMIT();

	/* Continue with expected values; R2 has value array: */
	mz_patch_branch(ref2);
	__END_SHORT_JUMPS__(1);
	for (i = 0; i < lv->count; i++) {
	  jit_ldxi_p(JIT_R1, JIT_R2, WORDS_TO_BYTES(i));
	  if (ab) {
	    pos = mz_remap(lv->position + i);
	    jit_ldxi_p(JIT_R0, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
	    jit_str_p(JIT_R0, JIT_R1);
	  } else {
	    pos = mz_remap(lv->position + i);
	    jit_stxi_p(WORDS_TO_BYTES(pos), JIT_RUNSTACK, JIT_R1);
	  }
	  CHECK_LIMIT();
	}
      }

      END_JIT_DATA(14);

      LOG_IT(("...in\n"));

      return generate(lv->body, jitter, is_tail, multi_ok);
    }
  case scheme_let_void_type:
    {
      Scheme_Let_Void *lv = (Scheme_Let_Void *)obj;
      int c = lv->count;
      START_JIT_DATA();

      LOG_IT(("letv...\n"));

      jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(c));
      mz_runstack_pushed(jitter, c);

      if (SCHEME_LET_AUTOBOX(lv)) {
	int i;
	JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
	for (i = 0; i < c; i++) {
	  CHECK_LIMIT();
	  (void)jit_movi_p(JIT_R0, scheme_undefined);
	  mz_prepare(1);
	  jit_pusharg_p(JIT_R0);
	  (void)mz_finish(scheme_make_envunbox);
	  jit_retval(JIT_R0);
	  jit_stxi_p(WORDS_TO_BYTES(i), JIT_RUNSTACK, JIT_R0);
	}
      }
      CHECK_LIMIT();

      END_JIT_DATA(15);

      LOG_IT(("...in\n"));

      return generate(lv->body, jitter, is_tail, multi_ok);
    }
  case scheme_letrec_type:
    {
      Scheme_Letrec *l = (Scheme_Letrec *)obj;
      int i;
      START_JIT_DATA();

      LOG_IT(("letrec...\n"));

      /* Create unfinished closures */
      for (i = 0; i < l->count; i++) {
	((Scheme_Closure_Data *)l->procs[i])->context = (Scheme_Object *)l;
	generate_closure((Scheme_Closure_Data *)l->procs[i], jitter);
	CHECK_LIMIT();
	jit_stxi_p(WORDS_TO_BYTES(i), JIT_RUNSTACK, JIT_R0);
      }

      /* Close them: */
      for (i = l->count; i--; ) {
	if (i != l->count - 1) {
	  /* Last one we created is still in JIT_R0: */
	  jit_ldxi_p(JIT_R0, JIT_RUNSTACK, WORDS_TO_BYTES(i));
	}
	generate_closure_fill((Scheme_Closure_Data *)l->procs[i], jitter);
	CHECK_LIMIT();
      }

      END_JIT_DATA(16);

      LOG_IT(("...in\n"));

      return generate(l->body, jitter, is_tail, multi_ok);
    }
  case scheme_let_one_type:
    {
      Scheme_Let_One *lv = (Scheme_Let_One *)obj;
      START_JIT_DATA();

      LOG_IT(("leto...\n"));

      jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
      mz_runstack_pushed(jitter, 1);

      PAUSE_JIT_DATA();
      generate_non_tail(lv->value, jitter, 0, 1);
      RESUME_JIT_DATA();
      CHECK_LIMIT();
      jit_str_p(JIT_RUNSTACK, JIT_R0);
	
      END_JIT_DATA(17);

      LOG_IT(("...in\n"));

      return generate(lv->body, jitter, is_tail, multi_ok);
    }
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)obj;
      START_JIT_DATA();

      LOG_IT(("wcm...\n"));

      /* Key: */
      generate_non_tail(wcm->key, jitter, 0, 1);
      CHECK_LIMIT();
      if (SCHEME_TYPE(obj) > _scheme_values_types_) {
	/* No need to push mark onto value stack: */
	jit_movr_p(JIT_V1, JIT_R0);
	generate_non_tail(wcm->val, jitter, 0, 1);
      } else {
	mz_pushr_p(JIT_R0); /* !!!!!!! */
	generate_non_tail(wcm->val, jitter, 0, 1);
	CHECK_LIMIT();
	mz_popr_p(JIT_V1);
      }

      JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();

      mz_prepare(2);
      jit_pusharg_p(JIT_R0);
      jit_pusharg_p(JIT_V1);
      (void)mz_finish(scheme_set_cont_mark);
      CHECK_LIMIT();

      END_JIT_DATA(18);

      LOG_IT(("...in\n"));
	
      return generate(wcm->body, jitter, is_tail, multi_ok);
    }
  default:
    {
      Scheme_Type type = SCHEME_TYPE(obj);
      START_JIT_DATA();

      LOG_IT(("const\n"));

      /* Avoid compiling closures multiple times: */
      if (jitter->retain_start) {
	if (type == scheme_closure_type) {
	  /* Empty closure? If so, compile the code and get a native closure: */
	  Scheme_Closure *c = (Scheme_Closure *)obj;
	  if (ZERO_SIZED_CLOSUREP(c))
	    obj = scheme_jit_closure((Scheme_Object *)c->code, NULL);
	} else if (type == scheme_case_closure_type) {
	  /* Empty case closure? Turn in into a JITted empty case closure. */
	  obj = scheme_unclose_case_lambda(obj, 1);
	}
      }

      if (!SCHEME_INTP(obj)
	  && !SAME_OBJ(obj, scheme_true)
	  && !SAME_OBJ(obj, scheme_false)
	  && !SAME_OBJ(obj, scheme_void)
	  && !SAME_OBJ(obj, scheme_null)) {
	mz_retain(obj);
      }
      (void)jit_movi_p(JIT_R0, obj); /* !! */

      END_JIT_DATA(19);
      return 1;
    }
  }
}

/*========================================================================*/
/*                          procedure codegen                             */
/*========================================================================*/

static void generate_function_prolog(mz_jit_state *jitter, void *code, int max_let_depth)
{
  int in;
  START_JIT_DATA();

  jit_prolog(3);
    
  in = jit_arg_p();
  jit_getarg_p(JIT_R0, in); /* closure */
  in = jit_arg_i();
  jit_getarg_i(JIT_R1, in); /* argc */
  in = jit_arg_p();
  jit_getarg_p(JIT_R2, in); /* argv */

  jit_ldi_p(JIT_RUNSTACK, &MZ_RUNSTACK);

  END_JIT_DATA(1);
}

static int generate_function_getarg(mz_jit_state *jitter, int has_rest, int num_params)
{
  int i, cnt;
  jit_insn *ref;
  int set_ref;

  mz_push_local_p(JIT_RUNSTACK, JIT_LOCAL1);

  /* If rands == runstack and there are no rest args, set runstack
     base to runstack + rands (and don't copy rands), otherwise set
     base to runstack and proceed normally. Implement this by
     optimisitcally assuming rands == runstack, so that there's just
     one jump. Skip this optimization when the procedure has
     rest args, because we'll have to copy anyway. */
  if (!has_rest && num_params) {
    jit_lshi_i(JIT_RUNSTACK_BASE, JIT_R1, JIT_LOG_WORD_SIZE);
    jit_addr_p(JIT_RUNSTACK_BASE, JIT_R2, JIT_RUNSTACK_BASE);
    __START_SHORT_JUMPS__(num_params < 100);
    ref = jit_beqr_p(jit_forward(), JIT_RUNSTACK, JIT_R2);
    __END_SHORT_JUMPS__(num_params < 100);
    set_ref = 1;
  } else {
    ref = 0;
    set_ref = 0;
  }
  jit_movr_p(JIT_RUNSTACK_BASE, JIT_RUNSTACK);

  /* Make stack room for arguments: */
  cnt = num_params;
  if (cnt) {
    CHECK_LIMIT();
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(cnt));
    if (has_rest)
      --cnt;
  }

  /* Extract arguments to runstack: */
  for (i = cnt; i--; ) {
    jit_ldxi_p(JIT_V1, JIT_R2, WORDS_TO_BYTES(i));
    jit_stxi_p(WORDS_TO_BYTES(i), JIT_RUNSTACK, JIT_V1);
    CHECK_LIMIT();
  }

  if (set_ref) {
    __START_SHORT_JUMPS__(num_params < 100);
    mz_patch_branch(ref);
    __END_SHORT_JUMPS__(num_params < 100);
  }

  return cnt;
}

static int do_generate_common(mz_jit_state *jitter, void *_data)
{
  int in, i;
  GC_CAN_IGNORE jit_insn *ref, *ref2;

  /* *** jump_to_native_[arity_]code *** */
  /* Called as a function: */
  for (i = 0; i < 2; i++) {
    if (!i)
      jump_to_native_code = jit_get_ip().ptr;
    else
      jump_to_native_arity_code = jit_get_ip().ptr;
    jit_prolog(3);
    in = jit_arg_p();
    jit_getarg_p(JIT_R0, in); /* closure */
    in = jit_arg_p();
    jit_getarg_i(JIT_R1, in); /* argc */
    in = jit_arg_p();
    jit_getarg_i(JIT_R2, in); /* argv */
    CHECK_LIMIT();
    jit_movr_p(JIT_RUNSTACK, JIT_R2);
    jit_movr_p(JIT_RUNSTACK_BASE, JIT_R2);
    mz_push_local_p(JIT_RUNSTACK, JIT_LOCAL1);
    jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
    if (!i) {
      jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->u.tail_code);
    } else {
      jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->arity_code);
    }
    jit_jmpr(JIT_V1);
    CHECK_LIMIT();
  }

  /* *** check_arity_code *** */
  /* Called as a function: */
  check_arity_code = (Native_Check_Arity_Proc)jit_get_ip().ptr;
  jit_prolog(2);
  in = jit_arg_p();
  jit_getarg_p(JIT_R0, in); /* closure */
  in = jit_arg_p();
  jit_getarg_i(JIT_R2, in); /* argc */
  jit_movi_i(JIT_R1, -1);
  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_i(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->arity_code);
  jit_jmpr(JIT_V1);
  CHECK_LIMIT();

  /* *** check_arity_code *** */
  /* Called as a function: */
  get_arity_code = (Native_Get_Arity_Proc)jit_get_ip().ptr;
  jit_prolog(1);
  in = jit_arg_p();
  jit_getarg_p(JIT_R0, in); /* closure */
  jit_movi_i(JIT_R1, -1);
  jit_movi_i(JIT_R2, 0x0);
  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_i(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->arity_code);
  jit_jmpr(JIT_V1);
  CHECK_LIMIT();

  /* *** bad_result_arity_code *** */
  /* Jumped-to from non-tail contexts  */
  bad_result_arity_code = (Native_Get_Arity_Proc)jit_get_ip().ptr;
  jit_ldi_p(JIT_R2, &scheme_current_thread);
  jit_ldxi_i(JIT_R1, JIT_R2, &((Scheme_Thread *)0x0)->ku.multiple.count);
  jit_ldxi_i(JIT_R2, JIT_R2, &((Scheme_Thread *)0x0)->ku.multiple.array);
  CHECK_LIMIT();
  mz_prepare(5);
  (void)jit_movi_p(JIT_V1, NULL);
  jit_pusharg_p(JIT_V1);
  jit_pusharg_p(JIT_R2);
  jit_pusharg_i(JIT_R1);
  CHECK_LIMIT();
  jit_movi_i(JIT_V1, 1);
  jit_pusharg_i(JIT_V1);
  (void)jit_movi_p(JIT_V1, NULL);
  jit_pusharg_i(JIT_V1);
  (void)mz_finish(scheme_wrong_return_arity);
  CHECK_LIMIT();

  /* *** unbound_global_code *** */
  unbound_global_code = jit_get_ip().ptr;
  JIT_UPDATE_THREAD_RSPTR();
  mz_prepare(1);
  jit_pusharg_p(JIT_R2);
  (void)mz_finish(scheme_unbound_global);
  CHECK_LIMIT();

  /* *** quote_syntax_code *** */
  /* R0 is WORDS_TO_BYTES(c), R1 is WORDS_TO_BYTES(i+p+1), R2 is WORDS_TO_BYTES(p) */
  quote_syntax_code = jit_get_ip().ptr;
  mz_prolog(JIT_V1);
  __START_SHORT_JUMPS__(1);
  /* Load global array: */
  jit_ldxr_p(JIT_V1, JIT_RUNSTACK, JIT_R0);
  /* Load syntax object: */
  jit_ldxr_p(JIT_R0, JIT_V1, JIT_R1);
  /* Is it null? */
  ref = jit_bnei_p(jit_forward(), JIT_R0, 0x0);
  CHECK_LIMIT();
  /* Syntax object is NULL, so we need to create it. */
  jit_ldxr_p(JIT_R0, JIT_V1, JIT_R2); /* put element at p in R0 */
  /* Save global array: */
  mz_push_local_p(JIT_V1, JIT_LOCAL3);
  /* Compute i in JIT_V1: */
  jit_subr_p(JIT_V1, JIT_R1, JIT_R2);
  jit_subi_p(JIT_V1, JIT_V1, WORDS_TO_BYTES(1));
  CHECK_LIMIT();
  /* Load car & cdr of elements at p: */
  jit_ldxi_p(JIT_R2, JIT_R0, &SCHEME_CAR((Scheme_Object *)0x0));
  jit_ldxi_p(JIT_R0, JIT_R0, &SCHEME_CDR((Scheme_Object *)0x0));
  jit_ldxr_p(JIT_R0, JIT_R0, JIT_V1);
  /* Move R1 to V1 to save it: */
  jit_movr_p(JIT_V1, JIT_R1);
  /* Call scheme_add_rename: */
  JIT_UPDATE_THREAD_RSPTR();
  CHECK_LIMIT();
  mz_prepare(2);
  jit_pusharg_p(JIT_R2);
  jit_pusharg_p(JIT_R0);
  (void)mz_finish(scheme_add_rename);
  CHECK_LIMIT();
  jit_retval(JIT_R0);
  /* Restore global array into JIT_R1, and put computed element at i+p+1: */
  mz_pop_local_p(JIT_R1, JIT_LOCAL3);
  jit_stxr_p(JIT_V1, JIT_R1, JIT_R0);
  mz_patch_branch(ref);
  __END_SHORT_JUMPS__(1);
  mz_epilog(JIT_V1);

  /* *** bad_{car,cdr}_code *** */
  /* Non-pair is in R0 */
  for (i = 0; i < 2; i++) {
    if (!i) {
      bad_car_code = jit_get_ip().ptr;
    } else {
      bad_cdr_code = jit_get_ip().ptr;
    }
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    JIT_UPDATE_THREAD_RSPTR();
    CHECK_LIMIT();
    jit_movi_i(JIT_R1, 1);
    jit_prepare(2);
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_R1);
    if (!i) {
      (void)jit_finish(scheme_checked_car);
    } else {
      (void)jit_finish(scheme_checked_cdr);
    }
    CHECK_LIMIT();
  }

  /* *** call_original_unary_arith_code *** */
  /* R0 is arg, R2 is code pointer, V1 is return address */
  for (i = 0; i < 3; i++) {
    int argc, j;
    for (j = 0; j < 2; j++) {
      if (!i) {
	if (!j)
	  call_original_unary_arith_code = jit_get_ip().ptr;
	else
	  call_original_unary_arith_for_branch_code = jit_get_ip().ptr;
	argc = 1;
      } else if (i == 1) {
	if (!j)
	  call_original_binary_arith_code = jit_get_ip().ptr;
	else
	  call_original_binary_arith_for_branch_code = jit_get_ip().ptr;
	argc = 2;
      } else {
	if (!j)
	  call_original_binary_rev_arith_code = jit_get_ip().ptr;
	else
	  call_original_binary_rev_arith_for_branch_code = jit_get_ip().ptr;
	argc = 2;
      }
      jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(argc));
      if (i == 2) {
	jit_str_p(JIT_RUNSTACK, JIT_R0);
	jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
      } else if (i == 1) {
	jit_str_p(JIT_RUNSTACK, JIT_R1);
	jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R0);
      } else {
	jit_str_p(JIT_RUNSTACK, JIT_R0);
      }
      jit_movi_i(JIT_R1, argc);
      mz_prepare(2);
      jit_pusharg_p(JIT_RUNSTACK);
      jit_pusharg_p(JIT_R1);
      (void)mz_finishr(JIT_R2);
      CHECK_LIMIT();
      jit_retval(JIT_R0);
      jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(argc));
      JIT_UPDATE_THREAD_RSPTR();
      if (!j) {
	jit_jmpr(JIT_V1);
      } else {
	/* In for_branch mode, V1 is target for false, LOCAL2 is target for true */
	mz_pop_local_p(JIT_R1, JIT_LOCAL2);
	__START_SHORT_JUMPS__(1);
	ref = jit_beqi_p(jit_forward(), JIT_R0, scheme_true);
	jit_jmpr(JIT_V1);
	mz_patch_branch(ref);
	jit_jmpr(JIT_R1);
	__END_SHORT_JUMPS__(1);
      }
      CHECK_LIMIT();
    }
  }

  /* *** on_demand_jit_[arity_]code *** */
  /* Used as the code stub for a closure whose
     code is not yet compiled. See generate_function_prolog
     for the state of registers on entry */
  on_demand_jit_code = jit_get_ip().ptr;
  jit_prolog(3);
  in = jit_arg_p();
  jit_getarg_p(JIT_R0, in); /* closure */
  in = jit_arg_i();
  jit_getarg_i(JIT_R1, in); /* argc */
  in = jit_arg_p();
  jit_getarg_p(JIT_R2, in); /* argv */
  CHECK_LIMIT();
  jit_ldi_p(JIT_RUNSTACK, &MZ_RUNSTACK);
  mz_push_local_p(JIT_RUNSTACK, JIT_LOCAL1);
  on_demand_jit_arity_code = jit_get_ip().ptr; /* <<<- arity variant starts here */
  jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(3));
  jit_str_p(JIT_RUNSTACK, JIT_R0);
  jit_lshi_ul(JIT_R1, JIT_R1, 0x1);
  jit_ori_ul(JIT_R1, JIT_R1, 0x1);
  CHECK_LIMIT();
  jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
  jit_stxi_p(WORDS_TO_BYTES(2), JIT_RUNSTACK, JIT_R2);
  JIT_UPDATE_THREAD_RSPTR();
  (void)jit_calli(on_demand);
  CHECK_LIMIT();
  /* Restore registers and runstack, and jump to arity checking
     of newly-created code when argv == runstack (i.e., a tail call): */
  jit_ldr_p(JIT_R0, JIT_RUNSTACK);
  jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(1));
  jit_rshi_ul(JIT_R1, JIT_R1, 0x1);
  jit_ldxi_p(JIT_R2, JIT_RUNSTACK, WORDS_TO_BYTES(2));
  jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(3));
  CHECK_LIMIT();
  ref = jit_bner_p(jit_forward(), JIT_RUNSTACK, JIT_R2);
  /* Also, check that the runstack is big enough with the revised
     max_let_depth. We can use JIT_V2 here because RUNSTACK_BASE isnot
     yet ready: */
  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_i(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->max_let_depth);
  jit_ldi_p(JIT_V2, &MZ_RUNSTACK_START);
  jit_subr_ul(JIT_V2, JIT_RUNSTACK, JIT_V2);
  ref2 = jit_bltr_ul(jit_forward(), JIT_V2, JIT_V1);
  CHECK_LIMIT();
  /* This is the tail-call fast path: */
  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->arity_code);
  /* Set runstack base to end of arguments on runstack: */
  jit_movr_p(JIT_RUNSTACK_BASE, JIT_R1);
  jit_lshi_ul(JIT_RUNSTACK_BASE, JIT_RUNSTACK_BASE, JIT_LOG_WORD_SIZE);
  jit_addr_p(JIT_RUNSTACK_BASE, JIT_RUNSTACK_BASE, JIT_RUNSTACK);
  jit_jmpr(JIT_V1);
  CHECK_LIMIT();
  /* Slower path (non-tail) when argv != runstack. To simulate
     a tail call, we must decrement the cont-mark pos. */
  mz_patch_branch(ref);
  mz_patch_branch(ref2);
  jit_ldi_i(JIT_V1, &scheme_current_cont_mark_pos);
  jit_subi_i(JIT_V1, JIT_V1, 2);
  jit_sti_i(&scheme_current_cont_mark_pos, JIT_V1);
  CHECK_LIMIT();
  mz_prepare(3);
  jit_pusharg_p(JIT_R2);
  jit_pusharg_p(JIT_R1);
  jit_pusharg_p(JIT_R0);
  (void)jit_finish(_scheme_apply_multi_from_native);
  CHECK_LIMIT();
  jit_ldi_i(JIT_NOT_RET, &scheme_current_cont_mark_pos);
  jit_addi_i(JIT_NOT_RET, JIT_NOT_RET, 2);
  jit_sti_i(&scheme_current_cont_mark_pos, JIT_NOT_RET);
  mz_pop_local_p(JIT_NOT_RET, JIT_LOCAL1);
  jit_ret();
  CHECK_LIMIT();

  /* *** get_stack_pointer_code *** */
  get_stack_pointer_code = jit_get_ip().ptr;
  jit_leaf(0);
  jit_movr_p(JIT_R0, JIT_STACK_FRAME);
  /* Get frame pointer of caller... */
#ifdef MZ_USE_JIT_PPC
  jit_ldr_p(JIT_R0, JIT_R0);
#endif
#ifdef MZ_USE_JIT_I386
  jit_ldr_p(JIT_R0, JIT_R0);
#endif
  jit_movr_p(JIT_RET, JIT_R0);
  jit_ret();
  CHECK_LIMIT();

  /* *** stack_cache_pop_code *** */
  stack_cache_pop_code = jit_get_ip().ptr;
  jit_movr_p(JIT_R0, JIT_RET);
  /* Decrement stack_cache_stack_pos */
  jit_ldi_i(JIT_R1, &stack_cache_stack_pos);
  jit_subi_i(JIT_R2, JIT_R1, 1);
  jit_sti_p(&stack_cache_stack_pos, JIT_R2);
  CHECK_LIMIT();
  /* Extract old return address and jump to it */
  jit_lshi_i(JIT_R1, JIT_R1, (JIT_LOG_WORD_SIZE + 2));
  jit_addi_i(JIT_R1, JIT_R1, (int)&((Stack_Cache_Elem *)0x0)->orig_return_address);
  (void)jit_movi_p(JIT_R2, &stack_cache_stack);
  jit_ldxr_p(JIT_R2, JIT_R2, JIT_R1);
  jit_movr_p(JIT_RET, JIT_R0);
  jit_jmpr(JIT_R2);
  CHECK_LIMIT();

  /* *** vector_ref_[check_index_]code *** */
  /* R0 is vector, R1 is index (Scheme number in check-index mode), 
     V1 is vector offset in non-check-index mode */
  for (i = 0; i < 2; i++) {
    jit_insn *ref, *reffail;

    if (!i) {
      vector_ref_code = jit_get_ip().ptr;
    } else {
      vector_ref_check_index_code = jit_get_ip().ptr;
    }

    __START_SHORT_JUMPS__(1);

    mz_prolog(JIT_R2);

    ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
    CHECK_LIMIT();

    reffail = _jit.x.pc;
    if (!i) {
      jit_lshi_ul(JIT_R1, JIT_R1, 1);
      jit_ori_ul(JIT_R1, JIT_R1, 0x1);
    }
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
    jit_movi_i(JIT_R1, 2);
    JIT_UPDATE_THREAD_RSPTR();
    jit_prepare(2);
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_R1);
    (void)jit_finish(scheme_checked_vector_ref);
    CHECK_LIMIT();
    
    mz_patch_branch(ref);
    if (i) {
      (void)jit_bmci_ul(reffail, JIT_R1, 0x1);
      (void)jit_blei_l(reffail, JIT_R1, 0x0);
    }
    jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Object *)0x0)->type);
    (void)jit_bnei_p(reffail, JIT_R2, scheme_vector_type);
    jit_ldxi_i(JIT_R2, JIT_R0, &SCHEME_VEC_SIZE(0x0));
    if (i) {
      jit_rshi_ul(JIT_R1, JIT_R1, 1);
    }
    (void)jit_bler_ul(reffail, JIT_R2, JIT_R1);
    if (i) {
      jit_lshi_ul(JIT_V1, JIT_R1, JIT_LOG_WORD_SIZE);
      jit_addi_p(JIT_V1, JIT_V1, ((int)&SCHEME_VEC_ELS(0x0)));
    }
    jit_ldxr_p(JIT_R0, JIT_R0, JIT_V1);
    mz_epilog(JIT_R2);
    CHECK_LIMIT();

    __END_SHORT_JUMPS__(1);
  }

  /* *** syntax_ecode *** */
  /* R0 is (potential) syntax object */
  {
    jit_insn *ref, *reffail;
    syntax_e_code = jit_get_ip().ptr;
    __START_SHORT_JUMPS__(1);
    mz_prolog(JIT_R2);

    ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);

    reffail = _jit.x.pc;
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    jit_movi_i(JIT_R1, 1);
    JIT_UPDATE_THREAD_RSPTR();
    CHECK_LIMIT();
    jit_prepare(2);
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_R1);
    (void)jit_finish(scheme_checked_syntax_e);
    jit_retval(JIT_R0);
    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    mz_epilog(JIT_R2);
    CHECK_LIMIT();
    
    /* It's not a fixnum... */
    mz_patch_branch(ref);
    jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Object *)0x0)->type);
    (void)jit_bnei_p(reffail, JIT_R2, scheme_stx_type);
    
    /* It's a syntax object... needs to propagate? */
    jit_ldxi_l(JIT_R2, JIT_R0, &((Scheme_Stx *)0x0)->u.lazy_prefix);
    ref = jit_beqi_l(jit_forward(), JIT_R2, 0x0);
    CHECK_LIMIT();

    /* Maybe needs to propagate; check STX_SUBSTX_FLAG flag */
    jit_ldxi_s(JIT_R2, JIT_R0, &MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso));
    (void)jit_bmsi_ul(reffail, JIT_R2, STX_SUBSTX_FLAG);
    
    /* No propagations. Extract value. */
    mz_patch_branch(ref);
    jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Stx *)0x0)->val);

    mz_epilog(JIT_R2);
    CHECK_LIMIT();
    __END_SHORT_JUMPS__(1);
  }

  return 1;
}

typedef struct {
  Scheme_Closure_Data *data;
  void *code, *tail_code, *code_end;
  int max_extra, max_depth;
} Generate_Closure_Data;

static int do_generate_closure(mz_jit_state *jitter, void *_data)
{
  Generate_Closure_Data *gdata = (Generate_Closure_Data *)_data;
  Scheme_Closure_Data *data = gdata->data;
  void *code, *tail_code, *code_end;
  int i, r, cnt;

  code = jit_get_ip().ptr;

  generate_function_prolog(jitter, code, 
			   /* max_extra_pushed may be wrong the first time around,
			      but it will be right the last time around */
			   WORDS_TO_BYTES(data->max_let_depth + jitter->max_extra_pushed));
  CHECK_LIMIT();

  cnt = generate_function_getarg(jitter, 
				 (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST),
				 data->num_params);
  CHECK_LIMIT();
  
  /* A tail call starts here. Caller must ensure that the
     stack is big enough, right number of arguments, closure
     is in R0. */
  tail_code = jit_get_ip().ptr;

  /* 0 params and has_rest => (lambda args E) where args is not in E,
     so accept any number of arguments and ignore them. */

  if ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
      && data->num_params) {
    /* If runstack == argv and argc == cnt, then we didn't
       copy args down, and we need to make room for scheme_null. */
    jit_insn *ref, *ref2, *ref3;
	  
    CHECK_LIMIT();
    
    __START_SHORT_JUMPS__(cnt < 100);

    ref = jit_bner_p(jit_forward(), JIT_RUNSTACK, JIT_R2);
    ref3 = jit_bgti_p(jit_forward(), JIT_R1, cnt);
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(cnt+1));
    for (i = cnt; i--; ) {
      jit_ldxi_p(JIT_V1, JIT_R2, WORDS_TO_BYTES(i));
      jit_stxi_p(WORDS_TO_BYTES(i), JIT_RUNSTACK, JIT_V1);
      CHECK_LIMIT();
    }
    (void)jit_movi_p(JIT_V1, scheme_null);
    jit_stxi_p(WORDS_TO_BYTES(cnt), JIT_RUNSTACK, JIT_V1);
    ref2 = jit_jmpi(jit_forward());
    CHECK_LIMIT();

    /* Build a list for extra arguments: */
    mz_patch_branch(ref);
    mz_patch_branch(ref3);
    if (data->closure_size)
      mz_pushr_p(JIT_R0);
    JIT_UPDATE_THREAD_RSPTR();
    CHECK_LIMIT();
    mz_prepare(3);
    jit_movi_i(JIT_V1, cnt);
    jit_pusharg_i(JIT_V1);
    jit_pusharg_p(JIT_R2);
    jit_pusharg_i(JIT_R1);
    CHECK_LIMIT();
    (void)mz_finish(scheme_build_list_offset);
    jit_retval(JIT_V1);
    if (data->closure_size)
      mz_popr_p(JIT_R0);
    jit_stxi_p(WORDS_TO_BYTES(cnt), JIT_RUNSTACK, JIT_V1);
    mz_patch_ucbranch(ref2); /* jump here if we copied and produced null */
    CHECK_LIMIT();

    __END_SHORT_JUMPS__(cnt < 100);
  }

  /* Extract closure to runstack: */
  cnt = data->closure_size;
  if (cnt) {
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(cnt));
    
    for (i = cnt; i--; ) {
      int pos;
      pos = WORDS_TO_BYTES(i) + (long)&((Scheme_Native_Closure *)0x0)->vals;
      jit_ldxi_p(JIT_R1, JIT_R0, pos);
      jit_stxi_p(WORDS_TO_BYTES(i), JIT_RUNSTACK, JIT_R1);
      CHECK_LIMIT();
    }
  }

  /* If we have a letrec context, record arities */
  if (data->context && SAME_TYPE(SCHEME_TYPE(data->context), scheme_letrec_type)) {
    Scheme_Letrec *lr = (Scheme_Letrec *)data->context;
    int pos;
    for (i = data->closure_size; i--; ) {
      pos = data->closure_map[i];
      if (pos < lr->count) {
	Scheme_Closure_Data *data2 = (Scheme_Closure_Data *)lr->procs[pos];
	mz_runstack_closure_pushed(jitter, (data2->num_params
					    - ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
					       ? 1
					       : 0)));
      } else
	mz_runstack_pushed(jitter, 1);
    }
  }

  LOG_IT(("PROC: %s\n", (data->name ? scheme_format_utf8("~s", 2, 1, &data->name, NULL) : "???")));
  FOR_LOG(jitter->log_depth++);
      
  /* Generate code for the body: */
  jitter->need_set_rs = 1;
  r = generate(data->code, jitter, 1, 1);
  /* Result is in JIT_R0 */

  CHECK_LIMIT();

  /* r == 2 => tail call performed */
  if (r != 2) {
    mz_pop_local_p(JIT_RUNSTACK, JIT_LOCAL1);
    jit_sti_p(&MZ_RUNSTACK, JIT_RUNSTACK);
    jit_movr_p(JIT_RET, JIT_R0);
    jit_ret();
  }

  code_end = jit_get_ip().ptr;

  gdata->code = code;
  gdata->tail_code = tail_code;
  gdata->max_extra = jitter->max_extra_pushed;
  gdata->max_depth = jitter->max_depth;
  gdata->code_end = code_end;

  return 1;
}

static void on_demand_generate_top(Scheme_Native_Closure_Data *ndata)
{
  Scheme_Closure_Data *data;
  Generate_Closure_Data gdata;
  void *code, *tail_code, *arity_code;
  int has_rest, is_method, num_params, max_depth;

  data = ndata->u2.orig_code;

  gdata.data = data;

  generate_one(NULL, do_generate_closure, &gdata, 1, data->name);

  if (gdata.max_depth > data->max_let_depth) {
    scheme_console_printf("Bad max depth!\n");
    abort();
  }

  code = gdata.code;
  tail_code = gdata.tail_code;
  
  if (data->name) {
    add_symbol((unsigned long)code, (unsigned long)gdata.code_end - 1, data->name, 1);
  }
  
  has_rest = ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST) ? 1 : 0);
  is_method = ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_IS_METHOD) ? 1 : 0);
  num_params = data->num_params;
  if (num_params && has_rest)
    --num_params;

  if (num_params < MAX_SHARED_ARITY_CHECK) {
    arity_code = shared_arity_check[num_params][has_rest][is_method];
    if (!arity_code) {
      arity_code = generate_top_simple_arity_check(num_params, has_rest, is_method, 1);
      shared_arity_check[num_params][has_rest][is_method] = arity_code;
    }
  } else
    arity_code = generate_top_simple_arity_check(num_params, has_rest, is_method, 0);

  max_depth = WORDS_TO_BYTES(data->max_let_depth + gdata.max_extra);

  /* max_let_depth is used for flags by generate_top: */
  if (ndata->max_let_depth & 0x1) {
    data->code = NULL;
  }
  data->context = NULL;
  if (ndata->max_let_depth & 0x2) {
    Scheme_Native_Closure_Data *case_lam;
    case_lam = ((Scheme_Native_Closure_Data_Plus_Case *)ndata)->case_lam;
    if (case_lam->max_let_depth < max_depth)
      case_lam->max_let_depth = max_depth;
  }
    
  ndata->code = code;
  ndata->u.tail_code = tail_code;
  ndata->arity_code = arity_code;
  ndata->u2.name = data->name;
  /* Let-depth is in bytes instead of words: */
  ndata->max_let_depth = max_depth;
}

static void on_demand()
{
  /* On runstack: closure (nearest), argc, argv (deepest) */
  Scheme_Object *c, *argc, **argv;
  Scheme_Native_Closure_Data *ndata;

  c = MZ_RUNSTACK[0];
  argc = MZ_RUNSTACK[1];
  argv = (Scheme_Object **)MZ_RUNSTACK[2];

  ndata = ((Scheme_Native_Closure *)c)->code;
  on_demand_generate_top(ndata);
}

Scheme_Native_Closure_Data *scheme_generate_lambda(Scheme_Closure_Data *data, int clear_code_after_jit,
						   Scheme_Native_Closure_Data *case_lam)
{
  Scheme_Native_Closure_Data *ndata;

  if (!jump_to_native_code) {
    /* Create shared code used for stack-overflow handling, etc.: */
    generate_one(NULL, do_generate_common, NULL, 0, NULL);
  }

  if (!case_lam) {
    ndata = MALLOC_ONE_RT(Scheme_Native_Closure_Data);
  } else {
    Scheme_Native_Closure_Data_Plus_Case *ndatap;
    ndatap = MALLOC_ONE_RT(Scheme_Native_Closure_Data_Plus_Case);
    ndatap->case_lam = case_lam;
    ndata = (Scheme_Native_Closure_Data *)ndatap;
  }
  ndata->code = on_demand_jit_code;
  ndata->u.tail_code = on_demand_jit_arity_code;
  ndata->arity_code = on_demand_jit_arity_code;
  ndata->u2.orig_code = data;
  ndata->closure_size = data->closure_size;
  ndata->max_let_depth = 0x4 | (case_lam ? 0x2 : 0) | (clear_code_after_jit ? 0x1 : 0);

#if 0
  /* Compile immediately: */
  on_demand_generate_top(ndata);
#endif

  return ndata;
}

static int generate_simple_arity_check(mz_jit_state *jitter, int num_params, int has_rest, int is_method)
{
  /* JIT_R0 is closure */
  /* JIT_R1 is argc */
  /* JIT_R2 is argv */
  /* If arity matches, JIT_RUNSTACK and JIT_RUNSTACK_BASE should be preserved */
  /* That leaves just JIT_V1 to use if airty is ok. */

  jit_insn *ref, *ref2;

  __START_SHORT_JUMPS__(1);

  if (!has_rest)
    ref = jit_bnei_i(jit_forward(), JIT_R1, num_params);
  else
    ref = jit_blti_i(jit_forward(), JIT_R1, num_params);

  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_i(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->u.tail_code);
  jit_jmpr(JIT_V1);
  CHECK_LIMIT();

  /* Failed */
  mz_patch_branch(ref);
  
  /* If argc is negative, this was really a request for arity checking or reporting */
  ref = jit_blti_i(jit_forward(), JIT_R1, 0x0);

  /* Not negative, so report run-time arity mismatch */
  mz_prepare(5);
  jit_pusharg_p(JIT_R2);
  jit_pusharg_p(JIT_R1);
  jit_movi_i(JIT_V1, -1);
  CHECK_LIMIT();
  jit_pusharg_i(JIT_V1);
  jit_pusharg_i(JIT_V1);
  jit_pusharg_p(JIT_R0);
  (void)mz_finish(scheme_wrong_count);
  CHECK_LIMIT();

  /* Arity check or reporting. If argv is NULL, it's a reporting request */
  mz_patch_branch(ref);
  ref = jit_beqi_i(jit_forward(), JIT_R2, 0x0);
  
  /* Arity check --- try again with argv cast to argc: */
  jit_subi_i(JIT_R2, JIT_R2, 1);
  if (!has_rest)
    ref2 = jit_bnei_i(jit_forward(), JIT_R2, num_params);
  else
    ref2 = jit_blti_i(jit_forward(), JIT_R2, num_params);
  CHECK_LIMIT();
  jit_movi_i(JIT_RET, 1);
  jit_ret();
  mz_patch_branch(ref2);
  jit_movi_i(JIT_RET, 0);
  jit_ret();
  CHECK_LIMIT();

  /* Finally, we know that it was an arity-report request */
  mz_patch_branch(ref);
  if (!has_rest) 
    (void)jit_movi_p(JIT_R0, scheme_make_integer(num_params));
  else
    (void)jit_movi_p(JIT_R0, scheme_make_integer(-(num_params+1)));
  CHECK_LIMIT();
  if (is_method) {
    mz_prepare(1);
    jit_pusharg_p(JIT_R0);
    (void)mz_finish(scheme_box);
    jit_ret();
  } else {
    jit_movr_p(JIT_RET, JIT_R0);
    jit_ret();
  }

  __END_SHORT_JUMPS__(1);

  return 1;
}

typedef struct {
  int num_params;
  int has_rest;
  int is_method;
} Generate_Arity_Check_Data;

static int do_generate_top_simple_arity_check(mz_jit_state *jitter, void *_data)
{
  Generate_Arity_Check_Data *data = (Generate_Arity_Check_Data *)_data;
  
#ifdef MZ_USE_JIT_PPC
  jitter->js.jitl.nbArgs = 2; /* matches check_arity_code prolog */
#endif

  return generate_simple_arity_check(jitter, data->num_params, data->has_rest, data->is_method);
}

static void *generate_top_simple_arity_check(int num_params, int has_rest, int is_method, int permanent)
{
  Generate_Arity_Check_Data data;

  data.num_params = num_params;
  data.has_rest = has_rest;
  data.is_method = is_method;

  return generate_one(NULL, do_generate_top_simple_arity_check, &data, !permanent, NULL);
}

int generate_case_lambda_dispatch(mz_jit_state *jitter, Scheme_Case_Lambda *c, Scheme_Native_Closure_Data *ndata,
				  int do_getarg)
{
  /* See top of generate_simple_arity_check for register info. */
  Scheme_Closure_Data *data;
  Scheme_Object *o;
  int i, cnt, has_rest, offset, num_params;
  jit_insn *ref = NULL;

  cnt = c->count;
  for (i = 0; i < cnt; i++) {
    /* Check arity for this case: */
    o = c->array[i];
    if (SCHEME_PROCP(o))
      o = (Scheme_Object *)((Scheme_Closure *)o)->code;
    data = (Scheme_Closure_Data *)o;

    num_params = data->num_params;
    has_rest = ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST) ? 1 : 0);
    if (has_rest && num_params)
      --num_params;

    /* Check for arity match - not needed in getarg mode if this
       is the last case, since the arity check as already done. */
    if (!do_getarg || (i < cnt - 1)) {
      if (!has_rest)
	ref = jit_bnei_i(jit_forward(), JIT_R1, num_params);
      else
	ref = jit_blti_i(jit_forward(), JIT_R1, num_params);
    }

    /* Function-argument handling for this case: */
    if (do_getarg) {
      generate_function_getarg(jitter, has_rest, num_params + (has_rest ? 1 : 0));
      CHECK_LIMIT();
    }
    
    /* Jump to tail-code location of the selected branch: */
    offset = WORDS_TO_BYTES(i) + (unsigned long)&((Scheme_Native_Closure *)0x0)->vals;
    jit_ldxi_p(JIT_R0, JIT_R0, offset);
    jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
    jit_ldxi_i(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->u.tail_code);
    jit_jmpr(JIT_V1);
    CHECK_LIMIT();
    
    if (!do_getarg || (i < cnt - 1)) {
      mz_patch_branch(ref);
    }
    /* Try the next one... */
  }

  if (!do_getarg) {
    /* Report run-time arity mismatch */
    JIT_UPDATE_THREAD_RSPTR();
    mz_prepare(5);
    jit_pusharg_p(JIT_R2);
    jit_pusharg_p(JIT_R1);
    jit_movi_i(JIT_V1, -1);
    CHECK_LIMIT();
    jit_pusharg_i(JIT_V1);
    jit_pusharg_i(JIT_V1);
    jit_pusharg_p(JIT_R0);
    (void)mz_finish(scheme_wrong_count);
    CHECK_LIMIT();
  }

  return 1;
}

typedef struct {
  Scheme_Case_Lambda *c;
  Scheme_Native_Closure_Data *ndata;
  int is_method;
} Generate_Case_Dispatch_Data;

static int do_generate_case_lambda_dispatch(mz_jit_state *jitter, void *_data)
{
  Generate_Case_Dispatch_Data *data = (Generate_Case_Dispatch_Data *)_data;
  void *code, *arity_code;

  code = jit_get_ip().ptr;
  
  generate_function_prolog(jitter, code, data->ndata->max_let_depth);
  CHECK_LIMIT();
  
  if (generate_case_lambda_dispatch(jitter, data->c, data->ndata, 1)) {
    arity_code = jit_get_ip().ptr;
    if (generate_case_lambda_dispatch(jitter, data->c, data->ndata, 0)) {
      data->ndata->code = code;
      data->ndata->arity_code = arity_code;
      
      return 1;
    }
  }

  return 0;
}

static void generate_top_case_lambda_dispatch(Scheme_Case_Lambda *c, Scheme_Native_Closure_Data *ndata, int is_method)
{
  Generate_Case_Dispatch_Data gdata;
  Scheme_Closure_Data *data;
  Scheme_Object *o;
  int i, cnt, num_params, has_rest;
  mzshort *arities;

  gdata.c = c;
  gdata.ndata = ndata;
  gdata.is_method = is_method;

  generate_one(NULL, do_generate_case_lambda_dispatch, &gdata, 1, NULL);

  /* Generate arity table used by scheme_native_arity_check
     and scheme_get_native_arity: */
  
  cnt = c->count;
  arities = (mzshort *)scheme_malloc_atomic(sizeof(mzshort) * (cnt + 1));
  arities[cnt] = is_method;
  for (i = 0; i < cnt; i++) {
    o = c->array[i];
    if (SCHEME_PROCP(o))
      o = (Scheme_Object *)((Scheme_Closure *)o)->code;
    data = (Scheme_Closure_Data *)o;
    num_params = data->num_params;
    has_rest = ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST) ? 1 : 0);
    if (has_rest && num_params)
      --num_params;
	  
    if (!has_rest) 
      arities[i] = num_params;
    else
      arities[i] = -(num_params+1);
  }
  ndata->u.arities = arities;
}

/*========================================================================*/
/*                          native arity queries                          */
/*========================================================================*/

int scheme_native_arity_check(Scheme_Object *closure, int argc)
{
  int cnt;

  cnt = ((Scheme_Native_Closure *)closure)->code->closure_size;
  if (cnt < 0) {
    /* Case-lambda */
    int i;
    mzshort *arities, v;

    arities = ((Scheme_Native_Closure *)closure)->code->u.arities;
    cnt = -(cnt + 1);
    for (i = 0; i < cnt; i++) {
      v = arities[i];
      if (v < 0) {
	v = -(v + 1);
	if (argc >= v)
	  return 1;
      } else if (argc == v)
	return 1;
    }
    return 0;
  }

  if (((Scheme_Native_Closure *)closure)->code->code == on_demand_jit_code) {
    Scheme_Closure c;
    c.so.type = scheme_closure_type;
    c.code = ((Scheme_Native_Closure *)closure)->code->u2.orig_code;
    return SCHEME_TRUEP(scheme_get_or_check_arity((Scheme_Object *)&c, argc));
  }

  return check_arity_code(closure, argc + 1);
}

Scheme_Object *scheme_get_native_arity(Scheme_Object *closure)
{
  int cnt;

  cnt = ((Scheme_Native_Closure *)closure)->code->closure_size;
  if (cnt < 0) {
    /* Case-lambda */
    Scheme_Object *l = scheme_null, *a;
    int i, has_rest, is_method;
    mzshort *arities, v;

    arities = ((Scheme_Native_Closure *)closure)->code->u.arities;
    cnt = -(cnt + 1);
    is_method = arities[cnt];
    for (i = cnt; i--; ) {
      v = arities[i];
      if (v < 0) {
	v = -(v + 1);
	has_rest = 1;
      } else 
	has_rest = 0;
      if (is_method && v)
	--v;
      a = scheme_make_arity(v, has_rest ? -1 : v);
      l = scheme_make_pair(a, l);
    }
    if (is_method)
      l = scheme_box(l);
    return l;
  }

  if (((Scheme_Native_Closure *)closure)->code->code == on_demand_jit_code) {
    Scheme_Closure c;
    c.so.type = scheme_closure_type;
    c.code = ((Scheme_Native_Closure *)closure)->code->u2.orig_code;
    return scheme_get_or_check_arity((Scheme_Object *)&c, -1);
  }

  return get_arity_code(closure);
}

/*========================================================================*/
/*                              stack trace                               */
/*========================================================================*/

typedef void *(*Get_Stack_Proc)();

#ifdef MZ_USE_JIT_PPC
# ifdef _CALL_DARWIN
#  define RETURN_ADDRESS_OFFSET 2
# else
#  define RETURN_ADDRESS_OFFSET 1
# endif
#endif
#ifdef MZ_USE_JIT_I386
# define RETURN_ADDRESS_OFFSET 1
#endif

#define CACHE_STACK_MIN_TRIGGER 1024

#define USE_STACK_CHECK 0

#if USE_STACK_CHECK
static void check_stack(void)
{
  void *p, *q;
  unsigned long stack_end;
  int pos = stack_cache_stack_pos;
  Get_Stack_Proc gs;

  gs = (Get_Stack_Proc)get_stack_pointer_code;
  p = gs();

  stack_end = (unsigned long)(scheme_current_thread->next 
			      ? scheme_current_thread->stack_start 
			      : scheme_current_thread->o_start);

  while (STK_COMP((unsigned long)p, stack_end)) {
    q = ((void **)p)[RETURN_ADDRESS_OFFSET];

    if (q == stack_cache_pop_code) {
      if (!pos)
	*(long *)0x0 = 1;
      else {
	if (stack_cache_stack[pos].stack_frame != (void *)(((void **)p) + RETURN_ADDRESS_OFFSET)) {
	  *(long *)0X0 = 1;
	}
	--pos;
      }
    }

    q = *(void **)p;
    if (STK_COMP((unsigned long)q, (unsigned long)p))
      break;
    p = q;
  }
}
#endif

unsigned long scheme_approx_sp()
{
  unsigned long p;
  p = (unsigned long)&p;
  return p;
}

Scheme_Object *scheme_native_stack_trace(void)
{
  void *p, *q;
  unsigned long stack_end, stack_start, halfway;
  Get_Stack_Proc gs;
  Scheme_Object *name, *last = NULL, *first = NULL, *tail;
  int set_next_push = 0, prev_had_name = 0;

  if (!get_stack_pointer_code)
    return NULL;

#if USE_STACK_CHECK
  check_stack();
#endif

  gs = (Get_Stack_Proc)get_stack_pointer_code;
  p = gs();
  stack_start = scheme_approx_sp();

  if (stack_cache_stack_pos) {
    stack_end = (unsigned long)stack_cache_stack[stack_cache_stack_pos].stack_frame;
    stack_end -= (RETURN_ADDRESS_OFFSET << JIT_LOG_WORD_SIZE);
    tail = stack_cache_stack[stack_cache_stack_pos].cache;
  } else {
    stack_end = (unsigned long)(scheme_current_thread->next 
				? scheme_current_thread->stack_start 
				: scheme_current_thread->o_start);
    tail = scheme_null;
  }

  halfway = STK_DIFF(stack_end, (unsigned long)p) / 2;
  if (halfway < CACHE_STACK_MIN_TRIGGER)
    halfway = stack_end;
  else {
#ifdef STACK_GROWS_DOWN
    halfway += (unsigned long)p;
#else
    halfway += stack_end;
#endif
  }

  while (STK_COMP((unsigned long)p, stack_end)
	 && STK_COMP(stack_start, (unsigned long)p)) {
    q = ((void **)p)[RETURN_ADDRESS_OFFSET];

    name = find_symbol((unsigned long)q);
    if (SCHEME_FALSEP(name)) {
      /* Code uses special calling convention */
#ifdef MZ_USE_JIT_PPC
      /* JIT_LOCAL2 has the next return address */
      q = ((void **)p)[JIT_LOCAL2 >> JIT_LOG_WORD_SIZE];
#endif
#ifdef MZ_USE_JIT_I386
      /* 2nd push onto local stack of return-address proc
	 has the next return address */
      q = *(void **)p;
      q = ((void **)q)[-5];
#endif
      name = find_symbol((unsigned long)q);
    }

    if (name) {
      name = scheme_make_pair(name, scheme_null);
      if (last)
	SCHEME_CDR(last) = name;
      else
	first = name;
      last = name;
      if (set_next_push) {
	stack_cache_stack[stack_cache_stack_pos].cache = name;
	set_next_push = 0;
      }
    }

    /* Cache the result halfway up the stack, if possible. Only cache
       on frames where the previous frame had a return address with a
       name, because an arbitrary frame's return address on the stack
       might not be used (depending on how the C compiler optimized the
       cdode); any frame whose procedure has a name is JITted code, so
       it will use the return address from the stack. */
    if (STK_COMP((unsigned long)halfway, (unsigned long)p)
	&& prev_had_name) {
      int pos;

      if (stack_cache_stack_pos >= (STACK_CACHE_SIZE - 1)) {
	/* Make room on the stack */
	void **z;
	z = (void **)stack_cache_stack[stack_cache_stack_pos].stack_frame;
	*z = stack_cache_stack[stack_cache_stack_pos].orig_return_address;
	--stack_cache_stack_pos;
      }

      pos = ++stack_cache_stack_pos;
      stack_cache_stack[pos].orig_return_address = ((void **)p)[RETURN_ADDRESS_OFFSET];
      stack_cache_stack[pos].stack_frame = (void *)(((void **)p) + RETURN_ADDRESS_OFFSET);
      stack_cache_stack[pos].cache = tail;
      set_next_push = 1;
      ((void **)p)[RETURN_ADDRESS_OFFSET] = stack_cache_pop_code;

      halfway = stack_end;
    }

    prev_had_name = !!name;

    q = *(void **)p;
    if (STK_COMP((unsigned long)q, (unsigned long)p))
      break;
    p = q;
  }

  if (last)
    SCHEME_CDR(last) = tail;
  else
    first = tail;

  if (SCHEME_NULLP(first))
    return NULL;

  return first;
}

void scheme_flush_stack_cache()
{
  void **p;

  while (stack_cache_stack_pos) {
    p = (void **)stack_cache_stack[stack_cache_stack_pos].stack_frame;
    *p = stack_cache_stack[stack_cache_stack_pos].orig_return_address;
    --stack_cache_stack_pos;
  }
}

void scheme_jit_longjmp(mz_jit_jmp_buf b, int v)
{
  unsigned long limit;
  void **p;

  limit = (unsigned long)b->stack_frame;

  while (stack_cache_stack_pos
	 && STK_COMP((unsigned long)stack_cache_stack[stack_cache_stack_pos].stack_frame,
		     limit)) {
    p = (void **)stack_cache_stack[stack_cache_stack_pos].stack_frame;
    *p = stack_cache_stack[stack_cache_stack_pos].orig_return_address;
    --stack_cache_stack_pos;
  }

  scheme_mz_longjmp(b->jb, v);
}

void scheme_jit_setjmp_prepare(mz_jit_jmp_buf b)
{
  void *p;
  p = &p;
  b->stack_frame = p;
}

void scheme_clean_native_symtab(void)
{
  clear_symbols_for_collected();
}

#endif /* MZ_USE_JIT */
