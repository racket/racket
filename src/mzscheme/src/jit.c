/*
  MzScheme
  Copyright (c) 2007 PLT Scheme Inc.

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

/*
  JIT limtations:

  1) See "About short-jump mode" below.

  2) Use jit_patchable_movi_p() when a constant needs to be
     visible to the GC.

  3) Immediate operands must be 32-bit values on x86_64, except with
     jit_movi, jit_sti, jit_ld, jit_bXi, jit_calli, and jit_finishi.

  4) Function calls are limited to 3 arguments (i.e., jit_prepare()
     must never be called with a number greater than 3). This limit
     is related to the way the x86_64 port shuffles arguments into
     temporary registers.

  5) On x86_64, arguments are delivered in JIT_V2, JIT_V3, and JIT_R2,
     in that order. So don't set JIT_R2 before getting the third
     argument, etc.
*/

#include "schpriv.h"
#include "schmach.h"

#ifdef MZ_USE_JIT

#ifdef __APPLE__
# define _CALL_DARWIN
#endif

/* Separate JIT_PRECISE_GC lets us test some 3m support 
   in non-3m mode: */
#ifdef MZ_PRECISE_GC
# define JIT_PRECISE_GC
#endif

/* IMPORTANT! 3m arithmetic checking disabled for the whole file! */
#ifdef MZ_PRECISE_GC
END_XFORM_ARITH;
#endif

#ifdef MZ_USE_JIT_X86_64
# define MZ_USE_JIT_I386
# define JIT_X86_64
#endif

#include "lightning/lightning.h"

#ifdef MZ_USE_JIT_X86_64
# define JIT_LOG_WORD_SIZE 3
#else
# define JIT_LOG_WORD_SIZE 2
#endif
#define JIT_WORD_SIZE (1 << JIT_LOG_WORD_SIZE)
#define WORDS_TO_BYTES(x) ((x) << JIT_LOG_WORD_SIZE)
#define MAX_TRY_SHIFT 30

/* a mzchar is an int: */
#define LOG_MZCHAR_SIZE 2

#if defined(MZ_USE_JIT_PPC) || defined(MZ_USE_JIT_X86_64)
# define NEED_LONG_JUMPS
#endif

#define JIT_NOT_RET JIT_R1
#if JIT_NOT_RET == JIT_RET
Fix me! See use.
#endif

#define MAX_SHARED_CALL_RANDS 25
static void *shared_tail_code[4][MAX_SHARED_CALL_RANDS];
static void *shared_non_tail_code[3][MAX_SHARED_CALL_RANDS][2];
static void *shared_non_tail_retry_code[2];
static void *shared_non_tail_argc_code[2];
static void *shared_tail_argc_code;

#define MAX_SHARED_ARITY_CHECK 25
static void *shared_arity_check[MAX_SHARED_ARITY_CHECK][2][2];

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
static void *bad_caar_code, *bad_cdar_code, *bad_cadr_code, *bad_cddr_code;
static void *bad_set_car_code, *bad_set_cdr_code;
static void *bad_unbox_code;
static void *vector_ref_code, *vector_ref_check_index_code, *vector_set_code, *vector_set_check_index_code;
static void *string_ref_code, *string_ref_check_index_code, *string_set_code, *string_set_check_index_code;
static void *bytes_ref_code, *bytes_ref_check_index_code, *bytes_set_code, *bytes_set_check_index_code;
static void *syntax_e_code;
static void *on_demand_jit_code;
static void *on_demand_jit_arity_code;
static void *get_stack_pointer_code;
static void *stack_cache_pop_code;
static void *struct_pred_code;
static void *struct_pred_branch_code;
static void *struct_get_code;
static void *bad_app_vals_target;
static void *app_values_slow_code, *app_values_multi_slow_code, *app_values_tail_slow_code;
static void *finish_tail_call_code, *finish_tail_call_fixup_code;

typedef struct {
  MZTAG_IF_REQUIRED
  GC_CAN_IGNORE jit_state js;
  char *limit;
  int extra_pushed, max_extra_pushed;
  int depth; /* the position of the closure's first arg on the stack */
  int max_depth;
  int *mappings; /* For each element,
		    case 0x1 bit:
		    . 0 -> case 0x2 bit:
                    .        0 -> case rest bits:
                    .               0 -> save point
                    .               1 -> shift >>2 to get orig pushed count
                    .        1 -> shift >>4 to get arity for single orig pushed
                                  shift >>2 to get flags
		    . 1 -> shift >>1 to get new (native) pushed */
  int num_mappings, mappings_size;
  int retained;
  int need_set_rs;
  void **retain_start;
  int log_depth;
  int self_pos, self_closure_size, self_toplevel_pos;
  void *self_restart_code;
  Scheme_Native_Closure *nc; /* for extract_globals, only */
  Scheme_Closure_Data *self_data;
} mz_jit_state;

typedef int (*Native_Check_Arity_Proc)(Scheme_Object *o, int argc, int dummy);
typedef Scheme_Object *(*Native_Get_Arity_Proc)(Scheme_Object *o, int dumm1, int dummy2);
static Native_Check_Arity_Proc check_arity_code;
static Native_Get_Arity_Proc get_arity_code;

static int generate_non_tail(Scheme_Object *obj, mz_jit_state *jitter, int multi_ok, int need_ends);
static int generate(Scheme_Object *obj, mz_jit_state *jitter, int tail_ok, int multi_ok);
static void *generate_lambda_simple_arity_check(int num_params, int has_rest, int is_method, int permanent);
static void generate_case_lambda(Scheme_Case_Lambda *c, Scheme_Native_Closure_Data *ndata, 
				 int is_method);
static void on_demand();
static int generate_non_tail_mark_pos_prefix(mz_jit_state *jitter);
static void generate_non_tail_mark_pos_suffix(mz_jit_state *jitter);
static void *generate_shared_call(int num_rands, mz_jit_state *old_jitter, int multi_ok, int is_tail, 
				  int direct_prim, int direct_native);

static int is_simple(Scheme_Object *obj, int depth, int just_markless, mz_jit_state *jitter, int stack_start);
static int lambda_has_been_jitted(Scheme_Native_Closure_Data *ndata);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
static void release_native_code(void *fnlized, void *p);
#endif

#define NATIVE_PRESERVES_MARKS 0x1
#define NATIVE_IS_SINGLE_RESULT 0x2

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
long stack_cache_stack_pos = 0;

#define IS_NAMED_PRIM(p, nm) (!strcmp(((Scheme_Primitive_Proc *)p)->name, nm))

#include "codetab.inc"

static Scheme_Object **fixup_runstack_base;
static int fixup_already_in_place;

static Scheme_Object *_scheme_tail_apply_from_native_fixup_args(Scheme_Object *rator,
                                                                int argc,
                                                                Scheme_Object **argv)
{
  int already = fixup_already_in_place, i;
  Scheme_Object **base;

  base = fixup_runstack_base XFORM_OK_MINUS argc XFORM_OK_MINUS already;

  /* Need to shift argc to end of base: */
  for (i = 0; i < argc; i++) {
    base[already + i] = argv[i];
  }

  return _scheme_tail_apply_from_native(rator, argc + already, base);
}

/*========================================================================*/
/*                              JIT buffer                                */
/*========================================================================*/

#ifdef SIXTY_FOUR_BIT_INTEGERS
# define JIT_BUFFER_PAD_SIZE 200
#else
# define JIT_BUFFER_PAD_SIZE 100
#endif

#define _jit (jitter->js)
#define PAST_LIMIT() ((unsigned long)jit_get_ip().ptr > (unsigned long)jitter->limit)
#define CHECK_LIMIT() if (PAST_LIMIT()) return past_limit(jitter);
#if 1
# define past_limit(j) 0
#else
static int past_limit(mz_jit_state *jitter)
{
  if (((unsigned long)jit_get_ip().ptr > (unsigned long)jitter->limit + JIT_BUFFER_PAD_SIZE)
      || (jitter->retain_start)) {
    printf("way past\n");
  }
  return 0;
}
#endif

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

static int mz_retain_it(mz_jit_state *jitter, void *v)
{
  if (jitter->retain_start) {
    jitter->retain_start[jitter->retained] = v;
  }
  jitter->retained++;
  return jitter->retained;
}

#ifdef JIT_PRECISE_GC
static void mz_load_retained(mz_jit_state *jitter, int rs, int retptr)
{
  void *p;
  p = jitter->retain_start + retptr - 1;
  (void)jit_patchable_movi_p(rs, p);
  jit_ldr_p(rs, rs);
}
#endif

static void *generate_one(mz_jit_state *old_jitter, 
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
  long size = JIT_BUFFER_INIT_SIZE, known_size = 0, size_pre_retained = 0, num_retained = 0, padding;
  int mappings_size = JIT_INIT_MAPPINGS_SIZE;
  int ok, max_extra_pushed = 0;
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
    REGISTER_SO(stack_cache_stack);
#ifdef MZ_PRECISE_GC
    register_traversers();
#endif
    /* printf("zap!\n"); */
  }

  while (1) {
    memset(jitter, 0, sizeof(_jitter));
#ifdef NEED_LONG_JUMPS
    _jitl.long_jumps = 1;
#endif
    padding = JIT_BUFFER_PAD_SIZE;
    if (known_size) {
      size_pre_retained = known_size;
      size = size_pre_retained + WORDS_TO_BYTES(num_retained);
      padding = 0;
      if (gcable) {
#ifdef MZ_PRECISE_GC
	buffer = malloc(size);
#else
	buffer = scheme_malloc(size);
#endif
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
#ifdef MZ_PRECISE_GC
	long minsz;
	minsz = GC_malloc_stays_put_threshold();
	if (size < minsz)
	  size = minsz;
	buffer = (char *)scheme_malloc_atomic(size);
#else
	buffer = scheme_malloc(size);
#endif
      }
      size_pre_retained = size;
    }
      
    (void)jit_set_ip(buffer).ptr;
    jitter->limit = (char *)buffer + size_pre_retained - padding;
    if (known_size) {
      jitter->retain_start = (void *)jitter->limit;
#ifdef MZ_PRECISE_GC
      if (ndata) {
	memset(jitter->retain_start, 0, num_retained * sizeof(void*));
	ndata->retained = jitter->retain_start;
	ndata->retain_count = num_retained;
	SCHEME_BOX_VAL(fnl_obj) = scheme_make_integer(size_pre_retained);
	GC_set_finalizer(fnl_obj, 1, 3,
			 release_native_code, buffer,
			 NULL, NULL);
      }
#endif
    } else
      jitter->retain_start = NULL;

    jitter->mappings = mappings;
    jitter->num_mappings = 0;
    jitter->mappings_size = mappings_size;
    mappings[0] = 0;
    jitter->max_extra_pushed = max_extra_pushed;
    jitter->self_pos = 1; /* beyond end of stack */
    jitter->self_toplevel_pos = -1;

    ok = generate(jitter, data);

    if (save_ptr) {
      mz_retain_it(jitter, save_ptr);
    }
#ifdef MZ_PRECISE_GC
    if (fnl_obj) {
      mz_retain_it(jitter, fnl_obj);
    }
#endif

    jitter->limit = (char *)jitter->limit + padding;
    if (PAST_LIMIT() || (jitter->retain_start
			 && (jitter->retained > num_retained))) {
      scheme_console_printf("JIT buffer overflow: %p [%p,%p] (%d)!!\n", 
			    jit_get_ip().ptr, 
			    buffer, jitter->limit,
			    !!jitter->retain_start);
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
	if (known_size & (JIT_WORD_SIZE - 1)) {
	  known_size += (JIT_WORD_SIZE - (known_size & (JIT_WORD_SIZE - 1)));
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

static MZ_INLINE Scheme_Object *do_make_native_closure(Scheme_Native_Closure_Data *code, int size)
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

static void call_set_global_bucket(Scheme_Bucket *b, Scheme_Object *val, int set_undef)
{
  scheme_set_global_bucket("set!", b, val, set_undef);
}

static void lexical_binding_wrong_return_arity(int expected, int got, Scheme_Object **argv)
{
  scheme_wrong_return_arity(NULL, expected, got, argv, "lexical binding");
}

static void call_wrong_return_arity(int expected, int got, Scheme_Object **argv)
  
{
  scheme_wrong_return_arity(NULL, expected, got, argv, NULL);
}

static void wrong_argument_count(Scheme_Object *proc, int argc, Scheme_Object **argv)
{
  scheme_wrong_count((char *)proc, -1, -1, argc, argv);
}

static void raise_bad_call_with_values(Scheme_Object *f)
{
  Scheme_Object *a[1];
  a[0] = f;
  scheme_wrong_type("call-with-values", "procedure", -1, 1, a);    
}

static Scheme_Object *call_with_values_from_multiple_result(Scheme_Object *f)
{
    Scheme_Thread *p = scheme_current_thread;
  if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
    p->values_buffer = NULL;
  return _scheme_apply(f, p->ku.multiple.count, p->ku.multiple.array);
}

static Scheme_Object *call_with_values_from_multiple_result_multi(Scheme_Object *f)
{
    Scheme_Thread *p = scheme_current_thread;
  if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
    p->values_buffer = NULL;
  return _scheme_apply_multi(f, p->ku.multiple.count, p->ku.multiple.array);
}

static Scheme_Object *tail_call_with_values_from_multiple_result(Scheme_Object *f)
{
  Scheme_Thread *p = scheme_current_thread;
  int num_rands = p->ku.multiple.count;
  
  if (num_rands > p->tail_buffer_size) {
    /* scheme_tail_apply will allocate */
    if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
      p->values_buffer = NULL;
  }
  return scheme_tail_apply(f, num_rands, p->ku.multiple.array);
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

#if 0
/* Debugging: checking for runstack overflow. A CHECK_RUNSTACK_OVERFLOW() should
   be included after each decrement of JIT_RUNTACK. Failure is "reported" by
   going into an immediate loop. */
static void *top;
static void *cr_tmp;
# define CHECK_RUNSTACK_OVERFLOW_NOCL() \
     jit_sti_l(&cr_tmp, JIT_R0); jit_ldi_l(JIT_R0, &scheme_current_runstack_start); \
  top = (_jit.x.pc); (void)jit_bltr_ul(top, JIT_RUNSTACK, JIT_R0); jit_ldi_l(JIT_R0, &cr_tmp)
# define CHECK_RUNSTACK_OVERFLOW() \
     CHECK_LIMIT(); CHECK_RUNSTACK_OVERFLOW_NOCL()
#else
# define CHECK_RUNSTACK_OVERFLOW() /* empty */
# define CHECK_RUNSTACK_OVERFLOW_NOCL() /* empty */
#endif

#if 0
/* Debugging: ... */
static void *top4;
# define VALIDATE_RESULT(reg) top4 = (_jit.x.pc); (void)jit_beqi_ul(top4, reg, 0)
#else
# define VALIDATE_RESULT(reg) /* empty */
#endif

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
  CHECK_RUNSTACK_OVERFLOW_NOCL();
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
  jitter->self_pos += n;
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
  jitter->self_pos -= n;
}

static void mz_runstack_pushed(mz_jit_state *jitter, int n)
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

static void mz_runstack_closure_pushed(mz_jit_state *jitter, int a, int flags)
{
  jitter->depth += 1;
  if (jitter->depth > jitter->max_depth)
    jitter->max_depth = jitter->depth;
  jitter->self_pos += 1;
  new_mapping(jitter);
  jitter->mappings[jitter->num_mappings] = (a << 4) | (flags << 2) | 0x2;
  jitter->need_set_rs = 1;
}

static void mz_runstack_popped(mz_jit_state *jitter, int n)
{
  int v;
  jitter->depth -= n;
  jitter->self_pos -= n;
  v = (jitter->mappings[jitter->num_mappings]) >> 2;
  v -= n;
  if (!v)
    --jitter->num_mappings;
  else
    jitter->mappings[jitter->num_mappings] = (v << 2);
  jitter->need_set_rs = 1;
}

static int mz_try_runstack_pop(mz_jit_state *jitter, int n)
{
  if (jitter->mappings[jitter->num_mappings] & 0x3)
    return 0;
  if ((jitter->mappings[jitter->num_mappings] >> 2) < n)
    return 0;
  mz_runstack_popped(jitter, n);
  return 1;
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
      jitter->self_pos--;
    } else {
      c = (c >> 2);
      amt += c;
      jitter->self_pos -= c;
    }
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

static int mz_is_closure(mz_jit_state *jitter, int i, int arity, int *_flags)
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
	if ((arity == (c >> 4)) || (arity == -1)) {
          *_flags = (c >> 2) & 0x3;
	  return 1;
        }
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

#if 0
/* Debugging: at each _finish(), double-check that the runstack register has been
   copied into scheme_current_runstack. This code assumes that mz_finishr() is not
   used with JIT_R0.  Failure is "reported" by going into an immediate loop, but
   check_location is set to the source line number to help indicate where the
   problem originated. */
static void *top;
int check_location;
# define CONFIRM_RUNSTACK() (jit_movi_l(JIT_R0, __LINE__), jit_sti_l(&check_location, JIT_R0), \
                             jit_ldi_p(JIT_R0, &MZ_RUNSTACK), top = (_jit.x.pc), jit_bner_p(top, JIT_RUNSTACK, JIT_R0))
#else
# define CONFIRM_RUNSTACK() 0
#endif

#define mz_prepare(x) jit_prepare(x)
#define mz_finish(x) ((void)CONFIRM_RUNSTACK(), jit_finish(x))
#define mz_finishr(x) ((void)CONFIRM_RUNSTACK(), jit_finishr(x))

#define mz_nonrs_finish(x) jit_finish(x)

#define mz_retain(x) mz_retain_it(jitter, x)
#define mz_remap(x) mz_remap_it(jitter, x)

#ifdef MZ_USE_JIT_PPC
/* JIT_LOCAL1, JIT_LOCAL2, and JIT_LOCAL3 are offsets in the stack frame. */
# define JIT_LOCAL1 56
# define JIT_LOCAL2 60
# define JIT_LOCAL3 64
# define mz_set_local_p(x, l) jit_stxi_p(l, 1, x)
# define mz_get_local_p(x, l) jit_ldxi_p(x, 1, l)
# define mz_patch_branch_at(a, v) (_jitl.long_jumps ? (void)jit_patch_movei(a-4, a-3, v) : (void)jit_patch_branch(a-1, v))
# define mz_patch_ucbranch_at(a, v) (_jitl.long_jumps ? (void)jit_patch_movei(a-4, a-3, v) : (void)jit_patch_ucbranch(a-1, v))
# define mz_prolog(x) (MFLRr(x), mz_set_local_p(x, JIT_LOCAL2))
# define mz_epilog(x) (mz_get_local_p(x, JIT_LOCAL2), jit_jmpr(x))
# define mz_epilog_without_jmp() /* empty */
# define jit_shuffle_saved_regs() /* empty */
# define jit_unshuffle_saved_regs() /* empty */
# define mz_push_locals() /* empty */
# define mz_pop_locals() /* empty */
static void _jit_prolog_again(mz_jit_state *jitter, int n, int ret_addr_reg)
{
  /* This must be consistent with _jit_prolog in many ways: */
  int frame_size;
  int ofs;
  int first_saved_reg = JIT_AUX - n;
  int num_saved_regs = 32 - first_saved_reg;

  frame_size = 24 + 32 + 12 + num_saved_regs * 4;	/* r27..r31 + args		   */
  frame_size += 15;			/* the stack must be quad-word     */
  frame_size &= ~15;			/* aligned			   */

  STWUrm(1, -frame_size, 1);		/* stwu  r1, -x(r1)		   */

  /* We actually only need to save V0-V2, which are at
     the end of the saved area: */
  first_saved_reg = 29;
  num_saved_regs = 3;

  ofs = frame_size - num_saved_regs * 4;
  STMWrm(first_saved_reg, ofs, 1);		/* stmw  rI, ofs(r1)		   */
#ifdef _CALL_DARWIN
  STWrm(ret_addr_reg, frame_size + 8, 1); /* stw   r0, x+8(r1)		   */
#else
  STWrm(ret_addr_reg, frame_size + 4, 1); /* stw   r0, x+4(r1)		   */
#endif
}
#else
# define JIT_LOCAL1 -(JIT_WORD_SIZE * 4)
# define JIT_LOCAL2 -(JIT_WORD_SIZE * 5)
# define mz_set_local_p(x, l) jit_stxi_p((l), JIT_FP, (x))
# define mz_get_local_p(x, l) jit_ldxi_p((x), JIT_FP, (l))
# define mz_patch_branch_at(a, v) jit_patch_branch_at(a, v)
# define mz_patch_ucbranch_at(a, v) jit_patch_ucbranch_at(a, v)
# ifdef _CALL_DARWIN
#  define X86_ALIGN_STACK
#  define STACK_ALIGN_WORDS 3
# endif
# ifdef JIT_X86_64
#  define X86_ALIGN_STACK
#  define STACK_ALIGN_WORDS 1
# endif
# ifdef X86_ALIGN_STACK
   /* Maintain 4-byte stack alignment. */
#  define mz_prolog(x) (SUBQir(STACK_ALIGN_WORDS * JIT_WORD_SIZE, JIT_SP))
#  define mz_epilog_without_jmp() ADDQir((STACK_ALIGN_WORDS + 1) * JIT_WORD_SIZE, JIT_SP)
#  define mz_epilog(x) (ADDQir(STACK_ALIGN_WORDS * JIT_WORD_SIZE, JIT_SP), RET_())
#  define LOCAL_FRAME_SIZE 3
#  define JIT_LOCAL3 -(JIT_WORD_SIZE * 6)
# else
#  define mz_prolog(x) /* empty */
#  define mz_epilog(x) RET_()
#  define mz_epilog_without_jmp() ADDQir(JIT_WORD_SIZE, JIT_SP)
#  define LOCAL_FRAME_SIZE 2
#  define JIT_LOCAL3 JIT_LOCAL2
# endif
# define mz_push_locals() SUBQir((LOCAL_FRAME_SIZE << JIT_LOG_WORD_SIZE), JIT_SP)
# define mz_pop_locals() ADDQir((LOCAL_FRAME_SIZE << JIT_LOG_WORD_SIZE), JIT_SP)
#define _jit_prolog_again(jitter, n, ret_addr_reg) (PUSHQr(ret_addr_reg), jit_base_prolog())
# ifdef MZ_USE_JIT_X86_64
#  define jit_shuffle_saved_regs() (MOVQrr(_ESI, _R12), MOVQrr(_EDI, _R13))
#  define jit_unshuffle_saved_regs() (MOVQrr(_R12, _ESI), MOVQrr(_R13, _EDI))
# else
#  define jit_shuffle_saved_regs() /* empty */
#  define jit_unshuffle_saved_regs() /* empty */
# endif
#endif

#define mz_patch_branch(a) mz_patch_branch_at(a, (_jit.x.pc))
#define mz_patch_ucbranch(a) mz_patch_ucbranch_at(a, (_jit.x.pc))

#ifdef NEED_LONG_JUMPS
# define __START_SHORT_JUMPS__(cond) if (cond) { _jitl.long_jumps = 0; }
# define __END_SHORT_JUMPS__(cond) if (cond) { _jitl.long_jumps = 1; }
#else
# define __START_SHORT_JUMPS__(cond) /* empty */
# define __END_SHORT_JUMPS__(cond) /* empty */
#endif

/* mz_b..i_p supports 64-bit constants on x86_64: */
#ifdef MZ_USE_JIT_X86_64
# define mz_beqi_p(a, v, i) ((void)jit_patchable_movi_p(JIT_REXTMP, i), jit_beqr_p(a, v, JIT_REXTMP))
# define mz_bnei_p(a, v, i) ((void)jit_patchable_movi_p(JIT_REXTMP, i), jit_bner_p(a, v, JIT_REXTMP))
#else
# define mz_beqi_p(a, v, i) jit_beqi_p(a, v, i)
# define mz_bnei_p(a, v, i) jit_bnei_p(a, v, i)
#endif

/* 
 About short-jump mode:
   
   In
      jit_jmpi(code);
   or
      jit_blti_i(code, v);
   the generated instructions can depend on the relative location
   between the instruction address and the actual value. Do not enable
   short jumps if the relative offset can change between the initial
   sizing pass and the final pass. Of course, also don't enable short
   jumps if the jump is potentially long (i.e. more than +/- 2^15
   on PowerPC, or more than +/- 2^31 on x86_64). Otherwise, enable
   shorty-jump mode as much as possible.

   All mz_finish() and jit_calli() are implicitly long jumps.
*/

/*========================================================================*/
/*                         bytecode properties                            */
/*========================================================================*/

#ifdef NEED_LONG_JUMPS
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
      if (t == CASE_LAMBDA_EXPD)
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
  case scheme_quote_syntax_type:
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

Scheme_Object *extract_global(Scheme_Object *o, Scheme_Native_Closure *nc)
{
  /* GLOBAL ASSUMPTION: we assume that globals are the last thing
     in the closure; grep for "GLOBAL ASSUMPTION" in fun.c. */
  Scheme_Object **globs;

  globs = (Scheme_Object **)nc->vals[nc->code->u2.orig_code->closure_size - 1];
  return globs[SCHEME_TOPLEVEL_POS(o)];
}

static int inlineable_struct_prim(Scheme_Object *o, mz_jit_state *jitter)
{
  if (jitter->nc
      && SAME_TYPE(SCHEME_TYPE(o), scheme_toplevel_type)) {
    Scheme_Object *p;
    p = extract_global(o, jitter->nc);
    p = ((Scheme_Bucket *)p)->val;
    if (p && SCHEME_PRIMP(p)) {
      if (((Scheme_Primitive_Proc *)p)->pp.flags & SCHEME_PRIM_IS_STRUCT_PRED)
	return 1;
      else if (((Scheme_Primitive_Proc *)p)->pp.flags & SCHEME_PRIM_IS_STRUCT_INDEXED_GETTER)
	return 2;
    }
  }
  return 0;
}

static int inlined_unary_prim(Scheme_Object *o, Scheme_Object *_app, mz_jit_state *jitter)
{
  if (SCHEME_PRIMP(o)
      && (SCHEME_PRIM_PROC_FLAGS(o) & SCHEME_PRIM_IS_UNARY_INLINED))
    return 1;

  if (inlineable_struct_prim(o, jitter))
    return 1;

  return 0;
}

static int inlined_binary_prim(Scheme_Object *o, Scheme_Object *_app)
{
  return (SCHEME_PRIMP(o)
	  && (SCHEME_PRIM_PROC_FLAGS(o) & SCHEME_PRIM_IS_BINARY_INLINED));
}

static int inlined_nary_prim(Scheme_Object *o, Scheme_Object *_app)
{
  return (SCHEME_PRIMP(o)
	  && (SCHEME_PRIM_PROC_FLAGS(o) & SCHEME_PRIM_IS_MIN_NARY_INLINED)
	  && (((Scheme_App_Rec *)_app)->num_args == ((Scheme_Primitive_Proc *)o)->mina));
}

static int is_noncm(Scheme_Object *a, mz_jit_state *jitter, int depth, int stack_start)
{
  if (SCHEME_PRIMP(a)) {
    if (((Scheme_Prim_Proc_Header *)a)->flags & SCHEME_PRIM_IS_NONCM)
      return 1;
  }

  if (depth 
      && jitter->nc 
      && SAME_TYPE(SCHEME_TYPE(a), scheme_toplevel_type)
      && (SCHEME_TOPLEVEL_FLAGS(a) & SCHEME_TOPLEVEL_CONST)) {
    Scheme_Object *p;
    p = extract_global(a, jitter->nc);
    p = ((Scheme_Bucket *)p)->val;
    if (p && SAME_TYPE(SCHEME_TYPE(p), scheme_native_closure_type)) {
      Scheme_Native_Closure_Data *ndata = ((Scheme_Native_Closure *)p)->code;
      if (ndata->closure_size >= 0) { /* not case-lambda */
        if (lambda_has_been_jitted(ndata)) {
          if (SCHEME_NATIVE_CLOSURE_DATA_FLAGS(ndata) & NATIVE_PRESERVES_MARKS)
            return 1;
        } else {
          if (SCHEME_CLOSURE_DATA_FLAGS(ndata->u2.orig_code) & CLOS_PRESERVES_MARKS)
            return 1;
        }
      }
    }
  }

  if (SAME_TYPE(SCHEME_TYPE(a), scheme_local_type)) {
    int pos = SCHEME_LOCAL_POS(a) - stack_start;
    if (pos >= 0) {
      int flags;
      if (mz_is_closure(jitter, pos, -1, &flags)) {
        return (flags & NATIVE_PRESERVES_MARKS);
      }
    }
  }

  if (depth && SAME_TYPE(SCHEME_TYPE(a), scheme_closure_type)) {
    Scheme_Closure_Data *data;

    data = ((Scheme_Closure *)a)->code;
    if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_PRESERVES_MARKS)
      return 1;
  }

  return 0;
}

#define INIT_SIMPLE_DEPTH 10

static int is_simple(Scheme_Object *obj, int depth, int just_markless, mz_jit_state *jitter, int stack_start)
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
      return (t == CASE_LAMBDA_EXPD);
    }
    break;

  case scheme_branch_type:
    if (depth) {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)obj;
      return (is_simple(b->tbranch, depth - 1, just_markless, jitter, stack_start)
	      && is_simple(b->fbranch, depth - 1, just_markless, jitter, stack_start));
    }
    break;
    
  case scheme_let_value_type:
    if (depth) {
      return is_simple(((Scheme_Let_Value *)obj)->body, depth - 1, just_markless, jitter, stack_start);
    }
    break;
  case scheme_let_one_type:
    if (just_markless && depth) {
      return is_simple(((Scheme_Let_One *)obj)->body, depth - 1, just_markless, jitter, stack_start + 1);
    }
    break;
  case scheme_let_void_type:
    if (just_markless && depth) {
      return is_simple(((Scheme_Let_Void *)obj)->body, depth - 1, just_markless, jitter,
                       stack_start + ((Scheme_Let_Void *)obj)->count);
    }
    break;
  case scheme_letrec_type:
    if (just_markless && depth) {
      return is_simple(((Scheme_Letrec *)obj)->body, depth - 1, just_markless, jitter,
                       stack_start + ((Scheme_Letrec *)obj)->count);
    }
    break;

  case scheme_application_type:
    if (inlined_nary_prim(((Scheme_App_Rec *)obj)->args[0], obj))
      return 1;
    if (just_markless) {
      return is_noncm(((Scheme_App_Rec *)obj)->args[0], jitter, depth, 
                      stack_start + ((Scheme_App_Rec *)obj)->num_args);
    }
    break;
  case scheme_application2_type:
    if (inlined_unary_prim(((Scheme_App2_Rec *)obj)->rator, obj, jitter))
      return 1;
    else if (just_markless) {
      return is_noncm(((Scheme_App2_Rec *)obj)->rator, jitter, depth, stack_start + 1);
    }
    break;
  case scheme_application3_type:
    if (inlined_binary_prim(((Scheme_App2_Rec *)obj)->rator, obj))
      return 1;
    else if (just_markless) {
      return is_noncm(((Scheme_App3_Rec *)obj)->rator, jitter, depth, stack_start + 2);
    }
    break;
    
  case scheme_toplevel_type:
  case scheme_quote_syntax_type:
  case scheme_local_type:
  case scheme_local_unbox_type:
  case scheme_unclosed_procedure_type:
    return 1;
    break;
  }

  return (type > _scheme_values_types_);
}

static int is_constant_and_avoids_r1(Scheme_Object *obj)
{
  Scheme_Type t = SCHEME_TYPE(obj);

  if (SAME_TYPE(t, scheme_toplevel_type)) {
    return ((SCHEME_TOPLEVEL_FLAGS(obj) & SCHEME_TOPLEVEL_CONST)
            ? 1
            : 0);
  } else if (SAME_TYPE(t, scheme_local_type)) {
    return 1;
  } else
    return (t >= _scheme_compiled_values_types_);
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
    CHECK_RUNSTACK_OVERFLOW();
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    JIT_UPDATE_THREAD_RSPTR();
  }
  jit_movi_i(JIT_R1, num_rands);
  mz_prepare(2); /* a prim takes 3 args, but a NONCM prim ignores the 3rd */
  CHECK_LIMIT();
  jit_pusharg_p(JIT_RUNSTACK);
  jit_pusharg_i(JIT_R1);
  mz_finishr(JIT_V1);
  CHECK_LIMIT();
  /* Pop saved runstack val and return: */
  mz_get_local_p(JIT_NOT_RET, JIT_LOCAL1);
  jit_sti_p(&scheme_current_runstack, JIT_NOT_RET);
  mz_pop_locals();
  jit_ret();

  return 1;
}

static int generate_tail_call(mz_jit_state *jitter, int num_rands, int direct_native, int need_set_rs)
/* If num_rands < 0, then argc is in LOCAL2 and arguments are already below RUNSTACK_BASE.
   If direct_native == 2, then some arguments are already in place (shallower in the runstack
   than the arguments to move). */
{
  int i;
  GC_CAN_IGNORE jit_insn *ref, *ref2, *ref4, *ref5;

  __START_SHORT_JUMPS__(num_rands < 100);

  /* First, try fast direct jump to native code: */
  if (!direct_native) {
    ref = jit_bmsi_ul(jit_forward(), JIT_V1, 0x1);
    jit_ldr_s(JIT_R1, JIT_V1);
    ref2 = jit_bnei_i(jit_forward(), JIT_R1, scheme_native_closure_type);
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
     At this point, V1 = closure and R0 = code. */

  /* Check for thread swap: */
  (void)jit_movi_p(JIT_R1, &scheme_fuel_counter);
  jit_ldr_i(JIT_R2, JIT_R1);
  ref5 = jit_blei_i(jit_forward(), JIT_R2, 0);
#ifndef FUEL_AUTODECEREMENTS
  jit_subi_p(JIT_R2, JIT_R2, 0x1);
  jit_str_i(JIT_R1, JIT_R2);
#endif
  CHECK_LIMIT();

  /* Copy args to runstack base: */
  if (num_rands >= 0) {
    /* Fixed argc: */
    if (num_rands) {
      jit_subi_p(JIT_R2, JIT_RUNSTACK_BASE, WORDS_TO_BYTES(num_rands)); 
      CHECK_RUNSTACK_OVERFLOW();
      for (i = num_rands; i--; ) {
        jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(i));
        jit_stxi_p(WORDS_TO_BYTES(i), JIT_R2, JIT_R1);
        CHECK_LIMIT();
      }
      jit_movr_p(JIT_RUNSTACK, JIT_R2);
    } else {
      jit_movr_p(JIT_RUNSTACK, JIT_RUNSTACK_BASE);
    }
    if (direct_native > 1) { /* => some_args_already_in_place */
      mz_get_local_p(JIT_R1, JIT_LOCAL2);
      jit_lshi_l(JIT_R1, JIT_R1, JIT_LOG_WORD_SIZE);
      jit_subr_p(JIT_RUNSTACK, JIT_RUNSTACK, JIT_R1);
      CHECK_RUNSTACK_OVERFLOW();
    }
  } else {
    /* Variable argc (in LOCAL2):
       arguments are already in place. */
  }
  /* RUNSTACK, RUNSTACK_BASE, V1, and R0 are ready */
  
  /* Extract function and data: */
  jit_movr_p(JIT_R2, JIT_V1);
  if (direct_native) {
    jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure_Data *)0x0)->u.tail_code);
  } else {
    jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure_Data *)0x0)->arity_code);
  }
  /* Set up arguments; JIT_RUNSTACK and JIT_RUNSTACK_BASE must also be ready */
  jit_movr_p(JIT_R0, JIT_R2);
  if (num_rands >= 0) {
    jit_movi_i(JIT_R1, num_rands);
    if (direct_native > 1) { /* => some_args_already_in_place */
      mz_get_local_p(JIT_R2, JIT_LOCAL2);
      jit_addr_i(JIT_R1, JIT_R1, JIT_R2);
    }
  } else {
    mz_get_local_p(JIT_R1, JIT_LOCAL2);    
  }
  jit_movr_p(JIT_R2, JIT_RUNSTACK);
  /* Now jump: */
  jit_jmpr(JIT_V1);
  CHECK_LIMIT();

  /* The slow way: */
  /*  V1 and RUNSTACK must be intact! */
  if (!direct_native) {
    mz_patch_branch(ref);
    mz_patch_branch(ref2);
  }
  mz_patch_branch(ref4);
  mz_patch_branch(ref5);
  CHECK_LIMIT();
  if (need_set_rs) {
    JIT_UPDATE_THREAD_RSPTR();
  }
  if (direct_native > 1) { /* => some_args_already_in_place */
    /* Need to shuffle argument lists. Since we can pass only
       three arguments, use static variables for the others. */
    jit_sti_p(&fixup_runstack_base, JIT_RUNSTACK_BASE);
    mz_get_local_p(JIT_R1, JIT_LOCAL2);
    jit_sti_l(&fixup_already_in_place, JIT_R1);
  }
  if (num_rands >= 0) {
    jit_movi_i(JIT_R0, num_rands);
  } else {
    mz_get_local_p(JIT_R0, JIT_LOCAL2);    
  }
  /* Since we've overwritten JIT_RUNSTACK, if this is not shared
     code, and if this is 3m, then the runstack no longer
     has a pointer to the closure for this code. To ensure that
     an appropriate return point exists, jump to static code
     for the rest. (This is the slow path, anyway.) */
   __END_SHORT_JUMPS__(num_rands < 100);
   if (direct_native > 1) {
     (void)jit_jmpi(finish_tail_call_fixup_code);
   } else {
     (void)jit_jmpi(finish_tail_call_code);
   }

   return 1;
}

static int generate_finish_tail_call(mz_jit_state *jitter, int direct_native)
{
  mz_prepare(3);
  CHECK_LIMIT();
  jit_pusharg_p(JIT_RUNSTACK);
  jit_pusharg_i(JIT_R0);
  jit_pusharg_p(JIT_V1);
  if (direct_native > 1) { /* => some_args_already_in_place */
    (void)mz_finish(_scheme_tail_apply_from_native_fixup_args);
  } else {
    (void)mz_finish(_scheme_tail_apply_from_native);
  }
  CHECK_LIMIT();
  /* Pop saved runstack val and return: */
  mz_get_local_p(JIT_NOT_RET, JIT_LOCAL1);
  jit_sti_p(&scheme_current_runstack, JIT_NOT_RET);
  mz_pop_locals();
  jit_ret();

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
    CHECK_RUNSTACK_OVERFLOW();
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    JIT_UPDATE_THREAD_RSPTR();
  }

  jit_movi_i(JIT_R1, num_rands);
  mz_prepare(2); /* a prim takes 3 args, but a NONCM prim ignores the 3rd */
  CHECK_LIMIT();
  jit_pusharg_p(JIT_RUNSTACK);
  jit_pusharg_i(JIT_R1);
  mz_finishr(JIT_V1);
  CHECK_LIMIT();
  jit_retval(JIT_R0);
  VALIDATE_RESULT(JIT_R0);
  /* No need to check for multi values or tail-call, because
     we only use this for noncm primitives. */

  if (num_rands == 1) {
    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    jitter->need_set_rs = 1;
  }

  if (pop_and_jump) {
    mz_epilog(JIT_V1);
  }

  return 1;
}

static int generate_retry_call(mz_jit_state *jitter, int num_rands, int multi_ok, GC_CAN_IGNORE jit_insn *reftop)
  /* If num_rands < 0, original argc is in V1, and we should
     pop argc arguments off runstack before pushing more.
     This function is called with short jumps enabled. */
{
  GC_CAN_IGNORE jit_insn *ref, *ref2, *refloop;

  if (!reftop) {
    reftop = shared_non_tail_retry_code[multi_ok ? 1 : 0];
  }

  /* Get new argc: */
  (void)jit_ldi_p(JIT_R1, &scheme_current_thread);
  jit_ldxi_l(JIT_R2, JIT_R1, &((Scheme_Thread *)0x0)->ku.apply.tail_num_rands);
  if (num_rands >= 0) {
    jit_movi_l(JIT_V1, 0);
  }
  /* Thread is in R1. New argc is in R2. Old argc to cancel is in V1. */

  /* Enough room on runstack? */
  jit_ldi_p(JIT_R0, &MZ_RUNSTACK_START);
  jit_subr_ul(JIT_R0, JIT_RUNSTACK, JIT_R0); /* R0 is space left (in bytes) */
  jit_subr_l(JIT_R2, JIT_R2, JIT_V1);
  jit_lshi_l(JIT_R2, JIT_R2, JIT_LOG_WORD_SIZE);
  ref = jit_bltr_ul(jit_forward(), JIT_R0, JIT_R2);
  CHECK_LIMIT();

  /* Yes, there's enough room. Adjust the runstack. */
  jit_subr_l(JIT_RUNSTACK, JIT_RUNSTACK, JIT_R2);
  CHECK_RUNSTACK_OVERFLOW();

  /* Copy argument to runstack, then jump to reftop. */
  jit_ldxi_l(JIT_R2, JIT_R1, &((Scheme_Thread *)0x0)->ku.apply.tail_num_rands);
  jit_ldxi_l(JIT_V1, JIT_R1, &((Scheme_Thread *)0x0)->ku.apply.tail_rands);
  jit_lshi_l(JIT_R2, JIT_R2, JIT_LOG_WORD_SIZE);
  CHECK_LIMIT();
  refloop = _jit.x.pc;
  ref2 = jit_blei_l(jit_forward(), JIT_R2, 0);
  jit_subi_l(JIT_R2, JIT_R2, JIT_WORD_SIZE);
  jit_ldxr_p(JIT_R0, JIT_V1, JIT_R2);
  jit_stxr_p(JIT_R2, JIT_RUNSTACK, JIT_R0);
  (void)jit_jmpi(refloop);
  CHECK_LIMIT();

  /* R1 is still the thread. 
     Put procedure and argc in place, then jump to apply: */
  mz_patch_branch(ref2);
  jit_ldxi_l(JIT_V1, JIT_R1, &((Scheme_Thread *)0x0)->ku.apply.tail_rator);
  jit_ldxi_l(JIT_R0, JIT_R1, &((Scheme_Thread *)0x0)->ku.apply.tail_num_rands);
  __END_SHORT_JUMPS__(1);
  (void)jit_jmpi(reftop);
  __START_SHORT_JUMPS__(1);
  
  /* Slow path; restore R0 to SCHEME_TAIL_CALL_WAITING */
  mz_patch_branch(ref);
  jit_movi_l(JIT_R0, SCHEME_TAIL_CALL_WAITING);

  return 1;
}

static int generate_non_tail_call(mz_jit_state *jitter, int num_rands, int direct_native, int need_set_rs, 
				  int multi_ok, int pop_and_jump)
{
  /* Non-tail call.
     Proc is in V1, args are at RUNSTACK.
     If num_rands < 0, then argc is in R0, and need to pop runstack before returning.
     If num_rands == -1, skip prolog. */
  GC_CAN_IGNORE jit_insn *ref, *ref2, *ref4, *ref5, *ref6, *ref7, *ref8, *ref9;
  GC_CAN_IGNORE jit_insn *ref10, *ref11, *reftop = NULL;

  __START_SHORT_JUMPS__(1);

  if (pop_and_jump) {
    if (num_rands != -1) {
      mz_prolog(JIT_R1);
    } else {
      reftop = _jit.x.pc;
    }
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
  jit_ldxi_p(JIT_R2, JIT_V1, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_i(JIT_R2, JIT_R2, &((Scheme_Native_Closure_Data *)0x0)->max_let_depth);
  jit_ldi_p(JIT_R1, &MZ_RUNSTACK_START);
  jit_subr_ul(JIT_R1, JIT_RUNSTACK, JIT_R1);
  ref4 = jit_bltr_ul(jit_forward(), JIT_R1, JIT_R2);
  CHECK_LIMIT();

  /* Before inlined native, check stack depth: */
  (void)jit_movi_p(JIT_R1, &scheme_stack_boundary);
  jit_ldr_l(JIT_R1, JIT_R1);
  ref9 = jit_bltr_ul(jit_forward(), JIT_STACK, JIT_R1);
  CHECK_LIMIT();

  /* Finally, check for thread swap: */
  (void)jit_movi_p(JIT_R1, &scheme_fuel_counter);
  jit_ldr_i(JIT_R2, JIT_R1);
  ref11 = jit_blei_i(jit_forward(), JIT_R2, 0);
#ifndef FUEL_AUTODECEREMENTS
  jit_subi_p(JIT_R2, JIT_R2, 0x1);
  jit_str_i(JIT_R1, JIT_R2);
#endif
  
  /* Fast inlined-native jump ok (proc will check argc, if necessary) */
  {
    jit_insn *refr;
    if (num_rands < 0) {
      /* We need to save argc to manually pop the
         runstack. So move V1 to R2 and move R0 to V1: */
      jit_movr_p(JIT_R2, JIT_V1);
      jit_movr_p(JIT_V1, JIT_R0);
    }
    refr = jit_patchable_movi_p(JIT_R1, jit_forward());
    jit_shuffle_saved_regs(); /* maybe copies V regsiters to be restored */
    _jit_prolog_again(jitter, 3, JIT_R1); /* saves V registers (or copied V registers) */
    if (num_rands >= 0) {
      jit_movr_p(JIT_R0, JIT_V1); /* closure */
      jit_movi_i(JIT_R1, num_rands); /* argc */
      jit_movr_p(JIT_R2, JIT_RUNSTACK); /* argv */
      jit_addi_p(JIT_RUNSTACK_BASE, JIT_RUNSTACK, WORDS_TO_BYTES(num_rands));
    } else {
      /* R2 is closure, V1 is argc */
      jit_lshi_l(JIT_R1, JIT_V1, JIT_LOG_WORD_SIZE);
      jit_addr_p(JIT_RUNSTACK_BASE, JIT_RUNSTACK, JIT_R1);
      jit_movr_p(JIT_R0, JIT_R2); /* closure */
      jit_movr_i(JIT_R1, JIT_V1); /* argc */
      jit_movr_p(JIT_R2, JIT_RUNSTACK); /* argv */
    }
    CHECK_LIMIT();
    mz_push_locals();
    mz_set_local_p(JIT_RUNSTACK, JIT_LOCAL1);
    jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
    if (direct_native) {
      jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->u.tail_code);
    } else {
      jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->arity_code);
    }
    jit_jmpr(JIT_V1); /* callee restores (copied) V registers, etc. */
    jit_patch_movi(refr, (_jit.x.pc));
    jit_unshuffle_saved_regs(); /* maybe uncopies V registers */
    /* If num_rands < 0, then V1 has argc */
  }
  CHECK_LIMIT();
  jit_retval(JIT_R0);
  VALIDATE_RESULT(JIT_R0);
  if (!multi_ok) {
    jit_insn *refm;
    __END_SHORT_JUMPS__(1);
    refm = jit_beqi_p(jit_forward(), JIT_R0, SCHEME_MULTIPLE_VALUES);
    mz_patch_branch_at(refm, bad_result_arity_code);
    __START_SHORT_JUMPS__(1);
  }
  ref6 = jit_bnei_p(jit_forward(), JIT_R0, SCHEME_TAIL_CALL_WAITING);
  if (pop_and_jump) {
    /* Expects argc in V1 if num_rands < 0: */
    generate_retry_call(jitter, num_rands, multi_ok, reftop);
  }
  CHECK_LIMIT();
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
    jit_ldxi_i(JIT_R2, JIT_V1, &((Scheme_Primitive_Proc *)0x0)->mina);
    if (num_rands >= 0) {
      ref7 = jit_bnei_i(jit_forward(), JIT_R2, num_rands);
    } else {
      ref7 = jit_bner_i(jit_forward(), JIT_R2, JIT_R0);
    }
    /* Fast prim application */
    jit_ldxi_p(JIT_R1, JIT_V1, &((Scheme_Primitive_Proc *)0x0)->prim_val);
    if (need_set_rs) {
      JIT_UPDATE_THREAD_RSPTR();
    }
    mz_prepare(3);
    jit_pusharg_p(JIT_V1);
    if (num_rands < 0) { jit_movr_p(JIT_V1, JIT_R0); } /* save argc to manually pop runstack */
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_R2);
    (void)mz_finishr(JIT_R1);
    CHECK_LIMIT();
    jit_retval(JIT_R0);
    VALIDATE_RESULT(JIT_R0);
    if (!multi_ok) {
      jit_insn *refm;
      __END_SHORT_JUMPS__(1);
      refm = jit_beqi_p(jit_forward(), JIT_R0, SCHEME_MULTIPLE_VALUES);
      mz_patch_branch_at(refm, bad_result_arity_code);
      __START_SHORT_JUMPS__(1);
    }
    ref10 = jit_bnei_p(jit_forward(), JIT_R0, SCHEME_TAIL_CALL_WAITING);
    if (pop_and_jump) {
      /* Expects argc in V1 if num_rands < 0: */
      generate_retry_call(jitter, num_rands, multi_ok, reftop);
    }
    CHECK_LIMIT();
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
  mz_patch_branch(ref11);
  if (need_set_rs) {
    JIT_UPDATE_THREAD_RSPTR();
  }
  if (num_rands >= 0) {
    jit_movi_i(JIT_R0, num_rands);
  }
  mz_prepare(3);
  CHECK_LIMIT();
  jit_pusharg_p(JIT_RUNSTACK);
  jit_pusharg_i(JIT_R0);
  jit_pusharg_p(JIT_V1);
  if (num_rands < 0) { jit_movr_p(JIT_V1, JIT_R0); } /* save argc to manually pop runstack */
  if (multi_ok) {
    (void)mz_finish(_scheme_apply_multi_from_native);
  } else {
    (void)mz_finish(_scheme_apply_from_native);
  }
  CHECK_LIMIT();
  mz_patch_ucbranch(ref5);
  if (!direct_native) {
    mz_patch_ucbranch(ref8);
  }
  jit_retval(JIT_R0);
  VALIDATE_RESULT(JIT_R0);
  mz_patch_branch(ref6);
  if (!direct_native) {
    mz_patch_branch(ref10);
  }
  if (num_rands < 0) { 
    /* At this point, argc must be in V1 */
    jit_lshi_l(JIT_R1, JIT_V1, JIT_LOG_WORD_SIZE);
    jit_addr_p(JIT_RUNSTACK, JIT_RUNSTACK, JIT_R1);
  }
  if (pop_and_jump) {
    mz_epilog(JIT_V1);
  }
  CHECK_LIMIT();

  __END_SHORT_JUMPS__(1);

  return 1;
}

static int generate_self_tail_call(Scheme_Object *rator, mz_jit_state *jitter, int num_rands, jit_insn *slow_code,
                                   int args_already_in_place)
{
  jit_insn *refslow;
  int i;
  int closure_size = jitter->self_closure_size;

  /* Last argument is in R0 */

#ifdef JIT_PRECISE_GC
  closure_size += 1; /* Skip procedure pointer, too */
#endif

  __START_SHORT_JUMPS__(1);

  /* Check for thread swap: */
  (void)jit_movi_p(JIT_R1, &scheme_fuel_counter);
  jit_ldr_i(JIT_R2, JIT_R1);
  refslow = jit_blei_i(jit_forward(), JIT_R2, 0);
#ifndef FUEL_AUTODECEREMENTS
  jit_subi_p(JIT_R2, JIT_R2, 0x1);
  jit_str_i(JIT_R1, JIT_R2);
#endif

  __END_SHORT_JUMPS__(1);

  /* Copy args to runstack after closure data: */
  jit_subi_p(JIT_R2, JIT_RUNSTACK_BASE, WORDS_TO_BYTES(num_rands + closure_size + args_already_in_place)); 
  if (num_rands) {
    jit_stxi_p(WORDS_TO_BYTES(num_rands - 1 + closure_size + args_already_in_place), JIT_R2, JIT_R0);
    for (i = num_rands - 1; i--; ) {
      jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(i));
      jit_stxi_p(WORDS_TO_BYTES(i + closure_size + args_already_in_place), JIT_R2, JIT_R1);
      CHECK_LIMIT();
    }
  }
  jit_movr_p(JIT_RUNSTACK, JIT_R2);

  /* Now jump: */
  (void)jit_jmpi(jitter->self_restart_code);
  CHECK_LIMIT();

  /* Slow path: */
  __START_SHORT_JUMPS__(1);
  mz_patch_branch(refslow);
  __END_SHORT_JUMPS__(1);

  if (args_already_in_place) {
    jit_movi_l(JIT_R2, args_already_in_place);
    mz_set_local_p(JIT_R2, JIT_LOCAL2);
  }

  jit_stxi_p(WORDS_TO_BYTES(num_rands - 1), JIT_RUNSTACK, JIT_R0);
  generate(rator, jitter, 0, 0);
  CHECK_LIMIT();
  jit_movr_p(JIT_V1, JIT_R0);

  (void)jit_jmpi(slow_code);

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

  return generate_one(old_jitter, do_generate_shared_call, &data, 0, NULL, NULL);
}

static void ensure_retry_available(mz_jit_state *jitter, int multi_ok)
{
  int mo = multi_ok ? 1 : 0;
  if (!shared_non_tail_retry_code[mo]) {
    void *code;
    code = generate_shared_call(-1, jitter, multi_ok, 0, 0, 0);
    shared_non_tail_retry_code[mo] = code;
  }
}

static int is_a_procedure(Scheme_Object *v, mz_jit_state *jitter)
{
  Scheme_Type t;

  if (SCHEME_PROCP(v))
    return 1;

  t = SCHEME_TYPE(v);
  if (SAME_TYPE(t, scheme_closure_type)
      || SAME_TYPE(t, scheme_unclosed_procedure_type))
    return 1;
  else if (SAME_TYPE(t, scheme_syntax_type)) {
    return (SCHEME_PINT_VAL(v) == CASE_LAMBDA_EXPD);
  } else if (SAME_TYPE(t, scheme_local_type)) {
    int flags;
    return mz_is_closure(jitter, SCHEME_LOCAL_POS(v), -1, &flags);
  } else if (t == scheme_toplevel_type) {
    if (SCHEME_TOPLEVEL_FLAGS(v) & SCHEME_TOPLEVEL_CONST) {
      if (jitter->nc) {
	Scheme_Object *p;
        
	p = extract_global(v, jitter->nc);
	p = ((Scheme_Bucket *)p)->val;
	return SAME_TYPE(SCHEME_TYPE(p), scheme_native_closure_type);
      }
    }
  }

  return 0;
}

static int generate_app(Scheme_App_Rec *app, Scheme_Object **alt_rands, int num_rands, 
			mz_jit_state *jitter, int is_tail, int multi_ok)
{
  int i, offset;
  int direct_prim = 0, need_non_tail = 0, direct_native = 0, direct_self = 0;
  int proc_already_in_place = 0;
  Scheme_Object *rator, *v;
  int reorder_ok = 0;
  int args_already_in_place = 0;
  START_JIT_DATA();

  rator = (alt_rands ? alt_rands[0] : app->args[0]);

  if (SCHEME_PRIMP(rator)) {
    if ((num_rands >= ((Scheme_Primitive_Proc *)rator)->mina)
	&& ((num_rands <= ((Scheme_Primitive_Proc *)rator)->mu.maxa)
	    || (((Scheme_Primitive_Proc *)rator)->mina < 0))
	&& (is_noncm(rator, jitter, 0, 0)
            /* It's also ok to directly call `values' if multiple values are ok: */
            || (multi_ok && SAME_OBJ(rator, scheme_values_func))))
      direct_prim = 1;
  } else {
    Scheme_Type t;
    t = SCHEME_TYPE(rator);
    if (t == scheme_local_type) {
      /* We can re-order evaluation of the rator. */
      reorder_ok = 1;

      /* Call to known native, or even known self? */
      {
	int pos, flags;
	pos = SCHEME_LOCAL_POS(rator) - num_rands;
	if (mz_is_closure(jitter, pos, num_rands, &flags)) {
	  direct_native = 1;
	  if (is_tail
	      && (pos == jitter->self_pos)
	      && (num_rands < MAX_SHARED_CALL_RANDS)) {
	    direct_self = 1;
	  }
	}
      }
    } else if ((t == scheme_toplevel_type)
	       && (SCHEME_TOPLEVEL_FLAGS(rator) & SCHEME_TOPLEVEL_CONST)) {
      /* We can re-order evaluation of the rator. */
      reorder_ok = 1;

      if (jitter->nc) {
	Scheme_Object *p;

	p = extract_global(rator, jitter->nc);
	p = ((Scheme_Bucket *)p)->val;
	if (SAME_TYPE(SCHEME_TYPE(p), scheme_native_closure_type)) {
	  if (scheme_native_arity_check(p, num_rands)
	      /* If it also accepts num_rands + 1, then it has a vararg,
		 so don't try direct_native. */
	      && !scheme_native_arity_check(p, num_rands + 1)) {
	    direct_native = 1;

	    if (is_tail
		&& (SCHEME_TOPLEVEL_POS(rator) == jitter->self_toplevel_pos)
		&& (num_rands < MAX_SHARED_CALL_RANDS)) {
	      direct_self = 1;
	    }
	  }
	}
      }
    } else if (SAME_TYPE(t, scheme_closure_type)) {
      Scheme_Closure_Data *data;
      data = ((Scheme_Closure *)rator)->code;
      if ((data->num_params == num_rands)
          && !(SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)) {
        direct_native = 1;

        if (is_tail 
            && SAME_OBJ(data->u.jit_clone, jitter->self_data)
            && (num_rands < MAX_SHARED_CALL_RANDS))
          direct_self = 1;
      }
      reorder_ok = 1;
    } else if (t > _scheme_values_types_) {
      /* We can re-order evaluation of the rator. */
      reorder_ok = 1;
    }

    if (direct_self)
      reorder_ok = 0; /* superceded by direct_self */
  }

  /* Direct native tail with same number of args as just received? */
  if (direct_native && is_tail && num_rands
      && (num_rands == jitter->self_data->num_params)
      && !(SCHEME_CLOSURE_DATA_FLAGS(jitter->self_data) & CLOS_HAS_REST)) {
    /* Check whether the actual arguments refer to Scheme-stack 
       locations that will be filled with argument values; that
       is, check how many arguments are already in place for
       the call. */
    mz_runstack_skipped(jitter, num_rands);
    for (i = 0; i < num_rands; i++) {
      v = (alt_rands ? alt_rands[i+1] : app->args[i+1]);
      if (SAME_TYPE(SCHEME_TYPE(v), scheme_local_type)) {
        int pos;
        pos = mz_remap(SCHEME_LOCAL_POS(v));
        if (pos == (jitter->depth + args_already_in_place))
          args_already_in_place++;
        else
          break;
      } else
        break;
    }
    mz_runstack_unskipped(jitter, num_rands);
    if (args_already_in_place) {
      direct_native = 2;
      mz_runstack_skipped(jitter, args_already_in_place);
      num_rands -= args_already_in_place;
    }
  }

  if (num_rands) {
    if (!direct_prim || (num_rands > 1)) {
      jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(num_rands));
      CHECK_RUNSTACK_OVERFLOW();
      mz_runstack_pushed(jitter, num_rands);
    } else {
      mz_runstack_skipped(jitter, 1);
    }
  }

  for (i = num_rands + args_already_in_place + 1; i--; ) {
    v = (alt_rands ? alt_rands[i] : app->args[i]);
    if (!is_simple(v, INIT_SIMPLE_DEPTH, 1, jitter, 0)) {
      need_non_tail = 1;
      break;
    }
  }

  if (need_non_tail) {
    offset = generate_non_tail_mark_pos_prefix(jitter);
    CHECK_LIMIT();
  } else
    offset = 0;

  if (!direct_prim && !reorder_ok && !direct_self) {
    generate_non_tail(rator, jitter, 0, !need_non_tail);
    CHECK_LIMIT();

    if (num_rands) {
      /* Save rator where GC can see it */
      Scheme_Type t;
      t = SCHEME_TYPE((alt_rands 
                       ? alt_rands[1+args_already_in_place] 
                       : app->args[1+args_already_in_place]));
      if ((num_rands == 1) && (SAME_TYPE(scheme_local_type, t)
			       || (t >= _scheme_values_types_))) {
	/* App of something complex to a local variable. We
	   can move the proc directly to V1. */
	jit_movr_p(JIT_V1, JIT_R0);
	proc_already_in_place = 1;
      } else
	jit_stxi_p(WORDS_TO_BYTES(num_rands - 1 + offset), JIT_RUNSTACK, JIT_R0);
    } else {
      jit_movr_p(JIT_V1, JIT_R0);
    }
  }

  for (i = 0; i < num_rands; i++) {
    PAUSE_JIT_DATA();
    generate_non_tail((alt_rands 
                       ? alt_rands[i+1+args_already_in_place] 
                       : app->args[i+1+args_already_in_place]), 
                      jitter, 0, !need_non_tail);
    RESUME_JIT_DATA();
    CHECK_LIMIT();
    if ((i == num_rands - 1) && !direct_prim && !reorder_ok && !direct_self && !proc_already_in_place) {
      /* Move rator back to register: */
      jit_ldxi_p(JIT_V1, JIT_RUNSTACK, WORDS_TO_BYTES(i + offset));
    }
    if ((!direct_prim || (num_rands > 1))
	&& (!direct_self || (i + 1 < num_rands))) {
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

  if (num_rands >= MAX_SHARED_CALL_RANDS) {
    LOG_IT(("<-many args\n"));
    if (is_tail) {
      if (direct_prim) {
        generate_direct_prim_tail_call(jitter, num_rands);
      } else {
        if (args_already_in_place) {
          jit_movi_l(JIT_R2, args_already_in_place);
          mz_set_local_p(JIT_R2, JIT_LOCAL2);
        }
	generate_tail_call(jitter, num_rands, direct_native, jitter->need_set_rs);
      }
    } else {
      if (direct_prim)
	generate_direct_prim_non_tail_call(jitter, num_rands, multi_ok, 0);
      else
	generate_non_tail_call(jitter, num_rands, direct_native, jitter->need_set_rs, multi_ok, 0);
    }
  } else {
    /* Jump to code to implement a tail call for num_rands arguments */
    void *code;
    int dp = (direct_prim ? 1 : (direct_native ? (1 + direct_native) : 0));
    if (is_tail) {
      if (!shared_tail_code[dp][num_rands]) {
	code = generate_shared_call(num_rands, jitter, multi_ok, is_tail, direct_prim, direct_native);
	shared_tail_code[dp][num_rands] = code;
      }
      code = shared_tail_code[dp][num_rands];
      if (direct_self) {
        LOG_IT(("<-self\n"));
	generate_self_tail_call(rator, jitter, num_rands, code, args_already_in_place);
	CHECK_LIMIT();
      } else {
        LOG_IT(("<-tail\n"));
        if (args_already_in_place) {
          jit_movi_l(JIT_R2, args_already_in_place);
          mz_set_local_p(JIT_R2, JIT_LOCAL2);
        }
	(void)jit_jmpi(code);
      }
    } else {
      int mo = (multi_ok ? 1 : 0);

      if (!shared_non_tail_code[dp][num_rands][mo]) {
        ensure_retry_available(jitter, multi_ok);
	code = generate_shared_call(num_rands, jitter, multi_ok, is_tail, direct_prim, direct_native);
	shared_non_tail_code[dp][num_rands][mo] = code;
      }
      LOG_IT(("<-non-tail %d %d %d\n", dp, num_rands, mo));
      code = shared_non_tail_code[dp][num_rands][mo];

      (void)jit_calli(code);

      /* Whether we call a prim, a native, or something else,
	 scheme_current_runstack is up-to-date --- unless
         it was a direct-prim call with 1 argument. */
      if (direct_prim && (num_rands == 1))
        jitter->need_set_rs = 1;
      else
        jitter->need_set_rs = 0;
    }
  }

  END_JIT_DATA(need_non_tail ? 22 : 4);
    
  return is_tail ? 2 : 1;
}

static jit_insn *generate_arith_slow_path(mz_jit_state *jitter, Scheme_Object *rator, 
					  jit_insn **_ref, jit_insn **_ref4,
					  jit_insn **for_branch, 
					  int orig_args, int reversed, int arith, int use_v, int v)
{
  jit_insn *ref, *ref4, *refslow;

  refslow = _jit.x.pc;

  (void)jit_movi_p(JIT_R2, ((Scheme_Primitive_Proc *)rator)->prim_val);
  if (for_branch) {
    ref4 = jit_patchable_movi_p(JIT_V1, jit_forward());
    mz_set_local_p(JIT_V1, JIT_LOCAL2);
  } else
    ref4 = NULL;
  ref = jit_patchable_movi_p(JIT_V1, jit_forward());

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

  if (arith == 6) {
    /* Add tag back to first arg, just in case. See arithmetic-shift branch to refslow. */
    ref = _jit.x.pc;

    if (reversed || use_v) {
      jit_ori_l(JIT_R0, JIT_R0, 0x1);
    } else {
      jit_ori_l(JIT_R1, JIT_R1, 0x1);
    }

    __START_SHORT_JUMPS__(1);
    (void)jit_jmpi(refslow);
    __END_SHORT_JUMPS__(1);

    return ref;
  } else {
    return refslow;
  }
}

#ifdef SIXTY_FOUR_BIT_INTEGERS
# define SCHEME_INT_SMALL_ENOUGH(rand2) ((((long)rand2 & 0xFFFFFFFF) == (long)rand2) || (((long)rand2 & 0xFFFFFFFFF0000000) == 0xFFFFFFFFF0000000))
#else
# define SCHEME_INT_SMALL_ENOUGH(rand2) 1
#endif

static int generate_arith(mz_jit_state *jitter, Scheme_Object *rator, Scheme_Object *rand, Scheme_Object *rand2, 
			  int orig_args, int arith, int cmp, int v, jit_insn **for_branch, int branch_short)
{
  GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3, *ref4, *refslow;
  int skipped, simple_rand, reversed = 0;

  LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)rator)->name));

  if (rand2) {
    if (SCHEME_INTP(rand2)
	&& SCHEME_INT_SMALL_ENOUGH(rand2)
	&& ((arith != 6)
	    || ((SCHEME_INT_VAL(rand2) <= MAX_TRY_SHIFT)
		&& (SCHEME_INT_VAL(rand2) >= -MAX_TRY_SHIFT)))) {
      /* Second is constant, so use constant mode.
	 For arithmetic shift, only do this if the constant
	 is in range. */
      v = SCHEME_INT_VAL(rand2);
      rand2 = NULL;
    } else if (SCHEME_INTP(rand)
	       && SCHEME_INT_SMALL_ENOUGH(rand)
	       && (arith != 6)) {
      /* First is constant; swap argument order and use constant mode. */
      v = SCHEME_INT_VAL(rand);
      cmp = -cmp;
      rand = rand2;
      rand2 = NULL;
      reversed = 1;
    } else if ((SAME_TYPE(SCHEME_TYPE(rand2), scheme_local_type)
		|| SCHEME_INTP(rand2))
	       && !(SAME_TYPE(SCHEME_TYPE(rand), scheme_local_type)
		    || SCHEME_INTP(rand))) {
      /* Second expression is side-effect-free, unlike the first; 
	 swap order and use the fast path for when the first arg is
	 side-effect free. */
      Scheme_Object *t = rand2;
      rand2 = rand;
      rand = t;
      cmp = -cmp;
      reversed = 1;
    }
  }

  if (rand2) {
    simple_rand = (SAME_TYPE(SCHEME_TYPE(rand), scheme_local_type)
		   || SCHEME_INTP(rand));
  } else
    simple_rand = 0;

  if (rand2 && !simple_rand)
    skipped = orig_args - 1;    
  else
    skipped = orig_args;

  mz_runstack_skipped(jitter, skipped);

  if (rand2 && !simple_rand) {
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    CHECK_RUNSTACK_OVERFLOW();
    mz_runstack_pushed(jitter, 1);
    generate_non_tail(rand, jitter, 0, 1);
    CHECK_LIMIT();
    jit_str_p(JIT_RUNSTACK, JIT_R0);
  }

  generate_non_tail(rand2 ? rand2 : rand, jitter, 0, 1);
  CHECK_LIMIT();

  if (simple_rand) {
    int pos, va;

    if (SCHEME_INTP(rand)) {
      (void)jit_movi_p(JIT_R1, rand);
      va = JIT_R0;
    } else {
      pos = mz_remap(SCHEME_LOCAL_POS(rand));
      jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
      jit_andr_ul(JIT_R2, JIT_R0, JIT_R1);
      va = JIT_R2;
    }

    __START_SHORT_JUMPS__(1);
    ref2 = jit_bmsi_ul(jit_forward(), va, 0x1);
    __END_SHORT_JUMPS__(1);

    /* Slow path */
    refslow = generate_arith_slow_path(jitter, rator, &ref, &ref4, for_branch, orig_args, reversed, arith, 0, 0);

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
    refslow = generate_arith_slow_path(jitter, rator, &ref, &ref4, for_branch, orig_args, reversed, arith, 0, 0);

    __START_SHORT_JUMPS__(1);
    mz_patch_branch(ref2);
    __END_SHORT_JUMPS__(1);
    CHECK_LIMIT();
  } else {
    __START_SHORT_JUMPS__(1);
    ref2 = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
    __END_SHORT_JUMPS__(1);

    /* Slow path */
    refslow = generate_arith_slow_path(jitter, rator, &ref, &ref4, for_branch, orig_args, reversed, arith, 1, v);

    __START_SHORT_JUMPS__(1);
    mz_patch_branch(ref2);
    __END_SHORT_JUMPS__(1);
  }

  CHECK_LIMIT();

  mz_runstack_unskipped(jitter, skipped);

  __START_SHORT_JUMPS__(branch_short);

  if (arith) {
    if (rand2) {
      /* First arg is in JIT_R1, second is in JIT_R0 */
      if (arith == 1) {
	jit_andi_ul(JIT_R2, JIT_R1, (~0x1));
	(void)jit_boaddr_l(refslow, JIT_R2, JIT_R0);
	jit_movr_p(JIT_R0, JIT_R2);
      } else if (arith == -1) {
	if (reversed) {
	  jit_movr_p(JIT_R2, JIT_R0);
	  (void)jit_bosubr_l(refslow, JIT_R2, JIT_R1);
	} else {
	  jit_movr_p(JIT_R2, JIT_R1);
	  (void)jit_bosubr_l(refslow, JIT_R2, JIT_R0);
	}
	jit_ori_ul(JIT_R0, JIT_R2, 0x1);
      } else if (arith == 3) {
	/* and */
	jit_andr_ul(JIT_R0, JIT_R1, JIT_R0);
      } else if (arith == 4) {
	/* ior */
	jit_orr_ul(JIT_R0, JIT_R1, JIT_R0);
      } else if (arith == 5) {
	/* xor */
	jit_andi_ul(JIT_R0, JIT_R0, (~0x1));
	jit_xorr_ul(JIT_R0, JIT_R1, JIT_R0);
      } else if (arith == 6) {
	/* arithmetic-shift 
	   This is a lot of code, but if you're using
	   arihtmetic-shift, then you probably want it. */
	int v1 = (reversed ? JIT_R0 : JIT_R1);
	int v2 = (reversed ? JIT_R1 : JIT_R0);
	jit_insn *refi, *refc;

	refi = jit_bgei_l(refslow, v2, (long)scheme_make_integer(0));

	/* Right shift (always works for a small enough shift) */
	(void)jit_blti_l(refslow, v2, scheme_make_integer(-MAX_TRY_SHIFT));
	jit_notr_l(JIT_V1, v2);
	jit_rshi_l(JIT_V1, JIT_V1, 0x1);
	jit_addi_l(JIT_V1, JIT_V1, 0x1);
#ifdef MZ_USE_JIT_I386
	/* Can't shift from _ECX */
	jit_movr_l(JIT_R2, v1);
	jit_rshr_l(JIT_R2, JIT_R2, JIT_V1);
#else
	jit_rshr_l(JIT_R2, v1, JIT_V1);
#endif
	jit_ori_l(JIT_R0, JIT_R2, 0x1);
	refc = jit_jmpi(jit_forward());

	/* Left shift */
	mz_patch_branch(refi);
	(void)jit_bgti_l(refslow, v2, (long)scheme_make_integer(MAX_TRY_SHIFT));
	jit_rshi_l(JIT_V1, v2, 0x1);
	jit_andi_l(v1, v1, (~0x1));
#ifdef MZ_USE_JIT_I386
	/* Can't shift from _ECX */
	jit_movr_l(JIT_R2, v1);
	jit_lshr_l(JIT_R2, JIT_R2, JIT_V1);
#else
	jit_lshr_l(JIT_R2, v1, JIT_V1);
#endif
	/* If shifting back right produces a different result, that's overflow... */
	jit_rshr_l(JIT_V1, JIT_R2, JIT_V1);
	/* !! In case we go refslow, it nseed to add back tag to v1 !! */
	(void)jit_bner_p(refslow, JIT_V1, v1);
	/* No overflow. */
	jit_ori_l(JIT_R0, JIT_R2, 0x1);

	mz_patch_ucbranch(refc);
      } else if (arith == 9) {
        /* min */
        jit_insn *refc;
        refc = jit_bltr_l(jit_forward(), JIT_R0, JIT_R1);
        jit_movr_l(JIT_R0, JIT_R1);
        mz_patch_branch(refc);
      } else if (arith == 10) {
        /* max */
        jit_insn *refc;
        refc = jit_bgtr_l(jit_forward(), JIT_R0, JIT_R1);
        jit_movr_l(JIT_R0, JIT_R1);
        mz_patch_branch(refc);
      }
    } else {
      /* Non-constant arg is in JIT_R0 */
      if (arith == 1) {
	jit_movr_p(JIT_R2, JIT_R0);
	(void)jit_boaddi_l(refslow, JIT_R2, v << 1);
	jit_movr_p(JIT_R0, JIT_R2);
      } else if (arith == -1) {
	if (reversed) {
	  (void)jit_movi_p(JIT_R2, scheme_make_integer(v));
	  (void)jit_bosubr_l(refslow, JIT_R2, JIT_R0);
	  jit_addi_ul(JIT_R0, JIT_R2, 0x1);
	} else {
	  jit_movr_p(JIT_R2, JIT_R0);
	  (void)jit_bosubi_l(refslow, JIT_R2, v << 1);
	  jit_movr_p(JIT_R0, JIT_R2);
	}
      } else {
	if (arith == 3) {
	  /* and */
	  long l = (long)scheme_make_integer(v);
	  jit_andi_ul(JIT_R0, JIT_R0, l);
	} else if (arith == 4) {
	  /* ior */
	  long l = (long)scheme_make_integer(v);
	  jit_ori_ul(JIT_R0, JIT_R0, l);
	} else if (arith == 5) {
	  /* xor */
	  jit_xori_ul(JIT_R0, JIT_R0, v << 1);
	} else if (arith == 6) {
	  /* arithmetic-shift */
	  /* We only get here when v is between -MAX_TRY_SHIFT and MAX_TRY_SHIFT, inclusive */
	  if (v <= 0) {
	    jit_rshi_l(JIT_R0, JIT_R0, -v);
	    jit_ori_l(JIT_R0, JIT_R0, 0x1);
	  } else {
	    jit_andi_l(JIT_R0, JIT_R0, (~0x1));
	    jit_lshi_l(JIT_R2, JIT_R0, v);
	    /* If shifting back right produces a different result, that's overflow... */
	    jit_rshi_l(JIT_V1, JIT_R2, v);
	    /* !! In case we go refslow, it nseed to add back tag to JIT_R0 !! */
	    (void)jit_bner_p(refslow, JIT_V1, JIT_R0);
	    /* No overflow. */
	    jit_ori_l(JIT_R0, JIT_R2, 0x1);
	  }
	} else if (arith == 7) {
	  jit_notr_ul(JIT_R0, JIT_R0);
	  jit_ori_ul(JIT_R0, JIT_R0, 0x1);
	} else if (arith == 9) {
          /* min */
          jit_insn *refc;
          refc = jit_blti_l(jit_forward(), JIT_R0, (long)scheme_make_integer(v));
          jit_movi_l(JIT_R0, (long)scheme_make_integer(v));
          mz_patch_branch(refc);
        } else if (arith == 10) {
          /* max */
          jit_insn *refc;
          refc = jit_bgti_l(jit_forward(), JIT_R0, (long)scheme_make_integer(v));
          jit_movi_l(JIT_R0, (long)scheme_make_integer(v));
          mz_patch_branch(refc);
        }
      }
    }
    jit_patch_movi(ref, (_jit.x.pc));
  } else {
    /* If second is constant, first arg is in JIT_R0. */
    /* Otherwise, first arg is in JIT_R1, second is in JIT_R0 */
    switch (cmp) {
    case -2:
      if (rand2) {
	ref3 = jit_bger_l(jit_forward(), JIT_R1, JIT_R0);
      } else {
	ref3 = jit_bgei_l(jit_forward(), JIT_R0, (long)scheme_make_integer(v));
      }
      break;
    case -1:
      if (rand2) {
	ref3 = jit_bgtr_l(jit_forward(), JIT_R1, JIT_R0);
      } else {
	ref3 = jit_bgti_l(jit_forward(), JIT_R0, (long)scheme_make_integer(v));
      }
      break;
    case 0:
      if (rand2) {
	ref3 = jit_bner_l(jit_forward(), JIT_R1, JIT_R0);
      } else {
	ref3 = jit_bnei_l(jit_forward(), JIT_R0, (long)scheme_make_integer(v));
      }
      break;
    case 1:
      if (rand2) {
	ref3 = jit_bltr_l(jit_forward(), JIT_R1, JIT_R0);
      } else {
	ref3 = jit_blti_l(jit_forward(), JIT_R0, (long)scheme_make_integer(v));
      }
      break;
    case 2:
    default:
      if (rand2) {
	ref3 = jit_bler_l(jit_forward(), JIT_R1, JIT_R0);
      } else {
	ref3 = jit_blei_l(jit_forward(), JIT_R0, (long)scheme_make_integer(v));
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
					  Scheme_Object *cnst, Scheme_Object *cnst2, 
					  jit_insn **for_branch, int branch_short)
{
  GC_CAN_IGNORE jit_insn *ref, *ref2;

  LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)app->rator)->name));

  mz_runstack_skipped(jitter, 1);

  generate_non_tail(app->rand, jitter, 0, 1);
  CHECK_LIMIT();

  mz_runstack_unskipped(jitter, 1);

  __START_SHORT_JUMPS__(branch_short);

  if (cnst2) {
    ref2 = mz_beqi_p(jit_forward(), JIT_R0, cnst);
    ref = mz_bnei_p(jit_forward(), JIT_R0, cnst2);
    mz_patch_branch(ref2);
  } else {
    ref = mz_bnei_p(jit_forward(), JIT_R0, cnst);
  }

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
				      Scheme_Type lo_ty, Scheme_Type hi_ty, 
				      jit_insn **for_branch, int branch_short)
{
  GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3, *ref4;
  int int_ok;

  int_ok = ((lo_ty <= scheme_integer_type) && (scheme_integer_type <= hi_ty));

  LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)app->rator)->name));

  mz_runstack_skipped(jitter, 1);

  generate_non_tail(app->rand, jitter, 0, 1);
  CHECK_LIMIT();

  mz_runstack_unskipped(jitter, 1);

  __START_SHORT_JUMPS__(branch_short);

  ref = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
  jit_ldxi_s(JIT_R0, JIT_R0, &((Scheme_Object *)0x0)->type);
  if (lo_ty == hi_ty) {
    ref3 = jit_bnei_p(jit_forward(), JIT_R0, lo_ty);
    ref4 = NULL;
  } else {
    ref3 = jit_blti_p(jit_forward(), JIT_R0, lo_ty);
    ref4 = jit_bgti_p(jit_forward(), JIT_R0, hi_ty);
  }
  if (int_ok) {
    mz_patch_branch(ref);
  }
  if (for_branch) {
    if  (!int_ok) {
      for_branch[0] = ref;
    }
    for_branch[1] = ref3;
    for_branch[3] = ref4;
  } else {
    if ((lo_ty <= scheme_integer_type) && (scheme_integer_type <= hi_ty)) {
      mz_patch_branch(ref);
    }
    (void)jit_movi_p(JIT_R0, scheme_true);
    ref2 = jit_jmpi(jit_forward());
    if  (!int_ok) {
      mz_patch_branch(ref);
    }
    mz_patch_branch(ref3);
    if (ref4) {
      mz_patch_branch(ref4);
    }
    (void)jit_movi_p(JIT_R0, scheme_false);
    mz_patch_ucbranch(ref2);
  }

  __END_SHORT_JUMPS__(branch_short);

  return 1;
}

static int generate_inlined_struct_op(int kind, mz_jit_state *jitter, 
				      Scheme_Object *rator, Scheme_Object *rand,
				      jit_insn **for_branch, int branch_short)
{
  mz_runstack_skipped(jitter, 1);

  generate(rator, jitter, 0, 0);
  CHECK_LIMIT();

  if (SAME_TYPE(scheme_local_type, SCHEME_TYPE(rand))) {
    jit_movr_p(JIT_R1, JIT_R0);
    generate(rand, jitter, 0, 0);
    mz_runstack_unskipped(jitter, 1);
  } else {
    mz_runstack_unskipped(jitter, 1);

    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    CHECK_RUNSTACK_OVERFLOW();
    mz_runstack_pushed(jitter, 1);
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    CHECK_LIMIT();
    
    generate_non_tail(rand, jitter, 0, 1);
    CHECK_LIMIT();

    jit_ldr_p(JIT_R1, JIT_RUNSTACK);
    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    mz_runstack_popped(jitter, 1);
  }

  /* R1 is [potential] predicate/getter, R0 is value */

  if (for_branch) {
    for_branch[2] = jit_patchable_movi_p(JIT_V1, jit_forward());
    (void)jit_calli(struct_pred_branch_code);
  } else if (kind == 1) {
    (void)jit_calli(struct_pred_code);
  } else {
    (void)jit_calli(struct_get_code);
  }

  return 1;
}

static int generate_inlined_unary(mz_jit_state *jitter, Scheme_App2_Rec *app, int is_tail, int multi_ok, 
				  jit_insn **for_branch, int branch_short)
{
  Scheme_Object *rator = app->rator;

  {
    int k;
    k = inlineable_struct_prim(rator, jitter);
    if (k == 1) {
      generate_inlined_struct_op(1, jitter, rator, app->rand, for_branch, branch_short);
      return 1;
    } else if ((k == 2) && !for_branch) {
      generate_inlined_struct_op(2, jitter, rator, app->rand, for_branch, branch_short);
      return 1;
    }
  }

  if (!SCHEME_PRIMP(rator))
    return 0;

  if (!(SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_IS_UNARY_INLINED))
    return 0;

  if (IS_NAMED_PRIM(rator, "not")) {
    generate_inlined_constant_test(jitter, app, scheme_false, NULL, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "null?")) {
    generate_inlined_constant_test(jitter, app, scheme_null, NULL, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "pair?")) {
    generate_inlined_type_test(jitter, app, scheme_pair_type, scheme_pair_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "symbol?")) {
    generate_inlined_type_test(jitter, app, scheme_symbol_type, scheme_symbol_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "syntax?")) {
    generate_inlined_type_test(jitter, app, scheme_stx_type, scheme_stx_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "char?")) {
    generate_inlined_type_test(jitter, app, scheme_char_type, scheme_char_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "boolean?")) {
    generate_inlined_constant_test(jitter, app, scheme_false, scheme_true, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "number?")) {
    generate_inlined_type_test(jitter, app, scheme_integer_type, scheme_complex_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "real?")) {
    generate_inlined_type_test(jitter, app, scheme_integer_type, scheme_complex_izi_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "procedure?")) {
    generate_inlined_type_test(jitter, app, scheme_prim_type, scheme_native_closure_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "vector?")) {
    generate_inlined_type_test(jitter, app, scheme_vector_type, scheme_vector_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "box?")) {
    generate_inlined_type_test(jitter, app, scheme_box_type, scheme_box_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "string?")) {
    generate_inlined_type_test(jitter, app, scheme_char_string_type, scheme_char_string_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "bytes?")) {
    generate_inlined_type_test(jitter, app, scheme_byte_string_type, scheme_byte_string_type, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "eof-object?")) {
    generate_inlined_constant_test(jitter, app, scheme_eof, NULL, for_branch, branch_short);
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
	|| IS_NAMED_PRIM(rator, "cdr")
	|| IS_NAMED_PRIM(rator, "cadr")
	|| IS_NAMED_PRIM(rator, "cdar")
	|| IS_NAMED_PRIM(rator, "caar")
	|| IS_NAMED_PRIM(rator, "cddr")) {
#     define MAX_LEVELS 2
      GC_CAN_IGNORE jit_insn *reffail = NULL, *ref;
      int steps, i;
      const char *name = ((Scheme_Primitive_Proc *)rator)->name;

      LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)rator)->name));

      for (steps = 0; name[steps+1] != 'r'; steps++) {
      }

      mz_runstack_skipped(jitter, 1);

      generate_non_tail(app->rand, jitter, 0, 1);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      __START_SHORT_JUMPS__(1);

      if (steps > 1) {
        jit_movr_p(JIT_R2, JIT_R0); /* save original argument */
      }
      for (i = 0; i < steps; i++) {
        if (!i) {
          ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
          reffail = _jit.x.pc;
          __END_SHORT_JUMPS__(1);
          if (steps == 1) {
            if (name[1] == 'a') {
              (void)jit_jmpi(bad_car_code);
            } else {
              (void)jit_jmpi(bad_cdr_code);
            }
          } else {
            if (name[1] == 'a') {
              if (name[2] == 'a') {
                (void)jit_jmpi(bad_caar_code);
              } else {
                (void)jit_jmpi(bad_cadr_code);
              }
            } else {
              if (name[2] == 'a') {
                (void)jit_jmpi(bad_cdar_code);
              } else {
                (void)jit_jmpi(bad_cddr_code);
              }
            }
          }
          __START_SHORT_JUMPS__(1);
          mz_patch_branch(ref);
        } else {
          (void)jit_bmsi_ul(reffail, JIT_R0, 0x1);
        }
        jit_ldxi_s(JIT_R1, JIT_R0, &((Scheme_Object *)0x0)->type);
        (void)jit_bnei_i(reffail, JIT_R1, scheme_pair_type);
        if (name[steps - i] == 'a') {
          (void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Simple_Object *)0x0)->u.pair_val.car);
        } else {
          (void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Simple_Object *)0x0)->u.pair_val.cdr);
        }
        VALIDATE_RESULT(JIT_R0);
        CHECK_LIMIT();
      }
      __END_SHORT_JUMPS__(1);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "unbox")) {
      GC_CAN_IGNORE jit_insn *reffail, *ref;

      LOG_IT(("inlined unbox\n"));

      mz_runstack_skipped(jitter, 1);

      generate_non_tail(app->rand, jitter, 0, 1);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      __START_SHORT_JUMPS__(1);
      ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
      __END_SHORT_JUMPS__(1);

      reffail = _jit.x.pc;
      (void)jit_jmpi(bad_unbox_code);

      __START_SHORT_JUMPS__(1);
      mz_patch_branch(ref);
      jit_ldxi_s(JIT_R1, JIT_R0, &((Scheme_Object *)0x0)->type);
      (void)jit_bnei_i(reffail, JIT_R1, scheme_box_type);
      __END_SHORT_JUMPS__(1);

      (void)jit_ldxi_p(JIT_R0, JIT_R0, &SCHEME_BOX_VAL(0x0));
      
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
    } else if (IS_NAMED_PRIM(rator, "bitwise-not")) {
      generate_arith(jitter, rator, app->rand, NULL, 1, 7, 0, 9, NULL, 1);
      return 1;
    }
  }

  if (!for_branch) {
    scheme_console_printf("Inlining expected.\n");
    abort();
  }

  return 0;
}

static int generate_two_args(Scheme_Object *rand1, Scheme_Object *rand2, mz_jit_state *jitter, int order_matters)
/* Results go into R0 and R1. If !order_matters, and if only the
   second is simple, then the arguments will be in reverse order. */
{
  int simple1, simple2, direction = 1;
  
  simple1 = is_constant_and_avoids_r1(rand1);
  simple2 = is_constant_and_avoids_r1(rand2);

  if (!simple1) {
    if (simple2) {
      mz_runstack_skipped(jitter, 2);

      generate_non_tail(rand1, jitter, 0, 1);
      CHECK_LIMIT();
      jit_movr_p(JIT_R1, JIT_R0);

      generate(rand2, jitter, 0, 0);
      CHECK_LIMIT();

      if (order_matters) {
        /* Swap arguments: */
        jit_movr_p(JIT_R2, JIT_R0);
        jit_movr_p(JIT_R0, JIT_R1);
        jit_movr_p(JIT_R1, JIT_R2);
      } else
        direction = -1;

      mz_runstack_unskipped(jitter, 2);
    } else {
      jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
      CHECK_RUNSTACK_OVERFLOW();
      mz_runstack_pushed(jitter, 1);
      mz_runstack_skipped(jitter, 1);

      generate_non_tail(rand1, jitter, 0, 1);
      CHECK_LIMIT();
      jit_str_p(JIT_RUNSTACK, JIT_R0);

      generate_non_tail(rand2, jitter, 0, 1);
      CHECK_LIMIT();

      jit_movr_p(JIT_R1, JIT_R0);
      jit_ldr_p(JIT_R0, JIT_RUNSTACK);

      mz_runstack_unskipped(jitter, 1);
      jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
      mz_runstack_popped(jitter, 1);
    }
  } else {
    mz_runstack_skipped(jitter, 2);

    if (simple2) {
      generate(rand2, jitter, 0, 0);
    } else {
      generate_non_tail(rand2, jitter, 0, 1);
    }
    CHECK_LIMIT();
    jit_movr_p(JIT_R1, JIT_R0);

    generate(rand1, jitter, 0, 0);
    CHECK_LIMIT();

    mz_runstack_unskipped(jitter, 2);
  }

  return direction;
}

static int generate_binary_char(mz_jit_state *jitter, Scheme_App3_Rec *app,
                                jit_insn **for_branch, int branch_short)
{
  Scheme_Object *r1, *r2, *rator = app->rator;
  GC_CAN_IGNORE jit_insn *reffail = NULL, *ref;
  int direct = 0, direction;

  LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)rator)->name));

  r1 = app->rand1;
  r2 = app->rand2;
  direction = generate_two_args(r1, r2, jitter, 1);
  CHECK_LIMIT();

  __START_SHORT_JUMPS__(branch_short);
  
  if (!SCHEME_CHARP(r1)) {
    GC_CAN_IGNORE jit_insn *pref;
    pref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
    reffail = _jit.x.pc;
    (void)jit_movi_p(JIT_R2, ((Scheme_Primitive_Proc *)rator)->prim_val);
    __END_SHORT_JUMPS__(branch_short);
    if (direction > 0) {
      (void)jit_jmpi(call_original_binary_rev_arith_code);
    } else {
      (void)jit_jmpi(call_original_binary_arith_code);
    }
    __START_SHORT_JUMPS__(branch_short);
    mz_patch_branch(pref);
    jit_ldxi_s(JIT_R2, JIT_R0, (int)&((Scheme_Object *)0x0)->type);
    (void)jit_bnei_i(reffail, JIT_R2, scheme_char_type);
    CHECK_LIMIT();
  } else {
    if (!direct)
      direct = (SCHEME_CHAR_VAL(r1) < 256);
  }
  if (!SCHEME_CHARP(r2)) {
    if (!reffail) {
      GC_CAN_IGNORE jit_insn *pref;
      pref = jit_bmci_ul(jit_forward(), JIT_R1, 0x1);
      reffail = _jit.x.pc;
      (void)jit_movi_p(JIT_R2, ((Scheme_Primitive_Proc *)rator)->prim_val);
      __END_SHORT_JUMPS__(branch_short);
      if (direction > 0) {
        (void)jit_jmpi(call_original_binary_rev_arith_code);
      } else {
        (void)jit_jmpi(call_original_binary_arith_code);
      }
      __START_SHORT_JUMPS__(branch_short);
      mz_patch_branch(pref);
    } else {
      (void)jit_bmsi_ul(reffail, JIT_R1, 0x1);
    }
    jit_ldxi_s(JIT_R2, JIT_R1, (int)&((Scheme_Object *)0x0)->type);
    (void)jit_bnei_i(reffail, JIT_R2, scheme_char_type);
    CHECK_LIMIT();
  } else {
    if (!direct)
      direct = (SCHEME_CHAR_VAL(r2) < 256);
  }

  if (!direct) {
    /* Extract character value */
    jit_ldxi_i(JIT_R0, JIT_R0, (int)&SCHEME_CHAR_VAL((Scheme_Object *)0x0));
    jit_ldxi_i(JIT_R1, JIT_R1, (int)&SCHEME_CHAR_VAL((Scheme_Object *)0x0));
    ref = jit_bner_i(jit_forward(), JIT_R0, JIT_R1);
  } else {
    ref = jit_bner_p(jit_forward(), JIT_R0, JIT_R1);
  }
  CHECK_LIMIT();
  if (for_branch) {
    for_branch[0] = ref;
  } else {
    GC_CAN_IGNORE jit_insn *ref2;
    (void)jit_movi_p(JIT_R0, scheme_true);
    ref2 = jit_jmpi(jit_forward());
    mz_patch_branch(ref);
    (void)jit_movi_p(JIT_R0, scheme_false);
    mz_patch_ucbranch(ref2);
  }
    
  __END_SHORT_JUMPS__(branch_short);

  return 1;
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
      int retptr;

      mz_runstack_skipped(jitter, 2);

      generate_non_tail(a2, jitter, 0, 1);
      CHECK_LIMIT();
      
      mz_runstack_unskipped(jitter, 2);

      if (!SCHEME_INTP(a1)
	  && !SCHEME_FALSEP(a1)
	  && !SCHEME_VOIDP(a1)
	  && !SAME_OBJ(a1, scheme_true))
	retptr = mz_retain(a1);
      else
	retptr = 0;
      
      __START_SHORT_JUMPS__(branch_short);
      
#ifdef JIT_PRECISE_GC
      if (retptr) {
	mz_load_retained(jitter, JIT_R1, retptr);
	ref = jit_bner_p(jit_forward(), JIT_R0, JIT_R1);
      } else
#endif
	ref = mz_bnei_p(jit_forward(), JIT_R0, a1);

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
      generate_two_args(a1, a2, jitter, 0);
      
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
  } else if (IS_NAMED_PRIM(rator, "char=?")) {
    generate_binary_char(jitter, app, for_branch, branch_short);
    return 1;
  } else if (!for_branch) {
    if (IS_NAMED_PRIM(rator, "+")) {
      generate_arith(jitter, rator, app->rand1, app->rand2, 2, 1, 0, 0, NULL, 1);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "-")) {
      generate_arith(jitter, rator, app->rand1, app->rand2, 2, -1, 0, 0, NULL, 1);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "min")) {
      generate_arith(jitter, rator, app->rand1, app->rand2, 2, 9, 0, 0, NULL, 1);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "max")) {
      generate_arith(jitter, rator, app->rand1, app->rand2, 2, 10, 0, 0, NULL, 1);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "bitwise-and")) {
      generate_arith(jitter, rator, app->rand1, app->rand2, 2, 3, 0, 0, NULL, 1);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "bitwise-ior")) {
      generate_arith(jitter, rator, app->rand1, app->rand2, 2, 4, 0, 0, NULL, 1);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "bitwise-xor")) {
      generate_arith(jitter, rator, app->rand1, app->rand2, 2, 5, 0, 0, NULL, 1);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "arithmetic-shift")) {
      generate_arith(jitter, rator, app->rand1, app->rand2, 2, 6, 0, 0, NULL, 1);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "vector-ref")
	       || IS_NAMED_PRIM(rator, "string-ref")
	       || IS_NAMED_PRIM(rator, "bytes-ref")) {
      int simple;
      int which;

      if (IS_NAMED_PRIM(rator, "vector-ref"))
	which = 0;
      else if (IS_NAMED_PRIM(rator, "string-ref"))
	which = 1;
      else
	which = 2;

      LOG_IT(("inlined vector-/string-/bytes-ref\n"));

      simple = (SCHEME_INTP(app->rand2)
		&& (SCHEME_INT_VAL(app->rand2) >= 0));
      
      if (simple)
	mz_runstack_skipped(jitter, 2);
      else
	mz_runstack_skipped(jitter, 1);

      if (!simple) {
	jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
        CHECK_RUNSTACK_OVERFLOW();
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
	
	if (!which) {
	  (void)jit_calli(vector_ref_check_index_code);
	} else if (which == 1) {
	  (void)jit_calli(string_ref_check_index_code);
	} else {
	  (void)jit_calli(bytes_ref_check_index_code);
	}
      } else {
	long offset;
	offset = SCHEME_INT_VAL(app->rand2);
	(void)jit_movi_p(JIT_R1, offset);
	if (!which)
	  offset = ((int)&SCHEME_VEC_ELS(0x0)) + WORDS_TO_BYTES(offset);
	else if (which == 1)
	  offset = offset << LOG_MZCHAR_SIZE;
	jit_movi_l(JIT_V1, offset);
	if (!which) {
	  (void)jit_calli(vector_ref_code);
	} else if (which == 1) {
	  (void)jit_calli(string_ref_code);
	} else {
	  (void)jit_calli(bytes_ref_code);
	}
      }

      if (simple)
	mz_runstack_unskipped(jitter, 2);
      else
	mz_runstack_unskipped(jitter, 1);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "set-car!")
               || IS_NAMED_PRIM(rator, "set-cdr!")) {
      GC_CAN_IGNORE jit_insn *reffail, *ref;
      int set_car;

      set_car = IS_NAMED_PRIM(rator, "set-car!");

      LOG_IT(("inlined set-car!\n"));

      generate_two_args(app->rand1, app->rand2, jitter, 1);
      CHECK_LIMIT();

      __START_SHORT_JUMPS__(1);
      ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
      reffail = _jit.x.pc;
      __END_SHORT_JUMPS__(1);
      if (set_car)
        (void)jit_jmpi(bad_set_car_code);
      else
        (void)jit_jmpi(bad_set_cdr_code);
      __START_SHORT_JUMPS__(1);
      mz_patch_branch(ref);
      jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Object *)0x0)->type);
      (void)jit_bnei_i(reffail, JIT_R2, scheme_pair_type);
      jit_ldxi_s(JIT_R2, JIT_R0, &(MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)0x0)));
      (void)jit_bmsi_ul(reffail, JIT_R2, 0x1);
      __END_SHORT_JUMPS__(1);
      CHECK_LIMIT();

      if (set_car)
        (void)jit_stxi_p(&((Scheme_Simple_Object *)0x0)->u.pair_val.car, JIT_R0, JIT_R1);
      else
        (void)jit_stxi_p(&((Scheme_Simple_Object *)0x0)->u.pair_val.cdr, JIT_R0, JIT_R1);
      
      (void)jit_movi_p(JIT_R0, scheme_void);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "cons")) {
      LOG_IT(("inlined cons\n"));

      generate_two_args(app->rand1, app->rand2, jitter, 1);
      CHECK_LIMIT();

      JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
      mz_prepare(2);
      jit_pusharg_p(JIT_R1);
      jit_pusharg_p(JIT_R0);
#ifdef MZ_PRECISE_GC
      (void)mz_finish(GC_malloc_pair);
#else
      (void)mz_finish(scheme_make_pair);
#endif
      jit_retval(JIT_R0);

      return 1;
    }
  }

  if (!for_branch) {
    scheme_console_printf("Inlining expected.\n");
    abort();
  }

  return 0;
}

static int generate_inlined_nary(mz_jit_state *jitter, Scheme_App_Rec *app, int is_tail, int multi_ok, 
				 jit_insn **for_branch, int branch_short)
{
  Scheme_Object *rator = app->args[0];

  if (!SCHEME_PRIMP(rator))
    return 0;

  if (!(SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_IS_MIN_NARY_INLINED))
    return 0;

  if (app->num_args != ((Scheme_Primitive_Proc *)rator)->mina)
    return 0;

  if (!for_branch) {
    if (IS_NAMED_PRIM(rator, "vector-set!")
	|| IS_NAMED_PRIM(rator, "string-set!")
	|| IS_NAMED_PRIM(rator, "bytes-set!")) {
      int simple, constval;
      int which;
      int pushed;

      if (IS_NAMED_PRIM(rator, "vector-set!"))
	which = 0;
      else if (IS_NAMED_PRIM(rator, "string-set!"))
	which = 1;
      else
	which = 2;

      LOG_IT(("inlined vector-set!\n"));

      simple = (SCHEME_INTP(app->args[2])
		&& (SCHEME_INT_VAL(app->args[2]) >= 0));

      constval = (SCHEME_TYPE(app->args[3]) > _scheme_values_types_);
      
      if (constval && simple)
	pushed = 1;
      else
	pushed = 2;

      mz_runstack_skipped(jitter, 3 - pushed);

      if (pushed) {
	jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(pushed));
        CHECK_RUNSTACK_OVERFLOW();
	mz_runstack_pushed(jitter, pushed);
      }

      generate_non_tail(app->args[1], jitter, 0, 1);
      CHECK_LIMIT();
      if (!constval || !simple) {
	jit_str_p(JIT_RUNSTACK, JIT_R0);
      } else {
	jit_movr_p(JIT_V1, JIT_R0);
      }

      if (!simple) {
	generate_non_tail(app->args[2], jitter, 0, 1);
	CHECK_LIMIT();
	if (!constval) {
	  jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R0);
	} else {
	  jit_movr_p(JIT_R1, JIT_R0);
	}
      }

      generate_non_tail(app->args[3], jitter, 0, 1);
      CHECK_LIMIT();
 
      if (!constval || !simple) {
	jit_movr_p(JIT_R2, JIT_R0);
	jit_ldr_p(JIT_R0, JIT_RUNSTACK);
	jit_str_p(JIT_RUNSTACK, JIT_R2);
	if (!simple && !constval) {
	  jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(1));
	}
      } else {
	jit_str_p(JIT_RUNSTACK, JIT_R0);
	jit_movr_p(JIT_R0, JIT_V1);
      }

      if (!simple) {
	if (!which) {
	  (void)jit_calli(vector_set_check_index_code);
	} else if (which == 1) {
	  (void)jit_calli(string_set_check_index_code);
	} else {
	  (void)jit_calli(bytes_set_check_index_code);
	}
      } else {
	long offset;
	offset = SCHEME_INT_VAL(app->args[2]);
	(void)jit_movi_p(JIT_R1, offset);
	if (!which)
	  offset = ((int)&SCHEME_VEC_ELS(0x0)) + WORDS_TO_BYTES(offset);
	else if (which == 1)
	  offset = offset << LOG_MZCHAR_SIZE;
	jit_movi_l(JIT_V1, offset);
	if (!which) {
	  (void)jit_calli(vector_set_code);
	} else if (which == 1) {
	  (void)jit_calli(string_set_code);
	} else {
	  (void)jit_calli(bytes_set_code);
	}
      }
      
      jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(pushed));
      mz_runstack_popped(jitter, pushed);

      mz_runstack_unskipped(jitter, 3 - pushed);

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

#ifdef JIT_PRECISE_GC
static Scheme_Object example_so = { scheme_native_closure_type, 0 };
#endif

static void ensure_closure_native(Scheme_Closure_Data *data, 
				  Scheme_Native_Closure_Data *case_lam)
{
  if (!data->u.native_code || SCHEME_FALSEP((Scheme_Object *)data->u.native_code)) {
    Scheme_Native_Closure_Data *code;
    code = scheme_generate_lambda(data, 0, case_lam);
    data->u.native_code = code;
  }
}

static int generate_closure(Scheme_Closure_Data *data, 
			    mz_jit_state *jitter,
                            int immediately_filled)
{
  Scheme_Native_Closure_Data *code;
  int retptr;
  
  ensure_closure_native(data, NULL);
  code = data->u.native_code;

  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();

#ifdef JIT_PRECISE_GC
  if (data->closure_size < 100) {
    int sz;
    long init_word;
    sz = (sizeof(Scheme_Native_Closure)
          + ((data->closure_size - 1) * sizeof(Scheme_Object *)));
    jit_movi_l(JIT_R0, sz);
    mz_prepare(1);
    jit_pusharg_l(JIT_R0);
    if (immediately_filled) {
      (void)mz_finish(GC_malloc_one_small_dirty_tagged);
    } else {
      (void)mz_finish(GC_malloc_one_small_tagged);
    }
    jit_retval(JIT_R0);
    retptr = mz_retain(code);
    init_word = *(long *)&example_so;
    jit_movi_l(JIT_R1, init_word);
    jit_str_l(JIT_R0, JIT_R1); 
    mz_load_retained(jitter, JIT_R1, retptr);
    jit_stxi_p((long)&((Scheme_Native_Closure *)0x0)->code, JIT_R0, JIT_R1);
    return 1;
  }
#endif

  mz_prepare(1);
  retptr = mz_retain(code);
#ifdef JIT_PRECISE_GC
  mz_load_retained(jitter, JIT_R0, retptr);
#else
  (void)jit_patchable_movi_p(JIT_R0, code); /* !! */
#endif
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
#ifdef MZTAG_REQUIRED
  ndata->iso.so.type = scheme_rt_native_code;
#endif
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
    if (data->u.native_code->max_let_depth > max_let_depth)
      max_let_depth = data->u.native_code->max_let_depth;
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

  generate_case_lambda(c, ndata, is_method);

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

static int generate_case_closure(Scheme_Object *obj, mz_jit_state *jitter)
{
  Scheme_Case_Lambda *c = (Scheme_Case_Lambda *)obj;
  Scheme_Native_Closure_Data *ndata;
  Scheme_Closure_Data *data;
  Scheme_Object *o;
  int i, offset, count, retptr;

  ensure_case_closure_native(c);
  ndata = c->native_code;

  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
  mz_prepare(1);
  retptr = mz_retain(ndata);
#ifdef JIT_PRECISE_GC
  mz_load_retained(jitter, JIT_R0, retptr);
#else
  (void)jit_patchable_movi_p(JIT_R0, ndata); /* !! */
#endif
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
    mz_pushr_p(JIT_R1);
    generate_closure(data, jitter, 1);
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
  jit_ldi_l(JIT_R2, &scheme_current_cont_mark_pos);
  jit_addi_l(JIT_R2, JIT_R2, 2);
  jit_sti_l(&scheme_current_cont_mark_pos, JIT_R2);
  return 0 /* = number of pushed items */;
}

static void generate_non_tail_mark_pos_suffix(mz_jit_state *jitter)
{
  jit_ldi_l(JIT_R2, &scheme_current_cont_mark_pos);
  jit_subi_l(JIT_R2, JIT_R2, 2);
  jit_sti_l(&scheme_current_cont_mark_pos, JIT_R2);
}

static int generate_non_tail(Scheme_Object *obj, mz_jit_state *jitter, int multi_ok, int mark_pos_ends)
{
  if (is_simple(obj, INIT_SIMPLE_DEPTH, 0, jitter, 0)) {
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
    if (is_simple(obj, INIT_SIMPLE_DEPTH, 1, jitter, 0)) {
      need_ends = 0;
    } else {
      if (mark_pos_ends)
	generate_non_tail_mark_pos_prefix(jitter);
      jit_ldi_p(JIT_R2, &scheme_current_cont_mark_stack);
      /* mark stack is an integer... turn it into a pointer */
      jit_lshi_l(JIT_R2, JIT_R2, 0x1);
      jit_ori_l(JIT_R2, JIT_R2, 0x1);
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
      jit_rshi_l(JIT_R2, JIT_R2, 0x1); /* pointer back to integer */
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

    jitter_copy = MALLOC_ONE_RT(mz_jit_state);
    memcpy(jitter_copy, jitter, sizeof(mz_jit_state));
#ifdef MZTAG_REQUIRED
    jitter_copy->type = scheme_rt_jitter_data;
#endif

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
      /* Other parts of the JIT rely on this code not modifying R1 */
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
      if (!(SCHEME_TOPLEVEL_FLAGS(obj) 
	    & (SCHEME_TOPLEVEL_CONST | SCHEME_TOPLEVEL_READY))) {
	/* Is it NULL? */
	(void)jit_beqi_p(unbound_global_code, JIT_R0, 0);
      }
      END_JIT_DATA(0);
      return 1;
    }
  case scheme_local_type:
    {
      /* Other parts of the JIT rely on this code modifying R0, only */
      int pos;
      START_JIT_DATA();
      LOG_IT(("local\n"));
      pos = mz_remap(SCHEME_LOCAL_POS(obj));
      jit_ldxi_p(JIT_R0, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
      VALIDATE_RESULT(JIT_R0);
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
      VALIDATE_RESULT(JIT_R0);

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
	  generate_case_closure(SCHEME_IPTR_VAL(obj), jitter);
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
	  mz_pushr_p(JIT_V1);
	  __START_SHORT_JUMPS__(1);
	  ref = jit_bnei_p(jit_forward(), JIT_R0, SCHEME_MULTIPLE_VALUES);
	  CHECK_LIMIT();
	  /* Save away multiple values */
	  mz_popr_p(JIT_V1);
	  mz_popr_p(JIT_V1);
	  mz_popr_p(JIT_V1);
	  jit_ldi_p(JIT_R0, &scheme_current_thread);
	  CHECK_LIMIT();
	  jit_ldxi_l(JIT_V1, JIT_R0, &((Scheme_Thread *)0x0)->ku.multiple.count);
	  jit_lshi_l(JIT_V1, JIT_V1, 0x1);
	  jit_ori_l(JIT_V1, JIT_V1, 0x1);
	  mz_pushr_p(JIT_V1);
	  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Thread *)0x0)->ku.multiple.array);
	  mz_pushr_p(JIT_V1);
	  CHECK_LIMIT();
	  (void)jit_movi_p(JIT_R1, 0x0);
	  mz_pushr_p(JIT_R1); /* pushing 0 indicates that multi-array follows */
	  /* If multi-value array is values buffer, zero out values buffer */
	  jit_ldxi_p(JIT_R2, JIT_R0, &((Scheme_Thread *)0x0)->values_buffer);
	  ref2 = jit_bner_p(jit_forward(), JIT_V1, JIT_R2);
	  jit_stxi_p(&((Scheme_Thread *)0x0)->values_buffer, JIT_R0, JIT_R1);
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
	  jit_stxi_l(&((Scheme_Thread *)0x0)->ku.multiple.count, JIT_R0, JIT_R2);
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
	  mz_prepare(3);
	  (void)jit_movi_i(JIT_R1, set_undef);
	  jit_pusharg_p(JIT_R1);
	  jit_pusharg_p(JIT_R0);
	  jit_pusharg_p(JIT_R2);
	  CHECK_LIMIT();
	  (void)mz_finish(call_set_global_bucket);
	  CHECK_LIMIT();
	  (void)jit_movi_p(JIT_R0, scheme_void);
	  END_JIT_DATA(7);
	}
	break;
      case APPVALS_EXPD:
        {
          Scheme_Object *p, *v;
          GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3, *ref5, *refloop;
          START_JIT_DATA();

	  LOG_IT(("appvals\n"));

          p = SCHEME_IPTR_VAL(obj);
	  v = SCHEME_CAR(p);
          p = SCHEME_CDR(p);

          generate_non_tail(v, jitter, 0, 1);
	  CHECK_LIMIT();

          /* If v is not known to produce a procedure, then check result: */
          if (!is_a_procedure(v, jitter)) {
            (void)jit_bmsi_l(bad_app_vals_target, JIT_R0, 0x1);
            jit_ldxi_s(JIT_R1, JIT_R0, &((Scheme_Object *)0x0)->type);
            (void)jit_blti_i(bad_app_vals_target, JIT_R1, scheme_prim_type);
            (void)jit_bgti_i(bad_app_vals_target, JIT_R1, scheme_native_closure_type);
            CHECK_LIMIT();
          }

          mz_pushr_p(JIT_R0);
          generate_non_tail(p, jitter, 1, 1);
          CHECK_LIMIT();

          mz_popr_p(JIT_V1);
          /* Function is in V1, argument(s) in R0 */

          __START_SHORT_JUMPS__(1);
          ref = jit_beqi_p(jit_forward(), JIT_R0, SCHEME_MULTIPLE_VALUES);
          /* Single-value case: --------------- */
          /* We definitely have stack space for one argument, because we
             just used it for the rator. */
          if (is_tail) {
            jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK_BASE, WORDS_TO_BYTES(1));
          } else {
            jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
          }
          CHECK_RUNSTACK_OVERFLOW();
          jit_str_p(JIT_RUNSTACK, JIT_R0);
          jit_movi_l(JIT_R0, 1);
          ref2 = jit_jmpi(jit_forward());
          CHECK_LIMIT();

          /* Multiple-values case: ------------ */
          mz_patch_branch(ref);
          /* Get new argc: */
          (void)jit_ldi_p(JIT_R1, &scheme_current_thread);
          jit_ldxi_l(JIT_R2, JIT_R1, &((Scheme_Thread *)0x0)->ku.multiple.count);
          /* Enough room on runstack? */
          jit_ldi_p(JIT_R0, &MZ_RUNSTACK_START);
          if (is_tail) {
            jit_subr_ul(JIT_R0, JIT_RUNSTACK_BASE, JIT_R0);
          } else {
            jit_subr_ul(JIT_R0, JIT_RUNSTACK, JIT_R0); 
          }
          CHECK_LIMIT();
          /* R0 is space left (in bytes), R2 is argc */
          jit_lshi_l(JIT_R2, JIT_R2, JIT_LOG_WORD_SIZE);
          if (is_tail) {
            __END_SHORT_JUMPS__(1);
            (void)jit_bltr_ul(app_values_tail_slow_code, JIT_R0, JIT_R2);
            __START_SHORT_JUMPS__(1);
            ref5 = 0;
          } else {
            GC_CAN_IGNORE jit_insn *refok;
            refok = jit_bger_ul(jit_forward(), JIT_R0, JIT_R2);
            __END_SHORT_JUMPS__(1);
            if (multi_ok) {
              (void)jit_calli(app_values_multi_slow_code);
            } else {
              (void)jit_calli(app_values_slow_code);
            }
            __START_SHORT_JUMPS__(1);
            ref5 = jit_jmpi(jit_forward());
            mz_patch_branch(refok);
          }
          CHECK_LIMIT();
          if (is_tail) {
            jit_subr_ul(JIT_RUNSTACK, JIT_RUNSTACK_BASE, JIT_R2);
          } else {
            jit_subr_ul(JIT_RUNSTACK, JIT_RUNSTACK, JIT_R2);
          }
          CHECK_RUNSTACK_OVERFLOW();
          /* Copy args: */
          jit_ldxi_l(JIT_R1, JIT_R1, &((Scheme_Thread *)0x0)->ku.multiple.array);
          refloop = _jit.x.pc;
          ref3 = jit_blei_l(jit_forward(), JIT_R2, 0);
          jit_subi_l(JIT_R2, JIT_R2, JIT_WORD_SIZE);
          jit_ldxr_p(JIT_R0, JIT_R1, JIT_R2);
          jit_stxr_p(JIT_R2, JIT_RUNSTACK, JIT_R0);
          (void)jit_jmpi(refloop);
          CHECK_LIMIT();
          mz_patch_branch(ref3);
          (void)jit_ldi_p(JIT_R0, &scheme_current_thread);
          jit_ldxi_l(JIT_R0, JIT_R0, &((Scheme_Thread *)0x0)->ku.multiple.count);
          
          /* Perform call --------------------- */
          /* Function is in V1, argc in R0, args on RUNSTACK */
          mz_patch_ucbranch(ref2);
          __END_SHORT_JUMPS__(1);

          if (is_tail) {
            if (!shared_tail_argc_code) {
              shared_tail_argc_code = generate_shared_call(-1, jitter, 1, 1, 0, 0);
            }
            mz_set_local_p(JIT_R0, JIT_LOCAL2);
            (void)jit_jmpi(shared_tail_argc_code);
          } else {
            int mo = multi_ok ? 1 : 0;
            void *code;
            if (!shared_non_tail_argc_code[mo]) {
              ensure_retry_available(jitter, multi_ok);
              code = generate_shared_call(-2, jitter, multi_ok, 0, 0, 0);
              shared_non_tail_argc_code[mo] = code;
            }
            code = shared_non_tail_argc_code[mo];
            (void)jit_calli(code);
            /* non-tail code pops args off runstack for us */
            jitter->need_set_rs = 1;
            mz_patch_ucbranch(ref5);
          }
          CHECK_LIMIT();

          END_JIT_DATA(81);

          if (is_tail)
            return 2;
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
      case SPLICE_EXPD:
        {
          scheme_signal_error("cannot JIT a top-level splice form");
        }
        break;
      default:
	{
	  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
	  obj = SCHEME_IPTR_VAL(obj);
	  (void)jit_patchable_movi_p(JIT_R2, obj); /* !! */
	  CHECK_LIMIT();
	  mz_prepare(1);
	  jit_pusharg_p(JIT_R2);
	  (void)mz_finish(scheme_syntax_executers[pos]);
	  CHECK_LIMIT();
	  jit_retval(JIT_R0);
          VALIDATE_RESULT(JIT_R0);
	}
      }
      return 1;
    }
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)obj;
      int r;

      LOG_IT(("app %d\n", app->num_args));

      r = generate_inlined_nary(jitter, app, is_tail, multi_ok, NULL, 1);
      if (r)
	return r;

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
      jit_insn *refs[5], *ref2;
      int nsrs, nsrs1, g1, g2, amt;
#ifdef NEED_LONG_JUMPS
      int then_short_ok, else_short_ok;
#else
      int then_short_ok = 1;
#endif
      START_JIT_DATA();

#ifdef NEED_LONG_JUMPS
      /* It's possible that the code for a then
	 or else branch will be so large that we might
	 need a long jump. Conservatively analyze the
	 `then' and `else' expressions. */
      then_short_ok = (is_short(branch->tbranch, 32) > 0);
      else_short_ok = (is_short(branch->fbranch, 32) > 0);
#endif

      LOG_IT(("if...\n"));

      refs[0] = NULL;
      refs[1] = NULL;
      refs[2] = NULL;
      refs[3] = NULL;
      refs[4] = NULL;

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
      if (refs[0]) {
	mz_patch_branch(refs[0]);
      }
      if (refs[1]) {
	mz_patch_branch(refs[1]);
      }
      if (refs[2]) {
	jit_patch_movi(refs[2], (_jit.x.pc));
      }
      if (refs[3]) {
	mz_patch_branch(refs[3]);
      }
      if (refs[4]) {
	mz_patch_branch(refs[4]);
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
      generate_closure(data, jitter, 1);
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
	CHECK_LIMIT();
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
	jit_ldxi_l(JIT_R1, JIT_R2, &((Scheme_Thread *)0x0)->ku.multiple.count);
	jit_ldxi_p(JIT_R2, JIT_R2, &((Scheme_Thread *)0x0)->ku.multiple.array);
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
	jit_movr_p(JIT_R2, JIT_R0);
	CHECK_LIMIT();
	  
	/* Error starts here: */
	mz_patch_ucbranch(ref3);
	JIT_UPDATE_THREAD_RSPTR_FOR_BRANCH_IF_NEEDED();
	mz_prepare(3);
	jit_pusharg_p(JIT_R2);
	jit_pusharg_i(JIT_R1);
	CHECK_LIMIT();
	jit_movi_i(JIT_V1, lv->count);
	jit_pusharg_i(JIT_V1);
	(void)mz_finish(lexical_binding_wrong_return_arity);
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
      CHECK_RUNSTACK_OVERFLOW();
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
      int i, nsrs;
      START_JIT_DATA();

      LOG_IT(("letrec...\n"));

      /* Create unfinished closures */
      for (i = 0; i < l->count; i++) {
	((Scheme_Closure_Data *)l->procs[i])->context = (Scheme_Object *)l;
	generate_closure((Scheme_Closure_Data *)l->procs[i], jitter, i + 1 == l->count);
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

      /* Assuming we can replace the last 6, push closure info instead: */
      nsrs = jitter->need_set_rs;
      if (mz_try_runstack_pop(jitter, l->count)) {
        int i;
        for (i = l->count; i--; ) {
          Scheme_Closure_Data *data2 = (Scheme_Closure_Data *)l->procs[i];
          mz_runstack_closure_pushed(jitter, 
                                     (data2->num_params
                                      - ((SCHEME_CLOSURE_DATA_FLAGS(data2) & CLOS_HAS_REST)
                                         ? 1
                                         : 0)),
                                     (((SCHEME_CLOSURE_DATA_FLAGS(data2) & CLOS_PRESERVES_MARKS)
                                       ? NATIVE_PRESERVES_MARKS
                                       : 0)
                                      | ((SCHEME_CLOSURE_DATA_FLAGS(data2) & CLOS_SINGLE_RESULT)
                                         ? NATIVE_IS_SINGLE_RESULT
                                         : 0)));
        }
        jitter->need_set_rs = nsrs;
      }

      return generate(l->body, jitter, is_tail, multi_ok);
    }
  case scheme_let_one_type:
    {
      Scheme_Let_One *lv = (Scheme_Let_One *)obj;
      START_JIT_DATA();

      LOG_IT(("leto...\n"));

      jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
      CHECK_RUNSTACK_OVERFLOW();
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
	CHECK_LIMIT();
      } else {
	mz_pushr_p(JIT_R0);
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
  case scheme_quote_syntax_type:
    {
      Scheme_Quote_Syntax *qs = (Scheme_Quote_Syntax *)obj;
      int i, c, p;
      START_JIT_DATA();
      
      LOG_IT(("quote-syntax\n"));
      
      i = qs->position;
      c = mz_remap(qs->depth);
      p = qs->midpoint;
	    
      jit_movi_i(JIT_R0, WORDS_TO_BYTES(c));
      jit_movi_i(JIT_R1, WORDS_TO_BYTES(i + p + 1));
      jit_movi_i(JIT_R2, WORDS_TO_BYTES(p));
      (void)jit_calli(quote_syntax_code);
      
      END_JIT_DATA(10);

      return 1;
    }
  default:
    {
      int retptr;
      Scheme_Type type = SCHEME_TYPE(obj);
      START_JIT_DATA();

      /* Other parts of the JIT rely on this code modifying R0, only */

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
	retptr = mz_retain(obj);
      } else
	retptr = 0;

#ifdef JIT_PRECISE_GC
      if (retptr)
	mz_load_retained(jitter, JIT_R0, retptr);
      else
#endif
	(void)jit_patchable_movi_p(JIT_R0, obj); /* !! */

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

  mz_push_locals();
  mz_set_local_p(JIT_RUNSTACK, JIT_LOCAL1);

  /* If rands == runstack and there are no rest args, set runstack
     base to runstack + rands (and don't copy rands), otherwise set
     base to runstack and proceed normally. Implement this by
     optimistically assuming rands == runstack, so that there's just
     one jump. Skip this optimization when the procedure has
     rest args, because we'll have to copy anyway. */
  if (!has_rest && num_params) {
    jit_lshi_l(JIT_RUNSTACK_BASE, JIT_R1, JIT_LOG_WORD_SIZE);
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
    CHECK_RUNSTACK_OVERFLOW();
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
  int in, i, ii, iii;
  GC_CAN_IGNORE jit_insn *ref, *ref2;

  /* *** check_arity_code *** */
  /* Called as a function: */
  check_arity_code = (Native_Check_Arity_Proc)jit_get_ip().ptr;
  jit_prolog(3); /* only need 1 argument, but return path overlaps with proc conventions */
  in = jit_arg_p();
  jit_getarg_p(JIT_R0, in); /* closure */
  in = jit_arg_p();
  jit_getarg_i(JIT_R2, in); /* argc */
  mz_push_locals();
  jit_movi_i(JIT_R1, -1);
  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->arity_code);
  jit_jmpr(JIT_V1); /* leads to a jit_ret() that assumes 3 arguments */
  CHECK_LIMIT();

  /* *** get_arity_code *** */
  /* Called as a function: */
  get_arity_code = (Native_Get_Arity_Proc)jit_get_ip().ptr;
  jit_prolog(3); /* only need 1 argument, but return path overlaps with proc conventions */
  in = jit_arg_p();
  jit_getarg_p(JIT_R0, in); /* closure */
  mz_push_locals();
  jit_movi_i(JIT_R1, -1);
  (void)jit_movi_p(JIT_R2, 0x0);
  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->arity_code);
  jit_jmpr(JIT_V1); /* leads to a jit_ret() that assumes 3 arguments */
  CHECK_LIMIT();

  /* *** bad_result_arity_code *** */
  /* Jumped-to from non-tail contexts  */
  bad_result_arity_code = (Native_Get_Arity_Proc)jit_get_ip().ptr;
  jit_ldi_p(JIT_R2, &scheme_current_thread);
  jit_ldxi_l(JIT_R1, JIT_R2, &((Scheme_Thread *)0x0)->ku.multiple.count);
  jit_ldxi_p(JIT_R2, JIT_R2, &((Scheme_Thread *)0x0)->ku.multiple.array);
  CHECK_LIMIT();
  mz_prepare(3);
  jit_pusharg_p(JIT_R2);
  jit_pusharg_i(JIT_R1);
  CHECK_LIMIT();
  jit_movi_i(JIT_V1, 1);
  jit_pusharg_i(JIT_V1);
  (void)mz_finish(call_wrong_return_arity);
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
#ifdef JIT_PRECISE_GC
  /* Save global-array index before we lose it: */
  mz_set_local_p(JIT_R0, JIT_LOCAL3);
#endif
  /* Load syntax object: */
  jit_ldxr_p(JIT_R0, JIT_V1, JIT_R1);
  /* Is it null? */
  ref = jit_bnei_p(jit_forward(), JIT_R0, 0x0);
  CHECK_LIMIT();
  /* Syntax object is NULL, so we need to create it. */
  jit_ldxr_p(JIT_R0, JIT_V1, JIT_R2); /* put element at p in R0 */
#ifndef JIT_PRECISE_GC
  /* Save global array: */
  mz_set_local_p(JIT_V1, JIT_LOCAL3);
#endif
  /* Move R1 to V1 to save it: */
  jit_movr_p(JIT_V1, JIT_R1);
  /* Compute i in JIT_R1: */
  jit_subr_p(JIT_R1, JIT_R1, JIT_R2);
  jit_subi_p(JIT_R1, JIT_R1, WORDS_TO_BYTES(1));
  jit_rshi_ul(JIT_R1, JIT_R1, JIT_LOG_WORD_SIZE);
  CHECK_LIMIT();
  /* Call scheme_delayed_rename: */
  JIT_UPDATE_THREAD_RSPTR();
  CHECK_LIMIT();
  mz_prepare(2);
  jit_pusharg_l(JIT_R1);
  jit_pusharg_p(JIT_R0);
  (void)mz_finish(scheme_delayed_rename);
  CHECK_LIMIT();
  jit_retval(JIT_R0);
  /* Restore global array into JIT_R1, and put computed element at i+p+1: */
#ifdef JIT_PRECISE_GC
  mz_get_local_p(JIT_R1, JIT_LOCAL3);
  jit_ldxr_p(JIT_R1, JIT_RUNSTACK, JIT_R1);
#else
  mz_get_local_p(JIT_R1, JIT_LOCAL3);
#endif
  jit_stxr_p(JIT_V1, JIT_R1, JIT_R0);
  mz_patch_branch(ref);
  __END_SHORT_JUMPS__(1);
  mz_epilog(JIT_V1);

  /* *** bad_{car,cdr,...}_code *** */
  /* Bad argument is in R0 for car/cdr, R2 otherwise */
  for (i = 0; i < 6; i++) {
    switch (i) {
    case 0:
      bad_car_code = jit_get_ip().ptr;
      break;
    case 1:
      bad_cdr_code = jit_get_ip().ptr;
      break;
    case 2:
      bad_caar_code = jit_get_ip().ptr;
      break;
    case 3:
      bad_cadr_code = jit_get_ip().ptr;
      break;
    case 4:
      bad_cdar_code = jit_get_ip().ptr;
      break;
    case 5:
      bad_cddr_code = jit_get_ip().ptr;      
      break;
    }
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    CHECK_RUNSTACK_OVERFLOW();
    if (i < 2) {
      jit_str_p(JIT_RUNSTACK, JIT_R0);
    } else {
      jit_str_p(JIT_RUNSTACK, JIT_R2);
    }
    JIT_UPDATE_THREAD_RSPTR();
    CHECK_LIMIT();
    jit_movi_i(JIT_R1, 1);
    jit_prepare(2);
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_R1);
    switch (i) {
    case 0:
      (void)mz_finish(scheme_checked_car);
      break;
    case 1:
      (void)mz_finish(scheme_checked_cdr);
      break;
    case 2:
      (void)mz_finish(scheme_checked_caar);
      break;
    case 3:
      (void)mz_finish(scheme_checked_cadr);
      break;
    case 4:
      (void)mz_finish(scheme_checked_cdar);
      break;
    case 5:
      (void)mz_finish(scheme_checked_cddr);
      break;
    }
    CHECK_LIMIT();
  }

  /* *** bad_set_{car,cdr}_code *** */
  /* Bad argument is in R0, other is in R1 */
  for (i = 0; i < 2; i++) {
    switch (i) {
    case 0:
      bad_set_car_code = jit_get_ip().ptr;
      break;
    case 1:
      bad_set_cdr_code = jit_get_ip().ptr;
      break;
    }
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
    CHECK_RUNSTACK_OVERFLOW();
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
    JIT_UPDATE_THREAD_RSPTR();
    CHECK_LIMIT();
    jit_movi_i(JIT_R1, 2);
    jit_prepare(2);
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_R1);
    switch (i) {
    case 0:
      (void)mz_finish(scheme_checked_set_car);
      break;
    case 1:
      (void)mz_finish(scheme_checked_set_cdr);
      break;
    }
    CHECK_LIMIT();
  }

  /* *** bad_unbox_code *** */
  /* R0 is argument */
  bad_unbox_code = jit_get_ip().ptr;
  jit_prepare(1);
  jit_pusharg_i(JIT_R0);
  (void)mz_finish(scheme_unbox);
  CHECK_LIMIT();

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
      CHECK_RUNSTACK_OVERFLOW();
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
      JIT_UPDATE_THREAD_RSPTR();
      mz_prepare(2);
      jit_pusharg_p(JIT_RUNSTACK);
      jit_pusharg_p(JIT_R1);
      (void)mz_finishr(JIT_R2);
      CHECK_LIMIT();
      jit_retval(JIT_R0);
      VALIDATE_RESULT(JIT_R0);
      jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(argc));
      JIT_UPDATE_THREAD_RSPTR();
      if (!j) {
	jit_jmpr(JIT_V1);
      } else {
	/* In for_branch mode, V1 is target for false, LOCAL2 is target for true */
	mz_get_local_p(JIT_R1, JIT_LOCAL2);
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
  mz_push_locals();
  mz_set_local_p(JIT_RUNSTACK, JIT_LOCAL1);
  on_demand_jit_arity_code = jit_get_ip().ptr; /* <<<- arity variant starts here */
  jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(3));
  CHECK_RUNSTACK_OVERFLOW();
  jit_str_p(JIT_RUNSTACK, JIT_R0);
  jit_lshi_ul(JIT_R1, JIT_R1, 0x1);
  jit_ori_ul(JIT_R1, JIT_R1, 0x1);
  CHECK_LIMIT();
  jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
  jit_stxi_p(WORDS_TO_BYTES(2), JIT_RUNSTACK, JIT_R2);
  JIT_UPDATE_THREAD_RSPTR();
  (void)jit_calli(on_demand); /* DARWIN: stack needs to be 16-byte aligned */
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
  /* Slower path (non-tail) when argv != runstack. */
  mz_patch_branch(ref);
  mz_patch_branch(ref2);
  CHECK_LIMIT();
  JIT_UPDATE_THREAD_RSPTR();
  mz_prepare(3);
  jit_pusharg_p(JIT_R2);
  jit_pusharg_p(JIT_R1);
  jit_pusharg_p(JIT_R0);
  (void)mz_finish(_scheme_apply_multi_from_native);
  CHECK_LIMIT();
  mz_get_local_p(JIT_NOT_RET, JIT_LOCAL1);
  mz_pop_locals();
  jit_ret();
  CHECK_LIMIT();

  /* *** app_values_tail_slow_code *** */
  /* RELIES ON jit_prolog(3) FROM ABOVE */
  /* Rator in V1, arguments are in thread's multiple-values cells. */
  app_values_tail_slow_code = jit_get_ip().ptr;
  JIT_UPDATE_THREAD_RSPTR();
  mz_prepare(1);
  jit_pusharg_p(JIT_V1);
  (void)mz_finish(tail_call_with_values_from_multiple_result);
  jit_retval(JIT_R0);
  VALIDATE_RESULT(JIT_R0);
  /* Pop saved runstack val and return: */
  mz_get_local_p(JIT_NOT_RET, JIT_LOCAL1);
  jit_sti_p(&scheme_current_runstack, JIT_NOT_RET);
  mz_pop_locals();
  jit_ret();  
  CHECK_LIMIT();

  /* *** finish_tail_call_[fixup_]code *** */
  /* RELIES ON jit_prolog(3) FROM ABOVE */
  finish_tail_call_code = jit_get_ip().ptr;
  generate_finish_tail_call(jitter, 0);
  CHECK_LIMIT();
  finish_tail_call_fixup_code = jit_get_ip().ptr;
  generate_finish_tail_call(jitter, 2);
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
  /* DANGER: this code must save and restore (or avoid)
     any registers that a function call would normally save 
     and restore. JIT_AUX, which is used by things like jit_ldi,
     is such a register for PPC. */
  stack_cache_pop_code = jit_get_ip().ptr;
  jit_movr_p(JIT_R0, JIT_RET);
#ifdef MZ_USE_JIT_PPC
  jit_movr_p(JIT_R(3), JIT_AUX);
#endif
  /* Decrement stack_cache_stack_pos */
  jit_ldi_l(JIT_R1, &stack_cache_stack_pos);
  jit_subi_i(JIT_R2, JIT_R1, 1);
  jit_sti_l(&stack_cache_stack_pos, JIT_R2);
  CHECK_LIMIT();
  /* Extract old return address and jump to it */
  jit_lshi_l(JIT_R1, JIT_R1, (JIT_LOG_WORD_SIZE + 2));
  jit_addi_l(JIT_R1, JIT_R1, (int)&((Stack_Cache_Elem *)0x0)->orig_return_address);
  (void)jit_movi_p(JIT_R2, &stack_cache_stack);
  jit_ldxr_p(JIT_R2, JIT_R2, JIT_R1);
  jit_movr_p(JIT_RET, JIT_R0);
#ifdef MZ_USE_JIT_PPC
  jit_movr_p(JIT_AUX, JIT_R(3));
#endif
  jit_jmpr(JIT_R2);
  CHECK_LIMIT();

  /* *** bad_app_vals_target *** */
  /* Non-proc is in R0 */
  bad_app_vals_target = jit_get_ip().ptr;
  JIT_UPDATE_THREAD_RSPTR();
  mz_prepare(1);
  jit_pusharg_p(JIT_R0);
  (void)mz_finish(raise_bad_call_with_values);
  /* Doesn't return */
  CHECK_LIMIT();

  /* *** app_values[_multi]_slow_code *** */
  /* Rator in V1, arguments are in thread's multiple-values cells. */
  for (i = 0; i < 2; i++) {
    if (i)
      app_values_multi_slow_code = jit_get_ip().ptr;
    else
      app_values_slow_code = jit_get_ip().ptr;
    mz_prolog(JIT_R1);
    JIT_UPDATE_THREAD_RSPTR();
    mz_prepare(1);
    jit_pusharg_p(JIT_V1);
    if (i) {
      (void)mz_finish(call_with_values_from_multiple_result_multi);
    } else {
      (void)mz_finish(call_with_values_from_multiple_result);
    }
    jit_retval(JIT_R0);
    VALIDATE_RESULT(JIT_R0);
    mz_epilog(JIT_R1);
    CHECK_LIMIT();
  }

  /* *** {vector,string,bytes}_{ref,set}_[check_index_]code *** */
  /* R0 is vector/string/bytes, R1 is index (Scheme number in check-index mode), 
     V1 is vector/string/bytes offset in non-check-index mode (and for
     vector, it includes the offset to the start of the elements array.
     In set mode, value is on run stack. */
  for (iii = 0; iii < 2; iii++) {
    for (ii = 0; ii < 3; ii++) {
      for (i = 0; i < 2; i++) {
	jit_insn *ref, *reffail;
	Scheme_Type ty;
	int offset, count_offset, log_elem_size;

	switch (ii) {
	case 0:
	  ty = scheme_vector_type;
	  offset = (int)&SCHEME_VEC_ELS(0x0);
	  count_offset = (int)&SCHEME_VEC_SIZE(0x0);
	  log_elem_size = JIT_LOG_WORD_SIZE;
	  if (!iii) {
	    if (!i) {
	      vector_ref_code = jit_get_ip().ptr;
	    } else {
	      vector_ref_check_index_code = jit_get_ip().ptr;
	    }
	  } else {
	    if (!i) {
	      vector_set_code = jit_get_ip().ptr;
	    } else {
	      vector_set_check_index_code = jit_get_ip().ptr;
	    }
	  }
	  break;
	case 1:
	  ty = scheme_char_string_type;
	  offset = (int)&SCHEME_CHAR_STR_VAL(0x0);
	  count_offset = (int)&SCHEME_CHAR_STRLEN_VAL(0x0);
	  log_elem_size = LOG_MZCHAR_SIZE;
	  if (!iii) {
	    if (!i) {
	      string_ref_code = jit_get_ip().ptr;
	    } else {
	      string_ref_check_index_code = jit_get_ip().ptr;
	    }
	  } else {
	    if (!i) {
	      string_set_code = jit_get_ip().ptr;
	    } else {
	      string_set_check_index_code = jit_get_ip().ptr;
	    }
	  }
	  break;
	default:
	case 2:
	  ty = scheme_byte_string_type;
	  offset = (int)&SCHEME_BYTE_STR_VAL(0x0);
	  count_offset = (int)&SCHEME_BYTE_STRLEN_VAL(0x0);
	  log_elem_size = 0;
	  if (!iii) {
	    if (!i) {
	      bytes_ref_code = jit_get_ip().ptr;
	    } else {
	      bytes_ref_check_index_code = jit_get_ip().ptr;
	    }
	  } else {
	    if (!i) {
	      bytes_set_code = jit_get_ip().ptr;
	    } else {
	      bytes_set_check_index_code = jit_get_ip().ptr;
	    }
	  }
	  break;
	}

	__START_SHORT_JUMPS__(1);

	mz_prolog(JIT_R2);

	ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
	CHECK_LIMIT();

	/* Slow path: */
	reffail = _jit.x.pc;
	if (!i) {
	  jit_lshi_ul(JIT_R1, JIT_R1, 1);
	  jit_ori_ul(JIT_R1, JIT_R1, 0x1);
	}
	jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
        CHECK_RUNSTACK_OVERFLOW();
	jit_str_p(JIT_RUNSTACK, JIT_R0);
	jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
	if (!iii) {
	  jit_movi_i(JIT_R1, 2);
	} else {
	  /* In set mode, value was already on run stack */
	  jit_movi_i(JIT_R1, 3);
	}
	JIT_UPDATE_THREAD_RSPTR();
	jit_prepare(2);
	jit_pusharg_p(JIT_RUNSTACK);
	jit_pusharg_i(JIT_R1);
	switch (ii) {
	case 0:
	  if (!iii) {
	    (void)mz_finish(scheme_checked_vector_ref);
	  } else {
	    (void)mz_finish(scheme_checked_vector_set);
	  }
	  break;
	case 1:
	  if (!iii) {
	    (void)mz_finish(scheme_checked_string_ref);
	    /* might return, if char was outside Latin-1 */
	    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
	    JIT_UPDATE_THREAD_RSPTR();
	    jit_retval(JIT_R0);
	    mz_epilog(JIT_R2);
	  } else {
	    (void)mz_finish(scheme_checked_string_set);
	  }
	  break;
	case 2:
	  if (!iii) {
	    (void)mz_finish(scheme_checked_byte_string_ref);
	  } else {
	    (void)mz_finish(scheme_checked_byte_string_set);
	  }
	  break;
	}
	/* doesn't return */
	CHECK_LIMIT();
    
	mz_patch_branch(ref);
	if (i) {
	  (void)jit_bmci_ul(reffail, JIT_R1, 0x1);
	  (void)jit_blei_l(reffail, JIT_R1, 0x0);
	}
	jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Object *)0x0)->type);
	(void)jit_bnei_i(reffail, JIT_R2, ty);
        if (iii) {
          jit_ldxi_s(JIT_R2, JIT_R0, &(MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)0x0)));
          (void)jit_bmsi_ul(reffail, JIT_R2, 0x1);
        }
	jit_ldxi_i(JIT_R2, JIT_R0, count_offset);
	if (i) {
	  /* index from expression: */
	  jit_rshi_ul(JIT_V1, JIT_R1, 1);
	  (void)jit_bler_ul(reffail, JIT_R2, JIT_V1);
	  if (log_elem_size)
	    jit_lshi_ul(JIT_V1, JIT_V1, log_elem_size);
	  if (!ii) /* vector */
	    jit_addi_p(JIT_V1, JIT_V1, offset);
	} else {
	  /* constant index supplied: */
	  (void)jit_bler_ul(reffail, JIT_R2, JIT_R1);
	}
	if (!iii) {
	  /* ref mode: */
	  switch (ii) {
	  case 0:
	    jit_ldxr_p(JIT_R0, JIT_R0, JIT_V1);
	    break;
	  case 1:
	    jit_ldxi_p(JIT_R2, JIT_R0, offset);
	    jit_ldxr_i(JIT_R2, JIT_R2, JIT_V1);
	    /* Non-Latin-1 char: use slow path: */
	    jit_extr_i_l(JIT_R2, JIT_R2);
	    (void)jit_bgti_l(reffail, JIT_R2, 255);
	    /* Latin-1: extract from scheme_char_constants: */
	    jit_lshi_l(JIT_R2, JIT_R2, JIT_LOG_WORD_SIZE);
	    (void)jit_movi_p(JIT_R0, scheme_char_constants);
	    jit_ldxr_p(JIT_R0, JIT_R0, JIT_R2);
	    break;
	  case 2:
	    jit_ldxi_p(JIT_R0, JIT_R0, offset);
	    jit_ldxr_c(JIT_R0, JIT_R0, JIT_V1);
	    jit_extr_uc_ul(JIT_R0, JIT_R0);
	    jit_lshi_l(JIT_R0, JIT_R0, 0x1);
	    jit_ori_l(JIT_R0, JIT_R0, 0x1);
	    break;
	  }
	} else {
	  /* set mode: */
	  jit_ldr_p(JIT_R2, JIT_RUNSTACK);
	  switch (ii) {
	  case 0:
	    jit_stxr_p(JIT_V1, JIT_R0, JIT_R2);
	    break;
	  case 1:
            (void)jit_bmsi_l(reffail, JIT_R2, 0x1);
	    jit_ldxi_s(JIT_R2, JIT_R2, &((Scheme_Object *)0x0)->type);
	    (void)jit_bnei_i(reffail, JIT_R2, scheme_char_type);
	    jit_ldr_p(JIT_R2, JIT_RUNSTACK);
	    jit_ldxi_i(JIT_R2, JIT_R2, &((Scheme_Small_Object *)0x0)->u.char_val);
	    jit_ldxi_p(JIT_R0, JIT_R0, offset);
	    jit_stxr_i(JIT_V1, JIT_R0, JIT_R2);
	    break;
	  case 2:
	    (void)jit_bmci_l(reffail, JIT_R2, 0x1);
	    jit_rshi_ul(JIT_R2, JIT_R2, 1);
	    (void)jit_bmsi_l(reffail, JIT_R2, ~0xFF);
	    jit_ldxi_p(JIT_R0, JIT_R0, offset);
	    jit_stxr_c(JIT_V1, JIT_R0, JIT_R2);
	    break;
	  }
	  (void)jit_movi_p(JIT_R0, scheme_void);
	}
	mz_epilog(JIT_R2);
	CHECK_LIMIT();

	__END_SHORT_JUMPS__(1);
      }
    }
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
    CHECK_RUNSTACK_OVERFLOW();
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    jit_movi_i(JIT_R1, 1);
    JIT_UPDATE_THREAD_RSPTR();
    CHECK_LIMIT();
    jit_prepare(2);
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_R1);
    (void)mz_finish(scheme_checked_syntax_e);
    jit_retval(JIT_R0);
    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    mz_epilog(JIT_R2);
    CHECK_LIMIT();
    
    /* It's not a fixnum... */
    mz_patch_branch(ref);
    jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Object *)0x0)->type);
    (void)jit_bnei_i(reffail, JIT_R2, scheme_stx_type);
    
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

  /* *** struct_{pred,get}[_branch]_code *** */
  /* R1 is (potential) struct proc, R0 is (potential) struct */
  /* In branch mode, V1 is target address for false branch */
  {
    for (i = 0; i < 3; i++) {
      void *code, *code_end;
      int kind, for_branch;
      jit_insn *ref, *ref2, *refslow, *bref1, *bref2, *bref3, *bref4, *bref5, *bref6;

      code = jit_get_ip().ptr;

      if (!i) {
	kind = 1;
	for_branch = 0;
	struct_pred_code = jit_get_ip().ptr;
      } else if (i == 1) {
	kind = 1;
	for_branch = 1;
	struct_pred_branch_code = jit_get_ip().ptr;
	/* Save target address for false branch: */
#ifdef MZ_USE_JIT_PPC
	jit_movr_p(JIT_V(3), JIT_V1);
#endif
#ifdef MZ_USE_JIT_I386
# ifdef X86_ALIGN_STACK
	mz_set_local_p(JIT_V1, JIT_LOCAL3);
# else
	jit_pushr_p(JIT_V1);
# endif
#endif
      } else {
	kind = 2;
	for_branch = 0;
	struct_get_code = jit_get_ip().ptr;
      }

      mz_prolog(JIT_V1);

      __START_SHORT_JUMPS__(1);

      ref = jit_bmci_ul(jit_forward(), JIT_R1, 0x1);
      CHECK_LIMIT();

      /* Slow path: non-struct proc, or argument type is
	 bad for a getter. */
      refslow = _jit.x.pc;
      jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
      CHECK_RUNSTACK_OVERFLOW();
      JIT_UPDATE_THREAD_RSPTR();
      jit_str_p(JIT_RUNSTACK, JIT_R0);
      jit_movi_i(JIT_V1, 1);
      jit_prepare(3);
      jit_pusharg_p(JIT_RUNSTACK);
      jit_pusharg_p(JIT_V1);
      jit_pusharg_p(JIT_R1);
      (void)mz_finish(_scheme_apply_from_native);
      jit_retval(JIT_R0);
      VALIDATE_RESULT(JIT_R0);
      jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
      JIT_UPDATE_THREAD_RSPTR();
      if (!for_branch) {
	mz_epilog(JIT_V1);
	bref5 = NULL;
	bref6 = NULL;
      } else {
	/* Need to check for true or false. */
	bref5 = jit_beqi_p(jit_forward(), JIT_R0, scheme_false);
	bref6 = jit_jmpi(jit_forward());
      }
      CHECK_LIMIT();

      /* Continue trying fast path: check proc */
      mz_patch_branch(ref);
      jit_ldxi_s(JIT_R2, JIT_R1, &((Scheme_Object *)0x0)->type);
      (void)jit_bnei_i(refslow, JIT_R2, scheme_prim_type);
      jit_ldxi_s(JIT_R2, JIT_R1, &((Scheme_Primitive_Proc *)0x0)->pp.flags);
      (void)jit_bmci_i(refslow, JIT_R2, ((kind == 1) 
					 ? SCHEME_PRIM_IS_STRUCT_PRED
					 : SCHEME_PRIM_IS_STRUCT_INDEXED_GETTER));
      CHECK_LIMIT();
      /* Check argument: */
      if (kind == 1) {
	bref1 = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
	jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Object *)0x0)->type);
	ref2 = jit_beqi_i(jit_forward(), JIT_R2, scheme_structure_type);
	bref2 = jit_bnei_i(jit_forward(), JIT_R2, scheme_proc_struct_type);
      } else {
	(void)jit_bmsi_ul(refslow, JIT_R0, 0x1);
	jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Object *)0x0)->type);
	ref2 = jit_beqi_i(jit_forward(), JIT_R2, scheme_structure_type);
	(void)jit_bnei_i(refslow, JIT_R2, scheme_proc_struct_type);
	bref1 = bref2 = NULL;
      }
      mz_patch_branch(ref2);
      CHECK_LIMIT();

      /* Put argument struct type in R2, target struct type in V1 */
      jit_ldxi_p(JIT_R2, JIT_R0, &((Scheme_Structure *)0x0)->stype);
      jit_ldxi_p(JIT_V1, JIT_R1, &((Scheme_Primitive_Closure *)0x0)->val);
      if (kind == 2) {
	jit_ldxi_p(JIT_V1, JIT_V1, &((Struct_Proc_Info *)0x0)->struct_type);
      }
      CHECK_LIMIT();

      jit_ldxi_i(JIT_R2, JIT_R2, &((Scheme_Struct_Type *)0x0)->name_pos);
      jit_ldxi_i(JIT_V1, JIT_V1, &((Scheme_Struct_Type *)0x0)->name_pos);
      /* Now R2 is argument depth, V1 is target depth */
      if (kind == 1) {
	bref3 = jit_bltr_i(jit_forward(), JIT_R2, JIT_V1);
      } else {
	(void)jit_bltr_i(refslow, JIT_R2, JIT_V1);
	bref3 = NULL;
      }
      CHECK_LIMIT();
      /* Lookup argument type at target type depth, put it in R2: */
      jit_lshi_ul(JIT_R2, JIT_V1, JIT_LOG_WORD_SIZE);
      jit_addi_p(JIT_R2, JIT_R2, &((Scheme_Struct_Type *)0x0)->parent_types);
      jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Structure *)0x0)->stype);
      jit_ldxr_p(JIT_R2, JIT_V1, JIT_R2);
      CHECK_LIMIT();

      /* Re-load target type into V1: */
      jit_ldxi_p(JIT_V1, JIT_R1, &((Scheme_Primitive_Closure *)0x0)->val);
      if (kind == 2) {
	jit_ldxi_p(JIT_V1, JIT_V1, &((Struct_Proc_Info *)0x0)->struct_type);
      }

      if (kind == 1) {
	bref4 = jit_bner_p(jit_forward(), JIT_R2, JIT_V1);

	/* True branch: */
	if (!for_branch) {
	  (void)jit_movi_p(JIT_R0, scheme_true);
	} else {
	  mz_patch_ucbranch(bref6);
#ifdef MZ_USE_JIT_I386
# ifndef X86_ALIGN_STACK
	  jit_popr_p(JIT_V1);
# endif
#endif
	}
	mz_epilog(JIT_V1);

	/* False branch: */
	mz_patch_branch(bref1);
	mz_patch_branch(bref2);
	mz_patch_branch(bref3);
	mz_patch_branch(bref4);
	if (for_branch) {
	  mz_patch_branch(bref5);
#ifdef MZ_USE_JIT_PPC
	  jit_movr_p(JIT_V1, JIT_V(3));
#endif
#ifdef MZ_USE_JIT_I386
# ifdef X86_ALIGN_STACK
	  mz_get_local_p(JIT_V1, JIT_LOCAL3);
# else
	  jit_popr_p(JIT_V1);
# endif
#endif	  
	  mz_epilog_without_jmp();
	  jit_jmpr(JIT_V1);
	} else {
	  (void)jit_movi_p(JIT_R0, scheme_false);
	  mz_epilog(JIT_V1);
	}
      } else {
	(void)jit_bner_p(refslow, JIT_R2, JIT_V1);
	bref4 = NULL;
	/* Extract field */
	jit_ldxi_p(JIT_V1, JIT_R1, &((Scheme_Primitive_Closure *)0x0)->val);
	jit_ldxi_i(JIT_V1, JIT_V1, &((Struct_Proc_Info *)0x0)->field);
	jit_lshi_ul(JIT_V1, JIT_V1, JIT_LOG_WORD_SIZE);
	jit_addi_p(JIT_V1, JIT_V1, &((Scheme_Structure *)0x0)->slots);
	jit_ldxr_p(JIT_R0, JIT_R0, JIT_V1);
	mz_epilog(JIT_V1);
      }
      CHECK_LIMIT();

      __END_SHORT_JUMPS__(1);

      if (jitter->retain_start) {
	code_end = jit_get_ip().ptr;
	add_symbol((unsigned long)code, (unsigned long)code_end - 1, scheme_false, 0);
      }
    }
  }

  return 1;
}

typedef struct {
  Scheme_Closure_Data *data;
  void *code, *tail_code, *code_end;
  int max_extra, max_depth;
  Scheme_Native_Closure *nc;
} Generate_Closure_Data;

static int do_generate_closure(mz_jit_state *jitter, void *_data)
{
  Generate_Closure_Data *gdata = (Generate_Closure_Data *)_data;
  Scheme_Closure_Data *data = gdata->data;
  void *code, *tail_code, *code_end;
  int i, r, cnt, has_rest;

  code = jit_get_ip().ptr;

  jitter->nc = gdata->nc;

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
     is in R0. If the closure has a rest arg, also ensure
     argc in R1 and argv in R2. */
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
    CHECK_RUNSTACK_OVERFLOW();
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
#ifndef JIT_PRECISE_GC
    if (data->closure_size)
#endif
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
#ifndef JIT_PRECISE_GC
    if (data->closure_size)
#endif
      mz_popr_p(JIT_R0);
    jit_stxi_p(WORDS_TO_BYTES(cnt), JIT_RUNSTACK, JIT_V1);
    mz_patch_ucbranch(ref2); /* jump here if we copied and produced null */
    CHECK_LIMIT();

    __END_SHORT_JUMPS__(cnt < 100);

    has_rest = 1;
  } else
    has_rest = 0;

#ifdef JIT_PRECISE_GC
  /* Keeping the native-closure pointer on the runstack
     ensures that the code won't be GCed while we're running
     it. */
  mz_pushr_p(JIT_R0);
#endif

  /* Extract closure to runstack: */
  cnt = data->closure_size;
  if (cnt) {
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(cnt));
    CHECK_RUNSTACK_OVERFLOW();
    
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
    int pos, self_pos = -1;
    for (i = data->closure_size; i--; ) {
      pos = data->closure_map[i];
      if (pos < lr->count) {
	Scheme_Closure_Data *data2 = (Scheme_Closure_Data *)lr->procs[pos];
	mz_runstack_closure_pushed(jitter, 
                                   (data2->num_params
                                    - ((SCHEME_CLOSURE_DATA_FLAGS(data2) & CLOS_HAS_REST)
                                       ? 1
                                       : 0)),
                                   (((SCHEME_CLOSURE_DATA_FLAGS(data2) & CLOS_PRESERVES_MARKS)
                                     ? NATIVE_PRESERVES_MARKS
                                     : 0)
                                    | ((SCHEME_CLOSURE_DATA_FLAGS(data2) & CLOS_SINGLE_RESULT)
                                     ? NATIVE_IS_SINGLE_RESULT
                                       : 0)));
	if (SAME_OBJ(lr->procs[pos], (Scheme_Object *)data)) {
	  self_pos = i;
	}
      } else
	mz_runstack_pushed(jitter, 1);
    }
    if ((self_pos >= 0) && !has_rest) {
      jitter->self_pos = self_pos;
      jitter->self_closure_size = data->closure_size;
    }
  } else {
    mz_runstack_pushed(jitter, cnt);

    /* A define-values context? */
    if (data->context && SAME_TYPE(SCHEME_TYPE(data->context), scheme_toplevel_type)) {
      jitter->self_toplevel_pos = SCHEME_TOPLEVEL_POS(data->context);
      jitter->self_closure_size = data->closure_size;
    }
  }

  LOG_IT(("PROC: %s\n", (data->name ? scheme_format_utf8("~s", 2, 1, &data->name, NULL) : "???")));
  FOR_LOG(jitter->log_depth++);

  jitter->self_data = data;

  jitter->self_restart_code = jit_get_ip().ptr;
  
  /* Generate code for the body: */
  jitter->need_set_rs = 1;
  r = generate(data->code, jitter, 1, 1);
  /* Result is in JIT_R0 */

  CHECK_LIMIT();

  /* r == 2 => tail call performed */
  if (r != 2) {
    mz_get_local_p(JIT_RUNSTACK, JIT_LOCAL1);
    jit_sti_p(&MZ_RUNSTACK, JIT_RUNSTACK);
    jit_movr_p(JIT_RET, JIT_R0);
    mz_pop_locals();
    jit_ret();
  }

  code_end = jit_get_ip().ptr;

  if (jitter->retain_start) {
    gdata->code = code;
    gdata->tail_code = tail_code;
    gdata->max_extra = jitter->max_extra_pushed;
    gdata->max_depth = jitter->max_depth;
    gdata->code_end = code_end;
  }

  return 1;
}

static void on_demand_generate_lambda(Scheme_Native_Closure *nc)
{
  Scheme_Native_Closure_Data *ndata = nc->code;
  Scheme_Closure_Data *data;
  Generate_Closure_Data gdata;
  void *code, *tail_code, *arity_code;
  int has_rest, is_method, num_params, max_depth;

  data = ndata->u2.orig_code;
  
  gdata.data = data;
  gdata.nc = nc;

  generate_one(NULL, do_generate_closure, &gdata, 1, data->name, ndata);

  if (gdata.max_depth > data->max_let_depth) {
    scheme_console_printf("Bad max depth! Given %d, counted %d.\n", data->max_let_depth, gdata.max_depth);
    abort();
  }

  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_PRESERVES_MARKS)
    SCHEME_NATIVE_CLOSURE_DATA_FLAGS(ndata) |= NATIVE_PRESERVES_MARKS;
  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_SINGLE_RESULT)
    SCHEME_NATIVE_CLOSURE_DATA_FLAGS(ndata) |= NATIVE_IS_SINGLE_RESULT;

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
      arity_code = generate_lambda_simple_arity_check(num_params, has_rest, is_method, 1);
      shared_arity_check[num_params][has_rest][is_method] = arity_code;
    }
  } else
    arity_code = generate_lambda_simple_arity_check(num_params, has_rest, is_method, 0);

  /* Add a couple of extra slots to computed let-depth, in case
     we haven't quite computed right for inlined uses, etc. */
  max_depth = WORDS_TO_BYTES(data->max_let_depth + gdata.max_extra + 2);

  /* max_let_depth is used for flags by generate_lambda: */
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

  c = MZ_RUNSTACK[0];
  argc = MZ_RUNSTACK[1];
  argv = (Scheme_Object **)MZ_RUNSTACK[2];

  on_demand_generate_lambda((Scheme_Native_Closure *)c);
}

Scheme_Native_Closure_Data *scheme_generate_lambda(Scheme_Closure_Data *data, int clear_code_after_jit,
						   Scheme_Native_Closure_Data *case_lam)
{
  Scheme_Native_Closure_Data *ndata;

  if (!check_arity_code) {
    /* Create shared code used for stack-overflow handling, etc.: */
    generate_one(NULL, do_generate_common, NULL, 0, NULL, NULL);
  }

  if (!case_lam) {
    ndata = MALLOC_ONE_RT(Scheme_Native_Closure_Data);
#ifdef MZTAG_REQUIRED
    ndata->iso.so.type = scheme_rt_native_code;
#endif
  } else {
    Scheme_Native_Closure_Data_Plus_Case *ndatap;
    ndatap = MALLOC_ONE_RT(Scheme_Native_Closure_Data_Plus_Case);
    ndatap->case_lam = case_lam;
    ndata = (Scheme_Native_Closure_Data *)ndatap;
#ifdef MZTAG_REQUIRED
    ndata->iso.so.type = scheme_rt_native_code_plus_case;
#endif
  }
  ndata->code = on_demand_jit_code;
  ndata->u.tail_code = on_demand_jit_arity_code;
  ndata->arity_code = on_demand_jit_arity_code;
  ndata->u2.orig_code = data;
  ndata->closure_size = data->closure_size;
  ndata->max_let_depth = 0x4 | (case_lam ? 0x2 : 0) | (clear_code_after_jit ? 0x1 : 0);

#if 0
  /* Compile immediately: */
  on_demand_generate_lambda(ndata);
#endif

  return ndata;
}

static int generate_simple_arity_check(mz_jit_state *jitter, int num_params, int has_rest, int is_method)
{
  /* JIT_R0 is closure */
  /* JIT_R1 is argc */
  /* JIT_R2 is argv */
  /* If arity matches, JIT_RUNSTACK and JIT_RUNSTACK_BASE should be preserved */
  /* That leaves just JIT_V1 to use if arity is ok. */
  /* This code expects a return context with 3 arguments, so make sure that's
     true dynamically for all jumps to the code. Also, at JIT time, make sure
     that jitter is initialized with a size-3 prolog. */

  jit_insn *ref, *ref2;

  __START_SHORT_JUMPS__(1);

  if (!has_rest)
    ref = jit_bnei_i(jit_forward(), JIT_R1, num_params);
  else
    ref = jit_blti_i(jit_forward(), JIT_R1, num_params);

  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->u.tail_code);
  jit_jmpr(JIT_V1);
  CHECK_LIMIT();

  /* Failed */
  mz_patch_branch(ref);
  
  /* If argc is negative, this was really a request for arity checking or reporting */
  ref = jit_blti_i(jit_forward(), JIT_R1, 0x0);

  /* Not negative, so report run-time arity mismatch */
  mz_prepare(3);
  jit_pusharg_p(JIT_R2);
  jit_pusharg_p(JIT_R1);
  jit_pusharg_p(JIT_R0);
  CHECK_LIMIT();
  (void)mz_nonrs_finish(wrong_argument_count);
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
  mz_pop_locals();
  jit_ret();
  mz_patch_branch(ref2);
  jit_movi_i(JIT_RET, 0);
  mz_pop_locals();
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
    (void)mz_nonrs_finish(scheme_box);
    mz_pop_locals();
    jit_ret();
  } else {
    jit_movr_p(JIT_RET, JIT_R0);
    mz_pop_locals();
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

static int do_generate_lambda_simple_arity_check(mz_jit_state *jitter, void *_data)
{
  Generate_Arity_Check_Data *data = (Generate_Arity_Check_Data *)_data;
  
#ifdef MZ_USE_JIT_PPC
  jitter->js.jitl.nbArgs = 2; /* matches check_arity_code prolog */
#endif

  return generate_simple_arity_check(jitter, data->num_params, data->has_rest, data->is_method);
}

static void *generate_lambda_simple_arity_check(int num_params, int has_rest, int is_method, int permanent)
{
  Generate_Arity_Check_Data data;

  data.num_params = num_params;
  data.has_rest = has_rest;
  data.is_method = is_method;

  return generate_one(NULL, do_generate_lambda_simple_arity_check, &data, !permanent, NULL, NULL);
}

static int generate_case_lambda_dispatch(mz_jit_state *jitter, Scheme_Case_Lambda *c, Scheme_Native_Closure_Data *ndata,
					 int do_getarg)
{
  /* See top of generate_simple_arity_check for register and other context info. */
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
    jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->u.tail_code);
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
    mz_prepare(3);
    jit_pusharg_p(JIT_R2);
    jit_pusharg_p(JIT_R1);
    jit_pusharg_p(JIT_R0);
    CHECK_LIMIT();
    (void)mz_finish(wrong_argument_count);
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

static void generate_case_lambda(Scheme_Case_Lambda *c, Scheme_Native_Closure_Data *ndata, int is_method)
{
  Generate_Case_Dispatch_Data gdata;
  Scheme_Closure_Data *data;
  Scheme_Object *o;
  int i, cnt, num_params, has_rest;
  mzshort *arities;

  gdata.c = c;
  gdata.ndata = ndata;
  gdata.is_method = is_method;

  generate_one(NULL, do_generate_case_lambda_dispatch, &gdata, 1, NULL, ndata);

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

static int lambda_has_been_jitted(Scheme_Native_Closure_Data *ndata)
{
  return (ndata->code != on_demand_jit_code);
}

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

  if (!lambda_has_been_jitted(((Scheme_Native_Closure *)closure)->code)) {
    Scheme_Closure c;
    c.so.type = scheme_closure_type;
    c.code = ((Scheme_Native_Closure *)closure)->code->u2.orig_code;
    return SCHEME_TRUEP(scheme_get_or_check_arity((Scheme_Object *)&c, argc));
  }

  return check_arity_code(closure, argc + 1, 0);
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
      a = scheme_make_arity(v, has_rest ? -1 : v);
      l = scheme_make_pair(a, l);
    }
    if (is_method)
      l = scheme_box(l);
    return l;
  }

  if (!lambda_has_been_jitted(((Scheme_Native_Closure *)closure)->code)) {
    Scheme_Closure c;
    Scheme_Object *a;
    c.so.type = scheme_closure_type;
    c.code = ((Scheme_Native_Closure *)closure)->code->u2.orig_code;
    a = scheme_get_or_check_arity((Scheme_Object *)&c, -1);
    if (SCHEME_CLOSURE_DATA_FLAGS(c.code) & CLOS_IS_METHOD)
      a = scheme_box(a);
    return a;
  }

  return get_arity_code(closure, 0, 0);
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
    stack_end = (unsigned long)ADJUST_STACK_START(scheme_current_thread->stack_start);
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
      /* Push after local stack of return-address proc
	 has the next return address */
      q = *(void **)p;
      q = ((void **)q)[-(3 + LOCAL_FRAME_SIZE + 1)];
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

#if 0
/* Sometimes useful for debugging MzScheme: */
void scheme_dump_stack_trace(void)
{
  void *p, *q;
  unsigned long stack_end, stack_start;
  Get_Stack_Proc gs;
  Scheme_Object *name;

  gs = (Get_Stack_Proc)get_stack_pointer_code;
  p = gs();
  stack_start = scheme_approx_sp();

  stack_end = (unsigned long)ADJUST_STACK_START(scheme_current_thread->stack_start);

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
      /* Push after local stack of return-address proc
	 has the next return address */
      q = *(void **)p;
      q = ((void **)q)[-(3 + LOCAL_FRAME_SIZE + 1)];
#endif
      name = find_symbol((unsigned long)q);
    }

    if (name) {
      printf(" scheme\n");
    } else {
      printf(" %p\n", q);
    }

    q = *(void **)p;
    if (STK_COMP((unsigned long)q, (unsigned long)p))
      break;
    p = q;
  }
}
#endif

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

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

  limit = b->stack_frame;

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
  b->stack_frame = (unsigned long)p;
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

void scheme_clean_native_symtab(void)
{
#ifndef MZ_PRECISE_GC
  clear_symbols_for_collected();
  jit_notify_freed_code();
#endif
}

#ifdef MZ_PRECISE_GC
static void release_native_code(void *fnlized, void *p)
{
  Scheme_Object *len;

  len = SCHEME_BOX_VAL(fnlized);

  /* Remove name mapping: */
  add_symbol((unsigned long)p, (unsigned long)p + SCHEME_INT_VAL(len), NULL, 0);
  /* Free memory: */
  free(p);
  jit_notify_freed_code();
}
#endif

/**********************************************************************/
/*                           Precise GC                               */
/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_JIT_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_native_closure_type, native_closure);
  GC_REG_TRAV(scheme_rt_jitter_data, mark_jit_state);
  GC_REG_TRAV(scheme_rt_native_code, native_unclosed_proc);
  GC_REG_TRAV(scheme_rt_native_code_plus_case, native_unclosed_proc_plus_case);
}

END_XFORM_SKIP;

#endif /* MZ_PRECISE_GC */

#endif /* MZ_USE_JIT */
