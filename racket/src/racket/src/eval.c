/* This file contains the main interpreter eval-apply loop,
   scheme_do_eval(), C and Scheme stack management routines,
   and other bridges between evaluation and compilation.

   Evaluation:

   The bytecode interpreter uses the C stack for continuations, and a
   separate Scheme stack for activation-frame variables and collecting
   application arguments. Closures are (nearly) flat, so mutable
   variables are boxed. A third stack is used for continuation marks,
   only as needed.

   Tail calls are, for the most part, gotos within scheme_do_eval(). A
   C function called by the main evaluation loop can perform a
   trampoling tail call via scheme_tail_apply(). The trampoline must
   return to its caller without allocating any memory, because an
   allocation optimization in the tail-call code assumes no GCs will
   occur between the time that a tail call is issued and the time when
   it's handled.

   Multiple values are returned as a special SCHEME_MULTIPLE_VALUES
   token that indicates actual values are stored in the current
   thread's record.

   The `apply' half of the `eval--apply' loop branches on all possible
   application types. Some functions can be JIT-generated native code,
   so `apply' is the bridge from interpreted code to JITted
   code. Primitive functions (including cons) are implemented by C
   functions outside the loop. Continuation applications are handled
   directly in scheme_do_eval(). That leaves calls to non-JITted
   closures, which are also performed within scheme_do_eval() (so that
   most tail calls avoid the trampoline), which is analogous to a
   primitive.

   The `eval' half of the loop handles all core syntactic forms, such
   as application and `letrec's.

   When collecting the arguments for an application, scheme_do_eval()
   avoids recursive C calls to evaluate arguments by recognizing
   easily-evaluated expressions, such as constants and variable
   lookups. This can be viewed as a kind of half-way A-normalization.

   Bytecodes are not linear. They're actually trees of expression
   nodes.

   Top-level variables (imported or defined in a linklet) are
   referenced through the Scheme stack, so that the variables can be
   re-linked each time a linklet is instantiated. The top-level are
   sometimes called the "prefix", and push_prefix() initializes the
   prefix portion of the stack. This prefix is captured in a
   continuation that refers to top-level variables (which is why the
   closure is not entirely flat). Special GC support allows a prefix
   to be pruned to just the globals that are used by live closures.

   Bytecode compilation:

   Compilation works in five passes.

   The first pass, called "compile", is the expander and compiler
   front-end. See "compile.c" along with "compenv.c".

   The second pass, called "letrec_check", determines which references
   to `letrec'-bound variables need to be guarded with a run-time
   check to prevent use before definition. The analysis result is
   reflected by the insertion of `check-not-unsafe-undefined`
   calls. This this pass mutates records produced by the "compile"
   pass.

   The third pass, called "optimize", performs constant propagation,
   constant folding, and function inlining; this pass mutates records
   produced by the "letrec_check" pass. See "optimize.c". This pass
   isn't optional; for example, it calculates closure information that
   the fourth pass uses.

   The fourth pass, called "resolve", finishes compilation by computing
   variable offsets and indirections (often mutating the records
   produced by the first pass). It is also responsible for closure
   conversion (i.e., converting closure content to arguments) and
   lifting (of procedures that close over nothing or only globals).
   Beware that the resulting bytecode object is a graph, not a tree,
   due to sharing (potentially cyclic) of closures that are "empty"
   but actually refer to other "empty" closures. See "resolve.c".

   The fifth pass, "sfs", performs another liveness analysis on stack
   slots and inserts operations to clear stack slots as necessary to
   make execution safe for space. In particular, dead slots need to be
   cleared before a non-tail call into arbitrary Racket code. This pass
   can mutate the result of the "resolve" pass. See "sfs.c".

   Bytecode marshaling and validation:

   See "marshal.c" for functions that [un]marshal bytecode form
   to/from S-expressions (roughly), which can then be printed using a
   "fast-load" format.

   The bytecode validator is applied to unmarshaled bytecode to check
   that the bytecode is well formed and won't cause any segfaults in
   the interpreter or in JITted form. See "validate.c".

   Just-in-time compilation:

   If the JIT is enabled, then an extra "jitprep" pass processes
   bytecode one more time --- but only bytecode that is not going to
   be marshaled, and possibly bytecode that was just  unmarshaled.
   In this "jitprep" pass, the `lambda' and `case-lambda'
   forms are converted to native-code generators, instead of bytecode
   variants.  The code is not actually JITted until it is called; this
   preparation step merely sets up a JIT hook for each function. The
   preparation pass is a shallow, functional (i.e., it doesn't mutate
   the original bytecode) pass; the body of a function is prepared for
   JITting lazily. See "jitprep.c".

*/

#include "schpriv.h"
#include "schrunst.h"
#ifdef MZ_USE_FUTURES
# include "future.h"
#endif

#ifdef USE_STACKAVAIL
#include <malloc.h>
#endif
#ifdef UNIX_FIND_STACK_BOUNDS
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif
#ifdef PTHREAD_STACKSEG_FIND_STACK_BOUNDS
# include <sys/signal.h>
# include <pthread.h>
# include <pthread_np.h>
#endif
#ifdef WINDOWS_FIND_STACK_BOUNDS
#include <windows.h>
#endif
#include "schmach.h"
#ifdef MACOS_STACK_LIMIT
#include <Memory.h>
#endif 
#ifdef MZ_USE_FUTURES
# include "future.h"
#endif

#ifdef MZ_USE_JIT
# define INIT_JIT_ON 1
#else
# define INIT_JIT_ON 0
#endif

#ifdef __clang__
# ifdef MZ_PRECISE_GC
#  pragma clang diagnostic ignored "-Wself-assign"
# endif
#endif

/* globals */
SHARED_OK int scheme_startup_use_jit = INIT_JIT_ON;
SHARED_OK int scheme_startup_compile_machine_independent = 0;
void scheme_set_startup_use_jit(int v) { scheme_startup_use_jit =  v; }
void scheme_set_startup_compile_machine_independent(int v) {
  scheme_startup_compile_machine_independent = v;
}

/* THREAD LOCAL SHARED */
THREAD_LOCAL_DECL(volatile int scheme_fuel_counter);
#ifdef USE_STACK_BOUNDARY_VAR
THREAD_LOCAL_DECL(uintptr_t scheme_stack_boundary);
THREAD_LOCAL_DECL(uintptr_t volatile scheme_jit_stack_boundary);
#endif
THREAD_LOCAL_DECL(int scheme_continuation_application_count);
THREAD_LOCAL_DECL(int scheme_overflow_count);
THREAD_LOCAL_DECL(Scheme_Prefix *scheme_prefix_finalize);
THREAD_LOCAL_DECL(Scheme_Prefix *scheme_inc_prefix_finalize);
THREAD_LOCAL_DECL(Scheme_Object *is_syntax_proc);
THREAD_LOCAL_DECL(Scheme_Object *expander_syntax_to_datum_proc);
THREAD_LOCAL_DECL(Scheme_Bucket_Table *scheme_namespace_to_env);
int scheme_get_overflow_count() { return scheme_overflow_count; }

/* read-only globals */
READ_ONLY Scheme_Object *scheme_eval_waiting;
READ_ONLY Scheme_Object *scheme_multiple_values;

/* symbols */
ROSYM Scheme_Object *scheme_stack_dump_key;
READ_ONLY static Scheme_Object *zero_rands_ptr; /* &zero_rands_ptr is dummy rands pointer */

/* locals */
static Scheme_Object *enable_break(int, Scheme_Object *[]);

static Scheme_Object *allow_set_undefined(int argc, Scheme_Object **argv);
static Scheme_Object *compile_module_constants(int argc, Scheme_Object **argv);
static Scheme_Object *use_jit(int argc, Scheme_Object **argv);
static Scheme_Object *disallow_inline(int argc, Scheme_Object **argv);
static Scheme_Object *compile_target_machine(int argc, Scheme_Object **argv);
static Scheme_Object *compile_is_target_machine(int argc, Scheme_Object **argv);

void scheme_escape_to_continuation(Scheme_Object *obj, int num_rands, Scheme_Object **rands, Scheme_Object *alt_full);

#define cons(x,y) scheme_make_pair(x,y)

typedef void (*DW_PrePost_Proc)(void *);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define icons scheme_make_pair

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

void
scheme_init_eval (Scheme_Startup_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

#ifdef MZ_EVAL_WAITING_CONSTANT
  scheme_eval_waiting = MZ_EVAL_WAITING_CONSTANT;
#else
  REGISTER_SO(scheme_eval_waiting);
  scheme_eval_waiting = scheme_alloc_eternal_object();
  scheme_eval_waiting->type = scheme_eval_waiting_type;
#endif

#ifdef MZ_EVAL_WAITING_CONSTANT
  scheme_multiple_values = MZ_MULTIPLE_VALUES_CONSTANT;
#else
  REGISTER_SO(scheme_multiple_values);
  scheme_multiple_values = scheme_alloc_eternal_object();
  scheme_multiple_values->type = scheme_multiple_values_type;
#endif

  REGISTER_SO(scheme_stack_dump_key);
  scheme_stack_dump_key = scheme_make_symbol("stk"); /* uninterned! */

  ADD_PRIM_W_ARITY("break-enabled",                           enable_break,                          0, 1, env);

  ADD_PARAMETER("compile-allow-set!-undefined",      allow_set_undefined,      MZCONFIG_ALLOW_SET_UNDEFINED,   env);
  ADD_PARAMETER("compile-enforce-module-constants",  compile_module_constants, MZCONFIG_COMPILE_MODULE_CONSTS, env);
  ADD_PARAMETER("eval-jit-enabled",                  use_jit,                  MZCONFIG_USE_JIT,               env);
  ADD_PARAMETER("compile-context-preservation-enabled", disallow_inline,       MZCONFIG_DISALLOW_INLINE,       env);
  ADD_PARAMETER("current-compile-target-machine",    compile_target_machine,  MZCONFIG_COMPILE_TARGET_MACHINE, env);

  ADD_PRIM_W_ARITY("compile-target-machine?",        compile_is_target_machine,                       1, 1, env);
}

void scheme_init_eval_places()
{
#ifdef DEBUG_CHECK_STACK_FRAME_SIZE
  (void)scheme_do_eval(SCHEME_TAIL_CALL_WAITING, 0, NULL, 0);
#endif
}

XFORM_NONGCING static void ignore_result(Scheme_Object *v)
{
  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
    scheme_current_thread->ku.multiple.array = NULL;
  }
}

void scheme_ignore_result(Scheme_Object *v)
{
  ignore_result(v);
}

/*========================================================================*/
/*                   C stack and Scheme stack handling                    */
/*========================================================================*/

Scheme_Object *
scheme_handle_stack_overflow(Scheme_Object *(*k)(void))
{
  /* "Stack overflow" means running out of C-stack space. The other
     end of this handler (i.e., the target for the longjmp) is
     scheme_top_level_do in fun.c */
  Scheme_Thread       *p = scheme_current_thread;
  Scheme_Overflow     *overflow;
  Scheme_Overflow_Jmp *jmp;

  scheme_about_to_move_C_stack();

  p->overflow_k = k;
  scheme_overflow_count++;

  overflow = MALLOC_ONE_RT(Scheme_Overflow);
#ifdef MZTAG_REQUIRED
  overflow->type = scheme_rt_overflow;
#endif
  /* push old overflow */
  overflow->prev = scheme_current_thread->overflow;
  p->overflow = overflow;

  overflow->stack_start = p->stack_start;

  jmp = MALLOC_ONE_RT(Scheme_Overflow_Jmp);
#ifdef MZTAG_REQUIRED
  jmp->type = scheme_rt_overflow_jmp;
#endif
  overflow->jmp = jmp;

  scheme_init_jmpup_buf(&overflow->jmp->cont);
  scheme_zero_unneeded_rands(scheme_current_thread); /* for GC */

  if (scheme_setjmpup(&overflow->jmp->cont, overflow->jmp, p->stack_start)) {
    p = scheme_current_thread;
    overflow = p->overflow;
    p->overflow = overflow->prev;
    p->error_buf = overflow->jmp->savebuf;
    if (p->meta_prompt) {
      /* When unwinding a stack overflow, we need to fix up
         the meta prompt to have the restored stack base.
         (When overflow happens with a meta prompt in place,
         no fixup is needed, because the overflow is detected 
         at the point where the meta-prompt's base would be used.) */
      Scheme_Prompt *meta_prompt;
      meta_prompt = MALLOC_ONE_TAGGED(Scheme_Prompt);
      memcpy(meta_prompt, p->meta_prompt, sizeof(Scheme_Prompt));
      meta_prompt->stack_boundary = p->stack_start;
      p->meta_prompt = meta_prompt;
    }
    if (!overflow->jmp->captured) /* reset if not captured in a continuation */
      scheme_reset_jmpup_buf(&overflow->jmp->cont);
    if (!scheme_overflow_reply) {
      /* No reply value means we should continue some escape. */
      if (p->cjs.jumping_to_continuation
          && p->cjs.is_escape) {
        /* Jump directly to prompt: */
        Scheme_Prompt *prompt = (Scheme_Prompt *)p->cjs.jumping_to_continuation;
        scheme_longjmp(*prompt->prompt_buf, 1);
      } else if (p->cjs.jumping_to_continuation
                 && SCHEME_CONTP(p->cjs.jumping_to_continuation)) {
        Scheme_Cont *c = (Scheme_Cont *)p->cjs.jumping_to_continuation;
        p->cjs.jumping_to_continuation = NULL;
        scheme_longjmpup(&c->buf_ptr->buf);
      } else {
        /* Continue normal escape: */
        scheme_longjmp(scheme_error_buf, 1);
      }
    } else {
      Scheme_Object *reply = scheme_overflow_reply;
      scheme_overflow_reply = NULL;
      return reply;
    }
  } else {
    p->stack_start = scheme_overflow_stack_start;
    scheme_longjmpup(&scheme_overflow_jmp->cont);
  }
  return NULL; /* never gets here */
}

#ifdef LINUX_FIND_STACK_BASE
static uintptr_t adjust_stack_base(uintptr_t bnd, uintptr_t lim) {
  if (bnd == scheme_get_primordial_thread_stack_base()) {
    /* The address `base' might be far from the actual stack base
       if Exec Shield is enabled (in some versions)? Use 
       "/proc/self/maps" to get exactly the stack base. */
    FILE *f;
    char *buf;
    f = fopen("/proc/self/maps", "r");
    if (f) {
      buf = malloc(256);
      while (fgets(buf, 256, f)) {
	int len;
	len = strlen(buf);
	if ((len > 8) && !strcmp("[stack]\n", buf XFORM_OK_PLUS len XFORM_OK_MINUS 8)) {
	  uintptr_t p = 0;
	  int i;
	  /* find separator: */
	  for (i = 0; buf[i]; i++) {
	    if (buf[i] == '-') {
	      i++;
	      break;
	    }
	  }
	  /* parse number after separator: */
	  for (; buf[i]; i++) {
	    if ((buf[i] >= '0') && (buf[i] <= '9')) {
	      p = (p << 4) | (buf[i] - '0');
	    } else if ((buf[i] >= 'a') && (buf[i] <= 'f')) {
	      p = (p << 4) | (buf[i] - 'a' + 10);
	    } else if ((buf[i] >= 'A') && (buf[i] <= 'F')) {
	      p = (p << 4) | (buf[i] - 'A' + 10);
	    } else
	      break;
	  }
	  /* printf("%p vs. %p: %d\n", (void*)bnd, (void*)p, p - bnd); */
	  if ((p > bnd) && ((p - lim) < bnd)) {
	    bnd = p;
	  } else {
	    /* bnd is too far from the expected range; on another thread? */
	  }
          break;
	}
      }
      free(buf);
      fclose(f);
    }
  }

  return bnd;
}
#endif

#ifdef WINDOWS_FIND_STACK_BOUNDS
intptr_t find_exe_stack_size()
{
# define WINDOWS_DEFAULT_STACK_SIZE 1048576
  intptr_t sz = WINDOWS_DEFAULT_STACK_SIZE;
  wchar_t *fn;
  DWORD len = 1024;

  /* Try to read the executable to find out the initial
     stack size. */

  fn = (wchar_t *)malloc(sizeof(wchar_t) * len);
  if (GetModuleFileNameW(NULL, fn, len) < len) {
    HANDLE fd;
    fd = CreateFileW(fn,
		     GENERIC_READ,
		     FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		     NULL,
		     OPEN_EXISTING,
		     0,
		     NULL);
    if (fd != INVALID_HANDLE_VALUE) {
      int pos;
      short kind;
      DWORD got;
      /* Skip DOS stub */
      if (SetFilePointer(fd, 0x3C, NULL, FILE_BEGIN)
	  != INVALID_SET_FILE_POINTER) {
	if (ReadFile(fd, &pos, sizeof(int), &got, NULL)
	    && (got == sizeof(int))) {
	  /* Read offset to header */
	  if (SetFilePointer(fd, pos + 20 + 4, NULL, FILE_BEGIN)
	      != INVALID_SET_FILE_POINTER) {
	    /* Check magic number */
	    if (ReadFile(fd, &kind, sizeof(short), &got, NULL)
		&& (got == sizeof(short))) {
	      /* Two possible magic numbers: PE32 or PE32+: */
	      if ((kind == 0x10b) || (kind == 0x20b)) {
		/* Skip to PE32[+] header's stack reservation value: */
		if (SetFilePointer(fd, pos + 20 + 4 + 72, NULL, FILE_BEGIN)
		    != INVALID_SET_FILE_POINTER) {
		  if (kind == 0x10b) {
		    /* PE32: 32-bit stack size: */
		    int ssz;
		    if (ReadFile(fd, &ssz, sizeof(int), &got, NULL)
			&& (got == sizeof(int))) {
		      sz = ssz;
		    }
		  } else {
		    /* PE32+: 64-bit stack size: */
		    mzlonglong lsz;
		    if (ReadFile(fd, &lsz, sizeof(mzlonglong), &got, NULL)
			&& (got == sizeof(mzlonglong))) {
		      sz = lsz;
		    }
		  }
		}
	      }
	    }
	  }
	}
      }
      CloseHandle(fd);
    }
  }
  free(fn);

  return sz;
}
#endif

void scheme_init_stack_check()
     /* Finds the C stack limit --- platform-specific. */
{
  int *v, stack_grows_up;
  uintptr_t deeper;
  
  deeper = scheme_get_deeper_address();
  stack_grows_up = (deeper > (uintptr_t)&v);

#ifdef STACK_GROWS_UP
  if (!stack_grows_up) {
    if (scheme_console_printf)
      scheme_console_printf("Stack grows DOWN, not UP.\n");
    else
      printf("Stack grows DOWN, not UP.\n");
    exit(1);
  }
#endif
#ifdef STACK_GROWS_DOWN
  if (stack_grows_up) {
    if (scheme_console_printf)
      scheme_console_printf("Stack grows UP, not DOWN.\n");
    else
      printf("Stack grows UP, not DOWN.\n");
    exit(1);
  }
#endif

#ifdef USE_STACK_BOUNDARY_VAR
  if (!scheme_stack_boundary) {
# ifdef ASSUME_FIXED_STACK_SIZE
    scheme_stack_boundary = scheme_get_current_os_thread_stack_base();
    if (stack_grows_up)
      scheme_stack_boundary += (FIXED_STACK_SIZE - STACK_SAFETY_MARGIN);
    else
      scheme_stack_boundary += (STACK_SAFETY_MARGIN - FIXED_STACK_SIZE);
# endif

# ifdef WINDOWS_FIND_STACK_BOUNDS
    scheme_stack_boundary = scheme_get_current_os_thread_stack_base();
    {
      intptr_t sz;
      sz = find_exe_stack_size();
      scheme_stack_boundary += (STACK_SAFETY_MARGIN - sz);
    }
# endif

# ifdef MACOS_FIND_STACK_BOUNDS
    scheme_stack_boundary = (uintptr_t)&v +  STACK_SAFETY_MARGIN - StackSpace();
# endif

# ifdef PALMOS_FIND_STACK_BOUNDS
    {
      Ptr s, e;
      SysGetStackInfo(Ptr &s, &e);
      scheme_stack_boundary = (uintptr_t)e + STACK_SAFETY_MARGIN;
    }
# endif

# ifdef UNIX_FIND_STACK_BOUNDS
    {
      struct rlimit rl;
      uintptr_t bnd, lim;

      bnd = (uintptr_t)scheme_get_current_os_thread_stack_base();

      getrlimit(RLIMIT_STACK, &rl);

      lim = (uintptr_t)rl.rlim_cur;
#  ifdef UNIX_STACK_MAXIMUM
      if (lim > UNIX_STACK_MAXIMUM)
        lim = UNIX_STACK_MAXIMUM;
#  endif

#  ifdef LINUX_FIND_STACK_BASE
      bnd = adjust_stack_base(bnd, lim);
#  endif

      if (stack_grows_up)
        bnd += (lim - STACK_SAFETY_MARGIN);
      else
        bnd += (STACK_SAFETY_MARGIN - lim);

      scheme_stack_boundary = bnd;
    }
# endif

# ifdef PTHREAD_STACKSEG_FIND_STACK_BOUNDS
    {
      stack_t stack;
      pthread_stackseg_np(pthread_self(), &stack);
      scheme_stack_boundary = (uintptr_t)((char *)stack.ss_sp - (stack.ss_size - STACK_SAFETY_MARGIN));
    }
# endif
  }
#endif

#ifdef USE_STACK_BOUNDARY_VAR
  scheme_jit_stack_boundary = scheme_stack_boundary;
#endif
}


int scheme_check_runstack(intptr_t size)
     /* Checks whether the Scheme stack has `size' room left */
{
  return ((MZ_RUNSTACK - MZ_RUNSTACK_START) >= (size + SCHEME_TAIL_COPY_THRESHOLD));
}

void *scheme_enlarge_runstack(intptr_t size, void *(*k)())
     /* Adds a Scheme stack segment, of at least `size' bytes */
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Saved_Stack *saved;
  void *v;
  int cont_count;
  volatile int escape;
  mz_jmp_buf newbuf, * volatile savebuf;

  saved = MALLOC_ONE_RT(Scheme_Saved_Stack);

#ifdef MZTAG_REQUIRED
  saved->type = scheme_rt_saved_stack;
#endif
  saved->prev = p->runstack_saved;
  saved->runstack_start = MZ_RUNSTACK_START;
  saved->runstack_offset = (MZ_RUNSTACK - MZ_RUNSTACK_START);
  saved->runstack_size = p->runstack_size;
  
  size += SCHEME_TAIL_COPY_THRESHOLD;

  if (size) {
    /* If we keep growing the stack, then probably it
       needs to be much larger, so at least double the 
       stack size, to a point: */
    intptr_t min_size;
    min_size = 2 * (p->runstack_size);
    if (min_size > 128000)
      min_size = 128000;
    if (size < min_size)
      size = min_size;
  } else {
    /* This is for a prompt. Re-use the current size, 
       up to a point: */
    size = p->runstack_size;
    if (size > 1000)
      size = 1000;
  }

  if (p->spare_runstack && (size <= p->spare_runstack_size)) {
    size = p->spare_runstack_size;
    MZ_RUNSTACK_START = p->spare_runstack;
    p->spare_runstack = NULL;
  } else {
    MZ_RUNSTACK_START = scheme_alloc_runstack(size);
  }
  p->runstack_size = size;
  MZ_RUNSTACK = MZ_RUNSTACK_START + size;
  p->runstack_saved = saved;
  
  cont_count = scheme_cont_capture_count;

  savebuf = p->error_buf;
  p->error_buf = &newbuf;
  if (scheme_setjmp(newbuf)) {
    v = NULL;
    escape = 1;
    p = scheme_current_thread; /* might have changed! */
  } else {
    v = k();
    escape = 0;
    p = scheme_current_thread; /* might have changed! */

    scheme_check_runstack_edge(MZ_RUNSTACK_START);

    if (cont_count == scheme_cont_capture_count) {
      if (!p->spare_runstack || (p->runstack_size > p->spare_runstack_size)) {
        p->spare_runstack = MZ_RUNSTACK_START;
        p->spare_runstack_size = p->runstack_size;
      }
    }
  }

  p->error_buf = savebuf;

  saved = p->runstack_saved;

  p->runstack_saved = saved->prev;
  MZ_RUNSTACK_START = saved->runstack_start;
  MZ_RUNSTACK = MZ_RUNSTACK_START + saved->runstack_offset;
  p->runstack_size = saved->runstack_size;

  if (escape) {
    scheme_longjmp(*p->error_buf, 1);
  }

  return v;
}

/*========================================================================*/
/*                          continuation marks                            */
/*========================================================================*/

void scheme_push_continuation_frame(Scheme_Cont_Frame_Data *d)
{
  d->cont_mark_pos = MZ_CONT_MARK_POS;
  d->cont_mark_stack = MZ_CONT_MARK_STACK;

  MZ_CONT_MARK_POS += 2;
}

void scheme_pop_continuation_frame(Scheme_Cont_Frame_Data *d)
{
  MZ_CONT_MARK_POS = d->cont_mark_pos;
  MZ_CONT_MARK_STACK = d->cont_mark_stack;
}

static MZ_MARK_STACK_TYPE clone_meta_cont_set_mark(Scheme_Meta_Continuation *mc, Scheme_Object *val, intptr_t findpos)
{
  /* Clone the meta-continuation, in case it was captured by
     a continuation in its current state. */
  Scheme_Meta_Continuation *naya;
  Scheme_Cont_Mark *cp;

  naya = MALLOC_ONE_RT(Scheme_Meta_Continuation);
  memcpy(naya, mc, sizeof(Scheme_Meta_Continuation));
  cp = MALLOC_N(Scheme_Cont_Mark, naya->cont_mark_total);
  memcpy(cp, mc->cont_mark_stack_copied, naya->cont_mark_total * sizeof(Scheme_Cont_Mark));
  naya->cont_mark_stack_copied = cp;
  naya->copy_after_captured = scheme_cont_capture_count;
  mc = naya;
  scheme_current_thread->meta_continuation = mc;

  mc->cont_mark_stack_copied[findpos].val = val;
  mc->cont_mark_stack_copied[findpos].cache = NULL;

  return 0;
}

void scheme_new_mark_segment(Scheme_Thread *p)
{
  int c = p->cont_mark_seg_count;
  Scheme_Cont_Mark **segs, *seg;
  
  /* Note: we perform allocations before changing p to avoid GC trouble,
     since Racket adjusts a thread's cont_mark_stack_segments on GC. */
  segs = MALLOC_N(Scheme_Cont_Mark *, c + 1);
  seg = scheme_malloc_allow_interior(sizeof(Scheme_Cont_Mark) * SCHEME_MARK_SEGMENT_SIZE);
  segs[c] = seg;

  if (c)
    memcpy(segs, p->cont_mark_stack_segments, c * sizeof(Scheme_Cont_Mark *));
  
  p->cont_mark_seg_count++;
  p->cont_mark_stack_segments = segs;
}

#ifdef MZ_USE_FUTURES
static void ts_scheme_new_mark_segment(Scheme_Thread *p) XFORM_SKIP_PROC
{
  if (scheme_use_rtcall && !(scheme_future_thread_state)->is_runtime_thread)
    scheme_rtcall_new_mark_segment(p);
  else
    scheme_new_mark_segment(p);
}
#else
# define ts_scheme_new_mark_segment scheme_new_mark_segment 
#endif

MZ_MARK_STACK_TYPE scheme_set_cont_mark(Scheme_Object *key, Scheme_Object *val)
/* This function can be called inside a future thread */
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Cont_Mark *cm = NULL;
  intptr_t findpos, bottom;

  findpos = (intptr_t)MZ_CONT_MARK_STACK;
  bottom = (intptr_t)p->cont_mark_stack_bottom;
  while (1) {
    if (findpos-- > bottom) {
      Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      intptr_t pos = findpos & SCHEME_MARK_SEGMENT_MASK;
      Scheme_Cont_Mark *find = seg + pos;

      if ((intptr_t)find->pos < (intptr_t)MZ_CONT_MARK_POS) {
        break;
      } else {
        if (find->key == key) {
          cm = find;
          break;
        } else {
          /* Assume that we'll mutate rather than allocate a new mark record. */
          /* This is a bad assumption for a nasty program that repeatedly
             creates a new key for the same frame, but it's good enough. */
          find->cache = NULL;
        }
      }
    } else {
      if (MZ_CONT_MARK_POS == p->cont_mark_pos_bottom + 2) {
        if (p->meta_continuation) {
          if (key != scheme_stack_dump_key) {
            /* Check the end of the meta-continuation's stack */
            Scheme_Meta_Continuation *mc = p->meta_continuation;
            for (findpos = (intptr_t)mc->cont_mark_total; findpos--; ) {
              if (mc->cont_mark_stack_copied[findpos].pos != mc->cont_mark_pos)
                break;
              if (mc->cont_mark_stack_copied[findpos].key == key) {
                if (mc->copy_after_captured < scheme_cont_capture_count) {
                  return clone_meta_cont_set_mark(mc, val, findpos);
                }
                mc->cont_mark_stack_copied[findpos].val = val;
                mc->cont_mark_stack_copied[findpos].cache = NULL;
                return 0;
              } else {
                mc->cont_mark_stack_copied[findpos].cache = NULL;
              }
            }
          }
        }
      }
      break;
    }
  }

  if (!cm) {
    /* Allocate a new mark record: */
    intptr_t segpos;
    intptr_t pos;
    Scheme_Cont_Mark *seg;

    findpos = MZ_CONT_MARK_STACK;
    segpos = ((intptr_t)findpos) >> SCHEME_LOG_MARK_SEGMENT_SIZE;
    pos = ((intptr_t)findpos) & SCHEME_MARK_SEGMENT_MASK;

    if (segpos >= p->cont_mark_seg_count) {
#ifdef MZ_USE_FUTURES
      jit_future_storage[0] = key;
      jit_future_storage[1] = val;
#endif
      ts_scheme_new_mark_segment(p);
      p = scheme_current_thread;
#ifdef MZ_USE_FUTURES
      key = jit_future_storage[0];
      val = jit_future_storage[1];
      jit_future_storage[0] = NULL;
      jit_future_storage[1] = NULL;
#endif
    }

    seg = p->cont_mark_stack_segments[segpos];
    cm = seg + pos;
    MZ_CONT_MARK_STACK = findpos + 1;
  }

  cm->key = key;
  cm->val = val;
  cm->pos = MZ_CONT_MARK_POS; /* always odd */
  cm->cache = NULL;

  return findpos;
}

void scheme_temp_dec_mark_depth()
{
  MZ_CONT_MARK_POS -= 2;
}

void scheme_temp_inc_mark_depth()
{
  MZ_CONT_MARK_POS += 2;
}

/*========================================================================*/
/*                         eval-apply helpers                             */
/*========================================================================*/

/* discourage inlining of functions call ed scheme_do_eval() to keep its frame size smaller */
MZ_DO_NOT_INLINE(static void unbound_global(Scheme_Object *obj));
MZ_DO_NOT_INLINE(static void make_tail_buffer_safe());
MZ_DO_NOT_INLINE(static Scheme_Object **evacuate_runstack(int num_rands, Scheme_Object **rands, Scheme_Object **runstack));
MZ_DO_NOT_INLINE(static Scheme_Object *define_values_execute(Scheme_Object *data));
MZ_DO_NOT_INLINE(static Scheme_Object *set_execute (Scheme_Object *data));
MZ_DO_NOT_INLINE(static Scheme_Object *ref_execute (Scheme_Object *data));
MZ_DO_NOT_INLINE(static Scheme_Object *apply_values_execute(Scheme_Object *data));
MZ_DO_NOT_INLINE(static Scheme_Object *bangboxenv_execute(Scheme_Object *data));
MZ_DO_NOT_INLINE(static Scheme_Object *begin0_execute(Scheme_Object *obj));

/* called in schapp.h */
static Scheme_Object *do_apply_known_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object **argv = (Scheme_Object **)p->ku.k.p2;

  p->ku.k.p2 = NULL;

  return _scheme_apply_known_prim_closure_multi((Scheme_Object *)p->ku.k.p1, 
						p->ku.k.i1, 
						argv);
}

#if 0
# define DEBUG_CHECK_TYPE(v) \
  if ((v != SCHEME_MULTIPLE_VALUES) \
      && (v != SCHEME_TAIL_CALL_WAITING) \
      && (v != SCHEME_EVAL_WAITING) \
      && (SCHEME_TYPE(v) > (_scheme_last_type_ + 25))) \
  { Scheme_Object *o = *(Scheme_Object **)(v); \
    if (SCHEME_TYPE(o) > (_scheme_last_type_ + 25))\
       scheme_signal_error("bad type"); }
#else
# define DEBUG_CHECK_TYPE(v) /**/
#endif

Scheme_Object *_scheme_apply_known_prim_closure_multi(Scheme_Object *rator,
						      int argc,
						      Scheme_Object **argv)
{
#define PRIM_CHECK_ARITY 0
#define PRIM_CHECK_MULTI 0
#include "schapp.inc"
}

Scheme_Object *_scheme_apply_prim_closure_multi(Scheme_Object *rator,
						int argc,
						Scheme_Object **argv)
{
#define PRIM_CHECK_ARITY 1
#define PRIM_CHECK_MULTI 0
#include "schapp.inc"
}

Scheme_Object *_scheme_apply_known_prim_closure(Scheme_Object *rator,
						int argc,
						Scheme_Object **argv)
{
#define PRIM_CHECK_ARITY 0
#define PRIM_CHECK_MULTI 1
#include "schapp.inc"
}

Scheme_Object *_scheme_apply_prim_closure(Scheme_Object *rator,
					  int argc,
					  Scheme_Object **argv)
{
#define PRIM_CHECK_ARITY 1
#define PRIM_CHECK_MULTI 1
#include "schapp.inc"
}


#ifdef MZ_USE_JIT

# define PRIM_APPLY_NAME _scheme_apply_from_native
# define PRIM_APPLY_NAME_FAST _scheme_apply_from_native_fast
# define PRIM_CHECK_VALUE 1
# define PRIM_CHECK_MULTI 1
# include "schnapp.inc"

# define PRIM_APPLY_NAME _scheme_apply_multi_from_native
# define PRIM_APPLY_NAME_FAST _scheme_apply_multi_from_native_fast
# define PRIM_CHECK_VALUE 1
# define PRIM_CHECK_MULTI 0
# include "schnapp.inc"

# define PRIM_APPLY_NAME _scheme_tail_apply_from_native
# define PRIM_APPLY_NAME_FAST _scheme_tail_apply_from_native_fast
/* It's ok to call primitive and closed primitives directly,
   since they implement further tail by trampolining. */
# define PRIM_CHECK_VALUE 0
# define PRIM_CHECK_MULTI 0
# include "schnapp.inc"

#endif

Scheme_Object *scheme_check_one_value(Scheme_Object *v)
{
  if (v == SCHEME_MULTIPLE_VALUES)
    scheme_wrong_return_arity(NULL, 1, scheme_multiple_count, scheme_multiple_array, NULL);
  return v;
}

static Scheme_Object *do_eval_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *obj = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object **argv = (Scheme_Object **)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return scheme_do_eval(obj, 
			p->ku.k.i1, 
			argv,
			p->ku.k.i2);
}

#ifdef MZ_USE_JIT
static Scheme_Object *do_eval_native_k(void)
{
  /* If argv corresponds to old runstack, copy to new runstack
     and clear old argv for space safety. */
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object **argv = (Scheme_Object **)p->ku.k.p2;

  if (argv == (p->runstack_saved->runstack_start
               + p->runstack_saved->runstack_offset)) {
    int argc = p->ku.k.i1;
    MZ_RUNSTACK -= argc;
    memcpy(MZ_RUNSTACK, argv, argc * sizeof(Scheme_Object*));
    memset(argv, 0, argc * sizeof(Scheme_Object*));
    p->ku.k.p2 = MZ_RUNSTACK;
  }

  return do_eval_k();
}
#endif

static void unbound_global(Scheme_Object *obj)
{
  Scheme_Object *tmp;

  tmp = MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(obj)];
  tmp = ((Scheme_Prefix *)tmp)->a[SCHEME_TOPLEVEL_POS(obj)];

  scheme_unbound_global((Scheme_Bucket *)tmp);
}

static void make_tail_buffer_safe()
{
  scheme_realloc_tail_buffer(scheme_current_thread);
}

static Scheme_Object **evacuate_runstack(int num_rands, Scheme_Object **rands, Scheme_Object **runstack)
{
  if (rands == runstack) {
    /* See [TC-SFS] in "schnapp.inc" */
    Scheme_Thread *p = scheme_current_thread;
    (void)scheme_tail_apply(scheme_void, num_rands, rands);
    rands = p->ku.apply.tail_rands;
    p->ku.apply.tail_rands = NULL;
    return rands;
  } else
    return rands;
}

static Scheme_Object *do_eval_k_readjust_mark(void)
{
  Scheme_Thread *p = scheme_current_thread;
  p->self_for_proc_chaperone = p->ku.k.p3;
  MZ_CONT_MARK_POS -= 2; /* undo increment in do_eval_stack_overflow() */
  return do_eval_k();
}

static Scheme_Object *do_eval_stack_overflow(Scheme_Object *obj, int num_rands, Scheme_Object **rands, 
                                             int get_value)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = (void *)obj;
  p->ku.k.i1 = num_rands;
  if (num_rands >= 0) {
    /* Copy rands: */
    GC_CAN_IGNORE void *ra;
    if (rands == p->tail_buffer)
      make_tail_buffer_safe();
    ra = (void *)MALLOC_N(Scheme_Object*, num_rands);
    p->ku.k.p2 = ra;
    {
      int i;
      for (i = num_rands; i--; ) {
        ((Scheme_Object **)ra)[i] = rands[i];
      }
    }
  } else
    p->ku.k.p2 = (void *)rands;
  p->ku.k.i2 = get_value;

  p->ku.k.p3 = p->self_for_proc_chaperone;
  p->self_for_proc_chaperone = NULL;

  /* In case we got here via scheme_force_value_same_mark(), in case
     overflow handling causes the thread to sleep, and in case another
     thread tries to get this thread's continuation marks: ensure tha
     the mark pos is not below any current mark. */
  MZ_CONT_MARK_POS += 2;

  return scheme_handle_stack_overflow(do_eval_k_readjust_mark);
}

static Scheme_Dynamic_Wind *intersect_dw(Scheme_Dynamic_Wind *a, Scheme_Dynamic_Wind *b, 
                                         Scheme_Object *prompt_tag, int b_has_tag, int *_common_depth)
{
  int alen = 0, blen = 0;
  int a_has_tag = 0, a_prompt_delta = 0, b_prompt_delta = 0;
  Scheme_Dynamic_Wind *dw, *match_a, *match_b;

  for (dw = a; dw && (dw->prompt_tag != prompt_tag); dw = dw->prev) {
  }
  if (dw) {
    /* Cut off `a' below the prompt dw. */
    a_prompt_delta = dw->depth;
    a_has_tag = 1;
  }

  if (a_has_tag)
    a_prompt_delta += 1;
  if (b_has_tag)
    b_prompt_delta += 1;

  alen = (a ? a->depth + 1 : 0) - a_prompt_delta;
  blen = (b ? b->depth + 1 : 0) - b_prompt_delta;

  while (alen > blen) {
    --alen;
    a = a->prev;
  }
  if (!alen) {
    *_common_depth = b_prompt_delta - 1;
    return a;
  }
  while (blen > alen) {
    --blen;
    b = b->prev;
  }

  /* At this point, we have chains that are the same length. */
  match_a = NULL;
  match_b = NULL;
  while (blen) {
    if (SAME_OBJ(a->id ? a->id : (Scheme_Object *)a, 
                 b->id ? b->id : (Scheme_Object *)b)) {
      if (!match_a) {
        match_a = a;
        match_b = b;
      }
    } else {
      match_a = NULL;
      match_b = NULL;
    }
    a = a->prev;
    b = b->prev;
    blen--;
  }

  if (!match_a) {
    match_a = a;
    match_b = b;
  }

  *_common_depth = (match_b ? match_b->depth : -1);

  return match_a;
}

static Scheme_Prompt *lookup_cont_prompt(Scheme_Cont *c, 
                                         Scheme_Meta_Continuation **_prompt_mc,
                                         MZ_MARK_POS_TYPE *_prompt_pos,
                                         const char *msg)
{
  Scheme_Prompt *prompt;
  Scheme_Object *pt;

  if (!c->runstack_copied)
    /* This continuation is the same as another... */
    c = c->buf_ptr->buf.cont;

  pt = c->prompt_tag;
  if (SCHEME_NP_CHAPERONEP(pt))
    pt = SCHEME_CHAPERONE_VAL(pt);

  prompt = scheme_get_prompt(SCHEME_PTR_VAL(pt), _prompt_mc, _prompt_pos);
  if (!prompt && !SAME_OBJ(scheme_default_prompt_tag, pt)) {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_CONTINUATION, msg);
  }

  return prompt;
}

#define LOOKUP_NO_PROMPT "continuation application: no corresponding prompt in the current continuation"

static Scheme_Prompt *check_barrier(Scheme_Prompt *prompt, 
                                    Scheme_Meta_Continuation *prompt_cont, MZ_MARK_POS_TYPE prompt_pos,
                                    Scheme_Cont *c)
/* A continuation barrier is analogous to a dynamic-wind. A jump is
   allowed if no dynamic-wind-like pre-thunks would be executed for
   the jump. */
{
  Scheme_Prompt *barrier_prompt, *b1, *b2;
  Scheme_Meta_Continuation *barrier_cont;
  MZ_MARK_POS_TYPE barrier_pos;

  barrier_prompt = scheme_get_barrier_prompt(&barrier_cont, &barrier_pos);
  b1 = barrier_prompt;
  if (b1) {
    if (!b1->is_barrier)
      b1 = NULL;
    else if (prompt
             && scheme_is_cm_deeper(barrier_cont, barrier_pos,
                                    prompt_cont, prompt_pos))
      b1 = NULL;
  }
  b2 = c->barrier_prompt;
  if (b2) {
    if (!b2->is_barrier)
      b2 = NULL;
  }

  if (b2 && (b1 != b2)) {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_CONTINUATION,
                     "continuation application: attempt to cross a continuation barrier");
  }

  return barrier_prompt;
}

void scheme_recheck_prompt_and_barrier(Scheme_Cont *c)
/* Check for prompt & barrier, again. We need to
   call this function like a d-w thunk, so that the meta
   continuation is right in case of an error. */
{
  Scheme_Prompt *prompt;
  Scheme_Meta_Continuation *prompt_cont;
  MZ_MARK_POS_TYPE prompt_pos;
  prompt = lookup_cont_prompt(c, &prompt_cont, &prompt_pos,
                              LOOKUP_NO_PROMPT
                              " on return from `dynamic-wind' post thunk");
  check_barrier(prompt, prompt_cont, prompt_pos, c);
}

static int exec_dyn_wind_posts(Scheme_Dynamic_Wind *common, Scheme_Cont *c, int common_depth,
                               Scheme_Dynamic_Wind **_common)
{
  int meta_depth;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Dynamic_Wind *dw;
  int old_cac = scheme_continuation_application_count;
  Scheme_Object *pt;

  *_common = common;

  for (dw = p->dw; 
       (common ? dw->depth != common->depth : dw != common);  /* not id, which may be duplicated */
       ) {
    meta_depth = p->next_meta;
    p->next_meta += dw->next_meta;
    p->dw = dw->prev;
    if (dw->post) {
      if (meta_depth > 0) {
        scheme_apply_dw_in_meta(dw, 1, meta_depth, c);
      } else {
        DW_PrePost_Proc post = dw->post;
        
        MZ_CONT_MARK_POS = dw->envss.cont_mark_pos;
        MZ_CONT_MARK_STACK = dw->envss.cont_mark_stack;
        post(dw->data);

        if (scheme_continuation_application_count != old_cac) {
          scheme_recheck_prompt_and_barrier(c);
        }
      }
      p = scheme_current_thread;
      /* p->dw might not match dw if the post thunk captures a
         continuation that is later restored in a different 
         meta continuation: */
      dw = p->dw;

      /* If any continuations were applied, then the set of dynamic
         winds may be different now than before. Re-compute the
         intersection. */
      if (scheme_continuation_application_count != old_cac) {
        old_cac = scheme_continuation_application_count;
        
        pt = c->prompt_tag;
        if (SCHEME_NP_CHAPERONEP(pt))
          pt = SCHEME_CHAPERONE_VAL(pt);

        common = intersect_dw(p->dw, c->dw, pt, c->has_prompt_dw, &common_depth);
        *_common = common;
      }
    } else
      dw = dw->prev;
  }
  return common_depth;
}

Scheme_Object *scheme_jump_to_continuation(Scheme_Object *obj, int num_rands, Scheme_Object **rands, 
                                           Scheme_Object **old_runstack, int can_ec)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Cont *c;
  Scheme_Dynamic_Wind *common, *new_common;
  Scheme_Object *value;
  Scheme_Meta_Continuation *prompt_mc;
  MZ_MARK_POS_TYPE prompt_pos;
  Scheme_Prompt *prompt, *barrier_prompt;
  int common_depth;

  /* Since scheme_escape_continuation_ok() may allocate... */
  if (rands == p->tail_buffer)
    make_tail_buffer_safe();

  c = (Scheme_Cont *)obj;
  
  /* Shortcut: if the target continuation is an extension of the current
     continuation, and if no prompt is in the way, then escape directly. */
  if (can_ec
      && c->escape_cont
      && scheme_escape_continuation_ok(c->escape_cont)) {
    prompt = lookup_cont_prompt(c, &prompt_mc, &prompt_pos, LOOKUP_NO_PROMPT);
    if (!prompt || (prompt->id
                    && (prompt->id == c->prompt_id)
                    && !prompt_mc))
      scheme_escape_to_continuation(c->escape_cont, num_rands, rands, (Scheme_Object *)c);
  }
      
  if (num_rands != 1) {
    GC_CAN_IGNORE Scheme_Object **vals;
    int i;

    vals = MALLOC_N(Scheme_Object *, num_rands);
    for (i = num_rands; i--; ) {
      vals[i] = rands[i];
    }

    value = (Scheme_Object *)vals;
  } else
    value = rands[0];
      
  DO_CHECK_FOR_BREAK(p, ;);

  if (!c->runstack_copied) {
    /* This continuation is the same as another, except
       that its mark stack is different. The different part
       of the mark stack won't be visible, so we use the other. */
    c = c->buf_ptr->buf.cont;
  }

  if (c->composable) {
    /* Composable continuation. Jump right in... */
    scheme_continuation_application_count++;
    MZ_RUNSTACK = old_runstack;
    return scheme_compose_continuation(c, num_rands, value);
  } else {
    /* Aborting (Scheme-style) continuation. */
    int orig_cac = scheme_continuation_application_count;
    Scheme_Overflow *thread_end_oflow;
    Scheme_Object *pt;

    scheme_about_to_move_C_stack();

    prompt = lookup_cont_prompt(c, &prompt_mc, &prompt_pos, LOOKUP_NO_PROMPT);
    barrier_prompt = check_barrier(prompt, prompt_mc, prompt_pos, c);

    p->suspend_break++; /* restored at call/cc destination */

    pt = c->prompt_tag;
    if (SCHEME_NP_CHAPERONEP(pt))
      pt = SCHEME_CHAPERONE_VAL(pt);

    /* Find `common', the intersection of dynamic-wind chain for 
       the current continuation and the given continuation, looking
       no further back in the current continuation than a prompt. */
    common = intersect_dw(p->dw, c->dw, pt, c->has_prompt_dw, &common_depth);

    /* For dynamic-winds after `common' in this
       continuation, execute the post-thunks */
    common_depth = exec_dyn_wind_posts(common, c, common_depth, &new_common);
    p = scheme_current_thread;

    if (orig_cac != scheme_continuation_application_count) {
      /* We checked for a barrier in exec_dyn_wind_posts, but
         get prompt & barrier again. */
      prompt = lookup_cont_prompt(c, &prompt_mc, &prompt_pos, "shouldn't fail!");
      barrier_prompt = scheme_get_barrier_prompt(NULL, NULL);
      common = new_common;
    }

    c->common_dw_depth = common_depth;
      
    /* in case we need it (since no allocation allowed later): */
    thread_end_oflow = scheme_get_thread_end_overflow();
        
    if (num_rands == 1)
      c->value = value;
    else {
      GC_CAN_IGNORE Scheme_Object *vals;
      vals = scheme_values(num_rands, (Scheme_Object **)value);
      if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
        p->values_buffer = NULL;
      c->value = vals;
    }

    /* !! No allocation or GCs allowed from here to the longjmp() !! */

    c->common_dw = common;
    c->common_next_meta = p->next_meta;

    scheme_continuation_application_count++;

    if (!prompt) {
      /* Invoke the continuation directly. If there's no prompt,
         then the prompt's job is taken by the pseudo-prompt
         created with a new thread or a barrier prompt. */
      p->meta_continuation = NULL; /* since prompt wasn't in any meta-continuation */
      p->meta_prompt = NULL;
      p->acting_barrier_prompt = NULL;
      if ((c->barrier_prompt == barrier_prompt) && barrier_prompt) {
        /* Barrier determines continuation end. */
        c->resume_to = NULL;
        p->stack_start = c->stack_start;
      } else {
        /* Prompt is pseudo-prompt at thread beginning.
           We're effectively composing the continuation,
           so use its prompt stack start. */
        c->resume_to = thread_end_oflow;
        p->stack_start = c->prompt_stack_start;
      }
      scheme_longjmpup(&c->buf_ptr->buf);
    } else if (prompt->id
               && (prompt->id == c->prompt_id)
               && !prompt_mc) {
      /* The current prompt is the same as the one in place when
         capturing the continuation, so we can jump directly. */
      scheme_drop_prompt_meta_continuations(pt);
      c->shortcut_prompt = prompt;
      if ((!prompt->boundary_overflow_id && !p->overflow)
          || (prompt->boundary_overflow_id
              && (prompt->boundary_overflow_id == p->overflow->id))) {
        scheme_longjmpup(&c->buf_ptr->buf);
      } else {
        /* Need to unwind overflows... */
        Scheme_Overflow *overflow;
        overflow = p->overflow;
        while (overflow->prev
               && (!overflow->prev->id
                   || (overflow->prev->id != prompt->boundary_overflow_id))) {
          overflow = overflow->prev;
        }
        /* Immediate destination is in scheme_handle_stack_overflow(). */
        p->cjs.jumping_to_continuation = (Scheme_Object *)c;
        p->cjs.alt_full_continuation = NULL;
        p->overflow = overflow;
        p->stack_start = overflow->stack_start;
        p->cjs.skip_dws = 0;
        scheme_longjmpup(&overflow->jmp->cont);
      }
    } else {
      /* The prompt is different than when we captured the continuation,
         so we need to compose the continuation with the current prompt. */
      p->cjs.jumping_to_continuation = (Scheme_Object *)prompt;
      p->cjs.alt_full_continuation = NULL;
      p->cjs.num_vals = 1;
      p->cjs.val = (Scheme_Object *)c;
      p->cjs.is_escape = 1;
      p->cjs.skip_dws = 0;
       
      if (prompt_mc) {
        /* The prompt is from a meta-continuation that's different
           from the current one. Jump to the meta-continuation
           and continue from there. Immediate destination is
           in compose_continuation() in fun.c; the ultimate
           destination is in scheme_finish_apply_for_prompt()
           in fun.c.
           We need to adjust the meta-continuation offsets in
           common, based on the number that we're discarding
           here. */
        {
          Scheme_Meta_Continuation *xmc;
          int offset = 1;
          for (xmc = p->meta_continuation; 
               xmc->prompt_tag != prompt_mc->prompt_tag; 
               xmc = xmc->next) {
            if (xmc->overflow)
              offset++;
          }
          c->common_next_meta -= offset;
        }
        p->meta_continuation = prompt_mc->next;
        p->stack_start = prompt_mc->overflow->stack_start;
        p->decompose_mc = prompt_mc;
        scheme_longjmpup(&prompt_mc->overflow->jmp->cont);
      } else if ((!prompt->boundary_overflow_id && !p->overflow)
                 || (prompt->boundary_overflow_id
                     && (prompt->boundary_overflow_id == p->overflow->id))) {
        /* Jump directly to the prompt: destination is in
           scheme_finish_apply_for_prompt() in fun.c. */
        if (!p->meta_continuation)
          scheme_signal_error("internal error: no meta-cont for escape");
        if (p->meta_continuation->pseudo)
          scheme_signal_error("internal error: trying to jump to a prompt in a meta-cont"
                              " that starts with a pseudo prompt");
        scheme_drop_prompt_meta_continuations(pt);
        scheme_longjmp(*prompt->prompt_buf, 1);
      } else {
        /* Need to unwind overflows to get to the prompt. */
        Scheme_Overflow *overflow;
        scheme_drop_prompt_meta_continuations(pt);
        overflow = p->overflow;
        while (overflow->prev
               && (!overflow->prev->id
                   || (overflow->prev->id != prompt->boundary_overflow_id))) {
          overflow = overflow->prev;
        }
        /* Immediate destination is in scheme_handle_stack_overflow().
           Ultimate destination is in scheme_finish_apply_for_prompt()
           in fun.c. */
        p->overflow = overflow;
        p->stack_start = overflow->stack_start;
        scheme_longjmpup(&overflow->jmp->cont);
      }
    }
    return NULL;
  }
}

void scheme_escape_to_continuation(Scheme_Object *obj, int num_rands, Scheme_Object **rands, Scheme_Object *alt_full)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *value;
  
  if (num_rands != 1) {
    GC_CAN_IGNORE Scheme_Object **vals;
    int i;

    if (rands == p->tail_buffer)
      make_tail_buffer_safe();

    vals = MALLOC_N(Scheme_Object *, num_rands);
    for (i = num_rands; i--; ) {
      vals[i] = rands[i];
    }
	
    value = (Scheme_Object *)vals;
    p->cjs.num_vals = num_rands;
  } else {
    value = rands[0];
    p->cjs.num_vals = 1;
  }
  
  if (!scheme_escape_continuation_ok(obj)) {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_CONTINUATION,
                     "continuation application: attempt to jump into an escape continuation");
  }
  
  p->cjs.val = value;
  p->cjs.jumping_to_continuation = obj;
  p->cjs.alt_full_continuation = alt_full;
  p->cjs.skip_dws = 0;
  scheme_longjmp(MZTHREADELEM(p, error_buf), 1);
}

/*========================================================================*/
/*                     evaluation of various forms                        */
/*========================================================================*/

#define CANNOT_SET_ERROR_STR "assignment disallowed"

void scheme_set_global_bucket(char *who, Scheme_Bucket *b, Scheme_Object *val,
			      int set_undef)
{
  if ((b->val || set_undef) 
      && ((b->so.type != scheme_variable_type)
	  || !(((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_IMMUTATED))
      && (val || !(((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_LINKED)))
    b->val = val;
  else {
    Scheme_Instance *home;
    home = scheme_get_bucket_home(b);
    if (home) {
      const char *msg;
      int is_set;

      if (SCHEME_TRUEP(scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_PRINT_SRCLOC)))
	msg = ("%s: " CANNOT_SET_ERROR_STR ";\n"
               " cannot %s\n"
               "  %s: %S\n"
               "  in module: %D");
      else
	msg = ("%s: " CANNOT_SET_ERROR_STR ";\n"
               " cannot %s\n"
               "  %s: %S");

      is_set = !strcmp(who, "set!");
      
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_VARIABLE, b->key,
		       msg,
		       who,
		       (b->val
			? (!val
                           ? "undefine variable that is used by other modules"
                           : (is_set
                              ? "modify a constant"
                              : "re-define a constant"))
			: "set variable before its definition"),
                       (b->val
			? (!val
                           ? "variable"
                           : "constant")
			: "variable"),
		       (Scheme_Object *)b->key,
                       home->name);
    } else {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_VARIABLE, b->key,
		       "%s: " CANNOT_SET_ERROR_STR ";\n"
                       " cannot %s\n"
                       "  %s: %S",
		       who,
                       (val
                        ? (b->val ? "change constant" : "set undefined")
                        : "undefine"),
                       (val
                        ? (b->val ? "constant" : "variable")
                        : "variable"),
		       (Scheme_Object *)b->key);
    }
  }
}

void scheme_install_macro(Scheme_Bucket *b, Scheme_Object *v)
{
  Scheme_Object *macro;

  macro = scheme_alloc_small_object();
  macro->type = scheme_macro_type;
  SCHEME_PTR_VAL(macro) = v;

  b->val = macro;
}

static Scheme_Object *define_values_execute(Scheme_Object *vec)
{
  Scheme_Object *name, *vals_expr, *vals, *var;
  int delta = 1;
  int i, g, show_any;
  Scheme_Bucket *b;

  vals_expr = SCHEME_VEC_ELS(vec)[0];

  vals = _scheme_eval_linked_expr_multi(vals_expr);

  if (SAME_OBJ(vals, SCHEME_MULTIPLE_VALUES)) {
    Scheme_Object **values;

    i = SCHEME_VEC_SIZE(vec) - delta;
    
    g = scheme_current_thread->ku.multiple.count;
    if (i == g) {
      int is_st;

      values = scheme_current_thread->ku.multiple.array;
      if (SAME_OBJ(values, scheme_current_thread->values_buffer))
	scheme_current_thread->values_buffer = NULL;
      scheme_current_thread->ku.multiple.array = NULL;

      is_st = !!scheme_is_simple_make_struct_type(vals_expr, g, CHECK_STRUCT_TYPE_RESOLVED, 
                                                  NULL, NULL, NULL, NULL,
                                                  NULL, MZ_RUNSTACK, 0, 
                                                  NULL, NULL, 5);
      if (!is_st)
        is_st = scheme_is_simple_make_struct_type_property(vals_expr, g, CHECK_STRUCT_TYPE_RESOLVED, 
                                                           NULL, NULL, NULL, MZ_RUNSTACK, 0, 
                                                           NULL, 5);
      
      for (i = 0; i < g; i++) {
	Scheme_Prefix *toplevels;

        var = SCHEME_VEC_ELS(vec)[i+delta];
        if (SAME_TYPE(SCHEME_TYPE(var), scheme_toplevel_type)) {
          toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(var)];
          b = (Scheme_Bucket *)toplevels->a[SCHEME_TOPLEVEL_POS(var)];
        } else
          b = (Scheme_Bucket *)SCHEME_STATIC_TOPLEVEL_PREFIX(var)->a[SCHEME_TOPLEVEL_POS(var)];

        scheme_set_global_bucket("define-values", b, values[i], 1);
        
        if (SCHEME_TOPLEVEL_FLAGS(var) & SCHEME_TOPLEVEL_SEAL) {
          if (is_st)
            ((Scheme_Bucket_With_Flags *)b)->flags |= (GLOB_IS_IMMUTATED | GLOB_IS_CONSISTENT);
          else
            ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_IS_IMMUTATED;
	}
      }
	
      return scheme_void;
    } else {
      if (SAME_OBJ(scheme_current_thread->ku.multiple.array, scheme_current_thread->values_buffer))
        scheme_current_thread->values_buffer = NULL;
    }
  } else if (SCHEME_VEC_SIZE(vec) == delta + 1) { /* => single var */
    Scheme_Prefix *toplevels;

    var = SCHEME_VEC_ELS(vec)[delta];
    if (SAME_TYPE(SCHEME_TYPE(var), scheme_toplevel_type)) {
      toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(var)];
      b = (Scheme_Bucket *)toplevels->a[SCHEME_TOPLEVEL_POS(var)];
    } else
      b = (Scheme_Bucket *)SCHEME_STATIC_TOPLEVEL_PREFIX(var)->a[SCHEME_TOPLEVEL_POS(var)];

    scheme_set_global_bucket("define-values", b, vals, 1);
      
    if (SCHEME_TOPLEVEL_FLAGS(var) & SCHEME_TOPLEVEL_SEAL) {
      int flags = GLOB_IS_IMMUTATED;
      if (scheme_is_statically_proc(vals_expr, NULL, OMITTABLE_RESOLVED)
          || (SCHEME_TYPE(vals_expr) >= _scheme_values_types_))
        flags |= GLOB_IS_CONSISTENT;
      ((Scheme_Bucket_With_Flags *)b)->flags |= flags;
    }

    return scheme_void;
  } else
    g = 1;
  
  i = SCHEME_VEC_SIZE(vec) - delta;

  show_any = i;

  if (show_any) {
    Scheme_Prefix *toplevels;
    var = SCHEME_VEC_ELS(vec)[delta];
    toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(var)];
    b = (Scheme_Bucket *)toplevels->a[SCHEME_TOPLEVEL_POS(var)];
    name = (Scheme_Object *)b->key;
  } else
    name = NULL;
  
  {
    const char *symname;

    symname = (show_any ? scheme_symbol_name(name) : "");

    scheme_wrong_return_arity("define-values",
			      i, g,
			      (g == 1) ? (Scheme_Object **)vals : scheme_current_thread->ku.multiple.array,
			      "\n  in: %s%s%s",
			      show_any ? "definition of " : "definition of 0 identifiers",
			      symname,
			      show_any ? ((i == 1) ? "" : " ...") : "");
  }

  return NULL;
}

static Scheme_Object *set_execute (Scheme_Object *data)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)data;
  Scheme_Object *val;
  Scheme_Bucket *var;
  Scheme_Prefix *toplevels;

  val = _scheme_eval_linked_expr(sb->val);

  if (SAME_TYPE(SCHEME_TYPE(sb->var), scheme_toplevel_type)) {
    toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(sb->var)];
    var = (Scheme_Bucket *)toplevels->a[SCHEME_TOPLEVEL_POS(sb->var)];
  } else
    var = (Scheme_Bucket *)SCHEME_STATIC_TOPLEVEL_PREFIX(sb->var)->a[SCHEME_TOPLEVEL_POS(sb->var)];

  scheme_set_global_bucket("set!", var, val, sb->set_undef);

  return scheme_void;
}

static Scheme_Object *ref_execute (Scheme_Object *data)
{
  Scheme_Prefix *toplevels;
  Scheme_Object *o;
  Scheme_Object *var;
  Scheme_Object *tl;
  Scheme_Instance *home;

  tl = SCHEME_PTR1_VAL(data);
  if (SCHEME_FALSEP(tl))
    var = NULL;
  else if (SCHEME_SYMBOLP(tl) || SAME_OBJ(tl, scheme_true))
    var = tl;
  else {
    toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(tl)];
    var = toplevels->a[SCHEME_TOPLEVEL_POS(tl)];
  }

  tl = SCHEME_PTR2_VAL(data);
  if (SCHEME_FALSEP(tl))
    home = NULL;
  else {
    toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(tl)];
    o = toplevels->a[SCHEME_TOPLEVEL_POS(tl)];
    home = scheme_get_bucket_home((Scheme_Bucket *)o);
  }
  
  o = scheme_alloc_object();
  o->type = scheme_global_ref_type;
  SCHEME_PTR1_VAL(o) = (var ? var : scheme_false);
  SCHEME_PTR2_VAL(o) = (home ? (Scheme_Object *)home : scheme_false);

  SCHEME_VARREF_FLAGS(data) |= (SCHEME_VARREF_FLAGS(o) & VARREF_FLAGS_MASK);

  return o;
}

static Scheme_Object *apply_values_execute(Scheme_Object *data)
{
  Scheme_Object *f, *v;
  
  f = SCHEME_PTR1_VAL(data);

  f = _scheme_eval_linked_expr(f);
  if (!SCHEME_PROCP(f)) {
    Scheme_Object *a[1];
    a[0] = f;
    scheme_wrong_contract("call-with-values", "procedure?", -1, 1, a);    
    return NULL;
  }

  v = _scheme_eval_linked_expr_multi(SCHEME_PTR2_VAL(data));
  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
    Scheme_Thread *p = scheme_current_thread;
    Scheme_Object **rands;
    int num_rands = p->ku.multiple.count;

    if (num_rands > p->tail_buffer_size) {
      /* scheme_tail_apply will allocate */
      if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
        p->values_buffer = NULL;
    }
    rands = p->ku.multiple.array;
    p->ku.multiple.array = NULL;
    return scheme_tail_apply(f, num_rands, rands);
  } else {
    Scheme_Object *a[1];
    a[0] = v;
    return scheme_tail_apply(f, 1, a);
  }
}

Scheme_Object *
scheme_case_lambda_execute(Scheme_Object *expr)
{
  Scheme_Case_Lambda *seqin, *seqout;
  int i, cnt;
  Scheme_Thread *p = scheme_current_thread;

  seqin = (Scheme_Case_Lambda *)expr;

#ifdef MZ_USE_JIT
  if (seqin->native_code) {
    Scheme_Native_Lambda *ndata;
    Scheme_Native_Closure *nc, *na;
    Scheme_Lambda *data;
    Scheme_Object *val;
    GC_CAN_IGNORE Scheme_Object **runstack;
    GC_CAN_IGNORE mzshort *map;
    int j, jcnt;

    ndata = seqin->native_code;
    nc = (Scheme_Native_Closure *)scheme_make_native_case_closure(ndata);

    cnt = seqin->count;
    for (i = 0; i < cnt; i++) {
      val = seqin->array[i];
      if (!SCHEME_PROCP(val)) {
	data = (Scheme_Lambda *)val;
	na = (Scheme_Native_Closure *)scheme_make_native_closure(data->u.native_code);
	runstack = MZ_RUNSTACK;
	jcnt = data->closure_size;
	map = data->closure_map;
	for (j = 0; j < jcnt; j++) {
	  na->vals[j] = runstack[map[j]];
	}
	val = (Scheme_Object *)na;
      }
      nc->vals[i] = val;
    }

    return (Scheme_Object *)nc;
  }
#endif

  seqout = (Scheme_Case_Lambda *)
    scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
			 + (seqin->count - mzFLEX_DELTA) * sizeof(Scheme_Object *));
  seqout->so.type = scheme_case_closure_type;
  seqout->count = seqin->count;
  seqout->name = seqin->name;

  cnt = seqin->count;
  for (i = 0; i < cnt; i++) {
    if (SAME_TYPE(SCHEME_TYPE(seqin->array[i]), scheme_closure_type)) {
      /* An empty closure, created at compile time */
      seqout->array[i] = seqin->array[i];
    } else {
      Scheme_Object *lc;
      lc = scheme_make_closure(p, seqin->array[i], 1);
      seqout->array[i] = lc;
    }
  }

  return (Scheme_Object *)seqout;
}

Scheme_Object *scheme_make_envunbox(Scheme_Object *value)
{
  Scheme_Object *obj;

  obj = (Scheme_Object *)scheme_malloc_envunbox(sizeof(Scheme_Object*));
  SCHEME_ENVBOX_VAL(obj) = value;

  return obj;
}

static Scheme_Object *bangboxenv_execute(Scheme_Object *data)
/* A bangboxenv step is inserted by the compilation of `lambda' and
   `let' forms where an argument or bindings is set!ed in the body. */
{
  int pos = SCHEME_INT_VAL(SCHEME_PTR1_VAL(data));
  Scheme_Object *bb;

  data = SCHEME_PTR2_VAL(data);
  
  bb = scheme_make_envunbox(MZ_RUNSTACK[pos]);
  MZ_RUNSTACK[pos] = bb;

  return _scheme_tail_eval(data);
}

static Scheme_Object *begin0_execute(Scheme_Object *obj)
{
  Scheme_Object *v, **mv;
  int i, mc, apos;
  
  i = ((Scheme_Sequence *)obj)->count;

  v = _scheme_eval_linked_expr_multi(((Scheme_Sequence *)obj)->array[0]);
  i--;
  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
    Scheme_Thread *p = scheme_current_thread;
    mv = p->ku.multiple.array;
    mc = p->ku.multiple.count;
    if (SAME_OBJ(mv, p->values_buffer))
      p->values_buffer = NULL;
  } else {
    mv = NULL;
    mc = 1;
  }

  apos = 1;
  while (i--) {
    ignore_result(_scheme_eval_linked_expr_multi(((Scheme_Sequence *)obj)->array[apos++]));
  }

  if (mc != 1) {
    Scheme_Thread *p = scheme_current_thread;
    p->ku.multiple.array = mv;
    p->ku.multiple.count = mc;
  }

  return v;
}

/*========================================================================*/
/*                               closures                                 */
/*========================================================================*/

Scheme_Object *
scheme_make_closure(Scheme_Thread *p, Scheme_Object *code, int close)
     /* Creates a closure at run-time (or an empty closure at compile
        time; note that the byte-code marshaller in print.c can handle
        empty closures for that reason). */
{
  Scheme_Lambda *data;
  Scheme_Closure *closure;
  GC_CAN_IGNORE Scheme_Object **runstack;
  GC_CAN_IGNORE Scheme_Object **dest;
  GC_CAN_IGNORE mzshort *map;
  int i;

  data = (Scheme_Lambda *)code;

#ifdef MZ_USE_JIT
  if (data->u.native_code
      /* If the union points to a another Scheme_Lambda*, then it's not actually
         a pointer to native code. We must have a closure referenced from non-JITted code
         where the closure is also referenced by JITted code. */
      && !SAME_TYPE(SCHEME_TYPE(data->u.native_code), scheme_lambda_type)) {
    Scheme_Object *nc;

    nc = scheme_make_native_closure(data->u.native_code);

    if (close) {
      runstack = MZ_RUNSTACK;
      dest = ((Scheme_Native_Closure *)nc)->vals;
      map = data->closure_map;
      i = data->closure_size;
      
      /* Copy data into the closure: */
      while (i--) {
	dest[i] = runstack[map[i]];
      }
    }

    return nc;
  }
#endif

  i = data->closure_size;

  closure = (Scheme_Closure *)
    scheme_malloc_tagged(sizeof(Scheme_Closure)
			 + (i - mzFLEX_DELTA) * sizeof(Scheme_Object *));

  closure->so.type = scheme_closure_type;
  SCHEME_CLOSURE_CODE(closure) = data;

  if (!close || !i)
    return (Scheme_Object *)closure;

  runstack = MZ_RUNSTACK;
  dest = closure->vals;
  map = data->closure_map;

  /* Copy data into the closure: */
  while (i--) {
    dest[i] = runstack[map[i]];
  }

  return (Scheme_Object *)closure;
}

Scheme_Closure *scheme_malloc_empty_closure()
{
  Scheme_Closure *cl;

  cl = (Scheme_Closure *)scheme_malloc_tagged(sizeof(Scheme_Closure) - (mzFLEX_DELTA * sizeof(Scheme_Object *)));
  cl->so.type = scheme_closure_type;

  return cl;
}

void scheme_delay_load_closure(Scheme_Lambda *data)
{
  if (SCHEME_RPAIRP(data->body)) {
    Scheme_Object *v, *vinfo = NULL;

    v = SCHEME_CAR(data->body);
    if (SCHEME_VECTORP(v)) {
      /* Has info for delayed validation */
      vinfo = v;
      v = SCHEME_VEC_ELS(vinfo)[0];
    }
    v = scheme_load_delayed_code(SCHEME_INT_VAL(v), 
                                 (struct Scheme_Load_Delay *)SCHEME_CDR(data->body));
    data->body = v;
    
    if (vinfo) {
      scheme_validate_closure(NULL, 
                              (Scheme_Object *)data,
                              (char *)SCHEME_VEC_ELS(vinfo)[1], 
                              (Validate_TLS)SCHEME_VEC_ELS(vinfo)[2], 
                              SCHEME_INT_VAL(SCHEME_VEC_ELS(vinfo)[3]),
                              SCHEME_INT_VAL(SCHEME_VEC_ELS(vinfo)[5]),
                              (SCHEME_TRUEP(SCHEME_VEC_ELS(vinfo)[8])
                               ? (void *)SCHEME_VEC_ELS(vinfo)[8]
                               : NULL),
                              (SCHEME_TRUEP(SCHEME_VEC_ELS(vinfo)[9])
                               ? (mzshort *)(SCHEME_VEC_ELS(vinfo)[9])
                               : NULL),
                              SCHEME_INT_VAL(SCHEME_VEC_ELS(vinfo)[10]),
                              SCHEME_INT_VAL(SCHEME_VEC_ELS(vinfo)[6]),
                              (SCHEME_TRUEP(SCHEME_VEC_ELS(vinfo)[7])
                               ? (Scheme_Hash_Tree *)SCHEME_VEC_ELS(vinfo)[7]
                               : NULL),
                              (Scheme_Hash_Table **)SCHEME_VEC_ELS(vinfo)[11]);
    }
  }
}

/*========================================================================*/
/*                        main eval-apply loop                            */
/*========================================================================*/

/* This is the main evaluator loop. It's roughly of the form:

   while (1) {
     if (now-applying) {
       if (apply-type-1)
         ...
       else if (apply-type-2)
         ...
       else ...
     } else {
       switch (eval-type) {
         case eval-type-1:
           ...
         case eval-type-2:
           ...
         ...
       }
     }
   }

   The main use of #ifdefs is whether to use global variables for the
   Scheme stack pointer or to use local variables. Different
   architectures work best with different choices.

 */

#ifdef INSTRUMENT_PRIMITIVES
extern int g_print_prims;
#endif

#ifdef REGISTER_POOR_MACHINE
# define USE_LOCAL_RUNSTACK 0
# define DELAY_THREAD_RUNSTACK_UPDATE 0
#else
# define USE_LOCAL_RUNSTACK 1
# define DELAY_THREAD_RUNSTACK_UPDATE 1
#endif

Scheme_Object *
scheme_do_eval(Scheme_Object *obj, int num_rands, Scheme_Object **rands, 
		int get_value)
  /* If rands == MZ_RUNSTACK on entry, rands elements can be modified. */
{
  Scheme_Type type;
  Scheme_Object *v;
  int check_rands;
  GC_CAN_IGNORE Scheme_Object *tmpv; /* safe-for-space relies on GC_CAN_IGNORE */
  GC_CAN_IGNORE Scheme_Object **tmprands; /* safe-for-space relies on GC_CAN_IGNORE */
  GC_MAYBE_IGNORE_INTERIOR Scheme_Object **old_runstack, **runstack_base;
  GC_MAYBE_IGNORE_INTERIOR MZ_MARK_STACK_TYPE old_cont_mark_stack;
#if USE_LOCAL_RUNSTACK
  GC_MAYBE_IGNORE_INTERIOR Scheme_Object **runstack;
#endif
  MZ_MARK_STACK_TYPE pmstack = -1;
# define p scheme_current_thread

#ifdef DO_STACK_CHECK
# define SCHEME_CURRENT_PROCESS p
# include "mzstkchk.h"
  {
    return do_eval_stack_overflow(obj, num_rands, rands, get_value);
  }
#endif

#if USE_LOCAL_RUNSTACK
# define RUNSTACK runstack
# if DELAY_THREAD_RUNSTACK_UPDATE
#  define UPDATE_THREAD_RSPTR() (MZ_RUNSTACK = runstack)
#  define RUNSTACK_CHANGED() /**/
# else
#  define UPDATE_THREAD_RSPTR() /**/
#  define RUNSTACK_CHANGED() (MZ_RUNSTACK = runstack)
# endif
  runstack = MZ_RUNSTACK;
# define RESET_LOCAL_RUNSTACK() (runstack = MZ_RUNSTACK)
#else
# define RUNSTACK MZ_RUNSTACK
# define UPDATE_THREAD_RSPTR() /**/
# define RUNSTACK_CHANGED() /**/
# define RESET_LOCAL_RUNSTACK() /**/
#endif

#if 1
# define EVAL_SFS_CLEAR(runstack, obj)                                \
          if (SCHEME_GET_LOCAL_FLAGS(obj) == SCHEME_LOCAL_CLEAR_ON_READ) { \
            runstack[SCHEME_LOCAL_POS(obj)] = NULL;                   \
          }
# define SFS_CLEAR_RUNSTACK_ONE(runstack, pos) runstack[pos] = NULL
# define SFS_CLEAR_RUNSTACK(runstack, i, n)  for (i = n; i--; ) { SFS_CLEAR_RUNSTACK_ONE(runstack, i); }
#else
# define EVAL_SFS_CLEAR(rs, obj) /* empty */
# define SFS_CLEAR_RUNSTACK_ONE(runstack, pos) /* empty */
# define SFS_CLEAR_RUNSTACK(runstack, i, n)  /* empty */
#endif

#define RUNSTACK_START MZ_RUNSTACK_START

#define UPDATE_THREAD_RSPTR_FOR_GC() UPDATE_THREAD_RSPTR()
#define UPDATE_THREAD_RSPTR_FOR_ERROR() UPDATE_THREAD_RSPTR()

#define UPDATE_THREAD_RSPTR_FOR_PROC_MARK() UPDATE_THREAD_RSPTR()

#ifdef DEBUG_CHECK_STACK_FRAME_SIZE
  if (obj == SCHEME_TAIL_CALL_WAITING) {
    scheme_do_eval(SCHEME_EVAL_WAITING, 0, &obj, 0);
    return NULL;
  } else if (obj == SCHEME_EVAL_WAITING) {
    printf("%ld\n", (char *)rands - (char *)&obj);
    return NULL;
  }
#endif

  MZ_CONT_MARK_POS += 2;
  old_runstack = RUNSTACK;
  if (num_rands >= 0) {
    /* If we have a call with arguments at runstack, then we're
       allowed to recycle the argument part of the runstack. In fact,
       space safety may relies on reusing that space to clear argument
       values. */
    if (rands == RUNSTACK)
      runstack_base = RUNSTACK + num_rands;
    else
      runstack_base = RUNSTACK;
  } else
    runstack_base = RUNSTACK;
  old_cont_mark_stack = MZ_CONT_MARK_STACK;

  if (num_rands >= 0) {

    if ((RUNSTACK - RUNSTACK_START) < SCHEME_TAIL_COPY_THRESHOLD) {
      /* It's possible that a sequence of primitive _scheme_tail_apply()
	 calls will exhaust the Scheme stack. Watch out for that. */
      rands = evacuate_runstack(num_rands, rands, RUNSTACK);

      p->ku.k.p1 = (void *)obj;
      p->ku.k.i1 = num_rands;
      p->ku.k.p2 = (void *)rands;
      p->ku.k.i2 = -1;
      
      UPDATE_THREAD_RSPTR();
      if (rands == p->tail_buffer)
	make_tail_buffer_safe();
      MZ_CONT_MARK_POS -= 2;
      return scheme_enlarge_runstack(SCHEME_TAIL_COPY_THRESHOLD, (void *(*)(void))do_eval_k);
    }

    check_rands = num_rands;

  apply_top:

    /* DANGER: if rands == p->tail_buffer, we have to be careful to
       get the arguments out of the buffer before a GC occurs.
       (Otherwise, they'll be zeroed.) One way to make things safe for
       GC is to let rands have the buffer and create a new one. */

    type = SCHEME_TYPE(obj);

    if (type == scheme_prim_type) {
      GC_CAN_IGNORE Scheme_Primitive_Proc *prim;
      GC_CAN_IGNORE Scheme_Primitive_Closure_Proc *f;
      
#define VACATE_TAIL_BUFFER_USE_RUNSTACK() \
      if (rands == p->tail_buffer) {                                \
	if (num_rands < SCHEME_TAIL_COPY_THRESHOLD) {               \
	  int i;                                                    \
	  GC_CAN_IGNORE Scheme_Object **quick_rands;                \
                                                                    \
	  quick_rands = PUSH_RUNSTACK(p, RUNSTACK, num_rands);      \
	  RUNSTACK_CHANGED();                                       \
                                                                    \
	  for (i = num_rands; i--; ) {                              \
	    quick_rands[i] = rands[i];                              \
	  }                                                         \
	  rands = quick_rands;                                      \
	} else {                                                    \
	  UPDATE_THREAD_RSPTR_FOR_GC();                             \
	  make_tail_buffer_safe();                                  \
	}                                                           \
      }

      VACATE_TAIL_BUFFER_USE_RUNSTACK();

      UPDATE_THREAD_RSPTR();

      prim = (Scheme_Primitive_Proc *)obj;

      if (num_rands < prim->mina 
	  || (num_rands > prim->mu.maxa && prim->mina >= 0)) {
	scheme_wrong_count_m(prim->name, prim->mina, prim->mu.maxa,
			     num_rands, rands,
			     0);
	return NULL; /* Shouldn't get here */
      }

      f = prim->prim_val;
      tmprands = rands;
      rands = NULL; /* safe for space, since tmprands is ignored by the GC */
      v = f(num_rands, tmprands, (Scheme_Object *)prim);

      DEBUG_CHECK_TYPE(v);
    } else if (type == scheme_closure_type) {
      Scheme_Lambda *data;
      GC_CAN_IGNORE Scheme_Object **stack, **src;
      int i, has_rest, num_params;
      
      DO_CHECK_FOR_BREAK(p, UPDATE_THREAD_RSPTR_FOR_GC(); if (rands == p->tail_buffer) make_tail_buffer_safe(););

      data = SCHEME_CLOSURE_CODE(obj);

      if ((runstack_base - RUNSTACK_START) < data->max_let_depth) {
        rands = evacuate_runstack(num_rands, rands, RUNSTACK);

	if (rands == p->tail_buffer) {
	  UPDATE_THREAD_RSPTR_FOR_GC();
	  make_tail_buffer_safe();
	}

	p->ku.k.p1 = (void *)obj;
	p->ku.k.i1 = num_rands;
	p->ku.k.p2 = (void *)rands;
	p->ku.k.i2 = -1;

	UPDATE_THREAD_RSPTR();
	MZ_CONT_MARK_POS -= 2;
	v = (Scheme_Object *)scheme_enlarge_runstack(data->max_let_depth, (void *(*)(void))do_eval_k);
	MZ_CONT_MARK_POS += 2;
	goto returnv;
      }

      num_params = data->num_params;
      has_rest = SCHEME_LAMBDA_FLAGS(data) & LAMBDA_HAS_REST;
      
      if (num_params) {
	if (has_rest) {
	  int extra, n;

	  if (num_rands < (num_params - 1)) {
	    UPDATE_THREAD_RSPTR_FOR_ERROR();
	    /* note: scheme_wrong_count_m handles rands == p->tail_buffer */
	    scheme_wrong_count_m((const char *)obj, 
				 -1, -1,
				 num_rands, rands,
				 SCHEME_LAMBDA_FLAGS(data) & LAMBDA_IS_METHOD);
	    return NULL; /* Doesn't get here */
	  }

	  n = num_params - has_rest;
	  
	  RUNSTACK = runstack_base - num_params;
	  CHECK_RUNSTACK(p, RUNSTACK);
	  RUNSTACK_CHANGED();

	  extra = num_rands - n;
	  if (extra) {
	    Scheme_Object *rest_vals;
            GC_CAN_IGNORE Scheme_Object *pairs;

	    /* This is a special case: GC may be triggered, but
	       p->runstack does not point at everything that needs
	       to be kept if args are lower on the stack. 
	       That's what runstack_tmp_keep is for. Also, if
	       runstack_tmp_keep == tail_buffer, then the buffer
	       won't be zeroed. */
	    UPDATE_THREAD_RSPTR_FOR_GC();
	    p->runstack_tmp_keep = rands;

	    rest_vals = scheme_null;
	    for (i = num_rands - 1; i >= n; --i) {
	      pairs = scheme_alloc_object();
	      pairs->type = scheme_pair_type;
	      SCHEME_CDR(pairs) = rest_vals;
	      SCHEME_CAR(pairs) = rands[i];
	      rest_vals = pairs;
	    }

	    p->runstack_tmp_keep = NULL;

	    stack = RUNSTACK;
	    stack[n] = rest_vals;
	    while (n--) {
	      stack[n] = rands[n];
	    }
	  } else {
	    stack = RUNSTACK;
	    /* Possibly, but not necessarily, rands > stack: */
	    if ((uintptr_t)rands > (uintptr_t)stack) {
	      int i;
	      for (i = 0; i < n; i++) {
		stack[i] = rands[i];
	      }
	      stack[n] = scheme_null;
	    } else {
	      stack[n] = scheme_null;
	      while (n--) {
		stack[n] = rands[n];
	      }
	    }
	  }
	} else {
	  if (num_rands != num_params) {
	    UPDATE_THREAD_RSPTR_FOR_ERROR();
	    /* note: scheme_wrong_count_m handles rands == p->tail_buffer */
	    scheme_wrong_count_m((const char *)obj,
				 -1, -1,
				 num_rands, rands,
				 SCHEME_LAMBDA_FLAGS(data) & LAMBDA_IS_METHOD);
	    return NULL; /* Doesn't get here */
	  }
	
          stack = RUNSTACK = runstack_base - num_params;
          CHECK_RUNSTACK(p, RUNSTACK);
          RUNSTACK_CHANGED();
          
          if (rands != stack) {
            int n = num_params; 
            while (n--) {
              stack[n] = rands[n];
            }
          }
	}
      } else {
	if (num_rands) {
	  if (has_rest) {
	    /* 0 params and has_rest => (lambda args E) where args is not in E,
	       so accept any number of arguments and ignore them. */
	    
	  } else {
	    UPDATE_THREAD_RSPTR_FOR_ERROR();
	    /* note: scheme_wrong_count handles rands == p->tail_buffer */
	    scheme_wrong_count((const char *)obj, -1, -1, num_rands, rands);
	    return NULL; /* Doesn't get here */
	  }
	}
	RUNSTACK = runstack_base;
	RUNSTACK_CHANGED();
      }
      
      {
	int n = data->closure_size;
      
	if (n) {
	  src = SCHEME_CLOSURE_ENV(obj);
	  stack = PUSH_RUNSTACK(p, RUNSTACK, n);
	  RUNSTACK_CHANGED();

	  while (n--) {
	    stack[n] = src[n];
	  }
	}
      }

      obj = data->body;

      if (SCHEME_RPAIRP(obj)) {
        UPDATE_THREAD_RSPTR_FOR_GC();
        make_tail_buffer_safe();
        scheme_delay_load_closure(data);
        obj = data->body;
      }

      if (pmstack >= 0) {
        intptr_t segpos = ((intptr_t)pmstack) >> SCHEME_LOG_MARK_SEGMENT_SIZE;
        intptr_t pos = ((intptr_t)pmstack) & SCHEME_MARK_SEGMENT_MASK;
        GC_CAN_IGNORE Scheme_Cont_Mark *pm = NULL;

        pm = p->cont_mark_stack_segments[segpos] + pos;

	if (!pm->cache)
	  pm->val = data->name; 
	else {
	  /* Need to clear caches, so do it the slow way */
	  UPDATE_THREAD_RSPTR_FOR_PROC_MARK();
	  pmstack = scheme_set_cont_mark(scheme_stack_dump_key, data->name); 
	}
      } else { 
	/* Allocate a new mark record: */
	intptr_t segpos = ((intptr_t)MZ_CONT_MARK_STACK) >> SCHEME_LOG_MARK_SEGMENT_SIZE;
	if (segpos >= p->cont_mark_seg_count) {
	  UPDATE_THREAD_RSPTR_FOR_PROC_MARK();
	  pmstack = scheme_set_cont_mark(scheme_stack_dump_key, data->name); 
	} else {
	  intptr_t pos = ((intptr_t)MZ_CONT_MARK_STACK) & SCHEME_MARK_SEGMENT_MASK;
          GC_CAN_IGNORE Scheme_Cont_Mark *pm;
	  GC_CAN_IGNORE Scheme_Cont_Mark *seg;
	
          pmstack = MZ_CONT_MARK_STACK;

	  seg = p->cont_mark_stack_segments[segpos];
	  pm = seg + pos;
	  MZ_CONT_MARK_STACK++;

	  pm->key = scheme_stack_dump_key;
	  pm->val = data->name;
	  pm->pos = MZ_CONT_MARK_POS;
	  pm->cache = NULL;
	}
      }

      goto eval_top;
    } else if (type == scheme_case_closure_type) {
      Scheme_Case_Lambda *seq;
      Scheme_Lambda *data;
      
      int i;
      
      seq = (Scheme_Case_Lambda *)obj;
      for (i = 0; i < seq->count; i++) {
	data = SCHEME_CLOSURE_CODE(seq->array[i]);
	if ((!(SCHEME_LAMBDA_FLAGS(data) & LAMBDA_HAS_REST) 
	     && (data->num_params == num_rands))
	    || ((SCHEME_LAMBDA_FLAGS(data) & LAMBDA_HAS_REST)
		&& (data->num_params - 1 <= num_rands))) {
	  obj = seq->array[i];
	  goto apply_top;
	}
      }
      
      UPDATE_THREAD_RSPTR_FOR_ERROR();
      /* note: scheme_wrong_count handles rands == p->tail_buffer */
      scheme_wrong_count((char *)seq, -1, -1, num_rands, rands);

      return NULL; /* Doesn't get here. */
#ifdef MZ_USE_JIT
    } else if (type == scheme_native_closure_type) {
      GC_CAN_IGNORE Scheme_Native_Lambda *data;

      VACATE_TAIL_BUFFER_USE_RUNSTACK();

      UPDATE_THREAD_RSPTR();
      
      DO_CHECK_FOR_BREAK(p, );

      /* See also _apply_native(), which effectively copies this code. */

      data = ((Scheme_Native_Closure *)obj)->code;

      /* Enlarge the runstack? This max_let_depth is in bytes instead of words. */
      if ((uintptr_t)data->max_let_depth > ((uintptr_t)RUNSTACK - (uintptr_t)RUNSTACK_START)) {
	p->ku.k.p1 = (void *)obj;
	p->ku.k.i1 = num_rands;
	p->ku.k.p2 = (void *)rands;
	p->ku.k.i2 = -1;

	MZ_CONT_MARK_POS -= 2;
	v = (Scheme_Object *)scheme_enlarge_runstack(data->max_let_depth / sizeof(void *), 
						     (void *(*)(void))do_eval_native_k);
	MZ_CONT_MARK_POS += 2;
	goto returnv;
      }

      tmpv = obj;
      obj = NULL; /* safe for space, since tmpv is ignored by the GC */
      tmprands = rands;
      if (rands != old_runstack)
        rands = NULL; /* safe for space, since tmprands is ignored by the GC */
      v = data->start_code(tmpv, num_rands, tmprands EXTRA_NATIVE_ARGUMENT);

      if (v == SCHEME_TAIL_CALL_WAITING) {
        /* [TC-SFS]; see schnapp.inc */
        if (rands == old_runstack) {
          int i;
          for (i = 0; i < num_rands; i++) { rands[i] = NULL; }
        }
      }

      DEBUG_CHECK_TYPE(v);
#endif
    } else if (type == scheme_cont_type) {
      UPDATE_THREAD_RSPTR();
      v = scheme_jump_to_continuation(obj, num_rands, rands, old_runstack, 1);
    } else if (type == scheme_escaping_cont_type) {
      UPDATE_THREAD_RSPTR();
      scheme_escape_to_continuation(obj, num_rands, rands, NULL);
      return NULL;
    } else if ((type == scheme_proc_struct_type)
               || ((type == scheme_proc_chaperone_type)
                   /* Chaperone is for struct fields, not function arguments --- but
                      the chaperone may guard access to the function as a field inside
                      the struct. We'll need to keep track of the original object
                      as we unwrap to discover procedure chaperones. */
		   && SCHEME_REDIRECTS_STRUCTP(((Scheme_Chaperone *) obj)->redirects))
               /* A raw pair is from scheme_apply_chaperone(), propagating the
                  original object for an applicable structure. */
               || (type == scheme_raw_pair_type)) {
      int is_method;
      Scheme_Object *orig_obj;

      orig_obj = obj;

      while (1) {
        /* Like the apply loop around this one, but we need
           to keep track of orig_obj until we get down to the
           structure. */

        if (SCHEME_RPAIRP(obj)) {
          orig_obj = SCHEME_CDR(obj);
          obj = SCHEME_CAR(obj);
        }

        type = SCHEME_TYPE(obj);
        if (type == scheme_proc_struct_type) {
          VACATE_TAIL_BUFFER_USE_RUNSTACK();

          UPDATE_THREAD_RSPTR_FOR_ERROR(); /* in case */

          v = obj;
          obj = scheme_extract_struct_procedure(orig_obj, check_rands, rands, &is_method);
          if (is_method) {
            /* Have to add an extra argument to the front of rands */
            if ((rands == RUNSTACK) && (RUNSTACK != RUNSTACK_START)){
              /* Common case: we can just push self onto the front: */
              rands = PUSH_RUNSTACK(p, RUNSTACK, 1);
              rands[0] = v;
            } else {
              int i;
              Scheme_Object **a;

              if (p->tail_buffer && (num_rands < p->tail_buffer_size)) {
                /* Use tail-call buffer. Shift in such a way that this works if
                   rands == p->tail_buffer */
                a = p->tail_buffer;
              } else {
                /* Uncommon general case --- allocate an array */
                UPDATE_THREAD_RSPTR_FOR_GC();
                a = MALLOC_N(Scheme_Object *, num_rands + 1);
              }

              for (i = num_rands; i--; ) {
                a[i + 1] = rands[i];
              }
              a[0] = v;
              rands = a;
            }
            num_rands++;
          }

          DO_CHECK_FOR_BREAK(p, UPDATE_THREAD_RSPTR_FOR_GC(); if (rands == p->tail_buffer) make_tail_buffer_safe(););

          /* After we check arity once, no need to check again
             (which would lead to O(n^2) checking for nested
             struct procs): */
          check_rands = -1;

          goto apply_top;
        } else {
          if (SCHEME_REDIRECTS_STRUCTP(((Scheme_Chaperone *)obj)->redirects))
            obj = ((Scheme_Chaperone *)obj)->prev;
          else if (SAME_TYPE(SCHEME_TYPE(((Scheme_Chaperone *)obj)->redirects), scheme_nack_guard_evt_type))
            /* Chaperone is for evt, not function arguments */
            obj = ((Scheme_Chaperone *)obj)->prev;
          else {
            /* Chaperone is for function arguments */
            VACATE_TAIL_BUFFER_USE_RUNSTACK();
            UPDATE_THREAD_RSPTR();
            tmprands = rands;
            rands = NULL; /* safe for space, since tmprands is ignored by the GC */
            v = scheme_apply_chaperone(scheme_make_raw_pair(obj, orig_obj), num_rands, tmprands, NULL, 0);
            
            if (SAME_OBJ(v, SCHEME_TAIL_CALL_WAITING)) {
              /* Need to stay in this loop, because a tail-call result must
                 be a tail call to an unwrapped layer, so we'll eventually
                 need to use orig_obj. */
              obj = p->ku.apply.tail_rator;
              num_rands = p->ku.apply.tail_num_rands;
              if (check_rands != -1) check_rands = num_rands;
              rands = p->ku.apply.tail_rands;
              p->ku.apply.tail_rator = NULL;
              p->ku.apply.tail_rands = NULL;
              RUNSTACK = runstack_base;
              RUNSTACK_CHANGED();
            } else {
              break;
            }
          }
        }
      }
    } else if (type == scheme_proc_chaperone_type) {
      if (SAME_TYPE(SCHEME_TYPE(((Scheme_Chaperone *)obj)->redirects), scheme_nack_guard_evt_type)) {
        /* Chaperone is for evt, not function arguments */
        obj = ((Scheme_Chaperone *)obj)->prev;
        check_rands = num_rands;
        goto apply_top;
      } else {
        /* Chaperone is for function arguments */
        VACATE_TAIL_BUFFER_USE_RUNSTACK();
        UPDATE_THREAD_RSPTR();
        tmprands = rands;
        rands = NULL; /* safe for space, since tmprands is ignored by the GC */
        v = scheme_apply_chaperone(obj, num_rands, tmprands, NULL, 0);
      }
    } else if (type == scheme_closed_prim_type) {
      GC_CAN_IGNORE Scheme_Closed_Primitive_Proc *prim;
      
      DO_CHECK_FOR_BREAK(p, UPDATE_THREAD_RSPTR_FOR_GC(); if (rands == p->tail_buffer) make_tail_buffer_safe(););

      VACATE_TAIL_BUFFER_USE_RUNSTACK();

      UPDATE_THREAD_RSPTR();

      prim = (Scheme_Closed_Primitive_Proc *)obj;
      
      if (num_rands < prim->mina 
	  || (num_rands > prim->maxa && prim->maxa >= 0)) {
	scheme_wrong_count_m(prim->name, prim->mina, prim->maxa, 
			     num_rands, rands,
			     0);
	return NULL; /* Shouldn't get here */
      }

      tmprands = rands;
      if (rands != old_runstack)
        rands = NULL; /* safe for space, since tmprands is ignored by the GC */
      v = prim->prim_val(prim->data, num_rands, tmprands);

      if (v == SCHEME_TAIL_CALL_WAITING) {
        /* [TC-SFS]; see schnapp.inc */
        if (rands == old_runstack) {
          int i;
          for (i = 0; i < num_rands; i++) { rands[i] = NULL; }
        }
      }

      DEBUG_CHECK_TYPE(v);
    } else {
      UPDATE_THREAD_RSPTR_FOR_ERROR();
      if (rands == p->tail_buffer)
	make_tail_buffer_safe();
      scheme_wrong_rator(obj, num_rands, rands);
      return NULL; /* Doesn't get here. */
    }
  } else {

  eval_top:

    if (SCHEME_INTP(obj)) {
      v = obj;
      goto returnv_never_multi;
    }

    type = _SCHEME_TYPE(obj);
    switch (type)
      {
      case scheme_toplevel_type:
	{
          /* Make sure that the GC can ignore tmp: */
#define global_lookup(prefix, _obj, tmp)                                \
          tmp = RUNSTACK[SCHEME_TOPLEVEL_DEPTH(_obj)];                  \
          tmp = ((Scheme_Prefix *)tmp)->a[SCHEME_TOPLEVEL_POS(_obj)];   \
	  tmp = (Scheme_Object *)(SCHEME_VAR_BUCKET(tmp))->val;         \
	  if (!tmp) {                                                   \
            UPDATE_THREAD_RSPTR_FOR_ERROR();                            \
            unbound_global(_obj);                                       \
            return NULL;                                                \
	  }                                                             \
	  prefix tmp

	  global_lookup(v = , obj, v);  
	  goto returnv_never_multi;
	}
      case scheme_static_toplevel_type:
	{
          obj = SCHEME_STATIC_TOPLEVEL_PREFIX(obj)->a[SCHEME_TOPLEVEL_POS(obj)];
	  v = (Scheme_Object *)(SCHEME_VAR_BUCKET(obj))->val;
	  if (!v) {
            UPDATE_THREAD_RSPTR_FOR_ERROR();
            scheme_unbound_global((Scheme_Bucket *)obj);
            return NULL;
	  }
	  goto returnv_never_multi;
	}
      case scheme_local_type:
	{
	  v = RUNSTACK[SCHEME_LOCAL_POS(obj)];
          EVAL_SFS_CLEAR(RUNSTACK, obj);
          goto returnv_never_multi;
	}
      case scheme_local_unbox_type:
	{
	  v = SCHEME_ENVBOX_VAL(RUNSTACK[SCHEME_LOCAL_POS(obj)]);
          EVAL_SFS_CLEAR(RUNSTACK, obj);
          goto returnv_never_multi;
	}
      case scheme_application_type:
	{
	  Scheme_App_Rec *app;
	  GC_MAYBE_IGNORE_INTERIOR Scheme_Object **randsp;
	  Scheme_Object **stack;
	  int k;
	  int d_evals;
#ifdef MZ_XFORM
# define GET_FIRST_EVAL ((char *)app)[d_evals]
#else
	  char *evals;	  
	  Scheme_Object **args;
# define GET_FIRST_EVAL evals[0]
#endif

	  app = (Scheme_App_Rec *)obj;
	  num_rands = app->num_args;
	  
	  d_evals = (sizeof(Scheme_App_Rec) 
                     + ((num_rands + 1 - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
#ifndef MZ_XFORM
	  evals = ((char *)obj) + d_evals;
#endif

	  obj = app->args[0];
	  
	  stack = PUSH_RUNSTACK(p, RUNSTACK, num_rands);
	  RUNSTACK_CHANGED();
	  UPDATE_THREAD_RSPTR();
          SFS_CLEAR_RUNSTACK(RUNSTACK, k, num_rands);

	  /* Inline local & global variable lookups for speed */
	  switch (GET_FIRST_EVAL) {
	  case SCHEME_EVAL_CONSTANT:
	    break;
	  case SCHEME_EVAL_GLOBAL:
	    global_lookup(obj =, obj, tmpv);
	    break;
	  case SCHEME_EVAL_LOCAL:
            {
              tmpv = stack[SCHEME_LOCAL_POS(obj)];
              EVAL_SFS_CLEAR(stack, obj);
              obj = tmpv;
            }
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
            {
              tmpv = SCHEME_ENVBOX_VAL(stack[SCHEME_LOCAL_POS(obj)]);
              EVAL_SFS_CLEAR(stack, obj);
              obj = tmpv;
            }
	    break;
	  default:
	    obj = _scheme_eval_linked_expr_wp(obj, p);
	    break;
	  }

	  if (num_rands) {
#ifdef MZ_XFORM
	    int evalpos = 1;
#endif

	    rands = stack;
	
	    /* Inline local & global variable lookups for speed */
#ifdef MZ_XFORM
# define GET_NEXT_EVAL ((char *)app)[d_evals + evalpos++]	    
# define GET_NEXT_ARG app->args[evalpos]
#else
	    evals++;
	    args = app->args + 1;
# define GET_NEXT_EVAL *(evals++)
# define GET_NEXT_ARG *(args++)
#endif
	    randsp = rands;
	    for (k = num_rands; k--; ) {
	      v = GET_NEXT_ARG;
	      switch (GET_NEXT_EVAL) {
	      case SCHEME_EVAL_CONSTANT:
		*(randsp++) = v;
		break;
	      case SCHEME_EVAL_GLOBAL:
		global_lookup(*(randsp++) =, v, tmpv);
		break;
	      case SCHEME_EVAL_LOCAL:
		*(randsp++) = stack[SCHEME_LOCAL_POS(v)];
                EVAL_SFS_CLEAR(stack, v);
		break;
	      case SCHEME_EVAL_LOCAL_UNBOX:
		*(randsp++) = SCHEME_ENVBOX_VAL(stack[SCHEME_LOCAL_POS(v)]);
                EVAL_SFS_CLEAR(stack, v);
		break;
	      default:
		{
		  GC_CAN_IGNORE Scheme_Object *er;
		  er = _scheme_eval_linked_expr_wp(v, p);
		  *(randsp++) = er;
		}
		break;
	      }

	      DEBUG_CHECK_TYPE(randsp[-1]);
	    }
	  } else
	    rands = &zero_rands_ptr;

          check_rands = num_rands;
	  goto apply_top;
	}

      case scheme_application2_type:
	{
	  Scheme_App2_Rec *app;
	  GC_CAN_IGNORE Scheme_Object *arg;
	  short flags;

	  app = (Scheme_App2_Rec *)obj;
	  
	  obj = app->rator;
	  flags = SCHEME_APPN_FLAGS(app);

	  rands = PUSH_RUNSTACK(p, RUNSTACK, 1);
	  RUNSTACK_CHANGED();
	  UPDATE_THREAD_RSPTR();
          SFS_CLEAR_RUNSTACK_ONE(RUNSTACK, 0);
	  
	  /* Inline local & global variable lookups for speed */
	  switch (flags & 0x7) {
	  case SCHEME_EVAL_CONSTANT:
	    break;
	  case SCHEME_EVAL_GLOBAL:
            {
              global_lookup(obj =, obj, tmpv);
            }
	    break;
	  case SCHEME_EVAL_LOCAL:
            {
              tmpv = rands[SCHEME_LOCAL_POS(obj)];
              EVAL_SFS_CLEAR(rands, obj);
              obj = tmpv;
            }
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
            {
              tmpv = SCHEME_ENVBOX_VAL(rands[SCHEME_LOCAL_POS(obj)]);
              EVAL_SFS_CLEAR(rands, obj);
              obj = tmpv;
            }
	    break;
	  default:
	    obj = _scheme_eval_linked_expr_wp(obj, p);
	    break;
	  }

	  arg = app->rand;

	  switch ((flags >> 3) & 0x7) {
	  case SCHEME_EVAL_CONSTANT:
	    break;
	  case SCHEME_EVAL_GLOBAL:
            {
              global_lookup(arg =, arg, tmpv);
            }
	    break;
	  case SCHEME_EVAL_LOCAL:
            {
              tmpv = rands[SCHEME_LOCAL_POS(arg)];
              EVAL_SFS_CLEAR(rands, arg);
              arg = tmpv;
            }
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
            {
              tmpv = SCHEME_ENVBOX_VAL(rands[SCHEME_LOCAL_POS(arg)]);
              EVAL_SFS_CLEAR(rands, arg);
              arg = tmpv;
            }
	    break;
	  default:
	    arg = _scheme_eval_linked_expr_wp(arg, p);
	    break;
	  }

	  rands[0] = arg;
	  num_rands = 1;

          check_rands = num_rands;
	  goto apply_top;
	}
	
      case scheme_application3_type:
	{
	  Scheme_App3_Rec *app;
	  GC_CAN_IGNORE Scheme_Object *arg;
	  short flags;

	  app = (Scheme_App3_Rec *)obj;
	  
	  obj = app->rator;
	  flags = SCHEME_APPN_FLAGS(app);

	  rands = PUSH_RUNSTACK(p, RUNSTACK, 2);
	  RUNSTACK_CHANGED();
	  UPDATE_THREAD_RSPTR();
          SFS_CLEAR_RUNSTACK_ONE(RUNSTACK, 0);
          SFS_CLEAR_RUNSTACK_ONE(RUNSTACK, 1);

	  /* Inline local & global variable lookups for speed */
	  switch (flags & 0x7) {
	  case SCHEME_EVAL_CONSTANT:
	    break;
	  case SCHEME_EVAL_GLOBAL:
	    global_lookup(obj =, obj, tmpv);
	    break;
	  case SCHEME_EVAL_LOCAL:
	    tmpv = rands[SCHEME_LOCAL_POS(obj)];
            EVAL_SFS_CLEAR(rands, obj);
            obj = tmpv;
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    tmpv = SCHEME_ENVBOX_VAL(rands[SCHEME_LOCAL_POS(obj)]);
            EVAL_SFS_CLEAR(rands, obj);
            obj = tmpv;
	    break;
	  default:
	    obj = _scheme_eval_linked_expr_wp(obj, p);
	    break;
	  }

	  arg = app->rand1;

	  switch ((flags >> 3) & 0x7) {
	  case SCHEME_EVAL_CONSTANT:
	    break;
	  case SCHEME_EVAL_GLOBAL:
	    global_lookup(arg =, arg, tmpv);
	    break;
	  case SCHEME_EVAL_LOCAL:
	    tmpv = rands[SCHEME_LOCAL_POS(arg)];
            EVAL_SFS_CLEAR(rands, arg);
            arg = tmpv;
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    tmpv = SCHEME_ENVBOX_VAL(rands[SCHEME_LOCAL_POS(arg)]);
            EVAL_SFS_CLEAR(rands, arg);
            arg = tmpv;
	    break;
	  default:
	    arg = _scheme_eval_linked_expr_wp(arg, p);
	    break;
	  }

	  rands[0] = arg;

	  arg = app->rand2;

	  switch ((flags >> 6) & 0x7) {
	  case SCHEME_EVAL_CONSTANT:
	    break;
	  case SCHEME_EVAL_GLOBAL:
	    global_lookup(arg =, arg, tmpv);
	    break;
	  case SCHEME_EVAL_LOCAL:
	    tmpv = rands[SCHEME_LOCAL_POS(arg)];
            EVAL_SFS_CLEAR(rands, arg);
            arg = tmpv;
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    tmpv = SCHEME_ENVBOX_VAL(rands[SCHEME_LOCAL_POS(arg)]);
            EVAL_SFS_CLEAR(rands, arg);
            arg = tmpv;
	    break;
	  default:
	    arg = _scheme_eval_linked_expr_wp(arg, p);
	    break;
	  }

	  rands[1] = arg;

	  num_rands = 2;

          check_rands = num_rands;
	  goto apply_top;
	}
      
      case scheme_sequence_type:
	{
	  int cnt;
	  int i;
	  
	  cnt = ((Scheme_Sequence *)obj)->count - 1;
	  
	  UPDATE_THREAD_RSPTR();
	  for (i = 0; i < cnt; i++) {
	    ignore_result(_scheme_eval_linked_expr_multi_wp(((Scheme_Sequence *)obj)->array[i], p));
	  }

	  obj = ((Scheme_Sequence *)obj)->array[cnt];
	  goto eval_top;
	}
      case scheme_branch_type:
	{
	  UPDATE_THREAD_RSPTR();
	  obj = (NOT_SAME_OBJ(_scheme_eval_linked_expr_wp(((Scheme_Branch_Rec *)obj)->test, p), 
			      scheme_false)
		 ? ((Scheme_Branch_Rec *)obj)->tbranch 
		 : ((Scheme_Branch_Rec *)obj)->fbranch);

	  goto eval_top;
	}
      case scheme_lambda_type:
	UPDATE_THREAD_RSPTR();
	v = scheme_make_closure(p, obj, 1);
	goto returnv_never_multi;

      case scheme_let_value_type:
	{
	  GC_CAN_IGNORE Scheme_Let_Value *lv;
	  GC_CAN_IGNORE Scheme_Object *value, **values;
	  int i, c, ab;

	  lv = (Scheme_Let_Value *)obj;

	  c = lv->count;

	  i = lv->position;
	  ab = SCHEME_LET_VALUE_AUTOBOX(lv);
	  value = lv->value;
	  obj = lv->body;

	  UPDATE_THREAD_RSPTR();

	  if (c == 1) {
	    value = _scheme_eval_linked_expr_wp(value, p);
	    if (ab)
	      SCHEME_ENVBOX_VAL(RUNSTACK[i]) = value;
	    else
	      RUNSTACK[i] = value;
	  } else {
	    int c2;
	    GC_CAN_IGNORE Scheme_Object **stack;

	    value = _scheme_eval_linked_expr_multi_wp(value, p);
	    c2 = (SAME_OBJ(value, SCHEME_MULTIPLE_VALUES) ? p->ku.multiple.count : 1);
	    if (c2 != c) {
	      scheme_wrong_return_arity(NULL, c, c2, 
					(c2 == 1) ? (Scheme_Object **)value : p->ku.multiple.array,
					"\n  in: local-binding form");
	      return NULL;
	    }

	    /* Precise GC: values++ is ok because we exit the block
	       before any GC can happen. Also, GC would zero `values'
	       if it turns out to be p->values_buffer. */

	    values = p->ku.multiple.array;
	    p->ku.multiple.array = NULL;
	    stack = RUNSTACK;
	    if (ab) {
	      while (c--) {
		SCHEME_ENVBOX_VAL(stack[i]) = *values;
		values++;
		i++;
	      }
	    } else {
	      while (c--) {
		stack[i] = *values;
		values++;
		i++;
	      }
	    }
	  }

	  goto eval_top;
	}

      case scheme_let_void_type:
	{
	  GC_CAN_IGNORE Scheme_Let_Void *lv;
	  int c;

	  lv = (Scheme_Let_Void *)obj;
	  c = lv->count;
	  obj = lv->body;

	  PUSH_RUNSTACK(p, RUNSTACK, c);
	  RUNSTACK_CHANGED();

	  if (SCHEME_LET_VOID_AUTOBOX(lv)) {
	    GC_MAYBE_IGNORE_INTERIOR Scheme_Object **stack = RUNSTACK;

	    UPDATE_THREAD_RSPTR_FOR_GC();

	    while (c--) {
	      GC_CAN_IGNORE Scheme_Object *ub;
	      ub = scheme_make_envunbox(scheme_undefined);
	      stack[c] = ub;
	    }
	  }

	  goto eval_top;
	}

      case scheme_letrec_type:
	{
	  /* Macro instead of var for efficient precise GC conversion */
# define l ((Scheme_Letrec *)obj)
	  Scheme_Object **a;
          GC_MAYBE_IGNORE_INTERIOR Scheme_Object **stack;
	  int i;

	  stack = RUNSTACK;
	  a = l->procs;
	  i = l->count;
	  
	  UPDATE_THREAD_RSPTR_FOR_GC();

	  /* Create unfinished closures */
	  while (i--) {
	    Scheme_Object *uc;
	    uc = scheme_make_closure(p, a[i], 0);
	    stack[i] = uc;
	  }

	  /* Close them: */
	  i = l->count;
	  while (i--) {
	    GC_CAN_IGNORE Scheme_Object *clos;
	    GC_CAN_IGNORE Scheme_Object **dest;
	    GC_CAN_IGNORE mzshort *map;
	    GC_CAN_IGNORE Scheme_Lambda *data;
	    int j;

	    clos = stack[i];

#ifdef MZ_USE_JIT
	    if (SAME_TYPE(_SCHEME_TYPE(clos), scheme_closure_type)) {
	      dest = ((Scheme_Closure *)clos)->vals;
	    } else {
	      dest = ((Scheme_Native_Closure *)clos)->vals;
	    }
#else
	    dest = ((Scheme_Closure *)clos)->vals;
#endif
	    
	    data = (Scheme_Lambda *)a[i];
	      
	    map = data->closure_map;
	    j = data->closure_size;

	    /* Beware - dest points to the middle of a block */

	    while (j--) {
	      dest[j] = stack[map[j]];
	    }
	  }

	  obj = l->body;
# undef l
	  goto eval_top;
	}

      case scheme_let_one_type:
	{
	  /* Macro instead of var for efficient precise GC conversion */
# define lo ((Scheme_Let_One *)obj)

	  PUSH_RUNSTACK(p, RUNSTACK, 1);
	  RUNSTACK_CHANGED();

          /* SFS pass may set LET_ONE_UNUSED, but not for the
             variable cases; in the constant case, the constant
             is #f, so it's ok to push it anyway. */

	  switch (SCHEME_LET_EVAL_TYPE(lo) & 0x7) {
	  case SCHEME_EVAL_CONSTANT:
	    RUNSTACK[0] = lo->value;
	    break;
	  case SCHEME_EVAL_GLOBAL:
	    {
	      global_lookup(RUNSTACK[0] =, lo->value, tmpv);
	    }
	    break;
	  case SCHEME_EVAL_LOCAL:
	    RUNSTACK[0] = RUNSTACK[SCHEME_LOCAL_POS(lo->value)];
            EVAL_SFS_CLEAR(RUNSTACK, lo->value);
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    RUNSTACK[0] = SCHEME_ENVBOX_VAL(RUNSTACK[SCHEME_LOCAL_POS(lo->value)]);
            EVAL_SFS_CLEAR(RUNSTACK, lo->value);
	    break;
	  default:
	    UPDATE_THREAD_RSPTR();
	    {
	      GC_CAN_IGNORE Scheme_Object *val;
              SFS_CLEAR_RUNSTACK_ONE(RUNSTACK, 0);
	      val = _scheme_eval_linked_expr_wp(lo->value, p);
              if (!(SCHEME_LET_EVAL_TYPE(lo) & LET_ONE_UNUSED))
                RUNSTACK[0] = val;
	    }
	    break;
	  }

	  obj = lo->body;
# undef lo
	  goto eval_top;
	}
      
      case scheme_with_cont_mark_type:
	{
	  /* Macro instead of var for efficient precise GC conversion */
# define wcm ((Scheme_With_Continuation_Mark *)obj)
	  Scheme_Object *key;
	  GC_CAN_IGNORE Scheme_Object *val;
	  
	  UPDATE_THREAD_RSPTR();
	  key = wcm->key;
	  if (SCHEME_TYPE(key) < _scheme_values_types_)
	    key = _scheme_eval_linked_expr_wp(key, p);
	  val = wcm->val;
	  if (SCHEME_TYPE(val) < _scheme_values_types_)
	    val = _scheme_eval_linked_expr_wp(val, p);

          if (SCHEME_NP_CHAPERONEP(key)
              && SCHEME_CONTINUATION_MARK_KEYP(SCHEME_CHAPERONE_VAL(key))) {
            val = scheme_chaperone_do_continuation_mark("with-continuation-mark", 0, key, val);
            key = SCHEME_CHAPERONE_VAL(key);
          }

	  scheme_set_cont_mark(key, val);

	  obj = wcm->body;
# undef wcm
	  goto eval_top;
	}

      case scheme_define_values_type:
        {
          UPDATE_THREAD_RSPTR();
          v = define_values_execute(obj);
          break;
        }
      case scheme_inline_variant_type:
        {
          obj = SCHEME_VEC_ELS(obj)[0];
          goto eval_top;
        }
      case scheme_set_bang_type:
        {
          UPDATE_THREAD_RSPTR();
          v = set_execute(obj);
          break;
        }
      case scheme_boxenv_type:
        {
          UPDATE_THREAD_RSPTR();
          v = bangboxenv_execute(obj);
          break;
        }
      case scheme_begin0_sequence_type:
        {
          UPDATE_THREAD_RSPTR();
          v = begin0_execute(obj);
          break;
        }
      case scheme_varref_form_type:
        {
          UPDATE_THREAD_RSPTR();
          v = ref_execute(obj);
          break;
        }
      case scheme_apply_values_type:
        {
          UPDATE_THREAD_RSPTR();
          v = apply_values_execute(obj);
          break;
        }
      case scheme_with_immed_mark_type:
        {
# define wcm ((Scheme_With_Continuation_Mark *)obj)
          Scheme_Object *mark_key;
          GC_CAN_IGNORE Scheme_Object *mark_val;

          mark_key = wcm->key;
          if (SCHEME_TYPE(mark_key) < _scheme_values_types_) {
            UPDATE_THREAD_RSPTR();
            mark_key = _scheme_eval_linked_expr_wp(mark_key, p);
          }

          mark_val = wcm->val;
          if (SCHEME_TYPE(mark_val) < _scheme_values_types_) {
            UPDATE_THREAD_RSPTR();
            mark_val = _scheme_eval_linked_expr_wp(mark_val, p);
          }

          UPDATE_THREAD_RSPTR();
          mark_val = scheme_chaperone_get_immediate_cc_mark(mark_key, mark_val);
          
          PUSH_RUNSTACK(p, RUNSTACK, 1);
	  RUNSTACK_CHANGED();
          RUNSTACK[0] = mark_val;

          obj = wcm->body;
          goto eval_top;
#undef wcm
        }
      case scheme_case_lambda_sequence_type:
        {
          UPDATE_THREAD_RSPTR();
          v = scheme_case_lambda_execute(obj);
          break;
        }
      default:
	v = obj;
	goto returnv_never_multi;
      }
  }

  if (SAME_OBJ(v, SCHEME_TAIL_CALL_WAITING)) {
    obj = p->ku.apply.tail_rator;
    num_rands = p->ku.apply.tail_num_rands;
    rands = p->ku.apply.tail_rands;
    p->ku.apply.tail_rator = NULL;
    p->ku.apply.tail_rands = NULL;
    RUNSTACK = runstack_base;
    RUNSTACK_CHANGED();
    check_rands = num_rands;
    goto apply_top;
  }

  if (SAME_OBJ(v, SCHEME_EVAL_WAITING)) {
    RESET_LOCAL_RUNSTACK();
    obj = p->ku.eval.wait_expr;
    p->ku.eval.wait_expr = NULL;
    goto eval_top;
  }

 returnv:

  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES))
    if (get_value > 0) {
      scheme_wrong_return_arity(NULL, 1, p->ku.multiple.count, 
				p->ku.multiple.array,
				NULL);
      return NULL;
    }

 returnv_never_multi:

  /* If resetting RUNSTACK to old_runstack makes the stack larger, we
     need to clear extra slots to avoid making an old value on the
     runstack suddenly live again */
  while ((uintptr_t)RUNSTACK > (uintptr_t)old_runstack) {
    RUNSTACK--;
    *RUNSTACK = NULL;
  }
  MZ_RUNSTACK = old_runstack;
  MZ_CONT_MARK_STACK = old_cont_mark_stack;
  MZ_CONT_MARK_POS -= 2;

  DEBUG_CHECK_TYPE(v);

  return v;

#ifdef p
# undef p
#endif
}

Scheme_Object **scheme_current_argument_stack()
{
  return MZ_RUNSTACK;
}

/*========================================================================*/
/*                  eval/compile/expand starting points                   */
/*========================================================================*/

Scheme_Object *scheme_dynamic_require(int argc, Scheme_Object *argv[])
{
  Scheme_Object *proc;
  proc = scheme_get_startup_export("dynamic-require");
  return scheme_apply(proc, argc, argv);
}

int scheme_is_syntax(Scheme_Object *v)
{
  Scheme_Object *a[1];
  if (!is_syntax_proc) {
    REGISTER_SO(is_syntax_proc);
    is_syntax_proc = scheme_get_startup_export("syntax?");
  }
  a[0] = v;
  return SCHEME_TRUEP(scheme_apply(is_syntax_proc, 1, a));
}

Scheme_Object *scheme_expander_syntax_to_datum(Scheme_Object *v)
{
  Scheme_Object *a[1];
  if (scheme_starting_up)
    return v;
  else {
    if (!expander_syntax_to_datum_proc) {
      REGISTER_SO(expander_syntax_to_datum_proc);
      expander_syntax_to_datum_proc = scheme_get_startup_export("maybe-syntax->datum");
    }
    a[0] = v;
    return scheme_apply(expander_syntax_to_datum_proc, 1, a);
  }
}

Scheme_Object *scheme_namespace_require(Scheme_Object *mod_path)
{
  Scheme_Object *proc, *a[1];
  proc = scheme_get_startup_export("namespace-require");
  a[0] = mod_path;
  return scheme_apply(proc, 1, a);
}

static Scheme_Env *namespace_to_env(Scheme_Object *ns)
{
  Scheme_Env *env;

  env = scheme_lookup_in_table(scheme_namespace_to_env, (char *)ns);

  if (!env) {
    env = MALLOC_ONE_TAGGED(Scheme_Env);
    env->so.type = scheme_env_type;
    env->namespace = ns;
    scheme_add_to_table(scheme_namespace_to_env, (char *)ns, (void *)env, 0);
  }

  return env;
}

Scheme_Env *scheme_make_empty_env(void)
{
  Scheme_Object *proc, *ns, *inst, *a[2];
  Scheme_Env *env;
  
  proc = scheme_get_startup_export("current-namespace");
  ns = scheme_apply(proc, 0, NULL);

  env = namespace_to_env(ns);

  proc = scheme_get_startup_export("namespace->instance");
  a[0] = ns;
  a[1] = scheme_make_integer(0);
  inst = scheme_apply(proc, 2, a);

  env->instance = (Scheme_Instance *)inst;

  return env;
}

Scheme_Env *scheme_get_current_namespace_as_env()
{
  Scheme_Object *proc, *ns;
  
  proc = scheme_get_startup_export("current-namespace");
  ns = scheme_apply(proc, 0, NULL);
  
  return namespace_to_env(ns);
}

void scheme_set_current_namespace_as_env(Scheme_Env *env)
{
  Scheme_Object *proc, *a[1];
  
  proc = scheme_get_startup_export("current-namespace");

  a[0] = env->namespace;
  (void)scheme_apply(proc, 1, a);
}

Scheme_Object *scheme_compile(Scheme_Object *form, Scheme_Env *env, int writeable)
{
  Scheme_Object *compile_proc, *a[3];
  compile_proc = scheme_get_startup_export("compile");
  a[0] = form;
  a[1] = env->namespace;
  a[2] = (writeable ? scheme_true : scheme_false);
  return scheme_apply(compile_proc, 3, a);
}

Scheme_Object *scheme_compile_for_eval(Scheme_Object *form, Scheme_Env *env)
{
  return scheme_compile(form, env, 0);
}

Scheme_Object *scheme_eval(Scheme_Object *obj, Scheme_Env *env)
{
  Scheme_Object *eval_proc, *a[2];
  eval_proc = scheme_get_startup_export("eval-top-level");
  a[0] = obj;
  a[1] = env->namespace;
  return scheme_apply(eval_proc, 2, a);
}

Scheme_Object *scheme_eval_multi(Scheme_Object *obj, Scheme_Env *env)
{
  Scheme_Object *eval_proc, *a[2];
  eval_proc = scheme_get_startup_export("eval-top-level");
  a[0] = obj;
  a[1] = env->namespace;
  return scheme_apply_multi(eval_proc, 2, a);
}

static Scheme_Object *finish_eval_with_prompt(void *_data, int argc, Scheme_Object **argv)
{
  Scheme_Object *data = (Scheme_Object *)_data;
  return scheme_eval(SCHEME_CAR(data), (Scheme_Env *)SCHEME_CDR(data));
}

Scheme_Object *scheme_eval_with_prompt(Scheme_Object *obj, Scheme_Env *env)
{
  return scheme_call_with_prompt(finish_eval_with_prompt, 
                                 scheme_make_pair(obj, (Scheme_Object *)env));
}

static Scheme_Object *finish_eval_multi_with_prompt(void *_data, int argc, Scheme_Object **argv)
{
  Scheme_Object *data = (Scheme_Object *)_data;
  return scheme_eval_multi(SCHEME_CAR(data), (Scheme_Env *)SCHEME_CDR(data));
}

Scheme_Object *scheme_eval_multi_with_prompt(Scheme_Object *obj, Scheme_Env *env)
{
  return scheme_call_with_prompt_multi(finish_eval_multi_with_prompt, 
                                       scheme_make_pair(obj, (Scheme_Object *)env));
}

Scheme_Object *_scheme_eval_compiled(Scheme_Object *obj, Scheme_Env *env)
{
  return _scheme_eval_linked_expr(obj);
}

Scheme_Object *_scheme_eval_compiled_multi(Scheme_Object *obj, Scheme_Env *env)
{
  return _scheme_eval_linked_expr_multi(obj);
}

Scheme_Object *scheme_tail_eval_expr(Scheme_Object *obj)
{
  return scheme_tail_eval(obj);
}

Scheme_Env *scheme_primitive_module(Scheme_Object *name, Scheme_Env *for_env)
{
  Scheme_Env *env;
  Scheme_Instance *inst;
  Scheme_Hash_Tree *protected;

  /* An environment wrapper just for filling in the instance: */
  env = MALLOC_ONE_TAGGED(Scheme_Env);
  env->so.type = scheme_env_type;
  env->namespace = for_env->namespace; /* records target namespace, not instance's namespace! */

  inst = scheme_make_instance(name, NULL);
  env->instance = (Scheme_Instance *)inst;

  protected = scheme_make_hash_tree(0);
  env->protected = protected;

  return env;
}

void scheme_finish_primitive_module(Scheme_Env *env)
{
  Scheme_Object *proc, *a[5];
  
  proc = scheme_get_startup_export("declare-primitive-module!");
  a[0] = env->instance->name;
  a[1] = (Scheme_Object *)env->instance;
  a[2] = env->namespace; /* target namespace */
  a[3] = (Scheme_Object *)env->protected;
  a[4] = (env->cross_phase ? scheme_true : scheme_false);
  scheme_apply(proc, 5, a);
}

void scheme_set_primitive_module_phaseless(Scheme_Env *env, int phaseless)
{
  env->cross_phase = phaseless;
}

void scheme_protect_primitive_provide(Scheme_Env *env, Scheme_Object *name)
{
  Scheme_Hash_Tree *protected;
  protected = scheme_hash_tree_set(env->protected, name, scheme_true);
  env->protected = protected;
}

/* local functions */

static Scheme_Object *read_syntax(Scheme_Object *port, Scheme_Object *src)
{
  Scheme_Object *proc, *a[2];
  proc = scheme_get_startup_export("read-syntax");
  a[0] = src;
  a[1] = port;
  return scheme_apply(proc, 2, a);
}

static Scheme_Object *namespace_introduce(Scheme_Object *stx)
{
  Scheme_Object *proc, *a[1];
  proc = scheme_get_startup_export("namespace-introduce");
  a[0] = stx;
  return scheme_apply(proc, 1, a);
}

static Scheme_Object *do_eval_string_all(Scheme_Object *port, const char *str, Scheme_Env *env, 
                                         int cont, int w_prompt)
/* cont == -2 => module (no result)
   cont == -1 => single result
   cont == 1 -> multiple result ok
   cont == 2 -> #%top-interaction, multiple result ok, use current_print to show results */
{
  Scheme_Object *expr, *result = scheme_void;

  if (!port)
    port = scheme_make_byte_string_input_port(str);

  do {
    expr = read_syntax(port, scheme_false);

    if ((cont == -2) && !SAME_OBJ(expr, scheme_eof)) {
      expr = namespace_introduce(expr);
    }

    if (SAME_OBJ(expr, scheme_eof))
      cont = 0;
    else if (cont < 0) {
      if (w_prompt)
        result = scheme_eval_with_prompt(expr, env);
      else
        result = scheme_eval(expr, env);
    } else {
      if (cont == 2)
        expr = scheme_make_pair(scheme_intern_symbol("#%top-interaction"), expr);

      if (w_prompt)
        result = scheme_eval_multi_with_prompt(expr, env);
      else
        result = scheme_eval_multi(expr, env);

      if (cont == 2) {
        Scheme_Object **a, *_a[1], *arg[1], *printer;
        int cnt, i;

        if (result == SCHEME_MULTIPLE_VALUES) {
          Scheme_Thread *p = scheme_current_thread;
          if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
            p->values_buffer = NULL;
          a = p->ku.multiple.array;
          p->ku.multiple.array = NULL;
          cnt = p->ku.multiple.count;
        } else {
          _a[0] = result;
          a = _a;
          cnt = 1;
        }

        for (i = 0; i < cnt; i++) {
          printer = scheme_get_param(scheme_current_config(), MZCONFIG_PRINT_HANDLER);
          arg[0] = a[i];
          scheme_apply(printer, 1, arg);
          scheme_flush_output(scheme_get_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT));
        }
      }
    }
  } while (cont > 0);

  return result;
}

Scheme_Object *scheme_eval_string_all(const char *str, Scheme_Env *env, int cont)
{
  return do_eval_string_all(NULL, str, env, cont, 0);
}

Scheme_Object *scheme_eval_string(const char *str, Scheme_Env *env)
{
  return do_eval_string_all(NULL, str, env, -1, 0);
}

Scheme_Object *scheme_eval_module_string(const char *str, Scheme_Env *env)
{
  return do_eval_string_all(NULL, str, env, -2, 0);
}

Scheme_Object *scheme_eval_string_multi(const char *str, Scheme_Env *env)
{
  return do_eval_string_all(NULL, str, env, 0, 0);
}

Scheme_Object *scheme_eval_string_all_with_prompt(const char *str, Scheme_Env *env, int cont)
{
  return do_eval_string_all(NULL, str, env, cont, 1);
}

Scheme_Object *scheme_eval_all_with_prompt(Scheme_Object *port, Scheme_Env *env, int cont)
{
  if (!port) port = scheme_orig_stdin_port;
  return do_eval_string_all(port, NULL, env, cont, 1);
}

Scheme_Object *scheme_eval_string_with_prompt(const char *str, Scheme_Env *env)
{
  return do_eval_string_all(NULL, str, env, -1, 1);
}

Scheme_Object *scheme_eval_string_multi_with_prompt(const char *str, Scheme_Env *env)
{
  return do_eval_string_all(NULL, str, env, 0, 1);
}

void scheme_embedded_load(intptr_t len, const char *desc, int predefined)
{
  Scheme_Object *s, *e, *a[4], *eload;
  eload = scheme_get_startup_export("embedded-load");
  if (len < 0) {
    /* description mode */
    s = scheme_make_utf8_string(desc);
    e = scheme_make_utf8_string(desc XFORM_OK_PLUS strlen(desc) XFORM_OK_PLUS 1);
    a[0] = s;
    a[1] = e;
    a[2] = scheme_false;
  } else {
    /* content mode */
    a[0] = scheme_false;
    a[1] = scheme_false;
    s = scheme_make_sized_byte_string((char *)desc, len, 0);
    a[2] = s;
  }
  a[3] = (predefined ? scheme_true : scheme_false);
  (void)scheme_apply(eload, 4, a);
}

int scheme_is_predefined_module_path(Scheme_Object *m)
{
  Scheme_Object *is_predef, *a[1], *r;
  is_predef = scheme_get_startup_export("module-predefined?");
  a[0] = m;
  r = scheme_apply(is_predef, 1, a);
  return SCHEME_TRUEP(r);
}

void scheme_init_collection_paths_post(Scheme_Env *env, Scheme_Object *extra_dirs, Scheme_Object *post_dirs)
{
  mz_jmp_buf * volatile save, newbuf;
  Scheme_Thread * volatile p;
  p = scheme_get_current_thread();
  save = p->error_buf;
  p->error_buf = &newbuf;
  if (!scheme_setjmp(newbuf)) {
    Scheme_Object *clcp, *flcp, *a[2];

    clcp = scheme_builtin_value("current-library-collection-links");
    flcp = scheme_builtin_value("find-library-collection-links");

    if (clcp && flcp) {
      a[0] = _scheme_apply(flcp, 0, NULL);
      _scheme_apply(clcp, 1, a);
    }

    clcp = scheme_builtin_value("current-library-collection-paths");
    flcp = scheme_builtin_value("find-library-collection-paths");

    if (clcp && flcp) {
      a[0] = extra_dirs;
      a[1] = post_dirs;
      a[0] = _scheme_apply(flcp, 2, a);
      _scheme_apply(clcp, 1, a);
    }
  } else {
    scheme_clear_escape();
  }
  p->error_buf = save;
}

void scheme_init_collection_paths(Scheme_Env *env, Scheme_Object *extra_dirs)
{
  scheme_init_collection_paths_post(env, extra_dirs, scheme_null);
}

void scheme_init_compiled_roots(Scheme_Env *global_env, const char *paths)
{
  mz_jmp_buf * volatile save, newbuf;
  Scheme_Thread * volatile p;
  p = scheme_get_current_thread();
  save = p->error_buf;
  p->error_buf = &newbuf;
  if (!scheme_setjmp(newbuf)) {
    Scheme_Object *rr, *ccfr, *pls2pl, *a[3];

    rr = scheme_builtin_value("regexp-replace*");
    ccfr = scheme_builtin_value("current-compiled-file-roots");
    pls2pl = scheme_builtin_value("path-list-string->path-list");

    if (rr && ccfr && pls2pl) {
      a[0] = scheme_make_utf8_string("@[(]version[)]");
      a[1] = scheme_make_utf8_string(paths);
      a[2] = scheme_make_utf8_string(scheme_version());
      a[2] = _scheme_apply(rr, 3, a);

      a[0] = scheme_intern_symbol("same");
      a[1] = scheme_build_path(1, a);

      a[0] = a[2];
      a[1] = scheme_make_pair(a[1], scheme_null);
      a[0] = _scheme_apply(pls2pl, 2, a);
      _scheme_apply(ccfr, 1, a);
    }
  } else {
    scheme_clear_escape();
  }
  p->error_buf = save;  
}

static Scheme_Object *allow_set_undefined(int argc, Scheme_Object **argv)
{
  return scheme_param_config("compile-allow-set!-undefined", 
			     scheme_make_integer(MZCONFIG_ALLOW_SET_UNDEFINED),
			     argc, argv,
			     -1, NULL, NULL, 1);
}

static Scheme_Object *compile_module_constants(int argc, Scheme_Object **argv)
{
  return scheme_param_config("compile-enforce-module-constants", 
			     scheme_make_integer(MZCONFIG_COMPILE_MODULE_CONSTS),
			     argc, argv,
			     -1, NULL, NULL, 1);
}

static Scheme_Object *use_jit(int argc, Scheme_Object **argv)
{
  return scheme_param_config("eval-jit-enabled", 
			     scheme_make_integer(MZCONFIG_USE_JIT),
			     argc, argv,
			     -1, NULL, NULL, 1);
}

static Scheme_Object *disallow_inline(int argc, Scheme_Object **argv)
{
  return scheme_param_config("compile-context-preservation-enabled", 
			     scheme_make_integer(MZCONFIG_DISALLOW_INLINE),
			     argc, argv,
			     -1, NULL, NULL, 1);
}

static Scheme_Object *compile_target_machine(int argc, Scheme_Object **argv)
{
  return scheme_param_config2("current-compile-target-machine", 
                              scheme_make_integer(MZCONFIG_COMPILE_TARGET_MACHINE),
                              argc, argv,
                              -1, scheme_compile_target_check, 
                              "(or/c #f (and/c symbol? compile-target-machine?))", 0);
}

static Scheme_Object *compile_is_target_machine(int argc, Scheme_Object **argv)
{
  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract("compile-target-machine?", "symbol?", 0, argc, argv);
  return scheme_compile_target_check(argc, argv);
}

static Scheme_Object *
enable_break(int argc, Scheme_Object *argv[])
{
  if (argc == 1) {
    scheme_set_can_break(SCHEME_TRUEP(argv[0]));
    if (SCHEME_TRUEP(argv[0])) {
      if (scheme_current_thread->external_break && scheme_can_break(scheme_current_thread)) {
	scheme_thread_block(0.0);
	scheme_current_thread->ran_some = 1;
      }
    }
    return scheme_void;
  } else {
    return scheme_can_break(scheme_current_thread) ? scheme_true : scheme_false;
  }
}

Scheme_Object *scheme_make_modidx(Scheme_Object *path,
                                  Scheme_Object *base,
                                  Scheme_Object *resolved)
{
  Scheme_Object *proc, *a[2];
  proc = scheme_get_startup_export("module-path-index-join");
  a[0] = path;
  a[1] = base;
  return scheme_apply(proc, 2, a);
 
}

int scheme_is_module_path_index(Scheme_Object *v)
{
  Scheme_Object *proc, *a[1];
  proc = scheme_get_startup_export("module-path-index?");
  a[0] = v;
  return SCHEME_TRUEP(scheme_apply(proc, 1, a));
}

int scheme_is_resolved_module_path(Scheme_Object *v)
{
  Scheme_Object *proc, *a[1];
  proc = scheme_get_startup_export("resolved-module-path?");
  a[0] = v;
  return SCHEME_TRUEP(scheme_apply(proc, 1, a));
}

int scheme_is_module_path(Scheme_Object *v)
{
  Scheme_Object *proc, *a[1];
  proc = scheme_get_startup_export("module-path?");
  a[0] = v;
  return SCHEME_TRUEP(scheme_apply(proc, 1, a));
}

int scheme_module_is_declared(Scheme_Object *name, int try_load)
{
  Scheme_Object *proc, *a[2];
  proc = scheme_get_startup_export("module-declared?");
  a[0] = name;
  a[1] = (try_load ? scheme_true : scheme_false);
  return SCHEME_TRUEP(scheme_apply(proc, 2, a));
}

Scheme_Object *scheme_datum_to_kernel_stx(Scheme_Object *v)
{
  Scheme_Object *proc, *a[1];
  proc = scheme_get_startup_export("datum->kernel-syntax");
  a[0] = v;
  return scheme_apply(proc, 1, a);
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_eval.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_saved_stack, mark_saved_stack);
}

END_XFORM_SKIP;

#endif
