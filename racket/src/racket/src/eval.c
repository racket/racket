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

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

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
   easily-evaluated expressions, such as constrants and variable
   lookups. This can be viewed as a kind of half-way A-normalization.

   Bytecodes are not linear. They're actually trees of expression
   nodes.

   Top-level variables (global or module) are referenced through the
   Scheme stack, so that the variables can be "re-linked" each time a
   module is instantiated. Syntax constants are similarly accessed
   through the Scheme stack. The global variables and syntax objects
   are sometimes called the "prefix", and scheme_push_prefix()
   initializes the prefix portion of the stack. This prefix is
   captured in a continuation that refers to global or module-level
   variables (which is why the closure is not entirely flat). Special
   GC support allows a prefix to be pruned to just the globals that
   are used by live closures.

   Bytecode compilation:

   Compilation works in four passes.

   The first pass, called "compile", performs most of the work and
   tracks variable usage (including whether a variable is mutated or
   not). See "compile.c" along with "compenv.c".

   The second pass, called "letrec_rec", determines which references
   to `letrec'-bound variables need to be guarded with a run-time
   check to prevent use before definition. The analysis result is
   reflected by the insertion of `check-notunsafe-undefined`
   calls. This this pass mutates records produced by the "compile"
   pass.

   The third pass, called "optimize", performs constant propagation,
   constant folding, and function inlining; this pass mutates records
   produced by the "letrec_check" pass. See "optimize.c".

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
   preparation pass is a shallow, function (i.e., it doesn't mutate
   the original bytecode) pass; the body of a fuction is preparred for
   JITting lazily. See "jitprep.c".

*/

#include "schpriv.h"
#include "schrunst.h"
#include "schexpobs.h"
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
#ifdef BEOS_FIND_STACK_BOUNDS
# include <be/kernel/OS.h>
#endif
#ifdef OSKIT_FIXED_STACK_BOUNDS
# include <oskit/machine/base_stack.h>
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
void scheme_set_startup_use_jit(int v) { scheme_startup_use_jit =  v; }

SHARED_OK static int valdiate_compile_result = 0;

/* THREAD LOCAL SHARED */
THREAD_LOCAL_DECL(volatile int scheme_fuel_counter);
#ifdef USE_STACK_BOUNDARY_VAR
THREAD_LOCAL_DECL(uintptr_t scheme_stack_boundary);
THREAD_LOCAL_DECL(uintptr_t volatile scheme_jit_stack_boundary);
#endif
THREAD_LOCAL_DECL(int scheme_continuation_application_count);
THREAD_LOCAL_DECL(static int generate_lifts_count);
THREAD_LOCAL_DECL(int scheme_overflow_count);
THREAD_LOCAL_DECL(Scheme_Prefix *scheme_prefix_finalize);
int scheme_get_overflow_count() { return scheme_overflow_count; }

/* read-only globals */
READ_ONLY Scheme_Object *scheme_eval_waiting;
READ_ONLY Scheme_Object *scheme_multiple_values;

/* symbols */
ROSYM static Scheme_Object *app_symbol;
ROSYM static Scheme_Object *datum_symbol;
ROSYM static Scheme_Object *top_symbol;
ROSYM static Scheme_Object *top_level_symbol;
ROSYM static Scheme_Object *define_values_symbol;
ROSYM static Scheme_Object *letrec_values_symbol;
ROSYM static Scheme_Object *lambda_symbol;
ROSYM static Scheme_Object *unknown_symbol;
ROSYM static Scheme_Object *void_link_symbol;
ROSYM static Scheme_Object *quote_symbol;
ROSYM static Scheme_Object *letrec_syntaxes_symbol;
ROSYM static Scheme_Object *begin_symbol;
ROSYM static Scheme_Object *let_values_symbol;
ROSYM static Scheme_Object *internal_define_symbol;
ROSYM static Scheme_Object *module_symbol;
ROSYM static Scheme_Object *module_begin_symbol;
ROSYM static Scheme_Object *expression_symbol;
ROSYM Scheme_Object *scheme_stack_dump_key;
READ_ONLY static Scheme_Object *zero_rands_ptr; /* &zero_rands_ptr is dummy rands pointer */

/* locals */
static Scheme_Object *eval(int argc, Scheme_Object *argv[]);
static Scheme_Object *compile(int argc, Scheme_Object *argv[]);
static Scheme_Object *compiled_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *expand(int argc, Scheme_Object **argv);
static Scheme_Object *local_expand(int argc, Scheme_Object **argv);
static Scheme_Object *local_expand_expr(int argc, Scheme_Object **argv);
static Scheme_Object *local_expand_catch_lifts(int argc, Scheme_Object **argv);
static Scheme_Object *local_transformer_expand(int argc, Scheme_Object **argv);
static Scheme_Object *local_transformer_expand_catch_lifts(int argc, Scheme_Object **argv);
static Scheme_Object *local_eval(int argc, Scheme_Object **argv);
static Scheme_Object *expand_once(int argc, Scheme_Object **argv);
static Scheme_Object *expand_to_top_form(int argc, Scheme_Object **argv);
static Scheme_Object *enable_break(int, Scheme_Object *[]);
static Scheme_Object *current_eval(int argc, Scheme_Object *[]);
static Scheme_Object *current_compile(int argc, Scheme_Object *[]);

static Scheme_Object *eval_stx(int argc, Scheme_Object *argv[]);
static Scheme_Object *compile_stx(int argc, Scheme_Object *argv[]);
static Scheme_Object *expand_stx(int argc, Scheme_Object **argv);
static Scheme_Object *expand_stx_once(int argc, Scheme_Object **argv);
static Scheme_Object *expand_stx_to_top_form(int argc, Scheme_Object **argv);
static Scheme_Object *top_introduce_stx(int argc, Scheme_Object **argv);

static Scheme_Object *allow_set_undefined(int argc, Scheme_Object **argv);
static Scheme_Object *compile_module_constants(int argc, Scheme_Object **argv);
static Scheme_Object *use_jit(int argc, Scheme_Object **argv);
static Scheme_Object *disallow_inline(int argc, Scheme_Object **argv);

static Scheme_Object *_eval_compiled_multi_with_prompt(Scheme_Object *obj, Scheme_Env *env);

void scheme_escape_to_continuation(Scheme_Object *obj, int num_rands, Scheme_Object **rands, Scheme_Object *alt_full);

#ifdef MZ_PRECISE_GC
static void mark_pruned_prefixes(struct NewGC *gc);
#endif

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
scheme_init_eval (Scheme_Env *env)
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

  REGISTER_SO(define_values_symbol);
  REGISTER_SO(letrec_values_symbol);
  REGISTER_SO(lambda_symbol);
  REGISTER_SO(unknown_symbol);
  REGISTER_SO(void_link_symbol);
  REGISTER_SO(quote_symbol);
  REGISTER_SO(letrec_syntaxes_symbol);
  REGISTER_SO(begin_symbol);
  REGISTER_SO(let_values_symbol);
  
  define_values_symbol    = scheme_intern_symbol("define-values");
  letrec_values_symbol    = scheme_intern_symbol("letrec-values");
  let_values_symbol       = scheme_intern_symbol("let-values");
  lambda_symbol           = scheme_intern_symbol("lambda");
  unknown_symbol          = scheme_intern_symbol("unknown");
  void_link_symbol        = scheme_intern_symbol("-v");
  quote_symbol            = scheme_intern_symbol("quote");
  letrec_syntaxes_symbol  = scheme_intern_symbol("letrec-syntaxes+values");
  begin_symbol            = scheme_intern_symbol("begin");
  
  REGISTER_SO(module_symbol);
  REGISTER_SO(module_begin_symbol);
  REGISTER_SO(internal_define_symbol);
  REGISTER_SO(expression_symbol);
  REGISTER_SO(top_level_symbol);

  module_symbol           = scheme_intern_symbol("module");
  module_begin_symbol     = scheme_intern_symbol("module-begin");
  internal_define_symbol  = scheme_intern_symbol("internal-define");
  expression_symbol       = scheme_intern_symbol("expression");
  top_level_symbol        = scheme_intern_symbol("top-level");

  REGISTER_SO(app_symbol);
  REGISTER_SO(datum_symbol);
  REGISTER_SO(top_symbol);

  app_symbol    = scheme_intern_symbol("#%app");
  datum_symbol  = scheme_intern_symbol("#%datum");
  top_symbol    = scheme_intern_symbol("#%top");

  REGISTER_SO(scheme_stack_dump_key);
  scheme_stack_dump_key = scheme_make_symbol("stk"); /* uninterned! */

  GLOBAL_PRIM_W_ARITY2("eval",        eval,     1, 2, 0, -1, env);
  GLOBAL_PRIM_W_ARITY2("eval-syntax", eval_stx, 1, 2, 0, -1, env);

  GLOBAL_PRIM_W_ARITY("compile",                                 compile,                               1, 1, env);
  GLOBAL_PRIM_W_ARITY("compile-syntax",                          compile_stx,                           1, 1, env);
  GLOBAL_PRIM_W_ARITY("compiled-expression?",                    compiled_p,                            1, 1, env);
  GLOBAL_PRIM_W_ARITY("expand",                                  expand,                                1, 1, env);
  GLOBAL_PRIM_W_ARITY("expand-syntax",                           expand_stx,                            1, 1, env);
  GLOBAL_PRIM_W_ARITY("local-expand",                            local_expand,                          3, 4, env);
  GLOBAL_PRIM_W_ARITY2("syntax-local-expand-expression",         local_expand_expr,                     1, 1, 2, 2, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-bind-syntaxes",              local_eval,                            3, 3, env);
  GLOBAL_PRIM_W_ARITY("local-expand/capture-lifts",              local_expand_catch_lifts,              3, 5, env);
  GLOBAL_PRIM_W_ARITY("local-transformer-expand",                local_transformer_expand,              3, 4, env);
  GLOBAL_PRIM_W_ARITY("local-transformer-expand/capture-lifts",  local_transformer_expand_catch_lifts,  3, 5, env);
  GLOBAL_PRIM_W_ARITY("expand-once",                             expand_once,                           1, 1, env);
  GLOBAL_PRIM_W_ARITY("expand-syntax-once",                      expand_stx_once,                       1, 1, env);
  GLOBAL_PRIM_W_ARITY("expand-to-top-form",                      expand_to_top_form,                    1, 1, env);
  GLOBAL_PRIM_W_ARITY("expand-syntax-to-top-form",               expand_stx_to_top_form,                1, 1, env);
  GLOBAL_PRIM_W_ARITY("namespace-syntax-introduce",              top_introduce_stx,                     1, 1, env);
  GLOBAL_PRIM_W_ARITY("break-enabled",                           enable_break,                          0, 1, env);

  GLOBAL_PARAMETER("current-eval",                      current_eval,             MZCONFIG_EVAL_HANDLER,          env);
  GLOBAL_PARAMETER("current-compile",                   current_compile,          MZCONFIG_COMPILE_HANDLER,       env);
  GLOBAL_PARAMETER("compile-allow-set!-undefined",      allow_set_undefined,      MZCONFIG_ALLOW_SET_UNDEFINED,   env);
  GLOBAL_PARAMETER("compile-enforce-module-constants",  compile_module_constants, MZCONFIG_COMPILE_MODULE_CONSTS, env);
  GLOBAL_PARAMETER("eval-jit-enabled",                  use_jit,                  MZCONFIG_USE_JIT,               env);
  GLOBAL_PARAMETER("compile-context-preservation-enabled", disallow_inline,       MZCONFIG_DISALLOW_INLINE,       env);

  if (getenv("PLT_VALIDATE_COMPILE")) {
    /* Enables validation of bytecode as it is generated,
       to double-check that the compiler is producing
       valid bytecode as it should. */
    valdiate_compile_result = 1;
  }
}

void scheme_init_eval_places()
{
#ifdef MZ_PRECISE_GC
  scheme_prefix_finalize = (Scheme_Prefix *)0x1; /* 0x1 acts as a sentenel */
  GC_set_post_propagate_hook(mark_pruned_prefixes);
#endif
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
static uintptr_t adjust_stack_base(uintptr_t bnd) {
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
	if ((len > 8) && !strcmp("[stack]\n", buf + len - 8)) {
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
	  bnd = p;
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
		  mzlonglong lsz;
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

# ifdef BEOS_FIND_STACK_BOUNDS
    {
      thread_info info;
      get_thread_info(find_thread(NULL), &info);
      scheme_stack_boundary = (uintptr_t)info.stack_base + STACK_SAFETY_MARGIN;
    }
# endif

# ifdef OSKIT_FIXED_STACK_BOUNDS
    scheme_stack_boundary = (uintptr_t)base_stack_start + STACK_SAFETY_MARGIN;
# endif

# ifdef UNIX_FIND_STACK_BOUNDS
    {
      struct rlimit rl;
      uintptr_t bnd, lim;

      bnd = (uintptr_t)scheme_get_current_os_thread_stack_base();

      getrlimit(RLIMIT_STACK, &rl);

#  ifdef LINUX_FIND_STACK_BASE
      bnd = adjust_stack_base(bnd);
#  endif

      lim = (uintptr_t)rl.rlim_cur;
#  ifdef UNIX_STACK_MAXIMUM
      if (lim > UNIX_STACK_MAXIMUM)
        lim = UNIX_STACK_MAXIMUM;
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
/*                           linking variables                            */
/*========================================================================*/

static Scheme_Object *link_module_variable(Scheme_Object *modidx,
					   Scheme_Object *varname,
					   int check_access, Scheme_Object *insp,
					   int pos, int mod_phase,
					   Scheme_Env *env, 
                                           Scheme_Object **exprs, int which,
                                           int flags, Scheme_Object *shape)
{
  Scheme_Object *modname;
  Scheme_Env *menv;
  Scheme_Bucket *bkt;
  int self = 0;

  /* If it's a name id, resolve the name. */
  modname = scheme_module_resolve(modidx, 1);

  if (env->module && SAME_OBJ(env->module->modname, modname)
      && (env->mod_phase == mod_phase)) {
    self = 1;
    menv = env;
  } else {
    menv = scheme_module_access(modname, env, mod_phase);
    
    if (!menv) {
      scheme_wrong_syntax("link", NULL, varname,
			  "namespace mismatch;\n"
                          " reference to a module that is not available\n"
                          "  reference phase: %d\n"
                          "  referenced module: %D\n"
                          "  referenced phase level: %d\n"
                          "  reference in module: %D",
			  env->phase,
                          modname,
                          mod_phase,
                          env->module ? env->module->modsrc : scheme_false);
      return NULL;
    }

    if (check_access && !SAME_OBJ(menv, env)) {
      varname = scheme_check_accessible_in_module(menv, insp, NULL, varname, NULL, NULL, 
                                                  insp, NULL, pos, 0, NULL, NULL, env, NULL,
                                                  NULL);
    }
  }

  if (exprs) {
    if (self) {
      exprs[which] = varname;
    } else {
      if (flags & SCHEME_MODVAR_CONST) {
        Scheme_Object *v;
        v = scheme_make_vector((mod_phase != 0) ? 4 : 3, modname);
        SCHEME_VEC_ELS(v)[1] = varname;
        SCHEME_VEC_ELS(v)[2] = (shape ? shape : scheme_false);
        if (mod_phase != 0)
          SCHEME_VEC_ELS(v)[3] = scheme_make_integer(mod_phase);
      } else {
        Scheme_Object *v = modname;
        if (mod_phase != 0)
          v = scheme_make_pair(v, scheme_make_integer(mod_phase));
        v = scheme_make_pair(varname, v);
        exprs[which] = v;
      }
    }
  }

  bkt = scheme_global_bucket(varname, menv);
  if (!self) {
    const char *bad_reason = NULL;

    if (!bkt->val) {
      bad_reason = "is uninitialized";
    } else if (flags) {
      if (flags & SCHEME_MODVAR_CONST) {
        if (!(((Scheme_Bucket_With_Flags *)bkt)->flags & GLOB_IS_CONSISTENT))
          bad_reason = "is not a procedure or structure-type constant across all instantiations";
        else if (shape && SCHEME_TRUEP(shape)) {
          if (!scheme_get_or_check_procedure_shape(bkt->val, shape))
            bad_reason = "has the wrong procedure or structure-type shape";
        }
      } else {
        if (!(((Scheme_Bucket_With_Flags *)bkt)->flags & GLOB_IS_IMMUTATED))
          bad_reason = "not constant";
      }
    }

    if (bad_reason) {
      scheme_wrong_syntax("link", NULL, varname,
                          "bad variable linkage;\n"
                          " reference to a variable that %s\n"
                          "  reference phase level: %d\n"
                          "  variable module: %D\n"
                          "  variable phase: %d\n"
                          "  reference in module: %D",
                          bad_reason,
                          env->phase,
                          modname,
                          mod_phase,
                          env->module ? env->module->modsrc : scheme_false);
    }

    if (!(((Scheme_Bucket_With_Flags *)bkt)->flags & (GLOB_IS_IMMUTATED | GLOB_IS_LINKED)))
      ((Scheme_Bucket_With_Flags *)bkt)->flags |= GLOB_IS_LINKED;
  }

  return (Scheme_Object *)bkt;
}

static Scheme_Object *link_toplevel(Scheme_Object **exprs, int which, Scheme_Env *env,
                                    Scheme_Object *src_modidx, 
                                    Scheme_Object *dest_modidx,
                                    Scheme_Object *insp)
{
  Scheme_Object *expr = exprs[which];

  if (SCHEME_FALSEP(expr)) {
    /* See scheme_make_environment_dummy */
    Scheme_Bucket *b;
    b = scheme_global_bucket(scheme_stack_dump_key, env);
    if (!(((Scheme_Bucket_With_Flags *)b)->flags & GLOB_STRONG_HOME_LINK)) {
      ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_STRONG_HOME_LINK;
      ((Scheme_Bucket_With_Home *)b)->home_link = (Scheme_Object *)env;
    }
    return (Scheme_Object *)b;
  } else if (SCHEME_PAIRP(expr) || SCHEME_SYMBOLP(expr) || SCHEME_VECTORP(expr)) {
    /* Simplified module reference (as installed by link_module_variable) */
    Scheme_Object *modname, *varname, *shape;
    int mod_phase = 0, flags = 0;
    if (SCHEME_SYMBOLP(expr)) {
      if (!env->module) {
        /* compiled as a module variable, but instantiated in a non-module
           namespace; grab a bucket */
        return (Scheme_Object *)scheme_global_bucket(expr, env);
      } else {
        varname = expr;
        modname = env->module->modname;
        mod_phase = env->mod_phase;
      }
      shape = NULL;
    } else if (SCHEME_PAIRP(expr)) {
      varname = SCHEME_CAR(expr);
      modname = SCHEME_CDR(expr);
      if (SCHEME_PAIRP(modname)) {
        mod_phase = SCHEME_INT_VAL(SCHEME_CDR(modname));
        modname = SCHEME_CAR(modname);
      }
      shape = NULL;
    } else {
      modname = SCHEME_VEC_ELS(expr)[0];
      varname = SCHEME_VEC_ELS(expr)[1];
      flags = SCHEME_MODVAR_CONST;
      shape = SCHEME_VEC_ELS(expr)[2];
      if (SCHEME_VEC_SIZE(expr) > 3)
        mod_phase = SCHEME_INT_VAL(SCHEME_VEC_ELS(expr)[3]);
    }
    return link_module_variable(modname,
                                varname,
                                0, NULL,
                                -1, mod_phase,
                                env, 
                                NULL, 0,
                                flags, shape);
  } else if (SAME_TYPE(SCHEME_TYPE(expr), scheme_variable_type)) {
    Scheme_Bucket *b = (Scheme_Bucket *)expr;
    Scheme_Env *home;

    home = scheme_get_bucket_home(b);
    
    if (!env || !home || !home->module)
      return (Scheme_Object *)b;
    else
      return link_module_variable(home->module->modname,
				  (Scheme_Object *)b->key,
				  1, home->access_insp,
				  -1, home->mod_phase,
				  env, 
                                  exprs, which,
                                  0, NULL);
  } else {
    Module_Variable *mv = (Module_Variable *)expr;

    if ((!insp || SCHEME_FALSEP(insp)) && !mv->insp)
      insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);

    return link_module_variable(scheme_modidx_shift(mv->modidx,
                                                    src_modidx,
                                                    dest_modidx),
				mv->sym, 1, (mv->insp ? mv->insp : insp),
				mv->pos, mv->mod_phase,
				env,
                                exprs, which,
                                SCHEME_MODVAR_FLAGS(mv) & 0x3, mv->shape);
  }
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
MZ_DO_NOT_INLINE(static Scheme_Object *splice_execute(Scheme_Object *data));
MZ_DO_NOT_INLINE(static Scheme_Object *define_syntaxes_execute(Scheme_Object *form));
MZ_DO_NOT_INLINE(static Scheme_Object *begin_for_syntax_execute(Scheme_Object *form));

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
  Scheme_Thread *p = scheme_current_thread;

  GC_CAN_IGNORE Scheme_Object **tb;
  p->tail_buffer = NULL; /* so args aren't zeroed */
  tb = MALLOC_N(Scheme_Object *, p->tail_buffer_size);
  p->tail_buffer = tb;
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
  return scheme_handle_stack_overflow(do_eval_k);
}

static Scheme_Dynamic_Wind *intersect_dw(Scheme_Dynamic_Wind *a, Scheme_Dynamic_Wind *b, 
                                         Scheme_Object *prompt_tag, int b_has_tag, int *_common_depth)
{
  int alen = 0, blen = 0;
  int a_has_tag = 0, a_prompt_delta = 0, b_prompt_delta = 0;
  Scheme_Dynamic_Wind *dw;

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
  while (blen) {
    if (SAME_OBJ(a->id ? a->id : (Scheme_Object *)a, 
                 b->id ? b->id : (Scheme_Object *)b))
      break;
    a = a->prev;
    b = b->prev;
    blen--;
  }

  *_common_depth = (b ? b->depth : -1);

  return a;
}

static Scheme_Prompt *lookup_cont_prompt(Scheme_Cont *c, 
                                         Scheme_Meta_Continuation **_prompt_mc,
                                         MZ_MARK_POS_TYPE *_prompt_pos,
                                         const char *msg)
{
  Scheme_Prompt *prompt;
  Scheme_Object *pt;

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
   allowed if no dynamic-wind-like barriers would be executed for
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
  
  if (b1 != b2) {
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
  
  if (can_ec
      && c->escape_cont
      && scheme_escape_continuation_ok(c->escape_cont))
    scheme_escape_to_continuation(c->escape_cont, num_rands, rands, (Scheme_Object *)c);
      
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
    Scheme_Env *home;
    home = scheme_get_bucket_home(b);
    if (home && home->module) {
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
		       home->module->modsrc);
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

static Scheme_Object *
define_execute_with_dynamic_state(Scheme_Object *vec, int delta, int defmacro,
                                  Resolve_Prefix *rp, Scheme_Env *dm_env, 
                                  Scheme_Dynamic_State *dyn_state)
{
  Scheme_Object *name, *macro, *vals_expr, *vals, *var;
  int i, g, show_any;
  Scheme_Bucket *b;
  Scheme_Object **save_runstack = NULL;

  vals_expr = SCHEME_VEC_ELS(vec)[0];

  if (dm_env) {
    scheme_prepare_exp_env(dm_env);

    save_runstack = scheme_push_prefix(dm_env->exp_env, rp, NULL, NULL, 1, 1, NULL, scheme_false);
    vals = scheme_eval_linked_expr_multi_with_dynamic_state(vals_expr, dyn_state);
    scheme_pop_prefix(save_runstack);
  } else {
    vals = _scheme_eval_linked_expr_multi(vals_expr);
    dm_env = NULL;
  }

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

      if (dm_env)
        is_st = 0;
      else
        is_st = !!scheme_is_simple_make_struct_type(vals_expr, g, 1, 1, 
                                                    NULL, NULL,
                                                    NULL, NULL, MZ_RUNSTACK, 0, 
                                                    NULL, NULL, 5);
      
      for (i = 0; i < g; i++) {
        var = SCHEME_VEC_ELS(vec)[i+delta];
	if (dm_env) {
	  b = scheme_global_keyword_bucket(var, dm_env);

	  macro = scheme_alloc_small_object();
	  macro->type = scheme_macro_type;
	  SCHEME_PTR_VAL(macro) = values[i];

	  scheme_set_global_bucket("define-syntaxes", b, macro, 1);
	  scheme_shadow(dm_env, (Scheme_Object *)b->key, 0);
	} else {
	  Scheme_Prefix *toplevels;
	  toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(var)];
	  b = (Scheme_Bucket *)toplevels->a[SCHEME_TOPLEVEL_POS(var)];
	
	  scheme_set_global_bucket("define-values", b, values[i], 1);
	  scheme_shadow(scheme_get_bucket_home(b), (Scheme_Object *)b->key, 1);

	  if (SCHEME_TOPLEVEL_FLAGS(var) & SCHEME_TOPLEVEL_SEAL) {
            if (is_st)
              ((Scheme_Bucket_With_Flags *)b)->flags |= (GLOB_IS_IMMUTATED | GLOB_IS_CONSISTENT);
            else
              ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_IS_IMMUTATED;
	  }
	}
      }
      if (defmacro)
	scheme_pop_prefix(save_runstack);
	
      return scheme_void;
    } else {
      if (SAME_OBJ(scheme_current_thread->ku.multiple.array, scheme_current_thread->values_buffer))
        scheme_current_thread->values_buffer = NULL;
    }
  } else if (SCHEME_VEC_SIZE(vec) == delta + 1) { /* => single var */
    var = SCHEME_VEC_ELS(vec)[delta];
    if (dm_env) {
      b = scheme_global_keyword_bucket(var, dm_env);

      macro = scheme_alloc_small_object();
      macro->type = scheme_macro_type;
      SCHEME_PTR_VAL(macro) = vals;
      
      scheme_set_global_bucket("define-syntaxes", b, macro, 1);
      scheme_shadow(dm_env, (Scheme_Object *)b->key, 0);
    } else {
      Scheme_Prefix *toplevels;
      toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(var)];
      b = (Scheme_Bucket *)toplevels->a[SCHEME_TOPLEVEL_POS(var)];

      scheme_set_global_bucket("define-values", b, vals, 1);
      scheme_shadow(scheme_get_bucket_home(b), (Scheme_Object *)b->key, 1);
      
      if (SCHEME_TOPLEVEL_FLAGS(var) & SCHEME_TOPLEVEL_SEAL) {
        int flags = GLOB_IS_IMMUTATED;
        if (SCHEME_PROCP(vals_expr) 
            || SAME_TYPE(SCHEME_TYPE(vals_expr), scheme_unclosed_procedure_type)
            || SAME_TYPE(SCHEME_TYPE(vals_expr), scheme_case_lambda_sequence_type)
            || SAME_TYPE(SCHEME_TYPE(vals_expr), scheme_inline_variant_type))
          flags |= GLOB_IS_CONSISTENT;
        ((Scheme_Bucket_With_Flags *)b)->flags |= flags;
      }
      
      if (defmacro)
	scheme_pop_prefix(save_runstack);
    }

    return scheme_void;
  } else
    g = 1;

  /* Special handling of 0 values for define-syntaxes:
     do nothing. This makes (define-values (a b c) (values))
     a kind of declaration form, which is useful is
     a, b, or c is introduced by a macro. */
  if (dm_env && !g)
    return scheme_void;
  
  i = SCHEME_VEC_SIZE(vec) - delta;

  show_any = i;

  if (show_any) {
    var = SCHEME_VEC_ELS(vec)[delta];
    if (dm_env) {
      b = scheme_global_keyword_bucket(var, dm_env);
      name = (Scheme_Object *)b->key;
    } else {
      Scheme_Prefix *toplevels;
      toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(var)];
      b = (Scheme_Bucket *)toplevels->a[SCHEME_TOPLEVEL_POS(var)];
      name = (Scheme_Object *)b->key;
    }
  } else
    name = NULL;
  
  {
    const char *symname;

    symname = (show_any ? scheme_symbol_name(name) : "");

    scheme_wrong_return_arity((defmacro 
			       ? "define-syntaxes"
			       : "define-values"),
			      i, g,
			      (g == 1) ? (Scheme_Object **)vals : scheme_current_thread->ku.multiple.array,
			      "\n  in: %s%s%s",
			      show_any ? "definition of " : "definition of 0 identifiers",
			      symname,
			      show_any ? ((i == 1) ? "" : " ...") : "");
  }

  return NULL;
}

static Scheme_Object *define_values_execute(Scheme_Object *data)
{
  return define_execute_with_dynamic_state(data, 1, 0, NULL, NULL, NULL);
}

static Scheme_Object *set_execute (Scheme_Object *data)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)data;
  Scheme_Object *val;
  Scheme_Bucket *var;
  Scheme_Prefix *toplevels;

  val = _scheme_eval_linked_expr(sb->val);

  toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(sb->var)];
  var = (Scheme_Bucket *)toplevels->a[SCHEME_TOPLEVEL_POS(sb->var)];
  
  scheme_set_global_bucket("set!", var, val, sb->set_undef);

  return scheme_void;
}

static Scheme_Object *ref_execute (Scheme_Object *data)
{
  Scheme_Prefix *toplevels;
  Scheme_Object *o;
  Scheme_Object *var;
  Scheme_Object *tl = SCHEME_PTR1_VAL(data);
  Scheme_Env *env;

  toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(tl)];
  var = toplevels->a[SCHEME_TOPLEVEL_POS(tl)];
  if (SCHEME_FALSEP(SCHEME_PTR2_VAL(data)))
    env = NULL;
  else
    env = scheme_environment_from_dummy(SCHEME_PTR2_VAL(data));
  
  o = scheme_alloc_object();
  o->type = scheme_global_ref_type;
  SCHEME_PTR1_VAL(o) = var;
  SCHEME_PTR2_VAL(o) = (env ? (Scheme_Object *)env : scheme_false);

  if (SCHEME_VARREF_FLAGS(data) & 0x1)
    SCHEME_VARREF_FLAGS(o) |= 0x1;

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
    Scheme_Native_Closure_Data *ndata;
    Scheme_Native_Closure *nc, *na;
    Scheme_Closure_Data *data;
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
	data = (Scheme_Closure_Data *)val;
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
    mc = 0; /* makes compilers happy */
  }

  apos = 1;
  while (i--) {
    ignore_result(_scheme_eval_linked_expr_multi(((Scheme_Sequence *)obj)->array[apos++]));
  }

  if (mv) {
    Scheme_Thread *p = scheme_current_thread;
    p->ku.multiple.array = mv;
    p->ku.multiple.count = mc;
  }

  return v;
}

static Scheme_Object *splice_one_expr(void *expr, int argc, Scheme_Object **argv)
{
  return _scheme_eval_linked_expr_multi((Scheme_Object *)expr);
}

static Scheme_Object *splice_execute(Scheme_Object *data)
{
  if (SAME_TYPE(SCHEME_TYPE(data), scheme_splice_sequence_type)) {
    Scheme_Sequence *seq = (Scheme_Sequence *)data;
    int i, cnt = seq->count - 1;
    
    for (i = 0; i < cnt; i++) {
      ignore_result(_scheme_call_with_prompt_multi(splice_one_expr, seq->array[i]));
    }
    
    return _scheme_eval_linked_expr_multi(seq->array[cnt]);
  } else {
    /* sequence was optimized on read? */
    return _scheme_eval_linked_expr_multi(data);
  }
}

static Scheme_Object *do_define_syntaxes_execute(Scheme_Object *expr, Scheme_Env *dm_env);

static void *define_syntaxes_execute_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *form = p->ku.k.p1;
  Scheme_Env *dm_env = (Scheme_Env *)p->ku.k.p2;
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  return do_define_syntaxes_execute(form, dm_env);
}

static Scheme_Object *
do_define_syntaxes_execute(Scheme_Object *form, Scheme_Env *dm_env)
{
  Scheme_Thread *p = scheme_current_thread;
  Resolve_Prefix *rp;
  Scheme_Object *base_stack_depth, *dummy;
  int depth;
  Scheme_Comp_Env *rhs_env;

  rp = (Resolve_Prefix *)SCHEME_VEC_ELS(form)[1];
  base_stack_depth = SCHEME_VEC_ELS(form)[2];

  depth = SCHEME_INT_VAL(base_stack_depth) + rp->num_stxes + 1;
  if (!scheme_check_runstack(depth)) {
    p->ku.k.p1 = form;

    if (!dm_env) {
      /* Need to get env before we enlarge the runstack: */
      dummy = SCHEME_VEC_ELS(form)[3];
      dm_env = scheme_environment_from_dummy(dummy);
    }
    p->ku.k.p2 = (Scheme_Object *)dm_env;

    return (Scheme_Object *)scheme_enlarge_runstack(depth, define_syntaxes_execute_k);
  }

  dummy = SCHEME_VEC_ELS(form)[3];

  rhs_env = scheme_new_comp_env(scheme_get_env(NULL), NULL, SCHEME_TOPLEVEL_FRAME);

  if (!dm_env)
    dm_env = scheme_environment_from_dummy(dummy);

  {
    Scheme_Dynamic_State dyn_state;
    Scheme_Cont_Frame_Data cframe;
    Scheme_Config *config;

    scheme_prepare_exp_env(dm_env);

    config = scheme_extend_config(scheme_current_config(),
				  MZCONFIG_ENV,
				  (Scheme_Object *)dm_env->exp_env);
    scheme_push_continuation_frame(&cframe);
    scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);

    scheme_set_dynamic_state(&dyn_state, rhs_env, NULL, scheme_false, dm_env, dm_env->link_midx);

    if (SAME_TYPE(SCHEME_TYPE(form), scheme_define_syntaxes_type)) {
      (void)define_execute_with_dynamic_state(form, 4, 1, rp, dm_env, &dyn_state);
    } else {
      Scheme_Object **save_runstack;

      form = SCHEME_VEC_ELS(form)[0];

      save_runstack = scheme_push_prefix(dm_env->exp_env, rp, NULL, NULL, 1, 1, NULL, scheme_false);

      while (!SCHEME_NULLP(form)) {
        ignore_result(scheme_eval_linked_expr_multi_with_dynamic_state(SCHEME_CAR(form), &dyn_state));
        form = SCHEME_CDR(form);
      }
      
      scheme_pop_prefix(save_runstack);
    }
    
    scheme_pop_continuation_frame(&cframe);

    return scheme_void;
  }
}

static Scheme_Object *define_syntaxes_execute(Scheme_Object *form)
{
  return do_define_syntaxes_execute(form, NULL);
}

static Scheme_Object *begin_for_syntax_execute(Scheme_Object *form)
{
  return do_define_syntaxes_execute(form, NULL);
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
  Scheme_Closure_Data *data;
  Scheme_Closure *closure;
  GC_CAN_IGNORE Scheme_Object **runstack;
  GC_CAN_IGNORE Scheme_Object **dest;
  GC_CAN_IGNORE mzshort *map;
  int i;

  data = (Scheme_Closure_Data *)code;
  
#ifdef MZ_USE_JIT
  if (data->u.native_code) {
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
  SCHEME_COMPILED_CLOS_CODE(closure) = data;

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

void scheme_delay_load_closure(Scheme_Closure_Data *data)
{
  if (SCHEME_RPAIRP(data->code)) {
    Scheme_Object *v, *vinfo = NULL;

    v = SCHEME_CAR(data->code);
    if (SCHEME_VECTORP(v)) {
      /* Has info for delayed validation */
      vinfo = v;
      v = SCHEME_VEC_ELS(vinfo)[0];
    }
    v = scheme_load_delayed_code(SCHEME_INT_VAL(v), 
                                 (struct Scheme_Load_Delay *)SCHEME_CDR(data->code));
    data->code = v;
    
    if (vinfo) {
      scheme_validate_closure(NULL, 
                              (Scheme_Object *)data,
                              (char *)SCHEME_VEC_ELS(vinfo)[1], 
                              (Validate_TLS)SCHEME_VEC_ELS(vinfo)[2], 
                              SCHEME_INT_VAL(SCHEME_VEC_ELS(vinfo)[3]),
                              SCHEME_INT_VAL(SCHEME_VEC_ELS(vinfo)[4]),
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
                               : NULL));
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
  GC_CAN_IGNORE Scheme_Object *tmpv; /* safe-for-space relies on GC_CAN_IGNORE */
  GC_MAYBE_IGNORE_INTERIOR Scheme_Object **old_runstack;
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
			     prim->pp.flags & SCHEME_PRIM_IS_METHOD);
	return NULL; /* Shouldn't get here */
      }

      f = prim->prim_val;

      v = f(num_rands, rands, (Scheme_Object *)prim);

      DEBUG_CHECK_TYPE(v);
    } else if (type == scheme_closure_type) {
      Scheme_Closure_Data *data;
      GC_CAN_IGNORE Scheme_Object **stack, **src;
      int i, has_rest, num_params;
      
      DO_CHECK_FOR_BREAK(p, UPDATE_THREAD_RSPTR_FOR_GC(); if (rands == p->tail_buffer) make_tail_buffer_safe(););

      data = SCHEME_COMPILED_CLOS_CODE(obj);

      if ((RUNSTACK - RUNSTACK_START) < data->max_let_depth) {
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
      has_rest = SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST;
      
      if (num_params) {
	if (has_rest) {
	  int extra, n;

	  if (num_rands < (num_params - 1)) {
	    UPDATE_THREAD_RSPTR_FOR_ERROR();
	    /* note: scheme_wrong_count_m handles rands == p->tail_buffer */
	    scheme_wrong_count_m((const char *)obj, 
				 -1, -1,
				 num_rands, rands,
				 SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_IS_METHOD);
	    return NULL; /* Doesn't get here */
	  }

	  n = num_params - has_rest;
	  
	  RUNSTACK = old_runstack - num_params;
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
				 SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_IS_METHOD);
	    return NULL; /* Doesn't get here */
	  }
	
          stack = RUNSTACK = old_runstack - num_params;
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
	RUNSTACK = old_runstack;
	RUNSTACK_CHANGED();
      }
      
      {
	int n = data->closure_size;
      
	if (n) {
	  src = SCHEME_COMPILED_CLOS_ENV(obj);
	  stack = PUSH_RUNSTACK(p, RUNSTACK, n);
	  RUNSTACK_CHANGED();

	  while (n--) {
	    stack[n] = src[n];
	  }
	}
      }

      obj = data->code;

      if (SCHEME_RPAIRP(obj)) {
        UPDATE_THREAD_RSPTR_FOR_GC();
        make_tail_buffer_safe();
        scheme_delay_load_closure(data);
        obj = data->code;
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
      Scheme_Closure_Data *data;
      
      int i;
      
      seq = (Scheme_Case_Lambda *)obj;
      for (i = 0; i < seq->count; i++) {
	data = SCHEME_COMPILED_CLOS_CODE(seq->array[i]);
	if ((!(SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST) 
	     && (data->num_params == num_rands))
	    || ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
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
      GC_CAN_IGNORE Scheme_Native_Closure_Data *data;

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
      obj = NULL; /* save for space, since tmpv is ignored by the GC */
      v = data->start_code(tmpv, num_rands, rands EXTRA_NATIVE_ARGUMENT);

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
                   && (SCHEME_VECTORP(((Scheme_Chaperone *)obj)->redirects))
                   && !(SCHEME_VEC_SIZE(((Scheme_Chaperone *)obj)->redirects) & 1))
               /* A raw pair is from scheme_apply_chaperone(), propagating the
                  original object for an applicable structure. */
               || (type == scheme_raw_pair_type)) {
      int is_method;
      int check_rands = num_rands;
      Scheme_Object *orig_obj;

      if (SCHEME_RPAIRP(obj)) {
        orig_obj = SCHEME_CDR(obj);
        obj = SCHEME_CAR(obj);
      } else {
        orig_obj = obj;
      }

      while (1) {
        /* Like the apply loop around this one, but we need
           to keep track of orig_obj until we get down to the
           structure. */

        type = SCHEME_TYPE(obj);
        if (type == scheme_proc_struct_type) {
          do {
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

            /* After we check arity once, no need to check again
               (which would lead to O(n^2) checking for nested
               struct procs): */
            check_rands = -1;

            DO_CHECK_FOR_BREAK(p, UPDATE_THREAD_RSPTR_FOR_GC(); if (rands == p->tail_buffer) make_tail_buffer_safe(););

            break;
          } while (SAME_TYPE(scheme_proc_struct_type, SCHEME_TYPE(obj)));

          goto apply_top;
        } else {
          if (SCHEME_VECTORP(((Scheme_Chaperone *)obj)->redirects)
              && !(SCHEME_VEC_SIZE(((Scheme_Chaperone *)obj)->redirects) & 1))
            obj = ((Scheme_Chaperone *)obj)->prev;
          else if (SAME_TYPE(SCHEME_TYPE(((Scheme_Chaperone *)obj)->redirects), scheme_nack_guard_evt_type))
            /* Chaperone is for evt, not function arguments */
            obj = ((Scheme_Chaperone *)obj)->prev;
          else {
            /* Chaperone is for function arguments */
            VACATE_TAIL_BUFFER_USE_RUNSTACK();
            UPDATE_THREAD_RSPTR();
            v = scheme_apply_chaperone(scheme_make_raw_pair(obj, orig_obj), num_rands, rands, NULL, 0);
            
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
              RUNSTACK = old_runstack;
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
        goto apply_top;
      } else {
        /* Chaperone is for function arguments */
        VACATE_TAIL_BUFFER_USE_RUNSTACK();
        UPDATE_THREAD_RSPTR();
        v = scheme_apply_chaperone(obj, num_rands, rands, NULL, 0);
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
			     prim->pp.flags & SCHEME_PRIM_IS_METHOD);
	return NULL; /* Shouldn't get here */
      }
      
      v = prim->prim_val(prim->data, num_rands, rands);

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
      case scheme_unclosed_procedure_type:
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
	  ab = SCHEME_LET_AUTOBOX(lv);
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

	  if (SCHEME_LET_AUTOBOX(lv)) {
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
	    GC_CAN_IGNORE Scheme_Closure_Data *data;
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
	    
	    data = (Scheme_Closure_Data *)a[i];
	      
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

      case scheme_quote_syntax_type:
	{
	  GC_CAN_IGNORE Scheme_Quote_Syntax *qs = (Scheme_Quote_Syntax *)obj;
	  Scheme_Prefix *globs;
	  int i, c, pos;

	  i = qs->position;
	  c = qs->depth;
	  pos = qs->midpoint;

	  globs = (Scheme_Prefix *)RUNSTACK[c];
	  v = globs->a[i+pos+1];
	  if (!v) {
	    v = globs->a[pos];
	    v = scheme_delayed_rename((Scheme_Object **)v, i);
	    globs->a[i+pos+1] = v;
	  }

	  goto returnv_never_multi;
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
      case scheme_define_syntaxes_type:
        {
          UPDATE_THREAD_RSPTR();
          v = define_syntaxes_execute(obj);
          break;
        }
      case scheme_begin_for_syntax_type:
        {
          UPDATE_THREAD_RSPTR();
          v = begin_for_syntax_execute(obj);
          break;
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
      case scheme_splice_sequence_type:
        {
          UPDATE_THREAD_RSPTR();
          v = splice_execute(obj);
          break;
        }
      case scheme_require_form_type:
        {
          UPDATE_THREAD_RSPTR();
          v = scheme_top_level_require_execute(obj);
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
      case scheme_case_lambda_sequence_type:
        {
          UPDATE_THREAD_RSPTR();
          v = scheme_case_lambda_execute(obj);
          break;
        }
      case scheme_module_type:
        {
          UPDATE_THREAD_RSPTR();
          v = scheme_module_execute(obj, NULL);
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
    RUNSTACK = old_runstack;
    RUNSTACK_CHANGED();
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

static Scheme_Object *add_renames_unless_module(Scheme_Object *form, Scheme_Env *genv)
{
  if (genv->rename_set) {
    if (SCHEME_STX_PAIRP(form)) {
      Scheme_Object *a, *d, *module_stx;
      
      a = SCHEME_STX_CAR(form);
      if (SCHEME_STX_SYMBOLP(a)) {
	a = scheme_add_rename(a, genv->rename_set);
        module_stx = scheme_datum_to_syntax(scheme_intern_symbol("module"),
                                            scheme_false, 
                                            scheme_sys_wraps_phase(scheme_make_integer(genv->phase)), 
                                            0, 0);
	if (scheme_stx_module_eq(a, module_stx, genv->phase)) {
	  /* Don't add renames to the whole module; let the 
	     module's language take over. */
	  d = SCHEME_STX_CDR(form);
	  a = scheme_make_pair(a, d);
	  form = scheme_datum_to_syntax(a, form, form, 0, 1);
	  return form;
	}
      }
    }
  }

  if (genv->rename_set) {
    form = scheme_add_rename(form, genv->rename_set);
    /* this "phase shift" just attaches the namespace's module registry: */
    form = scheme_stx_phase_shift(form, NULL, NULL, NULL, genv->module_registry->exports, NULL, NULL);
  }

  return form;
}

static Scheme_Object *call_compile_handler(Scheme_Object *form, int immediate_eval)
{
  Scheme_Object *argv[2], *o;

  argv[0] = form;
  argv[1] = (immediate_eval ? scheme_true : scheme_false);
  o = scheme_get_param(scheme_current_config(), MZCONFIG_COMPILE_HANDLER);
  o = scheme_apply(o, 2, argv);
  
  if (!SAME_TYPE(SCHEME_TYPE(o), scheme_compilation_top_type)) {
    argv[0] = o;
    scheme_wrong_contract("compile-handler", "compiled-expression?", 0, -1, argv);
    return NULL;
  }

  return o;
}

static int get_comp_flags(Scheme_Config *config)
{
  int comp_flags = 0;

  if (!config)
    config = scheme_current_config();

  if (SCHEME_TRUEP(scheme_get_param(scheme_current_config(), 
                                    MZCONFIG_ALLOW_SET_UNDEFINED)))
    comp_flags |= COMP_ALLOW_SET_UNDEFINED;
  if (SCHEME_FALSEP(scheme_get_param(scheme_current_config(), 
                                     MZCONFIG_DISALLOW_INLINE)))
    comp_flags |= COMP_CAN_INLINE;

  return comp_flags;
}

static void *compile_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *form;
  int writeable, for_eval, rename, enforce_consts, comp_flags;
  Scheme_Env *genv;
  Scheme_Compile_Info rec, rec2;
  Scheme_Object *o, *rl, *tl_queue;
  Scheme_Compilation_Top *top;
  Resolve_Prefix *rp;
  Resolve_Info *ri;
  Optimize_Info *oi;
  Scheme_Object *gval, *insp;
  Scheme_Comp_Env *cenv;

  form = (Scheme_Object *)p->ku.k.p1;
  genv = (Scheme_Env *)p->ku.k.p2;
  writeable = p->ku.k.i1;
  for_eval = p->ku.k.i2;
  rename = p->ku.k.i3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  if (!SCHEME_STXP(form)) {
    form = scheme_datum_to_syntax(form, scheme_false, scheme_false, 1, 0);
    rename = 1;
  }

  /* Renamings for requires: */
  if (rename) {
    form = add_renames_unless_module(form, genv);
    if (genv->module) {
      form = scheme_stx_phase_shift(form, NULL, 
				    genv->module->me->src_modidx, 
				    genv->module->self_modidx,
				    genv->module_registry->exports,
                                    NULL, NULL);
    }
  }

  tl_queue = scheme_null;

  {
    Scheme_Config *config;
    config = scheme_current_config();
    insp = scheme_get_param(config, MZCONFIG_CODE_INSPECTOR);
    enforce_consts = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_COMPILE_MODULE_CONSTS));
    comp_flags = get_comp_flags(config);
    if (enforce_consts)
      comp_flags |= COMP_ENFORCE_CONSTS;
  }

  while (1) {
    scheme_prepare_compile_env(genv);

    rec.comp = 1;
    rec.dont_mark_local_use = 0;
    rec.resolve_module_ids = !writeable && !genv->module;
    rec.value_name = scheme_false;
    rec.observer = NULL;
    rec.pre_unwrapped = 0;
    rec.env_already = 0;
    rec.comp_flags = comp_flags;

    cenv = scheme_new_comp_env(genv, insp, SCHEME_TOPLEVEL_FRAME);

    if (for_eval) {
      /* Need to look for top-level `begin', and if we
	 find one, break it up to eval first expression
	 before the rest. */
      while (1) {
	scheme_frame_captures_lifts(cenv, scheme_make_lifted_defn, scheme_sys_wraps(cenv), 
                                    scheme_false, scheme_top_level_lifts_key(cenv), scheme_null, scheme_false);
	form = scheme_check_immediate_macro(form,
					    cenv, &rec, 0,
					    0, &gval, NULL, NULL,
                                            1);
	if (SAME_OBJ(gval, scheme_begin_syntax)) {
	  if (scheme_stx_proper_list_length(form) > 1){
	    form = SCHEME_STX_CDR(form);
	    tl_queue = scheme_append(scheme_flatten_syntax_list(form, NULL),
				     tl_queue);
	    tl_queue = scheme_append(scheme_frame_get_lifts(cenv),
				     tl_queue);
	    form = SCHEME_CAR(tl_queue);
	    tl_queue = SCHEME_CDR(tl_queue);
	  } else
	    break;
	} else {
	  rl = scheme_frame_get_require_lifts(cenv);
	  o = scheme_frame_get_lifts(cenv);
	  if (!SCHEME_NULLP(o)
              || !SCHEME_NULLP(rl)) {
	    tl_queue = scheme_make_pair(form, tl_queue);
	    tl_queue = scheme_append(o, tl_queue);
	    tl_queue = scheme_append(rl, tl_queue);
	    form = SCHEME_CAR(tl_queue);
	    tl_queue = SCHEME_CDR(tl_queue);
	  }
	  break;
	}
      }
    }

    if (for_eval) {
      o = call_compile_handler(form, 1);
      top = (Scheme_Compilation_Top *)o;
    } else {
      /* We want to simply compile `form', but we have to loop in case
	 an expression is lifted in the process of compiling: */
      Scheme_Object *l, *prev_o = NULL;
      int max_let_depth;

      while (1) {
	scheme_frame_captures_lifts(cenv, scheme_make_lifted_defn, scheme_sys_wraps(cenv), 
                                    scheme_false, scheme_top_level_lifts_key(cenv), scheme_null, scheme_false);

	scheme_init_compile_recs(&rec, 0, &rec2, 1);

	o = scheme_compile_expr(form, cenv, &rec2, 0);

	/* If we had compiled an expression in a previous iteration,
	   combine it in a sequence: */
	if (prev_o) {
	  Scheme_Sequence *seq;
	  seq = scheme_malloc_sequence(2);
	  seq->so.type = scheme_sequence_type;
	  seq->count = 2;
	  seq->array[0] = o;
	  seq->array[1] = prev_o;
	  o = (Scheme_Object *)seq;
	}

	/* If any definitions were lifted in the process of compiling o,
	   we need to fold them in. */
	l = scheme_frame_get_lifts(cenv);
	rl = scheme_frame_get_require_lifts(cenv);
	if (!SCHEME_NULLP(l)
            || !SCHEME_NULLP(rl)) {
          rl = scheme_append(rl, l);
          rl = icons(scheme_datum_to_syntax(begin_symbol, scheme_false, scheme_sys_wraps(cenv), 0, 0),
                     rl);
          form = scheme_datum_to_syntax(rl, scheme_false, scheme_false, 0, 0);
	  prev_o = o;
	} else 
	  break;
      }

      o = scheme_letrec_check_expr(o);

      oi = scheme_optimize_info_create(cenv->prefix, 1);
      scheme_optimize_info_enforce_const(oi, enforce_consts);
      if (!(comp_flags & COMP_CAN_INLINE))
        scheme_optimize_info_never_inline(oi);
      o = scheme_optimize_expr(o, oi, 0);

      rp = scheme_resolve_prefix(0, cenv->prefix, 1);
      ri = scheme_resolve_info_create(rp);
      scheme_resolve_info_enforce_const(ri, enforce_consts);
      scheme_enable_expression_resolve_lifts(ri);

      o = scheme_resolve_expr(o, ri);
      max_let_depth = scheme_resolve_info_max_let_depth(ri);
      o = scheme_sfs(o, NULL, max_let_depth);

      o = scheme_merge_expression_resolve_lifts(o, rp, ri);

      rp = scheme_remap_prefix(rp, ri);

      top = MALLOC_ONE_TAGGED(Scheme_Compilation_Top);
      top->iso.so.type = scheme_compilation_top_type;
      top->max_let_depth = max_let_depth;
      top->code = o;
      top->prefix = rp;

      if (valdiate_compile_result) {
        scheme_validate_code(NULL, top->code,
                             top->max_let_depth,
                             top->prefix->num_toplevels,
                             top->prefix->num_stxes,
                             top->prefix->num_lifts,
                             NULL,
                             NULL,
                             0);
      }
    }

    if (SCHEME_PAIRP(tl_queue)) {
      /* This compile is interleaved with evaluation,
	 and we need to eval now before compiling more. */
      _eval_compiled_multi_with_prompt((Scheme_Object *)top, genv);

      form = SCHEME_CAR(tl_queue);
      tl_queue = SCHEME_CDR(tl_queue);
    } else
      break;
  }

  return (void *)top;
}

static Scheme_Object *_compile(Scheme_Object *form, Scheme_Env *env, int writeable, int for_eval, int eb, int rename)
{
  Scheme_Thread *p = scheme_current_thread;

  if (SAME_TYPE(SCHEME_TYPE(form), scheme_compilation_top_type))
    return form;

  if (SCHEME_STXP(form)) {
    if (SAME_TYPE(SCHEME_TYPE(SCHEME_STX_VAL(form)), scheme_compilation_top_type))
      return SCHEME_STX_VAL(form);
  }

  p->ku.k.p1 = form;
  p->ku.k.p2 = env;
  p->ku.k.i1 = writeable;
  p->ku.k.i2 = for_eval;
  p->ku.k.i3 = rename;

  return (Scheme_Object *)scheme_top_level_do(compile_k, eb);
}
 
Scheme_Object *scheme_compile(Scheme_Object *form, Scheme_Env *env, int writeable)
{
  return _compile(form, env, writeable, 0, 1, 1);
}

Scheme_Object *scheme_compile_for_eval(Scheme_Object *form, Scheme_Env *env)
{
  return _compile(form, env, 0, 1, 1, 1);
}

Scheme_Object *scheme_eval(Scheme_Object *obj, Scheme_Env *env)
{
  return scheme_eval_compiled(scheme_compile_for_eval(obj, env), env);
}

Scheme_Object *scheme_eval_multi(Scheme_Object *obj, Scheme_Env *env)
{
  return scheme_eval_compiled_multi(scheme_compile_for_eval(obj, env), env);
}

static Scheme_Object *finish_eval_with_prompt(void *_data, int argc, Scheme_Object **argv)
{
  Scheme_Object *data = (Scheme_Object *)_data;
  return _scheme_eval_compiled(SCHEME_CAR(data), (Scheme_Env *)SCHEME_CDR(data));
}

Scheme_Object *scheme_eval_with_prompt(Scheme_Object *obj, Scheme_Env *env)
{
  Scheme_Object *expr;
  expr = scheme_compile_for_eval(obj, env);
  return scheme_call_with_prompt(finish_eval_with_prompt, 
                                 scheme_make_pair(expr, (Scheme_Object *)env));
}

static Scheme_Object *finish_eval_multi_with_prompt(void *_data, int argc, Scheme_Object **argv)
{
  Scheme_Object *data = (Scheme_Object *)_data;
  return _scheme_eval_compiled_multi(SCHEME_CAR(data), (Scheme_Env *)SCHEME_CDR(data));
}

Scheme_Object *scheme_eval_multi_with_prompt(Scheme_Object *obj, Scheme_Env *env)
{
  Scheme_Object *expr;
  expr = scheme_compile_for_eval(obj, env);
  return scheme_call_with_prompt_multi(finish_eval_multi_with_prompt, 
                                       scheme_make_pair(expr, (Scheme_Object *)env));
}

static void *eval_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v, **save_runstack;
  Resolve_Prefix *rp;
  Scheme_Env *env;
  int isexpr, multi, use_jit, as_tail;

  v = (Scheme_Object *)p->ku.k.p1;
  env = (Scheme_Env *)p->ku.k.p2;
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  multi = p->ku.k.i1;
  isexpr = p->ku.k.i2;
  as_tail = p->ku.k.i3;

  {
    Scheme_Object *b;
    b = scheme_get_param(scheme_current_config(), MZCONFIG_USE_JIT);
    use_jit = SCHEME_TRUEP(b);
  }

  if (isexpr) {
    if (multi)
      v = _scheme_eval_linked_expr_multi_wp(v, p);
    else
      v = _scheme_eval_linked_expr_wp(v, p);
  } else if (SAME_TYPE(SCHEME_TYPE(v), scheme_compilation_top_type)) {
    Scheme_Compilation_Top *top = (Scheme_Compilation_Top *)v;
    int depth;

    if (!top->prefix)
      depth = 0;
    else
      depth = top->max_let_depth + scheme_prefix_depth(top->prefix);

    if (!scheme_check_runstack(depth)) {
      p->ku.k.p1 = top;
      p->ku.k.p2 = env;
      p->ku.k.i1 = multi;
      p->ku.k.i2 = 0;
      return (Scheme_Object *)scheme_enlarge_runstack(depth, eval_k);
    }

    v = top->code;

    if (!top->prefix) {
      /* top->code is shared module code */
      scheme_module_execute(top->code, env);
      v = scheme_void;
    } else {
      if (use_jit)
        v = scheme_jit_expr(v);
      else
        v = scheme_eval_clone(v);
      rp = scheme_prefix_eval_clone(top->prefix);

      save_runstack = scheme_push_prefix(env, rp, NULL, NULL, 0, env->phase, NULL, scheme_false);

      if (as_tail) {
        /* Cons up a closure to capture the prefix */
        Scheme_Closure_Data *data;
        mzshort *map;
        int i, sz;

        sz = (save_runstack XFORM_OK_MINUS MZ_RUNSTACK);
        map = (mzshort *)scheme_malloc_atomic(sizeof(mzshort) * sz);
        for (i = 0; i < sz; i++) {
          map[i] = i;
        }

        data = MALLOC_ONE_TAGGED(Scheme_Closure_Data);
        data->iso.so.type = scheme_compiled_unclosed_procedure_type;
        data->num_params = 0;
        data->max_let_depth = top->max_let_depth + sz;
        data->closure_size = sz;
        data->closure_map = map;
        data->code = v;

        v = scheme_make_closure(p, (Scheme_Object *)data, 1);

        v = _scheme_tail_apply(v, 0, NULL);
      } else if (multi)
        v = _scheme_eval_linked_expr_multi_wp(v, p);
      else
        v = _scheme_eval_linked_expr_wp(v, p);

      scheme_pop_prefix(save_runstack);
    }
  } else {
    v = scheme_void;
  }

  return (void *)v;
}

static Scheme_Object *_eval(Scheme_Object *obj, Scheme_Env *env, 
			    int isexpr, int multi, int top, int as_tail)
{
  Scheme_Thread *p = scheme_current_thread;
  
  p->ku.k.p1 = obj;
  p->ku.k.p2 = env;
  p->ku.k.i1 = multi;
  p->ku.k.i2 = isexpr;
  p->ku.k.i3 = as_tail;

  if (top)
    return (Scheme_Object *)scheme_top_level_do(eval_k, 1);
  else
    return (Scheme_Object *)eval_k();
}

Scheme_Object *scheme_eval_compiled(Scheme_Object *obj, Scheme_Env *env)
{
  return _eval(obj, env, 0, 0, 1, 0);
}

Scheme_Object *scheme_eval_compiled_multi(Scheme_Object *obj, Scheme_Env *env)
{
  return _eval(obj, env, 0, 1, 1, 0);
}

Scheme_Object *_scheme_eval_compiled(Scheme_Object *obj, Scheme_Env *env)
{
  return _eval(obj, env, 0, 0, 0, 0);
}

Scheme_Object *_scheme_eval_compiled_multi(Scheme_Object *obj, Scheme_Env *env)
{
  return _eval(obj, env, 0, 1, 0, 0);
}

static Scheme_Object *finish_compiled_multi_with_prompt(void *_data, int argc, Scheme_Object **argv)
{
  Scheme_Object *data = (Scheme_Object *)_data;
  return _eval(SCHEME_CAR(data), (Scheme_Env *)SCHEME_CDR(data), 0, 1, 0, 0);
}

Scheme_Object *_eval_compiled_multi_with_prompt(Scheme_Object *obj, Scheme_Env *env)
{
  return _scheme_call_with_prompt_multi(finish_compiled_multi_with_prompt,
                                        scheme_make_pair(obj, (Scheme_Object *)env));
}

Scheme_Object *scheme_eval_linked_expr(Scheme_Object *obj)
{
  return _eval(obj, NULL, 1, 0, 1, 0);
}

Scheme_Object *scheme_eval_linked_expr_multi(Scheme_Object *obj)
{
  return _eval(obj, NULL, 1, 1, 1, 0);
}

Scheme_Object *scheme_eval_linked_expr_multi_with_dynamic_state(Scheme_Object *obj, Scheme_Dynamic_State *dyn_state)
{
  Scheme_Thread *p = scheme_current_thread;
  
  p->ku.k.p1 = obj;
  p->ku.k.p2 = NULL;
  p->ku.k.i1 = 1;
  p->ku.k.i2 = 1;
  p->ku.k.i3 = 0;

    return (Scheme_Object *)scheme_top_level_do_worker(eval_k, 1, 0, dyn_state);
}

/* for mzc: */
Scheme_Object *scheme_load_compiled_stx_string(const char *str, intptr_t len)
{
  Scheme_Object *port, *expr;

  port = scheme_make_sized_byte_string_input_port(str, -len);

  expr = scheme_internal_read(port, NULL, 1, 0, 0, 0, -1, NULL, NULL, NULL, NULL);

  expr = _scheme_eval_compiled(expr, scheme_get_env(NULL));

  /* Unwrap syntax once; */
  expr = SCHEME_STX_VAL(expr);

  return expr;
}

/* for mzc: */
Scheme_Object *scheme_compiled_stx_symbol(Scheme_Object *stx)
{
  return SCHEME_STX_VAL(stx);
}

/* for mzc: */
Scheme_Object *scheme_eval_compiled_stx_string(Scheme_Object *expr, Scheme_Env *env,
					       intptr_t shift, Scheme_Object *modidx)
{
  /* If modidx, then last element is a module index; shift the rest. */
  if (modidx) {
    int i, len = SCHEME_VEC_SIZE(expr);
    Scheme_Object *orig = SCHEME_VEC_ELS(expr)[len - 1], *s, *result;

    orig = SCHEME_STX_VAL(orig);
    result = scheme_make_vector(len - 1, NULL);

    for (i = 0; i < len - 1; i++) {
      s = scheme_stx_phase_shift(SCHEME_VEC_ELS(expr)[i], scheme_make_integer(shift), orig, modidx, 
                                 env->module_registry->exports, NULL, NULL);
      SCHEME_VEC_ELS(result)[i] = s;
    }
    
    return result;
  } else
    return expr;
}

static Scheme_Object *add_lifts_as_begin(Scheme_Object *obj, Scheme_Object *l, Scheme_Comp_Env *env)
{
  obj = scheme_append(l, scheme_make_pair(obj, scheme_null));
  obj = icons(scheme_datum_to_syntax(begin_symbol, scheme_false, scheme_sys_wraps(env), 0, 0),
              obj);
  obj = scheme_datum_to_syntax(obj, scheme_false, scheme_false, 0, 0);
  return obj;
}

static void *expand_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *obj, *observer, *catch_lifts_key;
  Scheme_Comp_Env *env, **ip;
  Scheme_Expand_Info erec1;
  int depth, rename, just_to_top, as_local, comp_flags;

  obj = (Scheme_Object *)p->ku.k.p1;
  env = (Scheme_Comp_Env *)p->ku.k.p2;
  depth = p->ku.k.i1;
  rename = p->ku.k.i2;
  just_to_top = p->ku.k.i3;
  catch_lifts_key = p->ku.k.p4;
  as_local = p->ku.k.i4; /* < 0 => catch lifts to let */

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  if (SCHEME_FALSEP(catch_lifts_key))
    catch_lifts_key = scheme_top_level_lifts_key(env);

  if (!SCHEME_STXP(obj))
    obj = scheme_datum_to_syntax(obj, scheme_false, scheme_false, 1, 0);

  if (rename > 0) {
    /* Renamings for requires: */
    obj = add_renames_unless_module(obj, env->genv);
  }

  observer = scheme_get_expand_observe();
  SCHEME_EXPAND_OBSERVE_START_EXPAND(observer);

  comp_flags = get_comp_flags(NULL);

  if (as_local < 0) {
    /* Insert a dummy frame so that `pair_lifted' can add more. */
    env = scheme_new_compilation_frame(0, 0, env);
    ip = MALLOC_N(Scheme_Comp_Env *, 1);
    *ip = env;
  }  else
    ip = NULL;

  scheme_prepare_compile_env(env->genv);

  /* Loop for lifted expressions: */
  while (1) {
    erec1.comp = 0;
    erec1.depth = depth;
    erec1.value_name = scheme_false;
    erec1.observer = observer;
    erec1.pre_unwrapped = 0;
    erec1.env_already = 0;
    erec1.comp_flags = comp_flags;

    if (catch_lifts_key) {
      Scheme_Object *data;
      data = (as_local < 0) ? (Scheme_Object *)ip : scheme_sys_wraps(env);
      scheme_frame_captures_lifts(env, 
                                  (as_local < 0) ? scheme_pair_lifted : scheme_make_lifted_defn, 
                                  data, 
                                  scheme_false, catch_lifts_key, 
                                  (!as_local && catch_lifts_key) ? scheme_null : NULL,
                                  scheme_false);
    }

    if (just_to_top) {
      Scheme_Object *gval;
      obj = scheme_check_immediate_macro(obj, env, &erec1, 0, 0, &gval, NULL, NULL, 1);
    } else
      obj = scheme_expand_expr(obj, env, &erec1, 0);

    if (catch_lifts_key) {
      Scheme_Object *l, *rl;
      l = scheme_frame_get_lifts(env);
      rl = scheme_frame_get_require_lifts(env);
      if (SCHEME_PAIRP(l)
          || SCHEME_PAIRP(rl)) {
        l = scheme_append(rl, l);
        if (as_local < 0)
          obj = scheme_add_lifts_as_let(obj, l, env, scheme_false, 0);
        else
          obj = add_lifts_as_begin(obj, l, env);
        SCHEME_EXPAND_OBSERVE_LIFT_LOOP(erec1.observer,obj);
	if ((depth >= 0) || as_local)
	  break;
      } else {
        if (as_local > 0) {
          obj = add_lifts_as_begin(obj, scheme_null, env);
          SCHEME_EXPAND_OBSERVE_LIFT_LOOP(erec1.observer,obj);
        }
	break;
      }
    } else
      break;
  }

  if (rename && !just_to_top) {
    /* scheme_simplify_stx(obj, scheme_new_stx_simplify_cache()); */ /* too expensive */
  }

  return obj;
}

static Scheme_Object *r_expand(Scheme_Object *obj, Scheme_Comp_Env *env, 
			       int depth, int rename, int just_to_top, 
			       Scheme_Object *catch_lifts_key, int eb,
			       int as_local)
  /* as_local < 0 => catch lifts to let */
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = obj;
  p->ku.k.p2 = env;
  p->ku.k.i1 = depth;
  p->ku.k.i2 = rename;
  p->ku.k.i3 = just_to_top;
  p->ku.k.p4 = catch_lifts_key;
  p->ku.k.i4 = as_local;

  return (Scheme_Object *)scheme_top_level_do(expand_k, eb);
}

Scheme_Object *scheme_expand(Scheme_Object *obj, Scheme_Env *env)
{
  return r_expand(obj, scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
		  -1, 1, 0, scheme_false, -1, 0);
}

Scheme_Object *scheme_tail_eval_expr(Scheme_Object *obj)
{
  return scheme_tail_eval(obj);
}

/* local functions */

static Scheme_Object *
sch_eval(const char *who, int argc, Scheme_Object *argv[])
{
  if (argc == 1) {
    return _scheme_tail_apply(scheme_get_param(scheme_current_config(), MZCONFIG_EVAL_HANDLER),
                              1, argv);
  } else {
    Scheme_Config *config;
    
    if (SCHEME_TYPE(argv[1]) != scheme_namespace_type)
      scheme_wrong_contract(who, "namespace?", 1, argc, argv);

    config = scheme_extend_config(scheme_current_config(),
				  MZCONFIG_ENV,
				  argv[1]);
    scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);

    return _scheme_tail_apply(scheme_get_param(config, MZCONFIG_EVAL_HANDLER),
                              1, argv);
  }
}

static Scheme_Object *
eval(int argc, Scheme_Object *argv[])
{
  Scheme_Object *a[2], *form;

  form = argv[0];
  if (SCHEME_STXP(form)
      && !SAME_TYPE(SCHEME_TYPE(SCHEME_STX_VAL(form)), scheme_compilation_top_type)) {
    Scheme_Env *genv;
    if (argc > 1) {
      if (SCHEME_TYPE(argv[1]) != scheme_namespace_type)
	scheme_wrong_contract("eval", "namespace?", 1, argc, argv);
      genv = (Scheme_Env *)argv[1];
    } else
      genv = scheme_get_env(NULL);
    form = add_renames_unless_module(form, genv);
  }

  a[0] = form;
  if (argc > 1)
    a[1] = argv[1];  
  return sch_eval("eval", argc, a);
}

static Scheme_Object *
eval_stx(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_STXP(argv[0])) {
    scheme_wrong_contract("eval-syntax", "syntax?", 0, argc, argv);
    return NULL;
  }
  
  return sch_eval("eval-syntax", argc, argv);
}

Scheme_Object *
scheme_default_eval_handler(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;
  Scheme_Object *v;

  env = scheme_get_env(NULL);

  v = _compile(argv[0], env, 0, 1, 0, 0);

  /* Returns a tail apply: */
  return _eval(v, env, 0, 1, 0, 1);
}

Scheme_Object *
scheme_default_compile_handler(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  env = scheme_get_env(NULL);

  return _compile(argv[0], env, SCHEME_FALSEP(argv[1]), 0, 0, 0);
}

static Scheme_Object *
current_eval(int argc, Scheme_Object **argv)
{
  return scheme_param_config("current-eval", 
			     scheme_make_integer(MZCONFIG_EVAL_HANDLER),
			     argc, argv,
			     1, NULL, NULL, 0);
}

static Scheme_Object *
current_compile(int argc, Scheme_Object **argv)
{
  return scheme_param_config("current-compile", 
			     scheme_make_integer(MZCONFIG_COMPILE_HANDLER),
			     argc, argv,
			     2, NULL, NULL, 0);
}

static Scheme_Object *
top_introduce_stx(int argc, Scheme_Object **argv)
{
  Scheme_Object *form;

  if (!SCHEME_STXP(argv[0])) {
    scheme_wrong_contract("namespace-syntax-introduce", "syntax?", 0, argc, argv);
    return NULL;
  }
  
  form = argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(SCHEME_STX_VAL(form)), scheme_compilation_top_type)) {
    Scheme_Env *genv;
    genv = (Scheme_Env *)scheme_get_param(scheme_current_config(), MZCONFIG_ENV);
    form = add_renames_unless_module(form, genv);
  }

  return form;
}

Scheme_Object *scheme_datum_to_kernel_stx(Scheme_Object *e)
{
  return scheme_datum_to_syntax(e, scheme_false, scheme_sys_wraps(NULL), 0, 0);
}

static Scheme_Object *
compile(int argc, Scheme_Object *argv[])
{
  Scheme_Object *form = argv[0];
  Scheme_Env *genv;

  if (!SCHEME_STXP(form))
    form = scheme_datum_to_syntax(form, scheme_false, scheme_false, 1, 0);

  genv = scheme_get_env(NULL);
  form = add_renames_unless_module(form, genv);

  return call_compile_handler(form, 0);
}

static Scheme_Object *
compile_stx(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("compile-syntax", "syntax?", 0, argc, argv);

  return call_compile_handler(argv[0], 0);
}

static Scheme_Object *
compiled_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_compilation_top_type)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *expand(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  env = scheme_get_env(NULL);

  return r_expand(argv[0], scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
		  -1, 1, 0, scheme_false, 0, 0);
}

static Scheme_Object *expand_stx(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("expand-syntax", "syntax?", 0, argc, argv);

  env = scheme_get_env(NULL);
  
  return r_expand(argv[0], scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
		  -1, -1, 0, scheme_false, 0, 0);
}

Scheme_Object *scheme_generate_lifts_key(void)
{
  char buf[20];
  sprintf(buf, "lifts%d", generate_lifts_count++);
  return scheme_make_symbol(buf); /* uninterned */
}

Scheme_Object *scheme_top_level_lifts_key(Scheme_Comp_Env *env)
{
  if (!env->genv->lift_key) {
    Scheme_Object *o;
    o = scheme_generate_lifts_key();
    env->genv->lift_key = o;
  }
  return env->genv->lift_key;
}

Scheme_Object *
scheme_make_lifted_defn(Scheme_Object *sys_wraps, Scheme_Object **_ids, Scheme_Object *expr, Scheme_Comp_Env *env)
{
  Scheme_Object *l, *ids, *id;

  /* Registers marked ids: */
  for (ids = *_ids; !SCHEME_NULLP(ids); ids = SCHEME_CDR(ids)) {
    id = SCHEME_CAR(ids);
    scheme_tl_id_sym(env->genv, id, scheme_false, 2, NULL, NULL);
  }

  l = icons(scheme_datum_to_syntax(define_values_symbol, scheme_false, sys_wraps, 0, 0), 
	    icons(*_ids,
		  icons(expr,
			scheme_null)));

  return scheme_datum_to_syntax(l, scheme_false, scheme_false, 0, 0);
}

static Scheme_Object *add_intdef_renamings(Scheme_Object *l, Scheme_Object *renaming)
{
  Scheme_Object *rl = renaming;

  if (SCHEME_PAIRP(renaming)) {
    int need_delim;
    need_delim = !SCHEME_NULLP(SCHEME_CDR(rl));
    if (need_delim)
      l = scheme_add_rib_delimiter(l, scheme_null);
    while (!SCHEME_NULLP(rl)) {
      l = scheme_add_rename(l, SCHEME_CAR(rl));
      rl = SCHEME_CDR(rl);
    }
    if (need_delim)
      l = scheme_add_rib_delimiter(l, renaming);
  } else {
    l = scheme_add_rename(l, renaming);
  }

  return l;
}

static void update_intdef_chain(Scheme_Object *intdef)
{
  Scheme_Comp_Env *orig, *current_next;
  Scheme_Object *base;

  /* If this intdef chains to another, and if the other has been
     extended, then fix up the chain. */

  while (1) {
    base = (Scheme_Object *)((void **)SCHEME_PTR1_VAL(intdef))[1];
    if (base) {
      current_next = (Scheme_Comp_Env *)((void **)SCHEME_PTR1_VAL(base))[0];
      orig = (Scheme_Comp_Env *)((void **)SCHEME_PTR1_VAL(intdef))[2];
      if (orig) {
        orig->next = current_next;
      } else {
        ((void **)SCHEME_PTR1_VAL(base))[0] = current_next;
      }
      intdef = base;
    } else {
      break;
    }
  }
}

static Scheme_Object *
do_local_expand(const char *name, int for_stx, int catch_lifts, int for_expr, int argc, Scheme_Object **argv)
{
  Scheme_Comp_Env *env, *orig_env, **ip;
  Scheme_Object *l, *local_mark, *renaming = NULL, *orig_l, *exp_expr = NULL;
  int cnt, pos, kind, is_modstar;
  int bad_sub_env = 0, bad_intdef = 0;
  Scheme_Object *observer, *catch_lifts_key = NULL;

  env = scheme_current_thread->current_local_env;
  orig_env = env;

  if (!env)
    scheme_contract_error(name,
                          "not currently transforming",
                          NULL);

  if (for_stx) {
    scheme_prepare_exp_env(env->genv);
    env = scheme_new_comp_env(env->genv->exp_env, env->insp, 0);
    scheme_propagate_require_lift_capture(orig_env, env);
  }
  scheme_prepare_compile_env(env->genv);

  if (for_expr)
    kind = 0; /* expression */
  else if (!for_stx && SAME_OBJ(argv[1], module_symbol))
    kind = SCHEME_MODULE_BEGIN_FRAME; /* name is backwards compared to symbol! */
  else if (!for_stx && SAME_OBJ(argv[1], module_begin_symbol))
    kind = SCHEME_MODULE_FRAME; /* name is backwards compared to symbol! */
  else if (SAME_OBJ(argv[1], top_level_symbol)) {
    kind = SCHEME_TOPLEVEL_FRAME;
    if (catch_lifts < 0) catch_lifts = 0;
  } else if (SAME_OBJ(argv[1], expression_symbol))
    kind = 0;
  else if (scheme_proper_list_length(argv[1]) > 0)
    kind = SCHEME_INTDEF_FRAME;
  else  {
    scheme_wrong_contract(name,
                          (for_stx
                           ? "(or/c 'expression 'top-level (and/c pair? list?))"
                           : "(or/c 'expression 'module 'module-begin 'top-level (and/c pair? list?))"),
                          1, argc, argv);
    return NULL;
  }

  if (argc > 3) {
    if (SCHEME_TRUEP(argv[3])) {
      if (SAME_TYPE(scheme_intdef_context_type, SCHEME_TYPE(argv[3]))) {
	Scheme_Comp_Env *stx_env;
        update_intdef_chain(argv[3]);
	stx_env = (Scheme_Comp_Env *)((void **)SCHEME_PTR1_VAL(argv[3]))[0];
	renaming = SCHEME_PTR2_VAL(argv[3]);
	if (!scheme_is_sub_env(stx_env, env))
	  bad_sub_env = 1;
	env = stx_env;
      } else if (SCHEME_PAIRP(argv[3])) {
        Scheme_Object *rl = argv[3];
        while (SCHEME_PAIRP(rl)) {
          if (SAME_TYPE(scheme_intdef_context_type, SCHEME_TYPE(SCHEME_CAR(rl)))) {
            Scheme_Comp_Env *stx_env;
            stx_env = (Scheme_Comp_Env *)((void **)SCHEME_PTR1_VAL(SCHEME_CAR(rl)))[0];
            if (!scheme_is_sub_env(stx_env, env))
              bad_sub_env = 1;
          } else
            break;
          rl = SCHEME_CDR(rl);
        }
        if (!SCHEME_NULLP(rl))
          bad_intdef = 1;
        else {
          rl = argv[3];
          update_intdef_chain(SCHEME_CAR(rl));
          env = (Scheme_Comp_Env *)((void **)SCHEME_PTR1_VAL(SCHEME_CAR(rl)))[0];
          if (SCHEME_NULLP(SCHEME_CDR(rl)))
            renaming = SCHEME_PTR2_VAL(SCHEME_CAR(rl));
          else {
            /* reverse and extract: */
            renaming = scheme_null;
            while (!SCHEME_NULLP(rl)) {
              renaming = cons(SCHEME_PTR2_VAL(SCHEME_CAR(rl)), renaming);
              rl = SCHEME_CDR(rl);
            }
          }
        }
      } else
        bad_intdef = 1;
    }

    if (argc > 4) {
      /* catch_lifts */
      catch_lifts_key = argv[4];
    }
  }

  if (catch_lifts && !catch_lifts_key)
    catch_lifts_key = scheme_generate_lifts_key();

  /* For each given stop-point identifier, shadow any potential syntax
     in the environment with an identity-expanding syntax expander. */

  (void)scheme_get_stop_expander();

  env = scheme_new_compilation_frame(0, (SCHEME_CAPTURE_WITHOUT_RENAME 
					 | SCHEME_FOR_STOPS
					 | kind), 
				     env);
  if (catch_lifts < 0) {
    /* Note: extra frames can get inserted after env by pair_lifted */
    ip = MALLOC_N(Scheme_Comp_Env *, 1);
    *ip = env;
  } else
    ip = NULL;

  if (kind == SCHEME_INTDEF_FRAME)
    env->intdef_name = argv[1];
  env->in_modidx = scheme_current_thread->current_local_modidx;

  local_mark = scheme_current_thread->current_local_mark;
  
  if (for_expr) {
  } else if (SCHEME_TRUEP(argv[2])) {
#   define NUM_CORE_EXPR_STOP_FORMS 15
    cnt = scheme_stx_proper_list_length(argv[2]);

    if (cnt == 1)
      is_modstar = scheme_stx_module_eq_x(scheme_modulestar_stx, SCHEME_CAR(argv[2]), env->genv->phase);
    else
      is_modstar = 0;

    if (cnt > 0) {
      if (!is_modstar)
        cnt += NUM_CORE_EXPR_STOP_FORMS;
      scheme_add_local_syntax(cnt, env);
    }
    pos = 0;

    for (l = argv[2]; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      Scheme_Object *i;
    
      i = SCHEME_CAR(l);
      if (!SCHEME_STXP(i) || !SCHEME_STX_SYMBOLP(i)) {
        scheme_wrong_contract(name, "(or/c #f (listof identifier?))", 2, argc, argv);
        return NULL;
      }
    
      if (cnt > 0)
        scheme_set_local_syntax(pos++, i, scheme_get_stop_expander(), env);
    }
    if (!SCHEME_NULLP(l)) {
      scheme_wrong_contract(name, "(or/c #f (listof identifier?))", 2, argc, argv);
      return NULL;
    }

    if ((cnt > 0) && !is_modstar) {
      scheme_add_core_stop_form(pos++, begin_symbol, env);
      scheme_add_core_stop_form(pos++, scheme_intern_symbol("set!"), env);
      scheme_add_core_stop_form(pos++, app_symbol, env);
      scheme_add_core_stop_form(pos++, top_symbol, env);
      scheme_add_core_stop_form(pos++, lambda_symbol, env);
      scheme_add_core_stop_form(pos++, scheme_intern_symbol("case-lambda"), env);
      scheme_add_core_stop_form(pos++, let_values_symbol, env);
      scheme_add_core_stop_form(pos++, letrec_values_symbol, env);
      scheme_add_core_stop_form(pos++, scheme_intern_symbol("if"), env);
      scheme_add_core_stop_form(pos++, scheme_intern_symbol("begin0"), env);
      scheme_add_core_stop_form(pos++, scheme_intern_symbol("with-continuation-mark"), env);
      scheme_add_core_stop_form(pos++, letrec_syntaxes_symbol, env);
      scheme_add_core_stop_form(pos++, scheme_intern_symbol("#%variable-reference"), env);
      scheme_add_core_stop_form(pos++, scheme_intern_symbol("#%expression"), env);
      scheme_add_core_stop_form(pos++, quote_symbol, env);
    }
  }

  /* Report errors related to 3rd argument, finally */
  if (argc > 3) {
    if (bad_intdef) {
      scheme_wrong_contract(name, "(or/c internal-definition-context? (non-empty-listof internal-definition-context?) #f)",
                            3, argc, argv);
      return NULL;
    } else if (bad_sub_env) {
      scheme_contract_error(name, 
                            "transforming context does not match internal-definition context",
                            NULL);
      return NULL;
    }
  }

  l = argv[0];

  if (!SCHEME_STXP(l))
    l = scheme_datum_to_syntax(l, scheme_false, scheme_false, 1, 0);

  orig_l = l;

  observer = scheme_get_expand_observe();
  if (observer) {
    SCHEME_EXPAND_OBSERVE_ENTER_LOCAL(observer, l);
    if (for_stx) {
      SCHEME_EXPAND_OBSERVE_PHASE_UP(observer);
    }
  }

  if (local_mark) {
    /* Since we have an expression from local context,
       we need to remove the temporary mark... */
    l = scheme_add_remove_mark(l, local_mark);
  }

  if (renaming)
    l = add_intdef_renamings(l, renaming);

  SCHEME_EXPAND_OBSERVE_LOCAL_PRE(observer, l);

  if (SCHEME_FALSEP(argv[2])) {
    Scheme_Object *xl, *gval;
    Scheme_Compile_Expand_Info drec[1];

    if (catch_lifts_key) {
      Scheme_Object *data;
      data = (catch_lifts < 0) ? (Scheme_Object *)ip : scheme_sys_wraps(env);
      scheme_frame_captures_lifts(env, 
                                  (catch_lifts < 0) ? scheme_pair_lifted : scheme_make_lifted_defn, 
                                  data,
                                  scheme_top_level_lifts_key(env),
                                  catch_lifts_key, NULL,
                                  scheme_false);
    }

    memset(drec, 0, sizeof(drec));
    drec[0].value_name = scheme_false; /* or scheme_current_thread->current_local_name ? */
    drec[0].depth = -2;
    drec[0].observer = observer;
    {
      int comp_flags;
      comp_flags = get_comp_flags(NULL);
      drec[0].comp_flags = comp_flags;
    }

    xl = scheme_check_immediate_macro(l, env, drec, 0, 0, &gval, NULL, NULL, 1);

    if (SAME_OBJ(xl, l) && !for_expr) {
      SCHEME_EXPAND_OBSERVE_LOCAL_POST(observer, xl);
      SCHEME_EXPAND_OBSERVE_EXIT_LOCAL(observer, orig_l);
      return orig_l;
    }

    if (catch_lifts_key) {
      if (catch_lifts < 0)
        xl = scheme_add_lifts_as_let(xl, scheme_frame_get_lifts(env), env, orig_l, 0);
      else
        xl = add_lifts_as_begin(xl, scheme_frame_get_lifts(env), env);
      SCHEME_EXPAND_OBSERVE_LIFT_LOOP(observer,xl);
    }

    l = xl;
  } else {
    /* Expand the expression. depth = -2 means expand all the way, but
       preserve letrec-syntax. */
    l = r_expand(l, env, -2, 0, 0, catch_lifts_key, 0, catch_lifts ? catch_lifts : 1);
  }

  SCHEME_EXPAND_OBSERVE_LOCAL_POST(observer, l);

  if (renaming)
    l = add_intdef_renamings(l, renaming);

  if (for_expr) {
    /* Package up expanded expr with the environment. */
    while (1) {
      if (orig_env->flags & SCHEME_FOR_STOPS)
        orig_env = orig_env->next;
      else if ((orig_env->flags & SCHEME_INTDEF_FRAME)
               && !orig_env->num_bindings)
        orig_env = orig_env->next;
      else
        break;
    }
    exp_expr = scheme_alloc_object();
    exp_expr->type = scheme_expanded_syntax_type;
    SCHEME_PTR1_VAL(exp_expr) = l;
    SCHEME_PTR2_VAL(exp_expr) = orig_env;
    exp_expr = scheme_datum_to_syntax(exp_expr, l, scheme_false, 0, 0);
    if (local_mark)
      exp_expr = scheme_add_remove_mark(exp_expr, local_mark);
  }

  if (local_mark) {
    /* Put the temporary mark back: */
    l = scheme_add_remove_mark(l, local_mark);
  }

  if (for_expr) {
    Scheme_Object *a[2];
    SCHEME_EXPAND_OBSERVE_OPAQUE_EXPR(observer, exp_expr);
    SCHEME_EXPAND_OBSERVE_EXIT_LOCAL(observer, l);
    a[0] = l;
    a[1] = exp_expr;
    return scheme_values(2, a);
  } else {
    SCHEME_EXPAND_OBSERVE_EXIT_LOCAL(observer, l);
    if (kind == SCHEME_MODULE_FRAME)
      l = scheme_annotate_existing_submodules(l, 0);
    return l;
  }
}


static Scheme_Object *
local_expand(int argc, Scheme_Object **argv)
{
  return do_local_expand("local-expand", 0, 0, 0, argc, argv);
}

static Scheme_Object *
local_expand_expr(int argc, Scheme_Object **argv)
{
  return do_local_expand("syntax-local-expand-expression", 0, 0, 1, argc, argv);
}

static Scheme_Object *
local_transformer_expand(int argc, Scheme_Object **argv)
{
  return do_local_expand("local-transformer-expand", 1, -1, 0, argc, argv);
}

static Scheme_Object *
local_expand_catch_lifts(int argc, Scheme_Object **argv)
{
  return do_local_expand("local-expand/capture-lifts", 0, 1, 0, argc, argv);
}

static Scheme_Object *
local_transformer_expand_catch_lifts(int argc, Scheme_Object **argv)
{
  return do_local_expand("local-transformer-expand/capture-lifts", 1, 1, 0, argc, argv);
}

static Scheme_Object *
expand_once(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  env = scheme_get_env(NULL);

  return r_expand(argv[0], scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
		  1, 1, 0, scheme_false, 0, 0);
}

static Scheme_Object *
expand_stx_once(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("expand-syntax-once", "syntax?", 0, argc, argv);
  
  env = scheme_get_env(NULL);

  return r_expand(argv[0], scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
		  1, -1, 0, scheme_false, 0, 0);
}

static Scheme_Object *
expand_to_top_form(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  env = scheme_get_env(NULL);

  return r_expand(argv[0], scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
		  1, 1, 1, scheme_false, 0, 0);
}

static Scheme_Object *
expand_stx_to_top_form(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("expand-syntax-to-top", "syntax?", 0, argc, argv);
  
  env = scheme_get_env(NULL);

  return r_expand(argv[0], scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
		  1, -1, 1, scheme_false, 0, 0);
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
    expr = scheme_read_syntax(port, scheme_false);

    if (cont == -2) {
      if (SCHEME_STXP(expr)) {
        Scheme_Object *m;
        m = SCHEME_STX_VAL(expr);
        if (SCHEME_PAIRP(m)) {
          m = scheme_make_pair(scheme_datum_to_syntax(module_symbol, 
                                                      SCHEME_CAR(m), 
                                                      scheme_sys_wraps(NULL), 
                                                      0, 0),
                               SCHEME_CDR(m));
          expr = scheme_datum_to_syntax(m, expr, expr, 0, 1);
        }
      }
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
  Scheme_Object *s, *e, *a[3], *eload;
  eload = scheme_builtin_value("embedded-load");
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
  if (predefined)
    scheme_starting_up = 1;
  (void)scheme_apply(eload, 3, a);
  if (predefined)
    scheme_starting_up = 0;
}

void scheme_init_collection_paths_post(Scheme_Env *global_env, Scheme_Object *extra_dirs, Scheme_Object *post_dirs)
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

void scheme_init_collection_paths(Scheme_Env *global_env, Scheme_Object *extra_dirs)
{
  scheme_init_collection_paths_post(global_env, extra_dirs, scheme_null);
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

static Scheme_Object *
local_eval(int argc, Scheme_Object **argv)
{
  Scheme_Comp_Env *env, *stx_env, *old_stx_env;
  Scheme_Object *l, *a, *rib, *expr, *names, *rn_names, *observer;
  int cnt = 0, pos;

  observer = scheme_get_expand_observe();
  SCHEME_EXPAND_OBSERVE_LOCAL_BIND(observer, argv[0]);

  names = argv[0];
  for (l = names; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    a = SCHEME_CAR(l);
    if (!SCHEME_STXP(a) || !SCHEME_STX_SYMBOLP(a))
      break;
    cnt++;
  }
  if (!SCHEME_NULLP(l))
    scheme_wrong_contract("syntax-local-bind-syntaxes", "(listof identifier?)", 0, argc, argv);

  expr = argv[1];
  if (!SCHEME_FALSEP(expr) && !SCHEME_STXP(expr))
    scheme_wrong_contract("syntax-local-bind-syntaxes", "(or/c syntax? #f)", 1, argc, argv);
  if (!SAME_TYPE(scheme_intdef_context_type, SCHEME_TYPE(argv[2])))
    scheme_wrong_contract("syntax-local-bind-syntaxes", "internal-definition-context?", 2, argc, argv);

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_contract_error("syntax-local-bind-syntaxes",
                          "not currently transforming",
                          NULL);

  update_intdef_chain(argv[2]);
  stx_env = (Scheme_Comp_Env *)((void **)SCHEME_PTR1_VAL(argv[2]))[0];
  rib = SCHEME_PTR2_VAL(argv[2]);

  if (*scheme_stx_get_rib_sealed(rib)) {
    scheme_contract_error("syntax-local-bind-syntaxes",
                          "given internal-definition context has been sealed",
                          NULL);
  }
  
  if (!scheme_is_sub_env(stx_env, env)) {
    scheme_contract_error("syntax-local-bind-syntaxes",
                          "transforming context does not match given internal-definition context",
                          NULL);
  }

  old_stx_env = stx_env;
  stx_env = scheme_new_compilation_frame(0, SCHEME_FOR_INTDEF, stx_env);
  scheme_add_local_syntax(cnt, stx_env);

  /* Mark names */
  if (scheme_current_thread->current_local_mark)
    names = scheme_named_map_1(NULL, scheme_add_remove_mark, names,
                               scheme_current_thread->current_local_mark);

  SCHEME_EXPAND_OBSERVE_RENAME_LIST(observer,names);

  /* Initialize environment slots to #f, which means "not syntax". */
  cnt = 0;
  for (l = names; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    scheme_set_local_syntax(cnt++, SCHEME_CAR(l), scheme_false, stx_env);
  }
	  
  /* Extend shared rib with renamings */
  scheme_add_env_renames(rib, stx_env, old_stx_env);

  stx_env->in_modidx = scheme_current_thread->current_local_modidx;
  if (!SCHEME_FALSEP(expr)) {
    Scheme_Compile_Expand_Info rec;
    rec.comp = 0;
    rec.depth = -1;
    rec.value_name = scheme_false;
    rec.observer = observer;
    rec.pre_unwrapped = 0;
    rec.env_already = 0;
    rec.comp_flags = get_comp_flags(NULL);
    
    /* Evaluate and bind syntaxes */
    if (scheme_current_thread->current_local_mark)
      expr = scheme_add_remove_mark(expr, scheme_current_thread->current_local_mark);

    scheme_prepare_exp_env(stx_env->genv);
    scheme_prepare_compile_env(stx_env->genv->exp_env);
    pos = 0;
    expr = scheme_add_rename_rib(expr, rib);
    rn_names = scheme_named_map_1(NULL, scheme_add_rename_rib, names, rib);
    scheme_bind_syntaxes("local syntax definition", rn_names, expr,
			 stx_env->genv->exp_env, stx_env->insp, &rec, 0,
			 stx_env, stx_env,
			 &pos, rib);
  }

  /* Remember extended environment */
  ((void **)SCHEME_PTR1_VAL(argv[2]))[0] = stx_env;
  if (!((void **)SCHEME_PTR1_VAL(argv[2]))[2])
    ((void **)SCHEME_PTR1_VAL(argv[2]))[2] = stx_env;

  SCHEME_EXPAND_OBSERVE_EXIT_LOCAL_BIND(observer);

  return scheme_void;
}

/*========================================================================*/
/*                       cloning prefix information                       */
/*========================================================================*/

Scheme_Object *scheme_eval_clone(Scheme_Object *expr)
{
  /* Clone as much as necessary of `expr' so that prefixes are
     cloned. Cloned prefixes, in turn, can be updated by linking to
     reduce the overhead of cross-module references. */
  switch (SCHEME_TYPE(expr)) {
  case scheme_module_type:
    if (scheme_startup_use_jit)
      return scheme_module_jit(expr);
    else
      return scheme_module_eval_clone(expr);
    break;
  case scheme_define_syntaxes_type:
  case scheme_begin_for_syntax_type:
    return scheme_syntaxes_eval_clone(expr);
  default:
    return expr;
  }
}

Resolve_Prefix *scheme_prefix_eval_clone(Resolve_Prefix *rp)
{
  Resolve_Prefix *naya;
  Scheme_Object **tls;

  if (!rp->num_toplevels)
    return rp;

  naya = MALLOC_ONE_TAGGED(Resolve_Prefix);
  memcpy(naya, rp, sizeof(Resolve_Prefix));
  
  tls = MALLOC_N(Scheme_Object*, rp->num_toplevels);
  memcpy(tls, rp->toplevels, sizeof(Scheme_Object *) * rp->num_toplevels);
  naya->toplevels = tls;

  return naya;
}

/*========================================================================*/
/*        creating/pushing prefix for top-levels and syntax objects       */
/*========================================================================*/

int scheme_prefix_depth(Resolve_Prefix *rp)
{
  if (rp->num_toplevels || rp->num_stxes || rp->num_lifts)
    return 1;
  else
    return 0;
}

Scheme_Object **scheme_push_prefix(Scheme_Env *genv, Resolve_Prefix *rp, 
				   Scheme_Object *src_modidx, Scheme_Object *now_modidx,
				   int src_phase, int now_phase,
                                   Scheme_Env *dummy_env,
                                   Scheme_Object *insp)
{
  Scheme_Object **rs_save, **rs, *v;
  Scheme_Prefix *pf;
  int i, j, tl_map_len;

  rs_save = rs = MZ_RUNSTACK;

  if (rp->uses_unsafe) {
    scheme_check_unsafe_accessible((SCHEME_FALSEP(rp->uses_unsafe)
                                    ? (insp
                                       ? insp
                                       : genv->access_insp)
                                    : rp->uses_unsafe),
                                   genv);
  }

  if (rp->num_toplevels || rp->num_stxes || rp->num_lifts) {
    i = rp->num_toplevels;
    if (rp->num_stxes) {
      i += rp->num_stxes + 1;
    }
    i += rp->num_lifts;

    tl_map_len = ((rp->num_toplevels + rp->num_lifts) + 31) / 32;

    pf = scheme_malloc_tagged(sizeof(Scheme_Prefix) 
                              + ((i-mzFLEX_DELTA) * sizeof(Scheme_Object *))
                              + (tl_map_len * sizeof(int)));
    pf->so.type = scheme_prefix_type;
    pf->num_slots = i;
    pf->num_toplevels = rp->num_toplevels;
    pf->num_stxes = rp->num_stxes;
    --rs;
    MZ_RUNSTACK = rs;
    rs[0] = (Scheme_Object *)pf;
   
    for (i = 0; i < rp->num_toplevels; i++) {
      v = rp->toplevels[i];
      if (genv || SCHEME_FALSEP(v))
	v = link_toplevel(rp->toplevels, i, genv ? genv : dummy_env, src_modidx, now_modidx, insp);
      pf->a[i] = v;
    }

    if (rp->num_stxes) {
      if (insp && SCHEME_FALSEP(insp))
        insp = scheme_get_current_inspector();
      i = rp->num_toplevels;
      v = scheme_stx_phase_shift_as_rename(scheme_make_integer(now_phase - src_phase), 
                                           src_modidx, now_modidx, 
					   genv ? genv->module_registry->exports : NULL,
                                           insp, NULL);
      if (v || (rp->delay_info_rpair && SCHEME_CDR(rp->delay_info_rpair))) {
	/* Put lazy-shift info in pf->a[i]: */
        Scheme_Object **ls;
        ls = MALLOC_N(Scheme_Object *, 2);
        ls[0] = v;
        ls[1] = (Scheme_Object *)rp;
	pf->a[i] = (Scheme_Object *)ls;
	/* Rest of a left zeroed, to be filled in lazily by quote-syntax evaluation */
      } else {
	/* No shift, so fill in stxes immediately */
	i++;
	for (j = 0; j < rp->num_stxes; j++) {
	  pf->a[i + j] = rp->stxes[j];
	}
      }
      j = rp->num_stxes + 1;
    } else
      j = 0;

    if (rp->num_lifts) {
      Scheme_Object *sym, *home;
      sym = scheme_make_symbol("<lifted>"); /* uninterned! */
      j += rp->num_toplevels;
      home = (Scheme_Object *)scheme_get_home_weak_link(genv);
      for (i = 0; i < rp->num_lifts; i++, j++) {
        v = (Scheme_Object *)MALLOC_ONE_TAGGED(Scheme_Bucket_With_Home);
        v->type = scheme_variable_type;
        ((Scheme_Bucket_With_Flags *)v)->flags = GLOB_HAS_HOME_PTR;
        ((Scheme_Bucket_With_Home *)v)->home_link = home;
        ((Scheme_Bucket *)v)->key = (char *)sym;
        pf->a[j] = v;
      }
    }
  }

  return rs_save;
}

void scheme_pop_prefix(Scheme_Object **rs)
{
  /* This function must not allocate, since a relevant multiple-values
     result may be in the thread record (and we don't want it zerod) */
  MZ_RUNSTACK = rs;
}

Scheme_Object *scheme_suspend_prefix(Scheme_Object **rs)
{
  if (rs != MZ_RUNSTACK) {
    Scheme_Object *v;
    v = MZ_RUNSTACK[0];
    MZ_RUNSTACK++;
    return v;
  } else
    return NULL;
}

Scheme_Object **scheme_resume_prefix(Scheme_Object *v)
{
  if (v) {
    --MZ_RUNSTACK;
    MZ_RUNSTACK[0] = v;
    return MZ_RUNSTACK + 1;
  } else
    return MZ_RUNSTACK;
}

#ifdef MZ_PRECISE_GC
static void mark_pruned_prefixes(struct NewGC *gc) XFORM_SKIP_PROC
{
  if (scheme_prefix_finalize != (Scheme_Prefix *)0x1) {
    Scheme_Prefix *pf = scheme_prefix_finalize, *next;
    int i, *use_bits, maxpos;
    
    scheme_prefix_finalize = (Scheme_Prefix *)0x1;
    while (pf != (Scheme_Prefix *)0x1) {
      /* If not marked, only references are through closures: */
      if (!GC_is_marked2(pf, gc)) {
        /* Clear slots that are not use in map */
        maxpos = (pf->num_slots - pf->num_stxes - (pf->num_stxes ? 1 : 0));
        use_bits = PREFIX_TO_USE_BITS(pf);
        for (i = (maxpos + 31) / 32; i--; ) {
          int j;
          for (j = 0; j < 32; j++) {
            if (!(use_bits[i] & (1 << j))) {
              int pos;
              pos = (i * 32) + j;
              if (pos < pf->num_toplevels)
                pf->a[pos] = NULL;
              else if (pos < maxpos) {
                if (pf->num_stxes)
                  pf->a[pos + pf->num_stxes + 1] = NULL;
                else
                  pf->a[pos] = NULL;
              }
            }
          }
          use_bits[i] = 0;
        }
        /* Should mark/copy pf, but not trigger or require mark propagation: */
        gcMARK(pf);
        pf = (Scheme_Prefix *)GC_resolve2(pf, gc);
        GC_retract_only_mark_stack_entry(pf, gc);
      } else
        pf = (Scheme_Prefix *)GC_resolve2(pf, gc);

      /* Clear use map */
      use_bits = PREFIX_TO_USE_BITS(pf);
      maxpos = (pf->num_slots - pf->num_stxes - (pf->num_stxes ? 1 : 0));
      for (i = (maxpos + 31) / 32; i--; )
        use_bits[i] = 0;

      /* Next */
      next = pf->next_final;
      pf->next_final = NULL;

      pf = next;
    }
  }
}
#endif

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_eval.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_compile_info, mark_comp_info);
  GC_REG_TRAV(scheme_rt_saved_stack, mark_saved_stack);
}

END_XFORM_SKIP;

#endif
