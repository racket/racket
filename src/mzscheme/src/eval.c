/*
  MzScheme
  Copyright (c) 2004-2007 PLT Scheme Inc.
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

/* This file contains 

        * the main eval-apply loop, in scheme_do_eval()

        * the main compile loop, int scheme_compile_expand_expr()

        * compilation and bytecode [un]marshaling for
             - applications
             - sequences (along with code in syntax.c)
             - branches (along with code in syntax.c)
             - with-continuation-mark
           [These are here instead of syntax.c because they are
            tightly integrated into the evaluation loop.]

        * C and Scheme stack management routines

   Evaluation:

   The bytecode evaluator uses the C stack for continuations, and a
   separate Scheme stack for activation-frame variables and collecting
   application arguments. Closures are flat, so mutable variables are
   boxed. A third stack is used for continuation marks, only as
   needed.

   Tail calls are, for the most part, gotos within scheme_do_eval(). A
   C function called by the main evaluation loop can perform a
   trampoling tail call via scheme_tail_apply. The trampoline must
   return to its caller without allocating any memory, because an
   allocation optimization in the tail-call code assumes no GCs will
   occur between the time that a tail call is issued and the time when
   it's handled.

   Multiple values are returned as a special SCHEME_MULTIPLE_VALUES
   token that indicates actual values are stored in the current
   thread's record.

   The apply half of the eval-apply loop branches on all possible
   application types. All primitive functions (including cons) are
   implemented by C functions outside the loop. Continuation
   applications are handled directly in scheme_do_eval(). That leaves
   calls to closures, which are also performed within scheme_do_eval()
   (so that most tail calls avoid the trampoline), and native code,
   which is analogous to a primitive.

   The eval half of the loop detects a limited set of core syntactic
   forms, such as application and letrecs. Otherwise, it dispatches to
   external functions to implement elaborate syntactic forms, such as
   begin0 and case-lambda expressions.

   When collecting the arguments for an application, scheme_do_eval()
   avoids recursive C calls to evaluate arguments by recogzining
   easily-evaluated expressions, such as constrants and variable
   lookups. This can be viewed as a kind of half-way A-normalization.

   Bytecodes are not linear. They're actually trees of expression
   nodes.

   Compilation:

   Compilation works in three passes.

   The first pass, called "compile", performs most of the work and
   tracks variable usage (including whether a variable is mutated or
   not).

   The second pass, called "optimize", performs constant propagation,
   constant folding, and function inlining; this pass mutates records
   produced by the first pass. 

   The third pass, called "resolve", finishes compilation by computing
   variable offsets and indirections (often mutating the records
   produced by the first pass). It is also responsible for closure
   conversion (i.e., converting closure content to arguments) and
   lifting (of procedures that close over nothing or only globals).
   Beware that the resulting bytecode object is a graph, not a tree,
   due to sharing (potentially cyclic) of closures that are "empty"
   but actually refer to other "empty" closures.

   Top-level variables (global or module) are referenced through the
   Scheme stack, so that the variables can be "re-linked" each time a
   module is instantiated. Syntax constants are similarly accessed
   through the Scheme stack. The global variables and syntax objects
   are sometimes called the "prefix", and scheme_push_prefix()
   initializes the prefix portion of the stack.

   Just-in-time compilation:

   If the JIT is enabled, then `eval' processes a compiled expression
   one more time (functionally): `lambda' and `case-lambda' forms are
   converted to native-code generators, instead of bytecode variants.

*/

#include "schpriv.h"
#include "schrunst.h"
#include "schexpobs.h"

#ifdef USE_STACKAVAIL
#include <malloc.h>
#endif
#ifdef UNIX_FIND_STACK_BOUNDS
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
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

#define EMBEDDED_DEFINES_START_ANYWHERE 0

/* globals */
Scheme_Object *scheme_eval_waiting;
Scheme_Object *scheme_multiple_values;
int scheme_continuation_application_count;

volatile int scheme_fuel_counter;

int scheme_startup_use_jit = 1;
void scheme_set_startup_use_jit(int v) { scheme_startup_use_jit =  v; }

static Scheme_Object *app_symbol;
static Scheme_Object *datum_symbol;
static Scheme_Object *top_symbol;
static Scheme_Object *top_level_symbol;

static Scheme_Object *app_expander;
static Scheme_Object *datum_expander;
static Scheme_Object *top_expander;

static Scheme_Object *stop_expander;

static Scheme_Object *quick_stx;
static int taking_shortcut;

Scheme_Object *scheme_stack_dump_key;

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

static Scheme_Object *app_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *app_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *datum_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *datum_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *top_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *top_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Object *write_application(Scheme_Object *obj);
static Scheme_Object *read_application(Scheme_Object *obj);
static Scheme_Object *write_sequence(Scheme_Object *obj);
static Scheme_Object *read_sequence(Scheme_Object *obj);
static Scheme_Object *read_sequence_save_first(Scheme_Object *obj);
static Scheme_Object *write_branch(Scheme_Object *obj);
static Scheme_Object *read_branch(Scheme_Object *obj);
static Scheme_Object *write_with_cont_mark(Scheme_Object *obj);
static Scheme_Object *read_with_cont_mark(Scheme_Object *obj);
static Scheme_Object *write_syntax(Scheme_Object *obj);
static Scheme_Object *read_syntax(Scheme_Object *obj);
static Scheme_Object *write_quote_syntax(Scheme_Object *obj);
static Scheme_Object *read_quote_syntax(Scheme_Object *obj);

static Scheme_Object *define_values_symbol, *letrec_values_symbol, *lambda_symbol;
static Scheme_Object *unknown_symbol, *void_link_symbol, *quote_symbol;
static Scheme_Object *letrec_syntaxes_symbol, *begin_symbol;
static Scheme_Object *let_values_symbol;

static Scheme_Object *internal_define_symbol;
static Scheme_Object *module_symbol;
static Scheme_Object *module_begin_symbol;
static Scheme_Object *expression_symbol;

static Scheme_Object *protected_symbol;

static Scheme_Object *zero_rands_ptr; /* &zero_rands_ptr is dummy rands pointer */

int scheme_overflow_count;

static Scheme_Object *scheme_compile_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
						 Scheme_Compile_Expand_Info *rec, int drec, 
						 int app_position);

static Scheme_Object *_eval_compiled_multi_with_prompt(Scheme_Object *obj, Scheme_Env *env);

#define cons(x,y) scheme_make_pair(x,y)

typedef void (*DW_PrePost_Proc)(void *);

#ifdef USE_STACK_BOUNDARY_VAR
unsigned long scheme_stack_boundary;
#endif

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

/* Lookahead types for evaluating application arguments. */
/* 4 cases + else => magic number for some compilers doing a switch? */
enum {
  SCHEME_EVAL_CONSTANT = 0,
  SCHEME_EVAL_GLOBAL,
  SCHEME_EVAL_LOCAL,
  SCHEME_EVAL_LOCAL_UNBOX,
  SCHEME_EVAL_GENERAL
};

#define icons scheme_make_immutable_pair

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
  
  define_values_symbol = scheme_intern_symbol("define-values");
  letrec_values_symbol = scheme_intern_symbol("letrec-values");
  let_values_symbol = scheme_intern_symbol("let-values");
  lambda_symbol = scheme_intern_symbol("lambda");
  unknown_symbol = scheme_intern_symbol("unknown");
  void_link_symbol = scheme_intern_symbol("-v");
  quote_symbol = scheme_intern_symbol("quote");
  letrec_syntaxes_symbol = scheme_intern_symbol("letrec-syntaxes+values");
  begin_symbol = scheme_intern_symbol("begin");
  
  REGISTER_SO(module_symbol);
  REGISTER_SO(module_begin_symbol);
  REGISTER_SO(internal_define_symbol);
  REGISTER_SO(expression_symbol);
  REGISTER_SO(top_level_symbol);

  module_symbol = scheme_intern_symbol("module");
  module_begin_symbol = scheme_intern_symbol("module-begin");
  internal_define_symbol = scheme_intern_symbol("internal-define");
  expression_symbol = scheme_intern_symbol("expression");
  top_level_symbol = scheme_intern_symbol("top-level");

  REGISTER_SO(protected_symbol);
  protected_symbol = scheme_intern_symbol("protected");

  REGISTER_SO(scheme_stack_dump_key);
  scheme_stack_dump_key = scheme_make_symbol("stk"); /* uninterned! */

  scheme_install_type_writer(scheme_application_type, write_application);
  scheme_install_type_reader(scheme_application_type, read_application);
  scheme_install_type_writer(scheme_application2_type, write_application);
  scheme_install_type_reader(scheme_application2_type, read_application);
  scheme_install_type_writer(scheme_application3_type, write_application);
  scheme_install_type_reader(scheme_application3_type, read_application);
  scheme_install_type_writer(scheme_sequence_type, write_sequence);
  scheme_install_type_reader(scheme_sequence_type, read_sequence);
  scheme_install_type_writer(scheme_branch_type, write_branch);
  scheme_install_type_reader(scheme_branch_type, read_branch);
  scheme_install_type_writer(scheme_with_cont_mark_type, write_with_cont_mark);
  scheme_install_type_reader(scheme_with_cont_mark_type, read_with_cont_mark);
  scheme_install_type_writer(scheme_quote_syntax_type, write_quote_syntax);
  scheme_install_type_reader(scheme_quote_syntax_type, read_quote_syntax);
  scheme_install_type_writer(scheme_syntax_type, write_syntax);
  scheme_install_type_reader(scheme_syntax_type, read_syntax);
  
  scheme_install_type_writer(scheme_begin0_sequence_type, write_sequence);
  scheme_install_type_reader(scheme_begin0_sequence_type, read_sequence_save_first);
  
  scheme_add_global_constant("eval", 
			     scheme_make_prim_w_arity2(eval, 
						       "eval", 
						       1, 2,
						       0, -1), 
			     env);
  scheme_add_global_constant("eval-syntax", 
			     scheme_make_prim_w_arity2(eval_stx, 
						       "eval-syntax", 
						       1, 2,
						       0, -1), 
			     env);
  scheme_add_global_constant("compile", 
			     scheme_make_prim_w_arity(compile, 
						      "compile", 
						      1, 1), 
			     env);
  scheme_add_global_constant("compile-syntax", 
			     scheme_make_prim_w_arity(compile_stx, 
						      "compile-syntax", 
						      1, 1), 
			     env);
  scheme_add_global_constant("compiled-expression?",
			     scheme_make_prim_w_arity(compiled_p, 
						      "compiled-expression?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("expand", 
			     scheme_make_prim_w_arity(expand, 
						      "expand",
						      1, 1), 
			     env);
  scheme_add_global_constant("expand-syntax", 
			     scheme_make_prim_w_arity(expand_stx, 
						      "expand-syntax",
						      1, 1), 
			     env);
  scheme_add_global_constant("local-expand", 
			     scheme_make_prim_w_arity(local_expand, 
						      "local-expand",
						      3, 4), 
			     env);
  scheme_add_global_constant("syntax-local-expand-expression", 
			     scheme_make_prim_w_arity(local_expand_expr, 
                                                      "syntax-local-expand-expression",
						      1, 1), 
			     env);
  scheme_add_global_constant("syntax-local-bind-syntaxes", 
			     scheme_make_prim_w_arity(local_eval, 
						      "syntax-local-bind-syntaxes",
						      3, 3), 
			     env);
  scheme_add_global_constant("local-expand/capture-lifts", 
			     scheme_make_prim_w_arity(local_expand_catch_lifts, 
						      "local-expand/capture-lifts",
						      3, 5), 
			     env);
  scheme_add_global_constant("local-transformer-expand", 
			     scheme_make_prim_w_arity(local_transformer_expand, 
						      "local-transformer-expand",
						      3, 4), 
			     env);
  scheme_add_global_constant("local-transformer-expand/capture-lifts", 
			     scheme_make_prim_w_arity(local_transformer_expand_catch_lifts, 
						      "local-transformer-expand/capture-lifts",
						      3, 5), 
			     env);
  scheme_add_global_constant("expand-once", 
			     scheme_make_prim_w_arity(expand_once, 
						      "expand-once", 
						      1, 1), 
			     env);
  scheme_add_global_constant("expand-syntax-once", 
			     scheme_make_prim_w_arity(expand_stx_once, 
						      "expand-syntax-once", 
						      1, 1), 
			     env);
  scheme_add_global_constant("expand-to-top-form", 
			     scheme_make_prim_w_arity(expand_to_top_form, 
						      "expand-to-top-form", 
						      1, 1), 
			     env);
  scheme_add_global_constant("expand-syntax-to-top-form", 
			     scheme_make_prim_w_arity(expand_stx_to_top_form, 
						      "expand-syntax-to-top-form", 
						      1, 1), 
			     env);
  scheme_add_global_constant("namespace-syntax-introduce", 
			     scheme_make_prim_w_arity(top_introduce_stx, 
						      "namespace-syntax-introduce", 
						      1, 1), 
			     env);
  scheme_add_global_constant("break-enabled", 
			     scheme_make_prim_w_arity(enable_break, 
						      "break-enabled",
						      0, 1), 
			     env);
  scheme_add_global_constant("current-eval",
			     scheme_register_parameter(current_eval, 
						       "current-eval",
						       MZCONFIG_EVAL_HANDLER),
			     env);
  scheme_add_global_constant("current-compile",
			     scheme_register_parameter(current_compile, 
						       "current-compile",
						       MZCONFIG_COMPILE_HANDLER),
			     env);

  scheme_add_global_constant("compile-allow-set!-undefined", 
			     scheme_register_parameter(allow_set_undefined, 
						       "compile-allow-set!-undefined",
						       MZCONFIG_ALLOW_SET_UNDEFINED), 
			     env);
  scheme_add_global_constant("compile-enforce-module-constants", 
			     scheme_register_parameter(compile_module_constants, 
						       "compile-enforce-module-constants",
						       MZCONFIG_COMPILE_MODULE_CONSTS), 
			     env);

  scheme_add_global_constant("eval-jit-enabled", 
			     scheme_register_parameter(use_jit, 
						       "eval-jit-enabled",
						       MZCONFIG_USE_JIT), 
			     env);

  REGISTER_SO(app_symbol);
  REGISTER_SO(datum_symbol);
  REGISTER_SO(top_symbol);

  app_symbol = scheme_intern_symbol("#%app");
  datum_symbol = scheme_intern_symbol("#%datum");
  top_symbol = scheme_intern_symbol("#%top");

  REGISTER_SO(app_expander);
  REGISTER_SO(datum_expander);
  REGISTER_SO(top_expander);

  app_expander = scheme_make_compiled_syntax(app_syntax,
					     app_expand);
  scheme_add_global_keyword("#%app", 
			    app_expander, 
			    env);

  datum_expander = scheme_make_compiled_syntax(datum_syntax,
					       datum_expand);
  scheme_add_global_keyword("#%datum", 
			    datum_expander, 
			    env);

  top_expander = scheme_make_compiled_syntax(top_syntax,
					     top_expand);
  scheme_add_global_keyword("#%top", 
			    top_expander, 
			    env);

  REGISTER_SO(quick_stx);
}

/*========================================================================*/
/*                   C stack and Scheme stack handling                    */
/*========================================================================*/

# define DO_CHECK_FOR_BREAK(p, e) \
	if (DECREMENT_FUEL(scheme_fuel_counter, 1) <= 0) { \
	  e scheme_thread_block(0); \
          (p)->ran_some = 1; \
	}

Scheme_Object *
scheme_handle_stack_overflow(Scheme_Object *(*k)(void))
{
  /* "Stack overflow" means running out of C-stack space. The other
     end of this handler (i.e., the target for the longjmp) is
     scheme_top_level_do in fun.c */
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Overflow *overflow;
  Scheme_Overflow_Jmp *jmp;

  scheme_about_to_move_C_stack();

  scheme_overflow_k = k;
  scheme_overflow_count++;
  
  overflow = MALLOC_ONE_RT(Scheme_Overflow);
#ifdef MZTAG_REQUIRED
  overflow->type = scheme_rt_overflow;
#endif
  overflow->prev = scheme_current_thread->overflow;
  overflow->stack_start = p->stack_start;
  p->overflow = overflow;

  jmp = MALLOC_ONE_RT(Scheme_Overflow_Jmp);
#ifdef MZTAG_REQUIRED
  jmp->type = scheme_rt_overflow_jmp;
#endif
  overflow->jmp = jmp;

  scheme_init_jmpup_buf(&overflow->jmp->cont);
  scheme_zero_unneeded_rands(scheme_current_thread); /* for GC */
  if (scheme_setjmpup(&overflow->jmp->cont, overflow->jmp, ADJUST_STACK_START(p->stack_start))) {
    p = scheme_current_thread;
    overflow = p->overflow;
    p->overflow = overflow->prev;
    p->error_buf = overflow->jmp->savebuf;
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
        scheme_longjmpup(&c->buf);
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

void scheme_init_stack_check()
     /* Finds the C stack limit --- platform-specific. */
{
  int *v, stack_grows_up;
  unsigned long deeper;
#ifdef UNIX_FIND_STACK_BOUNDS
  struct rlimit rl;
#endif
  
  deeper = scheme_get_deeper_address();
  stack_grows_up = (deeper > (unsigned long)&v);

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
    scheme_stack_boundary = scheme_get_stack_base();
    if (stack_grows_up)
      scheme_stack_boundary += (FIXED_STACK_SIZE - STACK_SAFETY_MARGIN);
    else
      scheme_stack_boundary += (STACK_SAFETY_MARGIN - FIXED_STACK_SIZE);
# endif

# ifdef WINDOWS_FIND_STACK_BOUNDS
    scheme_stack_boundary = scheme_get_stack_base();
    scheme_stack_boundary += (STACK_SAFETY_MARGIN - 0x100000);
# endif

# ifdef MACOS_FIND_STACK_BOUNDS
    scheme_stack_boundary = (unsigned long)&v +  STACK_SAFETY_MARGIN - StackSpace();
# endif

# ifdef PALMOS_FIND_STACK_BOUNDS
    {
      Ptr s, e;
      SysGetStackInfo(Ptr &s, &e);
      scheme_stack_boundary = (unsigned long)e + STACK_SAFETY_MARGIN;
    }
# endif

# ifdef BEOS_FIND_STACK_BOUNDS
    {
      thread_info info;
      get_thread_info(find_thread(NULL), &info);
      scheme_stack_boundary = (unsigned long)info.stack_base + STACK_SAFETY_MARGIN;
    }
# endif

# ifdef OSKIT_FIXED_STACK_BOUNDS
    scheme_stack_boundary = (unsigned long)base_stack_start + STACK_SAFETY_MARGIN;
# endif

# ifdef UNIX_FIND_STACK_BOUNDS
    getrlimit(RLIMIT_STACK, &rl);
  
    {
      unsigned long bnd, lim;
      bnd = (unsigned long)scheme_get_stack_base();

      lim = (unsigned long)rl.rlim_cur;
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
  }
#endif
}


int scheme_check_runstack(long size)
     /* Checks whether the Scheme stack has `size' room left */
{
  return ((MZ_RUNSTACK - MZ_RUNSTACK_START) >= (size + SCHEME_TAIL_COPY_THRESHOLD));
}

void *scheme_enlarge_runstack(long size, void *(*k)())
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
    long min_size;
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
/*           compiling applications, sequences, and branches              */
/*========================================================================*/

int scheme_omittable_expr(Scheme_Object *o, int vals)
     /* Checks whether the bytecode `o' returns `vals' values with no
        side-effects and without pushing and using continuation marks. 
        -1 for vals means that any return count is ok. */
{
  Scheme_Type vtype;

  /* FIXME: can overflow the stack */

 try_again:

  vtype = SCHEME_TYPE(o);

  if ((vtype > _scheme_compiled_values_types_) 
      || (vtype == scheme_local_type)
      || (vtype == scheme_local_unbox_type)
      || (vtype == scheme_unclosed_procedure_type)
      || (vtype == scheme_compiled_unclosed_procedure_type)
      || (vtype == scheme_case_lambda_sequence_type)
      || (vtype == scheme_quote_syntax_type)
      || (vtype == scheme_compiled_quote_syntax_type))
    return ((vals == 1) || (vals < 0));

  if ((vtype == scheme_compiled_quote_syntax_type)) {
    return ((vals == 1) || (vals < 0));
  }

  if ((vtype == scheme_branch_type)) {
    Scheme_Branch_Rec *b;
    b = (Scheme_Branch_Rec *)o;
    return (scheme_omittable_expr(b->test, 1)
	    && scheme_omittable_expr(b->tbranch, vals)
	    && scheme_omittable_expr(b->fbranch, vals));
  }

#if 0
  /* We can't do this because a set! to a lexical is turned into
     a let_value_type! */
  if ((vtype == scheme_let_value_type)) {
    Scheme_Let_Value *lv = (Scheme_Let_Value *)o;
    return (scheme_omittable_expr(lv->value, lv->count)
	    && scheme_omittable_expr(lv->body, vals));
  }
#endif

  if ((vtype == scheme_let_one_type)) {
    Scheme_Let_One *lo = (Scheme_Let_One *)o;
    return (scheme_omittable_expr(lo->value, 1)
	    && scheme_omittable_expr(lo->body, vals));
  }

  if ((vtype == scheme_let_void_type)) {
    o = ((Scheme_Let_Void *)o)->body;
    goto try_again;
  }

  if ((vtype == scheme_letrec_type)) {
    o = ((Scheme_Letrec *)o)->body;
    goto try_again;
  }

  if ((vtype == scheme_application_type)) {
    /* Look for multiple values, or for `make-struct-type'.
       (The latter is especially useful to Honu.) */
    Scheme_App_Rec *app = (Scheme_App_Rec *)o;
    if (((vals == 5) || (vals < 0))
        && (app->num_args >= 4) && (app->num_args <= 10)
        && SAME_OBJ(scheme_make_struct_type_proc, app->args[0])) {
      /* Look for (make-struct-type sym #f non-neg-int non-neg-int [omitable null]) */
      if (SCHEME_SYMBOLP(app->args[1])
          && SCHEME_FALSEP(app->args[2])
          && SCHEME_INTP(app->args[3])
          && (SCHEME_INT_VAL(app->args[3]) >= 0)
          && SCHEME_INTP(app->args[4])
          && (SCHEME_INT_VAL(app->args[4]) >= 0)
          && ((app->num_args < 5)
              || scheme_omittable_expr(app->args[5], 1))
          && ((app->num_args < 6)
              || SCHEME_NULLP(app->args[6]))
          && ((app->num_args < 7)
              || SCHEME_FALSEP(app->args[7]))
          && ((app->num_args < 8)
              || SCHEME_FALSEP(app->args[8]))
          && ((app->num_args < 9)
              || SCHEME_NULLP(app->args[9]))) {
        return 1;
      }
    }
    if ((app->num_args == vals) || (vals < 0)) {
      if (SAME_OBJ(scheme_values_func, app->args[0])) {
	int i;
	for (i = app->num_args; i--; ) {
	  if (!scheme_omittable_expr(app->args[i + 1], 1))
	    return 0;
	}
	return 1;
      }
    }
    return 0;
  }

  if ((vtype == scheme_application2_type)) {
    if ((vals == 1) || (vals < 0)) {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)o;
      if (SAME_OBJ(scheme_values_func, app->rator)) {
	if (scheme_omittable_expr(app->rand, 1))
	  return 1;
      }
    }
    
  }

  if ((vtype == scheme_application3_type)) {
    if ((vals == 2) || (vals < 0)) {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)o;
      if (SAME_OBJ(scheme_values_func, app->rator)) {
	if (scheme_omittable_expr(app->rand1, 1)
	    && scheme_omittable_expr(app->rand2, 1))
	  return 1;
      }
    }
    
  }

  return 0;
}

int scheme_is_compiled_procedure(Scheme_Object *o, int can_be_closed, int can_be_liftable)
{
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_compiled_unclosed_procedure_type)) {
    if (!can_be_closed || !can_be_liftable) {
      Scheme_Closure_Data *data;
      data = (Scheme_Closure_Data *)o;
      /* Because == 0 is like a constant */
      if (!can_be_closed && !data->closure_size)
        return 0;
      /* Because procs that reference only globals are lifted: */
      if (!can_be_liftable && (data->closure_size == 1) && scheme_closure_has_top_level(data))
        return 0;
    }
    return 1;
  } else
    return 0;
}

int scheme_get_eval_type(Scheme_Object *obj)
     /* Categories for short-cutting recursive calls to the evaluator */
{
  Scheme_Type type;

  type = SCHEME_TYPE(obj);

  if (type > _scheme_values_types_)
    return SCHEME_EVAL_CONSTANT;
  else if (SAME_TYPE(type, scheme_local_type))
    return SCHEME_EVAL_LOCAL;
  else if (SAME_TYPE(type, scheme_local_unbox_type))
    return SCHEME_EVAL_LOCAL_UNBOX;
  else if (SAME_TYPE(type, scheme_toplevel_type))
    return SCHEME_EVAL_GLOBAL;
  else
    return SCHEME_EVAL_GENERAL;
}    

static Scheme_Object *try_apply(Scheme_Object *f, Scheme_Object *args)
     /* Apply `f' to `args' and ignore failues --- used for constant
        folding attempts */
{
  Scheme_Object * volatile result;
  mz_jmp_buf *savebuf, newbuf;

  scheme_current_thread->skip_error = 5;
  savebuf = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;

  if (scheme_setjmp(newbuf))
    result = NULL;
  else
    result = _scheme_apply_to_list(f, args);
  
  scheme_current_thread->error_buf = savebuf;
  scheme_current_thread->skip_error = 0;  

  return result;
}

static Scheme_Object *make_application(Scheme_Object *v)
{
  Scheme_Object *o;
  int i, nv;
  volatile int n;

  o = v;
  n = 0;
  nv = 0;
  while (!SCHEME_NULLP(o)) {
    Scheme_Type type;
    
    n++;
    type = SCHEME_TYPE(SCHEME_CAR(o));
    if (type < _scheme_compiled_values_types_)
      nv = 1;
    o = SCHEME_CDR(o);
  }

  if (!nv) {
    /* They're all values. Applying folding prim or closure? */
    Scheme_Object *f;

    f = SCHEME_CAR(v);

    if ((SCHEME_PRIMP(f) && (((Scheme_Primitive_Proc *)f)->pp.flags & SCHEME_PRIM_IS_FOLDING))
	|| (SCHEME_CLSD_PRIMP(f) 
	    && (((Scheme_Closed_Primitive_Proc *)f)->pp.flags & SCHEME_PRIM_IS_FOLDING))
	|| (SAME_TYPE(SCHEME_TYPE(f), scheme_closure_type)
	    && ((SCHEME_CLOSURE_DATA_FLAGS(SCHEME_COMPILED_CLOS_CODE(f)))
		& CLOS_FOLDABLE))) {
      f = try_apply(f, SCHEME_CDR(v));
      
      if (f)
	return f;
    }
  }

  if (n == 2) {
    Scheme_App2_Rec *app;

    app = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
    app->iso.so.type = scheme_application2_type;

    app->rator = SCHEME_CAR(v);
    v = SCHEME_CDR(v);
    app->rand = SCHEME_CAR(v);

    return (Scheme_Object *)app;
  } else if (n == 3) {
    Scheme_App3_Rec *app;

    app = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
    app->iso.so.type = scheme_application3_type;

    app->rator = SCHEME_CAR(v);
    v = SCHEME_CDR(v);
    app->rand1 = SCHEME_CAR(v);
    v = SCHEME_CDR(v);
    app->rand2 = SCHEME_CAR(v);

    return (Scheme_Object *)app;
  } else {
    Scheme_App_Rec *app;

    app = scheme_malloc_application(n);
    
    for (i = 0; i < n; i++, v = SCHEME_CDR(v)) {
      app->args[i] = SCHEME_CAR(v);
    }

    return (Scheme_Object *)app;
  }
}

Scheme_App_Rec *scheme_malloc_application(int n)
{
  Scheme_App_Rec *app;
  int size;

  size = (sizeof(Scheme_App_Rec) 
	  + ((n - 1) * sizeof(Scheme_Object *))
	  + n * sizeof(char));
  app = (Scheme_App_Rec *)scheme_malloc_tagged(size);

  app->so.type = scheme_application_type;

  app->num_args = n - 1;

  return app;
}

void scheme_finish_application(Scheme_App_Rec *app)
{
  int i, devals, n;

  n = app->num_args + 1;

  devals = sizeof(Scheme_App_Rec) + (app->num_args * sizeof(Scheme_Object *));

  for (i = 0; i < n; i++) {
    char etype;
    etype = scheme_get_eval_type(app->args[i]);
    ((char *)app XFORM_OK_PLUS devals)[i] = etype;
  }
}

static Scheme_Object *check_converted_rator(Scheme_Object *rator, Resolve_Info *info, Scheme_Object **new_rator,
                                            int orig_arg_cnt, int *_rdelta)
{
  Scheme_Object *lifted;
  int flags;

  if (!SAME_TYPE(SCHEME_TYPE(rator), scheme_local_type))
    return NULL;

  (void)scheme_resolve_info_lookup(info, SCHEME_LOCAL_POS(rator), &flags, &lifted, orig_arg_cnt + 1);

  if (lifted && SCHEME_RPAIRP(lifted)) {
    Scheme_Object *vec, *arity;

    *new_rator = SCHEME_CAR(lifted);
    vec = SCHEME_CDR(lifted);
    *_rdelta = 0;

    if (SCHEME_VEC_SIZE(vec) > 1) {
      /* Check that actual argument count matches expected. If
         it doesn't, we need to generate explicit code to report
         the error, so that the conversion's arity change isn't
         visible. */
      arity = SCHEME_VEC_ELS(vec)[0];
      if (SCHEME_INTP(arity)) {
        if (orig_arg_cnt == SCHEME_INT_VAL(arity))
          arity = NULL;
      } else {
        arity = SCHEME_BOX_VAL(arity);
        if (orig_arg_cnt >= SCHEME_INT_VAL(arity))
          arity = NULL;
        else {
          Scheme_App2_Rec *app;
          app = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
          app->iso.so.type = scheme_application2_type;
          app->rator = scheme_make_arity_at_least;
          app->rand = arity;
          arity = (Scheme_Object *)app;
          *_rdelta = 1; /* so app gets resolved */
        }
      }
      /* If arity is non-NULL, there's a mismatch. */
      if (arity) {
        /* Generate a call to `raise-arity-error' instead of
           the current *new_rator: */
        Scheme_Object *old_rator = *new_rator;
        if (SAME_TYPE(SCHEME_TYPE(old_rator), scheme_toplevel_type)) {
          /* More coordinate trouble. old_rator was computed for an
             application with a potentially different number of arguments. */
          int delta;
          delta = 3 - SCHEME_VEC_SIZE(vec);
          if (delta)
            old_rator = scheme_shift_toplevel(old_rator, delta);
        }
        vec = scheme_make_vector(3, NULL);
        SCHEME_VEC_ELS(vec)[0] = scheme_make_integer(0);
        SCHEME_VEC_ELS(vec)[1] = old_rator;
        SCHEME_VEC_ELS(vec)[2] = arity;
        *new_rator = scheme_raise_arity_error_proc;
      }
    }

    return vec;
  } else
    return NULL;
}

static Scheme_Object *resolve_application(Scheme_Object *o, Resolve_Info *orig_info, int already_resolved_arg_count)
{
  Resolve_Info *info;
  Scheme_App_Rec *app;
  int i, n, devals;

  app = (Scheme_App_Rec *)o;

  n = app->num_args + 1;

  if (!already_resolved_arg_count) {
    /* Check whether this is an application of a converted closure: */
    Scheme_Object *additions = NULL, *rator;
    int rdelta;
    additions = check_converted_rator(app->args[0], orig_info, &rator, n - 1, &rdelta);
    if (additions) {
      /* Expand application with m arguments */
      Scheme_App_Rec *app2;
      Scheme_Object *loc;
      int m;
      m = SCHEME_VEC_SIZE(additions) - 1;
      app2 = scheme_malloc_application(n + m);
      for (i = 0; i < m; i++) {
        loc = SCHEME_VEC_ELS(additions)[i+1];
        if (SCHEME_BOXP(loc)) 
          loc = SCHEME_BOX_VAL(loc);
        app2->args[i + 1] = loc;
      }
      for (i = 1; i < n; i++) {
        app2->args[i + m] = app->args[i];
      }
      app2->args[0] = rator;
      n += m;
      app = app2;
      already_resolved_arg_count = m + 1 + rdelta;
    }
  }

  devals = sizeof(Scheme_App_Rec) + ((n - 1) * sizeof(Scheme_Object *));
  
  info = scheme_resolve_info_extend(orig_info, n - 1, 0, 0);
  
  for (i = 0; i < n; i++) {
    Scheme_Object *le;
    if (already_resolved_arg_count) {
      already_resolved_arg_count--;
    } else {
      le = scheme_resolve_expr(app->args[i], info);
      app->args[i] = le;
    }
  }

  info->max_let_depth += (n - 1);
  if (orig_info->max_let_depth < info->max_let_depth)
    orig_info->max_let_depth = info->max_let_depth;

  for (i = 0; i < n; i++) {
    char et;
    et = scheme_get_eval_type(app->args[i]);
    ((char *)app XFORM_OK_PLUS devals)[i] = et;
  }

  return (Scheme_Object *)app;
}

static Scheme_Object *resolve_application3(Scheme_Object *o, Resolve_Info *orig_info, int already_resolved_arg_count);

static Scheme_Object *resolve_application2(Scheme_Object *o, Resolve_Info *orig_info, int already_resolved_arg_count)
{
  Resolve_Info *info;
  Scheme_App2_Rec *app;
  Scheme_Object *le;
  short et;

  app = (Scheme_App2_Rec *)o;

  if (!already_resolved_arg_count) {
    /* Check whether this is an application of a converted closure: */
    Scheme_Object *additions = NULL, *rator;
    int rdelta;
    additions = check_converted_rator(app->rator, orig_info, &rator, 1, &rdelta);
    if (additions) {
      int m;
      m = SCHEME_VEC_SIZE(additions) - 1;
      if (!m) {
        app->rator = rator;
        already_resolved_arg_count = 1 + rdelta;
      } else if (m > 1) {
        /* Expand application with m arguments */
        Scheme_App_Rec *app2;
        Scheme_Object *loc;
        int i;
        app2 = scheme_malloc_application(2 + m);
        for (i = 0; i < m; i++) {
          loc = SCHEME_VEC_ELS(additions)[i+1];
          if (SCHEME_BOXP(loc))
            loc = SCHEME_BOX_VAL(loc);
          app2->args[i + 1] = loc;
        }
        app2->args[0] = rator;
        app2->args[m+1] = app->rand;
        return resolve_application((Scheme_Object *)app2, orig_info, m + 1 + rdelta);
      } else {
        Scheme_App3_Rec *app2;
        Scheme_Object *loc;
        app2 = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
        app2->iso.so.type = scheme_application3_type;
        app2->rator = rator;
        loc = SCHEME_VEC_ELS(additions)[1];
        if (SCHEME_BOXP(loc))
          loc = SCHEME_BOX_VAL(loc);
        app2->rand1 = loc;
        app2->rand2 = app->rand;
        return resolve_application3((Scheme_Object *)app2, orig_info, 2 + rdelta);
      }
    }
  }

  info = scheme_resolve_info_extend(orig_info, 1, 0, 0);

  if (!already_resolved_arg_count) {
    le = scheme_resolve_expr(app->rator, info);
    app->rator = le;
  } else
    already_resolved_arg_count--;

  if (!already_resolved_arg_count) {
    le = scheme_resolve_expr(app->rand, info);
    app->rand = le;
  } else
    already_resolved_arg_count--;

  et = scheme_get_eval_type(app->rand);
  et = et << 3;
  et += scheme_get_eval_type(app->rator);
  
  SCHEME_APPN_FLAGS(app) = et;
  
  info->max_let_depth += 1;
  if (orig_info->max_let_depth < info->max_let_depth)
    orig_info->max_let_depth = info->max_let_depth;

  return (Scheme_Object *)app;
}

static int eq_testable_constant(Scheme_Object *v)
{
  if (SCHEME_SYMBOLP(v)
      || SCHEME_FALSEP(v)
      || SAME_OBJ(v, scheme_true)
      || SCHEME_VOIDP(v))
    return 1;

  if (SCHEME_CHARP(v) && (SCHEME_CHAR_VAL(v) < 256))
    return 1;

  return 0;
}

static Scheme_Object *resolve_application3(Scheme_Object *o, Resolve_Info *orig_info, int already_resolved_arg_count)
{
  Resolve_Info *info;
  Scheme_App3_Rec *app;
  Scheme_Object *le;
  short et;

  app = (Scheme_App3_Rec *)o;

  if (!already_resolved_arg_count) {
    /* Check whether this is an application of a converted closure: */
    Scheme_Object *additions = NULL, *rator;
    int rdelta;
    additions = check_converted_rator(app->rator, orig_info, &rator, 2, &rdelta);
    if (additions) {
      int m, i;
      m = SCHEME_VEC_SIZE(additions) - 1;
      if (m) {
        /* Expand application with m arguments */
        Scheme_App_Rec *app2;
        Scheme_Object *loc;
        app2 = scheme_malloc_application(3 + m);
        for (i = 0; i < m; i++) {
          loc = SCHEME_VEC_ELS(additions)[i+1];
          if (SCHEME_BOXP(loc))
            loc = SCHEME_BOX_VAL(loc);
          app2->args[i + 1] = loc;
        }
        app2->args[0] = rator;
        app2->args[m+1] = app->rand1;
        app2->args[m+2] = app->rand2;
        return resolve_application((Scheme_Object *)app2, orig_info, m + 1 + rdelta);
      } else {
        app->rator = rator;
        already_resolved_arg_count = 1 + rdelta;
      }
    }
  }

  info = scheme_resolve_info_extend(orig_info, 2, 0, 0);

  if (already_resolved_arg_count) {
    already_resolved_arg_count--;
  } else {
    le = scheme_resolve_expr(app->rator, info);
    app->rator = le;
  }

  if (already_resolved_arg_count) {
    already_resolved_arg_count--;
  } else {
    le = scheme_resolve_expr(app->rand1, info);
    app->rand1 = le;
  }

  if (already_resolved_arg_count) {
    already_resolved_arg_count--;
  } else {
    le = scheme_resolve_expr(app->rand2, info);
    app->rand2 = le;
  }

  /* Optimize `equal?' or `eqv?' test on certain types
     to `eq?'. This is especially helpful for the JIT. */
  if ((SAME_OBJ(app->rator, scheme_equal_prim)
       || SAME_OBJ(app->rator, scheme_eqv_prim))
      && (eq_testable_constant(app->rand1)
	  || eq_testable_constant(app->rand2))) {
    app->rator = scheme_eq_prim;
  }

  et = scheme_get_eval_type(app->rand2);
  et = et << 3;
  et += scheme_get_eval_type(app->rand1);
  et = et << 3;
  et += scheme_get_eval_type(app->rator);
  
  SCHEME_APPN_FLAGS(app) = et;

  info->max_let_depth += 2;
  if (orig_info->max_let_depth < info->max_let_depth)
    orig_info->max_let_depth = info->max_let_depth;

  return (Scheme_Object *)app;
}

Scheme_Object *
scheme_make_branch(Scheme_Object *test, Scheme_Object *thenp,
		   Scheme_Object *elsep)
{
  Scheme_Branch_Rec *b;

  if (SCHEME_TYPE(test) > _scheme_compiled_values_types_) {
    if (SCHEME_FALSEP(test))
      return elsep;
    else
      return thenp;
  }

  b = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
  b->so.type = scheme_branch_type;

  b->test = test;
  b->tbranch = thenp;
  b->fbranch = elsep;

  return (Scheme_Object *)b;
}

static Scheme_Object *resolve_branch(Scheme_Object *o, Resolve_Info *info)
{
  Scheme_Branch_Rec *b;
  Scheme_Object *t, *tb, *fb;

  b = (Scheme_Branch_Rec *)o;

  t = scheme_resolve_expr(b->test, info);
  tb = scheme_resolve_expr(b->tbranch, info);
  fb = scheme_resolve_expr(b->fbranch, info);

  b->test = t;
  b->tbranch = tb;
  b->fbranch = fb;

  return o;
}

static Scheme_Object *resolve_wcm(Scheme_Object *o, Resolve_Info *info)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)o;
  Scheme_Object *k, *v, *b;

  k = scheme_resolve_expr(wcm->key, info);
  v = scheme_resolve_expr(wcm->val, info);
  b = scheme_resolve_expr(wcm->body, info);
  wcm->key = k;
  wcm->val = v;
  wcm->body = b;

  return (Scheme_Object *)wcm;
}

static Scheme_Sequence *malloc_sequence(int count)
{
  return (Scheme_Sequence *)scheme_malloc_tagged(sizeof(Scheme_Sequence)
						 + (count - 1) 
						 * sizeof(Scheme_Object *));
}

Scheme_Object *scheme_make_sequence_compilation(Scheme_Object *seq, int opt)
{
  /* We have to be defensive in processing `seq'; it might be bad due
     to a bad .zo */
  Scheme_Object *list, *v, *good;
  Scheme_Sequence *o;
  int count, i, k, total, last, first, setgood, addconst;
  Scheme_Type type;

  type = scheme_sequence_type;

  list = seq;
  count = i = 0;
  good = NULL;
  total = 0;
  first = 1;
  setgood = 1;
  while (SCHEME_PAIRP(list)) {
    v = SCHEME_CAR(list);
    list = SCHEME_CDR(list);
    last = SCHEME_NULLP(list);

    if (((opt > 0) || !first) && SAME_TYPE(SCHEME_TYPE(v), type)) {
      /* "Inline" nested begins */
      count += ((Scheme_Sequence *)v)->count;
      total++;
    } else if (opt 
	       && (((opt > 0) && !last) || ((opt < 0) && !first))
	       && scheme_omittable_expr(v, -1)) {
      /* A value that is not the result. We'll drop it. */
      total++;
    } else {
      if (setgood)
	good = v;
      count++;
      total++;
    }
    i++;
    if (first) {
      if (opt < 0)
	setgood = 0;
      first = 0;
    }
  }

  if (!SCHEME_NULLP(list))
    return NULL; /* bad .zo */

  if (!count)
    return scheme_compiled_void();
  
  if (count == 1) {
    if ((opt < 0) && !scheme_omittable_expr(SCHEME_CAR(seq), 1)) {
      /* We can't optimize (begin0 expr cont) to expr because
	 exp is not in tail position in the original (so we'd mess
	 up continuation marks). */
      addconst = 1;
    } else
      return good;
  } else
    addconst = 0;

  o = malloc_sequence(count + addconst);

  o->so.type = ((opt < 0) ? scheme_begin0_sequence_type : scheme_sequence_type);
  o->count = count + addconst;
  
  --total;
  for (i = k = 0; i < count; k++) {
    v = SCHEME_CAR(seq);
    seq = SCHEME_CDR(seq);

    if (((opt > 0) || k) && SAME_TYPE(SCHEME_TYPE(v), type)) {
      int c, j;
      Scheme_Object **a;

      c = ((Scheme_Sequence *)v)->count;
      a = ((Scheme_Sequence *)v)->array; /* <-- mismaligned for precise GC */
      for (j = 0; j < c; j++) {
	o->array[i++] = a[j];
      }
    } else if (opt 
	       && (((opt > 0) && (k < total))
		   || ((opt < 0) && k))
	       && scheme_omittable_expr(v, -1)) {
      /* Value not the result. Do nothing. */
    } else
      o->array[i++] = v;
  }

  if (addconst)
    o->array[i] = scheme_make_integer(0);
  
  return (Scheme_Object *)o;
}

static Scheme_Object *look_for_letv_change(Scheme_Sequence *s)
{
  int i;

  /* Change (begin e1 ... (set!-for-let [x 10] (void)) e2 ...)
     to (begin e1 ... (set!-for-let [x 10] e2 ...)), which 
     avoids an unneeded recursive call in the evaluator */

  for (i = 0; i < s->count - 1; i++) {
    Scheme_Object *v;
    v = s->array[i];
    if (SAME_TYPE(SCHEME_TYPE(v), scheme_let_value_type)) {
      Scheme_Let_Value *lv = (Scheme_Let_Value *)v;
      if (scheme_omittable_expr(lv->body, 1)) {
	int esize = s->count - (i + 1);
	int nsize = i + 1;
	Scheme_Object *nv, *ev;

	if (nsize > 1) {
	  Scheme_Sequence *naya;

	  naya = malloc_sequence(nsize);
	  naya->so.type = scheme_sequence_type;
	  naya->count = nsize;
	  nv = (Scheme_Object *)naya;

	  for (i = 0; i < nsize; i++) {
	    naya->array[i] = s->array[i];
	  }
	} else
	  nv = (Scheme_Object *)lv;

	if (esize > 1) {
	  Scheme_Sequence *e;
	  e = malloc_sequence(esize);
	  e->so.type = scheme_sequence_type;
	  e->count = esize;

	  for (i = 0; i < esize; i++) {
	    e->array[i] = s->array[i + nsize];
	  }

	  ev = (Scheme_Object *)look_for_letv_change(e);
	} else
	  ev = s->array[nsize]; 

	lv->body = ev;

	return nv;
      }
    }
  }

  return (Scheme_Object *)s;
}

static Scheme_Object *resolve_sequence(Scheme_Object *o, Resolve_Info *info)
{
  Scheme_Sequence *s = (Scheme_Sequence *)o;
  int i;

  for (i = s->count; i--; ) {
    Scheme_Object *le;
    le = scheme_resolve_expr(s->array[i], info);
    s->array[i] = le;
  }
  
  return look_for_letv_change(s);
}

Scheme_Object *scheme_make_syntax_resolved(int idx, Scheme_Object *data)
{
  Scheme_Object *v;

  v = scheme_alloc_object();
  v->type = scheme_syntax_type;
  SCHEME_PINT_VAL(v) = idx;
  SCHEME_IPTR_VAL(v) = (void *)data;

  return v;
}

Scheme_Object *scheme_make_syntax_compiled(int idx, Scheme_Object *data)
{
  Scheme_Object *v;

  v = scheme_alloc_object();
  v->type = scheme_compiled_syntax_type;
  SCHEME_PINT_VAL(v) = idx;
  SCHEME_IPTR_VAL(v) = (void *)data;

  return v;  
}

static Scheme_Object *link_module_variable(Scheme_Object *modidx,
					   Scheme_Object *varname,
					   Scheme_Object *insp,
					   int pos, int mod_phase,
					   Scheme_Env *env)
{
  Scheme_Object *modname;
  Scheme_Env *menv;

  /* If it's a name id, resolve the name. */
  modname = scheme_module_resolve(modidx, 1);

  if (env->module && SAME_OBJ(env->module->modname, modname)
      && (env->mod_phase == mod_phase))
    menv = env;
  else {
    menv = scheme_module_access(modname, env, mod_phase);
    
    if (!menv && env->phase) {
      /* The failure might be due a laziness in required-syntax
	 execution. Force all laziness at the prior level 
	 and try again. */
      scheme_module_force_lazy(env, 1);
      menv = scheme_module_access(modname, env, mod_phase);
    }
    
    if (!menv) {
      scheme_wrong_syntax("link", NULL, varname,
			  "broken compiled code (phase %d, defn-phase %d, in %V), no declaration for module"
			  ": %S", 
			  env->phase, mod_phase,
			  env->module ? env->module->modname : scheme_false,
			  modname);
      return NULL;
    }

    if (!SAME_OBJ(menv, env)) {
      varname = scheme_check_accessible_in_module(menv, insp, NULL, varname, NULL, NULL, insp, pos, 0, NULL);
    }
  }

  return (Scheme_Object *)scheme_global_bucket(varname, menv);
}

static Scheme_Object *link_toplevel(Scheme_Object *expr, Scheme_Env *env,
					   Scheme_Object *src_modidx, 
					   Scheme_Object *dest_modidx)
{
  if (SAME_TYPE(SCHEME_TYPE(expr), scheme_variable_type)) {
    Scheme_Bucket_With_Home *b = (Scheme_Bucket_With_Home *)expr;
    
    if (!env || !b->home->module)
      return (Scheme_Object *)b;
    else
      return link_module_variable(b->home->module->modname,
				  (Scheme_Object *)b->bucket.bucket.key,
				  b->home->module->insp,
				  -1, b->home->mod_phase,
				  env);
  } else {
    Module_Variable *mv = (Module_Variable *)expr;
    
    return link_module_variable(scheme_modidx_shift(mv->modidx,
						    src_modidx,
						    dest_modidx),
				mv->sym, mv->insp,
				mv->pos, mv->mod_phase,
				env);
  }
}

static Scheme_Object *resolve_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *expr = (Scheme_Object *)p->ku.k.p1;
  Resolve_Info *info = (Resolve_Info *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return scheme_resolve_expr(expr, info);
}

Scheme_Object *scheme_resolve_expr(Scheme_Object *expr, Resolve_Info *info)
{
  Scheme_Type type = SCHEME_TYPE(expr);

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.k.p1 = (void *)expr;
    p->ku.k.p2 = (void *)info;

    return scheme_handle_stack_overflow(resolve_k);
  }
#endif

  switch (type) {
  case scheme_local_type:
    {
      int pos, flags;
      Scheme_Object *lifted;
      
      pos = scheme_resolve_info_lookup(info, SCHEME_LOCAL_POS(expr), &flags, &lifted, 0);
      if (lifted) {
        /* Lexical reference replaced with top-level reference for a lifted value: */
        return lifted;
      } else {
        return scheme_make_local((flags & SCHEME_INFO_BOXED) 
                                 ? scheme_local_unbox_type
                                 : scheme_local_type,
                                 pos);
      }
    }
  case scheme_compiled_syntax_type:
    {
      Scheme_Syntax_Resolver f;
	  
      f = scheme_syntax_resolvers[SCHEME_PINT_VAL(expr)];
      return f((Scheme_Object *)SCHEME_IPTR_VAL(expr), info);
    }
  case scheme_application_type:
    return resolve_application(expr, info, 0);
  case scheme_application2_type:
    return resolve_application2(expr, info, 0);
  case scheme_application3_type:
    return resolve_application3(expr, info, 0);
  case scheme_sequence_type:
    return resolve_sequence(expr, info);
  case scheme_branch_type:
    return resolve_branch(expr, info);
  case scheme_with_cont_mark_type:
    return resolve_wcm(expr, info);
  case scheme_compiled_unclosed_procedure_type:
    return scheme_resolve_closure_compilation(expr, info, 1, 0, 0, NULL);
  case scheme_compiled_let_void_type:
    return scheme_resolve_lets(expr, info);
  case scheme_compiled_toplevel_type:
    return scheme_resolve_toplevel(info, expr);
  case scheme_compiled_quote_syntax_type:
    {
      Scheme_Quote_Syntax *qs;
      int i, c, p;

      i = SCHEME_LOCAL_POS(expr);
      i = scheme_resolve_quote_syntax_offset(i, info);
      c = scheme_resolve_toplevel_pos(info);
      p = scheme_resolve_quote_syntax_pos(info);

      qs = MALLOC_ONE_TAGGED(Scheme_Quote_Syntax);
      qs->so.type = scheme_quote_syntax_type;
      qs->depth = c;
      qs->position = i;
      qs->midpoint = p;

      return (Scheme_Object *)qs;
    }
  case scheme_variable_type:
  case scheme_module_variable_type:
    scheme_signal_error("got top-level in wrong place");
    return 0;
  default:
    return expr;
  }
}

Scheme_Object *scheme_resolve_list(Scheme_Object *expr, Resolve_Info *info)
{
  Scheme_Object *first = scheme_null, *last = NULL;

  while (SCHEME_PAIRP(expr)) {
    Scheme_Object *pr;

    pr = scheme_make_pair(scheme_resolve_expr(SCHEME_CAR(expr), info),
			  scheme_null);

    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;

    expr = SCHEME_CDR(expr);
  }

  return first;
}

/*========================================================================*/
/*                               uncompile                                */
/*========================================================================*/

#if 0

/* For debugging, currently incomplete: */

static Scheme_Object *uncompile(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_uncompile_expr(Scheme_Object *expr, Resolve_Prefix *prefix);

static Scheme_Object *uncompile_k()
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *expr = (Scheme_Object *)p->ku.k.p1;
  Resolve_Prefix *prefix = (Resolve_Prefix *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return scheme_uncompile_expr(expr, prefix);
}

Scheme_Object *scheme_uncompile_expr(Scheme_Object *expr, Resolve_Prefix *prefix)
{
  char buf[32];

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.k.p1 = (void *)expr;
    p->ku.k.p2 = (void *)prefix;

    return scheme_handle_stack_overflow(uncompile_k);
  }
#endif

  switch (SCHEME_TYPE(expr)) {
  case scheme_toplevel_type:
    {
      expr = prefix->toplevels[SCHEME_TOPLEVEL_POS(expr)];
      if (SAME_TYPE(SCHEME_TYPE(expr), scheme_variable_type)) {
	return cons(scheme_intern_symbol("#%top"),
		    (Scheme_Object *)((Scheme_Bucket *)expr)->key);
      } else {
	Module_Variable *mv = (Module_Variable *)expr;

	return cons(scheme_intern_symbol("#%top"),
		    cons(mv->modidx, mv->sym));
      }
    }
  case scheme_local_type:
    {
      sprintf(buf, "@%d", SCHEME_LOCAL_POS(expr));
      return scheme_intern_symbol(buf);
    }
  case scheme_local_unbox_type:
    {
      sprintf(buf, "@!%d", SCHEME_LOCAL_POS(expr));
      return scheme_intern_symbol(buf);
    }
  case scheme_compiled_syntax_type:
    {
      return scheme_void;
    }
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)expr;
      int i;
      expr = scheme_null;
      for (i = app->num_args + 1; i--; ) {
	expr = cons(scheme_uncompile_expr(app->args[i], prefix),
		    expr);
      }
      return expr;
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr;
      return cons(scheme_uncompile_expr(app->rator, prefix),
		  cons(scheme_uncompile_expr(app->rand, prefix),
		       scheme_null));
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr;
      return cons(scheme_uncompile_expr(app->rator, prefix),
		  cons(scheme_uncompile_expr(app->rand1, prefix),
		       cons(scheme_uncompile_expr(app->rand2, prefix),
			    scheme_null)));
    }
  case scheme_sequence_type:
  case scheme_branch_type:
  case scheme_with_cont_mark_type:
    return scheme_void;
  case scheme_let_value_type:
    {
      Scheme_Let_Value *lv = (Scheme_Let_Value *)expr;
      sprintf(buf, "@%d", lv->position);
      return cons(scheme_intern_symbol("let!"),
		  cons(scheme_make_integer(lv->count),
		       cons(scheme_intern_symbol(buf),
			    cons(scheme_uncompile_expr(lv->value, prefix),
				 cons(scheme_uncompile_expr(lv->body, prefix),
				      scheme_null)))));
    }
  case scheme_let_void_type:
    {
      Scheme_Let_Void *lv = (Scheme_Let_Void *)expr;
      return cons(scheme_intern_symbol("let-undefined"),
		  cons(scheme_make_integer(lv->count),
		       cons(scheme_uncompile_expr(lv->body, prefix),
			    scheme_null)));
    }
  case scheme_letrec_type:
    {
      Scheme_Letrec *lr = (Scheme_Letrec *)expr;
      int i;

      expr = scheme_null;
      for (i = lr->count; i--; ) {
	sprintf(buf, "@%d", i);
	expr = cons(cons(scheme_intern_symbol(buf),
			 cons(scheme_uncompile_expr(lr->procs[i], prefix),
			      scheme_null)),
		    expr);
      }
      
      return cons(scheme_intern_symbol("letrec!"),
		  cons(expr,
		       cons(scheme_uncompile_expr(lr->body, prefix),
			    scheme_null)));
    }
  case scheme_let_one_type:
    {
      Scheme_Let_One *lo = (Scheme_Let_One *)expr;
      return cons(scheme_intern_symbol("let"),
		  cons(scheme_uncompile_expr(lo->value, prefix),
		       cons(scheme_uncompile_expr(lo->body, prefix),
			    scheme_null)));
    }
  case scheme_unclosed_procedure_type:
    {
      Scheme_Closure_Data *data = (Scheme_Closure_Data *)expr;
      Scheme_Object *vec;
      int i;
      vec = scheme_make_vector(data->closure_size, NULL);
      for (i = data->closure_size; i--; ) {
	SCHEME_VEC_ELS(vec)[i] = scheme_make_integer(data->closure_map[i]);
      }
      return cons(scheme_intern_symbol((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST) ? "lambda*" : "lambda"),
		  cons(data->name ? data->name : scheme_false,
		       cons(scheme_make_integer(data->num_params),
			    cons(vec,
				 cons(scheme_uncompile_expr(data->code, prefix),
				      scheme_null)))));
    }
  default:
    if (SCHEME_CLOSUREP(expr)) {
      return scheme_uncompile_expr((Scheme_Object *)SCHEME_COMPILED_CLOS_CODE(expr), prefix);
    }
    return cons(scheme_intern_symbol("quote"), cons(expr, scheme_null));
  }
}

static Scheme_Object *
uncompile(int argc, Scheme_Object *argv[])
{
  Scheme_Compilation_Top *t;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_compilation_top_type))
    scheme_wrong_type("compiled->datum", "compiled code", 0, argc, argv);

  t = (Scheme_Compilation_Top *)argv[0];

  return scheme_uncompile_expr(t->code, t->prefix);
}

#endif

/*========================================================================*/
/*                               optimize                                 */
/*========================================================================*/

static Scheme_Object *try_optimize_fold(Scheme_Object *f, Scheme_Object *o, Optimize_Info *info)
{
  if ((SCHEME_PRIMP(f) 
       && (((Scheme_Primitive_Proc *)f)->pp.flags & SCHEME_PRIM_IS_FOLDING))
      || (SCHEME_CLSD_PRIMP(f) 
	  && (((Scheme_Closed_Primitive_Proc *)f)->pp.flags & SCHEME_PRIM_IS_FOLDING))) {
    Scheme_Object *args;
    
    switch (SCHEME_TYPE(o)) {
    case scheme_application_type:
      {
	Scheme_App_Rec *app = (Scheme_App_Rec *)o;
	int i;
	
	args = scheme_null;
	for (i = app->num_args; i--; ) {
	  args = scheme_make_pair(app->args[i + 1], args);
	}
      }
      break;
    case scheme_application2_type:
      {
	Scheme_App2_Rec *app = (Scheme_App2_Rec *)o;
	args = scheme_make_pair(app->rand, scheme_null);
      }
      break;
    case scheme_application3_type:
    default:
      {
	Scheme_App3_Rec *app = (Scheme_App3_Rec *)o;
	args = scheme_make_pair(app->rand1, 
				scheme_make_pair(app->rand2,
						 scheme_null));
      }
      break;
    }
    
    return try_apply(f, args);
  }
  
  return NULL;
}

static Scheme_Object *apply_inlined(Scheme_Object *p, Scheme_Closure_Data *data, Optimize_Info *info,
				    int argc, Scheme_App_Rec *app, Scheme_App2_Rec *app2, Scheme_App3_Rec *app3)
{
  Scheme_Let_Header *lh;
  Scheme_Compiled_Let_Value *lv, *prev = NULL;
  int i;
  int *flags, flag;

  if (!argc) {
    info = scheme_optimize_info_add_frame(info, 0, 0, 0);
    info->inline_fuel >>= 1;
    p = scheme_optimize_expr(p, info);
    info->next->single_result = info->single_result;
    info->next->preserves_marks = info->preserves_marks;
    scheme_optimize_info_done(info);
    return p;
  }

  lh = MALLOC_ONE_TAGGED(Scheme_Let_Header);
  lh->iso.so.type = scheme_compiled_let_void_type;
  lh->count = argc;
  lh->num_clauses = argc;

  for (i = 0; i < argc; i++) {
    lv = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
    lv->so.type = scheme_compiled_let_value_type;
    lv->count = 1;
    lv->position = i;

    if (app)
      lv->value = app->args[i + 1];
    else if (app3)
      lv->value = (i ? app3->rand2 : app3->rand1);
    else if (app2)
      lv->value = app2->rand;

    flag = scheme_closure_argument_flags(data, i);
    flags = (int *)scheme_malloc_atomic(sizeof(int));
    flags[0] = flag;
    lv->flags = flags;

    if (prev)
      prev->body = (Scheme_Object *)lv;
    else
      lh->body = (Scheme_Object *)lv;
    prev = lv;
  }

  if (prev)
    prev->body = p;
  else
    lh->body = p;

  return scheme_optimize_lets((Scheme_Object *)lh, info, 1);
}

#if 0
# define LOG_INLINE(x) x
#else
# define LOG_INLINE(x) /*empty*/
#endif

Scheme_Object *optimize_for_inline(Optimize_Info *info, Scheme_Object *le, int argc,
				   Scheme_App_Rec *app, Scheme_App2_Rec *app2, Scheme_App3_Rec *app3,
                                   int *_flags)
/* If not app, app2, or app3, just return a known procedure, if any */
{
  int offset = 0;

  if (SAME_TYPE(SCHEME_TYPE(le), scheme_local_type)) {
    /* Check for inlining: */
    le = scheme_optimize_info_lookup(info, SCHEME_LOCAL_POS(le), &offset);
    if (!le)
      return NULL;
  }

  while (SAME_TYPE(SCHEME_TYPE(le), scheme_compiled_toplevel_type)) {
    if (info->top_level_consts) {
      int pos;
      pos = SCHEME_TOPLEVEL_POS(le);
      le = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
      if (!le)
	return NULL;
    } else
      return NULL;
  }

  if (le && SAME_TYPE(SCHEME_TYPE(le), scheme_compiled_unclosed_procedure_type)) {
    Scheme_Closure_Data *data = (Scheme_Closure_Data *)le;
    int sz;

    if (!app && !app2 && !app3) {
      return le;
    }

    *_flags = SCHEME_CLOSURE_DATA_FLAGS(data);
      
    if (data->num_params == argc) {
      sz = scheme_closure_body_size(data, 1);
      if ((sz >= 0) && (sz <= (info->inline_fuel * (argc + 2)))) {
	le = scheme_optimize_clone(0, data->code, info, offset, argc);
	if (le) {
	  LOG_INLINE(fprintf(stderr, "Inline %s\n", data->name ? scheme_write_to_string(data->name, NULL) : "???"));
	  return apply_inlined(le, data, info, argc, app, app2, app3);
	} else {
          LOG_INLINE(fprintf(stderr, "No inline %s\n", data->name ? scheme_write_to_string(data->name, NULL) : "???"));
        }
      } else {
        LOG_INLINE(fprintf(stderr, "No fuel %s %d*%d/%d\n", data->name ? scheme_write_to_string(data->name, NULL) : "???", 
                           sz, info->inline_fuel * (argc + 2),
                           info->inline_fuel));
      }
    }
  }

  if (le && SCHEME_PRIMP(le)) {
    if (((Scheme_Prim_Proc_Header *)le)->flags & SCHEME_PRIM_IS_NONCM)
      *_flags = (CLOS_PRESERVES_MARKS | CLOS_SINGLE_RESULT);
  }
  
  return NULL;
}

static void reset_rator(Scheme_Object *app, Scheme_Object *a)
{
  switch (SCHEME_TYPE(app)) {
  case scheme_application_type:
    ((Scheme_App_Rec *)app)->args[0] = a;
    break;
  case scheme_application2_type:
    ((Scheme_App2_Rec *)app)->rator = a;
    break;
  case scheme_application3_type:
    ((Scheme_App3_Rec *)app)->rator = a;
    break;
  }
}

static Scheme_Object *check_app_let_rator(Scheme_Object *app, Scheme_Object *rator, Optimize_Info *info, int argc)
{
  if (SAME_TYPE(SCHEME_TYPE(rator), scheme_compiled_let_void_type)) {
    Scheme_Let_Header *head = (Scheme_Let_Header *)rator;

    if ((head->count == 1) && (head->num_clauses == 1)) {
      Scheme_Object *body;
      Scheme_Compiled_Let_Value *clv;

      clv = (Scheme_Compiled_Let_Value *)head->body;
      body = clv->body;
      if (SAME_TYPE(SCHEME_TYPE(body), scheme_local_type)
          && (SCHEME_LOCAL_POS(body) == 0)
          && scheme_is_compiled_procedure(clv->value, 1, 1)) {
        
        reset_rator(app, scheme_false);
        app = scheme_optimize_shift(app, 1, 0);
        reset_rator(app, scheme_make_local(scheme_local_type, 0));

        clv->body = app;
        
        if (clv->flags[0] & SCHEME_WAS_APPLIED_EXCEPT_ONCE) {
          clv->flags[0] -= SCHEME_WAS_APPLIED_EXCEPT_ONCE;
          clv->flags[0] |= SCHEME_WAS_ONLY_APPLIED;
        }
        
        return scheme_optimize_expr(rator, info);
      }
    }
  }

  return NULL;
}

static Scheme_Object *optimize_application(Scheme_Object *o, Optimize_Info *info)
{
  Scheme_Object *le;
  Scheme_App_Rec *app;
  int i, n, all_vals = 1, rator_flags = 0;

  app = (Scheme_App_Rec *)o;

  le = check_app_let_rator(o, app->args[0], info, app->num_args);
  if (le) return le;

  n = app->num_args + 1;

  for (i = 0; i < n; i++) {
    if (!i) {
      le = optimize_for_inline(info, app->args[i], n - 1, app, NULL, NULL, &rator_flags);
      if (le)
	return le;
    }
     
    le = scheme_optimize_expr(app->args[i], info);
    app->args[i] = le;

    if (i && (SCHEME_TYPE(le) < _scheme_compiled_values_types_))
      all_vals = 0;
  }

  if (all_vals) {
    le = try_optimize_fold(app->args[0], (Scheme_Object *)app, info);
    if (le)
      return le;
  }

  info->size += 1;

  info->preserves_marks = !!(rator_flags & CLOS_PRESERVES_MARKS);
  info->single_result = !!(rator_flags & CLOS_SINGLE_RESULT);
  if (rator_flags & CLOS_RESULT_TENTATIVE) {
    info->preserves_marks = -info->preserves_marks;
    info->single_result = -info->single_result;
  }

  return (Scheme_Object *)app;
}

static Scheme_Object *optimize_application2(Scheme_Object *o, Optimize_Info *info)
{
  Scheme_App2_Rec *app;
  Scheme_Object *le;
  int rator_flags = 0;

  app = (Scheme_App2_Rec *)o;

  le = check_app_let_rator(o, app->rator, info, 1);
  if (le) return le;

  le = optimize_for_inline(info, app->rator, 1, NULL, app, NULL, &rator_flags);
  if (le)
    return le;

  le = scheme_optimize_expr(app->rator, info);
  app->rator = le;

  le = scheme_optimize_expr(app->rand, info);
  app->rand = le;
  if (SCHEME_TYPE(le) > _scheme_compiled_values_types_) {
    le = try_optimize_fold(app->rator, (Scheme_Object *)app, info);
    if (le)
      return le;
  }

  if (SAME_OBJ(scheme_procedure_p_proc, app->rator)) {
    if (SAME_TYPE(scheme_compiled_unclosed_procedure_type, SCHEME_TYPE(app->rand))) {
      info->preserves_marks = 1;
      info->single_result = 1;
      return scheme_true;
    }
    if (SAME_TYPE(SCHEME_TYPE(app->rand), scheme_local_type)) {
      int offset;
      if (scheme_optimize_info_lookup(info, SCHEME_LOCAL_POS(app->rand), &offset)) {
        info->preserves_marks = 1;
        info->single_result = 1;
        return scheme_true;
      }
    }
  }

  if (SAME_OBJ(scheme_values_func, app->rator)
      && scheme_omittable_expr(app->rand, 1)) {
    info->preserves_marks = 1;
    info->single_result = 1;
    return app->rand;
  }

  info->preserves_marks = !!(rator_flags & CLOS_PRESERVES_MARKS);
  info->single_result = !!(rator_flags & CLOS_SINGLE_RESULT);
  if (rator_flags & CLOS_RESULT_TENTATIVE) {
    info->preserves_marks = -info->preserves_marks;
    info->single_result = -info->single_result;
  }

  return (Scheme_Object *)app;
}

static Scheme_Object *optimize_application3(Scheme_Object *o, Optimize_Info *info)
{
  Scheme_App3_Rec *app;
  Scheme_Object *le;
  int all_vals = 1;
  int rator_flags = 0;

  app = (Scheme_App3_Rec *)o;

  le = check_app_let_rator(o, app->rator, info, 2);
  if (le) return le;

  le = optimize_for_inline(info, app->rator, 2, NULL, NULL, app, &rator_flags);
  if (le)
    return le;

  le = scheme_optimize_expr(app->rator, info);
  app->rator = le;

  /* 1st arg */

  le = scheme_optimize_expr(app->rand1, info);
  app->rand1 = le;

  if (SCHEME_TYPE(le) < _scheme_compiled_values_types_)
    all_vals = 0;

  /* 2nd arg */

  le = scheme_optimize_expr(app->rand2, info);
  app->rand2 = le;

  if (SCHEME_TYPE(le) < _scheme_compiled_values_types_)
    all_vals = 0;

  /* Fold or continue */

  if (all_vals) {
    le = try_optimize_fold(app->rator, (Scheme_Object *)app, info);
    if (le)
      return le;
  }

  info->size += 1;

  /* Check for (call-with-values (lambda () M) N): */
  if (SAME_OBJ(app->rator, scheme_call_with_values_proc)) {
    if (SAME_TYPE(SCHEME_TYPE(app->rand1), scheme_compiled_unclosed_procedure_type)) {
      Scheme_Closure_Data *data = (Scheme_Closure_Data *)app->rand1;

      if (!data->num_params) {
        /* Convert to apply-values form: */ 
        return scheme_optimize_apply_values(app->rand2, data->code, info,
                                            ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_SINGLE_RESULT)
                                             ? ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_RESULT_TENTATIVE)
                                                ? -1
                                                : 1)
                                             : 0));
      }
    }
  }

  info->preserves_marks = !!(rator_flags & CLOS_PRESERVES_MARKS);
  info->single_result = !!(rator_flags & CLOS_SINGLE_RESULT);
  if (rator_flags & CLOS_RESULT_TENTATIVE) {
    info->preserves_marks = -info->preserves_marks;
    info->single_result = -info->single_result;
  }

  return (Scheme_Object *)app;
}

Scheme_Object *scheme_optimize_apply_values(Scheme_Object *f, Scheme_Object *e, 
                                            Optimize_Info *info,
                                            int e_single_result)
/* f and e are already optimized */
{
  Scheme_Object *f_is_proc = NULL;

  info->preserves_marks = 0;
  info->single_result = 0;

  {
    Scheme_Object *rev;
    if (SAME_TYPE(SCHEME_TYPE(f), scheme_local_type)) {
      rev = scheme_optimize_reverse(info, SCHEME_LOCAL_POS(f), 1);
    } else
      rev = f;

    if (rev) {
      int rator2_flags;
      Scheme_Object *o_f;
      o_f = optimize_for_inline(info, rev, 1, NULL, NULL, NULL, &rator2_flags);
      if (o_f) {
        f_is_proc = rev;

        if (SAME_TYPE(SCHEME_TYPE(o_f), scheme_compiled_unclosed_procedure_type)) {
          Scheme_Closure_Data *data2 = (Scheme_Closure_Data *)o_f;
          int flags = SCHEME_CLOSURE_DATA_FLAGS(data2);
          info->preserves_marks = !!(flags & CLOS_PRESERVES_MARKS);
          info->single_result = !!(flags & CLOS_SINGLE_RESULT);
          if (flags & CLOS_RESULT_TENTATIVE) {
            info->preserves_marks = -info->preserves_marks;
            info->single_result = -info->single_result;
          }
        }
      }
    }
    
    if (!f_is_proc && SCHEME_PROCP(f)) {
      f_is_proc = f;
    }
  }

  if (f_is_proc && (e_single_result > 0)) {
    /* Just make it an application (N M): */
    Scheme_App2_Rec *app2;
    Scheme_Object *cloned, *f_cloned;

    app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
    app2->iso.so.type = scheme_application2_type;
    
    /* We'd like to try to inline here. The problem is that
       e (the argument) has been optimized already,
       which means it's in the wrong coordinate system.
       If we can shift-clone it, then it will be back in the right
       coordinates. */
    
    cloned = scheme_optimize_clone(1, e, info, 0, 0);
    if (cloned) {
      if (SAME_TYPE(SCHEME_TYPE(f_is_proc), scheme_compiled_unclosed_procedure_type))
        f_cloned = scheme_optimize_clone(1, f_is_proc, info, 0, 0);
      else {
        /* Otherwise, no clone is needed; in the case of a lexical
           variable, we already reversed it. */
        f_cloned = f_is_proc;
      }

      if (f_cloned) {
        app2->rator = f_cloned;
        app2->rand = cloned;
        return optimize_application2((Scheme_Object *)app2, info);
      }
    }
     
    app2->rator = f;
    app2->rand = e;
    return (Scheme_Object *)app2;
  }

  return scheme_make_syntax_compiled(APPVALS_EXPD, cons(f, e));
}

static Scheme_Object *optimize_sequence(Scheme_Object *o, Optimize_Info *info)
{
  Scheme_Sequence *s = (Scheme_Sequence *)o;
  Scheme_Object *le;
  int i;
  int drop = 0, preserves_marks = 0, single_result = 0;

  for (i = s->count; i--; ) {
    le = scheme_optimize_expr(s->array[i], info);
    if (i == s->count - 1) {
      single_result = info->single_result;
      preserves_marks = info->preserves_marks;
    }

    /* Inlining and constant propagation can expose
       omittable expressions. */
    if ((i + 1 != s->count)
	&& scheme_omittable_expr(le, -1)) {
      drop++;
      s->array[i] = NULL;
    } else {
      s->array[i] = le;
    }
  }

  info->preserves_marks = preserves_marks;
  info->single_result = single_result;

  if (drop + 1 == s->count) {
    return s->array[drop];
  } else if (drop) {
    Scheme_Sequence *s2;
    int j = 0;

    s2 = malloc_sequence(s->count - drop);
    s2->so.type = scheme_sequence_type;
    s2->count = s->count - drop;

    for (i = 0; i < s->count; i++) {
      if (s->array[i]) {
	s2->array[j++] = s->array[i];
      }
    }

    s = s2;
  }

  info->size += 1;

  return (Scheme_Object *)s;
}

int scheme_compiled_duplicate_ok(Scheme_Object *fb)
{
  return (SCHEME_VOIDP(fb)
	  || SAME_OBJ(fb, scheme_true)
	  || SCHEME_FALSEP(fb)
	  || SCHEME_SYMBOLP(fb)
	  || SCHEME_KEYWORDP(fb)
	  || SCHEME_EOFP(fb)
	  || SCHEME_INTP(fb)
	  || SCHEME_NULLP(fb)
	  || (SCHEME_CHARP(fb) && (SCHEME_CHAR_VAL(fb) < 256))
	  || SAME_TYPE(SCHEME_TYPE(fb), scheme_local_type)
          /* Values that are hashed by the printer to avoid
             duplication: */
	  || SCHEME_CHAR_STRINGP(fb) 
          || SCHEME_BYTE_STRINGP(fb)
          || SAME_TYPE(SCHEME_TYPE(fb), scheme_regexp_type)
          || SCHEME_NUMBERP(fb)
	  || SCHEME_PRIMP(fb));
}

static Scheme_Object *optimize_branch(Scheme_Object *o, Optimize_Info *info)
{
  Scheme_Branch_Rec *b;
  Scheme_Object *t, *tb, *fb;
  int preserves_marks = 1, single_result = 1;

  b = (Scheme_Branch_Rec *)o;

  t = b->test;
  tb = b->tbranch;
  fb = b->fbranch;
  
  /* Try optimize: (if (not x) y z) => (if x z y) */
  while (1) {
    if (SAME_TYPE(SCHEME_TYPE(t), scheme_application2_type)) {
      Scheme_App2_Rec *app;
      
      app = (Scheme_App2_Rec *)t;
      if (SAME_PTR(scheme_not_prim, app->rator)) {
	t = tb;
	tb = fb;
	fb = t;
	t = app->rand;
      } else
	break;
    } else
      break;
  }

  if (SAME_TYPE(SCHEME_TYPE(t), scheme_compiled_let_void_type)) {
    /* Maybe convert: (let ([x M]) (if x x N)) => (if M #t N) */
    t = scheme_optimize_lets_for_test(t, info);
  } else
    t = scheme_optimize_expr(t, info);

  if (SCHEME_TYPE(t) > _scheme_compiled_values_types_) {
    if (SCHEME_FALSEP(t))
      return scheme_optimize_expr(fb, info);
    else
      return scheme_optimize_expr(tb, info);
  } else if (SAME_TYPE(SCHEME_TYPE(t), scheme_compiled_quote_syntax_type)
             || SAME_TYPE(SCHEME_TYPE(t), scheme_compiled_unclosed_procedure_type))
    return scheme_optimize_expr(tb, info);

  tb = scheme_optimize_expr(tb, info);

  if (!info->preserves_marks) preserves_marks = 0;
  if (!info->single_result) single_result = 0;

  fb = scheme_optimize_expr(fb, info);

  if (!info->preserves_marks) preserves_marks = 0;
  if (!info->single_result) single_result = 0;

  info->preserves_marks = preserves_marks;
  info->single_result = single_result;

  /* Try optimize: (if x x #f) => x */
  if (SAME_TYPE(SCHEME_TYPE(t), scheme_local_type)
      && SAME_TYPE(SCHEME_TYPE(tb), scheme_local_type)
      && (SCHEME_LOCAL_POS(t) == SCHEME_LOCAL_POS(tb))
      && SCHEME_FALSEP(fb)) {
    return t;
  }

  /* Convert: (if (if M N #f) M2 K) => (if M (if N M2 K) K)
     for simple constants K. This is useful to expose simple
     tests to the JIT. */
  if (SAME_TYPE(SCHEME_TYPE(t), scheme_branch_type)
      && scheme_compiled_duplicate_ok(fb)) {
    Scheme_Branch_Rec *b2 = (Scheme_Branch_Rec *)t;
    if (SCHEME_FALSEP(b2->fbranch)) {
      Scheme_Branch_Rec *b3;
      b3 = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
      b3->so.type = scheme_branch_type;
      b3->test = b2->tbranch;
      b3->tbranch = tb;
      b3->fbranch = fb;
      t = b2->test;
      tb = (Scheme_Object *)b3;
    }
  }

  b->test = t;
  b->tbranch = tb;
  b->fbranch = fb;

  info->size += 1;

  return o;
}

static Scheme_Object *optimize_wcm(Scheme_Object *o, Optimize_Info *info)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)o;
  Scheme_Object *k, *v, *b;

  k = scheme_optimize_expr(wcm->key, info);

  v = scheme_optimize_expr(wcm->val, info);

  b = scheme_optimize_expr(wcm->body, info);

  /* info->single_result is already set */
  info->preserves_marks = 0;

  wcm->key = k;
  wcm->val = v;
  wcm->body = b;

  info->size += 1;

  return (Scheme_Object *)wcm;
}

static Scheme_Object *optimize_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *expr = (Scheme_Object *)p->ku.k.p1;
  Optimize_Info *info = (Optimize_Info *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return scheme_optimize_expr(expr, info);
}

Scheme_Object *scheme_optimize_expr(Scheme_Object *expr, Optimize_Info *info)
{
  Scheme_Type type = SCHEME_TYPE(expr);

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.k.p1 = (void *)expr;
    p->ku.k.p2 = (void *)info;

    return scheme_handle_stack_overflow(optimize_k);
  }
#endif

  info->preserves_marks = 1;
  info->single_result = 1;

  switch (type) {
  case scheme_local_type:
    {
      Scheme_Object *val;
      int pos, delta;
      
      info->size += 1;

      pos = SCHEME_LOCAL_POS(expr);

      val = scheme_optimize_info_lookup(info, pos, NULL);
      if (val) {
        if (SAME_TYPE(SCHEME_TYPE(val), scheme_compiled_toplevel_type))
          return scheme_optimize_expr(val, info);
	return val;
      }

      delta = scheme_optimize_info_get_shift(info, pos);
      if (delta)
	expr = scheme_make_local(scheme_local_type, pos + delta);

      return expr;
    }
  case scheme_compiled_syntax_type:
    {
      Scheme_Syntax_Optimizer f;
	  
      f = scheme_syntax_optimizers[SCHEME_PINT_VAL(expr)];
      return f((Scheme_Object *)SCHEME_IPTR_VAL(expr), info);
    }
  case scheme_application_type:
    return optimize_application(expr, info);
  case scheme_application2_type:
    return optimize_application2(expr, info);
  case scheme_application3_type:
    return optimize_application3(expr, info);
  case scheme_sequence_type:
    return optimize_sequence(expr, info);
  case scheme_branch_type:
    return optimize_branch(expr, info);
  case scheme_with_cont_mark_type:
    return optimize_wcm(expr, info);
  case scheme_compiled_unclosed_procedure_type:
    return scheme_optimize_closure_compilation(expr, info);
  case scheme_compiled_let_void_type:
    return scheme_optimize_lets(expr, info, 0);
  case scheme_compiled_toplevel_type:
    if (info->top_level_consts) {
      int pos;
      Scheme_Object *c;

      while (1) {
        pos = SCHEME_TOPLEVEL_POS(expr);
        c = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
        if (c && SAME_TYPE(SCHEME_TYPE(c), scheme_compiled_toplevel_type))
          expr = c;
        else
          break;
      }

      if (c) {
	if (scheme_compiled_duplicate_ok(c))
	  return c;

	/* We can't inline, but mark the top level as a constant, 
	   so we can direct-jump and avoid null checks in JITed code: */
	expr = scheme_toplevel_to_flagged_toplevel(expr, SCHEME_TOPLEVEL_CONST);
      } else {
	/* false is mapped to a table of non-constant ready values: */
	c = scheme_hash_get(info->top_level_consts, scheme_false);
	if (c) {
	  c = scheme_hash_get((Scheme_Hash_Table *)c, scheme_make_integer(pos));

	  if (c) {
	    /* We can't inline, but mark the top level as ready, 
	       so we can avoid null checks in JITed code: */
	    expr = scheme_toplevel_to_flagged_toplevel(expr, SCHEME_TOPLEVEL_READY);
	  }
	}
      }
    }
    scheme_optimize_info_used_top(info);
    return expr;
  case scheme_compiled_quote_syntax_type:
    scheme_optimize_info_used_top(info);
    return expr;
  case scheme_variable_type:
  case scheme_module_variable_type:
    scheme_signal_error("got top-level in wrong place");
    return 0;
  default:
    info->size += 1;
    return expr;
  }
}

Scheme_Object *scheme_optimize_clone(int dup_ok, Scheme_Object *expr, Optimize_Info *info, int delta, int closure_depth)
/* Past closure_depth, need to reverse optimize to unoptimzed with respect to info;
   delta is the amount to skip in info to get to the frame that bound the code.
   If dup_ok is 1, then the old copy will be dropped, so it's ok to "duplicate"
   any constant. */
{
  int t;

  t = SCHEME_TYPE(expr);

  switch(t) {
  case scheme_local_type:
    {
      int pos = SCHEME_LOCAL_POS(expr);
      if (pos >= closure_depth) {
	expr = scheme_optimize_reverse(info, pos + delta - closure_depth, 0);
	if (closure_depth)
	  expr = scheme_make_local(scheme_local_type, SCHEME_LOCAL_POS(expr) + closure_depth);
      }
      return expr;
    }
  case scheme_compiled_syntax_type:
    {
      Scheme_Syntax_Cloner f;
	  
      f = scheme_syntax_cloners[SCHEME_PINT_VAL(expr)];
      if (!f) return NULL;
      return f(dup_ok, (Scheme_Object *)SCHEME_IPTR_VAL(expr), info, delta, closure_depth);
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr, *app2;
      
      app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
      app2->iso.so.type = scheme_application2_type;
      
      expr = scheme_optimize_clone(dup_ok, app->rator, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rator = expr;
      
      expr = scheme_optimize_clone(dup_ok, app->rand, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rand = expr;

      return (Scheme_Object *)app2;
    }
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)expr, *app2;
      int i;
      
      app2 = scheme_malloc_application(app->num_args + 1);

      for (i = app->num_args + 1; i--; ) {
	expr = scheme_optimize_clone(dup_ok, app->args[i], info, delta, closure_depth);
	if (!expr) return NULL;
	app2->args[i] = expr;
      }

      return (Scheme_Object *)app2;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr, *app2;
      
      app2 = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
      app2->iso.so.type = scheme_application3_type;
      
      expr = scheme_optimize_clone(dup_ok, app->rator, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rator = expr;
      
      expr = scheme_optimize_clone(dup_ok, app->rand1, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rand1 = expr;
      
      expr = scheme_optimize_clone(dup_ok, app->rand2, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rand2 = expr;

      return (Scheme_Object *)app2;
    }
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *head = (Scheme_Let_Header *)expr, *head2;
      Scheme_Object *body;
      Scheme_Compiled_Let_Value *lv, *lv2, *prev = NULL;
      int i, *flags, sz;

      head2 = MALLOC_ONE_TAGGED(Scheme_Let_Header);
      head2->iso.so.type = scheme_compiled_let_void_type;
      head2->count = head->count;
      head2->num_clauses = head->num_clauses;
      SCHEME_LET_FLAGS(head2) = SCHEME_LET_FLAGS(head);

      /* Build let-value change: */
      body = head->body;
      for (i = head->num_clauses; i--; ) {
	lv = (Scheme_Compiled_Let_Value *)body;

	sz = sizeof(int) * lv->count;
	flags = (int *)scheme_malloc_atomic(sz);
	memcpy(flags, lv->flags, sz);

	lv2 = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
	lv2->so.type = scheme_compiled_let_value_type;
	lv2->count = lv->count;
	lv2->position = lv->position;
	lv2->flags = flags;

	expr = scheme_optimize_clone(dup_ok, lv->value, info, delta, closure_depth + head->count);
	if (!expr) return NULL;
	lv2->value = expr;

	if (prev)
	  prev->body = (Scheme_Object *)lv2;
	else
	  head2->body = (Scheme_Object *)lv2;
	prev = lv2;

	body = lv->body;
      }
      if (prev) 
	prev->body = body;
      else
	head2->body = body;

      expr = scheme_optimize_clone(dup_ok, body, info, delta, closure_depth + head->count);
      if (!expr) return NULL;

      if (prev) 
	prev->body = expr;
      else
	head2->body = expr;
      
      return (Scheme_Object *)head2;
    }
  case scheme_sequence_type:
  case scheme_begin0_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)expr, *seq2;
      int i;

      seq2 = malloc_sequence(seq->count);
      seq2->so.type = seq->so.type;
      seq2->count = seq->count;

      for (i = seq->count; i--; ) {
	expr = scheme_optimize_clone(dup_ok, seq->array[i], info, delta, closure_depth);
	if (!expr) return NULL;
	seq2->array[i] = expr;
      }
      
      return (Scheme_Object *)seq2;
    }
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr, *b2;

      b2 = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
      b2->so.type = scheme_branch_type;

      expr = scheme_optimize_clone(dup_ok, b->test, info, delta, closure_depth);
      if (!expr) return NULL;
      b2->test = expr;

      expr = scheme_optimize_clone(dup_ok, b->tbranch, info, delta, closure_depth);
      if (!expr) return NULL;
      b2->tbranch = expr;

      expr = scheme_optimize_clone(dup_ok, b->fbranch, info, delta, closure_depth);
      if (!expr) return NULL;
      b2->fbranch = expr;

      return (Scheme_Object *)b2;
    }
  case scheme_compiled_unclosed_procedure_type:
    return scheme_clone_closure_compilation(dup_ok, expr, info, delta, closure_depth);
  case scheme_compiled_toplevel_type:
  case scheme_compiled_quote_syntax_type:
    return expr;
  default:
    if (t > _scheme_compiled_values_types_) {
      if (dup_ok || scheme_compiled_duplicate_ok(expr))
	return expr;
    }
  }

  return NULL;
}

Scheme_Object *scheme_optimize_shift(Scheme_Object *expr, int delta, int after_depth)
/* Shift lexical addresses deeper by delta if already deeper than after_depth;
   can mutate. */
{
  int t;

  /* FIXME: need stack check */
    
  t = SCHEME_TYPE(expr);
  
  switch(t) {
  case scheme_local_type:
  case scheme_local_unbox_type:
    {
      int pos = SCHEME_LOCAL_POS(expr);
      if (pos >= after_depth) {
        expr = scheme_make_local(t, SCHEME_LOCAL_POS(expr) + delta);
      }
      return expr;
    }
  case scheme_compiled_syntax_type:
    {
      Scheme_Syntax_Shifter f;
      
      f = scheme_syntax_shifters[SCHEME_PINT_VAL(expr)];
      
      if (!f) {
        scheme_signal_error("scheme_optimize_shift: no shift available for %d", SCHEME_PINT_VAL(expr));
        return NULL;
      }
      return f((Scheme_Object *)SCHEME_IPTR_VAL(expr), delta, after_depth);
    }
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)expr;
      int i;
      
      for (i = app->num_args + 1; i--; ) {
	expr = scheme_optimize_shift(app->args[i], delta, after_depth);
	app->args[i] = expr;
      }

      return (Scheme_Object *)app;
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr;
      
      expr = scheme_optimize_shift(app->rator, delta, after_depth);
      app->rator = expr;
      
      expr = scheme_optimize_shift(app->rand, delta, after_depth);
      app->rand = expr;

      return (Scheme_Object *)app;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr;
      
      expr = scheme_optimize_shift(app->rator, delta, after_depth);
      app->rator = expr;
      
      expr = scheme_optimize_shift(app->rand1, delta, after_depth);
      app->rand1 = expr;
      
      expr = scheme_optimize_shift(app->rand2, delta, after_depth);
      app->rand2 = expr;

      return (Scheme_Object *)app;
    }
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *head = (Scheme_Let_Header *)expr;
      Scheme_Object *body;
      Scheme_Compiled_Let_Value *lv = NULL;
      int i;

      /* Build let-value change: */
      body = head->body;
      for (i = head->num_clauses; i--; ) {
	lv = (Scheme_Compiled_Let_Value *)body;

	expr = scheme_optimize_shift(lv->value, delta, after_depth + head->count);
	lv->value = expr;

        body = lv->body;
      }
      expr = scheme_optimize_shift(body, delta, after_depth + head->count);

      if (head->num_clauses)
	lv->body = expr;
      else
	head->body = expr;
      
      return (Scheme_Object *)head;
    }
  case scheme_sequence_type:
  case scheme_begin0_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)expr;
      int i;

      for (i = seq->count; i--; ) {
	expr = scheme_optimize_shift(seq->array[i], delta, after_depth);
	seq->array[i] = expr;
      }
      
      return (Scheme_Object *)seq;
    }
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr;

      expr = scheme_optimize_shift(b->test, delta, after_depth);
      b->test = expr;

      expr = scheme_optimize_shift(b->tbranch, delta, after_depth);
      b->tbranch = expr;

      expr = scheme_optimize_shift(b->fbranch, delta, after_depth);
      b->fbranch = expr;

      return (Scheme_Object *)b;
    }
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)expr;

      expr = scheme_optimize_shift(wcm->key, delta, after_depth);
      wcm->key = expr;

      expr = scheme_optimize_shift(wcm->val, delta, after_depth);
      wcm->val = expr;

      expr = scheme_optimize_shift(wcm->body, delta, after_depth);
      wcm->body = expr;

      return (Scheme_Object *)wcm;      
    }
  case scheme_compiled_unclosed_procedure_type:
    return scheme_shift_closure_compilation(expr, delta, after_depth);
  case scheme_compiled_toplevel_type:
  case scheme_compiled_quote_syntax_type:
    return expr;
  default:
    return expr;
  }

  return NULL;
}

/*========================================================================*/
/*                                  JIT                                   */
/*========================================================================*/

#ifdef MZ_USE_JIT

static Scheme_Object *jit_application(Scheme_Object *o)
{
  Scheme_Object *orig, *naya = NULL;
  Scheme_App_Rec *app, *app2;
  int i, n, size;

  app = (Scheme_App_Rec *)o;
  n = app->num_args + 1;

  for (i = 0; i < n; i++) {
    orig = app->args[i];
    naya = scheme_jit_expr(orig);
    if (!SAME_OBJ(orig, naya))
      break;
  }

  if (i >= n)
    return o;

  size = (sizeof(Scheme_App_Rec) 
	  + ((n - 1) * sizeof(Scheme_Object *))
	  + n * sizeof(char));
  app2 = (Scheme_App_Rec *)scheme_malloc_tagged(size);
  memcpy(app2, app, size);
  app2->args[i] = naya;

  for (i++; i < n; i++) {
    orig = app2->args[i];
    naya = scheme_jit_expr(orig);
    app2->args[i] = naya;
  }
  
  return (Scheme_Object *)app2;
}

static Scheme_Object *jit_application2(Scheme_Object *o)
{
  Scheme_App2_Rec *app;
  Scheme_Object *nrator, *nrand;

  app = (Scheme_App2_Rec *)o;

  nrator = scheme_jit_expr(app->rator);
  nrand = scheme_jit_expr(app->rand);
  
  if (SAME_OBJ(nrator, app->rator)
      && SAME_OBJ(nrand, app->rand))
    return o;

  app = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
  memcpy(app, o, sizeof(Scheme_App2_Rec));
  app->rator = nrator;
  app->rand = nrand;

  return (Scheme_Object *)app;
}

static Scheme_Object *jit_application3(Scheme_Object *o)
{
  Scheme_App3_Rec *app;
  Scheme_Object *nrator, *nrand1, *nrand2;

  app = (Scheme_App3_Rec *)o;

  nrator = scheme_jit_expr(app->rator);
  nrand1 = scheme_jit_expr(app->rand1);
  nrand2 = scheme_jit_expr(app->rand2);
  
  if (SAME_OBJ(nrator, app->rator)
      && SAME_OBJ(nrand1, app->rand1)
      && SAME_OBJ(nrand2, app->rand2))
    return o;

  app = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
  memcpy(app, o, sizeof(Scheme_App3_Rec));
  app->rator = nrator;
  app->rand1 = nrand1;
  app->rand2 = nrand2;

  return (Scheme_Object *)app;
}

static Scheme_Object *jit_sequence(Scheme_Object *o)
{
  Scheme_Object *orig, *naya = NULL;
  Scheme_Sequence *seq, *seq2;
  int i, n, size;

  seq = (Scheme_Sequence *)o;
  n = seq->count;

  for (i = 0; i < n; i++) {
    orig = seq->array[i];
    naya = scheme_jit_expr(orig);
    if (!SAME_OBJ(orig, naya))
      break;
  }

  if (i >= n)
    return o;

  size = (sizeof(Scheme_Sequence) 
	  + ((n - 1) * sizeof(Scheme_Object *)));
  seq2 = (Scheme_Sequence *)scheme_malloc_tagged(size);
  memcpy(seq2, seq, size);
  seq2->array[i] = naya;

  for (i++; i < n; i++) {
    orig = seq2->array[i];
    naya = scheme_jit_expr(orig);
    seq2->array[i] = naya;
  }
  
  return (Scheme_Object *)seq2;
}

static Scheme_Object *jit_branch(Scheme_Object *o)
{
  Scheme_Branch_Rec *b;
  Scheme_Object *t, *tb, *fb;

  b = (Scheme_Branch_Rec *)o;

  t = scheme_jit_expr(b->test);
  tb = scheme_jit_expr(b->tbranch);
  fb = scheme_jit_expr(b->fbranch);

  if (SAME_OBJ(t, b->test)
      && SAME_OBJ(tb, b->tbranch)
      && SAME_OBJ(fb, b->fbranch))
    return o;

  b = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
  memcpy(b, o, sizeof(Scheme_Branch_Rec));
  b->test = t;
  b->tbranch = tb;
  b->fbranch = fb;

  return (Scheme_Object *)b;
}

static Scheme_Object *jit_let_value(Scheme_Object *o)
{
  Scheme_Let_Value *lv = (Scheme_Let_Value *)o;
  Scheme_Object *body, *rhs;

  rhs = scheme_jit_expr(lv->value);
  body = scheme_jit_expr(lv->body);

  if (SAME_OBJ(rhs, lv->value)
      && SAME_OBJ(body, lv->body))
    return o;

  lv = MALLOC_ONE_TAGGED(Scheme_Let_Value);
  memcpy(lv, o, sizeof(Scheme_Let_Value));
  lv->value = rhs;
  lv->body = body;

  return (Scheme_Object *)lv;
}

static Scheme_Object *jit_let_one(Scheme_Object *o)
{
  Scheme_Let_One *lo = (Scheme_Let_One *)o;
  Scheme_Object *body, *rhs;

  rhs = scheme_jit_expr(lo->value);
  body = scheme_jit_expr(lo->body);

  if (SAME_OBJ(rhs, lo->value)
      && SAME_OBJ(body, lo->body))
    return o;

  lo = MALLOC_ONE_TAGGED(Scheme_Let_One);
  memcpy(lo, o, sizeof(Scheme_Let_One));
  lo->value = rhs;
  lo->body = body;

  return (Scheme_Object *)lo;
}

static Scheme_Object *jit_let_void(Scheme_Object *o)
{
  Scheme_Let_Void *lv = (Scheme_Let_Void *)o;
  Scheme_Object *body;

  body = scheme_jit_expr(lv->body);

  if (SAME_OBJ(body, lv->body))
    return o;

  lv = MALLOC_ONE_TAGGED(Scheme_Let_Void);
  memcpy(lv, o, sizeof(Scheme_Let_Void));
  lv->body = body;

  return (Scheme_Object *)lv;
}

static Scheme_Object *jit_letrec(Scheme_Object *o)
{
  Scheme_Letrec *lr = (Scheme_Letrec *)o, *lr2;
  Scheme_Object **procs, **procs2, *v;
  int i, count;

  count = lr->count;

  lr2 = MALLOC_ONE_TAGGED(Scheme_Letrec);
  memcpy(lr2, lr, sizeof(Scheme_Letrec));
  
  procs = lr->procs;
  procs2 = MALLOC_N(Scheme_Object *, count);
  lr2->procs = procs2;

  for (i = 0; i < count; i++) {
    v = scheme_jit_closure(procs[i], (Scheme_Object *)lr2);
    procs2[i] = v;
  }

  v = scheme_jit_expr(lr->body);
  lr2->body = v;

  return (Scheme_Object *)lr2;
}

static Scheme_Object *jit_wcm(Scheme_Object *o)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)o;
  Scheme_Object *k, *v, *b;

  k = scheme_jit_expr(wcm->key);
  v = scheme_jit_expr(wcm->val);
  b = scheme_jit_expr(wcm->body);
  if (SAME_OBJ(wcm->key, k)
      && SAME_OBJ(wcm->val, v)
      && SAME_OBJ(wcm->body, b))
    return o;

  wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  memcpy(wcm, o, sizeof(Scheme_With_Continuation_Mark));

  wcm->key = k;
  wcm->val = v;
  wcm->body = b;

  return (Scheme_Object *)wcm;
}

Scheme_Object *scheme_jit_expr(Scheme_Object *expr)
{
  Scheme_Type type = SCHEME_TYPE(expr);

  switch (type) {
  case scheme_syntax_type:
    {
      Scheme_Syntax_Jitter f;
      Scheme_Object *orig, *naya;
	  
      f = scheme_syntax_jitters[SCHEME_PINT_VAL(expr)];
      orig = SCHEME_IPTR_VAL(expr);
      naya = f(orig);
      if (SAME_OBJ(orig, naya))
	return expr;
      
      return scheme_make_syntax_resolved(SCHEME_PINT_VAL(expr), naya);
    }
  case scheme_application_type:
    return jit_application(expr);
  case scheme_application2_type:
    return jit_application2(expr);
  case scheme_application3_type:
    return jit_application3(expr);
  case scheme_sequence_type:
    return jit_sequence(expr);
  case scheme_branch_type:
    return jit_branch(expr);
  case scheme_with_cont_mark_type:
    return jit_wcm(expr);
  case scheme_unclosed_procedure_type:
    return scheme_jit_closure(expr, NULL);
  case scheme_let_value_type:
    return jit_let_value(expr);
  case scheme_let_void_type:
    return jit_let_void(expr);
  case scheme_letrec_type:
    return jit_letrec(expr);
  case scheme_let_one_type:
    return jit_let_one(expr);
  case scheme_closure_type:
    {
      Scheme_Closure *c = (Scheme_Closure *)expr;
      if (ZERO_SIZED_CLOSUREP(c)) {
	/* JIT the closure body, producing a native closure: */
	return scheme_jit_closure((Scheme_Object *)c->code, NULL);
      } else
	return expr;
    }
  case scheme_case_closure_type:
    {
      return scheme_unclose_case_lambda(expr, 1);
    }
  default:
    return expr;
  }
}

#else

Scheme_Object *scheme_jit_expr(Scheme_Object *expr)
{
  return expr;
}

#endif

/*========================================================================*/
/*                       compilation info management                      */
/*========================================================================*/

void scheme_default_compile_rec(Scheme_Compile_Info *rec, int drec)
{
}

void scheme_init_compile_recs(Scheme_Compile_Info *src, int drec, 
			      Scheme_Compile_Info *dest, int n)
{
  int i;

  for (i = 0; i < n; i++) {
#ifdef MZTAG_REQUIRED
    dest[i].type = scheme_rt_compile_info;
#endif
    dest[i].comp = 1;
    dest[i].dont_mark_local_use = src[drec].dont_mark_local_use;
    dest[i].resolve_module_ids = src[drec].resolve_module_ids;
    dest[i].value_name = scheme_false;
    dest[i].certs = src[drec].certs;
    /* should be always NULL */
    dest[i].observer = src[drec].observer;
  }
}

void scheme_init_expand_recs(Scheme_Expand_Info *src, int drec, 
			     Scheme_Expand_Info *dest, int n)
{
  int i;

  for (i = 0; i < n; i++) {
#ifdef MZTAG_REQUIRED
    dest[i].type = scheme_rt_compile_info;
#endif
    dest[i].comp = 0;
    dest[i].depth = src[drec].depth;
    dest[i].value_name = scheme_false;
    dest[i].certs = src[drec].certs;
    dest[i].observer = src[drec].observer;
  }
}

void scheme_merge_compile_recs(Scheme_Compile_Info *src, int drec, 
			       Scheme_Compile_Info *dest, int n)
{
  /* Nothing to do anymore, since we moved max_let_depth to resolve phase */
}

void scheme_init_lambda_rec(Scheme_Compile_Info *src, int drec,
			    Scheme_Compile_Info *lam, int dlrec)
{
#ifdef MZTAG_REQUIRED
  lam[dlrec].type = scheme_rt_compile_info;
#endif
  lam[dlrec].comp = 1;
  lam[dlrec].dont_mark_local_use = src[drec].dont_mark_local_use;
  lam[dlrec].resolve_module_ids = src[drec].resolve_module_ids;
  lam[dlrec].value_name = scheme_false;
  lam[dlrec].certs = src[drec].certs;
  lam[dlrec].observer = src[drec].observer;
}

void scheme_merge_lambda_rec(Scheme_Compile_Info *src, int drec,
			     Scheme_Compile_Info *lam, int dlrec)
{
}

void scheme_compile_rec_done_local(Scheme_Compile_Info *rec, int drec)
{
  rec[drec].value_name = scheme_false;
}

void scheme_rec_add_certs(Scheme_Compile_Expand_Info *src, int drec, Scheme_Object *stx)
{
  Scheme_Object *certs;
  certs = scheme_stx_extract_certs(stx, src[drec].certs);
  src[drec].certs = certs;
}

/*========================================================================*/
/*                         compilation dispatcher                         */
/*========================================================================*/

static Scheme_Object *
scheme_inner_compile_list(Scheme_Object *form, Scheme_Comp_Env *env, 
			  Scheme_Compile_Info *rec, int drec, int start_app_position)
{
  int len;

  len = scheme_stx_proper_list_length(form);

  if (!len) {
    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);
    return scheme_null;
  } else if (len > 0) {
    Scheme_Compile_Info *recs, quick[5];
    int i;
    Scheme_Object *c, *p, *comp_first, *comp_last, *name, *first, *rest;

    name = rec[drec].value_name;
    scheme_compile_rec_done_local(rec, drec);

    if (len <= 5)
      recs = quick;
    else
      recs = MALLOC_N_RT(Scheme_Compile_Info, len);
    scheme_init_compile_recs(rec, drec, recs, len);
    recs[len - 1].value_name = name;

    comp_first = comp_last = NULL;

    for (i = 0, rest = form; i < len; i++) {
      first = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);

      c = scheme_compile_expand_expr(first, env, recs, i,
				     !i && start_app_position);

      p = scheme_make_immutable_pair(c, scheme_null);
      if (comp_last)
	SCHEME_CDR(comp_last) = p;
      else
	comp_first = p;
      comp_last = p;
    }

    scheme_merge_compile_recs(rec, drec, recs, len);

    return comp_first;
  } else {
    scheme_signal_error("internal error: compile-list on non-list");
    return NULL;
  }
}

static Scheme_Object *compile_application(Scheme_Object *form, Scheme_Comp_Env *env,
					  Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *result;
  int len;

  len = scheme_stx_proper_list_length(form);

  if (len < 0)
    scheme_wrong_syntax(scheme_application_stx_string, NULL, form, NULL);
  
  scheme_compile_rec_done_local(rec, drec);
  scheme_rec_add_certs(rec, drec, form);
  form = scheme_inner_compile_list(form, scheme_no_defines(env), rec, drec, 1);

  result = make_application(form);
  
  return result;
}

Scheme_Object *
scheme_compile_list(Scheme_Object *form, Scheme_Comp_Env *env, 
		    Scheme_Compile_Info *rec, int drec)
{
  return scheme_inner_compile_list(form, env, rec, drec, 0);
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
    scheme_wrong_type("compile-handler", "compiled code", 0, -1, argv);
    return NULL;
  }

  return o;
}

static Scheme_Object *add_renames_unless_module(Scheme_Object *form, Scheme_Env *genv)
{
  if (genv->rename) {
    if (SCHEME_STX_PAIRP(form)) {
      Scheme_Object *a, *d;
      
      a = SCHEME_STX_CAR(form);
      if (SCHEME_STX_SYMBOLP(a)) {
	a = scheme_add_rename(a, genv->rename);
	if (scheme_stx_module_eq(a, scheme_module_stx, 0)) {
	  /* Don't add renames to the whole module; let the 
	     module's language take over. */
	  d = SCHEME_STX_CDR(form);
	  a = scheme_make_immutable_pair(a, d);
	  form = scheme_datum_to_syntax(a, form, form, 1, 0);
	  return form;
	}
      }
    }
  }

  if (genv->rename)
    form = scheme_add_rename(form, genv->rename);
  if (genv->exp_env && genv->exp_env->rename)
    form = scheme_add_rename(form, genv->exp_env->rename);
  if (genv->template_env && genv->template_env->rename)
    form = scheme_add_rename(form, genv->template_env->rename);
  if (genv->dt_rename)
    form = scheme_add_rename(form, genv->dt_rename);

  return form;
}

void scheme_enable_expression_resolve_lifts(Resolve_Info *ri)
{
  Scheme_Object *lift_vec;

  lift_vec = scheme_make_vector(2, NULL);
  SCHEME_VEC_ELS(lift_vec)[0] = scheme_null;
  SCHEME_VEC_ELS(lift_vec)[1] = scheme_make_integer(0);
  ri->lifts = lift_vec;
}

Scheme_Object *scheme_merge_expression_resolve_lifts(Scheme_Object *expr, Resolve_Prefix *rp, Resolve_Info *ri)
{
  Scheme_Object *lift_vec, *lifts;
  Scheme_Sequence *s;
  int n, i;

  lift_vec = ri->lifts;
  n = SCHEME_INT_VAL(SCHEME_VEC_ELS(lift_vec)[1]);
  if (n) {
    rp->num_lifts = n;
    lifts = SCHEME_VEC_ELS(lift_vec)[0];

    s = malloc_sequence(n + 1);
    s->so.type = scheme_sequence_type;
    s->count = n + 1;
    for (i = 0; i < n; i++, lifts = SCHEME_CDR(lifts)) {
      s->array[i] = SCHEME_CAR(lifts);
    }
    s->array[i] = expr;

    return (Scheme_Object *)s;
  } else
    return expr;
}

static void *compile_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *form;
  int writeable, for_eval, rename, enforce_consts;
  Scheme_Env *genv;
  Scheme_Compile_Info rec, rec2;
  Scheme_Object *o, *tl_queue;
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
      form = scheme_stx_phase_shift(form, 0, 
				    genv->module->me->src_modidx, 
				    genv->module->self_modidx,
				    genv->export_registry);
    }
  }

  tl_queue = scheme_null;

  {
    Scheme_Config *config;
    config = scheme_current_config();
    insp = scheme_get_param(config, MZCONFIG_CODE_INSPECTOR);
    enforce_consts = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_COMPILE_MODULE_CONSTS));
  }

  while (1) {
    rec.comp = 1;
    rec.dont_mark_local_use = 0;
    rec.resolve_module_ids = !writeable && !genv->module;
    rec.value_name = scheme_false;
    rec.certs = NULL;
    rec.observer = NULL;

    cenv = scheme_new_comp_env(genv, insp, SCHEME_TOPLEVEL_FRAME);

    if (for_eval) {
      /* Need to look for top-level `begin', and if we
	 find one, break it up to eval first expression
	 before the rest. */
      while (1) {
	scheme_frame_captures_lifts(cenv, scheme_make_lifted_defn, scheme_sys_wraps(cenv), scheme_false, scheme_true);
	form = scheme_check_immediate_macro(form, 
					    cenv, &rec, 0,
					    0, &gval, NULL, NULL);
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
	  o = scheme_frame_get_lifts(cenv);
	  if (!SCHEME_NULLP(o)) {
	    tl_queue = scheme_make_pair(form, tl_queue);
	    tl_queue = scheme_append(o, tl_queue);
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

      while (1) {
	scheme_frame_captures_lifts(cenv, scheme_make_lifted_defn, scheme_sys_wraps(cenv), scheme_false, scheme_true);

	scheme_init_compile_recs(&rec, 0, &rec2, 1);

	o = scheme_compile_expr(form, cenv, &rec2, 0);

	/* If we had compiled an expression in a previous iteration,
	   combine it in a sequence: */
	if (prev_o) {
	  Scheme_Sequence *seq;
	  seq = malloc_sequence(2);
	  seq->so.type = scheme_sequence_type;
	  seq->count = 2;
	  seq->array[0] = o;
	  seq->array[1] = prev_o;
	  o = (Scheme_Object *)seq;
	}

	/* If any definitions were lifted in the process of compiling o,
	   we need to fold them in. */
	l = scheme_frame_get_lifts(cenv);
	if (!SCHEME_NULLP(l)) {
	  l = icons(scheme_datum_to_syntax(begin_symbol, scheme_false, scheme_sys_wraps(cenv), 0, 0),
		    l);
	  form = scheme_datum_to_syntax(l, scheme_false, scheme_false, 0, 0);
	  prev_o = o;
	} else 
	  break;
      }

      oi = scheme_optimize_info_create();
      oi->enforce_const = enforce_consts;
      o = scheme_optimize_expr(o, oi);

      rp = scheme_resolve_prefix(0, cenv->prefix, 1);
      ri = scheme_resolve_info_create(rp);
      ri->enforce_const = enforce_consts;
      scheme_enable_expression_resolve_lifts(ri);

      o = scheme_resolve_expr(o, ri);

      o = scheme_merge_expression_resolve_lifts(o, rp, ri);

      rp = scheme_remap_prefix(rp, ri);

      top = MALLOC_ONE_TAGGED(Scheme_Compilation_Top);
      top->so.type = scheme_compilation_top_type;
      top->max_let_depth = ri->max_let_depth;
      top->code = o;
      top->prefix = rp;

      if (0) { /* <- change to 1 to check compilation result */
        scheme_validate_code(NULL, top->code,
                             scheme_make_hash_table(SCHEME_hash_ptr),
                             top->max_let_depth,
                             top->prefix->num_toplevels,
                             top->prefix->num_stxes,
                             top->prefix->num_lifts);
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

Scheme_Object *scheme_check_immediate_macro(Scheme_Object *first, 
					    Scheme_Comp_Env *env, 
					    Scheme_Compile_Expand_Info *rec, int drec,
					    int internel_def_pos,
					    Scheme_Object **current_val,
					    Scheme_Comp_Env **_xenv,
					    Scheme_Object *ctx)
{
  Scheme_Object *name, *val, *certs;
  Scheme_Comp_Env *xenv = (_xenv ? *_xenv : NULL);
  Scheme_Expand_Info erec1;
  Scheme_Env *menv = NULL;
  int need_cert;

  SCHEME_EXPAND_OBSERVE_ENTER_CHECK(rec[drec].observer, first);

  while (1) {
    *current_val = NULL;

    if (SCHEME_STX_PAIRP(first)) {
      name = SCHEME_STX_CAR(first);
      need_cert = 1;
    } else {
      name = first;
      need_cert = 0;
    }

    if (!SCHEME_STX_SYMBOLP(name)) {
      SCHEME_EXPAND_OBSERVE_EXIT_CHECK(rec[drec].observer, first);
      return first;
    }

    while (1) {

      if (need_cert) {
        /* While resolving name, we need certs from `first' */
        scheme_init_expand_recs(rec, drec, &erec1, 1);
        scheme_rec_add_certs(&erec1, 0, first);
        certs = erec1.certs;
      } else
        certs = rec[drec].certs;

      val = scheme_lookup_binding(name, env, 
                                  SCHEME_NULL_FOR_UNBOUND
                                  + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
                                  + ((rec[drec].comp && rec[drec].dont_mark_local_use) 
                                     ? SCHEME_DONT_MARK_USE 
                                     : 0)
                                  + ((rec[drec].comp && rec[drec].resolve_module_ids)
                                     ? SCHEME_RESOLVE_MODIDS
                                     : 0),
                                  certs, env->in_modidx,
                                  &menv, NULL, NULL);
    
      if (SCHEME_STX_PAIRP(first))
        *current_val = val;

      if (!val) {
        SCHEME_EXPAND_OBSERVE_EXIT_CHECK(rec[drec].observer, first);
        return first;
      } else if (SAME_TYPE(SCHEME_TYPE(val), scheme_macro_type)) {
        if (SAME_TYPE(SCHEME_TYPE(SCHEME_PTR_VAL(val)), scheme_id_macro_type)) {
          /* It's a rename. Look up the target name and try again. */
          name = scheme_stx_cert(SCHEME_PTR_VAL(SCHEME_PTR_VAL(val)), scheme_false, menv, name, NULL, 1);
          menv = NULL;
          SCHEME_USE_FUEL(1);
        } else {
          /* It's a normal macro; expand once. Also, extend env to indicate
             an internal-define position, if necessary. */
          if (!xenv) {
            if (internel_def_pos) {
              xenv = scheme_new_compilation_frame(0, SCHEME_INTDEF_FRAME, env, NULL);
              if (ctx)
                xenv->intdef_name = ctx;
              if (_xenv)
                *_xenv = xenv;
            } else
              xenv = env;
          }
          {
            scheme_init_expand_recs(rec, drec, &erec1, 1);
            erec1.depth = 1;
            erec1.value_name = rec[drec].value_name;
            first = scheme_expand_expr(first, xenv, &erec1, 0);
          }
          break; /* break to outer loop */
        }
      } else {
        SCHEME_EXPAND_OBSERVE_EXIT_CHECK(rec[drec].observer, first);
        return first;
      }
    }
  }
}

static Scheme_Object *
compile_expand_macro_app(Scheme_Object *name, Scheme_Env *menv, Scheme_Object *macro,
			 Scheme_Object *form, Scheme_Comp_Env *env,
			 Scheme_Compile_Expand_Info *rec, int drec)
{
  Scheme_Object *xformer, *boundname;

  xformer = (Scheme_Object *)SCHEME_PTR_VAL(macro);

  if (SAME_TYPE(SCHEME_TYPE(xformer), scheme_set_macro_type)) {
    /* scheme_apply_macro unwraps it */
  } else {
    if (!scheme_check_proc_arity(NULL, 1, 0, -1, &xformer)) {
      scheme_wrong_syntax(NULL, NULL, form, "illegal use of syntax");
      return NULL;
    }
  }

  boundname = rec[drec].value_name;
  if (!boundname)
    boundname = scheme_false;

  return scheme_apply_macro(name, menv, xformer, form, env, boundname, rec, drec, 0);

  /* caller expects rec[drec] to be used to compile the result... */
}

static int same_effective_env(Scheme_Comp_Env *orig, Scheme_Comp_Env *e)
{
  while (1) {
    if (orig == e)
      return 1;
    if (e && e->flags & SCHEME_FOR_STOPS)
      e = e->next;
    else
      return 0;
  }
}

static Scheme_Object *compile_expand_expr_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *form = (Scheme_Object *)p->ku.k.p1;
  Scheme_Comp_Env *env = (Scheme_Comp_Env *)p->ku.k.p2;
  Scheme_Compile_Info *rec = (Scheme_Compile_Info *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return scheme_compile_expand_expr(form, 
				    env,
				    rec,
				    p->ku.k.i3,
				    p->ku.k.i2);
}

static Scheme_Object *
scheme_compile_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
			   Scheme_Compile_Expand_Info *rec, int drec, 
			   int app_position)
{
  Scheme_Object *name, *var, *stx, *normal, *can_recycle_stx = NULL;
  Scheme_Env *menv = NULL;
  GC_CAN_IGNORE char *not_allowed;
  int looking_for_top;

 top:

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Compile_Expand_Info *recx;

      recx = MALLOC_ONE_RT(Scheme_Compile_Expand_Info);
      memcpy(recx, rec + drec, sizeof(Scheme_Compile_Expand_Info));
#ifdef MZTAG_REQUIRED
      recx->type = scheme_rt_compile_info;
#endif

      p->ku.k.p1 = (void *)form;
      p->ku.k.p2 = (void *)env;
      p->ku.k.p3 = (void *)recx;
      p->ku.k.i3 = 0;
      p->ku.k.i2 = app_position;

      var = scheme_handle_stack_overflow(compile_expand_expr_k);

      memcpy(rec + drec, recx, sizeof(Scheme_Compile_Expand_Info));
      return var;
    }
  }
#endif

  DO_CHECK_FOR_BREAK(scheme_current_thread, ;);

#if 1
  if (!SCHEME_STXP(form))
    scheme_signal_error("not syntax");
#endif

  if (rec[drec].comp) {
    scheme_default_compile_rec(rec, drec);
  } else {
    SCHEME_EXPAND_OBSERVE_VISIT(rec[drec].observer,form);
  }

  if (SAME_TYPE(SCHEME_TYPE(SCHEME_STX_VAL(form)), scheme_expanded_syntax_type)) {
    var = SCHEME_STX_VAL(form);
    if (scheme_stx_has_empty_wraps(form)
        && same_effective_env(SCHEME_PTR2_VAL(var), env)) {
      /* FIXME: this needs EXPAND_OBSERVE callbacks. */
      var = scheme_stx_track(SCHEME_PTR1_VAL(var), form, form);
      form = scheme_stx_cert(var, scheme_false, NULL, form, NULL, 1);
      if (!rec[drec].comp && (rec[drec].depth != -1)) {
        /* Already fully expanded. */
        return form;
      }
    } else {
      scheme_wrong_syntax(NULL, NULL, SCHEME_PTR1_VAL(var), 
                          "expanded syntax not in its original context");
    }
  }

  looking_for_top = 0;

  if (SCHEME_STX_NULLP(form)) {
    stx = app_symbol;
    not_allowed = "function application";
    normal = app_expander;
  } else if (!SCHEME_STX_PAIRP(form)) {
    if (SCHEME_STX_SYMBOLP(form)) {
      Scheme_Object *find_name = form, *lexical_binding_id;
      int protected = 0;

      while (1) {
        lexical_binding_id = NULL;
	var = scheme_lookup_binding(find_name, env, 
				    SCHEME_NULL_FOR_UNBOUND
				    + SCHEME_ENV_CONSTANTS_OK
				    + (rec[drec].comp
				       ? SCHEME_ELIM_CONST 
				       : 0)
				    + (app_position 
				       ? SCHEME_APP_POS 
				       : 0)
				    + ((rec[drec].comp && rec[drec].dont_mark_local_use) ? 
				       SCHEME_DONT_MARK_USE 
				       : 0)
				    + ((rec[drec].comp && rec[drec].resolve_module_ids)
				       ? SCHEME_RESOLVE_MODIDS
				       : 0),
				    rec[drec].certs, env->in_modidx, 
				    &menv, &protected, &lexical_binding_id);
	
        SCHEME_EXPAND_OBSERVE_RESOLVE(rec[drec].observer,find_name);

	if (var && SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
	    && SAME_TYPE(SCHEME_TYPE(SCHEME_PTR_VAL(var)), scheme_id_macro_type)) {
	  /* It's a rename. Look up the target name and try again. */
	  Scheme_Object *new_name;
	  new_name = SCHEME_PTR_VAL(SCHEME_PTR_VAL(var));
	  if (!rec[drec].comp) {
	    new_name = scheme_stx_track(new_name, find_name, find_name);
	  }
	  new_name = scheme_stx_cert(new_name, scheme_false, menv, find_name, NULL, 1);
	  find_name = new_name;
	  SCHEME_USE_FUEL(1);
	  menv = NULL;
	  protected = 0;
	} else
	  break;
      }
      
      if (!var) {
	/* Top variable */
	stx = top_symbol;
	not_allowed = "reference to top-level identifier";
	normal = top_expander;
	form = find_name; /* in case it was re-mapped */
	looking_for_top = 1;
      } else {
	if (SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
	  if (var == stop_expander) {
            SCHEME_EXPAND_OBSERVE_ENTER_PRIM(rec[drec].observer,form);
            SCHEME_EXPAND_OBSERVE_PRIM_STOP(rec[drec].observer);
            SCHEME_EXPAND_OBSERVE_EXIT_PRIM(rec[drec].observer,form);
            SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer,form);
	    return form;
          } else {
	    scheme_wrong_syntax(NULL, NULL, form, "bad syntax");
	    return NULL;
	  }
	} else if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)) {
	  name = form;
	  goto macro;
	}
	
	if (rec[drec].comp) {
	  scheme_compile_rec_done_local(rec, drec);
	  if (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)
	      || SAME_TYPE(SCHEME_TYPE(var), scheme_module_variable_type))
	    return scheme_register_toplevel_in_prefix(var, env, rec, drec);
	  else
	    return var;
	} else {
          SCHEME_EXPAND_OBSERVE_VARIABLE(rec[drec].observer, form, find_name);
          if (lexical_binding_id) {
            find_name = lexical_binding_id;
          }
	  if (protected) {
	    /* Add a property to indicate that the name is protected */
	    find_name = scheme_stx_property(find_name, protected_symbol, scheme_true);
	  }
          SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer, find_name);
	  return find_name; /* which is usually == form */
	}
      }
    } else {
      /* A hack for handling lifted expressions. See compile_expand_lift_to_let. */
      if (SAME_TYPE(SCHEME_TYPE(SCHEME_STX_VAL(form)), scheme_already_comp_type)) {
	form = SCHEME_STX_VAL(form);
	return SCHEME_IPTR_VAL(form);
      }

      stx = datum_symbol;
      not_allowed = "literal data";
      normal = datum_expander;
    }
  } else {
    name = SCHEME_STX_CAR(form);
    if (SCHEME_STX_SYMBOLP(name)) {
      /* Check for macros: */
      Scheme_Object *find_name = name;
      Scheme_Expand_Info erec1;

      /* While resolving name, we need certs from `form' */
      scheme_init_expand_recs(rec, drec, &erec1, 1);
      scheme_rec_add_certs(&erec1, 0, form);

      while (1) {
	var = scheme_lookup_binding(find_name, env, 
				    SCHEME_APP_POS
				    + SCHEME_NULL_FOR_UNBOUND
				    + SCHEME_ENV_CONSTANTS_OK
				    + (rec[drec].comp
				       ? SCHEME_ELIM_CONST
				       : 0)
				    + ((rec[drec].comp && rec[drec].dont_mark_local_use)
				       ? SCHEME_DONT_MARK_USE 
				       : 0)
				    + ((rec[drec].comp && rec[drec].resolve_module_ids)
				       ? SCHEME_RESOLVE_MODIDS
				       : 0),
				    erec1.certs, env->in_modidx, 
				    &menv, NULL, NULL);

        SCHEME_EXPAND_OBSERVE_RESOLVE(rec[drec].observer, find_name);
	if (var && SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
	    && SAME_TYPE(SCHEME_TYPE(SCHEME_PTR_VAL(var)), scheme_id_macro_type)) {
	  /* It's a rename. Look up the target name and try again. */
	  Scheme_Object *new_name;
	  new_name = SCHEME_PTR_VAL(SCHEME_PTR_VAL(var));
	  if (!rec[drec].comp) {
	    new_name = scheme_stx_track(new_name, find_name, find_name);
	  }
	  new_name = scheme_stx_cert(new_name, scheme_false, menv, find_name, NULL, 1);
	  find_name = new_name;
	  SCHEME_USE_FUEL(1);
	  menv = NULL;
	} else
	  break;
      }
      
      if (!var) {
	/* apply to global variable: compile it normally */
      } else if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)
		 || SAME_TYPE(SCHEME_TYPE(var), scheme_local_unbox_type)) {
	/* apply to local variable: compile it normally */
      } else {
	if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)) {
	  goto macro;
	} else if (SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
	  if (rec[drec].comp) {
	    Scheme_Syntax *f;
	    f = (Scheme_Syntax *)SCHEME_SYNTAX(var);
	    return f(form, env, rec, drec);
	  } else {
	    Scheme_Syntax_Expander *f;
	    f = (Scheme_Syntax_Expander *)SCHEME_SYNTAX_EXP(var);
	    SCHEME_EXPAND_OBSERVE_ENTER_PRIM(rec[drec].observer, form);
	    form = f(form, env, rec, drec);
	    SCHEME_EXPAND_OBSERVE_EXIT_PRIM(rec[drec].observer, form);
	    SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer, form);
	    return form;
	  }
	}
	
	/* Else: unknown global - must be a function: compile as application */
      }

      if (!SAME_OBJ(name, find_name)) {
	/* the rator position was mapped */
	Scheme_Object *code;
	code = SCHEME_STX_CDR(form);
	code = scheme_make_immutable_pair(find_name, code);
	form = scheme_datum_to_syntax(code, form, form, 0, 0);
      }
    }

    stx = app_symbol;
    not_allowed = "function application";
    normal = app_expander;
  }

  /* Compile/expand as application, datum, or top: */
  if (quick_stx && rec[drec].comp) {
    ((Scheme_Stx *)quick_stx)->val = stx;
    ((Scheme_Stx *)quick_stx)->wraps = ((Scheme_Stx *)form)->wraps;
    ((Scheme_Stx *)quick_stx)->u.modinfo_cache = NULL;
    stx = quick_stx;
    quick_stx = NULL;
  } else
    stx = scheme_datum_to_syntax(stx, scheme_false, form, 0, 0);
  if (rec[drec].comp)
    can_recycle_stx = stx;

  {
    Scheme_Object *find_name = stx;

    while (1) {
      var = scheme_lookup_binding(find_name, env,
				  SCHEME_NULL_FOR_UNBOUND
				  + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
				  + SCHEME_DONT_MARK_USE,
				  rec[drec].certs, env->in_modidx, 
				  &menv, NULL, NULL);

      SCHEME_EXPAND_OBSERVE_RESOLVE(rec[drec].observer, find_name);

      if (var && SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
	  && SAME_TYPE(SCHEME_TYPE(SCHEME_PTR_VAL(var)), scheme_id_macro_type)) {
	/* It's a rename. Look up the target name and try again. */
	Scheme_Object *new_name;
	new_name = SCHEME_PTR_VAL(SCHEME_PTR_VAL(var));
	if (!rec[drec].comp) {
	  new_name = scheme_stx_track(new_name, find_name, find_name);
	}
	new_name = scheme_stx_cert(new_name, scheme_false, menv, find_name, NULL, 1);
	find_name = new_name;
	SCHEME_USE_FUEL(1);
	menv = NULL;
      } else
	break;
    }
  }

  if (!SAME_OBJ(var, normal)) {
    /* Someone might keep the stx: */
    can_recycle_stx = NULL;
  }

  if (!var && looking_for_top) {
    /* If form is a marked name, then force #%top binding.
       This is so temporaries can be used as defined ids. */
    Scheme_Object *nm;
    nm = scheme_tl_id_sym(env->genv, form, NULL, 0);
    if (!SAME_OBJ(nm, SCHEME_STX_VAL(form))) {
      stx = scheme_datum_to_syntax(top_symbol, scheme_false, scheme_sys_wraps(env), 0, 0);

      /* Should be either top_expander or stop_expander: */
      var = scheme_lookup_binding(stx, env,
				  SCHEME_NULL_FOR_UNBOUND
				  + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
				  + SCHEME_DONT_MARK_USE,
				  rec[drec].certs, env->in_modidx, 
				  &menv, NULL, NULL);
    }
  }

  if (var && (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
	      || SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type))) {
    if (SAME_OBJ(var, stop_expander)) {
      /* Return original: */
      SCHEME_EXPAND_OBSERVE_ENTER_PRIM(rec[drec].observer, form);
      SCHEME_EXPAND_OBSERVE_PRIM_STOP(rec[drec].observer);
      SCHEME_EXPAND_OBSERVE_EXIT_PRIM(rec[drec].observer, form);
      SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer, form);
      return form;
    } else if (rec[drec].comp && SAME_OBJ(var, normal)) {
      /* Skip creation of intermediate form */
      Scheme_Syntax *f;
      taking_shortcut = 1;
      f = (Scheme_Syntax *)SCHEME_SYNTAX(var);
      if (can_recycle_stx && !quick_stx)
        quick_stx = can_recycle_stx;
      return f(form, env, rec, drec);
    } else {
      form = scheme_datum_to_syntax(scheme_make_immutable_pair(stx, form), form, form, 0, 2);
      
      if (SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
	if (rec[drec].comp) {
	  Scheme_Syntax *f;
	  f = (Scheme_Syntax *)SCHEME_SYNTAX(var);
	  return f(form, env, rec, drec);
	} else {
	  Scheme_Syntax_Expander *f;
	  f = (Scheme_Syntax_Expander *)SCHEME_SYNTAX_EXP(var);
	  SCHEME_EXPAND_OBSERVE_ENTER_PRIM(rec[drec].observer, form);
	  form = f(form, env, rec, drec);
	  SCHEME_EXPAND_OBSERVE_EXIT_PRIM(rec[drec].observer, form);
	  SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer, form);
	  return form;
	}
      } else {
	name = stx;
	goto macro;
      }
    }
  } else {
    /* Not allowed this context! */
    scheme_wrong_syntax(scheme_compile_stx_string, NULL, form, 
			"bad syntax; %s is not allowed, "
			"because no %S syntax transformer is bound",
			not_allowed,
			SCHEME_STX_VAL(stx));
    return NULL;
  }

 macro:
  if (!rec[drec].comp && !rec[drec].depth) {
    SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer, form);
    return form; /* We've gone as deep as requested */
  }

  SCHEME_EXPAND_OBSERVE_ENTER_MACRO(rec[drec].observer, form);
  form = compile_expand_macro_app(name, menv, var, form, env, rec, drec);
  SCHEME_EXPAND_OBSERVE_EXIT_MACRO(rec[drec].observer, form);

  if (rec[drec].comp)
    goto top;
  else {
    if (rec[drec].depth > 0)
      --rec[drec].depth;
    if (rec[drec].depth)
      goto top;
    else {
      SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer, form);
      return form;
    }
  }
}

static int arg_count(Scheme_Object *lam, Scheme_Comp_Env *env)
{
  Scheme_Object *l, *id, *form = lam;
  int cnt = 0;
  DupCheckRecord r;
  
  lam = SCHEME_STX_CDR(lam);
  if (!SCHEME_STX_PAIRP(lam)) return -1;

  l = SCHEME_STX_CAR(lam);

  lam = SCHEME_STX_CDR(lam);
  if (!SCHEME_STX_PAIRP(lam)) return -1;

  while (SCHEME_STX_PAIRP(lam)) { lam = SCHEME_STX_CDR(lam); }
  if (!SCHEME_STX_NULLP(lam)) return -1;
  

  scheme_begin_dup_symbol_check(&r, env);

  while (SCHEME_STX_PAIRP(l)) {
    id = SCHEME_STX_CAR(l);
    scheme_check_identifier("lambda", id, NULL, env, form);
    scheme_dup_symbol_check(&r, NULL, id, "argument", form);
    l = SCHEME_STX_CDR(l);
    cnt++;
  }
  if (!SCHEME_STX_NULLP(l)) return -1;

  return cnt;
}

static Scheme_Object *
compile_expand_app(Scheme_Object *forms, Scheme_Comp_Env *env, 
		   Scheme_Compile_Expand_Info *rec, int drec)
{
  Scheme_Object *form, *naya;
  int tsc = taking_shortcut;

  taking_shortcut = 0;

  scheme_rec_add_certs(rec, drec, forms);
  if (tsc) {
    form = forms;
  } else {
    form = SCHEME_STX_CDR(forms);
    form = scheme_datum_to_syntax(form, forms, forms, 0, 0);
  }
  
  if (SCHEME_STX_NULLP(form)) {
    /* Compile/expand empty application to null list: */
    if (rec[drec].comp)
      return scheme_null;
    else
      return scheme_datum_to_syntax(cons(quote_symbol,
					 cons(form, scheme_null)),
				    form,
				    scheme_sys_wraps(env), 
				    0, 2);
  } else if (!SCHEME_STX_PAIRP(form)) {
     /* will end in error */
    if (rec[drec].comp)
      return compile_application(form, env, rec, drec);
    else {
      rec[drec].value_name = scheme_false;
      naya = scheme_expand_list(form, scheme_no_defines(env), rec, drec);
      /* naya will be prefixed and returned... */
    }
  } else if (rec[drec].comp) {
    Scheme_Object *name, *origname, *gval, *orig_rest_form, *rest_form;
    name = SCHEME_STX_CAR(form);
    origname = name;
    
    name = scheme_check_immediate_macro(name, env, rec, drec, 0, &gval, NULL, NULL);
    
    /* look for ((lambda (x) ...) ...); */
    if (SAME_OBJ(gval, scheme_lambda_syntax)) {
      Scheme_Object *argsnbody;
	
      argsnbody = SCHEME_STX_CDR(name);
      if (SCHEME_STX_PAIRP(argsnbody)) {
        Scheme_Object *args, *body;

        args = SCHEME_STX_CAR(argsnbody);
        body = SCHEME_STX_CDR(argsnbody);
	  
        if (SCHEME_STX_PAIRP(body)) {
          int pl;
          pl = scheme_stx_proper_list_length(args);
          if (pl >= 0) {
            Scheme_Object *bindings = scheme_null, *last = NULL;
            Scheme_Object *rest;
            int al;
	      
            rest = SCHEME_STX_CDR(form);
            al = scheme_stx_proper_list_length(rest);

            if (al == pl) {	      
              DupCheckRecord r;

              scheme_begin_dup_symbol_check(&r, env);
	      
              while (!SCHEME_STX_NULLP(args)) {
                Scheme_Object *v, *n;
		  
                n = SCHEME_STX_CAR(args);
                scheme_check_identifier("lambda", n, NULL, env, name);

                /* If we don't check here, the error is in terms of `let': */
                scheme_dup_symbol_check(&r, NULL, n, "argument", name);
  
                v = SCHEME_STX_CAR(rest);
                v = cons(cons(cons(n, scheme_null), cons(v, scheme_null)), scheme_null);
                if (last)
                  SCHEME_CDR(last) = v;
                else
                  bindings = v;
		  
                last = v;
                args = SCHEME_STX_CDR(args);
                rest = SCHEME_STX_CDR(rest);
              }

              body = scheme_datum_to_syntax(cons(let_values_symbol,
                                                 cons(bindings,
                                                      body)),
                                            form, 
                                            scheme_sys_wraps(env), 
                                            0, 2);

              /* Copy certifications from lambda to `body'. */
              body = scheme_stx_cert(body, NULL, NULL, name, NULL, 1);
		
              return scheme_compile_expand_expr(body, env, rec, drec, 0);
            } else {
#if 0
              scheme_wrong_syntax(scheme_application_stx_string, NULL, form, 
                                  "procedure application: bad ((lambda (...) ...) ...) syntax");
              return NULL;
#endif
            }
          }
        }
      }
    }

    orig_rest_form = SCHEME_STX_CDR(form);

    /* Look for (call-with-values (lambda () M) (lambda (id ...) N)) */ 
    if (SCHEME_STX_SYMBOLP(name)) {
      Scheme_Object *at_first, *at_second, *the_end, *cwv_stx;
      at_first = SCHEME_STX_CDR(form);
      if (SCHEME_STX_PAIRP(at_first)) {
        at_second = SCHEME_STX_CDR(at_first);
        if (SCHEME_STX_PAIRP(at_second)) {
          the_end = SCHEME_STX_CDR(at_second);
          if (SCHEME_STX_NULLP(the_end)) {
            Scheme_Object *orig_at_second = at_second;

            cwv_stx = scheme_datum_to_syntax(scheme_intern_symbol("call-with-values"), 
                                             scheme_false, scheme_sys_wraps(env), 0, 0);
            if (scheme_stx_module_eq(name, cwv_stx, 0)) {
              Scheme_Object *first, *orig_first;
              orig_first = SCHEME_STX_CAR(at_first);
              first = scheme_check_immediate_macro(orig_first, env, rec, drec, 0, &gval, NULL, NULL);
              if (SAME_OBJ(gval, scheme_lambda_syntax) 
                  && SCHEME_STX_PAIRP(first)
                  && (arg_count(first, env) == 0)) {
                Scheme_Object *second, *orig_second;
                orig_second = SCHEME_STX_CAR(at_second);
                second = scheme_check_immediate_macro(orig_second, env, rec, drec, 0, &gval, NULL, NULL);
                if (SAME_OBJ(gval, scheme_lambda_syntax) 
                    && SCHEME_STX_PAIRP(second)
                    && (arg_count(second, env) >= 0)) {
                  Scheme_Object *lhs;
                  second = SCHEME_STX_CDR(second);
                  lhs = SCHEME_STX_CAR(second);
                  second = SCHEME_STX_CDR(second);
                  first = SCHEME_STX_CDR(first);
                  first = SCHEME_STX_CDR(first);
                  /* Convert to let-values: */
                  name = icons(let_values_symbol,
                               icons(icons(icons(lhs, icons(icons(begin_symbol, first), 
                                                            scheme_null)), 
                                           scheme_null),
                                     second));
                  form = scheme_datum_to_syntax(name, forms, scheme_sys_wraps(env), 0, 2);
                  return scheme_compile_expand_expr(form, env, rec, drec, 0);
                }
                if (!SAME_OBJ(second, orig_second)) {
                  at_second = scheme_datum_to_syntax(icons(second, the_end), at_second, at_second, 0, 2);
                } 
              }
              if (!SAME_OBJ(first, orig_first)
                  || !SAME_OBJ(at_second, orig_at_second)) {
                at_first = scheme_datum_to_syntax(icons(first, at_second), at_first, at_first, 0, 2);
              }
            }
          }
        }
      }
      rest_form = at_first;
    } else {
      rest_form = orig_rest_form;
    }

    if (NOT_SAME_OBJ(name, origname)
        || NOT_SAME_OBJ(rest_form, orig_rest_form)) {
      form = scheme_datum_to_syntax(scheme_make_immutable_pair(name, rest_form), forms, forms, 0, 2);
    }
    
    return compile_application(form, env, rec, drec);
  } else {
    scheme_rec_add_certs(rec, drec, form);
    rec[drec].value_name = scheme_false;
    naya = scheme_expand_list(form, scheme_no_defines(env), rec, drec);
    /* naya will be prefixed returned... */
  }

  if (SAME_OBJ(form, naya))
    return forms;

  /* Add #%app prefix back: */
  {
    Scheme_Object *first;

    first = SCHEME_STX_CAR(forms);
    return scheme_datum_to_syntax(scheme_make_immutable_pair(first, naya),
				  forms,
				  forms, 0, 2);
  }
}

static Scheme_Object *
app_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return compile_expand_app(form, env, rec, drec);
}

static Scheme_Object *
app_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_APP(erec[drec].observer);
  return compile_expand_app(form, env, erec, drec);
}

static Scheme_Object *
datum_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *c;

  if (taking_shortcut) {
    c = form;
    taking_shortcut = 0;
  } else {
    c = SCHEME_STX_CDR(form);
    /* Need datum->syntax, in case c is a list: */
    c = scheme_datum_to_syntax(c, form, form, 0, 2);
  }

  return scheme_syntax_to_datum(c, 0, NULL);
}

static Scheme_Object *
datum_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_DATUM(erec[drec].observer);
  return form;
}

static Scheme_Object *check_top(const char *when, Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *c;

  if (taking_shortcut) {
    c = form;
    taking_shortcut = 0;
  } else
    c = SCHEME_STX_CDR(form);

  if (!SCHEME_STX_SYMBOLP(c))
    scheme_wrong_syntax(NULL, NULL, form, NULL);

  if (env->genv->module) {
    Scheme_Object *modidx, *symbol = c, *tl_id;
    int bad;

    tl_id = scheme_tl_id_sym(env->genv, symbol, NULL, 0);
    if (NOT_SAME_OBJ(tl_id, SCHEME_STX_SYM(symbol))) {
      /* Since the module has a rename for this id, it's certainly defined. */
    } else {
      modidx = scheme_stx_module_name(&symbol, env->genv->phase, NULL, NULL, NULL);
      if (modidx) {
	/* If it's an access path, resolve it: */
	if (env->genv->module
	    && SAME_OBJ(scheme_module_resolve(modidx, 1), env->genv->module->modname))
	  bad = 0;
	else
	  bad = 1;
      } else
	bad = 1;

      if (!env->genv->rename) {
	if (bad || !scheme_lookup_in_table(env->genv->toplevel, (const char *)SCHEME_STX_SYM(c))) {
	  scheme_wrong_syntax(when, NULL, c, 
			      (env->genv->phase
			       ? "unbound variable in module (transformer environment)"
			       : "unbound variable in module"));
	}
      }
    }
  }

  return c;
}

static Scheme_Object *
top_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *c;

  c = check_top(scheme_compile_stx_string, form, env);

  c = scheme_tl_id_sym(env->genv, c, NULL, 0);

  if (env->genv->module && !rec[drec].resolve_module_ids) {
    /* Self-reference in a module; need to remember the modidx.  Don't
       need a pos, because the symbol's gensym-ness (if any) will be
       preserved within the module. */
    c = scheme_hash_module_variable(env->genv, env->genv->module->self_modidx, 
				    c, env->genv->module->insp,
				    -1, env->genv->mod_phase);
  } else
    c = (Scheme_Object *)scheme_global_bucket(c, env->genv);

  return scheme_register_toplevel_in_prefix(c, env, rec, drec);
}

static Scheme_Object *
top_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_TOP(erec[drec].observer);
  check_top(scheme_expand_stx_string, form, env);
  return form;
}

Scheme_Object *scheme_compile_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
				   Scheme_Compile_Info *rec, int drec)
{
  return scheme_compile_expand_expr(form, env, rec, drec, 0);
}

Scheme_Object *scheme_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
				  Scheme_Expand_Info *erec, int drec)
{
  return scheme_compile_expand_expr(form, env, erec, drec, 0);
}

static Scheme_Object *pair_lifted(Scheme_Object *_ip, Scheme_Object **_id, Scheme_Object *expr, Scheme_Comp_Env *env)
{
  Scheme_Comp_Env **ip = (Scheme_Comp_Env **)_ip, *naya;

  naya = scheme_new_compilation_frame(1, SCHEME_CAPTURE_LIFTED, (*ip)->next, NULL);
  (*ip)->next = naya;
  *ip = naya;

  scheme_add_compilation_binding(0, *_id, naya);

  return icons(icons(*_id, scheme_null), icons(expr, scheme_null));
}

static Scheme_Object *compile_expand_expr_lift_to_let_k(void);

static Scheme_Object *
compile_expand_expr_lift_to_let(Scheme_Object *form, Scheme_Comp_Env *env,
				Scheme_Expand_Info *rec, int drec)
{
  Scheme_Expand_Info recs[2];
  Scheme_Object *l, *orig_form = form, *context_key;
  Scheme_Comp_Env *inserted, **ip;

  /* This function only works when `env' has no lexical bindings,
     because we might insert new ones at the beginning.  In
     particular, we might insert frames between `inserted' and
     `env'.

     This function also relies on the way that compilation of `let'
     works. A let-bound variable is compiled to a count of the frames
     to skip and the index within the frame, so we can insert new
     frames without affecting lookups computed so far. Inserting each
     new frame before any previous one turns out to be consistent with
     the nested `let's that we generate at the end. 

     Some optimizations can happen later, for example constant
     propagate.  But these optimizations take place on the result of
     this function, so we don't have to worry about them.  

     Don't generate a `let*' expression instead of nested `let's,
     because the compiler actually takes shortcuts (that are
     inconsistent with our frame nesting) instead of expanding `let*'
     to `let'. */

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Compile_Expand_Info *recx;

      recx = MALLOC_ONE_RT(Scheme_Compile_Expand_Info);
      memcpy(recx, rec + drec, sizeof(Scheme_Compile_Expand_Info));
#ifdef MZTAG_REQUIRED
      recx->type = scheme_rt_compile_info;
#endif

      p->ku.k.p1 = (void *)form;
      p->ku.k.p2 = (void *)env;
      p->ku.k.p3 = (void *)recx;

      form = scheme_handle_stack_overflow(compile_expand_expr_lift_to_let_k);

      memcpy(rec + drec, recx, sizeof(Scheme_Compile_Expand_Info));
      return form;
    }
  }
#endif

  inserted = scheme_new_compilation_frame(0, 0, env, NULL);

  ip = MALLOC_N(Scheme_Comp_Env *, 1);
  *ip = inserted;

  context_key = scheme_generate_lifts_key();
  
  scheme_frame_captures_lifts(inserted, pair_lifted, (Scheme_Object *)ip, scheme_false, context_key);

  if (rec[drec].comp) {
    scheme_init_compile_recs(rec, drec, recs, 2);
    form = scheme_compile_expr(form, inserted, recs, 0);
  } else {
    scheme_init_expand_recs(rec, drec, recs, 2);
    form = scheme_expand_expr(form, inserted, recs, 0);
  }

  l = scheme_frame_get_lifts(inserted);
  if (SCHEME_NULLP(l)) {
    /* No lifts */
    if (rec[drec].comp)
      scheme_merge_compile_recs(rec, drec, recs, 1);
    return form;
  } else {
    /* We have lifts, so add let* wrapper and go again */
    Scheme_Object *o, *revl;
    if (rec[drec].comp) {
      /* Wrap compiled part so the compiler recognizes it later: */
      o = scheme_alloc_object();
      o->type = scheme_already_comp_type;
      SCHEME_IPTR_VAL(o) = form;
    } else
      o = form;
    for (revl = scheme_null; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      revl = icons(SCHEME_CAR(l), revl);
    }
    for (; SCHEME_PAIRP(revl); revl = SCHEME_CDR(revl)) {
      o = icons(scheme_datum_to_syntax(let_values_symbol, scheme_false, scheme_sys_wraps(env), 0, 0),
		icons(icons(SCHEME_CAR(revl), scheme_null),
		      icons(o, scheme_null)));
    }
    form = scheme_datum_to_syntax(o, orig_form, scheme_false, 0, 0);
    SCHEME_EXPAND_OBSERVE_LETLIFT_LOOP(rec[drec].observer, form);
    form = compile_expand_expr_lift_to_let(form, env, recs, 1);
    if (rec[drec].comp)
      scheme_merge_compile_recs(rec, drec, recs, 2);
    return form;
  }
}

static Scheme_Object *compile_expand_expr_lift_to_let_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *form = (Scheme_Object *)p->ku.k.p1;
  Scheme_Comp_Env *env = (Scheme_Comp_Env *)p->ku.k.p2;
  Scheme_Compile_Info *rec = (Scheme_Compile_Info *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return compile_expand_expr_lift_to_let(form, env, rec, 0);
}

Scheme_Object *
scheme_compile_expr_lift_to_let(Scheme_Object *form, Scheme_Comp_Env *env,
				Scheme_Compile_Info *rec, int drec)
{
  return compile_expand_expr_lift_to_let(form, env, rec, drec);
}

Scheme_Object *
scheme_expand_expr_lift_to_let(Scheme_Object *form, Scheme_Comp_Env *env,
			       Scheme_Expand_Info *erec, int drec)
{
  return compile_expand_expr_lift_to_let(form, env, erec, drec);
}

static Scheme_Object *
scheme_compile_expand_block(Scheme_Object *forms, Scheme_Comp_Env *env, 
			    Scheme_Compile_Expand_Info *rec, int drec)
/* This ugly code parses a block of code, transforming embedded
   define-values and define-syntax into letrec and letrec-syntax.
   It is espcailly ugly because we have to expand macros
   before deciding what we have. */
{
  Scheme_Object *first, *rib, *ctx, *ectx;
  Scheme_Comp_Env *xenv = NULL;
  Scheme_Compile_Info recs[2];
  DupCheckRecord r;

  if (rec[drec].comp) {
    scheme_default_compile_rec(rec, drec);
  } else {
    SCHEME_EXPAND_OBSERVE_ENTER_BLOCK(rec[drec].observer, forms);
  }

  if (SCHEME_STX_NULLP(forms)) {
    if (rec[drec].comp) {
      scheme_compile_rec_done_local(rec, drec);
      return scheme_null;
    } else {
      SCHEME_EXPAND_OBSERVE_BLOCK_TO_LIST(rec[drec].observer, forms);
      SCHEME_EXPAND_OBSERVE_ENTER_LIST(rec[drec].observer, forms);
      SCHEME_EXPAND_OBSERVE_EXIT_LIST(rec[drec].observer, forms);
      return forms;
    }
  }

  rib = scheme_make_rename_rib();
  ctx = scheme_alloc_object();
  ctx->type = scheme_intdef_context_type;
  SCHEME_PTR1_VAL(ctx) = env;
  SCHEME_PTR2_VAL(ctx) = rib;
  ectx = scheme_make_pair(ctx, scheme_null);
  scheme_begin_dup_symbol_check(&r, env);

 try_again:

  SCHEME_EXPAND_OBSERVE_NEXT(rec[drec].observer);

  if (!SCHEME_STX_PAIRP(forms)) {
    scheme_wrong_syntax(scheme_begin_stx_string, NULL, forms, "bad syntax");
    return NULL;
  }

  first = SCHEME_STX_CAR(forms);

  {
    /* Need to send both parts (before & after) of block rename */
    Scheme_Object *old_first;

    old_first = first;
    first = scheme_add_rename_rib(first, rib);
    
    SCHEME_EXPAND_OBSERVE_BLOCK_RENAMES(rec[drec].observer,old_first,first);
  }

  {
    Scheme_Object *gval, *result;
    int more = 1;

    result = forms;

    /* Check for macro expansion, which could mask the real
       define-values, define-syntax, etc.: */
    first = scheme_check_immediate_macro(first, env, rec, drec, 1, &gval, &xenv, ectx);
    
    if (SAME_OBJ(gval, scheme_begin_syntax)) {
      /* Inline content */
      Scheme_Object *orig_forms = forms;

      SCHEME_EXPAND_OBSERVE_PRIM_BEGIN(rec[drec].observer);

      /* FIXME: Redundant with check done by scheme_flatten_begin below? */
      if (scheme_stx_proper_list_length(first) < 0)
	scheme_wrong_syntax(scheme_begin_stx_string, NULL, first, 
			    "bad syntax (" IMPROPER_LIST_FORM ")");

      forms = SCHEME_STX_CDR(forms);

      if (SCHEME_STX_NULLP(forms)) {
	/* A `begin' that ends the block.  An `inferred-name' property
	   attached to this begin should apply to the ultimate last
	   thing in the block. */
	Scheme_Object *v;
	v = scheme_check_name_property(first, rec[drec].value_name);
	rec[drec].value_name = v;
      }

      forms = scheme_flatten_begin(first, forms);

      SCHEME_EXPAND_OBSERVE_SPLICE(rec[drec].observer, forms);

      if (SCHEME_STX_NULLP(forms)) {
	scheme_wrong_syntax(scheme_begin_stx_string, NULL, first, 
			    "bad syntax (empty form)");
      }

      forms = scheme_datum_to_syntax(forms, orig_forms, orig_forms, 0, 0);

      goto try_again;
    } else if (SAME_OBJ(gval, scheme_define_values_syntax)
	       || SAME_OBJ(gval, scheme_define_syntaxes_syntax)) {
      /* Turn defines into a letrec: */
      Scheme_Object *var, *vars, *v, *link;
      Scheme_Object *l = scheme_null, *start = NULL;
      Scheme_Object *stx_l = scheme_null, *stx_start = NULL;
      int is_val;

      while (1) {
	int cnt;

	is_val = SAME_OBJ(gval, scheme_define_values_syntax);
	
	v = SCHEME_STX_CDR(first);

        if (is_val) {
          SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_VALUES(rec[drec].observer);
        } else {
          SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_SYNTAXES(rec[drec].observer);
        }
	
	if (!SCHEME_STX_PAIRP(v))
	  scheme_wrong_syntax(NULL, NULL, first, 
			      "bad syntax (" IMPROPER_LIST_FORM ")");

	var = NULL;
	vars = SCHEME_STX_CAR(v);
	cnt = 0;
	while (SCHEME_STX_PAIRP(vars)) {
	  var = SCHEME_STX_CAR(vars);
	  if (!SCHEME_STX_SYMBOLP(var))
	    scheme_wrong_syntax(NULL, var, first, 
				"name must be an identifier");
	  scheme_dup_symbol_check(&r, "internal definition", var, "binding", first);
	  vars = SCHEME_STX_CDR(vars);
	  cnt++;
	}
	if (!SCHEME_STX_NULLP(vars)) {
	  vars = SCHEME_STX_CAR(v);
	  scheme_wrong_syntax(NULL, vars, first, 
			      "not a sequence of identifiers");
	}

	/* Preserve properties and track at the clause level: */
	v = scheme_datum_to_syntax(v, first, first, 0, 0);
	var = SCHEME_STX_CAR(first);
	v = scheme_stx_track(v, first, var);

	link = scheme_make_immutable_pair(v, scheme_null);
	if (is_val) {
	  if (!start)
	    start = link;
	  else
	    SCHEME_CDR(l) = link;
	  l = link;
	} else {
	  if (!stx_start)
	    stx_start = link;
	  else
	    SCHEME_CDR(stx_l) = link;
	  stx_l = link;
	}

	result = SCHEME_STX_CDR(result);
	if (!SCHEME_STX_NULLP(result) && !SCHEME_STX_PAIRP(result))
	  scheme_wrong_syntax(NULL, NULL, first, NULL);

	{
	  /* Execute internal macro definition and register non-macros */
	  Scheme_Comp_Env *new_env;
	  Scheme_Object *names, *expr, *l, *a;
	  int pos;

	  new_env = scheme_new_compilation_frame(0, SCHEME_FOR_INTDEF, env, rec[drec].certs);

	  names = SCHEME_STX_CAR(v);
	  expr = SCHEME_STX_CDR(v);
	  if (!SCHEME_STX_PAIRP(expr)) {
	    if (SCHEME_STX_NULLP(expr))
	      scheme_wrong_syntax(NULL, NULL, first, 
				  "bad syntax (missing expression)");
	    else
	      scheme_wrong_syntax(NULL, NULL, first, 
				  "bad syntax (" IMPROPER_LIST_FORM ")");
	  }
	  link = SCHEME_STX_CDR(expr);
	  if (!SCHEME_STX_NULLP(link)) {
	    scheme_wrong_syntax(NULL, NULL, first, 
				"bad syntax (extra data after expression)");
	  }
	  expr = SCHEME_STX_CAR(expr);
	  
	  scheme_add_local_syntax(cnt, new_env);

	  /* Initialize environment slots to #f, which means "not syntax". */
	  cnt = 0;
	  for (l = names; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
	    a = SCHEME_STX_CAR(l);
	    scheme_set_local_syntax(cnt++, a, scheme_false, new_env);
	  }

	  if (!is_val) {
	    /* Evaluate and bind syntaxes */
	    scheme_prepare_exp_env(new_env->genv);
	    pos = 0;
	    expr = scheme_add_rename_rib(expr, rib);
	    scheme_bind_syntaxes("local syntax definition", 
				 names, expr,
				 new_env->genv->exp_env, new_env->insp, rec, drec,
				 new_env, new_env,
				 &pos);
	  }

	  /* Extend shared rib with renamings */
	  scheme_add_env_renames(rib, new_env, env);
	  
	  /* Remember extended environment */
	  SCHEME_PTR1_VAL(ctx) = new_env;
	  env = new_env;
	  xenv = NULL;
	}

      define_try_again:
	if (!SCHEME_STX_NULLP(result)) {
	  first = SCHEME_STX_CAR(result);
	  first = scheme_datum_to_syntax(first, forms, forms, 0, 0);
          {
            Scheme_Object *old_first;
            old_first = first;
            first = scheme_add_rename_rib(first, rib);
            SCHEME_EXPAND_OBSERVE_NEXT(rec[drec].observer);
            SCHEME_EXPAND_OBSERVE_BLOCK_RENAMES(rec[drec].observer,old_first,first);
          }
	  first = scheme_check_immediate_macro(first, env, rec, drec, 1, &gval, &xenv, ectx);
	  more = 1;
	  if (NOT_SAME_OBJ(gval, scheme_define_values_syntax)
	      && NOT_SAME_OBJ(gval, scheme_define_syntaxes_syntax)) {
	    if (SAME_OBJ(gval, scheme_begin_syntax)) {
	      /* Inline content */
	      result = SCHEME_STX_CDR(result);
              SCHEME_EXPAND_OBSERVE_PRIM_BEGIN(rec[drec].observer);
	      result = scheme_flatten_begin(first, result);
	      SCHEME_EXPAND_OBSERVE_SPLICE(rec[drec].observer,result);
              goto define_try_again;
	    } else {
	      /* Keep partially expanded `first': */
	      result = SCHEME_STX_CDR(result);
	      result = scheme_make_pair(first, result);
	      break;
	    }
	  }
	} else
	  break;
      }

      if (SCHEME_STX_PAIRP(result)) {
	if (!start)
	  start = scheme_null;
        
	/* I think the following was intended as an optimization for `expand',
           since the syntax definition will be dropped. But it breaks
           `local-expand':
           if (stx_start && !(rec[drec].comp || (rec[drec].depth == -1)))
             stx_start = scheme_null; */
	if (stx_start) {
	  result = scheme_make_immutable_pair(letrec_syntaxes_symbol,
					      scheme_make_immutable_pair(stx_start,
									 scheme_make_immutable_pair(start, result)));
	} else {
	  result = scheme_make_immutable_pair(letrec_values_symbol, scheme_make_immutable_pair(start, result));
	}
	result = scheme_datum_to_syntax(result, forms, scheme_sys_wraps(env), 0, 2);
	result = scheme_add_rename_rib(result, rib);

	more = 0;
      } else {
	/* Empty body: illegal. */
	scheme_wrong_syntax(scheme_begin_stx_string, NULL, forms, 
			    "no expression after a sequence of internal definitions");
      }
    }

    if (!more) {
      if (rec[drec].comp) {
	result = scheme_compile_expr(result, env, rec, drec);
        return scheme_make_immutable_pair(result, scheme_null);
      } else {
	if (rec[drec].depth > 0)
	  --rec[drec].depth;
	if (rec[drec].depth) {
          result = scheme_make_immutable_pair(result, scheme_null);
          SCHEME_EXPAND_OBSERVE_BLOCK_TO_LETREC(rec[drec].observer, result);
          return scheme_expand_list(result, env, rec, drec);
        }
      }
    }
  }

  if (rec[drec].comp) {
    Scheme_Object *vname, *rest;

    vname = rec[drec].value_name;
    scheme_compile_rec_done_local(rec, drec);
    scheme_init_compile_recs(rec, drec, recs, 2);

    rest = SCHEME_STX_CDR(forms);
    if (SCHEME_STX_NULLP(rest))
      recs[0].value_name = vname;
    else
      recs[1].value_name = vname;

    rest = scheme_datum_to_syntax(rest, forms, forms, 0, 0);

    first = scheme_compile_expr(first, env, recs, 0);
#if EMBEDDED_DEFINES_START_ANYWHERE
    forms = scheme_compile_expand_block(rest, env, recs, 1);
#else
    forms = scheme_compile_list(rest, env, recs, 1);
#endif
    
    scheme_merge_compile_recs(rec, drec, recs, 2);
    return scheme_make_immutable_pair(first, forms);
  } else {
#if EMBEDDED_DEFINES_START_ANYWHERE
    /* Expand-observe not implemented for this case,
       so fix that if it's ever enabled. */
    Scheme_Object *rest, *vname;

    vname = rec[drec].value_name;
    rec[drec].value_name = scheme_false;
    scheme_init_expand_recs(rec, drec, recs, 2);

    rest = SCHEME_STX_CDR(forms);

    if (SCHEME_STX_NULLP(rest))
      recs[0].value_name = vname;
    else
      recs[1].value_name = vname;

    first = scheme_expand_expr(first, env, recs, 0);

    rest = scheme_datum_to_syntax(rest, forms, forms, 0, -1);
    forms = scheme_compile_expand_block(rest, env, recs, 1);
    return scheme_make_immutable_pair(first, forms);
#else
    Scheme_Object *newforms, *vname;

    vname = rec[drec].value_name;
    rec[drec].value_name = scheme_false;
    scheme_init_expand_recs(rec, drec, recs, 2);

    recs[0].value_name = vname;

    newforms = SCHEME_STX_CDR(forms);
    newforms = scheme_make_immutable_pair(first, newforms);
    forms = scheme_datum_to_syntax(newforms, forms, forms, 0, -1);
    
    if (scheme_stx_proper_list_length(forms) < 0)
      scheme_wrong_syntax(scheme_begin_stx_string, NULL, forms, "bad syntax");
    
    SCHEME_EXPAND_OBSERVE_BLOCK_TO_LIST(rec[drec].observer, forms);
    forms = scheme_expand_list(forms, env, recs, 0);
    return forms;
#endif
  }
}

Scheme_Object *
scheme_compile_block(Scheme_Object *forms, Scheme_Comp_Env *env, 
		     Scheme_Compile_Info *rec, int drec)
{
  return scheme_compile_expand_block(forms, env, rec, drec);
}

Scheme_Object *
scheme_expand_block(Scheme_Object *forms, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return scheme_compile_expand_block(forms, env, erec, drec);
}

Scheme_Object *
scheme_expand_list(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *first = NULL, *last = NULL, *fm;

  SCHEME_EXPAND_OBSERVE_ENTER_LIST(erec[drec].observer, form);

  if (SCHEME_STX_NULLP(form)) {
    SCHEME_EXPAND_OBSERVE_EXIT_LIST(erec[drec].observer, form);
    return scheme_null;
  }

  if (scheme_stx_proper_list_length(form) < 0) {
    /* This is already checked for anything but application */
    scheme_wrong_syntax(scheme_application_stx_string, NULL, form, 
			"bad syntax (" IMPROPER_LIST_FORM ")");
  }

  fm = form;
  while (SCHEME_STX_PAIRP(fm)) {
    Scheme_Object *r, *p;
    Scheme_Expand_Info erec1;

    SCHEME_EXPAND_OBSERVE_NEXT(erec[drec].observer);

    p = SCHEME_STX_CDR(fm);
    
    scheme_init_expand_recs(erec, drec, &erec1, 1);
    erec1.value_name = (SCHEME_STX_NULLP(p) ? erec[drec].value_name : scheme_false);

    r = SCHEME_STX_CAR(fm);
    r = scheme_expand_expr(r, env, &erec1, 0);
    p = scheme_make_immutable_pair(r, scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;

    fm = SCHEME_STX_CDR(fm);
  }

  form = scheme_datum_to_syntax(first, form, form, 0, 0);
  SCHEME_EXPAND_OBSERVE_EXIT_LIST(erec[drec].observer, form);
  return form;
}


Scheme_Object *
scheme_flatten_begin(Scheme_Object *expr, Scheme_Object *append_onto)
{
  Scheme_Object *l, *ll, *a, *name, *body;
  
  if (scheme_stx_proper_list_length(expr) < 0)
    scheme_wrong_syntax(NULL, NULL, expr, "bad syntax (" IMPROPER_LIST_FORM ")");

  name = SCHEME_STX_CAR(expr);
  body = SCHEME_STX_CDR(expr);

  /* Extract body of `begin' and add tracking information */
  l = scheme_copy_list(scheme_flatten_syntax_list(body, NULL));
  for (ll = l; !SCHEME_NULLP(ll); ll = SCHEME_CDR(ll)) {
    a = SCHEME_CAR(ll);
    a = scheme_stx_track(a, expr, name);
    SCHEME_CAR(ll) = a;
  }
  
  return scheme_append(l, append_onto);
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

static MZ_MARK_STACK_TYPE clone_meta_cont_set_mark(Scheme_Meta_Continuation *mc, Scheme_Object *val, long findpos)
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

static MZ_MARK_STACK_TYPE new_segment_set_mark(long segpos, long pos, Scheme_Object *key, Scheme_Object *val)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Cont_Mark *cm = NULL;
  int c = p->cont_mark_seg_count;
  Scheme_Cont_Mark **segs, *seg;
  long findpos;
  
  /* Note: we perform allocations before changing p to avoid GC trouble,
     since MzScheme adjusts a thread's cont_mark_stack_segments on GC. */
  segs = MALLOC_N(Scheme_Cont_Mark *, c + 1);
  seg = scheme_malloc_allow_interior(sizeof(Scheme_Cont_Mark) * SCHEME_MARK_SEGMENT_SIZE);
  segs[c] = seg;
  
  memcpy(segs, p->cont_mark_stack_segments, c * sizeof(Scheme_Cont_Mark *));
  
  p->cont_mark_seg_count++;
  p->cont_mark_stack_segments = segs;

  seg = p->cont_mark_stack_segments[segpos];
  cm = seg + pos;
  findpos = MZ_CONT_MARK_STACK;
  MZ_CONT_MARK_STACK++;

  cm->key = key;
  cm->val = val;
  cm->pos = MZ_CONT_MARK_POS; /* always odd */
  cm->cache = NULL;

  return findpos;
}


MZ_MARK_STACK_TYPE scheme_set_cont_mark(Scheme_Object *key, Scheme_Object *val)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Cont_Mark *cm = NULL;
  long findpos, bottom;

  findpos = (long)MZ_CONT_MARK_STACK;
  bottom = (long)p->cont_mark_stack_bottom;
  while (1) {
    if (findpos-- > bottom) {
      Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      long pos = findpos & SCHEME_MARK_SEGMENT_MASK;
      Scheme_Cont_Mark *find = seg + pos;

      if ((long)find->pos < (long)MZ_CONT_MARK_POS) {
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
            for (findpos = (long)mc->cont_mark_total; findpos--; ) {
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
    long segpos;
    long pos;
    Scheme_Cont_Mark *seg;

    findpos = MZ_CONT_MARK_STACK;
    segpos = ((long)findpos) >> SCHEME_LOG_MARK_SEGMENT_SIZE;
    pos = ((long)findpos) & SCHEME_MARK_SEGMENT_MASK;

    if (segpos >= p->cont_mark_seg_count) {
      /* Need a new segment */
      return new_segment_set_mark(segpos, pos, key, val);
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

static void unbound_global(Scheme_Object *obj)
{
  Scheme_Object *tmp;

  tmp = MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(obj)];
  tmp = ((Scheme_Object **)tmp)[SCHEME_TOPLEVEL_POS(obj)];
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

  prompt = (Scheme_Prompt *)scheme_extract_one_cc_mark_with_meta(NULL, 
                                                                 SCHEME_PTR_VAL(c->prompt_tag),
                                                                 NULL,
                                                                 _prompt_mc,
                                                                 _prompt_pos);
  if (!prompt && !SAME_OBJ(scheme_default_prompt_tag, c->prompt_tag)) {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_CONTINUATION,
                     msg);
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
        
        common = intersect_dw(p->dw, c->dw, c->prompt_tag, c->has_prompt_dw, &common_depth);
        *_common = common;
      }
    } else
      dw = dw->prev;
  }
  return common_depth;
}

Scheme_Object *scheme_jump_to_continuation(Scheme_Object *obj, int num_rands, Scheme_Object **rands, Scheme_Object **old_runstack)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Cont *c;
  Scheme_Dynamic_Wind *common, *new_common;
  Scheme_Object *value;
  Scheme_Meta_Continuation *prompt_mc;
  MZ_MARK_POS_TYPE prompt_pos;
  Scheme_Prompt *prompt, *barrier_prompt;
  int common_depth;
      
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
  } else
    value = rands[0];
      
  c = (Scheme_Cont *)obj;
      
  DO_CHECK_FOR_BREAK(p, ;);

  if (!c->runstack_copied) {
    /* This continuation is the same as another, except
       that its mark stack is different. The different part
       of the mark stack won't be visible, so we use the other. */
    c = c->buf.cont;
  }

  if (c->composable) {
    /* Composable continuation. Jump right in... */
    scheme_continuation_application_count++;
    MZ_RUNSTACK = old_runstack;
    return scheme_compose_continuation(c, num_rands, value);
  } else {
    /* Aborting (Scheme-style) continuation. */
    int orig_cac = scheme_continuation_application_count;

    scheme_about_to_move_C_stack();

    prompt = lookup_cont_prompt(c, &prompt_mc, &prompt_pos, LOOKUP_NO_PROMPT);
    barrier_prompt = check_barrier(prompt, prompt_mc, prompt_pos, c);

    p->suspend_break++; /* restored at call/cc destination */

    /* Find `common', the intersection of dynamic-wind chain for 
       the current continuation and the given continuation, looking
       no further back in the current continuation than a prompt. */
    common = intersect_dw(p->dw, c->dw, c->prompt_tag, c->has_prompt_dw, &common_depth);

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
      
    if (num_rands == 1)
      c->value = value;
    else {
      GC_CAN_IGNORE Scheme_Object *vals;
      vals = scheme_values(num_rands, (Scheme_Object **)value);
      c->value = vals;
    }

    c->common_dw = common;
    c->common_next_meta = p->next_meta;

    scheme_continuation_application_count++;

    if (!prompt) {
      /* Invoke the continuation directly. If there's no prompt,
         then the prompt's job is taken by the pseudo-prompt
         created with a new thread or a barrier prompt. */
      p->meta_continuation = NULL; /* since prompt wasn't in any meta-continuation */
      p->meta_prompt = NULL;
      if ((c->barrier_prompt == barrier_prompt) && barrier_prompt) {
        /* Barrier determines continuation end. */
        c->resume_to = NULL;
        p->stack_start = c->stack_start;
      } else {
        /* Prompt is pseudo-prompt at thread beginning.
           We're effectively composing the continuation,
           so use it's prompt stack start. */
        Scheme_Overflow *oflow;
        oflow = scheme_get_thread_end_overflow();
        c->resume_to = oflow;
        p->stack_start = c->prompt_stack_start;
      }
      scheme_longjmpup(&c->buf);
    } else if (prompt->id
               && (prompt->id == c->prompt_id)
               && !prompt_mc) {
      /* The current prompt is the same as the one in place when
         capturing the continuation, so we can jump directly. */
      scheme_drop_prompt_meta_continuations(c->prompt_tag);
      c->shortcut_prompt = prompt;
      if ((!prompt->boundary_overflow_id && !p->overflow)
          || (prompt->boundary_overflow_id
              && (prompt->boundary_overflow_id == p->overflow->id))) {
        scheme_longjmpup(&c->buf);
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
        p->overflow = overflow;
        p->stack_start = overflow->stack_start;
        scheme_longjmpup(&overflow->jmp->cont);
      }
    } else {
      p->cjs.jumping_to_continuation = (Scheme_Object *)prompt;
      p->cjs.num_vals = 1;
      p->cjs.val = (Scheme_Object *)c;
      p->cjs.is_escape = 1;
       
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
        scheme_longjmpup(&prompt_mc->overflow->jmp->cont);
      } else if ((!prompt->boundary_overflow_id && !p->overflow)
                 || (prompt->boundary_overflow_id
                     && (prompt->boundary_overflow_id == p->overflow->id))) {
        /* Jump directly to the prompt: destination is in
           scheme_finish_apply_for_prompt() in fun.c. */
        scheme_drop_prompt_meta_continuations(c->prompt_tag);
        scheme_longjmp(*prompt->prompt_buf, 1);
      } else {
        /* Need to unwind overflows to get to the prompt. */
        Scheme_Overflow *overflow;
        scheme_drop_prompt_meta_continuations(c->prompt_tag);
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

void scheme_escape_to_continuation(Scheme_Object *obj, int num_rands, Scheme_Object **rands)
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
  scheme_longjmp(MZTHREADELEM(p, error_buf), 1);
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
{
  Scheme_Type type;
  Scheme_Object *v;
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

#define RUNSTACK_START MZ_RUNSTACK_START

#define UPDATE_THREAD_RSPTR_FOR_GC() UPDATE_THREAD_RSPTR()
#define UPDATE_THREAD_RSPTR_FOR_ERROR() UPDATE_THREAD_RSPTR()

#define UPDATE_THREAD_RSPTR_FOR_PROC_MARK() UPDATE_THREAD_RSPTR()

  MZ_CONT_MARK_POS += 2;
  old_runstack = RUNSTACK;
  old_cont_mark_stack = MZ_CONT_MARK_STACK;

  if (num_rands >= 0) {

    if ((RUNSTACK - RUNSTACK_START) < SCHEME_TAIL_COPY_THRESHOLD) {
      /* It's possible that a sequence of primitive _scheme_tail_apply()
	 calls will exhaust the Scheme stack. Watch out for that. */
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
	    scheme_wrong_count_m(scheme_get_proc_name(obj, NULL, 1), 
				 num_params - 1, -1,
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
	    if ((unsigned long)rands > (unsigned long)stack) {
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
	    scheme_wrong_count_m(scheme_get_proc_name(obj, NULL, 1), 
				 num_params, num_params,
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
	    scheme_wrong_count(scheme_get_proc_name(obj, NULL, 1),
			       0, 0, num_rands, rands);
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

      if (pmstack >= 0) {
        long segpos = ((long)pmstack) >> SCHEME_LOG_MARK_SEGMENT_SIZE;
        long pos = ((long)pmstack) & SCHEME_MARK_SEGMENT_MASK;
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
	long segpos = ((long)MZ_CONT_MARK_STACK) >> SCHEME_LOG_MARK_SEGMENT_SIZE;
	if (segpos >= p->cont_mark_seg_count) {
	  UPDATE_THREAD_RSPTR_FOR_PROC_MARK();
	  pmstack = scheme_set_cont_mark(scheme_stack_dump_key, data->name); 
	} else {
	  long pos = ((long)MZ_CONT_MARK_STACK) & SCHEME_MARK_SEGMENT_MASK;
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

      if (!scheme_native_arity_check(obj, num_rands)) {
	scheme_wrong_count_m((const char *)obj, -1, -1,
			     num_rands, rands, 0);
	return NULL;
      }

      data = ((Scheme_Native_Closure *)obj)->code;

      /* Enlarge the runstack? This max_let_depth is in bytes instead of words. */
      if ((unsigned long)data->max_let_depth > ((unsigned long)RUNSTACK - (unsigned long)RUNSTACK_START)) {
	p->ku.k.p1 = (void *)obj;
	p->ku.k.i1 = num_rands;
	p->ku.k.p2 = (void *)rands;
	p->ku.k.i2 = -1;

	MZ_CONT_MARK_POS -= 2;
	v = (Scheme_Object *)scheme_enlarge_runstack(data->max_let_depth / sizeof(void *), 
						     (void *(*)(void))do_eval_k);
	MZ_CONT_MARK_POS += 2;
	goto returnv;
      }

      v = data->code(obj, num_rands, rands);

      DEBUG_CHECK_TYPE(v);
#endif
    } else if (type == scheme_cont_type) {
      UPDATE_THREAD_RSPTR();
      v = scheme_jump_to_continuation(obj, num_rands, rands, old_runstack);
    } else if (type == scheme_escaping_cont_type) {
      UPDATE_THREAD_RSPTR();
      scheme_escape_to_continuation(obj, num_rands, rands);
      return NULL;
    } else if (type == scheme_proc_struct_type) {
      int is_method;
      int check_rands = num_rands;

      do {
        VACATE_TAIL_BUFFER_USE_RUNSTACK();

        UPDATE_THREAD_RSPTR_FOR_ERROR(); /* in case */

        v = obj;
        obj = scheme_extract_struct_procedure(obj, check_rands, rands, &is_method);
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
      } while (SAME_TYPE(scheme_proc_struct_type, SCHEME_TYPE(obj)));

      goto apply_top;
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
          tmp = ((Scheme_Object **)tmp)[SCHEME_TOPLEVEL_POS(_obj)];     \
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
	  goto returnv_never_multi;
	}
      case scheme_local_unbox_type:
	{
	  v = SCHEME_ENVBOX_VAL(RUNSTACK[SCHEME_LOCAL_POS(obj)]);
	  goto returnv_never_multi;
	}
      case scheme_syntax_type:
	{
	  Scheme_Syntax_Executer f;

	  UPDATE_THREAD_RSPTR();
	  f = scheme_syntax_executers[SCHEME_PINT_VAL(obj)];
	  v = f((Scheme_Object *)SCHEME_IPTR_VAL(obj));
	  break;
	}
      case scheme_application_type:
	{
	  Scheme_App_Rec *app;
	  GC_CAN_IGNORE Scheme_Object *tmpv;
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
	  
	  d_evals = sizeof(Scheme_App_Rec) + (num_rands * sizeof(Scheme_Object *));
#ifndef MZ_XFORM
	  evals = ((char *)obj) + d_evals;
#endif

	  obj = app->args[0];
	  
	  stack = PUSH_RUNSTACK(p, RUNSTACK, num_rands);
	  RUNSTACK_CHANGED();
	  UPDATE_THREAD_RSPTR();

	  /* Inline local & global variable lookups for speed */
	  switch (GET_FIRST_EVAL) {
	  case SCHEME_EVAL_CONSTANT:
	    break;
	  case SCHEME_EVAL_GLOBAL:
	    global_lookup(obj =, obj, tmpv);
	    break;
	  case SCHEME_EVAL_LOCAL:
	    obj = stack[SCHEME_LOCAL_POS(obj)];
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    obj = SCHEME_ENVBOX_VAL(stack[SCHEME_LOCAL_POS(obj)]);
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
		break;
	      case SCHEME_EVAL_LOCAL_UNBOX:
		*(randsp++) = SCHEME_ENVBOX_VAL(stack[SCHEME_LOCAL_POS(v)]);
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
	  
	  /* Inline local & global variable lookups for speed */
	  switch (flags & 0x7) {
	  case SCHEME_EVAL_CONSTANT:
	    break;
	  case SCHEME_EVAL_GLOBAL:
            {
              GC_CAN_IGNORE Scheme_Object *tmpv;
              global_lookup(obj =, obj, tmpv);
            }
	    break;
	  case SCHEME_EVAL_LOCAL:
	    obj = rands[SCHEME_LOCAL_POS(obj)];
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    obj = SCHEME_ENVBOX_VAL(rands[SCHEME_LOCAL_POS(obj)]);
	    break;
	  default:
	    obj = _scheme_eval_linked_expr_wp(obj, p);
	    break;
	  }

	  arg = app->rand;

	  switch (flags >> 3) {
	  case SCHEME_EVAL_CONSTANT:
	    break;
	  case SCHEME_EVAL_GLOBAL:
            {
              GC_CAN_IGNORE Scheme_Object *tmpv;
              global_lookup(arg =, arg, tmpv);
            }
	    break;
	  case SCHEME_EVAL_LOCAL:
	    arg = rands[SCHEME_LOCAL_POS(arg)];
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    arg = SCHEME_ENVBOX_VAL(rands[SCHEME_LOCAL_POS(arg)]);
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
	  GC_CAN_IGNORE Scheme_Object *tmpv;

	  app = (Scheme_App3_Rec *)obj;
	  
	  obj = app->rator;
	  flags = SCHEME_APPN_FLAGS(app);

	  rands = PUSH_RUNSTACK(p, RUNSTACK, 2);
	  RUNSTACK_CHANGED();
	  UPDATE_THREAD_RSPTR();
	  
	  /* Inline local & global variable lookups for speed */
	  switch (flags & 0x7) {
	  case SCHEME_EVAL_CONSTANT:
	    break;
	  case SCHEME_EVAL_GLOBAL:
	    global_lookup(obj =, obj, tmpv);
	    break;
	  case SCHEME_EVAL_LOCAL:
	    obj = rands[SCHEME_LOCAL_POS(obj)];
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    obj = SCHEME_ENVBOX_VAL(rands[SCHEME_LOCAL_POS(obj)]);
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
	    arg = rands[SCHEME_LOCAL_POS(arg)];
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    arg = SCHEME_ENVBOX_VAL(rands[SCHEME_LOCAL_POS(arg)]);
	    break;
	  default:
	    arg = _scheme_eval_linked_expr_wp(arg, p);
	    break;
	  }

	  rands[0] = arg;

	  arg = app->rand2;

	  switch (SCHEME_APPN_FLAGS(app) >> 6) {
	  case SCHEME_EVAL_CONSTANT:
	    break;
	  case SCHEME_EVAL_GLOBAL:
	    global_lookup(arg =, arg, tmpv);
	    break;
	  case SCHEME_EVAL_LOCAL:
	    arg = rands[SCHEME_LOCAL_POS(arg)];
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    arg = SCHEME_ENVBOX_VAL(rands[SCHEME_LOCAL_POS(arg)]);
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
	    (void)_scheme_eval_linked_expr_multi_wp(((Scheme_Sequence *)obj)->array[i], p);
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
					"lexical binding");
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
	    Scheme_Object **stack = RUNSTACK;

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
	  Scheme_Letrec *l = (Scheme_Letrec *)obj;
	  Scheme_Object **a, **stack;
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
	  goto eval_top;
	}

      case scheme_let_one_type:
	{
	  /* Macro instead of var for efficient precise GC conversion */
# define lo ((Scheme_Let_One *)obj)

	  PUSH_RUNSTACK(p, RUNSTACK, 1);
	  RUNSTACK_CHANGED();

	  switch (SCHEME_LET_EVAL_TYPE(lo)) {
	  case SCHEME_EVAL_CONSTANT:
	    RUNSTACK[0] = lo->value;
	    break;
	  case SCHEME_EVAL_GLOBAL:
	    {
	      GC_CAN_IGNORE Scheme_Object *tmpv;
	      global_lookup(RUNSTACK[0] =, lo->value, tmpv);
	    }
	    break;
	  case SCHEME_EVAL_LOCAL:
	    RUNSTACK[0] = RUNSTACK[SCHEME_LOCAL_POS(lo->value)];
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    RUNSTACK[0] = SCHEME_ENVBOX_VAL(RUNSTACK[SCHEME_LOCAL_POS(lo->value)]);
	    break;
	  default:
	    UPDATE_THREAD_RSPTR();
	    {
	      GC_CAN_IGNORE Scheme_Object *val;
	      val = _scheme_eval_linked_expr_wp(lo->value, p);
	      RUNSTACK[0] = val;
	    }
	    break;
	  }

	  obj = lo->body;
#undef lo
	  goto eval_top;
	}
      
      case scheme_with_cont_mark_type:
	{
	  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)obj;
	  GC_CAN_IGNORE Scheme_Object *key, *val;
	  
	  UPDATE_THREAD_RSPTR();
	  key = wcm->key;
	  if (SCHEME_TYPE(key) < _scheme_values_types_)
	    key = _scheme_eval_linked_expr_wp(wcm->key, p);
	  val = wcm->val;
	  if (SCHEME_TYPE(val) < _scheme_values_types_)
	    val = _scheme_eval_linked_expr_wp(wcm->val, p);

	  scheme_set_cont_mark(key, val);

	  obj = wcm->body;

	  goto eval_top;
	}

      case scheme_quote_syntax_type:
	{
	  GC_CAN_IGNORE Scheme_Quote_Syntax *qs = (Scheme_Quote_Syntax *)obj;
	  Scheme_Object **globs;
	  int i, c, p;

	  i = qs->position;
	  c = qs->depth;
	  p = qs->midpoint;

	  globs = (Scheme_Object **)RUNSTACK[c];
	  v = globs[i+p+1];
	  if (!v) {
	    v = globs[p];
	    v = scheme_delayed_rename((Scheme_Object **)v, i);
	    globs[i+p+1] = v;
	  }

	  goto returnv_never_multi;
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

/*========================================================================*/
/*                  eval/compile/expand starting points                   */
/*========================================================================*/

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
  return scheme_call_with_prompt(finish_eval_multi_with_prompt, 
                                 scheme_make_pair(expr, (Scheme_Object *)env));
}

static void *eval_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v, **save_runstack;
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

    depth = top->max_let_depth + scheme_prefix_depth(top->prefix);
    if (!scheme_check_runstack(depth)) {
      p->ku.k.p1 = top;
      p->ku.k.p2 = env;
      p->ku.k.i1 = multi;
      p->ku.k.i2 = 0;
      return (Scheme_Object *)scheme_enlarge_runstack(depth, eval_k);
    }

    v = top->code;

    if (use_jit)
      v = scheme_jit_expr(v);

    save_runstack = scheme_push_prefix(env, top->prefix, NULL, NULL, 0, env->phase);

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

/* for mzc: */
Scheme_Object *scheme_load_compiled_stx_string(const char *str, long len)
{
  Scheme_Object *port, *expr;

  port = scheme_make_sized_byte_string_input_port(str, -len);

  expr = scheme_internal_read(port, NULL, 1, 0, 0, 0, 0, -1, NULL, NULL, NULL, NULL);

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
					       long shift, Scheme_Object *modidx)
{
  /* If modidx, then last element is a module index; shift the rest. */
  if (modidx) {
    int i, len = SCHEME_VEC_SIZE(expr);
    Scheme_Object *orig = SCHEME_VEC_ELS(expr)[len - 1], *s, *result;

    orig = SCHEME_STX_VAL(orig);
    result = scheme_make_vector(len - 1, NULL);

    for (i = 0; i < len - 1; i++) {
      s = scheme_stx_phase_shift(SCHEME_VEC_ELS(expr)[i], shift, orig, modidx, env->export_registry);
      SCHEME_VEC_ELS(result)[i] = s;
    }
    
    return result;
  } else
    return expr;
}

static void *expand_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *obj, *certs, *observer, *catch_lifts_key;
  Scheme_Comp_Env *env;
  Scheme_Expand_Info erec1;
  int depth, rename, just_to_top;

  obj = (Scheme_Object *)p->ku.k.p1;
  env = (Scheme_Comp_Env *)p->ku.k.p2;
  depth = p->ku.k.i1;
  rename = p->ku.k.i2;
  just_to_top = p->ku.k.i3;
  catch_lifts_key = p->ku.k.p4;
  certs = (Scheme_Object *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  if (!SCHEME_STXP(obj))
    obj = scheme_datum_to_syntax(obj, scheme_false, scheme_false, 1, 0);

  if (rename > 0) {
    /* Renamings for requires: */
    obj = add_renames_unless_module(obj, env->genv);
  }

  observer = scheme_get_expand_observe();
  SCHEME_EXPAND_OBSERVE_START_EXPAND(observer);

  /* Loop for lifted expressions: */
  while (1) {
    erec1.comp = 0;
    erec1.depth = depth;
    erec1.value_name = scheme_false;
    erec1.certs = certs;
    erec1.observer = observer;

    if (catch_lifts_key)
      scheme_frame_captures_lifts(env, scheme_make_lifted_defn, scheme_sys_wraps(env), scheme_false, catch_lifts_key);
  
    if (just_to_top) {
      Scheme_Object *gval;
      obj = scheme_check_immediate_macro(obj, env, &erec1, 0, 0, &gval, NULL, NULL);
    } else
      obj = scheme_expand_expr(obj, env, &erec1, 0);

    if (catch_lifts_key) {
      Scheme_Object *l;
      l = scheme_frame_get_lifts(env);
      if (SCHEME_PAIRP(l)) {
	obj = scheme_append(l, scheme_make_immutable_pair(obj, scheme_null));
	obj = icons(scheme_datum_to_syntax(begin_symbol, scheme_false, scheme_sys_wraps(env), 0, 0),
		    obj);
	obj = scheme_datum_to_syntax(obj, scheme_false, scheme_false, 0, 0);
        SCHEME_EXPAND_OBSERVE_LIFT_LOOP(erec1.observer,obj);
	if (depth >= 0)
	  break;
      } else
	break;
    } else
      break;
  }

  if (rename && !just_to_top) {
    /* scheme_simplify_stx(obj, scheme_new_stx_simplify_cache()); */ /* too expensive */
  }

  return obj;
}

static Scheme_Object *_expand(Scheme_Object *obj, Scheme_Comp_Env *env, 
			      int depth, int rename, int just_to_top, 
			      Scheme_Object *catch_lifts_key, int eb,
			      Scheme_Object *certs)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = obj;
  p->ku.k.p2 = env;
  p->ku.k.i1 = depth;
  p->ku.k.i2 = rename;
  p->ku.k.i3 = just_to_top;
  p->ku.k.p4 = catch_lifts_key;
  p->ku.k.p3 = certs;

  return (Scheme_Object *)scheme_top_level_do(expand_k, eb);
}

Scheme_Object *scheme_expand(Scheme_Object *obj, Scheme_Env *env)
{
  return _expand(obj, scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
		 -1, 1, 0, scheme_true, -1, NULL);
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
      scheme_wrong_type(who, "namespace", 1, argc, argv);

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
	scheme_wrong_type("eval", "namespace", 1, argc, argv);
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
    scheme_wrong_type("eval-syntax", "syntax", 0, argc, argv);
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
    scheme_wrong_type("namespace-syntax-introduce", "syntax", 0, argc, argv);
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
    scheme_wrong_type("compile-syntax", "syntax", 0, argc, argv);

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

  return _expand(argv[0], scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
                 -1, 1, 0, scheme_true, 0, NULL);
}

static Scheme_Object *expand_stx(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("expand-syntax", "syntax", 0, argc, argv);

  env = scheme_get_env(NULL);
  
  return _expand(argv[0], scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
                 -1, -1, 0, scheme_true, 0, NULL);
}

static Scheme_Object *stop_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
				  Scheme_Compile_Info *rec, int drec)
{
  scheme_signal_error("internal error: shouldn't get to stop syntax");
  return NULL;
}

static Scheme_Object *stop_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_STOP(erec[drec].observer);
  return form;
}

Scheme_Object *scheme_get_stop_expander(void)
{
  if (!stop_expander) {
    REGISTER_SO(stop_expander);
    stop_expander = scheme_make_compiled_syntax(stop_syntax, 
						stop_expand);
  }

  return stop_expander;
}

Scheme_Object *scheme_generate_lifts_key(void)
{
  static int cnt = 0;
  char buf[20];
  sprintf(buf, "lifts%d", cnt++);
  return scheme_make_symbol(buf); /* uninterned */
}

Scheme_Object *
scheme_make_lifted_defn(Scheme_Object *sys_wraps, Scheme_Object **_id, Scheme_Object *expr, Scheme_Comp_Env *env)
{
  Scheme_Object *l;

  /* Registers marked id: */
  scheme_tl_id_sym(env->genv, *_id, scheme_false, 2);

  l = icons(scheme_datum_to_syntax(define_values_symbol, scheme_false, sys_wraps, 0, 0), 
	    icons(scheme_make_immutable_pair(*_id, scheme_null),
		  icons(expr,
			scheme_null)));

  return scheme_datum_to_syntax(l, scheme_false, scheme_false, 0, 0);
}

static Scheme_Object *
do_local_expand(const char *name, int for_stx, int catch_lifts, int for_expr, int argc, Scheme_Object **argv)
{
  Scheme_Comp_Env *env, *orig_env;
  Scheme_Object *l, *local_mark, *renaming = NULL, *orig_l, *exp_expr = NULL;
  int cnt, pos, kind;
  int bad_sub_env = 0;
  Scheme_Object *observer, *catch_lifts_key = NULL;

  env = scheme_current_thread->current_local_env;
  orig_env = env;

  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, "%s: not currently transforming", name);

  if (for_stx) {
    scheme_prepare_exp_env(env->genv);
    env = scheme_new_comp_env(env->genv->exp_env, env->insp, 0);
  }

  if (for_expr)
    kind = 0; /* expression */
  else if (SAME_OBJ(argv[1], module_symbol))
    kind = SCHEME_MODULE_BEGIN_FRAME; /* name is backwards compared to symbol! */
  else if (SAME_OBJ(argv[1], module_begin_symbol))
    kind = SCHEME_MODULE_FRAME; /* name is backwards compared to symbol! */
  else if (SAME_OBJ(argv[1], top_level_symbol))
    kind = SCHEME_TOPLEVEL_FRAME;
  else if (SAME_OBJ(argv[1], expression_symbol))
    kind = 0;
  else if (scheme_proper_list_length(argv[1]) > 0)
    kind = SCHEME_INTDEF_FRAME;
  else  {
    scheme_wrong_type(name,
		      "'expression, 'module, 'module-begin, 'top-level, or non-empty list",
		      1, argc, argv);
    return NULL;
  }

  if (argc > 3) {
    if (SCHEME_TRUEP(argv[3])) {
      if (SAME_TYPE(scheme_intdef_context_type, SCHEME_TYPE(argv[3]))) {
	Scheme_Comp_Env *stx_env;
	stx_env = (Scheme_Comp_Env *)SCHEME_PTR1_VAL(argv[3]);
	renaming = SCHEME_PTR2_VAL(argv[3]);
	if (!scheme_is_sub_env(stx_env, env))
	  bad_sub_env = 1;
	env = stx_env;
      }
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
				     env, NULL);
  if (kind == SCHEME_INTDEF_FRAME)
    env->intdef_name = argv[1];
  env->in_modidx = scheme_current_thread->current_local_modidx;

  local_mark = scheme_current_thread->current_local_mark;
  
  if (for_expr) {
  } else if (SCHEME_TRUEP(argv[2])) {
    cnt = scheme_stx_proper_list_length(argv[2]);
    if (cnt > 0)
      scheme_add_local_syntax(cnt, env);
    pos = 0;

    for (l = argv[2]; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      Scheme_Object *i;
    
      i = SCHEME_CAR(l);
      if (!SCHEME_STX_SYMBOLP(i)) {
        scheme_wrong_type(name, "#f or list of identifier syntax", 2, argc, argv);
        return NULL;
      }
    
      if (cnt > 0)
        scheme_set_local_syntax(pos++, i, stop_expander, env);
    }
    if (!SCHEME_NULLP(l)) {
      scheme_wrong_type(name, "#f or list of identifier syntax", 2, argc, argv);
      return NULL;
    }
  }

  /* Report errors related to 3rd argument, finally */
  if (argc > 3) {
    if (SCHEME_TRUEP(argv[3])) {
      if (SAME_TYPE(scheme_intdef_context_type, SCHEME_TYPE(argv[3]))) {
	if (bad_sub_env) {
	  scheme_raise_exn(MZEXN_FAIL_CONTRACT, "%s: transforming context does "
			   "not match internal-definition context at the front of the context list",
			   name);
	  return NULL;
	}
      } else {
	scheme_wrong_type(name, "internal-definition context or #f", 3, argc, argv);
	return NULL;
      }
    }
  }

  l = argv[0];

  if (!SCHEME_STXP(l))
    l = scheme_datum_to_syntax(l, scheme_false, scheme_false, 1, 0);

  orig_l = l;

  observer = scheme_get_expand_observe();
  if (observer) {
    if (for_expr) {
      SCHEME_EXPAND_OBSERVE_ENTER_LOCAL_EXPR(observer, l);
    } else {
      SCHEME_EXPAND_OBSERVE_ENTER_LOCAL(observer, l);
    }
    if (for_stx) {
      SCHEME_EXPAND_OBSERVE_PHASE_UP(observer);
    }
  }

  if (local_mark) {
    /* Since we have an expression from local context,
       we need to remove the temporary mark... */
    l = scheme_add_remove_mark(l, local_mark);
  }

  l = scheme_stx_activate_certs(l);

  if (renaming)
    l = scheme_add_rename(l, renaming);

  SCHEME_EXPAND_OBSERVE_LOCAL_PRE(observer, l);

  if (SCHEME_FALSEP(argv[2])) {
    Scheme_Object *xl, *gval;
    Scheme_Compile_Expand_Info drec[1];
    memset(drec, 0, sizeof(drec));
    drec[0].value_name = scheme_false; /* or scheme_current_thread->current_local_name ? */
    drec[0].certs = scheme_current_thread->current_local_certs;
    drec[0].depth = -2;
    xl = scheme_check_immediate_macro(l, env, drec, 0, 0, &gval, NULL, NULL);
    if (SAME_OBJ(xl, l))
      return orig_l;
    l = xl;
  } else {
    /* Expand the expression. depth = -2 means expand all the way, but
       preserve letrec-syntax. */
    l = _expand(l, env, -2, 0, 0, catch_lifts_key, 0, scheme_current_thread->current_local_certs);
  }

  SCHEME_EXPAND_OBSERVE_LOCAL_POST(observer, l);

  if (renaming)
    l = scheme_add_rename(l, renaming);

  if (for_expr) {
    /* Package up expanded expr with the enviornment. */
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
    exp_expr = scheme_add_remove_mark(exp_expr, local_mark);
  }

  if (local_mark) {
    /* Put the temporary mark back: */
    l = scheme_add_remove_mark(l, local_mark);
  }

  if (for_expr) {
    Scheme_Object *a[2];
    SCHEME_EXPAND_OBSERVE_EXIT_LOCAL_EXPR(observer, l, exp_expr);
    a[0] = l;
    a[1] = exp_expr;
    return scheme_values(2, a);
  } else
    SCHEME_EXPAND_OBSERVE_EXIT_LOCAL(observer, l);
    return l;
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
  return do_local_expand("local-transformer-expand", 1, 0, 0, argc, argv);
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

  return _expand(argv[0], scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
                 1, 1, 0, scheme_true, 0, NULL);
}

static Scheme_Object *
expand_stx_once(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("expand-syntax-once", "syntax", 0, argc, argv);
  
  env = scheme_get_env(NULL);

  return _expand(argv[0], scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
                 1, -1, 0, scheme_true, 0, NULL);
}

static Scheme_Object *
expand_to_top_form(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  env = scheme_get_env(NULL);

  return _expand(argv[0], scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
                 1, 1, 1, scheme_true, 0, NULL);
}

static Scheme_Object *
expand_stx_to_top_form(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("expand-syntax-to-top", "syntax", 0, argc, argv);
  
  env = scheme_get_env(NULL);

  return _expand(argv[0], scheme_new_expand_env(env, NULL, SCHEME_TOPLEVEL_FRAME), 
                 1, -1, 1, scheme_true, 0, NULL);
}

static Scheme_Object *do_eval_string_all(const char *str, Scheme_Env *env, int cont, int w_prompt)
{
  Scheme_Object *port, *expr, *result = scheme_void;

  port = scheme_make_byte_string_input_port(str);
  do {
    expr = scheme_read_syntax(port, scheme_false);
    if (SAME_OBJ(expr, scheme_eof))
      cont = 0;
    else if (cont < 0) {
      if (w_prompt)
        result = scheme_eval_with_prompt(expr, env);
      else
        result = scheme_eval(expr, env);
    } else {
      if (w_prompt)
        result = scheme_eval_multi_with_prompt(expr, env);
      else
        result = scheme_eval_multi(expr, env);
    }
  } while (cont > 0);

  return result;
}

Scheme_Object *scheme_eval_string_all(const char *str, Scheme_Env *env, int cont)
{
  return do_eval_string_all(str, env, cont, 0);
}

Scheme_Object *scheme_eval_string(const char *str, Scheme_Env *env)
{
  return do_eval_string_all(str, env, -1, 0);
}

Scheme_Object *scheme_eval_string_multi(const char *str, Scheme_Env *env)
{
  return do_eval_string_all(str, env, 0, 0);
}

Scheme_Object *scheme_eval_string_all_with_prompt(const char *str, Scheme_Env *env, int cont)
{
  return do_eval_string_all(str, env, cont, 1);
}

Scheme_Object *scheme_eval_string_with_prompt(const char *str, Scheme_Env *env)
{
  return do_eval_string_all(str, env, -1, 1);
}

Scheme_Object *scheme_eval_string_multi_with_prompt(const char *str, Scheme_Env *env)
{
  return do_eval_string_all(str, env, 0, 1);
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
  Scheme_Object *l, *a, *rib, *expr, *certs, *names;
  int cnt = 0, pos;
  
  names = argv[0];
  for (l = names; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    a = SCHEME_CAR(l);
    if (!SCHEME_STX_SYMBOLP(a))
      break;
    cnt++;
  }
  if (!SCHEME_NULLP(l))
    scheme_wrong_type("syntax-local-bind-syntaxes", "list of syntax identifieres", 0, argc, argv);

  expr = argv[1];
  if (!SCHEME_FALSEP(expr) && !SCHEME_STXP(expr))
    scheme_wrong_type("syntax-local-bind-syntaxes", "syntax or #f", 1, argc, argv);
  if (!SAME_TYPE(scheme_intdef_context_type, SCHEME_TYPE(argv[2])))
    scheme_wrong_type("syntax-local-bind-syntaxes", "internal-definition context", 2, argc, argv);

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, "syntax-local-bind-syntaxes: not currently transforming");

  stx_env = (Scheme_Comp_Env *)SCHEME_PTR1_VAL(argv[2]);
  rib = SCHEME_PTR2_VAL(argv[2]);
  
  if (!scheme_is_sub_env(stx_env, env)) {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, "syntax-local-bind-syntaxes: transforming context does "
		     "not match given internal-definition context");
  }

  certs = scheme_current_thread->current_local_certs;
  old_stx_env = stx_env;
  stx_env = scheme_new_compilation_frame(0, SCHEME_FOR_INTDEF, stx_env, certs);
  scheme_add_local_syntax(cnt, stx_env);

  /* Mark names */
  names = scheme_named_map_1(NULL, scheme_add_remove_mark, names,
			     scheme_current_thread->current_local_mark);

  /* Initialize environment slots to #f, which means "not syntax". */
  cnt = 0;
  for (l = names; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    scheme_set_local_syntax(cnt++, SCHEME_CAR(l), scheme_false, stx_env);
  }
	  
  stx_env->in_modidx = scheme_current_thread->current_local_modidx;
  if (!SCHEME_FALSEP(expr)) {

    Scheme_Compile_Expand_Info rec;
    rec.comp = 0;
    rec.depth = -1;
    rec.value_name = scheme_false;
    rec.certs = certs;
    rec.observer = scheme_get_expand_observe();
    
    /* Evaluate and bind syntaxes */
    expr = scheme_add_remove_mark(expr, scheme_current_thread->current_local_mark);

    scheme_prepare_exp_env(stx_env->genv);
    pos = 0;
    expr = scheme_add_rename_rib(expr, rib);
    scheme_bind_syntaxes("local syntax definition", names, expr,
			 stx_env->genv->exp_env, stx_env->insp, &rec, 0,
			 stx_env, stx_env,
			 &pos);
  }

  /* Extend shared rib with renamings */
  scheme_add_env_renames(rib, stx_env, old_stx_env);

  /* Remember extended environment */
  SCHEME_PTR1_VAL(argv[2]) = stx_env;

  return scheme_void;
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
				   int src_phase, int now_phase)
{
  Scheme_Object **rs_save, **rs, *v, **a;
  int i, j;

  rs_save = rs = MZ_RUNSTACK;

  if (rp->num_toplevels || rp->num_stxes || rp->num_lifts) {
    i = rp->num_toplevels;
    if (rp->num_stxes) {
      i += rp->num_stxes + 1;
    }
    i += rp->num_lifts;

    a = MALLOC_N(Scheme_Object *, i);
    --rs;
    MZ_RUNSTACK = rs;
    rs[0] = (Scheme_Object *)a;
   
    for (i = 0; i < rp->num_toplevels; i++) {
      v = rp->toplevels[i];
      if (genv)
	v = link_toplevel(rp->toplevels[i], genv, src_modidx, now_modidx);
      a[i] = v;
    }

    if (rp->num_stxes) {
      i = rp->num_toplevels;
      v = scheme_stx_phase_shift_as_rename(now_phase - src_phase, src_modidx, now_modidx, 
					   genv ? genv->export_registry : NULL);
      if (v || rp->delay_info) {
	/* Put lazy-shift info in a[i]: */
        Scheme_Object **ls;
        ls = MALLOC_N(Scheme_Object *, 2);
        ls[0] = v;
        ls[1] = (Scheme_Object *)rp;
	a[i] = (Scheme_Object *)ls;
	/* Rest of a left zeroed, to be filled in lazily by quote-syntax evaluation */
      } else {
	/* No shift, so fill in stxes immediately */
	i++;
	for (j = 0; j < rp->num_stxes; j++) {
	  a[i + j] = rp->stxes[j];
	}
      }
      j = rp->num_stxes + 1;
    } else
      j = 0;

    if (rp->num_lifts) {
      Scheme_Object *sym;
      sym = scheme_make_symbol("<lifted>"); /* uninterned! */
      j += rp->num_toplevels;
      for (i = 0; i < rp->num_lifts; i++, j++) {
        v = (Scheme_Object *)MALLOC_ONE_TAGGED(Scheme_Bucket_With_Home);
        v->type = scheme_variable_type;
        ((Scheme_Bucket_With_Flags *)v)->flags = GLOB_HAS_HOME_PTR;
        ((Scheme_Bucket_With_Home *)v)->home = genv;
        ((Scheme_Bucket *)v)->key = (char *)sym;
        a[j] = v;
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

/*========================================================================*/
/*                          bytecode validation                           */
/*========================================================================*/

/* Bytecode validation is an abstract interpretation on the stack,
   where the abstract values are "not available", "value", "boxed
   value", "syntax object", or "global array". */

/* FIXME: validation doesn't check CLOS_SINGLE_RESULT or
   CLOS_PRESERVES_MARKS. (Maybe check them in the JIT pass?) */

#define VALID_NOT 0
#define VALID_VAL 1
#define VALID_BOX 2
#define VALID_TOPLEVELS 3

void scheme_validate_code(Mz_CPort *port, Scheme_Object *code, Scheme_Hash_Table *ht,
                          int depth, 
                          int num_toplevels, int num_stxes, int num_lifts)
{
  char *stack;
  int delta;
  Scheme_Object **tls;

  depth += ((num_toplevels || num_stxes || num_lifts) ? 1 : 0);

  stack = scheme_malloc_atomic(depth);
  
  if (num_toplevels || num_stxes || num_lifts) {
    stack[depth - 1] = VALID_TOPLEVELS;
  }

  delta = depth - ((num_toplevels || num_stxes || num_lifts) ? 1 : 0);

  tls = MALLOC_N(Scheme_Object*, num_toplevels + num_lifts);

  scheme_validate_expr(port, code, 
                       stack, ht, tls,
                       depth, delta, delta, 
                       num_toplevels, num_stxes, num_lifts,
                       NULL, 0);
}

static Scheme_Object *validate_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Mz_CPort *port = (Mz_CPort *)p->ku.k.p1;
  Scheme_Object *expr = (Scheme_Object *)p->ku.k.p2;
  char *stack = (char *)p->ku.k.p3;
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p->ku.k.p4;
  int *args = (int *)(((void **)p->ku.k.p5)[0]);
  Scheme_Object *app_rator = (Scheme_Object *)(((void **)p->ku.k.p5)[1]);
  Scheme_Object **tls = (Scheme_Object **)(((void **)p->ku.k.p5)[2]);
  
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;
  p->ku.k.p5 = NULL;

  scheme_validate_expr(port, expr, stack, ht, tls,
                       args[0], args[1], args[2],
                       args[3], args[4], args[5],
                       app_rator, args[6]);

  return scheme_true;
}

int scheme_validate_rator_wants_box(Scheme_Object *app_rator, int pos,
                                    int hope,
                                    Scheme_Object **tls,
                                    int num_toplevels, int num_stxes, int num_lifts)
{
  Scheme_Closure_Data *data = NULL;

  while (1) {
    if (SAME_TYPE(SCHEME_TYPE(app_rator), scheme_closure_type)) {
      data = SCHEME_COMPILED_CLOS_CODE(app_rator);
      break;
    } else if (SAME_TYPE(SCHEME_TYPE(app_rator), scheme_unclosed_procedure_type)) {
      data = (Scheme_Closure_Data *)app_rator;
      break;
    } else if (SAME_TYPE(SCHEME_TYPE(app_rator), scheme_toplevel_type)) {
      int p;
      p = SCHEME_TOPLEVEL_POS(app_rator);
      if (p >= (num_toplevels + num_stxes + (num_stxes ? 1 : 0))) {
        /* It's a lift. Check that the lift is defined, and that it
           doesn't want reference arguments. */
        int tp;
        tp = (p - (num_stxes + (num_stxes ? 1 : 0)));
        app_rator = tls[tp];
        if (!app_rator || SCHEME_VECTORP(app_rator)) {
          /* The lift isn't ready. Record what we expect to find when it
             is ready. */
          Scheme_Object *vec = app_rator;

          if (!vec || (SCHEME_VEC_SIZE(vec) < (pos + 1))) {
            int sz;
            Scheme_Object *naya;
            if (vec)
              sz = SCHEME_VEC_SIZE(vec);
            else
              sz = 3;
            sz *= 2;
            if (sz <= pos)
              sz = pos + 1;
            naya = scheme_make_vector(sz, scheme_null);
            if (vec)
              memcpy(SCHEME_VEC_ELS(naya), SCHEME_VEC_ELS(vec), sizeof(Scheme_Object*) * SCHEME_VEC_SIZE(vec));
            vec = naya;
            tls[tp] = vec;
          }

          if (SCHEME_NULLP(SCHEME_VEC_ELS(vec)[pos])) {
            SCHEME_VEC_ELS(vec)[pos] = hope ? scheme_true : scheme_false;
            return hope;
          } else if (SCHEME_TRUEP(SCHEME_VEC_ELS(vec)[pos]))
            return 1;
          else
            return 0;
        } else if (SCHEME_FALSEP(app_rator)) {
          return 0;
        } /* else iterate */
      } else
        return 0;
    } else
      return 0;
  }

  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REF_ARGS) {
    if (pos < data->num_params) {
      int bit = ((mzshort)1 << (pos & (BITS_PER_MZSHORT - 1)));
      if (data->closure_map[data->closure_size + (pos / BITS_PER_MZSHORT)] & bit)
        return 1;
    }
  }

  return 0;
}

static int argument_to_arity_error(Scheme_Object *app_rator, int proc_with_refs_ok)
{
  /* Since `raise-arity-error' doesn't actually apply its argument,
     it's ok to pass any procedure. In particular, the compiler generates
     calls to converted procedures. */
  return ((proc_with_refs_ok == 2)
          && SAME_OBJ(app_rator, scheme_raise_arity_error_proc));
}

void scheme_validate_expr(Mz_CPort *port, Scheme_Object *expr, 
                          char *stack, Scheme_Hash_Table *ht, Scheme_Object **tls,
			  int depth, int letlimit, int delta, 
			  int num_toplevels, int num_stxes, int num_lifts,
                          Scheme_Object *app_rator, int proc_with_refs_ok)
{
  Scheme_Type type;
  int did_one = 0;

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;
    void **pr;
    int *args;

    args = MALLOC_N_ATOMIC(int, 7);

    p->ku.k.p1 = (void *)port;
    p->ku.k.p2 = (void *)expr;
    p->ku.k.p3 = (void *)stack;
    p->ku.k.p4 = (void *)ht;

    args[0] = depth;
    args[1] = letlimit;
    args[2] = delta;
    args[3] = num_toplevels;
    args[4] = num_stxes;
    args[5] = num_lifts;
    args[6] = proc_with_refs_ok;

    pr = MALLOC_N(void*, 3);
    pr[0] = (void *)args;
    pr[1] = (void *)app_rator;
    pr[2] = (void *)tls;

    p->ku.k.p5 = (void *)pr;

    (void)scheme_handle_stack_overflow(validate_k);

    return;
  }
#endif

 top:
  if (did_one) {
    if (app_rator) {
      if (scheme_validate_rator_wants_box(app_rator, proc_with_refs_ok - 2, 0,
                                          tls, num_toplevels, num_stxes, num_lifts))
        scheme_ill_formed_code(port);
      app_rator = NULL;
    }
    proc_with_refs_ok = 0;
  } else
    did_one = 1;

  type = SCHEME_TYPE(expr);

  switch (type) {
  case scheme_toplevel_type:
    {
      int c = SCHEME_TOPLEVEL_DEPTH(expr);
      int d = c + delta;
      int p = SCHEME_TOPLEVEL_POS(expr);

      if ((c < 0) || (p < 0) || (d >= depth)
	  || (stack[d] != VALID_TOPLEVELS) 
	  || (p >= (num_toplevels + num_lifts + num_stxes + (num_stxes ? 1 : 0)))
	  || ((p >= num_toplevels) && (p < num_toplevels + num_stxes + (num_stxes ? 1 : 0))))
	scheme_ill_formed_code(port);
      
      if ((proc_with_refs_ok != 1) 
          && !argument_to_arity_error(app_rator, proc_with_refs_ok)) {
        if (p >= (num_toplevels + num_stxes + (num_stxes ? 1 : 0))) {
          /* It's a lift. Check that the lift is defined, and that it
             doesn't want reference arguments. */
          int tp;
          Scheme_Object *lift;
          tp = p - (num_stxes + (num_stxes ? 1 : 0));
          lift = tls[tp];
          if (lift) {
            if (SCHEME_VECTORP(lift)) {
              int i;
              for (i = SCHEME_VEC_SIZE(lift); i--; ) {
                if (!SCHEME_NULLP(SCHEME_VEC_ELS(lift)[i]))
                  if (SCHEME_TRUEP(SCHEME_VEC_ELS(lift)[i]))
                    scheme_ill_formed_code(port);
              }
              tls[tp] = scheme_false; /* means "no ref args anywhere" */
            } else {
              if (SAME_TYPE(SCHEME_TYPE(lift), scheme_closure_type))
                lift = (Scheme_Object *)SCHEME_COMPILED_CLOS_CODE(lift);
              if (SAME_TYPE(SCHEME_TYPE(lift), scheme_unclosed_procedure_type)) {
                Scheme_Closure_Data *data = (Scheme_Closure_Data *)lift;
                if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REF_ARGS)
                  scheme_ill_formed_code(port);
              }
            }
          } else {
            tls[tp] = scheme_false; /* means "no ref args anywhere" */
          }
        }
      }
    }
    break;
  case scheme_local_type:
    {
      int q = SCHEME_LOCAL_POS(expr);
      int p = q + delta;

      if ((q < 0) || (p >= depth))
	scheme_ill_formed_code(port);
      
      if (stack[p] != VALID_VAL) {
        if ((proc_with_refs_ok >= 2) && (stack[p] == VALID_BOX)
            && scheme_validate_rator_wants_box(app_rator, proc_with_refs_ok - 2, 1,
                                               tls, num_toplevels, num_stxes, num_lifts)) {
          /* It's ok - the function wants us to pass it a box, and
             we did. */
          app_rator = NULL;
        } else
          scheme_ill_formed_code(port);
      }
    }
    break;
  case scheme_local_unbox_type:
    {
      int q = SCHEME_LOCAL_POS(expr);
      int p = q + delta;

      if ((q < 0) || (p >= depth) || (stack[p] != VALID_BOX))
	scheme_ill_formed_code(port);
    }
    break;
  case scheme_syntax_type:
    {
      Scheme_Syntax_Validater f;
      int p = SCHEME_PINT_VAL(expr);
	
      if ((p < 0) || (p >= _COUNT_EXPD_))
	scheme_ill_formed_code(port);

      f = scheme_syntax_validaters[p];
      f((Scheme_Object *)SCHEME_IPTR_VAL(expr), port, stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts);
    }
    break;
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)expr;
      int i, n;
      
      n = app->num_args + 1;

      delta -= (n - 1);
      if (delta < 0)
	scheme_ill_formed_code(port);
      memset(stack + delta, VALID_NOT, n - 1);

      for (i = 0; i < n; i++) {
	scheme_validate_expr(port, app->args[i], stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts, 
                             i ? app->args[0] : NULL, i + 1);
      }
    }
    break;
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr;
      
      delta -= 1;
      if (delta < 0)
	scheme_ill_formed_code(port);
      stack[delta] = VALID_NOT;

      scheme_validate_expr(port, app->rator, stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts, 
                           NULL, 1);
      scheme_validate_expr(port, app->rand, stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts, 
                           app->rator, 2);
    }
    break;
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr;
      
      delta -= 2;
      if (delta < 0)
	scheme_ill_formed_code(port);
      stack[delta] = VALID_NOT;
      stack[delta+1] = VALID_NOT;

      scheme_validate_expr(port, app->rator, stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts, 
                           NULL, 1);
      scheme_validate_expr(port, app->rand1, stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts, 
                           app->rator, 2);
      scheme_validate_expr(port, app->rand2, stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts, 
                           app->rator, 3);
    }
    break;
  case scheme_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)expr;
      int cnt;
      int i;
      
      cnt = seq->count;
	  
      for (i = 0; i < cnt - 1; i++) {
	scheme_validate_expr(port, seq->array[i], stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts,
                             NULL, 0);
      }

      expr = seq->array[cnt - 1];
      goto top;
    }
    break;
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b;
      b = (Scheme_Branch_Rec *)expr;
      scheme_validate_expr(port, b->test, stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts,
                           NULL, 0);
      /* This is where letlimit is useful. It prevents let-assignment in the
	 "then" branch that could permit bad code in the "else" branch (or the
	 same thing with either branch affecting later code in a sequence). */
      letlimit = delta;
      scheme_validate_expr(port, b->tbranch, stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts,
                           NULL, 0);
      expr = b->fbranch;
      goto top;
    }
    break;
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)expr;
      
      scheme_validate_expr(port, wcm->key, stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts,
                           NULL, 0);
      scheme_validate_expr(port, wcm->val, stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts,
                           NULL, 0);
      expr = wcm->body;
      goto top;
    }
    break;
  case scheme_quote_syntax_type:
    {
      Scheme_Quote_Syntax *qs = (Scheme_Quote_Syntax *)expr;
      int c = qs->depth;
      int i = qs->position;
      int p = qs->midpoint;
      int d = c + delta;

      if ((c < 0) || (p < 0) || (d >= depth)
	  || (stack[d] != VALID_TOPLEVELS) 
	  || (p != num_toplevels)
	  || (i >= num_stxes))
	scheme_ill_formed_code(port);
    }
    break;
  case scheme_unclosed_procedure_type:
    {
      Scheme_Closure_Data *data = (Scheme_Closure_Data *)expr;
      int i, cnt, q, p, sz, base, vld;
      mzshort *map;
      char *new_stack;

      if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REF_ARGS) {
        if ((proc_with_refs_ok != 1)
            && !argument_to_arity_error(app_rator, proc_with_refs_ok))
          scheme_ill_formed_code(port);
      }
      
      sz = data->max_let_depth;
      map = data->closure_map;

      new_stack = scheme_malloc_atomic(sz);

      cnt = data->num_params;
      base = sz - cnt;
      for (i = 0; i < cnt; i++) {
        vld = VALID_VAL;
        if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REF_ARGS) {
          int bit = ((mzshort)1 << (i & (BITS_PER_MZSHORT - 1)));
          if (map[data->closure_size + (i / BITS_PER_MZSHORT)] & bit)
            vld = VALID_BOX;
        }
	new_stack[i + base] = vld;
      }

      cnt = data->closure_size;
      base = base - cnt;

      for (i = 0; i < cnt; i++) {
	q = map[i];
	p = q + delta;
	if ((q < 0) || (p > depth) || (stack[p] == VALID_NOT))
	  scheme_ill_formed_code(port);
	new_stack[i + base] = stack[p];
      }

      scheme_validate_expr(port, data->code, new_stack, ht, tls, sz, sz, base, num_toplevels, num_stxes, num_lifts,
                           NULL, 0);
    }
    break;
  case scheme_let_value_type:
    {
      Scheme_Let_Value *lv = (Scheme_Let_Value *)expr;
      int q, p, c, i;

      scheme_validate_expr(port, lv->value, stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts,
                           NULL, 0);
      memset(stack, VALID_NOT, delta);

      c = lv->count;
      q = lv->position;
      p = q + delta;

      for (i = 0; i < c; i++, p++) {
	if ((q < 0) 
	    || (SCHEME_LET_AUTOBOX(lv) && ((p >= depth)
					   || (stack[p] != VALID_BOX)))
	    || (!SCHEME_LET_AUTOBOX(lv) && ((p >= letlimit)
					    || ((stack[p] != VALID_VAL) && (stack[p] != VALID_NOT)))))
	  scheme_ill_formed_code(port);

	if (!SCHEME_LET_AUTOBOX(lv)) {
	  stack[p] = VALID_VAL;
	}
      }

      expr = lv->body;
      goto top;
    }
    break;
  case scheme_let_void_type:
    {
      Scheme_Let_Void *lv = (Scheme_Let_Void *)expr;
      int c, i;

      c = lv->count;

      if ((c < 0) || (c > delta))
	scheme_ill_formed_code(port);

      if (SCHEME_LET_AUTOBOX(lv)) {
	for (i = 0; i < c; i++) {
	  stack[--delta] = VALID_BOX;
	}
      } else {
	delta -= c;
	memset(stack + delta, VALID_NOT, c);
      }


      expr = lv->body;
      goto top;
    }
    break;
  case scheme_letrec_type:
    {
      Scheme_Letrec *l = (Scheme_Letrec *)expr;
      int i, c;

      c = l->count;
      
      if ((c < 0) || (c + delta > depth))
	scheme_ill_formed_code(port);

      for (i = 0; i < c; i++) {
	if (!SAME_TYPE(SCHEME_TYPE(l->procs[i]), scheme_unclosed_procedure_type))
	  scheme_ill_formed_code(port);
      }

      for (i = 0; i < c; i++) {
	stack[delta + i] = VALID_VAL;
      }

      for (i = 0; i < c; i++) {
	scheme_validate_expr(port, l->procs[i], stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts,
                             NULL, 0);
      }

      expr = l->body;
      goto top;
    }
    break;
  case scheme_let_one_type:
    {
      Scheme_Let_One *lo = (Scheme_Let_One *)expr;

      --delta;
      if (delta < 0)
	scheme_ill_formed_code(port);
      stack[delta] = VALID_NOT;

      scheme_validate_expr(port, lo->value, stack, ht, tls, depth, letlimit, delta, num_toplevels, num_stxes, num_lifts,
                           NULL, 0);
      stack[delta] = VALID_VAL;

      expr = lo->body;
      goto top;
    }
    break;
  default:
    /* All values are definitely ok, except pre-closed closures: */
    if (SAME_TYPE(type, scheme_closure_type)) {
      if (scheme_hash_get(ht, expr)) {
        /* Done with this one. */
      } else {
        scheme_hash_set(ht, expr, scheme_true);
        expr = (Scheme_Object *)SCHEME_COMPILED_CLOS_CODE(expr);
        did_one = 0;
        goto top;
      }
    }
    break;
  }

  if (app_rator)
    if (scheme_validate_rator_wants_box(app_rator, proc_with_refs_ok - 2, 0,
                                        tls, num_toplevels, num_stxes, num_lifts))
      scheme_ill_formed_code(port);
}

void scheme_validate_toplevel(Scheme_Object *expr, Mz_CPort *port,
			      char *stack, Scheme_Hash_Table *ht, Scheme_Object **tls,
                              int depth, int delta, 
			      int num_toplevels, int num_stxes, int num_lifts,
                              int skip_refs_check)
{
  if (!SAME_TYPE(scheme_toplevel_type, SCHEME_TYPE(expr)))
    scheme_ill_formed_code(port);

  scheme_validate_expr(port, expr, stack, ht, tls, 
                       depth, delta, delta, 
                       num_toplevels, num_stxes, num_lifts,
                       NULL, skip_refs_check ? 1 : 0);
}

void scheme_validate_boxenv(int p, Mz_CPort *port, char *stack, int depth, int delta)
{
  p += delta;

  if ((p < 0) || (p >= depth) || (stack[p] != VALID_VAL))
    scheme_ill_formed_code(port);

  stack[p] = VALID_BOX;
}

/*========================================================================*/
/*       [un]marshalling application, branch, sequence, wcm bytecode      */
/*========================================================================*/

#define BOOL(x) (x ? scheme_true : scheme_false)

static Scheme_Object *write_application(Scheme_Object *obj)
{
  scheme_signal_error("app writer shouldn't be used");
  return NULL;
}

static Scheme_Object *read_application(Scheme_Object *obj)
{
  return NULL;
}

static Scheme_Object *write_sequence(Scheme_Object *obj)
{
  Scheme_Object *l;
  int i;

  i = ((Scheme_Sequence *)obj)->count;

  l = scheme_null;
  for (; i--; ) {
    l = cons(scheme_protect_quote(((Scheme_Sequence *)obj)->array[i]), l);
  }
  
  return l;
}

static Scheme_Object *read_sequence(Scheme_Object *obj)
{
  return scheme_make_sequence_compilation(obj, 1);
}

static Scheme_Object *read_sequence_save_first(Scheme_Object *obj)
{
  return scheme_make_sequence_compilation(obj, -1);
}

static Scheme_Object *write_branch(Scheme_Object *obj)
{
  scheme_signal_error("branch writer shouldn't be used");
  return NULL;
}

static Scheme_Object *read_branch(Scheme_Object *obj)
{
  return NULL;
}

static Scheme_Object *write_with_cont_mark(Scheme_Object *obj)
{
  Scheme_With_Continuation_Mark *wcm;

  wcm = (Scheme_With_Continuation_Mark *)obj;

  return cons(scheme_protect_quote(wcm->key),
	      cons(scheme_protect_quote(wcm->val),
		   scheme_protect_quote(wcm->body)));
}

static Scheme_Object *read_with_cont_mark(Scheme_Object *obj)
{
  Scheme_With_Continuation_Mark *wcm;

  if (!SCHEME_PAIRP(obj) || !SCHEME_PAIRP(SCHEME_CDR(obj)))
    return NULL; /* bad .zo */

  wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  wcm->so.type = scheme_with_cont_mark_type;
  wcm->key = SCHEME_CAR(obj);
  wcm->val = SCHEME_CADR(obj);
  wcm->body = SCHEME_CDR(SCHEME_CDR(obj));

  return (Scheme_Object *)wcm;
}

static Scheme_Object *write_syntax(Scheme_Object *obj)
{
  Scheme_Object *idx, *rest, *l;
  int protect_after, c;

  c = SCHEME_PINT_VAL(obj);
  idx = scheme_make_integer(c);
  protect_after = scheme_syntax_protect_afters[c];

  l = rest = (Scheme_Object *)SCHEME_IPTR_VAL(obj);
  if (protect_after == -2) {
    /* -2 => protect first element of vector */
    if (SCHEME_VECTORP(l)) {
      l = scheme_protect_quote(SCHEME_VEC_ELS(rest)[0]);
      if (!SAME_OBJ(l, SCHEME_VEC_ELS(rest)[0])) {
        Scheme_Object *vec;
        long i, len;
        len = SCHEME_VEC_SIZE(rest);
        vec = scheme_make_vector(len, NULL);
        SCHEME_VEC_ELS(vec)[0] = l;
        for (i = 1; i < len; i++) {
          SCHEME_VEC_ELS(vec)[i] = SCHEME_VEC_ELS(rest)[i];
        }
        rest = vec;
      }
    } else {
      scheme_signal_error("expected a vector for syntax");
    }
  } else {
    for (c = 0; SCHEME_PAIRP(l) && (c < protect_after); c++) {
      l = SCHEME_CDR(l);
    }
    if (!SCHEME_NULLP(l) && (c == protect_after)) {
      Scheme_Object *new_l;

      new_l = scheme_protect_quote(l);

      if (new_l != l) {
        Scheme_Object *first = NULL, *last = NULL;
      
        while (rest != l) {
          Scheme_Object *p;
	
          p = scheme_make_pair(SCHEME_CAR(rest), scheme_null);
          if (last)
            SCHEME_CDR(last) = p;
          else
            first = p;
          last = p;

          rest = SCHEME_CDR(rest);
        }
      
        if (last)
          SCHEME_CDR(last) = new_l;
        else
          first = new_l;
      
        rest = first;
      }
    }
  }

  return cons(idx, rest);
}

static Scheme_Object *read_syntax(Scheme_Object *obj)
{
  Scheme_Object *idx;
  Scheme_Object *first = NULL, *last = NULL;
  int limit;

  if (!SCHEME_PAIRP(obj) || !SCHEME_INTP(SCHEME_CAR(obj)))
    return NULL; /* bad .zo */

  idx = SCHEME_CAR(obj);

  /* Copy obj, up to number of cons cells before a "protected" value: */
  limit = scheme_syntax_protect_afters[SCHEME_INT_VAL(idx)];
  obj = SCHEME_CDR(obj);
  while (SCHEME_PAIRP(obj) && (limit > 0)) {
    Scheme_Object *p;
    p = scheme_make_pair(SCHEME_CAR(obj), scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;
    obj = SCHEME_CDR(obj);
    limit--;
  }
  
  if (last)
    SCHEME_CDR(last) = obj;
  else
    first = obj;

  return scheme_make_syntax_resolved(SCHEME_INT_VAL(idx), first);
}

static Scheme_Object *write_quote_syntax(Scheme_Object *obj)
{
  Scheme_Quote_Syntax *qs = (Scheme_Quote_Syntax *)obj;

  return cons(scheme_make_integer(qs->depth),
	      cons(scheme_make_integer(qs->position),
		   scheme_make_integer(qs->midpoint)));
}

static Scheme_Object *read_quote_syntax(Scheme_Object *obj)
{
  Scheme_Quote_Syntax *qs;
  Scheme_Object *a;
  int c, i, p;
  
  if (!SCHEME_PAIRP(obj)) return NULL;

  a = SCHEME_CAR(obj);
  c = SCHEME_INT_VAL(a);

  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  
  a = SCHEME_CAR(obj);
  i = SCHEME_INT_VAL(a);

  a = SCHEME_CDR(obj);
  p = SCHEME_INT_VAL(a);

  qs = MALLOC_ONE_TAGGED(Scheme_Quote_Syntax);
  qs->so.type = scheme_quote_syntax_type;
  qs->depth = c;
  qs->position = i;
  qs->midpoint = p;  

  return (Scheme_Object *)qs;
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_EVAL_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_compile_info, mark_comp_info);
  GC_REG_TRAV(scheme_rt_saved_stack, mark_saved_stack);
}

END_XFORM_SKIP;

#endif
