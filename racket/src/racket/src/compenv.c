/*
  Racket
  Copyright (c) 2004-2013 PLT Design Inc.
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

#include "schpriv.h"
#include "schexpobs.h"

#define TABLE_CACHE_MAX_SIZE 2048

READ_ONLY static Scheme_Object *scheme_local[MAX_CONST_LOCAL_POS][MAX_CONST_LOCAL_TYPES][MAX_CONST_LOCAL_FLAG_VAL + 1];
READ_ONLY static Scheme_Object *toplevels[MAX_CONST_TOPLEVEL_DEPTH][MAX_CONST_TOPLEVEL_POS][SCHEME_TOPLEVEL_FLAGS_MASK + 1];

READ_ONLY static Scheme_Object *unshadowable_symbol;

/* If locked, these are probably sharable: */
THREAD_LOCAL_DECL(static Scheme_Hash_Table *toplevels_ht);
THREAD_LOCAL_DECL(static Scheme_Hash_Table *locals_ht[2]);
THREAD_LOCAL_DECL(static int env_uid_counter);

#define ARBITRARY_USE     0x1
#define CONSTRAINED_USE   0x2
#define WAS_SET_BANGED    0x4
#define ONE_ARBITRARY_USE 0x8
/* See also SCHEME_USE_COUNT_MASK */

typedef struct Compile_Data {
  int num_const;
  Scheme_Object **const_names;
  Scheme_Object **const_vals;
  Scheme_Object **const_uids;
  int *sealed; /* NULL => already sealed */
  int *use;
  Scheme_Object *lifts;
  int min_use, any_use;
} Compile_Data;

typedef struct Scheme_Full_Comp_Env {
  Scheme_Comp_Env base;
  Compile_Data data;
} Scheme_Full_Comp_Env;

static void init_compile_data(Scheme_Comp_Env *env);

/* Precise GC WARNING: this macro produces unaligned pointers: */
#define COMPILE_DATA(e) (&((Scheme_Full_Comp_Env *)e)->data)

#define SCHEME_NON_SIMPLE_FRAME (SCHEME_NO_RENAME | SCHEME_CAPTURE_WITHOUT_RENAME \
                                 | SCHEME_FOR_STOPS | SCHEME_CAPTURE_LIFTED)

static void init_scheme_local();
static void init_toplevels();

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

void scheme_init_compenv()
{
  init_scheme_local();
  init_toplevels();

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

void scheme_init_compenv_places(void)
{
  REGISTER_SO(toplevels_ht);
  REGISTER_SO(locals_ht[0]);
  REGISTER_SO(locals_ht[1]);

  {
    Scheme_Hash_Table *ht;
    toplevels_ht = scheme_make_hash_table_equal();
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    locals_ht[0] = ht;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    locals_ht[1] = ht;
  }
}

void scheme_init_compenv_symbol(void)
{
  REGISTER_SO(unshadowable_symbol);
  unshadowable_symbol = scheme_intern_symbol("unshadowable");
}

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
    /* should be always NULL */
    dest[i].observer = src[drec].observer;
    dest[i].pre_unwrapped = 0;
    dest[i].testing_constantness = 0;
    dest[i].env_already = 0;
    dest[i].comp_flags = src[drec].comp_flags;
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
    dest[i].observer = src[drec].observer;
    dest[i].pre_unwrapped = 0;
    dest[i].testing_constantness = 0;
    dest[i].env_already = 0;
    dest[i].comp_flags = src[drec].comp_flags;
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
  lam[dlrec].observer = src[drec].observer;
  lam[dlrec].pre_unwrapped = 0;
  lam[dlrec].testing_constantness = 0;
  lam[dlrec].env_already = 0;
  lam[dlrec].comp_flags = src[drec].comp_flags;
}

void scheme_merge_lambda_rec(Scheme_Compile_Info *src, int drec,
			     Scheme_Compile_Info *lam, int dlrec)
{
}

void scheme_compile_rec_done_local(Scheme_Compile_Info *rec, int drec)
{
  rec[drec].value_name = scheme_false;
}

/**********************************************************************/
/*                        expansion observer                          */
/**********************************************************************/

/* RMC
 * - Defines #%expobs module
 *   - current-expand-observe
 *   - ??? (other syntax observations)
 */

void scheme_call_expand_observe(Scheme_Object *obs, int tag, Scheme_Object *obj) 
{
  if (!SCHEME_PROCP(obs)) {
    scheme_signal_error("internal error: expand-observer should never be non-procedure");
  } else {
    Scheme_Object *buf[2];
    buf[0] = scheme_make_integer(tag);
    if (obj) {
      buf[1] = obj;
    } else {
      buf[1] = scheme_false;
    }
    scheme_apply(obs, 2, buf);
  }
}

static Scheme_Object *
current_expand_observe(int argc, Scheme_Object **argv)
{
  return scheme_param_config("current-expand-observe",
			     scheme_make_integer(MZCONFIG_EXPAND_OBSERVE),
			     argc, argv,
			     2, NULL, NULL, 0);
}

/* always returns either procedure or NULL */
Scheme_Object *scheme_get_expand_observe() 
{
  Scheme_Object *obs;
  obs = scheme_get_param(scheme_current_config(),
                         MZCONFIG_EXPAND_OBSERVE);
  if (SCHEME_PROCP(obs)) {
    return obs;
  } else {
    return NULL;
  }
}

void scheme_init_expand_observe(Scheme_Env *env) 
{
  Scheme_Env *newenv;
  Scheme_Object *modname;

  modname = scheme_intern_symbol("#%expobs");
  newenv = scheme_primitive_module(modname, env);

  scheme_add_global_constant
    ("current-expand-observe",
     scheme_register_parameter(current_expand_observe,
                               "current-expand-observe",
                               MZCONFIG_EXPAND_OBSERVE),
     newenv);
  scheme_finish_primitive_module(newenv);
}

/*========================================================================*/
/*        compile-time env, constructors and simple queries               */
/*========================================================================*/

static void init_compile_data(Scheme_Comp_Env *env)
{
  Compile_Data *data;
  int i, c, *use;

  c = env->num_bindings;
  if (c)
    use = MALLOC_N_ATOMIC(int, c);
  else
    use = NULL;

  data = COMPILE_DATA(env);

  data->use = use;
  for (i = 0; i < c; i++) {
    use[i] = 0;
  }

  data->min_use = c;
}

Scheme_Comp_Env *scheme_new_compilation_frame(int num_bindings, int flags, Scheme_Comp_Env *base)
{
  Scheme_Comp_Env *frame;
  int count;
  
  count = num_bindings;

  frame = (Scheme_Comp_Env *)MALLOC_ONE_RT(Scheme_Full_Comp_Env);
#ifdef MZTAG_REQUIRED
  frame->type = scheme_rt_comp_env;
#endif

  {
    Scheme_Object **vals;
    vals = MALLOC_N(Scheme_Object *, count);
    frame->values = vals;
  }

  frame->num_bindings = num_bindings;
  frame->flags = flags | (base->flags & SCHEME_NO_RENAME);
  frame->next = base;
  frame->genv = base->genv;
  frame->insp = base->insp;
  frame->prefix = base->prefix;
  frame->in_modidx = base->in_modidx;

  if (flags & SCHEME_NON_SIMPLE_FRAME)
    frame->skip_depth = 0;
  else if (base->next)
    frame->skip_depth = base->skip_depth + 1;
  else
    frame->skip_depth = 0;

  init_compile_data(frame);

  return frame;
}

Scheme_Comp_Env *scheme_new_comp_env(Scheme_Env *genv, Scheme_Object *insp, int flags)
{
  Scheme_Comp_Env *e;
  Comp_Prefix *cp;

  if (!insp)
    insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);

  e = (Scheme_Comp_Env *)MALLOC_ONE_RT(Scheme_Full_Comp_Env);
#ifdef MZTAG_REQUIRED
  e->type = scheme_rt_comp_env;
#endif
  e->num_bindings = 0;
  e->next = NULL;
  e->genv = genv;
  e->insp = insp;
  e->flags = flags;
  init_compile_data(e);

  cp = MALLOC_ONE_RT(Comp_Prefix);
#ifdef MZTAG_REQUIRED
  cp->type = scheme_rt_comp_prefix;
#endif

  e->prefix = cp;

  return e;
}

Scheme_Comp_Env *scheme_new_expand_env(Scheme_Env *genv, Scheme_Object *insp, int flags)
{
  Scheme_Comp_Env *e;

  e = scheme_new_comp_env(genv, insp, flags);
  e->prefix = NULL;

  return e;
}

int scheme_is_sub_env(Scheme_Comp_Env *stx_env, Scheme_Comp_Env *env)
{
  Scheme_Comp_Env *se;

  for (se = stx_env; NOT_SAME_OBJ(se, env); se = se->next) {
    if (!(se->flags & SCHEME_FOR_INTDEF))
      break;
  }
  return SAME_OBJ(se, env);
}

int scheme_used_ever(Scheme_Comp_Env *env, int which)
{
  Compile_Data *data = COMPILE_DATA(env);

  return !!data->use[which];
}

int scheme_is_env_variable_boxed(Scheme_Comp_Env *env, int which)
{
  Compile_Data *data = COMPILE_DATA(env);

  return !!(data->use[which] & WAS_SET_BANGED);
}

void
scheme_add_compilation_binding(int index, Scheme_Object *val, Scheme_Comp_Env *frame)
{
  if ((index >= frame->num_bindings) || (index < 0))
    scheme_signal_error("internal error: scheme_add_binding: "
			"index out of range: %d", index);
  
  frame->values[index] = val;
  frame->skip_table = NULL;
}

void scheme_frame_captures_lifts(Scheme_Comp_Env *env, Scheme_Lift_Capture_Proc cp, Scheme_Object *data, 
                                 Scheme_Object *end_stmts, Scheme_Object *context_key, 
                                 Scheme_Object *requires, Scheme_Object *provides)
{
  Scheme_Lift_Capture_Proc *pp;
  Scheme_Object *vec;
  
  pp = (Scheme_Lift_Capture_Proc *)scheme_malloc_atomic(sizeof(Scheme_Lift_Capture_Proc));
  *pp = cp;

  vec = scheme_make_vector(8, NULL);
  SCHEME_VEC_ELS(vec)[0] = scheme_null;
  SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)pp;
  SCHEME_VEC_ELS(vec)[2] = data;
  SCHEME_VEC_ELS(vec)[3] = end_stmts;
  SCHEME_VEC_ELS(vec)[4] = context_key;
  SCHEME_VEC_ELS(vec)[5] = (requires ? requires : scheme_false);
  SCHEME_VEC_ELS(vec)[6] = scheme_null; /* accumulated requires */
  SCHEME_VEC_ELS(vec)[7] = provides;

  COMPILE_DATA(env)->lifts = vec;
}

void scheme_propagate_require_lift_capture(Scheme_Comp_Env *orig_env, Scheme_Comp_Env *env)
{
  while (orig_env) {
    if ((COMPILE_DATA(orig_env)->lifts)
        && SCHEME_TRUEP(SCHEME_VEC_ELS(COMPILE_DATA(orig_env)->lifts)[5]))
      break;
    orig_env = orig_env->next;
  }
  
  if (orig_env) {
    Scheme_Object *vec, *p;

    p = scheme_make_raw_pair(NULL, (Scheme_Object *)orig_env);

    vec = scheme_make_vector(8, NULL);
    SCHEME_VEC_ELS(vec)[0] = scheme_false;
    SCHEME_VEC_ELS(vec)[1] = scheme_void;
    SCHEME_VEC_ELS(vec)[2] = scheme_void;
    SCHEME_VEC_ELS(vec)[3] = scheme_false;
    SCHEME_VEC_ELS(vec)[4] = scheme_false;
    SCHEME_VEC_ELS(vec)[5] = p; /* (rcons NULL env) => continue with env */
    SCHEME_VEC_ELS(vec)[6] = scheme_null;
    SCHEME_VEC_ELS(vec)[7] = scheme_false;

    COMPILE_DATA(env)->lifts = vec;
  }
}

Scheme_Object *scheme_frame_get_lifts(Scheme_Comp_Env *env)
{
  return scheme_reverse(SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[0]);
}

Scheme_Object *scheme_frame_get_end_statement_lifts(Scheme_Comp_Env *env)
{
  return SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[3];
}

Scheme_Object *scheme_frame_get_require_lifts(Scheme_Comp_Env *env)
{
  return SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[6];
}

Scheme_Object *scheme_frame_get_provide_lifts(Scheme_Comp_Env *env)
{
  return SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[7];
}

void scheme_add_local_syntax(int cnt, Scheme_Comp_Env *env)
{
  Scheme_Object **ns, **vs;
  
  if (cnt) {
    ns = MALLOC_N(Scheme_Object *, cnt);
    vs = MALLOC_N(Scheme_Object *, cnt);

    COMPILE_DATA(env)->num_const = cnt;
    COMPILE_DATA(env)->const_names = ns;
    COMPILE_DATA(env)->const_vals = vs;

  }
}

void scheme_set_local_syntax(int pos,
			     Scheme_Object *name, Scheme_Object *val,
			     Scheme_Comp_Env *env)
{
  COMPILE_DATA(env)->const_names[pos] = name;
  COMPILE_DATA(env)->const_vals[pos] = val;
  env->skip_table = NULL;
}

Scheme_Comp_Env *
scheme_add_compilation_frame(Scheme_Object *vals, Scheme_Comp_Env *env, int flags)
{
  Scheme_Comp_Env *frame;
  int len, i, count;
  
  len = scheme_stx_list_length(vals);
  count = len;

  frame = scheme_new_compilation_frame(count, flags, env);

  for (i = 0; i < len ; i++) {
    if (SCHEME_STX_SYMBOLP(vals))
      frame->values[i] = vals;
    else {
      Scheme_Object *a;
      a = SCHEME_STX_CAR(vals);
      frame->values[i] = a;
      vals = SCHEME_STX_CDR(vals);
    }
  }
  
  init_compile_data(frame);

  return frame;
}

Scheme_Comp_Env *scheme_no_defines(Scheme_Comp_Env *env)
{
  if (scheme_is_toplevel(env)
      || scheme_is_module_env(env)
      || scheme_is_module_begin_env(env)
      || (env->flags & SCHEME_INTDEF_FRAME))
    return scheme_new_compilation_frame(0, 0, env);
  else
    return env;
}

Scheme_Comp_Env *scheme_require_renames(Scheme_Comp_Env *env)
{
  if (env->flags & SCHEME_NO_RENAME) {
    env = scheme_new_compilation_frame(0, 0, env);
    env->flags -= SCHEME_NO_RENAME;
  }

  return env;
}

int scheme_is_toplevel(Scheme_Comp_Env *env)
{
  return !env->next || (env->flags & SCHEME_TOPLEVEL_FRAME);
}

int scheme_is_nested_module(Scheme_Comp_Env *env)
{
  return (env->flags & SCHEME_NESTED_MODULE_FRAME);
}

int scheme_is_module_env(Scheme_Comp_Env *env)
{
  return !!(env->flags & SCHEME_MODULE_BEGIN_FRAME); /* name is backwards compared to symbol! */
}

int scheme_is_module_begin_env(Scheme_Comp_Env *env)
{
  return !!(env->flags & SCHEME_MODULE_FRAME); /* name is backwards compared to symbol! */
}

Scheme_Comp_Env *scheme_extend_as_toplevel(Scheme_Comp_Env *env)
{
  if (scheme_is_toplevel(env))
    return env;
  else
    return scheme_new_compilation_frame(0, SCHEME_TOPLEVEL_FRAME, env);
}

Scheme_Object *scheme_make_toplevel(mzshort depth, int position, int resolved, int flags)
{
  Scheme_Toplevel *tl;
  Scheme_Object *v, *pr;

  /* Important: non-resolved can't be cached, because the ISCONST
     field is modified to track mutated module-level variables. But
     the value for a specific toplevel is cached in the environment
     layer. */

  if (resolved) {
    if ((depth < MAX_CONST_TOPLEVEL_DEPTH)
	&& (position < MAX_CONST_TOPLEVEL_POS))
      return toplevels[depth][position][flags];

    if ((position < 0xFFFF) && (depth < 0xFF)) {
      int ep = position | (depth << 16) | (flags << 24);
      pr = scheme_make_integer(ep);
    } else {
      pr = scheme_make_vector(3, NULL);
      SCHEME_VEC_ELS(pr)[0] = scheme_make_integer(position);
      SCHEME_VEC_ELS(pr)[1] = scheme_make_integer(flags);
      SCHEME_VEC_ELS(pr)[2] = scheme_make_integer(depth);
    }
    v = scheme_hash_get_atomic(toplevels_ht, pr);
    if (v)
      return v;
  } else
    pr = NULL;

  tl = (Scheme_Toplevel *)scheme_malloc_atomic_tagged(sizeof(Scheme_Toplevel));
  tl->iso.so.type = (resolved ? scheme_toplevel_type : scheme_compiled_toplevel_type);
  tl->depth = depth;
  tl->position = position;
  SCHEME_TOPLEVEL_FLAGS(tl) = flags;

  if (resolved) {
    if (toplevels_ht->count > TABLE_CACHE_MAX_SIZE) {
      toplevels_ht = scheme_make_hash_table_equal();
    }
    scheme_hash_set_atomic(toplevels_ht, pr, (Scheme_Object *)tl);
  }

  return (Scheme_Object *)tl;
}

Scheme_Object *scheme_register_toplevel_in_comp_prefix(Scheme_Object *var, Comp_Prefix *cp,
                                                       int imported, Scheme_Object *inline_variant)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *o;

  ht = cp->toplevels;
  if (!ht) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    cp->toplevels = ht;
  }

  o = scheme_hash_get(ht, var);
  if (o)
    return o;

  o = scheme_make_toplevel(0, cp->num_toplevels, 0, 
                           (imported 
                            ? ((SCHEME_MODVAR_FLAGS(var) & SCHEME_MODVAR_CONST)
                               ? SCHEME_TOPLEVEL_CONST
                               : ((SCHEME_MODVAR_FLAGS(var) & SCHEME_MODVAR_FIXED)
                                  ? SCHEME_TOPLEVEL_FIXED
                                  : SCHEME_TOPLEVEL_READY))
                            : 0));

  scheme_hash_set(ht, var, o);

  if (inline_variant) {
    ht = cp->inline_variants;
    if (!ht) {
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      cp->inline_variants = ht;
    }
    scheme_hash_set(ht, scheme_make_integer(cp->num_toplevels), inline_variant);
  }
  
  cp->num_toplevels++;

  return o;
}

Scheme_Object *scheme_register_toplevel_in_prefix(Scheme_Object *var, Scheme_Comp_Env *env,
						  Scheme_Compile_Info *rec, int drec,
                                                  int imported, Scheme_Object *inline_variant)
{
  Comp_Prefix *cp = env->prefix;

  if (rec && rec[drec].dont_mark_local_use) {
    /* Make up anything; it's going to be ignored. */
    return scheme_make_toplevel(0, 0, 0, 0);
  }

  return scheme_register_toplevel_in_comp_prefix(var, cp, imported, inline_variant);
}

void scheme_register_unbound_toplevel(Scheme_Comp_Env *env, Scheme_Object *id)
{
  Comp_Prefix *cp = env->prefix;

  if (!cp->unbound) cp->unbound = scheme_null;

  id = scheme_make_pair(id, cp->unbound);
  cp->unbound = id;
}

void scheme_merge_undefineds(Scheme_Comp_Env *exp_env, Scheme_Comp_Env *env)
{
  if (exp_env->prefix->unbound && (env->genv->disallow_unbound < 0)) {
    /* adding a list to env->prefix->unbound indicates a
       phase-1 shift for the identifiers in the list: */
    scheme_register_unbound_toplevel(env, exp_env->prefix->unbound);
  }
}

Scheme_Object *scheme_toplevel_to_flagged_toplevel(Scheme_Object *_tl, int flags)
{
  Scheme_Toplevel *tl = (Scheme_Toplevel *)_tl;
  return scheme_make_toplevel(tl->depth, tl->position, 0, flags);
}

Scheme_Object *scheme_register_stx_in_prefix(Scheme_Object *var, Scheme_Comp_Env *env, 
					     Scheme_Compile_Info *rec, int drec)
{
  Comp_Prefix *cp = env->prefix;
  Scheme_Local *l;
  Scheme_Object *o;
  int pos;

  if (rec && rec[drec].dont_mark_local_use) {
    /* Make up anything; it's going to be ignored. */
    l = (Scheme_Local *)scheme_malloc_atomic_tagged(sizeof(Scheme_Local));
    l->iso.so.type = scheme_compiled_quote_syntax_type;
    l->position = 0;

    return (Scheme_Object *)l;
  }

  if (!cp->stxes) {
    Scheme_Hash_Table *ht;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    cp->stxes = ht;
  }

  pos = cp->num_stxes;

  l = (Scheme_Local *)scheme_malloc_atomic_tagged(sizeof(Scheme_Local));
  l->iso.so.type = scheme_compiled_quote_syntax_type;
  l->position = pos;

  cp->num_stxes++;
  o = (Scheme_Object *)l;
  
  scheme_hash_set(cp->stxes, var, o);

  return o;
}

void scheme_register_unsafe_in_prefix(Scheme_Comp_Env *env, 
                                      Scheme_Compile_Info *rec, int drec,
                                      Scheme_Env *menv)
{
  Scheme_Object *v, *insp;

  if (rec && rec[drec].dont_mark_local_use) {
    return;
  }

  insp = menv->module->insp;

  v = env->prefix->uses_unsafe;
  if (!v)
    v = insp;
  else if (!SAME_OBJ(v, insp)) {
    Scheme_Hash_Tree *ht;

    if (SCHEME_HASHTRP(v)) {
      ht = (Scheme_Hash_Tree *)v;
    } else {
      ht = scheme_make_hash_tree(0);
      ht = scheme_hash_tree_set(ht, v, scheme_true);
    }
    
    if (!scheme_hash_tree_get(ht, insp)) {
      ht = scheme_hash_tree_set(ht, insp, scheme_true);
      env->prefix->uses_unsafe = (Scheme_Object *)ht;
    }
  }
}

/*========================================================================*/
/*                     compile-time env, lookup bindings                  */
/*========================================================================*/

static void init_scheme_local() 
{
  int i, k, cor;

#ifndef USE_TAGGED_ALLOCATION
  GC_CAN_IGNORE Scheme_Local *all;

  all = (Scheme_Local *)scheme_malloc_eternal(sizeof(Scheme_Local) 
                                              * (MAX_CONST_LOCAL_FLAG_VAL + 1)
                                              * MAX_CONST_LOCAL_TYPES
                                              * MAX_CONST_LOCAL_POS);
# ifdef MEMORY_COUNTING_ON
  scheme_misc_count += (sizeof(Scheme_Local) 
                        * (MAX_CONST_LOCAL_FLAG_VAL + 1)
                        * MAX_CONST_LOCAL_TYPES
                        * MAX_CONST_LOCAL_POS);
# endif    
#endif

  for (i = 0; i < MAX_CONST_LOCAL_POS; i++) {
    for (k = 0; k < MAX_CONST_LOCAL_TYPES; k++) {
      for (cor = 0; cor < (MAX_CONST_LOCAL_FLAG_VAL + 1); cor++) {
        Scheme_Object *v;

#ifndef USE_TAGGED_ALLOCATION
        v = (Scheme_Object *)(all++);
#else
        v = (Scheme_Object *)scheme_malloc_eternal_tagged(sizeof(Scheme_Local));
#endif
        v->type = k + scheme_local_type;
        SCHEME_LOCAL_POS(v) = i;
        SCHEME_LOCAL_FLAGS(v) = cor;

        scheme_local[i][k][cor] = v;
      }
    }
  }
}

static void init_toplevels()
{
  int i, k, cnst;

#ifndef USE_TAGGED_ALLOCATION
  GC_CAN_IGNORE Scheme_Toplevel *all;

  all = (Scheme_Toplevel *)scheme_malloc_eternal(sizeof(Scheme_Toplevel) 
      * MAX_CONST_TOPLEVEL_DEPTH 
      * MAX_CONST_TOPLEVEL_POS
      * (SCHEME_TOPLEVEL_FLAGS_MASK + 1));
# ifdef MEMORY_COUNTING_ON
  scheme_misc_count += (sizeof(Scheme_Toplevel) 
      * MAX_CONST_TOPLEVEL_DEPTH 
      * MAX_CONST_TOPLEVEL_POS
      * (SCHEME_TOPLEVEL_FLAGS_MASK + 1));
# endif
#endif

  for (i = 0; i < MAX_CONST_TOPLEVEL_DEPTH; i++) {
    for (k = 0; k < MAX_CONST_TOPLEVEL_POS; k++) {
      for (cnst = 0; cnst <= SCHEME_TOPLEVEL_FLAGS_MASK; cnst++) {
        Scheme_Toplevel *v;

#ifndef USE_TAGGED_ALLOCATION
        v = (all++);
#else
        v = (Scheme_Toplevel *)scheme_malloc_eternal_tagged(sizeof(Scheme_Toplevel));
#endif
        v->iso.so.type = scheme_toplevel_type;
        v->depth = i;
        v->position = k;
        SCHEME_TOPLEVEL_FLAGS(v) = cnst;

        toplevels[i][k][cnst] = (Scheme_Object *)v;
      }
    }
  }
}

static Scheme_Object *alloc_local(short type, int pos)
{
  Scheme_Object *v;

  v = (Scheme_Object *)scheme_malloc_atomic_tagged(sizeof(Scheme_Local));
  v->type = type;
  SCHEME_LOCAL_POS(v) = pos;

  return (Scheme_Object *)v;
}

Scheme_Object *scheme_make_local(Scheme_Type type, int pos, int flags)
{
  int k;
  Scheme_Object *v, *key;

  k = type - scheme_local_type;
  
  /* Helper for reading bytecode: make sure flags is a valid value */
  if ((flags < 0) || (flags > (SCHEME_MAX_LOCAL_TYPE + SCHEME_LOCAL_TYPE_OFFSET)))
    flags = SCHEME_LOCAL_OTHER_CLEARS;

  if (pos < MAX_CONST_LOCAL_POS) {
    return scheme_local[pos][k][flags];
  }

  key = scheme_make_integer(pos);
  if (flags) {
    key = scheme_make_pair(scheme_make_integer(flags), key);
  }

  v = scheme_hash_get(locals_ht[k], key);
  if (v)
    return v;

  v = alloc_local(type, pos);
  SCHEME_LOCAL_FLAGS(v) = flags;

  if (locals_ht[k]->count > TABLE_CACHE_MAX_SIZE) {
    Scheme_Hash_Table *ht;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    locals_ht[k] = ht;
  }

  scheme_hash_set(locals_ht[k], key, v);

  return v;
}

static Scheme_Local *get_frame_loc(Scheme_Comp_Env *frame,
				   int i, int j, int p, int flags)
/* Generates a Scheme_Local record for a static distance coodinate, and also
   marks the variable as used for closures. */
{
  int cnt, u;

  u = COMPILE_DATA(frame)->use[i];
  
  u |= (((flags & (SCHEME_APP_POS | SCHEME_SETTING))
	 ? CONSTRAINED_USE
	 : ((u & (ARBITRARY_USE | ONE_ARBITRARY_USE)) ? ARBITRARY_USE : ONE_ARBITRARY_USE))
	| ((flags & (SCHEME_SETTING | SCHEME_LINKING_REF))
	   ? WAS_SET_BANGED
	   : 0));

  cnt = ((u & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT);
  if (cnt < SCHEME_USE_COUNT_INF)
    cnt++;
  u -= (u & SCHEME_USE_COUNT_MASK);
  u |= (cnt << SCHEME_USE_COUNT_SHIFT);
  
  COMPILE_DATA(frame)->use[i] = u;
  if (i < COMPILE_DATA(frame)->min_use)
    COMPILE_DATA(frame)->min_use = i;
  COMPILE_DATA(frame)->any_use = 1;

  return (Scheme_Local *)scheme_make_local(scheme_local_type, p + i, 0);
}

Scheme_Object *scheme_hash_module_variable(Scheme_Env *env, Scheme_Object *modidx, 
					   Scheme_Object *stxsym, Scheme_Object *insp,
					   int pos, intptr_t mod_phase, int is_constant,
                                           Scheme_Object *shape)
/* is_constant == 2 => constant over all instantiations and phases */
{
  Scheme_Object *val;
  Scheme_Hash_Table *ht;

  if (!env->modvars) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    env->modvars = ht;
  }

  stxsym = SCHEME_STX_SYM(stxsym);

  ht = (Scheme_Hash_Table *)scheme_hash_get(env->modvars, modidx);

  if (!ht) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    scheme_hash_set(env->modvars, modidx, (Scheme_Object *)ht);
  }

  /* Loop for inspector-specific hash table, maybe: */
  while (1) {
    
    val = scheme_hash_get(ht, stxsym);
    
    if (!val) {
      Module_Variable *mv;
      
      mv = MALLOC_ONE_TAGGED(Module_Variable);
      mv->iso.so.type = scheme_module_variable_type;
      
      mv->modidx = modidx;
      mv->sym = stxsym;
      mv->insp = insp;
      mv->pos = pos;
      mv->mod_phase = (int)mod_phase;
      mv->shape = shape;

      if (is_constant > 1)
        SCHEME_MODVAR_FLAGS(mv) |= SCHEME_MODVAR_CONST;
      else if (is_constant)
        SCHEME_MODVAR_FLAGS(mv) |= SCHEME_MODVAR_FIXED;
      
      val = (Scheme_Object *)mv;
      
      scheme_hash_set(ht, stxsym, val);
      
      break;
    } else {
      /* Check that inspector is the same. */
      Module_Variable *mv = (Module_Variable *)val;
      
      if (!SAME_OBJ(mv->insp, insp)) {
	/* Need binding for a different inspector. Try again. */
	val = scheme_hash_get(ht, insp);
	if (!val) {
	  Scheme_Hash_Table *ht2;
	  /* Make a table for this specific inspector */
	  ht2 = scheme_make_hash_table(SCHEME_hash_ptr);
	  scheme_hash_set(ht, insp, (Scheme_Object *)ht2);
	  ht = ht2;
	  /* loop... */
	} else
	  ht = (Scheme_Hash_Table *)val;
      } else
	break;
    }
  }

  return val;
}

Scheme_Object *scheme_tl_id_sym(Scheme_Env *env, Scheme_Object *id, Scheme_Object *bdg, 
                                int mode, /* -1, 0 => lookup; 2, 3 => define
                                             -1 and 3 => use temp table
                                             1 would mean define if no match; not currently used */
                                Scheme_Object *phase, int *_skipped)
/* The `env' argument can actually be a hash table. */
{
  Scheme_Object *marks = NULL, *sym, *map, *l, *a, *amarks, *m, *best_match, *cm, *abdg;
  int best_match_skipped, ms;
  Scheme_Hash_Table *marked_names, *temp_marked_names, *dest_marked_names;

  sym = SCHEME_STX_SYM(id);

  if (_skipped)
    *_skipped = -1;

  if (SCHEME_HASHTP((Scheme_Object *)env)) {
    marked_names = (Scheme_Hash_Table *)env;
    temp_marked_names = NULL;
  } else {
    /* If there's no table and we're not defining, bail out fast */
    if ((mode <= 0) && !env->rename_set)
      return sym;
    marked_names = scheme_get_module_rename_marked_names(env->rename_set,
                                                         phase ? phase : scheme_make_integer(env->phase),
                                                         0);
    temp_marked_names = env->temp_marked_names;
  }

  if (mode > 0) {
    /* If we're defining, see if we need to create a table.  Getting
       marks is relatively expensive, but we only do this once per
       definition. */
    if (!bdg)
      bdg = scheme_stx_moduleless_env(id);
    marks = scheme_stx_extract_marks(id);
    if (SCHEME_NULLP(marks) && SCHEME_FALSEP(bdg))
      return sym;
  }

  if (!marked_names) {
    scheme_prepare_env_renames(env, mzMOD_RENAME_TOPLEVEL);
    marked_names = scheme_get_module_rename_marked_names(env->rename_set,
                                                         phase ? phase : scheme_make_integer(env->phase),
                                                         1);
  }
  if (!temp_marked_names && (mode > 2)) {
    /* The "temp" marked name table is used to correlate marked module
       requires with similarly marked provides. We don't go through
       the normal rename table because (for efficiency) the marks in
       this case are handled more directly in the shared_pes module
       renamings. */
    temp_marked_names = scheme_make_hash_table(SCHEME_hash_ptr);
    env->temp_marked_names = temp_marked_names;
  }
  
  map = scheme_hash_get(marked_names, sym);
  if (!map && ((mode < 0) || (mode > 2)) && temp_marked_names)
    map = scheme_hash_get(temp_marked_names, sym);

  if (!map) {
    /* If we're not defining, we can bail out before extracting marks. */
    if (mode <= 0)
      return sym;
    else
      map = scheme_null;
  }

  if (!bdg) {
    /* We need lexical binding, if any, too: */
    bdg = scheme_stx_moduleless_env(id);
  }

  if (!marks) {
    /* We really do need the marks. Get them. */
    marks = scheme_stx_extract_marks(id);
    if (SCHEME_NULLP(marks) && SCHEME_FALSEP(bdg))
      return sym;
  }

  best_match = NULL;
  best_match_skipped = scheme_list_length(marks);
  if (best_match_skipped == 1) {
    /* A mark list of length 1 is the common case.
       Since the list is otherwise marshaled into .zo, etc.,
       simplify by extracting just the mark: */
    marks = SCHEME_CAR(marks);
  }

  if (SCHEME_FALSEP(bdg))
    bdg = NULL;

  /* Find a mapping that matches the longest tail of marks
     in the first matching tail of bdg */
  while (1) {
    for (l = map; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      a = SCHEME_CAR(l);
      amarks = SCHEME_CAR(a);

      if (SCHEME_VECTORP(amarks)) {
        abdg = SCHEME_VEC_ELS(amarks)[1];
        amarks = SCHEME_VEC_ELS(amarks)[0];
      } else
        abdg = NULL;

      if (SAME_OBJ(abdg, bdg) 
          || (bdg && abdg && scheme_equal(abdg, bdg))) {
        if (mode > 0) {
          if (scheme_equal(amarks, marks)) {
            best_match = SCHEME_CDR(a);
            break;
          }
        } else {
          if (SCHEME_NULLP(amarks)) {
            /* can always match empty marks */
            best_match = SCHEME_CDR(a);
            best_match_skipped = scheme_proper_list_length(marks);
          } else if (!SCHEME_PAIRP(marks)) {
            /* To be better than nothing, could only match exactly: */
            if (scheme_equal(amarks, marks)) {
              best_match = SCHEME_CDR(a);
              best_match_skipped = 0;
            }
          } else {
            /* amarks can match a tail of marks: */
            for (m = marks, ms = 0; 
                 SCHEME_PAIRP(m) && (ms < best_match_skipped);
                 m = SCHEME_CDR(m), ms++) {

              cm = m;
              if (!SCHEME_PAIRP(amarks)) {
                /* If we're down to the last element
                   of marks, then extract it to try to
                   match the symbol amarks. */
                if (SCHEME_NULLP(SCHEME_CDR(m)))
                  cm = SCHEME_CAR(m);
              }
  
              if (scheme_equal(amarks, cm)) {
                best_match = SCHEME_CDR(a);
                best_match_skipped = ms;
                break;
              }
            }
          }
        }
      }
    }

    if (!best_match && (mode <= 1) && bdg && (SCHEME_PAIRP(bdg) || SCHEME_INTP(bdg) || SCHEME_BIGNUMP(bdg))) {
      /* try lookup with less bdg context */
      if (SCHEME_PAIRP(bdg)) {
        bdg = SCHEME_CDR(bdg);
        if (SCHEME_PAIRP(bdg) && SCHEME_NULLP(SCHEME_CDR(bdg)))
          bdg = SCHEME_CAR(bdg);
      } else
        bdg = NULL;
    } else
      break;
  }

  if (!best_match) {
    if (mode <= 0) {
      return sym;
    }

    /* Last chance before making up a new name. If we're processing a
       module body generated by `expand', then we picked a name last
       time around. We can't pick a new name now, otherwise
       "redundant" module renamings wouldn't be redundant (see
       simpify in "syntax.c") and submodules won't re-expand correctly.
       So, check for a context-determined existing rename. */
    if (!SCHEME_HASHTP((Scheme_Object *)env) && env->module && (mode <= 2)) {
      Scheme_Object *mod, *nm = id, *nom_modix = scheme_false;
      int skipped;
      mod = scheme_stx_module_name(NULL, &nm, scheme_make_integer(env->phase), &nom_modix, NULL, NULL, 
                                   NULL, NULL, NULL, NULL, NULL, &skipped);
      if (mod
          && !SAME_OBJ(mod, scheme_undefined)
          /* refers to env->module if nom_modix has #f path */
          && (!SAME_TYPE(SCHEME_TYPE(nom_modix), scheme_module_index_type)
              || SCHEME_FALSEP(((Scheme_Modidx *)nom_modix)->path))
          && ((skipped == 0) || (mode < 2))
	  && NOT_SAME_OBJ(nm, sym))
	/* It has a rename already! */
	best_match = nm;
    }

    /* Adding a definition. We "gensym" here in a sense; actually, we
       use a symbol table that's in parallel to the normal table, so
       that we get the same parallel-symbol when unmarshalling
       code. We use a counter attached to the environment. Normally,
       this counter just increments, but if a module is re-expanded,
       then the counter starts at 0 for the re-expand, and we may
       re-pick an existing name. To avoid re-picking the same name,
       double-check for a mapping in the environment by inspecting the
       renames attached to id. In the top-level environment, it's
       still possible to get a collision, because separately compiled
       code might be loaded into the same environment (which is just
       too bad). */
    if (!best_match) {
      char onstack[50], *buf;
      intptr_t len;

      while (1) {
	env->id_counter++;
	len = SCHEME_SYM_LEN(sym);
	if (len <= 35)
	  buf = onstack;
	else
	  buf = scheme_malloc_atomic(len + 15);
	memcpy(buf, SCHEME_SYM_VAL(sym), len);
	
	/* The dot here is significant; it might gets stripped away when
	   printing the symbol */
	sprintf(buf XFORM_OK_PLUS len, ".%d", env->id_counter);
	
	best_match = scheme_intern_exact_parallel_symbol(buf, strlen(buf));

	if (!scheme_stx_parallel_is_used(best_match, id)) {
	  /* Also check environment's rename tables. This last check
	     includes the temp table. It also turns out to matter for
	     compiling in `module->namespace' contexts, because no
	     renaming is added after expansion to record the rename
	     table. */
	  if (!scheme_tl_id_is_sym_used(marked_names, best_match)
              && (!temp_marked_names
                  || !scheme_tl_id_is_sym_used(temp_marked_names, best_match))) {
	    /* Ok, no matches, so this name is fine. */
	    break;
	  }
	}
	/* Otherwise, increment counter and try again... */
      }
    }
    if (bdg) {
      a = scheme_make_vector(2, NULL);
      SCHEME_VEC_ELS(a)[0] = marks;
      SCHEME_VEC_ELS(a)[1] = bdg;
      marks = a;
    }
    a = scheme_make_pair(marks, best_match);
    map = scheme_make_pair(a, map);
    
    dest_marked_names = ((mode < 0) || (mode > 2)) ? temp_marked_names : marked_names;
    scheme_hash_set(dest_marked_names, sym, map);
    {
      Scheme_Hash_Table *rev_ht;
      rev_ht = (Scheme_Hash_Table *)scheme_hash_get(dest_marked_names, scheme_false);
      if (rev_ht) {
        scheme_hash_set(rev_ht, best_match, scheme_true);
      }
    }
  } else {
    if (_skipped)
      *_skipped = best_match_skipped;
  }

  return best_match;
}

int scheme_tl_id_is_sym_used(Scheme_Hash_Table *marked_names, Scheme_Object *sym)
{
  intptr_t i;
  Scheme_Object *l, *a;
  Scheme_Hash_Table *rev_ht;

  if (!marked_names)
    return 0;

  if (!marked_names->count)
    return 0;

  rev_ht = (Scheme_Hash_Table *)scheme_hash_get(marked_names, scheme_false);

  if (!rev_ht) {
    rev_ht = scheme_make_hash_table(SCHEME_hash_ptr);

    for (i = marked_names->size; i--; ) {
      l = marked_names->vals[i];
      if (l) {
        for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
          a = SCHEME_CAR(l);
          scheme_hash_set(rev_ht, SCHEME_CDR(a), scheme_true);
        }
      }
      scheme_hash_set(marked_names, scheme_false, (Scheme_Object *)rev_ht);
    }
  }

  if (scheme_hash_get(rev_ht, sym))
    return 1;

  return 0;
}

static Scheme_Object *make_uid(int in_rib)
{
  char name[20];

  sprintf(name, "%cnv%d", in_rib ? 'r' : 'e', env_uid_counter++);
  return scheme_make_symbol(name); /* uninterned! */
}

Scheme_Object *scheme_env_frame_uid(Scheme_Comp_Env *env)
{
  if (env->flags & (SCHEME_NO_RENAME | SCHEME_CAPTURE_WITHOUT_RENAME | SCHEME_CAPTURE_LIFTED))
    return NULL;

  if (!env->uid) {
    Scheme_Object *sym;
    sym = make_uid(env->flags & SCHEME_FOR_INTDEF);
    env->uid = sym;
  }
  return env->uid;
}

static void make_env_renames(Scheme_Comp_Env *env, int rcount, int rstart, int rstart_sec, int force_multi,
			     Scheme_Object *stx)
{
  Scheme_Object *rnm;
  Scheme_Object *uid = NULL;
  int i, pos;

  if (env->flags & (SCHEME_NO_RENAME | SCHEME_CAPTURE_WITHOUT_RENAME | SCHEME_CAPTURE_LIFTED))
    return;

  scheme_env_frame_uid(env);

  if (force_multi) {
    if (env->num_bindings && !env->uids) {
      Scheme_Object **uids;
      uids = MALLOC_N(Scheme_Object *, env->num_bindings);
      env->uids = uids;
    }
    if (COMPILE_DATA(env)->num_const && !COMPILE_DATA(env)->const_uids) {
      Scheme_Object **cuids;
      cuids = MALLOC_N(Scheme_Object *, COMPILE_DATA(env)->num_const);
      COMPILE_DATA(env)->const_uids = cuids;
    }
    if (env->uid && !SCHEME_FALSEP(env->uid)) {
      uid = env->uid;
      env->uid = scheme_false;
    }
  }

  if (!uid) {
    if (env->uid && SCHEME_TRUEP(env->uid)) {
      /* single-uid mode (at least for now) */
      uid = env->uid;
    } else {
      /* multi-uid mode */
      if (!rstart_sec)
	uid = COMPILE_DATA(env)->const_uids[rstart];
      else
	uid = env->uids[rstart];
      if (!uid)
	uid = make_uid(env->flags & SCHEME_FOR_INTDEF);
    }
  }
  
  rnm = scheme_make_rename(uid, rcount);
  pos = 0;

  if (!rstart_sec) {
    for (i = rstart; (i < COMPILE_DATA(env)->num_const) && (pos < rcount); i++, pos++) {
      if (COMPILE_DATA(env)->const_uids)
	COMPILE_DATA(env)->const_uids[i] = uid;
      scheme_set_rename(rnm, pos, COMPILE_DATA(env)->const_names[i]);
    }
    rstart = 0;
  }
  for (i = rstart; pos < rcount; i++, pos++) {
    if (env->uids)
      env->uids[i] = uid;
    scheme_set_rename(rnm, pos, env->values[i]);
  }

  if (SCHEME_RIBP(stx))
    scheme_add_rib_rename(stx, rnm);
  
  if (env->renames) {
    if (SCHEME_PAIRP(env->renames) || SCHEME_NULLP(env->renames))
      rnm = scheme_make_pair(rnm, env->renames);
    else
      rnm = scheme_make_pair(rnm, scheme_make_pair(env->renames, scheme_null));
  }
  env->renames = rnm;
}

Scheme_Object *scheme_add_env_renames(Scheme_Object *stx, Scheme_Comp_Env *env, 
				      Scheme_Comp_Env *upto)
{
  if (!SCHEME_STXP(stx) && !SCHEME_RIBP(stx)) {
    scheme_signal_error("internal error: not syntax or rib");
    return NULL;
  }

  if (SCHEME_RIBP(stx)) {
    GC_CAN_IGNORE int *s;
    s = scheme_stx_get_rib_sealed(stx);
    COMPILE_DATA(env)->sealed = s;
  }

  while (env != upto) {
    if (!(env->flags & (SCHEME_NO_RENAME | SCHEME_CAPTURE_WITHOUT_RENAME 
                        | SCHEME_CAPTURE_LIFTED | SCHEME_INTDEF_SHADOW))) {
      int i, count;
      
      /* How many slots filled in the frame so far?  This can change
	 due to the style of let* compilation, which generates a
	 rename record after each binding set. The "const" bindings
	 are always all in place before we generate any renames in
	 that case. However, the "const" bindings can grow by
	 themselves before non-const bindings are installed. */
      count = COMPILE_DATA(env)->num_const;
      for (i = env->num_bindings; i--; ) {
	if (env->values[i])
	  count++;
      }
      
      if (count) {
	Scheme_Object *l;

	if (!env->renames || (env->rename_var_count != count)) {
	  /* Need to create lexical renaming record(s). We create
	     multiple records as necessary to avoid uids that contain
	     more than one variable with the same symbol name.

	     This is complicated, because we don't want to allocate a
	     hash table in the common case of a binding set with a few
	     names. It's also complicated by incremental rename
	     building: if env->rename_var_count is not zero, we've
	     done this before for a subset of `values' (and there are
	     no consts in that case). In the incremental case, we have
	     a dup_check hash table left from the previous round. */
	  Scheme_Hash_Table *ht;
	  Scheme_Object *name;
	  int rcount = 0, rstart, rstart_sec = 0, vstart;
	  
	  /* rstart is where the to-be-created rename table starts
	     (saved from last time around, or initially zero).
	     vstart is where we start looking for new dups.
	     rstart_sec is TRUE when the new frame starts in the
	     non-constant area. */
	  rstart = env->rename_rstart;
	  if (env->renames) {
	    /* Incremental mode. Drop the most recent (first) rename
               table, because we'll recreate it: */
	    if (SCHEME_PAIRP(env->renames))
	      env->renames = SCHEME_CDR(env->renames);
	    else
	      env->renames = NULL;
	    if (SCHEME_RIBP(stx))
	      scheme_drop_first_rib_rename(stx);
	    vstart = env->rename_var_count;
	    rstart_sec = 1;
	    /* We already know that the first rcount
	       are distinct (from the last iteration) */
	    rcount = vstart - rstart;
	  } else
	    vstart = 0;

	  /* Create or find the hash table: */
	  if (env->dup_check)
	    ht = env->dup_check;
	  else if (env->num_bindings + COMPILE_DATA(env)->num_const > 10)
	    ht = scheme_make_hash_table(SCHEME_hash_ptr);
	  else
	    ht = NULL;

	  if (rcount > 16) {
	    /* Instead of n^2 growth for the rename, just close the current
	       one off and start fresh. */
	    make_env_renames(env, rcount, rstart, rstart_sec, 1, stx);
	    rcount = 0;
	    rstart = vstart;
	    rstart_sec = 1;
	    if (ht) {
	      /* Flush the table for a new set: */
	      ht = scheme_make_hash_table(SCHEME_hash_ptr);
	    }
	  }
	  
	  /* Check for dups among the statics, and build a rename for
             each dup-free set. */

	  /* First: constants. */
	  if (!rstart_sec) {
	    if (COMPILE_DATA(env)->num_const) {
	      /* Start at the beginning, always. */
	      for (i = 0; i < COMPILE_DATA(env)->num_const; i++) {
		int found = 0;
		name = SCHEME_STX_VAL(COMPILE_DATA(env)->const_names[i]);
		if (ht) {
		  if (scheme_hash_get(ht, name))
		    found = 1;
		  else
		    scheme_hash_set(ht, name, scheme_true);
		} else {
		  int j;
		  for (j = rstart; j < i; j++) {
		    if (SAME_OBJ(name, SCHEME_STX_VAL(COMPILE_DATA(env)->const_names[j]))) {
		      found = 1;
		      break;
		    }
		  }
		}

		if (found) {
		  make_env_renames(env, rcount, rstart, rstart_sec, 1, stx);
		  rcount = 1;
		  rstart = i;
		  if (ht) {
		    /* Flush the table for a new set: */
		    ht = scheme_make_hash_table(SCHEME_hash_ptr);
		    scheme_hash_set(ht, name, scheme_true);
		  }
		} else
		  rcount++;
	      }
	    } else 
	      rstart_sec = 1;
	  }

	  for (i = vstart; (i < env->num_bindings) && env->values[i]; i++) {
	    int found = 0;
	    name = SCHEME_STX_VAL(env->values[i]);

	    if (ht) {
	      if (scheme_hash_get(ht, name))
		found = 1;
	      else
		scheme_hash_set(ht, name, scheme_true);
	    } else {
	      int j;
	      if (!rstart_sec) {
		/* Look in consts, first: */
		for (j = rstart; j < COMPILE_DATA(env)->num_const; j++) {
		  if (SAME_OBJ(name, SCHEME_STX_VAL(COMPILE_DATA(env)->const_names[j]))) {
		    found = 1;
		    break;
		  }
		}

		j = 0;
	      } else
		j = rstart;

	      if (!found) {
		for (; j < i; j++) {
		  if (SAME_OBJ(name, SCHEME_STX_VAL(env->values[j]))) {
		    found = 1;
		    break;
		  }
		}
	      }
	    }

	    if (found) {
	      make_env_renames(env, rcount, rstart, rstart_sec, 1, stx);
	      rcount = 1;
	      rstart = i;
	      rstart_sec = 1;
	      if (ht) {
		/* Flush the table for a new set: */
		ht = scheme_make_hash_table(SCHEME_hash_ptr);
		scheme_hash_set(ht, name, scheme_true);
	      }
	    } else
	      rcount++;
	  }
	  
	  make_env_renames(env, rcount, rstart, rstart_sec, 0, stx);

	  env->rename_var_count = count;
	  env->rename_rstart = rstart;
	  if (count < env->num_bindings) {
	    /* save for next time around: */
	    env->dup_check = ht;
	  } else { 
	    /* drop a saved table if there; we're done with all increments */
	    env->dup_check = NULL;
	  }
	}

	if (SCHEME_STXP(stx)) {
	  for (l = env->renames; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	    stx = scheme_add_rename(stx, SCHEME_CAR(l));
	  }
	  if (!SCHEME_NULLP(l))
	    stx = scheme_add_rename(stx, l);
	}
      }
    } else if (env->flags & SCHEME_INTDEF_SHADOW) {
      /* Just extract existing uids from identifiers, and don't need to
         add renames to syntax objects. */
      if (!env->uids) {
        Scheme_Object **uids, *uid;
        int i;
        
        uids = MALLOC_N(Scheme_Object *, env->num_bindings);
        env->uids = uids;
        
        for (i = env->num_bindings; i--; ) {
          uid = scheme_stx_moduleless_env(env->values[i]);
          if (SCHEME_FALSEP(uid))
            scheme_signal_error("intdef shadow binding is #f for %d/%s",
                                SCHEME_TYPE(env->values[i]),
                                scheme_write_to_string(SCHEME_STX_VAL(env->values[i]), 
                                                       NULL));
          env->uids[i] = uid;
        }
      }
    }

    env = env->next;
  }

  return stx;
}

void scheme_seal_env_renames(Scheme_Comp_Env *env)
{
  env->dup_check = NULL;
}

/*********************************************************************/

void create_skip_table(Scheme_Comp_Env *start_frame)
{
  Scheme_Comp_Env *end_frame, *frame;
  int depth, dj = 0, dp = 0, i;
  Scheme_Hash_Table *table;
  int stride = 0;

  depth = start_frame->skip_depth;

  /* Find frames to be covered by the skip table. */
  for (end_frame = start_frame->next;
       end_frame && ((depth & end_frame->skip_depth) != end_frame->skip_depth);
       end_frame = end_frame->next) {
    stride++;
  }

  table = scheme_make_hash_table(SCHEME_hash_ptr);
  
  for (frame = start_frame; frame != end_frame; frame = frame->next) {
    if (frame->flags & SCHEME_LAMBDA_FRAME)
      dj++;
    dp += frame->num_bindings;
    for (i = frame->num_bindings; i--; ) {
      if (frame->values[i]) {
	scheme_hash_set(table, SCHEME_STX_VAL(frame->values[i]), scheme_true);
      }
    }
    for (i = COMPILE_DATA(frame)->num_const; i--; ) {
      scheme_hash_set(table, SCHEME_STX_VAL(COMPILE_DATA(frame)->const_names[i]), scheme_true);
    }
  }

  scheme_hash_set(table, scheme_make_integer(0), (Scheme_Object *)end_frame);
  scheme_hash_set(table, scheme_make_integer(1), scheme_make_integer(dj));
  scheme_hash_set(table, scheme_make_integer(2), scheme_make_integer(dp));

  start_frame->skip_table = table;
}

static void check_taint(Scheme_Object *find_id)
{
  if (scheme_stx_is_tainted(find_id))
    scheme_wrong_syntax(scheme_compile_stx_string, NULL, find_id, 
                        "cannot use identifier tainted by macro transformation");
}

static Scheme_Object *intern_struct_proc_shape(int shape) {
  char buf[20];
  sprintf(buf, "struct%d", shape);
  return scheme_intern_symbol(buf);
}

/*********************************************************************/
/* 

   scheme_lookup_binding() is the main resolver of lexical, module,
   and top-level bindings. Depending on the value of `flags', it can
   return a value whose type tag is:

     scheme_macro_type (id was bound to syntax),

     scheme_macro_set_type (id was bound to a set!-transformer),

     scheme_macro_id_type (id was bound to a rename-transformer),

     scheme_local_type (id was lexical),

     scheme_variable_type (id is a global or module-bound variable),
     or

     scheme_module_variable_type (id is a module-bound variable).

*/

Scheme_Object *
scheme_lookup_binding(Scheme_Object *find_id, Scheme_Comp_Env *env, int flags,
		      Scheme_Object *in_modidx,
		      Scheme_Env **_menv, int *_protected,
                      Scheme_Object **_lexical_binding_id,
                      Scheme_Object **_inline_variant)
{
  Scheme_Comp_Env *frame;
  int j = 0, p = 0, modpos, skip_stops = 0, module_self_reference = 0, is_constant;
  Scheme_Bucket *b;
  Scheme_Object *val, *modidx, *modname, *src_find_id, *find_global_id, *mod_defn_phase;
  Scheme_Object *find_id_sym = NULL, *rename_insp = NULL, *mod_constant = NULL, *shape;
  Scheme_Env *genv;
  intptr_t phase;

  /* Need to know the phase being compiled */
  phase = env->genv->phase;

  /* Walk through the compilation frames */
  for (frame = env; frame->next != NULL; frame = frame->next) {
    int i;
    Scheme_Object *uid;

    while (1) {
      if (frame->skip_table) {
	if (!scheme_hash_get(frame->skip_table, SCHEME_STX_VAL(find_id))) {
	  /* Skip ahead. 0 maps to frame, 1 maps to j delta, and 2 maps to p delta */
	  val = scheme_hash_get(frame->skip_table, scheme_make_integer(1));
	  j += (int)SCHEME_INT_VAL(val);
	  val = scheme_hash_get(frame->skip_table, scheme_make_integer(2));
	  p += (int)SCHEME_INT_VAL(val);
	  frame = (Scheme_Comp_Env *)scheme_hash_get(frame->skip_table, scheme_make_integer(0));
	} else
	  break;
      } else if (frame->skip_depth && !(frame->skip_depth & 0x1F)) {
	/* We're some multiple of 32 frames deep. Build a skip table and try again. */
	create_skip_table(frame);
      } else
	break;
    }
    
    if (frame->flags & SCHEME_LAMBDA_FRAME)
      j++;

    if (!skip_stops || !(frame->flags & SCHEME_FOR_STOPS)) {
      if (frame->flags & SCHEME_FOR_STOPS)
	skip_stops = 1;

      uid = scheme_env_frame_uid(frame);

      if (!find_id_sym 
          && (frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME))
        find_id_sym = scheme_stx_get_module_eq_sym(find_id, scheme_make_integer(phase));

      for (i = frame->num_bindings; i--; ) {
	if (frame->values[i]) {
	  if (frame->uids) 
	    uid = frame->uids[i];
          if (SAME_OBJ(SCHEME_STX_VAL(find_id), SCHEME_STX_VAL(frame->values[i]))
	      && (scheme_stx_env_bound_eq(find_id, frame->values[i], uid, scheme_make_integer(phase))
		  || ((frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME)
		      && scheme_stx_module_eq2(find_id, frame->values[i], scheme_make_integer(phase), find_id_sym))
		  || ((frame->flags & SCHEME_CAPTURE_LIFTED)
		      && scheme_stx_bound_eq(find_id, frame->values[i], scheme_make_integer(phase))))) {
	    /* Found a lambda-, let-, etc. bound variable: */
            check_taint(find_id);
	    /* Looks ok; return a lexical reference */
            if (_lexical_binding_id) {
              if (!(frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME))
                val = scheme_stx_remove_extra_marks(find_id, frame->values[i],
                                                    ((frame->flags & SCHEME_CAPTURE_LIFTED)
                                                     ? NULL
                                                     : uid));
              else
                val = find_id;
              *_lexical_binding_id = val;
            }
	    if (flags & SCHEME_DONT_MARK_USE)
	      return scheme_make_local(scheme_local_type, 0, 0);
	    else
	      return (Scheme_Object *)get_frame_loc(frame, i, j, p, flags);
	  }
	}
      }

      for (i = COMPILE_DATA(frame)->num_const; i--; ) {
	int issame;
	if (frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME)
	  issame = scheme_stx_module_eq2(find_id, COMPILE_DATA(frame)->const_names[i], 
                                         scheme_make_integer(phase), find_id_sym);
        else {
	  if (COMPILE_DATA(frame)->const_uids) uid = COMPILE_DATA(frame)->const_uids[i];
	  issame = (SAME_OBJ(SCHEME_STX_VAL(find_id), 
			     SCHEME_STX_VAL(COMPILE_DATA(frame)->const_names[i]))
		    && scheme_stx_env_bound_eq(find_id, COMPILE_DATA(frame)->const_names[i], uid, 
                                               scheme_make_integer(phase)));
	}
      
	if (issame) {
          check_taint(find_id);

          if (_lexical_binding_id) {
            if (!(frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME))
              val = scheme_stx_remove_extra_marks(find_id, COMPILE_DATA(frame)->const_names[i],
                                                  ((frame->flags & SCHEME_CAPTURE_LIFTED)
                                                   ? NULL
                                                   : uid));
            else
              val = find_id;
            *_lexical_binding_id = val;
          }

	  val = COMPILE_DATA(frame)->const_vals[i];
	
	  if (!val) {
            scheme_wrong_syntax(scheme_compile_stx_string, NULL, find_id,
                                "identifier used out of context");
	    return NULL;
	  }

	  if (SCHEME_FALSEP(val)) {
	    /* Corresponds to a run-time binding (but will be replaced later
	       through a renaming to a different binding) */
            if (flags & SCHEME_OUT_OF_CONTEXT_LOCAL)
              return scheme_make_local(scheme_local_type, 0, 0);
            return NULL;
	  }

	  if (!(flags & SCHEME_ENV_CONSTANTS_OK)) {
	    if (SAME_TYPE(SCHEME_TYPE(val), scheme_macro_type))
	      return val;
	    else
	      scheme_wrong_syntax(scheme_set_stx_string, NULL, find_id,
				  "local syntax identifier cannot be mutated");
	    return NULL;
	  }

	  return val;
	}
      }
    }

    p += frame->num_bindings;
  }

  src_find_id = find_id;
  modidx = scheme_stx_module_name(NULL, &find_id, scheme_make_integer(phase), NULL, NULL, &mod_defn_phase, 
                                  NULL, NULL, NULL, NULL, &rename_insp, NULL);

  /* If modidx and modidx is not #<undefined>,  then find_id is now a 
     symbol, otherwise it's still an identifier. */

  /* Used out of context? */
  if (SAME_OBJ(modidx, scheme_undefined)) {
    if (SCHEME_STXP(find_id)) {
      /* Looks like lexically bound, but double-check that it's not bound via a tl_id: */
      find_global_id = scheme_tl_id_sym(env->genv, find_id, NULL, 0, NULL, NULL);
      if (!SAME_OBJ(find_global_id, SCHEME_STX_VAL(find_id)))
        modidx = NULL; /* yes, it is bound */
    }
    
    if (modidx) {
      if (!(flags & SCHEME_OUT_OF_CONTEXT_OK)) {
        scheme_wrong_syntax(scheme_compile_stx_string, NULL, find_id,
                            "identifier used out of context");
      }
      if (flags & SCHEME_OUT_OF_CONTEXT_LOCAL)
        return scheme_make_local(scheme_local_type, 0, 0);
      return NULL;
    }
  }

  if (modidx) {
    /* If it's an access path, resolve it: */
    modname = scheme_module_resolve(modidx, 1);

    if (env->genv->module && SAME_OBJ(modname, env->genv->module->modname)) {
      modidx = NULL;
      modname = NULL;
      genv = env->genv;
      /* So we can distinguish between unbound identifiers in a module
	 and references to top-level definitions: */
      module_self_reference = 1;
    } else {
      genv = scheme_module_access(modname, env->genv, SCHEME_INT_VAL(mod_defn_phase));

      if (!genv) {
        scheme_wrong_syntax("require", NULL, src_find_id,
                            "namespace mismatch;\n"
                            " reference to a module that is not available\n"
                            "  reference phase: %d\n"
                            "  referenced module: %D\n"
                            "  referenced phase level: %d",
                            env->genv->phase, modname, SCHEME_INT_VAL(mod_defn_phase));
      }
    }
  } else {
    genv = env->genv;
    modname = NULL;

    if (genv->module && genv->disallow_unbound) {
      if (genv->disallow_unbound > 0) {
        /* Free identifier. Maybe don't continue. */
        if (flags & (SCHEME_SETTING | SCHEME_REFERENCING)) {
          scheme_unbound_syntax(((flags & SCHEME_SETTING) 
                                 ? scheme_set_stx_string
                                 : scheme_var_ref_string),
                                NULL, src_find_id, "unbound identifier in module");
          return NULL;
        }
        if (flags & SCHEME_NULL_FOR_UNBOUND)
          return NULL;
      } else {
        if (flags & (SCHEME_SETTING | SCHEME_REFERENCING)) {
          scheme_register_unbound_toplevel(env, src_find_id);
        }
        /* continue, for now */
      }
    }
  }

  if (_menv && genv->module)
    *_menv = genv;
  
  if (!modname && SCHEME_STXP(find_id))
    find_global_id = scheme_tl_id_sym(env->genv, find_id, NULL, 0, NULL, NULL);
  else
    find_global_id = find_id;

  /* Try syntax table: */
  if (modname) {
    val = scheme_module_syntax(modname, env->genv, find_id, SCHEME_INT_VAL(mod_defn_phase));
    if (val && !(flags & SCHEME_NO_CERT_CHECKS))
      scheme_check_accessible_in_module(genv, env->insp, in_modidx, 
					find_id, src_find_id, NULL, NULL, rename_insp,
                                        -2, 0, 
					NULL, NULL,
                                        env->genv, NULL, NULL);
  } else {
    /* Only try syntax table if there's not an explicit (later)
       variable mapping: */
    if (genv->shadowed_syntax 
	&& scheme_hash_get(genv->shadowed_syntax, find_global_id))
      val = NULL;
    else
      val = scheme_lookup_in_table(genv->syntax, (const char *)find_global_id);
  }
  
  if (val) {
    check_taint(src_find_id);
    return val;
  }

  if (modname) {
    Scheme_Object *pos;
    if (flags & SCHEME_NO_CERT_CHECKS) 
      pos = 0;
    else
      pos = scheme_check_accessible_in_module(genv, env->insp, in_modidx, 
					      find_id, src_find_id, NULL, env->insp, rename_insp, -1, 1,
					      _protected, NULL, env->genv, NULL, &mod_constant);
    modpos = (int)SCHEME_INT_VAL(pos);
  } else
    modpos = -1;

  if (modname && (flags & SCHEME_SETTING)) {
    if (SAME_OBJ(src_find_id, find_id) || SAME_OBJ(SCHEME_STX_SYM(src_find_id), find_id))
      find_id = NULL;
    scheme_wrong_syntax(scheme_set_stx_string, find_id, src_find_id, "cannot mutate module-required identifier");
    return NULL;
  }

  if (!modname && (flags & (SCHEME_SETTING | SCHEME_REFERENCING)) 
      && (genv->module && (genv->disallow_unbound > 0))) {
    /* Check for set! of unbound identifier: */    
    if (!scheme_lookup_in_table(genv->toplevel, (const char *)find_global_id)) {
      scheme_unbound_syntax(((flags & SCHEME_SETTING) 
			     ? scheme_set_stx_string
			     : scheme_var_ref_string), 
                            NULL, src_find_id, "unbound identifier in module");
      return NULL;
    }
  }

  if (!modname && (flags & SCHEME_NULL_FOR_UNBOUND)) {
    if (module_self_reference) {
      /* Since the module has a rename for this id, it's certainly defined. */
      if (!(flags & SCHEME_RESOLVE_MODIDS)) {
	/* This is the same thing as #%top handling in compile mode. But
	   for expand mode, it prevents wrapping the identifier with #%top. */
	/* Don't need a pos, because the symbol's gensym-ness (if any) will be
	   preserved within the module. */
        check_taint(src_find_id);
	return scheme_hash_module_variable(genv, genv->module->self_modidx, find_id, 
					   genv->module->insp,
					   -1, genv->mod_phase, 0,
                                           NULL);
      }
    } else
      return NULL;
  }

  check_taint(src_find_id);

  shape = NULL;
  if (mod_constant) {
    if (SAME_OBJ(mod_constant, scheme_constant_key))
      is_constant = 2;
    else if (SAME_OBJ(mod_constant, scheme_fixed_key))
      is_constant = 1;
    else if (SAME_TYPE(SCHEME_TYPE(mod_constant), scheme_proc_shape_type)) {
      is_constant = 2;
      shape = SCHEME_PTR_VAL(mod_constant);
    } else if (SAME_TYPE(SCHEME_TYPE(mod_constant), scheme_struct_proc_shape_type)) {
      if (_inline_variant)
        *_inline_variant = mod_constant;
      is_constant = 2;
      shape = intern_struct_proc_shape(SCHEME_PROC_SHAPE_MODE(mod_constant));
    } else if (SAME_TYPE(SCHEME_TYPE(mod_constant), scheme_inline_variant_type)) {
      if (_inline_variant)
        *_inline_variant = mod_constant;
      is_constant = 2;
      shape = scheme_get_or_check_procedure_shape(mod_constant, NULL);
    } else {
      if (flags & SCHEME_ELIM_CONST) 
        return mod_constant;
      is_constant = 2;
    }
  } else
    is_constant = 0;

  /* Used to have `&& !SAME_OBJ(modidx, modname)' below, but that was a bad
     idea, because it causes module instances to be preserved. */
  if (modname && !(flags & SCHEME_RESOLVE_MODIDS) 
      && (!(scheme_is_kernel_modname(modname) 
            || scheme_is_unsafe_modname(modname)
            || scheme_is_flfxnum_modname(modname)
            || scheme_is_extfl_modname(modname)
            || scheme_is_futures_modname(modname))
          || (flags & SCHEME_REFERENCING))) {
    /* Create a module variable reference, so that idx is preserved: */
    return scheme_hash_module_variable(env->genv, modidx, find_id, 
				       (rename_insp ? rename_insp : genv->module->insp),
				       modpos, SCHEME_INT_VAL(mod_defn_phase),
                                       is_constant, shape);
  }

  if (!modname 
      && (flags & (SCHEME_SETTING | SCHEME_REFERENCING)) 
      && genv->module
      && !(flags & SCHEME_RESOLVE_MODIDS)) {
    /* Need to return a variable reference in this case, too. */
    return scheme_hash_module_variable(env->genv, genv->module->self_modidx, find_global_id, 
				       genv->module->insp,
				       modpos, genv->mod_phase,
                                       is_constant, shape);
  }

  b = scheme_bucket_from_table(genv->toplevel, (char *)find_global_id);

  if ((flags & SCHEME_ELIM_CONST) && b && b->val 
      && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_CONST)
      && !(flags & SCHEME_GLOB_ALWAYS_REFERENCE)
      && (!modname || scheme_is_kernel_modname(modname)))
    return (Scheme_Object *)b->val;

  ASSERT_IS_VARIABLE_BUCKET(b);
  scheme_set_bucket_home(b, genv);
  
  return (Scheme_Object *)b;
}

int scheme_is_imported(Scheme_Object *var, Scheme_Comp_Env *env)
{
  if (env->genv->module) {
    if (SAME_TYPE(SCHEME_TYPE(var), scheme_module_variable_type)) {
      if (!SAME_OBJ(((Module_Variable *)var)->modidx, env->genv->module->self_modidx))
        return 1;
    } else
      return 1;
  } else {
    if (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)) {
      Scheme_Env *home;
      home = scheme_get_bucket_home((Scheme_Bucket *)var);
      if (!SAME_OBJ(home, env->genv))
        return 1;
    } else
      return 1;
  }
  return 0;
}

Scheme_Object *scheme_extract_unsafe(Scheme_Object *o)
{
  Scheme_Env *home;
  home = scheme_get_bucket_home((Scheme_Bucket *)o);
  if (home && home->module && scheme_is_unsafe_modname(home->module->modname))
    return (Scheme_Object *)((Scheme_Bucket *)o)->val;
  else
    return NULL;
}

Scheme_Object *scheme_extract_flfxnum(Scheme_Object *o)
{
  Scheme_Env *home;
  home = scheme_get_bucket_home((Scheme_Bucket *)o);
  if (home && home->module && scheme_is_flfxnum_modname(home->module->modname))
    return (Scheme_Object *)((Scheme_Bucket *)o)->val;
  else
    return NULL;
}

Scheme_Object *scheme_extract_extfl(Scheme_Object *o)
{
  Scheme_Env *home;
  home = scheme_get_bucket_home((Scheme_Bucket *)o);
  if (home && home->module && scheme_is_extfl_modname(home->module->modname))
    return (Scheme_Object *)((Scheme_Bucket *)o)->val;
  else
    return NULL;
}

Scheme_Object *scheme_extract_futures(Scheme_Object *o)
{
  Scheme_Env *home;
  home = scheme_get_bucket_home((Scheme_Bucket *)o);
  if (home && home->module && scheme_is_futures_modname(home->module->modname))
    return (Scheme_Object *)((Scheme_Bucket *)o)->val;
  else
    return NULL;
}

int scheme_env_check_reset_any_use(Scheme_Comp_Env *frame)
{
  int any_use;

  any_use = COMPILE_DATA(frame)->any_use;
  COMPILE_DATA(frame)->any_use = 0;

  return any_use;
}

int scheme_env_min_use_below(Scheme_Comp_Env *frame, int pos)
{
  return COMPILE_DATA(frame)->min_use < pos;
}

int *scheme_env_get_flags(Scheme_Comp_Env *frame, int start, int count)
{
  int *v, i;
  
  v = MALLOC_N_ATOMIC(int, count);
  memcpy(v, COMPILE_DATA(frame)->use + start, sizeof(int) * count);

  for (i = count; i--; ) {
    int old;
    old = v[i];
    v[i] = 0;
    if (old & (ARBITRARY_USE | ONE_ARBITRARY_USE | CONSTRAINED_USE)) {
      v[i] |= SCHEME_WAS_USED;
      if (!(old & (ARBITRARY_USE | WAS_SET_BANGED))) {
        if (old & ONE_ARBITRARY_USE)
          v[i] |= SCHEME_WAS_APPLIED_EXCEPT_ONCE;
        else
          v[i] |= SCHEME_WAS_ONLY_APPLIED;
      }
    }
    if (old & WAS_SET_BANGED)
      v[i] |= SCHEME_WAS_SET_BANGED;
    v[i] |= (old & SCHEME_USE_COUNT_MASK);
  }

  return v;
}

/*========================================================================*/
/*                               macro hooks                              */
/*========================================================================*/


Scheme_Object *
scheme_do_local_lift_expr(const char *who, int stx_pos, int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env, *orig_env;
  Scheme_Object *id, *ids, *rev_ids, *local_mark, *expr, *data, *vec, *id_sym;
  Scheme_Lift_Capture_Proc cp;  
  Scheme_Object *orig_expr;
  int count;
  char buf[24];

  if (stx_pos) {
    if (SCHEME_INTP(argv[0])) {
      count = (int)SCHEME_INT_VAL(argv[0]);
    } else if (SCHEME_BIGNUMP(argv[0])) {
      if (SCHEME_BIGPOS(argv[0]))
        scheme_raise_out_of_memory(NULL, NULL);
      count = -1;
    } else
      count = -1;

    if (count < 0)
      scheme_wrong_contract(who, "exact-nonnegative-integer?", 0, argc, argv);
  } else
    count = 1;

  expr = argv[stx_pos];
  if (!SCHEME_STXP(expr))
    scheme_wrong_contract(who, "syntax?", stx_pos, argc, argv);

  env = orig_env = scheme_current_thread->current_local_env;
  local_mark = scheme_current_thread->current_local_mark;

  if (!env)
    scheme_contract_error(who,
                          "not currently transforming",
                          NULL);

  while (env && !COMPILE_DATA(env)->lifts) {
    env = env->next;
  }

  if (env)
    if (SCHEME_FALSEP(SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[0]))
      env = NULL;

  if (!env)
    scheme_contract_error("syntax-local-lift-expression",
                          "no lift target",
                          NULL);
  
  expr = scheme_add_remove_mark(expr, local_mark);

  /* We don't really need a new symbol each time, since the mark
     will generate new bindings. But lots of things work better or faster
     when different bindings have different symbols. Use env->genv->id_counter
     to help keep name generation deterministic within a module. */
  rev_ids = scheme_null;
  while (count--) {
    sprintf(buf, "lifted.%d", env->genv->id_counter++);
    id_sym = scheme_intern_exact_parallel_symbol(buf, strlen(buf));

    id = scheme_datum_to_syntax(id_sym, scheme_false, scheme_false, 0, 0);
    id = scheme_add_remove_mark(id, scheme_new_mark());

    rev_ids = scheme_make_pair(id, rev_ids);
  }
  ids = scheme_reverse(rev_ids);

  vec = COMPILE_DATA(env)->lifts;
  cp = *(Scheme_Lift_Capture_Proc *)SCHEME_VEC_ELS(vec)[1];
  data = SCHEME_VEC_ELS(vec)[2];

  orig_expr = expr;

  expr = cp(data, &ids, expr, orig_env);

  expr = scheme_make_pair(expr, SCHEME_VEC_ELS(vec)[0]);
  SCHEME_VEC_ELS(vec)[0] = expr;

  SCHEME_EXPAND_OBSERVE_LOCAL_LIFT(scheme_get_expand_observe(), ids, orig_expr);

  rev_ids = scheme_null;
  for (; !SCHEME_NULLP(ids); ids = SCHEME_CDR(ids)) {
    id = SCHEME_CAR(ids);
    id = scheme_add_remove_mark(id, local_mark);
    rev_ids = scheme_make_pair(id, rev_ids);
  }
  ids = scheme_reverse(rev_ids);

  return ids;
}

Scheme_Object *
scheme_local_lift_context(Scheme_Comp_Env *env)
{
  while (env && !COMPILE_DATA(env)->lifts) {
    env = env->next;
  }

  if (!env)
    return scheme_false;
  
  return SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[4];
}

Scheme_Comp_Env *scheme_get_module_lift_env(Scheme_Comp_Env *env)
{
  while (env) {
    if ((COMPILE_DATA(env)->lifts)
        && SCHEME_TRUEP(SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[3]))
      break;
    env = env->next;
  }

  return env;
}

Scheme_Object *
scheme_local_lift_end_statement(Scheme_Object *expr, Scheme_Object *local_mark, Scheme_Comp_Env *env)
{
  Scheme_Object *pr;
  Scheme_Object *orig_expr;

  env = scheme_get_module_lift_env(env);

  if (!env)
    scheme_contract_error("syntax-local-lift-module-end-declaration",
                          "not currently transforming"
                          " an expression within a module declaration",
                          NULL);
  
  expr = scheme_add_remove_mark(expr, local_mark);
  orig_expr = expr;

  pr = scheme_make_pair(expr, SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[3]);
  SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[3] = pr;

  SCHEME_EXPAND_OBSERVE_LIFT_STATEMENT(scheme_get_expand_observe(), orig_expr);
  
  return scheme_void;
}

Scheme_Object *scheme_local_lift_require(Scheme_Object *form, Scheme_Object *orig_form,
                                         intptr_t phase, Scheme_Object *local_mark, Scheme_Comp_Env *env)
{
  Scheme_Object *mark, *data, *pr;
  Scheme_Object *req_form;

  data = NULL;

  while (env) {
    if (COMPILE_DATA(env)->lifts
        && SCHEME_TRUEP(SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[5])) {
      data = SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[5];
      if (SCHEME_RPAIRP(data)
          && !SCHEME_CAR(data)) {
        env = (Scheme_Comp_Env *)SCHEME_CDR(data);
      } else
        break;
    } else
      env = env->next;
  }

  if (!env)
    scheme_contract_error("syntax-local-lift-requires",
                          "could not find target context",
                          NULL);

  
  mark = scheme_new_mark();

  if (SCHEME_RPAIRP(data))
    form = scheme_parse_lifted_require(form, phase, mark, SCHEME_CAR(data));
  else
    form = scheme_toplevel_require_for_expand(form, phase, env, mark);
  
  pr = scheme_make_pair(form, SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[6]);
  SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[6] = pr;

  req_form = form;

  form = orig_form;
  form = scheme_add_remove_mark(form, local_mark);
  form = scheme_add_remove_mark(form, mark);
  form = scheme_add_remove_mark(form, local_mark);

  SCHEME_EXPAND_OBSERVE_LIFT_REQUIRE(scheme_get_expand_observe(), req_form, orig_form, form);

  /* In a top-level context, may need to force compile-time evaluation: */
  if (!env->genv->module)
    scheme_prepare_compile_env(env->genv);

  return form;
}

Scheme_Object *scheme_local_lift_provide(Scheme_Object *form, Scheme_Object *local_mark, 
                                         Scheme_Comp_Env *env)
{
  Scheme_Object *pr;

  while (env) {
    if (COMPILE_DATA(env)->lifts
        && SCHEME_TRUEP(SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[7])) {
      break;
    } else
      env = env->next;
  }

  if (!env)
    scheme_contract_error("syntax-local-lift-provide",
                          "not expanding in a module run-time body",
                          NULL);
  
  form = scheme_add_remove_mark(form, local_mark);
  form = scheme_datum_to_syntax(scheme_make_pair(scheme_datum_to_syntax(scheme_intern_symbol("#%provide"), 
                                                                        scheme_false, scheme_sys_wraps(env), 
                                                                        0, 0),
                                                 scheme_make_pair(form, scheme_null)),
                                form, scheme_false, 0, 0);

  SCHEME_EXPAND_OBSERVE_LIFT_PROVIDE(scheme_get_expand_observe(), form);

  pr = scheme_make_pair(form, SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[7]);
  SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[7] = pr;

  return scheme_void;
}

Scheme_Object *scheme_namespace_lookup_value(Scheme_Object *sym, Scheme_Env *genv, 
                                             Scheme_Object **_id, int *_use_map)
{
  Scheme_Object *id = NULL, *v;
  Scheme_Full_Comp_Env inlined_e;

  scheme_prepare_env_renames(genv, mzMOD_RENAME_TOPLEVEL);
  scheme_prepare_compile_env(genv);

  id = scheme_make_renamed_stx(sym, genv->rename_set);

  inlined_e.base.num_bindings = 0;
  inlined_e.base.next = NULL;
  inlined_e.base.genv = genv;
  inlined_e.base.flags = SCHEME_TOPLEVEL_FRAME;
  init_compile_data((Scheme_Comp_Env *)&inlined_e);
  inlined_e.base.prefix = NULL;

  v = scheme_lookup_binding(id, (Scheme_Comp_Env *)&inlined_e, SCHEME_RESOLVE_MODIDS, 
                            NULL, NULL, NULL, NULL, NULL);
  if (v) {
    if (!SAME_TYPE(SCHEME_TYPE(v), scheme_variable_type)) {
      *_use_map = -1;
      v = NULL;
    } else
      v = (Scheme_Object *)(SCHEME_VAR_BUCKET(v))->val;
  }

  *_id = id;
  return v;
}

Scheme_Object *scheme_find_local_shadower(Scheme_Object *sym, Scheme_Object *sym_marks, Scheme_Comp_Env *env,
                                          Scheme_Object **_free_id)
{
  Scheme_Comp_Env *frame;
  Scheme_Object *esym, *uid = NULL, *env_marks, *prop, *val;

  /* Walk backward through the frames, looking for a renaming binding
     with the same marks as the given identifier, sym. Skip over
     unsealed ribs, though. When we find a match, rename the given
     identifier so that it matches frame. */
  for (frame = env; frame->next != NULL; frame = frame->next) {
    int i;

    for (i = frame->num_bindings; i--; ) {
      if (frame->values[i]) {
	if (SAME_OBJ(SCHEME_STX_VAL(sym), SCHEME_STX_VAL(frame->values[i])))  {
          prop = scheme_stx_property(frame->values[i], unshadowable_symbol, NULL);
          if (SCHEME_FALSEP(prop)) {
            esym = frame->values[i];
            env_marks = scheme_stx_extract_marks(esym);
            if (scheme_equal(env_marks, sym_marks)) {
              sym = esym;
              if (frame->uids)
                uid = frame->uids[i];
              else
                uid = frame->uid;
              break;
            }
          }
	}
      }
    }
    if (uid)
      break;

    if (!COMPILE_DATA(frame)->sealed || *COMPILE_DATA(frame)->sealed) {
      for (i = COMPILE_DATA(frame)->num_const; i--; ) {
        if (!(frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME)) {
          if (SAME_OBJ(SCHEME_STX_VAL(sym), 
                       SCHEME_STX_VAL(COMPILE_DATA(frame)->const_names[i]))) {
            esym = COMPILE_DATA(frame)->const_names[i];
            prop = scheme_stx_property(esym, unshadowable_symbol, NULL);
            if (SCHEME_FALSEP(prop)) {
              env_marks = scheme_stx_extract_marks(esym);
              if (scheme_equal(env_marks, sym_marks)) {
                sym = esym;
                if (COMPILE_DATA(frame)->const_uids)
                  uid = COMPILE_DATA(frame)->const_uids[i];
                else
                  uid = frame->uid;
                val = COMPILE_DATA(frame)->const_vals[i];
                if (val && SAME_TYPE(SCHEME_TYPE(val), scheme_macro_type)) {
                  if (scheme_is_binding_rename_transformer(SCHEME_PTR_VAL(val))) {
                    val = scheme_rename_transformer_id(SCHEME_PTR_VAL(val));
                    *_free_id = val;
                  }
                }
                break;
              }
            }
	  }
	}
      }
    }
    if (uid)
      break;
  }

  return uid;
}

/*========================================================================*/
/*                          syntax-checking utils                         */
/*========================================================================*/

void scheme_check_identifier(const char *formname, Scheme_Object *id, 
			     const char *where, Scheme_Comp_Env *env,
			     Scheme_Object *form)
{
  if (!where)
    where = "";

  if (!SCHEME_STX_SYMBOLP(id))
    scheme_wrong_syntax(formname, form ? id : NULL, 
			form ? form : id, 
			"not an identifier%s", where);

  if (scheme_stx_is_tainted(id))
    scheme_wrong_syntax(formname, form ? id : NULL, 
			form ? form : id, 
			"cannot bind identifier tainted by macro expansion%s", where);
}

void scheme_begin_dup_symbol_check(DupCheckRecord *r, Scheme_Comp_Env *env)
{
  r->phase = env->genv->phase;
  r->count = 0;
}

void scheme_dup_symbol_check(DupCheckRecord *r, const char *where,
			     Scheme_Object *symbol, char *what, 
			     Scheme_Object *form)
{
  int i;

  if (r->count <= 5) {
    for (i = 0; i < r->count; i++) {
      if (scheme_stx_bound_eq(symbol, r->syms[i], scheme_make_integer(r->phase)))
	scheme_wrong_syntax(where, symbol, form,
			    "duplicate %s name", what);
    }

    if (r->count < 5) {
      r->syms[r->count++] = symbol;
      return;
    } else {
      Scheme_Hash_Table *ht;
      ht = scheme_make_hash_table(SCHEME_hash_bound_id);
      r->ht = ht;
      for (i = 0; i < r->count; i++) {
	scheme_hash_set(ht, r->syms[i], scheme_true);
      }
      r->count++;
    }
  }

  if (scheme_hash_get(r->ht, symbol)) {
    scheme_wrong_syntax(where, symbol, form,
			"duplicate %s name", what);
  }

  scheme_hash_set(r->ht, symbol, scheme_true);
}


/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_compenv.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_comp_env, mark_comp_env);
}

END_XFORM_SKIP;

#endif
