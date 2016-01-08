/*
  Racket
  Copyright (c) 2004-2016 PLT Design Inc.
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

/* Pre-allocate local variable reference objects.
   first dimension: position in the current stack frame
   second dimension: 0 for local variables, 1 for unboxed local variables
   third dimension: flags. TODO has to do with whether something is an unboxed fixnum, flonum, or extnum */
READ_ONLY static Scheme_Object *scheme_local[MAX_CONST_LOCAL_POS][MAX_CONST_LOCAL_TYPES][MAX_CONST_LOCAL_FLAG_VAL + 1];
READ_ONLY static Scheme_Object *toplevels[MAX_CONST_TOPLEVEL_DEPTH][MAX_CONST_TOPLEVEL_POS][SCHEME_TOPLEVEL_FLAGS_MASK + 1];

/* If locked, these are probably sharable: */
THREAD_LOCAL_DECL(static Scheme_Hash_Table *toplevels_ht);
THREAD_LOCAL_DECL(static Scheme_Hash_Table *locals_ht[2]);

#define ARBITRARY_USE     0x1
#define CONSTRAINED_USE   0x2
#define WAS_SET_BANGED    0x4
#define ONE_ARBITRARY_USE 0x8
/* See also SCHEME_USE_COUNT_MASK */

static void init_compile_data(Scheme_Comp_Env *env);

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
    dest[i].comp = 1;
    dest[i].dont_mark_local_use = src[drec].dont_mark_local_use;
    dest[i].resolve_module_ids = src[drec].resolve_module_ids;
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
    dest[i].comp = 0;
    dest[i].depth = src[drec].depth;
    dest[i].pre_unwrapped = 0;
    dest[i].substitute_bindings = src[drec].substitute_bindings;
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
  lam[dlrec].comp = 1;
  lam[dlrec].dont_mark_local_use = src[drec].dont_mark_local_use;
  lam[dlrec].resolve_module_ids = src[drec].resolve_module_ids;
  lam[dlrec].substitute_bindings = src[dlrec].substitute_bindings;
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
  int i, c, *use;

  c = env->num_bindings;
  if (c)
    use = MALLOC_N_ATOMIC(int, c);
  else
    use = NULL;

  env->use = use;
  for (i = 0; i < c; i++) {
    use[i] = 0;
  }

  env->min_use = c;
}

Scheme_Comp_Env *scheme_new_compilation_frame(int num_bindings, int flags, Scheme_Object *scopes, Scheme_Comp_Env *base)
{
  Scheme_Comp_Env *frame;
  int count;
  
  count = num_bindings;

  frame = (Scheme_Comp_Env *)MALLOC_ONE_RT(Scheme_Comp_Env);
#ifdef MZTAG_REQUIRED
  frame->type = scheme_rt_comp_env;
#endif

  frame->scopes = scopes;

  {
    Scheme_Object **vals;
    vals = MALLOC_N(Scheme_Object *, count);
    frame->binders = vals;
    vals = MALLOC_N(Scheme_Object *, count);
    frame->bindings = vals;
  }

  frame->num_bindings = num_bindings;
  frame->flags = flags;
  frame->next = base;
  frame->genv = base->genv;
  frame->insp = base->insp;
  frame->prefix = base->prefix;
  frame->in_modidx = base->in_modidx;
  frame->observer = base->observer;

  if (base->next)
    frame->skip_depth = base->skip_depth + 1;
  else
    frame->skip_depth = 0;

  init_compile_data(frame);

  return frame;
}

Scheme_Comp_Env *scheme_new_comp_env(Scheme_Env *genv, Scheme_Object *insp, Scheme_Object *scopes, int flags)
{
  Scheme_Comp_Env *e;
  Comp_Prefix *cp;

  if (!insp)
    insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);

  e = (Scheme_Comp_Env *)MALLOC_ONE_RT(Scheme_Comp_Env);
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

  e->scopes = scopes;

  return e;
}

Scheme_Comp_Env *scheme_new_expand_env(Scheme_Env *genv, Scheme_Object *insp, Scheme_Object *scopes, int flags)
{
  Scheme_Comp_Env *e;

  if (SAME_OBJ(scopes, scheme_true)) {
    if (genv->stx_context)
      scopes = scheme_module_context_frame_scopes(genv->stx_context, NULL);
    else
      scopes = NULL;
  }

  e = scheme_new_comp_env(genv, insp, scopes, flags);
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
  return !!env->use[which];
}

int scheme_is_env_variable_boxed(Scheme_Comp_Env *env, int which)
{
  return !!(env->use[which] & WAS_SET_BANGED);
}

void
scheme_add_compilation_binding(int index, Scheme_Object *val, Scheme_Comp_Env *frame)
{
  Scheme_Object *binding;

  if ((index >= frame->num_bindings) || (index < 0))
    scheme_signal_error("internal error: scheme_add_binding: "
			"index out of range: %d", index);

  if (frame->scopes) {
    /* sometimes redundant: */
    val = scheme_stx_adjust_frame_bind_scopes(val, frame->scopes, scheme_env_phase(frame->genv),
                                              SCHEME_STX_ADD);
  }
  
  frame->binders[index] = val;
  
  if (!frame->bindings[index]) {
    if (frame->flags & SCHEME_INTDEF_SHADOW) {
      binding = scheme_stx_lookup(val, scheme_env_phase(frame->genv));
    } else {
      binding = scheme_gensym(SCHEME_STX_VAL(val));
      scheme_add_local_binding(val, scheme_env_phase(frame->genv), binding);
    }

    frame->bindings[index] = binding;
  }

  frame->skip_table = NULL;
}

void scheme_frame_captures_lifts(Scheme_Comp_Env *env, Scheme_Lift_Capture_Proc cp, Scheme_Object *data, 
                                 Scheme_Object *end_stmts, Scheme_Object *context_key, 
                                 Scheme_Object *requires, Scheme_Object *provides,
                                 Scheme_Object *module_lifts)
{
  Scheme_Lift_Capture_Proc *pp;
  Scheme_Object *vec;
  
  pp = (Scheme_Lift_Capture_Proc *)scheme_malloc_atomic(sizeof(Scheme_Lift_Capture_Proc));
  *pp = cp;

  vec = scheme_make_vector(9, NULL);
  SCHEME_VEC_ELS(vec)[0] = scheme_null;
  SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)pp;
  SCHEME_VEC_ELS(vec)[2] = data;
  SCHEME_VEC_ELS(vec)[3] = end_stmts;
  SCHEME_VEC_ELS(vec)[4] = context_key;
  SCHEME_VEC_ELS(vec)[5] = (requires ? requires : scheme_false);
  SCHEME_VEC_ELS(vec)[6] = scheme_null; /* accumulated requires */
  SCHEME_VEC_ELS(vec)[7] = provides;
  SCHEME_VEC_ELS(vec)[8] = module_lifts; /* #f => disallowed; #t or (void) => add to slot 0; (void) => `module*` allowed */

  env->lifts = vec;
}

void scheme_propagate_require_lift_capture(Scheme_Comp_Env *orig_env, Scheme_Comp_Env *env)
{
  while (orig_env) {
    if ((orig_env->lifts)
        && SCHEME_TRUEP(SCHEME_VEC_ELS(orig_env->lifts)[5]))
      break;
    orig_env = orig_env->next;
  }
  
  if (orig_env) {
    Scheme_Object *vec, *p;

    p = scheme_make_raw_pair(NULL, (Scheme_Object *)orig_env);

    vec = scheme_make_vector(9, NULL);
    SCHEME_VEC_ELS(vec)[0] = scheme_false;
    SCHEME_VEC_ELS(vec)[1] = scheme_void;
    SCHEME_VEC_ELS(vec)[2] = scheme_void;
    SCHEME_VEC_ELS(vec)[3] = scheme_false;
    SCHEME_VEC_ELS(vec)[4] = scheme_false;
    SCHEME_VEC_ELS(vec)[5] = p; /* (rcons NULL env) => continue with env */
    SCHEME_VEC_ELS(vec)[6] = scheme_null;
    SCHEME_VEC_ELS(vec)[7] = scheme_false;
    SCHEME_VEC_ELS(vec)[8] = scheme_false;

    env->lifts = vec;
  }
}

Scheme_Object *scheme_frame_get_lifts(Scheme_Comp_Env *env)
{
  return scheme_reverse(SCHEME_VEC_ELS(env->lifts)[0]);
}

Scheme_Object *scheme_frame_get_end_statement_lifts(Scheme_Comp_Env *env)
{
  return SCHEME_VEC_ELS(env->lifts)[3];
}

Scheme_Object *scheme_frame_get_modules(Scheme_Comp_Env *env)
{
  return SCHEME_VEC_ELS(env->lifts)[8];
}

Scheme_Object *scheme_frame_get_require_lifts(Scheme_Comp_Env *env)
{
  return SCHEME_VEC_ELS(env->lifts)[6];
}

Scheme_Object *scheme_frame_get_provide_lifts(Scheme_Comp_Env *env)
{
  return SCHEME_VEC_ELS(env->lifts)[7];
}

void scheme_add_local_syntax(int cnt, Scheme_Comp_Env *env)
{
  Scheme_Object **ns, **bs, **vs;
  
  if (cnt) {
    ns = MALLOC_N(Scheme_Object *, cnt);
    bs = MALLOC_N(Scheme_Object *, cnt);
    vs = MALLOC_N(Scheme_Object *, cnt);

    env->num_bindings = cnt;
    env->binders = ns;
    env->bindings = bs;
    env->vals = vs;
  }
}

void scheme_set_local_syntax(int pos,
			     Scheme_Object *name, Scheme_Object *val,
			     Scheme_Comp_Env *env,
                             int replace_value)
{
  Scheme_Object *binding;

  if (!replace_value) {
    if (env->flags & SCHEME_CAPTURE_WITHOUT_RENAME) {
      binding = scheme_stx_lookup(name, scheme_env_phase(env->genv));
    } else {
      if (env->scopes)
        name = scheme_stx_adjust_frame_bind_scopes(name, env->scopes, scheme_env_phase(env->genv),
                                                   SCHEME_STX_ADD);
      
      binding = scheme_gensym(SCHEME_STX_VAL(name));
      
      scheme_add_local_binding(name, scheme_env_phase(env->genv), binding);
    }
    
    env->binders[pos] = name;
    env->bindings[pos] = binding;
  }
  env->vals[pos] = val;
  env->skip_table = NULL;
}

Scheme_Comp_Env *
scheme_add_compilation_frame(Scheme_Object *vals, Scheme_Object *scope, Scheme_Comp_Env *env, int flags)
{
  Scheme_Comp_Env *frame;
  int len, i, count;
  
  len = scheme_stx_list_length(vals);
  count = len;

  frame = scheme_new_compilation_frame(count, flags, scope, env);

  for (i = 0; i < len ; i++) {
    if (SCHEME_STX_SYMBOLP(vals)) {
      scheme_add_compilation_binding(i, vals, frame);
    } else {
      Scheme_Object *a;
      a = SCHEME_STX_CAR(vals);
      scheme_add_compilation_binding(i, a, frame);
      vals = SCHEME_STX_CDR(vals);
    }
  }
  
  init_compile_data(frame);

  return frame;
}

void scheme_add_compilation_frame_use_site_scope(Scheme_Comp_Env *env, Scheme_Object *use_site_scope)
{
  while (env->flags & SCHEME_USE_SCOPES_TO_NEXT) {
    env = env->next;
  }

  if (env->flags & (SCHEME_TOPLEVEL_FRAME | SCHEME_MODULE_FRAME | SCHEME_MODULE_BEGIN_FRAME)) {
    scheme_module_context_add_use_site_scope(env->genv->stx_context, use_site_scope);
  } else {
    use_site_scope = scheme_add_frame_use_site_scope(env->scopes, use_site_scope);
    env->scopes = use_site_scope;
  }
}

void scheme_add_compilation_frame_intdef_scope(Scheme_Comp_Env *env, Scheme_Object *scope)
{
  while (env->flags & SCHEME_USE_SCOPES_TO_NEXT) {
    env = env->next;
  }

  if (env->flags & (SCHEME_TOPLEVEL_FRAME | SCHEME_MODULE_FRAME | SCHEME_MODULE_BEGIN_FRAME)) {
    /* we keep intdef scopes, even in this case, for use by get-shadower */
  }

  scope = scheme_add_frame_intdef_scope(env->scopes, scope);
  env->scopes = scope;
}

Scheme_Comp_Env *scheme_no_defines(Scheme_Comp_Env *env)
{
  if (scheme_is_toplevel(env)
      || scheme_is_module_env(env)
      || scheme_is_module_begin_env(env)
      || (env->flags & SCHEME_INTDEF_FRAME))
    return scheme_new_compilation_frame(0, 0, NULL, env);
  else
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
  return !!(env->flags & SCHEME_MODULE_FRAME);
}

int scheme_is_module_begin_env(Scheme_Comp_Env *env)
{
  return !!(env->flags & SCHEME_MODULE_BEGIN_FRAME);
}

Scheme_Comp_Env *scheme_extend_as_toplevel(Scheme_Comp_Env *env)
{
  if (scheme_is_toplevel(env))
    return env;
  else
    return scheme_new_compilation_frame(0, SCHEME_TOPLEVEL_FRAME, NULL, env);
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
  SCHEME_TOPLEVEL_FLAGS(tl) = flags | HIGH_BIT_TO_DISABLE_HASHING;

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

Scheme_Object *scheme_register_stx_in_comp_prefix(Scheme_Object *var, Comp_Prefix *cp) 
{
  Scheme_Local *l;
  Scheme_Object *o;
  int pos;

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

Scheme_Object *scheme_register_stx_in_prefix(Scheme_Object *var, Scheme_Comp_Env *env, 
					     Scheme_Compile_Info *rec, int drec)
{
  Scheme_Local *l;
  Comp_Prefix *cp = env->prefix;

  if (rec && rec[drec].dont_mark_local_use) {
    /* Make up anything; it's going to be ignored. */
    l = (Scheme_Local *)scheme_malloc_atomic_tagged(sizeof(Scheme_Local));
    l->iso.so.type = scheme_compiled_quote_syntax_type;
    l->position = 0;

    return (Scheme_Object *)l;
  }

  return scheme_register_stx_in_comp_prefix(var, cp);
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
        SCHEME_LOCAL_FLAGS(v) = cor | HIGH_BIT_TO_DISABLE_HASHING;

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
        SCHEME_TOPLEVEL_FLAGS(v) = cnst | HIGH_BIT_TO_DISABLE_HASHING;

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

/* type should be either scheme_local_type or scheme_local_unbox_type
   TODO: double check that */
Scheme_Object *scheme_make_local(Scheme_Type type, int pos, int flags)
{
  int k;
  Scheme_Object *v, *key;

  /* k is 0 if type is scheme_local_type and 1 if type is scheme_local_unbox_type */
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
  SCHEME_LOCAL_FLAGS(v) = flags | HIGH_BIT_TO_DISABLE_HASHING;

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

  u = frame->use[i];

  // flags -= (flags & SCHEME_APP_POS);

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
  
  frame->use[i] = u;
  if (i < frame->min_use)
    frame->min_use = i;
  frame->any_use = 1;

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
    ht = scheme_make_hash_table_equal_modix_eq();
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

/*********************************************************************/

#define IS_SKIPPING_DEPTH(n) (n && !(n & 31))

void create_skip_table(Scheme_Comp_Env *start_frame)
{
  Scheme_Comp_Env *end_frame, *frame, *other_frame;
  int depth, dj = 0, dp = 0, i;
  Scheme_Hash_Tree *table;
  int stride = 0, past_binding_frame = 0, past_stops_frame = 0;

  i = start_frame->skip_depth;
  depth = 0;
  while (!(i & 1)) {
    depth = (depth << 1) | 1;
    i >>= 1;
  }

  /* Find frames to be covered by the skip table. */
  for (end_frame = start_frame->next;
       end_frame && (depth & end_frame->skip_depth);
       end_frame = end_frame->next) {
    stride++;
  }

  table = NULL;
  
  for (frame = start_frame; frame != end_frame; frame = frame->next) {
    if (frame->skip_table) {
      other_frame = (Scheme_Comp_Env *)scheme_eq_hash_tree_get(frame->skip_table, scheme_make_integer(0));
      if (other_frame == end_frame) {
        end_frame = frame;
        table = frame->skip_table;
        dj = SCHEME_INT_VAL(scheme_eq_hash_tree_get(table, scheme_make_integer(1)));
        dp = SCHEME_INT_VAL(scheme_eq_hash_tree_get(table, scheme_make_integer(2)));
        past_binding_frame = SCHEME_TRUEP(scheme_eq_hash_tree_get(table, scheme_make_integer(3)));
        past_stops_frame = SCHEME_TRUEP(scheme_eq_hash_tree_get(table, scheme_make_integer(4)));
        break;
      }
    }
  }

  if (!table) {
    table = scheme_make_hash_tree(0);
    table = scheme_hash_tree_set(table, scheme_make_integer(0), (Scheme_Object *)end_frame);
  }

  for (frame = start_frame; frame != end_frame; frame = frame->next) {
    if (!(frame->flags & SCHEME_REC_BINDING_FRAME)
        && frame->scopes)
      past_binding_frame = 1;
    if (frame->flags & SCHEME_FOR_STOPS)
      past_stops_frame = 1;
    if (frame->flags & SCHEME_LAMBDA_FRAME)
      dj++;
    if (!frame->vals)
      dp += frame->num_bindings;
    for (i = frame->num_bindings; i--; ) {
      if (frame->bindings[i])
	table = scheme_hash_tree_set(table, frame->bindings[i], scheme_true);
      if (frame->binders[i])
	table = scheme_hash_tree_set(table, SCHEME_STX_VAL(frame->binders[i]), scheme_true);
    }
  }

  table = scheme_hash_tree_set(table, scheme_make_integer(1), scheme_make_integer(dj));
  table = scheme_hash_tree_set(table, scheme_make_integer(2), scheme_make_integer(dp));
  table = scheme_hash_tree_set(table, scheme_make_integer(3), past_binding_frame ? scheme_true : scheme_false);
  table = scheme_hash_tree_set(table, scheme_make_integer(4), past_stops_frame ? scheme_true : scheme_false);

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

void scheme_dump_env(Scheme_Comp_Env *env)
{
  Scheme_Comp_Env *frame;

  printf("Environment:\n");

  for (frame = env; frame->next != NULL; frame = frame->next) {
    int i;
    for (i = frame->num_bindings; i--; ) {
      printf("  %s -> %s\n  %s\n",
             scheme_write_to_string(frame->binders[i], NULL),
             scheme_write_to_string(frame->bindings[i], NULL),
             scheme_write_to_string((Scheme_Object *)((Scheme_Stx *)frame->binders[i])->scopes, NULL));
    }
  }
}

static int same_binding(Scheme_Object *a, Scheme_Object *b)
{
  if (SCHEME_VECTORP(a) && SCHEME_VECTORP(b)) {
    if (SAME_OBJ(SCHEME_VEC_ELS(a)[1], SCHEME_VEC_ELS(b)[1])
        && SAME_OBJ(SCHEME_VEC_ELS(a)[2], SCHEME_VEC_ELS(b)[2])
        && (SAME_OBJ(SCHEME_VEC_ELS(a)[0], SCHEME_VEC_ELS(b)[0])
            || (SCHEME_TRUEP(SCHEME_VEC_ELS(a)[0])
                && SCHEME_TRUEP(SCHEME_VEC_ELS(b)[0])
                && scheme_equal(scheme_module_resolve(SCHEME_VEC_ELS(a)[0], 0),
                                scheme_module_resolve(SCHEME_VEC_ELS(b)[0], 0)))))
      return 1;
    else
      return 0;
  } else
    return scheme_equal(a, b);
}

static void set_binder(Scheme_Object **_binder, Scheme_Object *ref, Scheme_Object *bind)
{
  if (SAME_OBJ(SCHEME_STX_VAL(ref), SCHEME_STX_VAL(bind)))
    ref = scheme_datum_to_syntax(SCHEME_STX_VAL(ref), ref, bind, 0, 2);
  else {
    /* rename transformer => treat like an expansion */
    ref = scheme_stx_track(scheme_datum_to_syntax(SCHEME_STX_VAL(bind), ref, bind, 0, 2),
                           ref,
                           ref);
 }

  *_binder = ref;
}

/*********************************************************************/
/* 

   scheme_compile_lookup() is the main resolver of lexical, module,
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
scheme_compile_lookup(Scheme_Object *find_id, Scheme_Comp_Env *env, int flags,
		      Scheme_Object *in_modidx,
		      Scheme_Env **_menv, int *_protected,
                      Scheme_Object **_binder, int *_need_macro_scope,
                      Scheme_Object **_inline_variant)
{
  Scheme_Comp_Env *frame;
  int j = 0, p = 0, modpos, skip_stops = 0, module_self_reference = 0, is_constant, ambiguous;
  Scheme_Bucket *b;
  Scheme_Object *binding, *val, *modidx, *modname, *src_find_id, *find_global_id, *mod_defn_phase;
  Scheme_Object *rename_insp = NULL, *mod_constant = NULL, *shape;
  Scheme_Env *genv;

  if (_binder) *_binder = NULL;
  if (_need_macro_scope) *_need_macro_scope = 1;

  binding = scheme_stx_lookup_w_nominal(find_id, scheme_env_phase(env->genv),
                                        (flags & SCHEME_STOP_AT_FREE_EQ),
                                        NULL, &ambiguous, NULL,
                                        &rename_insp,
                                        NULL, NULL, NULL, NULL);

#if 0
  if (!strcmp("cons", SCHEME_SYM_VAL(SCHEME_STX_VAL(find_id)))) {
    printf("%s\n", scheme_write_to_string(find_id, 0));
    scheme_stx_debug_print(find_id, scheme_env_phase(env->genv), 1);
    printf("%s\n", scheme_write_to_string(binding, NULL));
  }
#endif

  if (ambiguous) {
    if (SAME_OBJ(scheme_env_phase(env->genv), scheme_make_integer(0)))
      scheme_wrong_syntax(NULL, NULL, find_id,
                          "identifier's binding is ambiguous%s",
                          scheme_stx_describe_context(find_id, scheme_make_integer(0), 1));
    else
      scheme_wrong_syntax(NULL, NULL, find_id,
                          "identifier's binding is ambiguous\n"
                          "  at phase: %V",
                          scheme_env_phase(env->genv),
                          scheme_stx_describe_context(find_id, scheme_env_phase(env->genv), 1));
    return NULL;
  }

  /* If binding is a symbol, then it must be in the environment, or else
     the identifier is out of context.
     If binding is a vector, then it most likely refers to a module-level
     binding, but we may have a "fluid" binding for in the environment
     to implement stops. */

  if (SCHEME_SYMBOLP(binding)) {
    /* Walk through the compilation frames */
    for (frame = env; frame->next != NULL; frame = frame->next) {
      int i;

      while (1) {
        if (frame->skip_table) {
          if (!scheme_eq_hash_tree_get(frame->skip_table, binding)) {
            /* Skip ahead. 0 maps to frame, 1 maps to j delta, 2 maps to p delta,
               3 maps to binding-frameness, and 4 maps to stops-or-not (unneeded here) */
            val = scheme_eq_hash_tree_get(frame->skip_table, scheme_make_integer(1));
            j += (int)SCHEME_INT_VAL(val);
            val = scheme_eq_hash_tree_get(frame->skip_table, scheme_make_integer(2));
            p += (int)SCHEME_INT_VAL(val);
            val = scheme_eq_hash_tree_get(frame->skip_table, scheme_make_integer(3));
            if (SCHEME_TRUEP(val))
              if (_need_macro_scope)
                *_need_macro_scope = 0;
            frame = (Scheme_Comp_Env *)scheme_eq_hash_tree_get(frame->skip_table, scheme_make_integer(0));
          } else
            break;
        } else if (IS_SKIPPING_DEPTH(frame->skip_depth)) {
          create_skip_table(frame);
          /* try again... */
        } else
          break;
      }

      if (!(env->flags & SCHEME_REC_BINDING_FRAME) && env->scopes)
        if (_need_macro_scope)
          *_need_macro_scope = 0;

      if (frame->flags & SCHEME_LAMBDA_FRAME)
        j++;

      if (!skip_stops || !(frame->flags & SCHEME_FOR_STOPS)) {
        if (frame->flags & SCHEME_FOR_STOPS)
          skip_stops = 1;

        for (i = frame->num_bindings; i--; ) {
          if (frame->bindings[i] && SAME_OBJ(binding, frame->bindings[i])) {
            /* Found a lambda-, let-, etc. bound variable: */
            check_taint(find_id);
            if (_binder)
              set_binder(_binder, find_id, frame->binders[i]);

            if (!frame->vals) {
              if (flags & SCHEME_DONT_MARK_USE)
                return scheme_make_local(scheme_local_type, p+i, 0);
              else
                return (Scheme_Object *)get_frame_loc(frame, i, j, p, flags);
            } else {
              val = frame->vals[i];

              if (!val) {
                scheme_wrong_syntax(scheme_compile_stx_string, NULL, find_id,
                                    "identifier used out of context");
                return NULL;
              }

              if (SCHEME_FALSEP(val)) {
                /* Corresponds to a run-time binding (but will be replaced later
                   through a renaming to a different binding) */
                if (flags & (SCHEME_OUT_OF_CONTEXT_LOCAL | SCHEME_SETTING))
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
      }

      if (!frame->vals)
        p += frame->num_bindings;
    }
    
    if (!(flags & SCHEME_OUT_OF_CONTEXT_OK)) {
      scheme_wrong_syntax(scheme_compile_stx_string, NULL, find_id,
                          "identifier used out of context%s",
                          scheme_stx_describe_context(find_id, scheme_env_phase(env->genv), 1));
    }
    
    if (flags & SCHEME_OUT_OF_CONTEXT_LOCAL)
      return scheme_make_local(scheme_local_type, 0, 0);
    
    return NULL;
  } else {
    /* First, check for a "stop" */
    for (frame = env; frame->next != NULL; frame = frame->next) {
      while (1) {
        if (frame->skip_table) {
          /* skip if we won't jump over stops: */
          if (SCHEME_FALSEP(scheme_eq_hash_tree_get(frame->skip_table, scheme_make_integer(4))))
            frame = (Scheme_Comp_Env *)scheme_eq_hash_tree_get(frame->skip_table, scheme_make_integer(0));
          else
            break;
        } else if (IS_SKIPPING_DEPTH(frame->skip_depth)) {
          create_skip_table(frame);
          /* try again */
        } else
          break;
      }

      if (frame->flags & SCHEME_FOR_STOPS) {
        int i;
        for (i = frame->num_bindings; i--; ) {
          if (same_binding(frame->bindings[i], binding)
              && (SCHEME_TRUEP(binding)
                  || SAME_OBJ(SCHEME_STX_VAL(frame->binders[i]),
                              SCHEME_STX_VAL(find_id)))) {
            check_taint(find_id);
            
            return frame->vals[i];
          }
        }
        /* ignore any further stop frames: */
        break;
      }
    }

    if (SCHEME_FALSEP(binding)) {
      src_find_id = find_id;
      modidx = NULL;
      mod_defn_phase = NULL;
    } else {
      src_find_id = find_id;
      modidx = SCHEME_VEC_ELS(binding)[0];
      if (SCHEME_FALSEP(modidx)) modidx = NULL;
      find_id = SCHEME_VEC_ELS(binding)[1];
      mod_defn_phase = SCHEME_VEC_ELS(binding)[2];
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

      if (_need_macro_scope) {
        for (frame = env; frame->next != NULL; frame = frame->next) {
          if (!(frame->flags & (SCHEME_TOPLEVEL_FRAME
                                | SCHEME_MODULE_FRAME))
              && frame->scopes) {
            *_need_macro_scope = 0;
            break;
          }
        }
      }
    } else {
      if (_need_macro_scope)
        *_need_macro_scope = 0;

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
                                NULL, src_find_id, "unbound identifier in module",
                                scheme_stx_describe_context(src_find_id, scheme_env_phase(genv), 0));
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

  if (SCHEME_STXP(find_id)) {
    find_global_id = scheme_future_global_binding(find_id, env->genv);
    if (!SAME_OBJ(find_global_id, SCHEME_STX_VAL(find_id))
        && SCHEME_FALSEP(binding)) {
      /* Since we got a symbol back, there's at least a "temporary"
         top-level binding for the identifier in the current namespace */
      binding = scheme_make_vector(3, NULL);
      SCHEME_VEC_ELS(binding)[0] = find_global_id;
      SCHEME_VEC_ELS(binding)[1] = (env->genv->module ? env->genv->module->modname : scheme_false);
      SCHEME_VEC_ELS(binding)[2] = scheme_env_phase(env->genv);
    } else if (flags & SCHEME_NULL_FOR_UNBOUND)
      return NULL;
  } else
    find_global_id = find_id;

  /* Try syntax table: */
  if (modname) {
    val = scheme_module_syntax(modname, env->genv, find_id, SCHEME_INT_VAL(mod_defn_phase));
    if (val && !(flags & SCHEME_NO_CERT_CHECKS))
      scheme_check_accessible_in_module(genv, in_modidx, 
					find_id, src_find_id,
                                        env->insp, rename_insp,
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
      pos = scheme_check_accessible_in_module(genv, in_modidx, 
					      find_id, src_find_id, 
                                              env->insp, rename_insp,
                                              -1, 1,
					      _protected, NULL, 
                                              env->genv, NULL, &mod_constant);
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
                            NULL, src_find_id, "unbound identifier in module",
                            scheme_stx_describe_context(src_find_id, scheme_env_phase(genv), 0));
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
    } else if (SCHEME_VECTORP(binding) && !genv->module) {
      /* The identifier is specifically bound as a top-level definition. */
      return (Scheme_Object *)scheme_global_bucket(find_global_id, genv);
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
            || scheme_is_futures_modname(modname)
            || scheme_is_foreign_modname(modname))
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

static Scheme_Comp_Env *find_first_relevant(Scheme_Object *stx, Scheme_Comp_Env *frame)
{
  int i;

  for (; frame->next != NULL; frame = frame->next) {
    while (1) {
      if (frame->skip_table) {
        if (!scheme_eq_hash_tree_get(frame->skip_table, SCHEME_STX_VAL(stx))) {
          frame = (Scheme_Comp_Env *)scheme_eq_hash_tree_get(frame->skip_table, scheme_make_integer(0));
        } else
          break;
      } else if (IS_SKIPPING_DEPTH(frame->skip_depth)) {
        create_skip_table(frame);
        /* try again... */
      } else
        break;
    }

    for (i = frame->num_bindings; i--; ) {
      if (frame->binders[i] && SAME_OBJ(SCHEME_STX_VAL(stx), SCHEME_STX_VAL(frame->binders[i])))
        return frame;
    }
  }

  return frame;
}

static Scheme_Object *add_all_context(Scheme_Object *id, Scheme_Comp_Env *env)
{
  Scheme_Comp_Env *env2;

  for (env2 = env; env2; env2 = env2->next) {
    if (env2->scopes) {
      id = scheme_stx_adjust_frame_scopes(id, env2->scopes, scheme_env_phase(env2->genv),
                                          SCHEME_STX_ADD);
    }
  }

  if (env->genv->module && env->genv->module->ii_src)
    id = scheme_stx_binding_union(id, env->genv->module->ii_src, scheme_env_phase(env->genv));
  else
    id = scheme_stx_add_module_context(id, env->genv->stx_context);
  id = scheme_stx_adjust_module_use_site_context(id, env->genv->stx_context, SCHEME_STX_ADD);

  return id;
}

static Scheme_Object *find_local_binder(Scheme_Object *sym, Scheme_Comp_Env *env)
{
  Scheme_Comp_Env *frame;
  Scheme_Object *id, **sds, *sd;

  for (frame = env; frame->next != NULL; frame = frame->next) {
    int i;

    for (i = frame->num_bindings; i--; ) {
      id = frame->binders[i];
      if (id && SAME_OBJ(SCHEME_STX_VAL(sym), SCHEME_STX_VAL(frame->binders[i]))) {
        if (!frame->shadower_deltas) {
          sds = MALLOC_N(Scheme_Object*,frame->num_bindings);
          frame->shadower_deltas = sds;
        }
        sd = frame->shadower_deltas[i];
        if (!sd) {
          sd = add_all_context(scheme_datum_to_syntax(SCHEME_STX_VAL(id), scheme_false, scheme_false, 0, 0),
                               frame);
          sd = scheme_stx_binding_subtract(id, sd, scheme_env_phase(env->genv));
          frame->shadower_deltas[i] = sd;
        }
	if (scheme_stx_could_bind(sd, sym, scheme_env_phase(env->genv)))
          return id;
      }
    }
  }

  return NULL;
}

Scheme_Object *scheme_get_shadower(Scheme_Object *sym, Scheme_Comp_Env *env, int only_generated)
{
  Scheme_Comp_Env *start_env;
  Scheme_Object *binder, *orig_sym;

  orig_sym = sym;

  start_env = find_first_relevant(sym, env);
  if (start_env->next)
    binder = find_local_binder(sym, start_env);
  else
    binder = NULL;

  if (binder)
    sym = scheme_stx_binding_union(binder, sym, scheme_env_phase(env->genv));
  else if (only_generated)
    sym = scheme_stx_introduce_to_module_context(sym, env->genv->stx_context);
  else if (env->genv->module && env->genv->module->ii_src)
    sym = scheme_stx_binding_union(sym, env->genv->module->ii_src, scheme_env_phase(env->genv));
  else if (env->genv->stx_context)
    sym = scheme_stx_add_module_context(sym, env->genv->stx_context);

  if (!scheme_stx_is_clean(orig_sym))
    sym = scheme_stx_taint(sym);

  return sym;
}

Scheme_Hash_Table *scheme_get_binding_names_table(Scheme_Env *env)
{
  Scheme_Hash_Table *binding_names;

  scheme_binding_names_from_module(env);

  if (env->binding_names
      && SCHEME_HASHTRP(env->binding_names)) {
    /* convert to a mutable hash table */
    binding_names = (Scheme_Hash_Table *)scheme_hash_tree_copy(env->binding_names);
    env->binding_names = (Scheme_Object *)binding_names;
    if (env->binding_names_need_shift) {
      int i;
      for (i = binding_names->size; i--; ) {
        if (binding_names->vals[i]) {
          Scheme_Object *id;
          id = binding_names->vals[i];
          if (!SAME_OBJ(id, scheme_true))
            id = scheme_stx_shift(id, scheme_make_integer(env->phase - env->mod_phase),
                                  env->module->self_modidx, env->link_midx,
                                  env->module_registry->exports,
                                  env->module->prefix->src_insp_desc, env->access_insp);
          binding_names->vals[i] = id;
        }
      }
    }
  }

  binding_names = (Scheme_Hash_Table *)env->binding_names;
  if (!binding_names) {
    binding_names = scheme_make_hash_table(SCHEME_hash_ptr);
    env->binding_names = (Scheme_Object *)binding_names;
    env->binding_names_need_shift = 0;
  }

  return binding_names;
}

static int binding_name_available(Scheme_Hash_Table *binding_names, Scheme_Object *sym,
                                  Scheme_Object *id, Scheme_Object *phase)
{
  sym = scheme_eq_hash_get(binding_names, sym);
  if (!sym || (SCHEME_STXP(sym) && scheme_stx_bound_eq(sym, id, phase)))
    return 1;
  return 0;
}

static Scheme_Object *select_binding_name(Scheme_Object *sym, Scheme_Env *env,
                                          Scheme_Object *id, Scheme_Object *orig_id)
{
  int i;
  char onstack[50], *buf;
  intptr_t len;
  Scheme_Hash_Table *binding_names;

  binding_names = scheme_get_binding_names_table(env);

  /* Use a plain symbol only if the binding has no extra scopes: */
  if (SCHEME_SYM_WEIRDP(sym)
      || scheme_stx_equal_module_context(orig_id, ((env->module && env->module->ii_src)
                                                   ? env->module->ii_src
                                                   : env->stx_context))) {
    if (binding_name_available(binding_names, sym, orig_id, scheme_env_phase(env))) {
      scheme_hash_set(binding_names, sym, orig_id);
      return sym;
    }
  }

  len = SCHEME_SYM_LEN(sym);
  if (len <= 35)
    buf = onstack;
  else
    buf = scheme_malloc_atomic(len + 15);
  memcpy(buf, SCHEME_SYM_VAL(sym), len);
  
  i = 0;
  while (1) {
    sprintf(buf XFORM_OK_PLUS len, ".%d", i);
    sym = scheme_intern_exact_parallel_symbol(buf, strlen(buf));

    if (binding_name_available(binding_names, sym, id, scheme_env_phase(env))) {
      scheme_hash_set(binding_names, sym, orig_id);
      return sym;
    }

    i++;
  }
}

static int binding_matches_env(Scheme_Object *binding, Scheme_Env *env, Scheme_Object *phase)
{
  return (SCHEME_VECTORP(binding)
          && SAME_OBJ(SCHEME_VEC_ELS(binding)[0], 
                      (env->module
                       ? env->module->self_modidx
                       : scheme_false))
          && SAME_OBJ(SCHEME_VEC_ELS(binding)[2], phase));
}

Scheme_Object *scheme_global_binding(Scheme_Object *id, Scheme_Env *env, int for_top_level)
{
  Scheme_Object *sym, *binding, *phase, *orig_id = id;
  int exact_match;

  phase = scheme_env_phase(env);

  if (for_top_level) {
    /* While compiling, we want to avoid binding in the top-level namespace.
       Adding an extra scope avoids that while still letting us have some binding
       to generate names for top-level definitions. */
    if (!env->tmp_bind_scope) {
      sym = scheme_new_scope(SCHEME_STX_MODULE_SCOPE);
      env->tmp_bind_scope = sym;
    }
    id = scheme_stx_add_scope(id, env->tmp_bind_scope, phase);
  }

  binding = scheme_stx_lookup_stop_at_free_eq(id, phase, &exact_match);

  if (!SCHEME_FALSEP(binding)) {
    if (exact_match) {
      if (binding_matches_env(binding, env, phase)) {
        sym = SCHEME_VEC_ELS(binding)[1];
        /* Make sure name is in binding_names and with a specific `id`: */
        scheme_hash_set(scheme_get_binding_names_table(env), sym, orig_id);
        return sym;
      }
      /* Since the binding didn't match, we'll "shadow" the binding
         by replacing it below. */
    }
  }

  sym = select_binding_name(SCHEME_STX_VAL(id), env, id, orig_id);

  scheme_add_module_binding(id, phase,
                            (env->module ? env->module->self_modidx : scheme_false),
                            (env->module
                             ? (env->module->prefix
                                ? env->module->prefix->src_insp_desc
                                : env->module->insp)
                             : env->guard_insp),
                            sym,
                            phase);

  return sym;
}

Scheme_Object *scheme_future_global_binding(Scheme_Object *id, Scheme_Env *env)
/* The identifier id is being referenced before it has a binding. We
   want to allow it, anyway, perhaps because it's outside of a module
   context or because it's phase-1 code. So, we assume that it's going to
   have no extra scopes and get the base name.

   Then again, if `id` has a binding after adding the environment's temporary
   binding scope, then map the identifier to that temporary binding's name.
   That special case allows compiling a `define` to create a binding that
   can be referenced in the same compilation. */
{
  if (env->tmp_bind_scope) {
    Scheme_Object *binding, *phase;

    phase = scheme_env_phase(env);    
    id = scheme_stx_add_scope(id, env->tmp_bind_scope, phase);
    binding = scheme_stx_lookup_stop_at_free_eq(id, phase, NULL);
    
    if (binding_matches_env(binding, env, phase))
      return SCHEME_VEC_ELS(binding)[1];
  }
  
  return SCHEME_STX_VAL(id);
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

Scheme_Object *scheme_extract_foreign(Scheme_Object *o)
{
  Scheme_Env *home;
  home = scheme_get_bucket_home((Scheme_Bucket *)o);
  if (home && home->module && scheme_is_foreign_modname(home->module->modname))
    return (Scheme_Object *)((Scheme_Bucket *)o)->val;
  else
    return NULL;
}

int scheme_env_check_reset_any_use(Scheme_Comp_Env *frame)
{
  int any_use;

  any_use = frame->any_use;
  frame->any_use = 0;

  return any_use;
}

int scheme_env_min_use_below(Scheme_Comp_Env *frame, int pos)
{
  return frame->min_use < pos;
}

void scheme_mark_all_use(Scheme_Comp_Env *frame)
{
  /* Mark all variables as used for the purposes of `letrec-syntaxes+values`
     splitting */
  while (frame && (frame->min_use > -1)) {
    frame->min_use = -1;
    frame = frame->next;
  }
}

int *scheme_env_get_flags(Scheme_Comp_Env *frame, int start, int count)
{
  int *v, i;
  
  v = MALLOC_N_ATOMIC(int, count);
  memcpy(v, frame->use + start, sizeof(int) * count);

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
  Scheme_Object *id, *ids, *rev_ids, *local_scope, *expr, *data, *vec, *id_sym;
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
  local_scope = scheme_current_thread->current_local_scope;

  if (!env)
    scheme_contract_error(who,
                          "not currently transforming",
                          NULL);

  while (env && !env->lifts) {
    env = env->next;
  }

  if (env)
    if (SCHEME_FALSEP(SCHEME_VEC_ELS(env->lifts)[0]))
      env = NULL;

  if (!env)
    scheme_contract_error("syntax-local-lift-expression",
                          "no lift target",
                          NULL);

  if (local_scope)
    expr = scheme_stx_flip_scope(expr, local_scope, scheme_env_phase(env->genv));

  /* We don't really need a new symbol each time, since the scope
     will generate new bindings, but things may work better or faster
     when different bindings have different symbols. Use env->genv->id_counter
     to help keep name generation deterministic within a module. */
  rev_ids = scheme_null;
  while (count--) {
    sprintf(buf, "lifted.%d", env->genv->id_counter++);
    id_sym = scheme_intern_exact_parallel_symbol(buf, strlen(buf));

    id = scheme_datum_to_syntax(id_sym, scheme_false, scheme_false, 0, 0);
    id = scheme_stx_add_scope(id, scheme_new_scope(SCHEME_STX_MACRO_SCOPE), scheme_env_phase(env->genv));

    if (env->genv->stx_context)
      id = scheme_stx_introduce_to_module_context(id, env->genv->stx_context);

    rev_ids = scheme_make_pair(id, rev_ids);
  }
  ids = scheme_reverse(rev_ids);

  vec = env->lifts;
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
    if (local_scope)
      id = scheme_stx_flip_scope(id, local_scope, scheme_env_phase(env->genv));
    rev_ids = scheme_make_pair(id, rev_ids);
  }
  ids = scheme_reverse(rev_ids);

  return ids;
}

Scheme_Object *
scheme_local_lift_context(Scheme_Comp_Env *env)
{
  while (env && !env->lifts) {
    env = env->next;
  }

  if (!env)
    return scheme_false;
  
  return SCHEME_VEC_ELS(env->lifts)[4];
}

Scheme_Comp_Env *scheme_get_module_lift_env(Scheme_Comp_Env *env)
{
  while (env) {
    if ((env->lifts)
        && SCHEME_TRUEP(SCHEME_VEC_ELS(env->lifts)[3]))
      break;
    env = env->next;
  }

  return env;
}

static Scheme_Comp_Env *get_lift_env_for_module(Scheme_Comp_Env *env)
{
  while (env) {
    if ((env->lifts)
        && SCHEME_TRUEP(SCHEME_VEC_ELS(env->lifts)[8]))
      break;
    env = env->next;
  }

  return env;
}

Scheme_Object *
scheme_local_lift_end_statement(Scheme_Object *expr, Scheme_Object *local_scope, Scheme_Comp_Env *env)
{
  Scheme_Object *pr;
  Scheme_Object *orig_expr;

  env = scheme_get_module_lift_env(env);

  if (!env)
    scheme_contract_error("syntax-local-lift-module-end-declaration",
                          "not currently transforming"
                          " an expression within a module declaration",
                          NULL);
  
  if (local_scope)
    expr = scheme_stx_flip_scope(expr, local_scope, scheme_env_phase(env->genv));
  orig_expr = expr;

  pr = scheme_make_pair(expr, SCHEME_VEC_ELS(env->lifts)[3]);
  SCHEME_VEC_ELS(env->lifts)[3] = pr;

  SCHEME_EXPAND_OBSERVE_LIFT_STATEMENT(scheme_get_expand_observe(), orig_expr);
  
  return scheme_void;
}

Scheme_Object *
scheme_local_lift_module(Scheme_Object *expr, Scheme_Object *local_scope, Scheme_Comp_Env *env)
{
  Scheme_Object *pr;
  Scheme_Object *orig_expr;
  int star_ok, slot;

  env = get_lift_env_for_module(env);

  if (!env)
    scheme_contract_error("syntax-local-lift-module",
                          "not currently transforming within a module declaration or top level",
                          NULL);
  
  if (local_scope)
    expr = scheme_stx_flip_scope(expr, local_scope, scheme_env_phase(env->genv));
  orig_expr = expr;

  star_ok = !SAME_OBJ(scheme_true, SCHEME_VEC_ELS(env->lifts)[8]);
    
  if (SCHEME_STX_PAIRP(expr)) {
    pr = SCHEME_STX_CAR(expr);
    if (scheme_stx_free_eq3(pr, scheme_module_stx, scheme_env_phase(env->genv), scheme_make_integer(0))) {
      /* ok */
    } else if (scheme_stx_free_eq3(pr, scheme_modulestar_stx, scheme_env_phase(env->genv), scheme_make_integer(0))) {
      if (!star_ok)
        scheme_contract_error("syntax-local-lift-module",
                              "cannot lift `module*' to a top-level context",
                              "syntax", 1, expr,
                              NULL);
      /* otherwise, ok */
    } else
      pr = NULL;
  } else
    pr = NULL;

  if (!pr)
    scheme_contract_error("syntax-local-lift-module",
                          "not a module declaration",
                          "syntax", 1, expr,
                          NULL);

  /* Add to separate list or mingle with definitions? */
  if (SCHEME_NULLP(SCHEME_VEC_ELS(env->lifts)[8])
      || SCHEME_PAIRP(SCHEME_VEC_ELS(env->lifts)[8]))
    slot = 8;
  else
    slot = 0;
  
  pr = scheme_make_pair(expr, SCHEME_VEC_ELS(env->lifts)[slot]);
  SCHEME_VEC_ELS(env->lifts)[slot] = pr;

  SCHEME_EXPAND_OBSERVE_LIFT_STATEMENT(scheme_get_expand_observe(), orig_expr);
  
  return scheme_void;
}

Scheme_Object *scheme_local_lift_require(Scheme_Object *form, Scheme_Object *orig_form,
                                         intptr_t phase, Scheme_Object *local_scope, Scheme_Comp_Env *cenv)
{
  Scheme_Object *scope, *data, *pr;
  Scheme_Object *req_form;
  int need_prepare = 0;
  Scheme_Comp_Env *env;

  data = NULL;

  env = cenv;
  while (env) {
    if (env->lifts
        && SCHEME_TRUEP(SCHEME_VEC_ELS(env->lifts)[5])) {
      data = SCHEME_VEC_ELS(env->lifts)[5];
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

  
  scope = scheme_new_scope(SCHEME_STX_MACRO_SCOPE);

  if (SCHEME_RPAIRP(data))
    form = scheme_parse_lifted_require(form, phase, scope, SCHEME_CAR(data), &orig_form, cenv);
  else {
    form = scheme_toplevel_require_for_expand(form, phase, cenv, scope);
    need_prepare = 1;
  }
  
  pr = scheme_make_pair(form, SCHEME_VEC_ELS(env->lifts)[6]);
  SCHEME_VEC_ELS(env->lifts)[6] = pr;

  req_form = form;

  form = orig_form;
  form = scheme_stx_flip_scope(form, scope, scheme_env_phase(env->genv));

  SCHEME_EXPAND_OBSERVE_LIFT_REQUIRE(scheme_get_expand_observe(), req_form, orig_form, form);

  /* In a top-level context, may need to force compile-time evaluation: */
  if (need_prepare)
    scheme_prepare_compile_env(env->genv);

  return form;
}

Scheme_Object *scheme_local_lift_provide(Scheme_Object *form, Scheme_Object *local_scope, 
                                         Scheme_Comp_Env *env)
{
  Scheme_Object *pr;

  while (env) {
    if (env->lifts
        && SCHEME_TRUEP(SCHEME_VEC_ELS(env->lifts)[7])) {
      break;
    } else
      env = env->next;
  }

  if (!env)
    scheme_contract_error("syntax-local-lift-provide",
                          "not expanding in a module run-time body",
                          NULL);
  
  if (local_scope)
    form = scheme_stx_flip_scope(form, local_scope, scheme_env_phase(env->genv));
  form = scheme_datum_to_syntax(scheme_make_pair(scheme_datum_to_syntax(scheme_intern_symbol("#%provide"), 
                                                                        scheme_false, scheme_sys_wraps(env), 
                                                                        0, 0),
                                                 scheme_make_pair(form, scheme_null)),
                                form, scheme_false, 0, 0);

  SCHEME_EXPAND_OBSERVE_LIFT_PROVIDE(scheme_get_expand_observe(), form);

  pr = scheme_make_pair(form, SCHEME_VEC_ELS(env->lifts)[7]);
  SCHEME_VEC_ELS(env->lifts)[7] = pr;

  return scheme_void;
}

Scheme_Object *scheme_namespace_lookup_value(Scheme_Object *sym, Scheme_Env *genv, 
                                             Scheme_Object **_id, int *_use_map)
{
  Scheme_Object *id = NULL, *v;
  Scheme_Comp_Env inlined_e;

  scheme_prepare_env_stx_context(genv);
  scheme_prepare_compile_env(genv);

  id = scheme_datum_to_syntax(sym, scheme_false, scheme_false, 0, 0);
  id = scheme_stx_add_module_context(id, genv->stx_context);

  inlined_e.num_bindings = 0;
  inlined_e.next = NULL;
  inlined_e.genv = genv;
  inlined_e.flags = SCHEME_TOPLEVEL_FRAME;
  init_compile_data(&inlined_e);
  inlined_e.prefix = NULL;

  v = scheme_compile_lookup(id, (Scheme_Comp_Env *)&inlined_e, SCHEME_RESOLVE_MODIDS, 
                            NULL,
                            NULL, NULL,
                            NULL, NULL, NULL);
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
  Scheme_Object *l;

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
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      r->ht = ht;
      for (i = 0; i < r->count; i++) {
        l = scheme_hash_get(ht, SCHEME_STX_VAL(r->syms[i]));
        if (!l) l = scheme_null;
        l = scheme_make_pair(r->syms[i], l);
	scheme_hash_set(ht, SCHEME_STX_VAL(r->syms[i]), l);
      }
      r->count++;
    }
  }

  l = scheme_hash_get(r->ht, SCHEME_STX_VAL(symbol));
  if (!l) l = scheme_null;
  scheme_hash_set(r->ht, SCHEME_STX_VAL(symbol), scheme_make_pair(symbol, l));

  while (!SCHEME_NULLP(l)) {
    if (scheme_stx_bound_eq(symbol, SCHEME_CAR(l), scheme_make_integer(r->phase))) {
      scheme_wrong_syntax(where, symbol, form,
                          "duplicate %s name", what);
      return;
    }
    l = SCHEME_CDR(l);
  }
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
