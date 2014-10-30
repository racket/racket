/*
  Racket
  Copyright (c) 2004-2014 PLT Design Inc.
  Copyright (c) 2000-2001 Matthew Flatt

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
#include "schexpobs.h"

/* The implementation of syntax objects is extremely complex due to
   two levels of optimization:

    1. Different kinds of binding are handled in different ways,
       because they'll have different usage patterns. For example,
       module-level bindings are handled differently than local
       bindings, because modules can't be nested.

    2. To save time and space, the data structures involved have lots
       of caches, and syntax objects to be marshaled undergo a
       simplification pass.

   In addition, the need to marshal syntax objects to bytecode
   introduces some other complications. */

ROSYM static Scheme_Object *source_symbol; /* uninterned! */
ROSYM static Scheme_Object *share_symbol; /* uninterned! */
ROSYM static Scheme_Object *origin_symbol;
ROSYM static Scheme_Object *lexical_symbol;
ROSYM static Scheme_Object *protected_symbol;
ROSYM static Scheme_Object *nominal_id_symbol;

READ_ONLY Scheme_Object *scheme_syntax_p_proc;

READ_ONLY static Scheme_Stx_Srcloc *empty_srcloc;

typedef struct Scheme_Mark {
  Scheme_Object so;
  mzlonglong id;
  Scheme_Hash_Table *bindings;
  long binding_version;
} Scheme_Mark;

THREAD_LOCAL_DECL(static mzlonglong mark_counter);
THREAD_LOCAL_DECL(static Scheme_Object *last_phase_shift);
THREAD_LOCAL_DECL(static Scheme_Bucket_Table *taint_intern_table);

static Scheme_Object *syntax_p(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_to_datum(int argc, Scheme_Object **argv);
static Scheme_Object *datum_to_syntax(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_line(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_col(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_pos(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_span(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_src(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_to_list(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_tainted_p(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_original_p(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_property(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_property_keys(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_track_origin(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_shift_phase(int argc, Scheme_Object **argv);

static Scheme_Object *bound_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_trans_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_templ_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_label_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_binding(int argc, Scheme_Object **argv);
static Scheme_Object *module_trans_binding(int argc, Scheme_Object **argv);
static Scheme_Object *module_templ_binding(int argc, Scheme_Object **argv);
static Scheme_Object *module_label_binding(int argc, Scheme_Object **argv);
static Scheme_Object *module_binding_symbol(int argc, Scheme_Object **argv);
static Scheme_Object *identifier_prune(int argc, Scheme_Object **argv);
static Scheme_Object *identifier_prune_to_module(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_src_module(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_arm(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_disarm(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_rearm(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_taint(int argc, Scheme_Object **argv);

static Scheme_Object *write_free_id_info_prefix(Scheme_Object *obj);
static Scheme_Object *read_free_id_info_prefix(Scheme_Object *obj);

static Scheme_Object *raw_stx_content(Scheme_Object *o);
static Scheme_Object *set_false_insp(Scheme_Object *o, Scheme_Object *false_insp, int need_clone);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static void preemptive_chunk(Scheme_Stx *stx);

#define CONS scheme_make_pair
#define ICONS scheme_make_pair

#define HAS_SUBSTX(obj) (SCHEME_PAIRP(obj) || SCHEME_VECTORP(obj) || SCHEME_BOXP(obj) || prefab_p(obj) || SCHEME_HASHTRP(obj))
#define HAS_CHAPERONE_SUBSTX(obj) (HAS_SUBSTX(obj) || (SCHEME_NP_CHAPERONEP(obj) && HAS_SUBSTX(SCHEME_CHAPERONE_VAL(obj))))

#define SCHEME_INSPECTORP(obj) SAME_TYPE(scheme_inspector_type, SCHEME_TYPE(obj))

XFORM_NONGCING static int prefab_p(Scheme_Object *o)
{
  if (SCHEME_STRUCTP(o)) {
    if (((Scheme_Structure *)o)->stype->prefab_key)
      if (MZ_OPT_HASH_KEY(&((Scheme_Structure *)o)->stype->iso) & STRUCT_TYPE_ALL_IMMUTABLE)
        return 1;
  }
  return 0;
}

#define STX_KEY(stx) MZ_OPT_HASH_KEY(&(stx)->iso)

/* A `taints' field is one of
    - NULL => clean
    - #t => tainted, and taint propagated to children, if any)
    - (void) => tainted, and taint needs to be propagated to children
    - <insp> => clean, but inspector needs to be proagated to children
    - (list <insp/#f> <insp> ...+) [interned] => armed; first inspector is to propagate */

XFORM_NONGCING static int is_member(Scheme_Object *a, Scheme_Object *l)
{
  while (SCHEME_PAIRP(l)) {
    if (SAME_OBJ(a, SCHEME_CAR(l)))
      return 1;
    l = SCHEME_CDR(l);
  }
  return 0;
}

#define IS_CANCELING_MARK(x) (((Scheme_Mark *)x)->id < 0)
#define SCHEME_MARKP(x) (SAME_TYPE(SCHEME_TYPE(x), scheme_mark_type))

/*========================================================================*/
/*                           initialization                               */
/*========================================================================*/

void scheme_init_stx(Scheme_Env *env)
{
  Scheme_Object *o;

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  REGISTER_SO(scheme_syntax_p_proc);
  o = scheme_make_folding_prim(syntax_p, "syntax?", 1, 1, 1);
  scheme_syntax_p_proc = o;
  SCHEME_PRIM_PROC_FLAGS(o) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("syntax?", o, env);

  GLOBAL_FOLDING_PRIM("syntax->datum", syntax_to_datum, 1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("datum->syntax", datum_to_syntax, 2, 5, 1, env);
  
  GLOBAL_FOLDING_PRIM_UNARY_INLINED("syntax-e", scheme_checked_syntax_e, 1, 1, 1, env);

  GLOBAL_FOLDING_PRIM("syntax-line"    , syntax_line   , 1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("syntax-column"  , syntax_col    , 1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("syntax-position", syntax_pos    , 1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("syntax-span"    , syntax_span   , 1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("syntax-source"  , syntax_src    , 1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("syntax->list"   , syntax_to_list, 1, 1, 1, env);

  GLOBAL_IMMED_PRIM("syntax-original?"                 , syntax_original_p         , 1, 1, env);
  GLOBAL_IMMED_PRIM("syntax-property"                  , syntax_property           , 2, 3, env);
  GLOBAL_IMMED_PRIM("syntax-property-symbol-keys"      , syntax_property_keys      , 1, 1, env);

  GLOBAL_IMMED_PRIM("syntax-track-origin"              , syntax_track_origin       , 3, 3, env);

  GLOBAL_IMMED_PRIM("make-syntax-delta-introducer"     , scheme_syntax_make_transfer_intro, 2, 3, env);
  GLOBAL_IMMED_PRIM("syntax-shift-phase-level"         , syntax_shift_phase        , 2, 2, env);

  GLOBAL_IMMED_PRIM("bound-identifier=?"               , bound_eq                  , 2, 4, env);
  GLOBAL_IMMED_PRIM("free-identifier=?"                , module_eq                 , 2, 4, env);
  GLOBAL_IMMED_PRIM("free-transformer-identifier=?"    , module_trans_eq           , 2, 2, env);
  GLOBAL_IMMED_PRIM("free-template-identifier=?"       , module_templ_eq           , 2, 2, env);
  GLOBAL_IMMED_PRIM("free-label-identifier=?"          , module_label_eq           , 2, 2, env);

  GLOBAL_IMMED_PRIM("identifier-binding"               , module_binding            , 1, 2, env);
  GLOBAL_IMMED_PRIM("identifier-transformer-binding"   , module_trans_binding      , 1, 2, env);
  GLOBAL_IMMED_PRIM("identifier-template-binding"      , module_templ_binding      , 1, 1, env);
  GLOBAL_IMMED_PRIM("identifier-label-binding"         , module_label_binding      , 1, 1, env);
  GLOBAL_IMMED_PRIM("identifier-prune-lexical-context" , identifier_prune          , 1, 2, env);
  GLOBAL_IMMED_PRIM("identifier-prune-to-source-module", identifier_prune_to_module, 1, 1, env);

  GLOBAL_IMMED_PRIM("identifier-binding-symbol"        , module_binding_symbol     , 1, 2, env);

  GLOBAL_NONCM_PRIM("syntax-source-module"             , syntax_src_module         , 1, 2, env);

  GLOBAL_FOLDING_PRIM("syntax-tainted?", syntax_tainted_p, 1, 1, 1, env);
  GLOBAL_IMMED_PRIM("syntax-arm"                 , syntax_arm                , 1, 3, env);
  GLOBAL_IMMED_PRIM("syntax-disarm"              , syntax_disarm             , 2, 2, env);
  GLOBAL_IMMED_PRIM("syntax-rearm"               , syntax_rearm              , 2, 3, env);
  GLOBAL_IMMED_PRIM("syntax-taint"               , syntax_taint              , 1,1, env);

  REGISTER_SO(source_symbol);
  REGISTER_SO(share_symbol);
  REGISTER_SO(origin_symbol);
  REGISTER_SO(lexical_symbol);
  REGISTER_SO(protected_symbol);
  REGISTER_SO(nominal_id_symbol);
  source_symbol = scheme_make_symbol("source"); /* not interned! */
  share_symbol = scheme_make_symbol("share"); /* not interned! */
  origin_symbol = scheme_intern_symbol("origin");
  lexical_symbol = scheme_intern_symbol("lexical");
  protected_symbol = scheme_intern_symbol("protected");
  nominal_id_symbol = scheme_intern_symbol("nominal-id");

  REGISTER_SO(last_phase_shift);
  REGISTER_SO(mark_id);
  REGISTER_SO(neg_mark_id);
  mark_id = scheme_make_integer(0);
  neg_mark_id = scheme_make_integer(0);

  REGISTER_SO(empty_srcloc);
  empty_srcloc = MALLOC_ONE_RT(Scheme_Stx_Srcloc);
#ifdef MZTAG_REQUIRED
  empty_srcloc->type = scheme_rt_srcloc;
#endif
  empty_srcloc->src = scheme_false;
  empty_srcloc->line = -1;
  empty_srcloc->col = -1;
  empty_srcloc->pos = -1;

  scheme_install_type_writer(scheme_free_id_info_type, write_free_id_info_prefix);
  scheme_install_type_reader(scheme_free_id_info_type, read_free_id_info_prefix);
}

void scheme_init_stx_places(int initial_main_os_thread) {
  if (!initial_main_os_thread) {
    REGISTER_SO(last_phase_shift);
    REGISTER_SO(mark_id);
    REGISTER_SO(neg_mark_id);
    mark_id = scheme_make_integer(0);
    neg_mark_id = scheme_make_integer(0);
  }

  REGISTER_SO(taint_intern_table);
  taint_intern_table = scheme_make_weak_equal_table();
}

/*========================================================================*/
/*                       stx creation and maintenance                     */
/*========================================================================*/

Scheme_Object *scheme_make_stx(Scheme_Object *val, 
			       Scheme_Stx_Srcloc *srcloc,
			       Scheme_Object *props)
{
  Scheme_Stx *stx;

  stx = MALLOC_ONE_TAGGED(Scheme_Stx);
  stx->iso.so.type = scheme_stx_type;
  STX_KEY(stx) = HAS_SUBSTX(val) ? STX_SUBSTX_FLAG : 0;
  stx->val = val;
  stx->srcloc = srcloc;
  stx->wraps = scheme_empty_hash_tree;
  stx->u.to_propagate = NULL;
  stx->shifts = scheme_null;
  stx->props = props;

  return (Scheme_Object *)stx;
}

Scheme_Object *clone_stx(Scheme_Object *to)
{
  Scheme_Stx *stx = (Scheme_Stx *)to;
  Scheme_Object *wraps, *modinfo_cache;
  Scheme_Object *taints;
  Scheme_Hash_Tree *marks;
  Scheme_Object *to_propagate;
  intptr_t lazy_prefix;
  int dp;

  wraps = stx->wraps;
  taints = stx->taints;
  marks = stx->marks;
  to_propagate = stx->u.to_propagate;

  stx = (Scheme_Stx *)scheme_make_stx(stx->val, 
                                      stx->srcloc,
                                      stx->props);

  stx->wraps = wraps;
  stx->marks = marks;
  if (STX_KEY(stx) & STX_SUBSTX_FLAG)
    stx->u.to_propagate = to_propagate;
  stx->taints = taints;

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_make_stx_w_offset(Scheme_Object *val, 
					intptr_t line, intptr_t col, intptr_t pos, intptr_t span,
					Scheme_Object *src,
					Scheme_Object *props)
{
  Scheme_Stx_Srcloc *srcloc;

  srcloc = MALLOC_ONE_RT(Scheme_Stx_Srcloc);
#ifdef MZTAG_REQUIRED
  srcloc->type = scheme_rt_srcloc;
#endif
  srcloc->src = src;
  srcloc->line = line;
  srcloc->col = col;
  srcloc->pos = pos;
  srcloc->span = span;
   
  return scheme_make_stx(val, srcloc, props);
}

Scheme_Object *scheme_stx_track(Scheme_Object *naya, 
				Scheme_Object *old,
				Scheme_Object *origin)
     /* Maintain properties for an expanded expression */
{
  Scheme_Stx *nstx = (Scheme_Stx *)naya;
  Scheme_Stx *ostx = (Scheme_Stx *)old;
  Scheme_Object *ne, *oe, *e1, *e2;

  if (nstx->props) {
    if (SAME_OBJ(nstx->props, STX_SRCTAG)) {
      /* Retain 'source tag. */
      ne = ICONS(ICONS(source_symbol, scheme_true), scheme_null);
    } else
      ne = nstx->props;
  } else
    ne = scheme_null;
  
  if (ostx->props) {
    if (SAME_OBJ(ostx->props, STX_SRCTAG)) {
      /* Drop 'source, add 'origin. */
      oe = NULL;
    } else {
      Scheme_Object *p, *a;
      int mod = 0, add = 1;

      oe = ostx->props;

      /* Drop 'source and 'share, add 'origin if not there */
      for (p = oe; SCHEME_PAIRP(p); p = SCHEME_CDR(p)) {
	a = SCHEME_CAR(SCHEME_CAR(p));
	if (SAME_OBJ(a, source_symbol) || SAME_OBJ(a, share_symbol))
	  mod = 1;
	else if (SAME_OBJ(a, origin_symbol))
	  mod = 1;
      }

      if (mod) {
	Scheme_Object *first = scheme_null, *last = NULL;

	for (; SCHEME_PAIRP(oe); oe = SCHEME_CDR(oe)) {
	  a = SCHEME_CAR(SCHEME_CAR(oe));
	  if (!SAME_OBJ(a, source_symbol) && !SAME_OBJ(a, share_symbol)) {
	    if (!SAME_OBJ(a, origin_symbol)) {
	      p = ICONS(SCHEME_CAR(oe), scheme_null);
	    } else {
	      p = ICONS(ICONS(a, ICONS(origin, 
				       SCHEME_CDR(SCHEME_CAR(oe)))),
			scheme_null);
	      add = 0;
	    }

	    if (last)
	      SCHEME_CDR(last) = p;
	    else
	      first = p;
	    last = p;
	  }
	}

	oe = first;
      } 
      if (add) {
	oe = ICONS(ICONS(origin_symbol, 
			 ICONS(origin, scheme_null)),
		  oe);
      }
    }
  } else {
    /* Add 'origin. */
    oe = NULL;
  }

  if (!oe)
    oe = ICONS(ICONS(origin_symbol, 
		     ICONS(origin, scheme_null)),
	      scheme_null);

  /* Merge ne and oe (ne takes precedence). */
  
  /* First, check for overlap: */
  for (e1 = ne; SCHEME_PAIRP(e1); e1 = SCHEME_CDR(e1)) {
    Scheme_Object *a;
    a = SCHEME_CAR(SCHEME_CAR(e1));
    for (e2 = oe; SCHEME_PAIRP(e2); e2 = SCHEME_CDR(e2)) {
      if (SAME_OBJ(SCHEME_CAR(SCHEME_CAR(e2)), a)) {
	break;
      }
    }
    if (!SCHEME_NULLP(e1))
      break;
  }

  if (SCHEME_NULLP(e1)) {
    /* Can just append props info (probably the common case). */
    if (!SCHEME_NULLP(oe))
      ne = scheme_append(ne, oe);
  } else {
    /* Have to perform an actual merge: */
    Scheme_Object *first = scheme_null, *last = NULL, *p;

    for (e1 = ne; SCHEME_PAIRP(e1); e1 = SCHEME_CDR(e1)) {
      Scheme_Object *a, *v;
      a = SCHEME_CAR(SCHEME_CAR(e1));
      v = SCHEME_CDR(SCHEME_CAR(e1));
      for (e2 = oe; SCHEME_PAIRP(e2); e2 = SCHEME_CDR(e2)) {
	if (SAME_OBJ(SCHEME_CAR(SCHEME_CAR(e2)), a)) {
	  v = ICONS(v, SCHEME_CDR(SCHEME_CAR(e2)));
	  break;
	}
      }

      p = ICONS(ICONS(a, v), scheme_null);
      if (last)
	SCHEME_CDR(last) = p;
      else
	first = p;
      last = p;
    }

    for (e1 = oe; SCHEME_PAIRP(e1); e1 = SCHEME_CDR(e1)) {
      Scheme_Object *a, *v;
      a = SCHEME_CAR(SCHEME_CAR(e1));
      v = SCHEME_CDR(SCHEME_CAR(e1));
      for (e2 = ne; SCHEME_PAIRP(e2); e2 = SCHEME_CDR(e2)) {
	if (SAME_OBJ(SCHEME_CAR(SCHEME_CAR(e2)), a)) {
	  v = NULL;
	  break;
	}
      }

      if (v) {
	p = ICONS(ICONS(a, v), scheme_null);
	if (last)
	  SCHEME_CDR(last) = p;
	else
	  first = p;
	last = p;
      }
    }

    ne = first;
  }

  /* Clone nstx, keeping wraps, changing props to ne */
  nstx = (Scheme_Stx *)clone_stx((Scheme_Object *)nstx);
  nstx->props = ne;

  return (Scheme_Object *)nstx;
}

/******************** marks ********************/

Scheme_Object *scheme_new_mark(int canceling)
{
  Scheme_Mark *m;
  mzlonglong id;

  m = (Scheme_Mark *)scheme_malloc_small_tagged(sizeof(Scheme_Mark));
  m->type = scheme_mark_type;
  id = ++mark_counter;
  if (canceling)
    id = -id;
  m->id = id;

  return (Scheme_Object *)m;
}

static Scheme_Hash_Tree *add_remove_mark(Scheme_Hash_Tree *marks, Scheme_Object *m)
{
  if (scheme_hash_tree_get(marks, m)) {
    if (IS_CANCELING_MARK(m))
      return scheme_hash_tree_set(marks, m, NULL);
    else
      return marks;
  } else
    return scheme_hash_tree_set(phase_marks, m, scheme_true);
}

Scheme_Object *scheme_add_remove_mark(Scheme_Object *o, Scheme_Object *m)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Hash_Tree *marks;
  Scheme_Object *taints, *to_propagate;

  marks = add_remove_mark(stx->marks, m);
  if ((stx->marks == marks)
      && !(STX_KEY(stx) & STX_SUBSTX_FLAG)) {
    return (Scheme_Object *)stx;
  }

  if (STX_KEY(stx) & STX_SUBSTX_FLAG) {
    to_propagate = (stx->to_propagate ? stx->to_propagate : scheme_empty_hash_tree);
    to_propagate = add_remove_mark(stx->to_propagate, m);
    if ((stx->to_propagate == to_propagate)
        && (stx->marks == marks))
      return (Scheme_Object *)stx;
    
    if (!stx->to_propagate || (stx->to_propagate == scheme_empty_hash_tree)) {
      /* record base info as a shortcut for propagation */
      Scheme_Object *vec;
      vec = scheme_make_vector(2, NULL);
      SCHEME_VEC_ELS(vec)[0] = (Scheme_Object *)stx->marks;
      SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)(stx->to_propagate ? scheme_empty_hash_tree : stx->to_propagate);
      to_propagate = scheme_hash_tree_set(to_propagate, scheme_true, vec);
    }
  } else
    to_propagate = NULL; /* => cleared binding cache */

  taints = stx->taints;
  stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
  stx->marks = marks;
  stx->u.to_propagate = to_propagate;
  stx->taints = taints;

  return (Scheme_Object *)stx;
}

/******************** shifts ********************/

static Scheme_Object *extract_phase_shift(Scheme_Stx *stx)
{
  Scheme_Object *a;

  if (!SCHEME_NULLP(stx->shifts)) {
    a = stx->shifts;
    if (SCHEME_VECTOR(a))
      a = SCHEME_VEC_ELS(a)[0];
    if (SCHEME_PAIRP(a)) {
      a = SCHEME_CAR(a);
      if (SCHEME_INTP(a) || SCHEME_BIGNUMP(a) || SCHEME_FALSEP(a))
        return a;
    }
  }

  return scheme_make_integer(0);
}

static Scheme_Object *add_shift(Scheme_Object *shift, Scheme_Object *shifts)
{
  /* Collapse phase shifts and keep them at the front. */
  
  if (SCHEME_PAIRP(shifts) && (SCHEME_INTP(SCHEME_CAR(shifts))
                               || SCHEME_BIGNUMP(SCHEME_CAR(shifts)))) {
    if (SCHEME_INTP(shift) || SCHEME_BIGNUMP(shift))
      return scheme_make_pair(scheme_bin_plus(shift, SCHEME_CAR(shifts)),
                              SCHEME_CDR(shifts));
    else
      return scheme_make_pair(SCHEME_CAR(shifts),
                              scheme_make_pair(shift,
                                               SCHEME_CDR(shifts)));
  } else
    return scheme_make_pair(shift, shifts);
}

Scheme_Object *scheme_stx_add_shift(Scheme_Object *o, Scheme_Object *shift)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *vec, *shifts;

  if (STX_KEY(stx) & STX_SUBSTX_FLAG) {
    /* Keep track of shifts that need to be propagated */
    vec = scheme_make_vector(3, NULL);
    if (SCHEME_VECTORP(stx->shifts)) {
      shifts = add_shift(shift, SCHEME_VEC_ELS(stx->shifts)[1]);
      SCHEME_VEC_ELS(vec)[1] = shifts;
      SCHEME_VEC_ELS(vec)[2] = SCHEME_VEC_ELS(stx->shifts)[2];
      shifts = SCHEME_VEC_ELS(stx->shifts)[0];
    } else {
      shifts = add_shift(shift, scheme_null);
      SCHEME_VEC_ELS(vec)[1] = shifts;
      SCHEME_VEC_ELS(vec)[2] = scheme_null;
      shifts = stx->shifts;
    }
    shifts = add_shift(shift, shifts);
    SCHEME_VEC_SHIFTS(vec)[0] = shifts;
  } else {
    /* No need to propagate, so it's a simple addition. */
    shifts = add_shift(shift, stx->shifts);
  }

  stx = (Scheme_Stx *)clone_stx((Scheme_Object *)stx);
  stx->shifts = shifts;

  return (Scheme_Object *)shifts;
}

static Scheme_Object *scheme_make_phase_shift(Scheme_Object *old_midx, Scheme_Object *new_midx,
                                              Scheme_Hash_Table *export_registry, Scheme_Object *insp,
                                              Scheme_Object *ignore_old_identity)
{
  if (new_midx || export_registry || insp) {
    Scheme_Object *vec;
    
    if (last_phase_shift
	&& ((vec = SCHEME_BOX_VAL(last_phase_shift)))
	&& (SCHEME_VEC_ELS(vec)[0] == (new_midx ? old_midx : scheme_false))
	&& (SCHEME_VEC_ELS(vec)[1] == (new_midx ? new_midx : scheme_false))
	&& (SCHEME_VEC_ELS(vec)[2] == (export_registry ? (Scheme_Object *)export_registry : scheme_false))
        && (SCHEME_VEC_ELS(vec)[3] == (insp ? insp : scheme_false))
        && (SCHEME_VEC_ELS(vec)[4] == (ignore_old_identity ? ignore_old_identity : scheme_false))) {
      /* use the old one */
    } else {
      vec = scheme_make_vector(5, NULL);
      SCHEME_VEC_ELS(vec)[0] = (new_midx ? old_midx : scheme_false);
      SCHEME_VEC_ELS(vec)[1] = (new_midx ? new_midx : scheme_false);
      SCHEME_VEC_ELS(vec)[2] = (export_registry ? (Scheme_Object *)export_registry : scheme_false);
      SCHEME_VEC_ELS(vec)[3] = (insp ? insp : scheme_false);
      SCHEME_VEC_ELS(vec)[4] = (ignore_old_identity ? ignore_old_identity : scheme_false);
      
      last_phase_shift = scheme_box(vec);
    }

    return last_phase_shift;
  } else
    return NULL;
}

void scheme_clear_shift_cache(void)
{
  last_phase_shift = NULL;
}

Scheme_Object *scheme_stx_phase_shift(Scheme_Object *stx, Scheme_Object *shift,
				      Scheme_Object *old_midx, Scheme_Object *new_midx,
				      Scheme_Hash_Table *export_registry,
                                      Scheme_Object *insp,
                                      Scheme_Object *ignore_old_identity)
/* Shifts the phase on a syntax object in a module. A 0 shift might be
   used just to re-direct relative module paths. new_midx might be
   NULL to shift without redirection. And so on. */
{
  Scheme_Object *s;

  if (!SAME_OBJ(shift, scheme_make_integer(0)))
    stx = scheme_stx_add_shift(stx, shift);

  s = scheme_stx_phase_shift_as_rename(old_midx, new_midx, export_registry, insp, ignore_old_identity);
  if (s)
    stx = scheme_stx_add_shift(stx, s);

  return stx;
}

static Scheme_Object *apply_modidx_shifts(Scheme_Object *shifts, Scheme_Object *modidx, Scheme_Object **_insp)
{
  Scheme_Object *vec, *dest, *src, *insp;
  Scheme_Object *modidx_shift_to = NULL, *modidx_shift_from = NULL;

  while (!SCHEME_NULLP(shifts)) {
    vec = SCHEME_CAR(shifts);
     
    src = SCHEME_VEC_ELS(vec)[0];
    dest = SCHEME_VEC_ELS(vec)[1];
    insp = SCHEME_VEC_ELS(vec)[3];

    if (!modidx_shift_to) {
      modidx_shift_to = dest;
    } else if (!SAME_OBJ(modidx_shift_from, dest)) {
      modidx_shift_to = scheme_modidx_shift(dest,
                                            modidx_shift_from,
                                            modidx_shift_to);
    }
    modidx_shift_from = src;
      
    if (_insp && SCHEME_TRUEP(insp))
      *_insp = insp;
  }

  if (modidx_shift_from)
    return scheme_modidx_shift(modidx,
                               modidx_shift_from,
                               modidx_shift_to);
  else
    return modidx;
}

static Scheme_Object *syntax_shift_phase(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-shift-phase-level", "syntax?", 0, argc, argv);
  if (SCHEME_TRUEP(argv[1]) && !scheme_exact_p(argv[1]))
    scheme_wrong_contract("syntax-shift-phase-level", "(or/c exact-integer? #f)", 0, argc, argv);

  if (SCHEME_INTP(argv[1]) && !SCHEME_INT_VAL(argv[1]))
    return argv[0];

  return scheme_stx_shift(argv[0], argv[1]);
}

/******************** lazy propagation ********************/

static Scheme_Object *propagate_marks(Scheme_Object *o, Scheme_Hash_Tree *to_propagate,
                                      Scheme_Hash_Tree *parent_marks)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *to_propagate, *key, *val;
  intptr_t i;

  if (!to_propagate || (to_propagate == scheme_empty_hash_tree))
    return o;

  val = scheme_hash_get(to_propagate, scheme_true);
  if (val 
      && SAME_OBJ(SCHEME_VEC_ELS(val)[0], (Scheme_Object *)stx->marks)
      && (!(STX_KEY(stx) & STX_SUBSTX_FLAG)
          || SAME_OBJ(SCHEME_VEC_ELS(val)[1], (Scheme_Object *)stx->u.to_propagate))) {
    /* shortcut: child marks match parent */
    stx = (Scheme_Stx *)clone_stx((Scheme_Object *)stx);
    stx->marks = parent_marks;
    if (!(STX_KEY(stx) & STX_SUBSTX_FLAG))
      stx->u.to_propagate = to_propagate;
    return (Scheme_Object *)stx;
  }
  
  i = scheme_hash_tree_next(to_propagate, -1);
  while (i != -1) {
    scheme_hash_tree_index(to_propagate, i, &key, &val);

    o = scheme_add_remove_mark(o, key);
    
    i = scheme_hash_tree_next(to_propagate, i);
  }

  return o;
}

static Scheme_Object *propaagte_shifts(Scheme_Object *result, Scheme_Object *shifts)
{
  Scheme_Stx *stx = (Scheme_Stx *)result;
  Scheme_Object *l;

  if (SAME_OBJ(stx->shifts, SCHEME_VEC_ELS(shifts)[2])) {
    if ((STX_KEY(stx) & STX_SUBSTX_FLAG))
      stx->shifts = shifts;
    else
      stx->shifts = SCHEME_VEC_ELS(shifts)[0];
    return result;
  }

  for (l = scheme_reverse(SCHEME_VEC_ELS(shifts)[1]); !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    result = scheme_stx_shift(result, SCHEME_CAR(l));
  }

  return result;
}

static Scheme_Object *propagate(Scheme_Object *result, Scheme_Object *to_propagate,
                                Scheme_Object *parent_marks,
                                Scheme_Object *shifts,
                                int add_taint, Scheme_Object *false_insp)
{
  result = propagate_marks(result, to_propagate, stx->marks);

  if (shifts)
    result = propagate_shifts(result, shifts);

  if (add_taint)
    result = add_taint_to_stx(result, 1);
  else if (false_insp)
    result = set_false_insp(result, false_insp, 1);

  return result;
}

static Scheme_Object *raw_stx_content(Scheme_Object *o)
 /* Propagates wraps and taints while getting a syntax object's content. */
{
  Scheme_Stx *stx = (Scheme_Stx *)o;

  /* The fast-path tests are duplicated in jit.c. */

  if ((STX_KEY(stx) & STX_SUBSTX_FLAG) && stx->u.to_propagate) {
    Scheme_Object *v = stx->val, *to_propagate, *result;
    Scheme_Object *here_wraps, *false_insp, *shifts;
    int add_taint;
    
    to_propagate = stx->u.to_propagate;
    false_insp = stx->taints;
    if (SCHEME_VOIDP(false_insp)) {
      add_taint = 1;
    } else {
      add_taint = 0;
      if (SCHEME_PAIRP(false_insp))
        false_insp = SCHEME_CAR(false_insp);
      if (!SCHEME_INSPECTORP(false_insp))
        false_insp = NULL;
    }

    shifts = stx->shifts;
    if (!SCHEME_VECTORP(stx->shifts))
      shifts = NULL;

    if (SCHEME_PAIRP(v)) {
      Scheme_Object *last = NULL, *first = NULL;

      while (SCHEME_PAIRP(v)) {
	Scheme_Object *p;
        result = SCHEME_CAR(v);
        result = propagate(result, to_propagate, stx->marks,
                           shifts,
                           add_taint, false_insp);
	p = scheme_make_pair(result, scheme_null);
	if (last)
	  SCHEME_CDR(last) = p;
	else
	  first = p;
	last = p;
	v = SCHEME_CDR(v);
      }
      if (!SCHEME_NULLP(v)) {
        result = v;
        result = propagate(result, to_propagate, stx->marks,
                           shifts,
                           add_taint, false_insp);
	if (last)
	  SCHEME_CDR(last) = result;
	else
	  first = result;
      }
      v = first;
    } else if (SCHEME_BOXP(v)) {
      result = SCHEME_BOX_VAL(v);
      result = propagate(result, to_propagate, stx->marks,
                         shifts,
                         add_taint, false_insp);
      v = scheme_box(result);
    } else if (SCHEME_VECTORP(v)) {
      Scheme_Object *v2;
      int size = SCHEME_VEC_SIZE(v), i;
      
      v2 = scheme_make_vector(size, NULL);
      
      for (i = 0; i < size; i++) {
        result = SCHEME_VEC_ELS(v)[i];
        result = propagate(result, to_propagate, stx->marks,
                           shifts,
                           add_taint, false_insp);
      	SCHEME_VEC_ELS(v2)[i] = result;
      }
      
      v = v2;
    } else if (SCHEME_HASHTRP(v)) {
      Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)v, *ht2;
      Scheme_Object *key, *val;
      mzlonglong i;

      ht2 = scheme_make_hash_tree(SCHEME_HASHTR_FLAGS(ht) & 0x3);

      i = scheme_hash_tree_next(ht, -1);
      while (i != -1) {
        scheme_hash_tree_index(ht, i, &key, &val);
        val = propagate(val, to_propagate, stx->marks,
                        shifts,
                        add_taint, false_insp);
        ht2 = scheme_hash_tree_set(ht2, key, val);
        i = scheme_hash_tree_next(ht, i);
      }

      v = (Scheme_Object *)ht2;
    } else if (prefab_p(v)) {
      Scheme_Structure *s = (Scheme_Structure *)v;
      Scheme_Object *r;
      int size, i;

      s = (Scheme_Structure *)scheme_clone_prefab_struct_instance(s);
      
      size = s->stype->num_slots;
      for (i = 0; i < size; i++) {
        r = s->slots[i];
        r = propagate(r, to_propagate, stx->marks,
                      shifts,
                      add_taint, false_insp);
        s->slots[i] = r;
      }

      v = (Scheme_Object *)s;
    }

    if (!keep) 
      return v;

    stx->u.to_propagate = NULL;
    stx->val = v;
    if (add_taint) {
      /* if we're setting taints, we must be propagating
         taints to touch; change "taints" to "propagated" or "none": */
      stx->taints = scheme_true;
    } else if (false_insp) {
      /* If we're propagating an inspector with no dye packs,
         we're now done propagating. */
      if (!SCHEME_PAIRP(stx->taints))
        stx->taints = NULL;
    }
    if (shifts)
      stx->shifts = SCHEME_VEC_ELS(shifts)[0];
  }

  return stx->val;
}

Scheme_Object *scheme_stx_content(Scheme_Object *o)
/* Propagates wraps while getting a syntax object's content. */
{
  Scheme_Stx *stx = (Scheme_Stx *)o;

  if (!(STX_KEY(stx) & STX_ARMED_FLAG) || !is_armed((Scheme_Object *)stx))
    return raw_stx_content(o);

  /* force propagation: */
  raw_stx_content(o);

  /* taint */
  o = add_taint_to_stx(o, 1);

  /* return tainted content */
  return raw_stx_content(o);
}

/******************** taints ********************/

static Scheme_Object *taint_intern(Scheme_Object *v)
{
  Scheme_Bucket *b;

  scheme_start_atomic();
  b = scheme_bucket_from_table(taint_intern_table, (const char *)v);
  scheme_end_atomic_no_swap();
  if (!b->val)
    b->val = scheme_true;
  v = (Scheme_Object *)HT_EXTRACT_WEAK(b->key);

  return v;
}

static int is_tainted(Scheme_Object *v)
{
  v = ((Scheme_Stx *)v)->taints; 
  if (!v) return 0;
  if (SCHEME_VOIDP(v) || SAME_OBJ(v, scheme_true)) return 1;
  return 0;
}

static int is_clean(Scheme_Object *v)
{
  v = ((Scheme_Stx *)v)->taints; 
  if (!v) return 1;
  if (SCHEME_INSPECTORP(v)) return 1;  
  return 0;
}

static int is_armed(Scheme_Object *v)
{
  v = ((Scheme_Stx *)v)->taints; 
  if (!v) return 0;
  if (SCHEME_PAIRP(v)) return 1;
  return 0;
}

int scheme_stx_is_tainted(Scheme_Object *v)
{
  return is_tainted(v);
}

int scheme_stx_is_clean(Scheme_Object *v)
{
  return is_clean(v);
}

static int has_taint_arming(Scheme_Object *l, Scheme_Object *t, Scheme_Object *false_insp)
{ 
  Scheme_Object *a;

  for (; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    a = SCHEME_CAR(l);
    if (SCHEME_FALSEP(a)) a = false_insp;
    if (SAME_OBJ(a, t))
      return 1;
  }
  return 0;
}

static Scheme_Object *add_taint_to_stx(Scheme_Object *o, int need_clone)
{
  Scheme_Stx *stx;
  
  if (is_tainted(o))
    return o;

  if (need_clone)
    o = clone_stx(o);
  stx = (Scheme_Stx *)o;
  stx->taints = scheme_void; /* taint to propagate */

  /* Set to_propagate to indicate taint to propagate: */
  if (STX_KEY(stx) & STX_SUBSTX_FLAG) {
    if (!stx->u.to_propagate)
      stx->u.to_propagate = scheme_empty_hash_tree;
    if (STX_KEY(stx) & STX_ARMED_FLAG)
      STX_KEY(stx) -= STX_ARMED_FLAG;
  }
  
  return o;
}

static Scheme_Object *set_false_insp(Scheme_Object *o, Scheme_Object *false_insp, int need_clone)
{
  Scheme_Stx *stx;

  if (is_tainted(o))
    return o;
  else if (is_armed(o)) {
    if (SCHEME_TRUEP(SCHEME_CAR(((Scheme_Stx *)o)->taints)))
      return o;
  } else {
    if (((Scheme_Stx *)o)->taints)
      /* `taints' must be an inspector already */
      return o;
  }

  if (need_clone)
    o = clone_stx(o);
  stx = (Scheme_Stx *)o;
  if (stx->taints)
    false_insp = taint_intern(scheme_make_pair(false_insp, SCHEME_CDR(stx->taints)));
  stx->taints = false_insp;

  /* Set lazy_prefix to indicate inspector to propagate: */
  if (STX_KEY(stx) & STX_SUBSTX_FLAG) {
    if (!stx->u.to_propagate)
      stx->u.to_propagate = scheme_empty_hash_tree;
  }
  
  return o;
}

static Scheme_Object *do_add_taint_armings_to_stx(Scheme_Object *o, Scheme_Object *taint_armings, int many, int need_clone)
/* If `many', `taint_armings' should be taint-interned. */
{
  Scheme_Object *l, *taints, *new_taints, *false_insp, *alt_false_insp, *a;
  Scheme_Stx *stx;

  taints = ((Scheme_Stx *)o)->taints;
  if (taints) {
    if (SAME_OBJ(taints, scheme_true) || SCHEME_VOIDP(taints))
      /* tainted */
      return o;
    else if (SCHEME_INSPECTORP(taints)) {
      false_insp = taints;
      taints = NULL;
    } else {
      false_insp = SCHEME_CAR(taints);
      taints = SCHEME_CDR(taints);
    }
  } else
    false_insp = scheme_true; /* block future attempts to propagate a false_insp */

  if (!taints) {
    if (many)
      new_taints = taint_armings;
    else {
      new_taints = taint_intern(scheme_make_pair(taint_armings, scheme_null));
      new_taints = taint_intern(scheme_make_pair(false_insp, new_taints));
    }
  } else {
    new_taints = taints;

    if (many) {
      alt_false_insp = SCHEME_CAR(taint_armings);
      taint_armings = SCHEME_CDR(taint_armings);
    } else
      alt_false_insp = scheme_false;

    for (l = taint_armings; !SCHEME_NULLP(l); ) {
      a = many ? SCHEME_CAR(l) : l;
      if (SCHEME_FALSEP(a)) a = alt_false_insp;
      if (!has_taint_arming(new_taints, a, false_insp)) {
        new_taints = taint_intern(scheme_make_pair(a, new_taints));
      }
      if (many)
        l = SCHEME_CDR(l);
      else
        l = scheme_null;
    }

    if (SAME_OBJ(taints, new_taints))
      return o;

    new_taints = taint_intern(scheme_make_pair(false_insp, new_taints));
  }

  if (need_clone)
    o = clone_stx(o);
  stx = (Scheme_Stx *)o;
  stx->taints = new_taints;
  
  if (STX_KEY(stx) & STX_SUBSTX_FLAG)
    STX_KEY(stx) |= STX_ARMED_FLAG;
  
  return o;
}

static Scheme_Object *add_taint_arming_to_stx(Scheme_Object *o, Scheme_Object *taint, int need_clone)
{
  return do_add_taint_armings_to_stx(o, taint, 0, need_clone);
}

static Scheme_Object *add_taint_armings_to_stx(Scheme_Object *o, Scheme_Object *taints, int need_clone)
{
  return do_add_taint_armings_to_stx(o, taints, 1, need_clone);
}

Scheme_Object *scheme_stx_taint(Scheme_Object *o)
{
  return add_taint_to_stx(o, 1);
}

Scheme_Object *scheme_stx_taint_arm(Scheme_Object *o, Scheme_Object *insp)
{
  if (is_tainted(o))
    return o;
  else
    return add_taint_arming_to_stx(o, insp, 1);
}

Scheme_Object *scheme_stx_taint_rearm(Scheme_Object *o, Scheme_Object *copy_from)
{
  if (is_tainted(o) || is_clean(copy_from))
    return o;
  else if (is_tainted(copy_from))
    return add_taint_to_stx(o, 1);
  else
    return add_taint_armings_to_stx(o, ((Scheme_Stx *)copy_from)->taints, 1);
}

 static int is_same_or_subinspector(Scheme_Object *sub, Scheme_Object *sup, Scheme_Object *false_insp)
{
  if (SCHEME_FALSEP(sub)) sub = false_insp;
  if (SAME_OBJ(sub, sup)) return 1;
  return scheme_is_subinspector(sub, sup);
}

Scheme_Object *scheme_stx_taint_disarm(Scheme_Object *o, Scheme_Object *insp)
{
  Scheme_Object *l, *l2, *a, *taint_armings, *false_insp;
  
  if (is_tainted(o) || is_clean(o))
    return o;

  taint_armings = ((Scheme_Stx *)o)->taints;
  false_insp = SCHEME_CAR(taint_armings);
  taint_armings = SCHEME_CDR(taint_armings);

  if (insp) {
    for (l = taint_armings; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      a = SCHEME_CAR(l);
      if (is_same_or_subinspector(a, insp, false_insp)) {
        break;
      }
    }
    if (SCHEME_NULLP(l))
      return o;

    l2 = scheme_null;
    for (l = taint_armings; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      a = SCHEME_CAR(l);
      if (!is_same_or_subinspector(a, insp, false_insp)) {
        l2 = taint_intern(scheme_make_pair(a, l2));
      }
    }
  } else
    l2 = scheme_null;

  o = clone_stx(o);

  if (SCHEME_NULLP(l2)) {
    if (SCHEME_INSPECTORP(false_insp))
      ((Scheme_Stx *)o)->taints = false_insp;
    else
      ((Scheme_Stx *)o)->taints = NULL;
    if (STX_KEY(((Scheme_Stx *)o)) & STX_ARMED_FLAG)
      STX_KEY(((Scheme_Stx *)o)) -= STX_ARMED_FLAG;
  } else {
    l2 = taint_intern(scheme_make_pair(false_insp, l2));    
    ((Scheme_Stx *)o)->taints = l2;
  }

  return o;
}

/******************** bindings ********************/

Scheme_Mark *extract_max_mark_and_increment_bindings(Scheme_Hash_Tree *marks)
{
  intptr_t i;
  Scheme_Object *key, *val;
  Scheme_Mark *mark;
  mzlonglong mark_id_val, id_val;

  i = scheme_hash_tree_next(marks, -1);
  scheme_hash_tree_index(marks, i, &key, &val);

  mark = (Scheme_Mark *)key;
  mark_id_val = mark->id;
  if (mark_id_val < 0) mark_id_val = -mark_id_val;
  mark->binding_version++;

  i = scheme_hash_tree_next(to_propagate, i);
  while (i != -1) {
    scheme_hash_tree_index(marks, i, &key, &val);

    ((Scheme_Mark *)key)->binding_version++;

    id_val = ((Scheme_Mark *)key)->id;
    if (id_val < 0) id_val = -id_val;

    if (id_val > mark_id_val) {
      mark = (Scheme_Mark *)key;
      mark_id_val = id_val;
    }
    
    i = scheme_hash_tree_next(to_propagate, i);
  }

  return mark;
}

void scheme_add_binding(Scheme_Object *sym_or_pes, Scheme_Object *phase, Scheme_Object *_marks,
                        Scheme_Object *val)
{
  Scheme_Bucket_Table *bt;
  Scheme_Bucket *b;
  Scheme_Hash_Tree *marks = (Scheme_Hash_Tree *)_marks;
  Scheme_Mark *mark;
  Scheme_Object *l, *p, *vec;
  
  if (marks->count) {
    mark = extract_max_mark_and_increment-bindings(marks);
  } else {
    scheme_signal_error("cannot bind identifier with an empty context");
  }

  /* We add the binding the the maximum-valued mark, because it's
     likely to be in the least number of binding sets so far. */

  l = mark->bindings;
  if (!l) {
    bt = scheme_make_bucket_table(10, SCHEME_hash_weak_ptr);
    l = scheme_make_raw_pair(bt, NULL);
    mark->bindings = l;
  } else {
    bt = (Scheme_Bucket_Table *)SCHEME_CAR(l);
  }

  vec = scheme_make_vector(3, NULL);
  SCHEME_VEC_ELS(vec)[0] = phase;
  SCHEME_VEC_ELS(vec)[1] = marks;

  if (SCHEME_SYMBOLP(sym_or_pes)) {
    b = scheme_bucket_from_table(bt, (const char *)sym_or_pes);
    if (!b->val)
      b->val = scheme_null;
    SCHEME_VEC_ELS(vec)[2] = val;
    l = scheme_make_pair(vec, b->val);
    b->val = l;
  } else {
    SCHEME_VEC_ELS(vec)[2] = sym_or_pes;
    p = scheme_make_raw_pair(vec, SCHEME_CDR(l));
    SCHEME_CDR(l) = p;
  }
}

void scheme_add_binding_from_id(Scheme_Object *o, Scheme_Object *phase,
                                Scheme_Object *val)
{
  Scheme_Stx *stx = (Scheem_Stx *)o;

  phase_shift = extract_phase_shift(stx);
  if (SCHEME_FALSEP(phase_shift)) {
    if (!SCHEME_FALSEP(phase))
      scheme_signal_error("cannot bind given identifer in a non-label phase level;\n"
                          " identifier is shifted to the label phase level");
  } else if (!SCHEME_FALSEP(phase))
    phase = scheme_bin_minus(phase, phase_shift);
 
  scheme_add_binding(stx->val, phase, marks, val);
}

static void *do_scheme_stx_lookup(Scheme_Stx *stx, Scheme_Object *phase, Scheme_Hash_Tree *check_subset)
{
  int j;
  intptr_t i;
  Scheme_Object *key, *val, *result_best_so_far, **cached_result, *binding_phase;
  Scheme_Mark *mark, *mark_best_so_far;
  Scheme_Hash_Tree *binding_marks, *best_so_far;
  Scheme_Module_Phase_Exports *pt;

  best_so_far = NULL;
  mark_best_so_far = NULL;
  result_best_so_far = NULL;

  i = scheme_hash_tree_next(stx->marks, -1);
  while (i != -1) {
    scheme_hash_tree_index(stx->marks, i, &key, &val);

    mark = (Scheme_Mark *)key;
    if (mark->bindings) {
      for (j = 0; j < 2; j++) {
        if (!j)
          l = scheme_lookup_in_table(SCHEME_CAR(mark->bindings), stx->val);
        else
          l = SCHEME_CDR(mark->bindings);

        while (l && !SCHEME_NULLP(l)) {
          binding_phase = (Scheme_Hash_Tree *)(SCHEME_VEC_ELS(SCHEME_CAR(l))[0]);
          if (same_phase(phase, binding_phase)) {
            bindings_marks = (Scheme_Hash_Tree *)(SCHEME_VEC_ELS(SCHEME_CAR(l))[1]);

            if (j) {
              if (SCHEME_VEC_SIZE(SCHEME_CAR(l)) == 3) {
                if (SCHEME_TRUEP(SCHEME_VEC_ELS(SCHEME_CAR(l))[0])) {
                  /* Need unmarshal --- but only if the mark set is relevant */
                  if (marks_subset(binding_marks, stx->marks)) {
                    /* unmarshal and restart this iteration of the loop */
                    unmarshal_rename(stx, SCHEME_CAR(l));
                    j = 0;
                    break;
                  }
                }
                bindings_marks = NULL;
              } else {
                /* Check for id in pes */
                pt = (Scheme_Module_Phase_Exports *)SCHEME_VEC_ELS(SCHEME_CAR(l))[1];
                if (!pt->ht) {
                  /* Lookup table (which is created lazily) not yet created, so do that now... */
                  scheme_populate_pt_ht(pt);
                }
                
                if (!scheme_hash_get(pt->ht, stx->val))
                  binding_marks = NULL;
              }
            }

            if (binding_marks && marks_subset(binding_marks, stx->marks)) {
              if (check_subset && !marks_subset(binding_marks, check_subset))
                return NULL; /* ambiguous */
              if (!best_so_far
                  || ((bindings_marks->count > best_so_far->count)
                      && (!check_subset
                          || (bindings_marks->count == check_subset->count)))) {
                best_so_far = bindings_marks;
                mark_best_so_far = mark;
                result_best_so_far = SCHEME_VEC_ELS(SCHEME_CAR(l))[2];
              }
            }
          }
          l = SCHEME_CDR(l);
        }
      }
    }
    
    i = scheme_hash_tree_next(to_propagate, i);
  }

  if (!best_so_far)
    return NULL;

  if (check_subset) {
    cached_result = MALLOC_N(Scheme_Object*, 4);
    cached_result[0] = result_best_so_far;
    cached_result[1] = scheme_make_integer(mark_best_so_far->binding_version);
    cached_result[2] = (Scheme_Object *)mark_best_so_far;
    
    return cached_result;
  } else
    return best_so_far;
}

Scheme_Object *scheme_stx_lookup_w_nominal(Scheme_Object *o, Scheme_Object *phase,
                                           Scheme_Object **insp,              /* access-granting inspector */
                                           Scheme_Object **nominal_modidx,    /* how it was imported */
                                           Scheme_Object **nominal_name,      /* imported as name */
                                           Scheme_Object **src_phase,         /* phase level of import from nominal modidx */ 
                                           Scheme_Object **nominal_src_phase) /* phase level of export from nominal modidx */
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Hash_Tree *best_set;
  Scheme_Object **cached_result, *phase_shift, *result;

  if (stx->u.cached_binding) {
    if (SAME_OBJ(stx->u.cached_binding[3], phase)
        && (SCHEME_INT_VAL(stx->u.cached_binding[1])
            == (((Scheme_Mark *)stx->u.cached_binding[2])->binding_version)))
      return stx->u.cached_binding[0];
  }

  phase_shift = extract_phase_shift(stx);
  if (!SAME_OBJ(phase_shift, scheme_make_integer(0))) {
    if (SCHEME_FALSEP(phase_shift)) {
      if (!SCHEME_FALSEP(phase))
        return NULL;
    } else if (SCHEME_FALSEP(phase))
      phase_shift = scheme_false;
    else
      phase_shift = scheme_bin_add(phase, phase_shift);
  } else
    phase_shift = phase;

  best_set = (Scheme_Hash_Tree *)do_scheme_stx_lookup(stx, phase_shift, NULL);
  if (!best_set)
    return NULL;

  /* Find again, this time checking to ensure no ambiguity: */
  cached_result = (Scheme_Object **)do_scheme_stx_lookup(stx, phase_shift, best_set);
  if (!cached_result)
    return NULL;

  result = cached_result[0];

  /*
    `result` can be:
      - a symbol for a lexical binding,
      - a list for a module import
      - a vector for a pes (shared export table from a module)
  */
  if (!SCHEME_SYMBOLP(result)) {
    /* Generate a result vector: (vector <modidx> <sym> <phase>) */
    Scheme_Object *l = result;
    result = scheme_make_vector(3);
    SCHEME_VEC_ELS(result)[1] = stx->val;
    SCHEME_VEC_ELS(result)[2] = scheme_make_integer(0);

    if (nominal_modidx) *nominal_modidx = NULL;
    if (nominal_name) *nominal_name = NULL;
    if (src_phase) *src_phase = NULL;
    if (nominal_src_phase) *nominal_src_phase = NULL;

    if (SCHEME_PAIRP(l)) {
      /* A list for a module import */
      if (SCHEME_INSPECTORP(SCHEME_CAR(l))) {
        if (insp) *insp = SCHEME_CAR(l);
        l = SCHEME_CDR(l);
      } else {
        if (inspe) *insp = scheme_false;
      }
      SCHEME_VEC_ELS(result)[0] = SCHEME_CAR(l);
      l = SCHEME_CDR(l);
      if (SCHEME_SYMBOLP(l)) {
        /* l is exportname */
        SCHEME_VEC_ELS(result)[1] = l;
      } else if (SCHEME_MODIDXP(l)) {
        /* l is nominal_modidx */
        if (nominal_modidx) *nominal_modidx = l;
      } else {
        if (SCHEME_INTP(SCHEME_CAR(l)) || SCHEME_BIGNUMP(SCHEME_CAR(l))) {
          /* mod-phase before rest */
          SCHEME_VEC_ELS(result)[2] = SCHEME_CAR(l);
          l = SCHEME_CDR(l);
        }
        /* l is (list* exportname nominal_modidx_plus_phase nominal_exportname) */
        SCHEME_VEC_ELS(result)[1] = SCHEME_CAR(l);
        l = SCHEME_CDR(l);
        if (nominal_name)
          *nominal_name = SCHEME_CDR(l);
        l = SCHEME_CAR(l);
        /* l is nominal_modidx_plus_phase */
        if (SCHEME_PAIRP(l)) {
          if (nominal_modidx) *nominal_modidx = SCHEME_CAR(l);
          l = SCHEME_CDR(l);
          if (SCHEME_PAIRP(l)) {
            if (src_phase) *src_phase = SCHEME_CAR(l);
            if (nominal_src_phase) *nominal_src_phase = SCHEME_CDR(l);
          } else
            if (src_phase) *src_phase = l;
        } else {
          if (nominal_modidx) *nominal_modidx = l;
        }
      }
    } else {
      /* A vector for a pes */
      Scheme_Module_Phase_Exports *pt;
      
      pt = (Scheme_Module_Phase_Exports *)SCHEME_VEC_ELS(l)[1];
      
      SCHEME_VEC_ELS(result)[0] = SCHEME_VEC_ELS(l)[0];
      SCHEME_VEC_ELS(result)[1] = pt->provide_src_names[i];
      if (src_phase) *src_phase = pt->phase_index;
      if (nominal_src_phase) *nominal_src_phase = SCHEME_VEC_ELS(l)[2];
    }

    if (nominal_name && !*nominal_name)
      *nominal_name = SCHEME_VEC_ELS(result)[1];
    if (nominal_modidx && !*nominal_modidx)
      *nominal_modidx = SCHEME_VEC_ELS(result)[0];
    if (src_phase && !*src_phase)
      *src_phase = SCHEME_VEC_ELS(result)[2];
    if (nominal_src_phase && !*nominal_src_phase)
      *nominal_src_phase = *src_phase;

    l = apply_modidx_shifts(stx->shifts, SCHEME_VEC_ELS(result)[0], insp);
    SCHEME_VEC_ELS(result)[0] = l;

    if (nominal_modidx) {
      l = apply_modidx_shifts(stx->shifts, *nominal_modidx, NULL);
      *nominal_modidx = l;
    }

    cached_result[0] = result;
  }
     

  cached_result[3] = phase;
  stx->u.cached_binding = cached_result;

  return result;
}

Scheme_Object *scheme_stx_lookup(Scheme_Object *o, Scheme_Object *phase)
{
  return scheme_stx_lookup_w_nominal(o, phase, NULL, NULL, NULL, NULL, NULL);
}

/******************** module-import bindings ********************/

Scheme_Object *scheme_make_module_rename_set(Scheme_Object *insp)
{
  Scheme_Object *vec, *mark;

  mark = scheme_new_mark(0);

  vec = scheme_make_vector(2);
  SCHEME_VEC_ELS(vec)[0] = mark;
  SCHEME_VEC_ELS(vec)[1] = insp;

  return vec;
}

Scheme_Object *scheme_make_module_rename(Scheme_Object *phase, Scheme_Object *insp, Scheme_Object *set_identity)
{
  Scheme_Object *vec;

  if (!set_identity)
    set_identity = scheme_new_mark(0);

  vec = scheme_make_vector(3);
  SCHEME_VEC_ELS(vec)[0] = set_identity;
  SCHEME_VEC_ELS(vec)[1] = phase;
  SCHEME_VEC_ELS(vec)[2] = insp;

  return vec;
}

Scheme_Object *scheme_get_module_rename_from_set(Scheme_Object *set, Scheme_Object *phase)
{
  return scheme_make_module_rename(phase, SCHEME_VEC_ELS(set)[1], SCHEME_VEC_ELS(set)[0]);
}

Scheme_Object *scheme_rename_set_identity(Scheme_Object *rn_set)
{
  return SCHEME_VEC_ELS(rn_set)[0];
}

void scheme_extend_module_rename(Scheme_Object *mrn,         /* (vector <mark> <phase> <inspector>) */
                                 Scheme_Object *modname,     /* actual source module */
                                 Scheme_Object *localname,   /* name in local context */
                                 Scheme_Object *exname,      /* name in definition context  */
                                 Scheme_Object *nominal_mod, /* nominal source module */
                                 Scheme_Object *nominal_ex,  /* nominal import before local renaming */
                                 intptr_t mod_phase,         /* phase of source defn */
                                 Scheme_Object *src_phase,   /* nominal import phase */
                                 Scheme_Object *nom_phase)   /* nominal export phase */
{
  Scheme_Object *elem;
  Scheme_Object *phase;

  phase = SCHEME_VEC_ELS(mrn)[1];

 /*
   binding ::= mod_binding
   .        |  (cons inspector mod_binding)
   mod_binding ::=  modidx
   .            |   (cons modidx exportname)
   .            |   (cons modidx nominal_modidx)
   .            |   (list* modidx exportname nominal_modidx_plus_phase nominal_exportname)
   .            |   (list* modidx mod-phase exportname nominal_modidx_plus_phase nominal_exportname)
   nominal_modix_plus_phase ::= nominal_modix
   .                         |  (cons nominal_modix import_phase_plus_nominal_phase)
   import_phase_plus_nominal_phase ::= import-phase-index
   .                                |  (cons import-phase-index nom-phase)
 */

  if (!src_phase)
    src_phase = phase;
  if (!nom_phase)
    nom_phase = scheme_make_integer(mod_phase);

  if (SAME_OBJ(modname, nominal_mod)
      && SAME_OBJ(exname, nominal_ex)
      && !mod_phase
      && same_phase(src_phase, phase)
      && same_phase(nom_phase, scheme_make_integer(mod_phase))) {
    if (SAME_OBJ(localname, exname))
      elem = modname;
    else
      elem = CONS(modname, exname);
  } else if (SAME_OBJ(exname, nominal_ex)
	     && SAME_OBJ(localname, exname)
	     && !mod_phase
             && same_phase(src_phase, phase)
             && same_phase(nom_phase, scheme_make_integer(mod_phase))) {
    /* It's common that a sequence of similar mappings shows up,
       e.g., '(#%kernel . mzscheme) */
    if (nominal_ipair_cache
	&& SAME_OBJ(SCHEME_CAR(nominal_ipair_cache), modname)
	&& SAME_OBJ(SCHEME_CDR(nominal_ipair_cache), nominal_mod))
      elem = nominal_ipair_cache;
    else {
      elem = ICONS(modname, nominal_mod);
      nominal_ipair_cache = elem;
    }
  } else {
    if (same_phase(nom_phase, scheme_make_integer(mod_phase))) {
      if (same_phase(src_phase, phase))
        elem = nominal_mod;
      else
        elem = CONS(nominal_mod, src_phase);
    } else {
      elem = CONS(nominal_mod, CONS(src_phase, nom_phase));
    }
    elem = CONS(exname, CONS(elem, nominal_ex));
    if (mod_phase)
      elem = CONS(scheme_make_integer(mod_phase), elem);
    elem = CONS(modname, elem);
  }

  if (SCHEME_FALSEP(inspector))
    elem = CONS(inspector, elem);

  scheme_add_binding(localname, phase, SCHEME_VEC_ELS(mrn)[0], elem);
}

void scheme_extend_module_rename_with_shared(Scheme_Object *rn, /* (vector <mark> <phase> <inspector>) */
                                             Scheme_Object *modidx,      
                                             Scheme_Module_Phase_Exports *pt, 
                                             Scheme_Object *unmarshal_info,
                                             Scheme_Object *src_phase, /* nominal import phase */
                                             Scheme_Object *context,
                                             int save_unmarshal)
{
  Scheme_Stx *stx = (Scheme_Stx *)context;
  Scheme_Object *phase, *phase_shift, *pes;
  Scheme_hash_Tree *marks;

  phase = SCHEME_VEC_ELS(rn)[1];

  if (context) {
    phase_shift = extract_phase_shift(stx);
    if (SCHEME_FALSEP(phase_shift)) {
      if (!SCHEME_FALSEP(phase))
        scheme_signal_error("cannot bind with given context in a non-label phase level;\n"
                            " context is shifted to the label phase level");
    } else if (!SCHEME_FALSEP(phase))
      phase = scheme_bin_minus(phase, phase_shift);

    marks = context->marks;
  } else
    marks = add_remove_mark(scheme_empty_hash_tree, SCHEME_VEC_ELS(rn)[0]);

  if (unmarshal_info)
    unmarshal_info = scheme_make_pair(scheme_make_pair(rn, context),
                                      unmarshal_info);

  pes = scheme_make_vector(4);
  SCHEME_VEC_ELS(pes)[0] = modidx;
  SCHEME_VEC_ELS(pes)[1] = (Scheme_Object *)pt;
  SCHEME_VEC_ELS(pes)[2] = src_phase;
  SCHEME_VEC_ELS(pes)[3] = (unmarshal_info ? unmarshal_info : scheme_false);
 
  scheme_add_binding((Scheme_Object *)pt, phase, marks, pes);
}

static void unmarshal_rename(Scheme_Stx *stx, Scheme_Object *vec)
{
  Scheme_Object *modidx, *export_registry, *unmarshal_info, *rn, *context;

  modidx = SCHEME_VEC_ELS(vec)[0];

  modidx = apply_modidx_shifts(stx->shifts, modidx, NULL);
  export_registry = extract_export_registry(stx->shifts);

  src_phase = SCHEME_VEC_ELS(vec)[1];
  unmarshal_info = SCHEME_VEC_ELS(vec)[2];

  SCHEME_VEC_ELS(vec)[0] = scheme_false;
  SCHEME_VEC_ELS(vec)[1] = scheme_false;
  SCHEME_VEC_ELS(vec)[2] = scheme_false;

  rn = SCHEME_CAR(SCHEME_CAR(unmarshal_info));
  context = SCHEME_CDR(SCHEME_CAR(unmarshal_info));
  unmarshal_info = SCHEME_CDR(unmarshal_info);

  scheme_do_module_rename_unmarshal(modidx, rn, context, src_phase,
                                    unmarshal_info, export_registry);
}

/***********************************************************************/

void scheme_load_delayed_syntax(struct Resolve_Prefix *rp, intptr_t i)
{
  Scheme_Object *stx;
  int c;

  stx = scheme_load_delayed_code(SCHEME_INT_VAL(rp->stxes[i]),
                                 (struct Scheme_Load_Delay *)SCHEME_CDR(rp->delay_info_rpair));
  rp->stxes[i] = stx;
  c = SCHEME_INT_VAL(SCHEME_CAR(rp->delay_info_rpair));
  --c;
  SCHEME_CAR(rp->delay_info_rpair) = scheme_make_integer(c);
  if (!c) {
    SCHEME_CDR(rp->delay_info_rpair) = NULL;
    rp->delay_info_rpair = NULL;
  } 
}

Scheme_Object *scheme_delayed_rename(Scheme_Object **o, intptr_t i)
{
  Scheme_Object *rename, *v;
  Resolve_Prefix *rp;

  rename = o[0];

  if (!rename) return scheme_false; /* happens only with corrupted .zo! */

  rp = (Resolve_Prefix *)o[1];

  v = rp->stxes[i];

  if (SCHEME_INTP(v)) {
    scheme_load_delayed_syntax(rp, i);
    v = rp->stxes[i];
  }

  v = scheme_add_rename(v, rename);
  
  /* Phase shift... */
  rename = SCHEME_BOX_VAL(rename);
  rename = SCHEME_VEC_ELS(rename)[4];
  if (!SCHEME_FALSEP(rename)) {
    /* need to propagate the inspector for dye packs, too */
    (void)set_false_insp((Scheme_Object *)v, rename, 0);
  }

  return v;
}

/*========================================================================*/
/*                           stx comparison                               */
/*========================================================================*/

int scheme_stx_module_eq3(Scheme_Object *a, Scheme_Object *b, 
                          Scheme_Object *a_phase, Scheme_Object *b_phase)
{
  Scheme_Object *a_bind, *b_bind;

  a_bind = scheme_stx_lookup(a, a_phase);
  b_bind = scheme_stx_lookup(b, b_phase);

  if (SCHEME_SYMBOLP(a_bind) || SCHEME_SYMBOLP(b_bind))
    return SAME_OBJ(a_bind, b_bind);

  /* Comparison of names & definition phases is fast, so try that next: */
  if (!SAME_OBJ(SCHEME_VEC_ELS(a_bind)[1], SCHEME_VEC_ELS(b_bind)[1])
      || !SAME_OBJ(SCHEME_VEC_ELS(a_bind)[2], SCHEME_VEC_ELS(b_bind)[2]))
    return 0;

  /* Need to compare modidxs: */

  a_bind = scheme_module_resolve(SCHEME_VEC_ELS(a_bind)[0], 0);
  b_bind = scheme_module_resolve(SCHEME_VEC_ELS(b_bind)[0], 0);

  return SAME_OBJ(a, b);
}

int scheme_stx_module_eq2(Scheme_Object *a, Scheme_Object *b, Scheme_Object *phase)
{
  return scheme_stx_module_eq3(a, b, phase, phase);
}

int scheme_stx_module_eq(Scheme_Object *a, Scheme_Object *b, intptr_t phase)
{
  return scheme_stx_module_eq3(a, b, scheme_make_integer(phase), scheme_make_integer(phase))
}

int scheme_stx_module_eq_x(Scheme_Object *a, Scheme_Object *b, intptr_t b_phase)
{
  return scheme_stx_module_eq3(a, b, scheme_make_integer(0), scheme_make_integer(b_phase));
}

Scheme_Object *scheme_stx_get_module_eq_sym(Scheme_Object *a, Scheme_Object *phase)
{
  if (SCHEME_STXP(a)) {
    a = scheme_stx_lookup(a, phase);
    if (SCHEME_VECTORP(a))
      return SCHEME_VEC_ELS(a)[1];
    else
      return a;
  } else
    return a;
}

int scheme_stx_env_bound_eq2(Scheme_Object *_a, Scheme_Object *_b,
                             Scheme_Object *a_phase, Scheme_Object *b_phase)
{
  Scheme_Stx *a = (Scheme_Stx *)_a;
  Scheme_Stx *b = (Scheme_Stx *)_b;

  if (!SAME_OBJ(a->val, b->val))
    return 0;

  return scheme_hash_tree_equal(a->marks, b->marks);
}

int scheme_stx_bound_eq(Scheme_Object *a, Scheme_Object *b, Scheme_Object *phase)
{
  return scheme_stx_env_bound_eq2(a, b, phase, phase);
}

Scheme_Object *scheme_stx_source_module(Scheme_Object *stx, int resolve, int source)
{
  /* Inspect the wraps to look for a self-modidx shift: */
  WRAP_POS w;
  Scheme_Object *srcmod = scheme_false, *chain_from = NULL, *er;
  Scheme_Hash_Table *export_registry = NULL;

  WRAP_POS_INIT(w, ((Scheme_Stx *)stx)->wraps);

  while (!WRAP_POS_END_P(w)) {
    if (SCHEME_BOXP(WRAP_POS_FIRST(w))) {
      /* Phase shift:  */
      Scheme_Object *vec, *dest, *src;

      vec = SCHEME_PTR_VAL(WRAP_POS_FIRST(w));
      
      src = SCHEME_VEC_ELS(vec)[1];
      dest = SCHEME_VEC_ELS(vec)[2];

      /* If src is #f, shift is just for phase; no redirection */
      if (!SCHEME_FALSEP(src)) {
        
        if (!chain_from) {
          srcmod = dest;
        } else if (!SAME_OBJ(chain_from, dest)) {
          srcmod = scheme_modidx_shift(dest,
                                       chain_from,
                                       srcmod);
        }
        
        chain_from = src;

        if (!export_registry) {
          er = SCHEME_VEC_ELS(vec)[3];
          if (SCHEME_TRUEP(er))
            export_registry = (Scheme_Hash_Table *)er;
        }
      }
    }

    WRAP_POS_INC(w);
  }

  if (SCHEME_TRUEP(srcmod)) {
    if (resolve) {
      srcmod = scheme_module_resolve(srcmod, 0);
      if (export_registry && source) {
        er = scheme_hash_get(export_registry, srcmod);
        if (er)
          srcmod = ((Scheme_Module_Exports *)er)->modsrc;
      }
      srcmod = SCHEME_PTR_VAL(srcmod);
    }
  }

  return srcmod;
}

/*========================================================================*/
/*                           stx and lists                                */
/*========================================================================*/

int scheme_stx_list_length(Scheme_Object *list)
{
  int len;

  if (SCHEME_STXP(list))
    list = SCHEME_STX_VAL(list);

  len = 0;
  while (!SCHEME_NULLP(list)) {
    if (SCHEME_STXP(list))
      list = SCHEME_STX_VAL(list);
    if (SCHEME_PAIRP(list)) {
      len++;
      list = SCHEME_CDR(list);
    } else {
      if (!SCHEME_NULLP(list))
	len++;
      break;
    }
  }

  return len;
}

int scheme_stx_proper_list_length(Scheme_Object *list)
{
  int len;
  Scheme_Object *turtle;

  if (SCHEME_STXP(list))
    list = SCHEME_STX_VAL(list);

  len = 0;
  turtle = list;
  while (SCHEME_PAIRP(list)) {
    len++;

    list = SCHEME_CDR(list);
    if (SCHEME_STXP(list))
      list = SCHEME_STX_VAL(list);

    if (!SCHEME_PAIRP(list))
      break;
    len++;
    list = SCHEME_CDR(list);
    if (SCHEME_STXP(list))
      list = SCHEME_STX_VAL(list);

    if (SAME_OBJ(turtle, list))
      break;

    turtle = SCHEME_CDR(turtle);
    if (SCHEME_STXP(turtle))
      turtle = SCHEME_STX_VAL(turtle);

  }
  
  if (SCHEME_NULLP(list))
    return len;

  return -1;
}

#ifdef DO_STACK_CHECK
static Scheme_Object *flatten_syntax_list_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *l = (Scheme_Object *)p->ku.k.p1;
  int *r = (int *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return scheme_flatten_syntax_list(l, r);
}
#endif

Scheme_Object *scheme_flatten_syntax_list(Scheme_Object *lst, int *islist)
{
  Scheme_Object *l = lst, *lflat, *first, *last;

  /* Check whether the list ends in a null: */
  while (SCHEME_PAIRP(l)) {
    l = SCHEME_CDR(l);
  }

  if (SCHEME_NULLP(l)) {
    /* Yes. We're done: */
    if (islist)
      *islist = 1;
    return lst;
  }

  if (islist)
    *islist = 0;

  lflat = NULL;

  /* Is it a syntax object, possibly with a list? */
  if (SCHEME_STXP(l)) {
    l = scheme_stx_content(l);
    if (SCHEME_NULLP(l) || SCHEME_PAIRP(l)) {
      int lislist;

      lflat = NULL;

#ifdef DO_STACK_CHECK
      {
# include "mzstkchk.h"
	{
	  Scheme_Thread *p = scheme_current_thread;
	  int *r;

	  r = (int *)scheme_malloc_atomic(sizeof(int));

	  p->ku.k.p1 = (void *)l;
	  p->ku.k.p2 = (void *)r;

	  lflat = scheme_handle_stack_overflow(flatten_syntax_list_k);

	  lislist = *r;
	}
      }
#endif

      if (!lflat)
	lflat = scheme_flatten_syntax_list(l, &lislist);

      if (!lislist) {
	/* Not a list. Can't flatten this one. */
	return lst;
      }
    } else {
      /* Not a syntax list. No chance of flattening. */
      return lst;
    }
  } else {
    /* No. No chance of flattening, then. */
    return lst;
  }

  /* Need to flatten, end with lflat */

  if (islist)
    *islist = 1;

  first = last = NULL;
  for (l = lst; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    Scheme_Object *p;
    p = scheme_make_pair(SCHEME_CAR(l), scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;
  }

  if (last)
    SCHEME_CDR(last) = lflat;
  else
    first = lflat;

  return first;
}

/*========================================================================*/
/*                            wraps->datum                                */
/*========================================================================*/

/* Used for marshaling syntax objects. Note that we build a reverse
   list for wraps. (Unmarshaler will reverse it back.) 

   The wraps->datum tools are also used to simplify syntax object (to
   minimize the occupied space among a set of objects). */

#define EXPLAIN_SIMP 0
#if EXPLAIN_SIMP
#define EXPLAIN_S(x) if (explain_simp) x
static int explain_simp = 1;
static void print_skips(Scheme_Object *skips)
{
  while (skips) {
    if (SCHEME_PAIRP(skips)) {
      fprintf(stderr, "  skip %s\n", scheme_write_to_string(SCHEME_CAR(skips), NULL));
      skips = SCHEME_CDR(skips);
    } else {
      fprintf(stderr, "  skip val %s\n", scheme_write_to_string(skips, NULL));
      skips = NULL;
    }
  }
}
#else
#define EXPLAIN_S(x) /* empty */
#endif

static Scheme_Object *extract_free_id_info(Scheme_Object *id)
{
  Scheme_Object *bind;
  Scheme_Object *nominal_modidx;
  Scheme_Object *nominal_name, *nom2;
  Scheme_Object *mod_phase;
  Scheme_Object *src_phase_index;
  Scheme_Object *nominal_src_phase;
  Scheme_Object *lex_env = NULL;
  Scheme_Object *vec, *phase, *insp;
  Scheme_Hash_Table *free_id_recur;

  phase = SCHEME_CDR(id);
  id = SCHEME_CAR(id);

  nom2 = scheme_stx_property(id, nominal_id_symbol, NULL);

  free_id_recur = make_recur_table();
  bind = scheme_stx_module_name(free_id_recur, 
                                &id, phase, &nominal_modidx, &nominal_name,
                                &mod_phase, &src_phase_index, &nominal_src_phase,
                                &lex_env, NULL, &insp, NULL);
  release_recur_table(free_id_recur);

  if (SCHEME_SYMBOLP(nom2))
    nominal_name = nom2;
  if (!nominal_name)
    nominal_name = SCHEME_STX_VAL(id);

  if (!bind)
    return CONS(nominal_name, scheme_false);
  else if (SAME_OBJ(bind, scheme_undefined))
    return CONS(nominal_name, lex_env);
  else {
    vec = scheme_make_vector(8, NULL);
    vec->type = scheme_free_id_info_type;
    SCHEME_VEC_ELS(vec)[0] = bind;
    SCHEME_VEC_ELS(vec)[1] = id;
    SCHEME_VEC_ELS(vec)[2] = nominal_modidx;
    SCHEME_VEC_ELS(vec)[3] = nominal_name;
    SCHEME_VEC_ELS(vec)[4] = mod_phase;
    SCHEME_VEC_ELS(vec)[5] = src_phase_index;
    SCHEME_VEC_ELS(vec)[6] = nominal_src_phase;
    SCHEME_VEC_ELS(vec)[7] = (insp ? insp : scheme_false);
    return vec;
  }
}

static int not_in_rename(Scheme_Object *constrain_to_syms, Scheme_Object *rename)
{
  int istart, iend, ri;
  Scheme_Object *renamed, *s;

  while (SCHEME_PAIRP(constrain_to_syms)) {
  
    s = SCHEME_CAR(constrain_to_syms);
    extract_lex_range(rename, s, &istart, &iend);
    
    for (ri = istart; ri < iend; ri++) {
      renamed = SCHEME_VEC_ELS(rename)[2+ri];
      if (SAME_OBJ(renamed, s))
        return 0;
    }

    constrain_to_syms = SCHEME_CDR(constrain_to_syms);
  }
  
  return 1;
}

static int not_in_rib(Scheme_Object *constrain_to_syms, Scheme_Lexical_Rib *rib)
{
  for (rib = rib->next; rib; rib = rib->next) {
    if (!not_in_rename(constrain_to_syms, rib->rename))
      return 0;
  }
  return 1;
}

#define EXPLAIN_R(x) /* empty */

static Scheme_Object *simplify_lex_renames(Scheme_Object *wraps, Scheme_Hash_Table *lex_cache, 
                                           Scheme_Object *stx_datum)
{
  WRAP_POS w, prev, w2;
  Scheme_Object *stack = scheme_null, *key, *old_key, *prec_ribs, *prev_prec_ribs;
  Scheme_Object *ribs_stack = scheme_null, *rib_delim = scheme_false, *constrain_to_syms = NULL;
  Scheme_Object *v, *v2, *v2l, *v2rdl, *stx, *name, *svl, *end_mutable = NULL, **v2_rib_delims = NULL, *svrdl;
  Scheme_Lexical_Rib *did_rib = NULL;
  Scheme_Hash_Table *skip_ribs_ht = NULL, *prev_skip_ribs_ht;
  int copy_on_write, no_rib_mutation = 1, rib_count = 0;
  intptr_t size, vsize, psize, i, j, pos;

  /* Although it makes no sense to simplify the rename table itself,
     we can simplify it in the context of a particular wrap suffix.
     (But don't mutate the wrap list, because that will stomp on
     tables that might be needed by a propagation.)

     A lex_cache maps wrap starts within `w' to lists of simplified
     tables. This helps avoid re-simplifying when the result is
     clearly going to be the same. A lex_cache is read and modified by
     this function, only.

     In addition to depending on the rest of the wraps, a resolved
     binding can depend on preceding wraps due to rib skipping. For
     now, simplifications that depend on preceding wraps are not
     cached (though individual computed renamings are cached to save
     space).

     The simplification stragegy mostly works inside out: since later
     renames depend on earlier renames, we simplify the earlier ones
     first, and then collapse to a flattened rename while working
     outward. This also lets us track shared tails in some common
     cases.
     
     A catch with the inside-out approach has to do with ribs (again).
     Preceding ribs determine the recur_skip_ribs set, so we can
     simply track that as we recur into the wraps initially to build
     our worklist. However, whether we process a rib at all (on the
     way out in the second pass) for a given id depends on whether any
     preceding instance of the same rib (i.e., further out) matches
     the symbol and marks. So, we have to compute that summary as we
     go in. */

  if (SCHEME_SYMBOLP(stx_datum)) {
    /* Search for prunings */
    WRAP_POS_INIT(w, wraps);
    old_key = NULL;
    prec_ribs = NULL;
    while (!WRAP_POS_END_P(w)) {
      if (SCHEME_VECTORP(WRAP_POS_FIRST(w))
          || SCHEME_RIBP(WRAP_POS_FIRST(w))) {
        /* Lexical rename --- maybe an already-simplified point  */
        key = WRAP_POS_KEY(w);
        if (!SAME_OBJ(key, old_key)) {
          v = scheme_hash_get(lex_cache, key);
          if (v && SCHEME_HASHTP(v)) {
            v = scheme_hash_get((Scheme_Hash_Table *)v, prec_ribs ? prec_ribs : scheme_false);
          } else if (prec_ribs)
            v = NULL;
        } else
          v = NULL;
        old_key = key;

        if (v) {
          /* Tables here are already simplified. */
          break;
        }

        if (SCHEME_RIBP(WRAP_POS_FIRST(w))) {
          Scheme_Lexical_Rib *rib = (Scheme_Lexical_Rib *)WRAP_POS_FIRST(w);
          if (!nonempty_rib(rib))
            prec_ribs = add_skip_set(rib->timestamp, prec_ribs);
        }
      } else if (SCHEME_PRUNEP(WRAP_POS_FIRST(w))) {
        v = SCHEME_BOX_VAL(WRAP_POS_FIRST(w));
        if (is_member(stx_datum, v)) {
          if (!constrain_to_syms)
            constrain_to_syms = v;
          else {
            v2 = scheme_null;
            while (SCHEME_PAIRP(v)) {
              if (is_member(SCHEME_CAR(v), constrain_to_syms))
                v2 = scheme_make_pair(SCHEME_CAR(v), v2);
              v = SCHEME_CDR(v);
            }
            constrain_to_syms = v2;
          }
        } else
          constrain_to_syms = scheme_null;
      }
      WRAP_POS_INC(w);
    }
  }

  WRAP_POS_INIT(w, wraps);
  WRAP_POS_INIT_END(prev);

  old_key = NULL;
  prec_ribs = NULL;

  v2l = scheme_null;
  v2rdl = NULL;

  EXPLAIN_S(fprintf(stderr, "[in simplify %s]\n", scheme_write_to_string(stx_datum, NULL)));

  EXPLAIN_R(printf("Simplifying %p %s\n", lex_cache, scheme_write_to_string(stx_datum, NULL)));

  while (!WRAP_POS_END_P(w)) {
    if (SCHEME_VECTORP(WRAP_POS_FIRST(w))
	|| SCHEME_RIBP(WRAP_POS_FIRST(w))) {
      /* Lexical rename */
      key = WRAP_POS_KEY(w);
      EXPLAIN_R(printf(" key %p\n", key));
      if (!SAME_OBJ(key, old_key)) {
        v = scheme_hash_get(lex_cache, key);
        if (v && SCHEME_HASHTP(v)) {
          v = scheme_hash_get((Scheme_Hash_Table *)v, prec_ribs ? prec_ribs : scheme_false);
        } else if (prec_ribs)
          v = NULL;
      } else
	v = NULL;
      old_key = key;
      prev_prec_ribs = prec_ribs;
      prev_skip_ribs_ht = skip_ribs_ht;

      if (v) {
	/* Tables here are already simplified. */
        v2l = v; /* build on simplify chain extracted from cache */
        end_mutable = v2l;
	/* No non-simplified table can follow a simplified one */
	break;
      } else {
	int add = 0, skip_this = 0;

	v = WRAP_POS_FIRST(w);
	if (SCHEME_RIBP(v)) {
	  /* A rib certainly isn't simplified yet. */
          Scheme_Lexical_Rib *rib = (Scheme_Lexical_Rib *)v;
          no_rib_mutation = 0;
          add = 1;
          if (!*rib->sealed) {
            scheme_signal_error("compile: unsealed local-definition context found in fully expanded form");
            return NULL;
          }
          if (SAME_OBJ(did_rib, rib)
              || !nonempty_rib(rib)
              || (constrain_to_syms && !not_in_rib(constrain_to_syms, rib))) {
            skip_this = 1;
            if (!nonempty_rib(rib))
              prec_ribs = add_skip_set(rib->timestamp, prec_ribs);
            EXPLAIN_S(fprintf(stderr, " to skip %p=%s\n", rib, 
                              scheme_write_to_string(rib->timestamp, NULL)));
          } else {
            rib_count++;
            did_rib = rib;
            prec_ribs = add_skip_set(rib->timestamp, prec_ribs);

            EXPLAIN_S(fprintf(stderr, " down rib %p=%s\n", rib, 
                              scheme_write_to_string(rib->timestamp, NULL)));
            EXPLAIN_S(print_skips(prec_ribs));
          
            copy_on_write = 1;

            EXPLAIN_R(printf(" rib %p\n", rib->timestamp));

            /* Compute, per id, whether to skip later instances of rib: */
            for (rib = rib->next; rib; rib = rib->next) {
              vsize = SCHEME_RENAME_LEN(rib->rename);
              for (i = 0; i < vsize; i++) {
                stx = SCHEME_VEC_ELS(rib->rename)[2+i];

                EXPLAIN_S(fprintf(stderr, "   skip? %s %p=%s %s\n", 
                                  scheme_write_to_string(SCHEME_STX_VAL(stx), NULL), 
                                  rib,
                                  scheme_write_to_string(rib->timestamp, NULL),
                                  scheme_write_to_string(SCHEME_VEC_ELS(rib->rename)[0], NULL)));

                /* already skipped? */
                if ((!constrain_to_syms || is_member(SCHEME_STX_VAL(stx), constrain_to_syms))
                    && (!skip_ribs_ht
                        || !scheme_hash_get(skip_ribs_ht, scheme_make_pair(SCHEME_STX_VAL(stx), rib->timestamp)))) {
                  /* No. Should we skip? */
                  Scheme_Object *other_env;
                  other_env = SCHEME_VEC_ELS(rib->rename)[2+vsize+i];
                  other_env = filter_cached_env(other_env, prec_ribs);
                  if (SCHEME_VOIDP(other_env)) {
                    int rib_dep;
                    other_env = resolve_env(stx, 0, 0, NULL, prec_ribs, NULL, &rib_dep, 0, NULL);
                    if (rib_dep) {
                      scheme_signal_error("compile: unsealed local-definition context found in fully expanded form");
                      return NULL;
                    }
                    {
                      Scheme_Object *e;
                      e = extend_cached_env(SCHEME_VEC_ELS(rib->rename)[2+vsize+i], other_env, prec_ribs, 0);
                      SCHEME_VEC_ELS(rib->rename)[2+vsize+i] = e;
                    }
                  }
                  WRAP_POS_INIT(w2, ((Scheme_Stx *)stx)->wraps);
                  if (same_marks(&w2, &w, other_env)) {
                    /* yes, skip */
                    EXPLAIN_S(fprintf(stderr, "   skip! %s\n", 
                                      scheme_write_to_string(SCHEME_STX_VAL(stx), NULL)));
                    if (!skip_ribs_ht)
                      skip_ribs_ht = scheme_make_hash_table_equal();
                    else if (copy_on_write)
                      skip_ribs_ht = scheme_clone_hash_table(skip_ribs_ht);
                    copy_on_write = 0;
                    scheme_hash_set(skip_ribs_ht, 
                                    scheme_make_pair(SCHEME_STX_VAL(stx), rib->timestamp), 
                                    scheme_true);
                  }
                } else {
                  EXPLAIN_S(fprintf(stderr, "   already skipped %s\n", 
                                    scheme_write_to_string(SCHEME_STX_VAL(stx), NULL)));
                }
              }
            }
          }
	} else {
	  /* Need to simplify this vector? */
	  if (SCHEME_VEC_SIZE(v) == 1)
	    v = SCHEME_VEC_ELS(v)[0];
	  if ((SCHEME_VEC_SIZE(v) > 2) /* a simplified vec can be empty */
	      && !SCHEME_SYMBOLP(SCHEME_VEC_ELS(v)[2])) {
	    add = 1;

            if (constrain_to_syms) {
              /* Maybe pruned so that we don't need to resolve: */
              if (not_in_rename(constrain_to_syms, v))
                skip_this = 1;
            }
          }
          EXPLAIN_R(printf(" lex reset\n"));
          did_rib = NULL;
	}

	if (add) {
          if (skip_this) {
            ribs_stack = scheme_make_pair(scheme_false, ribs_stack);
          } else {
            ribs_stack = scheme_make_pair(scheme_make_pair(prec_ribs, 
                                                           scheme_make_pair((Scheme_Object *)prev_skip_ribs_ht,
                                                                            rib_delim)),
                                          ribs_stack);
          }
          
	  /* Need to simplify, but do deepest first: */
	  if (SCHEME_NULLP(stack) || !SAME_OBJ(SCHEME_VEC_ELS(SCHEME_CAR(stack))[0], key)) {
            v = scheme_make_vector(2, NULL);
            SCHEME_VEC_ELS(v)[0] = key;
            SCHEME_VEC_ELS(v)[1] = prev_prec_ribs;
	    stack = CONS(v, stack);
	  }
	} else {
	  /* This is already simplified. Remember it and stop, because
	     no non-simplified table can follow a simplified one. */
          WRAP_POS_COPY(prev, w);
	  break;
	}
      }
    } else if (SCHEME_RIB_DELIMP(WRAP_POS_FIRST(w))) {
      rib_delim = WRAP_POS_FIRST(w);
      if (SCHEME_NULLP(SCHEME_BOX_VAL(rib_delim)))
        rib_delim = scheme_false;
      if (rib_count > 1) {
        EXPLAIN_R(if (did_rib) printf("       reset delim %d\n", rib_count));
        did_rib = NULL;
      }
      rib_count = 0;
    } else if (SCHEME_NUMBERP(WRAP_POS_FIRST(w))) {
      v = WRAP_POS_FIRST(w);
      WRAP_POS_COPY(w2, w);
      WRAP_POS_INC(w2);
      if (!WRAP_POS_END_P(w2) && SAME_OBJ(v, WRAP_POS_FIRST(w2))) {
        WRAP_POS_INC(w);
      } else {
        EXPLAIN_R(printf("         reset by mark\n"));
        did_rib = NULL;
      }
    } else {
      EXPLAIN_R(if (did_rib) printf("       reset %d\n", SCHEME_TYPE(WRAP_POS_FIRST(w))));
      did_rib = NULL;
    }

    WRAP_POS_INC(w);
  }

  EXPLAIN_R(printf(" ... phase2\n"));

  while (!SCHEME_NULLP(stack)) {
    key = SCHEME_CAR(stack);
    prev_prec_ribs = SCHEME_VEC_ELS(key)[1];
    key = SCHEME_VEC_ELS(key)[0];

    WRAP_POS_REVINIT(w, key);

    while (!WRAP_POS_REVEND_P(w)) {
      v = WRAP_POS_FIRST(w);

      if (SCHEME_RIBP(v)
	  || (SCHEME_VECTORP(v)
	      && (SCHEME_VEC_SIZE(v) > 2) /* a simplified vec can be empty */
	      && !SCHEME_SYMBOLP(SCHEME_VEC_ELS(v)[2]))) {
	/* This is the place to simplify: */
	Scheme_Lexical_Rib *rib = NULL, *init_rib = NULL;
        Scheme_Object *local_ribs;
	int ii, vvsize, done_rib_pos = 0;

        rib_delim = scheme_false;

        if (SCHEME_FALSEP(SCHEME_CAR(ribs_stack))) {
          EXPLAIN_S(fprintf(stderr, " skip rib %p=%s\n", v, 
                            scheme_write_to_string(((Scheme_Lexical_Rib *)v)->timestamp, NULL)));
          ribs_stack = SCHEME_CDR(ribs_stack);
          vsize = 0;
          local_ribs = NULL;
        } else {
          rib_delim = SCHEME_CAR(ribs_stack);
          prec_ribs = SCHEME_CAR(rib_delim);
          rib_delim = SCHEME_CDR(rib_delim);
          skip_ribs_ht = (Scheme_Hash_Table *)SCHEME_CAR(rib_delim);
          rib_delim = SCHEME_CDR(rib_delim);
          ribs_stack = SCHEME_CDR(ribs_stack);

          if (SCHEME_RIBP(v)) {
            init_rib = (Scheme_Lexical_Rib *)v;
            EXPLAIN_S(fprintf(stderr, " up rib %p=%s\n", init_rib, 
                              scheme_write_to_string(init_rib->timestamp, NULL)));
            EXPLAIN_S(print_skips(prec_ribs));
            rib = init_rib->next;
            vsize = 0;
            local_ribs = NULL;
            while (rib) {
              /* We need to process the renamings in reverse order: */
              local_ribs = scheme_make_raw_pair((Scheme_Object *)rib, local_ribs);
              
              vsize += SCHEME_RENAME_LEN(rib->rename);
              rib = rib->next;
            }
            if (local_ribs) {
              rib = (Scheme_Lexical_Rib *)SCHEME_CAR(local_ribs);
              local_ribs = SCHEME_CDR(local_ribs);
            }
          } else {
            vsize = SCHEME_RENAME_LEN(v);
            local_ribs = NULL;
          }
        }

        /* Initial size; may shrink: */
	size = vsize;

	v2 = scheme_make_vector(2 + (2 * size), NULL);
        v2_rib_delims = MALLOC_N(Scheme_Object *, size);

	pos = 0; /* counter for used slots */

	/* Local vector (different from i when we have a rib) */
	ii = 0;
	vvsize = vsize;

	for (i = 0; i < vsize; i++) {
	  if (rib) {
	    v = rib->rename;
	    vvsize = SCHEME_RENAME_LEN(v);
	    while (ii >= vvsize) {
	      ii = 0;
              done_rib_pos = pos;
              rib = (Scheme_Lexical_Rib *)SCHEME_CAR(local_ribs);
              local_ribs = SCHEME_CDR(local_ribs);
	      v = rib->rename;
	      vvsize = SCHEME_RENAME_LEN(v);
	    }
	  }
	  stx = SCHEME_VEC_ELS(v)[2+ii];
	  name = SCHEME_STX_VAL(stx);
	  SCHEME_VEC_ELS(v2)[2+pos] = name;

          if ((!constrain_to_syms || is_member(name, constrain_to_syms))
              && (!rib
                  || !skip_ribs_ht 
                  || !scheme_hash_get(skip_ribs_ht, scheme_make_pair(name, rib->timestamp)))) {
	    /* Either this name is in prev, in which case the answer
	       must match this rename's target, or this rename's
	       answer applies. */
	    Scheme_Object *ok = NULL, *ok_replace = NULL, **ok_replace_rd = NULL;
            int ok_replace_index = 0, ok_replace_rd_index = 0;
            Scheme_Object *other_env, *free_id_rename, *prev_env, *orig_prev_env;

            if (rib) {
              EXPLAIN_S(fprintf(stderr, "   resolve %s %s (%d)\n", 
                                scheme_write_to_string(name, NULL),
                                scheme_write_to_string(rib->timestamp, NULL),
                                done_rib_pos));
            }

            other_env = SCHEME_VEC_ELS(v)[2+vvsize+ii];
            if (SCHEME_PAIRP(other_env))
              free_id_rename = extract_free_id_info(SCHEME_CDR(other_env));
            else
              free_id_rename = NULL;
            other_env = filter_cached_env(other_env, prec_ribs);
            if (SCHEME_VOIDP(other_env)) {
              int rib_dep;
              other_env = resolve_env(stx, 0, 0, NULL, prec_ribs, NULL, &rib_dep, 0, NULL);
              if (rib_dep) {
                scheme_signal_error("compile: unsealed local-definition context found in fully expanded form");
                return NULL;
              }
              if (!prec_ribs) {
                if (free_id_rename)
                  ok = CONS(other_env, free_id_rename);
                else
                  ok = other_env;
                SCHEME_VEC_ELS(v)[2+vvsize+ii] = ok;
                ok = NULL;
              } else {
                ok = extend_cached_env(SCHEME_VEC_ELS(v)[2+vvsize+ii], other_env, prec_ribs, 0);
                SCHEME_VEC_ELS(v)[2+vvsize+ii] = ok;
                ok = NULL;
              }
            }

	    if (!WRAP_POS_END_P(prev)
                || SCHEME_PAIRP(v2l)) {
	      WRAP_POS w3;
	      Scheme_Object *vp, **vrdp;

	      /* Check marks (now that we have the correct barriers). */
	      WRAP_POS_INIT(w2, ((Scheme_Stx *)stx)->wraps);
	      if (!same_marks(&w2, &w, other_env)) {
		other_env = NULL;
	      }

              if (other_env) {
                /* A simplified table needs to have the final answer, so
                   fold conversions from the rest of the wraps. In the case
                   of ribs, the "rest" can include earlier rib renamings.
                   Otherwise, check simplications accumulated in v2l (possibly from a
                   previously simplified tail in the same cache). Finally, 
                   try prev (from a previously simplified tail in an earlier
                   round of simplifying). */
                int rib_found = 0;
                if (done_rib_pos) {
                  for (j = 0; j < done_rib_pos; j++) {
                    if (SAME_OBJ(SCHEME_VEC_ELS(v2)[2+j], name)) {
                      rib_found = 1;
                      prev_env = SCHEME_VEC_ELS(v2)[2+size+j];
                      orig_prev_env = prev_env;
                      if (SCHEME_PAIRP(prev_env)) prev_env = SCHEME_CAR(prev_env);
                      if (SAME_OBJ(prev_env, other_env)) {
                        if (SCHEME_FALSEP(rib_delim) 
                            || SAME_OBJ(v2_rib_delims[j], rib_delim) 
                            || !is_in_rib_delim(prev_env, rib_delim)) {
                          ok = SCHEME_VEC_ELS(v)[0];
                          ok_replace = v2;
                          ok_replace_index = 2 + size + j;
                          ok_replace_rd = v2_rib_delims;
                          if (!free_id_rename && SCHEME_PAIRP(orig_prev_env))
                            free_id_rename = SCHEME_CDR(orig_prev_env);
                        }
                      } else {
                        EXPLAIN_S(fprintf(stderr, "    not matching prev rib\n"));
                        ok = NULL;
                      }
                      break;
                    }
                  }
                }
                if (!rib_found) {
                  int passed_mutable = 0;
                  WRAP_POS_COPY(w3, prev);
                  svl = v2l;
                  svrdl = v2rdl;
                  for (; SCHEME_PAIRP(svl) || !WRAP_POS_END_P(w3); ) {
                    if (SAME_OBJ(svl, end_mutable)) passed_mutable = 1;
                    if (SCHEME_PAIRP(svl)) {
                      vp = SCHEME_CAR(svl);
                      if (svrdl)
                        vrdp = (Scheme_Object **)SCHEME_CAR(svrdl);
                      else
                        vrdp = NULL;
                    } else {
                      vp = WRAP_POS_FIRST(w3);
                      vrdp = NULL;
                    }
                    if (SCHEME_VECTORP(vp)) {
                      psize = SCHEME_RENAME_LEN(vp);
                      for (j = 0; j < psize; j++) {
                        if (SAME_OBJ(SCHEME_VEC_ELS(vp)[2+j], name)) {
                          prev_env = SCHEME_VEC_ELS(vp)[2+psize+j];
                          orig_prev_env = prev_env;
                          if (SCHEME_PAIRP(prev_env)) prev_env = SCHEME_CAR(prev_env);
                          if (SAME_OBJ(prev_env, other_env)
                              && (SCHEME_FALSEP(rib_delim) 
                                  || (vrdp && (SAME_OBJ(vrdp[j], rib_delim)))
                                  || !is_in_rib_delim(prev_env, rib_delim))) {
                            ok = SCHEME_VEC_ELS(v)[0];
                            if (!free_id_rename && SCHEME_PAIRP(orig_prev_env))
                              free_id_rename = SCHEME_CDR(orig_prev_env);
                          } else {
                            EXPLAIN_S(fprintf(stderr,
                                              "    not matching deeper %s\n",
                                              scheme_write_to_string(other_env, NULL)));
                            ok = NULL; 
                            /* Alternate time/space tradeoff: could be
                                 SCHEME_VEC_ELS(vp)[2+psize+j],
                                 which is the value from prev */
                          }
                          if (ok && SCHEME_PAIRP(svl) && !passed_mutable
                              && (SCHEME_FALSEP(rib_delim) || vrdp)) {
                            /* Can overwrite old map, instead
                               of adding a new one. */
                            ok_replace = vp;
                            ok_replace_index = 2 + psize + j;
                            ok_replace_rd = vrdp;
                            ok_replace_rd_index = j;
                          }
                          break;
                        }
                      }
                      if (j < psize)
                        break;
                    }
                    if (SCHEME_PAIRP(svl)) {
                      svl = SCHEME_CDR(svl);
                      if (svrdl) svrdl = SCHEME_CDR(svrdl);
                    } else {
                      WRAP_POS_INC(w3);
                    }
                  }
                  if (WRAP_POS_END_P(w3) && SCHEME_NULLP(svl) && SCHEME_FALSEP(other_env))
                    ok = SCHEME_VEC_ELS(v)[0];
                }
              } else
                ok = NULL;
	    } else {
              if (!SCHEME_FALSEP(other_env)) {
                EXPLAIN_S(fprintf(stderr, "    not based on #f\n"));
                ok = NULL;
              } else {
                WRAP_POS_INIT(w2, ((Scheme_Stx *)stx)->wraps);
                if (same_marks(&w2, &w, scheme_false))
                  ok = SCHEME_VEC_ELS(v)[0];
                else {
                  EXPLAIN_S(fprintf(stderr, "    not matching marks\n"));
                  ok = NULL;
                }
              }
	    }

	    if (ok) {
              if (free_id_rename)
                ok = CONS(ok, free_id_rename);
              if (ok_replace) {
                EXPLAIN_S(fprintf(stderr, "   replace mapping %s\n", 
                                  scheme_write_to_string(ok, NULL)));
                SCHEME_VEC_ELS(ok_replace)[ok_replace_index] = ok;
                ok_replace_rd[ok_replace_rd_index] = rib_delim;
              } else {
                EXPLAIN_S(fprintf(stderr, "   add mapping %s\n", 
                                  scheme_write_to_string(ok, NULL)));
                SCHEME_VEC_ELS(v2)[2+size+pos] = ok;
                v2_rib_delims[pos] = rib_delim;
                pos++;
              }
	    } else {
              EXPLAIN_S(fprintf(stderr, "   no mapping %s\n", 
                                scheme_write_to_string(name, NULL)));
            }
	  } else {
            EXPLAIN_S(fprintf(stderr, "   skip %s %s %p\n", 
                              scheme_write_to_string(name, NULL),
                              scheme_write_to_string(rib->timestamp, NULL),
                              rib));
          }
	  ii++;
	}

        if (!pos)
          v2 = empty_simplified;
        else {
          if (pos != size) {
            /* Shrink simplified vector */
            v = v2;
            v2 = scheme_make_vector(2 + (2 * pos), NULL);
            for (i = 0; i < pos; i++) {
              SCHEME_VEC_ELS(v2)[2+i] = SCHEME_VEC_ELS(v)[2+i];
              SCHEME_VEC_ELS(v2)[2+pos+i] = SCHEME_VEC_ELS(v)[2+size+i];
            }
          }

          SCHEME_VEC_ELS(v2)[0] = scheme_false;
          for (i = 0; i < pos; i++) {
            if (!SCHEME_SYMBOLP(SCHEME_VEC_ELS(v2)[2+pos+i]))
              SCHEME_VEC_ELS(v2)[0] = scheme_true;
          }

          SCHEME_VEC_ELS(v2)[1] = scheme_false;
          maybe_install_rename_hash_table(v2);

          if (no_rib_mutation) {
            /* Sometimes we generate the same simplified lex table, so
               look for an equivalent one in the cache. */
            v = scheme_hash_get(lex_cache, scheme_true);
            if (!v) {
              v = (Scheme_Object *)scheme_make_hash_table_equal();
              scheme_hash_set(lex_cache, scheme_true, v);
            }
            svl = scheme_hash_get((Scheme_Hash_Table *)v, v2);
            if (svl)
              v2 = svl;
            else
              scheme_hash_set((Scheme_Hash_Table *)v, v2, v2);
          }
        }

        EXPLAIN_S({
            int k;
            for (k = 2; k < SCHEME_VEC_SIZE(v2); k++) {
              fprintf(stderr, " %p[%d]: %s\n", v2, k, scheme_write_to_string(SCHEME_VEC_ELS(v2)[k], NULL));
            }
          });

	v2l = CONS(v2, v2l);
	v2rdl = scheme_make_raw_pair((Scheme_Object *)v2_rib_delims, v2rdl);
      }

      WRAP_POS_DEC(w);
    }

    if (!constrain_to_syms) {
      v = scheme_hash_get(lex_cache, key);
      if (!v && !prev_prec_ribs) {
        /* no dependency on ribs, so we can simply cache this result: */
        scheme_hash_set(lex_cache, key, v2l);
      } else {
        Scheme_Hash_Table *ht;
        if (v && SCHEME_HASHTP(v))
          ht = (Scheme_Hash_Table *)v;
        else {
          ht = scheme_make_hash_table(SCHEME_hash_ptr);
        }
        if (v && !SCHEME_HASHTP(v))
          scheme_hash_set(ht, scheme_false, v);
        scheme_hash_set(ht, prev_prec_ribs ? prev_prec_ribs : scheme_false, v2l);
        scheme_hash_set(lex_cache, key, (Scheme_Object *)ht);
      }
      end_mutable = v2l;
    }

    stack = SCHEME_CDR(stack);
  }

  EXPLAIN_R(printf(" ... done\n"));

  return v2l;
}

static Scheme_Object *add_rename_to_stack(Module_Renames* mrn, Scheme_Object *stack,
                                          Scheme_Marshal_Tables *mt,
                                          Scheme_Object *a)
{
  Scheme_Object *local_key;
	  
  local_key = scheme_marshal_lookup(mt, (Scheme_Object *)mrn);
  if (!local_key) {
    /* Convert hash table to vector, etc.: */
    int i, j, count = 0;
    Scheme_Hash_Table *ht;
    Scheme_Object *l, *fil;
	    
    ht = mrn->ht;
    count = ht->count;
    l = scheme_make_vector(count * 2, NULL);                  
    for (i = ht->size, j = 0; i--; ) {
      if (ht->vals[i]) {
        SCHEME_VEC_ELS(l)[j++] = ht->keys[i];
        fil = ht->vals[i];
        SCHEME_VEC_ELS(l)[j++] = fil;
      }
    }

    ht = mrn->free_id_renames;
    if (ht && ht->count) {
      count = ht->count;
      fil = scheme_make_vector(count * 2, NULL);                  
      for (i = ht->size, j = 0; i--; ) {
        if (ht->vals[i]) {
          SCHEME_VEC_ELS(fil)[j++] = ht->keys[i];
          SCHEME_VEC_ELS(fil)[j++] = ht->vals[i];
        }
      }
    } else
      fil = NULL;

    if (mrn->marked_names && mrn->marked_names->count) {
      Scheme_Object *d = scheme_null, *p;

      for (i = mrn->marked_names->size; i--; ) {
        if (mrn->marked_names->vals[i]
            /* #f mapping used to store reverse-map cache: */
            && !SCHEME_FALSEP(mrn->marked_names->keys[i])) {
          p = CONS(mrn->marked_names->keys[i],
                   mrn->marked_names->vals[i]);
          d = CONS(p, d);
        }
      }

      if (fil)
        fil = CONS(fil, d);
      else
        fil = d;
    } else if (fil)
      fil = CONS(fil, scheme_null);
    else
      fil = scheme_null;
                    
    l = CONS(l, fil);
                  
    if (SCHEME_PAIRP(mrn->unmarshal_info))
      l = CONS(mrn->unmarshal_info, l); 
	      
    l = CONS(mrn->set_identity, l);
    l = CONS((mrn->kind == mzMOD_RENAME_MARKED) ? scheme_true : scheme_false, l);
    l = CONS(mrn->phase, l);
	    
    local_key = scheme_marshal_lookup(mt, a);
    if (local_key)
      scheme_marshal_using_key(mt, a);
    else {
      local_key = scheme_marshal_wrap_set(mt, a, l);
    }
  } else {
    scheme_marshal_using_key(mt, (Scheme_Object *)mrn);
  }
  return CONS(local_key, stack);
}

static Scheme_Object *wraps_to_datum(Scheme_Object *stx_datum,
                                     Scheme_Object *w_in, 
				     Scheme_Marshal_Tables *mt,
                                     Scheme_Hash_Table *rns,
				     int just_simplify)
{
  Scheme_Object *stack, *a, *old_key, *simplifies = scheme_null, *prec_ribs = scheme_null;
  WRAP_POS w;
  Scheme_Hash_Table *lex_cache, *reverse_map;
  int stack_size = 0, specific_to_datum = 0;

  if (!rns)
    rns = mt->rns;

  if (just_simplify) {
    a = scheme_hash_get(rns, w_in);
  } else {
    if (mt->same_map) {
      a = scheme_hash_get(mt->same_map, w_in);
      if (a)
        w_in = a;
    }
    a = scheme_marshal_lookup(mt, w_in);
  }
  if (a) {
    if (just_simplify)
      return a;
    else {
      scheme_marshal_using_key(mt, w_in);
      return a;
    }
  }

  WRAP_POS_INIT(w, w_in);

  stack = scheme_null;

  lex_cache = (Scheme_Hash_Table *)scheme_hash_get(rns, scheme_void);
  if (!lex_cache) {
    lex_cache = scheme_make_hash_table(SCHEME_hash_ptr);
    scheme_hash_set(rns, scheme_void, (Scheme_Object *)lex_cache);
  }

  if (!just_simplify)
    stx_datum = scheme_false;

  /* Ensures that all lexical tables in w have been simplified */
  simplifies = simplify_lex_renames(w_in, lex_cache, stx_datum);

  if (mt)
    scheme_marshal_push_refs(mt);

  while (!WRAP_POS_END_P(w)) {
    a = WRAP_POS_FIRST(w);
    old_key = WRAP_POS_KEY(w);
    WRAP_POS_INC(w);
    if (SCHEME_NUMBERP(a)) {
      /* Mark numbers get parenthesized */
      if (!WRAP_POS_END_P(w) && SAME_OBJ(a, WRAP_POS_FIRST(w)))
	WRAP_POS_INC(w); /* delete cancelled mark */
      else {
	if (just_simplify)
	  stack = CONS(a, stack);
	else
	  stack = CONS(CONS(a, scheme_null), stack);
	stack_size++;
      }
    } else if (SCHEME_VECTORP(a)
	       || SCHEME_RIBP(a)) {
      if (SCHEME_RIBP(a) || (SCHEME_VEC_SIZE(a) > 2)) {

	if (SCHEME_RIBP(a) || !SCHEME_SYMBOLP(SCHEME_VEC_ELS(a)[2])) {
	  /* a is not a simplified table; need to look it up; if
	     simplifies is non-null, then we already have found a list
	     of simplified tables for the current wrap segment. */
          if (SCHEME_RIBP(a)) {
            if (nonempty_rib((Scheme_Lexical_Rib *)a))
              prec_ribs = scheme_make_pair(((Scheme_Lexical_Rib *)a)->timestamp, prec_ribs);
          }
	  a = SCHEME_CAR(simplifies);
	  /* used up one simplification: */
	  simplifies = SCHEME_CDR(simplifies);
	}
	  
	/* Simplification may have left us with the null table: */
	if (SCHEME_VEC_SIZE(a) > 2) {
	  if (just_simplify) {
	    stack = CONS(a, stack);
	  } else {
	    Scheme_Object *local_key;
	    
	    local_key = scheme_marshal_lookup(mt, a);
	    if (local_key) {
              scheme_marshal_using_key(mt, a);
              a = local_key;
            } else {
              a = scheme_marshal_wrap_set(mt, a, a);
            }
            stack = CONS(a, stack);
	  }
	  stack_size++;
	}
      }
      /* else empty simplified vector, which we drop */
    } else if (SCHEME_RIB_DELIMP(a)) {
      /* simpliciation eliminates the need for rib delimiters */
    } else if (SCHEME_RENAMESP(a)
               || SCHEME_RENAMES_SETP(a)) {
      int which = 0, all_redundant = 1;

      while (1) {
        Module_Renames *mrn;
        int redundant = 0;
      
        if (SCHEME_RENAMESP(a)) {
          if (!which) {
            mrn = (Module_Renames *)a;
            which++;
          } else
            break;
        } else {
          /* flatten sets */
          Module_Renames_Set *s = (Module_Renames_Set *)a;
          mrn = NULL;
          while (!mrn 
                 && (which - 2 < (s->other_phases
                                  ? s->other_phases->size
                                  : 0))) {
            if (!which)
              mrn = s->rt;
            else if (which == 1)
              mrn = s->et;
            else
              mrn = (Module_Renames *)s->other_phases->vals[which - 2];
            which++;
          }
          if (!mrn
              && (which - 2 >= (s->other_phases
                                ? s->other_phases->size
                                : 0)))
            break;
        }
      
        if (mrn) {
          if (mrn->kind == mzMOD_RENAME_MARKED) {
            /* Not useful if there's no marked names. */
            redundant = ((mrn->sealed >= STX_SEAL_ALL)
                         && (!mrn->marked_names || !mrn->marked_names->count)
                         && (!mrn->free_id_renames || !mrn->free_id_renames->count)
                         && SCHEME_NULLP(mrn->shared_pes));
            if (!redundant) {
              /* Otherwise, watch out for multiple instances of the same rename: */
              WRAP_POS l;
              Scheme_Object *la;
	
              WRAP_POS_COPY(l,w);
	  
              for (; !WRAP_POS_END_P(l); WRAP_POS_INC(l)) {
                la = WRAP_POS_FIRST(l);
                if (SAME_OBJ(a, la)) {
                  redundant = 1;
                  break;
                }
              }
            }
          } else {
            /* Check for later [non]module rename at the same phase: */
            Scheme_Object *phase;
            WRAP_POS l;
            Scheme_Object *la;
	
            WRAP_POS_COPY(l,w);

            phase = mrn->phase;

            for (; !WRAP_POS_END_P(l); WRAP_POS_INC(l)) {
              la = WRAP_POS_FIRST(l);
              if (SCHEME_RENAMESP(la)) {
                Module_Renames *lrn = (Module_Renames *)WRAP_POS_FIRST(l);
                if ((lrn->kind == mrn->kind)
                    && (same_phase(lrn->phase, phase))) {
                  /* mrn is redundant */
                  redundant = 1;
                  break;
                }
              } else if (SCHEME_RENAMES_SETP(la)) {
                Module_Renames_Set *s = (Module_Renames_Set *)WRAP_POS_FIRST(l);
                if ((s->kind == mrn->kind)
                    && extract_renames(s, phase)) {
                  redundant = 1;
                  break;
                }
              } else if (SCHEME_BOXP(la))
                phase = reverse_phase_shift(phase, SCHEME_VEC_ELS(SCHEME_BOX_VAL(la))[0]);
            }
          }

          if (!redundant) {
            all_redundant = 0;
            if (just_simplify) {
              stack = CONS((Scheme_Object *)mrn, stack);
            } else {
              if (mrn->free_id_renames) {
                /* resolve all renamings */
                int i;
                Scheme_Object *b;
                for (i = mrn->free_id_renames->size; i--; ) {
                  if (mrn->free_id_renames->vals[i]) {
                    if (SCHEME_STXP(mrn->free_id_renames->vals[i])) {
                      int sealed;
                      Scheme_Hash_Table *free_id_recur;
                      
                      free_id_recur = make_recur_table();
                      b = extract_module_free_id_binding((Scheme_Object *)mrn,
                                                         mrn->free_id_renames->keys[i],
                                                         mrn->free_id_renames->vals[i],
                                                         &sealed,
                                                         free_id_recur);
                      release_recur_table(free_id_recur);
                      if (!sealed) {
                        free_id_recur = make_recur_table();
                        extract_module_free_id_binding((Scheme_Object *)mrn,
                                                       mrn->free_id_renames->keys[i],
                                                       mrn->free_id_renames->vals[i],
                                                       &sealed,
                                                       free_id_recur);
                        scheme_signal_error("write: unsealed local-definition or module context"
                                            " found in syntax object");
                      }
                      scheme_hash_set(mrn->free_id_renames, mrn->free_id_renames->keys[i], b);
                    }
                  }
                }
              }
            
              if (mrn->kind == mzMOD_RENAME_TOPLEVEL) {
                if (same_phase(mrn->phase, scheme_make_integer(0)))
                  stack = CONS(scheme_true, stack);
                else
                  stack = CONS(scheme_false, stack);
              } else {
                stack = add_rename_to_stack(mrn, stack, mt, a);
              }
            }
            stack_size++;
          }
        }
      }

      if (all_redundant) {
        /* The rename isn't actually redundant if we need to keep the
           rename-set identity --- but we can simplify to just the
           identity. */
        WRAP_POS l;
        Scheme_Object *la, *this_set_identity, *set_identity;
        int kind;

        if (SCHEME_RENAMESP(a)) {
          this_set_identity = ((Module_Renames *)a)->set_identity;
          kind = ((Module_Renames *)a)->kind;
        } else {
          this_set_identity = ((Module_Renames_Set *)a)->set_identity;
          kind = ((Module_Renames_Set *)a)->kind;
        }
        
        if (kind != mzMOD_RENAME_TOPLEVEL) {
          WRAP_POS_COPY(l,w);
        
          for (; !WRAP_POS_END_P(l); WRAP_POS_INC(l)) {
            la = WRAP_POS_FIRST(l);
            if (SCHEME_RENAMESP(la))
              set_identity = ((Module_Renames *)la)->set_identity;
            else if (SCHEME_RENAMES_SETP(la))
              set_identity = ((Module_Renames_Set *)la)->set_identity;
            else if (SCHEME_BOXP(la)) {
              set_identity = SCHEME_VEC_ELS(SCHEME_BOX_VAL(la))[5];
              if (SAME_OBJ(set_identity, this_set_identity))
                set_identity = scheme_false;
              else
                set_identity = NULL;
            } else
              set_identity = NULL;

            if (set_identity) {
              if (SAME_OBJ(set_identity, this_set_identity)) {
                all_redundant = 0;
                break;
              } else
                break;
            }
          }

          if (all_redundant) {
            Scheme_Hash_Table *identity_map;
            Scheme_Object *key;

            identity_map = (Scheme_Hash_Table *)scheme_hash_get(rns, scheme_eof);
            if (!identity_map) {
              identity_map = scheme_make_hash_table_equal();
              scheme_hash_set(rns, scheme_eof, (Scheme_Object *)identity_map);
            }

            key = scheme_make_pair(scheme_make_integer(kind), this_set_identity);
            
            la = scheme_hash_get(identity_map, key);
            if (!la) {
              la = scheme_make_module_rename(scheme_make_integer(0), kind, NULL, NULL, this_set_identity);
              ((Module_Renames *)la)->sealed = STX_SEAL_ALL;
              scheme_hash_set(identity_map, key, la);
            }

            if (just_simplify) 
              stack = CONS(la, stack);
            else
              stack = add_rename_to_stack((Module_Renames *)la, stack, mt, a);
            stack_size++;
          }
        }
      }
    } else if (SCHEME_SYMBOLP(a)) {
      /* mark barrier */
      stack = CONS(a, stack);
      stack_size++;
    } else if (SCHEME_HASHTP(a)) {
      /* chain-specific cache; drop it */
    } else if (SCHEME_PRUNEP(a)) {
      if (SCHEME_SYMBOLP(stx_datum)) {
        /* Assuming that there are lex renames later, then this chain is
           specific to this wrap. */
        specific_to_datum = 1;
      }
      if (!just_simplify)
        a = scheme_box(SCHEME_BOX_VAL(a));
      stack = CONS(a, stack);
      stack_size++;
    } else {
      /* box, a phase shift */
      /* We used to drop a phase shift if there are no following
         rename tables. However, the phase shift also identifies
         the source module, which can be relevant. So, keep the
         phase shift. */
      /* Need the phase shift, but drop the export table, if any: */
      Scheme_Object *local_key;
      Scheme_Object *aa;
      
      aa = SCHEME_BOX_VAL(a);
      if (SCHEME_TRUEP(SCHEME_VEC_ELS(aa)[3])
          || (!just_simplify && SCHEME_TRUEP(SCHEME_VEC_ELS(aa)[4]))) {
        if (mt)
          a = scheme_hash_get(mt->shift_map, aa);
        else
          a = scheme_hash_get(rns, aa);
        if (!a) {
          a = scheme_make_vector(6, NULL);
          SCHEME_VEC_ELS(a)[0] = SCHEME_VEC_ELS(aa)[0];
          SCHEME_VEC_ELS(a)[1] = SCHEME_VEC_ELS(aa)[1];
          SCHEME_VEC_ELS(a)[2] = SCHEME_VEC_ELS(aa)[2];
          SCHEME_VEC_ELS(a)[3] = scheme_false;
          if (just_simplify)
            SCHEME_VEC_ELS(a)[4] = SCHEME_VEC_ELS(aa)[4];
          else
            SCHEME_VEC_ELS(a)[4] = scheme_false;
          SCHEME_VEC_ELS(a)[5] = SCHEME_VEC_ELS(aa)[5];
          a = scheme_box(a);
          scheme_hash_set(rns, aa, a);
        }
      }
      
      if (!just_simplify) {
        local_key = scheme_marshal_lookup(mt, a);
        if (local_key) {
          scheme_marshal_using_key(mt, a);
          a = local_key;
        } else {
          a = scheme_marshal_wrap_set(mt, a, a);
        }
      }
      
      stack = CONS(a, stack);
      stack_size++;
    }
  }

  /* Double-check for equivalent list in table (after simplification): */
  if (mt && mt->pass) {
    /* No need to check for later passes, since mt->same_map
       covers the equivalence. */
  } else {
    if (mt) {
      reverse_map = mt->reverse_map;
    } else {
      reverse_map = (Scheme_Hash_Table *)scheme_hash_get(rns, scheme_undefined);
    }
    if (!reverse_map) {
      reverse_map = scheme_make_hash_table_equal();
      if (mt)
        mt->reverse_map = reverse_map;
      else
        scheme_hash_set(rns, scheme_undefined, (Scheme_Object *)reverse_map);
    }
    old_key = scheme_hash_get(reverse_map, stack);
    if (old_key) {
      if (just_simplify) {
        return scheme_hash_get(rns, old_key);
      } else {
        a = scheme_marshal_lookup(mt, old_key);
        if (!mt->same_map) {
          Scheme_Hash_Table *same_map;
          same_map = scheme_make_hash_table(SCHEME_hash_ptr);
          mt->same_map = same_map;
        }
        scheme_hash_set(mt->same_map, w_in, old_key);
        /* nevermind references that we saw when creating `stack': */
        scheme_marshal_pop_refs(mt, 0);
        scheme_marshal_using_key(mt, old_key);
        return a;
      }
    }

    if (!specific_to_datum)
      scheme_hash_set(reverse_map, stack, w_in);
  }

  /* Convert to a chunk if just simplifying.
     (Note that we do this after looking for equivalent stacks.) */
  if (just_simplify) {
    if (stack_size) {
      Wrap_Chunk *wc;
      int i;
      wc = MALLOC_WRAP_CHUNK(stack_size);
      wc->type = scheme_wrap_chunk_type;
      wc->len = stack_size;
      for (i = stack_size; i--; stack = SCHEME_CDR(stack)) {
        wc->a[i] = SCHEME_CAR(stack);
      }
      stack = CONS((Scheme_Object *)wc, scheme_null);
    } else
      stack= scheme_null;
  }
  
  if (mt) {
    /* preserve references that we saw when creating `stack': */
    scheme_marshal_pop_refs(mt, 1);
  }

  /* Remember this wrap set: */
  if (just_simplify) {
    if (!specific_to_datum)
      scheme_hash_set(rns, w_in, stack);
    return stack;
  } else {
    return scheme_marshal_wrap_set(mt, w_in, stack);
  }
}

/*========================================================================*/
/*                           syntax->datum                                */
/*========================================================================*/

/* This code can convert a syntax object plus its wraps to something
   writeable. In that case, the result is a <converted>:

      <converted> = <simple converted pair> | ...

      <simple converted pair> = (cons (cons <int> (cons <converted> ... <converted>)) <wrap>)
                              | (cons (cons <converted> ... null) <wrap>)
                              | (cons (cons #t <s-exp>) <wrap>)
                                 ; where <s-exp> has no boxes or vectors, and
                                 ;  <wrap> is shared in all <s-exp> elements
      <simple converted box> = (cons (box <converted>) <wrap>)
      <simple converted vector> = (cons (vector <converted> ...) <wrap>)
      <simple converted other> = (cons <s-exp> <wrap>)
                                 ; where <s-exp> is not a pair, vector, or box
*/

static Scheme_Object *extract_for_common_wrap(Scheme_Object *a, int get_mark, int pair_ok)
{
  /* We only share wraps for things constucted with pairs and
     atomic (w.r.t. syntax) values. */
  Scheme_Object *v;

  if (SCHEME_PAIRP(a)) {
    v = SCHEME_CAR(a);

    if (SCHEME_PAIRP(v)) {
      if (pair_ok && SAME_OBJ(SCHEME_CAR(v), scheme_true)) {
        /* A pair with shared wraps for its elements */
        if (get_mark)
          return SCHEME_CDR(a);
        else
          return SCHEME_CDR(v);
      }
    } else if (!SCHEME_NULLP(v) && !SCHEME_BOXP(v) && !SCHEME_VECTORP(v) && !SCHEME_HASHTRP(v) && !prefab_p(v)) {
      /* It's atomic. */
      if (get_mark)
        return SCHEME_CDR(a);
      else
        return v;
    }
  }

  return NULL;
}

static void lift_common_wraps(Scheme_Object *l, Scheme_Object *common_wraps, int cnt, int tail)
{
  Scheme_Object *a;

  while (cnt--) {
    a = SCHEME_CAR(l);
    a = extract_for_common_wrap(a, 0, 1);
    SCHEME_CAR(l) = a;
    if (cnt)
      l = SCHEME_CDR(l);
  }
  if (tail) {
    a = SCHEME_CDR(l);
    a = extract_for_common_wrap(a, 0, 0);
    SCHEME_CDR(l) = a;
  }
}

#ifdef DO_STACK_CHECK
static Scheme_Object *syntax_to_datum_inner(Scheme_Object *o, 
					    int with_marks,
					    Scheme_Marshal_Tables *mt);

static Scheme_Object *syntax_to_datum_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Marshal_Tables *mt = (Scheme_Marshal_Tables *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p3 = NULL;

  return syntax_to_datum_inner(o, p->ku.k.i1, mt);
}
#endif

static Scheme_Object *syntax_to_datum_inner(Scheme_Object *o, 
					    int with_marks, /* abs > 1 => marshal; negative => implicitly tainted */
					    Scheme_Marshal_Tables *mt)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *v, *result, *converted_wraps = NULL;
  int add_taint = 0;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)o;
      p->ku.k.i1 = with_marks;
      p->ku.k.p3 = (void *)mt;
      return scheme_handle_stack_overflow(syntax_to_datum_k);
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  if (with_marks) {
    /* Propagate wraps: */
    scheme_stx_content((Scheme_Object *)stx);
    if (with_marks > 0) {
      if (is_tainted((Scheme_Object *)stx)) {
        add_taint = 1;
        with_marks = -with_marks;
      } else if (is_armed((Scheme_Object *)stx)) {
        add_taint = 2;
      }
    }
  }

  v = stx->val;
  
  if (SCHEME_PAIRP(v)) {
    Scheme_Object *first = NULL, *last = NULL, *p, *common_wraps = NULL;
    int cnt = 0;
    
    while (SCHEME_PAIRP(v)) {
      Scheme_Object *a;

      cnt++;

      a = syntax_to_datum_inner(SCHEME_CAR(v), with_marks, mt);

      p = CONS(a, scheme_null);
      
      if (last)
	SCHEME_CDR(last) = p;
      else
	first = p;
      last = p;
      v = SCHEME_CDR(v);

      if (with_marks) {
        a = extract_for_common_wrap(a, 1, 1);
        if (!common_wraps) {
          if (a)
            common_wraps = a;
          else
            common_wraps = scheme_false;
        } else if (!a || !SAME_OBJ(common_wraps, a))
          common_wraps = scheme_false;
      }
    }
    if (!SCHEME_NULLP(v)) {
      v = syntax_to_datum_inner(v, with_marks, mt);
      SCHEME_CDR(last) = v;

      if (with_marks) {
        v = extract_for_common_wrap(v, 1, 0);
        if (v && SAME_OBJ(common_wraps, v)) {
          converted_wraps = wraps_to_datum(scheme_false, stx->wraps, mt, NULL, 0);
          if (SAME_OBJ(common_wraps, converted_wraps))
            lift_common_wraps(first, common_wraps, cnt, 1);
          else
            common_wraps = scheme_false;
        } else
          common_wraps = scheme_false;
      }

      if (((with_marks > 1) || (with_marks < -1)) && SCHEME_FALSEP(common_wraps)) {
	/* v is likely a pair, and v's car might be a pair,
	   which means that the datum->syntax part
	   won't be able to detect that v is a "non-pair"
	   terminal. Therefore, we communicate the
	   length before the terminal to datum->syntax: */
	first = scheme_make_pair(scheme_make_integer(cnt), first);
      }
    } else if (with_marks && SCHEME_TRUEP(common_wraps)) {
      converted_wraps = wraps_to_datum(scheme_false, stx->wraps, mt, NULL, 0);
      if (SAME_OBJ(common_wraps, converted_wraps))
        lift_common_wraps(first, common_wraps, cnt, 0);
      else
        common_wraps = scheme_false;
    }

    if (with_marks && SCHEME_TRUEP(common_wraps)) {
      first = scheme_make_pair(scheme_true, first);
    }

    result = first;
  } else if (SCHEME_BOXP(v)) {
    v = syntax_to_datum_inner(SCHEME_BOX_VAL(v), with_marks, mt);
    result = scheme_box(v);
    SCHEME_SET_IMMUTABLE(result);
  } else if (SCHEME_VECTORP(v)) {
    int size = SCHEME_VEC_SIZE(v), i;
    Scheme_Object *r, *a;
    
    r = scheme_make_vector(size, NULL);
    
    for (i = 0; i < size; i++) {
      a = syntax_to_datum_inner(SCHEME_VEC_ELS(v)[i], with_marks, mt);
      SCHEME_VEC_ELS(r)[i] = a;
    }

    result = r;
    SCHEME_SET_IMMUTABLE(result);
  } else if (SCHEME_HASHTRP(v)) {
    Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)v, *ht2;
    Scheme_Object *key, *val;
    mzlonglong i;
    
    ht2 = scheme_make_hash_tree(SCHEME_HASHTR_FLAGS(ht) & 0x3);
    
    i = scheme_hash_tree_next(ht, -1);
    while (i != -1) {
      scheme_hash_tree_index(ht, i, &key, &val);
      val = syntax_to_datum_inner(val, with_marks, mt);
      ht2 = scheme_hash_tree_set(ht2, key, val);
      i = scheme_hash_tree_next(ht, i);
    }
    
    result = (Scheme_Object *)ht2;
  } else if (prefab_p(v)) {
    Scheme_Structure *s = (Scheme_Structure *)v;
    Scheme_Object *a;
    int size = s->stype->num_slots, i;
    
    s = (Scheme_Structure *)scheme_clone_prefab_struct_instance(s);
    for (i = 0; i < size; i++) {
      a = syntax_to_datum_inner(s->slots[i], with_marks, mt);
      s->slots[i] = a;
    }

    result = (Scheme_Object *)s;
  } else
    result = v;

  if ((with_marks > 1) || (with_marks < -1)) {
    if (!converted_wraps)
      converted_wraps = wraps_to_datum(stx->val, stx->wraps, mt, NULL, 0);
    result = CONS(result, converted_wraps);
    if (add_taint == 1)
      result = scheme_make_vector(1, result); /* vector of size 1 => tainted */
    else if (add_taint == 2) {
      result = scheme_make_vector(2, result); /* vector of size 2 => armed */
      SCHEME_VEC_ELS(result)[1] = scheme_false;
    }
  }

  return result;
}

Scheme_Object *scheme_syntax_to_datum(Scheme_Object *stx, int with_marks,
				      Scheme_Marshal_Tables *mt)
{
  Scheme_Object *v;

  if (mt)
    scheme_marshal_push_refs(mt);

  v = syntax_to_datum_inner(stx, with_marks, mt);

  if (mt) {
    /* A symbol+wrap combination is likely to be used multiple
       times. This is a relatively minor optimization in .zo size,
       since v is already fairly compact, but it also avoids
       allocating extra syntax objects at load time. For consistency,
       we try to reuse all combinations. */
    Scheme_Hash_Table *top_map;
    Scheme_Object *key;
    
    top_map = mt->top_map;
    if (!top_map) {
      top_map = scheme_make_hash_table_equal();
      mt->top_map = top_map;
    }
    
    key = scheme_hash_get(top_map, v);
    if (key) {
      scheme_marshal_pop_refs(mt, 0);
      v = scheme_marshal_lookup(mt, key);
      scheme_marshal_using_key(mt, key);
    } else {
      scheme_hash_set(top_map, stx, v);
      v = scheme_marshal_wrap_set(mt, stx, v);
      scheme_marshal_pop_refs(mt, 1);
    }
  }

  return v;
}

/*========================================================================*/
/*                            datum->wraps                                */
/*========================================================================*/

static Scheme_Object *unmarshal_mark(Scheme_Object *_a, Scheme_Unmarshal_Tables *ut)
{
  Scheme_Object *n, *a = _a;

  if (SCHEME_INTP(a) && IS_POSMARK(a))
    a = scheme_make_integer(-SCHEME_INT_VAL(a));
  else if (!SCHEME_NUMBERP(a))
    return NULL;
  else
    a = scheme_intern_symbol(scheme_number_to_string(10, a));
  
  /* Picked a mapping yet? */
  n = scheme_hash_get(ut->rns, a);
  if (!n) {
    /* Map marshaled mark to a new mark. */
    n = scheme_new_mark();
    scheme_hash_set(ut->rns, a, n);
  }
  
  /* Really a mark? */
  if (!SCHEME_NUMBERP(n))
    return NULL;

  return n;
}

#if 0 
# define return_NULL return (printf("%d\n", __LINE__), NULL)
#else
# define return_NULL return NULL
#endif

static int ok_phase(Scheme_Object *o) {
  return (SCHEME_INTP(o) || SCHEME_BIGNUMP(o) || SCHEME_FALSEP(o));
}
static int ok_phase_index(Scheme_Object *o) {
  return ok_phase(o);
}

static Scheme_Object *datum_to_module_renames(Scheme_Object *a, Scheme_Hash_Table *ht, int lex_ok,
                                              Scheme_Unmarshal_Tables *ut)
{
  int count, i;
  Scheme_Object *key, *p0, *p;

  if (!SCHEME_VECTORP(a)) return_NULL;
  count = SCHEME_VEC_SIZE(a);
  if (count & 0x1) return_NULL;

  for (i = 0; i < count; i+= 2) {
    key = SCHEME_VEC_ELS(a)[i];
    p0 = SCHEME_VEC_ELS(a)[i+1];
	
    if (!SCHEME_SYMBOLP(key)) return_NULL;

    p = p0;

    if (SAME_TYPE(SCHEME_TYPE(p), scheme_module_index_type)) {
      /* Ok */
    } else if (SCHEME_PAIRP(p)) {
      Scheme_Object *midx;

      midx = SCHEME_CAR(p);
      if (!SAME_TYPE(SCHEME_TYPE(midx), scheme_module_index_type))
        return_NULL;

      if (SCHEME_SYMBOLP(SCHEME_CDR(p))) {
        /* Ok */
      } else if (SAME_TYPE(SCHEME_TYPE(SCHEME_CDR(p)), scheme_module_index_type)) {
        /* Ok */
      } else {
        Scheme_Object *ap, *bp;

        ap = SCHEME_CDR(p);
        if (!SCHEME_PAIRP(ap))
          return_NULL;

        /* mod-phase, maybe */
        if (SCHEME_INTP(SCHEME_CAR(ap))) {
          bp = SCHEME_CDR(ap);
        } else
          bp = ap;
            
        /* exportname */
        if (!SCHEME_PAIRP(bp))
          return_NULL;
        ap = SCHEME_CAR(bp);
        if (!SCHEME_SYMBOLP(ap))
          return_NULL;
            
        /* nominal_modidx_plus_phase */
        bp = SCHEME_CDR(bp);
        if (!SCHEME_PAIRP(bp))
          return_NULL;
        ap = SCHEME_CAR(bp);
        if (SAME_TYPE(SCHEME_TYPE(ap), scheme_module_index_type)) {
          /* Ok */
        } else if (SCHEME_PAIRP(ap)) {
          if (!SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(ap)), scheme_module_index_type))
            return_NULL;
          ap = SCHEME_CDR(ap);
          /* import_phase_plus_nominal_phase */
          if (SCHEME_PAIRP(ap)) {
            if (!ok_phase_index(SCHEME_CAR(ap))) return_NULL;
            if (!ok_phase_index(SCHEME_CDR(ap))) return_NULL;
          } else if (!ok_phase_index(ap))
            return_NULL;
        } else
          return_NULL;

        /* nominal_exportname */
        ap = SCHEME_CDR(bp);
        if (!SCHEME_SYMBOLP(ap))
          return_NULL;
      }
    } else if (lex_ok) {
      Scheme_Object *ap;
      if (!SCHEME_BOXP(p))
        return_NULL;
      ap = SCHEME_BOX_VAL(p);
      if (!SCHEME_PAIRP(ap))
        return_NULL;
      if (!SCHEME_SYMBOLP(SCHEME_CAR(ap)))
        return_NULL;
      ap = SCHEME_CDR(ap);
      if (!SCHEME_SYMBOLP(ap) && !SCHEME_FALSEP(ap))
        return_NULL;
    } else
      return_NULL;
	
    scheme_hash_set(ht, key, p0);
  }

  return scheme_true;
}

static Scheme_Object *datum_to_wraps(Scheme_Object *w,
                                     Scheme_Unmarshal_Tables *ut)
{
  Scheme_Object *a, *wraps_key, *local_key;
  int stack_size, decoded;
  Wrap_Chunk *wc;

  /* ut->rns maps numbers (table indices) to renaming tables, and negative
     numbers (negated fixnum marks) and symbols (interned marks) to marks.*/

  /* This function has to be defensive, since `w' can originate in
     untrusted .zo bytecodes. Return NULL for bad wraps. */

  if (SCHEME_INTP(w)) {
    wraps_key = w;
    w = scheme_unmarshal_wrap_get(ut, wraps_key, &decoded);
    if (decoded && (!w || !SCHEME_LISTP(w))) /* list => a wrap, as opposed to a mark, etc. */
      return_NULL;
    if (decoded)
      return w;
  } else {
    /* not shared */
    wraps_key = NULL;
  }

  stack_size = scheme_proper_list_length(w);
  if (stack_size < 1) {
    scheme_unmarshal_wrap_set(ut, wraps_key, scheme_null);
    return scheme_null;
  } else if (stack_size < 2) {
    wc = NULL;
  } else {
    wc = MALLOC_WRAP_CHUNK(stack_size);
    wc->type = scheme_wrap_chunk_type;
    wc->len = stack_size;
  }

  a = NULL;

  while (!SCHEME_NULLP(w)) {
    a = SCHEME_CAR(w);
    if (SCHEME_NUMBERP(a)) {
      /* Re-use rename table or env rename */
      local_key = a;
      a = scheme_unmarshal_wrap_get(ut, local_key, &decoded);
      if (decoded && (!a || SCHEME_LISTP(a))) /* list => a whole wrap, no good as an element */
	return_NULL;
    } else  {
      /* Not shared */
      local_key = NULL;
      decoded = 0;
    }

    if (decoded) {
      /* done */
    } else if (SCHEME_PAIRP(a) 
	       && SCHEME_NULLP(SCHEME_CDR(a))
	       && SCHEME_NUMBERP(SCHEME_CAR(a))) {
      /* Mark */
      a = unmarshal_mark(SCHEME_CAR(a), ut);
      if (!a) return_NULL;
    } else if (SCHEME_VECTORP(a)) {
      /* A (simplified) rename table. */
      int sz = SCHEME_VEC_SIZE(a), cnt, i, any_free_id_renames = 0;
      Scheme_Object *v;

      /* Make sure that it's a well-formed rename table. */
      if (sz < 2)
	return_NULL;
      cnt = (sz - 2) >> 1;
      for (i = 0; i < cnt; i++) {
	if (!SCHEME_SYMBOLP(SCHEME_VEC_ELS(a)[i + 2]))
	  return_NULL;
        v = SCHEME_VEC_ELS(a)[i + cnt + 2];
        if (SCHEME_SYMBOLP(v)) {
          /* simple target-environment symbol */
        } else if (SCHEME_PAIRP(v)) {
          /* target-environment symbol paired with free-id=? rename info */
          any_free_id_renames = 1;
          if (!SCHEME_SYMBOLP(SCHEME_CAR(v)))
            return_NULL;
          v = SCHEME_CDR(v);
          if (SCHEME_PAIRP(v)) {
            if (!SCHEME_SYMBOLP(SCHEME_CAR(v)))
              return_NULL;
            v = SCHEME_CDR(v);
            if (!SCHEME_SYMBOLP(v) && !SCHEME_FALSEP(v))
              return_NULL;
          } else if (SAME_TYPE(SCHEME_TYPE(v), scheme_free_id_info_type)) {
            if (!SCHEME_MODIDXP(SCHEME_VEC_ELS(v)[0])
                || !SCHEME_SYMBOLP(SCHEME_VEC_ELS(v)[1])
                || !SCHEME_MODIDXP(SCHEME_VEC_ELS(v)[2])
                || !SCHEME_SYMBOLP(SCHEME_VEC_ELS(v)[3])
                || !ok_phase(SCHEME_VEC_ELS(v)[4])
                || !ok_phase(SCHEME_VEC_ELS(v)[5])
                || !ok_phase(SCHEME_VEC_ELS(v)[6]))
              return_NULL;
          } else
            return_NULL;
        } else
          return_NULL;
      }

      SCHEME_VEC_ELS(a)[0] = (any_free_id_renames ? scheme_true : scheme_false);
      
      if (!SCHEME_FALSEP(SCHEME_VEC_ELS(a)[1])) {
        SCHEME_VEC_ELS(a)[1] = scheme_false;
        maybe_install_rename_hash_table(a);
      }

      /* It's ok: */
      scheme_unmarshal_wrap_set(ut, local_key, a);
    } else if (SCHEME_PAIRP(a)) {
      /* A rename table:
           - ([#t] <phase-num> <kind-num> <set-identity> [unmarshal] #(<table-elem> ...)
	       . ((<sym> (<marked-list-or-mark> . <target-gensym>) ...) ...)) ; <- marked_names
	where a <table-elem> is actually two values, one of:
           - <exname> <modname>
           - <exname> (<modname> . <defname>)
      */
      Scheme_Object *mns;
      Module_Renames *mrn;
      Scheme_Object *p, *key;
      int kind;
      Scheme_Object *phase, *set_identity;
      
      if (!SCHEME_PAIRP(a)) return_NULL;
      
      /* Convert list to rename table: */
      
      if (SAME_OBJ(SCHEME_CAR(a), scheme_true)) {
        scheme_signal_error("leftover plus-kernel");
      }

      if (!SCHEME_PAIRP(a)) return_NULL;
      phase = SCHEME_CAR(a);
      if (!ok_phase(phase)) return_NULL;
      a = SCHEME_CDR(a);

      if (!SCHEME_PAIRP(a)) return_NULL;
      if (SCHEME_TRUEP(SCHEME_CAR(a)))
	kind = mzMOD_RENAME_MARKED;
      else
	kind = mzMOD_RENAME_NORMAL;
      a = SCHEME_CDR(a);

      if (!SCHEME_PAIRP(a)) return_NULL;
      set_identity = unmarshal_mark(SCHEME_CAR(a), ut); 
      if (!set_identity) return_NULL;
      a = SCHEME_CDR(a);
      
      mrn = (Module_Renames *)scheme_make_module_rename(phase, kind, 
                                                        NULL, NULL, 
                                                        set_identity);

      if (!SCHEME_PAIRP(a)) return_NULL;
      mns = SCHEME_CDR(a);
      a = SCHEME_CAR(a);

      if (!SCHEME_VECTORP(a)) {
	/* Unmarshall info: */
	Scheme_Object *ml = a, *mli, *first = scheme_null, *last = NULL, *ai;
	while (SCHEME_PAIRP(ml)) {
          ai = SCHEME_CAR(ml);
	  mli = ai;
	  if (!SCHEME_PAIRP(mli)) return_NULL;

	  /* A module path index: */
	  p = SCHEME_CAR(mli);
	  if (!(SCHEME_SYMBOLP(p)
		|| SAME_TYPE(SCHEME_TYPE(p), scheme_module_index_type)))
	    return_NULL;
	  mli = SCHEME_CDR(mli);

          if (!SCHEME_PAIRP(mli)) return_NULL;

          /* A phase/dimension index k */
          p = SCHEME_CAR(mli);
          if (!ok_phase_index(p))
            return_NULL;
          
          p = SCHEME_CDR(mli);
          if (SCHEME_PAIRP(p) && (SCHEME_PAIRP(SCHEME_CAR(p))
                                  || SCHEME_VECTORP(SCHEME_CAR(p)))) {
            /* list of marks or a vector of marks and bdg: */
            Scheme_Object *m_first = scheme_null, *m_last = NULL, *mp, *after_marks, *bdg;

            after_marks = SCHEME_CDR(p);
            mli = SCHEME_CAR(p);

            if (SCHEME_VECTORP(mli)) {
              if (SCHEME_VEC_SIZE(mli) != 2) return_NULL;
              bdg = SCHEME_VEC_ELS(mli)[1];
              mli = SCHEME_VEC_ELS(mli)[0];
            } else
              bdg = NULL;

            while (SCHEME_PAIRP(mli)) {
              p = SCHEME_CAR(mli);
              p = unmarshal_mark(p, ut); 
              if (!p) return_NULL;

              mp = scheme_make_pair(p, scheme_null);
              if (m_last)
                SCHEME_CDR(m_last) = mp;
              else
                m_first = mp;
              m_last = mp;

              mli = SCHEME_CDR(mli);
            }
            if (!SCHEME_NULLP(mli)) return_NULL;

            mli = m_first;

            if (bdg) {
              if (!SCHEME_SYMBOLP(bdg) && !SCHEME_FALSEP(bdg)) {
                if (SCHEME_MARKP(bdg))
                  bdg = unmarshal_mark(bdg, ut);
                else {
                  m_first = scheme_null;
                  m_last = NULL;
                  while (SCHEME_PAIRP(bdg)) {
                    p = SCHEME_CAR(bdg);
                    if (!SCHEME_MARKP(p)) return_NULL;
                    p = unmarshal_mark(p, ut);
                    mp = scheme_make_pair(p, scheme_null);
                    if (m_last)
                      SCHEME_CDR(m_last) = mp;
                    else
                      m_first = mp;
                    m_last = mp;
                    bdg = SCHEME_CDR(bdg);
                  }
                  if (!SCHEME_NULLP(bdg)
                      || !SCHEME_PAIRP(m_first)
                      || !SCHEME_PAIRP(SCHEME_CDR(m_first)))
                    return_NULL;
                  bdg = m_first;
                }
              }
              mli = scheme_make_vector(2, mli);
              SCHEME_VEC_ELS(mli)[1] = bdg;
            }

            /* Rebuild for unmarshaled marks: */
            ai = scheme_make_pair(SCHEME_CAR(ai),
                                  scheme_make_pair(SCHEME_CADR(ai),
                                                   scheme_make_pair(mli, after_marks)));

            p = after_marks;
          }

          if (ok_phase_index(p)) {
            /* For a shared table: src-phase-index */
          } else {
            /* For a non-shared table: (list* src-phase-index exceptions prefix), after k */
            mli = p;
            if (!SCHEME_PAIRP(mli)) return_NULL;

            p = SCHEME_CAR(mli);
            if (!ok_phase_index(p))
              return_NULL;
            mli = SCHEME_CDR(mli);

            if (!SCHEME_PAIRP(mli)) return_NULL;

            /* A list of symbols: */
            p = SCHEME_CAR(mli);
            while (SCHEME_PAIRP(p)) {
              if (!SCHEME_SYMBOLP(SCHEME_CAR(p))) return_NULL;
              p = SCHEME_CDR(p);
            }
            if (!SCHEME_NULLP(p)) return_NULL;

            /* #f or a symbol: */
            p = SCHEME_CDR(mli);
            if (!SCHEME_SYMBOLP(p) && !SCHEME_FALSEP(p)) return_NULL;
          }

	  ml = SCHEME_CDR(ml);

          /* rebuild, in case we converted marks */
          p = scheme_make_pair(ai, scheme_null);
          if (last)
            SCHEME_CDR(last) = p;
          else
            first = p;
          last = p;
	}
	if (!SCHEME_NULLP(ml)) return_NULL;

	mrn->unmarshal_info = first;
	if (SCHEME_PAIRP(first))
	  mrn->needs_unmarshal = 1;

	if (!SCHEME_PAIRP(mns)) return_NULL;
	a = SCHEME_CAR(mns);
	mns = SCHEME_CDR(mns);
      }

      if (!datum_to_module_renames(a, mrn->ht, 0, ut))
        return_NULL;

      /* Extract free-id=? renames, if any */
      if (SCHEME_PAIRP(mns) && SCHEME_VECTORP(SCHEME_CAR(mns))) {
        Scheme_Hash_Table *ht;
        ht = scheme_make_hash_table(SCHEME_hash_ptr);
        mrn->free_id_renames = ht;
        if (!datum_to_module_renames(SCHEME_CAR(mns), mrn->free_id_renames, 1, ut))
          return_NULL;
        mns = SCHEME_CDR(mns);
      }

      /* Extract the mark-rename table, if any: */
      if (SCHEME_PAIRP(mns)) {
	Scheme_Hash_Table *ht;
	Scheme_Object *ll, *kkey, *kfirst, *klast, *kp;

	ht = scheme_make_hash_table(SCHEME_hash_ptr);
	for (; SCHEME_PAIRP(mns); mns = SCHEME_CDR(mns)) {
	  p = SCHEME_CAR(mns);
	  if (!SCHEME_PAIRP(p)) return_NULL;
	  key = SCHEME_CAR(p);
	  p = SCHEME_CDR(p);
	  if (!SCHEME_SYMBOLP(key)) return_NULL;
	  
	  ll = scheme_null;

	  /* Convert marks */
	  for (; SCHEME_PAIRP(p); p = SCHEME_CDR(p)) {
	    a = SCHEME_CAR(p);
	    if (!SCHEME_PAIRP(a))  return_NULL;
	    kkey = SCHEME_CDR(a);
	    if (!SCHEME_SYMBOLP(kkey)) return_NULL;

	    kfirst = scheme_null;
	    klast = NULL;
	    a = SCHEME_CAR(a);
	    if (SCHEME_MARKP(a)) {
	      kfirst = unmarshal_mark(a, ut);
	    } else {
              Scheme_Object *bdg = NULL;

              if (SCHEME_VECTORP(a)) {
                if (SCHEME_VEC_SIZE(a) != 2) return_NULL;
                bdg = SCHEME_VEC_ELS(a)[1];
                if (SCHEME_SYMBOLP(bdg)) {
                  /* ok */
                } else if (SCHEME_MARKP(bdg)) {
                  bdg = unmarshal_mark(bdg, ut);
                } else {
                  Scheme_Object *bl = scheme_null;
                  while (SCHEME_PAIRP(bdg)) {
                    if (SCHEME_MARKP(SCHEME_CAR(bdg)))
                      bl = scheme_make_pair(unmarshal_mark(SCHEME_CAR(bdg), ut),
                                            bl);
                    else
                      break;
                    bdg = SCHEME_CDR(bdg);
                  }
                  if (!SCHEME_NULLP(bdg))
                    return_NULL;
                  bdg = scheme_reverse(bl);
                }
                a = SCHEME_VEC_ELS(a)[0];
              }

	      for (; SCHEME_PAIRP(a); a = SCHEME_CDR(a)) {
		kp = CONS(unmarshal_mark(SCHEME_CAR(a), ut), scheme_null);
		if (!klast)
		  kfirst = kp;
		else
		  SCHEME_CDR(klast) = kp;
		klast = kp;
	      }
	      if (!SCHEME_NULLP(a)) {
                if (bdg && SCHEME_MARKP(a) && SCHEME_NULLP(kfirst))
                  kfirst = unmarshal_mark(a, ut);
                else
                  return_NULL;
              }

              if (bdg) {
                a = scheme_make_vector(2, NULL);
                SCHEME_VEC_ELS(a)[0] = kfirst;
                SCHEME_VEC_ELS(a)[1] = bdg;
                kfirst = a;
              }
	    }

	    ll = CONS(CONS(kfirst, kkey), ll);
	  }
	  
	  scheme_hash_set(ht, key, ll);

	  if (!SCHEME_NULLP(p)) return_NULL;
	}
	if (!SCHEME_NULLP(mns)) return_NULL;

	mrn->marked_names = ht;
      }

      scheme_unmarshal_wrap_set(ut, local_key, (Scheme_Object *)mrn);

      scheme_seal_module_rename((Scheme_Object *)mrn, STX_SEAL_ALL);

      a = (Scheme_Object *)mrn;
    } else if (SAME_OBJ(a, scheme_true)
               || SCHEME_FALSEP(a)) {
      /* current env rename */
      Scheme_Env *env;

      env = scheme_get_env(NULL);
      scheme_prepare_env_renames(env, mzMOD_RENAME_TOPLEVEL);
      a = scheme_get_module_rename_from_set(env->rename_set, 
                                            (SCHEME_FALSEP(a) 
                                             ? scheme_make_integer(1) 
                                             : scheme_make_integer(0)), 
                                            1);
    } else if (SCHEME_SYMBOLP(a)) {
      /* mark barrier */
    } else if (SCHEME_BOXP(a)) {
      if (SCHEME_PAIRP(SCHEME_BOX_VAL(a))) {
        /* prune context */
        a = make_prune_context(SCHEME_BOX_VAL(a));
      } else {
        /* must be a phase shift */
        Scheme_Object *vec, *cancel_id;
        vec = SCHEME_BOX_VAL(a);
        if (!SCHEME_VECTORP(vec)) return_NULL;
        if (SCHEME_VEC_SIZE(vec) != 6) return_NULL;

        cancel_id = SCHEME_VEC_ELS(vec)[5];
        if (SCHEME_TRUEP(cancel_id)) {
          cancel_id = unmarshal_mark(cancel_id, ut);
          SCHEME_VEC_ELS(vec)[5] = cancel_id;
        }
      }
    } else {
      return_NULL;
    }

    if (wc)
      wc->a[--stack_size] = a;

    w = SCHEME_CDR(w);
  }

  if (wc)
    a = (Scheme_Object *)wc;
  a = CONS(a, scheme_null);

  scheme_unmarshal_wrap_set(ut, wraps_key, a);

  return a;
}

/*========================================================================*/
/*                           datum->syntax                                */
/*========================================================================*/


#ifdef DO_STACK_CHECK
static Scheme_Object *datum_to_syntax_inner(Scheme_Object *o, 
					    Scheme_Unmarshal_Tables *ut,
					    Scheme_Stx *stx_src,
					    Scheme_Stx *stx_wraps,
                                            Scheme_Hash_Table *ht,
                                            int tainted);

static Scheme_Object *datum_to_syntax_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Stx *stx_src = (Scheme_Stx *)p->ku.k.p2;
  Scheme_Stx *stx_wraps = (Scheme_Stx *)p->ku.k.p3;
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p->ku.k.p4;
  Scheme_Unmarshal_Tables *ut = (Scheme_Unmarshal_Tables *)p->ku.k.p5;
					    
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;
  p->ku.k.p5 = NULL;

  return datum_to_syntax_inner(o, ut, stx_src, stx_wraps, ht, (int)p->ku.k.i1);
}
#endif

static Scheme_Object *datum_to_syntax_inner(Scheme_Object *o, 
                                            Scheme_Unmarshal_Tables *ut,
					    Scheme_Stx *stx_src,
					    Scheme_Stx *stx_wraps, /* or rename table, or boxed precomputed wrap */
					    Scheme_Hash_Table *ht,
                                            int tainted)
{
  Scheme_Object *result, *wraps, *hashed;
  int do_not_unpack_wraps = 0, armed = 0;

  if (SCHEME_STXP(o))
    return o;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)o;
      p->ku.k.p2 = (void *)stx_src;
      p->ku.k.p3 = (void *)stx_wraps;
      p->ku.k.p4 = (void *)ht;
      p->ku.k.p5 = (void *)ut;
      p->ku.k.i1 = tainted;
      return scheme_handle_stack_overflow(datum_to_syntax_k);
    }
  }
#endif

  SCHEME_USE_FUEL(1);

  if (ht) {
    if (HAS_CHAPERONE_SUBSTX(o)) {
      if (scheme_hash_get(ht, o)) {
        /* Graphs disallowed */
        return_NULL;
      }

      scheme_hash_set(ht, o, scheme_true);
      hashed = o;
    } else 
      hashed = NULL;
  } else
    hashed = NULL;

  if (ut && !SCHEME_BOXP(stx_wraps)) {
    if (SCHEME_VECTORP(o)) {
      if (SCHEME_VEC_SIZE(o) == 1) {
        /* tainted --- forced on all enclosed syntax objects, too */
	o = SCHEME_VEC_ELS(o)[0];
        tainted = 1;
      } else if (SCHEME_VEC_SIZE(o) == 2) {
        /* armed */
        o = SCHEME_VEC_ELS(o)[0];
        armed = 1;
      } else
	return_NULL;
    }
    if (!SCHEME_PAIRP(o)) 
      return_NULL;
    wraps = SCHEME_CDR(o);
    o = SCHEME_CAR(o);
  } else if (SCHEME_BOXP(stx_wraps)) {
    /* Shared wraps, to be used directly everywhere: */
    wraps = SCHEME_BOX_VAL(stx_wraps);
    do_not_unpack_wraps = 1;
  } else
    wraps = NULL;

  if (SCHEME_PAIRP(o)) {
    Scheme_Object *first = NULL, *last = NULL, *p;
    
    /* Check whether it's all conses with
       syntax inside */
    p = o;
    while (SCHEME_PAIRP(p)) {
      if (!SCHEME_STXP(SCHEME_CAR(p)))
	break;
      p = SCHEME_CDR(p);
    }
    if (SCHEME_NULLP(p) || SCHEME_STXP(p)) {
      result = o;
    } else {
      int cnt = -1;
      Scheme_Stx *sub_stx_wraps = stx_wraps;

      if (wraps && !SCHEME_BOXP(stx_wraps) && SAME_OBJ(SCHEME_CAR(o), scheme_true)) {
        /* Resolve wraps now, and then share it with
           all nested objects (as indicated by a box
           for stx_wraps). */
        wraps = datum_to_wraps(wraps, ut);
        do_not_unpack_wraps = 1;
        sub_stx_wraps = (Scheme_Stx *)scheme_box(wraps);
        o = SCHEME_CDR(o);
      } else if (wraps && !SCHEME_BOXP(stx_wraps) && SCHEME_INTP(SCHEME_CAR(o))) {
	/* First element is the number of items
	   before a non-null terminal: */
	cnt = SCHEME_INT_VAL(SCHEME_CAR(o));
	o = SCHEME_CDR(o);
      }

      /* Build up a new list while converting elems */
      while (SCHEME_PAIRP(o) && cnt) {
	Scheme_Object *a;
      
	if (ht && last) {
	  if (scheme_hash_get(ht, o)) {
            /* cdr is shared. Stop here and let someone else complain. */
            break;
	  }
	}

	a = datum_to_syntax_inner(SCHEME_CAR(o), ut, stx_src, sub_stx_wraps, ht, tainted);
	if (!a) return_NULL;
      
	p = scheme_make_pair(a, scheme_null);
      
	if (last)
	  SCHEME_CDR(last) = p;
	else
	  first = p;
	last = p;
	o = SCHEME_CDR(o);

	--cnt;
      }
      if (!first) return_NULL;
      if (!SCHEME_NULLP(o)) {
	o = datum_to_syntax_inner(o, ut, stx_src, sub_stx_wraps, ht, tainted);
	if (!o) return_NULL;
	SCHEME_CDR(last) = o;
      }

      result = first;
    }
  } else if (SCHEME_CHAPERONE_BOXP(o)) {
    if (SCHEME_NP_CHAPERONEP(o))
      o = scheme_unbox(o);
    else
      o = SCHEME_PTR_VAL(o);

    o = datum_to_syntax_inner(o, ut, stx_src, stx_wraps, ht, tainted);
    if (!o) return_NULL;
    result = scheme_box(o);
    SCHEME_SET_BOX_IMMUTABLE(result);
  } else if (SCHEME_CHAPERONE_VECTORP(o)) {
    int size, i;
    Scheme_Object *a, *oo;

    oo = o;
    if (SCHEME_NP_CHAPERONEP(o))
      o = SCHEME_CHAPERONE_VAL(o);
    size = SCHEME_VEC_SIZE(o);

    result = scheme_make_vector(size, NULL);
    
    for (i = 0; i < size; i++) {
      if (SAME_OBJ(o, oo))
        a = SCHEME_VEC_ELS(o)[i];
      else
        a = scheme_chaperone_vector_ref(oo, i);
      a = datum_to_syntax_inner(a, ut, stx_src, stx_wraps, ht, tainted);
      if (!a) return_NULL;
      SCHEME_VEC_ELS(result)[i] = a;
    }

    SCHEME_SET_VECTOR_IMMUTABLE(result);
  } else if (SCHEME_CHAPERONE_HASHTRP(o)) {
    Scheme_Hash_Tree *ht1, *ht2;
    Scheme_Object *key, *val;
    mzlonglong i;

    if (SCHEME_NP_CHAPERONEP(o))
      ht1 = (Scheme_Hash_Tree *)SCHEME_CHAPERONE_VAL(o);
    else
      ht1 = (Scheme_Hash_Tree *)o;
    
    ht2 = scheme_make_hash_tree(SCHEME_HASHTR_FLAGS(ht1) & 0x3);
    
    i = scheme_hash_tree_next(ht1, -1);
    while (i != -1) {
      scheme_hash_tree_index(ht1, i, &key, &val);
      if (!SAME_OBJ((Scheme_Object *)ht1, o))
        val = scheme_chaperone_hash_traversal_get(o, key, &key);
      val = datum_to_syntax_inner(val, ut, stx_src, stx_wraps, ht, tainted);
      if (!val) return NULL;
      ht2 = scheme_hash_tree_set(ht2, key, val);
      i = scheme_hash_tree_next(ht1, i);
    }
    
    result = (Scheme_Object *)ht2;
  } else if (prefab_p(o) || (SCHEME_CHAPERONEP(o) && prefab_p(SCHEME_CHAPERONE_VAL(o)))) {
    Scheme_Structure *s;
    Scheme_Object *a;
    int size, i;

    s = (Scheme_Structure *)scheme_clone_prefab_struct_instance((Scheme_Structure *)o);
    size = s->stype->num_slots;

    for (i = 0; i < size; i++) {
      a = datum_to_syntax_inner(s->slots[i], ut, stx_src, stx_wraps, ht, tainted);
      if (!a) return NULL;
      s->slots[i] = a;
    }

    result = (Scheme_Object *)s;
  } else {
    if (!wraps)
      o = scheme_read_intern(o);
    result = o;
  }

  if (SCHEME_FALSEP((Scheme_Object *)stx_src))
    result = scheme_make_stx(result, empty_srcloc, NULL);
  else
    result = scheme_make_stx(result, stx_src->srcloc, NULL);

  if (tainted)
    (void)add_taint_to_stx(result, 0);
  else if (armed) {
    /* Arm with #f as the inspector; #f is replaced by a
       specific inspector when the encloding code is instanted */
    Scheme_Object *l; 
    l = taint_intern(scheme_make_pair(scheme_false, scheme_null));
    l = taint_intern(scheme_make_pair(scheme_false, l));
    ((Scheme_Stx *)result)->taints = l;
  }

  if (wraps) {
    if (!do_not_unpack_wraps) {
      wraps = datum_to_wraps(wraps, ut);
      if (!wraps)
        return_NULL;
    }
    ((Scheme_Stx *)result)->wraps = wraps;
  } else if (SCHEME_FALSEP((Scheme_Object *)stx_wraps)) {
    /* wraps already nulled */
  } else {
    /* Note: no propagation will be needed for SUBSTX */
    ((Scheme_Stx *)result)->wraps = stx_wraps->wraps;
  }

  if (hashed) {
    scheme_hash_set(ht, hashed, NULL);
  }
  
  return result;
}

static int quick_check_graph(Scheme_Object *o, int fuel)
{
  if (!fuel) return 0;

  if (SCHEME_PAIRP(o))
    return quick_check_graph(SCHEME_CDR(o),
                             quick_check_graph(SCHEME_CAR(o), fuel - 1));

  if (HAS_CHAPERONE_SUBSTX(o))
    return 0;
  else
    return fuel;
}

static Scheme_Object *general_datum_to_syntax(Scheme_Object *o, 
                                              Scheme_Unmarshal_Tables *ut,
                                              Scheme_Object *stx_src,
                                              Scheme_Object *stx_wraps,
                                              int can_graph, int copy_props)
     /* If stx_wraps is a hash table, then `o' includes marks.
	If copy_props > 0, properties are copied from src.
	If copy_props != 1 or 0, then taint armings are copied from src, too,
          but src must not be tainted. */
{
  Scheme_Hash_Table *ht;
  Scheme_Object *v, *code = NULL;

  if (!SCHEME_FALSEP(stx_src) && !SCHEME_STXP(stx_src))
    return o;

  if (SCHEME_STXP(o))
    return o;

  if (can_graph && !quick_check_graph(o, 10))
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
  else
    ht = NULL;

  if (ut) {
    /* If o is just a number, look it up in the table. */
    if (SCHEME_INTP(o)) {
      int decoded;
      v = scheme_unmarshal_wrap_get(ut, o, &decoded);
      if (!decoded) {
        code = o;
        o = v;
      } else
        return v;
    }
  }

  v = datum_to_syntax_inner(o, 
                            ut,
			    (Scheme_Stx *)stx_src,
			    (Scheme_Stx *)stx_wraps,
			    ht,
                            0);

  if (!v) {
    if (ut)
      return_NULL; /* happens with bad wraps from a bad .zo */
    /* otherwise, only happens with cycles: */
    scheme_contract_error("datum->syntax",
                          "cannot create syntax from cyclic datum",
                          "datum", 1, o,
                          NULL);
    return NULL;
  }

  if (code) {
    scheme_unmarshal_wrap_set(ut, code, v);
  }

  if (copy_props > 0)
    ((Scheme_Stx *)v)->props = ((Scheme_Stx *)stx_src)->props;

  if (copy_props && (copy_props != 1)) {
    if (!is_clean(stx_src)) {
      if (is_tainted(stx_src))
        scheme_signal_error("internal error: cannot copy taint armings from tainted source");
      v = add_taint_armings_to_stx(v, ((Scheme_Stx *)stx_src)->taints, 0);
    }
  }

  return v;
}

Scheme_Object *scheme_datum_to_syntax(Scheme_Object *o, 
				      Scheme_Object *stx_src,
				      Scheme_Object *stx_wraps,
				      int can_graph, int copy_props)
{
  return general_datum_to_syntax(o, NULL, stx_src, stx_wraps, can_graph, copy_props);
}

Scheme_Object *scheme_unmarshal_datum_to_syntax(Scheme_Object *o,
                                                struct Scheme_Unmarshal_Tables *ut,
                                                int can_graph)
{
  return general_datum_to_syntax(o, ut, scheme_false, scheme_false, can_graph, 0);
}

/*========================================================================*/
/*                    Racket functions and helpers                        */
/*========================================================================*/

static Scheme_Object *syntax_p(int argc, Scheme_Object **argv)
{
  return SCHEME_STXP(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *syntax_to_datum(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax->datum", "syntax?", 0, argc, argv);
    
  return scheme_syntax_to_datum(argv[0], 0, NULL);
}

static int nonneg_exact_or_false_p(Scheme_Object *o)
{
  return SCHEME_FALSEP(o) || scheme_nonneg_exact_p(o);
}

static int pos_exact_or_false_p(Scheme_Object *o)
{
  return (SCHEME_FALSEP(o)
	  || (SCHEME_INTP(o) && (SCHEME_INT_VAL(o) > 0))
	  || (SCHEME_BIGNUMP(o) && SCHEME_BIGPOS(o)));
}

static Scheme_Object *datum_to_syntax(int argc, Scheme_Object **argv)
{
  Scheme_Object *src = scheme_false, *properties = NULL;
  
  if (!SCHEME_FALSEP(argv[0]) && !SCHEME_STXP(argv[0]))
    scheme_wrong_contract("datum->syntax", "(or/c syntax? #f)", 0, argc, argv);
  if (argc > 2) {
    int ll;

    src = argv[2];

    ll = scheme_proper_list_length(src);

    if (SCHEME_CHAPERONEP(src)) {
      src = SCHEME_CHAPERONE_VAL(src);
      if (SCHEME_VECTORP(src) && (SCHEME_VEC_SIZE(src) == 5)) {
        Scheme_Object *a;
        int i;
        src = scheme_make_vector(5, NULL);
        for (i = 0; i < 5; i++) {
          a = scheme_chaperone_vector_ref(argv[2], i);
          SCHEME_VEC_ELS(src)[i] = a;
        }
      }
    }

    if (!SCHEME_FALSEP(src) 
	&& !SCHEME_STXP(src)
	&& !(SCHEME_VECTORP(src)
             && (SCHEME_VEC_SIZE(src) == 5)
	     && pos_exact_or_false_p(SCHEME_VEC_ELS(src)[1])
	     && nonneg_exact_or_false_p(SCHEME_VEC_ELS(src)[2])
	     && pos_exact_or_false_p(SCHEME_VEC_ELS(src)[3])
	     && nonneg_exact_or_false_p(SCHEME_VEC_ELS(src)[4]))
	&& !((ll == 5)
	     && pos_exact_or_false_p(SCHEME_CADR(src))
	     && nonneg_exact_or_false_p(SCHEME_CADR(SCHEME_CDR(src)))
	     && pos_exact_or_false_p(SCHEME_CADR(SCHEME_CDR(SCHEME_CDR(src))))
	     && nonneg_exact_or_false_p(SCHEME_CADR(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(src)))))))
      scheme_wrong_type("datum->syntax", "syntax, source location vector or list, or #f", 2, argc, argv);

    if (SCHEME_VECTORP(src))
      ll = 5;

    if (argc > 3) {
      if (!SCHEME_FALSEP(argv[3])) {
	if (!SCHEME_STXP(argv[3]))
	  scheme_wrong_contract("datum->syntax", "(or/c syntax? #f)", 3, argc, argv);
	properties = ((Scheme_Stx *)argv[3])->props;
      }
      
      if (argc > 4) {
        /* Not used; allowed for backward-compatibility */
        if (!SCHEME_FALSEP(argv[4])) {
          if (!SCHEME_STXP(argv[4]))
            scheme_wrong_contract("datum->syntax", "(or/c syntax? #f)", 4, argc, argv);
        } 
      }
    }

    if (ll == 5) {
      /* line--column--pos--span format */
      Scheme_Object *line, *col, *pos, *span;
      if (SCHEME_VECTORP(src)) {
        line = SCHEME_VEC_ELS(src)[1];
        col = SCHEME_VEC_ELS(src)[2];
        pos = SCHEME_VEC_ELS(src)[3];
        span = SCHEME_VEC_ELS(src)[4];
        src = SCHEME_VEC_ELS(src)[0];
      } else {
        line = SCHEME_CADR(src);
        col = SCHEME_CADR(SCHEME_CDR(src));
        pos = SCHEME_CADR(SCHEME_CDR(SCHEME_CDR(src)));
        span = SCHEME_CADR(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(src))));
        src = SCHEME_CAR(src);
      }
      
      if (SCHEME_FALSEP(line) != SCHEME_FALSEP(col))
	scheme_contract_error("datum->syntax", 
                              "line and column positions must both be numbers or #f", 
                              "in location", 1, argv[2],
                              NULL);

      /* Too-large positions go to unknown */
      if (SCHEME_BIGNUMP(line) || SCHEME_BIGNUMP(col)) {
	line = scheme_make_integer(-1);
	col = scheme_make_integer(-1);
      }
      if (SCHEME_BIGNUMP(pos))
	pos = scheme_make_integer(-1);
      if (span && SCHEME_BIGNUMP(span))
	span = scheme_make_integer(-1);

      src = scheme_make_stx_w_offset(scheme_false,
				     SCHEME_FALSEP(line) ? -1 : SCHEME_INT_VAL(line),
				     SCHEME_FALSEP(col) ? -1 : (SCHEME_INT_VAL(col)+1),
				     SCHEME_FALSEP(pos) ? -1 : SCHEME_INT_VAL(pos),
				     SCHEME_FALSEP(span) ? -1 : SCHEME_INT_VAL(span),
				     src,
				     NULL);
    }
  }

  if (SCHEME_STXP(argv[1]))
    return argv[1];

  src = scheme_datum_to_syntax(argv[1], src, argv[0], 1, 0);

  if (properties) {
    ((Scheme_Stx *)src)->props = properties;
  }

  if (!SCHEME_FALSEP(argv[0]) && !is_clean(argv[0])) {
    add_taint_to_stx(src, 0);
  }

  return src;
}

Scheme_Object *scheme_checked_syntax_e(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-e", "syntax?", 0, argc, argv);
    
  return scheme_stx_content(argv[0]);
}

static Scheme_Object *syntax_line(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-line", "syntax?", 0, argc, argv);
    
  if (stx->srcloc->line < 0)
    return scheme_false;
  else
    return scheme_make_integer(stx->srcloc->line);
}

static Scheme_Object *syntax_col(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-column", "syntax?", 0, argc, argv);
    
  if (stx->srcloc->col < 0)
    return scheme_false;
  else
    return scheme_make_integer(stx->srcloc->col-1);
}

static Scheme_Object *syntax_pos(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-position", "syntax?", 0, argc, argv);
    
  if (stx->srcloc->pos < 0)
    return scheme_false;
  else
    return scheme_make_integer(stx->srcloc->pos);
}

static Scheme_Object *syntax_span(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-span", "syntax?", 0, argc, argv);
    
  if (stx->srcloc->span < 0)
    return scheme_false;
  else
    return scheme_make_integer(stx->srcloc->span);
}

static Scheme_Object *syntax_src(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-source", "syntax?", 0, argc, argv);

  return stx->srcloc->src;
}

static Scheme_Object *syntax_to_list(int argc, Scheme_Object **argv)
{
  Scheme_Object *l;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax->list", "syntax?", 0, argc, argv);

  l = scheme_stx_content(argv[0]);
  if (SCHEME_NULLP(l))
    return scheme_null;
  else if (SCHEME_PAIRP(l)) {
    int islist;
    l = scheme_flatten_syntax_list(l, &islist);
    if (islist)
      return l;
    else
      return scheme_false;
  } else
    return scheme_false;
}

static Scheme_Object *syntax_tainted_p(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-tainted?", "syntax?", 0, argc, argv);

  return (scheme_stx_is_tainted(argv[0])
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *syntax_original_p(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx;
  WRAP_POS awl;
  WRAP_POS ewl;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-original?", "syntax?", 0, argc, argv);

  stx = (Scheme_Stx *)argv[0];

  if (stx->props) {
    if (SAME_OBJ(stx->props, STX_SRCTAG)) {
      /* Check for marks... */
    } else {
      Scheme_Object *e;

      for (e = stx->props; SCHEME_PAIRP(e); e = SCHEME_CDR(e)) {
	if (SAME_OBJ(source_symbol, SCHEME_CAR(SCHEME_CAR(e)))) {
	  break;
	}
      }

      if (SCHEME_NULLP(e))
	return scheme_false;
    }
  } else
    return scheme_false;

  WRAP_POS_INIT(awl, stx->wraps);
  WRAP_POS_INIT_END(ewl);

  if (same_marks(&awl, &ewl, scheme_false))
    return scheme_true;
  else
    return scheme_false;
}

Scheme_Object *scheme_stx_property(Scheme_Object *_stx,
				   Scheme_Object *key,
				   Scheme_Object *val)
{
  Scheme_Stx *stx;
  Scheme_Object *l;

  stx = (Scheme_Stx *)_stx;

  if (stx->props) {
    if (SAME_OBJ(stx->props, STX_SRCTAG)) {
      if (val)
	l = CONS(CONS(source_symbol, scheme_true),
		 scheme_null);
      else
	l = NULL;
    } else {
      Scheme_Object *e;

      for (e = stx->props; SCHEME_PAIRP(e); e = SCHEME_CDR(e)) {
	if (SAME_OBJ(key, SCHEME_CAR(SCHEME_CAR(e)))) {
	  if (val)
	    break;
	  else
	    return SCHEME_CDR(SCHEME_CAR(e));
	}
      }

      if (SCHEME_NULLP(e))
	l = stx->props;
      else {
	/* Remove existing binding: */
	Scheme_Object *first = scheme_null, *last = NULL, *p;

	for (e = stx->props; SCHEME_PAIRP(e); e = SCHEME_CDR(e)) {
	  if (SAME_OBJ(key, SCHEME_CAR(SCHEME_CAR(e)))) {
	    p = SCHEME_CDR(e);
	    e = NULL;
	  } else {
	    p = CONS(SCHEME_CAR(e), scheme_null);
	  }

	  if (last)
	    SCHEME_CDR(last) = p;
	  else
	    first = p;
	  last = p;

	  if (!e)
	    break;
	}
	
	l = first;
      }
    }
  } else
    l = scheme_null;

  if (val) {
    l = CONS(CONS(key, val), l);
    stx = (Scheme_Stx *)clone_stx((Scheme_Object *)stx);
    stx->props = l;

    return (Scheme_Object *)stx;
  } else
    return scheme_false;
}

static Scheme_Object *syntax_property(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-property", "syntax?", 0, argc, argv);

  return scheme_stx_property(argv[0],
			     argv[1],
			     (argc > 2) ? argv[2] : NULL);
}

static Scheme_Object *syntax_property_keys(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-property-symbol-keys", "syntax?", 0, argc, argv);

  stx = (Scheme_Stx *)argv[0];

  if (stx->props) {
    if (!SAME_OBJ(stx->props, STX_SRCTAG)) {
      Scheme_Object *e, *k, *l = scheme_null;

      for (e = stx->props; SCHEME_PAIRP(e); e = SCHEME_CDR(e)) {
	k = SCHEME_CAR(SCHEME_CAR(e));
	if (SCHEME_SYMBOLP(k) && !SCHEME_SYM_WEIRDP(k))
	  l = scheme_make_pair(k, l);
      }
      return l;
    }
  }
   
  return scheme_null;
}

#define SCHEME_STX_IDP(o) (SCHEME_STXP(o) && SCHEME_SYMBOLP(SCHEME_STX_VAL(o)))

static Scheme_Object *syntax_track_origin(int argc, Scheme_Object **argv)
{
  Scheme_Object *result, *observer;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-track-origin", "syntax?", 0, argc, argv);
  if (!SCHEME_STXP(argv[1]))
    scheme_wrong_contract("syntax-track-origin", "syntax?", 1, argc, argv);
  if (!SCHEME_STX_IDP(argv[2]))
    scheme_wrong_contract("syntax-track-origin", "identifier?", 2, argc, argv);
  
  result = scheme_stx_track(argv[0], argv[1], argv[2]);
  observer = scheme_get_expand_observe();
  SCHEME_EXPAND_OBSERVE_TRACK_ORIGIN(observer, argv[0], result);
  return result;
}

Scheme_Object *scheme_transfer_srcloc(Scheme_Object *to, Scheme_Object *from)
{
  if (!SAME_OBJ(((Scheme_Stx *)from)->srcloc, empty_srcloc)) {
    to = clone_stx(to);
    ((Scheme_Stx *)to)->srcloc = ((Scheme_Stx *)from)->srcloc;
  }

  return to;
}

static Scheme_Object *delta_introducer(int argc, struct Scheme_Object *argv[], Scheme_Object *p)
{
  Scheme_Object *r, *delta, *taint_p;

  r = argv[0];

  if (!SCHEME_STXP(r))
    scheme_wrong_contract("delta-introducer", "syntax?", 0, argc, argv);

  delta = SCHEME_PRIM_CLOSURE_ELS(p)[0];
  taint_p = SCHEME_PRIM_CLOSURE_ELS(p)[1];

  for(; !SCHEME_NULLP(delta); delta = SCHEME_CDR(delta)) {
    r = scheme_add_remove_mark(r, SCHEME_CAR(delta));
  }

  if (SCHEME_TRUEP(taint_p))
    r = scheme_stx_taint(r);

  return r;
}

static Scheme_Object *extract_phase(const char *who, int pos, int argc, Scheme_Object **argv, 
                                    Scheme_Object *delta, int use_shift)
{
  Scheme_Object *phase;

  if (argc > pos) {
    phase = argv[pos];
    if (!SCHEME_FALSEP(phase)
        && !SCHEME_INTP(phase)
        && !SCHEME_BIGNUMP(phase))
      scheme_wrong_contract(who, "(or/c exact-integer? #f)", pos, argc, argv);
  } else {
    Scheme_Thread *p = scheme_current_thread;
    intptr_t ph;
    ph = (p->current_local_env
          ? p->current_local_env->genv->phase
          : (use_shift
             ? p->current_phase_shift
             : 0));
    phase = scheme_make_integer(ph);
    
    if (SCHEME_FALSEP(delta) || SCHEME_FALSEP(phase))
      phase = scheme_false;
    else
      phase = scheme_bin_plus(delta, phase);
  }

  return phase;
}

Scheme_Object *scheme_syntax_make_transfer_intro(int argc, Scheme_Object **argv)
{
  Scheme_Object *orig_m1, *m1, *m2, *delta, *a[2];
  int l1, l2;
  Scheme_Object *phase;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(argv[0])))
    scheme_wrong_contract("make-syntax-delta-introducer", "identifier?", 0, argc, argv);
  if (!SCHEME_STXP(argv[1]) && !SCHEME_FALSEP(argv[1]))
    scheme_wrong_contract("make-syntax-delta-introducer", "(or/c syntax? #f)", 1, argc, argv);

  phase = extract_phase("make-syntax-delta-introducer", 2, argc, argv, scheme_make_integer(0), 1);

  m1 = scheme_stx_extract_marks(argv[0]);
  orig_m1 = m1;
  l1 = scheme_list_length(m1);
  delta = scheme_null;
  if (SCHEME_FALSEP(argv[1])) {
    m2 = scheme_false;
  } else {
    m2 = scheme_stx_extract_marks(argv[1]);

    l2 = scheme_list_length(m2);

    while (l1 > l2) {
      delta = CONS(SCHEME_CAR(m1), delta);
      m1 = SCHEME_CDR(m1);
      l1--;
    }
  }

  if (!scheme_equal(m1, m2)) {
    /* tails don't match, so keep all marks --- except 
       those that determine a module binding */
    int skipped = -1;
    Scheme_Object *mod;

    mod = resolve_env(argv[0], phase, 1, NULL, NULL, &skipped, NULL, 0, 
                      scheme_make_hash_table(SCHEME_hash_ptr));

    if ((skipped == -1) && SCHEME_FALSEP(mod)) {
      /* For top-level bindings, need to check the current environment's table,
         because the identifier might not have the top level in its renamings. */
      Scheme_Env *env;

      if (scheme_current_thread->current_local_env)
        env = scheme_current_thread->current_local_env->genv;
      else
        env = NULL;
      if (!env) env = scheme_get_env(NULL);
      if (env) {
        scheme_tl_id_sym(env, argv[0], NULL, 0, NULL, &skipped);
      }
    }

    if (skipped > -1) {
      /* Just keep the first `skipped' marks. */
      delta = scheme_null;
      m1 = orig_m1;
      while (skipped) {
        delta = CONS(SCHEME_CAR(m1), delta);
        m1 = SCHEME_CDR(m1);
        skipped--;
      }
    } else {
      /* Keep them all */
      while (l1) {
        delta = CONS(SCHEME_CAR(m1), delta);
        m1 = SCHEME_CDR(m1);
        l1--;
      }
    }
  }

  a[0] = delta;
  if (scheme_stx_is_clean(argv[0]))
    a[1] = scheme_false;
  else
    a[1] = scheme_true;

  return scheme_make_prim_closure_w_arity(delta_introducer, 2, a, "delta-introducer", 1, 1);
}

static Scheme_Object *bound_eq(int argc, Scheme_Object **argv)
{
  Scheme_Object *phase;

  if (!SCHEME_STX_IDP(argv[0]))
    scheme_wrong_contract("bound-identifier=?", "identifier?", 0, argc, argv);
  if (!SCHEME_STX_IDP(argv[1]))
    scheme_wrong_contract("bound-identifier=?", "identifier?", 1, argc, argv);

  phase = extract_phase("bound-identifier=?", 2, argc, argv, scheme_make_integer(0), 0);

  return (scheme_stx_env_bound_eq2(argv[0], argv[1], NULL, phase, phase)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *do_module_eq(const char *who, int delta, int argc, Scheme_Object **argv)
{
  Scheme_Object *phase, *phase2;

  if (!SCHEME_STX_IDP(argv[0]))
    scheme_wrong_contract(who, "identifier?", 0, argc, argv);
  if (!SCHEME_STX_IDP(argv[1]))
    scheme_wrong_contract(who, "identifier?", 1, argc, argv);

  phase = extract_phase(who, 2, argc, argv, 
                        ((delta == MZ_LABEL_PHASE) 
                         ? scheme_false 
                         : scheme_make_integer(delta)),
                        0);
  if (argc > 3)
    phase2 = extract_phase(who, 3, argc, argv, phase, 0);
  else
    phase2 = phase;

  return (scheme_stx_module_eq3(argv[0], argv[1], phase, phase2, NULL)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *module_eq(int argc, Scheme_Object **argv)
{
  return do_module_eq("free-identifier=?", 0, argc, argv);
}

static Scheme_Object *module_trans_eq(int argc, Scheme_Object **argv)
{
  return do_module_eq("free-transformer-identifier=?", 1, argc, argv);
}

static Scheme_Object *module_templ_eq(int argc, Scheme_Object **argv)
{
  return do_module_eq("free-template-identifier=?", -1, argc, argv);
}

static Scheme_Object *module_label_eq(int argc, Scheme_Object **argv)
{
  return do_module_eq("free-label-identifier=?", MZ_LABEL_PHASE, argc, argv);
}

static Scheme_Object *do_module_binding(char *name, int argc, Scheme_Object **argv, 
                                        Scheme_Object *dphase, int get_symbol)
{
  Scheme_Object *a, *m, *nom_mod, *nom_a, *phase;
  Scheme_Object *src_phase_index, *mod_phase, *nominal_src_phase;

  a = argv[0];

  if (!SCHEME_STXP(a) || !SCHEME_STX_SYMBOLP(a))
    scheme_wrong_contract(name, "identifier?", 0, argc, argv);

  phase = extract_phase(name, 1, argc, argv, dphase, 1);

  if (argc > 1) {
    phase = argv[1];
    if (!SCHEME_FALSEP(phase)
        && !SCHEME_INTP(phase)
        && !SCHEME_BIGNUMP(phase))
      scheme_wrong_contract(name, "(or/c exact-integer? #f)", 1, argc, argv);
  } else {
    Scheme_Thread *p = scheme_current_thread;
    phase = scheme_make_integer(p->current_local_env
                                ? p->current_local_env->genv->phase
                                : p->current_phase_shift);
    if (SCHEME_FALSEP(dphase) || SCHEME_FALSEP(phase))
      phase = scheme_false;
    else
      phase = scheme_bin_plus(dphase, phase);
  }

  m = scheme_stx_module_name(scheme_make_hash_table(SCHEME_hash_ptr),
                             &a, 
                             phase,
			     &nom_mod, &nom_a,
			     &mod_phase,
                             &src_phase_index,
                             &nominal_src_phase,
                             NULL,
                             NULL,
                             NULL,
                             NULL);

  if (get_symbol) {
    if ((!m || SAME_OBJ(m, scheme_undefined)) && nom_a) 
      a = nom_a;
    if (SCHEME_STXP(a))
      a = SCHEME_STX_VAL(a);
    return a;
  }

  if (!m)
    return scheme_false;
  else if (SAME_OBJ(m, scheme_undefined)) {
    return lexical_symbol;
  } else
    return CONS(m, CONS(a, CONS(nom_mod, 
                                CONS(nom_a, 
                                     CONS(mod_phase,
                                          CONS(src_phase_index, 
                                               CONS(nominal_src_phase,
                                                    scheme_null)))))));
}

static Scheme_Object *module_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-binding", argc, argv, scheme_make_integer(0), 0);
}

static Scheme_Object *module_trans_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-transformer-binding", argc, argv, scheme_make_integer(1), 0);
}

static Scheme_Object *module_templ_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-template-binding", argc, argv, scheme_make_integer(-1), 0);
}

static Scheme_Object *module_label_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-label-binding", argc, argv, scheme_false, 0);
}

static Scheme_Object *module_binding_symbol(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-binding-symbol", argc, argv, scheme_make_integer(0), 1);
}

static Scheme_Object *identifier_prune(int argc, Scheme_Object **argv)
{
  Scheme_Object *a = argv[0], *p, *l;

  if (!SCHEME_STXP(a) || !SCHEME_STX_SYMBOLP(a))
    scheme_wrong_contract("identifier-prune-lexical-context", "identifier?", 0, argc, argv);

  if (argc > 1) {
    l = argv[1];
    while (SCHEME_PAIRP(l)) {
      if (!SCHEME_SYMBOLP(SCHEME_CAR(l)))
        break;
      l = SCHEME_CDR(l);
    }
    if (!SCHEME_NULLP(l))
      scheme_wrong_contract("identifier-prune-lexical-context", "(listof symbol?)", 1, argc, argv);
    l = argv[1];
  } else {
    l = scheme_make_pair(SCHEME_STX_VAL(a), scheme_null);
  }

  p = make_prune_context(l);

  return scheme_add_rename(a, p);
}

static Scheme_Object *identifier_prune_to_module(int argc, Scheme_Object **argv)
{
  WRAP_POS w;
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];
  Scheme_Object *l = scheme_null;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_STX_SYMBOLP(argv[0]))
    scheme_wrong_contract("identifier-prune-to-source-module", "identifier?", 0, argc, argv);

  /* Keep only redirecting phase shifts */

  WRAP_POS_INIT(w, ((Scheme_Stx *)stx)->wraps);
  while (!WRAP_POS_END_P(w)) {
    if (SCHEME_BOXP(WRAP_POS_FIRST(w))) {
      /* Phase shift: */
      Scheme_Object *vec, *src;

      vec = SCHEME_BOX_VAL(WRAP_POS_FIRST(w));
      
      src = SCHEME_VEC_ELS(vec)[1];

      /* If src is #f, shift is just for phase; no redirection */
      if (!SCHEME_FALSEP(src)) {
        l = scheme_make_pair(WRAP_POS_FIRST(w), l);
      }
    }

    WRAP_POS_INC(w);
  }

  l = scheme_reverse(l);

  stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
  stx->wraps = l;

  return (Scheme_Object *)stx;
}

static Scheme_Object *syntax_src_module(int argc, Scheme_Object **argv)
{
  int source = 0;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-source-module", "syntax?", 0, argc, argv);

  if ((argc > 1) && SCHEME_TRUEP(argv[1]))
    source = 1;

  return scheme_stx_source_module(argv[0], source, source);
}

/**********************************************************************/

static Scheme_Object *syntax_arm(int argc, Scheme_Object **argv)
{
  Scheme_Object *insp;
  int use_mode;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-arm", "syntax?", 0, argc, argv);
  if ((argc > 1) && !SCHEME_FALSEP(argv[1])) {
    if (!SAME_TYPE(SCHEME_TYPE(argv[1]), scheme_inspector_type))
      scheme_wrong_contract("syntax-arm", "(or/c inspector? #f)", 1, argc, argv);
    insp = argv[1];
  } else
    insp = scheme_false;

  use_mode = ((argc > 2) && SCHEME_TRUEP(argv[2]));
    
  return scheme_syntax_taint_arm(argv[0], insp, use_mode);
}

static Scheme_Object *syntax_disarm(int argc, Scheme_Object **argv)
{
  Scheme_Object *insp;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-disarm", "syntax?", 0, argc, argv);
  if (argc > 1) {
    if (SCHEME_TRUEP(argv[1]) && !SAME_TYPE(SCHEME_TYPE(argv[1]), scheme_inspector_type))
      scheme_wrong_contract("syntax-disarm", "(or/c inspector? #f)", 1, argc, argv);
    insp = argv[1];
  } else
    insp = scheme_false;
   
  return scheme_stx_taint_disarm(argv[0], insp);
}

static Scheme_Object *syntax_rearm(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-rearm", "syntax?", 0, argc, argv);
  if (!SCHEME_STXP(argv[1]))
    scheme_wrong_contract("syntax-rearm", "syntax?", 1, argc, argv);

  if ((argc > 2) && SCHEME_TRUEP(argv[2]))
    return scheme_syntax_taint_rearm(argv[0], argv[1]);
  else
    return scheme_stx_taint_rearm(argv[0], argv[1]);
}

static Scheme_Object *syntax_taint(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-taint", "syntax?", 0, argc, argv);

  return add_taint_to_stx(argv[0], 1);
  
}

/**********************************************************************/
/*                             Debugging                              */
/**********************************************************************/

static Scheme_Object *explode_wraps(Scheme_Object *wraps, Scheme_Hash_Table *ht)
{
  Scheme_Object *key, *prev_key = NULL, *pr, *first = scheme_null, *last = NULL, *v;
  WRAP_POS awl;

  WRAP_POS_INIT(awl, wraps);

  while (!WRAP_POS_END_P(awl)) {
    key = WRAP_POS_KEY(awl);
    if (key != prev_key) {
      pr = scheme_hash_get(ht, key);
      if (pr) {
        if (last)
          SCHEME_CDR(last) = pr;
        else
          first = pr;
        break;
      } else {
        pr = scheme_make_pair(scheme_void, scheme_null);
        if (last)
          SCHEME_CDR(last) = pr;
        else
          first = pr;
        last = pr;
        pr = scheme_make_pair(scheme_false, scheme_null);
        scheme_hash_set(ht, key, pr);
      }
      prev_key = key;
    } else {
      pr = scheme_make_pair(scheme_false, scheme_null);
    }
    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;

    v = WRAP_POS_FIRST(awl);

    if (SCHEME_RENAMESP(v)) {
      Module_Renames *mrn = (Module_Renames *)v;
      Scheme_Object *o;

      v = scheme_hash_get(ht, (Scheme_Object *)mrn);
      if (!v) {
        v = scheme_make_vector(7, NULL);
        o = scheme_intern_symbol("rename:");
        SCHEME_VEC_ELS(v)[0] = o;
        SCHEME_VEC_ELS(v)[1] = mrn->phase;
        SCHEME_VEC_ELS(v)[2] = (Scheme_Object *)mrn->ht;
        SCHEME_VEC_ELS(v)[3] = (mrn->nomarshal_ht ? (Scheme_Object *)mrn->nomarshal_ht : scheme_false);
        SCHEME_VEC_ELS(v)[4] = scheme_true; /* mrn->shared_pes; */
        SCHEME_VEC_ELS(v)[5] = (mrn->marked_names ? (Scheme_Object *)mrn->marked_names : scheme_false);
        SCHEME_VEC_ELS(v)[6] = (Scheme_Object *)mrn->unmarshal_info;
        scheme_hash_set(ht, (Scheme_Object *)mrn, v);
      }
    }

    SCHEME_CAR(pr) = v;
    
    WRAP_POS_INC(awl);
  }

  return first;
}

Scheme_Object *scheme_explode_syntax(Scheme_Object *stx, Scheme_Hash_Table *ht)
{
  Scheme_Object *vec, *v;

  if (SCHEME_PAIRP(stx)) {
    return scheme_make_pair(scheme_explode_syntax(SCHEME_CAR(stx), ht),
                            scheme_explode_syntax(SCHEME_CDR(stx), ht));
  }
  if (SCHEME_NULLP(stx))
    return scheme_null;

  vec = scheme_hash_get(ht, stx);
  if (vec)
    return vec;

  vec = scheme_make_vector(3, NULL);
  scheme_hash_set(ht, stx, vec);

  v = ((Scheme_Stx *)stx)->val;
  if (SCHEME_PAIRP(v)) {
    v = scheme_make_pair(scheme_explode_syntax(SCHEME_CAR(v), ht),
                         scheme_explode_syntax(SCHEME_CDR(v), ht));
  }
  SCHEME_VEC_ELS(vec)[0] = v;

  v = ((Scheme_Stx *)stx)->taints;
  SCHEME_VEC_ELS(vec)[1] = (v ? v : scheme_null);
  v = explode_wraps(((Scheme_Stx *)stx)->wraps, ht);
  SCHEME_VEC_ELS(vec)[2] = v;

  return vec;
}

/**********************************************************************/

static Scheme_Object *write_free_id_info_prefix(Scheme_Object *obj)
{
  Scheme_Object *vec;
  int i;

  vec = scheme_make_vector(8, NULL);
  for (i = 0; i < 8; i++) {
    SCHEME_VEC_ELS(vec)[i] = SCHEME_VEC_ELS(obj)[i];
  }
  if (SCHEME_TRUEP(SCHEME_VEC_ELS(vec)[7]))
    SCHEME_VEC_ELS(vec)[7] = scheme_true;

  return vec;
}

static Scheme_Object *read_free_id_info_prefix(Scheme_Object *obj)
{
  Scheme_Object *vec;
  int i;

  if (!SCHEME_VECTORP(obj)
      || (SCHEME_VEC_SIZE(obj) != 8))
    return NULL;

  vec = scheme_make_vector(8, NULL);
  for (i = 0; i < 8; i++) {
    SCHEME_VEC_ELS(vec)[i] = SCHEME_VEC_ELS(obj)[i];
  }

  SCHEME_VEC_ELS(vec)[7] = scheme_false;

  vec->type = scheme_free_id_info_type;
    
  return vec;
}

/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_syntax.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rename_table_type, mark_rename_table);
  GC_REG_TRAV(scheme_rename_table_set_type, mark_rename_table_set);
  GC_REG_TRAV(scheme_rt_srcloc, mark_srcloc);
  GC_REG_TRAV(scheme_wrap_chunk_type, mark_wrapchunk);
  GC_REG_TRAV(scheme_lexical_rib_type, lex_rib);
}

END_XFORM_SKIP;

#endif
