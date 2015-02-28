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

ROSYM static Scheme_Object *source_symbol; /* uninterned! */
ROSYM static Scheme_Object *share_symbol; /* uninterned! */
ROSYM static Scheme_Object *origin_symbol;
ROSYM static Scheme_Object *lexical_symbol;
ROSYM static Scheme_Object *protected_symbol;
ROSYM static Scheme_Object *nominal_id_symbol;

READ_ONLY Scheme_Object *scheme_syntax_p_proc;

READ_ONLY Scheme_Hash_Tree *empty_hash_tree;
READ_ONLY Scheme_Mark_Table *empty_mark_table;
READ_ONLY Scheme_Mark_Table *empty_propagate_table;
READ_ONLY Scheme_Mark_Set *empty_mark_set;

READ_ONLY static Scheme_Stx_Srcloc *empty_srcloc;

typedef struct Scheme_Mark {
  Scheme_Inclhash_Object iso; /* includes SCHEME_MARK_NONORIGINAL flag */
  mzlonglong id;
  Scheme_Object *bindings; /* list or marshaled info */
  intptr_t timestamp;
  int kind; // REMOVEME
  Scheme_Object *owner_multi_mark; /* (cons <multi-mark> <phase>) */
} Scheme_Mark;

#define SCHEME_MARK_FLAGS(mark) MZ_OPT_HASH_KEY(&(mark)->iso)
#define SCHEME_MARK_NONORIGINAL    0x1
#define SCHEME_MARK_MODULE_CONTEXT 0x2

typedef struct Scheme_Propagate_Table {
  Scheme_Mark_Table mt;
  Scheme_Mark_Table *prev; /* points to old mark table */
  Scheme_Object *phase_shift; /* number of (box <n>); latter converts only <n> to #f */
} Scheme_Propagate_Table;

THREAD_LOCAL_DECL(static mzlonglong mark_counter);
THREAD_LOCAL_DECL(static Scheme_Object *last_phase_shift);
THREAD_LOCAL_DECL(static Scheme_Object *nominal_ipair_cache);
THREAD_LOCAL_DECL(static Scheme_Bucket_Table *taint_intern_table);
THREAD_LOCAL_DECL(static intptr_t binding_cache_timestamp);

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

static Scheme_Object *set_false_insp(Scheme_Object *o, Scheme_Object *false_insp, int need_clone);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

XFORM_NONGCING static int is_armed(Scheme_Object *v);
static Scheme_Object *add_taint_to_stx(Scheme_Object *o, int need_clone);
static void unmarshal_module_context_additions(Scheme_Stx *stx, Scheme_Object *vec, Scheme_Mark_Set *marks, Scheme_Object *replace_at);

XFORM_NONGCING static int marks_equal(Scheme_Mark_Set *a, Scheme_Mark_Set *b);
static Scheme_Object *remove_at(Scheme_Object *l, Scheme_Object *p);

static Scheme_Object *mark_unmarshal_content(Scheme_Object *c, struct Scheme_Unmarshal_Tables *utx);

#define CONS scheme_make_pair
#define ICONS scheme_make_pair

#define HAS_SUBSTX(obj) (SCHEME_PAIRP(obj) || SCHEME_VECTORP(obj) || SCHEME_BOXP(obj) || prefab_p(obj) || SCHEME_HASHTRP(obj))
#define HAS_CHAPERONE_SUBSTX(obj) (HAS_SUBSTX(obj) || (SCHEME_NP_CHAPERONEP(obj) && HAS_SUBSTX(SCHEME_CHAPERONE_VAL(obj))))

#define SCHEME_INSPECTORP(obj) SAME_TYPE(scheme_inspector_type, SCHEME_TYPE(obj))
#define SCHEME_MODIDXP(l) SAME_TYPE(SCHEME_TYPE(l), scheme_module_index_type)
#define SCHEME_PHASEP(a) (SCHEME_INTP(a) || SCHEME_BIGNUMP(a) || SCHEME_FALSEP(a))

#define SCHEME_PHASE_SHIFTP(a) (SCHEME_PHASEP(a) || (SCHEME_BOXP(a) && SCHEME_PHASEP(SCHEME_BOX_VAL(a))))
/* #f as a phase shift is an alias for (box 0) */

#define SCHEME_MULTI_MARKP(o) SCHEME_HASHTP(o)
#define SCHEME_MARKP(x) (SAME_TYPE(SCHEME_TYPE(x), scheme_mark_type))

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

#ifdef OS_X
# define CHECK_STX_ASSERTS
#endif

#ifdef CHECK_STX_ASSERTS
# include <assert.h>
# define STX_ASSERT(x) assert(x)
#else
# define STX_ASSERT(x) /* empty */
#endif
/*========================================================================*/
/*                           initialization                               */
/*========================================================================*/

void scheme_init_stx(Scheme_Env *env)
{
  Scheme_Object *o;

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  REGISTER_SO(empty_hash_tree);
  REGISTER_SO(empty_mark_table);
  REGISTER_SO(empty_propagate_table);
  REGISTER_SO(empty_mark_set);
  empty_hash_tree =scheme_make_hash_tree(0);
  empty_mark_set = (Scheme_Mark_Set *)empty_hash_tree;
  empty_mark_table = MALLOC_ONE_TAGGED(Scheme_Mark_Table);
  empty_mark_table->so.type = scheme_mark_table_type;
  empty_mark_table->single_marks = empty_mark_set;
  empty_mark_table->multi_marks = scheme_null;
  empty_propagate_table = (Scheme_Mark_Table *)MALLOC_ONE_TAGGED(Scheme_Propagate_Table);
  memcpy(empty_propagate_table, empty_mark_table, sizeof(Scheme_Mark_Table));
  empty_propagate_table->so.type = scheme_propagate_table_type;
  ((Scheme_Propagate_Table *)empty_propagate_table)->phase_shift = scheme_make_integer(0);
  ((Scheme_Propagate_Table *)empty_propagate_table)->prev = NULL;

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

  REGISTER_SO(empty_srcloc);
  empty_srcloc = MALLOC_ONE_RT(Scheme_Stx_Srcloc);
#ifdef MZTAG_REQUIRED
  empty_srcloc->type = scheme_rt_srcloc;
#endif
  empty_srcloc->src = scheme_false;
  empty_srcloc->line = -1;
  empty_srcloc->col = -1;
  empty_srcloc->pos = -1;
}

void scheme_init_stx_places(int initial_main_os_thread) {
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
  stx->marks = empty_mark_table;
  stx->u.to_propagate = NULL;
  stx->shifts = scheme_null;
  stx->props = props;

  return (Scheme_Object *)stx;
}

Scheme_Object *clone_stx(Scheme_Object *to)
{
  Scheme_Stx *stx = (Scheme_Stx *)to;
  Scheme_Object *taints, *shifts;
  Scheme_Mark_Table *marks;
  Scheme_Mark_Table *to_propagate;

  taints = stx->taints;
  marks = stx->marks;
  shifts = stx->shifts;
  to_propagate = stx->u.to_propagate;

  stx = (Scheme_Stx *)scheme_make_stx(stx->val, 
                                      stx->srcloc,
                                      stx->props);

  stx->marks = marks;
  if (STX_KEY(stx) & STX_SUBSTX_FLAG)
    stx->u.to_propagate = to_propagate;
  stx->taints = taints;
  stx->shifts = shifts;

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

void scheme_stx_set(Scheme_Object *q_stx, Scheme_Object *val, Scheme_Object *context)
{
  ((Scheme_Stx *)q_stx)->val = val;

  if (context) {
    ((Scheme_Stx *)q_stx)->marks = ((Scheme_Stx *)context)->marks;
    ((Scheme_Stx *)q_stx)->shifts = ((Scheme_Stx *)context)->shifts;
  } else {
    ((Scheme_Stx *)q_stx)->marks = NULL;
    ((Scheme_Stx *)q_stx)->shifts = NULL;
  }

  ((Scheme_Stx *)q_stx)->u.to_propagate = NULL;
  ((Scheme_Stx *)q_stx)->taints = NULL;
}

/******************** marks ********************/

Scheme_Object *scheme_new_mark(int kind)
{
  Scheme_Mark *m;
  mzlonglong id;

  m = (Scheme_Mark *)scheme_malloc_small_tagged(sizeof(Scheme_Mark));
  m->iso.so.type = scheme_mark_type;
  id = ++mark_counter;
  m->id = id;
  m->kind = kind;

  return (Scheme_Object *)m;
}

Scheme_Object *scheme_new_nonoriginal_mark(int kind)
{
  Scheme_Object *m;

  m = scheme_new_mark(kind);

  SCHEME_MARK_FLAGS((Scheme_Mark*)m) |= SCHEME_MARK_NONORIGINAL;

  return m;
}

static Scheme_Object *new_module_context_mark(int kind)
{
  Scheme_Object *m;

  m = scheme_new_mark(kind);

  SCHEME_MARK_FLAGS((Scheme_Mark*)m) |= SCHEME_MARK_MODULE_CONTEXT;

  return m;
}

Scheme_Object *scheme_new_multi_mark()
{
  /* Maps a phase to a mark, where each mark is created on demand: */
  return (Scheme_Object *)scheme_make_hash_table_eqv();
}

Scheme_Object *scheme_mark_printed_form(Scheme_Object *m)
{
#if 0
  return scheme_make_integer_value_from_long_long(((Scheme_Mark *)m)->id);
#else
  // REMOVEME
  return scheme_make_pair(scheme_make_integer_value_from_long_long(((Scheme_Mark *)m)->id),
                          scheme_make_integer(((Scheme_Mark *)m)->kind));
#endif
}

#define SCHEME_MARK_SETP(m) SCHEME_HASHTRP((Scheme_Object *)(m))

XFORM_NONGCING static intptr_t mark_set_count(Scheme_Mark_Set *s)
{
  return ((Scheme_Hash_Tree *)s)->count;
}

XFORM_NONGCING static Scheme_Object *mark_set_get(Scheme_Mark_Set *s, Scheme_Object *key)
{
  return scheme_eq_hash_tree_get((Scheme_Hash_Tree *)s, key);
}

static Scheme_Mark_Set *mark_set_set(Scheme_Mark_Set *s, Scheme_Object *key, Scheme_Object *val)
{
  return (Scheme_Mark_Set *)scheme_hash_tree_set((Scheme_Hash_Tree *)s, key, val);
}

XFORM_NONGCING static mzlonglong mark_set_next(Scheme_Mark_Set *s, mzlonglong pos)
{
  return scheme_hash_tree_next((Scheme_Hash_Tree *)s, pos);
}

XFORM_NONGCING static int mark_set_index(Scheme_Mark_Set *s, mzlonglong pos, Scheme_Object **_key, Scheme_Object **_val)
{
  return scheme_hash_tree_index((Scheme_Hash_Tree *)s, pos, _key, _val);
}

XFORM_NONGCING static int mark_subset(Scheme_Mark_Set *sa, Scheme_Mark_Set *sb)
{
  Scheme_Hash_Tree *a = (Scheme_Hash_Tree *)sa;
  Scheme_Hash_Tree *b = (Scheme_Hash_Tree *)sb;
  intptr_t i;
  Scheme_Object *key, *val;

  if (a->count > b->count)
    return 0;

  i = scheme_hash_tree_next(a, -1);
  while (i != -1) {
    scheme_hash_tree_index(a, i, &key, &val);
    if (!scheme_eq_hash_tree_get(b, key))
      return 0;

    i = scheme_hash_tree_next(a, i);
  }

  return 1;
}

static int marks_equal(Scheme_Mark_Set *a, Scheme_Mark_Set *b)
{
  return (mark_set_count(a) == mark_set_count(b)) && mark_subset(a, b);
}

Scheme_Object *extract_single_mark(Scheme_Object *multi_mark, Scheme_Object *phase)
{
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)multi_mark;
  Scheme_Object *m, *mm;
  
  m = scheme_hash_get(ht, phase);
  if (!m) {
    m = scheme_new_mark(0);
    mm = scheme_make_pair((Scheme_Object *)ht, phase);
    ((Scheme_Mark *)m)->owner_multi_mark = mm;
    scheme_hash_set(ht, phase, m);
  }

  return m;
}

static Scheme_Mark_Set *extract_mark_set(Scheme_Stx *stx, Scheme_Object *phase)
{
  Scheme_Mark_Table *mt = stx->marks;
  Scheme_Mark_Set *marks;
  Scheme_Object *multi_marks, *ph, *m;

  marks = mt->single_marks;

  for (multi_marks = mt->multi_marks;
       !SCHEME_NULLP(multi_marks);
       multi_marks= SCHEME_CDR(multi_marks)) {
    ph = SCHEME_CDR(SCHEME_CAR(multi_marks));
    if (SCHEME_FALSEP(phase)) {
      if (!SCHEME_BOXP(ph)) {
        /* number phase shift, so look for #f */
        ph = scheme_false;
      } else {
        /* phase shift of some <n> to #f, so look for <n> */
        ph = SCHEME_BOX_VAL(ph); 
      }
    } else if (SCHEME_BOXP(ph)) {
      /* we want a number phase, but this is shifted to #f */
      ph = NULL;
    } else
      ph = scheme_bin_minus(phase, ph);

    if (ph) {
      m = extract_single_mark(SCHEME_CAR(SCHEME_CAR(multi_marks)), ph);
      
      marks = mark_set_set(marks, m, scheme_true);
    }
  }

  return marks;
}

static Scheme_Mark_Set *adjust_mark(Scheme_Mark_Set *marks, Scheme_Object *m, int mode)
{
  STX_ASSERT(SAME_TYPE(SCHEME_TYPE(m), scheme_mark_type));

  if (mark_set_get(marks, m)) {
    if ((mode == SCHEME_STX_FLIP) || (mode == SCHEME_STX_REMOVE))
      return mark_set_set(marks, m, NULL);
    else
      return marks;
  } else {
    if (mode == SCHEME_STX_REMOVE)
      return marks;
    else
      return mark_set_set(marks, m, scheme_true);
  }
}

Scheme_Object *adjust_mark_list(Scheme_Object *multi_marks, Scheme_Object *m, Scheme_Object *phase, int mode)
{
  Scheme_Object *l;

  for (l = multi_marks; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (SAME_OBJ(m, SCHEME_CAR(SCHEME_CAR(l)))
        && SAME_OBJ(phase, SCHEME_CDR(SCHEME_CAR(l)))) {
      if (mode == SCHEME_STX_ADD)
        return multi_marks;
      break;
    }
  }

  if ((mode == SCHEME_STX_REMOVE) && SCHEME_NULLP(l))
    return multi_marks;
  else if ((mode == SCHEME_STX_REMOVE) 
           || ((mode == SCHEME_STX_FLIP && !SCHEME_NULLP(l)))) {
    return remove_at(multi_marks, l);
  } else
    return scheme_make_pair(scheme_make_pair(m, phase), multi_marks);
}

static Scheme_Mark_Set *combine_mark(Scheme_Mark_Set *marks, Scheme_Object *m, int mode)
{
  Scheme_Object *old_mode;

  STX_ASSERT(SAME_TYPE(SCHEME_TYPE(m), scheme_mark_type));

  old_mode = mark_set_get(marks, m);

  if (old_mode) {
    if (SCHEME_INT_VAL(old_mode) == mode) {
      if (mode == SCHEME_STX_FLIP)
        return mark_set_set(marks, m, NULL);
      else
        return marks;
    } else if (mode == SCHEME_STX_FLIP) {
      mode = SCHEME_INT_VAL(old_mode);
      mode = ((mode == SCHEME_STX_REMOVE) ? SCHEME_STX_ADD : SCHEME_STX_REMOVE);
      return mark_set_set(marks, m, scheme_make_integer(mode));
    } else
      return mark_set_set(marks, m, scheme_make_integer(mode));
  } else
    return mark_set_set(marks, m, scheme_make_integer(mode));
}

Scheme_Object *combine_mark_list(Scheme_Object *multi_marks, Scheme_Object *m, Scheme_Object *phase, int mode)
{
  Scheme_Object *l;

  for (l = multi_marks; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (SAME_OBJ(m, SCHEME_VEC_ELS(SCHEME_CAR(l))[0])
        && SAME_OBJ(phase, SCHEME_VEC_ELS(SCHEME_CAR(l))[1])) {
      int prev_mode = SCHEME_INT_VAL(SCHEME_VEC_ELS(SCHEME_CAR(l))[2]);
      if (mode == SCHEME_STX_FLIP) {
        if (prev_mode == SCHEME_STX_FLIP)
          return remove_at(multi_marks, l);
        else {
          if (prev_mode == SCHEME_STX_ADD)
            mode = SCHEME_STX_REMOVE;
          else
            mode = SCHEME_STX_ADD;
          multi_marks = remove_at(multi_marks, l);
          break;
        }
      } else if (mode != prev_mode) {
        multi_marks = remove_at(multi_marks, l);
        break;
      } else
        return multi_marks;
    }
  }

  l = scheme_make_vector(3, NULL);
  SCHEME_VEC_ELS(l)[0] = m;
  SCHEME_VEC_ELS(l)[1] = phase;
  SCHEME_VEC_ELS(l)[2] = scheme_make_integer(mode);

  return scheme_make_pair(l, multi_marks);
}

static Scheme_Object *remove_at(Scheme_Object *l, Scheme_Object *p)
{
  Scheme_Object *r = SCHEME_CDR(p);

  while (!SAME_OBJ(l, p)) {
    r = scheme_make_pair(SCHEME_CAR(l), r);
    l = SCHEME_CDR(l);
  }

  return r;
}

static Scheme_Mark_Table *clone_mark_table(Scheme_Mark_Table *mt, Scheme_Mark_Table *prev)
/* If prev is non-NULL, then `mt` is a propagate table */
{
  Scheme_Mark_Table *mt2;

  if (!prev) {
    mt2 = MALLOC_ONE_TAGGED(Scheme_Mark_Table);
    memcpy(mt2, mt, sizeof(Scheme_Mark_Table));
  } else {
    mt2 = (Scheme_Mark_Table *)MALLOC_ONE_TAGGED(Scheme_Propagate_Table);
    memcpy(mt2, mt, sizeof(Scheme_Propagate_Table));
    if (SAME_OBJ(mt, empty_propagate_table))
      ((Scheme_Propagate_Table *)mt2)->prev = prev;
  }

  return mt2;
}

typedef Scheme_Mark_Set *(*do_mark_t)(Scheme_Mark_Set *marks, Scheme_Object *m, int mode);
typedef Scheme_Object *(do_mark_list_t)(Scheme_Object *multi_marks, Scheme_Object *m, Scheme_Object *phase, int mode);

static Scheme_Mark_Table *do_mark_at_phase(Scheme_Mark_Table *mt, Scheme_Object *m, Scheme_Object *phase, int mode, 
                                           do_mark_t do_mark, do_mark_list_t do_mark_list, Scheme_Mark_Table *prev)
{
  Scheme_Object *l;
  Scheme_Mark_Set *marks;

  if (SCHEME_MARKP(m) && ((Scheme_Mark *)m)->owner_multi_mark) {
    if (!SCHEME_FALSEP(phase))
      phase = scheme_bin_minus(SCHEME_CDR(((Scheme_Mark *)m)->owner_multi_mark),
                               phase);
    m = SCHEME_CAR(((Scheme_Mark *)m)->owner_multi_mark);
  }

  if (SCHEME_MULTI_MARKP(m)) {
    l = do_mark_list(mt->multi_marks, m, phase, mode);
    if (SAME_OBJ(l, mt->multi_marks))
      return mt;
    mt = clone_mark_table(mt, prev);
    mt->multi_marks = l;
    return mt;
  } else {
    marks = do_mark(mt->single_marks, m, mode);
    if (SAME_OBJ(marks, mt->single_marks))
      return mt;
    mt = clone_mark_table(mt, prev);
    mt->single_marks = marks;
    return mt;
  }
}


int props_zeroed, props_oned;

Scheme_Object *scheme_stx_adjust_mark(Scheme_Object *o, Scheme_Object *m, Scheme_Object *phase, int mode)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Mark_Table *marks;
  Scheme_Mark_Table *to_propagate;
  Scheme_Object *taints, *shifts;
  int mutate;

  STX_ASSERT(SCHEME_STXP(o));

  if (mode & SCHEME_STX_MUTATE) {
    mode -= SCHEME_STX_MUTATE;
    mutate = 1;
  } else
    mutate = 0;

  if (mode & SCHEME_STX_PROPONLY) {
    marks = stx->marks;
    mode -= SCHEME_STX_PROPONLY;
  } else {
    marks = do_mark_at_phase(stx->marks, m, phase, mode, adjust_mark, adjust_mark_list, NULL);
    if ((stx->marks == marks)
        && !(STX_KEY(stx) & STX_SUBSTX_FLAG)) {
      return (Scheme_Object *)stx;
    }
  }

  if (STX_KEY(stx) & STX_SUBSTX_FLAG) {
    to_propagate = (stx->u.to_propagate ? stx->u.to_propagate : empty_propagate_table);
    to_propagate = do_mark_at_phase(to_propagate, m, phase, mode, combine_mark, combine_mark_list, stx->marks);
    if ((stx->u.to_propagate == to_propagate)
        && (stx->marks == marks))
      return (Scheme_Object *)stx;
  } else
    to_propagate = NULL; /* => cleared binding cache */

  if (mutate) {
    stx->marks = marks;
    stx->u.to_propagate = to_propagate;
  } else {
    taints = stx->taints;
    shifts = stx->shifts;
    stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
    stx->marks = marks;
    stx->u.to_propagate = to_propagate;
    stx->taints = taints;
    stx->shifts = shifts;
  }

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_stx_add_mark(Scheme_Object *o, Scheme_Object *m, Scheme_Object *phase)
{
  return scheme_stx_adjust_mark(o, m, phase, SCHEME_STX_ADD);
}

Scheme_Object *scheme_stx_remove_mark(Scheme_Object *o, Scheme_Object *m, Scheme_Object *phase)
{
  return scheme_stx_adjust_mark(o, m, phase, SCHEME_STX_REMOVE);
}

Scheme_Object *scheme_stx_flip_mark(Scheme_Object *o, Scheme_Object *m, Scheme_Object *phase)
{
  return scheme_stx_adjust_mark(o, m, phase, SCHEME_STX_FLIP);
}

Scheme_Object *scheme_stx_adjust_marks(Scheme_Object *o, Scheme_Mark_Set *marks, Scheme_Object *phase, int mode)
{
  Scheme_Object *key, *val;
  intptr_t i;
  int flag = 0;

  i = mark_set_next(marks, -1);
  while (i != -1) {
    mark_set_index(marks, i, &key, &val);

    val = scheme_stx_adjust_mark(o, key, phase, mode | flag);
    if (!SAME_OBJ(val, o))
      flag = SCHEME_STX_MUTATE;
    o = val;
    
    i = mark_set_next(marks, i);
  }

  return o;
}

Scheme_Object *scheme_stx_adjust_frame_marks(Scheme_Object *o, Scheme_Object *mark, Scheme_Object *phase, int mode)
{
  if (SCHEME_PAIRP(mark)) {
    if (!SCHEME_FALSEP(SCHEME_CDR(mark)))
      o = scheme_stx_adjust_mark(o, SCHEME_CDR(mark), phase, mode);
  }

  return scheme_stx_adjust_frame_bind_marks(o, mark, phase, mode);
}

Scheme_Object *scheme_stx_adjust_frame_bind_marks(Scheme_Object *o, Scheme_Object *mark, Scheme_Object *phase, int mode)
{
  if (SCHEME_PAIRP(mark))
    mark = SCHEME_CAR(mark);

  if (SCHEME_FALSEP(mark))
    return o;
  else if (SCHEME_MARKP(mark) || SCHEME_MULTI_MARKP(mark))
    return scheme_stx_adjust_mark(o, mark, phase, mode);
  else
    return scheme_stx_adjust_marks(o, (Scheme_Mark_Set *)mark, phase, mode);
}

Scheme_Object *scheme_stx_adjust_frame_expression_marks(Scheme_Object *o, Scheme_Object *mark, Scheme_Object *phase, int mode)
{
  if (SCHEME_PAIRP(mark)) {
    mark = SCHEME_CDR(mark);
    return scheme_stx_adjust_mark(o, mark, phase, mode);
  }

  return o;
}

Scheme_Object *scheme_make_frame_marks(Scheme_Object *mark, Scheme_Object *expr_mark)
{
  return scheme_make_pair(mark, expr_mark);
}

Scheme_Object *scheme_stx_remove_module_binding_marks(Scheme_Object *o)
{
  Scheme_Object *l = ((Scheme_Stx *)o)->marks->multi_marks;

  while (!SCHEME_NULLP(l)) {
    o = scheme_stx_remove_mark(o,
                               extract_single_mark(SCHEME_CAR(SCHEME_CAR(l)),
                                                   scheme_make_integer(0)),
                               scheme_make_integer(0));
    l = SCHEME_CDR(l);
  }

  return o;
}

Scheme_Object *scheme_stx_remove_module_context_marks(Scheme_Object *o)
{
  Scheme_Mark_Set *marks = ((Scheme_Stx *)o)->marks->single_marks;
  intptr_t i;
  Scheme_Object *key, *val;

  i = mark_set_next(marks, -1);
  while (i != -1) {
    mark_set_index(marks, i, &key, &val);

    if (SCHEME_MARK_FLAGS((Scheme_Mark *)key) & SCHEME_MARK_MODULE_CONTEXT)
      o = scheme_stx_remove_mark(o, key, scheme_make_integer(0));

    i = mark_set_next(marks, i);
  }

  return scheme_stx_remove_module_binding_marks(o);
}

int scheme_stx_has_empty_wraps(Scheme_Object *stx, Scheme_Object *phase)
{
  return (mark_set_count(extract_mark_set((Scheme_Stx *)stx, phase)) == 0);
}

/******************** shifts ********************/

XFORM_NONGCING static int same_phase(Scheme_Object *a, Scheme_Object *b)
{
  return ((SAME_OBJ(a, b) || scheme_eqv(a, b))
          ? 1
          : 0);
}

static Scheme_Object *add_shifts(Scheme_Object *old_shift, Scheme_Object *shift)
/* The new `shift` is allowed to be #f, but `old_shift` and the result are
   normalized to `(box 0)` */
{
  if (SCHEME_BOXP(shift) && SCHEME_FALSEP(SCHEME_BOX_VAL(shift))) {
    /* (box #f) is an impossible shift, so discard */
    return NULL;
  }

  if ((SCHEME_FALSEP(shift) || SCHEME_BOXP(shift))
      && SCHEME_BOXP(old_shift)) {
    /* shifting some numbered phase when already shifted to #f; discard */
    return NULL;
  }

  if (SCHEME_BOXP(old_shift)) {
    /* numbered shift on already shifted to #f => no change */
    return old_shift;
  }

  if (SCHEME_FALSEP(shift)) {
    /* shift of <n> before shifting 0 to #f => shift -<n> to #f */
    return scheme_box(scheme_bin_minus(scheme_make_integer(0), old_shift));
  } else if (SCHEME_BOXP(shift)) {
    /* shift of <n> before shifting <m> to #f => shift <m>-<n> to #f */
    if (SAME_OBJ(old_shift, scheme_make_integer(0)))
      return shift;
    else
      return scheme_box(scheme_bin_minus(SCHEME_BOX_VAL(shift), old_shift));
  } else
    return scheme_bin_plus(old_shift, shift);
}

static Scheme_Object *shift_multi_mark(Scheme_Object *p, Scheme_Object *shift)
{
  shift = add_shifts(SCHEME_CDR(p), shift);

  if (!shift)
    return NULL;

  if (SAME_OBJ(shift, SCHEME_CDR(p)))
    return p;

  return scheme_make_pair(SCHEME_CAR(p), shift);
}

static Scheme_Object *shift_prop_multi_mark(Scheme_Object *p, Scheme_Object *shift)
{
  Scheme_Object *p2;
  
  shift = add_shifts(SCHEME_VEC_ELS(p)[1], shift);
  if (!shift)
    return NULL;

  if (SAME_OBJ(shift, SCHEME_VEC_ELS(p)[1]))
    return p;

  p2 = scheme_make_vector(3, NULL);
  SCHEME_VEC_ELS(p2)[0] = SCHEME_VEC_ELS(p)[0];
  SCHEME_VEC_ELS(p2)[1] = shift;
  SCHEME_VEC_ELS(p2)[2] = SCHEME_VEC_ELS(p)[2];
  
  return p2;
}

typedef Scheme_Object *(shift_multi_mark_t)(Scheme_Object *p, Scheme_Object *shift);

static Scheme_Mark_Table *shift_mark_table(Scheme_Mark_Table *mt, Scheme_Object *shift, 
                                           shift_multi_mark_t shift_mm, Scheme_Mark_Table *prev)
{
  Scheme_Mark_Table *mt2;
  Scheme_Object *l, *key, *val;

  if (SAME_OBJ(mt, empty_mark_table)) {
    STX_ASSERT(!prev);
    return mt;
  }

  if (SCHEME_NULLP(mt->multi_marks) && !prev)
    return mt;

  mt2 = clone_mark_table(mt, prev);

  val = scheme_null;
  for (l = mt->multi_marks; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    key = shift_mm(SCHEME_CAR(l), shift);
    if (key)
      val = scheme_make_pair(key, val);
  }
  mt2->multi_marks = val;

  if (prev) {
    /* record accumulated shift for propagation */
    shift = add_shifts(((Scheme_Propagate_Table *)mt)->phase_shift, shift);
    if (!shift)
      shift = scheme_box(scheme_false); /* i.e., the impossible shift */
    ((Scheme_Propagate_Table *)mt2)->phase_shift = shift;
  }

  return mt2;
}

static Scheme_Object *shift_marks(Scheme_Object *o, Scheme_Object *shift, int can_mutate, int prop_only)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Mark_Table *mt, *p_mt;

  if (prop_only)
    mt = stx->marks;
  else
    mt = shift_mark_table(stx->marks, shift, shift_multi_mark, NULL);
  if (STX_KEY(stx) & STX_SUBSTX_FLAG)
    p_mt = shift_mark_table((stx->u.to_propagate ? stx->u.to_propagate : empty_propagate_table),
                            shift, shift_prop_multi_mark, stx->marks);
  else
    p_mt = NULL;
  
  if (SAME_OBJ(stx->marks, mt)
      && (!(STX_KEY(stx) & STX_SUBSTX_FLAG)
          || SAME_OBJ(stx->u.to_propagate, p_mt)))
    return (Scheme_Object *)stx;

  if (!can_mutate)
    stx = (Scheme_Stx *)clone_stx((Scheme_Object *)stx);

  stx->marks = mt;
  if (p_mt)
    stx->u.to_propagate = p_mt;
  
  return (Scheme_Object *)stx;
}

static Scheme_Object *do_stx_add_shift(Scheme_Object *o, Scheme_Object *shift, int can_mutate)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *vec, *shifts;

  if (!shift) return (Scheme_Object *)stx;

  if (SCHEME_PHASE_SHIFTP(shift)) {
    if (SAME_OBJ(shift, scheme_make_integer(0)))
      return (Scheme_Object *)stx;
    return shift_marks((Scheme_Object *)stx, shift, can_mutate, 0);
  }

  if (SCHEME_VECTORP(shift)
      && (SCHEME_VEC_ELS(shift)[4] != scheme_make_integer(0))) {
    /* Handle phase shift by itself, first: */
    stx = (Scheme_Stx *)do_stx_add_shift((Scheme_Object *)stx, SCHEME_VEC_ELS(shift)[4], can_mutate);
    /* strip away phase shift: */
    vec = scheme_make_vector(5, NULL);
    SCHEME_VEC_ELS(vec)[0] = SCHEME_VEC_ELS(shift)[0];
    SCHEME_VEC_ELS(vec)[1] = SCHEME_VEC_ELS(shift)[1];
    SCHEME_VEC_ELS(vec)[2] = SCHEME_VEC_ELS(shift)[2];
    SCHEME_VEC_ELS(vec)[3] = SCHEME_VEC_ELS(shift)[3];
    SCHEME_VEC_ELS(vec)[4] = scheme_make_integer(0);
    shift = vec;
    can_mutate = 1;
  }

  /* Drop useless shift: */
  if (SAME_OBJ(SCHEME_VEC_ELS(shift)[0], SCHEME_VEC_ELS(shift)[1])
      && SCHEME_FALSEP(SCHEME_VEC_ELS(shift)[2])
      && SCHEME_FALSEP(SCHEME_VEC_ELS(shift)[3]))
    return (Scheme_Object *)stx;

  if (STX_KEY(stx) & STX_SUBSTX_FLAG) {
    /* Keep track of shifts that need to be propagated */
    vec = scheme_make_vector(3, NULL);
    if (SCHEME_VECTORP(stx->shifts)) {
      shifts = scheme_make_pair(shift, SCHEME_VEC_ELS(stx->shifts)[1]);
      SCHEME_VEC_ELS(vec)[1] = shifts;
      SCHEME_VEC_ELS(vec)[2] = SCHEME_VEC_ELS(stx->shifts)[2];
      shifts = SCHEME_VEC_ELS(stx->shifts)[0];
    } else {
      shifts = scheme_make_pair(shift, scheme_null);
      SCHEME_VEC_ELS(vec)[1] = shifts;
      SCHEME_VEC_ELS(vec)[2] = stx->shifts;
      shifts = stx->shifts;
    }
    shifts = scheme_make_pair(shift, shifts);
    SCHEME_VEC_ELS(vec)[0] = shifts;
    shifts = vec;
  } else {
    /* No need to propagate, so it's a simple addition. */
    shifts = scheme_make_pair(shift, stx->shifts);
  }

  if (!can_mutate)
    stx = (Scheme_Stx *)clone_stx((Scheme_Object *)stx);
  stx->shifts = shifts;
  
  if ((STX_KEY(stx) & STX_SUBSTX_FLAG) && !stx->u.to_propagate)
    stx->u.to_propagate = empty_propagate_table;

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_stx_add_shift(Scheme_Object *o, Scheme_Object *shift)
{
  return do_stx_add_shift(o, shift, 0);
}

Scheme_Object *scheme_stx_add_shifts(Scheme_Object *o, Scheme_Object *l)
{
  int can_mutate = 0;

  for (l = scheme_reverse(l); !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    o = do_stx_add_shift(o, SCHEME_CAR(l), can_mutate);
    can_mutate = 1;
  }

  return o;
}

Scheme_Object *scheme_make_shift(Scheme_Object *phase_delta,
                                 Scheme_Object *old_midx, Scheme_Object *new_midx,
                                 Scheme_Hash_Table *export_registry, Scheme_Object *insp)
{
  if (!phase_delta)
    phase_delta = scheme_make_integer(0);

  if (new_midx || export_registry || insp) {
    Scheme_Object *vec;

    vec = last_phase_shift;
    
    if (vec
	&& (SCHEME_VEC_ELS(vec)[0] == (new_midx ? old_midx : scheme_false))
	&& (SCHEME_VEC_ELS(vec)[1] == (new_midx ? new_midx : scheme_false))
	&& (SCHEME_VEC_ELS(vec)[2] == (export_registry ? (Scheme_Object *)export_registry : scheme_false))
        && (SCHEME_VEC_ELS(vec)[3] == (insp ? insp : scheme_false))
        && (SCHEME_VEC_ELS(vec)[4] == phase_delta)) {
      /* use the old one */
    } else {
      vec = scheme_make_vector(5, NULL);
      SCHEME_VEC_ELS(vec)[0] = (new_midx ? old_midx : scheme_false);
      SCHEME_VEC_ELS(vec)[1] = (new_midx ? new_midx : scheme_false);
      SCHEME_VEC_ELS(vec)[2] = (export_registry ? (Scheme_Object *)export_registry : scheme_false);
      SCHEME_VEC_ELS(vec)[3] = (insp ? insp : scheme_false);
      SCHEME_VEC_ELS(vec)[4] = phase_delta;
      
      last_phase_shift = vec;
    }

    return last_phase_shift;
  } else
    return NULL;
}

void scheme_clear_shift_cache(void)
{
  last_phase_shift = NULL;
  nominal_ipair_cache = NULL;
}

Scheme_Object *scheme_stx_shift(Scheme_Object *stx, 
                                Scheme_Object *phase_delta,
                                Scheme_Object *old_midx, Scheme_Object *new_midx,
                                Scheme_Hash_Table *export_registry,
                                Scheme_Object *insp)
/* Shifts the modidx on a syntax object in a module as well as the phase of marks. */
{
  Scheme_Object *s;

  s = scheme_make_shift(phase_delta, old_midx, new_midx, export_registry, insp);
  if (s)
    stx = scheme_stx_add_shift(stx, s);

  return stx;
}

static Scheme_Object *apply_modidx_shifts(Scheme_Object *shifts, Scheme_Object *modidx, Scheme_Object **_insp)
{
#define QUICK_SHIFT_LEN 5
  Scheme_Object *vec, *dest, *src, *insp;
  Scheme_Object *quick_a[QUICK_SHIFT_LEN], **a;
  intptr_t i, len;

  /* Strip away propagation layer, if any: */
  if (SCHEME_VECTORP(shifts))
    shifts = SCHEME_VEC_ELS(shifts)[0];

  /* Skip phase shift, if any: */
  if (!SCHEME_NULLP(shifts) && SCHEME_PHASE_SHIFTP(SCHEME_CAR(shifts)))
    shifts = SCHEME_CDR(shifts);

  /* The `shifts` list is in the reverse order that we want... */

  len = scheme_list_length(shifts);
  if (len <= QUICK_SHIFT_LEN)
    a = quick_a;
  else
    a = MALLOC_N(Scheme_Object *, len);

  i = len;
  while (!SCHEME_NULLP(shifts)) {
    a[--i] = SCHEME_CAR(shifts);
    shifts = SCHEME_CDR(shifts);
  }

  for (i = 0; i < len; i++) {
    vec = a[i];
     
    src = SCHEME_VEC_ELS(vec)[0];
    dest = SCHEME_VEC_ELS(vec)[1];
    insp = SCHEME_VEC_ELS(vec)[3];

    if (!SCHEME_FALSEP(src))
      modidx = scheme_modidx_shift(modidx, src, dest);

    if (_insp && SCHEME_TRUEP(insp))
      *_insp = insp;
  }

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

  return scheme_stx_add_shift(argv[0], argv[1]);
}

/******************** lazy propagation ********************/

#if 0
# define COUNT_PROPAGATES(x) x
int stx_shorts, stx_meds, stx_longs, stx_couldas;
#else
# define COUNT_PROPAGATES(x) /* empty */
#endif

static Scheme_Object *propagate_mark_set(Scheme_Mark_Set *props, Scheme_Object *o,
                                         Scheme_Object *phase, int flag)
{
  intptr_t i;
  Scheme_Object *key, *val;

  i = mark_set_next(props, -1);
  while (i != -1) {
    mark_set_index(props, i, &key, &val);

    STX_ASSERT(!((Scheme_Mark *)key)->owner_multi_mark);

    val = scheme_stx_adjust_mark(o, key, phase, SCHEME_INT_VAL(val) | flag);
    if (!SAME_OBJ(o, val))
      flag |= SCHEME_STX_MUTATE;
    o = val;
    
    i = mark_set_next(props, i);
  }  

  return o;
}

XFORM_NONGCING static int equiv_mark_tables(Scheme_Mark_Table *a, Scheme_Mark_Table *b)
{
  if (a == b)
    return 1;

  if (!mark_set_count(a->single_marks)
      && !mark_set_count(b->single_marks)
      && SAME_OBJ(a->multi_marks, b->multi_marks))
    return 1;

  return 0;
}

static Scheme_Object *propagate_marks(Scheme_Object *o, Scheme_Mark_Table *to_propagate,
                                      Scheme_Mark_Table *parent_marks, int flag)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *key, *val;

  if (!to_propagate || (to_propagate == empty_propagate_table))
    return o;

  /* Check whether the child marks currently match the
     parent's marks before the propagated changes: */
  if (!(flag & SCHEME_STX_PROPONLY)
      && equiv_mark_tables(((Scheme_Propagate_Table *)to_propagate)->prev, stx->marks)) {
    /* Yes, so we can take a shortcut: child marks still match parent.
       Does the child need to propagate, and if so, does it just
       get the parent's propagation? */
    if (!(STX_KEY(stx) & STX_SUBSTX_FLAG)
        || !stx->u.to_propagate
        || SAME_OBJ(stx->u.to_propagate, empty_propagate_table)) {
      /* Yes, child matches the parent in all relevant dimensions */
      stx = (Scheme_Stx *)clone_stx((Scheme_Object *)stx);
      stx->marks = parent_marks;
      if (STX_KEY(stx) & STX_SUBSTX_FLAG)
        stx->u.to_propagate = to_propagate;
      COUNT_PROPAGATES(stx_shorts++);
      return (Scheme_Object *)stx;
    } else {
      /* Child marks match parent, so we don't need to reconstruct
         the mark set, but we need to build a new propagation set
         to augment the propagate set already here */
      flag |= SCHEME_STX_PROPONLY;
      COUNT_PROPAGATES(stx_meds++);
    }
  } else {
    COUNT_PROPAGATES(stx_longs++);
  }

  val = ((Scheme_Propagate_Table *)to_propagate)->phase_shift;
  if (!SAME_OBJ(val, scheme_make_integer(0))) {
    val = shift_marks(o, val, flag & SCHEME_STX_MUTATE, flag & SCHEME_STX_PROPONLY);
    if (!SAME_OBJ(o, val))
      flag |= SCHEME_STX_MUTATE;
    o = val;
  }

  val = propagate_mark_set(to_propagate->single_marks, o, scheme_true, flag);
  if (!SAME_OBJ(o, val))
    flag |= SCHEME_STX_MUTATE;
  o = val;

  for (key = to_propagate->multi_marks; !SCHEME_NULLP(key); key = SCHEME_CDR(key)) {
    val = SCHEME_CAR(key);
    STX_ASSERT(SCHEME_MULTI_MARKP(SCHEME_VEC_ELS(val)[0]));
    val = scheme_stx_adjust_mark(o, SCHEME_VEC_ELS(val)[0], SCHEME_VEC_ELS(val)[1], 
                                 SCHEME_INT_VAL(SCHEME_VEC_ELS(val)[2]) | flag);
    if (!SAME_OBJ(o, val))
      flag |= SCHEME_STX_MUTATE;
    o = val;
  }
  
  if ((flag & SCHEME_STX_PROPONLY) && !(flag & SCHEME_STX_MUTATE)) {
    /* Force clone so that we can set marks to parent marks */
    o = clone_stx(o);
    flag |= SCHEME_STX_MUTATE;
  }

  if (flag & SCHEME_STX_PROPONLY)
    ((Scheme_Stx *)o)->marks = parent_marks;

  return o;
}

static Scheme_Object *propagate_shifts(Scheme_Object *result, Scheme_Object *shifts, int can_mutate)
{
  Scheme_Stx *stx = (Scheme_Stx *)result;
  Scheme_Object *l;

  if (SAME_OBJ(stx->shifts, SCHEME_VEC_ELS(shifts)[2])) {
    if (!can_mutate) {
      result = clone_stx(result);
      stx = (Scheme_Stx *)result;
    }
    if ((STX_KEY(stx) & STX_SUBSTX_FLAG)) {
      stx->shifts = shifts;
      if (!stx->u.to_propagate)
        stx->u.to_propagate = empty_propagate_table;
    } else
      stx->shifts = SCHEME_VEC_ELS(shifts)[0];
    return result;
  }

  for (l = scheme_reverse(SCHEME_VEC_ELS(shifts)[1]); !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    result = do_stx_add_shift(result, SCHEME_CAR(l), can_mutate);
    can_mutate = 1;
  }

  return result;
}

static Scheme_Object *propagate(Scheme_Object *result,
                                Scheme_Mark_Table *to_propagate,
                                Scheme_Mark_Table *parent_marks,
                                Scheme_Object *shifts,
                                int add_taint, Scheme_Object *false_insp)
{
  Scheme_Object *orig_result = result;

  result = propagate_marks(result, to_propagate, parent_marks, 0);

  if (shifts)
    result = propagate_shifts(result, shifts, !SAME_OBJ(result, orig_result));

  if (add_taint)
    result = add_taint_to_stx(result, SAME_OBJ(result, orig_result));
  else if (false_insp)
    result = set_false_insp(result, false_insp, SAME_OBJ(result, orig_result));

  return result;
}

int propagate_count;

static Scheme_Object *raw_stx_content(Scheme_Object *o)
 /* Propagates wraps and taints while getting a syntax object's content. */
{
  Scheme_Stx *stx = (Scheme_Stx *)o;

  /* The fast-path tests are duplicated in jit.c. */

  if ((STX_KEY(stx) & STX_SUBSTX_FLAG) && stx->u.to_propagate) {
    Scheme_Object *v = stx->val, *result;
    Scheme_Mark_Table *to_propagate;
    Scheme_Object *false_insp, *shifts;
    int add_taint;

    to_propagate = stx->u.to_propagate;
    false_insp = stx->taints;
    if (false_insp && SCHEME_VOIDP(false_insp)) {
      add_taint = 1;
    } else {
      add_taint = 0;
      if (false_insp) {
        if (SCHEME_PAIRP(false_insp))
          false_insp = SCHEME_CAR(false_insp);
        if (!SCHEME_INSPECTORP(false_insp))
          false_insp = NULL;
      }
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
      stx->u.to_propagate = empty_propagate_table;
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
      stx->u.to_propagate = empty_propagate_table;
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

Scheme_Mark *extract_max_mark_and_increment_timestamps(Scheme_Mark_Set *marks)
{
  intptr_t i;
  Scheme_Object *key, *val;
  Scheme_Mark *mark;
  mzlonglong mark_id_val, id_val;
  intptr_t timestamp;

  timestamp = ++binding_cache_timestamp;

  i = mark_set_next(marks, -1);
  mark_set_index(marks, i, &key, &val);

  mark = (Scheme_Mark *)key;
  mark_id_val = mark->id;
  mark->timestamp = timestamp;

  i = mark_set_next(marks, i);
  while (i != -1) {
    mark_set_index(marks, i, &key, &val);

    ((Scheme_Mark *)key)->timestamp = timestamp;

    id_val = ((Scheme_Mark *)key)->id;
    if (id_val > mark_id_val) {
      mark = (Scheme_Mark *)key;
      mark_id_val = id_val;
    }
    
    i = mark_set_next(marks, i);
  }

  return mark;
}

static int mark_timestamps_older(Scheme_Mark_Set *marks, intptr_t timestamp)
{
  intptr_t i;
  Scheme_Object *key, *val;

  i = mark_set_next(marks, -1);
  while (i != -1) {
    mark_set_index(marks, i, &key, &val);

    if (((Scheme_Mark *)key)->timestamp > timestamp)
      return 0;
    i = mark_set_next(marks, i);
  }

  return 1;
}

#define SCHEME_BINDING_MARKS(p) ((Scheme_Mark_Set *)SCHEME_CAR(p))
#define SCHEME_BINDING_VAL(p)   SCHEME_CDR(p)

XFORM_NONGCING static void save_old_value(Scheme_Object *mp, Scheme_Object *old_val)
{
  if (SCHEME_MPAIRP(old_val))
    SCHEME_CAR(mp) = SCHEME_CAR(old_val);
  else
    SCHEME_CAR(mp) = old_val;
}

static void add_binding(Scheme_Object *sym, Scheme_Object *phase, Scheme_Mark_Set *marks,
                        Scheme_Object *val)
/* `val` can be a symbol (local binding), a modidx/pair/#f
   (module/global binding), a shared-binding vector (i.e., a pes), or
   a syntax object (for a `free-identifier=?` equivalence) to be
   mutable-paired with the existing binding; the `sym` argument should
   be NULL when `val` is a shared-binding vector */

{
  Scheme_Hash_Table *ht;
  Scheme_Mark *mark;
  Scheme_Object *l, *p, *bind;

  if (mark_set_count(marks)) {
    mark = extract_max_mark_and_increment_timestamps(marks);
  } else {
    scheme_signal_error("internal error: cannot bind identifier with an empty context");
    return;
  }

  /* We add the binding to the maximum-valued mark, because it's
     likely to be in the least number of binding sets so far. */

  l = mark->bindings;
  if (!l) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    l = scheme_make_raw_pair((Scheme_Object *)ht, NULL);
    mark->bindings = l;
  } else {
    ht = (Scheme_Hash_Table *)SCHEME_CAR(l);
  }
  
  STX_ASSERT(SCHEME_STXP(val)
             || SCHEME_FALSEP(val)
             || SCHEME_MODIDXP(val)
             || SCHEME_PAIRP(val)
             || SCHEME_VECTORP(val)
             || SCHEME_SYMBOLP(val));

  if (SCHEME_STXP(val))
    val = scheme_make_mutable_pair(scheme_false, scheme_make_pair(val, phase));

  bind = scheme_make_pair((Scheme_Object *)marks, val);

  if (sym) {
    STX_ASSERT(SCHEME_SYMBOLP(sym));
    l = scheme_hash_get(ht, sym);
    if (!l) l = scheme_null;
    for (p = l; !SCHEME_NULLP(p); p = SCHEME_CDR(p)) {
      if (marks_equal(marks, SCHEME_BINDING_MARKS(SCHEME_CAR(p)))) {
        if (SCHEME_MPAIRP(val))
          save_old_value(val, SCHEME_BINDING_VAL(SCHEME_CAR(p)));
        SCHEME_CAR(p) = bind;
        break;
      }
    }
    if (SCHEME_NULLP(p)) {
      l = scheme_make_pair(bind, l);
      scheme_hash_set(ht, sym, l);
    }
  } else {
    /* Order matters: the new bindings should hide any existing bindings for the same name */
    /* REMOVEME: FIXME: need to remove mappings from the hash table that are replaced here;
       that can happen due to an import at the top level, for example */
    p = scheme_make_raw_pair(bind, SCHEME_CDR(l));
    SCHEME_CDR(l) = p;
  }
}

void scheme_add_local_binding(Scheme_Object *o, Scheme_Object *phase, Scheme_Object *binding_sym)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;

  STX_ASSERT(SCHEME_SYMBOLP(binding_sym));

  add_binding(stx->val, phase, extract_mark_set(stx, phase), binding_sym);
}

static void do_add_module_binding(Scheme_Mark_Set *marks, Scheme_Object *localname, Scheme_Object *phase,
                                  Scheme_Object *modidx, Scheme_Object *exname, Scheme_Object *defn_phase,
                                  Scheme_Object *inspector,
                                  Scheme_Object *nominal_mod, Scheme_Object *nominal_ex,
                                  Scheme_Object *src_phase, 
                                  Scheme_Object *nom_phase,
                                  int skip_marshal)
{
  Scheme_Object *elem;
  int mod_phase;

  if (SCHEME_FALSEP(modidx)) {
    if (SAME_OBJ(localname, exname))
      add_binding(localname, phase, marks, scheme_false);
    else
      add_binding(localname, phase, marks, scheme_make_pair(scheme_false, exname));
    return;
  }

  STX_ASSERT(SCHEME_MODIDXP(modidx));

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

  mod_phase = SCHEME_INT_VAL(defn_phase);

  if (!src_phase)
    src_phase = phase;
  if (!nom_phase)
    nom_phase = scheme_make_integer(mod_phase);

  if (SAME_OBJ(modidx, nominal_mod)
      && SAME_OBJ(exname, nominal_ex)
      && !mod_phase
      && same_phase(src_phase, phase)
      && same_phase(nom_phase, scheme_make_integer(mod_phase))) {
    if (SAME_OBJ(localname, exname))
      elem = modidx;
    else
      elem = CONS(modidx, exname);
  } else if (SAME_OBJ(exname, nominal_ex)
	     && SAME_OBJ(localname, exname)
	     && !mod_phase
             && same_phase(src_phase, phase)
             && same_phase(nom_phase, scheme_make_integer(mod_phase))) {
    /* It's common that a sequence of similar mappings shows up,
       e.g., '(#%kernel . mzscheme) */
    if (nominal_ipair_cache
	&& SAME_OBJ(SCHEME_CAR(nominal_ipair_cache), modidx)
	&& SAME_OBJ(SCHEME_CDR(nominal_ipair_cache), nominal_mod))
      elem = nominal_ipair_cache;
    else {
      elem = ICONS(modidx, nominal_mod);
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
    elem = CONS(modidx, elem);
  }

  if (!SCHEME_FALSEP(inspector))
    elem = CONS(inspector, elem);

  add_binding(localname, phase, marks, elem);
}

void scheme_add_module_binding(Scheme_Object *o, Scheme_Object *phase, 
                               Scheme_Object *modidx, Scheme_Object *sym, Scheme_Object *defn_phase)
{
  STX_ASSERT(SCHEME_SYMBOLP(((Scheme_Stx *)o)->val));

  do_add_module_binding(extract_mark_set((Scheme_Stx *)o, phase), SCHEME_STX_VAL(o), phase,
                        modidx, sym, defn_phase,
                        scheme_false,
                        modidx, sym,
                        NULL, NULL,
                        0);
}

void scheme_add_module_binding_w_nominal(Scheme_Object *o, Scheme_Object *phase,
                                         Scheme_Object *modidx, Scheme_Object *defn_name, Scheme_Object *defn_phase,
                                         Scheme_Object *inspector,
                                         Scheme_Object *nominal_mod, Scheme_Object *nominal_name,
                                         Scheme_Object *nominal_import_phase, 
                                         Scheme_Object *nominal_export_phase,
                                         int skip_marshal)
{
  STX_ASSERT(SCHEME_STXP(o));
  do_add_module_binding(extract_mark_set((Scheme_Stx *)o, phase), SCHEME_STX_VAL(o), phase,
                        modidx, defn_name, defn_phase,
                        inspector,
                        nominal_mod, nominal_name,
                        nominal_import_phase, nominal_export_phase,
                        skip_marshal);
}

static void print_indent(int indent)
{
  while (indent--) {
    printf("#");
  }
}

static void print_marks(Scheme_Mark_Set *marks, Scheme_Mark_Set *check_against_marks, int indent)
{
  intptr_t i;
  Scheme_Object *key, *val;

  print_indent(indent);
  printf("  ");

  i = mark_set_next(marks, -1);
  while (i != -1) {
    mark_set_index(marks, i, &key, &val);
    
    printf(" %s", scheme_write_to_string(scheme_mark_printed_form(key), NULL));
    if (((Scheme_Mark *)key)->owner_multi_mark)
      printf("/%" PRIdPTR, scheme_hash_key(SCHEME_CAR(((Scheme_Mark *)key)->owner_multi_mark)));

    i = mark_set_next(marks, i);
  }

  if (check_against_marks && mark_subset(marks, check_against_marks))
    printf(" [match]");

  printf("\n");
}


static void print_at(Scheme_Stx *stx, Scheme_Object *phase, int level, int indent, Scheme_Object *seen)
{
  int full_imports = 0;
  Scheme_Hash_Table *ht;
  Scheme_Object *key, *val, *l, *pes;
  intptr_t i, j;
  Scheme_Mark *mark;
  Scheme_Mark_Set *marks;
  Scheme_Module_Phase_Exports *pt;

  for (l = seen; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (SAME_OBJ((Scheme_Object *)stx, SCHEME_CAR(seen))) {
      print_indent(indent);
      printf(" [cycle]\n");
      return;
    }
  }

  marks = extract_mark_set(stx, phase);

  print_indent(indent);
  printf(" At phase %s:\n", scheme_write_to_string(phase, NULL));
  print_marks(marks, NULL, indent);

  if (!level)
    return;

  i = mark_set_next(marks, -1);
  while (i != -1) {
    mark_set_index(marks, i, &key, &val);

    mark = (Scheme_Mark *)key;
    if (mark->bindings) {
      ht = (Scheme_Hash_Table *)SCHEME_CAR(mark->bindings);
      for (j = ht->size; j--; ) {
	if (ht->vals[j]
            && ((level > 1) || SAME_OBJ(ht->keys[j], stx->val))) {
          print_indent(indent);
          printf("  %s =", scheme_write_to_string(ht->keys[j], NULL));
          l = ht->vals[j];
          while (l && !SCHEME_NULLP(l)) {
            val = SCHEME_BINDING_VAL(SCHEME_CAR(l));
            if (SCHEME_SYMBOLP(val))
              printf(" local\n");
            else
              printf(" %s\n", scheme_write_to_string(val, 0));
            print_marks(SCHEME_BINDING_MARKS(SCHEME_CAR(l)), marks, indent);

            if (SCHEME_MPAIRP(val)) {
              print_indent(indent);
              printf("  == free-identifier=? =>\n");
              print_at((Scheme_Stx *)SCHEME_CAR(SCHEME_CDR(val)), SCHEME_CDR(SCHEME_CDR(val)),
                       level, indent+2,
                       scheme_make_pair((Scheme_Object *)stx, seen));
            }

            l = SCHEME_CDR(l);
          }
        }
      }
          
      l = SCHEME_CDR(mark->bindings);
      while (l && !SCHEME_NULLP(l)) {
        print_indent(indent);
        printf("  ...\n");
        print_marks(SCHEME_BINDING_MARKS(SCHEME_CAR(l)), marks, indent);
        
        pes = SCHEME_BINDING_VAL(SCHEME_CAR(l));
        if (SCHEME_VEC_SIZE(pes) == 3) {
          print_indent(indent);
          printf("    [unmarshal %s]\n",
                 (SCHEME_FALSEP(SCHEME_VEC_ELS(pes)[0]) ? "done" : "pending"));
        } else {
          print_indent(indent);
          printf("    %s\n", scheme_write_to_string(scheme_module_resolve(SCHEME_VEC_ELS(pes)[0], 0), NULL));
          
          pt = (Scheme_Module_Phase_Exports *)SCHEME_VEC_ELS(pes)[1];
          if (!pt->ht)
            scheme_populate_pt_ht(pt);

          ht = pt->ht;
          
          if (full_imports) {
            for (j = ht->size; j--; ) {
              if (ht->vals[j]) {
                print_indent(indent);
                printf("    %s\n", scheme_write_to_string(ht->keys[j], NULL));
              }
            }
          } else {
            if (scheme_hash_get(ht, stx->val)) {
              print_indent(indent);
              printf("      has %s\n", scheme_write_to_string(stx->val, NULL));
            }
          }
        }
        l = SCHEME_CDR(l);
      }
    }
    
    i = mark_set_next(marks, i);
  }
}

void scheme_stx_debug_print(Scheme_Object *_stx, Scheme_Object *phase, int level)
{
  Scheme_Stx *stx = (Scheme_Stx *)_stx;

  STX_ASSERT(SCHEME_STXP(_stx));

  printf("%s:\n", scheme_write_to_string(stx->val, NULL));
  print_at(stx, phase, level, 0, scheme_null);
}

static void add_marks_mapped_names(Scheme_Mark_Set *marks, Scheme_Hash_Table *mapped)
{
  int retry;
  Scheme_Hash_Table *ht;
  Scheme_Object *key, *val, *l, *pes;
  intptr_t i, j;
  Scheme_Mark *mark;
  Scheme_Mark_Set *binding_marks;
  Scheme_Module_Phase_Exports *pt;

  do {
    retry = 0;
    i = mark_set_next(marks, -1);
    while (i != -1) {
      mark_set_index(marks, i, &key, &val);
      
      mark = (Scheme_Mark *)key;
      if (mark->bindings) {
        /* Check table of symbols */
        ht = (Scheme_Hash_Table *)SCHEME_CAR(mark->bindings);
        for (j = ht->size; j--; ) {
          l = ht->vals[j];
          while (l && !SCHEME_NULLP(l)) {
            if (mark_subset(SCHEME_BINDING_MARKS(SCHEME_CAR(l)), marks)) {
              scheme_hash_set(mapped, ht->keys[j], scheme_true);
              break;
            }
            l = SCHEME_CDR(l);
          }
        }

        /* Check list of shared-binding tables */
        l = SCHEME_CDR(mark->bindings);
        while (l && !SCHEME_NULLP(l)) {
          binding_marks = SCHEME_BINDING_MARKS(SCHEME_CAR(l));
          if (mark_subset(binding_marks, marks)) {
            pes = SCHEME_BINDING_VAL(SCHEME_CAR(l));
            if (SCHEME_VEC_SIZE(pes) == 3) {
              if (SCHEME_TRUEP(SCHEME_VEC_ELS(pes)[0])) {
                unmarshal_module_context_additions(NULL, pes, binding_marks, l);
                retry = 1;
              }
            } else {
              pt = (Scheme_Module_Phase_Exports *)SCHEME_VEC_ELS(pes)[1];
              if (!pt->ht)
                scheme_populate_pt_ht(pt);
              for (j = pt->ht->size; j--; ) {
                if (pt->ht->vals[j]) {
                  scheme_hash_set(mapped, pt->ht->keys[j], scheme_true);
                }
              }
            }
          }
          l = SCHEME_CDR(l);
        }
      }
      i = mark_set_next(marks, i);
    }
  } while (retry);
}

static void *do_stx_lookup(Scheme_Stx *stx, Scheme_Mark_Set *marks,
                           Scheme_Mark_Set *check_subset,
                           int *_exact_match, int *_ambiguous)
{
  int j, invalid;
  intptr_t i;
  Scheme_Object *key, *val, *result_best_so_far, **cached_result, *l, *pes;
  Scheme_Mark *mark, *mark_best_so_far;
  Scheme_Mark_Set *binding_marks, *best_so_far;
  Scheme_Module_Phase_Exports *pt;

  do {
    invalid = 0; /* to indicate retry if we unmarshal */
    best_so_far = NULL;
    mark_best_so_far = NULL;
    result_best_so_far = NULL;

    i = mark_set_next(marks, -1);
    while ((i != -1) && !invalid) {
      mark_set_index(marks, i, &key, &val);

      mark = (Scheme_Mark *)key;
      if (mark->bindings) {
        for (j = 0; j < 2; j++) {
          if (!j)
            l = scheme_hash_get((Scheme_Hash_Table *)SCHEME_CAR(mark->bindings),
                                stx->val);
          else
            l = SCHEME_CDR(mark->bindings);

          while (l && !SCHEME_NULLP(l) && !invalid) {
            binding_marks = SCHEME_BINDING_MARKS(SCHEME_CAR(l));
          
            if (j) {
              pes = SCHEME_BINDING_VAL(SCHEME_CAR(l));
              if (SCHEME_VEC_SIZE(pes) == 3) {
                /* Not a pes; an unmarshal */
                if (SCHEME_TRUEP(SCHEME_VEC_ELS(pes)[0])) {
                  /* Need unmarshal --- but only if the mark set is relevant */
                  if (mark_subset(binding_marks, marks)) {
                    /* unmarshal and note that we must restart */
                    unmarshal_module_context_additions(stx, pes, binding_marks, l);
                    invalid = 1;
                    /* Shouldn't encounter these on a second pass: */
                    STX_ASSERT(!check_subset);
                    STX_ASSERT(!_ambiguous);
                    STX_ASSERT(!_exact_match);
                  }
                }
                binding_marks = NULL;
              } else {
                /* Check for id in pes */
                pt = (Scheme_Module_Phase_Exports *)SCHEME_VEC_ELS(pes)[1];
                if (!pt->ht) {
                  /* Lookup table (which is created lazily) not yet created, so do that now... */
                  scheme_populate_pt_ht(pt);
                }
                
                if (!scheme_hash_get(pt->ht, stx->val))
                  binding_marks = NULL;
              }
            }
          
            if (binding_marks && mark_subset(binding_marks, marks)) {
              if (check_subset && !mark_subset(binding_marks, check_subset)) {
                if (_ambiguous) *_ambiguous = 1;
                return NULL; /* ambiguous */
              }
              if (!best_so_far
                  || ((mark_set_count(binding_marks) > mark_set_count(best_so_far))
                      && (!check_subset
                          || (mark_set_count(binding_marks) == mark_set_count(check_subset))))) {
                best_so_far = binding_marks;
                mark_best_so_far = mark;
                result_best_so_far = SCHEME_BINDING_VAL(SCHEME_CAR(l));
                STX_ASSERT(SCHEME_FALSEP(result_best_so_far)
                           || SCHEME_MODIDXP(result_best_so_far)
                           || SCHEME_PAIRP(result_best_so_far)
                           || SCHEME_VECTORP(result_best_so_far)
                           || SCHEME_SYMBOLP(result_best_so_far)
                           || SCHEME_MPAIRP(result_best_so_far));
                if (_exact_match) *_exact_match = (mark_set_count(binding_marks) == mark_set_count(marks));
              }
            }
            l = SCHEME_CDR(l);
          }
        }
      }
    
      i = mark_set_next(marks, i);
    }
  } while (invalid);

  if (!best_so_far)
    return NULL;

  if (check_subset) {
    cached_result = MALLOC_N(Scheme_Object*, 5);
    cached_result[0] = result_best_so_far;
    cached_result[1] = scheme_make_integer(binding_cache_timestamp);
    cached_result[2] = (Scheme_Object *)best_so_far;
    
    return cached_result;
  } else
    return best_so_far;
}

static Scheme_Object **do_stx_lookup_nonambigious(Scheme_Stx *stx, Scheme_Object *phase,
                                                  int *_exact_match, int *_ambiguous,
                                                  Scheme_Mark_Set **_binding_marks)
{
  Scheme_Mark_Set *marks, *best_set;

  marks = extract_mark_set(stx, phase);

  best_set = (Scheme_Mark_Set *)do_stx_lookup(stx, marks,
                                              NULL, 
                                              NULL, NULL);
  if (!best_set)
    return NULL;

  if (_binding_marks) *_binding_marks = best_set;

  /* Find again, this time checking to ensure no ambiguity: */
  return (Scheme_Object **)do_stx_lookup(stx, marks,
                                         best_set,
                                         _exact_match, _ambiguous);
}

static Scheme_Object *apply_accumulated_shifts(Scheme_Object *result, Scheme_Object *prev_shifts, 
                                               Scheme_Object **_insp, Scheme_Object **nominal_modidx,
                                               Scheme_Stx *stx, Scheme_Object *orig_name, Scheme_Object *phase)
/* Adjust result to take the `free-id=?` chain into account: adjust a
   `#f` result to add in the original name, or adjust a module name
   for modidx shifts */
{
  Scheme_Object *o;

  if (SCHEME_VECTORP(result)) {
    if (!SCHEME_NULLP(prev_shifts)
        || (SCHEME_FALSEP(SCHEME_VEC_ELS(result)[0])
            && !SAME_OBJ(stx->val, orig_name))) {
      /* Clone result vector */
      o = scheme_make_vector(3, NULL);
      SCHEME_VEC_ELS(o)[0] = SCHEME_VEC_ELS(result)[0];
      SCHEME_VEC_ELS(o)[1] = SCHEME_VEC_ELS(result)[1];
      SCHEME_VEC_ELS(o)[2] = SCHEME_VEC_ELS(result)[2];
      result = o;

      if (SCHEME_FALSEP(SCHEME_VEC_ELS(result)[1]))
        SCHEME_VEC_ELS(result)[1] = stx->val;
    
      for (; !SCHEME_NULLP(prev_shifts); prev_shifts = SCHEME_CDR(prev_shifts)) {
        o = apply_modidx_shifts(SCHEME_CAR(prev_shifts), SCHEME_VEC_ELS(result)[0], _insp);
        SCHEME_VEC_ELS(result)[0] = o;
        if (nominal_modidx) {
          o = apply_modidx_shifts(SCHEME_CAR(prev_shifts), *nominal_modidx, NULL);
          *nominal_modidx = o;
        }
      }
    }
  } else if (SCHEME_FALSEP(result) && !SAME_OBJ(stx->val, orig_name)) {
    result = scheme_make_vector(3, NULL);
    SCHEME_VEC_ELS(result)[0] = scheme_false;
    SCHEME_VEC_ELS(result)[1] = stx->val;
    SCHEME_VEC_ELS(result)[2] = phase;
  }

  return result;
}

Scheme_Object *scheme_stx_lookup_w_nominal(Scheme_Object *o, Scheme_Object *phase, 
                                           int stop_at_free_eq,
                                           int *_exact_match, int *_ambiguous,
                                           Scheme_Mark_Set **_binding_marks,
                                           Scheme_Object **_insp,             /* access-granting inspector */
                                           Scheme_Object **nominal_modidx,    /* how it was imported */
                                           Scheme_Object **nominal_name,      /* imported as name */
                                           Scheme_Object **src_phase,         /* phase level of import from nominal modidx */ 
                                           Scheme_Object **nominal_src_phase) /* phase level of export from nominal modidx */
/* Result is either a representation of a local binding (probably a symbol),
   a vector of the form (vector <modidx> <symbol> <defn-phase>), or
   #f */
{
  Scheme_Stx *stx;
  Scheme_Object **cached_result, *result, *insp;
  Scheme_Object *free_eq_mpair, *prev_shifts = scheme_null, *orig_name;
  Scheme_Hash_Table *free_id_seen = NULL;

  STX_ASSERT(SCHEME_STXP(o));

  orig_name = SCHEME_STX_VAL(o);

  while (1) { /* loop for `free-identifier=?` chains */
    stx = (Scheme_Stx *)o;

    if (_ambiguous) *_ambiguous = 0;

    if (stx->u.cached_binding && !nominal_name) {
      Scheme_Mark_Set *marks;

      marks = (Scheme_Mark_Set *)stx->u.cached_binding[2];

      if (SAME_OBJ(stx->u.cached_binding[3], phase)
          && mark_timestamps_older(marks, SCHEME_INT_VAL(stx->u.cached_binding[1]))) {
        if (_binding_marks)
          *_binding_marks = marks;
        if (_exact_match) {
          if (mark_set_count(marks) == mark_set_count(extract_mark_set(stx, phase)))
            *_exact_match = 1;
          else
            *_exact_match = 0;
        }
        if (_insp) *_insp = stx->u.cached_binding[4];
        o = stx->u.cached_binding[0];

        if (SCHEME_MPAIRP(o)) {
          if (!stop_at_free_eq) {
            o = SCHEME_CDR(o);
            phase = SCHEME_CDR(o);
            o = SCHEME_CAR(o);
            /* recur to handle `free-identifier=?` chain */
            if (!free_id_seen)
              free_id_seen = scheme_make_hash_table(SCHEME_hash_ptr);
            if (scheme_hash_get(free_id_seen, o))
              return scheme_false; /* found a cycle */
            scheme_hash_set(free_id_seen, o, scheme_true);
            prev_shifts = scheme_make_pair(stx->shifts, prev_shifts);
            continue;
          } else
            return apply_accumulated_shifts(SCHEME_CAR(o), prev_shifts, _insp, NULL,
                                            stx, orig_name, phase);
        } else
          return apply_accumulated_shifts(o, prev_shifts, _insp, NULL,
                                          stx, orig_name, phase);
      }
    }

    if (_binding_marks) *_binding_marks = NULL;
    if (_exact_match) *_exact_match = 0;

    cached_result = do_stx_lookup_nonambigious(stx, phase,
                                               _exact_match, _ambiguous,
                                               _binding_marks);

    if (!cached_result)
      return apply_accumulated_shifts(scheme_false, scheme_null, NULL, NULL,
                                      stx, orig_name, phase);

    result = cached_result[0];

    /*
      `result` can be:
      - a symbol for a lexical binding,
      - a pair, modidx, or #f for a module import
      - a vector for a pes (shared export table from a module)
      - a mutable pair of the above plus an identifier for a `free-identifier=?` link
    */
    if (SCHEME_MPAIRP(result)) {
      free_eq_mpair = result;
      result = SCHEME_CAR(result);
    } else
      free_eq_mpair = NULL;

    if (!SCHEME_SYMBOLP(result)) {
      /* Generate a result vector: (vector <modidx> <sym> <phase>) */
      Scheme_Object *l = result;

      result = scheme_make_vector(3, NULL);
      SCHEME_VEC_ELS(result)[1] = stx->val;
      SCHEME_VEC_ELS(result)[2] = scheme_make_integer(0);

      if (nominal_modidx) *nominal_modidx = NULL;
      if (nominal_name) *nominal_name = NULL;
      if (src_phase) *src_phase = NULL;
      if (nominal_src_phase) *nominal_src_phase = NULL;

      if (SCHEME_FALSEP(l)) {
        /* top-level bound */
        SCHEME_VEC_ELS(result)[0] = scheme_false;
        /* phase of defn must be binding phase: */
        SCHEME_VEC_ELS(result)[2] = phase;
        insp = scheme_false;
      } else if (SCHEME_MODIDXP(l)) {
        SCHEME_VEC_ELS(result)[0] = l;
        insp = scheme_false;
      } else if (SCHEME_PAIRP(l)) {
        /* A list for a module import */
        if (SCHEME_INSPECTORP(SCHEME_CAR(l))) {
          insp = SCHEME_CAR(l);
          l = SCHEME_CDR(l);
        } else {
          insp = scheme_false;
        }
        if (SCHEME_MODIDXP(l)) {
          SCHEME_VEC_ELS(result)[0] = l;
        } else {
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
        }
      } else {
        /* A vector for a pes */
        Scheme_Module_Phase_Exports *pt;
        Scheme_Object *pos, *mod;

        STX_ASSERT(SCHEME_VECTORP(l));

        pt = (Scheme_Module_Phase_Exports *)SCHEME_VEC_ELS(l)[1];

        pos = scheme_hash_get(pt->ht, stx->val);

        if (pt->provide_srcs) {
          mod = pt->provide_srcs[SCHEME_INT_VAL(pos)];
          if (SCHEME_FALSEP(mod))
            mod = SCHEME_VEC_ELS(l)[0];
          else
            mod = scheme_modidx_shift(mod,
                                      pt->src_modidx,
                                      SCHEME_VEC_ELS(l)[0]);
        } else
          mod = SCHEME_VEC_ELS(l)[0];

        SCHEME_VEC_ELS(result)[0] = mod;

        if (nominal_modidx)
          *nominal_modidx = SCHEME_VEC_ELS(l)[0];

        SCHEME_VEC_ELS(result)[1] = pt->provide_src_names[SCHEME_INT_VAL(pos)];

        if (nominal_name)
          *nominal_name = pt->provides[SCHEME_INT_VAL(pos)];

        if (pt->provide_src_phases)
          SCHEME_VEC_ELS(result)[2] = scheme_make_integer(pt->provide_src_phases[SCHEME_INT_VAL(pos)]);

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

      l = apply_modidx_shifts(stx->shifts, SCHEME_VEC_ELS(result)[0], &insp);
      SCHEME_VEC_ELS(result)[0] = l;

      if (nominal_modidx) {
        l = apply_modidx_shifts(stx->shifts, *nominal_modidx, NULL);
        *nominal_modidx = l;
      }

      cached_result[0] = result;
    } else
      insp = scheme_false;

    if (free_eq_mpair) {
      free_eq_mpair = scheme_make_mutable_pair(result,
                                               SCHEME_CDR(free_eq_mpair));
      cached_result[0] = free_eq_mpair;
    }

    cached_result[3] = phase;
    cached_result[4] = insp;
    stx->u.cached_binding = cached_result;
  
    if (_insp) *_insp = insp;

    if (!free_eq_mpair || stop_at_free_eq)
      return apply_accumulated_shifts(result, prev_shifts, _insp, nominal_modidx,
                                      stx, orig_name, phase);

    /* Recur for `free-identifier=?` mapping */
    o = SCHEME_CDR(free_eq_mpair);
    phase = SCHEME_CDR(o);
    o = SCHEME_CAR(o);
    prev_shifts = scheme_make_pair(stx->shifts, prev_shifts);
  }
}

Scheme_Object *scheme_stx_lookup(Scheme_Object *o, Scheme_Object *phase)
{
  return scheme_stx_lookup_w_nominal(o, phase, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
}

Scheme_Object *scheme_stx_lookup_exact(Scheme_Object *o, Scheme_Object *phase)
{
  int exact;
  Scheme_Object *b;

  b = scheme_stx_lookup_w_nominal(o, phase, 1, &exact, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

  if (!exact)
    return scheme_false;
  else
    return b;
}

void scheme_populate_pt_ht(Scheme_Module_Phase_Exports * pt) {
  if (!pt->ht) {
    /* Lookup table (which is created lazily) not yet created, so do that now... */
    Scheme_Hash_Table *ht;
    int i;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    for (i = pt->num_provides; i--; ) {
      scheme_hash_set(ht, pt->provides[i], scheme_make_integer(i));
    }
    pt->ht = ht;
  }
}

void scheme_add_binding_copy(Scheme_Object *o, Scheme_Object *from_o, Scheme_Object *phase)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;

  STX_ASSERT(SCHEME_STXP(o));
  STX_ASSERT(SCHEME_STXP(from_o));

  /* Passing an identifier as the "value" adds to the existing binding,
     instead of replacing it: */
  add_binding(stx->val, phase, extract_mark_set(stx, phase), from_o);
}

/******************** module-import bindings ********************/

Scheme_Object *scheme_make_top_level_module_context(Scheme_Object *phase)
{
  return scheme_make_module_context(scheme_false, NULL, scheme_new_multi_mark());
}

Scheme_Object *scheme_make_module_context(Scheme_Object *insp, 
                                          Scheme_Object *shift_or_shifts,
                                          Scheme_Object *intro_multi_mark)
{
  Scheme_Object *vec;
  Scheme_Object *multi_marks, *expr_mark;

  multi_marks = scheme_null;
  if (intro_multi_mark)
    multi_marks = scheme_make_pair(intro_multi_mark, multi_marks);

  /* Having both a multi mark and a plain mark means that the
     multi-mark can be stripped away to remove access to bindings,
     while the non-multi mark can track expansion history. */
  multi_marks = scheme_make_pair(scheme_new_multi_mark(), multi_marks);
  multi_marks = scheme_make_pair(new_module_context_mark(100), multi_marks);

  if (!shift_or_shifts)
    shift_or_shifts = scheme_null;
  else if (!SCHEME_PAIRP(shift_or_shifts) && !SCHEME_NULLP(shift_or_shifts))
    shift_or_shifts = scheme_make_pair(shift_or_shifts, scheme_null);

  expr_mark = scheme_new_mark(50);

  /* A module context consists of
       - A set of multi-marks that corresponds to the module body;
         the set includes base mark and potentially a mark
         that applies to anything introduced into the module
       - A phase
       - An inspector
       - A list of module-index shifts
       - A mark that is specific to introduction
       - A mark that corresponds to expressions in the module
         body at a given phase; the expression mark effectively
         draws a line between bindings and uses, where the mark
         does not apply to bindings or compile-time relative to the
         phase
  */

  vec = scheme_make_vector(6, NULL);
  SCHEME_VEC_ELS(vec)[0] = multi_marks;
  SCHEME_VEC_ELS(vec)[1] = scheme_make_integer(0);
  SCHEME_VEC_ELS(vec)[2] = insp;
  SCHEME_VEC_ELS(vec)[3] = shift_or_shifts;
  SCHEME_VEC_ELS(vec)[4] = intro_multi_mark;
  SCHEME_VEC_ELS(vec)[5] = expr_mark;

  return vec;
}

Scheme_Mark_Set *scheme_module_context_marks(Scheme_Object *mc)
{
  Scheme_Object *multi_marks = SCHEME_VEC_ELS(mc)[0], *mark;
  Scheme_Object *phase = SCHEME_VEC_ELS(mc)[1];
  Scheme_Mark_Set *marks = empty_mark_set;

  while (!SCHEME_NULLP(multi_marks)) {
    mark = SCHEME_CAR(multi_marks);
    if (!SCHEME_MARKP(mark))
      mark = extract_single_mark(mark, phase);
    marks = mark_set_set(marks, mark, scheme_true);
    multi_marks = SCHEME_CDR(multi_marks);
  }

  return marks;
}

Scheme_Object *scheme_module_context_frame_marks(Scheme_Object *mc)
{
  Scheme_Object *marks, *expr_mark;

  marks = (Scheme_Object *)scheme_module_context_marks(mc);
  
  expr_mark = SCHEME_VEC_ELS(mc)[5];
  if (SCHEME_FALSEP(expr_mark))
    return marks;
  else
    return scheme_make_pair(marks, expr_mark);
}

Scheme_Object *scheme_module_context_expression_frame_marks(Scheme_Object *mc)
{
  Scheme_Object *expr_mark;
  
  expr_mark = SCHEME_VEC_ELS(mc)[5];
  if (SCHEME_FALSEP(expr_mark))
    return NULL;
  else
    return scheme_make_pair(scheme_false, expr_mark);
}

Scheme_Object *scheme_module_context_inspector(Scheme_Object *mc)
{
  return SCHEME_VEC_ELS(mc)[2];
}

void scheme_module_context_add_mapped_symbols(Scheme_Object *mc, Scheme_Hash_Table *mapped)
{
  add_marks_mapped_names(scheme_module_context_marks(mc), mapped);
}

Scheme_Object *scheme_module_context_at_phase(Scheme_Object *mc, Scheme_Object *phase, int for_expr)
{
  Scheme_Object *vec, *expr_mark;

  if (SAME_OBJ(SCHEME_VEC_ELS(mc)[1], phase))
    return mc;

  vec = scheme_make_vector(6, NULL);
  SCHEME_VEC_ELS(vec)[0] = SCHEME_VEC_ELS(mc)[0];
  SCHEME_VEC_ELS(vec)[1] = phase;
  SCHEME_VEC_ELS(vec)[2] = SCHEME_VEC_ELS(mc)[2];
  SCHEME_VEC_ELS(vec)[3] = SCHEME_VEC_ELS(mc)[3];
  SCHEME_VEC_ELS(vec)[4] = SCHEME_VEC_ELS(mc)[4];
  if (for_expr && SCHEME_TRUEP(SCHEME_VEC_ELS(mc)[5]))
    expr_mark = scheme_new_mark((SCHEME_INT_VAL(phase) ? 53 : 51));
  else
    expr_mark = scheme_false;
  SCHEME_VEC_ELS(vec)[5] = expr_mark;

  return vec;
}

Scheme_Object *scheme_stx_add_module_context(Scheme_Object *stx, Scheme_Object *mc)
{
  Scheme_Object *multi_marks = SCHEME_VEC_ELS(mc)[0];

  while (!SCHEME_NULLP(multi_marks)) {
    stx = scheme_stx_add_mark(stx,
                              SCHEME_CAR(multi_marks),
                              scheme_make_integer(0));
    multi_marks = SCHEME_CDR(multi_marks);
  }

  stx = scheme_stx_add_shifts(stx, SCHEME_VEC_ELS(mc)[3]);

  return stx;
}

Scheme_Object *scheme_stx_add_module_frame_context(Scheme_Object *stx, Scheme_Object *mc)
{
  Scheme_Object *expr_mark = SCHEME_VEC_ELS(mc)[5];

  stx = scheme_stx_add_module_context(stx, mc);

  if (!SCHEME_FALSEP(expr_mark))
    stx = scheme_stx_add_mark(stx, expr_mark, SCHEME_VEC_ELS(mc)[1]);

  return stx;
}

Scheme_Object *scheme_stx_introduce_to_module_context(Scheme_Object *stx, Scheme_Object *mc)
{
  Scheme_Object *multi_mark;

  STX_ASSERT(SCHEME_VECTORP(mc));

  multi_mark = SCHEME_VEC_ELS(mc)[4];
  if (SCHEME_FALSEP(multi_mark))
    scheme_signal_error("internal error: cannot introduce to a module context without an introduction mark");
  
  return scheme_stx_add_mark(stx, multi_mark, scheme_make_integer(0));
}

Scheme_Object *scheme_stx_add_module_expression_context(Scheme_Object *stx, Scheme_Object *mc)
{
  Scheme_Object *expr_mark = SCHEME_VEC_ELS(mc)[5];

  if (SCHEME_TRUEP(expr_mark))
    return scheme_stx_add_mark(stx, expr_mark, SCHEME_VEC_ELS(mc)[1]);
  else
    return stx;
}

void scheme_extend_module_context(Scheme_Object *mc,          /* (vector <mark-set> <phase> <inspector> ...) */
                                  Scheme_Object *ctx,         /* binding context (as stx) or NULL */
                                  Scheme_Object *modidx,      /* actual source module */
                                  Scheme_Object *localname,   /* name in local context */
                                  Scheme_Object *exname,      /* name in definition context  */
                                  Scheme_Object *nominal_mod, /* nominal source module */
                                  Scheme_Object *nominal_ex,  /* nominal import before local renaming */
                                  intptr_t mod_phase,         /* phase of source defn */
                                  Scheme_Object *src_phase,   /* nominal import phase */
                                  Scheme_Object *nom_phase,   /* nominal export phase */
                                  int skip_marshal)           /* 1 => don't save on marshal; reconstructed via unmarshal info*/
{
  Scheme_Mark_Set *marks;
  
  if (ctx)
    marks = extract_mark_set((Scheme_Stx *)ctx, SCHEME_VEC_ELS(mc)[1]);
  else
    marks = scheme_module_context_marks(mc);
  
  do_add_module_binding(marks, localname, SCHEME_VEC_ELS(mc)[1],
                        modidx, exname, scheme_make_integer(mod_phase),
                        SCHEME_VEC_ELS(mc)[2],
                        nominal_mod, nominal_ex,
                        src_phase, nom_phase,
                        skip_marshal);
}

void scheme_extend_module_context_with_shared(Scheme_Object *mc, /* (vector <mark> <phase> <inspector> <shifts>) or <phase> */
                                              Scheme_Object *modidx,      
                                              Scheme_Module_Phase_Exports *pt, 
                                              Scheme_Object *unmarshal_info,
                                              Scheme_Object *src_phase, /* nominal import phase */
                                              Scheme_Object *context,
                                              int save_unmarshal,
                                              Scheme_Object *replace_at)
{
  Scheme_Object *phase, *pes;
  Scheme_Mark_Set *marks;

  if (SCHEME_VECTORP(mc))
    phase = SCHEME_VEC_ELS(mc)[1];
  else
    phase = mc;

  if (context)
    marks = extract_mark_set((Scheme_Stx *)context, phase);
  else
    marks = scheme_module_context_marks(mc);

  if (SCHEME_FALSEP(unmarshal_info))
    unmarshal_info = pt->phase_index;
  else
    unmarshal_info = scheme_make_pair(pt->phase_index, unmarshal_info);

  pes = scheme_make_vector(4, NULL);
  SCHEME_VEC_ELS(pes)[0] = modidx;
  SCHEME_VEC_ELS(pes)[1] = (Scheme_Object *)pt;
  SCHEME_VEC_ELS(pes)[2] = src_phase;
  SCHEME_VEC_ELS(pes)[3] = unmarshal_info;

  if (replace_at) {
    SCHEME_BINDING_VAL(SCHEME_CAR(replace_at)) = pes;
  } else {
    add_binding(NULL, phase, marks, pes);
  }
}

void scheme_save_module_context_unmarshal(Scheme_Object *mc, Scheme_Object *info)
{
  /* TODO */
}

XFORM_NONGCING static Scheme_Hash_Table *extract_export_registry(Scheme_Object *shifts)
{
  Scheme_Object *l, *a;

  l = shifts;
  if (SCHEME_VECTORP(l))
    l = SCHEME_VEC_ELS(l)[0];
  while (!SCHEME_NULLP(l)) {
    a = SCHEME_CAR(l);
    if (SCHEME_VECTORP(a)) {
      a = SCHEME_VEC_ELS(a)[2];
      if (!SCHEME_FALSEP(a))
        return (Scheme_Hash_Table *)a;
    }
    l = SCHEME_CDR(l);
  }

  return NULL;
}

static void unmarshal_module_context_additions(Scheme_Stx *stx, Scheme_Object *vec, Scheme_Mark_Set *marks, Scheme_Object *replace_at)
{
  Scheme_Object *req_modidx, *modidx, *unmarshal_info, *context, *src_phase, *pt_phase, *bind_phase;
  Scheme_Hash_Table *export_registry;

  req_modidx = SCHEME_VEC_ELS(vec)[0];
  
  if (stx) {
    modidx = apply_modidx_shifts(stx->shifts, req_modidx, NULL);
    export_registry = extract_export_registry(stx->shifts);
  } else {
    modidx = req_modidx;
    export_registry = NULL;
  }

  src_phase = SCHEME_VEC_ELS(vec)[1];
  unmarshal_info = SCHEME_VEC_ELS(vec)[2];
  if (SCHEME_PAIRP(unmarshal_info)) {
    pt_phase = SCHEME_CAR(unmarshal_info);
    unmarshal_info = SCHEME_CDR(unmarshal_info);
  } else {
    pt_phase = unmarshal_info;
    unmarshal_info = scheme_false;
  }

  SCHEME_VEC_ELS(vec)[0] = scheme_false;
  SCHEME_VEC_ELS(vec)[1] = scheme_false;
  SCHEME_VEC_ELS(vec)[2] = scheme_false;

  if (SCHEME_FALSEP(src_phase) || SCHEME_FALSEP(pt_phase))
    bind_phase = scheme_false;
  else
    bind_phase = scheme_bin_plus(src_phase, pt_phase);

  context = scheme_datum_to_syntax(scheme_false, scheme_false, scheme_false, 0, 0);
  context = scheme_stx_adjust_marks(context, marks, bind_phase, SCHEME_STX_ADD);

  scheme_do_module_context_unmarshal(modidx, req_modidx, context,
                                     bind_phase, pt_phase, src_phase,
                                     unmarshal_info, export_registry,
                                     replace_at);
}

Scheme_Object *scheme_module_context_to_stx(Scheme_Object *mc, int track_expr, Scheme_Object *orig_src)
{
  Scheme_Object *plain, *o, *for_intro, *for_expr, *vec;

  plain = scheme_datum_to_syntax(scheme_true, scheme_false, scheme_false, 0, 0);

  if (orig_src)
    o = scheme_datum_to_syntax(scheme_true, scheme_false, orig_src, 0, 0);
  else
    o = scheme_stx_add_module_context(plain, mc);

  if (!track_expr)
    return o;

  if (SCHEME_TRUEP(SCHEME_VEC_ELS(mc)[4])
      || SCHEME_TRUEP(SCHEME_VEC_ELS(mc)[5])) {
    /* Keep track of expression & intro mark separately: */
    for_intro = scheme_stx_introduce_to_module_context(plain, mc);
    for_expr = scheme_stx_add_module_expression_context(plain, mc);
    vec = scheme_make_vector(3, NULL);
    SCHEME_VEC_ELS(vec)[0] = o;
    SCHEME_VEC_ELS(vec)[1] = for_intro;
    SCHEME_VEC_ELS(vec)[2] = for_expr;
    return scheme_datum_to_syntax(vec, scheme_false, scheme_false, 0, 0);
  } else
    return o;
}

Scheme_Object *scheme_stx_to_module_context(Scheme_Object *_stx)
{
  Scheme_Stx *stx = (Scheme_Stx *)_stx;
  Scheme_Object *vec, *shifts, *a, *multi_marks, *phase = scheme_make_integer(0);
  Scheme_Object *expr_mark = scheme_false, *intro_mark;

  if (SCHEME_VECTORP(stx->val)) {
    intro_mark = SCHEME_VEC_ELS(stx->val)[1];
    if (SCHEME_VEC_SIZE(stx->val) > 2)
      expr_mark = SCHEME_VEC_ELS(stx->val)[2];
    stx = (Scheme_Stx *)(SCHEME_VEC_ELS(stx->val)[0]);
  } else
    intro_mark = NULL;

  shifts = stx->shifts;
  if (SCHEME_VECTORP(shifts))
    shifts = SCHEME_VEC_ELS(shifts)[0];

  multi_marks = scheme_null;
  for (a = stx->marks->multi_marks; !SCHEME_NULLP(a); a = SCHEME_CDR(a)) {
    multi_marks = scheme_make_pair(SCHEME_CAR(SCHEME_CAR(a)), multi_marks);
    phase = SCHEME_CDR(SCHEME_CAR(a));
  }

  if (SCHEME_TRUEP(expr_mark)
      && mark_set_count(((Scheme_Stx *)expr_mark)->marks->single_marks) == 1) {
    Scheme_Object *key, *val;
    intptr_t i;
    i = mark_set_next(((Scheme_Stx *)expr_mark)->marks->single_marks, -1);
    mark_set_index(((Scheme_Stx *)expr_mark)->marks->single_marks, i, &key, &val);
    expr_mark = key;

    i = mark_set_next(stx->marks->single_marks, -1);
    while (i != -1) {
      mark_set_index(stx->marks->single_marks, i, &key, &val);
      if (!SAME_OBJ(key, expr_mark))
        multi_marks = scheme_make_pair(key, multi_marks);
      i = mark_set_next(stx->marks->single_marks, i);
    }
  } else if (mark_set_count(stx->marks->single_marks) == 1) {
    Scheme_Object *key, *val;
    intptr_t i;
    i = mark_set_next(stx->marks->single_marks, -1);
    mark_set_index(stx->marks->single_marks, i, &key, &val);
    expr_mark = key;
  }

  if (intro_mark) {
    stx = (Scheme_Stx *)intro_mark;
    if (SCHEME_PAIRP(stx->marks->multi_marks)) {
      intro_mark = SCHEME_CAR(SCHEME_CAR(stx->marks->multi_marks));
    } else
      intro_mark = scheme_false;
  } else
    intro_mark = scheme_false;
  
  vec = scheme_make_vector(6, NULL);
  SCHEME_VEC_ELS(vec)[0] = multi_marks;
  SCHEME_VEC_ELS(vec)[1] = phase;
  SCHEME_VEC_ELS(vec)[2] = scheme_false; /* not sure this is right */
  SCHEME_VEC_ELS(vec)[3] = shifts;
  SCHEME_VEC_ELS(vec)[4] = intro_mark;
  SCHEME_VEC_ELS(vec)[5] = expr_mark;

  return vec;
}

int scheme_stx_in_plain_module_context(Scheme_Object *stx, Scheme_Object *mc)
{
  Scheme_Mark_Set *marks;
  Scheme_Object *phase;

  marks = scheme_module_context_marks(mc);
  phase = SCHEME_VEC_ELS(mc)[1];

  return marks_equal(extract_mark_set((Scheme_Stx *)stx, phase), marks);
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

Scheme_Object *scheme_delayed_shift(Scheme_Object **o, intptr_t i)
{
  Scheme_Object *shift, *v;
  Resolve_Prefix *rp;

  shift = o[0];

  if (!shift) return scheme_false; /* happens only with corrupted .zo! */

  rp = (Resolve_Prefix *)o[1];

  v = rp->stxes[i];

  if (SCHEME_INTP(v)) {
    scheme_load_delayed_syntax(rp, i);
    v = rp->stxes[i];
  }

  v = scheme_stx_add_shift(v, shift);

  shift = SCHEME_VEC_ELS(shift)[3];
  if (!SCHEME_FALSEP(shift)) {
    /* need to propagate the inspector for dye packs, too */
    (void)set_false_insp((Scheme_Object *)v, shift, 0);
  }

  return v;
}

/*========================================================================*/
/*                           stx comparison                               */
/*========================================================================*/

int scheme_stx_could_bind(Scheme_Object *bind_id, Scheme_Object *ref_id, Scheme_Object *phase)
{
  Scheme_Stx *bind = (Scheme_Stx *)bind_id;
  Scheme_Stx *ref = (Scheme_Stx *)ref_id;
  
  if (!SAME_OBJ(ref->val, bind->val))
    return 0;

  return mark_subset(extract_mark_set(bind, phase),
                     extract_mark_set(ref, phase));
}

int scheme_stx_module_eq3(Scheme_Object *a, Scheme_Object *b, 
                          Scheme_Object *a_phase, Scheme_Object *b_phase)
{
  Scheme_Object *a_bind, *b_bind;

  STX_ASSERT(SCHEME_STXP(a));
  STX_ASSERT(SCHEME_STXP(b));
  
  a_bind = scheme_stx_lookup(a, a_phase);
  b_bind = scheme_stx_lookup(b, b_phase);

  if (SCHEME_SYMBOLP(a_bind) || SCHEME_SYMBOLP(b_bind)) {
    return SAME_OBJ(a_bind, b_bind);
  }

  if (SCHEME_FALSEP(a_bind) || SCHEME_FALSEP(b_bind)) {
    /* A `#f` binding can be equal to a vector that starts `#f` */
    if (SCHEME_FALSEP(a_bind))
      a = SCHEME_STX_VAL(a);
    else if (SCHEME_VECTORP(a_bind)
             && SCHEME_FALSEP(SCHEME_VEC_ELS(a_bind)[0])
             && SAME_OBJ(SCHEME_VEC_ELS(a_bind)[2], a_phase)) {
      a = SCHEME_VEC_ELS(a_bind)[1];
      a_bind = scheme_false;
    }

    if (SCHEME_FALSEP(b_bind))
      b = SCHEME_STX_VAL(b);
    else if (SCHEME_VECTORP(b_bind)
             && SCHEME_FALSEP(SCHEME_VEC_ELS(b_bind)[0])
             && SAME_OBJ(SCHEME_VEC_ELS(b_bind)[2], b_phase)) {
      b = SCHEME_VEC_ELS(b_bind)[1];
      b_bind = scheme_false;
    }

    if (SCHEME_FALSEP(a_bind) && SCHEME_FALSEP(b_bind))
      return SAME_OBJ(a, b);
    else
      return 0;
  }

  /* Comparison of names & definition phases is fast, so try that next: */
  if (!SAME_OBJ(SCHEME_VEC_ELS(a_bind)[1], SCHEME_VEC_ELS(b_bind)[1])
      || !SAME_OBJ(SCHEME_VEC_ELS(a_bind)[2], SCHEME_VEC_ELS(b_bind)[2])) {
    return 0;
  }

  /* Need to compare modidxs: */

  a_bind = scheme_module_resolve(SCHEME_VEC_ELS(a_bind)[0], 0);
  b_bind = scheme_module_resolve(SCHEME_VEC_ELS(b_bind)[0], 0);

  return SAME_OBJ(a_bind, b_bind);
}

int scheme_stx_module_eq2(Scheme_Object *a, Scheme_Object *b, Scheme_Object *phase)
{
  return scheme_stx_module_eq3(a, b, phase, phase);
}

int scheme_stx_module_eq(Scheme_Object *a, Scheme_Object *b, intptr_t phase)
{
  return scheme_stx_module_eq3(a, b, scheme_make_integer(phase), scheme_make_integer(phase));
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

  STX_ASSERT(SCHEME_STXP(_a));
  STX_ASSERT(SCHEME_STXP(_b));

  if (!SAME_OBJ(a->val, b->val))
    return 0;

  return marks_equal(extract_mark_set(a, a_phase), extract_mark_set(b, b_phase));
}

int scheme_stx_bound_eq(Scheme_Object *a, Scheme_Object *b, Scheme_Object *phase)
{
  return scheme_stx_env_bound_eq2(a, b, phase, phase);
}

Scheme_Object *scheme_stx_source_module(Scheme_Object *stx, int resolve, int source)
{
  /* Look for a self-modidx shift: */
  Scheme_Object *l = ((Scheme_Stx *)stx)->shifts, *a;
  Scheme_Object *srcmod = scheme_false, *chain_from = NULL;
  Scheme_Hash_Table *export_registry = NULL;

  if (SCHEME_VECTORP(l))
    l = SCHEME_VEC_ELS(l)[0];
  
  while (!SCHEME_NULLP(l)) {
    a = SCHEME_CAR(l);
    if (SCHEME_VECTORP(a)) {
      /* Phase shift:  */
      Scheme_Object *dest, *src, *er;

      src = SCHEME_VEC_ELS(a)[0];
      dest = SCHEME_VEC_ELS(a)[1];

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
          er = SCHEME_VEC_ELS(a)[2];
          if (SCHEME_TRUEP(er))
            export_registry = (Scheme_Hash_Table *)er;
        }
      }
    }

    l = SCHEME_CDR(l);
  }

  if (SCHEME_TRUEP(srcmod)) {
    if (resolve) {
      srcmod = scheme_module_resolve(srcmod, 0);
      if (export_registry && source) {
        a = scheme_hash_get(export_registry, srcmod);
        if (a)
          srcmod = ((Scheme_Module_Exports *)a)->modsrc;
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

static Scheme_Object *intern_tails(Scheme_Object *l, Scheme_Hash_Table *ht)
{
  Scheme_Object *r, *result;

  r = scheme_null;
  do {
    if (SCHEME_NULLP(l))
      result = scheme_null;
    else
      result = scheme_hash_get(ht, l);
    if (!result) {
      r = scheme_make_pair(SCHEME_CAR(l), r);
      l = SCHEME_CDR(l);
    }
  } while (!result);

  while (!SCHEME_NULLP(r)) {
    result = scheme_make_pair(SCHEME_CAR(r), result);
    l = scheme_make_pair(SCHEME_CAR(r), l);
    result = scheme_make_marshal_shared(result);
    scheme_hash_set(ht, l, result);
    r = SCHEME_CDR(r);
  }

  return result;
}

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif
#include "../gc2/my_qsort.c"
#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

typedef int (*compar_t)(const void *, const void *);

static int compare_marks(const void *a, const void *b)
{
  if (*(void **)a == *(void **)b)
    return 0;
  else if ((*(Scheme_Mark **)a)->id > (*(Scheme_Mark **)b)->id)
    return -1;
  else
    return 1;
}

static Scheme_Object *marks_to_sorted_list(Scheme_Mark_Set *marks)
{
  Scheme_Object **a, *r, *key, *val;
  intptr_t i, j = 0;

  i = mark_set_count(marks);
  a = MALLOC_N(Scheme_Object *, i);

  i = mark_set_next(marks, -1);
  while (i != -1) {
    mark_set_index(marks, i, &key, &val);
    a[j++] = key;
    i = mark_set_next(marks, i);
  }
  
  my_qsort(a, j, sizeof(Scheme_Object *), compare_marks);

  r = scheme_null;
  for (i = j; i--; ) {
    r = scheme_make_pair(a[i], r);
  }

  return r;
}

static Scheme_Object *drop_export_registries(Scheme_Object *shifts)
{
  Scheme_Object *l, *a, *vec, *p, *first = scheme_null, *last = NULL;

  if (SCHEME_VECTORP(shifts))
    shifts = SCHEME_VEC_ELS(shifts)[0];

  for (l = shifts; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    a = SCHEME_CAR(l);
    if (!SAME_OBJ(SCHEME_VEC_ELS(a)[0], SCHEME_VEC_ELS(a)[1])) {
      vec = scheme_make_vector(5, NULL);
      SCHEME_VEC_ELS(vec)[0] = SCHEME_VEC_ELS(a)[0];
      SCHEME_VEC_ELS(vec)[1] = SCHEME_VEC_ELS(a)[1];
      SCHEME_VEC_ELS(vec)[2] = scheme_false;
      SCHEME_VEC_ELS(vec)[3] = scheme_false;
      SCHEME_VEC_ELS(vec)[4] = SCHEME_VEC_ELS(a)[4];

      p = scheme_make_pair(vec, scheme_null);
      if (last)
        SCHEME_CDR(last) = p;
      else
        first = p;
      last = p;
    }
  }

  return first;
}

static Scheme_Object *multi_mark_to_vector(Scheme_Object *multi_mark, Scheme_Marshal_Tables *mt)
{
  Scheme_Object *vec;
  Scheme_Hash_Table *marks = (Scheme_Hash_Table *)multi_mark, *id_map;
  intptr_t i, j;

  if (!mt->identity_map) {
    id_map = scheme_make_hash_table(SCHEME_hash_ptr);
    mt->identity_map = id_map;
  }

  vec = scheme_hash_get(mt->identity_map, multi_mark);
  if (vec)
    return vec;

  vec = scheme_make_vector(2 * marks->count, NULL);
  j = 0;
  for (i = marks->size; i--; ) {
    if (marks->vals[i]) {
      SCHEME_VEC_ELS(vec)[j++] = marks->keys[i]; /* a phase */
      SCHEME_VEC_ELS(vec)[j++] = marks->vals[i]; /* a mark */
    }
  }

  vec = scheme_make_marshal_shared(vec);

  scheme_hash_set(mt->identity_map, multi_mark, vec);

  return vec;
}

static Scheme_Object *multi_marks_to_list(Scheme_Object *l, Scheme_Marshal_Tables *mt)
{
  Scheme_Object *p, *first = scheme_null, *last = NULL;

  while (!SCHEME_NULLP(l)) {
    p = scheme_make_pair(scheme_make_pair(multi_mark_to_vector(SCHEME_CAR(SCHEME_CAR(l)), mt),
                                          SCHEME_CDR(SCHEME_CAR(l))),
                         scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;
    l = SCHEME_CDR(l);
  }

  return first;
}

static Scheme_Object *wraps_to_datum(Scheme_Stx *stx, Scheme_Marshal_Tables *mt)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *shifts, *singles, *multi, *v, *vec;

  ht = mt->intern_map;
  if (!ht) {
    /* We need to compare a modidx using `eq?`, because shifting
       is based on `eq`ness. */
    ht = scheme_make_hash_table_equal_modix_eq();
    mt->intern_map = ht;
  }

  shifts = intern_tails(drop_export_registries(stx->shifts), ht);
  singles = intern_tails(marks_to_sorted_list(stx->marks->single_marks), ht);
  multi = intern_tails(multi_marks_to_list(stx->marks->multi_marks, mt), ht);

  vec = scheme_make_vector(3, NULL);
  SCHEME_VEC_ELS(vec)[0] = shifts;
  SCHEME_VEC_ELS(vec)[1] = singles;
  SCHEME_VEC_ELS(vec)[2] = multi;

  v = scheme_hash_get(ht, vec);
  if (!v) {
    v = scheme_make_marshal_shared(vec);
    scheme_hash_set(ht, vec, v);
  }

  return v;
}

static Scheme_Object *marshal_free_id_info(Scheme_Object *id_plus_phase, Scheme_Marshal_Tables *mt)
{
  Scheme_Stx *stx = (Scheme_Stx *)SCHEME_CAR(id_plus_phase);

  return scheme_make_pair(scheme_make_pair(stx->val, wraps_to_datum(stx, mt)),
                          SCHEME_CDR(id_plus_phase));
}

Scheme_Object *scheme_mark_marshal_content(Scheme_Object *m, Scheme_Marshal_Tables *mt)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *v, *l, *r, *l2, *tab, *marks, *free_id_info;
  intptr_t i, j;

  if (!mt->identity_map) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    mt->identity_map = ht;
  }

  v = scheme_hash_get(mt->identity_map, m);
  if (v)
    return v;

  v = ((Scheme_Mark *)m)->bindings;
  if (v) {
    ht = (Scheme_Hash_Table *)SCHEME_CAR(v);
    l2 = SCHEME_CDR(v);

    /* remove inspector, if any, from each mapping in the hash table,
       and adjust encoding of `free-identifier=?` equivalences */
    tab = scheme_make_vector(2 * ht->count, NULL);
    j = 0;
    for (i = ht->size; i--; ) {
      l = ht->vals[i];
      if (l) {
        r = scheme_null;
        while (l && !SCHEME_NULLP(l)) {
          v = SCHEME_BINDING_VAL(SCHEME_CAR(l));
          if (SCHEME_MPAIRP(v)) {
            /* has a `free-id=?` equivalence; the marshaled form of a mark's content
               cannot contain a syntax object, so we keep just the syntax object's symbol
               and marks */
            free_id_info = marshal_free_id_info(SCHEME_CDR(v), mt);
            v = SCHEME_CAR(v);
          } else
            free_id_info = NULL;
          if (SCHEME_PAIRP(v) && SCHEME_INSPECTORP(SCHEME_CAR(v)))
            v = SCHEME_CDR(v);
          if (free_id_info)
            v = scheme_box(scheme_make_pair(v, free_id_info)); /* a box indicates `free-id=?` info */
          marks = intern_tails(marks_to_sorted_list((Scheme_Mark_Set *)SCHEME_CAR(SCHEME_CAR(l))),
                               mt->intern_map);
          r = scheme_make_pair(scheme_make_pair(marks, v), r);
          l = SCHEME_CDR(l);
        }

        SCHEME_VEC_ELS(tab)[j++] = ht->keys[i];
        SCHEME_VEC_ELS(tab)[j++] = r;
      }
    }
    r = tab;

    /* convert marks+pes to mark + unmarshal request */
    for (l = l2; l && !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      v = SCHEME_CDR(SCHEME_CAR(l));
      if (SCHEME_VEC_SIZE(v) == 4) {
        l2 = scheme_make_vector(3, NULL);
        SCHEME_VEC_ELS(l2)[0] = SCHEME_VEC_ELS(v)[0];
        SCHEME_VEC_ELS(l2)[1] = SCHEME_VEC_ELS(v)[2];
        SCHEME_VEC_ELS(l2)[2] = SCHEME_VEC_ELS(v)[3];
        v = l2;
      } else if (SCHEME_VEC_SIZE(v) == 3) {
        if (SCHEME_TRUEP(SCHEME_VEC_ELS(v)[0])) {
          /* never unmarshaled, so keep it */
        } else
          v = NULL;
      } else {
        abort();
      }
      if (v) {
        marks = intern_tails(marks_to_sorted_list((Scheme_Mark_Set *)SCHEME_CAR(SCHEME_CAR(l))),
                             mt->intern_map);
        r = scheme_make_pair(scheme_make_pair(marks, v), r);
      }
    }

    if (SCHEME_MARK_FLAGS((Scheme_Mark*)m) & SCHEME_MARK_NONORIGINAL)
      r = scheme_make_pair(scheme_make_integer(1), r);
    else if (SCHEME_MARK_FLAGS((Scheme_Mark*)m) & SCHEME_MARK_MODULE_CONTEXT)
      r = scheme_make_pair(scheme_make_integer(2), r);

    v = r;
  } else if (SCHEME_MARK_FLAGS((Scheme_Mark*)m) & SCHEME_MARK_NONORIGINAL)
    v = scheme_make_integer(1);
  else if (SCHEME_MARK_FLAGS((Scheme_Mark*)m) & SCHEME_MARK_MODULE_CONTEXT)
    v = scheme_make_integer(2);
  else
    v = scheme_false;

  scheme_hash_set(mt->identity_map, m, v);

  return v;
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
          converted_wraps = wraps_to_datum(stx, mt);
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
      converted_wraps = wraps_to_datum(stx, mt);
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
      converted_wraps = wraps_to_datum(stx, mt);
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
/*                           datum->syntax                                */
/*========================================================================*/

#define return_NULL abort()
// return NULL

Scheme_Mark_Set *list_to_mark_set(Scheme_Object *l, Scheme_Unmarshal_Tables *ut)
{
  Scheme_Mark_Set *marks = NULL;
  Scheme_Object *r = scheme_null, *mark;

  while (!SCHEME_NULLP(l)) {
    if (!SCHEME_PAIRP(l)) return_NULL;
    marks = (Scheme_Mark_Set *)scheme_hash_get(ut->rns, l);
    if (marks)
      break;
    r = scheme_make_pair(l, r);
    l = SCHEME_CDR(l);
  }

  if (!marks) marks = empty_mark_set;

  while (!SCHEME_NULLP(r)) {
    l = SCHEME_CAR(r);

    mark = mark_unmarshal_content(SCHEME_CAR(l), ut);
    if (!mark) return_NULL;

    marks = mark_set_set(marks, mark, scheme_true);
    scheme_hash_set(ut->rns, l, (Scheme_Object *)marks);
    
    r = SCHEME_CDR(r);
  }

  return marks;
}

static Scheme_Hash_Table *vector_to_multi_mark(Scheme_Object *mht, Scheme_Unmarshal_Tables *ut)
{
  /* Convert multi-mark vector to hash table */
  intptr_t i, len;
  Scheme_Hash_Table *multi_mark;
  Scheme_Object *mark, *mm;

  if (!SCHEME_VECTORP(mht)) return_NULL;

  multi_mark = (Scheme_Hash_Table *)scheme_hash_get(ut->rns, mht);
  if (multi_mark) return multi_mark;

  multi_mark = scheme_make_hash_table(SCHEME_hash_ptr);

  len = SCHEME_VEC_SIZE(mht);
  if (len & 1) return_NULL;

  /* A multi-mark might refer back to itself via free-id=? info: */
  scheme_hash_set(ut->rns, mht, (Scheme_Object *)multi_mark);

  for (i = 0; i < len; i += 2) {
    if (!SCHEME_PHASEP(SCHEME_VEC_ELS(mht)[i]))
      return_NULL;
    mark = mark_unmarshal_content(SCHEME_VEC_ELS(mht)[i+1], ut);
    if (!mark) return_NULL;
    scheme_hash_set(multi_mark, SCHEME_VEC_ELS(mht)[i], mark);
    if (((Scheme_Mark *)mark)->owner_multi_mark)
      return_NULL;
    mm = scheme_make_pair((Scheme_Object *)multi_mark, SCHEME_VEC_ELS(mht)[i]);
    ((Scheme_Mark *)mark)->owner_multi_mark = mm;
  }
      
  return multi_mark;
}

Scheme_Object *unmarshal_multi_marks(Scheme_Object *multi_marks,
                                     Scheme_Unmarshal_Tables *ut)
{
  Scheme_Hash_Table *multi_mark;
  Scheme_Object *l;

  for (l = multi_marks; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (SCHEME_VECTORP(SCHEME_CAR(SCHEME_CAR(l)))) {
      multi_mark = vector_to_multi_mark(SCHEME_CAR(SCHEME_CAR(l)), ut);
      if (!multi_mark) return_NULL;
      SCHEME_CAR(SCHEME_CAR(l)) = (Scheme_Object *)multi_mark;
    } else {
      /* rest of list must be converted already, too */
      break;
    }
  }

  return multi_marks;
}


static Scheme_Object *datum_to_wraps(Scheme_Object *w,
                                     Scheme_Unmarshal_Tables *ut)
{
  Scheme_Mark_Table *mt;
  Scheme_Mark_Set *marks;
  Scheme_Object *l;

  l = scheme_hash_get(ut->rns, w);
  if (l) return l;

  if (!SCHEME_VECTORP(w)
      || ((SCHEME_VEC_SIZE(w) != 3)
          && (SCHEME_VEC_SIZE(w) != 4)))
    return_NULL;

  mt = MALLOC_ONE_TAGGED(Scheme_Mark_Table);
  mt->so.type = scheme_mark_table_type;

  marks = list_to_mark_set(SCHEME_VEC_ELS(w)[1], ut);
  if (!marks) return NULL;
  mt->single_marks = marks;

  l = unmarshal_multi_marks(SCHEME_VEC_ELS(w)[2], ut);
  if (!l) return NULL;
  mt->multi_marks = l;

  l = scheme_make_pair((Scheme_Object *)mt, SCHEME_VEC_ELS(w)[0]);
  scheme_hash_set(ut->rns, w, l);

  return l;
}

static Scheme_Object *validate_binding(Scheme_Object *p)
{
  if (SCHEME_SYMBOLP(p)) {
    /* Ok: local binding */
  } else if (SAME_TYPE(SCHEME_TYPE(p), scheme_module_index_type)) {
    /* Ok */
  } else if (SCHEME_PAIRP(p)) {
    Scheme_Object *midx;

    midx = SCHEME_CAR(p);
    if (SCHEME_TRUEP(midx)
        && !SAME_TYPE(SCHEME_TYPE(midx), scheme_module_index_type))
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
          if (!SCHEME_PHASE_SHIFTP(SCHEME_CAR(ap))) return_NULL;
          if (!SCHEME_PHASE_SHIFTP(SCHEME_CDR(ap))) return_NULL;
        } else if (!SCHEME_PHASE_SHIFTP(ap))
          return_NULL;
      } else
        return_NULL;

      /* nominal_exportname */
      ap = SCHEME_CDR(bp);
      if (!SCHEME_SYMBOLP(ap))
        return_NULL;
    }
  }

  return p;
}

static Scheme_Object *unmarshal_free_id_info(Scheme_Object *p, Scheme_Unmarshal_Tables *ut)
{
  Scheme_Object *o, *phase;

  phase = SCHEME_CDR(p);
  p = SCHEME_CAR(p);
  o = scheme_make_stx(SCHEME_CAR(p), NULL, NULL);
  p = datum_to_wraps(SCHEME_CDR(p), ut);
  if (!p) return_NULL;

  ((Scheme_Stx *)o)->marks = (Scheme_Mark_Table *)SCHEME_CAR(p);
  STX_ASSERT(SAME_TYPE(SCHEME_TYPE(((Scheme_Stx *)o)->marks), scheme_mark_table_type));
  ((Scheme_Stx *)o)->shifts = SCHEME_CDR(p);

  return scheme_make_pair(o, phase);
}

Scheme_Object *mark_unmarshal_content(Scheme_Object *box, Scheme_Unmarshal_Tables *ut)
{
  Scheme_Object *l = scheme_null, *l2, *r, *b, *m, *c, *free_id;
  Scheme_Hash_Table *ht;
  Scheme_Mark_Set *marks;
  intptr_t i, len;

  r = scheme_hash_get(ut->rns, box);
  if (r)
    return r;

  if (!SCHEME_BOXP(box)) return_NULL;
  c = SCHEME_BOX_VAL(box);

  /* Content as 1 or (cons 1 ...) is a non-original mark */
  if (SAME_OBJ(c, scheme_make_integer(1)))
    m = scheme_new_nonoriginal_mark(0);
  else if (SAME_OBJ(c, scheme_make_integer(2)))
    m = new_module_context_mark(0);
  else if (SCHEME_PAIRP(c) && SAME_OBJ(scheme_make_integer(1), SCHEME_CAR(c))) {
    c = SCHEME_CDR(c);
    m = scheme_new_nonoriginal_mark(0);
  } else if (SCHEME_PAIRP(c) && SAME_OBJ(scheme_make_integer(2), SCHEME_CAR(c))) {
    c = SCHEME_CDR(c);
    m = new_module_context_mark(0);
  } else
    m = scheme_new_mark(0);
  scheme_hash_set(ut->rns, box, m);
  /* Since we've created the mark before unmarshaling its content,
     cycles among marks are ok. */

  while (SCHEME_PAIRP(c)) {
    if (!SCHEME_PAIRP(SCHEME_CAR(c))) return_NULL;
    marks = list_to_mark_set(SCHEME_CAR(SCHEME_CAR(c)), ut);
    l = scheme_make_pair(scheme_make_pair((Scheme_Object *)marks,
                                          SCHEME_CDR(SCHEME_CAR(c))),
                         l);
    c = SCHEME_CDR(c);
  }

  ht = scheme_make_hash_table(SCHEME_hash_ptr);
  if (SCHEME_VECTORP(c)) {
    len = SCHEME_VEC_SIZE(c);
    if (len & 1) return_NULL;
    for (i = 0; i < len; i += 2) {
      l2 = SCHEME_VEC_ELS(c)[i+1];
      r = scheme_null;
      while (SCHEME_PAIRP(l2)) {
        marks = list_to_mark_set(SCHEME_CAR(SCHEME_CAR(l2)), ut);
        if (marks) {
          b = SCHEME_CDR(SCHEME_CAR(l2));
          if (SCHEME_BOXP(b)) {
            /* has `free-id=?` info */
            b = SCHEME_BOX_VAL(b);
            free_id = unmarshal_free_id_info(SCHEME_CDR(b), ut);
            if (!free_id) return_NULL;
            b = SCHEME_CAR(b);
          } else
            free_id = NULL;
          b = validate_binding(b);
          if (b) {
            if (free_id)
              b = scheme_make_mutable_pair(b, free_id);
            r = scheme_make_pair(scheme_make_pair((Scheme_Object *)marks, b),
                                 r);
          } else
            return_NULL;
        } else
          return_NULL;
        l2 = SCHEME_CDR(l2);
      }
      scheme_hash_set(ht, SCHEME_VEC_ELS(c)[i], r);
    }
    l = scheme_make_pair((Scheme_Object *)ht, l);
  } else
    l = scheme_make_pair((Scheme_Object *)ht, l);

  ((Scheme_Mark *)m)->bindings = l;

  return m;
}


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
    ((Scheme_Stx *)result)->marks = (Scheme_Mark_Table *)SCHEME_CAR(wraps);
    STX_ASSERT(SAME_TYPE(SCHEME_TYPE(((Scheme_Stx *)result)->marks), scheme_mark_table_type));
    ((Scheme_Stx *)result)->shifts = SCHEME_CDR(wraps);
  } else if (SCHEME_FALSEP((Scheme_Object *)stx_wraps)) {
    /* wraps already nulled */
  } else {
    /* Note: no propagation will be needed for SUBSTX */
    ((Scheme_Stx *)result)->marks = stx_wraps->marks;
    STX_ASSERT(SAME_TYPE(SCHEME_TYPE(((Scheme_Stx *)result)->marks), scheme_mark_table_type));
    ((Scheme_Stx *)result)->shifts = stx_wraps->shifts;
    if (SCHEME_VECTORP(((Scheme_Stx *)result)->shifts))
      ((Scheme_Stx *)result)->shifts = SCHEME_VEC_ELS(((Scheme_Stx *)result)->shifts)[0];
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
  Scheme_Object *key, *val;
  intptr_t i;

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

  /* Look for any non-original mark: */
  i = mark_set_next(stx->marks->single_marks, -1);
  while (i != -1) {
    mark_set_index(stx->marks->single_marks, i, &key, &val);

    if ((SCHEME_MARK_FLAGS((Scheme_Mark*)key) & SCHEME_MARK_NONORIGINAL))
      return scheme_false;
    
    i = mark_set_next(stx->marks->single_marks, i);
  }

  return scheme_true;
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
  Scheme_Object *r, *delta, *taint_p, *phase;
  int mode = SCHEME_STX_ADD;

  r = argv[0];
  if (argc > 1)
    mode = scheme_get_introducer_mode("syntax-delta-introducer", 1, argc, argv);

  if (!SCHEME_STXP(r))
    scheme_wrong_contract("syntax-delta-introducer", "syntax?", 0, argc, argv);

  delta = SCHEME_PRIM_CLOSURE_ELS(p)[0];
  taint_p = SCHEME_PRIM_CLOSURE_ELS(p)[1];
  phase = SCHEME_PRIM_CLOSURE_ELS(p)[2];

  r = scheme_stx_adjust_marks(r, (Scheme_Mark_Set *)delta, phase, mode);

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
  Scheme_Object *a[3], *key, *val, *src;
  Scheme_Object *phase;
  Scheme_Mark_Set *delta, *m2;
  intptr_t i;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(argv[0])))
    scheme_wrong_contract("make-syntax-delta-introducer", "identifier?", 0, argc, argv);
  if (!SCHEME_STXP(argv[1]) && !SCHEME_FALSEP(argv[1]))
    scheme_wrong_contract("make-syntax-delta-introducer", "(or/c syntax? #f)", 1, argc, argv);

  phase = extract_phase("make-syntax-delta-introducer", 2, argc, argv, scheme_make_integer(0), 1);

  delta = extract_mark_set((Scheme_Stx *)argv[0], phase);

  src = argv[1];
  if (!SCHEME_FALSEP(src)) {
    m2 = extract_mark_set((Scheme_Stx *)src, phase);
    if (!mark_subset(m2, delta))
      m2 = NULL;
  } else
    m2 = NULL;

  if (!m2) {
    src = scheme_stx_lookup_w_nominal(argv[1], phase, 1,
                                      NULL, NULL, &m2,
                                      NULL, NULL, NULL, NULL, NULL);
    if (SCHEME_FALSEP(src))
      m2 = NULL;
  }

  if (m2) {
    i = mark_set_next(m2, -1);
    while (i != -1) {
      mark_set_index(m2, i, &key, &val);
      if (mark_set_get(delta, key))
        delta = mark_set_set(delta, key, NULL);

      i = mark_set_next(m2, i);
    }
  }

  a[0] = (Scheme_Object *)delta;
  if (scheme_stx_is_clean(argv[0]))
    a[1] = scheme_false;
  else
    a[1] = scheme_true;
  a[2] = phase;

  return scheme_make_prim_closure_w_arity(delta_introducer, 3, a, "delta-introducer", 1, 2);
}

Scheme_Object *scheme_stx_binding_union(Scheme_Object *o, Scheme_Object *b, Scheme_Object *phase)
{
  Scheme_Mark_Set *current, *m2;
  Scheme_Object *key, *val;
  intptr_t i;
  int flags = 0;

  current = extract_mark_set((Scheme_Stx *)o, phase);
  m2 = extract_mark_set((Scheme_Stx *)b, phase);

  i = mark_set_next(m2, -1);
  while (i != -1) {
    mark_set_index(m2, i, &key, &val);
    if (!mark_set_get(current, key)) {
      o = scheme_stx_adjust_mark(o, key, phase, flags | SCHEME_STX_ADD);
      flags |= SCHEME_STX_MUTATE;
    }
    
    i = mark_set_next(m2, i);
  }

  return o;
}

static Scheme_Object *bound_eq(int argc, Scheme_Object **argv)
{
  Scheme_Object *phase;

  if (!SCHEME_STX_IDP(argv[0]))
    scheme_wrong_contract("bound-identifier=?", "identifier?", 0, argc, argv);
  if (!SCHEME_STX_IDP(argv[1]))
    scheme_wrong_contract("bound-identifier=?", "identifier?", 1, argc, argv);

  phase = extract_phase("bound-identifier=?", 2, argc, argv, scheme_make_integer(0), 0);

  return (scheme_stx_env_bound_eq2(argv[0], argv[1], phase, phase)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *do_module_eq(const char *who, int delta, int argc, Scheme_Object **argv)
{
  Scheme_Object *phase, *phase2;
  int v;

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

  v = scheme_stx_module_eq3(argv[0], argv[1], phase, phase2);

  return (v
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

  m = scheme_stx_lookup_w_nominal(a, phase, 0,
                                  NULL, NULL, NULL, NULL,
                                  &nom_mod, &nom_a,
                                  &src_phase_index,
                                  &nominal_src_phase);

  if (get_symbol) {
    if (SCHEME_VECTORP(m))
      return SCHEME_VEC_ELS(m)[1];
    else
      return SCHEME_STX_VAL(a);
  }

  if (SCHEME_FALSEP(m))
    return scheme_false;
  else if (SCHEME_SYMBOLP(m))
    return lexical_symbol;
  else {
    a = SCHEME_VEC_ELS(m)[1];
    mod_phase = SCHEME_VEC_ELS(m)[2];
    m = SCHEME_VEC_ELS(m)[0];

    if (SCHEME_FALSEP(m)) {
      /* loses information; improve API in the future? */
      return scheme_false;
    }

    return CONS(m, CONS(a, CONS(nom_mod, 
                                CONS(nom_a, 
                                     CONS(mod_phase,
                                          CONS(src_phase_index, 
                                               CONS(nominal_src_phase,
                                                    scheme_null)))))));
  }
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
  Scheme_Object *a = argv[0], *l;

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

  /* FIXME: implement pruning */

  return a;
}

static Scheme_Object *identifier_prune_to_module(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];
  Scheme_Object *shifts;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_STX_SYMBOLP(argv[0]))
    scheme_wrong_contract("identifier-prune-to-source-module", "identifier?", 0, argc, argv);

  shifts = stx->shifts;
  stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
  stx->shifts = shifts;

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

  return scheme_syntax_taint_disarm(argv[0], insp);
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
  SCHEME_VEC_ELS(vec)[2] = (Scheme_Object *)((Scheme_Stx *)stx)->marks;
  
  return vec;
}

/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_syntax.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_srcloc, mark_srcloc);
  GC_REG_TRAV(scheme_mark_type, mark_mark);
  GC_REG_TRAV(scheme_mark_table_type, mark_mark_table);
  GC_REG_TRAV(scheme_propagate_table_type, mark_propagate_table);
}

END_XFORM_SKIP;

#endif
