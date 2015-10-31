/*
  Racket
  Copyright (c) 2004-2015 PLT Design Inc.
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

ROSYM static Scheme_Object *module_symbol;
ROSYM static Scheme_Object *top_symbol;
ROSYM static Scheme_Object *macro_symbol;
ROSYM static Scheme_Object *local_symbol;
ROSYM static Scheme_Object *intdef_symbol;
ROSYM static Scheme_Object *use_site_symbol;

ROSYM static Scheme_Object *name_symbol;
ROSYM static Scheme_Object *context_symbol;
ROSYM static Scheme_Object *bindings_symbol;
ROSYM static Scheme_Object *matchp_symbol;
ROSYM static Scheme_Object *cycle_symbol;
ROSYM static Scheme_Object *free_symbol;
ROSYM static Scheme_Object *fallbacks_symbol;

READ_ONLY Scheme_Object *scheme_syntax_p_proc;

READ_ONLY Scheme_Hash_Tree *empty_hash_tree;
READ_ONLY Scheme_Scope_Table *empty_scope_table;
READ_ONLY Scheme_Scope_Table *empty_propagate_table;
READ_ONLY Scheme_Scope_Set *empty_scope_set;

ROSYM Scheme_Object *scheme_paren_shape_symbol;

READ_ONLY Scheme_Hash_Tree *scheme_source_stx_props;
READ_ONLY static Scheme_Hash_Tree *square_stx_props;
READ_ONLY static Scheme_Hash_Tree *curly_stx_props;

READ_ONLY static Scheme_Stx_Srcloc *empty_srcloc;

typedef struct Scheme_Scope {
  Scheme_Inclhash_Object iso; /* 0x1 => Scheme_Scope_With_Owner */
  mzlonglong id; /* low SCHEME_STX_SCOPE_KIND_SHIFT bits indicate kind */
  Scheme_Object *bindings; /* NULL, vector for one binding, hash table for multiple bindings,
                              or (rcons hash-table (rcons (cons scope-set pes-info) ... NULL));
                              each hash table maps symbols to (cons scope-set binding)
                              or (mlist (cons scope-set binding) ...) */
} Scheme_Scope;

/* For a scope that is for a particular phase within a set of phase-specific scopes: */
typedef struct Scheme_Scope_With_Owner {
  Scheme_Scope m;
  Scheme_Object *owner_multi_scope;
  Scheme_Object *phase;
} Scheme_Scope_With_Owner;

#define SCHEME_SCOPE_FLAGS(m) MZ_OPT_HASH_KEY(&(m)->iso)
#define SCHEME_SCOPE_HAS_OWNER(m) (SCHEME_SCOPE_FLAGS(m) & 0x1)

#define SCHEME_SCOPE_KIND(m) (((Scheme_Scope *)(m))->id & SCHEME_STX_SCOPE_KIND_MASK)

READ_ONLY static Scheme_Object *root_scope;

/* For lazy propagation of scope changes: */
typedef struct Scheme_Propagate_Table {
  Scheme_Scope_Table st; /* Maps scopes to actions, instead of just holding a set of scopes;
                            action compositions can be collased to an action:
                            SCHEME_STX_ADD + SCHEME_STX_FLIP = SCHEME_STX_REMOVE, etc. */
  Scheme_Scope_Table *prev; /* points to old scope table as a shortcut;
                               the old table plus these actions equals
                               the owning object's current table */
  Scheme_Object *phase_shift; /* <number> or (box <nnumber>); latter converts only <nnumber> to #f */
} Scheme_Propagate_Table;

THREAD_LOCAL_DECL(static mzlonglong scope_counter);
THREAD_LOCAL_DECL(static Scheme_Object *last_phase_shift);
THREAD_LOCAL_DECL(static Scheme_Object *nominal_ipair_cache);
THREAD_LOCAL_DECL(static Scheme_Bucket_Table *taint_intern_table);
THREAD_LOCAL_DECL(static struct Binding_Cache_Entry *binding_cache_table);
THREAD_LOCAL_DECL(static intptr_t binding_cache_pos);
THREAD_LOCAL_DECL(static intptr_t binding_cache_len);
THREAD_LOCAL_DECL(static Scheme_Scope_Set *recent_scope_sets[2][NUM_RECENT_SCOPE_SETS]);
THREAD_LOCAL_DECL(static int recent_scope_sets_pos[2]);

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
static Scheme_Object *free_eq(int argc, Scheme_Object **argv);
static Scheme_Object *free_trans_eq(int argc, Scheme_Object **argv);
static Scheme_Object *free_templ_eq(int argc, Scheme_Object **argv);
static Scheme_Object *free_label_eq(int argc, Scheme_Object **argv);
static Scheme_Object *free_binding(int argc, Scheme_Object **argv);
static Scheme_Object *free_trans_binding(int argc, Scheme_Object **argv);
static Scheme_Object *free_templ_binding(int argc, Scheme_Object **argv);
static Scheme_Object *free_label_binding(int argc, Scheme_Object **argv);
static Scheme_Object *free_binding_symbol(int argc, Scheme_Object **argv);
static Scheme_Object *identifier_prune(int argc, Scheme_Object **argv);
static Scheme_Object *identifier_prune_to_module(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_src_module(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_arm(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_disarm(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_rearm(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_taint(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_debug_info(int argc, Scheme_Object **argv);

static Scheme_Object *set_false_insp(Scheme_Object *o, Scheme_Object *false_insp, int *mutate);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

XFORM_NONGCING static int is_armed(Scheme_Object *v);
static Scheme_Object *add_taint_to_stx(Scheme_Object *o, int *mutate);

static void unmarshal_module_context_additions(Scheme_Stx *stx, Scheme_Object *vec, Scheme_Scope_Set *scopes, Scheme_Object *replace_at);

static Scheme_Object *make_unmarshal_info(Scheme_Object *phase, Scheme_Object *prefix, Scheme_Object *excepts);
XFORM_NONGCING static Scheme_Object *extract_unmarshal_phase(Scheme_Object *unmarshal_info);
XFORM_NONGCING static Scheme_Object *extract_unmarshal_prefix(Scheme_Object *unmarshal_info);
static Scheme_Hash_Tree *extract_unmarshal_excepts(Scheme_Object *unmarshal_info);
static Scheme_Object *unmarshal_lookup_adjust(Scheme_Object *sym, Scheme_Object *pes);
static Scheme_Object *unmarshal_key_adjust(Scheme_Object *sym, Scheme_Object *pes);

XFORM_NONGCING static int scopes_equal(Scheme_Scope_Set *a, Scheme_Scope_Set *b);
static Scheme_Object *remove_at_scope_list(Scheme_Object *l, Scheme_Object *p);
static Scheme_Object *add_to_scope_list(Scheme_Object *l, Scheme_Object *p);

static Scheme_Object *wraps_to_datum(Scheme_Stx *stx, Scheme_Marshal_Tables *mt);
static Scheme_Object *scope_unmarshal_content(Scheme_Object *c, struct Scheme_Unmarshal_Tables *utx);

static Scheme_Object *scopes_to_sorted_list(Scheme_Scope_Set *scopes);
static void sort_vector_symbols(Scheme_Object *vec);

static void sort_scope_array(Scheme_Object **a, intptr_t count);
static void sort_symbol_array(Scheme_Object **a, intptr_t count);
static void sort_number_array(Scheme_Object **a, intptr_t count);

XFORM_NONGCING static void extract_module_binding_parts(Scheme_Object *l,
                                                        Scheme_Object *phase,
                                                        Scheme_Object **_insp_desc,
                                                        Scheme_Object **_modidx,
                                                        Scheme_Object **_exportname,
                                                        Scheme_Object **_nominal_modidx,
                                                        Scheme_Object **_mod_phase,
                                                        Scheme_Object **_nominal_name,
                                                        Scheme_Object **_src_phase,
                                                        Scheme_Object **_nominal_src_phase);

static Scheme_Object *stx_debug_info(Scheme_Stx *stx, Scheme_Object *phase, Scheme_Object *seen, int all_bindings);

static void init_binding_cache(void);
XFORM_NONGCING static void clear_binding_cache(void);
XFORM_NONGCING static void clear_binding_cache_for(Scheme_Object *sym);
XFORM_NONGCING static void clear_binding_cache_stx(Scheme_Stx *stx);

#define CONS scheme_make_pair
#define ICONS scheme_make_pair

/* "substx" means that we need to propagate marks to nested syntax objects */
#define HAS_SUBSTX(obj) (SCHEME_PAIRP(obj) || SCHEME_VECTORP(obj) || SCHEME_BOXP(obj) || prefab_p(obj) || SCHEME_HASHTRP(obj))
#define HAS_CHAPERONE_SUBSTX(obj) (HAS_SUBSTX(obj) || (SCHEME_NP_CHAPERONEP(obj) && HAS_SUBSTX(SCHEME_CHAPERONE_VAL(obj))))

#define SCHEME_INSPECTORP(obj) SAME_TYPE(scheme_inspector_type, SCHEME_TYPE(obj))
#define SCHEME_INSPECTOR_DESCP(obj) (SCHEME_INSPECTORP(obj) || SCHEME_SYMBOLP(obj))
#define SCHEME_MODIDXP(l) SAME_TYPE(SCHEME_TYPE(l), scheme_module_index_type)
#define SCHEME_PHASEP(a) (SCHEME_INTP(a) || SCHEME_BIGNUMP(a) || SCHEME_FALSEP(a))

#define SCHEME_PHASE_SHIFTP(a) (SCHEME_PHASEP(a) || (SCHEME_BOXP(a) && SCHEME_PHASEP(SCHEME_BOX_VAL(a))))
/* #f as a phase shift is an alias for (box 0) */

#define SCHEME_MULTI_SCOPEP(o) SCHEME_HASHTP(o)
#define SCHEME_SCOPEP(x) (SAME_TYPE(SCHEME_TYPE(x), scheme_scope_type))

#define SCHEME_TL_MULTI_SCOPEP(o) (MZ_OPT_HASH_KEY(&(((Scheme_Hash_Table *)o)->iso)) & 0x2)

/* A hash tabel for a multi scope has meta information mapped from void: */
#define MULTI_SCOPE_METAP(v) SCHEME_VOIDP(v)
#define MULTI_SCOPE_META_HASHEDP(v) SCHEME_MPAIRP(v)

/* Represent fallback as vectors, either of size 2 (for normal scope
   sets) or size 4 (for sets of propagation instructions, because adding
   a fallback layer is an action): */
#define SCHEME_FALLBACKP(o) SCHEME_VECTORP(o)
#define SCHEME_FALLBACK_QUADP(o) (SCHEME_VEC_SIZE(o) == 4)
#define SCHEME_FALLBACK_FIRST(o) (SCHEME_VEC_ELS(o)[0])
#define SCHEME_FALLBACK_REST(o) (SCHEME_VEC_ELS(o)[1])
#define SCHEME_FALLBACK_SCOPE(o) (SCHEME_VEC_ELS(o)[2])
#define SCHEME_FALLBACK_PHASE(o) (SCHEME_VEC_ELS(o)[3])

/* Bindings of the form "everything from module" */
#define PES_UNMARSHAL_DESCP(v) (SCHEME_VEC_SIZE(v) == 4)
#define PES_BINDINGP(v)        (SCHEME_VEC_SIZE(v) == 5)

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

#define MUTATE_STX_OBJ        1
#define MUTATE_STX_SCOPE_TABLE 2
#define MUTATE_STX_PROP_TABLE 4

#if 0
int stx_alloc_obj, stx_skip_alloc_obj;
int stx_alloc_scope_table, stx_skip_alloc_scope_table;
int stx_alloc_prop_table, stx_skip_alloc_prop_table;
# define COUNT_MUTATE_ALLOCS(x) x
#else
# define COUNT_MUTATE_ALLOCS(x) /* empty */
#endif

/* A `taints' field is one of
    - NULL => clean
    - #t => tainted, and taint propagated to children, if any)
    - (void) => tainted, and taint needs to be propagated to children
    - <insp> => clean, but inspector needs to be proagated to children
    - (list <insp/#f> <insp> ...+) [interned] => armed; first inspector is to propagate */

#define STX_ASSERT(x) MZ_ASSERT(x)

static Scheme_Object *make_vector3(Scheme_Object *a, Scheme_Object *b, Scheme_Object *c)
{
  Scheme_Object *vec;

  vec = scheme_make_vector(3, NULL);
  SCHEME_VEC_ELS(vec)[0] = a;
  SCHEME_VEC_ELS(vec)[1] = b;
  SCHEME_VEC_ELS(vec)[2] = c;

  return vec;
}

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
  REGISTER_SO(empty_scope_table);
  REGISTER_SO(empty_propagate_table);
  REGISTER_SO(empty_scope_set);
  empty_hash_tree = scheme_make_hash_tree(0);
  empty_scope_set = (Scheme_Scope_Set *)scheme_make_hash_tree_set(0);
  empty_scope_table = MALLOC_ONE_TAGGED(Scheme_Scope_Table);
  empty_scope_table->so.type = scheme_scope_table_type;
  empty_scope_table->simple_scopes = empty_scope_set;
  empty_scope_table->multi_scopes = scheme_null;
  empty_propagate_table = (Scheme_Scope_Table *)MALLOC_ONE_TAGGED(Scheme_Propagate_Table);
  memcpy(empty_propagate_table, empty_scope_table, sizeof(Scheme_Scope_Table));
  empty_propagate_table->simple_scopes = (Scheme_Scope_Set *)empty_hash_tree;
  empty_propagate_table->so.type = scheme_propagate_table_type;
  ((Scheme_Propagate_Table *)empty_propagate_table)->phase_shift = scheme_make_integer(0);
  ((Scheme_Propagate_Table *)empty_propagate_table)->prev = NULL;

  REGISTER_SO(scheme_syntax_p_proc);
  o = scheme_make_folding_prim(syntax_p, "syntax?", 1, 1, 1);
  scheme_syntax_p_proc = o;
  SCHEME_PRIM_PROC_FLAGS(o) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("syntax?", o, env);

  GLOBAL_FOLDING_PRIM("syntax->datum", syntax_to_datum, 1, 1, 1, env);
  GLOBAL_IMMED_PRIM("datum->syntax", datum_to_syntax, 2, 5, env);
  
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
  GLOBAL_IMMED_PRIM("free-identifier=?"                , free_eq                 , 2, 4, env);
  GLOBAL_IMMED_PRIM("free-transformer-identifier=?"    , free_trans_eq           , 2, 2, env);
  GLOBAL_IMMED_PRIM("free-template-identifier=?"       , free_templ_eq           , 2, 2, env);
  GLOBAL_IMMED_PRIM("free-label-identifier=?"          , free_label_eq           , 2, 2, env);

  GLOBAL_IMMED_PRIM("identifier-binding"               , free_binding            , 1, 2, env);
  GLOBAL_IMMED_PRIM("identifier-transformer-binding"   , free_trans_binding      , 1, 2, env);
  GLOBAL_IMMED_PRIM("identifier-template-binding"      , free_templ_binding      , 1, 1, env);
  GLOBAL_IMMED_PRIM("identifier-label-binding"         , free_label_binding      , 1, 1, env);
  GLOBAL_IMMED_PRIM("identifier-prune-lexical-context" , identifier_prune          , 1, 2, env);
  GLOBAL_IMMED_PRIM("identifier-prune-to-source-module", identifier_prune_to_module, 1, 1, env);

  GLOBAL_IMMED_PRIM("identifier-binding-symbol"        , free_binding_symbol     , 1, 2, env);

  GLOBAL_NONCM_PRIM("syntax-source-module"             , syntax_src_module         , 1, 2, env);

  GLOBAL_FOLDING_PRIM("syntax-tainted?", syntax_tainted_p, 1, 1, 1, env);
  GLOBAL_IMMED_PRIM("syntax-arm"                 , syntax_arm                , 1, 3, env);
  GLOBAL_IMMED_PRIM("syntax-disarm"              , syntax_disarm             , 2, 2, env);
  GLOBAL_IMMED_PRIM("syntax-rearm"               , syntax_rearm              , 2, 3, env);
  GLOBAL_IMMED_PRIM("syntax-taint"               , syntax_taint              , 1, 1, env);

  GLOBAL_IMMED_PRIM("syntax-debug-info"          , syntax_debug_info         , 1, 3, env);

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

  REGISTER_SO(module_symbol);
  REGISTER_SO(top_symbol);
  REGISTER_SO(macro_symbol);
  REGISTER_SO(local_symbol);
  REGISTER_SO(intdef_symbol);
  REGISTER_SO(use_site_symbol);
  module_symbol = scheme_intern_symbol("module");
  top_symbol = scheme_intern_symbol("top");
  macro_symbol = scheme_intern_symbol("macro");
  local_symbol = scheme_intern_symbol("local");
  intdef_symbol = scheme_intern_symbol("intdef");
  use_site_symbol = scheme_intern_symbol("use-site");

  REGISTER_SO(name_symbol);
  REGISTER_SO(context_symbol);
  REGISTER_SO(bindings_symbol);
  REGISTER_SO(matchp_symbol);
  REGISTER_SO(cycle_symbol);
  REGISTER_SO(free_symbol);
  REGISTER_SO(fallbacks_symbol);
  name_symbol = scheme_intern_symbol("name");
  context_symbol = scheme_intern_symbol("context");
  bindings_symbol = scheme_intern_symbol("bindings");
  matchp_symbol = scheme_intern_symbol("match?");
  cycle_symbol = scheme_intern_symbol("cycle");
  free_symbol = scheme_intern_symbol("free-identifier=?");
  fallbacks_symbol = scheme_intern_symbol("fallbacks");

  REGISTER_SO(empty_srcloc);
  empty_srcloc = MALLOC_ONE_RT(Scheme_Stx_Srcloc);
#ifdef MZTAG_REQUIRED
  empty_srcloc->type = scheme_rt_srcloc;
#endif
  empty_srcloc->src = scheme_false;
  empty_srcloc->line = -1;
  empty_srcloc->col = -1;
  empty_srcloc->pos = -1;

  REGISTER_SO(root_scope);
  root_scope = scheme_new_scope(SCHEME_STX_MODULE_SCOPE);

  REGISTER_SO(scheme_paren_shape_symbol);
  scheme_paren_shape_symbol = scheme_intern_symbol("paren-shape");

  REGISTER_SO(scheme_source_stx_props);
  REGISTER_SO(square_stx_props);
  REGISTER_SO(curly_stx_props);
  scheme_source_stx_props = scheme_hash_tree_set(empty_hash_tree, source_symbol, scheme_true);
  square_stx_props = scheme_hash_tree_set(empty_hash_tree, scheme_paren_shape_symbol, scheme_make_char('['));
  curly_stx_props = scheme_hash_tree_set(empty_hash_tree, scheme_paren_shape_symbol, scheme_make_char('{'));
}

void scheme_init_stx_places(int initial_main_os_thread) {
  REGISTER_SO(taint_intern_table);
  taint_intern_table = scheme_make_weak_equal_table();

  init_binding_cache();
}

/*========================================================================*/
/*                       stx creation and maintenance                     */
/*========================================================================*/

Scheme_Object *scheme_make_stx(Scheme_Object *val, 
			       Scheme_Stx_Srcloc *srcloc,
			       Scheme_Hash_Tree *props)
{
  Scheme_Stx *stx;

  stx = MALLOC_ONE_TAGGED(Scheme_Stx);
  stx->iso.so.type = scheme_stx_type;
  STX_KEY(stx) = HAS_SUBSTX(val) ? STX_SUBSTX_FLAG : 0;
  stx->val = val;
  stx->srcloc = srcloc;
  stx->scopes = empty_scope_table;
  stx->u.to_propagate = NULL;
  stx->shifts = scheme_null;
  stx->props = props;

  return (Scheme_Object *)stx;
}

Scheme_Object *clone_stx(Scheme_Object *to, GC_CAN_IGNORE int *mutate)
/* the `mutate` argument tracks whether we can mutate `to` */
{
  Scheme_Stx *stx = (Scheme_Stx *)to;
  Scheme_Object *taints, *shifts;
  Scheme_Scope_Table *scopes;
  Scheme_Scope_Table *to_propagate;
  int armed;

  STX_ASSERT(SCHEME_STXP(to));

  if (mutate && (*mutate & MUTATE_STX_OBJ)) {
    COUNT_MUTATE_ALLOCS(stx_skip_alloc_obj++);
    return to;
  }

  taints = stx->taints;
  scopes = stx->scopes;
  shifts = stx->shifts;
  to_propagate = stx->u.to_propagate;
  armed = (STX_KEY(stx) & STX_ARMED_FLAG);

  stx = (Scheme_Stx *)scheme_make_stx(stx->val, 
                                      stx->srcloc,
                                      stx->props);

  stx->scopes = scopes;
  if (STX_KEY(stx) & STX_SUBSTX_FLAG) {
    stx->u.to_propagate = to_propagate;
    if (armed)
      STX_KEY(stx) |= STX_ARMED_FLAG;
  }
  stx->taints = taints;
  stx->shifts = shifts;

  if (mutate) {
    COUNT_MUTATE_ALLOCS(stx_alloc_obj++);
    *mutate |= MUTATE_STX_OBJ;
  }

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_make_stx_w_offset(Scheme_Object *val, 
					intptr_t line, intptr_t col, intptr_t pos, intptr_t span,
					Scheme_Object *src,
					Scheme_Hash_Tree *props)
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
  Scheme_Hash_Tree *ne, *oe;
  Scheme_Object *e1, *key, *val;
  mzlonglong i;

  STX_ASSERT(!origin || SCHEME_STX_SYMBOLP(origin));

  if (nstx->props)
    ne = nstx->props;
  else
    ne = empty_hash_tree;
  
  if (ostx->props) {
    if (SAME_OBJ(ostx->props, STX_SRCTAG)) {
      /* Drop 'source; will add 'origin. */
      oe = empty_hash_tree;
    } else {
      oe = ostx->props;

      /* Drop 'source and 'share; will add 'origin */
      oe = scheme_hash_tree_set(oe, source_symbol, NULL);
      oe = scheme_hash_tree_set(oe, share_symbol, NULL);
    }
  } else {
    /* Will add 'origin */
    oe = empty_hash_tree;
  }

  e1 = scheme_hash_tree_get(oe, origin_symbol);
  if (e1 && origin)
    oe = scheme_hash_tree_set(oe, origin_symbol, ICONS(origin, e1));
  else if (origin)
    oe = scheme_hash_tree_set(oe, origin_symbol, ICONS(origin, scheme_null));
    
  /* Merge ne and oe */

  if (SAME_OBJ(ne, empty_hash_tree))
    ne = oe;
  else if (ne->count < oe->count) {
    i = scheme_hash_tree_next(ne, -1);
    while (i != -1) {
      scheme_hash_tree_index(ne, i, &key, &val);
      e1 = scheme_hash_tree_get(oe, key);
      if (e1)
        oe = scheme_hash_tree_set(oe, key, ICONS(val, e1));
      else
        oe = scheme_hash_tree_set(oe, key, val);
      i = scheme_hash_tree_next(ne, i);
    }
    ne = oe;
  } else {
    i = scheme_hash_tree_next(oe, -1);
    while (i != -1) {
      scheme_hash_tree_index(oe, i, &key, &val);
      e1 = scheme_hash_tree_get(ne, key);
      if (e1)
        ne = scheme_hash_tree_set(ne, key, ICONS(e1, val));
      else
        ne = scheme_hash_tree_set(ne, key, val);
      i = scheme_hash_tree_next(oe, i);
    }
  }

  /* Clone nstx, keeping wraps, changing props to ne */
  nstx = (Scheme_Stx *)clone_stx((Scheme_Object *)nstx, NULL);
  nstx->props = ne;

  return (Scheme_Object *)nstx;
}

void scheme_stx_set(Scheme_Object *q_stx, Scheme_Object *val, Scheme_Object *context)
{
  clear_binding_cache_stx((Scheme_Stx *)q_stx);

  ((Scheme_Stx *)q_stx)->val = val;

  if (context) {
    ((Scheme_Stx *)q_stx)->scopes = ((Scheme_Stx *)context)->scopes;
    ((Scheme_Stx *)q_stx)->shifts = ((Scheme_Stx *)context)->shifts;
  } else {
    ((Scheme_Stx *)q_stx)->scopes = NULL;
    ((Scheme_Stx *)q_stx)->shifts = NULL;
  }

  ((Scheme_Stx *)q_stx)->u.to_propagate = NULL;
  ((Scheme_Stx *)q_stx)->taints = NULL;
}

/******************** scopes ********************/

Scheme_Object *scheme_stx_root_scope()
{
  /* The root scope is an all-phases scope used by all top-level namespaces
     (and not by module namespaces): */
  return root_scope;
}

Scheme_Object *scheme_new_scope(int kind)
{
  mzlonglong id;
  Scheme_Object *m;

  if (kind == SCHEME_STX_MODULE_MULTI_SCOPE) {
    m = scheme_malloc_small_tagged(sizeof(Scheme_Scope_With_Owner));
    SCHEME_SCOPE_FLAGS((Scheme_Scope *)m) |= 0x1;
  } else
    m = scheme_malloc_small_tagged(sizeof(Scheme_Scope));

  ((Scheme_Scope *)m)->iso.so.type = scheme_scope_type;
  id = ((++scope_counter) << SCHEME_STX_SCOPE_KIND_SHIFT) | kind;
  ((Scheme_Scope *)m)->id = id;

  return m;
}

static Scheme_Object *new_multi_scope(Scheme_Object *debug_name)
/* a multi-scope is a set of phase-specific scopes that are
   always added, removed, or flipped as a group */
{
  Scheme_Hash_Table *multi_scope;

  /* Maps a phase to a scope, where each scope is created on demand: */
  multi_scope = scheme_make_hash_table(SCHEME_hash_ptr);

  if (SCHEME_FALSEP(debug_name))
    MZ_OPT_HASH_KEY(&(multi_scope->iso)) |= 0x2;

  if (SAME_TYPE(SCHEME_TYPE(debug_name), scheme_resolved_module_path_type))
    debug_name = scheme_resolved_module_path_value(debug_name);
  if (SCHEME_FALSEP(debug_name))
    debug_name = scheme_gensym(top_symbol);
  
  scheme_hash_set(multi_scope, scheme_void, debug_name);

  return (Scheme_Object *)multi_scope;
}

static void repair_scope_owner(Scheme_Object *m)
{
  Scheme_Object *multi_scope;
  
  /* The owner scope might be missing due to broken bytecode. For
     non-broken bytecode, there shouldn't be a way to reach a
     scope withough going through its owner. Work around the
     broken scope object by creating a new owner. */

  multi_scope = new_multi_scope(scheme_false);
  scheme_hash_set((Scheme_Hash_Table *)multi_scope, scheme_make_integer(0), m);
  ((Scheme_Scope_With_Owner *)m)->owner_multi_scope = multi_scope;
}

Scheme_Object *scheme_scope_printed_form(Scheme_Object *m)
{
  int kind = ((Scheme_Scope *)m)->id & SCHEME_STX_SCOPE_KIND_MASK;
  Scheme_Object *num, *kind_sym, *vec, *name;

  num = scheme_make_integer_value_from_long_long(((Scheme_Scope *)m)->id >> SCHEME_STX_SCOPE_KIND_SHIFT);
  
  switch (kind) {
  case SCHEME_STX_MODULE_SCOPE:
  case SCHEME_STX_MODULE_MULTI_SCOPE:
    if (SAME_OBJ(m, root_scope))
      kind_sym = top_symbol;
    else
      kind_sym = module_symbol;
    break;
  case SCHEME_STX_MACRO_SCOPE:
    kind_sym = macro_symbol;
    break;
  case SCHEME_STX_LOCAL_BIND_SCOPE:
    kind_sym = local_symbol;
    break;
  case SCHEME_STX_INTDEF_SCOPE:
    kind_sym = intdef_symbol;
    break;
  case SCHEME_STX_USE_SITE_SCOPE:
    kind_sym = use_site_symbol;
    break;
  default:
    kind_sym = scheme_false;
    break;
  }

  if (SCHEME_SCOPE_HAS_OWNER((Scheme_Scope *)m)) {
    Scheme_Object *multi_scope = ((Scheme_Scope_With_Owner *)m)->owner_multi_scope;
    if (multi_scope) {
      name = scheme_eq_hash_get((Scheme_Hash_Table *)multi_scope, scheme_void);
      if (!name) name = scheme_false;
      if (MULTI_SCOPE_META_HASHEDP(name)) name = SCHEME_CAR(name);
      
      if (SCHEME_TL_MULTI_SCOPEP(multi_scope))
        kind_sym = top_symbol;
      
      vec = scheme_make_vector(4, NULL);
      SCHEME_VEC_ELS(vec)[2] = name;
      SCHEME_VEC_ELS(vec)[3] = ((Scheme_Scope_With_Owner *)m)->phase;
    } else {
      /* owner is either missing (bad bytecode) or hasn't been loaded on demand */
      vec = scheme_make_vector(2, NULL);
    }
  } else {
    vec = scheme_make_vector(2, NULL);
  }

  SCHEME_VEC_ELS(vec)[0] = num;
  SCHEME_VEC_ELS(vec)[1] = kind_sym;

  return vec;
}

#define SCHEME_SCOPE_SETP(m) SCHEME_HASHTRP((Scheme_Object *)(m))

XFORM_NONGCING static intptr_t scope_set_count(Scheme_Scope_Set *s)
{
  return ((Scheme_Hash_Tree *)s)->count;
}

XFORM_NONGCING static Scheme_Object *scope_set_get(Scheme_Scope_Set *s, Scheme_Object *key)
{
  return scheme_eq_hash_tree_get((Scheme_Hash_Tree *)s, key);
}

static Scheme_Scope_Set *scope_set_set(Scheme_Scope_Set *s, Scheme_Object *key, Scheme_Object *val)
{
  return (Scheme_Scope_Set *)scheme_hash_tree_set((Scheme_Hash_Tree *)s, key, val);
}

XFORM_NONGCING static mzlonglong scope_set_next(Scheme_Scope_Set *s, mzlonglong pos)
{
  return scheme_hash_tree_next((Scheme_Hash_Tree *)s, pos);
}

XFORM_NONGCING static int scope_set_index(Scheme_Scope_Set *s, mzlonglong pos, Scheme_Object **_key, Scheme_Object **_val)
{
  return scheme_hash_tree_index((Scheme_Hash_Tree *)s, pos, _key, _val);
}

XFORM_NONGCING static int scope_subset(Scheme_Scope_Set *sa, Scheme_Scope_Set *sb)
{
  return scheme_eq_hash_tree_subset_of((Scheme_Hash_Tree *)sa,
                                       (Scheme_Hash_Tree *)sb);
}

static int scopes_equal(Scheme_Scope_Set *a, Scheme_Scope_Set *b)
{
  return (scope_set_count(a) == scope_set_count(b)) && scope_subset(a, b);
}

XFORM_NONGCING static int scope_props_equal(Scheme_Scope_Set *a, Scheme_Scope_Set *b)
{
  return ((scope_set_count(a) == scope_set_count(b))
          && scheme_eq_hash_tree_subset_match_of((Scheme_Hash_Tree *)a,
                                                 (Scheme_Hash_Tree *)b));
}

static Scheme_Object *make_fallback_pair(Scheme_Object *a, Scheme_Object *b)
{
  a = scheme_make_vector(2, a);
  SCHEME_VEC_ELS(a)[1] = b;
  return a;
}

static Scheme_Object *make_fallback_quad(Scheme_Object *a, Scheme_Object *b,
                                         Scheme_Object *c, Scheme_Object *d)
{
  a = scheme_make_vector(4, a);
  SCHEME_VEC_ELS(a)[1] = b;
  SCHEME_VEC_ELS(a)[2] = c;
  SCHEME_VEC_ELS(a)[3] = d;
  return a;
}

Scheme_Object *extract_simple_scope(Scheme_Object *multi_scope, Scheme_Object *phase)
{
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)multi_scope;
  Scheme_Object *m;

  if (SCHEME_TRUEP(phase) && !SCHEME_INTP(phase)) {
    /* make sure phases are interned (in case of a bignum phase, which should be very rare): */
    phase = scheme_intern_literal_number(phase);
  }

  m = scheme_eq_hash_get(ht, phase);
  if (!m) {
    m = scheme_new_scope(SCHEME_STX_MODULE_MULTI_SCOPE);
    ((Scheme_Scope_With_Owner *)m)->owner_multi_scope = (Scheme_Object *)ht;
    ((Scheme_Scope_With_Owner *)m)->phase = phase;
    scheme_hash_set(ht, phase, m);

    if (SCHEME_MPAIRP(scheme_hash_get(ht, scheme_void))) {
      /* pair indicates loading from bytecode;
         zero out id, so that ordering is based on the owner plus the phase;
         this approach helps ensure determinstic ordering independent of
         the time at which simple scopes are generated */
      ((Scheme_Scope *)m)->id &= SCHEME_STX_SCOPE_KIND_MASK;
    }
  }

  return m;
}

static Scheme_Object *extract_simple_scope_from_shifted(Scheme_Object *multi_scope_and_phase, Scheme_Object *phase)
{
  Scheme_Object *ph;
    
  ph = SCHEME_CDR(multi_scope_and_phase);
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
    return NULL;
  } else
    ph = scheme_bin_minus(phase, ph);
  
  return extract_simple_scope(SCHEME_CAR(multi_scope_and_phase), ph);
}

static Scheme_Scope_Set *extract_scope_set_from_scope_list(Scheme_Scope_Set *scopes,
                                                           Scheme_Object *multi_scopes,
                                                           Scheme_Object *phase)
{
  Scheme_Object *m;

  /* Combine scopes that exist at all phases with a specific scope for
     each set of phase-specific scopes */

  if (SCHEME_FALLBACKP(multi_scopes))
    multi_scopes = SCHEME_FALLBACK_FIRST(multi_scopes);
  
  for (; !SCHEME_NULLP(multi_scopes); multi_scopes= SCHEME_CDR(multi_scopes)) {
    m = extract_simple_scope_from_shifted(SCHEME_CAR(multi_scopes), phase);
    if (m)
      scopes = scope_set_set(scopes, m, scheme_true);
  }

  return scopes;
}

static Scheme_Scope_Set *extract_scope_set(Scheme_Stx *stx, Scheme_Object *phase)
{
   Scheme_Scope_Table *st = stx->scopes;
   return extract_scope_set_from_scope_list(st->simple_scopes, st->multi_scopes, phase);
}

static Scheme_Scope_Set *adjust_scope(Scheme_Scope_Set *scopes, Scheme_Object *m, int mode)
/* operate on a single scope within a set */
{
  STX_ASSERT(SAME_TYPE(SCHEME_TYPE(m), scheme_scope_type));

  if (scope_set_get(scopes, m)) {
    if ((mode == SCHEME_STX_FLIP) || (mode == SCHEME_STX_REMOVE))
      return scope_set_set(scopes, m, NULL);
    else
      return scopes;
  } else {
    if (mode == SCHEME_STX_REMOVE)
      return scopes;
    else
      return scope_set_set(scopes, m, scheme_true);
  }
}

Scheme_Object *adjust_scope_list(Scheme_Object *multi_scopes, Scheme_Object *m, Scheme_Object *phase, int mode)
/* operate on a set of phase-specific scopes within a set */
{
  Scheme_Object *l;

  l = multi_scopes;
  if (SCHEME_FALLBACKP(l))
    l = SCHEME_FALLBACK_FIRST(l);
  
  for (; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (SAME_OBJ(m, SCHEME_CAR(SCHEME_CAR(l)))
        && SAME_OBJ(phase, SCHEME_CDR(SCHEME_CAR(l)))) {
      if ((mode == SCHEME_STX_ADD) || (mode == SCHEME_STX_PUSH))
        return multi_scopes;
      break;
    }
  }

  if (mode == SCHEME_STX_PUSH) {
    if (!SCHEME_NULLP(multi_scopes))
      return make_fallback_pair(scheme_make_pair(scheme_make_pair(m, phase),
                                                 (SCHEME_FALLBACKP(multi_scopes)
                                                  ? SCHEME_FALLBACK_FIRST(multi_scopes)
                                                  : multi_scopes)),
                                multi_scopes);
  }

  if ((mode == SCHEME_STX_REMOVE) && SCHEME_NULLP(l))
    return multi_scopes;
  else if ((mode == SCHEME_STX_REMOVE) 
           || ((mode == SCHEME_STX_FLIP && !SCHEME_NULLP(l)))) {
    return remove_at_scope_list(multi_scopes, l);
  } else
    return add_to_scope_list(scheme_make_pair(m, phase), multi_scopes);
}

static Scheme_Scope_Set *combine_scope(Scheme_Scope_Set *scopes, Scheme_Object *m, int mode)
/* operate on a single scope within a set of propagation instructions */
{
  Scheme_Object *old_mode;

  STX_ASSERT(SAME_TYPE(SCHEME_TYPE(m), scheme_scope_type));

  old_mode = scope_set_get(scopes, m);

  if (old_mode) {
    if (SCHEME_INT_VAL(old_mode) == mode) {
      if (mode == SCHEME_STX_FLIP)
        return scope_set_set(scopes, m, NULL);
      else
        return scopes;
    } else if (mode == SCHEME_STX_FLIP) {
      mode = SCHEME_INT_VAL(old_mode);
      mode = ((mode == SCHEME_STX_REMOVE) ? SCHEME_STX_ADD : SCHEME_STX_REMOVE);
      return scope_set_set(scopes, m, scheme_make_integer(mode));
    } else
      return scope_set_set(scopes, m, scheme_make_integer(mode));
  } else
    return scope_set_set(scopes, m, scheme_make_integer(mode));
}

Scheme_Object *combine_scope_list(Scheme_Object *multi_scopes, Scheme_Object *m, Scheme_Object *phase, int mode)
/* operate on a set of phase-specific scopes within a set of propagation instructions */
{
  Scheme_Object *l;

  l = multi_scopes;
  if (SCHEME_FALLBACKP(l)) {
    if ((mode == SCHEME_STX_PUSH)
        && SAME_OBJ(SCHEME_FALLBACK_SCOPE(l), m)
        && SAME_OBJ(SCHEME_FALLBACK_PHASE(l), phase))
      return multi_scopes;
    l = SCHEME_FALLBACK_FIRST(l);
  }

  for (; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (SAME_OBJ(m, SCHEME_VEC_ELS(SCHEME_CAR(l))[0])
        && SAME_OBJ(phase, SCHEME_VEC_ELS(SCHEME_CAR(l))[1])) {
      int prev_mode = SCHEME_INT_VAL(SCHEME_VEC_ELS(SCHEME_CAR(l))[2]);
      if (mode == SCHEME_STX_PUSH) {
        if (prev_mode == SCHEME_STX_ADD)
          return multi_scopes;
        break;
      } else if (mode == SCHEME_STX_FLIP) {
        if (prev_mode == SCHEME_STX_FLIP)
          return remove_at_scope_list(multi_scopes, l);
        else {
          if (prev_mode == SCHEME_STX_ADD)
            mode = SCHEME_STX_REMOVE;
          else
            mode = SCHEME_STX_ADD;
          multi_scopes = remove_at_scope_list(multi_scopes, l);
          break;
        }
      } else if (mode != prev_mode) {
        multi_scopes = remove_at_scope_list(multi_scopes, l);
        break;
      } else
        return multi_scopes;
    }
  }

  if (mode == SCHEME_STX_PUSH)
    return make_fallback_quad(scheme_null, multi_scopes, m, phase);
  else
    return add_to_scope_list(make_vector3(m, phase, scheme_make_integer(mode)),
                            multi_scopes);
}

static Scheme_Object *reconstruct_fallback(Scheme_Object *fb, Scheme_Object *r)
/* update actions for first (maybe only) in fallback chain */
{
  if (fb) {
    if (SCHEME_FALLBACK_QUADP(fb))
      return make_fallback_quad(r,
                                SCHEME_FALLBACK_REST(fb),
                                SCHEME_FALLBACK_SCOPE(fb),
                                SCHEME_FALLBACK_PHASE(fb));
    else
      return make_fallback_pair(r, SCHEME_FALLBACK_REST(fb));
  } else
    return r;
}

static Scheme_Object *clone_fallback_chain(Scheme_Object *fb)
{
  Scheme_Object *first = NULL, *last = NULL, *p;

  while (SCHEME_FALLBACKP(fb)) {
    p = reconstruct_fallback(fb, SCHEME_FALLBACK_FIRST(fb));
    if (last)
      SCHEME_FALLBACK_REST(last) = p;
    else
      first = p;
    last = p;
    fb = SCHEME_FALLBACK_REST(fb);
  }

  return first;
}

static Scheme_Object *remove_at_scope_list(Scheme_Object *l, Scheme_Object *p)
/* remove element at `p` within `l` */
{
  Scheme_Object *fb;
  Scheme_Object *r = SCHEME_CDR(p);

  if (SCHEME_FALLBACKP(l)) {
    fb = l;
    l = SCHEME_FALLBACK_FIRST(fb);
  } else
    fb = NULL;

  while (!SAME_OBJ(l, p)) {
    r = scheme_make_pair(SCHEME_CAR(l), r);
    l = SCHEME_CDR(l);
  }

  return reconstruct_fallback(fb, r);
}

static Scheme_Object *add_to_scope_list(Scheme_Object *p, Scheme_Object *l)
{
  if (SCHEME_FALLBACKP(l))
    return reconstruct_fallback(l, scheme_make_pair(p, SCHEME_FALLBACK_FIRST(l)));
  else
    return scheme_make_pair(p, l);
}

static Scheme_Scope_Table *clone_scope_table(Scheme_Scope_Table *st, Scheme_Scope_Table *prev,
                                             GC_CAN_IGNORE int *mutate)
/* If prev is non-NULL, then `st` is a propagate table */
{
  Scheme_Scope_Table *st2;

  if (!prev) {
    if (*mutate & MUTATE_STX_SCOPE_TABLE) {
      st2 = st;
      COUNT_MUTATE_ALLOCS(stx_skip_alloc_scope_table++);
    } else {
      st2 = MALLOC_ONE_TAGGED(Scheme_Scope_Table);
      memcpy(st2, st, sizeof(Scheme_Scope_Table));
      *mutate |= MUTATE_STX_SCOPE_TABLE;
      COUNT_MUTATE_ALLOCS(stx_alloc_scope_table++);
    }
  } else {
    if (*mutate & MUTATE_STX_PROP_TABLE) {
      st2 = st;
      COUNT_MUTATE_ALLOCS(stx_skip_alloc_prop_table++);
    } else {
      st2 = (Scheme_Scope_Table *)MALLOC_ONE_TAGGED(Scheme_Propagate_Table);
      memcpy(st2, st, sizeof(Scheme_Propagate_Table));
      if (SAME_OBJ(st, empty_propagate_table))
        ((Scheme_Propagate_Table *)st2)->prev = prev;
      *mutate |= MUTATE_STX_PROP_TABLE;
      COUNT_MUTATE_ALLOCS(stx_alloc_prop_table++);
    }
  }

  return st2;
}

typedef Scheme_Scope_Set *(*do_scope_t)(Scheme_Scope_Set *scopes, Scheme_Object *m, int mode);
typedef Scheme_Object *(do_scope_list_t)(Scheme_Object *multi_scopes, Scheme_Object *m, Scheme_Object *phase, int mode);

static Scheme_Scope_Table *do_scope_at_phase(Scheme_Scope_Table *st, Scheme_Object *m, Scheme_Object *phase, int mode, 
                                             do_scope_t do_scope, do_scope_list_t do_scope_list, Scheme_Scope_Table *prev,
                                             GC_CAN_IGNORE int *mutate)
/* operate on a scope or set of phase specific scopes,
   either on a scope set or a set of propagation instructions */
{
  Scheme_Object *l;
  Scheme_Scope_Set *scopes;

  if (SCHEME_SCOPEP(m) && SCHEME_SCOPE_HAS_OWNER((Scheme_Scope *)m)) {
    if (!SCHEME_FALSEP(phase))
      phase = scheme_bin_minus(phase, ((Scheme_Scope_With_Owner *)m)->phase);
    if (!((Scheme_Scope_With_Owner *)m)->owner_multi_scope)
      repair_scope_owner(m);
    m = ((Scheme_Scope_With_Owner *)m)->owner_multi_scope;
  }

  if (SCHEME_MULTI_SCOPEP(m)) {
    l = do_scope_list(st->multi_scopes, m, phase, mode);
    if (SAME_OBJ(l, st->multi_scopes))
      return st;
    st = clone_scope_table(st, prev, mutate);
    st->multi_scopes = l;
    return st;
  } else {
    scopes = do_scope(st->simple_scopes, m, mode);
    if (SAME_OBJ(scopes, st->simple_scopes))
      return st;
    st = clone_scope_table(st, prev, mutate);
    st->simple_scopes = scopes;
    return st;
  }
}

static Scheme_Object *stx_adjust_scope(Scheme_Object *o, Scheme_Object *m, Scheme_Object *phase, int mode,
                                       GC_CAN_IGNORE int *mutate)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Scope_Table *scopes;
  Scheme_Scope_Table *to_propagate;
  Scheme_Object *taints, *shifts;

  STX_ASSERT(SCHEME_STXP(o));

  if (mode & SCHEME_STX_PROPONLY) {
    scopes = stx->scopes;
    mode -= SCHEME_STX_PROPONLY;
  } else {
    scopes = do_scope_at_phase(stx->scopes, m, phase, mode, adjust_scope, adjust_scope_list, NULL, mutate);
    if ((stx->scopes == scopes)
        && !(STX_KEY(stx) & STX_SUBSTX_FLAG)) {
      return (Scheme_Object *)stx;
    }
  }

  if (STX_KEY(stx) & STX_SUBSTX_FLAG) {
    to_propagate = (stx->u.to_propagate ? stx->u.to_propagate : empty_propagate_table);
    to_propagate = do_scope_at_phase(to_propagate, m, phase, mode, combine_scope, combine_scope_list, stx->scopes, mutate);
    if ((stx->u.to_propagate == to_propagate)
        && (stx->scopes == scopes))
      return (Scheme_Object *)stx;
  } else
    to_propagate = NULL; /* => clear cache */

  if (*mutate & MUTATE_STX_OBJ) {
    stx->scopes = scopes;
    stx->u.to_propagate = to_propagate;
  } else {
    int armed = (STX_KEY(stx) & STX_ARMED_FLAG);
    taints = stx->taints;
    shifts = stx->shifts;
    stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
    stx->scopes = scopes;
    stx->u.to_propagate = to_propagate;
    stx->taints = taints;
    stx->shifts = shifts;
    if (armed)
      STX_KEY(stx) |= STX_ARMED_FLAG;
    *mutate |= MUTATE_STX_OBJ;
  }

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_stx_adjust_scope(Scheme_Object *o, Scheme_Object *m, Scheme_Object *phase, int mode)
{
  int mutate = 0;
  return stx_adjust_scope(o, m, phase, mode, &mutate);
}

Scheme_Object *scheme_stx_add_scope(Scheme_Object *o, Scheme_Object *m, Scheme_Object *phase)
{
  return scheme_stx_adjust_scope(o, m, phase, SCHEME_STX_ADD);
}

Scheme_Object *scheme_stx_remove_scope(Scheme_Object *o, Scheme_Object *m, Scheme_Object *phase)
{
  return scheme_stx_adjust_scope(o, m, phase, SCHEME_STX_REMOVE);
}

Scheme_Object *scheme_stx_flip_scope(Scheme_Object *o, Scheme_Object *m, Scheme_Object *phase)
{
  return scheme_stx_adjust_scope(o, m, phase, SCHEME_STX_FLIP);
}

static Scheme_Object *stx_adjust_scopes(Scheme_Object *o, Scheme_Scope_Set *scopes, Scheme_Object *phase, int mode,
                                       GC_CAN_IGNORE int *mutate)
{
  Scheme_Object *key, *val;
  intptr_t i;

  STX_ASSERT(SCHEME_STXP(o));
  STX_ASSERT(SCHEME_SCOPE_SETP(scopes));

  i = scope_set_next(scopes, -1);
  while (i != -1) {
    scope_set_index(scopes, i, &key, &val);

    o = stx_adjust_scope(o, key, phase, mode, mutate);
    
    i = scope_set_next(scopes, i);
  }

  return o;
}

Scheme_Object *scheme_stx_adjust_scopes(Scheme_Object *o, Scheme_Scope_Set *scopes, Scheme_Object *phase, int mode)
{
  int mutate = 0;
  return stx_adjust_scopes(o, scopes, phase, mode, &mutate);
}

/* For each continuation frame, we need to keep track of various sets of scopes:
    - bind scopes (normally 0 or 1) are created for the binding context
    - use-site scopes are created for macro expansions that need them
    - intdef scopes are for immediately nested internal-definition contexts;
      they're treated the same as bind scopes

   frame-scopes = main-scopes
   .           | (vector bind-scopes use-site-scopes intdef-scopes)
   bind-scopes = some-scopes
   use-site-scopes = some-scopes
   intdef-scopes = some-scopes
   some-scopes = #f | scope | scope-set */

static Scheme_Object *stx_adjust_frame_scopes(Scheme_Object *o, Scheme_Object *scope, int which, Scheme_Object *phase, int mode)
{
  if (SCHEME_VECTORP(scope)) {
    scope = SCHEME_VEC_ELS(scope)[which];
  } else if (which != 0)
    return o;

  if (SCHEME_FALSEP(scope))
    return o;
  else if (SCHEME_SCOPEP(scope))
    return scheme_stx_adjust_scope(o, scope, phase, mode);
  else {
    STX_ASSERT(SCHEME_SCOPE_SETP(scope));
    return scheme_stx_adjust_scopes(o, (Scheme_Scope_Set *)scope, phase, mode);
  }
}

Scheme_Object *scheme_stx_adjust_frame_scopes(Scheme_Object *o, Scheme_Object *scope, Scheme_Object *phase, int mode)
{
  o = scheme_stx_adjust_frame_use_site_scopes(o, scope, phase, mode);
  o = scheme_stx_adjust_frame_bind_scopes(o, scope, phase, mode);
  return stx_adjust_frame_scopes(o, scope, 2, phase, mode);
}

Scheme_Object *scheme_stx_adjust_frame_bind_scopes(Scheme_Object *o, Scheme_Object *scope, Scheme_Object *phase, int mode)
{
  return stx_adjust_frame_scopes(o, scope, 0, phase, mode);
}

Scheme_Object *scheme_stx_adjust_frame_use_site_scopes(Scheme_Object *o, Scheme_Object *scope, Scheme_Object *phase, int mode)
{
  return stx_adjust_frame_scopes(o, scope, 1, phase, mode);
}

Scheme_Object *scheme_make_frame_scopes(Scheme_Object *scope)
{
  return scope;
}

static Scheme_Object *add_frame_scope(Scheme_Object *frame_scopes, Scheme_Object *scope, int pos)
{
  Scheme_Object *scopes;
  
  if (!frame_scopes) {
    if (pos == 0)
      return scope;
    else
      frame_scopes = scheme_false;
  }

  if (SCHEME_VECTORP(frame_scopes))
    scopes = SCHEME_VEC_ELS(frame_scopes)[pos];
  else if (pos == 0)
    scopes = frame_scopes;
  else
    scopes = scheme_false;

  if (SCHEME_FALSEP(scopes))
    scopes = scope;
  else {
    STX_ASSERT(!SCHEME_MULTI_SCOPEP(scopes));
    if (SCHEME_SCOPEP(scopes))
      scopes = (Scheme_Object *)scope_set_set(empty_scope_set, scopes, scheme_true);
    scopes = (Scheme_Object *)scope_set_set((Scheme_Scope_Set *)scopes, scope, scheme_true);
  }

  if (SCHEME_VECTORP(frame_scopes))
    frame_scopes = make_vector3(SCHEME_VEC_ELS(frame_scopes)[0],
                               SCHEME_VEC_ELS(frame_scopes)[1],
                               SCHEME_VEC_ELS(frame_scopes)[2]);
  else
    frame_scopes = make_vector3(frame_scopes, scheme_false, scheme_false);

  SCHEME_VEC_ELS(frame_scopes)[pos] = scopes;

  return frame_scopes;
}

Scheme_Object *scheme_add_frame_use_site_scope(Scheme_Object *frame_scopes, Scheme_Object *use_site_scope)
{
  return add_frame_scope(frame_scopes, use_site_scope, 1);
}

Scheme_Object *scheme_add_frame_intdef_scope(Scheme_Object *frame_scopes, Scheme_Object *scope)
{
  return add_frame_scope(frame_scopes, scope, 2);
}

static Scheme_Object *add_intdef_scopes_of(Scheme_Object *scopes, Scheme_Object *keep_intdef_scopes)
{
  if (SCHEME_VECTORP(keep_intdef_scopes)
      && SCHEME_TRUEP(SCHEME_VEC_ELS(keep_intdef_scopes)[2])) {
    if (scopes && SCHEME_VECTORP(scopes)) {
      if (SCHEME_TRUEP(SCHEME_VEC_ELS(scopes)[2]))
        scheme_signal_error("internal error: cannot currently merge intdef scopes");
      return make_vector3(SCHEME_VEC_ELS(scopes)[0],
                          SCHEME_VEC_ELS(scopes)[1],
                          SCHEME_VEC_ELS(keep_intdef_scopes)[2]);
    } else
      return make_vector3(scopes ? scopes : scheme_false,
                          scheme_false,
                          SCHEME_VEC_ELS(keep_intdef_scopes)[2]);
  }

  return scopes;
}

int scheme_stx_has_empty_wraps(Scheme_Object *stx, Scheme_Object *phase)
{
  return (scope_set_count(extract_scope_set((Scheme_Stx *)stx, phase)) == 0);
}

/******************** shifts ********************/

/* Shifts includes both phase shifts (in the sense of
   `syntax-shift-phase-level`) and shifting a module reference based
   on one modix (at compile time, say) to a different one (at run
   time, say). A modidx kind of shift can also include an inspector
   substution (e.g., a load-time inspectr to take the place of the
   compile-time one) and an export registry for restoring lazily load
   bulk-import bindings (for when all exports of a module are
   imported, and we go find the imported module on demand). */

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

static Scheme_Object *shift_multi_scope(Scheme_Object *p, Scheme_Object *shift)
/* shift all phase-specific scopes in a set */
{
  shift = add_shifts(SCHEME_CDR(p), shift);

  if (!shift)
    return NULL;

  if (SAME_OBJ(shift, SCHEME_CDR(p)))
    return p;

  return scheme_make_pair(SCHEME_CAR(p), shift);
}

static Scheme_Object *shift_prop_multi_scope(Scheme_Object *p, Scheme_Object *shift)
  /* shift all phase-specific scopes in a set of propagation instructions */
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

typedef Scheme_Object *(shift_multi_scope_t)(Scheme_Object *p, Scheme_Object *shift);

static Scheme_Scope_Table *shift_scope_table(Scheme_Scope_Table *st, Scheme_Object *shift, 
                                             shift_multi_scope_t shift_mm, Scheme_Scope_Table *prev,
                                             GC_CAN_IGNORE int *mutate)
{
  Scheme_Scope_Table *st2;
  Scheme_Object *l, *key, *val, *fbs;

  if (SAME_OBJ(st, empty_scope_table)) {
    STX_ASSERT(!prev);
    return st;
  }

  if ((SCHEME_NULLP(st->multi_scopes)
       || (SCHEME_FALLBACKP(st->multi_scopes)
           && SCHEME_NULLP(SCHEME_FALLBACK_FIRST(st->multi_scopes))))
      && !prev)
    return st;

  st2 = clone_scope_table(st, prev, mutate);

  l = st->multi_scopes;
  if (SCHEME_FALLBACKP(l)) {
    l = clone_fallback_chain(l);
    st2->multi_scopes = l;
    fbs = l;
  } else
    fbs = scheme_false;
  /* loop to cover all fallbacks; fbs is #f for
     no fallback handling, otherwise it's always
     a fallback record and the updated list goes
     in first or rest */
  while (1) {
    int was_fb;
    if (SCHEME_FALLBACKP(l)) {
      l = SCHEME_FALLBACK_FIRST(l);
      was_fb = 1;
    } else
      was_fb = 0;

    /* Loop through one list of multi scopes: */
    val = scheme_null;
    for (; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      key = shift_mm(SCHEME_CAR(l), shift);
      if (key)
        val = scheme_make_pair(key, val);
    }

    if (SCHEME_FALLBACKP(fbs)) {
      if (was_fb) {
        SCHEME_FALLBACK_FIRST(fbs) = val;
        l = SCHEME_FALLBACK_REST(fbs);
        if (SCHEME_FALLBACKP(l))
          fbs = l;
      } else {
        SCHEME_FALLBACK_REST(fbs) = val;
        break;
      }
    } else {
      st2->multi_scopes = val;
      break;
    }
  }

  if (prev) {
    /* record accumulated shift for propagation */
    shift = add_shifts(((Scheme_Propagate_Table *)st)->phase_shift, shift);
    if (!shift)
      shift = scheme_box(scheme_false); /* i.e., the impossible shift */
    ((Scheme_Propagate_Table *)st2)->phase_shift = shift;
  }

  return st2;
}

static Scheme_Object *shift_scopes(Scheme_Object *o, Scheme_Object *shift, int prop_only,
                                  GC_CAN_IGNORE int *mutate)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Scope_Table *st, *p_st;

  if (prop_only)
    st = stx->scopes;
  else
    st = shift_scope_table(stx->scopes, shift, shift_multi_scope, NULL, mutate);
  if (STX_KEY(stx) & STX_SUBSTX_FLAG)
    p_st = shift_scope_table((stx->u.to_propagate ? stx->u.to_propagate : empty_propagate_table),
                            shift, shift_prop_multi_scope, stx->scopes,
                            mutate);
  else
    p_st = NULL;
  
  if (SAME_OBJ(stx->scopes, st)
      && (!(STX_KEY(stx) & STX_SUBSTX_FLAG)
          || SAME_OBJ(stx->u.to_propagate, p_st)))
    return (Scheme_Object *)stx;

  stx = (Scheme_Stx *)clone_stx((Scheme_Object *)stx, mutate);

  stx->scopes = st;
  if (p_st)
    stx->u.to_propagate = p_st;
  
  return (Scheme_Object *)stx;
}

static Scheme_Object *do_stx_add_shift(Scheme_Object *o, Scheme_Object *shift, GC_CAN_IGNORE int *mutate)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *vec, *shifts;

  if (!shift) return (Scheme_Object *)stx;

  if (SCHEME_PHASE_SHIFTP(shift)) {
    if (SAME_OBJ(shift, scheme_make_integer(0)))
      return (Scheme_Object *)stx;
    return shift_scopes((Scheme_Object *)stx, shift, 0, mutate);
  }

  if (SCHEME_VECTORP(shift)
      && (SCHEME_VEC_SIZE(shift) == 6)
      && (SCHEME_VEC_ELS(shift)[5] != scheme_make_integer(0))) {
    /* Handle phase shift by itself, first: */
    stx = (Scheme_Stx *)do_stx_add_shift((Scheme_Object *)stx, SCHEME_VEC_ELS(shift)[5], mutate);
    /* strip away phase shift: */
    vec = scheme_make_vector(6, NULL);
    SCHEME_VEC_ELS(vec)[0] = SCHEME_VEC_ELS(shift)[0];
    SCHEME_VEC_ELS(vec)[1] = SCHEME_VEC_ELS(shift)[1];
    SCHEME_VEC_ELS(vec)[2] = SCHEME_VEC_ELS(shift)[2];
    SCHEME_VEC_ELS(vec)[3] = SCHEME_VEC_ELS(shift)[3];
    SCHEME_VEC_ELS(vec)[4] = SCHEME_VEC_ELS(shift)[4];
    SCHEME_VEC_ELS(vec)[5] = scheme_make_integer(0);
    shift = vec;
  }

  /* Drop useless shift (identidy modidx shift and no inspector or exports): */
  if (SAME_OBJ(SCHEME_VEC_ELS(shift)[0], SCHEME_VEC_ELS(shift)[1])
      && ((SCHEME_VEC_SIZE(shift) <= 3)
          || SCHEME_FALSEP(SCHEME_VEC_ELS(shift)[3]))
      && ((SCHEME_VEC_SIZE(shift) <= 4)
          || SCHEME_FALSEP(SCHEME_VEC_ELS(shift)[4])))
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

  stx = (Scheme_Stx *)clone_stx((Scheme_Object *)stx, mutate);
  stx->shifts = shifts;
  
  if ((STX_KEY(stx) & STX_SUBSTX_FLAG) && !stx->u.to_propagate)
    stx->u.to_propagate = empty_propagate_table;

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_stx_add_shift(Scheme_Object *o, Scheme_Object *shift)
{
  int mutate = 0;
  return do_stx_add_shift(o, shift, &mutate);
}

Scheme_Object *scheme_stx_add_shifts(Scheme_Object *o, Scheme_Object *l)
{
  int mutate = 0;

  for (l = scheme_reverse(l); !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    o = do_stx_add_shift(o, SCHEME_CAR(l), &mutate);
  }

  return o;
}

Scheme_Object *scheme_make_shift(Scheme_Object *phase_delta,
                                 Scheme_Object *old_midx, Scheme_Object *new_midx,
                                 Scheme_Hash_Table *export_registry,
                                 Scheme_Object *src_insp_desc, Scheme_Object *insp)
{
  Scheme_Object *exr;
  
  if (!phase_delta)
    phase_delta = scheme_make_integer(0);

  if (!new_midx) {
    old_midx = scheme_false;
    new_midx = scheme_false;
  }
  if (!src_insp_desc)
    src_insp_desc = scheme_false;
  if (!insp)
    insp = scheme_false;
  if (!export_registry)
    exr = scheme_false;
  else
    exr = (Scheme_Object *)export_registry;

  if (new_midx || export_registry || insp) {
    Scheme_Object *vec;

    vec = last_phase_shift;
    
    if (vec
	&& (SCHEME_VEC_ELS(vec)[0] == old_midx)
        && (SCHEME_VEC_ELS(vec)[1] == new_midx)
        && (SCHEME_VEC_ELS(vec)[2] == src_insp_desc)
        && (SCHEME_VEC_ELS(vec)[3] == insp)
	&& (SCHEME_VEC_ELS(vec)[4] == exr)
        && (SCHEME_VEC_ELS(vec)[5] == phase_delta)) {
      /* use the old one */
    } else {
      vec = scheme_make_vector(6, NULL);
      SCHEME_VEC_ELS(vec)[0] = old_midx;
      SCHEME_VEC_ELS(vec)[1] = new_midx;
      SCHEME_VEC_ELS(vec)[2] = src_insp_desc;
      SCHEME_VEC_ELS(vec)[3] = insp;
      SCHEME_VEC_ELS(vec)[4] = exr;
      SCHEME_VEC_ELS(vec)[5] = phase_delta;
      
      last_phase_shift = vec;
    }

    return last_phase_shift;
  } else
    return NULL;
}

void scheme_clear_shift_cache(void)
{
  int i;

  for (i = 0; i < NUM_RECENT_SCOPE_SETS; i++) {
    recent_scope_sets[0][i] = NULL;
    recent_scope_sets[1][i] = NULL;
  }

  last_phase_shift = NULL;
  nominal_ipair_cache = NULL;
  clear_binding_cache();
}

Scheme_Object *scheme_stx_shift(Scheme_Object *stx, 
                                Scheme_Object *phase_delta,
                                Scheme_Object *old_midx, Scheme_Object *new_midx,
                                Scheme_Hash_Table *export_registry,
                                Scheme_Object *src_insp_desc, Scheme_Object *insp)
/* Shifts the modidx on a syntax object in a module as well as the phase of scopes. */
{
  Scheme_Object *s;

  s = scheme_make_shift(phase_delta, old_midx, new_midx, export_registry, src_insp_desc, insp);
  if (s)
    stx = scheme_stx_add_shift(stx, s);

  return stx;
}

static Scheme_Object *apply_modidx_shifts(Scheme_Object *shifts, Scheme_Object *modidx,
                                          Scheme_Object **_insp, Scheme_Hash_Table **_export_registry)
{
#define QUICK_SHIFT_LEN 5
  Scheme_Object *vec, *dest, *src, *insp_desc;
  Scheme_Object *quick_a[QUICK_SHIFT_LEN], **a;
  intptr_t i, len;

  /* Strip away propagation layer, if any: */
  if (SCHEME_VECTORP(shifts))
    shifts = SCHEME_VEC_ELS(shifts)[0];

  if (_insp && *_insp)
    insp_desc = *_insp;
  else
    insp_desc = scheme_false;

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

  if (_export_registry)
    *_export_registry = NULL;

  for (i = 0; i < len; i++) {
    vec = a[i];
     
    src = SCHEME_VEC_ELS(vec)[0];
    dest = SCHEME_VEC_ELS(vec)[1];

    modidx = scheme_modidx_shift(modidx, src, dest);

    if (SCHEME_VEC_SIZE(vec) > 2) {
      if (SCHEME_SYMBOLP(insp_desc)
          && SAME_OBJ(insp_desc, SCHEME_VEC_ELS(vec)[2])) {
        if (!SCHEME_FALSEP(SCHEME_VEC_ELS(vec)[3]))
          insp_desc = SCHEME_VEC_ELS(vec)[3];
        if (_export_registry
            && (SCHEME_VEC_SIZE(vec) > 4)
            && !SCHEME_FALSEP(SCHEME_VEC_ELS(vec)[4]))
          *_export_registry = (Scheme_Hash_Table *)SCHEME_VEC_ELS(vec)[4];
      }
    }
  }

  if (_insp && (!*_insp || !SCHEME_INSPECTORP(*_insp)))
    *_insp = insp_desc;
  
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

#define DO_COUNT_PROPAGATES 0
#if DO_COUNT_PROPAGATES
# define COUNT_PROPAGATES(x) x
int stx_shorts, stx_meds, stx_longs, stx_couldas;
#else
# define COUNT_PROPAGATES(x) /* empty */
#endif

XFORM_NONGCING static void intern_scope_set(Scheme_Scope_Table *t, int prop_table)
/* We don't realy intern, but approximate interning by checking
   against a small set of recently allocated scope sets. That's good
   enough to find sharing for a deeply nested sequence of `let`s from
   a many-argument `or`, for example, where the interleaving of
   original an macro-introduced syntax prevents the usual
   child-is-same-as-parent sharing detecting from working well
   enough. */
{
  int i;
  Scheme_Scope_Set *s;

  if (!t->simple_scopes || !scope_set_count(t->simple_scopes))
    return;

  for (i = 0; i < NUM_RECENT_SCOPE_SETS; i++) {
    s = recent_scope_sets[prop_table][i];
    if (s) {
      if (s == t->simple_scopes)
        return;
      if ((!prop_table && scopes_equal(s, t->simple_scopes))
          || (prop_table && scope_props_equal(s, t->simple_scopes))) {
        t->simple_scopes = s;
        return;
      }
    }
  }

  recent_scope_sets[prop_table][recent_scope_sets_pos[prop_table]] = t->simple_scopes;

  recent_scope_sets_pos[prop_table] = ((recent_scope_sets_pos[prop_table] + 1) & (NUM_RECENT_SCOPE_SETS - 1));
}

static Scheme_Object *propagate_scope_set(Scheme_Scope_Set *props, Scheme_Object *o,
                                          Scheme_Object *phase, int flag,
                                          GC_CAN_IGNORE int *mutate)
{
  intptr_t i;
  Scheme_Object *key, *val;

  i = scope_set_next(props, -1);
  if (i != -1) {
    do {
      scope_set_index(props, i, &key, &val);

      STX_ASSERT(!SCHEME_SCOPE_HAS_OWNER((Scheme_Scope *)key));

      o = stx_adjust_scope(o, key, phase, SCHEME_INT_VAL(val) | flag, mutate);

      i = scope_set_next(props, i);
    } while (i != -1);

    intern_scope_set(((Scheme_Stx *)o)->scopes, 0);
    if (STX_KEY(((Scheme_Stx *)o)) & STX_SUBSTX_FLAG
        && ((Scheme_Stx *)o)->u.to_propagate)
      intern_scope_set(((Scheme_Stx *)o)->u.to_propagate, 1);
  }

  return o;
}

XFORM_NONGCING static int equiv_scope_tables(Scheme_Scope_Table *a, Scheme_Scope_Table *b)
/* try to cheaply detect equivalent tables to enable shortcuts */
{
  if (a == b)
    return 1;

  if (((a->simple_scopes == b->simple_scopes)
       || (!scope_set_count(a->simple_scopes)
           && !scope_set_count(b->simple_scopes)))
      && SAME_OBJ(a->multi_scopes, b->multi_scopes))
    return 1;

  return 0;
}

static Scheme_Object *propagate_scopes(Scheme_Object *o, Scheme_Scope_Table *to_propagate,
                                       Scheme_Scope_Table *parent_scopes, int flag,
                                       GC_CAN_IGNORE int *mutate)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *key, *val, *fb;

  if (!to_propagate || (to_propagate == empty_propagate_table))
    return o;

  /* Check whether the child scopes currently match the
     parent's scopes before the propagated changes: */
  if (!(flag & SCHEME_STX_PROPONLY)
      && equiv_scope_tables(((Scheme_Propagate_Table *)to_propagate)->prev, stx->scopes)) {
    /* Yes, so we can take a shortcut: child scopes still match parent.
       Does the child need to propagate, and if so, does it just
       get the parent's propagation? */
    if (!(STX_KEY(stx) & STX_SUBSTX_FLAG)
        || !stx->u.to_propagate
        || SAME_OBJ(stx->u.to_propagate, empty_propagate_table)) {
      /* Yes, child matches the parent in all relevant dimensions */
      stx = (Scheme_Stx *)clone_stx((Scheme_Object *)stx, mutate);
      stx->scopes = parent_scopes;
      *mutate -= (*mutate & MUTATE_STX_SCOPE_TABLE);
      if (STX_KEY(stx) & STX_SUBSTX_FLAG) {
        stx->u.to_propagate = to_propagate;
        *mutate -= (*mutate & MUTATE_STX_PROP_TABLE);
      }
      COUNT_PROPAGATES(stx_shorts++);
      return (Scheme_Object *)stx;
    } else {
      /* Child scopes match parent, so we don't need to reconstruct
         the scope set, but we need to build a new propagation set
         to augment the propagate set already here */
      flag |= SCHEME_STX_PROPONLY;
      COUNT_PROPAGATES(stx_meds++);
    }
  } else {
    COUNT_PROPAGATES(stx_longs++);
  }

  val = ((Scheme_Propagate_Table *)to_propagate)->phase_shift;
  if (!SAME_OBJ(val, scheme_make_integer(0))) {
    o = shift_scopes(o, val, flag & SCHEME_STX_PROPONLY, mutate);
  }

  o = propagate_scope_set(to_propagate->simple_scopes, o, scheme_true, flag, mutate);

  /* fallbacks here mean that we need to propagate fallback creations,
     as well as propagating actions at each fallback layer: */

  fb = to_propagate->multi_scopes;
  if (SCHEME_FALLBACKP(fb)) {
    /* reverse the fallback list so we can replay them in the right order: */
    key = scheme_null;
    while (SCHEME_FALLBACKP(fb)) {
      key = make_fallback_quad(SCHEME_FALLBACK_FIRST(fb),
                               key,
                               SCHEME_FALLBACK_SCOPE(fb),
                               SCHEME_FALLBACK_PHASE(fb));
      fb = SCHEME_FALLBACK_REST(fb);
    }
    fb = make_fallback_pair(fb, key);
  }

  while (fb) {
    if (SCHEME_FALLBACKP(fb)) {
      if (SCHEME_FALLBACK_QUADP(fb)) {
        o = stx_adjust_scope(o, SCHEME_FALLBACK_SCOPE(fb), SCHEME_FALLBACK_PHASE(fb),
                            SCHEME_STX_PUSH | flag, mutate);
      }
      key = SCHEME_FALLBACK_FIRST(fb);
    } else
      key = fb;
    
    for (; !SCHEME_NULLP(key); key = SCHEME_CDR(key)) {
      val = SCHEME_CAR(key);
      STX_ASSERT(SCHEME_MULTI_SCOPEP(SCHEME_VEC_ELS(val)[0]));
      o = stx_adjust_scope(o, SCHEME_VEC_ELS(val)[0], SCHEME_VEC_ELS(val)[1], 
                          SCHEME_INT_VAL(SCHEME_VEC_ELS(val)[2]) | flag, mutate);
    }

    if (SCHEME_FALLBACKP(fb))
      fb = SCHEME_FALLBACK_REST(fb);
    else
      fb = NULL;
  }
  
  if (flag & SCHEME_STX_PROPONLY) {
    o = clone_stx(o, mutate);
    ((Scheme_Stx *)o)->scopes = parent_scopes;
    *mutate -= (*mutate & MUTATE_STX_SCOPE_TABLE);
  }

#if DO_COUNT_PROPAGATES
  if (!(flag & SCHEME_STX_PROPONLY)) {
    if (scheme_equal((Scheme_Object *)parent_scopes->simple_scopes,
                     (Scheme_Object *)((Scheme_Stx *)o)->scopes->simple_scopes)
        && scheme_equal(parent_scopes->multi_scopes,
                        ((Scheme_Stx *)o)->scopes->multi_scopes))
      stx_couldas++;
  }
#endif
  
  return o;
}

static Scheme_Object *propagate_shifts(Scheme_Object *result, Scheme_Object *shifts, GC_CAN_IGNORE int *mutate)
{
  Scheme_Stx *stx = (Scheme_Stx *)result;
  Scheme_Object *l;

  if (SAME_OBJ(stx->shifts, SCHEME_VEC_ELS(shifts)[2])) {
    result = clone_stx(result, mutate);
    stx = (Scheme_Stx *)result;

    if ((STX_KEY(stx) & STX_SUBSTX_FLAG)) {
      stx->shifts = shifts;
      if (!stx->u.to_propagate)
        stx->u.to_propagate = empty_propagate_table;
    } else
      stx->shifts = SCHEME_VEC_ELS(shifts)[0];
    return result;
  }

  for (l = scheme_reverse(SCHEME_VEC_ELS(shifts)[1]); !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    result = do_stx_add_shift(result, SCHEME_CAR(l), mutate);
  }

  return result;
}

static Scheme_Object *propagate(Scheme_Object *result,
                                Scheme_Scope_Table *to_propagate,
                                Scheme_Scope_Table *parent_scopes,
                                Scheme_Object *shifts,
                                int add_taint, Scheme_Object *false_insp)
{
  int mutate = 0;

  result = propagate_scopes(result, to_propagate, parent_scopes, 0, &mutate);

  if (shifts)
    result = propagate_shifts(result, shifts, &mutate);

  if (add_taint)
    result = add_taint_to_stx(result, &mutate);
  else if (false_insp)
    result = set_false_insp(result, false_insp, &mutate);

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
    Scheme_Scope_Table *to_propagate;
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
        result = propagate(result, to_propagate, stx->scopes,
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
        result = propagate(result, to_propagate, stx->scopes,
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
      result = propagate(result, to_propagate, stx->scopes,
                         shifts,
                         add_taint, false_insp);
      v = scheme_box(result);
    } else if (SCHEME_VECTORP(v)) {
      Scheme_Object *v2;
      int size = SCHEME_VEC_SIZE(v), i;
      
      v2 = scheme_make_vector(size, NULL);
      
      for (i = 0; i < size; i++) {
        result = SCHEME_VEC_ELS(v)[i];
        result = propagate(result, to_propagate, stx->scopes,
                           shifts,
                           add_taint, false_insp);
      	SCHEME_VEC_ELS(v2)[i] = result;
      }
      
      v = v2;
    } else if (SCHEME_HASHTRP(v)) {
      Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)v, *ht2;
      Scheme_Object *key, *val;
      mzlonglong i;

      ht2 = scheme_make_hash_tree_of_type(SCHEME_HASHTR_TYPE(ht));

      i = scheme_hash_tree_next(ht, -1);
      while (i != -1) {
        scheme_hash_tree_index(ht, i, &key, &val);
        val = propagate(val, to_propagate, stx->scopes,
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
        r = propagate(r, to_propagate, stx->scopes,
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
  o = add_taint_to_stx(o, NULL);

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

static Scheme_Object *add_taint_to_stx(Scheme_Object *o, GC_CAN_IGNORE int *mutate)
{
  Scheme_Stx *stx;
  
  if (is_tainted(o))
    return o;

  o = clone_stx(o, mutate);
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

static Scheme_Object *set_false_insp(Scheme_Object *o, Scheme_Object *false_insp, GC_CAN_IGNORE int *mutate)
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

  o = clone_stx(o, mutate);
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
    o = clone_stx(o, NULL);
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
  return add_taint_to_stx(o, NULL);
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
    return add_taint_to_stx(o, NULL);
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

  o = clone_stx(o, NULL);

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

XFORM_NONGCING static Scheme_Scope *extract_max_scope(Scheme_Scope_Set *scopes)
{
  intptr_t i;
  Scheme_Object *key, *val;
  Scheme_Scope *scope;
  mzlonglong scope_id_val, id_val;

  i = scope_set_next(scopes, -1);
  scope_set_index(scopes, i, &key, &val);

  scope = (Scheme_Scope *)key;
  scope_id_val = scope->id;

  i = scope_set_next(scopes, i);
  while (i != -1) {
    scope_set_index(scopes, i, &key, &val);

    id_val = ((Scheme_Scope *)key)->id;
    if (id_val > scope_id_val) {
      scope = (Scheme_Scope *)key;
      scope_id_val = id_val;
    }
    
    i = scope_set_next(scopes, i);
  }

  return scope;
}

#define SCHEME_BINDING_SCOPES(p) ((Scheme_Scope_Set *)SCHEME_CAR(p))
#define SCHEME_BINDING_VAL(p)   SCHEME_CDR(p)

#define SCHEME_VEC_BINDING_KEY(p)   (SCHEME_VEC_ELS(p)[0])
#define SCHEME_VEC_BINDING_SCOPES(p) ((Scheme_Scope_Set *)(SCHEME_VEC_ELS(p)[1]))
#define SCHEME_VEC_BINDING_VAL(p)   (SCHEME_VEC_ELS(p)[2])

#define CONV_RETURN_UNLESS(p) if (!p) return

static void check_for_conversion(Scheme_Object *sym,
                                 Scheme_Scope *scope,
                                 Scheme_Module_Phase_Exports *pt,
                                 Scheme_Hash_Table *collapse_table,
                                 Scheme_Hash_Tree *ht,
                                 Scheme_Scope_Set *scopes,
                                 Scheme_Object *phase,
                                 Scheme_Object *bind)
/* Due to `require` macros, importing a whole module can turn into
   individual imports from the module. Detect when everything that a
   module exports (at a given phase) is imported as a set of bindings,
   and collapse them to a bulk-import "pes". */
{
  Scheme_Hash_Table *mht;
  Scheme_Object *v, *v2, *cnt;
  int i;

  mht = (Scheme_Hash_Table *)scheme_eq_hash_get(collapse_table, (Scheme_Object *)scope);
  if (!mht) {
    mht = scheme_make_hash_table(SCHEME_hash_ptr);
    scheme_hash_set(collapse_table, (Scheme_Object *)scope, (Scheme_Object *)mht);
  }
  
  cnt = scheme_eq_hash_get(mht, (Scheme_Object *)pt);
  if (!cnt)
    cnt = scheme_make_integer(1);
  else
    cnt = scheme_bin_plus(cnt, scheme_make_integer(1));
  scheme_hash_set(mht, (Scheme_Object *)pt, cnt);

  if (bind && (SCHEME_INT_VAL(cnt) == pt->num_provides)) {
    Scheme_Object *modidx, *modidx2, *insp_desc, *insp_desc2, *src_phase;
    Scheme_Object *exportname, *nominal_modidx, *nominal_modidx2, *mod_phase, *nominal_name;
    Scheme_Object *nominal_src_phase;
    Scheme_Object *pes;

    nominal_modidx = NULL;

    extract_module_binding_parts(SCHEME_BINDING_VAL(bind), phase,
                                 &insp_desc,
                                 &modidx,
                                 &exportname,
                                 &nominal_modidx,
                                 &mod_phase,
                                 NULL,
                                 NULL,
                                 NULL);

    if (!nominal_modidx)
      nominal_modidx = modidx;

    /* since we've mapped N identifiers from a source of N identifiers,
       maybe we mapped all of them. */
    for (i = pt->num_provides; i--; ) {
      v2 = scheme_eq_hash_tree_get(ht, pt->provides[i]);
      CONV_RETURN_UNLESS(v2);

      /* For now, allow only a single binding: */
      CONV_RETURN_UNLESS(SCHEME_PAIRP(v2)
                         || (SCHEME_MPAIRP(v2) && SCHEME_NULLP(SCHEME_CDR(v2))));
      if (SCHEME_MPAIRP(v2))
        v2 = SCHEME_CAR(v2);
      
      CONV_RETURN_UNLESS(scopes_equal(scopes, SCHEME_BINDING_SCOPES(v2)));
      
      /* Pull apart module bindings to make sure they're consistent: */
      exportname = pt->provides[i];
      nominal_modidx2 = NULL;
      mod_phase = pt->phase_index;
      nominal_name = exportname;
      src_phase = scheme_make_integer(0);
      nominal_src_phase = NULL;
      mod_phase = pt->phase_index;
      
      extract_module_binding_parts(SCHEME_BINDING_VAL(v2), phase,
                                   &insp_desc2,
                                   &modidx,
                                   &exportname,
                                   &nominal_modidx2,
                                   &mod_phase,
                                   &nominal_name,
                                   &src_phase,
                                   &nominal_src_phase);

      if (!nominal_modidx2)
        nominal_modidx2 = modidx;
      if (!nominal_src_phase)
        nominal_src_phase = mod_phase;

      CONV_RETURN_UNLESS(SAME_OBJ(insp_desc2, insp_desc));
      modidx2 = (pt->provide_srcs ? pt->provide_srcs[i] : scheme_false);
      if (SCHEME_FALSEP(modidx2))
        modidx2 = nominal_modidx;
      else if (pt->src_modidx)
        modidx2 = scheme_modidx_shift(modidx2, pt->src_modidx, nominal_modidx);
      CONV_RETURN_UNLESS(scheme_equal(modidx, modidx2));
      CONV_RETURN_UNLESS(SAME_OBJ(exportname, pt->provide_src_names[i]));
      CONV_RETURN_UNLESS(scheme_equal(nominal_modidx2, nominal_modidx));
      CONV_RETURN_UNLESS(scheme_eqv(mod_phase, (pt->provide_src_phases
                                    ? scheme_make_integer(pt->provide_src_phases[i])
                                    : pt->phase_index)));
      CONV_RETURN_UNLESS(SAME_OBJ(nominal_name, pt->provides[i]));
      CONV_RETURN_UNLESS(scheme_eqv(src_phase, phase));
      CONV_RETURN_UNLESS(scheme_eqv(nominal_src_phase, pt->phase_index));
    }
    
    /* found a match; convert to a pes: */
    pes = scheme_make_vector(5, NULL);
    SCHEME_VEC_ELS(pes)[0] = nominal_modidx;
    SCHEME_VEC_ELS(pes)[1] = (Scheme_Object *)pt;
    SCHEME_VEC_ELS(pes)[2] = phase;
    SCHEME_VEC_ELS(pes)[3] = pt->phase_index;
    SCHEME_VEC_ELS(pes)[4] = insp_desc;

    bind = scheme_make_pair((Scheme_Object *)scopes, pes);
        
    /* install pes: */
    v = scope->bindings;
    if (!SCHEME_RPAIRP(v)) {
      STX_ASSERT(SCHEME_HASHTRP(v));
      v = scheme_make_raw_pair(v, NULL);
      scope->bindings = v;
    }
    v = scheme_make_raw_pair(bind, SCHEME_CDR(v));
    SCHEME_CDR(scope->bindings) = v;

    /* remove per-symbol bindings: */
    for (i = pt->num_provides; i--; ) {
      ht = scheme_hash_tree_set(ht, pt->provides[i], NULL);
    }
    SCHEME_CAR(scope->bindings) = (Scheme_Object *)ht;
  }
}

static Scheme_Object *replace_matching_scopes(Scheme_Object *l, Scheme_Scope_Set *scopes)
/* Takes a list of scope--value pairs for a binding table and removes
   any match to `scopes` */
{
  Scheme_Object *p;
  int c = 0;

  if (SCHEME_PAIRP(l)) {
    /* only one item to check */
    if (scopes_equal(scopes, SCHEME_BINDING_SCOPES(l)))
      return NULL;
    else
      return l;
  }

  for (p = l; !SCHEME_NULLP(p); p = SCHEME_CDR(p)) {
    if (scopes_equal(scopes, SCHEME_BINDING_SCOPES(SCHEME_CAR(p)))) {
      break;
    }
    c++;
  }

  if (SCHEME_NULLP(p))
    return l;

  p = SCHEME_CDR(p);
  while (c--) {
    p = scheme_make_pair(SCHEME_CAR(l), p);
    l = SCHEME_CDR(l);
  }

  /* down to one item? */
  if (SCHEME_NULLP(SCHEME_CDR(p)))
    return SCHEME_CAR(p);

  /* no items? */
  if (SCHEME_NULLP(p))
    return NULL;

  return p;
}

static void clear_matching_bindings(Scheme_Object *pes,
                                    Scheme_Scope_Set *scopes,
                                    Scheme_Object *l)
/* a new bulk import needs to override any individual imports; this
   should only matter for top-level interactions, since modules only
   allow shadowing of the initial bulk import */
{
  Scheme_Hash_Tree *excepts;
  Scheme_Object *prefix;
  Scheme_Module_Phase_Exports *pt;
  Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)SCHEME_CAR(l), *new_ht;
  Scheme_Object *key, *val, *new_val;
  intptr_t i;

  pt = (Scheme_Module_Phase_Exports *)SCHEME_VEC_ELS(pes)[1];

  if (!pt->ht) {
    /* Lookup table (which is created lazily) not yet created, so do that now... */
    scheme_populate_pt_ht(pt);
  }

  new_ht = ht;

  excepts = extract_unmarshal_excepts(SCHEME_VEC_ELS(pes)[3]);
  prefix = extract_unmarshal_prefix(SCHEME_VEC_ELS(pes)[3]);

  if ((ht->count < pt->ht->count)
      || SCHEME_TRUEP(prefix)
      || excepts) {
    /* faster to scan per-symbol binding table */
    i = -1;
    while ((i = scheme_hash_tree_next(ht, i)) != -1) {
      scheme_hash_tree_index(ht, i, &key, &val);
      if (scheme_eq_hash_get(pt->ht, unmarshal_lookup_adjust(key, pes))) {
        new_val = replace_matching_scopes(val, scopes);
        if (!SAME_OBJ(val, new_val))
          new_ht = scheme_hash_tree_set(new_ht, key, new_val);
      }
    }
  } else {
    /* faster to scan export table */
    for (i = pt->ht->size; i--; ) {
      if (pt->ht->vals[i]) {
        key = pt->ht->keys[i];
        val = scheme_eq_hash_tree_get(new_ht, key);
        if (val) {
          new_val = replace_matching_scopes(val, scopes);
          if (!SAME_OBJ(val, new_val))
            new_ht = scheme_hash_tree_set(new_ht, key, new_val);
        }
      }
    }
  }

  if (!SAME_OBJ(new_ht, ht))
    SCHEME_CAR(l) = (Scheme_Object *)new_ht;
}

XFORM_NONGCING static void save_old_value(Scheme_Object *mp, Scheme_Object *old_val)
{
  if (SCHEME_MPAIRP(old_val))
    SCHEME_CAR(mp) = SCHEME_CAR(old_val);
  else
    SCHEME_CAR(mp) = old_val;
}

static void add_binding(Scheme_Object *sym, Scheme_Object *phase, Scheme_Scope_Set *scopes,
                        Scheme_Object *val,
                        Scheme_Module_Phase_Exports *from_pt, /* to detect collapse conversion */
                        Scheme_Hash_Table *collapse_table) /* to triggere collapse detection */
/* `val` can be a symbol (local binding), a modidx/pair/#f
   (module/global binding), a shared-binding vector (i.e., a pes), or
   a syntax object (for a `free-identifier=?` equivalence) to be
   mutable-paired with the existing binding; the `sym` argument should
   be NULL when `val` is a shared-binding vector */

{
  Scheme_Hash_Tree *ht;
  Scheme_Scope *scope;
  Scheme_Object *l, *p, *bind;

  if (scope_set_count(scopes)) {
    /* We add the binding to the maximum-valued scope, because it's
       likely to be in the least number of binding sets so far. */
    scope = extract_max_scope(scopes);
    if (SAME_OBJ((Scheme_Object*)scope, root_scope))
      scheme_signal_error("internal error: cannot bind with only a root scope");
  } else {
    scheme_signal_error("internal error: cannot bind identifier with an empty context");
    return;
  }
  STX_ASSERT(SCHEME_STXP(val)
             || SCHEME_FALSEP(val)
             || SCHEME_MODIDXP(val)
             || SCHEME_PAIRP(val)
             || SCHEME_VECTORP(val)
             || SCHEME_SYMBOLP(val));

  if (SCHEME_STXP(val))
    val = scheme_make_mutable_pair(scheme_false, scheme_make_pair(val, phase));

  l = scope->bindings;
  if (!l) {
    if (sym) {
      /* simple case: a single binding */
      STX_ASSERT(SCHEME_SYMBOLP(sym));
      bind = make_vector3(sym, (Scheme_Object *)scopes, val);
      scope->bindings = bind;
      clear_binding_cache_for(sym);
      if (from_pt) {
        /* don't convert, but record addition for potential conversion */
        check_for_conversion(sym, scope, from_pt, collapse_table, NULL, scopes, phase, NULL);
      }
      return;
    }
    ht = empty_hash_tree;
    l = scheme_make_raw_pair((Scheme_Object *)ht, NULL);
    scope->bindings = l;
  } else if (SCHEME_VECTORP(l)) {
    /* convert simple case to more general case */
    ht = scheme_hash_tree_set(empty_hash_tree,
                              SCHEME_VEC_BINDING_KEY(l),
                              scheme_make_pair((Scheme_Object *)SCHEME_VEC_BINDING_SCOPES(l),
                                               SCHEME_VEC_BINDING_VAL(l)));
    if (sym) {
      /* more complex case: table of bindings */
      scope->bindings = (Scheme_Object *)ht;
    } else {
      /* need most complex form */
      l = scheme_make_raw_pair((Scheme_Object *)ht, NULL);
      scope->bindings = l;
    }
  } else if (SCHEME_RPAIRP(l)) {
    /* already in complex form */
    ht = (Scheme_Hash_Tree *)SCHEME_CAR(l);
  } else {
    STX_ASSERT(SCHEME_HASHTRP(l));
    ht = (Scheme_Hash_Tree *)l;
    if (!sym) {
      /* need most complex form */
      l = scheme_make_raw_pair((Scheme_Object *)ht, NULL);
      scope->bindings = l;
    }
  }

  bind = scheme_make_pair((Scheme_Object *)scopes, val);

  if (sym) {
    STX_ASSERT(SCHEME_SYMBOLP(sym));
    clear_binding_cache_for(sym);
    l = scheme_eq_hash_tree_get(ht, sym);
    if (!l) {
      ht = scheme_hash_tree_set(ht, sym, bind);
      if (SCHEME_RPAIRP(scope->bindings))
        SCHEME_CAR(scope->bindings) = (Scheme_Object *)ht;
      else
        scope->bindings = (Scheme_Object *)ht;
    } else {
      if (!SCHEME_MPAIRP(l))
        l = scheme_make_mutable_pair(l, scheme_null);
      for (p = l; !SCHEME_NULLP(p); p = SCHEME_CDR(p)) {
        if (scopes_equal(scopes, SCHEME_BINDING_SCOPES(SCHEME_CAR(p)))) {
          if (SCHEME_MPAIRP(val))
            save_old_value(val, SCHEME_BINDING_VAL(SCHEME_CAR(p)));
          SCHEME_CAR(p) = bind;
          break;
        }
      }
      if (SCHEME_NULLP(p)) {
        l = scheme_make_mutable_pair(bind, l);
        ht = scheme_hash_tree_set(ht, sym, l);
      } else if (SCHEME_NULLP(SCHEME_CDR(l))) {
        ht = scheme_hash_tree_set(ht, sym, SCHEME_CAR(l));
        from_pt = NULL; /* single binding; no benefit from pes conversion */
      }

      if (SCHEME_RPAIRP(scope->bindings))
        SCHEME_CAR(scope->bindings) = (Scheme_Object *)ht;
      else
        scope->bindings = (Scheme_Object *)ht;
    }
    if (from_pt)
      check_for_conversion(sym, scope, from_pt, collapse_table, ht, scopes, phase, bind);
  } else {
    /* Order matters: the new bindings should hide any existing bindings for the same name. */
    clear_binding_cache();
    p = scheme_make_raw_pair(bind, SCHEME_CDR(l));
    SCHEME_CDR(l) = p;

    /* Remove any matching mappings form the hash table, since it gets checked first. */
    clear_matching_bindings(val, scopes, l);
  }
}

void scheme_add_local_binding(Scheme_Object *o, Scheme_Object *phase, Scheme_Object *binding_sym)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;

  STX_ASSERT(SCHEME_SYMBOLP(binding_sym));

  add_binding(stx->val, phase, extract_scope_set(stx, phase), binding_sym, NULL, NULL);
}

static void do_add_module_binding(Scheme_Scope_Set *scopes, Scheme_Object *localname, Scheme_Object *phase,
                                  Scheme_Object *modidx, Scheme_Object *exname, Scheme_Object *defn_phase,
                                  Scheme_Object *insp_desc,
                                  Scheme_Object *nominal_mod, Scheme_Object *nominal_ex,
                                  Scheme_Object *src_phase, 
                                  Scheme_Object *nom_phase,
                                  Scheme_Module_Phase_Exports *from_pt,
                                  Scheme_Hash_Table *collapse_table)
{
  Scheme_Object *elem;
  int mod_phase;

  if (SCHEME_FALSEP(modidx)) {
    if (SAME_OBJ(localname, exname))
      add_binding(localname, phase, scopes, scheme_false, NULL, NULL);
    else
      add_binding(localname, phase, scopes, scheme_make_pair(scheme_false, exname), NULL, NULL);
    return;
  }

  STX_ASSERT(SCHEME_MODIDXP(modidx));

 /*
   This encoding is meant to be progressively less compact for
   progressively less-common cases:

   binding ::= mod_binding
   .        |  (cons inspector-desc mod_binding)
   mod_binding ::=  modidx         ; mod-phase = 0
   .            |   (cons modidx exportname)
   .            |   (cons modidx nominal_modidx)
   .            |   (list* modidx exportname nominal_modidx_plus_phase nominal_exportname)
   .            |   (list* modidx mod-phase exportname nominal_modidx_plus_phase nominal_exportname)
   nominal_modix_plus_phase ::= nominal_modix ; import-phase-level is 0, nom-phase = mod-phase
   .                         |  (cons nominal_modix import_phase_plus_nominal_phase)
   import_phase_plus_nominal_phase ::= import-phase-level   ; nom-phase = mod-phase
   .                                |  (cons import-phase-level nom-phase)
   inspector-desc = inspector
   .             | symbol
 */

  mod_phase = SCHEME_INT_VAL(defn_phase);

  if (!src_phase)
    src_phase = phase;
  if (!nom_phase)
    nom_phase = scheme_make_integer(mod_phase);

  if (SAME_OBJ(modidx, nominal_mod)
      && SAME_OBJ(exname, nominal_ex)
      && !mod_phase
      && same_phase(src_phase, scheme_make_integer(0))
      && same_phase(nom_phase, scheme_make_integer(mod_phase))) {
    if (SAME_OBJ(localname, exname))
      elem = modidx;
    else
      elem = CONS(modidx, exname);
  } else if (SAME_OBJ(exname, nominal_ex)
	     && SAME_OBJ(localname, exname)
	     && !mod_phase
             && same_phase(src_phase, scheme_make_integer(0))
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
      if (same_phase(src_phase, scheme_make_integer(0)))
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

  if (!SCHEME_FALSEP(insp_desc))
    elem = CONS(insp_desc, elem);

  add_binding(localname, phase, scopes, elem, from_pt, collapse_table);
}

void extract_module_binding_parts(Scheme_Object *l,
                                  Scheme_Object *phase,
                                  Scheme_Object **_insp_desc,      /* required */
                                  Scheme_Object **_modidx,         /* required */
                                  Scheme_Object **_exportname,     /* required, maybe unset */
                                  Scheme_Object **_nominal_modidx, /* maybe unset */
                                  Scheme_Object **_mod_phase,      /* required, maybe unset */
                                  Scheme_Object **_nominal_name,   /* maybe unset */
                                  Scheme_Object **_src_phase,      /* maybe unset */
                                  Scheme_Object **_nominal_src_phase) /* maybe unset */
/* unpack an encodings created by do_add_module_binding() */
{
  if (SCHEME_PAIRP(l)
      && SCHEME_INSPECTOR_DESCP(SCHEME_CAR(l))) {
    *_insp_desc = SCHEME_CAR(l);
    l = SCHEME_CDR(l);
  } else
    *_insp_desc = scheme_false;
        
  if (SCHEME_MODIDXP(l))
    *_modidx = l;
  else {
    *_modidx = SCHEME_CAR(l);
    l = SCHEME_CDR(l);

    if (SCHEME_SYMBOLP(l)) {
      /* l is exportname */
      *_exportname = l;
    } else if (SCHEME_MODIDXP(l)) {
      /* l is nominal_modidx */
      if (_nominal_modidx) *_nominal_modidx = l;
    } else {
      if (SCHEME_INTP(SCHEME_CAR(l)) || SCHEME_BIGNUMP(SCHEME_CAR(l))) {
        /* mod-phase before rest */
        *_mod_phase = SCHEME_CAR(l);
        l = SCHEME_CDR(l);
      }
      
      /* l is (list* exportname nominal_modidx_plus_phase nominal_exportname) */
      *_exportname = SCHEME_CAR(l);
      l = SCHEME_CDR(l);
      if (_nominal_name)
        *_nominal_name = SCHEME_CDR(l);
      l = SCHEME_CAR(l);
      /* l is nominal_modidx_plus_phase */
      if (SCHEME_PAIRP(l)) {
        if (_nominal_modidx) *_nominal_modidx = SCHEME_CAR(l);
        l = SCHEME_CDR(l);
        if (SCHEME_PAIRP(l)) {
          if (_src_phase) *_src_phase = SCHEME_CAR(l);
          if (_nominal_src_phase) *_nominal_src_phase = SCHEME_CDR(l);
        } else {
          if (_src_phase) *_src_phase = l;
          if (_nominal_src_phase) *_nominal_src_phase = *_mod_phase;
        }
      } else {
        if (_nominal_modidx) *_nominal_modidx = l;
      }
    }
  }
}

void scheme_add_module_binding(Scheme_Object *o, Scheme_Object *phase, 
                               Scheme_Object *modidx, Scheme_Object *inspector,
                               Scheme_Object *sym, Scheme_Object *defn_phase)
{
  STX_ASSERT(SCHEME_SYMBOLP(((Scheme_Stx *)o)->val));

  do_add_module_binding(extract_scope_set((Scheme_Stx *)o, phase), SCHEME_STX_VAL(o), phase,
                        modidx, sym, defn_phase,
                        inspector,
                        modidx, sym,
                        NULL, NULL,
                        NULL, NULL);
}

void scheme_add_module_binding_w_nominal(Scheme_Object *o, Scheme_Object *phase,
                                         Scheme_Object *modidx, Scheme_Object *defn_name, Scheme_Object *defn_phase,
                                         Scheme_Object *inspector,
                                         Scheme_Object *nominal_mod, Scheme_Object *nominal_name,
                                         Scheme_Object *nominal_import_phase, 
                                         Scheme_Object *nominal_export_phase,
                                         Scheme_Module_Phase_Exports *from_pt,
                                         Scheme_Hash_Table *collapse_table)
{
  STX_ASSERT(SCHEME_STXP(o));
  do_add_module_binding(extract_scope_set((Scheme_Stx *)o, phase), SCHEME_STX_VAL(o), phase,
                        modidx, defn_name, defn_phase,
                        inspector,
                        nominal_mod, nominal_name,
                        nominal_import_phase, nominal_export_phase,
                        from_pt, collapse_table);
}

/******************** debug-info ********************/

static Scheme_Object *scopes_to_printed_list(Scheme_Scope_Set *scopes)
{
  Scheme_Object *l, *val, *key;
  
  l = scopes_to_sorted_list(scopes);
  val = scheme_null;
  for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    key = SCHEME_CAR(l);
    val = scheme_make_pair(scheme_scope_printed_form(key), val);
  }

  return val;
}

Scheme_Object *add_bindings_info(Scheme_Object *bindings, Scheme_Object *key, Scheme_Object *l,
                                 Scheme_Stx *stx, int all_bindings, Scheme_Object *seen)
{
  Scheme_Hash_Tree *bind_desc;
  Scheme_Object *val;

  if (SCHEME_PAIRP(l)) {
    l = scheme_make_mutable_pair(l, scheme_null);
  }

  while (!SCHEME_NULLP(l)) {
    if (all_bindings || SAME_OBJ(key, stx->val)) {
      bind_desc = empty_hash_tree;
      bind_desc = scheme_hash_tree_set(bind_desc, name_symbol, key);

      val = SCHEME_BINDING_VAL(SCHEME_CAR(l));
      if (SCHEME_MPAIRP(val)) {
        bind_desc = scheme_hash_tree_set(bind_desc, free_symbol,
                                         stx_debug_info((Scheme_Stx *)SCHEME_CAR(SCHEME_CDR(val)),
                                                        SCHEME_CDR(SCHEME_CDR(val)),
                                                        scheme_make_pair((Scheme_Object *)stx, seen),
                                                        all_bindings));
        val = SCHEME_CAR(val);
      }

      if (SCHEME_SYMBOLP(val))
        bind_desc = scheme_hash_tree_set(bind_desc, local_symbol, val);
      else {
        if (SCHEME_PAIRP(val)) {
          if (SCHEME_INSPECTOR_DESCP(SCHEME_CAR(val)))
            val = SCHEME_CDR(val);
          val = SCHEME_CAR(val);
        }
        if (SCHEME_MODIDXP(val))
          val = apply_modidx_shifts(stx->shifts, val, NULL, NULL);
        bind_desc = scheme_hash_tree_set(bind_desc, module_symbol, val);
      }

      bind_desc = scheme_hash_tree_set(bind_desc, context_symbol,
                                       scopes_to_printed_list(SCHEME_BINDING_SCOPES(SCHEME_CAR(l))));

      bindings = scheme_make_pair((Scheme_Object *)bind_desc, bindings);
    }

    l = SCHEME_CDR(l);
  }

  return bindings;
}

#ifdef DO_STACK_CHECK
static Scheme_Object *stx_debug_info_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Stx *stx = (Scheme_Stx *)p->ku.k.p1;
  Scheme_Object *phase = (Scheme_Object *)p->ku.k.p2;
  Scheme_Object *seen = (Scheme_Object *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return stx_debug_info(stx, phase, seen, p->ku.k.i1);
}
#endif

static Scheme_Object *stx_debug_info(Scheme_Stx *stx, Scheme_Object *phase, Scheme_Object *seen, int all_bindings)
{
  Scheme_Hash_Tree *desc, *bind_desc;
  Scheme_Hash_Tree *ht;
  Scheme_Object *key, *val, *l, *pes, *descs = scheme_null, *bindings;
  intptr_t i, j;
  Scheme_Scope *scope;
  Scheme_Scope_Set *scopes;
  Scheme_Module_Phase_Exports *pt;
  Scheme_Object *multi_scopes;
  
#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;

      p->ku.k.p1 = (void *)stx;
      p->ku.k.p2 = (void *)phase;
      p->ku.k.p3 = (void *)seen;
      p->ku.k.i1 = all_bindings;

      return scheme_handle_stack_overflow(stx_debug_info_k);
    }
  }
#endif

  {
    int up = 0;
    for (l = seen; !SCHEME_NULLP(l); l = SCHEME_CDR(l), up++) {
      if (SAME_OBJ((Scheme_Object *)stx, SCHEME_CAR(l))) {
        return scheme_make_pair(cycle_symbol,
                                scheme_make_pair(scheme_make_integer(up),
                                                 scheme_null));
      }
    }
  }

  multi_scopes = stx->scopes->multi_scopes;

  /* Loop for top-level fallbacks: */
  while (1) {
    scopes = extract_scope_set_from_scope_list(stx->scopes->simple_scopes, multi_scopes, phase);

    desc = empty_hash_tree;

    if (SCHEME_SYMBOLP(stx->val))
      desc = scheme_hash_tree_set(desc, name_symbol, stx->val);
    desc = scheme_hash_tree_set(desc, context_symbol, scopes_to_printed_list(scopes));

    /* Describe other bindings */
    bindings = scheme_null;
    i = scope_set_next(scopes, -1);
    while (i != -1) {
      scope_set_index(scopes, i, &key, &val);

      scope = (Scheme_Scope *)key;
      if (scope->bindings) {
        if (SCHEME_VECTORP(scope->bindings)) {
          l = scheme_make_pair((Scheme_Object *)SCHEME_VEC_BINDING_SCOPES(scope->bindings),
                               SCHEME_VEC_BINDING_VAL(scope->bindings));
          bindings = add_bindings_info(bindings, SCHEME_VEC_BINDING_KEY(scope->bindings), l,
                                       stx, all_bindings, seen);
          l = NULL;
        } else {
          l = scope->bindings;
          if (SCHEME_RPAIRP(l))
            ht = (Scheme_Hash_Tree *)SCHEME_CAR(scope->bindings);
          else {
            STX_ASSERT(SCHEME_HASHTRP(l));
            ht = (Scheme_Hash_Tree *)l;
          }

          j = -1;
          while ((j = scheme_hash_tree_next(ht, j)) != -1) {
            scheme_hash_tree_index(ht, j, &key, &val);
            bindings = add_bindings_info(bindings, key, val, stx, all_bindings, seen);
          }

          l = scope->bindings;
          if (SCHEME_RPAIRP(l))
            l = SCHEME_CDR(l);
          else
            l = NULL;
        }

        while (l) {
          STX_ASSERT(SCHEME_RPAIRP(l));

          bind_desc = empty_hash_tree;

          bind_desc = scheme_hash_tree_set(bind_desc, context_symbol,
                                           scopes_to_printed_list(SCHEME_BINDING_SCOPES(SCHEME_CAR(l))));

          pes = SCHEME_BINDING_VAL(SCHEME_CAR(l));
          val = SCHEME_VEC_ELS(pes)[0];
          if (SCHEME_MODIDXP(val))
            val = apply_modidx_shifts(stx->shifts, val, NULL, NULL);
          bind_desc = scheme_hash_tree_set(bind_desc, module_symbol, val);

          if (PES_UNMARSHAL_DESCP(pes)) {
            /* unmarshal hasn't happened */
          } else {
            pt = (Scheme_Module_Phase_Exports *)SCHEME_VEC_ELS(pes)[1];
            if (!pt->ht)
              scheme_populate_pt_ht(pt);

            if (scheme_eq_hash_get(pt->ht, unmarshal_lookup_adjust(stx->val, pes)))
              bind_desc = scheme_hash_tree_set(bind_desc, matchp_symbol, scheme_true);
            else
              bind_desc = scheme_hash_tree_set(bind_desc, matchp_symbol, scheme_false);
          }

          bindings = scheme_make_pair((Scheme_Object *)bind_desc, bindings);
          
          l = SCHEME_CDR(l);
        }
      }
    
      i = scope_set_next(scopes, i);
    }

    if (!SCHEME_NULLP(bindings))
      desc = scheme_hash_tree_set(desc, bindings_symbol, scheme_reverse(bindings));

    descs = scheme_make_pair((Scheme_Object *)desc, descs);
    
    if (SCHEME_FALLBACKP(multi_scopes)) {
      multi_scopes = SCHEME_FALLBACK_REST(multi_scopes);
    } else
      break;
  }

  if (SCHEME_NULLP(SCHEME_CDR(descs)))
    return SCHEME_CAR(descs);
  else {
    descs = scheme_reverse(descs);
    return (Scheme_Object *)scheme_hash_tree_set((Scheme_Hash_Tree *)SCHEME_CAR(descs),
                                                 fallbacks_symbol,
                                                 SCHEME_CDR(descs));
  }
}

void scheme_stx_debug_print(Scheme_Object *_stx, Scheme_Object *phase, int level)
{
  Scheme_Stx *stx = (Scheme_Stx *)_stx;
  Scheme_Object *info;

  STX_ASSERT(SCHEME_STXP(_stx));

  info = stx_debug_info(stx, phase, scheme_null, level > 1);
  if (!level) {
    info = scheme_hash_tree_get((Scheme_Hash_Tree *)info, context_symbol);
    if (!info) info = scheme_false;
  }

  printf("%s at phase %s:\n",
         scheme_write_to_string(stx->val, NULL),
         scheme_write_to_string(phase, NULL));
  printf("  %s\n",
         scheme_write_to_string(info, NULL));
}

static void fprint_string(Scheme_Object *o, const char *s)
{
  (void)scheme_put_byte_string("describe", o, s, 0, strlen(s), 1);
}

static void fprint_label_string(Scheme_Object *o, int rename_level, Scheme_Object *rename_sym, const char *s)
{
  fprint_string(o, "\n  ");
  if (rename_level) {
    while (rename_level--) {
      fprint_string(o, "=");
    }
    fprint_string(o, "> ");
    scheme_write(rename_sym, o);
    fprint_string(o, " ");
  }
  fprint_string(o, s);
}

static void write_context(Scheme_Object *l, Scheme_Object *o)
{
  intptr_t col = 2, len;
  char *s;
  
  while (!SCHEME_NULLP(l)) {
    s = scheme_write_to_string(SCHEME_CAR(l), &len);
    if ((col > 2) && (col + len + 1 > 80)) {
      col = 2;
      fprint_string(o, "\n  ");
    }
    fprint_string(o, " ");
    scheme_put_byte_string("describe", o, s, 0, len, 1);
    col += len;

    l = SCHEME_CDR(l);
  }
}

static int context_matches(Scheme_Object *l1, Scheme_Object *l2)
/* Check whether the sorted list l2 is a subset of the sorted list l1 */
{
  while (!SCHEME_NULLP(l2)) {
    if (SCHEME_NULLP(l1))
      return 0;

    if (scheme_equal(SCHEME_CAR(l1), SCHEME_CAR(l2))) {
      l1 = SCHEME_CDR(l1);
      l2 = SCHEME_CDR(l2);
    } else
      l1 = SCHEME_CDR(l1);
  }
  
  return 1;
}

static Scheme_Object *describe_bindings(Scheme_Object *o, Scheme_Object *di,
                                        int rename_level, Scheme_Object *rename_sym,
                                        int always)
{
  Scheme_Object *l, *report, *val, *free_id;
  Scheme_Hash_Tree *dit, *bt;
  int fallback;

  fallback = 0;
  while (!SCHEME_NULLP(di)) {
    if (SCHEME_PAIRP(di))
      dit = (Scheme_Hash_Tree *)SCHEME_CAR(di);
    else
      dit = (Scheme_Hash_Tree *)di;

    l = scheme_hash_tree_get(dit, bindings_symbol);
    if (l) {
      report = scheme_null;
      while (!SCHEME_NULLP(l)) {
        bt = (Scheme_Hash_Tree *)SCHEME_CAR(l);

        val = scheme_hash_tree_get(bt, matchp_symbol);
        
        if ((val && SCHEME_TRUEP(val))
            || scheme_hash_tree_get(bt, name_symbol))
          report = scheme_make_pair((Scheme_Object *)bt, report);

        l = SCHEME_CDR(l);
      }

      if (!SCHEME_NULLP(report) || always) {
        if (!o)
          o = scheme_make_byte_string_output_port();

        fprint_label_string(o, rename_level, rename_sym, "context");
        if (fallback) {
          fprint_string(o, " at layer ");
          scheme_display(scheme_make_integer(fallback), o);
        }
        fprint_string(o, "...:\n  ");
        write_context(scheme_hash_tree_get(dit, context_symbol), o);

        while (!SCHEME_NULLP(report)) {
          bt = (Scheme_Hash_Tree *)SCHEME_CAR(report);

          if (context_matches(scheme_hash_tree_get(dit, context_symbol),
                              scheme_hash_tree_get(bt, context_symbol)))
            fprint_label_string(o, rename_level, rename_sym, "matching binding");
          else
            fprint_label_string(o, rename_level, rename_sym, "other binding");
          if (fallback) {
            fprint_string(o, " at layer ");
            scheme_display(scheme_make_integer(fallback), o);
          }
          fprint_string(o, "...:\n   ");
          val = scheme_hash_tree_get(bt, module_symbol);
          if (!val) {
            fprint_string(o, "local ");
            val = scheme_hash_tree_get(bt, local_symbol);
          }
          scheme_write(val, o);
          fprint_string(o, "\n  ");
          write_context(scheme_hash_tree_get(bt, context_symbol), o);

          free_id = scheme_hash_tree_get(bt, free_symbol);
          if (free_id) {
            fprint_string(o, "\n   free-identifier=? to ");
            if (SCHEME_PAIRP(free_id)
                && SAME_OBJ(SCHEME_CAR(free_id), cycle_symbol)) {
              int up = SCHEME_INT_VAL(SCHEME_CAR(SCHEME_CDR(free_id)));
              if (!up)
                fprint_string(o, "[cycle to self]");
              else {
                fprint_string(o, "[cycle, up ");
                scheme_write(scheme_make_integer(up), o);
                fprint_string(o, " levels]");
              }
            } else {
              if (SCHEME_HASHTRP(free_id))
                val = scheme_hash_tree_get((Scheme_Hash_Tree *)free_id, name_symbol);
              else
                val = NULL;
              if (val) {
                scheme_write(val, o);
                o = describe_bindings(o, free_id, rename_level + 1, val, always);
              } else {
                fprint_string(o, "[unknown]");
              }
            }
          }
        
          report = SCHEME_CDR(report);
        }
      }
    }

    if (SCHEME_PAIRP(di))
      di = SCHEME_CDR(di);
    else {
      di = scheme_hash_tree_get(dit, fallbacks_symbol);
      if (!di)
        di = scheme_null;
    }
    fallback++;
  }

  return o;
}

char *scheme_stx_describe_context(Scheme_Object *stx, Scheme_Object *phase, int always)
{
  Scheme_Object *di, *o = NULL;
  intptr_t len;
  char *r;

  if (!stx)
    return "";

  di = stx_debug_info((Scheme_Stx *)stx, phase, scheme_null, 0);

  o = describe_bindings(o, di, 0, NULL, always);

  if (o) {
    r = scheme_get_sized_byte_string_output(o, &len);
    /* make sure error buffer is allocated large enough: */
    scheme_ensure_max_symbol_length(len);
    return r;
  }
  else
    return "";
}

static void add_scopes_mapped_names(Scheme_Scope_Set *scopes, Scheme_Hash_Table *mapped)
{
  int retry;
  Scheme_Hash_Tree *ht;
  Scheme_Object *key, *val, *l, *pes;
  intptr_t i, j;
  Scheme_Scope *scope;
  Scheme_Scope_Set *binding_scopes;
  Scheme_Module_Phase_Exports *pt;

  do {
    retry = 0;
    i = scope_set_next(scopes, -1);
    while (i != -1) {
      scope_set_index(scopes, i, &key, &val);
      
      scope = (Scheme_Scope *)key;
      if (scope->bindings) {
        if (SCHEME_VECTORP(scope->bindings)) {
          if (scope_subset(SCHEME_VEC_BINDING_SCOPES(scope->bindings), scopes))
            scheme_hash_set(mapped, SCHEME_VEC_BINDING_KEY(scope->bindings), scheme_true);
        } else {
          /* Check table of symbols */
          if (SCHEME_RPAIRP(scope->bindings))
            ht = (Scheme_Hash_Tree *)SCHEME_CAR(scope->bindings);
          else {
            STX_ASSERT(SCHEME_HASHTRP(scope->bindings));
            ht = (Scheme_Hash_Tree *)scope->bindings;
          }
          j = -1;
          while ((j = scheme_hash_tree_next(ht, j)) != -1) {
            scheme_hash_tree_index(ht, j, &key, &val);
            l = val;
            if (l) {
              if (SCHEME_PAIRP(l)) {
                if (scope_subset(SCHEME_BINDING_SCOPES(l), scopes))
                  scheme_hash_set(mapped, key, scheme_true);
              } else {
                while (!SCHEME_NULLP(l)) {
                  STX_ASSERT(SCHEME_MPAIRP(l));
                  if (scope_subset(SCHEME_BINDING_SCOPES(SCHEME_CAR(l)), scopes)) {
                    scheme_hash_set(mapped, key, scheme_true);
                    break;
                  }
                  l = SCHEME_CDR(l);
                }
              }
            }
          }
        }

        /* Check list of shared-binding tables */
        if (SCHEME_RPAIRP(scope->bindings))
          l = SCHEME_CDR(scope->bindings);
        else
          l = NULL;
        while (l) {
          STX_ASSERT(SCHEME_RPAIRP(l));
          binding_scopes = SCHEME_BINDING_SCOPES(SCHEME_CAR(l));
          if (scope_subset(binding_scopes, scopes)) {
            pes = SCHEME_BINDING_VAL(SCHEME_CAR(l));
            if (PES_UNMARSHAL_DESCP(pes)) {
              if (SCHEME_TRUEP(SCHEME_VEC_ELS(pes)[0])) {
                unmarshal_module_context_additions(NULL, pes, binding_scopes, l);
                retry = 1;
              }
            } else {
              pt = (Scheme_Module_Phase_Exports *)SCHEME_VEC_ELS(pes)[1];
              if (!pt->ht)
                scheme_populate_pt_ht(pt);
              for (j = pt->ht->size; j--; ) {
                if (pt->ht->vals[j]) {
                  val = unmarshal_key_adjust(pt->ht->keys[j], pes);
                  if (val)
                    scheme_hash_set(mapped, val, scheme_true);
                }
              }
            }
          }
          l = SCHEME_CDR(l);
        }
      }
      i = scope_set_next(scopes, i);
    }
  } while (retry);
}

/******************** lookup ********************/

static Scheme_Object *do_stx_lookup(Scheme_Stx *stx, Scheme_Scope_Set *scopes,
                                    Scheme_Scope_Set *check_subset,
                                    GC_CAN_IGNORE int *_exact_match,
                                    GC_CAN_IGNORE int *_ambiguous,
                                    GC_CAN_IGNORE Scheme_Object **_sole_result)
/* the core lookup operation: walk through an identifier's marks,
   and walk through the bindings attached to each of those marks */
{
  int j, invalid, matches = 0;
  intptr_t i;
  Scheme_Object *key, *val, *result_best_so_far, *l, *pes;
  Scheme_Scope *scope;
  Scheme_Scope_Set *binding_scopes, *best_so_far;
  Scheme_Module_Phase_Exports *pt;

  do {
    invalid = 0; /* to indicate retry if we unmarshal */
    best_so_far = NULL;
    result_best_so_far = NULL;

    i = scope_set_next(scopes, -1);
    while ((i != -1) && !invalid) {
      scope_set_index(scopes, i, &key, &val);

      scope = (Scheme_Scope *)key;
      if (scope->bindings) {
        for (j = 0; j < 2; j++) {
          l = scope->bindings;
          if (!j) {
            if (SCHEME_VECTORP(l)) {
              if (!SAME_OBJ(SCHEME_VEC_BINDING_KEY(l), stx->val))
                l = NULL;
              /* l is NULL or a vector-form binding */
            } else if (SCHEME_HASHTRP(l)) {
              l = scheme_eq_hash_tree_get((Scheme_Hash_Tree *)l, stx->val);
              /* l is a pair or mlist */
            } else {
              STX_ASSERT(SCHEME_RPAIRP(l));
              l = scheme_eq_hash_tree_get((Scheme_Hash_Tree *)SCHEME_CAR(l),
                                          stx->val);
              /* l is a pair or mlist */
            }
          } else {
            if (SCHEME_RPAIRP(l))
              l = SCHEME_CDR(l);
            else
              l = NULL;
            /* l is an rlist */
          }

          /* l can have many different forms; see above */

          while (l && !SCHEME_NULLP(l) && !invalid) {
            if (SCHEME_VECTORP(l))
              binding_scopes = SCHEME_VEC_BINDING_SCOPES(l);
            else if (SCHEME_PAIRP(l))
              binding_scopes = SCHEME_BINDING_SCOPES(l);
            else {
              STX_ASSERT(SCHEME_RPAIRP(l) || SCHEME_MPAIRP(l));
              binding_scopes = SCHEME_BINDING_SCOPES(SCHEME_CAR(l));
            }

            if (j) {
              STX_ASSERT(SCHEME_RPAIRP(l));
              pes = SCHEME_BINDING_VAL(SCHEME_CAR(l));
              if (PES_UNMARSHAL_DESCP(pes)) {
                /* Not a pes; an unmarshal */
                if (SCHEME_TRUEP(SCHEME_VEC_ELS(pes)[0])) {
                  /* Need unmarshal --- but only if the scope set is relevant */
                  if (scope_subset(binding_scopes, scopes)) {
                    /* unmarshal and note that we must restart */
                    unmarshal_module_context_additions(stx, pes, binding_scopes, l);
                    invalid = 1;
                    /* Shouldn't encounter this on a second pass: */
                    STX_ASSERT(!check_subset);
                  }
                }
                binding_scopes = NULL;
              } else {
                /* Check for id in pes */
                pt = (Scheme_Module_Phase_Exports *)SCHEME_VEC_ELS(pes)[1];
                if (!pt->ht) {
                  /* Lookup table (which is created lazily) not yet created, so do that now... */
                  scheme_populate_pt_ht(pt);
                }

                if (!scheme_eq_hash_get(pt->ht, unmarshal_lookup_adjust(stx->val, pes)))
                  binding_scopes = NULL;
              }
            }
          
            if (binding_scopes && scope_subset(binding_scopes, scopes)) {
              if (check_subset && !scope_subset(binding_scopes, check_subset)) {
                if (_ambiguous) *_ambiguous = 1;
                return NULL; /* ambiguous */
              }
              matches++;
              if (!best_so_far
                  || ((scope_set_count(binding_scopes) > scope_set_count(best_so_far))
                      && (!check_subset
                          || (scope_set_count(binding_scopes) == scope_set_count(check_subset))))) {
                best_so_far = binding_scopes;
                if (SCHEME_VECTORP(l))
                  result_best_so_far = SCHEME_VEC_BINDING_VAL(l);
                else if (SCHEME_PAIRP(l))
                  result_best_so_far = SCHEME_BINDING_VAL(l);
                else {
                  STX_ASSERT(SCHEME_RPAIRP(l) || SCHEME_MPAIRP(l));
                  result_best_so_far = SCHEME_BINDING_VAL(SCHEME_CAR(l));
                }
                STX_ASSERT(SCHEME_FALSEP(result_best_so_far)
                           || SCHEME_MODIDXP(result_best_so_far)
                           || SCHEME_PAIRP(result_best_so_far)
                           || SCHEME_VECTORP(result_best_so_far)
                           || SCHEME_SYMBOLP(result_best_so_far)
                           || SCHEME_MPAIRP(result_best_so_far));
                if (_exact_match) *_exact_match = (scope_set_count(binding_scopes) == scope_set_count(scopes));
              }
            }

            if (SCHEME_RPAIRP(l) || SCHEME_MPAIRP(l))
              l = SCHEME_CDR(l);
            else
              l = NULL;
          }
        }
      }
    
      i = scope_set_next(scopes, i);
    }
  } while (invalid);

  if (!best_so_far)
    return NULL;

  if (check_subset)
    return result_best_so_far;
  else {
    if (matches == 1)
      *_sole_result = result_best_so_far;
    else
      *_sole_result = NULL;
    return (Scheme_Object *)best_so_far;
  }
}

static Scheme_Object *do_stx_lookup_nonambigious(Scheme_Stx *stx, Scheme_Object *phase,
                                                 GC_CAN_IGNORE int *_exact_match,
                                                 GC_CAN_IGNORE int *_ambiguous,
                                                 Scheme_Scope_Set **_binding_scopes)
{
  Scheme_Scope_Set *scopes, *best_set;
  Scheme_Object *multi_scopes, *result;

  multi_scopes = stx->scopes->multi_scopes;

  /* Loop for top-level fallbacks: */
  while (1) {
    scopes = extract_scope_set_from_scope_list(stx->scopes->simple_scopes, multi_scopes, phase);

    best_set = (Scheme_Scope_Set *)do_stx_lookup(stx, scopes,
                                                NULL,
                                                _exact_match, _ambiguous,
                                                &result);
    if (best_set) {
      if (_binding_scopes) *_binding_scopes = best_set;

      if (!result) {
        /* Find again, this time checking to ensure no ambiguity: */
        result = do_stx_lookup(stx, scopes,
                               best_set,
                               _exact_match, _ambiguous,
                               NULL);
      }

      if (!result && SCHEME_FALLBACKP(multi_scopes)) {
        if (_ambiguous) *_ambiguous = 0;
        if (_exact_match) *_exact_match = 0;
        multi_scopes = SCHEME_FALLBACK_REST(multi_scopes);
      } else
        return result;
    } else if (SCHEME_FALLBACKP(multi_scopes))
      multi_scopes = SCHEME_FALLBACK_REST(multi_scopes);
    else
      return NULL; 
  }
}

static Scheme_Object *apply_accumulated_shifts(Scheme_Object *result, Scheme_Object *prev_shifts, 
                                               GC_CAN_IGNORE Scheme_Object **_insp,
                                               GC_CAN_IGNORE Scheme_Object **nominal_modidx,
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
        o = apply_modidx_shifts(SCHEME_CAR(prev_shifts), SCHEME_VEC_ELS(result)[0], _insp, NULL);
        SCHEME_VEC_ELS(result)[0] = o;
        if (nominal_modidx) {
          o = apply_modidx_shifts(SCHEME_CAR(prev_shifts), *nominal_modidx, NULL, NULL);
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

  if (_insp && *_insp && SCHEME_SYMBOLP(*_insp))
    *_insp = scheme_false; /* wasn't shifted, for some reason */

  return result;
}

#define BINDING_CACHE_SIZE 32

typedef struct Binding_Cache_Entry {
  Scheme_Stx *id;
  Scheme_Object *phase;
  Scheme_Object *result;
  Scheme_Scope_Set *binding_scopes;
  Scheme_Object *insp_desc;
  Scheme_Object *free_eq;
} Binding_Cache_Entry;

static void init_binding_cache(void)
{
  REGISTER_SO(binding_cache_table);
  binding_cache_table = MALLOC_N_ATOMIC(Binding_Cache_Entry, BINDING_CACHE_SIZE);
}

static void clear_binding_cache(void)
{
  binding_cache_len = 0;
}

static void clear_binding_cache_for(Scheme_Object *sym)
{
  clear_binding_cache();
}

static void clear_binding_cache_stx(Scheme_Stx *stx)
{
  Binding_Cache_Entry *binding_cache = binding_cache_table;
  int i;

  for (i = binding_cache_len; i--; ) {
    if (SAME_OBJ(binding_cache[i].id, stx))
      binding_cache[i].id = NULL;
  }
}

XFORM_NONGCING static int find_in_binding_cache(Scheme_Stx *id, Scheme_Object *phase)
{
  Binding_Cache_Entry *binding_cache = binding_cache_table;
  int i;

  for (i = binding_cache_len; i--; ) {
    if (SAME_OBJ(binding_cache[i].id, id)
        && SAME_OBJ(binding_cache[i].phase, phase)){
      return i;
    }
  }

  return -1;
}

XFORM_NONGCING static void save_in_binding_cache(Scheme_Stx *id, Scheme_Object *phase,
                                                 Scheme_Object *result,
                                                 Scheme_Scope_Set *binding_scopes, Scheme_Object *insp_desc,
                                                 Scheme_Object *free_eq)
{
  Binding_Cache_Entry *binding_cache = binding_cache_table;
  int i;

  if (binding_cache_len < BINDING_CACHE_SIZE) {
    i = binding_cache_len++;
  } else if (binding_cache_pos < binding_cache_len) {
    i = binding_cache_pos;
    binding_cache_pos++;
  } else {
    i = 0;
    binding_cache_pos = 1;
  }

  binding_cache[i].id = id;
  binding_cache[i].phase = phase;
  binding_cache[i].result = result;
  binding_cache[i].binding_scopes = binding_scopes;
  binding_cache[i].insp_desc = insp_desc;
  binding_cache[i].free_eq = free_eq;
}

Scheme_Object *scheme_stx_lookup_w_nominal(Scheme_Object *o, Scheme_Object *phase, 
                                           int stop_at_free_eq,
                                           GC_CAN_IGNORE int *_exact_match,
                                           GC_CAN_IGNORE int *_ambiguous,
                                           GC_CAN_IGNORE Scheme_Scope_Set **_binding_scopes,
                                           GC_CAN_IGNORE Scheme_Object **_insp,             /* access-granting inspector */
                                           GC_CAN_IGNORE Scheme_Object **nominal_modidx,    /* how it was imported */
                                           GC_CAN_IGNORE Scheme_Object **nominal_name,      /* imported as name */
                                           GC_CAN_IGNORE Scheme_Object **src_phase,         /* phase level of import from nominal modidx */ 
                                           GC_CAN_IGNORE Scheme_Object **nominal_src_phase) /* phase level of export from nominal modidx */
/* Result is either a representation of a local binding (probably a symbol),
   a vector of the form (vector <modidx> <symbol> <defn-phase>), or
   #f */
{
  Scheme_Stx *stx;
  Scheme_Object *result, *insp_desc;
  Scheme_Scope_Set *binding_scopes;
  Scheme_Object *free_eq, *prev_shifts = scheme_null, *orig_name;
  Scheme_Hash_Table *free_id_seen = NULL;
  int cache_pos;

  STX_ASSERT(SCHEME_STXP(o));
  STX_ASSERT(nominal_name || (!src_phase && !nominal_src_phase));

  orig_name = SCHEME_STX_VAL(o);

  while (1) { /* loop for `free-identifier=?` chains */
    stx = (Scheme_Stx *)o;

    if (_ambiguous) *_ambiguous = 0;

    if (nominal_name)
      cache_pos = -1;
    else
      cache_pos = find_in_binding_cache(stx, phase);

    if (cache_pos >= 0) {
      /* must extract from cache before a GC: */
      GC_CAN_IGNORE Binding_Cache_Entry *binding_cache = binding_cache_table;

      result = binding_cache[cache_pos].result;
      binding_scopes = binding_cache[cache_pos].binding_scopes;
      if (_insp) *_insp = binding_cache[cache_pos].insp_desc;
      free_eq = binding_cache[cache_pos].free_eq;

      if (_binding_scopes)
        *_binding_scopes = binding_scopes;
      if (_exact_match) {
        if (binding_scopes
            && (scope_set_count(binding_scopes) == scope_set_count(extract_scope_set(stx, phase))))
          *_exact_match = 1;
        else
          *_exact_match = 0;
      }
      
      if (free_eq) {
        if (!stop_at_free_eq) {
          o = SCHEME_CAR(free_eq);
          phase = SCHEME_CDR(free_eq);
          /* recur to handle `free-identifier=?` chain */
          if (!free_id_seen)
            free_id_seen = scheme_make_hash_table(SCHEME_hash_ptr);
          if (scheme_eq_hash_get(free_id_seen, o))
            return scheme_false; /* found a cycle */
          scheme_hash_set(free_id_seen, o, scheme_true);
          prev_shifts = scheme_make_pair(stx->shifts, prev_shifts);
          continue;
        } else
          return apply_accumulated_shifts(result, prev_shifts, _insp, NULL,
                                          stx, orig_name, phase);
      } else
        return apply_accumulated_shifts(result, prev_shifts, _insp, NULL,
                                        stx, orig_name, phase);
    }

    binding_scopes = NULL;
    if (_exact_match) *_exact_match = 0;

    result = do_stx_lookup_nonambigious(stx, phase,
                                        _exact_match, _ambiguous,
                                        &binding_scopes);

    if (_binding_scopes) *_binding_scopes = binding_scopes;

    if (!result) {
      save_in_binding_cache(stx, phase, scheme_false,
                            NULL, NULL, NULL);
      return apply_accumulated_shifts(scheme_false, scheme_null, NULL, NULL,
                                      stx, orig_name, phase);
    }
    
    /*
      `result` can be:
      - a symbol for a lexical binding,
      - a pair, modidx, or #f for a module import
      - a vector for a pes (shared export table from a module)
      - a mutable pair of the above plus an identifier for a `free-identifier=?` link
    */
    if (SCHEME_MPAIRP(result)) {
      free_eq = SCHEME_CDR(result);
      result = SCHEME_CAR(result);
    } else
      free_eq = NULL;

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
        insp_desc = scheme_false;
      } else if (SCHEME_MODIDXP(l)) {
        SCHEME_VEC_ELS(result)[0] = l;
        insp_desc = scheme_false;
      } else if (SCHEME_PAIRP(l)) {
        /* A list for a module import */
        Scheme_Object *modidx;
        Scheme_Object *exportname = SCHEME_VEC_ELS(result)[1];
        Scheme_Object *mod_phase = SCHEME_VEC_ELS(result)[2];
        
        extract_module_binding_parts(l,
                                     SCHEME_VEC_ELS(result)[2],
                                     &insp_desc,
                                     &modidx,    /* required */
                                     &exportname,    /* required */
                                     nominal_modidx,
                                     &mod_phase, /* required */
                                     nominal_name,
                                     src_phase,
                                     nominal_src_phase);
        
        SCHEME_VEC_ELS(result)[0] = modidx;
        SCHEME_VEC_ELS(result)[1] = exportname;
        SCHEME_VEC_ELS(result)[2] = mod_phase;
      } else {
        /* A vector for a pes */
        Scheme_Module_Phase_Exports *pt;
        Scheme_Object *pos, *mod;

        STX_ASSERT(SCHEME_VECTORP(l));

        pt = (Scheme_Module_Phase_Exports *)SCHEME_VEC_ELS(l)[1];
        insp_desc = SCHEME_VEC_ELS(l)[4];

        pos = scheme_eq_hash_get(pt->ht, unmarshal_lookup_adjust(stx->val, l));

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

        if (src_phase) *src_phase = SCHEME_VEC_ELS(l)[2];
        if (nominal_src_phase) *nominal_src_phase = pt->phase_index;
      }

      if (nominal_name && !*nominal_name)
        *nominal_name = stx->val;
      if (nominal_modidx && !*nominal_modidx)
        *nominal_modidx = SCHEME_VEC_ELS(result)[0];
      if (src_phase && !*src_phase)
        *src_phase = scheme_make_integer(0);
      if (nominal_src_phase && !*nominal_src_phase)
        *nominal_src_phase = SCHEME_VEC_ELS(result)[2];
      
      l = apply_modidx_shifts(stx->shifts, SCHEME_VEC_ELS(result)[0], &insp_desc, NULL);
      SCHEME_VEC_ELS(result)[0] = l;

      if (nominal_modidx) {
        l = apply_modidx_shifts(stx->shifts, *nominal_modidx, NULL, NULL);
        *nominal_modidx = l;
      }
    } else
      insp_desc = scheme_false;

    save_in_binding_cache(stx, phase, result,
                          binding_scopes, insp_desc,
                          free_eq);
  
    if (_insp) *_insp = insp_desc;

    if (!free_eq || stop_at_free_eq)
      return apply_accumulated_shifts(result, prev_shifts, _insp, nominal_modidx,
                                      stx, orig_name, phase);

    /* Recur for `free-identifier=?` mapping */
    phase = SCHEME_CDR(free_eq);
    o = SCHEME_CAR(free_eq);
    prev_shifts = scheme_make_pair(stx->shifts, prev_shifts);

    if (!free_id_seen)
      free_id_seen = scheme_make_hash_table(SCHEME_hash_ptr);
    if (scheme_eq_hash_get(free_id_seen, o))
      return scheme_false; /* found a cycle */
  }
}

Scheme_Object *scheme_stx_lookup(Scheme_Object *o, Scheme_Object *phase)
{
  return scheme_stx_lookup_w_nominal(o, phase, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
}

Scheme_Object *scheme_stx_lookup_stop_at_free_eq(Scheme_Object *o, Scheme_Object *phase, int *_exact_match)
{
  return scheme_stx_lookup_w_nominal(o, phase, 1, _exact_match, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
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
  add_binding(stx->val, phase, extract_scope_set(stx, phase), from_o, NULL, NULL);
}

/******************** module contexts ********************/

/* A module context is a convenience record to track the scopes,
   inspector, etc. that are related to expanding a `module` form */

Scheme_Object *scheme_make_module_context(Scheme_Object *insp, 
                                          Scheme_Object *shift_or_shifts,
                                          Scheme_Object *debug_name)
{
  Scheme_Object *vec, *bx;
  Scheme_Object *body_scopes;
  Scheme_Object *intro_multi_scope;

  /* The `intro_multi_scope` is the home for all bindings in a given context.
     It is added to any form that emerges into a module context via
     macro expansion.
     In the case of top-level forms, this context is sometimes stripped away
     and replaced with a new top-level context. */
  intro_multi_scope = new_multi_scope(debug_name);
  body_scopes = scheme_make_pair(intro_multi_scope, scheme_null);

  /* An additional scope identifies the original module home of an
     identifier (i.e., not added to things that are macro-introduced
     into the module context). The root scope serves to unify all
     top-level contexts. */
  if (SCHEME_FALSEP(debug_name))
    body_scopes = scheme_make_pair(root_scope, body_scopes);
  else
    body_scopes = scheme_make_pair(scheme_new_scope(SCHEME_STX_MODULE_SCOPE), body_scopes);

  if (!shift_or_shifts)
    shift_or_shifts = scheme_null;
  else if (!SCHEME_PAIRP(shift_or_shifts) && !SCHEME_NULLP(shift_or_shifts))
    shift_or_shifts = scheme_make_pair(shift_or_shifts, scheme_null);

  /* A module context consists of
       - A list of scopes, multi-scopes, and (cons multi-scope phase) that
         corresponds to the module body
       - A phase used for extracting scopes (not a shift for the intro scope)
       - An inspector
       - A list of module-index shifts
       - A multi-scope for binding/introduction (included in body scopes)
       - A list of scopes that correspond to macro uses;
         this scopes must be stripped away from a definition
  */

  vec = scheme_make_vector(6, NULL);
  SCHEME_VEC_ELS(vec)[0] = body_scopes;
  SCHEME_VEC_ELS(vec)[1] = scheme_make_integer(0);
  SCHEME_VEC_ELS(vec)[2] = insp;
  SCHEME_VEC_ELS(vec)[3] = shift_or_shifts;
  SCHEME_VEC_ELS(vec)[4] = intro_multi_scope;
  bx = scheme_box((Scheme_Object *)empty_scope_set);
  SCHEME_VEC_ELS(vec)[5] = bx;

  return vec;
}

Scheme_Scope_Set *scheme_module_context_scopes(Scheme_Object *mc)
{
  Scheme_Object *body_scopes = SCHEME_VEC_ELS(mc)[0], *scope;
  Scheme_Object *phase = SCHEME_VEC_ELS(mc)[1];
  Scheme_Scope_Set *scopes = empty_scope_set;

  while (!SCHEME_NULLP(body_scopes)) {
    scope = SCHEME_CAR(body_scopes);
    if (!SCHEME_SCOPEP(scope)) {
      if (SCHEME_PAIRP(scope))
        scope = extract_simple_scope_from_shifted(scope, phase);
      else
        scope = extract_simple_scope(scope, phase);
    }
    if (scope)
      scopes = scope_set_set(scopes, scope, scheme_true);
    body_scopes = SCHEME_CDR(body_scopes);
  }

  return scopes;
}

Scheme_Object *scheme_module_context_frame_scopes(Scheme_Object *mc, Scheme_Object *keep_intdef_scopes)
{
  Scheme_Object *scopes;

  scopes = (Scheme_Object *)scheme_module_context_scopes(mc);

  if (keep_intdef_scopes)
    scopes = add_intdef_scopes_of(scopes, keep_intdef_scopes);

  return scopes;
}

void scheme_module_context_add_use_site_scope(Scheme_Object *mc, Scheme_Object *use_site_scope)
{
  Scheme_Scope_Set *use_site_scopes = (Scheme_Scope_Set *)SCHEME_BOX_VAL(SCHEME_VEC_ELS(mc)[5]);

  STX_ASSERT(SCHEME_SCOPEP(use_site_scope));

  use_site_scopes = scope_set_set(use_site_scopes, use_site_scope, scheme_true);
  
  SCHEME_BOX_VAL(SCHEME_VEC_ELS(mc)[5]) = (Scheme_Object *)use_site_scopes;
}

Scheme_Object *scheme_module_context_use_site_frame_scopes(Scheme_Object *mc)
{
  Scheme_Scope_Set *use_site_scopes;
  
  use_site_scopes = (Scheme_Scope_Set *)SCHEME_BOX_VAL(SCHEME_VEC_ELS(mc)[5]);
  if (SAME_OBJ(use_site_scopes, empty_scope_set))
    return NULL;
  else
    return make_vector3(scheme_false, (Scheme_Object *)use_site_scopes, scheme_false);
}

Scheme_Object *scheme_module_context_inspector(Scheme_Object *mc)
{
  return SCHEME_VEC_ELS(mc)[2];
}

void scheme_module_context_add_mapped_symbols(Scheme_Object *mc, Scheme_Hash_Table *mapped)
{
  add_scopes_mapped_names(scheme_module_context_scopes(mc), mapped);
}

Scheme_Object *scheme_module_context_at_phase(Scheme_Object *mc, Scheme_Object *phase)
{
  Scheme_Object *vec;

  /* Clones the module context, but with a different convenience phase */

  if (SAME_OBJ(SCHEME_VEC_ELS(mc)[1], phase))
    return mc;

  vec = scheme_make_vector(6, NULL);
  SCHEME_VEC_ELS(vec)[0] = SCHEME_VEC_ELS(mc)[0];
  SCHEME_VEC_ELS(vec)[1] = phase;
  SCHEME_VEC_ELS(vec)[2] = SCHEME_VEC_ELS(mc)[2];
  SCHEME_VEC_ELS(vec)[3] = SCHEME_VEC_ELS(mc)[3];
  SCHEME_VEC_ELS(vec)[4] = SCHEME_VEC_ELS(mc)[4];
  SCHEME_VEC_ELS(vec)[5] = SCHEME_VEC_ELS(mc)[5];

  return vec;
}

static Scheme_Object *adjust_module_context_except(Scheme_Object *stx, Scheme_Object *mc, Scheme_Object *skip,
                                                   int mode)
{
  Scheme_Object *body_scopes = SCHEME_VEC_ELS(mc)[0], *scope;
  Scheme_Object *phase = SCHEME_VEC_ELS(mc)[1];

  while (!SCHEME_NULLP(body_scopes)) {
    scope = SCHEME_CAR(body_scopes);
    if (skip && SAME_OBJ(scope, skip))
      scope = NULL;
    else if (!SCHEME_SCOPEP(scope)) {
      if (SCHEME_PAIRP(scope))
        scope = extract_simple_scope_from_shifted(scope, phase);
      else
        scope = extract_simple_scope(scope, phase);
    }
    if (scope)
      stx = scheme_stx_adjust_scope(stx, scope, phase, mode);
    body_scopes = SCHEME_CDR(body_scopes);
  }

  if (mode == SCHEME_STX_ADD)
    stx = scheme_stx_add_shifts(stx, SCHEME_VEC_ELS(mc)[3]);

  return stx;
}

Scheme_Object *scheme_stx_add_module_context(Scheme_Object *stx, Scheme_Object *mc)
{
  return adjust_module_context_except(stx, mc, NULL, SCHEME_STX_ADD);
}

Scheme_Object *scheme_stx_remove_module_context(Scheme_Object *stx, Scheme_Object *mc)
{
  return adjust_module_context_except(stx, mc, NULL, SCHEME_STX_REMOVE);
}

Scheme_Object *scheme_stx_push_module_context(Scheme_Object *stx, Scheme_Object *mc)
{
  Scheme_Object *intro_multi_scope = SCHEME_VEC_ELS(mc)[4];

  stx = scheme_stx_adjust_scope(stx, intro_multi_scope, scheme_make_integer(0), SCHEME_STX_PUSH);
  stx = adjust_module_context_except(stx, mc, intro_multi_scope, SCHEME_STX_ADD);

  return stx;
}

Scheme_Object *scheme_stx_push_introduce_module_context(Scheme_Object *stx, Scheme_Object *mc)
{
  Scheme_Object *intro_multi_scope = SCHEME_VEC_ELS(mc)[4];

  return scheme_stx_adjust_scope(stx, intro_multi_scope, scheme_make_integer(0), SCHEME_STX_PUSH);
}

Scheme_Object *scheme_stx_add_module_frame_context(Scheme_Object *stx, Scheme_Object *mc)
{
  return scheme_stx_add_module_context(stx, mc);
}

Scheme_Object *scheme_stx_introduce_to_module_context(Scheme_Object *stx, Scheme_Object *mc)
{
  Scheme_Object *multi_scope;

  STX_ASSERT(SCHEME_VECTORP(mc));

  multi_scope = SCHEME_VEC_ELS(mc)[4];

  return scheme_stx_add_scope(stx, multi_scope, scheme_make_integer(0));
}

Scheme_Object *scheme_stx_unintroduce_from_module_context(Scheme_Object *stx, Scheme_Object *mc)
{
  return adjust_module_context_except(stx, mc, NULL, SCHEME_STX_REMOVE);
}

Scheme_Object *scheme_stx_adjust_module_use_site_context(Scheme_Object *stx, Scheme_Object *mc, int mode)
{
  Scheme_Scope_Set *scopes = (Scheme_Scope_Set *)SCHEME_BOX_VAL(SCHEME_VEC_ELS(mc)[5]);

  return scheme_stx_adjust_scopes(stx, scopes, SCHEME_VEC_ELS(mc)[1], mode);
}

#ifdef DO_STACK_CHECK
static Scheme_Object *replace_scopes(Scheme_Object *stx, Scheme_Object *remove_scopes,
                                     Scheme_Object *add_scopes, Scheme_Object *phase);

static Scheme_Object *replace_scopes_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *stx = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object *remove_scopes = (Scheme_Object *)p->ku.k.p2;
  Scheme_Object *add_scopes = (Scheme_Object *)p->ku.k.p3;
  Scheme_Object *phase = (Scheme_Object *)p->ku.k.p4;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  return replace_scopes(stx, remove_scopes, add_scopes, phase);
}
#endif

static Scheme_Object *replace_scopes(Scheme_Object *stx, Scheme_Object *remove_scopes,
                                     Scheme_Object *add_scopes, Scheme_Object *phase)
{
  Scheme_Object *sym, *sym2, *content;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;

      p->ku.k.p1 = (void *)stx;
      p->ku.k.p2 = (void *)remove_scopes;
      p->ku.k.p3 = (void *)add_scopes;
      p->ku.k.p4 = (void *)phase;

      return scheme_handle_stack_overflow(replace_scopes_k);
    }
  }
#endif

  if (SCHEME_STXP(stx)) {
    int mutate = 0;

    scheme_stx_content(stx);
    if (HAS_SUBSTX(SCHEME_STX_VAL(stx))) {
      content = replace_scopes(SCHEME_STX_VAL(stx), remove_scopes, add_scopes, phase);
      sym = scheme_datum_to_syntax(scheme_false, scheme_false, stx, 0, 0);
    } else {
      sym = stx;
      content = SCHEME_STX_VAL(stx);
    }

    if (SCHEME_SCOPEP(remove_scopes) || SCHEME_MULTI_SCOPEP(remove_scopes))
      sym2 = stx_adjust_scope(sym, remove_scopes, phase, SCHEME_STX_REMOVE, &mutate);
    else
      sym2 = stx_adjust_scopes(sym, (Scheme_Scope_Set *)remove_scopes, phase, SCHEME_STX_REMOVE, &mutate);

    if (!SAME_OBJ(sym, sym2) || !SAME_OBJ(content, SCHEME_STX_VAL(stx))) {
      if (SCHEME_SCOPEP(add_scopes) || SCHEME_MULTI_SCOPEP(add_scopes))
        sym2 = stx_adjust_scope(sym2, add_scopes, phase, SCHEME_STX_ADD, &mutate);
      else
        sym2 = stx_adjust_scopes(sym2, (Scheme_Scope_Set *)add_scopes, phase, SCHEME_STX_ADD, &mutate);
      return scheme_datum_to_syntax(content, stx, sym2, 0, 2);
    } else
      return stx;
  } else if (SCHEME_NULLP(stx)) {
    return stx;
  } else if (SCHEME_PAIRP(stx)) {
    sym = replace_scopes(SCHEME_CAR(stx), remove_scopes, add_scopes, phase);
    sym2 = replace_scopes(SCHEME_CDR(stx), remove_scopes, add_scopes, phase);
    if (SAME_OBJ(sym, SCHEME_CAR(stx)) && SAME_OBJ(sym2, SCHEME_CDR(stx)))
      return stx;
    else
      return scheme_make_pair(sym, sym2);
  } else {
    scheme_signal_error("internal error: unsupported form for replace_scopes()");
    return NULL;
  }
}

Scheme_Object *scheme_stx_from_module_context_to_generic(Scheme_Object *stx, Scheme_Object *mc)
{
  /* remove the introduction scope, which should be everywhere, and
     map the other scopes to the root scope */
  Scheme_Object *scopes;
  stx = scheme_stx_remove_scope(stx, SCHEME_VEC_ELS(mc)[4], SCHEME_VEC_ELS(mc)[1]);
  scopes = (Scheme_Object *)scheme_module_context_scopes(mc);
  return replace_scopes(stx, scopes, root_scope, SCHEME_VEC_ELS(mc)[1]);
}

Scheme_Object *scheme_stx_from_generic_to_module_context(Scheme_Object *stx, Scheme_Object *mc)
{
  /* map the root scope to the body scope, and add the introduction
     scope everywhere */
  Scheme_Object *scopes;
  scopes = (Scheme_Object *)scheme_module_context_scopes(mc);
  stx = replace_scopes(stx, root_scope, scopes, SCHEME_VEC_ELS(mc)[1]);
  return scheme_stx_introduce_to_module_context(stx, mc);
}

void scheme_extend_module_context(Scheme_Object *mc,          /* (vector <scope-set> <phase> <inspector> ...) */
                                  Scheme_Object *ctx,         /* binding context (as stx) or NULL */
                                  Scheme_Object *modidx,      /* actual source module */
                                  Scheme_Object *localname,   /* name in local context */
                                  Scheme_Object *exname,      /* name in definition context  */
                                  Scheme_Object *nominal_mod, /* nominal source module */
                                  Scheme_Object *nominal_ex,  /* nominal import before local renaming */
                                  intptr_t mod_phase,         /* phase of source defn */
                                  Scheme_Object *src_phase,   /* nominal import phase */
                                  Scheme_Object *nom_phase)   /* nominal export phase */
{
  Scheme_Scope_Set *scopes;
  
  if (ctx)
    scopes = extract_scope_set((Scheme_Stx *)ctx, SCHEME_VEC_ELS(mc)[1]);
  else
    scopes = scheme_module_context_scopes(mc);
  
  do_add_module_binding(scopes, localname, SCHEME_VEC_ELS(mc)[1],
                        modidx, exname, scheme_make_integer(mod_phase),
                        SCHEME_VEC_ELS(mc)[2],
                        nominal_mod, nominal_ex,
                        src_phase, nom_phase,
                        NULL, NULL);
}

void scheme_extend_module_context_with_shared(Scheme_Object *mc, /* (vector <scope> <phase> <inspector> <shifts>) or (cons <phase> <inspector-desc>) */
                                              Scheme_Object *modidx,      
                                              Scheme_Module_Phase_Exports *pt,
                                              Scheme_Object *prefix, /* a sybmol; not included in `excepts` keys */
                                              Scheme_Hash_Tree *excepts, /* NULL => empty */
                                              Scheme_Object *src_phase, /* nominal import phase */
                                              Scheme_Object *context,
                                              Scheme_Object *replace_at)
/* create a bulk import */
{
  Scheme_Object *phase, *pes, *insp_desc, *unmarshal_info;
  Scheme_Scope_Set *scopes;

  if (SCHEME_VECTORP(mc)) {
    phase = SCHEME_VEC_ELS(mc)[1];
    insp_desc = SCHEME_VEC_ELS(mc)[2];
  } else {
    phase = SCHEME_CAR(mc);
    insp_desc = SCHEME_CDR(mc);
  }

  if (context)
    scopes = extract_scope_set((Scheme_Stx *)context, phase);
  else
    scopes = scheme_module_context_scopes(mc);

  unmarshal_info = make_unmarshal_info(pt->phase_index, prefix, (Scheme_Object *)excepts);
  
  pes = scheme_make_vector(5, NULL);
  SCHEME_VEC_ELS(pes)[0] = modidx;
  SCHEME_VEC_ELS(pes)[1] = (Scheme_Object *)pt;
  SCHEME_VEC_ELS(pes)[2] = src_phase;
  SCHEME_VEC_ELS(pes)[3] = unmarshal_info;
  SCHEME_VEC_ELS(pes)[4] = (insp_desc ? insp_desc : scheme_false);

  if (replace_at) {
    SCHEME_BINDING_VAL(SCHEME_CAR(replace_at)) = pes;
  } else {
    add_binding(NULL, phase, scopes, pes, NULL, NULL);
  }
}

static Scheme_Object *make_unmarshal_info(Scheme_Object *phase,
                                          Scheme_Object *prefix,
                                          Scheme_Object *excepts)
{
  Scheme_Object *unmarshal_info;
  
  /* unmarshal_info = phase 
     .              | (cons phase adjusts)
     adjusts = prefix
     .       | (cons excepts-ht prefix)
     .       | excepts-list
     excepts-ht = (hasheq symbol #t ... ...)
  */
  unmarshal_info = prefix;
  if (excepts) {
    if (SCHEME_FALSEP(unmarshal_info))
      unmarshal_info = excepts;
    else
      unmarshal_info = scheme_make_pair(excepts, prefix);
  }
  if (SCHEME_FALSEP(unmarshal_info))
    unmarshal_info = phase;
  else
    unmarshal_info = scheme_make_pair(phase, unmarshal_info);

  return unmarshal_info;
}

XFORM_NONGCING static Scheme_Object *extract_unmarshal_phase(Scheme_Object *unmarshal_info)
{
  if (SCHEME_PAIRP(unmarshal_info))
    return SCHEME_CAR(unmarshal_info);
  else
    return unmarshal_info;
}

XFORM_NONGCING static Scheme_Object *extract_unmarshal_prefix(Scheme_Object *unmarshal_info)
{
  if (SCHEME_PAIRP(unmarshal_info)) {
    unmarshal_info = SCHEME_CDR(unmarshal_info);
    if (SCHEME_PAIRP(unmarshal_info))
      unmarshal_info = SCHEME_CDR(unmarshal_info);

    if (SCHEME_SYMBOLP(unmarshal_info))
      return unmarshal_info;
    else
      return scheme_false;
  } else
    return scheme_false;
}

static Scheme_Hash_Tree *unmarshal_vector_to_excepts(Scheme_Object *unmarshal_info,
                                                     Scheme_Object *ht_target,
                                                     int ht_to_cdr)
{
  Scheme_Hash_Tree *ht = empty_hash_tree;
  intptr_t i;

  for (i = SCHEME_VEC_SIZE(unmarshal_info); i--; ) {
    if (SCHEME_SYMBOLP(SCHEME_VEC_ELS(unmarshal_info)[i]))
      ht = scheme_hash_tree_set(ht, SCHEME_VEC_ELS(unmarshal_info)[i], scheme_true);
  }

  if (ht_to_cdr)
    SCHEME_CDR(ht_target) = (Scheme_Object *)ht;
  else
    SCHEME_CAR(ht_target) = (Scheme_Object *)ht;

  return ht;
}

static Scheme_Hash_Tree *extract_unmarshal_excepts(Scheme_Object *unmarshal_info)
{
  if (SCHEME_PAIRP(unmarshal_info)) {
    Scheme_Object *ht_target = unmarshal_info;
    int ht_to_cdr = 1;

    unmarshal_info = SCHEME_CDR(unmarshal_info);
    if (SCHEME_PAIRP(unmarshal_info)) {
      ht_target = unmarshal_info;
      ht_to_cdr = 0;
      unmarshal_info = SCHEME_CAR(unmarshal_info);
    }

    if (SCHEME_HASHTRP(unmarshal_info))
      return (Scheme_Hash_Tree *)unmarshal_info;
    else if (SCHEME_VECTORP(unmarshal_info)) {
      /* Hash table was converted to a vector in a marshaled unmarshal request */
      return unmarshal_vector_to_excepts(unmarshal_info, ht_target, ht_to_cdr);
    } else
      return NULL;
  } else
    return NULL;
}

static Scheme_Object *unmarshal_excepts_to_vector(Scheme_Object *unmarshal_info)
{
  Scheme_Hash_Tree *ht;
  
  ht = extract_unmarshal_excepts(unmarshal_info);
  if (ht) {
    intptr_t i = -1, j = 0;
    Scheme_Object *vec, *key, *val;

    vec = scheme_make_vector(ht->count, NULL);

    while ((i = scheme_hash_tree_next(ht, i)) != -1) {
      scheme_hash_tree_index(ht, i, &key, &val);
      SCHEME_VEC_ELS(vec)[j++] = key;
    }

    sort_vector_symbols(vec);

    return make_unmarshal_info(extract_unmarshal_phase(unmarshal_info),
                               extract_unmarshal_prefix(unmarshal_info),
                               vec);
  }

  return unmarshal_info;
}

static Scheme_Object *unmarshal_lookup_adjust(Scheme_Object *sym, Scheme_Object *pes)
{
  Scheme_Hash_Tree *excepts;
  Scheme_Object *prefix;

  if (!SCHEME_SYMBOLP(sym))
    return scheme_false;

  excepts = extract_unmarshal_excepts(SCHEME_VEC_ELS(pes)[3]);
  prefix = extract_unmarshal_prefix(SCHEME_VEC_ELS(pes)[3]);
 
  if (SCHEME_TRUEP(prefix) && !SCHEME_SYM_WEIRDP(sym)) {
    int plen = SCHEME_SYM_LEN(prefix);
    if (SCHEME_SYM_LEN(sym) >= plen) {
      if (!scheme_strncmp(SCHEME_SYM_VAL(sym), SCHEME_SYM_VAL(prefix), plen)) {
        char buf[64], *b;
        int slen = SCHEME_SYM_LEN(sym) - plen;
        if (slen < 64)
          b = buf;
        else
          b = scheme_malloc_atomic(slen+1);
        memcpy(b, SCHEME_SYM_VAL(sym) + plen, slen+1);
        sym = scheme_intern_exact_symbol(b, slen);
      } else
        return scheme_false; /* so lookup will fail */
    } else
      return scheme_false;
  }

  if (excepts) {
    if (scheme_eq_hash_tree_get(excepts, sym))
      return scheme_false; /* so lookup will fail */
  }

  return sym;
}

static Scheme_Object *unmarshal_key_adjust(Scheme_Object *sym, Scheme_Object *pes)
{
  Scheme_Hash_Tree *excepts;
  Scheme_Object *prefix;
  
  excepts = extract_unmarshal_excepts(SCHEME_VEC_ELS(pes)[3]);
  prefix = extract_unmarshal_prefix(SCHEME_VEC_ELS(pes)[3]);

  if (excepts && scheme_eq_hash_tree_get(excepts, sym))
    return NULL;

  if (SCHEME_TRUEP(prefix)) {
    int plen = SCHEME_SYM_LEN(prefix);
    int slen = SCHEME_SYM_LEN(sym) + plen;
    char buf[64], *b;

    if (slen < 64)
      b = buf;
    else
      b = scheme_malloc_atomic(slen+1);
    memcpy(b, SCHEME_SYM_VAL(prefix), plen);
    memcpy(b+plen, SCHEME_SYM_VAL(sym), SCHEME_SYM_LEN(sym)+1);
    sym = scheme_intern_exact_symbol(b, slen);
  }

  return sym;
}

static void unmarshal_module_context_additions(Scheme_Stx *stx, Scheme_Object *vec, Scheme_Scope_Set *scopes, Scheme_Object *replace_at)
{
  Scheme_Object *req_modidx, *modidx, *unmarshal_info, *context, *src_phase, *pt_phase, *bind_phase;
  Scheme_Object *insp, *req_insp;
  Scheme_Hash_Table *export_registry;

  req_modidx = SCHEME_VEC_ELS(vec)[0];
  insp = SCHEME_VEC_ELS(vec)[3];
  req_insp = insp;

  if (stx) {
    modidx = apply_modidx_shifts(stx->shifts, req_modidx, &insp, &export_registry);
  } else {
    modidx = req_modidx;
    export_registry = NULL;
    insp = scheme_false;
    req_insp = scheme_false;
  }

  src_phase = SCHEME_VEC_ELS(vec)[1];
  unmarshal_info = SCHEME_VEC_ELS(vec)[2];
  pt_phase = extract_unmarshal_phase(unmarshal_info);

  SCHEME_VEC_ELS(vec)[0] = scheme_false;
  SCHEME_VEC_ELS(vec)[1] = scheme_false;
  SCHEME_VEC_ELS(vec)[2] = scheme_false;

  if (SCHEME_FALSEP(src_phase) || SCHEME_FALSEP(pt_phase))
    bind_phase = scheme_false;
  else
    bind_phase = scheme_bin_plus(src_phase, pt_phase);

  context = scheme_datum_to_syntax(scheme_false, scheme_false, scheme_false, 0, 0);
  context = scheme_stx_adjust_scopes(context, scopes, bind_phase, SCHEME_STX_ADD);

  scheme_do_module_context_unmarshal(modidx, req_modidx, context,
                                     bind_phase, pt_phase, src_phase,
                                     extract_unmarshal_prefix(unmarshal_info),
                                     extract_unmarshal_excepts(unmarshal_info),
                                     export_registry, insp, req_insp,
                                     replace_at);
}

Scheme_Object *scheme_module_context_to_stx(Scheme_Object *mc, Scheme_Object *orig_src)
{
  Scheme_Object *plain, *o, *for_intro, *vec;

  plain = scheme_datum_to_syntax(scheme_false, scheme_false, scheme_false, 0, 0);

  if (orig_src)
    o = scheme_datum_to_syntax(scheme_true, scheme_false, orig_src, 0, 0);
  else
    o = scheme_stx_add_module_context(plain, mc);

  /* Keep track of intro scope separately: */
  for_intro = scheme_stx_introduce_to_module_context(plain, mc);
  vec = scheme_make_vector(2, NULL);
  SCHEME_VEC_ELS(vec)[0] = o;
  SCHEME_VEC_ELS(vec)[1] = for_intro;
  return scheme_datum_to_syntax(vec, scheme_false, scheme_false, 0, 0);
}

Scheme_Object *scheme_stx_to_module_context(Scheme_Object *_stx)
{
  Scheme_Stx *stx = (Scheme_Stx *)_stx;
  Scheme_Object *vec, *shifts, *a, *body_scopes, *phase = scheme_make_integer(0);
  Scheme_Object *intro_multi_scope = NULL;

  if (SCHEME_VECTORP(stx->val) && (SCHEME_VEC_SIZE(stx->val) >= 2)) {
    (void)scheme_stx_content((Scheme_Object *)stx); /* propagate */
    intro_multi_scope = SCHEME_VEC_ELS(stx->val)[1];
    stx = (Scheme_Stx *)SCHEME_VEC_ELS(stx->val)[0];
  }

  shifts = stx->shifts;
  if (SCHEME_VECTORP(shifts))
    shifts = SCHEME_VEC_ELS(shifts)[0];

  phase = scheme_make_integer(0);

  body_scopes = scheme_null;
  a = stx->scopes->multi_scopes;
  if (SCHEME_FALLBACKP(a))
    a = SCHEME_FALLBACK_FIRST(a);
  for (; !SCHEME_NULLP(a); a = SCHEME_CDR(a)) {
    if (SAME_OBJ(phase, SCHEME_CDR(SCHEME_CAR(a))))
      body_scopes = scheme_make_pair(SCHEME_CAR(SCHEME_CAR(a)), body_scopes);
    else
      body_scopes = scheme_make_pair(SCHEME_CAR(a), body_scopes);
  }
  {
    Scheme_Object *key, *val;
    intptr_t i;
    i = -1;
    while ((i = scope_set_next(stx->scopes->simple_scopes, i)) != -1) {
      scope_set_index(stx->scopes->simple_scopes, i, &key, &val);
      body_scopes = scheme_make_pair(key, body_scopes);
    }
  }

  if (intro_multi_scope) {
    stx = (Scheme_Stx *)intro_multi_scope;
    if (!SCHEME_FALLBACKP(stx->scopes->multi_scopes)
        && SCHEME_PAIRP(stx->scopes->multi_scopes)) {
      intro_multi_scope = SCHEME_CAR(SCHEME_CAR(stx->scopes->multi_scopes));
    }
  }
  if (!intro_multi_scope) {
    /* This won't happen for a well-formed representation */
    intro_multi_scope = new_multi_scope(scheme_false);
  }
  
  vec = scheme_make_vector(6, NULL);
  SCHEME_VEC_ELS(vec)[0] = body_scopes;
  SCHEME_VEC_ELS(vec)[1] = phase;
  SCHEME_VEC_ELS(vec)[2] = scheme_false; /* not sure this is right */
  SCHEME_VEC_ELS(vec)[3] = shifts;
  SCHEME_VEC_ELS(vec)[4] = intro_multi_scope;
  a = scheme_box((Scheme_Object *)empty_scope_set);
  SCHEME_VEC_ELS(vec)[5] = a;

  return vec;
}

int scheme_stx_equal_module_context(Scheme_Object *other_stx, Scheme_Object *mc_or_stx)
{
  Scheme_Stx *stx;
  Scheme_Object *phase;
  
  if (SCHEME_STXP(mc_or_stx)) {
    stx = (Scheme_Stx *)mc_or_stx;
    if (SCHEME_VECTORP(stx->val) && (SCHEME_VEC_SIZE(stx->val) >= 2))
      stx = (Scheme_Stx *)SCHEME_VEC_ELS(stx->val)[0];
  } else {
    Scheme_Object *plain;
    plain = scheme_datum_to_syntax(scheme_false, scheme_false, scheme_false, 0, 0);
    mc_or_stx = scheme_stx_add_module_context(plain, mc_or_stx);
    stx = (Scheme_Stx *)mc_or_stx;
  }

  phase = scheme_make_integer(0);

  return scopes_equal(extract_scope_set((Scheme_Stx *)other_stx, phase),
                     extract_scope_set(stx, phase));
}

/******************** lazy syntax-object unmarshaling ********************/

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
  int mutate = 0;

  shift = o[0];

  if (!shift) return scheme_false; /* happens only with corrupted .zo! */

  rp = (Resolve_Prefix *)o[1];

  v = rp->stxes[i];

  if (SCHEME_INTP(v)) {
    scheme_load_delayed_syntax(rp, i);
    v = rp->stxes[i];
  }

  v = do_stx_add_shift(v, shift, &mutate);

  shift = SCHEME_VEC_ELS(shift)[3];
  if (!SCHEME_FALSEP(shift)) {
    /* need to propagate the inspector for dye packs, too */
    (void)set_false_insp((Scheme_Object *)v, shift, &mutate);
  }
  
  return v;
}

Scheme_Object *scheme_stx_force_delayed(Scheme_Object *stx)
{
  if (SCHEME_RPAIRP(stx))
    return scheme_load_delayed_code(SCHEME_INT_VAL(SCHEME_CAR(stx)),
                                    (struct Scheme_Load_Delay *)SCHEME_CDR(stx));
  else
    return stx;
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

  return scope_subset(extract_scope_set(bind, phase),
                     extract_scope_set(ref, phase));
}

int scheme_stx_free_eq3(Scheme_Object *a, Scheme_Object *b, 
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

int scheme_stx_free_eq2(Scheme_Object *a, Scheme_Object *b, Scheme_Object *phase)
{
  return scheme_stx_free_eq3(a, b, phase, phase);
}

int scheme_stx_free_eq(Scheme_Object *a, Scheme_Object *b, intptr_t phase)
{
  return scheme_stx_free_eq3(a, b, scheme_make_integer(phase), scheme_make_integer(phase));
}

int scheme_stx_free_eq_x(Scheme_Object *a, Scheme_Object *b, intptr_t b_phase)
{
  return scheme_stx_free_eq3(a, b, scheme_make_integer(0), scheme_make_integer(b_phase));
}

Scheme_Object *scheme_stx_get_free_eq_sym(Scheme_Object *a, Scheme_Object *phase)
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

  return scopes_equal(extract_scope_set(a, a_phase), extract_scope_set(b, b_phase));
}

int scheme_stx_bound_eq(Scheme_Object *a, Scheme_Object *b, Scheme_Object *phase)
{
  return scheme_stx_env_bound_eq2(a, b, phase, phase);
}

Scheme_Object *scheme_stx_source_module(Scheme_Object *stx, int resolve, int source)
{
  /* Look for the oldest "self" modidx that has a resolution: */
  Scheme_Object *l = ((Scheme_Stx *)stx)->shifts, *a, *src;
  Scheme_Hash_Table *export_registry;

  if (SCHEME_VECTORP(l))
    l = SCHEME_VEC_ELS(l)[0];

  l = scheme_reverse(l);
  
  while (!SCHEME_NULLP(l)) {
    a = SCHEME_CAR(l);
    if (SCHEME_VECTORP(a)) {
      src = SCHEME_VEC_ELS(a)[1];

      if (SCHEME_MODIDXP(src)) {
        if (SCHEME_FALSEP(((Scheme_Modidx *)src)->path)) {
          src = apply_modidx_shifts(((Scheme_Stx *)stx)->shifts, src,
                                    NULL, &export_registry);
          if (!SCHEME_FALSEP(((Scheme_Modidx *)src)->path)
              || !SCHEME_FALSEP(((Scheme_Modidx *)src)->resolved)) {
            if (resolve) {
              src = scheme_module_resolve(src, 0);
              if (export_registry && source) {
                a = scheme_hash_get(export_registry, src);
                if (a)
                  src = ((Scheme_Module_Exports *)a)->modsrc;
              }
              src = SCHEME_PTR_VAL(src);
            }
            return src;
          }
        }
      }
    }

    l = SCHEME_CDR(l);
  }

  return scheme_false;
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

static void sort_added_scopes(Scheme_Object *scopes, int added)
{
  Scheme_Object **a, *l;
  int i;

  if (!added)
    return;

  a = MALLOC_N(Scheme_Object *, added);
  for (i = 0, l = scopes; i < added; i++, l = SCHEME_CDR(l)) {
    a[i] = SCHEME_CAR(l);
  }

  sort_scope_array(a, added);

  for (i = 0, l = scopes; i < added; i++, l = SCHEME_CDR(l)) {
    SCHEME_CAR(l) = a[i];
  }
}

static void add_reachable_scopes(Scheme_Scope_Set *scopes, Scheme_Marshal_Tables *mt)
{
  intptr_t i, added = 0;
  Scheme_Object *key, *val;

  i = -1;
  while ((i = scope_set_next(scopes, i)) != -1) {
    scope_set_index(scopes, i, &key, &val);
    if (!scheme_eq_hash_get(mt->reachable_scopes, key)) {
      scheme_hash_set(mt->conditionally_reachable_scopes, key, NULL);
      scheme_hash_set(mt->reachable_scopes, key, scheme_true);
      val = scheme_make_pair(key, mt->reachable_scope_stack);
      mt->reachable_scope_stack = val;
      added++;
    }
  }

  sort_added_scopes(mt->reachable_scope_stack, added);
}

static void add_conditional_as_reachable(Scheme_Scope_Set *scopes, Scheme_Marshal_Tables *mt)
{
  int added = 0;
  intptr_t i;
  Scheme_Object *key, *val;

  STX_ASSERT(SCHEME_SCOPE_SETP(scopes));
  
  i = -1;
  while ((i = scope_set_next(scopes, i)) != -1) {
    scope_set_index(scopes, i, &key, &val);
    if (SCHEME_SCOPE_HAS_OWNER((Scheme_Scope *)key)
        && scheme_eq_hash_get(mt->conditionally_reachable_scopes, key)
        && !scheme_eq_hash_get(mt->reachable_scopes, key)) {
      scheme_hash_set(mt->conditionally_reachable_scopes, key, NULL);
      scheme_hash_set(mt->reachable_scopes, key, scheme_true);
      val = scheme_make_pair(key, mt->reachable_scope_stack);
      mt->reachable_scope_stack = val;
      added++;
    }
  }

  sort_added_scopes(mt->reachable_scope_stack, added);
}

static void add_reachable_multi_scope(Scheme_Object *ms, Scheme_Marshal_Tables *mt)
{
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)ms;
  Scheme_Scope_Set *binding_scopes = empty_scope_set;
  Scheme_Object *scope;
  int j;
  
  for (j = ht->size; j--; ) {
    scope = ht->vals[j];
    if (scope) {
      if (!MULTI_SCOPE_METAP(ht->keys[j])) {
        if (!scheme_eq_hash_get(mt->reachable_scopes, scope)
            && !scheme_eq_hash_get(mt->conditionally_reachable_scopes, scope)) {
          /* This scope is reachable via its multi-scope, but it only
             matters if it's reachable through a binding (otherwise it
             can be re-generated later). We don't want to keep a scope
             that can be re-generated, because pruning it makes
             compilation more deterministic relative to other
             compilations that involve a shared module. If the scope
             itself has any bindings, then we count it as reachable
             through a binding (which is an approxmation, because other scopes
             in the binding may be unreachable, but it seems good enough for
             determinism). */
          scheme_hash_set(mt->conditionally_reachable_scopes, scope, scheme_true);
          if (((Scheme_Scope *)scope)->bindings)
            binding_scopes = scope_set_set(binding_scopes, scope, scheme_true);
        }
      }
    }
  }

  if (!SAME_OBJ(binding_scopes, empty_scope_set))
    add_conditional_as_reachable(binding_scopes, mt);
}

static void add_reachable_multi_scopes(Scheme_Object *multi_scopes, Scheme_Marshal_Tables *mt)
{
  Scheme_Object *l;

  while (1) {
    l = multi_scopes;
    if (SCHEME_FALLBACKP(l))
      l = SCHEME_FALLBACK_FIRST(l);
    
    for (; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      add_reachable_multi_scope(SCHEME_CAR(SCHEME_CAR(l)), mt);
    }
    
    if (SCHEME_FALLBACKP(multi_scopes))
      multi_scopes = SCHEME_FALLBACK_REST(multi_scopes);
    else
      break;
  }
}

static Scheme_Object *any_unreachable_scope(Scheme_Scope_Set *scopes, Scheme_Marshal_Tables *mt,
                                            int check_conditionals)
{
  intptr_t i;
  int saw_conditional = 0;
  Scheme_Object *key, *val;

  i = -1;
  while ((i = scope_set_next(scopes, i)) != -1) {
    scope_set_index(scopes, i, &key, &val);
    if (!scheme_eq_hash_get(mt->reachable_scopes, key)) {
      if (check_conditionals && scheme_eq_hash_get(mt->conditionally_reachable_scopes, key))
        saw_conditional = 1;
      else
        return key;
    }
  }

  if (saw_conditional) {
    /* since this binding is reachable, move any conditional to reachable */
    add_conditional_as_reachable(scopes, mt);
  }

  return NULL;
}

static void possiblly_reachable_free_id(Scheme_Object *val, /* mpair or stx */
                                        Scheme_Scope_Set *scopes,
                                        Scheme_Marshal_Tables *mt)
{
  Scheme_Stx *free_id = (Scheme_Stx *)SCHEME_CAR(SCHEME_CDR(val));
  Scheme_Object *unreachable_scope, *l;
  Scheme_Hash_Table *ht;

  if (SCHEME_MPAIRP(val))
    free_id = (Scheme_Stx *)SCHEME_CAR(SCHEME_CDR(val));
  else
    free_id = (Scheme_Stx *)val;

  STX_ASSERT(SCHEME_STXP((Scheme_Object *)free_id));

  unreachable_scope = any_unreachable_scope(scopes, mt, 1);

  if (!unreachable_scope) {
    /* causes the free-id mapping's scopes to be reachable: */
    (void)wraps_to_datum(free_id, mt);
  } else {
    /* the mapping will become reachable only if `unreachable_scope` becomes reachable */
    if (!mt->pending_reachable_ids) {
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      mt->pending_reachable_ids = ht;
    }
    l = scheme_eq_hash_get(mt->pending_reachable_ids, unreachable_scope);
    if (!l) l = scheme_null;
    scheme_hash_set(mt->pending_reachable_ids, unreachable_scope,
                    scheme_make_pair(scheme_make_pair((Scheme_Object *)free_id,
                                                      (Scheme_Object *)scopes),
                                     l));
  }
}

static int all_symbols(Scheme_Object **a, int c)
{
  while (c--) {
    if (!SCHEME_SYMBOLP(a[c]))
      return 0;
  }
  return 1;
}

static int all_reals(Scheme_Object **a, int c)
{
  while (c--) {
    if (!SCHEME_REALP(a[c]))
      return 0;
  }
  return 1;
}

Scheme_Object **scheme_extract_sorted_keys(Scheme_Object *tree)
{
  intptr_t j, i, count;
  Scheme_Object **a, *key;

  if (SCHEME_HASHTRP(tree)) {
    Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)tree;

    count = ht->count;
    if (!count)
      return NULL;
    
    a = MALLOC_N(Scheme_Object *, count);
    
    j = -1;
    i = 0;
    while ((j = scheme_hash_tree_next(ht, j)) != -1) {
      scheme_hash_tree_index(ht, j, &key, NULL);
      a[i++] = key;
    }

    STX_ASSERT(i == count);
  } else {
    Scheme_Hash_Table *t = (Scheme_Hash_Table *)tree;

    count = t->count;
    
    if (!count)
      return NULL;

    a = MALLOC_N(Scheme_Object *, count);
    j = 0;
    
    for (i = t->size; i--; ) {
      if (t->vals[i]) {
        a[j++] = t->keys[i];
      }
    }

    STX_ASSERT(j == count);
  }

  if (SCHEME_SYMBOLP(a[0]) && all_symbols(a, count))
    sort_symbol_array(a, count);
  else if (SCHEME_SCOPEP(a[0]))
    sort_scope_array(a, count);
  else if (all_reals(a, count))
    sort_number_array(a, count);
  else
    return NULL;

  return a;
}

void scheme_iterate_reachable_scopes(Scheme_Marshal_Tables *mt)
{
  Scheme_Scope *scope;
  Scheme_Object *l, *val, *key, **sorted_keys, *pesl;
  Scheme_Hash_Tree *ht;
  intptr_t j, count;

  /* For each scope, recur on `free-identifier=?` mappings */
  while (!SCHEME_NULLP(mt->reachable_scope_stack)) {
    scope = (Scheme_Scope *)SCHEME_CAR(mt->reachable_scope_stack);
    mt->reachable_scope_stack = SCHEME_CDR(mt->reachable_scope_stack);

    if (scope->bindings) {
      val = scope->bindings;
      if (SCHEME_VECTORP(val)) {
        add_conditional_as_reachable(SCHEME_VEC_BINDING_SCOPES(val), mt);
        l = SCHEME_VEC_BINDING_VAL(val);
        if (SCHEME_MPAIRP(l)) {
          /* It's a free-id mapping: */
          possiblly_reachable_free_id(l, SCHEME_VEC_BINDING_SCOPES(val), mt);
        }
      } else {
        if (SCHEME_RPAIRP(val)) {
          ht = (Scheme_Hash_Tree *)SCHEME_CAR(val);
          pesl = SCHEME_CDR(val);
        } else {
          STX_ASSERT(SCHEME_HASHTRP(val));
          ht = (Scheme_Hash_Tree *)val;
          pesl = NULL;
        }
        sorted_keys = scheme_extract_sorted_keys((Scheme_Object *)ht);
        count = ht->count;
        for (j = 0; j < count; j++) {
          key = sorted_keys[j];
          val = scheme_hash_tree_get(ht, key);
          l = val;
          if (SCHEME_PAIRP(l)) {
            add_conditional_as_reachable(SCHEME_BINDING_SCOPES(l), mt);
            val = SCHEME_BINDING_VAL(l);
            if (SCHEME_MPAIRP(val)) {
              /* It's a free-id mapping: */
              possiblly_reachable_free_id(val, SCHEME_BINDING_SCOPES(l), mt);
            }
          } else {
            STX_ASSERT(SCHEME_MPAIRP(l));
            for (; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
              add_conditional_as_reachable(SCHEME_BINDING_SCOPES(SCHEME_CAR(l)), mt);
              val = SCHEME_BINDING_VAL(SCHEME_CAR(l));
              if (SCHEME_MPAIRP(val)) {
                /* It's a free-id mapping: */
                possiblly_reachable_free_id(val, SCHEME_BINDING_SCOPES(SCHEME_CAR(l)), mt);
              }
            }
          }
        }
        while (pesl) {
          STX_ASSERT(SCHEME_RPAIRP(pesl));
          val = SCHEME_CAR(pesl);
          STX_ASSERT(SCHEME_PAIRP(val));
          add_conditional_as_reachable((Scheme_Scope_Set *)SCHEME_CAR(val), mt);
          pesl = SCHEME_CDR(pesl);
        }
      }
    }

    /* Check for any free-id mappings whose reachbility depended on `scope`: */
    if (mt->pending_reachable_ids) {
      l = scheme_eq_hash_get(mt->pending_reachable_ids, (Scheme_Object *)scope);
      if (l) {
        scheme_hash_set(mt->pending_reachable_ids, (Scheme_Object *)scope, NULL);
        while (!SCHEME_NULLP(l)) {
          val = SCHEME_CAR(l);
          possiblly_reachable_free_id(SCHEME_CAR(val), (Scheme_Scope_Set *)SCHEME_CDR(val), mt);
          l = SCHEME_CDR(l);
        }
      }
    }
  }

  /* Adjust mapping so that each scope maps to its relative position: */
  {
    int i;
    sorted_keys = scheme_extract_sorted_keys((Scheme_Object *)mt->reachable_scopes);
    for (j = mt->reachable_scopes->count, i = 0; j--; i++) {
      STX_ASSERT(SCHEME_SCOPEP(sorted_keys[j]));
      scheme_hash_set(mt->reachable_scopes, sorted_keys[j], scheme_make_integer(i));
    }
  }
}

static Scheme_Object *intern_one(Scheme_Object *v, Scheme_Hash_Table *ht)
{
  Scheme_Object *result;
  
  result = scheme_hash_get(ht, v);
  if (!result) {
    result = scheme_make_marshal_shared(v);
    scheme_hash_set(ht, v, result);
  }

  return result;
}

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

static Scheme_Object *intern_fallback_tails(Scheme_Object *l, Scheme_Hash_Table *ht)
{
  Scheme_Object *r, *result;

  r = scheme_null;
  do {
    if (!SCHEME_FALLBACKP(l))
      result = l;
    else
      result = scheme_hash_get(ht, l);
    if (!result) {
      r = scheme_make_pair(SCHEME_FALLBACK_FIRST(l), r);
      l = SCHEME_FALLBACK_REST(l);
    }
  } while (!result);

  while (!SCHEME_NULLP(r)) {
    result = make_fallback_pair(SCHEME_CAR(r), result);
    l = make_fallback_pair(SCHEME_CAR(r), l);
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

static int compare_scopes_from_multi(Scheme_Scope *a, Scheme_Scope *b)
{
  Scheme_Scope_With_Owner *ao, *bo;
  
  ao = (Scheme_Scope_With_Owner *)a;
  bo = (Scheme_Scope_With_Owner *)b;
    
  if (SAME_OBJ(ao->owner_multi_scope, bo->owner_multi_scope)) {
    if (SCHEME_FALSEP(ao->phase))
      return 1;
    else if (SCHEME_FALSEP(bo->phase))
      return 1;
    else if (scheme_bin_lt(ao->phase, bo->phase))
      return 1;
    else
      return -1;
  } else {
    Scheme_Object *na, *nb;
    na = scheme_hash_get((Scheme_Hash_Table *)ao->owner_multi_scope, scheme_void);
    nb = scheme_hash_get((Scheme_Hash_Table *)bo->owner_multi_scope, scheme_void);
    STX_ASSERT(MULTI_SCOPE_META_HASHEDP(na));
    STX_ASSERT(MULTI_SCOPE_META_HASHEDP(nb));
    na = SCHEME_CDR(na);
    nb = SCHEME_CDR(nb);
    STX_ASSERT(SCHEME_REALP(na));
    STX_ASSERT(SCHEME_REALP(nb));
    if (scheme_bin_lt(na, nb))
      return 1;
    else if (scheme_bin_lt(nb, na))
      return -1;
    else
      return 0;
  }
}

static int compare_scopes(const void *_a, const void *_b)
{
  Scheme_Scope *a = *(Scheme_Scope **)_a;
  Scheme_Scope *b = *(Scheme_Scope **)_b;

  STX_ASSERT(SCHEME_SCOPEP(a));
  STX_ASSERT(SCHEME_SCOPEP(b));

  /* Scopes for multi-scopes that were generated late are
     ordered before everything else: */
  if (!(a->id >> SCHEME_STX_SCOPE_KIND_SHIFT)) {
    STX_ASSERT(SCHEME_SCOPE_HAS_OWNER(a));
    if (b->id >> SCHEME_STX_SCOPE_KIND_SHIFT)
      return 1;
    STX_ASSERT(SCHEME_SCOPE_HAS_OWNER(b));

    return compare_scopes_from_multi(a, b);
  } else if (!(b->id >> SCHEME_STX_SCOPE_KIND_SHIFT)) {
    STX_ASSERT(SCHEME_SCOPE_HAS_OWNER(b));
    return -1;
  }
  
  if (a->id > b->id)
    return -1;
  else if (a->id < b->id)
    return 1;
  else
    return 0;
}

static void sort_scope_array(Scheme_Object **a, intptr_t count)
{
  my_qsort(a, count, sizeof(Scheme_Object *), compare_scopes);
}

static Scheme_Object *scopes_to_sorted_list(Scheme_Scope_Set *scopes)
{
  Scheme_Object **a, *r, *key, *val;
  intptr_t i, j = 0;

  i = scope_set_count(scopes);
  a = MALLOC_N(Scheme_Object *, i);

  i = scope_set_next(scopes, -1);
  while (i != -1) {
    scope_set_index(scopes, i, &key, &val);
    a[j++] = key;
    i = scope_set_next(scopes, i);
  }

  sort_scope_array(a, j);

  r = scheme_null;
  for (i = j; i--; ) {
    r = scheme_make_pair(a[i], r);
  }

  return r;
}

static int compare_syms(const void *_a, const void *_b)
{
  Scheme_Object *a = *(Scheme_Object **)_a;
  Scheme_Object *b = *(Scheme_Object **)_b;
  intptr_t l = SCHEME_SYM_LEN(a), i;

  STX_ASSERT(SCHEME_SYMBOLP(a));
  STX_ASSERT(SCHEME_SYMBOLP(b));

  if (SCHEME_SYM_LEN(b) < l)
    l = SCHEME_SYM_LEN(b);
  
  for (i = 0; i < l; i++) {
    if (SCHEME_SYM_VAL(a)[i] != SCHEME_SYM_VAL(b)[i])
      return (SCHEME_SYM_VAL(a)[i] - SCHEME_SYM_VAL(b)[i]);
  }

  return SCHEME_SYM_LEN(a) - SCHEME_SYM_LEN(b);
}

static void sort_vector_symbols(Scheme_Object *vec)
{
  my_qsort(SCHEME_VEC_ELS(vec), SCHEME_VEC_SIZE(vec), sizeof(Scheme_Object *), compare_syms);
}

static void sort_symbol_array(Scheme_Object **a, intptr_t count)
{
  my_qsort(a, count, sizeof(Scheme_Object *), compare_syms);
}

static int compare_nums(const void *_a, const void *_b)
/* also allow #fs */
{
  Scheme_Object *a = *(Scheme_Object **)_a;
  Scheme_Object *b = *(Scheme_Object **)_b;

  if (SCHEME_FALSEP(a))
    return -1;
  else if (SCHEME_FALSEP(b))
    return 1;

  STX_ASSERT(SCHEME_REALP(a));
  STX_ASSERT(SCHEME_REALP(b));

  if (scheme_bin_lt(a, b))
    return -1;
  else if (scheme_bin_lt(b, a))
    return 1;
  else
    return 0;
}

static void sort_number_array(Scheme_Object **a, intptr_t count)
{
  my_qsort(a, count, sizeof(Scheme_Object *), compare_nums);
}

static Scheme_Object *drop_export_registries(Scheme_Object *shifts)
{
  Scheme_Object *l, *a, *vec, *p, *first = scheme_null, *last = NULL;
  int same_insp;

  if (SCHEME_VECTORP(shifts))
    shifts = SCHEME_VEC_ELS(shifts)[0];

  for (l = shifts; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    a = SCHEME_CAR(l);
    same_insp = ((SCHEME_VEC_SIZE(a) <= 2)
                 || SAME_OBJ(SCHEME_VEC_ELS(a)[2], SCHEME_VEC_ELS(a)[3])
                 || SCHEME_FALSEP(SCHEME_VEC_ELS(a)[3]));
    if (!SAME_OBJ(SCHEME_VEC_ELS(a)[0], SCHEME_VEC_ELS(a)[1])
        || !same_insp) {
      if (same_insp)
        vec = scheme_make_vector(2, NULL);
      else
        vec = scheme_make_vector(4, NULL);
      SCHEME_VEC_ELS(vec)[0] = SCHEME_VEC_ELS(a)[0];
      SCHEME_VEC_ELS(vec)[1] = SCHEME_VEC_ELS(a)[1];
      if (!same_insp) {
        SCHEME_VEC_ELS(vec)[2] = SCHEME_VEC_ELS(a)[2];
        SCHEME_VEC_ELS(vec)[3] = SCHEME_VEC_ELS(a)[3];
      }

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

static void init_identity_map(Scheme_Marshal_Tables *mt)
{
  Scheme_Hash_Table *id_map;
  id_map = scheme_make_hash_table(SCHEME_hash_ptr);
  mt->identity_map = id_map;
}

static int compare_phased_scopes(const void *_a, const void *_b)
{
  Scheme_Object *a = *(Scheme_Object **)_a;
  Scheme_Object *b = *(Scheme_Object **)_b;

  if (SCHEME_FALSEP(a))
    return -1;
  else if (SCHEME_FALSEP(b))
    return 1;
  else {
    STX_ASSERT(SCHEME_REALP(a));
    STX_ASSERT(SCHEME_REALP(b));
    if (scheme_bin_lt(a, b))
      return -1;
    else
      return 1;
  }
}

static Scheme_Object *multi_scope_to_vector(Scheme_Object *multi_scope, Scheme_Marshal_Tables *mt)
{
  Scheme_Object *vec;
  Scheme_Hash_Table *scopes = (Scheme_Hash_Table *)multi_scope;
  intptr_t i, j, count;

  if (!mt->identity_map)
    init_identity_map(mt);

  vec = scheme_hash_get(mt->identity_map, multi_scope);
  if (vec)
    return vec;

  /* only keep reachable scopes: */
  count = 0;
  for (i = scopes->size; i--; ) {
    if (scopes->vals[i]) {
      if (!MULTI_SCOPE_METAP(scopes->keys[i])) {
        if (scheme_hash_get(mt->reachable_scopes, scopes->vals[i]))
          count++;
      }
    }
  }
    
  vec = scheme_make_vector((2 * count) + 1, scheme_void);
  j = 0;
  for (i = scopes->size; i--; ) {
    if (scopes->vals[i]) {
      if (!MULTI_SCOPE_METAP(scopes->keys[i])) {
        if (scheme_hash_get(mt->reachable_scopes, scopes->vals[i])) {
          SCHEME_VEC_ELS(vec)[j++] = scopes->keys[i]; /* a phase */
          SCHEME_VEC_ELS(vec)[j++] = scopes->vals[i]; /* a scope */
        }
      } else {
        /* debug name */
        SCHEME_VEC_ELS(vec)[2 * count] = (MULTI_SCOPE_META_HASHEDP(scopes->vals[i])
                                          ? SCHEME_CAR(scopes->vals[i])
                                          : scopes->vals[i]);
      }
    }
  }

  my_qsort(SCHEME_VEC_ELS(vec), count, 2 * sizeof(Scheme_Object *), compare_phased_scopes);
  
  vec = scheme_make_marshal_shared(vec);

  scheme_hash_set(mt->identity_map, multi_scope, vec);

  return vec;
}

static Scheme_Object *marshal_multi_scopes(Scheme_Object *multi_scopes, Scheme_Marshal_Tables *mt, Scheme_Hash_Table *ht)
{
  Scheme_Object *l, *p, *first, *last;
  Scheme_Object *fb_first = scheme_null, *fb_last = NULL;

  while (1) {
    l = multi_scopes;
    if (SCHEME_FALLBACKP(l))
      l = SCHEME_FALLBACK_FIRST(l);

    first = scheme_null;
    last = NULL;
    
    while (!SCHEME_NULLP(l)) {
      p = scheme_make_pair(scheme_make_pair(multi_scope_to_vector(SCHEME_CAR(SCHEME_CAR(l)), mt),
                                            SCHEME_CDR(SCHEME_CAR(l))),
                           scheme_null);
      if (last)
        SCHEME_CDR(last) = p;
      else
        first = p;
      last = p;
      l = SCHEME_CDR(l);
    }

    first = intern_tails(first, ht);

    if (SCHEME_FALLBACKP(multi_scopes))
      first = make_fallback_pair(first, scheme_false);
    
    if (fb_last)
      SCHEME_FALLBACK_REST(fb_last) = first;
    else
      fb_first = first;
    fb_last = first;

    if (SCHEME_FALLBACKP(multi_scopes))
      multi_scopes = SCHEME_FALLBACK_REST(multi_scopes);
    else
      break;
  }

  if (SCHEME_FALLBACKP(fb_first))
    fb_first = intern_fallback_tails(fb_first, ht);

  return fb_first;
}

static Scheme_Object *wraps_to_datum(Scheme_Stx *stx, Scheme_Marshal_Tables *mt)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *shifts, *simples, *multi, *v, *vec;

  if (mt->pass < 0) {
    /* This is the pass to discover reachable scopes. */
    add_reachable_scopes(stx->scopes->simple_scopes, mt);
    add_reachable_multi_scopes(stx->scopes->multi_scopes, mt);
    return scheme_void;
  }

  ht = mt->intern_map;

  shifts = intern_tails(drop_export_registries(stx->shifts), ht);
  simples = intern_tails(scopes_to_sorted_list(stx->scopes->simple_scopes), ht);
  multi = marshal_multi_scopes(stx->scopes->multi_scopes, mt, ht);

  vec = scheme_make_vector(3, NULL);
  SCHEME_VEC_ELS(vec)[0] = shifts;
  SCHEME_VEC_ELS(vec)[1] = simples;
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

static Scheme_Object *marshal_bindings(Scheme_Object *l, Scheme_Marshal_Tables *mt)
/* l is a pair for one binding, or an mlist of bindings */
{
  Scheme_Object *r, *scopes, *v;

  r = scheme_null;

  while (!SCHEME_NULLP(l)) {
    if (SCHEME_PAIRP(l))
      scopes = (Scheme_Object *)SCHEME_BINDING_SCOPES(l);
    else {
      STX_ASSERT(SCHEME_MPAIRP(l));
      scopes = (Scheme_Object *)SCHEME_BINDING_SCOPES(SCHEME_CAR(l));
    }

    if (!any_unreachable_scope((Scheme_Scope_Set *)scopes, mt, 0)) {
      if (SCHEME_PAIRP(l))
        v = SCHEME_BINDING_VAL(l);
      else
        v = SCHEME_BINDING_VAL(SCHEME_CAR(l));
      if (SCHEME_MPAIRP(v)) {
        /* has a `free-id=?` equivalence; the marshaled form of a scope's content
           cannot contain a syntax object, so we keep just the syntax object's symbol
           and scopes */
        v = scheme_make_pair(SCHEME_CAR(v), marshal_free_id_info(SCHEME_CDR(v), mt));
        v = scheme_box(v); /* a box indicates `free-id=?` info */
      }
      v = intern_one(v, mt->intern_map);
      scopes = intern_tails(scopes_to_sorted_list((Scheme_Scope_Set *)scopes),
                           mt->intern_map);
      r = scheme_make_pair(intern_one(scheme_make_pair(scopes, v), mt->intern_map), r);
    }

    if (SCHEME_MPAIRP(l))
      l = SCHEME_CDR(l);
    else
      l = scheme_null;
  }

  if (!SCHEME_NULLP(r))
    r = intern_one(r, mt->intern_map);

  return r;
}

Scheme_Object *scheme_scope_marshal_content(Scheme_Object *m, Scheme_Marshal_Tables *mt)
{
  Scheme_Hash_Tree *ht;
  Scheme_Object *v, *l, *r, *l2, *tab, *scopes, *val, **sorted_keys;
  intptr_t i, j;

  if (!mt->identity_map)
    init_identity_map(mt);

  v = scheme_hash_get(mt->identity_map, m);
  if (v)
    return v;

  v = ((Scheme_Scope *)m)->bindings;
  if (v) {
    int count;

    if (SCHEME_VECTORP(v)) {
      ht = NULL;
      l2 = NULL;
      count = 1;
    } else {
      if (SCHEME_RPAIRP(v)) {
        ht = (Scheme_Hash_Tree *)SCHEME_CAR(v);
        l2 = SCHEME_CDR(v);
      } else {
        STX_ASSERT(SCHEME_HASHTRP(v));
        ht = (Scheme_Hash_Tree *)v;
        l2 = NULL;
      }
      count = ht->count;
    }

    /* convert to a vector, pruning unreachable and adjusting
       encoding of `free-identifier=?` equivalences */
    tab = scheme_make_vector(2 * count, NULL);
    j = 0;
    if (!ht) {
      STX_ASSERT(SCHEME_VECTORP(v));
      r = marshal_bindings(scheme_make_pair((Scheme_Object *)SCHEME_VEC_BINDING_SCOPES(v),
                                            SCHEME_VEC_BINDING_VAL(v)),
                           mt);
      if (SCHEME_NULLP(r)) {
        /* no reachable bindings */
      } else {
        SCHEME_VEC_ELS(tab)[j++] = SCHEME_VEC_BINDING_KEY(v);
        SCHEME_VEC_ELS(tab)[j++] = r;
      }
    } else {
      intptr_t count = ht->count;
      sorted_keys = scheme_extract_sorted_keys((Scheme_Object *)ht);
      for (i = 0; i < count; i++) {
        val = scheme_hash_tree_get(ht, sorted_keys[i]);
        r = marshal_bindings(val, mt);

        if (SCHEME_NULLP(r)) {
          /* no reachable bindings */
        } else {
          STX_ASSERT(j < (2 * count));
          SCHEME_VEC_ELS(tab)[j++] = sorted_keys[i];
          SCHEME_VEC_ELS(tab)[j++] = r;
        }
      }
    }

    if (j < SCHEME_VEC_SIZE(tab)) {
      /* shrink vector: */
      r = scheme_make_vector(j, NULL);
      memcpy(SCHEME_VEC_ELS(r), SCHEME_VEC_ELS(tab), j * sizeof(Scheme_Object *));
    } else
      r = tab;

    /* convert scopes+pes to scope + unmarshal request */
    for (l = l2; l; l = SCHEME_CDR(l)) {
      STX_ASSERT(SCHEME_RPAIRP(l));
      v = SCHEME_CDR(SCHEME_CAR(l));
      if (any_unreachable_scope((Scheme_Scope_Set *)SCHEME_CAR(SCHEME_CAR(l)), mt, 0)) {
        /* drop unreachable bindings */
        v = NULL;
      } else if (PES_BINDINGP(v)) {
        l2 = scheme_make_vector(4, NULL);
        SCHEME_VEC_ELS(l2)[0] = SCHEME_VEC_ELS(v)[0];
        SCHEME_VEC_ELS(l2)[1] = SCHEME_VEC_ELS(v)[2];
        SCHEME_VEC_ELS(l2)[3] = SCHEME_VEC_ELS(v)[4];
        v = unmarshal_excepts_to_vector(SCHEME_VEC_ELS(v)[3]);
        SCHEME_VEC_ELS(l2)[2] = v;
        v = l2;
      } else if (PES_UNMARSHAL_DESCP(v)) {
        if (SCHEME_TRUEP(SCHEME_VEC_ELS(v)[0])) {
          /* never unmarshaled, so keep it */
        } else {
          /* this shouldn't happen, because it should have been
             replaced on unmarshal, but discard it if we get here */
          v = NULL;
        }
      } else {
        STX_ASSERT(0);
      }
      if (v) {
        scopes = intern_tails(scopes_to_sorted_list((Scheme_Scope_Set *)SCHEME_CAR(SCHEME_CAR(l))),
                             mt->intern_map);
        r = scheme_make_pair(scheme_make_pair(scopes, v), r);
      }
    }

    v = scheme_make_pair(scheme_make_integer(SCHEME_SCOPE_KIND(m)), r);
  } else
    v = scheme_make_integer(SCHEME_SCOPE_KIND(m));

  scheme_hash_set(mt->identity_map, m, v);

  return v;
}

/*========================================================================*/
/*                           syntax->datum                                */
/*========================================================================*/

/* This code can convert a syntax object plus its wraps to something
   writeable. In that case, the result is a <converted>:

      <converted> = <simple converted pair> | ...

      <simple converted pair> = (MK (cons <int> (cons <converted> ... <converted>)) <wrap> <srcloc> <taint/arm>)
                              | (MK (cons <converted> ... null) <wrap> <srcloc> <taint/arm>)
                              | (MK (cons #t <s-exp>) <wrap> <srcloc> <taint/arm>)
                                 ; where <s-exp> has no boxes or vectors, and
                                 ;  <wrap>, <srcloc>, and <taint/arm> are shared in all <s-exp> elements
      <simple converted box> = (MK (box <converted>) <wrap> <srcloc> <taint/arm>)
      <simple converted vector> = (MK (vector <converted> ...) <wrap> <srcloc> <taint/arm>)
      <simple converted other> = (MK <s-exp> <wrap> <srcloc> <taint/arm>)
                                 ; where <s-exp> is not a pair, vector, or box

    where

      (MK <content> <wraps> #f 0)               = (cons <content> <wraps>)
      (MK <content> <wraps> <vector> 0)         = (vector <content> <wraps> <vector>)
      (MK <content> <wraps> #f <positive-int>)  = (vector <content> <wraps> <positive-int>)
      (MK <content> <wraps> <vector> <pos-int>) = (vector <content> <wraps> <vector> <pos-int>)

*/

#define COMMON_EXTRACT_DATUM  0
#define COMMON_EXTRACT_WRAPS  1
#define COMMON_EXTRACT_SRCLOC 2
#define COMMON_EXTRACT_TAINT  3

static Scheme_Object *extract_for_common_wrap(Scheme_Object *a, int get_part, int pair_ok)
{
  /* We only share wraps for things constucted with pairs and
     atomic (w.r.t. syntax) values. */
  Scheme_Object *v, *wraps, *srcloc, *taint;

  if (SCHEME_PAIRP(a)) {
    v = SCHEME_CAR(a);
    wraps = SCHEME_CDR(a);
    srcloc = scheme_false;
    taint = scheme_make_integer(0);
  } else if (SCHEME_VECTORP(a)) {
    v = SCHEME_VEC_ELS(a)[0];
    wraps = SCHEME_VEC_ELS(a)[1];
    srcloc = SCHEME_VEC_ELS(a)[2];
    if (SCHEME_INTP(srcloc)) { /* an integer is a taint or arm value */
      taint = srcloc;
      srcloc = scheme_false;
    } else if (SCHEME_VEC_SIZE(a) > 3)
      taint = SCHEME_VEC_ELS(a)[3];
    else
      taint = scheme_make_integer(0);
  } else
    return NULL;

  if (SCHEME_PAIRP(v)) {
    if (pair_ok && SAME_OBJ(SCHEME_CAR(v), scheme_true)) {
      /* A pair with shared wraps for its elements */
      if (get_part == COMMON_EXTRACT_WRAPS)
        return wraps;
      else if (get_part == COMMON_EXTRACT_SRCLOC)
        return srcloc;
      else if (get_part == COMMON_EXTRACT_TAINT)
        return taint;
      else
        return SCHEME_CDR(v);
    }
  } else if (!SCHEME_NULLP(v) && !SCHEME_BOXP(v) && !SCHEME_VECTORP(v) && !SCHEME_HASHTRP(v) && !prefab_p(v)) {
    /* It's atomic. */
    if (get_part == COMMON_EXTRACT_WRAPS)
      return wraps;
    else if (get_part == COMMON_EXTRACT_SRCLOC)
      return srcloc;
    else if (get_part == COMMON_EXTRACT_TAINT)
      return taint;
    else
      return v;
  }

  return NULL;
}

static void lift_common_wraps(Scheme_Object *l, int cnt, int tail)
{
  Scheme_Object *a;

  while (cnt--) {
    a = SCHEME_CAR(l);
    a = extract_for_common_wrap(a, COMMON_EXTRACT_DATUM, 1);
    SCHEME_CAR(l) = a;
    if (cnt)
      l = SCHEME_CDR(l);
  }
  if (tail) {
    a = SCHEME_CDR(l);
    a = extract_for_common_wrap(a, COMMON_EXTRACT_DATUM, 0);
    SCHEME_CDR(l) = a;
  }
}

static Scheme_Object *srcloc_path_to_string(Scheme_Object *p)
{
  Scheme_Object *base, *name, *dir_name;
  int isdir;
  
  name = scheme_split_path(SCHEME_PATH_VAL(p), SCHEME_PATH_LEN(p), &base, &isdir, SCHEME_PLATFORM_PATH_KIND);
  if (SCHEME_PATHP(name) && SCHEME_PATHP(base)) {
    dir_name = scheme_split_path(SCHEME_PATH_VAL(base), SCHEME_PATH_LEN(base), &base, &isdir, SCHEME_PLATFORM_PATH_KIND);
    if (SCHEME_PATHP(dir_name))
      name = scheme_append_strings(scheme_path_to_char_string(dir_name),
                                   scheme_append_strings(scheme_make_utf8_string("/"),
                                                         scheme_path_to_char_string(name)));
    else
      name = scheme_path_to_char_string(name);
    return scheme_append_strings(scheme_make_utf8_string(".../"), name);
  } else if (SCHEME_PATHP(name))
    return scheme_path_to_char_string(name);
  else
    return scheme_false;
}

static Scheme_Object *convert_srcloc(Scheme_Stx_Srcloc *srcloc, Scheme_Hash_Tree *props, Scheme_Marshal_Tables *mt)
{
  Scheme_Object *vec, *paren, *src, *dir;

  if (props) {
    paren = scheme_hash_tree_get(props, scheme_paren_shape_symbol);
    if (paren && !SCHEME_CHARP(paren))
      paren = NULL;
  } else
    paren = NULL;

  if ((!srcloc || (SCHEME_FALSEP(srcloc->src)
                   && (srcloc->line < 0)
                   && (srcloc->col < 0)
                   && (srcloc->pos < 0)))
      && !paren)
    return scheme_false;

  if (!srcloc)
    srcloc = empty_srcloc;

  src = srcloc->src;
  if (SCHEME_PATHP(src)) {
    /* To make paths portable and to avoid full paths, check whether the
       path can be made relative (in which case it is turned into a list
       of byte strings). If not, convert to a string using only the
       last couple of path elements. */
    dir = scheme_get_param(scheme_current_config(),
                           MZCONFIG_WRITE_DIRECTORY);
    if (SCHEME_TRUEP(dir))
      src = scheme_extract_relative_to(src, dir, mt->path_cache);
    if (SCHEME_PATHP(src)) {
      src = scheme_hash_get(mt->path_cache, scheme_box(srcloc->src));
      if (!src) {
        src = srcloc_path_to_string(srcloc->src);
        scheme_hash_set(mt->path_cache, scheme_box(srcloc->src), src);
      }
    } else {
      /* use the path directly and let the printer make it relative */
      src = srcloc->src;
    }
  }

  vec = scheme_make_vector((paren ? 6 : 5), NULL);
  SCHEME_VEC_ELS(vec)[0] = src;
  SCHEME_VEC_ELS(vec)[1] = scheme_make_integer(srcloc->line);
  SCHEME_VEC_ELS(vec)[2] = scheme_make_integer(srcloc->col);
  SCHEME_VEC_ELS(vec)[3] = scheme_make_integer(srcloc->pos);
  SCHEME_VEC_ELS(vec)[4] = scheme_make_integer(srcloc->span);
  if (paren)
    SCHEME_VEC_ELS(vec)[5] = paren;

  return intern_one(vec, mt->intern_map);
}

static void unconvert_srcloc(Scheme_Object *srcloc_vec, Scheme_Stx *dest)
{
  Scheme_Stx_Srcloc *srcloc;
  
  if (!SCHEME_VECTORP(srcloc_vec)
      || ((SCHEME_VEC_SIZE(srcloc_vec) != 5)
          && (SCHEME_VEC_SIZE(srcloc_vec) != 6)))
    return;

  srcloc = MALLOC_ONE_RT(Scheme_Stx_Srcloc);
#ifdef MZTAG_REQUIRED
  srcloc->type = scheme_rt_srcloc;
#endif
  srcloc->src = SCHEME_VEC_ELS(srcloc_vec)[0];
  srcloc->line = SCHEME_INT_VAL(SCHEME_VEC_ELS(srcloc_vec)[1]);
  srcloc->col = SCHEME_INT_VAL(SCHEME_VEC_ELS(srcloc_vec)[2]);
  srcloc->pos = SCHEME_INT_VAL(SCHEME_VEC_ELS(srcloc_vec)[3]);
  srcloc->span = SCHEME_INT_VAL(SCHEME_VEC_ELS(srcloc_vec)[4]);
  
  dest->srcloc = srcloc;

  if ((SCHEME_VEC_SIZE(srcloc_vec) > 5)
      && SCHEME_CHARP((SCHEME_VEC_ELS(srcloc_vec)[5]))) {
    if (SCHEME_CHAR_VAL(SCHEME_VEC_ELS(srcloc_vec)[5]) == '[')
      dest->props = square_stx_props;
    else if (SCHEME_CHAR_VAL(SCHEME_VEC_ELS(srcloc_vec)[5]) == '{')
      dest->props = curly_stx_props;
  }
}

#ifdef DO_STACK_CHECK
static Scheme_Object *syntax_to_datum_inner(Scheme_Object *o, 
					    int with_scopes,
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
					    int with_scopes, /* non-zero => marshal; negative => implicitly tainted */
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
      p->ku.k.i1 = with_scopes;
      p->ku.k.p3 = (void *)mt;
      return scheme_handle_stack_overflow(syntax_to_datum_k);
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  if (with_scopes) {
    /* Propagate wraps: */
    scheme_stx_content((Scheme_Object *)stx);
    if (with_scopes > 0) {
      if (is_tainted((Scheme_Object *)stx)) {
        add_taint = 1;
        with_scopes = -with_scopes;
      } else if (is_armed((Scheme_Object *)stx)) {
        add_taint = 2;
      }
    }
  }

  v = stx->val;
  
  if (SCHEME_PAIRP(v)) {
    Scheme_Object *first = NULL, *last = NULL, *p;
    Scheme_Object *common_wraps = NULL, *common_srcloc = NULL, *common_taint = NULL;
    Scheme_Object *a, *sa, *ta;
    int cnt = 0;
    
    while (SCHEME_PAIRP(v)) {
      cnt++;

      a = syntax_to_datum_inner(SCHEME_CAR(v), with_scopes, mt);

      p = CONS(a, scheme_null);
      
      if (last)
	SCHEME_CDR(last) = p;
      else
	first = p;
      last = p;
      v = SCHEME_CDR(v);

      if (with_scopes) {
        sa = extract_for_common_wrap(a, COMMON_EXTRACT_SRCLOC, 1);
        ta = extract_for_common_wrap(a, COMMON_EXTRACT_TAINT, 1);
        a = extract_for_common_wrap(a, COMMON_EXTRACT_WRAPS, 1);
        if (!common_wraps) {
          if (a) {
            common_wraps = a;
            common_srcloc = sa;
            common_taint = ta;
          } else
            common_wraps = scheme_false;
        } else if (!a
                   || !SAME_OBJ(common_wraps, a)
                   || !SAME_OBJ(common_srcloc, sa)
                   || !SAME_OBJ(common_taint, ta))
          common_wraps = scheme_false;
      }
    }
    if (!SCHEME_NULLP(v)) {
      v = syntax_to_datum_inner(v, with_scopes, mt);
      SCHEME_CDR(last) = v;

      if (with_scopes) {
        sa = extract_for_common_wrap(v, COMMON_EXTRACT_SRCLOC, 0);
        ta = extract_for_common_wrap(v, COMMON_EXTRACT_TAINT, 0);
        v = extract_for_common_wrap(v, COMMON_EXTRACT_WRAPS, 0);
        if (v
            && SAME_OBJ(common_wraps, v)
            && SAME_OBJ(common_srcloc, sa)
            && SAME_OBJ(common_taint, ta)) {
          converted_wraps = wraps_to_datum(stx, mt);
          sa = convert_srcloc(stx->srcloc, stx->props, mt);
          if (SAME_OBJ(common_wraps, converted_wraps)
              && SAME_OBJ(common_srcloc, sa)
              && SAME_OBJ(common_taint, scheme_make_integer(add_taint)))
            lift_common_wraps(first, cnt, 1);
          else
            common_wraps = scheme_false;
        } else
          common_wraps = scheme_false;
      }

      if (with_scopes && SCHEME_FALSEP(common_wraps)) {
	/* v is likely a pair, and v's car might be a pair,
	   which means that the datum->syntax part
	   won't be able to detect that v is a "non-pair"
	   terminal. Therefore, we communicate the
	   length before the terminal to datum->syntax: */
	first = scheme_make_pair(scheme_make_integer(cnt), first);
      }
    } else if (with_scopes && SCHEME_TRUEP(common_wraps)) {
      converted_wraps = wraps_to_datum(stx, mt);
      sa = convert_srcloc(stx->srcloc, stx->props, mt);
      if (SAME_OBJ(common_wraps, converted_wraps)
          && SAME_OBJ(common_srcloc, sa)
          && SAME_OBJ(common_taint, scheme_make_integer(add_taint)))
        lift_common_wraps(first, cnt, 0);
      else
        common_wraps = scheme_false;
    }

    if (with_scopes && SCHEME_TRUEP(common_wraps)) {
      first = scheme_make_pair(scheme_true, first);
    }

    result = first;
  } else if (SCHEME_BOXP(v)) {
    v = syntax_to_datum_inner(SCHEME_BOX_VAL(v), with_scopes, mt);
    result = scheme_box(v);
    SCHEME_SET_IMMUTABLE(result);
  } else if (SCHEME_VECTORP(v)) {
    int size = SCHEME_VEC_SIZE(v), i;
    Scheme_Object *r, *a;
    
    r = scheme_make_vector(size, NULL);
    
    for (i = 0; i < size; i++) {
      a = syntax_to_datum_inner(SCHEME_VEC_ELS(v)[i], with_scopes, mt);
      SCHEME_VEC_ELS(r)[i] = a;
    }

    result = r;
    SCHEME_SET_IMMUTABLE(result);
  } else if (SCHEME_HASHTRP(v)) {
    Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)v, *ht2;
    Scheme_Object *key, *val;
    mzlonglong i;
    
    ht2 = scheme_make_hash_tree_of_type(SCHEME_HASHTR_TYPE(ht));
    
    i = scheme_hash_tree_next(ht, -1);
    while (i != -1) {
      scheme_hash_tree_index(ht, i, &key, &val);
      val = syntax_to_datum_inner(val, with_scopes, mt);
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
      a = syntax_to_datum_inner(s->slots[i], with_scopes, mt);
      s->slots[i] = a;
    }

    result = (Scheme_Object *)s;
  } else
    result = v;

  if (with_scopes) {
    if (!converted_wraps)
      converted_wraps = wraps_to_datum(stx, mt);
    v = convert_srcloc(stx->srcloc, stx->props, mt);
    if (SCHEME_TRUEP(v)) {
      result = scheme_make_vector((add_taint ? 4 : 3), result);
      SCHEME_VEC_ELS(result)[1] = converted_wraps;
      SCHEME_VEC_ELS(result)[2] = v;
      if (add_taint)
        SCHEME_VEC_ELS(result)[3] = scheme_make_integer(add_taint); /* 1 => tainted, 2 => armed */
    } else if (add_taint) {
      result = scheme_make_vector(3, result);
      SCHEME_VEC_ELS(result)[1] = converted_wraps;
      SCHEME_VEC_ELS(result)[2] = scheme_make_integer(add_taint); /* 1 => tainted, 2 => armed */
    } else
      result = CONS(result, converted_wraps);
  }

  return result;
}

Scheme_Object *scheme_syntax_to_datum(Scheme_Object *stx, int with_scopes,
				      Scheme_Marshal_Tables *mt)
{
  Scheme_Object *v;

  if (mt && (mt->pass >= 0))
    scheme_marshal_push_refs(mt);

  v = syntax_to_datum_inner(stx, with_scopes, mt);

  if (mt && (mt->pass >= 0)) {
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

#define return_NULL return NULL

Scheme_Object *scheme_hash_get_either(Scheme_Hash_Table *ht, Scheme_Hash_Table *ht2,
                                      Scheme_Object *key)
{
  Scheme_Object *val;
  val = scheme_hash_get(ht, key);
  if (val)
    return val;
  else if (ht2)
    return scheme_hash_get(ht2, key);
  else
    return NULL;
}

static void ensure_current_rns(Scheme_Unmarshal_Tables *ut)
{
  Scheme_Hash_Table *rht;
  if (!ut->current_rns) {
    rht = scheme_make_hash_table(SCHEME_hash_ptr);
    ut->current_rns = rht;
  }
}

static void ensure_current_multi_scope_pairs(Scheme_Unmarshal_Tables *ut)
{
  Scheme_Hash_Table *rht;
  if (!ut->current_multi_scope_pairs) {
    rht = scheme_make_hash_table(SCHEME_hash_ptr);
    ut->current_multi_scope_pairs = rht;
  }
}

Scheme_Scope_Set *list_to_scope_set(Scheme_Object *l, Scheme_Unmarshal_Tables *ut)
{
  Scheme_Scope_Set *scopes = NULL;
  Scheme_Object *r = scheme_null, *scope;

  while (!SCHEME_NULLP(l)) {
    if (!SCHEME_PAIRP(l)) return_NULL;
    scopes = (Scheme_Scope_Set *)scheme_hash_get_either(ut->rns, ut->current_rns, l);
    if (scopes)
      break;
    r = scheme_make_pair(l, r);
    l = SCHEME_CDR(l);
  }

  if (!scopes) scopes = empty_scope_set;

  while (!SCHEME_NULLP(r)) {
    l = SCHEME_CAR(r);

    scope = scope_unmarshal_content(SCHEME_CAR(l), ut);
    if (!scope) return_NULL;

    scopes = scope_set_set(scopes, scope, scheme_true);
    ensure_current_rns(ut);
    scheme_hash_set(ut->current_rns, l, (Scheme_Object *)scopes);
    
    r = SCHEME_CDR(r);
  }

  return scopes;
}

static Scheme_Hash_Table *vector_to_multi_scope(Scheme_Object *mht, Scheme_Unmarshal_Tables *ut)
{
  /* Convert multi-scope vector to hash table */
  intptr_t i, len;
  Scheme_Hash_Table *multi_scope;
  Scheme_Object *scope;

  if (!SCHEME_VECTORP(mht)) return_NULL;

  multi_scope = (Scheme_Hash_Table *)scheme_hash_get_either(ut->rns, ut->current_rns, mht);
  if (multi_scope) return multi_scope;

  multi_scope = scheme_make_hash_table(SCHEME_hash_ptr);

  len = SCHEME_VEC_SIZE(mht);
  if (!(len & 1)) return_NULL;

  STX_ASSERT(ut->bytecode_hash);
  
  multi_scope = (Scheme_Hash_Table *)new_multi_scope(SCHEME_VEC_ELS(mht)[len-1]);
  scheme_hash_set(multi_scope,
                  scheme_void,
                  /* record bytecode hash for making fresh scopes for other phases: */
                  scheme_make_mutable_pair(scheme_hash_get(multi_scope, scheme_void),
                                           scheme_make_integer_value_from_long_long(ut->bytecode_hash
                                                                                    >> SCHEME_STX_SCOPE_KIND_SHIFT)));
  len -= 1;

  /* A multi-scope can refer back to itself via free-id=? info: */
  ensure_current_rns(ut);
  scheme_hash_set(ut->current_rns, mht, (Scheme_Object *)multi_scope);

  for (i = 0; i < len; i += 2) {
    if (!SCHEME_PHASEP(SCHEME_VEC_ELS(mht)[i]))
      return_NULL;
    scope = SCHEME_VEC_ELS(mht)[i+1];
    scope = scope_unmarshal_content(scope, ut);
    if (!scope) return_NULL;
    if (!SCHEME_SCOPE_HAS_OWNER((Scheme_Scope *)scope))
      return_NULL;
    if (((Scheme_Scope_With_Owner *)scope)->owner_multi_scope)
      return_NULL;
    scheme_hash_set(multi_scope, SCHEME_VEC_ELS(mht)[i], scope);
    ((Scheme_Scope_With_Owner *)scope)->owner_multi_scope = (Scheme_Object *)multi_scope;
    ((Scheme_Scope_With_Owner *)scope)->phase = SCHEME_VEC_ELS(mht)[i];
  }
      
  return multi_scope;
}

Scheme_Object *unmarshal_multi_scopes(Scheme_Object *multi_scopes,
                                     Scheme_Unmarshal_Tables *ut)
{
  Scheme_Hash_Table *multi_scope;
  Scheme_Object *l, *mm_l, *first = NULL, *last = NULL;
  Scheme_Object *l_first, *l_last, *p;

  mm_l = multi_scopes;

  while (1) {
    l = mm_l;
    if (SCHEME_FALLBACKP(l))
      l = SCHEME_FALLBACK_FIRST(l);

    l_first = scheme_null;
    l_last = NULL;
    for (; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      int stop;

      if (!SCHEME_PAIRP(l)) return_NULL;
      if (!SCHEME_PAIRP(SCHEME_CAR(l))) return_NULL;

      p = scheme_hash_get_either(ut->multi_scope_pairs, ut->current_multi_scope_pairs, l);
      if (!p) {
        p = scheme_hash_get_either(ut->multi_scope_pairs, ut->current_multi_scope_pairs, SCHEME_CAR(l));
        if (p) {
          p = scheme_make_pair(p, scheme_null);
        } else {
          if (SCHEME_VECTORP(SCHEME_CAR(SCHEME_CAR(l)))) {
            multi_scope = vector_to_multi_scope(SCHEME_CAR(SCHEME_CAR(l)), ut);
            if (!multi_scope) return_NULL;
            if (!SCHEME_PHASE_SHIFTP(SCHEME_CDR(SCHEME_CAR(l)))) return_NULL;
            p = scheme_make_pair((Scheme_Object *)multi_scope,
                                 SCHEME_CDR(SCHEME_CAR(l)));
            ensure_current_multi_scope_pairs(ut);
            scheme_hash_set(ut->current_multi_scope_pairs, SCHEME_CAR(l), p);
          } else
            return_NULL;
        }
        ensure_current_multi_scope_pairs(ut);
        scheme_hash_set(ut->current_multi_scope_pairs, SCHEME_CAR(l), p);
        p = scheme_make_pair(p, scheme_null);
        stop = 0;
      } else
        stop = 1;

      if (l_last)
        SCHEME_CDR(l_last) = p;
      else
        l_first = p;
      l_last = p;

      if (stop)
        break;
      else {
        ensure_current_multi_scope_pairs(ut);
        scheme_hash_set(ut->current_multi_scope_pairs, l, p);
      }
    }

    if (SCHEME_FALLBACKP(mm_l)) {
      p = make_fallback_pair(l_first, scheme_null);
      if (last)
        SCHEME_FALLBACK_REST(last) = p;
      else
        first = p;
      last = p;
      mm_l = SCHEME_FALLBACK_REST(mm_l);
    } else {
      if (last)
        SCHEME_FALLBACK_REST(last) = l_first;
      else
        first = l_first;
      break;
    }
  }

  return first;
}

static Scheme_Object *datum_to_wraps(Scheme_Object *w,
                                     Scheme_Unmarshal_Tables *ut)
{
  Scheme_Scope_Table *st;
  Scheme_Scope_Set *scopes;
  Scheme_Object *l;

  l = scheme_hash_get_either(ut->rns, ut->current_rns, w);
  if (l) {
    if (!SCHEME_PAIRP(l)
        || !SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(l)), scheme_scope_table_type))
      return NULL;
    return l;
  }

  if (!SCHEME_VECTORP(w)
      || ((SCHEME_VEC_SIZE(w) != 3)
          && (SCHEME_VEC_SIZE(w) != 4)))
    return_NULL;

  st = MALLOC_ONE_TAGGED(Scheme_Scope_Table);
  st->so.type = scheme_scope_table_type;

  scopes = list_to_scope_set(SCHEME_VEC_ELS(w)[1], ut);
  if (!scopes) return NULL;
  st->simple_scopes = scopes;

  l = unmarshal_multi_scopes(SCHEME_VEC_ELS(w)[2], ut);
  if (!l) return NULL;
  st->multi_scopes = l;

  l = scheme_make_pair((Scheme_Object *)st, SCHEME_VEC_ELS(w)[0]);
  ensure_current_rns(ut);
  scheme_hash_set(ut->current_rns, w, l);

  return l;
}

static Scheme_Object *validate_binding(Scheme_Object *p)
{
  if (SCHEME_SYMBOLP(p)) {
    /* Ok: local binding */
  } else {
    if (SCHEME_PAIRP(p) && SCHEME_SYMBOLP(SCHEME_CAR(p))) {
      /* Inpsector descriptor ok */
      p = SCHEME_CDR(p);
    }
    
    if (SAME_TYPE(SCHEME_TYPE(p), scheme_module_index_type)) {
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
  }

  return scheme_true;
}

static Scheme_Object *unmarshal_free_id_info(Scheme_Object *p, Scheme_Unmarshal_Tables *ut)
{
  Scheme_Object *o, *phase;

  if (!SCHEME_PAIRP(p)) return_NULL;
  phase = SCHEME_CDR(p);
  p = SCHEME_CAR(p);
  if (!SCHEME_PAIRP(p)) return_NULL;
  o = scheme_make_stx(SCHEME_CAR(p), NULL, NULL);
  p = datum_to_wraps(SCHEME_CDR(p), ut);
  if (!p) return_NULL;

  ((Scheme_Stx *)o)->scopes = (Scheme_Scope_Table *)SCHEME_CAR(p);
  STX_ASSERT(SAME_TYPE(SCHEME_TYPE(((Scheme_Stx *)o)->scopes), scheme_scope_table_type));
  ((Scheme_Stx *)o)->shifts = SCHEME_CDR(p);

  return scheme_make_pair(o, phase);
}

Scheme_Object *scope_unmarshal_content(Scheme_Object *box, Scheme_Unmarshal_Tables *ut)
{
  Scheme_Object *l = NULL, *l2, *r, *b, *m, *c, *free_id;
  Scheme_Hash_Tree *ht;
  Scheme_Scope_Set *scopes;
  intptr_t i, len, relative_id;

  if (SAME_OBJ(box, root_scope))
    return root_scope;

  r = scheme_hash_get_either(ut->rns, ut->current_rns, box);
  if (r)
    return r;

  if (!SCHEME_BOXP(box)) return_NULL;
  c = SCHEME_BOX_VAL(box);

  if (!SCHEME_PAIRP(c)) return_NULL;

  relative_id = SCHEME_INT_VAL(SCHEME_CAR(c));
  c = SCHEME_CDR(c);

  if (SCHEME_INTP(c)) {
    m = scheme_new_scope(SCHEME_INT_VAL(c));
    c = NULL;
  } else if (SCHEME_PAIRP(c)) {
    m = scheme_new_scope(SCHEME_INT_VAL(SCHEME_CAR(c)));
    c = SCHEME_CDR(c);
  } else
    m = scheme_new_scope(SCHEME_STX_MACRO_SCOPE);

  ensure_current_rns(ut);
  scheme_hash_set(ut->current_rns, box, m);
  /* Since we've created the scope before unmarshaling its content,
     cycles among scopes are ok. */

  /* Reset the scope's id to a hash from the bytecode plus a relative
     offset. The only use of a scope's id is for debugging and
     ordering, and using the bytecode's hash as part of the number is
     intended to make ordering deterministic even across modules,
     independent of the order that modules are loaded or delay-loaded.
     Hashes are not gauarnteed to be distinct or far enough apart, but
     they're likely to be. */
  STX_ASSERT(ut->bytecode_hash);
  ((Scheme_Scope *)m)->id = ((SCHEME_STX_SCOPE_KIND_MASK & ((Scheme_Scope*)m)->id)
                             | ((umzlonglong)((relative_id << SCHEME_STX_SCOPE_KIND_SHIFT)
                                              + ut->bytecode_hash)
                                & (~(umzlonglong)SCHEME_STX_SCOPE_KIND_MASK)));

  if (!c) return m;

  while (SCHEME_PAIRP(c)) {
    if (!SCHEME_PAIRP(SCHEME_CAR(c))) return_NULL;
    scopes = list_to_scope_set(SCHEME_CAR(SCHEME_CAR(c)), ut);
    l = scheme_make_raw_pair(scheme_make_pair((Scheme_Object *)scopes,
                                              SCHEME_CDR(SCHEME_CAR(c))),
                             l);
    c = SCHEME_CDR(c);
  }

  if (!SCHEME_VECTORP(c)) return_NULL;
  
  len = SCHEME_VEC_SIZE(c);
  if (len & 1) return_NULL;

  /* If the vector length is 2, and if the only key has a single
     binding, then we could generate the compact vector form of
     bindings. For now, we just build the hash table. */

  ht = empty_hash_tree;
  for (i = 0; i < len; i += 2) {
    l2 = SCHEME_VEC_ELS(c)[i+1];
    r = scheme_null;
    while (SCHEME_PAIRP(l2)) {
      if (!SCHEME_PAIRP(SCHEME_CAR(l2))) return_NULL;
      scopes = list_to_scope_set(SCHEME_CAR(SCHEME_CAR(l2)), ut);
      if (!scopes) return_NULL;

      b = SCHEME_CDR(SCHEME_CAR(l2));
      if (SCHEME_BOXP(b)) {
        /* has `free-id=?` info */
        b = SCHEME_BOX_VAL(b);
        if (!SCHEME_PAIRP(b)) return_NULL;
        free_id = unmarshal_free_id_info(SCHEME_CDR(b), ut);
        if (!free_id) return_NULL;
        b = SCHEME_CAR(b);
      } else
        free_id = NULL;
      if (!validate_binding(b)) return_NULL;

      if (free_id)
        b = scheme_make_mutable_pair(b, free_id);

      b = scheme_make_pair((Scheme_Object *)scopes, b);

      if (SCHEME_NULLP(r) && SCHEME_NULLP(SCHEME_CDR(l2))) {
        /* leave r as a single binding */
        r = b;
      } else
        r = scheme_make_mutable_pair(b, r);

      l2 = SCHEME_CDR(l2);
    }

    ht = scheme_hash_tree_set(ht, SCHEME_VEC_ELS(c)[i], r);
  }

  if (!l)
    l = (Scheme_Object *)ht;
  else
    l = scheme_make_raw_pair((Scheme_Object *)ht, l);

  ((Scheme_Scope *)m)->bindings = l;

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
					    Scheme_Stx *stx_wraps, /* or rename table, or vectored wrap+srcloc+taint */
					    Scheme_Hash_Table *ht,
                                            int tainted)
{
  Scheme_Object *result, *wraps, *hashed, *srcloc_vec;
  int do_not_unpack_wraps = 0, taintval = 0;

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

  srcloc_vec = scheme_false;
  
  if (ut && !SCHEME_VECTORP(stx_wraps)) {
    if (SCHEME_VECTORP(o)) {
      if (SCHEME_VEC_SIZE(o) == 4) {
        srcloc_vec = SCHEME_VEC_ELS(o)[2];
        taintval = SCHEME_INT_VAL(SCHEME_VEC_ELS(o)[3]);
      } else if (SCHEME_VEC_SIZE(o) == 3) {
        if (SCHEME_INTP(SCHEME_VEC_ELS(o)[2]))
          taintval = SCHEME_INT_VAL(SCHEME_VEC_ELS(o)[2]);
        else {
          srcloc_vec = SCHEME_VEC_ELS(o)[2];
          taintval = 0;
        }
      } else
        return_NULL;
      wraps = SCHEME_VEC_ELS(o)[1];
      o = SCHEME_VEC_ELS(o)[0];
    } else {
      if (!SCHEME_PAIRP(o)) 
        return_NULL;
      wraps = SCHEME_CDR(o);
      o = SCHEME_CAR(o);
    }
  } else if (SCHEME_VECTORP(stx_wraps)) {
    /* Shared wraps, to be used directly everywhere: */
    wraps = SCHEME_VEC_ELS(stx_wraps)[0];
    srcloc_vec = SCHEME_VEC_ELS(stx_wraps)[1];
    taintval = SCHEME_INT_VAL(SCHEME_VEC_ELS(stx_wraps)[2]);
    do_not_unpack_wraps = 1;
  } else
    wraps = NULL;

  if (taintval == 1)
    tainted = 1;

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

      if (wraps && !SCHEME_VECTORP(stx_wraps) && SAME_OBJ(SCHEME_CAR(o), scheme_true)) {
        /* Resolve wraps now, and then share it with
           all nested objects (as indicated by a box
           for stx_wraps). */
        wraps = datum_to_wraps(wraps, ut);
        if (!wraps) return_NULL;
        do_not_unpack_wraps = 1;
        sub_stx_wraps = (Scheme_Stx *)scheme_make_vector(3, wraps);
        SCHEME_VEC_ELS((Scheme_Object *)sub_stx_wraps)[1] = srcloc_vec;
        SCHEME_VEC_ELS((Scheme_Object *)sub_stx_wraps)[2] = scheme_make_integer(taintval);
        o = SCHEME_CDR(o);
      } else if (wraps && !SCHEME_VECTORP(stx_wraps) && SCHEME_INTP(SCHEME_CAR(o))) {
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
    
    ht2 = scheme_make_hash_tree_of_type(SCHEME_HASHTR_TYPE(ht1));
    
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

  if (tainted) {
    int mutate = MUTATE_STX_OBJ;
    (void)add_taint_to_stx(result, &mutate);
  } else if (taintval == 2) {
    /* Arm with #f as the inspector; #f is replaced by a
       specific inspector when the encloding code is instanted */
    Scheme_Object *l; 
    l = taint_intern(scheme_make_pair(scheme_false, scheme_null));
    l = taint_intern(scheme_make_pair(scheme_false, l));
    ((Scheme_Stx *)result)->taints = l;
  }

  if (SCHEME_TRUEP(srcloc_vec))
    unconvert_srcloc(srcloc_vec, (Scheme_Stx *)result);

  if (wraps) {
    if (!do_not_unpack_wraps) {
      wraps = datum_to_wraps(wraps, ut);
      if (!wraps)
        return_NULL;
    }

    if (!SCHEME_PAIRP(wraps)) return_NULL;
    ((Scheme_Stx *)result)->scopes = (Scheme_Scope_Table *)SCHEME_CAR(wraps);
    STX_ASSERT(SAME_TYPE(SCHEME_TYPE(((Scheme_Stx *)result)->scopes), scheme_scope_table_type));
    ((Scheme_Stx *)result)->shifts = SCHEME_CDR(wraps);
  } else if (SCHEME_FALSEP((Scheme_Object *)stx_wraps)) {
    /* wraps already nulled */
  } else {
    /* Note: no propagation will be needed for SUBSTX */
    ((Scheme_Stx *)result)->scopes = stx_wraps->scopes;
    STX_ASSERT(SAME_TYPE(SCHEME_TYPE(((Scheme_Stx *)result)->scopes), scheme_scope_table_type));
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
     /* If stx_wraps is a hash table, then `o' includes scopes.
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
  Scheme_Object *src = scheme_false;
  Scheme_Hash_Tree *properties = NULL;
  
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
    int mutate = MUTATE_STX_OBJ;
    add_taint_to_stx(src, &mutate);
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
    if (!scheme_hash_tree_get(stx->props, source_symbol))
      return scheme_false;
  } else
    return scheme_false;

  /* Look for any non-original scope: */
  i = scope_set_next(stx->scopes->simple_scopes, -1);
  while (i != -1) {
    scope_set_index(stx->scopes->simple_scopes, i, &key, &val);

    if (SCHEME_SCOPE_KIND(key) == SCHEME_STX_MACRO_SCOPE)
      return scheme_false;
    
    i = scope_set_next(stx->scopes->simple_scopes, i);
  }

  return scheme_true;
}

Scheme_Object *scheme_stx_property(Scheme_Object *_stx,
				   Scheme_Object *key,
				   Scheme_Object *val)
{
  Scheme_Stx *stx;
  Scheme_Hash_Tree *props;

  stx = (Scheme_Stx *)_stx;

  props = stx->props;
  if (!props)
    props = empty_hash_tree;

  if (val) {
    props = scheme_hash_tree_set(props, key, val);
    stx = (Scheme_Stx *)clone_stx((Scheme_Object *)stx, NULL);
    stx->props = props;
    return (Scheme_Object *)stx;
  } else {
    val = scheme_hash_tree_get(props, key);
    if (!val)
      return scheme_false;
    else
      return val;
  }
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
    mzlonglong i;
    Scheme_Object *key, *l = scheme_null;
    
    i = scheme_hash_tree_next(stx->props, -1);
    while (i != -1) {
      scheme_hash_tree_index(stx->props, i, &key, NULL);
      if (SCHEME_SYMBOLP(key) && !SCHEME_SYM_WEIRDP(key))
        l = scheme_make_pair(key, l);
      i = scheme_hash_tree_next(stx->props, i);
    }

    return l;
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
    to = clone_stx(to, NULL);
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

  r = scheme_stx_adjust_scopes(r, (Scheme_Scope_Set *)delta, phase, mode);

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

static Scheme_Object *syntax_debug_info(int argc, Scheme_Object **argv)
{
  Scheme_Object *phase;
  int all_bindings;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-debug-info", "syntax?", 0, argc, argv);

  phase = extract_phase("syntax-debug-info", 1, argc, argv,
                        scheme_make_integer(0), 0);

  all_bindings = ((argc > 2) && SCHEME_TRUEP(argv[2]));

  return stx_debug_info((Scheme_Stx *)argv[0], phase, scheme_null, all_bindings);
}

Scheme_Object *scheme_syntax_make_transfer_intro(int argc, Scheme_Object **argv)
{
  Scheme_Object *a[3], *key, *val, *src;
  Scheme_Object *phase;
  Scheme_Scope_Set *delta, *m2;
  intptr_t i;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(argv[0])))
    scheme_wrong_contract("make-syntax-delta-introducer", "identifier?", 0, argc, argv);
  if (!SCHEME_STXP(argv[1]) && !SCHEME_FALSEP(argv[1]))
    scheme_wrong_contract("make-syntax-delta-introducer", "(or/c syntax? #f)", 1, argc, argv);

  phase = extract_phase("make-syntax-delta-introducer", 2, argc, argv, scheme_make_integer(0), 1);

  delta = extract_scope_set((Scheme_Stx *)argv[0], phase);

  src = argv[1];
  if (!SCHEME_FALSEP(src)) {
    m2 = extract_scope_set((Scheme_Stx *)src, phase);
    if (!scope_subset(m2, delta))
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
    i = scope_set_next(m2, -1);
    while (i != -1) {
      scope_set_index(m2, i, &key, &val);
      if (scope_set_get(delta, key))
        delta = scope_set_set(delta, key, NULL);

      i = scope_set_next(m2, i);
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
  Scheme_Scope_Set *current, *m2;
  Scheme_Object *key, *val;
  intptr_t i;
  int mutate = 0;

  current = extract_scope_set((Scheme_Stx *)o, phase);
  m2 = extract_scope_set((Scheme_Stx *)b, phase);

  i = scope_set_next(m2, -1);
  while (i != -1) {
    scope_set_index(m2, i, &key, &val);
    if (!scope_set_get(current, key)) {
      o = stx_adjust_scope(o, key, phase, SCHEME_STX_ADD, &mutate);
    }
    
    i = scope_set_next(m2, i);
  }

  return o;
}

Scheme_Object *scheme_stx_binding_subtract(Scheme_Object *o, Scheme_Object *b, Scheme_Object *phase)
{
  Scheme_Scope_Set *current, *m2;
  Scheme_Object *key, *val;
  intptr_t i;
  int mutate = 0;

  current = extract_scope_set((Scheme_Stx *)o, phase);
  m2 = extract_scope_set((Scheme_Stx *)b, phase);

  i = scope_set_next(m2, -1);
  while (i != -1) {
    scope_set_index(m2, i, &key, &val);
    if (scope_set_get(current, key)) {
      o = stx_adjust_scope(o, key, phase, SCHEME_STX_REMOVE, &mutate);
    }

    i = scope_set_next(m2, i);
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

static Scheme_Object *do_free_eq(const char *who, int delta, int argc, Scheme_Object **argv)
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

  v = scheme_stx_free_eq3(argv[0], argv[1], phase, phase2);

  return (v
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *free_eq(int argc, Scheme_Object **argv)
{
  return do_free_eq("free-identifier=?", 0, argc, argv);
}

static Scheme_Object *free_trans_eq(int argc, Scheme_Object **argv)
{
  return do_free_eq("free-transformer-identifier=?", 1, argc, argv);
}

static Scheme_Object *free_templ_eq(int argc, Scheme_Object **argv)
{
  return do_free_eq("free-template-identifier=?", -1, argc, argv);
}

static Scheme_Object *free_label_eq(int argc, Scheme_Object **argv)
{
  return do_free_eq("free-label-identifier=?", MZ_LABEL_PHASE, argc, argv);
}

static Scheme_Object *do_free_binding(char *name, int argc, Scheme_Object **argv, 
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

static Scheme_Object *free_binding(int argc, Scheme_Object **argv)
{
  return do_free_binding("identifier-binding", argc, argv, scheme_make_integer(0), 0);
}

static Scheme_Object *free_trans_binding(int argc, Scheme_Object **argv)
{
  return do_free_binding("identifier-transformer-binding", argc, argv, scheme_make_integer(1), 0);
}

static Scheme_Object *free_templ_binding(int argc, Scheme_Object **argv)
{
  return do_free_binding("identifier-template-binding", argc, argv, scheme_make_integer(-1), 0);
}

static Scheme_Object *free_label_binding(int argc, Scheme_Object **argv)
{
  return do_free_binding("identifier-label-binding", argc, argv, scheme_false, 0);
}

static Scheme_Object *free_binding_symbol(int argc, Scheme_Object **argv)
{
  return do_free_binding("identifier-binding-symbol", argc, argv, scheme_make_integer(0), 1);
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

  return add_taint_to_stx(argv[0], NULL);
  
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
  SCHEME_VEC_ELS(vec)[2] = (Scheme_Object *)((Scheme_Stx *)stx)->scopes;
  
  return vec;
}

/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_syntax.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_srcloc, mark_srcloc);
  GC_REG_TRAV(scheme_scope_type, mark_scope);
  GC_REG_TRAV(scheme_scope_table_type, mark_scope_table);
  GC_REG_TRAV(scheme_propagate_table_type, mark_propagate_table);
}

END_XFORM_SKIP;

#endif
