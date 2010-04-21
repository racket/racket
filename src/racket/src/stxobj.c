/*
  MzScheme
  Copyright (c) 2004-2010 PLT Scheme Inc.
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

READ_ONLY static Scheme_Object *scheme_datum_to_syntax_proc;
ROSYM static Scheme_Object *source_symbol; /* uninterned! */
ROSYM static Scheme_Object *share_symbol; /* uninterned! */
ROSYM static Scheme_Object *origin_symbol;
ROSYM static Scheme_Object *lexical_symbol;
ROSYM static Scheme_Object *protected_symbol;
ROSYM static Scheme_Object *nominal_id_symbol;

READ_ONLY static Scheme_Stx_Srcloc *empty_srcloc;
READ_ONLY static Scheme_Object *empty_simplified;
READ_ONLY static Scheme_Object *no_nested_inactive_certs;
READ_ONLY static Scheme_Object *no_nested_active_certs;
READ_ONLY static Scheme_Object *no_nested_certs;

THREAD_LOCAL_DECL(static Scheme_Object *nominal_ipair_cache);
THREAD_LOCAL_DECL(static Scheme_Object *mark_id);
THREAD_LOCAL_DECL(static Scheme_Object *current_rib_timestamp);
THREAD_LOCAL_DECL(static Scheme_Hash_Table *quick_hash_table);
THREAD_LOCAL_DECL(static Scheme_Object *last_phase_shift);
THREAD_LOCAL_DECL(static Scheme_Object *unsealed_dependencies);
THREAD_LOCAL_DECL(static Scheme_Hash_Table *id_marks_ht); /* a cache */
THREAD_LOCAL_DECL(static Scheme_Hash_Table *than_id_marks_ht); /* a cache */
THREAD_LOCAL_DECL(static Scheme_Bucket_Table *interned_skip_ribs);


static Scheme_Object *syntax_p(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_to_datum(int argc, Scheme_Object **argv);
static Scheme_Object *datum_to_syntax(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_line(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_col(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_pos(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_span(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_src(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_to_list(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_original_p(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_property(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_property_keys(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_track_origin(int argc, Scheme_Object **argv);

static Scheme_Object *bound_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_trans_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_templ_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_label_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_binding(int argc, Scheme_Object **argv);
static Scheme_Object *module_trans_binding(int argc, Scheme_Object **argv);
static Scheme_Object *module_templ_binding(int argc, Scheme_Object **argv);
static Scheme_Object *module_label_binding(int argc, Scheme_Object **argv);
static Scheme_Object *identifier_prune(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_src_module(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_recertify(int argc, Scheme_Object **argv);

static Scheme_Object *lift_inactive_certs(Scheme_Object *o, int as_active);

static Scheme_Object *write_free_id_info_prefix(Scheme_Object *obj);
static Scheme_Object *read_free_id_info_prefix(Scheme_Object *obj, Scheme_Object *insp);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static int includes_mark(Scheme_Object *wraps, Scheme_Object *mark);
static void add_all_marks(Scheme_Object *wraps, Scheme_Hash_Table *marks);
static struct Scheme_Cert *cons_cert(Scheme_Object *mark, Scheme_Object *modidx, 
				     Scheme_Object *insp, Scheme_Object *key, 
				     struct Scheme_Cert *next_cert);
static void phase_shift_certs(Scheme_Object *o, Scheme_Object *owner_wraps, int len);
static void preemptive_chunk(Scheme_Stx *stx);

#define CONS scheme_make_pair
#define ICONS scheme_make_pair

#define HAS_SUBSTX(obj) (SCHEME_PAIRP(obj) || SCHEME_VECTORP(obj) || SCHEME_BOXP(obj) || prefab_p(obj) || SCHEME_HASHTRP(obj))
#define HAS_CHAPERONE_SUBSTX(obj) (HAS_SUBSTX(obj) || (SCHEME_NP_CHAPERONEP(obj) && HAS_SUBSTX(SCHEME_CHAPERONE_VAL(obj))))

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

typedef struct Module_Renames {
  Scheme_Object so; /* scheme_rename_table_type */
  char kind, needs_unmarshal;
  char sealed; /* 1 means bound won't change; 2 means unbound won't change, either */
  Scheme_Object *phase;
  Scheme_Object *set_identity;
  Scheme_Hash_Table *ht; /* localname ->  modidx  OR
                                          (cons modidx exportname) OR
                                          (cons modidx nominal_modidx) OR
                                          (list* modidx exportname nominal_modidx_plus_phase nominal_exportname) OR
                                          (list* modidx mod-phase exportname nominal_modidx_plus_phase nominal_exportname) OR
                                          (cons insp localname)
                            nominal_modix_plus_phase -> nominal_modix | (cons nominal_modix import_phase_plus_nominal_phase)
                            import_phase_plus_nominal_phase -> import-phase-index | (cons import-phase-index nom-phase) */
  Scheme_Hash_Table *nomarshal_ht; /* like ht, but dropped on marshal */
  Scheme_Object *shared_pes; /* list of (cons modidx (cons phase_export phase_and_marks))
                                  phase_and_marks -> phase-index-int OR
                                                     (cons (nonempty-listof mark) phase-index-int)
                                like nomarshal ht, but shared from provider */
  Scheme_Hash_Table *marked_names; /* shared with module environment while compiling the module;
				      this table maps a top-level-bound identifier with a non-empty mark
				      set to a gensym created for the binding */
  Scheme_Object *unmarshal_info; /* stores some renamings as infomation needed to consult
				    imported modules and restore renames from their exports */
  Scheme_Hash_Table *free_id_renames; /* like `ht', but only for free-id=? checking,
                                         and targets can also include:
                                            id => resolve id (but cache if possible; never appears after simplifying)
                                            (box (cons sym #f)) => top-level binding
                                            (box (cons sym sym)) => lexical binding */
} Module_Renames;

static void unmarshal_rename(Module_Renames *mrn,
			     Scheme_Object *modidx_shift_from, Scheme_Object *modidx_shift_to,
			     Scheme_Hash_Table *export_registry);

typedef struct Module_Renames_Set {
  Scheme_Object so; /* scheme_rename_table_set_type */
  char kind, sealed;
  Scheme_Object *set_identity;
  Module_Renames *rt, *et;
  Scheme_Hash_Table *other_phases;
  Scheme_Object *share_marked_names; /* a Module_Renames_Set */
} Module_Renames_Set;

typedef struct Scheme_Cert {
  Scheme_Inclhash_Object iso;
  Scheme_Object *mark;
  Scheme_Object *modidx;
  Scheme_Object *insp;
  Scheme_Object *key;
  Scheme_Object *mapped; /* Indicates which mark+key combinations are in
			    this chain. The table is created for every 16
			    items in the list. For a power of 2, all items
			    in the rest of the chain are in the table, and
			    the "next" pointer is NULL. For 2^n + 2^m, then
                            2^m items are in the table, and so on. Overall, the
			    chain's total size if O(n * lg n) for a chain of
			    length n, and lookup for a mark+key pair is
			    O(lg n). */
  int depth;
  struct Scheme_Cert *next;
} Scheme_Cert;

#define CERT_NO_KEY(c) (MZ_OPT_HASH_KEY(&(c)->iso) & 0x1)
#define CERT_SET_NO_KEY(c) (MZ_OPT_HASH_KEY(&(c)->iso) |= 0x1)

/* Certs encoding:
    - NULL: no inactive or active certs; 
            maybe inactive certs in nested parts
    - rcons(c1, c2): active certs c1 (maybe NULL), inactive certs c2 (maybe NULL); 
            maybe inactive certs in nested parts 
    Use flags 0x1 and 02 to indicate no inactive or active certs in nested parts */
#define ACTIVE_CERTS(stx) ((Scheme_Cert *)((stx)->certs ? (SCHEME_RPAIRP((stx)->certs) ? SCHEME_CAR((stx)->certs) : (stx)->certs) : NULL))
#define INACTIVE_CERTS(stx) ((Scheme_Cert *)((stx)->certs ? (SCHEME_RPAIRP((stx)->certs) ? SCHEME_CDR((stx)->certs) : NULL) : NULL))
static Scheme_Object *stx_strip_certs(Scheme_Object *o, Scheme_Cert **cp, int active);

#define SCHEME_NO_INACTIVE_SUBS_P(obj) (MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)(obj)) & 0x1)
#define SCHEME_NO_ACTIVE_SUBS_P(obj) (MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)(obj)) & 0x2)
#define SCHEME_SET_NO_X_SUBS(obj, flag) (MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)(obj)) |= flag)
#define SCHEME_SET_NO_INACTIVE_SUBS(obj) SCHEME_SET_NO_X_SUBS(obj, 0x1)
#define SCHEME_SET_NO_ACTIVE_SUBS(obj) SCHEME_SET_NO_X_SUBS(obj, 0x2)

#define SCHEME_RENAME_LEN(vec)  ((SCHEME_VEC_SIZE(vec) - 2) >> 1)

typedef struct Scheme_Lexical_Rib {
  Scheme_Object so;
  Scheme_Object *rename; /* a vector for a lexical rename */
  Scheme_Object *timestamp;
  int *sealed;
  Scheme_Object *mapped_names; /* only in the initial link; int or hash table */
  struct Scheme_Lexical_Rib *next;
} Scheme_Lexical_Rib;

#define SCHEME_RENAMESP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_rename_table_type))
#define SCHEME_RENAMES_SETP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_rename_table_set_type))

#define SCHEME_MODIDXP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_module_index_type))
#define SCHEME_RIB_DELIMP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_rib_delimiter_type))

#define SCHEME_PRUNEP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_prune_context_type))

XFORM_NONGCING static int is_member(Scheme_Object *a, Scheme_Object *l)
{
  while (SCHEME_PAIRP(l)) {
    if (SAME_OBJ(a, SCHEME_CAR(l)))
      return 1;
    l = SCHEME_CDR(l);
  }
  return 0;
}

static int is_rename_inspector_info(Scheme_Object *v)
{
  return (SAME_TYPE(SCHEME_TYPE(v), scheme_inspector_type)
          || (SCHEME_PAIRP(v)
              && SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(v)), scheme_inspector_type)
              && SAME_TYPE(SCHEME_TYPE(SCHEME_CDR(v)), scheme_inspector_type)));
}

/* Wraps:

   A wrap is a list of wrap-elems and wrap-chunks. A wrap-chunk is a
   "vector" (a scheme_wrap_chunk_type) of wrap-elems.

   Each wrap-elem has one of several shapes:

   - A wrap-elem <+num> is a mark

   - A wrap-elem <-num> is a certificate-only mark (doesn't conttribute to
       id equivalence)

   - A wrap-elem (vector <sym> <ht> <stx> ..._0 <recur-state> ..._0) is a lexical rename
                         env  (sym   var      <var-resolved>:
                              ->pos)           void => not yet computed
                              or #f            sym => var-resolved is answer to replace #f
                                                      for nozero skipped ribs
                                               (rlistof (rcons skipped sym)) => generalization of sym
                                               (mcons var-resolved next) => depends on unsealed rib,
                                                      will be cleared when rib set
                                              or:
                                               (cons <var-resolved> (cons <id> <phase>)) =>
                                                      free-id=? renaming to <id> on match
   - A wrap-elem (vector <free-id-renames?> <ht> <sym> ..._0 <sym> ..._0) is also a lexical rename
                               bool               var       resolved: sym or (cons <sym> <bind-info>), 
                                                             where <bind-info> is module/lexical binding info:
                                                              (cons <sym> #f) => top-level binding
                                                              (cons <sym> <sym>) => lexical binding
                                                              (free-eq-info ...) => module-binding
         where the variables have already been resolved and filtered (no mark
         or lexical-env comparison needed with the remaining wraps)

   - A wrap-elem (make-rib vector rib)
         is an extensible set of lexical renames; it is the same as
         having the vectors inline in place of the rib, except that
         new vectors can be added imperatively; simplification turns this
	 into a vector

   - A wrap-elem (make-rib-delimiter <list-of-rib>)
         appears in pairs around rib elements; the deeper is just a
         bracket, while the shallow one contains a non-empty list of
         ribs; for each environment name defined within the set of
         ribs, no rib within the set can build on a binding to that
         environment past the end delimiter; this is used by `local-expand'
         when given a list of ribs, and simplifcation eliminates
         rib delimiters

   - A wrap-elem (make-prune <sym>)
         restricts binding information to that relevant for <sym>
         as a datum

   - A wrap-elem <rename-table> is a module rename set
         the hash table maps renamed syms to modname-srcname pairs

   - A wrap-elem <rename-table-set> is a set of <rename-table>s for
         different phases.

   - A wrap-elem <hash-table> is a chain-specific cache; it maps
         identifiers to #t, and 0 to a deeper part of the chain; a
         resolution for an identifier can safely skip to the deeper
         part if the identifer does not have a mapping; this skips
         simple lexical renames (not ribs) and marks, only, and it's
         inserted into a chain heuristically

   - A wrap-elem (box (vector <num> <midx> <midx> <export-registry>))
         is a phase shift by <num>, remapping the first <midx> to the 
         second <midx>; the <export-registry> part is for finding
         modules to unmarshal import renamings

     [Don't add a pair case, because sometimes we test for element 
      versus list-of-element.]

  The lazy_prefix field of a syntax object keeps track of how many of
  the first wraps (items and chunks in the list) need to be propagated
  to sub-syntax.  */

#define IS_POSMARK(x) (SCHEME_INTP(x) ? (SCHEME_INT_VAL(x) >= 0) : SCHEME_BIGPOS(x))
#define SCHEME_MARKP(x) (SCHEME_INTP(x) || SCHEME_BIGNUMP(x))

XFORM_NONGCING static int nom_mod_p(Scheme_Object *p)
{
  p = SCHEME_CDR(p);
  return !SCHEME_PAIRP(p) && !SCHEME_SYMBOLP(p);
}

/*========================================================================*/
/*                            wrap chunks                                 */
/*========================================================================*/

typedef struct {
  Scheme_Type type;
  mzshort len;
  Scheme_Object *a[1];
} Wrap_Chunk;

#define MALLOC_WRAP_CHUNK(n) (Wrap_Chunk *)scheme_malloc_tagged(sizeof(Wrap_Chunk) + ((n - 1) * sizeof(Scheme_Object *)))

/* Macros for iterating over the elements of a wrap. */

typedef struct {
  Scheme_Object *l;
  Scheme_Object *a;
  int is_limb;
  int pos;
} Wrap_Pos;

XFORM_NONGCING static void WRAP_POS_SET_FIRST(Wrap_Pos *w)
{
  if (!SCHEME_NULLP(w->l)) {
    Scheme_Object *a;
    a = SCHEME_CAR(w->l);
    if (SCHEME_TYPE(a) == scheme_wrap_chunk_type) {
      w->is_limb = 1;
      w->pos = 0;
      w->a = ((Wrap_Chunk *)a)->a[0];
    } else {
      w->is_limb = 0;
      w->a = a;
    }
  }
  /* silence gcc "may be used uninitialized in this function" warnings */
  else {
    w->a = NULL;
    w->is_limb = 0;
  }
}

XFORM_NONGCING static MZ_INLINE void DO_WRAP_POS_INC(Wrap_Pos *w)
{
  Scheme_Object *a;
  if (w->is_limb && (w->pos + 1 < ((Wrap_Chunk *)SCHEME_CAR(w->l))->len)) {
    a = SCHEME_CAR(w->l);
    w->pos++;
    w->a = ((Wrap_Chunk *)a)->a[w->pos];
  } else {
    w->l = SCHEME_CDR(w->l);
    if (!SCHEME_NULLP(w->l)) {
      a = SCHEME_CAR(w->l);
      if (SCHEME_TYPE(a) == scheme_wrap_chunk_type) {
	w->is_limb = 1;
	w->pos = 0;
	w->a = ((Wrap_Chunk *)a)->a[0];
      } else {
	w->is_limb = 0;
	w->a = a;
      }
    } else
      w->is_limb = 0;
  }
}

#define WRAP_POS Wrap_Pos
#define WRAP_POS_INIT(w, wr) w.l = wr; WRAP_POS_SET_FIRST(&w)

#define WRAP_POS_INC(w) DO_WRAP_POS_INC(&w)

#define WRAP_POS_INIT_END(w) (w.l = scheme_null, w.a = NULL, w.is_limb = 0, w.pos = 0)
#define WRAP_POS_END_P(w) SCHEME_NULLP(w.l)
#define WRAP_POS_FIRST(w) w.a
#define WRAP_POS_COPY(w, w2) w.l = (w2).l; w.a = (w2).a; w.is_limb= (w2).is_limb; w.pos = (w2).pos

/* Walking backwards through one chunk: */

XFORM_NONGCING static void DO_WRAP_POS_REVINIT(Wrap_Pos *w, Scheme_Object *k)
{
  Scheme_Object *a;
  a = SCHEME_CAR(k);
  if (SCHEME_TYPE(a) == scheme_wrap_chunk_type) {
    w->is_limb = 1;
    w->l = k;
    w->pos = ((Wrap_Chunk *)a)->len - 1;
    w->a = ((Wrap_Chunk *)a)->a[w->pos];
  } else {
    w->l = k;
    w->a = a;
    w->is_limb = 0;
    w->pos = 0;
  }
}

#define WRAP_POS_KEY(w) w.l
#define WRAP_POS_REVINIT(w, k) DO_WRAP_POS_REVINIT(&w, k)
#define WRAP_POS_REVEND_P(w) (w.pos < 0)
#define WRAP_POS_DEC(w) --w.pos; if (w.pos >= 0) w.a = ((Wrap_Chunk *)SCHEME_CAR(w.l))->a[w.pos]

#define WRAP_POS_PLAIN_TAIL(w) (w.is_limb ? (w.pos ? NULL : w.l) : w.l)

/*========================================================================*/
/*                           initialization                               */
/*========================================================================*/

void scheme_init_stx(Scheme_Env *env)
{
  Scheme_Object *p;

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  p = scheme_make_folding_prim(syntax_p, "syntax?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("syntax?", p, env);

  scheme_add_global_constant("syntax->datum", 
			     scheme_make_folding_prim(syntax_to_datum,
						      "syntax->datum",
						      1, 1, 1),
			     env);
  
  REGISTER_SO(scheme_datum_to_syntax_proc);
  scheme_datum_to_syntax_proc = scheme_make_folding_prim(datum_to_syntax,
							 "datum->syntax",
							 2, 5, 1);
  scheme_add_global_constant("datum->syntax", 
			     scheme_datum_to_syntax_proc,
			     env);

  
  p = scheme_make_folding_prim(scheme_checked_syntax_e, "syntax-e", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("syntax-e", p, env);

  scheme_add_global_constant("syntax-line", 
			     scheme_make_folding_prim(syntax_line,
						      "syntax-line",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("syntax-column", 
			     scheme_make_folding_prim(syntax_col,
						      "syntax-column",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("syntax-position", 
			     scheme_make_folding_prim(syntax_pos,
						      "syntax-position",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("syntax-span", 
			     scheme_make_folding_prim(syntax_span,
						      "syntax-span",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("syntax-source", 
			     scheme_make_folding_prim(syntax_src,
						      "syntax-source",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("syntax->list", 
			     scheme_make_folding_prim(syntax_to_list,
						      "syntax->list",
						      1, 1, 1),
			     env);

  scheme_add_global_constant("syntax-original?", 
			     scheme_make_immed_prim(syntax_original_p,
						    "syntax-original?",
						    1, 1),
			     env);
  scheme_add_global_constant("syntax-property", 
			     scheme_make_immed_prim(syntax_property,
						    "syntax-property",
						    2, 3),
			     env);
  scheme_add_global_constant("syntax-property-symbol-keys", 
			     scheme_make_immed_prim(syntax_property_keys,
						    "syntax-property-symbol-keys",
						    1, 1),
			     env);

  scheme_add_global_constant("syntax-track-origin", 
			     scheme_make_immed_prim(syntax_track_origin,
						    "syntax-track-origin",
						    3, 3),
			     env);

  scheme_add_global_constant("make-syntax-delta-introducer", 
			     scheme_make_immed_prim(scheme_syntax_make_transfer_intro,
						    "make-syntax-delta-introducer",
						    2, 3),
			     env);

  scheme_add_global_constant("bound-identifier=?", 
			     scheme_make_immed_prim(bound_eq,
						    "bound-identifier=?",
						    2, 3),
			     env);
  scheme_add_global_constant("free-identifier=?", 
			     scheme_make_immed_prim(module_eq,
						    "free-identifier=?",
						    2, 3),
			     env);
  scheme_add_global_constant("free-transformer-identifier=?", 
			     scheme_make_immed_prim(module_trans_eq,
						    "free-transformer-identifier=?",
						    2, 2),
			     env);
  scheme_add_global_constant("free-template-identifier=?", 
			     scheme_make_immed_prim(module_templ_eq,
						    "free-template-identifier=?",
						    2, 2),
			     env);
  scheme_add_global_constant("free-label-identifier=?", 
			     scheme_make_immed_prim(module_label_eq,
						    "free-label-identifier=?",
						    2, 2),
			     env);

  scheme_add_global_constant("identifier-binding", 
			     scheme_make_immed_prim(module_binding,
						    "identifier-binding",
						    1, 2),
			     env);
  scheme_add_global_constant("identifier-transformer-binding", 
			     scheme_make_immed_prim(module_trans_binding,
						    "identifier-transformer-binding",
						    1, 2),
			     env);
  scheme_add_global_constant("identifier-template-binding", 
			     scheme_make_immed_prim(module_templ_binding,
						    "identifier-template-binding",
						    1, 1),
			     env);
  scheme_add_global_constant("identifier-label-binding", 
			     scheme_make_immed_prim(module_label_binding,
						    "identifier-label-binding",
						    1, 1),
			     env);
  scheme_add_global_constant("identifier-prune-lexical-context", 
			     scheme_make_immed_prim(identifier_prune,
						    "identifier-prune-lexical-context",
						    1, 2),
			     env);


  scheme_add_global_constant("syntax-source-module", 
			     scheme_make_noncm_prim(syntax_src_module,
                                                    "syntax-source-module",
                                                    1, 2),
			     env);

  scheme_add_global_constant("syntax-recertify", 
			     scheme_make_immed_prim(syntax_recertify,
						    "syntax-recertify",
						    4, 4),
			     env);

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

  REGISTER_SO(mark_id);
  REGISTER_SO(current_rib_timestamp);
  mark_id = scheme_make_integer(0);
  current_rib_timestamp = scheme_make_integer(0);

  REGISTER_SO(empty_srcloc);
  empty_srcloc = MALLOC_ONE_RT(Scheme_Stx_Srcloc);
#ifdef MZTAG_REQUIRED
  empty_srcloc->type = scheme_rt_srcloc;
#endif
  empty_srcloc->src = scheme_false;
  empty_srcloc->line = -1;
  empty_srcloc->col = -1;
  empty_srcloc->pos = -1;

  REGISTER_SO(empty_simplified);
  empty_simplified = scheme_make_vector(2, scheme_false);

  REGISTER_SO(no_nested_inactive_certs);
  REGISTER_SO(no_nested_active_certs);
  REGISTER_SO(no_nested_certs);
  no_nested_inactive_certs = scheme_make_raw_pair(NULL, NULL);
  no_nested_active_certs = scheme_make_raw_pair(NULL, NULL);
  no_nested_certs = scheme_make_raw_pair(NULL, NULL);
  SCHEME_SET_NO_INACTIVE_SUBS(no_nested_inactive_certs);
  SCHEME_SET_NO_ACTIVE_SUBS(no_nested_active_certs);
  SCHEME_SET_NO_INACTIVE_SUBS(no_nested_certs);
  SCHEME_SET_NO_ACTIVE_SUBS(no_nested_certs);

  scheme_install_type_writer(scheme_free_id_info_type, write_free_id_info_prefix);
  scheme_install_type_reader2(scheme_free_id_info_type, read_free_id_info_prefix);
}

void scheme_init_stx_places(int initial_main_os_thread) {
  REGISTER_SO(last_phase_shift);
  REGISTER_SO(nominal_ipair_cache);
  REGISTER_SO(quick_hash_table);
  REGISTER_SO(id_marks_ht);
  REGISTER_SO(than_id_marks_ht);
  REGISTER_SO(interned_skip_ribs);
  REGISTER_SO(unsealed_dependencies);
  
  if (!initial_main_os_thread) {
    REGISTER_SO(mark_id);
    REGISTER_SO(current_rib_timestamp);
    mark_id = scheme_make_integer(0);
    current_rib_timestamp = scheme_make_integer(0);
  }

  interned_skip_ribs = scheme_make_weak_equal_table();
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
  stx->wraps = scheme_null;
  stx->props = props;

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_make_stx_w_offset(Scheme_Object *val, 
					long line, long col, long pos, long span,
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

Scheme_Object *scheme_make_renamed_stx(Scheme_Object *sym, 
				       Scheme_Object *rn)
{
  Scheme_Object *stx;

  stx = scheme_make_stx(sym, empty_srcloc, NULL);

  if (rn) {
    rn = scheme_make_pair(rn, scheme_null);
    ((Scheme_Stx *)stx)->wraps = rn;
  }

  return stx;
}

Scheme_Object *scheme_stx_track(Scheme_Object *naya, 
				Scheme_Object *old,
				Scheme_Object *origin)
     /* Maintain properties for an expanded expression */
{
  Scheme_Stx *nstx = (Scheme_Stx *)naya;
  Scheme_Stx *ostx = (Scheme_Stx *)old;
  Scheme_Object *ne, *oe, *e1, *e2;
  Scheme_Object *certs;
  Scheme_Object *wraps, *modinfo_cache;
  long lazy_prefix;

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

  wraps = nstx->wraps;
  if (STX_KEY(nstx) & STX_SUBSTX_FLAG) {
    modinfo_cache = NULL;
    lazy_prefix = nstx->u.lazy_prefix;
  } else {
    modinfo_cache = nstx->u.modinfo_cache;
    lazy_prefix = 0;
  }

  certs = nstx->certs;

  nstx = (Scheme_Stx *)scheme_make_stx(nstx->val, nstx->srcloc, ne);

  nstx->wraps = wraps;
  if (modinfo_cache)
    nstx->u.modinfo_cache = modinfo_cache;
  else
    nstx->u.lazy_prefix = lazy_prefix;

  nstx->certs = certs;

  return (Scheme_Object *)nstx;
}

/******************** chain cache ********************/

static int maybe_add_chain_cache(Scheme_Stx *stx)
{
  WRAP_POS awl;
  Scheme_Object *p;
  int skipable = 0, pos = 1;

  WRAP_POS_INIT(awl, ((Scheme_Stx *)stx)->wraps);

  while (!WRAP_POS_END_P(awl)) {
    /* Skip over renames, cancelled marks, and negative marks: */
    p = WRAP_POS_FIRST(awl);
    if (SCHEME_VECTORP(p)) {
      skipable++;
    } else if (SCHEME_NUMBERP(p) || SCHEME_SYMBOLP(p)) {
      /* ok to skip, but don<'t count toward needing a cache */
    } else if (SCHEME_HASHTP(p)) {
      /* Hack: we store the depth of the table in the chain
	 in the `size' fields, at least until the table is initialized: */
      Scheme_Hash_Table *ht2 = (Scheme_Hash_Table *)p;
      if (!ht2->count)
	pos = ht2->size;
      else {
	p = scheme_hash_get(ht2, scheme_make_integer(2));
	pos = SCHEME_INT_VAL(p);
      }
      pos++;
      break;
    } else
      break;
    WRAP_POS_INC(awl);
  }

  if (skipable >= 32) {
    /* Insert a cache placeholder. We'll fill it if
       it's ever used in resolve_env(). */
    Scheme_Hash_Table *ht;

    ht = scheme_make_hash_table(SCHEME_hash_ptr);

    ht->size = pos;

    p = scheme_make_pair((Scheme_Object *)ht, stx->wraps);
    stx->wraps = p;
    
    if (STX_KEY(stx) & STX_SUBSTX_FLAG)
      stx->u.lazy_prefix++;

    return 1;
  }

  return 0;
}

static void set_wraps_to_skip(Scheme_Hash_Table *ht, WRAP_POS *wraps)
{
  Scheme_Object *v;

  v = scheme_hash_get(ht, scheme_make_integer(0));
  wraps->l = v;
  v = scheme_hash_get(ht, scheme_make_integer(1));
  if (SCHEME_TRUEP(v)) {
    wraps->pos = SCHEME_INT_VAL(v);
    wraps->is_limb = 1;
    wraps->a = ((Wrap_Chunk *)SCHEME_CAR(wraps->l))->a[wraps->pos];
  } else {
    wraps->is_limb = 0;
    if (!SCHEME_NULLP(wraps->l))
      wraps->a = SCHEME_CAR(wraps->l);
  }
}

static void fill_chain_cache(Scheme_Object *wraps)
{
  int pos, max_depth, limit;
  Scheme_Hash_Table *ht;
  Scheme_Object *p, *id;
  WRAP_POS awl;

  ht = (Scheme_Hash_Table *)SCHEME_CAR(wraps);

  p = scheme_hash_get(ht, scheme_make_integer(5));
  if (p) {
    limit = SCHEME_INT_VAL(p);

    /* Extend the chain cache to deeper: */
    set_wraps_to_skip(ht, &awl);

    p = scheme_hash_get(ht, scheme_make_integer(2));
    pos = SCHEME_INT_VAL(p);

    scheme_hash_set(ht, scheme_make_integer(5), NULL);
  } else {
    pos = ht->size;
    ht->size = 0;

    wraps = SCHEME_CDR(wraps);

    WRAP_POS_INIT(awl, wraps);

    limit = 4;
  }

  /* Limit how much of the cache we build, in case we never
     reuse this cache: */
  max_depth = limit;

  while (!WRAP_POS_END_P(awl)) {
    if (!(max_depth--)) {
      limit *= 2;
      scheme_hash_set(ht, scheme_make_integer(5), scheme_make_integer(limit));
      break;
    }

    p = WRAP_POS_FIRST(awl);
    if (SCHEME_VECTORP(p)) {
      int i, len;
      len = SCHEME_RENAME_LEN(p);
      for (i = 0; i < len; i++) {
	id = SCHEME_VEC_ELS(p)[i+2];
	if (SCHEME_STXP(id))
	  id = SCHEME_STX_VAL(id);
	scheme_hash_set(ht, id, scheme_true);
      }
    } else if (SCHEME_NUMBERP(p) || SCHEME_SYMBOLP(p)) {
      /* ok to skip */
    } else if (SCHEME_HASHTP(p)) {
      /* Hack: we store the depth of the table in the chain
	 in the `size' fields, at least until the table is initialized: */
      Scheme_Hash_Table *ht2 = (Scheme_Hash_Table *)p;
      int pos2;
      if (!ht2->count)
	pos2 = ht2->size;
      else {
	p = scheme_hash_get(ht2, scheme_make_integer(2));
	pos2 = SCHEME_INT_VAL(p);
      }
      /* The theory here is the same as the `mapped' table:
	 every power of two covers the whole range, etc. */
      if ((pos & pos2) == pos2)
	break;
    } else
      break;
    WRAP_POS_INC(awl);
  }

  /* Record skip destination: */
  scheme_hash_set(ht, scheme_make_integer(0), awl.l);
  if (!awl.is_limb) {
    scheme_hash_set(ht, scheme_make_integer(1), scheme_false);
  } else {
    scheme_hash_set(ht, scheme_make_integer(1), scheme_make_integer(awl.pos));
  }
  scheme_hash_set(ht, scheme_make_integer(2), scheme_make_integer(pos));
}

/******************** marks ********************/

Scheme_Object *scheme_new_mark()
{
  mark_id = scheme_add1(1, &mark_id);
  return mark_id;
}

static Scheme_Object *negate_mark(Scheme_Object *n)
{
  return scheme_bin_minus(scheme_make_integer(0), n);
}

Scheme_Object *scheme_add_remove_mark(Scheme_Object *o, Scheme_Object *m)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *wraps;
  Scheme_Object *certs;
  long lp;

  if (STX_KEY(stx) & STX_SUBSTX_FLAG)
    lp = stx->u.lazy_prefix;
  else
    lp = 1;

  wraps = stx->wraps;
  if (SCHEME_PAIRP(wraps)
      && SAME_OBJ(m, SCHEME_CAR(wraps))
      && lp) {
    --lp;
    wraps = SCHEME_CDR(wraps);
  } else {
    if (maybe_add_chain_cache(stx))
      lp++;
    wraps = stx->wraps;
    lp++;
    wraps = CONS(m, wraps);
  }

  certs = stx->certs;
  stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
  stx->wraps = wraps;
  stx->certs = certs;

  if (STX_KEY(stx) & STX_SUBSTX_FLAG)
    stx->u.lazy_prefix = lp;
  /* else cache should stay zeroed */

  return (Scheme_Object *)stx;
}

/******************** lexical renames ********************/

#define RENAME_HT_THRESHOLD 15

Scheme_Object *scheme_make_rename(Scheme_Object *newname, int c)
{
  Scheme_Object *v;
  int i;

  v = scheme_make_vector((2 * c) + 2, NULL);
  SCHEME_VEC_ELS(v)[0] = newname;
  if (c > RENAME_HT_THRESHOLD) {
    Scheme_Hash_Table *ht;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    SCHEME_VEC_ELS(v)[1] = (Scheme_Object *)ht;
  } else 
    SCHEME_VEC_ELS(v)[1] = scheme_false;

  for (i = 0; i < c; i++) {
    SCHEME_VEC_ELS(v)[2 + c + i] = scheme_void;
  }

  return v;
}

static void maybe_install_rename_hash_table(Scheme_Object *v)
{
  if (SCHEME_VEC_SIZE(v) > ((2 * RENAME_HT_THRESHOLD) + 2)) {
    Scheme_Hash_Table *ht;
    int i;

    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    MZ_OPT_HASH_KEY(&(ht->iso)) |= 0x1;
    for (i = (SCHEME_VEC_SIZE(v) - 2) >> 1; i--; ) {
      scheme_hash_set(ht, SCHEME_VEC_ELS(v)[i + 2], scheme_make_integer(i));
    }
    SCHEME_VEC_ELS(v)[1] = (Scheme_Object *)ht;
  }
}

void scheme_set_rename(Scheme_Object *rnm, int pos, Scheme_Object *oldname)
{
  /* Every added name must be symbolicly distinct! */

  SCHEME_VEC_ELS(rnm)[2 + pos] = oldname;

  /* Add ht mapping, if there's a hash table: */
  if (!SCHEME_FALSEP(SCHEME_VEC_ELS(rnm)[1])) {
    Scheme_Hash_Table *ht;
    ht = (Scheme_Hash_Table *)SCHEME_VEC_ELS(rnm)[1];
    if (scheme_hash_get(ht, SCHEME_STX_VAL(oldname)))
      pos = -1; /* -1 means multiple entries matching a name */
    scheme_hash_set(ht, SCHEME_STX_VAL(oldname), scheme_make_integer(pos));
  }
}

Scheme_Object *scheme_make_rename_rib()
{
  Scheme_Lexical_Rib *rib;
  int *sealed;

  rib = MALLOC_ONE_TAGGED(Scheme_Lexical_Rib);
  rib->so.type = scheme_lexical_rib_type;
  rib->timestamp = current_rib_timestamp;

  sealed = (int *)scheme_malloc_atomic(sizeof(int));
  *sealed = 0;
  rib->sealed = sealed;

  current_rib_timestamp = scheme_add1(1, &current_rib_timestamp);

  return (Scheme_Object *)rib;
}

void scheme_add_rib_rename(Scheme_Object *ro, Scheme_Object *rename)
{
  Scheme_Lexical_Rib *rib, *naya;
  Scheme_Object *next;
  Scheme_Hash_Table *mapped_names;
  int i;

  naya = MALLOC_ONE_TAGGED(Scheme_Lexical_Rib);
  naya->so.type = scheme_lexical_rib_type;
  naya->rename = rename;

  rib = (Scheme_Lexical_Rib *)ro;
  naya->next = rib->next;
  rib->next = naya;

  naya->timestamp = rib->timestamp;
  naya->sealed = rib->sealed;

  while (unsealed_dependencies) {
    next = SCHEME_CDR(unsealed_dependencies);
    SCHEME_CAR(unsealed_dependencies) = NULL;
    SCHEME_CDR(unsealed_dependencies) = NULL;    
    unsealed_dependencies = next;
  }

  if (!rib->mapped_names)
    rib->mapped_names = scheme_make_integer(1);
  else if (SCHEME_INTP(rib->mapped_names)) {
    rib->mapped_names = scheme_make_integer(SCHEME_INT_VAL(rib->mapped_names) + 1);
    if (SCHEME_INT_VAL(rib->mapped_names) > 32) {
      /* Build the initial table */
      mapped_names = scheme_make_hash_table(SCHEME_hash_ptr);
      while (naya) {
        for (i = SCHEME_RENAME_LEN(naya->rename); i--; ) {
          scheme_hash_set(mapped_names, 
                          SCHEME_STX_SYM(SCHEME_VEC_ELS(naya->rename)[2+i]), 
                          scheme_true);
        }
        naya = naya->next;
      }
      rib->mapped_names = (Scheme_Object *)mapped_names;
    }
  } else {
    for (i = SCHEME_RENAME_LEN(naya->rename); i--; ) {
      scheme_hash_set((Scheme_Hash_Table *)rib->mapped_names, 
                      SCHEME_STX_SYM(SCHEME_VEC_ELS(naya->rename)[2+i]),
                      scheme_true);
    }
  }
}

void scheme_drop_first_rib_rename(Scheme_Object *ro)
{
  Scheme_Lexical_Rib *rib = (Scheme_Lexical_Rib *)ro;
  rib->next = rib->next->next;
}

void scheme_stx_seal_rib(Scheme_Object *rib)
{
  *((Scheme_Lexical_Rib *)rib)->sealed = 1;
}

int *scheme_stx_get_rib_sealed(Scheme_Object *rib)
{
  return ((Scheme_Lexical_Rib *)rib)->sealed;
}

Scheme_Object *scheme_stx_id_remove_rib(Scheme_Object *stx, Scheme_Object *ro)
{
  Scheme_Object *v;
  int count = 0, rib_count = 0;
  WRAP_POS awl;
  Wrap_Chunk *wc;
  Scheme_Lexical_Rib *rib = (Scheme_Lexical_Rib *)ro, *rib2;

  WRAP_POS_INIT(awl, ((Scheme_Stx *)stx)->wraps);
  while (!WRAP_POS_END_P(awl)) {
    count++;
    v = WRAP_POS_FIRST(awl);
    if (SCHEME_RIBP(v)) {
      rib2 = (Scheme_Lexical_Rib *)v;
      if (SAME_OBJ(rib2->timestamp, rib->timestamp))
        rib_count++;
    }
    WRAP_POS_INC(awl);
  }

  if (!rib_count)
    return stx;

  count -= rib_count;

  wc = MALLOC_WRAP_CHUNK(count);
  wc->type = scheme_wrap_chunk_type;
  wc->len = count;

  count = 0;
  WRAP_POS_INIT(awl, ((Scheme_Stx *)stx)->wraps);
  while (!WRAP_POS_END_P(awl)) {
    v = WRAP_POS_FIRST(awl);
    if (SCHEME_RIBP(v)) {
      rib2 = (Scheme_Lexical_Rib *)v;
      if (SAME_OBJ(rib2->timestamp, rib->timestamp))
        v = NULL;
    }
    if (v) {
      wc->a[count++] = v;
    }
    WRAP_POS_INC(awl);
  }

  v = scheme_make_pair((Scheme_Object *)wc, scheme_null);

  stx = scheme_add_rename(stx, scheme_make_integer(0));
  ((Scheme_Stx *)stx)->wraps = v;
  
  return stx;
}

static Scheme_Object *make_prune_context(Scheme_Object *a)
{
  Scheme_Object *p;

  p = scheme_alloc_small_object();
  p->type = scheme_prune_context_type;
  SCHEME_BOX_VAL(p) = a;

  return p;
}

/******************** module renames ********************/

static int same_phase(Scheme_Object *a, Scheme_Object *b)
{
  if (SAME_OBJ(a, b))
    return 1;
  else if (SCHEME_INTP(a) || SCHEME_INTP(b)
           || SCHEME_FALSEP(a) || SCHEME_FALSEP(b))
    return 0;
  else
    return scheme_eqv(a, b);
}

Scheme_Object *scheme_make_module_rename_set(int kind, Scheme_Object *share_marked_names)
{
  Module_Renames_Set *mrns;
  Scheme_Object *mk;

  if (share_marked_names)
    mk = ((Module_Renames_Set *)share_marked_names)->set_identity;
  else
    mk = scheme_new_mark();

  mrns = MALLOC_ONE_TAGGED(Module_Renames_Set);
  mrns->so.type = scheme_rename_table_set_type;
  mrns->kind = kind;
  mrns->share_marked_names = share_marked_names;
  mrns->set_identity = mk;

  return (Scheme_Object *)mrns;
}

void scheme_add_module_rename_to_set(Scheme_Object *set, Scheme_Object *rn)
{
  Module_Renames_Set *mrns = (Module_Renames_Set *)set;
  Module_Renames *mrn = (Module_Renames *)rn;

  mrn->set_identity = mrns->set_identity;

  if (same_phase(mrn->phase, scheme_make_integer(0)))
    mrns->rt = mrn;
  else if (same_phase(mrn->phase, scheme_make_integer(1)))
    mrns->et = mrn;
  else {
    Scheme_Hash_Table *ht;
    ht = mrns->other_phases;
    if (!ht) {
      ht = scheme_make_hash_table_equal();
      mrns->other_phases = ht;
    }
    scheme_hash_set(ht, mrn->phase, (Scheme_Object *)mrn);
  }
}

Scheme_Object *scheme_get_module_rename_from_set(Scheme_Object *set, Scheme_Object *phase, int create)
{
  Module_Renames_Set *mrns = (Module_Renames_Set *)set;
  Module_Renames *mrn;

  if (same_phase(phase, scheme_make_integer(0)))
    mrn = mrns->rt;
  else if (same_phase(phase, scheme_make_integer(1)))
    mrn = mrns->et;
  else if (mrns->other_phases)
    mrn = (Module_Renames *)scheme_hash_get(mrns->other_phases, phase);
  else
    mrn = NULL;

  if (!mrn && create) {
    Scheme_Hash_Table *marked_names;

    if (mrns->share_marked_names)
      marked_names = scheme_get_module_rename_marked_names(mrns->share_marked_names, phase, 1);
    else
      marked_names = NULL;

    mrn = (Module_Renames *)scheme_make_module_rename(phase, mrns->kind, marked_names);

    scheme_add_module_rename_to_set(set, (Scheme_Object *)mrn);
  }

  return (Scheme_Object *)mrn;
}

Scheme_Hash_Table *scheme_get_module_rename_marked_names(Scheme_Object *set, Scheme_Object *phase, int create)
{
  Scheme_Object *rn;

  rn = scheme_get_module_rename_from_set(set, phase, create);
  if (!rn)
    return NULL;

  if (((Module_Renames *)rn)->marked_names)
    return ((Module_Renames *)rn)->marked_names;

  if (create) {
    Scheme_Hash_Table *ht;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    ((Module_Renames *)rn)->marked_names = ht;
    return ht;
  }

  return NULL;
}

Scheme_Object *scheme_make_module_rename(Scheme_Object *phase, int kind, Scheme_Hash_Table *marked_names)
{
  Module_Renames *mr;
  Scheme_Hash_Table *ht;
  Scheme_Object *mk;

  mk = scheme_new_mark();

  mr = MALLOC_ONE_TAGGED(Module_Renames);
  mr->so.type = scheme_rename_table_type;

  ht = scheme_make_hash_table(SCHEME_hash_ptr);

  mr->ht = ht;
  mr->phase = phase;
  mr->kind = kind;
  mr->set_identity = mk;
  mr->marked_names = marked_names;
  mr->shared_pes = scheme_null;
  mr->unmarshal_info = scheme_null;

  return (Scheme_Object *)mr;
}

void scheme_seal_module_rename(Scheme_Object *rn, int level)
{
  ((Module_Renames *)rn)->sealed = level;
}

void scheme_seal_module_rename_set(Scheme_Object *_rns, int level)
{
  Module_Renames_Set *rns = (Module_Renames_Set *)_rns;
  
  rns->sealed = level;
  if (rns->rt)
    rns->rt->sealed = level;
  if (rns->et)
    rns->et->sealed = level;
  if (rns->other_phases) {
    int i;
    for (i = 0; i < rns->other_phases->size; i++) {
      if (rns->other_phases->vals[i]) {
        ((Module_Renames *)rns->other_phases->vals[i])->sealed = level;
      }
    }
  }
}

static void check_not_sealed(Module_Renames *mrn)
{
  if (mrn->sealed >= STX_SEAL_ALL)
    scheme_signal_error("internal error: attempt to change sealed module rename");
}

static Scheme_Object *phase_to_index(Scheme_Object *phase)
{
  return phase;
}

Scheme_Object *scheme_extend_module_rename(Scheme_Object *mrn,
                                           Scheme_Object *modname,     /* actual source module */
                                           Scheme_Object *localname,   /* name in local context */
                                           Scheme_Object *exname,      /* name in definition context  */
                                           Scheme_Object *nominal_mod, /* nominal source module */
                                           Scheme_Object *nominal_ex,  /* nominal import before local renaming */
                                           int mod_phase,              /* phase of source defn */
                                           Scheme_Object *src_phase_index, /* nominal import phase */
                                           Scheme_Object *nom_phase,   /* nominal export phase */
                                           Scheme_Object *insp,        /* inspector for re-export */
                                           int mode)         /* 1 => can be reconstructed from unmarshal info
                                                                2 => free-id=? renaming
                                                                3 => return info */
{
  Scheme_Object *elem;
  Scheme_Object *phase_index;

  if (mode != 3)
    check_not_sealed((Module_Renames *)mrn);

  phase_index = phase_to_index(((Module_Renames *)mrn)->phase);
  if (!src_phase_index)
    src_phase_index = phase_index;
  if (!nom_phase)
    nom_phase = scheme_make_integer(mod_phase);

  if (SAME_OBJ(modname, nominal_mod)
      && SAME_OBJ(exname, nominal_ex)
      && !mod_phase
      && same_phase(src_phase_index, phase_index)
      && same_phase(nom_phase, scheme_make_integer(mod_phase))) {
    if (SAME_OBJ(localname, exname))
      elem = modname;
    else
      elem = CONS(modname, exname);
  } else if (SAME_OBJ(exname, nominal_ex)
	     && SAME_OBJ(localname, exname)
	     && !mod_phase
             && same_phase(src_phase_index, phase_index)
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
      if (same_phase(src_phase_index, phase_index))
        elem = nominal_mod;
      else
        elem = CONS(nominal_mod, src_phase_index);
    } else {
      elem = CONS(nominal_mod, CONS(src_phase_index, nom_phase));
    }
    elem = CONS(exname, CONS(elem, nominal_ex));
    if (mod_phase)
      elem = CONS(scheme_make_integer(mod_phase), elem);
    elem = CONS(modname, elem);
  }

  if (insp)
    elem = CONS(insp, elem);
  
  if (mode == 1) {
    if (!((Module_Renames *)mrn)->nomarshal_ht) {
      Scheme_Hash_Table *ht;
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      ((Module_Renames *)mrn)->nomarshal_ht = ht;
    }
    scheme_hash_set(((Module_Renames *)mrn)->nomarshal_ht, localname, elem);
  } else if (mode == 2) {
    scheme_hash_set(((Module_Renames *)mrn)->free_id_renames, localname, elem);
  } else if (mode == 3) {
    return elem;
  } else
    scheme_hash_set(((Module_Renames *)mrn)->ht, localname, elem);

  return NULL;
}

void scheme_extend_module_rename_with_shared(Scheme_Object *rn, Scheme_Object *modidx, 
                                             Scheme_Module_Phase_Exports *pt, 
                                             Scheme_Object *unmarshal_phase_index,
                                             Scheme_Object *src_phase_index,
                                             Scheme_Object *marks,
                                             int save_unmarshal)
{
  Module_Renames *mrn = (Module_Renames *)rn;
  Scheme_Object *pr, *index_plus_marks;

  check_not_sealed(mrn);

  if (SCHEME_PAIRP(marks))
    index_plus_marks = scheme_make_pair(marks, src_phase_index);
  else
    index_plus_marks = src_phase_index;

  pr = scheme_make_pair(scheme_make_pair(modidx, 
                                         scheme_make_pair((Scheme_Object *)pt,
                                                          index_plus_marks)),
                        mrn->shared_pes);
  mrn->shared_pes = pr;

  if (save_unmarshal) {
    pr = scheme_make_pair(scheme_make_pair(modidx, 
                                           scheme_make_pair(unmarshal_phase_index,
                                                            index_plus_marks)),
                          mrn->unmarshal_info);
    mrn->unmarshal_info = pr;
  }
}

void scheme_save_module_rename_unmarshal(Scheme_Object *rn, Scheme_Object *info)
{
  Scheme_Object *l;

  l = scheme_make_pair(info, ((Module_Renames *)rn)->unmarshal_info);
  ((Module_Renames *)rn)->unmarshal_info = l;
}

static void do_append_module_rename(Scheme_Object *src, Scheme_Object *dest,
				    Scheme_Object *old_midx, Scheme_Object *new_midx,
                                    int do_pes, int do_unm)
{
  Scheme_Hash_Table *ht, *hts, *drop_ht;
  Scheme_Object *v;
  int i, t;

  check_not_sealed((Module_Renames *)dest);

  if (do_pes) {
    if (!SCHEME_NULLP(((Module_Renames *)src)->shared_pes)) {
      Scheme_Object *first = NULL, *last = NULL, *pr, *l;
      for (l = ((Module_Renames *)src)->shared_pes; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
        pr = scheme_make_pair(SCHEME_CAR(l), scheme_null);
        if (last)
          SCHEME_CDR(last) = pr;
        else
          first = pr;
        last = pr;
      }
      SCHEME_CDR(last) = ((Module_Renames *)dest)->shared_pes;
      ((Module_Renames *)dest)->shared_pes = first;
    }
  }

  if (do_unm) {
    if (!SCHEME_NULLP(((Module_Renames *)src)->unmarshal_info)) {
      Scheme_Object *first = NULL, *last = NULL, *pr, *l;
      for (l = ((Module_Renames *)src)->unmarshal_info; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
        pr = scheme_make_pair(SCHEME_CAR(l), scheme_null);
        if (last)
          SCHEME_CDR(last) = pr;
        else
          first = pr;
        last = pr;
      }
      SCHEME_CDR(last) = ((Module_Renames *)dest)->unmarshal_info;
      ((Module_Renames *)dest)->unmarshal_info = first;

      ((Module_Renames *)dest)->needs_unmarshal = 1;
    }
  }

  for (t = 0; t < 2; t++) {
    if (!t) {
      ht = ((Module_Renames *)dest)->ht;
      hts = ((Module_Renames *)src)->ht;
      drop_ht = ((Module_Renames *)dest)->nomarshal_ht;
    } else {
      hts = ((Module_Renames *)src)->nomarshal_ht;
      if (!hts)
	break;
      ht = ((Module_Renames *)dest)->nomarshal_ht;
      if (!ht) {
	ht = scheme_make_hash_table(SCHEME_hash_ptr);
	((Module_Renames *)dest)->nomarshal_ht = ht;
      }
      drop_ht = ((Module_Renames *)dest)->ht;
    }
  
    /* Mappings in src overwrite mappings in dest: */

    for (i = hts->size; i--; ) {
      if (hts->vals[i]) {
	v = hts->vals[i];
	if (old_midx) {
          Scheme_Object *insp = NULL;

          if (SCHEME_PAIRP(v) && is_rename_inspector_info(SCHEME_CAR(v))) {
            insp = SCHEME_CAR(v);
            v = SCHEME_CDR(v);
          } else
            insp = NULL;

	  /* Shift the modidx part */
	  if (SCHEME_PAIRP(v)) {
	    if (SCHEME_PAIRP(SCHEME_CDR(v))) {
	      /* (list* modidx [mod-phase] exportname nominal_modidx+index nominal_exportname) */
	      Scheme_Object *midx1, *midx2;
	      int mod_phase;
	      midx1 = SCHEME_CAR(v);
	      v = SCHEME_CDR(v);
	      if (SCHEME_INTP(SCHEME_CAR(v))) {
		mod_phase = SCHEME_INT_VAL(SCHEME_CAR(v));
		v = SCHEME_CDR(v);
	      } else
		mod_phase = 0;
	      midx2 = SCHEME_CAR(SCHEME_CDR(v));
	      midx1 = scheme_modidx_shift(midx1, old_midx, new_midx);
              if (SCHEME_PAIRP(midx2)) {
                midx2 = scheme_make_pair(scheme_modidx_shift(SCHEME_CAR(midx2), old_midx, new_midx),
                                         SCHEME_CDR(midx2));
              } else {
                midx2 = scheme_modidx_shift(midx2, old_midx, new_midx);
              }
	      v = CONS(SCHEME_CAR(v), CONS(midx2, SCHEME_CDR(SCHEME_CDR(v))));
	      if (mod_phase)
		v = CONS(scheme_make_integer(mod_phase), v);
	      v = CONS(midx1, v);
	    } else if (nom_mod_p(v)) {
	      /* (cons modidx nominal_modidx) */
	      v = ICONS(scheme_modidx_shift(SCHEME_CAR(v), old_midx, new_midx),
			scheme_modidx_shift(SCHEME_CDR(v), old_midx, new_midx));
	    } else {
	      /* (cons modidx exportname) */
	      v = CONS(scheme_modidx_shift(SCHEME_CAR(v), old_midx, new_midx),
		       SCHEME_CDR(v));
	    }
	  } else {
	    /* modidx */
	    v = scheme_modidx_shift(v, old_midx, new_midx);
	  }

          if (insp)
            v = CONS(insp, v);
	}
	scheme_hash_set(ht, hts->keys[i], v);
	if (drop_ht)
	  scheme_hash_set(drop_ht, hts->keys[i], NULL);
      }
    }
  }

  /* Need to share marked names: */

  if (((Module_Renames *)src)->marked_names) {
    ((Module_Renames *)dest)->marked_names = ((Module_Renames *)src)->marked_names;
  }
}

void scheme_append_module_rename(Scheme_Object *src, Scheme_Object *dest, int do_unm)
{
  do_append_module_rename(src, dest, NULL, NULL, 1, do_unm);
}

void scheme_append_rename_set_to_env(Scheme_Object *_mrns, Scheme_Env *env)
{
  Module_Renames_Set *mrns = (Module_Renames_Set *)_mrns;
  Scheme_Object *mrns2;
  int i;

  scheme_prepare_env_renames(env, mzMOD_RENAME_TOPLEVEL);
  mrns2 = env->rename_set;

  if (mrns->rt) {
    scheme_append_module_rename((Scheme_Object *)mrns->rt, 
                                scheme_get_module_rename_from_set(mrns2, scheme_make_integer(0), 1),
                                1);
  }
  if (mrns->et) {
    scheme_append_module_rename((Scheme_Object *)mrns->et, 
                                scheme_get_module_rename_from_set(mrns2, scheme_make_integer(1), 1),
                                1);
  }
  if (mrns->other_phases) {
    for (i = 0; i < mrns->other_phases->size; i++) {
      if (mrns->other_phases->vals[i]) {
        scheme_append_module_rename(mrns->other_phases->vals[i],
                                    scheme_get_module_rename_from_set(mrns2, 
                                                                      mrns->other_phases->keys[i],
                                                                      1),
                                    1);
      }
    }
  }
}

void scheme_remove_module_rename(Scheme_Object *mrn,
				 Scheme_Object *localname)
{
  check_not_sealed((Module_Renames *)mrn);
  scheme_hash_set(((Module_Renames *)mrn)->ht, localname, NULL);
  if (((Module_Renames *)mrn)->nomarshal_ht)
    scheme_hash_set(((Module_Renames *)mrn)->nomarshal_ht, localname, NULL);
  if (((Module_Renames *)mrn)->free_id_renames)
    scheme_hash_set(((Module_Renames *)mrn)->free_id_renames, localname, NULL);
}

void scheme_list_module_rename(Scheme_Object *set, Scheme_Hash_Table *ht,
                               Scheme_Hash_Table *export_registry)
{
  /* Put every name mapped by src into ht: */
  Scheme_Object *pr;
  Scheme_Hash_Table *hts;
  int i, t;
  Scheme_Module_Phase_Exports *pt;
  Module_Renames *src;

  if (SCHEME_RENAMES_SETP(set))
    src = ((Module_Renames_Set *)set)->rt;
  else
    src = (Module_Renames *)set;

  if (!src)
    return;
  
  if (src->needs_unmarshal) {
    unmarshal_rename(src, NULL, NULL, export_registry);
  }

  for (t = 0; t < 2; t++) {
    if (!t)
      hts = src->ht;
    else {
      hts = src->nomarshal_ht;
    }

    if (hts) {
      for (i = hts->size; i--; ) {
        if (hts->vals[i]) {
          scheme_hash_set(ht, hts->keys[i], scheme_false);
        }
      }
    }
  }

  for (pr = src->shared_pes; !SCHEME_NULLP(pr); pr = SCHEME_CDR(pr)) {
    pt = (Scheme_Module_Phase_Exports *)SCHEME_CADR(SCHEME_CAR(pr));
    for (i = pt->num_provides; i--; ) {
      scheme_hash_set(ht, pt->provides[i], scheme_false);
    }
  }
}


Scheme_Object *scheme_rename_to_stx(Scheme_Object *mrn)
{
  Scheme_Object *stx;
  stx = scheme_make_stx(scheme_false, empty_srcloc, NULL); 
  return scheme_add_rename(stx, mrn);
}

Scheme_Object *scheme_stx_to_rename(Scheme_Object *stx)
{
  Scheme_Object *rns = NULL, *v;
  WRAP_POS wl;
  
  WRAP_POS_INIT(wl, ((Scheme_Stx *)stx)->wraps);
  while (!WRAP_POS_END_P(wl)) {
    v = WRAP_POS_FIRST(wl);
    if (SCHEME_RENAMES_SETP(v)) {
      if (rns)
        scheme_signal_error("can't convert syntax to rename (two sets)");
      rns = v;
    } else if (SCHEME_RENAMESP(v)) {
      if (!rns)
        rns = scheme_make_module_rename_set(((Module_Renames *)v)->kind, NULL);
      scheme_add_module_rename_to_set(rns, v);
    } else {
      scheme_signal_error("can't convert syntax to rename (non-rename in wrap)");
    }
    WRAP_POS_INC(wl);
  }

  if (!rns)
    scheme_signal_error("can't convert syntax to rename (empty)");

  return rns;
}

Scheme_Object *scheme_stx_shift_rename(Scheme_Object *mrn, 
				       Scheme_Object *old_midx, Scheme_Object *new_midx)
{
  Scheme_Object *nmrn, *a, *l, *nl, *first, *last;

  nmrn = scheme_make_module_rename(((Module_Renames *)mrn)->phase, 
                                   mzMOD_RENAME_NORMAL, 
                                   NULL);

  /* use "append" to copy most info: */
  do_append_module_rename(mrn, nmrn, old_midx, new_midx, 0, 0);

  /* Manually copy unmarshal_infos, where we have to shift anyway: */

  l = ((Module_Renames *)mrn)->unmarshal_info;
  first = scheme_null;
  last = NULL;
  while (!SCHEME_NULLP(l)) {
    a = SCHEME_CAR(l);
    nl = scheme_make_pair(scheme_make_pair(scheme_modidx_shift(SCHEME_CAR(a), old_midx, new_midx),
					   SCHEME_CDR(a)),
			  scheme_null);
    if (last)
      SCHEME_CDR(last) = nl;
    else
      first = nl;
    last = nl;
    l = SCHEME_CDR(l);
  }
  ((Module_Renames *)nmrn)->unmarshal_info = first;

  l = ((Module_Renames *)mrn)->shared_pes;
  first = scheme_null;
  last = NULL;
  while (!SCHEME_NULLP(l)) {
    a = SCHEME_CAR(l);
    nl = scheme_make_pair(scheme_make_pair(scheme_modidx_shift(SCHEME_CAR(a), old_midx, new_midx),
					   SCHEME_CDR(a)),
			  scheme_null);
    if (last)
      SCHEME_CDR(last) = nl;
    else
      first = nl;
    last = nl;
    l = SCHEME_CDR(l);
  }
  ((Module_Renames *)nmrn)->shared_pes = first;

  if (((Module_Renames *)mrn)->needs_unmarshal) {
    ((Module_Renames *)nmrn)->needs_unmarshal = 1;
  }  

  return nmrn;
}

Scheme_Object *scheme_stx_shift_rename_set(Scheme_Object *_mrns, 
                                           Scheme_Object *old_midx, Scheme_Object *new_midx)
{
  Module_Renames_Set *mrns = (Module_Renames_Set *)_mrns;
  Scheme_Object *mrn, *mrns2;
  int i;

  mrns2 = scheme_make_module_rename_set(mrns->kind, NULL);
  if (mrns->rt) {
    mrn = scheme_stx_shift_rename((Scheme_Object *)mrns->rt, old_midx, new_midx);
    scheme_add_module_rename_to_set(mrns2, mrn);
  }
  if (mrns->et) {
    mrn = scheme_stx_shift_rename((Scheme_Object *)mrns->et, old_midx, new_midx);
    scheme_add_module_rename_to_set(mrns2, mrn);
  }
  if (mrns->other_phases) {
    for (i = 0; i < mrns->other_phases->size; i++) {
      if (mrns->other_phases->vals[i]) {
        mrn = scheme_stx_shift_rename(mrns->other_phases->vals[i], old_midx, new_midx);
        scheme_add_module_rename_to_set(mrns2, mrn);
      }
    }
  }

  return (Scheme_Object *)mrns2;
}


Scheme_Hash_Table *scheme_module_rename_marked_names(Scheme_Object *rn)
{
  return ((Module_Renames *)rn)->marked_names;
}

static void unmarshal_rename(Module_Renames *mrn,
			     Scheme_Object *modidx_shift_from, Scheme_Object *modidx_shift_to,
			     Scheme_Hash_Table *export_registry)
{
  Scheme_Object *l;
  int sealed;

  mrn->needs_unmarshal = 0;

  sealed = mrn->sealed;
  if (sealed)
    mrn->sealed = 0;
    
  l = scheme_reverse(mrn->unmarshal_info);
  for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    scheme_do_module_rename_unmarshal((Scheme_Object *)mrn, SCHEME_CAR(l),
				      modidx_shift_from, modidx_shift_to,
				      export_registry);
  }

  if (sealed)
    mrn->sealed = sealed;
}

/******************** wrap manipulations ********************/

Scheme_Object *scheme_add_rename(Scheme_Object *o, Scheme_Object *rename)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *wraps;
  Scheme_Object *certs;
  long lp;

  if (STX_KEY(stx) & STX_SUBSTX_FLAG)
    preemptive_chunk(stx);

  /* relative order matters: chunk first, so that chunking
     doesn't immediately throw away a chain cache */

  maybe_add_chain_cache(stx);

  wraps = CONS(rename, stx->wraps);
  if (STX_KEY(stx) & STX_SUBSTX_FLAG)
    lp = stx->u.lazy_prefix + 1;
  else
    lp = 0;

  certs = stx->certs;
  stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
  stx->wraps = wraps;
  stx->certs = certs;

  stx->u.lazy_prefix = lp; /* same as zeroing cache if no SUBSTX */

  if (stx->certs)
    phase_shift_certs((Scheme_Object *)stx, stx->wraps, 1);
  
  return (Scheme_Object *)stx;
}

void scheme_load_delayed_syntax(struct Resolve_Prefix *rp, long i)
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

Scheme_Object *scheme_delayed_rename(Scheme_Object **o, long i)
{
  Scheme_Object *rename;
  Resolve_Prefix *rp;

  rename = o[0];

  if (!rename) return scheme_false; /* happens only with corrupted .zo! */

  rp = (Resolve_Prefix *)o[1];

  if (SCHEME_INTP(rp->stxes[i]))
    scheme_load_delayed_syntax(rp, i);

  return scheme_add_rename(rp->stxes[i], rename);
}

Scheme_Object *scheme_add_rename_rib(Scheme_Object *o, Scheme_Object *rib)
{
#if 0
  WRAP_POS wl;

  /* Shortcut: there's a good chance that o already has the renaming rib */
  WRAP_POS_INIT(wl, ((Scheme_Stx *)o)->wraps);
  if (!WRAP_POS_END_P(wl)) {
    if (SAME_OBJ(rib, WRAP_POS_FIRST(wl))) {
      return o;
    }
  }
#endif

  return scheme_add_rename(o, rib);
}

Scheme_Object *scheme_add_rib_delimiter(Scheme_Object *o, Scheme_Object *ribs)
{
  Scheme_Object *s;

  s = scheme_alloc_small_object();
  s->type = scheme_rib_delimiter_type;
  SCHEME_BOX_VAL(s) = ribs;

  return scheme_add_rename(o, s);
}

static int is_in_rib_delim(Scheme_Object *envname, Scheme_Object *rib_delim)
{
  Scheme_Object *l = SCHEME_BOX_VAL(rib_delim);
  Scheme_Lexical_Rib *rib;

  while (!SCHEME_NULLP(l)) {
    rib = (Scheme_Lexical_Rib *)SCHEME_CAR(l);
    while (rib) {
      if (rib->rename && SAME_OBJ(envname, SCHEME_VEC_ELS(rib->rename)[0]))
        return 1;
      rib = rib->next;
    }
    l = SCHEME_CDR(l);
  }
  return 0;
}

static Scheme_Hash_Table *make_recur_table()
{
  if (quick_hash_table) {
    GC_CAN_IGNORE Scheme_Hash_Table *t;
    t = quick_hash_table;
    quick_hash_table = NULL;
    return t;
  } else
    return scheme_make_hash_table(SCHEME_hash_ptr);
}

static void release_recur_table(Scheme_Hash_Table *free_id_recur)
{
  if (!free_id_recur->size && !quick_hash_table) {
    quick_hash_table = free_id_recur;
  }
}

static Scheme_Object *extract_module_free_id_binding(Scheme_Object *mrn,
                                                     Scheme_Object *id, 
                                                     Scheme_Object *orig_id,
                                                     int *_sealed,
                                                     Scheme_Hash_Table *free_id_recur)
{
  Scheme_Object *result;
  Scheme_Object *modname;
  Scheme_Object *nominal_modidx;
  Scheme_Object *nominal_name, *nom2;
  Scheme_Object *mod_phase;
  Scheme_Object *src_phase_index;
  Scheme_Object *nominal_src_phase;
  Scheme_Object *lex_env;
  Scheme_Object *rename_insp;

  if (scheme_hash_get(free_id_recur, id)) {
    return id;
  }
  scheme_hash_set(free_id_recur, id, id);
  
  nom2 = scheme_stx_property(orig_id, nominal_id_symbol, NULL);

  modname = scheme_stx_module_name(free_id_recur,
                                   &orig_id, ((Module_Renames *)mrn)->phase, &nominal_modidx,
                                   &nominal_name,
                                   &mod_phase, 
                                   &src_phase_index,
                                   &nominal_src_phase,
                                   &lex_env,
                                   _sealed,
                                   &rename_insp);
 
  if (SCHEME_SYMBOLP(nom2))
    nominal_name = nom2;
  
  if (!modname)
    result = scheme_box(CONS(SCHEME_STX_VAL(orig_id), scheme_false));
  else if (SAME_OBJ(modname, scheme_undefined))
    result = scheme_box(CONS(SCHEME_STX_VAL(orig_id), lex_env));
  else
    result = scheme_extend_module_rename(mrn,
                                         modname,
                                         id,                 /* name in local context */
                                         orig_id,            /* name in definition context  */
                                         nominal_modidx,     /* nominal source module */
                                         nominal_name,       /* nominal import before local renaming */
                                         SCHEME_INT_VAL(mod_phase), /* phase of source defn */
                                         src_phase_index,    /* nominal import phase */
                                         nominal_src_phase,  /* nominal export phase */
                                         rename_insp,
                                         3);

  if (*_sealed) {
    /* cache the result */
    scheme_hash_set(((Module_Renames *)mrn)->free_id_renames, id, result);
  }

  return result;
}

void scheme_install_free_id_rename(Scheme_Object *id, 
                                   Scheme_Object *orig_id,
                                   Scheme_Object *rename_rib,
                                   Scheme_Object *phase)
{
  Scheme_Object *v = NULL, *env, *r_id;
  Scheme_Lexical_Rib *rib = NULL;

  if (rename_rib && (SCHEME_RENAMESP(rename_rib) || SCHEME_RENAMES_SETP(rename_rib))) {
    /* Install a Module_Rename-level free-id=? rename, instead of at
       the level of a lexical-rename. In this case, id is a symbol instead
       of an identifier. */
    Module_Renames *rn;

    if (SCHEME_RENAMES_SETP(rename_rib))
      rename_rib = scheme_get_module_rename_from_set(rename_rib, phase, 1);
    rn = (Module_Renames *)rename_rib;

    if (!rn->free_id_renames) {
      Scheme_Hash_Table *ht;
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      rn->free_id_renames = ht;
    }

    scheme_hash_set(rn->free_id_renames, id, orig_id);

    return;
  }

  env = scheme_stx_moduleless_env(id);

  if (rename_rib) {
    rib = (Scheme_Lexical_Rib *)rename_rib;
  } else {
    WRAP_POS wl;
    
    WRAP_POS_INIT(wl, ((Scheme_Stx *)id)->wraps);
    while (!WRAP_POS_END_P(wl)) {
      v = WRAP_POS_FIRST(wl);
      if (SCHEME_VECTORP(v) && SAME_OBJ(SCHEME_VEC_ELS(v)[0], env)) {
        break;
      } if (SCHEME_RIBP(v)) {
        rib = (Scheme_Lexical_Rib *)v;
        while (rib) {
          if (rib->rename) {
            v = rib->rename;
            if (SCHEME_VECTORP(v) && SAME_OBJ(SCHEME_VEC_ELS(v)[0], env))
              break;
            v = NULL;
          }
          rib = rib->next;
        }
      } else
        v = NULL;
      WRAP_POS_INC(wl);
    }
  }

  while (v || rib) {
    if (!v) {
      while (rib) {
        if (rib->rename) {
          v = rib->rename;
          if (SCHEME_VECTORP(v) && SAME_OBJ(SCHEME_VEC_ELS(v)[0], env))
            break;
          v = NULL;
        }
        rib = rib->next;
      }
    }
    
    if (v) {
      int i, sz;
    
      sz = SCHEME_RENAME_LEN(v);
      for (i = 0; i < sz; i++) {
        r_id = SCHEME_VEC_ELS(v)[i+2];
        if (SAME_OBJ(SCHEME_STX_SYM(r_id), SCHEME_STX_VAL(id))) {
          /* Install rename: */
          env = SCHEME_VEC_ELS(v)[i+sz+2];
          if (SCHEME_PAIRP(env)) env = SCHEME_CAR(env);
          env = CONS(env, CONS(orig_id, phase));
          SCHEME_VEC_ELS(v)[i+sz+2] = env;
          return;
        }
      }
    }

    v = NULL;
    if (rib) rib = rib->next;
  }
}

Scheme_Object *scheme_stx_phase_shift_as_rename(long shift, Scheme_Object *old_midx, Scheme_Object *new_midx,
						Scheme_Hash_Table *export_registry)
{
  if (shift || new_midx || export_registry) {
    Scheme_Object *vec;
    
    if (last_phase_shift
	&& ((vec = SCHEME_BOX_VAL(last_phase_shift)))
	&& (SCHEME_VEC_ELS(vec)[0] == scheme_make_integer(shift))
	&& (SCHEME_VEC_ELS(vec)[1] == (new_midx ? old_midx : scheme_false))
	&& (SCHEME_VEC_ELS(vec)[2] == (new_midx ? new_midx : scheme_false))
	&& (SCHEME_VEC_ELS(vec)[3] == (export_registry ? (Scheme_Object *)export_registry : scheme_false))) {
      /* use the old one */
    } else {
      vec = scheme_make_vector(4, NULL);
      SCHEME_VEC_ELS(vec)[0] = scheme_make_integer(shift);
      SCHEME_VEC_ELS(vec)[1] = (new_midx ? old_midx : scheme_false);
      SCHEME_VEC_ELS(vec)[2] = (new_midx ? new_midx : scheme_false);
      SCHEME_VEC_ELS(vec)[3] = (export_registry ? (Scheme_Object *)export_registry : scheme_false);

      last_phase_shift = scheme_box(vec);
    }
    
    return last_phase_shift;
  } else
    return NULL;
}

Scheme_Object *scheme_stx_phase_shift(Scheme_Object *stx, long shift,
				      Scheme_Object *old_midx, Scheme_Object *new_midx,
				      Scheme_Hash_Table *export_registry)
/* Shifts the phase on a syntax object in a module. A 0 shift might be
   used just to re-direct relative module paths. new_midx might be
   NULL to shift without redirection. And so on. */
{
  Scheme_Object *ps;

  ps = scheme_stx_phase_shift_as_rename(shift, old_midx, new_midx, export_registry);
  if (ps)
    return scheme_add_rename(stx, ps);  
  else
    return stx;
}

void scheme_clear_shift_cache(void)
{
  last_phase_shift = NULL;
}

static void phase_shift_certs(Scheme_Object *o, Scheme_Object *owner_wraps, int len)
     /* Mutates o to change its certs, in the case that the first len
	elements of owner_wraps includes any phase-shifting (i.e.,
	modidx-shifting) elements. */
{
  Scheme_Object *l, *a, *modidx_shift_to = NULL, *modidx_shift_from = NULL, *vec, *src, *dest;
  int i, j, cnt;

  for (i = 0, l = owner_wraps; i < len; i++, l = SCHEME_CDR(l)) {
    a = SCHEME_CAR(l);
    if (SAME_TYPE(SCHEME_TYPE(a), scheme_wrap_chunk_type)) {
      cnt = ((Wrap_Chunk *)a)->len;
      for (j = 0; j < cnt; j++) {
	if (SCHEME_BOXP(((Wrap_Chunk *)a)->a[j])) {
	  vec = SCHEME_BOX_VAL(((Wrap_Chunk *)a)->a[j]);
	  src = SCHEME_VEC_ELS(vec)[1];
	  dest = SCHEME_VEC_ELS(vec)[2];
	  if (!modidx_shift_to) {
	    modidx_shift_to = dest;
	  } else if (!SAME_OBJ(modidx_shift_from, dest)) {
	    modidx_shift_to = scheme_modidx_shift(dest,
						  modidx_shift_from,
						  modidx_shift_to);
	  }
	  modidx_shift_from = src;
	}
      }
    } else if (SCHEME_BOXP(a)) {
      vec = SCHEME_BOX_VAL(a);
      src = SCHEME_VEC_ELS(vec)[1];
      dest = SCHEME_VEC_ELS(vec)[2];
      if (!modidx_shift_to) {
	modidx_shift_to = dest;
      } else if (!SAME_OBJ(modidx_shift_from, dest)) {
	modidx_shift_to = scheme_modidx_shift(dest,
					      modidx_shift_from,
					      modidx_shift_to);
      }
      modidx_shift_from = src;
    }
  }

  if (modidx_shift_from) {
    Scheme_Cert *certs, *acerts, *icerts, *first = NULL, *last = NULL, *c;
    Scheme_Object *nc;
    int i;

    acerts = ACTIVE_CERTS(((Scheme_Stx *)o));
    icerts = INACTIVE_CERTS(((Scheme_Stx *)o));
    
    /* Clone certs list, phase-shifting each cert */
    for (i = 0; i < 2; i++) {
      certs = (i ? acerts : icerts);
      first = last = NULL;
      while (certs) {
	a = scheme_modidx_shift(certs->modidx, modidx_shift_from, modidx_shift_to);
	c = cons_cert(certs->mark, a, certs->insp, certs->key, NULL);
	c->mapped = certs->mapped;
	c->depth = certs->depth;
	if (first)
	  last->next = c;
	else
	  first = c;
	last = c;
	certs = certs->next;
      }
      if (i)
	acerts = first;
      else
	icerts = first;
    }

    /* Even if icerts is NULL, may preserve the pair in ->certs, 
       to indicate no nested inactive certs: */
    {
      int no_ia_sub = (SCHEME_RPAIRP(((Scheme_Stx *)o)->certs)
                       && SCHEME_NO_INACTIVE_SUBS_P(((Scheme_Stx *)o)->certs));
      int no_a_sub = (SCHEME_RPAIRP(((Scheme_Stx *)o)->certs)
                      && SCHEME_NO_ACTIVE_SUBS_P(((Scheme_Stx *)o)->certs));
      if (icerts || no_ia_sub || no_a_sub) {
        nc = scheme_make_raw_pair((Scheme_Object *)acerts, (Scheme_Object *)icerts);
        if (no_ia_sub)
          SCHEME_SET_NO_INACTIVE_SUBS(nc);
        if (no_a_sub)
          SCHEME_SET_NO_ACTIVE_SUBS(nc);
      } else
        nc = (Scheme_Object *)acerts;
      
      ((Scheme_Stx *)o)->certs = nc;
    }
  }
}

static Scheme_Object *make_chunk(int len, Scheme_Object *owner_wraps)
/* Result is a single wrap element (possibly a chunk) or a list
   of elements in reverse order. */
{
  Wrap_Chunk *wc;
  Scheme_Object *l, *a, *max_chunk_start_list = NULL, *ml;
  int i, count = 0, j, max_chunk_size = 0, max_chunk_start_pos = 0;

  if (len > 1) {
    for (i = 0, l = owner_wraps; i < len; i++, l = SCHEME_CDR(l)) {
      a = SCHEME_CAR(l);
      if (SAME_TYPE(SCHEME_TYPE(a), scheme_wrap_chunk_type)) {
	j = ((Wrap_Chunk *)a)->len;
	if (j > max_chunk_size) {
	  max_chunk_start_list = l;
	  max_chunk_start_pos = i;
	  max_chunk_size = j;
	}
	count += j;
      } else if (SCHEME_NUMBERP(a)) {
	if ((i >= len-1) || !SAME_OBJ(a, SCHEME_CADR(l)))
	  count++;
	else {
	  /* Skip canceling marks */
	  i++;
	  l = SCHEME_CDR(l);
	}
      } else if (SCHEME_HASHTP(a)) {
	/* Don't propagate chain-specific table */
      } else
	count++;
    }

    if ((max_chunk_size > 8) && ((max_chunk_size * 2) > count)) {
      /* It's not worth copying a big existing chunk into
	 a new chunk. First copy over the part before new chunk,
	 then the new chunk, and finally the rest. */
      Scheme_Object *ml2;
      if (max_chunk_start_pos) {
	ml = make_chunk(max_chunk_start_pos, owner_wraps);
	if (!SCHEME_PAIRP(ml) && !SCHEME_NULLP(ml))
	  ml = scheme_make_pair(ml, scheme_null);
      } else
	ml = scheme_null;
      ml = scheme_make_pair(SCHEME_CAR(max_chunk_start_list), ml);
      if (max_chunk_start_pos + 1 < len) {
	ml2 = make_chunk(len - 1 - max_chunk_start_pos, 
			 SCHEME_CDR(max_chunk_start_list));
	if (!SCHEME_NULLP(ml2)) {
	  if (SCHEME_PAIRP(ml2))
	    ml = scheme_append(ml2, ml);
	  else
	    ml = scheme_make_pair(ml2, ml);
	}
      }
    } else {
      if (!count) {
	ml = scheme_null; /* everything disappeared! */
      } else {
	wc = MALLOC_WRAP_CHUNK(count);
	wc->type = scheme_wrap_chunk_type;
	wc->len = count;
	
	ml = NULL; /* to make compiler happy */

	j = 0;
	for (i = 0, l = owner_wraps; i < len; i++, l = SCHEME_CDR(l)) {
	  a = SCHEME_CAR(l);
	  if (SAME_TYPE(SCHEME_TYPE(a), scheme_wrap_chunk_type)) {
	    int k, cl = ((Wrap_Chunk *)a)->len;
	    for (k = 0; k < cl; k++) {
	      wc->a[j++] = ((Wrap_Chunk *)a)->a[k];
	    }
	  }  else if (SCHEME_NUMBERP(a)) {
	    if ((i >= len-1) || !SAME_OBJ(a, SCHEME_CADR(l)))
	      wc->a[j++] = a;
	    else {
	      /* Skip canceling marks */
	      i++;
	      l= SCHEME_CDR(l);
	    }
	  } else if (SCHEME_HASHTP(a)) {
	    /* Skip chain-specific table */
	  } else
	    wc->a[j++] = a;
	}

	if (count == 1) /* in case mark removal left only one */
	  ml = wc->a[0];
	else
	  ml = (Scheme_Object *)wc;
      }
    }
  } else {
    ml = SCHEME_CAR(owner_wraps);
    if (SCHEME_HASHTP(ml))
      return scheme_null;
  }

  return ml;
}

#define PREEMPTIVE_CHUNK_THRESHOLD 32

static void preemptive_chunk(Scheme_Stx *stx)
{
  int wl_count;
  int new_count;
  Scheme_Object *here_wraps, *ml;

  /* If the lazy prefix is long, transform it into a chunk. Probably,
     some syntax object derived from this one will be unpacked, and
     then the lazy prefix will need to be pushed down.

     This chunking fights somewhat with the chain-cache heuristic,
     since a chain cache can't be included in a chunk. Still, the
     combination seems to work better than either alone for deeply
     nested scopes.

     It might also interact badly with simplication or marshaling,
     since it decreases chain sharing. This is seems unlikely to
     matter, since deeply nested syntax information will be expensive
     in any case, and nodes in the wraps are still shared. */

  wl_count = stx->u.lazy_prefix;

  if (wl_count > PREEMPTIVE_CHUNK_THRESHOLD) {
    /* Chunk it */
    here_wraps = stx->wraps;

    ml = make_chunk(wl_count, here_wraps);
    
    if (SCHEME_PAIRP(ml) || SCHEME_NULLP(ml)) {
      new_count = scheme_list_length(ml);
      if (new_count == 1)
	ml = SCHEME_CAR(ml);
    } else {
      new_count = 1;
    }

    while (wl_count--) {
      here_wraps = SCHEME_CDR(here_wraps);
    }
    wl_count = new_count;

    if (new_count == 1)
      here_wraps = scheme_make_pair(ml, here_wraps);
    else {
      while (new_count--) {
	here_wraps = scheme_make_pair(SCHEME_CAR(ml), here_wraps);
	ml = SCHEME_CDR(ml);
      }
    }

    stx->wraps = here_wraps;
    stx->u.lazy_prefix = wl_count;
  }
}

static Scheme_Object *propagate_wraps(Scheme_Object *o, 
				      int len, Scheme_Object **_ml,
				      Scheme_Object *owner_wraps)
{
  int i;
  Scheme_Object *ml, *a;

  /* Would adding the wraps generate a list equivalent to owner_wraps?
     If so, use owner_wraps directly. But if len is too big, then it
     takes too long to check, and so it's better to start chunking. */
  if (len < 128) {
    Scheme_Stx *stx = (Scheme_Stx *)o;
    Scheme_Object *p1 = owner_wraps;
    Scheme_Object *certs;

    /* Find list after |wl| items in owner_wraps: */
    for (i = 0; i < len; i++) {
      p1 = SCHEME_CDR(p1);
    }
    /* p1 is the list after wl... */
    
    if (SAME_OBJ(stx->wraps, p1)) {
      /* So, we can use owner_wraps directly instead of building
	 new wraps. */
      long lp;

      if (STX_KEY(stx) & STX_SUBSTX_FLAG)
	lp = stx->u.lazy_prefix + len;
      else
	lp = 0;

      certs = stx->certs;
      stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
      stx->wraps = owner_wraps;
      stx->u.lazy_prefix = lp; /* same as zeroing cache if no SUBSTX */
      stx->certs = certs;

      if (stx->certs)
	phase_shift_certs((Scheme_Object *)stx, owner_wraps, len);

      return (Scheme_Object *)stx;
    }
  }

  ml = *_ml;
  if (!ml) {
    ml = make_chunk(len, owner_wraps);
    *_ml = ml;
  }

  if (SCHEME_PAIRP(ml)) {
    while (SCHEME_PAIRP(ml)) {
      a = SCHEME_CAR(ml);
      if (SCHEME_NUMBERP(a)) {
	o = scheme_add_remove_mark(o, a);
      } else {
	o = scheme_add_rename(o, a);
      }
      ml = SCHEME_CDR(ml);
    }
  } else if (SCHEME_NUMBERP(ml))
    o = scheme_add_remove_mark(o, ml);
  else if (SCHEME_NULLP(ml)) {
    /* nothing to add */
  } else
    o = scheme_add_rename(o, ml);

  if (((Scheme_Stx *)o)->certs)
    phase_shift_certs(o, owner_wraps, len);

  return o;
}

int scheme_stx_certified(Scheme_Object *stx, Scheme_Object *extra_certs, 
			 Scheme_Object *home_modidx, Scheme_Object *home_insp)
{
  Scheme_Cert *certs = ACTIVE_CERTS((Scheme_Stx *)stx);
  Scheme_Object *cert_modidx, *a, *b;

  do {
    while (certs) {
      if (!scheme_module_protected_wrt(home_insp, certs->insp)) {
	if (home_modidx) {
	  if (SCHEME_FALSEP(certs->modidx))
	    cert_modidx = home_modidx;
	  else
	    cert_modidx = certs->modidx;
	  
	  a = scheme_module_resolve(home_modidx, 0);
	  b = scheme_module_resolve(cert_modidx, 0);
	} else
	  a = b = NULL;
	
	if (SAME_OBJ(a, b)) {
	  /* Found a certification. Does this identifier have the
	     associated mark? */
	  if (includes_mark(((Scheme_Stx *)stx)->wraps, certs->mark))
	    return 1;
	}
      }
      certs = certs->next;
    }
    if (extra_certs) {
      certs = (Scheme_Cert *)extra_certs;
      extra_certs = NULL;
    }
  } while (certs);

  return 0;
}

static Scheme_Cert *cons_cert(Scheme_Object *mark, Scheme_Object *modidx, 
			      Scheme_Object *insp, Scheme_Object *key, 
			      Scheme_Cert *next_cert)
{
  Scheme_Cert *cert;

  cert = MALLOC_ONE_RT(Scheme_Cert);
  cert->iso.so.type = scheme_certifications_type;
  cert->mark = mark;
  cert->modidx = modidx;
  cert->insp = insp;
  cert->key = key;
  cert->next = next_cert;
  cert->depth = (next_cert ? next_cert->depth + 1 : 1);

  if (!key && (!next_cert || CERT_NO_KEY(next_cert))) {
    CERT_SET_NO_KEY(cert);
  }

  return cert;
}

#ifdef DO_STACK_CHECK
static void make_mapped(Scheme_Cert *cert);
static Scheme_Object *make_mapped_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Cert *cert = (Scheme_Cert *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  make_mapped(cert);

  return scheme_void;
}
#endif

static void make_mapped(Scheme_Cert *cert)
{
  Scheme_Cert *stop, *c2;
  Scheme_Object *pr;
  Scheme_Hash_Table *ht;

  if (cert->mapped)
    return;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)cert;
      scheme_handle_stack_overflow(make_mapped_k);
      return;
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  if (cert->depth == 16) {
    stop = NULL;
  } else {
    for (stop = cert->next; 
	 stop && ((stop->depth & cert->depth) != stop->depth); 
	 stop = stop->next) {
    }
    if (stop)
      make_mapped(stop);
  }

  /* Check whether an `eq?' table will work: */
  for (c2 = cert; c2 != stop; c2 = c2->next) {
    if (c2->key)
      break;
    if (!SCHEME_INTP(c2->mark))
      break;
  }

  if (c2 == stop)
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
  else
    ht = scheme_make_hash_table_equal();

  pr = scheme_make_raw_pair((Scheme_Object *)ht, (Scheme_Object *)stop);
  cert->mapped = pr;

  for (; cert != stop; cert = cert->next) {
    if (cert->key)
      pr = scheme_make_pair(cert->mark, cert->key);
    else
      pr = cert->mark;
    scheme_hash_set_atomic(ht, pr, scheme_true);
  }
}

static int cert_in_chain(Scheme_Object *mark, Scheme_Object *key, Scheme_Cert *cert)
{
  Scheme_Object *hkey = key ? NULL : mark;
  Scheme_Hash_Table *ht;

  while (cert) {
    if (!(cert->depth & 0xF)) {
      make_mapped(cert);

      ht = (Scheme_Hash_Table *)SCHEME_CAR(cert->mapped);
      cert = (Scheme_Cert *)SCHEME_CDR(cert->mapped);

      if (!hkey)
	hkey = scheme_make_pair(mark, key);

      if (scheme_hash_get_atomic(ht, hkey))
	return 1;
    } else if (SAME_OBJ(cert->mark, mark)
	       && SAME_OBJ(cert->key, key)) {
      return 1;
    } else
      cert = cert->next;
  }

  return 0;
}

static Scheme_Cert *append_certs(Scheme_Cert *a, Scheme_Cert *b)
{
  Scheme_Cert *c;

  if (!a) return b;
  if (!b) return a;
  
  if (a->depth < b->depth) {
    c = a;
    a = b;
    b = c;
  }

  c = a;
  if (b->depth > (a->depth >> 1)) {
    /* There's a good chance that b shares a tail with a, 
       so check for that, and b is large enough relative to
       a that it's worth iterating down to b's depth in a: */
    while (c->depth > b->depth) {
      c = c->next;
    }
  }

  for (; b; b = b->next) {
    if (b == c) break;
    if (!cert_in_chain(b->mark, b->key, a))
      a = cons_cert(b->mark, b->modidx, b->insp, b->key, a);
    c = c->next;
  }

  return a;
}

static Scheme_Object *add_certs(Scheme_Object *o, Scheme_Cert *certs, Scheme_Object *use_key, int active)
{
  Scheme_Cert *orig_certs, *cl, *now_certs, *next_certs, *check_tail;
  Scheme_Stx *stx = (Scheme_Stx *)o, *res;
  Scheme_Object *pr;
  int shortcut;

  if (!stx->certs) {
    if (!certs)
      return (Scheme_Object *)stx;

    if (use_key) {
      for (cl = certs; cl; cl = cl->next) {
	if (!SAME_OBJ(cl->key, use_key))
	  break;
      }
    } else
      cl = NULL;

    if (!cl) {
      res = (Scheme_Stx *)scheme_make_stx(stx->val, 
					  stx->srcloc,
					  stx->props);
      res->wraps = stx->wraps;
      res->u.lazy_prefix = stx->u.lazy_prefix;
      if (active)
	res->certs = (Scheme_Object *)certs;
      else {
	pr = scheme_make_raw_pair(NULL, (Scheme_Object *)certs);
	res->certs = pr;
      }
      return (Scheme_Object *)res;
    }
  }

  if (active)
    orig_certs = ACTIVE_CERTS(stx);
  else
    orig_certs = INACTIVE_CERTS(stx);
  now_certs = orig_certs;

  shortcut = 0;
  if (now_certs && certs && !use_key && CERT_NO_KEY(certs)) {
    if (now_certs->depth < certs->depth) {
      /* We can add now_certs onto certs, instead of the other
         way around. */
      now_certs = certs;
      certs = orig_certs;
    }
  }

  check_tail = now_certs;
  if (check_tail && certs
      && (certs->depth  > (check_tail->depth >> 1))) {
    while (check_tail->depth > certs->depth) {
      check_tail = check_tail->next;
    }
  }
  
  for (; certs; certs = next_certs) {
    next_certs = certs->next;
    if (check_tail && (check_tail->depth > certs->depth))
      check_tail = check_tail->next;
    if (SAME_OBJ(certs, check_tail)) {
      /* tails match --- no need to keep checking */
      break;
    }
    if (!cert_in_chain(certs->mark, use_key, now_certs)) {
      if (!now_certs && !use_key && (shortcut || CERT_NO_KEY(certs))) {
        now_certs = certs;
        next_certs = NULL;
      } else {
        now_certs = cons_cert(certs->mark, certs->modidx, certs->insp, use_key, 
                              now_certs);
      }
    }
  }

  if (!SAME_OBJ(now_certs, orig_certs)) {
    res = (Scheme_Stx *)scheme_make_stx(stx->val, 
                                        stx->srcloc,
                                        stx->props);
    res->wraps = stx->wraps;
    res->u.lazy_prefix = stx->u.lazy_prefix;
    if (!active) {
      pr = scheme_make_raw_pair((Scheme_Object *)ACTIVE_CERTS(stx), (Scheme_Object *)orig_certs);
      res->certs = pr;
      if (stx->certs && SCHEME_RPAIRP(stx->certs)) {
        if (SCHEME_NO_INACTIVE_SUBS_P(stx->certs))
          SCHEME_SET_NO_INACTIVE_SUBS(pr);
        if (SCHEME_NO_ACTIVE_SUBS_P(stx->certs))
          SCHEME_SET_NO_ACTIVE_SUBS(pr);
      }
    } else if (stx->certs && SCHEME_RPAIRP(stx->certs)) {
      pr = scheme_make_raw_pair((Scheme_Object *)orig_certs, SCHEME_CDR(stx->certs));
      res->certs = pr;
      if (SCHEME_NO_INACTIVE_SUBS_P(stx->certs))
        SCHEME_SET_NO_INACTIVE_SUBS(pr);
      if (SCHEME_NO_ACTIVE_SUBS_P(stx->certs))
        SCHEME_SET_NO_ACTIVE_SUBS(pr);
    } else
      res->certs = (Scheme_Object *)orig_certs;
    stx = res;

    if (!active) {
      SCHEME_CDR(stx->certs) = (Scheme_Object *)now_certs;
    } else if (stx->certs && SCHEME_RPAIRP(stx->certs))
      SCHEME_CAR(stx->certs) = (Scheme_Object *)now_certs;
    else
      stx->certs = (Scheme_Object *)now_certs;
  }

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_stx_add_inactive_certs(Scheme_Object *o, Scheme_Object *certs)
  /* Also lifts existing inactive certs to the top. */
{
  o = lift_inactive_certs(o, 0);

  return add_certs(o, (Scheme_Cert *)certs, NULL, 0);
}

Scheme_Object *scheme_stx_propagate_inactive_certs(Scheme_Object *o, Scheme_Object *orig)
{
  Scheme_Cert *certs;

  certs = INACTIVE_CERTS((Scheme_Stx *)orig);

  if (certs)
    return scheme_stx_add_inactive_certs(o, (Scheme_Object *)certs);
  else
    return o;
}

Scheme_Object *scheme_stx_extract_certs(Scheme_Object *o, Scheme_Object *base_certs)
{
  return (Scheme_Object *)append_certs((Scheme_Cert *)base_certs,
                                       ACTIVE_CERTS((Scheme_Stx *)o));
}

Scheme_Object *scheme_stx_cert(Scheme_Object *o, Scheme_Object *mark, Scheme_Env *menv, 
			       Scheme_Object *plus_stx_or_certs, Scheme_Object *key, 
			       int active)
     /* If `name' is module-bound, add the module's certification.
	Also copy any certifications from plus_stx.
	If active and mark is non-NULL, make inactive certificates active.
        Existing inactive are lifted when adding from plus_stx_or_certs. */
{
  if (mark && active) {
    o = scheme_stx_activate_certs(o);
  }

  if (plus_stx_or_certs) {
    Scheme_Cert *certs;
    if (SCHEME_STXP(plus_stx_or_certs))
      certs = ACTIVE_CERTS((Scheme_Stx *)plus_stx_or_certs);
    else
      certs = (Scheme_Cert *)plus_stx_or_certs;
    if (certs) {
      if (!active)
        o = lift_inactive_certs(o, 0);
      o = add_certs(o, certs, key, active);
    }
    /* Also copy over inactive certs, if any */
    if (SCHEME_STXP(plus_stx_or_certs)) {
      o = lift_inactive_certs(o, 0);
      o = add_certs(o, INACTIVE_CERTS((Scheme_Stx *)plus_stx_or_certs), key, 0);
    }
  }

  if (menv && !menv->module->no_cert) {
    Scheme_Stx *stx = (Scheme_Stx *)o, *res;
    Scheme_Cert *cert;

    res = (Scheme_Stx *)scheme_make_stx(stx->val, 
					stx->srcloc,
					stx->props);
    res->wraps = stx->wraps;
    res->u.lazy_prefix = stx->u.lazy_prefix;

    if (SCHEME_FALSEP(mark)) {
      /* Need to invent a certificate-only mark and apply it */
      mark = scheme_new_mark();
      mark = negate_mark(mark);
      res = (Scheme_Stx *)scheme_add_remove_mark((Scheme_Object *)res, mark);
    }

    if (active)
      cert = ACTIVE_CERTS(stx);
    else
      cert = INACTIVE_CERTS(stx);

    cert = cons_cert(mark, menv->link_midx ? menv->link_midx : menv->module->me->src_modidx, 
                     menv->module->insp, key, cert);

    if (active) {
      if (stx->certs && SCHEME_RPAIRP(stx->certs)) {
	Scheme_Object *pr;
	pr = scheme_make_raw_pair((Scheme_Object *)cert, SCHEME_CDR(stx->certs));
	res->certs = pr;
        if (SCHEME_NO_INACTIVE_SUBS_P(stx->certs))
          SCHEME_SET_NO_INACTIVE_SUBS(pr);
        if (SCHEME_NO_ACTIVE_SUBS_P(stx->certs))
          SCHEME_SET_NO_ACTIVE_SUBS(pr);
      } else
	res->certs = (Scheme_Object *)cert;
    } else {
      Scheme_Object *pr;
      pr = scheme_make_raw_pair((Scheme_Object *)ACTIVE_CERTS(stx), (Scheme_Object *)cert);
      res->certs = pr;
      if (stx->certs && SCHEME_RPAIRP(stx->certs)) {
        if (SCHEME_NO_INACTIVE_SUBS_P(stx->certs))
          SCHEME_SET_NO_INACTIVE_SUBS(pr);
        if (SCHEME_NO_ACTIVE_SUBS_P(stx->certs))
          SCHEME_SET_NO_ACTIVE_SUBS(pr);
      }
    }
    
    o = (Scheme_Object *)res;
  }

  return o;
}

Scheme_Object *scheme_stx_content(Scheme_Object *o)
     /* Propagates wraps while getting a syntax object's content. */
{
  Scheme_Stx *stx = (Scheme_Stx *)o;

  /* The fast-past tests are duplicated in jit.c. */

  if ((STX_KEY(stx) & STX_SUBSTX_FLAG) && stx->u.lazy_prefix) {
    Scheme_Object *v = stx->val, *result;
    Scheme_Object *here_wraps;
    Scheme_Object *ml = NULL;
    int wl_count = 0;

    here_wraps = stx->wraps;
    wl_count = stx->u.lazy_prefix;
    stx->u.lazy_prefix = 0;

    if (SCHEME_PAIRP(v)) {
      Scheme_Object *last = NULL, *first = NULL;

      while (SCHEME_PAIRP(v)) {
	Scheme_Object *p;
	result = propagate_wraps(SCHEME_CAR(v), wl_count, &ml, here_wraps);
	p = scheme_make_pair(result, scheme_null);
	if (last)
	  SCHEME_CDR(last) = p;
	else
	  first = p;
	last = p;
	v = SCHEME_CDR(v);
      }
      if (!SCHEME_NULLP(v)) {
	result = propagate_wraps(v, wl_count, &ml, here_wraps);
	if (last)
	  SCHEME_CDR(last) = result;
	else
	  first = result;
      }
      v = first;
    } else if (SCHEME_BOXP(v)) {
      result = propagate_wraps(SCHEME_BOX_VAL(v), wl_count, &ml, here_wraps);
      v = scheme_box(result);
    } else if (SCHEME_VECTORP(v)) {
      Scheme_Object *v2;
      int size = SCHEME_VEC_SIZE(v), i;
      
      v2 = scheme_make_vector(size, NULL);
      
      for (i = 0; i < size; i++) {
	result = propagate_wraps(SCHEME_VEC_ELS(v)[i], wl_count, &ml, here_wraps);
	SCHEME_VEC_ELS(v2)[i] = result;
      }
      
      v = v2;
    } else if (SCHEME_HASHTRP(v)) {
      Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)v, *ht2;
      Scheme_Object *key, *val;
      int i;

      ht2 = scheme_make_hash_tree(SCHEME_HASHTR_FLAGS(ht) & 0x3);

      i = scheme_hash_tree_next(ht, -1);
      while (i != -1) {
        scheme_hash_tree_index(ht, i, &key, &val);
        val = propagate_wraps(val, wl_count, &ml, here_wraps);
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
        r = propagate_wraps(s->slots[i], wl_count, &ml, here_wraps);
        s->slots[i] = r;
      }

      v = (Scheme_Object *)s;
    }

    stx->val = v;
  }

  return stx->val;
}

Scheme_Object *scheme_stx_extract_marks(Scheme_Object *stx)
/* Does not include negative marks */
{
  WRAP_POS awl;
  Scheme_Object *acur_mark, *p, *marks = scheme_null;

  WRAP_POS_INIT(awl, ((Scheme_Stx *)stx)->wraps);

  while (1) {
    /* Skip over renames, immediately-canceled marks, and negative marks: */
    acur_mark = NULL;
    while (1) {
      if (WRAP_POS_END_P(awl))
	break;
      p = WRAP_POS_FIRST(awl);
      if (SCHEME_NUMBERP(p) && IS_POSMARK(p)) {
	if (acur_mark) {
	  if (SAME_OBJ(acur_mark, p)) {
	    acur_mark = NULL;
	    WRAP_POS_INC(awl);
	  } else
	    break;
	} else {
	  acur_mark = p;
	  WRAP_POS_INC(awl);
	}
      } else {
	WRAP_POS_INC(awl);
      }
    }

    if (acur_mark) {
      if (SCHEME_PAIRP(marks) && SAME_OBJ(acur_mark, SCHEME_CAR(marks)))
        marks = SCHEME_CDR(marks);
      else
        marks = scheme_make_pair(acur_mark, marks);
    }

    if (WRAP_POS_END_P(awl))
      return scheme_reverse(marks);
  }
}

Scheme_Object *scheme_stx_strip_module_context(Scheme_Object *_stx)
{
  Scheme_Stx *stx = (Scheme_Stx *)_stx;
  WRAP_POS awl;
  int mod_ctx_count = 0, skipped = 0;
  Scheme_Object *v;
  Wrap_Chunk *chunk;

  /* Check for module context, first: */
  WRAP_POS_INIT(awl, stx->wraps);
  while (!WRAP_POS_END_P(awl)) {
    v = WRAP_POS_FIRST(awl);
    if (SCHEME_RENAMESP(v) || SCHEME_BOXP(v) || SCHEME_RENAMES_SETP(v)) {
      mod_ctx_count++;
    }
    WRAP_POS_INC(awl);
    skipped++;
  }
  
  if (!mod_ctx_count)
    return _stx;

  if (mod_ctx_count == skipped) {
    /* Everything was a module context? An unlikely but easy case. */
    return scheme_make_stx(stx->val, stx->srcloc, stx->props);
  } else {
    /* Copy everything else into a new chunk. */
    chunk = MALLOC_WRAP_CHUNK((skipped - mod_ctx_count));
    chunk->type = scheme_wrap_chunk_type;
    chunk->len = skipped - mod_ctx_count;
    skipped = 0;
    WRAP_POS_INIT(awl, stx->wraps);
    while (!WRAP_POS_END_P(awl)) {
      v = WRAP_POS_FIRST(awl);
      if (!SCHEME_RENAMESP(v) && !SCHEME_BOXP(v) && !SCHEME_RENAMES_SETP(v)) {
	chunk->a[skipped] = v;
	skipped++;
      }
      WRAP_POS_INC(awl);
    }

    stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
    v = scheme_make_pair((Scheme_Object *)chunk, scheme_null);
    stx->wraps = v;
    return (Scheme_Object *)stx;
  }
}

#ifdef DO_STACK_CHECK
static Scheme_Object *stx_strip_certs_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Cert **cp = (Scheme_Cert **)p->ku.k.p2;
  int active = p->ku.k.i1;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return stx_strip_certs(o, cp, active);
}
#endif

static Scheme_Object *stx_strip_certs(Scheme_Object *o, Scheme_Cert **cp, int active)
{
#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Cert **_cp;
      _cp = MALLOC_N(Scheme_Cert*, 1);
      *_cp = *cp;
      p->ku.k.p1 = (void *)o;
      p->ku.k.p2 = (void *)_cp;
      p->ku.k.i1 = active;
      o = scheme_handle_stack_overflow(stx_strip_certs_k);
      *cp = *_cp;
      return o;
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  if (SCHEME_PAIRP(o)) {
    Scheme_Object *a, *d;
    a = stx_strip_certs(SCHEME_CAR(o), cp, active);
    d = stx_strip_certs(SCHEME_CDR(o), cp, active);
    if (SAME_OBJ(a, SCHEME_CAR(o))
	&& SAME_OBJ(d, SCHEME_CDR(o)))
      return o;
    return ICONS(a, d);
  } else if (SCHEME_NULLP(o)) {
    return o;
  } else if (SCHEME_BOXP(o)) {
    Scheme_Object *c;
    c = stx_strip_certs(SCHEME_BOX_VAL(o), cp, active);
    if (SAME_OBJ(c, SCHEME_BOX_VAL(o)))
      return o;
    o = scheme_box(c);
    SCHEME_SET_IMMUTABLE(o);
    return o;
  } else if (SCHEME_VECTORP(o)) {
    Scheme_Object *e = NULL, *v2;
    int size = SCHEME_VEC_SIZE(o), i, j;
    
    for (i = 0; i < size; i++) {
      e = stx_strip_certs(SCHEME_VEC_ELS(o)[i], cp, active);
      if (!SAME_OBJ(e, SCHEME_VEC_ELS(o)[i]))
	break;
    }

    if (i == size)
      return o;

    v2 = scheme_make_vector(size, NULL);
    
    for (j = 0; j < i; j++) {
      SCHEME_VEC_ELS(v2)[j] = SCHEME_VEC_ELS(o)[j];
    }
    SCHEME_VEC_ELS(v2)[i] = e;
    for (i++; i < size; i++) {
      e = stx_strip_certs(SCHEME_VEC_ELS(o)[i], cp, active);
      SCHEME_VEC_ELS(v2)[i] = e;
    }

    SCHEME_SET_IMMUTABLE(v2);
    return v2;
  } else if (SCHEME_HASHTRP(o)) {
    Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)o, *ht2;
    Scheme_Object *key = NULL, *val, *e, *jkey;
    int i, j;
    
    j = scheme_hash_tree_next(ht, -1);
    while (j != -1) {
      scheme_hash_tree_index(ht, j, &key, &val);
      e = stx_strip_certs(val, cp, active);
      if (!SAME_OBJ(e, val))
        break;
      j = scheme_hash_tree_next(ht, j);
    }

    if (j == -1)
      return o;
    jkey = key;

    ht2 = scheme_make_hash_tree(SCHEME_HASHTR_FLAGS(ht) & 0x3);
    
    i = scheme_hash_tree_next(ht, -1);
    while (i != j) {
      scheme_hash_tree_index(ht, i, &key, &val);
      ht2 = scheme_hash_tree_set(ht2, key, val);
      i = scheme_hash_tree_next(ht, i);
    }
    ht2 = scheme_hash_tree_set(ht2, key, e);
    i = scheme_hash_tree_next(ht, i);
    while (i != -1) {
      scheme_hash_tree_index(ht, i, &key, &val);
      val = stx_strip_certs(val, cp, active);
      ht2 = scheme_hash_tree_set(ht2, key, val);
      i = scheme_hash_tree_next(ht, i);
    }
    
    return (Scheme_Object *)ht2;
  } else if (prefab_p(o)) {
    Scheme_Object *e = NULL;
    Scheme_Structure *s = (Scheme_Structure *)o;
    int i, size = s->stype->num_slots;

    for (i = 0; i < size; i++) {
      e = stx_strip_certs(s->slots[i], cp, active);
      if (!SAME_OBJ(e, s->slots[i]))
	break;
    }

    if (i == size)
      return o;

    s = (Scheme_Structure *)scheme_clone_prefab_struct_instance(s);
    s->slots[i] = e;
    
    for (i++; i < size; i++) {
      e = stx_strip_certs(s->slots[i], cp, active);
      s->slots[i] = e;
    }
    
    return (Scheme_Object *)s;
  } else if (SCHEME_STXP(o)) {
    Scheme_Stx *stx = (Scheme_Stx *)o;

    if ((!active && INACTIVE_CERTS(stx))
        || (active && ACTIVE_CERTS(stx))) {
      Scheme_Object *np, *v;
      Scheme_Stx *res;
      Scheme_Cert *certs;

      if ((!active && SCHEME_NO_INACTIVE_SUBS_P(stx->certs))
          || (active && stx->certs && SCHEME_RPAIRP(stx->certs) && SCHEME_NO_ACTIVE_SUBS_P(stx->certs))) {
        /* No sub-object has other [in]active certs */
        v = stx->val;
      } else {
        v = stx_strip_certs(stx->val, cp, active);
      }

      res = (Scheme_Stx *)scheme_make_stx(v, 
					  stx->srcloc,
					  stx->props);
      res->wraps = stx->wraps;
      res->u.lazy_prefix = stx->u.lazy_prefix;
      if (!active) {
        if (!ACTIVE_CERTS(stx)) {
          if (stx->certs && SCHEME_RPAIRP(stx->certs) && SCHEME_NO_ACTIVE_SUBS_P(stx->certs))
            np = no_nested_certs;
          else
            np = no_nested_inactive_certs;
        } else {
          np = scheme_make_raw_pair((Scheme_Object *)ACTIVE_CERTS(stx), NULL);
          SCHEME_SET_NO_INACTIVE_SUBS(np);
          if (stx->certs && SCHEME_RPAIRP(stx->certs) && SCHEME_NO_ACTIVE_SUBS_P(stx->certs))
            SCHEME_SET_NO_ACTIVE_SUBS(np);
        }
      } else {
        if (!INACTIVE_CERTS(stx)) {
          if (stx->certs && SCHEME_RPAIRP(stx->certs) && SCHEME_NO_INACTIVE_SUBS_P(stx->certs))
            np = no_nested_certs;
          else
            np = no_nested_active_certs;
        } else {
          np = scheme_make_raw_pair(NULL, (Scheme_Object *)INACTIVE_CERTS(stx));
          SCHEME_SET_NO_ACTIVE_SUBS(np);
          if (SCHEME_NO_INACTIVE_SUBS_P(stx->certs))
            SCHEME_SET_NO_INACTIVE_SUBS(np);
        }
      }
      res->certs = np;

      certs = append_certs((active ? ACTIVE_CERTS(stx) : INACTIVE_CERTS(stx)), *cp);
      *cp = certs;

      return (Scheme_Object *)res;
    } else if (stx->certs 
               && SCHEME_RPAIRP(stx->certs) 
               && (active
                   ? SCHEME_NO_ACTIVE_SUBS_P(stx->certs)
                   : SCHEME_NO_INACTIVE_SUBS_P(stx->certs))) {
      /* Explicit pair, but no [in]active certs anywhere in this object. */
      return (Scheme_Object *)stx;
    } else {
      Scheme_Stx *res;
      Scheme_Object *prev;

      o = stx_strip_certs(stx->val, cp, active);

      if (!SAME_OBJ(o, stx->val)) {
	res = (Scheme_Stx *)scheme_make_stx(o, 
					    stx->srcloc,
					    stx->props);
	res->wraps = stx->wraps;
	res->u.lazy_prefix = stx->u.lazy_prefix;
      } else {
        /* No new syntax object, but record the absence of certificates in 
           sub-parts: */
        res = stx;
      }

      prev = stx->certs;
      if (!active) {
        if (ACTIVE_CERTS(stx)) {
          Scheme_Object *np;
          np = scheme_make_raw_pair((Scheme_Object *)ACTIVE_CERTS(stx), NULL);
          res->certs = np;
          SCHEME_SET_NO_INACTIVE_SUBS(np);
          if (prev && SCHEME_RPAIRP(prev) && SCHEME_NO_ACTIVE_SUBS_P(prev))
            SCHEME_SET_NO_ACTIVE_SUBS(np);
        } else if (prev && SCHEME_RPAIRP(prev) && SCHEME_NO_ACTIVE_SUBS_P(prev))
          res->certs = no_nested_certs;
        else
          res->certs = no_nested_inactive_certs;
      } else {
        if (INACTIVE_CERTS(stx)) {
          Scheme_Object *np;
          np = scheme_make_raw_pair(NULL, (Scheme_Object *)INACTIVE_CERTS(stx));
          res->certs = np;
          SCHEME_SET_NO_ACTIVE_SUBS(np);
          if (prev && SCHEME_RPAIRP(prev) && SCHEME_NO_INACTIVE_SUBS_P(prev))
            SCHEME_SET_NO_INACTIVE_SUBS(np);
        } else if (prev && SCHEME_RPAIRP(prev) && SCHEME_NO_INACTIVE_SUBS_P(prev))
          res->certs = no_nested_certs;
        else
          res->certs = no_nested_active_certs;
      }
      
      return (Scheme_Object *)res;
    }
  } else
    return o;
}

static Scheme_Object *lift_inactive_certs(Scheme_Object *o, int as_active)
{
  Scheme_Cert *certs = NULL;

  o = stx_strip_certs(o, &certs, 0);

  if (certs)
    o = add_certs(o, certs, NULL, as_active);

  return o;
}

Scheme_Object *scheme_stx_activate_certs(Scheme_Object *o)
{
  return lift_inactive_certs(o, 1);
}

Scheme_Object *scheme_stx_lift_active_certs(Scheme_Object *o)
{
  Scheme_Cert *certs = NULL;
  Scheme_Stx *stx = (Scheme_Stx *)o;

  if (stx->certs && SCHEME_RPAIRP(stx->certs) && SCHEME_NO_ACTIVE_SUBS_P(stx->certs))
    return o;

  o = stx_strip_certs(o, &certs, 1);

  if (certs)
    o = add_certs(o, certs, NULL, 1);

  return o;  
}

int scheme_stx_has_empty_wraps(Scheme_Object *o)
{
  WRAP_POS awl;
  Scheme_Object *mark = NULL, *v;

  WRAP_POS_INIT(awl, ((Scheme_Stx *)o)->wraps);
  while (!WRAP_POS_END_P(awl)) {
    v = WRAP_POS_FIRST(awl);
    if (mark) {
      if (!SAME_OBJ(mark, v))
        return 0;
      mark = NULL;
    } else
      mark = v;
    WRAP_POS_INC(awl);
  }

  return !mark;
}

/*========================================================================*/
/*                           stx comparison                               */
/*========================================================================*/

/* If no marks and no rename with this set's tag,
   then it was an unmarked-but-actually-introduced id. */

static Scheme_Object *check_floating_id(Scheme_Object *stx)
{
  /* If `a' has a mzMOD_RENAME_MARKED rename with no following
     mzMOD_RENAME_NORMAL using the same set tag, and if there are no
     marks after the mzMOD_RENAME_MARKED rename, then we've hit a
     corner case: an identifier that was introduced by macro expansion
     but marked so that it appears to be original. To ensure that it
     gets a generated symbol in the MOD_RENAME_MARKED table, give it a
     "floating" binding: scheme_void. This is a rare case, and it more
     likely indicates a buggy macro than anything else. */
  WRAP_POS awl;
  Scheme_Object *cur_mark = NULL, *searching_identity = NULL, *a;
  int no_mark_means_floating = 0;

  WRAP_POS_INIT(awl, ((Scheme_Stx *)stx)->wraps);
  
  while (!WRAP_POS_END_P(awl)) {

    a = WRAP_POS_FIRST(awl);
    
    if (SCHEME_RENAMESP(a)
        || SCHEME_RENAMES_SETP(a)) {
      int kind;
      Scheme_Object *set_identity;

      if (SCHEME_RENAMESP(a)) {
        Module_Renames *mrn = (Module_Renames *)a;
        
        kind = mrn->kind;
        set_identity = mrn->set_identity;
      } else {
        Module_Renames_Set *mrns = (Module_Renames_Set *)a;

        kind = mrns->kind;
        set_identity = mrns->set_identity;
      }

      if (SAME_OBJ(set_identity, searching_identity))
        searching_identity = NULL;

      if (searching_identity)
        no_mark_means_floating = 1;

      if (kind == mzMOD_RENAME_MARKED)
        searching_identity = set_identity;
      else
        searching_identity = NULL;
        
    } else if (SCHEME_MARKP(a)) {
      if (SAME_OBJ(a, cur_mark))
        cur_mark = 0;
      else {
        if (cur_mark) {
          no_mark_means_floating = 0;
          searching_identity = NULL;
        }
        cur_mark = a;
      }
    }

    WRAP_POS_INC(awl);
  }

  if (cur_mark) {
    no_mark_means_floating = 0;
    searching_identity = NULL;
  }

  if (searching_identity || no_mark_means_floating)
    return scheme_void;

  return scheme_false;
}

#define EXPLAIN_RESOLVE 0
#if EXPLAIN_RESOLVE
int scheme_explain_resolves = 0;
# define EXPLAIN(x) if (scheme_explain_resolves) { x; }
#else
# define EXPLAIN(x) /* empty */
#endif

static int same_marks(WRAP_POS *_awl, WRAP_POS *_bwl, Scheme_Object *barrier_env)
/* Compares the marks in two wraps lists. A result of 2 means that the
   result depended on a barrier env. For a rib-based renaming, we need
   to check only up to the rib, and the barrier effect important for
   when a rib-based renaming is layered with another renaming (such as
   when an internal-definition-base local-expand is used to form a new
   set of bindings, as in the unit form); simplification cleans up the
   layers, so that we only need to check in ribs. */
{
  WRAP_POS awl;
  WRAP_POS bwl;
  Scheme_Object *acur_mark, *bcur_mark;
# define FAST_STACK_SIZE 4
  Scheme_Object *a_mark_stack_fast[FAST_STACK_SIZE], *b_mark_stack_fast[FAST_STACK_SIZE];
  Scheme_Object **a_mark_stack = a_mark_stack_fast, **b_mark_stack = b_mark_stack_fast, **naya;
  int a_mark_cnt = 0, a_mark_size = FAST_STACK_SIZE, b_mark_cnt = 0, b_mark_size = FAST_STACK_SIZE;
  int used_barrier = 0;

  WRAP_POS_COPY(awl, *_awl);
  WRAP_POS_COPY(bwl, *_bwl);

  /* A simple way to compare marks would be to make two lists of
     marks.  The loop below attempts to speed up that process by
     discovering common and canceled marks early, so they can be
     omitted from the lists. The "stack" arrays accumulate the parts
     of the list that can't be skipped that way. */

  while (1) {
    /* Skip over renames and canceled marks: */
    acur_mark = NULL;
    while (1) { /* loop for canceling stack */
      /* this loop handles immediately canceled marks */
      while (1) {
        if (WRAP_POS_END_P(awl))
          break;
        if (SCHEME_NUMBERP(WRAP_POS_FIRST(awl)) && IS_POSMARK(WRAP_POS_FIRST(awl))) {
          if (acur_mark) {
            if (SAME_OBJ(acur_mark, WRAP_POS_FIRST(awl))) {
              acur_mark = NULL;
              WRAP_POS_INC(awl);
            } else
              break;
          } else {
            acur_mark = WRAP_POS_FIRST(awl);
            WRAP_POS_INC(awl);
          }
        } else if (SCHEME_RIBP(WRAP_POS_FIRST(awl))) {
          if (SCHEME_FALSEP(barrier_env)) {
            WRAP_POS_INC(awl);
          } else {
            /* See if the barrier environment is in this rib. */
            Scheme_Lexical_Rib *rib;
            rib = (Scheme_Lexical_Rib *)WRAP_POS_FIRST(awl);
            for (rib = rib->next; rib; rib = rib->next) {
              if (SAME_OBJ(SCHEME_VEC_ELS(rib->rename)[0], barrier_env))
                break;
            }
            if (!rib) {
              WRAP_POS_INC(awl);
            } else {
              WRAP_POS_INIT_END(awl);
              used_barrier = 1;
            }
          }
        } else {
          WRAP_POS_INC(awl);
        }
      }
      /* Maybe cancel a mark on the stack */
      if (acur_mark && a_mark_cnt) {
        if (SAME_OBJ(acur_mark, a_mark_stack[a_mark_cnt - 1])) {
          --a_mark_cnt;
          if (a_mark_cnt) {
            acur_mark = a_mark_stack[a_mark_cnt - 1];
            --a_mark_cnt;
            break;
          } else
            acur_mark = NULL;
        } else
          break;
      } else
        break;
    }

    bcur_mark = NULL;
    while (1) { /* loop for canceling stack */
      while (1) {
        if (WRAP_POS_END_P(bwl))
          break;
        if (SCHEME_NUMBERP(WRAP_POS_FIRST(bwl)) && IS_POSMARK(WRAP_POS_FIRST(bwl))) {
          if (bcur_mark) {
            if (SAME_OBJ(bcur_mark, WRAP_POS_FIRST(bwl))) {
              bcur_mark = NULL;
              WRAP_POS_INC(bwl);
            } else
              break;
          } else {
            bcur_mark = WRAP_POS_FIRST(bwl);
            WRAP_POS_INC(bwl);
          }
        } else if (SCHEME_RIBP(WRAP_POS_FIRST(bwl))) {
          if (SCHEME_FALSEP(barrier_env)) {
            WRAP_POS_INC(bwl);
          } else {
            /* See if the barrier environment is in this rib. */
            Scheme_Lexical_Rib *rib;
            rib = (Scheme_Lexical_Rib *)WRAP_POS_FIRST(bwl);
            for (rib = rib->next; rib; rib = rib->next) {
              if (SAME_OBJ(SCHEME_VEC_ELS(rib->rename)[0], barrier_env))
                break;
            }
            if (!rib) {
              WRAP_POS_INC(bwl);
            } else {
              WRAP_POS_INIT_END(bwl);
              used_barrier = 1;
            }
          }
        } else {
          WRAP_POS_INC(bwl);
        }
      }
      /* Maybe cancel a mark on the stack */
      if (bcur_mark && b_mark_cnt) {
        if (SAME_OBJ(bcur_mark, b_mark_stack[b_mark_cnt - 1])) {
          --b_mark_cnt;
          if (b_mark_cnt) {
            bcur_mark = b_mark_stack[b_mark_cnt - 1];
            --b_mark_cnt;
            break;
          } else
            bcur_mark = NULL;
        } else
          break;
      } else
        break;
    }

    /* Same mark? */
    if (a_mark_cnt || b_mark_cnt || !SAME_OBJ(acur_mark, bcur_mark)) {
      /* Not the same, so far; push onto stacks in case they're
         cancelled later */
      if (acur_mark) {
        if (a_mark_cnt >= a_mark_size) {
          a_mark_size *= 2;
          naya = MALLOC_N(Scheme_Object*, a_mark_size);
          memcpy(naya, a_mark_stack, sizeof(Scheme_Object *)*a_mark_cnt);
          a_mark_stack = naya;
        }
        a_mark_stack[a_mark_cnt++] = acur_mark;
      }
      if (bcur_mark) {
        if (b_mark_cnt >= b_mark_size) {
          b_mark_size *= 2;
          naya = MALLOC_N(Scheme_Object*, b_mark_size);
          memcpy(naya, b_mark_stack, sizeof(Scheme_Object *)*b_mark_cnt);
          b_mark_stack = naya;
        }
        b_mark_stack[b_mark_cnt++] = bcur_mark;
      }
    }

    /* Done if both reached the end: */
    if (WRAP_POS_END_P(awl) && WRAP_POS_END_P(bwl)) {
      EXPLAIN(fprintf(stderr, "    %d vs. %d marks\n", a_mark_cnt, b_mark_cnt));
      if (a_mark_cnt == b_mark_cnt) {
        while (a_mark_cnt--) {
          if (!SAME_OBJ(a_mark_stack[a_mark_cnt], b_mark_stack[a_mark_cnt]))
            return 0;
        }
        return used_barrier + 1;
      } else
        return 0;
    }
  }
}

static int includes_mark(Scheme_Object *wraps, Scheme_Object *mark)
/* Checks for positive or negative (certificate-only) mark.
   FIXME: canceling marks are detected only when they're immediately
   canceling (i.e., no canceled marks in between). */
{
  WRAP_POS awl;
  Scheme_Object *acur_mark;

  WRAP_POS_INIT(awl, wraps);

  while (1) {
    /* Skip over renames and cancelled marks: */
    acur_mark = NULL;
    while (1) {
      if (WRAP_POS_END_P(awl))
	break;
      if (SCHEME_NUMBERP(WRAP_POS_FIRST(awl))) {
	if (acur_mark) {
	  if (SAME_OBJ(acur_mark, WRAP_POS_FIRST(awl))) {
	    acur_mark = NULL;
	    WRAP_POS_INC(awl);
	  } else
	    break;
	} else {
	  acur_mark = WRAP_POS_FIRST(awl);
	  WRAP_POS_INC(awl);
	}
      } else {
	WRAP_POS_INC(awl);
      }
    }

    /* Same mark? */
    if (SAME_OBJ(acur_mark, mark))
      return 1;

    if (WRAP_POS_END_P(awl))
      return 0;
  }
}

static void add_all_marks(Scheme_Object *wraps, Scheme_Hash_Table *marks)
/* Adds both positive and negative marks to marks table. This may add too many
   marks, because it detects only immediately canceling marks. */
{
  WRAP_POS awl;
  Scheme_Object *acur_mark;

  WRAP_POS_INIT(awl, wraps);

  while (1) {
    /* Skip over renames and cancelled marks: */
    acur_mark = NULL;
    while (1) {
      if (WRAP_POS_END_P(awl))
	break;
      if (SCHEME_NUMBERP(WRAP_POS_FIRST(awl))) {
	if (acur_mark) {
	  if (SAME_OBJ(acur_mark, WRAP_POS_FIRST(awl))) {
	    acur_mark = NULL;
	    WRAP_POS_INC(awl);
	  } else
	    break;
	} else {
	  acur_mark = WRAP_POS_FIRST(awl);
	  WRAP_POS_INC(awl);
	}
      } else {
	WRAP_POS_INC(awl);
      }
    }

    if (acur_mark)
      scheme_hash_set(marks, acur_mark, scheme_true);
    else
      return;
  }
}

static int check_matching_marks(Scheme_Object *p, Scheme_Object *orig_id, Scheme_Object **marks_cache, int depth, 
                                int *_skipped)
{
  int l1, l2;
  Scheme_Object *m1, *m2;

  p = SCHEME_CDR(p); /* skip modidx */
  p = SCHEME_CDR(p); /* skip phase_export */
  if (SCHEME_PAIRP(p)) {
    /* has marks */
    int skip = 0;
    
    EXPLAIN(fprintf(stderr, "%d       has marks\n", depth));

    m1 = SCHEME_CAR(p);
    if (*marks_cache)
      m2 = *marks_cache;
    else {
      EXPLAIN(fprintf(stderr, "%d       extract marks\n", depth));
      m2 = scheme_stx_extract_marks(orig_id);
      *marks_cache = m2;
    }

    l1 = scheme_list_length(m1);
    l2 = scheme_list_length(m2);

    if (l2 < l1) return -1; /* no match */

    while (l2 > l1) {
      m2 = SCHEME_CDR(m2);
      l2--;
      skip++;
    }

    if (scheme_equal(m1, m2)) {
      if (_skipped ) *_skipped = skip;
      return l1; /* matches */
    } else
      return -1; /* no match */
  } else {
    if (_skipped) *_skipped = -1;
    return 0; /* match empty mark set */
  }
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

static Scheme_Object *search_shared_pes(Scheme_Object *shared_pes, 
                                        Scheme_Object *glob_id, Scheme_Object *orig_id,
                                        Scheme_Object **get_names, int get_orig_name,
                                        int depth,
                                        int *_skipped)
{
  Scheme_Object *pr, *idx, *pos, *src, *best_match = NULL;
  Scheme_Module_Phase_Exports *pt;
  int i, phase, best_match_len = -1, skip = 0;
  Scheme_Object *marks_cache = NULL;

  for (pr = shared_pes; !SCHEME_NULLP(pr); pr = SCHEME_CDR(pr)) {
    pt = (Scheme_Module_Phase_Exports *)SCHEME_CADR(SCHEME_CAR(pr));

    EXPLAIN(fprintf(stderr, "%d     pes table %s\n", depth, 
                    pt->src_modidx
                    ? scheme_write_to_string(scheme_module_resolve(pt->src_modidx, 0), NULL)
                    : "?"));

    if (!pt->ht) {
      /* Lookup table (which is created lazily) not yet created, so do that now... */
      EXPLAIN(fprintf(stderr, "%d     {create lookup}\n", depth));
      scheme_populate_pt_ht(pt);
    }

    pos = scheme_hash_get(pt->ht, glob_id);
    if (pos) {
      /* Found it, maybe. Check marks. */
      int mark_len;
      EXPLAIN(fprintf(stderr, "%d     found %p\n", depth, pos));
      mark_len = check_matching_marks(SCHEME_CAR(pr), orig_id, &marks_cache, depth, &skip);
      if (mark_len > best_match_len) {
        /* Marks match and improve on previously found match. Build suitable rename: */
        best_match_len = mark_len;
        if (_skipped) *_skipped = skip;
        
        idx = SCHEME_CAR(SCHEME_CAR(pr));

        i = SCHEME_INT_VAL(pos);

        if (get_orig_name)
          best_match = pt->provide_src_names[i];
        else {
          if (pt->provide_srcs)
            src = pt->provide_srcs[i];
          else
            src = scheme_false;

          if (get_names) {
            /* If module bound, result is module idx, and get_names[0] is set to source name,
               get_names[1] is set to the nominal source module, get_names[2] is set to
               the nominal source module's export, get_names[3] is set to the phase of
               the source definition, get_names[4] is set to the module import phase index,
               and get_names[5] is set to the nominal export phase */

            if (pt->provide_src_phases)
              phase = pt->provide_src_phases[i];
            else
              phase = 0;

            EXPLAIN(fprintf(stderr, "%d     srcname %s\n", depth, SCHEME_SYM_VAL(pt->provide_src_names[i])));
            get_names[0] = pt->provide_src_names[i];
            get_names[1] = idx;
            get_names[2] = glob_id;
            get_names[3] = scheme_make_integer(phase);
            get_names[4] = SCHEME_CDR(SCHEME_CDR(SCHEME_CAR(pr)));
            if (SCHEME_PAIRP(get_names[4])) /* skip over marks, if any */
              get_names[4] = SCHEME_CDR(get_names[4]);
            get_names[5] = pt->phase_index;
            get_names[6] = (pt->provide_insps ? pt->provide_insps[i] : NULL);
          }

          if (SCHEME_FALSEP(src)) {
            src = idx;
          } else {
            src = scheme_modidx_shift(src, pt->src_modidx, idx);
          }

          best_match = src;
        }
      }
    }
  }

  return best_match;
}

static Module_Renames *extract_renames(Module_Renames_Set *mrns, Scheme_Object *phase)
{
  if (SAME_OBJ(phase, scheme_make_integer(0)))
    return mrns->rt;
  else if (SAME_OBJ(phase, scheme_make_integer(1)))
    return mrns->et;
  else if (mrns->other_phases)
    return (Module_Renames *)scheme_hash_get(mrns->other_phases, phase);
  else
    return NULL;
}

static int nonempty_rib(Scheme_Lexical_Rib *rib)
{
  rib = rib->next;

  while (rib) {
    if (SCHEME_RENAME_LEN(rib->rename))
      return 1;
    rib = rib->next;
  }

  return 0;
}

static int in_skip_set(Scheme_Object *timestamp, Scheme_Object *skip_ribs)
{
  if (!skip_ribs)
    return 0;
  
  if (scheme_hash_tree_get((Scheme_Hash_Tree *)skip_ribs, timestamp))
    return 1;
  
  return 0;
}

static Scheme_Object *add_skip_set(Scheme_Object *timestamp, Scheme_Object *skip_ribs)
{
  if (in_skip_set(timestamp, skip_ribs))
    return skip_ribs;
  
  if (!skip_ribs)
    skip_ribs = (Scheme_Object *)scheme_make_hash_tree(1);
  
  skip_ribs = (Scheme_Object *)scheme_hash_tree_set((Scheme_Hash_Tree *)skip_ribs, timestamp, scheme_true);

  {
    Scheme_Bucket *b;
    scheme_start_atomic();
    b = scheme_bucket_from_table(interned_skip_ribs, (const char *)skip_ribs);
    scheme_end_atomic_no_swap();
    if (!b->val)
      b->val = scheme_true;

    skip_ribs = (Scheme_Object *)HT_EXTRACT_WEAK(b->key);
  }

  return skip_ribs;
}

XFORM_NONGCING static int same_skipped_ribs(Scheme_Object *a, Scheme_Object *b)
{
  return SAME_OBJ(a, b);
}

XFORM_NONGCING static Scheme_Object *filter_cached_env(Scheme_Object *other_env, Scheme_Object *skip_ribs)
{
  Scheme_Object *p;

  if (SCHEME_PAIRP(other_env)) {
    /* paired with free-id=? rename */
    other_env = SCHEME_CAR(other_env);
  }

  if (SCHEME_MPAIRP(other_env)) {
    other_env = SCHEME_CAR(other_env);
    if (!other_env) 
      return scheme_void;
  }

  if (SCHEME_RPAIRP(other_env)) {
    while (other_env) {
      p = SCHEME_CAR(other_env);
      if (same_skipped_ribs(SCHEME_CAR(p), skip_ribs)) {
        return SCHEME_CDR(p);
      }
      other_env = SCHEME_CDR(other_env);
    }
    return scheme_void;
  } else if (!skip_ribs)
    return other_env;
  else
    return scheme_void;
}

static Scheme_Object *extend_cached_env(Scheme_Object *orig, Scheme_Object *other_env, Scheme_Object *skip_ribs,
                                        int depends_on_unsealed_rib)
{
  Scheme_Object *in_mpair = NULL;
  Scheme_Object *free_id_rename = NULL;

  if (SCHEME_PAIRP(orig)) {
    free_id_rename = SCHEME_CDR(orig);
    orig = SCHEME_CAR(orig);
  }

  if (SCHEME_MPAIRP(orig)) {
    in_mpair = orig;
    orig = SCHEME_CAR(orig);
    if (!depends_on_unsealed_rib && !orig) {
      /* no longer depends on unsealed rib: */
      in_mpair = NULL;
      orig = scheme_void;
    } else {
      /* (some) still depends on unsealed rib: */
      if (!orig) {
        /* re-register in list of dependencies */
        SCHEME_CDR(in_mpair) = unsealed_dependencies;
        unsealed_dependencies = in_mpair;
        orig = scheme_void;
      }
    }
  } else if (depends_on_unsealed_rib) {
    /* register dependency: */
    in_mpair = scheme_make_mutable_pair(NULL, unsealed_dependencies);
    unsealed_dependencies = in_mpair;
  }

  if (SCHEME_VOIDP(orig) && !skip_ribs) {
    orig = other_env;
  } else {
    if (!SCHEME_RPAIRP(orig))
      orig = scheme_make_raw_pair(scheme_make_raw_pair(NULL, orig), NULL);

    orig = scheme_make_raw_pair(scheme_make_raw_pair(skip_ribs, other_env), orig);
  }

  if (in_mpair) {
    SCHEME_CAR(in_mpair) = orig;
    orig = in_mpair;
  }

  if (free_id_rename) {
    orig = CONS(orig, free_id_rename);
  }

  return orig;
}

static void extract_lex_range(Scheme_Object *rename, Scheme_Object *a, int *_istart, int *_iend)
{
  int istart, iend, c;

  c = SCHEME_RENAME_LEN(rename);

  if (!SCHEME_FALSEP(SCHEME_VEC_ELS(rename)[1])) {
    void *pos;
    pos = scheme_hash_get((Scheme_Hash_Table *)(SCHEME_VEC_ELS(rename)[1]), a);
    if (pos) {
      istart = SCHEME_INT_VAL(pos);
      if (istart < 0) {
        /* -1 indicates multiple slots matching this name. */
        istart = 0;
        iend = c;
      } else
        iend = istart + 1;
    } else {
      istart = 0;
      iend = 0;
    }
  } else {
    istart = 0;
    iend = c;
  }

  *_istart = istart;
  *_iend = iend;
}

/* This needs to be a multiple of 4: */
#define QUICK_STACK_SIZE 16

/* Although resolve_env may call itself recursively, the recursion
   depth is bounded (by the fact that modules can't be nested,
   etc.). */

static Scheme_Object *resolve_env(WRAP_POS *_wraps,
                                  Scheme_Object *a, Scheme_Object *orig_phase, 
                                  int w_mod, Scheme_Object **get_names,
                                  Scheme_Object *skip_ribs, int *_binding_marks_skipped,
                                  int *_depends_on_unsealed_rib, int depth, 
                                  Scheme_Hash_Table *free_id_recur)
/* Module binding ignored if w_mod is 0.
   If module bound, result is module idx, and get_names[0] is set to source name,
     get_names[1] is set to the nominal source module, get_names[2] is set to
     the nominal source module's export, get_names[3] is set to the phase of
     the source definition, and get_names[4] is set to the nominal import phase index,
     and get_names[5] is set to the nominal export phase; get_names[6] is set to
     an inspector/pair if one applies for a re-export of a protected or unexported, NULL or 
     #f otherwise.
   If lexically bound, result is env id, and a get_names[0] is set to scheme_undefined;
     get_names[1] is set if a free-id=? rename provides a different name for the bindig.
   If neither, result is #f and get_names[0] is either unchanged or NULL; get_names[1]
     is set if a free-id=? rename provides a different name. */
{
  WRAP_POS wraps;
  Scheme_Object *o_rename_stack = scheme_null, *recur_skip_ribs = skip_ribs;
  Scheme_Object *mresult = scheme_false, *mresult_insp;
  Scheme_Object *modidx_shift_to = NULL, *modidx_shift_from = NULL;
  Scheme_Object *rename_stack[QUICK_STACK_SIZE], *rib_delim = scheme_false;
  int stack_pos = 0, no_lexical = 0;
  int is_in_module = 0, skip_other_mods = 0, floating_checked = 0;
  Scheme_Lexical_Rib *rib = NULL, *did_rib = NULL;
  Scheme_Object *phase = orig_phase;
  Scheme_Object *bdg = NULL, *floating = NULL;
  Scheme_Hash_Table *export_registry = NULL;
  int mresult_skipped = -1;
  int depends_on_unsealed_rib = 0, mresult_depends_unsealed = 0;

  EXPLAIN(fprintf(stderr, "%d Resolving %s [skips: %s]:\n", depth, SCHEME_SYM_VAL(SCHEME_STX_VAL(a)),
                  scheme_write_to_string(skip_ribs ? skip_ribs : scheme_false, NULL)));

  if (_wraps) {
    WRAP_POS_COPY(wraps, *_wraps);
    WRAP_POS_INC(wraps);
  } else
    WRAP_POS_INIT(wraps, ((Scheme_Stx *)a)->wraps);
  
  while (1) {
    if (WRAP_POS_END_P(wraps)) {
      /* See rename case for info on rename_stack: */
      Scheme_Object *result, *result_free_rename, *key, *rd;
      int did_lexical = 0;

      EXPLAIN(fprintf(stderr, "%d Rename...\n", depth));

      result = scheme_false;
      result_free_rename = scheme_false;
      rib_delim = scheme_null;
      while (!SCHEME_NULLP(o_rename_stack)) {
	key = SCHEME_VEC_ELS(SCHEME_CAR(o_rename_stack))[0];
	if (SAME_OBJ(key, result)) {
          EXPLAIN(fprintf(stderr, "%d Match %s\n", depth, scheme_write_to_string(key, 0)));
	  did_lexical = 1;
          rd = SCHEME_VEC_ELS(SCHEME_CAR(o_rename_stack))[3];
          if (SCHEME_TRUEP(rd) && !SAME_OBJ(rd, rib_delim) && is_in_rib_delim(result, rd)) {
            /* not a match, due to rib delimiter */
          } else {
            result = SCHEME_VEC_ELS(SCHEME_CAR(o_rename_stack))[1];
            result_free_rename = SCHEME_VEC_ELS(SCHEME_CAR(o_rename_stack))[2];
            rib_delim = rd;
          }
	} else {
          EXPLAIN(fprintf(stderr, "%d No match %s\n", depth, scheme_write_to_string(key, 0)));
          if (SAME_OBJ(key, scheme_true)) {
            /* marks a module-level renaming that overrides lexical renaming */
            did_lexical = 0;
          }
        }
	o_rename_stack = SCHEME_CDR(o_rename_stack);
      }
      while (stack_pos) {
	key = rename_stack[stack_pos - 1];
	if (SAME_OBJ(key, result)) {
          EXPLAIN(fprintf(stderr, "%d Match %s\n", depth, scheme_write_to_string(key, 0)));
          rd = rename_stack[stack_pos - 4];
          if (SCHEME_TRUEP(rd) && !SAME_OBJ(rd, rib_delim) && is_in_rib_delim(result, rd)) {
            /* not a match, due to rib delimiter */
          } else {
            result = rename_stack[stack_pos - 2];
            result_free_rename = rename_stack[stack_pos - 3];
            rib_delim = rd;
            did_lexical = 1;
          }
	} else {
          EXPLAIN(fprintf(stderr, "%d No match %s\n", depth, scheme_write_to_string(key, 0)));
          if (SAME_OBJ(key, scheme_true)) {
            /* marks a module-level renaming that overrides lexical renaming */
            did_lexical = 0;
          }
        }
	stack_pos -= 4;
      }
      if (!did_lexical) {
	result = mresult;
        if (_binding_marks_skipped)
          *_binding_marks_skipped = mresult_skipped;
        if (mresult_depends_unsealed)
          depends_on_unsealed_rib = 1;
      } else {
        if (free_id_recur && !SCHEME_VOIDP(result_free_rename)) {
          Scheme_Object *orig;
          int rib_dep = 0;
          orig = result_free_rename;
          result_free_rename = SCHEME_VEC_ELS(orig)[0];
          if (SCHEME_PAIRP(result_free_rename) && SCHEME_STXP(SCHEME_CAR(result_free_rename))) {
            phase = SCHEME_CDR(result_free_rename);
            if (!SCHEME_FALSEP(SCHEME_VEC_ELS(orig)[1]))
              phase = scheme_bin_plus(phase, SCHEME_VEC_ELS(orig)[1]);
            if (get_names)
              get_names[1] = NULL;
            result = SCHEME_CAR(result_free_rename);
            if (!scheme_hash_get(free_id_recur, result)) {
              scheme_hash_set(free_id_recur, result, scheme_true);
              result = resolve_env(NULL, result, phase,
                                   w_mod, get_names,
                                   NULL, _binding_marks_skipped,
                                   &rib_dep, depth + 1, free_id_recur);
            }
            if (get_names && !get_names[1])
              if (SCHEME_FALSEP(result) || SAME_OBJ(scheme_undefined, get_names[0]))
                get_names[1] = SCHEME_STX_VAL(SCHEME_CAR(result_free_rename));
          } else if (SCHEME_PAIRP(result_free_rename) && SCHEME_SYMBOLP(SCHEME_CDR(result_free_rename))) {
            if (get_names)
              get_names[1] = SCHEME_CAR(result_free_rename);
            result = SCHEME_CDR(result_free_rename);
            if (get_names)
              get_names[0] = scheme_undefined;
          } else if (SAME_OBJ(SCHEME_TYPE(result_free_rename), scheme_free_id_info_type)) {
            result = SCHEME_VEC_ELS(result_free_rename)[0];
            if (get_names) {
              get_names[0] = SCHEME_VEC_ELS(result_free_rename)[1];
              get_names[1] = SCHEME_VEC_ELS(result_free_rename)[2];
              get_names[2] = SCHEME_VEC_ELS(result_free_rename)[3];
              get_names[3] = SCHEME_VEC_ELS(result_free_rename)[4];
              get_names[4] = SCHEME_VEC_ELS(result_free_rename)[5];
              get_names[5] = SCHEME_VEC_ELS(result_free_rename)[6];
              get_names[6] = SCHEME_VEC_ELS(result_free_rename)[7];
            }
          } else {
            if (get_names)
              get_names[1] = SCHEME_CAR(result_free_rename);
            result = scheme_false;
          }
          if (rib_dep)
            depends_on_unsealed_rib = 1;
          if (SAME_TYPE(SCHEME_TYPE(result), scheme_module_index_type))
            result = scheme_modidx_shift(result, SCHEME_VEC_ELS(orig)[2], SCHEME_VEC_ELS(orig)[3]);
        } else {
          if (get_names) {
            get_names[0] = scheme_undefined;
            get_names[1] = NULL;
          }
        }
      }

      if (_depends_on_unsealed_rib)
        *_depends_on_unsealed_rib = depends_on_unsealed_rib;

      EXPLAIN(fprintf(stderr, "%d Result: %s\n", depth, scheme_write_to_string(result, 0)));

      return result;
    } else if ((SCHEME_RENAMESP(WRAP_POS_FIRST(wraps)) 
                || SCHEME_RENAMES_SETP(WRAP_POS_FIRST(wraps)))
               && w_mod) {
      /* Module rename: */
      Module_Renames *mrn;
      int skipped;

      EXPLAIN(fprintf(stderr, "%d Rename/set\n", depth));
	
      if (SCHEME_RENAMESP(WRAP_POS_FIRST(wraps))) {
        mrn = (Module_Renames *)WRAP_POS_FIRST(wraps);
      } else {
        /* Extract the relevant phase, if available */
        Module_Renames_Set *mrns = (Module_Renames_Set *)WRAP_POS_FIRST(wraps);

        if (mrns->kind != mzMOD_RENAME_TOPLEVEL)
	  is_in_module = 1;

        mrn = extract_renames(mrns, phase);
      }

      if (mrn && (!is_in_module || (mrn->kind != mzMOD_RENAME_TOPLEVEL)) 
          && !skip_other_mods) {
	if (mrn->kind != mzMOD_RENAME_TOPLEVEL)
	  is_in_module = 1;

        if (same_phase(phase, mrn->phase)) {
	  Scheme_Object *rename, *nominal = NULL, *glob_id;
          int get_names_done;

          EXPLAIN(fprintf(stderr, "%d  use rename %p %d\n", depth, mrn->phase, mrn->kind));

	  if (mrn->needs_unmarshal) {
            EXPLAIN(fprintf(stderr, "%d  {unmarshal}\n", depth));
	    unmarshal_rename(mrn, modidx_shift_from, modidx_shift_to, export_registry);
          }

          if (mrn->marked_names) {
	    /* Resolve based on rest of wraps: */
            EXPLAIN(fprintf(stderr, "%d  tl_id_sym\n", depth));
	    if (!bdg) {
              EXPLAIN(fprintf(stderr, "%d   get bdg\n", depth));
	      bdg = resolve_env(&wraps, a, orig_phase, 0, NULL, recur_skip_ribs, NULL, NULL, depth+1, NULL);
              if (SCHEME_FALSEP(bdg)) {
                if (!floating_checked) {
                  floating = check_floating_id(a);
                  floating_checked = 1;
                }
                bdg = floating;
              }
            }
	    /* Remap id based on marks and rest-of-wraps resolution: */
	    glob_id = scheme_tl_id_sym((Scheme_Env *)mrn->marked_names, a, bdg, 0, NULL, &skipped);
	  
	    if (SCHEME_TRUEP(bdg)
		&& !SAME_OBJ(glob_id, SCHEME_STX_VAL(a))) {
	      /* Even if this module doesn't match, the lex-renamed id
		 has been top-level bound in its scope, so ignore all
		 lexical renamings.  (If the id was further renamed, then
		 the further renaming would show up in bdg, and bdg wouldn't
		 have matched in marked_names.) */
	      no_lexical = 1;
	      stack_pos = 0;
	      o_rename_stack = scheme_null;
	    }
	  } else {
            skipped = -1;
	    glob_id = SCHEME_STX_VAL(a);
          }

          EXPLAIN(fprintf(stderr, "%d  search %s\n", depth, scheme_write_to_string(glob_id, 0)));

          if (free_id_recur && mrn->free_id_renames) {
            rename = scheme_hash_get(mrn->free_id_renames, glob_id);
            if (rename && SCHEME_STXP(rename)) {
              int sealed;
              rename = extract_module_free_id_binding((Scheme_Object *)mrn,
                                                      glob_id, 
                                                      rename,
                                                      &sealed,
                                                      free_id_recur);
              if (!sealed)
                mresult_depends_unsealed = 1;
            }
          } else
            rename = NULL;
          if (!rename)
            rename = scheme_hash_get(mrn->ht, glob_id);
	  if (!rename && mrn->nomarshal_ht)
	    rename = scheme_hash_get(mrn->nomarshal_ht, glob_id);
          get_names_done = 0;
          if (!rename) {
            EXPLAIN(fprintf(stderr, "%d    in pes\n", depth));
            rename = search_shared_pes(mrn->shared_pes, glob_id, a, get_names, 0, depth, &skipped);
            if (rename)
              get_names_done = 1;
          }

          EXPLAIN(fprintf(stderr, "%d  search result: %p\n", depth, rename));
            	  
	  if (rename) {
            if (mrn->sealed < STX_SEAL_BOUND)
              mresult_depends_unsealed = 1;

	    if (mrn->kind == mzMOD_RENAME_MARKED) {
              /* One job of a mzMOD_RENAME_MARKED renamer is to replace any
                 binding that might have come from the identifier in its source
                 module, instead of the module where it was eventually bound
                 (after being introduced by a macro in the source module). */
	      skip_other_mods = 1;
            }

	    /* match; set mresult, which is used in the case of no lexical capture: */
            mresult_skipped = skipped;

            mresult_insp = NULL;
            
            if (SCHEME_BOXP(rename)) {
              /* This should only happen for mappings from free_id_renames */
              mresult = SCHEME_BOX_VAL(rename);
              if (get_names) {
                if (SCHEME_FALSEP(SCHEME_CDR(mresult)))
                  get_names[0] = NULL;
                else
                  get_names[0] = scheme_undefined;
                get_names[1] = SCHEME_CAR(mresult);
              }
              mresult = SCHEME_CDR(mresult);
            } else {
              if (SCHEME_PAIRP(rename)) {
                mresult = SCHEME_CAR(rename);
                if (is_rename_inspector_info(mresult)) {
                  mresult_insp = mresult;
                  rename = SCHEME_CDR(rename);
                  mresult = SCHEME_CAR(rename);
                }
              } else
                mresult = rename;
	    
              if (modidx_shift_from)
                mresult = scheme_modidx_shift(mresult,
                                              modidx_shift_from,
                                              modidx_shift_to);

              if (get_names) {
                int no_shift = 0;

                if (!get_names_done) {
                  if (SCHEME_PAIRP(rename)) {
                    if (nom_mod_p(rename)) {
                      /* (cons modidx nominal_modidx) case */
                      get_names[0] = glob_id;
                      get_names[1] = SCHEME_CDR(rename);
                      get_names[2] = get_names[0];
                    } else {
                      rename = SCHEME_CDR(rename);
                      if (SCHEME_PAIRP(rename)) {
                        /* (list* modidx [mod-phase] exportname nominal_modidx nominal_exportname) case */
                        if (SCHEME_INTP(SCHEME_CAR(rename))
                            || SCHEME_FALSEP(SCHEME_CAR(rename))) {
                          get_names[3] = SCHEME_CAR(rename);
                          rename = SCHEME_CDR(rename);
                        }
                        get_names[0] = SCHEME_CAR(rename);
                        get_names[1] = SCHEME_CADR(rename);
                        if (SCHEME_PAIRP(get_names[1])) {
                          get_names[4] = SCHEME_CDR(get_names[1]);
                          get_names[1] = SCHEME_CAR(get_names[1]);
                          if (SCHEME_PAIRP(get_names[4])) {
                            get_names[5] = SCHEME_CDR(get_names[4]);
                            get_names[4] = SCHEME_CAR(get_names[4]);
                          } else {
                            get_names[5] = get_names[3];
                          }
                        }
                        get_names[2] = SCHEME_CDDR(rename);
                      } else {
                        /* (cons modidx exportname) case */
                        get_names[0] = rename;
                        get_names[2] = NULL; /* finish below */
                      }
                    }
                  } else {
                    get_names[0] = glob_id;
                    get_names[2] = NULL; /* finish below */
                  }

                  if (!get_names[2]) {
                    get_names[2] = get_names[0];
                    if (nominal)
                      get_names[1] = nominal;
                    else {
                      no_shift = 1;
                      get_names[1] = mresult;
                    }
                  }
                  if (!get_names[4]) {
                    GC_CAN_IGNORE Scheme_Object *pi;
                    pi = phase_to_index(mrn->phase);
                    get_names[4] = pi;
                  }
                  if (!get_names[5]) {
                    get_names[5] = get_names[3];
                  }
                  get_names[6] = mresult_insp;
                }

                if (modidx_shift_from && !no_shift) {
                  Scheme_Object *nom;
                  nom = get_names[1];
                  nom = scheme_modidx_shift(nom,
                                            modidx_shift_from,
                                            modidx_shift_to);
                  get_names[1] = nom;
                }
              }
            }
          } else {
            if (mrn->sealed < STX_SEAL_ALL)
              mresult_depends_unsealed = 1;
	    mresult = scheme_false;
            mresult_skipped = -1;
	    if (get_names)
	      get_names[0] = NULL;
	  }
	}
      }
    } else if (SCHEME_BOXP(WRAP_POS_FIRST(wraps)) && w_mod) {
      /* Phase shift */
      Scheme_Object *vec, *n, *dest, *src;
      
      EXPLAIN(fprintf(stderr, "%d phase shift\n", depth));

      vec = SCHEME_PTR_VAL(WRAP_POS_FIRST(wraps));
      n = SCHEME_VEC_ELS(vec)[0];
      if (SCHEME_TRUEP(phase))
        phase = scheme_bin_minus(phase, n);
     
      src = SCHEME_VEC_ELS(vec)[1];
      dest = SCHEME_VEC_ELS(vec)[2];

      /* If src is #f, shift is just for phase; no redirection */

      if (!SCHEME_FALSEP(src)) {
	if (!modidx_shift_to) {
	  modidx_shift_to = dest;
	} else if (!SAME_OBJ(modidx_shift_from, dest)) {
	  modidx_shift_to = scheme_modidx_shift(dest,
						modidx_shift_from,
						modidx_shift_to);
	}
	
	modidx_shift_from = src;
      }

      {
	Scheme_Object *er;
	er = SCHEME_VEC_ELS(vec)[3];
	if (SCHEME_TRUEP(er))
	  export_registry = (Scheme_Hash_Table *)er;
      }
    } else if (rib || (SCHEME_VECTORP(WRAP_POS_FIRST(wraps))
		       && !no_lexical)) {
      /* Lexical rename: */
      Scheme_Object *rename, *renamed;
      int ri, c, istart, iend;
      Scheme_Lexical_Rib *is_rib;

      if (rib) {
	rename = rib->rename;
	is_rib = rib;
	rib = rib->next;
      } else {
	rename = WRAP_POS_FIRST(wraps);
	is_rib = NULL;
        did_rib = NULL;
      }

      EXPLAIN(fprintf(stderr, "%d lexical rename (%d) %d %s%s\n", depth, is_rib ? 1 : 0,
                      SCHEME_VEC_SIZE(rename), 
                      SCHEME_SYMBOLP(SCHEME_VEC_ELS(rename)[0]) ? SCHEME_SYM_VAL(SCHEME_VEC_ELS(rename)[0]) : "<simp>",
                      SCHEME_FALSEP(SCHEME_VEC_ELS(rename)[1]) ? "" : " hash"));

      c = SCHEME_RENAME_LEN(rename);

      /* Get index from hash table, if there is one: */
      extract_lex_range(rename, SCHEME_STX_VAL(a), &istart, &iend);

      for (ri = istart; ri < iend; ri++) {
	renamed = SCHEME_VEC_ELS(rename)[2+ri];
	if (SAME_OBJ(SCHEME_STX_VAL(a), SCHEME_STX_SYM(renamed))) {
	  int same;

	  {
	    Scheme_Object *other_env, *envname, *free_id_rename;

	    if (SCHEME_SYMBOLP(renamed)) {
	      /* Simplified table */
	      other_env = scheme_false;
	      envname = SCHEME_VEC_ELS(rename)[2+c+ri];
              if (SCHEME_PAIRP(envname)) {
                free_id_rename = SCHEME_CDR(envname);
                envname = SCHEME_CAR(envname);
              } else
                free_id_rename = scheme_void;
	      same = 1;
              no_lexical = 1; /* simplified table always has final result */
              EXPLAIN(fprintf(stderr, "%d Targes %s <- %s %p\n", depth,
                              scheme_write_to_string(envname, 0),
                              scheme_write_to_string(other_env, 0),
                              free_id_rename));
	    } else {
	      envname = SCHEME_VEC_ELS(rename)[0];
	      other_env = SCHEME_VEC_ELS(rename)[2+c+ri];
              if (SCHEME_PAIRP(other_env))
                free_id_rename = SCHEME_CDR(other_env);
              else
                free_id_rename = scheme_void;
              other_env = filter_cached_env(other_env, recur_skip_ribs);
              
	      if (SCHEME_VOIDP(other_env)) {
                int rib_dep = 0;
		SCHEME_USE_FUEL(1);
		other_env = resolve_env(NULL, renamed, 0, 0, NULL, recur_skip_ribs, NULL, &rib_dep, depth+1, NULL);
		{
                  Scheme_Object *e;
                  e = extend_cached_env(SCHEME_VEC_ELS(rename)[2+c+ri], other_env, recur_skip_ribs,
                                        (is_rib && !(*is_rib->sealed)) || rib_dep);
                  SCHEME_VEC_ELS(rename)[2+c+ri] = e;
                }
                if (rib_dep)
                  depends_on_unsealed_rib = 1;
		SCHEME_USE_FUEL(1);
	      }

              EXPLAIN(fprintf(stderr, "%d Target %s <- %s (%d)\n", depth,
                              scheme_write_to_string(envname, 0),
                              scheme_write_to_string(other_env, 0),
                              nom_mod_p(rename)));

	      {
		WRAP_POS w2;
		WRAP_POS_INIT(w2, ((Scheme_Stx *)renamed)->wraps);
		same = same_marks(&w2, &wraps, other_env);
                if (!same)
                  EXPLAIN(fprintf(stderr, "%d Different marks\n", depth));
	      }
	    }
	    
	    if (same) {
	      /* If it turns out that we're going to return
		 other_env, then return envname instead. 
		 It's tempting to try to compare envname to the
		 top element of the stack and combine the two
		 mappings, but the intermediate name may be needed
		 (for other_env values that don't come from this stack). */
              if (free_id_recur && !SCHEME_VOIDP(free_id_rename)) {
                /* Need to remember phase ad shifts for free-id=? rename: */
                Scheme_Object *vec;
                vec = scheme_make_vector(4, NULL);
                SCHEME_VEC_ELS(vec)[0] = free_id_rename;
                SCHEME_VEC_ELS(vec)[1] = phase; 
                SCHEME_VEC_ELS(vec)[2] = modidx_shift_from;
                SCHEME_VEC_ELS(vec)[3] = modidx_shift_to;
                free_id_rename = vec;
              }
	      if (stack_pos < QUICK_STACK_SIZE) {
		rename_stack[stack_pos++] = rib_delim;
		rename_stack[stack_pos++] = free_id_rename;
		rename_stack[stack_pos++] = envname;
		rename_stack[stack_pos++] = other_env;
	      } else {
                Scheme_Object *vec;
                vec = scheme_make_vector(4, NULL);
                SCHEME_VEC_ELS(vec)[0] = other_env;
                SCHEME_VEC_ELS(vec)[1] = envname;
                SCHEME_VEC_ELS(vec)[2] = free_id_rename;
                SCHEME_VEC_ELS(vec)[3] = rib_delim;
		o_rename_stack = CONS(vec, o_rename_stack);
	      }
              if (is_rib) {
                /* skip future instances of the same rib;
                   used to skip the rest of the current rib, too, but 
                   that's wrong in the case that the same symbolic 
                   name with multiple binding contexts is re-bound 
                   in a rib */
                skip_ribs = add_skip_set(is_rib->timestamp, skip_ribs);
              }
	    }

	    break;
	  }
	}
      }
    } else if (SCHEME_RIBP(WRAP_POS_FIRST(wraps)) && !no_lexical) {
      /* Lexical-rename rib. Splice in the names. */
      rib = (Scheme_Lexical_Rib *)WRAP_POS_FIRST(wraps);
      EXPLAIN(fprintf(stderr, "%d Rib: %p...\n", depth, rib));
      if (skip_ribs) {
	if (in_skip_set(rib->timestamp, skip_ribs)) {
          EXPLAIN(fprintf(stderr, "%d Skip rib\n", depth));
	  rib = NULL;
        }
      }
      if (rib) {
        if (!*rib->sealed)
          depends_on_unsealed_rib = 1;
        if (nonempty_rib(rib)) {
          if (SAME_OBJ(did_rib, rib)) {
            EXPLAIN(fprintf(stderr, "%d Did rib\n", depth));
            rib = NULL;
          } else {
            recur_skip_ribs = add_skip_set(rib->timestamp, recur_skip_ribs);
            did_rib = rib;
            if (rib->mapped_names
                && !SCHEME_INTP(rib->mapped_names)
                && !scheme_hash_get((Scheme_Hash_Table *)rib->mapped_names, SCHEME_STX_VAL(a)))
              rib = NULL; /* no need to check individual renames */
            else
              rib = rib->next; /* First rib record has no rename */
          }
        } else
          rib = NULL;
      }
    } else if (SCHEME_RIB_DELIMP(WRAP_POS_FIRST(wraps))) {
      rib_delim = WRAP_POS_FIRST(wraps);
      if (SCHEME_NULLP(SCHEME_BOX_VAL(rib_delim)))
        rib_delim = scheme_false;
      did_rib = NULL;
    } else if (SCHEME_NUMBERP(WRAP_POS_FIRST(wraps))) {
      EXPLAIN(fprintf(stderr, "%d mark %p\n", depth, WRAP_POS_FIRST(wraps)));
      did_rib = NULL;
    } else if (SCHEME_HASHTP(WRAP_POS_FIRST(wraps))) {
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)WRAP_POS_FIRST(wraps);

      EXPLAIN(fprintf(stderr, "%d forwarding table...\n", depth));

      did_rib = NULL;

      if (!ht->count 
	  /* Table isn't finished if 5 is mapped to a limit: */
	  || scheme_hash_get(ht, scheme_make_integer(5))) {
	fill_chain_cache(wraps.l);
      }

      if (!scheme_hash_get(ht, SCHEME_STX_VAL(a))) {
        EXPLAIN(fprintf(stderr, "%d   forwarded\n", depth));
	set_wraps_to_skip(ht, &wraps);

	continue; /* <<<<< ------ */
      }
    } else if (SCHEME_PRUNEP(WRAP_POS_FIRST(wraps))) {
      if (!is_member(SCHEME_STX_VAL(a), SCHEME_BOX_VAL(WRAP_POS_FIRST(wraps)))) {
        /* Doesn't match pruned-to sym; already produce #f */
        return scheme_false;
      }
    }

    if (!rib)
      WRAP_POS_INC(wraps);
  }
}

static Scheme_Object *get_module_src_name(Scheme_Object *a, Scheme_Object *orig_phase, 
                                          Scheme_Hash_Table *free_id_recur)
     /* Gets a module source name under the assumption that the identifier
	is not lexically renamed. This is used as a quick pre-test for
	free-identifier=?. We do have to look at lexical renames to check for
        equivalences installed on detection of make-rename-transformer, but at least
        we can normally cache the result. */
{
  WRAP_POS wraps;
  Scheme_Object *result, *result_from;
  int is_in_module = 0, skip_other_mods = 0, sealed = STX_SEAL_ALL, floating_checked = 0;
  int no_lexical = !free_id_recur;
  Scheme_Object *phase = orig_phase;
  Scheme_Object *bdg = NULL, *floating = NULL;

  if (!free_id_recur
      && SAME_OBJ(phase, scheme_make_integer(0))
      && ((Scheme_Stx *)a)->u.modinfo_cache)
    return ((Scheme_Stx *)a)->u.modinfo_cache;

  WRAP_POS_INIT(wraps, ((Scheme_Stx *)a)->wraps);

  result = NULL;

  while (1) {
    if (WRAP_POS_END_P(wraps)) {
      int can_cache = (sealed >= STX_SEAL_ALL);

      if (result)
        can_cache = (sealed >= STX_SEAL_BOUND); /* If it becomes bound, it can't become unbound. */

      if (!result)
	result = SCHEME_STX_VAL(a);
      
      if (can_cache && SAME_OBJ(orig_phase, scheme_make_integer(0)) && !free_id_recur)
        ((Scheme_Stx *)a)->u.modinfo_cache = result;
 
      return result;
    } else if (SCHEME_RENAMESP(WRAP_POS_FIRST(wraps))
               || SCHEME_RENAMES_SETP(WRAP_POS_FIRST(wraps))) {
      Module_Renames *mrn;

      if (SCHEME_RENAMESP(WRAP_POS_FIRST(wraps))) {
        mrn = (Module_Renames *)WRAP_POS_FIRST(wraps);
      } else {
        /* Extract the relevant phase, if available */
        Module_Renames_Set *mrns = (Module_Renames_Set *)WRAP_POS_FIRST(wraps);

        if (mrns->kind != mzMOD_RENAME_TOPLEVEL)
	  is_in_module = 1;
        
        if ((!is_in_module || (mrns->kind != mzMOD_RENAME_TOPLEVEL))
            && !skip_other_mods) {
          if (mrns->sealed < sealed)
            sealed = mrns->sealed;
        }

        mrn = extract_renames(mrns, phase);
      }

      if (mrn && (!is_in_module || (mrn->kind != mzMOD_RENAME_TOPLEVEL)) 
          && !skip_other_mods) {
	if (mrn->kind != mzMOD_RENAME_TOPLEVEL)
	  is_in_module = 1;

	if (same_phase(phase, mrn->phase)) {
	  /* Module rename: */
	  Scheme_Object *rename, *glob_id;

          if (mrn->sealed < sealed)
            sealed = mrn->sealed;
          
	  if (mrn->needs_unmarshal) {
	    /* Use resolve_env to trigger unmarshal, so that we
	       don't have to implement top/from shifts here: */
	    resolve_env(NULL, a, orig_phase, 1, NULL, NULL, NULL, NULL, 0, NULL);
	  }

	  if (mrn->marked_names) {
	    /* Resolve based on rest of wraps: */
	    if (!bdg)
	      bdg = resolve_env(&wraps, a, orig_phase, 0, NULL, NULL, NULL, NULL, 0, NULL);
            if (SCHEME_FALSEP(bdg))  {
              if (!floating_checked) {
                floating = check_floating_id(a);
                floating_checked = 1;
              }
              bdg = floating;
            }
	    /* Remap id based on marks and rest-of-wraps resolution: */
	    glob_id = scheme_tl_id_sym((Scheme_Env *)mrn->marked_names, a, bdg, 0, NULL, NULL);

            if (SCHEME_TRUEP(bdg)
		&& !SAME_OBJ(glob_id, SCHEME_STX_VAL(a))) {
	      /* See "Even if this module doesn't match, the lex-renamed id" in resolve_env() */
	      no_lexical = 1;
	    }
	  } else
	    glob_id = SCHEME_STX_VAL(a);

          if (free_id_recur && mrn->free_id_renames) {
            rename = scheme_hash_get(mrn->free_id_renames, glob_id);
            if (rename && SCHEME_STXP(rename)) {
              int sealed;
              rename = extract_module_free_id_binding((Scheme_Object *)mrn,
                                                      glob_id, 
                                                      rename,
                                                      &sealed,
                                                      free_id_recur);
              if (!sealed)
                sealed = 0;
            }
          } else
            rename = NULL;
          if (!rename)
            rename = scheme_hash_get(mrn->ht, glob_id);
	  if (!rename && mrn->nomarshal_ht)
	    rename = scheme_hash_get(mrn->nomarshal_ht, glob_id);

          if (!rename)
            result = search_shared_pes(mrn->shared_pes, glob_id, a, NULL, 1, 0, NULL);
	  else {
	    /* match; set result: */
	    if (mrn->kind == mzMOD_RENAME_MARKED)
	      skip_other_mods = 1;
            if (SCHEME_BOXP(rename)) {
              /* only happens with free_id_renames */
              rename = SCHEME_BOX_VAL(rename);
              result = SCHEME_CAR(rename);
            } else if (SCHEME_PAIRP(rename)) {
	      if (nom_mod_p(rename)) {
		result = glob_id;
	      } else {
		result = SCHEME_CDR(rename);
		if (SCHEME_PAIRP(result))
		  result = SCHEME_CAR(result);
	      }
	    } else
	      result = glob_id;
	  }

          result_from = WRAP_POS_FIRST(wraps);
	}
      }
    } else if (SCHEME_BOXP(WRAP_POS_FIRST(wraps))) {
      /* Phase shift */
      Scheme_Object *n, *vec;
      vec = SCHEME_PTR_VAL(WRAP_POS_FIRST(wraps));
      n = SCHEME_VEC_ELS(vec)[0];
      if (SCHEME_TRUEP(phase))
        phase = scheme_bin_minus(phase, n);
    } else if (!no_lexical
               && (SCHEME_VECTORP(WRAP_POS_FIRST(wraps))
                   || SCHEME_RIBP(WRAP_POS_FIRST(wraps)))) {
      /* Lexical rename */
      Scheme_Object *rename, *renamed, *renames;
      Scheme_Lexical_Rib *rib;
      int ri, istart, iend;

      rename = WRAP_POS_FIRST(wraps);
      if (SCHEME_RIBP(rename)) {
        rib = (Scheme_Lexical_Rib *)rename;
        if (rib->mapped_names
            && !SCHEME_INTP(rib->mapped_names)
            && !scheme_hash_get((Scheme_Hash_Table *)rib->mapped_names, SCHEME_STX_VAL(a)))
          rib = NULL; /* no need to check individual renames */
        else
          rib = rib->next;
        rename = NULL;
      } else {
        rib = NULL;
        if (SCHEME_FALSEP(SCHEME_VEC_ELS(rename)[0])) {
          /* No free-id=? renames here. */
          rename = NULL;
        }
      }

      do {
        if (rib) {
          if (!*rib->sealed) sealed = 0;
          rename = rib->rename;
          rib = rib->next;
        }

        if (rename) {
          int c = SCHEME_RENAME_LEN(rename);

          /* Get index from hash table, if there is one: */
          if (!SCHEME_FALSEP(SCHEME_VEC_ELS(rename)[1])) {
            void *pos;
            pos = scheme_hash_get((Scheme_Hash_Table *)(SCHEME_VEC_ELS(rename)[1]), SCHEME_STX_VAL(a));
            if (pos) {
              istart = SCHEME_INT_VAL(pos);
              if (istart < 0) {
                /* -1 indicates multiple slots matching this name. */
                istart = 0;
                iend = c;
              } else
                iend = istart + 1;
            } else {
              istart = 0;
              iend = 0;
            }
          } else {
            istart = 0;
            iend = c;
          }

          for (ri = istart; ri < iend; ri++) {
            renamed = SCHEME_VEC_ELS(rename)[2+ri];
            if (SAME_OBJ(SCHEME_STX_VAL(a), SCHEME_STX_SYM(renamed))) {
              /* Check for free-id mapping: */
              renames = SCHEME_VEC_ELS(rename)[2 + ri + c];
              if (SCHEME_PAIRP(renames)) {
                /* Has a relevant-looking free-id mapping. 
                   Give up on the "fast" traversal. */
                Scheme_Object *modname, *names[7];
                int rib_dep;

                names[0] = NULL;
                names[1] = NULL;
                names[3] = scheme_make_integer(0);
                names[4] = NULL;
                names[5] = NULL;
                names[6] = NULL;

                modname = resolve_env(NULL, a, orig_phase, 1, names, NULL, NULL, &rib_dep, 0, free_id_recur);
                if (rib_dep)
                  sealed = 0;

                if (!SCHEME_FALSEP(modname)
                    && !SAME_OBJ(names[0], scheme_undefined)) {
                  result = names[0];
                } else {
                  result = names[1]; /* can be NULL or alternate name */
                }
                
                WRAP_POS_INIT_END(wraps);
                rib = NULL;
                break;
              }
            }
          }
        }
      } while (rib);
    } else if (SCHEME_PRUNEP(WRAP_POS_FIRST(wraps))) {
      if (!is_member(SCHEME_STX_VAL(a), SCHEME_BOX_VAL(WRAP_POS_FIRST(wraps)))) {
        /* Doesn't match pruned-to sym, so no binding */
        return SCHEME_STX_VAL(a);
      }
    }
    
    /* Keep looking: */
    if (!WRAP_POS_END_P(wraps))
      WRAP_POS_INC(wraps);
  }
}

int scheme_stx_module_eq2(Scheme_Object *a, Scheme_Object *b, Scheme_Object *phase, Scheme_Object *asym)
{
  Scheme_Object *bsym;
  Scheme_Hash_Table *free_id_recur;

  if (!a || !b)
    return (a == b);

  if (SCHEME_STXP(b)) {
    if (!asym)
      free_id_recur = make_recur_table();
    else
      free_id_recur = NULL;
    bsym = get_module_src_name(b, phase, free_id_recur);
    if (!asym)
      release_recur_table(free_id_recur);
  } else
    bsym = b;
  if (!asym) {
    if (SCHEME_STXP(a)) {
      free_id_recur = make_recur_table();
      asym = get_module_src_name(a, phase, free_id_recur);
      release_recur_table(free_id_recur);
    } else
      asym = a;
  }

  /* Same name? */
  if (!SAME_OBJ(asym, bsym))
    return 0;

  if ((a == asym) || (b == bsym))
    return 1;

  free_id_recur = make_recur_table();
  a = resolve_env(NULL, a, phase, 1, NULL, NULL, NULL, NULL, 0, free_id_recur);
  release_recur_table(free_id_recur);

  free_id_recur = make_recur_table();
  b = resolve_env(NULL, b, phase, 1, NULL, NULL, NULL, NULL, 0, free_id_recur);
  release_recur_table(free_id_recur);

  if (SAME_TYPE(SCHEME_TYPE(a), scheme_module_index_type))
    a = scheme_module_resolve(a, 0);
  if (SAME_TYPE(SCHEME_TYPE(b), scheme_module_index_type))
    b = scheme_module_resolve(b, 0);

  /* Same binding environment? */
  return SAME_OBJ(a, b);
}

int scheme_stx_module_eq(Scheme_Object *a, Scheme_Object *b, long phase)
{
  return scheme_stx_module_eq2(a, b, scheme_make_integer(phase), NULL);
}

Scheme_Object *scheme_stx_get_module_eq_sym(Scheme_Object *a, Scheme_Object *phase)
{
  if (SCHEME_STXP(a))
    return get_module_src_name(a, phase, NULL);
  else
    return a;
}

Scheme_Object *scheme_stx_module_name(Scheme_Hash_Table *free_id_recur,
                                      Scheme_Object **a, Scheme_Object *phase, 
				      Scheme_Object **nominal_modidx,    /* how it was imported */
				      Scheme_Object **nominal_name,      /* imported as name */
				      Scheme_Object **mod_phase,         /* original defn phase level */
                                      Scheme_Object **src_phase_index,   /* phase level of import from nominal modidx */ 
                                      Scheme_Object **nominal_src_phase, /* phase level of export from nominal modidx */
                                      Scheme_Object **lex_env,
                                      int *_sealed,
                                      Scheme_Object **insp)
     /* If module bound, result is module idx, and a is set to source name.
	If lexically bound, result is scheme_undefined, a is unchanged,
           and nominal_name is NULL or a free_id=? renamed id.
	If neither, result is NULL, a is unchanged, and
           and nominal_name is NULL or a free_id=? renamed id. */
{
  if (SCHEME_STXP(*a)) {
    Scheme_Object *modname, *names[7];
    int rib_dep;

    names[0] = NULL;
    names[1] = NULL;
    names[3] = scheme_make_integer(0);
    names[4] = NULL;
    names[5] = NULL;
    names[6] = NULL;

    modname = resolve_env(NULL, *a, phase, 1, names, NULL, NULL, _sealed ? &rib_dep : NULL, 0, free_id_recur);
    
    if (_sealed) *_sealed = !rib_dep;

    if (names[0]) {
      if (SAME_OBJ(names[0], scheme_undefined)) {
        if (lex_env)
          *lex_env = modname;
        if (nominal_name)
          *nominal_name = names[1];
        return scheme_undefined;
      } else {
	*a = names[0];
	if (nominal_modidx)
	  *nominal_modidx = names[1];
	if (nominal_name)
	  *nominal_name = names[2];
	if (mod_phase)
	  *mod_phase = names[3];
        if (src_phase_index)
	  *src_phase_index = names[4];
	if (nominal_src_phase)
	  *nominal_src_phase = names[5];
        if (insp)
          *insp = names[6];
	return modname;
      }
    } else {
      if (nominal_name) *nominal_name = names[1];
      return NULL;
    }
  } else {
    if (nominal_name) *nominal_name = NULL;
    if (_sealed) *_sealed = 1;
    return NULL;
  }
}

int scheme_stx_ribs_matter(Scheme_Object *a, Scheme_Object *skip_ribs)
{
  Scheme_Object *m1, *m2, *skips = NULL;

  while (SCHEME_PAIRP(skip_ribs)) {
    skips = add_skip_set(((Scheme_Lexical_Rib *)SCHEME_CAR(skip_ribs))->timestamp,
                         skips);
    skip_ribs = SCHEME_CDR(skip_ribs);
  }

  m1 = resolve_env(NULL, a, scheme_make_integer(0), 1, NULL, NULL, NULL, NULL, 0, NULL);
  m2 = resolve_env(NULL, a, scheme_make_integer(0), 1, NULL, skips, NULL, NULL, 0, NULL);

  return !SAME_OBJ(m1, m2);
}

Scheme_Object *scheme_stx_moduleless_env(Scheme_Object *a)
  /* Returns either false, a lexical-rename symbol, or void for "floating" */
{
  if (SCHEME_STXP(a)) {
    Scheme_Object *r;

    r = resolve_env(NULL, a, scheme_make_integer(0), 0, NULL, NULL, NULL, NULL, 0, NULL);

    if (SCHEME_FALSEP(r))
      r = check_floating_id(a);

    if (r)
      return r;
  }
  return scheme_false;
}

int scheme_stx_env_bound_eq(Scheme_Object *a, Scheme_Object *b, Scheme_Object *uid, Scheme_Object *phase)
     /* If uid is given, it's the environment for b. */
{
  Scheme_Object *asym, *bsym, *ae, *be;

  if (!a || !b)
    return (a == b);

  if (SCHEME_STXP(a))
    asym = SCHEME_STX_VAL(a);
  else
    asym = a;
  if (SCHEME_STXP(b))
    bsym = SCHEME_STX_VAL(b);
  else
    bsym = b;

  /* Same name? */
  if (!SAME_OBJ(asym, bsym))
    return 0;

  ae = resolve_env(NULL, a, phase, 0, NULL, NULL, NULL, NULL, 0, NULL);
  /* No need to module_resolve ae, because we ignored module renamings. */

  if (uid)
    be = uid;
  else {
    be = resolve_env(NULL, b, phase, 0, NULL, NULL, NULL, NULL, 0, NULL);
    /* No need to module_resolve be, because we ignored module renamings. */
  }

  /* Same binding environment? */
  if (!SAME_OBJ(ae, be))
    return 0;

  /* Same marks? (If not lexically bound, ignore mark barriers.) */
  if (!uid) {
    WRAP_POS aw;
    WRAP_POS bw;
    WRAP_POS_INIT(aw, ((Scheme_Stx *)a)->wraps);
    WRAP_POS_INIT(bw, ((Scheme_Stx *)b)->wraps);
    if (!same_marks(&aw, &bw, ae))
      return 0;
  }

  return 1;
}

int scheme_stx_bound_eq(Scheme_Object *a, Scheme_Object *b, Scheme_Object *phase)
{
  return scheme_stx_env_bound_eq(a, b, NULL, phase);
}

#if EXPLAIN_RESOLVE
Scheme_Object *scheme_explain_resolve_env(Scheme_Object *a)
{
  scheme_explain_resolves++;
  a = resolve_env(NULL, a, 0, 0, NULL, NULL, NULL, NULL, 0, NULL);
  --scheme_explain_resolves;
  return a;
}
#endif

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

int scheme_stx_parallel_is_used(Scheme_Object *sym, Scheme_Object *stx)
{
  /* Inspect the wraps to look for a binding: */
  WRAP_POS w;

  WRAP_POS_INIT(w, ((Scheme_Stx *)stx)->wraps);

  while (!WRAP_POS_END_P(w)) {
    if (SCHEME_RENAMESP(WRAP_POS_FIRST(w))) {
      /* Module rename. For simplicity, we look at all renames, even
	 if they're in the wrong phase, or for the wrong module,
	 etc. */
      Module_Renames *mrn = (Module_Renames *)WRAP_POS_FIRST(w);
      
      if (scheme_tl_id_is_sym_used(mrn->marked_names, sym))
	return 1;
    } else if (SCHEME_RENAMES_SETP(WRAP_POS_FIRST(w))) {
      Module_Renames_Set *mrns = (Module_Renames_Set *)WRAP_POS_FIRST(w);
      int i;
      
      if (mrns->rt && scheme_tl_id_is_sym_used(mrns->rt->marked_names, sym))
	return 1;
      if (mrns->et && scheme_tl_id_is_sym_used(mrns->et->marked_names, sym))
	return 1;

      if (mrns->other_phases) {
        for (i = 0; i < mrns->other_phases->size; i++) {
          if (mrns->other_phases->vals[i])
            scheme_tl_id_is_sym_used(((Module_Renames *)mrns->other_phases->vals[i])->marked_names, 
                                     sym);
        }
      }
    }
    WRAP_POS_INC(w);
  }
  
  return 0;
}

int scheme_stx_has_more_certs(Scheme_Object *id, Scheme_Object *id_certs,
			      Scheme_Object *than_id, Scheme_Object *than_id_certs)
  /* There's a good chance that certs is an extension of than_certs. */
{
  int i, j;
  Scheme_Cert *certs, *t_certs;
  Scheme_Hash_Table *ht, *t_ht = NULL;

  if ((!id_certs || SAME_OBJ(id_certs, than_id_certs))
      && !ACTIVE_CERTS((Scheme_Stx *)id))
    return 0;

  if (id_marks_ht) {
    ht = id_marks_ht;
    id_marks_ht = NULL;
  } else
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
  add_all_marks(((Scheme_Stx *)id)->wraps, ht);

  for (i = 0; i < 2; i++) {
    if (i)
      certs = ACTIVE_CERTS((Scheme_Stx *)id);
    else
      certs = (Scheme_Cert *)id_certs;
    while (certs && !SAME_OBJ(certs, (Scheme_Cert *)than_id_certs)) {
      if (scheme_hash_get(ht, certs->mark)) {
	/* Found a relevant certificate in id */
	if (!t_ht) {
	  if (than_id_marks_ht) {
	    t_ht = than_id_marks_ht;
	    than_id_marks_ht = NULL;
	  } else
	    t_ht = scheme_make_hash_table(SCHEME_hash_ptr);
	  add_all_marks(((Scheme_Stx *)than_id)->wraps, t_ht);
	}
	if (scheme_hash_get(t_ht, certs->mark)) {
	  /* than_id has the same mark */
	  for (j = 0; j < 2; j++) {
	    if (j)
	      t_certs = ACTIVE_CERTS((Scheme_Stx *)than_id);
	    else
	      t_certs = (Scheme_Cert *)than_id_certs;
	    while (t_certs) {
	      if (SAME_OBJ(t_certs->mark, certs->mark))
		break;
	      t_certs = t_certs->next;
	    }
	    if (t_certs)
	      break;
	  }
	  if (j == 2) {
	    scheme_reset_hash_table(ht, NULL);
	    id_marks_ht = ht;
	    scheme_reset_hash_table(t_ht, NULL);
	    than_id_marks_ht = t_ht;
	    return 1;
	  }
	}
      }
      certs = certs->next;
    }
  }

  scheme_reset_hash_table(ht, NULL);
  id_marks_ht = ht;
  if (t_ht) {
    scheme_reset_hash_table(t_ht, NULL);
    than_id_marks_ht = t_ht;
  }

  return 0;
}

Scheme_Object *scheme_stx_remove_extra_marks(Scheme_Object *a, Scheme_Object *relative_to,
                                             Scheme_Object *uid)
{
  WRAP_POS aw;
  WRAP_POS bw;

  WRAP_POS_INIT(aw, ((Scheme_Stx *)a)->wraps);
  WRAP_POS_INIT(bw, ((Scheme_Stx *)relative_to)->wraps);

  if (!same_marks(&aw, &bw, scheme_false)) {
    Scheme_Object *wraps = ((Scheme_Stx *)relative_to)->wraps;
    if (uid) {
      /* Add a rename record: */
      Scheme_Object *rn;
      rn = scheme_make_rename(uid, 1);
      scheme_set_rename(rn, 0, relative_to);
      wraps = scheme_make_pair(rn, wraps);
    }

    {
      Scheme_Stx *stx = (Scheme_Stx *)a;
      Scheme_Object *certs;
      certs = stx->certs;
      stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
      stx->wraps = wraps;
      stx->certs = certs;
      a = (Scheme_Object *)stx;
    }
   }

  return a;
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
static int explain_simp = 0;
static void print_skips(Scheme_Object *skips)
{
  while (skips) {
    fprintf(stderr, "  skip %s\n", scheme_write_to_string(SCHEME_CAR(skips), NULL));
    skips = SCHEME_CDR(skips);
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
                                &lex_env, NULL, &insp);
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
  long size, vsize, psize, i, j, pos;

  /* Although it makes no sense to simplify the rename table itself,
     we can simplify it in the context of a particular wrap suffix.
     (But don't mutate the wrap list, because that will stomp on
     tables that might be needed by a propoagation.)

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

  EXPLAIN_S(fprintf(stderr, "[in simplify]\n"));

  EXPLAIN_R(printf("Simplifying %p\n", lex_cache));

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
                    other_env = resolve_env(NULL, stx, 0, 0, NULL, prec_ribs, NULL, &rib_dep, 0, NULL);
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
              other_env = resolve_env(NULL, stx, 0, 0, NULL, prec_ribs, NULL, &rib_dep, 0, NULL);
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
      int which = 0;

      while (1) {
        Module_Renames *mrn;
        int redundant = 0;
      
        if (SCHEME_RENAMESP(a))  {
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
              } else if (SCHEME_BOXP(la)) {
                if (SCHEME_TRUEP(phase))
                  phase = scheme_bin_minus(phase,
                                           SCHEME_VEC_ELS(SCHEME_PTR_VAL(WRAP_POS_FIRST(l)))[0]);
              }
            }
          }

          if (!redundant) {
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
                      if (SCHEME_PAIRP(fil) && is_rename_inspector_info(SCHEME_CAR(fil))) {
                        /* use 1 or 2 to indicate inspector info */
                        if (SCHEME_PAIRP(SCHEME_CAR(fil)))
                          fil = CONS(scheme_make_integer(2), SCHEME_CDR(fil));
                        else
                          fil = CONS(scheme_make_integer(1), SCHEME_CDR(fil));
                      }
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
                stack = CONS(local_key, stack);
              }
            }
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
      if (SCHEME_TRUEP(SCHEME_VEC_ELS(aa)[3])) {
        if (mt)
          a = scheme_hash_get(mt->shift_map, aa);
        else
          a = scheme_hash_get(rns, aa);
        if (!a) {
          a = scheme_make_vector(4, NULL);
          SCHEME_VEC_ELS(a)[0] = SCHEME_VEC_ELS(aa)[0];
          SCHEME_VEC_ELS(a)[1] = SCHEME_VEC_ELS(aa)[1];
          SCHEME_VEC_ELS(a)[2] = SCHEME_VEC_ELS(aa)[2];
          SCHEME_VEC_ELS(a)[3] = scheme_false;
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
        scheme_marshal_using_key(mt, old_key);
        if (!mt->same_map) {
          Scheme_Hash_Table *same_map;
          same_map = scheme_make_hash_table(SCHEME_hash_ptr);
          mt->same_map = same_map;
        }
        scheme_hash_set(mt->same_map, w_in, old_key);
        /* nevermind references that we saw when creating `stack': */
        scheme_marshal_pop_refs(mt, 0);
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

      <converted> = (vector <simple converted> <cert>)
                  | <simple converted>
      <simple converted> = <simple converted pair> | ...

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
     atomic (w.r.t. syntax) values, where there are no certificates
     on any of the sub-parts. */
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
    } else if (!SCHEME_BOXP(v) && !SCHEME_VECTORP(v)) {
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

static Scheme_Object *record_certs(Scheme_Object *cert_marks, Scheme_Marshal_Tables *mt)
{
  Scheme_Object *v, *local_key;

  if (SCHEME_PAIRP(cert_marks)) {
    v = scheme_hash_get(mt->cert_lists, cert_marks);
    if (!v) {
      scheme_hash_set(mt->cert_lists, cert_marks, cert_marks);
      v = cert_marks;
    }

    local_key = scheme_marshal_lookup(mt, v);
    if (local_key) {
      scheme_marshal_using_key(mt, v);
      return local_key;
    } else {
      return scheme_marshal_wrap_set(mt, v, v);
    }
  } else
    return scheme_null;
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
					    int with_marks,
					    Scheme_Marshal_Tables *mt)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *v, *result, *converted_wraps = NULL;

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

      if ((with_marks > 1) && SCHEME_FALSEP(common_wraps)) {
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
    int i;
    
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

  if (with_marks > 1) {
    if (!converted_wraps)
      converted_wraps = wraps_to_datum(stx->val, stx->wraps, mt, NULL, 0);
    result = CONS(result, converted_wraps);
    if (stx->certs) {
      Scheme_Object *cert_marks = scheme_null, *icert_marks = scheme_null;
      Scheme_Cert *certs;

      certs = ACTIVE_CERTS(stx);
      while (certs) {
	cert_marks = scheme_make_pair(certs->modidx, cert_marks);
	cert_marks = scheme_make_pair(certs->mark, cert_marks);
	certs = certs->next;
      }
      certs = INACTIVE_CERTS(stx);
      while (certs) {
	icert_marks = scheme_make_pair(certs->modidx, icert_marks);
	icert_marks = scheme_make_pair(certs->mark, icert_marks);
	certs = certs->next;
      }

      if (SCHEME_PAIRP(cert_marks)
          || SCHEME_PAIRP(icert_marks)) {
        
        cert_marks = record_certs(cert_marks, mt);
        icert_marks = record_certs(icert_marks, mt);

        v = scheme_make_vector(2, NULL);
        SCHEME_VEC_ELS(v)[0] = result;
        if (!SCHEME_NULLP(icert_marks)) {
          cert_marks = scheme_make_pair(cert_marks, icert_marks);
          if (SCHEME_NUMBERP(SCHEME_CAR(cert_marks)))
            cert_marks = scheme_make_pair(scheme_false, cert_marks);
        }
        SCHEME_VEC_ELS(v)[1] = cert_marks;
        result = v;
      }
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
    if (!IS_POSMARK(_a)) {
      /* Map negative mark to negative mark: */
      n = negate_mark(n);
    }
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
    if (SCHEME_PAIRP(p) && SCHEME_INTP(SCHEME_CAR(p))) {
      /* reconstruct inspector info */
      Scheme_Object *insp;
      if (ut)
        insp = scheme_get_cport_inspector(ut->rp);
      else
        insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
      if (!SAME_OBJ(scheme_make_integer(1), SCHEME_CAR(p))) {
        insp = CONS(scheme_make_inspector(insp), insp);
      }
      p = SCHEME_CDR(p0);
      p0 = CONS(insp, p);
    }

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

      mrn = (Module_Renames *)scheme_make_module_rename(phase, kind, NULL);
      mrn->set_identity = set_identity;

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
          if (SCHEME_PAIRP(p) && SCHEME_PAIRP(SCHEME_CAR(p))) {
            /* list of marks: */
            Scheme_Object *m_first = scheme_null, *m_last = NULL, *mp, *after_marks;

            after_marks = SCHEME_CDR(p);
            mli = SCHEME_CAR(p);

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

            /* Rebuild for unmarshaled marks: */
            ai = scheme_make_pair(SCHEME_CAR(ai),
                                  scheme_make_pair(SCHEME_CADR(ai),
                                                   scheme_make_pair(m_first, after_marks)));

            if (!SCHEME_NULLP(mli)) return_NULL;
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
                if (!SCHEME_SYMBOLP(bdg)) return_NULL;
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
        Scheme_Object *vec;
        vec = SCHEME_BOX_VAL(a);
        if (!SCHEME_VECTORP(vec)) return_NULL;
        if (SCHEME_VEC_SIZE(vec) != 4) return_NULL;
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
                                            Scheme_Hash_Table *ht);

Scheme_Object *cert_marks_to_certs(Scheme_Object *cert_marks, 
                                   Scheme_Unmarshal_Tables *ut,
                                   Scheme_Stx *stx_wraps, int *bad)
{
  /* Need to convert a list of marks to certs */
  Scheme_Cert *certs = NULL;
  Scheme_Object *a, *b, *insp, *orig = cert_marks;

  if (SCHEME_NUMBERP(cert_marks)) {
    /* Re-use rename table or env rename */
    int decoded;
    a = scheme_unmarshal_wrap_get(ut, cert_marks, &decoded);
    if (decoded && !a)
      return_NULL;
    if (decoded)
      return a;
    cert_marks = a;
  }

  if (ut)
    insp = scheme_get_cport_inspector(ut->rp);
  else
    insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);

  while (SCHEME_PAIRP(cert_marks)) {
    a = SCHEME_CAR(cert_marks);
    if (!SCHEME_NUMBERP(a)) {
      *bad = 1;
      return_NULL;
    }
    a = unmarshal_mark(a, ut);
    if (!a) { *bad = 1; return_NULL; }
    
    cert_marks = SCHEME_CDR(cert_marks);
    if (!SCHEME_PAIRP(cert_marks)) {
      *bad = 1;
      return_NULL;
    }
    b = SCHEME_CAR(cert_marks);
    if (!SCHEME_SYMBOLP(b)
	&& !SAME_TYPE(SCHEME_TYPE(b), scheme_module_index_type)) {
      *bad = 1;
      return_NULL;
    }
    
    if (!cert_in_chain(a, NULL, certs))
      certs = cons_cert(a, b, insp, NULL, certs);
    
    cert_marks = SCHEME_CDR(cert_marks);
  }
  if (!SCHEME_NULLP(cert_marks)) {
    *bad = 1;
    return_NULL;
  }

  if (SCHEME_NUMBERP(orig)) {
    scheme_unmarshal_wrap_set(ut, orig, (Scheme_Object *)certs);
  }

  return (Scheme_Object *)certs;
}

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

  return datum_to_syntax_inner(o, ut, stx_src, stx_wraps, ht);
}
#endif

static Scheme_Object *datum_to_syntax_inner(Scheme_Object *o, 
                                            Scheme_Unmarshal_Tables *ut,
					    Scheme_Stx *stx_src,
					    Scheme_Stx *stx_wraps, /* or rename table, or boxed precomputed wrap */
					    Scheme_Hash_Table *ht)
{
  Scheme_Object *result, *wraps, *cert_marks = NULL, *hashed;
  int do_not_unpack_wraps = 0;

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
      /* This one has certs */
      if (SCHEME_VEC_SIZE(o) == 2) {
	cert_marks = SCHEME_VEC_ELS(o)[1];
	o = SCHEME_VEC_ELS(o)[0];
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

	a = datum_to_syntax_inner(SCHEME_CAR(o), ut, stx_src, sub_stx_wraps, ht);
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
      if (!SCHEME_NULLP(o)) {
	o = datum_to_syntax_inner(o, ut, stx_src, sub_stx_wraps, ht);
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

    o = datum_to_syntax_inner(o, ut, stx_src, stx_wraps, ht);
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
      a = datum_to_syntax_inner(a, ut, stx_src, stx_wraps, ht);
      if (!a) return_NULL;
      SCHEME_VEC_ELS(result)[i] = a;
    }

    SCHEME_SET_VECTOR_IMMUTABLE(result);
  } else if (SCHEME_CHAPERONE_HASHTRP(o)) {
    Scheme_Hash_Tree *ht1, *ht2;
    Scheme_Object *key, *val;
    int i;

    if (SCHEME_NP_CHAPERONEP(o))
      ht1 = (Scheme_Hash_Tree *)SCHEME_CHAPERONE_VAL(o);
    else
      ht1 = (Scheme_Hash_Tree *)o;
    
    ht2 = scheme_make_hash_tree(SCHEME_HASHTR_FLAGS(ht1) & 0x3);
    
    i = scheme_hash_tree_next(ht1, -1);
    while (i != -1) {
      scheme_hash_tree_index(ht1, i, &key, &val);
      if (!SAME_OBJ((Scheme_Object *)ht1, o))
        val = scheme_chaperone_hash_traversal_get(o, key);
      val = datum_to_syntax_inner(val, ut, stx_src, stx_wraps, ht);
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
      a = datum_to_syntax_inner(s->slots[i], ut, stx_src, stx_wraps, ht);
      if (!a) return NULL;
      s->slots[i] = a;
    }

    result = (Scheme_Object *)s;
  } else {
    result = o;
  }

  if (SCHEME_FALSEP((Scheme_Object *)stx_src))
    result = scheme_make_stx(result, empty_srcloc, NULL);
  else
    result = scheme_make_stx(result, stx_src->srcloc, NULL);

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

  if (cert_marks) {
    /* Need to convert a list of marks to certs */
    Scheme_Object *certs;
    int bad = 0;

    if (SCHEME_PAIRP(cert_marks) 
	&& (SCHEME_PAIRP(SCHEME_CAR(cert_marks))
	    || SCHEME_NULLP(SCHEME_CAR(cert_marks))
            || SCHEME_FALSEP(SCHEME_CAR(cert_marks)))) {
      /* Have both active and inactive certs */
      Scheme_Object *icerts;
      if (SCHEME_FALSEP(SCHEME_CAR(cert_marks)))
        cert_marks = SCHEME_CDR(cert_marks);
      certs = cert_marks_to_certs(SCHEME_CAR(cert_marks), ut, stx_wraps, &bad);
      icerts = cert_marks_to_certs(SCHEME_CDR(cert_marks), ut, stx_wraps, &bad);
      certs = scheme_make_raw_pair(certs, icerts);
    } else {
      /* Just active certs */
      certs = cert_marks_to_certs(cert_marks, ut, stx_wraps, &bad);
    }
    if (bad)
      return_NULL;
    ((Scheme_Stx *)result)->certs = certs;
  }

  if (hashed) {
    scheme_hash_set(ht, hashed, NULL);
  }
  
  return result;
}

static Scheme_Object *general_datum_to_syntax(Scheme_Object *o, 
                                              Scheme_Unmarshal_Tables *ut,
                                              Scheme_Object *stx_src,
                                              Scheme_Object *stx_wraps,
                                              int can_graph, int copy_props)
     /* If stx_wraps is a hash table, then `o' includes marks and certs.
	If copy_props > 0, properties are copied from src.
	If copy_props != 1 or 0, then certs are copied from src, too. */
{
  Scheme_Hash_Table *ht;
  Scheme_Object *v, *code = NULL;

  if (!SCHEME_FALSEP(stx_src) && !SCHEME_STXP(stx_src))
    return o;

  if (SCHEME_STXP(o))
    return o;

  if (can_graph && HAS_CHAPERONE_SUBSTX(o))
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
			    ht);

  if (!v) {
    if (ut)
      return_NULL; /* happens with bad wraps from a bad .zo */
    /* otherwise, only happens with cycles: */
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                     "datum->syntax: cannot create syntax from cyclic datum: %V",
                     o);
    return NULL;
  }

  if (code) {
    scheme_unmarshal_wrap_set(ut, code, v);
  }

  if (copy_props > 0)
    ((Scheme_Stx *)v)->props = ((Scheme_Stx *)stx_src)->props;

  if (copy_props && (copy_props != 1)) {
    if (ACTIVE_CERTS(((Scheme_Stx *)stx_src)))
      v = add_certs(v, ACTIVE_CERTS((Scheme_Stx *)stx_src), NULL, 1);
    if (INACTIVE_CERTS((Scheme_Stx *)stx_src)) {
      v = lift_inactive_certs(v, 0);
      v = add_certs(v, INACTIVE_CERTS((Scheme_Stx *)stx_src), NULL, 0);
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
/*                              simplify                                  */
/*========================================================================*/

#ifdef DO_STACK_CHECK
static void simplify_syntax_inner(Scheme_Object *o,
				  Scheme_Hash_Table *rns,
				  Scheme_Hash_Table *marks);

static Scheme_Object *simplify_syntax_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table *rns = (Scheme_Hash_Table *)p->ku.k.p2;
  Scheme_Hash_Table *marks = (Scheme_Hash_Table *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  simplify_syntax_inner(o, rns, marks);

  return NULL;
}
#endif

static void simplify_syntax_inner(Scheme_Object *o, 
				  Scheme_Hash_Table *rns, 
				  Scheme_Hash_Table *marks)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *v;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)o;
      p->ku.k.p2 = (void *)rns;
      p->ku.k.p3 = (void *)marks;
      scheme_handle_stack_overflow(simplify_syntax_k);
      return;
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  /* Propagate wraps: */
  scheme_stx_content((Scheme_Object *)stx);

  if (rns) {
    v = wraps_to_datum(stx->val, stx->wraps, NULL, rns, 1);
    stx->wraps = v;
  }

  if (stx->certs && !marks)
    marks = scheme_make_hash_table(SCHEME_hash_ptr);

  v = stx->val;
  
  if (SCHEME_PAIRP(v)) {
    while (SCHEME_PAIRP(v)) {
      simplify_syntax_inner(SCHEME_CAR(v), rns, marks);
      v = SCHEME_CDR(v);
    }
    if (!SCHEME_NULLP(v)) {
      simplify_syntax_inner(v, rns, marks);
    }
  } else if (SCHEME_BOXP(v)) {
    simplify_syntax_inner(SCHEME_BOX_VAL(v), rns, marks);
  } else if (SCHEME_VECTORP(v)) {
    int size = SCHEME_VEC_SIZE(v), i;
    
    for (i = 0; i < size; i++) {
      simplify_syntax_inner(SCHEME_VEC_ELS(v)[i], rns, marks);
    }
  } else if (SCHEME_HASHTRP(v)) {
    Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)v;
    Scheme_Object *key, *val;
    int i;
    
    i = scheme_hash_tree_next(ht, -1);
    while (i != -1) {
      scheme_hash_tree_index(ht, i, &key, &val);
      simplify_syntax_inner(val, rns, marks);
      i = scheme_hash_tree_next(ht, i);
    }
  } else if (prefab_p(v)) {
    Scheme_Structure *s = (Scheme_Structure *)v;
    int size = s->stype->num_slots, i;
    
    for (i = 0; i < size; i++) {
      simplify_syntax_inner(s->slots[i], rns, marks);
    }
  }

  if (marks)
    add_all_marks(stx->wraps, marks);

  /* Pare certs based on marks that are actually used,
     and eliminate redundant certs. */
  if (stx->certs) {
    Scheme_Cert *orig_certs, *certs, *cl, *all_used_after, *result;
    int i;
    for (i = 0; i < 2; i++) {
      if (!i)
	certs = ACTIVE_CERTS(stx);
      else
	certs = INACTIVE_CERTS(stx);
      orig_certs = certs;
      /* Is there a tail where all certs are used? */
      all_used_after = certs;
      for (cl = certs; cl; cl = cl->next) {
	if (!scheme_hash_get(marks, cl->mark))
	  all_used_after = cl->next;
      }
      /* In the all-used tail, are any redundant? */
      for (cl = all_used_after; cl; cl = cl->next) {
	v = scheme_hash_get(marks, cl->mark);
	if (SCHEME_VOIDP(v)) {
	  /* Reset marks, because we're giving up on all_used_after */
	  result = cl;
	  for (cl = all_used_after; cl != result; cl = cl->next) {
	    scheme_hash_set(marks, cl->mark, scheme_true);
	  }
	  all_used_after = NULL;
	  break;
	}
	scheme_hash_set(marks, cl->mark, scheme_void);
      }
      /* If any marks are unused or redundant, then all_used_after will
	 have been changed. Also, every mark in all_used_after is mapped
	 to void instead of true in the marks hash table. */
      if (all_used_after != certs) {
	/* We can simplify... */
	result = all_used_after;
	for (cl = orig_certs; cl; cl = cl->next) {
	  if (SAME_OBJ(cl, all_used_after))
	    break;
	  if (scheme_hash_get(marks, cl->mark)) {
	    v = scheme_hash_get(marks, cl->mark);
	    if (!SCHEME_VOIDP(v))
	      result = cons_cert(cl->mark, cl->modidx, cl->insp, cl->key, result);
	  }
	}
	if (!i) {
	  if (SCHEME_RPAIRP(stx->certs)) {
	    Scheme_Object *pr;
	    pr = scheme_make_raw_pair((Scheme_Object *)result, SCHEME_CDR(stx->certs));
	    stx->certs = pr;
	  } else
	    stx->certs = (Scheme_Object *)result;
	} else {
	  if (!result)
	    stx->certs = SCHEME_CAR(stx->certs);
	  else {
	    Scheme_Object *pr;
	    pr = scheme_make_raw_pair(SCHEME_CAR(stx->certs), (Scheme_Object *)result);
	    stx->certs = pr;
	  }
	}
      } 
      /* Reset mark map from void to true: */
      for (cl = all_used_after; cl; cl = cl->next) {
	scheme_hash_set(marks, cl->mark, scheme_true);
      }
    }
  }
}

Scheme_Object *scheme_new_stx_simplify_cache()
{
  return (Scheme_Object *)scheme_make_hash_table(SCHEME_hash_ptr);
}

void scheme_simplify_stx(Scheme_Object *stx, Scheme_Object *cache)
{
#if 0
  if (SAME_OBJ(scheme_intern_symbol("y"), SCHEME_STX_VAL(stx))) {
    fprintf(stderr,
            "simplifying... %s\n",
            scheme_write_to_string(resolve_env(NULL, stx, 0, 0, NULL, NULL, NULL, NULL, 0, NULL),
                                   NULL));
    explain_simp = 1;
  }
#endif

  if (cache) {
    Scheme_Hash_Table *rns;

    rns = (Scheme_Hash_Table *)cache;

    simplify_syntax_inner(stx, rns, NULL);
  }

#if 0
  if (explain_simp) {
    explain_simp = 0;
    fprintf(stderr, "simplified: %s\n",
            scheme_write_to_string(resolve_env(NULL, stx, 0, 0, NULL, NULL, NULL, NULL, 0, NULL),
                                   NULL));
  }
#endif
}

/*========================================================================*/
/*                    Scheme functions and helpers                        */
/*========================================================================*/

static Scheme_Object *syntax_p(int argc, Scheme_Object **argv)
{
  return SCHEME_STXP(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *syntax_to_datum(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax->datum", "syntax", 0, argc, argv);
    
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
  Scheme_Object *src = scheme_false, *properties = NULL, *certs = NULL;
  
  if (!SCHEME_FALSEP(argv[0]) && !SCHEME_STXP(argv[0]))
    scheme_wrong_type("datum->syntax", "syntax or #f", 0, argc, argv);
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
	  scheme_wrong_type("datum->syntax", "syntax or #f", 3, argc, argv);
	properties = ((Scheme_Stx *)argv[3])->props;
      }
      
      if (argc > 4) {
        if (!SCHEME_FALSEP(argv[4])) {
          if (!SCHEME_STXP(argv[4]))
            scheme_wrong_type("datum->syntax", "syntax or #f", 4, argc, argv);
          certs = (Scheme_Object *)INACTIVE_CERTS((Scheme_Stx *)argv[4]);
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
	scheme_arg_mismatch("datum->syntax", 
			    "line and column positions must both be numbers or #f in: ", 
			    argv[2]);

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

  if (certs)
    src = add_certs(src, (Scheme_Cert *)certs, NULL, 0);    

  return src;
}


Scheme_Object *scheme_checked_syntax_e(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-e", "syntax", 0, argc, argv);
    
  return scheme_stx_content(argv[0]);
}

static Scheme_Object *syntax_line(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-line", "syntax", 0, argc, argv);
    
  if (stx->srcloc->line < 0)
    return scheme_false;
  else
    return scheme_make_integer(stx->srcloc->line);
}

static Scheme_Object *syntax_col(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-column", "syntax", 0, argc, argv);
    
  if (stx->srcloc->col < 0)
    return scheme_false;
  else
    return scheme_make_integer(stx->srcloc->col-1);
}

static Scheme_Object *syntax_pos(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-position", "syntax", 0, argc, argv);
    
  if (stx->srcloc->pos < 0)
    return scheme_false;
  else
    return scheme_make_integer(stx->srcloc->pos);
}

static Scheme_Object *syntax_span(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-span", "syntax", 0, argc, argv);
    
  if (stx->srcloc->span < 0)
    return scheme_false;
  else
    return scheme_make_integer(stx->srcloc->span);
}

static Scheme_Object *syntax_src(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-source", "syntax", 0, argc, argv);

  return stx->srcloc->src;
}

static Scheme_Object *syntax_to_list(int argc, Scheme_Object **argv)
{
  Scheme_Object *l;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax->list", "syntax", 0, argc, argv);

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

static Scheme_Object *syntax_original_p(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx;
  WRAP_POS awl;
  WRAP_POS ewl;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-original?", "syntax", 0, argc, argv);

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
    Scheme_Object *wraps, *modinfo_cache;
    Scheme_Object *certs;
    long lazy_prefix;

    l = CONS(CONS(key, val), l);

    wraps = stx->wraps;
    if (STX_KEY(stx) & STX_SUBSTX_FLAG) {
      modinfo_cache = NULL;
      lazy_prefix = stx->u.lazy_prefix;
    } else {
      modinfo_cache = stx->u.modinfo_cache;
      lazy_prefix = 0;
    }
    certs = stx->certs;

    stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, l);

    stx->wraps = wraps;
    if (modinfo_cache)
      stx->u.modinfo_cache = modinfo_cache;
    else
      stx->u.lazy_prefix = lazy_prefix; /* same as NULL modinfo if no SUBSTX */
    stx->certs = certs;

    return (Scheme_Object *)stx;
  } else
    return scheme_false;
}

static Scheme_Object *syntax_property(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-property", "syntax", 0, argc, argv);

  return scheme_stx_property(argv[0],
			     argv[1],
			     (argc > 2) ? argv[2] : NULL);
}

static Scheme_Object *syntax_property_keys(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-property-symbol-keys", "syntax", 0, argc, argv);

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
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-track-origin", "syntax", 0, argc, argv);
  if (!SCHEME_STXP(argv[1]))
    scheme_wrong_type("syntax-track-origin", "syntax", 1, argc, argv);
  if (!SCHEME_STX_IDP(argv[2]))
    scheme_wrong_type("syntax-track-origin", "identifier syntax", 2, argc, argv);
  
  return scheme_stx_track(argv[0], argv[1], argv[2]);
}

static Scheme_Object *delta_introducer(int argc, struct Scheme_Object *argv[], Scheme_Object *p)
{
  Scheme_Object *r, *delta;

  r = argv[0];

  if (!SCHEME_STXP(r))
    scheme_wrong_type("delta-introducer", "syntax", 0, argc, argv);

  delta = SCHEME_PRIM_CLOSURE_ELS(p)[0];

  for(; !SCHEME_NULLP(delta); delta = SCHEME_CDR(delta)) {
    r = scheme_add_remove_mark(r, SCHEME_CAR(delta));
  }

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
      scheme_wrong_type(who, "exact integer or #f", pos, argc, argv);
  } else {
    Scheme_Thread *p = scheme_current_thread;
    long ph;
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
  Scheme_Object *orig_m1, *m1, *m2, *delta, *a[1];
  int l1, l2;
  Scheme_Object *phase;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(argv[0])))
    scheme_wrong_type("make-syntax-delta-introducer", "syntax identifier", 0, argc, argv);
  if (!SCHEME_STXP(argv[1]) && !SCHEME_FALSEP(argv[1]))
    scheme_wrong_type("make-syntax-delta-introducer", "syntax or #f", 1, argc, argv);

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

    mod = resolve_env(NULL, argv[0], phase, 1, NULL, NULL, &skipped, NULL, 0, 
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

  return scheme_make_prim_closure_w_arity(delta_introducer, 1, a, "delta-introducer", 1, 1);
}

static Scheme_Object *bound_eq(int argc, Scheme_Object **argv)
{
  Scheme_Object *phase;

  if (!SCHEME_STX_IDP(argv[0]))
    scheme_wrong_type("bound-identifier=?", "identifier syntax", 0, argc, argv);
  if (!SCHEME_STX_IDP(argv[1]))
    scheme_wrong_type("bound-identifier=?", "identifier syntax", 1, argc, argv);

  phase = extract_phase("bound-identifier=?", 2, argc, argv, scheme_make_integer(0), 0);

  return (scheme_stx_bound_eq(argv[0], argv[1], phase)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *do_module_eq(const char *who, int delta, int argc, Scheme_Object **argv)
{
  Scheme_Object *phase;

  if (!SCHEME_STX_IDP(argv[0]))
    scheme_wrong_type(who, "identifier syntax", 0, argc, argv);
  if (!SCHEME_STX_IDP(argv[1]))
    scheme_wrong_type(who, "identifier syntax", 1, argc, argv);

  phase = extract_phase(who, 2, argc, argv, 
                        ((delta == MZ_LABEL_PHASE) 
                         ? scheme_false 
                         : scheme_make_integer(delta)),
                        0);

  return (scheme_stx_module_eq2(argv[0], argv[1], phase, NULL)
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

static Scheme_Object *do_module_binding(char *name, int argc, Scheme_Object **argv, Scheme_Object *dphase)
{
  Scheme_Object *a, *m, *nom_mod, *nom_a, *phase;
  Scheme_Object *src_phase_index, *mod_phase, *nominal_src_phase;

  a = argv[0];

  if (!SCHEME_STXP(a) || !SCHEME_STX_SYMBOLP(a))
    scheme_wrong_type(name, "identifier syntax", 0, argc, argv);

  phase = extract_phase(name, 1, argc, argv, dphase, 1);

  if (argc > 1) {
    phase = argv[1];
    if (!SCHEME_FALSEP(phase)
        && !SCHEME_INTP(phase)
        && !SCHEME_BIGNUMP(phase))
      scheme_wrong_type(name, "exact integer or #f", 1, argc, argv);
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
                             NULL);

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
  return do_module_binding("identifier-binding", argc, argv, scheme_make_integer(0));
}

static Scheme_Object *module_trans_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-transformer-binding", argc, argv, scheme_make_integer(1));
}

static Scheme_Object *module_templ_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-template-binding", argc, argv, scheme_make_integer(-1));
}

static Scheme_Object *module_label_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-label-binding", argc, argv, scheme_false);
}

static Scheme_Object *identifier_prune(int argc, Scheme_Object **argv)
{
  Scheme_Object *a = argv[0], *p, *l;

  if (!SCHEME_STXP(a) || !SCHEME_STX_SYMBOLP(a))
    scheme_wrong_type("identifier-prune-lexical-context", "identifier syntax", 0, argc, argv);

  if (argc > 1) {
    l = argv[1];
    while (SCHEME_PAIRP(l)) {
      if (!SCHEME_SYMBOLP(SCHEME_CAR(l)))
        break;
      l = SCHEME_CDR(l);
    }
    if (!SCHEME_NULLP(l))
      scheme_wrong_type("identifier-prune-lexical-context", "list of symbols", 1, argc, argv);
    l = argv[1];
  } else {
    l = scheme_make_pair(SCHEME_STX_VAL(a), scheme_null);
  }

  p = make_prune_context(l);

  return scheme_add_rename(a, p);
}

static Scheme_Object *syntax_src_module(int argc, Scheme_Object **argv)
{
  int source = 0;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-source-module", "syntax", 0, argc, argv);

  if ((argc > 1) && SCHEME_TRUEP(argv[1]))
    source = 1;

  return scheme_stx_source_module(argv[0], source, source);
}

/**********************************************************************/

static Scheme_Object *syntax_recertify(int argc, Scheme_Object **argv)
{
  Scheme_Object *insp, *key;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-recertify", "syntax", 0, argc, argv);
  if (!SCHEME_STXP(argv[1]))
    scheme_wrong_type("syntax-recertify", "syntax", 1, argc, argv);
  if (SCHEME_TRUEP(argv[2]) && !SAME_TYPE(SCHEME_TYPE(argv[2]), scheme_inspector_type))
    scheme_wrong_type("syntax-recertify", "inspector or #f", 2, argc, argv);
  
  if (SAME_OBJ(argv[0], argv[1]))
    return argv[0];

  insp = argv[2];
  if (SCHEME_FALSEP(insp))
    insp = NULL;
  key = argv[3];

  if (((Scheme_Stx *)argv[1])->certs) {
    Scheme_Stx *stx, *res;
    Scheme_Cert *certs, *new_certs, *orig_certs;
    int i;
    
    stx = (Scheme_Stx *)argv[0];
    
    for (i = 0; i < 2; i++) {
      if (!i) {
	certs = ACTIVE_CERTS((Scheme_Stx *)argv[1]);
	new_certs = ACTIVE_CERTS(stx);
      } else {
	certs = INACTIVE_CERTS((Scheme_Stx *)argv[1]);
	new_certs = INACTIVE_CERTS(stx);
      }

      orig_certs = new_certs;

      while (certs) {
	if (!SAME_OBJ(certs->key, key) 
	    && !SAME_OBJ(certs->insp, insp) 
	    && (!insp || !scheme_is_subinspector(certs->insp, insp))) {
	  /* Drop opaque certification. */
	} else {
	  if (!cert_in_chain(certs->mark, certs->key, new_certs))
	    new_certs = cons_cert(certs->mark, certs->modidx, certs->insp, certs->key, new_certs);
	}
	certs = certs->next;
      }
      
      if (!SAME_OBJ(orig_certs, new_certs)) {
        if (i && !orig_certs)
          stx = (Scheme_Stx *)lift_inactive_certs((Scheme_Object *)stx, 0);

	res = (Scheme_Stx *)scheme_make_stx(stx->val, 
					    stx->srcloc,
					    stx->props);
	res->wraps = stx->wraps;
	res->u.lazy_prefix = stx->u.lazy_prefix;

	if (!i && (!stx->certs || !SCHEME_RPAIRP(stx->certs) || !SCHEME_CDR(stx->certs)))
	  res->certs = (Scheme_Object *)new_certs;
	else {
	  Scheme_Object *pr;
	  if (!i)
	    pr = scheme_make_raw_pair((Scheme_Object *)new_certs, SCHEME_CDR(stx->certs));
	  else
	    pr = scheme_make_raw_pair((Scheme_Object *)ACTIVE_CERTS(stx), (Scheme_Object *)new_certs);
	  res->certs = pr;
	}
      
	stx = res;
      }
    }

    return (Scheme_Object *)stx;
  } else
    return argv[0];
}

/**********************************************************************/
/*                             Debugging                              */
/**********************************************************************/

static Scheme_Object *explode_cert_chain(Scheme_Cert *c, Scheme_Hash_Table *ht)
{
  Scheme_Object *first = scheme_null, *last = NULL, *pr, *vec;
  Scheme_Cert *next;
  int depth = c ? c->depth : 0;

  while (c) {
    next = c->next;
    pr = scheme_hash_get(ht, (Scheme_Object *)c);
    if (!pr) {
      vec = scheme_make_vector(3, NULL);
      SCHEME_VEC_ELS(vec)[0] = c->mark;
      SCHEME_VEC_ELS(vec)[1] = (c->modidx ? c->modidx : scheme_false);
      SCHEME_VEC_ELS(vec)[2] = (c->key ? c->key : scheme_false);
      pr = scheme_make_pair(vec, scheme_null);
      scheme_hash_set(ht, (Scheme_Object *)c, pr);
    } else
      next = NULL;
    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;
    c = next;
  }

  if (!SCHEME_NULLP(first)) {
    first = scheme_make_pair(scheme_make_integer(depth), first);
  }

  return first;
}

static Scheme_Object *explode_certs(Scheme_Stx *stx, Scheme_Hash_Table *ht)
{
  Scheme_Cert *a, *i;

  a = ACTIVE_CERTS(stx);
  i = INACTIVE_CERTS(stx);

  return scheme_make_pair(explode_cert_chain(a, ht),
                          explode_cert_chain(i, ht));
}

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

  v = explode_certs((Scheme_Stx *)stx, ht);
  SCHEME_VEC_ELS(vec)[1] = v;
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

static Scheme_Object *read_free_id_info_prefix(Scheme_Object *obj, Scheme_Object *insp)
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

  if (SCHEME_TRUEP(SCHEME_VEC_ELS(vec)[7]))
    SCHEME_VEC_ELS(vec)[7] = insp;

  vec->type = scheme_free_id_info_type;
    
  return vec;
}

/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_STXOBJ_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rename_table_type, mark_rename_table);
  GC_REG_TRAV(scheme_rename_table_set_type, mark_rename_table_set);
  GC_REG_TRAV(scheme_rt_srcloc, mark_srcloc);
  GC_REG_TRAV(scheme_wrap_chunk_type, mark_wrapchunk);
  GC_REG_TRAV(scheme_certifications_type, mark_cert);
  GC_REG_TRAV(scheme_lexical_rib_type, lex_rib);
  GC_REG_TRAV(scheme_free_id_info_type, mark_free_id_info);
}

END_XFORM_SKIP;

#endif
