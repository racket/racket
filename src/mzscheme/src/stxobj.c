/*
  MzScheme
  Copyright (c) 2004-2007 PLT Scheme Inc.
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

/* REMOVEME */
# define scheme_stx_placeholder_type scheme_multiple_values_type

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

#define STX_DEBUG 0

Scheme_Object *scheme_datum_to_syntax_proc;

static Scheme_Object *syntax_p(int argc, Scheme_Object **argv);
static Scheme_Object *graph_syntax_p(int argc, Scheme_Object **argv);

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
static Scheme_Object *free_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_trans_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_templ_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_label_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_binding(int argc, Scheme_Object **argv);
static Scheme_Object *module_trans_binding(int argc, Scheme_Object **argv);
static Scheme_Object *module_templ_binding(int argc, Scheme_Object **argv);
static Scheme_Object *module_label_binding(int argc, Scheme_Object **argv);
static Scheme_Object *module_binding_pos(int argc, Scheme_Object **argv);
static Scheme_Object *module_trans_binding_pos(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_src_module(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_recertify(int argc, Scheme_Object **argv);

static Scheme_Object *lift_inactive_certs(Scheme_Object *o, int as_active);

static Scheme_Object *source_symbol; /* uninterned! */
static Scheme_Object *share_symbol; /* uninterned! */
static Scheme_Object *origin_symbol;
static Scheme_Object *lexical_symbol;
static Scheme_Object *protected_symbol;

static Scheme_Object *nominal_ipair_cache;

static Scheme_Object *mark_id = scheme_make_integer(0);
static Scheme_Object *current_rib_timestamp = scheme_make_integer(0);

static Scheme_Stx_Srcloc *empty_srcloc;

static Scheme_Object *empty_simplified;

static Scheme_Hash_Table *empty_hash_table;

static Scheme_Object *last_phase_shift;

static Scheme_Hash_Table *id_marks_ht, *than_id_marks_ht;

static Scheme_Object *no_nested_inactive_certs;

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
#define ICONS scheme_make_immutable_pair

#define HAS_SUBSTX(obj) (SCHEME_PAIRP(obj) || SCHEME_VECTORP(obj) || SCHEME_BOXP(obj))

#define STX_KEY(stx) MZ_OPT_HASH_KEY(&(stx)->iso)

typedef struct Module_Renames {
  Scheme_Object so; /* scheme_rename_table_type */
  char plus_kernel, kind, needs_unmarshal;
  long phase;
  Scheme_Object *plus_kernel_nominal_source;
  Scheme_Hash_Table *ht; /* localname ->  modidx  OR
                                          (cons modidx exportname) OR
                                          (cons-immutable modidx nominal_modidx) OR
                                          (list* modidx exportname nominal_modidx nominal_exportname) OR
                                          (list* modidx mod-phase exportname nominal_modidx nominal_exportname) */
  Scheme_Hash_Table *nomarshal_ht; /* like ht, but dropped on marshal */
  Scheme_Hash_Table *marked_names; /* shared with module environment while compiling the module;
				      this table maps a top-level-bound identifier with a non-empty mark
				      set to a gensym created for the binding */
  Scheme_Object *unmarshal_info; /* stores some renamings as infomation needed to consult
				    imported modules and restore renames from their exports */
} Module_Renames;

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
    - cons(c1, c2): active certs c1 (maybe NULL), inactive certs c2 (maybe NULL); 
                    no inactive certs in nested parts */
#define ACTIVE_CERTS(stx) ((Scheme_Cert *)((stx)->certs ? (SCHEME_RPAIRP((stx)->certs) ? SCHEME_CAR((stx)->certs) : (stx)->certs) : NULL))
#define INACTIVE_CERTS(stx) ((Scheme_Cert *)((stx)->certs ? (SCHEME_RPAIRP((stx)->certs) ? SCHEME_CDR((stx)->certs) : NULL) : NULL))
static Scheme_Object *stx_activate_certs(Scheme_Object *o, Scheme_Cert **cp, Scheme_Hash_Table **ht);

#define SCHEME_RENAME_LEN(vec)  ((SCHEME_VEC_SIZE(vec) - 2) >> 1)

typedef struct Scheme_Lexical_Rib {
  Scheme_Object so;
  Scheme_Object *rename; /* a vector for a lexical rename */
  Scheme_Object *timestamp;
  struct Scheme_Lexical_Rib *next;
} Scheme_Lexical_Rib;

static Module_Renames *krn;

#define SCHEME_RENAMESP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_rename_table_type))

/* Wraps:

   A wrap is a list of wrap-elems and wrap-chunks. A wrap-chunk is a
   "vector" (a scheme_wrap_chunk_type) of wrap-elems.

   Each wrap-elem has one of several shapes:

   - A wrap-elem <+num> is a mark

   - A wrap-elem <-num> is a certificate-only mark (doesn't conttribute to
       id equivalence)

   - A wrap-elem (vector <sym> <ht> <stx> ... <sym-or-void> ...) is a lexical rename
                         env  (sym   var      var-resolved
                              ->pos)           void => not yet computed
                              or #f            sym => mark check done, 
                                                      var-resolved is answer to replace #f
   - A wrap-elem (vector <any> <ht> <sym> ... <sym> ...) is also a lexical rename
                                    var       resolved
         where the variables have already been resolved and filtered (no mark
         comparison needed with the remaining wraps)

   - A wrap-elem (make-rib vector rib)
         is an extensible set of lexical renames; it is the same as
         having the vectors inline in place of the rib, except that
         new vectors can be added imperatively; simplification turns this
	 into a vector

   - A wrap-elem <rename-table> is a module rename set
         the hash table maps renamed syms to modname-srcname pairs

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

  scheme_add_global_constant("syntax-graph?", 
			     scheme_make_folding_prim(graph_syntax_p,
						      "syntax-graph?",
						      1, 1, 1),
			     env);

  scheme_add_global_constant("syntax-object->datum", 
			     scheme_make_folding_prim(syntax_to_datum,
						      "syntax-object->datum",
						      1, 1 + STX_DEBUG, 1),
			     env);
  
  REGISTER_SO(scheme_datum_to_syntax_proc);
  scheme_datum_to_syntax_proc = scheme_make_folding_prim(datum_to_syntax,
							 "datum->syntax-object",
							 2, 5, 1);
  scheme_add_global_constant("datum->syntax-object", 
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
			     scheme_make_noncm_prim(syntax_original_p,
						    "syntax-original?",
						    1, 1),
			     env);
  scheme_add_global_constant("syntax-property", 
			     scheme_make_noncm_prim(syntax_property,
						    "syntax-property",
						    2, 3),
			     env);
  scheme_add_global_constant("syntax-property-symbol-keys", 
			     scheme_make_noncm_prim(syntax_property_keys,
						    "syntax-property-symbol-keys",
						    1, 1),
			     env);

  scheme_add_global_constant("syntax-track-origin", 
			     scheme_make_noncm_prim(syntax_track_origin,
						    "syntax-track-origin",
						    3, 3),
			     env);

  scheme_add_global_constant("bound-identifier=?", 
			     scheme_make_noncm_prim(bound_eq,
						    "bound-identifier=?",
						    2, 2),
			     env);
  scheme_add_global_constant("free-identifier=?", 
			     scheme_make_noncm_prim(free_eq,
						    "free-identifier=?",
						    2, 2),
			     env);
  scheme_add_global_constant("module-identifier=?", 
			     scheme_make_noncm_prim(module_eq,
						    "module-identifier=?",
						    2, 2),
			     env);
  scheme_add_global_constant("module-transformer-identifier=?", 
			     scheme_make_noncm_prim(module_trans_eq,
						    "module-transformer-identifier=?",
						    2, 2),
			     env);
  scheme_add_global_constant("module-template-identifier=?", 
			     scheme_make_noncm_prim(module_templ_eq,
						    "module-template-identifier=?",
						    2, 2),
			     env);
  scheme_add_global_constant("module-label-identifier=?", 
			     scheme_make_noncm_prim(module_label_eq,
						    "module-label-identifier=?",
						    2, 2),
			     env);

  scheme_add_global_constant("identifier-binding", 
			     scheme_make_noncm_prim(module_binding,
						    "identifier-binding",
						    1, 1),
			     env);
  scheme_add_global_constant("identifier-transformer-binding", 
			     scheme_make_noncm_prim(module_trans_binding,
						    "identifier-transformer-binding",
						    1, 2),
			     env);
  scheme_add_global_constant("identifier-template-binding", 
			     scheme_make_noncm_prim(module_templ_binding,
						    "identifier-template-binding",
						    1, 1),
			     env);
  scheme_add_global_constant("identifier-label-binding", 
			     scheme_make_noncm_prim(module_label_binding,
						    "identifier-label-binding",
						    1, 1),
			     env);

  scheme_add_global_constant("identifier-binding-export-position",
			     scheme_make_noncm_prim(module_binding_pos,
						    "identifier-binding-export-position",
						    1, 1),
			     env);
  scheme_add_global_constant("identifier-transformer-binding-export-position",
			     scheme_make_noncm_prim(module_trans_binding_pos,
						    "identifier-transformer-binding-export-position",
						    1, 1),
			     env);

  scheme_add_global_constant("syntax-source-module", 
			     scheme_make_folding_prim(syntax_src_module,
						      "syntax-source-module",
						      1, 1, 1),
			     env);

  scheme_add_global_constant("syntax-recertify", 
			     scheme_make_noncm_prim(syntax_recertify,
						    "syntax-recertify",
						    4, 4),
			     env);

  REGISTER_SO(source_symbol);
  REGISTER_SO(share_symbol);
  REGISTER_SO(origin_symbol);
  REGISTER_SO(lexical_symbol);
  REGISTER_SO(protected_symbol);
  source_symbol = scheme_make_symbol("source"); /* not interned! */
  share_symbol = scheme_make_symbol("share"); /* not interned! */
  origin_symbol = scheme_intern_symbol("origin");
  lexical_symbol = scheme_intern_symbol("lexical");
  protected_symbol = scheme_intern_symbol("protected");

  REGISTER_SO(mark_id);

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

  REGISTER_SO(nominal_ipair_cache);

  REGISTER_SO(last_phase_shift);

  REGISTER_SO(empty_hash_table);
  empty_hash_table = scheme_make_hash_table(SCHEME_hash_ptr);

  REGISTER_SO(id_marks_ht);
  REGISTER_SO(than_id_marks_ht);

  REGISTER_SO(no_nested_inactive_certs);
  no_nested_inactive_certs = scheme_make_raw_pair(NULL, NULL);
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

Scheme_Object *scheme_make_graph_stx(Scheme_Object *stx, long line, long col, long pos)
     /* Sets the "is graph" flag */
{
  Scheme_Object *tmp, *key;

  STX_KEY((Scheme_Stx *)stx) |= STX_GRAPH_FLAG;
  
  /* Add back-pointing property to track sharing 
     independent of marks. */
  key = scheme_new_mark();
  tmp = scheme_stx_property(stx, share_symbol, key);
  ((Scheme_Stx *)stx)->props = ((Scheme_Stx *)tmp)->props;

  return stx;
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
  int graph;

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

  graph = (STX_KEY(nstx) & STX_GRAPH_FLAG);

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

  if (graph)
    STX_KEY(nstx) |= STX_GRAPH_FLAG;

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
      /* ok to skip, but don't count toward needing a cache */
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
  int graph;

  graph = (STX_KEY(stx) & STX_GRAPH_FLAG);

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

  if (graph)
    STX_KEY(stx) |= STX_GRAPH_FLAG;

  return (Scheme_Object *)stx;
}

/******************** lexical renames ********************/

Scheme_Object *scheme_make_rename(Scheme_Object *newname, int c)
{
  Scheme_Object *v;
  int i;

  v = scheme_make_vector((2 * c) + 2, NULL);
  SCHEME_VEC_ELS(v)[0] = newname;
  if (c > 15) {
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

  rib = MALLOC_ONE_TAGGED(Scheme_Lexical_Rib);
  rib->so.type = scheme_lexical_rib_type;
  rib->timestamp = current_rib_timestamp;

  current_rib_timestamp = scheme_add1(1, &current_rib_timestamp);

  return (Scheme_Object *)rib;
}

void scheme_add_rib_rename(Scheme_Object *ro, Scheme_Object *rename)
{
  Scheme_Lexical_Rib *rib, *naya;

  naya = MALLOC_ONE_TAGGED(Scheme_Lexical_Rib);
  naya->so.type = scheme_lexical_rib_type;
  naya->rename = rename;

  rib = (Scheme_Lexical_Rib *)ro;
  naya->next = rib->next;
  rib->next = naya;

  naya->timestamp = rib->timestamp;
}

void scheme_drop_first_rib_rename(Scheme_Object *ro)
{
  Scheme_Lexical_Rib *rib = (Scheme_Lexical_Rib *)ro;
  rib->next = rib->next->next;
}

/******************** module renames ********************/

Scheme_Object *scheme_make_module_rename(long phase, int kind, Scheme_Hash_Table *marked_names)
{
  Module_Renames *mr;
  Scheme_Hash_Table *ht;

  mr = MALLOC_ONE_TAGGED(Module_Renames);
  mr->so.type = scheme_rename_table_type;

  ht = scheme_make_hash_table(SCHEME_hash_ptr);

  mr->ht = ht;
  mr->phase = phase;
  mr->kind = kind;
  mr->marked_names = marked_names;
  mr->unmarshal_info = scheme_null;

  if (!krn) {
    REGISTER_SO(krn);
    krn = mr;
  }

  return (Scheme_Object *)mr;
}

void scheme_extend_module_rename_with_kernel(Scheme_Object *mrn, Scheme_Object *nominal_mod)
{
  /* Don't use on a non-module namespace, where renames may need
     to be removed... */
  ((Module_Renames *)mrn)->plus_kernel = 1;
  ((Module_Renames *)mrn)->plus_kernel_nominal_source = nominal_mod;
}

void scheme_extend_module_rename(Scheme_Object *mrn,
				 Scheme_Object *modname,     /* actual source module */
				 Scheme_Object *localname,   /* name in local context */
				 Scheme_Object *exname,      /* name in definition context  */
				 Scheme_Object *nominal_mod, /* nominal source module */
				 Scheme_Object *nominal_ex,  /* nominal import before local renaming */
				 int mod_phase,              /* phase of source defn */
				 int unmarshal_drop)         /* 1 => can be reconstructed from unmarshal info */
{
  Scheme_Object *elem;

  if (SAME_OBJ(modname, nominal_mod)
      && SAME_OBJ(exname, nominal_ex)
      && !mod_phase) {
    if (SAME_OBJ(localname, exname))
      elem = modname;
    else
      elem = CONS(modname, exname);
  } else if (SAME_OBJ(exname, nominal_ex)
	     && SAME_OBJ(localname, exname)
	     && !mod_phase) {
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
    elem = CONS(exname, CONS(nominal_mod, nominal_ex));
    if (mod_phase)
      elem = CONS(scheme_make_integer(mod_phase), elem);
    elem = CONS(modname, elem);
  }
  
  if (unmarshal_drop) {
    if (!((Module_Renames *)mrn)->nomarshal_ht) {
      Scheme_Hash_Table *ht;
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      ((Module_Renames *)mrn)->nomarshal_ht = ht;
    }
    scheme_hash_set(((Module_Renames *)mrn)->nomarshal_ht, localname, elem);
  } else
    scheme_hash_set(((Module_Renames *)mrn)->ht, localname, elem);
}

void scheme_save_module_rename_unmarshal(Scheme_Object *rn, Scheme_Object *info)
{
  Scheme_Object *l;

  l = scheme_make_pair(info, ((Module_Renames *)rn)->unmarshal_info);
  ((Module_Renames *)rn)->unmarshal_info = l;
}

static void do_append_module_rename(Scheme_Object *src, Scheme_Object *dest,
				    Scheme_Object *old_midx, Scheme_Object *new_midx)
{
  Scheme_Hash_Table *ht, *hts, *drop_ht;
  Scheme_Object *v;
  int i, t;

  if (((Module_Renames *)src)->plus_kernel) {
    ((Module_Renames *)dest)->plus_kernel = 1;
    ((Module_Renames *)dest)->plus_kernel_nominal_source = ((Module_Renames *)src)->plus_kernel_nominal_source;
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
	  /* Shift the modidx part */
	  if (SCHEME_PAIRP(v)) {
	    if (SCHEME_PAIRP(SCHEME_CDR(v))) {
	      /* (list* modidx [mod-phase] exportname nominal_modidx nominal_exportname) */
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
	      midx2 = scheme_modidx_shift(midx2, old_midx, new_midx);
	      v = CONS(SCHEME_CAR(v), CONS(midx2, SCHEME_CDR(SCHEME_CDR(v))));
	      if (mod_phase)
		v = CONS(scheme_make_integer(mod_phase), v);
	      v = CONS(midx1, v);
	    } else if (SCHEME_IMMUTABLEP(v)) {
	      /* (cons-immutable modidx nominal_modidx) */
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

void scheme_append_module_rename(Scheme_Object *src, Scheme_Object *dest)
{
  do_append_module_rename(src, dest, NULL, NULL);
}

void scheme_remove_module_rename(Scheme_Object *mrn,
				 Scheme_Object *localname)
{
  scheme_hash_set(((Module_Renames *)mrn)->ht, localname, NULL);
  if (((Module_Renames *)mrn)->nomarshal_ht)
    scheme_hash_set(((Module_Renames *)mrn)->nomarshal_ht, localname, NULL);
}

void scheme_list_module_rename(Scheme_Object *src, Scheme_Hash_Table *ht)
{
  /* Put every name mapped by src into ht: */
  Scheme_Hash_Table *hts;
  int i, t;

  for (t = 0; t < 2; t++) {
    if (!t)
      hts = ((Module_Renames *)src)->ht;
    else {
      hts = ((Module_Renames *)src)->nomarshal_ht;
      if (!hts)
	break;
    }
   
    for (i = hts->size; i--; ) {
      if (hts->vals[i]) {
	scheme_hash_set(ht, hts->keys[i], scheme_false);
      }
    }
  }

  if (((Module_Renames *)src)->plus_kernel) {
    scheme_list_module_rename((Scheme_Object *)krn, ht);
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
  Scheme_Object *wraps;
  wraps = ((Scheme_Stx *)stx)->wraps;
  return SCHEME_CAR(wraps);
}

Scheme_Object *scheme_stx_shift_rename(Scheme_Object *mrn, 
				       Scheme_Object *old_midx, Scheme_Object *new_midx)
{
  Scheme_Object *nmrn, *a, *l, *nl;

  nmrn = scheme_make_module_rename(0, mzMOD_RENAME_NORMAL, NULL);
  do_append_module_rename(mrn, nmrn, old_midx, new_midx);

  /* Shift each mark_info: */
  l = ((Module_Renames *)mrn)->unmarshal_info;
  nl = scheme_null;
  while (!SCHEME_NULLP(l)) {
    a = SCHEME_CAR(l);
    nl = scheme_make_pair(scheme_make_pair(scheme_modidx_shift(SCHEME_CAR(a), old_midx, new_midx),
					   SCHEME_CDR(a)),
			  nl);
    l = SCHEME_CDR(l);
  }
  ((Module_Renames *)nmrn)->unmarshal_info = nl;

  return nmrn;
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

  mrn->needs_unmarshal = 0;
  for (l = mrn->unmarshal_info; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    scheme_do_module_rename_unmarshal((Scheme_Object *)mrn, SCHEME_CAR(l),
				      modidx_shift_from, modidx_shift_to,
				      export_registry);
  }
}

/******************** wrap manipulations ********************/

Scheme_Object *scheme_add_rename(Scheme_Object *o, Scheme_Object *rename)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *wraps;
  Scheme_Object *certs;
  long lp;
  int graph;

  if (STX_KEY(stx) & STX_SUBSTX_FLAG)
    preemptive_chunk(stx);

  /* relative order matters: chunk first, so that chunking
     doesn't immediately throw away a chain cache */

  maybe_add_chain_cache(stx);

  graph = (STX_KEY(stx) & STX_GRAPH_FLAG);

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

  if (graph)
    STX_KEY(stx) |= STX_GRAPH_FLAG;

  if (stx->certs)
    phase_shift_certs((Scheme_Object *)stx, stx->wraps, 1);
  
  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_delayed_rename(Scheme_Object **o, long i)
{
  Scheme_Object *rename;
  Resolve_Prefix *rp;

  rename = o[0];

  if (!rename) return scheme_false; /* happens only with corrupted .zo! */

  rp = (Resolve_Prefix *)o[1];

  if (SCHEME_INTP(rp->stxes[i])) {
    Scheme_Object *stx;
    stx = scheme_load_delayed_code(SCHEME_INT_VAL(rp->stxes[i]),
                                   rp->delay_info);
    rp->stxes[i] = stx;
    --rp->delay_refcount;
    if (!rp->delay_refcount)
      rp->delay_info = NULL;
  }

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

    /* Even if icerts is NULL, preserve the pair in ->certs, 
       to indicate no nested inactive certs. */

    if (icerts || SCHEME_RPAIRP(((Scheme_Stx *)o)->certs)) {
      nc = scheme_make_raw_pair((Scheme_Object *)acerts, (Scheme_Object *)icerts);
    } else
      nc = (Scheme_Object *)acerts;

    ((Scheme_Stx *)o)->certs = nc;
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
      int graph;

      graph = (STX_KEY(stx) & STX_GRAPH_FLAG);

      if (STX_KEY(stx) & STX_SUBSTX_FLAG)
	lp = stx->u.lazy_prefix + len;
      else
	lp = 0;

      certs = stx->certs;
      stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
      stx->wraps = owner_wraps;
      stx->u.lazy_prefix = lp; /* same as zeroing cache if no SUBSTX */
      stx->certs = certs;

      if (graph)
	STX_KEY(stx) |= STX_GRAPH_FLAG;

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
  Scheme_Cert *stop;
  Scheme_Object *pr;
  Scheme_Hash_Table *ht;

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

  if (cert->mapped)
    return;

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
  if (!a) return b;
  if (!b) return a;
  
  if (a->depth < b->depth) {
    Scheme_Cert *c = a;
    a = b;
    b = c;
  }

  for (; b; b = b->next) {
    if (!cert_in_chain(b->mark, b->key, a))
      a = cons_cert(b->mark, b->modidx, b->insp, b->key, a);
  }

  return a;
}

static Scheme_Object *add_certs(Scheme_Object *o, Scheme_Cert *certs, Scheme_Object *use_key, int active)
/* If !active, then inactive certs must have been lifted already. */
{
  Scheme_Cert *orig_certs, *cl, *now_certs, *next_certs;
  Scheme_Stx *stx = (Scheme_Stx *)o, *res;
  Scheme_Object *pr;
  int copy_on_write;

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

  copy_on_write = 1;
  if (active)
    orig_certs = ACTIVE_CERTS(stx);
  else
    orig_certs = INACTIVE_CERTS(stx);
  now_certs = orig_certs;

  for (; certs; certs = next_certs) {
    next_certs = certs->next;
    if (!cert_in_chain(certs->mark, use_key, now_certs)) {
      if (copy_on_write) {
	res = (Scheme_Stx *)scheme_make_stx(stx->val, 
					    stx->srcloc,
					    stx->props);
	res->wraps = stx->wraps;
	res->u.lazy_prefix = stx->u.lazy_prefix;
	if (!active) {
	  pr = scheme_make_raw_pair((Scheme_Object *)ACTIVE_CERTS(stx), (Scheme_Object *)orig_certs);
	  res->certs = pr;
	} else if (stx->certs && SCHEME_RPAIRP(stx->certs)) {
	  pr = scheme_make_raw_pair((Scheme_Object *)orig_certs, SCHEME_CDR(stx->certs));
	  res->certs = pr;
	} else
	  res->certs = (Scheme_Object *)orig_certs;
	stx = res;
	copy_on_write = 0;
      }
      if (!now_certs && !use_key && CERT_NO_KEY(certs)) {
        cl = certs;
        next_certs = NULL;
      } else {
        cl = cons_cert(certs->mark, certs->modidx, certs->insp, use_key, 
                       active ? ACTIVE_CERTS(stx) : INACTIVE_CERTS(stx));
      }
      now_certs = cl;
      if (!active) {
	SCHEME_CDR(stx->certs) = (Scheme_Object *)cl;
      } else if (stx->certs && SCHEME_RPAIRP(stx->certs))
	SCHEME_CAR(stx->certs) = (Scheme_Object *)cl;
      else
	stx->certs = (Scheme_Object *)cl;
    }
  }

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_stx_add_inactive_certs(Scheme_Object *o, Scheme_Object *certs)
  /* Also lifts existing inactive certs to the top. */
{
  /* Lift inactive certs*/
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
	If active and mark is non-NULL, make inactive certificates active. */
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
      } else
	res->certs = (Scheme_Object *)cert;
    } else {
      Scheme_Object *pr;
      pr = scheme_make_raw_pair((Scheme_Object *)ACTIVE_CERTS(stx), (Scheme_Object *)cert);
      res->certs = pr;
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
	p = scheme_make_immutable_pair(result, scheme_null);
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
    }

    stx->val = v;
  }

  return stx->val;
}

Scheme_Object *scheme_stx_extract_marks(Scheme_Object *stx)
/* Does not include negative marks */
{
  WRAP_POS awl;
  Scheme_Object *acur_mark, *first = scheme_null, *last = NULL, *p;

  WRAP_POS_INIT(awl, ((Scheme_Stx *)stx)->wraps);

  while (1) {
    /* Skip over renames, cancelled marks, and negative marks: */
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
      p = scheme_make_pair(acur_mark, scheme_null);
      if (!last)
	first = p;
      else
	SCHEME_CDR(last) = p;
      last = p;
    }

    if (WRAP_POS_END_P(awl))
      return first;
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
    if (SCHEME_RENAMESP(v) || SCHEME_BOXP(v)) {
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
      if (!SCHEME_RENAMESP(v) && !SCHEME_BOXP(v)) {
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
static Scheme_Object *stx_activate_certs_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Cert **cp = (Scheme_Cert **)p->ku.k.p2;
  Scheme_Hash_Table **ht = (Scheme_Hash_Table **)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return stx_activate_certs(o, cp, ht);
}
#endif

static Scheme_Object *stx_activate_certs(Scheme_Object *o, Scheme_Cert **cp, Scheme_Hash_Table **ht)
{
#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Cert **_cp;
      Scheme_Hash_Table **_ht;
      _cp = MALLOC_N(Scheme_Cert*, 1);
      _ht = MALLOC_N(Scheme_Hash_Table*, 1);
      *_cp = *cp;
      *_ht = *ht;
      p->ku.k.p1 = (void *)o;
      p->ku.k.p2 = (void *)_cp;
      p->ku.k.p3 = (void *)_ht;
      o = scheme_handle_stack_overflow(stx_activate_certs_k);
      *cp = *_cp;
      *ht = *_ht;
      return o;
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  if (SCHEME_PAIRP(o)) {
    Scheme_Object *a, *d;
    a = stx_activate_certs(SCHEME_CAR(o), cp, ht);
    d = stx_activate_certs(SCHEME_CDR(o), cp, ht);
    if (SAME_OBJ(a, SCHEME_CAR(o))
	&& SAME_OBJ(d, SCHEME_CDR(o)))
      return o;
    return ICONS(a, d);
  } else if (SCHEME_NULLP(o)) {
    return o;
  } else if (SCHEME_BOXP(o)) {
    Scheme_Object *c;
    c = stx_activate_certs(SCHEME_BOX_VAL(o), cp, ht);
    if (SAME_OBJ(c, SCHEME_BOX_VAL(o)))
      return o;
    o = scheme_box(c);
    SCHEME_SET_IMMUTABLE(o);
    return o;
  } else if (SCHEME_VECTORP(o)) {
    Scheme_Object *e = NULL, *v2;
    int size = SCHEME_VEC_SIZE(o), i, j;
    
    for (i = 0; i < size; i++) {
      e = stx_activate_certs(SCHEME_VEC_ELS(o)[i], cp, ht);
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
      e = stx_activate_certs(SCHEME_VEC_ELS(o)[i], cp, ht);
      SCHEME_VEC_ELS(v2)[i] = e;
    }

    SCHEME_SET_IMMUTABLE(v2);
    return v2;
  } else if (SCHEME_STXP(o)) {
    Scheme_Stx *stx = (Scheme_Stx *)o;

    if (INACTIVE_CERTS(stx)) {
      /* Change inactive certs to active certs. (No
	 sub-object has inactive certs, because they
	 are always lifted when inactive certs are added.) */
      Scheme_Object *np;
      Scheme_Stx *res;
      Scheme_Cert *certs;

      res = (Scheme_Stx *)scheme_make_stx(stx->val, 
					  stx->srcloc,
					  stx->props);
      res->wraps = stx->wraps;
      res->u.lazy_prefix = stx->u.lazy_prefix;
      np = scheme_make_raw_pair(SCHEME_CAR(stx->certs), NULL);
      res->certs = np;

      certs = append_certs(INACTIVE_CERTS(stx), *cp);
      *cp = certs;

      return (Scheme_Object *)res;
    } else if (stx->certs && SCHEME_RPAIRP(stx->certs)) {
      /* Explicit pair but NULL for inactive certs means no
	 inactive certs anywhere in this object. */
      return (Scheme_Object *)stx;
    } else {
      /* Before going to stx->val, we have to check
	 for cycles: */
      Scheme_Object *ph;
      Scheme_Object *key;

      if (STX_KEY(stx) & STX_GRAPH_FLAG) {
        /* FIXME: this is wrong */
	if (!*ht) {
	  GC_CAN_IGNORE Scheme_Hash_Table *htv;
	  htv = scheme_make_hash_table(SCHEME_hash_ptr);
	  *ht = htv;
	}
    
	key = scheme_stx_property((Scheme_Object *)stx, share_symbol, NULL);
	if (SCHEME_FALSEP(key)) {
	  scheme_signal_error("bad 'share key");
	}

	ph = scheme_hash_get(*ht, key);

	if (ph)
	  return ph;
	else {
	  ph = scheme_alloc_small_object();
	  ph->type = scheme_stx_placeholder_type;
      
	  scheme_hash_set(*ht, key, (Scheme_Object *)ph);
	}
      } else {
	ph = NULL;
	key = NULL;
      }

      o = stx_activate_certs(stx->val, cp, ht);

      if (!SAME_OBJ(o, stx->val)) {
	Scheme_Stx *res;
	res = (Scheme_Stx *)scheme_make_stx(o, 
					    stx->srcloc,
					    stx->props);
	res->wraps = stx->wraps;
	res->u.lazy_prefix = stx->u.lazy_prefix;
	/* stx->certs must not be a pair, otherwise we
	   would have taken an earlier branch; allocate
	   a pair with an explicitl NULL now to inidicate 
	   that there are no nested certs here */
	if (stx->certs) {
	  Scheme_Object *np;
	  np = scheme_make_raw_pair(stx->certs, NULL);
	  res->certs = np;
	} else
	  res->certs = no_nested_inactive_certs;

	if (ph) {
	  scheme_make_graph_stx((Scheme_Object *)res, -1, -1, -1);
	  SCHEME_PTR_VAL(ph) = (Scheme_Object *)res;
	}

	return (Scheme_Object *)res;
      } else {
	/* Record the absence of certificates in sub-parts: */
	if (stx->certs) {
	  Scheme_Object *np;
	  np = scheme_make_raw_pair(stx->certs, NULL);
	  stx->certs = np;
	} else
	  stx->certs = no_nested_inactive_certs;

	if (ph) {
	  /* Must not be a cycle, but may be shared. Avoid
	     using the placeholder. */
	  scheme_hash_set(*ht, key, (Scheme_Object *)stx);
	}
	return (Scheme_Object *)stx;
      }
    }
  } else
    return o;
}

static Scheme_Object *lift_inactive_certs(Scheme_Object *o, int as_active)
{
  Scheme_Cert *certs = NULL;
  Scheme_Hash_Table *ht = NULL;

  o = stx_activate_certs(o, &certs, &ht);

  if (certs)
    o = add_certs(o, certs, NULL, as_active);

  if (ht)
    o = scheme_resolve_placeholders(o, 1, scheme_stx_placeholder_type);

  return o;
}

Scheme_Object *scheme_stx_activate_certs(Scheme_Object *o)
{
  return lift_inactive_certs(o, 1);
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

XFORM_NONGCING static int same_marks(WRAP_POS *_awl, WRAP_POS *_bwl,
				     Scheme_Object *barrier_env, Scheme_Object *ignore_rib)
/* Compares the marks in two wraps lists. A result of 2 means that the
   result depended on a barrier env. Use #f for barrier_env
   to treat no rib envs as barriers; we check for barrier_env only in ribs
   because simpliciation eliminates the need for these checks(?). */
{
  WRAP_POS awl;
  WRAP_POS bwl;
  Scheme_Object *acur_mark, *bcur_mark;
  int used_barrier = 0;

  WRAP_POS_COPY(awl, *_awl);
  WRAP_POS_COPY(bwl, *_bwl);

  while (1) {
    /* Skip over renames and cancelled marks: */
    acur_mark = NULL;
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
	if (SAME_OBJ(ignore_rib, WRAP_POS_FIRST(awl))) {
	  WRAP_POS_INC(awl);
	} else if (SCHEME_FALSEP(barrier_env)) {
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
    bcur_mark = NULL;
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
	if (SAME_OBJ(ignore_rib, WRAP_POS_FIRST(bwl))) {
	  WRAP_POS_INC(bwl);
	} else if (SCHEME_FALSEP(barrier_env)) {
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

    /* Same mark? */
    if (!SAME_OBJ(acur_mark, bcur_mark))
      return 0;

    /* Done if both reached the end: */
    if (WRAP_POS_END_P(awl) && WRAP_POS_END_P(bwl))
      return used_barrier + 1;
  }
}

static int includes_mark(Scheme_Object *wraps, Scheme_Object *mark)
/* Checks for positive or negative (certificate-only) mark */
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
/* Adds both positive and negative marks to marks table */
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

#define QUICK_STACK_SIZE 10

#define EXPLAIN_RESOLVE 0
#if EXPLAIN_RESOLVE
static int explain_resolves = 0;
# define EXPLAIN(x) if (explain_resolves) { x; }
#else
# define EXPLAIN(x) /* empty */
#endif

/* Although resolve_env may call itself recursively, the recursion
   depth is bounded (by the fact that modules can't be nested,
   etc.). */

static Scheme_Object *resolve_env(WRAP_POS *_wraps,
				  Scheme_Object *a, long phase, 
				  int w_mod, Scheme_Object **get_names,
				  Scheme_Object *skip_ribs)
/* Module binding ignored if w_mod is 0.
   If module bound, result is module idx, and get_names[0] is set to source name,
     get_names[1] is set to the nominal source module, get_names[2] is set to
     the nominal source module's export, and get_names[3] is set to the phase of
     the source definition
   If lexically bound, result is env id, and a get_names[0] is set to scheme_undefined.
   If neither, result is #f and get_names[0] is either unchanged or NULL. */
{
  WRAP_POS wraps;
  Scheme_Object *o_rename_stack = scheme_null;
  Scheme_Object *mresult = scheme_false;
  Scheme_Object *modidx_shift_to = NULL, *modidx_shift_from = NULL;
  Scheme_Object *rename_stack[QUICK_STACK_SIZE];
  int stack_pos = 0, no_lexical = 0;
  int is_in_module = 0, skip_other_mods = 0;
  Scheme_Lexical_Rib *rib = NULL, *did_rib = NULL;
  long orig_phase = phase;
  Scheme_Object *bdg = NULL;
  Scheme_Hash_Table *export_registry = NULL;

  EXPLAIN(printf("Resolving %s:\n", SCHEME_SYM_VAL(SCHEME_STX_VAL(a))));

  if (_wraps) {
    WRAP_POS_COPY(wraps, *_wraps);
    WRAP_POS_INC(wraps);
  } else
    WRAP_POS_INIT(wraps, ((Scheme_Stx *)a)->wraps);
  
  while (1) {
    if (WRAP_POS_END_P(wraps)) {
      /* See rename case for info on rename_stack: */
      Scheme_Object *result, *key;
      int did_lexical = 0;

      EXPLAIN(printf("Rename...\n"));

      result = scheme_false;
      while (!SCHEME_NULLP(o_rename_stack)) {
	key = SCHEME_CAAR(o_rename_stack);
	if (SAME_OBJ(key, result)) {
          EXPLAIN(printf("Match %s\n", scheme_write_to_string(key, 0)));
	  did_lexical = 1;
	  result = SCHEME_CDR(SCHEME_CAR(o_rename_stack));
	} else {
          EXPLAIN(printf("No match %s\n", scheme_write_to_string(key, 0)));
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
          EXPLAIN(printf("Match %s\n", scheme_write_to_string(key, 0)));
	  result = rename_stack[stack_pos - 2];
	  did_lexical = 1;
	} else {
          EXPLAIN(printf("No match %s\n", scheme_write_to_string(key, 0)));
          if (SAME_OBJ(key, scheme_true)) {
            /* marks a module-level renaming that overrides lexical renaming */
            did_lexical = 0;
          }
        }
	stack_pos -= 2;
      }
      if (!did_lexical)
	result = mresult;
      else if (get_names)
	get_names[0] = scheme_undefined;

      EXPLAIN(printf("Result: %s\n", scheme_write_to_string(result, 0)));

      return result;
    } else if (SCHEME_RENAMESP(WRAP_POS_FIRST(wraps)) && w_mod) {
      /* Module rename: */
      Module_Renames *mrn = (Module_Renames *)WRAP_POS_FIRST(wraps);
      if ((!is_in_module || (mrn->kind != mzMOD_RENAME_TOPLEVEL)) && !skip_other_mods) {
	if (mrn->kind != mzMOD_RENAME_TOPLEVEL)
	  is_in_module = 1;
	
	if (phase == mrn->phase) {
	  Scheme_Object *rename, *nominal = NULL, *glob_id;

	  if (mrn->needs_unmarshal)
	    unmarshal_rename(mrn, modidx_shift_from, modidx_shift_to, export_registry);
	  
	  if (mrn->marked_names) {
	    /* Resolve based on rest of wraps: */
	    if (!bdg)
	      bdg = resolve_env(NULL, a, orig_phase, 0, NULL, skip_ribs);
	    /* Remap id based on marks and rest-of-wraps resolution: */
	    glob_id = scheme_tl_id_sym((Scheme_Env *)mrn->marked_names, a, bdg, 0);
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
	  } else
	    glob_id = SCHEME_STX_VAL(a);

	  rename = scheme_hash_get(mrn->ht, glob_id);
	  if (!rename && mrn->nomarshal_ht)
	    rename = scheme_hash_get(mrn->nomarshal_ht, glob_id);
	  if (!rename && mrn->plus_kernel) {
	    rename = scheme_hash_get(krn->ht, glob_id);
	    nominal = mrn->plus_kernel_nominal_source;
	  }
	  
	  if (rename) {
	    if (mrn->kind == mzMOD_RENAME_MARKED)
	      skip_other_mods = 1;

	    /* match; set mresult, which is used in the case of no lexical capture: */
	    if (SCHEME_PAIRP(rename))
	      mresult = SCHEME_CAR(rename);
	    else
	      mresult = rename;
	    
	    if (modidx_shift_from)
	      mresult = scheme_modidx_shift(mresult,
					    modidx_shift_from,
					    modidx_shift_to);

	    if (get_names) {
	      if (SCHEME_PAIRP(rename)) {
		if (SCHEME_IMMUTABLEP(rename)) {
		  /* (cons-immutable modidx nominal_modidx) case */
		  get_names[0] = glob_id;
		  get_names[1] = SCHEME_CDR(rename);
		  get_names[2] = get_names[0];
		} else {
		  rename = SCHEME_CDR(rename);
		  if (SCHEME_PAIRP(rename)) {
		    /* (list* modidx [mod-phase] exportname nominal_modidx nominal_exportname) case */
		    if (SCHEME_INTP(SCHEME_CAR(rename))) {
		      get_names[3] = SCHEME_CAR(rename);
		      rename = SCHEME_CDR(rename);
		    }
		    get_names[0] = SCHEME_CAR(rename);
		    get_names[1] = SCHEME_CADR(rename);
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
		else
		  get_names[1] = mresult;
	      }
	    }
	  } else {
	    mresult = scheme_false;
	    if (get_names)
	      get_names[0] = NULL;
	  }
	}
      }
    } else if (SCHEME_BOXP(WRAP_POS_FIRST(wraps)) && w_mod) {
      /* Phase shift */
      Scheme_Object *vec, *n, *dest, *src;
      vec = SCHEME_PTR_VAL(WRAP_POS_FIRST(wraps));
      n = SCHEME_VEC_ELS(vec)[0];
      phase -= SCHEME_INT_VAL(n);
     
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
      Scheme_Object *rename, *renamed, *recur_skip_ribs;
      int ri, c, istart, iend, is_rib;

      if (rib) {
	rename = rib->rename;
	recur_skip_ribs = rib->timestamp;
	rib = rib->next;
	is_rib = 1;
      } else {
	rename = WRAP_POS_FIRST(wraps);
	recur_skip_ribs = skip_ribs;
	is_rib = 0;
      }

      c = SCHEME_RENAME_LEN(rename);

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
	  int same;

	  {
	    Scheme_Object *other_env, *envname;

	    if (SCHEME_SYMBOLP(renamed)) {
	      /* Simplified table */
	      other_env = scheme_false;
	      envname = SCHEME_VEC_ELS(rename)[2+c+ri];
	      same = 1;
              EXPLAIN(printf("Targes %s <- %s\n", 
                             scheme_write_to_string(envname, 0),
                             scheme_write_to_string(other_env, 0)));
	    } else {
	      envname = SCHEME_VEC_ELS(rename)[0];
	      other_env = SCHEME_VEC_ELS(rename)[2+c+ri];
	    	      
	      if (SCHEME_VOIDP(other_env)) {
		SCHEME_USE_FUEL(1);
		other_env = resolve_env(NULL, renamed, 0, 0, NULL, recur_skip_ribs);
		if (!is_rib)
		  SCHEME_VEC_ELS(rename)[2+c+ri] = other_env;
		SCHEME_USE_FUEL(1);
	      }

              EXPLAIN(printf("Target %s <- %s (%d)\n", 
                             scheme_write_to_string(envname, 0),
                             scheme_write_to_string(other_env, 0),
                             SCHEME_IMMUTABLEP(rename)));

	      {
		WRAP_POS w2;
		WRAP_POS_INIT(w2, ((Scheme_Stx *)renamed)->wraps);
		same = same_marks(&w2, &wraps, other_env, WRAP_POS_FIRST(wraps));
                if (!same)
                  EXPLAIN(printf("Different marks\n"));
	      }
	    }
	    
	    if (same) {
	      /* If it turns out that we're going to return
		 other_env, then return envname instead. 
		 It's tempting to try to compare envname to the
		 top element of the stack and combine the two
		 mappings, but the intermediate name may be needed
		 (for other_env values that don't come from this stack). */
	      if (stack_pos < QUICK_STACK_SIZE) {
		rename_stack[stack_pos++] = envname;
		rename_stack[stack_pos++] = other_env;
	      } else {
		o_rename_stack = CONS(CONS(other_env, envname),
				      o_rename_stack);
	      }
	      rib = NULL; /* skip rest of rib (if any) */
	    }

	    break;
	  }
	}
      }
    } else if (SCHEME_RIBP(WRAP_POS_FIRST(wraps)) && !no_lexical) {
      /* Lexical-rename rib. Splice in the names. */
      rib = (Scheme_Lexical_Rib *)WRAP_POS_FIRST(wraps);
      EXPLAIN(printf("Rib: %p...\n", rib));
      if (skip_ribs) {
	if (scheme_bin_gt_eq(rib->timestamp, skip_ribs)) {
          EXPLAIN(printf("Skip rib\n"));
	  rib = NULL;
        }
      }
      if (rib) {
	if (SAME_OBJ(did_rib, rib)) {
          EXPLAIN(printf("Did rib\n"));
	  rib = NULL;
	} else {
	  did_rib = rib;
	  rib = rib->next; /* First rib record has no rename */
	}
      }
    } else if (SCHEME_NUMBERP(WRAP_POS_FIRST(wraps))) {
      did_rib = NULL;
    } else if (SCHEME_HASHTP(WRAP_POS_FIRST(wraps))) {
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)WRAP_POS_FIRST(wraps);

      did_rib = NULL;

      if (!ht->count 
	  /* Table isn't finished if 5 is mapped to a limit: */
	  || scheme_hash_get(ht, scheme_make_integer(5))) {
	fill_chain_cache(wraps.l);
      }

      if (!scheme_hash_get(ht, SCHEME_STX_VAL(a))) {
	set_wraps_to_skip(ht, &wraps);

	continue; /* <<<<< ------ */
      }
    }

    if (!rib)
      WRAP_POS_INC(wraps);
  }
}

static Scheme_Object *get_module_src_name(Scheme_Object *a, long phase)
     /* Gets a module source name under the assumption that the identifier
	is not lexically renamed. This is used as a quick pre-test for
	module-identifier=?. */
{
  WRAP_POS wraps;
  Scheme_Object *result;
  int is_in_module = 0, skip_other_mods = 0;
  long orig_phase = phase;
  Scheme_Object *bdg = NULL;

  if (((Scheme_Stx *)a)->u.modinfo_cache)
    return ((Scheme_Stx *)a)->u.modinfo_cache;

  WRAP_POS_INIT(wraps, ((Scheme_Stx *)a)->wraps);

  result = NULL;

  while (1) {
    if (WRAP_POS_END_P(wraps)) {
      if (!result)
	result = SCHEME_STX_VAL(a);

      ((Scheme_Stx *)a)->u.modinfo_cache = result;
 
      return result;
    } else if (SCHEME_RENAMESP(WRAP_POS_FIRST(wraps))) {
      Module_Renames *mrn = (Module_Renames *)WRAP_POS_FIRST(wraps);

      if ((!is_in_module || (mrn->kind != mzMOD_RENAME_TOPLEVEL)) && !skip_other_mods) {
	if (mrn->kind != mzMOD_RENAME_TOPLEVEL)
	  is_in_module = 1;
	
	if (phase == mrn->phase) {
	  /* Module rename: */
	  Scheme_Object *rename, *glob_id;

	  if (mrn->needs_unmarshal) {
	    /* Use resolve_env to trigger unmarshal, so that we
	       don't have to implement top/from shifts here: */
	    resolve_env(NULL, a, orig_phase, 1, NULL, NULL);
	  }

	  if (mrn->marked_names) {
	    /* Resolve based on rest of wraps: */
	    if (!bdg)
	      bdg = resolve_env(&wraps, a, orig_phase, 0, NULL, NULL);
	    /* Remap id based on marks and rest-of-wraps resolution: */
	    glob_id = scheme_tl_id_sym((Scheme_Env *)mrn->marked_names, a, bdg, 0);
	  } else
	    glob_id = SCHEME_STX_VAL(a);

	  rename = scheme_hash_get(mrn->ht, glob_id);
	  if (!rename && mrn->nomarshal_ht)
	    rename = scheme_hash_get(mrn->nomarshal_ht, glob_id);
	  if (!rename && mrn->plus_kernel)
	    rename = scheme_hash_get(krn->ht, glob_id);
	  
	  if (rename) {
	    /* match; set result: */
	    if (mrn->kind == mzMOD_RENAME_MARKED)
	      skip_other_mods = 1;
	    if (SCHEME_PAIRP(rename)) {
	      if (SCHEME_IMMUTABLEP(rename)) {
		result = glob_id;
	      } else {
		result = SCHEME_CDR(rename);
		if (SCHEME_PAIRP(result))
		  result = SCHEME_CAR(result);
	      }
	    } else
	      result = glob_id;
	  } else
	    result = NULL;
	}
      }
    } else if (SCHEME_BOXP(WRAP_POS_FIRST(wraps))) {
      /* Phase shift */
      Scheme_Object *n, *vec;
      vec = SCHEME_PTR_VAL(WRAP_POS_FIRST(wraps));
      n = SCHEME_VEC_ELS(vec)[0];
      phase -= SCHEME_INT_VAL(n);
    }
    
    /* Keep looking: */
    WRAP_POS_INC(wraps);
  }
}

int scheme_stx_free_eq(Scheme_Object *a, Scheme_Object *b, long phase)
{
  Scheme_Object *asym, *bsym;

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

  if ((a == asym) || (b == bsym))
    return 1;
  
  a = resolve_env(NULL, a, phase, 1, NULL, NULL);
  b = resolve_env(NULL, b, phase, 1, NULL, NULL);

  a = scheme_module_resolve(a, 0);
  b = scheme_module_resolve(b, 0);

  /* Same binding environment? */
  return SAME_OBJ(a, b);
}

int scheme_stx_module_eq(Scheme_Object *a, Scheme_Object *b, long phase)
{
  Scheme_Object *asym, *bsym;

  if (!a || !b)
    return (a == b);

  if (SCHEME_STXP(a))
    asym = get_module_src_name(a, phase);
  else
    asym = a;
  if (SCHEME_STXP(b))
    bsym = get_module_src_name(b, phase);
  else
    bsym = b;

  /* Same name? */
  if (!SAME_OBJ(asym, bsym))
    return 0;

  if ((a == asym) || (b == bsym))
    return 1;
  
  a = resolve_env(NULL, a, phase, 1, NULL, NULL);
  b = resolve_env(NULL, b, phase, 1, NULL, NULL);

  a = scheme_module_resolve(a, 0);
  b = scheme_module_resolve(b, 0);

  /* Same binding environment? */
  return SAME_OBJ(a, b);
}

Scheme_Object *scheme_stx_module_name(Scheme_Object **a, long phase, 
				      Scheme_Object **nominal_modidx,
				      Scheme_Object **nominal_name,
				      int *mod_phase)
     /* If module bound, result is module idx, and a is set to source name.
	If lexically bound, result is scheme_undefined and a is unchanged. 
	If neither, result is NULL and a is unchanged. */
{
  if (SCHEME_STXP(*a)) {
    Scheme_Object *modname, *names[4];

    names[0] = NULL;
    names[3] = scheme_make_integer(0);

    modname = resolve_env(NULL, *a, phase, 1, names, NULL);
    
    if (names[0]) {
      if (SAME_OBJ(names[0], scheme_undefined)) {
	return scheme_undefined;
      } else {
	*a = names[0];
	if (nominal_modidx)
	  *nominal_modidx = names[1];
	if (nominal_name)
	  *nominal_name = names[2];
	if (mod_phase)
	  *mod_phase = SCHEME_INT_VAL(names[3]);
	return modname;
      }
    } else
      return NULL;
  } else
    return NULL;
}

Scheme_Object *scheme_stx_moduleless_env(Scheme_Object *a, long phase)
  /* Returns either NULL or a lexical-rename symbol */
{
  if (SCHEME_STXP(a)) {
    Scheme_Object *r;

    r = resolve_env(NULL, a, phase, 0, NULL, NULL);

    if (r)
      return r;
  }
  return NULL;
}

int scheme_stx_env_bound_eq(Scheme_Object *a, Scheme_Object *b, Scheme_Object *uid, long phase)
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

  ae = resolve_env(NULL, a, phase, 0, NULL, NULL);
  /* No need to module_resolve ae, because we ignored module renamings. */

  if (uid)
    be = uid;
  else {
    be = resolve_env(NULL, b, phase, 0, NULL, NULL);
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
    if (!same_marks(&aw, &bw, ae, NULL))
      return 0;
  }

  return 1;
}

int scheme_stx_bound_eq(Scheme_Object *a, Scheme_Object *b, long phase)
{
  return scheme_stx_env_bound_eq(a, b, NULL, phase);
}

#if EXPLAIN_RESOLVE
Scheme_Object *scheme_explain_resolve_env(Scheme_Object *a)
{
  explain_resolves++;
  a = resolve_env(NULL, a, 0, 0, NULL, NULL);
  --explain_resolves;
  return a;
}
#endif

Scheme_Object *scheme_stx_source_module(Scheme_Object *stx, int resolve)
{
  /* Inspect the wraps to look for a self-modidx shift: */
  WRAP_POS w;
  Scheme_Object *srcmod = scheme_false, *chain_from = NULL;

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
      }
    }

    WRAP_POS_INC(w);
  }

  if (SCHEME_TRUEP(srcmod) && resolve)
    srcmod = scheme_module_resolve(srcmod, 0);

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

  if (!same_marks(&aw, &bw, NULL, NULL)) {
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
    p = scheme_make_immutable_pair(SCHEME_CAR(l), scheme_null);
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

static void simplify_lex_renames(Scheme_Object *wraps, Scheme_Hash_Table *lex_cache)
{
  WRAP_POS w;
  WRAP_POS prev;
  WRAP_POS w2;
  Scheme_Object *stack = scheme_null, *key, *old_key;
  Scheme_Object *v, *v2, *v2l, *stx, *name, *svl;
  long size, vsize, psize, i, j, pos;

  /* Although it makes no sense to simplify the rename table itself,
     we can simplify it in the context of a particular wrap suffix.
     (But don't mutate the wrap list, because that will stomp on
     tables that might be needed by a propoagation.)
     
     A lex_cache maps wrap starts w to simplified tables. A lex_cache
     is modified by this function, only. */

  WRAP_POS_INIT(w, wraps);
  WRAP_POS_INIT_END(prev);

  old_key = NULL;

  while (!WRAP_POS_END_P(w)) {
    if (SCHEME_VECTORP(WRAP_POS_FIRST(w))
	|| SCHEME_RIBP(WRAP_POS_FIRST(w))) {
      /* Lexical rename */
      key = WRAP_POS_KEY(w);
      if (!SAME_OBJ(key, old_key)) {
	v = scheme_hash_get(lex_cache, key);
      } else
	v = NULL;
      old_key = key;

      if (v) {
	/* Tables here are already simplified. */
	WRAP_POS_COPY(prev, w);
	/* No non-simplified table can follow a simplified one */
	break;
      } else {
	int add = 0;

	v = WRAP_POS_FIRST(w);
	if (SCHEME_RIBP(v)) {
	  /* A rib certainly isn't simplified yet. */
	  add = 1;
	} else {
	  /* Need to simplify this vector? */
	  if (SCHEME_VEC_SIZE(v) == 1)
	    v = SCHEME_VEC_ELS(v)[0];
	  if ((SCHEME_VEC_SIZE(v) > 2) /* a simplified vec can be empty */
	      && !SCHEME_SYMBOLP(SCHEME_VEC_ELS(v)[2])) {
	    add = 1;
	  }
	}

	if (add) {
	  /* Need to simplify, but do deepest first: */
	  if (SCHEME_NULLP(stack) || !SAME_OBJ(SCHEME_CAR(stack), key)) {
	    stack = CONS(key, stack);
	  }
	} else {
	  /* This is already simplified. Remember it and stop, because
	     no non-simplified table can follow a simplified one. */
	  if (WRAP_POS_END_P(prev))
	    WRAP_POS_COPY(prev, w);
	  break;
	}
      }
    }
    
    WRAP_POS_INC(w);
  }

  while (!SCHEME_NULLP(stack)) {
    key = SCHEME_CAR(stack);
    v2l = scheme_null;

    WRAP_POS_REVINIT(w, key);

    while (!WRAP_POS_REVEND_P(w)) {
      v = WRAP_POS_FIRST(w);
      
      if (SCHEME_RIBP(v)
	  || (SCHEME_VECTORP(v)
	      && (SCHEME_VEC_SIZE(v) > 2) /* a simplified vec can be empty */
	      && !SCHEME_SYMBOLP(SCHEME_VEC_ELS(v)[2]))) {
	/* This is the place to simplify: */
	Scheme_Lexical_Rib *rib = NULL, *init_rib = NULL;
	Scheme_Object *skip_ribs = NULL;
	int ii, vvsize;

	if (SCHEME_RIBP(v)) {
	  init_rib = (Scheme_Lexical_Rib *)v;
	  skip_ribs = init_rib->timestamp;
	  rib = init_rib->next;
	  vsize = 0;
	  while (rib) {
	    vsize += SCHEME_RENAME_LEN(rib->rename);
	    rib = rib->next;
	  }
	  rib = init_rib->next;
	} else
	  vsize = SCHEME_RENAME_LEN(v);

        /* Initial size; may shrink: */
	size = vsize;

	v2 = scheme_make_vector(2 + (2 * size), NULL);

	pos = 0; /* counter for used slots */

	/* Local vector (different from i when we have a rib) */
	ii = 0;
	vvsize= vsize;

	for (i = 0; i < vsize; i++) {
	  if (rib) {
	    v = rib->rename;
	    vvsize = SCHEME_RENAME_LEN(v);
	    while (ii >= vvsize) {
	      ii = 0;
	      rib = rib->next;
	      v = rib->rename;
	      vvsize = SCHEME_RENAME_LEN(v);
	    }
	  }
	  stx = SCHEME_VEC_ELS(v)[2+ii];
	  name = SCHEME_STX_VAL(stx);
	  SCHEME_VEC_ELS(v2)[2+pos] = name;

	  {
	    /* Either this name is in prev, in which case the answer
	       must match this rename's target, or this rename's
	       answer applies. */
	    Scheme_Object *ok = NULL, *ok_replace = NULL;
            int ok_replace_index = 0;

	    if (!WRAP_POS_END_P(prev)
                || SCHEME_PAIRP(v2l)) {
	      WRAP_POS w3;
	      Scheme_Object *vp;
	      Scheme_Object *other_env;

	      other_env = SCHEME_VEC_ELS(v)[2+vvsize+ii];
	      if (SCHEME_VOIDP(other_env)) {
		other_env = resolve_env(NULL, stx, 0, 0, NULL, skip_ribs);
		SCHEME_VEC_ELS(v)[2+vvsize+ii] = other_env;
	      }

              /* Check marks (now that we have the correct barriers). */
	      WRAP_POS_INIT(w2, ((Scheme_Stx *)stx)->wraps);
	      if (!same_marks(&w2, &w, other_env, (Scheme_Object *)init_rib)) {
		other_env = NULL;
	      }

              if (other_env) {
                /* First, check simplications in v2l.
                   If not in v2l, try prev. */
                if (!ok) {
                  WRAP_POS_COPY(w3, prev);
                  svl = v2l;
                  for (; SCHEME_PAIRP(svl) || !WRAP_POS_END_P(w3); ) {
                    if (SCHEME_PAIRP(svl))
                      vp = SCHEME_CAR(svl);
                    else
                      vp = WRAP_POS_FIRST(w3);
                    if (SCHEME_VECTORP(vp)) {
                      psize = SCHEME_RENAME_LEN(vp);
                      for (j = 0; j < psize; j++) {
                        if (SAME_OBJ(SCHEME_VEC_ELS(vp)[2+j], name)) {
                          if (SAME_OBJ(SCHEME_VEC_ELS(vp)[2+psize+j], other_env)) {
                            ok = SCHEME_VEC_ELS(v)[0];
                          } else {
                            ok = NULL; 
                            /* Alternate time/space tradeoff: could be
                               SCHEME_VEC_ELS(vp)[2+psize+j],
                               which is the value from prev */
                          }
                          if (ok && SCHEME_PAIRP(svl)) {
                            /* Need to overwrite old map, instead
                               of adding a new one. */
                            ok_replace = vp;
                            ok_replace_index = 2 + psize + j;
                          }
                          break;
                        }
                      }
                      if (j < psize)
                        break;
                    }
                    if (SCHEME_PAIRP(svl))
                      svl = SCHEME_CDR(svl);
                    else {
                      WRAP_POS_INC(w3);
                    }
                  }
                  if (WRAP_POS_END_P(w3) && SCHEME_NULLP(svl) && SCHEME_FALSEP(other_env))
                    ok = SCHEME_VEC_ELS(v)[0];
                } else
                  ok = NULL;
              }
	    } else {
	      WRAP_POS_INIT(w2, ((Scheme_Stx *)stx)->wraps);
	      if (same_marks(&w2, &w, scheme_false, (Scheme_Object *)init_rib))
                ok = SCHEME_VEC_ELS(v)[0];
	      else
		ok = NULL;
	    }

	    if (ok) {
              if (ok_replace) {
                SCHEME_VEC_ELS(ok_replace)[ok_replace_index] = ok;
              } else {
                SCHEME_VEC_ELS(v2)[2+size+pos] = ok;
                pos++;
              }
	    }
	  }
	  ii++;
	}

	if (pos != size) {
	  /* Shrink simplified vector */
	  if (!pos)
	    v2 = empty_simplified;
	  else {
	    v = v2;
	    v2 = scheme_make_vector(2 + (2 * pos), NULL);
	    for (i = 0; i < pos; i++) {
	      SCHEME_VEC_ELS(v2)[2+i] = SCHEME_VEC_ELS(v)[2+i];
	      SCHEME_VEC_ELS(v2)[2+pos+i] = SCHEME_VEC_ELS(v)[2+size+i];
	    }
	  }
	}

	SCHEME_VEC_ELS(v2)[0] = scheme_false;
	SCHEME_VEC_ELS(v2)[1] = scheme_false;

	v2l = CONS(v2, v2l);
      }

      WRAP_POS_DEC(w);
    }

    scheme_hash_set(lex_cache, key, v2l);

    stack = SCHEME_CDR(stack);
  }
}

static Scheme_Object *wraps_to_datum(Scheme_Object *w_in, 
				     Scheme_Marshal_Tables *mt,
                                     Scheme_Hash_Table *rns,
				     int just_simplify)
{
  Scheme_Object *stack, *a, *old_key, *simplifies = scheme_null;
  WRAP_POS w;
  Scheme_Hash_Table *lex_cache, *reverse_map;
  int stack_size = 0;

  if (!rns)
    rns = mt->rns;

  if (just_simplify) {
    a = scheme_hash_get(rns, w_in);
  } else {
    if (mt->pass && mt->same_map) {
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

  /* Ensures that all lexical tables in w have been simplified */
  simplify_lex_renames(w_in, lex_cache);

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
	  if (SCHEME_NULLP(simplifies)) {
	    simplifies = scheme_hash_get(lex_cache, old_key);
	    /* assert: a is not NULL; see the simplify_lex_rename() call above */
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
    } else if (SCHEME_RENAMESP(a)) {
      Module_Renames *mrn = (Module_Renames *)a;
      int redundant = 0;
      
      if (mrn->kind == mzMOD_RENAME_MARKED) {
	/* Not useful if there's no marked names. */
	redundant = !mrn->marked_names->count;
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
	long shift = 0;	
	WRAP_POS l;
	Scheme_Object *la;
	
	WRAP_POS_COPY(l,w);

	for (; !WRAP_POS_END_P(l); WRAP_POS_INC(l)) {
	  la = WRAP_POS_FIRST(l);
	  if (SCHEME_RENAMESP(la)) {
	    Module_Renames *lrn = (Module_Renames *)WRAP_POS_FIRST(l);
	    if ((lrn->kind == mrn->kind)
		&& ((lrn->phase + shift) == mrn->phase)) {
	      /* mrn is redundant */
	      redundant = 1;
	      break;
	    }
	  } else if (SCHEME_BOXP(la)) {
	    shift += SCHEME_INT_VAL(SCHEME_VEC_ELS(SCHEME_PTR_VAL(WRAP_POS_FIRST(l)))[0]);
	  }
	}
      }

      if (!redundant) {
	if (just_simplify) {
	  stack = CONS((Scheme_Object *)mrn, stack);
	} else {
	  if (mrn->kind == mzMOD_RENAME_TOPLEVEL) {
	    stack = CONS(((mrn->phase == 0)
			  ? scheme_true
			  : scheme_false), 
			 stack);
	  } else {
	    Scheme_Object *local_key;
	  
	    local_key = scheme_marshal_lookup(mt, (Scheme_Object *)mrn);
	    if (!local_key) {
	      /* Convert hash table to vector: */
	      int i, j, count = 0;
	      Scheme_Object *l, *idi;
	    
	      count = mrn->ht->count;

	      l = scheme_make_vector(count * 2, NULL);
	    
	      for (i = mrn->ht->size, j = 0; i--; ) {
		if (mrn->ht->vals[i]) {
		  SCHEME_VEC_ELS(l)[j++] = mrn->ht->keys[i];
		  idi = mrn->ht->vals[i];
		  /* Drop info on nominals, if any: */
		  if (SCHEME_PAIRP(idi)) {
		    if (SCHEME_IMMUTABLEP(idi))
		      idi = SCHEME_CAR(idi);
		    else if (SCHEME_PAIRP(SCHEME_CDR(idi))) {
		      if (SCHEME_INTP(SCHEME_CADR(idi))) {
			idi = CONS(SCHEME_CAR(idi), 
				   CONS(SCHEME_CADR(idi),
					SCHEME_CADR(SCHEME_CDR(idi))));
		      } else
			idi = CONS(SCHEME_CAR(idi), SCHEME_CADR(idi));
		    }
		  }
		  SCHEME_VEC_ELS(l)[j++] = idi;
		}
	      }

	      if (mrn->marked_names && mrn->marked_names->count) {
		Scheme_Object *d = scheme_null, *p;

		for (i = mrn->marked_names->size; i--; ) {
		  if (mrn->marked_names->vals[i]) {
		    p = CONS(mrn->marked_names->keys[i],
			     mrn->marked_names->vals[i]);
		    d = CONS(p, d);
		  }
		}

		l = CONS(l, d);
	      } else
		l = CONS(l, scheme_null);

	      if (SCHEME_PAIRP(mrn->unmarshal_info))
		l = CONS(mrn->unmarshal_info, l); 
	      
	      l = CONS((mrn->kind == mzMOD_RENAME_MARKED) ? scheme_true : scheme_false, l);
	      l = CONS(scheme_make_integer(mrn->phase), l);
	      if (mrn->plus_kernel) {
		l = CONS(scheme_true,l);
		/* note: information on nominals intentially omitted */
	      }
	    
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
    } else if (SCHEME_SYMBOLP(a)) {
      /* mark barrier */
      stack = CONS(a, stack);
      stack_size++;
    } else if (SCHEME_HASHTP(a)) {
      /* chain-specific cache; drop it */
    } else {
      /* box, a phase shift */
      /* We used to drop a phase shift if there are no following
         rename tables. However, the phase shift also identifies
         the source module, which can be relevant. So, keep the
         phase shift. */
      /* Need the phase shift, but drop the export table, if any: */
      Scheme_Object *aa;
      aa = SCHEME_BOX_VAL(a);
      if (SCHEME_TRUEP(SCHEME_VEC_ELS(aa)[3])) {
        a = scheme_make_vector(4, NULL);
        SCHEME_VEC_ELS(a)[0] = SCHEME_VEC_ELS(aa)[0];
        SCHEME_VEC_ELS(a)[1] = SCHEME_VEC_ELS(aa)[1];
        SCHEME_VEC_ELS(a)[2] = SCHEME_VEC_ELS(aa)[2];
        SCHEME_VEC_ELS(a)[3] = scheme_false;
        a = scheme_box(a);
      }
      
      stack = CONS(a, stack);
      stack_size++;
    }
  }

  if (just_simplify) {
    if (stack_size) {
      /* Convert to a chunk: */
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
  
  /* Double-check for equivalent list in table (after simplification): */
  if (mt && mt->pass) {
    /* No need to check for later passed, since mt->same_map
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

    scheme_hash_set(reverse_map, stack, w_in);
  }

  if (mt) {
    /* preserve references that we saw when creating `stack': */
    scheme_marshal_pop_refs(mt, 1);
  }

  /* Remember this wrap set: */
  if (just_simplify) {
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

#ifdef DO_STACK_CHECK
static Scheme_Object *syntax_to_datum_inner(Scheme_Object *o, 
					    Scheme_Hash_Table **ht,
					    int with_marks,
					    Scheme_Marshal_Tables *mt);

static Scheme_Object *syntax_to_datum_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table **ht = (Scheme_Hash_Table **)p->ku.k.p2;
  Scheme_Marshal_Tables *mt = (Scheme_Marshal_Tables *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return syntax_to_datum_inner(o, ht, p->ku.k.i1, mt);
}
#endif

static Scheme_Object *syntax_to_datum_inner(Scheme_Object *o, 
					    Scheme_Hash_Table **ht,
					    int with_marks,
					    Scheme_Marshal_Tables *mt)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *ph, *v, *result, *converted_wraps = NULL;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)o;
      p->ku.k.p2 = (void *)ht;
      p->ku.k.i1 = with_marks;
      p->ku.k.p3 = (void *)mt;
      return scheme_handle_stack_overflow(syntax_to_datum_k);
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  if (STX_KEY(stx) & STX_GRAPH_FLAG) {
    Scheme_Object *key;

    if (!*ht) {
      GC_CAN_IGNORE Scheme_Hash_Table *htv;
      htv = scheme_make_hash_table(SCHEME_hash_ptr);
      *ht = htv;
    }
    
    key = scheme_stx_property((Scheme_Object *)stx, share_symbol, NULL);
    if (SCHEME_FALSEP(key)) {
      scheme_signal_error("bad 'share key");
    }

    ph = scheme_hash_get(*ht, key);

    if (ph)
      return ph;
    else {
      ph = scheme_alloc_small_object();
      ph->type = scheme_stx_placeholder_type;
      
      scheme_hash_set(*ht, key, (Scheme_Object *)ph);
    }
  } else 
    ph = NULL;

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

      a = syntax_to_datum_inner(SCHEME_CAR(v), ht, with_marks, mt);

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
      v = syntax_to_datum_inner(v, ht, with_marks, mt);
      SCHEME_CDR(last) = v;

      if (with_marks) {
        v = extract_for_common_wrap(v, 1, 0);
        if (v && SAME_OBJ(common_wraps, v)) {
          converted_wraps = wraps_to_datum(stx->wraps, mt, NULL, 0);
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
      converted_wraps = wraps_to_datum(stx->wraps, mt, NULL, 0);
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
    v = syntax_to_datum_inner(SCHEME_BOX_VAL(v), ht, with_marks, mt);
    result = scheme_box(v);
  } else if (SCHEME_VECTORP(v)) {
    int size = SCHEME_VEC_SIZE(v), i;
    Scheme_Object *r, *a;
    
    r = scheme_make_vector(size, NULL);
    
    for (i = 0; i < size; i++) {
      a = syntax_to_datum_inner(SCHEME_VEC_ELS(v)[i], ht, with_marks, mt);
      SCHEME_VEC_ELS(r)[i] = a;
    }
    
    result = r;
#ifdef STX_DEBUG
  } else if ((with_marks == 1) && SCHEME_SYMBOLP(v)) {
    result = CONS(v, stx->wraps); /* wraps_to_datum(stx->wraps, mt, 0)); */
#endif
  } else
    result = v;

  if (with_marks > 1) {
    if (!converted_wraps)
      converted_wraps = wraps_to_datum(stx->wraps, mt, NULL, 0);
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
        v = scheme_make_vector(2, NULL);
        SCHEME_VEC_ELS(v)[0] = result;
        if (SCHEME_PAIRP(icert_marks))
          cert_marks = scheme_make_pair(cert_marks, icert_marks);
        SCHEME_VEC_ELS(v)[1] = cert_marks;
        result = v;
      }
    }
  }

  if (ph)
    SCHEME_PTR_VAL(ph) = result;

  return result;
}

Scheme_Object *scheme_syntax_to_datum(Scheme_Object *stx, int with_marks,
				      Scheme_Marshal_Tables *mt)
{
  Scheme_Hash_Table *ht = NULL;
  Scheme_Object *v;

  if (mt)
    scheme_marshal_push_refs(mt);

  v = syntax_to_datum_inner(stx, &ht, with_marks, mt);

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

  if (ht)
    v = scheme_resolve_placeholders(v, 0, scheme_stx_placeholder_type);

  return v;
}

/*========================================================================*/
/*                           syntax is graph?                             */
/*========================================================================*/

#ifdef DO_STACK_CHECK
static int syntax_is_graph_inner(Scheme_Object *o);

static Scheme_Object *syntax_is_graph_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return syntax_is_graph_inner(o) ? scheme_true : scheme_false;
}
#endif

static int syntax_is_graph_inner(Scheme_Object *o)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *v;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)o;
      v = scheme_handle_stack_overflow(syntax_is_graph_k);
      return SCHEME_TRUEP(v);
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  if (STX_KEY(stx) & STX_GRAPH_FLAG)
    return 1;

  v = stx->val;
  
  if (SCHEME_PAIRP(v)) {
    while (SCHEME_PAIRP(v)) {
      if (syntax_is_graph_inner(SCHEME_CAR(v)))
	return 1;
      v = SCHEME_CDR(v);
    }
    if (!SCHEME_NULLP(v)) {
      if (syntax_is_graph_inner(v))
	return 1;
    }
    return 0;
  } else if (SCHEME_BOXP(v)) {
    return syntax_is_graph_inner(SCHEME_BOX_VAL(v));
  } else if (SCHEME_VECTORP(v)) {
    int size = SCHEME_VEC_SIZE(v), i;
    
    for (i = 0; i < size; i++) {
      if (syntax_is_graph_inner(SCHEME_VEC_ELS(v)[i]))
	return 1;
    }
    
    return 0;
  } else
    return 0;
}

int scheme_syntax_is_graph(Scheme_Object *stx)
{
  return syntax_is_graph_inner(stx);
}

/*========================================================================*/
/*                            datum->wraps                                */
/*========================================================================*/

static Scheme_Object *unmarshal_mark(Scheme_Object *_a, Scheme_Unmarshal_Tables *ut)
{
  Scheme_Object *n, *a = _a;

  if (SCHEME_INTP(a) && IS_POSMARK(a))
    a = scheme_make_integer(-SCHEME_INT_VAL(a));
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

#define return_NULL return NULL

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
      int i = SCHEME_VEC_SIZE(a);

      /* Make sure that it's a well-formed rename table. */
      if ((i < 2) || !SCHEME_FALSEP(SCHEME_VEC_ELS(a)[1]))
	return_NULL;
      while (i > 2) {
	i--;
	if (!SCHEME_SYMBOLP(SCHEME_VEC_ELS(a)[i]))
	  return_NULL;
      }

      /* It's ok: */
      scheme_unmarshal_wrap_set(ut, local_key, a);
    } else if (SCHEME_PAIRP(a)) {
      /* A rename table:
           - ([#t] <index-num> <phase-num> <bool> [unmarshal] #(<table-elem> ...) 
	       . ((<sym> (<marked-list-or-mark> . <target-gensym>) ...) ...)) ; <- marked_names
	where a <table-elem> is actually two values, one of:
           - <exname> <modname>
           - <exname> (<modname> . <defname>)
      */
      Scheme_Object *mns;
      Module_Renames *mrn;
      Scheme_Object *p, *key;
      int plus_kernel, i, count, kind;
      long phase;
      
      if (!SCHEME_PAIRP(a)) return_NULL;
      
      /* Convert list to rename table: */
      
      if (SCHEME_BOOLP(SCHEME_CAR(a))) {
	plus_kernel = 1;
	a = SCHEME_CDR(a);
      } else
	plus_kernel = 0;

      if (!SCHEME_PAIRP(a)) return_NULL;
      phase = SCHEME_INT_VAL(SCHEME_CAR(a));
      a = SCHEME_CDR(a);

      if (!SCHEME_PAIRP(a)) return_NULL;
      if (SCHEME_TRUEP(SCHEME_CAR(a)))
	kind = mzMOD_RENAME_MARKED;
      else
	kind = mzMOD_RENAME_NORMAL;
      a = SCHEME_CDR(a);

      mrn = (Module_Renames *)scheme_make_module_rename(phase, kind, NULL);
      mrn->plus_kernel = plus_kernel;
      /* note: information on nominals has been dropped */

      if (!SCHEME_PAIRP(a)) return_NULL;
      mns = SCHEME_CDR(a);
      a = SCHEME_CAR(a);

      if (!SCHEME_VECTORP(a)) {
	/* Unmarshall info: */
	Scheme_Object *ml = a, *mli;
	while (SCHEME_PAIRP(ml)) {
	  mli = SCHEME_CAR(ml);
	  if (!SCHEME_PAIRP(mli)) return_NULL;

	  /* A module path index: */
	  p = SCHEME_CAR(mli);
	  if (!(SCHEME_SYMBOLP(p)
		|| SAME_TYPE(SCHEME_TYPE(p), scheme_module_index_type)))
	    return_NULL;
	  mli = SCHEME_CDR(mli);
	  if (!SCHEME_PAIRP(mli)) return_NULL;

          /* A phase/dimension index (temporarily optional) */
          p = SCHEME_CAR(mli);
          if ((SCHEME_INT_VAL(p) < 0)
              || (SCHEME_INT_VAL(p) > 2))
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

	  ml = SCHEME_CDR(ml);
	}
	if (!SCHEME_NULLP(ml)) return_NULL;

	mrn->unmarshal_info = a;
	if (SCHEME_PAIRP(a))
	  mrn->needs_unmarshal = 1;

	if (!SCHEME_PAIRP(mns)) return_NULL;
	a = SCHEME_CAR(mns);
	mns = SCHEME_CDR(mns);
      }

      if (!SCHEME_VECTORP(a)) return_NULL;
      count = SCHEME_VEC_SIZE(a);
      if (count & 0x1) return_NULL;

      for (i = 0; i < count; i+= 2) {
	key = SCHEME_VEC_ELS(a)[i];
	p = SCHEME_VEC_ELS(a)[i+1];
	
	if (!SCHEME_SYMBOLP(key)) return_NULL;

	if (SCHEME_SYMBOLP(p)
	    || SAME_TYPE(SCHEME_TYPE(p), scheme_module_index_type)) {
	  /* Ok */
	} else if (SCHEME_PAIRP(p)) {
	  Scheme_Object *midx;

	  midx = SCHEME_CAR(p);
	  if (!SCHEME_SYMBOLP(midx)
	      && !SAME_TYPE(SCHEME_TYPE(midx), scheme_module_index_type))
	    return_NULL;

	  if (SCHEME_SYMBOLP(SCHEME_CDR(p))) {
	    /* Ok */
	  } else {
	    if (!SCHEME_PAIRP(SCHEME_CDR(p)))
	      return_NULL;
	    if (!SCHEME_INTP(SCHEME_CADR(p)))
	      return_NULL;
	    if (!SCHEME_SYMBOLP(SCHEME_CDDR(p)))
	      return_NULL;
	    p = CONS(midx, CONS(SCHEME_CADR(p),
				CONS(SCHEME_CDDR(p),
				     CONS(midx, SCHEME_CDDR(p)))));
	  }
	} else
	  return_NULL;
	
	scheme_hash_set(mrn->ht, key, p);
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

      a = (Scheme_Object *)mrn;
    } else if (SAME_OBJ(a, scheme_true)) {
      /* current env rename */
      Scheme_Env *env;

      env = scheme_get_env(NULL);
      if (!env->rename) {
	Scheme_Object *rn;
	rn = scheme_make_module_rename(0, mzMOD_RENAME_TOPLEVEL, NULL);
	env->rename = rn;
      }
      a = env->rename;
    } else if (SCHEME_FALSEP(a)) {
      /* current exp-env rename */
      Scheme_Env *env;
      env = scheme_get_env(NULL);
      scheme_prepare_exp_env(env);
      if (!env->exp_env->rename) {
	Scheme_Object *rn;
	rn = scheme_make_module_rename(1, mzMOD_RENAME_TOPLEVEL, NULL);
	env->exp_env->rename = rn;
      }
      a = env->exp_env->rename;
    } else if (SCHEME_SYMBOLP(a)) {
      /* mark barrier */
    } else {
      /* must be a box for a phase shift */
      /* (or garbage due to a bad .zo, and we'll ignore it) */
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
  Scheme_Object *a, *b, *insp;

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
  Scheme_Object *result, *ph = NULL, *wraps, *cert_marks = NULL;
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
    if (HAS_SUBSTX(o)) {
      long val;

      val = (long)scheme_hash_get(ht, o);
      
      if (val != 1) {
	if (val & 0x1) {
	  ph = scheme_alloc_small_object();
	  ph->type = scheme_stx_placeholder_type;
	  scheme_hash_set(ht, o, (Scheme_Object *)ph);
	} else {
	  return (Scheme_Object *)val;
	}
      }
    }
  }

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
    
    /* Check whether it's all immutable conses with
       syntax inside */
    p = o;
    while (SCHEME_IMMUTABLE_PAIRP(p)) {
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
	  if ((long)scheme_hash_get(ht, o) != 1) {
	    /* cdr is shared. Stop here. */
	    break;
	  }
	}

	a = datum_to_syntax_inner(SCHEME_CAR(o), ut, stx_src, sub_stx_wraps, ht);
	if (!a) return_NULL;
      
	p = scheme_make_immutable_pair(a, scheme_null);
      
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
  } else if (SCHEME_BOXP(o)) {
    o = datum_to_syntax_inner(SCHEME_PTR_VAL(o), ut, stx_src, stx_wraps, ht);
    if (!o) return_NULL;
    result = scheme_box(o);
    SCHEME_SET_BOX_IMMUTABLE(result);
  } else if (SCHEME_VECTORP(o)) {
    int size = SCHEME_VEC_SIZE(o), i;
    Scheme_Object *a;

    result = scheme_make_vector(size, NULL);
    
    for (i = 0; i < size; i++) {
      a = datum_to_syntax_inner(SCHEME_VEC_ELS(o)[i], ut, stx_src, stx_wraps, ht);
      if (!a) return_NULL;
      SCHEME_VEC_ELS(result)[i] = a;
    }

    if (size)
      SCHEME_SET_VECTOR_IMMUTABLE(result);
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
	    || SCHEME_NULLP(SCHEME_CAR(cert_marks)))) {
      /* Have both active and inactive certs */
      Scheme_Object *icerts;
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
  
  if (ph) {
    scheme_make_graph_stx(result, -1, -1, -1);
    SCHEME_PTR_VAL(ph) = result;
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

  if (can_graph && HAS_SUBSTX(o))
    ht = scheme_setup_datum_graph(o, 0);
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

  if (!v) return_NULL; /* only happens with bad wraps from a bad .zo */

  if (code) {
    scheme_unmarshal_wrap_set(ut, code, v);
  }

  if (ht)
    v = scheme_resolve_placeholders(v, 1, scheme_stx_placeholder_type);

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

  if (STX_KEY(stx) & STX_GRAPH_FLAG) {
    /* Instead of potentially losing graph structure
       (or looping!), give up on simplifying. */
    return;
  }
 
  /* Propagate wraps: */
  scheme_stx_content((Scheme_Object *)stx);

  if (rns) {
    v = wraps_to_datum(stx->wraps, NULL, rns, 1);
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
  if (cache) {
    Scheme_Hash_Table *rns;

    rns = (Scheme_Hash_Table *)cache;

    simplify_syntax_inner(stx, rns, NULL);
  }
}

/*========================================================================*/
/*                    Scheme functions and helpers                        */
/*========================================================================*/

static Scheme_Object *syntax_p(int argc, Scheme_Object **argv)
{
  return SCHEME_STXP(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *graph_syntax_p(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-graph?", "syntax", 0, argc, argv);

  return ((STX_KEY((Scheme_Stx *)argv[0]) & STX_GRAPH_FLAG)
	  ? scheme_true
	  : scheme_false);
}


static Scheme_Object *syntax_to_datum(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-object->datum", "syntax", 0, argc, argv);
    
#if STX_DEBUG
  if (argc == 2)
      return scheme_syntax_to_datum(argv[0], 1, scheme_make_hash_table(SCHEME_hash_ptr));
#endif

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
    scheme_wrong_type("datum->syntax-object", "syntax or #f", 0, argc, argv);
  if (argc > 2) {
    int ll;

    src = argv[2];

    ll = scheme_proper_list_length(src);

    if (!SCHEME_FALSEP(src) 
	&& !SCHEME_STXP(src)
	&& !((ll == 5)
	     && pos_exact_or_false_p(SCHEME_CADR(src))
	     && nonneg_exact_or_false_p(SCHEME_CADR(SCHEME_CDR(src)))
	     && pos_exact_or_false_p(SCHEME_CADR(SCHEME_CDR(SCHEME_CDR(src))))
	     && nonneg_exact_or_false_p(SCHEME_CADR(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(src)))))))
      scheme_wrong_type("datum->syntax-object", "syntax, source location list, or #f", 2, argc, argv);

    if (argc > 3) {
      if (!SCHEME_FALSEP(argv[3])) {
	if (!SCHEME_STXP(argv[3]))
	  scheme_wrong_type("datum->syntax-object", "syntax or #f", 3, argc, argv);
	properties = ((Scheme_Stx *)argv[3])->props;
      }
      
      if (argc > 4) {
        if (!SCHEME_FALSEP(argv[4])) {
          if (!SCHEME_STXP(argv[4]))
            scheme_wrong_type("datum->syntax-object", "syntax or #f", 4, argc, argv);
          certs = (Scheme_Object *)INACTIVE_CERTS((Scheme_Stx *)argv[4]);
        }
      }
    }

    if (ll == 5) {
      /* line--column--pos--span format */
      Scheme_Object *line, *col, *pos, *span;
      line = SCHEME_CADR(src);
      col = SCHEME_CADR(SCHEME_CDR(src));
      pos = SCHEME_CADR(SCHEME_CDR(SCHEME_CDR(src)));
      span = SCHEME_CADR(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(src))));
      src = SCHEME_CAR(src);
      
      if (SCHEME_FALSEP(line) != SCHEME_FALSEP(col))
	scheme_arg_mismatch("datum->syntax-object", 
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

  if (certs) {
    src = lift_inactive_certs(src, 0);
    src = add_certs(src, (Scheme_Cert *)certs, NULL, 0);    
  }

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

  if (same_marks(&awl, &ewl, scheme_false, NULL))
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
    int graph;
    
    graph = (STX_KEY(stx) & STX_GRAPH_FLAG);

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

    if (graph)
      STX_KEY(stx) |= STX_GRAPH_FLAG;

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
    scheme_wrong_type("syntax-property", "syntax", 0, argc, argv);

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

static Scheme_Object *bound_eq(int argc, Scheme_Object **argv)
{
  Scheme_Thread *p = scheme_current_thread;

  if (!SCHEME_STX_IDP(argv[0]))
    scheme_wrong_type("bound-identifier=?", "identifier syntax", 0, argc, argv);
  if (!SCHEME_STX_IDP(argv[1]))
    scheme_wrong_type("bound-identifier=?", "identifier syntax", 1, argc, argv);

  return (scheme_stx_bound_eq(argv[0], argv[1],
			      (p->current_local_env
			       ? p->current_local_env->genv->phase
			       : 0))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *free_eq(int argc, Scheme_Object **argv)
{
  Scheme_Thread *p = scheme_current_thread;

  if (!SCHEME_STX_IDP(argv[0]))
    scheme_wrong_type("free-identifier=?", "identifier syntax", 0, argc, argv);
  if (!SCHEME_STX_IDP(argv[1]))
    scheme_wrong_type("free-identifier=?", "identifier syntax", 1, argc, argv);

  return (scheme_stx_free_eq(argv[0], argv[1],
			     (p->current_local_env
			      ? p->current_local_env->genv->phase
			      : 0))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *do_module_eq(const char *who, int delta, int argc, Scheme_Object **argv)
{
  Scheme_Thread *p = scheme_current_thread;

  if (!SCHEME_STX_IDP(argv[0]))
    scheme_wrong_type(who, "identifier syntax", 0, argc, argv);
  if (!SCHEME_STX_IDP(argv[1]))
    scheme_wrong_type(who, "identifier syntax", 1, argc, argv);

  return (scheme_stx_module_eq(argv[0], argv[1],
                               ((delta == MZ_LABEL_PHASE)
                                ? MZ_LABEL_PHASE
                                : (delta + (p->current_local_env
                                            ? p->current_local_env->genv->phase
                                            : 0))))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *module_eq(int argc, Scheme_Object **argv)
{
  return do_module_eq("module-identifier=?", 0, argc, argv);
}

static Scheme_Object *module_trans_eq(int argc, Scheme_Object **argv)
{
  return do_module_eq("module-transformer-identifier=?", 1, argc, argv);
}

static Scheme_Object *module_templ_eq(int argc, Scheme_Object **argv)
{
  return do_module_eq("module-template-identifier=?", -1, argc, argv);
}

static Scheme_Object *module_label_eq(int argc, Scheme_Object **argv)
{
  return do_module_eq("module-label-identifier=?", MZ_LABEL_PHASE, argc, argv);
}

static Scheme_Object *do_module_binding(char *name, int argc, Scheme_Object **argv, 
					int dphase, int get_position)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *a, *m, *nom_mod, *nom_a;
  int mod_phase;

  a = argv[0];

  if (!SCHEME_STXP(a) || !SCHEME_STX_SYMBOLP(a))
    scheme_wrong_type(name, "identifier syntax", 0, argc, argv);

  m = scheme_stx_module_name(&a, 
                             ((dphase == MZ_LABEL_PHASE)
                              ? MZ_LABEL_PHASE
                              : (dphase + (p->current_local_env
					   ? p->current_local_env->genv->phase
					   : 0))),
			     &nom_mod, &nom_a,
			     &mod_phase);

  if (!m)
    return scheme_false;
  else if (SAME_OBJ(m, scheme_undefined)) {
    if (get_position)
      return scheme_false;
    else
      return lexical_symbol;
  } else {
    if (get_position) {
      /* Imported or a "self" variable? */
      if (SAME_TYPE(SCHEME_TYPE(m), scheme_module_index_type)
	  && SCHEME_FALSEP(((Scheme_Modidx *)m)->path)
	  && SCHEME_FALSEP(((Scheme_Modidx *)m)->base)) {
	/* self */
	return scheme_false;
      } else {
	/* Imported */
	int pos;
	
	m = scheme_module_resolve(m, 0);
	pos = scheme_module_export_position(m, scheme_get_env(NULL), a);
	if (pos < 0)
	  return scheme_false;
	else
	  return scheme_make_integer(pos);
      }
    } else
      return CONS(m, CONS(a, CONS(nom_mod, 
				  CONS(nom_a, 
				       CONS(mod_phase ? scheme_true : scheme_false, 
					    scheme_null)))));
  }
}

static Scheme_Object *module_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-binding", argc, argv, 0, 0);
}

static Scheme_Object *module_trans_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-transformer-binding", argc, argv, 1, 0);
}

static Scheme_Object *module_templ_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-template-binding", argc, argv, -1, 0);
}

static Scheme_Object *module_label_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-label-binding", argc, argv, MZ_LABEL_PHASE, 0);
}

static Scheme_Object *module_binding_pos(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-binding-export-position", argc, argv, 0, 1);
}

static Scheme_Object *module_trans_binding_pos(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-transformer-binding-export-position", argc, argv, 1, 1);
}

static Scheme_Object *syntax_src_module(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-source-module", "syntax", 0, argc, argv);

  return scheme_stx_source_module(argv[0], 0);
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

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_STXOBJ_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rename_table_type, mark_rename_table);
  GC_REG_TRAV(scheme_rt_srcloc, mark_srcloc);
  GC_REG_TRAV(scheme_wrap_chunk_type, mark_wrapchunk);
  GC_REG_TRAV(scheme_certifications_type, mark_cert);
  GC_REG_TRAV(scheme_lexical_rib_type, lex_rib);
}

END_XFORM_SKIP;

#endif
