/* The "syntax" layer here is visible via `racket/linklet` as
   the "correlated" API. */

#include "schpriv.h"
#include "schmach.h"

ROSYM static Scheme_Object *source_symbol; /* uninterned! */

READ_ONLY Scheme_Object *scheme_syntax_p_proc;

READ_ONLY static Scheme_Hash_Tree *empty_hash_tree;

ROSYM Scheme_Object *scheme_paren_shape_symbol;

READ_ONLY Scheme_Hash_Tree *scheme_source_stx_props;
READ_ONLY Scheme_Object *scheme_paren_shape_preserve_square;
READ_ONLY Scheme_Object *scheme_paren_shape_preserve_curly;
READ_ONLY static Scheme_Hash_Tree *square_stx_props;
READ_ONLY static Scheme_Hash_Tree *curly_stx_props;

READ_ONLY static Scheme_Stx_Srcloc *empty_srcloc;

static Scheme_Object *syntax_p(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_to_datum(int argc, Scheme_Object **argv);
static Scheme_Object *datum_to_syntax(int argc, Scheme_Object **argv);

Scheme_Object *scheme_checked_syntax_e(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_line(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_col(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_pos(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_span(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_src(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_property(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_property_keys(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_to_datum_inner(Scheme_Object *o);
static Scheme_Object *datum_to_syntax_inner(Scheme_Object *o, 
					    Scheme_Stx *stx_src,
                                            Scheme_Hash_Table *ht);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define CONS scheme_make_pair

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

#define STX_ASSERT(x) MZ_ASSERT(x)

#define HAS_SUBSTX(obj) (SCHEME_PAIRP(obj) || SCHEME_VECTORP(obj) || SCHEME_BOXP(obj) || prefab_p(obj) || SCHEME_HASHTRP(obj))
#define HAS_CHAPERONE_SUBSTX(obj) (HAS_SUBSTX(obj) || (SCHEME_NP_CHAPERONEP(obj) && HAS_SUBSTX(SCHEME_CHAPERONE_VAL(obj))))

/*========================================================================*/
/*                           initialization                               */
/*========================================================================*/

void scheme_init_stx(Scheme_Startup_Env *env)
{
  Scheme_Object *o;

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  REGISTER_SO(empty_hash_tree);
  empty_hash_tree = scheme_make_hash_tree(0);

  REGISTER_SO(scheme_syntax_p_proc);
  o = scheme_make_folding_prim(syntax_p, "syntax?", 1, 1, 1);
  scheme_syntax_p_proc = o;
  SCHEME_PRIM_PROC_FLAGS(o) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance("syntax?", o, env);

  ADD_FOLDING_PRIM("syntax->datum", syntax_to_datum, 1, 1, 1, env);
  ADD_IMMED_PRIM("datum->syntax", datum_to_syntax, 2, 5, env);
  
  o = scheme_make_folding_prim(scheme_checked_syntax_e, "syntax-e", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(o) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance("syntax-e", o, env);

  ADD_FOLDING_PRIM("syntax-line"    , syntax_line   , 1, 1, 1, env);
  ADD_FOLDING_PRIM("syntax-column"  , syntax_col    , 1, 1, 1, env);
  ADD_FOLDING_PRIM("syntax-position", syntax_pos    , 1, 1, 1, env);
  ADD_FOLDING_PRIM("syntax-span"    , syntax_span   , 1, 1, 1, env);
  ADD_FOLDING_PRIM("syntax-source"  , syntax_src    , 1, 1, 1, env);

  ADD_IMMED_PRIM("syntax-property"                  , syntax_property           , 2, 3, env);
  ADD_IMMED_PRIM("syntax-property-symbol-keys"      , syntax_property_keys      , 1, 1, env);

  REGISTER_SO(source_symbol);
  source_symbol = scheme_make_symbol("source"); /* not interned! */

  REGISTER_SO(empty_srcloc);
  empty_srcloc = MALLOC_ONE_RT(Scheme_Stx_Srcloc);
#ifdef MZTAG_REQUIRED
  empty_srcloc->type = scheme_rt_srcloc;
#endif
  empty_srcloc->src = scheme_false;
  empty_srcloc->line = -1;
  empty_srcloc->col = -1;
  empty_srcloc->pos = -1;
  empty_srcloc->span = -1;

  REGISTER_SO(scheme_paren_shape_symbol);
  scheme_paren_shape_symbol = scheme_intern_symbol("paren-shape");

  REGISTER_SO(scheme_paren_shape_preserve_square);
  scheme_paren_shape_preserve_square = scheme_make_ascii_character('[');

  REGISTER_SO(scheme_paren_shape_preserve_curly);
  scheme_paren_shape_preserve_curly = scheme_make_ascii_character('{');

  REGISTER_SO(scheme_source_stx_props);
  REGISTER_SO(square_stx_props);
  REGISTER_SO(curly_stx_props);
  scheme_source_stx_props = scheme_hash_tree_set(empty_hash_tree, source_symbol, scheme_true);
  square_stx_props = scheme_hash_tree_set(empty_hash_tree, scheme_paren_shape_symbol, scheme_paren_shape_preserve_square);
  curly_stx_props = scheme_hash_tree_set(empty_hash_tree, scheme_paren_shape_symbol, scheme_paren_shape_preserve_curly);
}

void scheme_init_stx_places(int initial_main_os_thread) {
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
  stx->so.type = scheme_stx_type;
  stx->val = val;
  stx->srcloc = srcloc;
  stx->props = props;

  return (Scheme_Object *)stx;
}

Scheme_Object *clone_stx(Scheme_Object *to, GC_CAN_IGNORE int *mutate)
/* the `mutate` argument tracks whether we can mutate `to` */
{
  Scheme_Stx *stx = (Scheme_Stx *)to;

  STX_ASSERT(SCHEME_STXP(to));

  if (mutate && (*mutate & MUTATE_STX_OBJ))
    return to;

  stx = (Scheme_Stx *)scheme_make_stx(stx->val,
                                      stx->srcloc,
                                      stx->props);

  if (mutate)
    *mutate |= MUTATE_STX_OBJ;

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

/*========================================================================*/
/*                           syntax->datum                                */
/*========================================================================*/

#ifdef DO_STACK_CHECK
static Scheme_Object *syntax_to_datum_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return syntax_to_datum_inner(o);
}
#endif

static Scheme_Object *syntax_to_datum_inner(Scheme_Object *o)
/* Recurs through `o` to find syntax objects and strip them away, or
   returns `o` if no syntax objects are inside. */
{
  Scheme_Object *v, *result;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)o;
      return scheme_handle_stack_overflow(syntax_to_datum_k);
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  if (SCHEME_STXP(o))
    o = SCHEME_STX_VAL(o);
  v = o;
  
  if (SCHEME_PAIRP(v)) {
    Scheme_Object *first = NULL, *last = NULL, *p;
    Scheme_Object *a;
    int same = 0;
    
    while (SCHEME_PAIRP(v)) {
      a = syntax_to_datum_inner(SCHEME_CAR(v));

      if (!first && SAME_OBJ(a, SCHEME_CAR(v))) {
        same++;
        v = SCHEME_CDR(v);
      } else {
        if (!first && (same > 0)) {
          v = o;
          while (same--) {
            p = CONS(SCHEME_CAR(v), scheme_null);
            if (last)
              SCHEME_CDR(last) = p;
            else
              first = p;
            last = p;
            v = SCHEME_CDR(v);
          }
        }
        
        p = CONS(a, scheme_null);
      
        if (last)
          SCHEME_CDR(last) = p;
        else
          first = p;
        last = p;
        v = SCHEME_CDR(v);
      }
    }
    if (!SCHEME_NULLP(v)) {
      a = syntax_to_datum_inner(v);
      if (!first && SAME_OBJ(v, a))
        return o;
      else {
        v = o;
        while (same--) {
          p = CONS(SCHEME_CAR(v), scheme_null);
          if (last)
            SCHEME_CDR(last) = p;
          else
            first = p;
          last = p;
          v = SCHEME_CDR(v);
        }
        
        SCHEME_CDR(last) = a;
      }
    } else if (!first)
      return o;

    result = first;
  } else if (SCHEME_BOXP(v)) {
    v = syntax_to_datum_inner(SCHEME_BOX_VAL(v));
    if (v == SCHEME_BOX_VAL(o))
      return o;
    result = scheme_box(v);
    SCHEME_SET_IMMUTABLE(result);
  } else if (SCHEME_VECTORP(v)) {
    int size = SCHEME_VEC_SIZE(v), i, j;
    Scheme_Object *r, *a;

    for (i = 0; i < size; i++) {
      a = syntax_to_datum_inner(SCHEME_VEC_ELS(v)[i]);
      if (!SAME_OBJ(a, SCHEME_VEC_ELS(v)[i]))
        break;
    }

    if (i >= size)
      return o;
    
    r = scheme_make_vector(size, NULL);

    for (j = 0; j < i; j++) {
      SCHEME_VEC_ELS(r)[j] = SCHEME_VEC_ELS(v)[j];
    }
    SCHEME_VEC_ELS(r)[i] = a;
    for (i++; i < size; i++) {
      a = syntax_to_datum_inner(SCHEME_VEC_ELS(v)[i]);
      SCHEME_VEC_ELS(r)[i] = a;
    }

    result = r;
    SCHEME_SET_IMMUTABLE(result);
  } else if (SCHEME_HASHTRP(v)) {
    Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)v, *ht2;
    Scheme_Object *key, *val, *val2;
    mzlonglong i, j;

    i = scheme_hash_tree_next(ht, -1);
    while (i != -1) {
      scheme_hash_tree_index(ht, i, &key, &val);
      val2 = syntax_to_datum_inner(val);
      if (!SAME_OBJ(val, val2))
        break;
      i = scheme_hash_tree_next(ht, i);
    }
    if (i == -1)
      return o;
    
    ht2 = scheme_make_hash_tree_of_type(SCHEME_HASHTR_TYPE(ht));

    j = scheme_hash_tree_next(ht, -1);
    while (j != i) {
      scheme_hash_tree_index(ht, j, &key, &val);
      val = syntax_to_datum_inner(val);
      ht2 = scheme_hash_tree_set(ht2, key, val);
      j = scheme_hash_tree_next(ht, j);
    }
    
    scheme_hash_tree_index(ht, i, &key, &val);
    ht2 = scheme_hash_tree_set(ht2, key, val2);
    
    i = scheme_hash_tree_next(ht, i);
    while (i != -1) {
      scheme_hash_tree_index(ht, i, &key, &val);
      val = syntax_to_datum_inner(val);
      ht2 = scheme_hash_tree_set(ht2, key, val);
      i = scheme_hash_tree_next(ht, i);
    }
    
    result = (Scheme_Object *)ht2;
  } else if (prefab_p(v)) {
    Scheme_Structure *s = (Scheme_Structure *)v;
    Scheme_Object *a;
    int size = s->stype->num_slots, i;

    for (i = 0; i < size; i++) {
      a = syntax_to_datum_inner(s->slots[i]);
      if (!SAME_OBJ(a, s->slots[i]))
        break;
    }
    if (i >= size)
      return o;

    s = (Scheme_Structure *)scheme_clone_prefab_struct_instance(s);
    s->slots[i] = a;
    for (i++; i < size; i++) {
      a = syntax_to_datum_inner(s->slots[i]);
      s->slots[i] = a;
    }

    result = (Scheme_Object *)s;
  } else
    result = v;

  return result;
}

Scheme_Object *scheme_syntax_to_datum(Scheme_Object *stx)
{
  return syntax_to_datum_inner(stx);
}

/*========================================================================*/
/*                           datum->syntax                                */
/*========================================================================*/

#define return_NULL return NULL

#ifdef DO_STACK_CHECK
static Scheme_Object *datum_to_syntax_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Stx *stx_src = (Scheme_Stx *)p->ku.k.p2;
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p->ku.k.p3;
					    
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return datum_to_syntax_inner(o, stx_src, ht);
}
#endif

static Scheme_Object *datum_to_syntax_inner(Scheme_Object *o, 
					    Scheme_Stx *stx_src,
					    Scheme_Hash_Table *ht)
{
  Scheme_Object *result, *hashed;

  if (SCHEME_STXP(o))
    return o;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)o;
      p->ku.k.p2 = (void *)stx_src;
      p->ku.k.p3 = (void *)ht;
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
      /* Build up a new list while converting elems */
      while (SCHEME_PAIRP(o)) {
	Scheme_Object *a;
      
	if (ht && last) {
	  if (scheme_hash_get(ht, o)) {
            /* cdr is shared. Stop here and let someone else complain. */
            break;
	  }
	}

	a = datum_to_syntax_inner(SCHEME_CAR(o), stx_src, ht);
	if (!a) return_NULL;
      
	p = scheme_make_pair(a, scheme_null);
      
	if (last)
	  SCHEME_CDR(last) = p;
	else
	  first = p;
	last = p;
	o = SCHEME_CDR(o);
      }
      if (!first) return_NULL;
      if (!SCHEME_NULLP(o)) {
	o = datum_to_syntax_inner(o, stx_src, ht);
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

    o = datum_to_syntax_inner(o, stx_src, ht);
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
      a = datum_to_syntax_inner(a, stx_src, ht);
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
      val = datum_to_syntax_inner(val, stx_src, ht);
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
      a = datum_to_syntax_inner(s->slots[i], stx_src, ht);
      if (!a) return NULL;
      s->slots[i] = a;
    }

    result = (Scheme_Object *)s;
  } else
    result = scheme_read_intern(o);

  if (SCHEME_FALSEP((Scheme_Object *)stx_src))
    result = scheme_make_stx(result, empty_srcloc, NULL);
  else
    result = scheme_make_stx(result, stx_src->srcloc, NULL);
  
  if (hashed)
    scheme_hash_set(ht, hashed, NULL);
  
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

Scheme_Object *scheme_datum_to_syntax(Scheme_Object *o, 
                                      Scheme_Object *stx_src,
                                      int flags)
{
  Scheme_Object *v;

  if (!SCHEME_FALSEP(stx_src) && !SCHEME_STXP(stx_src))
    return o;

  if (SCHEME_STXP(o))
    return o;

  if (flags & DTS_RECUR) {
    Scheme_Hash_Table *ht;

    if ((flags & DTS_CAN_GRAPH) && !quick_check_graph(o, 10))
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
    else
      ht = NULL;

    v = datum_to_syntax_inner(o, (Scheme_Stx *)stx_src, ht);

    if (!v) {
      /* only happens with cycles: */
      scheme_contract_error("datum->syntax",
                            "cannot create syntax from cyclic datum",
                            "datum", 1, o,
                            NULL);
      return NULL;
    }
  } else if (SCHEME_FALSEP(stx_src))
    v = scheme_make_stx(o, empty_srcloc, NULL);
  else
    v = scheme_make_stx(o, ((Scheme_Stx *)stx_src)->srcloc, NULL);

  if (flags & DTS_COPY_PROPS)
    ((Scheme_Stx *)v)->props = ((Scheme_Stx *)stx_src)->props;

  return v;
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
    
  return scheme_syntax_to_datum(argv[0]);
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
  
  /* The first argument is accepted only for backward compatibility: */
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

  src = scheme_datum_to_syntax(argv[1], src, DTS_CAN_GRAPH);

  if (properties) {
    ((Scheme_Stx *)src)->props = properties;
  }

  return src;
}

Scheme_Object *scheme_checked_syntax_e(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-e", "syntax?", 0, argc, argv);
    
  return SCHEME_STX_VAL(argv[0]);
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
    
  if (stx->srcloc->col <= 0)
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

Scheme_Object *scheme_stx_property(Scheme_Object *_stx,
                                   Scheme_Object *key,
                                   Scheme_Object *val)
{
  Scheme_Stx *stx;
  Scheme_Hash_Tree *props;

  if (!SCHEME_STXP(_stx))
    return scheme_false;

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
    return val;
  }
}

static Scheme_Object *syntax_property(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_contract("syntax-property", "syntax?", 0, argc, argv);

  if (argc > 2)
    return scheme_stx_property(argv[0], argv[1], argv[2]);
  else
    return scheme_stx_property(argv[0], argv[1], NULL);
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

/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_syntax.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_srcloc, mark_srcloc);
}

END_XFORM_SKIP;

#endif
