/* This file is #included by expander.inc when it is built via cify */
#include "schmach.h"

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

/* Disable the use of source or bytecode: */
#define SCHEME_STARTUP_DEFINED

#ifdef c_VALIDATE_DEBUG
static Scheme_Object *c_validate(Scheme_Object *s);
#endif

THREAD_LOCAL_DECL(static struct startup_instance_top_t *c_startup_instance_top);

typedef struct c_saved_mark_stack_t {
  MZ_MARK_POS_TYPE pos;
  MZ_MARK_STACK_TYPE stack;
} c_saved_mark_stack_t;

/* Pulling the address of the thread-local table into a local variable
   can have a big effect on compile time (not so much on run time) if
   the the thread-local implementation is opqaue to the compiler. */
#ifdef PREFER_TO_CACHE_THREAD_LOCAL
# define c_LINK_THREAD_LOCAL Thread_Local_Variables *c_racket_tls = scheme_get_thread_local_variables();
# define c_current_runstack       (c_racket_tls)->scheme_current_runstack_
# define c_current_runstack_start (c_racket_tls)->scheme_current_runstack_start_
# define c_current_thread         (c_racket_tls)->scheme_current_thread_
# define c__startup_instance_top  (c_racket_tls)->c_startup_instance_top_
# define c_scheme_fuel_counter    (c_racket_tls)->scheme_fuel_counter_
static c_saved_mark_stack_t c__push_mark_stack(Thread_Local_Variables *c_racket_tls)
{
  c_saved_mark_stack_t s;
  s.pos = c_racket_tls->scheme_current_cont_mark_pos_;
  s.stack = c_racket_tls->scheme_current_cont_mark_stack_;
  c_racket_tls->scheme_current_cont_mark_pos_ = s.pos + 2;
  return s;
}
# define c_push_mark_stack()     c__push_mark_stack(c_racket_tls)
static void c__pop_mark_stack(Thread_Local_Variables *c_racket_tls, c_saved_mark_stack_t s)
{
  c_racket_tls->scheme_current_cont_mark_pos_ = s.pos;
  c_racket_tls->scheme_current_cont_mark_stack_ = s.stack;
}
# define c_pop_mark_stack(s)     c__pop_mark_stack(c_racket_tls, s)
#else
# define c_LINK_THREAD_LOCAL /* empty */
# define c_current_runstack       MZ_RUNSTACK
# define c_current_runstack_start MZ_RUNSTACK_START
# define c_current_thread         scheme_current_thread
# define c__startup_instance_top  c_startup_instance_top
# define c_scheme_fuel_counter    scheme_fuel_counter
static c_saved_mark_stack_t c_push_mark_stack()
{
  c_saved_mark_stack_t s;
  s.pos = MZ_CONT_MARK_POS;
  s.stack = MZ_CONT_MARK_STACK;
  MZ_CONT_MARK_POS = s.pos + 2;
  return s;
}
static void c_pop_mark_stack(c_saved_mark_stack_t s)
{
  MZ_CONT_MARK_POS = s.pos;
  MZ_CONT_MARK_STACK = s.stack;
}
#endif

#ifdef SCHEME_BIG_ENDIAN
# define c_SELECT_LITTLE_BIG_ENDIAN(l, b) b
#else
# define c_SELECT_LITTLE_BIG_ENDIAN(l, b) l
#endif

#define c_use_fuel() if (DECREMENT_FUEL(c_scheme_fuel_counter, 1) <= 0) scheme_out_of_fuel();

#define c_RUNSTACK_INIT_VAL NULL

static void scheme_instance_add(Scheme_Instance *inst, const char *name, Scheme_Object *val)
{
  Scheme_Bucket *b;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, val);
  MZ_GC_REG();

  b = scheme_instance_variable_bucket(scheme_intern_symbol(name), inst);
  b->val = val;
  ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_IS_CONST | GLOB_IS_CONSISTENT;

  MZ_GC_UNREG();
}

#define c_check_runstack_space(max_depth, runstack, runstack_start) \
  ((runstack - runstack_start) < (max_depth + SCHEME_TAIL_COPY_THRESHOLD))

static int c_check_overflow_or_runstack_space(int max_depth, Scheme_Object **runstack, Scheme_Object **runstack_start)
{
#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      return 1;
    }
  }
#endif
  return c_check_runstack_space(max_depth, runstack, runstack_start);
}

static void c_check_top_runstack_depth(int max_depth)
{
  if (c_check_runstack_space(max_depth, MZ_RUNSTACK, MZ_RUNSTACK_START)) {
    scheme_log_abort("initial runstack is too small to start up");
    abort();
  }
}


static Scheme_Object *do_apply_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  
  if (c_check_runstack_space(p->ku.k.i2, MZ_RUNSTACK, MZ_RUNSTACK_START)) {
    return (Scheme_Object *)scheme_enlarge_runstack(p->ku.k.i2, (void *(*)())do_apply_k);
  } else {
    Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
    Scheme_Object **argv = (Scheme_Object **)p->ku.k.p2;

#ifdef c_VALIDATE_DEBUG
    {
      int i;
      c_validate(o);
      for (i = 0; i < p->ku.k.i1; i++)
        c_validate(argv[i]);
    }
#endif

    p->ku.k.p1 = NULL;
    p->ku.k.p2 = NULL;
    
    return _scheme_apply_multi(o, p->ku.k.i1, argv);
  }
}

static Scheme_Object *c_handle_overflow_or_space(Scheme_Object *proc, int argc, Scheme_Object **argv, int runstack_space)
{
  Scheme_Thread *p;
  Scheme_Object **argv2 = NULL;

  /* stash before allocation: */
  p = scheme_current_thread;
  p->ku.k.p1 = (void *)proc;
  p->ku.k.i1 = argc;
  p->ku.k.i2 = runstack_space;
  p->ku.k.p2 = (void *)argv;

  if (argc != 0) {
    argv2 = MALLOC_N(Scheme_Object*, argc);

    /* In order for this code to be GC friendly, which can be triggered 
     *  anytime memory is allocated, we need to refresh scheme_current_thread */
    p = scheme_current_thread;
    argv = (Scheme_Object **)p->ku.k.p2;
    memcpy(argv2, argv, sizeof(Scheme_Object *) * argc);
  }
    
  if (argv == MZ_RUNSTACK)
    memset(argv, 0, sizeof(Scheme_Object *) * argc); /* space safety */

  p->ku.k.p2 = (void *)argv2;
  
#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    return scheme_handle_stack_overflow(do_apply_k);
  }
#endif
  
  return (Scheme_Object *)scheme_enlarge_runstack(runstack_space, (void *(*)())do_apply_k);
}

static Scheme_Object *c_ensure_args_in_place_rest(int argc, Scheme_Object **argv, Scheme_Object **runbase,
                                                  int direct_args, int rest_args, int rest_arg_used,
                                                  Scheme_Object *self)
{
  Scheme_Object **runstack = runbase - direct_args - rest_args;
  int i;

  if (argc == direct_args) {
    /* Copy into runbase. If there's a rest arg not supplied, then the
       copy may be shifting down, and we need to add a `null` value
       for the rest arg. */
    for (i = 0; i < direct_args; i++)
      runstack[i] = argv[i];
    if (rest_args)
      runstack[direct_args] = scheme_null;
  } else {
    /* Need to build a list and then copy or shift up */
    Scheme_Object *l = scheme_null;
    if (rest_arg_used) {
      MZ_GC_DECL_REG(2);
    
      MZ_GC_VAR_IN_REG(0, argv);
      MZ_GC_VAR_IN_REG(1, self);
      MZ_GC_REG();
      for (i = argc; i-- > direct_args; )
        l = scheme_make_pair(argv[i], l);
      MZ_GC_UNREG();
    }
    
    runstack[direct_args] = l;
    for (i = direct_args; i--; )
      runstack[i] = argv[i];
  }

  return self;
}

#define c_ensure_args_in_place(argc, argv, runbase) \
  if (argv != (runbase - argc)) (void)c_ensure_args_in_place_rest(argc, argv, runbase, argc, 0, 0, NULL)
#define c_rest_arg_used   1
#define c_rest_arg_unused 0


static Scheme_Object *c_wrong_arity(const char *name, int argc, Scheme_Object **argv)
{
  scheme_wrong_count(name, -2, 0, argc, argv);
  return NULL;
}

static Scheme_Object *scheme_make_prim_w_case_arity(Scheme_Prim *prim, const char *name, mzshort mina, const char *arities)
{
  Scheme_Object *p;
  p = scheme_make_prim_w_arity(prim, name, 0, 0);
  ((Scheme_Primitive_Proc *)p)->mina = mina;
  ((Scheme_Primitive_Proc *)p)->mu.cases = (mzshort *)arities;
  return p;
}

static Scheme_Object *scheme_make_prim_closure_w_case_arity(Scheme_Primitive_Closure_Proc *prim,
                                                            int size, Scheme_Object **vals,
                                                            const char *name,
                                                            mzshort mina, const char *arities)
{
  Scheme_Object *p;
  p = scheme_make_prim_closure_w_arity(prim, size, vals, name, 0, 0);
  ((Scheme_Primitive_Proc *)p)->mina = mina;
  ((Scheme_Primitive_Proc *)p)->mu.cases = (mzshort *)arities;
  return p;
}

#define c_extract_prim(o) ((Scheme_Prim *)((Scheme_Primitive_Proc *)o)->prim_val)

static MZ_INLINE int c_same_obj(Scheme_Object *a, Scheme_Object *b)
{
  return SAME_OBJ(a, b);
}

static MZ_INLINE Scheme_Object *c_malloc_struct(int c)
{
  return scheme_malloc_tagged(sizeof(Scheme_Structure) + (((c) - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
}

static MZ_INLINE void c_struct_set_type(Scheme_Object *s, Scheme_Object *_st)
{
  Scheme_Struct_Type *stype = (Scheme_Struct_Type *)_st;
  s->type = (stype->proc_attr ? scheme_proc_struct_type : scheme_structure_type);
  ((Scheme_Structure *)s)->stype = stype;
}

#define c_STRUCT_ELS(o) (((Scheme_Structure *)(o))->slots)

static MZ_INLINE int c_is_struct_instance(Scheme_Object *v, Scheme_Object *_st)
{
  Scheme_Struct_Type *st = (Scheme_Struct_Type *)_st;
  if (SCHEME_CHAPERONEP(v))
    v = SCHEME_CHAPERONE_VAL(v);
  return (SCHEME_STRUCTP(v)
          && (((Scheme_Structure *)v)->stype->parent_types[st->name_pos] == st));
}

static MZ_INLINE int c_is_authentic_struct_instance(Scheme_Object *v, Scheme_Object *_st)
{
  Scheme_Struct_Type *st = (Scheme_Struct_Type *)_st;
  return (SCHEME_STRUCTP(v)
          && (((Scheme_Structure *)v)->stype->parent_types[st->name_pos] == st));
}

static MZ_INLINE Scheme_Object *c_struct_ref(Scheme_Object *v, int pos)
{
  if (SCHEME_CHAPERONEP(v))
    return scheme_struct_ref(v, pos);
  else
    return ((Scheme_Structure *)v)->slots[pos];
}

static MZ_INLINE Scheme_Object *c_authentic_struct_ref(Scheme_Object *v, int pos)
{
  return ((Scheme_Structure *)v)->slots[pos];
}

static MZ_INLINE Scheme_Object *c_struct_set(Scheme_Object *v, Scheme_Object *a, int pos)
{
  if (SCHEME_CHAPERONEP(v))
    scheme_struct_set(v, pos, a);
  else
    ((Scheme_Structure *)v)->slots[pos] = a;
  return scheme_void;
}

static MZ_INLINE Scheme_Object *c_authentic_struct_set(Scheme_Object *v, Scheme_Object *a, int pos)
{
  ((Scheme_Structure *)v)->slots[pos] = a;
  return scheme_void;
}

static MZ_INLINE Scheme_Object *c_struct_property_ref(Scheme_Object *v, Scheme_Object *prop)
{
  return scheme_chaperone_struct_type_property_ref(prop, v);
}

static MZ_INLINE int c_int_lt(Scheme_Object *a, Scheme_Object *b)
{
  return SCHEME_INT_VAL(a) < SCHEME_INT_VAL(b);
}

static MZ_INLINE int c_int_gt(Scheme_Object *a, Scheme_Object *b)
{
  return SCHEME_INT_VAL(a) > SCHEME_INT_VAL(b);
}

static MZ_INLINE Scheme_Object *c_int_add(Scheme_Object *a, Scheme_Object *b)
{
  return scheme_make_integer(SCHEME_INT_VAL(a) + SCHEME_INT_VAL(b));
}

static MZ_INLINE Scheme_Object *c_int_mult(Scheme_Object *a, Scheme_Object *b)
{
  return scheme_make_integer(SCHEME_INT_VAL(a) * SCHEME_INT_VAL(b));
}

static MZ_INLINE Scheme_Object *c_int_sub(Scheme_Object *a, Scheme_Object *b)
{
  return scheme_make_integer(SCHEME_INT_VAL(a) - SCHEME_INT_VAL(b));
}

#if 0
static MZ_INLINE Scheme_Object *c_int_mult(Scheme_Object *a, Scheme_Object *b)
{
  return scheme_make_integer(SCHEME_INT_VAL(a) * SCHEME_INT_VAL(b));
}
#endif

static MZ_INLINE Scheme_Object *c_int_and(Scheme_Object *a, Scheme_Object *b)
{
  return scheme_make_integer(SCHEME_INT_VAL(a) & SCHEME_INT_VAL(b));
}

static MZ_INLINE Scheme_Object *c_int_rshift(Scheme_Object *a, Scheme_Object *b)
{
  return scheme_make_integer(SCHEME_INT_VAL(a) >> SCHEME_INT_VAL(b));
}

/* Can GC if not in fixnum range */
static Scheme_Object *c_number_add1(Scheme_Object *a)
{
  if (SCHEME_INTP(a)) {
    intptr_t v;
    v = SCHEME_INT_VAL(a);
    if (v < 0x3FFFFFFF)
      return scheme_make_integer(v + 1);
  }

  return scheme_bin_plus(a, scheme_make_integer(1));
}

/* Can GC if not in fixnum range */
static Scheme_Object *c_number_sub1(Scheme_Object *a)
{
  if (SCHEME_INTP(a)) {
    intptr_t v;
    v = SCHEME_INT_VAL(a);
    if (v > -0x3FFFFFFF)
      return scheme_make_integer(v - 1);
  }

  return scheme_bin_minus(a, scheme_make_integer(1));
}

#define c_SCHEME_BIN_NUMBER_COMP(id, op, scheme_id)             \
  static MZ_INLINE int id(Scheme_Object *a, Scheme_Object *b) { \
    if (SCHEME_INTP(a) && SCHEME_INTP(b))                       \
      return (SCHEME_INT_VAL(a) op SCHEME_INT_VAL(b));          \
    return scheme_id(a, b);                                     \
  }
c_SCHEME_BIN_NUMBER_COMP(c_number_eq, ==, scheme_bin_eq)
c_SCHEME_BIN_NUMBER_COMP(c_number_gt, >, scheme_bin_gt)
c_SCHEME_BIN_NUMBER_COMP(c_number_lt, <, scheme_bin_lt)
c_SCHEME_BIN_NUMBER_COMP(c_number_gt_eq, >=, scheme_bin_gt_eq)
c_SCHEME_BIN_NUMBER_COMP(c_number_lt_eq, <=, scheme_bin_lt_eq)

static int c_number_zerop(Scheme_Object *a)
{
  if (SCHEME_INTP(a))
    return SCHEME_INT_VAL(a) == 0;
  else
    return scheme_is_zero(a);
}

#define c_SCHEME_PREDFUNC(id, ID) static MZ_INLINE int id(Scheme_Object *v) { return ID(v); }

c_SCHEME_PREDFUNC(c_scheme_truep, SCHEME_TRUEP)
c_SCHEME_PREDFUNC(c_scheme_falsep, SCHEME_FALSEP)
c_SCHEME_PREDFUNC(c_scheme_nullp, SCHEME_NULLP)
c_SCHEME_PREDFUNC(c_scheme_eof_objectp, SCHEME_EOFP)
c_SCHEME_PREDFUNC(c_scheme_voidp, SCHEME_VOIDP)
c_SCHEME_PREDFUNC(c_scheme_boolp, SCHEME_BOOLP)
c_SCHEME_PREDFUNC(c_scheme_pairp, SCHEME_PAIRP)
c_SCHEME_PREDFUNC(c_scheme_numberp, SCHEME_NUMBERP)
c_SCHEME_PREDFUNC(c_scheme_charp, SCHEME_CHARP)
c_SCHEME_PREDFUNC(c_scheme_chaperone_vectorp, SCHEME_CHAPERONE_VECTORP)
c_SCHEME_PREDFUNC(c_scheme_chaperone_boxp, SCHEME_CHAPERONE_BOXP)
c_SCHEME_PREDFUNC(c_scheme_symbolp, SCHEME_SYMBOLP)
c_SCHEME_PREDFUNC(c_scheme_keywordp, SCHEME_KEYWORDP)
c_SCHEME_PREDFUNC(c_scheme_char_stringp, SCHEME_CHAR_STRINGP)
c_SCHEME_PREDFUNC(c_scheme_byte_stringp, SCHEME_BYTE_STRINGP)
c_SCHEME_PREDFUNC(c_scheme_pathp, SCHEME_PATHP)

static MZ_INLINE int c_scheme_hashp(Scheme_Object *v)
{
  if (SCHEME_NP_CHAPERONEP(v)) v = SCHEME_CHAPERONE_VAL(v);
  return SCHEME_HASHTRP(v) || SCHEME_HASHTP(v) || SCHEME_BUCKTP(v);
}

/* GC *not* possible during scheme_is_list */
static MZ_INLINE int c_scheme_listp(Scheme_Object *v)
{
  return scheme_is_list(v);
}

static MZ_INLINE int c_scheme_char_eq(Scheme_Object *a, Scheme_Object *b)
{
  return SCHEME_CHAR_VAL(a) == SCHEME_CHAR_VAL(b);
}

static MZ_INLINE int c_scheme_char_whitespacep(Scheme_Object *c)
{
  return scheme_isspace(SCHEME_CHAR_VAL(c));
}

static MZ_INLINE Scheme_Object *c_authentic_vector_ref(Scheme_Object *v, Scheme_Object *i)
{
  return SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)];
}

static MZ_INLINE Scheme_Object *c_vector_ref(Scheme_Object *v, Scheme_Object *i)
{
  if (SCHEME_NP_CHAPERONEP(v))
    return scheme_chaperone_vector_ref(v, SCHEME_INT_VAL(i));
  else
    return SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)];
}

static MZ_INLINE Scheme_Object *c_vector_set(Scheme_Object *v, Scheme_Object *i, Scheme_Object *a)
{
  if (SCHEME_NP_CHAPERONEP(v))
    scheme_chaperone_vector_set(v, SCHEME_INT_VAL(i), a);
  SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)] = a;
  return scheme_void;
}

static MZ_INLINE Scheme_Object *c_vector_length(Scheme_Object *v)
{
  if (SCHEME_NP_CHAPERONEP(v)) v = SCHEME_CHAPERONE_VAL(v);
  return scheme_make_integer(SCHEME_VEC_SIZE(v));
}

static MZ_INLINE Scheme_Object *c_string_ref(Scheme_Object *v, Scheme_Object *i)
{
  mzchar c = SCHEME_CHAR_STR_VAL(v)[SCHEME_INT_VAL(i)];
  return scheme_make_character(c);
}

static MZ_INLINE Scheme_Object *c_bytes_ref(Scheme_Object *v, Scheme_Object *i)
{
  int c = ((unsigned char *)SCHEME_BYTE_STR_VAL(v))[SCHEME_INT_VAL(i)];
  return scheme_make_integer(c);
}

static MZ_INLINE Scheme_Object *c_make_box(Scheme_Object *v)
{
  return scheme_box(v);
}

static MZ_INLINE Scheme_Object *c_box_ref(Scheme_Object *b)
{
  if (SCHEME_NP_CHAPERONEP(b))
    return scheme_unbox(b);
  else
    return SCHEME_BOX_VAL(b);
}

static MZ_INLINE Scheme_Object *c_box_set(Scheme_Object *b, Scheme_Object *a)
{
  if (SCHEME_NP_CHAPERONEP(b))
    scheme_set_box(b, a);
  else
    SCHEME_BOX_VAL(b) = a;
  return scheme_void;
}

static MZ_INLINE Scheme_Object *c_weak_box_value(Scheme_Object *o)
{
  o = SCHEME_BOX_VAL(o);
  if (!o)
    return scheme_false;
  return o;
}

#if 0
static MZ_INLINE Scheme_Object *c_weak_box_value2(Scheme_Object *o, Scheme_Object *defval)
{
  o = SCHEME_BOX_VAL(o);
  if (!o)
    return defval;
  return o;
}
#endif

static Scheme_Object *c_make_list1(Scheme_Object *v)
{
  return scheme_make_pair(v, scheme_null);
}

static Scheme_Object *c_make_list2(Scheme_Object *v1, Scheme_Object *v2)
{
  /* A trick to avoid GC registration: put v1 in the wrong place, then move it */
  Scheme_Object *p = scheme_make_pair(v2, v1);
  p = scheme_make_pair(scheme_null, p);
  SCHEME_CAR(p) = SCHEME_CDR(SCHEME_CDR(p));
  SCHEME_CDR(SCHEME_CDR(p)) = scheme_null;
  return p;
}

static MZ_INLINE Scheme_Object *c_pair_car(Scheme_Object *p)
{
  return SCHEME_CAR(p);
}

static MZ_INLINE Scheme_Object *c_pair_cdr(Scheme_Object *p)
{
  return SCHEME_CDR(p);
}

static MZ_INLINE Scheme_Object *c_pair_caar(Scheme_Object *p)
{
  return SCHEME_CAR(SCHEME_CAR(p));
}

static MZ_INLINE Scheme_Object *c_pair_cdar(Scheme_Object *p)
{
  return SCHEME_CDR(SCHEME_CAR(p));
}

static MZ_INLINE Scheme_Object *c_pair_cadr(Scheme_Object *p)
{
  return SCHEME_CAR(SCHEME_CDR(p));
}

static MZ_INLINE Scheme_Object *c_pair_cddr(Scheme_Object *p)
{
  return SCHEME_CDR(SCHEME_CDR(p));
}

/* Only when `default` is definitely not a procedure */
/* Can GC */
static Scheme_Object *c_hash_ref(Scheme_Object *ht, Scheme_Object *key, Scheme_Object *defval)
{
  Scheme_Object *v;
  
  /* The fast path doesn't trigger any GCs: */
  if (SCHEME_HASHTP(ht)) {
    if (!((Scheme_Hash_Table *)ht)->make_hash_indices) {
      v = scheme_eq_hash_get((Scheme_Hash_Table *)ht, key);
      if (v)
        return v;
      else
        return defval;
    }
  } else if (SCHEME_HASHTRP(ht)) {
    if (SAME_TYPE(scheme_eq_hash_tree_type, SCHEME_HASHTR_TYPE(ht))) {
      v = scheme_eq_hash_tree_get((Scheme_Hash_Tree *)ht, key);
      if (v)
        return v;
      else
        return defval;
    }
  }

  {
    Scheme_Object *argv[3];
    MZ_GC_DECL_REG(3);

    MZ_GC_VAR_IN_REG(0, argv[0]);
    MZ_GC_VAR_IN_REG(1, argv[1]);
    MZ_GC_VAR_IN_REG(2, argv[2]);
    MZ_GC_REG();

    argv[0] = ht;
    argv[1] = key;
    argv[2] = defval;

    v = scheme_checked_hash_ref(3, argv);

    MZ_GC_UNREG();

    return v;
  }
}

/* Can GC */
static Scheme_Object *c_hash_ref2(Scheme_Object *ht, Scheme_Object *key)
{
  Scheme_Object *argv[2], *v;
  MZ_GC_DECL_REG(2);

  MZ_GC_VAR_IN_REG(0, argv[0]);
  MZ_GC_VAR_IN_REG(1, argv[1]);
  MZ_GC_REG();

  argv[0] = ht;
  argv[1] = key;

  v = scheme_checked_hash_ref(2, argv);

  MZ_GC_UNREG();

  return v;
}

/* Can GC */
static Scheme_Object *c_hash_set(Scheme_Object *ht, Scheme_Object *key, Scheme_Object *val)
{
  Scheme_Object *argv[3], *v;
  MZ_GC_DECL_REG(3);

  MZ_GC_VAR_IN_REG(0, argv[0]);
  MZ_GC_VAR_IN_REG(1, argv[1]);
  MZ_GC_VAR_IN_REG(2, argv[2]);
  MZ_GC_REG();

  argv[0] = ht;
  argv[1] = key;
  argv[2] = val;

  v = scheme_hash_table_put(3, argv);

  MZ_GC_UNREG();

  return v;
}

/* Can GC in the general case */
static Scheme_Object *c_hash_count(Scheme_Object *ht)
{
  if (SCHEME_CHAPERONEP(ht)) 
    ht = SCHEME_CHAPERONE_VAL(ht);

  if (SCHEME_HASHTP(ht)) {
    Scheme_Hash_Table *t = (Scheme_Hash_Table *)ht;
    return scheme_make_integer(t->count);
  } else if (SCHEME_HASHTRP(ht)) {
    Scheme_Hash_Tree *t = (Scheme_Hash_Tree *)ht;
    return scheme_make_integer(t->count);
  } else {
    Scheme_Object *argv[1], *v;
    MZ_GC_DECL_REG(1);
    
    MZ_GC_VAR_IN_REG(0, argv[0]);
    MZ_GC_REG();
    
    argv[0] = ht;
    
    v = scheme_checked_hash_count(1, argv);
    
    MZ_GC_UNREG();

    return v;
  }
}

/* Can GC */
static Scheme_Object *c_hash_iterate_first(Scheme_Object *ht)
{
  Scheme_Object *argv[1], *v;
  MZ_GC_DECL_REG(1);
    
  MZ_GC_VAR_IN_REG(0, argv[0]);
  MZ_GC_REG();
  
  argv[0] = ht;
  
  v = scheme_hash_table_iterate_start(1, argv);
  
  MZ_GC_UNREG();
  
  return v;
}

/* Can GC */
static Scheme_Object *c_unsafe_immutable_hash_iterate_first(Scheme_Object *ht)
{
  if (SCHEME_NP_CHAPERONEP(ht)) ht = SCHEME_CHAPERONE_VAL(ht);
  return scheme_unsafe_hash_tree_start((Scheme_Hash_Tree *)ht);
}

/* Can GC */
static Scheme_Object *c_unsafe_immutable_hash_iterate_next(Scheme_Object *ht, Scheme_Object *i)
{
  if (SCHEME_NP_CHAPERONEP(ht)) ht = SCHEME_CHAPERONE_VAL(ht);
  return scheme_unsafe_hash_tree_next((Scheme_Hash_Tree *)ht, i);
}

/* Can GC in case of chaperone */
static Scheme_Object *c_unsafe_immutable_hash_iterate_key(Scheme_Object *ht, Scheme_Object *idx)
{
  Scheme_Object *key;
  Scheme_Hash_Tree *subtree;
  int i;
  
  scheme_unsafe_hash_tree_subtree(ht, idx, &subtree, &i);
  key = subtree->els[i];
  
  if (SCHEME_NP_CHAPERONEP(ht))
    return scheme_chaperone_hash_key("unsafe-immutable-hash-iterate-key", ht, idx);
  else
    return key;
}

/* Can GC */
static Scheme_Object *c_unsafe_immutable_hash_iterate_key_value(Scheme_Object *ht, Scheme_Object *idx)
{
  Scheme_Object *key, *res[2], *v;
  Scheme_Hash_Tree *subtree;
  int i;
  MZ_GC_DECL_REG(2);
    
  MZ_GC_VAR_IN_REG(0, res[0]);
  MZ_GC_VAR_IN_REG(1, res[1]);
  MZ_GC_REG();

  scheme_unsafe_hash_tree_subtree(ht, idx, &subtree, &i);
  key = subtree->els[i];

  if (SCHEME_NP_CHAPERONEP(ht)) {
    scheme_chaperone_hash_key_value("unsafe-immutable-hash-iterate-key+value",
                                    ht, subtree->els[i], &res[0], &res[1], 0);
  } else {
    res[0] = key;
    res[1] = scheme_unsafe_hash_tree_access(subtree, i);
  }

  v = scheme_values(2, res);

  MZ_GC_UNREG();

  return v;
}

static MZ_INLINE Scheme_Object *c_prefab_struct_key(Scheme_Object *v)
{
  return scheme_prefab_struct_key(v);
}

static Scheme_Object *c_zero_values()
{
  Scheme_Thread *p = scheme_current_thread;
  p->ku.multiple.count = 0;
  p->ku.multiple.array = NULL;
  return SCHEME_MULTIPLE_VALUES;
}

static MZ_INLINE Scheme_Object *c_last_use(Scheme_Object **r, int i)
{
  Scheme_Object *v = r[i];
  r[i] = NULL;
  return v;
}

/* static MZ_INLINE void c_no_use(Scheme_Object **r, int i) { r[i] = NULL; } */
#define c_no_use(r, i) r[i] = NULL

#ifndef c_VALIDATE_DEBUG

# define SCHEME_UNBOX_VARIABLE(var) (*(Scheme_Object **)(var))
# define SCHEME_UNBOX_VARIABLE_LHS(var) SCHEME_UNBOX_VARIABLE(var)

static Scheme_Object *scheme_box_variable(Scheme_Object *v)
{
  Scheme_Object **b;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, v);
  MZ_GC_REG();

  b = MALLOC_ONE(Scheme_Object *);
  b[0] = v;

  MZ_GC_UNREG();
  return (Scheme_Object *)b;
}

#else

# define SCHEME_UNBOX_VARIABLE(var) SCHEME_BOX_VAL(var)
# define SCHEME_UNBOX_VARIABLE_LHS(var) SCHEME_BOX_VAL(var)

static Scheme_Object *scheme_box_variable(Scheme_Object *v)
{
  return scheme_box(v);
}

static Scheme_Object *c_validate(Scheme_Object *s)
{
  if ((SCHEME_TYPE(s) < 0) || (SCHEME_TYPE(s) > _scheme_last_type_))
    abort();
  return s;
}

#endif

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif
