#include "schpriv.h"
#include "schrunst.h"

READ_ONLY Scheme_Object *scheme_varref_const_p_proc;
READ_ONLY Scheme_Object *scheme_varref_unsafe_p_proc;

SHARED_OK Scheme_Hash_Tree *empty_hash_tree;

SHARED_OK static int validate_compile_result = 0;
SHARED_OK static int recompile_every_compile = 0;
SHARED_OK static int show_linklets = 0;

static Scheme_Object *serializable_symbol;
static Scheme_Object *unsafe_symbol;
static Scheme_Object *static_symbol;
static Scheme_Object *use_prompt_symbol;
static Scheme_Object *uninterned_literal_symbol;
static Scheme_Object *constant_symbol;
static Scheme_Object *consistent_symbol;
static Scheme_Object *noncm_symbol;
static Scheme_Object *immediate_symbol;
static Scheme_Object *omitable_symbol;
static Scheme_Object *folding_symbol;

THREAD_LOCAL_DECL(Scheme_Hash_Table *local_primitive_tables);
THREAD_LOCAL_DECL(extern intptr_t scheme_code_page_total);
THREAD_LOCAL_DECL(extern intptr_t scheme_code_total);
THREAD_LOCAL_DECL(extern intptr_t scheme_code_count);

static Scheme_Object *primitive_table(int argc, Scheme_Object **argv);
static Scheme_Object *primitive_to_position(int argc, Scheme_Object **argv);
static Scheme_Object *position_to_primitive(int argc, Scheme_Object **argv);
static Scheme_Object *primitive_in_category_p(int argc, Scheme_Object **argv);

static Scheme_Object *linklet_p(int argc, Scheme_Object **argv);
static Scheme_Object *compile_linklet(int argc, Scheme_Object **argv);
static Scheme_Object *recompile_linklet(int argc, Scheme_Object **argv);
static Scheme_Object *eval_linklet(int argc, Scheme_Object **argv);
static Scheme_Object *instantiate_linklet(int argc, Scheme_Object **argv);
static Scheme_Object *linklet_import_variables(int argc, Scheme_Object **argv);
static Scheme_Object *linklet_export_variables(int argc, Scheme_Object **argv);

static Scheme_Object *linklet_vm_bytes(int argc, Scheme_Object **argv);
static Scheme_Object *write_linklet_bundle_hash(int argc, Scheme_Object **argv);
static Scheme_Object *read_linklet_bundle_hash(int argc, Scheme_Object **argv);

static Scheme_Object *instance_p(int argc, Scheme_Object **argv);
static Scheme_Object *make_instance(int argc, Scheme_Object **argv);
static Scheme_Object *instance_name(int argc, Scheme_Object **argv);
static Scheme_Object *instance_data(int argc, Scheme_Object **argv);
static Scheme_Object *instance_variable_names(int argc, Scheme_Object **argv);
static Scheme_Object *instance_variable_value(int argc, Scheme_Object **argv);
static Scheme_Object *instance_set_variable_value(int argc, Scheme_Object **argv);
static Scheme_Object *instance_unset_variable(int argc, Scheme_Object **argv);
static Scheme_Object *instance_describe_variable(int argc, Scheme_Object **argv);

static Scheme_Object *variable_p(int argc, Scheme_Object **argv);
static Scheme_Object *variable_instance(int argc, Scheme_Object **argv);
static Scheme_Object *variable_const_p(int argc, Scheme_Object **argv);
static Scheme_Object *variable_unsafe_p(int argc, Scheme_Object **argv);

static Scheme_Linklet *compile_and_or_optimize_linklet(Scheme_Object *form, Scheme_Linklet *linklet,
                                                       Scheme_Object *name,
                                                       Scheme_Object **_import_keys,
                                                       Scheme_Object *get_import,
                                                       int unsafe_mode, int static_mode);

static Scheme_Object *_instantiate_linklet_multi(Scheme_Linklet *linklet, Scheme_Instance *instance,
                                                 int num_instances, Scheme_Instance **instances,
                                                 int use_prompt);

static Scheme_Hash_Tree *push_prefix(Scheme_Linklet *linklet, Scheme_Instance *instance,
                                     int num_instances, Scheme_Instance **instances,
                                     Scheme_Hash_Tree *source_names);
static void pop_prefix();
static Scheme_Object *suspend_prefix();
static void resume_prefix(Scheme_Object *v);

static Scheme_Bucket *make_bucket(Scheme_Object *key, Scheme_Object *val, Scheme_Instance *inst);

#ifdef MZ_PRECISE_GC
static void mark_pruned_prefixes(struct NewGC *gc);
static int check_pruned_prefix(void *p);
#endif

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

void scheme_init_linklet(Scheme_Startup_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  REGISTER_SO(serializable_symbol);
  REGISTER_SO(unsafe_symbol);
  REGISTER_SO(static_symbol);
  REGISTER_SO(use_prompt_symbol);
  REGISTER_SO(uninterned_literal_symbol);
  serializable_symbol = scheme_intern_symbol("serializable");
  unsafe_symbol = scheme_intern_symbol("unsafe");
  static_symbol = scheme_intern_symbol("static");
  use_prompt_symbol = scheme_intern_symbol("use-prompt");
  uninterned_literal_symbol = scheme_intern_symbol("uninterned-literal");

  REGISTER_SO(constant_symbol);
  REGISTER_SO(consistent_symbol);
  constant_symbol = scheme_intern_symbol("constant");
  consistent_symbol = scheme_intern_symbol("consistent");

  REGISTER_SO(noncm_symbol);
  REGISTER_SO(immediate_symbol);
  REGISTER_SO(omitable_symbol);
  REGISTER_SO(folding_symbol);
  noncm_symbol = scheme_intern_symbol("noncm");
  immediate_symbol = scheme_intern_symbol("immediate");
  omitable_symbol = scheme_intern_symbol("omitable");
  folding_symbol = scheme_intern_symbol("folding");

  scheme_switch_prim_instance(env, "#%linklet");

  ADD_IMMED_PRIM("primitive->compiled-position", primitive_to_position, 1, 1, env);
  ADD_IMMED_PRIM("compiled-position->primitive", position_to_primitive, 1, 1, env);
  ADD_IMMED_PRIM("primitive-in-category?", primitive_in_category_p, 2, 2, env);

  ADD_FOLDING_PRIM("linklet?", linklet_p, 1, 1, 1, env);
  ADD_PRIM_W_ARITY2("compile-linklet", compile_linklet, 1, 5, 2, 2, env);
  ADD_PRIM_W_ARITY2("recompile-linklet", recompile_linklet, 1, 5, 2, 2, env);
  ADD_IMMED_PRIM("eval-linklet", eval_linklet, 1, 1, env);
  ADD_PRIM_W_ARITY2("instantiate-linklet", instantiate_linklet, 2, 4, 0, -1, env);
  ADD_PRIM_W_ARITY("linklet-import-variables", linklet_import_variables, 1, 1, env);
  ADD_PRIM_W_ARITY("linklet-export-variables", linklet_export_variables, 1, 1, env);

  ADD_PRIM_W_ARITY("linklet-virtual-machine-bytes", linklet_vm_bytes, 0, 0, env);
  ADD_PRIM_W_ARITY("write-linklet-bundle-hash", write_linklet_bundle_hash, 2, 2, env);
  ADD_PRIM_W_ARITY("read-linklet-bundle-hash", read_linklet_bundle_hash, 1, 1, env);

  ADD_FOLDING_PRIM("instance?", instance_p, 1, 1, 1, env);
  ADD_PRIM_W_ARITY("make-instance", make_instance, 1, -1, env);
  ADD_PRIM_W_ARITY("instance-name", instance_name, 1, 1, env);
  ADD_PRIM_W_ARITY("instance-data", instance_data, 1, 1, env);
  ADD_PRIM_W_ARITY("instance-variable-names", instance_variable_names, 1, 1, env);
  ADD_PRIM_W_ARITY2("instance-variable-value", instance_variable_value, 2, 3, 0, -1, env);
  ADD_PRIM_W_ARITY("instance-set-variable-value!", instance_set_variable_value, 3, 4, env);
  ADD_PRIM_W_ARITY("instance-unset-variable!", instance_unset_variable, 2, 2, env);
  ADD_PRIM_W_ARITY("instance-describe-variable!", instance_describe_variable, 3, 3, env);

  ADD_FOLDING_PRIM_UNARY_INLINED("variable-reference?", variable_p, 1, 1, 1, env);
  ADD_IMMED_PRIM("variable-reference->instance", variable_instance, 1, 2, env);

  REGISTER_SO(scheme_varref_const_p_proc);
  scheme_varref_const_p_proc = scheme_make_prim_w_arity(variable_const_p, 
                                                        "variable-reference-constant?", 
                                                        1, 1);
  scheme_addto_prim_instance("variable-reference-constant?", scheme_varref_const_p_proc, env);

  REGISTER_SO(scheme_varref_unsafe_p_proc);
  scheme_varref_unsafe_p_proc = scheme_make_prim_w_arity(variable_unsafe_p, 
                                                         "variable-reference-from-unsafe?", 
                                                         1, 1);
  scheme_addto_prim_instance("variable-reference-from-unsafe?", scheme_varref_unsafe_p_proc, env);

  scheme_restore_prim_instance(env);

  if (scheme_getenv("PLT_VALIDATE_COMPILE")) {
    /* Enables validation of bytecode as it is generated,
       to double-check that the compiler is producing
       valid bytecode as it should. */
    validate_compile_result = 1;
  }

  {
    /* Enables re-running the optimizer N times on every compilation. */
    const char *s;
    s = scheme_getenv("PLT_RECOMPILE_COMPILE");
    if (s) {
      int i = 0;
      while ((s[i] >= '0') && (s[i] <= '9')) {
        recompile_every_compile = (recompile_every_compile * 10) + (s[i]-'0');
        i++;
      }
      if (recompile_every_compile <= 0)
        recompile_every_compile = 1;
      else if (recompile_every_compile > 32)
        recompile_every_compile = 32;
    }
  }

  if (scheme_getenv("PLT_LINKLET_SHOW"))
    show_linklets = 1;
}

void scheme_init_unsafe_linklet(Scheme_Startup_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  scheme_switch_prim_instance(env, "#%linklet");

  ADD_IMMED_PRIM("primitive-table", primitive_table, 1, 2, env);

  scheme_restore_prim_instance(env);
}

void scheme_init_linklet_places(void)
{
#ifdef MZ_PRECISE_GC
  scheme_prefix_finalize = (Scheme_Prefix *)0x1; /* 0x1 acts as a sentenel */
  scheme_inc_prefix_finalize = (Scheme_Prefix *)0x1;
  GC_set_post_propagate_hook(mark_pruned_prefixes);
  GC_set_treat_as_incremental_mark(scheme_prefix_type, check_pruned_prefix);
#endif
}

/*========================================================================*/
/*                    linklet and instance functions                      */
/*========================================================================*/

static Scheme_Object *primitive_table(int argc, Scheme_Object *argv[])
{
  Scheme_Hash_Table *table;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract("primitive-table", "symbol?", 0, argc, argv);
  if ((argc > 1) && !SCHEME_HASHTRP(argv[1]))
    scheme_wrong_contract("primitive-table", "(and/c hash? immutable?)", 1, argc, argv);

  table = (Scheme_Hash_Table *)scheme_hash_get(scheme_startup_env->primitive_tables, argv[0]);
  if (!table && local_primitive_tables)
    table = (Scheme_Hash_Table *)scheme_hash_get(local_primitive_tables, argv[0]);
  
  if (!table) {
    if (argc > 1) {
      if (!local_primitive_tables) {
        REGISTER_SO(local_primitive_tables);
        local_primitive_tables = scheme_make_hash_table(SCHEME_hash_ptr);
      }
      scheme_hash_set(local_primitive_tables, argv[0], argv[1]);
    } else
      return scheme_false;
  }

  if (argc < 2)
    return (Scheme_Object *)table;
  else
    return scheme_void;
}

static Scheme_Object *primitive_to_position(int argc, Scheme_Object **argv)
{
  Scheme_Object *pos;
  pos = scheme_hash_get(scheme_startup_env->primitive_ids_table, argv[0]);
  return (pos ? pos : scheme_false);
}

static Scheme_Object *position_to_primitive(int argc, Scheme_Object **argv)
{
  Scheme_Object *v;
  if (SCHEME_INTP(argv[0]) && (SCHEME_INT_VAL(argv[0]) >= 0))
    v = scheme_position_to_builtin(SCHEME_INT_VAL(argv[0]));
  else
    v = NULL;
  return (v ? v : scheme_false);
}

static Scheme_Object *primitive_in_category_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *v, *cat;
  int r;
  
  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract("primitive-in-category?", "symbol?", 0, argc, argv);
  cat = argv[1];
  if (!SCHEME_SYMBOLP(cat))
    scheme_wrong_contract("primitive-in-category?", "symbol?", 1, argc, argv);

  v = scheme_hash_get(scheme_startup_env->all_primitives_table, argv[0]);
  if (!v)
    r = 0;
  else if (SCHEME_PRIMP(v)) {
    int opt = ((Scheme_Prim_Proc_Header *)v)->flags & SCHEME_PRIM_OPT_MASK;
    if (SAME_OBJ(cat, noncm_symbol)) {
      r = (opt >= SCHEME_PRIM_OPT_NONCM);
      /* Remove closures from noncm */
      if (((Scheme_Prim_Proc_Header *)v)->flags & SCHEME_PRIM_IS_CLOSURE)
        r = 0;
    } else if (SAME_OBJ(cat, immediate_symbol))
      r  = (opt >= SCHEME_PRIM_OPT_IMMEDIATE);
    else if (SAME_OBJ(cat, folding_symbol))
      r = (opt >= SCHEME_PRIM_OPT_FOLDING);
    else if (SAME_OBJ(cat, omitable_symbol))
      r = (SCHEME_PRIM_PROC_OPT_FLAGS(v) & (SCHEME_PRIM_IS_OMITABLE_ANY
                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                            | SCHEME_PRIM_IS_UNSAFE_OMITABLE));
    else
      r = 0;
  } else
    r = 0;

  return (r ? scheme_true : scheme_false);
}

static Scheme_Object *linklet_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_type)
          ? scheme_true
          : scheme_false);
}

static void check_linklet_allowed(const char *who, Scheme_Linklet *linklet)
{
  if (linklet->reject_eval) {
    scheme_raise_exn(MZEXN_FAIL,
                     "%s: cannot use linklet loaded with non-original code inspector",
                     who);
  }
}

void extract_import_info(const char *who, int argc, Scheme_Object **argv,
                         Scheme_Object **_import_keys, Scheme_Object **_get_import)
{
  
  if (argc > 2) {
    *_import_keys = argv[2];
    if (SCHEME_FALSEP(*_import_keys))
      *_import_keys = NULL;
    else if (!SCHEME_VECTORP(*_import_keys))
      scheme_wrong_contract(who, "(or/c vector? #f)", 2, argc, argv);
  } else
    *_import_keys = NULL;

  if (argc > 3) {
    scheme_check_proc_arity2(who, 1, 3, argc, argv, 1);
    if (SCHEME_TRUEP(argv[3])) {
      if (!*_import_keys) {
        scheme_contract_error(who,
                              "no vector supplied for import keys, but import-getting function provided;\n"
                              " the function argument must be `#f` when the vector argument is `#f`",
                              "import-getting function", 1, argv[3],
                              NULL);
      }
      *_get_import = argv[3];
    } else
      *_get_import = NULL;
  } else
    *_get_import = NULL;
}

static void parse_compile_options(const char *who, int arg_pos,
                                  int argc, Scheme_Object **argv,
                                  int *_unsafe, int *_static_mode)
{
  Scheme_Object *redundant = NULL, *flag, *flags = argv[arg_pos];
  int serializable = 0;
  int unsafe = *_unsafe;
  int static_mode = *_static_mode;
  int use_prompt_mode = 0;
  int uninterned_literal_mode = 0;
  
  while (SCHEME_PAIRP(flags)) {
    flag = SCHEME_CAR(flags);
    if (SAME_OBJ(flag, serializable_symbol)) {
      if (serializable && !redundant)
        redundant = flag;
      serializable = 1;
    } else if (SAME_OBJ(flag, unsafe_symbol)) {
      if (unsafe && !redundant)
        redundant = flag;
      unsafe = 1;
    } else if (SAME_OBJ(flag, static_symbol)) {
      if (static_mode && !redundant)
        redundant = flag;
      static_mode = 1;
    } else if (SAME_OBJ(flag, use_prompt_symbol)) {
      if (use_prompt_mode && !redundant)
        redundant = flag;
      use_prompt_mode = 1;
    } else if (SAME_OBJ(flag, uninterned_literal_symbol)) {
      if (uninterned_literal_mode && !redundant)
        redundant = flag;
      uninterned_literal_mode = 1;
    } else
      break;
    flags = SCHEME_CDR(flags);
  }

  if (!SCHEME_NULLP(flags))
    scheme_wrong_contract("compile-linklet",
                          "(listof/c 'serializable 'unsafe 'static 'use-prompt 'uninterned-literal)",
                          arg_pos, argc, argv);

  if (redundant)
    scheme_contract_error("compile-linklet", "redundant option",
                          "redundant option", 1, redundant,
                          "supplied options", 1, argv[arg_pos],
                          NULL);

  *_unsafe = unsafe;
  *_static_mode = static_mode;
}

static Scheme_Object *compile_linklet(int argc, Scheme_Object **argv)
{
  Scheme_Object *name, *e, *import_keys, *get_import, *a[2];
  int unsafe = 0, static_mode = 0;

  /* Last argument, `serializable?`, is ignored */

  extract_import_info("compile-linklet", argc, argv, &import_keys, &get_import);

  if ((argc > 1) && SCHEME_TRUEP(argv[1]))
    name = argv[1];
  else
    name = scheme_intern_symbol("anonymous");

  e = argv[0];
  if (!SCHEME_STXP(e))
    e = scheme_datum_to_syntax(e, scheme_false, DTS_CAN_GRAPH);

  if (show_linklets) {
    char *s;
    intptr_t s_len;
    s = scheme_write_to_string(scheme_syntax_to_datum(e), &s_len);
    printf("%s\n", s);
  }

  if (argc > 4)
    parse_compile_options("compile-linklet", 4, argc, argv, &unsafe, &static_mode);

  e = (Scheme_Object *)compile_and_or_optimize_linklet(e, NULL, name, &import_keys, get_import,
                                                       unsafe, static_mode);

  if (import_keys) {
    a[0] = e;
    a[1] = import_keys;
    return scheme_values(2, a);
  } else
    return e;
}

static Scheme_Object *recompile_linklet(int argc, Scheme_Object **argv)
{
  Scheme_Object *name, *import_keys, *get_import, *a[2];
  Scheme_Linklet *linklet;
  int unsafe = 0, static_mode = 0;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_type))
    scheme_wrong_contract("recompile-linklet", "linklet?", 0, argc, argv);

  linklet = (Scheme_Linklet *)argv[0];

  check_linklet_allowed("recompile-linklet", linklet);

  extract_import_info("recompile-linklet", argc, argv, &import_keys, &get_import);

  if ((argc > 1) && SCHEME_TRUEP(argv[1]))
    name = argv[1];
  else
    name = ((Scheme_Linklet *)argv[0])->name;

  if (import_keys && (SCHEME_VEC_SIZE(import_keys) != SCHEME_VEC_SIZE(linklet->importss))) {
    scheme_contract_error("recompile-linklet",
                          "given number of import keys does not match import count of linklet",
                          "linklet", 1, linklet,
                          "linklet imports", 1, scheme_make_integer(SCHEME_VEC_SIZE(linklet->importss)),
                          "given keys", 1, scheme_make_integer(SCHEME_VEC_SIZE(import_keys)),
                          NULL);
  }

  if (argc > 4)
    parse_compile_options("recompile-linklet", 4, argc, argv, &unsafe, &static_mode);
  
  linklet = compile_and_or_optimize_linklet(NULL, linklet, name, &import_keys, get_import,
                                            unsafe, static_mode);

  if (import_keys) {
    a[0] = (Scheme_Object *)linklet;
    a[1] = import_keys;

    return scheme_values(2, a);
  } else
    return (Scheme_Object *)linklet;
}

static Scheme_Object *eval_linklet(int argc, Scheme_Object **argv)
{
  /* "Evaluation" is not necessary before instantiation, but it makes
     the linklet JIT-prepared (so the JIT-prepared linklet could be
     reused, for example) while also making the linklet ineligible for
     marshaling. */
  Scheme_Linklet *linklet;
  
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_type))
    scheme_wrong_contract("eval-linklet", "linklet?", 0, argc, argv);

  linklet = (Scheme_Linklet *)argv[0];

  check_linklet_allowed("eval-linklet", linklet);
  
  if (!linklet->jit_ready) {
    Scheme_Object *b;
    b = scheme_get_param(scheme_current_config(), MZCONFIG_USE_JIT);
    if (SCHEME_TRUEP(b)) {
      /* Make a JIT-prepable linklet --- but don't actually prep until
         forced by instantiation. */
      linklet = scheme_jit_linklet(linklet, 1);
    }
  }

#ifdef MZ_USE_JIT
  if (linklet->native_lambdas) {
    Scheme_Object *l;
    l = linklet->native_lambdas;
    linklet->native_lambdas = NULL;
    while (SCHEME_PAIRP(l)) {
      scheme_force_jit_generate((Scheme_Native_Lambda *)SCHEME_CAR(l));
      l = SCHEME_CDR(l);
    }
  }
#endif

  return (Scheme_Object *)linklet;
}

static Scheme_Object *linklet_vm_bytes(int argc, Scheme_Object **argv)
{
  return scheme_make_byte_string("racket");
}

static Scheme_Object *read_linklet_bundle_hash(int argc, Scheme_Object **argv)
{
  if (!SCHEME_INPUT_PORTP(argv[0]))
    scheme_wrong_contract("read-linklet-bundle-hash", "input-port?", 0, argc, argv);

  return scheme_read_linklet_bundle_hash(argv[0]);
}

static Scheme_Object *write_linklet_bundle_hash(int argc, Scheme_Object **argv)
{
  mzlonglong pos;
  Scheme_Object *k, *v;
  Scheme_Hash_Tree *hash;
  
  if (!SCHEME_HASHTRP(argv[0])
      || !SAME_TYPE(scheme_eq_hash_tree_type, SCHEME_HASHTR_TYPE(argv[0])))
    scheme_wrong_contract("write-linklet-bundle-hash",
                          "(and/c hash? hash-eq? immutable? (not/c impersonator?))",
                          0, argc, argv);

  if (!SCHEME_OUTPUT_PORTP(argv[1]))
    scheme_wrong_contract("write-linklet-bundle-hash", "output-port?", 0, argc, argv);

  hash = (Scheme_Hash_Tree *)argv[0];

  /* mapping: keys must be symbols and fixnums */

  pos = scheme_hash_tree_next(hash, -1);
  while (pos != -1) {
    scheme_hash_tree_index(hash, pos, &k, &v);
    if (!SCHEME_SYMBOLP(k) && !SCHEME_INTP(k)) {
      scheme_contract_error("write-linklet-bundle-hash",
                            "key in given hash is not a symbol or fixnum",
                            "key", 1, k,
                            NULL);
    }
    pos = scheme_hash_tree_next(hash, pos);
  }

  v = scheme_alloc_small_object();
  v->type = scheme_linklet_bundle_type;
  SCHEME_PTR_VAL(v) = argv[0];

  scheme_write(v, argv[1]);

  return scheme_void;
}

static Scheme_Object *instantiate_linklet(int argc, Scheme_Object **argv)
{
  Scheme_Linklet *linklet;
  Scheme_Object *l;
  Scheme_Instance *inst, **instances;
  int len = 0, num_importss, use_prompt, return_instance;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_type))
    scheme_wrong_contract("instantiate-linklet", "linklet?", 0, argc, argv);

  l = argv[1];
  while (!SCHEME_NULLP(l)) {
    if (!SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(l)), scheme_instance_type))
      break;
    l = SCHEME_CDR(l);
    len++;
  }
  if (!SCHEME_NULLP(l))
    scheme_wrong_contract("instantiate-linklet", "(listof instance?)", 1, argc, argv);

  linklet = (Scheme_Linklet *)argv[0];
  check_linklet_allowed("instantiate-linklet", linklet);
  num_importss = SCHEME_VEC_SIZE(linklet->importss);
  if (len != num_importss)
    scheme_contract_error("instantiate-linklet",
                          "given number of instances does not match import count of linklet",
                          "linklet", 1, linklet,
                          "expected imports", 1, scheme_make_integer(num_importss),
                          "given instances", 1, scheme_make_integer(len),
                          NULL);

  if ((argc > 2) && SCHEME_TRUEP(argv[2])) {
    if (!SAME_TYPE(SCHEME_TYPE(argv[2]), scheme_instance_type))
      scheme_wrong_contract("instantiate-linklet", "(or/c instance? #f)", 2, argc, argv);
    inst = (Scheme_Instance *)argv[2];
    return_instance = 0;
  } else {
    inst = scheme_make_instance(linklet->name, scheme_false);
    return_instance = 1;
  }

  use_prompt = ((argc < 4) || SCHEME_TRUEP(argv[3]));

  instances = MALLOC_N(Scheme_Instance*, len);
  l = argv[1];
  len = 0;
  while (!SCHEME_NULLP(l)) {
    instances[len++] = (Scheme_Instance *)SCHEME_CAR(l);
    l = SCHEME_CDR(l);
  }

  if (!return_instance)
    return _instantiate_linklet_multi(linklet, inst, len, instances, use_prompt);
  else {
    (void)_instantiate_linklet_multi(linklet, inst, len, instances, use_prompt);
    return (Scheme_Object *)inst;
  }
}

static Scheme_Object *linklet_import_variables(int argc, Scheme_Object **argv)
{
  Scheme_Linklet *linklet;
  int i, j;
  Scheme_Object *l, *ll = scheme_null;
  
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_type))
    scheme_wrong_contract("linklet-import-variables", "linklet?", 0, argc, argv);

  linklet = (Scheme_Linklet *)argv[0];

  for (i = SCHEME_VEC_SIZE(linklet->importss); i--; ) {
    l = scheme_null;
    for (j = SCHEME_VEC_SIZE(SCHEME_VEC_ELS(linklet->importss)[i]); j--; ) {
      l = scheme_make_pair(SCHEME_VEC_ELS(SCHEME_VEC_ELS(linklet->importss)[i])[j], l);
    }
    ll = scheme_make_pair(l, ll);
  }

  return ll;
}

static Scheme_Object *linklet_export_variables(int argc, Scheme_Object **argv)
{
  Scheme_Linklet *linklet;
  int i;
  Scheme_Object *l = scheme_null;
  
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_type))
    scheme_wrong_contract("linklet-export-variables", "linklet?", 0, argc, argv);

  linklet = (Scheme_Linklet *)argv[0];

  for (i = linklet->num_exports; i--; ) {
    l = scheme_make_pair(SCHEME_VEC_ELS(linklet->defns)[i], l);
  }

  return l;
}

static Scheme_Object *instance_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type)
          ? scheme_true
          : scheme_false);
}

static int parse_constantness_flag(const char *who, int i, int argc, Scheme_Object **argv)
{
  int set_flags = 0;

  if (SCHEME_FALSEP(argv[i]))
    set_flags = 0;
  else if (SAME_OBJ(argv[i], constant_symbol))
    set_flags = GLOB_IS_IMMUTATED;
  else if (SAME_OBJ(argv[i], consistent_symbol))
    set_flags = GLOB_IS_IMMUTATED | GLOB_IS_CONSISTENT;
  else
    scheme_wrong_contract(who, "(or/c #f 'constant 'consistent)", i, argc, argv);

  return set_flags;
}

static Scheme_Object *make_instance(int argc, Scheme_Object **argv)
{
  Scheme_Instance *inst;
  int i;

  inst = scheme_make_instance(argv[0], (argc > 1) ? argv[1] : scheme_false);

  if (argc > 3) {
    Scheme_Bucket **a, *b;
    int set_flags = 0;

    set_flags = parse_constantness_flag("make-instance", 2, argc, argv);

    i = 3;
    a = MALLOC_N(Scheme_Bucket *, (argc - i) >> 1);
    
    for (; i < argc; i += 2) {
      if (!SCHEME_SYMBOLP(argv[i]))
        scheme_wrong_contract("make-instance", "symbol?", i, argc, argv);
      if (i+1 == argc)
        scheme_contract_error("make-instance",
                              "value missing for variable name",
                              "variable name", 1, argv[i],
                              NULL);
      b = make_bucket(argv[i], argv[i+1], inst);
      if (set_flags)
        ((Scheme_Bucket_With_Flags *)b)->flags |= set_flags;
      a[(i-2)>>1] = b;
    }

    inst->array_size = (argc-2)>>1;
    inst->variables.a = a;
  }

  return (Scheme_Object *)inst;
}

static Scheme_Object *instance_name(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type))
    scheme_wrong_contract("instance-name", "instance?", 0, argc, argv);

  return ((Scheme_Instance *)argv[0])->name;
}

static Scheme_Object *instance_data(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type))
    scheme_wrong_contract("instance-data", "instance?", 0, argc, argv);

  return ((Scheme_Instance *)argv[0])->data;
}
  
static Scheme_Object *instance_variable_names(int argc, Scheme_Object **argv)
{
  Scheme_Bucket *b;
  int i;
  Scheme_Object *l = scheme_null;
  Scheme_Instance *inst;
  
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type))
    scheme_wrong_contract("instance-variable-names", "instance?", 0, argc, argv);

  inst = (Scheme_Instance *)argv[0];

  if (inst->array_size) {
    for (i = inst->array_size; i--; ) {
      l = scheme_make_pair((Scheme_Object *)inst->variables.a[i]->key, l);
    }
  } else if (inst->variables.bt) {
    for (i = inst->variables.bt->size; i--; ) {
      b = inst->variables.bt->buckets[i];
      if (b && b->val) {
        l = scheme_make_pair((Scheme_Object *)b->key, l);
      }
    }
  }

  return l;
}

static Scheme_Object *instance_variable_value(int argc, Scheme_Object **argv)
{
  Scheme_Instance *inst;
  Scheme_Bucket *b;
    
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type))
    scheme_wrong_contract("instance-variable-value", "instance?", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_contract("instance-variable-value", "symbol?", 1, argc, argv);

  inst = (Scheme_Instance *)argv[0];

  b = scheme_instance_variable_bucket_or_null(argv[1], inst);
  if (b && b->val)
    return b->val;

  if (argc > 2) {
    if (SCHEME_PROCP(argv[2]))
      return _scheme_tail_apply(argv[2], 0, NULL);
    return argv[2];
  }

  scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                   "instance-variable-value: instance variable not found\n"
                   "  instance: %V\n"
                   "  name: %S",
                   inst->name,
                   argv[1]);
  return NULL;
}

static Scheme_Object *instance_set_variable_value(int argc, Scheme_Object **argv)
{
  Scheme_Bucket *b;
  int set_flags = 0;
  
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type))
    scheme_wrong_contract("instance-set-variable-value!", "instance?", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_contract("instance-set-variable-value!", "symbol?", 1, argc, argv);
  if (argc > 3)
    set_flags = parse_constantness_flag("instance-set-variable-value!", 3, argc, argv);

  b = scheme_instance_variable_bucket(argv[1], (Scheme_Instance *)argv[0]);

  scheme_set_global_bucket("instance-set-variable-value!", b, argv[2], 1);
    
  b->val = argv[2];
  if (set_flags)
    ((Scheme_Bucket_With_Flags *)b)->flags |= set_flags;

  return scheme_void;
}

static Scheme_Object *instance_unset_variable(int argc, Scheme_Object **argv)
{
  Scheme_Bucket *b;
    
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type))
    scheme_wrong_contract("instance-unset-variable!", "instance?", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_contract("instance-unset-variable!", "symbol?", 1, argc, argv);

  b = scheme_instance_variable_bucket(argv[1], (Scheme_Instance *)argv[0]);
  b->val = NULL;

  return scheme_void;
}

static Scheme_Object *instance_describe_variable(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type))
    scheme_wrong_contract("instance-describe-variable!", "instance?", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_contract("instance-describe-variable!", "symbol?", 1, argc, argv);

  return scheme_void;
}

static Scheme_Object *variable_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_global_ref_type)
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *variable_instance(int argc, Scheme_Object **argv)
{
  Scheme_Object *v;

  v = argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(v), scheme_global_ref_type))
    scheme_wrong_contract("variable-reference->instance", "variable-reference?", 0, argc, argv);

  if ((argc < 2) || SCHEME_FALSEP(argv[1])) {
    /* Definition instance might be a primitive-table symbol, or it might be #f for "anonymous": */
    v = SCHEME_PTR1_VAL(argv[0]);
    if (SCHEME_SYMBOLP(v) || SCHEME_FALSEP(v))
      return v;
    else if (SAME_OBJ(v, scheme_true))
      return SCHEME_PTR2_VAL(argv[0]); /* same as use instance for a local */
    else {
      v = (Scheme_Object *)scheme_get_bucket_home((Scheme_Bucket *)v);
      if (!v) {
        /* The definition instance was GCed? Return the use-site instance */
        return SCHEME_PTR2_VAL(argv[0]);
      }
      return v;
    }
  } else {
    /* Get use instance: */
    return SCHEME_PTR2_VAL(argv[0]);
  }
}

static Scheme_Object *variable_const_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *v;

  v = argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(v), scheme_global_ref_type))
    scheme_wrong_contract("variable-reference-constant?", "variable-reference?", 0, argc, argv);

  if (SCHEME_VARREF_FLAGS(v) & VARREF_IS_CONSTANT)
    return scheme_true;

  v = SCHEME_PTR1_VAL(v);
  if (!SCHEME_FALSEP(v)) {
    if (SCHEME_SYMBOLP(v)
        || (((Scheme_Bucket_With_Flags *)v)->flags & GLOB_IS_IMMUTATED))
      return scheme_true;
  }

  return scheme_false;
}

static Scheme_Object *variable_unsafe_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *v;

  v = argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(v), scheme_global_ref_type))
    scheme_wrong_contract("variable-reference-from-unsafe?", "variable-reference?", 0, argc, argv);

  if (SCHEME_VARREF_FLAGS(v) & VARREF_FROM_UNSAFE)
    return scheme_true;
  else
    return scheme_false;
}

/*========================================================================*/
/*                       instance variable buckets                        */
/*========================================================================*/

Scheme_Object *scheme_get_home_weak_link(Scheme_Instance *i)
{
  if (!i->weak_self_link) {
    Scheme_Object *wb;
    if (scheme_starting_up)
      wb = scheme_box((Scheme_Object *)i);
    else
      wb = scheme_make_weak_box((Scheme_Object *)i);
    i->weak_self_link = wb;
  }

  return i->weak_self_link;
}

Scheme_Instance *scheme_get_bucket_home(Scheme_Bucket *b)
{
  Scheme_Object *l;

  l = ((Scheme_Bucket_With_Home *)b)->home_link;
  if (l) {
    if (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_STRONG_HOME_LINK)
      return (Scheme_Instance *)l;
    else
      return (Scheme_Instance *)SCHEME_WEAK_BOX_VAL(l);
  } else
    return NULL;
}

void scheme_set_bucket_home(Scheme_Bucket *b, Scheme_Instance *e)
{
  if (!((Scheme_Bucket_With_Home *)b)->home_link) {
    if (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_STRONG_HOME_LINK)
      ((Scheme_Bucket_With_Home *)b)->home_link = (Scheme_Object *)e;
    else {
      Scheme_Object *link;
      link = scheme_get_home_weak_link(e);
      ((Scheme_Bucket_With_Home *)b)->home_link = link;
    }
  }
}

static Scheme_Bucket *make_bucket(Scheme_Object *key, Scheme_Object *val, Scheme_Instance *inst)
{
  Scheme_Bucket *b;

  b = (Scheme_Bucket *)MALLOC_ONE_TAGGED(Scheme_Bucket_With_Home);
  b->so.type = scheme_variable_type;
  b->key = (char *)key;
  b->val = val;
  scheme_set_bucket_home(b, inst);
  
  return b;
}

Scheme_Instance *scheme_make_instance(Scheme_Object *name, Scheme_Object *data)
{
  Scheme_Instance *inst;

  if (!empty_hash_tree) {
    REGISTER_SO(empty_hash_tree);
    empty_hash_tree = scheme_make_hash_tree(0);
  }
  
  inst = MALLOC_ONE_TAGGED(Scheme_Instance);
  inst->iso.so.type = scheme_instance_type;

  inst->name = (name ? name : scheme_false);
  inst->data = data;

  inst->source_names = empty_hash_tree;

  if (scheme_starting_up) {
    /* Avoid recording procedure-implementation details in bytecode
       that uses the instances that are created on startup. */
    SCHEME_INSTANCE_FLAGS(inst) |= SCHEME_INSTANCE_USE_IMPRECISE;
  }

  return inst;
}

void scheme_instance_to_hash_mode(Scheme_Instance *inst, int size_estimate)
{
  Scheme_Bucket_Table *variables;
  Scheme_Bucket **a;

  if (inst->array_size) {
    size_estimate = inst->array_size * 2;
    a = inst->variables.a;
  } else
    a = NULL;

  variables = scheme_make_bucket_table(size_estimate, SCHEME_hash_ptr);
  variables->with_home = 1;

  inst->variables.bt = variables;
  inst->array_size = 0;

  if (a) {
    size_estimate >>= 1;
    while (size_estimate--) {
      scheme_add_bucket_to_table(inst->variables.bt, a[size_estimate]);
    }
  }
}

Scheme_Bucket *scheme_instance_variable_bucket(Scheme_Object *symbol, Scheme_Instance *inst)
{
  Scheme_Bucket *b;

  if (inst->array_size) {
    int i;
    for (i = inst->array_size; i--; ) {
      b = inst->variables.a[i];
      if (SAME_OBJ(symbol, (Scheme_Object *)b->key))
        return b;
    }
  }

  if (inst->array_size || !inst->variables.bt)
    scheme_instance_to_hash_mode(inst, 0);
  
  b = scheme_bucket_from_table(inst->variables.bt, (char *)symbol);
  ASSERT_IS_VARIABLE_BUCKET(b);
  if (SCHEME_FALSEP(symbol))
    ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_STRONG_HOME_LINK;

  scheme_set_bucket_home(b, inst);

  return b;
}

Scheme_Bucket *scheme_instance_variable_bucket_or_null(Scheme_Object *symbol, Scheme_Instance *inst)
{
  Scheme_Bucket *b;

  if (inst->array_size) {
    int i;
    for (i = inst->array_size; i--; ) {
      b = inst->variables.a[i];
      if (SAME_OBJ(symbol, (Scheme_Object *)b->key))
        return b;
    }
    return NULL;
  } else if (!inst->variables.bt)
    return NULL;

  b = scheme_bucket_or_null_from_table(inst->variables.bt, (char *)symbol, 0);
  if (b) {
    ASSERT_IS_VARIABLE_BUCKET(b);
    scheme_set_bucket_home(b, inst);
  }

  return b;
}

/*========================================================================*/
/*                          managing bucket names                         */
/*========================================================================*/

static Scheme_Object *generate_bucket_name(Scheme_Object *old_name, Scheme_Instance *instance)
{
  int search_start = 0;
  char buf[32];
  Scheme_Object *n;
  
  while (1) {
    sprintf(buf, ".%d", search_start);
    n = scheme_intern_exact_parallel_symbol(buf, strlen(buf));
    n = scheme_symbol_append(old_name, n);
    if (!scheme_instance_variable_bucket_or_null(n, instance))
      return n;
    search_start++;
  }
}

static Scheme_Hash_Tree *update_source_names(Scheme_Hash_Tree *source_names,
                                             Scheme_Object *old_name, Scheme_Object *new_name)
{
  Scheme_Object *v;

  v = scheme_hash_tree_get(source_names, old_name);
  if (v)
    return scheme_hash_tree_set(source_names, new_name, v);
  else
    return source_names;
}

/*========================================================================*/
/*                            compiling linklets                          */
/*========================================================================*/

static Scheme_Linklet *compile_and_or_optimize_linklet(Scheme_Object *form, Scheme_Linklet *linklet,
                                                       Scheme_Object *name,
                                                       Scheme_Object **_import_keys, Scheme_Object *get_import,
                                                       int unsafe_mode, int static_mode)
{
  Scheme_Config *config;
  int enforce_const, set_undef, can_inline;
  Scheme_Performance_State perf_state;

  scheme_performance_record_start(&perf_state);

  config = scheme_current_config();
  enforce_const = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_COMPILE_MODULE_CONSTS));
  set_undef = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_ALLOW_SET_UNDEFINED));
  can_inline = SCHEME_FALSEP(scheme_get_param(config, MZCONFIG_DISALLOW_INLINE));

  if (_import_keys && !*_import_keys)
    _import_keys = NULL;

  if (!linklet) {
    linklet = scheme_compile_linklet(form, set_undef, (_import_keys ? *_import_keys : NULL));
    linklet = scheme_letrec_check_linklet(linklet);
  } else {
    linklet = scheme_unresolve_linklet(linklet, (set_undef ? COMP_ALLOW_SET_UNDEFINED : 0));
  }
  linklet->name = name;
  linklet = scheme_optimize_linklet(linklet, enforce_const, can_inline, unsafe_mode,
                                    _import_keys, get_import);

  linklet = scheme_resolve_linklet(linklet, enforce_const, static_mode);
  linklet = scheme_sfs_linklet(linklet);
  
  if (recompile_every_compile) {
    int i;
    for (i = recompile_every_compile; i--; ) {
      linklet = scheme_unresolve_linklet(linklet, (set_undef ? COMP_ALLOW_SET_UNDEFINED : 0));
      linklet = scheme_optimize_linklet(linklet, enforce_const, can_inline, unsafe_mode,
                                        _import_keys, get_import);
      linklet = scheme_resolve_linklet(linklet, enforce_const, static_mode);
      linklet = scheme_sfs_linklet(linklet);
    }
  }

  if (validate_compile_result)
    scheme_validate_linklet(NULL, linklet);

  scheme_performance_record_end("compile", &perf_state);

  return linklet;
}

Scheme_Linklet *scheme_compile_and_optimize_linklet(Scheme_Object *form, Scheme_Object *name)
{
  return compile_and_or_optimize_linklet(form, NULL, name, NULL, NULL, 0, 1);
}

/*========================================================================*/
/*                          instantiating linklets                        */
/*========================================================================*/

static Scheme_Object *body_one_expr(void *prefix_plus_expr, int argc, Scheme_Object **argv)
{
  Scheme_Object *v;

  resume_prefix(SCHEME_CAR((Scheme_Object *)prefix_plus_expr));
  v = _scheme_eval_linked_expr_multi(SCHEME_CDR((Scheme_Object *)prefix_plus_expr));
  (void)suspend_prefix();

  return v;
}

static int needs_prompt(Scheme_Object *e)
{
  Scheme_Type t;
  
  while (1) {
    t = SCHEME_TYPE(e);
    if (t > _scheme_values_types_)
      return 0;
  
    switch (t) {
    case scheme_lambda_type:
    case scheme_toplevel_type:
    case scheme_local_type:
    case scheme_local_unbox_type:
      return 0;
    case scheme_case_lambda_sequence_type:
      return 0;
    case scheme_define_values_type:
      e = SCHEME_VEC_ELS(e)[0];
      break;
    case scheme_inline_variant_type:
      e = SCHEME_VEC_ELS(e)[0];
      break;
    default:
      return 1;
    }
  }
}

Scheme_Object *scheme_linklet_run_finish(Scheme_Linklet* linklet, Scheme_Instance *instance, int use_prompt)
{
  Scheme_Thread *p;
  Scheme_Object *body, *save_prefix, *v = scheme_void;
  int i, cnt;
  mz_jmp_buf newbuf, * volatile savebuf;

  p = scheme_current_thread;
  savebuf = p->error_buf;
  p->error_buf = &newbuf;

  if (scheme_setjmp(newbuf)) {
    Scheme_Thread *p2;
    p2 = scheme_current_thread;
    p2->error_buf = savebuf;
    scheme_longjmp(*savebuf, 1);
  } else {
    cnt = SCHEME_VEC_SIZE(linklet->bodies);
    for (i = 0; i < cnt; i++) {
      body = SCHEME_VEC_ELS(linklet->bodies)[i];
      if (use_prompt && needs_prompt(body)) {
        /* We need to push the prefix after the prompt is set, so
           restore the runstack and then add the prefix back. */
        save_prefix = suspend_prefix();
        v = _scheme_call_with_prompt_multi(body_one_expr, 
                                           scheme_make_raw_pair(save_prefix, body));
        resume_prefix(save_prefix);

        /* Double-check that the definition-installing part of the
           continuation was not skipped. Otherwise, the compiler would
           not be able to assume that a variable reference that is
           lexically later (incuding a reference to an imported
           variable) always references a defined variable. Putting the
           prompt around a definition's RHS might be a better
           approach, but that would change the language (so mabe next
           time). */
        if (SAME_TYPE(SCHEME_TYPE(body), scheme_define_values_type)) {
          int vcnt, j;
          
          vcnt = SCHEME_VEC_SIZE(body) - 1;
          for (j = 0; j < vcnt; j++) {
            Scheme_Object *var;
            Scheme_Prefix *toplevels;
            Scheme_Bucket *b;
            
            var = SCHEME_VEC_ELS(body)[j+1];
            toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(var)];
            b = (Scheme_Bucket *)toplevels->a[SCHEME_TOPLEVEL_POS(var)];
            
            if (!b->val) {
              scheme_raise_exn(MZEXN_FAIL_CONTRACT_VARIABLE, 
                               b->key,
                               "define-values: skipped variable definition;\n"
                               " cannot continue without defining variable\n"
                               "  variable: %S\n"
                               "  in module: %D",
                               (Scheme_Object *)b->key,
                               instance->name);
            }
          }
        }
      } else
        v = _scheme_eval_linked_expr_multi(body);

      if (i < (cnt - 1))
        scheme_ignore_result(v);
    }

    p = scheme_current_thread;
    p->error_buf = savebuf;
  }

  return v;
}

static Scheme_Object *eval_linklet_body(Scheme_Linklet *linklet, Scheme_Instance *instance, int use_prompt)
{
#ifdef MZ_USE_JIT
  if (use_prompt)
    return scheme_linklet_run_start(linklet, instance, scheme_make_pair(instance->name, scheme_true));
#endif
  
  return scheme_linklet_run_finish(linklet, instance, use_prompt);
}

static void *instantiate_linklet_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Linklet *linklet = (Scheme_Linklet *)p->ku.k.p1;
  Scheme_Instance *instance = (Scheme_Instance *)p->ku.k.p2;
  Scheme_Instance **instances = (Scheme_Instance **)p->ku.k.p3;
  int multi = p->ku.k.i1;
  int num_instances = p->ku.k.i2;
  int use_prompt = p->ku.k.i3;
  int depth;
  Scheme_Object *b, *v;
  Scheme_Hash_Tree *source_names;
  Scheme_Performance_State perf_state;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  depth = linklet->max_let_depth;  
  if (!scheme_check_runstack(depth)) {
    p->ku.k.p1 = linklet;
    p->ku.k.p2 = instance;
    p->ku.k.p3 = instances;
    p->ku.k.i1 = multi;
    p->ku.k.i2 = num_instances;
    p->ku.k.i3 = use_prompt;
    return (Scheme_Object *)scheme_enlarge_runstack(depth, instantiate_linklet_k);
  }

  scheme_performance_record_start(&perf_state);

  if (!linklet->jit_ready) {
    b = scheme_get_param(scheme_current_config(), MZCONFIG_USE_JIT);
    if (SCHEME_TRUEP(b))
      linklet = scheme_jit_linklet(linklet, 2);
  } else {
    linklet = scheme_jit_linklet(linklet, 2);
  }

  /* Pushng the prefix looks up imported variables */
  source_names = push_prefix(linklet, instance, num_instances, instances, linklet->source_names);

  /* For variables in this instances, merge source-name info from the
     linklet to the instance */
  if (source_names->count) {
    if (instance->source_names->count) {
      mzlonglong pos;
      Scheme_Hash_Tree *ht = instance->source_names;
      Scheme_Object *k, *v;
      pos = scheme_hash_tree_next(source_names, -1);
      while (pos != -1) {
        scheme_hash_tree_index(source_names, pos, &k, &v);
        ht = scheme_hash_tree_set(ht, k, v);
        pos = scheme_hash_tree_next(source_names, pos);
      }
      instance->source_names = ht;
    } else
      instance->source_names = source_names;
  }

  v = eval_linklet_body(linklet, instance, use_prompt);

  pop_prefix();

  if (!multi)
    v = scheme_check_one_value(v);

#ifdef MZ_USE_JIT
  if (linklet->native_lambdas) {
    int mc;
    Scheme_Object **mv, *l;

    if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
      p = scheme_current_thread;
      mv = p->ku.multiple.array;
      mc = p->ku.multiple.count;
      if (SAME_OBJ(mv, p->values_buffer))
        p->values_buffer = NULL;
    } else {
      mv = NULL;
      mc = 0;
    }

    l = linklet->native_lambdas;
    linklet->native_lambdas = NULL;

    while (SCHEME_PAIRP(l)) {
      scheme_force_jit_generate((Scheme_Native_Lambda *)SCHEME_CAR(l));
      l = SCHEME_CDR(l);
    }

    if (mv) {
      p = scheme_current_thread;
      p->ku.multiple.array = mv;
      p->ku.multiple.count = mc;
    }
  }
#endif

  scheme_performance_record_end("instantiate", &perf_state);

  return (void *)v;
}

static Scheme_Object *do_instantiate_linklet(Scheme_Linklet *linklet, Scheme_Instance *instance,
                                             int num_instances, Scheme_Instance **instances,
                                             int use_prompt, int multi, int top)
{
  Scheme_Thread *p = scheme_current_thread;
  
  p->ku.k.p1 = linklet;
  p->ku.k.p2 = instance;
  p->ku.k.p3 = instances;
  
  p->ku.k.i1 = multi;
  p->ku.k.i2 = num_instances;
  p->ku.k.i3 = use_prompt;

  if (top)
    return (Scheme_Object *)scheme_top_level_do(instantiate_linklet_k, 1);
  else
    return (Scheme_Object *)instantiate_linklet_k();
}

static Scheme_Object *_instantiate_linklet_multi(Scheme_Linklet *linklet, Scheme_Instance *instance,
                                                 int num_instances, Scheme_Instance **instances,
                                                 int use_prompt)
{
  return do_instantiate_linklet(linklet, instance, num_instances, instances, use_prompt, 1, 0);
}

Scheme_Object *scheme_instantiate_linklet_multi(Scheme_Linklet *linklet, Scheme_Instance *instance,
                                                int num_instances, Scheme_Instance **instances,
                                                int use_prompt)
{
  return do_instantiate_linklet(linklet, instance, num_instances, instances, use_prompt, 1, 1);
}

/*========================================================================*/
/*        creating/pushing prefix for top-levels and syntax objects       */
/*========================================================================*/

Scheme_Prefix *scheme_allocate_linklet_prefix(Scheme_Linklet *linklet, int extra)
{
  int num_defns, n;
  
  num_defns = SCHEME_VEC_SIZE(linklet->defns);

  n = 1 + linklet->num_total_imports + num_defns + extra;

  return scheme_allocate_prefix(n);
}

Scheme_Prefix *scheme_allocate_prefix(intptr_t n)
{
  Scheme_Prefix *pf;
  int tl_map_len;

  tl_map_len = (n + 31) / 32;

  pf = scheme_malloc_tagged(sizeof(Scheme_Prefix) 
                            + ((n-mzFLEX_DELTA) * sizeof(Scheme_Object *))
                            + (tl_map_len * sizeof(int)));
  pf->iso.so.type = scheme_prefix_type;
  pf->num_slots = n;

  return pf;
}

static Scheme_Hash_Tree *push_prefix(Scheme_Linklet *linklet, Scheme_Instance *instance,
                                     int num_instances, Scheme_Instance **instances,
                                     Scheme_Hash_Tree *source_names)
{
  Scheme_Object **rs, *v;
  Scheme_Prefix *pf;
  int i, j, pos, num_importss, num_defns, starts_empty;
  GC_CAN_IGNORE const char *bad_reason = NULL;

  rs = MZ_RUNSTACK;

  num_importss = SCHEME_VEC_SIZE(linklet->importss);
  num_defns = SCHEME_VEC_SIZE(linklet->defns);

  pf = linklet->static_prefix;
  if (!pf)
    pf = scheme_allocate_linklet_prefix(linklet, 0);

  --rs;
  MZ_RUNSTACK = rs;
  rs[0] = (Scheme_Object *)pf;

  pos = 0;

  /* Initial bucket, key by #f, provides access to the instance */
  if (linklet->need_instance_access)
    v = (Scheme_Object *)scheme_instance_variable_bucket(scheme_false, instance);
  else
    v = NULL;
  pf->a[pos++] = v;
  
  for (j = 0; j < num_importss; j++) {
    int num_imports = SCHEME_VEC_SIZE(SCHEME_VEC_ELS(linklet->importss)[j]);
    for (i = 0; i < num_imports; i++) {
      v = SCHEME_VEC_ELS(SCHEME_VEC_ELS(linklet->importss)[j])[i];
      v = (Scheme_Object *)scheme_instance_variable_bucket(v, (Scheme_Instance *)instances[j]);

      if (v) {
        if (!((Scheme_Bucket *)v)->val) {
          bad_reason = "is uninitialized";
          v = NULL;
        } else if (linklet->import_shapes) {
          Scheme_Object *shape = SCHEME_VEC_ELS(linklet->import_shapes)[pos-1];
          if (SAME_OBJ(shape, scheme_void)) {
            /* Optimizer assumed constant; if it isn't, too bad */
            bad_reason = NULL;
          } else if (SAME_OBJ(shape, scheme_true)) {
            if (!(((Scheme_Bucket_With_Flags *)v)->flags & GLOB_IS_CONSISTENT)) {
              bad_reason = "is not a procedure or structure-type constant across all instantiations";
              v = NULL;
            }
          } else if (SCHEME_TRUEP(shape)) {
            if (!scheme_get_or_check_procedure_shape(((Scheme_Bucket *)v)->val, shape, 0)) {
              bad_reason = "has the wrong procedure or structure-type shape";
              v = NULL;
            }
          }
        }
      } else
        bad_reason = "is not exported";
      
      if (!v) {
        scheme_signal_error("instantiate-linklet: mismatch;\n"
                            " reference to a variable that %s;\n"
                            " possibly, bytecode file needs re-compile because dependencies changed\n"
                            "  name: %D\n"
                            "  exporting instance: %D\n"
                            "  importing instance: %D",
                            bad_reason,
                            SCHEME_VEC_ELS(SCHEME_VEC_ELS(linklet->importss)[j])[i],
                            instances[j]->name,
                            instance->name);
      }
      pf->a[pos++] = v;
    }
  }

  starts_empty = (!instance->array_size && !instance->variables.bt);

  if (!num_defns) {
    /* don't allocate empty array, etc. */
  } else if (starts_empty && (num_defns < 10)) {
    /* Faster to build an array-shaped instance (which will be
       converted to a bucket table on demand, if necessary) */
    Scheme_Bucket **a, *b;

    a = MALLOC_N(Scheme_Bucket *, num_defns);
    for (i = 0; i < num_defns; i++) {
      v = SCHEME_VEC_ELS(linklet->defns)[i];
      if (SCHEME_FALSEP(v)) {
        pf->a[pos++] = NULL;
      } else {
        b = make_bucket(v, NULL, instance);
        a[i] = b;
        pf->a[pos++] = (Scheme_Object *)b;
      }
    }

    instance->array_size = num_defns;
    instance->variables.a = a;
  } else {
    /* General case: bucket-table instance: */
    for (i = 0; i < num_defns; i++) {
      v = SCHEME_VEC_ELS(linklet->defns)[i];
      if (SCHEME_FALSEP(v)) {
        v = NULL;
      } else {
        if ((i >= linklet->num_exports) && !starts_empty) {
          /* avoid conflict with any existing bucket */
          if (scheme_instance_variable_bucket_or_null(v, instance)) {
            v = generate_bucket_name(v, instance);
            source_names = update_source_names(source_names, SCHEME_VEC_ELS(linklet->defns)[i], v);
          }
        }
        v = (Scheme_Object *)scheme_instance_variable_bucket(v, instance);
      }
      pf->a[pos++] = v;
    }
  }

  return source_names;
}

static void pop_prefix()
{
  /* This function must not allocate, since a relevant multiple-values
     result may be in the thread record (and we don't want it zerod) */
  MZ_RUNSTACK++;
}

static Scheme_Object *suspend_prefix()
{
  Scheme_Object *v;
  v = MZ_RUNSTACK[0];
  MZ_RUNSTACK++;
  return v;
}

static void resume_prefix(Scheme_Object *v)
{
  --MZ_RUNSTACK;
  MZ_RUNSTACK[0] = v;
}

#ifdef MZ_PRECISE_GC
static void mark_pruned_prefixes(struct NewGC *gc) XFORM_SKIP_PROC
{
  if (!GC_is_partial(gc)) {
    if (scheme_inc_prefix_finalize != (Scheme_Prefix *)0x1) {
      Scheme_Prefix *pf = scheme_inc_prefix_finalize;
      while (pf->next_final != (Scheme_Prefix *)0x1) {
        pf = pf->next_final;
      }
      pf->next_final = scheme_prefix_finalize;
      scheme_prefix_finalize = scheme_inc_prefix_finalize;
      scheme_inc_prefix_finalize = (Scheme_Prefix *)0x1;
    }
  }
  
  if (scheme_prefix_finalize != (Scheme_Prefix *)0x1) {
    Scheme_Prefix *pf = scheme_prefix_finalize, *next;
    Scheme_Object *clo;
    int i, *use_bits, maxpos;
    
    scheme_prefix_finalize = (Scheme_Prefix *)0x1;
    while (pf != (Scheme_Prefix *)0x1) {
      /* If not marked, only references are through closures: */
      if (!GC_is_marked2(pf, gc)) {
        /* Clear slots that are not use in map */
        maxpos = pf->num_slots;
        use_bits = PREFIX_TO_USE_BITS(pf);
        for (i = (maxpos + 31) / 32; i--; ) {
          int j;
          for (j = 0; j < 32; j++) {
            if (!(use_bits[i] & ((unsigned)1 << j))) {
              int pos;
              pos = (i * 32) + j;
              if (pos < maxpos)
                pf->a[pos] = NULL;
            }
          }
          use_bits[i] = 0;
        }
        /* Should mark/copy pf, but not trigger or require mark propagation: */
#ifdef MZ_GC_BACKTRACE
        GC_set_backpointer_object(pf->backpointer);
#endif
        GC_mark_no_recur(gc, 1);
        gcMARK2(pf, gc);
        pf = (Scheme_Prefix *)GC_resolve2(pf, gc);
        GC_retract_only_mark_stack_entry(pf, gc);
        GC_mark_no_recur(gc, 0);
        pf->saw_num_slots = -1;
      } else
        pf = (Scheme_Prefix *)GC_resolve2(pf, gc);

      /* Clear use map */
      use_bits = PREFIX_TO_USE_BITS(pf);
      maxpos = pf->num_slots;
      for (i = (maxpos + 31) / 32; i--; )
        use_bits[i] = 0;

      /* Fix up closures that reference this prefix: */
      clo = (Scheme_Object *)GC_resolve2(pf->fixup_chain, gc);
      pf->fixup_chain = NULL;
      while (clo) {
        Scheme_Object *next;
        if (SCHEME_TYPE(clo) == scheme_closure_type) {
          Scheme_Closure *cl = (Scheme_Closure *)clo;
          int closure_size = ((Scheme_Lambda *)GC_resolve2(cl->code, gc))->closure_size;
          next = cl->vals[closure_size - 1];
          cl->vals[closure_size-1] = (Scheme_Object *)pf;
        } else if (SCHEME_TYPE(clo) == scheme_native_closure_type) {
          Scheme_Native_Closure *cl = (Scheme_Native_Closure *)clo;
          int closure_size = ((Scheme_Native_Lambda *)GC_resolve2(cl->code, gc))->closure_size;
          next = cl->vals[closure_size - 1];
          cl->vals[closure_size-1] = (Scheme_Object *)pf;
        } else {
          MZ_ASSERT(0);
          next = NULL;
        }
        clo = (Scheme_Object *)GC_resolve2(next, gc);
      }
      if (SCHEME_PREFIX_FLAGS(pf) & 0x1)
        SCHEME_PREFIX_FLAGS(pf) -= 0x1;

      /* Next */
      next = pf->next_final;
      pf->next_final = NULL;

      pf = next;
    }
  }
}

int check_pruned_prefix(void *p) XFORM_SKIP_PROC
{
  Scheme_Prefix *pf = (Scheme_Prefix *)p;
  return SCHEME_PREFIX_FLAGS(pf) & 0x1;
}
#endif

/*========================================================================*/
/*  Recorindg performance times                                           */
/*========================================================================*/

static intptr_t nested_delta, nested_gc_delta;
static int perf_reg, perf_count;

typedef struct {
  const char *name;
  intptr_t accum;
  intptr_t gc_accum;
  intptr_t count;
} Performance_Entry;

#define MAX_PERF_ENTRIES 16

static Performance_Entry perf_entries[MAX_PERF_ENTRIES];

#define MAX_PERF_CATS    3
#define MAX_PERF_SUBS    3

typedef struct {
  const char *name;
  Performance_Entry perf_entries[MAX_PERF_SUBS];
  int perf_count;
} Performance_Cat;

typedef struct {
  const char *entry;
  const char *cat;
} Performace_Recat;

static Performace_Recat recats[] = { { "instantiate", "run" },
                                     { "jit", "run" },
                                     { "comp-ffi-call", "comp-ffi" },
                                     { "comp-ffi-back", "comp-ffi" },
                                     { NULL, NULL} };

static char *do_tab(int len, char *tab, int max_len)
{
  int i;

  len = max_len - len;
  if (len < 0)
    len = 0;
  for (i = 0; i < len; i++) {
    tab[i] = ' ';
  }
  tab[i] = 0;

  return tab;
}

static int numlen(intptr_t n)
{
  int len = 1;

  while (n >= 10) {
    n = n / 10;
    len++;
  }

  return len;
}

static char *tab_number(intptr_t n, char *tab, int max_len)
{
  return do_tab(numlen(n), tab, max_len);
}

static char *tab_string(const char *s, char *tab, int max_len)
{
  return do_tab(strlen(s), tab, max_len);
}

static void sort_perf(Performance_Entry *pref_entries, int lo, int hi)
{
  int i, pivot;
  
  if (lo >= hi)
    return;

  pivot = lo;
  for (i = lo + 1; i < hi; i++) {
    if (perf_entries[i].accum < perf_entries[pivot].accum) {
      Performance_Entry tmp = perf_entries[pivot];
      perf_entries[pivot] = perf_entries[i];
      perf_entries[i] = perf_entries[pivot+1];
      perf_entries[pivot+1] = tmp;
      pivot++;
    }
  }

  sort_perf(perf_entries, lo, pivot);
  sort_perf(perf_entries, pivot+1, hi);
}

static void show_perf(Performance_Entry *perf_entries, int perf_count,
                      int len, int name_len, int gc_len,
                      int depth)
{
  intptr_t total = 0, gc_total = 0;
  int i, j, k, m, n;
  char name_tab[16], tab[10], gc_tab[10], pre_indent[8], post_indent[8];
  Performance_Cat cats[MAX_PERF_CATS];
  int num_cats = 0;

  memset(cats, 0, sizeof(cats));

  if (!depth) {
    for (i = 0; i < perf_count; i++) {
      for (j = 0; recats[j].entry; j++) {
        if (!strcmp(recats[j].entry, perf_entries[i].name)) {
          for (m = 0; m < num_cats; m++) {
            if (!strcmp(recats[j].cat, cats[m].name))
              break;
          }
          if (num_cats <= m) num_cats = m+1;
          cats[m].name = recats[j].cat;
          for (k = 0; k < perf_count; k++) {
            if (perf_entries[k].name) {
              if (!strcmp(perf_entries[k].name, recats[j].cat))
                break;
            } else
              break;
          }
          perf_entries[k].name = recats[j].cat;
          if (perf_count <= k) perf_count = k+1;
          perf_entries[k].accum += perf_entries[i].accum;
          perf_entries[k].gc_accum += perf_entries[i].gc_accum;
          perf_entries[k].count = -1;

          n = cats[m].perf_count++;
          cats[m].perf_entries[n] = perf_entries[i];
          perf_entries[i].accum = 0;
          perf_entries[i].gc_accum = 0;
          perf_entries[i].count = 0;
        }
      }
    }
  }

  sort_perf(perf_entries, 0, perf_count);

  for (i = 0; i < perf_count; i++) {
    n = strlen(perf_entries[i].name);
    if (n > name_len) name_len = n;
    total += perf_entries[i].accum;
    gc_total += perf_entries[i].gc_accum;
  }

  n = numlen(total);
  if (n > len) len = n;
  n = numlen(gc_total);
  if (n > gc_len) gc_len = n;

  if (name_len >= sizeof(name_tab))
    name_len = sizeof(name_tab) - 1;
  if (len >= sizeof(tab))
    len = sizeof(tab) - 1;
  if (gc_len >= sizeof(gc_tab))
    gc_len = sizeof(gc_tab) -1;

  for (i = 0; i < depth * 2; i++) {
    pre_indent[i] = ' ';
  }
  pre_indent[i] = 0;
  for (i = 0; i < (3 - depth) * 2; i++) {
    post_indent[i] = ' ';
  }
  post_indent[i] = 0;

  if (!depth)
    scheme_log(NULL, SCHEME_LOG_ERROR, 0, ";;");

#define BASE_TIMES_TEMPLATE ";; %s%s%s%s  %s%ld [%s%ld] ms"
#define FULL_TIMES_TEMPLATE BASE_TIMES_TEMPLATE " ; %ld times"

  for (i = 0; i < perf_count; i++) {
    if (perf_entries[i].count)
      scheme_log(NULL, SCHEME_LOG_ERROR, 0,
                 ((perf_entries[i].count < 0)
                  ? BASE_TIMES_TEMPLATE
                  : FULL_TIMES_TEMPLATE),
                 pre_indent,
                 perf_entries[i].name,
                 tab_string(perf_entries[i].name, name_tab, name_len),
                 post_indent,
                 tab_number(perf_entries[i].accum, tab, len),
                 perf_entries[i].accum,
                 tab_number(perf_entries[i].gc_accum, gc_tab, gc_len),
                 perf_entries[i].gc_accum,
                 perf_entries[i].count);
    for (m = 0; m < num_cats; m++) {
      if (!strcmp(perf_entries[i].name, cats[m].name))
        show_perf(cats[m].perf_entries, cats[m].perf_count, len, name_len, gc_len, depth+1);
    }
  }

  if (!depth) {
    scheme_log(NULL, SCHEME_LOG_ERROR, 0,
               ";; %stotal%s  %s%ld [%s%ld] ms",
               tab_number(total, tab, len),
               tab_string("total", name_tab, name_len),
               post_indent,
               total,
               tab_number(gc_total, gc_tab, gc_len),
               gc_total);
#ifdef MZ_PRECISE_GC
    scheme_log(NULL, SCHEME_LOG_ERROR, 0, ";;");
    scheme_log(NULL, SCHEME_LOG_ERROR, 0,
               ";; [JIT code: %d procs  %d bytes  code+admin: %d bytes]",
               scheme_code_count,
               scheme_code_total,
               scheme_code_page_total);
#endif
  }
}

static void show_all_perf()
{
  return show_perf(perf_entries, perf_count, 0, 0, 0, 0);
}

void scheme_performance_record_start(GC_CAN_IGNORE Scheme_Performance_State *perf_state)
{
#if defined(MZ_USE_PLACES)
  if (scheme_current_place_id != 0)
    return;
#endif
  
  if (!perf_reg) {
    if (scheme_getenv("PLT_LINKLET_TIMES")) {
      perf_reg = 1;
      scheme_atexit(show_all_perf);
    } else {
      perf_reg = -1;
    }
  }

  if (perf_reg < 0)
    return;

  perf_state->gc_start = scheme_total_gc_time;
  perf_state->start = scheme_get_process_milliseconds();
  perf_state->old_nested_delta = nested_delta;
  perf_state->old_nested_gc_delta = nested_gc_delta;

  nested_delta = 0;
  nested_gc_delta = 0;
}

void scheme_performance_record_end(const char *who, GC_CAN_IGNORE Scheme_Performance_State *perf_state)
{
  int i;
  intptr_t d, gc_d;
  Scheme_Performance_State zero_perf_state;

#if defined(MZ_USE_PLACES)
  if (scheme_current_place_id != 0)
    return;
#endif

  if (perf_reg < 0)
    return;

  for (i = 0; i < MAX_PERF_ENTRIES; i++) {
    if (perf_entries[i].name) {
      if (!strcmp(perf_entries[i].name, who))
        break;
    } else
      break;
  }

  if (i >= MAX_PERF_ENTRIES)
    return;

  if (!perf_state) {
    memset(&zero_perf_state, 0, sizeof(zero_perf_state));
    perf_state = &zero_perf_state;
  }

  d = (scheme_get_process_milliseconds() - perf_state->start);
  gc_d = (scheme_total_gc_time - perf_state->gc_start);

  perf_state->old_nested_delta += d;
  perf_state->old_nested_gc_delta += gc_d;

  d -= nested_delta;
  gc_d -= nested_gc_delta;

  nested_delta = perf_state->old_nested_delta;
  nested_gc_delta = perf_state->old_nested_gc_delta;

  if (!perf_entries[i].name) {
    perf_entries[i].name = who;
    perf_count++;
  }
  perf_entries[i].accum += d;
  perf_entries[i].gc_accum += gc_d;
  perf_entries[i].count++;
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_linklet.inc"

static void register_traversers(void)
{
}

END_XFORM_SKIP;

#endif
