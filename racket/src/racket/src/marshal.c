#include "schpriv.h"

#define CONS(a,b) scheme_make_pair(a,b)

void scheme_init_marshal(Scheme_Startup_Env *env) 
{
  /* nothing */
}

static int not_relative_path(Scheme_Object *p, Scheme_Hash_Table *cache)
{
  Scheme_Object *dir, *rel_p;

  dir = scheme_get_param(scheme_current_config(),
                         MZCONFIG_WRITE_DIRECTORY);
  if (SCHEME_TRUEP(dir)) {
    rel_p = scheme_extract_relative_to(p, dir, cache);
    if (SCHEME_PATHP(rel_p))
      return 1;
  }
  
  return 0;
}

Scheme_Object *scheme_closure_marshal_name(Scheme_Object *name)
{
  if (name) {
    if (SCHEME_VECTORP(name)) {
      /* We can only save marshalable src names, which includes
	 paths, symbols, and strings: */
      Scheme_Object *src;
      src = SCHEME_VEC_ELS(name)[1];
      if ((!SCHEME_PATHP(src)
           /* If MZCONFIG_WRITE_DIRECTORY, drop any non-relative path
              (which might happen due to function inlining, for example)
              to avoid embedding absolute paths in bytecode files: */
           || not_relative_path(src, scheme_current_thread->current_mt->path_cache))
	  && !SCHEME_CHAR_STRINGP(src)
	  && !SCHEME_SYMBOLP(src)) {
	/* Just keep the name */
	name = SCHEME_VEC_ELS(name)[0];
      }
    }
  } else
    name = scheme_null;

  return name;
}

void scheme_write_lambda(Scheme_Object *obj,
                         Scheme_Object **_name,
                         Scheme_Object **_ds,
                         Scheme_Object **_closure_map,
                         Scheme_Object **_tl_map)
{
  Scheme_Lambda *data;
  Scheme_Object *name, *code, *ds, *tl_map, *closure_map;
  int svec_size, pos;
  Scheme_Marshal_Tables *mt;

  data = (Scheme_Lambda *)obj;

  name = scheme_closure_marshal_name(data->name);

  svec_size = data->closure_size;
  if (SCHEME_LAMBDA_FLAGS(data) & LAMBDA_HAS_TYPED_ARGS) {
    svec_size += scheme_boxmap_size(data->num_params + data->closure_size);
    {
      int k, mv;
      for (k = data->num_params + data->closure_size; --k; ) {
        mv = scheme_boxmap_get(data->closure_map, k, data->closure_size);
        if (mv > (LAMBDA_TYPE_TYPE_OFFSET + SCHEME_MAX_LOCAL_TYPE))
          scheme_signal_error("internal error: inconsistent closure/argument type");
      }
    }
  }

  if (SCHEME_RPAIRP(data->body)) {
    /* This can happen if loaded bytecode is printed out and the procedure
       body has never been needed before.
       It's also possible in non-JIT mode if an empty closure is embedded 
       as a 3-D value in compiled code. */
    scheme_delay_load_closure(data);
  }

  /* If the body is simple enough, write it directly.
     Otherwise, create a delay indirection so that the body
     is loaded on demand. */
  code = data->body;
  switch (SCHEME_TYPE(code)) {
  case scheme_toplevel_type:
  case scheme_local_type:
  case scheme_local_unbox_type:
  case scheme_true_type:
  case scheme_false_type:
  case scheme_void_type:
    ds = code;
    break;
  default:
    if (SCHEME_NUMBERP(code))
      ds = code;
    else
      ds = NULL;
    break;
  }
  
  if (!ds) {
    mt = scheme_current_thread->current_mt;
    if (mt->pass < 0) {
      /* nothing to do, yet */
      ds = scheme_false;
    } else {
      if (!mt->pass) {
        int key;

        pos = mt->cdata_counter;
        if ((!mt->cdata_map || (pos >= 32))
            && !(pos & (pos - 1))) {
          /* Need to grow the array */
          Scheme_Object **a;
          a = MALLOC_N(Scheme_Object *, (pos ? 2 * pos : 32));
          if (pos)
            memcpy(a, mt->cdata_map, pos * sizeof(Scheme_Object *));
          mt->cdata_map = a;
        }
        mt->cdata_counter++;

        key = pos & 255;
        MZ_OPT_HASH_KEY(&data->iso) = ((int)MZ_OPT_HASH_KEY(&data->iso) & 0x00FF) | (key << 8);
      } else {
        pos = ((int)MZ_OPT_HASH_KEY(&data->iso) & 0xFF00) >> 8;

        while (pos < mt->cdata_counter) {
          ds = mt->cdata_map[pos];
          if (ds) {
            ds = SCHEME_PTR_VAL(ds);
            if (SAME_OBJ(data->body, ds))
              break;
            if (SAME_TYPE(scheme_quote_compilation_type, SCHEME_TYPE(ds)))
              if (SAME_OBJ(data->body, SCHEME_PTR_VAL(ds)))
                break;
          }
          pos += 256;
        }
        if (pos >= mt->cdata_counter) {
          scheme_signal_error("didn't find delay record");
        }
      }

      ds = mt->cdata_map[pos];
      if (!ds) {
        if (mt->pass)
          scheme_signal_error("broken closure-data table\n");

        code = scheme_protect_quote(data->body);

        ds = scheme_alloc_small_object();
        ds->type = scheme_delay_syntax_type;
        SCHEME_PTR_VAL(ds) = code;

        MZ_OPT_HASH_KEY(&((Scheme_Small_Object *)ds)->iso) |= 1; /* => hash on ds, not contained data */

        mt->cdata_map[pos] = ds;
      }
    }
  }

  /* Encode data->tl_map as either a fixnum or a vector of 16-bit values */
  if (!data->tl_map)
    tl_map = scheme_false;
  else if ((uintptr_t)data->tl_map & 0x1) {
    if (((uintptr_t)data->tl_map & 0xFFFFFFF) == (uintptr_t)data->tl_map) {
      /* comfortably a fixnum */
      tl_map = (Scheme_Object *)data->tl_map;
    } else {
      uintptr_t v;
      tl_map = scheme_make_vector(2, NULL);
      v = ((uintptr_t)data->tl_map >> 1) & 0x7FFFFFFF;
      SCHEME_VEC_ELS(tl_map)[0] = scheme_make_integer(v & 0xFFFF);
      SCHEME_VEC_ELS(tl_map)[1] = scheme_make_integer((v >> 16) & 0xFFFF);
    }
  } else {
    int len = ((int *)data->tl_map)[0], i, v;
    tl_map = scheme_make_vector(2 * len, NULL);
    for (i = 0; i < len; i++) {
      v = ((int *)data->tl_map)[i+1];
      SCHEME_VEC_ELS(tl_map)[2*i] = scheme_make_integer(v & 0xFFFF);
      SCHEME_VEC_ELS(tl_map)[(2*i)+1] = scheme_make_integer((v >> 16) & 0xFFFF);
    }
  }

  *_name = name;
  *_ds = ds;
  closure_map = scheme_make_svector(svec_size, data->closure_map);
  *_closure_map = closure_map;
  *_tl_map = tl_map;
}

Scheme_Object *scheme_read_lambda(int flags, int closure_size, int num_params, int max_let_depth,
                                  Scheme_Object *name,
                                  Scheme_Object *ds,
                                  Scheme_Object *closure_map,
                                  Scheme_Object *tl_map)
{
  Scheme_Lambda *data;

#define BAD_CC "bad compiled closure"
#define X_SCHEME_ASSERT(x, y)

  data  = (Scheme_Lambda *)scheme_malloc_tagged(sizeof(Scheme_Lambda));
  data->iso.so.type = scheme_lambda_type;

  SCHEME_LAMBDA_FLAGS(data) = (short)flags;

  data->num_params = num_params;
  if (data->num_params < 0) return NULL;

  data->max_let_depth = max_let_depth;
  if (data->max_let_depth < 0) return NULL;

  if (!SCHEME_FALSEP(tl_map)) {
    if (SCHEME_INTP(tl_map))
      data->tl_map = (void *)tl_map;
    else if (SCHEME_VECTORP(tl_map)) {
      int *n, i, len, v1, v2;
      len = SCHEME_VEC_SIZE(tl_map);
      if (len & 0x1)
        return NULL;
      n = (int *)scheme_malloc_atomic(((len/2) + 1) * sizeof(int));
      n[0] = len/2;
      for (i = 0; i < len/2; i++) {
        v1 = SCHEME_INT_VAL(SCHEME_VEC_ELS(tl_map)[2*i]);
        v2 = SCHEME_INT_VAL(SCHEME_VEC_ELS(tl_map)[(2*i) + 1]);
        v2 = ((unsigned int)v2 << 16) | v1;
        n[i+1] = v2;
      }
      if ((len == 2) && (!(n[1] & 0x80000000)))
        data->tl_map = (void *)(intptr_t)(((uintptr_t)n[1] << 1) | 0x1);
      else
        data->tl_map = n;
    } else
      return NULL;
  }

  data->name = name;
  if (SCHEME_NULLP(data->name))
    data->name = NULL;

  data->body = ds;

  if (!SAME_TYPE(scheme_svector_type, SCHEME_TYPE(closure_map))) return NULL;
  data->closure_map = SCHEME_SVEC_VEC(closure_map);

  if (SCHEME_LAMBDA_FLAGS(data) & LAMBDA_HAS_TYPED_ARGS) {
    data->closure_size = closure_size;
    if (data->closure_size + scheme_boxmap_size(data->closure_size + data->num_params) != SCHEME_SVEC_LEN(closure_map))
      return NULL;
  } else
    data->closure_size = SCHEME_SVEC_LEN(closure_map);

  /* If the closure is empty, create the closure now */
  if (!data->closure_size)
    return scheme_make_closure(NULL, (Scheme_Object *)data, 0);
  else
    return (Scheme_Object *)data;
}

static Scheme_Object *hash_tree_to_vector(Scheme_Hash_Tree *ht)
{
  Scheme_Object **keys;
  Scheme_Object *vec, *k, *v;
  int i = 0, pos = 0;

  vec = scheme_make_vector(2 * ht->count, NULL);

  keys = scheme_extract_sorted_keys((Scheme_Object *)ht);

  for (i = 0; i < ht->count; i++) {
    k = keys[i];
    v = scheme_hash_tree_get(ht, k);
    SCHEME_VEC_ELS(vec)[pos++] = k;
    SCHEME_VEC_ELS(vec)[pos++] = v;
  }

  return vec;
}

Scheme_Object *scheme_write_linklet(Scheme_Object *obj)
{
  Scheme_Linklet *linklet = (Scheme_Linklet *)obj;
  Scheme_Object *l;

  if (linklet->jit_ready)
    scheme_arg_mismatch("write",
                        "cannot marshal linklet that has been evaluated",
                        obj);

  l = scheme_null;
  
  if (linklet->import_shapes)
    l = scheme_make_pair(linklet->import_shapes, l);
  else
    l = scheme_make_pair(scheme_false, l);

  l = scheme_make_pair(linklet->importss, l);
  l = scheme_make_pair(linklet->defns, l);
  l = scheme_make_pair(hash_tree_to_vector(linklet->source_names), l);

  l = scheme_make_pair(linklet->bodies, l);

  l = scheme_make_pair(scheme_make_integer(linklet->num_exports), l);
  l = scheme_make_pair(scheme_make_integer(linklet->num_lifts), l);
  l = scheme_make_pair(scheme_make_integer(linklet->max_let_depth), l);
  l = scheme_make_pair((linklet->need_instance_access ? scheme_true : scheme_false), l);

  l = scheme_make_pair(linklet->name, l);

  return l;
}

#if 0
# define return_NULL() return (printf("%d\n", __LINE__), NULL)
#else
# define return_NULL() return NULL
#endif

static int is_vector_of_symbols(Scheme_Object *v, int false_ok)
{
  int i;

  if (!SCHEME_VECTORP(v))
    return 0;
  
  for (i = SCHEME_VEC_SIZE(v); i--; ) {
    if (!SCHEME_SYMBOLP(SCHEME_VEC_ELS(v)[i])
        && (!false_ok || !SCHEME_FALSEP(SCHEME_VEC_ELS(v)[i])))
      return 0;
  }

  return 1;
}

static int is_vector_of_shapes(Scheme_Object *v)
{
  int i;
  Scheme_Object *s;

  if (!SCHEME_VECTORP(v))
    return 0;
  
  for (i = SCHEME_VEC_SIZE(v); i--; ) {
    s = SCHEME_VEC_ELS(v)[i];
    if (SCHEME_TRUEP(s)
        && !SCHEME_SYMBOLP(s)
        && !SCHEME_INTP(s)
        && !SAME_OBJ(s, scheme_true)
        && !SAME_OBJ(s, scheme_void))
      return 0;
  }

  return 1;
}

static int is_vector_of_vector_of_symbols(Scheme_Object *v)
{
  int i;

  if (!SCHEME_VECTORP(v))
    return 0;
  
  for (i = SCHEME_VEC_SIZE(v); i--; ) {
    if (!is_vector_of_symbols(SCHEME_VEC_ELS(v)[i], 0))
      return 0;
  }

  return 1;
}

static Scheme_Object *vector_to_hash_tree(Scheme_Object *vec)
{
  Scheme_Hash_Tree *ht;
  int i = 0;

  if (!SCHEME_VECTORP(vec))
    return NULL;
  if (SCHEME_VEC_SIZE(vec) & 0x1)
    return NULL;

  ht = scheme_make_hash_tree(0);
  for (i = SCHEME_VEC_SIZE(vec) - 2; i >= 0; i -= 2) {
    if (!SCHEME_SYMBOLP(SCHEME_VEC_ELS(vec)[i])
        || !SCHEME_SYMBOLP(SCHEME_VEC_ELS(vec)[i+1]))
      return NULL;
    ht = scheme_hash_tree_set(ht, SCHEME_VEC_ELS(vec)[i], SCHEME_VEC_ELS(vec)[i+1]);
  }

  return (Scheme_Object *)ht;
}

Scheme_Object *scheme_read_linklet(Scheme_Object *obj, int unsafe_ok)
{
  Scheme_Linklet *linklet = (Scheme_Linklet *)obj;
  Scheme_Object *e, *a;

  linklet = MALLOC_ONE_TAGGED(Scheme_Linklet);
  linklet->so.type = scheme_linklet_type;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  linklet->name = SCHEME_CAR(obj);
  if (!SCHEME_SYMBOLP(linklet->name)) return_NULL();
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  linklet->need_instance_access = SCHEME_TRUEP(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = SCHEME_CAR(obj);
  linklet->max_let_depth = SCHEME_INT_VAL(e);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = SCHEME_CAR(obj);
  linklet->num_lifts = SCHEME_INT_VAL(e);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = SCHEME_CAR(obj);
  linklet->num_exports = SCHEME_INT_VAL(e);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  a = SCHEME_CAR(obj);
  if (!SCHEME_VECTORP(a)) return_NULL();
  linklet->bodies = a;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  a = vector_to_hash_tree(SCHEME_CAR(obj));
  if (!a) return_NULL();
  linklet->source_names = (Scheme_Hash_Tree *)a;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  a = SCHEME_CAR(obj);
  if (!is_vector_of_symbols(a, 1)) return_NULL();
  linklet->defns = a;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  a = SCHEME_CAR(obj);
  if (!is_vector_of_vector_of_symbols(a)) return_NULL();
  linklet->importss = a;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  a = SCHEME_CAR(obj);
  if (!SCHEME_FALSEP(a)) {
    if (!is_vector_of_shapes(a)) return_NULL();
    linklet->import_shapes = a;
  }
  
  if (linklet->num_exports > SCHEME_VEC_SIZE(linklet->defns))
    return_NULL();
  if (linklet->num_lifts > (SCHEME_VEC_SIZE(linklet->defns) - linklet->num_exports))
    return_NULL();

  {
    int i = 0, j;
    for (j = SCHEME_VEC_SIZE(linklet->importss); j--; ) {
      i += SCHEME_VEC_SIZE(SCHEME_VEC_ELS(linklet->importss)[j]);
    }
    linklet->num_total_imports = i;
  }

  if (linklet->import_shapes) {
    if (linklet->num_total_imports != SCHEME_VEC_SIZE(linklet->import_shapes))
      return_NULL();
  }

  if (!unsafe_ok)
    linklet->reject_eval = 1;

  return (Scheme_Object *)linklet;
}
