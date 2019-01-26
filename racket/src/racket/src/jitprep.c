/* This file implements a bytecode pass to insert hook that trigger
   JIT compilation. This pass is performed after bytecode is marshaled
   or unmarshaled.

   See "eval.c" for an overview of compilation passes and JIT
   prepraration. */

#include "schpriv.h"
#include "schrunst.h"
#include "schmach.h"

THREAD_LOCAL_DECL(static Scheme_Object *current_linklet_native_lambdas);
static int force_jit;

#ifdef MZ_USE_JIT

void scheme_init_jitprep()
{
  REGISTER_SO(current_linklet_native_lambdas);

  if (getenv("PLT_EAGER_JIT"))
    force_jit = 1;
}

static Scheme_Object *jit_expr(Scheme_Object *expr);

static Scheme_Object *jit_application(Scheme_Object *o)
{
  Scheme_Object *orig, *naya = NULL;
  Scheme_App_Rec *app, *app2;
  int i, n, size;

  app = (Scheme_App_Rec *)o;
  n = app->num_args + 1;

  for (i = 0; i < n; i++) {
    orig = app->args[i];
    naya = jit_expr(orig);
    if (!SAME_OBJ(orig, naya))
      break;
  }

  if (i >= n)
    return o;

  size = (sizeof(Scheme_App_Rec) 
	  + ((n - mzFLEX_DELTA) * sizeof(Scheme_Object *))
	  + n * sizeof(char));
  app2 = (Scheme_App_Rec *)scheme_malloc_tagged(size);
  memcpy(app2, app, size);
  app2->args[i] = naya;

  for (i++; i < n; i++) {
    orig = app2->args[i];
    naya = jit_expr(orig);
    app2->args[i] = naya;
  }
  
  return (Scheme_Object *)app2;
}

static Scheme_Object *jit_application2(Scheme_Object *o)
{
  Scheme_App2_Rec *app;
  Scheme_Object *nrator, *nrand;

  app = (Scheme_App2_Rec *)o;

  nrator = jit_expr(app->rator);
  nrand = jit_expr(app->rand);
  
  if (SAME_OBJ(nrator, app->rator)
      && SAME_OBJ(nrand, app->rand))
    return o;

  app = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
  memcpy(app, o, sizeof(Scheme_App2_Rec));
  app->rator = nrator;
  app->rand = nrand;

  return (Scheme_Object *)app;
}

static Scheme_Object *jit_application3(Scheme_Object *o)
{
  Scheme_App3_Rec *app;
  Scheme_Object *nrator, *nrand1, *nrand2;

  app = (Scheme_App3_Rec *)o;

  nrator = jit_expr(app->rator);
  nrand1 = jit_expr(app->rand1);
  nrand2 = jit_expr(app->rand2);
  
  if (SAME_OBJ(nrator, app->rator)
      && SAME_OBJ(nrand1, app->rand1)
      && SAME_OBJ(nrand2, app->rand2))
    return o;

  app = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
  memcpy(app, o, sizeof(Scheme_App3_Rec));
  app->rator = nrator;
  app->rand1 = nrand1;
  app->rand2 = nrand2;

  return (Scheme_Object *)app;
}

static Scheme_Object *jit_sequence(Scheme_Object *o)
{
  Scheme_Object *orig, *naya = NULL;
  Scheme_Sequence *seq, *seq2;
  int i, n, size;

  seq = (Scheme_Sequence *)o;
  n = seq->count;

  for (i = 0; i < n; i++) {
    orig = seq->array[i];
    naya = jit_expr(orig);
    if (!SAME_OBJ(orig, naya))
      break;
  }

  if (i >= n)
    return o;

  size = (sizeof(Scheme_Sequence) 
	  + ((n - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
  seq2 = (Scheme_Sequence *)scheme_malloc_tagged(size);
  memcpy(seq2, seq, size);
  seq2->array[i] = naya;

  for (i++; i < n; i++) {
    orig = seq2->array[i];
    naya = jit_expr(orig);
    seq2->array[i] = naya;
  }
  
  return (Scheme_Object *)seq2;
}

static Scheme_Object *jit_branch(Scheme_Object *o)
{
  Scheme_Branch_Rec *b;
  Scheme_Object *t, *tb, *fb;

  b = (Scheme_Branch_Rec *)o;

  t = jit_expr(b->test);
  tb = jit_expr(b->tbranch);
  fb = jit_expr(b->fbranch);

  if (SAME_OBJ(t, b->test)
      && SAME_OBJ(tb, b->tbranch)
      && SAME_OBJ(fb, b->fbranch))
    return o;

  b = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
  memcpy(b, o, sizeof(Scheme_Branch_Rec));
  b->test = t;
  b->tbranch = tb;
  b->fbranch = fb;

  return (Scheme_Object *)b;
}

static Scheme_Object *jit_let_value(Scheme_Object *o)
{
  Scheme_Let_Value *lv = (Scheme_Let_Value *)o;
  Scheme_Object *body, *rhs;

  rhs = jit_expr(lv->value);
  body = jit_expr(lv->body);

  if (SAME_OBJ(rhs, lv->value)
      && SAME_OBJ(body, lv->body))
    return o;

  lv = MALLOC_ONE_TAGGED(Scheme_Let_Value);
  memcpy(lv, o, sizeof(Scheme_Let_Value));
  lv->value = rhs;
  lv->body = body;

  return (Scheme_Object *)lv;
}

static Scheme_Object *jit_let_one(Scheme_Object *o)
{
  Scheme_Let_One *lo = (Scheme_Let_One *)o;
  Scheme_Object *body, *rhs;

  rhs = jit_expr(lo->value);
  body = jit_expr(lo->body);

  if (SAME_OBJ(rhs, lo->value)
      && SAME_OBJ(body, lo->body))
    return o;

  lo = MALLOC_ONE_TAGGED(Scheme_Let_One);
  memcpy(lo, o, sizeof(Scheme_Let_One));
  lo->value = rhs;
  lo->body = body;

  return (Scheme_Object *)lo;
}

static Scheme_Object *jit_let_void(Scheme_Object *o)
{
  Scheme_Let_Void *lv = (Scheme_Let_Void *)o;
  Scheme_Object *body;

  body = jit_expr(lv->body);

  if (SAME_OBJ(body, lv->body))
    return o;

  lv = MALLOC_ONE_TAGGED(Scheme_Let_Void);
  memcpy(lv, o, sizeof(Scheme_Let_Void));
  lv->body = body;

  return (Scheme_Object *)lv;
}

static Scheme_Object *jit_letrec(Scheme_Object *o)
{
  Scheme_Letrec *lr = (Scheme_Letrec *)o, *lr2;
  Scheme_Object **procs, **procs2, *v;
  int i, count;

  count = lr->count;

  lr2 = MALLOC_ONE_TAGGED(Scheme_Letrec);
  memcpy(lr2, lr, sizeof(Scheme_Letrec));
  
  procs = lr->procs;
  procs2 = MALLOC_N(Scheme_Object *, count);
  lr2->procs = procs2;

  for (i = 0; i < count; i++) {
    v = scheme_jit_closure(procs[i], (Scheme_Object *)lr2);
    procs2[i] = v;
  }

  v = jit_expr(lr->body);
  lr2->body = v;

  return (Scheme_Object *)lr2;
}

static Scheme_Object *jit_wcm(Scheme_Object *o)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)o;
  Scheme_Object *k, *v, *b;

  k = jit_expr(wcm->key);
  v = jit_expr(wcm->val);
  b = jit_expr(wcm->body);
  if (SAME_OBJ(wcm->key, k)
      && SAME_OBJ(wcm->val, v)
      && SAME_OBJ(wcm->body, b))
    return o;

  wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  memcpy(wcm, o, sizeof(Scheme_With_Continuation_Mark));

  wcm->key = k;
  wcm->val = v;
  wcm->body = b;

  return (Scheme_Object *)wcm;
}

/*========================================================================*/
/*                            other syntax                                */
/*========================================================================*/

static Scheme_Object *clone_inline_variant(Scheme_Object *obj, Scheme_Object *naya)
{
  Scheme_Object *naya2;
  naya2 = scheme_make_vector(3, scheme_false);
  naya2->type = scheme_inline_variant_type;
  SCHEME_VEC_ELS(naya2)[0] = naya;
  SCHEME_VEC_ELS(naya2)[1] = SCHEME_VEC_ELS(obj)[1];
  return naya2;
}

static Scheme_Object *define_values_jit(Scheme_Object *data)
{
  Scheme_Object *orig = SCHEME_DEFN_RHS(data), *naya;

  if (SAME_TYPE(SCHEME_TYPE(orig), scheme_lambda_type)
      && (SCHEME_DEFN_VAR_COUNT(data) == 1))
    naya = scheme_jit_closure(orig, SCHEME_DEFN_VAR_(data, 0));
  else if (SAME_TYPE(SCHEME_TYPE(orig), scheme_inline_variant_type)
           && SAME_TYPE(SCHEME_TYPE(SCHEME_VEC_ELS(orig)[0]), scheme_lambda_type)
           && (SCHEME_DEFN_VAR_COUNT(data) == 1)) {
    naya = scheme_jit_closure(SCHEME_VEC_ELS(orig)[0], SCHEME_DEFN_VAR_(data, 0));
    if (!SAME_OBJ(naya, SCHEME_DEFN_RHS(orig)))
      naya = clone_inline_variant(orig, naya);
  } else
    naya = jit_expr(orig);

  if (SAME_OBJ(naya, orig))
    return data;
  else {
    orig = naya;
    naya = scheme_clone_vector(data, 0, 1);
    SCHEME_DEFN_RHS(naya) = orig;
    return naya;
  }
}

static Scheme_Object *inline_variant_jit(Scheme_Object *data)
{
  Scheme_Object *a, *orig;

  orig = SCHEME_VEC_ELS(data)[0];
  a = jit_expr(orig);
  if (!SAME_OBJ(a, orig))
    return clone_inline_variant(data, a);
  else
    return data;
}

static Scheme_Object *set_jit(Scheme_Object *data)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)data, *naya;
  Scheme_Object *orig_val, *naya_val;

  orig_val = sb->val;

  naya_val = jit_expr(orig_val);
  
  if (SAME_OBJ(naya_val, orig_val))
    return data;
  else {
    naya = MALLOC_ONE_TAGGED(Scheme_Set_Bang);
    memcpy(naya, sb, sizeof(Scheme_Set_Bang));
    naya->val = naya_val;
    return (Scheme_Object *)naya;
  }
}

static Scheme_Object *ref_jit(Scheme_Object *data)
{
  return data;
}

static Scheme_Object *apply_values_jit(Scheme_Object *data)
{
  Scheme_Object *f, *e;

  f = jit_expr(SCHEME_PTR1_VAL(data));
  e = jit_expr(SCHEME_PTR2_VAL(data));
  
  if (SAME_OBJ(f, SCHEME_PTR1_VAL(data))
      && SAME_OBJ(e, SCHEME_PTR2_VAL(data)))
    return data;
  else {
    data = scheme_alloc_object();
    data->type = scheme_apply_values_type;
    SCHEME_PTR1_VAL(data) = f;
    SCHEME_PTR2_VAL(data) = e;
    return data;
  }
}

static Scheme_Object *with_immed_mark_jit(Scheme_Object *o)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)o;
  Scheme_Object *k, *v, *b;

  k = jit_expr(wcm->key);
  v = jit_expr(wcm->val);
  b = jit_expr(wcm->body);
  if (SAME_OBJ(wcm->key, k)
      && SAME_OBJ(wcm->val, v)
      && SAME_OBJ(wcm->body, b))
    return o;

  wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  memcpy(wcm, o, sizeof(Scheme_With_Continuation_Mark));

  wcm->key = k;
  wcm->val = v;
  wcm->body = b;

  return (Scheme_Object *)wcm;
}

Scheme_Object *scheme_case_lambda_jit(Scheme_Object *expr)
{
#ifdef MZ_USE_JIT
  Scheme_Case_Lambda *seqin = (Scheme_Case_Lambda *)expr;

  if (!seqin->native_code) {
    Scheme_Case_Lambda *seqout;
    Scheme_Native_Lambda *ndata;
    Scheme_Object *val, *name;
    int i, cnt, size, all_closed = 1;

    cnt = seqin->count;
    
    size = sizeof(Scheme_Case_Lambda) + ((cnt - mzFLEX_DELTA) * sizeof(Scheme_Object *));

    seqout = (Scheme_Case_Lambda *)scheme_malloc_tagged(size);
    memcpy(seqout, seqin, size);

    name = seqin->name;
    if (name && SCHEME_BOXP(name))
      name = SCHEME_BOX_VAL(name);

    for (i = 0; i < cnt; i++) {
      val = seqout->array[i];
      if (SCHEME_PROCP(val)) {
	/* Undo creation of empty closure */
	val = (Scheme_Object *)((Scheme_Closure *)val)->code;
	seqout->array[i] = val;
      }
      ((Scheme_Lambda *)val)->name = name;
      if (((Scheme_Lambda *)val)->closure_size)
	all_closed = 0;
    }

    /* Generating the code may cause empty closures to be formed: */
    ndata = scheme_generate_case_lambda(seqout);
    seqout->native_code = ndata;

    if (current_linklet_native_lambdas) {
      for (i = 0; i < cnt; i++) {
        val = seqout->array[i];
        {
          /* Force jitprep on body, too, to discover all lambdas */
          Scheme_Object *body;
          body = jit_expr(((Scheme_Lambda *)val)->body);
          ((Scheme_Lambda *)val)->body = body;
        }
        val = (Scheme_Object *)((Scheme_Lambda *)val)->u.native_code;
        current_linklet_native_lambdas = scheme_make_pair(val, current_linklet_native_lambdas);
      }
    }

    if (all_closed) {
      /* Native closures do not refer back to the original bytecode,
	 so no need to worry about clearing the reference. */
      Scheme_Native_Closure *nc;
      nc = (Scheme_Native_Closure *)scheme_make_native_case_closure(ndata);
      for (i = 0; i < cnt; i++) {
	val = seqout->array[i];
	if (!SCHEME_PROCP(val)) {
	  val = scheme_make_native_closure(((Scheme_Lambda *)val)->u.native_code);
	}
	nc->vals[i] = val;
      }
      return (Scheme_Object *)nc;
    } else {
      /* The case-lambda data must point to the original closure-data
	 record, because that's where the closure maps are kept. But
	 we don't need the bytecode, anymore. So clone the
	 closure-data record and drop the bytecode in thte clone. */
      for (i = 0; i < cnt; i++) {
	val = seqout->array[i];
	if (!SCHEME_PROCP(val)) {
	  Scheme_Lambda *data;
	  data = MALLOC_ONE_TAGGED(Scheme_Lambda);
	  memcpy(data, val, sizeof(Scheme_Lambda));
	  data->body = NULL;
	  seqout->array[i] = (Scheme_Object *)data;
	}
      }
    }

    return (Scheme_Object *)seqout;
  }
#endif
 
  return expr;
}

static Scheme_Object *bangboxenv_jit(Scheme_Object *data)
{
  Scheme_Object *orig, *naya, *new_data;

  orig = SCHEME_PTR2_VAL(data);
  naya = jit_expr(orig);
  if (SAME_OBJ(naya, orig))
    return data;
  else {
    new_data = scheme_alloc_object();
    new_data->type = scheme_boxenv_type;
    SCHEME_PTR1_VAL(new_data) = SCHEME_PTR1_VAL(data);
    SCHEME_PTR2_VAL(new_data) = naya;
    return new_data;
  }
}

static Scheme_Object *begin0_jit(Scheme_Object *data)
{
  Scheme_Sequence *seq = (Scheme_Sequence *)data, *seq2;
  Scheme_Object *old, *naya = NULL;
  int i, j, count;

  count = seq->count;
  for (i = 0; i < count; i++) {
    old = seq->array[i];
    naya = jit_expr(old);
    if (!SAME_OBJ(old, naya))
      break;
  }

  if (i >= count)
    return data;

  seq2 = (Scheme_Sequence *)scheme_malloc_tagged(sizeof(Scheme_Sequence)
						 + (count - mzFLEX_DELTA) 
						 * sizeof(Scheme_Object *));
  seq2->so.type = scheme_begin0_sequence_type;
  seq2->count = count;
  for (j = 0; j < i; j++) {
    seq2->array[j] = seq->array[j];
  }
  seq2->array[i] = naya;
  for (i++; i < count; i++) {
    old = seq->array[i];
    naya = jit_expr(old);
    seq2->array[i] = naya;
  }
  
  return (Scheme_Object *)seq2;
}

/*========================================================================*/
/*                             closures                                   */
/*========================================================================*/

Scheme_Object *scheme_jit_closure(Scheme_Object *code, Scheme_Object *context)
  /* If lr is supplied as a letrec binding this closure, it may be used
     for JIT compilation. */
{
#ifdef MZ_USE_JIT
  Scheme_Lambda *data = (Scheme_Lambda *)code, *data2;

  /* We need to cache clones to support multiple references
     to a zero-sized closure in bytecode. We need either a clone
     or native code, and context determines which field is relevant,
     so we put the two possibilities in a union `u'. */

  if (!context)
    data2 = data->u.jit_clone;
  else
    data2 = NULL;

  if (!data2) {
    Scheme_Native_Lambda *ndata;
    
    data2 = MALLOC_ONE_TAGGED(Scheme_Lambda);
    memcpy(data2, code, sizeof(Scheme_Lambda));

    data2->context = context;

    ndata = scheme_generate_lambda(data2, 1, NULL);
    data2->u.native_code = ndata;

    if (current_linklet_native_lambdas)
      current_linklet_native_lambdas = scheme_make_pair((Scheme_Object *)ndata,
                                                        current_linklet_native_lambdas);

    if (!context)
      data->u.jit_clone = data2;

    if (current_linklet_native_lambdas) {
      /* Force jitprep on body, too, to discover all lambdas */
      Scheme_Object *body;
      body = jit_expr(data2->body);
      data2->body = body;
    }
  }

  /* If it's zero-sized, then create closure now */
  if (!data2->closure_size)
    return scheme_make_native_closure(data2->u.native_code);

  return (Scheme_Object *)data2;
#endif

  return code;
}

/*========================================================================*/
/*                            expressions                                 */
/*========================================================================*/

static Scheme_Object *jit_expr_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *expr = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return jit_expr(expr);
}

static Scheme_Object *jit_expr(Scheme_Object *expr)
{
  Scheme_Type type = SCHEME_TYPE(expr);

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;

      p->ku.k.p1 = (void *)expr;

      return scheme_handle_stack_overflow(jit_expr_k);
    }
  }
#endif

  switch (type) {
  case scheme_application_type:
    return jit_application(expr);
  case scheme_application2_type:
    return jit_application2(expr);
  case scheme_application3_type:
    return jit_application3(expr);
  case scheme_sequence_type:
    return jit_sequence(expr);
  case scheme_branch_type:
    return jit_branch(expr);
  case scheme_with_cont_mark_type:
    return jit_wcm(expr);
  case scheme_lambda_type:
    return scheme_jit_closure(expr, NULL);
  case scheme_let_value_type:
    return jit_let_value(expr);
  case scheme_let_void_type:
    return jit_let_void(expr);
  case scheme_letrec_type:
    return jit_letrec(expr);
  case scheme_let_one_type:
    return jit_let_one(expr);
  case scheme_closure_type:
    {
      Scheme_Closure *c = (Scheme_Closure *)expr;
      if (ZERO_SIZED_CLOSUREP(c)) {
	/* JIT the closure body, producing a native closure: */
	return scheme_jit_closure((Scheme_Object *)c->code, NULL);
      } else
	return expr;
    }
  case scheme_case_closure_type:
    {
      return scheme_unclose_case_lambda(expr, 1);
    }
  case scheme_define_values_type:
    return define_values_jit(expr);
  case scheme_set_bang_type:
    return set_jit(expr);
  case scheme_boxenv_type:
    return bangboxenv_jit(expr);
  case scheme_begin0_sequence_type:
    return begin0_jit(expr);
  case scheme_varref_form_type:
    return ref_jit(expr);
  case scheme_apply_values_type:
    return apply_values_jit(expr);
  case scheme_with_immed_mark_type:
    return with_immed_mark_jit(expr);
  case scheme_case_lambda_sequence_type:
    return scheme_case_lambda_jit(expr);
  case scheme_inline_variant_type:
    return inline_variant_jit(expr);
  default:
    return expr;
  }
}

Scheme_Linklet *scheme_jit_linklet(Scheme_Linklet *linklet, int step)
/* step 1: clone the immediate record, to be mutated for actual prepataion
   step 2: actual preparation */
{
  Scheme_Linklet *new_linklet;
  Scheme_Object *bodies, *v;
  int i;

  if (force_jit)
    step = 2;

  if (!linklet->jit_ready) {
    new_linklet = MALLOC_ONE_TAGGED(Scheme_Linklet);
    memcpy(new_linklet, linklet, sizeof(Scheme_Linklet));
  } else
    new_linklet = linklet;

  if (new_linklet->jit_ready >= step)
    return new_linklet;

  if (step == 1) {
    new_linklet->jit_ready = 1;
    return new_linklet;
  }

  if (force_jit)
    current_linklet_native_lambdas = scheme_null;

  i = SCHEME_VEC_SIZE(linklet->bodies);
  bodies = scheme_make_vector(i, NULL);
  while (i--) {
    v = jit_expr(SCHEME_VEC_ELS(linklet->bodies)[i]);
    SCHEME_VEC_ELS(bodies)[i] = v;
  }

  new_linklet->bodies = bodies;

  new_linklet->jit_ready = 2;

  new_linklet->native_lambdas = current_linklet_native_lambdas;
  current_linklet_native_lambdas = NULL;

  return new_linklet;
}

#else

Scheme_Linklet *scheme_jit_linklet(Scheme_Linklet *linklet, int step)
{
  return linklet;
}

#endif
