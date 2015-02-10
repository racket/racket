/*
  Racket
  Copyright (c) 2004-2014 PLT Design Inc.
  Copyright (c) 1995-2001 Matthew Flatt

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

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"

#define cons(a,b) scheme_make_pair(a,b)
#define CONS(a,b) scheme_make_pair(a,b)

static Scheme_Object *write_let_value(Scheme_Object *obj);
static Scheme_Object *read_let_value(Scheme_Object *obj);
static Scheme_Object *write_let_void(Scheme_Object *obj);
static Scheme_Object *read_let_void(Scheme_Object *obj);
static Scheme_Object *write_letrec(Scheme_Object *obj);
static Scheme_Object *read_letrec(Scheme_Object *obj);
static Scheme_Object *write_let_one(Scheme_Object *obj);
static Scheme_Object *read_let_one(Scheme_Object *obj);
static Scheme_Object *write_top(Scheme_Object *obj);
static Scheme_Object *read_top(Scheme_Object *obj);
static Scheme_Object *write_case_lambda(Scheme_Object *obj);
static Scheme_Object *read_case_lambda(Scheme_Object *obj);

static Scheme_Object *read_define_values(Scheme_Object *obj);
static Scheme_Object *write_define_values(Scheme_Object *obj);
static Scheme_Object *read_define_syntaxes(Scheme_Object *obj);
static Scheme_Object *write_define_syntaxes(Scheme_Object *obj);
static Scheme_Object *read_begin_for_syntax(Scheme_Object *obj);
static Scheme_Object *write_begin_for_syntax(Scheme_Object *obj);
static Scheme_Object *read_set_bang(Scheme_Object *obj);
static Scheme_Object *write_set_bang(Scheme_Object *obj);
static Scheme_Object *read_boxenv(Scheme_Object *obj);
static Scheme_Object *write_boxenv(Scheme_Object *obj);
static Scheme_Object *read_varref(Scheme_Object *obj);
static Scheme_Object *write_varref(Scheme_Object *obj);
static Scheme_Object *read_apply_values(Scheme_Object *obj);
static Scheme_Object *write_apply_values(Scheme_Object *obj);
static Scheme_Object *read_inline_variant(Scheme_Object *obj);
static Scheme_Object *write_inline_variant(Scheme_Object *obj);

static Scheme_Object *write_application(Scheme_Object *obj);
static Scheme_Object *read_application(Scheme_Object *obj);
static Scheme_Object *write_sequence(Scheme_Object *obj);
static Scheme_Object *read_sequence(Scheme_Object *obj);
static Scheme_Object *read_sequence_save_first(Scheme_Object *obj);
static Scheme_Object *read_sequence_splice(Scheme_Object *obj);
static Scheme_Object *write_branch(Scheme_Object *obj);
static Scheme_Object *read_branch(Scheme_Object *obj);
static Scheme_Object *write_with_cont_mark(Scheme_Object *obj);
static Scheme_Object *read_with_cont_mark(Scheme_Object *obj);
static Scheme_Object *write_quote_syntax(Scheme_Object *obj);
static Scheme_Object *read_quote_syntax(Scheme_Object *obj);

static Scheme_Object *write_toplevel(Scheme_Object *obj);
static Scheme_Object *read_toplevel(Scheme_Object *obj);
static Scheme_Object *write_variable(Scheme_Object *obj);
static Scheme_Object *read_variable(Scheme_Object *obj);
static Scheme_Object *write_module_variable(Scheme_Object *obj);
static Scheme_Object *read_module_variable(Scheme_Object *obj);
static Scheme_Object *write_local(Scheme_Object *obj);
static Scheme_Object *read_local(Scheme_Object *obj);
static Scheme_Object *read_local_unbox(Scheme_Object *obj);
static Scheme_Object *write_resolve_prefix(Scheme_Object *obj);
static Scheme_Object *read_resolve_prefix(Scheme_Object *obj);

static Scheme_Object *write_compiled_closure(Scheme_Object *obj);
static Scheme_Object *read_compiled_closure(Scheme_Object *obj);

static Scheme_Object *write_module(Scheme_Object *obj);
static Scheme_Object *read_module(Scheme_Object *obj);
static Scheme_Object *read_top_level_require(Scheme_Object *obj);
static Scheme_Object *write_top_level_require(Scheme_Object *obj);

void scheme_init_marshal(Scheme_Env *env) 
{
  scheme_install_type_writer(scheme_application_type, write_application);
  scheme_install_type_reader(scheme_application_type, read_application);
  scheme_install_type_writer(scheme_application2_type, write_application);
  scheme_install_type_reader(scheme_application2_type, read_application);
  scheme_install_type_writer(scheme_application3_type, write_application);
  scheme_install_type_reader(scheme_application3_type, read_application);
  scheme_install_type_writer(scheme_sequence_type, write_sequence);
  scheme_install_type_reader(scheme_sequence_type, read_sequence);
  scheme_install_type_writer(scheme_branch_type, write_branch);
  scheme_install_type_reader(scheme_branch_type, read_branch);
  scheme_install_type_writer(scheme_with_cont_mark_type, write_with_cont_mark);
  scheme_install_type_reader(scheme_with_cont_mark_type, read_with_cont_mark);
  scheme_install_type_writer(scheme_quote_syntax_type, write_quote_syntax);
  scheme_install_type_reader(scheme_quote_syntax_type, read_quote_syntax);
  scheme_install_type_writer(scheme_begin0_sequence_type, write_sequence);
  scheme_install_type_reader(scheme_begin0_sequence_type, read_sequence_save_first);
  scheme_install_type_writer(scheme_splice_sequence_type, write_sequence);
  scheme_install_type_reader(scheme_splice_sequence_type, read_sequence_splice);
  
 scheme_install_type_writer(scheme_let_value_type, write_let_value);
  scheme_install_type_reader(scheme_let_value_type, read_let_value);
  scheme_install_type_writer(scheme_let_void_type, write_let_void);
  scheme_install_type_reader(scheme_let_void_type, read_let_void);
  scheme_install_type_writer(scheme_letrec_type, write_letrec);
  scheme_install_type_reader(scheme_letrec_type, read_letrec);
  scheme_install_type_writer(scheme_let_one_type, write_let_one);
  scheme_install_type_reader(scheme_let_one_type, read_let_one);
  scheme_install_type_writer(scheme_case_lambda_sequence_type, write_case_lambda);
  scheme_install_type_reader(scheme_case_lambda_sequence_type, read_case_lambda);

  scheme_install_type_writer(scheme_define_values_type, write_define_values);
  scheme_install_type_reader(scheme_define_values_type, read_define_values);
  scheme_install_type_writer(scheme_define_syntaxes_type, write_define_syntaxes);
  scheme_install_type_reader(scheme_define_syntaxes_type, read_define_syntaxes);
  scheme_install_type_writer(scheme_begin_for_syntax_type, write_begin_for_syntax);
  scheme_install_type_reader(scheme_begin_for_syntax_type, read_begin_for_syntax);
  scheme_install_type_writer(scheme_set_bang_type, write_set_bang);
  scheme_install_type_reader(scheme_set_bang_type, read_set_bang);
  scheme_install_type_writer(scheme_boxenv_type, write_boxenv);
  scheme_install_type_reader(scheme_boxenv_type, read_boxenv);
  scheme_install_type_writer(scheme_varref_form_type, write_varref);
  scheme_install_type_reader(scheme_varref_form_type, read_varref);
  scheme_install_type_writer(scheme_apply_values_type, write_apply_values);
  scheme_install_type_reader(scheme_apply_values_type, read_apply_values);
  scheme_install_type_writer(scheme_inline_variant_type, write_inline_variant);
  scheme_install_type_reader(scheme_inline_variant_type, read_inline_variant);

  scheme_install_type_writer(scheme_compilation_top_type, write_top);
  scheme_install_type_reader(scheme_compilation_top_type, read_top);

  scheme_install_type_writer(scheme_unclosed_procedure_type,
			     write_compiled_closure);
  scheme_install_type_reader(scheme_unclosed_procedure_type,
			     read_compiled_closure);

  scheme_install_type_writer(scheme_toplevel_type, write_toplevel);
  scheme_install_type_reader(scheme_toplevel_type, read_toplevel);
  scheme_install_type_writer(scheme_variable_type, write_variable);
  scheme_install_type_reader(scheme_variable_type, read_variable);
  scheme_install_type_writer(scheme_module_variable_type, write_module_variable);
  scheme_install_type_reader(scheme_module_variable_type, read_module_variable);
  scheme_install_type_writer(scheme_local_type, write_local);
  scheme_install_type_reader(scheme_local_type, read_local);
  scheme_install_type_writer(scheme_local_unbox_type, write_local);
  scheme_install_type_reader(scheme_local_unbox_type, read_local_unbox);
  scheme_install_type_writer(scheme_resolve_prefix_type, write_resolve_prefix);
  scheme_install_type_reader(scheme_resolve_prefix_type, read_resolve_prefix);

  scheme_install_type_writer(scheme_module_type, write_module);
  scheme_install_type_reader(scheme_module_type, read_module);
  scheme_install_type_writer(scheme_require_form_type, write_top_level_require);
  scheme_install_type_reader(scheme_require_form_type, read_top_level_require);
}


static Scheme_Object *write_let_value(Scheme_Object *obj)
{
  Scheme_Let_Value *lv;
 
  lv = (Scheme_Let_Value *)obj;

  return cons(scheme_make_integer(lv->count),
	      cons(scheme_make_integer(lv->position),
		   cons(SCHEME_LET_AUTOBOX(lv) ? scheme_true : scheme_false,
			cons(scheme_protect_quote(lv->value), 
			     scheme_protect_quote(lv->body)))));
}

static Scheme_Object *read_let_value(Scheme_Object *obj)
{
  Scheme_Let_Value *lv;
 
  lv = (Scheme_Let_Value *)scheme_malloc_tagged(sizeof(Scheme_Let_Value));
  lv->iso.so.type = scheme_let_value_type;

  if (!SCHEME_PAIRP(obj)) return NULL;
  lv->count = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  lv->position = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  SCHEME_LET_AUTOBOX(lv) = SCHEME_TRUEP(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  lv->value = SCHEME_CAR(obj);
  lv->body = SCHEME_CDR(obj);

  return (Scheme_Object *)lv;
}

static Scheme_Object *write_let_void(Scheme_Object *obj)
{
  Scheme_Let_Void *lv;
 
  lv = (Scheme_Let_Void *)obj;

  return cons(scheme_make_integer(lv->count), 
	      cons(SCHEME_LET_AUTOBOX(lv) ? scheme_true : scheme_false,
		   scheme_protect_quote(lv->body)));
}

static Scheme_Object *read_let_void(Scheme_Object *obj)
{
  Scheme_Let_Void *lv;
 
  lv = (Scheme_Let_Void *)scheme_malloc_tagged(sizeof(Scheme_Let_Void));
  lv->iso.so.type = scheme_let_void_type;
  
  if (!SCHEME_PAIRP(obj)) return NULL;
  lv->count = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  SCHEME_LET_AUTOBOX(lv) = SCHEME_TRUEP(SCHEME_CAR(obj));
  lv->body = SCHEME_CDR(obj);

  return (Scheme_Object *)lv;
}

static Scheme_Object *write_let_one(Scheme_Object *obj)
{
  scheme_signal_error("let-one writer shouldn't be used");
  return NULL;
}

static Scheme_Object *read_let_one(Scheme_Object *obj)
{
  return NULL;
}

static Scheme_Object *write_letrec(Scheme_Object *obj)
{
  Scheme_Letrec *lr = (Scheme_Letrec *)obj;
  Scheme_Object *l = scheme_null;
  int i = lr->count;
  
  while (i--) {
    l = cons(scheme_protect_quote(lr->procs[i]), l);
  }

  return cons(scheme_make_integer(lr->count), 
	      cons(scheme_protect_quote(lr->body), l));
}

static Scheme_Object *read_letrec(Scheme_Object *obj)
{
  Scheme_Letrec *lr;
  int i, c;
  Scheme_Object **sa;

  lr = MALLOC_ONE_TAGGED(Scheme_Letrec);

  lr->so.type = scheme_letrec_type;

  if (!SCHEME_PAIRP(obj)) return NULL;
  c = lr->count = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return NULL;
  lr->body = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  if (c < 0) return NULL;
  if (c < 4096)
    sa = MALLOC_N(Scheme_Object*, c);
  else {
    sa = scheme_malloc_fail_ok(scheme_malloc, scheme_check_overflow(c, sizeof(Scheme_Object *), 0));
    if (!sa) scheme_signal_error("out of memory allocating letrec bytecode");
  }
  lr->procs = sa;
  for (i = 0; i < c; i++) {
    if (!SCHEME_PAIRP(obj)) return NULL;
    lr->procs[i] = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
  }

  return (Scheme_Object *)lr;
}

static Scheme_Object *write_top(Scheme_Object *obj)
{
  Scheme_Compilation_Top *top = (Scheme_Compilation_Top *)obj;

  if (!top->prefix)
    scheme_contract_error("write",
                          "cannot marshal shared compiled code",
                          "compiled code", 1, obj,
                          NULL);

  return cons(scheme_make_integer(top->max_let_depth),
	      cons((Scheme_Object *)top->prefix,
		   scheme_protect_quote(top->code)));
}

static Scheme_Object *read_top(Scheme_Object *obj)
{
  Scheme_Compilation_Top *top;

  top = MALLOC_ONE_TAGGED(Scheme_Compilation_Top);
  top->iso.so.type = scheme_compilation_top_type;
  if (!SCHEME_PAIRP(obj)) return NULL;
  top->max_let_depth = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  top->prefix = (Resolve_Prefix *)SCHEME_CAR(obj);
  top->code = SCHEME_CDR(obj);
  if (!SAME_TYPE(SCHEME_TYPE(top->prefix), scheme_resolve_prefix_type)) 
    return NULL;

  return (Scheme_Object *)top;
}

static Scheme_Object *write_case_lambda(Scheme_Object *obj)
{
  Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)obj;
  int i;
  Scheme_Object *l;

  i = cl->count;

  l = scheme_null;
  for (; i--; ) {
    l = cons(cl->array[i], l);
  }
  
  return cons((cl->name ? cl->name : scheme_null),
	      l);
}

static Scheme_Object *read_case_lambda(Scheme_Object *obj)
{
  Scheme_Object *s, *a;
  int count, i, all_closed = 1;
  Scheme_Case_Lambda *cl;

  if (!SCHEME_PAIRP(obj)) return NULL;
  s = SCHEME_CDR(obj);
  for (count = 0; SCHEME_PAIRP(s); s = SCHEME_CDR(s)) {
    count++;
  }

  cl = (Scheme_Case_Lambda *)
    scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
			 + (count - mzFLEX_DELTA) * sizeof(Scheme_Object *));

  cl->so.type = scheme_case_lambda_sequence_type;
  cl->count = count;
  cl->name = SCHEME_CAR(obj);
  if (SCHEME_NULLP(cl->name))
    cl->name = NULL;

  s = SCHEME_CDR(obj);
  for (i = 0; i < count; i++, s = SCHEME_CDR(s)) {
    a = SCHEME_CAR(s);
    cl->array[i] = a;
    if (!SCHEME_PROCP(a)) {
      if (!SAME_TYPE(SCHEME_TYPE(a), scheme_unclosed_procedure_type))
        return NULL;
      all_closed = 0;
    }
  }

  if (all_closed) {
    /* Empty closure: produce procedure value directly.
       (We assume that this was generated by a direct write of
        a case-lambda data record in print.c, and that it's not
	in a CASE_LAMBDA_EXPD syntax record.) */
    return scheme_case_lambda_execute((Scheme_Object *)cl);
  }

  return (Scheme_Object *)cl;
}

static Scheme_Object *read_define_values(Scheme_Object *obj)
{
  if (!SCHEME_VECTORP(obj)) return NULL;

  obj = scheme_clone_vector(obj, 0, 0);
  obj->type = scheme_define_values_type;
  return obj;
}

static Scheme_Object *write_define_values(Scheme_Object *obj)
{
  Scheme_Object *e;

  obj = scheme_clone_vector(obj, 0, 0);
  e = scheme_protect_quote(SCHEME_VEC_ELS(obj)[0]);
  SCHEME_VEC_ELS(obj)[0] = e;

  return obj;
}

static Scheme_Object *read_define_syntaxes(Scheme_Object *obj)
{
  if (!SCHEME_VECTORP(obj)) return NULL;

  obj = scheme_clone_vector(obj, 0, 0);
  obj->type = scheme_define_syntaxes_type;
  return obj;
}

static Scheme_Object *write_define_syntaxes(Scheme_Object *obj)
{
  return write_define_values(obj);
}

static Scheme_Object *read_begin_for_syntax(Scheme_Object *obj)
{
  if (!SCHEME_VECTORP(obj)) return NULL;

  obj = scheme_clone_vector(obj, 0, 0);
  obj->type = scheme_begin_for_syntax_type;
  return obj;
}

static Scheme_Object *write_begin_for_syntax(Scheme_Object *obj)
{
  return write_define_values(obj);
}

static Scheme_Object *read_set_bang(Scheme_Object *obj)
{
  Scheme_Set_Bang *sb;

  sb = MALLOC_ONE_TAGGED(Scheme_Set_Bang);
  sb->so.type = scheme_set_bang_type;

  if (!SCHEME_PAIRP(obj)) return NULL;
  sb->set_undef = SCHEME_TRUEP(SCHEME_CAR(obj));

  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;

  sb->var = SCHEME_CAR(obj);
  sb->val = SCHEME_CDR(obj);

  return (Scheme_Object *)sb;
}

static Scheme_Object *write_set_bang(Scheme_Object *obj)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)obj;
  return scheme_make_pair((sb->set_undef ? scheme_true : scheme_false),
                          scheme_make_pair(sb->var, 
                                           scheme_protect_quote(sb->val)));
}

Scheme_Object *write_varref(Scheme_Object *o)
{
  int is_const = (SCHEME_VARREF_FLAGS(o) & 0x1);

  if (is_const) {
    if (SCHEME_PTR1_VAL(o) != SCHEME_PTR2_VAL(o))
      scheme_signal_error("internal error: expected varref halves to be the same");
  }
  
  return scheme_make_pair((is_const ? scheme_true : SCHEME_PTR1_VAL(o)), 
                          SCHEME_PTR2_VAL(o));
}

Scheme_Object *read_varref(Scheme_Object *o)
{
  Scheme_Object *data;

  if (!SCHEME_PAIRP(o)) return NULL;

  data = scheme_alloc_object();
  data->type = scheme_varref_form_type;
  SCHEME_PTR2_VAL(data) = SCHEME_CDR(o);
  if (SAME_OBJ(SCHEME_CAR(o), scheme_true)) {
    SCHEME_VARREF_FLAGS(data) |= 0x1;
    SCHEME_PTR1_VAL(data) = SCHEME_CDR(o);
  } else
    SCHEME_PTR1_VAL(data) = SCHEME_CAR(o);
  
  return data;
}

Scheme_Object *write_apply_values(Scheme_Object *o)
{
  return scheme_make_pair(scheme_protect_quote(SCHEME_PTR1_VAL(o)), 
                          scheme_protect_quote(SCHEME_PTR2_VAL(o)));
}

Scheme_Object *read_apply_values(Scheme_Object *o)
{
  Scheme_Object *data;

  if (!SCHEME_PAIRP(o)) return NULL;

  data = scheme_alloc_object();
  data->type = scheme_apply_values_type;
  SCHEME_PTR1_VAL(data) = SCHEME_CAR(o);
  SCHEME_PTR2_VAL(data) = SCHEME_CDR(o);
  
  return data;
}

Scheme_Object *write_boxenv(Scheme_Object *o)
{
  return scheme_make_pair(SCHEME_PTR1_VAL(o), SCHEME_PTR2_VAL(o));
}

Scheme_Object *read_boxenv(Scheme_Object *o)
{
  Scheme_Object *data;

  if (!SCHEME_PAIRP(o)) return NULL;

  data = scheme_alloc_object();
  data->type = scheme_boxenv_type;
  SCHEME_PTR1_VAL(data) = SCHEME_CAR(o);
  SCHEME_PTR2_VAL(data) = SCHEME_CDR(o);
  
  return data;
}

static Scheme_Object *read_inline_variant(Scheme_Object *obj)
{
  Scheme_Object *data;

  if (!SCHEME_PAIRP(obj)) return NULL;

  data = scheme_make_vector(3, scheme_false);
  data->type = scheme_inline_variant_type;
  SCHEME_VEC_ELS(data)[0] = SCHEME_CAR(obj);
  SCHEME_VEC_ELS(data)[1] = SCHEME_CDR(obj);
  /* third slot is filled when module->accessible table is made */
  
  return data;
}

static Scheme_Object *write_inline_variant(Scheme_Object *obj)
{
  return scheme_make_pair(SCHEME_VEC_ELS(obj)[0],
                          SCHEME_VEC_ELS(obj)[1]);
}


#define BOOL(x) (x ? scheme_true : scheme_false)

static Scheme_Object *write_application(Scheme_Object *obj)
{
  scheme_signal_error("app writer shouldn't be used");
  return NULL;
}

static Scheme_Object *read_application(Scheme_Object *obj)
{
  return NULL;
}

static Scheme_Object *write_sequence(Scheme_Object *obj)
{
  Scheme_Object *l;
  int i;

  i = ((Scheme_Sequence *)obj)->count;

  l = scheme_null;
  for (; i--; ) {
    l = cons(scheme_protect_quote(((Scheme_Sequence *)obj)->array[i]), l);
  }
  
  return l;
}

static Scheme_Object *read_sequence(Scheme_Object *obj)
{
  return scheme_make_sequence_compilation(obj, 1);
}

static Scheme_Object *read_sequence_save_first(Scheme_Object *obj)
{
  return scheme_make_sequence_compilation(obj, -2);
}

static Scheme_Object *read_sequence_splice(Scheme_Object *obj)
{
  obj = scheme_make_sequence_compilation(obj, 1);
  if (!obj) return NULL;

  if (SAME_TYPE(SCHEME_TYPE(obj), scheme_sequence_type))
    obj->type = scheme_splice_sequence_type;
  return obj;
}

static Scheme_Object *write_branch(Scheme_Object *obj)
{
  scheme_signal_error("branch writer shouldn't be used");
  return NULL;
}

static Scheme_Object *read_branch(Scheme_Object *obj)
{
  return NULL;
}

static Scheme_Object *write_with_cont_mark(Scheme_Object *obj)
{
  Scheme_With_Continuation_Mark *wcm;

  wcm = (Scheme_With_Continuation_Mark *)obj;

  return cons(scheme_protect_quote(wcm->key),
	      cons(scheme_protect_quote(wcm->val),
		   scheme_protect_quote(wcm->body)));
}

static Scheme_Object *read_with_cont_mark(Scheme_Object *obj)
{
  Scheme_With_Continuation_Mark *wcm;

  if (!SCHEME_PAIRP(obj) || !SCHEME_PAIRP(SCHEME_CDR(obj)))
    return NULL; /* bad .zo */

  wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  wcm->so.type = scheme_with_cont_mark_type;
  wcm->key = SCHEME_CAR(obj);
  wcm->val = SCHEME_CADR(obj);
  wcm->body = SCHEME_CDR(SCHEME_CDR(obj));

  return (Scheme_Object *)wcm;
}

static Scheme_Object *write_quote_syntax(Scheme_Object *obj)
{
  Scheme_Quote_Syntax *qs = (Scheme_Quote_Syntax *)obj;

  return cons(scheme_make_integer(qs->depth),
	      cons(scheme_make_integer(qs->position),
		   scheme_make_integer(qs->midpoint)));
}

static Scheme_Object *read_quote_syntax(Scheme_Object *obj)
{
  Scheme_Quote_Syntax *qs;
  Scheme_Object *a;
  int c, i, p;
  
  if (!SCHEME_PAIRP(obj)) return NULL;

  a = SCHEME_CAR(obj);
  c = SCHEME_INT_VAL(a);

  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  
  a = SCHEME_CAR(obj);
  i = SCHEME_INT_VAL(a);

  a = SCHEME_CDR(obj);
  p = SCHEME_INT_VAL(a);

  qs = MALLOC_ONE_TAGGED(Scheme_Quote_Syntax);
  qs->so.type = scheme_quote_syntax_type;
  qs->depth = c;
  qs->position = i;
  qs->midpoint = p;  

  return (Scheme_Object *)qs;
}

#define BOOL(x) (x ? scheme_true : scheme_false)

static int not_relative_path(Scheme_Object *p)
{
  Scheme_Object *dir, *rel_p;
  
  dir = scheme_get_param(scheme_current_config(),
                         MZCONFIG_WRITE_DIRECTORY);
  if (SCHEME_TRUEP(dir)) {
    rel_p = scheme_extract_relative_to(p, dir);
    if (SAME_OBJ(rel_p, p))
      return 1;
  }
  
  return 0;
}

static Scheme_Object *write_compiled_closure(Scheme_Object *obj)
{
  Scheme_Closure_Data *data;
  Scheme_Object *name, *l, *code, *ds, *tl_map;
  int svec_size, pos;
  Scheme_Marshal_Tables *mt;

  data = (Scheme_Closure_Data *)obj;

  if (data->name) {
    name = data->name;
    if (SCHEME_VECTORP(name)) {
      /* We can only save marshalable src names, which includes
	 paths, symbols, and strings: */
      Scheme_Object *src;
      src = SCHEME_VEC_ELS(name)[1];
      if ((!SCHEME_PATHP(src)
           /* If MZCONFIG_WRITE_DIRECTORY, drop any non-relative path
              (which might happen due to function inlining, for example)
              to avoid embedding absolute paths in bytecode files: */
           || not_relative_path(src))
	  && !SCHEME_CHAR_STRINGP(src)
	  && !SCHEME_SYMBOLP(src)) {
	/* Just keep the name */
	name = SCHEME_VEC_ELS(name)[0];
      }
    }
  } else {
    name = scheme_null;
  }

  svec_size = data->closure_size;
  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_TYPED_ARGS) {
    svec_size += ((CLOS_TYPE_BITS_PER_ARG * (data->num_params + data->closure_size)) + BITS_PER_MZSHORT - 1) / BITS_PER_MZSHORT;
    {
      int k, mv;
      for (k = data->num_params + data->closure_size; --k; ) {
        mv = scheme_boxmap_get(data->closure_map, k, data->closure_size);
        if (mv > (CLOS_TYPE_TYPE_OFFSET + SCHEME_MAX_LOCAL_TYPE))
          scheme_signal_error("internal error: inconsistent closure/argument type");
      }
    }
  }

  if (SCHEME_RPAIRP(data->code)) {
    /* This can happen if loaded bytecode is printed out and the procedure
       body has never been needed before.
       It's also possible in non-JIT mode if an empty closure is embedded 
       as a 3-D value in compiled code. */
    scheme_delay_load_closure(data);
  }

  /* If the body is simple enough, write it directly.
     Otherwise, create a delay indirection so that the body
     is loaded on demand. */
  code = data->code;
  switch (SCHEME_TYPE(code)) {
  case scheme_toplevel_type:
  case scheme_local_type:
  case scheme_local_unbox_type:
  case scheme_integer_type:
  case scheme_true_type:
  case scheme_false_type:
  case scheme_void_type:
  case scheme_quote_syntax_type:
    ds = code;
    break;
  default:
    ds = NULL;
    break;
  }
  
  if (!ds) {
    mt = scheme_current_thread->current_mt;
    if (!mt->pass) {
      int key;

      pos = mt->cdata_counter;
      if ((!mt->cdata_map || (pos >= 32))
          && !(pos & (pos - 1))) {
        /* Need to grow the array */
        Scheme_Object **a;
        a = MALLOC_N(Scheme_Object *, (pos ? 2 * pos : 32));
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
          if (SAME_OBJ(data->code, ds))
            break;
          if (SAME_TYPE(scheme_quote_compilation_type, SCHEME_TYPE(ds)))
            if (SAME_OBJ(data->code, SCHEME_PTR_VAL(ds)))
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
    
      code = scheme_protect_quote(data->code);
    
      ds = scheme_alloc_small_object();
      ds->type = scheme_delay_syntax_type;
      SCHEME_PTR_VAL(ds) = code;

      MZ_OPT_HASH_KEY(&((Scheme_Small_Object *)ds)->iso) |= 1; /* => hash on ds, not contained data */

      mt->cdata_map[pos] = ds;
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

  l = CONS(scheme_make_svector(svec_size,
                               data->closure_map),
           ds);

  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_TYPED_ARGS)
    l = CONS(scheme_make_integer(data->closure_size),
             l);

  return CONS(scheme_make_integer(SCHEME_CLOSURE_DATA_FLAGS(data) & 0x7F),
	      CONS(scheme_make_integer(data->num_params),
		   CONS(scheme_make_integer(data->max_let_depth),
                        CONS(tl_map,
                             CONS(name,
                                  l)))));
}

static Scheme_Object *read_compiled_closure(Scheme_Object *obj)
{
  Scheme_Closure_Data *data;
  Scheme_Object *v, *tl_map;

#define BAD_CC "bad compiled closure"
#define X_SCHEME_ASSERT(x, y)

  data  = (Scheme_Closure_Data *)scheme_malloc_tagged(sizeof(Scheme_Closure_Data));

  data->iso.so.type = scheme_unclosed_procedure_type;

  if (!SCHEME_PAIRP(obj)) return NULL;
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  SCHEME_CLOSURE_DATA_FLAGS(data) = (short)(SCHEME_INT_VAL(v));

  if (!SCHEME_PAIRP(obj)) return NULL;
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  data->num_params = SCHEME_INT_VAL(v);
  if (data->num_params < 0) return NULL;

  if (!SCHEME_PAIRP(obj)) return NULL;
  data->max_let_depth = SCHEME_INT_VAL(SCHEME_CAR(obj));
  if (data->max_let_depth < 0) return NULL;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return NULL;
  tl_map = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
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
        v2 = (v2 << 16) | v1;
        n[i+1] = v2;
      }
      if ((len == 2) && (!(n[1] & 0x80000000)))
        data->tl_map = (void *)(intptr_t)((n[1] << 1) | 0x1);
      else
        data->tl_map = n;
    } else
      return NULL;
  }

  if (!SCHEME_PAIRP(obj)) return NULL;
  data->name = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  if (SCHEME_NULLP(data->name))
    data->name = NULL;

  if (!SCHEME_PAIRP(obj)) return NULL;
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  /* v is an svector or an integer... */
  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_TYPED_ARGS) {
    if (!SCHEME_INTP(v)) return NULL;
    data->closure_size = SCHEME_INT_VAL(v);
    
    if (!SCHEME_PAIRP(obj)) return NULL;
    v = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
  }

  data->code = obj;

  if (!SAME_TYPE(scheme_svector_type, SCHEME_TYPE(v))) return NULL;

  if (!(SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_TYPED_ARGS))
    data->closure_size = SCHEME_SVEC_LEN(v);
  data->closure_map = SCHEME_SVEC_VEC(v);

  /* If the closure is empty, create the closure now */
  if (!data->closure_size)
    return scheme_make_closure(NULL, (Scheme_Object *)data, 0);
  else
    return (Scheme_Object *)data;
}


static Scheme_Object *write_toplevel(Scheme_Object *obj)
{
  int pos, flags;
  Scheme_Object *pr;

  pos = SCHEME_TOPLEVEL_POS(obj);
  flags = (SCHEME_TOPLEVEL_FLAGS(obj) & SCHEME_TOPLEVEL_FLAGS_MASK);

  pr = (flags
	? scheme_make_pair(scheme_make_integer(pos),
			   scheme_make_integer(flags))
	: scheme_make_integer(pos));

  return scheme_make_pair(scheme_make_integer(SCHEME_TOPLEVEL_DEPTH(obj)),
			  pr);
}

static Scheme_Object *read_toplevel(Scheme_Object *obj)
{
  int pos, depth, flags;

  if (!SCHEME_PAIRP(obj)) return NULL;

  depth = (int)SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (SCHEME_PAIRP(obj)) {
    pos = (int)SCHEME_INT_VAL(SCHEME_CAR(obj));
    flags = SCHEME_INT_VAL(SCHEME_CDR(obj)) & SCHEME_TOPLEVEL_FLAGS_MASK;
  } else {
    pos = (int)SCHEME_INT_VAL(obj);
    flags = 0;
  }

  if (depth < 0) return NULL;
  if (pos < 0) return NULL;

  return scheme_make_toplevel(depth, pos, 1, flags);
}

static Scheme_Object *write_variable(Scheme_Object *obj)
  /* #%kernel references are handled in print.c, instead */
{
  Scheme_Object *sym;
  Scheme_Env *home;
  Scheme_Module *m;
    
  sym = (Scheme_Object *)(SCHEME_VAR_BUCKET(obj))->key;
    
  home = scheme_get_bucket_home((Scheme_Bucket *)obj);
  if (home)
    m = home->module;
  else
    m = NULL;
    
  /* If we get a writeable variable (instead of a module variable),
     it must be a reference to a module referenced directly by its
     a symbolic name (i.e., no path). */
    
  if (m) {
    sym = scheme_make_pair(m->modname, sym);
    if (home->mod_phase)
      sym = scheme_make_pair(scheme_make_integer(home->mod_phase), sym);
  }

  return sym;
}

static Scheme_Object *read_variable(Scheme_Object *obj)
  /* #%kernel references are handled in read.c, instead */
{
  Scheme_Env *env;

  env = scheme_get_env(NULL);

  if (!SCHEME_SYMBOLP(obj)) return NULL;

  return (Scheme_Object *)scheme_global_bucket(obj, env);
}

static Scheme_Object *write_module_variable(Scheme_Object *obj)
{
  scheme_signal_error("module variables should have been handled in print.c");
  return NULL;
}

static Scheme_Object *read_module_variable(Scheme_Object *obj)
{
  scheme_signal_error("module variables should have been handled in read.c");
  return NULL;
}

static Scheme_Object *write_local(Scheme_Object *obj)
{
  return scheme_make_integer(SCHEME_LOCAL_POS(obj));
}

static Scheme_Object *do_read_local(Scheme_Type t, Scheme_Object *obj)
{
  int n, flags;

  if (SCHEME_PAIRP(obj)) {
    flags = (int)SCHEME_INT_VAL(SCHEME_CAR(obj));
    obj = SCHEME_CDR(obj);
  } else
    flags = 0;

  n = (int)SCHEME_INT_VAL(obj);
  if (n < 0) return NULL;

  return scheme_make_local(t, n, flags);
}

static Scheme_Object *read_local(Scheme_Object *obj)
{
  return do_read_local(scheme_local_type, obj);
}

static Scheme_Object *read_local_unbox(Scheme_Object *obj)
{
  return do_read_local(scheme_local_unbox_type, obj);
}

static Scheme_Object *write_resolve_prefix(Scheme_Object *obj)
{
  Resolve_Prefix *rp = (Resolve_Prefix *)obj;
  Scheme_Object *tv, *sv, *ds;
  int i;

  i = rp->num_toplevels;
  tv = scheme_make_vector(i, NULL);
  while (i--) {
    SCHEME_VEC_ELS(tv)[i] = rp->toplevels[i];
  }

  i = rp->num_stxes;
  sv = scheme_make_vector(i, NULL);
  while (i--) {
    if (rp->stxes[i]) {
      if (SCHEME_INTP(rp->stxes[i])) {
        /* Need to foce this object, so we can write it.
           This should only happen if we're writing back 
           code loaded from bytecode. */
        scheme_load_delayed_syntax(rp, i);
      }

      ds = scheme_alloc_small_object();
      ds->type = scheme_delay_syntax_type;
      SCHEME_PTR_VAL(ds) = rp->stxes[i];
    } else
      ds = scheme_false;
    SCHEME_VEC_ELS(sv)[i] = ds;
  }

  tv = scheme_make_pair(scheme_make_integer(rp->num_lifts), 
                        scheme_make_pair(tv, sv));

  return tv;
}

static Scheme_Object *read_resolve_prefix(Scheme_Object *obj)
{
  Resolve_Prefix *rp;
  Scheme_Object *tv, *sv, **a, *stx, *tl;
  intptr_t i;

  if (!SCHEME_PAIRP(obj)) return NULL;

  if (!SCHEME_INTP(SCHEME_CAR(obj))) {
    obj = SCHEME_CDR(obj);
  }

  if (!SCHEME_PAIRP(obj)) return NULL;

  i = SCHEME_INT_VAL(SCHEME_CAR(obj));
  if (i < 0) return NULL;

  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;

  tv = SCHEME_CAR(obj);
  sv = SCHEME_CDR(obj);

  if (!SCHEME_VECTORP(tv)) return NULL;
  if (!SCHEME_VECTORP(sv)) return NULL;

  rp = MALLOC_ONE_TAGGED(Resolve_Prefix);
  rp->so.type = scheme_resolve_prefix_type;
  rp->num_toplevels = (int)SCHEME_VEC_SIZE(tv);
  rp->num_stxes = (int)SCHEME_VEC_SIZE(sv);
  rp->num_lifts = (int)i;

  i = rp->num_toplevels;
  a = MALLOC_N(Scheme_Object *, i);
  while (i--) {
    tl = SCHEME_VEC_ELS(tv)[i];
    if (!SCHEME_FALSEP(tl)
        && !SCHEME_SYMBOLP(tl)
        && !SAME_TYPE(SCHEME_TYPE(tl), scheme_variable_type)
        && !SAME_TYPE(SCHEME_TYPE(tl), scheme_module_variable_type))
      return NULL;
    a[i] = tl;
  }
  rp->toplevels = a;
  
  i = rp->num_stxes;
  a = MALLOC_N(Scheme_Object *, i);
  while (i--) {
    stx = SCHEME_VEC_ELS(sv)[i];
    if (SCHEME_FALSEP(stx)) {
      stx = NULL;
    } else if (SCHEME_RPAIRP(stx)) {
      struct Scheme_Load_Delay *d;
      Scheme_Object *pr;
      d = (struct Scheme_Load_Delay *)SCHEME_CDR(stx);
      stx = SCHEME_CAR(stx);
      pr = rp->delay_info_rpair;
      if (!pr) {
        pr = scheme_make_raw_pair(scheme_make_integer(0), (Scheme_Object *)d);
        rp->delay_info_rpair = pr;
      }
      SCHEME_CAR(pr) = scheme_make_integer(SCHEME_INT_VAL(SCHEME_CAR(pr)) + 1);
    } else {
      if (!SCHEME_STXP(stx)) return NULL;
    }
    a[i] = stx;
  }
  rp->stxes = a;

  return (Scheme_Object *)rp;
}

static Scheme_Object *write_module(Scheme_Object *obj)
{
  Scheme_Module *m = (Scheme_Module *)obj;
  Scheme_Module_Phase_Exports *pt;
  Scheme_Object *l, *v, *phase;
  int i, j, k, count, cnt;

  l = scheme_null;
  cnt = 0;
  if (m->other_requires) {
    for (i = 0; i < m->other_requires->size; i++) {
      if (m->other_requires->vals[i]) {
        cnt++;
        l = scheme_make_pair(m->other_requires->keys[i],
                             scheme_make_pair(m->other_requires->vals[i],
                                              l));
      }
    }
  }
  l = cons(scheme_make_integer(cnt), l);

  l = cons(m->dt_requires, l);
  l = cons(m->tt_requires, l);
  l = cons(m->et_requires, l);
  l = cons(m->requires, l);

  for (j = 0; j < m->num_phases; j++) {
    l = cons(m->bodies[j], l);
  }

  cnt = 0;
  for (k = -3; k < (m->me->other_phases ? m->me->other_phases->size : 0); k++) {
    switch (k) {
    case -3:
      phase = scheme_make_integer(-1);
      pt = m->me->dt;
      break;
    case -2:
      phase = scheme_make_integer(1);
      pt = m->me->et;
      break;
    case -1:
      phase = scheme_make_integer(0);
      pt = m->me->rt;
      break;
    default:
      phase = m->me->other_phases->keys[k];
      pt = (Scheme_Module_Phase_Exports *)m->me->other_phases->vals[k];
    }
    
    if (pt) {
      l = cons(scheme_make_integer(pt->num_provides), l);
      l = cons(scheme_make_integer(pt->num_var_provides), l);

      count = pt->num_provides;

      v = scheme_make_vector(count, NULL);
      for (i = 0; i < count; i++) {
        SCHEME_VEC_ELS(v)[i] = pt->provides[i];
      }
      l = cons(v, l);
  
      v = scheme_make_vector(count, NULL);
      for (i = 0; i < count; i++) {
        SCHEME_VEC_ELS(v)[i] = pt->provide_srcs[i];
      }
      l = cons(v, l);
    
      v = scheme_make_vector(count, NULL);
      for (i = 0; i < count; i++) {
        SCHEME_VEC_ELS(v)[i] = pt->provide_src_names[i];
      }
      l = cons(v, l);

      if (pt->provide_nominal_srcs) {
        v = scheme_make_vector(count, NULL);
        for (i = 0; i < count; i++) {
          SCHEME_VEC_ELS(v)[i] = pt->provide_nominal_srcs[i];
        }
        l = cons(v, l);
      } else {
        l = cons(scheme_false, l);
      }

      if (pt->provide_src_phases) {
        v = scheme_make_vector(count, NULL);
        for (i = 0; i < count; i++) {
          SCHEME_VEC_ELS(v)[i] = scheme_make_integer(pt->provide_src_phases[i]);
        } 
      } else
        v = scheme_false;
      l = cons(v, l);

      if ((SCHEME_INT_VAL(phase) >= 0) && (SCHEME_INT_VAL(phase) < m->num_phases)) {
        Scheme_Module_Export_Info *exp_info = m->exp_infos[SCHEME_INT_VAL(phase)];

        if (exp_info) {
          v = scheme_false;

          if (exp_info->provide_protects) {
            for (i = 0; i < count; i++) {
              if (exp_info->provide_protects[i])
                break;
            }
            if (i < count) {
              v = scheme_make_vector(count, NULL);
              for (i = 0; i < count; i++) {
                SCHEME_VEC_ELS(v)[i] = (exp_info->provide_protects[i] ? scheme_true : scheme_false);
              }
            }
          }
          l = cons(v, l);

          count = exp_info->num_indirect_provides;
          l = cons(scheme_make_integer(count), l);
          v = scheme_make_vector(count, NULL);
          for (i = 0; i < count; i++) {
            SCHEME_VEC_ELS(v)[i] = exp_info->indirect_provides[i];
          }
          l = cons(v, l);
          
          count = exp_info->num_indirect_syntax_provides;
          l = cons(scheme_make_integer(count), l);
          v = scheme_make_vector(count, NULL);
          for (i = 0; i < count; i++) {
            SCHEME_VEC_ELS(v)[i] = exp_info->indirect_syntax_provides[i];
          }
          l = cons(v, l);
        } else
          l = cons(scheme_void, l);
      } else
        l = cons(scheme_void, l);
      
      l = cons(pt->phase_index, l);
      cnt++;
    }
  }
  l = cons(scheme_make_integer(cnt), l);
  l = cons(scheme_make_integer(m->num_phases), l);

  l = cons((Scheme_Object *)m->prefix, l);
  l = cons(m->dummy, l);

  l = cons(scheme_make_integer(m->max_let_depth), l);

  v = m->rn_stx;
  if (!v)
    v = scheme_false;
  else if (SCHEME_PAIRP(v))
    v = scheme_list_to_vector(v);
  l = cons(v, l);

  /* previously recorded "functional?" info: */
  l = cons(scheme_false, l);
  l = cons(scheme_false, l);

  if (m->lang_info)
    l = cons(scheme_protect_quote(m->lang_info), l);
  else
    l = cons(scheme_false, l);

  for (k = 0; k < 2; k++) {
    v = (k ? m->pre_submodules : m->post_submodules);
    if (v && !SCHEME_NULLP(v)) {
      Scheme_Object *l2 = scheme_null;
      while (!SCHEME_NULLP(v)) {
        l2 = scheme_make_pair(write_module(SCHEME_CAR(v)),
                              l2);
        v = SCHEME_CDR(v);
      }
      l = cons(l2, l);
    } else
      l = cons(scheme_null, l);
  }

  l = cons((m->phaseless ? scheme_true : scheme_false), l);

  l = cons(m->me->src_modidx, l);
  l = cons(scheme_resolved_module_path_value(m->modsrc), l);
  l = cons(scheme_resolved_module_path_value(m->modname), l);

  if (m->submodule_path)
    l = cons(m->submodule_path, l);
  else
    l = cons(scheme_null, l);

  return l;
}

static int check_requires_ok(Scheme_Object *l)
{
  Scheme_Object *x;
  while (!SCHEME_NULLP(l)) {
    x = SCHEME_CAR(l);
    if (!SAME_TYPE(SCHEME_TYPE(x), scheme_module_index_type))
      return 0;
    l = SCHEME_CDR(l);
  }
  return 1;
}

#if 0
# define return_NULL() return (printf("%d\n", __LINE__), NULL)
#else
# define return_NULL() return NULL
#endif

static Scheme_Object *read_module(Scheme_Object *obj)
{
  Scheme_Module *m;
  Scheme_Object *ie, *nie, **bodies;
  Scheme_Object *esp, *esn, *esph, *es, *esnom, *e, *nve, *ne, **v;
  Scheme_Module_Exports *me;
  Scheme_Module_Phase_Exports *pt;
  Scheme_Module_Export_Info **exp_infos, *exp_info;
  char *ps;
  int *sps;
  int i, j, count, cnt;

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->so.type = scheme_module_type;
  m->predefined = scheme_starting_up;

  me = scheme_make_module_exports();
  m->me = me;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = SCHEME_CAR(obj);
  m->submodule_path = e;
  if (!scheme_is_list(e)) return_NULL();
  while (!SCHEME_NULLP(e)) {
    if (!SCHEME_SYMBOLP(SCHEME_CAR(e))) return_NULL();
    e = SCHEME_CDR(e);
  }
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = scheme_intern_resolved_module_path(SCHEME_CAR(obj));
  m->modname = e;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = scheme_intern_resolved_module_path(SCHEME_CAR(obj));
  m->modsrc = e;
  m->me->modsrc = e;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  me->src_modidx = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  if (!SAME_TYPE(SCHEME_TYPE(me->src_modidx), scheme_module_index_type))
    return_NULL();
  ((Scheme_Modidx *)me->src_modidx)->resolved = m->modname;
  m->self_modidx = me->src_modidx;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->phaseless = (SCHEME_TRUEP(SCHEME_CAR(obj)) ? scheme_true : NULL);
  obj = SCHEME_CDR(obj);

  for (i = 0; i < 2; i++) {
    if (!SCHEME_PAIRP(obj)) return_NULL();
    e = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
    nve = scheme_null;
    while (!SCHEME_NULLP(e)) {
      if (!SCHEME_PAIRP(e)) return_NULL();
      ne = read_module(SCHEME_CAR(e));
      nve = scheme_make_pair(ne, nve);
      e = SCHEME_CDR(e);
    }
    if (i)
      m->post_submodules = nve;
    else
      m->pre_submodules = nve;
  }

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = SCHEME_CAR(obj);
  if (SCHEME_FALSEP(e))
    e = NULL;
  else if (!(SCHEME_VECTORP(e)
             && (3 == SCHEME_VEC_SIZE(e))
             && scheme_is_module_path(SCHEME_VEC_ELS(e)[0])
             && SCHEME_SYMBOLP(SCHEME_VEC_ELS(e)[1])))
    return_NULL();
  m->lang_info = e;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  /* "functional?" info ignored */
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  /* "functional?" info ignored */
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->rn_stx = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  if (SCHEME_FALSEP(m->rn_stx))
    m->rn_stx = NULL;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->max_let_depth = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->dummy = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->prefix = (Resolve_Prefix *)SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  cnt = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (cnt < 1) return_NULL();
  
  m->num_phases = cnt;
  exp_infos = (Scheme_Module_Export_Info **)scheme_malloc_fail_ok(scheme_malloc, scheme_check_overflow(cnt, sizeof(Scheme_Module_Export_Info *), 0));
  while (cnt--) {
    exp_info = MALLOC_ONE_RT(Scheme_Module_Export_Info);
    SET_REQUIRED_TAG(exp_info->type = scheme_rt_export_info);
    exp_infos[cnt] = exp_info;
  }
  m->exp_infos = exp_infos;
  cnt = m->num_phases;
  
  if (!SCHEME_PAIRP(obj)) return_NULL();
  cnt = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (cnt < 0) return_NULL();
  
  while (cnt--) {
    Scheme_Object *phase;

    if (!SCHEME_PAIRP(obj)) return_NULL();
    phase = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_FALSEP(phase)
        && !SCHEME_INTP(phase)
        && !SCHEME_BIGNUMP(phase))
      return_NULL();

    if (SAME_OBJ(phase, scheme_make_integer(0))) {
      pt = me->rt;
    } else if (SAME_OBJ(phase, scheme_make_integer(1))) {
      pt = me->et;
    } else if (SAME_OBJ(phase, scheme_false)) {
      pt = me->dt;
    } else {
      pt = MALLOC_ONE_RT(Scheme_Module_Phase_Exports);
      pt->so.type = scheme_module_phase_exports_type;
      pt->phase_index = phase;
      if (!me->other_phases) {
        Scheme_Hash_Table *ht;
        ht = scheme_make_hash_table_equal();
        me->other_phases = ht;
      }
      scheme_hash_set(me->other_phases, phase, (Scheme_Object *)pt);      
    }

    if (!SCHEME_PAIRP(obj)) return_NULL();
    ie = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
    if (SCHEME_VOIDP(ie)) {
      /* no exp_infos entry */
      count = -1;
    } else {
      if (!SCHEME_INTP(phase) || (SCHEME_INT_VAL(phase) < 0)
          || (SCHEME_INT_VAL(phase) >= m->num_phases))
        return_NULL();
      exp_info = m->exp_infos[SCHEME_INT_VAL(phase)];
      
      if (!SCHEME_PAIRP(obj)) return_NULL();
      nie = SCHEME_CAR(obj);
      obj = SCHEME_CDR(obj);
  
      count = SCHEME_INT_VAL(nie);
      if (!SCHEME_VECTORP(ie) || (SCHEME_VEC_SIZE(ie) != count)) return_NULL();
      v = MALLOC_N(Scheme_Object *, count);
      for (i = 0; i < count; i++) {
        v[i] = SCHEME_VEC_ELS(ie)[i];
      }
      exp_info->indirect_syntax_provides = v;
      exp_info->num_indirect_syntax_provides = count;

      if (!SCHEME_PAIRP(obj)) return_NULL();
      ie = SCHEME_CAR(obj);
      obj = SCHEME_CDR(obj);

      if (!SCHEME_PAIRP(obj)) return_NULL();
      nie = SCHEME_CAR(obj);
      obj = SCHEME_CDR(obj);
  
      count = SCHEME_INT_VAL(nie);

      if (!SCHEME_VECTORP(ie) || (SCHEME_VEC_SIZE(ie) != count)) return_NULL();
      v = MALLOC_N(Scheme_Object *, count);
      for (i = 0; i < count; i++) {
        v[i] = SCHEME_VEC_ELS(ie)[i];
      }
      exp_info->indirect_provides = v;
      exp_info->num_indirect_provides = count;

      if (!SCHEME_PAIRP(obj)) return_NULL();
      esp = SCHEME_CAR(obj);
      obj = SCHEME_CDR(obj);

      if (SCHEME_FALSEP(esp)) {
        exp_info->provide_protects = NULL;
        count = -1;
      } else {
        if (!SCHEME_VECTORP(esp)) return_NULL();
        count = SCHEME_VEC_SIZE(esp);
        ps = MALLOC_N_ATOMIC(char, count);
        for (i = 0; i < count; i++) {
          ps[i] = SCHEME_TRUEP(SCHEME_VEC_ELS(esp)[i]);
        }
        exp_info->provide_protects = ps;
      }
    }

    if (!SCHEME_PAIRP(obj)) return_NULL();
    esph = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_PAIRP(obj)) return_NULL();
    esnom = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_PAIRP(obj)) return_NULL();
    esn = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_PAIRP(obj)) return_NULL();
    es = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_PAIRP(obj)) return_NULL();
    e = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
  
    if (!SCHEME_PAIRP(obj)) return_NULL();
    nve = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_PAIRP(obj)) return_NULL();
    ne = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if ((count != -1) && (SCHEME_INT_VAL(ne) != count)) return_NULL();
    
    count = SCHEME_INT_VAL(ne);
    pt->num_provides = count;
    pt->num_var_provides = SCHEME_INT_VAL(nve);

    if (!SCHEME_VECTORP(e) || (SCHEME_VEC_SIZE(e) != count)) return_NULL();
    v = MALLOC_N(Scheme_Object *, count);
    for (i = 0; i < count; i++) {
      v[i] = SCHEME_VEC_ELS(e)[i];
    }
    pt->provides = v;

    if (!SCHEME_VECTORP(es) || (SCHEME_VEC_SIZE(es) != count)) return_NULL();
    v = MALLOC_N(Scheme_Object *, count);
    for (i = 0; i < count; i++) {
      v[i] = SCHEME_VEC_ELS(es)[i];
    }
    pt->provide_srcs = v;

    if (!SCHEME_VECTORP(esn) || (SCHEME_VEC_SIZE(esn) != count)) return_NULL();
    v = MALLOC_N(Scheme_Object *, count);
    for (i = 0; i < count; i++) {
      v[i] = SCHEME_VEC_ELS(esn)[i];
    }
    pt->provide_src_names = v;

    if (SCHEME_FALSEP(esnom)) {
      pt->provide_nominal_srcs = NULL;
    } else {
      if (!SCHEME_VECTORP(esnom) || (SCHEME_VEC_SIZE(esnom) != count)) return_NULL();
      v = MALLOC_N(Scheme_Object *, count);
      for (i = 0; i < count; i++) {
        v[i] = SCHEME_VEC_ELS(esnom)[i];
      }
      pt->provide_nominal_srcs = v;
    }

    if (SCHEME_FALSEP(esph))
      sps = NULL;
    else {
      if (!SCHEME_VECTORP(esph) || (SCHEME_VEC_SIZE(esph) != count)) return_NULL();
      sps = MALLOC_N_ATOMIC(int, count);
      for (i = 0; i < count; i++) {
        sps[i] = SCHEME_INT_VAL(SCHEME_VEC_ELS(esph)[i]);
      }
    }
    pt->provide_src_phases = sps;
  }

  count = me->rt->num_provides;

  bodies = MALLOC_N(Scheme_Object*, m->num_phases);
  m->bodies = bodies;
  for (j = m->num_phases; j--; ) {
    if (!SCHEME_PAIRP(obj)) return_NULL();
    e = SCHEME_CAR(obj);
    if (!SCHEME_VECTORP(e)) return_NULL();
    if (j) {
      bodies[j] = e;
      for (i = SCHEME_VEC_SIZE(e); i--; ) {
        e = SCHEME_VEC_ELS(bodies[j])[i];
        if (!SCHEME_VECTORP(e)) return_NULL();
        if (SCHEME_VEC_SIZE(e) != 5) return_NULL();
        /* SCHEME_VEC_ELS(e)[1] should be code */
        if (!SCHEME_INTP(SCHEME_VEC_ELS(e)[2])) return_NULL();
        if (!SAME_TYPE(SCHEME_TYPE(SCHEME_VEC_ELS(e)[3]), scheme_resolve_prefix_type))
          return_NULL();
        if (SCHEME_FALSEP(SCHEME_VEC_ELS(e)[0])) {
          if (SCHEME_FALSEP(SCHEME_VEC_ELS(e)[4])) return_NULL();
        } else {
          e = SCHEME_VEC_ELS(e)[0];
          if (!SCHEME_SYMBOLP(e)) {
            while (SCHEME_PAIRP(e)) {
              if (!SCHEME_SYMBOLP(SCHEME_CAR(e))) return_NULL();
              e = SCHEME_CDR(e);
            }
            if (!SCHEME_NULLP(e)) return_NULL();
          }
        }
      }
    } else {
      bodies[j] = e;
    }
    obj = SCHEME_CDR(obj);
  }

  if (!SCHEME_PAIRP(obj)) return_NULL();
  if (scheme_proper_list_length(SCHEME_CAR(obj)) < 0) return_NULL();
  e = scheme_copy_list(SCHEME_CAR(obj));
  m->requires = e;
  if (!check_requires_ok(e)) return_NULL();
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  if (scheme_proper_list_length(SCHEME_CAR(obj)) < 0) return_NULL();
  e = scheme_copy_list(SCHEME_CAR(obj));
  m->et_requires = e;
  if (!check_requires_ok(e)) return_NULL();
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  if (scheme_proper_list_length(SCHEME_CAR(obj)) < 0) return_NULL();
  e = scheme_copy_list(SCHEME_CAR(obj));
  m->tt_requires = e;
  if (!check_requires_ok(e)) return_NULL();
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  if (scheme_proper_list_length(SCHEME_CAR(obj)) < 0) return_NULL();
  e = scheme_copy_list(SCHEME_CAR(obj));
  m->dt_requires = e;
  if (!check_requires_ok(e)) return_NULL();
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  cnt = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  while (cnt--) {
    Scheme_Object *phase;

    if (!SCHEME_PAIRP(obj)) return_NULL();
    phase = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_INTP(phase)
        && !SCHEME_BIGNUMP(phase))
      return_NULL();

    if (SAME_OBJ(phase, scheme_make_integer(0))
        || SAME_OBJ(phase, scheme_make_integer(1))
        || SAME_OBJ(phase, scheme_make_integer(-1)))
      return_NULL();

    if (!SCHEME_PAIRP(obj)) return_NULL();
    e = scheme_copy_list(SCHEME_CAR(obj));
    if (!check_requires_ok(e)) return_NULL();

    if (!m->other_requires) {
      Scheme_Hash_Table *ht;
      ht = scheme_make_hash_table_equal();
      m->other_requires = ht;
    }
    scheme_hash_set(m->other_requires, phase, e);
    
    obj = SCHEME_CDR(obj);
  }
  
  return (Scheme_Object *)m;
}

Scheme_Object *write_top_level_require(Scheme_Object *o)
{
  return scheme_make_pair(SCHEME_PTR1_VAL(o), SCHEME_PTR2_VAL(o));
}

Scheme_Object *read_top_level_require(Scheme_Object *o)
{
  Scheme_Object *data;

  if (!SCHEME_PAIRP(o)) return NULL;

  data = scheme_alloc_object();
  data->type = scheme_require_form_type;
  SCHEME_PTR1_VAL(data) = SCHEME_CAR(o);
  SCHEME_PTR2_VAL(data) = SCHEME_CDR(o);
  
  return data;
}
