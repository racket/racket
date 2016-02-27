/*
  Racket
  Copyright (c) 2004-2016 PLT Design Inc.
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
*/

#include "schpriv.h"
#include "schmach.h"

#define PROP_USE_HT_COUNT 5

/* globals */
READ_ONLY Scheme_Object *scheme_arity_at_least;
READ_ONLY Scheme_Object *scheme_date;
READ_ONLY Scheme_Object *scheme_make_arity_at_least;
READ_ONLY Scheme_Object *scheme_source_property;
READ_ONLY Scheme_Object *scheme_module_path_property;
READ_ONLY Scheme_Object *scheme_input_port_property;
READ_ONLY Scheme_Object *scheme_output_port_property;
READ_ONLY Scheme_Object *scheme_cpointer_property;
READ_ONLY Scheme_Object *scheme_equal_property;
READ_ONLY Scheme_Object *scheme_no_arity_property;
READ_ONLY Scheme_Object *scheme_impersonator_of_property;
READ_ONLY Scheme_Object *scheme_make_struct_type_proc;
READ_ONLY Scheme_Object *scheme_make_struct_field_accessor_proc;
READ_ONLY Scheme_Object *scheme_make_struct_field_mutator_proc;
READ_ONLY Scheme_Object *scheme_struct_type_p_proc;
READ_ONLY Scheme_Object *scheme_current_inspector_proc;
READ_ONLY Scheme_Object *scheme_make_inspector_proc;
READ_ONLY Scheme_Object *scheme_recur_symbol;
READ_ONLY Scheme_Object *scheme_display_symbol;
READ_ONLY Scheme_Object *scheme_write_special_symbol;
READ_ONLY Scheme_Object *scheme_app_mark_impersonator_property;
READ_ONLY Scheme_Object *scheme_liberal_def_ctx_type;;
READ_ONLY Scheme_Object *scheme_object_name_property;
READ_ONLY Scheme_Object *scheme_struct_to_vector_proc;

READ_ONLY static Scheme_Object *location_struct;
READ_ONLY static Scheme_Object *write_property;
READ_ONLY static Scheme_Object *print_attribute_property;
READ_ONLY static Scheme_Object *evt_property;
READ_ONLY static Scheme_Object *proc_property;
READ_ONLY static Scheme_Object *method_property;
READ_ONLY static Scheme_Object *rename_transformer_property;
READ_ONLY static Scheme_Object *set_transformer_property;
READ_ONLY static Scheme_Object *expansion_contexts_property;
READ_ONLY static Scheme_Object *not_free_id_symbol;
READ_ONLY static Scheme_Object *scheme_checked_proc_property;
READ_ONLY static Scheme_Object *struct_info_proc;
ROSYM static Scheme_Object *ellipses_symbol;
ROSYM static Scheme_Object *prefab_symbol;

/* locals */

typedef enum {
  SCHEME_CONSTR = 1, 
  SCHEME_PRED, 
  SCHEME_GETTER, 
  SCHEME_SETTER,
  SCHEME_GEN_GETTER, 
  SCHEME_GEN_SETTER
} Scheme_ProcT;

typedef struct {
  Scheme_Object so;
  Scheme_Object *evt;
  Scheme_Object *wrapper;
} Wrapped_Evt;

typedef struct {
  Scheme_Object so;
  Scheme_Object *maker;
} Nack_Guard_Evt;

typedef struct {
  Scheme_Object so;
  int done;
  Syncing *syncing;
  Scheme_Object *wrapper;
  Scheme_Object *orig;
} Active_Replace_Evt;

static Scheme_Object *make_inspector(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_sibling_inspector(int argc, Scheme_Object *argv[]);
static Scheme_Object *inspector_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_inspector(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_code_inspector(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_struct_type_property(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_struct_type_property_from_c(int argc, Scheme_Object *argv[],
                                                       Scheme_Object **predout, Scheme_Object **accessout,
                                                       Scheme_Type type);
static Scheme_Object *struct_type_property_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *chaperone_property_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *check_evt_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *check_object_name_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *check_equal_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *check_impersonator_of_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *check_write_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *check_print_attribute_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *check_input_port_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *check_output_port_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *check_cpointer_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *check_rename_transformer_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *check_set_transformer_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *check_expansion_contexts_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *check_checked_proc_property_value_ok(int argc, Scheme_Object *argv[]);

static Scheme_Object *unary_acc(int argc, Scheme_Object **argv, Scheme_Object *self);

static Scheme_Object *make_struct_type(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_struct_field_accessor(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_struct_field_mutator(int argc, Scheme_Object *argv[]);

static Scheme_Object *nack_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *handle_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *replace_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *chaperone_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *handle_evt_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *struct_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_type_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *proc_struct_type_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *struct_info(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_type_info(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_type_pred(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_type_constr(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_to_vector(int argc, Scheme_Object *argv[]);
static Scheme_Object *prefab_struct_key(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_prefab_struct(int argc, Scheme_Object *argv[]);
static Scheme_Object *prefab_key_struct_type(int argc, Scheme_Object *argv[]);
static Scheme_Object *is_prefab_key(int argc, Scheme_Object *argv[]);

static Scheme_Object *struct_setter_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_getter_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_pred_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_constr_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_prop_getter_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *chaperone_prop_getter_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_struct_proc(Scheme_Struct_Type *struct_type, char *func_name, 
				       Scheme_ProcT proc_type, int field_num);

static Scheme_Object *make_name(const char *pre, const char *tn, int tnl, const char *post1, 
				const char *fn, int fnl, const char *post2, int sym);

static void get_struct_type_info(int argc, Scheme_Object *argv[], Scheme_Object **a, int always);


static int evt_struct_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo);
static int is_evt_struct(Scheme_Object *);

static int wrapped_evt_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo);
static int nack_guard_evt_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo);
static int nack_evt_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo);
static int poll_evt_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo);
static int replace_evt_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo);
static int active_replace_evt_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo);
static void active_replace_evt_needs_wakeup(Scheme_Object *s, void *fds);

static int chaperone_evt_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo);
static int is_chaperone_evt(Scheme_Object *o);

Scheme_Object *make_special_comment(int argc, Scheme_Object **argv);
Scheme_Object *special_comment_value(int argc, Scheme_Object **argv);
Scheme_Object *special_comment_p(int argc, Scheme_Object **argv);

static Scheme_Object *check_arity_at_least_fields(int argc, Scheme_Object **argv);
static Scheme_Object *check_date_fields(int argc, Scheme_Object **argv);
static Scheme_Object *check_date_star_fields(int argc, Scheme_Object **argv);
static Scheme_Object *check_location_fields(int argc, Scheme_Object **argv);

static Scheme_Object *check_exn_source_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *exn_source_p(int argc, Scheme_Object **argv);
static Scheme_Object *exn_source_get(int argc, Scheme_Object **argv);

static Scheme_Object *check_exn_module_path_property_value_ok(int argc, Scheme_Object *argv[]);
static Scheme_Object *exn_module_path_p(int argc, Scheme_Object **argv);
static Scheme_Object *exn_module_path_get(int argc, Scheme_Object **argv);

static Scheme_Object *procedure_extract_target(int argc, Scheme_Object **argv);
static Scheme_Struct_Type *hash_prefab(Scheme_Struct_Type *type);

static Scheme_Object *chaperone_struct(int argc, Scheme_Object **argv);
static Scheme_Object *impersonate_struct(int argc, Scheme_Object **argv);
static Scheme_Object *chaperone_struct_type(int argc, Scheme_Object **argv);
static Scheme_Object *make_chaperone_property(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_chaperone_property_from_c(Scheme_Object *name);

static Scheme_Object *is_liberal_def_ctx(int argc, Scheme_Object **argv, Scheme_Object *self);

/* This needs to be even, so that structure chaperones are
   distingiushed from procedure chaperones: */
#define PRE_REDIRECTS 2

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

THREAD_LOCAL_DECL(static Scheme_Bucket_Table *prefab_table);
static Scheme_Object *make_prefab_key(Scheme_Struct_Type *type);

#define cons scheme_make_pair
#define icons scheme_make_pair
#define _intern scheme_intern_symbol

#define BUILTIN_STRUCT_FLAGS (SCHEME_STRUCT_NO_SET | SCHEME_STRUCT_EXPTIME | SCHEME_STRUCT_NO_MAKE_PREFIX)

#define TYPE_NAME(base, blen, sym) make_name("struct:", base, blen, "", NULL, 0, "", sym)
#define CSTR_NAME(base, blen, sym) make_name("", base, blen, "", NULL, 0, "", sym)
#define CSTR_MAKE_NAME(base, blen, sym) make_name("make-", base, blen, "", NULL, 0, "", sym)
#define PRED_NAME(base, blen, sym) make_name("", base, blen, "?", NULL, 0, "", sym)
#define GET_NAME(base, blen, field, flen, sym) make_name("", base, blen, "-", field, flen, "", sym)
#define SET_NAME(base, blen, field, flen, sym) make_name("set-", base, blen, "-", field, flen, "!", sym)
#define GENGET_NAME(base, blen, sym) make_name("", base, blen, "-ref", NULL, 0, "", sym)
#define GENSET_NAME(base, blen, sym) make_name("", base, blen, "-set!", NULL, 0, "", sym)
#define EXPTIME_NAME(base, blen, sym) make_name("", base, blen, "", NULL, 0, "", sym)

#define TYPE_NAME_STR(sym) (char *)make_name("struct:", (char *)sym, -1, "", NULL, 0, "", 0)

#define mzNUM_ST_INFO 8

static char *pred_name_string(Scheme_Object *sym)
{
  return (char *)PRED_NAME(scheme_symbol_val(sym), SCHEME_SYM_LEN(sym), 0);
}

void
scheme_init_struct (Scheme_Env *env)
{
  Scheme_Object **as_names;
  Scheme_Object **as_values;
  int as_count;
  Scheme_Object **ts_names;
  Scheme_Object **ts_values;
  int ts_count;
  Scheme_Object **loc_names;
  Scheme_Object **loc_values;
  int loc_count;
  int i;
  Scheme_Object *guard;

  READ_ONLY static const char *arity_fields[1] = { "value" };
  READ_ONLY static const char *date_fields[10] = { "second", "minute", "hour",
                                                   "day", "month", "year",
                                                   "week-day", "year-day", "dst?", "time-zone-offset" };
  READ_ONLY static const char *date_star_fields[2] = { "nanosecond", "time-zone-name" };
  READ_ONLY static const char *location_fields[10] = { "source", "line", "column", "position", "span" };
  
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  /* Add arity structure */
  REGISTER_SO(scheme_arity_at_least);
  REGISTER_SO(scheme_make_arity_at_least);
  scheme_arity_at_least = scheme_make_struct_type_from_string("arity-at-least", NULL, 1, NULL, 
                                                              scheme_make_prim(check_arity_at_least_fields), 1);
  as_names = scheme_make_struct_names_from_array("arity-at-least",
						 1, arity_fields,
						 BUILTIN_STRUCT_FLAGS, 
						 &as_count);
  as_values = scheme_make_struct_values(scheme_arity_at_least, as_names, as_count, 
					BUILTIN_STRUCT_FLAGS);
  scheme_make_arity_at_least = as_values[1];
  for (i = 0; i < as_count - 1; i++) {
    scheme_add_global_constant(scheme_symbol_val(as_names[i]), as_values[i],
			       env);
  }

  /* Add date structure: */
  REGISTER_SO(scheme_date);
  scheme_date = scheme_make_struct_type_from_string("date", NULL, 10, NULL,
                                                    scheme_make_prim(check_date_fields), 1);
  
  ts_names = scheme_make_struct_names_from_array("date",
						 10, date_fields,
						 BUILTIN_STRUCT_FLAGS, &ts_count);

  ts_values = scheme_make_struct_values(scheme_date, ts_names, ts_count, 
					BUILTIN_STRUCT_FLAGS);
  for (i = 0; i < ts_count - 1; i++) {
    scheme_add_global_constant(scheme_symbol_val(ts_names[i]), ts_values[i], 
			       env);
  }

  scheme_date = scheme_make_struct_type_from_string("date*", scheme_date, 2, NULL,
                                                    scheme_make_prim(check_date_star_fields), 1);
  
  ts_names = scheme_make_struct_names_from_array("date*",
						 2, date_star_fields,
						 BUILTIN_STRUCT_FLAGS, &ts_count);

  ts_values = scheme_make_struct_values(scheme_date, ts_names, ts_count, 
					BUILTIN_STRUCT_FLAGS);
  
  for (i = 0; i < ts_count - 1; i++) {
    scheme_add_global_constant(scheme_symbol_val(ts_names[i]), ts_values[i], 
			       env);
  }
  

  /* Add location structure: */
  REGISTER_SO(location_struct);
  location_struct = scheme_make_struct_type_from_string("srcloc", NULL, 5, NULL, 
                                                        scheme_make_prim(check_location_fields), 1);
  
  loc_names = scheme_make_struct_names_from_array("srcloc",
						  5, location_fields,
						  BUILTIN_STRUCT_FLAGS, &loc_count);
  
  loc_values = scheme_make_struct_values(location_struct, loc_names, loc_count, 
					 BUILTIN_STRUCT_FLAGS);
  for (i = 0; i < loc_count - 1; i++) {
    scheme_add_global_constant(scheme_symbol_val(loc_names[i]), loc_values[i], 
			       env);
  }

  REGISTER_SO(write_property);
  {
    Scheme_Object *a[2], *pred, *access;
    guard = scheme_make_prim_w_arity(check_write_property_value_ok,
				     "guard-for-prop:custom-write",
				     2, 2);

    a[0] = scheme_intern_symbol("custom-write");
    a[1] = guard;
    write_property = make_struct_type_property_from_c(2, a, &pred, &access,
                                                      scheme_struct_property_type);
    scheme_add_global_constant("prop:custom-write", write_property, env);
    scheme_add_global_constant("custom-write?", pred, env);

    a[0] = access;
    scheme_add_global_constant("custom-write-accessor", 
                               scheme_make_prim_closure_w_arity(unary_acc, 1, a, 
                                                                "custom-write-accessor",
                                                                1, 1),
                               env);
  }

  REGISTER_SO(print_attribute_property);
  {
    Scheme_Object *a[2], *pred, *access;
    guard = scheme_make_prim_w_arity(check_print_attribute_property_value_ok,
				     "guard-for-prop:custom-print-quotable",
				     2, 2);

    a[0] = scheme_intern_symbol("custom-print-quotable");
    a[1] = guard;
    print_attribute_property = make_struct_type_property_from_c(2, a, &pred, &access,
                                                                scheme_struct_property_type);
    scheme_add_global_constant("prop:custom-print-quotable", print_attribute_property, env);
    scheme_add_global_constant("custom-print-quotable?", pred, env);

    a[0] = access;
    scheme_add_global_constant("custom-print-quotable-accessor", 
                               scheme_make_prim_closure_w_arity(unary_acc, 1, a, 
                                                                "custom-print-quotable-accessor",
                                                                1, 1),
                               env);
  }
  
  REGISTER_SO(evt_property);
  {
    guard = scheme_make_prim_w_arity(check_evt_property_value_ok,
				     "guard-for-prop:evt",
				     2, 2);
    evt_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("evt"),
                                                            guard);
    scheme_add_global_constant("prop:evt", evt_property, env);
  }

  {
    REGISTER_SO(proc_property);
    proc_property = scheme_make_struct_type_property(scheme_intern_symbol("procedure"));
    scheme_add_global_constant("prop:procedure", proc_property, env);
  }

  {
    REGISTER_SO(scheme_object_name_property);
    guard = scheme_make_prim_w_arity(check_object_name_property_value_ok,
				     "guard-for-prop:object-name",
				     2, 2);
    scheme_object_name_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("object-name"),
                                                                           guard);
    scheme_add_global_constant("prop:object-name", scheme_object_name_property, env);
  }

  {
    REGISTER_SO(scheme_no_arity_property);
    scheme_no_arity_property = scheme_make_struct_type_property(scheme_intern_symbol("incomplete-arity"));
    scheme_add_global_constant("prop:incomplete-arity", scheme_no_arity_property, env);
  }

  {
    guard = scheme_make_prim_w_arity(check_equal_property_value_ok,
				     "guard-for-prop:equal+hash",
				     2, 2);
    REGISTER_SO(scheme_equal_property);
    scheme_equal_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("equal+hash"),
                                                                     guard);
    scheme_add_global_constant("prop:equal+hash", scheme_equal_property, env);
  }

  {
    guard = scheme_make_prim_w_arity(check_impersonator_of_property_value_ok,
				     "guard-for-prop:impersonator-of",
				     2, 2);
    REGISTER_SO(scheme_impersonator_of_property);
    scheme_impersonator_of_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("impersonator-of"),
                                                                               guard);
    scheme_add_global_constant("prop:impersonator-of", scheme_impersonator_of_property, env);
  }

  {
    REGISTER_SO(scheme_input_port_property);
    REGISTER_SO(scheme_output_port_property);

    guard = scheme_make_prim_w_arity(check_input_port_property_value_ok,
				     "guard-for-prop:input-port",
				     2, 2);
    scheme_input_port_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("input-port"),
                                                                          guard);
    
    guard = scheme_make_prim_w_arity(check_output_port_property_value_ok,
				     "guard-for-prop:output-port",
				     2, 2);
    scheme_output_port_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("output-port"),
                                                                           guard);
    
    scheme_add_global_constant("prop:input-port", scheme_input_port_property, env);
    scheme_add_global_constant("prop:output-port", scheme_output_port_property, env);
  }

  {
    REGISTER_SO(scheme_cpointer_property);

    guard = scheme_make_prim_w_arity(check_cpointer_property_value_ok,
				     "guard-for-prop:cpointer",
				     2, 2);
    scheme_cpointer_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("cpointer"),
                                                                        guard);
  }

  {
    REGISTER_SO(rename_transformer_property);

    guard = scheme_make_prim_w_arity(check_rename_transformer_property_value_ok,
				     "guard-for-prop:rename-transformer",
				     2, 2);
    rename_transformer_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("rename-transformer"),
                                                                           guard);
    
    scheme_add_global_constant("prop:rename-transformer", rename_transformer_property, env);
  }

  {
    REGISTER_SO(set_transformer_property);

    guard = scheme_make_prim_w_arity(check_set_transformer_property_value_ok,
				     "guard-for-prop:set!-transformer",
				     2, 2);
    set_transformer_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("set!-transformer"),
                                                                        guard);
    
    scheme_add_global_constant("prop:set!-transformer", set_transformer_property, env);
  }

  {
    REGISTER_SO(expansion_contexts_property);

    guard = scheme_make_prim_w_arity(check_expansion_contexts_property_value_ok,
				     "guard-for-prop:expansion-contexts",
				     2, 2);
    expansion_contexts_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("expansion-contexts"),
                                                                           guard);
    
    scheme_add_global_constant("prop:expansion-contexts", expansion_contexts_property, env);
  }


  {
    guard = scheme_make_prim_w_arity(check_checked_proc_property_value_ok,
				     "guard-for-prop:checked-procedure",
				     2, 2);
    REGISTER_SO(scheme_checked_proc_property);
    scheme_checked_proc_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("checked-procedure"),
                                                                            guard);
    scheme_add_global_constant("prop:checked-procedure", scheme_checked_proc_property, env);
  }

  REGISTER_SO(scheme_liberal_def_ctx_type);
  {
    Scheme_Object *a[1], *prop, *pred, *access;
    
    a[0] = scheme_intern_symbol("liberal-define-context");
    prop = make_struct_type_property_from_c(1, a, &pred, &access,
                                            scheme_struct_property_type);
    scheme_add_global_constant("prop:liberal-define-context", prop, env);

    a[0] = prop;
    scheme_add_global_constant("liberal-define-context?", 
                               scheme_make_prim_closure_w_arity(is_liberal_def_ctx, 1, a, 
                                                                "liberal-define-context?",
                                                                1, 1),
                               env);

    scheme_liberal_def_ctx_type = scheme_make_struct_type_from_string("liberal-define-context", NULL, 0, 
                                                                      cons(cons(prop, scheme_true), scheme_null),
                                                                      NULL, 1);
  }

  {
    REGISTER_SO(method_property);
    method_property = scheme_make_struct_type_property(scheme_intern_symbol("method-arity-error"));
    scheme_add_global_constant("prop:method-arity-error", method_property, env);
  }

  REGISTER_SO(not_free_id_symbol);
  not_free_id_symbol = scheme_intern_symbol("not-free-identifier=?");

  REGISTER_SO(scheme_recur_symbol);
  REGISTER_SO(scheme_display_symbol);
  REGISTER_SO(scheme_write_special_symbol);
  scheme_recur_symbol = scheme_intern_symbol("recur");
  scheme_display_symbol = scheme_intern_symbol("display");
  scheme_write_special_symbol = scheme_intern_symbol("write-special");

  /*** basic interface ****/

  REGISTER_SO(scheme_make_struct_type_proc);
  scheme_make_struct_type_proc = scheme_make_prim_w_arity2(make_struct_type,
                                                           "make-struct-type",
                                                           4, 11,
                                                           5, 5);

  scheme_add_global_constant("make-struct-type", 
                             scheme_make_struct_type_proc,
                             env);

  scheme_add_global_constant("make-struct-type-property", 
                             scheme_make_prim_w_arity2(make_struct_type_property,
                                                       "make-struct-type-property",
                                                       1, 4,
                                                       3, 3),
                             env);

  REGISTER_SO(scheme_make_struct_field_accessor_proc);
  scheme_make_struct_field_accessor_proc = scheme_make_prim_w_arity(make_struct_field_accessor,
                                                                    "make-struct-field-accessor",
                                                                    2, 3);
  scheme_add_global_constant("make-struct-field-accessor",
                             scheme_make_struct_field_accessor_proc,
			     env);

  REGISTER_SO(scheme_make_struct_field_mutator_proc);
  scheme_make_struct_field_mutator_proc = scheme_make_prim_w_arity(make_struct_field_mutator,
                                                                   "make-struct-field-mutator",
                                                                   2, 3);
  scheme_add_global_constant("make-struct-field-mutator",
			     scheme_make_struct_field_mutator_proc,
			     env);

  scheme_add_global_constant("wrap-evt",
			     scheme_make_prim_w_arity(scheme_wrap_evt,
						      "wrap-evt",
						      2, 2),
			     env);
  scheme_add_global_constant("handle-evt",
			     scheme_make_prim_w_arity(handle_evt,
						      "handle-evt",
						      2, 2),
			     env);
  scheme_add_global_constant("replace-evt",
			     scheme_make_prim_w_arity(replace_evt,
						      "replace-evt",
						      2, 2),
			     env);
  scheme_add_global_constant("chaperone-evt",
			     scheme_make_prim_w_arity(chaperone_evt,
						      "chaperone-evt",
						      2, -1),
			     env);
  scheme_add_global_constant("nack-guard-evt",
			     scheme_make_prim_w_arity(nack_evt,
						      "nack-guard-evt",
						      1, 1),
			     env);
  scheme_add_global_constant("poll-guard-evt",
			     scheme_make_prim_w_arity(scheme_poll_evt,
						      "poll-guard-evt",
						      1, 1),
			     env);
  scheme_add_global_constant("handle-evt?",
			     scheme_make_folding_prim(handle_evt_p,
						      "handle-evt?",
						      1, 1, 1),
			     env);

  scheme_add_global_constant("struct?",
			     scheme_make_folding_prim(struct_p,
						      "struct?",
						      1, 1, 1),
			     env);

  REGISTER_SO(scheme_struct_type_p_proc);
  scheme_struct_type_p_proc = scheme_make_folding_prim(struct_type_p,
                                                       "struct-type?",
                                                       1, 1, 1);
  scheme_add_global_constant("struct-type?", scheme_struct_type_p_proc, env);

  scheme_add_global_constant("struct-type-property?",
			     scheme_make_folding_prim(struct_type_property_p,
                                                      "struct-type-property?",
                                                      1, 1, 1),
                             env);
  scheme_add_global_constant("procedure-struct-type?",
			     scheme_make_folding_prim(proc_struct_type_p,
                                                      "procedure-struct-type?",
                                                      1, 1, 1),
                             env);
  scheme_add_global_constant("procedure-extract-target",
                             scheme_make_prim_w_arity(procedure_extract_target,
                                                      "procedure-extract-target",
                                                      1, 1),
                             env);

  /*** Debugging ****/

  REGISTER_SO(struct_info_proc);
  struct_info_proc = scheme_make_prim_w_arity2(struct_info,
                                               "struct-info",
                                               1, 1,
                                               2, 2);
  scheme_add_global_constant("struct-info", struct_info_proc, env);
  scheme_add_global_constant("struct-type-info",
			     scheme_make_prim_w_arity2(struct_type_info,
						       "struct-type-info",
						       1, 1,
						       mzNUM_ST_INFO, mzNUM_ST_INFO),
			     env);
  scheme_add_global_constant("struct-type-make-predicate",
			     scheme_make_prim_w_arity(struct_type_pred,
						      "struct-type-make-predicate",
						      1, 1),
			     env);
  scheme_add_global_constant("struct-type-make-constructor",
			     scheme_make_prim_w_arity(struct_type_constr,
						      "struct-type-make-constructor",
						      1, 2),
			     env);

  REGISTER_SO(scheme_struct_to_vector_proc);
  scheme_struct_to_vector_proc = scheme_make_noncm_prim(struct_to_vector,
                                                        "struct->vector",
                                                        1, 2);
  scheme_add_global_constant("struct->vector", scheme_struct_to_vector_proc, env);

  scheme_add_global_constant("prefab-struct-key",
			     scheme_make_immed_prim(prefab_struct_key,
                                                    "prefab-struct-key",
                                                    1, 1),
			     env);
  scheme_add_global_constant("make-prefab-struct",
			     scheme_make_prim_w_arity(make_prefab_struct,
						      "make-prefab-struct",
						      1, -1),
			     env);
  scheme_add_global_constant("prefab-key->struct-type",
			     scheme_make_prim_w_arity(prefab_key_struct_type,
						      "prefab-key->struct-type",
						      2, 2),
			     env);
  scheme_add_global_constant("prefab-key?",
			     scheme_make_folding_prim(is_prefab_key, "prefab-key?", 
                                                      1, 1, 1),
			     env);

  /*** Predicates ****/

  scheme_add_global_constant("struct-mutator-procedure?",
			     scheme_make_immed_prim(struct_setter_p,
                                                    "struct-mutator-procedure?",
                                                    1, 1),
                             env);
  scheme_add_global_constant("struct-accessor-procedure?",
			     scheme_make_immed_prim(struct_getter_p,
                                                    "struct-accessor-procedure?",
                                                    1, 1),
                             env);
  scheme_add_global_constant("struct-predicate-procedure?",
			     scheme_make_immed_prim(struct_pred_p,
                                                    "struct-predicate-procedure?",
                                                    1, 1),
			     env);
  scheme_add_global_constant("struct-constructor-procedure?",
			     scheme_make_immed_prim(struct_constr_p,
                                                    "struct-constructor-procedure?",
                                                    1, 1),
			     env);
  scheme_add_global_constant("struct-type-property-accessor-procedure?",
			     scheme_make_immed_prim(struct_prop_getter_p,
                                                    "struct-type-property-accessor-procedure?",
                                                    1, 1),
			     env);
  scheme_add_global_constant("impersonator-property-accessor-procedure?",
			     scheme_make_immed_prim(chaperone_prop_getter_p,
                                                    "impersonator-property-accessor-procedure?",
                                                    1, 1),
			     env);
  
  /*** Inspectors ****/

  REGISTER_SO(scheme_make_inspector_proc);
  scheme_make_inspector_proc = scheme_make_immed_prim(make_inspector,
                                                      "make-inspector",
                                                      0, 1);
  scheme_add_global_constant("make-inspector", scheme_make_inspector_proc, env);
  scheme_add_global_constant("make-sibling-inspector",
			     scheme_make_immed_prim(make_sibling_inspector,
                                                    "make-sibling-inspector",
                                                    0, 1),
			     env);
  scheme_add_global_constant("inspector?",
			     scheme_make_folding_prim(inspector_p,
						      "inspector?",
						      1, 1, 1),
			     env);
  
  REGISTER_SO(scheme_current_inspector_proc);
  scheme_current_inspector_proc = scheme_register_parameter(current_inspector,
                                                            "current-inspector",
                                                            MZCONFIG_INSPECTOR);
  scheme_add_global_constant("current-inspector", 
			     scheme_current_inspector_proc,
			     env);
  scheme_add_global_constant("current-code-inspector", 
			     scheme_register_parameter(current_code_inspector,
						       "current-code-inspector",
						       MZCONFIG_CODE_INSPECTOR),
			     env);


  scheme_add_global_constant("make-special-comment", 
			     scheme_make_immed_prim(make_special_comment,
                                                    "make-special-comment",
                                                    1, 1),
			     env);
  scheme_add_global_constant("special-comment-value", 
			     scheme_make_immed_prim(special_comment_value,
                                                    "special-comment-value",
                                                    1, 1),
			     env);
  scheme_add_global_constant("special-comment?", 
			     scheme_make_folding_prim(special_comment_p,
						      "special-comment?",
						      1, 1, 1),
			     env);

  REGISTER_SO(ellipses_symbol);
  ellipses_symbol = scheme_intern_symbol("...");

  REGISTER_SO(prefab_symbol);
  prefab_symbol = scheme_intern_symbol("prefab");


  REGISTER_SO(scheme_source_property);
  {
    guard = scheme_make_prim_w_arity(check_exn_source_property_value_ok,
				     "guard-for-prop:exn:srclocs",
				     2, 2);
    scheme_source_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("prop:exn:srclocs"),
								      guard);
  }
  scheme_add_global_constant("prop:exn:srclocs", scheme_source_property, env);
  scheme_add_global_constant("exn:srclocs?", 
			     scheme_make_folding_prim(exn_source_p,
						      "exn:srclocs?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("exn:srclocs-accessor", 
			     scheme_make_folding_prim(exn_source_get,
						      "exn:srclocs-accessor",
						      1, 1, 1),
			     env);

  REGISTER_SO(scheme_module_path_property);
  {
    guard = scheme_make_prim_w_arity(check_exn_module_path_property_value_ok,
				     "guard-for-prop:exn:srclocs",
				     2, 2);
    scheme_module_path_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("prop:exn:missing-module"),
                                                                           guard);
  }
  scheme_add_global_constant("prop:exn:missing-module", scheme_module_path_property, env);
  scheme_add_global_constant("exn:missing-module?",
			     scheme_make_folding_prim(exn_module_path_p,
						      "exn:missing-module?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("exn:missing-module-accessor", 
			     scheme_make_folding_prim(exn_module_path_get,
						      "exn:missing-module-accessor",
						      1, 1, 1),
			     env);

  {
    Scheme_Object *p;
    p = scheme_make_prim_w_arity(scheme_extract_checked_procedure,
                                 "checked-procedure-check-and-extract",
                                 5, 5);
    SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
    scheme_add_global_constant("checked-procedure-check-and-extract", p, env);
  }

  scheme_add_global_constant("chaperone-struct",
                             scheme_make_prim_w_arity(chaperone_struct,
                                                      "chaperone-struct",
                                                      1, -1),
                             env);
  scheme_add_global_constant("impersonate-struct",
                             scheme_make_prim_w_arity(impersonate_struct,
                                                      "impersonate-struct",
                                                      1, -1),
                             env);
  scheme_add_global_constant("chaperone-struct-type",
                             scheme_make_prim_w_arity(chaperone_struct_type,
                                                      "chaperone-struct-type",
                                                      4, -1),
                             env);
  scheme_add_global_constant("make-impersonator-property", 
			    scheme_make_prim_w_arity2(make_chaperone_property,
						      "make-impersonator-property",
						      1, 1,
						      3, 3),
			    env);
  scheme_add_global_constant("impersonator-property?",
			     scheme_make_folding_prim(chaperone_property_p,
						     "impersonator-property?",
						     1, 1, 1),
			    env);

  {
    REGISTER_SO(scheme_app_mark_impersonator_property);
    scheme_app_mark_impersonator_property = make_chaperone_property_from_c(scheme_intern_symbol("application-mark"));
    scheme_add_global_constant("impersonator-prop:application-mark",
                               scheme_app_mark_impersonator_property,
                               env);
  }
}

void scheme_init_struct_wait()
{
  scheme_add_evt(scheme_structure_type,
                 (Scheme_Ready_Fun)evt_struct_is_ready,
                 NULL,
                 is_evt_struct, 1);
  scheme_add_evt(scheme_proc_struct_type,
                 (Scheme_Ready_Fun)evt_struct_is_ready,
                 NULL,
                 is_evt_struct, 1);

  scheme_add_evt(scheme_wrap_evt_type,
		 (Scheme_Ready_Fun)wrapped_evt_is_ready,
		 NULL, NULL, 1);
  scheme_add_evt(scheme_handle_evt_type,
		 (Scheme_Ready_Fun)wrapped_evt_is_ready,
		 NULL, NULL, 1);
  scheme_add_evt(scheme_replace_evt_type,
		 (Scheme_Ready_Fun)replace_evt_is_ready,
		 NULL, NULL, 1);
  scheme_add_evt(scheme_active_replace_evt_type,
		 (Scheme_Ready_Fun)active_replace_evt_is_ready,
		 active_replace_evt_needs_wakeup,
		 NULL, 1);
  scheme_add_evt(scheme_chaperone_type,
		 (Scheme_Ready_Fun)chaperone_evt_is_ready,
		 NULL, 
                 is_chaperone_evt, 1);
  scheme_add_evt(scheme_proc_chaperone_type,
		 (Scheme_Ready_Fun)chaperone_evt_is_ready,
		 NULL, 
                 is_chaperone_evt, 1);
  scheme_add_evt(scheme_nack_guard_evt_type,
		 (Scheme_Ready_Fun)nack_guard_evt_is_ready,
		 NULL, NULL, 1);
  scheme_add_evt(scheme_nack_evt_type,
		 (Scheme_Ready_Fun)nack_evt_is_ready,
		 NULL, NULL, 1);
  scheme_add_evt(scheme_poll_evt_type,
		 (Scheme_Ready_Fun)poll_evt_is_ready,
		 NULL, NULL, 1);
}

/*========================================================================*/
/*                             inspectors                                 */
/*========================================================================*/

Scheme_Object *scheme_make_initial_inspectors(void)
{
  Scheme_Inspector *superior, *root;

  superior = MALLOC_ONE_TAGGED(Scheme_Inspector);
  superior->so.type = scheme_inspector_type;
  superior->depth = 0;
  
  root = MALLOC_ONE_TAGGED(Scheme_Inspector);
  root->so.type = scheme_inspector_type;
  root->depth = 1;
  root->superior = superior;

  return (Scheme_Object *)root;
}

Scheme_Object *scheme_make_inspector(Scheme_Object *superior)
{
  Scheme_Inspector *naya;

  naya = MALLOC_ONE_TAGGED(Scheme_Inspector);
  naya->so.type = scheme_inspector_type;
  naya->depth = ((Scheme_Inspector *)superior)->depth + 1;
  naya->superior = (Scheme_Inspector *)superior;

  return (Scheme_Object *)naya;
}

static Scheme_Object *make_inspector(int argc, Scheme_Object **argv)
{
  Scheme_Object *superior;

  if (argc) {
    superior = argv[0];
    if (!SAME_TYPE(SCHEME_TYPE(superior), scheme_inspector_type))
      scheme_wrong_contract("make-inspector", "inspector?", 0, argc, argv);
  } else
    superior = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);

  return scheme_make_inspector(superior);
}

static Scheme_Object *make_sibling_inspector(int argc, Scheme_Object **argv)
{
  Scheme_Object *superior;

  if (argc) {
    superior = argv[0];
    if (!SAME_TYPE(SCHEME_TYPE(superior), scheme_inspector_type))
      scheme_wrong_contract("make-sibling-inspector", "inspector?", 0, argc, argv);
  } else
    superior = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);

  superior = (Scheme_Object *)((Scheme_Inspector *)superior)->superior;

  return scheme_make_inspector(superior);
}

static Scheme_Object *inspector_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_inspector_type)
	  ? scheme_true
	  : scheme_false);
}

int scheme_is_subinspector(Scheme_Object *i, Scheme_Object *sup)
{
  Scheme_Inspector *ins, *superior;

  if (SCHEME_FALSEP(i))
    return 1;
  if (SCHEME_FALSEP(sup))
    return 0;

  ins = (Scheme_Inspector *)i;
  superior = (Scheme_Inspector *)sup;

  while (ins->depth > superior->depth) {
    if (ins->superior == superior)
      return 1;
    ins = ins->superior;
  }
   
  return 0;
}

static Scheme_Object *current_inspector(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-inspector", 
			     scheme_make_integer(MZCONFIG_INSPECTOR),
			     argc, argv,
			     -1, inspector_p, "inspector", 0);
}

static Scheme_Object *current_code_inspector(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-code-inspector", 
			     scheme_make_integer(MZCONFIG_CODE_INSPECTOR),
			     argc, argv,
			     -1, inspector_p, "inspector", 0);
}

/*========================================================================*/
/*                             properties                                 */
/*========================================================================*/

static Scheme_Object *prop_pred(int argc, Scheme_Object **args, Scheme_Object *prim)
{
  Scheme_Struct_Type *stype;
  Scheme_Object *prop = SCHEME_PRIM_CLOSURE_ELS(prim)[0], *v;
  Scheme_Chaperone *px;

  v = args[0];

  while (1) {
    if (SCHEME_CHAPERONEP(v)) {
      /* Check for property at chaperone level: */
      px = (Scheme_Chaperone *)v;
      if (px->props)
        v = scheme_hash_tree_get(px->props, prop);
      else
        v = NULL;
      if (v)
        return scheme_true;
      v = px->val;
    }

    if (SAME_TYPE(SCHEME_TYPE(prop), scheme_chaperone_property_type)) {
      /* An impersonator property must be at the chaperone level, but can
         be via `prop:impersonator-of`: */
      Scheme_Object *procs;
      procs = scheme_struct_type_property_ref(scheme_impersonator_of_property, v);
      if (procs) {
        v = scheme_apply_impersonator_of(0, procs, v);
        if (!v)
          return scheme_false;
      } else
        return scheme_false;
    } else
      break;
  }

  if (SCHEME_STRUCTP(v))
    stype = ((Scheme_Structure *)v)->stype;
  else if (SAME_TYPE(SCHEME_TYPE(v), scheme_struct_type_type))
    stype = (Scheme_Struct_Type *)v;
  else
    return scheme_false;

  if (stype->num_props < 0) {
    if (scheme_hash_get((Scheme_Hash_Table *)stype->props, prop))
      return scheme_true;
  } else {
    int i;
    for (i = stype->num_props; i--; ) {
      if (SAME_OBJ(SCHEME_CAR(stype->props[i]), prop))
	return scheme_true;
    }
  }
   
  return scheme_false;
}

XFORM_NONGCING static Scheme_Object *do_prop_accessor(Scheme_Object *prop, Scheme_Object *arg)
{
  Scheme_Struct_Type *stype;

  if (SCHEME_STRUCTP(arg))
    stype = ((Scheme_Structure *)arg)->stype;
  else if (SAME_TYPE(SCHEME_TYPE(arg), scheme_struct_type_type))
    stype = (Scheme_Struct_Type *)arg;
  else
    stype = NULL;

  if (stype) {
    if (stype->num_props < 0) {
      Scheme_Object *v;
      v = (Scheme_Object *)scheme_eq_hash_get((Scheme_Hash_Table *)stype->props, prop);
      if (v)
	return v;
    } else {
      int i;
      for (i = stype->num_props; i--; ) {
	if (SAME_OBJ(SCHEME_CAR(stype->props[i]), prop))
	  return SCHEME_CDR(stype->props[i]);
      }
    }
  }
  
  return NULL;
}

static Scheme_Object *do_chaperone_prop_accessor(const char *who, Scheme_Object *prop, 
                                                 Scheme_Object *orig_arg, Scheme_Object *arg);

static Scheme_Object *chaperone_prop_acc_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object *arg = (Scheme_Object *)p->ku.k.p2;
  const char *who = (const char *)p->ku.k.p3;
  Scheme_Object *orig_arg = (Scheme_Object *)p->ku.k.p4;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  return do_chaperone_prop_accessor(who, o, orig_arg, arg);
}

static Scheme_Object *chaperone_prop_acc_overflow(const char *who, Scheme_Object *o,
                                                  Scheme_Object *orig_arg, Scheme_Object *arg)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = (void *)o;
  p->ku.k.p2 = (void *)arg;
  p->ku.k.p3 = (void *)who;
  p->ku.k.p4 = (void *)orig_arg;

  return scheme_handle_stack_overflow(chaperone_prop_acc_k);
}

static Scheme_Object *do_chaperone_prop_accessor(const char *who, Scheme_Object *prop,
                                                 Scheme_Object *orig_arg,  Scheme_Object *arg)
{
  while (1) {
    if (SCHEME_CHAPERONEP(arg)) {
      Scheme_Chaperone *px = (Scheme_Chaperone *)arg;
      Scheme_Object *a[2], *red, *orig;
      Scheme_Object *v;
      Scheme_Hash_Tree *ht;

      if (px->props) {
        v = scheme_hash_tree_get(px->props, prop);
        if (v)
          return v;
      }

      if (!SCHEME_VECTORP(px->redirects)
          || (SCHEME_VEC_SIZE(px->redirects) & 1)
          || SCHEME_FALSEP(SCHEME_VEC_ELS(px->redirects)[0]))
        arg = px->prev;
      else {
        ht = (Scheme_Hash_Tree *)SCHEME_VEC_ELS(px->redirects)[0];
        if (ht)
          red = scheme_hash_tree_get(ht, prop);
        else
          red = NULL;
        if (!red)
          arg = px->prev;
        else {
#ifdef DO_STACK_CHECK
          {
# include "mzstkchk.h"
            return chaperone_prop_acc_overflow(who, prop, orig_arg, arg);
          }
#endif
          
          arg = px->prev;
          if (SCHEME_PAIRP(red)) {
            /* Operation used to chaperone the struct was itself chaperoned.
               Use the chaperoned operation to get the result to chaperone
               further. */
            a[0] = arg;
            orig = _scheme_apply(SCHEME_CAR(red), 1, a);
            red = SCHEME_CDR(red);
          } else {
            orig = do_chaperone_prop_accessor(who, prop, orig_arg, arg);
          }

          if (!orig) return NULL;
          
          a[0] = orig_arg;
          a[1] = orig;
          v = _scheme_apply(red, 2, a);
    
          if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR))
            if (!scheme_chaperone_of(v, orig))
              scheme_wrong_chaperoned(who, "result", orig, v);
          
          return v;
        }
      }
    } else {
      if (SAME_TYPE(SCHEME_TYPE(prop), scheme_chaperone_property_type)) {
        Scheme_Object *procs;
        procs = scheme_struct_type_property_ref(scheme_impersonator_of_property, arg);
        if (procs) {
          arg = scheme_apply_impersonator_of(0, procs, arg);
          if (!arg)
            return NULL;
          /* loop to try again */
        } else {
          /* an impersonator property lives at the impersonator/chaperone level, only: */
          return NULL;
        }
      } else {
        return do_prop_accessor(prop, arg);
      }
    }
  }
}

static Scheme_Object *prop_accessor(int argc, Scheme_Object **args, Scheme_Object *prim)
{
  Scheme_Object *v;

  v = args[0];
  if (SCHEME_CHAPERONEP(v)
      || SAME_TYPE(SCHEME_TYPE(SCHEME_PRIM_CLOSURE_ELS(prim)[0]), scheme_chaperone_property_type))
    v = do_chaperone_prop_accessor(((Scheme_Primitive_Proc *)prim)->name, SCHEME_PRIM_CLOSURE_ELS(prim)[0],
                                   v, v);
  else
    v = do_prop_accessor(SCHEME_PRIM_CLOSURE_ELS(prim)[0], v);

  if (v)
    return v;
  else if (argc == 1) {
    scheme_wrong_contract(((Scheme_Primitive_Proc *)prim)->name,
                          pred_name_string(((Scheme_Struct_Property *)SCHEME_PRIM_CLOSURE_ELS(prim)[0])->name),
                          0, 1, args);
    return NULL;
  } else {
    v = args[1];
    if (SCHEME_PROCP(v))
      return _scheme_tail_apply(v, 0, NULL);
    else
      return v;
  }
}

static Scheme_Object *make_struct_type_property_from_c(int argc, Scheme_Object *argv[],
                                                       Scheme_Object **predout, Scheme_Object **accessout,
                                                       Scheme_Type type) 
{

  Scheme_Struct_Property *p;
  Scheme_Object *a[1], *v, *supers = scheme_null;
  char *name;
  int len;
  const char *who;
  char can_impersonate = 0;

  if (type == scheme_struct_property_type)
    who = "make-struct-type-property";
  else
    who = "make-impersonator-property";

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract(who, "symbol?", 0, argc, argv);
  if (argc > 1) {
    if (SCHEME_SYMBOLP(argv[1])
        && !SCHEME_SYM_WEIRDP(argv[1])
        && !strcmp("can-impersonate", SCHEME_SYM_VAL(argv[1]))) {
      can_impersonate = 1;
    } else if (SCHEME_TRUEP(argv[1])
               && !scheme_check_proc_arity(NULL, 2, 1, argc, argv))
      scheme_wrong_contract(who, "(or/c (any/c any/c . -> . any) #f 'can-impersonate)", 1, argc, argv);

    if (argc > 2) {
      supers = argv[2];
      if (scheme_proper_list_length(supers) < 0)
        supers = NULL;
      else {
        Scheme_Object *pr;
        for (pr = supers; supers && SCHEME_PAIRP(pr); pr = SCHEME_CDR(pr)) {
          v = SCHEME_CAR(pr);
          if (!SCHEME_PAIRP(v)) {
            supers = NULL;
          } else {
            if (!SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(v)), scheme_struct_property_type))
              supers = NULL;
            a[0] = SCHEME_CDR(v);
            if (!scheme_check_proc_arity(NULL, 1, 0, 1, a))
              supers = NULL;
          }
        }
      }

      if (!supers) {
        scheme_wrong_contract(who, 
                              "(listof (cons struct-type-property? (any/c . -> . any)))", 
                              2, argc, argv);
      }

      if (argc > 3)
        can_impersonate = SCHEME_TRUEP(argv[3]);
    }
  }

  p = MALLOC_ONE_TAGGED(Scheme_Struct_Property);
  p->so.type = type;
  p->name = argv[0];
  if ((argc > 1) && SCHEME_TRUEP(argv[1]))
    p->guard = argv[1];
  p->supers = supers;
  p->can_impersonate = can_impersonate;

  a[0] = (Scheme_Object *)p;

  len = SCHEME_SYM_LEN(argv[0]);
  name = MALLOC_N_ATOMIC(char, len + 2);
  memcpy(name, SCHEME_SYM_VAL(argv[0]), len);
  name[len] = '?';
  name[len+1] = 0;

  v = scheme_make_folding_prim_closure(prop_pred, 1, a, name, 1, 1, 0);
  ((Scheme_Closed_Primitive_Proc *)v)->pp.flags |= SCHEME_PRIM_STRUCT_TYPE_STRUCT_PROP_PRED;
  *predout = v;

  name = MALLOC_N_ATOMIC(char, len + 10);
  memcpy(name, SCHEME_SYM_VAL(argv[0]), len);
  memcpy(name + len, "-accessor", 10);

  v = scheme_make_prim_closure_w_arity(prop_accessor, 1, a, name, 1, 2);
  ((Scheme_Closed_Primitive_Proc *)v)->pp.flags |= SCHEME_PRIM_TYPE_STRUCT_PROP_GETTER;
  
  *accessout = v;

  return a[0];
}

static Scheme_Object *make_struct_type_property(int argc, Scheme_Object *argv[])
{
  Scheme_Object *a[3];
  a[0] = make_struct_type_property_from_c(argc, argv, &a[1], &a[2], scheme_struct_property_type);
  return scheme_values(3, a);
}

static Scheme_Object *make_chaperone_property(int argc, Scheme_Object *argv[])
{
  Scheme_Object *a[3];
  a[0] = make_struct_type_property_from_c(argc, argv, &a[1], &a[2], scheme_chaperone_property_type);
  return scheme_values(3, a);
}

static Scheme_Object *make_chaperone_property_from_c(Scheme_Object *name)
{
  Scheme_Object *a[3];

  a[0] = name;
  return make_struct_type_property_from_c(1, a, &a[1], &a[2], scheme_chaperone_property_type);
}

Scheme_Object *scheme_make_struct_type_property_w_guard(Scheme_Object *name, Scheme_Object *guard)
{
  Scheme_Object *a[2];
  Scheme_Object *pred = NULL;
  Scheme_Object *access = NULL;

  a[0] = name;
  a[1] = guard;
  return make_struct_type_property_from_c(2, a, &pred, &access, scheme_struct_property_type);
}

Scheme_Object *scheme_make_struct_type_property(Scheme_Object *name)
{
  return scheme_make_struct_type_property_w_guard(name, scheme_false);
}

Scheme_Object *scheme_chaperone_struct_type_property_ref(Scheme_Object *prop, Scheme_Object *s)
{
  if (SCHEME_CHAPERONEP(s))
    return do_chaperone_prop_accessor("impersonator-property-ref", prop, s, s);
  else
    return do_prop_accessor(prop, s);
}

Scheme_Object *scheme_struct_type_property_ref(Scheme_Object *prop, Scheme_Object *s)
{
  if (SCHEME_CHAPERONEP(s))
    s = SCHEME_CHAPERONE_VAL(s);
  return do_prop_accessor(prop, s);
}

static Scheme_Object *struct_type_property_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_struct_property_type)
	  ? scheme_true : scheme_false);
}

static Scheme_Object *chaperone_property_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_chaperone_property_type)
	  ? scheme_true : scheme_false);
}

static Scheme_Object *guard_property(Scheme_Object *prop, Scheme_Object *v, Scheme_Struct_Type *t)
{
  Scheme_Struct_Property *p = (Scheme_Struct_Property *)prop;

  if (SAME_OBJ(prop, proc_property)) {
    /* prop:procedure guard: */
    Scheme_Object *orig_v = v;
    if (SCHEME_INTP(v) || SCHEME_BIGNUMP(v)) {
      intptr_t pos;

      if (SCHEME_INTP(v))
	pos = SCHEME_INT_VAL(v);
      else if (SCHEME_BIGPOS(v))
	pos = t->num_slots; /* too big */
      else
        pos = -1; /* negative bignum */

      if (pos >= 0) {
        Scheme_Struct_Type *parent_type;
        intptr_t field_count;

        if (t->name_pos > 0)
          parent_type = t->parent_types[t->name_pos - 1];
        else
          parent_type = NULL;

        field_count = (t->num_islots - (parent_type ? parent_type->num_islots : 0));
        if (pos >= field_count) {
          scheme_contract_error("make-struct-type", 
                                "index for procedure >= initialized-field count", 
                                "index", 1, v,
                                "field count", 1, scheme_make_integer(field_count),
                                NULL);
          return NULL;
        }

        if (parent_type) {
          /* proc_attr needs to be in terms of the whole field array */
          pos += parent_type->num_slots;
          v = scheme_make_integer(pos);
        }
      } else
        v = scheme_false; /* complain below */
    }

    if (SCHEME_INTP(v) || SCHEME_PROCP(v)) {
      /* ok */
    } else {
      scheme_contract_error("make-struct-type", 
                            "given value did not satisfy the contract for prop:procedure", 
                            "expected",  0, "(or/c procedure? exact-nonnegative-integer?)",
                            "given", 1, orig_v, 
                            NULL);
    }

    t->proc_attr = v;

    if (SCHEME_INTP(v)) {
      intptr_t pos;
      pos = SCHEME_INT_VAL(orig_v);
      if (!t->immutables || !t->immutables[pos]) { 
        scheme_contract_error("make-struct-type", 
                              "field is not specified as immutable for a prop:procedure index", 
                              "index", 1, orig_v,
                              NULL);
      }
    }

    return orig_v;
  } else {
    /* Normal guard handling: */
    if (p->guard && !SCHEME_SYMBOLP(p->guard)) {
      if(!scheme_defining_primitives) {
        Scheme_Object *a[2], *info[mzNUM_ST_INFO], *l;

        a[0] = (Scheme_Object *)t;
        get_struct_type_info(1, a, info, 1);

        l = scheme_build_list(mzNUM_ST_INFO, info);

        a[0] = v;
        a[1] = l;

        return _scheme_apply(p->guard, 2, a);
      }
      else 
        return v;
    } else
      return v;
  }
}

/*========================================================================*/
/*                            evt structs                                 */
/*========================================================================*/

static int extract_accessor_offset(Scheme_Object *acc)
{
  Scheme_Struct_Type *st = (Scheme_Struct_Type *)SCHEME_PRIM_CLOSURE_ELS(acc)[0];

  if (st->name_pos)
    return st->parent_types[st->name_pos - 1]->num_slots;
  else
    return 0;
}

static char *extract_field_proc_name(Scheme_Object *prim)
{
  return (char *)SCHEME_PRIM_CLOSURE_ELS(prim)[2];
}

typedef int (*Check_Val_Proc)(Scheme_Object *);

static void wrong_property_contract(const char *name, const char *contract, Scheme_Object *v)
{
  scheme_contract_error(name, 
                        "contract violation for given property value",
                        "expected", 0, contract,
                        "given", 1, v,
                        NULL);
}

static Scheme_Object *check_indirect_property_value_ok(const char *name, Check_Val_Proc ck, int proc_ok,
                                                       const char *contract,
                                                       int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *l, *acc;
  int pos, num_islots;

  v = argv[0];
  
  if (ck(v))
    return v;

  if (proc_ok && scheme_check_proc_arity(NULL, 1, 0, 1, &v))
    return v;
  
  if (!((SCHEME_INTP(v) && (SCHEME_INT_VAL(v) >= 0))
	|| (SCHEME_BIGNUMP(v) && SCHEME_BIGPOS(v))))
    wrong_property_contract(name, contract,v);
  
  l = argv[1];
  l = SCHEME_CDR(l);
  num_islots = SCHEME_INT_VAL(SCHEME_CAR(l));
  l = SCHEME_CDR(l);
  l = SCHEME_CDR(l);
  acc = SCHEME_CAR(l);
  l = SCHEME_CDR(l);
  l = SCHEME_CDR(l);
  l = SCHEME_CAR(l);

  if (SCHEME_BIGNUMP(v))
    pos = num_islots; /* too big */
  else
    pos = SCHEME_INT_VAL(v);

  if (pos >= num_islots) {
    scheme_contract_error(name,
                          "field index >= initialized-field count for structure type",
                          "field index", 1, v,
                          "initialized-field count", 1, scheme_make_integer(num_islots),
                          NULL);
  }

  for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    if (SCHEME_INT_VAL(SCHEME_CAR(l)) == pos)
      break;
  }

  if (!SCHEME_PAIRP(l)) {
    scheme_contract_error(name,
                          "field index not declared immutable",
                          "field index", 1, v,
                          NULL);
  }

  pos += extract_accessor_offset(acc);
  v = scheme_make_integer(pos);

  return v;
}

static Scheme_Object *check_evt_property_value_ok(int argc, Scheme_Object *argv[])
/* This is the guard for prop:evt */
{
  return check_indirect_property_value_ok("guard-for-prop:evt", 
                                          scheme_is_evt, 1,
                                          "(or/c evt? (any/c . -> . any) exact-nonnegative-integer?)",
                                          argc, argv);
}

static Scheme_Object *return_wrapped(void *data, int argc, Scheme_Object *argv[])
{
  return (Scheme_Object *)data;
}

static int evt_struct_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *v;

  if (sinfo->false_positive_ok) {
    sinfo->potentially_false_positive = 1;
    return 1;
  }

  v = scheme_struct_type_property_ref(evt_property, o);

  if (!v) {
    /* Must be an input or output port: */
    if (SCHEME_INPUT_PORTP(o)) {
      v = (Scheme_Object *)scheme_input_port_record(o);
    } else {
      v = (Scheme_Object *)scheme_output_port_record(o);
    }
    scheme_set_sync_target(sinfo, v, NULL, NULL, 0, 1, NULL);
    return 0;
  }

  if (SCHEME_INTP(v))
    v = scheme_struct_ref(o, SCHEME_INT_VAL(v));

  if (scheme_is_evt(v)) {
    scheme_set_sync_target(sinfo, v, NULL, NULL, 0, 1, NULL);
    return 0;
  }

  if (SCHEME_PROCP(v)) {
    if (scheme_check_proc_arity(NULL, 1, 0, 1, &v)) {
      Scheme_Object *f = v, *result, *a[1];

      a[0] = o;
      result = scheme_apply(f, 1, a);

      if (scheme_is_evt(result)) {
	SCHEME_USE_FUEL(1); /* Needed beause an apply of a mzc-generated function
			       might not check for breaks. */
	scheme_set_sync_target(sinfo, result, NULL, NULL, 0, 1, NULL);
	return 0;
      }

      /* non-evt => ready and result is self; if self is a procedure,
         we need to wrap it, so that self is not treated as a `wrap-evt'
         procedure. */
      if (SCHEME_PROCP(o)) {
        o = scheme_make_closed_prim_w_arity(return_wrapped, (void *)o, "wrapper", 1, 1);
      }
      scheme_set_sync_target(sinfo, o, o, NULL, 0, 0, NULL);

      return 1;
    }
  }

  return 0;
}

static int is_evt_struct(Scheme_Object *o)
{
  if (scheme_struct_type_property_ref(evt_property, o))
    return 1;
  if (scheme_struct_type_property_ref(scheme_input_port_property, o))
    return 1;
  if (scheme_struct_type_property_ref(scheme_output_port_property, o))
    return 1;
  return 0;
}


/*========================================================================*/
/*                            object-name structs                         */
/*========================================================================*/
/* This is here so it can use check_indirect_property_value_ok */

static int is_proc_1(Scheme_Object *o) { return (SCHEME_PROCP(o) && scheme_check_proc_arity(NULL, 1, -1, 0, &o)); }
static int is_proc_1_or_2(Scheme_Object *o) { return (SCHEME_PROCP(o) && (scheme_check_proc_arity(NULL, 1, -1, 0, &o)
                                                                          || scheme_check_proc_arity(NULL, 2, -1, 0, &o))); }



static Scheme_Object *check_object_name_property_value_ok(int argc, Scheme_Object *argv[])
/* This is the guard for prop:object-name */
{
  return check_indirect_property_value_ok("guard-for-prop:object-name",
                                          is_proc_1, 1,
                                          "(or/c (any/c . -> . any) exact-nonnegative-integer?)",
                                          argc, argv);
}


/*========================================================================*/
/*                            port structs                                */
/*========================================================================*/

static int is_input_port(Scheme_Object *v) {  return SCHEME_INPUT_PORTP(v); }
static int is_output_port(Scheme_Object *v) {  return SCHEME_OUTPUT_PORTP(v); }

static Scheme_Object *check_port_property_value_ok(const char *name, int input, int argc, Scheme_Object *argv[])
/* This is the guard for prop:input-port and prop:output-port */
{
  return check_indirect_property_value_ok(name, 
                                          input ? is_input_port : is_output_port, 0,
                                          (input
                                           ? "(or/c input-port? exact-nonnegative-integer?)"
                                           : "(or/c output-port? exact-nonnegative-integer?)"),
                                          argc, argv);
}

static Scheme_Object *check_input_port_property_value_ok(int argc, Scheme_Object *argv[])
{
  return check_port_property_value_ok("guard-for-prop:input-port", 1, argc, argv);
}

static Scheme_Object *check_output_port_property_value_ok(int argc, Scheme_Object *argv[])
{
  return check_port_property_value_ok("guard-for-prop:output-port", 0, argc, argv);
}

/*========================================================================*/
/*                          cpointer property                             */
/*========================================================================*/

static Scheme_Object *check_cpointer_property_value_ok(int argc, Scheme_Object *argv[])
{
  return check_indirect_property_value_ok("guard-for-prop:cpointer", 
                                          scheme_is_cpointer, 1,
                                          "(or/c cpointer? (any/c . -> . any) exact-nonnegative-integer?)",
                                          argc, argv);
}

/*========================================================================*/
/*                         equal+hash property                            */
/*========================================================================*/

static Scheme_Object *check_equal_property_value_ok(int argc, Scheme_Object *argv[])
/* This is the guard for prop:equal+hash */
{
  Scheme_Object *v, *p;

  v = argv[0];

  if (scheme_proper_list_length(v) != 3) {
    v = NULL;
  } else {
    v = scheme_make_pair(scheme_make_symbol("tag"), v);
    v = scheme_list_to_vector(v);
    p = SCHEME_VEC_ELS(v)[1];
    if (!scheme_check_proc_arity(NULL, 3, 0, 1, &p)) {
      v = NULL;
    } else {
      p = SCHEME_VEC_ELS(v)[2];
      if (!scheme_check_proc_arity(NULL, 2, 0, 1, &p)) {
        v = NULL;
      } else {
        p = SCHEME_VEC_ELS(v)[3];
        if (!scheme_check_proc_arity(NULL, 2, 0, 1, &p)) {
          v = NULL;
        }
      }
    }
  }

  if (!v) {
    wrong_property_contract("guard-for-prop:equal+hash",
                            "(list/c (any/c any/c any/c . -> . any)\n"
                            "        (any/c any/c . -> . any)\n"
                            "        (any/c any/c . -> . any))",
                            argv[0]);
  }

  return v;
}

static Scheme_Object *check_impersonator_of_property_value_ok(int argc, Scheme_Object *argv[])
{
  /* This is the guard for prop:impersonator-of */
  Scheme_Object *v;

  v = argv[0];

  if (!scheme_check_proc_arity(NULL, 1, 0, argc, argv)) {
    wrong_property_contract("guard-for-prop:impersonator-of",
                            "(any/c . -> . any)",
                            v); 
  }

  /* Add a tag to track origin of the impersonator-of property: */
  v = scheme_make_pair(scheme_make_symbol("tag"), v);

  return v;
}

/*========================================================================*/
/*                          writeable structs                             */
/*========================================================================*/

static Scheme_Object *check_write_property_value_ok(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  v = argv[0];

  if (!scheme_check_proc_arity(NULL, 3, 0, argc, argv)) {
    wrong_property_contract("guard-for-prop:custom-write",
                            "(any/c any/c any/c . -> . any)",
                            v);
  }

  return v;
}

static Scheme_Object *check_print_attribute_property_value_ok(int argc, Scheme_Object *argv[])
{
  Scheme_Object *s = argv[0];
  if (SCHEME_SYMBOLP(s) && !SCHEME_SYM_WEIRDP(s)) {
    if (!strcmp(SCHEME_SYM_VAL(s), "self")
        || !strcmp(SCHEME_SYM_VAL(s), "never")
        || !strcmp(SCHEME_SYM_VAL(s), "always")
        || !strcmp(SCHEME_SYM_VAL(s), "maybe"))
      return s;
  }

  wrong_property_contract("guard-for-prop:custom-print-quotable",
                          "(or/c 'self 'never 'always 'maybe)",
                          s);

  return NULL;
}

Scheme_Object *scheme_is_writable_struct(Scheme_Object *s)
{
  return scheme_struct_type_property_ref(write_property, s);
}

Scheme_Object *scheme_print_attribute_ref(Scheme_Object *s)
{
  return scheme_struct_type_property_ref(print_attribute_property, s);
}

static Scheme_Object *unary_acc(int argc, Scheme_Object **argv, Scheme_Object *self)
{
  Scheme_Object *acc = SCHEME_PRIM_CLOSURE_ELS(self)[0];

  return _scheme_apply(acc, argc, argv);
}

/*========================================================================*/
/*                  rename and set! transformer properties                */
/*========================================================================*/

int scheme_is_rename_transformer(Scheme_Object *o)
{
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_id_macro_type))
    return 1;
  if (SCHEME_CHAPERONE_STRUCTP(o)
      && scheme_struct_type_property_ref(rename_transformer_property, o))
    return 1;
  return 0;
}

int scheme_is_binding_rename_transformer(Scheme_Object *o)
{
  if (scheme_is_rename_transformer(o)) {
    o = scheme_rename_transformer_id(o);
    o = scheme_stx_property(o, not_free_id_symbol, NULL);
    if (o && SCHEME_TRUEP(o))
      return 0;
    return 1;
  }
  return 0;
}

static int is_stx_id(Scheme_Object *o) { return (SCHEME_STXP(o) && SCHEME_SYMBOLP(SCHEME_STX_VAL(o))); }

static int is_stx_id_or_proc_1(Scheme_Object *o) { return (is_stx_id(o) || is_proc_1(o)); }

Scheme_Object *scheme_rename_transformer_id(Scheme_Object *o)
{
  Scheme_Object *a[1];

  if (SAME_TYPE(SCHEME_TYPE(o), scheme_id_macro_type))
    return SCHEME_PTR1_VAL(o);
  if (SCHEME_CHAPERONE_STRUCTP(o)) {
    Scheme_Object *v;
    v = scheme_struct_type_property_ref(rename_transformer_property, o);
    if (SCHEME_PROCP(v)) {
      a[0] = o;
      /* apply a continuation barrier here to prevent a capture in
       * the property access */
      v = scheme_apply(v, 1, a);
      if (!is_stx_id(v)) {
        scheme_contract_error("prop:rename-transformer",
                              "contract violation for given value",
                              "expected", 0, "identifier?",
                              "given", 1, v,
                              NULL);
      }
    } else if (SCHEME_INTP(v)) {
      v = scheme_struct_ref(o, SCHEME_INT_VAL(v));
      if (!is_stx_id(v)) {
        v = scheme_datum_to_syntax(scheme_intern_symbol("?"), scheme_false, scheme_false, 0, 0);
      }
    }
    return v;
  }
  return NULL;
}

static Scheme_Object *check_rename_transformer_property_value_ok(int argc, Scheme_Object *argv[])
{
  return check_indirect_property_value_ok("guard-for-prop:rename-transformer", 
                                          is_stx_id_or_proc_1, 0,
                                          "(or/c exact-nonnegative-integer? identifier? (-> any/c identifier?))",
                                          argc, argv);
}

int scheme_is_set_transformer(Scheme_Object *o)
{
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_set_macro_type))
    return 1;
  if (SCHEME_CHAPERONE_STRUCTP(o)
      && scheme_struct_type_property_ref(set_transformer_property, o))
    return 1;
  return 0;
}

Scheme_Object *signal_bad_syntax(int argc, Scheme_Object **argv)
{
  scheme_wrong_syntax(NULL, NULL, argv[0], "bad syntax");
  return NULL;
}

static Scheme_Object *chain_transformer(void *data, int argc, Scheme_Object *argv[])
{
  Scheme_Object *a[2], *v = (Scheme_Object *)data;
  a[0] = SCHEME_CAR(v);
  a[1] = argv[0];
  return _scheme_tail_apply(SCHEME_CDR(v), 2, a);
}

Scheme_Object *scheme_set_transformer_proc(Scheme_Object *o)
{
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_set_macro_type))
    return SCHEME_PTR_VAL(o);
  if (SCHEME_CHAPERONE_STRUCTP(o)) {
    Scheme_Object *v;
    v = scheme_struct_type_property_ref(set_transformer_property, o);
    if (SCHEME_INTP(v)) {
      v = ((Scheme_Structure *)o)->slots[SCHEME_INT_VAL(v)];
      if (!is_proc_1(v)) {
        v = scheme_make_prim_w_arity(signal_bad_syntax,
                                     "bad-syntax-set!-transformer",
                                     1, 1);
      }
    } else if (!scheme_check_proc_arity(NULL, 1, -1, 0, &v)) {
      /* Must be a procedure of 2 arguments. Reduce to a procedure of 1. */
      o = scheme_make_pair(o, v);
      v = scheme_make_closed_prim_w_arity(chain_transformer, (void *)o,
                                          "set!-transformer", 1, 1);
    }
    return v;
  }
  return NULL;
}

static Scheme_Object *check_set_transformer_property_value_ok(int argc, Scheme_Object *argv[])
{
  return check_indirect_property_value_ok("guard-for-prop:set!-transformer", 
                                          is_proc_1_or_2, 0,
                                          "(or/c  (any/c . -> . any) (any/c any/c . -> . any) exact-nonnegative-integer?)",
                                          argc, argv);
}

/*========================================================================*/
/*                        expansion-contexts property                     */
/*========================================================================*/

static Scheme_Object *check_expansion_contexts_property_value_ok(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;
  
  v = argv[0];

  while (SCHEME_PAIRP(v)) {
    if (!scheme_is_expansion_context_symbol(SCHEME_CAR(v)))
      break;
    v = SCHEME_CDR(v);
  }

  if (SCHEME_NULLP(v))
    return argv[0];

  wrong_property_contract("guard-for-prop:expression-contexts",
                          "(lisrof (or/c 'expression 'top-level 'module 'module-begin 'definition-context)",
                          v);

  return NULL;
}

int scheme_expansion_contexts_include(Scheme_Object *o, Scheme_Object *ctx)
{
  Scheme_Object *v;

  if (SCHEME_CHAPERONE_STRUCTP(o)) {
    v = scheme_chaperone_struct_type_property_ref(expansion_contexts_property, o);
    if (v) {
      while (!SCHEME_NULLP(v)) {
        if (SAME_OBJ(SCHEME_CAR(v), ctx))
          return 1;
        v = SCHEME_CDR(v);
      }
      return 0;
    }
  }
  
  return 1;
}

/*========================================================================*/
/*                           checked-proc property                        */
/*========================================================================*/

static Scheme_Object *check_checked_proc_property_value_ok(int argc, Scheme_Object *argv[])
{
  Scheme_Object *parent, *l;
  int num_islots, num_aslots;

  l = argv[1];
  l = SCHEME_CDR(l);
  num_islots = SCHEME_INT_VAL(SCHEME_CAR(l));
  l = SCHEME_CDR(l);
  num_aslots = SCHEME_INT_VAL(SCHEME_CAR(l));
  l = SCHEME_CDR(l);
  l = SCHEME_CDR(l);
  l = SCHEME_CDR(l);
  l = SCHEME_CDR(l);
  parent = SCHEME_CAR(l);

  if (SCHEME_TRUEP(parent)) {
    scheme_contract_error("prop:checked-procedure",
                          "not allowed on a structure type with a supertype",
                          NULL);
  }

  if (num_islots + num_aslots < 2) {
    scheme_contract_error("prop:checked-procedure",
                          "need at least two fields in the structure type",
                          NULL);
  }

  return scheme_true;
}

Scheme_Object *scheme_extract_checked_procedure(int argc, Scheme_Object **argv)
{
  Scheme_Struct_Type *stype;
  Scheme_Object *v, *checker, *proc, *a[3];
  
  v = argv[1];

  if (SCHEME_STRUCT_TYPEP(argv[0]))
    stype = (Scheme_Struct_Type *)argv[0];  
  else
    stype = NULL;

  if (!stype || !(MZ_OPT_HASH_KEY(&stype->iso) & STRUCT_TYPE_CHECKED_PROC)) {
    scheme_wrong_type("checked-procedure-check-and-extract", "unchaperoned structure type with prop:checked-procedure property",
                      0, argc, argv);
    return NULL;
  }

  /* let chaperones use the slow path, for now */
  if (SCHEME_STRUCTP(v) && scheme_is_struct_instance((Scheme_Object *)stype, v)) {
    checker = ((Scheme_Structure *)v)->slots[0];
    proc = ((Scheme_Structure *)v)->slots[1];
    
    a[0] = argv[3];
    a[1] = argv[4];
    v = _scheme_apply(checker, 2, a);
    
    if (SCHEME_TRUEP(v))
      return proc;
  }

  a[0] = argv[1];
  a[1] = argv[3];
  a[2] = argv[4];
  return _scheme_apply(argv[2], 3, a);
}

/*========================================================================*/
/*                             liberal-define                             */
/*========================================================================*/

static Scheme_Object *is_liberal_def_ctx(int argc, Scheme_Object **argv, Scheme_Object *self)
{
  Scheme_Object *prop = SCHEME_PRIM_CLOSURE_ELS(self)[0], *val;
  
  val = scheme_struct_type_property_ref(prop, argv[0]);

  if (!val || SCHEME_FALSEP(val))
    return scheme_false;
  else
    return scheme_true;
}

/*========================================================================*/
/*                             struct ops                                 */
/*========================================================================*/

static void wrong_struct_type(char *name, 
			      Scheme_Object *expected,
			      Scheme_Object *received,
			      int which, int argc,
			      Scheme_Object **argv)
{
  if (SAME_OBJ(expected, received))
    scheme_contract_error(name,
                          "contract violation;\n"
                          " given value instantiates a different structure type with the same name",
                          "expected", 0, pred_name_string(expected),
                          "given", 1, argv[which],
                          NULL);
  else
    scheme_wrong_contract(name,
                          pred_name_string(expected), 
                          which, argc, argv);
}

#define STRUCT_TYPEP(st, v) \
        ((st->name_pos <= v->stype->name_pos) \
	 && (st == v->stype->parent_types[st->name_pos]))

int scheme_is_struct_instance(Scheme_Object *type, Scheme_Object *v)
{
  Scheme_Struct_Type *stype = (Scheme_Struct_Type *)type;
  Scheme_Structure *s = (Scheme_Structure *)v;

  return STRUCT_TYPEP(stype, s);
}

static Scheme_Object *chaperone_struct_ref(const char *who, Scheme_Object *prim,
                                           Scheme_Object *orig_o, Scheme_Object *o, int i);

static Scheme_Object *chaperone_struct_ref_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object *prim = (Scheme_Object *)p->ku.k.p3;
  const char *who = (const char *)p->ku.k.p2;
  Scheme_Object *orig_o = (Scheme_Object *)p->ku.k.p4;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  return chaperone_struct_ref(who, prim, orig_o, o, p->ku.k.i1);
}

static Scheme_Object *chaperone_struct_ref_overflow(const char *who, Scheme_Object *prim, 
                                                    Scheme_Object *orig_o, Scheme_Object *o, int i)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = (void *)o;
  p->ku.k.p2 = (void *)who;
  p->ku.k.p3 = (void *)prim;
  p->ku.k.p4 = (void *)orig_o;
  p->ku.k.i1 = i;

  return scheme_handle_stack_overflow(chaperone_struct_ref_k);
}

static void raise_undefined_error(const char *who, Scheme_Object *prim, Scheme_Object *val,
                                  const char *short_error, const char *mode, int i)
{
  int len;
  Scheme_Object *o;

  o = scheme_struct_type_property_ref(scheme_chaperone_undefined_property, val);
  len = (o ? scheme_proper_list_length(o) : 0);
  if (i < len) {
    for (i = len - i; --i; ) {
      o = SCHEME_CDR(o);
    }
    o = SCHEME_CAR(o);
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_VARIABLE,
                     o,
                     "%S: %s;\n cannot %s field before initialization",
                     o, short_error, mode);
  } else {
    if (prim)
      who = extract_field_proc_name(prim);

    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                     "%s: %s;\n cannot %s field before initialization",
                     who,
                     short_error, mode);
  }

}

static Scheme_Object *chaperone_struct_ref(const char *who, Scheme_Object *prim, 
                                           Scheme_Object *orig_o, Scheme_Object *o, int i)
{
  while (1) {
    if (!SCHEME_CHAPERONEP(o)) {
      return ((Scheme_Structure *)o)->slots[i];
    } else {
      Scheme_Chaperone *px = (Scheme_Chaperone *)o;
      Scheme_Object *a[2], *red, *orig;

      if (SCHEME_VECTORP(px->redirects)
          && !(SCHEME_VEC_SIZE(px->redirects) & 1)
          && SAME_OBJ(SCHEME_VEC_ELS(px->redirects)[1], scheme_undefined)) {
        /* chaperone on every field: check that result is not undefined */
        o = px->prev;
        
        if (!SCHEME_CHAPERONEP(o))
          orig = ((Scheme_Structure *)o)->slots[i];
        else
          orig = chaperone_struct_ref(who, prim, orig_o, o, i);

        if (SAME_OBJ(orig, scheme_undefined)) {
          raise_undefined_error(who, prim, px->val, "undefined", "use", i);
        }

        return orig;
      } else if (!SCHEME_VECTORP(px->redirects)
          || (SCHEME_VEC_SIZE(px->redirects) & 1)
          || SCHEME_FALSEP(SCHEME_VEC_ELS(px->redirects)[PRE_REDIRECTS + i])) {
        o = px->prev;
      } else {
#ifdef DO_STACK_CHECK
        {
# include "mzstkchk.h"
          return chaperone_struct_ref_overflow(who, prim, orig_o, o, i);
        }
#endif

        red = SCHEME_VEC_ELS(px->redirects)[PRE_REDIRECTS + i];
        if (SCHEME_PAIRP(red)) {
          /* Operation used to chaperone the struct was itself chaperoned.
             Use the chaperoned operation to get the result to chaperone
             further. */
          a[0] = px->prev;
          orig = _scheme_apply(SCHEME_CAR(red), 1, a);
          red = SCHEME_CDR(red);
        } else
          orig = chaperone_struct_ref(who, prim, orig_o, px->prev, i);

        a[0] = orig_o;
        a[1] = orig;
        if (SAME_TYPE(SCHEME_TYPE(red), scheme_native_closure_type)) {
          o = _scheme_apply_native(red, 2, a);
          if (o == SCHEME_MULTIPLE_VALUES) {
            GC_CAN_IGNORE Scheme_Thread *p = scheme_current_thread;
            scheme_wrong_return_arity(NULL, 1, p->ku.multiple.count, 
                                      p->ku.multiple.array,
                                      NULL);
            return NULL;
          }
        } else
          o = _scheme_apply(red, 2, a);
        
        if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR))
          if (!SAME_OBJ(o, orig) && !scheme_chaperone_of(o, orig))
            scheme_wrong_chaperoned(who, "result", orig, o);
        
        return o;
      }
    }
  }
}

Scheme_Object *scheme_struct_ref(Scheme_Object *sv, int pos)
{
  if (SCHEME_CHAPERONEP(sv)) {
    return chaperone_struct_ref("struct-ref", NULL, sv, sv, pos);
  } else {
    Scheme_Structure *s = (Scheme_Structure *)sv;
    
    return s->slots[pos];
  }
}

static void chaperone_struct_set(const char *who, Scheme_Object *prim,
                                 Scheme_Object *o, int i, Scheme_Object *v);

static Scheme_Object *chaperone_struct_set_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object *prim = (Scheme_Object *)p->ku.k.p3;
  const char *who = (const char *)p->ku.k.p2;
  Scheme_Object *v = (Scheme_Object *)p->ku.k.p4;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  chaperone_struct_set(who, prim, o, p->ku.k.i1, v);

  return scheme_false;
}

static void chaperone_struct_set_overflow(const char *who, Scheme_Object *prim, 
                                          Scheme_Object *o, int i, Scheme_Object *v)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = (void *)o;
  p->ku.k.p2 = (void *)who;
  p->ku.k.p3 = (void *)prim;
  p->ku.k.p4 = (void *)v;
  p->ku.k.i1 = i;

  (void)scheme_handle_stack_overflow(chaperone_struct_set_k);
}

static void chaperone_struct_set(const char *who, Scheme_Object *prim,
                                 Scheme_Object *o, int i, Scheme_Object *v)
{
  Scheme_Object *orig_o = o;

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    chaperone_struct_set_overflow(who, prim, o, i, v);
    return;
  }
#endif

  while (1) {
    if (!SCHEME_CHAPERONEP(o)) {
      ((Scheme_Structure *)o)->slots[i] = v;
      return;
    } else {
      Scheme_Chaperone *px = (Scheme_Chaperone *)o;
      Scheme_Object *a[2], *red;
      int half;

      o = px->prev;
      if (SCHEME_VECTORP(px->redirects)
          && !(SCHEME_VEC_SIZE(px->redirects) & 1)
          && !SAME_OBJ(SCHEME_VEC_ELS(px->redirects)[1], scheme_undefined)) {
        half = (SCHEME_VEC_SIZE(px->redirects) - PRE_REDIRECTS) >> 1;
        red = SCHEME_VEC_ELS(px->redirects)[PRE_REDIRECTS + half + i];
        if (SCHEME_TRUEP(red)) {
          Scheme_Object *finish_setter = NULL;

          a[0] = orig_o;
          a[1] = v;

          if (SCHEME_PAIRP(red)) {
            finish_setter = SCHEME_CAR(red);
            red = SCHEME_CDR(red);
          }

          if (SAME_TYPE(SCHEME_TYPE(red), scheme_native_closure_type)) {
            v = _scheme_apply_native(red, 2, a);
            if (v == SCHEME_MULTIPLE_VALUES) {
              GC_CAN_IGNORE Scheme_Thread *p = scheme_current_thread;
              scheme_wrong_return_arity(NULL, 1, p->ku.multiple.count, 
                                        p->ku.multiple.array,
                                        NULL);
            }
          } else
            v = _scheme_apply(red, 2, a);
          
          if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR))
            if (!SAME_OBJ(v, a[1]) && !scheme_chaperone_of(v, a[1]))
              scheme_wrong_chaperoned(who, "value", a[1], v);

          if (finish_setter) {
            /* Operation used to chaperone the struct was itself chaperoned.
               Use the chaperoned operation to finish the assignment. */
            a[0] = o;
            a[1] = v;
            (void)_scheme_apply_multi(finish_setter, 2, a);
            return;
          }
        } 
      } else if (SCHEME_VECTORP(px->redirects)
                 && !(SCHEME_VEC_SIZE(px->redirects) & 1)
                 && SAME_OBJ(SCHEME_VEC_ELS(px->redirects)[1], scheme_undefined)) {
        /* chaperone on every field: check that current value is not undefined
           --- unless check is disabled by a mark (bit it's faster to check
           for `undefined` before checking the mark) */
        if (SAME_OBJ(scheme_undefined, ((Scheme_Structure *)px->val)->slots[i])) {
          Scheme_Object *m;
          
          m = scheme_extract_one_cc_mark(NULL, scheme_chaperone_undefined_property);
          if (!m || !SAME_OBJ(m, scheme_undefined))
            raise_undefined_error(who, prim, px->val, "assignment disallowed", "assign", i);
        }
      }
    }
  }
}

void scheme_struct_set(Scheme_Object *sv, int pos, Scheme_Object *v)
{
  if (SCHEME_CHAPERONEP(sv)) {
    chaperone_struct_set("struct-set!", NULL, sv, pos, v);
  } else {
    Scheme_Structure *s = (Scheme_Structure *)sv;  
    
    s->slots[pos] = v;
  }
}

int scheme_is_noninterposing_chaperone(Scheme_Object *o)
/* Checks whether the immediate impersonator layer for `o` is known to
   interpose on no operations (i.e., it's for impersonator properties,
   only) */
{
  Scheme_Chaperone *px = (Scheme_Chaperone *)o;  
  int i;

  if (!SCHEME_VECTORP(px->redirects))
    return 0;

  if (SCHEME_VEC_SIZE(px->redirects) & 1) {
    /* procedure chaperone */
    if (SCHEME_FALSEP(SCHEME_VEC_ELS(px->redirects)[1]))
      return 1;
    return 0;
  }

  if (SCHEME_TRUEP(SCHEME_VEC_ELS(px->redirects)[0]))
    return 0;

  for (i = SCHEME_VEC_SIZE(px->redirects); i-- > PRE_REDIRECTS; ) {
    if (!SCHEME_FALSEP(SCHEME_VEC_ELS(px->redirects)[i]))
      return 0;
  }

  return 1;
}

static Scheme_Object **apply_guards(Scheme_Struct_Type *stype, int argc, Scheme_Object **args,
                                    int *_chaperone_undefined)
{
  Scheme_Object **guard_argv = NULL, *v, *prev_guards = NULL, *guard;
  int p, gcount;

  for (p = stype->name_pos; p >= 0; p--) {
    if (stype->parent_types[p]->guard || prev_guards) {
      int got;

      if (!guard_argv) {
	guard_argv = MALLOC_N(Scheme_Object *, argc + 1);
	memcpy(guard_argv, args, sizeof(Scheme_Object *) * argc);
	args = guard_argv;
      }

      if (!prev_guards)
        prev_guards = scheme_null;
      while (prev_guards) {
        if (SCHEME_PAIRP(prev_guards))
          guard = SCHEME_CAR(prev_guards);
        else {
          guard = stype->parent_types[p]->guard;
          /* In case there are chaperone-added guards: */
          if (guard) {
            if (SCHEME_PAIRP(guard)) guard = SCHEME_CAR(guard);
          } else
            guard = scheme_false;
        }

        if (SAME_OBJ(guard, scheme_undefined))
          *_chaperone_undefined = 1;
        else if (!SCHEME_FALSEP(guard)) {
          gcount = stype->parent_types[p]->num_islots;
          guard_argv[argc] = guard_argv[gcount];
          guard_argv[gcount] = stype->name;
          v = _scheme_apply_multi(guard, gcount + 1, guard_argv);
          got = (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES) ? scheme_multiple_count : 1);
          if (gcount != got) {
            scheme_wrong_return_arity("constructor",
                                      gcount, got, 
                                      (got == 1) ? (Scheme_Object **)v : scheme_multiple_array,
                                      "calling guard procedure");
            return NULL;
          }
          if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES))
            memcpy(guard_argv, scheme_multiple_array, gcount * sizeof(Scheme_Object *));
          else
            guard_argv[0] = v;
          guard_argv[gcount] = guard_argv[argc];
        }

        if (SCHEME_NULLP(prev_guards))
          prev_guards = NULL;
        else
          prev_guards = SCHEME_CDR(prev_guards);
      }
    }

    /* Any chaperone-imposed guards for the next layer down? */
    if (stype->parent_types[p]->guard
        && SCHEME_PAIRP(stype->parent_types[p]->guard))
      prev_guards = SCHEME_CDR(stype->parent_types[p]->guard);
  }

  return args;
}

Scheme_Object *
scheme_make_struct_instance(Scheme_Object *_stype, int argc, Scheme_Object **args)
{
  Scheme_Structure *inst;
  Scheme_Struct_Type *stype;
  int p, i, j, nis, ns, c;
  int chaperone_undefined = 0;

  stype = (Scheme_Struct_Type *)_stype;

  c = stype->num_slots;
  inst = (Scheme_Structure *)
    scheme_malloc_tagged(sizeof(Scheme_Structure) 
			 + ((c - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
  
  inst->so.type = (stype->proc_attr ? scheme_proc_struct_type : scheme_structure_type);
  inst->stype = stype;

  /* Apply guards, if any: */
  args = apply_guards(stype, argc, args, &chaperone_undefined);
  
  /* Fill in fields: */
  j = c;
  i = argc;
  for (p = stype->name_pos; p >= 0; p--) {
    /* Determine which fields are automatic: */
    if (p) {
      ns = stype->parent_types[p]->num_slots - stype->parent_types[p - 1]->num_slots;
      nis = stype->parent_types[p]->num_islots - stype->parent_types[p - 1]->num_islots;
    } else {
      ns = stype->parent_types[0]->num_slots;
      nis = stype->parent_types[0]->num_islots;
    }

    ns -= nis;

    /* Fill in automatics: */
    while (ns--) {
      inst->slots[--j] = stype->parent_types[p]->uninit_val;
    }

    /* Fill in supplied: */
    while (nis--) {
      inst->slots[--j] = args[--i];
    }
  }

  if (chaperone_undefined)
    return scheme_chaperone_not_undefined((Scheme_Object *)inst);
  else
    return (Scheme_Object *)inst;
}

Scheme_Object *scheme_make_blank_prefab_struct_instance(Scheme_Struct_Type *stype)
{
  Scheme_Structure *inst;
  int c;

  c = stype->num_slots;
  inst = (Scheme_Structure *)
    scheme_malloc_tagged(sizeof(Scheme_Structure) 
			 + ((c - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
  
  inst->so.type = scheme_structure_type;
  inst->stype = stype;
  
  return (Scheme_Object *)inst;
}

#ifdef MZ_USE_PLACES
Scheme_Object *scheme_make_serialized_struct_instance(Scheme_Object *prefab_key, int num_slots)
{
  Scheme_Serialized_Structure *inst;

  inst = (Scheme_Serialized_Structure *)
    scheme_malloc_tagged(sizeof(Scheme_Serialized_Structure) 
			 + ((num_slots - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
  
  inst->so.type = scheme_serialized_structure_type;
  inst->num_slots = num_slots;
  inst->prefab_key = prefab_key;

  return (Scheme_Object *)inst;
}
#endif

Scheme_Object *scheme_make_prefab_struct_instance(Scheme_Struct_Type *stype,
                                                         Scheme_Object *vec)
{
  Scheme_Structure *inst;
  int i, c;

  c = stype->num_slots;
  inst = (Scheme_Structure *)
    scheme_malloc_tagged(sizeof(Scheme_Structure) 
			 + ((c - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
  
  inst->so.type = scheme_structure_type;
  inst->stype = stype;
  
  for (i = 0; i < c; i++) {
    inst->slots[i] = SCHEME_VEC_ELS(vec)[i + 1];
  }

  return (Scheme_Object *)inst;
}

Scheme_Object *scheme_clone_prefab_struct_instance(Scheme_Structure *s)
{
  Scheme_Object *chaperone, *v;
  Scheme_Structure *inst;
  int c, sz, i;

  if (SCHEME_CHAPERONEP((Scheme_Object *)s)) {
    chaperone = (Scheme_Object *)s;
    s = (Scheme_Structure *)SCHEME_CHAPERONE_VAL(chaperone);
  } else
    chaperone = NULL;

  c = s->stype->num_slots;
  sz = (sizeof(Scheme_Structure) 
        + ((c - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
  inst = (Scheme_Structure *)scheme_malloc_tagged(sz);
  memcpy(inst, s, sz);

  if (chaperone) {
    for (i = 0; i < c; i++) {
      v = scheme_struct_ref(chaperone, i);
      inst->slots[i] = v;
    }
  }
  
  return (Scheme_Object *)inst;
}

static Scheme_Object *
make_struct_instance(int argc, Scheme_Object **args, Scheme_Object *prim)
{
  return scheme_make_struct_instance(SCHEME_PRIM_CLOSURE_ELS(prim)[0], argc, args);
}

static Scheme_Object *
make_simple_struct_instance(int argc, Scheme_Object **args, Scheme_Object *prim)
/* No guards, uninitialized slots, or proc type */
{
  Scheme_Structure *inst;
  Scheme_Struct_Type *stype = (Scheme_Struct_Type *)SCHEME_PRIM_CLOSURE_ELS(prim)[0];
  int i, c;

  c = stype->num_slots;
  inst = (Scheme_Structure *)
    scheme_malloc_tagged(sizeof(Scheme_Structure) 
			 + ((c - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
  
  inst->so.type = scheme_structure_type;
  inst->stype = stype;

  for (i = 0; i < argc; i++) {
    inst->slots[i] = args[i];
  }
  
  return (Scheme_Object *)inst;
}

int scheme_is_simple_struct_type(Scheme_Struct_Type *stype)
{
  int p;

  if (stype->proc_attr)
    return 0;

  for (p = stype->name_pos; p >= 0; p--) {
    if (stype->parent_types[p]->guard)
      return 0;
    if (stype->parent_types[p]->num_slots != stype->parent_types[p]->num_islots)
      return 0;
  }

  return 1;
}

static Scheme_Object *struct_pred(int argc, Scheme_Object **args, Scheme_Object *prim)
{
  Scheme_Object *v = args[0];

  if (SCHEME_CHAPERONEP(v)) v = SCHEME_CHAPERONE_VAL(v);

  if (SCHEME_STRUCTP(v)) {
    Scheme_Struct_Type *stype = (Scheme_Struct_Type *)SCHEME_PRIM_CLOSURE_ELS(prim)[0];
    if (STRUCT_TYPEP(stype, ((Scheme_Structure *)v)))
      return scheme_true;
  }

  return scheme_false;
}

static int parse_pos(const char *who, Scheme_Object *prim, Scheme_Object **args, int argc)
{
  int pos;
  Scheme_Struct_Type *st = (Scheme_Struct_Type *)SCHEME_PRIM_CLOSURE_ELS(prim)[0];

  if (!SCHEME_INTP(args[1]) || (SCHEME_INT_VAL(args[1]) < 0)) {
    if (SCHEME_BIGNUMP(args[1]) && SCHEME_BIGPOS(args[1])) {
      pos = 32769; /* greater than max field count */
    } else {
      if (!who)
	who = extract_field_proc_name(prim);
      scheme_wrong_contract(who, 
                            "exact-nonnegative-integer?", 
                            1, argc, args);
      return 0;
    }
  } else
    pos = SCHEME_INT_VAL(args[1]);
  
  if ((pos < st->num_slots)
      && st->name_pos)
    pos += st->parent_types[st->name_pos - 1]->num_slots;
  
  if (pos >= st->num_slots) {
    int sc;

    if (!who)
      who = extract_field_proc_name(prim);

    sc = (st->name_pos
	  ? (st->num_slots
	     - st->parent_types[st->name_pos - 1]->num_slots)
	  : st->num_slots);

    scheme_contract_error(who,
                          "index too large",
                          "index", 1, args[1],
                          "maximum allowed index", 1, scheme_make_integer(sc-1),
                          "structure", 1, args[0],
                          NULL);

    return 0;
  }

  return pos;
}

Scheme_Object *scheme_struct_getter(int argc, Scheme_Object **args, Scheme_Object *prim)
{
  Scheme_Structure *inst;
  int pos;
  Scheme_Struct_Type *st = (Scheme_Struct_Type *)SCHEME_PRIM_CLOSURE_ELS(prim)[0];

  inst = (Scheme_Structure *)args[0];
  if (SCHEME_CHAPERONEP(((Scheme_Object *)inst)))
    inst = (Scheme_Structure *)SCHEME_CHAPERONE_VAL((Scheme_Object *)inst);

  if (!SCHEME_STRUCTP(((Scheme_Object *)inst))) {
    scheme_wrong_contract(extract_field_proc_name(prim), 
                          pred_name_string(st->name), 
                          0, argc, args);
    return NULL;
  } else if (!STRUCT_TYPEP(st, inst)) {
    wrong_struct_type(extract_field_proc_name(prim), 
		      st->name, 
		      SCHEME_STRUCT_NAME_SYM(inst), 
		      0, argc, args);
    return NULL;
  }
  
  if (argc == 2)
    pos = parse_pos(NULL, prim, args, argc);
  else
    pos = SCHEME_INT_VAL(SCHEME_PRIM_CLOSURE_ELS(prim)[1]);

  if (SAME_OBJ((Scheme_Object *)inst, args[0]))
    return inst->slots[pos];
  else
    return chaperone_struct_ref("struct-ref", prim, args[0], args[0], pos);
}

Scheme_Object *scheme_struct_setter(int argc, Scheme_Object **args, Scheme_Object *prim)
{
  Scheme_Structure *inst;
  int pos;
  Scheme_Object *v;
  Scheme_Struct_Type *st = (Scheme_Struct_Type *)SCHEME_PRIM_CLOSURE_ELS(prim)[0];

  inst = (Scheme_Structure *)args[0];
  if (SCHEME_CHAPERONEP(((Scheme_Object *)inst)))
    inst = (Scheme_Structure *)SCHEME_CHAPERONE_VAL((Scheme_Object *)inst);

  if (!SCHEME_STRUCTP(((Scheme_Object *)inst))) {
    scheme_wrong_contract(extract_field_proc_name(prim),
                          pred_name_string(st->name), 
                          0, argc, args);
    return NULL;
  }
	
  if (!STRUCT_TYPEP(st, inst)) {
    wrong_struct_type(extract_field_proc_name(prim),
		      st->name, 
		      SCHEME_STRUCT_NAME_SYM(inst),
		      0, argc, args);
    return NULL;
  }
	
  if (argc == 3) {
    pos = parse_pos(NULL, prim, args, argc);
    v = args[2];
  } else {
    pos = SCHEME_INT_VAL(SCHEME_PRIM_CLOSURE_ELS(prim)[1]);
    v = args[1];
  }

  if (st->immutables) {
    int p = pos;

    if (st->name_pos)
      p -= st->parent_types[st->name_pos - 1]->num_slots;
    
    if (st->immutables[p]) {
      scheme_contract_error(extract_field_proc_name(prim),
                            "cannot modify value of immutable field in structure", 
                            "structure", 1, args[0],
                            "field index", 1, scheme_make_integer(pos),
                            NULL);
      return NULL;
    }
  }

  if (SAME_OBJ((Scheme_Object *)inst, args[0]))
    inst->slots[pos] = v;
  else
    chaperone_struct_set("struct-set!", prim, args[0], pos, v);
  
  return scheme_void;
}

static Scheme_Object *
struct_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  if (SCHEME_CHAPERONEP(v))
    v = SCHEME_CHAPERONE_VAL(v);

  if (SCHEME_STRUCTP(v)) {
    Scheme_Object *insp;
    insp = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
    if (scheme_inspector_sees_part(v, insp, -1))
      return scheme_true;
    else
      return scheme_false;
  } else 
    return scheme_false;
}

static Scheme_Object *
struct_type_p(int argc, Scheme_Object *argv[])
{
  return (SCHEME_CHAPERONE_STRUCT_TYPEP(argv[0])
          ? scheme_true : scheme_false);
}

static Scheme_Object *proc_struct_type_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  if (SCHEME_NP_CHAPERONEP(v))
    v = SCHEME_CHAPERONE_VAL(v);

  if (SCHEME_STRUCT_TYPEP(v)) {
    if (((Scheme_Struct_Type *)v)->proc_attr)
      return scheme_true;
    else
      return scheme_false;
  }
  scheme_wrong_contract("procedure-struct-type?", "struct-type?", 0, argc, argv);
  return NULL;
}

static Scheme_Object *apply_chaperones(const char *who, Scheme_Object *procs, int argc, Scheme_Object **a)
{
  Scheme_Object *v, **vals, *v1[1];
  int cnt, i, is_impersonator;
  Scheme_Thread *p;

  while (SCHEME_PAIRP(procs)) {
    v = SCHEME_CAR(procs);
    if (SCHEME_BOXP(v)) {
      is_impersonator = 1;
      v = SCHEME_BOX_VAL(v);
    } else
      is_impersonator = 0;

    v = _scheme_apply_multi(v, argc, a);

    if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
      p = scheme_current_thread;
      cnt = p->ku.multiple.count;
      vals = p->ku.multiple.array;
      p->ku.multiple.array = NULL;
      if (SAME_OBJ(vals, p->values_buffer))
        p->values_buffer = NULL;
      p = NULL;
    } else {
      v1[0] = v;
      vals = v1;
      cnt = 1;
    }

    if (cnt != argc) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_ARITY,
                       "%s: arity mismatch;\n"
                       " received wrong number of values from %s replacement procedure\n"
                       "  expected: %d\n"
                       "  received: %d\n"
                       "  %s: %V\n",
                       who,
                       is_impersonator ? "an impersonator's" : "an chaperone's",
                       SCHEME_CAR(procs),
                       argc, cnt,
                       is_impersonator ? "impersonator" : "chaperone");
    }

    if (!is_impersonator) {
      for (i = 0; i < argc; i++) {
        if (!scheme_chaperone_of(vals[i], a[i]))
          scheme_wrong_chaperoned(who, "result", a[i], vals[i]);
      }
    }

    a = vals;
    procs = SCHEME_CDR(procs);
  }

  return scheme_values(argc, a);
}

static Scheme_Object *struct_info_chaperone(Scheme_Object *o, Scheme_Object *si, Scheme_Object *b)
{
  Scheme_Object *procs = scheme_null, *proc, *a[2];
  Scheme_Chaperone *px;

  while (SCHEME_CHAPERONEP(o)) {
    px = (Scheme_Chaperone *)o;
    if (SCHEME_VECTORP(px->redirects)
        && !(SCHEME_VEC_SIZE(px->redirects) & 1)) {
      proc = SCHEME_VEC_ELS(px->redirects)[1];
      if (SCHEME_TRUEP(proc) && !SAME_OBJ(proc, scheme_undefined)) {
        if (SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR)
          proc = scheme_box(proc);
        procs = scheme_make_pair(proc, procs);
      }
    }
    o = px->prev;
  }

  a[0] = si;
  a[1] = b;
  
  return apply_chaperones("struct-info", procs, 2, a);
}

static Scheme_Object *struct_info(int argc, Scheme_Object *argv[])
{
  Scheme_Structure *s;
  Scheme_Struct_Type *stype;
  int p;
  Scheme_Object *insp, *a[2];
  Scheme_Object *v = argv[0];
  
  if (SCHEME_CHAPERONEP(v))
    v = SCHEME_CHAPERONE_VAL(v);

  if (SCHEME_STRUCTP(v)) {
    s = (Scheme_Structure *)v;

    insp = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
    
    stype = s->stype;
    p = stype->name_pos + 1;
    
    while (p--) {
      stype = stype->parent_types[p];
      if (scheme_is_subinspector(stype->inspector, insp)) {
        a[0] = (Scheme_Object *)stype;
        a[1] = ((SAME_OBJ(stype, s->stype)) ? scheme_false : scheme_true);
        
        if (!SAME_OBJ(v, argv[0]))
          return struct_info_chaperone(argv[0], a[0], a[1]);
        else
          return scheme_values(2, a);
      }
    }
  }
  
  a[0] = scheme_false;
  a[1] = scheme_true;

  return scheme_values(2, a);
}

static Scheme_Object *check_type_and_inspector(const char *who, int always, int argc, Scheme_Object *argv[])
{
  Scheme_Object *insp, *val;
  Scheme_Struct_Type *stype;

  val = argv[0];
  if (SCHEME_NP_CHAPERONEP(val))
    val = SCHEME_CHAPERONE_VAL(val);

  if (!SCHEME_STRUCT_TYPEP(val))
    scheme_wrong_contract(who, "struct-type?", 0, argc, argv);

  stype = (Scheme_Struct_Type *)val;

  insp = scheme_get_current_inspector();

  if (!always && !scheme_is_subinspector(stype->inspector, insp)) {
    scheme_contract_error(who, 
                          "current inspector cannot extract info for structure type",
                          "structure type", 1, argv[0], 
                          NULL);
    return NULL;
  }

  return insp;
}

void scheme_force_struct_type_info(Scheme_Struct_Type *stype)
{
  /* Make sure generic accessor and mutator are created: */
  if (!stype->accessor) {
    Scheme_Object *p;
    char *fn;
    
    fn = (char *)GENGET_NAME((char *)stype->name, -1, 0);
    p = make_struct_proc(stype, fn, SCHEME_GEN_GETTER, 0);
    stype->accessor = p;
    fn = (char *)GENSET_NAME((char *)stype->name, -1, 0);
    p = make_struct_proc(stype, fn, SCHEME_GEN_SETTER, 0);
    stype->mutator = p;
  }
}

static void get_struct_type_info(int argc, Scheme_Object *argv[], Scheme_Object **a, int always)
{
  Scheme_Struct_Type *stype, *parent;
  Scheme_Object *insp, *ims;
  int p, cnt;

  insp = check_type_and_inspector("struct-type-info", always, argc, argv);
  if (SCHEME_NP_CHAPERONEP(argv[0]))
    stype = (Scheme_Struct_Type *)SCHEME_CHAPERONE_VAL(argv[0]);
  else
    stype = (Scheme_Struct_Type *)argv[0];

  scheme_force_struct_type_info(stype);

  if (stype->name_pos)
    parent = stype->parent_types[stype->name_pos - 1];
  else
    parent = NULL;

  a[0] = stype->name;
  cnt = stype->num_islots - (parent ? parent->num_islots : 0);
  a[1] = scheme_make_integer(cnt);
  a[2] = scheme_make_integer(stype->num_slots - (parent ? parent->num_slots : 0) - cnt);
  a[3] = stype->accessor;
  a[4] = stype->mutator;

  p = stype->name_pos;
  while (--p >= 0) {
    if (scheme_is_subinspector(stype->parent_types[p]->inspector, insp)) {
      break;
    }
  }

  ims = scheme_null;
  if (stype->immutables) {
    int i;
    for (i = stype->num_islots - (parent ? parent->num_islots : 0); i--; ) {
      if (stype->immutables[i])
	ims = scheme_make_pair(scheme_make_integer(i), ims);
    }
  }
  a[5] = ims;

  a[6] = ((p >= 0) ? (Scheme_Object *)stype->parent_types[p] : scheme_false);
  a[7] = ((p == stype->name_pos - 1) ? scheme_false : scheme_true);
}

static Scheme_Object *struct_type_info_chaperone(Scheme_Object *o, Scheme_Object **a)
{
  Scheme_Object *procs = scheme_null, *proc;
  Scheme_Chaperone *px;

  while (SCHEME_NP_CHAPERONEP(o)) {
    px = (Scheme_Chaperone *)o;
    if (SCHEME_PAIRP(px->redirects)) {
      proc = SCHEME_CAR(px->redirects);
      if (SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR)
        proc = scheme_box(proc);
      procs = scheme_make_pair(proc, procs);
    }
    o = px->prev;
  }

  return apply_chaperones("struct-type-info", procs, mzNUM_ST_INFO, a);
}

static Scheme_Object *struct_type_info(int argc, Scheme_Object *argv[])
{
  Scheme_Object *a[mzNUM_ST_INFO];

  get_struct_type_info(argc, argv, a, 0);

  if (SCHEME_NP_CHAPERONEP(argv[0])) {
    return struct_type_info_chaperone(argv[0], a);
  }

  return scheme_values(mzNUM_ST_INFO, a);
}

static Scheme_Object *struct_type_pred(int argc, Scheme_Object *argv[])
{
  Scheme_Struct_Type *stype;
  char *name;

  check_type_and_inspector("struct-type-make-predicate", 0, argc, argv);
  if (SCHEME_NP_CHAPERONEP(argv[0]))
    stype = (Scheme_Struct_Type *)SCHEME_CHAPERONE_VAL(argv[0]);
  else
    stype = (Scheme_Struct_Type *)argv[0];

  name = (char *)PRED_NAME(scheme_symbol_val(stype->name),
                           SCHEME_SYM_LEN(stype->name),
                           0);

  return make_struct_proc(stype, name, SCHEME_PRED, stype->num_slots);
}

static Scheme_Object *type_constr_chaperone(Scheme_Object *o, Scheme_Object *v)
{
  Scheme_Object *procs = scheme_null, *proc, *a[1];
  Scheme_Chaperone *px;

  while (SCHEME_NP_CHAPERONEP(o)) {
    px = (Scheme_Chaperone *)o;
    if (SCHEME_PAIRP(px->redirects)) {
      proc = SCHEME_CADR(px->redirects);
      if (SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR)
        proc = scheme_box(proc);
      procs = scheme_make_pair(proc, procs);
    }
    o = px->prev;
  }

  a[0] = v;
  return apply_chaperones("struct-type-make-constructor", procs, 1, a);
}

static Scheme_Object *struct_type_constr(int argc, Scheme_Object *argv[])
{
  Scheme_Struct_Type *stype;
  Scheme_Object *v;

  check_type_and_inspector("struct-type-make-constructor", 0, argc, argv);
  if (SCHEME_NP_CHAPERONEP(argv[0]))
    stype = (Scheme_Struct_Type *)SCHEME_CHAPERONE_VAL(argv[0]);
  else
    stype = (Scheme_Struct_Type *)argv[0];

  if ((argc < 2) || SCHEME_FALSEP(argv[1]))
    v = CSTR_MAKE_NAME(scheme_symbol_val(stype->name), SCHEME_SYM_LEN(stype->name), 1);
  else if (SCHEME_SYMBOLP(argv[1]))
    v = argv[1];
  else {
    scheme_wrong_contract("struct-type-make-constructor", "symbol?", 1, argc, argv);
    return NULL;
  }
  
  v = make_struct_proc(stype, 
                       scheme_symbol_val(v),
                       SCHEME_CONSTR,
                       stype->num_slots);

  if (SCHEME_NP_CHAPERONEP(argv[0]))
    return type_constr_chaperone(argv[0], v);

  return v;
}

Scheme_Object *scheme_struct_to_vector(Scheme_Object *_s, Scheme_Object *unknown_val, Scheme_Object *insp)
{
  Scheme_Structure *s;
  Scheme_Struct_Type *stype;
  Scheme_Object *v, *elem, *name;
  GC_CAN_IGNORE Scheme_Object **array;
  int i, m, p, n, last_is_unknown;

  if (!unknown_val)
    unknown_val = ellipses_symbol;

  if (SCHEME_CHAPERONEP(_s))
    s = (Scheme_Structure *)SCHEME_CHAPERONE_VAL(_s);
  else
    s = (Scheme_Structure *)_s;
  
  stype = s->stype;
  p = stype->name_pos + 1;
  m = 0;
  last_is_unknown = 0;
  while (p--) {
    stype = stype->parent_types[p];
    if (!scheme_is_subinspector(stype->inspector, insp)) {
      if (!last_is_unknown)
	m++;
      last_is_unknown = 1;
    } else {
      int count;
      if (p)
	count = stype->num_slots - stype->parent_types[p-1]->num_slots;
      else
	count = stype->num_slots;
      m += count;
      if (count)
        last_is_unknown = 0;
    }
  }

  stype = s->stype;
  p = stype->name_pos + 1;
  i = stype->num_slots;
  last_is_unknown = 0;
 
  name = TYPE_NAME((char *)SCHEME_STRUCT_NAME_SYM(s), -1, 1);

  /* Precise GC >>> BEWARE <<<, array is not GC_aligned,
     and is therefore marked with GC_CAN_IGNORE. */

  v = scheme_make_vector(m + 1, NULL);
  array = SCHEME_VEC_ELS(v);
  array[0] = name;
  while (p--) {
    stype = stype->parent_types[p];
    if (p)
      n = stype->num_slots - stype->parent_types[p-1]->num_slots;
    else
      n = stype->num_slots;
      
    if (!scheme_is_subinspector(stype->inspector, insp)) {
      if (!last_is_unknown)
	array[1 + (--m)] = unknown_val;
      i -= n;
      last_is_unknown = 1;
    } else {
      if (n)
        last_is_unknown = 0;
      while (n--) {
        --i;
        if (SAME_OBJ((Scheme_Object *)s, _s))
          elem = s->slots[i];
        else
          elem = scheme_struct_ref(_s, i);
	array[1 + (--m)] = elem;
      }
    }
  }

  return v;
}

static Scheme_Object *struct_to_vector(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_CHAPERONE_STRUCTP(argv[0])) {
    char *tn, *s;
    int l;
    Scheme_Object *v;

    tn = scheme_get_type_name(SCHEME_TYPE(argv[0]));
    l = strlen(tn) - 2; /* drop < ... > */
    s = scheme_malloc_atomic(l + 8);
    strcpy(s, "struct:");
    memcpy(s + 7, tn + 1, l);
    s[7 + l] = 0;
    
    v = scheme_intern_symbol(s);
    v = scheme_make_vector(2, v);
    SCHEME_VEC_ELS(v)[1] = (argc > 1) ? argv[1] : ellipses_symbol;

    return v;
  }

  return scheme_struct_to_vector(argv[0], 
				 (argc > 1) ? argv[1] : NULL, 
				 scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR));
}

static Scheme_Object *prefab_struct_key(int argc, Scheme_Object *argv[])
{
  return scheme_prefab_struct_key(argv[0]);
}

Scheme_Object *scheme_prefab_struct_key(Scheme_Object *so)
{
  Scheme_Structure *s = (Scheme_Structure *)so;

  if (SCHEME_CHAPERONEP((Scheme_Object *)s))
    s = (Scheme_Structure *)SCHEME_CHAPERONE_VAL((Scheme_Object *)s);
  
  if (SCHEME_STRUCTP(((Scheme_Object *)s)) && s->stype->prefab_key) {
    return SCHEME_CDR(s->stype->prefab_key);
  }
  
  return scheme_false;
}

static Scheme_Object *make_prefab_struct(int argc, Scheme_Object *argv[])
{
  Scheme_Struct_Type *stype;
  Scheme_Object *vec;
  int i;
  
  stype = scheme_lookup_prefab_type(argv[0], argc - 1);

  if (!stype)
    scheme_wrong_contract("make-prefab-struct", "prefab-key?", 0, argc, argv);

  if (stype->num_slots != (argc - 1)) {
    scheme_contract_error("make-prefab-struct", 
                          "mismatch between argument count and prefab key", 
                          "number of field arguments", 1, scheme_make_integer(argc-1),
                          "prefab key", 1, argv[0],
                          NULL);
  }

  vec = scheme_make_vector(argc, 0);
  for (i = 0; i < argc ; i++) {
    SCHEME_VEC_ELS(vec)[i] = argv[i];
  }

  return scheme_make_prefab_struct_instance(stype, vec);
}

#define MAX_STRUCT_FIELD_COUNT 32768
#define MAX_STRUCT_FIELD_COUNT_STR "32768"

static Scheme_Object *prefab_key_struct_type(int argc, Scheme_Object *argv[])
{
  Scheme_Struct_Type *stype;
  int v;

  if (SCHEME_INTP(argv[1])) {
    v = SCHEME_INT_VAL(argv[1]);
    if (v > MAX_STRUCT_FIELD_COUNT)
      v = -1;
  } else
    v = -1;

  stype = scheme_lookup_prefab_type(argv[0], (v >= 0) ? v : -1);

  if (!stype)
    scheme_wrong_contract("prefab-key->struct-type", "prefab-key?", 0, argc, argv);

  if (v < 0)
    scheme_wrong_contract("prefab-key->struct-type", 
                          "(integer-in 0 " MAX_STRUCT_FIELD_COUNT_STR ")", 
                          1, argc, argv);

  if (stype->num_slots != v) {
    scheme_contract_error("prefab-key->struct-type", 
                          "mismatch between prefab key and field count", 
                          "prefab key", 1, argv[0],
                          "field count", 1, argv[1],
                          NULL);
  }

  return (Scheme_Object *)stype;
}

static Scheme_Object *is_prefab_key(int argc, Scheme_Object *argv[])
{
  return (scheme_lookup_prefab_type(argv[0], -1)
          ? scheme_true
          : scheme_false);
}

int scheme_inspector_sees_part(Scheme_Object *s, Scheme_Object *insp, int pos)
     /* pos == -1 => sees any part
	pos == -2 => sees all parts */
{
  Scheme_Struct_Type *stype;
  int p;

  if (SCHEME_CHAPERONEP(s))
    s = SCHEME_CHAPERONE_VAL(s);

  stype = ((Scheme_Structure *)s)->stype;

  p = stype->name_pos;  

  if (pos == -1) {
    /* Check for any visible field */
    Scheme_Object *prev = NULL;
    while (p > -1) {
      if (!SAME_OBJ(stype->parent_types[p]->inspector, prev)) {
	prev = stype->parent_types[p]->inspector;
	if (scheme_is_subinspector(prev, insp))
	  return 1;
      }
      p--;
    }

    return 0;
  } else if (pos == -2) {
    /* Check for all visible fields */
    Scheme_Object *prev = NULL;
    while (p > -1) {
      if (!SAME_OBJ(stype->parent_types[p]->inspector, prev)) {
	prev = stype->parent_types[p]->inspector;
	if (!scheme_is_subinspector(prev, insp))
	  return 0;
      }
      p--;
    }

    return 1;
  } else {
    /* Find struct containing position. */
    while (p && (stype->parent_types[p - 1]->num_slots > pos)) {
      p--;
    }

    return scheme_is_subinspector(stype->parent_types[p]->inspector, insp);
  }
}

int scheme_struct_is_transparent(Scheme_Object *s)
{
  Scheme_Struct_Type *stype;
  int p;

  if (SCHEME_CHAPERONEP(s))
    s = SCHEME_CHAPERONE_VAL(s);

  stype = ((Scheme_Structure *)s)->stype;

  for (p = stype->name_pos + 1; p--; ) {
    if (SCHEME_TRUEP(stype->parent_types[p]->inspector))
      return 0;
  }

  return 1;
}

#define STRUCT_mPROCP(o, v)						\
  (SCHEME_PRIMP(o) && ((((Scheme_Primitive_Proc *)o)->pp.flags & SCHEME_PRIM_OTHER_TYPE_MASK) == (v)))

static Scheme_Object *
struct_setter_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];
  if (SCHEME_CHAPERONEP(v)) v = SCHEME_CHAPERONE_VAL(v);
  return ((STRUCT_mPROCP(v, SCHEME_PRIM_STRUCT_TYPE_INDEXED_SETTER)
	   || STRUCT_mPROCP(v, SCHEME_PRIM_STRUCT_TYPE_INDEXLESS_SETTER)
           || STRUCT_mPROCP(v, SCHEME_PRIM_STRUCT_TYPE_BROKEN_INDEXED_SETTER))
	  ? scheme_true : scheme_false);
}

static Scheme_Object *
struct_getter_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];
  if (SCHEME_CHAPERONEP(v)) v = SCHEME_CHAPERONE_VAL(v);
  return ((STRUCT_mPROCP(v, SCHEME_PRIM_STRUCT_TYPE_INDEXED_GETTER)
           || STRUCT_mPROCP(v, SCHEME_PRIM_STRUCT_TYPE_INDEXLESS_GETTER))
	  ? scheme_true : scheme_false);
}

static Scheme_Object *
struct_pred_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];
  if (SCHEME_CHAPERONEP(v)) v = SCHEME_CHAPERONE_VAL(v);
  return (STRUCT_mPROCP(v, SCHEME_PRIM_STRUCT_TYPE_PRED)
	  ? scheme_true : scheme_false);
}

static Scheme_Object *
struct_constr_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];
  if (SCHEME_CHAPERONEP(v)) v = SCHEME_CHAPERONE_VAL(v);
  return ((STRUCT_mPROCP(v, SCHEME_PRIM_STRUCT_TYPE_CONSTR)
           || STRUCT_mPROCP(v, SCHEME_PRIM_STRUCT_TYPE_SIMPLE_CONSTR))
	  ? scheme_true : scheme_false);
}

static Scheme_Object *
struct_prop_getter_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];
  if (SCHEME_CHAPERONEP(v)) v = SCHEME_CHAPERONE_VAL(v);
  return ((STRUCT_mPROCP(v, SCHEME_PRIM_TYPE_STRUCT_PROP_GETTER)
           && SAME_TYPE(SCHEME_TYPE(SCHEME_PRIM_CLOSURE_ELS(v)[0]), scheme_struct_property_type))
	  ? scheme_true : scheme_false);
}

static Scheme_Object *
chaperone_prop_getter_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];
  if (SCHEME_CHAPERONEP(v)) v = SCHEME_CHAPERONE_VAL(v);
  return ((STRUCT_mPROCP(v, SCHEME_PRIM_TYPE_STRUCT_PROP_GETTER)
           && SAME_TYPE(SCHEME_TYPE(SCHEME_PRIM_CLOSURE_ELS(v)[0]), scheme_chaperone_property_type))
	  ? scheme_true : scheme_false);
}

int scheme_decode_struct_shape(Scheme_Object *expected, intptr_t *_v)
{
  intptr_t v;
  int i;

  if (!expected || !SCHEME_SYMBOLP(expected))
    return 0;

  if (SCHEME_SYM_VAL(expected)[0] != 's')
    return 0;
  
  for (i = 6, v = 0; SCHEME_SYM_VAL(expected)[i]; i++) {
    v = (v * 10) + (SCHEME_SYM_VAL(expected)[i] - '0');
  }

  *_v = v;
  
  return 1;
}

int scheme_check_structure_shape(Scheme_Object *e, Scheme_Object *expected)
{
  intptr_t _v, v;
  int i;
  Scheme_Struct_Type *st;

  if (!scheme_decode_struct_shape(expected, &_v))
    return 0;
  v = _v;

  if (SCHEME_STRUCT_TYPEP(e)) {
    st = (Scheme_Struct_Type *)e;
    if (st->num_slots != st->num_islots)
      return (v == STRUCT_PROC_SHAPE_OTHER);
    return (v == ((st->num_slots << STRUCT_PROC_SHAPE_SHIFT) 
                  | STRUCT_PROC_SHAPE_STRUCT));
  } else if (!SCHEME_PRIMP(e))
    return 0;

  i = (((Scheme_Primitive_Proc *)e)->pp.flags & SCHEME_PRIM_OTHER_TYPE_MASK);
  if ((i == SCHEME_PRIM_STRUCT_TYPE_CONSTR)
      || (i == SCHEME_PRIM_STRUCT_TYPE_SIMPLE_CONSTR)) {
    st = (Scheme_Struct_Type *)SCHEME_PRIM_CLOSURE_ELS(e)[0];
    return (v == ((st->num_islots << STRUCT_PROC_SHAPE_SHIFT) 
                  | STRUCT_PROC_SHAPE_CONSTR));
  } else if (i == SCHEME_PRIM_STRUCT_TYPE_PRED) {
    return (v == STRUCT_PROC_SHAPE_PRED);
  } else if (i == SCHEME_PRIM_STRUCT_TYPE_INDEXED_SETTER) {
    st = (Scheme_Struct_Type *)SCHEME_PRIM_CLOSURE_ELS(e)[0];
    return (v == ((st->num_slots << STRUCT_PROC_SHAPE_SHIFT) 
                  | STRUCT_PROC_SHAPE_SETTER));
  } else if (i == SCHEME_PRIM_STRUCT_TYPE_INDEXED_GETTER) {
    st = (Scheme_Struct_Type *)SCHEME_PRIM_CLOSURE_ELS(e)[0];
    return (v == ((st->num_slots << STRUCT_PROC_SHAPE_SHIFT) 
                  | STRUCT_PROC_SHAPE_GETTER));
  } else if ((i == SCHEME_PRIM_STRUCT_TYPE_INDEXLESS_SETTER)
             || (i == SCHEME_PRIM_STRUCT_TYPE_BROKEN_INDEXED_SETTER)
             || (i == SCHEME_PRIM_STRUCT_TYPE_INDEXLESS_GETTER))
    return (v == STRUCT_PROC_SHAPE_OTHER);

  return 0;
}

static Scheme_Object *make_struct_field_xxor(const char *who, int getter,
					      int argc, Scheme_Object *argv[])
{
  int pos;  
  char *name;
  const char *fieldstr;
  char digitbuf[20];
  int fieldstrlen;
  Scheme_Struct_Type *st;

  /* We don't allow chaperones on the getter or setter procedure, because we
     can't preserve them in the generated procedure. */

  if (!STRUCT_mPROCP(argv[0], (getter 
                               ? SCHEME_PRIM_STRUCT_TYPE_INDEXLESS_GETTER
                               : SCHEME_PRIM_STRUCT_TYPE_INDEXLESS_SETTER))) {
    scheme_wrong_contract(who, (getter 
                                ? "(and/c struct-accessor-procedure? (lambda (p) (procedure-arity-includes? p 2)))"
                                : "(and/c struct-mutator-procedure? (lambda (p) (procedure-arity-includes? p 2)))"),
                          0, argc, argv);
    return NULL;
  }

  pos = parse_pos(who, argv[0], argv, argc);
  
  if (argc > 2) {
    if (SCHEME_FALSEP(argv[2])) {
      fieldstr = NULL;
      fieldstrlen = 0;
    } else {
      if (!SCHEME_SYMBOLP(argv[2])) {
        scheme_wrong_contract(who, "(or/c symbol? #f)", 2, argc, argv);
        return NULL;
      }
      fieldstr = scheme_symbol_val(argv[2]);
      fieldstrlen = SCHEME_SYM_LEN(argv[2]);
    }
  } else {
    sprintf(digitbuf, "field%d", (int)SCHEME_INT_VAL(argv[1]));
    fieldstr = digitbuf;
    fieldstrlen = strlen(fieldstr);
  }

  st = (Scheme_Struct_Type *)SCHEME_PRIM_CLOSURE_ELS(argv[0])[0];

  if (!fieldstr) {
    if (getter)
      name = "accessor";
    else
      name = "mutator";
  } else if (getter) {
    name = (char *)GET_NAME((char *)st->name, -1,
			    fieldstr, fieldstrlen, 0);
  } else {
    name = (char *)SET_NAME((char *)st->name, -1,
			    fieldstr, fieldstrlen, 0);
  }

  return make_struct_proc(st, 
			  name, 
			  (getter ? SCHEME_GETTER : SCHEME_SETTER), pos);
}

static Scheme_Object *make_struct_field_accessor(int argc, Scheme_Object *argv[])
{
  return make_struct_field_xxor("make-struct-field-accessor", 1, argc, argv);
				
}

static Scheme_Object *make_struct_field_mutator(int argc, Scheme_Object *argv[])
{
  return make_struct_field_xxor("make-struct-field-mutator", 0, argc, argv);
}

/*========================================================================*/
/*                           wraps and nacks                              */
/*========================================================================*/

static Scheme_Object *wrap_evt(const char *who, Scheme_Type ty, int argc, Scheme_Object *argv[])
{
  Wrapped_Evt *ww;

  if (!scheme_is_evt(argv[0]))
    scheme_wrong_contract(who, "evt?", 0, argc, argv);

  if (!SCHEME_PROCP(argv[1]))
    scheme_wrong_contract(who, "procedure?", 1, argc, argv);

  ww = MALLOC_ONE_TAGGED(Wrapped_Evt);
  ww->so.type = ty;
  ww->evt = argv[0];
  ww->wrapper = argv[1];

  return (Scheme_Object *)ww;
}

Scheme_Object *scheme_wrap_evt(int argc, Scheme_Object *argv[])
{
  return wrap_evt("wrap-evt", scheme_wrap_evt_type, argc, argv);
}

Scheme_Object *handle_evt(int argc, Scheme_Object *argv[])
{
  return wrap_evt("handle-evt", scheme_handle_evt_type, argc, argv);
}

Scheme_Object *replace_evt(int argc, Scheme_Object *argv[])
{
  return wrap_evt("replace-evt", scheme_replace_evt_type, argc, argv);
}

Scheme_Object *handle_evt_p(int argc, Scheme_Object *argv[])
{
  if (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_handle_evt_type))
    return scheme_true;

  if (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_evt_set_type)) {
    Evt_Set *es = (Evt_Set *)argv[0];
    int i;
    for (i = es->argc; i--; ) {
      if (SAME_TYPE(SCHEME_TYPE(es->argv[i]), scheme_handle_evt_type)) {
	return scheme_true;
      }
    }
  }

  if (argc)
    return scheme_false;
  else
    return NULL;
}

static Scheme_Object *do_chaperone_result_guard_proc(int is_impersonator, void *data, int argc, Scheme_Object *argv[])
{
  Scheme_Object *proc = (Scheme_Object *)data, *o, *a[1];
  Scheme_Object **received_vals, **guard_argv;
  int received_cnt, i;

  /* in case calling `proc` mutates argv */
  guard_argv = MALLOC_N(Scheme_Object *, argc);
  memcpy(guard_argv, argv, sizeof(Scheme_Object *) * argc);

  o = _scheme_apply_multi(proc, argc, guard_argv);

  if (SAME_OBJ(o, SCHEME_MULTIPLE_VALUES)) {
    received_cnt = scheme_multiple_count;
    received_vals = scheme_multiple_array;
    scheme_detach_multple_array(received_vals);
  } else {
      a[0] = o;
      received_vals = a;
      received_cnt = 1;
  }

  if (received_cnt != argc) {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_ARITY,
                     "evt %s: returned wrong number of values\n"
                     "  %s : %V\n"
                     "  expected count: %d\n"
                     "  returned count: %d\n",
                     is_impersonator ? "impersonator" : "chaperone",
                     is_impersonator ? "impersonator" : "chaperone",
                     proc,
                     argc, received_cnt);
  }

  if (!is_impersonator) {
    for (i = 0; i < argc; i++) {
      if (!scheme_chaperone_of(received_vals[i], argv[i]))
        scheme_wrong_chaperoned("evt result", "value", argv[i], received_vals[i]);
    }
  }

  return o;
}

static Scheme_Object *chaperone_result_guard_proc(void *data, int argc, Scheme_Object *argv[])
{
  return do_chaperone_result_guard_proc(0, data, argc, argv);
}

static Scheme_Object *impersonator_result_guard_proc(void *data, int argc, Scheme_Object *argv[])
{
  return do_chaperone_result_guard_proc(1, data, argc, argv);
}

static Scheme_Object *do_chaperone_guard_proc(int is_impersonator, void *data, int argc, Scheme_Object *argv[])
{
  Scheme_Object *evt = SCHEME_CAR((Scheme_Object *)data);
  Scheme_Object *proc = SCHEME_CDR((Scheme_Object *)data);
  Scheme_Object *a[2], *o, **vals;
  int cnt;
  Scheme_Thread *p;

  a[0] = evt;

  o = _scheme_apply_multi(proc, 1, a);

  if (SAME_OBJ(o, SCHEME_MULTIPLE_VALUES)) {
    p = scheme_current_thread;
    cnt = p->ku.multiple.count;
    vals = p->ku.multiple.array;
    p->ku.multiple.array = NULL;
    if (SAME_OBJ(vals, p->values_buffer))
      p->values_buffer = NULL;
    p = NULL;
  } else {
    vals = NULL;
    cnt = 1;
  }

  if (cnt != 2)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_ARITY,
                     "evt %s: returned wrong number of values\n"
                     "  %s: %V\n"
                     "  expected count: 2\n"
                     "  returned count: %d",
                     (is_impersonator ? "impersonator" : "chaperone"),
                     (is_impersonator ? "impersonator" : "chaperone"),
                     proc,
                     cnt);

  if (!is_impersonator)
    if (!scheme_chaperone_of(vals[0], evt))
      scheme_wrong_chaperoned("evt chaperone", "value", evt, vals[0]);
  if (!scheme_check_proc_arity(NULL, 1, 1, 1, vals))
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                     "evt %s: contract violation for second %s result\n"
                     "  expected: (any/c any/c . -> . any)\n"
                     "  received: %V",
                     (is_impersonator ? "impersonator" : "chaperone"),
                     (is_impersonator ? "impersonator" : "chaperone"),
                     vals[1]);

  a[0] = vals[0];
  o = scheme_make_closed_prim_w_arity((is_impersonator
                                       ? impersonator_result_guard_proc
                                       : chaperone_result_guard_proc),
                                      (void *)vals[1], 
                                      "evt-result-chaperone", 
                                      1, -1);
  a[1] = o;

  return scheme_wrap_evt(1, a);
}

static Scheme_Object *chaperone_guard_proc(void *data, int argc, Scheme_Object *argv[])
{
  return do_chaperone_guard_proc(0, data, argc, argv);
}

static Scheme_Object *impersonator_guard_proc(void *data, int argc, Scheme_Object *argv[])
{
  return do_chaperone_guard_proc(1, data, argc, argv);
}

Scheme_Object *scheme_do_chaperone_evt(const char *name, int is_impersonator, int argc, Scheme_Object *argv[])
{
  Scheme_Chaperone *px;
  Scheme_Object *o, *val, *a[1];
  Scheme_Hash_Tree *props;

  val = argv[0];
  if (SCHEME_CHAPERONEP(val))
    val = SCHEME_CHAPERONE_VAL(val);

  if (!scheme_is_evt(val))
    scheme_wrong_contract(name, "evt?", 0, argc, argv);
  scheme_check_proc_arity(name, 1, 1, argc, argv);

  props = scheme_parse_chaperone_props(name, 2, argc, argv);

  o = scheme_make_pair(argv[0], argv[1]);
  o = scheme_make_closed_prim_w_arity((is_impersonator
                                       ? impersonator_guard_proc
                                       : chaperone_guard_proc),
                                      (void *)o, 
                                      (is_impersonator
                                       ? "chaperone-evt"
                                       : "impersonate-evt"),
                                      1, 1);
  a[0] = o;
  o = nack_evt(1, a);
  
  px = MALLOC_ONE_TAGGED(Scheme_Chaperone);
  if (SCHEME_PROCP(val))
    px->iso.so.type = scheme_proc_chaperone_type;
  else
    px->iso.so.type = scheme_chaperone_type;
  px->val = val;
  px->prev = argv[0];
  px->props = props;
  px->redirects = o;
  
  if (is_impersonator)
    SCHEME_CHAPERONE_FLAGS(px) |= SCHEME_CHAPERONE_IS_IMPERSONATOR;

  return (Scheme_Object *)px;
}

static Scheme_Object *chaperone_evt(int argc, Scheme_Object *argv[])
{
  return scheme_do_chaperone_evt("chaperone-evt", 0, argc, argv);
}

static int chaperone_evt_is_ready(Scheme_Object *obj, Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *o = obj;
  Scheme_Chaperone *px;
  int redirected = 0;

  if (sinfo->false_positive_ok) {
    /* Safer, though maybe unnecessarily conservative: */
    sinfo->potentially_false_positive = 1;
    return 1;
  }

  while (SCHEME_CHAPERONEP(o)) {
    px = (Scheme_Chaperone *)o;
    if (SAME_TYPE(SCHEME_TYPE(px->redirects), scheme_nack_guard_evt_type)) {
      o = px->redirects;
      redirected = 1;
      break;
    }
    o = px->prev;
  }

  if (!redirected && SCHEME_STRUCTP(o)) {
    /* No chaperone was an `evt` chaperone. Don't discard
       other chaperones, because they may be relevant for
       structure properties. */
    return evt_struct_is_ready(obj, sinfo);
  }

  scheme_set_sync_target(sinfo, o, NULL, NULL, 0, 1, NULL);
  return 0;
}

static int is_chaperone_evt(Scheme_Object *o)
{
  return scheme_is_evt(SCHEME_CHAPERONE_VAL(o));
}

static Scheme_Object *nack_evt(int argc, Scheme_Object *argv[])
{
  Nack_Guard_Evt *nw;

  scheme_check_proc_arity("nack-guard-evt", 1, 0, argc, argv);

  nw = MALLOC_ONE_TAGGED(Nack_Guard_Evt);
  nw->so.type = scheme_nack_guard_evt_type;
  nw->maker = argv[0];

  return (Scheme_Object *)nw;
}

Scheme_Object *scheme_poll_evt(int argc, Scheme_Object *argv[])
{
  Nack_Guard_Evt *nw;

  scheme_check_proc_arity("poll-guard-evt", 1, 0, argc, argv);

  nw = MALLOC_ONE_TAGGED(Nack_Guard_Evt);
  nw->so.type = scheme_poll_evt_type;
  nw->maker = argv[0];

  return (Scheme_Object *)nw;
}

static int wrapped_evt_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo)
{
  Wrapped_Evt *ww = (Wrapped_Evt *)o;
  Scheme_Object *wrapper;

  if (ww->so.type == scheme_wrap_evt_type) {
    wrapper = ww->wrapper;
  } else {
    /* A box around the proc means that it's a cont wrapper: */
    wrapper = scheme_box(ww->wrapper);
  }

  scheme_set_sync_target(sinfo, ww->evt, wrapper, NULL, 0, 1, NULL);
  return 0;
}

static int nack_guard_evt_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo)
{
  Nack_Guard_Evt *nw = (Nack_Guard_Evt *)o;
  Scheme_Object *sema, *a[1], *result;
  Scheme_Object *nack;

  if (sinfo->false_positive_ok) {
    sinfo->potentially_false_positive = 1;
    return 1;
  }

  sema = scheme_make_sema(0);

  /* Install the semaphore immediately, so that it's posted on
     exceptions (e.g., breaks) even if they happen while trying
     to run the maker. */
  scheme_set_sync_target(sinfo, o, NULL, sema, 0, 0, NULL);

  /* Remember both the sema and the current thread's dead evt: */
  nack = scheme_alloc_object();
  nack->type = scheme_nack_evt_type;
  SCHEME_PTR1_VAL(nack) = sema;
  result = scheme_get_thread_sync(scheme_current_thread);
  SCHEME_PTR2_VAL(nack) = result;

  a[0] = nack;
  result = scheme_apply(nw->maker, 1, a);

  if (scheme_is_evt(result)) {
    scheme_set_sync_target(sinfo, result, NULL, NULL, 0, 1, NULL);
    return 0;
  } else
    return 1; /* Non-evt => ready */
}

static int nack_evt_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *a[2], *wset;

  wset = SCHEME_PTR1_VAL(o);
  /* Lazily construct an evt set: */
  if (SCHEME_SEMAP(wset)) {
    a[0] = wset;
    a[1] = SCHEME_PTR2_VAL(o);
    wset = scheme_make_evt_set(2, a);
    SCHEME_PTR1_VAL(o) = wset;
  }

  /* Redirect to the set, and wrap with void: */
  scheme_set_sync_target(sinfo, wset, scheme_void, NULL, 0, 1, NULL);

  return 0;
}

static int poll_evt_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo)
{
  Nack_Guard_Evt *nw = (Nack_Guard_Evt *)o;
  Scheme_Object *a[1], *result;

  if (sinfo->false_positive_ok) {
    sinfo->potentially_false_positive = 1;
    return 1;
  }

  a[0] = (sinfo->is_poll ? scheme_true : scheme_false);
  result = scheme_apply(nw->maker, 1, a);

  if (scheme_is_evt(result)) {
    scheme_set_sync_target(sinfo, result, NULL, NULL, 0, 1, NULL);
    return 0;
  } else
    return 1; /* Non-evt => ready */
}

static int replace_evt_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo)
{
  Active_Replace_Evt *a;
  Syncing *s;
  Scheme_Object *argv[1];

  argv[0] = ((Wrapped_Evt *)o)->evt;
  s = scheme_make_syncing(1, argv);

  a = MALLOC_ONE_TAGGED(Active_Replace_Evt);
  a->so.type = scheme_active_replace_evt_type;
  a->done = 0;
  a->wrapper = ((Wrapped_Evt *)o)->wrapper;
  a->syncing = s;
  a->orig = o;

  scheme_set_sync_target(sinfo, (Scheme_Object *)a, NULL, NULL, 0, 1, NULL);
  return 0;
}

static int active_replace_evt_is_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo)
{
  Active_Replace_Evt *a = (Active_Replace_Evt *)o;
  int nested = 0;

  if (!a->syncing)
    return 0;

  do {
    if (a->syncing) {
      /* Can't finish in a scheduler context: */
      if (a->done && sinfo->false_positive_ok) {
        sinfo->potentially_false_positive = 1;
        if (nested)
          sinfo->replace_chain = NULL;
        return 1;
      }

      if (!a->done && sinfo->replace_chain && !nested) {
        /* In a nested context; trampoline to avoid stack overflow.
           If the trampolined check succeeds, then the context calling here will
           not see it; we'll return a potentially-false-positive success,
           and then check again in a way that bubbles results up. */
        Scheme_Object *l;
        l = scheme_make_pair((Scheme_Object *)a, sinfo->replace_chain);
        sinfo->replace_chain = l;
        return 0;
      }

      if (!a->done && !sinfo->replace_chain) {
        /* In a non-nested scheduler context; We can receive trampolined syncs: */
        sinfo->replace_chain = scheme_null;
      }

      if (a->done || scheme_syncing_ready(a->syncing, sinfo, 0)) {
        sinfo->replace_chain = NULL;

        if (sinfo->potentially_false_positive)
          return 1;

        a->done = 1;
        if (sinfo->false_positive_ok) {
          sinfo->potentially_false_positive = 1;
          return 1;
        } else if (nested) {
          /* We're in a trampilined sync. Now that we've recorded success,
             wait for the next round, and we won't trampoline then. This
             waiting causes a quadratic bubbling effect for deeply nested
             events, so uses of `replace-evt` shouldn't deeply nest! */
          sinfo->spin = 1;
          return 0;
        } else {
          /* Non-nested, non-scheduler context. */
          Scheme_Object *v, *argv[1], **args;
          Syncing *s = a->syncing;
          int argc;

          a->syncing = NULL;

          v = scheme_syncing_result(s, 0);

          if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
            argc = scheme_multiple_count;
            args = scheme_multiple_array;
            scheme_detach_multple_array(args);
          } else {
            argv[0] = v;
            args = argv;
            argc = 1;
          }

          v = scheme_apply(a->wrapper, argc, args);

          if (scheme_is_evt(v)) {
            scheme_set_sync_target(sinfo, v, NULL, NULL, 0, 1, NULL);
            return 0;
          } else {
            /* Non-event => ready */
            scheme_set_sync_target(sinfo, a->orig, NULL, NULL, 0, 1, NULL);
            return 1;
          }
        }
      }
    }

    if (sinfo->replace_chain && !SCHEME_NULLP(sinfo->replace_chain)) {
      /* Receive a trampoline */
      a = (Active_Replace_Evt *)SCHEME_CAR(sinfo->replace_chain);
      sinfo->replace_chain = SCHEME_CDR(sinfo->replace_chain);
      nested = 1;
    } else
      a = NULL;
  } while (a);

  sinfo->replace_chain = NULL;

  return 0;
}

static void active_replace_evt_needs_wakeup(Scheme_Object *o, void *fds)
{
  Active_Replace_Evt *a = (Active_Replace_Evt *)o;

  if (a->syncing && !a->done)
    scheme_syncing_needs_wakeup(a->syncing, fds);
}

Syncing *scheme_replace_evt_needs_wakeup(Scheme_Object *o)
{
  Active_Replace_Evt *a = (Active_Replace_Evt *)o;

  if (a->syncing && !a->done)
    return a->syncing;

  return NULL;
}

Syncing *scheme_replace_evt_nack(Scheme_Object *o)
{
  Active_Replace_Evt *a = (Active_Replace_Evt *)o;
  Syncing *s = NULL;

  if (a->syncing) {
    s = a->syncing;
    a->syncing = NULL;
  }

  return s;
}

Syncing *scheme_replace_evt_get(Scheme_Object *o)
{
  Active_Replace_Evt *a = (Active_Replace_Evt *)o;

  return a->syncing;
}

/*========================================================================*/
/*                          struct op maker                               */
/*========================================================================*/

#define NUM_BASE_VALUES 3
#define NUM_VALUES_PER_FIELD 2

Scheme_Object **scheme_make_struct_values(Scheme_Object *type,
					  Scheme_Object **names,
					  int count,
					  int flags)
{
  Scheme_Struct_Type *struct_type;
  Scheme_Object **values;
  Scheme_Object *vi;
  char *nm;
  int slot_num, pos;

  struct_type = (Scheme_Struct_Type *)type;

  if (flags & SCHEME_STRUCT_EXPTIME)
    --count;

  values = MALLOC_N(Scheme_Object *, count);
 
#ifdef MEMORY_COUNTING_ON
  if (scheme_starting_up) {
    /* We know that these values will be kept (exns, arity-at-least, etc.). */
    scheme_misc_count += count * sizeof(Scheme_Object *);
  }
#endif

  pos = 0;
  if (!(flags & SCHEME_STRUCT_NO_TYPE))
    values[pos++] = (Scheme_Object *)struct_type;
  if (!(flags & SCHEME_STRUCT_NO_CONSTR)) {
    nm = ((flags & SCHEME_STRUCT_NAMES_ARE_STRINGS)
          ? (char *)names[pos]
          : scheme_symbol_val(names[pos]));
    vi = make_struct_proc(struct_type,
                          nm,
			  SCHEME_CONSTR, 
			  struct_type->num_slots);
    values[pos] = vi;
    pos++;
  }
  if (!(flags & SCHEME_STRUCT_NO_PRED)) {
    nm = ((flags & SCHEME_STRUCT_NAMES_ARE_STRINGS)
          ? (char *)names[pos]
          : scheme_symbol_val(names[pos]));
    vi = make_struct_proc(struct_type,
                          nm,
			  SCHEME_PRED,
			  0);
    values[pos] = vi;
    pos++;
  }

  if (flags & SCHEME_STRUCT_GEN_GET)
    --count;
  if (flags & SCHEME_STRUCT_GEN_SET)
    --count;

  slot_num = (struct_type->name_pos
	      ? struct_type->parent_types[struct_type->name_pos - 1]->num_slots 
	      : 0);
  while (pos < count) {
    if (!(flags & SCHEME_STRUCT_NO_GET)) {
      nm = ((flags & SCHEME_STRUCT_NAMES_ARE_STRINGS)
            ? (char *)names[pos]
            : scheme_symbol_val(names[pos]));
      vi = make_struct_proc(struct_type,
                            nm,
			    SCHEME_GETTER,
			    slot_num);
      values[pos] = vi;
      pos++;
    }
    
    if (!(flags & SCHEME_STRUCT_NO_SET)) {
      nm = ((flags & SCHEME_STRUCT_NAMES_ARE_STRINGS)
            ? (char *)names[pos]
            : scheme_symbol_val(names[pos]));
      vi = make_struct_proc(struct_type,
                            nm,
			    SCHEME_SETTER,
			    slot_num);
      values[pos] = vi;
      pos++;
    }

    slot_num++;
  }

  if (flags & SCHEME_STRUCT_GEN_GET) {
    nm = ((flags & SCHEME_STRUCT_NAMES_ARE_STRINGS)
          ? (char *)names[pos]
          : scheme_symbol_val(names[pos]));
    vi = make_struct_proc(struct_type,
                          nm,
			  SCHEME_GEN_GETTER,
			  slot_num);
    values[pos] = vi;
    pos++;
  }
  if (flags & SCHEME_STRUCT_GEN_SET) {
    nm = ((flags & SCHEME_STRUCT_NAMES_ARE_STRINGS)
          ? (char *) names[pos]
          : scheme_symbol_val(names[pos]));
    vi = make_struct_proc(struct_type,
                          nm,
			  SCHEME_GEN_SETTER,
			  slot_num);
    values[pos] = vi;
    pos++;
  }
  
  return values;
}

static Scheme_Object **_make_struct_names(const char *base, int blen,
					  int fcount,
					  Scheme_Object *field_symbols,
					  const char **field_strings,
					  int flags, int *count_out)
{
  Scheme_Object **names;
  const char *field_name;
  int count, fnlen;
  int slot_num, pos, as_sym;

  count = 0;

  if (!(flags & SCHEME_STRUCT_NO_TYPE))
    count++;
  if (!(flags & SCHEME_STRUCT_NO_CONSTR))
    count++;
  if (!(flags & SCHEME_STRUCT_NO_PRED))
    count++;
  if (!(flags & SCHEME_STRUCT_NO_GET))
    count += fcount;
  if (!(flags & SCHEME_STRUCT_NO_SET))
    count += fcount;
  if (flags & SCHEME_STRUCT_GEN_GET)
    count++;
  if (flags & SCHEME_STRUCT_GEN_SET)
    count++;
  if (flags & SCHEME_STRUCT_EXPTIME)
    count++;

  if (count_out) {
    *count_out = count;
    count_out = NULL; /* Might be an interior pointer. */
  }

  names = MALLOC_N(Scheme_Object *, count);

#ifdef MEMORY_COUNTING_ON
  if (scheme_starting_up) {
    /* We know that these names will be kept (exns, arity-at-least, etc.). */
    scheme_misc_count += count * sizeof(Scheme_Object *);
  }
#endif

  pos = 0;

  as_sym = ((flags & SCHEME_STRUCT_NAMES_ARE_STRINGS) ? 0 : 1);

  if (!(flags & SCHEME_STRUCT_NO_TYPE)) {
    Scheme_Object *nm;
    nm = TYPE_NAME(base, blen, as_sym);
    names[pos++] = nm;
  }
  if (!(flags & SCHEME_STRUCT_NO_CONSTR)) {
    Scheme_Object *nm;
    if (flags & SCHEME_STRUCT_NO_MAKE_PREFIX)
      nm = CSTR_NAME(base, blen, as_sym);
    else
      nm = CSTR_MAKE_NAME(base, blen, as_sym);
    names[pos++] = nm;
  }
  if (!(flags & SCHEME_STRUCT_NO_PRED)) {
    Scheme_Object *nm;
    nm = PRED_NAME(base, blen, as_sym);
    names[pos++] = nm;
  }

  if (fcount) {
    for (slot_num = 0; slot_num < fcount; slot_num++) {
      if (field_symbols) {
	Scheme_Object *fn = SCHEME_CAR(field_symbols);
	field_symbols = SCHEME_CDR(field_symbols);

	field_name = scheme_symbol_val(fn);
	fnlen = SCHEME_SYM_LEN(fn);
      } else {
	field_name = field_strings[slot_num];
	fnlen = strlen(field_name);
      }

      if (!(flags & SCHEME_STRUCT_NO_GET)) {
	Scheme_Object *nm;
	nm = GET_NAME(base, blen, field_name, fnlen, as_sym);
	names[pos++] = nm;
      }
      if (!(flags & SCHEME_STRUCT_NO_SET)) {
	Scheme_Object *nm;
	nm = SET_NAME(base, blen, field_name, fnlen, as_sym);
	names[pos++] = nm;
      }
    }
  }

  if (flags & SCHEME_STRUCT_GEN_GET) {
    Scheme_Object *nm;
    nm = GENGET_NAME(base, blen, as_sym);
    names[pos++] = nm;
  }
  if (flags & SCHEME_STRUCT_GEN_SET) {
    Scheme_Object *nm;
    nm = GENSET_NAME(base, blen, as_sym);
    names[pos++] = nm;
  }

  if (flags & SCHEME_STRUCT_EXPTIME) {
    Scheme_Object *nm;
    nm = EXPTIME_NAME(base, blen, as_sym);
    names[pos++] = nm;
  }

  return names;
}

Scheme_Object **scheme_make_struct_names(Scheme_Object *base, 
					 Scheme_Object *field_symbols,
					 int flags, int *count_out)
{
  int len;
  len = field_symbols ? scheme_list_length(field_symbols) : 0;

  return _make_struct_names(scheme_symbol_val(base),
			    SCHEME_SYM_LEN(base),
			    len,
			    field_symbols, NULL,
			    flags, count_out);
}

Scheme_Object **scheme_make_struct_names_from_array(const char *base, 
						    int fcount,
						    const char **fields,
						    int flags, int *count_out)
{
  return _make_struct_names(base,
			    strlen(base),
			    fcount,
			    NULL, fields,
			    flags, count_out);
}

static Scheme_Object *
make_struct_proc(Scheme_Struct_Type *struct_type, 
		 char *func_name, 
		 Scheme_ProcT proc_type, int field_num)
{
  Scheme_Object *p, *a[3];
  short flags = 0;

  if (proc_type == SCHEME_CONSTR) {
    int simple;
    simple = scheme_is_simple_struct_type(struct_type);
    a[0] = (Scheme_Object *)struct_type;
    p = scheme_make_folding_prim_closure((simple 
					  ? make_simple_struct_instance
					  : make_struct_instance),
					 1, a,
					 func_name,
					 struct_type->num_islots,
					 struct_type->num_islots,
					 0);
    flags |= (simple
              ? SCHEME_PRIM_STRUCT_TYPE_SIMPLE_CONSTR
              : SCHEME_PRIM_STRUCT_TYPE_CONSTR);
  } else if (proc_type == SCHEME_PRED) {
    a[0] = (Scheme_Object *)struct_type;
    p = scheme_make_folding_prim_closure(struct_pred,
					 1, a,
					 func_name,
					 1, 1, 1);
    flags |= SCHEME_PRIM_STRUCT_TYPE_PRED;
  } else {
    int need_pos;

    if ((proc_type == SCHEME_GEN_GETTER)
	|| (proc_type == SCHEME_GEN_SETTER))
      need_pos = 1;
    else
      need_pos = 0;

    a[0] = (Scheme_Object *)struct_type;
    a[1] = scheme_make_integer(field_num);
    a[2] = (Scheme_Object *)func_name;

    if ((proc_type == SCHEME_GETTER) || (proc_type == SCHEME_GEN_GETTER)) {
      p = scheme_make_folding_prim_closure(scheme_struct_getter,
					   3, a,
					   func_name,
					   1 + need_pos, 1 + need_pos, 0);
      if (need_pos)
	flags |= SCHEME_PRIM_STRUCT_TYPE_INDEXLESS_GETTER;
      else
	flags |= SCHEME_PRIM_STRUCT_TYPE_INDEXED_GETTER;
      /* Cache the accessor only if `struct_info' is used.
	 This avoids keep lots of useless accessors.
	 if (need_pos) struct_type->accessor = p; */
    } else {
      p = scheme_make_folding_prim_closure(scheme_struct_setter,
					   3, a,
					   func_name,
					   2 + need_pos, 2 + need_pos, 0);
      if (need_pos)
	flags |= SCHEME_PRIM_STRUCT_TYPE_INDEXLESS_SETTER;
      else {
        flags |= SCHEME_PRIM_STRUCT_TYPE_INDEXED_SETTER;

        if (struct_type->immutables) {
          if (struct_type->name_pos)
            field_num -= struct_type->parent_types[struct_type->name_pos - 1]->num_slots;
          if (struct_type->immutables[field_num]) {
            flags -= SCHEME_PRIM_STRUCT_TYPE_INDEXED_SETTER;
            flags |= SCHEME_PRIM_STRUCT_TYPE_BROKEN_INDEXED_SETTER;
          }
        }
      }
      /* See note above:
	 if (need_pos) struct_type->mutator = p; */
    }
  }

  ((Scheme_Closed_Primitive_Proc *)p)->pp.flags |= flags;

  return p;
}

Scheme_Object *scheme_rename_struct_proc(Scheme_Object *p, Scheme_Object *sym)
{
  if (SCHEME_PRIMP(p)) {
    unsigned short flags = ((Scheme_Primitive_Proc *)p)->pp.flags;
    int is_getter = ((flags & SCHEME_PRIM_OTHER_TYPE_MASK) == SCHEME_PRIM_STRUCT_TYPE_INDEXED_GETTER);
    int is_setter = (((flags & SCHEME_PRIM_OTHER_TYPE_MASK) == SCHEME_PRIM_STRUCT_TYPE_INDEXED_SETTER)
                     || ((flags & SCHEME_PRIM_OTHER_TYPE_MASK) == SCHEME_PRIM_STRUCT_TYPE_BROKEN_INDEXED_SETTER));
      
    if (is_getter || is_setter) {
      const char *func_name;
      Scheme_Struct_Type *st;
      int field_pos;
      
      func_name = scheme_symbol_name(sym);

      st = (Scheme_Struct_Type *)SCHEME_PRIM_CLOSURE_ELS(p)[0];
      field_pos = SCHEME_INT_VAL(SCHEME_PRIM_CLOSURE_ELS(p)[1]);
      
      return make_struct_proc(st, (char *)func_name, 
                              is_getter ? SCHEME_GETTER : SCHEME_SETTER,
                              field_pos);
    }
  }

  return NULL;
}

static Scheme_Object *make_name(const char *pre, const char *tn, int ltn,
				const char *post1, const char *fn, int lfn,
				const char *post2, int sym)
{
  int total, lp, lp1, lp2, xltn, xlfn;
  char *name, buffer[256];

  if (ltn < 0)
    xltn = SCHEME_SYM_LEN((Scheme_Object *)tn);
  else
    xltn = ltn;
  
  if (lfn < 0)
    xlfn = SCHEME_SYM_LEN((Scheme_Object *)fn);
  else
    xlfn = lfn;
  
  total = lp = strlen(pre);
  total += xltn;
  total += (lp1 = strlen(post1));
  total += xlfn;
  total += (lp2 = strlen(post2));

  if (sym && (total < 256))
    name = buffer;
  else
    name = (char *)scheme_malloc_atomic(sizeof(char)*(total + 1));
  
  memcpy(name, pre, lp);
  total = lp;
  memcpy(name + total, (ltn < 0) ? SCHEME_SYM_VAL((Scheme_Object *)tn) : tn, xltn);
  total += xltn;
  memcpy(name + total, post1, lp1);
  total += lp1;
  memcpy(name + total, (lfn < 0) ? SCHEME_SYM_VAL((Scheme_Object *)fn) : fn, xlfn);
  total += xlfn;
  memcpy(name + total, post2, lp2);
  total += lp2;

  name[total] = 0;

  if (sym)
    return scheme_intern_exact_symbol(name, total);
  else
    return (Scheme_Object *)name;
}

/*========================================================================*/
/*                             struct type                                */
/*========================================================================*/

static Scheme_Object *count_k(void);

static int count_non_proc_props(Scheme_Object *props)
{
  Scheme_Struct_Property *p;
  Scheme_Object *v;
  int count = 0;

  {
#include "mzstkchk.h"
    {
      scheme_current_thread->ku.k.p1 = (void *)props;
      return SCHEME_INT_VAL(scheme_handle_stack_overflow(count_k));
    }
  }
  SCHEME_USE_FUEL(1);

  for (; SCHEME_PAIRP(props); props = SCHEME_CDR(props)) {
    v = SCHEME_CAR(props);
    p = (Scheme_Struct_Property *)SCHEME_CAR(v);
    if (!SAME_OBJ((Scheme_Object *)p, proc_property))
      count++;
    if (p->supers) {
      count += count_non_proc_props(p->supers);
    }
  }

  return count;
}

static Scheme_Object *count_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *props = (Scheme_Object *)p->ku.k.p1;
  int c;

  p->ku.k.p1 = NULL;

  c = count_non_proc_props(props);

  return scheme_make_integer(c);
}

static Scheme_Object *append_super_props(Scheme_Struct_Property *p, Scheme_Object *arg, Scheme_Object *orig)
{
  Scheme_Object *first = NULL, *last = NULL, *props, *pr, *v, *a[1];

  if (p->supers) {
    props = p->supers;
    for (; SCHEME_PAIRP(props); props = SCHEME_CDR(props)) {
      v = SCHEME_CAR(props);

      a[0] = arg;
      v = scheme_make_pair(SCHEME_CAR(v), _scheme_apply(SCHEME_CDR(v), 1, a));

      pr = scheme_make_pair(v, scheme_null);
      if (last)
        SCHEME_CDR(last) = pr;
      else
        first = pr;
      last = pr;
    }
  }

  if (last) {
    SCHEME_CDR(last) = orig;
    return first;
  } else
    return orig;
}

static Scheme_Object *add_struct_type_chaperone_guards(Scheme_Object *o, Scheme_Object *orig_guard)
{
  Scheme_Object *first = NULL, *last = NULL, *p;
  Scheme_Chaperone *px;

  /* Order of resulting list should match order of application. Since
     we're checking arguments going in, apply more recent chaperone
     wrappers first. */
  while (SCHEME_NP_CHAPERONEP(o)) {
    px = (Scheme_Chaperone *)o;
    if (SCHEME_PAIRP(px->redirects)) {
      p = scheme_make_pair(SCHEME_CDR(SCHEME_CDR(px->redirects)), scheme_null);
      if (last)
        SCHEME_CDR(last) = p;
      else
        first = p;
      last = p;
    }
    o = px->prev;
  }

  if (!last)
    return orig_guard;
  
  if (!orig_guard) 
    orig_guard = scheme_false;

  return scheme_make_pair(orig_guard, first);
}

static void struct_type_set_if_immutable(Scheme_Struct_Type *struct_type) {
  if (!struct_type->name_pos
      || MZ_OPT_HASH_KEY(&struct_type->parent_types[struct_type->name_pos - 1]->iso) & STRUCT_TYPE_ALL_IMMUTABLE) {
    int i, size;
    size = struct_type->num_islots;
    if (struct_type->name_pos)
      size -= struct_type->parent_types[struct_type->name_pos - 1]->num_islots;
    if (!size || struct_type->immutables) {
      for (i = 0; i < size; i++) {
        if (!struct_type->immutables[i])
          return;
      }
      MZ_OPT_HASH_KEY(&struct_type->iso) |= STRUCT_TYPE_ALL_IMMUTABLE;
    }
  }
}

Scheme_Struct_Type *scheme_make_prefab_struct_type_raw(Scheme_Object *base,
                                                       Scheme_Object *parent,
                                                       int num_fields,
                                                       int num_uninit_fields,
                                                       Scheme_Object *uninit_val,
                                                       char *immutable_array)
{
  Scheme_Struct_Type *struct_type, *parent_type;
  int j, depth;

  parent_type = (Scheme_Struct_Type *)parent;
  depth = parent_type ? (1 + parent_type->name_pos) : 0;
  struct_type = (Scheme_Struct_Type *)scheme_malloc_tagged(sizeof(Scheme_Struct_Type)
                                                           + ((depth + 1 - mzFLEX_DELTA)
                                                              * sizeof(Scheme_Struct_Type *)));
  struct_type->iso.so.type = scheme_struct_type_type;

  struct_type->parent_types[depth] = struct_type;
  for (j = depth; j--; ) {
    struct_type->parent_types[j] = parent_type->parent_types[j];
  }

  struct_type->name = base;
  struct_type->num_slots = num_fields + num_uninit_fields + (parent_type ? parent_type->num_slots : 0);
  struct_type->num_islots = num_fields + (parent_type ? parent_type->num_islots : 0);
  struct_type->name_pos = depth;
  struct_type->inspector = scheme_false;
  struct_type->uninit_val = uninit_val;
  struct_type->props = NULL;
  struct_type->num_props = 0;
  struct_type->proc_attr = NULL;
  struct_type->immutables = immutable_array;
  struct_type->guard = NULL;

  struct_type_set_if_immutable(struct_type);
  struct_type = hash_prefab(struct_type);
      
  return struct_type;
}

static Scheme_Struct_Type *scheme_make_prefab_struct_type(Scheme_Object *base,
                                                          Scheme_Object *parent,
                                                          int num_fields,
                                                          int num_uninit_fields,
                                                          Scheme_Object *uninit_val,
                                                          char *immutable_array)
{
  return scheme_make_prefab_struct_type_raw(base,
                                            parent,
                                            num_fields,
                                            num_uninit_fields,
                                            uninit_val,
                                            immutable_array);
}

static Scheme_Object *_make_struct_type(Scheme_Object *base,
					Scheme_Object *parent,
					Scheme_Object *inspector,
					int num_fields,
					int num_uninit_fields,
					Scheme_Object *uninit_val,
					Scheme_Object *props,
					Scheme_Object *proc_attr,
                                        char *immutable_array,
					Scheme_Object *guard)
{
  Scheme_Struct_Type *struct_type, *parent_type;
  int j, depth, checked_proc = 0, chaperone_undefined = 0;
  
  if (parent && SCHEME_NP_CHAPERONEP(parent))
    parent_type = (Scheme_Struct_Type *)SCHEME_CHAPERONE_VAL(parent);
  else
    parent_type = (Scheme_Struct_Type *)parent;

  depth = parent_type ? (1 + parent_type->name_pos) : 0;

  struct_type = (Scheme_Struct_Type *)scheme_malloc_tagged(sizeof(Scheme_Struct_Type)
                                                           + ((depth + 1 - mzFLEX_DELTA)
                                                              * sizeof(Scheme_Struct_Type *)));

  /* defeats optimizer bug in gcc 2.7.2.3: */
  depth = parent_type ? (1 + parent_type->name_pos) : 0;

  struct_type->iso.so.type = scheme_struct_type_type;

  struct_type->name_pos = depth;
  struct_type->parent_types[depth] = struct_type;
  for (j = depth; j--; ) {
    struct_type->parent_types[j] = parent_type->parent_types[j];
  }

  struct_type->name = base;

  struct_type->num_slots = num_fields + num_uninit_fields + (parent_type ? parent_type->num_slots : 0);
  struct_type->num_islots = num_fields + (parent_type ? parent_type->num_islots : 0);
  if (parent_type)
    struct_type->proc_attr = parent_type->proc_attr;

  /* Check for integer overflow or total more than MAX_STRUCT_FIELD_COUNT: */
  if ((num_fields < 0) || (num_uninit_fields < 0)
      || (num_fields > MAX_STRUCT_FIELD_COUNT)
      || (num_uninit_fields > MAX_STRUCT_FIELD_COUNT)
      || (num_uninit_fields + num_fields > MAX_STRUCT_FIELD_COUNT)
      || (parent_type
	  && ((struct_type->num_slots < parent_type->num_slots)
	      || (struct_type->num_islots < parent_type->num_islots)))) {
    /* Too many fields. */
    scheme_raise_exn(MZEXN_FAIL, "too many fields for struct-type\n"
                     "  maximum total field count: " MAX_STRUCT_FIELD_COUNT_STR);
    return NULL;
  }

  if (!inspector) {
    if (parent_type) {
      inspector = parent_type->inspector;
      if (SCHEME_SYMBOLP(inspector))
        inspector = scheme_false;
    } else {
      inspector = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
    }
  }
  struct_type->inspector = inspector;

  if (parent_type) {
    struct_type->num_props = parent_type->num_props;
    struct_type->props = parent_type->props;
    if (MZ_OPT_HASH_KEY(&parent_type->iso) & STRUCT_TYPE_CHECKED_PROC)
      checked_proc = 1;
  }

  /* In principle, we should check for duplicate properties here
     to keep the mismatch exceptions in the right order. */

  if (!uninit_val)
    uninit_val = scheme_false;
  struct_type->uninit_val = uninit_val;

  if ((struct_type->proc_attr && SCHEME_INTP(struct_type->proc_attr))
      || (proc_attr && SCHEME_INTP(proc_attr))) {
    int n, ni, p;

    n = struct_type->num_slots;
    ni = struct_type->num_islots;
    if (parent_type) {
      n -= parent_type->num_slots;
      ni -= parent_type->num_islots;
    }

    if (proc_attr && SCHEME_INTP(proc_attr)) {
      p = SCHEME_INT_VAL(proc_attr);
      if (p < ni) {
        if (!immutable_array) {
          immutable_array = (char *)scheme_malloc_atomic(n);
          memset(immutable_array, 0, n);
        }
        immutable_array[p] = 1;
      }
    }
  }
  struct_type->immutables = immutable_array;

  /* We add properties last, because a property guard receives a
     struct-type descriptor. */

  if (proc_attr)
    props = scheme_append(props ? props : scheme_null, 
                          scheme_make_pair(scheme_make_pair(proc_property, proc_attr),
                                           scheme_null));

  if (props) {
    int num_props, i;
    Scheme_Object *proc_prop_set = NULL;
    Scheme_Hash_Table *can_override;
    Scheme_Object *l, *a, *prop, *propv, *oldv;

    can_override = scheme_make_hash_table(SCHEME_hash_ptr);

    num_props = count_non_proc_props(props);
    if ((struct_type->num_props < 0) || (struct_type->num_props + num_props > PROP_USE_HT_COUNT)) {
      Scheme_Hash_Table *ht;

      ht = scheme_make_hash_table(SCHEME_hash_ptr);
    
      if (struct_type->num_props >= 0) {
	for (i = 0; i < struct_type->num_props; i++) {
	  prop = SCHEME_CAR(struct_type->props[i]);
	  scheme_hash_set(ht, prop, SCHEME_CDR(struct_type->props[i]));
	  scheme_hash_set(can_override, prop, scheme_true);
	}
      } else {
	/* Duplicate the hash table: */
	Scheme_Hash_Table *oht = (Scheme_Hash_Table *)struct_type->props;
	for (i =  oht->size; i--; ) {
	  if (oht->vals[i]) {
	    prop = oht->keys[i];
	    scheme_hash_set(ht, prop, oht->vals[i]);
	    scheme_hash_set(can_override, prop, scheme_true);
	  }
	}
      }

      /* Add new props: */
      for (l = props; SCHEME_PAIRP(l); ) {
        int skip_supers = 0;

	a = SCHEME_CAR(l);
	prop = SCHEME_CAR(a);

        if (SAME_OBJ(prop, scheme_checked_proc_property))
          checked_proc = 1;
        if (SAME_OBJ(prop, scheme_chaperone_undefined_property))
          chaperone_undefined = 1;

        propv = guard_property(prop, SCHEME_CDR(a), struct_type);
        
        if (SAME_OBJ(prop, proc_property)) {
          if (proc_prop_set && !SAME_OBJ(proc_prop_set, propv))
            break;
        } else {
          oldv = scheme_hash_get(ht, prop);
          if (oldv) {
            /* Property is already in the superstruct_type */
            if (!scheme_hash_get(can_override, prop)) {
              if (!SAME_OBJ(oldv, propv))
                break;
              skip_supers = 1;
            }
            /* otherwise we override */
            scheme_hash_set(can_override, prop, NULL);
          }
        }
        
        l = SCHEME_CDR(l);
        if (!skip_supers)
          l = append_super_props((Scheme_Struct_Property *)prop, propv, l);
        
        if (SAME_OBJ(prop, proc_property))
          proc_prop_set = propv;
        else
          scheme_hash_set(ht, prop, propv);
      }

      struct_type->props = (Scheme_Object **)ht;
      struct_type->num_props = -1;
    } else {
      /* Make props array: */
      Scheme_Object **pa;
      int j;
      
      /* Remember origs, so we can override */
      for (i = 0; i < struct_type->num_props; i++) {
	prop = SCHEME_CAR(struct_type->props[i]);
	scheme_hash_set(can_override, prop, scheme_true);
      }
      
      pa = MALLOC_N(Scheme_Object *, i + num_props);
      memcpy(pa, struct_type->props, sizeof(Scheme_Object *) * i);

      num_props = i;

      for (l = props; SCHEME_PAIRP(l); ) {
        int skip_supers = 0;

	a = SCHEME_CAR(l);

	prop = SCHEME_CAR(a);

        if (SAME_OBJ(prop, scheme_checked_proc_property))
          checked_proc = 1;
        if (SAME_OBJ(prop, scheme_chaperone_undefined_property))
          chaperone_undefined = 1;

        propv = guard_property(prop, SCHEME_CDR(a), struct_type);

        /* Check whether already in table: */
        if (SAME_OBJ(prop, proc_property)) {
          if (proc_prop_set && !SAME_OBJ(proc_prop_set, propv))
            break;
          j = 0;
        } else {
          for (j = 0; j < num_props; j++) {
            if (SAME_OBJ(SCHEME_CAR(pa[j]), prop))
              break;
          }
          if (j < num_props) {
            /* already there */
            if (!scheme_hash_get(can_override, prop)) {
              if (!SAME_OBJ(propv, SCHEME_CDR(pa[j])))
                break;
              skip_supers = 1;
            }
            /* overriding it: */
            scheme_hash_set(can_override, prop, NULL);
          } else
            num_props++;
        }
        
        l = SCHEME_CDR(l);
        if (!skip_supers)
          l = append_super_props((Scheme_Struct_Property *)prop, propv, l);
        
        if (SAME_OBJ(prop, proc_property))
          proc_prop_set = propv;
        else {
          a = scheme_make_pair(prop, propv);
          pa[j] = a;
        }
      }
     
      if (num_props) {
        struct_type->num_props = num_props;
        struct_type->props = pa;
      }
    }

    if (!SCHEME_NULLP(l)) {
      /* SCHEME_CAR(l) is a duplicate */
      a = SCHEME_CAR(l);
      scheme_contract_error("make-struct-type", "duplicate property binding", 
                            "property", 1, a,
                            NULL);
    }
  }


  if (guard) {
    if (!scheme_check_proc_arity(NULL, struct_type->num_islots + 1, -1, 0, &guard)) {
      scheme_contract_error("make-struct-type",
                            "guard procedure does not accept correct number of arguments;\n"
                            " should accept one more than the number of constructor arguments",
                            "guard procedure", 1, guard,
                            "expected arity", 1, scheme_make_integer(struct_type->num_islots + 1),
                            NULL);
    }
    
    struct_type->guard = guard;
  } else if (chaperone_undefined) {
    struct_type->guard = scheme_undefined;
  }

  if (parent && SCHEME_NP_CHAPERONEP(parent)) {
    guard = add_struct_type_chaperone_guards(parent, struct_type->guard);
    struct_type->guard = guard;
  }

  if (checked_proc)
    MZ_OPT_HASH_KEY(&struct_type->iso) |= STRUCT_TYPE_CHECKED_PROC;
      
    /* Check all immutable */
  struct_type_set_if_immutable(struct_type);

  return (Scheme_Object *)struct_type;
}

Scheme_Object *scheme_make_struct_type(Scheme_Object *base,
				       Scheme_Object *parent,
				       Scheme_Object *inspector,
				       int num_fields, int num_uninit,
				       Scheme_Object *uninit_val,
				       Scheme_Object *properties,
				       Scheme_Object *guard)
{
  return _make_struct_type(base,
			   parent, inspector, 
			   num_fields, num_uninit,
			   uninit_val, properties, 
			   NULL, NULL,
			   guard);
}

Scheme_Object *scheme_make_struct_type2(Scheme_Object *base,
                                        Scheme_Object *parent,
                                        Scheme_Object *inspector,
                                        int num_fields, int num_uninit,
                                        Scheme_Object *uninit_val,
                                        Scheme_Object *properties,
                                        Scheme_Object *proc_attr,
                                        char *immutable_array,
                                        Scheme_Object *guard)
{
  return _make_struct_type(base,
			   parent, inspector, 
			   num_fields, num_uninit,
			   uninit_val, properties, 
			   proc_attr, immutable_array,
			   guard);
}

Scheme_Object *scheme_make_struct_type_from_string(const char *base,
						   Scheme_Object *parent,
						   int num_fields,
						   Scheme_Object *props,
						   Scheme_Object *guard,
						   int immutable)
{
  Scheme_Object *basesym, *r;
  char *immutable_array = NULL;

  if (immutable) {
    immutable_array = (char *)scheme_malloc_atomic(num_fields);
    memset(immutable_array, 1, num_fields);
  }

  basesym = scheme_intern_exact_symbol(base, strlen(base));

  r = _make_struct_type(basesym,
                        parent, scheme_false, 
                        num_fields, 0, 
                        NULL, props,
                        NULL, immutable_array,
                        guard);

  if (scheme_starting_up)
    /* Force allocation for a strcuture type that may be in the master GC: */
    scheme_force_struct_type_info((Scheme_Struct_Type *)r);

  return r;
}

static Scheme_Struct_Type *lookup_prefab(Scheme_Object *key) {
  Scheme_Object *a = NULL;

  if (prefab_table) {
    a = scheme_lookup_in_table(prefab_table, (const char *)key);
  }

  if (a)
    return (Scheme_Struct_Type *)SCHEME_WEAK_BOX_VAL(a);

  return NULL;
}

static Scheme_Struct_Type *hash_prefab(Scheme_Struct_Type *type)
{
  Scheme_Object *k, *v;
 
  if (!prefab_table) {
    REGISTER_SO(prefab_table);
    prefab_table = scheme_make_weak_equal_table();
  } 

  k = make_prefab_key(type);
  type->prefab_key = k;
  
  v = scheme_lookup_in_table(prefab_table, (const char *)k);
  
  if (v)
    v = SCHEME_WEAK_BOX_VAL(v);
  
  if (v) {
    type = (Scheme_Struct_Type *)v;
  } else {
    v = scheme_make_weak_box((Scheme_Object *)type);
    scheme_add_to_table(prefab_table, (const char *)k, v, 0);
  }

  return type;
}

static char* immutable_pos_list_to_immutable_array(Scheme_Object *immutable_pos_list, int localfieldc) {
  char* ia;
  Scheme_Object *l;
  ia = (char *)scheme_malloc_atomic(localfieldc);
  memset(ia, 0, localfieldc);

  for (l = immutable_pos_list; l && SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    int a_val;
    Scheme_Object *a;
    a = SCHEME_CAR(l);
    if (!SCHEME_INTP(a))
      a_val = -1;
    else
      a_val = SCHEME_INT_VAL(a); 
    if (a_val < 0) {
      scheme_contract_error("make-struct-type",
                            "contract violation for index of immutable field",
                            "expected:", 0, "(and/c exact-nonnegative-integer? fixnum?)",
                            "given", 1, a,
                            "in list", 1, immutable_pos_list,
                            NULL);
      return NULL;
    }
    
    if (a_val >= localfieldc) {
      scheme_contract_error("make-struct-type",
                            "index for immutable field >= initialized-field count",
                            "index", 1, scheme_make_integer(a_val),
                            "initialized-field count", 1, scheme_make_integer(localfieldc), 
                            "in list", 1, immutable_pos_list,
                            NULL);
      return NULL;
    }
    if (ia[a_val]) {
      scheme_contract_error("make-struct-type",
                            "redundant immutable field index",
                            "index", 1, scheme_make_integer(a_val),
                            "in list", 1, immutable_pos_list,
                            NULL);
      return NULL;
    }
    ia[a_val] = 1;
  }

  return ia;
}
  
static Scheme_Object *make_struct_type(int argc, Scheme_Object **argv)
{
  int initc, uninitc, num_props = 0, prefab = 0;
  Scheme_Object *props = scheme_null, *l, *a, **r, *cstr_name = NULL;
  Scheme_Object *inspector = NULL, *uninit_val;
  Scheme_Struct_Type *type;
  Scheme_Object *proc_attr = NULL, *immutable_pos_list = scheme_null, *guard = NULL;
  char *immutable_array;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract("make-struct-type", "symbol?", 0, argc, argv);
  if (!SCHEME_FALSEP(argv[1])
      && !SCHEME_CHAPERONE_STRUCT_TYPEP(argv[1]))
    scheme_wrong_contract("make-struct-type", "(or/c struct-type? #f)", 1, argc, argv);

  if (!SCHEME_INTP(argv[2]) || (SCHEME_INT_VAL(argv[2]) < 0)) {
    if (SCHEME_BIGNUMP(argv[2]) && SCHEME_BIGPOS(argv[2]))
      initc = -1;
    else {
      scheme_wrong_contract("make-struct-type", "exact-nonnegative-integer?", 2, argc, argv);
      return NULL;
    }
  } else
    initc = SCHEME_INT_VAL(argv[2]);

  if (!SCHEME_INTP(argv[3]) || (SCHEME_INT_VAL(argv[3]) < 0)) {
    if (SCHEME_BIGNUMP(argv[3]) && SCHEME_BIGPOS(argv[3]))
      uninitc = -1;
    else {
      scheme_wrong_contract("make-struct-type", "exact-nonnegative-integer?", 3, argc, argv);
      return NULL;
    }
  } else
    uninitc = SCHEME_INT_VAL(argv[3]);
  
  if (argc > 4) {
    uninit_val = argv[4];

    if (argc > 5) {
      props = argv[5];
      for (l = props; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	a = SCHEME_CAR(l);
	if (!SCHEME_PAIRP(a)
	    || !SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(a)), scheme_struct_property_type))
	  break;
	num_props++;
      }
      if (!SCHEME_NULLP(l)) {
	scheme_wrong_contract("make-struct-type", "(listof (cons/c struct-type-property? any/c))", 5, argc, argv);
      }

      if (argc > 6) {
        inspector = argv[6];
        if (SAME_OBJ(inspector, prefab_symbol)) {
          prefab = 1;
          inspector = scheme_false;
	} else if (!SCHEME_FALSEP(inspector)) {
	  if (!SAME_TYPE(SCHEME_TYPE(argv[6]), scheme_inspector_type))
	    scheme_wrong_contract("make-struct-type", "(or/c inspector? #f 'prefab)", 6, argc, argv);
	}

	if (argc > 7) {
	  if (!SCHEME_FALSEP(argv[7])) {
	    proc_attr = argv[7];
	    
	    if (!((SCHEME_INTP(proc_attr) && (SCHEME_INT_VAL(proc_attr) >= 0))
		  || (SCHEME_BIGNUMP(proc_attr) && SCHEME_BIGPOS(proc_attr))
		  || SCHEME_PROCP(proc_attr))) {
	      scheme_wrong_contract("make-struct-type", 
                                    "(or/c exact-nonnegative-integer? procedure? #f)",
                                    7, argc, argv);
	      return NULL;
	    }
	  }

	  if (argc > 8) {
	    l = immutable_pos_list = argv[8];
	    
	    if (scheme_proper_list_length(l) < 0) {
	      scheme_wrong_contract("make-struct-type", 
                                    "(listof exact-nonnegative-integer?)",
                                    8, argc, argv);
	      return NULL;
	    }

	    if (argc > 9) {
	      if (SCHEME_TRUEP(argv[9])) {
		guard = argv[9];
		if (!SCHEME_PROCP(guard))
		  scheme_wrong_contract("make-struct-type", "(or/c procedure? #f)", 9, argc, argv);
	      }

              if (argc > 10) {
                if (!SCHEME_FALSEP(argv[10])) {
                  if (!SCHEME_SYMBOLP(argv[10]))
                    scheme_wrong_contract("make-struct-type", "(or/c symbol? #f)", 10, argc, argv);
                  cstr_name = argv[10];
                }
              }
	    }
	  }
	}
      }
    }
  } else
    uninit_val = scheme_false;

  if (!uninitc)
    uninit_val = scheme_false;

  if (!inspector)
    inspector = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);

  immutable_array = immutable_pos_list_to_immutable_array(immutable_pos_list, initc + uninitc);

  if (prefab) {
    const char *bad = NULL;
    Scheme_Object *parent = argv[1];
    if (SCHEME_NP_CHAPERONEP(parent)) {
      bad = "chaperoned supertype disallowed for non-generative structure type";
    } else if (!SCHEME_FALSEP(parent) && !((Scheme_Struct_Type *)parent)->prefab_key) {
      bad = "generative supertype disallowed for non-generative structure type";
    } else if (!SCHEME_NULLP(props)) {
      bad = "properties disallowed for non-generative structure type";
    } else if (proc_attr) {
      bad = "procedure specification disallowed for non-generative structure type";
    } else if (guard) {
      bad = "guard disallowed for non-generative structure type";
    }
    if (bad) {
      scheme_contract_error("make-struct-type",
                            bad,
                            "structure type name", 1, argv[0],
                            NULL);
    }

    type = scheme_make_prefab_struct_type(argv[0],
                                          SCHEME_FALSEP(argv[1]) ? NULL : argv[1],
                                          initc, uninitc,
                                          uninit_val,
                                          immutable_array);
  } else {
    type = (Scheme_Struct_Type *)_make_struct_type(argv[0],
                                                   SCHEME_FALSEP(argv[1]) ? NULL : argv[1],
                                                   inspector,
                                                   initc, uninitc,
                                                   uninit_val, props,
                                                   proc_attr,
                                                   immutable_array,
                                                   guard);
  }

  {
    int i;
    Scheme_Object **names;

    names = scheme_make_struct_names(argv[0],
                                     NULL,
                                     (SCHEME_STRUCT_GEN_GET | SCHEME_STRUCT_GEN_SET
                                      | SCHEME_STRUCT_NAMES_ARE_STRINGS), 
                                     &i);
    if (cstr_name) {
      a = (Scheme_Object *)scheme_symbol_val(cstr_name);
      names[1] = a;
    }
    r = scheme_make_struct_values((Scheme_Object *)type, names, i, 
                                  (SCHEME_STRUCT_GEN_GET | SCHEME_STRUCT_GEN_SET
                                   | SCHEME_STRUCT_NAMES_ARE_STRINGS));

    return scheme_values(i, r);
  }
}

static Scheme_Object *make_prefab_key(Scheme_Struct_Type *type)
{
  Scheme_Object *key = scheme_null, *stack = scheme_null;
  int total_cnt;

  total_cnt = type->num_slots;

  while (type->name_pos) {
    stack = scheme_make_pair((Scheme_Object *)type, stack);
    type = type->parent_types[type->name_pos - 1];
  }

  while (type) {
    int cnt = type->num_slots;
    int icnt = type->num_islots;
    if (type->name_pos) {
      cnt -= type->parent_types[type->name_pos - 1]->num_slots;
      icnt -= type->parent_types[type->name_pos - 1]->num_islots;
    }

    if (cnt) {
      int i;
      Scheme_Object *v = scheme_null;
      for (i = icnt; i--; ) {
        if (!type->immutables || !type->immutables[i]) {
          v = scheme_make_pair(scheme_make_integer(i), v);
        }
      }
      if (!SCHEME_NULLP(v)) {
        v = scheme_list_to_vector(v);
        key = scheme_make_pair(v, key);
      }
      
      if (cnt > icnt) {
        key = scheme_make_pair(scheme_make_pair(scheme_make_integer(cnt - icnt),
                                                scheme_make_pair(type->uninit_val, 
                                                                 scheme_null)),
                               key);
      }
    }
    if (!SCHEME_NULLP(stack))
      key = scheme_make_pair(scheme_make_integer(icnt), key);

    key = scheme_make_pair(type->name, key);

    if (SCHEME_PAIRP(stack)) {
      type = (Scheme_Struct_Type *)SCHEME_CAR(stack);
      stack = SCHEME_CDR(stack);
    } else {
      type = NULL;
    }
  }

  if (SCHEME_PAIRP(key)
      && SCHEME_NULLP(SCHEME_CDR(key)))
    key = SCHEME_CAR(key);

  /* Turn the "external" key into a hashable key by adding the
     total field count. */
  
  key = scheme_make_pair(scheme_make_integer(total_cnt),
                         key);

  return key;
}

static char *mutability_data_to_immutability_data(int icnt, Scheme_Object *mutables, int *_min_cnt)
/* If `_min_cnt` is not NULL, then mutability positions can determine a minimum
   argument count that is bigger than `icnt`. */
{
  char *immutable_array = NULL, *a2;

  if (_min_cnt)
    *_min_cnt = icnt;
  
  if ((icnt > 0) || _min_cnt) {
    int sz = (icnt ? icnt : 1);
    immutable_array = (char *)scheme_malloc_atomic(icnt);
    memset(immutable_array, 1, icnt);

    if (mutables) {
      int i;
      int len;
      len = SCHEME_VEC_SIZE(mutables);
      if ((len > icnt) && !_min_cnt)
        return NULL;

      for (i = 0; i < len; i++) {
        int a_val;
        Scheme_Object *a;
        a = SCHEME_VEC_ELS(mutables)[i];
        if (!SCHEME_INTP(a)
            || (SCHEME_INT_VAL(a) < 0)
            || ((SCHEME_INT_VAL(a) >= icnt)
                && !_min_cnt))
          return NULL;
        a_val = SCHEME_INT_VAL(a);
        if (_min_cnt && (a_val >= *_min_cnt)) {
          *_min_cnt = a_val+1;
        }
        if (a_val >= sz) {
          a2 = (char *)scheme_malloc_atomic(a_val * 2);
          memset(a2, 1, a_val * 2);
          memcpy(a2, immutable_array, sz);
          sz = a_val * 2;
          immutable_array = a2;
        }
        immutable_array[a_val] = 0;
      }
    }
  }
  
  return immutable_array;
}

Scheme_Struct_Type *scheme_lookup_prefab_type(Scheme_Object *key, int min_field_count)
{
  Scheme_Struct_Type *parent = NULL;
  Scheme_Object *a, *uninit_val, *mutables, *name;
  intptr_t ucnt, icnt;
  int inferred_size = 0;
  char *immutable_array = NULL;

  if (SCHEME_SYMBOLP(key))
    key = scheme_make_pair(key, scheme_null);

  if (scheme_proper_list_length(key) < 0)
    return NULL;

  if (min_field_count > MAX_STRUCT_FIELD_COUNT)
    min_field_count = MAX_STRUCT_FIELD_COUNT;

  {
    Scheme_Struct_Type *stype = NULL;
    stype = lookup_prefab(key);
    if (stype) {
      return stype;
    }
  }


  key = scheme_reverse(key);

  while (SCHEME_PAIRP(key)) {
    /* mutable array? */
    a = SCHEME_CAR(key);
    if (SCHEME_VECTORP(a)) {
      mutables = a;
      key = SCHEME_CDR(key);
    } else
      mutables = NULL;
    
    /* auto fields? */
    if (!SCHEME_PAIRP(key))
      return NULL;
    a = SCHEME_CAR(key);
    if (SCHEME_PAIRP(a)) {
      if (scheme_proper_list_length(a) != 2)
        return NULL;
      if (!SCHEME_INTP(SCHEME_CAR(a)))
        return NULL;
      ucnt = SCHEME_INT_VAL(SCHEME_CAR(a));
      a = SCHEME_CDR(a);
      uninit_val = SCHEME_CAR(a);
      key = SCHEME_CDR(key);
    } else {
      ucnt = 0;
      uninit_val = scheme_false;
    }
        
    /* field count? */
    if (!SCHEME_PAIRP(key))
      return NULL;
    a = SCHEME_CAR(key);
    if (!SCHEME_INTP(a)) {
      if (SCHEME_NULLP(SCHEME_CDR(key))) {
        /* For last one, size can be inferred */
        icnt = min_field_count - ucnt - (parent
                                         ? parent->num_slots
                                         : 0);
        if (icnt < 0)
          icnt = 0;
        inferred_size = 1;
      } else
        return NULL;
    } else {
      icnt = SCHEME_INT_VAL(a);
      if (icnt > MAX_STRUCT_FIELD_COUNT)
        return NULL;
      key = SCHEME_CDR(key);
    }
    
    /* name */
    if (!SCHEME_PAIRP(key))
      return NULL;
    a = SCHEME_CAR(key);
    key = SCHEME_CDR(key);

    if (!SCHEME_SYMBOLP(a))
      return NULL;
    name = a;

    if ((icnt + ucnt) || (mutables && SCHEME_VEC_SIZE(mutables))) {
      int min_cnt;
      immutable_array = mutability_data_to_immutability_data(icnt + ucnt,
                                                             mutables,
                                                             inferred_size ? &min_cnt : NULL);
      if (!immutable_array)
        return NULL;
      if (inferred_size && (min_cnt > icnt + ucnt))
        icnt = min_cnt - ucnt;
    }

    if (parent && (icnt + parent->num_slots > MAX_STRUCT_FIELD_COUNT))
      return NULL;

    parent = scheme_make_prefab_struct_type(name,
                                            (Scheme_Object *)parent,
                                            icnt, ucnt,
                                            uninit_val,
                                            immutable_array);
    
  }

  if (!SCHEME_NULLP(key))
    return NULL;

  return parent;
}

/*========================================================================*/
/*                           procedure struct                             */
/*========================================================================*/

Scheme_Object *scheme_extract_struct_procedure(Scheme_Object *obj, int num_rands, Scheme_Object **rands, int *is_method)
{
  Scheme_Struct_Type *stype;
  Scheme_Object *plain_obj, *a, *proc;
  int meth_wrap = 0;

  if (SCHEME_CHAPERONEP(obj))
    plain_obj = SCHEME_CHAPERONE_VAL(obj);
  else
    plain_obj = obj;

  stype = ((Scheme_Structure *)plain_obj)->stype;
  a = stype->proc_attr;

  if (SCHEME_INTP(a)) {
    *is_method = 0;
    if (!SAME_OBJ(plain_obj, obj)) {
      proc = chaperone_struct_ref("struct-ref", NULL, obj, obj, SCHEME_INT_VAL(a));
    } else {
      proc = ((Scheme_Structure *)obj)->slots[SCHEME_INT_VAL(a)];
    }
  } else {
    *is_method = 1;
    proc = a;
  }
  
  if (num_rands >= 0) {
    /* num_rands is non-negative => do arity check */
    if (!SCHEME_PROCP(proc)
	|| !scheme_check_proc_arity(NULL, num_rands, -1, 0, &obj)) {
      /* If we're wrapping the result of procedure->method, we need to
       * account for that.
       */
      if (scheme_reduced_procedure_struct
	  && scheme_is_struct_instance(scheme_reduced_procedure_struct, plain_obj))
	meth_wrap = SCHEME_TRUEP(((Scheme_Structure *)obj)->slots[3]);
      else {
        a = do_prop_accessor(method_property, plain_obj);
        if (a && SCHEME_TRUEP(a))
          meth_wrap = 1;
      }

      scheme_wrong_count_m((char *)obj,
			   -1 /* means "name argument is really a proc struct" */, 0,
			   num_rands, rands, meth_wrap);
      return NULL;
    }
  }

  return proc;
}

static Scheme_Object *procedure_extract_target(int argc, Scheme_Object **argv)
{
  Scheme_Object *v;
  int is_method;

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_contract("procedure-extract-target", "procedure?", 0, argc, argv);
  
  if (SCHEME_STRUCTP(argv[0])) { /* don't allow chaperones */
    /* Don't expose arity reducer: */
    if (scheme_reduced_procedure_struct
        && scheme_is_struct_instance(scheme_reduced_procedure_struct, argv[0]))
      return scheme_false;

    v = scheme_extract_struct_procedure(argv[0], -1, NULL, &is_method);
    if (v && !is_method && SCHEME_PROCP(v))
      return v;
  }

  return scheme_false;
}

/*========================================================================*/
/*                           location struct                              */
/*========================================================================*/

static int exact_pos_integer(Scheme_Object *o)
{
  if (SCHEME_INTP(o))
    return SCHEME_INT_VAL(o) > 0;
  else if (SCHEME_BIGNUMP(o))
    return SCHEME_BIGPOS(o);
  else
    return 0;
}

static int exact_nneg_integer(Scheme_Object *o)
{
  if (SCHEME_INTP(o))
    return SCHEME_INT_VAL(o) >= 0;
  else if (SCHEME_BIGNUMP(o))
    return SCHEME_BIGPOS(o);
  else
    return 0;
}

Scheme_Object *scheme_make_location(Scheme_Object *src,
				    Scheme_Object *line,
				    Scheme_Object *col,
				    Scheme_Object *pos,
				    Scheme_Object *span)
{
  Scheme_Object *a[5];
  a[0] = src;
  a[1] = line;
  a[2] = col;
  a[3] = pos;
  a[4] = span;
  return scheme_make_struct_instance(location_struct, 5, a);
}

int scheme_is_location(Scheme_Object *o)
{
  if (SCHEME_CHAPERONEP(o))
    o = SCHEME_CHAPERONE_VAL(o);

  return (SCHEME_STRUCTP(o)
          && scheme_is_struct_instance(location_struct, o));
}

static Scheme_Object *check_location_fields(int argc, Scheme_Object **argv)
{
  if (SCHEME_TRUEP(argv[1]) && !exact_pos_integer(argv[1]))
    scheme_wrong_field_contract(argv[5], "(or/c exact-positive-integer? #f)", argv[1]);
  if (SCHEME_TRUEP(argv[2]) && !exact_nneg_integer(argv[2]))
    scheme_wrong_field_contract(argv[5], "(or/c exact-nonnegative-integer #f)", argv[2]);
  if (SCHEME_TRUEP(argv[3]) && !exact_pos_integer(argv[3]))
    scheme_wrong_field_contract(argv[5], "(or/c exact-positive-integer? #f)", argv[3]);
  if (SCHEME_TRUEP(argv[4]) && !exact_nneg_integer(argv[4]))
    scheme_wrong_field_contract(argv[5], "(or/c exact-nonnegative-integer? #f)", argv[4]);
  
  return scheme_values(5, argv);
}

/*========================================================================*/
/*                        date and arity checkers                         */
/*========================================================================*/

static Scheme_Object *check_arity_at_least_fields(int argc, Scheme_Object **argv)
{
  Scheme_Object *a;

  a = argv[0];
  if (SCHEME_INTP(a)) {
    if (SCHEME_INT_VAL(a) >= 0)
      return a;
  } else if (SCHEME_BIGNUMP(a)) {
    if (SCHEME_BIGPOS(a))
      return a;
  }

  scheme_wrong_field_contract(argv[1], "exact-nonnegative-integer?", a);
  return NULL;
}

static Scheme_Object *check_date_fields(int argc, Scheme_Object **argv)
{
  Scheme_Object *a, *args[10];

  a = argv[0];
  if (!SCHEME_INTP(a) || (SCHEME_INT_VAL(a) < 0) || (SCHEME_INT_VAL(a) > 60))
    scheme_wrong_field_contract(argv[10], "(integer-in 0 60)", a);
  a = argv[1];
  if (!SCHEME_INTP(a) || (SCHEME_INT_VAL(a) < 0) || (SCHEME_INT_VAL(a) > 59))
    scheme_wrong_field_contract(argv[10], "(integer-in 0 59)", a);
  a = argv[2];
  if (!SCHEME_INTP(a) || (SCHEME_INT_VAL(a) < 0) || (SCHEME_INT_VAL(a) > 23))
    scheme_wrong_field_contract(argv[10], "(integer-in 0 23)", a);
  a = argv[3];
  if (!SCHEME_INTP(a) || (SCHEME_INT_VAL(a) < 1) || (SCHEME_INT_VAL(a) > 31))
    scheme_wrong_field_contract(argv[10], "(integer-in 1 31)", a);
  a = argv[4];
  if (!SCHEME_INTP(a) || (SCHEME_INT_VAL(a) < 1) || (SCHEME_INT_VAL(a) > 12))
    scheme_wrong_field_contract(argv[10], "(integer-in 1 12)", a);
  a = argv[5];
  if (!SCHEME_INTP(a) && !SCHEME_BIGNUMP(a))
    scheme_wrong_field_contract(argv[10], "exact-integer?", a);
  a = argv[6];
  if (!SCHEME_INTP(a) || (SCHEME_INT_VAL(a) < 0) || (SCHEME_INT_VAL(a) > 6))
    scheme_wrong_field_contract(argv[10], "(integer-in 0 6)", a);
  a = argv[7];
  if (!SCHEME_INTP(a) || (SCHEME_INT_VAL(a) < 0) || (SCHEME_INT_VAL(a) > 365))
    scheme_wrong_field_contract(argv[10], "(integer-in 0 365)", a);
  a = argv[9];
  if (!SCHEME_INTP(a) && !SCHEME_BIGNUMP(a))
    scheme_wrong_field_contract(argv[10], "exact-integer?", a);

  /* Normalize dst? boolean: */
  memcpy(args, argv, sizeof(Scheme_Object *) * 10);
  args[8] = (SCHEME_TRUEP(argv[8]) ? scheme_true : scheme_false);
  
  return scheme_values(10, args);
}

static Scheme_Object *check_date_star_fields(int argc, Scheme_Object **argv)
{
  Scheme_Object *args[12], *a;

  a = argv[10];
  if (!SCHEME_INTP(a) || (SCHEME_INT_VAL(a) < 0)
      || (SCHEME_INT_VAL(a) > 999999999))
    scheme_wrong_field_contract(argv[12], "(integer-in 0 999999999)", a);
  
  a = argv[11];
  if (!SCHEME_CHAR_STRINGP(a)) 
    scheme_wrong_field_contract(argv[12], "string?", a);

  memcpy(args, argv, sizeof(Scheme_Object *) * 12);
  if (!SCHEME_IMMUTABLEP(argv[11])) {
    a = argv[11];
    a = scheme_make_immutable_sized_char_string(SCHEME_CHAR_STR_VAL(a),
                                                SCHEME_CHAR_STRLEN_VAL(a),
                                                1);
    args[11] = a;
  }
   
  return scheme_values(12, args);
}

/*========================================================================*/
/*                        special-comment struct                          */
/*========================================================================*/

Scheme_Object *scheme_special_comment_value(Scheme_Object *o)
{
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_special_comment_type))
    return ((Scheme_Small_Object *)o)->u.ptr_val;
  else
    return NULL;
}

Scheme_Object *make_special_comment(int argc, Scheme_Object **argv)
{
  Scheme_Object *o;

  o = scheme_alloc_small_object();
  o->type = scheme_special_comment_type;
  SCHEME_PTR_VAL(o) = argv[0];

  return o;
}

Scheme_Object *special_comment_value(int argc, Scheme_Object **argv)
{
  Scheme_Object *v;

  v = scheme_special_comment_value(argv[0]);
  if (!v)
    scheme_wrong_contract("special-comment-value", "special-comment?", 0, argc, argv);
  return v;
}

Scheme_Object *special_comment_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_special_comment_type)
	  ? scheme_true
	  : scheme_false);
}

/**********************************************************************/

static Scheme_Object *exn_source_p(int argc, Scheme_Object **argv)
{
  return (scheme_struct_type_property_ref(scheme_source_property, argv[0])
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *exn_source_get(int argc, Scheme_Object **argv)
{
  Scheme_Object *v;

  v = scheme_struct_type_property_ref(scheme_source_property, argv[0]);
  if (!v)
    scheme_wrong_contract("exn:srclocs-accessor", "exn:srclocs?", 0, argc, argv);
  
  return v;
}

static Scheme_Object *check_exn_source_property_value_ok(int argc, Scheme_Object *argv[])
     /* This is the guard for prop:exn:srclocs */
{
  scheme_check_proc_arity("guard-for-prop:exn:srclocs", 1, 0, argc, argv);

  return argv[0];
}

/**********************************************************************/

static Scheme_Object *exn_module_path_p(int argc, Scheme_Object **argv)
{
  return (scheme_struct_type_property_ref(scheme_module_path_property, argv[0])
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *exn_module_path_get(int argc, Scheme_Object **argv)
{
  Scheme_Object *v;

  v = scheme_struct_type_property_ref(scheme_module_path_property, argv[0]);
  if (!v)
    scheme_wrong_contract("exn:missing-module-accessor", "exn:missing-module?", 0, argc, argv);
  
  return v;
}

static Scheme_Object *check_exn_module_path_property_value_ok(int argc, Scheme_Object *argv[])
     /* This is the guard for prop:exn:srclocs */
{
  scheme_check_proc_arity("guard-for-prop:exn:missing-module", 1, 0, argc, argv);

  return argv[0];
}

/**********************************************************************/

static Scheme_Object *do_chaperone_struct(const char *name, int is_impersonator, int argc, Scheme_Object **argv)
/* (chaperone-struct v mutator/selector redirect-proc ...) */
{
  Scheme_Chaperone *px;
  Scheme_Struct_Type *stype, *st;
  Scheme_Object *val = argv[0], *proc;
  Scheme_Object *redirects, *prop, *si_chaperone = scheme_false;
  Scheme_Object *a[1], *inspector, *getter_positions = scheme_null;
  int i, offset, arity, non_applicable_op, repeat_op;
  const char *kind;
  Scheme_Hash_Tree *props = NULL, *red_props = NULL, *empty_red_props = NULL, *setter_positions = NULL;
  intptr_t field_pos;
  int empty_si_chaperone = 0, *empty_redirects = NULL, has_redirect = 0, witnessed = 0;

  if (argc == 1) return argv[0];

  if (SCHEME_CHAPERONEP(val)) {
    props = ((Scheme_Chaperone *)val)->props;
    val = SCHEME_CHAPERONE_VAL(val);
  }

  if (SCHEME_STRUCTP(val)) {
    stype = ((Scheme_Structure *)val)->stype;
    /* vector size needs to be even to distinguish it from procedure chaperones: */
    redirects = scheme_make_vector(PRE_REDIRECTS + 2 * stype->num_slots, scheme_false);
  } else {
    stype = NULL;
    redirects = NULL;
  }

  if (is_impersonator)
    inspector = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
  else
    inspector = NULL;

  i = 1;

  if ((i < argc) && (SCHEME_STRUCT_TYPEP(argv[i])
                     || (SCHEME_NP_CHAPERONEP(argv[i])
                         && SCHEME_STRUCT_TYPEP(SCHEME_CHAPERONE_VAL(argv[i]))))) {
    if (!SCHEME_STRUCTP(val) || !scheme_is_struct_instance((SCHEME_NP_CHAPERONEP(argv[i])
                                                            ? SCHEME_CHAPERONE_VAL(argv[i])
                                                            : argv[i]),
                                                           val)) {
      scheme_contract_error(name,
                            "given value is not an instance of the given structure type",
                            "struct type", 1, argv[i],
                            "value", 1, argv[0],
                            NULL);
      return NULL;
    }
    i++;
    witnessed = 1;
  }

  for (; i < argc; i++) {
    proc = argv[i];

    if ((i > 1) && SAME_TYPE(SCHEME_TYPE(proc), scheme_chaperone_property_type)) {
      props = scheme_parse_chaperone_props(name, i, argc, argv);
      break;
    }

    a[0] = proc;
    if (SCHEME_CHAPERONEP(proc)) proc = SCHEME_CHAPERONE_VAL(proc);
    if (SCHEME_TRUEP(struct_setter_p(1, a))) {
      kind = "mutator";
      if (stype)
        offset = stype->num_slots;
      else
        offset = 0;
      witnessed = 1;
    } else if (SCHEME_TRUEP(struct_getter_p(1, a))) {
      kind = "accessor";
      offset = 0;
      witnessed = 1;
    } else if (SCHEME_TRUEP(struct_prop_getter_p(1, a))) {
      kind = "struct-type property accessor";
      offset = -1;
      witnessed = 1;
    } else if (!is_impersonator && SAME_OBJ(proc, struct_info_proc)) {
      kind = "struct-info";
      offset = -2;
    } else {
#define CHAP_PROC_CONTRACT_STR(extra)                      \
      ("(or/c " extra "struct-accessor-procedure?\n"      \
       "      struct-mutator-procedure?\n"                 \
       "      struct-type-property-accessor-procedure?\n"  \
       "      (one-of/c struct-info))")
      
      scheme_wrong_contract(name,
                            ((i == 1)
                             ? CHAP_PROC_CONTRACT_STR("struct-type?\n      ")
                             : CHAP_PROC_CONTRACT_STR("")),
                            i, argc, argv);
      return NULL;
    }

    non_applicable_op = 0;
    repeat_op = 0;

    if (offset == -2) {
      if (SCHEME_TRUEP(si_chaperone) || empty_si_chaperone)
        scheme_contract_error(name,
                              "struct-info procedure supplied a second time",
                              "procedure", 1, a[0],
                              NULL);
      st = NULL;
      field_pos = 0;
      prop = NULL;
      arity = 2;
    } else if (offset == -1) {
      prop = SCHEME_PRIM_CLOSURE_ELS(proc)[0];
      st = NULL;
      field_pos = 0;

      if (is_impersonator
          && !((Scheme_Struct_Property *)prop)->can_impersonate)
        scheme_contract_error(name,
                              "operation cannot be impersonated",
                              "operation kind", 0, kind,
                              "operation procedure", 1, a[0],
                              NULL);

      if (!scheme_struct_type_property_ref(prop, argv[0])) {
        non_applicable_op = 1;
        arity = 0;
      } else {
        if (red_props && scheme_hash_tree_get(red_props, prop))
          repeat_op = 1;
        if (empty_red_props && scheme_hash_tree_get(empty_red_props, prop))
          repeat_op = 1;

        arity = 2;
      }
    } else {
      st = (Scheme_Struct_Type *)((Scheme_Primitive_Closure *)proc)->val[0];
      field_pos = SCHEME_INT_VAL(((Scheme_Primitive_Closure *)proc)->val[1]);
      prop = NULL;
    
      if (!SCHEME_STRUCTP(val) || !scheme_is_struct_instance((Scheme_Object *)st, val))
        non_applicable_op = 1;
      else if (SCHEME_TRUEP(SCHEME_VEC_ELS(redirects)[PRE_REDIRECTS + offset + field_pos]))
        repeat_op = 1;
      else if (empty_redirects && empty_redirects[offset + field_pos])
        repeat_op = 1;
      else {
        if (is_impersonator) {
          intptr_t loc_field_pos;
          loc_field_pos = field_pos - (st->name_pos 
                                       ? st->parent_types[st->name_pos - 1]->num_slots 
                                       : 0);
          /* Must not be an immutable field. */
          if (stype->immutables) {
            if (stype->immutables[loc_field_pos])
              scheme_contract_error(name,
                                    "cannot replace operation for an immutable field",
                                    "operation kind", 0, kind,
                                    "operation procedure", 1, a[0],
                                    NULL);
          }
          if (!offset) {
            /* impersonating a getter is allowed only if the structure type is
               transparent or if the setter is also impersonated (which would prove
               that the code creating the impersonator has suitable access). */
            if (!scheme_inspector_sees_part(argv[0], inspector, field_pos)) {
              getter_positions = scheme_make_pair(scheme_make_pair(scheme_make_integer(field_pos), a[0]),
                                                  getter_positions);
            }
          } else {
            if (!scheme_inspector_sees_part(argv[0], inspector, field_pos)) {
              if (!setter_positions)
                setter_positions = scheme_make_hash_tree(0);
              setter_positions = scheme_hash_tree_set(setter_positions, scheme_make_integer(field_pos), scheme_true);
            }
          }
        }
      }

      arity = 2;
    }

    if (repeat_op)
      scheme_contract_error(name,
                            "given operation accesses the same value as a previous operation argument",
                            "operation kind", 0, kind,
                            "operation procedure", 1, a[0],
                            NULL);

    if (non_applicable_op)
      scheme_contract_error(name,
                            "operation does not apply to given value",
                            "operation kind", 0, kind,
                            "operation procedure", 1, a[0],
                            "value", 1, argv[0],
                            NULL);

    i++;
    if (i >= argc)
      scheme_contract_error(name,
                            "missing redirection procedure after operation",
                            "operation kind", 0, kind,
                            "operation procedure", 1, a[0],
                            NULL);

    proc = argv[i];
    if (SCHEME_TRUEP(proc)
        && !scheme_check_proc_arity(NULL, arity, i, argc, argv)) {
      char buf[64];
      sprintf(buf, "(or/c (procedure-arity-includes/c %d) #f)", arity);
      scheme_contract_error(name,
                            "operation's redirection procedure does not match the expected arity",
                            "given", 1, proc,
                            "expected", 0, buf,
                            "operation kind", 0, kind,
                            "operation procedure", 1, a[0],
                            NULL);
    }

    /* If the operation to chaperone was itself a chaperone, we need to
       preserve and use the chaperoned variant of the operation. */
    if (SCHEME_CHAPERONEP(a[0]) && SCHEME_TRUEP(proc)) {
      Scheme_Chaperone *ppx = (Scheme_Chaperone *)a[0];
      if (!is_impersonator
          && (SCHEME_CHAPERONE_FLAGS(ppx) & SCHEME_CHAPERONE_IS_IMPERSONATOR)) {
        scheme_contract_error(name,
                              "impersonated operation cannot be used to create a chaperone",
                              "operation", 1, a[0],
                              NULL);
      }
      proc = scheme_make_pair(a[0], proc);
    }

    if (prop) {
      if (SCHEME_TRUEP(proc)) {
        if (!red_props)
          red_props = scheme_make_hash_tree(0);
        red_props = scheme_hash_tree_set(red_props, prop, proc);
        has_redirect = 1;
      } else {
        if (!empty_red_props)
          empty_red_props = scheme_make_hash_tree(0);
        empty_red_props = scheme_hash_tree_set(empty_red_props, prop, proc);
      }
    } else if (st) {
      if (SCHEME_TRUEP(proc)) {
        SCHEME_VEC_ELS(redirects)[PRE_REDIRECTS + offset + field_pos] = proc;
        has_redirect = 1;
      } else {
        if (!empty_redirects) {
          empty_redirects = MALLOC_N_ATOMIC(int, 2 * stype->num_slots);
          memset(empty_redirects, 0, sizeof(int) * 2 * stype->num_slots);
        }
        empty_redirects[offset + field_pos] = 1;
      }
    } else {
      if (SCHEME_TRUEP(proc)) {
        si_chaperone = proc;
        has_redirect = 1;
      } else
        empty_si_chaperone = 1;
    }
  }

  if (is_impersonator) {
    /* For each getter for a non-transparent field, check that a witness
       setter was provided */
    getter_positions = scheme_reverse(getter_positions);
    while (!SCHEME_NULLP(getter_positions)) {
      prop = SCHEME_CAR(getter_positions);
      if (!setter_positions
          || !scheme_hash_tree_get(setter_positions, SCHEME_CAR(prop))) {
        scheme_contract_error(name,
                              "accessor redirection for a non-transparent field requires a mutator redirection",
                              "explanation", 0, "a mutator redirection acts as a witness that access is allowed",
                              "accessor", 1, SCHEME_CDR(prop),
                              "value to impersonate", 1, argv[0],
                              NULL);
      }
      getter_positions = SCHEME_CDR(getter_positions);
    }
  }

  if (!has_redirect && !props)
    return argv[0];

  if (!witnessed) {
    scheme_contract_error(name,
                          (is_impersonator
                           ? "cannot impersonate value as a structure without a witness"
                           : "cannot chaperone value as a structure without a witness"),
                          "explanation", 0, ("a structure type, accessor, or mutator acts as a witness\n"
                                             "   that the given value's representation can be chaperoned or impersonated"),
                          "given value", 1, argv[0],
                          NULL);
    return NULL;
  }
  
  if (!redirects) {
    /* a non-structure chaperone */
    redirects = scheme_make_vector(1, NULL);
  } else {
    SCHEME_VEC_ELS(redirects)[1] = si_chaperone;
  }

  SCHEME_VEC_ELS(redirects)[0] = (red_props ? (Scheme_Object *)red_props : scheme_false);

  px = MALLOC_ONE_TAGGED(Scheme_Chaperone);
  if (SCHEME_PROCP(val))
    px->iso.so.type = scheme_proc_chaperone_type;
  else
    px->iso.so.type = scheme_chaperone_type;
  px->val = val;
  px->prev = argv[0];
  px->props = props;
  px->redirects = redirects;

  if (is_impersonator)
    SCHEME_CHAPERONE_FLAGS(px) |= SCHEME_CHAPERONE_IS_IMPERSONATOR;

  return (Scheme_Object *)px;
}

static Scheme_Object *chaperone_struct(int argc, Scheme_Object **argv)
{
  return do_chaperone_struct("chaperone-struct", 0, argc, argv);
}

static Scheme_Object *impersonate_struct(int argc, Scheme_Object **argv)
{
  return do_chaperone_struct("impersonate-struct", 1, argc, argv);
}


Scheme_Object *scheme_chaperone_not_undefined (Scheme_Object *orig_val)
{
  Scheme_Chaperone *px;
  Scheme_Object *val, *redirects;
  Scheme_Hash_Tree *props;

  val = orig_val;

  if (SCHEME_CHAPERONEP(val)) {
    props = ((Scheme_Chaperone *)val)->props;
    val = SCHEME_CHAPERONE_VAL(val);
  } else
    props = NULL;

  redirects = scheme_make_vector(PRE_REDIRECTS, scheme_false);

  SCHEME_VEC_ELS(redirects)[0] = scheme_false;
  SCHEME_VEC_ELS(redirects)[1] = scheme_undefined; /* special handing in struct_ref */

  px = MALLOC_ONE_TAGGED(Scheme_Chaperone);
  if (SCHEME_PROCP(val))
    px->iso.so.type = scheme_proc_chaperone_type;
  else
    px->iso.so.type = scheme_chaperone_type;
  px->val = val;
  px->prev = orig_val;
  px->props = props;
  px->redirects = redirects;

  return (Scheme_Object *)px;
}


static Scheme_Object *do_chaperone_struct_type(const char *name, int is_impersonator, int argc, Scheme_Object **argv)
{
  Scheme_Chaperone *px;
  Scheme_Object *val = argv[0];
  Scheme_Object *redirects;
  Scheme_Hash_Tree *props;
  int arity;

  if (SCHEME_CHAPERONEP(val))
    val = SCHEME_CHAPERONE_VAL(val);

  if (!SCHEME_STRUCT_TYPEP(val))
    scheme_wrong_contract(name, "struct-type?", 0, argc, argv);
  scheme_check_proc_arity(name, 8, 1, argc, argv);
  scheme_check_proc_arity(name, 1, 2, argc, argv);
  if (!SCHEME_PROCP(argv[3]))
    scheme_wrong_contract(name, "procedure?", 3, argc, argv);

  arity = ((Scheme_Struct_Type *)val)->num_islots + 1;
  if (!scheme_check_proc_arity(NULL, arity, 3, argc, argv))
    scheme_contract_error(name,
                          "guard procedure does not accept correct number of arguments",
                          "explanation", 0, "should accept one more than the number of constructor arguments",
                          "guard procedure", 1, argv[0],
                          "expected arity", 1, scheme_make_integer(arity),
                          NULL);

  props = scheme_parse_chaperone_props(name, 4, argc, argv);

  redirects = scheme_make_pair(argv[1], 
                               scheme_make_pair(argv[2],
                                                argv[3]));
  
  px = MALLOC_ONE_TAGGED(Scheme_Chaperone);
  px->iso.so.type = scheme_chaperone_type;
  px->props = props;
  px->val = val;
  px->prev = argv[0];
  px->redirects = redirects;

  if (is_impersonator)
    SCHEME_CHAPERONE_FLAGS(px) |= SCHEME_CHAPERONE_IS_IMPERSONATOR;

  return (Scheme_Object *)px;
}

static Scheme_Object *chaperone_struct_type(int argc, Scheme_Object **argv)
{
  return do_chaperone_struct_type("chaperone-struct-type", 0, argc, argv);
}

Scheme_Hash_Tree *scheme_parse_chaperone_props(const char *who, int start_at, int argc, Scheme_Object **argv)
{
  Scheme_Hash_Tree *ht;
  Scheme_Object *v;

  if (SCHEME_CHAPERONEP(argv[0]))
    ht = ((Scheme_Chaperone *)argv[0])->props;
  else
    ht = NULL;

  while (start_at < argc) {
    v = argv[start_at];
    if (!SAME_TYPE(SCHEME_TYPE(v), scheme_chaperone_property_type))
      scheme_wrong_contract(who, "impersonator-property?", start_at, argc, argv);

    if (start_at + 1 >= argc)
      scheme_contract_error(who,
                            "missing value after chaperone property",
                            "chaperone property", 1, v,
                            NULL);

    if (!ht)
      ht = scheme_make_hash_tree(0);
    ht = scheme_hash_tree_set(ht, v, argv[start_at + 1]);

    start_at += 2;
  }

  return ht;
}

/**********************************************************************/

#if MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_struct.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_structure_type, mark_struct_val);
#ifdef MZ_USE_PLACES  
  GC_REG_TRAV(scheme_serialized_structure_type, mark_serialized_struct_val);
#endif
  GC_REG_TRAV(scheme_proc_struct_type, mark_struct_val);
  GC_REG_TRAV(scheme_struct_type_type, mark_struct_type_val);
  GC_REG_TRAV(scheme_struct_property_type, mark_struct_property);
  GC_REG_TRAV(scheme_chaperone_property_type, mark_struct_property);

  GC_REG_TRAV(scheme_wrap_evt_type, mark_wrapped_evt);
  GC_REG_TRAV(scheme_handle_evt_type, mark_wrapped_evt);
  GC_REG_TRAV(scheme_nack_guard_evt_type, mark_nack_guard_evt);
  GC_REG_TRAV(scheme_poll_evt_type, mark_nack_guard_evt);
  GC_REG_TRAV(scheme_replace_evt_type, mark_wrapped_evt);
  GC_REG_TRAV(scheme_active_replace_evt_type, mark_active_replace_evt);

  GC_REG_TRAV(scheme_chaperone_type, mark_chaperone);
  GC_REG_TRAV(scheme_proc_chaperone_type, mark_chaperone);
}

END_XFORM_SKIP;

#endif
