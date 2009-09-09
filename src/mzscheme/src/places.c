
#include "schpriv.h"

/* READ ONLY SHARABLE GLOBALS */

#ifdef MZ_USE_PLACES

#include "mzrt.h"


mz_proc_thread *scheme_master_proc_thread;
THREAD_LOCAL mz_proc_thread *proc_thread_self;

Scheme_Object *scheme_place(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_wait(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_sleep(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_p(int argc, Scheme_Object *args[]);
static void load_namespace(char *namespace_name);
static void load_namespace_utf8(Scheme_Object *namespace_name);
static Scheme_Object *scheme_places_deep_copy_in_master(Scheme_Object *so);

# ifdef MZ_PRECISE_GC
static void register_traversers(void);
# endif

static void *place_start_proc(void *arg);

# define PLACE_PRIM_W_ARITY(name, func, a1, a2, env) GLOBAL_PRIM_W_ARITY(name, func, a1, a2, env)

#else

# define PLACE_PRIM_W_ARITY(name, func, a1, a2, env) GLOBAL_PRIM_W_ARITY(name, not_implemented, a1, a2, env)

static Scheme_Object *not_implemented(int argc, Scheme_Object **argv)
{
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED, "not supported");
  return NULL;
}

# ifdef MZ_PRECISE_GC
static void register_traversers(void) { }
# endif

#endif

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/
void scheme_init_place(Scheme_Env *env)
{
  Scheme_Env *plenv;

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
  
  plenv = scheme_primitive_module(scheme_intern_symbol("#%place"), env);

  PLACE_PRIM_W_ARITY("place",       scheme_place,       1, 3, plenv);
  PLACE_PRIM_W_ARITY("place-sleep", scheme_place_sleep, 1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-wait",  scheme_place_wait,  1, 1, plenv);
  PLACE_PRIM_W_ARITY("place?",      scheme_place_p,     1, 1, plenv);

  scheme_finish_primitive_module(plenv);
}

#ifdef MZ_USE_PLACES

/************************************************************************/
/************************************************************************/
/************************************************************************/

/* FIXME this struct probably will need to be garbage collected as stuff
 * is added to it */
typedef struct Place_Start_Data {
  int argc;
  Scheme_Object *thunk;
  Scheme_Object *module;
  Scheme_Object *function;
  Scheme_Object *channel;
  Scheme_Object *current_library_collection_paths;
} Place_Start_Data;

static void null_out_runtime_globals() {
  scheme_current_thread           = NULL;
  scheme_first_thread             = NULL;
  scheme_main_thread              = NULL;
                                                                   
  scheme_current_runstack_start   = NULL;
  scheme_current_runstack         = NULL;
  scheme_current_cont_mark_stack  = 0;
  scheme_current_cont_mark_pos    = 0;
}

Scheme_Object *scheme_place_sleep(int argc, Scheme_Object *args[]) {
  mzrt_sleep(SCHEME_INT_VAL(args[0]));
  return scheme_void;
}

Scheme_Object *scheme_place(int argc, Scheme_Object *args[]) {
  Scheme_Place          *place;
  Place_Start_Data      *place_data;
  mz_proc_thread        *proc_thread;
  Scheme_Object         *collection_paths;

  /* create place object */
  place = MALLOC_ONE_TAGGED(Scheme_Place);
  place->so.type = scheme_place_type;

  /* pass critical info to new place */
  place_data = MALLOC_ONE(Place_Start_Data);
  place_data->argc = argc;
  if (argc == 1) {
    place_data->thunk    = args[0];
  }
  else if (argc == 3 ) {
    place_data->module   = args[0];
    place_data->function = args[1];
    place_data->channel  = args[2];
  }
  else {
    scheme_wrong_count_m("place", 1, 2, argc, args, 0);
  }
  collection_paths = scheme_current_library_collection_paths(0, NULL);
  collection_paths = scheme_places_deep_copy_in_master(collection_paths);
  place_data->current_library_collection_paths = collection_paths;

  /* create new place */
  proc_thread = mz_proc_thread_create(place_start_proc, place_data);
  place->proc_thread = proc_thread;

  return (Scheme_Object*) place;
}

static Scheme_Object *scheme_place_wait(int argc, Scheme_Object *args[]) {
  void                  *rc;
  Scheme_Place          *place;
  place = (Scheme_Place *) args[0];

  rc = mz_proc_thread_wait((mz_proc_thread *)place->proc_thread);
  
  return scheme_void;
}

static Scheme_Object *scheme_place_p(int argc, Scheme_Object *args[])
{
  return SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type) ? scheme_true : scheme_false;
}

static void load_namespace(char *namespace_name) {
  load_namespace_utf8( scheme_make_utf8_string(namespace_name));
}

static void load_namespace_utf8(Scheme_Object *namespace_name) {
  Scheme_Object *nsreq;
  Scheme_Object *a[1];
  Scheme_Thread * volatile p;
  mz_jmp_buf * volatile saved_error_buf;
  mz_jmp_buf new_error_buf;

  nsreq = scheme_builtin_value("namespace-require");
  a[0] = scheme_make_pair(scheme_intern_symbol("lib"),
      scheme_make_pair(namespace_name, scheme_make_null()));

  p = scheme_get_current_thread();
  saved_error_buf = p->error_buf;
  p->error_buf = &new_error_buf;
  if (!scheme_setjmp(new_error_buf))
    scheme_apply(nsreq, 1, a);
  p->error_buf = saved_error_buf;
}

Scheme_Object *scheme_places_deep_copy(Scheme_Object *so)
{
  Scheme_Object *new_so = so;
  if (SCHEME_INTP(so)) {
    return so;
  }

  switch (so->type) {
    case scheme_char_string_type: /*43*/
      new_so = scheme_make_sized_offset_char_string(SCHEME_CHAR_STR_VAL(so), 0, SCHEME_CHAR_STRLEN_VAL(so), 1);
      break;
    case scheme_unix_path_type:
      new_so = scheme_make_sized_offset_path(SCHEME_BYTE_STR_VAL(so), 0, SCHEME_BYTE_STRLEN_VAL(so), 1);
      break;
    case scheme_symbol_type:
      {
        Scheme_Symbol *sym = (Scheme_Symbol *)so;
        new_so = scheme_intern_exact_symbol(sym->s, sym->len);
      }
      break;
    case scheme_pair_type:
      {
        Scheme_Object *car;
        Scheme_Object *cdr;
        Scheme_Object *pair;
        car = scheme_places_deep_copy(SCHEME_CAR(so));
        cdr = scheme_places_deep_copy(SCHEME_CDR(so));
        pair = scheme_make_pair(car, cdr);
        return pair;
      }
      break;
    case scheme_null_type:
      new_so = so;
      break;
    case scheme_resolved_module_path_type:
      abort();
      break;
    default:
      abort();
      break;
  }
  return new_so;
}

static void *place_start_proc(void *data_arg) {
  void *stack_base;
  Scheme_Object *thunk;
  Place_Start_Data *place_data;
  Scheme_Object *a[2];
  int ptid;
  ptid = mz_proc_thread_self();


  stack_base = PROMPT_STACK(stack_base);
  place_data = (Place_Start_Data *) data_arg;
 
  printf("Startin place: proc thread id%u\n", ptid);

  /* create pristine THREAD_LOCAL variables*/
  null_out_runtime_globals();

  /* scheme_make_thread behaves differently if the above global vars are not null */
#ifdef MZ_PRECISE_GC
  GC_construct_child_gc();
#endif
  scheme_place_instance_init(stack_base);
  a[0] = place_data->current_library_collection_paths;
  scheme_current_library_collection_paths(1, a);


  if (place_data->argc == 1)
  {
    load_namespace("scheme/init");
    thunk = place_data->thunk;
    scheme_apply(thunk, 0, NULL);
    stack_base = NULL;
  } else {
    Scheme_Object *place_main;
    a[0] = scheme_places_deep_copy(place_data->module);
    a[1] = scheme_places_deep_copy(place_data->function);
    place_main = scheme_dynamic_require(2, a);

    a[0] = scheme_places_deep_copy(place_data->channel);
    scheme_apply(place_main, 1, a);
  }

  return scheme_true;
}

Scheme_Object *scheme_places_deep_copy_in_master(Scheme_Object *so) {
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  void *return_payload;
  return_payload = scheme_master_fast_path(5, so);
  return (Scheme_Object*) return_payload;
#endif
  return so;
}

#ifdef MZ_PRECISE_GC
static void* scheme_master_place_handlemsg(int msg_type, void *msg_payload);
static void *master_scheme_place(void *data) {
  mz_proc_thread *myself;
  myself = proc_thread_self;
  GC_switch_in_master_gc();

  while(1) {
    int recv_type;
    void *recv_payload;
    pt_mbox *origin;
    Scheme_Object *o;

    pt_mbox_recv(myself->mbox, &recv_type, &recv_payload, &origin);
    o = scheme_master_place_handlemsg(recv_type, recv_payload);
    pt_mbox_send(origin, 2, (void *) o, NULL);
  }
  return NULL;
}

static void* scheme_master_place_handlemsg(int msg_type, void *msg_payload)
{
  switch(msg_type) {
    case 1:
      {
        Scheme_Object *o;
        Scheme_Object *copied_o;
        copied_o = scheme_places_deep_copy((Scheme_Object *)msg_payload);
        o = scheme_intern_resolved_module_path_worker(copied_o);
        return o;
      }
      break;
    case 3:
      {
        Scheme_Object *o;
        Scheme_Symbol_Parts *parts;
        parts = (Scheme_Symbol_Parts *) msg_payload;
        o = (Scheme_Object *)scheme_intern_exact_symbol_in_table_worker(parts->table, parts->kind, parts->name, parts->len);
        return o;
      }
      break;
    case 5:
      { 
        Scheme_Object *copied_o;
        copied_o = scheme_places_deep_copy((Scheme_Object *)msg_payload);
        return copied_o;
      }
      break;
  }
  return NULL;
}

void* scheme_master_fast_path(int msg_type, void *msg_payload) {
  Scheme_Object *o;
  void *original_gc;

  original_gc = GC_switch_to_master_gc();
  o = scheme_master_place_handlemsg(msg_type, msg_payload);
  GC_switch_back_from_master(original_gc);

  return o;
}


void spawn_master_scheme_place() {
  mzrt_proc_first_thread_init();
  

  //scheme_master_proc_thread = mz_proc_thread_create(master_scheme_place, NULL);
  scheme_master_proc_thread = ~0;

}
#endif

/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_PLACES_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_place_type, place_val);
}

END_XFORM_SKIP;

#endif

/************************************************************************/
/************************************************************************/
/************************************************************************/

#endif
