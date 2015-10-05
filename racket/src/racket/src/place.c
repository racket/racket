/*
  Racket
  Copyright (c) 2009-2015 PLT Design Inc.

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
static Scheme_Object* scheme_place_enabled(int argc, Scheme_Object *args[]);
static Scheme_Object* scheme_place_shared(int argc, Scheme_Object *args[]);

THREAD_LOCAL_DECL(int scheme_current_place_id);

SHARED_OK static intptr_t embedded_load_len;
SHARED_OK static const char *embedded_load;

#ifdef MZ_USE_PLACES

#ifdef UNIX_PROCESSES
# include <unistd.h>
#endif

#include "schmach.h"

READ_ONLY static Scheme_Object *scheme_def_place_exit_proc;
SHARED_OK static int scheme_places_enabled = 1;

ROSYM static Scheme_Object *quote_symbol;

static int id_counter;
static mzrt_mutex *id_counter_mutex;

SHARED_OK mz_proc_thread *scheme_master_proc_thread;
THREAD_LOCAL_DECL(static struct Scheme_Place_Object *place_object);
THREAD_LOCAL_DECL(static Scheme_Place *all_child_places);
THREAD_LOCAL_DECL(static uintptr_t force_gc_for_place_accounting);
THREAD_LOCAL_DECL(static Scheme_Struct_Type *place_event_prefab);
static Scheme_Object *scheme_place(int argc, Scheme_Object *args[]);
static Scheme_Object *place_pumper_threads(int argc, Scheme_Object *args[]);
static Scheme_Object *place_wait(int argc, Scheme_Object *args[]);
static Scheme_Object *place_kill(int argc, Scheme_Object *args[]);
static Scheme_Object *place_break(int argc, Scheme_Object *args[]);
static Scheme_Object *place_sleep(int argc, Scheme_Object *args[]);
static Scheme_Object *place_p(int argc, Scheme_Object *args[]);
static Scheme_Object *place_send(int argc, Scheme_Object *args[]);
static Scheme_Object *place_receive(int argc, Scheme_Object *args[]);
static Scheme_Object *place_channel_p(int argc, Scheme_Object *args[]);
static Scheme_Object *def_place_exit_handler_proc(int argc, Scheme_Object *args[]);
static Scheme_Object *place_channel(int argc, Scheme_Object *args[]);
static Scheme_Object* place_allowed_p(int argc, Scheme_Object *args[]);
static void cust_kill_place(Scheme_Object *pl, void *notused);
static void resume_one_place_with_lock(Scheme_Place_Object *place_obj);

static Scheme_Place_Async_Channel *place_async_channel_create();
static Scheme_Place_Bi_Channel *place_bi_channel_malloc();
static Scheme_Place_Bi_Channel *place_bi_channel_create();
static Scheme_Place_Bi_Channel *place_bi_peer_channel_create(Scheme_Place_Bi_Channel *orig);
static int place_channel_ready(Scheme_Object *so, Scheme_Schedule_Info *sinfo);
static void place_async_send(Scheme_Place_Async_Channel *ch, Scheme_Object *o);
static Scheme_Object *place_async_receive(Scheme_Place_Async_Channel *ch);
static Scheme_Object *places_deep_copy_to_master(Scheme_Object *so);
static Scheme_Object *make_place_dead(int argc, Scheme_Object *argv[]);
static int place_dead_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo);
static void* GC_master_malloc_tagged(size_t size);
static void destroy_place_object_locks(Scheme_Place_Object *place_obj);

static void bi_channel_refcount(Scheme_Place_Bi_Channel *ch, int delta);
static void bi_channel_set_finalizer(Scheme_Place_Bi_Channel *ch);

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
static Scheme_Object *places_deep_copy_worker(Scheme_Object *so, Scheme_Hash_Table **ht, 
                                              int mode, int gcable, int can_raise_exn,
                                              Scheme_Object **master_chain,
                                              Scheme_Object **invalid_object);
# define mzPDC_CHECK 0
# define mzPDC_COPY 1
# define mzPDC_UNCOPY 2
# define mzPDC_DIRECT_UNCOPY 3
# define mzPDC_DESER 4
# define mzPDC_CLEAN 5
#endif

static void places_prepare_direct(Scheme_Object *so);
static void log_place_event(const char *what, const char *tag, int has_amount, intptr_t amount);

# ifdef MZ_PRECISE_GC
static void register_traversers(void);
# endif

static void *place_start_proc(void *arg);
MZ_DO_NOT_INLINE(static void *place_start_proc_after_stack(void *data_arg, void *stack_base));

# define PLACE_PRIM_W_ARITY(name, func, a1, a2, env) GLOBAL_PRIM_W_ARITY(name, func, a1, a2, env)

#else

SHARED_OK static int scheme_places_enabled = 0;

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

  GLOBAL_PRIM_W_ARITY("place-enabled?",       scheme_place_enabled,   0, 0, plenv);
  GLOBAL_PRIM_W_ARITY("place-shared?",        scheme_place_shared,    1, 1, plenv);
  PLACE_PRIM_W_ARITY("dynamic-place",         scheme_place,           5, 5, plenv);
  PLACE_PRIM_W_ARITY("place-pumper-threads",  place_pumper_threads,   1, 2, plenv);
  PLACE_PRIM_W_ARITY("place-sleep",           place_sleep,     1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-wait",            place_wait,      1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-kill",            place_kill,      1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-break",           place_break,     1, 2, plenv);
  PLACE_PRIM_W_ARITY("place?",                place_p,         1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-channel",         place_channel,   0, 0, plenv);
  PLACE_PRIM_W_ARITY("place-channel-put",     place_send,      2, 2, plenv);
  PLACE_PRIM_W_ARITY("place-channel-get",     place_receive,   1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-channel?",        place_channel_p, 1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-message-allowed?", place_allowed_p, 1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-dead-evt",        make_place_dead, 1, 1, plenv);

  scheme_finish_primitive_module(plenv);

  /* Treat place creation as "unsafe", since the new place starts with
     permissive guards that can access unsafe features that affect
     existing places. */
  scheme_protect_primitive_provide(plenv, scheme_intern_symbol("dynamic-place"));

#ifdef MZ_USE_PLACES
  REGISTER_SO(all_child_places);
  
  REGISTER_SO(place_event_prefab);
  place_event_prefab = scheme_lookup_prefab_type(scheme_intern_symbol("place-event"), 4);
#endif
}

static Scheme_Object* scheme_place_enabled(int argc, Scheme_Object *args[]) {
  return (scheme_places_enabled == 0) ? scheme_false : scheme_true;
}

static Scheme_Object* scheme_place_shared(int argc, Scheme_Object *args[]) {
  return SHARED_ALLOCATEDP(args[0]) ? scheme_true : scheme_false;
}

void scheme_init_places_once() {
#ifdef MZ_USE_PLACES
  scheme_add_evt(scheme_place_type,            (Scheme_Ready_Fun)place_channel_ready, NULL, NULL, 1); 
  scheme_add_evt(scheme_place_bi_channel_type, (Scheme_Ready_Fun)place_channel_ready, NULL, NULL, 1);
  scheme_add_evt(scheme_place_dead_type,       (Scheme_Ready_Fun)place_dead_ready, NULL, NULL, 1);
  mzrt_mutex_create(&id_counter_mutex);
  REGISTER_SO(scheme_def_place_exit_proc);
  scheme_def_place_exit_proc = scheme_make_prim_w_arity(def_place_exit_handler_proc, "default-place-exit-handler", 1, 1);

 REGISTER_SO(quote_symbol);
 quote_symbol = scheme_intern_symbol("quote");
#endif
}

int scheme_get_place_id(void)
{
#ifdef MZ_USE_PLACES
  return scheme_current_place_id;
#else
  return 0;
#endif
}

void scheme_register_embedded_load(intptr_t len, const char *s)
{
  embedded_load_len = len;
  embedded_load = s;
}

#ifdef MZ_USE_PLACES

/************************************************************************/
/************************************************************************/
/************************************************************************/

typedef struct Place_Start_Data {
  /* Allocated as array of objects, so all
     field must be pointers */
  Scheme_Object *module;
  Scheme_Object *function;
  Scheme_Object *channel;
  Scheme_Object *current_library_collection_paths;
  Scheme_Object *current_library_collection_links;
  Scheme_Object *compiled_roots;
  mzrt_sema *ready;  /* malloc'ed item */
  struct Scheme_Place_Object *place_obj;   /* malloc'ed item */
  struct NewGC *parent_gc;
  Scheme_Object *cust_limit;
  intptr_t in;
  intptr_t out;
  intptr_t err;
  Scheme_Object *new_id;
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

Scheme_Object *place_sleep(int argc, Scheme_Object *args[]) {
  mzrt_sleep(SCHEME_INT_VAL(args[0]));
  return scheme_void;
}

Scheme_Object *scheme_make_place_object() {
  Scheme_Place_Object   *place_obj;
  place_obj = GC_master_malloc_tagged(sizeof(Scheme_Place_Object));
  place_obj->so.type = scheme_place_object_type;
  mzrt_mutex_create(&place_obj->lock);
  place_obj->die = 0;
  place_obj->dead = 0;
  place_obj->refcount = 1;
  place_obj->pbreak = 0;
  place_obj->result = 1;
  return (Scheme_Object *)place_obj;
}

static void close_six_fds(intptr_t *rw) {
  int i;
  for (i=0; i<6; i++) { if (rw[i] >= 0) scheme_close_file_fd(rw[i]); }
}

Scheme_Object *place_pumper_threads(int argc, Scheme_Object *args[]) {
  Scheme_Place          *place;
  Scheme_Object         *tmp;

  place = (Scheme_Place *) args[0];
  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type))
    scheme_wrong_contract("place-pumper-threads", "place?", 0, argc, args);

  if (argc == 2) {
    tmp = args[1];
    if (!SCHEME_VECTORP(tmp) || SCHEME_VEC_SIZE(tmp) != 3)
      scheme_wrong_type("place-pumper-threads", "vector of size 3", 1, argc, args);
    place->pumper_threads = tmp;
  }
  return place->pumper_threads;
}

Scheme_Object *scheme_place(int argc, Scheme_Object *args[]) {
  Scheme_Place          *place;
  Place_Start_Data      *place_data;
  mz_proc_thread        *proc_thread;
  Scheme_Object         *collection_paths;
  Scheme_Object         *collection_links;
  Scheme_Place_Object   *place_obj;
  mzrt_sema             *ready;
  struct NewGC          *parent_gc;
  Scheme_Custodian      *cust;
  intptr_t              mem_limit;
  Scheme_Object         *in_arg;
  Scheme_Object         *out_arg;
  Scheme_Object         *err_arg;
  intptr_t rw[6] = {-1, -1, -1, -1, -1, -1};

  /* To avoid runaway place creation, check for termination before continuing. */
  scheme_thread_block(0.0);

  parent_gc = GC_get_current_instance();

  /* create place object */
  place = MALLOC_ONE_TAGGED(Scheme_Place);
  place->so.type = scheme_place_type;
  place_obj = (Scheme_Place_Object *) scheme_make_place_object();
  place->place_obj = place_obj;
  {
    GC_CAN_IGNORE void *handle;
    handle = scheme_get_signal_handle();
    place_obj->parent_signal_handle = handle;
  }

  /* The use_factor partly determines how often a child place notifies
     a parent place that it is using more memory. If the child
     notified the parent evey time its memory use increased, that
     would probably be too often. But notifying every time the memory
     use doubles isn't good enough, because a long chain of places
     wouldn't alert parents often enough to limit total memory
     use. Decreasing the factor for each generation means that the
     alerts become more frequent as nesting gets deeper. */
  place_obj->use_factor = (place_object ? (place_object->use_factor / 2) : 1.0);

  mzrt_sema_create(&ready, 0);

  /* pass critical info to new place */
  place_data = MALLOC_ONE(Place_Start_Data);
  place_data->ready    = ready;
  place_data->place_obj = place_obj;
  place_data->parent_gc = parent_gc;
  
  {
    in_arg = args[2];
    out_arg = args[3];
    err_arg = args[4];

    if (!scheme_is_module_path(args[0]) && !SCHEME_PATHP(args[0]) && !SCHEME_MODNAMEP(args[0])) {
      scheme_wrong_contract("dynamic-place", "(or/c module-path? path? resolved-module-path?)", 0, argc, args);
    }
    if (!SCHEME_SYMBOLP(args[1])) {
      scheme_wrong_contract("dynamic-place", "symbol?", 1, argc, args);
    }
    if (SCHEME_TRUEP(in_arg) && !SCHEME_TRUEP(scheme_file_stream_port_p(1, &in_arg))) {
      scheme_wrong_contract("dynamic-place", "(or/c (and/c file-stream-port? input-port?) #f)", 2, argc, args);
    }
    if (SCHEME_TRUEP(out_arg) && !SCHEME_TRUEP(scheme_file_stream_port_p(1, &out_arg))) {
      scheme_wrong_contract("dynamic-place", "(or/c (and/c file-stream-port? input-port?) #f)", 3, argc, args);
    }
    if (SCHEME_TRUEP(err_arg) && !SCHEME_TRUEP(scheme_file_stream_port_p(1, &err_arg))) {
      scheme_wrong_contract("dynamic-place", "(or/c (and/c file-stream-port? input-port?) #f)", 4, argc, args);
    }

    if (SCHEME_PAIRP(args[0]) 
        && SAME_OBJ(SCHEME_CAR(args[0]), quote_symbol)
        && !scheme_is_predefined_module_p(args[0])) {
      scheme_contract_error("dynamic-place", "not a filesystem or predefined module-path", 
                            "module path", 1, args[0],
                            NULL);
    }

    place_data->module   = args[0];
    place_data->function = args[1];
    place_data->ready    = ready;
    
    /* create channel */
    {
      Scheme_Place_Bi_Channel *channel;
      channel = place_bi_channel_create();
      place->channel = (Scheme_Object *) channel;
      channel = place_bi_peer_channel_create(channel);
      place_data->channel = (Scheme_Object *) channel;
    }
  }

  collection_paths = scheme_current_library_collection_paths(0, NULL);
  place_data->current_library_collection_paths = collection_paths;

  collection_links = scheme_current_library_collection_links(0, NULL);
  place_data->current_library_collection_links = collection_links;

  collection_paths = scheme_compiled_file_roots(0, NULL);
  place_data->compiled_roots = collection_paths;

  cust = scheme_get_current_custodian();
  mem_limit = GC_get_account_memory_limit(cust);
  place_data->cust_limit = scheme_make_integer(mem_limit);
  place_obj->memory_limit = mem_limit;
  place_obj->parent_need_gc = &force_gc_for_place_accounting;

  { 
    intptr_t tmpfd;
    int errorno;

    if (SCHEME_TRUEP(in_arg)) {
      if (scheme_port_closed_p(in_arg)) {
        close_six_fds(rw);
        scheme_contract_error("dynamic-place", "port is closed", 
                              "port", 1, in_arg,
                              NULL);
      }
      scheme_get_port_file_descriptor(in_arg, &tmpfd);
      tmpfd = scheme_dup_file(tmpfd);
      if (tmpfd == -1) {
        errorno = scheme_errno();
        close_six_fds(rw);
        scheme_system_error("dynamic-place", "stdin dup", errorno);
      }
      rw[0] = tmpfd;
    }
    else if (scheme_os_pipe(rw, -1)) {
      errorno = scheme_errno();
      close_six_fds(rw);
      scheme_system_error("dynamic-place", "stdin pipe", errorno);
    }

    if (SCHEME_TRUEP(out_arg)) {
      if (scheme_port_closed_p(out_arg)) {
        close_six_fds(rw);
        scheme_contract_error("dynamic-place", "port is closed", 
                              "port", 1, out_arg,
                              NULL);
      }
      scheme_get_port_file_descriptor(out_arg, &tmpfd);
      tmpfd = scheme_dup_file(tmpfd);
      if (tmpfd == -1) {
        errorno = scheme_errno();
        close_six_fds(rw);
        scheme_system_error("dynamic-place", "stdout dup", errorno);
      }
      rw[3] = tmpfd;
    }
    else if (scheme_os_pipe(rw + 2, -1)) {
      errorno = scheme_errno();
      close_six_fds(rw);
      scheme_system_error("dynamic-place", "stdout pipe", errorno);
    }

    if (SCHEME_TRUEP(err_arg)) {
      if (scheme_port_closed_p(err_arg)) {
        close_six_fds(rw);
        scheme_contract_error("dynamic-place", "port is closed", 
                              "port", 1, err_arg,
                              NULL);
      }
      scheme_get_port_file_descriptor(err_arg, &tmpfd);
      tmpfd = scheme_dup_file(tmpfd);
      if (tmpfd == -1) {
        errorno = scheme_errno();
        close_six_fds(rw);
        scheme_system_error("dynamic-place", "stderr dup", errorno);
      }
      rw[5] = tmpfd;
    }
    else if (scheme_os_pipe(rw + 4, -1)) {
      errorno = scheme_errno();
      close_six_fds(rw);
      scheme_system_error("dynamic-place", "stderr pipe", errorno);
    }

    {
      place_data->in = rw[0];
      place_data->out = rw[3];
      place_data->err = rw[5];
    }
  }

  places_prepare_direct(place_data->current_library_collection_paths);
  places_prepare_direct(place_data->current_library_collection_links);
  places_prepare_direct(place_data->compiled_roots);
  places_prepare_direct(place_data->channel);
  places_prepare_direct(place_data->module);
  places_prepare_direct(place_data->function);
  
  /* create new place */
  proc_thread = mz_proc_thread_create(place_start_proc, place_data);

  if (!proc_thread) {
    mzrt_sema_destroy(ready);
    ready = NULL;
    scheme_signal_error("place: place creation failed");
  }

  mz_proc_thread_detach(proc_thread);
  proc_thread = NULL;

  /* wait until the place has started and grabbed the value
     from `place_data'; it's important that a GC doesn't happen
     here until the other place is far enough. */
  mzrt_sema_wait(ready);
  mzrt_sema_destroy(ready);
  ready = NULL;

  log_place_event("id %d: create %" PRIdPTR, "create", 1, place_data->place_obj->id);

  place_data->ready = NULL;
  place_data->place_obj = NULL;

  place->next = all_child_places;
  if (place->next)
    place->next->prev = place;
  all_child_places = place;
  
  {
    Scheme_Custodian_Reference *mref;
    mref = scheme_add_managed(NULL,
                              (Scheme_Object *)place,
                              cust_kill_place,
                              NULL,
                              1);
    place->mref = mref;
  }

#ifdef MZ_PRECISE_GC
  GC_register_new_thread(place, cust);
#endif

  {
    Scheme_Object *a[4];
    Scheme_Object *tmpport;
    a[0] = (Scheme_Object *) place;
    if (rw[1] >= 0) {
      tmpport = scheme_make_fd_output_port(rw[1], scheme_intern_symbol("place-in"),  0, 0, 0);
      a[1] = tmpport;
    }
    else
      a[1] = scheme_false;

    if (rw[2] >= 0) {
      tmpport = scheme_make_fd_input_port(rw[2],  scheme_intern_symbol("place-out"), 0, 0);
      a[2] = tmpport;
    }
    else
      a[2] = scheme_false;

    if (rw[4] >= 0) {
      tmpport = scheme_make_fd_input_port(rw[4],  scheme_intern_symbol("place-err"), 0, 0);
      a[3] = tmpport;
    }
    else
      a[3] = scheme_false;
    return scheme_values(4, a);
  }
}

static void do_place_kill(Scheme_Place *place) 
{
  Scheme_Place_Object *place_obj;
  intptr_t refcount;
  int old_id;

  place_obj = place->place_obj;

  if (!place_obj) return;

  {
    mzrt_mutex_lock(place_obj->lock);

    if (!place_obj->die)
      place_obj->die = 1;
    place_obj->refcount--;
    refcount = place_obj->refcount;

    if (place_obj->signal_handle) { scheme_signal_received_at(place_obj->signal_handle); }

    place->result = place_obj->result;

    place_obj->parent_signal_handle = NULL;

    if (refcount)
      resume_one_place_with_lock(place_obj);

    mzrt_mutex_unlock(place_obj->lock);
  }

  scheme_remove_managed(place->mref, (Scheme_Object *)place);

  if (place->next)
    place->next->prev = place->prev;
  if (place->prev)
    place->prev->next = place->next;
  else
    all_child_places = place->next;

  old_id = place_obj->id;

  if (!refcount) {
    destroy_place_object_locks(place_obj);
  }
  place->place_obj = NULL;

  log_place_event("id %d: reap %" PRIdPTR, "reap", 1, old_id);
}

static int do_place_break(Scheme_Place *place, int kind) 
{
  Scheme_Place_Object *place_obj;
  place_obj = place->place_obj;

  if (place_obj) {
    mzrt_mutex_lock(place_obj->lock);

    place_obj->pbreak = kind;

    if (place_obj->signal_handle)
      scheme_signal_received_at(place_obj->signal_handle);

    mzrt_mutex_unlock(place_obj->lock);
  }

  return 0;
}

static void cust_kill_place(Scheme_Object *pl, void *notused) 
{
  do_place_kill((Scheme_Place *)pl);
}

static Scheme_Object *place_kill(int argc, Scheme_Object *args[]) {
  Scheme_Place          *place;
  place = (Scheme_Place *) args[0];

  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type))
    scheme_wrong_contract("place-kill", "place?", 0, argc, args);

  do_place_kill(place);
  return scheme_void;
}

static Scheme_Object *place_break(int argc, Scheme_Object *args[]) 
{
  Scheme_Place *place = (Scheme_Place *) args[0];
  int kind = MZEXN_BREAK;

  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type))
    scheme_wrong_contract("place-break", "place?", 0, argc, args);

  if ((argc > 1) && SCHEME_TRUEP(args[1])) {
    if (SCHEME_SYMBOLP(args[1]) 
        && !SCHEME_SYM_WEIRDP(args[1]) 
        && !strcmp(SCHEME_SYM_VAL(args[1]), "hang-up"))
      kind = MZEXN_BREAK_HANG_UP;
    else if (SCHEME_SYMBOLP(args[1]) 
             && !SCHEME_SYM_WEIRDP(args[1]) 
             && !strcmp(SCHEME_SYM_VAL(args[1]), "terminate"))
      kind = MZEXN_BREAK_TERMINATE;
    else
      scheme_wrong_contract("place-break", "(or/c #f 'hang-up 'terminate)", 1, argc, args);
  }

  do_place_break(place, kind);

  return scheme_void;
}

static int place_deadp(Scheme_Object *place) {
  Scheme_Place_Object *place_obj;
  int dead = 0;
  place_obj = (Scheme_Place_Object*) ((Scheme_Place *)place)->place_obj;

  if (place_obj == NULL) {
    return 1;
  }
  else
  {
    mzrt_mutex_lock(place_obj->lock);

    dead = place_obj->die | place_obj->dead;

    mzrt_mutex_unlock(place_obj->lock);
  }
  if (dead) { return 1; }
  return 0;
}

static Scheme_Object *make_place_dead(int argc, Scheme_Object *argv[])
{ 
  Scheme_Object *b;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_place_type))
    scheme_wrong_contract("place-dead-evt", "place?", 0, argc, argv);

  b = scheme_alloc_small_object();
  b->type = scheme_place_dead_type;
  SCHEME_PTR_VAL(b) = argv[0];
  return b;
} 
  
static int place_dead_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo) {
  if (place_deadp(SCHEME_PTR_VAL(o))) {
    scheme_set_sync_target(sinfo, o, NULL, NULL, 0, 0, NULL);
    return 1;
  }
  return 0;
}

# if defined(MZ_PLACES_WAITPID)
/*============= SIGCHLD SIGNAL HANDLING =============*/

/* If SIGCHLD is unblocked, it gets delivered to a random thread
   --- not necessarily on in the right place for the subprocess.
   To avoid that problem, we centralize SIGCHLD handling here, and
   then dispatch back out to specific places as they request 
   information. */

#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>

typedef struct Child_Status {
  int pid;
  int status;
  char done;
  char unneeded; /* not in a group; result not needed */
  char is_group;
  void *signal_fd;
  struct Child_Status *next;
  struct Child_Status *next_unused; /* see unused_pid_statuses */
} Child_Status;

SHARED_OK static Child_Status *child_statuses = NULL;
SHARED_OK static mzrt_mutex* child_status_lock = NULL;
SHARED_OK static mzrt_mutex* child_wait_lock = NULL; /* ordered before status lock */

SHARED_OK static int started_thread, pending_children;

/* When the Racket process value for a process in a different group becomes 
   GC-unreachable before a waitpid() on the process, then we 
   need to keep waiting on the pid to let the OS gc the process.
   This list is especially needed for processes that we create in
   their own group, but it's also needed for processes that put
   themselves in their own group (which we conservatively assume
   can be any child process).
   This list is protect by the wait lock. */
SHARED_OK static Child_Status *unused_pid_statuses = NULL;

static void add_group_signal_fd(void *signal_fd);
static void remove_group_signal_fd(void *signal_fd);
static void do_group_signal_fds();

static void add_child_status(int pid, int status) {
  Child_Status *st;

  /* Search for existing record, which will have a signal_fd: */
  mzrt_mutex_lock(child_status_lock);
  for (st = child_statuses; st; st = st->next) {
    if (st->pid == pid)
      break;
  }

  if (!st) {
    /* must have terminated before it was registered
       (and since we detected it, it must not be a group) */
    st = malloc(sizeof(Child_Status));
    st->pid = pid;
    st->signal_fd = NULL;
    st->next = child_statuses;
    child_statuses = st;
    st->next_unused = NULL;
    st->unneeded = 0;
    st->is_group = 0;
  }
  st->status = status;
  st->done = 1;

  if (st->signal_fd && st->is_group)
    remove_group_signal_fd(st->signal_fd);

  mzrt_mutex_unlock(child_status_lock);
  
  if (st->signal_fd)
    scheme_signal_received_at(st->signal_fd);
  if (st->unneeded)
    (void)scheme_get_child_status(st->pid, 0, 0, NULL);
}

static int raw_get_child_status(int pid, int *status, int done_only, int do_remove, int do_free) {
  Child_Status *st;
  Child_Status *prev;
  int found = 0;

  for (st = child_statuses, prev = NULL; st; prev = st, st = st->next) {
    if (st->pid == pid) {
      if (!done_only || st->done) {
        if (status)
          *status = st->status;
        found = 1;
        if (do_remove) {
          if (prev)
            prev->next = st->next;
          else
            child_statuses = st->next;
        }
        if (do_free)
          free(st);
      }
      break;
    }
  }
  return found;
}

int scheme_get_child_status(int pid, int is_group, int can_check_group, int *status) {
  int found = 0;

  /* Check specific pid, in case the child has its own group
     (either given by Racket or given to itself): */
  if (can_check_group) {
    pid_t pid2;
    int status;

    do {
      pid2 = waitpid((pid_t)pid, &status, WNOHANG);
    } while ((pid2 == -1) && (errno == EINTR));

    if (pid2 > 0)
      add_child_status(pid, scheme_extract_child_status(status));
  }

  mzrt_mutex_lock(child_status_lock);
  found = raw_get_child_status(pid, status, 1, 1, 1);
  mzrt_mutex_unlock(child_status_lock);
  /* printf("scheme_get_child_status found %i pid %i status %i\n", found,  pid, *status); */

  return found;
}

int scheme_places_register_child(int pid, int is_group, void *signal_fd, int *status)
{
  int found = 0;

  mzrt_mutex_lock(child_status_lock);

  /* The child may have terminated already: */
  found = raw_get_child_status(pid, status, 0, 0, 0);

  if (!found) {
    /* Create a record for the child: */
    Child_Status *st;
    st = malloc(sizeof(Child_Status));
    st->pid = pid;
    st->signal_fd = signal_fd;
    st->status = 0;
    st->unneeded = 0;
    st->done = 0;
    st->is_group = is_group;

    st->next = child_statuses;
    child_statuses = st;
    st->next_unused = NULL;

    if (is_group)
      add_group_signal_fd(signal_fd);
  }

  mzrt_mutex_unlock(child_status_lock);
  return found;
}

static void *mz_proc_thread_signal_worker(void *data) {
  int status;
  int pid, check_pid, is_group;
  sigset_t set;
  Child_Status *unused_status, *prev_unused, *next;

  sigemptyset(&set);
  sigaddset(&set, SIGCHLD);

  while (1) {
    int rc;
    int signalid;

    do {
      rc = sigwait(&set, &signalid);
      if (rc == -1) {
        if (errno != EINTR) {
          fprintf(stderr, "unexpected error from sigwait(): %d\n", errno);
        }
      }
    } while (rc == -1 && errno == EINTR);

    mzrt_mutex_lock(child_status_lock);
    do_group_signal_fds();
    mzrt_mutex_unlock(child_status_lock);

    mzrt_mutex_lock(child_wait_lock);

    unused_status = unused_pid_statuses;
    prev_unused = NULL;

    do {
      if (unused_status) {
        /* See unused_pid_statuses above */
        check_pid = unused_status->pid;
        is_group = 1;
      } else {
        /* We wait only on processes in the same group as Racket,
           because detecting the termination of a group's main process
           disables our ability to terminate all processes in the group. */
        if (pending_children)
          check_pid = 0; /* => processes in the same group as Racket */
        else
          check_pid = -1; /* don't check */
        is_group = 0;
      }

      if (check_pid == -1) {
        pid = -1;
        errno = ECHILD;
      } else
        pid = waitpid(check_pid, &status, WNOHANG);

      if (pid == -1) {
        if (errno == EINTR) {
          /* try again */
          pid = 1;
        } else if (!is_group && (errno == ECHILD)) {
          /* no more to check */
        } else {
          fprintf(stderr, "unexpected error from waitpid(%d[%d]): %d\n", 
                  check_pid, is_group, errno);
          if (is_group) {
            prev_unused = unused_status;
            unused_status = unused_status->next;
          } 
        }
      } else if (pid > 0) {
        /* printf("SIGCHILD pid %i with status %i %i\n", pid, status, WEXITSTATUS(status)); */
        if (is_group) {
          next = unused_status->next_unused;
          if (prev_unused)
            prev_unused->next_unused = next;
          else
            unused_pid_statuses = next;
          free(unused_status);
          unused_status = next;
        } else {
          /* Double-check for pid in unused_pid_statuses, since
             it may have completed between the pid-specific waitpid and the
             non-group waitpid: */
          prev_unused = NULL;
          for (unused_status = unused_pid_statuses; unused_status; unused_status = unused_status->next_unused) {
            if (unused_status->pid == pid)
              break;
            prev_unused = unused_status;
          }
          if (!unused_status) {
            /* not in unused_pid_statuses: */
            add_child_status(pid, scheme_extract_child_status(status));
          } else {
            if (prev_unused)
              prev_unused->next_unused = unused_status->next_unused;
            else
              unused_pid_statuses = unused_status->next_unused;
            free(unused_status);
            unused_status = NULL;
          }
        }
      } else {
        if (is_group) {
          prev_unused = unused_status;
          unused_status = unused_status->next_unused;
        }
      }
    } while ((pid > 0) || is_group);

    mzrt_mutex_unlock(child_wait_lock);
  }

  return NULL;
}

void scheme_done_with_process_id(int pid, int is_group)
{
  Child_Status *st;
  int keep_unused = 1; /* assume that any process can be in a new group */

  mzrt_mutex_lock(child_wait_lock); /* protects unused_pid_statuses */
  mzrt_mutex_lock(child_status_lock);

  for (st = child_statuses; st; st = st->next) {
    if (st->pid == pid) {
      if (!st->done) {
        if (keep_unused) {
          st->next_unused = unused_pid_statuses;
          unused_pid_statuses = st;
          if (st->signal_fd)
            remove_group_signal_fd(st->signal_fd);
        } else
          st->unneeded = 1;
        st->signal_fd = NULL;
      }
      break;
    }
  }

  if (st && (keep_unused || st->done)) {
    /* remove it from normal list: */
    raw_get_child_status(pid, NULL, 0, 1, st->done);
  }

  mzrt_mutex_unlock(child_status_lock);
  mzrt_mutex_unlock(child_wait_lock);
}

static void got_sigchld() XFORM_SKIP_PROC
{ 
  if(-1 == write(2, "SIGCHLD handler called (some thread has SIGCHLD unblocked)\n", 59)) {
    
  }
}

void scheme_places_block_child_signal() XFORM_SKIP_PROC
{
  sigset_t set;

  /* Mac OS X seems to need a handler installed for SIGCHLD to be
     delivered, since the default is to drop the signal. Also, this
     handler serves as a back-up alert if some thread is created that
     does not block SIGCHLD.
     Solaris, meanwhile, seems to unmask SIGCHLD as a result of
     setting a handler, so do this before masking the signal. */
  MZ_SIGSET(SIGCHLD, got_sigchld);

  sigemptyset(&set);
  sigaddset(&set, SIGCHLD);
  sigprocmask(SIG_BLOCK, &set, NULL);
}

void scheme_places_unblock_child_signal() XFORM_SKIP_PROC
{
  sigset_t set;

  MZ_SIGSET(SIGCHLD, SIG_DFL);

  sigemptyset(&set);
  sigaddset(&set, SIGCHLD);
  sigprocmask(SIG_UNBLOCK, &set, NULL);
}

void scheme_places_start_child_signal_handler()
{
  mzrt_mutex_create(&child_status_lock);
  mzrt_mutex_create(&child_wait_lock);
}

void scheme_wait_suspend()
{
  mzrt_mutex_lock(child_wait_lock);
}

void scheme_wait_resume()
{
  mzrt_mutex_unlock(child_wait_lock);
}

void scheme_starting_child()
{
  mzrt_mutex_lock(child_wait_lock);

  if (!started_thread) {
    mz_proc_thread *signal_thread;  

    signal_thread = mz_proc_thread_create(mz_proc_thread_signal_worker, NULL);
    mz_proc_thread_detach(signal_thread);
    started_thread = 1;
  }

  pending_children++;

  mzrt_mutex_unlock(child_wait_lock);
}

void scheme_ended_child()
{
  mzrt_mutex_lock(child_wait_lock);
  --pending_children;
  mzrt_mutex_unlock(child_wait_lock);
}

/* ---------------------------------------------------------------------- */

/* When a place has a process-group that it may be waiting on, the we
   need to wake up the place whenever any SIGCHLD is received, since
   the SIGDCHLD may apply to one of those places.
   The list of signal_fds is protected by the status lock. */

typedef struct Group_Signal_FD {
  void *signal_fd;
  int refcount;
} Group_Signal_FD;

SHARED_OK static Group_Signal_FD *signal_fds;
SHARED_OK static int signal_fd_count;

static void add_group_signal_fd(void *signal_fd)
{
  int i, count = 0;
  Group_Signal_FD *a;

  for (i = 0; i < signal_fd_count; i++) {
    if (signal_fds[i].refcount) {
      count++;
      if (signal_fds[i].signal_fd == signal_fd) {
        signal_fds[i].refcount++;
        return;
      }      
    }
  }

  if (count == signal_fd_count) {
    signal_fd_count = (signal_fd_count + 4) * 2;
    a = (Group_Signal_FD *)malloc(sizeof(Group_Signal_FD) * signal_fd_count);
    memset(a, 0, sizeof(Group_Signal_FD) * signal_fd_count);
    memcpy(a, signal_fds, sizeof(Group_Signal_FD) * count);
    if (signal_fds) free(signal_fds);
    signal_fds = a;
  }

  for (i = 0; i < signal_fd_count; i++) {
    if (!signal_fds[i].refcount) {
      signal_fds[i].signal_fd = signal_fd;
      signal_fds[i].refcount = 1;
      break;
    }
  }
}

static void remove_group_signal_fd(void *signal_fd)
{
  int i;

  for (i = 0; i < signal_fd_count; i++) {
    if (signal_fds[i].refcount) {
      if (signal_fds[i].signal_fd == signal_fd) {
        --signal_fds[i].refcount;
        return;
      }
    }
  }
}

static void do_group_signal_fds()
{
  int i;

  for (i = 0; i < signal_fd_count; i++) {
    if (signal_fds[i].refcount) {
      scheme_signal_received_at(signal_fds[i].signal_fd);
    }
  }
}

#endif

/* ---------------------------------------------------------------------- */

static int place_wait_ready(Scheme_Object *_p) {
  Scheme_Place *p = (Scheme_Place *)_p;
  int done;

  if (!p->place_obj) return 1;

  mzrt_mutex_lock(p->place_obj->lock);
  done = p->place_obj->dead;
  mzrt_mutex_unlock(p->place_obj->lock);

  if (done) {
    do_place_kill(p); /* sets result, frees place */
    /* wait for pumper threads to finish */
    return 1;
  }

  return 0;
}

static Scheme_Object *place_wait(int argc, Scheme_Object *args[]) {
  Scheme_Place          *place;
  place = (Scheme_Place *) args[0];

  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type))
    scheme_wrong_contract("place-wait", "place?", 0, argc, args);
  
  scheme_block_until(place_wait_ready, NULL, (Scheme_Object*)place, 0);

  if (SCHEME_VECTORP(place->pumper_threads)) {
    int i;
    for (i=0; i<3; i++) {
      Scheme_Object *tmp;
      tmp = SCHEME_VEC_ELS(place->pumper_threads)[i];
      if (SCHEME_THREADP(tmp)) 
        scheme_thread_wait(tmp);
    }
  }

  return scheme_make_integer(place->result);
}

static Scheme_Object *place_p(int argc, Scheme_Object *args[])
{
  return SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type) ? scheme_true : scheme_false;
}

static Scheme_Object *do_places_deep_copy(Scheme_Object *so, int mode, int gcable, 
                                          Scheme_Object **master_chain,
                                          Scheme_Object **invalid_object) 
{
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  Scheme_Hash_Table *ht = NULL;
  return places_deep_copy_worker(so, &ht, mode, gcable, gcable, master_chain, invalid_object);
#else
  return so;
#endif
}

static void places_prepare_direct(Scheme_Object *so) {
  (void)do_places_deep_copy(so, mzPDC_CHECK, 1, NULL, NULL);
}

static Scheme_Object *places_deep_direct_uncopy(Scheme_Object *so) {
  return do_places_deep_copy(so, mzPDC_DIRECT_UNCOPY, 1, NULL, NULL);
}

static void bad_place_message(Scheme_Object *so) {
  scheme_contract_error("place-channel-put", 
                        "value not allowed in a message", 
                        "value", 1, so,
                        NULL);
}

static void bad_place_message2(Scheme_Object *so, Scheme_Object *o, int can_raise_exn) {
  Scheme_Object *l;
  Scheme_Vector *v = (Scheme_Vector *) o;
  if (v) {
    if (SCHEME_VEC_ELS(v)[0]) {
      l = SCHEME_VEC_ELS(v)[0];
      while (SCHEME_PAIRP(l)) {
        scheme_close_file_fd(SCHEME_INT_VAL(SCHEME_CAR(l)));
        l = SCHEME_CDR(l);
        SCHEME_USE_FUEL(1);
      }
    }
    if (SCHEME_VEC_ELS(v)[1]) {
      l = SCHEME_VEC_ELS(v)[1];
      while (SCHEME_PAIRP(l)) {
        scheme_close_socket_fd(SCHEME_INT_VAL(SCHEME_CAR(l)));
        l = SCHEME_CDR(l);
        SCHEME_USE_FUEL(1);
      }
    }
  }
  if (can_raise_exn)
    bad_place_message(so);
}

static void push_duped_fd(Scheme_Object **fd_accumulators, intptr_t slot, intptr_t dupfd) {
  Scheme_Object *tmp;
  Scheme_Vector *v; 
  if (fd_accumulators) {
    if (!*fd_accumulators) {
      tmp = scheme_make_vector(2, scheme_null);
      *fd_accumulators = tmp;
    }
    v = (Scheme_Vector*) *fd_accumulators;
    
    tmp = scheme_make_pair(scheme_make_integer(dupfd), SCHEME_VEC_ELS(v)[slot]);
    SCHEME_VEC_ELS(v)[slot] = tmp;
  }
}

static Scheme_Object *trivial_copy(Scheme_Object *so, Scheme_Object **master_chain)
{
  switch (SCHEME_TYPE(so)) {
    case scheme_integer_type:
    case scheme_true_type:
    case scheme_false_type:
    case scheme_null_type:
    case scheme_void_type:
      return so;
    case scheme_byte_string_type:
    case scheme_flvector_type:
#ifdef MZ_LONG_DOUBLE
    case scheme_extflvector_type:
#endif
    case scheme_fxvector_type:
      if (SHARED_ALLOCATEDP(so)) {
        scheme_hash_key(so);
        if (master_chain) {
          /* Keep track of all the objects that are in a message that
             refer to master-allocated objects, so that the
             corresponding objects can be marked during a master GC,
             in case one happens before the message is received. */
          Scheme_Object *mc;
          mc = scheme_make_raw_pair(so, *master_chain);
          *master_chain = mc;
        }
        return so;
    }
  }

  return NULL;
}

static Scheme_Object *shallow_types_copy(Scheme_Object *so, Scheme_Hash_Table *ht, 
                                         Scheme_Object **fd_accumulators, intptr_t *delayed_errno, 
                                         int mode, int can_raise_exn,
                                         Scheme_Object **master_chain,
                                         Scheme_Object **invalid_object) {
  Scheme_Object *new_so;
  int copy_mode = ((mode == mzPDC_COPY) || (mode == mzPDC_UNCOPY) || (mode == mzPDC_DIRECT_UNCOPY));

  new_so = trivial_copy(so, master_chain);
  if (new_so) return new_so;

  new_so = so;

  switch (SCHEME_TYPE(so)) {
    case scheme_place_type:
      so = ((Scheme_Place *) so)->channel;
      new_so = so;
    case scheme_place_bi_channel_type: /* ^^^ fall through ^^* */
      if (copy_mode) {
        Scheme_Place_Bi_Channel *ch;
        ch = place_bi_channel_malloc();
        ch->link->sendch = ((Scheme_Place_Bi_Channel *)so)->link->sendch;
        ch->link->recvch = ((Scheme_Place_Bi_Channel *)so)->link->recvch;

        if ((mode == mzPDC_COPY) || (mode == mzPDC_DIRECT_UNCOPY))
          bi_channel_refcount(ch, 1);
        if ((mode == mzPDC_UNCOPY) || (mode == mzPDC_DIRECT_UNCOPY))
          bi_channel_set_finalizer(ch);

        if (master_chain) {
          /* See setting of master_chain in trivial_copy(): */
          new_so = scheme_make_raw_pair((Scheme_Object *)ch->link->sendch, *master_chain);
          new_so = scheme_make_raw_pair((Scheme_Object *)ch->link->recvch, new_so);
          *master_chain = new_so;
        }
        new_so = (Scheme_Object *)ch;
      } else if (mode == mzPDC_CLEAN) {
        bi_channel_refcount((Scheme_Place_Bi_Channel *)so, -1);
      } else if (mode == mzPDC_DESER) {
        bi_channel_set_finalizer((Scheme_Place_Bi_Channel *)so);
      }
      break;
    case scheme_char_type:
      if (copy_mode)
        new_so = scheme_make_char(SCHEME_CHAR_VAL(so));
      break;
    case scheme_bignum_type:
      if (copy_mode)
        new_so = scheme_bignum_copy(so);
      break;
    case scheme_rational_type:
      if (copy_mode) {
        Scheme_Object *n;
        Scheme_Object *d;
        n = scheme_rational_numerator(so);
        d = scheme_rational_denominator(so);
        n = shallow_types_copy(n, NULL, fd_accumulators, delayed_errno, mode, can_raise_exn, master_chain, invalid_object);
        d = shallow_types_copy(d, NULL, fd_accumulators, delayed_errno, mode, can_raise_exn, master_chain, invalid_object);
        new_so = scheme_make_rational(n, d);
      }
      break;
    case scheme_float_type:
      if (copy_mode)
        new_so = scheme_make_float(SCHEME_FLT_VAL(so));
      break;
    case scheme_double_type:
      if (copy_mode)
        new_so = scheme_make_double(SCHEME_DBL_VAL(so));
      break;
#ifdef MZ_LONG_DOUBLE
    case scheme_long_double_type:
      if (copy_mode)
        new_so = scheme_make_long_double(SCHEME_LONG_DBL_VAL(so));
      break;
#endif
    case scheme_complex_type:
      if (copy_mode) {
        Scheme_Object *r;
        Scheme_Object *i;
        r = scheme_complex_real_part(so);
        i = scheme_complex_imaginary_part(so);
        r = shallow_types_copy(r, NULL, fd_accumulators, delayed_errno, mode, can_raise_exn, master_chain, invalid_object);
        i = shallow_types_copy(i, NULL, fd_accumulators, delayed_errno, mode, can_raise_exn, master_chain, invalid_object);
        new_so = scheme_make_complex(r, i);
      }
      break;
    case scheme_char_string_type:
      if (copy_mode) {
        new_so = scheme_make_sized_offset_char_string(SCHEME_CHAR_STR_VAL(so), 0, SCHEME_CHAR_STRLEN_VAL(so), 1);
        SCHEME_SET_IMMUTABLE(new_so);
      }
      break;
    case scheme_byte_string_type:
      /* not allocated as shared, since that's covered above */
      if (copy_mode) {
        new_so = scheme_make_sized_offset_byte_string(SCHEME_BYTE_STR_VAL(so), 0, SCHEME_BYTE_STRLEN_VAL(so), 1);
        SCHEME_SET_IMMUTABLE(new_so);
      }
      break;
    case scheme_unix_path_type:
    case scheme_windows_path_type:
      if (copy_mode)
        new_so = scheme_make_sized_offset_kind_path(SCHEME_BYTE_STR_VAL(so), 0, SCHEME_BYTE_STRLEN_VAL(so), 1,
                                                    SCHEME_TYPE(so));
      break;
    case scheme_symbol_type:
      if (mode == mzPDC_COPY) {
        new_so = scheme_make_sized_offset_byte_string((char *)so, SCHEME_SYMSTR_OFFSET(so), SCHEME_SYM_LEN(so), 1);
        if (SCHEME_SYM_UNINTERNEDP(so)) {
          MZ_OPT_HASH_KEY(&((Scheme_Symbol*)new_so)->iso) = 0x1;
        } else if (SCHEME_SYM_PARALLELP(so)) {
          MZ_OPT_HASH_KEY(&((Scheme_Symbol*)new_so)->iso) = 0x2;
        }
        new_so->type = scheme_serialized_symbol_type;
      } else if (mode == mzPDC_DIRECT_UNCOPY) {
        char *str, buf[64];
        intptr_t len;
        len = SCHEME_SYM_LEN(so);
        if (len < 64)
          str = buf;
        else
          str = (char *)scheme_malloc_atomic(len);
        memcpy(str, SCHEME_SYM_VAL(so), len);
        if (SCHEME_SYM_UNINTERNEDP(so))
          new_so = scheme_make_exact_symbol(str, len);
        else if (SCHEME_SYM_PARALLELP(so))
          new_so = scheme_intern_exact_parallel_symbol(str, len);
        else
          new_so = scheme_intern_exact_symbol(str, len);
      } else if (mode != mzPDC_CHECK) {
        scheme_log_abort("encountered symbol in bad mode");
        abort();
      }
      break;
    case scheme_serialized_symbol_type:
      if ((mode == mzPDC_UNCOPY) || (mode == mzPDC_DESER)) {
        if (SCHEME_SYM_UNINTERNEDP(so)) {
          new_so = scheme_make_exact_symbol(SCHEME_BYTE_STR_VAL(so), SCHEME_BYTE_STRLEN_VAL(so));
        }
        else if (SCHEME_SYM_PARALLELP(so)) {
          new_so = scheme_intern_exact_parallel_symbol(SCHEME_BYTE_STR_VAL(so), SCHEME_BYTE_STRLEN_VAL(so));
        }
        else {
          new_so = scheme_intern_exact_symbol(SCHEME_BYTE_STR_VAL(so), SCHEME_BYTE_STRLEN_VAL(so));
        }
      } else if (mode != mzPDC_CLEAN) {
        scheme_log_abort("encountered serialized symbol in bad mode");
        abort();
      }
      break;
    case scheme_keyword_type:
      if (mode == mzPDC_COPY) {
        new_so = scheme_make_sized_offset_byte_string((char *)so, SCHEME_SYMSTR_OFFSET(so), SCHEME_SYM_LEN(so), 1);
        new_so->type = scheme_serialized_keyword_type;
      } else if (mode == mzPDC_DIRECT_UNCOPY) {
        char *str, buf[64];
        intptr_t len;
        len = SCHEME_SYM_LEN(so);
        if (len < 64)
          str = buf;
        else
          str = (char *)scheme_malloc_atomic(len);
        memcpy(str, SCHEME_SYM_VAL(so), len);
        new_so = scheme_intern_exact_keyword(str, len);
      } else if (mode != mzPDC_CHECK) {
        scheme_log_abort("encountered keyword in bad mode");
        abort();
      }
      break;
    case scheme_serialized_keyword_type:
      if ((mode == mzPDC_UNCOPY) || (mode == mzPDC_DESER)) {
        new_so = scheme_intern_exact_keyword(SCHEME_BYTE_STR_VAL(so), SCHEME_BYTE_STRLEN_VAL(so));
      } else if (mode != mzPDC_CLEAN) {
        scheme_log_abort("encountered serialized keyword in bad mode");
        abort();
      }
      break;
    case scheme_fxvector_type:
      /* not allocated as shared, since that's covered above */
      if (copy_mode) {
        Scheme_Vector *vec;
        intptr_t i;
        intptr_t size = SCHEME_FXVEC_SIZE(so);
        vec = scheme_alloc_fxvector(size);

        for (i = 0; i < size; i++) {
          SCHEME_FXVEC_ELS(vec)[i] = SCHEME_FXVEC_ELS(so)[i];
        }
        new_so = (Scheme_Object *) vec;
      }
      break;
    case scheme_flvector_type:
      /* not allocated as shared, since that's covered above */
      if (copy_mode) {
        Scheme_Double_Vector *vec;
        intptr_t i;
        intptr_t size = SCHEME_FLVEC_SIZE(so);
        vec = scheme_alloc_flvector(size);

        for (i = 0; i < size; i++) {
          SCHEME_FLVEC_ELS(vec)[i] = SCHEME_FLVEC_ELS(so)[i];
        }
        new_so = (Scheme_Object *) vec;
      }
      break;
#ifdef MZ_LONG_DOUBLE
    case scheme_extflvector_type:
      /* not allocated as shared, since that's covered above */
      if (copy_mode) {
        Scheme_Long_Double_Vector *vec;
        intptr_t i;
        intptr_t size = SCHEME_EXTFLVEC_SIZE(so);
        vec = scheme_alloc_extflvector(size);

        for (i = 0; i < size; i++) {
          SCHEME_EXTFLVEC_ELS(vec)[i] = SCHEME_EXTFLVEC_ELS(so)[i];
        }
        new_so = (Scheme_Object *) vec;
      }
      break;
#endif
    case scheme_cpointer_type:
      if (SCHEME_CPTR_FLAGS(so) & 0x1) {
        if (copy_mode) {
          Scheme_Object *o;
          Scheme_Object *o2;
          if (SCHEME_CPTR_FLAGS(so) & 0x2) {
            o = (Scheme_Object *)scheme_malloc_small_tagged(sizeof(Scheme_Offset_Cptr));
            SCHEME_CPTR_FLAGS(o) |= 0x2;
            ((Scheme_Offset_Cptr *)o)->offset = ((Scheme_Offset_Cptr *)so)->offset;
          }
          else
            o = (Scheme_Object *)scheme_malloc_small_tagged(sizeof(Scheme_Cptr));

          o->type = scheme_cpointer_type;
          SCHEME_CPTR_FLAGS(o) |= 0x1;
          SCHEME_CPTR_VAL(o) = SCHEME_CPTR_VAL(so);
          o2 = SCHEME_CPTR_TYPE(so);
          if (o2)
            o2 = shallow_types_copy(o2, NULL, fd_accumulators, delayed_errno, mode, 
                                    can_raise_exn, master_chain, invalid_object);
          SCHEME_CPTR_TYPE(o) = o2;

          new_so = o;
        } else {
          if (SCHEME_CPTR_TYPE(so)) {
            (void)shallow_types_copy(SCHEME_CPTR_TYPE(so), NULL, fd_accumulators, delayed_errno, mode, 
                                     can_raise_exn, master_chain, invalid_object);
          }
        }
      }
      else {
        bad_place_message2(so, *fd_accumulators, can_raise_exn);
        if (invalid_object) *invalid_object = so;
        return NULL;
      }
      break;
    case scheme_input_port_type:
    case scheme_output_port_type:
      {
        intptr_t fd;
        if (scheme_get_port_socket(so, &fd)) {
#ifdef USE_TCP
          if (mode == mzPDC_COPY) {
            Scheme_Object *tmp;
            Scheme_Object *portname;
            Scheme_Serialized_Socket_FD *ssfd;
            int dupfd;
            dupfd = scheme_dup_socket(fd);
            if (dupfd == -1) {
              if (can_raise_exn)
                scheme_system_error("dynamic-place", "socket dup", scheme_socket_errno());
              if (delayed_errno) {
                intptr_t tmp;
                tmp = scheme_socket_errno();
                *delayed_errno = tmp;
              }
              return NULL;
            }
            push_duped_fd(fd_accumulators, 1, dupfd);
            ssfd = scheme_malloc_tagged(sizeof(Scheme_Serialized_Socket_FD));
            ssfd->so.type = scheme_serialized_tcp_fd_type;
            ssfd->type = so->type;
            ssfd->fd = dupfd;
            portname = scheme_port_name(so);
            tmp = shallow_types_copy(portname, ht, fd_accumulators, delayed_errno, mode, can_raise_exn,
                                     master_chain, invalid_object);
            ssfd->name = tmp;
            return (Scheme_Object *)ssfd;
          }
#else
          scheme_signal_error("sockets aren't supported");
#endif
        }
        else if (SCHEME_TRUEP(scheme_file_stream_port_p(1, &so))) {
          if (scheme_get_port_file_descriptor(so, &fd)) {
            if (mode == mzPDC_COPY) {
              Scheme_Object *tmp;
              Scheme_Serialized_File_FD *sffd;
              int dupfd;
              sffd = scheme_malloc_tagged(sizeof(Scheme_Serialized_File_FD));
              sffd->so.type = scheme_serialized_file_fd_type;
              scheme_get_serialized_fd_flags(so, sffd);
              tmp = shallow_types_copy(scheme_port_name(so), ht, fd_accumulators, delayed_errno, mode, 
                                       can_raise_exn, master_chain, invalid_object);
              sffd->name = tmp;
              dupfd = scheme_dup_file(fd);
              if (dupfd == -1) {
                if (can_raise_exn)
                  scheme_system_error("dynamic-place", "port dup", scheme_errno());
                if (delayed_errno) {
                  intptr_t tmp;
                  tmp = scheme_errno();
                  *delayed_errno = tmp;
                }
                return NULL;
              }
              push_duped_fd(fd_accumulators, 0, dupfd);
              sffd->fd = dupfd;
              sffd->type = so->type;
              new_so = (Scheme_Object *) sffd;
            }
          }
          else {
            bad_place_message2(so, *fd_accumulators, can_raise_exn);
            if (invalid_object) *invalid_object = so;
            return NULL;
          }
        }
        else {
          bad_place_message2(so, *fd_accumulators, can_raise_exn);
          if (invalid_object) *invalid_object = so;
          return NULL;
        }
      }
      break;
    case scheme_serialized_tcp_fd_type:
      {
        if ((mode == mzPDC_UNCOPY) || (mode == mzPDC_DIRECT_UNCOPY) || (mode == mzPDC_DESER)) {
          Scheme_Object *in;
          Scheme_Object *out;
          Scheme_Object *name;
          int type = ((Scheme_Serialized_Socket_FD *) so)->type;
          int fd   = ((Scheme_Serialized_Socket_FD *) so)->fd;
          name = ((Scheme_Serialized_Socket_FD *) so)->name;

          /* scheme_socket_to_ports(fd, "tcp-accepted", 1, &in, &out); */
          if (type == scheme_input_port_type) {
            scheme_socket_to_input_port(fd, name, 1, &in);
            /* scheme_tcp_abandon_port(out); */
            new_so = in;
          }
          else {
            scheme_socket_to_output_port(fd, name, 1, &out);
            /* scheme_tcp_abandon_port(in); */
            new_so = out;
          }
        } else if (mode == mzPDC_CLEAN) {
          int fd = ((Scheme_Simple_Object *) so)->u.two_int_val.int2;
          scheme_close_socket_fd(fd);
        } else {
          scheme_log_abort("encountered serialized TCP socket in bad mode");
          abort();
        }
      }
      break;
    case scheme_serialized_file_fd_type:
      {
        if ((mode == mzPDC_UNCOPY) || (mode == mzPDC_DIRECT_UNCOPY) || (mode == mzPDC_DESER)) {
          Scheme_Serialized_File_FD *ffd;
          Scheme_Object *name;
          int fd;
          int type;
          int regfile;
          int textmode;

          ffd = (Scheme_Serialized_File_FD *) so;
          fd = ffd->fd;
          name = ffd->name;
          type = ffd->type;
          regfile = ffd->regfile;
          textmode = ffd->textmode;

          if (type == scheme_input_port_type) {
            new_so = scheme_make_fd_input_port(fd, name, regfile, textmode);
          }
          else {
            new_so = scheme_make_fd_output_port(fd, name, regfile, textmode, 0);
          }
        } else if (mode == mzPDC_CLEAN) {
          Scheme_Serialized_File_FD *sffd;
          sffd = (Scheme_Serialized_File_FD *) so;
          scheme_close_file_fd(sffd->fd);
        } else {
          scheme_log_abort("encountered serialized fd in bad mode");
          abort();
        }
      }
      break;
    default:
      new_so = NULL;
      break;
  }
  if (ht && new_so) {
    scheme_hash_set(ht, so, new_so);
  }
  return new_so;
}

/* InFinite Stack */
#define IFS_SIZE 512
#define IFS_CACHE_SLOT (IFS_SIZE - 1)
#define IFS_SEGMENT_BOTTOM 1
#define IFS_PREV_SEG_SLOT  0
static Scheme_Object* create_infinite_stack(int gcable) { 
  Scheme_Object **v;

  if (gcable) {
    /* If a GC is not possible, then we prefer to malloc() the stack
       space so that the space doesn't show up as part of the
       message allocation. */
    if (reusable_ifs_stack) {
      v = reusable_ifs_stack;
      reusable_ifs_stack = NULL;
    } else {
      v = GC_malloc(IFS_SIZE * sizeof(Scheme_Object*));
    }
  } else {
    v = malloc(IFS_SIZE * sizeof(Scheme_Object*));
    v[IFS_PREV_SEG_SLOT] = NULL;
    v[IFS_CACHE_SLOT] = NULL;
  }

  return (Scheme_Object *) v;
}
static void  free_infinite_stack(Scheme_Object** st, intptr_t max_depth, int gcable) {
  Scheme_Object **prev;
  if (st[IFS_CACHE_SLOT]) {
    if (!gcable) free(st[IFS_CACHE_SLOT]);
    st[IFS_CACHE_SLOT] = NULL;
  }
  prev = (Scheme_Object **) st[IFS_PREV_SEG_SLOT];
  if (prev) {
    prev[IFS_CACHE_SLOT] = NULL;
  }
  if (!gcable) free(st);
  else if (!reusable_ifs_stack && (max_depth >= 0)) {
    if (max_depth > IFS_SIZE)
      max_depth = IFS_SIZE;
    memset(st, 0, max_depth * sizeof(Scheme_Object*));
    reusable_ifs_stack = st;
  }
}

void scheme_clear_place_ifs_stack()
{
  reusable_ifs_stack = NULL;
}

static MZ_INLINE void inf_push(Scheme_Object **instack, Scheme_Object *item, uintptr_t *indepth, 
                               uintptr_t *maxdepth, int gcable) {
  Scheme_Object **stack = (Scheme_Object **) *instack;
  if (*indepth == IFS_CACHE_SLOT) {
    if (stack[IFS_CACHE_SLOT]) { /* cached */
      stack = (Scheme_Object **) stack[IFS_CACHE_SLOT];
    }
    else { /* no cache */ 
      Scheme_Object *tmp;
      tmp = create_infinite_stack(gcable);
      stack[IFS_CACHE_SLOT] = tmp;
      stack = (Scheme_Object **)stack[IFS_CACHE_SLOT];
      stack[IFS_PREV_SEG_SLOT] = (Scheme_Object *) (*instack);
    }
    *instack = (Scheme_Object *) stack;
    *indepth = IFS_SEGMENT_BOTTOM;
  }

  /* printf("PUSH %p %li %p\n", stack, *indepth, item); */
  stack[((*indepth)++)] = item;
  if (*indepth > *maxdepth)
    *maxdepth = *indepth;
  return;
}

static MZ_INLINE Scheme_Object *inf_pop(Scheme_Object **instack, uintptr_t *indepth, int gcable) {
  Scheme_Object **stack = (Scheme_Object **) *instack;
  Scheme_Object *val;
  if (*indepth == IFS_SEGMENT_BOTTOM) {
    if (stack[IFS_CACHE_SLOT]) { /* already have cached segment, free it*/
      free_infinite_stack((Scheme_Object **) stack[IFS_CACHE_SLOT], -1, gcable);
      stack[IFS_CACHE_SLOT] = NULL;
    }
    if (stack[IFS_PREV_SEG_SLOT]) {
      stack = (Scheme_Object **) stack[IFS_PREV_SEG_SLOT];
      stack[IFS_CACHE_SLOT] = (Scheme_Object *)(*instack);
      *instack = (Scheme_Object*) stack;
      *indepth = IFS_CACHE_SLOT;
    }
    else {
      printf("pop beyond start of inf stack\n");
      abort();
      return NULL;
    }
  }

  val = stack[--(*indepth)];
  /* printf("Pop %p %li %p\n", stack, *indepth, val); */
  stack[*indepth] = NULL;
  return val;
}

static MZ_INLINE Scheme_Object *inf_set(Scheme_Object **instack, int pos, Scheme_Object *item, uintptr_t *indepth) {
  Scheme_Object **stack = (Scheme_Object **) *instack;
  Scheme_Object *old;
  int realpos;
  if (*indepth <= pos + 1) {
    if (stack[IFS_PREV_SEG_SLOT]) {
      stack = (Scheme_Object **) stack[IFS_PREV_SEG_SLOT];
      realpos = (IFS_CACHE_SLOT - (pos + 2)) + *indepth;
    }
    else {
      printf("set beyond start of inf stack\n");
      abort();
      return NULL;
    }
  }
  else { realpos = *indepth - 1 - pos; }

  /* printf("Set %p %i %li %i %p\n", stack, pos, *indepth, realpos, item); */
  old = stack[realpos];
  stack[realpos] = item;
  return old;
}

static MZ_INLINE Scheme_Object *inf_get(Scheme_Object **instack, int pos, uintptr_t *indepth) {
  Scheme_Object **stack = (Scheme_Object **) *instack;
  Scheme_Object *item;
  int realpos;
  if (*indepth <= pos + 1) {
    if (stack[IFS_PREV_SEG_SLOT]) {
      stack = (Scheme_Object **) stack[IFS_PREV_SEG_SLOT];
      realpos = (IFS_CACHE_SLOT - (pos + 2)) + *indepth;
    }
    else {
      printf("get beyond start of inf stack\n");
      abort();
      return NULL;
    }
  }
  else { realpos = *indepth - 1 - pos; }

  item = stack[realpos];

  /* printf("Get %p %i %li %i %p\n", stack, pos, *indepth, realpos, item); */
  return item;
}

/* This code often executes with the master GC switched on, so it
   cannot use the usual stack overflow mechanism or raise exceptions
   in that case. Therefore, it must use its own stack implementation
   for recursion. */
static Scheme_Object *places_deep_copy_worker(Scheme_Object *so, Scheme_Hash_Table **ht, 
                                              int mode, int gcable, int can_raise_exn,
                                              Scheme_Object **master_chain,
                                              Scheme_Object **invalid_object) {
  Scheme_Object *inf_stack = NULL;
  Scheme_Object *reg0 = NULL;
  uintptr_t inf_stack_depth = 0, inf_max_depth = 0;

  Scheme_Object *fd_accumulators = NULL;
  intptr_t delayed_errno = 0;

  int set_mode = ((mode == mzPDC_COPY) 
                  || (mode == mzPDC_UNCOPY) || (mode == mzPDC_DIRECT_UNCOPY) 
                  || (mode == mzPDC_DESER));
  
  /* lifted variables for xform*/
  Scheme_Object *pair;
  Scheme_Object *vec;
  Scheme_Object *nht;
  Scheme_Object *hti;
  Scheme_Object *htk;
  intptr_t i;
  intptr_t size;
  Scheme_Structure *st;
  Scheme_Serialized_Structure *sst;
  Scheme_Struct_Type *stype;
  Scheme_Struct_Type *ptype;
  int local_slots;

#define DEEP_DO_CDR       1
#define DEEP_DO_FIN_PAIR  2
#define DEEP_VEC1         3
#define DEEP_ST1          4   
#define DEEP_ST2          5
#define DEEP_SST1         6
#define DEEP_SST2         7      
#define DEEP_HT1          8
#define DEEP_HT2          9      
#define DEEP_RETURN      10
#define DEEP_DONE        11
#define RETURN do { goto DEEP_RETURN_L; } while(0);
#define ABORT do { goto DEEP_DONE_L; } while(0);
#define IFS_PUSH(x) inf_push(&inf_stack, x, &inf_stack_depth, &inf_max_depth, gcable)
#define IFS_POP inf_pop(&inf_stack, &inf_stack_depth, gcable)
#define IFS_POPN(n) do { int N = (n); while (N > 0) { IFS_POP; N--;} } while(0);
#define IFS_GET(n) inf_get(&inf_stack, (n), &inf_stack_depth)
#define IFS_SET(n, x) inf_set(&inf_stack, (n), x, &inf_stack_depth)
#define GOTO_NEXT_CONT(dest, cont) do { IFS_PUSH(scheme_make_integer(cont)); goto DEEP_DO; } while(0);
#define SET_R0(x) reg0 = x
#define GET_R0() (reg0)

  Scheme_Object *new_so = so;
  int ctr = 0;

  /* First, check for simple values that don't need to be hashed: */
  new_so = shallow_types_copy(so, *ht, &fd_accumulators, &delayed_errno, mode, can_raise_exn, master_chain,
                              invalid_object);
  if (new_so) return new_so;

  if (*ht) {
    Scheme_Object *r; 
    if ((r = scheme_hash_get(*ht, so))) {
      return r;
    }
  }

  if (!*ht) {
    Scheme_Hash_Table *_ht;
    _ht = scheme_make_hash_table(SCHEME_hash_ptr);
    *ht = _ht;
  }

  inf_stack = create_infinite_stack(gcable);
  inf_stack_depth = 1;
  inf_max_depth = 1;

  IFS_PUSH(scheme_make_integer(DEEP_DONE));
  SET_R0(so);

DEEP_DO:
  ctr++;
    
  so = GET_R0();
  new_so = trivial_copy(so, master_chain);
  if (new_so) RETURN;

  if (*ht) {
    if ((new_so = scheme_hash_get(*ht, so))) {
      SET_R0(new_so);
      RETURN;
    }
  }

  new_so = shallow_types_copy(so, *ht, &fd_accumulators, &delayed_errno, mode, 
                              can_raise_exn, master_chain, invalid_object);
  if (new_so) RETURN;
  new_so = so;

  if (gcable && (mode == mzPDC_UNCOPY))
    SCHEME_USE_FUEL(1);

  switch (SCHEME_TYPE(so)) {
    /* --------- pair ----------- */
    case scheme_pair_type:
      /* handle cycles: */
      if ((mode == mzPDC_COPY) || (mode == mzPDC_UNCOPY) || (mode == mzPDC_DIRECT_UNCOPY)) {
        pair = scheme_make_pair(scheme_false, scheme_false);
        SCHEME_PAIR_COPY_FLAGS(pair, so);
      } else
        pair = so;
      scheme_hash_set(*ht, so, pair);

      IFS_PUSH(so); 
      IFS_PUSH(pair); 
      SET_R0(SCHEME_CAR(so));
      GOTO_NEXT_CONT(DEEP_DO, DEEP_DO_CDR);

DEEP_DO_CDR_L:
      pair = IFS_GET(0);
      so   = IFS_GET(1);
      if (set_mode) {
        SCHEME_CAR(pair) = GET_R0();
      }
      SET_R0(SCHEME_CDR(so));
      GOTO_NEXT_CONT(DEEP_DO, DEEP_DO_FIN_PAIR);

DEEP_DO_FIN_PAIR_L:
      pair = IFS_POP; 
      so   = IFS_POP; 
      if (set_mode) {
        SCHEME_CDR(pair) = GET_R0();
        new_so = pair;
      }
      RETURN;
      break;

      /* --------- vector ----------- */
    case scheme_vector_type:
      size = SCHEME_VEC_SIZE(so);

      if ((mode == mzPDC_COPY) || (mode == mzPDC_UNCOPY) || (mode == mzPDC_DIRECT_UNCOPY))
        vec = scheme_make_vector(size, 0);
      else
        vec = so;

      /* handle cycles: */
      scheme_hash_set(*ht, so, vec);
      i = 0;
      
      IFS_PUSH(vec);
      IFS_PUSH(so);
      IFS_PUSH(scheme_make_integer(size));
      IFS_PUSH(scheme_make_integer(i));
      
      if (i < size) {
        SET_R0(SCHEME_VEC_ELS(so)[i]);
        GOTO_NEXT_CONT(DEEP_DO, DEEP_VEC1);
      }
      else {
        goto DEEP_VEC2;
      }

DEEP_VEC1_L:
      /* vector loop*/
      i    = SCHEME_INT_VAL(IFS_GET(0));
      size = SCHEME_INT_VAL(IFS_GET(1));
      so   = IFS_GET(2);
      vec  = IFS_GET(3);
      if (set_mode) {
        SCHEME_VEC_ELS(vec)[i] = GET_R0();
      }
      i++;
      if (i < size) {
        IFS_SET(0, scheme_make_integer(i));
        SET_R0(SCHEME_VEC_ELS(so)[i]);
        GOTO_NEXT_CONT(DEEP_DO, DEEP_VEC1);
      }
      else {
        goto DEEP_VEC2;
      }

DEEP_VEC2:
      i    = SCHEME_INT_VAL(IFS_POP);
      size = SCHEME_INT_VAL(IFS_POP);
      so   = IFS_POP;
      vec  = IFS_POP;

      if (set_mode) {
        SCHEME_SET_IMMUTABLE(vec);
        new_so = vec;
      }
      RETURN;
      break;

      /* --------- structure ----------- */
    case scheme_structure_type:
      st = (Scheme_Structure*)so;
      stype = st->stype;
      ptype = stype->parent_types[stype->name_pos - 1];
      size = stype->num_slots;
      local_slots = stype->num_slots - (ptype ? ptype->num_slots : 0);

      if (!stype->prefab_key) {
        bad_place_message2(so, fd_accumulators, can_raise_exn);
        if (invalid_object) *invalid_object = so;
        new_so = NULL;
        ABORT;
      }
      for (i = 0; i < local_slots; i++) {
        if (!stype->immutables || stype->immutables[i] != 1) {
          bad_place_message2(so, fd_accumulators, can_raise_exn);
          if (invalid_object) *invalid_object = so;
          new_so = NULL;
          ABORT;
        }
      }

      IFS_PUSH((Scheme_Object *)st);
      SET_R0(SCHEME_CDR(stype->prefab_key));
      GOTO_NEXT_CONT(DEEP_DO, DEEP_ST1);

DEEP_ST1_L:
      st = (Scheme_Structure*) IFS_GET(0);
      so = (Scheme_Object *) st;
      size = st->stype->num_slots;
      if (mode == mzPDC_COPY) {
        new_so = scheme_make_serialized_struct_instance(GET_R0(), size);
        sst = (Scheme_Serialized_Structure*)new_so;
      } else if (mode == mzPDC_CHECK) {
        sst = NULL;
      } else {
        scheme_log_abort("encountered structure in bad mode");
        abort();
      }

      /* handle cycles: */
      scheme_hash_set(*ht, so, new_so);

      i = 0;
      if (i < size) {
        IFS_PUSH(scheme_make_integer(size));
        IFS_PUSH(scheme_make_integer(i));
        IFS_PUSH((Scheme_Object *)sst);
        SET_R0(st->slots[i]);
        GOTO_NEXT_CONT(DEEP_DO, DEEP_ST2);
      }
      else {
        IFS_POP;
        RETURN;
      }

DEEP_ST2_L:
      i = SCHEME_INT_VAL(IFS_GET(1)); 
      size = SCHEME_INT_VAL(IFS_GET(2));
      st = (Scheme_Structure*) IFS_GET(3);
      so = (Scheme_Object *) st;
      if (mode == mzPDC_COPY) {
        sst = (Scheme_Serialized_Structure *)IFS_GET(0); 
        sst->slots[i] = GET_R0();
      }
      i++;
      if (i < size) {
        IFS_SET(1, scheme_make_integer(i));
        SET_R0(st->slots[i]);
        GOTO_NEXT_CONT(DEEP_DO, DEEP_ST2);
      }
      else {
        if (mode == mzPDC_COPY)
          new_so = (Scheme_Object *)sst;
        IFS_POPN(4);
        RETURN;
      }
      break;

      /* --------- serialized structure ----------- */
    case scheme_serialized_structure_type:
      sst = (Scheme_Serialized_Structure*)so;
      
      IFS_PUSH((Scheme_Object *)sst);
      SET_R0(sst->prefab_key);
      GOTO_NEXT_CONT(DEEP_DO, DEEP_SST1);

DEEP_SST1_L:
      sst = (Scheme_Serialized_Structure*) IFS_GET(0);
      so = (Scheme_Object *) sst;
      size = sst->num_slots;
      if ((mode == mzPDC_UNCOPY) || (mode == mzPDC_DESER)) {
        stype = scheme_lookup_prefab_type(GET_R0(), size);
        new_so = scheme_make_blank_prefab_struct_instance(stype);

        st = (Scheme_Structure*)new_so;
      } else if (mode == mzPDC_CLEAN) {
        st = NULL;
      } else {
        scheme_log_abort("encountered serialized structure in bad mode");
        abort();
      }

      /* handle cycles: */
      scheme_hash_set(*ht, so, new_so);

      i = 0;
      if (i < size) {
        IFS_PUSH(scheme_make_integer(size));
        IFS_PUSH(scheme_make_integer(i));
        IFS_PUSH((Scheme_Object *)st);
        SET_R0(sst->slots[i]);
        GOTO_NEXT_CONT(DEEP_DO, DEEP_SST2);
      }
      else {
        IFS_POP;
        RETURN;
      }

DEEP_SST2_L:
      i    = SCHEME_INT_VAL(IFS_GET(1));
      size = SCHEME_INT_VAL(IFS_GET(2));
      sst = (Scheme_Serialized_Structure*) IFS_GET(3);
      so = (Scheme_Object *) sst;
      if ((mode == mzPDC_UNCOPY) || (mode == mzPDC_DESER)) {
        st = (Scheme_Structure *) IFS_GET(0); 
        st->slots[i] = GET_R0();
      }
      i++;
      if (i < size) {
        IFS_SET(1, scheme_make_integer(i));
        SET_R0(sst->slots[i]);
        GOTO_NEXT_CONT(DEEP_DO, DEEP_SST2);
      }
      else {
        new_so = (Scheme_Object *)st;
        IFS_POPN(4);
        RETURN;
      }
      break;
    case scheme_hash_table_type:
    case scheme_hash_tree_type:
    case scheme_eq_hash_tree_type:
    case scheme_eqv_hash_tree_type:
      if (set_mode) {
        if (scheme_true == scheme_hash_eq_p(1, &so)) {
          nht = scheme_make_immutable_hasheq(0, NULL);
        }
        else if ( scheme_true == scheme_hash_eqv_p(1, &so)) {
          nht = scheme_make_immutable_hasheqv(0, NULL);
        }
        else if ( scheme_true == scheme_hash_equal_p(1, &so)) {
          nht = scheme_make_immutable_hash(0, NULL);
        }
      }
      else
        nht = so;

      /* handle cycles: */
      scheme_hash_set(*ht, so, nht);
      hti = scheme_hash_table_iterate_start(1,&so);
      i = 0;
      
      IFS_PUSH(nht);
      IFS_PUSH(so);
      IFS_PUSH(hti);
      
      if (SCHEME_INTP(hti)) {
        Scheme_Object *a[2];
        a[0] = so;
        a[1] = hti;
        SET_R0(scheme_hash_table_iterate_key(2, a));
        GOTO_NEXT_CONT(DEEP_DO, DEEP_HT1);
      }
      else {
        goto DEEP_HT3;
      }

DEEP_HT1_L:
      /* hash table loop*/
      hti  = IFS_GET(0);
      so   = IFS_GET(1);
      nht  = IFS_GET(2);
      IFS_PUSH(GET_R0());

      {
        Scheme_Object *a[2];                                                                                  
        a[0] = so;                                                                                            
        a[1] = hti;                                                                                           
        SET_R0(scheme_hash_table_iterate_value(2, a));                                                                 
        GOTO_NEXT_CONT(DEEP_DO, DEEP_HT2);
      }

DEEP_HT2_L:
      htk  = IFS_POP;
      hti  = IFS_GET(0);
      so   = IFS_GET(1);
      nht  = IFS_GET(2);

      if (set_mode) {
        Scheme_Object *a[3];                                                                                  
        a[0] = nht;                                                                                            
        a[1] = htk;                                                                                           
        a[2] = GET_R0();                                                                                           
        nht = scheme_hash_table_put(3, a);
        IFS_SET(2, nht);
      }
      {
        Scheme_Object *a[3];                                                                                  
        a[0] = so;                                                                                            
        a[1] = hti;                                                                                            
        hti = scheme_hash_table_iterate_next(2, a);
      }

      if (SCHEME_INTP(hti)) {
        Scheme_Object *a[2];
        IFS_SET(0, hti);
        a[0] = so;
        a[1] = hti;
        SET_R0(scheme_hash_table_iterate_key(2, a));
        GOTO_NEXT_CONT(DEEP_DO, DEEP_HT1);
      }
      else {
        goto DEEP_HT3;
      }

DEEP_HT3:
      hti  = IFS_POP;
      so   = IFS_POP;
      nht  = IFS_POP;

      if (set_mode) {
        new_so = nht;
      }
      RETURN;
      break;

    default:
      if (delayed_errno)
        scheme_warning("Error serializing place message: %e", delayed_errno);
      bad_place_message2(so, fd_accumulators, can_raise_exn);
      if (invalid_object) *invalid_object = so;
      new_so = NULL;
      ABORT;
      break;
  }

DEEP_RETURN_L:
  {
    ctr--;
    SET_R0(new_so);
    switch(SCHEME_INT_VAL(IFS_POP)) {
      case DEEP_DO_CDR:      goto DEEP_DO_CDR_L;
      case DEEP_DO_FIN_PAIR: goto DEEP_DO_FIN_PAIR_L;
      case DEEP_VEC1:        goto DEEP_VEC1_L;
      case DEEP_ST1:         goto DEEP_ST1_L;
      case DEEP_ST2:         goto DEEP_ST2_L;
      case DEEP_SST1:        goto DEEP_SST1_L;
      case DEEP_SST2:        goto DEEP_SST2_L;
      case DEEP_HT1:         goto DEEP_HT1_L;
      case DEEP_HT2:         goto DEEP_HT2_L;
      case DEEP_RETURN:      goto DEEP_RETURN_L;
      case DEEP_DONE:        goto DEEP_DONE_L;
      default:
        printf("Invalid places_deep_copy_worker state\n");
        abort();
    }
  }

DEEP_DONE_L:
  free_infinite_stack((Scheme_Object **) inf_stack, inf_max_depth, gcable);
  return new_so;

#undef DEEP_DO_CDR
#undef DEEP_DO_FIN_PAIR
#undef DEEP_VEC1
#undef DEEP_ST1
#undef DEEP_ST2
#undef DEEP_SST1
#undef DEEP_SST2
#undef DEEP_HT1
#undef DEEP_TT2
#undef DEEP_RETURN
#undef DEEP_DONE
#undef RETURNS
#undef IFS_PUSH
#undef IFS_POP
#undef IFS_POPN
#undef IFS_GET
#undef IFS_SET
#undef GOTO_NEXT_CONT
#undef GOTO_NEXT

}

#if 0
/* unused code, may be useful when/if we revive shared symbol and prefab key tables */
Scheme_Struct_Type *scheme_make_prefab_struct_type_in_master(Scheme_Object *base,
					Scheme_Object *parent,
					int num_fields,
					int num_uninit_fields,
					Scheme_Object *uninit_val,
					char *immutable_array)
{
# ifdef MZ_PRECISE_GC
  void *original_gc;
# endif
  Scheme_Object *cname;
  Scheme_Object *cuninit_val;
  char *cimm_array = NULL;
  int local_slots = num_fields + num_uninit_fields;
  Scheme_Struct_Type *stype;

# ifdef MZ_PRECISE_GC
  original_gc = GC_switch_to_master_gc();
  scheme_start_atomic();
# endif

  cname = places_deep_uncopy(base);
  cuninit_val = places_deep_uncopy(uninit_val);
  if (local_slots) {
    cimm_array   = (char *)scheme_malloc_atomic(local_slots);
    memcpy(cimm_array, immutable_array, local_slots);
  }
  stype = scheme_make_prefab_struct_type_raw(cname, parent, num_fields, num_uninit_fields, cuninit_val, cimm_array);

# ifdef MZ_PRECISE_GC
  scheme_end_atomic_no_swap();
  GC_switch_back_from_master(original_gc);
# endif

  return stype;
}
#endif

static void log_place_event(const char *what, const char *tag, int has_amount, intptr_t amount)
{
  int id;
  Scheme_Logger *pl;
  Scheme_Object *data, *tag_sym, *t;

  pl = scheme_get_place_logger();
  if (!scheme_log_level_p(pl, SCHEME_LOG_DEBUG))
    return;

  id = scheme_current_place_id;
  tag_sym = scheme_intern_symbol(tag);

  data = scheme_make_blank_prefab_struct_instance(place_event_prefab);
  ((Scheme_Structure *)data)->slots[0] = scheme_make_integer(id);
  ((Scheme_Structure *)data)->slots[1] = tag_sym;
  ((Scheme_Structure *)data)->slots[2] = (has_amount 
                                          ? scheme_make_integer(amount)
                                          : scheme_false);
  t = scheme_make_double(scheme_get_inexact_milliseconds());
  ((Scheme_Structure *)data)->slots[3] = t;

  if (has_amount)
    scheme_log_w_data(pl, SCHEME_LOG_DEBUG, 0, data,
                      what, id, amount);
  else
    scheme_log_w_data(pl, SCHEME_LOG_DEBUG, 0, data,
                      what, id);
}

static void *place_start_proc(void *data_arg) {
  void *stack_base;
  void *rc;
  stack_base = PROMPT_STACK(stack_base);
  rc = place_start_proc_after_stack(data_arg, stack_base);
  stack_base = NULL;
  return rc;
}

static void pause_one_place(Scheme_Place *p)
{
  Scheme_Place_Object *place_obj = p->place_obj;

  if (place_obj) {
    mzrt_mutex_lock(place_obj->lock);
    if (!place_obj->pause) {
      mzrt_sema *s;
      mzrt_sema_create(&s, 0);
      place_obj->pause = s;
    }
    mzrt_mutex_unlock(place_obj->lock);
  }
}

static void resume_one_place_with_lock(Scheme_Place_Object *place_obj)
{
  if (place_obj->pause) {
    mzrt_sema *s = place_obj->pause;
    place_obj->pause = NULL;
    if (!place_obj->pausing) {
      mzrt_sema_destroy(s);
    } else {
      mzrt_sema_post(s);
    }
  }
}

static void resume_one_place(Scheme_Place *p)
{
  Scheme_Place_Object *place_obj = p->place_obj;

  if (place_obj) {
    mzrt_mutex_lock(place_obj->lock);
    resume_one_place_with_lock(place_obj);
    mzrt_mutex_unlock(place_obj->lock);
  }
}

static void pause_all_child_places()
{
  Scheme_Place *p = all_child_places;
  while (p) {
    pause_one_place(p);
    p = p->next;
  }
}

static void resume_all_child_places()
{
  Scheme_Place *p = all_child_places;
  while (p) {
    resume_one_place(p);
    p = p->next;
  }
}

void destroy_place_object_locks(Scheme_Place_Object *place_obj) {
  mzrt_mutex_destroy(place_obj->lock);
  if (place_obj->pause)
    mzrt_sema_destroy(place_obj->pause);
  place_obj->lock = NULL;
  place_obj->pause = NULL;
}

void scheme_place_check_for_interruption() 
{
  Scheme_Place_Object *place_obj;
  char local_die;
  char local_break;
  mzrt_sema *local_pause;

  place_obj = place_object;
  if (!place_obj)
    return;
  
  while (1) {
    mzrt_mutex_lock(place_obj->lock);
  
    local_die = place_obj->die;
    local_break = place_obj->pbreak;
    local_pause = place_obj->pause;
    if (local_die)
      place_obj->die = -1;
    place_obj->pbreak = 0;
    if (local_pause)
      place_obj->pausing = 1;

    mzrt_mutex_unlock(place_obj->lock);
    
    if (local_pause) {
      pause_all_child_places();
      mzrt_sema_wait(local_pause);
      mzrt_sema_destroy(local_pause);
      local_pause = NULL;
      resume_all_child_places();
    } else
      break;
  }
  
  if (local_die > 0)
    scheme_kill_thread(scheme_main_thread);
  if (local_break)
    scheme_break_kind_thread(NULL, local_break);
}

void scheme_place_set_memory_use(intptr_t mem_use)
{
  Scheme_Place_Object *place_obj;

  place_obj = place_object;
  if (!place_obj)
    return;

  mzrt_mutex_lock(place_obj->lock);
  place_obj->memory_use = mem_use;
  mzrt_mutex_unlock(place_obj->lock);

  if (place_obj->parent_signal_handle && place_obj->memory_limit) {
    if (mem_use > place_obj->memory_limit) {
      /* tell the parent place to force a GC, and therefore check
         custodian limits that will kill this place; pause this
         place and its children to give the original place time 
         to kill this one */
      pause_all_child_places();
      mzrt_ensure_max_cas(place_obj->parent_need_gc, 1);
      scheme_signal_received_at(place_obj->parent_signal_handle);
    } else if (mem_use > (1 + place_obj->use_factor) * place_obj->prev_notify_memory_use) {
      /* make sure the parent notices that we're using more memory: */
      if (place_obj->parent_signal_handle)
        scheme_signal_received_at(place_obj->parent_signal_handle);
      place_obj->prev_notify_memory_use = mem_use;
    } else if (mem_use < place_obj->prev_notify_memory_use) {
      place_obj->prev_notify_memory_use = mem_use;
    }
  }
}

void scheme_place_check_memory_use()
{
  intptr_t m;

  m = GC_propagate_hierarchy_memory_use();
  scheme_place_set_memory_use(m);

  if (force_gc_for_place_accounting) {
    force_gc_for_place_accounting = 0;
    scheme_collect_garbage();
    resume_all_child_places();
  }
}

static void place_set_result(struct Scheme_Place_Object *place_obj, Scheme_Object *result)
/* always called as a place terminates */
{
  intptr_t status;

  if (SCHEME_INTP(result)) {
    status = SCHEME_INT_VAL(result);
    if (status < 1 || status > 255)
      status = 0;
  } else
    status = 0;

  mzrt_mutex_lock(place_obj->lock);
  place_obj->result = status;
  if (place_obj->parent_signal_handle) {
    scheme_signal_received_at(place_obj->parent_signal_handle);
    place_obj->parent_signal_handle = NULL;
  }
  place_obj->signal_handle = NULL;
  place_obj->dead = 1;
  mzrt_mutex_unlock(place_obj->lock);
}

static void terminate_current_place(Scheme_Object *result)
{
  intptr_t place_obj_die;
  intptr_t refcount;
  Scheme_Place_Object *place_obj;

  place_obj = place_object;

  mzrt_mutex_lock(place_obj->lock);
  place_obj_die = place_obj->die;
  mzrt_mutex_unlock(place_obj->lock);
  
  if (!place_obj_die) {
    if (scheme_flush_managed(NULL, 1))
      result = scheme_make_integer(1);
  }

  place_object = NULL;

  /*printf("Leavin place: proc thread id%u\n", ptid);*/

  /* Beware that the destroy operation might trigger a GC to cooperate
     with the master GC: */
  scheme_place_instance_destroy(place_obj_die);

  place_set_result(place_obj, result);
  
  mzrt_mutex_lock(place_obj->lock);
  
  place_obj->refcount--;
  refcount = place_obj->refcount;
  
  mzrt_mutex_unlock(place_obj->lock);
  
  if (!refcount)
    destroy_place_object_locks(place_obj);
}

static Scheme_Object *def_place_exit_handler_proc(int argc, Scheme_Object *argv[])
{
  log_place_event("id %d: exit (via `exit')", "exit", 0, 0);

  terminate_current_place(argv[0]);

  mz_proc_thread_exit(NULL);

  return scheme_void; /* Never get here */
}

static int do_embedded_load()
{
  if (embedded_load) {
    Scheme_Thread * volatile p;
    mz_jmp_buf * volatile save, newbuf;
    volatile int rc;

    p = scheme_get_current_thread();
    save = p->error_buf;
    p->error_buf = &newbuf;
    
    if (!scheme_setjmp(newbuf)) {
      scheme_embedded_load(embedded_load_len, embedded_load, 1);
      rc = 1;
    } else {
      rc = 0;
    }

    p->error_buf = save;

    return rc;
  }

  return 1;
}

static void *place_start_proc_after_stack(void *data_arg, void *stack_base) {
  Place_Start_Data *place_data;
  Scheme_Place_Object *place_obj;
  Scheme_Object *place_main;
  Scheme_Object *a[2], *channel, *result;
  intptr_t mem_limit;
  
  place_data = (Place_Start_Data *) data_arg;
  data_arg = NULL;
 
  /* printf("Startin place: proc thread id%u\n", ptid); */

  /* create pristine THREAD_LOCAL variables*/
  null_out_runtime_globals();

  mzrt_mutex_lock(id_counter_mutex);
  scheme_current_place_id = ++id_counter;
  mzrt_mutex_unlock(id_counter_mutex);

  mem_limit = SCHEME_INT_VAL(place_data->cust_limit);

  /* scheme_make_thread behaves differently if the above global vars are not null */
  scheme_place_instance_init(stack_base, place_data->parent_gc, mem_limit);

  a[0] = places_deep_direct_uncopy(place_data->current_library_collection_paths);
  scheme_current_library_collection_paths(1, a);
  a[0] = places_deep_direct_uncopy(place_data->current_library_collection_links);
  scheme_current_library_collection_links(1, a);
  a[0] = places_deep_direct_uncopy(place_data->compiled_roots);
  scheme_compiled_file_roots(1, a);
  scheme_seal_parameters();

  a[0] = places_deep_direct_uncopy(place_data->module);
  a[1] = places_deep_direct_uncopy(place_data->function);
  a[1] = scheme_intern_exact_symbol(SCHEME_SYM_VAL(a[1]), SCHEME_SYM_LEN(a[1]));
  channel = places_deep_direct_uncopy(place_data->channel);
  place_obj = place_data->place_obj;
  REGISTER_SO(place_object);
  place_object = place_obj;
  place_obj->refcount++;

  place_obj->id = scheme_current_place_id;
  
  {
    void *signal_handle;
    signal_handle = scheme_get_signal_handle();
    place_obj->signal_handle = signal_handle;
  }

  {
    Scheme_Object *tmp;
    if (place_data->in >= 0) {
      tmp = scheme_make_fd_input_port (place_data->in,  scheme_intern_symbol("place-in"),  0, 0);
      if (scheme_orig_stdin_port) {
        scheme_close_input_port(scheme_orig_stdin_port);
      }
      scheme_orig_stdin_port = tmp;
    }
    if (place_data->out >= 0) {
      tmp = scheme_make_fd_output_port(place_data->out, scheme_intern_symbol("place-out"), 0, 0, 0);
      if (scheme_orig_stdout_port) {
        scheme_close_output_port(scheme_orig_stdout_port);
      }
      scheme_orig_stdout_port = tmp;
    }
    if (place_data->err >= 0) {
      tmp = scheme_make_fd_output_port(place_data->err, scheme_intern_symbol("place-err"), 0, 0, 0);
      if (scheme_orig_stderr_port) {
        scheme_close_output_port(scheme_orig_stderr_port);
      }
      scheme_orig_stderr_port = tmp;
    }
    scheme_init_port_config();
  }

  mzrt_sema_post(place_data->ready);
  place_data = NULL;
# ifdef MZ_PRECISE_GC
  /* this prevents a master collection attempt from deadlocking with the 
     place_data->ready semaphore above */
  GC_allow_master_gc_check();
# endif

  /* at point, don't refer to place_data or its content
     anymore, because it's allocated in the other place */

  scheme_set_root_param(MZCONFIG_EXIT_HANDLER, scheme_def_place_exit_proc);

  log_place_event("id %d: enter", "enter", 0, 0);

  if (do_embedded_load()) {
    Scheme_Thread * volatile p;
    mz_jmp_buf * volatile saved_error_buf;
    mz_jmp_buf new_error_buf;
    Scheme_Object * volatile rc = scheme_false;
    
    p = scheme_get_current_thread();
    saved_error_buf = p->error_buf;
    p->error_buf = &new_error_buf;
    if (!scheme_setjmp(new_error_buf)) {
      Scheme_Object *dynamic_require;

      scheme_check_place_port_ok();

      dynamic_require = scheme_builtin_value("dynamic-require");
      place_main = scheme_apply(dynamic_require, 2, a);
      a[0] = channel;
      (void)scheme_apply(place_main, 1, a);
      rc = scheme_make_integer(0);
    } else {
      rc = scheme_make_integer(1);
    }
    p->error_buf = saved_error_buf;
    
    result = rc;
  } else {
    result = scheme_make_integer(1);
  }

  log_place_event("id %d: exit", "exit", 0, 0);

  terminate_current_place(result);

  return NULL;
}

static Scheme_Object *places_serialize(Scheme_Object *so, void **msg_memory, Scheme_Object **master_chain,
                                       Scheme_Object **invalid_object) {
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  Scheme_Object *new_so;
  Scheme_Object *tmp;

  new_so = trivial_copy(so, NULL);
  if (new_so) return new_so;

  GC_create_message_allocator();
  new_so = do_places_deep_copy(so, mzPDC_COPY, 0, master_chain, invalid_object);
  tmp = GC_finish_message_allocator();
  (*msg_memory) = tmp;
  return new_so;
#else
  return so;
#endif
}

static Scheme_Object *places_deserialize(Scheme_Object *so, void *msg_memory, Scheme_Thread *from_p)
/* The caller must immediately drop any reference to `so' and
   `msg_memory' after this function returns; otherwise, since the
   `msg_memory' page may be deallocated, a GC could crash.
   Also, we have to clear out the in-flight references in `from_p`
   before the pages are discarded or adopted (where the latter
   can trigger a GC, which creates the main problem) */
{
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  Scheme_Object *new_so = so;

  new_so = trivial_copy(so, NULL);
  if (new_so) return new_so;

  /* small messages are deemed to be < 1k, this could be tuned in either direction */
  if (GC_message_small_objects_size(msg_memory, 1024)) {
    new_so = do_places_deep_copy(so, mzPDC_UNCOPY, 1, NULL, NULL);
    from_p->place_channel_msg_in_flight = NULL;
    from_p->place_channel_msg_chain_in_flight = NULL;
    GC_dispose_short_message_allocator(msg_memory);
    /* from this point, we must return immediately, so that any
       reference to `so' can be dropped before GC. */
    msg_memory = NULL;
  }
  else {
    from_p->place_channel_msg_in_flight = NULL;
    from_p->place_channel_msg_chain_in_flight = NULL;
    GC_adopt_message_allocator(msg_memory);
    msg_memory = NULL;
#if !defined(SHARED_TABLES)
    new_so = do_places_deep_copy(so, mzPDC_DESER, 1, NULL, NULL);
#endif
  }
  return new_so;
#else
  return so;
#endif
}

Scheme_Object *place_send(int argc, Scheme_Object *args[]) 
{
  Scheme_Place_Bi_Channel *ch;
  if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type)) {
    ch = (Scheme_Place_Bi_Channel *) ((Scheme_Place *) args[0])->channel;
  }
  else if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_bi_channel_type)) {
    ch = (Scheme_Place_Bi_Channel *) args[0];
  }
  else {
    ch = NULL;
    scheme_wrong_contract("place-channel-put", "place-channel?", 0, argc, args);
  }
  place_async_send((Scheme_Place_Async_Channel *) ch->link->sendch, args[1]);
  return scheme_void;
}

Scheme_Object *place_receive(int argc, Scheme_Object *args[]) {
  Scheme_Place_Bi_Channel *ch;
  if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type)) {
    ch = (Scheme_Place_Bi_Channel *) ((Scheme_Place *) args[0])->channel;
  }
  else if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_bi_channel_type)) {
    ch = (Scheme_Place_Bi_Channel *) args[0];
  }
  else {
    ch = NULL;
    scheme_wrong_contract("place-channel-get", "place-channel?", 0, argc, args);
  }
  return place_async_receive((Scheme_Place_Async_Channel *)ch->link->recvch);
}

static Scheme_Object* place_allowed_p(int argc, Scheme_Object *args[])
{
  Scheme_Hash_Table *ht = NULL;
  
  if (places_deep_copy_worker(args[0], &ht, mzPDC_CHECK, 1, 0, NULL, NULL))
    return scheme_true;
  else
    return scheme_false;
}

# ifdef MZ_PRECISE_GC
void scheme_spawn_master_place() {
  mzrt_proc_first_thread_init();
  

  /* scheme_master_proc_thread = mz_proc_thread_create(master_scheme_place, NULL); */
  scheme_master_proc_thread = (void*) ~0;

}
# endif

/*========================================================================*/
/*                       places async channels                            */
/*========================================================================*/

static void* GC_master_malloc(size_t size) {
  void *ptr;
#ifdef MZ_PRECISE_GC
  void *original_gc;
  original_gc = GC_switch_to_master_gc();
#endif
  ptr = GC_malloc(size);
#ifdef MZ_PRECISE_GC
  GC_switch_back_from_master(original_gc);
#endif
  return ptr;
}

static void* GC_master_malloc_tagged(size_t size) {
  void *ptr;
#ifdef MZ_PRECISE_GC
  void *original_gc;
  original_gc = GC_switch_to_master_gc();
#endif
  ptr = scheme_malloc_small_tagged(size);
#ifdef MZ_PRECISE_GC
  GC_switch_back_from_master(original_gc);
#endif
  return ptr;
}

static void maybe_report_message_size(Scheme_Place_Async_Channel *ch)
{
#ifdef MZ_PRECISE_GC
  if ((ch->reported_size > (2 * ch->mem_size))
      || (((ch->reported_size * 2) < ch->mem_size)
          && ((ch->mem_size - ch->reported_size) > (1 << (LOG_APAGE_SIZE + 1))))) {
    intptr_t delta = ch->mem_size - ch->reported_size;
    ch->reported_size = ch->mem_size;
    GC_report_unsent_message_delta(delta);
  }
#endif
}

static void async_channel_finalize(void *p, void* data) {
  Scheme_Place_Async_Channel *ch;
  int i;
  Scheme_Hash_Table *ht = NULL;
  ch = (Scheme_Place_Async_Channel*)p;

  ch->mem_size = 0;
  maybe_report_message_size(ch);

  mzrt_mutex_destroy(ch->lock);
  ch->lock = NULL;
  for (i = 0; i < ch->size ; i++) {
    ht = NULL;
    if (ch->msgs[i]) {
      (void)places_deep_copy_worker(ch->msgs[i], &ht, mzPDC_CLEAN, 0, 0, NULL, NULL);
      ch->msgs[i] = NULL;
    }
#ifdef MZ_PRECISE_GC
    if (ch->msg_memory[i]) {
      GC_destroy_orphan_msg_memory(ch->msg_memory[i]);
    }
#endif
    ch->msg_memory[i] = NULL;
    ch->msg_chains[i] = NULL;
  }
  ch->in = 0;
  ch->out = 0;
  ch->count = 0;

  if (ch->wakeup_signal) {
    /*release single receiver */  
    if (SCHEME_PLACE_OBJECTP(ch->wakeup_signal)) {
      int refcount = 0;
      Scheme_Place_Object *place_obj;
      place_obj = ((Scheme_Place_Object *) ch->wakeup_signal);

      mzrt_mutex_lock(place_obj->lock);
      place_obj->refcount--;
      refcount = place_obj->refcount;
      mzrt_mutex_unlock(place_obj->lock);
      if (!refcount) {
        destroy_place_object_locks(place_obj);
      }
    }
    /*release multiple receiver */  
    else if (SCHEME_VECTORP(ch->wakeup_signal)) {
      Scheme_Object *v = ch->wakeup_signal;
      int i;
      int size = SCHEME_VEC_SIZE(v);
      for (i = 0; i < size; i++) {
        Scheme_Place_Object *o3;
        o3 = (Scheme_Place_Object *)SCHEME_VEC_ELS(v)[i];
        if (o3) {
          int refcount = 0;
          mzrt_mutex_lock(o3->lock);
            SCHEME_VEC_ELS(v)[i] = NULL;
            o3->refcount--;
            refcount = o3->refcount;
          mzrt_mutex_unlock(o3->lock);

          if (!refcount) {
            destroy_place_object_locks(o3);
          }
        }
      }
    }
  }
}

Scheme_Place_Async_Channel *place_async_channel_create() {
  Scheme_Object **msgs, **msg_chains;
  Scheme_Place_Async_Channel *ch;
  void **msg_memory;
#ifdef MZ_PRECISE_GC
  void *original_gc;
#endif

  ch = GC_master_malloc_tagged(sizeof(Scheme_Place_Async_Channel));
  ch->so.type = scheme_place_async_channel_type;

  msgs = GC_master_malloc(sizeof(Scheme_Object*) * 8);
  msg_memory = GC_master_malloc(sizeof(void*) * 8);
  msg_chains = GC_master_malloc(sizeof(Scheme_Object*) * 8);

  ch->in = 0;
  ch->out = 0;
  ch->count = 0;
  ch->size = 8;
  mzrt_mutex_create(&ch->lock);
  ch->msgs = msgs;
  ch->msg_memory = msg_memory;
  ch->msg_chains = msg_chains;
  ch->wakeup_signal = NULL;

#ifdef MZ_PRECISE_GC
  original_gc = GC_switch_to_master_gc();
  GC_set_finalizer(ch, 1, 1, async_channel_finalize, NULL, NULL, NULL);
  GC_switch_back_from_master(original_gc);
#endif
  /* FIXME? Need finalizer for non-precise GC if places become supported 
     in that mode. */

  return ch;
}

static void async_channel_refcount(Scheme_Place_Async_Channel *ch, int for_send, int delta)
{
  if (!ch->lock) {
    /* can happen via finalization, where the channel is already finalized
       m(due to the lack of ordering on finalization) */
    return;
  }
  mzrt_mutex_lock(ch->lock);
  if (for_send)
    ch->wr_ref += delta;
  else
    ch->rd_ref += delta;
  if ((ch->wr_ref < 0) || (ch->rd_ref < 0)) {
    scheme_log_abort("internal error: bad reference count on async channel");
    abort();
  }
  mzrt_mutex_unlock(ch->lock);
}

Scheme_Object *scheme_place_make_async_channel()
{
  Scheme_Place_Async_Channel *ch;
  ch = place_async_channel_create();

  /* we don't allocate a bi channel, so claim an implicit sender and receiver: */
  async_channel_refcount(ch, 0, 1);
  async_channel_refcount(ch, 1, 1);

  return (Scheme_Object *)ch;
}

static void bi_channel_refcount(Scheme_Place_Bi_Channel *ch, int delta)
{
  async_channel_refcount(ch->link->sendch, 1, delta);
  async_channel_refcount(ch->link->recvch, 0, delta);
}

static void bi_channel_refcount_down(void *_ch, void *data)
{
  Scheme_Place_Bi_Channel *ch = (Scheme_Place_Bi_Channel *)_ch;

  if (!ch->link->sendch) {
    /* released by scheme_free_place_bi_channels() already */
    return;
  }

  if (ch->link->prev)
    ch->link->prev->next = ch->link->next;
  else
    place_channel_links = ch->link->next;
  if (ch->link->next)
    ch->link->next->prev = ch->link->prev;

  bi_channel_refcount(ch, -1);
}

void scheme_free_place_bi_channels()
{
  Scheme_Place_Bi_Channel_Link *link;

  for (link = place_channel_links; link; link = link->next) {
    async_channel_refcount(link->sendch, 1, -1);
    async_channel_refcount(link->recvch, 0, -1);
    /* It's possible that a GC will run after this: */
    link->sendch = NULL;
    link->recvch = NULL;
  }
  place_channel_links = NULL;
}

static void bi_channel_set_finalizer(Scheme_Place_Bi_Channel *ch)
{
  ch->link->next = place_channel_links;
  if (place_channel_links)
    place_channel_links->prev = ch->link;
  place_channel_links = ch->link;
  
  scheme_add_finalizer(ch, bi_channel_refcount_down, NULL);
}


Scheme_Place_Bi_Channel *place_bi_channel_malloc() {
  Scheme_Place_Bi_Channel *ch;
  Scheme_Place_Bi_Channel_Link *link;

  ch = MALLOC_ONE_TAGGED(Scheme_Place_Bi_Channel);
  ch->so.type = scheme_place_bi_channel_type;
  
  link = (Scheme_Place_Bi_Channel_Link*)scheme_malloc(sizeof(Scheme_Place_Bi_Channel_Link));
  ch->link = link;

  return ch;
}

Scheme_Place_Bi_Channel *place_bi_channel_create() {
  Scheme_Place_Async_Channel *tmp;
  Scheme_Place_Bi_Channel *ch;

  ch = place_bi_channel_malloc();

  tmp = place_async_channel_create();
  ch->link->sendch = tmp;
  tmp = place_async_channel_create();
  ch->link->recvch = tmp;

  bi_channel_refcount(ch, 1);
  bi_channel_set_finalizer(ch);

  return ch;
}

Scheme_Place_Bi_Channel *place_bi_peer_channel_create(Scheme_Place_Bi_Channel *orig) {
  Scheme_Place_Bi_Channel *ch;

  ch = place_bi_channel_malloc();

  ch->link->sendch = orig->link->recvch;
  ch->link->recvch = orig->link->sendch;

  bi_channel_refcount(ch, 1);
  bi_channel_set_finalizer(ch);

  return ch;
}

static Scheme_Object *place_channel(int argc, Scheme_Object *args[]) {
  Scheme_Place_Bi_Channel *ch;
  Scheme_Object *a[2];
  ch = place_bi_channel_create();
  a[0] = (Scheme_Object *) ch;
  a[1] = (Scheme_Object *) place_bi_peer_channel_create(ch);
  return scheme_values(2, a);
}

static Scheme_Object *place_channel_p(int argc, Scheme_Object *args[])
{
  return (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_bi_channel_type) ||
          SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type)) ? scheme_true : scheme_false;
}

static Scheme_Object *GC_master_make_vector(int size) {
  Scheme_Object *v;
#ifdef MZ_PRECISE_GC
  void *original_gc;
  original_gc = GC_switch_to_master_gc();
#endif
  v = scheme_make_vector(size, NULL);
#ifdef MZ_PRECISE_GC
  GC_switch_back_from_master(original_gc);
#endif
  return v;
}

static void place_async_send(Scheme_Place_Async_Channel *ch, Scheme_Object *uo) {
  void *msg_memory = NULL;
  Scheme_Object *o, *master_chain = NULL, *invalid_object = NULL;
  intptr_t sz, cnt;

  o = places_serialize(uo, &msg_memory, &master_chain, &invalid_object);
  /* uo needs to stay live until `master_chain` is registered in `ch` */

  if (!o) {
    if (invalid_object) {
      scheme_contract_error("place-channel-put",
                            "value not allowed in a message", 
                            "value", 1, invalid_object,
                            "message", 1, uo,
                            NULL);
    }
    else bad_place_message(uo);
  }

  {
    intptr_t msg_size;
    msg_size = GC_message_allocator_size(msg_memory);
    log_place_event("id %d: put message of %" PRIdPTR " bytes", "put", 1, msg_size);
  }

  mzrt_mutex_lock(ch->lock);
  {
    cnt = ch->count;
    if (ch->count == ch->size) { /* GROW QUEUE */
      Scheme_Object **new_msgs = NULL, **new_chains = NULL;
      void **new_msg_memory = NULL;
      intptr_t sz = 0;

      /* Can't allocate while holding the lock, so release lock and loop: */
      while (ch->count == ch->size) {
        if ((sz == ch->size) && new_msgs) {
          if (ch->out < ch->in) {
            memcpy(new_msgs, ch->msgs + ch->out, sizeof(Scheme_Object *) * (ch->in - ch->out));
            memcpy(new_msg_memory, ch->msg_memory + ch->out, sizeof(void*) * (ch->in - ch->out));
            memcpy(new_chains, ch->msg_chains + ch->out, sizeof(void*) * (ch->in - ch->out));
          }
          else {
            int s1 = (ch->size - ch->out);
            memcpy(new_msgs, ch->msgs + ch->out, sizeof(Scheme_Object *) * s1);
            memcpy(new_msgs + s1, ch->msgs, sizeof(Scheme_Object *) * ch->in);

            memcpy(new_msg_memory, ch->msg_memory + ch->out, sizeof(void*) * s1);
            memcpy(new_msg_memory + s1, ch->msg_memory, sizeof(void*) * ch->in);

            memcpy(new_chains, ch->msg_chains + ch->out, sizeof(Scheme_Object *) * s1);
            memcpy(new_chains + s1, ch->msg_chains, sizeof(Scheme_Object *) * ch->in);
          }

          ch->msgs = new_msgs;
          ch->msg_memory = new_msg_memory;
          ch->msg_chains = new_chains;
          ch->in = ch->size;
          ch->out = 0;
          ch->size *= 2;

          break;
        } else {
          sz = ch->size;
          mzrt_mutex_unlock(ch->lock);

          new_msgs = GC_master_malloc(sizeof(Scheme_Object*) * sz * 2);
          new_msg_memory = GC_master_malloc(sizeof(void*) * sz * 2);
          new_chains = GC_master_malloc(sizeof(Scheme_Object*) * sz * 2);

          mzrt_mutex_lock(ch->lock);
        }
      }
    }

    ch->msgs[ch->in] = o;
    ch->msg_memory[ch->in] = msg_memory;
    ch->msg_chains[ch->in] = master_chain;
    ++ch->count;
    ch->in = ((ch->in + 1) % ch->size);

    sz = GC_message_allocator_size(msg_memory);
    ch->mem_size += sz;

    maybe_report_message_size(ch);
  }

  /* make sure `uo` is treated as live until here: */
  if (!uo) scheme_signal_error("?");

  if (!cnt && ch->wakeup_signal) {
    /*wake up possibly sleeping single receiver */  
    if (SCHEME_PLACE_OBJECTP(ch->wakeup_signal)) {
      Scheme_Place_Object *place_obj;
      place_obj = ((Scheme_Place_Object *) ch->wakeup_signal);

      mzrt_mutex_lock(place_obj->lock);
      if (place_obj->signal_handle) {
        scheme_signal_received_at(place_obj->signal_handle);
      }
      mzrt_mutex_unlock(place_obj->lock);
    }
    /*wake up possibly sleeping multiple receiver */  
    else if (SCHEME_VECTORP(ch->wakeup_signal)) {
      Scheme_Object *v = ch->wakeup_signal;
      int i, j, delta;
      int size = SCHEME_VEC_SIZE(v);
      int alive = 0;
      /* Try to be fair by cycling through the available places
         starting at `delta'. */
      delta = ch->delta++;
      if (delta < 0) delta = -delta;
      for (j = 0; j < size; j++) {
        Scheme_Place_Object *o3;
        i = (j + delta) % size;
        o3 = (Scheme_Place_Object *)SCHEME_VEC_ELS(v)[i];
        if (o3) {
          int refcount = 0;
          mzrt_mutex_lock(o3->lock);
          if (o3->signal_handle != NULL) {
            scheme_signal_received_at(o3->signal_handle);
            alive++;
          }
          else {
            SCHEME_VEC_ELS(v)[i] = NULL;
            o3->refcount--;
          }
          refcount = o3->refcount;
          mzrt_mutex_unlock(o3->lock);

          if (!refcount) {
            destroy_place_object_locks(o3);
          }
        }
      }
      /* shrink if more than half are unused */
      if (alive < (size / 2)) {
        if (alive == 1) {
          ch->wakeup_signal = NULL;
          for (i = 0; i < size; i++) {
            Scheme_Place_Object *o2 = (Scheme_Place_Object *)SCHEME_VEC_ELS(v)[i];
            if (o2) {
              ch->wakeup_signal = (Scheme_Object *)o2;
              break;
            }
          }
        }
        else {
          Scheme_Object *nv;
          int ncnt = 0;
          nv = GC_master_make_vector(size/2);
          for (i = 0; i < size; i++) {
            Scheme_Place_Object *o2 = (Scheme_Place_Object *)SCHEME_VEC_ELS(v)[i];
            if (o2) {
              SCHEME_VEC_ELS(nv)[ncnt] = (Scheme_Object *)o2;
              ncnt++;
            }
          }
          ch->wakeup_signal = nv;
        }
      }
    }
    else {
      printf("Oops not a valid ch->wakeup_signal\n");
      exit(1);
    }
  }
  mzrt_mutex_unlock(ch->lock);
}

void scheme_place_async_channel_send(Scheme_Object *ch, Scheme_Object *uo) {
  place_async_send((Scheme_Place_Async_Channel *)ch, uo);
}

static void place_object_inc_refcount(Scheme_Object *o) {
  Scheme_Place_Object *place_obj;
  place_obj = (Scheme_Place_Object *) o;

  mzrt_mutex_lock(place_obj->lock);
  place_obj->refcount++;
  mzrt_mutex_unlock(place_obj->lock);
}

static void place_object_dec_refcount(Scheme_Object *o) {
  int refcount;
  Scheme_Place_Object *place_obj;
  place_obj = (Scheme_Place_Object *) o;

  mzrt_mutex_lock(place_obj->lock);
  place_obj->refcount--;
  refcount = place_obj->refcount;
  mzrt_mutex_unlock(place_obj->lock);

  if (!refcount) {
    destroy_place_object_locks(place_obj);
  }
}

static void lock_and_register_place_object_with_channel(Scheme_Place_Async_Channel *ch, Scheme_Object *o)
{
  Scheme_Object *avail_vector;

  mzrt_mutex_lock(ch->lock);

  if (ch->count)
    return; /* no need for a wakeup signal, since data is available */

  /* loop in case we need to release the lock temporarily to allocate: */
  while (1) {
    if (ch->wakeup_signal == o) {
      return;
    }
    else if (!ch->wakeup_signal) {
      place_object_inc_refcount(o);
      ch->wakeup_signal = o;
      return;
    }
    else if (SCHEME_PLACE_OBJECTP(ch->wakeup_signal)
             && ( (Scheme_Place_Object *) ch->wakeup_signal)->signal_handle == NULL) {
      place_object_dec_refcount(ch->wakeup_signal);
      place_object_inc_refcount(o);
      ch->wakeup_signal = o;
      return;
    }
    else if (SCHEME_VECTORP(ch->wakeup_signal)) {
      int i = 0;
      Scheme_Object *v = ch->wakeup_signal;
      int size = SCHEME_VEC_SIZE(v);
      /* already registered? */
      for (i = 0; i < size; i++) {
        Scheme_Object *vo = SCHEME_VEC_ELS(v)[i];
        if (vo == o)
          return;
      }
      /* look for unused slot in wakeup vector */
      for (i = 0; i < size; i++) {
        Scheme_Object *vo = SCHEME_VEC_ELS(v)[i];
        if (!vo) {
          place_object_inc_refcount(o);
          SCHEME_VEC_ELS(v)[i] = o;
          return;
        }
        else if (SCHEME_PLACE_OBJECTP(vo) &&
                 ((Scheme_Place_Object *)vo)->signal_handle == NULL) {
          place_object_dec_refcount(vo);
          place_object_inc_refcount(o);
          SCHEME_VEC_ELS(v)[i] = o;
          return;
        }
      }
      /* fall through to here, need to grow wakeup vector;
         must do so without the lock */
      {
        if (avail_vector && (SCHEME_VEC_SIZE(avail_vector) == size*2)) {
          Scheme_Object *nv;
          nv = avail_vector;
          for (i = 0; i < size; i++) {
            SCHEME_VEC_ELS(nv)[i] = SCHEME_VEC_ELS(v)[i];
          }
          place_object_inc_refcount(o);
          SCHEME_VEC_ELS(nv)[size+1] = o;
          ch->wakeup_signal = nv;
        } else {
          mzrt_mutex_unlock(ch->lock);
          avail_vector = GC_master_make_vector(size*2);
          mzrt_mutex_lock(ch->lock);
        }
      }
    }
    /* grow from single wakeup to multiple wakeups */
    else if (SCHEME_PLACE_OBJECTP(ch->wakeup_signal)) {
      if (avail_vector && (SCHEME_VEC_SIZE(avail_vector) == 2)) {
        Scheme_Object *v;
        v = avail_vector;
        SCHEME_VEC_ELS(v)[0] = ch->wakeup_signal;
        place_object_inc_refcount(o);
        SCHEME_VEC_ELS(v)[1] = o;
        ch->wakeup_signal = v;
      } else {
        mzrt_mutex_unlock(ch->lock);
        avail_vector = GC_master_make_vector(2);
        mzrt_mutex_lock(ch->lock);
      }
    }
    else {
      printf("Oops not a valid ch->wakeup_signal\n");
      exit(1);
    }
  }
}

static Scheme_Object *place_async_try_receive_raw(Scheme_Place_Async_Channel *ch,
                                                  void **msg_memory_ptr,
                                                  void **msg_chain_ptr,
                                                  int *_no_writers) 
/* The result must not be retained past extraction from `*msg_memory_ptr'! */
{
  Scheme_Object *msg = NULL;
  void *msg_memory = NULL, *msg_chain = NULL;
  intptr_t sz;

  lock_and_register_place_object_with_channel(ch, (Scheme_Object *) place_object);
  {
    if (ch->count > 0) { /* GET MSG */
      msg = ch->msgs[ch->out];
      msg_memory = ch->msg_memory[ch->out];
      msg_chain = ch->msg_chains[ch->out];

      ch->msgs[ch->out] = NULL;
      ch->msg_memory[ch->out] = NULL;
      ch->msg_chains[ch->out] = NULL;

      /* No GCs from here until msg_chain is registered */

      --ch->count;
      ch->out = ((ch->out + 1) % ch->size);

      sz = GC_message_allocator_size(msg_memory);
      ch->mem_size -= sz;

      maybe_report_message_size(ch);
    }
  }
  if (!msg && !ch->wr_ref && _no_writers)
    *_no_writers = 1;
  mzrt_mutex_unlock(ch->lock);

  *msg_memory_ptr = msg_memory;
  *msg_chain_ptr = msg_chain;

  return msg;
}

static void cleanup_msg_memmory(void *thread) {
  Scheme_Thread *p = thread;
  if (p->place_channel_msg_in_flight) {
    p->place_channel_msg_chain_in_flight = NULL;
    GC_destroy_orphan_msg_memory(p->place_channel_msg_in_flight);
    p->place_channel_msg_in_flight = NULL;
  }
}

static void log_received_msg(Scheme_Object *msg, void *msg_memory)
{
  if (msg) {
    intptr_t msg_size;
    msg_size = GC_message_allocator_size(msg_memory);
    log_place_event("id %d: get message of %" PRIdPTR " bytes", "get", 1, msg_size);
  }
}

static Scheme_Object *place_async_try_receive(Scheme_Place_Async_Channel *ch, int *_no_writers) {
  Scheme_Object *msg = NULL;
  Scheme_Thread *p = scheme_current_thread;
  GC_CAN_IGNORE void *msg_memory, *msg_chain;
  BEGIN_ESCAPEABLE(cleanup_msg_memmory, p);
  msg = place_async_try_receive_raw(ch, &msg_memory, &msg_chain, _no_writers);
  /* no GCs until msg_chain is registered */
  if (msg) {
    p->place_channel_msg_in_flight = msg_memory;
    p->place_channel_msg_chain_in_flight = msg_chain;
    log_received_msg(msg, msg_memory);
    msg = places_deserialize(msg, msg_memory, p);
  }
  END_ESCAPEABLE();
  return msg;
}

static int scheme_place_async_ch_ready(Scheme_Place_Async_Channel *ch) {
  int ready = 0;
  lock_and_register_place_object_with_channel(ch, (Scheme_Object *) place_object);
  {
    if (ch->count > 0) ready = 1;
    if (!ch->wr_ref) ready = 1;
  }
  mzrt_mutex_unlock(ch->lock);
  return ready;
}

static Scheme_Object *place_channel_finish_ready(void *d, int argc, struct Scheme_Object *argv[]) 
{
  Scheme_Object *msg;
  Scheme_Thread *p = scheme_current_thread;                                                                 

  msg = *(Scheme_Object **)d;

  BEGIN_ESCAPEABLE(cleanup_msg_memmory, p);
  msg = places_deserialize(msg, p->place_channel_msg_in_flight, p);
  END_ESCAPEABLE();

  return msg;
}

static int place_channel_ready(Scheme_Object *so, Scheme_Schedule_Info *sinfo) {
  Scheme_Place_Bi_Channel *ch;
  Scheme_Object *msg = NULL;
  Scheme_Object *wrapper;
  GC_CAN_IGNORE void *msg_memory = NULL, *msg_chain = NULL;
  int no_writers = 0;

  if (SAME_TYPE(SCHEME_TYPE(so), scheme_place_type)) {
    ch = (Scheme_Place_Bi_Channel *) ((Scheme_Place *) so)->channel;
  }
  else {
    ch = (Scheme_Place_Bi_Channel *)so;
  }

  msg = place_async_try_receive_raw((Scheme_Place_Async_Channel *) ch->link->recvch, 
                                    &msg_memory, &msg_chain, &no_writers);
  /* no GCs until msg_chain is registered */
  if (msg != NULL) {
    Scheme_Object **msg_holder;
    Scheme_Thread *p = ((Syncing *)(sinfo->current_syncing))->thread;

    p->place_channel_msg_in_flight = msg_memory;
    p->place_channel_msg_chain_in_flight = msg_chain;

    log_received_msg(msg, msg_memory);

    /* Hold `msg' in atomic memory, because we're not allowed to hold onto
       it beyond release of msg_memory, and `wrapper' and the result
       flow into the evt system in general. */
    msg_holder = (Scheme_Object **)scheme_malloc_atomic(sizeof(Scheme_Object*));
    *msg_holder = msg;

    wrapper = scheme_make_closed_prim(place_channel_finish_ready, msg_holder);
    scheme_set_sync_target(sinfo, scheme_void, wrapper, NULL, 0, 0, NULL);

    return 1;
  }

  if (no_writers) {
    /* block on a semaphore that is not accessible, which may allow the thread
       to be GCed */
    scheme_set_sync_target(sinfo, scheme_make_sema(0), scheme_void, NULL, 0, 0, NULL);
    return 0;
  }

  return 0;
}

static Scheme_Object *place_async_receive(Scheme_Place_Async_Channel *ch) {
  Scheme_Object *msg = NULL;
  int no_writers = 0;

  while (1) {
    msg = place_async_try_receive(ch, &no_writers);
    if (msg) 
      break;
    else {
      if (no_writers) {
        /* No writers are left for this channel, so suspend the thread */
        scheme_wait_sema(scheme_make_sema(0), 0);
      }
      scheme_thread_block(0);
      scheme_block_until((Scheme_Ready_Fun) scheme_place_async_ch_ready, NULL, (Scheme_Object *) ch, 0);
    }
  }

  return msg;
}

Scheme_Object *scheme_place_async_channel_receive(Scheme_Object *ch) {
  return place_async_receive((Scheme_Place_Async_Channel *)ch);
}

/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_place.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_place_type, place_val);
  GC_REG_TRAV(scheme_place_object_type, place_object_val);
  GC_REG_TRAV(scheme_place_async_channel_type, place_async_channel_val);
  GC_REG_TRAV(scheme_place_bi_channel_type, place_bi_channel_val);
  GC_REG_TRAV(scheme_serialized_file_fd_type, serialized_file_fd_val);
  GC_REG_TRAV(scheme_serialized_tcp_fd_type, serialized_socket_fd_val);
}

END_XFORM_SKIP;

#endif

/************************************************************************/
/************************************************************************/
/************************************************************************/

#endif
