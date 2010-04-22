
#include "schpriv.h"

/* READ ONLY SHARABLE GLOBALS */

#ifdef MZ_USE_PLACES

#include "mzrt.h"

READ_ONLY static Scheme_Object *scheme_def_place_exit_proc;

SHARED_OK mz_proc_thread *scheme_master_proc_thread;
THREAD_LOCAL_DECL(mz_proc_thread *proc_thread_self);
static Scheme_Object *scheme_place(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_wait(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_sleep(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_p(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_places_deep_copy_in_master(Scheme_Object *so);
static Scheme_Object *scheme_place_send(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_recv(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_channel_p(int argc, Scheme_Object *args[]);
static Scheme_Object *def_place_exit_handler_proc(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_channel(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_channel_receiver_channel(int argc, Scheme_Object *args[]);

static Scheme_Object *scheme_place_async_channel_create();
static Scheme_Object *scheme_place_bi_channel_create();
static Scheme_Object *scheme_place_bi_peer_channel_create(Scheme_Object *orig);
static void scheme_place_bi_channel_set_signal(Scheme_Object *cho);
static int scheme_place_channel_ready(Scheme_Object *so);


static void scheme_place_async_send(Scheme_Place_Async_Channel *ch, Scheme_Object *o);
static Scheme_Object *scheme_place_async_recv(Scheme_Place_Async_Channel *ch);
static Scheme_Object *scheme_places_deep_copy_worker(Scheme_Object *so, Scheme_Hash_Table *ht);

# ifdef MZ_PRECISE_GC
static void register_traversers(void);
# endif

static void *place_start_proc(void *arg);
static void *place_start_proc_after_stack(void *data_arg, void *stack_base);

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

  PLACE_PRIM_W_ARITY("place",          scheme_place,       1, 3, plenv);
  PLACE_PRIM_W_ARITY("place-sleep",    scheme_place_sleep, 1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-wait",     scheme_place_wait,  1, 1, plenv);
  PLACE_PRIM_W_ARITY("place?",         scheme_place_p,     1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-channel",  scheme_place_channel,  0, 0, plenv);
  PLACE_PRIM_W_ARITY("place-channel->receiver-channel",  scheme_place_channel_receiver_channel, 1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-channel-send",  scheme_place_send,  1, 2, plenv);
  PLACE_PRIM_W_ARITY("place-channel-recv",  scheme_place_recv,  1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-channel?",      scheme_place_channel_p,  1, 1, plenv);

#ifdef MZ_USE_PLACES
  REGISTER_SO(scheme_def_place_exit_proc);
  scheme_def_place_exit_proc = scheme_make_prim_w_arity(def_place_exit_handler_proc, "default-place-exit-handler", 1, 1);
#endif
  scheme_finish_primitive_module(plenv);

}

void scheme_init_places_once() {
#ifdef MZ_USE_PLACES
  scheme_add_evt(scheme_place_type,            (Scheme_Ready_Fun)scheme_place_channel_ready, NULL, NULL, 1); 
  scheme_add_evt(scheme_place_bi_channel_type, (Scheme_Ready_Fun)scheme_place_channel_ready, NULL, NULL, 1);
#endif
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
  mzrt_sema *ready;
} Place_Start_Data;

static Scheme_Object *def_place_exit_handler_proc(int argc, Scheme_Object *argv[])
{
  long status;

  if (SCHEME_INTP(argv[0])) {
    status = SCHEME_INT_VAL(argv[0]);
    if (status < 1 || status > 255)
      status = 0;
  } else
    status = 0;

  mz_proc_thread_exit((void *) status);
  return scheme_void; /* Never get here */
}

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
  mzrt_sema             *ready;

  /* create place object */
  place = MALLOC_ONE_TAGGED(Scheme_Place);
  place->so.type = scheme_place_type;

  mzrt_sema_create(&ready, 0);

  /* pass critical info to new place */
  place_data = MALLOC_ONE(Place_Start_Data);
  place_data->ready    = ready;

  if (argc == 2 || argc == 3 ) {
    place_data->module   = args[0];
    place_data->function = args[1];
    place_data->ready    = ready;
    if (argc == 2) {
      Scheme_Object *channel;
      channel = scheme_place_bi_channel_create();
      place->channel = channel;
      scheme_place_bi_channel_set_signal(channel);
      channel = scheme_place_bi_peer_channel_create(channel);
      place_data->channel = channel;
    }
    else {
      Scheme_Object *channel;
      channel = args[2];
      place_data->channel = channel;
      place->channel = channel;
    }
  }
  else {
    scheme_wrong_count_m("place", 1, 2, argc, args, 0);
  }

  collection_paths = scheme_current_library_collection_paths(0, NULL);
  collection_paths = scheme_places_deep_copy_in_master(collection_paths);
  place_data->current_library_collection_paths = collection_paths;

  /* create new place */
  proc_thread = mz_proc_thread_create(place_start_proc, place_data);

  /* wait until the place has started and grabbed the value
     from `place_data'; it's important that a GC doesn't happen
     here until the other place is far enough. */
  mzrt_sema_wait(ready);
  mzrt_sema_destroy(ready);
  
  place->proc_thread = proc_thread;

  return (Scheme_Object*) place;
}

# ifdef MZ_PRECISE_GC
/*============= SIGNAL HANDLER =============*/
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>


static void error_info() {
  char *erstr;
  erstr = strerror(errno);
  printf("errno %i %s\n", errno, erstr);
}

typedef struct Child_Status {
  int pid;
  int status;
  void *signal_fd;
  struct Child_Status *next;
} Child_Status;

SHARED_OK static Child_Status *child_statuses = NULL;
SHARED_OK static mzrt_mutex* child_status_lock = NULL;

static void add_child_status(int pid, int status) {
  Child_Status *st;
  st = malloc(sizeof(Child_Status));
  st->pid = pid;
  st->signal_fd = NULL;
  st->status = status;

  mzrt_mutex_lock(child_status_lock);
  st->next = child_statuses;
  child_statuses = st;
  mzrt_mutex_unlock(child_status_lock);
}

static int raw_get_child_status(int pid, int *status) {
  Child_Status *st;
  Child_Status *prev;
  int found = 0;

  for (st = child_statuses, prev = NULL; st; prev = st, st = st->next) {
    if (st->pid == pid) {
      *status = st->status;
      found = 1;
      if (prev) {
        prev->next = st->next;
      }
      else {
        child_statuses = st->next;
      }
      free(st);
      break;
    }
  }
  return found;
}

int scheme_get_child_status(int pid, int *status) {
  int found = 0;
  mzrt_mutex_lock(child_status_lock);
  found = raw_get_child_status(pid, status);
  mzrt_mutex_unlock(child_status_lock);
  /* printf("scheme_get_child_status found %i pid %i status %i\n", found,  pid, *status); */
  return found;
}

int scheme_places_register_child(int pid, void *signal_fd, int *status) {
  int found = 0;

  mzrt_mutex_lock(child_status_lock);
  found = raw_get_child_status(pid, status);
  if (!found) {
    Child_Status *st;
    st = malloc(sizeof(Child_Status));
    st->pid = pid;
    st->signal_fd = signal_fd;
    st->status = 0;

    st->next = child_statuses;
    child_statuses = st;
  }
  mzrt_mutex_unlock(child_status_lock);
  return found;
}

static void *mz_proc_thread_signal_worker(void *data) {
  int status;
  int pid;
  sigset_t set;
  //GC_CAN_IGNORE siginfo_t info;
  {
    sigemptyset(&set);
    sigaddset(&set, SIGCHLD);
    pthread_sigmask(SIG_UNBLOCK, &set, NULL);
  }

  while(1) {
    int rc;
    int signalid;
    do {
      rc = sigwait(&set, &signalid);
      if (rc == -1) {
        if (errno != EINTR ) {
        error_info();
        }
      }
    } while (rc == -1 && errno == EINTR);

    pid = waitpid((pid_t)-1, &status, WNOHANG);
    if (pid == -1) {
      char *erstr;
      erstr = strerror(errno);
      /* printf("errno %i %s\n", errno, erstr); */
    }
    else {
      /* printf("SIGCHILD pid %i with status %i %i\n", pid, status, WEXITSTATUS(status)); */
      add_child_status(pid, status);
    }
  };
  return NULL;
}


void scheme_places_block_child_signal() {
  {
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGCHLD);
    pthread_sigmask(SIG_BLOCK, &set, NULL);
  }

  {
    mz_proc_thread *signal_thread;
    mzrt_mutex_create(&child_status_lock);
    signal_thread = mz_proc_thread_create(mz_proc_thread_signal_worker, NULL);
    mz_proc_thread_detach(signal_thread);
  }
}

/*============= THREAD JOIN HANDLER =============*/
typedef struct {
  mz_proc_thread *proc_thread;
  Scheme_Place   *waiting_place; 
  int            *wake_fd;
  int             ready;
  long            rc;
} proc_thread_wait_data;


static void *mz_proc_thread_wait_worker(void *data) {
  void           *rc;
  proc_thread_wait_data *wd = (proc_thread_wait_data*) data;

  rc = mz_proc_thread_wait(wd->proc_thread);
  wd->rc = (long) rc;
  wd->ready = 1;
  scheme_signal_received_at(wd->wake_fd);
  return NULL;
}

static int place_wait_ready(Scheme_Object *o) {
  proc_thread_wait_data *wd = (proc_thread_wait_data*) o;
  if (wd->ready) {
    return 1;
  }
  return 0;
}
# endif

static Scheme_Object *scheme_place_wait(int argc, Scheme_Object *args[]) {
  Scheme_Place          *place;
  place = (Scheme_Place *) args[0];
 
# ifdef MZ_PRECISE_GC
   {
    Scheme_Object *rc;
    mz_proc_thread *worker_thread;
    Scheme_Place *waiting_place;
    int *wake_fd;

    proc_thread_wait_data *wd;
    wd = (proc_thread_wait_data*) malloc(sizeof(proc_thread_wait_data));
    wd->proc_thread = (mz_proc_thread *)place->proc_thread;
    wd->waiting_place = waiting_place;
    wake_fd = scheme_get_signal_handle();
    wd->wake_fd = wake_fd;
    wd->ready   = 0;

    worker_thread = mz_proc_thread_create(mz_proc_thread_wait_worker, wd);
    mz_proc_thread_detach(worker_thread);
    scheme_block_until(place_wait_ready, NULL, (Scheme_Object *) wd, 0);

    rc = scheme_make_integer((long)wd->rc);
    free(wd);
    return rc;
  }
# else
  {
    void *rcvoid;
    rcvoid = mz_proc_thread_wait((mz_proc_thread *)place->proc_thread);
    return scheme_make_integer((long) rcvoid);
  }
# endif
}

static Scheme_Object *scheme_place_p(int argc, Scheme_Object *args[])
{
  return SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type) ? scheme_true : scheme_false;
}

Scheme_Object *scheme_places_deep_copy(Scheme_Object *so) {
  Scheme_Object *new_so = so;
  if (SCHEME_INTP(so)) {
    return so;
  }

  switch (so->type) {
    case scheme_pair_type:
    case scheme_vector_type:
    case scheme_struct_type_type:
    case scheme_structure_type:
      {
        Scheme_Hash_Table *ht;
        ht = scheme_make_hash_table(SCHEME_hash_ptr);
        new_so = scheme_places_deep_copy_worker(so, ht);
      }
      break;
    default:
      new_so = scheme_places_deep_copy_worker(so, NULL);
      break;
  }
  return new_so;
}

Scheme_Object *scheme_places_deep_copy_worker(Scheme_Object *so, Scheme_Hash_Table *ht)
{
  Scheme_Object *new_so = so;
  if (SCHEME_INTP(so)) {
    return so;
  }
  if (ht) {
    Scheme_Object *r; 
    if ((r = scheme_hash_get(ht, so))) {
      return r;
    }
  }

  switch (so->type) {
    case scheme_true_type:
    case scheme_false_type:
    case scheme_null_type:
    /* place_bi_channels are allocated in the master and can be passed along as is */
    case scheme_place_bi_channel_type:
      new_so = so;
      break;
    case scheme_char_type:
      new_so = scheme_make_char(SCHEME_CHAR_VAL(so));
      break;
    case scheme_rational_type:
      {
        Scheme_Object *n;
        Scheme_Object *d;
        n = scheme_rational_numerator(so);
        d = scheme_rational_denominator(so);
        n = scheme_places_deep_copy_worker(n, ht);
        d = scheme_places_deep_copy_worker(d, ht);
        new_so = scheme_make_rational(n, d);
      }
      break;
    case scheme_float_type:
      new_so = scheme_make_char(SCHEME_FLT_VAL(so));
      break;
    case scheme_double_type:
      new_so = scheme_make_char(SCHEME_DBL_VAL(so));
      break;
    case scheme_complex_type:
      {
        Scheme_Object *r;
        Scheme_Object *i;
        r = scheme_complex_real_part(so);
        i = scheme_complex_imaginary_part(so);
        r = scheme_places_deep_copy_worker(r, ht);
        i = scheme_places_deep_copy_worker(i, ht);
        new_so = scheme_make_complex(r, i);
      }
      break;
    case scheme_char_string_type:
      new_so = scheme_make_sized_offset_char_string(SCHEME_CHAR_STR_VAL(so), 0, SCHEME_CHAR_STRLEN_VAL(so), 1);
      break;
    case scheme_byte_string_type:
      new_so = scheme_make_sized_offset_byte_string(SCHEME_BYTE_STR_VAL(so), 0, SCHEME_BYTE_STRLEN_VAL(so), 1);
      break;
    case scheme_unix_path_type:
      new_so = scheme_make_sized_offset_path(SCHEME_BYTE_STR_VAL(so), 0, SCHEME_BYTE_STRLEN_VAL(so), 1);
      break;
    case scheme_symbol_type:
      if (SCHEME_SYM_UNINTERNEDP(so)) {
        scheme_log_abort("cannot copy uninterned symbol");
        abort();
      } else
        new_so = so;
      break;
    case scheme_pair_type:
      {
        Scheme_Object *car;
        Scheme_Object *cdr;
        Scheme_Object *pair;
        car = scheme_places_deep_copy_worker(SCHEME_CAR(so), ht);
        cdr = scheme_places_deep_copy_worker(SCHEME_CDR(so), ht);
        pair = scheme_make_pair(car, cdr);
        return pair;
      }
      break;
    case scheme_vector_type:
      {
        Scheme_Object *vec;
        long i;
        long size = SCHEME_VEC_SIZE(so);
        vec = scheme_make_vector(size, 0);
        for (i = 0; i <size ; i++) {
          Scheme_Object *tmp;
          tmp = scheme_places_deep_copy_worker(SCHEME_VEC_ELS(so)[i], ht);
          SCHEME_VEC_ELS(vec)[i] = tmp;
        }
        SCHEME_SET_IMMUTABLE(vec);
        new_so = vec;
      }
      break;
    case scheme_structure_type:
      {
        Scheme_Structure *st = (Scheme_Structure*)so;
        Scheme_Structure *nst;
        Scheme_Struct_Type *stype = st->stype;
        Scheme_Struct_Type *ptype = stype->parent_types[stype->name_pos - 1];
        long i;
        long size = stype->num_slots;
        int local_slots = stype->num_slots - (ptype ? ptype->num_slots : 0);

        if (!stype->prefab_key) {
          scheme_log_abort("cannot copy non prefab structure");
          abort();
        }
        {
          int i = 0;
          for (i = 0; i < local_slots; i++) {
            if (!stype->immutables || stype->immutables[i] != 1) {
              scheme_log_abort("cannot copy mutable prefab structure");
              abort();
            }
          }
        }

        nst = (Scheme_Structure*) scheme_make_blank_prefab_struct_instance(stype);
        for (i = 0; i <size ; i++) {
          Scheme_Object *tmp;
          tmp = scheme_places_deep_copy_worker((Scheme_Object*) st->slots[i], ht);
          nst->slots[i] = tmp;
        }
        new_so = (Scheme_Object*)nst;
      }
      break;
    case scheme_resolved_module_path_type:
    default:
      scheme_log_abort("cannot copy object");
      abort();
      break;
  }
  if (ht) {
    scheme_hash_set(ht, so, new_so);
  }
  return new_so;
}

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

  cname = scheme_places_deep_copy(base);
  cuninit_val = scheme_places_deep_copy(uninit_val);
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

static void *place_start_proc(void *data_arg) {
  void *stack_base;
  void *rc;
  stack_base = PROMPT_STACK(stack_base);
  rc = place_start_proc_after_stack(data_arg, stack_base);
  stack_base = NULL;
  return rc;
}
  
static void *place_start_proc_after_stack(void *data_arg, void *stack_base) {
  Place_Start_Data *place_data;
  Scheme_Object *place_main;
  Scheme_Object *a[2], *channel;
  mzrt_thread_id ptid;
  long rc = 0;
  ptid = mz_proc_thread_self();
  
  place_data = (Place_Start_Data *) data_arg;
  data_arg = NULL;
 
  /* printf("Startin place: proc thread id%u\n", ptid); */

  /* create pristine THREAD_LOCAL variables*/
  null_out_runtime_globals();

  /* scheme_make_thread behaves differently if the above global vars are not null */
  scheme_place_instance_init(stack_base);

  a[0] = place_data->current_library_collection_paths;
  scheme_current_library_collection_paths(1, a);

  a[0] = scheme_places_deep_copy(place_data->module);
  a[1] = scheme_places_deep_copy(place_data->function);
  if (!SAME_TYPE(SCHEME_TYPE(place_data->channel), scheme_place_bi_channel_type)) {
    channel = scheme_places_deep_copy(place_data->channel);
  }
  else {
    channel = place_data->channel;
    scheme_place_bi_channel_set_signal(channel);
  }

  mzrt_sema_post(place_data->ready);
  place_data = NULL;
  /* at point point, don't refer to place_data or its content
     anymore, because it's allocated in the other place */

  scheme_set_root_param(MZCONFIG_EXIT_HANDLER, scheme_def_place_exit_proc);

  {
    Scheme_Thread * volatile p;
    mz_jmp_buf * volatile saved_error_buf;
    mz_jmp_buf new_error_buf;

    p = scheme_get_current_thread();
    saved_error_buf = p->error_buf;
    p->error_buf = &new_error_buf;
    if (!scheme_setjmp(new_error_buf)) {
      Scheme_Object *dynamic_require;
      dynamic_require = scheme_builtin_value("dynamic-require");
      place_main = scheme_apply(dynamic_require, 2, a);
      a[0] = channel;
      scheme_apply(place_main, 1, a);
    }
    else {
      rc = 1;
    }
    p->error_buf = saved_error_buf;
  }

  /*printf("Leavin place: proc thread id%u\n", ptid);*/
  scheme_place_instance_destroy();

  return (void*) rc;
}

Scheme_Object *scheme_places_deep_copy_in_master(Scheme_Object *so) {
# if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  void *return_payload;
  return_payload = scheme_master_fast_path(5, so);
  return (Scheme_Object*) return_payload;
# endif
  return so;
}

Scheme_Object *scheme_place_send(int argc, Scheme_Object *args[]) {
  if (argc == 2) {
    Scheme_Object *mso;
    Scheme_Place_Bi_Channel *ch;
    if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type)) {
      ch = (Scheme_Place_Bi_Channel *) ((Scheme_Place *) args[0])->channel;
    }
    else {
      ch = (Scheme_Place_Bi_Channel *)args[0];
    }
    mso = scheme_places_deep_copy_in_master(args[1]);
    scheme_place_async_send((Scheme_Place_Async_Channel *) ch->sendch, mso);
  }
  else {
    scheme_wrong_count_m("place-channel-send", 1, 2, argc, args, 0);
  }
  return scheme_true;
}

Scheme_Object *scheme_place_recv(int argc, Scheme_Object *args[]) {
  if (argc == 1) {
    Scheme_Place_Bi_Channel *ch;
    if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type)) {
      ch = (Scheme_Place_Bi_Channel *) ((Scheme_Place *) args[0])->channel;
    }
    else {
      ch = (Scheme_Place_Bi_Channel *) args[0];
    }
    return scheme_place_async_recv((Scheme_Place_Async_Channel *) ch->recvch);
  }
  else {
    scheme_wrong_count_m("place-channel-recv", 1, 2, argc, args, 0);
  }
  return scheme_true;
}

# ifdef MZ_PRECISE_GC
void force_hash_worker(Scheme_Object *so, Scheme_Hash_Table *ht);
Scheme_Hash_Table *force_hash(Scheme_Object *so) {
  if (SCHEME_INTP(so)) {
    return NULL;
  }

  switch (so->type) {
    case scheme_pair_type:
    case scheme_vector_type:
    case scheme_struct_type_type:
    case scheme_structure_type:
      {
        Scheme_Hash_Table *ht;
        ht = scheme_make_hash_table(SCHEME_hash_ptr);
        force_hash_worker(so, ht);
        return ht;
      }
      break;
    default:
      break;
  }
  return NULL;
}

void force_hash_worker(Scheme_Object *so, Scheme_Hash_Table *ht)
{
  if (SCHEME_INTP(so)) {
    return;
  }
  if (ht) {
    Scheme_Object *r; 
    if ((r = scheme_hash_get(ht, so))) {
      return;
    }
  }

  switch (so->type) {
    case scheme_true_type:
    case scheme_false_type:
    case scheme_null_type:
    case scheme_char_type:
    case scheme_rational_type:
    case scheme_float_type:
    case scheme_double_type:
    case scheme_complex_type:
    case scheme_char_string_type:
    case scheme_byte_string_type:
    case scheme_unix_path_type:
    case scheme_symbol_type:
      break;
    case scheme_pair_type:
      {
        force_hash_worker(SCHEME_CAR(so), ht);
        force_hash_worker(SCHEME_CDR(so), ht);
      }
      break;
    case scheme_vector_type:
      {
        long i;
        long size = SCHEME_VEC_SIZE(so);
        for (i = 0; i <size ; i++) {
          scheme_places_deep_copy_worker(SCHEME_VEC_ELS(so)[i], ht);
        }
      }
      break;
    case scheme_structure_type:
      {
        Scheme_Structure *st = (Scheme_Structure*)so;
        Scheme_Struct_Type *stype = st->stype;
        long i;
        long size = stype->num_slots;
        
        for (i = 0; i <size ; i++) {
          force_hash_worker((Scheme_Object*) st->slots[i], ht);
        }
      }
      break;
    case scheme_resolved_module_path_type:
    default:
      scheme_log_abort("cannot copy object");
      abort();
      break;
  }
  if (ht) {
    scheme_hash_set(ht, so, NULL);
  }
  return;
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
  Scheme_Hash_Table *ht;

  switch(msg_type) {
    case 1:
    case 5:
      ht = force_hash(msg_payload);
      break;
  }

# ifdef MZ_PRECISE_GC
  original_gc = GC_switch_to_master_gc();
  scheme_start_atomic();
# endif
  o = scheme_master_place_handlemsg(msg_type, msg_payload);
# ifdef MZ_PRECISE_GC
  scheme_end_atomic_no_swap();
  GC_switch_back_from_master(original_gc);
# endif

  return o;
}


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

Scheme_Object *scheme_place_async_channel_create() {
  Scheme_Object **msgs;
  Scheme_Place_Async_Channel *ch;

  ch = GC_master_malloc(sizeof(Scheme_Place_Async_Channel));
  msgs = GC_master_malloc(sizeof(Scheme_Object*) * 8);

  ch->so.type = scheme_place_async_channel_type;
  ch->in = 0;
  ch->out = 0;
  ch->count = 0;
  ch->size = 8;
  mzrt_mutex_create(&ch->lock);
  ch->msgs = msgs;
  ch->wakeup_signal = NULL;
  return (Scheme_Object *)ch;
}

Scheme_Object *scheme_place_bi_channel_create() {
  Scheme_Object *tmp;
  Scheme_Place_Bi_Channel *ch;

  ch = GC_master_malloc(sizeof(Scheme_Place_Bi_Channel));
  ch->so.type = scheme_place_bi_channel_type;

  tmp = scheme_place_async_channel_create();
  ch->sendch = tmp;
  tmp = scheme_place_async_channel_create();
  ch->recvch = tmp;
  return (Scheme_Object *)ch;
}

Scheme_Object *scheme_place_bi_peer_channel_create(Scheme_Object *orig) {
  Scheme_Place_Bi_Channel *ch;

  ch = GC_master_malloc(sizeof(Scheme_Place_Bi_Channel));
  ch->so.type = scheme_place_bi_channel_type;

  ch->sendch = ((Scheme_Place_Bi_Channel *)orig)->recvch;
  ch->recvch = ((Scheme_Place_Bi_Channel *)orig)->sendch;
  return (Scheme_Object *)ch;
}

static Scheme_Object *scheme_place_channel(int argc, Scheme_Object *args[]) {
  if (argc == 0) {
    return scheme_place_bi_channel_create();
  }
  else {
    scheme_wrong_count_m("place-channel", 0, 0, argc, args, 0);
  }
  return scheme_true;
}

static Scheme_Object *scheme_place_channel_receiver_channel(int argc, Scheme_Object *args[]) {
  if (argc == 1) {
    if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_bi_channel_type)) {
      return scheme_place_bi_peer_channel_create(args[0]);
    }
    else {
      scheme_wrong_type("place-channel->receive-channel", "place-channel?", 0, argc, args);
    }
  }
  else {
    scheme_wrong_count_m("place-channel-send", 1, 1, argc, args, 0);
  }
  return scheme_true;
}


static void scheme_place_bi_channel_set_signal(Scheme_Object *cho) {
  Scheme_Place_Async_Channel *ch;
  void *signaldescr;
  signaldescr = scheme_get_signal_handle();
  ch = (Scheme_Place_Async_Channel *) ((Scheme_Place_Bi_Channel *)cho)->recvch;
  ch->wakeup_signal = signaldescr;
}

static Scheme_Object *scheme_place_channel_p(int argc, Scheme_Object *args[])
{
  return SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_bi_channel_type) ? scheme_true : scheme_false;
}


void scheme_place_async_send(Scheme_Place_Async_Channel *ch, Scheme_Object *o) {
  int cnt;
  mzrt_mutex_lock(ch->lock);
  {
    cnt = ch->count;
    if (ch->count == ch->size) { /* GROW QUEUE */
      Scheme_Object **new_msgs;

      new_msgs = GC_master_malloc(sizeof(Scheme_Object*) *2);

      if (ch->out < ch->in) {
        memcpy(new_msgs, ch->msgs + ch->out, sizeof(Scheme_Object *) * (ch->in - ch->out));
      }
      else {
        int s1 = (ch->size - ch->out);
        memcpy(new_msgs, ch->msgs + ch->out, sizeof(Scheme_Object *) * s1);
        memcpy(new_msgs + s1, ch->msgs, sizeof(Scheme_Object *) * ch->in);
      }
      
      ch->msgs = new_msgs;
      ch->in = ch->size;
      ch->out = 0;
      ch->size *= 2;
    }

    ch->msgs[ch->in] = o;
    ++ch->count;
    ch->in = (++ch->in % ch->size);
  }
  mzrt_mutex_unlock(ch->lock);

  if (!cnt && ch->wakeup_signal) {
    /*wake up possibly sleeping receiver */  
    scheme_signal_received_at(ch->wakeup_signal);
  }
}

static int scheme_place_async_ch_ready(Scheme_Place_Async_Channel *ch) {
  int ready = 0;
  mzrt_mutex_lock(ch->lock);
  {
    if (ch->count > 0) ready = 1;
  }
  mzrt_mutex_unlock(ch->lock);
  return ready;
}

static int scheme_place_channel_ready(Scheme_Object *so) {
  Scheme_Place_Bi_Channel *ch;
  if (SAME_TYPE(SCHEME_TYPE(so), scheme_place_type)) {
    ch = (Scheme_Place_Bi_Channel *) ((Scheme_Place *) so)->channel;
  }
  else {
    ch = (Scheme_Place_Bi_Channel *)so;
  }

  return scheme_place_async_ch_ready((Scheme_Place_Async_Channel *) ch->recvch);
}

Scheme_Object *scheme_place_async_recv(Scheme_Place_Async_Channel *ch) {
  Scheme_Object *msg = NULL;
  while(1) {
    mzrt_mutex_lock(ch->lock);
    {
      if (ch->count > 0) { /* GET MSG */
        msg = ch->msgs[ch->out];
        ch->msgs[ch->out] = NULL;
        --ch->count;
        ch->out = (++ch->out % ch->size);
      }
    }
    mzrt_mutex_unlock(ch->lock);
    if(msg) break;
    scheme_thread_block(0);
    scheme_block_until((Scheme_Ready_Fun) scheme_place_async_ch_ready, NULL, (Scheme_Object *) ch, 0);
  }
  return msg;
}

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
  GC_REG_TRAV(scheme_place_async_channel_type, place_async_channel_val);
  GC_REG_TRAV(scheme_place_bi_channel_type, place_bi_channel_val);
}

END_XFORM_SKIP;

#endif

/************************************************************************/
/************************************************************************/
/************************************************************************/

#endif
