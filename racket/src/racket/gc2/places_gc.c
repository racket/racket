
#ifdef MZ_USE_PLACES
static NewGC *MASTERGC;
static NewGCMasterInfo *MASTERGCINFO;
inline static int premaster_or_master_gc(NewGC *gc) {
  return (!MASTERGC || gc == MASTERGC);
}
inline static int premaster_or_place_gc(NewGC *gc) {
  return (!MASTERGC || gc != MASTERGC);
}
inline static int postmaster_and_master_gc(NewGC *gc) {
  return (MASTERGC && gc == MASTERGC);
}
inline static int postmaster_and_place_gc(NewGC *gc) {
  return (MASTERGC && gc != MASTERGC);
}

intptr_t GC_is_place() {
  NewGC *gc = GC_get_GC();
  return postmaster_and_place_gc(gc);
}

struct Log_Master_Info {
  int ran, full;
  intptr_t pre_used, post_used, pre_admin, post_admin;
};

# define PLACES_AND(v) v
#else
# define premaster_or_master_gc(gc)   1
# define premaster_or_place_gc(gc)    1
# define postmaster_and_master_gc(gc) 0
# define postmaster_and_place_gc(gc)  1
# define PLACES_AND(v) 0
#endif

#ifdef MZ_USE_PLACES
static void adjust_page_lock(int is_a_master_page, mpage *page, intptr_t prev, intptr_t next)
{
  if (is_a_master_page) {
    while (!mzrt_cas(&page->page_lock, prev, next)) { /* spin! */ }
  }
}
# define TAKE_PAGE_LOCK(is_a_master_page, page) adjust_page_lock(is_a_master_page, page, 0, 1);
# define RELEASE_PAGE_LOCK(is_a_master_page, page) adjust_page_lock(is_a_master_page, page, 1, 0);
#else
# define TAKE_PAGE_LOCK(is_a_master_page, page) /* empty */
# define RELEASE_PAGE_LOCK(is_a_master_page, page) /* empty */
#endif

/*****************************************************************************/
/* Debugging                                                                 */
/*****************************************************************************/

#if defined(MZ_USE_PLACES) && defined(GC_DEBUG_PAGES)
static FILE* gcdebugOUT(NewGC *gc) {

  if (gc->GCVERBOSEFH) { fflush(gc->GCVERBOSEFH); }
  else {
    char buf[50];
    sprintf(buf, "GCDEBUGOUT_%i", gc->place_id);
    gc->GCVERBOSEFH = fopen(buf, "w");
  }
  return gc->GCVERBOSEFH;
}

static void GCVERBOSEprintf(NewGC *gc, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(gcdebugOUT(gc), fmt, ap);
    va_end(ap);
}

static void GCVERBOSEPAGE(NewGC *gc, const char *msg, mpage* page) {
  GCVERBOSEprintf(gc, "%s %p: %p %p %p\n", msg, gc, page, page->addr, (void*)((intptr_t)page->addr + real_page_size(page)));
}
#else
# define GCVERBOSEPAGE(gc, msg, page) /* EMPTY */
MAYBE_UNUSED static void GCVERBOSEprintf(NewGC *gc, const char *fmt, ...) {
}
#endif

/*****************************************************************************/
/* Coordinating place GC                                                     */
/*****************************************************************************/

#ifdef MZ_USE_PLACES
enum {
  SIGNALED_BUT_NOT_REGISTERED = -3,
  REAPED_SLOT_AVAILABLE       = -2,
  CREATED_BUT_NOT_REGISTERED  = -1,
};

void GC_allow_master_gc_check() {
  NewGC *gc = GC_get_GC();
  gc->dont_master_gc_until_child_registers = 0;
}
static void NewGCMasterInfo_initialize() {
  int i;
  MASTERGCINFO = ofm_malloc_zero(sizeof(NewGCMasterInfo));
  MASTERGCINFO->size = 4;
  MASTERGCINFO->alive = 0;
  MASTERGCINFO->ready = 0;
  MASTERGCINFO->signal_fds = (void **)ofm_malloc(sizeof(void*) * MASTERGCINFO->size);
  for (i=0; i < MASTERGCINFO->size; i++ ) {
    MASTERGCINFO->signal_fds[i] = (void *)REAPED_SLOT_AVAILABLE;
  }
  mzrt_rwlock_create(&MASTERGCINFO->cangc);
  mzrt_sema_create(&MASTERGCINFO->wait_go_sema, 0);
  mzrt_sema_create(&MASTERGCINFO->wait_done_sema, 0);
}

#if 0
/* Not yet used: */
static void NewGCMasterInfo_cleanup() {
  mzrt_rwlock_destroy(MASTERGCINFO->cangc);
  ofm_free(MASTERGCINFO->signal_fds, sizeof(void*) * MASTERGCINFO->size);
  ofm_free(MASTERGCINFO, sizeof(NewGCMasterInfo));
  MASTERGCINFO = NULL;
}
#endif

/* signals every place to do a full gc at then end of 
   garbage_collect the places will call 
   wait_while_master_in_progress and
   rendezvous for a master gc */
/* this is only called from the master so the cangc lock should already be held */
static void master_collect_request() {
  if (MASTERGC->major_places_gc == 0) {
    int i = 0;
    int size = MASTERGCINFO->size;
    int count = 0;
    MASTERGC->major_places_gc = 1;
    MASTERGCINFO->ready = 0;

    for (i = 1; i < size; i++) {
      void *signal_fd = MASTERGCINFO->signal_fds[i];
      if (signal_fd < (void*) -2) { 
        scheme_signal_received_at(signal_fd);
#if defined(GC_DEBUG_PAGES)
        printf("%i SIGNALED BUT NOT COLLECTED\n", i);
        GCVERBOSEprintf(gc, "%i SIGNALED BUT NOT COLLECTED\n", i);
#endif
        count++;
      }
      else if ( signal_fd == (void*)-1) {
        /* printf("%i SIGNALED BUT NOT REGISTERED YET\n", i); */
        MASTERGCINFO->signal_fds[i] = (void*) SIGNALED_BUT_NOT_REGISTERED;
        count++;
      }
      if (count == (MASTERGCINFO->alive - 1)) {
        break;
      }
    }
    if (count != (MASTERGCINFO->alive - 1)) {
      printf("GC2 count != MASTERGCINFO->alive %i %" PRIdPTR "\n", count, MASTERGCINFO->alive);
      abort();
    }
#if defined(GC_DEBUG_PAGES)
    printf("Woke up %i places for MASTER GC\n", count);
    GCVERBOSEprintf(gc, "Woke up %i places for MASTER GC\n", count);
#endif
  }
}

static void collect_master(Log_Master_Info *lmi) {
  NewGC *saved_gc;
  saved_gc = GC_switch_to_master_gc();
  {
#if defined(GC_DEBUG_PAGES)
    NewGC *gc = GC_get_GC();
    printf("START MASTER COLLECTION\n");
    GCVERBOSEprintf(gc, "START MASTER COLLECTION\n");
#endif
    MASTERGC->major_places_gc = 0;
    garbage_collect(MASTERGC, 1, 0, 0, lmi);
#if defined(GC_DEBUG_PAGES)
    printf("END MASTER COLLECTION\n");
    GCVERBOSEprintf(gc, "END MASTER COLLECTION\n");
#endif

    MASTERGC->prev_pending_msg_size = MASTERGC->pending_msg_size;

    {
      int i = 0;
      int alive = MASTERGCINFO->alive;
      /* wake everyone back up, except MASTERGC and ourself */  
      for (i = 2; i < alive; i++) {
        mzrt_sema_post(MASTERGCINFO->wait_done_sema);
      }
    }
  }
  GC_switch_back_from_master(saved_gc);
}

static void sync_master_progress(NewGC *gc, int done, Log_Master_Info *lmi) {
  int last_one_here = -1;

  mzrt_rwlock_wrlock(MASTERGCINFO->cangc);

  if (MASTERGC->major_places_gc == 1) {
    MASTERGCINFO->ready++;
#if defined(GC_DEBUG_PAGES)
    printf("%i READY\n", gc->place_id);
    GCVERBOSEprintf(gc, "%i READY\n", gc->place_id);
    GCVERBOSEprintf(gc, "START MASTER COLLECTION\n");
#endif
    /* don't count MASTERGC */
    if ((MASTERGCINFO->alive - 1) == MASTERGCINFO->ready) {
      last_one_here = 1;
      MASTERGCINFO->ready = 0;
    } else {
      last_one_here = 0; 
    }
  } else {
    last_one_here = -1;
  }

  mzrt_rwlock_unlock(MASTERGCINFO->cangc);

  switch(last_one_here) {
  case -1:
    /* master doesn't want to collect */
    return;
    break;
  case 0:
    /* wait on semaphore */
    if (done) {
      mzrt_sema_wait(MASTERGCINFO->wait_done_sema);
      GCVERBOSEprintf(gc, "END MASTER COLLECTION\n");
    } else
      mzrt_sema_wait(MASTERGCINFO->wait_go_sema);
    break;
  case 1:
    /* You're the last one here. */
    if (done) {
      collect_master(lmi); /* notifies other places on completion */
      GCVERBOSEprintf(gc, "END MASTER COLLECTION\n");
    } else {
      int i = 0;
      int alive = MASTERGCINFO->alive;
      /* wake everyone back up, except MASTERGC and ourself */
      for (i = 2; i < alive; i++) {
        mzrt_sema_post(MASTERGCINFO->wait_go_sema);
      }
    }
    break;
  default:
    printf("GC2 sync_master_in_progress invalid case, unreachable\n");
    abort();
    break;
  }
}

static void wait_until_master_in_progress(NewGC *gc) {
  sync_master_progress(gc, 0, NULL);
}

static void wait_while_master_in_progress(NewGC *gc, Log_Master_Info *lmi) {
  sync_master_progress(gc, 1, lmi);
}

/* MUST CALL WITH cangc lock */
static intptr_t NewGCMasterInfo_find_free_id() {
  int i, size;
  
  GC_ASSERT(MASTERGCINFO->alive <= MASTERGCINFO->size);
  if ((MASTERGCINFO->alive + 1) == MASTERGCINFO->size) {
    void **new_signal_fds;

    size = MASTERGCINFO->size * 2;
    new_signal_fds = ofm_malloc(sizeof(void*) * size);
    memcpy(new_signal_fds, MASTERGCINFO->signal_fds, sizeof(void*) * MASTERGCINFO->size);

    for (i = MASTERGCINFO->size; i < size; i++ ) {
      new_signal_fds[i] = (void *)REAPED_SLOT_AVAILABLE;
    }

    ofm_free(MASTERGCINFO->signal_fds, sizeof(void*) * MASTERGCINFO->size);

    MASTERGCINFO->signal_fds = new_signal_fds;
    MASTERGCINFO->size = size;
  }

  size = MASTERGCINFO->size;
  for (i = 0; i < size; i++) {
    if (MASTERGCINFO->signal_fds[i] == (void*) REAPED_SLOT_AVAILABLE) {
      MASTERGCINFO->alive++;
      return i;
    }
  }

  printf("Error in MASTERGCINFO table\n");
  abort();
  return 0;
} 

static void NewGCMasterInfo_register_gc(NewGC *newgc) {
  mzrt_rwlock_wrlock(MASTERGCINFO->cangc); 
  {
    intptr_t newid = NewGCMasterInfo_find_free_id();
    newgc->place_id = newid;
    MASTERGCINFO->signal_fds[newid] = (void *) CREATED_BUT_NOT_REGISTERED;
  }
  mzrt_rwlock_unlock(MASTERGCINFO->cangc);
}

void GC_set_put_external_event_fd(void *fd) {
  NewGC *gc = GC_get_GC();
  mzrt_rwlock_wrlock(MASTERGCINFO->cangc);
  {
    if ( MASTERGCINFO->signal_fds[gc->place_id] == (void*) SIGNALED_BUT_NOT_REGISTERED) {
      scheme_signal_received_at(fd);
      /* printf("%i THERE WAITING ON ME\n", gc->place_id); */
    }
    MASTERGCINFO->signal_fds[gc->place_id] = fd;
  } 
  mzrt_rwlock_unlock(MASTERGCINFO->cangc);
}
#endif

/*****************************************************************************/
/* Creating and switching GCs                                                */
/*****************************************************************************/

#ifdef MZ_USE_PLACES
void GC_construct_child_gc(struct NewGC *parent_gc, intptr_t limit) {
  NewGC *gc = MASTERGC;
  NewGC *newgc = init_type_tags_worker(gc, parent_gc, 0, 0, 0, gc->weak_box_tag, gc->ephemeron_tag, 
                                       gc->weak_array_tag, gc->cust_box_tag, gc->phantom_tag);
  newgc->primoridal_gc = MASTERGC;
  newgc->dont_master_gc_until_child_registers = 1;
  if (limit)
    newgc->place_memory_limit = limit;
}

void GC_destruct_child_gc() {
  NewGC *gc = GC_get_GC();
  int waiting = 0;

  do {
    mzrt_rwlock_wrlock(MASTERGCINFO->cangc);
    waiting = MASTERGC->major_places_gc;
    if (!waiting) {
      MASTERGCINFO->signal_fds[gc->place_id] = (void *) REAPED_SLOT_AVAILABLE;
      gc->place_id = -1;
      MASTERGCINFO->alive--;
    }
    mzrt_rwlock_unlock(MASTERGCINFO->cangc);

    if (waiting) {
      collect_now(gc, 1, 0);
      waiting = 1;
    }
  } while (waiting == 1);

  free_child_gc();
}

static inline void save_globals_to_gc(NewGC *gc) {
  gc->saved_GC_variable_stack       = GC_variable_stack;
  gc->saved_GC_gen0_alloc_page_ptr  = GC_gen0_alloc_page_ptr;
  gc->saved_GC_gen0_alloc_page_end  = GC_gen0_alloc_page_end;
}

static inline void restore_globals_from_gc(NewGC *gc) {
  GC_variable_stack       = gc->saved_GC_variable_stack;
  GC_gen0_alloc_page_ptr  = gc->saved_GC_gen0_alloc_page_ptr;
  GC_gen0_alloc_page_end  = gc->saved_GC_gen0_alloc_page_end;
}

void GC_switch_out_master_gc() {
  static int initialized = 0;

  if(!initialized) {
    NewGC *gc = GC_get_GC();

    initialized = 1;

    if (!gc->avoid_collection)
      garbage_collect(gc, 1, 0, 1, NULL);

#ifdef MZ_USE_PLACES
    GC_gen0_alloc_page_ptr = 2;
    GC_gen0_alloc_page_end = 1;
    gc->dont_master_gc_until_child_registers = 0;
#endif
 
    MASTERGC = gc;

    save_globals_to_gc(MASTERGC);
    GC_construct_child_gc(NULL, 0);
    GC_allow_master_gc_check();
  }
  else {
    GCPRINT(GCOUTF, "GC_switch_out_master_gc should only be called once!\n");
    abort();
  }
}

/*used in scheme_master_fast_path*/
void *GC_switch_to_master_gc() {
  NewGC *gc = GC_get_GC();
  /* return if MASTERGC hasn't been constructed yet, allow recursive locking */
  if (premaster_or_master_gc(gc)) { return MASTERGC; }

  save_globals_to_gc(gc);

  /*obtain exclusive access to MASTERGC*/
  mzrt_rwlock_wrlock(MASTERGCINFO->cangc);

  GC_set_GC(MASTERGC);
  restore_globals_from_gc(MASTERGC);
  return gc;
}

void GC_switch_back_from_master(void *gc) {
  /* return if MASTERGC hasn't been constructed yet, allow recursive locking */
  if (premaster_or_master_gc(gc)) { return; }
  save_globals_to_gc(MASTERGC);

  /*release exclusive access to MASTERGC*/
  mzrt_rwlock_unlock(MASTERGCINFO->cangc);

  GC_set_GC(gc);
  restore_globals_from_gc(gc);
}

int GC_is_using_master() {
  return postmaster_and_master_gc(GC_get_GC());
}

#endif

/*****************************************************************************/
/* More debugging                                                            */
/*****************************************************************************/

/* Debuging pointer ownership */
/* #define POINTER_OWNERSHIP_CHECK 1 */

#define SHARED_PAGE_ORPHANED ((NewGC *)0x1)

#ifdef POINTER_OWNERSHIP_CHECK

static mzrt_mutex *lock;
static PageMap shared_pagemap;

static void shared_pagemap_set(void *ptr, size_t len, NewGC *owner)
{
  if (!lock) {
    mzrt_mutex_create(&lock);
# ifdef SIXTY_FOUR_BIT_INTEGERS
    shared_pagemap = ofm_malloc_zero(PAGEMAP64_LEVEL1_SIZE * sizeof (mpage***));
# else
    shared_pagemap = ofm_malloc_zero(PAGEMAP32_SIZE * sizeof (mpage*));
# endif
  }

  mzrt_mutex_lock(lock);
  while (len > 0) {
    pagemap_set(shared_pagemap, ptr, (mpage*)owner);
    len -= APAGE_SIZE;
    ptr = (char *)ptr + APAGE_SIZE;
  }
  mzrt_mutex_unlock(lock);
}

static void check_page_owner(NewGC *gc, const void *p)
{
  NewGC *owner;
  owner = (NewGC *)pagemap_find_page(shared_pagemap, p);
  if (owner && (owner != gc) && (owner != MASTERGC) && (owner != SHARED_PAGE_ORPHANED)) {
    mzrt_mutex_lock(lock);
    owner = (NewGC *)pagemap_find_page(shared_pagemap, p);
    if (owner && (owner != gc) && (owner != MASTERGC) && (owner != SHARED_PAGE_ORPHANED)) {
      printf("%p is owned by place %i not the current place %i\n", p, owner->place_id, gc->place_id);
      abort();
    }
    mzrt_mutex_unlock(lock);
  }
}

#else

static void shared_pagemap_set(void *ptr, size_t len, NewGC *owner) { }
static void check_page_owner(NewGC *gc, const void *p) { }

#endif

/*****************************************************************************/
/* Memory-use propagation                                                    */
/*****************************************************************************/

intptr_t GC_propagate_hierarchy_memory_use() 
{
  NewGC *gc = GC_get_GC();

#ifdef MZ_USE_PLACES
  if (gc->parent_gc) {
    /* report memory use to parent */
    intptr_t total = gc->memory_in_use + gc->child_gc_total;
    intptr_t delta = total - gc->previously_reported_total;
    mzrt_mutex_lock(gc->parent_gc->child_total_lock);
    gc->parent_gc->child_gc_total += delta;
    mzrt_mutex_unlock(gc->parent_gc->child_total_lock);
    gc->previously_reported_total = total;
  }
#endif

  return add_no_overflow(gc->memory_in_use, gc->child_gc_total);
}
