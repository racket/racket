/*****************************************************************************/
/* memory accounting                                                         */
/*****************************************************************************/
#ifdef NEWGC_BTC_ACCOUNT

#include "../src/schpriv.h"
/* BTC_ prefixed functions are called by newgc.c */
/* btc_ prefixed functions are internal to mem_account.c */

static const int btc_redirect_thread    = 511;
static const int btc_redirect_custodian = 510;
static const int btc_redirect_ephemeron = 509;
static const int btc_redirect_cust_box  = 508;
static const int btc_redirect_bi_chan   = 507;

inline static void account_memory(NewGC *gc, int set, intptr_t amount, int to_master);

/*****************************************************************************/
/* thread list                                                               */
/*****************************************************************************/
inline static int current_owner(NewGC *gc, Scheme_Custodian *c);

inline static void BTC_register_new_thread(void *t, void *c)
{
  NewGC *gc = GC_get_GC();
  GC_Thread_Info *work;

  work = (GC_Thread_Info *)ofm_malloc(sizeof(GC_Thread_Info));
  if (((Scheme_Object *)t)->type == scheme_thread_type)
    ((Scheme_Thread *)t)->gc_info = work;
  else
    ((Scheme_Place *)t)->gc_info = work;
  work->owner = current_owner(gc, (Scheme_Custodian *)c);
  work->thread = t;

  work->next = gc->thread_infos;
  gc->thread_infos = work;
}

inline static void BTC_register_thread(void *t, void *c)
{
  NewGC *gc = GC_get_GC();
  GC_Thread_Info *work;
  
  if (((Scheme_Object *)t)->type == scheme_thread_type)
    work = ((Scheme_Thread *)t)->gc_info;
  else
    work = ((Scheme_Place *)t)->gc_info;
  work->owner = current_owner(gc, (Scheme_Custodian *)c);
}

inline static void mark_threads(NewGC *gc, int owner)
{
  GC_Thread_Info *work;
  Mark2_Proc thread_mark = gc->mark_table[btc_redirect_thread];

  for(work = gc->thread_infos; work; work = work->next) {
    if (work->owner == owner) {
      if (((Scheme_Object *)work->thread)->type == scheme_thread_type) {
        /* thread */
        if (((Scheme_Thread *)work->thread)->running) {
          thread_mark(work->thread, gc);
          if (work->thread == scheme_current_thread) {
            GC_mark_variable_stack(GC_variable_stack, 0, get_stack_base(gc), NULL);
          }
        }
      } else {
        /* place */
#ifdef MZ_USE_PLACES
        /* add in the memory used by the place's GC */
        intptr_t sz;
        Scheme_Place_Object *place_obj = ((Scheme_Place *)work->thread)->place_obj;
        if (place_obj) {
          mzrt_mutex_lock(place_obj->lock);
          sz = place_obj->memory_use;
          mzrt_mutex_unlock(place_obj->lock);
          account_memory(gc, owner, gcBYTES_TO_WORDS(sz), 0);
        }
#endif
      }
    }
  }
}

inline static void clean_up_thread_list(NewGC *gc)
{
  GC_Thread_Info *work = gc->thread_infos;
  GC_Thread_Info *prev = NULL;

  while(work) {
    if(!pagemap_find_page(gc->page_maps, work->thread) || marked(gc, work->thread)) {
      work->thread = GC_resolve2(work->thread, gc);
      prev = work;
      work = work->next;
    } else {
      GC_Thread_Info *next = work->next;

      if(prev) prev->next = next;
      if(!prev) gc->thread_infos = next;
      free(work);
      work = next;
    }
  }
}

inline static int thread_get_owner(void *p)
{
  return ((Scheme_Thread *)p)->gc_info->owner;
}

#define OWNER_TABLE_INIT_AMT 10

inline static int create_blank_owner_set(NewGC *gc)
{
  int i;
  unsigned int curr_size = gc->owner_table_size;
  OTEntry **owner_table = gc->owner_table;
  unsigned int old_size;
  OTEntry **naya;

  for (i = 1; i < curr_size; i++) {
    if (!owner_table[i]) {
      owner_table[i] = ofm_malloc(sizeof(OTEntry));
      bzero(owner_table[i], sizeof(OTEntry));
      return i;
    }
  }

  old_size = curr_size;
  if (!curr_size) {
    curr_size = OWNER_TABLE_INIT_AMT;
  }
  else {
    curr_size *= 2;
  }
  gc->owner_table_size = curr_size;

  naya = (OTEntry **)ofm_malloc(curr_size * sizeof(OTEntry*));
  memcpy(naya, owner_table, old_size*sizeof(OTEntry*));
  gc->owner_table = owner_table = naya;
  bzero(((char*)owner_table) + (sizeof(OTEntry*) * old_size),
        (curr_size - old_size) * sizeof(OTEntry*));

  return create_blank_owner_set(gc);
}

inline static int custodian_to_owner_set(NewGC *gc,Scheme_Custodian *cust)
{
  int i;

  if (cust->gc_owner_set)
    return cust->gc_owner_set;

  i = create_blank_owner_set(gc);
  gc->owner_table[i]->originator = cust;
  cust->gc_owner_set = i;

  return i;
}

inline static int current_owner(NewGC *gc, Scheme_Custodian *c)
{
  if (!scheme_current_thread)
    return 1;
  else if (!c)
    return thread_get_owner(scheme_current_thread);
  else
    return custodian_to_owner_set(gc, c);
}

void BTC_register_root_custodian(void *_c)
{
  NewGC *gc = GC_get_GC();
  Scheme_Custodian *c = (Scheme_Custodian *)_c;

  if (gc->owner_table) {
    /* Reset */
    free(gc->owner_table);
    gc->owner_table = NULL;
    gc->owner_table_size = 0;
  }

  if (create_blank_owner_set(gc) != 1) {
    GCPRINT(GCOUTF, "Something extremely weird (and bad) has happened.\n");
    abort();
  }

  gc->owner_table[1]->originator = c;
  c->gc_owner_set = 1;
}

inline static int custodian_member_owner_set(NewGC *gc, void *cust, int set)
{
  Scheme_Custodian_Reference *box;
  Scheme_Custodian *work = (Scheme_Custodian *) gc->owner_table[set]->originator;

  while(work) {
    if(work == cust) return 1;
    box = work->parent;
    work = box ? SCHEME_PTR1_VAL(box) : NULL;
  }
  return 0;
}

inline static void account_memory(NewGC *gc, int set, intptr_t amount, int to_master)
{
  if (to_master)
    gc->owner_table[set]->master_memory_use += amount;
  else
    gc->owner_table[set]->memory_use += amount;
}

inline static void free_owner_set(NewGC *gc, int set)
{
  OTEntry **owner_table = gc->owner_table;
  if(owner_table[set]) {
    free(owner_table[set]);
  }
  owner_table[set] = NULL;
}

inline static void clean_up_owner_table(NewGC *gc)
{
  OTEntry **owner_table = gc->owner_table;
  const int table_size = gc->owner_table_size;
  int i;

  for(i = 1; i < table_size; i++)
    if(owner_table[i]) {
      /* repair or delete the originator */
      if(!marked(gc, owner_table[i]->originator)) {
        owner_table[i]->originator = NULL;
      } else 
        owner_table[i]->originator = GC_resolve2(owner_table[i]->originator, gc);

      /* potential delete */
      if(i != 1) 
        if((owner_table[i]->memory_use == 0) && !owner_table[i]->originator)
          free_owner_set(gc, i);
    }
}

inline static uintptr_t custodian_usage(NewGC*gc, void *custodian)
{
  OTEntry **owner_table;
  uintptr_t retval = 0;
  int i;

  if(!gc->really_doing_accounting) {
    if (!gc->avoid_collection) {
      CHECK_PARK_UNUSED(gc);
      gc->park[0] = custodian;
      gc->really_doing_accounting = 1;
      garbage_collect(gc, 1, 0, NULL);
      custodian = gc->park[0]; 
      gc->park[0] = NULL;
    }
  }

  i = custodian_to_owner_set(gc, (Scheme_Custodian *)custodian);

  owner_table = gc->owner_table;
  if (owner_table[i])
    retval = (owner_table[i]->memory_use + owner_table[i]->master_memory_use);
  else
    retval = 0;

  return gcWORDS_TO_BYTES(retval);
}

inline static void BTC_memory_account_mark(NewGC *gc, mpage *page, void *ptr, int is_a_master_page)
{
  GCDEBUG((DEBUGOUTF, "BTC_memory_account_mark: %p/%p\n", page, ptr));

  /* In the case of is_a_master_page, whether this place is charged is
     a little random: there's no guarantee that the btc_mark values are
     in sync, and there are races among places. Approximations are ok for
     accounting, though, as long as the probably for completely wrong
     accounting is very low. */

  if(page->size_class) {
    if(page->size_class > 1) {
      /* big page */
      objhead *info = BIG_PAGE_TO_OBJHEAD(page);
      
      if(info->btc_mark == gc->old_btc_mark) {
        info->btc_mark = gc->new_btc_mark;
        account_memory(gc, gc->current_mark_owner, gcBYTES_TO_WORDS(page->size), is_a_master_page);
        push_ptr(gc, TAG_AS_BIG_PAGE_PTR(ptr));
      }
    } else {
      /* medium page */
      objhead *info = MED_OBJHEAD(ptr, page->size);

      if(info->btc_mark == gc->old_btc_mark) {
        info->btc_mark = gc->new_btc_mark;
        account_memory(gc, gc->current_mark_owner, info->size, is_a_master_page);
        ptr = OBJHEAD_TO_OBJPTR(info);
        push_ptr(gc, ptr);
      }
    }
  } else {
    objhead *info = OBJPTR_TO_OBJHEAD(ptr);

    if(info->btc_mark == gc->old_btc_mark) {
      info->btc_mark = gc->new_btc_mark;
      account_memory(gc, gc->current_mark_owner, info->size, 0);
      push_ptr(gc, ptr);
    }
  }
}

inline static void mark_cust_boxes(NewGC *gc, Scheme_Custodian *cur)
{
  Scheme_Object *pr, *prev = NULL, *next;
  GC_Weak_Box *wb;
  Mark2_Proc cust_box_mark = gc->mark_table[btc_redirect_cust_box];

  /* cust boxes is a list of weak boxes to cust boxes */

  pr = cur->cust_boxes;
  while (pr) {
    wb = (GC_Weak_Box *)SCHEME_CAR(pr);
    next = SCHEME_CDR(pr);
    if (wb->val) {
      cust_box_mark(wb->val, gc);
      prev = pr;
    } else {
      if (prev)
        SCHEME_CDR(prev) = next;
      else
        cur->cust_boxes = next;
      --cur->num_cust_boxes;
    }
    pr = next;
  }
  cur->checked_cust_boxes = cur->num_cust_boxes;
}

int BTC_thread_mark(void *p, struct NewGC *gc)
{
  if (gc->doing_memory_accounting) {
    return OBJPTR_TO_OBJHEAD(p)->size;
  }
  return gc->mark_table[btc_redirect_thread](p, gc);
}

int BTC_custodian_mark(void *p, struct NewGC *gc)
{
  if (gc->doing_memory_accounting) {
    if(custodian_to_owner_set(gc, p) == gc->current_mark_owner)
      return gc->mark_table[btc_redirect_custodian](p, gc);
    else
      return OBJPTR_TO_OBJHEAD(p)->size;
  }
  return gc->mark_table[btc_redirect_custodian](p, gc);
}

int BTC_cust_box_mark(void *p, struct NewGC *gc)
{
  if (gc->doing_memory_accounting) {
    return OBJPTR_TO_OBJHEAD(p)->size;
  }
  return gc->mark_table[btc_redirect_cust_box](p, gc);
}

int BTC_bi_chan_mark(void *p, struct NewGC *gc)
{
  if (gc->doing_memory_accounting) {
    Scheme_Place_Bi_Channel *bc = (Scheme_Place_Bi_Channel *)p;
    /* Race conditions here on `mem_size', and likely double counting
       when the same async channels are accessible from paired bi
       channels --- but those approximations are ok for accounting. */
    account_memory(gc, gc->current_mark_owner, bc->link->sendch->mem_size, 0);
    account_memory(gc, gc->current_mark_owner, bc->link->recvch->mem_size, 0);
  }
  return gc->mark_table[btc_redirect_bi_chan](p, gc);
}

static void btc_overmem_abort(NewGC *gc)
{
  gc->kill_propagation_loop = 1;
  GCWARN((GCOUTF, "WARNING: Ran out of memory accounting. "
        "Info will be wrong.\n"));
}

static void propagate_accounting_marks(NewGC *gc)
{
  void *p;
  Mark2_Proc *mark_table = gc->mark_table;

  while(pop_ptr(gc, &p) && !gc->kill_propagation_loop) {
    /* GCDEBUG((DEBUGOUTF, "btc_account: popped off page %p:%p, ptr %p\n", page, page->addr, p)); */
    propagate_marks_worker(gc, mark_table, p); 
  }
  if(gc->kill_propagation_loop)
    reset_pointer_stack(gc);
}

inline static void BTC_initialize_mark_table(NewGC *gc) {
  gc->mark_table[scheme_thread_type]    = BTC_thread_mark;
  gc->mark_table[scheme_custodian_type] = BTC_custodian_mark;
  gc->mark_table[gc->ephemeron_tag]     = BTC_ephemeron_mark;
  gc->mark_table[gc->cust_box_tag]      = BTC_cust_box_mark;
  gc->mark_table[scheme_place_bi_channel_type] = BTC_bi_chan_mark;
}

inline static int BTC_get_redirect_tag(NewGC *gc, int tag) {
  if (tag == scheme_thread_type)         { tag = btc_redirect_thread; }
  else if (tag == scheme_custodian_type) { tag = btc_redirect_custodian; }
  else if (tag == gc->ephemeron_tag)     { tag = btc_redirect_ephemeron; }
  else if (tag == gc->cust_box_tag)      { tag = btc_redirect_cust_box; }
  else if (tag == scheme_place_bi_channel_type) { tag = btc_redirect_bi_chan; }
  return tag;
}

static void BTC_do_accounting(NewGC *gc)
{
  const int table_size = gc->owner_table_size;
  OTEntry **owner_table = gc->owner_table;

  if(gc->really_doing_accounting) {
    Scheme_Custodian *cur = owner_table[current_owner(gc, NULL)]->originator, *last, *parent;
    Scheme_Custodian_Reference *box = cur->global_next;
    int i;

    GCDEBUG((DEBUGOUTF, "\nBEGINNING MEMORY ACCOUNTING\n"));
    gc->doing_memory_accounting = 1;
    gc->in_unsafe_allocation_mode = 1;
    gc->unsafe_allocation_abort = btc_overmem_abort;

    /* clear the memory use numbers out */
    for(i = 1; i < table_size; i++)
      if(owner_table[i]) {
        owner_table[i]->memory_use = 0;
#ifdef MZ_USE_PLACES
        if (MASTERGC && MASTERGC->major_places_gc)
          owner_table[i]->master_memory_use = 0;
#endif
      }
    
    /* start with root: */
    while (cur->parent && SCHEME_PTR1_VAL(cur->parent)) {
      cur = SCHEME_PTR1_VAL(cur->parent);
    }

    /* walk forward for the order we want (blame parents instead of children) */
    last = cur;
    while(cur) {
      int owner = custodian_to_owner_set(gc, cur);
      uintptr_t save_count = gc->phantom_count;

      gc->phantom_count = 0;

      gc->current_mark_owner = owner;
      GCDEBUG((DEBUGOUTF,"MARKING THREADS OF OWNER %i (CUST %p)\n", owner, cur));
      gc->kill_propagation_loop = 0;
      mark_threads(gc, owner);
      mark_cust_boxes(gc, cur);
      GCDEBUG((DEBUGOUTF, "Propagating accounting marks\n"));
      propagate_accounting_marks(gc);

      last = cur;
      box = cur->global_next; cur = box ? SCHEME_PTR1_VAL(box) : NULL;

      owner_table = gc->owner_table;
      owner_table[owner]->memory_use = add_no_overflow(owner_table[owner]->memory_use, 
                                                       gcBYTES_TO_WORDS(gc->phantom_count));
      gc->phantom_count = save_count;
    }

    /* walk backward folding totals int parent */
    cur = last;
    while (cur) {
      int owner = custodian_to_owner_set(gc, cur);
      
      box = cur->parent; parent = box ? SCHEME_PTR1_VAL(box) : NULL;
      if (parent) {
        int powner = custodian_to_owner_set(gc, parent);

        owner_table = gc->owner_table;
        owner_table[powner]->memory_use = add_no_overflow(owner_table[powner]->memory_use,
                                                          owner_table[owner]->memory_use);
        owner_table[powner]->master_memory_use += owner_table[owner]->master_memory_use;
      }

      box = cur->global_prev; cur = box ? SCHEME_PTR1_VAL(box) : NULL;
    }

    gc->in_unsafe_allocation_mode = 0;
    gc->doing_memory_accounting = 0;
    gc->old_btc_mark = gc->new_btc_mark;
    gc->new_btc_mark = !gc->new_btc_mark;
  }

  clear_stack_pages(gc);
}

inline static void BTC_add_account_hook(int type,void *c1,void *c2,uintptr_t b)
{
  NewGC *gc = GC_get_GC();
  AccountHook *work;

  if(!gc->really_doing_accounting) {
    if (!gc->avoid_collection) {
      CHECK_PARK_UNUSED(gc);
      gc->park[0] = c1; 
      gc->park[1] = c2;
      gc->really_doing_accounting = 1;
      garbage_collect(gc, 1, 0, NULL);
      c1 = gc->park[0]; gc->park[0] = NULL;
      c2 = gc->park[1]; gc->park[1] = NULL;
    }
  }

  if (type == MZACCT_LIMIT)
    gc->reset_limits = 1;
  if (type == MZACCT_REQUIRE)
    gc->reset_required = 1;

  for(work = gc->hooks; work; work = work->next) {
    if((work->type == type) && (work->c2 == c2) && (work->c1 == c1)) {
      if(type == MZACCT_REQUIRE) {
        if(b > work->amount) work->amount = b;
      } else { /* (type == MZACCT_LIMIT) */
        if(b < work->amount) work->amount = b;
      }
      break;
    } 
  }

  if(!work) {
    work = ofm_malloc(sizeof(AccountHook));
    work->type = type; 
    work->c1 = c1; 
    work->c2 = c2; 
    work->amount = b;

    /* push work onto hooks */
    work->next = gc->hooks;
    gc->hooks = work;
  }
}

inline static void clean_up_account_hooks(NewGC *gc)
{
  AccountHook *work = gc->hooks;
  AccountHook *prev = NULL;

  while(work) {
    if((!work->c1 || marked(gc, work->c1)) && marked(gc, work->c2)) {
      work->c1 = GC_resolve2(work->c1, gc);
      work->c2 = GC_resolve2(work->c2, gc);
      prev = work;
      work = work->next;
    } else {
      /* remove work hook */
      AccountHook *next = work->next;

      if(prev) prev->next = next;
      if(!prev) gc->hooks = next;
      free(work);
      work = next;
    }
  }
}

static uintptr_t custodian_super_require(NewGC *gc, void *c)
{
  int set = ((Scheme_Custodian *)c)->gc_owner_set;
  const int table_size = gc->owner_table_size;
  OTEntry **owner_table = gc->owner_table;

  if (gc->reset_required) {
    int i;
    for(i = 1; i < table_size; i++)
      if (owner_table[i])
        owner_table[i]->required_set = 0;
    gc->reset_required = 0;
  }

  if (!owner_table[set]->required_set) {
    uintptr_t req = 0, r;
    AccountHook *work = gc->hooks;

    while(work) {
      if ((work->type == MZACCT_REQUIRE) && (c == work->c2)) {
        r = work->amount + custodian_super_require(gc, work->c1);
        if (r > req)
          req = r;
      }
      work = work->next;
    }
    owner_table[set]->super_required = req;
    owner_table[set]->required_set = 1;
  }

  return owner_table[set]->super_required;
}

inline static void BTC_run_account_hooks(NewGC *gc)
{
  AccountHook *work = gc->hooks; 
  AccountHook *prev = NULL;

  while (work) {
    if( ((work->type == MZACCT_REQUIRE) && 
          ((gc->used_pages > (gc->max_pages_for_use / 2))
           || ((((gc->max_pages_for_use / 2) - gc->used_pages) * APAGE_SIZE)
               < (work->amount + custodian_super_require(gc, work->c1)))))
        ||
        ((work->type == MZACCT_LIMIT) &&
         (GC_get_memory_use(work->c1) > work->amount))) {
      AccountHook *next = work->next;

      if(prev) prev->next = next;
      if(!prev) gc->hooks = next;
      scheme_schedule_custodian_close(work->c2);
      free(work);
      work = next;
    } else {
      prev = work; 
      work = work->next;
    }
  }
}

static uintptr_t custodian_single_time_limit(NewGC *gc, int set)
{
  OTEntry **owner_table = gc->owner_table;
  const int table_size = gc->owner_table_size;

  if (!set)
    return gc->place_memory_limit;

  if (gc->reset_limits) {
    int i;
    for(i = 1; i < table_size; i++)
      if (owner_table[i])
        owner_table[i]->limit_set = 0;
    gc->reset_limits = 0;
  }

  if (!owner_table[set]->limit_set) {
    /* Check for limits on this custodian or one of its ancestors: */
    uintptr_t limit = gc->place_memory_limit;
    Scheme_Custodian *orig = (Scheme_Custodian *) owner_table[set]->originator, *c;
    AccountHook *work = gc->hooks;

    while(work) {
      if ((work->type == MZACCT_LIMIT) && (work->c1 == work->c2)) {
        c = orig;
        while (1) {
          if (work->c2 == c) {
            if (work->amount < limit)
              limit = work->amount;
            break;
          }
          if (!c->parent)
            break;
          c = (Scheme_Custodian*)SCHEME_PTR1_VAL(c->parent);
          if (!c)
            break;
        }
      }
      work = work->next;
    }
    owner_table[set]->single_time_limit = limit;
    owner_table[set]->limit_set = 1;
  }

  return owner_table[set]->single_time_limit;
}

intptr_t BTC_get_memory_use(NewGC* gc, void *o)
{
  Scheme_Object *arg = (Scheme_Object*)o;
  if(SAME_TYPE(SCHEME_TYPE(arg), scheme_custodian_type)) {
    return custodian_usage(gc, arg);
  }

  return 0;
}

int BTC_single_allocation_limit(NewGC *gc, size_t sizeb) 
/* Use this function to check for allocations that exceed a single-time
 * limit. Otherwise, the limit doesn't work as intended, because
 * a program can allocate a large block that nearly exhausts memory,
 * and then a subsequent allocation can fail. As long as the limit
 * is much smaller than the actual available memory, and as long as
 * GC_out_of_memory protects any user-requested allocation whose size
 * is independent of any existing object, then we can enforce the limit. */
{
  Scheme_Thread *p = scheme_current_thread;
  if (p)
    return (custodian_single_time_limit(gc, thread_get_owner(p)) < sizeb);
  else
    return (gc->place_memory_limit < sizeb);
}

static uintptr_t BTC_get_account_hook(void *c1)
{
  NewGC *gc = GC_get_GC();
  uintptr_t mem;

  if (!gc->really_doing_accounting)
    return 0;

  mem = custodian_single_time_limit(gc, custodian_to_owner_set(gc, c1));
  
  if (mem == (uintptr_t)(intptr_t)-1)
    return 0;

  return mem;
}


static inline void BTC_clean_up(NewGC *gc) {
  clean_up_thread_list(gc);
  clean_up_owner_table(gc);
  clean_up_account_hooks(gc);
}

static inline void BTC_set_btc_mark(NewGC *gc, objhead* info) {
  info->btc_mark = gc->old_btc_mark;
}
#endif
