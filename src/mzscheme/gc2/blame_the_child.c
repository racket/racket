/*****************************************************************************/
/* blame-the-child accounting                                                */
/*****************************************************************************/
#ifdef NEWGC_BTC_ACCOUNT

#define OWNER_TABLE_INIT_AMT 10

struct ot_entry {
  Scheme_Custodian *originator;
  Scheme_Custodian **members;
  unsigned long memory_use;
  unsigned long single_time_limit, super_required;
  char limit_set, required_set;
};

static struct ot_entry **owner_table = NULL;
static unsigned int owner_table_top = 0;

inline static int create_blank_owner_set(void)
{
  int i;
  unsigned int old_top;
  struct ot_entry **naya;

  for (i = 1; i < owner_table_top; i++) {
    if (!owner_table[i]) {
      owner_table[i] = malloc(sizeof(struct ot_entry));
      bzero(owner_table[i], sizeof(struct ot_entry));
      return i;
    }
  }

  old_top = owner_table_top;
  if (!owner_table_top)
    owner_table_top = OWNER_TABLE_INIT_AMT;
  else
    owner_table_top *= 2;

  naya = (struct ot_entry **)malloc(owner_table_top*sizeof(struct ot_entry*));
  memcpy(naya, owner_table, old_top*sizeof(struct ot_entry*));
  owner_table = naya;
  bzero((char*)owner_table + (sizeof(struct ot_entry*) * old_top),
	(owner_table_top - old_top) * sizeof(struct ot_entry*));
  
  return create_blank_owner_set();
}

inline static int custodian_to_owner_set(Scheme_Custodian *cust)
{
  int i;

  if (cust->gc_owner_set)
    return cust->gc_owner_set;

  i = create_blank_owner_set();
  owner_table[i]->originator = cust;
  cust->gc_owner_set = i;

  return i;
}

inline static int current_owner(Scheme_Custodian *c)
{
  if (!scheme_current_thread)
    return 1;
  else if (!c)
    return thread_get_owner(scheme_current_thread);
  else
    return custodian_to_owner_set(c);
}

void GC_register_root_custodian(void *_c)
{
  Scheme_Custodian *c = (Scheme_Custodian *)_c;

  if (owner_table) {
    /* Reset */
    free(owner_table);
    owner_table = NULL;
    owner_table_top = 0;
  }

  if (create_blank_owner_set() != 1) {
    GCPRINT(GCOUTF, "Something extremely weird (and bad) has happened.\n");
    abort();
  }
  
  owner_table[1]->originator = c;
  c->gc_owner_set = 1;
}

inline static int custodian_member_owner_set(void *cust, int set)
{
  Scheme_Custodian_Reference *box;
  Scheme_Custodian *work = owner_table[set]->originator;

  while(work) {
    if(work == cust) return 1;
    box = work->parent;
    work = box ? SCHEME_PTR1_VAL(box) : NULL;
  }
  return 0;
}

inline static void account_memory(int set, long amount)
{
  owner_table[set]->memory_use += amount;
}

inline static void free_owner_set(int set)
{
  if(owner_table[set]) {
    free(owner_table[set]);
  }
  owner_table[set] = NULL;
}

inline static void clean_up_owner_table(void)
{
  int i;

  for(i = 1; i < owner_table_top; i++)
    if(owner_table[i]) {
      /* repair or delete the originator */
      if(!marked(owner_table[i]->originator)) {
	owner_table[i]->originator = NULL;
      } else 
	owner_table[i]->originator = GC_resolve(owner_table[i]->originator);

      /* potential delete */
      if(i != 1) 
	if((owner_table[i]->memory_use == 0) && !owner_table[i]->originator)
	  free_owner_set(i);
    }
}

inline static unsigned long custodian_usage(void *custodian)
{
  unsigned long retval = 0;
  int i;
  
  if(!GC->really_doing_accounting) {
    GC->park[0] = custodian;
    GC->really_doing_accounting = 1;
    garbage_collect(1);
    custodian = GC->park[0]; 
    GC->park[0] = NULL;
  }
  for(i = 1; i < owner_table_top; i++)
    if(owner_table[i] && custodian_member_owner_set(custodian, i)) 
      retval += owner_table[i]->memory_use;
  return gcWORDS_TO_BYTES(retval);
}

inline static void memory_account_mark(struct mpage *page, void *ptr)
{
  GCDEBUG((DEBUGOUTF, "memory_account_mark: %p/%p\n", page, ptr));
  if(page->big_page) {
    struct objhead *info = (struct objhead *)(NUM(page->addr) + PREFIX_SIZE);

    if(info->btc_mark == GC->old_btc_mark) {
      info->btc_mark = GC->new_btc_mark;
      account_memory(GC->current_mark_owner, gcBYTES_TO_WORDS(page->size));
      push_ptr(ptr);
    }
  } else {
    struct objhead *info = (struct objhead *)((char*)ptr - WORD_SIZE);
    
    if(info->btc_mark == GC->old_btc_mark) {
      info->btc_mark = GC->new_btc_mark;
      account_memory(GC->current_mark_owner, info->size);
      push_ptr(ptr);
    }
  }
}

inline static void mark_cust_boxes(Scheme_Custodian *cur)
{
  Scheme_Object *pr, *prev = NULL, *next;
  GC_Weak_Box *wb;

  /* cust boxes is a list of weak boxes to cust boxes */

  pr = cur->cust_boxes;
  while (pr) {
    wb = (GC_Weak_Box *)SCHEME_CAR(pr);
    next = SCHEME_CDR(pr);
    if (wb->val) {
      GC->normal_cust_box_mark(wb->val);
      prev = pr;
    } else {
      if (prev)
        SCHEME_CDR(prev) = next;
      else
        cur->cust_boxes = next;
    }
    pr = next;
  }
  cur->cust_boxes = NULL;
}

int BTC_thread_mark(void *p)
{
  return ((struct objhead *)(NUM(p) - WORD_SIZE))->size;
}

int BTC_custodian_mark(void *p)
{
  if(custodian_to_owner_set(p) == GC->current_mark_owner)
    return GC->normal_custodian_mark(p);
  else
    return ((struct objhead *)(NUM(p) - WORD_SIZE))->size;
}

int BTC_cust_box_mark(void *p)
{
  return ((struct objhead *)(NUM(p) - WORD_SIZE))->size;
}

inline static void mark_normal_obj(struct mpage *page, void *ptr)
{
  switch(page->page_type) {
    case PAGE_TAGGED: {
      /* we do not want to mark the pointers in a thread or custodian 
	 unless the object's owner is the current owner. In the case
	 of threads, we already used it for roots, so we can just
	 ignore them outright. In the case of custodians, we do need
	 to do the check; those differences are handled by replacing
         the mark procedure in mark_table. */
      GC->mark_table[*(unsigned short*)ptr](ptr);
      break;
    }
    case PAGE_ATOMIC: break;
    case PAGE_ARRAY: { 
      struct objhead *info = (struct objhead *)((char*)ptr - WORD_SIZE);
      void **temp = ptr, **end = temp + (info->size - 1);
      
      while(temp < end) gcMARK(*(temp++));
      break;
    };
    case PAGE_TARRAY: {
      struct objhead *info = (struct objhead *)((char*)ptr - WORD_SIZE);
      unsigned short tag = *(unsigned short*)ptr;
      void **temp = ptr, **end = PPTR(info) + (info->size - INSET_WORDS);
      
      while(temp < end) temp += GC->mark_table[tag](temp);
      break;
    }
    case PAGE_XTAGGED: GC_mark_xtagged(ptr); break;
  }
}

inline static void mark_acc_big_page(struct mpage *page)
{
  void **start = PPTR(NUM(page->addr) + PREFIX_SIZE + WORD_SIZE);
  void **end = PPTR(NUM(page->addr) + page->size);

  switch(page->page_type) {
    case PAGE_TAGGED: 
      {
        unsigned short tag = *(unsigned short*)start;
        if((unsigned long)GC->mark_table[tag] < PAGE_TYPES) {
          /* atomic */
        } else
          GC->mark_table[tag](start); break;
      }
    case PAGE_ATOMIC: break;
    case PAGE_ARRAY: while(start < end) gcMARK(*(start++)); break;
    case PAGE_XTAGGED: GC_mark_xtagged(start); break;
    case PAGE_TARRAY: {
      unsigned short tag = *(unsigned short *)start;
      end -= INSET_WORDS;
      while(start < end) start += GC->mark_table[tag](start);
      break;
    }
  }
}


static void btc_overmem_abort()
{
  GC->kill_propagation_loop = 1;
  GCWARN((GCOUTF, "WARNING: Ran out of memory accounting. "
	          "Info will be wrong.\n"));
}

static void propagate_accounting_marks(void)
{
  struct mpage *page;
  void *p;

  while(pop_ptr(&p) && !GC->kill_propagation_loop) {
    page = pagemap_find_page(p);
    set_backtrace_source(p, page->page_type);
    GCDEBUG((DEBUGOUTF, "btc_account: popped off page %p:%p, ptr %p\n", page, page->addr, p));
    if(page->big_page)
      mark_acc_big_page(page);
    else
      mark_normal_obj(page, p);
  }
  if(GC->kill_propagation_loop)
    reset_pointer_stack();
}

static void do_btc_accounting(void)
{
  if(GC->really_doing_accounting) {
    Scheme_Custodian *cur = owner_table[current_owner(NULL)]->originator;
    Scheme_Custodian_Reference *box = cur->global_next;
    int i;

    GCDEBUG((DEBUGOUTF, "\nBEGINNING MEMORY ACCOUNTING\n"));
    GC->doing_memory_accounting = 1;
    GC->in_unsafe_allocation_mode = 1;
    GC->unsafe_allocation_abort = btc_overmem_abort;
    
    if(!GC->normal_thread_mark) {
      GC->normal_thread_mark    = GC->mark_table[scheme_thread_type];
      GC->normal_custodian_mark = GC->mark_table[scheme_custodian_type];
      GC->normal_cust_box_mark  = GC->mark_table[GC->cust_box_tag];
    }

    GC->mark_table[scheme_thread_type]    = BTC_thread_mark;
    GC->mark_table[scheme_custodian_type] = BTC_custodian_mark;
    GC->mark_table[GC->ephemeron_tag]     = BTC_ephemeron_mark;
    GC->mark_table[GC->cust_box_tag]      = BTC_cust_box_mark;

    /* clear the memory use numbers out */
    for(i = 1; i < owner_table_top; i++)
      if(owner_table[i])
	owner_table[i]->memory_use = 0;

    /* the end of the custodian list is where we want to start */
    while(SCHEME_PTR1_VAL(box)) {
      cur = (Scheme_Custodian*)SCHEME_PTR1_VAL(box);
      box = cur->global_next;
    }

    /* walk backwards for the order we want */
    while(cur) {
      int owner = custodian_to_owner_set(cur);
      
      GC->current_mark_owner = owner;
      GCDEBUG((DEBUGOUTF,"MARKING THREADS OF OWNER %i (CUST %p)\n", owner, cur));
      GC->kill_propagation_loop = 0;
      mark_threads(owner);
      mark_cust_boxes(cur);
      GCDEBUG((DEBUGOUTF, "Propagating accounting marks\n"));
      propagate_accounting_marks();
      
      box = cur->global_prev; cur = box ? SCHEME_PTR1_VAL(box) : NULL;
    }
  
    GC->mark_table[scheme_thread_type]    = GC->normal_thread_mark;
    GC->mark_table[scheme_custodian_type] = GC->normal_custodian_mark;
    GC->mark_table[GC->ephemeron_tag]     = mark_ephemeron;
    GC->mark_table[GC->cust_box_tag]      = GC->normal_cust_box_mark;

    GC->in_unsafe_allocation_mode = 0;
    GC->doing_memory_accounting = 0;
    GC->old_btc_mark = GC->new_btc_mark;
    GC->new_btc_mark = !GC->new_btc_mark;
  }

  clear_stack_pages();
}

struct account_hook {
  int type;
  void *c1, *c2;
  unsigned long amount;
  struct account_hook *next;
};

static struct account_hook *hooks = NULL;

inline static void add_account_hook(int type,void *c1,void *c2,unsigned long b)
{
  struct account_hook *work;

  if(!GC->really_doing_accounting) {
    GC->park[0] = c1; GC->park[1] = c2;
    GC->really_doing_accounting = 1;
    garbage_collect(1);
    c1 = GC->park[0]; c2 = GC->park[1];
    GC->park[0] = GC->park[1] = NULL;
  }

  if (type == MZACCT_LIMIT)
    GC->reset_limits = 1;
  if (type == MZACCT_REQUIRE)
    GC->reset_required = 1;

  for(work = hooks; work; work = work->next) {
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
    work = malloc(sizeof(struct account_hook));
    work->type = type; work->c1 = c1; work->c2 = c2; work->amount = b;
    work->next = hooks; hooks = work;
  }
}

inline static void clean_up_account_hooks()
{
  struct account_hook *work = hooks, *prev = NULL;

  while(work) {
    if((!work->c1 || marked(work->c1)) && marked(work->c2)) {
      work->c1 = GC_resolve(work->c1);
      work->c2 = GC_resolve(work->c2);
      prev = work; work = work->next;
    } else {
      struct account_hook *next = work->next;

      if(prev) prev->next = next;
      if(!prev) hooks = next;
      free(work);
      work = next;
    }
  }
}

static unsigned long custodian_super_require(void *c)
{
  int set = ((Scheme_Custodian *)c)->gc_owner_set;

  if (GC->reset_required) {
    int i;
    for(i = 1; i < owner_table_top; i++)
      if (owner_table[i])
        owner_table[i]->required_set = 0;
    GC->reset_required = 0;
  }

  if (!owner_table[set]->required_set) {
    unsigned long req = 0, r;
    struct account_hook *work = hooks;

    while(work) {
      if ((work->type == MZACCT_REQUIRE) && (c == work->c2)) {
        r = work->amount + custodian_super_require(work->c1);
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

inline static void run_account_hooks()
{
  struct account_hook *work = hooks, *prev = NULL;

  while(work) {
    if( ((work->type == MZACCT_REQUIRE) && 
         ((GC->used_pages > (GC->max_pages_for_use / 2))
          || ((((GC->max_pages_for_use / 2) - GC->used_pages) * APAGE_SIZE)
              < (work->amount + custodian_super_require(work->c1)))))
	||
	((work->type == MZACCT_LIMIT) &&
	 (GC_get_memory_use(work->c1) > work->amount))) {
      struct account_hook *next = work->next;

      if(prev) prev->next = next;
      if(!prev) hooks = next;
      scheme_schedule_custodian_close(work->c2);
      free(work);
      work = next;
    } else {
      prev = work; work = work->next;
    }
  }
}

static unsigned long custodian_single_time_limit(int set)
{
  if (!set)
    return (unsigned long)(long)-1;

  if (GC->reset_limits) {
    int i;
    for(i = 1; i < owner_table_top; i++)
      if (owner_table[i])
        owner_table[i]->limit_set = 0;
    GC->reset_limits = 0;
  }

  if (!owner_table[set]->limit_set) {
    /* Check for limits on this custodian or one of its ancestors: */
    unsigned long limit = (unsigned long)(long)-1;
    Scheme_Custodian *orig = owner_table[set]->originator, *c;
    struct account_hook *work = hooks;

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


# define set_account_hook(a,b,c,d) { add_account_hook(a,b,c,d); return 1; }
# define set_btc_mark(x) (((struct objhead *)(x))->btc_mark = GC->old_btc_mark)
#endif

#ifndef NEWGC_BTC_ACCOUNT
# define clean_up_owner_table() /* */
# define do_btc_accounting() /* */
# define doing_memory_accounting 0
# define memory_account_mark(p,o) /* */
# define set_account_hook(a,b,c,d) return 0
# define clean_up_account_hooks() /* */
# define run_account_hooks() /* */
# define custodian_usage(cust) 0
# define set_btc_mark(x) /* */
static unsigned long custodian_single_time_limit(int set)
{
  return (unsigned long)(long)-1;
}
#endif
