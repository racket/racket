
/* 
   Provides:
      struct finalizer { ... } Fnl
      GC_set_finalizer
      reset_finalizer_tree
      finalizers
      num_fnls
   Requires:
      is_finalizable_page(gc, p)
      is_in_gen_half(p, gc)
      park
*/

#define Tree Fnl
#define Splay_Item(t) ((intptr_t)t->p)
#define Set_Splay_Item(t, v) (t)->p = (void *)v
#define splay fnl_splay
#define splay_insert fnl_splay_insert
#define splay_delete fnl_splay_delete
#include "../utils/splay.c"
#undef splay
#undef splay_insert
#undef splay_delete

static void remove_finalizer(Fnl *fnl, int lvl, GCTYPE *gc)
{
  if (fnl->prev)
    fnl->prev->next = fnl->next;
  else
    gc->finalizers[lvl] = fnl->next;
  if (fnl->next)
    fnl->next->prev = fnl->prev;
  
  gc->splayed_finalizers[lvl] = fnl_splay_delete((intptr_t)fnl->p, gc->splayed_finalizers[lvl]);
}

static void add_finalizer(Fnl *fnl, int lvl, GCTYPE *gc)
{
  fnl->next = gc->finalizers[lvl];

  fnl->prev = NULL;
  if (fnl->next)
    fnl->next->prev = fnl;

  gc->finalizers[lvl] = fnl;
  gc->splayed_finalizers[lvl] = fnl_splay_insert((intptr_t)fnl->p, fnl, gc->splayed_finalizers[lvl]);
}

void GC_set_finalizer(void *p, int tagged, int level, void (*f)(void *p, void *data), 
    void *data, void (**oldf)(void *p, void *data), 
    void **olddata)
{
  GCTYPE *gc = GC_get_GC();
  Fnl *fnl;
  int lvl;

  if (!is_finalizable_page(gc, p)) {
    /* Never collected. Don't finalize it. */
    if (oldf) *oldf = NULL;
    if (olddata) *olddata = NULL;
    return;
  }

  for (lvl = 0; lvl < NUM_FNL_LEVELS; lvl++) {
    gc->splayed_finalizers[lvl] = fnl_splay((intptr_t)p, gc->splayed_finalizers[lvl]);
    fnl = gc->splayed_finalizers[lvl];
    if (!fnl || (fnl->p != p)) {
      fnl = NULL;
    } else {
      if (lvl > FNL_LEVEL_GEN_0) {
        /* since we're mutating this finalizer, move it to the gen0 set */
        remove_finalizer(fnl, lvl, gc);
        add_finalizer(fnl, FNL_LEVEL_GEN_0, gc);
      }
      break;
    }
  }
  
  if (fnl && (fnl->p == p)) {
    if (oldf) *oldf = fnl->f;
    if (olddata) *olddata = fnl->data;
    if (f) {
      fnl->f = f;
      fnl->data = data;
      fnl->eager_level = level;
    } else {
      /* remove finalizer */
      remove_finalizer(fnl, FNL_LEVEL_GEN_0, gc);
      --gc->num_fnls;
    }
    return;
  }

  if (oldf) *oldf = NULL;
  if (olddata) *olddata = NULL;

  if (!f)
    return;

  /* Allcation might trigger GC, so we use park: */
  CHECK_PARK_UNUSED(gc);
  gc->park[0] = p;
  gc->park[1] = data;

  fnl = (Fnl *)GC_malloc_atomic(sizeof(Fnl));
  memset(fnl, 0, sizeof(Fnl));

  p = gc->park[0];
  data = gc->park[1];
  gc->park[0] = NULL;
  gc->park[1] = NULL;
  
  fnl->p = p;
  fnl->f = f;
  fnl->data = data;
  fnl->eager_level = level;
  fnl->tagged = tagged;

#if CHECKS
  {
    MPage *m;

    m = find_page(p);

    if (tagged) {
      if ((m->type != MTYPE_TAGGED)
          || (m->type != MTYPE_PAIR)) {
        GCPRINT(GCOUTF, "Not tagged: %lx (%d)\n", 
            (intptr_t)p, m->type);
        CRASH(4);
      }
    }
  }
#endif

  add_finalizer(fnl, FNL_LEVEL_GEN_0, gc);
  gc->num_fnls++;
}

static void merge_finalizer_trees(GCTYPE *gc)
/* For a full GC, move all finalizers to the gen0 list */
{
  Fnl *fnl, *next;
  int lvl;

  for (lvl = FNL_LEVEL_GEN_1; lvl < NUM_FNL_LEVELS; lvl++) {
    for (fnl = gc->finalizers[lvl]; fnl; fnl = next) {
      next = fnl->next;
      add_finalizer(fnl, FNL_LEVEL_GEN_0, gc);
    }
    gc->finalizers[lvl] = NULL;
    gc->splayed_finalizers[lvl] = NULL;
  }
}

static void reset_finalizer_tree(GCTYPE *gc)
/* After a GC, move gen0 finalizers to the old finalizer list. Note
   that the old gen0 splay tree is otherwise broken, since object
   addresses have moved. */
{
  Fnl *fnl, *next;

  if (gc->gc_full) {
    GC_ASSERT(!gc->finalizers[FNL_LEVEL_INC_1]);
    GC_ASSERT(!gc->splayed_finalizers[FNL_LEVEL_INC_1]);
    GC_ASSERT(!gc->finalizers[FNL_LEVEL_INC_2]);
    GC_ASSERT(!gc->splayed_finalizers[FNL_LEVEL_INC_2]);
    if (gc->finished_incremental) {
      fnl = gc->finalizers[FNL_LEVEL_GEN_1];
      for (; fnl; fnl = next) {
        next = fnl->next;
        add_finalizer(fnl, FNL_LEVEL_INC_3, gc);
      }
      gc->finalizers[FNL_LEVEL_GEN_1] = gc->finalizers[FNL_LEVEL_INC_3];
      gc->splayed_finalizers[FNL_LEVEL_GEN_1] = gc->splayed_finalizers[FNL_LEVEL_INC_3];
      gc->finalizers[FNL_LEVEL_INC_3] = NULL;
      gc->splayed_finalizers[FNL_LEVEL_INC_3] = NULL;
    } else {
      GC_ASSERT(!gc->finalizers[FNL_LEVEL_INC_3]);
      GC_ASSERT(!gc->splayed_finalizers[FNL_LEVEL_INC_3]);
    }
  }

  fnl = gc->finalizers[FNL_LEVEL_GEN_0];
  gc->finalizers[FNL_LEVEL_GEN_0] = NULL;
  gc->splayed_finalizers[FNL_LEVEL_GEN_0] = NULL;

  for (; fnl; fnl = next) {
    next = fnl->next;
    /* Checking both `fnl` and `fnl->p` is redundant, since
       `fnl` is always allocated after `fnl->p`, but check
       both just in case the order of allocation somehow
       changes in the future. */
    if (is_in_generation_half(gc, fnl)
        || is_in_generation_half(gc, fnl->f)
        || is_in_generation_half(gc, fnl->p)
        || is_in_generation_half(gc, fnl->data))
      add_finalizer(fnl, FNL_LEVEL_GEN_0, gc);
    else
      add_finalizer(fnl, FNL_LEVEL_GEN_1, gc);
  }
}
