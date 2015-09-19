
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

static void remove_finalizer(Fnl *fnl, int gen0, GCTYPE *gc)
{
  if (fnl->prev)
    fnl->prev->next = fnl->next;
  else {
    if (gen0)
      gc->gen0_finalizers = fnl->next;
    else
      gc->finalizers = fnl->next;
  }
  if (fnl->next)
    fnl->next->prev = fnl->prev;
  
  if (gen0)
    gc->splayed_gen0_finalizers = fnl_splay_delete((intptr_t)fnl->p, gc->splayed_gen0_finalizers);
  else
    gc->splayed_finalizers = fnl_splay_delete((intptr_t)fnl->p, gc->splayed_finalizers);
}

static void add_finalizer(Fnl *fnl, int gen0, GCTYPE *gc)
{
  fnl->next = (gen0 ? gc->gen0_finalizers : gc->finalizers);
  fnl->prev = NULL;
  if (fnl->next)
    fnl->next->prev = fnl;

  if (gen0) {
    gc->gen0_finalizers = fnl;
    gc->splayed_gen0_finalizers = fnl_splay_insert((intptr_t)fnl->p, fnl, gc->splayed_gen0_finalizers);
  } else {
    gc->finalizers = fnl;
    gc->splayed_finalizers = fnl_splay_insert((intptr_t)fnl->p, fnl, gc->splayed_finalizers);
  }
}

void GC_set_finalizer(void *p, int tagged, int level, void (*f)(void *p, void *data), 
    void *data, void (**oldf)(void *p, void *data), 
    void **olddata)
{
  GCTYPE *gc = GC_get_GC();
  Fnl *fnl;

  if (!is_finalizable_page(gc, p)) {
    /* Never collected. Don't finalize it. */
    if (oldf) *oldf = NULL;
    if (olddata) *olddata = NULL;
    return;
  }

  gc->splayed_gen0_finalizers = fnl_splay((intptr_t)p, gc->splayed_gen0_finalizers);
  fnl = gc->splayed_gen0_finalizers;
  if (!fnl || (fnl->p != p)) {
    gc->splayed_finalizers = fnl_splay((intptr_t)p, gc->splayed_finalizers);
    fnl = gc->splayed_finalizers;
    if (!fnl || (fnl->p != p))
      fnl = NULL;
    else {
      /* since we're mutating this finalizer, move it to the gen0 list and tree */
      remove_finalizer(fnl, 0, gc);
      add_finalizer(fnl, 1, gc);
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
      remove_finalizer(fnl, 1, gc);
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

  add_finalizer(fnl, 1, gc);
  gc->num_fnls++;
}

static void merge_finalizer_trees(GCTYPE *gc)
/* For a full GC, move all finalizers to the gen0 list */
{
  Fnl *fnl, *next;

  for (fnl = gc->finalizers; fnl; fnl = next) {
    next = fnl->next;
    add_finalizer(fnl, 1, gc);
  }

  gc->finalizers = NULL;
  gc->splayed_finalizers = NULL;
}

static void reset_finalizer_tree(GCTYPE *gc)
/* After a GC, move gen0 finalizers to the old finalizer list. Note
   that the old gen0 splay tree is otherwise broken, since object
   addresses have moved. */
{
  Fnl *fnl, *next;

  fnl = gc->gen0_finalizers;
  gc->gen0_finalizers = NULL;
  gc->splayed_gen0_finalizers = NULL;

  for (; fnl; fnl = next) {
    next = fnl->next;
    if (is_in_gen_half(fnl, gc)
        || is_in_gen_half(fnl->f, gc)
        || is_in_gen_half(fnl->data, gc))
      add_finalizer(fnl, 1, gc);
    else
      add_finalizer(fnl, 0, gc);
  }

}
