
/* 
   Provides:
      struct finalizer { ... } Fnl
      GC_set_finalizer
      reset_finalizer_tree
      finalizers
      num_fnls
   Requires:
      is_finalizable_page(gc, p)
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

  gc->splayed_finalizers = fnl_splay((intptr_t)p, gc->splayed_finalizers);
  fnl = gc->splayed_finalizers;
  if (fnl && (fnl->p == p)) {
    if (oldf) *oldf = fnl->f;
    if (olddata) *olddata = fnl->data;
    if (f) {
      fnl->f = f;
      fnl->data = data;
      fnl->eager_level = level;
    } else {
      /* remove finalizer */
      if (fnl->prev)
        fnl->prev->next = fnl->next;
      else
        gc->finalizers = fnl->next;
      if (fnl->next)
        fnl->next->prev = fnl->prev;

      --gc->num_fnls;
      gc->splayed_finalizers = fnl_splay_delete((intptr_t)p, gc->splayed_finalizers);
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

  /* push finalizer */
  fnl->next = gc->finalizers;
  fnl->prev = NULL;
  if (gc->finalizers) {
    gc->finalizers->prev = fnl;
  }
  gc->finalizers = fnl;

  gc->splayed_finalizers = fnl_splay_insert((intptr_t)p, fnl, gc->splayed_finalizers);

  gc->num_fnls++;
}

static void reset_finalizer_tree(GCTYPE *gc)
  /* After a GC, rebuild the splay tree, since object addresses
     have moved. */
{
  Fnl *fnl;
  Fnl *prev = NULL;

  gc->splayed_finalizers = NULL;

  for (fnl = gc->finalizers; fnl; fnl = fnl->next) {
    fnl->prev = prev;
    gc->splayed_finalizers = fnl_splay_insert((intptr_t)fnl->p, fnl, gc->splayed_finalizers);
    prev = fnl;
  }
}

