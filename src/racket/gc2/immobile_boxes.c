/*****************************************************************************/
/* immobile boxes                                                            */
/*****************************************************************************/
void **GC_malloc_immobile_box(void *p)
{
  GCTYPE *gc = GC_get_GC();
  GC_Immobile_Box *ib = ofm_malloc(sizeof(GC_Immobile_Box));
  if(!ib) GCERR((GCOUTF, "Couldn't allocate space for immobile box!\n"));
  ib->p     = p; 
  ib->next  = gc->immobile_boxes;
  ib->prev  = NULL;
  if(ib->next) ib->next->prev = ib;
  gc->immobile_boxes = ib;
  return (void**)ib;
}

void GC_free_immobile_box(void **b) 
{
  GCTYPE *gc = GC_get_GC();
  GC_Immobile_Box *ib = (GC_Immobile_Box *)b;

#if 0
  /* For debugging: */
  for(ib = gc->immobile_boxes; ib; ib = ib->next)
    if(PPTR(ib) == b) {
      if(ib->prev) ib->prev->next = ib->next;
      if(!ib->prev) gc->immobile_boxes = ib->next;
      if(ib->next) ib->next->prev = ib->prev;
      free(ib);
      return;
    }
  GCWARN((GCOUTF, "Attempted free of non-existent immobile box %p\n", b));
#else
  if(ib->prev) ib->prev->next = ib->next;
  if(!ib->prev) gc->immobile_boxes = ib->next;
  if(ib->next) ib->next->prev = ib->prev;
  free(ib);
#endif
}

#define traverse_immobiles(gcMUCK, set_bt_src) {			  \
    GC_Immobile_Box *ib;                                \
    for(ib = gc->immobile_boxes; ib; ib = ib->next) {		\
      set_bt_src(gc, ib, BT_IMMOBILE);                           \
      gcMUCK(ib->p);                                    \
    }                                                   \
  }

inline static void mark_immobiles(GCTYPE *gc)
{
  traverse_immobiles(gcMARK, set_backtrace_source);
}

inline static void repair_immobiles(GCTYPE *gc)
{
  traverse_immobiles(gcFIXUP, three_arg_no_op);
}
