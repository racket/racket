  /* setup for mzclpf_post.inc */
  gc_mode = GC_current_mode(gc);
  if ((gc_mode != GC_CURRENT_MODE_ACCOUNTING)
      && c->code) {
    data = (CLOSURE_DATA_TYPE *)GC_resolve2(c->code, gc);
    if (data->tl_map) {
      /* In GC_CURRENT_MODE_BACKPOINTER_REMARK mode, we can
         ignore the prefix, because it must be at least
         as old as the closure.
         In GC_CURRENT_MODE_MINOR, if the prefix is in an
         old collection, then GC_is_marked() will return 1;
         in incremental mode, we'll mark the prefix and
         effectively disable unused-variable clearing. */
      if ((gc_mode == GC_CURRENT_MODE_BACKPOINTER_REMARK)
          || !GC_is_marked2(c->vals[closure_size - 1], gc)) {
        /* don't mark last item, which is a prefix */
        i--;
      } else
        data = NULL;
    } else
      data = NULL;
  } else
    data = NULL;
