/* setup for mzclpf_post.inc */
  if (!GC_is_partial(gc) && c->code) {
    data = (CLOSURE_DATA_TYPE *)GC_resolve2(c->code, gc);
    if (data->tl_map) {
      if (!GC_is_marked2(c->vals[closure_size - 1], gc)) {
        /* don't mark last item, which is a prefix */
        i--;
      } else
        data = NULL;
    } else
      data = NULL;
  } else
    data = NULL;
