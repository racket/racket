/* Cooperates with the GC to prune a Scheme_Prefix's variables
   in the case that the prefix is accessible only via closures
   and not all of the variables are used by the closures. This
   special handling is a kind of dependent reference, where the
   prefix itself is not marked, and then a clean-up hook ---
   mark_pruned_prefixes() in "eval.c" --- NULLs out unused
   fields before finally marking the prefix. If the prefix
   is ever marked through some other reference, then 
   mark_pruned_prefixes() doesn't actually prune. */
  if (data) {
    /* GLOBAL ASSUMPTION: prefix is at the end of a closure */
    Scheme_Prefix *pf = (Scheme_Prefix *)c->vals[closure_size - 1];
    
    if (pf) {
      /* Since pf hasn't been marked, we don't need a GC_resolve(): */
      int *use_bits = PREFIX_TO_USE_BITS(pf);
      uintptr_t map;

      if (!pf->next_final) {
        /* We're the first to look at this prefix... */
        if (pf->num_stxes) {
          /* Mark all syntax-object references */
          for (i = pf->num_stxes+1; i--;) {
            gcMARK2(pf->a[i+pf->num_toplevels], gc);
          }
        }
        /* Add it to the chain of prefixes to finish after
           all other marking: */
        pf->next_final = scheme_prefix_finalize;
        scheme_prefix_finalize = pf;
      }

      /* Mark just the elements of the prefix that are (newly) used: */
      if ((uintptr_t)data->tl_map & 0x1) {
        map = ((uintptr_t)data->tl_map) >> 1;
        for (i = 0; i < 31; i++) {
          if (map & (1 << i)) {
            if (!(use_bits[0] & (1 << i))) {
              if ((i < pf->num_toplevels) || !pf->num_stxes)
                gcMARK2(pf->a[i], gc);
              else
                gcMARK2(pf->a[i + pf->num_stxes + 1], gc);
            }
          }
        }
        use_bits[0] |= (map & 0x7FFFFFFF);
      } else {
        int *u = (int *)GC_resolve2(data->tl_map, gc), j, pos;
      
        for (i = u[0]; i--; ) {
          map = u[i+1];
          for (j = 0; j < 32; j++) {
            if (map & (1 << j)) {
              if (!(use_bits[i] & (1 << j))) {
                pos = (i * 32) + j;
                if ((pos < pf->num_toplevels) || !pf->num_stxes)
                  gcMARK2(pf->a[pos], gc);
                else
                  gcMARK2(pf->a[pos + pf->num_stxes + 1], gc);
              }
            }
          }
          use_bits[i] |= map;
        }
      }
    }
  }
