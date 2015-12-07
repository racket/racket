/* Cooperates with the GC to prune a Scheme_Prefix's variables
   in the case that the prefix is accessible only via closures
   and not all of the variables are used by the closures. This
   special handling is a kind of dependent reference, where the
   prefix itself is not marked, and then a clean-up hook ---
   mark_pruned_prefixes() in "eval.c" --- NULLs out unused
   fields before finally marking the prefix. If the prefix
   is ever marked through some other reference, then 
   mark_pruned_prefixes() doesn't actually prune.
   To support incremental collection, we rely on the fact that
   and old-generation closure cannot point to a new-generation
   prefix; the prefix is always allocated before the closure. */
  if (data && (gc_mode != GC_CURRENT_MODE_BACKPOINTER_REMARK)) {
    /* GLOBAL ASSUMPTION: prefix is at the end of a closure */
    Scheme_Prefix *pf = (Scheme_Prefix *)c->vals[closure_size - 1];

    if (pf) {
      int *use_bits;
      uintptr_t map;
      int mark_stxes;

      /* pf might have been marked via fields: */
      pf = (Scheme_Prefix *)GC_resolve2(pf, gc);
      use_bits = PREFIX_TO_USE_BITS(pf);

      if (!pf->next_final) {
        /* We're the first to look at this prefix... */
        /* Add it to the chain of prefixes to finish after
           all other marking: */
        if ((gc_mode == GC_CURRENT_MODE_INCREMENTAL)
            || (gc_mode == GC_CURRENT_MODE_INCREMENTAL_FINAL)) {
          pf->next_final = scheme_inc_prefix_finalize;
          scheme_inc_prefix_finalize = pf;
        } else {
          pf->next_final = scheme_prefix_finalize;
          scheme_prefix_finalize = pf;
        }
#ifdef MZ_GC_BACKTRACE
        pf->backpointer = (Scheme_Object *)c;
#endif
      }
      mark_stxes = 0;

      /* Add this closure to the chain to be repaired when the
         prefix is marked and potentially moved; if we're here
         in incremental mode, though, the prefix won't be moved: */
      if (gc_mode != GC_CURRENT_MODE_INCREMENTAL) {
        c->vals[closure_size - 1] = pf->fixup_chain;
        pf->fixup_chain = (Scheme_Object *)c;
      } else {
        /* Mark the prefix as reached in incremental mode, which
           triggers special handling for backpointers */
        SCHEME_PREFIX_FLAGS(pf) |= 0x1;
      }

      /* Mark just the elements of the prefix that are (newly) used: */
      if ((uintptr_t)data->tl_map & 0x1) {
        map = (((uintptr_t)data->tl_map) >> 1) & 0x7FFFFFFF;
        if ((use_bits[0] & map) != map) {
          for (i = 0; i < 31; i++) {
            if (map & (1 << i)) {
              if (!(use_bits[0] & (1 << i))) {
                if ((i < pf->num_toplevels) || !pf->num_stxes)
                  gcMARK2(pf->a[i], gc); /* top level */
                else if (i == pf->num_toplevels)
                  mark_stxes = 1; /* any syntax object */
                else
                  gcMARK2(pf->a[i + pf->num_stxes], gc); /* lifted */
              }
            }
          }
          use_bits[0] |= map;
        }
      } else {
        int *u = (int *)GC_resolve2(data->tl_map, gc), j, pos;
      
        for (i = u[0]; i--; ) {
          map = u[i+1];
          if ((use_bits[i] & map) != map) {
            for (j = 0; j < 32; j++) {
              if (map & (1 << j)) {
                if (!(use_bits[i] & (1 << j))) {
                  pos = (i * 32) + j;
                  if ((pos < pf->num_toplevels) || !pf->num_stxes)
                    gcMARK2(pf->a[pos], gc);  /* top level */
                  else if (pos == pf->num_toplevels)
                    mark_stxes = 1;  /* any syntax object */
                  else
                    gcMARK2(pf->a[pos + pf->num_stxes], gc); /* lifted */
                }
              }
            }
            use_bits[i] |= map;
          }
        }
      }
      if (mark_stxes) {
        /* Mark all syntax-object references */
        for (i = pf->num_stxes+1; i--;) {
          gcMARK2(pf->a[i+pf->num_toplevels], gc);
        }
      }
    }
  }
