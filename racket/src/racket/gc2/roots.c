
/* 
   Provides:
      GC_add_roots
      my_qsort
      roots, roots_count
   Requires:
      WORD_SIZE
*/

#define ROOTS_PTR_ALIGNMENT WORD_SIZE
#define ROOTS_PTR_TO_INT(x) ((uintptr_t)x)

static void grow_roots(Roots *roots) {
  uintptr_t *new_roots;

  roots->size = roots->size ? ( 2 * roots->size ) : 500;
  new_roots   = (uintptr_t *)ofm_malloc(sizeof(uintptr_t) * (roots->size + 1));

  memcpy((void *)new_roots, (void *)roots->roots, sizeof(uintptr_t) * roots->count);

  if (roots->roots)
    free(roots->roots);

  roots->roots = new_roots;
}

static int compare_roots(const void *a, const void *b)
{
  if (*(uintptr_t *)a < *(uintptr_t *)b)
    return -1;
  else
    return 1;
}

static void sort_and_merge_roots(Roots *roots)
{
  if (roots->nothing_new)
    return;

  if (roots->count < 4)
    return;

  my_qsort(roots->roots, roots->count >> 1, 2 * sizeof(uintptr_t), compare_roots);

  {
    int i;
    int offset = 0;
    int top = roots->count;
    for (i = 2; i < top; i += 2) {
      int astart = i - 2 - offset;
      int aend   = i - 1 - offset;
      int bstart = i;
      int bend   = i + 1;
      /*|----a----|
       *       |----b----| */
      if (    (roots->roots[astart] <= roots->roots[bstart])
          && ((roots->roots[aend] + (ROOTS_PTR_ALIGNMENT - 1)) >= roots->roots[bstart])) {
        /* merge overlapping roots: */
        if (roots->roots[bend] > roots->roots[aend])
          roots->roots[aend] = roots->roots[bend];
        offset += 2;
        roots->count -= 2;
      } else if (roots->roots[bstart] == roots->roots[bend]) {
        /* Remove empty range: */
        offset += 2;
        roots->count -= 2;
      } else if (offset) {
        /* compact: */
        int emptystart = i - offset;
        int emptyend   = i + 1 - offset;
        roots->roots[emptystart] = roots->roots[bstart];
        roots->roots[emptyend]   = roots->roots[bend];
      }
    }
  }

  roots->nothing_new = 1;
}

void GC_add_roots(void *start, void *end)
{
  GCTYPE *gc = GC_get_GC();
  Roots *roots = &gc->roots;

  if (roots->count >= roots->size) {
    grow_roots(roots);
  }

  roots->roots[roots->count++] = ROOTS_PTR_TO_INT(start);
  roots->roots[roots->count++] = ROOTS_PTR_TO_INT(end) - ROOTS_PTR_ALIGNMENT;
  roots->nothing_new = 0;
}
