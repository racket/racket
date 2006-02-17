
/* 
   Provides:
      GC_add_roots
      my_qsort
      roots, roots_count
   Requires:
      WORD_SIZE
*/

#define ROOTS_PTR_ALIGNMENT WORD_SIZE
#define ROOTS_PTR_TO_INT(x) ((unsigned long)x)

static long roots_count;
static long roots_size;
static unsigned long *roots;
static int nothing_new = 0;

static int compare_roots(const void *a, const void *b)
{
  if (*(unsigned long *)a < *(unsigned long *)b)
    return -1;
  else
    return 1;
}

static void sort_and_merge_roots()
{
  int i, offset, top;

  if (nothing_new)
    return;

  if (roots_count < 4)
    return;

  my_qsort(roots, roots_count >> 1, 2 * sizeof(unsigned long), compare_roots);
  offset = 0;
  top = roots_count;
  for (i = 2; i < top; i += 2) {
    if ((roots[i - 2 - offset] <= roots[i])
	&& ((roots[i - 1 - offset] + (ROOTS_PTR_ALIGNMENT - 1)) >= roots[i])) {
      /* merge: */
      if (roots[i + 1] > roots[i - 1 - offset])
	roots[i - 1 - offset] = roots[i + 1];
      offset += 2;
      roots_count -= 2;
    } else if (roots[i] == roots[i + 1]) {
      /* Remove empty range: */
      offset += 2;
      roots_count -= 2;
    } else if (offset) {
      /* compact: */
      roots[i - offset] = roots[i];
      roots[i + 1 - offset] = roots[i + 1];
    }
  }

  nothing_new = 1;
}

void GC_add_roots(void *start, void *end)
{
  if (roots_count >= roots_size) {
    unsigned long *naya;

    roots_size = roots_size ? 2 * roots_size : 500;
    naya = (unsigned long *)malloc(sizeof(unsigned long) * (roots_size + 1));

    memcpy((void *)naya, (void *)roots, 
	   sizeof(unsigned long) * roots_count);

    if (roots)
      free(roots);

    roots = naya;
  }

  roots[roots_count++] = ROOTS_PTR_TO_INT(start);
  roots[roots_count++] = ROOTS_PTR_TO_INT(end) - ROOTS_PTR_ALIGNMENT;
  nothing_new = 0;
}
