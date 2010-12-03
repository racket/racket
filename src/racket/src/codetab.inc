
/* Implementation of the "symbol table" for mapping code
   pointers to function names. */

#ifndef MZ_PRECISE_GC
# ifdef USE_SENORA_GC
extern void *GC_base(void *d);
#  define GC_is_marked(p) GC_base(p)
# else
extern MZ_DLLIMPORT int GC_is_marked(void *);
# endif
#endif

#define LOG_KEY_SIZE 4
#define KEY_MASK ((1 << LOG_KEY_SIZE) - 1)
#define KEY_COUNT (1 << LOG_KEY_SIZE)

/* In words: */
#define NODE_HEADER_SIZE 3
#define NODE_STARTS_OFFSET 1
#define NODE_GCABLE_OFFSET 2

THREAD_LOCAL_DECL(static void **codetab_tree);
THREAD_LOCAL_DECL(static int during_set);

static int do_clear_symbols(void **t, uintptr_t start, int offset, uintptr_t addr, int clearing);

static void *find_symbol(uintptr_t v)
{
  uintptr_t k;
  void **t = codetab_tree, *val;
  int offset = (JIT_WORD_SIZE * 8);
  
  while (offset) {
    if (!t)
      return NULL;
    offset -= LOG_KEY_SIZE;
    k = ((v >> offset) & KEY_MASK) + NODE_HEADER_SIZE;
    val = t[k];
    if (!val)
      return NULL;
    if (*(Scheme_Type *)val)
      return val;
    t = (void **)val;
  }

  printf("Error: walked off end of tree\n");
  return NULL;
}

static void **malloc_node()
{
  void **v;
  v = (void **)scheme_malloc((KEY_COUNT + NODE_HEADER_SIZE) * sizeof(void *));

  /* Set low bit in each of STARTS and GCABLE so that they're not confused
     for pointers: */
  ((uintptr_t *)v)[NODE_STARTS_OFFSET] = 0x1;
  ((uintptr_t *)v)[NODE_GCABLE_OFFSET] = 0x1;

  return v;
}

static void add_symbol(uintptr_t start, uintptr_t end, void *value, int gc_able)
{
  uintptr_t k1, k2, split_t_start = 0, split_t_end = 0, i;
  int m;
  int offset = (JIT_WORD_SIZE * 8), split_offset = 0;
  void **t1, **t2, **split_t, *val1, *val2;

  if (!codetab_tree) {
    REGISTER_SO(codetab_tree);
    codetab_tree = malloc_node();
  }

  during_set++;
  
  t1 = t2 = codetab_tree;
  split_t = NULL;
  while (offset) {
    offset -= LOG_KEY_SIZE;

    k1 = ((start >> offset) & KEY_MASK) + NODE_HEADER_SIZE;
    if (offset) {
      val1 = t1[k1];
      if (!val1) {
	val1 = malloc_node();
	t1[k1] = val1;
      }
    } else
      val1 = t1;

    k2 = ((end >> offset) & KEY_MASK) + NODE_HEADER_SIZE;
    if (offset) {
      /* Need to go deeper... */
      val2 = t2[k2];
      if (!val2) {
	val2 = malloc_node();
	t2[k2] = val2;
      }
    } else
      val2 = t2;

    if (!split_t && (val1 != val2)) {
      split_t = t1;
      split_t_start = k1;
      split_t_end = k2;
      split_offset = offset;
    }

    t1 = val1;
    t2 = val2;
  }

  if (!split_t) {
    /* assert: t1 == t2 */
    split_t = t1;
    split_t_start = k1;
    split_t_end = k2;
  }

  /* Mark start bit: */
  m = (1 << (k1 - NODE_HEADER_SIZE + 1));
  ((uintptr_t *)t1)[NODE_STARTS_OFFSET] |= m;
#ifndef MZ_PRECISE_GC
  /* GCABLE flag indicates whether to check for GC later */
  if (gc_able)
    ((uintptr_t *)t1)[NODE_GCABLE_OFFSET] |= m;
#else
  /* GCABLE flag indicates whether it's been GCed: */
  if (!value)
    ((uintptr_t *)t1)[NODE_GCABLE_OFFSET] |= m;
#endif

  /* Fill in start and end: */
  t1[k1] = value;
  t2[k2] = value;
  /* Fill in range between branches: */
  for (i = split_t_start + 1; i < split_t_end; i++) {
    split_t[i] = value;
  }
  /* Fill in places to right of start branches: */
  if (t1 != split_t) {
    k1 = ((start >> split_offset) & KEY_MASK) + NODE_HEADER_SIZE;
    t1 = split_t[k1];
    offset = split_offset;
    while (offset) {
      offset -= LOG_KEY_SIZE;
      k1 = ((start >> offset) & KEY_MASK) + NODE_HEADER_SIZE;
      for (i = k1 + 1; i < KEY_COUNT + NODE_HEADER_SIZE; i++) {
	t1[i] = value;
      }
      t1 = t1[k1];
    }
  }
  /* Fill in places to left of end branch: */
  if (t2 != split_t) {
    k2 = ((end >> split_offset) & KEY_MASK) + NODE_HEADER_SIZE;
    t2 = split_t[k2];
    offset = split_offset;
    while (offset) {
      offset -= LOG_KEY_SIZE;
      k2 = ((end >> offset) & KEY_MASK) + NODE_HEADER_SIZE;
      for (i = NODE_HEADER_SIZE; i < k2; i++) {
	t2[i] = value;
      }
      t2 = t2[k2];
    }
  }

  --during_set;

#ifdef MZ_PRECISE_GC
  if (!value) {
    /* Prune empty branches in the tree. Only do this if this
       object is mapped deeply enough in the tree, otherwise
       we end up scanning the whole tree. */
    do_clear_symbols(codetab_tree, start, 0, 0, 0);
  }
#endif
}

static int do_clear_symbols(void **t, uintptr_t start, int offset, uintptr_t addr, int clearing)
  /* If MZ_PRECISE_GC, then offset and addr are not used. */
{
  int i, m, j;
  void *p, *val, **subt;

  /* Note: this function might be called  (via a GC callback)
     while add_symbol is running. */

  for (i = ((start >> offset) & KEY_MASK); i < KEY_COUNT; i++) {
    m = (1 << (i + 1));
    if (((uintptr_t *)t)[NODE_STARTS_OFFSET] & m) {
      clearing = 0;
      if (((uintptr_t *)t)[NODE_GCABLE_OFFSET] & m) {
	/* GCable pointer starts here */
#ifndef MZ_PRECISE_GC
	/* Conservative GC: GCable flag means use GC_is_marked */
	p = (void *)(addr + ((uintptr_t)i << offset));
	if (!GC_is_marked(p))
	  clearing = 1;
#else
	/* Precise GC: GCable flag means it's gone */
	clearing = 1;
#endif
	if (clearing) {
	  /* Collected... */
	  ((uintptr_t *)t)[NODE_STARTS_OFFSET] -= m;
	  ((uintptr_t *)t)[NODE_GCABLE_OFFSET] -= m;
	}
      } else {
#ifdef MZ_PRECISE_GC
	return 0;
#endif
      }
    }

#ifdef MZ_PRECISE_GC
    if (!clearing)
      val = NULL;
    else
#endif
      val = t[i + NODE_HEADER_SIZE];

    if (val) {
      if (!*(Scheme_Type *)val) {
	subt = (void **)val;
	clearing = do_clear_symbols(subt, start,
				    offset - LOG_KEY_SIZE,
				    (addr + ((uintptr_t)i << offset)),
				    clearing);
	if (!during_set) {
	  /* If the branch is empty, then drop it. */
	  for (j = 0; j < KEY_COUNT; j++) {
	    if (subt[j + NODE_HEADER_SIZE])
	      break;
	  }
	  if (j == KEY_COUNT) {
	    t[i + NODE_HEADER_SIZE] = NULL;
	  }
	}
#ifdef MZ_PRECISE_GC
	if (!clearing) {
	  /* Finished clearing the one item, so return. */
	  return 0;
	}
#endif
      } else if (clearing)
	t[i + NODE_HEADER_SIZE] = NULL;
    }
  }

  return clearing;
}

#ifndef MZ_PRECISE_GC

static void clear_symbols_for_collected()
{
  if (codetab_tree) {
    do_clear_symbols(codetab_tree, 0, (JIT_WORD_SIZE * 8) - LOG_KEY_SIZE, 0, 0); 
  }
}

#endif

/*============================================================*/
/*                          testing                           */
/*============================================================*/

#if 0

Scheme_Type a[] = {1};
Scheme_Type b[] = {2};
Scheme_Type c[] = {3};

static char *nameof(void *p)
{
  if (p == a) return "a";
  if (p == b) return "b";
  if (p == c) return "c";
  if (!p) return "NULL";
  return "?";
}

void *alt_gc;
void *gcs[3];

int GC_is_marked(void *p)
{
  if (alt_gc) {
    if (p == alt_gc)
      return 1;
    else
      return 0;
  } else {
    if ((p == gcs[0])
	|| (p == gcs[1])
	|| (p == gcs[2]))
      return 0;
    else
      return 1;
  }
}

void check(int j, int delta, int i, void *expect, uintptr_t addr)
{
  void *got;

  got = find_symbol(addr);

  if (i == 2)
    expect = NULL;

  if (expect != got)
    printf("(%d,%d,%d) Expected %s, found %s at %p\n", 
	   j, delta, i,
	   nameof(expect), nameof(got),
	   addr);
}

int main()
{
  int i, d, delta, j;

  for (j = 0; j < 2; j++) {
    for (d = 0; d < 16; d++) {
      delta = d;
      for (i = 0; i < 3; i++) {
	if (i != 1)
	  check(j, delta, 1, NULL, (delta + 0x12341234));
	if (!i)
	  add_symbol(delta + 0x12341200, delta + 0x12341234, a, 1);
	check(j, delta, i, a, ((delta + 0x12341234)));
	check(j, delta, i, a, ((delta + 0x12341200)));
	check(j, delta, i, a, ((delta + 0x12341201)));
	check(j, delta, i, a, ((delta + 0x12341210)));
	check(j, delta, i, a, ((delta + 0x12341231)));
	check(j, delta, i, a, ((delta + 0x12341200)));

	if (i != 1)
	  check(j, delta, i, NULL, ((delta + 0x12341236)));
	if (!i)
	  add_symbol(delta + 0x12341236, delta + 0x12370000, b, 1);
	check(j, delta, i, a, ((delta + 0x12341234)));
	if (!i)
	  check(j, delta, i, NULL, ((delta + 0x12341235)));
	check(j, delta, i, b, ((delta + 0x12341236)));
	check(j, delta, i, b, ((delta + 0x12370000)));
	check(j, delta, i, NULL, ((delta + 0x12370001)));
	check(j, delta, i, b, ((delta + 0x12351236)));
	check(j, delta, i, b, ((delta + 0x12350000)));
	check(j, delta, i, b, ((delta + 0x12360000)));

	if (!i) {
	  check(j, delta, i, NULL, ((delta + 0x12341235)));
	  add_symbol(delta + 0x12341235, delta + 0x12341235, c, 1);
	}
	check(j, delta, i, a, ((delta + 0x12341234)));
	check(j, delta, i == 2 ? 0 : i, c, ((delta + 0x12341235)));
	check(j, delta, i, b, ((delta + 0x12341236)));

	if (!delta) {
	  if (!i && !j) {
	    check(j, delta, i, NULL, ((0x55556666)));
	    add_symbol(0x55556663, 0x55556669, a, 0); /* Not GCable */
	  }
	}
	check(j, delta, 0, a, ((0x55556666)));
	check(j, delta, 0, a, ((0x55556663)));
	check(j, delta, 0, a, ((0x55556669)));

	if (i == 0) {
	  alt_gc = NULL;
	  gcs[0] = NULL;
	  gcs[1] = NULL;
	  gcs[2] = NULL;
	} else {
	  if (0)
	    alt_gc = (void *)0x55556663;
	  gcs[0] = (void *)(delta + 0x12341200);
	  gcs[1] = (void *)(delta + 0x12341236);
	  if (i == 2)
	    gcs[2] = (void *)(delta + 0x12341235);
	  else
	    gcs[2] = NULL;
	}

	clear_symbols_for_collected();
      }
    }
  }
}

#endif
