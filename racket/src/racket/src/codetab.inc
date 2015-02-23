
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

/* Represent a node in the tree as an array of
   NODE_HEADER_SIZE + KEY_COUNT pointers.

   The first pointer is always NULL, so that the node can be
   recognized as a node (as opposed to a Racket value) when referenced
   by other nodes.

   The second "pointer" is used as an array of bits, where each bit
   indicates whether the corresponding pointer is the start of a
   mapped range. The bits are shfted up by 1 and the low bit is always
   set to 1, so that the GC doesn't think it's a pointer.

   The third "pointer" is similar to the first, but it represents the
   GC status of the reference for CGC, and the bit is only used at a
   range start.

   Then there are KEY_COUNT "key" pointers for the content of the
   node.  Note that KEY_COUNT is smaller than the number of bits
   available in the header's second and third "pointer"s.

   In a leaf node, every "key" corresponds to an address. In a node
   that references leaf nodes, every "key" corresponds to KEY_SIZE
   addresses. So, at the root, the first key corresonds to address 0,
   the second key corresponds to address (1 << (32-LOG_KEY_SIZE)) on a
   32-bit platform, and so on.

   To make the tree more compact, when all of the pointers in a node
   would be the same non-node value, then the node itself can be
   replaced with the non-node value. (That's why we need to be able to
   distinguish node references from Scheme-object references.) Of
   course, "leaf node" above means nodes that would be leaves without
   this compaction. 

   When PLT_DUMP_JIT_RANGES is defined for compilation and as an
   environment variable at run time, then an exit hook prints the
   ranges of JIT-generated code. Furthermore, for 3m, defining
   PLT_DUMP_JIT_RANGES at compilation time disables GC of
   JIT-generated code. */

THREAD_LOCAL_DECL(static void **codetab_tree);
THREAD_LOCAL_DECL(static int during_set);
#ifdef MZ_USE_PLACES
static void **shared_codetab_tree;
/* The table is shared but not locked for read; We rely on x86-TSO
   and the fact that kentries are only added (never removed)
   to skip the lock. */
static mzrt_mutex *shared_codetab_lock;
#endif

#ifdef PLT_DUMP_JIT_RANGES
static int dump_registered = 0;
static void dump_symbol_ranges();
#endif

static int do_clear_symbols(void **t, uintptr_t start, int offset, uintptr_t addr, int clearing);

static void *do_find_symbol(void **the_tree, uintptr_t v)
{
  uintptr_t k;
  void **t = the_tree, *val;
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

static void *find_symbol(uintptr_t v)
{
  void *r;

  r = do_find_symbol(codetab_tree, v);

#ifdef MZ_USE_PLACES
  if (!r && shared_codetab_tree) {
    r = do_find_symbol(shared_codetab_tree, v);
  }
#endif

  return r;
}

static void **malloc_node(int for_gc_able)
{
  void **v;
  long sz = (KEY_COUNT + NODE_HEADER_SIZE) * sizeof(void *);

#ifdef MZ_USE_PLACES
  if (!for_gc_able) {
    v = (void **)malloc(sz);
    memset(v, 0, sz);
  } else
#endif
    v = (void **)scheme_malloc(sz);
  
  /* Set low bit in each of STARTS and GCABLE so that they're not confused
     for pointers: */
  ((uintptr_t *)v)[NODE_STARTS_OFFSET] = 0x1;
  ((uintptr_t *)v)[NODE_GCABLE_OFFSET] = 0x1;

  return v;
}

void scheme_jit_add_symbol(uintptr_t start, uintptr_t end, void *value, int gc_able)
{
  uintptr_t k1, k2, split_t_start = 0, split_t_end = 0, i;
  int m;
  int offset = (JIT_WORD_SIZE * 8), split_offset = 0;
  void **the_tree, **t1, **t2, **split_t, *val1, *val2;

#ifdef PLT_DUMP_JIT_RANGES
  if (!dump_registered) {
    dump_registered = 1;
    if (getenv("PLT_DUMP_JIT_RANGES"))
      atexit(dump_symbol_ranges);
  }
#endif

#ifdef MZ_USE_PLACES
  if (!gc_able) {
    if (!shared_codetab_lock) {
      /* this function will be called in the main place 
         before others are started, so a lazy lock creation 
         is ok */
      mzrt_mutex_create(&shared_codetab_lock);      
    }
    mzrt_mutex_lock(shared_codetab_lock);
    if (!shared_codetab_tree)
      shared_codetab_tree = malloc_node(0);
    the_tree = shared_codetab_tree;
  } else
#endif
    {
      if (!codetab_tree) {
        REGISTER_SO(codetab_tree);
        codetab_tree = malloc_node(gc_able);
      }
      the_tree = codetab_tree;
    }

  during_set++;
  
  t1 = t2 = the_tree;
  split_t = NULL;
  while (offset) {
    offset -= LOG_KEY_SIZE;

    k1 = ((start >> offset) & KEY_MASK) + NODE_HEADER_SIZE;
    if (offset) {
      val1 = t1[k1];
      if (!val1) {
	val1 = malloc_node(gc_able);
	t1[k1] = val1;
      }
    } else
      val1 = t1;

    k2 = ((end >> offset) & KEY_MASK) + NODE_HEADER_SIZE;
    if (offset) {
      /* Need to go deeper... */
      val2 = t2[k2];
      if (!val2) {
	val2 = malloc_node(gc_able);
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

#ifdef MZ_USE_PLACES
  if (!gc_able) {
    mzrt_mutex_unlock(shared_codetab_lock);      
  }
#endif
}

#ifndef MZ_PRECISE_GC

static int do_clear_symbols(void **t, uintptr_t start, int offset, uintptr_t addr, int clearing)
{
  int i, m, j;
  void *val, **subt;

  /* Note: this function might be called  (via a GC callback)
     while add_symbol is running. */

  for (i = ((start >> offset) & KEY_MASK); i < KEY_COUNT; i++) {
    m = (1 << (i + 1));
    if (((uintptr_t *)t)[NODE_STARTS_OFFSET] & m) {
      clearing = 0;
      if (((uintptr_t *)t)[NODE_GCABLE_OFFSET] & m) {
	/* GCable flag means use GC_is_marked */
	void *p = (void *)(addr + ((uintptr_t)i << offset));
	if (!GC_is_marked(p))
	  clearing = 1;
	if (clearing) {
	  /* Collected... */
	  ((uintptr_t *)t)[NODE_STARTS_OFFSET] -= m;
	  ((uintptr_t *)t)[NODE_GCABLE_OFFSET] -= m;
	}
      }
    }

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
      } else if (clearing)
	t[i + NODE_HEADER_SIZE] = NULL;
    }
  }

  return clearing;
}

static void clear_symbols_for_collected()
{
  if (codetab_tree) {
    do_clear_symbols(codetab_tree, 0, (JIT_WORD_SIZE * 8) - LOG_KEY_SIZE, 0, 0); 
  }
}

#else

static void check_clear_symbols(void **t)
{
  int i, j;
  void *val, **subt;

#ifdef MZ_PRECISE_GC
  if (((uintptr_t *)t)[NODE_GCABLE_OFFSET] != 0x1)
    printf("Found GCed\n");
#endif

  for (i = 0; i < KEY_COUNT; i++) {
    val = t[i + NODE_HEADER_SIZE];

    if (val) {
      if (!*(Scheme_Type *)val) {
	subt = (void **)val;
	check_clear_symbols(subt);
        /* If the branch is empty, then drop it. */
        for (j = 0; j < KEY_COUNT; j++) {
          if (subt[j + NODE_HEADER_SIZE])
            break;
        }
        if (j == KEY_COUNT) {
          t[i+NODE_HEADER_SIZE] = NULL;
          // should_have_cleared();
        }
      }
    }
  }
}

static void clear_symbols_for_collected()
{
  if (!during_set && codetab_tree)
    check_clear_symbols(codetab_tree);
}

#endif

#ifdef PLT_DUMP_JIT_RANGES

static void *dump_symbol_range(void **t, int offset, uintptr_t base, void *val)
{
  int i, m;
  void *p;

  offset -= LOG_KEY_SIZE;

  for (i = 0; i < KEY_COUNT; i++) {
    p = t[i + NODE_HEADER_SIZE];
    if (p) {
      if (*(Scheme_Type *)p) {
        int is_start;

        m = (1 << (i + 1));
        is_start = (((uintptr_t *)t)[NODE_STARTS_OFFSET] & m);

        if (val && ((p != val) || is_start)) {
          printf("%" PRIxPTR "\n", base + (((uintptr_t)i - 1) << offset)); /* end range */
          val = NULL;
        }

        if (is_start) {
          /* start new range */
          val = p;
          printf("%" PRIxPTR "-", base + ((uintptr_t)i << offset));
          if (find_symbol(base + ((uintptr_t)i << offset)) != val)
            printf("[oops]");
        }
      } else {
        val = dump_symbol_range((void **)p, offset, base + ((uintptr_t)i << offset), val);
      }
    } else if (val) {
      printf("%" PRIxPTR "\n", base + (((uintptr_t)i - 1) << offset)); /* end range */
      val = NULL;
    }
  }

  return val;
}

static void dump_symbol_ranges()
{
  (void)dump_symbol_range(codetab_tree, JIT_WORD_SIZE * 8, 0, NULL);

#ifdef MZ_USE_PLACES
  if (shared_codetab_tree)
    (void)dump_symbol_range(shared_codetab_tree, JIT_WORD_SIZE * 8, 0, NULL);
#endif
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
