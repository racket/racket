
/* Slow, but effective, memory debugging for Unix. */
/* Run checkmem() to test memory consistency and get allocated bytes. */

/* Replace GC routines, too? (Don't link with libgc.a.) */
#define GC_too 0

typedef long SPACE;
#define SPACE_VAL 0x77777777
#define CHAR_SPACE_VAL ((unsigned char)0x99)
#define FREED_VAL 0x55555555

static long total_allocated;

typedef struct HEADER {
  struct HEADER *prev;
  struct HEADER *next;
  unsigned long exact_len; /* 0 => free */
  unsigned long len;
  SPACE s;
} HEADER;

HEADER *chain_start, *chain_end;

static long looking_for;

void *malloc(unsigned long len)
{
  long pos, d, exact_len = len;
  HEADER *h;

  total_allocated += exact_len;

  d = len % sizeof(long);
  if (d)
    len += (sizeof(long) - d);

  pos = sbrk(len + sizeof(HEADER) + sizeof(SPACE));

  if (pos == looking_for) {
    /* Found it ... */
    looking_for = 0;
  }

  h = (HEADER *)pos;
  if (chain_end)
    chain_end->next = h;
  else
    chain_start = h;
  h->prev = chain_end;
  h->len = len;
  h->exact_len = exact_len;

  h->s = SPACE_VAL;
  
  *(SPACE *)(pos + sizeof(HEADER) + len) = SPACE_VAL;
  while (exact_len < len) {
    *(unsigned char *)(pos + sizeof(HEADER) + exact_len) = CHAR_SPACE_VAL;
    exact_len++;
  }

  chain_end = h;

  return (void *)(pos + sizeof(HEADER));
}

void *realloc(void *p, unsigned long len)
{
  void *n;
  unsigned long olen;

  olen = ((HEADER *)((long)p - sizeof(HEADER)))->len;
  if (olen > len)
    return p;

  n = malloc(len);

  memcpy(n, p, olen);

  free(p);

  return n;
}

void *calloc(unsigned long len0, unsigned long len)
{
  void *v;

  len *= len0;
  v = malloc(len);
  memset(v, 0, len);
  return v;
}

void free(void *p)
{
  HEADER *h = (HEADER *)(((long)p) - sizeof(HEADER));
  SPACE v;

  if (!p)
    return;

  if (!h->exact_len) {
    printf("%lx: double-free (%lx)\n", h, h->len);
  } else {
    int i;

    if (h->s != SPACE_VAL)
      printf("%lx: bad start for free (%lx)\n", h, h->s);

    if (h->exact_len < h->len) {
      for (i = h->exact_len; i < h->len; i++) {
	v = (*(unsigned char *)((long)h + sizeof(HEADER) + i));
	if (v != CHAR_SPACE_VAL)
	  printf("%lx: bad inexact end for free (%lx)\n", h, v);
      }
    }

    v = (*(SPACE *)((long)h + sizeof(HEADER) + h->len));
    if (v != SPACE_VAL)
      printf("%lx: bad end for free (%lx)\n", h, v);

    for (i = h->len; i; ) {
      i -= sizeof(long);
      (*(long *)((long)h + sizeof(HEADER) + i)) = FREED_VAL;
    }
  }

  total_allocated -= h->exact_len;

  h->exact_len = 0;
}

unsigned long checkmem(void)
{
  HEADER *h, *prev = 0;
  SPACE v;
  unsigned long total_use = 0;

  for (h = chain_start; h; prev = h, h = h->next) {
    if (h->s != SPACE_VAL)
      printf("%lx [after %lx]: bad start (%lx)\n", h, prev, h->s);

    if (h->exact_len) {
      total_use += h->exact_len;

      if (h->exact_len < h->len) {
	int i;
	for (i = h->exact_len; i < h->len; i++) {
	  v = (*(unsigned char *)((long)h + sizeof(HEADER) + i));
	  if (v != CHAR_SPACE_VAL)
	    printf("%lx: bad inexact end (%lx)\n", h, v);
	}
      }
    } else {
      int i;
      for (i = h->len; i; ) {
	i -= sizeof(long);
	v = (*(long *)((long)h + sizeof(HEADER) + i));
	if (v != FREED_VAL)
	  printf("%lx: bad freed val (%lx)\n", h, v);
      }
    }

    v = (*(SPACE *)((long)h + sizeof(HEADER) + h->len));
    if (v != SPACE_VAL)
      printf("%lx: bad end (%lx)\n", h, v);
  }

  return total_use;
}

long get_total(void)
{
  return total_allocated;
}

#if GC_too

void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_out_of_memory)(void);

long GC_dl_entries;
long GC_fo_entries;
void *GC_changing_list_start, *GC_changing_list_current;

void *GC_malloc(unsigned long len)
{
  return malloc(len);
}

void *GC_malloc_atomic(unsigned long len)
{
  return malloc(len);
}

void *GC_malloc_stubborn(unsigned long len)
{
  return malloc(len);
}

void *GC_malloc_uncollectable(unsigned long len)
{
  return malloc(len);
}

void GC_free(void *p)
{
}

void GC_end_stubborn_change(void *p)
{
}

void *GC_base(void *p)
{
  return 0;
}

void GC_general_register_disappearing_link(void)
{
}

void GC_register_finalizer_ignore_self(void)
{
}

void GC_register_finalizer(void)
{
}

void GC_gcollect(void)
{
}

void GC_dump(void)
{
}

long GC_get_heap_size(void)
{
  return 0;
}

void GC_add_roots(void)
{
}

void GC_set_warn_proc(void)
{
}

void *GC_get_stack_base(void)
{
  void *dummy;

  return (void *)(((char *)(&dummy)) + 3000);
}

#endif
