
/* 
   Provides:
    initialize_page_ranges
    flush_page_ranges
    add_page_range
*/

typedef struct Range {
  unsigned long start, len;
  struct Range *left, *right, *prev, *next;
} Range;

#define Tree Range
#define Splay_Item(t) (t)->start
#define Set_Splay_Item(t, v) (t)->start = (v)
#define splay range_splay
#define splay_insert range_splay_insert
#define OMIT_SPLAY_DELETE
#include "../utils/splay.c"
#undef splay
#undef splay_insert
#undef OMIT_SPLAY_DELETE
#undef Tree
#undef Splay_Item
#undef Set_Splay_Item

typedef struct Page_Range {
  Range *range_root, *range_start;
  void *range_alloc_block;
  unsigned long range_alloc_size;
  unsigned long range_alloc_used;
} Page_Range;

static void initialize_page_ranges(Page_Range *pr, void *block, unsigned long size)
{
  pr->range_root = NULL;
  pr->range_start = NULL;
  pr->range_alloc_block = block;
  pr->range_alloc_size = size;
  pr->range_alloc_used = 0;
}

static void compact_page_ranges(Page_Range *pr)
{
  Range *work, *next;
  unsigned long start, len;

  for (work = pr->range_start; work; work = next) {
    next = work->next;

    start = work->start;
    len = work->len;

    /* Collapse adjacent nodes: */
    while (next && (next->start == start + len)) {
      len += next->len;
      next = next->next;
    }

    work->start = start;
    work->len = len;
    work->next = next;
  }
}

static void reset_page_ranges(Page_Range *pr)
{
  pr->range_alloc_used = 0;
  pr->range_root = NULL;
  pr->range_start = NULL;
}

static int try_extend(Range *r, unsigned long start, unsigned long len)
{
  if (!r)
    return 0;

  if (r->start == start + len) {
    r->start = start;
    r->len += len;
    return 1;
  }
  if (r->start + r->len == start) {
    r->len += len;
    return 1;
  }

  return 0;
 }

static int add_page_range(Page_Range *pr, void *_start, unsigned long len, unsigned long alignment)
{
  unsigned long start = (unsigned long)_start;
  Range *r, *range_root = pr->range_root;

  len += (alignment - 1);
  len -= (len & (alignment - 1));

  range_root = range_splay(start, range_root);

  if (range_root) {
    if (try_extend(range_root, start, len)
	|| try_extend(range_root->prev, start, len)
	|| try_extend(range_root->next, start, len)) {
      pr->range_root = range_root;
      return 1;
    }
  }

  r = (Range *)((char *)pr->range_alloc_block + pr->range_alloc_used);
  pr->range_alloc_used += sizeof(Range);
  if (pr->range_alloc_used > pr->range_alloc_size) {
    return 0;
  } else {
    r->len = len;
    if (range_root) {
      if (start < range_root->start) {
	r->next = range_root;
	r->prev = range_root->prev;
	if (r->prev)
	  r->prev->next = r;
	else
	  pr->range_start = r;
	range_root->prev = r;
      } else {
	r->prev = range_root;
	r->next = range_root->next;
	if (r->next)
	  r->next->prev = r;
	range_root->next = r;
      }
      range_root = range_splay_insert(start, r, range_root);
    } else {
      r->prev = r->next = NULL;
      r->left = r->right = NULL;
      range_root = r;
      r->start = start;
      pr->range_start = r;
    }
    pr->range_root = range_root;
    return 1;
  }
}

