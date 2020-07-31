/*
  Provides:
    page_range_initialize
    page_range_add
    page_range_flush
  Requires:
    os_protect_pages
*/

#if defined(_WIN32) || defined(__CYGWIN32__)

/* VirtualProtect can be used only on pages allocated at the same
   time, so we can't collapse ranges. */

# define initialize_protect_page_ranges(pr, b, s) /* */
# define add_protect_page_range(pr, s, l, a, w) vm_protect_pages(s, l, w)
# define flush_protect_page_ranges(pr, w) /* */

#else

static void page_range_compact(Page_Range *pr);
static void page_range_reset(Page_Range *pr);
static void page_range_flush(Page_Range *pr, int writeable);
static int page_range_add_worker(Page_Range *pr, void *_start, uintptr_t len);


static Page_Range *page_range_create()
{
  Page_Range *pr = ofm_malloc_zero(sizeof(Page_Range));
  pr->range_root = NULL;
  pr->range_start = NULL;
  pr->range_alloc_block = ofm_malloc(APAGE_SIZE);
  pr->range_alloc_size = APAGE_SIZE;
  pr->range_alloc_used = 0;
  return pr;
}

static void page_range_free(Page_Range *pr)
{
  if (pr) {
    free(pr->range_alloc_block);
    free(pr);
  }
}

static void page_range_add(Page_Range *pr, void *_start, uintptr_t len, int writeable)
{
  if (!page_range_add_worker(pr, _start, len)) {
    page_range_flush(pr, writeable);
    page_range_add_worker(pr, _start, len);
  }
}


static void page_range_flush(Page_Range *pr, int writeable)
{
  Range *work;

  page_range_compact(pr);

  for (work = pr->range_start; work; work = work->next) {
    os_protect_pages((void *)work->start, work->len, writeable);
  }

  page_range_reset(pr);
}

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

static void page_range_compact(Page_Range *pr)
{
  Range *work, *next;
  uintptr_t start, len;

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

static void page_range_reset(Page_Range *pr)
{
  pr->range_alloc_used = 0;
  pr->range_root = NULL;
  pr->range_start = NULL;
}

static int try_extend(Range *r, uintptr_t start, uintptr_t len)
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

static int page_range_add_worker(Page_Range *pr, void *_start, uintptr_t len)
{
  uintptr_t start = (uintptr_t)_start;
  Range *r, *range_root = pr->range_root;

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
#endif
