/*****************************************************************************/
/* Backtrace                                                                 */
/*****************************************************************************/

#if MZ_GC_BACKTRACE

static void backtrace_new_page(struct mpage *page)
{
  /* This is a little wastefull for big pages, because we'll
     only use the first few words: */
  page->backtrace = (void **)malloc_pages(APAGE_SIZE, APAGE_SIZE);
}

static void free_backtrace(struct mpage *page)
{
  free_pages(page->backtrace, APAGE_SIZE);
}

static void *bt_source;
static int bt_type;

static void set_backtrace_source(void *source, int type)
{
  bt_source = source;
  bt_type = type;
}

static void record_backtrace(struct mpage *page, void *ptr)
/* ptr is after objhead */
{
  unsigned long delta;

  delta = PPTR(ptr) - PPTR(page->addr);
  page->backtrace[delta - 1] = bt_source;
  ((long *)page->backtrace)[delta] = bt_type;
}

static void copy_backtrace_source(struct mpage *to_page, void *to_ptr,
				  struct mpage *from_page, void *from_ptr)
/* ptrs are at objhead */
{
  unsigned long to_delta, from_delta;

  to_delta = PPTR(to_ptr) - PPTR(to_page->addr);
  from_delta = PPTR(from_ptr) - PPTR(from_page->addr);

  to_page->backtrace[to_delta] = from_page->backtrace[from_delta];
  to_page->backtrace[to_delta+1] = from_page->backtrace[from_delta+1];
}


static void *get_backtrace(struct mpage *page, void *ptr)
/* ptr is after objhead */
{
  unsigned long delta;

  if (page->big_page)
    ptr = PTR(page->addr + PREFIX_SIZE + WORD_SIZE);

  delta = PPTR(ptr) - PPTR(page->addr);
  return page->backtrace[delta - 1];
}


# define BT_STACK      (PAGE_TYPES + 0)
# define BT_ROOT       (PAGE_TYPES + 1)
# define BT_FINALIZER  (PAGE_TYPES + 2)
# define BT_WEAKLINK   (PAGE_TYPES + 3)
# define BT_IMMOBILE   (PAGE_TYPES + 4)

#else
# define backtrace_new_page(page) /* */
# define free_backtrace(page) /* */
# define set_backtrace_source(ptr, type) /* */
# define record_backtrace(page, ptr) /* */
# define copy_backtrace_source(to_page, to_ptr, from_page, from_ptr) /* */
#endif

