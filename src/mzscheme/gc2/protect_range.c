/*
  Provides:
  Requires:
   [page_range.c exports]
   [page allocator]
*/

#ifdef _WIN32

/* VirtualProtect can be used only on pages allocated at the same
   time, so we can't collapse ranges. */

# define initialize_protect_page_ranges(pr, b, s) /* */
# define add_protect_page_range(pr, s, l, a, w) vm_protect_pages(s, l, w)
# define flush_protect_page_ranges(pr, w) /* */

#else

static void initialize_protect_page_ranges(Page_Range *protect_range, void *block, unsigned long size)
{
  initialize_page_ranges(protect_range, block, size);
}

static void flush_protect_page_ranges(Page_Range *protect_range, int writeable)
{
  Range *work;

  compact_page_ranges(protect_range);

  for (work = protect_range->range_start; work; work = work->next) {
    vm_protect_pages((void *)work->start, work->len, writeable);
  }

  reset_page_ranges(protect_range);
}

static void add_protect_page_range(Page_Range *protect_range, void *_start, unsigned long len, unsigned long alignment, int writeable)
{
  if (!add_page_range(protect_range, _start, len, alignment)) {
    flush_protect_page_ranges(protect_range, writeable);
    add_page_range(protect_range, _start, len, alignment);
  }
}

#endif
