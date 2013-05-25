
int GC_is_live(void *p)
{
  NewGC *gc = GC_get_GC();
  mpage *page = pagemap_find_page(gc->page_maps, p);
  if (!page) { /* NOT GC ALLOCATED */
    printf("%p page: %p NOT GC ALLOCATED\n", p, page);
    fflush(stdout);
    return 0; 
  }
  else if (page->generation == 0) {
    if (page == gc->gen0.curr_alloc_page) {
      if (p < (void*) GC_gen0_alloc_page_ptr) {
        printf("GEN0 object %p page: %p gen: %i class: %i ALIVE\n", p, page, page->generation, page->size_class);
        printf("%p BEGIN: %p ALLOCED_UPTO: %p END: %p\n", p, (void*) gc->gen0.curr_alloc_page->addr, (void*) GC_gen0_alloc_page_ptr, (void*) GC_gen0_alloc_page_end);
        fflush(stdout);
        return 1;
      }
      else {
        printf("GEN0 object %p page: %p gen: %i class: %i DEAD\n", p, page, page->generation, page->size_class);
        printf("%p BEGIN: %p ALLOCED_UPTO: %p END: %p\n", p, (void*) gc->gen0.curr_alloc_page->addr, (void*) GC_gen0_alloc_page_ptr, (void*) GC_gen0_alloc_page_end);
        fflush(stdout);
        return 0;
      }
    }
    return NUM(p) < (NUM(page->addr) + page->size);
  }
  else { /* page->generation */
    if (page->size_class == 1) {
      int dead = OBJPTR_TO_OBJHEAD(p)->dead;
      printf("MEDIUM object %p page: %p gen: %i class: %i dead: %i\n", p, page, page->generation, page->size_class, dead);
      fflush(stdout);
      return !dead;
    }
    else if((NUM(page->addr) + page->size) > NUM(p)) {
      printf("%p page: %p gen: %i class: %i ALIVE\n", p, page, page->generation, page->size_class);
      printf("%p BEGIN: %p ALLOCED_UPTO: %p\n", p, (void*) page->addr, (void*) (NUM(page->addr) + page->size));
      fflush(stdout);
      return 1;
    }
    else {
      printf("%p page: %p gen: %i class: %i DEAD\n", p, page, page->generation, page->size_class);
      printf("%p BEGIN: %p ALLOCED_UPTO: %p\n", p, (void*) page->addr, (void*) (NUM(page->addr) + page->size));
      fflush(stdout);
      return 0;
    }
  }
}

void GC_dbg_dump_mpage(mpage *page) {
  printf("mpage:  %p\n", page);
  printf("next:  %p\n", page->next);
  printf("prev:  %p\n", page->prev);
  printf("addr:  %p\n", page->addr);
  printf("previous_size:  %li\n", page->previous_size);
  printf("for med page, points to place to search for available block\n");
  printf("size:  %li\n", page->size);
  printf("generation:  %i\n", page->generation);

  printf("back_pointers: %i\n", page->back_pointers);
  printf("size_class: %i 1 => med; 2 => big; 3 => big marked \n", page->size_class);    ; /* */
  printf("page_type:  %i\n", page->page_type);
  printf("marked_on   %i\n", page->marked_on);
  printf("has_new     %i\n", page->has_new);
  printf("mprotected  %i\n", page->mprotected);
  printf("added       %i\n", page->added);;
  printf("live_size: %i\n", page->live_size);
  printf("backgrace:  %p\n", page->backtrace);
}

int GC_dbg_dump_mpage_for_p(void *p) {
  NewGC *gc = GC_get_GC();
  mpage *page = pagemap_find_page(gc->page_maps, p);
  if (page) {
    GC_dbg_dump_mpage(page);
    return 1;
  }
  else {
    printf("Not allocated by the GC\n");
    return 0;
  }
}

