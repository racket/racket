
/* 
   This function can be compiled in three modes:
   STACK_TRACE - stack mode: pull off one set of set & end and push
       the contents. An offset is passed into the function; this
       offset is typically used for a second pass on a stack
       range to check for one-past-end pointers
   FOLLOW_INTERIOR - internals mode: trace with internals, but no 
       offset is passed in.
   otherwise - exact mode: don't follow interior pointers; this is
       the fastest mode

   Also #define:
    COLLECT - the name of the function
*/


#ifdef STACK_TRACE
# define OFFSET_ARG int offset
# define ADD_LOCAL_OFFSET(x) (x + offset)
# define IF_OR_WHILE if
# define FOLLOW_INTERIOR
#else
# define OFFSET_ARG /* empty */
# define ADD_LOCAL_OFFSET(x) x
# define IF_OR_WHILE while
#endif

#if MARK_STATS
# define MARK_STATISTIC(x) x
#else
# define MARK_STATISTIC(x)
#endif

#ifdef FOLLOW_INTERIOR
# define ALIGNED(x) 1
#else
# define ALIGNED(x) !(x & (PTR_ALIGNMENT - 1))
#endif

#ifdef STACK_TRACE
# define ALLOW_TAIL_PUSH 0
#else
# define ALLOW_TAIL_PUSH 0
#endif

static void COLLECT(OFFSET_ARG)
{
  SectorPage **local_sector_pagetables;
  unsigned local_low_plausible;
  unsigned local_high_plausible;
  int local_collect_stack_size;
  int local_collect_stack_count;
  unsigned long *local_collect_stack;
  long local_mem_use;

  /* Download globals into locals so they can be put in registers: */
  local_sector_pagetables = sector_pagetables;
  local_low_plausible = low_plausible;
  local_high_plausible = high_plausible;
  local_collect_stack_size = collect_stack_size;
  local_collect_stack_count = collect_stack_count;
  local_collect_stack = collect_stack;
  local_mem_use = mem_use;
  
  IF_OR_WHILE (local_collect_stack_count) {
    unsigned long s, end;
#if KEEP_DETAIL_PATH
    unsigned long source;
#endif

#if CHECK_SKIP_MARK_AT_FIRST
    if (local_collect_stack_count == collect_start_disable_mark_skip) {
      skip_mark_at_first = NULL;
      collect_start_disable_mark_skip = 0;
    }
#endif

#if ALLOW_TRACE_COUNT
    if (local_collect_stack_count == collect_start_tracing) {
      void *tracing_for_object;
      GC_count_tracer count_tracer;
      int size;

      tracing_for_object = (void *)POP_WAIT_TRACE();
      count_tracer = (GC_count_tracer)POP_WAIT_TRACE();
      size = POP_WAIT_TRACE();

      /* Push current trace onto the stack: */
      PUSH_TRACE(collect_end_tracing);
      PUSH_TRACE(collect_trace_count);
      PUSH_TRACE(count_tracer);
      PUSH_TRACE(tracing_for_object);

      collect_trace_count = size;
  
      collect_end_tracing = collect_start_tracing - COLLECT_STACK_FRAME_SIZE;

      collect_start_tracing = POP_WAIT_TRACE();
    }
#endif

# if KEEP_DETAIL_PATH
    source = local_collect_stack[--local_collect_stack_count];
# endif
    end = local_collect_stack[--local_collect_stack_count];
    s = local_collect_stack[--local_collect_stack_count];
    
#if ALLOW_TRACE_PATH
    if (collecting_with_trace_path) {
      PUSH_PATH_ELEM(collect_end_path_elem);
      PUSH_PATH_ELEM(s);
# if KEEP_DETAIL_PATH
      PUSH_PATH_ELEM(source);
# else
      PUSH_PATH_ELEM(0);
# endif
      collect_end_path_elem = local_collect_stack_count;
    }
#endif

    MARK_STATISTIC(num_pairs_stat++);

    while (s < end) {
      void *d = *(void **)INT_TO_PTR(s);
      unsigned long p = ADD_LOCAL_OFFSET(PTR_TO_INT(d));

      MARK_STATISTIC(num_checks_stat++);
#ifdef FOLLOW_INTERIOR
      MARK_STATISTIC(num_interior_checks_stat++);
#endif
      if (p >= local_low_plausible && p < local_high_plausible && ALIGNED(p)) {
	SectorPage *pagetable = local_sector_pagetables[SECTOR_LOOKUP_PAGETABLE(p)];

	MARK_STATISTIC(num_plausibles_stat++);

	if (pagetable) {
	  SectorPage *page = pagetable + SECTOR_LOOKUP_PAGEPOS(p);
	  long kind = page->kind;

	  MARK_STATISTIC(num_pages_stat++);

	  if (kind == sector_kind_block) {
	    /* Found common block: */
	    BlockOfMemory *block = (BlockOfMemory *)INT_TO_PTR(page->start);
	    unsigned long bstart = block->start;

	    MARK_STATISTIC(num_blocks_stat++);

	    if ((p >= bstart) && (p < block->top)) {
	      int size = block->size;
	      int pos = block->positions[(p - bstart) >> LOG_PTR_SIZE];
	      int start = bstart + pos * size;
	    
	      MARK_STATISTIC(num_blockallocs_stat++);

#ifndef FOLLOW_INTERIOR
	      if (p == start)
#endif
#if CHECK_SKIP_MARK_AT_FIRST
		if (!skip_mark_at_first || !skip_mark_at_first((void *)p, size))
#endif
		  {
		    int bpos;
		    unsigned char bit;
#if DISTINGUISH_FREE_FROM_UNMARKED
		    unsigned char fbit;
#endif
		    unsigned char freebyte;
	
		    MARK_STATISTIC(num_blockaligns_stat++);

		    bpos = POS_TO_UNMARK_INDEX(pos);
		    bit = POS_TO_UNMARK_BIT(pos);
#if DISTINGUISH_FREE_FROM_UNMARKED
		    fbit = POS_TO_FREE_BIT(pos);
#endif

		    freebyte = block->free[bpos];
		
		    if (NOT_MARKED(freebyte & bit) && _NOT_FREE(freebyte & fbit)) {
		      MARK_STATISTIC(num_blockmarks_stat++);
#if ALLOW_TRACE_COUNT
		      if (collecting_with_trace_count) {
			GC_count_tracer count_tracer;
			if ((count_tracer = common_sets[block->set_no]->count_tracer)) {
			  void *o;
#ifdef FOLLOW_INTERIOR
			  p = start;
#endif
			  o = INT_TO_PTR(p);
			  if (block->atomic) {
			    void *s = o;
#if PAD_BOUNDARY_BYTES
			    s = PAD_FORWARD(s);
#endif
			    count_tracer(s, size); 
			    mem_traced += size;
			  } else {
			    /* Push new trace onto the stack: */
			    PUSH_WAIT_TRACE(collect_start_tracing);
			    PUSH_WAIT_TRACE(size);
			    PUSH_WAIT_TRACE(count_tracer);
			    PUSH_WAIT_TRACE(o);
			    collect_start_tracing = local_collect_stack_count + COLLECT_STACK_FRAME_SIZE;
			  }
			} else
			  collect_trace_count += size;
		      }
#endif
#if ALLOW_TRACE_PATH
		      if (collecting_with_trace_path) {
			GC_path_tracer path_tracer;
			if ((path_tracer = common_sets[block->set_no]->path_tracer)) {
			  void *o;
#ifdef FOLLOW_INTERIOR
			  p = start;
#endif
			  o = INT_TO_PTR(p);
# if PAD_BOUNDARY_BYTES
			  o = PAD_FORWARD(o);
# endif
			  path_tracer(o, s, &collect_trace_path_stack);
			}
		      }
#endif
		
#if PRINT && 0
# ifdef FOLLOW_INTERIOR
		      if (diff % size)
			FPRINTF(STDERR,
				"inexact block: %d for %lx[%d], %d=(%d, %d) {%d} %lx -> %lx\n", 
				diff % size, block, size, pos, bpos, bit,
				freebyte, p, start);
# endif
#endif
		  
		      block->free[bpos] = freebyte ^ bit;
		  
		      local_mem_use += size;
		  
		      if (!block->atomic) {
			MARK_STATISTIC(num_blockpushes_stat++);
#ifdef FOLLOW_INTERIOR
			p = start;
#endif
#if ALLOW_TAIL_PUSH
			if (s + PTR_ALIGNMENT >= end) {
			  MARK_STATISTIC(num_blockpushes_tail_stat++);
			  s = p - PTR_ALIGNMENT;
			  end = p + size;
			} else 
#endif
			  LOCAL_PUSH_COLLECT(p, p + size, s);
		      }
		
#if STAMP_AND_REMEMBER_SOURCE
		      if (!block->low_marker || (s < block->low_marker))
			block->low_marker = s;
		      if (!block->high_marker || (s > block->high_marker))
			block->high_marker = s;
#endif
		    }
		  }
	    }
	  } else if (kind == sector_kind_chunk) {
	    MemoryChunk *c = (MemoryChunk *)INT_TO_PTR(page->start);
	    
	    MARK_STATISTIC(num_chunks_stat++);

	    if (((p == c->start) 
#ifdef FOLLOW_INTERIOR
		 || ((p > c->start) && (p < c->end))
#endif
		 )
		&& !c->marked) {
	      MARK_STATISTIC(num_chunkmarks_stat++);
#if ALLOW_TRACE_COUNT
	      if (collecting_with_trace_count) {
		GC_count_tracer count_tracer;
		int size = (c->end - c->start);
		if ((count_tracer = common_sets[c->set_no]->count_tracer)) {
		  void *o;
		  o = INT_TO_PTR(c->start);
		  if (c->atomic) {
		    void *s = o;
#if PAD_BOUNDARY_BYTES
		    s = PAD_FORWARD(s);
#endif
		    count_tracer(s, size); 
		    mem_traced += size;
		  } else {
		    /* Push new trace onto the stack: */
		    PUSH_WAIT_TRACE(collect_start_tracing);
		    PUSH_WAIT_TRACE(size);
		    PUSH_WAIT_TRACE(count_tracer);
		    PUSH_WAIT_TRACE(o);
		    collect_start_tracing = local_collect_stack_count + COLLECT_STACK_FRAME_SIZE;
		  }
		} else
		  collect_trace_count += size;
	      }
#endif
#if ALLOW_TRACE_PATH
	      if (collecting_with_trace_path) {
		GC_path_tracer path_tracer;
		if ((path_tracer = common_sets[c->set_no]->path_tracer)) {
		  void *o;
		  o = INT_TO_PTR(c->start);
#if PAD_BOUNDARY_BYTES
		  o = PAD_FORWARD(o);
#endif
		  path_tracer(o, s, &collect_trace_path_stack);
		}
	      }
#endif

#if PRINT && 0
# ifdef FOLLOW_INTERIOR
	      if (p != c->start)
		FPRINTF(STDERR, "inexact chunk: %lx != %lx\n", p, c->start);
# endif
#endif
#if PRINT && 0
	      FPRINTF(STDERR,
		      "push %ld (%ld) from %ld\n",
		      p, (c->end - c->start), s);
#endif
	      c->marked = 1;
	      local_mem_use += (c->end - c->start);
	      if (!c->atomic) {
		LOCAL_PUSH_COLLECT(c->start, c->end, s);
	      }
#if STAMP_AND_REMEMBER_SOURCE
	      c->marker = s;
#endif
	    }
	  }
	}
      }
      s += PTR_ALIGNMENT;
    }

#if ALLOW_TRACE_COUNT
    while (local_collect_stack_count == collect_end_tracing) {
      void *tracing_for_object, *s;
      GC_count_tracer count_tracer;
      
      tracing_for_object = (void *)POP_TRACE();
      count_tracer = (GC_count_tracer)POP_TRACE();

      s = tracing_for_object;
#if PAD_BOUNDARY_BYTES
      s = PAD_FORWARD(s);
#endif
      count_tracer(s, collect_trace_count);
      mem_traced += collect_trace_count;

      collect_trace_count = POP_TRACE();
      collect_end_tracing = POP_TRACE();
    }
#endif
#if ALLOW_TRACE_PATH
    if (collecting_with_trace_path) {
      while (PATH_ELEM_STACK_NONEMPTY() && (local_collect_stack_count == collect_end_path_elem)) {
	(void)POP_PATH_ELEM(); /* source */
	(void)POP_PATH_ELEM(); /* obj */
	collect_end_path_elem = POP_PATH_ELEM();
      }
    }
#endif
  }

  /* Upload back into globals: */
  collect_stack_size = local_collect_stack_size;
  collect_stack_count =local_collect_stack_count;
  collect_stack = local_collect_stack;
  mem_use = local_mem_use;

#if ALLOW_TRACE_COUNT && CHECK
# ifndef STACK_TRACE
  if (collect_trace_stack.count)
    FPRINTF(STDERR, "BOO-BOO: trace stack not emty: %d\n", collect_trace_stack.count);
  if (collect_wait_trace_stack.count)
    FPRINTF(STDERR, "BOO-BOO: wait trace stack not emty: %d\n", collect_wait_trace_stack.count);
# endif
#endif
}

#undef ALIGNED
#undef OFFSET_ARG
#undef ADD_LOCAL_OFFSET
#undef IF_OR_WHILE
#undef STACK_TRACE
#undef FOLLOW_INTERIOR
#undef ALLOW_TAIL_PUSH
#undef COLLECT
