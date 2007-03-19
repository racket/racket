/* 
   Provides:
      reset_object_traces
      register_traced_object
      print_traced_objects
      print_out_pointer
   Requires:
      avoid_collection
      trace_page_t
      find_page 
      trace_page_type
       TRACE_PAGE_TAGGED
       TRACE_PAGE_ARRAY
       TRACE_PAGE_TAGGED_ARRAY
       TRACE_PAGE_ATOMIC
       TRACE_PAGE_XTAGGED
       TRACE_PAGE_MALLOCFREE
       TRACE_PAGE_BAD
      trace_page_is_big
      trace_backpointer
*/


# define MAX_FOUND_OBJECTS 5000
static int found_object_count;
static void *found_objects[MAX_FOUND_OBJECTS];

static void reset_object_traces()
{
  found_object_count = 0;
}

static void register_traced_object(void *p)
{
  if (found_object_count < MAX_FOUND_OBJECTS) {
    found_objects[found_object_count++] = p;
  }
}

static void *print_out_pointer(const char *prefix, void *p,
			       GC_get_type_name_proc get_type_name,
			       GC_get_xtagged_name_proc get_xtagged_name,
			       GC_print_tagged_value_proc print_tagged_value)
{
  trace_page_t *page;
  const char *what;

  page = find_page(p);
  if (!page || (trace_page_type(page) == TRACE_PAGE_BAD)) {
    GCPRINT(GCOUTF, "%s??? %p\n", prefix, p);
    return NULL;
  }
  p = trace_pointer_start(page, p);

  if (trace_page_type(page) == TRACE_PAGE_TAGGED) {
    Type_Tag tag;
    tag = *(Type_Tag *)p;
    if ((tag >= 0) && get_type_name && get_type_name(tag)) {
      print_tagged_value(prefix, p, 0, 0, 1000, "\n");
    } else {
      GCPRINT(GCOUTF, "%s<#%d> %p\n", prefix, tag, p);
    }
    what = NULL;
  } else if (trace_page_type(page) == TRACE_PAGE_ARRAY) {
    what = "ARRAY";
  } else if (trace_page_type(page) == TRACE_PAGE_TAGGED_ARRAY) {
    what = "TARRAY";
  } else if (trace_page_type(page) == TRACE_PAGE_ATOMIC) {
    what = "ATOMIC";
  } else if (trace_page_type(page) == TRACE_PAGE_XTAGGED) {
    if (get_xtagged_name)
      what = get_xtagged_name(p);
    else
      what = "XTAGGED";
  } else if (trace_page_type(page) == TRACE_PAGE_MALLOCFREE) {
    what = "MALLOCED";
  } else {
    what = "?!?";
  }

  if (what) {
    GCPRINT(GCOUTF, "%s%s%s %p\n", 
	    prefix, what, 
	    (trace_page_is_big(page) ? "b" : ""),
	    p);
  }

  return trace_backpointer(page, p);
}

static void print_traced_objects(int path_length_limit,
				 GC_get_type_name_proc get_type_name,
				 GC_get_xtagged_name_proc get_xtagged_name,
				 GC_print_tagged_value_proc print_tagged_value)
{
  int i;
  avoid_collection++;
  GCPRINT(GCOUTF, "Begin Trace\n");
  for (i = 0; i < found_object_count; i++) {
    void *p;
    int limit = path_length_limit;
    p = found_objects[i];
    p = print_out_pointer("==* ", p, get_type_name, get_xtagged_name, print_tagged_value);
    while (p && limit) {
      p = print_out_pointer(" <- ", p, get_type_name, get_xtagged_name, print_tagged_value);
      limit--;
    }
  }
  GCPRINT(GCOUTF, "End Trace\n");
  --avoid_collection;
}
