
/* Extra headers for the GC2 tracing interface */

#ifndef __mzscheme_gc_2_dump__
#define __mzscheme_gc_2_dump__

typedef char *(*GC_get_type_name_proc)(short t);
typedef void (*GC_for_each_found_proc)(void *p);
typedef void (*GC_for_each_struct_proc)(void *p);

typedef void (*GC_print_tagged_value_proc)(const char *prefix, 
					   void *v, uintptr_t diff, int max_w,
					   const char *suffix);
typedef int (*GC_print_traced_filter_proc)(void *p);

GC2_EXTERN void GC_dump_with_traces(int flags,
				    GC_get_type_name_proc get_type_name,
				    GC_for_each_found_proc for_each_found,
				    short min_trace_for_tag, short max_trace_for_tag,
                                    GC_print_traced_filter_proc print_traced_filter,
				    GC_print_tagged_value_proc print_tagged_value,
				    int path_length_limit,
                                    GC_for_each_struct_proc for_each_struct);

GC2_EXTERN void GC_dump_variable_stack(void **var_stack,
                                       intptr_t delta,
                                       void *limit,
                                       void *stack_mem,
                                       GC_get_type_name_proc get_type_name,
                                       GC_print_tagged_value_proc print_tagged_value);

# define GC_DUMP_SHOW_DETAILS     0x1
# define GC_DUMP_SHOW_TRACE       0x2
# define GC_DUMP_SHOW_FINALS      0x4
# define GC_DUMP_SUPPRESS_SUMMARY 0x8

GC2_EXTERN int GC_is_tagged(void *p);
GC2_EXTERN int GC_is_tagged_start(void *p);
GC2_EXTERN void *GC_next_tagged_start(void *p);

#endif
