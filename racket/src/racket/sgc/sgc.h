
# ifdef __cplusplus
extern "C" {
# endif

#include <stddef.h>

#define GC_PTR void*

#if defined(WIN32) || defined(__CYGWIN32__)
# if (defined(SGC_EXPORTS) || (defined(__CYGWIN32__) && !defined(MZ_USES_SHARED_LIB)) \
      || defined(__MINGW32_DELAY_LOAD__))
#  define SGC_EXTERN __declspec(dllexport)
# else
#  define SGC_EXTERN __declspec(dllimport)
# endif
#else
# define SGC_EXTERN extern
#endif

SGC_EXTERN void GC_add_roots(void *start, void *end);

SGC_EXTERN void (*GC_start_collect_callback)(void);
SGC_EXTERN void (*GC_end_collect_callback)(void);
SGC_EXTERN void (*GC_custom_finalize)(void);
SGC_EXTERN void (*GC_out_of_memory)(void);

SGC_EXTERN void *GC_initial_trace_root;
SGC_EXTERN int (*GC_inital_root_skip)(void *, size_t);

SGC_EXTERN void GC_set_stack_base(void *base);
SGC_EXTERN void *GC_get_stack_base(void);

SGC_EXTERN void *GC_base(void *);

SGC_EXTERN void GC_dump(void);

SGC_EXTERN long GC_get_memory_use();

SGC_EXTERN void GC_end_stubborn_change(void *);

SGC_EXTERN void GC_gcollect(void);

SGC_EXTERN void *GC_malloc(size_t size_in_bytes);
SGC_EXTERN void *GC_malloc_atomic(size_t size_in_bytes);
SGC_EXTERN void *GC_malloc_stubborn(size_t size_in_bytes);
SGC_EXTERN void *GC_malloc_uncollectable(size_t size_in_bytes);
SGC_EXTERN void *GC_malloc_atomic_uncollectable(size_t size_in_bytes);
SGC_EXTERN void *GC_malloc_code(size_t size_in_bytes);

typedef void (*GC_collect_start_callback_Proc)(void);
typedef void (*GC_collect_end_callback_Proc)(void);
SGC_EXTERN GC_collect_start_callback_Proc GC_set_collect_start_callback(GC_collect_start_callback_Proc);
SGC_EXTERN GC_collect_end_callback_Proc GC_set_collect_end_callback(GC_collect_end_callback_Proc);

SGC_EXTERN void GC_free(void *); /* ... but only if it's turned on in sgc.c. */

struct GC_Set;
SGC_EXTERN void *GC_malloc_specific(size_t size, struct GC_Set *set);

SGC_EXTERN void GC_general_register_disappearing_link(void **p, void *a);
SGC_EXTERN void GC_register_late_disappearing_link(void **p, void *a);
SGC_EXTERN void GC_unregister_disappearing_link(void **p);

SGC_EXTERN void GC_register_finalizer(void *p, void (*f)(void *p, void *data), 
                                  void *data, void (**oldf)(void *p, void *data), 
                                  void **olddata);
SGC_EXTERN void GC_register_finalizer_ignore_self(void *p, void (*f)(void *p, void *data), 
                                              void *data, void (**oldf)(void *p, void *data), 
                                              void **olddata);
SGC_EXTERN void GC_register_eager_finalizer(void *p, int level, void (*f)(void *p, void *data), 
                                        void *data, void (**oldf)(void *p, void *data), 
                                        void **olddata);

typedef void (*GC_count_tracer)(void *v, int);
typedef void (*GC_path_tracer)(void *v, uintptr_t src, void *pdata);
typedef void (*GC_trace_init)(void);
typedef void (*GC_trace_done)(void);
typedef void (*GC_set_elem_finalizer)(void *p);

#define SGC_ATOMIC_SET 1
#define SGC_UNCOLLECTABLE_SET 2
SGC_EXTERN struct GC_Set *GC_new_set(char *name, 
                                 GC_trace_init init,
                                 GC_trace_init done,
                                 GC_count_tracer count_tracer,
                                 GC_path_tracer path_tracer,
                                 GC_set_elem_finalizer final,
                                 int flags);

SGC_EXTERN struct GC_Set *GC_set(void *d);

SGC_EXTERN void GC_for_each_element(struct GC_Set *set,
                                void (*f)(void *p, int size, void *data),
                                void *data);

SGC_EXTERN int GC_trace_count(int *stack, int *roots, int *uncollectable, int *final);
SGC_EXTERN void GC_trace_path(void);

SGC_EXTERN void GC_store_path(void *v, uintptr_t src, void *path_data);
SGC_EXTERN void **GC_get_next_path(void **prev, int *len);
SGC_EXTERN void GC_clear_paths();

SGC_EXTERN void GC_flush_mark_stack();
SGC_EXTERN void GC_push_all_stack(void *sp, void *ep);
SGC_EXTERN void GC_register_indirect_disappearing_link(void **p, void *a);

SGC_EXTERN void (*GC_push_last_roots)(void);
SGC_EXTERN void (*GC_push_last_roots_again)(void);

SGC_EXTERN int GC_dont_gc;

# ifdef __cplusplus
};
# endif
