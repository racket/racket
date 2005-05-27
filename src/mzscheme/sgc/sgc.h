
# ifdef __cplusplus
extern "C" {
# endif

#include <stddef.h>

#define GC_PTR void*

void GC_add_roots(void *start, void *end);

extern void (*GC_start_collect_callback)(void);
extern void (*GC_end_collect_callback)(void);
extern void (*GC_custom_finalize)(void);
extern void (*GC_out_of_memory)(void);

extern void *GC_initial_trace_root;
extern int (*GC_inital_root_skip)(void *, size_t);

void GC_set_stack_base(void *base);
void *GC_get_stack_base(void);

void *GC_base(void *);

void GC_dump(void);

long GC_get_memory_use();

void GC_end_stubborn_change(void *);

void GC_gcollect(void);

void *GC_malloc(size_t size_in_bytes);
void *GC_malloc_atomic(size_t size_in_bytes);
void *GC_malloc_stubborn(size_t size_in_bytes);
void *GC_malloc_uncollectable(size_t size_in_bytes);
void *GC_malloc_atomic_uncollectable(size_t size_in_bytes);

void GC_free(void *); /* ... but only if it's turned on in sgc.c. */

struct GC_Set;
void *GC_malloc_specific(size_t size, struct GC_Set *set);

void GC_general_register_disappearing_link(void **p, void *a);
void GC_register_late_disappearing_link(void **p, void *a);
void GC_unregister_disappearing_link(void **p);

void GC_register_finalizer(void *p, void (*f)(void *p, void *data), 
			   void *data, void (**oldf)(void *p, void *data), 
			   void **olddata);
void GC_register_finalizer_ignore_self(void *p, void (*f)(void *p, void *data), 
				       void *data, void (**oldf)(void *p, void *data), 
				       void **olddata);
void GC_register_eager_finalizer(void *p, int level, void (*f)(void *p, void *data), 
				 void *data, void (**oldf)(void *p, void *data), 
				 void **olddata);

typedef void (*GC_count_tracer)(void *v, int);
typedef void (*GC_path_tracer)(void *v, unsigned long src, void *pdata);
typedef void (*GC_trace_init)(void);
typedef void (*GC_trace_done)(void);
typedef void (*GC_set_elem_finalizer)(void *p);

#define SGC_ATOMIC_SET 1
#define SGC_UNCOLLECTABLE_SET 2
struct GC_Set *GC_new_set(char *name, 
			  GC_trace_init init,
			  GC_trace_init done,
			  GC_count_tracer count_tracer,
			  GC_path_tracer path_tracer,
			  GC_set_elem_finalizer final,
			  int flags);

struct GC_Set *GC_set(void *d);

void GC_for_each_element(struct GC_Set *set,
			 void (*f)(void *p, int size, void *data),
			 void *data);

int GC_trace_count(int *stack, int *roots, int *uncollectable, int *final);
void GC_trace_path(void);

void GC_store_path(void *v, unsigned long src, void *path_data);
void **GC_get_next_path(void **prev, int *len);
void GC_clear_paths();

# ifdef __cplusplus
};
# endif
