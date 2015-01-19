#ifndef __mzscheme_gc_2__
#define __mzscheme_gc_2__

#ifndef GC2_JUST_MACROS
# ifdef INCLUDE_WITHOUT_PATHS
#  include "schthread.h"
# else
#  include "../include/schthread.h"
# endif
#endif

# ifdef __GNUC__
#  define MAYBE_UNUSED __attribute__((unused))
# else
#  define MAYBE_UNUSED
# endif

/***************************************************************************/
/***   See README for a general overview of the interface architecture.  ***/
/***************************************************************************/

#ifndef GC2_JUST_MACROS

struct NewGC;

typedef int (*Size_Proc)(void *obj);
typedef int (*Size2_Proc)(void *obj, struct NewGC *);
typedef int (*Mark_Proc)(void *obj);
typedef int (*Mark2_Proc)(void *obj, struct NewGC *);
typedef int (*Fixup_Proc)(void *obj);
typedef int (*Fixup2_Proc)(void *obj, struct NewGC *);
typedef void (*GC_collect_start_callback_Proc)(void);
typedef void (*GC_collect_end_callback_Proc)(void);
typedef void (*GC_collect_inform_callback_Proc)(int master_gc, int major_gc, 
                                                intptr_t pre_used, intptr_t post_used,
                                                intptr_t pre_admin, intptr_t post_admin,
                                                intptr_t post_child_places_used);
typedef uintptr_t (*GC_get_thread_stack_base_Proc)(void);
typedef void (*GC_Post_Propagate_Hook_Proc)(struct NewGC *);
/* 
   Types of the traversal procs (supplied by Racket); see overview in README
   for information about traversals. The return value is the size of
   the object in words. */

# ifdef GC2_JUST_MACROS_AND_TYPEDEFS
#  define GC2_JUST_MACROS
# endif

#endif

#ifndef GC2_JUST_MACROS

#include <stddef.h>

#ifndef GC2_EXTERN
# ifdef GC2_AS_EXPORT
#  define GC2_EXTERN __declspec(dllexport)
# endif
# ifdef GC2_AS_IMPORT
#  define GC2_EXTERN __declspec(dllimport)
# endif
# ifndef GC2_EXTERN
#  define GC2_EXTERN extern
# endif
#endif

# ifdef __cplusplus
extern "C" {
# endif

/***************************************************************************/
/* Administration                                                          */
/***************************************************************************/

GC2_EXTERN void GC_set_get_thread_stack_base(uintptr_t (*)(void));
/* 
   Sets callback called by GC to get the base for stack traversal in the current
   thread (see README). The returned address must not be in the middle
   of a variable-stack record. */

GC2_EXTERN void GC_set_stack_base(void *base);
GC2_EXTERN uintptr_t GC_get_stack_base(void);
/*
   Called by Racket to set/get value used for stack base when
   GC_get_thread_stack_base is null. This is mainly useful for getting
   Racket started, before it has multiple threads. */

GC2_EXTERN void GC_add_roots(void *start, void *end);
/*
   Called by Racket to install roots. The memory between
   `start' (inclusive) and `end' (exclusive) contains pointers. */

GC2_EXTERN void GC_init_type_tags(int count, int pair, int mutable_pair, int weakbox, 
                                  int ephemeron, int weakarray, int custbox,
                                  int phantom);
/*
   Called by Racket to indicate the number of different type tags it
   uses, starting from 0. `count' is always less than 256. The weakbox
   argument is the value to be used for tagging weak box, the
   ephemeron is the value to tagging an ephemeron, etc. (The GC has some
   freedom in the layout of a weak box or ephemeron, so it performs weak
   box traversals itself, but Racket gets to choose the tag.) */

GC2_EXTERN void GC_register_root_custodian(void *);
/*
   Registers the root custodian. */

GC2_EXTERN void GC_register_new_thread(void *, void *);
/*
   Indicates that a just-allocated point is for a thread 
   or place owned by a particular custodian. */

GC2_EXTERN void GC_register_thread(void *, void *);
/*
   Indicates that a a thread or place is now owned by a
   particular custodian. */

GC2_EXTERN GC_collect_start_callback_Proc GC_set_collect_start_callback(GC_collect_start_callback_Proc);
GC2_EXTERN GC_collect_end_callback_Proc GC_set_collect_end_callback(GC_collect_end_callback_Proc);
GC2_EXTERN void GC_set_collect_inform_callback(GC_collect_inform_callback_Proc);
GC2_EXTERN void GC_set_post_propagate_hook(GC_Post_Propagate_Hook_Proc);
/*
   Sets callbacks called by GC before/after performing a collection.  Used by
   Racket to zero out some data and record collection times. The end
   procedure should be called before finalizations are performed. */

GC2_EXTERN void (*GC_out_of_memory)(void);
/*
   Called by GC when it can't satify a memory request. GC_out_of_memory()
   might perform a longjmp. */

GC2_EXTERN void (*GC_report_out_of_memory)(void);
/*
   Called by GC when it has to give up, maybe due to running out of memory
   during a collection. */

GC2_EXTERN void GC_dump(void);
/*
   Dumps memory state info to stderr. */

GC2_EXTERN intptr_t GC_get_memory_use(void *c);
/*
   Returns the number of currently-allocated bytes (speficilly for
   custodian c, as much as the GC's accounting makes possible). */

GC2_EXTERN int GC_accouting_enabled();
/* 
   Reports whether memory accounting is enabled. */

#define MZACCT_REQUIRE		0
#define MZACCT_LIMIT		1
GC2_EXTERN int GC_set_account_hook(int type, void *c1, uintptr_t b, void *c2);
/*
  Set a memory-accounting property. Returns 0 for failure (i.e., not
  supported). */

GC2_EXTERN uintptr_t GC_get_account_memory_limit(void *c1);
/*
  Returns a moemory accounting limit for c1 (or any ancestor),
  or 0 if none is set. */

GC2_EXTERN void GC_gcollect(void);
GC2_EXTERN void GC_gcollect_minor(void);
/*
   Performs an immediate [full] collection. */

GC2_EXTERN void GC_enable_collection(int on);
/*
   Performs an immediate (full) collection. */

GC2_EXTERN void GC_free_all(void);
/*
   Releases all memory, removes all signal handlers, etc.
   This is mainly useful for unloading a DLL within an embedding
   program tht will keep running. */

/***************************************************************************/
/* Allocation                                                              */
/***************************************************************************/

GC2_EXTERN void *GC_malloc(size_t size_in_bytes);
/*
   Alloc an array of pointers, initially zeroed. */

GC2_EXTERN void *GC_malloc_one_tagged(size_t);
/* 
   Alloc a tagged item, initially zeroed.  Racket sets the tag
   before a collection. */

GC2_EXTERN void *GC_malloc_one_small_tagged(size_t);
/* 
   Like GC_malloc_one_tagged, but the size must be less than 1kb,
   it must not be zero, and it must be a multiple of the word size. */

GC2_EXTERN void *GC_malloc_one_small_dirty_tagged(size_t);
/* 
   Like GC_malloc_one_small_tagged, but the memory is not
   zeroed. The client must set all words in the allocated
   object before a GC can occur. */

GC2_EXTERN void *GC_malloc_pair(void *car, void *cdr);
/* 
   Like GC_malloc_one_tagged, but even more streamlined.
   The main potential advantage is that `car' and `cdr' don't
   have to be retained by the callee in the case of a GC. */

GC2_EXTERN void *GC_malloc_array_tagged(size_t);
/* 
   Alloc an array of tagged items. Racket sets the tag in the first
   item before a collection, by maybe not all items. When traversing,
   use the first one for size. */

GC2_EXTERN void *GC_malloc_atomic(size_t size_in_bytes);
/*
   Alloc pointerless memory (not necessarily zeroed). */

#define GC_malloc_atomic_tagged GC_malloc_one_tagged
#define GC_malloc_small_atomic_tagged GC_malloc_one_small_dirty_tagged
/*
   Alloc pointer-free tagged memory (not necessarily zeroed).
   Racket sets the tag before a collection. */

GC2_EXTERN void *GC_malloc_atomic_uncollectable(size_t size_in_bytes);
/*
   Like plain malloc: pointer-free, never collected. */

GC2_EXTERN void *GC_malloc_allow_interior(size_t size_in_bytes);
/*
   Alloc an array of pointers (typically large), and recognize
   pointers into the middle of the array, or just past the end of the
   array. */

GC2_EXTERN void *GC_malloc_atomic_allow_interior(size_t size_in_bytes);
/*
   Like GC_malloc_allow_interior(), but for an atomic object. */

GC2_EXTERN void *GC_malloc_tagged_allow_interior(size_t size_in_bytes);
/*
   Like GC_malloc_allow_interior(), but for a tagged object. */

GC2_EXTERN void *GC_malloc_weak_array(size_t size_in_bytes, void *replace_val);
/*
   Alloc an array of weak pointers, initially zeroed.  When a value in
   the array is collected, it's replaced by `replace-val'. The
   precense of a pointer in the array doesn't keep the referenced
   memory from being collected. See also README for information about
   the structure of the array. */

GC2_EXTERN void GC_free(void *);
/* 
   Lets the collector optionally reverse an allocation immediately.
   [Generally a no-op.] */

GC2_EXTERN void *GC_malloc_weak_box(void *p, void **secondary, int soffset, int is_late);
/* 
   Allocate a weak box. See README for details. */

GC2_EXTERN void *GC_malloc_ephemeron(void *p, void *p2);
/* 
   Allocate an ephemeron. See README for details. */

GC2_EXTERN void **GC_malloc_immobile_box(void *p);
GC2_EXTERN void GC_free_immobile_box(void **b);
/* 
   Allocate (or free) a non-GCed box containing a pointer to a GCed
   value.  The pointer is stored as the first longword of the box. */

GC2_EXTERN intptr_t GC_malloc_stays_put_threshold();
/*
   Returns a minimum size for which allocations generate
   objects that never move, and where pointers are allowed
   into the object's interior. */

GC2_EXTERN int GC_is_on_allocated_page(void *p);
/* 
   Returns 1 if p refers to a page of memory on which
   the GC allocates objects (although p may or may not
   be a valid pointer to the start of an alloctaed object). */

GC2_EXTERN int GC_allocate_phantom_bytes(intptr_t);
/* 
   Returns 0 if allocation should fail due to a memory limit, 
   1 otherwise. */

/***************************************************************************/
/* Memory tracing                                                          */
/***************************************************************************/
GC2_EXTERN int GC_mtrace_new_id(void *f);
GC2_EXTERN int GC_mtrace_union_current_with(int newval);

/***************************************************************************/
/* Finalization                                                            */
/***************************************************************************/

typedef void (*GC_finalization_proc)(void *p, void *data);
/*
   Type of a finalization procedure. */

GC2_EXTERN void GC_set_finalizer(void *p, int tagged, int level, 
				 GC_finalization_proc f, void *data, 
				 GC_finalization_proc *oldf, void **olddata);
/*
   See README for details. */

/***************************************************************************/
/* Cooperative GC                                                          */
/***************************************************************************/

THREAD_LOCAL_DECL(GC2_EXTERN void **GC_variable_stack);
/*
   See the general overview in README. */

GC2_EXTERN void **GC_get_variable_stack();
GC2_EXTERN void GC_set_variable_stack(void **p);

GC2_EXTERN void GC_register_traversers(short tag, Size_Proc size, Mark_Proc mark, Fixup_Proc fixup,
				       int is_constant_size, int is_atomic);
GC2_EXTERN void GC_register_traversers2(short tag, Size2_Proc size, Mark2_Proc mark, Fixup2_Proc fixup,
				        int is_constant_size, int is_atomic);
/*
   Registers a traversal procedure for a tag. Obviously, a traversal
   procedure must be installed for each tag before a collection
   happens where an instance of the tag as been allocated.  If objects
   using the tag are always of the same size, is_constant_size can be
   non-zero, and `size' must return the right size given a null
   pointer. If objects using the tag are atomic, is_atomic can be
   non-zero. */

/* #define gcMARK(x) ... see below ... */
/* #define gcMARK_TYPED(t, x) ... see below ... */
/* #define gcMARK_TYPED_NOW(t, x) ... see below ... */
/* #define gcFIXUP(x) ... see below ... */
/* #define gcFIXUP_TYPED(t, x) ... see below ... */
/* #define gcFIXUP_TYPED_NOW(t, x) ... see below ... */
/* Macros that, given an l-value and optional type, marks the
   referenced memory as live and updates the pointer as necessary
   (i.e., if it's GCable memory that is moving). The `x' argument can
   appear in the macro's output multiple times, and the output can be
   a statement rather than a expression. 

   The NOW versions force the mark or fixup to happen immediately. The
   other forms can queue the mark or fixup to happen later. */

/* #define gcBYTES_TO_WORDS(x) ((x + 3) >> 2) */
/*
   Helpful macro for computing the return value in a traversal proc,
   which must be in words. */

GC2_EXTERN void *GC_resolve(void *p);
GC2_EXTERN void *GC_resolve2(void *p, struct NewGC *gc);
/*
   Can be called by a traversal proc to get the current address of a
   object that might have been moved already. This is necessary, for
   example, if the size or structure of an object depends on the
   content of an object it references. For example, the size of a
   class instance usually depends on a field count that is stored in
   the class. In fixup mode, call this before fixing up. */

GC2_EXTERN void *GC_fixup_self(void *p);
/*
   Can be called by a fixup proc to get the final address of the
   pointer passed to the fixup proc. This is the identity function
   only when objects are moved before fixup, but objects might
   be moved after fixup. */

/* INTERNAL for the current implemenation (used by macros): */
GC2_EXTERN void GC_mark(const void *p);
GC2_EXTERN void GC_fixup(void *p);
GC2_EXTERN void GC_mark2(const void *p, struct NewGC *gc);
GC2_EXTERN void GC_fixup2(void *p, struct NewGC *gc);
/*
   Used in the expansion of gcMARK and gcFIXUP. 
 
   These procedures and variables are internal to the current
   implementation, and are *not* part of the "official" interface. */

GC2_EXTERN int GC_is_marked2(const void *p, struct NewGC *gc);
/*
   Reports whether p has been marked. */

GC2_EXTERN int GC_is_partial(struct NewGC *gc);
/* 
   Reports whether the current GC is a non-full collection. */

GC2_EXTERN void GC_retract_only_mark_stack_entry(void *pf, struct NewGC *gc);
/*
   Used for very special collaboration with GC. */


GC2_EXTERN void GC_mark_variable_stack(void **var_stack,
				       intptr_t delta,
				       void *limit,
                                       void *stack_mem);
GC2_EXTERN void GC_fixup_variable_stack(void **var_stack,
					intptr_t delta,
					void *limit,
                                        void *stack_mem);
GC2_EXTERN void GC_mark2_variable_stack(void **var_stack,
                                        intptr_t delta,
                                        void *limit,
                                        void *stack_mem,
                                        struct NewGC *gc);
GC2_EXTERN void GC_fixup2_variable_stack(void **var_stack,
                                         intptr_t delta,
                                         void *limit,
                                         void *stack_mem,
                                         struct NewGC *gc);
/*
   Can be called by a mark or fixup traversal proc to traverse and
   update a chunk of (atomically-allocated) memory containing an image
   of the stack.

   The `var_stack' argument corresponds to the value of GC_var_stack
   for the copied stack (see the overview at the top of this
   file). The `var_stack' pointer refers to the address of the chain
   in the original stack, not in the heap copy. The `delta' argument
   specifies the difference heap_copy_address - stack_address (where
   stack_address is the numerically lower bound for the copied stack
   region, regardless of which direction the stack grows). The `limit'
   argument corresponds to the value that would have been returned by
   GC_get_thread_stack_base() at the time the stack was copied. 

   The `stack_mem' argument indicates the start of the allocated memory
   that contains `var_stack'. It is used for backtraces. */

GC2_EXTERN int GC_merely_accounting();
/*
   Can be called by a mark or fixup traversal proc to determine whether
   the traversal is merely for accounting, in which case some marking
   can be skipped if the corresponding data should be charged to a
   different object. */

GC2_EXTERN void GC_write_barrier(void *p);
/* 
   Explicit write barrier to ensure that a write-barrier signal is not
   triggered by a memory write.
*/

GC2_EXTERN void GC_switch_out_master_gc();
/*
   Makes the current GC the master GC.
   Creates a new place specific GC and links it to the master GC.
*/

GC2_EXTERN struct NewGC *GC_get_current_instance();
/*
   Returns a representation of the current GC.
*/

GC2_EXTERN void GC_construct_child_gc(struct NewGC *parent_gc, intptr_t limit);
/*
   Creates a new place-specific GC that is a child for memory-accounting
   purposes of the give parent GC. If `limit' is not 0, set the maximum
   amount of memory the new GC is supposed to use.
*/

GC2_EXTERN intptr_t GC_propagate_hierarchy_memory_use();
/* 
   Notifies the parent GC (if any) of memory use by the current GC
   and its children. The result is total memory use. */

GC2_EXTERN void GC_destruct_child_gc();
/*
   Destroys a place-specific GC once the place has finished.
*/

GC2_EXTERN void *GC_switch_to_master_gc();
/*
   Switches to the master GC.
*/

GC2_EXTERN void GC_switch_back_from_master(void *gc);
/*
   Switches to back to gc from the master GC.
*/

GC2_EXTERN int GC_is_using_master();
/*
   Reports whether the master GC is in use after a non-master GC
   has been created.
*/

GC2_EXTERN intptr_t GC_alloc_alignment();
/*
   Guaranteeed alignment for nusery pages. Returns a constant, and
   can be called from any thread.
*/

GC2_EXTERN uintptr_t GC_make_jit_nursery_page(int count, uintptr_t *sz);
/*
   Obtains nursery pages from the GC for thread local allocation;
   resulting space is count times the allocation alignment.
   The result is an uintptr_t because it's not a valid
   pointer to a GCable object. The result becomes invalid (i.e. it's collected)
   with the next GC. If non-NULL, the `sz' argument is filled
   with the length of the allocation area after the result.
*/

GC2_EXTERN void GC_check_master_gc_request();
/*
   Checks to see if the master has requested a places major GC run 
   and executes a GC if requested
*/

GC2_EXTERN void GC_set_put_external_event_fd(void *fd);
/*
   Sets the fd that can be passed to scheme_signal_received_at to wake up the place for GC
*/

GC2_EXTERN void GC_allow_master_gc_check();
/*
   Signals the GC after spawning a place that the places is sufficiently set up to participate
   in master gc collections
*/

GC2_EXTERN void GC_create_message_allocator();
/*
   Saves off the gc->gen0 to gc->saved_allocator.
   Captures all memory allocations until GC_finish_message_allocator i
   is called so they can be sent to another place.
*/

GC2_EXTERN void *GC_finish_message_allocator();
/*
   Stops memory allocation capture.
   Restores gc->saved_allocator to gc->gen0.
   Returns a void* that represents the message memory captured.
*/

GC2_EXTERN void GC_adopt_message_allocator(void *msg_memory);
/*
   Adopts the message memory captured by the sending place into
   the current receiving place's gc
*/

GC2_EXTERN intptr_t GC_is_place();
/*
   Returns 1 if current GC is a place gc.
   Otherwise returns 0;
*/

  GC2_EXTERN int GC_message_small_objects_size(void *msg_memory, intptr_t up_to);
/*
 Determines whether the message qualifies as short and whether the
 total size of all objects allocated by the  message allocator is less
 than `up_to'
 */

GC2_EXTERN intptr_t GC_message_allocator_size(void *msg_memory);
/*
 Returns the total size of all memory pages allocated by the message allocator
 */

GC2_EXTERN void GC_dispose_short_message_allocator(void *msg_memory);
/*
 Disposes of small message allocators that were copied by the receiving place
 */

GC2_EXTERN void GC_destroy_orphan_msg_memory(void *msg_memory);
/*
 Used to destroys a message allocators that is still in the place channel queue when
 the place channels finalizer is called.
 */

GC2_EXTERN void GC_report_unsent_message_delta(intptr_t amt);
/* 
 Report message-in-flight size changes to the GC. This functionality is exposed,
 rather than built into GC_finish_message_allocator(), GC_adpot_message_allocator(),
 GC_dispose_short_message_allocator(), and GC_destroy_orphan_msg_memory(), so that
 meesages to the master GC can be limited as long as the unsent message tracking
 is within a factor of 2 or so.
 */

GC2_EXTERN void GC_set_backpointer_object(void *p);
/*
 Registers the current object for backpointers, which is used when backtrace
 support is enabled.
*/

# ifdef __cplusplus
};
# endif

#endif

/* Macros (implementation-specific): */
#if defined(__x86_64__) || defined(_WIN64)
# define gcLOG_WORD_SIZE 3
#else
# define gcLOG_WORD_SIZE 2
#endif
#define gcMARK(x) GC_mark(x)
#define gcMARK2(x, gc) GC_mark2(x, gc)
#define gcMARK_TYPED(t, x) gcMARK(x)
#define gcMARK2_TYPED(t, x, gc) gcMARK2(x, gc)
#define gcMARK_TYPED_NOW(t, x) gcMARK(x)
#define gcMARK2_TYPED_NOW(t, x, gc) gcMARK(x, gc)
#define gcFIXUP_TYPED_NOW(t, x) GC_fixup(&(x))
#define gcFIXUP2_TYPED_NOW(t, x, gc) GC_fixup2(&(x), gc)
#define gcFIXUP_TYPED(t, x) gcFIXUP_TYPED_NOW(void*, x)
#define gcFIXUP2_TYPED(t, x, gc) gcFIXUP2_TYPED_NOW(void*, x, gc)
#define gcFIXUP(x) gcFIXUP_TYPED(void*, x)
#define gcFIXUP2(x, gc) gcFIXUP2_TYPED(void*, x, gc)
#define gcBYTES_TO_WORDS(x) ((x + (1 << gcLOG_WORD_SIZE) - 1) >> gcLOG_WORD_SIZE)
#define gcWORDS_TO_BYTES(x) (x << gcLOG_WORD_SIZE)

#define GC_INTERIORABLES_NEVER_MOVE 1

#endif /* __mzscheme_gc_2__ */
