
#ifndef __mzscheme_gc_2__
#define __mzscheme_gc_2__

/***************************************************************************/
/***   See README for a general overview of the interface architecture.  ***/
/***************************************************************************/

#ifndef GC2_JUST_MACROS

typedef int (*Size_Proc)(void *obj);
typedef int (*Mark_Proc)(void *obj);
typedef int (*Fixup_Proc)(void *obj);
/* 
   Types of the traversal procs (supplied by MzScheme); see overview in README
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

GC2_EXTERN unsigned long (*GC_get_thread_stack_base)(void);
/* 
   Called by GC to get the base for stack traversal in the current
   thread (see README). The returned address must not be in the middle
   of a variable-stack record. */

GC2_EXTERN void GC_set_stack_base(void *base);
GC2_EXTERN unsigned long GC_get_stack_base(void);
/*
   Called by MzScheme to set/get value used for stack base when
   GC_get_thread_stack_base is null. This is mainly useful for getting
   MzScheme started, before it has multiple threads. */

GC2_EXTERN void GC_add_roots(void *start, void *end);
/*
   Called by MzScheme to install roots. The memory between
   `start' (inclusive) and `end' (exclusive) contains pointers. */

GC2_EXTERN void GC_init_type_tags(int count, int weakbox);
/*
   Called by MzScheme to indicate the number of different type tags it
   uses, starting from 0. `count' is always less than 256. The weakbox
   argument is the value to be used for tagging weak box. (The GC has
   some freedom in the layout of a weak box, so it performs weak box
   traversals itself, but MzScheme gets to choose the tag.) */

GC2_EXTERN void GC_register_thread(void *, void *);
/*
   Indicates that a just-allocated point is for a thread record
   owned by a particular custodian. */

GC2_EXTERN void (*GC_collect_start_callback)(void);
GC2_EXTERN void (*GC_collect_end_callback)(void);
/*
   Called by GC before/after performing a collection. Used by MzScheme
   to zero out some data and record collection times. The end
   procedure should be called before finalizations are performed. */

GC2_EXTERN void (*GC_out_of_memory)(void);
/*
   Called by GC when it can't satify a memory request. GC_out_of_memory()
   might perform a longjmp. */

GC2_EXTERN void GC_dump(void);
/*
   Dumps memory state info to stderr. */

GC2_EXTERN long GC_get_memory_use(void *c);
/*
   Returns the number of currently-allocated bytes (speficilly for
   custodian c, as much as the GC's accounting makes possible). */

#define MZACCT_REQUIRE		0
#define MZACCT_LIMIT		1
GC2_EXTERN int GC_set_account_hook(int type, void *c1, unsigned long b, void *c2);
/*
  Set a memory-accounting property. Returns 0 for failure (i.e., not
  supported). */

GC2_EXTERN void GC_gcollect(void);
/*
   Performs an immediate (full) collection. */

/***************************************************************************/
/* Allocation                                                              */
/***************************************************************************/

GC2_EXTERN void *GC_malloc(size_t size_in_bytes);
/*
   Alloc an array of pointers, initially zeroed. */

GC2_EXTERN void *GC_malloc_one_tagged(size_t);
/* 
   Alloc a tagged item, initially zeroed.  MzScheme sets the tag
   before a collection. */

GC2_EXTERN void *GC_malloc_one_xtagged(size_t);
/* 
   Alloc an item, initially zeroed. Rather than having a specific tag,
   all objects allocated this way are marked/fixedup via the function
   in GC_mark_xtagged and GC_fixup_xtagged. MzScheme sets
   GC_{mark,fixup}_xtagged. */

GC2_EXTERN void (*GC_mark_xtagged)(void *obj);
GC2_EXTERN void (*GC_fixup_xtagged)(void *obj);
/* 
  Mark and fixup functions for memory allocated with
  GC_malloc_one_xtagged(). */

GC2_EXTERN void *GC_malloc_array_tagged(size_t);
/* 
   Alloc an array of tagged items. MzScheme sets the tag in the first
   item before a collection, by maybe not all items. When traversing,
   use the first one for size. */

GC2_EXTERN void *GC_malloc_atomic(size_t size_in_bytes);
/*
   Alloc pointerless memory (not necessarily zeroed). */

#define GC_malloc_atomic_tagged GC_malloc_one_tagged
/*
   Alloc pointer-free tagged memory (not necessarily zeroed).
   MzScheme sets the tag before a collection. */

GC2_EXTERN void *GC_malloc_atomic_uncollectable(size_t size_in_bytes);
/*
   Like plain malloc: pointer-free, never collected. */

GC2_EXTERN void *GC_malloc_allow_interior(size_t size_in_bytes);
/*
   Alloc an array of pointers (typically large), and recognize
   pointers into the middle of the array, or just past the end of the
   array. */

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

GC2_EXTERN void *GC_malloc_weak_box(void *p, void **secondary, int soffset);
/* 
   Allocate a weak box. See README for details. */

GC2_EXTERN void **GC_malloc_immobile_box(void *p);
GC2_EXTERN void GC_free_immobile_box(void **b);
/* 
   Allocate (or free) a non-GCed box containing a pointer to a GCed
   value.  The pointer is stored as the first longword of the box. */

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

GC2_EXTERN void GC_finalization_weak_ptr(void **p, int offset);
/*
   See README for details. */

/***************************************************************************/
/* Cooperative GC                                                          */
/***************************************************************************/

GC2_EXTERN void **GC_variable_stack;
/*
   See the general overview in README. */

GC2_EXTERN void GC_register_traversers(short tag, Size_Proc size, Mark_Proc mark, Fixup_Proc fixup,
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
/*
   Used in the expansion of gcMARK and gcFIXUP. 
 
   These procedures and variables are internal to the current
   implementation, and are *not* part of the "official" interface. */

GC2_EXTERN void GC_mark_variable_stack(void **var_stack,
				       long delta,
				       void *limit);
GC2_EXTERN void GC_fixup_variable_stack(void **var_stack,
					long delta,
					void *limit);
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
   GC_get_thread_stack_base() at the time the stack was copied. */

# ifdef __cplusplus
};
# endif

#endif

/* Macros (implementation-specific): */
#define gcMARK(x) GC_mark(x)
#define gcMARK_TYPED(t, x) gcMARK(x)
#define gcMARK_TYPED_NOW(t, x) gcMARK(x)
#define gcFIXUP_TYPED_NOW(t, x) GC_fixup(&(x))
#define gcFIXUP_TYPED(t, x) gcFIXUP_TYPED_NOW(void*, x)
#define gcFIXUP(x) gcFIXUP_TYPED(void*, x)
#define gcBYTES_TO_WORDS(x) ((x + 3) >> 2)
#define gcWORDS_TO_BYTES(x) (x << 2)

#define GC_INTERIORABLES_NEVER_MOVE 1

#endif /* __mzscheme_gc_2__ */
