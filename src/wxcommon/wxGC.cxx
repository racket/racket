/*************************************************************************

MrEd interface to various garbage collectors, including the Boehm
 collector, SenoraGC, and MzScheme's precise collector.

Copyright (c) 2004-2005 PLT Scheme, Inc.

*************************************************************************/

/*************************************************************************
Based On:

Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 
THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 
    Last modified on Sat Nov 19 19:31:14 PST 1994 by ellis
                  on Sat Jun  8 15:10:00 PST 1994 by boehm

Permission is hereby granted to copy this code for any purpose,
provided the above notices are retained on all copies.

Authors: John R. Ellis and Jesse Hull

**************************************************************************/
/* Boehm, December 20, 1994 7:26 pm PST */

#include <stddef.h>
#include "wxGC.h"

#ifdef MPW_CPLUS
extern "C" {
  typedef void (*GC_F_PTR)(void *, void *);
}
# define CAST_GCP (GC_F_PTR)
# define CAST_GCPP (GC_F_PTR *)
#else
# define CAST_GCP /* empty */
# define CAST_GCPP /* empty */
#endif

#ifdef USE_SENORA_GC
struct GC_Set *cpp_objects;
typedef void (*GC_finalization_proc)(void *, void *);
#if SGC_STD_DEBUGGING
# define USE_WXOBJECT_TRACE_COUNTER
#endif
#endif

void *operator new(size_t size)
{
#ifdef USE_SENORA_GC
  if (!cpp_objects)
    cpp_objects = GC_new_set("C++", NULL, NULL, NULL, NULL, NULL, 0);

  return GC_malloc_specific(size, cpp_objects);
#else
  return GC_malloc(size);
#endif
}

void operator delete(void * /*obj*/)
{
}

void gc_cleanup::install_cleanup(void)
{
  GC_finalization_proc old_fn;
  void *old_data;

# ifdef MZ_PRECISE_GC
#  define ALLOW_NON_BASE 0
#  define CHECK_BASE 0
# else
#  ifdef wx_xt
#   define ALLOW_NON_BASE 0
#   define CHECK_BASE 0
#  else
#   ifdef WIN32
#    define ALLOW_NON_BASE 0
#    define CHECK_BASE 1
#    define CRASH_ON_NONBASE 1
#   else
#    define ALLOW_NON_BASE 1
#    define CHECK_BASE 0
#   endif
#  endif
# endif

# if CHECK_BASE || ALLOW_NON_BASE
  if (GC_base(this) != (void *)this) {
#  if ALLOW_NON_BASE
    return;
#  else
#   ifdef CRASH_ON_NONBASE
    *(long *)0x0 = 1;
#   else
    printf("Clean-up object is not the base object\n");
    abort();
#   endif
#  endif
  }
# endif

  GC_register_finalizer_ignore_self(gcOBJ_TO_PTR(this), 
				    CAST_GCP GC_cleanup, NULL, 
				    CAST_GCPP &old_fn, &old_data);

# if CHECK_BASE
  if (old_fn) {
#  ifdef CRASH_ON_NONBASE
	*(long *)0x0 = 1;
#  else
    printf("Object already has a clean-up\n");
    abort();
#  endif
  }
# endif
}

void GC_cleanup(void *obj, void *)
{
  gc *clean = (gc *)gcPTR_TO_OBJ(obj);

#ifdef MZ_PRECISE_GC
  GC_cpp_delete(clean);
#else
  clean->~gc();
#endif
}

/**********************************************************************/  

#ifdef OPERATOR_NEW_ARRAY

void* operator new[](size_t size)
{
#ifdef USE_SENORA_GC
  if (!cpp_objects)
    cpp_objects = GC_new_set("C++", NULL, NULL, NULL, NULL, NULL, 0);
  
  return GC_malloc_specific(size, cpp_objects);
#else
  return GC_malloc(size);
#endif
}
  
void operator delete[](void * /*obj*/)
{
}

#endif

/**********************************************************************/

#ifdef USE_SENORA_GC

struct GC_Set *wx_objects;

# ifdef USE_WXOBJECT_TRACE_COUNTER
extern void wxTraceCount(void *, int);
extern void wxTracePath(void *, unsigned long, void *);
extern void wxTraceInit(void);
extern void wxTraceDone(void);
extern void wxObjectFinalize(void *);
# endif

void *GC_cpp_malloc(size_t size)
{
  if (!wx_objects)
    wx_objects = GC_new_set("wxObjects", 
# ifdef USE_WXOBJECT_TRACE_COUNTER
			    wxTraceInit,
			    wxTraceDone,
			    wxTraceCount,
			    wxTracePath,
			    wxObjectFinalize,
# else
			    NULL, NULL, NULL, NULL, NULL,
# endif
			    0);

  return GC_malloc_specific(size, wx_objects);
}

# ifdef SGC_STD_DEBUGGING

void GC_cpp_for_each(void (*f)(void *, int, void *), void *data)
{
  if (wx_objects)
    GC_for_each_element(wx_objects, f, data);
}

int GC_is_wx_object(void *v)
{
  return wx_objects && (GC_set(v) == wx_objects);
}

# endif

#endif

/**********************************************************************/

#ifdef MZ_PRECISE_GC

# define ZERO_OUT_DISPATCH 1

typedef struct {
  short tag;
  short filler_used_for_hashing;
  void *val;
} GC_WB;

void *GC_weak_box_val(void *b)
{
  return ((GC_WB *)b)->val;
}

#include "scheme.h"

static void mark_cpp_object(void *p)
{
  gc *obj = (gc *)p;

#if ZERO_OUT_DISPATCH
  if (*(long *)obj)
#endif
    obj->gcMark();
}

static void fixup_cpp_object(void *p)
{
  gc *obj = (gc *)p;

#if ZERO_OUT_DISPATCH
  if (*(long *)obj)
#endif
    obj->gcFixup();
}

static int is_initialized;

static void initize(void)
{
  /* Initialize: */
  GC_mark_xtagged = mark_cpp_object;
  GC_fixup_xtagged = fixup_cpp_object;
  
  is_initialized = 1;
}

void *GC_cpp_malloc(size_t size)
{
  void *p;

  if (!is_initialized)
    initize();

  p = GC_malloc_one_xtagged(size);

  return p;
}

void GC_cpp_delete(gc *v)
{
  v->~gc();
#if ZERO_OUT_DISPATCH
  ((long *)v)[0] = 0;
#endif
}

#endif

/**********************************************************************/

static long total, accum = 1024 * 1024 * 5;

void *GC_malloc_accounting_shadow(long a)
{
  long *p;
  if (a < (long)sizeof(long))
    a = sizeof(long);
  total += a;
  accum -= a;
  if (accum <= 0) {
    GC_gcollect();
    accum = total >> 2;
  }
  p = (long *)GC_malloc_atomic(a);
  *p = a;
  return (void *)p;
}

void GC_free_accounting_shadow(void *p)
{
  if (p)
    total -= *(long *)p;
}
