/*************************************************************************

MrEd interface to various garbage collectors, including the Boehm
 collector, SenoraGC, and MzScheme's precise collector.

Copyright (c) 2004-2010 PLT Scheme Inc.

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

/* With extensions, or with x86 Mac OS X, MrEd's `new' gets used by
   libraries that shouldn't use it. So we can't define `new' on that
   platform. For PPC, we define `new' and `delete' to use malloc() and
   free(); for some reason, linking fails in Mac OS X 10.3 if we just
   omit `new' and `delete'. There should be no problem under Windows,
   due to the way DLL linking works. */

#ifndef WIN32
# if defined(OS_X) && defined(__POWERPC__)
#  define MALLOC_FOR_BUILTIN_NEW
#  include <stdio.h>
#  include <stdlib.h>
# else
#  define DONT_DEFINE_BUILTIN_NEW
# endif
#endif

#ifdef COMPACT_BACKTRACE_GC  
# include <stdio.h>
#endif

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

#ifdef MZ_PRECISE_GC
# undef USE_SENORA_GC
#endif

#ifdef USE_SENORA_GC
struct GC_Set *cpp_objects;
typedef void (*GC_finalization_proc)(void *, void *);
#if SGC_STD_DEBUGGING
# define USE_WXOBJECT_TRACE_COUNTER
#endif
#endif

#ifndef DONT_DEFINE_BUILTIN_NEW

void *operator new(size_t size)
{
#ifdef MALLOC_FOR_BUILTIN_NEW
  return malloc(size);
#else
# ifdef USE_SENORA_GC
  if (!cpp_objects)
    cpp_objects = GC_new_set("C++", NULL, NULL, NULL, NULL, NULL, 0);

  return GC_malloc_specific(size, cpp_objects);
# else
  return GC_malloc(size);
#endif
#endif
}

void operator delete(void *obj)
{
#ifdef MALLOC_FOR_BUILTIN_NEW
  free(obj);
#endif
}

#endif

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

#define SHOW_CLEANUP_TIMES 0

#if SHOW_CLEANUP_TIMES
extern "C" long scheme_get_process_milliseconds();
#endif

void GC_cleanup(void *obj, void *)
{
  gc *clean = (gc *)gcPTR_TO_OBJ(obj);

#ifdef MZ_PRECISE_GC
# ifdef COMPACT_BACKTRACE_GC  
#  if SHOW_CLEANUP_TIMES
  long start;
  start = scheme_get_process_milliseconds();
  char *s;
  s = clean->gcGetName();
  printf("Cleanup: %s\n", s ? s : "???");
#  endif
# endif

  GC_cpp_delete(clean);

# ifdef COMPACT_BACKTRACE_GC  
#  if SHOW_CLEANUP_TIMES
  start = scheme_get_process_milliseconds() - start;
  printf("  done %d\n", start);
#  endif
# endif
#else
  clean->~gc();
#endif
}

/**********************************************************************/  

#ifdef OPERATOR_NEW_ARRAY
# ifndef DONT_DEFINE_BUILTIN_NEW

void* operator new[](size_t size)
{
#ifdef MALLOC_FOR_BUILTIN_NEW
  return malloc(size);
#else
# ifdef USE_SENORA_GC
  if (!cpp_objects)
    cpp_objects = GC_new_set("C++", NULL, NULL, NULL, NULL, NULL, 0);
  
  return GC_malloc_specific(size, cpp_objects);
# else
  return GC_malloc(size);
# endif
#endif
}
  
void operator delete[](void *obj)
{
#ifdef MALLOC_FOR_BUILTIN_NEW
  free(obj);
#endif
}

# endif
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

# ifdef COMPACT_BACKTRACE_GC
static char *get_xtagged_name(void *p);
extern char *(*GC_get_xtagged_name)(void *p);
# endif

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

# ifdef COMPACT_BACKTRACE_GC
  GC_get_xtagged_name = get_xtagged_name;
# endif
  
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

# ifdef COMPACT_BACKTRACE_GC

static char name_buffer[256];

static char *get_xtagged_name(void *p)
{
  char *s;
  s = ((gc *)gcPTR_TO_OBJ(p))->gcGetName();
  sprintf(name_buffer, "<%s>", (s ? s : "XTAGGED"));
  return name_buffer;
}

char *gc::gcGetName() {
  return NULL;
}

# endif

#endif

/**********************************************************************/

/* The accounting shadow serves two purposes. First, it allocates
   largely untouched atomic memory to reflect the allocation of
   bitmaps outside the GC space, so that the nominal allocation total
   is related to the total taking into accoun bitmaps. Second, because
   bitmaps are released through finalizers, the accounting shadow
   forces a GC more frequently than might otherwise happen as the
   total size of bitmaps grows. */

#define INIT_ACCUM_SIZE 1024 * 1024 * 5
#define INIT_ACCUM_COUNT 1000

static long total, accum = INIT_ACCUM_SIZE;
static int total_count, accum_count = INIT_ACCUM_COUNT;

void *GC_malloc_accounting_shadow(long a)
{
  long *p;
  if (a < (long)sizeof(long))
    a = sizeof(long);
  total += a;
  accum -= a;
  total_count += 1;
  accum_count -= 1;
  if (accum <= 0) {
    GC_gcollect();
    accum = total >> 1;
    if (accum < INIT_ACCUM_SIZE)
      accum = INIT_ACCUM_SIZE;
  }
#ifdef wx_msw
  /* Under Windows, the number of bitmaps matters, even if
     they're small. */
  if (accum_count <= 0) {
    GC_gcollect();
    accum_count = total_count >> 1;
    if (accum_count < INIT_ACCUM_COUNT)
      accum_count = INIT_ACCUM_COUNT;
  }
#endif
  p = (long *)GC_malloc_atomic(a);
  *p = a;
  return (void *)p;
}

void GC_free_accounting_shadow(void *p)
{
  if (p) {
    total -= *(long *)p;
    accum += *(long *)p;
    total_count -= 1;
    accum_count += 1;
  }
}
