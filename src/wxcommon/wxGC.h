#ifndef WXGC_CPP_H
#define WXGC_CPP_H

/****************************************************************************

MrEd interface to various garbage collectors, including the Boehm
 collector, SenoraGC, and MzScheme's precise collector.

Copyright (c) 2004-2005 PLT Scheme, Inc.

****************************************************************************/

/****************************************************************************
Based On:

C++ Interface to the Boehm Collector

    John R. Ellis and Jesse Hull 
    Last modified on Wed Jan  4 16:30:20 PST 1995 by ellis


Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 
THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 
Permission is hereby granted to use or copy this program for any
purpose, provided the above notices are retained on all copies.
Permission to modify the code and to distribute modified code is
granted, provided the above notices are retained, and a notice that
the code was modified is included with the above copyright notice.
****************************************************************************/

enum GCPlacement {UseGC, AtomicGC};

typedef void (*GCCleanUpFunc)(void* obj, void* clientData);

extern "C" {
  void objscheme_mark_external_invalid(void *);
}
void GC_cleanup(void *obj, void *ignored);

#include "gc.h"

/******************** Special kinds of GC *********************/

#if SGC_STD_DEBUGGING
# ifndef USE_SENORA_GC
#  define USE_SENORA_GC
# endif
# define USE_MEMORY_TRACING 
#endif

#ifdef USE_SENORA_GC
extern void *GC_cpp_malloc(size_t);
#endif

#define WX_REGISTER_GLOBAL_MEMORY

#ifdef WX_REGISTER_GLOBAL_MEMORY
extern "C" {
# ifdef wx_msw
	__declspec(dllimport)
# endif
	void scheme_register_static(void *p, long size);
}
# define wxREGGLOB(x) scheme_register_static((void *)&x, sizeof(x))
#else
# define wxREGGLOB(x) /* empty */
#endif

#ifdef MZ_PRECISE_GC
extern void *GC_cpp_malloc(size_t);
extern void GC_cpp_delete(class gc *);
# define GC_register_finalizer_ignore_self(self, a, b, c, d) GC_set_finalizer(self, 0, 3, a, b, c, d)
extern void *GC_weak_box_val(void *);
#endif


#define gcOBJ_TO_PTR(x) x
#define gcPTR_TO_OBJ(x) x

extern void *GC_malloc_accounting_shadow(long a);
extern void GC_free_accounting_shadow(void *a);

#ifndef START_XFORM_SKIP
# ifndef MZ_PRECISE_GC
#  define START_XFORM_SKIP /**/
#  define END_XFORM_SKIP /**/
#  define GC_CAN_IGNORE /**/
#  define GC_MAYBE_IGNORE_INTERIOR /**/
#  define XFORM_OK_PLUS +
#  define XFORM_OK_MINUS -
# else
#  ifdef GC_INTERIORABLES_NEVER_MOVE
#   define GC_MAYBE_IGNORE_INTERIOR GC_CAN_IGNORE
#  else
#   define GC_MAYBE_IGNORE_INTERIOR /**/
#  endif
# endif
#endif

/**** The `gc' and `gc_cleanup' class ************/

class gc
{
public:
  inline virtual ~gc();

  inline void *operator new(size_t size);
  inline void *operator new(size_t size, GCPlacement gcp);
  inline void operator delete(void *obj);
#ifdef OPERATOR_NEW_ARRAY
  inline void *operator new[](size_t size);
  inline void *operator new[](size_t size, GCPlacement gcp);
  inline void operator delete[](void *obj);
#endif

#ifdef MZ_PRECISE_GC
  /* Overridden in each subclass: */
  virtual inline void gcMark();
  virtual inline void gcFixup();
#endif
};

class gc_cleanup : public gc
{
public:
  void *__gc_external;

  inline gc_cleanup();
  inline gc_cleanup(int cleanup);
  inline virtual ~gc_cleanup();
  void install_cleanup();  

#ifdef MZ_PRECISE_GC
  inline void gcMark();
  inline void gcFixup();
#endif
};

#ifdef MZ_PRECISE_GC
inline void gc::gcMark()
{
}

inline void gc::gcFixup()
{
}

inline void gc_cleanup::gcMark()
{
  gcMARK(__gc_external);
}

inline void gc_cleanup::gcFixup()
{
  gcFIXUP(__gc_external);
}
#endif

/***** Constructors and Destructors: ******/

inline gc_cleanup::gc_cleanup(void)
{
  __gc_external = NULL;
  install_cleanup();
}

inline gc_cleanup::gc_cleanup(int cleanup)
{
  __gc_external = NULL;
  if (cleanup)
    install_cleanup();
}

inline gc_cleanup::~gc_cleanup(void)
{
  if (__gc_external)
    objscheme_mark_external_invalid(__gc_external);
  GC_register_finalizer_ignore_self(gcOBJ_TO_PTR(this), 0, 0, 0, 0);
}

inline gc::~gc(void)
{
}

/***** Allocators: ******/

inline void *gc::operator new(size_t size)
{
#if defined(USE_SENORA_GC) || defined(MZ_PRECISE_GC)
  return GC_cpp_malloc(size);
#else
  return GC_malloc(size);
#endif
}

inline void *gc::operator new(size_t size, GCPlacement gcp)
{
  if (gcp == AtomicGC) 
    return GC_malloc_atomic(size);

#if defined(USE_SENORA_GC) || defined(MZ_PRECISE_GC)
  return GC_cpp_malloc(size);
#else
  return GC_malloc(size);
#endif
}

inline void gc::operator delete(void * /*obj*/) 
{
}


#ifdef OPERATOR_NEW_ARRAY
inline void *gc::operator new[](size_t size) {
#if defined(USE_SENORA_GC) || defined(MZ_PRECISE_GC)
  return ::operator new(size);
#else
  return gc::operator new(size);
#endif
}
    
inline void *gc::operator new[](size_t size, GCPlacement gcp) {
  return gc::operator new(size, gcp);
}

inline void gc::operator delete[](void *obj) {
  gc::operator delete(obj);
}
#endif


/*************** For objects not derived from `gc' ***********************/

inline void *operator new(size_t size, GCPlacement gcp)
{
  void *obj;
  
  if (gcp == AtomicGC)
    obj = GC_malloc_atomic(size);
  else
    obj = GC_malloc(size);

  return obj;
}
        

#ifdef OPERATOR_NEW_ARRAY
inline void *operator new[](size_t size, GCPlacement gcp)
{
  return ::operator new(size, gcp);
}
#endif


#endif /* WXGC_CPP_H */
