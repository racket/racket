/*
 * File:	wx_obj.h
 * Purpose:	Top level object for wxWindows
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_objh
#define wxb_objh

#include "common.h"

#ifdef IN_CPROTO
typedef       void    *wxObject ;
#else

#include "wxGC.h"

#ifdef MZ_PRECISE_GC
# define WXGC_IGNORE(base, ptr) GC_finalization_weak_ptr((void **)base, (void **)&(ptr) - (void **)base)
# define WXGC_ATOMIC /* empty */
# define COPYSTRING_TO_ALIGNED(s, d) copystring_to_aligned(s, d)
# define DELETE_OBJ delete_wxobject
# define DELETE_VAL delete
# define WRAP_SAFEREF(x) (void *)GC_malloc_immobile_box(GC_malloc_weak_box(gcOBJ_TO_PTR(x), NULL, 0))
# define FREE_SAFEREF(x) GC_free_immobile_box((void **)x)
typedef struct {
  short tag;
  short filler_used_for_hashing;
  void *val;
} wxWeak_Box;
# define GET_SAFEREF(x) ((*(void **)x) ? gcPTR_TO_OBJ((*(wxWeak_Box **)x)->val) : NULL)
#else
# define WXGC_IGNORE(base, ptr) GC_general_register_disappearing_link((void **)&(ptr), NULL)
# define WXGC_ATOMIC (AtomicGC)
# define COPYSTRING_TO_ALIGNED(s, d) (s + d)
# define DELETE_OBJ delete
# define DELETE_VAL delete
# define WRAP_SAFEREF(x) x
# define FREE_SAFEREF(x) /* empty */
# define GET_SAFEREF(x) x
#endif
#define WXGC_NO_CLEANUP FALSE

class wxObject : public gc_cleanup
{
  public:
  WXTYPE __type;
  wxObject(void);
  wxObject(Bool cleanup);
  wxObject(Bool cleanup, WXTYPE t);
  virtual ~wxObject(void);
};

#endif // IN_CPROTO
#endif // wx_objh
