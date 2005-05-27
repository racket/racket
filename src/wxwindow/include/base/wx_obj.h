/*
 * File:	wx_obj.h
 * Purpose:	Top level object for wxWindows
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_objh
#define wxb_objh

#include "common.h"

/* Even with normal GC, MrEd for Windows registers globals: */
#define WX_REGISTER_GLOBAL_MEMORY

#include "../../../wxcommon/wxGC.h"

#ifndef WXGC_CLEANUP_CLASS
# define WXGC_CLEANUP_CLASS gc_cleanup
#endif

#ifdef MZ_PRECISE_GC
# define WXGC_IGNORE(base, ptr) GC_finalization_weak_ptr((void **)base, (void **)&(ptr) - (void **)base)
# define WXGC_ATOMIC /* empty */
# define COPYSTRING_TO_ALIGNED(s, d) copystring(s, d)
# define DELETE_OBJ delete_wxobject
# define DELETE_VAL delete
#else
# define WXGC_IGNORE(base, ptr) GC_general_register_disappearing_link((void **)&(ptr), NULL)
# define WXGC_ATOMIC (AtomicGC)
# define COPYSTRING_TO_ALIGNED(s, d) (s + d)
# define DELETE_OBJ delete
# define DELETE_VAL delete
#endif

#define WXGC_NO_CLEANUP FALSE

class wxObject : public WXGC_CLEANUP_CLASS
{
 public:
  WXTYPE __type;

  wxObject(void);
  wxObject(Bool cleanup);
  virtual ~wxObject(void);
};


#endif // wx_objh
