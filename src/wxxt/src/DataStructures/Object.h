/*								-*- C++ -*-
 *
 * Purpose: Top level object and memory debugging for wxWindows
 *
 * Authors: Markus Holzem, Julian Smart and Arthur Seaton
 *
 * Copyright: (C) 2004-2005 PLT Scheme, Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian, Arthur)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef Object_h
#define Object_h

#ifndef MZ_PRECISE_GC
# ifdef __GNUG__
# pragma interface
# endif
#endif

//-----------------------------------------------------------------------------
// wxObject: top level object
//-----------------------------------------------------------------------------

#ifdef MZ_PRECISE_GC
# define WXGC_IGNORE(base, ptr) GC_finalization_weak_ptr((void **)base, (void **)&(ptr) - (void **)base)
# define WXGC_ATOMIC /* empty */
# define COPYSTRING_TO_ALIGNED(s, d) copystring_to_aligned(s, d)
# define DELETE_OBJ delete_wxobject
# define DELETE_VAL delete
# define MALLOC_SAFEREF() (void *)GC_malloc_immobile_box(GC_malloc_weak_box(NULL, NULL, 0))
# define FREE_SAFEREF(x) GC_free_immobile_box((void **)x)
typedef struct {
  short tag;
  short filler_used_for_hashing;
  void *val;
} wxWeak_Box;
# define SET_SAFEREF(x, v) (*(wxWeak_Box **)x)->val = gcOBJ_TO_PTR(v)
# define GET_SAFEREF(x) ((*(void **)x) ? gcPTR_TO_OBJ((*(wxWeak_Box **)x)->val) : NULL)
#else
# define WXGC_IGNORE(base, ptr) GC_general_register_disappearing_link((void **)&(ptr), NULL)
# define WXGC_ATOMIC (AtomicGC)
# define COPYSTRING_TO_ALIGNED(s, d) (s + d)
# define DELETE_OBJ delete
# define DELETE_VAL delete
# define MALLOC_SAFEREF() malloc(sizeof(void *))
# define FREE_SAFEREF(x) free(x)
# define SET_SAFEREF(x, v) (*(void **)x) = v
# define GET_SAFEREF(x) (*(void **)x)
#endif
#define WXGC_NO_CLEANUP FALSE

#if SGC_STD_DEBUGGING
# define MEMORY_USE_METHOD
#endif

class wxObject : public gc_cleanup
{
public:
  wxObject(void);
  wxObject(Bool cleanup);
  virtual ~wxObject(void);
  
  WXTYPE __type;
  
#ifdef MEMORY_USE_METHOD
  virtual long MemoryUse(void);
#endif
};


#define wxASSERT(ignore1, ignore2) ((void) 0)

#define DEBUG_NEW new

#endif // Object_h
