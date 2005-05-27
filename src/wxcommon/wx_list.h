/*								-*- C++ -*-
 * File:		wx_list.h
 * Purpose:	wxList implementation much used in wxWindows
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_listh
#define wxb_listh

#ifdef __GNUG__
# ifndef wx_mac
#  pragma interface
# endif
#endif

#ifndef wx_xt
    // wxWindows standard include mechanism
    /* sccsid[] = "@(#)wx_list.h	1.2 5/9/94" */
#   include "common.h"
#   include "wx_obj.h"
#endif

#ifdef IN_CPROTO
typedef       void    *wxList ;
typedef       void    *wxNode;
typedef       void    *wxStringList;
#else

class wxList;

enum KeyType {wxKEY_NONE = 0, wxKEY_INTEGER, wxKEY_STRING};

class wxNode
{
 private:
 
  wxObject *data;
  wxNode *next;
  wxNode *previous;

  void Setup(wxNode *last_one, wxNode *next_one, 
	     wxObject *object);

 public:
  // Optional key stuff
  long integer_key;
  char *string_key;

  wxNode(wxNode *last_one = NULL, wxNode *next_one = NULL,
	 wxObject *object = NULL);
  wxNode(wxNode *last_one, wxNode *next_one,
         wxObject *object, long the_key);
  wxNode(wxNode *last_one, wxNode *next_one,
         wxObject *object, const char *the_key);
  wxNode(wxNode *last_one, wxNode *next_one,
         wxObject *object, void *the_key);

  void Kill(wxList *list);

  inline wxNode *Next(void) { return next; }
  inline wxNode *Previous(void) { return previous; }
  inline wxObject *Data(void) { return data; }
  inline void SetData(wxObject *the_data) { data = the_data; }

  inline void     DataGCIgnored(void)         { WXGC_IGNORE(this, data); }
};

class wxList: public wxObject
{

 public:
#ifdef wx_mac
   enum DestroyDataCode {kNoDestroyData = 0, kDestroyData = 1};
#endif
  int n;
  int destroy_data;
  wxNode *first_node;
  wxNode *last_node;
  unsigned int key_type;

#ifdef wx_mac
  wxList(DestroyDataCode destroyData = kNoDestroyData, Bool clean_up = TRUE);
#else
  wxList(void);
#endif
  wxList(KeyType the_key_type, Bool clean_up = TRUE);
  wxList(int N, wxObject *Objects[]);
  ~wxList(void);

  inline int Number(void) { return n; }

  // Append to end of list
  wxNode *Append(wxObject *object);

  // Insert at front of list
  wxNode *Insert(wxObject *object);

  // Insert before given node
  wxNode *Insert(wxNode *position, wxObject *object);

  // Keyed append
  wxNode *Append(long key, wxObject *object);
  wxNode *Append(const char *key, wxObject *object);
  wxNode *Append(void *key, wxObject *object);

  Bool DeleteNode(wxNode *node);
  Bool DeleteObject(wxObject *object);  // Finds object pointer and
                                        // deletes node (and object if
                                        // DeleteContents is on)
  void Clear(void);                     // Delete all nodes

#ifdef wx_mac
  Bool OnDeleteObject(wxObject *object); // mac platform only
  long MemberIndex(wxObject *object); // WCH wx_mac added 8/12/94
#endif

  inline wxNode *First(void) { return first_node; }
  inline wxNode *Last(void) { return last_node; }
  wxNode *Nth(int i);                  // nth node counting from 0

  // Keyed search
  wxNode *Find(long key);
  wxNode *Find(const char *key);
  wxNode *FindPtr(void *key);

  wxNode *Member(wxObject *object);

  inline void DeleteContents(int destroy) { destroy_data = destroy; }
                                             // Instruct it to destroy user data
                                             // when deleting nodes

#ifdef MEMORY_USE_METHOD
  long MemoryUse(void);
#endif

 private:
   wxNode *DoAppend(wxNode *node);
};

// String list class. N.B. this always copies strings
// with Add and deletes them itself.
class wxStringList: public wxList
{

 public:
  wxStringList(void);
  ~wxStringList(void);

  wxNode *Add(const char *s);
  void Delete(const char *s);
  char **ListToArray(Bool new_copies = FALSE);
  Bool Member(const char *s);
};

class wxChildList;

class wxChildNode
{
public:
  wxChildList *owner;
  wxObject *strong;
#ifdef MZ_PRECISE_GC
  void *weak;
# define cnGET_WEAK(weak) ((wxObject *)gcPTR_TO_OBJ(GC_weak_box_val(weak)))
#else
  wxObject **weak; /* atomic-allocated disappearing ptr */
# define cnGET_WEAK(weak) (*weak)
#endif

  wxChildNode *Next();
  wxObject *Data();
  Bool IsShown();
};

class wxChildList
{
 public:
  int n;
  int size;
  wxChildNode **nodes;

  wxChildList(void);
  ~wxChildList(void);

  inline int Number(void) { return n; }

  // Append to end of list
  void Append(wxObject *object);

  Bool DeleteObject(wxObject *object);
  Bool DeleteNode(wxChildNode *node);

  inline wxChildNode *First(void) { return FindNode(NULL); }
  
  wxChildNode *FindNode(wxChildNode *after);
  wxChildNode *NextNode(int &pos);

  void Show(wxObject *object, Bool show);

  Bool IsShown(wxObject *object);
};

#endif // IN_CPROTO
#endif // wxb_listh
