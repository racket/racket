/*								-*- C++ -*-
 * File:		wx_hash.h
 * Purpose:	Basic hash table implementation
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_hashh
#define wxb_hashh

#ifdef __GNUG__
# ifndef wx_mac
#  pragma interface
# endif
#endif

#ifndef wx_xt
#   include "wx_obj.h"
#   include "wx_list.h"
#endif

/*
 * A hash table is an array of user-definable size with lists
 * of data items hanging off the array positions.  Usually there'll
 * be a hit, so no search is required; otherwise we'll have to run down
 * the list to find the desired item.
*/

#ifdef IN_CPROTO
typedef       void    *wxHashTable ;
#else

class wxHashTable: public wxObject
{

 public:
  int n;
  int current_position;
  wxNode *current_node;

  wxList **hash_table;

  wxHashTable(int the_key_type = 0, int size = 1000);
  ~wxHashTable(void);

  // key and value are the same
  void Put(long value, wxObject *object);
  void Put(const char *value, wxObject *object);

  // key and value are the same
  wxObject *Get(long value);
  wxObject *Get(const char *value);

  // Deletes entry and returns data if found
  wxObject *Delete(long key);
  wxObject *Delete(const char *key);

  // Construct your own integer key from a string, e.g. in case
  // you need to combine it with something
  int MakeKey(const char *string);
  int MakeKey(long val);

  // Way of iterating through whole hash table (e.g. to delete everything)
  // Not necessary, of course, if you're only storing pointers to
  // objects maintained separately

  void BeginFind(void);
  wxNode *Next(void);

  void DeleteContents(Bool flag);
  void Clear(void);

private:
  wxList *GetList(int position, KeyType ktype = wxKEY_INTEGER, Bool makeit = TRUE);
};

/* Special hash table implementation for widgets. */
class wxNonlockingHashTable
{
  struct Bucket *buckets;
  long numbuckets, numwidgets, numused;
 public:
  wxNonlockingHashTable(void);
  ~wxNonlockingHashTable();
  void Put(long widget, wxObject *object);
  wxObject *Get(long widget);
  void Delete(long widget);
  void DeleteObject(wxObject *object);
  inline void Append(long w, wxObject *o) { Put(w, o); }
  inline wxObject *Find(long w) { return Get(w); }
};

#endif // IN_CPROTO
#endif // wxb_hashh
