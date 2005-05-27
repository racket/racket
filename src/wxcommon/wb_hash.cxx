/*
 * File:		wb_hash.cc
 * Purpose:	Hash table implementation
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#pragma implementation "wx_hash.h"

#if defined(_MSC_VER)
# include "wx.h"
#else
# ifdef wx_xt
#  define  Uses_wxHashTable
#  include "wx.h"
# else
#  include "common.h"
#  include "wx_list.h"
#  include "wx_hash.h"
#  include "wx_types.h"
# endif
#endif

#include <string.h>
#include <stdarg.h>

wxHashTable::wxHashTable (int, int size)
 : wxObject(WXGC_NO_CLEANUP)
{
  int i;
  wxList **ll;

  __type = wxTYPE_HASH_TABLE;
  n = size;
  current_position = -1;
  current_node = NULL;

  ll = new wxList *[size];
  hash_table = ll;
  for (i = 0; i < size; i++) {
    hash_table[i] = NULL;
  }
}

wxHashTable::~wxHashTable (void)
{
  int i;
  for (i = 0; i < n; i++) {
    if (hash_table[i]) {
      wxList *l;
      l = hash_table[i];
      DELETE_OBJ l;
    }
  }
}

wxList *wxHashTable::GetList(int position, KeyType ktype, Bool makeit)
{
  wxList *l;

  l = hash_table[position];

  if (!l) {
    if (makeit) {
      l = new wxList(ktype, FALSE);
      hash_table[position] = l;
    }
  }
  
  return l;
}

void wxHashTable::Put(long key, wxObject * object)
{
  wxList *l;

  l = GetList(MakeKey(key));

  l->Append(key, object);
}

void wxHashTable::Put(const char *key, wxObject * object)
{
  wxList *l;

  l = GetList(MakeKey(key), wxKEY_STRING);

  l->Append(key, object);
}

wxObject *wxHashTable::Get(long key)
{
  wxList *l;

  l = GetList(MakeKey(key), wxKEY_INTEGER, FALSE);

  if (l) {
    wxNode *node;
    node = l->Find(key);
    if (node)
      return node->Data();
  }

  return NULL;
}

wxObject *wxHashTable::Get(const char *key)
{
  wxList *l;

  l = GetList(MakeKey(key), wxKEY_STRING, FALSE);

  if (l) {
    wxNode *node;
    node = l->Find (key);
    if (node)
      return node->Data();
  }

  return NULL;
}

wxObject *wxHashTable::Delete(long key)
{
  wxList *l;

  l = GetList(MakeKey(key), wxKEY_INTEGER, FALSE);

  if (l) {
    wxNode *node;
    node = l->Find(key);
    if (node) {
      wxObject *data;
      data = node->Data();
      l->DeleteNode(node);
      return data;
    }
  }
  return NULL;
}

wxObject *wxHashTable::Delete(const char *key)
{
  wxList *l;

  l = GetList(MakeKey(key), wxKEY_STRING, FALSE);

  if (l) {
    wxNode *node;
    node = l->Find(key);
    if (node) {
      wxObject *data;
      data = node->Data();
      l->DeleteNode(node);
      return data;
    }
  }

  return NULL;
}

int wxHashTable::MakeKey(const char *string)
{
  long int_key = 0;

  while (*string) {
    int_key += (unsigned char) *string++;
  }

  if (int_key < 0)
    int_key = -int_key;

  return int_key % n;
}

int wxHashTable::MakeKey(long int_key)
{
  if (int_key < 0)
    int_key = -int_key;

  return int_key % n;
}

void wxHashTable::BeginFind (void)
{
  current_position = -1;
  current_node = NULL;
}

wxNode *wxHashTable::Next (void)
{
  wxNode *found = NULL;
  Bool end = FALSE;
  while (!end && !found) {
    if (!current_node) {
      current_position++;
      if (current_position >= n) {
	current_position = -1;
	current_node = NULL;
	end = TRUE;
      } else {
	wxList *l;
	l = hash_table[current_position];
	if (l) {
	  current_node = l->First();
	  found = current_node;
	}
      }
    } else {
      current_node = current_node->Next ();
      found = current_node;
    }
  }
  return found;
}

void wxHashTable::DeleteContents (Bool flag)
{
  int i;
  for (i = 0; i < n; i++) {
    if (hash_table[i]) {
      wxList *l;
      l = hash_table[i];
      l->DeleteContents(flag);
    }
  }
}

void wxHashTable::Clear (void)
{
  int i;
  for (i = 0; i < n; i++) {
    if (hash_table[i])  {
      wxList *l;
      l = hash_table[i];
      l->Clear();
    }
  }
}



/* This is a hash table implementation which does not lock the objects
   from garbage collection. */

#ifdef MZ_PRECISE_GC
typedef long *nlWidgetRef;
# define nl_malloc_bucket_array(size) GC_malloc(size)
# define nlGET_WIDGET(x) (*(long *)(x))
# define nlGET_OBJECT(x) (((wxObject **)(x))[1])
# define nlALLOC_WIDGET(w) { long *p; p = (long *)GC_malloc_atomic(sizeof(long)); w = p; }
# define nlALLOC_OBJECT(o) { void *p; p = GC_malloc_weak_box(NULL, NULL, 0); o = (wxObject *)p; }
#else
# define nl_malloc_bucket_array(size) GC_malloc_atomic(size)
typedef long nlWidgetRef;
# define nlGET_WIDGET(x) x
# define nlGET_OBJECT(x) x
# define nlALLOC_WIDGET(w) /* empty */
# define nlALLOC_OBJECT(w) /* empty */
#endif

typedef struct Bucket {
  nlWidgetRef widget;
  wxObject *object;
} Bucket;

/* because widgets are likely to be word-aligned */
#define HASH(w) ((((unsigned long)w) >> 2) % numbuckets)

#define FILL_FACTOR 2 /* inverted max fraction of hash table implying reash */

wxNonlockingHashTable::wxNonlockingHashTable()
{
  long i;
  Bucket *bs;

  numbuckets = 1001;
  bs = (Bucket *)nl_malloc_bucket_array(sizeof(Bucket) * numbuckets);
  buckets = bs;
  for (i = 0; i < numbuckets; i++) {
    buckets[i].widget = 0;
  }
  numwidgets = numused = 0;
}

wxNonlockingHashTable::~wxNonlockingHashTable()
{
}

void wxNonlockingHashTable::Put(long widget, wxObject *object)
{
  long i;

  if (FILL_FACTOR * numused >= numbuckets) {
    /* Rehash */
    Bucket *oldbuckets = buckets, *bs;
    long oldnumbuckets = numbuckets;

    if (FILL_FACTOR * numwidgets >= numbuckets)
      numbuckets = (numbuckets * FILL_FACTOR) + 1;
    /* else, just need to rehash after many deletions */

    bs = (Bucket *)nl_malloc_bucket_array(sizeof(Bucket) * numbuckets);
    buckets = bs;
    for (i = 0; i < numbuckets; i++) {
      buckets[i].widget = 0;
    }

    numwidgets = numused = 0;
    for (i = 0; i < oldnumbuckets; i++) {
      if (oldbuckets[i].widget && oldbuckets[i].object)
	Put(nlGET_WIDGET(oldbuckets[i].widget), nlGET_OBJECT(oldbuckets[i].object));
    }
  }

  i = HASH(widget);
  while (buckets[i].widget && buckets[i].object
	 && (nlGET_WIDGET(buckets[i].widget) != widget)) {
    i = (i + 1) % numbuckets;
  }
  if (!buckets[i].widget)
    numused++;
  nlALLOC_WIDGET(buckets[i].widget);
  nlGET_WIDGET(buckets[i].widget) = widget;
  nlALLOC_OBJECT(buckets[i].object);
  nlGET_OBJECT(buckets[i].object) = object;
  numwidgets++;
}

wxObject *wxNonlockingHashTable::Get(long widget)
{
  long i;

  i = HASH(widget);
  while (buckets[i].widget && (nlGET_WIDGET(buckets[i].widget) != widget)) {
    i = (i + 1) % numbuckets;
  }

  if (buckets[i].widget 
      && (nlGET_WIDGET(buckets[i].widget) == widget)
      && buckets[i].object) {
    wxObject *r;
    r = nlGET_OBJECT(buckets[i].object);
    return r;
  }

  return NULL;
}

void wxNonlockingHashTable::Delete(long widget)
{
  long i;

  i = HASH(widget);
  while (buckets[i].widget && (nlGET_WIDGET(buckets[i].widget) != widget)) {
    i = (i + 1) % numbuckets;
  }

  if (buckets[i].widget && (nlGET_WIDGET(buckets[i].widget) == widget)) {
    buckets[i].object = NULL;
    --numwidgets;
    /* Don't decrement numused, since the widget half is still set;
       we should re-hash after enough deletions */
  }
}

/* not particularly fast... */
void wxNonlockingHashTable::DeleteObject(wxObject *o)
{
  long i;
  
  for (i = 0; i < numbuckets; i++) {
    if (buckets[i].widget && buckets[i].object && nlGET_OBJECT(buckets[i].object) == o)
      Delete(nlGET_WIDGET(buckets[i].widget));
  }
}

