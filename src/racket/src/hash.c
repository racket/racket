/*
  Racket
  Copyright (c) 2004-2010 PLT Scheme Inc.
  Copyright (c) 1995-2001 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"
#include "schmach.h"
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "../gc2/gc2_obj.h"

THREAD_LOCAL_DECL(intptr_t scheme_hash_request_count);
THREAD_LOCAL_DECL(intptr_t scheme_hash_iteration_count);

READ_ONLY static Scheme_Object GONE[1];

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#ifdef MZ_PRECISE_GC
/* keygen race conditions below are ok, because keygen is randomness used
to create a hashkey.  Setting a hashkey on a Scheme_Object however, may 
lead to race conditions */

FIXME_LATER static intptr_t keygen;
XFORM_NONGCING static MZ_INLINE
intptr_t PTR_TO_LONG(Scheme_Object *o)
{
  intptr_t bits;
  short v;

  if (SCHEME_INTP(o))
    return (intptr_t)o;

  v = o->keyex;

  if (!(v & 0xFFFC)) {
    intptr_t local_keygen = keygen;
    v |= (short)local_keygen;
#ifdef OBJHEAD_HAS_HASH_BITS
    /* In 3m mode, we only have 14 bits of hash code in the
       Scheme_Object header. But the GC-level object header has some
       leftover bits (currently 9, 11, 41, or 43, depending on the
       platform), so use those, too. That only works for GCable
       objects, so we use 1 of our 14 bits to indicate whether the
       other bit are present. */
    if (GC_is_allocated(o)) {
      OBJHEAD_HASH_BITS(o) = (local_keygen >> 16);
      v |= 0x4000;
    } else
      v &= ~0x4000;
#endif
    if (!v) v = 0x1AD0;
    o->keyex = v;
    keygen += 4;
  }

#ifdef OBJHEAD_HAS_HASH_BITS
  if (v & 0x4000)
    bits = OBJHEAD_HASH_BITS(o);
  else
#endif
    bits = o->type;

  /* Note: low two bits will be ignored */
  return (bits << 16) | (v & 0xFFFF);
}
#else
# define PTR_TO_LONG(p) ((intptr_t)(p))
#endif

#define FILL_FACTOR 1.4

#define MIN_HTABLE_SIZE 8

typedef int (*Hash_Compare_Proc)(void*, void*);

typedef uintptr_t hash_v_t;

#define MAX_HASH_DEPTH 128

/*========================================================================*/
/*                         hashing functions                              */
/*========================================================================*/

static void string_hash_indices(void *_key, intptr_t *_h, intptr_t *_h2)
{
  const char *key = (char *)_key;
  intptr_t i, h, h2;

  h2 = h = i = 0;
  while (key[i]) {
    int c = key[i++];
    h += (h << 5) + h + c;
    h2 += c;
  }

  if (_h)
    *_h = h;
  if (_h2)
    *_h2 = h2;
}

static void id_hash_indices(void *_key, intptr_t *_h, intptr_t *_h2)
{
  Scheme_Object *key = (Scheme_Object *)_key;
  intptr_t lkey;

  if (SCHEME_STXP(key))
    key = SCHEME_STX_VAL(key);
    
  lkey = PTR_TO_LONG((Scheme_Object *)key);
  if (_h)
    *_h = (lkey >> 2);
  if (_h2)
    *_h2 = (lkey >> 3);
}

static int not_stx_bound_eq(char *a, char *b)
{
  return !scheme_stx_bound_eq((Scheme_Object *)a, (Scheme_Object *)b, 0);
}

/*========================================================================*/
/*                         normal hash table                              */
/*========================================================================*/


Scheme_Hash_Table *scheme_make_hash_table(int type)
{
  Scheme_Hash_Table *table;

  table = MALLOC_ONE_TAGGED(Scheme_Hash_Table);

  table->size = 0;
    
  table->iso.so.type = scheme_hash_table_type;

  if (type == SCHEME_hash_string) {
    table->make_hash_indices = string_hash_indices;
    table->compare = (Hash_Compare_Proc)strcmp;
  }
  if (type == SCHEME_hash_bound_id) {
    table->make_hash_indices = id_hash_indices;
    table->compare = (Hash_Compare_Proc)not_stx_bound_eq;
  }

  return table;
}

static Scheme_Object *do_hash(Scheme_Hash_Table *table, Scheme_Object *key, int set, Scheme_Object *val)
{
  Scheme_Object *tkey, **keys;
  hash_v_t h, h2, useme = 0;
  uintptr_t mask;

 rehash_key:

  mask = table->size - 1;

  if (table->make_hash_indices) {
    GC_CAN_IGNORE hash_v_t *_h2;
    if (table->compare) {
      h2 = 0;
      _h2 = NULL;
    } else
      _h2 = &h2;
    table->make_hash_indices((void *)key, (intptr_t *)&h, (intptr_t *)_h2);
    h = h & mask;
    if (_h2) {
      h2 = (h2 & mask) | 1;
    }
  } else {
    uintptr_t lkey;
    lkey = (uintptr_t)PTR_TO_LONG((Scheme_Object *)key);
    h = (lkey >> 2) & mask;
    h2 = ((lkey >> 3) & mask) | 1;
  }

  keys = table->keys;
  
  if (table->compare) {
    scheme_hash_request_count++;
    while ((tkey = keys[h])) {
      if (SAME_PTR(tkey, GONE)) {
	if (set > 1) {
	  useme = h;
	  set = 1;
	}
      } else if (!table->compare(tkey, (char *)key)) {
	if (set) {
	  table->vals[h] = val;
	  if (!val) {
	    keys[h] = GONE;
	    --table->count;
	  }
	  return val;
	} else
	  return table->vals[h];
      }
      scheme_hash_iteration_count++;
      if (!h2) {
        table->make_hash_indices((void *)key, NULL, (intptr_t *)&h2);
        h2 = (h2 & (table->size - 1)) | 1;
      }
      h = (h + h2) & mask;
    }
  } else {
    scheme_hash_request_count++;
    while ((tkey = keys[h])) {
      if (SAME_PTR(tkey, key)) {
	if (set) {
	  table->vals[h] = val;
	  if (!val) {
	    keys[h] = GONE;
	    --table->count;
	  }
	  return val;
	} else
	  return table->vals[h];
      } else if (SAME_PTR(tkey, GONE)) {
	if (set > 1) {
	  useme = h;
	  set = 1;
	}
      } 
      scheme_hash_iteration_count++;
      h = (h + h2) & mask;
    }
  }

  if (!set || !val)
    return NULL;

  if (set == 1)
    h = useme;
  else if (table->mcount * FILL_FACTOR >= table->size) {
    /* Rehash */
    int i, oldsize = table->size, size;
    Scheme_Object **oldkeys = table->keys;
    Scheme_Object **oldvals = table->vals;

    size = oldsize << 1;
    table->size = size;
    
    {
      Scheme_Object **ba;
      ba = MALLOC_N(Scheme_Object *, size);
      table->vals = ba;
      ba = MALLOC_N(Scheme_Object *, size);
      table->keys = ba;
    }

    table->count = 0;
    table->mcount = 0;
    for (i = 0; i < oldsize; i++) {
      if (oldkeys[i] && !SAME_PTR(oldkeys[i], GONE))
	do_hash(table, oldkeys[i], 2, oldvals[i]);
    }

    goto rehash_key;
  } else {
    table->mcount++;
  }

  table->count++;
  table->keys[h] = key;
  table->vals[h] = val;

  return val;
}

static Scheme_Object *do_hash_set(Scheme_Hash_Table *table, Scheme_Object *key, Scheme_Object *val)
{
  Scheme_Object *tkey, **keys;
  hash_v_t h, h2, useme = 0;
  uintptr_t mask;
  uintptr_t lkey;
  int set = 2;

  mask = table->size - 1;

  lkey = (uintptr_t)PTR_TO_LONG((Scheme_Object *)key);
  h = (lkey >> 2) & mask;
  h2 = (lkey >> 3) & mask;

  h2 |= 1;

  keys = table->keys;
  
  scheme_hash_request_count++;
  while ((tkey = keys[h])) {
    if (SAME_PTR(tkey, key)) {
      table->vals[h] = val;
      if (!val) {
	keys[h] = GONE;
	--table->count;
      }
      return val;
    } else if (SAME_PTR(tkey, GONE)) {
      if (set > 1) {
	useme = h;
	set = 1;
      }
    } 
    scheme_hash_iteration_count++;
    h = (h + h2) & mask;
  }

  if (!val)
    return NULL;

  if (set == 1)
    h = useme;
  else if (table->mcount * FILL_FACTOR >= table->size) {
    /* Use slow path to grow table: */
    return do_hash(table, key, 2, val);
  } else {
    table->mcount++;
  }

  table->count++;
  table->keys[h] = key;
  table->vals[h] = val;

  return val;
}

XFORM_NONGCING static Scheme_Object *do_hash_get(Scheme_Hash_Table *table, Scheme_Object *key)
{
  Scheme_Object *tkey, **keys;
  hash_v_t h, h2;
  uintptr_t mask;
  uintptr_t lkey;

  mask = table->size - 1;

  lkey = (uintptr_t)PTR_TO_LONG((Scheme_Object *)key);
  h = (lkey >> 2) & mask;
  h2 = (lkey >> 3) & mask;

  h2 |= 1;

  keys = table->keys;
  
  scheme_hash_request_count++;
  while ((tkey = keys[h])) {
    if (SAME_PTR(tkey, key)) {
      return table->vals[h];
    } 
    scheme_hash_iteration_count++;
    h = (h + h2) & mask;
  }

  return NULL;
}

void scheme_hash_set(Scheme_Hash_Table *table, Scheme_Object *key, Scheme_Object *val)
{
  if (!table->vals) {
    Scheme_Object **ba;
    
    table->size = 8;
    
    ba = MALLOC_N(Scheme_Object *, table->size);
    table->vals = ba;
    ba = MALLOC_N(Scheme_Object *, table->size);
    table->keys = ba;
  }

  if (table->make_hash_indices)
    do_hash(table, key, 2, val);
  else
    do_hash_set(table, key, val);
}

Scheme_Object *scheme_hash_get(Scheme_Hash_Table *table, Scheme_Object *key)
{
  if (!table->vals)
    return NULL;
  else if (table->make_hash_indices)
    return do_hash(table, key, 0, NULL);
  else
    return do_hash_get(table, key);
}

Scheme_Object *scheme_eq_hash_get(Scheme_Hash_Table *table, Scheme_Object *key)
/* Specialized to allow XFORM_NONGCING */
{
  if (!table->vals)
    return NULL;
  else
    return do_hash_get(table, key);
}

Scheme_Object *scheme_hash_get_atomic(Scheme_Hash_Table *table, Scheme_Object *key)
/* Mostly useful for acessing equal-based hash table when you don't want
   thread switches (such as in stx object manipulations). Simply grabbing the
   table's lock would be enough to make access to the table single-threaded,
   but sometimes you don't want any thread switches at all. */
{
  Scheme_Object *r;
  scheme_start_atomic();
  r = scheme_hash_get(table, key);
  scheme_end_atomic_no_swap();
  return r;
}

void scheme_hash_set_atomic(Scheme_Hash_Table *table, Scheme_Object *key, Scheme_Object *val)
/* See rationale with scheme_hash_get_atomic. */
{
  scheme_start_atomic();
  scheme_hash_set(table, key, val);
  scheme_end_atomic_no_swap();
}

int scheme_hash_table_equal_rec(Scheme_Hash_Table *t1, Scheme_Hash_Table *t2, void *eql)
{
  Scheme_Object **vals, **keys, *v;
  int i;

  if ((t1->count != t2->count)
      || (t1->make_hash_indices != t2->make_hash_indices)
      || (t1->compare != t2->compare))
    return 0;
    
  keys = t1->keys;
  vals = t1->vals;
  for (i = t1->size; i--; ) {
    if (vals[i]) {
      v = scheme_hash_get(t2, keys[i]);
      if (!v)
	return 0;
      if (!scheme_recur_equal(vals[i], v, eql))
	return 0;
    }
  }

  return 1;
}

int scheme_hash_table_equal(Scheme_Hash_Table *t1, Scheme_Hash_Table *t2)
{
  return scheme_equal((Scheme_Object *)t1, (Scheme_Object *)t2);
}

Scheme_Hash_Table *scheme_clone_hash_table(Scheme_Hash_Table *ht)
{
  Scheme_Hash_Table *table;
  Scheme_Object **ba;

  table = MALLOC_ONE_TAGGED(Scheme_Hash_Table);
  memcpy(table, ht, sizeof(Scheme_Hash_Table));
  MZ_OPT_HASH_KEY(&(table->iso)) = 0;

  if (table->size) {
    ba = MALLOC_N(Scheme_Object *, table->size);
    memcpy(ba, table->vals, sizeof(Scheme_Object *) * table->size);
    table->vals = ba;
    ba = MALLOC_N(Scheme_Object *, table->size);
    memcpy(ba, table->keys, sizeof(Scheme_Object *) * table->size);
    table->keys = ba;
  }

  if (table->mutex) {
    Scheme_Object *sema;
    sema = scheme_make_sema(1);
    table->mutex = sema;
  }

  return table;
}

void scheme_reset_hash_table(Scheme_Hash_Table *table, int *history)
{
  if ((table->size <= 8)
      || (table->count * FILL_FACTOR > (table->size >> 1))) {
    /* Keep same size */
  } else {
    /* Shrink by one step */
    Scheme_Object **ba;
    table->size >>= 1;
    ba = MALLOC_N(Scheme_Object *, table->size);
    table->vals = ba;
    ba = MALLOC_N(Scheme_Object *, table->size);
    table->keys = ba;
  }
  memset(table->vals, 0, sizeof(Scheme_Object *) * table->size);
  memset(table->keys, 0, sizeof(Scheme_Object *) * table->size);
  table->count = 0;
  table->mcount = 0;
}

/*========================================================================*/
/*                  old-style hash table, with buckets                    */
/*========================================================================*/

Scheme_Bucket_Table *
scheme_make_bucket_table (intptr_t size, int type)
{
  Scheme_Bucket_Table *table;
  size_t asize;

  table = MALLOC_ONE_TAGGED(Scheme_Bucket_Table);

  table->size = 4;
  while (table->size < size) {
    table->size <<= 1;
  }

  table->count = 0;

  table->so.type = scheme_bucket_table_type;

  asize = (size_t)table->size * sizeof(Scheme_Bucket *);
  {
    Scheme_Bucket **ba;
    ba = (Scheme_Bucket **)scheme_malloc(asize);
    table->buckets = ba;
  }

  if (type == SCHEME_hash_weak_ptr)
    table->weak = 1;
  else if (type == SCHEME_hash_late_weak_ptr)
    table->weak = 2;
  else
    table->weak = 0;
  
  return table;
}

Scheme_Bucket_Table *scheme_clone_bucket_table(Scheme_Bucket_Table *bt)
{
  Scheme_Bucket_Table *table;
  size_t asize;

  table = MALLOC_ONE_TAGGED(Scheme_Bucket_Table);
  table->so.type = scheme_bucket_table_type;
  table->size = bt->size;
  table->count = bt->count;
  table->weak = bt->weak;
  table->with_home = 0;
  table->make_hash_indices = bt->make_hash_indices;
  table->compare = bt->compare;
  if (bt->mutex) {
    Scheme_Object *sema;
    sema = scheme_make_sema(1);
    table->mutex = sema;
  }
  {
    Scheme_Bucket **ba;
    asize = (size_t)table->size * sizeof(Scheme_Bucket *);
    ba = (Scheme_Bucket **)scheme_malloc(asize);
    table->buckets = ba;
    memcpy(ba, bt->buckets, asize);
  }

  return table;
}

static Scheme_Bucket *
get_bucket (Scheme_Bucket_Table *table, const char *key, int add, Scheme_Bucket *b)
{
  hash_v_t h, h2;
  Scheme_Bucket *bucket;
  Compare_Proc compare = table->compare;
  uintptr_t mask;

 rehash_key:

  mask = table->size - 1;

  if (table->make_hash_indices) {
    table->make_hash_indices((void *)key, (intptr_t *)&h, (intptr_t *)&h2);
    h = h & mask;
    h2 = h2 & mask;
  } else {
    uintptr_t lkey;
    lkey = (uintptr_t)PTR_TO_LONG((Scheme_Object *)key);
    h = (lkey >> 2) & mask;
    h2 = (lkey >> 3) & mask;
  }

  h2 |= 0x1;

  if (table->weak) {
    int reuse_bucket = 0;
    scheme_hash_request_count++;
    while ((bucket = table->buckets[h])) {
      if (bucket->key) {
	void *hk = (void *)HT_EXTRACT_WEAK(bucket->key);
	if (!hk) {
          if (!reuse_bucket)
            reuse_bucket = h + 1;
	} else if (SAME_PTR(hk, key))
	  return bucket;
	else if (compare && !compare((void *)hk, (void *)key))
	  return bucket;
      } else if (add)
	break;
      scheme_hash_iteration_count++;
      h = (h + h2) & mask;
    }

    if (reuse_bucket && add) {
      /* Re-use a bucket slot whose key is collected: */
      /* DON'T increment counter overall... */
      h = reuse_bucket - 1;
      --table->count;
    }
  } else {
    scheme_hash_request_count++;
    while ((bucket = table->buckets[h])) {
      if (SAME_PTR(bucket->key, key))
	return bucket;
      else if (compare && !compare((void *)bucket->key, (void *)key))
	return bucket;
      scheme_hash_iteration_count++;
      h = (h + h2) & mask;
    }
  }

  if (!add)
    return NULL;

  if (table->count * FILL_FACTOR >= table->size) {
    /* Rehash */
    int i, oldsize = table->size;
    size_t asize;
    Scheme_Bucket **old = table->buckets;

    if (table->weak && (table->size > 4096)) {
      int actual = 0;

      /* It might be nice to force a GC so that the new table is
         as small as possible, but that's too expensive. */
      /* scheme_collect_garbage(); */

      /* Check actual count: */
      for (i = 0; i < oldsize; i++) {
	if (old[i] && old[i]->key && HT_EXTRACT_WEAK(old[i]->key)) {
	  actual++;
	}
      }

      if (actual * FILL_FACTOR < table->count) {
	/* Decrement size so that the table won't actually grow. */
	table->size >>= 1;
      }
    }

    table->size <<= 1;
    
    asize = (size_t)table->size * sizeof(Scheme_Bucket *);
    {
      Scheme_Bucket **ba;
      ba = (Scheme_Bucket **)scheme_malloc(asize);
      table->buckets = ba;
    }

    table->count = 0;
    if (table->weak) {
      for (i = 0; i < oldsize; i++) {
	if (old[i] && old[i]->key && HT_EXTRACT_WEAK(old[i]->key))
	  get_bucket(table, (char *)HT_EXTRACT_WEAK(old[i]->key), 1, old[i]);
      }
    } else {
      for (i = 0; i < oldsize; i++) {
	if (old[i] && old[i]->key)
	  get_bucket(table, old[i]->key, 1, old[i]);
      }
    }

    goto rehash_key;
  }

  if (b) {
    bucket = b;
  } else {
    size_t bsize;
    Scheme_Type type;

    if (table->with_home) {
      bsize = sizeof(Scheme_Bucket_With_Home);
      type = scheme_variable_type;
    } else  {
      bsize = sizeof(Scheme_Bucket);
      type = scheme_bucket_type;
    }

    bucket = (Scheme_Bucket *)scheme_malloc_tagged(bsize);

    bucket->so.type = type;

    if (type == scheme_variable_type)
      ((Scheme_Bucket_With_Flags *)bucket)->flags = GLOB_HAS_HOME_PTR;

    if (table->weak) {
#ifdef MZ_PRECISE_GC
      void *kb;
      kb = GC_malloc_weak_box((void *)key, (void **)bucket, (void **)&bucket->val - (void **)bucket, 
                              (table->weak > 1));
      bucket->key = (char *)kb;
#else
      char *kb;
      kb = (char *)MALLOC_ONE_WEAK(void *);
      bucket->key = kb;
      *(void **)bucket->key = (void *)key;
      if (table->weak > 1) {
        scheme_late_weak_reference_indirect((void **)bucket->key, (void *)key);
        scheme_late_weak_reference_indirect((void **)&bucket->val, (void *)key);
      } else {
        scheme_weak_reference_indirect((void **)bucket->key, (void *)key);
        scheme_weak_reference_indirect((void **)&bucket->val, (void *)key);
      }
#endif
    } else
      bucket->key = (char *)key;
    bucket->val = NULL;
  }

  table->buckets[h] = bucket;

  table->count++;

  return bucket;
}

Scheme_Bucket *
scheme_bucket_or_null_from_table (Scheme_Bucket_Table *table, const char *key, int add)
{
  Scheme_Bucket *b;

  b = get_bucket(table, key, add, NULL);

  return b;
}

Scheme_Bucket *
scheme_bucket_from_table (Scheme_Bucket_Table *table, const char *key)
{
  return scheme_bucket_or_null_from_table(table, key, 1);
}

void 
scheme_add_to_table (Scheme_Bucket_Table *table, const char *key, void *val, 
		     int constant)
{
  Scheme_Bucket *b;

  b = get_bucket(table, key, 1, NULL);

  if (val)
    b->val = val;
  if (constant && table->with_home)
    ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_IS_CONST;
}

void scheme_add_bucket_to_table(Scheme_Bucket_Table *table, Scheme_Bucket *b)
{
  get_bucket(table, table->weak ? (char *)HT_EXTRACT_WEAK(b->key) : b->key, 1, b);
}

void *
scheme_lookup_in_table (Scheme_Bucket_Table *table, const char *key)
{
  Scheme_Bucket *bucket;

  bucket = get_bucket(table, key, 0, NULL);

  if (bucket)
    return bucket->val;
  else
    return NULL;
}

void
scheme_change_in_table (Scheme_Bucket_Table *table, const char *key, void *naya)
{
  Scheme_Bucket *bucket;

  bucket = get_bucket(table, key, 0, NULL);

  if (bucket)
    bucket->val = naya;
}

int scheme_bucket_table_equal_rec(Scheme_Bucket_Table *t1, Scheme_Bucket_Table *t2, void *eql)
{
  Scheme_Bucket **buckets, *bucket;
  void *v;
  const char *key;
  int i, weak, checked = 0;

  /* We can't compare the count values, because they're merely
     >= the number of mapped keys */

  if ((t1->weak != t2->weak)
      || (t1->make_hash_indices != t2->make_hash_indices)
      || (t1->compare != t2->compare))
    return 0;
  
  buckets = t1->buckets;
  weak = t1->weak;

  for (i = t1->size; i--; ) {
    bucket = buckets[i];
    if (bucket) {
      if (weak) {
	key = (const char *)HT_EXTRACT_WEAK(bucket->key);
      } else {
	key = bucket->key;
      }
      if (key) {
	checked++;
	v = scheme_lookup_in_table(t2, key);
	if (!v)
	  return 0;
	if (!scheme_recur_equal((Scheme_Object *)bucket->val, (Scheme_Object *)v, eql))
	  return 0;
      }
    }
  }

  /* If count is checked, then all buckets must be for mapped keys. */
  if (t2->count == checked)
    return 1;

  /* Need to see whether "t2" maps exactly "checked" keys */
  buckets = t2->buckets;
  weak = t2->weak;
  for (i = t2->size; i--; ) {
    bucket = buckets[i];
    if (bucket) {
      if (weak) {
	key = (const char *)HT_EXTRACT_WEAK(bucket->key);
      } else {
	key = bucket->key;
      }
      if (key) {
	if (!checked)
	  return 0;
	--checked;
      }
    }
  }

  return !checked;
}

int scheme_bucket_table_equal(Scheme_Bucket_Table *t1, Scheme_Bucket_Table *t2)
{
  return scheme_equal((Scheme_Object *)t1, (Scheme_Object *)t2);
}

/*========================================================================*/
/*                         precise GC hashing                             */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

void scheme_init_hash_key_procs(void)
{
  register_traversers();
}

intptr_t scheme_hash_key(Scheme_Object *o)
{
  return PTR_TO_LONG(o) >> 2;
}

END_XFORM_SKIP;

#endif

/*========================================================================*/
/*                           equal? hashing                               */
/*========================================================================*/

typedef struct Hash_Info {
  intptr_t depth; /* always odd */
  Scheme_Object *recur;
} Hash_Info;

static intptr_t equal_hash_key(Scheme_Object *o, intptr_t k, Hash_Info *hi);
static intptr_t equal_hash_key2(Scheme_Object *o, Hash_Info *hi);

static Scheme_Object *hash_recur(int argc, Scheme_Object **argv, Scheme_Object *prim)
{
  intptr_t v;
  Hash_Info *hi;

  hi = (Hash_Info *)SCHEME_PRIM_CLOSURE_ELS(prim)[0];
  hi->depth += 2;

  v = equal_hash_key(argv[0], 0, hi);
  
  return scheme_make_integer(v);
}

static Scheme_Object *hash_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v = (Scheme_Object *)p->ku.k.p1;
  Hash_Info *hi = (Hash_Info *)p->ku.k.p2;
  intptr_t nv;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  
  nv = equal_hash_key(v, p->ku.k.i1, hi);

  return scheme_make_integer_value(nv);
}

static intptr_t overflow_equal_hash_key(Scheme_Object *o, intptr_t k, Hash_Info *hi)
{
  Scheme_Object *nv;
  intptr_t val;
  Hash_Info *hi2;
  Scheme_Thread *p = scheme_current_thread;

  hi2 = (Hash_Info *)scheme_malloc(sizeof(Hash_Info));
  memcpy(hi2, hi, sizeof(Hash_Info));

  p->ku.k.p1 = (void *)o;
  p->ku.k.p2 = (void *)hi2;
  p->ku.k.i1 = k;

  nv = scheme_handle_stack_overflow(hash_k);
  scheme_get_int_val(nv, &val);

  memcpy(hi, hi2, sizeof(Hash_Info));

  return val;
}

XFORM_NONGCING static intptr_t dbl_hash_val(double d) 
  XFORM_SKIP_PROC
{
  int e;
  
  if (MZ_IS_NAN(d)) {
    d = 0.0;
    e = 1000;
  } else if (MZ_IS_POS_INFINITY(d)) {
    d = 0.5;
    e = 1000;
  } else if (MZ_IS_NEG_INFINITY(d)) {
    d = -0.5;
    e = 1000;
  } else if (!d && scheme_minus_zero_p(d)) {
    d = 0;
    e = 1000;
  } else {
    /* frexp should not be used on inf or nan: */
    d = frexp(d, &e);
  }

  return ((intptr_t)(d * (1 << 30))) + e;
}

XFORM_NONGCING static intptr_t dbl_hash2_val(double d)  
  XFORM_SKIP_PROC
{
  int e;
  
  if (MZ_IS_NAN(d)
      || MZ_IS_POS_INFINITY(d)
      || MZ_IS_NEG_INFINITY(d)) {
    e = 1;
  } else {
    /* frexp should not be used on inf or nan: */
    d = frexp(d, &e);
  }
  return e;
}

#define OVERFLOW_HASH() overflow_equal_hash_key(o, k - t, hi)

/* Based on Bob Jenkins's one-at-a-time hash function at
   http://www.burtleburtle.net/bob/hash/doobs.html: */
#define MZ_MIX(k) (k += (k << 10), k ^= (k >> 6))

static intptr_t equal_hash_key(Scheme_Object *o, intptr_t k, Hash_Info *hi)
{
  Scheme_Type t;

 top:
  if (SCHEME_CHAPERONEP(o))
    o = ((Scheme_Chaperone *)o)->val;
  
  t = SCHEME_TYPE(o);
  k += t;

  if (hi->depth > (MAX_HASH_DEPTH << 1))
    return k;
  
  switch(t) {
  case scheme_integer_type:
    return k + SCHEME_INT_VAL(o);
#ifdef MZ_USE_SINGLE_FLOATS
  case scheme_float_type:
#endif
  case scheme_double_type:
    {
      return k + dbl_hash_val(SCHEME_DBL_VAL(o));
    }
  case scheme_bignum_type:
    {
      int i = SCHEME_BIGLEN(o);
      bigdig *d = SCHEME_BIGDIG(o), k2;
      
      k2 = k;
      while (i--) {
	k2 = (k2 << 3) + k2 + d[i];
      }
    
      return (intptr_t)k2;
    }
    break;
  case scheme_rational_type:
    {
      k += equal_hash_key(scheme_rational_numerator(o), 0, hi);
      o = scheme_rational_denominator(o);
      break;
    }
  case scheme_complex_type:
    {
      Scheme_Complex *c = (Scheme_Complex *)o;
      k += equal_hash_key(c->r, 0, hi);
      o = c->i;
      break;
    }
  case scheme_pair_type:
    {
#     include "mzhashchk.inc"
      hi->depth += 2;
      k = (k << 3) + k;
      k += equal_hash_key(SCHEME_CAR(o), 0, hi);
      /* If it's a list, don't count cdr direction as depth: */
      if (scheme_is_list(o))
        hi->depth -= 2;
      o = SCHEME_CDR(o);
      break;
    }
  case scheme_mutable_pair_type:
    {
#     include "mzhashchk.inc"
      hi->depth += 2;
      k = (k << 3) + k;
      k += equal_hash_key(SCHEME_CAR(o), 0, hi);
      o = SCHEME_CDR(o);
      break;
    }
  case scheme_cpointer_type:
    {
      k = (k << 3) + k;
      k += (intptr_t)((char *)SCHEME_CPTR_VAL(o) + SCHEME_CPTR_OFFSET(o));
      return k;
    }
  case scheme_vector_type:
  case scheme_fxvector_type:
  case scheme_wrap_chunk_type:
    {
      int len = SCHEME_VEC_SIZE(o), i, val;
#     include "mzhashchk.inc"

      if (!len)
	return k + 1;
      
      hi->depth += 2;
      --len;
      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
	val = equal_hash_key(SCHEME_VEC_ELS(o)[i], 0, hi);
	k = (k << 5) + k + val;
      }
      
      o = SCHEME_VEC_ELS(o)[len];
      break;
    }
  case scheme_flvector_type:
    {
      intptr_t len = SCHEME_FLVEC_SIZE(o), i;
      double d;

      if (!len)
	return k + 1;
      
      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
	d = SCHEME_FLVEC_ELS(o)[i];
        k = (k << 5) + k + dbl_hash_val(d);
      }
      
      return k;
    }
  case scheme_char_type:
    return k + SCHEME_CHAR_VAL(o);
  case scheme_byte_string_type:
  case scheme_unix_path_type:
  case scheme_windows_path_type:
    {
      int i = SCHEME_BYTE_STRLEN_VAL(o);
      char *s = SCHEME_BYTE_STR_VAL(o);
      
      while (i--) {
	k += s[i];
        MZ_MIX(k);
      }
      
      return k;
    }
  case scheme_char_string_type:
    {
      int i = SCHEME_CHAR_STRLEN_VAL(o);
      mzchar *s = SCHEME_CHAR_STR_VAL(o);
      
      while (i--) {
	k += s[i];
        MZ_MIX(k);
      }
      
      return k;
    }
  case scheme_structure_type:
  case scheme_proc_struct_type:
    {
      Scheme_Object *procs;

      procs = scheme_struct_type_property_ref(scheme_equal_property, o);
      if (procs) {
        Scheme_Object *a[2], *recur, *v;
        Hash_Info *hi2;

#       include "mzhashchk.inc"

        /* Create/cache closure to use for recursive hashing: */
        if (hi->recur) {
          recur = hi->recur;
          hi2 = (Hash_Info *)SCHEME_PRIM_CLOSURE_ELS(recur)[0];
        } else {
          hi2 = (Hash_Info *)scheme_malloc(sizeof(Hash_Info));
          a[0] = (Scheme_Object *)hi2;
          recur = scheme_make_prim_closure_w_arity(hash_recur,
                                                   1, a,
                                                   "equal-hash-code/recur",
                                                   1, 1);
          hi->recur = recur;
        }
        memcpy(hi2, hi, sizeof(Hash_Info));

        a[0] = o;
        a[1] = recur;
        
        procs = SCHEME_VEC_ELS(procs)[2];
        
        v = _scheme_apply(procs, 2, a);

        if (SCHEME_INTP(v))
          return k + SCHEME_INT_VAL(v);
        else if (SCHEME_BIGNUMP(v)) {
          return k + (intptr_t)((Scheme_Bignum *)v)->digits[0];
        } else {
          scheme_arg_mismatch("equal-hash-code",
                              "hash procedure returned a value other than an exact integer: ",
                              v);
          return 0;
        }
      } else {
        Scheme_Object *insp;
        insp = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
        if (scheme_inspector_sees_part(o, insp, -2)) {
          int i;
          Scheme_Structure *s1 = (Scheme_Structure *)o;
	
#         include "mzhashchk.inc"
	
          hi->depth += 2;

          for (i = SCHEME_STRUCT_NUM_SLOTS(s1); i--; ) {
            k += equal_hash_key(s1->slots[i], 0, hi);
            MZ_MIX(k);
          }
	
          return k;
        } else
          return k + (PTR_TO_LONG(o) >> 4);
      }
    }
  case scheme_box_type:
    {
      SCHEME_USE_FUEL(1);
      k += 1;
      o = SCHEME_BOX_VAL(o);
      hi->depth += 2;
      break;
    }
  case scheme_hash_table_type:
    {
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)o;
      Scheme_Object **vals, **keys;
      int i;
      intptr_t vk, old_depth;

#     include "mzhashchk.inc"

      k = (k << 1) + 3;
      hi->depth += 2;
      old_depth = hi->depth;
      
      keys = ht->keys;
      vals = ht->vals;
      for (i = ht->size; i--; ) {
	if (vals[i]) {
          vk = equal_hash_key(keys[i], 0, hi);
          MZ_MIX(vk);
	  vk += equal_hash_key(vals[i], 0, hi);
          MZ_MIX(vk);
          k += vk;  /* can't mix k, because the key order shouldn't matter */
          hi->depth = old_depth; /* also needed to avoid order-sensitivity */
	}
      }
      
      return k;
    }
  case scheme_hash_tree_type:
    {
      Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)o;
      Scheme_Object *ik, *iv;
      int i;
      intptr_t vk, old_depth;

#     include "mzhashchk.inc"

      k = (k << 1) + 3;
      hi->depth += 2;
      old_depth = hi->depth;

      for (i = ht->count; i--; ) {
        scheme_hash_tree_index(ht, i, &ik, &iv);
        vk = equal_hash_key(ik, 0, hi);
        MZ_MIX(vk);
        vk += equal_hash_key(iv, 0, hi);
        MZ_MIX(vk);
        k += vk;  /* can't mix k, because the key order shouldn't matter */
        hi->depth = old_depth; /* also needed to avoid order-sensitivity */
      }
      
      return k;
    }
  case scheme_bucket_table_type:
    {
      Scheme_Bucket_Table *ht = (Scheme_Bucket_Table *)o;
      Scheme_Bucket **buckets, *bucket;
      const char *key;
      int i, weak;
      intptr_t vk, old_depth;
  
#    include "mzhashchk.inc"

      buckets = ht->buckets;
      weak = ht->weak;
      hi->depth += 2;
      old_depth = hi->depth;
      
      k = (k << 1) + 7;
      
      for (i = ht->size; i--; ) {
	bucket = buckets[i];
	if (bucket) {
	  if (weak) {
	    key = (const char *)HT_EXTRACT_WEAK(bucket->key);
	  } else {
	    key = bucket->key;
	  }
	  if (key) {
	    vk = equal_hash_key((Scheme_Object *)bucket->val, 0, hi);
            MZ_MIX(vk);
	    vk += equal_hash_key((Scheme_Object *)key, 0, hi);
            MZ_MIX(vk);
            k += vk; /* can't mix k, because the key order shouldn't matter */
            hi->depth = old_depth; /* also needed to avoid order-sensitivity */
	  }
	}
      }
      
      return k;
    }
# ifndef MZ_PRECISE_GC
  case scheme_keyword_type:
  case scheme_symbol_type:
    {
      Scheme_Symbol *s = (Scheme_Symbol *)o;
      if (!(MZ_OPT_HASH_KEY(&s->iso) & 0x1)) {
	/* Interned. Make key depend only on the content. */
	if (!(MZ_OPT_HASH_KEY(&s->iso) & 0xFFFC)) {
	  int i, h = 0;
	  for (i = s->len; i--; ) {
	    h += (h << 5) + h + s->s[i];
	  }
	  h += (h << 2);
	  if (!(((short)h) & 0xFFFC))
	    h = 0x10;
	  MZ_OPT_HASH_KEY(&s->iso) |= (((short)h) & 0xFFFC);
	}
	
	return k + (MZ_OPT_HASH_KEY(&s->iso) & 0xFFFC);
      } else
	return k + (PTR_TO_LONG(o) >> 4);
    }
# endif
  case scheme_resolved_module_path_type:
    /* Needed for interning */
    {
      k += 7;
      o = SCHEME_PTR_VAL(o);
    }
    break;
  default:    
    {
      Scheme_Primary_Hash_Proc h1 = scheme_type_hash1s[t];
      if (h1)
        return h1(o, k, hi);
      else
        return k + (PTR_TO_LONG(o) >> 4);
    }
  }

  MZ_MIX(k);
  goto top;
}

intptr_t scheme_equal_hash_key(Scheme_Object *o)
{
  Hash_Info hi;

  hi.depth = 1;
  hi.recur = NULL;

  return equal_hash_key(o, 0, &hi);
}

intptr_t scheme_equal_hash_key2(Scheme_Object *o)
{
  Hash_Info hi;

  hi.depth = 1;
  hi.recur = NULL;

  return equal_hash_key2(o, &hi);
}

intptr_t scheme_eqv_hash_key(Scheme_Object *o)
{
  if (!SCHEME_INTP(o) && (SCHEME_NUMBERP(o) || SCHEME_CHARP(o)))
    return scheme_equal_hash_key(o);
  else
    return (PTR_TO_LONG(o) >> 2);
}

intptr_t scheme_eqv_hash_key2(Scheme_Object *o)
{
  if (!SCHEME_INTP(o) && (SCHEME_NUMBERP(o) || SCHEME_CHARP(o)))
    return scheme_equal_hash_key2(o);
  else
    return (PTR_TO_LONG(o) >> 3);
}

static Scheme_Object *hash2_recur(int argc, Scheme_Object **argv, Scheme_Object *prim)
{
  intptr_t v;
  Hash_Info *hi;

  hi = (Hash_Info *)SCHEME_PRIM_CLOSURE_ELS(prim)[0];
  hi->depth += 2;

  v = equal_hash_key2(argv[0], hi);
  
  return scheme_make_integer(v);
}

static Scheme_Object *hash2_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v = (Scheme_Object *)p->ku.k.p1;
  Hash_Info *hi = (Hash_Info *)p->ku.k.p2;
  intptr_t nv;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  
  nv = equal_hash_key2(v, hi);

  return scheme_make_integer(nv);
}

static intptr_t overflow_equal_hash_key2(Scheme_Object *o, Hash_Info *hi)
{
  Scheme_Object *nv;
  intptr_t val;
  Hash_Info *hi2;
  Scheme_Thread *p = scheme_current_thread;

  hi2 = (Hash_Info *)scheme_malloc(sizeof(Hash_Info));
  memcpy(hi2, hi, sizeof(Hash_Info));

  p->ku.k.p1 = (void *)o;
  p->ku.k.p2 = (void *)hi2;

  nv = scheme_handle_stack_overflow(hash2_k);
  scheme_get_int_val(nv, &val);

  memcpy(hi, hi2, sizeof(Hash_Info));

  return val;
}

#undef OVERFLOW_HASH
#define OVERFLOW_HASH() overflow_equal_hash_key2(o, hi)

static intptr_t equal_hash_key2(Scheme_Object *o, Hash_Info *hi)
{
  Scheme_Type t;

 top:
  if (SCHEME_CHAPERONEP(o))
    o = ((Scheme_Chaperone *)o)->val;

  t = SCHEME_TYPE(o);

  if (hi->depth > (MAX_HASH_DEPTH << 1))
    return t;
  
  switch(t) {
  case scheme_integer_type:
    return t - SCHEME_INT_VAL(o);
#ifdef MZ_USE_SINGLE_FLOATS
  case scheme_float_type:
#endif
  case scheme_double_type:
    {
      return dbl_hash2_val(SCHEME_FLOAT_VAL(o));
    }
  case scheme_bignum_type:
    return SCHEME_BIGDIG(o)[0];
  case scheme_rational_type:
    return equal_hash_key2(scheme_rational_numerator(o), hi);
  case scheme_complex_type:
    {
      intptr_t v1, v2;
      Scheme_Complex *c = (Scheme_Complex *)o;
      v1 = equal_hash_key2(c->r, hi);
      v2 = equal_hash_key2(c->i, hi);
      return v1 + v2;
    }
  case scheme_pair_type:
    {
      intptr_t v1, v2;
#     include "mzhashchk.inc"
      hi->depth += 2;
      v1 = equal_hash_key2(SCHEME_CAR(o), hi);
      v2 = equal_hash_key2(SCHEME_CDR(o), hi);
      return v1 + v2;
    }
  case scheme_mutable_pair_type:
    {
      intptr_t v1, v2;
#     include "mzhashchk.inc"
      hi->depth += 2;
      v1 = equal_hash_key2(SCHEME_CAR(o), hi);
      v2 = equal_hash_key2(SCHEME_CDR(o), hi);
      return v1 + v2;
    }
  case scheme_cpointer_type:
    {
      return (intptr_t)((char *)SCHEME_CPTR_VAL(o) + SCHEME_CPTR_OFFSET(o));
    }
  case scheme_vector_type:
  case scheme_fxvector_type:
  case scheme_wrap_chunk_type:
    {
      int len = SCHEME_VEC_SIZE(o), i;
      intptr_t k = 0;

#     include "mzhashchk.inc"

      hi->depth += 2;

      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
	k += equal_hash_key2(SCHEME_VEC_ELS(o)[i], hi);
      }
      
      return k;
    }
  case scheme_flvector_type:
    {
      intptr_t len = SCHEME_FLVEC_SIZE(o), i;
      double d;
      intptr_t k = 0;

      if (!len)
	return k + 1;
      
      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
	d = SCHEME_FLVEC_ELS(o)[i];
        k = (k << 5) + k + dbl_hash2_val(d);
      }
      
      return k;
    }
  case scheme_char_type:
    return t;
  case scheme_byte_string_type:
  case scheme_unix_path_type:
  case scheme_windows_path_type:
    {
      int k = 0, i = SCHEME_BYTE_STRLEN_VAL(o);
      char *s = SCHEME_BYTE_STR_VAL(o);
    
      while (i--) {
	k += s[i];
      }
    
      return k;
    }
  case scheme_char_string_type:
    {
      int k = 0, i = SCHEME_CHAR_STRLEN_VAL(o);
      mzchar *s = SCHEME_CHAR_STR_VAL(o);
    
      while (i--) {
	k += s[i];
      }
    
      return k;
    }
  case scheme_structure_type:
  case scheme_proc_struct_type:
    {
      Scheme_Object *procs;

      procs = scheme_struct_type_property_ref(scheme_equal_property, o);
      if (procs) {
        Scheme_Object *a[2], *v, *recur;
        Hash_Info *hi2;

#       include "mzhashchk.inc"

        /* Create/cache closure to use for recursive hashing: */
        if (hi->recur) {
          recur = hi->recur;
          hi2 = (Hash_Info *)SCHEME_PRIM_CLOSURE_ELS(recur)[0];
        } else {
          hi2 = (Hash_Info *)scheme_malloc(sizeof(Hash_Info));
          a[0] = (Scheme_Object *)hi2;
          recur = scheme_make_prim_closure_w_arity(hash2_recur,
                                                   1, a,
                                                   "equal-secondary-hash-code/recur",
                                                   1, 1);
          hi->recur = recur;
        }
        memcpy(hi2, hi, sizeof(Hash_Info));
        
        a[0] = o;
        a[1] = recur;
        
        procs = SCHEME_VEC_ELS(procs)[3];
        
        v = _scheme_apply(procs, 2, a);

        if (SCHEME_INTP(v))
          return SCHEME_INT_VAL(v);
        else if (SCHEME_BIGNUMP(v)) {
          return (intptr_t)((Scheme_Bignum *)v)->digits[0];
        } else {
          scheme_arg_mismatch("equal-secondary-hash-code",
                              "hash procedure returned a value other than an exact integer: ",
                              v);
          return 0;
        }
      } else {
        Scheme_Object *insp;
        insp = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
        if (scheme_inspector_sees_part(o, insp, -2)) {
          int i;
          intptr_t k = 0;
          Scheme_Structure *s1 = (Scheme_Structure *)o;
          
#         include "mzhashchk.inc"
	
          hi->depth += 2;

          for (i = SCHEME_STRUCT_NUM_SLOTS(s1); i--; ) {
            k += equal_hash_key2(s1->slots[i], hi);
          }
          
          return k;
        } else
          return t;
      }
    }
  case scheme_box_type:
    o = SCHEME_BOX_VAL(o);
    hi->depth += 2;
    goto top;
  case scheme_hash_table_type:
    {
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)o;
      Scheme_Object **vals, **keys;
      int i;
      intptr_t k = 0, old_depth;
      
#     include "mzhashchk.inc"

      hi->depth += 2;
      old_depth = hi->depth;

      keys = ht->keys;
      vals = ht->vals;
      for (i = ht->size; i--; ) {
	if (vals[i]) {
	  k += equal_hash_key2(keys[i], hi);
	  k += equal_hash_key2(vals[i], hi);
          hi->depth = old_depth;
	}
      }
      
      return k;
    }
  case scheme_hash_tree_type:
    {
      Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)o;
      Scheme_Object *iv, *ik;
      int i;
      intptr_t k = 0, old_depth;
      
#     include "mzhashchk.inc"

      hi->depth += 2;
      old_depth = hi->depth;

      for (i = ht->count; i--; ) {
        scheme_hash_tree_index(ht, i, &ik, &iv);
        k += equal_hash_key2(ik, hi);
        k += equal_hash_key2(iv, hi);
        hi->depth = old_depth;
      }
      
      return k;
    }
  case scheme_bucket_table_type:
    {
      Scheme_Bucket_Table *ht = (Scheme_Bucket_Table *)o;
      Scheme_Bucket **buckets, *bucket;
      const char *key;
      int i, weak;
      intptr_t k = 0, old_depth;

#     include "mzhashchk.inc"
  
      buckets = ht->buckets;
      weak = ht->weak;
      
      hi->depth += 2;
      old_depth = hi->depth;

      for (i = ht->size; i--; ) {
	bucket = buckets[i];
	if (bucket) {
	  if (weak) {
	    key = (const char *)HT_EXTRACT_WEAK(bucket->key);
	  } else {
	    key = bucket->key;
	  }
	  if (key) {
	    k += equal_hash_key2((Scheme_Object *)bucket->val, hi);
	    k += equal_hash_key2((Scheme_Object *)key, hi);
            old_depth = hi->depth;
	  }
	}
      }
    
      return k;
    }
  case scheme_resolved_module_path_type:
    /* Needed for interning */
    o = SCHEME_PTR_VAL(o);
    goto top;
  default:
    {
      Scheme_Secondary_Hash_Proc h2 = scheme_type_hash2s[t];
      if (h2)
        return h2(o, hi);
      else
        return t;
    }
  }
}

intptr_t scheme_recur_equal_hash_key(Scheme_Object *o, void *cycle_data)
{
  return equal_hash_key(o, 0, (Hash_Info *)cycle_data);
}

intptr_t scheme_recur_equal_hash_key2(Scheme_Object *o, void *cycle_data)
{
  return equal_hash_key2(o, (Hash_Info *)cycle_data);
}

/*========================================================================*/
/*                        functional hash tables                          */
/*========================================================================*/

/* Direct port of red-black trees in Jens Axel Soegaard's "galore" package,
   which implemented in Scheme (5th may 2006 version) and says:
   
   ; This is direct port of Jean-Christophe Filliatre's implementation
   ; of red-black trees in Ocaml. */

typedef struct RBNode {
  Scheme_Inclhash_Object iso; /* 0x1 => red */
  uintptr_t code;
  Scheme_Object *key; /* NULL => val is list of key-value pairs */
  Scheme_Object *val;
  struct RBNode *left, *right;
} RBNode;

#define RB_REDP(rb) (MZ_OPT_HASH_KEY(&(rb)->iso) & 0x1)

#if 0
# define RB_ASSERT(p) if (p) { } else { scheme_signal_error("hash-tree assert failure %d", __LINE__); }
#else
# define RB_ASSERT(p) /* empty */
#endif

static RBNode *make_rb(int red, 
                       RBNode *left,
                       uintptr_t code, Scheme_Object *key, Scheme_Object *val,
                       RBNode *right)
{
  RBNode *rb;

  rb = MALLOC_ONE_TAGGED(RBNode);
  SET_REQUIRED_TAG(rb->iso.so.type = scheme_rt_rb_node);
  if (red)
    MZ_OPT_HASH_KEY(&rb->iso) |= 0x1;
  rb->code = code;
  rb->key = key;
  rb->val = val;
  rb->left = left;
  rb->right = right;

  return rb;
}

static RBNode *recolor_rb(int red, RBNode *rb)
{
  return make_rb(red, rb->left, 
                 rb->code, rb->key, rb->val,
                 rb->right);
}

static RBNode *rb_find(uintptr_t code, RBNode *s)
{
  while (1) {
    if (!s)
      return NULL;
    
    if (s->code == code)
      return s;
    else if (s->code > code)
      s = s->left;
    else
      s = s->right;
  }
}

static RBNode *RB_CHK(RBNode *rb, uintptr_t code)
{
  RB_ASSERT(rb_find(code, rb));
  return rb;
}
  
/*
  ;;; INVARIANTS
  
  ;  (* Invariants: (1) a red node has no red son, and (2) any path from the
  ;     root to a leaf has the same number of black nodes *)
*/

static RBNode *lbalance(RBNode *x1, 
                        uintptr_t code, Scheme_Object *key, Scheme_Object *val,
                        RBNode *d)
{
  /*
  (define (lbalance x1 x2 x3)
    (let ([z x2] [d x3])
      (match x1
        [($ R ($ R a x b) y c)  (R- (B- a x b) y (B- c z d))]
        [($ R a x ($ R b y c))  (R- (B- a x b) y (B- c z d))]
        [_                      (B- x1 x2 x3)])))
  */

  if (x1 && RB_REDP(x1)) {
    RBNode *left = x1->left;
    if (left && RB_REDP(left)) {
      return make_rb(1, 
                     recolor_rb(0, left),
                     x1->code, x1->key, x1->val,
                     make_rb(0, x1->right,
                             code, key, val,
                             d));
                             
    } else {
      RBNode *right = x1->right;
      if (right && RB_REDP(right)) {
        return make_rb(1,
                       make_rb(0, x1->left,
                               x1->code, x1->key, x1->val,
                               right->left),
                       right->code, right->key, right->val,
                       make_rb(0,
                               right->right,
                               code, key, val,
                               d));

      }
    }
  }

  return make_rb(0, x1, code, key, val, d);
}

static RBNode *rbalance(RBNode *a, 
                        uintptr_t code, Scheme_Object *key, Scheme_Object *val,
                        RBNode *x3)
{
  /*
  (define (rbalance x1 x2 x3)
    (let ([a x1] [x x2])
      (match x3
        [($ R ($ R b y c) z d)  (R- (B- a x b) y (B- c z d))]
        [($ R b y ($ R c z d))  (R- (B- a x b) y (B- c z d))]
        [_                      (B- x1 x2 x3)])))
  */

  if (x3 && RB_REDP(x3)) {
    RBNode *left = x3->left;
    if (left && RB_REDP(left)) {
      return make_rb(1,
                     make_rb(0, a,
                             code, key, val,
                             left->left),
                     left->code, left->key, left->val, 
                     make_rb(0, left->right,
                             x3->code, x3->key, x3->val,
                             x3->right));
    } else {
      RBNode *right = x3->right;
      if (right && RB_REDP(right)) {
        return make_rb(1,
                       make_rb(0, a,
                               code, key, val,
                               x3->left),
                       x3->code, x3->key, x3->val,
                       recolor_rb(0, right));
      }
    }
  }

  return make_rb(0, a, code, key, val, x3);
}

static RBNode *ins(uintptr_t code, Scheme_Object *key, Scheme_Object *val, RBNode *s)
{
  /*
      (match s
        [()           (R- empty x empty)]
        [($ R a y b)  (if3 (cmp x y)
                           (R- (ins a) y b)
                           s
                           (R- a y (ins b)))]
        [($ B a y b)  (if3 (cmp x y)
                           (lbalance (ins a) y b)
                           s
                           (rbalance a y (ins b)))]))
  */

  if (!s) {
    s = RB_CHK(make_rb(1, NULL, code, key, val, NULL), code);
    return s;
  } else if (RB_REDP(s)) {
    if (code < s->code) {
      return RB_CHK(make_rb(1, ins(code, key, val, s->left),
                            s->code, s->key, s->val,
                            s->right),
                    code);
    } else if (s->code == code) {
      return RB_CHK(s, code);
    } else {
      return RB_CHK(make_rb(1, s->left,
                            s->code, s->key, s->val,
                            ins(code, key, val, s->right)),
                    code);
    }
  } else {
    if (code < s->code) {
      return RB_CHK(lbalance(ins(code, key, val, s->left),
                             s->code, s->key, s->val,
                             s->right),
                    code);
    } else if (s->code == code) {
      return RB_CHK(s, code);
    } else {
      RBNode *r;
      r = RB_CHK(ins(code, key, val, s->right), code);
      return RB_CHK(rbalance(s->left,
                             s->code, s->key, s->val,
                             r),
                    code);
    }
  }
}

static RBNode *rb_insert(uintptr_t code, Scheme_Object *key, Scheme_Object *val,
                         RBNode *s)
{
  RBNode *s1;

  s1 = ins(code, key, val, s);

  /* ; color the root black */
  if (RB_REDP(s1))
    return recolor_rb(0, s1);
  else
    return s1;
}

static RBNode *rb_replace(RBNode *s, RBNode *orig, RBNode *naya)
{
  if (SAME_OBJ(s, orig))
    return naya;
  if (s->code > orig->code)
    return make_rb(RB_REDP(s),
                   rb_replace(s->left, orig, naya),
                   s->code, s->key, s->val,
                   s->right);
  else
    return make_rb(RB_REDP(s),
                   s->left,
                   s->code, s->key, s->val,
                   rb_replace(s->right, orig, naya));
}

static RBNode *unbalanced_left(RBNode *s, int *_bh_dec)
{
  /*
  ;  (* [unbalanced_left] repares invariant (2) when the black height of the
  ;     left son exceeds (by 1) the black height of the right son *)
  ; [original spelling kept -- a quote is a quote ]

  (define (unbalanced-left s)
    (match s
      [($ R ($ B t1 x1 t2) x2 t3)              (values (lbalance (R- t1 x1 t2) x2 t3) #f)]
      [($ B ($ B t1 x1 t2) x2 t3)              (values (lbalance (R- t1 x1 t2) x2 t3) #t)]
      [($ B ($ R t1 x1 ($ B t2 x2 t3)) x3 t4)  (values (B- t1 x1 (lbalance (R- t2 x2 t3) x3 t4)) #f)]
      [_                                       (error 'unbalanced-left
                                                      (format "Black height of both sons were the same: ~a"
                                                              (->sexp s)))]))
  */
  RBNode *left = s->left;
  
  RB_ASSERT(left);

  if (!RB_REDP(left)) {
    *_bh_dec = !RB_REDP(s);
    return lbalance(recolor_rb(1, left),
                    s->code, s->key,s->val,
                    s->right);
  } else {
    RBNode *lr = left->right;
    *_bh_dec = 0;
    RB_ASSERT(RB_REDP(left));
    RB_ASSERT(lr && !RB_REDP(lr));
    return make_rb(0, left->left,
                   left->code, left->key, left->val, 
                   lbalance(recolor_rb(1, lr),
                            s->code, s->key,s->val,
                            s->right));
  }
}

static RBNode *unbalanced_right(RBNode *s, int *_bh_dec)
{
  /*
  ;  (* [unbalanced_right] repares invariant (2) when the black height of the
  ;     right son exceeds (by 1) the black height of the left son *)
  
  (define (unbalanced-right s)
    (match s
      [($ R t1 x1 ($ B t2 x2 t3))             (values (rbalance t1 x1 (R- t2 x2 t3)) #f)]
      [($ B t1 x1 ($ B t2 x2 t3))             (values (rbalance t1 x1 (R- t2 x2 t3)) #t)]
      [($ B t1 x1 ($ R ($ B t2 x2 t3) x3 t4)) (values (B- (rbalance t1 x1 (R- t2 x2 t3)) x3 t4) #f)]
      [_                                      (error 'unbalanced-right 
                                                     (format "Black height of both sons were the same: ~a"
                                                             (->sexp s)))]))
  
  */
  RBNode *right = s->right;
  
  RB_ASSERT(right);

  if (!RB_REDP(right)) {
    *_bh_dec = !RB_REDP(s);
    return rbalance(s->left,
                    s->code, s->key,s->val,
                    recolor_rb(1, right));
  } else {
    RBNode *rl = right->left;
    *_bh_dec = 0;
    RB_ASSERT(RB_REDP(right));
    RB_ASSERT(rl && !RB_REDP(rl));
    return make_rb(0, rbalance(s->left,
                               s->code, s->key,s->val,
                               recolor_rb(1, rl)),
                   right->code, right->key, right->val, 
                   right->right);
  }
}
 
static RBNode *remove_min(RBNode *s, RBNode **_m, int *_bh_dec)
{
  /*
  ;  (* [remove_min s = (s',m,b)] extracts the minimum [m] of [s], [s'] being the
  ;     resulting set, and indicates with [b] whether the black height has
  ;     decreased *)
  
  (define (remove-min s)
    (match s
      [()                         (error "remove-min: Called on empty set")]
      ;  minimum is reached
      [($ B () x ())           (values empty x #t)]
      [($ B () x ($ R l y r))  (values (B- l y r) x #f)]
      [($ B () _ ($ B _ _ _))  (error)]
      [($ R () x r)            (values r x #f)]
      ;  minimum is recursively extracted from [l]
      [($ B l x r)                (let-values ([(l1 m d) (remove-min l)])
                                    (let ([t (B- l1 x r)])
                                      (if d
                                          (let-values ([(t d1) (unbalanced-right t)])
                                            (values t m d1))
                                          (values t m #f))))]
      [($ R l x r)                (let-values ([(l1 m d) (remove-min l)])
                                    (let ([t (R- l1 x r)])
                                      (if d
                                          (let-values ([(t d1) (unbalanced-right t)])
                                            (values t m d1))
                                          (values t m #f))))]))
  */

  RB_ASSERT(s);

  if (!RB_REDP(s) && !s->left) {
    if (!s->right) {
      *_bh_dec = 1;
      *_m = s;
      return NULL;
    } else if (RB_REDP(s->right)) {
      *_bh_dec = 0;
      *_m = s;
      return recolor_rb(0, s->right);
    } else {
      RB_ASSERT(0);
      return NULL;
    }
  }
  if (RB_REDP(s) && !s->left) {
    *_bh_dec = 0;
    *_m = s;
    return s->right;
  }
  /* covers last two cases of Scheme code: */
  {
    int left_bh_dec;
    RBNode *l1, *t;
    l1 = remove_min(s->left, _m, &left_bh_dec);
    t = make_rb(RB_REDP(s), l1, s->code, s->key, s->val, s->right);
    if (left_bh_dec)
      return unbalanced_right(t, _bh_dec);
    else {
      *_bh_dec = 0;
      return t;
    }
  }
}

static RBNode *remove_aux(RBNode *s, uintptr_t code, int *_bh_dec)
{
  /*
    (define (remove-aux s)
      (match s
        [()           (values empty #f)]
        [($ B l y r)  (if3 (cmp x y)
                           (let-values ([(l1 d) (remove-aux l)])
                             (let ([t (B- l1 y r)]) ; [mm: R-]
                               (if d
                                   (unbalanced-right t)
                                   (values t #f))))
                           (match r
                             [()    (blackify l)] ; [mm: (values l #f)]
                             [_     (let-values ([(r1 m d) (remove-min r)])
                                      (let ([t (B- l m r1)]) ; [mm: R-]
                                        (if d
                                            (unbalanced-left t)
                                            (values t #f))))])
                           
                           (let-values ([(r1 d) (remove-aux r)])
                             (let ([t (B- l y r1)]) ; [mm: R-]
                               (if d
                                   (unbalanced-left t)
                                   (values t #f)))))]
        [($ R l y r)  ...])) ; the same, with "mm" changes
  */

  if (!s) {
    *_bh_dec = 0;
    return NULL;
  } else {
    if (code < s->code) {
      RBNode *l1, *t;
      int left_bh_dec;
      l1 = remove_aux(s->left, code, &left_bh_dec);
      t = make_rb(RB_REDP(s), l1,
                  s->code, s->key, s->val,
                  s->right);
      if (left_bh_dec)
        return unbalanced_right(t, _bh_dec);
      else {
        *_bh_dec = 0;
        return t;
      }
    } else if (code == s->code) {
      if (!s->right) {
        if (!RB_REDP(s)) {
          RBNode *l = s->left;
          /* (blackify l) */
          if (!l) {
            *_bh_dec = 1;
            return NULL;
          } else if (RB_REDP(l)) {
            *_bh_dec = 0;
            return recolor_rb(0, l);
          } else {
            *_bh_dec = 1;
            return l;
          }
        } else {
          *_bh_dec = 0;
          return s->left;
        }
      } else {
        RBNode *r1, *t, *m;
        int right_bh_dec;
        r1 = remove_min(s->right, &m, &right_bh_dec);
        t = make_rb(RB_REDP(s), s->left,
                    m->code, m->key, m->val,
                    r1);
        if (right_bh_dec)
          return unbalanced_left(t, _bh_dec);
        else {
          *_bh_dec = 0;
          return t;
        }
      }
    } else {
      RBNode *r1, *t;
      int right_bh_dec;
      r1 = remove_aux(s->right, code, &right_bh_dec);
      t = make_rb(RB_REDP(s), s->left,
                  s->code, s->key, s->val,
                  r1);
      if (right_bh_dec)
        return unbalanced_left(t, _bh_dec);
      else {
        *_bh_dec = 0;
        return t;
      }
    }
  }
}

static RBNode *rb_remove(RBNode *s, uintptr_t code)
{
  int bh_dec;
  return remove_aux(s, code, &bh_dec);
}

Scheme_Hash_Tree *scheme_make_hash_tree(int kind)
{
  Scheme_Hash_Tree *tree;

  tree = MALLOC_ONE_TAGGED(Scheme_Hash_Tree);

  tree->count = 0;
  
  tree->iso.so.type = scheme_hash_tree_type;
  SCHEME_HASHTR_FLAGS(tree) |= (kind & 0x3);

  return tree;
}

Scheme_Hash_Tree *scheme_hash_tree_set(Scheme_Hash_Tree *tree, Scheme_Object *key, Scheme_Object *val)
{
  Scheme_Hash_Tree *tree2;
  uintptr_t h;
  RBNode *root, *added;
  int delta;

  if (SCHEME_HASHTR_FLAGS(tree) & 0x3) {
    if (SCHEME_HASHTR_FLAGS(tree) & 0x1) {
      h = (uintptr_t)scheme_equal_hash_key(key);
    } else {
      h = (uintptr_t)scheme_eqv_hash_key(key);
    }
  } else {
    h = (uintptr_t)PTR_TO_LONG((Scheme_Object *)key);
  }

  if (!val) {
    /* Removing ... */
    added = rb_find(h, tree->root);
    if (!added)
      return tree; /* nothing to remove */
    if (added->key) {
      int kind = (SCHEME_HASHTR_FLAGS(tree) & 0x3);

      if ((kind && ((kind == 1)
                    ? scheme_equal(added->key, key)
                    : scheme_eqv(added->key, key)))
          || (!kind && SAME_OBJ(added->key, key))) {
        /* remove single item */
        root = rb_remove(tree->root, h);
        
        tree2 = MALLOC_ONE_TAGGED(Scheme_Hash_Tree);
        memcpy(tree2, tree, sizeof(Scheme_Hash_Tree));
        tree2->elems_box = NULL;
        
        tree2->root = root;
        --tree2->count;
        
        return tree2;
      } else {
        /* Nothing to remove */
        return tree;
      }
    } else {
      /* multiple mappings; remove it below */
      root = tree->root;
    }
  } else {
    /* Adding/setting: */
    root = rb_insert(h, NULL, NULL, tree->root);
    added = rb_find(h, root);
  }

  delta = 0;
  
  if (added->val) {
    int kind = (SCHEME_HASHTR_FLAGS(tree) & 0x3);

    if (!added->key) {
      /* Have a list of keys and vals. In this case, val can be NULL
         to implement removal. */
      Scheme_Object *prs = added->val, *a;
      int cnt = 0;
      while (prs) {
        a = SCHEME_CAR(prs);
        if (kind) {
          if (kind == 1) {
            if (scheme_equal(SCHEME_CAR(a), key))
              break;
          } else {
            if (scheme_eqv(SCHEME_CAR(a), key))
              break;
          }
        } else {
          if (SAME_OBJ(SCHEME_CAR(a), key))
            break;
        }
        prs = SCHEME_CDR(prs);
        cnt++;
      }
      if (!prs) {
        /* Not mapped already: */
        if (!val) return tree; /* nothing to remove after all */
        val = scheme_make_raw_pair(scheme_make_raw_pair(key, val), added->val);
        key = NULL;
        delta = 1;
      } else {
        /* Mapped already: */
        prs = SCHEME_CDR(prs);
        for (a = added->val; cnt--; a = SCHEME_CDR(a)) {
          prs = scheme_make_raw_pair(SCHEME_CAR(a), prs);
        }
        if (val) {
          prs = scheme_make_raw_pair(scheme_make_raw_pair(key, val),
                                     prs);
        } else {
          delta = -1;
        }
        val = prs;
        key = NULL;
        if (!SCHEME_CDR(prs)) {
          /* Removal reduced to a single mapping: */
          a = SCHEME_CAR(prs);
          key = SCHEME_CAR(a);
          val = SCHEME_CDR(a);
        }
      }
    } else {
      /* Currently have one value for this hash code */
      int same;
      if (kind) {
        if (kind == 1)
          same = scheme_equal(key, added->key);
        else
          same = scheme_eqv(key, added->key);
      } else {
        same = SAME_OBJ(key, added->key);
      }
      if (!same) {
        val = scheme_make_raw_pair(scheme_make_raw_pair(key, val),
                                   scheme_make_raw_pair(scheme_make_raw_pair(added->key, added->val),
                                                        NULL));
        key = NULL;
        delta = 1;
      }
    }
    root = rb_replace(root,
                      added,
                      make_rb(RB_REDP(added),
                              added->left,
                              added->code, key, val,
                              added->right));
  } else {
    added->key = key;
    added->val = val;
    delta = 1;
  }

  tree2 = MALLOC_ONE_TAGGED(Scheme_Hash_Tree);
  memcpy(tree2, tree, sizeof(Scheme_Hash_Tree));
  tree2->elems_box = NULL;

  if (delta)
    tree2->count += delta;
  tree2->root = root;

  return tree2;
}

Scheme_Object *scheme_hash_tree_get(Scheme_Hash_Tree *tree, Scheme_Object *key)
{
  uintptr_t h;
  RBNode *rb;
  int kind = (SCHEME_HASHTR_FLAGS(tree) & 0x3);

  if (kind) {
    if (kind == 1)
      h = (uintptr_t)scheme_equal_hash_key(key);
    else
      h = (uintptr_t)scheme_eqv_hash_key(key);
  } else {
    h = (uintptr_t)PTR_TO_LONG((Scheme_Object *)key);
  }

  rb = rb_find(h, tree->root);
  if (rb) {
    if (!rb->key) {
      /* Have list of keys & vals: */
      Scheme_Object *prs = rb->val, *a;
      while (prs) {
        a = SCHEME_CAR(prs);
        if (kind) {
          if (kind == 1) {
            if (scheme_equal(SCHEME_CAR(a), key))
              return SCHEME_CDR(a);
          } else {
            if (scheme_eqv(SCHEME_CAR(a), key))
              return SCHEME_CDR(a);
          }
        } else {
          if (SAME_OBJ(SCHEME_CAR(a), key))
            return SCHEME_CDR(a);
        }
        prs = SCHEME_CDR(prs);
      }
    } else {
      if (kind) {
        if (kind == 1) {
          if (scheme_equal(key, rb->key))
            return rb->val;
        } else {
          if (scheme_eqv(key, rb->key))
            return rb->val;
        }
      } else if (SAME_OBJ(key, rb->key))
        return rb->val;
    }
  }

  return NULL;
}

intptr_t scheme_hash_tree_next(Scheme_Hash_Tree *tree, intptr_t pos)
{
  if (pos >= tree->count)
    return -2;
  pos++;
  if (tree->count > pos)
    return pos;
  else
    return -1;
}

static int fill_elems(RBNode *rb, Scheme_Object *vec, intptr_t pos, intptr_t count)
{
  if (!rb)
    return pos;

  if (rb->left)
    pos = fill_elems(rb->left, vec, pos, count);

  if (rb->key) {
    SCHEME_VEC_ELS(vec)[pos] = rb->val;
    SCHEME_VEC_ELS(vec)[pos + count] = rb->key;
    pos++;
  } else {
    Scheme_Object *prs = rb->val, *a;
    while (prs) {
      a = SCHEME_CAR(prs);
      SCHEME_VEC_ELS(vec)[pos] = SCHEME_CDR(a);
      SCHEME_VEC_ELS(vec)[pos + count] = SCHEME_CAR(a);
      pos++;
      prs = SCHEME_CDR(prs);
    }
  }

  if (rb->right)
    pos = fill_elems(rb->right, vec, pos, count);

  return pos;
}

int scheme_hash_tree_index(Scheme_Hash_Tree *tree, intptr_t pos, Scheme_Object **_key, Scheme_Object **_val)
{
  Scheme_Object *elems, *elems_box;

  if ((pos < 0) || (pos >= tree->count))
    return 0;

  elems_box = tree->elems_box;
  if (elems_box)
    elems = SCHEME_WEAK_BOX_VAL(elems_box);
  else
    elems = NULL;
  if (!elems) {
    int total_pos;
    elems = scheme_make_vector(tree->count * 2, NULL);
    total_pos = fill_elems(tree->root, elems, 0, tree->count);
    RB_ASSERT(total_pos == tree->count);
    elems_box = scheme_make_weak_box(elems);
    tree->elems_box = elems_box;
  }

  *_val = SCHEME_VEC_ELS(elems)[pos];
  *_key = SCHEME_VEC_ELS(elems)[tree->count + pos];

  return 1;
}

int scheme_hash_tree_equal_rec(Scheme_Hash_Tree *t1, Scheme_Hash_Tree *t2, void *eql)
{
  Scheme_Object *k, *v, *v2;
  int i;

  if ((t1->count != t2->count)
      || ((SCHEME_HASHTR_FLAGS(t1) & 0x3) != (SCHEME_HASHTR_FLAGS(t2) & 0x3)))
    return 0;
    
  for (i = t1->count; i--; ) {
    scheme_hash_tree_index(t1, i, &k, &v);
    v2 = scheme_hash_tree_get(t2, k);
    if (!v2)
      return 0;
    if (!scheme_recur_equal(v, v2, eql))
      return 0;
  }

  return 1;
}

int scheme_hash_tree_equal(Scheme_Hash_Tree *t1, Scheme_Hash_Tree *t2)
{
  return scheme_equal((Scheme_Object *)t1, (Scheme_Object *)t2);
}


/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_HASH_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_hash_tree_type, hash_tree_val);
  GC_REG_TRAV(scheme_rt_rb_node, mark_rb_node);
}

END_XFORM_SKIP;

#endif
