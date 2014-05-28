/*
  Racket
  Copyright (c) 2004-2014 PLT Design Inc.
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
#include <ctype.h>
#include <math.h>
#include "../gc2/gc2_obj.h"

THREAD_LOCAL_DECL(intptr_t scheme_hash_request_count);
THREAD_LOCAL_DECL(intptr_t scheme_hash_iteration_count);

READ_ONLY static Scheme_Object GONE[1];

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

/* Hash calculations need to use unsigned integers, where
   wraparound behavior is defined for overflow. But some 
   parts of the published hash API use signed integers.
   The to_signed_hash() and to_unsigned_hash() macros are
   supposed to marshal unsigned to signed and back without
   any loss of unsigned information.
   FIXME: The current implementation as a cast is *not* 
   consistent with the C standard (the cast to unsigned can
   be implementation-dependent), but it works fine with all
   compilers that we currently use. */
#define to_signed_hash(v) ((intptr_t)v)
#define to_unsigned_hash(v) ((uintptr_t)v)

#ifdef OBJHEAD_HAS_HASH_BITS
/* In 3m mode, we only have 14 bits of hash code in the
   Scheme_Object header. But the GC-level object header has some
   leftover bits (currently 9, 11, 41, or 43, depending on the
   platform), so use those, too. That only works for GCable
   objects, so we use 1 of our 14 bits to indicate whether the
   other bits are present. */
# define GCABLE_OBJ_HASH_BIT 0x0004
# define OBJ_HASH_USELESS_BITS 3
#else
# define GCABLE_OBJ_HASH_BIT 0
# define OBJ_HASH_USELESS_BITS 2
#endif
#define OBJ_HASH_USEFUL_BITS (16 - OBJ_HASH_USELESS_BITS)
#define OBJ_HASH_USEFUL_MASK ((1 << OBJ_HASH_USEFUL_BITS)-1)

#ifdef MZ_PRECISE_GC
/* keygen race conditions below are "ok", because keygen is randomness
   used to create a hashkey. Technically, a race condition allows
   undefined behavior by some C standards, but we don't expect
   compilers to actually impose a "catch fire" semantics. Make sure
   that only one thread at a time sets a hash code in a specific
   object, though, and watch out for a race with JIT-generated code
   running in a future and setting flags on pairs. */
SHARED_OK static uintptr_t keygen = GCABLE_OBJ_HASH_BIT;

XFORM_NONGCING static MZ_INLINE
uintptr_t PTR_TO_LONG(Scheme_Object *o)
{
  uintptr_t bits;
  short v;

  if (SCHEME_INTP(o))
    return (uintptr_t)o >> 1;

  v = o->keyex;

  if (!(v & 0xFFFC)) {
    uintptr_t local_keygen = keygen;
    v |= (short)local_keygen;
#ifdef OBJHEAD_HAS_HASH_BITS
    if (GC_is_allocated(o)) {
      OBJHEAD_HASH_BITS(o) = (local_keygen >> 16);
      v |= GCABLE_OBJ_HASH_BIT;
    } else
      v &= ~GCABLE_OBJ_HASH_BIT;
#endif
    if (!v) v = 0x1AD0;
#ifdef MZ_USE_FUTURES
    if (SCHEME_PAIRP(o) && scheme_is_multithreaded(1)) {
      /* Use CAS to avoid losing a hash code due to a conflict with
         JIT-generated `list?' test, which itself uses CAS to set "is
         a list" or "not a list" flags on pairs. */
      while (!mzrt_cas16(&o->keyex, o->keyex, v)) {
      } 
    } else
#endif
      o->keyex = v;
    keygen += (1 << OBJ_HASH_USELESS_BITS);
  }

#ifdef OBJHEAD_HAS_HASH_BITS
  if (v & GCABLE_OBJ_HASH_BIT)
    bits = OBJHEAD_HASH_BITS(o);
  else
#endif
    bits = o->type;

  /* We need to drop the low two bits of `v', which
     are used for non-hashing purposes in some types. */

  return (bits << OBJ_HASH_USEFUL_BITS) | ((v >> OBJ_HASH_USELESS_BITS) & OBJ_HASH_USEFUL_MASK);
}
#else
# define PTR_TO_LONG(p) ((uintptr_t)(p)>>2)
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
  uintptr_t i, h, h2;

  h2 = h = i = 0;
  while (key[i]) {
    int c = key[i++];
    h += (h << 5) + h + c;
    h2 += c;
  }

  if (_h)
    *_h = to_signed_hash(h);
  if (_h2)
    *_h2 = to_signed_hash(h2);
}

static void id_hash_indices(void *_key, intptr_t *_h, intptr_t *_h2)
{
  Scheme_Object *key = (Scheme_Object *)_key;
  uintptr_t lkey;

  if (SCHEME_STXP(key))
    key = SCHEME_STX_VAL(key);
    
  lkey = PTR_TO_LONG((Scheme_Object *)key);
  if (_h)
    *_h = to_signed_hash(lkey);
  if (_h2)
    *_h2 = to_signed_hash(lkey >> 1);
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

void scheme_clear_hash_table(Scheme_Hash_Table *ht)
{
  ht->size = 0;
  ht->count = 0;
  ht->keys = NULL;
  ht->vals = NULL;
  ht->mcount = 0;
}

static Scheme_Object *do_hash(Scheme_Hash_Table *table, Scheme_Object *key, int set, Scheme_Object *val)
{
  Scheme_Object *tkey, **keys;
  intptr_t hx, h2x;
  hash_v_t h, h2, useme = 0;
  uintptr_t mask;

 rehash_key:

  mask = table->size - 1;

  if (table->make_hash_indices) {
    GC_CAN_IGNORE intptr_t *_h2x;
    if (table->compare) {
      h2 = 0;
      _h2x = NULL;
    } else
      _h2x = &h2x;
    table->make_hash_indices((void *)key, &hx, _h2x);
    h = to_unsigned_hash(hx) & mask;
    if (_h2x)
      h2 = (to_unsigned_hash(h2x) & mask) | 1;
  } else {
    uintptr_t lkey;
    lkey = PTR_TO_LONG((Scheme_Object *)key);
    h = lkey & mask;
    h2 = ((lkey >> 1) & mask) | 1;
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
        table->make_hash_indices((void *)key, NULL, &h2x);
        h2 = (to_unsigned_hash(h2x) & (table->size - 1)) | 1;
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

    if (table->count << 1 >= table->mcount)
      size = oldsize << 1;
    else
      size = oldsize;
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

  lkey = PTR_TO_LONG((Scheme_Object *)key);
  h = lkey & mask;
  h2 = (lkey >> 1) & mask;

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

  lkey = PTR_TO_LONG((Scheme_Object *)key);
  h = lkey & mask;
  h2 = (lkey >> 1) & mask;

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

int scheme_hash_table_equal_rec(Scheme_Hash_Table *t1, Scheme_Object *orig_t1,
                                Scheme_Hash_Table *t2, Scheme_Object *orig_t2,
                                void *eql)
{
  Scheme_Object **vals, **keys, *val1, *val2, *key;
  int i;

  if ((t1->count != t2->count)
      || (t1->make_hash_indices != t2->make_hash_indices)
      || (t1->compare != t2->compare))
    return 0;
    
  keys = t1->keys;
  vals = t1->vals;
  for (i = t1->size; i--; ) {
    if (vals[i]) {
      key = keys[i];

      if (!SAME_OBJ((Scheme_Object *)t1, orig_t1))
        val1 = scheme_chaperone_hash_traversal_get(orig_t1, key, &key);
      else
        val1 = vals[i];
      
      if (!SAME_OBJ((Scheme_Object *)t2, orig_t2))
        val2 = scheme_chaperone_hash_get(orig_t2, key);
      else
        val2 = scheme_hash_get(t2, key);

      if (!val2)
	return 0;
      if (!scheme_recur_equal(val1, val2, eql))
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

void scheme_clear_bucket_table(Scheme_Bucket_Table *bt)
{
  Scheme_Bucket **ba;

  bt->count = 0;
  bt->size = 4;
  ba = (Scheme_Bucket **)scheme_malloc(bt->size * sizeof(Scheme_Bucket **));
  bt->buckets = ba;
}

static Scheme_Bucket *
allocate_bucket (Scheme_Bucket_Table *table, const char *key, void *val)
{
  size_t bsize;
  Scheme_Type type;
  Scheme_Bucket *bucket;

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
  bucket->val = val;

  return bucket;
}

static Scheme_Bucket *
get_bucket (Scheme_Bucket_Table *table, const char *key, int add, Scheme_Bucket *b)
{
  intptr_t hx, h2x;
  hash_v_t h, h2;
  Scheme_Bucket *bucket;
  Compare_Proc compare = table->compare;
  uintptr_t mask;

 rehash_key:

  mask = table->size - 1;

  if (table->make_hash_indices) {
    table->make_hash_indices((void *)key, &hx, &h2x);
    h = to_unsigned_hash(hx) & mask;
    h2 = to_unsigned_hash(h2x) & mask;
  } else {
    uintptr_t lkey;
    lkey = PTR_TO_LONG((Scheme_Object *)key);
    h = lkey & mask;
    h2 = (lkey >> 1) & mask;
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

  if (b)
    bucket = b;
  else
    bucket = allocate_bucket(table, key, NULL);

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

int scheme_bucket_table_equal_rec(Scheme_Bucket_Table *t1, Scheme_Object *orig_t1,
                                  Scheme_Bucket_Table *t2, Scheme_Object *orig_t2,
                                  void *eql)
{
  Scheme_Bucket **buckets, *bucket;
  Scheme_Object *key, *val1, *val2;
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
	key = (Scheme_Object *)HT_EXTRACT_WEAK(bucket->key);
      } else {
	key = (Scheme_Object *)bucket->key;
      }
      if (key) {
        if (!SAME_OBJ((Scheme_Object *)t1, orig_t1))
          val1 = scheme_chaperone_hash_traversal_get(orig_t1, key, &key);
        else
          val1 = (Scheme_Object *)bucket->val;

	checked++;
      
        if (!SAME_OBJ((Scheme_Object *)t2, orig_t2))
          val2 = scheme_chaperone_hash_get(orig_t2, key);
        else
          val2 = (Scheme_Object *)scheme_lookup_in_table(t2, (const char *)key);

	if (!val2)
	  return 0;
	if (!scheme_recur_equal(val1, val2, eql))
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
	key = (Scheme_Object *)HT_EXTRACT_WEAK(bucket->key);
      } else {
	key = (Scheme_Object *)bucket->key;
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
    Scheme_Bucket **ba, *bucket;
    int i;
    asize = (size_t)table->size * sizeof(Scheme_Bucket *);
    ba = (Scheme_Bucket **)scheme_malloc(asize);
    table->buckets = ba;
    memcpy(ba, bt->buckets, asize);
    /* clone individual buckets */
    for (i = table->size; i--; ) {
      bucket = ba[i];
      if (bucket) {
        if (bucket->key) {
          if (table->weak) {
            void *hk = (void *)HT_EXTRACT_WEAK(bucket->key);
            if (hk)
              bucket = allocate_bucket(table, hk, bucket->val);
          } else
            bucket = allocate_bucket(table, bucket->key, bucket->val);
          ba[i] = bucket;
        }
      }
    }
  }

  return table;
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
  return to_signed_hash(PTR_TO_LONG(o));
}

END_XFORM_SKIP;

#endif

/*========================================================================*/
/*                           equal? hashing                               */
/*========================================================================*/

typedef struct Hash_Info {
  intptr_t depth; /* always odd */
  Scheme_Object *recur;
  Scheme_Object *insp; /* obtained lazily */
} Hash_Info;

static uintptr_t equal_hash_key(Scheme_Object *o, uintptr_t k, Hash_Info *hi);
static uintptr_t equal_hash_key2(Scheme_Object *o, Hash_Info *hi);

static Scheme_Object *hash_recur(int argc, Scheme_Object **argv, Scheme_Object *prim)
{
  intptr_t v;
  Hash_Info *hi;

  hi = (Hash_Info *)SCHEME_PRIM_CLOSURE_ELS(prim)[0];
  hi->depth += 2;
  hi->insp = NULL; /* in case recursive call is `parameterize'd */

  v = to_signed_hash(equal_hash_key(argv[0], 0, hi));

  hi->insp = NULL;
  
  return scheme_make_integer(v);
}

static Scheme_Object *hash_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v = (Scheme_Object *)p->ku.k.p1;
  Hash_Info *hi = (Hash_Info *)p->ku.k.p2;
  uintptr_t nv;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  
  nv = equal_hash_key(v, to_unsigned_hash(p->ku.k.i1), hi);

  return scheme_make_integer_value(to_signed_hash(nv));
}

static uintptr_t overflow_equal_hash_key(Scheme_Object *o, uintptr_t k, Hash_Info *hi)
{
  Scheme_Object *nv;
  intptr_t val;
  Hash_Info *hi2;
  Scheme_Thread *p = scheme_current_thread;

  hi2 = (Hash_Info *)scheme_malloc(sizeof(Hash_Info));
  memcpy(hi2, hi, sizeof(Hash_Info));

  p->ku.k.p1 = (void *)o;
  p->ku.k.p2 = (void *)hi2;
  p->ku.k.i1 = to_signed_hash(k);

  nv = scheme_handle_stack_overflow(hash_k);
  scheme_get_int_val(nv, &val);

  memcpy(hi, hi2, sizeof(Hash_Info));

  return to_unsigned_hash(val);
}

XFORM_NONGCING static uintptr_t dbl_hash_val(double d) 
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

  return ((uintptr_t)(d * (1 << 30))) + e;
}

XFORM_NONGCING static uintptr_t dbl_hash2_val(double d)  
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
  return to_unsigned_hash(e);
}

#ifdef MZ_LONG_DOUBLE
XFORM_NONGCING static uintptr_t long_dbl_hash_val(long_double d) 
  XFORM_SKIP_PROC
{
  int e;
  
  if (MZ_IS_LONG_NAN(d)) {
    d = get_long_double_zero();
    e = 1000;
  } else if (MZ_IS_LONG_POS_INFINITY(d)) {
    d = get_long_double_one_half();
    e = 1000;
  } else if (MZ_IS_LONG_NEG_INFINITY(d)) {
    d = long_double_neg(get_long_double_one_half());
    e = 1000;
  } else if (long_double_eqv(d, get_long_double_zero()) && scheme_long_minus_zero_p(d)) {
    d = get_long_double_zero();
    e = 1000;
  } else {
    /* frexpl should not be used on inf or nan: */
    d = long_double_frexp(d, &e);
  }

  return uintptr_from_long_double(long_double_mult_i(d, 1<<30)) + e;
  /*return ((uintptr_t)(d * (1 << 30))) + e;*/
}

XFORM_NONGCING static uintptr_t long_dbl_hash2_val(long_double d)  
  XFORM_SKIP_PROC
{
  int e;
  
  if (MZ_IS_LONG_NAN(d)
      || MZ_IS_LONG_POS_INFINITY(d)
      || MZ_IS_LONG_NEG_INFINITY(d)) {
    e = 1;
  } else {
    /* frexp should not be used on inf or nan: */
    d = long_double_frexp(d, &e);
  }
  return to_unsigned_hash(e);
}
#endif

#define OVERFLOW_HASH() overflow_equal_hash_key(o, k - t, hi)

/* Based on Bob Jenkins's one-at-a-time hash function at
   http://www.burtleburtle.net/bob/hash/doobs.html: */
#define MZ_MIX(k) (k += (k << 10), k ^= (k >> 6))

static uintptr_t equal_hash_key(Scheme_Object *o, uintptr_t k, Hash_Info *hi)
{
  Scheme_Type t;
  Scheme_Object *orig_obj;

 top:
  orig_obj = o;
  if (SCHEME_CHAPERONEP(o))
    o = ((Scheme_Chaperone *)o)->val;
  
  t = SCHEME_TYPE(o);
  k += t;

  if (hi->depth > (MAX_HASH_DEPTH << 1))
    return k;
  
  switch(t) {
  case scheme_integer_type:
    {
      uintptr_t iv = to_unsigned_hash(SCHEME_INT_VAL(o));
      MZ_MIX(iv);
      return k + iv;
    }
#ifdef MZ_USE_SINGLE_FLOATS
  case scheme_float_type:
#endif
  case scheme_double_type:
    {
      return k + dbl_hash_val(SCHEME_DBL_VAL(o));
    }
#ifdef MZ_LONG_DOUBLE
  case scheme_long_double_type:
    {
      return k + long_dbl_hash_val(SCHEME_LONG_DBL_VAL(o));
    }
#endif
  case scheme_bignum_type:
    {
      int i = SCHEME_BIGLEN(o);
      bigdig *d = SCHEME_BIGDIG(o), k2;
      
      k2 = k;
      while (i--) {
	k2 = (k2 << 3) + k2 + d[i];
      }
    
      return (uintptr_t)k2;
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
      k += (uintptr_t)((char *)SCHEME_CPTR_VAL(o) + SCHEME_CPTR_OFFSET(o));
      return k;
    }
  case scheme_vector_type:
  case scheme_fxvector_type:
  case scheme_wrap_chunk_type:
    {
      int len = SCHEME_VEC_SIZE(o), i, val;
      Scheme_Object *elem;
#     include "mzhashchk.inc"

      if (!len)
	return k + 1;
      
      hi->depth += 2;
      --len;
      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
        if (SAME_OBJ(o, orig_obj))
          elem = SCHEME_VEC_ELS(o)[i];
        else
          elem = scheme_chaperone_vector_ref(orig_obj, i);
	val = equal_hash_key(elem, 0, hi);
	k = (k << 5) + k + val;
      }
      
      if (SAME_OBJ(o, orig_obj))
        o = SCHEME_VEC_ELS(o)[len];
      else
        o = scheme_chaperone_vector_ref(orig_obj, len);

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
#ifdef MZ_LONG_DOUBLE
  case scheme_extflvector_type:
    {
      intptr_t len = SCHEME_EXTFLVEC_SIZE(o), i;
      long_double d;

      if (!len)
	return k + 1;
      
      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
	d = SCHEME_EXTFLVEC_ELS(o)[i];
        k = (k << 5) + k + long_dbl_hash_val(d);
      }
      
      return k;
    }
#endif
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
  case scheme_regexp_type:
    {
      o = scheme_regexp_source(o);
      break;
    }
  case scheme_structure_type:
  case scheme_proc_struct_type:
    {
      Scheme_Object *procs;

      procs = scheme_struct_type_property_ref(scheme_equal_property, orig_obj);
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

        a[0] = orig_obj;
        a[1] = recur;
        
        procs = SCHEME_VEC_ELS(procs)[2];
        
        v = _scheme_apply(procs, 2, a);

        if (SCHEME_INTP(v))
          return k + SCHEME_INT_VAL(v);
        else if (SCHEME_BIGNUMP(v)) {
          return k + (uintptr_t)((Scheme_Bignum *)v)->digits[0];
        } else {
          scheme_contract_error("equal-hash-code",
                                "hash procedure returned a value other than an exact integer",
                                "resul1", 1, v,
                                NULL);
          return 0;
        }
      } else {
        Scheme_Object *insp;
        if (scheme_struct_is_transparent(o))
          insp = NULL;
        else {
          insp = hi->insp;
          if (!insp) {
            insp = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
            hi->insp = insp;
          }
        }
        if (!insp || scheme_inspector_sees_part(o, insp, -2)) {
          int i;
          Scheme_Structure *s1 = (Scheme_Structure *)o;
          Scheme_Object *elem;
	
#         include "mzhashchk.inc"
	
          hi->depth += 2;

          for (i = SCHEME_STRUCT_NUM_SLOTS(s1); i--; ) {
            if (SAME_OBJ(o, orig_obj))
              elem = s1->slots[i];
            else
              elem = scheme_struct_ref(orig_obj, i);
            k += equal_hash_key(elem, 0, hi);
            MZ_MIX(k);
          }
	
          return k;
        } else
          return k + PTR_TO_LONG(o);
      }
    }
  case scheme_box_type:
    {
      SCHEME_USE_FUEL(1);
      k += 1;
      if (SAME_OBJ(o, orig_obj))
        o = SCHEME_BOX_VAL(o);
      else
        o = scheme_unbox(orig_obj);
      hi->depth += 2;
      break;
    }
  case scheme_hash_table_type:
    {
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)o;
      Scheme_Object **vals, **keys, *key, *val;
      int i;
      uintptr_t vk;
      intptr_t old_depth;

#     include "mzhashchk.inc"

      k = (k << 1) + 3;
      hi->depth *= 2; /* mult to counteract potential explosion due to old_depth reset */
      old_depth = hi->depth;
      
      keys = ht->keys;
      vals = ht->vals;
      for (i = ht->size; i--; ) {
	if (vals[i]) {
          key = keys[i];
          if (SAME_OBJ(o, orig_obj))
            val = vals[i];
          else
            val = scheme_chaperone_hash_traversal_get(orig_obj, key, &key);
          vk = equal_hash_key(key, 0, hi);
          MZ_MIX(vk);
	  vk += equal_hash_key(val, 0, hi);
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
      uintptr_t vk;
      intptr_t old_depth;

#     include "mzhashchk.inc"

      k = (k << 1) + 3;
      hi->depth += 2;
      old_depth = hi->depth;

      for (i = scheme_hash_tree_next(ht, -1); i != -1; i = scheme_hash_tree_next(ht, i)) {
        scheme_hash_tree_index(ht, i, &ik, &iv);
        if (!SAME_OBJ(o, orig_obj))
          iv = scheme_chaperone_hash_traversal_get(orig_obj, ik, &ik);
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
      const char *_key;
      Scheme_Object *key, *val;
      int i, weak;
      uintptr_t vk;
      intptr_t old_depth;
  
#    include "mzhashchk.inc"

      buckets = ht->buckets;
      weak = ht->weak;
      hi->depth += 2;
      old_depth = hi->depth;
      
      k = (k << 1) + 7;
      
      for (i = ht->size; i--; ) {
	bucket = buckets[i];
	if (bucket) {
	  if (weak)
	    _key = (const char *)HT_EXTRACT_WEAK(bucket->key);
	  else
	    _key = bucket->key;
	  if (_key) {
            key = (Scheme_Object *)_key;
            if (SAME_OBJ(o, orig_obj))
              val = (Scheme_Object *)bucket->val;
            else
              val = scheme_chaperone_hash_traversal_get(orig_obj, key, &key);
	    vk = equal_hash_key(val, 0, hi);
            MZ_MIX(vk);
	    vk += equal_hash_key(key, 0, hi);
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
	return k + PTR_TO_LONG(o);
    }
# endif
  case scheme_resolved_module_path_type:
    /* Needed for interning */
    {
      k += 7;
      o = SCHEME_PTR_VAL(o);
    }
    break;
  case scheme_module_index_type:
    {
      Scheme_Modidx *midx = (Scheme_Modidx *)o;
#     include "mzhashchk.inc"
      hi->depth += 2;
      k++;
      k = (k << 3) + k;
      k += equal_hash_key(midx->path, 0, hi);
      o = midx->base;
      break;
    }
    break;
  case scheme_place_bi_channel_type:
    {
      k += 7;
      /* a bi channel has sendch and recvch, but
         sends are the same iff recvs are the same: */
      o = (Scheme_Object *)((Scheme_Place_Bi_Channel *)o)->link->sendch;
    }
    break;
  default:    
    {
      Scheme_Primary_Hash_Proc h1 = scheme_type_hash1s[t];
      if (h1)
        return h1(o, k, hi);
      else
        return k + PTR_TO_LONG(o);
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
  hi.insp = NULL;

  return to_signed_hash(equal_hash_key(o, 0, &hi));
}

intptr_t scheme_equal_hash_key2(Scheme_Object *o)
{
  Hash_Info hi;

  hi.depth = 1;
  hi.recur = NULL;
  hi.insp = NULL;

  return to_signed_hash(equal_hash_key2(o, &hi));
}

intptr_t scheme_eqv_hash_key(Scheme_Object *o)
{
  if (!SCHEME_INTP(o) && (SCHEME_NUMBERP(o) || SCHEME_CHARP(o)))
    return to_signed_hash(scheme_equal_hash_key(o));
  else
    return to_signed_hash(PTR_TO_LONG(o));
}

intptr_t scheme_eqv_hash_key2(Scheme_Object *o)
{
  if (!SCHEME_INTP(o) && (SCHEME_NUMBERP(o) || SCHEME_CHARP(o)))
    return to_signed_hash(scheme_equal_hash_key2(o));
  else
    return to_signed_hash(PTR_TO_LONG(o) >> 1);
}

static Scheme_Object *hash2_recur(int argc, Scheme_Object **argv, Scheme_Object *prim)
{
  intptr_t v;
  Hash_Info *hi;

  hi = (Hash_Info *)SCHEME_PRIM_CLOSURE_ELS(prim)[0];
  hi->depth += 2;

  v = to_signed_hash(equal_hash_key2(argv[0], hi));
  
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
  
  nv = to_signed_hash(equal_hash_key2(v, hi));

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

static uintptr_t equal_hash_key2(Scheme_Object *o, Hash_Info *hi)
{
  Scheme_Type t;
  Scheme_Object *orig_obj;

 top:
  orig_obj = o;
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
#ifdef MZ_LONG_DOUBLE
  case scheme_long_double_type:
    {
      return long_dbl_hash2_val(SCHEME_LONG_DBL_VAL(o));
    }
#endif
  case scheme_bignum_type:
    return SCHEME_BIGDIG(o)[0];
  case scheme_rational_type:
    return equal_hash_key2(scheme_rational_numerator(o), hi);
  case scheme_complex_type:
    {
      uintptr_t v1, v2;
      Scheme_Complex *c = (Scheme_Complex *)o;
      v1 = equal_hash_key2(c->r, hi);
      v2 = equal_hash_key2(c->i, hi);
      return v1 + v2;
    }
  case scheme_pair_type:
    {
      uintptr_t v1, v2;
#     include "mzhashchk.inc"
      hi->depth += 2;
      v1 = equal_hash_key2(SCHEME_CAR(o), hi);
      v2 = equal_hash_key2(SCHEME_CDR(o), hi);
      return v1 + v2;
    }
  case scheme_mutable_pair_type:
    {
      uintptr_t v1, v2;
#     include "mzhashchk.inc"
      hi->depth += 2;
      v1 = equal_hash_key2(SCHEME_CAR(o), hi);
      v2 = equal_hash_key2(SCHEME_CDR(o), hi);
      return v1 + v2;
    }
  case scheme_cpointer_type:
    {
      return (uintptr_t)((char *)SCHEME_CPTR_VAL(o) + SCHEME_CPTR_OFFSET(o));
    }
  case scheme_vector_type:
  case scheme_fxvector_type:
  case scheme_wrap_chunk_type:
    {
      int len = SCHEME_VEC_SIZE(o), i;
      uintptr_t k = 0;
      Scheme_Object *elem;

#     include "mzhashchk.inc"

      hi->depth += 2;

      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
        if (SAME_OBJ(o, orig_obj))
          elem = SCHEME_VEC_ELS(o)[i];
        else
          elem = scheme_chaperone_vector_ref(orig_obj, i);
	k += equal_hash_key2(elem, hi);
      }
      
      return k;
    }
  case scheme_flvector_type:
    {
      intptr_t len = SCHEME_FLVEC_SIZE(o), i;
      double d;
      uintptr_t k = 0;

      if (!len)
	return k + 1;
      
      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
	d = SCHEME_FLVEC_ELS(o)[i];
        k = (k << 5) + k + dbl_hash2_val(d);
      }
      
      return k;
    }
#ifdef MZ_LONG_DOUBLE
  case scheme_extflvector_type:
    {
      intptr_t len = SCHEME_EXTFLVEC_SIZE(o), i;
      long_double d;
      uintptr_t k = 0;

      if (!len)
	return k + 1;
      
      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
	d = SCHEME_EXTFLVEC_ELS(o)[i];
        k = (k << 5) + k + long_dbl_hash2_val(d);
      }
      
      return k;
    }
#endif
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
  case scheme_regexp_type:
    {
      o = scheme_regexp_source(o);
      goto top;
    }
  case scheme_structure_type:
  case scheme_proc_struct_type:
    {
      Scheme_Object *procs;

      procs = scheme_struct_type_property_ref(scheme_equal_property, orig_obj);
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
        
        a[0] = orig_obj;
        a[1] = recur;
        
        procs = SCHEME_VEC_ELS(procs)[3];
        
        v = _scheme_apply(procs, 2, a);

        if (SCHEME_INTP(v))
          return SCHEME_INT_VAL(v);
        else if (SCHEME_BIGNUMP(v)) {
          return (uintptr_t)((Scheme_Bignum *)v)->digits[0];
        } else {
          scheme_contract_error("equal-secondary-hash-code",
                                "hash procedure returned a value other than an exact integer",
                                "result", 1, v,
                                NULL);
          return 0;
        }
      } else {
        Scheme_Object *insp;
        if (scheme_struct_is_transparent(o))
          insp = NULL;
        else {
          insp = hi->insp;
          if (!insp) {
            insp = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
            hi->insp = insp;
          }
        }
        if (!insp || scheme_inspector_sees_part(o, insp, -2)) {
          int i;
          uintptr_t k = 0;
          Scheme_Structure *s1 = (Scheme_Structure *)o;
          Scheme_Object *elem;
          
#         include "mzhashchk.inc"
	
          hi->depth += 2;

          for (i = SCHEME_STRUCT_NUM_SLOTS(s1); i--; ) {
            if (SAME_OBJ(o, orig_obj))
              elem = s1->slots[i];
            else
              elem = scheme_struct_ref(orig_obj, i);
            k += equal_hash_key2(elem, hi);
          }
          
          return k;
        } else
          return t;
      }
    }
  case scheme_box_type:
    if (SAME_OBJ(o, orig_obj))
      o = SCHEME_BOX_VAL(o);
    else
      o = scheme_unbox(orig_obj);
    hi->depth += 2;
    goto top;
  case scheme_hash_table_type:
    {
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)o;
      Scheme_Object **vals, **keys, *key, *val;
      int i;
      uintptr_t k = 0;
      intptr_t old_depth;
      
#     include "mzhashchk.inc"

      hi->depth *= 2; /* mult to counteract potential explosion due to old_depth reset */
      old_depth = hi->depth;

      keys = ht->keys;
      vals = ht->vals;
      for (i = ht->size; i--; ) {
	if (vals[i]) {
          key = keys[i];
          if (SAME_OBJ(o, orig_obj))
            val = vals[i];
          else
            val = scheme_chaperone_hash_traversal_get(orig_obj, key, &key);
	  k += equal_hash_key2(key, hi);
	  k += equal_hash_key2(val, hi);
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
      uintptr_t k = 0;
      intptr_t old_depth;
      
#     include "mzhashchk.inc"

      hi->depth += 2;
      old_depth = hi->depth;
      
      for (i = scheme_hash_tree_next(ht, -1); i != -1; i = scheme_hash_tree_next(ht, i)) {
        scheme_hash_tree_index(ht, i, &ik, &iv);
        if (!SAME_OBJ(o, orig_obj))
          iv = scheme_chaperone_hash_traversal_get(orig_obj, ik, &ik);
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
      const char *_key;
      Scheme_Object *key, *val;
      int i, weak;
      uintptr_t k = 0;
      intptr_t old_depth;

#     include "mzhashchk.inc"
  
      buckets = ht->buckets;
      weak = ht->weak;
      
      hi->depth += 2;
      old_depth = hi->depth;

      for (i = ht->size; i--; ) {
	bucket = buckets[i];
	if (bucket) {
	  if (weak)
	    _key = (const char *)HT_EXTRACT_WEAK(bucket->key);
	  else
	    _key = bucket->key;
	  if (_key) {
            key = (Scheme_Object *)_key;
            if (SAME_OBJ(o, orig_obj))
              val = (Scheme_Object *)bucket->val;
            else
              val = scheme_chaperone_hash_traversal_get(orig_obj, key, &key);
	    k += equal_hash_key2(val, hi);
	    k += equal_hash_key2(key, hi);
            hi->depth = old_depth;
	  }
	}
      }
    
      return k;
    }
  case scheme_resolved_module_path_type:
    /* Needed for interning */
    o = SCHEME_PTR_VAL(o);
    goto top;
  case scheme_module_index_type:
    {
      Scheme_Modidx *midx = (Scheme_Modidx *)o;
      uintptr_t v1, v2;
#     include "mzhashchk.inc"
      hi->depth += 2;
      v1 = equal_hash_key2(midx->path, hi);
      v2 = equal_hash_key2(midx->base, hi);
      return v1 + v2;
    }
  case scheme_place_bi_channel_type:
    /* a bi channel has sendch and recvch, but
       sends are the same iff recvs are the same: */
    o = (Scheme_Object *)((Scheme_Place_Bi_Channel *)o)->link->sendch;
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
  return to_signed_hash(equal_hash_key(o, 0, (Hash_Info *)cycle_data));
}

intptr_t scheme_recur_equal_hash_key2(Scheme_Object *o, void *cycle_data)
{
  return to_signed_hash(equal_hash_key2(o, (Hash_Info *)cycle_data));
}

/*========================================================================*/
/*                        functional hash tables                          */
/*========================================================================*/

typedef struct AVLNode {
  MZTAG_IF_REQUIRED
  char height;
  uintptr_t code;
  Scheme_Object *key; /* NULL => val is another tree for multiple key-value pairs */
  Scheme_Object *val;
  struct AVLNode *left;  
  struct AVLNode *right;
} AVLNode;

#if 0
# define AVL_ASSERT(p) if (p) { } else { scheme_signal_error("hash-tree assert failure %d", __LINE__); }
# define AVL_ASSERT_ONLY(x) x
# define AVL_CHECK_FORMS 1
#else
# define AVL_ASSERT(p) /* empty */
# define AVL_ASSERT_ONLY(x) /* empty */
#endif

XFORM_NONGCING static int get_height(AVLNode* t)
{
  if (t == NULL)
    return 0;
  else
    return t->height;
}

XFORM_NONGCING static void fix_height(AVLNode* t)
{
  int h;
  h = get_height(t->left);
  if (get_height(t->right) > h)
    h = get_height(t->right);
  t->height = h + 1;
}

static AVLNode *make_avl(AVLNode *left,
                         uintptr_t code, Scheme_Object *key, Scheme_Object *val,
                         AVLNode *right)
{
  AVLNode *avl;

  avl = scheme_malloc_small_dirty_tagged(sizeof(AVLNode));
  SET_REQUIRED_TAG(avl->type = scheme_rt_avl_node);
  avl->code = code;
  avl->key = key;
  avl->val = val;
  avl->left = left;
  avl->right = right;

  fix_height(avl);

  return avl;
}

static AVLNode *avl_clone(AVLNode *avl)
{
  AVLNode *naya;
  naya = MALLOC_ONE_TAGGED(AVLNode);
  memcpy(naya, avl, sizeof(AVLNode));
  return naya;
}

XFORM_NONGCING static int get_balance(AVLNode* t)
{
  return get_height(t->left) - get_height(t->right);
}

XFORM_NONGCING static AVLNode *avl_find(uintptr_t code, AVLNode *s)
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

#ifdef AVL_CHECK_FORMS
static AVLNode *AVL_CHK(AVLNode *avl, uintptr_t code)
{
  AVL_ASSERT(avl_find(code, avl));
  return avl;
}
static void AVL_CHK_FORM(AVLNode *avl)
{
  if (avl) {
    int h1, h2;
    h1 = get_height(avl->left);
    h2 = get_height(avl->right);
    if (h2 > h1) h1 = h2;
    AVL_ASSERT(avl->height == (h1 + 1));
    AVL_CHK_FORM(avl->left);
    AVL_CHK_FORM(avl->right);
  }
}
#else
# define AVL_CHK(avl, code) avl
XFORM_NONGCING static void AVL_CHK_FORM(AVLNode *avl)  { }
#endif
  
AVLNode* check_rotate_right(AVLNode* t)
{
  if (get_balance(t) == 2) {
    /* need to rotate right */
    AVLNode* left = t->left;
    left = avl_clone(left);
    if (get_balance(left) < 0) {
      /* double right rotation */
      AVLNode* left_right = left->right;
      left_right = avl_clone(left_right);
      left->right = left_right->left;
      left_right->left = left;
      fix_height(left);
      left = left_right;
    }
    t = avl_clone(t);
    t->left = left->right;
    left->right = t;
    fix_height(t);
    fix_height(left);
    return left;
  }
   
  return t;
}

AVLNode* check_rotate_left(AVLNode* t)
{
  if (get_balance(t) == -2) {
    /* need to rotate left */
    AVLNode* right = t->right;
    right = avl_clone(right);
    if (get_balance(right) > 0) {
      /* double left rotation */
      AVLNode* right_left = right->left;
      right_left = avl_clone(right_left);
      right->left = right_left->right;
      right_left->right = right;
      fix_height(right);
      right = right_left;
    } else
      right = avl_clone(right);
    t = avl_clone(t);
    t->right = right->left;
    right->left = t;
    fix_height(t);
    fix_height(right);
    return right;
  }
  
  return t;
}

static AVLNode *avl_ins(uintptr_t code, Scheme_Object *key, Scheme_Object *val, AVLNode *t)
{
  if (t == NULL)
    return AVL_CHK(make_avl(NULL, code, key, val, NULL), code);
  else {
    if (t->code > code) {
      /* insert on left */
      AVLNode *left;

      left = avl_ins(code, key, val, t->left);
      if (left == t->left)
        return t;
      
      t = avl_clone(t);
      t->left = left;
      fix_height(t);
      
      return check_rotate_right(t);
    } else if (t->code < code) {
      /* insert on right */
      AVLNode *right;
      
      right = avl_ins(code, key, val, t->right);
      if (right == t->right)
        return t;
      
      t = avl_clone(t);
      t->right = right;
      fix_height(t);
      
      return check_rotate_left(t);
    } else
      return t;
  }
}

static AVLNode* avl_del(AVLNode* t, uintptr_t code)
{
  if (t == NULL)
    return NULL;
  else {
    if (code < t->code) {
      /* delete on left */
      AVLNode *new_left;
      
      new_left = avl_del(t->left, code);
      if (new_left == t->left)
        return t;
      
      t = avl_clone(t);
      t->left = new_left;
      fix_height(t);
      return check_rotate_left(t);
    } else if (code > t->code) {
      /* delete on right */
      AVLNode *new_right;
      
      new_right = avl_del(t->right, code);
      if (new_right == t->right)
        return t;
      
      t = avl_clone(t);
      t->right = new_right;
      fix_height(t);
      return check_rotate_right(t);
    } else {
      if (!t->left)
        return t->right;
      else if (!t->right)
        return t->left;
      else {
        AVLNode *lm, *new_left;
        /* Get the max of the left: */
        for (lm = t->left; lm->right != NULL; lm = lm->right) {
        }
        /* Delete it: */
        new_left = avl_del(t->left, lm->code);
        /* Use it in place of t: */
        lm = avl_clone(lm);
        lm->left = new_left;
        lm->right = t->right;
        fix_height(lm);

        if (get_balance(lm) == -2)
          return check_rotate_left(lm);
        else
          return check_rotate_right(lm);
      }
    }
  }
}

static AVLNode *avl_replace(AVLNode *s, AVLNode *orig, AVLNode *naya)
{
  AVLNode *next;

  if (SAME_OBJ(s, orig))
    return naya;

  s = avl_clone(s);

  if (s->code > orig->code) {
    next = avl_replace(s->left, orig, naya);
    s->left = next;
  } else {
    next = avl_replace(s->right, orig, naya);
    s->right = next;
  }

  return s;
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

static intptr_t search_nodes(AVLNode *n, Scheme_Object *key, int kind)
/* O(N) search full tree to find a code for `key'; returns -1 if not found */
{
  intptr_t code;

  if ((kind && ((kind == 1)
                ? scheme_equal(n->key, key)
                : scheme_eqv(n->key, key)))
      || (!kind && SAME_OBJ(n->key, key)))
    return n->code;

  if (n->left) {
    code = search_nodes(n->left, key, kind);
    if (code >= 0) 
      return code;
  }

  if (n->right)
    return search_nodes(n->right, key, kind);
  else
    return -1;
}

XFORM_NONGCING static intptr_t search_nodes_eq(AVLNode *n, Scheme_Object *key)
/* O(N) search full tree to find a code for `key'; returns -1 if not found */
{
  intptr_t code;

  if (SAME_OBJ(n->key, key))
    return n->code;

  if (n->left) {
    code = search_nodes_eq(n->left, key);
    if (code >= 0) 
      return code;
  }

  if (n->right)
    return search_nodes_eq(n->right, key);
  else
    return -1;
}

XFORM_NONGCING static intptr_t fresh_code(AVLNode *root)
/* O(n) search for an available code */
{
  int i = 0;
  while (1) {
    if (!avl_find(i, root))
      return i;
    i++;
  }
}

static void *hash_tree_set(Scheme_Hash_Tree *tree, Scheme_Object *key, Scheme_Object *val, intptr_t h,
                           AVLNode *root, int kind)
{
  Scheme_Hash_Tree *tree2;
  AVLNode *added;
  int delta;

  AVL_CHK_FORM(root);

  if (!val) {
    /* Removing ... */
    added = avl_find(h, root);
    if (!added) {
       /* nothing to remove */
      return (tree ? (void *)tree : (void *)root);
    }
    if (added->key) {
      if ((kind && ((kind == 1)
                    ? scheme_equal(added->key, key)
                    : scheme_eqv(added->key, key)))
          || (!kind && SAME_OBJ(added->key, key))) {
        /* remove single item */
        root = avl_del(root, h);
        
        if (tree) {
          tree2 = MALLOC_ONE_TAGGED(Scheme_Hash_Tree);
          memcpy(tree2, tree, sizeof(Scheme_Hash_Tree));

          AVL_CHK_FORM(root);
          
          tree2->root = root;
          --tree2->count;
          
          return tree2;
        } else
          return root;
      } else {
        /* Nothing to remove */
        return (tree ? (void *)tree : (void *)root);
      }
    } else {
      /* multiple mappings; remove it below */
    }
  } else {
    /* Adding/setting: */
    root = avl_ins(h, NULL, NULL, root);
    added = avl_find(h, root);
  }

  delta = 0;
  
  if (added->val) {
    if (!added->key) {
      /* Have a subtree of keys and vals (with bogus "code"s). */
      AVLNode *savl = (AVLNode *)added->val;
      intptr_t code;
      code = search_nodes(savl, key, kind);
      if (code < 0) {
        /* Not mapped already: */
        if (!val) {
          /* nothing to remove after all */
          return (tree ? (void *)tree : (void *)root);
        }
        savl = (AVLNode *)hash_tree_set(NULL, key, val, fresh_code(savl), savl, kind);
        val = (Scheme_Object *)savl;
        key = NULL;
        delta = 1;
      } else {
        /* Mapped already: */
        savl = (AVLNode *)hash_tree_set(NULL, key, val, code, savl, kind);
        if (val) {
          /* Updated */
          val = (Scheme_Object *)savl;
          key = NULL;
        } else {
          /* Removed */
          delta = -1;
          if (!savl->left && !savl->right) {
            /* Removal reduced to a single mapping: */
            val = savl->val;
            key = savl->key;
          } else {
            val = (Scheme_Object *)savl;
            key = NULL;
          }
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
        /* Switch to sub-tree mode to hold mulitple keys for the
           same code: */
        static AVLNode *sn;

        /* avoid intermediate allocations by constructing directly: */
        sn = make_avl(NULL, 1, added->key, added->val, NULL);
        sn = make_avl(NULL, 0, key, val, sn);

        val = (Scheme_Object *)sn;
        key = NULL;
        delta = 1;
      }
    }
    root = avl_replace(root,
                       added,
                       make_avl(added->left,
                                added->code, key, val,
                                added->right));
  } else {
    added->key = key;
    added->val = val;
    delta = 1;
  }

  AVL_CHK_FORM(root);

  if (tree) {
    tree2 = MALLOC_ONE_TAGGED(Scheme_Hash_Tree);
    memcpy(tree2, tree, sizeof(Scheme_Hash_Tree));
    
    if (delta)
      tree2->count += delta;
    tree2->root = root;
    
    return tree2;
  } else
    return root;
}

Scheme_Hash_Tree *scheme_hash_tree_set(Scheme_Hash_Tree *tree, Scheme_Object *key, Scheme_Object *val)
{
  uintptr_t h;
  int kind = (SCHEME_HASHTR_FLAGS(tree) & 0x3);

  if (kind) {
    if (kind == 1) {
      h = to_unsigned_hash(scheme_equal_hash_key(key));
    } else {
      h = to_unsigned_hash(scheme_eqv_hash_key(key));
    }
  } else {
    h = PTR_TO_LONG((Scheme_Object *)key);
  }

  return (Scheme_Hash_Tree *)hash_tree_set(tree, key, val, h, tree->root, kind);
}

Scheme_Object *scheme_eq_hash_tree_get(Scheme_Hash_Tree *tree, Scheme_Object *key)
{
  uintptr_t h;
  AVLNode *avl;

  h = PTR_TO_LONG((Scheme_Object *)key);

  avl = avl_find(h, tree->root);
  if (avl) {
    if (!avl->key) {
      /* Have tree */
      AVLNode *savl = (AVLNode *)avl->val;
      intptr_t code;
      code = search_nodes_eq(savl, key);
      if (code >= 0) {
        avl = avl_find(code, savl);
        return avl->val;
      }
    } else if (SAME_OBJ(avl->key, key))
      return avl->val;
  }

  return NULL;
}

Scheme_Object *scheme_hash_tree_get(Scheme_Hash_Tree *tree, Scheme_Object *key)
{
  uintptr_t h;
  AVLNode *avl;
  int kind = (SCHEME_HASHTR_FLAGS(tree) & 0x3);

  if (kind) {
    if (kind == 1)
      h = to_unsigned_hash(scheme_equal_hash_key(key));
    else
      h = to_unsigned_hash(scheme_eqv_hash_key(key));
  } else {
    return scheme_eq_hash_tree_get(tree, key);
  }

  avl = avl_find(h, tree->root);
  if (avl) {
    if (!avl->key) {
      /* Have tree */
      AVLNode *savl = (AVLNode *)avl->val;
      intptr_t code;
      code = search_nodes(savl, key, kind);
      if (code >= 0) {
        avl = avl_find(code, savl);
        return avl->val;
      }
    } else {
      if (kind == 1) {
        if (scheme_equal(key, avl->key))
          return avl->val;
      } else {
        if (scheme_eqv(key, avl->key))
          return avl->val;
      }
    }
  }

  return NULL;
}

XFORM_NONGCING mzlonglong path_next(AVLNode *avl, mzlonglong path)
{
  if (!avl)
    return -1;
  
  if (!avl->key) {
    /* subtree choice */
    if (path & 0x1) {
      /* in subtree or right */
      if (!(path & 0x2)) {
        /* haven't exhausted the subtree, yet: */
        path >>= 2;
        path = path_next((AVLNode *)avl->val, path);
        if (path > 0)
          return (path << 2) | 0x1;
        path = 0x1; /* move on to right */
      } else {
        /* we have exhausted the subtree, and we're working on right */
        path >>= 1;
        /* assert: path & 0x1 */
      }
    }
  }

  if (path & 0x1) {
    path = path_next(avl->right, path >> 1);
    /* The result cannot be 0.
       If the result is -1, then the following calculation preserves the -1.
       If the result is positive, then we preserve the decision to go right here. */
    if (avl->key)
      return (path << 1) | 0x1;
    else
      return (path << 2) | 0x3;
  }

  path = path_next(avl->left, path >> 1);
  if (path > 0)
    return path << 1;

  /* start here */
  if (avl->key)
    return 0x1;
  else {
    /* start subtree */
    path = path_next((AVLNode *)avl->val, 0);
    return (path << 2) | 0x1;
  }
}

XFORM_NONGCING int path_find(AVLNode *avl, mzlonglong path, Scheme_Object **_key, Scheme_Object **_val)
{
  if (!avl) return 0;

  if (!avl->key) {
    /* subtree choice */
    if (path & 0x1) {
      /* in subtree or right */
      if (!(path & 0x2)) {
        /* in subtree */
        return path_find((AVLNode *)avl->val, path >> 2, _key, _val);
      } else {
        /* in right */
        path >>= 1;
        /* assert: path & 0x1 */
      }
    }
  }

  if (path & 0x1) {
    if (path >> 1)
      return path_find(avl->right, path >> 1, _key, _val);
    else {
      *_key = avl->key;
      *_val = avl->val;
      return 1;
    }
  } else
    return path_find(avl->left, path >> 1, _key, _val);
}

mzlonglong scheme_hash_tree_next(Scheme_Hash_Tree *tree, mzlonglong pos)
{
  /* Iteration uses a key where the bits say when to turn right */
  return path_next(tree->root, ((pos == -1) ? 0 : pos));
}

int scheme_hash_tree_index(Scheme_Hash_Tree *tree, mzlonglong pos, Scheme_Object **_key, Scheme_Object **_val)
{
  return path_find(tree->root, pos, _key, _val);
}

int scheme_hash_tree_equal_rec(Scheme_Hash_Tree *t1, Scheme_Object *orig_t1,
                               Scheme_Hash_Tree *t2, Scheme_Object *orig_t2,
                               void *eql)
{
  Scheme_Object *k, *v, *v2;
  int i;

  if ((t1->count != t2->count)
      || ((SCHEME_HASHTR_FLAGS(t1) & 0x3) != (SCHEME_HASHTR_FLAGS(t2) & 0x3)))
    return 0;
    
  for (i = scheme_hash_tree_next(t1, -1); i != -1; i = scheme_hash_tree_next(t1, i)) {
    scheme_hash_tree_index(t1, i, &k, &v);

    if (!SAME_OBJ((Scheme_Object *)t1, orig_t1))
      v = scheme_chaperone_hash_traversal_get(orig_t1, k, &k);
      
    if (!SAME_OBJ((Scheme_Object *)t2, orig_t2))
      v2 = scheme_chaperone_hash_get(orig_t2, k);
    else
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

#include "mzmark_hash.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_hash_tree_type, hash_tree_val);
  GC_REG_TRAV(scheme_rt_avl_node, mark_avl_node);
}

END_XFORM_SKIP;

#endif
