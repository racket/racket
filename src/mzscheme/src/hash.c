/*
  MzScheme
  Copyright (c) 2004-2007 PLT Scheme Inc.
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

int scheme_hash_request_count;
int scheme_hash_iteration_count;

#ifdef MZ_PRECISE_GC
static short keygen;
XFORM_NONGCING static MZ_INLINE
long PTR_TO_LONG(Scheme_Object *o)
{
  short v;

  if (SCHEME_INTP(o))
    return (long)o;

  v = o->keyex;

  if (!(v & 0xFFFC)) {
    if (!keygen)
      keygen += 4;
    v |= keygen;
    o->keyex = v;
    keygen += 4;
  }

  return (o->type << 16) | v;
}
#else
# define PTR_TO_LONG(p) ((long)(p))
#endif

#define FILL_FACTOR 1.4

#define MIN_HTABLE_SIZE 8

typedef int (*Hash_Compare_Proc)(void*, void*);

typedef unsigned long hash_v_t;

/*========================================================================*/
/*                         hashing functions                              */
/*========================================================================*/

static void string_hash_indices(void *_key, long *_h, long *_h2)
{
  const char *key = (char *)_key;
  long i, h, h2;

  h2 = h = i = 0;
  while (key[i]) {
    int c = key[i++];
    h += (h << 5) + h + c;
    h2 += c;
  }

  *_h = h;
  *_h2 = h2;
}

#ifdef PALMOS_STUFF
static int p_strcmp(char *a, char *b)
{
  return strcmp(a, b);
}
#endif

static void id_hash_indices(void *_key, long *_h, long *_h2)
{
  Scheme_Object *key = (Scheme_Object *)_key;
  long lkey;

  if (SCHEME_STXP(key))
    key = SCHEME_STX_VAL(key);
    
  lkey = PTR_TO_LONG((Scheme_Object *)key);
  *_h = (lkey >> 2);
  *_h2 = (lkey >> 3);
}

static int not_stx_bound_eq(char *a, char *b)
{
  return !scheme_stx_bound_eq((Scheme_Object *)a, (Scheme_Object *)b, 0);
}

/*========================================================================*/
/*                         normal hash table                              */
/*========================================================================*/

static Scheme_Object GONE[1];

Scheme_Hash_Table *scheme_make_hash_table(int type)
{
  Scheme_Hash_Table *table;

  table = MALLOC_ONE_TAGGED(Scheme_Hash_Table);

  table->size = 0;
    
  table->iso.so.type = scheme_hash_table_type;

  if (type == SCHEME_hash_string) {
    table->make_hash_indices = string_hash_indices;
#ifdef PALMOS_STUFF
    table->compare = (Hash_Compare_Proc)p_strcmp;
#else
    table->compare = (Hash_Compare_Proc)strcmp;
#endif
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
  unsigned long mask;

 rehash_key:

  mask = table->size - 1;

  if (table->make_hash_indices) {
    table->make_hash_indices((void *)key, (long *)&h, (long *)&h2);
    h = h & mask;
    h2 = h2 & mask;
  } else {
    unsigned long lkey;
    lkey = (unsigned long)PTR_TO_LONG((Scheme_Object *)key);
    h = (lkey >> 2) & mask;
    h2 = (lkey >> 3) & mask;
  }

  h2 |= 1;

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
  unsigned long mask;
  unsigned long lkey;
  int set = 2;

  mask = table->size - 1;

  lkey = (unsigned long)PTR_TO_LONG((Scheme_Object *)key);
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
  unsigned long mask;
  unsigned long lkey;

  mask = table->size - 1;

  lkey = (unsigned long)PTR_TO_LONG((Scheme_Object *)key);
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

int scheme_hash_table_equal(Scheme_Hash_Table *t1, Scheme_Hash_Table *t2)
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
      if (!scheme_equal(vals[i], v))
	return 0;
    }
  }

  return 1;
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
    memset(table->vals, 0, sizeof(Scheme_Object *) * table->size);
    memset(table->keys, 0, sizeof(Scheme_Object *) * table->size);
  } else {
    /* Shrink by one step */
    Scheme_Object **ba;
    table->size >>= 1;
    ba = MALLOC_N(Scheme_Object *, table->size);
    memcpy(ba, table->vals, sizeof(Scheme_Object *) * table->size);
    table->vals = ba;
    ba = MALLOC_N(Scheme_Object *, table->size);
    memcpy(ba, table->keys, sizeof(Scheme_Object *) * table->size);
    table->keys = ba;
  }
  table->count = 0;
  table->mcount = 0;
}

/*========================================================================*/
/*                  old-style hash table, with buckets                    */
/*========================================================================*/

Scheme_Bucket_Table *
scheme_make_bucket_table (int size, int type)
{
  Scheme_Bucket_Table *table;
  size_t asize;

  table = MALLOC_ONE_TAGGED(Scheme_Bucket_Table);

  table->size = 2;
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

  table->weak = (type == SCHEME_hash_weak_ptr);
  
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
  unsigned long mask;

 rehash_key:

  mask = table->size - 1;

  if (table->make_hash_indices) {
    table->make_hash_indices((void *)key, (long *)&h, (long *)&h2);
    h = h & mask;
    h2 = h2 & mask;
  } else {
    unsigned long lkey;
    lkey = (unsigned long)PTR_TO_LONG((Scheme_Object *)key);
    h = (lkey >> 2) & mask;
    h2 = (lkey >> 3) & mask;
  }

  h2 |= 0x1;

  if (table->weak) {
    scheme_hash_request_count++;
    while ((bucket = table->buckets[h])) {
      if (bucket->key) {
	void *hk = (void *)HT_EXTRACT_WEAK(bucket->key);
	if (!hk) {
	  if (add) {
	    /* Re-use a bucket slot whose key is collected: */
	    /* DON'T increment counter overall... */
	    --table->count;
	    break;
	  }
	} else if (SAME_PTR(hk, key))
	  return bucket;
	else if (compare && !compare((void *)hk, (void *)key))
	  return bucket;
      } else if (add)
	break;
      scheme_hash_iteration_count++;
      h = (h + h2) & mask;
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

      /* Forced GC: so that the new table is as small as possible. */
      scheme_collect_garbage();

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
      kb = GC_malloc_weak_box((void *)key, (void **)bucket, (void **)&bucket->val - (void **)bucket);
      bucket->key = (char *)kb;
#else
      char *kb;
      kb = (char *)MALLOC_ONE_WEAK(void *);
      bucket->key = kb;
      *(void **)bucket->key = (void *)key;
      scheme_weak_reference_indirect((void **)bucket->key, (void *)key);
      scheme_weak_reference_indirect((void **)&bucket->val, (void *)key);
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

int scheme_bucket_table_equal(Scheme_Bucket_Table *t1, Scheme_Bucket_Table *t2)
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
	if (!scheme_equal((Scheme_Object *)bucket->val, (Scheme_Object *)v))
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

/*========================================================================*/
/*                         precise GC hashing                             */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

void scheme_init_hash_key_procs(void)
{
  /* No initialization needed anymore. */
}

long scheme_hash_key(Scheme_Object *o)
{
  return PTR_TO_LONG(o);
}

END_XFORM_SKIP;

#endif

/*========================================================================*/
/*                           equal? hashing                               */
/*========================================================================*/

static long equal_hash_key(Scheme_Object *o, long k);

static Scheme_Object *hash_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v = (Scheme_Object *)p->ku.k.p1;
  long nv;

  p->ku.k.p1 = NULL;
  
  nv = equal_hash_key(v, p->ku.k.i1);

  return scheme_make_integer_value(nv);
}

/* Number of lists/vectors/structs/boxes to hash before
   paying for a stack check. */
#define HASH_COUNT_START 20

#define MZ_HASH_K hash_k
#define MZ_HASH_I1 (k - t)

/* Based on Bob Jenkins's one-at-a-time hash function at
   http://www.burtleburtle.net/bob/hash/doobs.html: */
#define MZ_MIX(k) (k += (k << 10), k ^= (k >> 6))

static long equal_hash_key(Scheme_Object *o, long k)
{
  Scheme_Type t;
  static int hash_counter = HASH_COUNT_START;

 top:
  t = SCHEME_TYPE(o);
  k += t;
  
  switch(t) {
  case scheme_integer_type:
    return k + SCHEME_INT_VAL(o);
#ifdef MZ_USE_SINGLE_FLOATS
  case scheme_float_type:
#endif
  case scheme_double_type:
    {
      double d;
      int e;
      d = SCHEME_DBL_VAL(o);
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
      return k + ((long)(d * (1 << 30))) + e;
    }
  case scheme_bignum_type:
    {
      int i = SCHEME_BIGLEN(o);
      bigdig *d = SCHEME_BIGDIG(o), k2;
      
      k2 = k;
      while (i--) {
	k2 = (k2 << 3) + k2 + d[i];
      }
    
      return (long)k2;
    }
    break;
  case scheme_rational_type:
    {
      k += equal_hash_key(scheme_rational_numerator(o), 0);
      o = scheme_rational_denominator(o);
      break;
    }
  case scheme_complex_type:
  case scheme_complex_izi_type:
    {
      Scheme_Complex *c = (Scheme_Complex *)o;
      k += equal_hash_key(c->r, 0);
      o = c->i;
      break;
    }
  case scheme_pair_type:
    {
#     include "mzhashchk.inc"
      k += equal_hash_key(SCHEME_CAR(o), 0);
      o = SCHEME_CDR(o);
      break;
    }
  case scheme_vector_type:
  case scheme_wrap_chunk_type:
    {
      int len = SCHEME_VEC_SIZE(o), i, val;
#     include "mzhashchk.inc"

      if (!len)
	return k + 1;
      
      --len;
      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
	val = equal_hash_key(SCHEME_VEC_ELS(o)[i], 0);
	k = (k << 5) + k + val;
      }
      
      o = SCHEME_VEC_ELS(o)[len];
      break;
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
      Scheme_Object *insp;
      insp = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
      if (scheme_inspector_sees_part(o, insp, -2)) {
	int i;
	Scheme_Structure *s1 = (Scheme_Structure *)o;
	
#       include "mzhashchk.inc"
	
	for (i = SCHEME_STRUCT_NUM_SLOTS(s1); i--; ) {
	  k += equal_hash_key(s1->slots[i], 0);
          MZ_MIX(k);
	}
	
	return k;
      } else
	return k + (PTR_TO_LONG(o) >> 4);
      break;
    }
  case scheme_box_type:
    {
      SCHEME_USE_FUEL(1);
      k += 1;
      o = SCHEME_BOX_VAL(o);
      break;
    }
  case scheme_hash_table_type:
    {
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)o;
      Scheme_Object **vals, **keys;
      int i;
      long vk;

#     include "mzhashchk.inc"

      k = (k << 1) + 3;
      
      keys = ht->keys;
      vals = ht->vals;
      for (i = ht->size; i--; ) {
	if (vals[i]) {
          vk = equal_hash_key(keys[i], 0);
          MZ_MIX(vk);
	  vk += equal_hash_key(vals[i], 0);
          MZ_MIX(vk);
          k += vk;  /* can't mix k, because the key order shouldn't matter */
	}
      }
      
      return k;
    }
  case scheme_bucket_table_type:
    {
      Scheme_Bucket_Table *ht = (Scheme_Bucket_Table *)o;
      Scheme_Bucket **buckets, *bucket;
      const char *key;
      int i, weak;
      long vk;
  
#    include "mzhashchk.inc"

      buckets = ht->buckets;
      weak = ht->weak;
      
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
	    vk = equal_hash_key((Scheme_Object *)bucket->val, 0);
            MZ_MIX(vk);
	    vk += equal_hash_key((Scheme_Object *)key, 0);
            MZ_MIX(vk);
            k += vk; /* can't mix k, because the key order shouldn't matter */
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
  default:    
    {
      Scheme_Primary_Hash_Proc h1 = scheme_type_hash1s[t];
      if (h1)
        return h1(o, k);
      else
        return k + (PTR_TO_LONG(o) >> 4);
    }
  }

  MZ_MIX(k);
  goto top;
}

long scheme_equal_hash_key(Scheme_Object *o)
{
  return equal_hash_key(o, 0);
}

static Scheme_Object *hash2_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v = (Scheme_Object *)p->ku.k.p1;
  long nv;

  p->ku.k.p1 = NULL;
  
  nv = scheme_equal_hash_key2(v);

  return scheme_make_integer(nv);
}

#undef MZ_HASH_K
#undef MZ_HASH_I1
#define MZ_HASH_K hash2_k
#define MZ_HASH_I1 0

long scheme_equal_hash_key2(Scheme_Object *o)
{
  Scheme_Type t;
  static int hash_counter = HASH_COUNT_START;

 top:
  t = SCHEME_TYPE(o);

  switch(t) {
  case scheme_integer_type:
    return t;
#ifdef MZ_USE_SINGLE_FLOATS
  case scheme_float_type:
    return t;
#endif
  case scheme_double_type:
    {
      double d;
      int e;
      d = SCHEME_DBL_VAL(o);
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
  case scheme_bignum_type:
    return SCHEME_BIGDIG(o)[0];
  case scheme_rational_type:
    return scheme_equal_hash_key2(scheme_rational_numerator(o));
  case scheme_complex_type:
  case scheme_complex_izi_type:
    {
      long v1, v2;
      Scheme_Complex *c = (Scheme_Complex *)o;
      v1 = scheme_equal_hash_key2(c->r);
      v2 = scheme_equal_hash_key2(c->i);
      return v1 + v2;
    }
  case scheme_pair_type:
    {
      long v1, v2;
#     include "mzhashchk.inc"
      v1 = scheme_equal_hash_key2(SCHEME_CAR(o));
      v2 = scheme_equal_hash_key2(SCHEME_CDR(o));
      return v1 + v2;
    }
  case scheme_vector_type:
  case scheme_wrap_chunk_type:
    {
      int len = SCHEME_VEC_SIZE(o), i;
      long k = 0;

#     include "mzhashchk.inc"

      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
	k += scheme_equal_hash_key2(SCHEME_VEC_ELS(o)[i]);
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
      Scheme_Object *insp;
      insp = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
      if (scheme_inspector_sees_part(o, insp, -2)) {
	int i;
	long k = 0;
	Scheme_Structure *s1 = (Scheme_Structure *)o;
	
#       include "mzhashchk.inc"
	
	for (i = SCHEME_STRUCT_NUM_SLOTS(s1); i--; ) {
	  k += scheme_equal_hash_key2(s1->slots[i]);
	}
	
	return k;
      } else
	return t;
    }
  case scheme_box_type:
    o = SCHEME_BOX_VAL(o);
    goto top;
  case scheme_hash_table_type:
    {
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)o;
      Scheme_Object **vals, **keys;
      int i;
      long k = 0;
      
#     include "mzhashchk.inc"

      keys = ht->keys;
      vals = ht->vals;
      for (i = ht->size; i--; ) {
	if (vals[i]) {
	  k += scheme_equal_hash_key2(keys[i]);
	  k += scheme_equal_hash_key2(vals[i]);
	}
      }
      
      return k;
    }
  case scheme_bucket_table_type:
    {
      Scheme_Bucket_Table *ht = (Scheme_Bucket_Table *)o;
      Scheme_Bucket **buckets, *bucket;
      const char *key;
      int i, weak;
      long k = 0;

#     include "mzhashchk.inc"
  
      buckets = ht->buckets;
      weak = ht->weak;
      
      for (i = ht->size; i--; ) {
	bucket = buckets[i];
	if (bucket) {
	  if (weak) {
	    key = (const char *)HT_EXTRACT_WEAK(bucket->key);
	  } else {
	    key = bucket->key;
	  }
	  if (key) {
	    k += scheme_equal_hash_key((Scheme_Object *)bucket->val);
	    k += scheme_equal_hash_key((Scheme_Object *)key);
	  }
	}
      }
    
      return k;
    }
  default:
    {
      Scheme_Secondary_Hash_Proc h2 = scheme_type_hash2s[t];
      if (h2)
        return h2(o);
      else
        return t;
    }
  }
}
