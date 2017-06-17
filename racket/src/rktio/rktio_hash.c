#include "rktio.h"
#include "rktio_private.h"
#include <stdlib.h>

struct rktio_hash_t {
  struct bucket_t *buckets;
  intptr_t size, count;
};

typedef struct bucket_t {
  /* v is non-NULL => bucket is filled */
  /* v is NULL and fd is -1 => was removed */
  intptr_t key;
  void *v;
} bucket_t;

rktio_hash_t *rktio_hash_new(void)
{
  return calloc(1, sizeof(rktio_hash_t));
}

void rktio_hash_free(rktio_hash_t *ht, int free_values)
{
  if (ht->buckets) {
    intptr_t i;

    if (free_values) {
      for (i = ht->size; --i; ) {
        if (ht->buckets[i].v)
          free(ht->buckets[i].v);
      }
    }
    
    free(ht->buckets);
  }

  free(ht);
}

int rktio_hash_is_empty(rktio_hash_t *ht)
{
  return (ht->count == 0);
}

intptr_t rktio_hash_size(rktio_hash_t *ht)
{
  return ht->size;
}

intptr_t rktio_hash_get_key(rktio_hash_t *ht, intptr_t i)
{
  if (ht->buckets[i].v)
    return ht->buckets[i].key;
  else
    return -1;
}

static void do_rehash(rktio_hash_t *ht, intptr_t new_size)
{
  if (new_size >= 16) {
    bucket_t *old_buckets = ht->buckets;
    intptr_t old_size = ht->size, i;

    ht->size = new_size;
    ht->buckets = calloc(new_size, sizeof(bucket_t));
    ht->count = 0;

    for (i = old_size; i--; ) {
      if (old_buckets[i].v)
        rktio_hash_set(ht, old_buckets[i].key, old_buckets[i].v);
    }

    free(old_buckets);
  }
}

void *rktio_hash_get(rktio_hash_t *ht, intptr_t key)
{
  if (ht->buckets) {
    intptr_t mask = (ht->size - 1);
    intptr_t hc = key & mask;
    intptr_t init_hc = hc;
    intptr_t d = ((key >> 3) & mask) | 0x1;

    while (1) {
      if (ht->buckets[hc].key == key)
        return ht->buckets[hc].v;
      else if (ht->buckets[hc].v
               || (ht->buckets[hc].key == -1)) {
        /* keep looking */
        hc = (hc + d) & mask;
        if (hc == init_hc) {
          /* didn't find in a table that has only full and vacated slots */
          return NULL;
        }
      } else
        return NULL;
    }
  } else
    return NULL;
}

void rktio_hash_remove(rktio_hash_t *ht, intptr_t key, int dont_rehash)
{
  if (ht->buckets) {
    intptr_t mask = (ht->size - 1);
    intptr_t hc = key & mask;
    intptr_t init_hc = hc;
    intptr_t d = ((key >> 3) & mask) | 0x1;
    
    while (1) {
      if (ht->buckets[hc].key == key) {
        ht->buckets[hc].key = -1;
        ht->buckets[hc].v = NULL;
        --ht->count;
        if (!dont_rehash) {
          if (4 * ht->count <= ht->size)
            do_rehash(ht, ht->size >> 1);
        }
        return;
      } else if (ht->buckets[hc].v
                 || (ht->buckets[hc].key == -1)) {
        /* keep looking */
        hc = (hc + d) & mask;
        if (hc == init_hc) {
          /* didn't find in a table that has only full and vacated slots */
          return;
        }
      } else
        break;
    }
  }
}

void rktio_hash_set(rktio_hash_t *ht, intptr_t key, void *v)
{
  if (!ht->buckets) {
    ht->size = 16;
    ht->buckets = calloc(ht->size, sizeof(bucket_t));
  }

  {
    intptr_t mask = (ht->size - 1);
    intptr_t hc = key & mask;
    intptr_t init_hc = hc,use_hc = -1;
    intptr_t d = ((key >> 3) & mask) | 0x1;
    
    while (1) {
      if (ht->buckets[hc].v) {
        if (ht->buckets[hc].key == key) {
          ht->buckets[hc].v = v;
          return;
        } else {
          if (ht->buckets[hc].key == -1) {
            /* use bucket whose content was previously removed, but only
               if we don't find it later */
            if (use_hc < 0) use_hc = hc;
          }
          /* keep looking for a spot */
          hc = (hc + d) & mask;
          if (hc == init_hc) {
            /* didn't find in a table that has only full and vacated slots */
            break;
          }
        }
      } else {
        if (use_hc < 0) use_hc = hc;
        break;
      }
    }
    
    ht->buckets[use_hc].key = key;
    ht->buckets[use_hc].v = v;
    ht->count++;

    if (2 * ht->count >= ht->size)
      do_rehash(ht, ht->size << 1);
  }
}
