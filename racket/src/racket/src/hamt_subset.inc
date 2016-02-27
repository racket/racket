/* Parameterize subset_of code over the euqality predicate, so that
   an eq variant (for small enough tables) can be marked as non-GCing */

HAMT_NONGCING static int HAMT_ELEMENT_OF_COLLISION(Scheme_Object *key1, Scheme_Object *val1,
                                                   Scheme_Hash_Tree *t2,
                                                   int stype, void *eql_data)
/* linear search for an element */
{
  int i;
  Scheme_Object *key2;
  HAMT_IF_VAL(Scheme_Object *val2, );
  
  for (i = t2->count; i--; ) {
    hamt_at_index(t2, i, &key2, HAMT_IF_VAL(&val2, NULL), NULL);
    if (HAMT_EQUAL_ENTRIES(stype, eql_data,
                           key1, val1,
                           key2, HAMT_IF_VAL(val2, NULL)))
      return 1;
  }

  return 0;
}

HAMT_NONGCING static int HAMT_ELEMENT_OF(Scheme_Object *key1, Scheme_Object *val1, uintptr_t code1,
                                         Scheme_Hash_Tree *t2, int shift,
                                         int stype, void *eql_data)
/* search for one element in a subtree */
{
  int pos2;
  
  t2 = hamt_assoc(t2, code1, &pos2, shift);
  if (t2) {
    if (HASHTR_COLLISIONP(t2->els[pos2]))
      return HAMT_ELEMENT_OF_COLLISION(key1, val1, (Scheme_Hash_Tree *)t2->els[pos2], stype, eql_data);
    else
      return HAMT_EQUAL_ENTRIES(stype, eql_data,
                                key1, val1,
                                t2->els[pos2], HAMT_IF_VAL(mzHAMT_VAL(t2, pos2), NULL));
  } else
    return 0;
}

HAMT_NONGCING int HAMT_SUBSET_OF(Scheme_Hash_Tree *t1, Scheme_Hash_Tree *t2, int shift,
                                 int stype, void *eql_data)
/* checks wheher `t1` is a subset of `t2`; `t1` and `t2` must be of the same kind */
{
  hash_tree_bitmap_t i;
  int pos1, pos2, index, popcount1, popcount2;
  Scheme_Object *k1, *k2;
  
  if ((t1->bitmap & t2->bitmap) != t1->bitmap)
    return 0;

  popcount1 = hamt_popcount(t1->bitmap);
  popcount2 = hamt_popcount(t2->bitmap);

  for (i = t1->bitmap, pos1 = 0, index = 0; i; ) {
    if (i & 1) {
      pos2 = hamt_popcount_below(t2->bitmap, index);
      k1 = t1->els[pos1];
      k2 = t2->els[pos2];
      if (SAME_OBJ(k1, k2)) {
        if (HAMT_IF_VAL(0, 1)
            || HASHTR_SUBTREEP(k1)
            || HASHTR_COLLISIONP(k1)) {
          /* Shared element, subtree, or collision; no need to look further */
        } else {
          /* need to compare values */
          if (!HAMT_EQUAL_ENTRIES(stype, eql_data,
                                  k1, HAMT_IF_VAL(_mzHAMT_VAL(t1, pos1, popcount1), NULL),
                                  k2, HAMT_IF_VAL(_mzHAMT_VAL(t2, pos2, popcount2), NULL)))
            return 0;
        }
      } else if (HASHTR_SUBTREEP(k1)) {
        /* Since a subtree always has at least two items with different
           hashes, t2 must have a subtree in the same position */
        if (HASHTR_SUBTREEP(k2)) {
          if (!HAMT_SUBSET_OF((Scheme_Hash_Tree *)k1,
                              (Scheme_Hash_Tree *)k2,
                              shift + mzHAMT_LOG_WORD_SIZE,
                              stype, eql_data))
            return 0;
        } else
          return 0;
      } else if (HASHTR_COLLISIONP(k1)) {
        intptr_t i;
        Scheme_Object *key;
        HAMT_IF_VAL(Scheme_Object *val, );
        
        if (HASHTR_SUBTREEP(k2)) {
          /* check each element of collision */
          for (i = ((Scheme_Hash_Tree *)k1)->count; i--; ) {
            uintptr_t code;
            hamt_at_index(((Scheme_Hash_Tree *)k1), i, &key, HAMT_IF_VAL(&val, NULL), &code);
            if (!HAMT_ELEMENT_OF(key, HAMT_IF_VAL(val, NULL), code,
                                 (Scheme_Hash_Tree *)k2,
                                 shift + mzHAMT_LOG_WORD_SIZE,
                                 stype, eql_data))
              return 0;
          }
        } else if (HASHTR_COLLISIONP(k2)) {
          /* hash codes of collisions must match */
          if (_mzHAMT_CODE(t1, pos1, popcount1) != _mzHAMT_CODE(t2, pos2, popcount2))
            return 0;
          /* must check each element of t1 in t2 */
          for (i = ((Scheme_Hash_Tree *)k1)->count; i--; ) {
            hamt_at_index((Scheme_Hash_Tree *)k1, i, &key, HAMT_IF_VAL(&val, NULL), NULL);
            if (!HAMT_ELEMENT_OF_COLLISION(key, HAMT_IF_VAL(val, NULL),
                                           (Scheme_Hash_Tree *)k2,
                                           stype, eql_data))
              return 0;
          }
        } else {
          /* A single element in t2 can't cover eveything in the collision */
          return 0;
        }
      } else {
        if (HASHTR_SUBTREEP(k2)) {
          if (!HAMT_ELEMENT_OF(k1, HAMT_IF_VAL(_mzHAMT_VAL(t1, pos1, popcount1), NULL),
                               _mzHAMT_CODE(t1, pos1, popcount1),
                               (Scheme_Hash_Tree *)k2,
                               shift + mzHAMT_LOG_WORD_SIZE,
                               stype, eql_data))
            return 0;
        } else {
          /* two elements or an element and a collision;
             hash codes much match either way */
          if (_mzHAMT_CODE(t1, pos1, popcount1) != _mzHAMT_CODE(t2, pos2, popcount2))
            return 0;
          if (HASHTR_COLLISIONP(k2)) {
            /* look for an invidual value in t2: */
            if (!HAMT_ELEMENT_OF_COLLISION(k1, HAMT_IF_VAL(_mzHAMT_VAL(t1, pos1, popcount1), NULL),
                                           (Scheme_Hash_Tree *)k2,
                                           stype, eql_data))
              return 0;
          } else {
            if (!HAMT_EQUAL_ENTRIES(stype, eql_data,
                                    k1, HAMT_IF_VAL(_mzHAMT_VAL(t1, pos1, popcount1), NULL),
                                    k2, HAMT_IF_VAL(_mzHAMT_VAL(t2, pos2, popcount2), NULL)))
              return 0;
          }
        }
      }
      pos1++;
      HAMT_USE_FUEL(1);
      i >>= 1;
      index++;
    } else if (i & 0xFF) {
      i >>= 1;
      index++;
    } else {
      i >>= 8;
      index += 8;
    }
  }

  return 1;
}

#undef HAMT_NONGCING
#undef HAMT_SUBSET_OF
#undef HAMT_ELEMENT_OF
#undef HAMT_ELEMENT_OF_COLLISION
#undef HAMT_EQUAL_ENTRIES
#undef HAMT_IF_VAL
#undef HAMT_USE_FUEL
