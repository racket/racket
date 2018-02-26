/*
  Racket
  Copyright (c) 2004-2016 PLT Design Inc.
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

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif
#include "../gc2/my_qsort.c"
#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

static int compare_syms(const void *_a, const void *_b)
{
  Scheme_Object *a = *(Scheme_Object **)_a;
  Scheme_Object *b = *(Scheme_Object **)_b;
  intptr_t l = SCHEME_SYM_LEN(a), i;

  MZ_ASSERT(SCHEME_SYMBOLP(a));
  MZ_ASSERT(SCHEME_SYMBOLP(b));

  if (SCHEME_SYM_LEN(b) < l)
    l = SCHEME_SYM_LEN(b);
  
  for (i = 0; i < l; i++) {
    if (SCHEME_SYM_VAL(a)[i] != SCHEME_SYM_VAL(b)[i])
      return (SCHEME_SYM_VAL(a)[i] - SCHEME_SYM_VAL(b)[i]);
  }

  return SCHEME_SYM_LEN(a) - SCHEME_SYM_LEN(b);
}

static void sort_symbol_array(Scheme_Object **a, intptr_t count)
{
  my_qsort(a, count, sizeof(Scheme_Object *), compare_syms);
}

static int compare_nums(const void *_a, const void *_b)
/* also allow #fs */
{
  Scheme_Object *a = *(Scheme_Object **)_a;
  Scheme_Object *b = *(Scheme_Object **)_b;

  if (SCHEME_FALSEP(a))
    return -1;
  else if (SCHEME_FALSEP(b))
    return 1;

  MZ_ASSERT(SCHEME_REALP(a));
  MZ_ASSERT(SCHEME_REALP(b));

  if (scheme_bin_lt(a, b))
    return -1;
  else if (scheme_bin_lt(b, a))
    return 1;
  else
    return 0;
}

static void sort_number_array(Scheme_Object **a, intptr_t count)
{
  my_qsort(a, count, sizeof(Scheme_Object *), compare_nums);
}

static int compare_vars_at_resolve(const void *_a, const void *_b)
{
  Scheme_IR_Local *a = *(Scheme_IR_Local **)_a;
  Scheme_IR_Local *b = *(Scheme_IR_Local **)_b;
  return a->resolve.lex_depth - b->resolve.lex_depth;
}

void scheme_sort_resolve_ir_local_array(Scheme_IR_Local **a, intptr_t count)
{
  my_qsort(a, count, sizeof(Scheme_IR_Local *), compare_vars_at_resolve);
}

/**************************************************************/

static int all_symbols(Scheme_Object **a, int c)
{
  while (c--) {
    if (!SCHEME_SYMBOLP(a[c]))
      return 0;
  }
  return 1;
}

static int all_reals(Scheme_Object **a, int c)
{
  while (c--) {
    if (!SCHEME_REALP(a[c]))
      return 0;
  }
  return 1;
}

Scheme_Object **scheme_extract_sorted_keys(Scheme_Object *tree)
{
  intptr_t j, i, count;
  Scheme_Object **a, *key;

  if (SCHEME_HASHTRP(tree)) {
    Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)tree;

    count = ht->count;
    if (!count)
      return NULL;
    
    a = MALLOC_N(Scheme_Object *, count);
    
    j = -1;
    i = 0;
    while ((j = scheme_hash_tree_next(ht, j)) != -1) {
      scheme_hash_tree_index(ht, j, &key, NULL);
      a[i++] = key;
    }

    MZ_ASSERT(i == count);
  } else {
    Scheme_Hash_Table *t = (Scheme_Hash_Table *)tree;

    count = t->count;
    
    if (!count)
      return NULL;

    a = MALLOC_N(Scheme_Object *, count);
    j = 0;
    
    for (i = t->size; i--; ) {
      if (t->vals[i]) {
        a[j++] = t->keys[i];
      }
    }

    MZ_ASSERT(j == count);
  }

  if (SCHEME_SYMBOLP(a[0]) && all_symbols(a, count))
    sort_symbol_array(a, count);
  else if (all_reals(a, count))
    sort_number_array(a, count);
  else
    return NULL;

  return a;
}
