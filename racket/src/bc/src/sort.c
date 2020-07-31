#include "schpriv.h"

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif
#include "../gc2/my_qsort.c"
#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

enum {
      sort_major_unknown,
      sort_major_boolean,
      sort_major_char,
      sort_major_real,
      sort_major_symbol,
      sort_major_keyword,
      sort_major_string,
      sort_major_bytes,
      sort_major_null,
      sort_major_void,
      sort_major_eof,
};

static int sort_major(Scheme_Object *v)
{
  if (SAME_OBJ(v, scheme_true) || SCHEME_FALSEP(v))
    return sort_major_boolean;
  else if (SCHEME_CHARP(v))
    return sort_major_char;
  else if (SCHEME_REALP(v))
    return sort_major_real;
  else if (SCHEME_SYMBOLP(v))
    return sort_major_symbol;
  else if (SCHEME_KEYWORDP(v))
    return sort_major_keyword;
  else if (SCHEME_CHAR_STRINGP(v))
    return sort_major_string;
  else if (SCHEME_BYTE_STRINGP(v))
    return sort_major_bytes;
  else if (SCHEME_NULLP(v))
    return sort_major_null;
  else if (SCHEME_VOIDP(v))
    return sort_major_void;
  else if (SCHEME_EOFP(v))
    return sort_major_eof;
  else
    return sort_major_unknown;
}

static int compare_sym_likes(Scheme_Object *a, Scheme_Object *b)
{
  intptr_t l = SCHEME_SYM_LEN(a), i;

  if (SCHEME_SYM_LEN(b) < l)
    l = SCHEME_SYM_LEN(b);
  
  for (i = 0; i < l; i++) {
    if (SCHEME_SYM_VAL(a)[i] != SCHEME_SYM_VAL(b)[i])
      return (((unsigned char *)SCHEME_SYM_VAL(a))[i] - ((unsigned char *)SCHEME_SYM_VAL(b))[i]);
  }

  return SCHEME_SYM_LEN(a) - SCHEME_SYM_LEN(b);
}

static int compare_syms(Scheme_Object *a, Scheme_Object *b)
{
  MZ_ASSERT(SCHEME_SYMBOLP(a));
  MZ_ASSERT(SCHEME_SYMBOLP(b));

  /* Sort uninterned before unreadable before interned.
     There's no guarantee that uninterned symbols are
     usefully sorted, but try anyway. */
  if (SCHEME_SYM_UNINTERNEDP(a)) {
    if (!SCHEME_SYM_UNINTERNEDP(b))
      return -1;
  } else {
    if (SCHEME_SYM_UNINTERNEDP(b))
      return 1;
    if (SCHEME_SYM_PARALLELP(a)) {
      if (!SCHEME_SYM_PARALLELP(b))
        return -1;
    } else {
      if (SCHEME_SYM_PARALLELP(b))
        return 1;
    }
  }

  return compare_sym_likes(a, b);
}

static int compare_keywords(Scheme_Object *a, Scheme_Object *b)
{
  MZ_ASSERT(SCHEME_KEYWORDP(a));
  MZ_ASSERT(SCHEME_KEYWORDP(b));

  return compare_sym_likes(a, b);
}

static int compare_reals(Scheme_Object *a, Scheme_Object *b)
{
  MZ_ASSERT(SCHEME_REALP(a));
  MZ_ASSERT(SCHEME_REALP(b));

  if (scheme_bin_lt(a, b))
    return -1;
  else if (scheme_bin_lt(b, a))
    return 1;
  else
    return 0;
}

int compare_sortable(const void *_a, const void *_b)
{
  Scheme_Object *a = *(Scheme_Object **)_a;
  Scheme_Object *b = *(Scheme_Object **)_b;
  int am, bm;

  am = sort_major(a);
  bm = sort_major(b);

  if (am != bm)
    return am - bm;
  else {
    switch (am) {
    case sort_major_boolean:
      if (SAME_OBJ(a, b))
        return 0;
      else if (SCHEME_FALSEP(a))
        return -1;
      else
        return 1;
    case sort_major_char:
      return SCHEME_CHAR_VAL(a) - SCHEME_CHAR_VAL(b);
    case sort_major_real:
      return compare_reals(a, b);
    case sort_major_symbol:
      return compare_syms(a, b);
    case sort_major_keyword:
      return compare_keywords(a, b);
    case sort_major_string:
      return scheme_string_compare(a, b);
    case sort_major_bytes:
      return scheme_bytes_compare(a, b);
    case sort_major_null:
    case sort_major_void:
    case sort_major_eof:
      /* There can be only one. */
      return 0;
    }
  }

  return 0;
}

/**************************************************************/

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

static int all_sortable(Scheme_Object **a, int c)
{
  while (c--) {
    if (sort_major(a[c]) == sort_major_unknown)
      return 0;
  }
  return 1;
}

static void sort_sortable_array(Scheme_Object **a, intptr_t count)
{
  my_qsort(a, count, sizeof(Scheme_Object *), compare_sortable);
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

  if (all_sortable(a, count)) {
    sort_sortable_array(a, count);
    return a;
  } else
    return NULL;

  return a;
}
