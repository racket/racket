
/**********************************************************************/
/*                           constructors                             */
/**********************************************************************/

Scheme_Object *
X(scheme_make_sized_offset, _string)(Xchar *chars, intptr_t d, intptr_t len, int copy)
{
  Scheme_Object *str;

  if (!chars) chars = EMPTY;

  str = scheme_alloc_object();
  str->type = scheme_x_string_type;

  if (len < 0)
    len = xstrlen(chars XFORM_OK_PLUS d);
  if (copy) {
    Xchar *naya;

    if (len < 100)
      naya = (Xchar *)scheme_malloc_atomic((len + 1) * sizeof(Xchar));
    else
      naya = (Xchar *)scheme_malloc_fail_ok(scheme_malloc_atomic, (len + 1) * sizeof(Xchar));
    SCHEME_X_STR_VAL(str) = naya;
    memcpy(naya, chars + d, len * sizeof(Xchar));
    naya[len] = 0;
  } else
    SCHEME_X_STR_VAL(str) = chars + d;
  SCHEME_X_STRTAG_VAL(str) = len;

  return str;
}

Scheme_Object *
X(scheme_make_sized, _string)(Xchar *chars, intptr_t len, int copy)
{
  return X(scheme_make_sized_offset, _string)(chars, 0, len, copy);
}

Scheme_Object *
X(scheme_make_immutable_sized, _string)(Xchar *chars, intptr_t len, int copy)
{
  Scheme_Object *s;
  
  s = X(scheme_make_sized_offset, _string)(chars, 0, len, copy);
  SCHEME_SET_X_STRING_IMMUTABLE(s);

  return s;
}

Scheme_Object *
X(scheme_make, _string_without_copying)(Xchar *chars)
{
  return X(scheme_make_sized_offset, _string)(chars, 0, -1, 0);
}

Scheme_Object *
X(scheme_make, _string)(const Xchar *chars)
{
  return X(scheme_make_sized_offset, _string)((Xchar *)chars, 0, -1, 1);
}

Scheme_Object *
X(scheme_alloc, _string)(intptr_t size, Xchar fill)
{
  Scheme_Object *str;
  Xchar *s;
  intptr_t i;
  
  if (size < 0) {
    str = scheme_make_integer(size);
    scheme_wrong_contract("make-" XSTRINGSTR, "exact-nonnegative-integer?",
                          -1, 0, &str);
  }

  str = scheme_alloc_object();
  str->type = scheme_x_string_type;
  if (size < 100)
    s = (Xchar *)scheme_malloc_atomic(sizeof(Xchar)*(size + 1));
  else
    s = (Xchar *)scheme_malloc_fail_ok(scheme_malloc_atomic, sizeof(Xchar)*(size + 1));
  for (i = size; i--; ) {
    s[i] = fill;
  }
  s[size] = 0;
  SCHEME_X_STR_VAL(str) = s;
  SCHEME_X_STRTAG_VAL(str) = size;

  return str;
}

#if defined(GENERATING_BYTE)
Scheme_Object *
X(scheme_alloc_shared, _string)(intptr_t size, Xchar fill)
{
  Scheme_Object *str;
  Xchar *s;
  intptr_t i;
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  void *original_gc;
#endif
  
  if (size < 0) {
    str = scheme_make_integer(size);
    scheme_wrong_contract("make-" XSTRINGSTR, "exact-nonnegative-integer?",
                          -1, 0, &str);
  }

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  original_gc = GC_switch_to_master_gc();
#endif
  str = scheme_alloc_object();
  str->type = scheme_x_string_type;
  SHARED_ALLOCATED_SET(str);

  if (size < 100)
    s = (Xchar *)scheme_malloc_atomic(sizeof(Xchar)*(size + 1));
  else
    s = (Xchar *)scheme_malloc_fail_ok(scheme_malloc_atomic, sizeof(Xchar)*(size + 1));
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  GC_switch_back_from_master(original_gc);
#endif

  for (i = size; i--; ) {
    s[i] = fill;
  }
  s[size] = 0;
  SCHEME_X_STR_VAL(str) = s;
  SCHEME_X_STRTAG_VAL(str) = size;

  return str;
}
#endif

/**********************************************************************/
/*                          string procs                              */
/**********************************************************************/

static Scheme_Object *
X__(string_p) (int argc, Scheme_Object *argv[])
{
  return (SCHEME_X_STRINGP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
X_(make, string) (int argc, Scheme_Object *argv[])
{
  intptr_t len;
  Xchar fill;
  Scheme_Object *str;

  len = scheme_extract_index("make-" XSTRINGSTR, 0, argc, argv, -1, 0);

  if (argc == 2) {
    if (!CHARP(argv[1]))
      scheme_wrong_contract("make-" XSTRINGSTR, CHAR_STR, 1, argc, argv);
    fill = (Xchar)CHAR_VAL(argv[1]);
  } else
    fill = 0;

  if (len == -1) {
    scheme_raise_out_of_memory("make-" XSTRINGSTR, "making " XSTR "string of length %s",
			       scheme_make_provided_string(argv[0], 0, NULL));
  }

  str = X(scheme_alloc, _string)(len, fill);
  return str;
}

static Scheme_Object *
X__(string) (int argc, Scheme_Object *argv[])
{
  Scheme_Object *str;
  int i;

  str = X(scheme_alloc, _string)(argc, 0);

  for ( i=0 ; i<argc ; ++i ) {
    if (!CHARP(argv[i]))
      scheme_wrong_contract(XSTRINGSTR, CHAR_STR, i, argc, argv);
    SCHEME_X_STR_VAL(str)[i] = (Xchar)CHAR_VAL(argv[i]);
  }

  return str;
}

#if defined(GENERATING_BYTE)
static Scheme_Object *
X_(make_shared, string) (int argc, Scheme_Object *argv[])
{
  intptr_t len;
  Xchar fill;
  Scheme_Object *str;

  len = scheme_extract_index("make-" XSTRINGSTR, 0, argc, argv, -1, 0);

  if (argc == 2) {
    if (!CHARP(argv[1]))
      scheme_wrong_contract("make-" XSTRINGSTR, CHAR_STR, 1, argc, argv);
    fill = (Xchar)CHAR_VAL(argv[1]);
  } else
    fill = 0;

  if (len == -1) {
    scheme_raise_out_of_memory("make-" XSTRINGSTR, "making " XSTR "string of length %s",
			       scheme_make_provided_string(argv[0], 0, NULL));
  }

  str = X(scheme_alloc_shared, _string)(len, fill);
  return str;
}

static Scheme_Object *
X_(shared, string) (int argc, Scheme_Object *argv[])
{
  Scheme_Object *str;
  int i;

  str = X(scheme_alloc_shared, _string)(argc, 0);

  for ( i=0 ; i<argc ; ++i ) {
    if (!CHARP(argv[i]))
      scheme_wrong_contract(XSTRINGSTR, CHAR_STR, i, argc, argv);
    SCHEME_X_STR_VAL(str)[i] = (Xchar)CHAR_VAL(argv[i]);
  }

  return str;
}
#endif

static Scheme_Object *
X__(string_length) (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_X_STRINGP(argv[0]))
    scheme_wrong_contract(XSTRINGSTR "-length", IS_STR, 0, argc, argv);

  return scheme_make_integer(SCHEME_X_STRTAG_VAL(argv[0]));
}

Scheme_Object *
X_(scheme, string_length) (Scheme_Object *v)
{
  return X__(string_length)(1, &v);
}

Scheme_Object *
X_(scheme_checked, string_ref) (int argc, Scheme_Object *argv[])
{
  intptr_t i, len;
  int c;
  Xchar *str;

  if (!SCHEME_X_STRINGP(argv[0]))
    scheme_wrong_contract(XSTRINGSTR "-ref", IS_STR, 0, argc, argv);

  str = SCHEME_X_STR_VAL(argv[0]);
  len = SCHEME_X_STRTAG_VAL(argv[0]);

  i = scheme_extract_index(XSTRINGSTR "-ref", 1, argc, argv, len, 0);

  if (i >= len) {
    scheme_out_of_range(XSTRINGSTR "-ref", XSTR "string", "", argv[1], argv[0], -1, len);
    return NULL;
  }

  c = ((uXchar *)str)[i];
  return MAKE_CHAR(c);
}

Scheme_Object *
X_(scheme_checked, string_set) (int argc, Scheme_Object *argv[])
{
  intptr_t i, len;
  Xchar *str;
  
  if (!SCHEME_MUTABLE_X_STRINGP(argv[0]))
    scheme_wrong_contract(XSTRINGSTR "-set!", "(and/c " IS_STR " (not/c immutable?))", 0, argc, argv);

  str = SCHEME_X_STR_VAL(argv[0]);
  len = SCHEME_X_STRTAG_VAL(argv[0]);

  i = scheme_extract_index(XSTRINGSTR "-set!", 1, argc, argv, len, 0);

  if (!CHARP(argv[2]))
    scheme_wrong_contract(XSTRINGSTR "-set!", CHAR_STR, 2, argc, argv);

  if (i >= len) {
    scheme_out_of_range(XSTRINGSTR "-set!", XSTR "string", "", argv[1], argv[0], 0, len - 1);
    return NULL;
  }

  str[i] = (Xchar)CHAR_VAL(argv[2]);

  return scheme_void;
}

static Scheme_Object *
X__(substring) (int argc, Scheme_Object *argv[])
{
  intptr_t start, finish;
  Xchar *chars;
  Scheme_Object *str;

  if (!SCHEME_X_STRINGP(argv[0]))
    scheme_wrong_contract(SUBXSTR, IS_STR, 0, argc, argv);

  chars = SCHEME_X_STR_VAL(argv[0]);

  scheme_do_get_substring_indices(SUBXSTR, argv[0], argc, argv, 1, 2,
                                  &start, &finish, SCHEME_X_STRTAG_VAL(argv[0]));

  str = X(scheme_alloc, _string)(finish-start, 0);
  memcpy(SCHEME_X_STR_VAL(str), chars + start, (finish - start) * sizeof(Xchar));
  
  return str;
}

static Scheme_Object *
X__(string_append) (int argc, Scheme_Object *argv[])
{
  Scheme_Object *naya, *s;
  Xchar *chars;
  int i;
  intptr_t len;

  len = 0;
  for (i = 0; i < argc; i++) {
    s = argv[i];
    if (!SCHEME_X_STRINGP(s))
      scheme_wrong_contract(XSTRINGSTR "-append", IS_STR, i, argc, argv);
    len += SCHEME_X_STRTAG_VAL(s);
  }

  if (!len)
    return X(zero_length, _string);

  naya = X(scheme_alloc, _string)(len, 0);
  chars = SCHEME_X_STR_VAL(naya);

  for (i = 0; i < argc; i++) {
    s = argv[i];
    len = SCHEME_X_STRTAG_VAL(s);
    memcpy(chars, SCHEME_X_STR_VAL(s), len * sizeof(Xchar));
    chars = chars XFORM_OK_PLUS len;
  }
  
  return naya;
}

Scheme_Object *
X(scheme_append, _string)(Scheme_Object *str1, Scheme_Object *str2)
{
  intptr_t len1, len2;
  Xchar *r;
  Scheme_Object *naya;

  len1 = SCHEME_X_STRTAG_VAL(str1);
  len2 = SCHEME_X_STRTAG_VAL(str2);

  naya = X(scheme_alloc, _string)(len1 + len2, 0);

  r = SCHEME_X_STR_VAL(naya);
  memcpy(r, SCHEME_X_STR_VAL(str1), len1 * sizeof(Xchar));
  memcpy(r + len1, SCHEME_X_STR_VAL(str2), len2 * sizeof(Xchar));

  r[len1 + len2] = 0;

  return naya;
}

static Scheme_Object *
X__(string_to_list) (int argc, Scheme_Object *argv[])
{
  int len, i;
  uXchar *chars;
  Scheme_Object *pair = scheme_null, *v;

  if (!SCHEME_X_STRINGP(argv[0]))
    scheme_wrong_contract(XSTRINGSTR "->list", IS_STR, 0, argc, argv);

  chars = (uXchar *)SCHEME_X_STR_VAL(argv[0]);
  len = SCHEME_X_STRTAG_VAL(argv[0]);

  if (len < 0xFFF) {
    for (i = len ; i--; ) {
      v = MAKE_CHAR(chars[i]);
      pair = scheme_make_pair(v, pair);
    }
  } else {
    for (i = len ; i--; ) {
      if (!(i & 0xFFF))
	SCHEME_USE_FUEL(0xFFF);
      v = MAKE_CHAR(chars[i]);
      pair = scheme_make_pair(v, pair);
    }
  }

  return pair;
}

static Scheme_Object *
X_(list_to, string) (int argc, Scheme_Object *argv[])
{
  int len, i;
  Scheme_Object *list, *str, *ch;

  list = argv[0];
  len = scheme_list_length(list);
  str = X(scheme_alloc, _string)(len, 0);
  i = 0;
  while (SCHEME_PAIRP (list)) {
    ch = SCHEME_CAR(list);
    
    if (!CHARP(ch))
      scheme_wrong_contract("list->" XSTRINGSTR, "(listof " CHAR_STR ")", 0, 
                            argc, argv);
    
    SCHEME_X_STR_VAL(str)[i] = (Xchar)CHAR_VAL(ch);
    i++;
    list = SCHEME_CDR(list);
  }  
  
  if (!SCHEME_NULLP(list))
    scheme_wrong_contract("list->" XSTRINGSTR, "(listof " CHAR_STR ")", 0, argc, argv);

  return str;
}

static Scheme_Object *
X__(string_copy) (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_X_STRINGP(argv[0]))
    scheme_wrong_contract(XSTRINGSTR "-copy", IS_STR, 0, argc, argv);

  return X(scheme_make_sized, _string)(SCHEME_X_STR_VAL(argv[0]), 
                                       SCHEME_X_STRTAG_VAL(argv[0]), 1);
}

static Scheme_Object *
X__(string_copy_bang)(int argc, Scheme_Object *argv[])
{
  Scheme_Object *s1, *s2;
  intptr_t istart, ifinish;
  intptr_t ostart, ofinish;

  s1 = argv[0];
  if (!SCHEME_MUTABLE_X_STRINGP(s1))
    scheme_wrong_contract(XSTRINGSTR "-copy!", "(and/c " IS_STR " (not/c immutable?))", 0, argc, argv);

  scheme_do_get_substring_indices(XSTRINGSTR "-copy!", s1, 
                                  argc, argv, 1, 5, 
                                  &ostart, &ofinish, SCHEME_X_STRTAG_VAL(s1));

  s2 = argv[2];
  if (!SCHEME_X_STRINGP(s2))
    scheme_wrong_contract(XSTRINGSTR "-copy!", IS_STR, 2, argc, argv);

  scheme_do_get_substring_indices(XSTRINGSTR "-copy!", s2, 
                                  argc, argv, 3, 4, 
                                  &istart, &ifinish, SCHEME_X_STRTAG_VAL(s2));
  
  if ((ofinish - ostart) < (ifinish - istart)) {
    scheme_arg_mismatch(XSTRINGSTR "-copy!",
			"not enough room in target " XSTR "string: ",
			argv[2]);
    return NULL;
  }

  memmove(SCHEME_X_STR_VAL(s1) + ostart,
	  SCHEME_X_STR_VAL(s2) + istart,
	  (ifinish - istart) * sizeof(Xchar));
  
  return scheme_void;
}

static Scheme_Object *
X__(string_fill) (int argc, Scheme_Object *argv[])
{
  int len, i;
  Xchar *chars, ch;

  if (!SCHEME_MUTABLE_X_STRINGP(argv[0]))
    scheme_wrong_contract(XSTRINGSTR "-fill!", "(and/c " IS_STR " (not/c immutable?))", 0, argc, argv);
  if (!CHARP(argv[1]))
    scheme_wrong_contract(XSTRINGSTR "-fill!", CHAR_STR, 1, argc, argv);
  
  chars = SCHEME_X_STR_VAL(argv[0]);
  ch = (Xchar)CHAR_VAL(argv[1]);
  len = SCHEME_X_STRTAG_VAL(argv[0]);
  for (i = 0; i < len; i++) {
    chars[i] = ch;
  }

  return scheme_void;
}

static Scheme_Object *
X__(string_to_immutable) (int argc, Scheme_Object *argv[])
{
  Scheme_Object *s = argv[0];

  if (!SCHEME_X_STRINGP(s))
    scheme_wrong_contract(XSTRINGSTR "->immutable-" XSTRINGSTR, IS_STR, 0, argc, argv);

  if (SCHEME_MUTABLE_X_STRINGP(s)) {
    Scheme_Object *s2;
    s2 = X(scheme_make_sized, _string)(SCHEME_X_STR_VAL(s), SCHEME_X_STRTAG_VAL(s), 1);
    SCHEME_SET_X_STRING_IMMUTABLE(s2);
    return s2;
  } else
    return s;
}

static Scheme_Object *
X_(append_all, strings_backwards)(Scheme_Object *l)
{
  int i, len;
  Scheme_Object **a;

  len = scheme_list_length(l);
  a = MALLOC_N(Scheme_Object *, len);
  for (i = len; i--; l = SCHEME_CDR(l)) {
    a[i] = SCHEME_CAR(l);
  }
  
  return X__(string_append)(len, a);
}

#undef SCHEME_X_STR_VAL
#undef SCHEME_X_STRTAG_VAL
#undef SCHEME_X_STRINGP
#undef SCHEME_MUTABLE_X_STRINGP
#undef SCHEME_SET_X_STRING_IMMUTABLE
#undef scheme_x_string_type
#undef X
#undef X_
#undef X__
#undef EMPTY
#undef Xchar
#undef uXchar
#undef XSTR
#undef IS_STR
#undef XSTRINGSTR
#undef SUBXSTR
#undef CHARP
#undef CHAR_VAL
#undef CHAR_STR
#undef MAKE_CHAR
#undef xstrlen
