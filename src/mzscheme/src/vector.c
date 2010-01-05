/*
  MzScheme
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

/* globals */
READ_ONLY Scheme_Object *scheme_vector_proc;
READ_ONLY Scheme_Object *scheme_vector_immutable_proc;

/* locals */
static Scheme_Object *vector_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *make_vector (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_immutable (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_to_list (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_to_vector (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_fill (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_copy_bang(int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_to_immutable (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_to_values (int argc, Scheme_Object *argv[]);

static Scheme_Object *unsafe_vector_len (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_vector_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_vector_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_struct_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_struct_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_string_len (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_string_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_string_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_bytes_len (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_bytes_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_bytes_set (int argc, Scheme_Object *argv[]);

void
scheme_init_vector (Scheme_Env *env)
{
  Scheme_Object *p;

  p = scheme_make_folding_prim(vector_p, "vector?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("vector?", p, env);

  scheme_add_global_constant("make-vector", 
			     scheme_make_immed_prim(make_vector, 
						    "make-vector", 
						    1, 2), 
			     env);
  
  REGISTER_SO(scheme_vector_proc);
  p = scheme_make_immed_prim(vector, "vector", 0, -1);
  scheme_vector_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_UNARY_INLINED
                                | SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_NARY_INLINED);
  scheme_add_global_constant("vector", p, env);

  REGISTER_SO(scheme_vector_immutable_proc);
  p = scheme_make_immed_prim(vector_immutable, "vector-immutable", 0, -1);
  scheme_vector_immutable_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_UNARY_INLINED
                                | SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_NARY_INLINED);
  scheme_add_global_constant("vector-immutable", p, env);
  
  p = scheme_make_folding_prim(vector_length, "vector-length", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("vector-length", p, env);

  p = scheme_make_immed_prim(scheme_checked_vector_ref, 
			     "vector-ref", 
			     2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("vector-ref", p, env);

  p = scheme_make_immed_prim(scheme_checked_vector_set,
			     "vector-set!", 
			     3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_NARY_INLINED;
  scheme_add_global_constant("vector-set!", p, env);

  scheme_add_global_constant("vector->list", 
			     scheme_make_immed_prim(vector_to_list, 
						    "vector->list", 
						    1, 1), 
			     env);
  scheme_add_global_constant("list->vector", 
			     scheme_make_immed_prim(list_to_vector, 
						    "list->vector", 
						    1, 1), 
			     env);
  scheme_add_global_constant("vector-fill!", 
			     scheme_make_immed_prim(vector_fill, 
						    "vector-fill!", 
						    2, 2), 
			     env);
  scheme_add_global_constant("vector-copy!", 
			     scheme_make_immed_prim(vector_copy_bang, 
						    "vector-copy!", 
						    3, 5), 
			     env);
  scheme_add_global_constant("vector->immutable-vector", 
			     scheme_make_immed_prim(vector_to_immutable, 
						    "vector->immutable-vector", 
						    1, 1), 
			     env);
  scheme_add_global_constant("vector->values", 
			     scheme_make_prim_w_arity2(vector_to_values, 
                                                       "vector->values", 
                                                       1, 3,
                                                       0, -1), 
			     env);
}

void
scheme_init_unsafe_vector (Scheme_Env *env)
{
  Scheme_Object *p;

  p = scheme_make_immed_prim(unsafe_vector_len, 
			     "unsafe-vector-length", 
			     1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_UNARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-vector-length", p, env);

  p = scheme_make_immed_prim(unsafe_vector_ref, 
			     "unsafe-vector-ref", 
			     2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-vector-ref", p, env);

  p = scheme_make_immed_prim(unsafe_vector_set,
			     "unsafe-vector-set!", 
			     3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_NARY_INLINED;
  scheme_add_global_constant("unsafe-vector-set!", p, env);  
  p = scheme_make_immed_prim(unsafe_vector_ref, 
			     "unsafe-vector-ref", 
			     2, 2);

  p = scheme_make_immed_prim(unsafe_struct_ref, 
			     "unsafe-struct-ref", 
			     2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-struct-ref", p, env);

  p = scheme_make_immed_prim(unsafe_struct_set,
			     "unsafe-struct-set!", 
			     3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_NARY_INLINED;
  scheme_add_global_constant("unsafe-struct-set!", p, env);  


  p = scheme_make_immed_prim(unsafe_string_len, 
			     "unsafe-string-length", 
			     1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_UNARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-string-length", p, env);

  p = scheme_make_immed_prim(unsafe_string_ref, 
			     "unsafe-string-ref", 
			     2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-string-ref", p, env);

  p = scheme_make_immed_prim(unsafe_string_set,
			     "unsafe-string-set!", 
			     3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_NARY_INLINED;
  scheme_add_global_constant("unsafe-string-set!", p, env);  

  p = scheme_make_immed_prim(unsafe_bytes_len, 
			     "unsafe-bytes-length", 
			     1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_UNARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-bytes-length", p, env);

  p = scheme_make_immed_prim(unsafe_bytes_ref, 
			     "unsafe-bytes-ref", 
			     2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-bytes-ref", p, env);

  p = scheme_make_immed_prim(unsafe_bytes_set,
			     "unsafe-bytes-set!", 
			     3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_NARY_INLINED;
  scheme_add_global_constant("unsafe-bytes-set!", p, env);  
  p = scheme_make_immed_prim(unsafe_bytes_ref, 
			     "unsafe-bytes-ref", 
			     2, 2);
}

#define VECTOR_BYTES(size) (sizeof(Scheme_Vector) + ((size) - 1) * sizeof(Scheme_Object *))

Scheme_Object *
scheme_make_vector (long size, Scheme_Object *fill)
{
  Scheme_Object *vec;
  long i;

  if (size < 0) {
    vec = scheme_make_integer(size);
    scheme_wrong_type("make-vector", "non-negative exact integer", -1, 0, &vec);
  }

  if (size < 1024) {
    vec = (Scheme_Object *)scheme_malloc_tagged(VECTOR_BYTES(size));
  } else {
    vec = (Scheme_Object *)scheme_malloc_fail_ok(scheme_malloc_tagged, VECTOR_BYTES(size));
  }

  vec->type = scheme_vector_type;
  SCHEME_VEC_SIZE(vec) = size;

  if (fill) {
    for (i = 0; i < size; i++) {
      SCHEME_VEC_ELS(vec)[i] = fill;
    }
  }

  return vec;
}

/* locals */

static Scheme_Object *
vector_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_VECTORP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
make_vector (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec, *fill;
  long len;

  len = scheme_extract_index("make-vector", 0, argc, argv, -1, 0);

  if ((len == -1) 
      /* also watch for overflow: */
      || ((long)VECTOR_BYTES(len) < len)) {
    scheme_raise_out_of_memory("make-vector", "making vector of length %s",
			       scheme_make_provided_string(argv[0], 1, NULL));
  }

  if (argc == 2)
    fill = argv[1];
  else
    fill = scheme_make_integer(0);

  vec = scheme_make_vector(len, fill);

  return vec;
}

static Scheme_Object *
vector (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec;
  int i;

  vec = scheme_make_vector (argc, 0);
  for (i = 0; i < argc ; i++) {
    SCHEME_VEC_ELS(vec)[i] = argv[i];
  }

  return vec;
}

static Scheme_Object *
vector_immutable (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec;

  vec = vector(argc, argv);
  SCHEME_SET_IMMUTABLE(vec);

  return vec;
}

static Scheme_Object *
vector_length (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_VECTORP(argv[0]))
    scheme_wrong_type("vector-length", "vector", 0, argc, argv);

  return scheme_make_integer(SCHEME_VEC_SIZE(argv[0]));
}

Scheme_Object *scheme_vector_length(Scheme_Object *v)
{
  Scheme_Object *a[1];
  a[0] = v;
  return vector_length(1, a);
}

void scheme_bad_vec_index(char *name, Scheme_Object *i, const char *what, Scheme_Object *vec, 
                          long bottom, long len)
{
  if (len) {
    long n = len - 1;
    char *vstr;
    int vlen;
    vstr = scheme_make_provided_string(vec, 2, &vlen);
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: index %s out of range [%ld, %ld] for %s: %t",
		     name, 
		     scheme_make_provided_string(i, 2, NULL), 
		     bottom, n,
                     what,
		     vstr, vlen);
  } else
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: bad index %s for empty %s",
		     name,
		     scheme_make_provided_string(i, 0, NULL),
                     what);
}

static Scheme_Object *
bad_index(char *name, Scheme_Object *i, Scheme_Object *vec, int bottom)
{
  scheme_bad_vec_index(name, i, "vector", vec, bottom, SCHEME_VEC_SIZE(vec));
  return NULL;
}

Scheme_Object *
scheme_checked_vector_ref (int argc, Scheme_Object *argv[])
{
  long i, len;

  if (!SCHEME_VECTORP(argv[0]))
    scheme_wrong_type("vector-ref", "vector", 0, argc, argv);

  len = SCHEME_VEC_SIZE(argv[0]);

  i = scheme_extract_index("vector-ref", 1, argc, argv, len, 0);

  if (i >= len)
    return bad_index("vector-ref", argv[1], argv[0], 0);

  return (SCHEME_VEC_ELS(argv[0]))[i];
}

Scheme_Object *
scheme_checked_vector_set(int argc, Scheme_Object *argv[])
{
  long i, len;

  if (!SCHEME_MUTABLE_VECTORP(argv[0]))
    scheme_wrong_type("vector-set!", "mutable vector", 0, argc, argv);

  len = SCHEME_VEC_SIZE(argv[0]);

  i = scheme_extract_index("vector-set!", 1, argc, argv, len, 0);

  if (i >= len)
    return bad_index("vector-set!", argv[1], argv[0], 0);

  (SCHEME_VEC_ELS(argv[0]))[i] = argv[2];

  return scheme_void;
}

static Scheme_Object *
vector_to_list (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_VECTORP(argv[0]))
    scheme_wrong_type("vector->list", "vector", 0, argc, argv);

  return scheme_vector_to_list(argv[0]);
}

# define cons(car, cdr) scheme_make_pair(car, cdr)

Scheme_Object *
scheme_vector_to_list (Scheme_Object *vec)
{
  int i;
  Scheme_Object *pair = scheme_null;

  i = SCHEME_VEC_SIZE(vec);

  if (i < 0xFFF) {
    for (; i--; ) {
      pair = cons(SCHEME_VEC_ELS(vec)[i], pair);
    }
  } else {
    for (; i--; ) {
      if (!(i & 0xFFF))
	SCHEME_USE_FUEL(0xFFF);
      pair = cons(SCHEME_VEC_ELS(vec)[i], pair);
    }
  }

  return pair;
}

static Scheme_Object *
list_to_vector (int argc, Scheme_Object *argv[])
{
  return scheme_list_to_vector(argv[0]);
}

Scheme_Object *
scheme_list_to_vector (Scheme_Object *list)
{
  long len, i;
  Scheme_Object *vec, *orig = list;

  len = scheme_proper_list_length(list);
  if (len < 0)
    scheme_wrong_type("list->vector", "proper list", -1, 0, &orig);

  vec = scheme_make_vector(len, NULL);
  for (i = 0; i < len; i++) {
    SCHEME_VEC_ELS(vec)[i] = SCHEME_CAR(list);
    list = SCHEME_CDR(list);
  }

  return vec;
}

static Scheme_Object *
vector_fill (int argc, Scheme_Object *argv[])
{
  int i, sz;
  Scheme_Object *v;
  
  if (!SCHEME_MUTABLE_VECTORP(argv[0]))
    scheme_wrong_type("vector-fill!", "mutable vector", 0, argc, argv);

  v = argv[1];
  sz = SCHEME_VEC_SIZE(argv[0]);
  for (i = 0; i < sz; i++) {
    SCHEME_VEC_ELS(argv[0])[i] = v;
  }

  return argv[0];
}

static Scheme_Object *vector_copy_bang(int argc, Scheme_Object *argv[])
{
  Scheme_Object *s1, *s2;
  long istart, ifinish;
  long ostart, ofinish;

  s1 = argv[0];
  if (!SCHEME_MUTABLE_VECTORP(s1))
    scheme_wrong_type("vector-copy!", "mutable vector", 0, argc, argv);

  scheme_do_get_substring_indices("vector-copy!", s1, 
                                  argc, argv, 1, 5, 
                                  &ostart, &ofinish, SCHEME_VEC_SIZE(s1));

  s2 = argv[2];
  if (!SCHEME_VECTORP(s2))
    scheme_wrong_type("vector-copy!", "vector", 2, argc, argv);

  scheme_do_get_substring_indices("vector-copy!", s2, 
                                  argc, argv, 3, 4, 
                                  &istart, &ifinish, SCHEME_VEC_SIZE(s2));

  if ((ofinish - ostart) < (ifinish - istart)) {
    scheme_arg_mismatch("vector-copy!",
			"not enough room in target vector: ",
			argv[2]);
    return NULL;
  }

  memmove(SCHEME_VEC_ELS(s1) + ostart,
	  SCHEME_VEC_ELS(s2) + istart,
	  (ifinish - istart) * sizeof(Scheme_Object*));
  
  return scheme_void;
}

static Scheme_Object *vector_to_immutable (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec, *ovec;
  long len, i;

  if (!SCHEME_VECTORP(argv[0]))
    scheme_wrong_type("vector->immutable-vector", "vector", 0, argc, argv);

  if (SCHEME_IMMUTABLEP(argv[0]))
    return argv[0];

  ovec = argv[0];
  len = SCHEME_VEC_SIZE(ovec);

  vec = scheme_make_vector(len, NULL);
  for (i = 0; i < len; i++) {
    SCHEME_VEC_ELS(vec)[i] = SCHEME_VEC_ELS(ovec)[i];
  }
  SCHEME_SET_IMMUTABLE(vec);

  return vec;  
}

static Scheme_Object *vector_to_values (int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p;
  Scheme_Object *vec, **a;
  long len, start, finish, i;

  vec = argv[0];

  if (!SCHEME_VECTORP(vec))
    scheme_wrong_type("vector->values", "vector", 0, argc, argv);

  len = SCHEME_VEC_SIZE(vec);

  if (argc > 1)
    start = scheme_extract_index("vector->values", 1, argc, argv, len + 1, 0);
  else
    start = 0;
  if (argc > 2)
    finish = scheme_extract_index("vector->values", 2, argc, argv, len + 1, 0);
  else
    finish = len;

  if (!(start <= len)) {
    bad_index("vector->values", argv[1], vec, 0);
  }
  if (!(finish >= start && finish <= len)) {
    bad_index("vector->values", argv[2], vec, start);
  }

  len = finish - start;
  if (len == 1)
    return SCHEME_VEC_ELS(vec)[start];

  p = scheme_current_thread;
  if (p->values_buffer && (p->values_buffer_size >= len))
    a = p->values_buffer;
  else {
    a = MALLOC_N(Scheme_Object *, len);
    p->values_buffer = a;
    p->values_buffer_size = len;
  }

  p->ku.multiple.array = a;
  p->ku.multiple.count = len;

  for (i = 0; i < len; i++) {
    a[i] = SCHEME_VEC_ELS(vec)[start + i];
  }

  return SCHEME_MULTIPLE_VALUES;
}

/************************************************************/
/*                        unsafe                            */
/************************************************************/

static Scheme_Object *unsafe_vector_len (int argc, Scheme_Object *argv[])
{
  long n = SCHEME_VEC_SIZE(argv[0]);
  return scheme_make_integer(n);
}

static Scheme_Object *unsafe_vector_ref (int argc, Scheme_Object *argv[])
{
  return SCHEME_VEC_ELS(argv[0])[SCHEME_INT_VAL(argv[1])];
}

static Scheme_Object *unsafe_vector_set (int argc, Scheme_Object *argv[])
{
  SCHEME_VEC_ELS(argv[0])[SCHEME_INT_VAL(argv[1])] = argv[2];
  return scheme_void;
}

static Scheme_Object *unsafe_struct_ref (int argc, Scheme_Object *argv[])
{
  return ((Scheme_Structure *)argv[0])->slots[SCHEME_INT_VAL(argv[1])];
}

static Scheme_Object *unsafe_struct_set (int argc, Scheme_Object *argv[])
{
  ((Scheme_Structure *)argv[0])->slots[SCHEME_INT_VAL(argv[1])] = argv[2];
  return scheme_void;
}

static Scheme_Object *unsafe_string_len (int argc, Scheme_Object *argv[])
{
  long n = SCHEME_CHAR_STRLEN_VAL(argv[0]);
  return scheme_make_integer(n);
}

static Scheme_Object *unsafe_string_ref (int argc, Scheme_Object *argv[])
{
  mzchar v;
  v = SCHEME_CHAR_STR_VAL(argv[0])[SCHEME_INT_VAL(argv[1])];
  return scheme_make_ascii_character(v);
}

static Scheme_Object *unsafe_string_set (int argc, Scheme_Object *argv[])
{
  SCHEME_CHAR_STR_VAL(argv[0])[SCHEME_INT_VAL(argv[1])] = SCHEME_CHAR_VAL(argv[2]);
  return scheme_void;
}

static Scheme_Object *unsafe_bytes_len (int argc, Scheme_Object *argv[])
{
  long n = SCHEME_BYTE_STRLEN_VAL(argv[0]);
  return scheme_make_integer(n);
}

static Scheme_Object *unsafe_bytes_ref (int argc, Scheme_Object *argv[])
{
  long v;
  v = (unsigned char)SCHEME_BYTE_STR_VAL(argv[0])[SCHEME_INT_VAL(argv[1])];
  return scheme_make_integer(v);
}

static Scheme_Object *unsafe_bytes_set (int argc, Scheme_Object *argv[])
{
  SCHEME_BYTE_STR_VAL(argv[0])[SCHEME_INT_VAL(argv[1])] = (char)SCHEME_INT_VAL(argv[2]);
  return scheme_void;
}

