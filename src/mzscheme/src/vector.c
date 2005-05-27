/*
  MzScheme
  Copyright (c) 2004-2005 PLT Scheme, Inc.
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
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"

/* locals */
static Scheme_Object *vector_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *make_vector (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_immutable (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_to_list (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_to_vector (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_fill (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_to_immutable (int argc, Scheme_Object *argv[]);

static Scheme_Object *zero_length_vector;

void
scheme_init_vector (Scheme_Env *env)
{
  REGISTER_SO(zero_length_vector);
  zero_length_vector = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Vector)
							     - sizeof(Scheme_Object *));
  zero_length_vector->type = scheme_vector_type;
  SCHEME_VEC_SIZE(zero_length_vector) = 0;

  scheme_add_global_constant("vector?", 
			     scheme_make_folding_prim(vector_p, 
						      "vector?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("make-vector", 
			     scheme_make_prim_w_arity(make_vector, 
						      "make-vector", 
						      1, 2), 
			     env);
  scheme_add_global_constant("vector", 
			     scheme_make_prim_w_arity(vector, 
						      "vector", 
						      0, -1), 
			     env);
  scheme_add_global_constant("vector-immutable", 
			     scheme_make_prim_w_arity(vector_immutable, 
						      "vector-immutable", 
						      0, -1), 
			     env);
  scheme_add_global_constant("vector-length", 
			     scheme_make_folding_prim(vector_length, 
						      "vector-length", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("vector-ref", 
			     scheme_make_prim_w_arity(vector_ref, 
						      "vector-ref", 
						      2, 2), 
			     env);
  scheme_add_global_constant("vector-set!", 
			     scheme_make_prim_w_arity(vector_set, 
						      "vector-set!", 
						      3, 3), 
			     env);
  scheme_add_global_constant("vector->list", 
			     scheme_make_prim_w_arity(vector_to_list, 
						      "vector->list", 
						      1, 1), 
			     env);
  scheme_add_global_constant("list->vector", 
			     scheme_make_prim_w_arity(list_to_vector, 
						      "list->vector", 
						      1, 1), 
			     env);
  scheme_add_global_constant("vector-fill!", 
			     scheme_make_prim_w_arity(vector_fill, 
						      "vector-fill!", 
						      2, 2), 
			     env);
  scheme_add_global_constant("vector->immutable-vector", 
			     scheme_make_prim_w_arity(vector_to_immutable, 
						      "vector->immutable-vector", 
						      1, 1), 
			     env);
}

Scheme_Object *
scheme_make_vector (int size, Scheme_Object *fill)
{
  Scheme_Object *vec;
  int i;

  if (size <= 0) {
    if (size) {
      vec = scheme_make_integer(size);
      scheme_wrong_type("make-vector", "non-negative exact integer", -1, 0, &vec);
    } else
      return zero_length_vector;
  }

  if (size < 1024) {
    vec = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Vector) 
						+ (size - 1) * sizeof(Scheme_Object *));
  } else {
    vec = (Scheme_Object *)scheme_malloc_fail_ok(scheme_malloc_tagged,
						 sizeof(Scheme_Vector) 
						 + (size - 1) * sizeof(Scheme_Object *));
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

  if (len == -1) {
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

static Scheme_Object *
bad_index(char *name, Scheme_Object *i, Scheme_Object *vec)
{
  int n = SCHEME_VEC_SIZE(vec) - 1;

  if (SCHEME_VEC_SIZE(vec)) {
    char *vstr;
    int vlen;
    vstr = scheme_make_provided_string(vec, 2, &vlen);
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: index %s out of range [%d, %d] for vector: %t",
		     name, 
		     scheme_make_provided_string(i, 2, NULL), 
		     0, n,
		     vstr, vlen);
  } else
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: bad index %s for empty vector",
		     name,
		     scheme_make_provided_string(i, 0, NULL));
  
  return NULL;
}

static Scheme_Object *
vector_ref (int argc, Scheme_Object *argv[])
{
  long i, len;

  if (!SCHEME_VECTORP(argv[0]))
    scheme_wrong_type("vector-ref", "vector", 0, argc, argv);

  len = SCHEME_VEC_SIZE(argv[0]);

  i = scheme_extract_index("vector-ref", 1, argc, argv, len, 0);

  if (i >= len)
    return bad_index("vector-ref", argv[1], argv[0]);

  return (SCHEME_VEC_ELS(argv[0]))[i];
}

static Scheme_Object *
vector_set(int argc, Scheme_Object *argv[])
{
  long i, len;

  if (!SCHEME_MUTABLE_VECTORP(argv[0]))
    scheme_wrong_type("vector-set!", "mutable vector", 0, argc, argv);

  len = SCHEME_VEC_SIZE(argv[0]);

  i = scheme_extract_index("vector-set!", 1, argc, argv, len, 0);

  if (i >= len)
    return bad_index("vector-set!", argv[1], argv[0]);

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

Scheme_Object *
scheme_vector_to_list (Scheme_Object *vec)
{
  int i;
  Scheme_Object *pair = scheme_null;

  i = SCHEME_VEC_SIZE(vec);

  if (i < 0xFFF) {
    for (; i--; ) {
      pair = scheme_make_pair(SCHEME_VEC_ELS(vec)[i], pair);
    }
  } else {
    for (; i--; ) {
      if (!(i & 0xFFF))
	SCHEME_USE_FUEL(0xFFF);
      pair = scheme_make_pair(SCHEME_VEC_ELS(vec)[i], pair);
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
  int len, i;
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
  int i;
  
  if (!SCHEME_MUTABLE_VECTORP(argv[0]))
    scheme_wrong_type("vector-fill!", "mutable vector", 0, argc, argv);

  for (i = 0; i < SCHEME_VEC_SIZE(argv[0]); i++) {
    SCHEME_VEC_ELS(argv[0])[i] = argv[1];
  }

  return argv[0];
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
