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

/* globals */
Scheme_Object scheme_null[1];

/* locals */
static Scheme_Object *pair_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cons_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *car_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *set_car_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *set_cdr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cons_immutable (int argc, Scheme_Object *argv[]);
static Scheme_Object *null_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_immutable_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_star_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_star_immutable_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *immutablep (int argc, Scheme_Object *argv[]);
static Scheme_Object *length_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *append_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *append_bang_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *reverse_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *reverse_bang_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_tail_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_ref_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *memv (int argc, Scheme_Object *argv[]);
static Scheme_Object *memq (int argc, Scheme_Object *argv[]);
static Scheme_Object *member (int argc, Scheme_Object *argv[]);
static Scheme_Object *assv (int argc, Scheme_Object *argv[]);
static Scheme_Object *assq (int argc, Scheme_Object *argv[]);
static Scheme_Object *assoc (int argc, Scheme_Object *argv[]);
static Scheme_Object *caar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cddar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdddr_prim (int argc, Scheme_Object *argv[]);

static Scheme_Object *cddddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdaddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cddadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdddar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caaddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caddar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdaadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdadar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cddaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdaaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caadar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caaadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caaaar_prim (int argc, Scheme_Object *argv[]);

static Scheme_Object *box (int argc, Scheme_Object *argv[]);
static Scheme_Object *immutable_box (int argc, Scheme_Object *argv[]);
static Scheme_Object *box_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *unbox (int argc, Scheme_Object *argv[]);
static Scheme_Object *set_box (int argc, Scheme_Object *argv[]);

static Scheme_Object *make_hash_table(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_immutable_hash_table(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_count(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_copy(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_put(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_get(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_remove(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_map(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_for_each(int argc, Scheme_Object *argv[]);
static Scheme_Object *eq_hash_code(int argc, Scheme_Object *argv[]);
static Scheme_Object *equal_hash_code(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_weak_box(int argc, Scheme_Object *argv[]);
static Scheme_Object *weak_box_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *weak_boxp(int argc, Scheme_Object *argv[]);

#define BOX "box"
#define BOXP "box?"
#define UNBOX "unbox"
#define SETBOX "set-box!"

static Scheme_Object *weak_symbol, *equal_symbol;

void
scheme_init_list (Scheme_Env *env)
{
  scheme_null->type = scheme_null_type;

  scheme_add_global_constant ("null", scheme_null, env);

  scheme_add_global_constant ("pair?",
			      scheme_make_folding_prim(pair_p_prim,
						       "pair?",
						       1, 1, 1),
			      env);
  scheme_add_global_constant ("cons",
			      scheme_make_prim_w_arity(cons_prim,
						       "cons",
						       2, 2),
			      env);
  scheme_add_global_constant ("car",
			      scheme_make_prim_w_arity(car_prim,
						       "car",
						       1, 1),
			      env);
  scheme_add_global_constant ("cdr",
			      scheme_make_prim_w_arity(cdr_prim,
						       "cdr",
						       1, 1),
			      env);
  scheme_add_global_constant ("set-car!",
			      scheme_make_prim_w_arity(set_car_prim,
						       "set-car!",
						       2, 2),
			      env);
  scheme_add_global_constant ("set-cdr!",
			      scheme_make_prim_w_arity(set_cdr_prim,
						       "set-cdr!",
						       2, 2),
			      env);
  scheme_add_global_constant ("cons-immutable",
			      scheme_make_prim_w_arity(cons_immutable,
						       "cons-immutable",
						       2, 2),
			      env);
  scheme_add_global_constant ("null?",
			      scheme_make_folding_prim(null_p_prim,
						       "null?",
						       1, 1, 1),
			      env);
  scheme_add_global_constant ("list?",
			      scheme_make_prim_w_arity(list_p_prim,
						       "list?",
						       1, 1),
			      env);
  scheme_add_global_constant ("list",
			      scheme_make_prim_w_arity(list_prim,
						       "list",
						       0, -1),
			      env);
  scheme_add_global_constant ("list-immutable",
			      scheme_make_prim_w_arity(list_immutable_prim,
						       "list-immutable",
						       0, -1),
			      env);
  scheme_add_global_constant ("list*",
			      scheme_make_prim_w_arity(list_star_prim,
						       "list*",
						       1, -1),
			      env);
  scheme_add_global_constant ("list*-immutable",
			      scheme_make_prim_w_arity(list_star_immutable_prim,
						       "list*-immutable",
						       1, -1),
			      env);
  scheme_add_global_constant("immutable?",
			     scheme_make_folding_prim(immutablep,
						      "immutable?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant ("length",
			      scheme_make_prim_w_arity(length_prim,
						       "length",
						       1, 1),
			      env);
  scheme_add_global_constant ("append",
			      scheme_make_prim_w_arity(append_prim,
						       "append",
						       0, -1),
			      env);
  scheme_add_global_constant ("append!",
			      scheme_make_prim_w_arity(append_bang_prim,
						       "append!",
						       0, -1),
			      env);
  scheme_add_global_constant ("reverse",
			      scheme_make_prim_w_arity(reverse_prim,
						       "reverse",
						       1, 1),
			      env);
  scheme_add_global_constant ("reverse!",
			      scheme_make_prim_w_arity(reverse_bang_prim,
						       "reverse!",
						       1, 1),
			      env);
  scheme_add_global_constant ("list-tail",
			      scheme_make_prim_w_arity(list_tail_prim,
						       "list-tail",
						       2, 2),
			      env);
  scheme_add_global_constant ("list-ref",
			      scheme_make_prim_w_arity(list_ref_prim,
						       "list-ref",
						       2, 2),
			      env);
  scheme_add_global_constant ("memq",
			      scheme_make_prim_w_arity(memq,
						       "memq",
						       2, 2),
			      env);
  scheme_add_global_constant ("memv",
			      scheme_make_prim_w_arity(memv,
						       "memv",
						       2, 2),
			      env);
  scheme_add_global_constant ("member",
			      scheme_make_prim_w_arity(member,
						       "member",
						       2, 2),
			      env);
  scheme_add_global_constant ("assq",
			      scheme_make_prim_w_arity(assq,
						       "assq",
						       2, 2),
			      env);
  scheme_add_global_constant ("assv",
			      scheme_make_prim_w_arity(assv,
						       "assv",
						       2, 2),
			      env);
  scheme_add_global_constant ("assoc",
			      scheme_make_prim_w_arity(assoc,
						       "assoc",
						       2, 2),
			      env);
  scheme_add_global_constant ("caar",
			      scheme_make_prim_w_arity(caar_prim,
						       "caar",
						       1, 1),
			      env);
  scheme_add_global_constant ("cadr",
			      scheme_make_prim_w_arity(cadr_prim,
						       "cadr",
						       1, 1),
			      env);
  scheme_add_global_constant ("cdar",
			      scheme_make_prim_w_arity(cdar_prim,
						       "cdar",
						       1, 1),
			      env);
  scheme_add_global_constant ("cddr",
			      scheme_make_prim_w_arity(cddr_prim,
						       "cddr",
						       1, 1),
			      env);
  scheme_add_global_constant ("caaar",
			      scheme_make_prim_w_arity(caaar_prim,
						       "caaar",
						       1, 1),
			      env);
  scheme_add_global_constant ("caadr",
			      scheme_make_prim_w_arity(caadr_prim,
						       "caadr",
						       1, 1),
			      env);
  scheme_add_global_constant ("cadar",
			      scheme_make_prim_w_arity(cadar_prim,
						       "cadar",
						       1, 1),
			      env);
  scheme_add_global_constant ("cdaar",
			      scheme_make_prim_w_arity(cdaar_prim,
						       "cdaar",
						       1, 1),
			      env);
  scheme_add_global_constant ("cdadr",
			      scheme_make_prim_w_arity(cdadr_prim,
						       "cdadr",
						       1, 1),
			      env);
  scheme_add_global_constant ("cddar",
			      scheme_make_prim_w_arity(cddar_prim,
						       "cddar",
						       1, 1),
			      env);
  scheme_add_global_constant ("caddr",
			      scheme_make_prim_w_arity(caddr_prim,
						       "caddr",
						       1, 1),
			      env);
  scheme_add_global_constant ("cdddr",
			      scheme_make_prim_w_arity(cdddr_prim,
						       "cdddr",
						       1, 1),
			      env);
  scheme_add_global_constant ("cddddr",
			      scheme_make_prim_w_arity(cddddr_prim,
						       "cddddr",
						       1, 1),
			      env);

  scheme_add_global_constant ("cadddr",
			      scheme_make_prim_w_arity(cadddr_prim,
						       "cadddr",
						       1, 1),
			      env);
  scheme_add_global_constant ("cdaddr",
			      scheme_make_prim_w_arity(cdaddr_prim,
						       "cdaddr",
						       1, 1),
			      env);
  scheme_add_global_constant ("cddadr",
			      scheme_make_prim_w_arity(cddadr_prim,
						       "cddadr",
						       1, 1),
			      env);
  scheme_add_global_constant ("cdddar",
			      scheme_make_prim_w_arity(cdddar_prim,
						       "cdddar",
						       1, 1),
			      env);
  scheme_add_global_constant ("caaddr",
			      scheme_make_prim_w_arity(caaddr_prim,
						       "caaddr",
						       1, 1),
			      env);
  scheme_add_global_constant ("cadadr",
			      scheme_make_prim_w_arity(cadadr_prim,
						       "cadadr",
						       1, 1),
			      env);
  scheme_add_global_constant ("caddar",
			      scheme_make_prim_w_arity(caddar_prim,
						       "caddar",
						       1, 1),
			      env);
  scheme_add_global_constant ("cdaadr",
			      scheme_make_prim_w_arity(cdaadr_prim,
						       "cdaadr",
						       1, 1),
			      env);
  scheme_add_global_constant ("cdadar",
			      scheme_make_prim_w_arity(cdadar_prim,
						       "cdadar",
						       1, 1),
			      env);
  scheme_add_global_constant ("cddaar",
			      scheme_make_prim_w_arity(cddaar_prim,
						       "cddaar",
						       1, 1),
			      env);
  scheme_add_global_constant ("cdaaar",
			      scheme_make_prim_w_arity(cdaaar_prim,
						       "cdaaar",
						       1, 1),
			      env);
  scheme_add_global_constant ("cadaar",
			      scheme_make_prim_w_arity(cadaar_prim,
						       "cadaar",
						       1, 1),
			      env);
  scheme_add_global_constant ("caadar",
			      scheme_make_prim_w_arity(caadar_prim,
						       "caadar",
						       1, 1),
			      env);
  scheme_add_global_constant ("caaadr",
			      scheme_make_prim_w_arity(caaadr_prim,
						       "caaadr",
						       1, 1),
			      env);
  scheme_add_global_constant ("caaaar",
			      scheme_make_prim_w_arity(caaaar_prim,
						       "caaaar",
						       1, 1),
			      env);

  scheme_add_global_constant(BOX,
			     scheme_make_prim_w_arity(box,
						      BOX,
						      1, 1),
			     env);
  scheme_add_global_constant("box-immutable",
			     scheme_make_prim_w_arity(immutable_box,
						      "box-immutable",
						      1, 1),
			     env);
  scheme_add_global_constant(BOXP,
			     scheme_make_folding_prim(box_p,
						      BOXP,
						      1, 1, 1),
			     env);
  scheme_add_global_constant(UNBOX,
			     scheme_make_prim_w_arity(unbox,
						      UNBOX,
						      1, 1),
			     env);
  scheme_add_global_constant(SETBOX,
			     scheme_make_prim_w_arity(set_box,
						      SETBOX,
						      2, 2),
			     env);

  scheme_add_global_constant("make-hash-table",
			     scheme_make_prim_w_arity(make_hash_table,
						      "make-hash-table",
						      0, 2),
			     env);
  scheme_add_global_constant("make-immutable-hash-table",
			     scheme_make_prim_w_arity(make_immutable_hash_table,
						      "make-immutable-hash-table",
						      1, 2),
			     env);
  scheme_add_global_constant("hash-table?",
			     scheme_make_folding_prim(hash_table_p,
						      "hash-table?",
						      1, 3, 1),
			     env);
  scheme_add_global_constant("hash-table-count",
			     scheme_make_prim_w_arity(hash_table_count,
						      "hash-table-count",
						      1, 1),
			     env);
  scheme_add_global_constant("hash-table-copy",
			     scheme_make_prim_w_arity(hash_table_copy,
						      "hash-table-copy",
						      1, 1),
			     env);
  scheme_add_global_constant("hash-table-put!",
			     scheme_make_prim_w_arity(hash_table_put,
						      "hash-table-put!",
						      3, 3),
			     env);
  scheme_add_global_constant("hash-table-get",
			     scheme_make_prim_w_arity(hash_table_get,
						      "hash-table-get",
						      2, 3),
			     env);
  scheme_add_global_constant("hash-table-remove!",
			     scheme_make_prim_w_arity(hash_table_remove,
						      "hash-table-remove!",
						      2, 2),
			     env);
  scheme_add_global_constant("hash-table-map",
			     scheme_make_prim_w_arity(hash_table_map,
						      "hash-table-map",
						      2, 2),
			     env);
  scheme_add_global_constant("hash-table-for-each",
			     scheme_make_prim_w_arity(hash_table_for_each,
						      "hash-table-for-each",
						      2, 2),
			     env);

  scheme_add_global_constant("eq-hash-code",
			     scheme_make_prim_w_arity(eq_hash_code,
						      "eq-hash-code",
						      1, 1),
			     env);
  scheme_add_global_constant("equal-hash-code",
			     scheme_make_prim_w_arity(equal_hash_code,
						      "equal-hash-code",
						      1, 1),
			     env);

  scheme_add_global_constant("make-weak-box",
			     scheme_make_prim_w_arity(make_weak_box,
						      "make-weak-box",
						      1, 1),
			     env);
  scheme_add_global_constant("weak-box-value",
			     scheme_make_prim_w_arity(weak_box_value,
						      "weak-box-value",
						      1, 1),
			     env);
  scheme_add_global_constant("weak-box?",
			     scheme_make_folding_prim(weak_boxp,
						      "weak-box?",
						      1, 1, 1),
			     env);

  REGISTER_SO(weak_symbol);
  REGISTER_SO(equal_symbol);

  weak_symbol = scheme_intern_symbol("weak");
  equal_symbol = scheme_intern_symbol("equal");
}

Scheme_Object *scheme_make_pair(Scheme_Object *car, Scheme_Object *cdr)
{
  Scheme_Object *cons;

  cons = scheme_alloc_object();
  cons->type = scheme_pair_type;
  SCHEME_CAR(cons) = car;
  SCHEME_CDR(cons) = cdr;
  return cons;
}

Scheme_Object *scheme_make_immutable_pair(Scheme_Object *car, Scheme_Object *cdr)
{
  Scheme_Object *cons;

  cons = scheme_alloc_object();
  cons->type = scheme_pair_type;
  SCHEME_CAR(cons) = car;
  SCHEME_CDR(cons) = cdr;
  SCHEME_SET_PAIR_IMMUTABLE(cons);
  return cons;
}

Scheme_Object *scheme_build_list(int size, Scheme_Object **argv)
{
  Scheme_Object *pair = scheme_null;
  int i;

  for (i = size; i--; ) {
    pair = scheme_make_pair(argv[i], pair);
  }

  return pair;
}

Scheme_Object *scheme_alloc_list(int size)
{
  Scheme_Object *pair = scheme_null;
  int i;

  for (i = size; i--; ) {
    pair = scheme_make_pair(scheme_false, pair);
  }

  return pair;
}

void scheme_make_list_immutable(Scheme_Object *l)
{
  for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    if (SCHEME_MUTABLEP(l))
      SCHEME_SET_IMMUTABLE(l);
  }
}

int
scheme_list_length (Scheme_Object *list)
{
  int len;

  len = 0;
  while (!SCHEME_NULLP(list)) {
    len++;
    if (SCHEME_PAIRP(list))
      list = SCHEME_CDR(list);
    else
      list = scheme_null;
  }

  return len;
}

int
scheme_proper_list_length (Scheme_Object *list)
{
  int len;
  Scheme_Object *turtle;

  len = 0;
  turtle = list;
  while (SCHEME_PAIRP(list)) {
    len++;
    list = SCHEME_CDR(list);
    if (!SCHEME_PAIRP(list))
      break;
    len++;
    list = SCHEME_CDR(list);

    if (SAME_OBJ(turtle, list))
      break;

    turtle = SCHEME_CDR(turtle);
  }

  if (SCHEME_NULLP(list))
    return len;

  return -1;
}

Scheme_Object *
scheme_named_map_1(char *name, Scheme_Object *(*fun)(Scheme_Object*, Scheme_Object*),
		   Scheme_Object *lst, Scheme_Object *form)
{
  if (SCHEME_STX_NULLP(lst))
      return (scheme_null);
  else if (SCHEME_STX_PAIRP(lst)) {
    Scheme_Object *v;
    v = SCHEME_STX_CAR(lst);
    v = fun(v, form);
    lst = SCHEME_STX_CDR(lst);
    return scheme_make_pair(v, scheme_named_map_1(name, fun, lst, form));
  } else {
    scheme_wrong_syntax(name, lst, form, "bad syntax (" IMPROPER_LIST_FORM ")");
    return scheme_void;
  }
}

Scheme_Object *
scheme_map_1 (Scheme_Object *(*fun)(Scheme_Object*), Scheme_Object *lst)
{
  return scheme_named_map_1("map",
			    (Scheme_Object *(*)(Scheme_Object *, Scheme_Object *))fun,
			    lst, NULL);
}

Scheme_Object *
scheme_car (Scheme_Object *pair)
{
  return (SCHEME_CAR (pair));
}

Scheme_Object *
scheme_cdr (Scheme_Object *pair)
{
  return (SCHEME_CDR (pair));
}

Scheme_Object *
scheme_cadr (Scheme_Object *pair)
{
  return (SCHEME_CAR (SCHEME_CDR (pair)));
}

Scheme_Object *
scheme_caddr (Scheme_Object *pair)
{
  return (SCHEME_CAR (SCHEME_CDR (SCHEME_CDR (pair))));
}

Scheme_Object *scheme_copy_list(Scheme_Object *l)
{
  return scheme_vector_to_list(scheme_list_to_vector(l));
}

/* local functions */

static Scheme_Object *
pair_p_prim (int argc, Scheme_Object *argv[])
{
  return (SCHEME_PAIRP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
cons_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *cons;

  cons = scheme_make_pair(argv[0], argv[1]);
  return (cons);
}

static Scheme_Object *
cons_immutable (int argc, Scheme_Object *argv[])
{
  Scheme_Object *cons;

  cons = scheme_make_immutable_pair(argv[0], argv[1]);
  return (cons);
}

static Scheme_Object *
car_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PAIRP(argv[0]))
    scheme_wrong_type("car", "pair", 0, argc, argv);
  return (SCHEME_CAR (argv[0]));
}

static Scheme_Object *
cdr_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PAIRP(argv[0]))
    scheme_wrong_type("cdr", "pair", 0, argc, argv);

  return (SCHEME_CDR (argv[0]));
}

static Scheme_Object *
set_car_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_MUTABLE_PAIRP(argv[0]))
    scheme_wrong_type("set-car!", "mutable-pair", 0, argc, argv);

  SCHEME_CAR (argv[0]) = argv[1];
  return scheme_void;
}

static Scheme_Object *
set_cdr_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_MUTABLE_PAIRP(argv[0]))
    scheme_wrong_type("set-cdr!", "mutable-pair", 0, argc, argv);

  SCHEME_CDR (argv[0]) = argv[1];
  return scheme_void;
}

static Scheme_Object *
null_p_prim (int argc, Scheme_Object *argv[])
{
  return (SCHEME_NULLP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
list_p_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *obj1, *obj2;

  obj1 = obj2 = argv[0];
  do {
    if (SCHEME_NULLP(obj1))
      return scheme_true;
    if (!SCHEME_PAIRP(obj1))
      return (scheme_false);

    obj1 = SCHEME_CDR (obj1);

    if (SCHEME_NULLP(obj1))
      return scheme_true;
    if (!SCHEME_PAIRP(obj1))
      return scheme_false;

    obj1 = SCHEME_CDR(obj1);

    obj2 = SCHEME_CDR(obj2);
  } while (NOT_SAME_OBJ(obj1, obj2));

  return scheme_false;
}

static Scheme_Object *
list_exec (int argc, Scheme_Object *argv[], int star)
{
  int i;
  Scheme_Object *l;

  if (star) {
    --argc;
    l = argv[argc];
  } else
    l = scheme_null;

  for (i = argc ; i--; ) {
    l = scheme_make_pair(argv[i], l);
  }

  return l;
}

static Scheme_Object *
list_prim (int argc, Scheme_Object *argv[])
{
  return list_exec(argc, argv, 0);
}

static Scheme_Object *
list_immutable_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *l;
  l = list_exec(argc, argv, 0);
  scheme_make_list_immutable(l);
  return l;
}

static Scheme_Object *
list_star_prim (int argc, Scheme_Object *argv[])
{
  return list_exec(argc, argv, 1);
}

static Scheme_Object *
list_star_immutable_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *l;
  l = list_exec(argc, argv, 1);
  scheme_make_list_immutable(l);
  return l;
}

static Scheme_Object *
immutablep (int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  return ((!SCHEME_INTP(v)
	   && SCHEME_IMMUTABLEP(v)
	   && (SCHEME_PAIRP(v)
	       || SCHEME_VECTORP(v)
	       || SCHEME_BYTE_STRINGP(v)
	       || SCHEME_CHAR_STRINGP(v)
	       || SCHEME_BOXP(v)))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *
length_prim (int argc, Scheme_Object *argv[])
{
  int l;

  if (!SCHEME_LISTP(argv[0]))
    scheme_wrong_type("length", "proper list", 0, argc, argv);

  l = scheme_proper_list_length(argv[0]);

  if (l < 0)
    scheme_wrong_type("length", "proper list", 0, argc, argv);

  return scheme_make_integer(l);
}

Scheme_Object *
scheme_append (Scheme_Object *lst1, Scheme_Object *lst2)
{
  Scheme_Object *first, *last, *orig1, *v;

  orig1 = lst1;

  first = last = NULL;
  while (SCHEME_PAIRP(lst1)) {
    v = scheme_make_pair(SCHEME_CAR(lst1), scheme_null);
    if (!first)
      first = v;
    else
      SCHEME_CDR(last) = v;
    last = v;
    lst1 = SCHEME_CDR(lst1);

    SCHEME_USE_FUEL(1);
  }

  if (!SCHEME_NULLP(lst1))
    scheme_wrong_type("append", "proper list", -1, 0, &orig1);

  if (!last)
    return lst2;

  SCHEME_CDR(last) = lst2;

  return first;
}

static Scheme_Object *
scheme_append_bang (Scheme_Object *lst1, Scheme_Object *lst2)
{
  if (SCHEME_NULLP(lst1))
    return lst2;
  else {
    Scheme_Object *prev, *orig;

    orig = lst1;

    do {
      prev = lst1;
      if (!SCHEME_PAIRP(lst1))
	scheme_wrong_type("append!", "proper list", -1, 0, &lst1);
      lst1 = SCHEME_CDR(lst1);

      SCHEME_USE_FUEL(1);
    } while (!SCHEME_NULLP(lst1));

    if (!SCHEME_MUTABLE_PAIRP(prev))
      scheme_wrong_type("append!", "mutable proper list", -1, 0, &lst1);
    SCHEME_CDR(prev) = lst2;

    return orig;
  }
}

static Scheme_Object *
append_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *res;
  int i;

  if (!argc)
    return scheme_null;

  res = argv[argc - 1];
  for (i = argc - 1; i--;  ) {
    res = scheme_append(argv[i], res);
  }

  return res;
}

static Scheme_Object *
append_bang_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *res;
  int i;

  if (!argc)
    return scheme_null;

  res = argv[argc - 1];
  for (i = argc - 1; i--; ) {
    res = scheme_append_bang(argv[i], res);
  }

  return res;
}

static Scheme_Object *
reverse_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *lst, *last;

  last = scheme_null;
  lst = argv[0];
  while (!SCHEME_NULLP (lst)) {
    if (!SCHEME_PAIRP(lst))
      scheme_wrong_type("reverse", "proper list", 0, argc, argv);
    last = scheme_make_pair (SCHEME_CAR (lst), last);
    lst = SCHEME_CDR (lst);

    SCHEME_USE_FUEL(1);
  }
  return (last);
}

static Scheme_Object *
reverse_bang_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *lst, *prev, *next;

  prev = NULL;
  lst = argv[0];
  while (!SCHEME_NULLP(lst)) {
    if (!SCHEME_MUTABLE_PAIRP(lst))
      scheme_wrong_type("reverse!", "mutable proper list", 0, argc, argv);
    next = SCHEME_CDR(lst);
    if (prev)
      SCHEME_CDR(lst) = prev;
    else
      SCHEME_CDR(lst) = scheme_null;
    prev = lst;
    lst = next;

    SCHEME_USE_FUEL(1);
  }

  if (prev)
    return prev;
  else
    return scheme_null;
}

#define OCCASIONAL_CHECK ((int)0xFF)
#ifdef PALMOS_STUFF
# define LISTREF_BIGNUM_SLICE 1000
#else
# define LISTREF_BIGNUM_SLICE 1000000
#endif

static Scheme_Object *
do_list_ref(char *name, int takecar, int argc, Scheme_Object *argv[])
{
  long i, k;
  Scheme_Object *lst, *index, *bnindex;

  if (SCHEME_BIGNUMP(argv[1])) {
    bnindex = argv[1];
    k = 0;
  } else if (!SCHEME_INTP(argv[1])) {
    scheme_wrong_type(name, "non-negative exact integer", 1, argc, argv);
    return NULL;
  } else {
    bnindex = NULL;
    k = SCHEME_INT_VAL(argv[1]);
  }

  lst = argv[0];
  index = argv[1];

  if ((bnindex && !SCHEME_BIGPOS(bnindex))
      || (!bnindex && (k < 0))) {
    scheme_wrong_type(name, "non-negative exact integer", 1, argc, argv);
    return NULL;
  }

  do {
    if (bnindex) {
      if (SCHEME_INTP(bnindex)) {
	k = SCHEME_INT_VAL(bnindex);
	bnindex = 0;
      } else {
	k = LISTREF_BIGNUM_SLICE;
	bnindex = scheme_bin_minus(bnindex, scheme_make_integer(LISTREF_BIGNUM_SLICE));
      }
    }

    for (i = 0; i < k; i++) {
      if (!SCHEME_PAIRP(lst)) {
	char *lstr;
	int llen;

	lstr = scheme_make_provided_string(argv[0], 2, &llen);
	scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			 "%s: index %s too large for list%s: %t", name,
			 scheme_make_provided_string(index, 2, NULL),
			 SCHEME_NULLP(lst) ? "" : " (not a proper list)",
			 lstr, llen);
	return NULL;
      }
      lst = SCHEME_CDR(lst);
      if (!(i & OCCASIONAL_CHECK))
	SCHEME_USE_FUEL(OCCASIONAL_CHECK);
    }
  } while(bnindex);

  if (takecar) {
    if (!SCHEME_PAIRP(lst)) {
      char *lstr;
      int llen;

      lstr = scheme_make_provided_string(argv[0], 2, &llen);
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: index %s too large for list%s: %t", name,
		       scheme_make_provided_string(index, 2, NULL),
		       SCHEME_NULLP(lst) ? "" : " (not a proper list)",
		       lstr, llen);
      return NULL;
    }

    return SCHEME_CAR(lst);
  } else
    return lst;
}

static Scheme_Object *
list_tail_prim(int argc, Scheme_Object *argv[])
{
  return do_list_ref("list-tail", 0, argc, argv);
}

static Scheme_Object *
list_ref_prim(int argc, Scheme_Object *argv[])
{
  return do_list_ref("list-ref", 1, argc, argv);
}


#define GEN_MEM(name, scheme_name, comp) \
static Scheme_Object * \
name (int argc, Scheme_Object *argv[]) \
{ \
  Scheme_Object *list; \
  list = argv[1]; \
  while (SCHEME_PAIRP(list)) \
    { \
      if (comp (argv[0], SCHEME_CAR (list))) \
	{ \
          return (list); \
	} \
      list = SCHEME_CDR (list); \
    } \
  if (!SCHEME_NULLP(list)) { \
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, \
		     "%s: not a proper list: %V", #scheme_name, \
		     argv[1]); \
  } \
  return (scheme_false); \
}

GEN_MEM(memv, memv, scheme_eqv)
GEN_MEM(memq, memq, scheme_eq)
GEN_MEM(member, member, scheme_equal)

#define GEN_ASS(name, scheme_name, comp) \
static Scheme_Object * \
name (int argc, Scheme_Object *argv[]) \
{ \
  Scheme_Object *pair, *list; \
  list = argv[1]; \
  while (SCHEME_PAIRP (list)) \
    { \
      pair = SCHEME_CAR (list); \
      if (!SCHEME_PAIRP (pair)) {\
        char *npstr, *lstr; \
        int nplen, llen; \
        npstr = scheme_make_provided_string(pair, 2, &nplen); \
        lstr = scheme_make_provided_string(argv[1], 2, &llen); \
	scheme_raise_exn(MZEXN_FAIL_CONTRACT, \
			 "%s: non-pair found in list: %t in %t", #scheme_name, \
			 npstr, nplen, \
			 lstr, llen); \
	return NULL; \
      } \
      if (comp (argv[0], SCHEME_CAR (pair))) \
	{ \
          return (pair); \
	} \
      list = SCHEME_CDR (list); \
    } \
  if (!SCHEME_NULLP(list)) {\
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, \
		     "%s: not a proper list: %V", #scheme_name, \
		     argv[1]); \
  } \
  return (scheme_false); \
}

GEN_ASS(assv, assv, scheme_eqv)
GEN_ASS(assq, assq, scheme_eq)
GEN_ASS(assoc, assoc, scheme_equal)

#define LISTFUNC2(name, C, D) \
static Scheme_Object * \
name ## _prim (int argc, Scheme_Object *argv[]) \
{ \
  if (!(SCHEME_PAIRP(argv[0]) \
	&& SCHEME_PAIRP(D(argv[0])))) \
      scheme_wrong_type(#name, #name "able value", 0, argc, argv); \
  return C(D(argv[0])); \
}

LISTFUNC2(cddr, SCHEME_CDR, SCHEME_CDR)
LISTFUNC2(cadr, SCHEME_CAR, SCHEME_CDR)
LISTFUNC2(cdar, SCHEME_CDR, SCHEME_CAR)
LISTFUNC2(caar, SCHEME_CAR, SCHEME_CAR)

#define LISTFUNC3(name, B, C, D) \
static Scheme_Object * \
name ## _prim (int argc, Scheme_Object *argv[]) \
{ \
  if (!((SCHEME_PAIRP(argv[0])) \
	&& SCHEME_PAIRP(D(argv[0])) \
	&& SCHEME_PAIRP(C(D(argv[0]))))) \
    scheme_wrong_type(#name, #name "able value", 0, argc, argv); \
  return B (C (D (argv[0]))); \
}

LISTFUNC3(cdddr, SCHEME_CDR, SCHEME_CDR, SCHEME_CDR)

LISTFUNC3(caddr, SCHEME_CAR, SCHEME_CDR, SCHEME_CDR)
LISTFUNC3(cdadr, SCHEME_CDR, SCHEME_CAR, SCHEME_CDR)
LISTFUNC3(cddar, SCHEME_CDR, SCHEME_CDR, SCHEME_CAR)

LISTFUNC3(cdaar, SCHEME_CDR, SCHEME_CAR, SCHEME_CAR)
LISTFUNC3(cadar, SCHEME_CAR, SCHEME_CDR, SCHEME_CAR)
LISTFUNC3(caadr, SCHEME_CAR, SCHEME_CAR, SCHEME_CDR)

LISTFUNC3(caaar, SCHEME_CAR, SCHEME_CAR, SCHEME_CAR)


#define LISTFUNC4(name, A, B, C, D) \
static Scheme_Object * \
name ## _prim (int argc, Scheme_Object *argv[]) \
{ \
  if (!(SCHEME_PAIRP(argv[0]) \
	&& SCHEME_PAIRP(D (argv[0])) \
	&& SCHEME_PAIRP(C(D(argv[0]))) \
	&&SCHEME_PAIRP(B(C(D(argv[0]))))))\
    scheme_wrong_type(#name, #name "able value", 0, argc, argv); \
  return A(B(C(D(argv[0]))));\
}

LISTFUNC4(cddddr, SCHEME_CDR, SCHEME_CDR, SCHEME_CDR, SCHEME_CDR)

LISTFUNC4(cadddr, SCHEME_CAR, SCHEME_CDR, SCHEME_CDR, SCHEME_CDR)
LISTFUNC4(cdaddr, SCHEME_CDR, SCHEME_CAR, SCHEME_CDR, SCHEME_CDR)
LISTFUNC4(cddadr, SCHEME_CDR, SCHEME_CDR, SCHEME_CAR, SCHEME_CDR)
LISTFUNC4(cdddar, SCHEME_CDR, SCHEME_CDR, SCHEME_CDR, SCHEME_CAR)

LISTFUNC4(caaddr, SCHEME_CAR, SCHEME_CAR, SCHEME_CDR, SCHEME_CDR)
LISTFUNC4(cadadr, SCHEME_CAR, SCHEME_CDR, SCHEME_CAR, SCHEME_CDR)
LISTFUNC4(caddar, SCHEME_CAR, SCHEME_CDR, SCHEME_CDR, SCHEME_CAR)
LISTFUNC4(cdaadr, SCHEME_CDR, SCHEME_CAR, SCHEME_CAR, SCHEME_CDR)
LISTFUNC4(cdadar, SCHEME_CDR, SCHEME_CAR, SCHEME_CDR, SCHEME_CAR)
LISTFUNC4(cddaar, SCHEME_CDR, SCHEME_CDR, SCHEME_CAR, SCHEME_CAR)

LISTFUNC4(cdaaar, SCHEME_CDR, SCHEME_CAR, SCHEME_CAR, SCHEME_CAR)
LISTFUNC4(cadaar, SCHEME_CAR, SCHEME_CDR, SCHEME_CAR, SCHEME_CAR)
LISTFUNC4(caadar, SCHEME_CAR, SCHEME_CAR, SCHEME_CDR, SCHEME_CAR)
LISTFUNC4(caaadr, SCHEME_CAR, SCHEME_CAR, SCHEME_CAR, SCHEME_CDR)

LISTFUNC4(caaaar, SCHEME_CAR, SCHEME_CAR, SCHEME_CAR, SCHEME_CAR)

Scheme_Object *scheme_box(Scheme_Object *v)
{
  Scheme_Object *obj;

  obj = scheme_alloc_small_object();
  obj->type = scheme_box_type;
  SCHEME_BOX_VAL(obj) = v;

  return obj;
}

Scheme_Object *scheme_unbox(Scheme_Object *obj)
{
  if (!SCHEME_BOXP(obj))
      scheme_wrong_type(UNBOX, "box", 0, 1, &obj);
  return (Scheme_Object *)SCHEME_BOX_VAL(obj);
}

void scheme_set_box(Scheme_Object *b, Scheme_Object *v)
{
  if (!SCHEME_MUTABLE_BOXP(b))
      scheme_wrong_type(SETBOX, "mutable box", 0, 1, &b);
  SCHEME_BOX_VAL(b) = v;
}

static Scheme_Object *box(int c, Scheme_Object *p[])
{
  return scheme_box(p[0]);
}

static Scheme_Object *immutable_box(int c, Scheme_Object *p[])
{
  Scheme_Object *obj;

  obj = scheme_box(p[0]);
  SCHEME_SET_IMMUTABLE(obj);

  return obj;
}

static Scheme_Object *box_p(int c, Scheme_Object *p[])
{
  return SCHEME_BOXP(p[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *unbox(int c, Scheme_Object *p[])
{
  return scheme_unbox(p[0]);
}

static Scheme_Object *set_box(int c, Scheme_Object *p[])
{
  scheme_set_box(p[0], p[1]);
  return scheme_void;
}

static int compare_equal(void *v1, void *v2)
{
  return !scheme_equal((Scheme_Object *)v1, (Scheme_Object *)v2);
}

static void make_hash_indices_for_equal(void *v, long *_stk_h1, long *_stk_h2)
{
  *_stk_h1 = scheme_equal_hash_key((Scheme_Object *)v);
  *_stk_h2 = scheme_equal_hash_key2((Scheme_Object *)v);
}

static void check_hash_table_flags(const char *name, int i, int argc, Scheme_Object **argv, int *flags)
{
  for (; i < argc; i++) {
    int j;
    if (SAME_OBJ(argv[i], weak_symbol))
      j = 0;
    else if (SAME_OBJ(argv[i], equal_symbol))
      j = 1;
    else {
      scheme_wrong_type(name, "'weak or 'equal", i, argc, argv);
      return;
    }

    if (flags[j])
      scheme_arg_mismatch(name, "redundant flag: ", argv[i]);

    flags[j] = 1;
  }
}

static Scheme_Object *make_hash_table(int argc, Scheme_Object *argv[])
{
  int flags[2] = { 0 /* weak */ , 0 /* equal */ };

  check_hash_table_flags("make-hash-table", 0, argc, argv, flags);

  if (flags[0]) {
    /* Weak */
    Scheme_Bucket_Table *t;

    t = scheme_make_bucket_table(20, SCHEME_hash_weak_ptr);

    if (flags[1]) {
      Scheme_Object *sema;
      sema = scheme_make_sema(1);
      t->mutex = sema;
      t->compare = compare_equal;
      t->make_hash_indices = make_hash_indices_for_equal;
    }

    return (Scheme_Object *)t;
  } else {
    /* Normal */
    if (flags[1])
      return (Scheme_Object *)scheme_make_hash_table_equal();
    else
      return (Scheme_Object *)scheme_make_hash_table(SCHEME_hash_ptr);
  }
}

static Scheme_Object *make_immutable_hash_table(int argc, Scheme_Object *argv[])
{
  Scheme_Object *l = argv[0], *a;
  Scheme_Hash_Table *ht;

  if (scheme_proper_list_length(l) >= 0) {
    for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      a = SCHEME_CAR(l);
      if (!SCHEME_PAIRP(a))
	break;
    }
  }

  if (!SCHEME_NULLP(l))
    scheme_wrong_type("make-immutable-hash-table", "list of pairs", 0, argc, argv);

  if (argc > 1) {
    if (!SAME_OBJ(equal_symbol, argv[1]))
      scheme_wrong_type("make-immutable-hash-table", "'equal", 1, argc, argv);
    ht = scheme_make_hash_table_equal();
  } else
    ht = scheme_make_hash_table(SCHEME_hash_ptr);

  for (l = argv[0]; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    a = SCHEME_CAR(l);
    scheme_hash_set(ht, SCHEME_CAR(a), SCHEME_CDR(a));
  }

  SCHEME_SET_IMMUTABLE((Scheme_Object *)ht);

  return (Scheme_Object *)ht;
}

Scheme_Hash_Table *scheme_make_hash_table_equal()
{
  Scheme_Hash_Table *t;
  Scheme_Object *sema;

  t = scheme_make_hash_table(SCHEME_hash_ptr);

  sema = scheme_make_sema(1);
  t->mutex = sema;
  t->compare = compare_equal;
  t->make_hash_indices = make_hash_indices_for_equal;

  return t;
}

static Scheme_Object *hash_table_count(int argc, Scheme_Object *argv[])
{
  if (SCHEME_HASHTP(argv[0])) {
    Scheme_Hash_Table *t = (Scheme_Hash_Table *)argv[0];
    return scheme_make_integer(t->count);
  } else if (SCHEME_BUCKTP(argv[0])) {
    Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)argv[0];
    int count = 0, weak, i;
    Scheme_Bucket **buckets, *bucket;
    const char *key;

    buckets = t->buckets;
    weak = t->weak;

    for (i = t->size; i--; ) {
      bucket = buckets[i];
      if (bucket) {
	if (weak) {
	  key = (const char *)HT_EXTRACT_WEAK(bucket->key);
	} else {
	  key = bucket->key;
	}
	if (key)
	  count++;
      }
      SCHEME_USE_FUEL(1);
    }

    return scheme_make_integer(count);
  } else {
    scheme_wrong_type("hash-table-count", "hash-table", 0, argc, argv);
    return NULL;
  }
}

static Scheme_Object *hash_table_copy(int argc, Scheme_Object *argv[])
{
  if (SCHEME_HASHTP(argv[0])) {
    Scheme_Object *o;
    Scheme_Hash_Table *t = (Scheme_Hash_Table *)argv[0];
    if (t->mutex) scheme_wait_sema(t->mutex,0);
    o = (Scheme_Object *)scheme_clone_hash_table(t);
    if (t->mutex) scheme_post_sema(t->mutex);
    return o;
  } else if (SCHEME_BUCKTP(argv[0])) {
    Scheme_Object *o;
    Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)argv[0];
    if (t->mutex) scheme_wait_sema(t->mutex,0);
    o = (Scheme_Object *)scheme_clone_bucket_table(t);
    if (t->mutex) scheme_post_sema(t->mutex);
    return o;
  } else {
    scheme_wrong_type("hash-table-copy", "hash-table", 0, argc, argv);
    return NULL;
  }
}

static Scheme_Object *hash_table_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  int flags[2] = { 0 /* weak */ , 0 /* equal */ };

  check_hash_table_flags("hash-table?", 1, argc, argv, flags);

  if (SCHEME_HASHTP(o)) {
    if (flags[0])
      return scheme_false;
    if (flags[1] && (((Scheme_Hash_Table *)o)->compare != compare_equal))
      return scheme_false;
    return scheme_true;
  } else if (SCHEME_BUCKTP(o)) {
    if (flags[1] && (((Scheme_Bucket_Table *)o)->compare != compare_equal))
      return scheme_false;
    return scheme_true;
  } else
    return scheme_false;
}

int scheme_is_hash_table_equal(Scheme_Object *o)
{
  return (((Scheme_Hash_Table *)o)->compare == compare_equal);
}

static Scheme_Object *hash_table_put(int argc, Scheme_Object *argv[])
{
  if (!(SCHEME_HASHTP(argv[0]) || SCHEME_BUCKTP(argv[0])) || SCHEME_IMMUTABLEP(argv[0]))
    scheme_wrong_type("hash-table-put!", "mutable hash-table", 0, argc, argv);

  if (SCHEME_BUCKTP(argv[0])) {
    Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)argv[0];
    if (t->mutex) scheme_wait_sema(t->mutex,0);
    scheme_add_to_table(t, (char *)argv[1], (void *)argv[2], 0);
    if (t->mutex) scheme_post_sema(t->mutex);
  } else{
    Scheme_Hash_Table *t = (Scheme_Hash_Table *)argv[0];
    if (t->mutex) scheme_wait_sema(t->mutex, 0);
    scheme_hash_set(t, argv[1], argv[2]);
    if (t->mutex) scheme_post_sema(t->mutex);
  }

  return scheme_void;
}

static Scheme_Object *hash_table_get(int argc, Scheme_Object *argv[])
{
  void *v;

  if (!(SCHEME_HASHTP(argv[0]) || SCHEME_BUCKTP(argv[0])))
    scheme_wrong_type("hash-table-get", "hash-table", 0, argc, argv);

  if (SCHEME_BUCKTP(argv[0])){
    Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)argv[0];
    if (t->mutex) scheme_wait_sema(t->mutex, 0);
    v = scheme_lookup_in_table(t, (char *)argv[1]);
    if (t->mutex) scheme_post_sema(t->mutex);
  } else {
    Scheme_Hash_Table *t = (Scheme_Hash_Table *)argv[0];
    if (t->mutex) scheme_wait_sema(t->mutex, 0);
    v = scheme_hash_get(t, argv[1]);
    if (t->mutex) scheme_post_sema(t->mutex);
  }

  if (v)
    return (Scheme_Object *)v;
  else if (argc == 3)
    return _scheme_tail_apply(argv[2], 0, NULL);
  else {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "hash-table-get: no value found for key: %V",
		     argv[1]);
    return scheme_void;
  }
}

static Scheme_Object *hash_table_remove(int argc, Scheme_Object *argv[])
{
  if (!(SCHEME_HASHTP(argv[0]) || SCHEME_BUCKTP(argv[0])) || SCHEME_IMMUTABLEP(argv[0]))
    scheme_wrong_type("hash-table-remove!", "mutable hash-table", 0, argc, argv);

  if (SCHEME_BUCKTP(argv[0])) {
    Scheme_Bucket *b;
    Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)argv[0];
    if (t->mutex) scheme_wait_sema(t->mutex, 0);
    b = scheme_bucket_or_null_from_table((Scheme_Bucket_Table *)argv[0], (char *)argv[1], 0);
    if (b) {
      HT_EXTRACT_WEAK(b->key) = NULL;
      b->val = NULL;
    }
    if (t->mutex) scheme_post_sema(t->mutex);
  } else{
    Scheme_Hash_Table *t = (Scheme_Hash_Table *)argv[0];
    if (t->mutex) scheme_wait_sema(t->mutex, 0);
    scheme_hash_set(t, argv[1], NULL);
    if (t->mutex) scheme_post_sema(t->mutex);
  }

  return scheme_void;
}

static Scheme_Object *do_map_hash_table(int argc,
					Scheme_Object *argv[],
					char *name,
					int keep)
{
  int i;
  Scheme_Object *f;
  Scheme_Object *first, *last = NULL, *v, *p[2];

  if (!(SCHEME_HASHTP(argv[0]) || SCHEME_BUCKTP(argv[0])))
    scheme_wrong_type(name, "hash table", 0, argc, argv);
  scheme_check_proc_arity(name, 2, 1, argc, argv);

  f = argv[1];

  if (keep)
    first = scheme_null;
  else
    first = scheme_void;

  if (SCHEME_BUCKTP(argv[0])) {
    Scheme_Bucket_Table *hash;
    Scheme_Bucket *bucket;

    hash = (Scheme_Bucket_Table *)argv[0];

    for (i = hash->size; i--; ) {
      bucket = hash->buckets[i];
      if (bucket && bucket->val && bucket->key) {
	if (hash->weak)
	  p[0] = (Scheme_Object *)HT_EXTRACT_WEAK(bucket->key);
	else
	  p[0] = (Scheme_Object *)bucket->key;
	p[1] = (Scheme_Object *)bucket->val;
	if (keep) {
	  v = _scheme_apply(f, 2, p);
	  v = scheme_make_pair(v, scheme_null);
	  if (last)
	    SCHEME_CDR(last) = v;
	  else
	    first = v;
	  last = v;
	} else
	  _scheme_apply_multi(f, 2, p);
      }
    }
  } else {
    Scheme_Hash_Table *hash;

    hash = (Scheme_Hash_Table *)argv[0];

    for (i = hash->size; i--; ) {
      if (hash->vals[i]) {
	p[0] = hash->keys[i];
	p[1] = hash->vals[i];
	if (keep) {
	  v = _scheme_apply(f, 2, p);
	  v = scheme_make_pair(v, scheme_null);
	  if (last)
	    SCHEME_CDR(last) = v;
	  else
	    first = v;
	  last = v;
	} else
	  _scheme_apply_multi(f, 2, p);
      }
    }
  }

  return first;
}

static Scheme_Object *hash_table_map(int argc, Scheme_Object *argv[])
{
  return do_map_hash_table(argc, argv, "hash-table-map", 1);
}

static Scheme_Object *hash_table_for_each(int argc, Scheme_Object *argv[])
{
  return do_map_hash_table(argc, argv, "hash-table-for-each", 0);
}

static Scheme_Object *eq_hash_code(int argc, Scheme_Object *argv[])
{
  long v;

  if (SCHEME_INTP(argv[0]))
    return argv[0];

#ifdef MZ_PRECISE_GC
  v = scheme_hash_key(argv[0]);
#else
  v = ((long)argv[0]) >> 2;
#endif

  return scheme_make_integer(v);
}

static Scheme_Object *equal_hash_code(int argc, Scheme_Object *argv[])
{
  long v;

  if (SCHEME_INTP(argv[0]))
    return argv[0];

  v = scheme_equal_hash_key(argv[0]);

  return scheme_make_integer(v);
}

Scheme_Object *scheme_make_weak_box(Scheme_Object *v)
{
#ifdef MZ_PRECISE_GC
  return (Scheme_Object *)GC_malloc_weak_box(v, NULL, 0);
#else
  Scheme_Small_Object *obj;

  obj = MALLOC_ONE_TAGGED_WEAK(Scheme_Small_Object);

  obj->so.type = scheme_weak_box_type;

  obj->u.ptr_val = v;
  scheme_weak_reference((void **)&obj->u.ptr_val);

  return (Scheme_Object *)obj;
#endif
}

static Scheme_Object *make_weak_box(int argc, Scheme_Object *argv[])
{
  return scheme_make_weak_box(argv[0]);
}

static Scheme_Object *weak_box_value(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o;

  if (!SCHEME_WEAKP(argv[0]))
    scheme_wrong_type("weak-box-value", "weak-box", 0, argc, argv);

  o = SCHEME_BOX_VAL(argv[0]);
  if (!o)
    return scheme_false;
  else
    return o;
}

static Scheme_Object *weak_boxp(int argc, Scheme_Object *argv[])
{
  return (SCHEME_WEAKP(argv[0]) ? scheme_true : scheme_false);
}

Scheme_Object * scheme_make_null (void)
{
  return scheme_null;
}
