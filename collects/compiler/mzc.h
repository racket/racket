
#include <stdlib.h>
#include <stdarg.h>

#define GLOBAL_VARREF(x) ((x)->val ? (Scheme_Object *)(x)->val : \
  (scheme_unbound_global(x), (Scheme_Object *)NULL))
#define CHECK_GLOBAL_BOUND(x) \
    if (!(x)->val) scheme_raise_exn(MZEXN_UNIT, \
				    "invoke-unit: cannot link to undefined identifier: %S", \
				    (Scheme_Object*)(x)->key);

#define DO_FUEL_POLL ((scheme_fuel_counter-- <= 0) ? (scheme_process_block(0), 0) : 0)

#define _scheme_direct_apply_primitive_multi_poll(prim, argc, argv) \
    (DO_FUEL_POLL, _scheme_direct_apply_primitive_multi(prim, argc, argv))
#define _scheme_direct_apply_primitive_poll(prim, argc, argv) \
    (DO_FUEL_POLL, _scheme_direct_apply_primitive(prim, argc, argv))
#define _scheme_direct_apply_closed_primitive_multi_poll(prim, argc, argv) \
    (DO_FUEL_POLL, _scheme_direct_apply_closed_primitive_multi(prim, argc, argv))
#define _scheme_direct_apply_closed_primitive_poll(prim, argc, argv) \
    (DO_FUEL_POLL, _scheme_direct_apply_closed_primitive(prim, argc, argv))

#ifdef KEEP_CLOSURE_COUNT
static int closure_alloc_cnt;
static void print_closures()
{
  printf("closures allocated in " MZC_SRC_FILE ": %d\n", closure_alloc_cnt);
}
# define CLOSURE_ALLOC_PP closure_alloc_inc(), 
static void closure_alloc_inc()
{
  if (!closure_alloc_cnt)
    atexit(print_closures);
  closure_alloc_cnt++;
}
#else
# define CLOSURE_ALLOC_PP /**/
#endif

#define _scheme_make_c_proc_closure(cfunc, rec, name, amin, amax, flags) \
  (CLOSURE_ALLOC_PP (Scheme_Object *)_scheme_fill_prim_closure(&rec->prim, cfunc, &rec->data, name, amin, amax, flags))

#define _scheme_make_c_proc_closure_empty(cfunc, rec, name, amin, amax, flags) \
  (CLOSURE_ALLOC_PP (Scheme_Object *)_scheme_fill_prim_closure(&rec->prim, cfunc, NULL, name, amin, amax, flags))

#define _scheme_make_c_case_proc_closure(cfunc, rec, name, ccnt, cses, flags) \
  (CLOSURE_ALLOC_PP (Scheme_Object *)_scheme_fill_prim_case_closure(&rec->prim, cfunc, &rec->data, name, ccnt, cses, flags))

#define _scheme_make_c_case_proc_closure_empty(cfunc, rec, name, ccnt, cses, flags) \
  (CLOSURE_ALLOC_PP (Scheme_Object *)_scheme_fill_prim_case_closure(&rec->prim, cfunc, NULL, name, ccnt, cses, flags))

#define NO_MULTIPLE_VALUES(res) \
	if (res == SCHEME_MULTIPLE_VALUES) \
	    scheme_wrong_return_arity(NULL, 1, scheme_multiple_count, scheme_multiple_array, NULL);
#define CHECK_MULTIPLE_VALUES(res, expected) \
	if (res != SCHEME_MULTIPLE_VALUES || scheme_multiple_count != expected) \
        scheme_wrong_return_arity(NULL, expected, \
                                  (res == SCHEME_MULTIPLE_VALUES ? scheme_multiple_count : 1), \
								  (res == SCHEME_MULTIPLE_VALUES ? scheme_multiple_array : (Scheme_Object**)res), \
                                  NULL);

#define SCHEME_DETATCH_MV_BUFFER(mv, pr) if (SAME_OBJ(mv, pr->values_buffer)) pr->values_buffer = NULL

#define SCHEME_CURRENT_ENV(pr) scheme_get_env(NULL)

typedef struct {
  Scheme_Object * val;
  Scheme_Object ** array;
  int count;
} _Scheme_Begin0_Rec;

typedef struct {
  Scheme_Cont_Frame_Data cf;
  Scheme_Object *val;
} _Scheme_WCM_Rec;

#define _scheme_apply_ckp(f, argc, argv) (SCHEME_CLSD_PRIMP(f) ? _scheme_apply_closed_prim(f, argc, argv) : _scheme_apply(f, argc, argv))
#define _scheme_apply_multi_ckp(f, argc, argv) (SCHEME_CLSD_PRIMP(f) ? _scheme_apply_closed_prim_multi(f, argc, argv) : _scheme_apply_multi(f, argc, argv))

#define MZC_EQP(ltp, av, bv) (SAME_OBJ(av, bv))
#define MZC_EQVP(ltp, av, bv) (SAME_OBJ(av, bv) || scheme_eqv(av, bv))
#define MZC_EQUALP(ltp, av, bv) scheme_equal(av, bv)
#define MZC_NOTP(p, av) (SCHEME_FALSEP(av))
#define MZC_NULLP(p, av) (SCHEME_NULLP(av))
#define MZC_PAIRP(p, av) (SCHEME_PAIRP(av))
#define MZC_SYMBOLP(p, av) (SCHEME_SYMBOLP(av))
#define MZC_STRINGP(p, av) (SCHEME_CHAR_STRINGP(av))
#define MZC_BYTESP(p, av) (SCHEME_BYTE_STRINGP(av))
#define MZC_VECTORP(p, av) (SCHEME_VECTORP(av))
#define MZC_NUMBERP(p, av) (SCHEME_NUMBERP(av))
#define MZC_PROCEDUREP(p, av) (SCHEME_PROCP(av))
#define MZC_EOFP(p, av) (SCHEME_EOFP(av))
#define MZC_CHARP(p, av) (SCHEME_CHARP(av))

#define MZC_CONS(p, av, bv) scheme_make_pair(av, bv)
#define MZC_LIST1(p, av) scheme_make_pair(av, scheme_null)
#define MZC_LIST2(p, av, bv) scheme_make_pair(av, scheme_make_pair(bv, scheme_null))
#define MZC_APPEND(p, av, bv) scheme_append(av, bv)

#define MZC_FOR_SYNTAX_IN_ENV(ignored, proc) scheme_apply_for_syntax_in_env(proc, env)

#if MZC_UNSAFE
/* Unsafe versions */
#define MZC_CAR(p, av) SCHEME_CAR(av)
#define MZC_CDR(p, av) SCHEME_CDR(av)
#define MZC_CADR(p, av) SCHEME_CAR(SCHEME_CDR(av))
#define MZC_CDDR(p, av) SCHEME_CDR(SCHEME_CDR(av))
#define MZC_CDAR(p, av) SCHEME_CDR(SCHEME_CAR(av))
#define MZC_CAAR(p, av) SCHEME_CAR(SCHEME_CAR(av))
#define MZC_CADDR(p, av) SCHEME_CADR(SCHEME_CDR(av))
#define MZC_SET_CAR(p, av, bv) (SCHEME_CAR(av)=bv, scheme_void)
#define MZC_SET_CDR(p, av, bv) (SCHEME_CDR(av)=bv, scheme_void)

# define MZC_VECTOR_REF(p, v, i) SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)]
# define MZC_VECTOR_SET(p, v, i, x) (SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)] = x, scheme_void)

# define MZC_STRING_REF(p, v, i) scheme_make_character(SCHEME_CHAR_STR_VAL(v)[SCHEME_INT_VAL(i)])
# define MZC_STRING_SET(p, v, i, x) (SCHEME_CHAR_STR_VAL(v)[SCHEME_INT_VAL(i)] = SCHEME_CHAR_VAL(x), scheme_void)

# define MZC_BYTES_REF(p, v, i) scheme_make_integer(SCHEME_BYTE_STR_VAL(v)[SCHEME_INT_VAL(i)])
# define MZC_BYTES_SET(p, v, i, x) (SCHEME_BYTE_STR_VAL(v)[SCHEME_INT_VAL(i)] = SCHEME_INT_VAL(x), scheme_void)

#define MZC_CHAR_TO_INTEGER(p, v) scheme_make_integer((unsigned char)SCHEME_CHAR_VAL(v))
/* End unsafe versions */
#else
/* Safe versions */
#define MZC_CAR(p, av) (SCHEME_PAIRP(av) ? SCHEME_CAR(av) : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_CDR(p, av) (SCHEME_PAIRP(av) ? SCHEME_CDR(av) : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_CADR(p, av) ((SCHEME_PAIRP(av) && SCHEME_PAIRP(SCHEME_CDR(av))) ? SCHEME_CAR(SCHEME_CDR(av)) : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_CDDR(p, av) ((SCHEME_PAIRP(av) && SCHEME_PAIRP(SCHEME_CDR(av))) ? SCHEME_CDR(SCHEME_CDR(av)) : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_CDAR(p, av) ((SCHEME_PAIRP(av) && SCHEME_PAIRP(SCHEME_CAR(av))) ? SCHEME_CDR(SCHEME_CAR(av)) : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_CAAR(p, av) ((SCHEME_PAIRP(av) && SCHEME_PAIRP(SCHEME_CAR(av))) ? SCHEME_CAR(SCHEME_CAR(av)) : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_CADDR(p, av) ((SCHEME_PAIRP(av) && SCHEME_PAIRP(SCHEME_CDR(av)) && SCHEME_PAIRP(SCHEME_CDR(SCHEME_CDR(av)))) ? SCHEME_CADR(SCHEME_CDR(av)) : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_SET_CAR(p, av, bv) (SCHEME_MUTABLE_PAIRP(av) ? (SCHEME_CAR(av)=bv, scheme_void) : (arg[0] = av, arg[1] = bv, _scheme_direct_apply_primitive_multi(p, 2, arg)))
#define MZC_SET_CDR(p, av, bv) (SCHEME_MUTABLE_PAIRP(av) ? (SCHEME_CDR(av)=bv, scheme_void) : (arg[0] = av, arg[1] = bv, _scheme_direct_apply_primitive_multi(p, 2, arg)))

#define MZC_CHAR_TO_INTEGER(p, v) (SCHEME_CHARP(v) ? scheme_make_integer((unsigned char)SCHEME_CHAR_VAL(v)) \
                                   : (arg[0] = v, _scheme_direct_apply_primitive_multi(p, 1, arg)))

# define MZC_VECTOR_REF(p, v, i) ((SCHEME_INTP(i) && SCHEME_VECTORP(v) && (SCHEME_INT_VAL(i) >= 0) \
                                  && (SCHEME_INT_VAL(i) < SCHEME_VEC_SIZE(v)) \
                                 ? SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)] \
				  : (arg[0] = v, arg[1] = i, _scheme_direct_apply_primitive_multi(p, 2, arg))))
# define MZC_VECTOR_SET(p, v, i, x) ((SCHEME_INTP(i) && SCHEME_VECTORP(v) && (SCHEME_INT_VAL(i) >= 0) \
                                    && (SCHEME_INT_VAL(i) < SCHEME_VEC_SIZE(v)) \
                                    ? (SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)] = x, scheme_void) \
				    : (arg[0] = v, arg[1] = i, arg[2] = x, _scheme_direct_apply_primitive_multi(p, 3, arg))))
# define MZC_STRING_REF(p, v, i) ((SCHEME_INTP(i) && SCHEME_CHAR_STRINGP(v) && (SCHEME_INT_VAL(i) >= 0) \
                                  && (SCHEME_INT_VAL(i) < SCHEME_CHAR_STRLEN_VAL(v)) \
                                  ? scheme_make_character(SCHEME_CHAR_STR_VAL(v)[SCHEME_INT_VAL(i)]) \
				  : (arg[0] = v, arg[1] = i, _scheme_direct_apply_primitive_multi(p, 2, arg))))
# define MZC_STRING_SET(p, v, i, x) ((SCHEME_INTP(i) && SCHEME_MUTABLE_CHAR_STRINGP(v) && SCHEME_CHARP(x) && (SCHEME_INT_VAL(i) >= 0) \
                                      && (SCHEME_INT_VAL(i) < SCHEME_CHAR_STRLEN_VAL(v)) \
                                     ? (SCHEME_CHAR_STR_VAL(v)[SCHEME_INT_VAL(i)] = SCHEME_CHAR_VAL(x), scheme_void) \
				     : (arg[0] = v, arg[1] = i, arg[2] = x, _scheme_direct_apply_primitive_multi(p, 3, arg))))
# define MZC_BYTES_REF(p, v, i) ((SCHEME_INTP(i) && SCHEME_BYTE_STRINGP(v) && (SCHEME_INT_VAL(i) >= 0) \
                                  && (SCHEME_INT_VAL(i) < SCHEME_BYTE_STRLEN_VAL(v)) \
                                  ? scheme_make_integer(SCHEME_BYTE_STR_VAL(v)[SCHEME_INT_VAL(i)]) \
				  : (arg[0] = v, arg[1] = i, _scheme_direct_apply_primitive_multi(p, 2, arg))))
# define MZC_BYTES_SET(p, v, i, x) ((SCHEME_INTP(i) && SCHEME_MUTABLE_BYTE_STRINGP(v) && SCHEME_INTP(x) \
                                      && (SCHEME_INT_VAL(x) >= 0) && (SCHEME_INT_VAL(x) <= 255) \
                                      && (SCHEME_INT_VAL(i) >= 0) && (SCHEME_INT_VAL(i) < SCHEME_BYTE_STRLEN_VAL(v)) \
                                     ? (SCHEME_BYTE_STR_VAL(v)[SCHEME_INT_VAL(i)] = SCHEME_INT_VAL(x), scheme_void) \
				     : (arg[0] = v, arg[1] = i, arg[2] = x, _scheme_direct_apply_primitive_multi(p, 3, arg))))
/* End safe versions */
#endif

#define _MZC_DBLP(obj) SAME_TYPE(_SCHEME_TYPE(obj), scheme_double_type)
     
#define MZC_ZEROP(zp, av) (SCHEME_INTP(av) \
                                ? (av == scheme_make_integer(0)) \
                                : (_MZC_DBLP(av) \
                                   ? !SCHEME_DBL_VAL(av) \
                                   : (arg[0] = av, SCHEME_TRUEP(_scheme_direct_apply_primitive_multi(zp, 1, arg)))))

#define MZC_ARITH_COMPARE(cp, av, bv, compareop) \
                                     ((SCHEME_INTP(av) && SCHEME_INTP(bv)) \
                                      ? (SCHEME_INT_VAL(av) compareop SCHEME_INT_VAL(bv)) \
                                      : ((SCHEME_DBLP(av) && SCHEME_DBLP(bv)) \
                                         ? (SCHEME_DBL_VAL(av) compareop SCHEME_DBL_VAL(bv)) \
                                         : (arg[0] = av, arg[1] = bv, SCHEME_TRUEP(_scheme_direct_apply_primitive_multi(cp, 2, arg)))))

#define MZC_LTP(cp, av, bv) MZC_ARITH_COMPARE(cp, av, bv, <)
#define MZC_GTP(cp, av, bv) MZC_ARITH_COMPARE(cp, av, bv, >)
#define MZC_LTEP(cp, av, bv) MZC_ARITH_COMPARE(cp, av, bv, <=)
#define MZC_GTEP(cp, av, bv) MZC_ARITH_COMPARE(cp, av, bv, >=)
#define MZC_EQLP(cp, av, bv) MZC_ARITH_COMPARE(cp, av, bv, ==)

#if MZC_FIXNUM
/* Numerically incorrect */
#define MZC_ADD1(p, av) (SCHEME_INTP(av) \
                         ? scheme_make_integer(SCHEME_INT_VAL(av)+1) \
                         : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_SUB1(p, av) (SCHEME_INTP(av) \
                         ? scheme_make_integer(SCHEME_INT_VAL(av)-1) \
                         : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))

#define MZC_ARITH_OP(cp, av, bv, op, revop) \
                      ((SCHEME_INTP(av) && SCHEME_INTP(bv)) \
                        ? scheme_make_integer(SCHEME_INT_VAL(av) op SCHEME_INT_VAL(bv)) \
                        : ((SCHEME_DBLP(av) && SCHEME_DBLP(bv)) \
                           ? scheme_make_double(SCHEME_DBL_VAL(av) op SCHEME_DBL_VAL(bv)) \
                           : (arg[0] = av, arg[1] = bv, _scheme_direct_apply_primitive_multi(cp, 2, arg))))

#define MZC_TIMES2(cp, av, bv) MZC_ARITH_OP(cp, av, bv, *, /)

/* End numerically incorrect */
#else
/* Numerically correct */
#define MZC_ADD1(p, av) ((SCHEME_INTP(av) && (SCHEME_INT_VAL(av) < 0x3FFFFFFF)) \
                         ? scheme_make_integer(SCHEME_INT_VAL(av)+1) \
                         : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_SUB1(p, av) ((SCHEME_INTP(av) && (SCHEME_INT_VAL(av) > (-0x3FFFFFFF))) \
                         ? scheme_make_integer(SCHEME_INT_VAL(av)-1) \
                         : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))

#define MZC_ARITH_OP(cp, av, bv, op, revop) \
                      ((SCHEME_INTP(av) && SCHEME_INTP(bv) \
                        && (((SCHEME_INT_VAL(scheme_make_integer(SCHEME_INT_VAL(av) op SCHEME_INT_VAL(bv))) \
                              revop SCHEME_INT_VAL(bv)) \
                             == SCHEME_INT_VAL(av)))) \
                        ? scheme_make_integer(SCHEME_INT_VAL(av) op SCHEME_INT_VAL(bv)) \
                        : ((SCHEME_DBLP(av) && SCHEME_DBLP(bv)) \
                           ? scheme_make_double(SCHEME_DBL_VAL(av) op SCHEME_DBL_VAL(bv)) \
                           : (arg[0] = av, arg[1] = bv, _scheme_direct_apply_primitive_multi(cp, 2, arg))))
/* End numerically correct */
#endif

#define MZC_PLUS2(cp, av, bv) MZC_ARITH_OP(cp, av, bv, +, -)
#define MZC_MINUS2(cp, av, bv) MZC_ARITH_OP(cp, av, bv, -, +)

#define MZC_MAXMIN_OP(cp, av, bv, minlt) \
            ((SCHEME_INTP(av) && SCHEME_INTP(bv)) \
             ? ((SCHEME_INT_VAL(av) minlt SCHEME_INT_VAL(bv)) ? av : bv) \
             : (arg[0] = av, arg[1] = bv, _scheme_direct_apply_primitive_multi(cp, 2, arg)))

#define MZC_MAX2(cp, av, bv) MZC_MAXMIN_OP(cp, av, bv, >)
#define MZC_MIN2(cp, av, bv) MZC_MAXMIN_OP(cp, av, bv, <)

#define MZC_QUOTIENT(cp, av, bv) \
            ((SCHEME_INTP(av) && SCHEME_INTP(bv) && SCHEME_INT_VAL(bv)) \
             ? scheme_make_integer(SCHEME_INT_VAL(av) / SCHEME_INT_VAL(bv)) \
             : (arg[0] = av, arg[1] = bv, _scheme_direct_apply_primitive_multi(cp, 2, arg)))

static MSC_IZE(inline) Scheme_Object *mzc_force_value(Scheme_Object *v)
{
  return _scheme_force_value(v);
}

#define _scheme_direct_apply_closed_primitive_multi_fv(prim, argc, argv) \
    mzc_force_value(_scheme_direct_apply_closed_primitive_multi(prim, argc, argv))
#define _scheme_direct_apply_closed_primitive_fv(prim, argc, argv) \
    scheme_check_one_value(_scheme_direct_apply_closed_primitive_multi_fv(prim, argc, argv))

static int mzc_strlen(const char *c) {
  int l;
  for (l = 0; c[l]; l++);
  return l;
}

#if 0
static Scheme_Object *DEBUG_CHECK(Scheme_Object *v)
{
  if ((SCHEME_TYPE(v) < _scheme_values_types_) || (SCHEME_TYPE(v) > _scheme_last_type_ + 5)) {
    /* Could be a boxed value ... */
    Scheme_Object *o = *(Scheme_Object **)v;
    if ((SCHEME_TYPE(v) < _scheme_values_types_) || (SCHEME_TYPE(v) > _scheme_last_type_ + 5)) {
      printf("wrong!\n");
    }
  }
  return v;
}
#endif
