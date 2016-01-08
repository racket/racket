/*
  Racket
  Copyright (c) 2004-2016 PLT Design Inc.
  Copyright (c) 1995 Matthew Flatt

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

#define NEED_NUMBER(name) \
  scheme_wrong_contract(#name, "number?", 0, argc, argv)
#define NEED_REAL(name) \
  scheme_wrong_contract(#name, "real?", 0, argc, argv)
#define NEED_INTEGER(name) \
  scheme_wrong_contract(#name, "integer", 0, argc, argv)

#define rat_from_float(d, sr) force_rat(scheme_rational_from_float(d), sr)
#define rat_from_double(d, sr) force_rat(scheme_rational_from_double(d), sr)

#ifdef MZ_USE_SINGLE_FLOATS
# define FLOATWRAP(x) x
# ifdef USE_SINGLE_FLOATS_AS_DEFAULT
#  define D_FLOATWRAP(x) /* empty */
#  define S_FLOATWRAP(x) x
# else
#  define D_FLOATWRAP(x) x
#  define S_FLOATWRAP(x) /* empty */
# endif
#else
# define FLOATWRAP(x) /* empty */
# define D_FLOATWRAP(x) x
# define S_FLOATWRAP(x) /* empty */
#endif

#define GEN_BIN_COMP_PROT(name) \
static int name (Scheme_Object *n1, Scheme_Object *n2)

#define GEN_NARY_COMP(name, scheme_name, bin_name, TYPEP, type) \
static Scheme_Object *name (int argc, Scheme_Object *argv[]); \
static MZ_INLINE Scheme_Object * \
name ## __slow (Scheme_Object *p, int argc, Scheme_Object *argv[]) \
{ \
  Scheme_Object *o; \
  int i; \
  for (i = 1; i < argc; i++) {\
    o = argv[i]; \
    if (!TYPEP(o)) { \
      scheme_wrong_contract(scheme_name, type, i, argc, argv); \
      return NULL; \
    } \
    if (!bin_name(p, o)) { \
        for (i++; i < argc; i++) { \
          if (!TYPEP(argv[i])) \
           scheme_wrong_contract(scheme_name, type, i, argc, argv); \
        } \
        return scheme_false; \
    } \
    p = o; \
  } \
  return scheme_true; \
} \
static MZ_INLINE Scheme_Object *name ## __bin(Scheme_Object *a, Scheme_Object *b) { \
  return (bin_name(a, b) ? scheme_true : scheme_false); \
} \
Scheme_Object * \
name (int argc, Scheme_Object *argv[]) \
{ \
  Scheme_Object *p, *p2; \
  p = argv[0]; \
  if (!TYPEP(p)) \
   scheme_wrong_contract(scheme_name, type, 0, argc, argv); \
  if (argc == 2) { \
    p2 = argv[1]; \
    if (!TYPEP(p2)) \
      scheme_wrong_contract(scheme_name, type, 1, argc, argv); \
    return name ## __bin(p, p2); \
  } else \
    return name ## __slow(p, argc, argv); \
}

#define GEN_BIN_PROT(name) \
static Scheme_Object *name (const Scheme_Object *n1, const Scheme_Object *n2)

#define cx_NO_CHECK(n1, n2) /* empty */

/* This macro is used to implement most all binary math and comparison functions (!): */
#define GEN_BIN_THING(rettype, name, scheme_name, \
                      iop, fop, fsop, bn_op, rop, cxop, \
                      wrap, combineinf, \
                      waybigf, swaybigf, waysmallf, swaysmallf, \
                      waybigs, swaybigs, waysmalls, swaysmalls, \
                      combinezero, firstzero, sfirstzero, secondzero, ssecondzero, \
                      nanchk, snanchk, nanchk_more, snanchk_more, \
                      complexwrap, noniziwrap, exactzerowrapl, exactzerowrapr, numbertype,\
                      toi_or_toe, \
                      check_exact_zero1, check_exact_one1, check_exact_zero2, check_exact_one2) \
rettype name (const Scheme_Object *n1, const Scheme_Object *n2); \
static rettype name ## __wrong_contract(const Scheme_Object *v) \
{ \
  Scheme_Object *a[1]; \
  a[0] = (Scheme_Object *)v; \
  scheme_wrong_contract(scheme_name, numbertype, -1, 0, a); \
  return 0; \
} \
static MZ_INLINE rettype name ## __int_big(const Scheme_Object *n1, const Scheme_Object *n2) { \
        Small_Bignum sb; \
        check_exact_zero1(n1, n2); \
        check_exact_one1(n1, n2); \
        return bn_op((scheme_make_small_bignum(SCHEME_INT_VAL(n1), \
					       &sb)), \
		     (n2)); \
} \
static MZ_INLINE rettype name ## __int_rat(const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Rational sr1; \
  check_exact_zero1(n1, n2); \
  check_exact_one1(n1, n2); \
  return rop((scheme_make_small_rational(SCHEME_INT_VAL(n1), \
					 &sr1)), \
	     (n2)); \
} \
complexwrap( \
static MZ_INLINE rettype name ## __int_comp(const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Complex sc; \
  check_exact_zero1(n1, n2); \
  check_exact_one1(n1, n2); \
  return cxop((scheme_make_small_complex(n1, &sc)), \
              (n2)); \
}) \
FLOATWRAP( \
static MZ_INLINE rettype name ## __flt_big(float d1, const Scheme_Object *n1, const Scheme_Object *n2) { \
  toi_or_toe(float tmp2, Small_Rational sr2); \
  snanchk_more(d1); \
  wrap(if (MZ_IS_POS_INFINITY(d1)) return combineinf(swaybigf, n2);) \
  wrap(if (MZ_IS_NEG_INFINITY(d1)) return combineinf(swaysmallf, n2);) \
  toi_or_toe(tmp2=scheme_bignum_to_float(n2),); \
  return toi_or_toe(fsop(d1, tmp2), \
                    rop(rat_from_float(d1, &sr2), scheme_integer_to_rational(n2))); \
}) \
FLOATWRAP( \
static MZ_INLINE rettype name ## __flt_rat(float d1, const Scheme_Object *n1, const Scheme_Object *n2) { \
  toi_or_toe(float tmp3, Small_Rational sr3); \
  snanchk_more(d1); \
  wrap(if (MZ_IS_POS_INFINITY(d1)) return combineinf(swaybigf, n2);) \
  wrap(if (MZ_IS_NEG_INFINITY(d1)) return combineinf(swaysmallf, n2);) \
  wrap(if (d1 == 0.0) return combinezero(sfirstzero, n2, d1);) \
  toi_or_toe(tmp3=scheme_rational_to_float(n2),); \
  return toi_or_toe(fsop(d1, tmp3), \
                    rop(rat_from_float(d1, &sr3), (n2))); \
})\
FLOATWRAP(complexwrap(  \
static MZ_INLINE rettype name ## __flt_comp(float d1, const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Complex sc; \
  snanchk_more(d1); \
  return cxop((scheme_make_small_complex(n1, &sc)), \
  (n2)); \
}))    \
static MZ_INLINE rettype name ## __dbl_big(double d1, const Scheme_Object *n1, const Scheme_Object *n2) { \
  toi_or_toe(double tmp4, Small_Rational sr4); \
  nanchk_more(d1); \
  wrap(if (MZ_IS_POS_INFINITY(d1)) return combineinf(waybigf, n2);) \
  wrap(if (MZ_IS_NEG_INFINITY(d1)) return combineinf(waysmallf, n2);) \
  toi_or_toe(tmp4=scheme_bignum_to_double(n2),); \
  return toi_or_toe(fop(d1, tmp4), \
                    rop(rat_from_double(d1, &sr4), scheme_integer_to_rational(n2))); \
} \
static MZ_INLINE rettype name ## __dbl_rat(double d1, const Scheme_Object *n1, const Scheme_Object *n2) { \
  toi_or_toe(double tmp5, Small_Rational sr5);       \
  nanchk_more(d1); \
  wrap(if (MZ_IS_POS_INFINITY(d1)) return combineinf(waybigf, n2);) \
  wrap(if (MZ_IS_NEG_INFINITY(d1)) return combineinf(waysmallf, n2);) \
  wrap(if (d1 == 0.0) return combinezero(firstzero, n2, d1);) \
  toi_or_toe(tmp5=scheme_rational_to_double(n2),); \
  return toi_or_toe(fop(d1, tmp5), \
                    rop(rat_from_double(d1, &sr5), (n2))); \
} \
complexwrap( \
static MZ_INLINE rettype name ## __dbl_comp(double d1, const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Complex sc; \
  nanchk_more(d1); \
  return cxop((scheme_make_small_complex(n1, &sc)), \
	      (n2)); \
}) \
static MZ_INLINE rettype name ## __big_int(const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Bignum sb; \
  check_exact_zero2(n2, n1); \
  check_exact_one2(n2, n1); \
  return bn_op((n1), (scheme_make_small_bignum(SCHEME_INT_VAL(n2), &sb))); \
} \
FLOATWRAP( \
static MZ_INLINE rettype name ## __big_flt(const Scheme_Object *n1, const Scheme_Object *n2) { \
  float d2; \
  toi_or_toe(float tmp6, Small_Rational sr6); \
  d2 = SCHEME_FLT_VAL(n2); \
  snanchk_more(d2); \
  wrap(if (MZ_IS_POS_INFINITY(d2)) return combineinf(swaysmalls, n1);) \
  wrap(if (MZ_IS_NEG_INFINITY(d2)) return combineinf(swaybigs, n1);) \
  toi_or_toe(tmp6=scheme_bignum_to_float(n1),); \
  return toi_or_toe(fsop(tmp6, d2), \
                    rop(scheme_integer_to_rational(n1), rat_from_float(d2, &sr6))); \
}) \
static MZ_INLINE rettype name ## __big_dbl(const Scheme_Object *n1, const Scheme_Object *n2) { \
  double d2; \
  toi_or_toe(double tmp7, Small_Rational sr7); \
  d2 = SCHEME_DBL_VAL(n2); \
  nanchk_more(d2); \
  wrap(if (MZ_IS_POS_INFINITY(d2)) return combineinf(waysmalls, n1);) \
  wrap(if (MZ_IS_NEG_INFINITY(d2)) return combineinf(waybigs, n1);) \
  toi_or_toe(tmp7=scheme_bignum_to_double(n1),); \
  return toi_or_toe(fop(tmp7, d2), \
                    rop(scheme_integer_to_rational(n1), rat_from_double(d2, &sr7))); \
} \
static MZ_INLINE rettype name ## __big_rat(const Scheme_Object *n1, const Scheme_Object *n2) { \
  return rop(scheme_integer_to_rational(n1), (n2)); \
} \
complexwrap(\
static MZ_INLINE rettype name ## __big_comp(const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Complex sc; \
  return cxop((scheme_make_small_complex(n1, &sc)), (n2)); \
} \
) \
static MZ_INLINE rettype name ## __rat_int(const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Rational sr8; \
  check_exact_zero2(n2, n1); \
  check_exact_one2(n2, n1); \
  return rop((n1), \
             (scheme_make_small_rational(SCHEME_INT_VAL(n2), \
                                         &sr8))); \
} \
FLOATWRAP( \
static MZ_INLINE rettype name ## __rat_flt(const Scheme_Object *n1, const Scheme_Object *n2) { \
  float d2; \
  toi_or_toe(float tmp9, Small_Rational sr9); \
  d2 = SCHEME_FLT_VAL(n2); \
  snanchk_more(d2); \
  wrap(if (MZ_IS_POS_INFINITY(d2)) return combineinf(swaysmalls, n1);) \
  wrap(if (MZ_IS_NEG_INFINITY(d2)) return combineinf(swaybigs, n1);) \
  wrap(if (d2 == 0.0) return combinezero(ssecondzero, n1, d2);) \
  toi_or_toe(tmp9=scheme_rational_to_float(n1),); \
  return toi_or_toe(fsop(tmp9, d2), \
                    rop((n1), rat_from_float(d2, &sr9))); \
}) \
static MZ_INLINE rettype name ## __rat_dbl(const Scheme_Object *n1, const Scheme_Object *n2) { \
  double d2; \
  toi_or_toe(double tmp10, Small_Rational sr10);      \
  d2 = SCHEME_DBL_VAL(n2); \
  nanchk_more(d2); \
  wrap(if (MZ_IS_POS_INFINITY(d2)) return combineinf(waysmalls, n1);) \
  wrap(if (MZ_IS_NEG_INFINITY(d2)) return combineinf(waybigs, n1);) \
  wrap(if (d2 == 0.0) return combinezero(secondzero, n1, d2);) \
  toi_or_toe(tmp10=scheme_rational_to_double(n1),); \
  return toi_or_toe(fop(tmp10, d2), \
                    rop((n1), rat_from_double(d2, &sr10))); \
} \
static MZ_INLINE rettype name ## __rat_big(const Scheme_Object *n1, const Scheme_Object *n2) { \
  return rop((n1), scheme_integer_to_rational(n2)); \
} \
complexwrap( \
static MZ_INLINE rettype name ## __rat_comp(const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Complex sc; \
  return cxop((scheme_make_small_complex(n1, &sc)), (n2)); \
}) \
complexwrap( \
static MZ_INLINE rettype name ## __comp_int(const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Complex sc; \
  check_exact_zero2(n2, n1); \
  check_exact_one2(n2, n1); \
  return cxop((n1), (scheme_make_small_complex(n2, &sc))); \
}) \
FLOATWRAP(complexwrap( \
static MZ_INLINE rettype name ## __comp_flt(const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Complex sc; \
  snanchk_more(SCHEME_FLT_VAL(n2)); \
  return cxop((n1), (scheme_make_small_complex(n2, &sc))); \
}))                                                  \
complexwrap( \
static MZ_INLINE rettype name ## __comp_dbl(const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Complex sc; \
  nanchk_more(SCHEME_DBL_VAL(n2)); \
  return cxop((n1), (scheme_make_small_complex(n2, &sc))); \
}) \
complexwrap( \
static MZ_INLINE rettype name ## __comp_big(const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Complex sc; \
  return cxop((n1), (scheme_make_small_complex(n2, &sc))); \
}) \
complexwrap( \
static MZ_INLINE rettype name ## __comp_rat(const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Complex sc; \
  return cxop((n1), (scheme_make_small_complex(n2, &sc))); \
}) \
rettype \
name (const Scheme_Object *n1, const Scheme_Object *n2) \
{ \
  Scheme_Type t1, t2; \
  exactzerowrapr( if (n2 == zeroi) ) \
  if (SCHEME_INTP(n1)) \
    { \
      exactzerowrapl( if (n1 == zeroi) ) \
      if (SCHEME_INTP(n2)) \
	return iop(SCHEME_INT_VAL(n1), SCHEME_INT_VAL(n2)); \
      t2 = _SCHEME_TYPE(n2); \
      FLOATWRAP( \
      if (t2 == scheme_float_type) { \
        float d = SCHEME_FLT_VAL(n2); \
        snanchk(d); \
        return fsop(SCHEME_INT_VAL(n1), d); \
      } \
      ) \
      if (t2 == scheme_double_type) { \
        double d = SCHEME_DBL_VAL(n2); \
        nanchk(d); \
        return fop(SCHEME_INT_VAL(n1), d); \
      } \
      if (t2 == scheme_bignum_type) { \
        return name ## __int_big(n1, n2); \
       } \
       if (t2 == scheme_rational_type) { \
        return name ## __int_rat(n1, n2); \
       } \
      complexwrap( \
      if (noniziwrap((t2 == scheme_complex_type))) {        \
        return name ## __int_comp(n1, n2); \
      } \
      ) \
      return name ## __wrong_contract(n2); \
    } \
  else { \
   t1 = _SCHEME_TYPE(n1); \
   FLOATWRAP( \
   if (t1 == scheme_float_type) \
    { \
      float d1 = SCHEME_FLT_VAL(n1); \
      if (SCHEME_INTP(n2)) { \
        snanchk(d1); \
        return fsop(d1, SCHEME_INT_VAL(n2)); \
      } \
      t2 = _SCHEME_TYPE(n2); \
      if (t2 == scheme_float_type) { \
        float d2 = SCHEME_FLT_VAL(n2); \
        snanchk(d1); \
        snanchk(d2); \
        return fsop(d1, d2); \
      } \
      if (t2 == scheme_double_type) { \
        double d2 = SCHEME_DBL_VAL(n2); \
        nanchk(d1); \
        nanchk(d2); \
        return fop(d1, d2); \
      } \
      if (t2 == scheme_bignum_type) { \
        return name ## __flt_big(d1, n1, n2);    \
      } \
       if (t2 == scheme_rational_type) { \
        return name ## __flt_rat(d1, n1, n2);   \
      } \
      complexwrap( \
       if (noniziwrap((t2 == scheme_complex_type))) { \
        return name ## __flt_comp(d1, n1, n2);        \
      } \
      )\
      return name ## __wrong_contract(n2); \
    } else \
   ) \
   if (t1 == scheme_double_type) \
    { \
      double d1 = SCHEME_DBL_VAL(n1); \
      if (SCHEME_INTP(n2)) { \
        nanchk(d1); \
        return fop(d1, SCHEME_INT_VAL(n2)); \
      } \
      t2 = _SCHEME_TYPE(n2); \
      FLOATWRAP( \
      if (t2 == scheme_float_type) { \
        double d2 = SCHEME_FLT_VAL(n2); \
        nanchk(d1); \
        nanchk(d2); \
        return fop(d1, d2); \
      } \
      ) \
      if (t2 == scheme_double_type) { \
        double d2 = SCHEME_DBL_VAL(n2); \
        nanchk(d1); \
        nanchk(d2); \
        return fop(d1, d2); \
      } \
      if (t2 == scheme_bignum_type) { \
        return name ## __dbl_big(d1, n1, n2);    \
      } \
       if (t2 == scheme_rational_type) { \
         return name ## __dbl_rat(d1, n1, n2);   \
      } \
      complexwrap( \
      if (noniziwrap((t2 == scheme_complex_type))) { \
        return name ## __dbl_comp(d1, n1, n2); \
      } \
      )\
      return name ## __wrong_contract(n2); \
    } \
  else if (t1 == scheme_bignum_type) \
    { \
      if (SCHEME_INTP(n2)) { \
        return name ## __big_int(n1, n2); \
      } \
      t2 = _SCHEME_TYPE(n2); \
      FLOATWRAP( \
      if (t2 == scheme_float_type) { \
        return name ## __big_flt(n1, n2); \
      } \
      ) \
      if (t2 == scheme_double_type) { \
        return name ## __big_dbl(n1, n2); \
       } \
       if (t2 == scheme_bignum_type) \
         return bn_op((n1), (n2)); \
       if (t2 == scheme_rational_type) \
	 return name ## __big_rat(n1, n2); \
       complexwrap( \
       if (noniziwrap((t2 == scheme_complex_type))) { \
	 return name ## __big_comp(n1, n2); \
       } \
       )\
       return name ## __wrong_contract(n2); \
    } \
  else if (t1 == scheme_rational_type) \
    { \
      if (SCHEME_INTP(n2)) { \
         return name ## __rat_int(n1, n2); \
      } \
      t2 = _SCHEME_TYPE(n2); \
      FLOATWRAP( \
      if (t2 == scheme_float_type) { \
         return name ## __rat_flt(n1, n2); \
       } \
       ) \
       if (t2 == scheme_double_type) { \
         return name ## __rat_dbl(n1, n2); \
       } \
       if (t2 == scheme_bignum_type) \
         return name ## __rat_big(n1, n2); \
       if (t2 == scheme_rational_type) \
	 return rop((n1), (n2)); \
       complexwrap( \
       if (noniziwrap((t2 == scheme_complex_type))) { \
         return name ## __rat_comp(n1, n2); \
       } \
       )\
       return name ## __wrong_contract(n2); \
    } \
  complexwrap( \
  else if (noniziwrap((t1 == scheme_complex_type))) \
    { \
       if (SCHEME_INTP(n2)) \
         return name ## __comp_int(n1, n2); \
       t2 = _SCHEME_TYPE(n2); \
       FLOATWRAP( \
       if (t2 == scheme_float_type) { \
         return name ## __comp_flt(n1, n2); \
       } \
       ) \
       if (t2 == scheme_double_type) { \
         return name ## __comp_dbl(n1, n2); \
       } \
       if (t2 == scheme_bignum_type) \
         return name ## __comp_big(n1, n2); \
       if (t2 == scheme_rational_type) \
         return name ## __comp_rat(n1, n2); \
       if (noniziwrap((t2 == scheme_complex_type))) \
	 return cxop((n1), (n2)); \
       return name ## __wrong_contract(n2); \
    } \
  ) \
  else \
       return name ## __wrong_contract(n1); \
  } \
}

#ifdef NAN_EQUALS_ANYTHING
# define GR_NAN_CHK(n2) MZ_IS_NAN(SCHEME_FLOAT_VAL(n2))
#else
# define GR_NAN_CHK(n2) 0
#endif

#define GEN_IDENT(x) x
#define GEN_OMIT(x) 
#define GEN_FIRST_ONLY(x, y) x
#define GEN_APPLY(x, y) x(y)
#define GEN_APPLY3(x, y, z) x(y, z)
#define GEN_SCHEME_BOOL_APPLY(x, y, z) x(y)
#define GEN_TOI(x, y) x
#define GEN_TOE(x, y) y

#define GEN_RETURN_0(x) x return zeroi;
#define GEN_RETURN_0_USUALLY(x) x if (!SCHEME_FLOATP(n2) || GR_NAN_CHK(n2) || (SCHEME_FLOAT_VAL(n2) != 0)) return zeroi;
#define GEN_RETURN_1(x) x return scheme_make_integer(1);
#define GEN_RETURN_N1(x) x return (Scheme_Object *)n1;
#define GEN_RETURN_N2(x) x return (Scheme_Object *)n2;
#define GEN_SINGLE_SUBTRACT_N2(x) x if SCHEME_FLOATP(n2) return unary_minus(n2);

#define GEN_SAME_INF(x) (scheme_is_positive(x) ? scheme_inf_object : scheme_minus_inf_object)
#define GEN_OPP_INF(x) (!scheme_is_positive(x) ? scheme_inf_object : scheme_minus_inf_object)
#define GEN_MAKE_PZERO(x) (!scheme_is_positive(x) ? scheme_nzerod : scheme_zerod)
#define GEN_MAKE_NZERO(x) (!scheme_is_positive(x) ? scheme_zerod : scheme_nzerod)
#define GEN_MAKE_ZERO_Z(x, y) (scheme_minus_zero_p(y) ? GEN_MAKE_NZERO(x) : GEN_MAKE_PZERO(x))
#define GEN_SAME_INF_Z(x, y) (scheme_minus_zero_p(y) ?  GEN_OPP_INF(x) : GEN_SAME_INF(x))

#define GEN_SAME_SINF(x) (scheme_is_positive(x) ? scheme_single_inf_object : scheme_single_minus_inf_object)
#define GEN_OPP_SINF(x) (!scheme_is_positive(x) ? scheme_single_inf_object : scheme_single_minus_inf_object)
#define GEN_MAKE_PSZERO(x) (!scheme_is_positive(x) ? scheme_nzerof : scheme_zerof)
#define GEN_MAKE_NSZERO(x) (!scheme_is_positive(x) ? scheme_zerof : scheme_nzerof)
#define GEN_MAKE_SZERO_Z(x, y) (scheme_minus_zero_p(y) ? GEN_MAKE_NSZERO(x) : GEN_MAKE_PSZERO(x))
#define GEN_SAME_SINF_Z(x, y) (scheme_minus_zero_p(y) ?  GEN_OPP_SINF(x) : GEN_SAME_SINF(x))

#define NO_NAN_CHECK(x) /* empty */
#define NAN_RETURNS_NAN(x) if (MZ_IS_NAN(x)) return scheme_nan_object
#define NAN_RETURNS_SNAN(x) if (MZ_IS_NAN(x)) return scheme_single_nan_object

#ifdef NAN_EQUALS_ANYTHING
# define NAN_CHECK_0_IF_WEIRD(x) if (MZ_IS_NAN(x)) return 0
# define NAN_CHECK_NAN_IF_WEIRD(x) if (MZ_IS_NAN(x)) return scheme_nan_object
# define SNAN_CHECK_NAN_IF_WEIRD(x) if (MZ_IS_NAN(x)) return scheme_single_nan_object
#else
# define NAN_CHECK_0_IF_WEIRD(x) /* empty */
# define NAN_CHECK_NAN_IF_WEIRD(x) /* empty */
# define SNAN_CHECK_NAN_IF_WEIRD(x) /* empty */
#endif

# define NAN_CHECK_0(x) if (MZ_IS_NAN(x)) return 0

#define GEN_BIN_OP(name, scheme_name, iop, fop, fsop, bn_op, rop, cxop, exzeopl, exzeopr, nanckop, snanckop, nanckop_more, snanckop_more, c0_1, c1_1, c0_2, c1_2) \
  GEN_BIN_THING(Scheme_Object *, name, scheme_name, \
                iop, fop, fsop, bn_op, rop, cxop, \
                GEN_OMIT, GEN_FIRST_ONLY, \
                0, 0, 0, 0, \
                0, 0, 0, 0, \
                GEN_SCHEME_BOOL_APPLY, badfunc, badfunc, badfunc, badfunc, \
                nanckop, snanckop, nanckop_more, snanckop_more, \
                GEN_IDENT, GEN_IDENT, exzeopl, exzeopr, "number?", GEN_TOI, \
                c0_1, c1_1, c0_2, c1_2)

#define GEN_BIN_DIV_OP(name, scheme_name, iop, fop, fsop, bn_op, rop, cxop, c0_1, c1_1, c0_2, c1_2) \
  GEN_BIN_THING(Scheme_Object *, name, scheme_name, \
                iop, fop, fsop, bn_op, rop, cxop, \
                GEN_IDENT, GEN_APPLY, \
                GEN_SAME_INF, GEN_SAME_SINF, GEN_OPP_INF, GEN_OPP_SINF, \
                GEN_MAKE_NZERO, GEN_MAKE_NSZERO, GEN_MAKE_PZERO, GEN_MAKE_PSZERO, \
                GEN_APPLY3, GEN_MAKE_ZERO_Z, GEN_MAKE_SZERO_Z, GEN_SAME_INF_Z, GEN_SAME_SINF_Z, \
                NAN_CHECK_NAN_IF_WEIRD, SNAN_CHECK_NAN_IF_WEIRD, NAN_CHECK_NAN_IF_WEIRD, SNAN_CHECK_NAN_IF_WEIRD, \
                GEN_IDENT, GEN_IDENT, GEN_RETURN_0, GEN_OMIT, "number?", GEN_TOI, \
                c0_1, c1_1, c0_2, c1_2)

#define GEN_BIN_COMP(name, scheme_name, iop, fop, bn_op, rop, cxop, waybig, waysmall, firstzero, secondzero, complexwrap, noniziwrap, numbertype) \
 GEN_BIN_THING(int, name, scheme_name, \
               iop, fop, fop, bn_op, rop, cxop, \
               GEN_IDENT, GEN_FIRST_ONLY, \
               waybig, waybig, waysmall, waysmall, \
               waybig, waybig, waysmall, waysmall, \
               GEN_SCHEME_BOOL_APPLY, firstzero, firstzero, secondzero, secondzero, \
               NAN_CHECK_0_IF_WEIRD, NAN_CHECK_0_IF_WEIRD, NAN_CHECK_0, NAN_CHECK_0, \
               complexwrap, noniziwrap, GEN_OMIT, GEN_OMIT, numbertype, GEN_TOE, \
               cx_NO_CHECK, cx_NO_CHECK, cx_NO_CHECK, cx_NO_CHECK)

#define GEN_BIN_INT_OP(name, scheme_name, op, bigop) \
static Scheme_Object *name (const Scheme_Object *n1, const Scheme_Object *n2); \
static Scheme_Object *name ## __wrong_contract(const Scheme_Object *v) \
{ \
  Scheme_Object *a[1]; \
  a[0] = (Scheme_Object *)v; \
  scheme_wrong_contract(scheme_name, "exact-integer?", -1, 0, a); \
  return NULL; \
} \
static MZ_INLINE Scheme_Object * name ## __int_big(const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Bignum sb; \
  return bigop((scheme_make_small_bignum(SCHEME_INT_VAL(n1), &sb)), n2); \
} \
static MZ_INLINE Scheme_Object * name ## __big_int(const Scheme_Object *n1, const Scheme_Object *n2) { \
  Small_Bignum sb; \
  return bigop(n1, (scheme_make_small_bignum(SCHEME_INT_VAL(n2), &sb))); \
} \
static Scheme_Object * \
name (const Scheme_Object *n1, const Scheme_Object *n2) \
{ \
  if (SCHEME_INTP(n1)){ \
    if (SCHEME_INTP(n2)) { \
      intptr_t a, b; \
      a = SCHEME_INT_VAL(n1); \
      b = SCHEME_INT_VAL(n2); \
      return scheme_make_integer(a op b); \
    } else if (SCHEME_BIGNUMP(n2)) { \
      return name ## __int_big(n1, n2); \
    } \
  } else if (SCHEME_BIGNUMP(n1)) { \
    if (SCHEME_INTP(n2)) { \
      return name ## __big_int(n1, n2); \
    } \
    if (SCHEME_BIGNUMP(n2)) \
      return bigop(n1, n2); \
  } else { \
    return name ## __wrong_contract(n1);       \
  } \
 \
  return name ## __wrong_contract(n2); \
}

#define GEN_NARY_OP(stat, name, scheme_name, bin_name, ident, TYPEP, type, single) \
stat Scheme_Object *name (int argc, Scheme_Object *argv[]); \
static MZ_INLINE Scheme_Object * \
name ## __slow (Scheme_Object *ret, int argc, Scheme_Object *argv[])  \
{ \
  int i; \
  for (i = 1 ; i<argc ; ++i ) { \
    Scheme_Object *o; \
    o = argv[i]; \
    if (!TYPEP(o)) { scheme_wrong_contract(scheme_name, type, i, argc, argv); return NULL; } \
    ret = bin_name (ret, o); \
  } \
  return (ret); \
}\
Scheme_Object * \
name (int argc, Scheme_Object *argv[]) \
{ \
  Scheme_Object *ret;                          \
  if (!argc) return scheme_make_integer(ident); \
  ret = argv[0]; \
  if (!TYPEP(ret)) { scheme_wrong_contract(scheme_name, type, 0, argc, argv); return NULL; } \
  if (argc == 2) { \
    Scheme_Object *b; \
    b = argv[1]; \
    if (!TYPEP(b)) { scheme_wrong_contract(scheme_name, type, 1, argc, argv); return NULL; } \
    return bin_name(ret, b); \
  } \
  if (argc == 1) { return single(ret); } \
  return name ## __slow(ret, argc, argv); \
}

#define GEN_TWOARY_OP(stat, name, scheme_name, bin_name, TYPEP, type) \
stat Scheme_Object * name (int argc, Scheme_Object *argv[]); \
static MZ_INLINE Scheme_Object * \
name ## __slow (Scheme_Object *ret, int argc, Scheme_Object *argv[]) \
{\
  int i; \
  for ( i=1 ; i<argc ; ++i ) { \
    if (!TYPEP(argv[i])) \
      scheme_wrong_contract(scheme_name, type, i, argc, argv); \
    ret = bin_name (ret, argv[i]); \
  } \
  return ret; \
}\
Scheme_Object * \
name (int argc, Scheme_Object *argv[]) \
{ \
  Scheme_Object *ret = argv[0]; \
  if (!TYPEP(ret)) \
    scheme_wrong_contract(scheme_name, type, 0, argc, argv); \
  if (argc == 1) return ret; \
  if (argc == 2) { \
    if (!TYPEP(argv[1])) \
      scheme_wrong_contract(scheme_name, type, 1, argc, argv); \
    return bin_name(ret, argv[1]); \
  } \
  return name ## __slow(ret, argc, argv); \
}

#define BIGNUMS_AS_DOUBLES(o) d = scheme_bignum_to_double(o);

#define GEN_UNARY_OP(name, scheme_name, c_name, inf_val, sinf_val, neginf_val, sneginf_val, nan_val, snan_val, complex_fun, PRECHECK, USE_COMPLEX, BIGNUM_MODE) \
static Scheme_Object * \
name (int argc, Scheme_Object *argv[]) \
{ \
  Scheme_Type t; \
  double d; \
  FLOATWRAP( \
  D_FLOATWRAP( int is_single = 0; ) \
  S_FLOATWRAP( int is_double = 0; ) \
  ) \
  Scheme_Object *o = argv[0]; \
  PRECHECK() \
  if (SCHEME_INTP(o)) \
    d = SCHEME_INT_VAL(o); \
  else { \
   t = _SCHEME_TYPE(o); \
   FLOATWRAP( \
   if (t == scheme_float_type) { \
     D_FLOATWRAP( is_single = 1; ) \
     d = SCHEME_FLT_VAL(o); \
   } else ) \
   if (t == scheme_double_type) { \
     S_FLOATWRAP( is_double = 1; ) \
     d = SCHEME_DBL_VAL(o); \
   } else if (t == scheme_bignum_type) { \
     BIGNUM_MODE(o) \
   } else if (t == scheme_rational_type) { \
     d = scheme_rational_to_double(o); \
   } else if (t == scheme_complex_type) \
     return complex_fun(o); \
   else { \
     scheme_wrong_contract(#scheme_name, "number?", 0, argc, argv); \
     return NULL; \
    } \
  } \
  if (MZ_IS_NAN(d)) { FLOATWRAP(if (D_FLOATWRAP(is_single) S_FLOATWRAP(!is_double)) return snan_val; ) return nan_val; } \
  if (MZ_IS_POS_INFINITY(d)) { FLOATWRAP(if (D_FLOATWRAP(is_single) S_FLOATWRAP(!is_double)) return sinf_val; ) return inf_val; } \
  if (MZ_IS_NEG_INFINITY(d)) { FLOATWRAP(if (D_FLOATWRAP(is_single) S_FLOATWRAP(!is_double)) return sneginf_val; ) return neginf_val; } \
  if (USE_COMPLEX(d)) { \
      Small_Complex sc; \
      Scheme_Object *o; \
      FLOATWRAP( \
      D_FLOATWRAP( if (is_single) ) \
      S_FLOATWRAP( if (!is_double) ) \
      FLOATWRAP(    o = scheme_make_float((float)d); \
        else ) \
      ) \
      o = scheme_make_double(d); \
      return complex_fun(scheme_make_small_complex(o, &sc)); \
  } \
  d = c_name(d); \
  FLOATWRAP( \
  D_FLOATWRAP( if (is_single) ) \
  S_FLOATWRAP( if (!is_double) ) \
    FLOATWRAP(    return scheme_make_float((float)d); ) \
  ) \
  return scheme_make_double(d); \
}

