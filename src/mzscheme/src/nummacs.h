/*
  MzScheme
  Copyright (c) 2004-2005 PLT Scheme, Inc.
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
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#define NEED_NUMBER(name) \
  scheme_wrong_type(#name, "number", 0, argc, argv)
#define NEED_REAL(name) \
  scheme_wrong_type(#name, REAL_NUMBER_STR, 0, argc, argv)
#define NEED_INTEGER(name) \
  scheme_wrong_type(#name, "integer", 0, argc, argv)
#define WRONG_TYPE(name, expected, value) \
  scheme_wrong_type(name, expected, -1, 0, (Scheme_Object **)&value)

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
static Scheme_Object * \
name (int argc, Scheme_Object *argv[]) \
{ \
  int i; \
  Scheme_Object *p; \
  p = argv[0]; \
  if (argc == 1) if (!TYPEP(p)) \
   scheme_wrong_type(scheme_name, type, 0, argc, argv); \
  for (i = 1; i < argc; i++) {\
    Scheme_Object *o = argv[i]; \
    if (!TYPEP(o)) { \
      scheme_wrong_type(scheme_name, type, i, argc, argv); \
      return NULL; \
    } \
    if (!bin_name(p, o)) { \
        for (i++; i < argc; i++) { \
          if (!TYPEP(argv[i])) \
           scheme_wrong_type(scheme_name, type, i, argc, argv); \
        } \
        return scheme_false; \
    } \
    p = o; \
  } \
  return scheme_true; \
}

#define GEN_BIN_PROT(name) \
static Scheme_Object *name (const Scheme_Object *n1, const Scheme_Object *n2)

/* This macro is used to implement most all binary math and comparison functions (!): */
#define GEN_BIN_THING(rettype, name, scheme_name, \
                      iop, fop, fsop, bn_op, rop, cxop, \
                      wrap, combineinf, \
                      waybigf, swaybigf, waysmallf, swaysmallf, \
                      waybigs, swaybigs, waysmalls, swaysmalls, \
                      combinezero, firstzero, sfirstzero, secondzero, ssecondzero, \
                      nanchk, snanchk, \
                      complexwrap, noniziwrap, exactzerowrapl, exactzerowrapr, numbertype,\
                      toi_or_toe) \
rettype \
name (const Scheme_Object *n1, const Scheme_Object *n2) \
{ \
  Small_Bignum sb; \
  Small_Rational sr; \
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
      if (t2 == scheme_bignum_type) \
        return bn_op((scheme_make_small_bignum(SCHEME_INT_VAL(n1), \
					       &sb)), \
		     (n2)); \
       if (t2 == scheme_rational_type) \
        return rop((scheme_make_small_rational(SCHEME_INT_VAL(n1), \
						      &sr)), \
		   (n2)); \
      complexwrap( \
      if (noniziwrap((t2 == scheme_complex_type) ||) (t2 == scheme_complex_izi_type)) { \
        Small_Complex sc; \
	return cxop((scheme_make_small_complex(n1, &sc)), \
		    (n2)); \
      } \
      ) \
      WRONG_TYPE(scheme_name, numbertype, n2); \
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
        snanchk(d1); \
        wrap(if (MZ_IS_POS_INFINITY(d1)) return combineinf(swaybigf, n2);) \
        wrap(if (MZ_IS_NEG_INFINITY(d1)) return combineinf(swaysmallf, n2);) \
        return toi_or_toe(fsop(d1, scheme_bignum_to_float(n2)), \
                          rop(rat_from_float(d1, &sr), scheme_integer_to_rational(n2))); \
      } \
       if (t2 == scheme_rational_type) { \
        snanchk(d1); \
        wrap(if (MZ_IS_POS_INFINITY(d1)) return combineinf(swaybigf, n2);) \
        wrap(if (MZ_IS_NEG_INFINITY(d1)) return combineinf(swaysmallf, n2);) \
        wrap(if (d1 == 0.0) return combinezero(sfirstzero, n2, d1);) \
	return toi_or_toe(fsop(d1, scheme_rational_to_float(n2)), \
                          rop(rat_from_float(d1, &sr), (n2))); \
      } \
      complexwrap( \
      if (noniziwrap((t2 == scheme_complex_type) ||) (t2 == scheme_complex_izi_type)) { \
        Small_Complex sc; \
        snanchk(d1); \
	return cxop((scheme_make_small_complex(n1, &sc)), \
		    (n2)); \
      } \
      )\
      WRONG_TYPE(scheme_name, numbertype, n2); \
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
        nanchk(d1); \
        wrap(if (MZ_IS_POS_INFINITY(d1)) return combineinf(waybigf, n2);) \
        wrap(if (MZ_IS_NEG_INFINITY(d1)) return combineinf(waysmallf, n2);) \
        return toi_or_toe(fop(d1, scheme_bignum_to_double(n2)), \
                          rop(rat_from_double(d1, &sr), scheme_integer_to_rational(n2))); \
      } \
       if (t2 == scheme_rational_type) { \
        nanchk(d1); \
        wrap(if (MZ_IS_POS_INFINITY(d1)) return combineinf(waybigf, n2);) \
        wrap(if (MZ_IS_NEG_INFINITY(d1)) return combineinf(waysmallf, n2);) \
        wrap(if (d1 == 0.0) return combinezero(firstzero, n2, d1);) \
	return toi_or_toe(fop(d1, scheme_rational_to_double(n2)), \
                          rop(rat_from_double(d1, &sr), (n2))); \
      } \
      complexwrap( \
      if (noniziwrap((t2 == scheme_complex_type) ||) (t2 == scheme_complex_izi_type)) { \
        Small_Complex sc; \
        nanchk(d1); \
	return cxop((scheme_make_small_complex(n1, &sc)), \
		    (n2)); \
      } \
      )\
      WRONG_TYPE(scheme_name, numbertype, n2); \
    } \
  else if (t1 == scheme_bignum_type) \
    { \
       if (SCHEME_INTP(n2)) \
	 return bn_op((n1), \
		      (scheme_make_small_bignum(SCHEME_INT_VAL(n2), &sb))); \
      t2 = _SCHEME_TYPE(n2); \
      FLOATWRAP( \
      if (t2 == scheme_float_type) { \
         float d2; \
         d2 = SCHEME_FLT_VAL(n2); \
         snanchk(d2); \
         wrap(if (MZ_IS_POS_INFINITY(d2)) return combineinf(swaysmalls, n1);) \
         wrap(if (MZ_IS_NEG_INFINITY(d2)) return combineinf(swaybigs, n1);) \
	 return toi_or_toe(fsop(scheme_bignum_to_float(n1), d2), \
                           rop(scheme_integer_to_rational(n1), rat_from_float(d2, &sr))); \
      } \
      ) \
      if (t2 == scheme_double_type) { \
         double d2; \
         d2 = SCHEME_DBL_VAL(n2); \
         nanchk(d2); \
         wrap(if (MZ_IS_POS_INFINITY(d2)) return combineinf(waysmalls, n1);) \
         wrap(if (MZ_IS_NEG_INFINITY(d2)) return combineinf(waybigs, n1);) \
	 return toi_or_toe(fop(scheme_bignum_to_double(n1), d2), \
                           rop(scheme_integer_to_rational(n1), rat_from_double(d2, &sr))); \
       } \
       if (t2 == scheme_bignum_type) \
         return bn_op((n1), (n2)); \
       if (t2 == scheme_rational_type) \
	 return rop(scheme_integer_to_rational(n1), \
		    (n2)); \
       complexwrap( \
       if (noniziwrap((t2 == scheme_complex_type) ||) (t2 == scheme_complex_izi_type)) { \
         Small_Complex sc; \
	 return cxop((scheme_make_small_complex(n1, &sc)), \
		     (n2)); \
       } \
       )\
       WRONG_TYPE(scheme_name, numbertype, n2); \
    } \
  else if (t1 == scheme_rational_type) \
    { \
       if (SCHEME_INTP(n2)) \
	 return rop((n1), \
		    (scheme_make_small_rational(SCHEME_INT_VAL(n2), \
						       &sr))); \
      t2 = _SCHEME_TYPE(n2); \
      FLOATWRAP( \
      if (t2 == scheme_float_type) { \
         float d2; \
         d2 = SCHEME_FLT_VAL(n2); \
         snanchk(d2); \
         wrap(if (MZ_IS_POS_INFINITY(d2)) return combineinf(swaysmalls, n1);) \
         wrap(if (MZ_IS_NEG_INFINITY(d2)) return combineinf(swaybigs, n1);) \
         wrap(if (d2 == 0.0) return combinezero(ssecondzero, n1, d2);) \
	 return toi_or_toe(fsop(scheme_rational_to_float(n1), d2), \
                           rop((n1), rat_from_float(d2, &sr))); \
       } \
       ) \
       if (t2 == scheme_double_type) { \
         double d2; \
         d2 = SCHEME_DBL_VAL(n2); \
         nanchk(d2); \
         wrap(if (MZ_IS_POS_INFINITY(d2)) return combineinf(waysmalls, n1);) \
         wrap(if (MZ_IS_NEG_INFINITY(d2)) return combineinf(waybigs, n1);) \
         wrap(if (d2 == 0.0) return combinezero(secondzero, n1, d2);) \
	 return toi_or_toe(fop(scheme_rational_to_double(n1), d2), \
                           rop((n1), rat_from_double(d2, &sr))); \
       } \
       if (t2 == scheme_bignum_type) \
         return rop((n1), \
		    scheme_integer_to_rational(n2)); \
       if (t2 == scheme_rational_type) \
	 return rop((n1), (n2)); \
       complexwrap( \
       if (noniziwrap((t2 == scheme_complex_type) ||) (t2 == scheme_complex_izi_type)) { \
         Small_Complex sc; \
	 return cxop((scheme_make_small_complex(n1, &sc)), (n2)); \
       } \
       )\
       WRONG_TYPE(scheme_name, numbertype, n2); \
    } \
  complexwrap( \
  else if (noniziwrap((t1 == scheme_complex_type) ||) (t1 == scheme_complex_izi_type)) \
    { \
       Small_Complex sc; \
       if (SCHEME_INTP(n2)) \
	 return cxop((n1), \
		     (scheme_make_small_complex(n2, &sc))); \
       t2 = _SCHEME_TYPE(n2); \
       FLOATWRAP( \
       if (t2 == scheme_float_type) { \
         snanchk(SCHEME_FLT_VAL(n2)); \
	 return cxop((n1), \
		     (scheme_make_small_complex(n2, &sc))); \
       } \
       ) \
       if (t2 == scheme_double_type) { \
         nanchk(SCHEME_DBL_VAL(n2)); \
	 return cxop((n1), \
		     (scheme_make_small_complex(n2, &sc))); \
       } \
       if (t2 == scheme_bignum_type) \
         return cxop((n1), \
		     (scheme_make_small_complex(n2, &sc))); \
       if (t2 == scheme_rational_type) \
         return cxop((n1), \
		     (scheme_make_small_complex(n2, &sc))); \
       if (noniziwrap((t2 == scheme_complex_type) ||) (t2 == scheme_complex_izi_type)) \
	 return cxop((n1), (n2)); \
       WRONG_TYPE(scheme_name, numbertype, n2); \
    } \
  ) \
  else \
       WRONG_TYPE(scheme_name, numbertype, n1); \
  } \
  return 0; \
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
#define GEN_SCHEME_BOOL_APPLY(x, y, z) SCHEME_TRUEP(x(1, (Scheme_Object **)&y))
#define GEN_TOI(x, y) x
#define GEN_TOE(x, y) y

#define GEN_RETURN_0(x) x return zeroi;
#define GEN_RETURN_0_USUALLY(x) x if (!SCHEME_FLOATP(n2) || GR_NAN_CHK(n2) || (SCHEME_FLOAT_VAL(n2) != 0)) return zeroi;
#define GEN_RETURN_1(x) x return scheme_make_integer(1);
#define GEN_RETURN_N1(x) x return (Scheme_Object *)n1;
#define GEN_RETURN_N2(x) x return (Scheme_Object *)n2;
#define GEN_SINGLE_SUBTRACT_N2(x) x if SCHEME_FLOATP(n2) return minus(1, (Scheme_Object **)&n2);

#define GEN_SAME_INF(x) ((SCHEME_TRUEP(scheme_positive_p(1, (Scheme_Object **)&x))) ? scheme_inf_object : scheme_minus_inf_object)
#define GEN_OPP_INF(x) ((SCHEME_FALSEP(scheme_positive_p(1, (Scheme_Object **)&x))) ? scheme_inf_object : scheme_minus_inf_object)
#define GEN_MAKE_PZERO(x) ((SCHEME_FALSEP(scheme_positive_p(1, (Scheme_Object **)&x))) ? scheme_nzerod : scheme_zerod)
#define GEN_MAKE_NZERO(x) ((SCHEME_FALSEP(scheme_positive_p(1, (Scheme_Object **)&x))) ? scheme_zerod : scheme_nzerod)
#define GEN_MAKE_ZERO_Z(x, y) (scheme_minus_zero_p(y) ? GEN_MAKE_NZERO(x) : GEN_MAKE_PZERO(x))
#define GEN_SAME_INF_Z(x, y) (scheme_minus_zero_p(y) ?  GEN_OPP_INF(x) : GEN_SAME_INF(x))

#define GEN_SAME_SINF(x) ((SCHEME_TRUEP(scheme_positive_p(1, (Scheme_Object **)&x))) ? scheme_single_inf_object : scheme_single_minus_inf_object)
#define GEN_OPP_SINF(x) ((SCHEME_FALSEP(scheme_positive_p(1, (Scheme_Object **)&x))) ? scheme_single_inf_object : scheme_single_minus_inf_object)
#define GEN_MAKE_PSZERO(x) ((SCHEME_FALSEP(scheme_positive_p(1, (Scheme_Object **)&x))) ? scheme_nzerof : scheme_zerof)
#define GEN_MAKE_NSZERO(x) ((SCHEME_FALSEP(scheme_positive_p(1, (Scheme_Object **)&x))) ? scheme_zerof : scheme_nzerof)
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

#define GEN_BIN_OP(name, scheme_name, iop, fop, fsop, bn_op, rop, cxop, exzeopl, exzeopr, nanckop, snanckop) \
  GEN_BIN_THING(Scheme_Object *, name, scheme_name, \
                iop, fop, fsop, bn_op, rop, cxop, \
                GEN_OMIT, GEN_FIRST_ONLY, \
                0, 0, 0, 0, \
                0, 0, 0, 0, \
                GEN_SCHEME_BOOL_APPLY, badfunc, badfunc, badfunc, badfunc, \
                nanckop, snanckop, \
                GEN_IDENT, GEN_IDENT, exzeopl, exzeopr, "number", GEN_TOI)

#define GEN_BIN_DIV_OP(name, scheme_name, iop, fop, fsop, bn_op, rop, cxop) \
  GEN_BIN_THING(Scheme_Object *, name, scheme_name, \
                iop, fop, fsop, bn_op, rop, cxop, \
                GEN_IDENT, GEN_APPLY, \
                GEN_SAME_INF, GEN_SAME_SINF, GEN_OPP_INF, GEN_OPP_SINF, \
                GEN_MAKE_NZERO, GEN_MAKE_NSZERO, GEN_MAKE_PZERO, GEN_MAKE_PSZERO, \
                GEN_APPLY3, GEN_MAKE_ZERO_Z, GEN_MAKE_SZERO_Z, GEN_SAME_INF_Z, GEN_SAME_SINF_Z, \
                NAN_CHECK_NAN_IF_WEIRD, SNAN_CHECK_NAN_IF_WEIRD, \
                GEN_IDENT, GEN_IDENT, GEN_RETURN_0, GEN_OMIT, "number", GEN_TOI)

#define GEN_BIN_COMP(name, scheme_name, iop, fop, bn_op, rop, cxop, waybig, waysmall, firstzero, secondzero, complexwrap, noniziwrap, numbertype) \
 GEN_BIN_THING(int, name, scheme_name, \
               iop, fop, fop, bn_op, rop, cxop, \
               GEN_IDENT, GEN_FIRST_ONLY, \
               waybig, waybig, waysmall, waysmall, \
               waybig, waybig, waysmall, waysmall, \
               GEN_SCHEME_BOOL_APPLY, firstzero, firstzero, secondzero, secondzero, \
               NAN_CHECK_0_IF_WEIRD, NAN_CHECK_0_IF_WEIRD, \
               complexwrap, noniziwrap, GEN_OMIT, GEN_OMIT, numbertype, GEN_TOE)

#define GEN_BIN_INT_OP(name, scheme_name, op, bigop) \
static Scheme_Object * \
name (const Scheme_Object *n1, const Scheme_Object *n2) \
{ \
  Small_Bignum sb; \
  if (SCHEME_INTP(n1)){ \
    if (SCHEME_INTP(n2)) { \
      long a, b; \
      a = SCHEME_INT_VAL(n1); \
      b = SCHEME_INT_VAL(n2); \
      return scheme_make_integer(a op b); \
    } else if (SCHEME_BIGNUMP(n2)) \
      return bigop(scheme_make_small_bignum(SCHEME_INT_VAL(n1), &sb), n2); \
  } else if (SCHEME_BIGNUMP(n1)) { \
    if (SCHEME_INTP(n2)) \
      return bigop(n1, scheme_make_small_bignum(SCHEME_INT_VAL(n2), &sb)); \
    if (SCHEME_BIGNUMP(n2)) \
      return bigop(n1, n2); \
  } else { \
    WRONG_TYPE(scheme_name, "exact integer", n1); \
    return scheme_void; \
  } \
 \
  WRONG_TYPE(scheme_name, "exact integer", n2); \
  return scheme_void; \
}

#define GEN_NARY_OP(name, scheme_name, bin_name, ident, TYPEP, type) \
static Scheme_Object * \
name (int argc, Scheme_Object *argv[]) \
{ \
  Scheme_Object *ret; \
  int i; \
  if (!argc) return scheme_make_integer(ident); \
  ret = argv[0]; \
  if (!TYPEP(ret)) { scheme_wrong_type(scheme_name, type, 0, argc, argv); return NULL; } \
  if (argc == 2) { \
    Scheme_Object *b; \
    b = argv[1]; \
    if (!TYPEP(b)) { scheme_wrong_type(scheme_name, type, 1, argc, argv); return NULL; } \
    return bin_name(ret, b); \
  } \
  for (i = 1 ; i<argc ; ++i ) { \
    Scheme_Object *o; \
    o = argv[i]; \
    if (!TYPEP(o)) { scheme_wrong_type(scheme_name, type, i, argc, argv); return NULL; } \
    ret = bin_name (ret, o); \
  } \
  return (ret); \
}

#define GEN_TWOARY_OP(stat, name, scheme_name, bin_name, TYPEP, type) \
stat Scheme_Object * \
name (int argc, Scheme_Object *argv[]) \
{ \
  Scheme_Object *ret; \
  int i; \
  if (!TYPEP(argv[0])) \
    scheme_wrong_type(scheme_name, type, 0, argc, argv); \
  if (argc == 1) return argv[0]; \
  if (argc == 2) { \
    if (!TYPEP(argv[1])) \
      scheme_wrong_type(scheme_name, type, 1, argc, argv); \
    return bin_name(argv[0], argv[1]); \
  } \
  ret = argv[0]; \
  for ( i=1 ; i<argc ; ++i ) { \
    if (!TYPEP(argv[i])) \
      scheme_wrong_type(scheme_name, type, i, argc, argv); \
    ret = bin_name (ret, argv[i]); \
  } \
  return ret; \
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
   } else if ((t == scheme_complex_type) || (t == scheme_complex_izi_type)) \
     return complex_fun(o); \
   else { \
     scheme_wrong_type(#scheme_name, "number", 0, argc, argv); \
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

