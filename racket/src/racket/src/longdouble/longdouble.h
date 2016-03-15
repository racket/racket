#ifndef MZ_LONGDOUBLE_H
#define MZ_LONGDOUBLE_H

/* Functions like `extfl->floating-point-bytes` assume that the
   content of a `long double' occupies the first 10 bytes: */
#define LONG_DOUBLE_BYTE_LEN 10

#ifndef MZ_LONG_DOUBLE_API_IS_EXTERNAL
# if defined(__MINGW32__) && defined(MZ_LONG_DOUBLE)
#  define LONG_DOUBLE_STRING_OP_API_IS_EXTERNAL
# endif
#endif

#if defined(MZ_LONG_DOUBLE_API_IS_EXTERNAL)           \
    || defined(LONG_DOUBLE_STRING_OP_API_IS_EXTERNAL) \
    || defined(IMPLEMENTING_MSC_LONGDOUBLE)
# define SIZEOF_LONGDOUBLE 16
#endif

#ifdef BYTES_RESERVED_FOR_LONG_DOUBLE
/* check "scheme.h" versus "longdouble.h": */
# if BYTES_RESERVED_FOR_LONG_DOUBLE != SIZEOF_LONGDOUBLE
   !! mismatch in mz_long_double size !!
# endif
#endif
             
#ifdef IMPLEMENTING_MSC_LONGDOUBLE
typedef union long_double 
{
  char bytes[SIZEOF_LONGDOUBLE];
  long double val;
} long_double;
# define LDBL_DLL_API __declspec(dllexport)
# define XFORM_NONGCING /* empty */
#else
#  define long_double mz_long_double
# define LDBL_DLL_API /* empty */
#endif

#if defined(MZ_LONG_DOUBLE_API_IS_EXTERNAL) || defined(IMPLEMENTING_MSC_LONGDOUBLE)

void scheme_load_long_double_dll();

XFORM_NONGCING LDBL_DLL_API long_double get_long_double_infinity_val();
XFORM_NONGCING LDBL_DLL_API long_double get_long_double_minus_infinity_val();
XFORM_NONGCING LDBL_DLL_API long_double get_long_double_zero();
XFORM_NONGCING LDBL_DLL_API long_double get_long_double_nzero();
XFORM_NONGCING LDBL_DLL_API long_double get_long_double_nan();
XFORM_NONGCING LDBL_DLL_API long_double get_long_double_1();
XFORM_NONGCING LDBL_DLL_API long_double get_long_double_minus_1();
XFORM_NONGCING LDBL_DLL_API long_double get_long_double_2();
XFORM_NONGCING LDBL_DLL_API long_double get_long_double_one_half();

XFORM_NONGCING LDBL_DLL_API long_double get_long_double_pi();
XFORM_NONGCING LDBL_DLL_API long_double get_long_double_half_pi();

XFORM_NONGCING LDBL_DLL_API void set_long_double(long_double a, long_double b);

XFORM_NONGCING LDBL_DLL_API long_double long_double_from_int(int a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_from_float(float a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_from_double(double a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_from_intptr(intptr_t a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_from_uintptr(uintptr_t a);

XFORM_NONGCING LDBL_DLL_API double double_from_long_double(long_double a);
XFORM_NONGCING LDBL_DLL_API float float_from_long_double(long_double a);
XFORM_NONGCING LDBL_DLL_API intptr_t int_from_long_double(long_double a);

XFORM_NONGCING LDBL_DLL_API uintptr_t uintptr_from_long_double(long_double a);

XFORM_NONGCING LDBL_DLL_API long_double long_double_plus(long_double a, long_double b);
XFORM_NONGCING LDBL_DLL_API long_double long_double_minus(long_double a, long_double b);
XFORM_NONGCING LDBL_DLL_API long_double long_double_mult(long_double a, long_double b);
XFORM_NONGCING LDBL_DLL_API long_double long_double_mult_i(long_double a, int b);
XFORM_NONGCING LDBL_DLL_API long_double long_double_div(long_double a, long_double b);
XFORM_NONGCING LDBL_DLL_API long_double long_double_neg(long_double a);

XFORM_NONGCING LDBL_DLL_API int long_double_eqv(long_double a, long_double b);
XFORM_NONGCING LDBL_DLL_API int long_double_less(long_double a, long_double b);
XFORM_NONGCING LDBL_DLL_API int long_double_less_or_eqv(long_double a, long_double b);
XFORM_NONGCING LDBL_DLL_API int long_double_greater(long_double a, long_double b);
XFORM_NONGCING LDBL_DLL_API int long_double_greater_or_eqv(long_double a, long_double b);

XFORM_NONGCING LDBL_DLL_API int long_double_eqv_i(int a, long_double b);

XFORM_NONGCING LDBL_DLL_API int long_double_is_zero(long_double a);
XFORM_NONGCING LDBL_DLL_API int long_double_is_1(long_double a);
XFORM_NONGCING LDBL_DLL_API int long_double_minus_zero_p(long_double a);
XFORM_NONGCING LDBL_DLL_API int long_double_is_nan(long_double a);
XFORM_NONGCING LDBL_DLL_API int long_double_is_pos_infinity(long_double a);
XFORM_NONGCING LDBL_DLL_API int long_double_is_neg_infinity(long_double a);
XFORM_NONGCING LDBL_DLL_API int long_double_is_infinity(long_double a);

XFORM_NONGCING LDBL_DLL_API long_double long_double_fabs(long_double a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_modf(long_double a, long_double *b);
XFORM_NONGCING LDBL_DLL_API long_double long_double_fmod(long_double a, long_double b);
XFORM_NONGCING LDBL_DLL_API long_double long_double_trunc(long_double a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_floor(long_double a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_ceil(long_double a);

XFORM_NONGCING LDBL_DLL_API long_double long_double_sin(long_double a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_cos(long_double a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_tan(long_double a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_asin(long_double a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_acos(long_double a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_atan(long_double a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_log(long_double a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_exp(long_double a);
XFORM_NONGCING LDBL_DLL_API long_double long_double_ldexp(long_double a, int i);

XFORM_NONGCING LDBL_DLL_API long_double long_double_pow(long_double a, long_double b);

XFORM_NONGCING LDBL_DLL_API long_double long_double_sqrt(long_double a);

XFORM_NONGCING LDBL_DLL_API long_double long_double_frexp(long_double a, int* i);

XFORM_NONGCING LDBL_DLL_API void long_double_sprint(char* buffer, int digits, long_double d);

XFORM_NONGCING LDBL_DLL_API long_double long_double_array_ref(void *pointer, int index);
XFORM_NONGCING LDBL_DLL_API void long_double_array_set(void *pointer, int index, long_double value);

XFORM_NONGCING LDBL_DLL_API long_double long_double_from_string(char* buff, char** p);
XFORM_NONGCING LDBL_DLL_API void long_double_from_string_indirect(char* buff, char** p, long_double *_ld);

XFORM_NONGCING void to_double_prec();
XFORM_NONGCING void to_extended_prec();

XFORM_NONGCING int long_double_available();

#else

# define get_long_double_infinity_val() 1.0L/0.0L
# define get_long_double_minus_infinity_val() -1.0L/0.0L
# define get_long_double_zero() 0.0L
# define get_long_double_nzero() (0.0L*(-1.0L))
# define get_long_double_nan() get_long_double_infinity_val() + get_long_double_minus_infinity_val()
# define get_long_double_1() 1.0L
# define get_long_double_minus_1() (-1.0L)
# define get_long_double_2() 2.0L
# define get_long_double_one_half() 0.5L 

# define get_long_double_pi() atan2l(0.0L, -1.0L)
# define get_long_double_half_pi() atan2l(0.0L, -1.0L)/2.0L

# define long_double_from_int(a) ((long double)(a))
# define long_double_from_float(a) ((long double)(a))
# define long_double_from_double(a) ((long double)(a))
# define long_double_from_intptr(a) ((long double)(a))
# define long_double_from_uintptr(a) ((long double)(a))

# define double_from_long_double(a) (a)
# define float_from_long_double(a) (a)
# define int_from_long_double(a) ((int)(a))
# define uintptr_from_long_double(a) ((uintptr_t)(a))

# define long_double_plus(a,b) ((a)+(b))
# define long_double_minus(a,b) ((a)-(b))
# define long_double_mult(a,b) ((a)*(b))
# define long_double_div(a,b) ((a)/(b))
# define long_double_neg(x) (-(x))

# define long_double_mult_i(a,b) ((a)*(b))

# define long_double_eqv(a,b) ((a)==(b))
# define long_double_less(a,b) ((a)<(b))
# define long_double_less_or_eqv(a,b) ((a)<=(b))
# define long_double_greater(a,b) ((a)>(b))
# define long_double_greater_or_eqv(a,b) ((a)>=(b))

# define long_double_eqv_i(a,b) ((long double)(a) == (b))

# define long_double_is_zero(a) ((a) == 0.0L)
# define long_double_is_1(a) ((a) == 1.0L)
# define long_double_minus_zero_p(a) ((1.0L/(a)) < 0.0L)
# define long_double_is_nan(a) (isnan(a))
# define long_double_is_pos_infinity(a) (isinf(a)&&((a)>0))
# define long_double_is_neg_infinity(a) (isinf(a)&&((a)<0))
# define long_double_is_infinity(a) (isinf(a))

# define long_double_fabs(a) fabsl(a)
# define long_double_modf(a,b) modfl(a,b)
# define long_double_fmod(a,b) fmodl(a,b)
# define long_double_trunc(a) truncl(a)
# define long_double_floor(a) floorl(a)
# define long_double_ceil(a) ceill(a)

# define long_double_sin(x) sinl(x)
# define long_double_cos(x) cosl(x)
# define long_double_tan(x) tanl(x)
# define long_double_asin(x) asinl(x)
# define long_double_acos(x) acosl(x)
# define long_double_atan(x) atanl(x)
# define long_double_log(x) logl(x)
# define long_double_exp(x) expl(x)
# define long_double_ldexp(a, i) ldexpl(a, i)

# define long_double_pow(x,y) powl(x, y)

# define long_double_sqrt(a) sqrtl(a)

# define long_double_frexp(a, i) frexpl(a, i)

# ifdef LONG_DOUBLE_STRING_OP_API_IS_EXTERNAL
XFORM_NONGCING LDBL_DLL_API long_double long_double_from_string(char* buff, char** p);
XFORM_NONGCING LDBL_DLL_API void long_double_sprint(char* buffer, int digits, long_double d);
# else
#  define long_double_from_string(x,y) strtold(x, y)
#  define long_double_sprint(buffer,digits,d) sprintf(buffer, "%.*Lg", digits, d)
# endif

# define long_double_array_ref(pointer,index) ((long_double *)(pointer))[index]
# define long_double_array_set(pointer,index,value) ((long_double *)(pointer))[index] = (value)

# ifdef LONG_DOUBLE_STRING_OP_API_IS_EXTERNAL
XFORM_NONGCING void scheme_load_long_double_dll();
XFORM_NONGCING int long_double_available();
# else
#  define long_double_available() 1
# endif

#endif

#endif // MZ_LONGDOUBLE_H
