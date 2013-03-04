#ifndef LONGDOUBLE_H
#define LONGDOUBLE_H

#ifdef __MINGW32__
#include <stdint.h>
#endif

#if defined(_MSC_VER) || defined(__MINGW32__)
/* aligning */
# if defined(_X86_64) || defined(_M_X64) || defined(_WIN64)
#  define SIZEOF_LONGDOUBLE 16
# else
#  define SIZEOF_LONGDOUBLE 16
# endif
             
# ifdef __MINGW32__
typedef union long_double 
{
  char bytes[SIZEOF_LONGDOUBLE];
  long double val;
} long_double;
# else
typedef struct 
{
  char bytes[SIZEOF_LONGDOUBLE];
} long_double;
# endif
#else
# define long_double long double
# define SIZEOF_LONGDOUBLE sizeof(long double)
#endif


#ifdef _MSC_VER
/* because dll can be compiled only with mingw/gcc */
# define DLL_API __declspec(dllimport) 
#else
# ifdef _MINGW32_
#  define DLL_API __declspec(dllexport)
# else
#  define DLL_API 
# endif
#endif

#if defined(_MSC_VER) || defined(__MINGW32__)

DLL_API long_double get_long_double_infinity_val();
DLL_API long_double get_long_double_minus_infinity_val();
DLL_API long_double get_long_double_zero();
DLL_API long_double get_long_double_nzero();
DLL_API long_double get_long_double_nan();
DLL_API long_double get_long_double_1();
DLL_API long_double get_long_double_minus_1();
DLL_API long_double get_long_double_2();
DLL_API long_double get_long_double_one_half();

DLL_API long_double get_long_double_pi();
DLL_API long_double get_long_double_half_pi();

DLL_API void set_long_double(long_double a, long_double b);

DLL_API long_double long_double_from_int(int a);
DLL_API long_double long_double_from_float(float a);
DLL_API long_double long_double_from_double(double a);
DLL_API long_double long_double_from_uintptr(uintptr_t a);

DLL_API double double_from_long_double(long_double a);
DLL_API float float_from_long_double(long_double a);
DLL_API intptr_t int_from_long_double(long_double a);

DLL_API uintptr_t uintptr_from_long_double(long_double a);

DLL_API long_double long_double_plus(long_double a, long_double b);
DLL_API long_double long_double_minus(long_double a, long_double b);
DLL_API long_double long_double_mult(long_double a, long_double b);
DLL_API long_double long_double_mult_i(long_double a, int b);
DLL_API long_double long_double_div(long_double a, long_double b);
DLL_API long_double long_double_neg(long_double a);

DLL_API int long_double_eqv(long_double a, long_double b);
DLL_API int long_double_less(long_double a, long_double b);
DLL_API int long_double_less_or_eqv(long_double a, long_double b);
DLL_API int long_double_greater(long_double a, long_double b);
DLL_API int long_double_greater_or_eqv(long_double a, long_double b);

DLL_API int long_double_eqv_i(int a, long_double b);

DLL_API int long_double_is_zero(long_double a);
DLL_API int long_double_is_1(long_double a);
DLL_API int long_double_minus_zero_p(long_double a);
DLL_API int long_double_is_nan(long_double a);
DLL_API int long_double_is_pos_infinity(long_double a);
DLL_API int long_double_is_neg_infinity(long_double a);
DLL_API int long_double_is_infinity(long_double a);

DLL_API long_double long_double_fabs(long_double a);
DLL_API long_double long_double_modf(long_double a, long_double *b);
DLL_API long_double long_double_fmod(long_double a, long_double b);
DLL_API long_double long_double_trunc(long_double a);
DLL_API long_double long_double_floor(long_double a);
DLL_API long_double long_double_ceil(long_double a);

DLL_API long_double long_double_sin(long_double a);
DLL_API long_double long_double_cos(long_double a);
DLL_API long_double long_double_tan(long_double a);
DLL_API long_double long_double_asin(long_double a);
DLL_API long_double long_double_acos(long_double a);
DLL_API long_double long_double_atan(long_double a);
DLL_API long_double long_double_log(long_double a);
DLL_API long_double long_double_exp(long_double a);
DLL_API long_double long_double_ldexp(long_double a, int i);

DLL_API long_double long_double_pow(long_double a, long_double b);

DLL_API long_double long_double_sqrt(long_double a);

DLL_API long_double long_double_frexp(long_double a, int* i);

DLL_API void long_double_sprint(char* buffer, int digits, long_double d);

DLL_API long_double long_double_array_ref(void *pointer, int index);
DLL_API void long_double_array_set(void *pointer, int index, long_double value);

DLL_API long_double long_double_from_string(char* buff, char** p);

#else

# define get_long_double_infinity_val() 1.0L/0.0L
# define get_long_double_minus_infinity_val() -1.0L/0.0L
# define get_long_double_zero() 0.0L
# define get_long_double_nzero() 0.0L*-1.0L
# define get_long_double_nan() get_long_double_infinity_val() + get_long_double_minus_infinity_val()
# define get_long_double_1() 1.0L
# define get_long_double_minus_1() -1.0L
# define get_long_double_2() 2.0L
# define get_long_double_one_half() 0.5L 

# define get_long_double_pi() atan2l(0.0L, -1.0L)
# define get_long_double_half_pi() atan2l(0.0L, -1.0L)/2.0L

# define long_double_from_int(a) (long double) a
# define long_double_from_float(a) (long double) a
# define long_double_from_double(a) (long double) a
# define long_double_from_uintptr(a) (long double) a

# define double_from_long_double(a) a
# define float_from_long_double(a) a
# define int_from_long_double(a) (int)a
# define uintptr_from_long_double(a) (uintptr_t)a

# define long_double_plus(a,b) a+b
# define long_double_minus(a,b) a-b
# define long_double_mult(a,b) a*b
# define long_double_div(a,b) a/b
# define long_double_neg(x) -x

# define long_double_mult_i(a,b) a*b

# define long_double_eqv(a,b) (a==b)
# define long_double_less(a,b) (a<b)
# define long_double_less_or_eqv(a,b) (a<=b)
# define long_double_greater(a,b) (a>b)
# define long_double_greater_or_eqv(a,b) (a>=b)

# define long_double_eqv_i(a,b) ((long double)a == b)

# define long_double_is_zero(a) (a==0.0L)
# define long_double_is_1(a) (a == 1.0L)
# define long_double_minus_zero_p(a) ((1.0L/d) < 0.0L)
# define long_double_is_nan(a) (isnan(a))
# define long_double_is_pos_infinity(a) (isinf(a)&&(a>0))
# define long_double_is_neg_infinity(a) (isinf(a)&&(a<0))
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

# define long_double_from_string(x,y) strtold(x, y)

# define long_double_sprint(buffer,digits,d) sprintf(buffer, "%.*Lg", digits, d)

# define long_double_array_ref(pointer,index) ((long_double *)pointer)[index]
# define long_double_array_set(pointer,index,value) ((long_double *)pointer)[index] = value

#endif

#endif // LONGDOUBLE_H
