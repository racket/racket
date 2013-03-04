#include "longdouble.h"
#include <math.h>
#include <stdio.h>
#include <string.h>

long_double get_long_double_infinity_val()
{
  long_double result;
  result.val = 1.0L / get_long_double_zero().val;
  return result;
}

long_double get_long_double_minus_infinity_val()
{
  long_double result;
  result.val = -get_long_double_infinity_val().val;
  return result;
}

long_double get_long_double_zero()
{
  long_double result;
  result.val = 0.0L;
  return result;
}

long_double get_long_double_nzero()
{
  long_double result;
  result.val = -1.0L / get_long_double_infinity_val().val;
  return result;
}

long_double get_long_double_nan()
{
  long_double result;
  result.val = get_long_double_infinity_val().val + get_long_double_minus_infinity_val().val;
  return result;
}

long_double get_long_double_1()
{
  long_double result;
  result.val = 1.0L;
  return result;
}
long_double get_long_double_minus_1()
{
  long_double result;
  result.val = -1.0L;
  return result;
}
long_double get_long_double_2()
{
  long_double result;
  result.val = 2.0L;
  return result;
}

long_double get_long_double_one_half()
{
  long_double result;
  result.val = 0.5L;
  return result;
}

long_double get_long_double_pi()
{
  long_double result;
  result.val = atan2l(0.0L, -1.0L);
  return result;
}

long_double get_long_double_half_pi()
{
  long_double result;
  result.val = atan2l(0.0L, -1.0L)/2.0L;
  return result;
}
long_double long_double_from_int(int a)
{
  long_double result;
  result.val = (long double) a;
  return result;
}


long_double long_double_from_float(float a)
{
  long_double result;
  result.val = (long double) a;
  return result;
}

long_double long_double_from_double(double a)
{
  long_double result;
  result.val = (long double) a;
  return result;
}

long_double long_double_from_uintptr(uintptr_t a)
{
  long_double result;
  result.val = a;
  return result;
}

double double_from_long_double(long_double a)
{
  return (double)a.val;
}

float float_from_long_double(long_double a)
{
  return (float)a.val;
}
intptr_t int_from_long_double(long_double a)
{
  return (intptr_t)a.val;
}

long_double long_double_plus(long_double a, long_double b)
{
  long_double result;
  result.val = a.val + b.val;
  return result;
}

long_double long_double_minus(long_double a, long_double b)
{
  long_double result;
  result.val = a.val - b.val;
  return result;
}

long_double long_double_mult(long_double a, long_double b)
{
  long_double result;
  result.val = a.val * b.val;
  return result;
}

long_double long_double_mult_i(long_double a, int b)
{
  long_double result;
  result.val = a.val * b;
  return result;
}

uintptr_t uintptr_from_long_double(long_double a)
{
  uintptr_t result;
  result = a.val;
  return result;
}

long_double long_double_div(long_double a, long_double b)
{
  long_double result;
  result.val = a.val / b.val;
  return result;
}
long_double long_double_neg(long_double a)
{
  long_double result;
  result.val = -a.val;
  return result;
}

int long_double_eqv(long_double a, long_double b)
{
  return a.val == b.val;
}
int long_double_less(long_double a, long_double b)
{
  return a.val < b.val;
}
int long_double_less_or_eqv(long_double a, long_double b)
{
  return a.val <= b.val;
}
int long_double_greater(long_double a, long_double b)
{
  return a.val > b.val;
}
int long_double_greater_or_eqv(long_double a, long_double b)
{
  return a.val >= b.val;
}

int long_double_eqv_i(int a, long_double b)
{
  return (long double) a == b.val;
}

int long_double_is_zero(long_double a)
{
  return a.val == 0.0L;
}

int long_double_is_1(long_double a)
{
  return a.val == 1.0L;
}

int long_double_minus_zero_p(long_double a)
{
  return (1.0L / a.val) < 0.0L;
}
int long_double_is_nan(long_double a)
{
  return isnan(a.val);
}
int long_double_is_pos_infinity(long_double a)
{
  return isinf(a.val) && a.val > 0;
}

int long_double_is_neg_infinity(long_double a)
{
  return isinf(a.val) && a.val < 0;
}

int long_double_is_infinity(long_double a)
{
  return isinf(a.val);
}

long_double long_double_fabs(long_double a)
{
  long_double result;
  result.val = fabsl(a.val);
  return result;
}

long_double long_double_modf(long_double a, long_double *b)
{
  long_double result;
  result.val = modfl(a.val, &b->val);
  return result;
}
long_double long_double_fmod(long_double a, long_double b)
{
  long_double result;
  result.val = fmodl(a.val, b.val);
  return result;
}
long_double long_double_trunc(long_double a)
{
  long_double result;
  result.val = truncl(a.val);
  return result;

}
long_double long_double_floor(long_double a)
{
  long_double result;
  result.val = floorl(a.val);
  return result;
}
long_double long_double_ceil(long_double a)
{
  long_double result;
  result.val = ceill(a.val);
  return result;
}

long_double long_double_sin(long_double a)
{
  long_double result;
  result.val = sinl(a.val);
  return result;
}
long_double long_double_cos(long_double a)
{
  long_double result;
  result.val = cosl(a.val);
  return result;
}
long_double long_double_tan(long_double a)
{
  long_double result;
  result.val = tanl(a.val);
  return result;
}
long_double long_double_asin(long_double a)
{
  long_double result;
  result.val = asinl(a.val);
  return result;
}
long_double long_double_acos(long_double a)
{
  long_double result;
  result.val = acosl(a.val);
  return result;
}
long_double long_double_atan(long_double a)
{
  long_double result;
  result.val = atanl(a.val);
  return result;
}
long_double long_double_log(long_double a)
{
  long_double result;
  result.val = logl(a.val);
  return result;
}
long_double long_double_exp(long_double a)
{
  long_double result;
  result.val = expl(a.val);
  return result;
}

long_double long_double_ldexp(long_double a, int i)
{
  long_double result;
  result.val = ldexpl(a.val, i);
  return result;
}

long_double long_double_pow(long_double a, long_double b)
{
  long_double result;
  result.val = powl(a.val, b.val);
  return result;
}

long_double long_double_sqrt(long_double a)
{
  long_double result;
  result.val = sqrtl(a.val);
  return result;
}

long_double long_double_frexp(long_double a, int* i)
{
  long_double result;
  result.val = frexpl(a.val, i);
  return result;
}

void long_double_sprint(char* buffer, int digits, long_double d)
{
  __mingw_sprintf(buffer, "%.*Lg", digits, d.val);
}

long_double long_double_array_ref(void *pointer, int index)
{
  long_double result;
  result = ((long_double *)pointer)[index];
  return result;
}

void long_double_array_set(void *pointer, int index, long_double value)
{
  ((long_double *)pointer)[index] = value;
  return ;
}


long_double long_double_from_string(char* buff, char** p)
{
  long_double result;
  char* ptr;
  __asm("nop");
  __mingw_sscanf(buff, "%Lf", &result.val);
  *p = &buff[strlen(buff)];
  return result;
}
