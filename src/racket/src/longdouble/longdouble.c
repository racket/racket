
/* This file is really three implementations: the external API, glue
   for the external API, and glue for just the string external API. */

/**********************************************************************/
/* External long_double implementation                                */
/**********************************************************************/

#ifdef IMPLEMENTING_MSC_LONGDOUBLE

/* Implement the `long_double' API.
   This code is meant to be compiled with MinGW gcc
   to produce a DLL that is used by an MSVC-based
   build. For a 32-bit build, use gcc v4.7.0 or later,
   where the default handling of struct results matches
   MSVC. */

#include <stdint.h>
#include "longdouble.h"
#include <math.h>
#include <stdio.h>
#include <string.h>

LDBL_DLL_API void set_x87_control(int v)
{
  asm ("fldcw %0" : : "m" (v));
}

LDBL_DLL_API int get_x87_control()
{
  int v;
  asm ("fnstcw %0" : : "m" (v));
  return v;
}

static int ext_mode()
{
  int m = get_x87_control();
  set_x87_control(0x37F);
  return m;
}

static void restore_mode(int m)
{
  set_x87_control(m);
}

long_double get_long_double_infinity_val()
{
  long_double result;
  int m = ext_mode();
  result.val = 1.0L / get_long_double_zero().val;
  restore_mode(m);
  return result;
}

long_double get_long_double_minus_infinity_val()
{
  long_double result;
  int m = ext_mode();
  result.val = -get_long_double_infinity_val().val;
  restore_mode(m);
  return result;
}

long_double get_long_double_zero()
{
  long_double result;
  int m = ext_mode();
  result.val = 0.0L;
  restore_mode(m);
  return result;
}

long_double get_long_double_nzero()
{
  long_double result;
  int m = ext_mode();
  result.val = -1.0L / get_long_double_infinity_val().val;
  restore_mode(m);
  return result;
}

long_double get_long_double_nan()
{
  long_double result;
  int m = ext_mode();
  result.val = get_long_double_infinity_val().val + get_long_double_minus_infinity_val().val;
  restore_mode(m);
  return result;
}

long_double get_long_double_1()
{
  long_double result;
  int m = ext_mode();
  result.val = 1.0L;
  restore_mode(m);
  return result;
}
long_double get_long_double_minus_1()
{
  long_double result;
  int m = ext_mode();
  result.val = -1.0L;
  restore_mode(m);
  return result;
}
long_double get_long_double_2()
{
  long_double result;
  int m = ext_mode();
  result.val = 2.0L;
  restore_mode(m);
  return result;
}

long_double get_long_double_one_half()
{
  long_double result;
  int m = ext_mode();
  result.val = 0.5L;
  restore_mode(m);
  return result;
}

long_double get_long_double_pi()
{
  long_double result;
  int m = ext_mode();
  result.val = atan2l(0.0L, -1.0L);
  restore_mode(m);
  return result;
}

long_double get_long_double_half_pi()
{
  long_double result;
  int m = ext_mode();
  result.val = atan2l(0.0L, -1.0L)/2.0L;
  restore_mode(m);
  return result;
}
long_double long_double_from_int(int a)
{
  long_double result;
  int m = ext_mode();
  result.val = (long double) a;
  restore_mode(m);
  return result;
}


long_double long_double_from_float(float a)
{
  long_double result;
  int m = ext_mode();
  result.val = (long double) a;
  restore_mode(m);
  return result;
}

long_double long_double_from_double(double a)
{
  long_double result;
  int m = ext_mode();
  result.val = (long double) a;
  restore_mode(m);
  return result;
}

long_double long_double_from_uintptr(uintptr_t a)
{
  long_double result;
  int m = ext_mode();
  result.val = a;
  restore_mode(m);
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
  int m = ext_mode();
  result.val = a.val + b.val;
  restore_mode(m);
  return result;
}

long_double long_double_minus(long_double a, long_double b)
{
  long_double result;
  int m = ext_mode();
  result.val = a.val - b.val;
  restore_mode(m);
  return result;
}

long_double long_double_mult(long_double a, long_double b)
{
  long_double result;
  int m = ext_mode();
  result.val = a.val * b.val;
  restore_mode(m);
  return result;
}

long_double long_double_mult_i(long_double a, int b)
{
  long_double result;
  int m = ext_mode();
  result.val = a.val * b;
  restore_mode(m);
  return result;
}

uintptr_t uintptr_from_long_double(long_double a)
{
  uintptr_t result;
  int m = ext_mode();
  result = a.val;
  restore_mode(m);
  return result;
}

long_double long_double_div(long_double a, long_double b)
{
  long_double result;
  int m = ext_mode();
  result.val = a.val / b.val;
  restore_mode(m);
  return result;
}
long_double long_double_neg(long_double a)
{
  long_double result;
  int m = ext_mode();
  result.val = -a.val;
  restore_mode(m);
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
  int v;
  int m = ext_mode();
  v = ((1.0L / a.val) < 0.0L);
  restore_mode(m);
  return v;
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
  int m = ext_mode();
  result.val = fabsl(a.val);
  restore_mode(m);
  return result;
}

long_double long_double_modf(long_double a, long_double *b)
{
  long_double result;
  int m = ext_mode();
  result.val = modfl(a.val, &b->val);
  restore_mode(m);
  return result;
}
long_double long_double_fmod(long_double a, long_double b)
{
  long_double result;
  int m = ext_mode();
  result.val = fmodl(a.val, b.val);
  restore_mode(m);
  return result;
}
long_double long_double_trunc(long_double a)
{
  long_double result;
  int m = ext_mode();
  result.val = truncl(a.val);
  restore_mode(m);
  return result;

}
long_double long_double_floor(long_double a)
{
  long_double result;
  int m = ext_mode();
  result.val = floorl(a.val);
  restore_mode(m);
  return result;
}
long_double long_double_ceil(long_double a)
{
  long_double result;
  int m = ext_mode();
  result.val = ceill(a.val);
  restore_mode(m);
  return result;
}

long_double long_double_sin(long_double a)
{
  long_double result;
  int m = ext_mode();
  result.val = sinl(a.val);
  restore_mode(m);
  return result;
}
long_double long_double_cos(long_double a)
{
  long_double result;
  int m = ext_mode();
  result.val = cosl(a.val);
  restore_mode(m);
  return result;
}
long_double long_double_tan(long_double a)
{
  long_double result;
  int m = ext_mode();
  result.val = tanl(a.val);
  restore_mode(m);
  return result;
}
long_double long_double_asin(long_double a)
{
  long_double result;
  int m = ext_mode();
  result.val = asinl(a.val);
  restore_mode(m);
  return result;
}
long_double long_double_acos(long_double a)
{
  long_double result;
  int m = ext_mode();
  result.val = acosl(a.val);
  restore_mode(m);
  return result;
}
long_double long_double_atan(long_double a)
{
  long_double result;
  int m = ext_mode();
  result.val = atanl(a.val);
  restore_mode(m);
  return result;
}
long_double long_double_log(long_double a)
{
  long_double result;
  int m = ext_mode();
  result.val = logl(a.val);
  restore_mode(m);
  return result;
}
long_double long_double_exp(long_double a)
{
  long_double result;
  int m = ext_mode();
  result.val = expl(a.val);
  restore_mode(m);
  return result;
}

long_double long_double_ldexp(long_double a, int i)
{
  long_double result;
  int m = ext_mode();
  result.val = ldexpl(a.val, i);
  restore_mode(m);
  return result;
}

long_double long_double_pow(long_double a, long_double b)
{
  long_double result;
  int m = ext_mode();
  result.val = powl(a.val, b.val);
  restore_mode(m);
  return result;
}

long_double long_double_sqrt(long_double a)
{
  long_double result;
  int m = ext_mode();
  result.val = sqrtl(a.val);
  restore_mode(m);
  return result;
}

long_double long_double_frexp(long_double a, int* i)
{
  long_double result;
  int m = ext_mode();
  result.val = frexpl(a.val, i);
  restore_mode(m);
  return result;
}

void long_double_sprint(char* buffer, int digits, long_double d)
{
  int m = ext_mode();
  __mingw_sprintf(buffer, "%.*Lg", digits, d.val);
  restore_mode(m);
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
  char* ptr, one_char;
  int n;
  int m = ext_mode();
  n = __mingw_sscanf(buff, "%Lf%c", &result.val, &one_char);
  restore_mode(m);
  if (n == 1) {
    /* all characters consumed for the number */
    *p = &buff[strlen(buff)];
  } else {
    /* didn't use the input string exactly;
       pretend that no characters were consumed */
    *p = buff;
  }
  return result;
}

void long_double_from_string_indirect(char* buff, char** p, long_double *_ld)
{
  *_ld = long_double_from_string(buff, p);
}

#endif

/**********************************************************************/
/* Glue for external long_double implementation                       */
/**********************************************************************/

#ifdef MZ_LONG_DOUBLE_API_IS_EXTERNAL

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static int long_double_dll_available;

/* pointers to dynamically loaded functions */
#define DECLARE_LDBL(res, name, args) \
  typedef res (* name ## _t)args; \
  static name ## _t _imp_ ## name;
DECLARE_LDBL(long_double, get_long_double_infinity_val, ())
DECLARE_LDBL(long_double, get_long_double_minus_infinity_val, ())
DECLARE_LDBL(long_double, get_long_double_zero, ())
DECLARE_LDBL(long_double, get_long_double_nzero, ())
DECLARE_LDBL(long_double, get_long_double_nan, ())
DECLARE_LDBL(long_double, get_long_double_1, ())
DECLARE_LDBL(long_double, get_long_double_minus_1, ())
DECLARE_LDBL(long_double, get_long_double_2, ())
DECLARE_LDBL(long_double, get_long_double_one_half, ())
DECLARE_LDBL(long_double, get_long_double_pi, ())
DECLARE_LDBL(long_double, get_long_double_half_pi, ())
DECLARE_LDBL(void, set_long_double, (long_double a, long_double b))
DECLARE_LDBL(long_double, long_double_from_int, (int a))
DECLARE_LDBL(long_double, long_double_from_float, (float a))
DECLARE_LDBL(long_double, long_double_from_double, (double a))
DECLARE_LDBL(long_double, long_double_from_uintptr, (uintptr_t a))
DECLARE_LDBL(double, double_from_long_double, (long_double a))
DECLARE_LDBL(float, float_from_long_double, (long_double a))
DECLARE_LDBL(intptr_t, int_from_long_double, (long_double a))
DECLARE_LDBL(uintptr_t, uintptr_from_long_double, (long_double a))
DECLARE_LDBL(long_double, long_double_plus, (long_double a, long_double b))
DECLARE_LDBL(long_double, long_double_minus, (long_double a, long_double b))
DECLARE_LDBL(long_double, long_double_mult, (long_double a, long_double b))
DECLARE_LDBL(long_double, long_double_mult_i, (long_double a, int b))
DECLARE_LDBL(long_double, long_double_div, (long_double a, long_double b))
DECLARE_LDBL(long_double, long_double_neg, (long_double a))
DECLARE_LDBL(int, long_double_eqv, (long_double a, long_double b))
DECLARE_LDBL(int, long_double_less, (long_double a, long_double b))
DECLARE_LDBL(int, long_double_less_or_eqv, (long_double a, long_double b))
DECLARE_LDBL(int, long_double_greater, (long_double a, long_double b))
DECLARE_LDBL(int, long_double_greater_or_eqv, (long_double a, long_double b))
DECLARE_LDBL(int, long_double_eqv_i, (int a, long_double b))
DECLARE_LDBL(int, long_double_is_zero, (long_double a))
DECLARE_LDBL(int, long_double_is_1, (long_double a))
DECLARE_LDBL(int, long_double_minus_zero_p, (long_double a))
DECLARE_LDBL(int, long_double_is_nan, (long_double a))
DECLARE_LDBL(int, long_double_is_pos_infinity, (long_double a))
DECLARE_LDBL(int, long_double_is_neg_infinity, (long_double a))
DECLARE_LDBL(int, long_double_is_infinity, (long_double a))
DECLARE_LDBL(long_double, long_double_fabs, (long_double a))
DECLARE_LDBL(long_double, long_double_modf, (long_double a, long_double *b))
DECLARE_LDBL(long_double, long_double_fmod, (long_double a, long_double b))
DECLARE_LDBL(long_double, long_double_trunc, (long_double a))
DECLARE_LDBL(long_double, long_double_floor, (long_double a))
DECLARE_LDBL(long_double, long_double_ceil, (long_double a))
DECLARE_LDBL(long_double, long_double_sin, (long_double a))
DECLARE_LDBL(long_double, long_double_cos, (long_double a))
DECLARE_LDBL(long_double, long_double_tan, (long_double a))
DECLARE_LDBL(long_double, long_double_asin, (long_double a))
DECLARE_LDBL(long_double, long_double_acos, (long_double a))
DECLARE_LDBL(long_double, long_double_atan, (long_double a))
DECLARE_LDBL(long_double, long_double_log, (long_double a))
DECLARE_LDBL(long_double, long_double_exp, (long_double a))
DECLARE_LDBL(long_double, long_double_ldexp, (long_double a, int i))
DECLARE_LDBL(long_double, long_double_pow, (long_double a, long_double b))
DECLARE_LDBL(long_double, long_double_sqrt, (long_double a))
DECLARE_LDBL(long_double, long_double_frexp, (long_double a, int* i))
DECLARE_LDBL(void, long_double_sprint, (char* buffer, int digits, long_double d))
DECLARE_LDBL(long_double, long_double_array_ref, (void *pointer, int index))
DECLARE_LDBL(void, long_double_array_set, (void *pointer, int index, long_double value))
DECLARE_LDBL(long_double, long_double_from_string, (char* buff, char** p))
DECLARE_LDBL(void, set_x87_control, (int v))
DECLARE_LDBL(int, get_x87_control, ())

static long_double fail_long_double() {
  long_double d;
  memset(&d, 0, sizeof(d));
  return d;
}

static int fail_int() { return 0; }
static void fail_void() {  }
static double fail_double() { return 0.0; }
static float fail_float() { return 0.0; }
static uintptr_t fail_uintptr() { return 0; }

/* If "longdouble.dll" is not available, then fall back to `double'
   parsing and printing, so that we can at least implement reading
   and printing (which are supposed to always work). */

static long_double fail_from_string(char* buff, char** p)
{
  double d;
  long_double ld;

  d = strtod(buff, p);
  memcpy(&ld, &d, sizeof(double));

  return ld;
}

static void fail_sprint(char* buffer, int digits, long_double ld)
{
  double d;
  memcpy(&d, &ld, sizeof(double));
  sprintf(buffer, "%.*Lg", digits, d);
}

/* initialization */
void scheme_load_long_double_dll()
{
  HANDLE m;
  m = LoadLibraryW(scheme_get_dll_path(L"longdouble.dll"));

  if (m) long_double_dll_available = 1;

# define EXTRACT_LDBL(name, fail)                      \
  _imp_ ## name = (name ##_t)(m ? GetProcAddress(m, # name) : NULL);  \
  if (!(_imp_ ## name)) _imp_ ## name = (name ##_t)fail;

  EXTRACT_LDBL(get_long_double_infinity_val, fail_long_double);
  EXTRACT_LDBL(get_long_double_minus_infinity_val, fail_long_double);
  EXTRACT_LDBL(get_long_double_zero, fail_long_double);
  EXTRACT_LDBL(get_long_double_nzero, fail_long_double);
  EXTRACT_LDBL(get_long_double_nan, fail_long_double);
  EXTRACT_LDBL(get_long_double_1, fail_long_double);
  EXTRACT_LDBL(get_long_double_minus_1, fail_long_double);
  EXTRACT_LDBL(get_long_double_2, fail_long_double);
  EXTRACT_LDBL(get_long_double_one_half, fail_long_double);
  EXTRACT_LDBL(get_long_double_pi, fail_long_double);
  EXTRACT_LDBL(get_long_double_half_pi, fail_long_double);
  EXTRACT_LDBL(set_long_double, fail_void);
  EXTRACT_LDBL(long_double_from_int, fail_long_double);
  EXTRACT_LDBL(long_double_from_float, fail_long_double);
  EXTRACT_LDBL(long_double_from_double, fail_long_double);
  EXTRACT_LDBL(long_double_from_uintptr, fail_long_double);
  EXTRACT_LDBL(double_from_long_double, fail_double);
  EXTRACT_LDBL(float_from_long_double, fail_float);
  EXTRACT_LDBL(int_from_long_double, fail_int);
  EXTRACT_LDBL(uintptr_from_long_double, fail_uintptr);
  EXTRACT_LDBL(long_double_plus, fail_long_double);
  EXTRACT_LDBL(long_double_minus, fail_long_double);
  EXTRACT_LDBL(long_double_mult, fail_long_double);
  EXTRACT_LDBL(long_double_mult_i, fail_long_double);
  EXTRACT_LDBL(long_double_div, fail_long_double);
  EXTRACT_LDBL(long_double_neg, fail_long_double);
  EXTRACT_LDBL(long_double_eqv, fail_int);
  EXTRACT_LDBL(long_double_less, fail_int);
  EXTRACT_LDBL(long_double_less_or_eqv, fail_int);
  EXTRACT_LDBL(long_double_greater, fail_int);
  EXTRACT_LDBL(long_double_greater_or_eqv, fail_int);
  EXTRACT_LDBL(long_double_eqv_i, fail_int);
  EXTRACT_LDBL(long_double_is_zero, fail_int);
  EXTRACT_LDBL(long_double_is_1, fail_int);
  EXTRACT_LDBL(long_double_minus_zero_p, fail_int);
  EXTRACT_LDBL(long_double_is_nan, fail_int);
  EXTRACT_LDBL(long_double_is_pos_infinity, fail_int);
  EXTRACT_LDBL(long_double_is_neg_infinity, fail_int);
  EXTRACT_LDBL(long_double_is_infinity, fail_int);
  EXTRACT_LDBL(long_double_fabs, fail_long_double);
  EXTRACT_LDBL(long_double_modf, fail_long_double);
  EXTRACT_LDBL(long_double_fmod, fail_long_double);
  EXTRACT_LDBL(long_double_trunc, fail_long_double);
  EXTRACT_LDBL(long_double_floor, fail_long_double);
  EXTRACT_LDBL(long_double_ceil, fail_long_double);
  EXTRACT_LDBL(long_double_sin, fail_long_double);
  EXTRACT_LDBL(long_double_cos, fail_long_double);
  EXTRACT_LDBL(long_double_tan, fail_long_double);
  EXTRACT_LDBL(long_double_asin, fail_long_double);
  EXTRACT_LDBL(long_double_acos, fail_long_double);
  EXTRACT_LDBL(long_double_atan, fail_long_double);
  EXTRACT_LDBL(long_double_log, fail_long_double);
  EXTRACT_LDBL(long_double_exp, fail_long_double);
  EXTRACT_LDBL(long_double_ldexp, fail_long_double);
  EXTRACT_LDBL(long_double_pow, fail_long_double);
  EXTRACT_LDBL(long_double_sqrt, fail_long_double);
  EXTRACT_LDBL(long_double_frexp, fail_long_double);
  EXTRACT_LDBL(long_double_sprint, fail_sprint);
  EXTRACT_LDBL(long_double_array_ref, fail_long_double);
  EXTRACT_LDBL(long_double_array_set, fail_void);
  EXTRACT_LDBL(long_double_from_string, fail_from_string);
  EXTRACT_LDBL(set_x87_control, fail_void);
  EXTRACT_LDBL(get_x87_control, fail_int);
}

int long_double_available() {
  return long_double_dll_available;
}

/* Glue functions */

long_double get_long_double_infinity_val() { return _imp_get_long_double_infinity_val(); }
long_double get_long_double_minus_infinity_val() { return _imp_get_long_double_minus_infinity_val(); }
long_double get_long_double_zero() { return _imp_get_long_double_zero(); }
long_double get_long_double_nzero() { return _imp_get_long_double_nzero(); }
long_double get_long_double_nan() { return _imp_get_long_double_nan(); }
long_double get_long_double_1() { return _imp_get_long_double_1(); }
long_double get_long_double_minus_1() { return _imp_get_long_double_minus_1(); }
long_double get_long_double_2() { return _imp_get_long_double_2(); }
long_double get_long_double_one_half() { return _imp_get_long_double_one_half(); }
long_double get_long_double_pi() { return _imp_get_long_double_pi(); }
long_double get_long_double_half_pi() { return _imp_get_long_double_half_pi(); }

void set_long_double(long_double a, long_double b) { _imp_set_long_double(a, b); }

long_double long_double_from_int(int a) { return _imp_long_double_from_int(a); }
long_double long_double_from_float(float a) { return _imp_long_double_from_float(a); }
long_double long_double_from_double(double a) { return _imp_long_double_from_double(a); }
long_double long_double_from_uintptr(uintptr_t a) { return _imp_long_double_from_uintptr(a); }

double double_from_long_double(long_double a) { return _imp_double_from_long_double(a); }
float float_from_long_double(long_double a) { return _imp_float_from_long_double(a); }
intptr_t int_from_long_double(long_double a) { return _imp_int_from_long_double(a); }

uintptr_t uintptr_from_long_double(long_double a) { return _imp_uintptr_from_long_double(a); }

long_double long_double_plus(long_double a, long_double b) { return _imp_long_double_plus(a, b); }
long_double long_double_minus(long_double a, long_double b) { return _imp_long_double_minus(a, b); }
long_double long_double_mult(long_double a, long_double b) { return _imp_long_double_mult(a, b); }
long_double long_double_mult_i(long_double a, int b) { return _imp_long_double_mult_i(a, b); }
long_double long_double_div(long_double a, long_double b) { return _imp_long_double_div(a, b); }
long_double long_double_neg(long_double a) { return _imp_long_double_neg(a); }

int long_double_eqv(long_double a, long_double b) { return _imp_long_double_eqv(a, b); }
int long_double_less(long_double a, long_double b) { return _imp_long_double_less(a, b); }
int long_double_less_or_eqv(long_double a, long_double b) { return _imp_long_double_less_or_eqv(a, b); }
int long_double_greater(long_double a, long_double b) { return _imp_long_double_greater(a, b); }
int long_double_greater_or_eqv(long_double a, long_double b) { return _imp_long_double_greater_or_eqv(a, b); }

int long_double_eqv_i(int a, long_double b) { return _imp_long_double_eqv_i(a, b); }

int long_double_is_zero(long_double a) { return _imp_long_double_is_zero(a); }
int long_double_is_1(long_double a) { return _imp_long_double_is_1(a); }
int long_double_minus_zero_p(long_double a) { return _imp_long_double_minus_zero_p(a); }
int long_double_is_nan(long_double a) { return _imp_long_double_is_nan(a); }
int long_double_is_pos_infinity(long_double a) { return _imp_long_double_is_pos_infinity(a); }
int long_double_is_neg_infinity(long_double a) { return _imp_long_double_is_neg_infinity(a); }
int long_double_is_infinity(long_double a) { return _imp_long_double_is_infinity(a); }

long_double long_double_fabs(long_double a) { return _imp_long_double_fabs(a); }
long_double long_double_modf(long_double a, long_double *b) { return _imp_long_double_modf(a, b); }
long_double long_double_fmod(long_double a, long_double b) { return _imp_long_double_fmod(a, b); }
long_double long_double_trunc(long_double a) { return _imp_long_double_trunc(a); }
long_double long_double_floor(long_double a) { return _imp_long_double_floor(a); }
long_double long_double_ceil(long_double a) { return _imp_long_double_ceil(a); }

long_double long_double_sin(long_double a) { return _imp_long_double_sin(a); }
long_double long_double_cos(long_double a) { return _imp_long_double_cos(a); }
long_double long_double_tan(long_double a) { return _imp_long_double_tan(a); }
long_double long_double_asin(long_double a) { return _imp_long_double_asin(a); }
long_double long_double_acos(long_double a) { return _imp_long_double_acos(a); }
long_double long_double_atan(long_double a) { return _imp_long_double_atan(a); }
long_double long_double_log(long_double a) { return _imp_long_double_log(a); }
long_double long_double_exp(long_double a) { return _imp_long_double_exp(a); }
long_double long_double_ldexp(long_double a, int i) { return _imp_long_double_ldexp(a, i); }

long_double long_double_pow(long_double a, long_double b) { return _imp_long_double_pow(a, b); }

long_double long_double_sqrt(long_double a) { return _imp_long_double_sqrt(a); }

long_double long_double_frexp(long_double a, int* i) { return _imp_long_double_frexp(a, i); }

void long_double_sprint(char* buffer, int digits, long_double d) { _imp_long_double_sprint(buffer, digits, d); }

long_double long_double_array_ref(void *pointer, int index) { return _imp_long_double_array_ref(pointer, index); }
void long_double_array_set(void *pointer, int index, long_double value) { _imp_long_double_array_set(pointer, index, value); }

long_double long_double_from_string(char* buff, char** p) { return _imp_long_double_from_string(buff, p); }

void to_double_prec() { _imp_set_x87_control(0x27F); }
void to_extended_prec() { _imp_set_x87_control(0x37F); }

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

#endif

/**********************************************************************/
/* Glue for external long_double string-op implementation             */
/**********************************************************************/

#ifdef LONG_DOUBLE_STRING_OP_API_IS_EXTERNAL

/* Like regular glue mode, but only for the string operations.

   It may seem strage to resort to a MinGW-compiled DLL to implement
   functionality when compiling with MinGW, but the MinGW version has
   to be recent enough to get __mingw_sscanf, so we do things this
   way to allow building with older MinGWs (such as the default MinGW
   release at the time of writing). */

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static int long_double_dll_available;

typedef union dll_long_double 
{
  char bytes[SIZEOF_LONGDOUBLE];
  long double val;
} dll_long_double;

/* pointers to dynamically loaded functions */
#define DECLARE_LDBL(res, name, args) \
  typedef res (* name ## _t)args; \
  static name ## _t _imp_ ## name;
DECLARE_LDBL(void, long_double_sprint, (char* buffer, int digits, dll_long_double ld))
DECLARE_LDBL(void, long_double_from_string_indirect, (char* buff, char** p, dll_long_double *_ld))

static void fail_from_string_indirect(char* buff, char** p, dll_long_double *_ld)
{
  double d;

  d = strtod(buff, p);
  memcpy(_ld, &d, sizeof(double));
}

static void fail_sprint(char* buffer, int digits, dll_long_double ld)
{
  double d;
  memcpy(&d, &ld, sizeof(double));
  sprintf(buffer, "%.*Lg", digits, d);
}

/* initialization */
void scheme_load_long_double_dll()
{
  HANDLE m;
  m = LoadLibraryW(scheme_get_dll_path(L"longdouble.dll"));

  if (m) long_double_dll_available = 1;

# define EXTRACT_LDBL(name, fail)                      \
  _imp_ ## name = (name ##_t)(m ? GetProcAddress(m, # name) : NULL);  \
  if (!(_imp_ ## name)) _imp_ ## name = (name ##_t)fail;

  EXTRACT_LDBL(long_double_sprint, fail_sprint);
  EXTRACT_LDBL(long_double_from_string_indirect, fail_from_string_indirect);
}

int long_double_available() {
  return long_double_dll_available;
}

void long_double_sprint(char* buffer, int digits, long double d) { 
  dll_long_double ld;
  ld.val = d;
  _imp_long_double_sprint(buffer, digits, ld); 
}

long double long_double_from_string(char* buff, char** p) { 
  dll_long_double ld;
  _imp_long_double_from_string_indirect(buff, p, &ld); 
  return ld.val;
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

#endif
