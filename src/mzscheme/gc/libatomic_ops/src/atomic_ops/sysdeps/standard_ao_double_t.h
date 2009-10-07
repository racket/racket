/* NEC LE-IT: For 64Bit OS we extend the double type to hold two int64's
*	
*  x86-64: __m128 serves as placeholder which also requires the compiler
*   	   to align	it on 16 byte boundary (as required by cmpxchg16.
* Similar things could be done for PowerPC 64bit using a VMX data type...	*/

#if defined(__GNUC__)
# if defined(__x86_64__)
# include<xmmintrin.h>
   typedef __m128 double_ptr_storage;
#  define AO_HAVE_DOUBLE_PTR_STORAGE
# endif /* __x86_64__ */
#endif

#ifdef _MSC_VER
# ifdef _WIN64
   typedef __m128 double_ptr_storage;
#  define AO_HAVE_DOUBLE_PTR_STORAGE
# elif _WIN32
   typedef unsigned __int64 double_ptr_storage;
#  define AO_HAVE_DOUBLE_PTR_STORAGE
# endif
#endif

#ifndef AO_HAVE_DOUBLE_PTR_STORAGE
   typedef unsigned long long double_ptr_storage;
#endif

typedef union {
    double_ptr_storage AO_whole;
    struct {AO_t AO_v1; AO_t AO_v2;} AO_parts;
} AO_double_t;

#define AO_HAVE_double_t
#define AO_val1 AO_parts.AO_v1
#define AO_val2 AO_parts.AO_v2
