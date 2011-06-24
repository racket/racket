#lang scribble/doc
@(require "utils.rkt")

@title{Bignums, Rationals, and Complex Numbers}

Racket supports integers of an arbitrary magnitude; when an integer
cannot be represented as a fixnum (i.e., 30 or 62 bits plus a sign
bit), then it is represented by the Racket type
@cppi{scheme_bignum_type}. There is no overlap in integer values
represented by fixnums and bignums.

Rationals are implemented by the type @cppi{scheme_rational_type},
composed of a numerator and a denominator.
The numerator and denominator fixnums or bignums (possibly mixed).

Complex numbers are implemented by the types
@cppi{scheme_complex_type} and @cppi{scheme_complex_izi_type},
composed of a real and imaginary part. The real and imaginary parts
will either be both flonums, both exact numbers (fixnums, bignums, and
rationals can be mixed in any way), or one part will be exact 0 and
the other part will be a flonum. If the inexact part is inexact 0, the
type is @cpp{scheme_complex_izi_type}, otherwise the type is
@cppi{scheme_complex_type}; this distinction make it easy to test
whether a complex number should be treated as a real number.


@function[(int scheme_is_exact
           [Scheme_Object* n])]{

Returns @cpp{1} if @var{n} is an exact number, @racket[0] otherwise
(@var{n} need not be a number).}

@function[(int scheme_is_inexact
           [Scheme_Object* n])]{

Returns @cpp{1} if @var{n} is an inexact number, @racket[0] otherwise
(@var{n} need not be a number).}

@function[(Scheme_Object* scheme_make_bignum
           [intptr_t v])]{

Creates a bignum representing the integer @var{v}. This can create a
bignum that otherwise fits into a fixnum.  This must only be used to
create temporary values for use with the @cpp{bignum} functions. Final
results can be normalized with @cpp{scheme_bignum_normalize}.  Only
normalized numbers can be used with procedures that are not specific
to bignums.}

@function[(Scheme_Object* scheme_make_bignum_from_unsigned
           [uintptr_t v])]{

Like @cpp{scheme_make_bignum}, but works on unsigned integers.}

@function[(double scheme_bignum_to_double
           [Scheme_Object* n])]{

Converts a bignum to a floating-point number, with reasonable but
unspecified accuracy.}

@function[(float scheme_bignum_to_float
           [Scheme_Object* n])]{

If Racket is not compiled with single-precision floats, this procedure
is actually a macro alias for @cpp{scheme_bignum_to_double}.}

@function[(Scheme_Object* scheme_bignum_from_double
           [double d])]{

Creates a bignum that is close in magnitude to the floating-point
number @var{d}. The conversion accuracy is reasonable but unspecified.}

@function[(Scheme_Object* scheme_bignum_from_float
           [float f])]{

If Racket is not compiled with single-precision floats, this procedure
is actually a macro alias for @cpp{scheme_bignum_from_double}.}

@function[(char* scheme_bignum_to_string
           [Scheme_Object* n]
           [int radix])]{

Writes a bignum into a newly allocated byte string.}

@function[(Scheme_Object* scheme_read_bignum
           [mzchar* str]
           [int offset]
           [int radix])]{

Reads a bignum from a @cpp{mzchar} string, starting from position
 @var{offset} in @var{str}. If the string does not represent an
 integer, then @cpp{NULL} will be returned. If the string represents a
 number that fits in 31 bits, then a @cpp{scheme_integer_type}
 object will be returned.}

@function[(Scheme_Object* scheme_read_bignum_bytes
           [char* str]
           [int offset]
           [int radix])]{

Like @cpp{scheme_read_bignum}, but from a UTF-8-encoding byte string.}

@function[(Scheme_Object* scheme_bignum_normalize
           [Scheme_Object* n])]{

If @var{n} fits in 31 bits, then a @cpp{scheme_integer_type} object
 will be returned. Otherwise, @var{n} is returned.}

@function[(Scheme_Object* scheme_make_rational
           [Scheme_Object* n]
           [Scheme_Object* d])]{

Creates a rational from a numerator and denominator. The @var{n} and
@var{d} parameters must be fixnums or bignums (possibly mixed). The
resulting will be normalized (thus, a bignum or fixnum might be returned).}

@function[(double scheme_rational_to_double
           [Scheme_Object* n])]{

Converts the rational @var{n} to a @cpp{double}.}

@function[(float scheme_rational_to_float
           [Scheme_Object* n])]{

If Racket is not compiled with single-precision floats, this procedure
is actually a macro alias for @cpp{scheme_rational_to_double}.}

@function[(Scheme_Object* scheme_rational_numerator
           [Scheme_Object* n])]{

Returns the numerator of the rational @var{n}.}

@function[(Scheme_Object* scheme_rational_denominator
           [Scheme_Object* n])]{

Returns the denominator of the rational @var{n}.}

@function[(Scheme_Object* scheme_rational_from_double
           [double d])]{

Converts the given @cpp{double} into a maximally-precise rational.}

@function[(Scheme_Object* scheme_rational_from_float
           [float d])]{

If Racket is not compiled with single-precision floats, this procedure
is actually a macro alias for @cpp{scheme_rational_from_double}.}

@function[(Scheme_Object* scheme_make_complex
           [Scheme_Object* r]
           [Scheme_Object* i])]{

Creates a complex number from real and imaginary parts. The @var{r}
and @var{i} arguments must be fixnums, bignums, flonums, or rationals
(possibly mixed). The resulting number will be normalized (thus, a real
number might be returned).}

@function[(Scheme_Object* scheme_complex_real_part
           [Scheme_Object* n])]{

Returns the real part of the complex number @var{n}.}

@function[(Scheme_Object* scheme_complex_imaginary_part
           [Scheme_Object* n])]{

Returns the imaginary part of the complex number @var{n}.}
