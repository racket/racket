#lang scribble/doc
@(require "utils.rkt"
          (for-label racket/unsafe/ops
                     ffi/unsafe))

@cs-title[#:tag "cs-values+types"]{Values and Types}

A Racket value is represented by a pointer-sized value. The low bits
of the value indicate the encoding that it uses. For example, two (on
32-bit platform) or three (on 64-bit platforms) low bits indicates a
fixnum encoding, while a one low bit and zero second-lowest bit
indicates a pair whose address in memory is specified by the other
bits.

The C type for a Racket value is @tt{ptr}. For most Racket types, a
constructor is provided for creating values of the type. For example,
@cpp{Scons} takes two @cpp{ptr} values and returns the @racket[cons]
of the values as a new @cpp{ptr} value. In addition to providing
constructors, Racket defines several global constant Racket values,
such as @cppi{Strue} for @racket[#t].

@; ----------------------------------------------------------------------

@section[#:tag "cs-constants"]{Global Constants}

There are six global constants:

@itemize[

 @item{@cppdef{Strue} --- @racket[#t]}

 @item{@cppdef{Sfalse} --- @racket[#f]}

 @item{@cppdef{Snil} --- @racket[null]}

 @item{@cppdef{Seof_object} --- @racket[eof-object]}

 @item{@cppdef{Svoid} --- @racket[(void)]}

]

@; ----------------------------------------------------------------------

@section[#:tag "cs-value-funcs"]{Value Functions}

Many of these functions are actually macros.

@(define-syntax-rule (predicates (name ...) desc ...)
   (together
     (@function[(int name [ptr v])] ...)
     desc ...))

@predicates[(Sfixnump
             Scharp
             Snullp
             Seof_objectp
             Sbooleanp
             Spairp
             Ssymbolp
             Sprocedurep
             Sflonump
             Svectorp
             Sfxvectorp
             Sbytevectorp
             Sstringp
             Sbignump
             Sboxp
             Sinexactnump
             Sexactnump
             Sratnump
             Srecordp)]{

Predicates to recognize different kinds of Racket values, such as
fixnums, characters, the empty list, etc. The @cpp{Srecordp} predicate
recognizes structures, but some built-in Racket datatypes are also
implemented as records.}

@function[(ptr Sfixnum [int i])]{

Returns a Racket integer value, where @var{i} must fit in a fixnum.}

@together[(
@function[(ptr Sinteger [iptr i])]
@function[(ptr Sunsigned [uptr i])]
@function[(ptr Sinteger32 [int i])]
@function[(ptr Sunsigned32 [unsigned-int i])]
@function[(ptr Sinteger64 [long i])]
@function[(ptr Sunsigned64 [unsigned-long i])]
)]{

Returns an integer value for different conversions from C, where the
result is allocated as a bignum if necessary to hold the value.}

@function[(iptr Sfixnum_value [ptr v])]{

Converts a Racket fixnum to a C integer.}

@together[(
@function[(iptr Sinteger_value [ptr v])]
@function[(uptr Sunsigned_value [ptr v])]
@function[(int Sinteger32_value [ptr v])]
@function[(long Sunsigned32_value [ptr v])]
@function[(long Sinteger64_value [ptr v])]
@function[(unsigned-long Sunsigned64_value [ptr v])]
)]{

Converts a Racket integer (possibly a bignum) to a C integer, assuming
that the integer fits in the return type.}

@function[(ptr Sflonum [double f])]{

Returns a Racket flonum value.}

@function[(double Sflonum_value [ptr v])]{

Converts a Racket flonum value to a C floating-point number.}


@function[(ptr Schar [int ch])]{

Returns a Racket character value. The @var{ch} value must be a legal
Unicode code point (and not a surrogate, for example). All characters
are represented by constant values.}


@function[(ptr Schar_value [ptr ch])]{

Returns the Unicode code point for the Racket character @var{ch}.}


@function[(ptr Sboolean [int bool])]{

Returns @cppi{Strue} or @cppi{Sfalse}.}


@function[(ptr Scons [ptr car] [ptr cdr])]{

Makes a @racket[cons] pair.}

@together[(
@function[(ptr Scar [ptr pr])]
@function[(ptr Scdr [ptr pr])]
)]{

Extracts the @racket[car] or @racket[cdr] of a pair.}

@function[(ptr Sstring_to_symbol [const-char* str])]{

Returns the interned symbol whose name matches @var{str}.}

@function[(ptr Ssymbol_to_string [ptr sym])]{

Returns the Racket immutable string value for the Racket symbol
@var{sym}.}

@together[(
@function[(ptr Smake_string [iptr len] [int ch])]
@function[(ptr Smake_uninitialized_string [iptr len])]
)]{

Allocates a fresh Racket mutable string with @var{len} characters. The
content of the string is either all @var{ch}s when @var{ch} is
provided or unspecified otherwise.}

@together[(
@function[(ptr Sstring [const-char* str])]
@function[(ptr Sstring_of_length [const-char* str] [iptr len])]
@function[(ptr Sstring_utf8 [const-char* str] [iptr len])]
)]{

Allocates a fresh Racket mutable string with the content of @var{str}.
If @var{len} is not provided, @var{str} must be nul-terminated.
In the case of @cppi{Sstring_utf8}, @var{str} is decoded as
UTF-8, otherwise it is decided as Latin-1.}


@function[(uptr Sstring_length [ptr str])]{

Returns the length of the string @var{str}.}

@function[(ptr Sstring_ref [ptr str] [uptr i])]{

Returns the @var{i}th Racket character of the string @var{str}.}

@function[(int Sstring_set [ptr str] [uptr i] [ptr ch])]{

Installs @var{ch} as the @var{i}th Racket character of the string @var{str}.}



@function[(ptr Smake_vector [iptr len] [ptr v])]{

Allocates a fresh mutable @tech[#:doc reference-doc]{vector} of length
@var{len} and with @var{v} initially in every slot.}


@function[(uptr Svector_length [ptr vec])]{

Returns the length of the vector @var{vec}.}


@function[(ptr Svector_ref [ptr vec] [uptr i])]{

Returns the @var{i}th element of the vector @var{vec}.}


@function[(void Svector_set [ptr vec] [uptr i] [ptr v])]{

Installs @var{v} as the @var{i}th element of the vector @var{vec}.}


@function[(ptr Smake_fxvector [iptr len] [ptr v])]{

Allocates a fresh mutable @tech[#:doc reference-doc]{fxvector} of
length @var{len} and with @var{v} initially in every slot.}


@function[(uptr Sfxvector_length [ptr vec])]{

Returns the length of the fxvector @var{vec}.}


@function[(iptr Sfxvector_ref [ptr vec] [uptr i])]{

Returns the @var{i}th fixnum of the fxvector @var{vec}.}


@function[(void Sfxvector_set [ptr vec] [uptr i] [ptr v])]{

Installs the fixnum @var{v} as the @var{i}th element of the fxvector
@var{vec}.}



@function[(ptr Smake_bytevector [iptr len] [int byte])]{

Allocates a fresh mutable @tech[#:doc reference-doc]{byte string} of
length @var{len} and with @var{byte} initially in every slot.}

@function[(uptr Sbytevector_length [ptr bstr])]{

Returns the length of the byte string @var{bstr}.}

@function[(int Sbytevector_u8_ref [ptr bstr] [uptr i])]{

Returns the @var{i}th byte of the byte string @var{bstr}.}

@function[(int Sbytevector_u8_set [ptr bstr] [uptr i] [int byte])]{

Installs @var{byte} as the @var{i}th byte of the byte string @var{bstr}.}

@function[(char* Sbytevector_data [ptr vec])]{

Returns a pointer to the start of the bytes for the byte string @var{bstr}.}


@function[(ptr Sbox [ptr v])]{

Allocates a fresh mutable @tech[#:doc reference-doc]{box} containing
@var{v}.}

@function[(ptr Sunbox [ptr bx])]{

Extract the content of the box @var{bx}.}

@function[(ptr Sset_box [ptr bx] [ptr v])]{

Installs @var{v} as the content of the box @var{bx}.}

@together[(
@function[(ptr Srecord_type [ptr rec])]
@function[(ptr Srecord_type_parent [ptr rtd])]
@function[(uptr Srecord_type_size [ptr rtd])]
@function[(int Srecord_type_uniformp [ptr rtd])]
@function[(ptr Srecord_uniform_ref [ptr rec][iptr i])]
)]{

Accesses record information, where Racket structures are implemented
as records. The @cpp{Srecord_type} returns a value representing a
record's type (so, a structure type). Given a record type,
@cpp{Srecord_type_parent} returns its supertype or @cpp{Sfalse},
@cpp{Srecord_type_size} returns the allocation size of a record in
bytes, and @cpp{Srecord_type_uniformp} indicates whether all of the
record fields are Scheme values --- which is always true for a Racket
structure. When a record has all Scheme-valued fields, the allocation
size is the number of fields plus one times the size of a pointer in
bytes.

When a record has all Scheme fields (which is the case for all Racket
structures), @cpp{Srecord_uniform_ref} accesses a field value in the
same way as @racket[unsafe-struct*-ref].}

@together[(
@function[(void* racket_cpointer_address [ptr cptr])]
@function[(void* racket_cpointer_base_address [ptr cptr])]
@function[(iptr racket_cpointer_offset [ptr cptr])]
)]{

Extracts an address and offset from a C-pointer object in the sense of
@racket[cpointer?], but only for values using the predefined representation
that is not a byte string, @racket[#f], or implemented by a new
structure type with @racket[prop:cpointer].

The result of @cpp{racket_cpointer_address} is the same as
@cpp{racket_cpointer_base_address} plus @cpp{racket_cpointer_offset},
where @cpp{racket_cpointer_offset} is non-zero for C-pointer values
created by @racket[ptr-add].}


@together[(
@function[(void Slock_object [ptr cptr])]
@function[(void Sunlock_object [ptr cptr])]
)]{

``Locks'' or ``unlocks'' n object, which prevents it from being
garbage collected or moved to a different address.

Lock objects sparingly, because the garbage collector is not designed
to deal with a large number of locked objects. To retain multiple
values from use from C, a good approach may be to allocate and lock a
vector that has a slot for each other (unlocked) object to retain.}
