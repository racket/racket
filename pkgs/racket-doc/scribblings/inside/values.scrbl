#lang scribble/doc
@(require "utils.rkt")

@title[#:tag "im:values+types"]{Values and Types}

A Racket value is represented by a pointer-sized value. The low bit is
a mark bit: a 1 in the low bit indicates an immediate integer, a 0
indicates a (word-aligned) pointer.

A pointer Racket value references a structure that begins with a
@cppi{Scheme_Object} sub-structure, which in turn starts with a tag
that has the C type @cppi{Scheme_Type}. The rest of the structure,
following the @cppi{Scheme_Object} header, is type-dependent.
Racket's C interface gives Racket values the type
@cpp{Scheme_Object*}. (The ``object'' here does not refer to objects
in the sense of the @racketmodname[racket/class] library.)

Examples of @cpp{Scheme_Type} values include @cpp{scheme_pair_type}
and @cpp{scheme_symbol_type}. Some of these are implemented as
instances of @cppi{Scheme_Simple_Object}, which is defined in
@filepath{scheme.h}, but extension or embedding code should never access
this structure directly. Instead, the code should use macros, such as
@cpp{SCHEME_CAR}, that provide access to the data of common Racket
types.

For most Racket types, a constructor is provided for creating values
of the type. For example, @cpp{scheme_make_pair} takes two
@cpp{Scheme_Object*} values and returns the @racket[cons] of the
values.

The macro @cppdef{SCHEME_TYPE} takes a @cpp{Scheme_Object *} and returns
the type of the object. This macro performs the tag-bit check, and
returns @cppi{scheme_integer_type} when the value is an immediate
integer; otherwise, @cpp{SCHEME_TYPE} follows the pointer to get the
type tag. Macros are provided to test for common Racket types; for
example, @cpp{SCHEME_PAIRP} returns @cpp{1} if the value is a cons
cell, @cpp{0} otherwise.

In addition to providing constructors, Racket defines six global
constant Racket values: @cppi{scheme_true}, @cppi{scheme_false},
@cppi{scheme_null}, @cppi{scheme_eof}, @cppi{scheme_void}, and
@cppi{scheme_undefined}. Each of these has a type tag, but each is
normally recognized via its constant address.

@index['("types" "creating")]{An} extension or embedding application
can create new a primitive data type by calling
@cppi{scheme_make_type}, which returns a fresh @cpp{Scheme_Type}
value. To create a collectable instance of this type, allocate memory
for the instance with @cpp{scheme_malloc_atomic}. From Racket's
perspective, the main constraint on the data format of such an
instance is that the first @cpp{sizeof(Scheme_Object)} bytes must
correspond to a @cpp{Scheme_Object} record; furthermore, the first
@cpp{sizeof(Scheme_Type)} bytes must contain the value returned by
@cpp{scheme_make_type}. Extensions with modest needs can use
@cppi{scheme_make_cptr}, instead of creating an entirely new type.

Racket values should never be allocated on the stack, and they should
never contain pointers to values on the stack. Besides the problem of
restricting the value's lifetime to that of the stack frame,
allocating values on the stack creates problems for continuations and
threads, both of which copy into and out of the stack.

@; ----------------------------------------------------------------------

@section[#:tag "im:stdtypes"]{Standard Types}

The following are the @cpp{Scheme_Type} values for the standard
types:

@itemize[

 @item{@cppdef{scheme_bool_type} --- the constants
 @cpp{scheme_true} and @cpp{scheme_false} are the only values of this
 type; use @cpp{SCHEME_FALSEP} to recognize @cpp{scheme_false} and use
 @cpp{SCHEME_TRUEP} to recognize anything except @cpp{scheme_false};
 test for this type with @cppdef{SCHEME_BOOLP}}

 @item{@cppdef{scheme_char_type} --- @cppdef{SCHEME_CHAR_VAL}
 extracts the character (of type @cppi{mzchar}); test for this type
 with @cppdef{SCHEME_CHARP}}

 @item{@cppdef{scheme_integer_type} --- fixnum integers, which are
 identified via the tag bit rather than following a pointer to this
 @cpp{Scheme_Type} value; @cppdef{SCHEME_INT_VAL} extracts the integer 
 to an @cpp{intptr_t}; test for this type with @cppdef{SCHEME_INTP}}

 @item{@cppdef{scheme_double_type} --- flonum inexact numbers;
 @cppdef{SCHEME_FLOAT_VAL} or @cppdef{SCHEME_DBL_VAL} extracts the
 floating-point value; test for this type with @cppdef{SCHEME_DBLP}}

 @item{@cppdef{scheme_float_type} --- single-precision flonum
 inexact numbers, when specifically enabled when compiling Racket;
 @cppi{SCHEME_FLOAT_VAL} or @cppdef{SCHEME_FLT_VAL} extracts the
 floating-point value; test for this type with @cppdef{SCHEME_FLTP}}

 @item{@cppdef{scheme_bignum_type} --- test for this type with
 @cppdef{SCHEME_BIGNUMP}}
 
 @item{@cppdef{scheme_rational_type} --- test for this type with
 @cppdef{SCHEME_RATIONALP}}

 @item{@cppdef{scheme_complex_type} --- test for this type or
 @cpp{scheme_complex_izi_type} with @cppdef{SCHEME_COMPLEXP}}

 @item{@cppdef{scheme_complex_izi_type} --- complex number with an inexact
 zero imaginary part (so it counts as a real number); test for this
 type specifically with @cppdef{SCHEME_COMPLEX_IZIP}}

 @item{@cppdef{scheme_char_string_type} --- @index['("strings"
 "conversion to C")]{@cppdef{SCHEME_CHAR_STR_VAL}} extracts the string
 as a @cpp{mzchar*}; the string is always nul-terminated, but may also
 contain embedded nul characters, and the Racket string is modified if
 this string is modified; @cppdef{SCHEME_CHAR_STRLEN_VAL} extracts the
 string length (in characters, not counting the nul terminator); test
 for this type with @cppdef{SCHEME_CHAR_STRINGP}}

 @item{@cppdef{scheme_byte_string_type} ---
 @cppdef{SCHEME_BYTE_STR_VAL} extracts the string as a @cpp{char*}; the
 string is always nul-terminated, but may also contain embedded nul
 characters, and the Racket string is modified if this string is
 modified; @cppdef{SCHEME_BYTE_STRLEN_VAL} extracts the string length
 (in bytes, not counting the nul terminator); test for this type with
 @cppdef{SCHEME_BYTE_STRINGP}}

 @item{@cppdef{scheme_path_type} --- 
 @index['("strings" "conversion to C")] @cppdef{SCHEME_PATH_VAL}
 extracts the path as a @cpp{char*}; the string is always
 nul-terminated; @cppdef{SCHEME_PATH_LEN} extracts the path length (in
 bytes, not counting the nul terminator); test for this type with
 @cppdef{SCHEME_PATHP}}

 @item{@cppdef{scheme_symbol_type} --- @cppdef{SCHEME_SYM_VAL}
 extracts the symbol's string as a @cpp{char*} UTF-8 encoding (do not
 modify this string); @cppdef{SCHEME_SYM_LEN} extracts the number of
 bytes in the symbol name (not counting the nul terminator); test for
 this type with @cppdef{SCHEME_SYMBOLP}; 3m: see @secref["im:3m"] for
 a caution about @cppi{SCHEME_SYM_VAL}}

 @item{@cppdef{scheme_keyword_type} --- @cppdef{SCHEME_KEYWORD_VAL}
 extracts the keyword's string (without the leading hash colon) as a
 @cpp{char*} UTF-8 encoding (do not modify this string);
 @cppdef{SCHEME_KEYWORD_LEN} extracts the number of bytes in the keyword
 name (not counting the nul terminator); test for this type with
 @cppdef{SCHEME_KEYWORDP}; 3m: see @secref["im:3m"] for a caution
 about @cppi{SCHEME_KEYWORD_VAL}}

 @item{@cppdef{scheme_box_type} --- @cppdef{SCHEME_BOX_VAL}
 extracts/sets the boxed value; test for this type with
 @cppdef{SCHEME_BOXP}}

 @item{@cppdef{scheme_pair_type} --- @cppdef{SCHEME_CAR} extracts/sets
 the @racket[car] and @cppdef{SCHEME_CDR} extracts/sets the
 @racket[cdr]; test for this type with @cppdef{SCHEME_PAIRP}}

 @item{@cppdef{scheme_mutable_pair_type} --- @cppdef{SCHEME_MCAR} extracts/sets
 the @racket[mcar] and @cppdef{SCHEME_MCDR} extracts/sets the
 @racket[mcdr]; test for this type with @cppdef{SCHEME_MPAIRP}}

 @item{@cppdef{scheme_vector_type} --- @cppdef{SCHEME_VEC_SIZE}
 extracts the length and @cppdef{SCHEME_VEC_ELS} extracts the array of
 Racket values (the Racket vector is modified when this array is
 modified); test for this type with @cppdef{SCHEME_VECTORP}; 3m: see
 @secref["im:3m"] for a caution about @cppi{SCHEME_VEC_ELS}}

 @item{@cppdef{scheme_flvector_type} --- @cppdef{SCHEME_FLVEC_SIZE}
 extracts the length and @cppdef{SCHEME_FLVEC_ELS} extracts the array of
 @cpp{double}s; test for this type with @cppdef{SCHEME_FLVECTORP}; 3m: see
 @secref["im:3m"] for a caution about @cppi{SCHEME_FLVEC_ELS}}

 @item{@cppdef{scheme_fxvector_type} --- uses the same representation
 as @cpp{scheme_vector_type}, so use @cpp{SCHEME_VEC_SIZE}
 for the length and @cpp{SCHEME_VEC_ELS} for the array of
 Racket fixnum values; test for this type with @cppdef{SCHEME_FXVECTORP}; 3m: see
 @secref["im:3m"] for a caution about @cppi{SCHEME_VEC_ELS}}

 @item{@cppdef{scheme_structure_type} --- structure instances; test
 for this type with @cppdef{SCHEME_STRUCTP}}

 @item{@cppdef{scheme_struct_type_type} --- structure types; test for
 this type with @cppdef{SCHEME_STRUCT_TYPEP}}

 @item{@cppdef{scheme_struct_property_type} --- structure type
 properties}

 @item{@cppdef{scheme_input_port_type} --- @cppdef{SCHEME_INPORT_VAL}
 extracts/sets the user data pointer; test for just this type with
 @cppdef{SCHEME_INPORTP}, but use @cppdef{SCHEME_INPUT_PORTP} to recognize
 all input ports (including structures with the
 @racket[prop:input-port] property)}

 @item{@cppdef{scheme_output_port_type} --- @cppdef{SCHEME_OUTPORT_VAL}
 extracts/sets the user data pointer; test for just this type with
 @cppdef{SCHEME_OUTPORTP}, but use @cppdef{SCHEME_OUTPUT_PORTP} to
 recognize all output ports (including structures with the
 @racket[prop:output-port] property)}

 @item{@cppdef{scheme_thread_type} --- thread descriptors; test for
 this type with @cppdef{SCHEME_THREADP}}

 @item{@cppdef{scheme_sema_type} --- semaphores; test for this type
 with @cppdef{SCHEME_SEMAP}}

 @item{@cppdef{scheme_hash_table_type} --- test for this type with
 @cppdef{SCHEME_HASHTP}}

 @item{@cppdef{scheme_bucket_table_type} --- test for this type with
 @cppdef{SCHEME_BUCKTP}}

 @item{@cppdef{scheme_weak_box_type} --- test for this type with
 @cppdef{SCHEME_WEAKP}; @cppdef{SCHEME_WEAK_PTR} extracts the contained
 object, or @cpp{NULL} after the content is collected; do not set the
 content of a weak box}

 @item{@cppdef{scheme_namespace_type} --- namespaces; test for this
 type with @cppdef{SCHEME_NAMESPACEP}}

 @item{@cppdef{scheme_cpointer_type} --- @|void-const| pointer with a
 type-describing @cpp{Scheme_Object}; @cppdef{SCHEME_CPTR_VAL} extracts
 the pointer and @cppdef{SCHEME_CPTR_TYPE} extracts the type tag object;
 test for this type with @cppdef{SCHEME_CPTRP}.  The tag is used when
 printing such objects when it's a symbol, a byte string, a string, or
 a pair holding one of these in its car.}

]

The following are the procedure types:

@itemize[

 @item{@cppdef{scheme_prim_type} --- a primitive procedure,
 possibly with data elements}

 @item{@cppdef{scheme_closed_prim_type} --- an old-style primitive
 procedure with a data pointer}

 @item{@cppdef{scheme_compiled_closure_type} --- a Racket
 procedure}

 @item{@cppdef{scheme_cont_type} --- a continuation}

 @item{@cppdef{scheme_escaping_cont_type} --- an escape continuation}

 @item{@cppdef{scheme_case_closure_type} --- a @racket[case-lambda]
 procedure}

 @item{@cppdef{scheme_native_closure_type} --- a procedure with
 native code generated by the just-in-time compiler}

]

The predicate @cppdef{SCHEME_PROCP} returns 1 for all procedure types
 and 0 for anything else.

The following are additional number predicates:

@itemize[

 @item{@cppdef{SCHEME_NUMBERP} --- all numerical types}

 @item{@cppdef{SCHEME_REALP} --- all non-complex numerical types, plus
 @cpp{scheme_complex_izi_type}}

 @item{@cppdef{SCHEME_EXACT_INTEGERP} --- fixnums and bignums}

 @item{@cppdef{SCHEME_EXACT_REALP} --- fixnums, bignums, and rationals}

 @item{@cppdef{SCHEME_FLOATP} --- both single-precision (when enabled)
 and double-precision flonums}

]

@; ----------------------------------------------------------------------

@section{Global Constants}

There are six global constants:

@itemize[

 @item{@cppdef{scheme_null} --- test for this value with
 @cppdef{SCHEME_NULLP}}

 @item{@cppdef{scheme_eof} --- test for this value with
 @cppdef{SCHEME_EOFP}}

 @item{@cppdef{scheme_true}}

 @item{@cppdef{scheme_false} --- test for this value with
 @cppdef{SCHEME_FALSEP}; test @italic{against} it with
 @cppdef{SCHEME_TRUEP}}

 @item{@cppdef{scheme_void} --- test for this value with
 @cppdef{SCHEME_VOIDP}}

 @item{@cppdef{scheme_undefined}}

]

In some embedding contexts, the function forms
@cppi{scheme_make_null}, etc., must be used, instead.

@; ----------------------------------------------------------------------

@section[#:tag "im:strings"]{Strings}

As noted in @secref["im:unicode"], a Racket character is a Unicode
 code point represented by a @cpp{mzchar} value, and character strings
 are @cpp{mzchar} arrays. Racket also supplies byte strings, which
 are @cpp{char} arrays.

For a character string @var{s}, @cpp{@cpp{SCHEME_CHAR_STR_VAL}(@var{s})}
 produces a pointer to @cpp{mzchar}s, not @cpp{char}s. Convert a
 character string to its UTF-8 encoding as byte string with
 @cpp{scheme_char_string_to_byte_string}. For a byte string
 @var{bs}, @cpp{@cpp{SCHEME_BYTE_STR_VAL}(@var{bs})} produces a pointer
 to @cpp{char}s. The function
 @cpp{scheme_byte_string_to_char_string} decodes a byte string as
 UTF-8 and produces a character string. The functions
 @cpp{scheme_char_string_to_byte_string_locale} and
 @cpp{scheme_byte_string_to_char_string_locale} are similar, but
 they use the current locale's encoding instead of UTF-8.

For more fine-grained control over UTF-8 encoding, use the
 @cpp{scheme_utf8_decode} and @cpp{scheme_utf8_encode} functions, which 
 are described in @secref["im:encodings"].

@; ----------------------------------------------------------------------

@section{Value Functions}

@function[(Scheme_Object* scheme_make_null)]{

Returns @cppi{scheme_null}.
}

@function[(Scheme_Object* scheme_make_eof)]{

Returns @cppi{scheme_eof}.
}

@function[(Scheme_Object* scheme_make_true)]{

Returns @cppi{scheme_true}.
}

@function[(Scheme_Object* scheme_make_false)]{

Returns @cppi{scheme_false}.
}

@function[(Scheme_Object* scheme_make_void)]{

Returns @cppi{scheme_void}.
}

@function[(Scheme_Object* scheme_make_char
           [mzchar ch])]{

Returns the character value. The @var{ch} value must be a legal
Unicode code point (and not a surrogate, for example). The first 256
characters are represented by constant Racket values, and others are
allocated.}

@function[(Scheme_Object* scheme_make_char_or_null
           [mzchar ch])]{

Like @cpp{scheme_make_char}, but the result is @cpp{NULL} if @var{ch}
is not a legal Unicode code point.}

@function[(Scheme_Object* scheme_make_character
           [mzchar ch])]{

Returns the character value. This is a macro that directly accesses
 the array of constant characters when @var{ch} is less than 256.}

@function[(Scheme_Object* scheme_make_ascii_character
           [mzchar ch])]{

Returns the character value, assuming that @var{ch} is less than 256. (This is a macro.)}

@function[(Scheme_Object* scheme_make_integer
           [intptr_t i])]{

Returns the integer value; @var{i} must fit in a fixnum. (This is a macro.)}

@function[(Scheme_Object* scheme_make_integer_value
           [intptr_t i])]{

Returns the integer value. If @var{i} does not fit in a fixnum,
 a bignum is returned.}

@function[(Scheme_Object* scheme_make_integer_value_from_unsigned
           [uintptr_t i])]{

Like @cpp{scheme_make_integer_value}, but for unsigned integers.}

@function[(Scheme_Object* scheme_make_integer_value_from_long_long
           [mzlonglong i])]{

Like @cpp{scheme_make_integer_value}, but for @cpp{mzlonglong}
 values (see @secref["im:intsize"]).}

@function[(Scheme_Object* scheme_make_integer_value_from_unsigned_long_long
           [umzlonglong i])]{

Like @cpp{scheme_make_integer_value_from_long_long}, but for unsigned integers.}

@function[(Scheme_Object* scheme_make_integer_value_from_long_halves
           [uintptr_t hi]
           [uintptr_t lo])]{

Creates an integer given the high and low @cpp{intptr_t}s of a signed
 integer. Note that on 64-bit platforms where @cpp{long long} is the
 same as @cpp{intptr_t}, the resulting integer has 128 bits. (See also
 @secref["im:intsize"].)}

@function[(Scheme_Object* scheme_make_integer_value_from_unsigned_long_halves
           [uintptr_t hi]
           [uintptr_t lo])]{

Creates an integer given the high and low @cpp{intptr_t}s of an unsigned
 integer. Note that on 64-bit platforms where @cpp{long long} is the
 same as @cpp{intptr_t}, the resulting integer has 128 bits.}

@function[(int scheme_get_int_val
           [Scheme_Object* o]
           [intptr_t* i])]{

Extracts the integer value. Unlike the @cppi{SCHEME_INT_VAL} macro,
 this procedure will extract an integer that fits in a @cpp{intptr_t} from
 a Racket bignum. If @var{o} fits in a @cpp{intptr_t}, the extracted
 integer is placed in @var{*i} and 1 is returned; otherwise, 0 is
 returned and @var{*i} is unmodified.}

@function[(int scheme_get_unsigned_int_val
           [Scheme_Object* o]
           [uintptr_t* i])]{

Like @cpp{scheme_get_int_val}, but for unsigned integers.}

@function[(int scheme_get_long_long_val
           [Scheme_Object* o]
           [mzlonglong* i])]{

Like @cpp{scheme_get_int_val}, but for @cpp{mzlonglong} values (see
 @secref["im:intsize"]).}

@function[(int scheme_get_unsigned_long_long_val
           [Scheme_Object* o]
           [umzlonglong* i])]{

Like @cpp{scheme_get_int_val}, but for unsigned @cpp{mzlonglong} values (see
 @secref["im:intsize"]).}

@function[(Scheme_Object* scheme_make_double
           [double d])]{

Creates a new floating-point value.}

@function[(Scheme_Object* scheme_make_float
           [float d])]{

Creates a new single-precision floating-point value. The procedure is
available only when Racket is compiled with single-precision
numbers enabled.}

@function[(double scheme_real_to_double
           [Scheme_Object* o])]{

Converts a Racket real number to a double-precision floating-point
value.}

@function[(Scheme_Object* scheme_make_pair
           [Scheme_Object* carv]
           [Scheme_Object* cdrv])]{

Makes a @racket[cons] pair.}

@function[(Scheme_Object* scheme_make_byte_string
           [char* bytes])]{

Makes a Racket byte string from a nul-terminated C string. The
@var{bytes} string is copied.}

@function[(Scheme_Object* scheme_make_byte_string_without_copying
           [char* bytes])]{

Like @cpp{scheme_make_byte_string}, but the string is not copied.}

@function[(Scheme_Object* scheme_make_sized_byte_string
           [char* bytes]
           [intptr_t len]
           [int copy])]{

Makes a byte string value with size @var{len}. A copy of @var{bytes}
 is made if @var{copy} is not 0. The string @var{bytes} should
 contain @var{len} bytes; @var{bytes} can contain the nul byte at any
 position, and need not be nul-terminated if @var{copy} is
 non-zero. However, if @var{len} is negative, then the nul-terminated
 length of @var{bytes} is used for the length, and if @var{copy} is
 zero, then @var{bytes} must be nul-terminated.}

@function[(Scheme_Object* scheme_make_sized_offset_byte_string
           [char* bytes]
           [intptr_t d]
           [intptr_t len]
           [int copy])]{

Like @cpp{scheme_make_sized_byte_string}, except the @var{len}
 characters start from position @var{d} in @var{bytes}. If @var{d} is
 non-zero, then @var{copy} must be non-zero.}

@function[(Scheme_Object* scheme_alloc_byte_string
           [intptr_t size]
           [char fill])]{

Allocates a new Racket byte string.}

@function[(Scheme_Object* scheme_append_byte_string
           [Scheme_Object* a]
           [Scheme_Object* b])]{

Creates a new byte string by appending the two given byte strings.}

@function[(Scheme_Object* scheme_make_locale_string
           [char* bytes])]{

Makes a Racket string from a nul-terminated byte string that is a
 locale-specific encoding of a character string; a new string is
 allocated during decoding.  The ``locale in the name of this function
 thus refers to @var{bytes}, and not the resulting string (which is
 internally stored as UCS-4).}

@function[(Scheme_Object* scheme_make_utf8_string
           [char* bytes])]{

Makes a Racket string from a nul-terminated byte string that is a
 UTF-8 encoding. A new string is allocated during decoding. The
 ``utf8'' in the name of this function thus refers to @var{bytes}, and
 not the resulting string (which is internally stored as UCS-4).}

@function[(Scheme_Object* scheme_make_sized_utf8_string
           [char* bytes]
           [intptr_t len])]{

Makes a string value, based on @var{len} UTF-8-encoding bytes (so the
 resulting string is @var{len} characters or less). The string
 @var{bytes} should contain at least @var{len} bytes; @var{bytes} can
 contain the nul byte at any position, and need not be
 null-terminated. However, if @var{len} is negative, then the
 nul-terminated length of @var{bytes} is used for the length.}

@function[(Scheme_Object* scheme_make_sized_offset_utf8_string
           [char* bytes]
           [intptr_t d]
           [intptr_t len])]{

Like @cpp{scheme_make_sized_char_string}, except the @var{len} characters
 start from position @var{d} in @var{bytes}.}


@function[(Scheme_Object* scheme_make_char_string
           [mzchar* chars])]{

Makes a Racket string from a nul-terminated UCS-4 string. The
 @var{chars} string is copied.}

@function[(Scheme_Object* scheme_make_char_string_without_copying
           [mzchar* chars])]{

Like @cpp{scheme_make_char_string}, but the string is not copied.}

@function[(Scheme_Object* scheme_make_sized_char_string
           [mzchar* chars]
           [intptr_t len]
           [int copy])]{

Makes a string value with size @var{len}. A copy of @var{chars} is
 made if @var{copy} is not 0. The string @var{chars} should
 contain @var{len} characters; @var{chars} can contain the nul
 character at any position, and need not be nul-terminated
 if @var{copy} is non-zero. However, if @var{len} is negative, then
 the nul-terminated length of @var{chars} is used for the length, and
 if @var{copy} is zero, then the @var{chars} must be nul-terminated.}

@function[(Scheme_Object* scheme_make_sized_offset_char_string
           [mzchar* chars]
           [intptr_t d]
           [intptr_t len]
           [int copy])]{

Like @cpp{scheme_make_sized_char_string}, except the @var{len}
 characters start from position @var{d} in @var{chars}. If @var{d} is
 non-zero, then @var{copy} must be non-zero.}

@function[(Scheme_Object* scheme_alloc_char_string
           [intptr_t size]
           [mzchar fill])]{

Allocates a new Racket string.}

@function[(Scheme_Object* scheme_append_char_string
           [Scheme_Object* a]
           [Scheme_Object* b])]{

Creates a new string by appending the two given strings.}

@function[(Scheme_Object* scheme_char_string_to_byte_string
           [Scheme_Object* s])]{

Converts a Racket character string into a Racket byte string via UTF-8.}

@function[(Scheme_Object* scheme_byte_string_to_char_string
           [Scheme_Object* s])]{

Converts a Racket byte string into a Racket character string via UTF-8.}

@function[(Scheme_Object* scheme_char_string_to_byte_string_locale
           [Scheme_Object* s])]{

Converts a Racket character string into a Racket byte string via the locale's encoding.}

@function[(Scheme_Object* scheme_byte_string_to_char_string_locale
           [Scheme_Object* s])]{

Converts a Racket byte string into a Racket character string via the locale's encoding.}

@function[(Scheme_Object* scheme_intern_symbol
           [char* name])]{

Finds (or creates) the symbol matching the given nul-terminated, ASCII
 string (not UTF-8). The case of @var{name} is (non-destructively) normalized
 before interning if @cppi{scheme_case_sensitive} is 0.}

@function[(Scheme_Object* scheme_intern_exact_symbol
           [char* name]
           [int len])]{

Creates or finds a symbol given the symbol's length in UTF-8-encoding
 bytes. The case of @var{name} is not normalized.}

@function[(Scheme_Object* scheme_intern_exact_char_symbol
           [mzchar* name]
           [int len])]{

Like @cpp{scheme_intern_exact_symbol}, but given a character array
 instead of a UTF-8-encoding byte array.}

@function[(Scheme_Object* scheme_make_symbol
           [char* name])]{

Creates an uninterned symbol from a nul-terminated, UTF-8-encoding
 string. The case is not normalized.}

@function[(Scheme_Object* scheme_make_exact_symbol
           [char* name]
           [int len])]{

Creates an uninterned symbol given the symbol's length in
 UTF-8-encoded bytes.}

@function[(Scheme_Object* scheme_intern_exact_keyword
           [char* name]
           [int len])]{

Creates or finds a keyword given the keywords length in UTF-8-encoding
 bytes. The case of @var{name} is not normalized, and it should
 not include the leading hash and colon of the keyword's printed form.}

@function[(Scheme_Object* scheme_intern_exact_char_keyword
           [mzchar* name]
           [int len])]{

Like @cpp{scheme_intern_exact_keyword}, but given a character array
 instead of a UTF-8-encoding byte array.}

@function[(Scheme_Object* scheme_make_vector
           [intptr_t size]
           [Scheme_Object* fill])]{

Allocates a new vector.}

@function[(Scheme_Double_Vector* scheme_alloc_flvector
           [intptr_t size])]{

Allocates an uninitialized flvector.
The result type is effectively an alias for @cpp{Scheme_Object*}.}

@function[(Scheme_Vector* scheme_alloc_fxvector
           [intptr_t size])]{

Allocates an uninitialized fxvector. 
The result type is effectively an alias for @cpp{Scheme_Object*}.}

@function[(Scheme_Object* scheme_box
           [Scheme_Object* v])]{

Creates a new box containing the value @var{v}.}

@function[(Scheme_Object* scheme_make_weak_box
           [Scheme_Object* v])]{

Creates a new weak box containing the value @var{v}.}

@function[(Scheme_Type scheme_make_type
           [char* name])]{

Creates a new type (not a Racket value). The type tag is valid across
all @|tech-place|s.}

@function[(Scheme_Object* scheme_make_cptr
           [void* ptr]
           [const-Scheme_Object* typetag])]{

Creates a C-pointer object that encapsulates @var{ptr} and uses
 @var{typetag} to identify the type of the pointer. The
 @cppi{SCHEME_CPTRP} macro recognizes objects created by
 @cpp{scheme_make_cptr}. The @cppi{SCHEME_CPTR_VAL} macro extracts
 the original @var{ptr} from the Racket object, and
 @cppi{SCHEME_CPTR_TYPE} extracts the type tag.
 The @cppi{SCHEME_CPTR_OFFSETVAL} macro returns @cpp{0} 
 for the result Racket object.

 The @var{ptr} can refer to either memory managed by the garbage
 collector or by some other memory manager. Beware, however, of
 retaining a @var{ptr} that refers to memory released by another
 memory manager, since the enclosing memory range might later become
 managed by the garbage collector (in which case @var{ptr} might
 become an invalid pointer that can crash the garbage collector).}

@function[(Scheme_Object* scheme_make_external_cptr
           [void* ptr]
           [const-Scheme_Object* typetag])]{

Like @cpp{scheme_make_cptr}, but @var{ptr} is never treated as
referencing memory managed by the garbage collector.}

@function[(Scheme_Object* scheme_make_offset_cptr
           [void* ptr]
           [intptr_t offset]
           [const-Scheme_Object* typetag])]{

Creates a C-pointer object that encapsulates both @var{ptr} and @var{offset}.
 The @cppi{SCHEME_CPTR_OFFSETVAL} macro returns @var{offset} 
 for the result Racket object (and the macro be used to change the offset,
 since it also works on objects with no offset).

 The @var{ptr} can refer to either memory managed by the garbage
 collector or by some other memory manager; see also 
 @cpp{scheme_make_cptr}.}

@function[(Scheme_Object* scheme_make_offset_external_cptr
           [void* ptr]
           [intptr_t offset]
           [const-Scheme_Object* typetag])]{

 Like @cpp{scheme_make_offset_cptr}, but @var{ptr} is never treated as
referencing memory managed by the garbage collector.}


@function[(void scheme_set_type_printer
           [Scheme_Type type]
           [Scheme_Type_Printer printer])]{

Installs a printer to be used for printing (or writing or displaying)
 values that have the type tag @var{type}.

The type of @var{printer} is defined as follows:

@verbatim[#:indent 2]{
 typedef void (*Scheme_Type_Printer)(Scheme_Object *v, int dis,
                                     Scheme_Print_Params *pp);
}

Such a printer must print a representation of the value using
 @cppi{scheme_print_bytes} and @cppi{scheme_print_string}.  The
 first argument to the printer, @var{v}, is the value to be printed.
 The second argument indicates whether @var{v} is printed via
 @racket[write] or @racket[display]. The last argument is to be passed
 on to @cppi{scheme_print_bytes} or @cppi{scheme_print_string} to
 identify the printing context.}

@function[(void scheme_print_bytes
           [Scheme_Print_Params* pp]
           [const-char* str]
           [int offset]
           [int len])]{

Writes the content of @var{str} --- starting from @var{offset} and
 running @var{len} bytes --- into a printing context determined by
 @var{pp}. This function is for use by a printer that is installed
 with @cpp{scheme_set_type_printer}.}

@function[(void scheme_print_string
           [Scheme_Print_Params* pp]
           [const-mzchar* str]
           [int offset]
           [int len])]{

Writes the content of @var{str} --- starting from @var{offset} and
 running @var{len} characters --- into a printing context determined
 by @var{pp}. This function is for use by a printer that is installed
 with @cpp{scheme_set_type_printer}.}

@function[(void scheme_set_type_equality
           [Scheme_Type type]
           [Scheme_Equal_Proc equalp]
           [Scheme_Primary_Hash_Proc hash1]
           [Scheme_Secondary_Hash_Proc hash2])]{

Installs an equality predicate and associated hash functions for
values that have the type tag @var{type}. The @var{equalp} predicate
is only applied to values that both have tag @var{type}.

The type of @var{equalp}, @var{hash1}, and @var{hash2} are defined as
follows:

@verbatim[#:indent 2]{
 typedef int (*Scheme_Equal_Proc)(Scheme_Object* obj1,
                                  Scheme_Object* obj2,
                                  void* cycle_data);
 typedef intptr_t (*Scheme_Primary_Hash_Proc)(Scheme_Object* obj, 
                                          intptr_t base,
                                          void* cycle_data);
 typedef intptr_t (*Scheme_Secondary_Hash_Proc)(Scheme_Object* obj,
                                           void* cycle_data);
}

The two hash functions are use to generate primary and secondary keys
for double hashing in an @racket[equal?]-based hash table. The result
of the primary-key function should depend on both @var{obj} and
@var{base}.

The @var{cycle_data} argument in each case allows checking and hashing
on cyclic values. It is intended for use in recursive checking or
hashing via @cpp{scheme_recur_equal},
@cpp{scheme_recur_equal_hash_key}, and
@cpp{scheme_recur_equal_hash_key}. That is, do not call plain
@cpp{scheme_equal}, @cpp{scheme_equal_hash_key}, or
@cpp{scheme_equal_hash_key} for recursive checking or hashing on
sub-elements of the given value(s).}


