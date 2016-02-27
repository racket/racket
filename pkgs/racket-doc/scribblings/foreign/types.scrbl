#lang scribble/doc
@(require "utils.rkt" 
          (for-label scheme/match)
          (for-syntax racket/base)
          scribble/eval
          scribble/racket)

@(define-syntax _float*
   (make-element-id-transformer
     (lambda (stx)
       #'@racketidfont{_float*})))

@(define-syntax-rule (defform-arrow . content)
   (begin
     (require (only-in (for-label ffi/unsafe) ->))
     (defidform -> . content)))

@(define ffi-eval (make-base-eval))
@(ffi-eval '(require ffi/unsafe))

@title[#:tag "types" #:style 'toc]{C Types}

@deftech{C types} are the main concept of the @tech{FFI}, either
primitive types or user-defined types.  The @tech{FFI} deals with
primitive types internally, converting them to and from C types.  A
user type is defined in terms of existing primitive and user types,
along with conversion functions to and from the existing types.

@local-table-of-contents[]

@; ----------------------------------------------------------------------

@section[#:tag "ctype"]{Type Constructors}

@defproc[(make-ctype [type ctype?]
                     [racket-to-c (or/c #f (any/c . -> . any))]
                     [c-to-racket (or/c #f (any/c . -> . any))])
         ctype?]{

Creates a new @tech{C type} value whose representation for foreign
code is the same as @racket[type]'s.

The given conversion functions convert to and from the Racket
representation of the new type. Either conversion function can be
@racket[#f], meaning that the conversion for the corresponding
direction is the identity function.  If both functions are
@racket[#f], @racket[type] is returned.

The @racket[racket-to-c] function takes any value and, if it is a
valid representation of the new type, converts it to a representation
of @racket[type]. The @racket[c-to-racket] function takes a
representation of @racket[type] and produces a representation of the
new type.}


@defproc[(ctype? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{C type}, @racket[#f]
otherwise.

@examples[#:eval ffi-eval
  (ctype? _int)
  (ctype? (_fun _int -> _int))
  (ctype? #f)
  (ctype? "foo")
]}


@defproc*[([(ctype-sizeof [type ctype?]) exact-nonnegative-integer?]
           [(ctype-alignof [type ctype?]) exact-nonnegative-integer?])]{

Returns the size or alignment of a given @racket[type] for the current
platform.

@examples[#:eval ffi-eval
  (ctype-sizeof _int)
  (ctype-sizeof (_fun _int -> _int))
  (ctype-alignof _int)
  (ctype-alignof (_fun _int -> _int))
]}


@defproc[(ctype->layout [type ctype?])
         (flat-rec-contract rep symbol? (listof rep))]{

Returns a value to describe the eventual C representation of the
type. It can be any of the following symbols:

@racketblock[
  'int8 'uint8 'int16 'uint16 'int32 'uint32 'int64 'uint64
  'float 'double 'bool 'void 'pointer 'fpointer 
  'bytes 'string/ucs-4 'string/utf-16
]

The result can also be a list, which describes a C struct whose
element representations are provided in order within the
list. Finally, the result can be a vector of size 2 containing an
element representation followed by an exact-integer count.

@examples[#:eval ffi-eval
  (ctype->layout _int)
  (ctype->layout _void)
  (ctype->layout (_fun _int -> _int))
]}


@defproc[(compiler-sizeof [sym (or/c symbol? (listof symbol?))]) exact-nonnegative-integer?]{

Possible values for @racket[sym] are @racket['int], @racket['char],
@racket['short], @racket['long], @racket['*], @racket['void],
@racket['float], @racket['double], or lists of symbols, such as 
@racket['(long long)]. The result is the size of the
corresponding type according to the C @cpp{sizeof} operator for the
current platform. The @racket[compiler-sizeof] operation should be
used to gather information about the current platform, such as
defining alias type like @racket[_int] to a known type like
@racket[_int32].

@examples[#:eval ffi-eval
  (compiler-sizeof 'int)
  (compiler-sizeof '(long long))
]}

@; ----------------------------------------------------------------------

@section{Numeric Types}

@defthing*[([_int8 ctype?]
            [_sint8 ctype?]
            [_uint8 ctype?]
            [_int16 ctype?]
            [_sint16 ctype?]
            [_uint16 ctype?]
            [_int32 ctype?]
            [_sint32 ctype?]
            [_uint32 ctype?]
            [_int64 ctype?]
            [_sint64 ctype?]
            [_uint64 ctype?])]{

The basic integer types at various sizes. The @racketidfont{s} or
@racketidfont{u} prefix specifies a signed or an unsigned integer,
respectively; the ones with no prefix are signed.}


@defthing*[([_byte ctype?]
            [_sbyte ctype?]
            [_ubyte ctype?])]{

The @racket[_sbyte] and @racket[_ubyte] types are aliases
for @racket[_sint8] and @racket[_uint8], respectively.
The @racket[_byte] type is like @racket[_ubyte], but adds
256 to a negative Racket value that would work as a @racket[_sbyte]
(i.e., it casts signed bytes to unsigned bytes).}


@defthing*[([_word ctype?]
            [_sword ctype?]
            [_uword ctype?]
            )]{

The @racket[_sword] and @racket[_uword] types are aliases
for @racket[_sint16] and @racket[_uint16], respectively.
The @racket[_word] type is like @racket[_uword], but coerces
negative values in the same way as @racket[_byte].}


@defthing*[([_short ctype?]
            [_sshort ctype?]
            [_ushort ctype?]
            [_int ctype?]
            [_sint ctype?]
            [_uint ctype?]
            [_long ctype?]
            [_slong ctype?]
            [_ulong ctype?]
            [_llong ctype?]
            [_sllong ctype?]
            [_ullong ctype?]
            [_intptr ctype?]
            [_sintptr ctype?]
            [_uintptr ctype?])]{

Aliases for basic integer types. The @racket[_short] aliases
correspond to @racket[_int16]. The @racket[_int] aliases correspond to
@racket[_int32]. The @racket[_long] aliases correspond to either
@racket[_int32] or @racket[_int64], depending on the platform. Similarly,
the @racket[_intptr] aliases correspond to either
@racket[_int32] or @racket[_int64], depending on the platform.}

@defthing*[([_size ctype?]
            [_ssize ctype?]
            [_ptrdiff ctype?]
            [_intmax ctype?]
            [_uintmax ctype?])]{

More aliases for basic integer types. The @racket[_size] and
@racket[_uintmax] types are aliases for @racket[_uintptr], and
the rest are aliases for @racket[_intptr].}

@defthing*[([_fixnum ctype?]
            [_ufixnum ctype?])]{

For cases where speed matters and where you know that the integer is
small enough, the types @racket[_fixnum] and @racket[_ufixnum] are
similar to @racket[_intptr] and @racket[_uintptr] but assume that the
quantities fit in Racket's immediate integers (i.e., not bignums).}

@defthing*[([_fixint ctype?]
            [_ufixint ctype?])]{

Similar to @racket[_fixnum]/@racket[_ufixnum], but based on
@racket[_int]/@racket[_uint] instead of
@racket[_intptr]/@racket[_uintptr], and coercions from C are checked
to be in range.}

@defthing*[([_float ctype?]
            [_double ctype?]
            [_double* ctype?])]{

The @racket[_float] and @racket[_double] types represent the
corresponding C types. Both single- and double-precision Racket
numbers are accepted for conversion via both @racket[_float] and 
@racket[_double], while both @racket[_float] and @racket[_double]
coerce C values to double-precision Racket numbers.
The type @racket[_double*]
coerces any Racket real number to a C @cpp{double}.}

@defthing[_longdouble ctype?]{

Represents the @cpp{long double} type on platforms where it is
supported, in which case Racket @tech[#:doc
reference.scrbl]{extflonums} convert to and from @cpp{long double}
values.}

@; ------------------------------------------------------------

@section{Other Atomic Types}

@defthing[_stdbool ctype?]{

The @racket[_stdbool] type represents the C99 @cpp{bool} type from
@cpp{<stdbool.h>}. It translates @racket[#f] to a @racket[0]
@cpp{bool} and any other value to a @racket[1] @cpp{bool}.

@history[#:added "6.0.0.6"]}

@defthing[_bool ctype?]{

Translates @racket[#f] to a @racket[0] @cpp{int} and any other
value to a @racket[1] @cpp{int}, reflecting one of many
traditional (i.e., pre-C99) encodings of booleans. See also
@racket[_stdbool].}

@defthing[_void ctype?]{

Indicates a Racket @|void-const| return value, and it cannot be used
to translate values to C. This type cannot be used for function
inputs.}

@; ------------------------------------------------------------

@section{String Types}

@subsection{Primitive String Types}

The five primitive string types correspond to cases where a C
representation matches Racket's representation without encodings.

The form @racket[_bytes] form can be used type for Racket byte
strings, which corresponds to C's @cpp{char*} type.  In addition to
translating byte strings, @racket[#f] corresponds to the @cpp{NULL}
pointer.

@deftogether[(
@defthing[_string/ucs-4 ctype?]
)]{

A type for Racket's native Unicode strings, which are in UCS-4 format.
These correspond to the C @cpp{mzchar*} type used by Racket. As usual, the types
treat @racket[#f] as @cpp{NULL} and vice versa.}


@deftogether[(
@defthing[_string/utf-16 ctype?]
)]{

Unicode strings in UTF-16 format. As usual, the types treat
@racket[#f] as @cpp{NULL} and vice versa.}


@defthing[_path ctype?]{

Simple @cpp{char*} strings, corresponding to Racket's paths. As usual,
the types treat @racket[#f] as @cpp{NULL} and vice versa.

Beware that changing the current directory via
@racket[current-directory] does not change the OS-level current
directory as seen by foreign library functions. Paths normally should
be converted to absolute form using @racket[path->complete-path]
(which uses the @racket[current-directory] parameter) before passing
them to a foreign function.}


@defthing[_symbol ctype?]{

Simple @cpp{char*} strings as Racket symbols (encoded in UTF-8).
Return values using this type are interned as symbols.}


@subsection{Fixed Auto-Converting String Types}

@defthing*[([_string/utf-8 ctype?]
            [_string/latin-1 ctype?]
            [_string/locale ctype?])]{

Types that correspond to (character) strings on the Racket side and
@cpp{char*} strings on the C side.  The bridge between the two requires
a transformation on the content of the string.  As usual, the types
treat @racket[#f] as @cpp{NULL} and vice versa.}

@defthing*[([_string*/utf-8 ctype?]
            [_string*/latin-1 ctype?]
            [_string*/locale ctype?])]{

Similar to @racket[_string/utf-8], etc., but accepting a wider range
of values: Racket byte strings are allowed and passed as is, and
Racket paths are converted using @racket[path->bytes].}


@subsection{Variable Auto-Converting String Type}

The @racket[_string/ucs-4] type is rarely useful when interacting with
foreign code, while using @racket[_bytes] is somewhat unnatural, since
it forces Racket programmers to use byte strings. Using
@racket[_string/utf-8], etc., meanwhile, may prematurely commit to a
particular encoding of strings as bytes. The @racket[_string] type
supports conversion between Racket strings and @cpp{char*} strings
using a parameter-determined conversion.

@defthing[_string ctype?]{

Expands to a use of the @racket[default-_string-type] parameter.  The
parameter's value is consulted when @racket[_string] is evaluated, so
the parameter should be set before any interface definition that uses
@racket[_string].}

@defparam[default-_string-type type ctype?]{

A parameter that determines the current meaning of @racket[_string].
It is initially set to @racket[_string*/utf-8].  If you change it, do
so @italic{before} interfaces are defined.}


@subsection{Other String Types}

@defthing[_file ctype?]{

Like @racket[_path], but when values go from Racket to C,
@racket[cleanse-path] is used on the given value.  As an output value,
it is identical to @racket[_path].}

@defthing[_bytes/eof ctype?]{

Similar to the @racket[_bytes] type, except that a foreign return
value of @cpp{NULL} is translated to a Racket @racket[eof] value.}

@defthing[_string/eof ctype?]{

Similar to the @racket[_string] type, except that a foreign return
value of @cpp{NULL} is translated to a Racket @racket[eof] value.}

@; ------------------------------------------------------------

@section{Pointer Types}

@defthing[_pointer ctype?]{

Corresponds to Racket @deftech{C pointer} values.  These pointers can have
an arbitrary Racket object attached as a type tag.  The tag is ignored
by built-in functionality; it is intended to be used by interfaces.
See @secref["foreign:tagged-pointers"] for creating pointer types that
use these tags for safety. A @racket[#f] value is converted to
@cpp{NULL} and vice versa.

The address referenced by a @racket[_pointer] value must not refer to
memory managed by the garbage collector (unless the address
corresponds to a value that supports interior pointers and that is
otherwise referenced to preserve the value from garbage collection).
The reference is not traced or updated by the garbage collector.

The @racket[equal?] predicate equates C pointers (including pointers
for @racket[_gcpointer] and possibly containing an offset) when they
refer to the same address---except for C pointers that are instances
of structure types with the @racket[prop:cpointer] property, in which
case the equality rules of the relevant structure types apply.}


@defthing[_gcpointer ctype?]{

Like @racket[_pointer], but for a C pointer value that can refer to memory
managed by the garbage collector.

Although a @racket[_gcpointer] can reference to memory that is not
managed by the garbage collector, beware of using an address that
might eventually become managed by the garbage collector. For example,
if a reference is created by @racket[malloc] with @racket['raw] and
released by @racket[free], then the @racket[free] may allow the memory
formerly occupied by the reference to be used later by the garbage
collector.

The @racket[cpointer-gcable?] function returns @racket[#t] for a
cpointer generated via the @racket[_gcpointer] type, while it
generates @racket[#f] for a cpointer generated via the
@racket[_cpointer] type.}


@deftogether[(
@defthing[_racket ctype?]
@defthing[_scheme ctype?]
)]{

A type that can be used with any Racket object; it corresponds to the
@cpp{Scheme_Object*} type of Racket's C API (see @|InsideRacket|). The
@racket[_racket] or @racket[_scheme] type is useful only for libraries
that are aware of Racket's C API.

As a result type with a function type, @racket[_racket] or
@racket[_scheme] permits multiple values, but multiple values are not
allowed in combination with a true value for
@racket[#:in-original-place?] or @racket[#:async-apply] in
@racket[_cprocedure] or @racket[_fun].}


@defthing[_fpointer ctype?]{

Similar to @racket[_pointer], except that when @racket[_fpointer] is
used as the type for @racket[get-ffi-obj] or @racket[ffi-obj-ref],
then a level of indirection is skipped. Furthermore, for a C pointer
value from @racket[get-ffi-obj] or @racket[ffi-obj-ref] using
@racket[_fpointer], @racket[ptr-ref] on the pointer as a
@racket[_fpointer] simply returns the pointer instead of dereferencing
it.  Like @racket[_pointer], @racket[_fpointer] treats @racket[#f] as
@cpp{NULL} and vice versa.

A type generated by @racket[_cprocedure] or @racket[_fun] builds on
@racket[_fpointer], and normally @racket[_cprocedure] or @racket[_fun]
should be used instead of @racket[_fpointer].}


@defproc[(_or-null [ctype ctype?]) ctype?]{

Creates a type that is like @racket[ctype], but @racket[#f] is
converted to @cpp{NULL} and vice versa. The given @racket[ctype] must
have the same C representation as @racket[_pointer],
@racket[_gcpointer], or @racket[_fpointer].}


@defproc[(_gcable [ctype ctype?]) ctype?]{

Creates a type that is like @racket[ctype], but whose base
representation is like @racket[_gcpointer] instead of
@racket[_pointer]. The given @racket[ctype] must have a base
representation like @racket[_pointer] or @racket[_gcpointer] (and in
the later case, the result is the @racket[ctype]).}


@; ------------------------------------------------------------

@section[#:tag "foreign:procedures"]{Function Types}

@defproc[(_cprocedure [input-types (list ctype?)]
                      [output-type ctype?]
                      [#:abi abi (or/c #f 'default 'stdcall 'sysv) #f]
                      [#:atomic? atomic? any/c #f]
                      [#:async-apply async-apply (or/c #f ((-> any/c) . -> . any/c) box?) #f]
                      [#:lock-name lock-name (or/c string? #f) #f]
                      [#:in-original-place? in-original-place? any/c #f]
                      [#:save-errno save-errno (or/c #f 'posix 'windows) #f]
                      [#:wrapper wrapper (or/c #f (procedure? . -> . procedure?))
                                         #f]
                      [#:keep keep (or/c boolean? box? (any/c . -> . any/c))
                                   #t])
         any]{

A type constructor that creates a new function type, which is
specified by the given @racket[input-types] list and @racket[output-type].
Usually, the @racket[_fun] syntax (described below) should be used
instead, since it manages a wide range of complicated cases.

The resulting type can be used to reference foreign functions (usually
@racket[ffi-obj]s, but any pointer object can be referenced with this type),
generating a matching foreign @deftech{callout} object.  Such objects are new primitive
procedure objects that can be used like any other Racket procedure.
As with other pointer types, @racket[#f] is treated as a @cpp{NULL}
function pointer and vice versa.

A type created with @racket[_cprocedure] can also be used for passing
Racket procedures to foreign functions, which will generate a foreign
function pointer that calls to the given Racket @deftech{callback}
procedure.  There are no restrictions on the Racket procedure; in
particular, its lexical context is properly preserved.

The optional @racket[abi] keyword argument determines the foreign ABI
that is used. Supplying @racket[#f] or @racket['default] indicates the
platform-dependent default. The other possible
values---@racket['stdcall] and @racket['sysv] (i.e., ``cdecl'')---are
currently supported only for 32-bit Windows; using them on other
platforms raises an exception. See also @racketmodname[ffi/winapi].

For @tech{callouts} to foreign functions with the generated type:

@itemize[

 @item{If @racket[save-errno] is @racket['posix], then the value of
       @as-index{@tt{errno}} is saved (specific to the current thread)
       immediately after a foreign function @tech{callout}
       returns. The saved value is accessible through
       @racket[saved-errno]. If @racket[save-errno] is
       @racket['windows], then the value of
       @as-index{@tt{GetLastError}}@tt{()} is saved for later use via
       @racket[saved-errno]; the @racket['windows] option is available
       only on Windows (on other platforms @racket[saved-errno] will
       return 0). If @racket[save-errno] is @racket[#f], no error
       value is saved automatically.

       The error-recording support provided by @racket[save-errno] is
       needed because the Racket runtime system may otherwise preempt
       the current Racket thread and itself call functions that set
       error values.}

 @item{If @racket[wrapper] is not @racket[#f], it takes the
       @tech{callout} that would otherwise be generated and returns a
       replacement procedure. Thus, @racket[wrapper] acts a hook to
       perform various argument manipulations before the true
       @tech{callout} is invoked, and it can return different results
       (for example, grabbing a value stored in an ``output'' pointer
       and returning multiple values).}

 @item{If @racket[lock-name] is not @racket[#f], then a process-wide
       lock with the given name is held during the foreign call. In a
       build that supports parallel places, @racket[lock-name] is
       registered via @cpp{scheme_register_process_global}, so choose
       names that are suitably distinct.}

 @item{If @racket[in-original-place?] is true, then when a foreign
       @tech{callout} procedure with the generated type is called in
       any Racket @tech-place[], the procedure
       is called from the original Racket place. Use this mode for a
       foreign function that is not thread-safe at the C level, which
       means that it is not place-safe at the Racket
       level. @tech{Callbacks} from place-unsafe code back into Racket
       at a non-original place typically will not work, since the
       place of the Racket code may have a different allocator than
       the original place.}

 @item{Values that are provided to a @tech{callout} (i.e., the
       underlying callout, and not the replacement produced by a
       @racket[wrapper], if any) are always considered reachable by the
       garbage collector until the called foreign function returns. If
       the foreign function invokes Racket callbacks, however, beware
       that values managed by the Racket garbage collector might be
       moved in memory by the garbage collector.}

 @item{A @tech{callout} object is finalized internally. Beware
       of trying to use a @tech{callout} object that is reachable
       only from a finalized object, since the two objects
       might be finalized in either order.}

]

For @tech{callbacks} to Racket functions with the generated type:

@itemize[

@item{The @racket[keep] argument provides control over reachbility by
      the garbage collector of the underlying value that foreign code
      see as a plain C function.  Additional care must be taken in
      case the foreign code might retain the callback function, in
      which case the callback value must remain reachable or else the
      held callback will become invalid.  The possible values of
      @racket[keep] are as follows:

   @itemize[

    @item{@racket[#t] --- the @tech{callback} stays in memory as long
      as the converted Racket function is reachable. This mode is the
      default, as it is fine in most cases. Note that each Racket
      function can hold onto only one callback value through this
      mode, so it is not suitable for a function used multiple times
      as a reatined callback.}

   @item{@racket[#f] --- the @tech{callback} value is not held.  This
      mode may be useful for a callback that is only used for the
      duration of the foreign call; for example, the comparison
      function argument to the standard C library @tt{qsort} function
      is only used while @tt{qsort} is working, and no additional
      references to the comparison function are kept.  Use this option
      only in such cases, when no holding is necessary and you want to
      avoid the extra cost.}

   @item{A box holding @racket[#f] or any other non-list value --- the
      callback value is stored in the box, overriding any non-list
      value that was in the box (making it useful for holding a single
      callback value).  When you know that the callback is no longer
      needed, you can ``release'' the callback value by changing the
      box contents or by allowing the box itself to become
      unreachable.  This mode can be useful if the box is held for a
      dynamic extent that corresponds to when the callback is needed;
      for example, you might encapsulate some foreign functionality in
      a Racket class or a unit, and keep the callback box as a field
      in new instances or instantiations of the unit.}

   @item{A box holding @racket[null] (or any list) --- similar to a
      box holding a non-list value, except that new callback values are
      @racket[cons]ed onto the contents of the box.  This mode is
      therefore useful in cases when a Racket function is used
      in multiple callbacks (that is, sent to foreign code to hold
      onto multiple times) and all callbacks should be retained together.}

   @item{A one-argument function --- the function is invoked with the
      callback value when it is generated.  This mode allows you to
      explicitly manage reachability of the generated callback closure.}

   ]}

 @item{If @racket[wrapper] is not @racket[#f], it takes the procedure
       to be converted into a @tech{callback} and returns a
       replacement procedure to be invoked as the callback. Thus,
       @racket[wrapper] acts a hook to perform various argument
       manipulations before a Racket callback function is called, and
       it can return different results to the foreign caller.

       The callback value's reachability (and its interaction with
       @racket[keep] is based on the original function for the
       callback, not the result of @racket[wrapper].}

 @item{If @racket[atomic?] is true, then when a Racket procedure is
       given this procedure type and called as a @tech{callback} from
       foreign code, then the Racket process is put into atomic mode
       while evaluating the Racket procedure body.

        In atomic mode, other Racket threads do not run, so the Racket
       code must not call any function that potentially blocks on
       synchronization with other threads, or else it may lead to
       deadlock. In addition, the Racket code must not perform any
       potentially blocking operation (such as I/O), it must not raise
       an uncaught exception, it must not perform any escaping
       continuation jumps, and its non-tail recursion must be minimal
       to avoid C-level stack overflow; otherwise, the process may
       crash or misbehave.}

 @item{If a @racket[async-apply] is provided as a procedure or box, then a Racket
       @tech{callback} procedure with the generated procedure type can
       be applied in a foreign thread (i.e., an OS-level thread other
       than the one used to run Racket).

       If @racket[async-apply] is a procedure, the call in the foreign
       thread is transferred to the OS-level thread that runs Racket,
       but the Racket-level thread (in the sense of @racket[thread])
       is unspecified; the job of the provided @racket[async-apply]
       procedure is to arrange for the callback procedure to be run in
       a suitable Racket thread.

       The given @racket[async-apply]
       procedure is applied to a thunk that encapsulates the specific
       callback invocation, and the foreign OS-level thread blocks
       until the thunk is called and completes; the thunk must be
       called exactly once, and the callback invocation must return
       normally. The given @racket[async-apply] procedure itself is
       called in atomic mode (see @racket[atomic?] above). 

       If the callback is known to complete quickly, requires no
       synchronization, and works independent of the Racket thread in
       which it runs, then it is safe for the given
       @racket[async-apply] procedure to apply the thunk
       directly. Otherwise, the given @racket[async-apply] procedure
       must arrange for the thunk to be applied in a suitable Racket
       thread sometime after the given @racket[async-apply] procedure
       itself returns; if the thunk raises an exception or
       synchronizes within an unsuitable Racket-level thread, it can
       deadlock or otherwise damage the Racket process.

       If @racket[async-apply] is a box, then the value contained in
       the box is used as the result of the callback when it is called
       in a foreign thread; the @racket[async-apply] value is
       converted to a foreign value at the time that
       @racket[_cprocedure] is called. Using a boxed constant value
       for @racket[async-apply] avoids the need to synchronize with
       the OS-level thread that runs Racket, but it effectively ignores
       the Racket procedure that is wrapped as @tech{callback} when
       the @tech{callback} is applied in a foreign thread.

       Foreign-thread detection to trigger @racket[async-apply] works
       only when Racket is compiled with OS-level thread support,
       which is the default for many platforms. If a callback with an
       @racket[async-apply] is called from foreign code in the same
       OS-level thread that runs Racket, then @racket[async-apply]
       is not used.}

]

@history[#:changed "6.3" @elem{Added the @racket[#:lock-name] argument.}]}

@defform/subs[#:literals (->> :: :)
              (_fun fun-option ... maybe-args type-spec ... ->> type-spec
                    maybe-wrapper)
              ([fun-option (code:line #:abi abi-expr)
                           (code:line #:save-errno save-errno-expr)
                           (code:line #:keep keep-expr)
                           (code:line #:atomic? atomic?-expr)
                           (code:line #:async-apply async-apply-expr)
                           (code:line #:lock-name lock-name-expr)
                           (code:line #:in-original-place? in-original-place?-expr)
                           (code:line #:retry (retry-id [arg-id init-expr]))]
               [maybe-args code:blank
                           (code:line formals ::)]
               [type-spec type-expr
                          (id : type-expr)
                          (type-expr = value-expr)
                          (id : type-expr = value-expr)]
               [maybe-wrapper code:blank
                              (code:line ->> output-expr)])]{

Creates a new function type.  The @racket[_fun] form is a convenient
syntax for the @racket[_cprocedure] type constructor. In its simplest
form, only the input @racket[type-expr]s and the output @racket[type-expr] are
specified, and each types is a simple expression, which creates a
straightforward function type.

For example,

@racketblock[
(_fun _string _int ->> _int)
]

specifies a function that receives a string and an integer
and returns an integer.

See @racket[_cprocedure] for information about the @racket[#:abi],
@racket[#:save-errno], @racket[#:keep], @racket[#:atomic?],
@racket[#:async-apply], and @racket[#:in-original-place?] options.

In its full form, the @racket[_fun] syntax provides an IDL-like
language that creates a wrapper function around the
primitive foreign function when the type is used for a @tech{callout}.
These wrappers can implement complex interfaces given simple
specifications:
@;
@itemlist[

 @item{The full form of each argument @racket[type-spec] can include
       an optional label and an expression. A label @racket[id :]
       makes the argument value accessible to later expressions using
       @racket[id]. A @racket[= value-expr] expression causes the
       wrapper function to calculates the argument for that position
       using @racket[value-expr], implying that the wrapper does not
       expect to be given an argument for that position.

       For example,

       @racketblock[
        (_fun (s : _string) (_int = (string-length s)) ->> _int)
       ]

       produces a wrapper that takes a single string argument and
       calls a foreign function that takes a string and an integer;
       the string's length is provided as the integer argument.}

 @item{If the optional @racket[output-expr] is specified, or if an
       expression is provided for the output type, then the expression
       specifies an expression that will be used as a return value for
       the function call, replacing the foreign function's result. The
       @racket[output-expr] can use any of the previous labels,
       including a label given for the output to access the foreign
       function's return value.

       For example,

       @racketblock[
        (_fun _string (len : _int) ->> (r : _int) ->> (min r len))
       ]

       produces a wrapper that returns the minimum of the foreign
       function's result and the given integer argument.}

 @item{A @racket[#:retry (retry-id [arg-id init-expr] ...)]
       specification binds @racket[retry-id] for use in an
       @racket[output-expr] for retrying the foreign call (normally in
       tail position). The function bound to @racket[retry-id] accepts
       each @racket[arg-id] as an argument, each @racket[arg-id] can
       be used in @racket[= value-expr]s, and each @racket[init-expr]s
       provides the initial value for the corresponding
       @racket[arg-id].

       For example,

       @racketblock[
        (_fun #:retry (again [count 0])
              _string _int ->> (r : _int)
              ->> (if (and (= r ERR_BUSY)
                           (< count 5))
                      (retry (add1 count))
                      r))
       ]

       produces a wrapper that calls the foreign function up to five
       times if it continues to produce a number equal to
       @racket[ERR_BUSY].}

 @item{In rare cases where complete control over the input arguments
       is needed, the wrapper's argument list can be specified as
       @racket[maybe-args] with a @racket[formals] as for
       @racket[lambda] (including keyword arguments and/or a ``rest''
       argument). When an argument @racket[type-spec] includes a label
       that matches an binding identifier in @racket[formals], then
       the identifier is used as the default value for the argument.
       All argument @racket[type-spec]s must include either explicit
       @racket[= value-expr] annotations or an implicit one through a
       matching label.

       For example,

       @racketblock[
         (_fun (n s) :: (s : _string) (n : _int) ->> _int)
       ]

       produces a wrapper that receives an integer and a string, but
       the foreign function receives the string first.}

]

@history[#:changed "6.2" @elem{Added the @racket[#:retry] option.}
         #:changed "6.3" @elem{Added the @racket[#:lock-name] option.}]}

@defproc[(function-ptr [ptr-or-proc (or cpointer? procedure?)]
                       [fun-type ctype?])
         cpointer?]{

Casts @racket[ptr-or-proc] to a function pointer of type @racket[fun-type].}

@defform-arrow{

A literal used in @racket[_fun] forms. (It's unfortunate that this
literal has the same name as @racket[->] from
@racketmodname[racket/contract], but it's a different binding.}}

@; ----------------------------------------------------------------------

@subsection[#:tag "foreign:custom-types"]{Custom Function Types}

The behavior of the @racket[_fun] type can be customized via
@deftech{custom function types}, which are pieces of syntax that can
behave as C types and C type constructors, but they can interact with
function calls in several ways that are not possible otherwise.  When
the @racket[_fun] form is expanded, it tries to expand each of the
given type expressions, and ones that expand to certain keyword-value
lists interact with the generation of the foreign function wrapper.
This expansion makes it possible to construct a single wrapper
function, avoiding the costs involved in compositions of higher-order
functions.

Custom function types are macros that expand to a sequence
@racket[(_key: _val ...)], where each @racket[_key:] is from a short list
of known keys.  Each key interacts with generated wrapper functions in
a different way, which affects how its corresponding argument is
treated:

@itemize[

 @item{@racket[type:] specifies the foreign type that should be used, if it is
   @racket[#f] then this argument does not participate in the foreign call.}

 @item{@racket[expr:] specifies an expression to be used for arguments of this
   type, removing it from wrapper arguments.}

 @item{@racket[bind:] specifies a name that is bound to the original
   argument if it is required later (e.g., @racket[_box] converts its
   associated value to a C pointer, and later needs to refer back to
   the original box).}

 @item{@racket[1st-arg:] specifies a name that can be used to refer to
   the first argument of the foreign call (good for common cases where
   the first argument has a special meaning, e.g., for method calls).}

 @item{@racket[prev-arg:] similar to @racket[1st-arg:], but refers to the
   previous argument.}

 @item{@racket[pre:] a pre-foreign code chunk that is used to change the
   argument's value.}

 @item{@racket[post:] a similar post-foreign code chunk.}

 @item{@racket[keywords:] specifies keyword/value expressions that will
   be used with the surrounding @racket[_fun] form.  (Note: the
   keyword/value sequence follows @racket[keywords:], not parenthesized.)}
]

The @racket[pre:] and @racket[post:] bindings can be of the form
@racket[(_id => _expr)] to use the existing value.  Note that if the
@racket[pre:] expression is not @racket[(_id => _expr)], then it means
that there is no input for this argument to the
@racket[_fun]-generated procedure.  Also note that if a custom type is
used as an output type of a function, then only the @racket[post:]
code is used.

Most custom types are meaningful only in a @racket[_fun] context, and
will raise a syntax error if used elsewhere.  A few such types can be
used in non-@racket[_fun] contexts: types which use only
@racket[type:], @racket[pre:], @racket[post:], and no others.  Such
custom types can be used outside a @racket[_fun] by expanding them
into a usage of @racket[make-ctype], using other keywords makes this
impossible, because it means that the type has specific interaction
with a function call.


@defform[(define-fun-syntax id transformer-expr)]{

Binds @racket[id] as a @tech{custom function type} as well as a syntax
transformer (i.e, macro). The type is expanded by applying the
procedure produced by @racket[transformer-expr] to a use of the
@tech{custom function type}.

For instance, the following defines a new type that automatically
coerces the input number to an inexact form which is compatible with
the @racket[_float] type.

@racketblock[
(define-fun-syntax _float*
  (syntax-id-rules (_float*)
    [_float* (type: _float pre: (x => (+ 0.0 x)))]))

(_fun _float* ->> _bool)]}

@defidform[_?]{

A @tech{custom function type} that is a marker for expressions that
should not be sent to the foreign function.  Use this to bind local
values in a computation that is part of an ffi wrapper interface, or
to specify wrapper arguments that are not sent to the foreign function
(e.g., an argument that is used for processing the foreign output).}


@defform/subs[#:literals (i o io)
              (_ptr mode type-expr)
              ([mode i o io])]{

Creates a C pointer type, where @racket[mode] indicates input or
output pointers (or both).  The @racket[mode] can be one of the
following:

@itemize[

 @item{@racket[i] --- indicates an @italic{input} pointer argument:
  the wrapper arranges for the function call to receive a value that
  can be used with the @racket[type] and to send a pointer to this
  value to the foreign function.  After the call, the value is
  discarded.}

 @item{@racket[o] --- indicates an @italic{output} pointer argument:
  the foreign function expects a pointer to a place where it will save
  some value, and this value is accessible after the call, to be used
  by an extra return expression.  If @racket[_ptr] is used in this
  mode, then the generated wrapper does not expect an argument since
  one will be freshly allocated before the call.}

 @item{@racket[io] --- combines the above into an
  @italic{input/output} pointer argument: the wrapper gets the Racket
  value, allocates and set a pointer using this value, and then
  references the value after the call.  The ``@racket[_ptr]'' name can
  be confusing here: it means that the foreign function expects a
  pointer, but the generated wrapper uses an actual value.  (Note that
  if this is used with structs, a struct is created when calling the
  function, and a copy of the return value is made too---which is
  inefficient, but ensures that structs are not modified by C code.)}

]

For example, the @racket[_ptr] type can be used in output mode to create a
foreign function wrapper that returns more than a single argument.  The
following type:

@racketblock[
(_fun (i : (_ptr o _int))
      ->> (d : _double)
      ->> (values d i))
]

creates a function that calls the foreign function with a fresh
integer pointer, and use the value that is placed there as a second
return value.}


@defidform[_box]{

A @tech{custom function type} similar to a @racket[(_ptr io _type)]
argument, where the input is expected to be a box holding an
appropriate value, which is unboxed on entry and modified accordingly
on exit.}

@defform/subs[(_list mode type maybe-len)
              ([mode i o io]
               [maybe-len code:blank
                          len-expr])]{

A @tech{custom function type} that is similar to @racket[_ptr], except
that it is used for converting lists to/from C vectors.  The optional
@racket[maybe-len] argument is needed for output values where it is used in
the post code, and in the pre code of an output mode to allocate the
block.  (If the length is 0, then NULL is passed in and an empty list is
returned.)  In either case, it can refer to a previous binding for the
length of the list which the C function will most likely require.}

@defform[(_vector mode type maybe-len)]{

A @tech{custom function type} like @racket[_list], except that it uses
Racket vectors instead of lists.}


@defform*[#:id _bytes
          #:literals (o)
          [_bytes
           (_bytes o len-expr)]]{

A @tech{custom function type} that can be used by itself as a simple
type for a byte string as a C pointer. Coercion of a C pointer to
simply @racket[_bytes] (without a specified length) requires that the pointer
refers to a nul-terminated byte string. When the length-specifying form is used
for a function argument, a byte string is allocated with the given
length, including an extra byte for the nul terminator.}


@; ------------------------------------------------------------

@section{C Struct Types}

@defproc[(make-cstruct-type [types (non-empty-listof ctype?)]
                            [abi (or/c #f 'default 'stdcall 'sysv) #f]
                            [alignment (or/c #f 1 2 4 8 16) #f]) 
         ctype?]{

The primitive type constructor for creating new C struct types.  These
types are actually new primitive types; they have no conversion
functions associated.  The corresponding Racket objects that are used
for structs are pointers, but when these types are used, the value
that the pointer @italic{refers to} is used, rather than the pointer
itself.  This value is basically made of a number of bytes that is
known according to the given list of @racket[types] list.

If @racket[alignment] is @racket[#f], then the natural alignment of
each type in @racket[types] is used for its alignment within the
struct type. Otherwise, @racket[alignment] is used for all struct type
members.}


@defproc[(_list-struct [#:alignment alignment (or/c #f 1 2 4 8 16) #f] 
                       [#:malloc-mode malloc-mode
                                      (one-of/c 'raw 'atomic 'nonatomic
                                                 'atomic-interior 'interior
                                                 'stubborn 'uncollectable 'eternal)
                                      'atomic]
                       [type ctype?] ...+)
         ctype?]{

A type constructor that builds a struct type using
@racket[make-cstruct-type] function and wraps it in a type that
marshals a struct as a list of its components.  Note that space for
structs must to be allocated using @racket[malloc] with @racket[malloc-mode]; the converter for a
@racket[_list-struct] type immediately allocates and uses a list from
the allocated space, so it is inefficient. Use @racket[define-cstruct]
below for a more efficient approach.

@history[#:changed "6.0.0.6" @elem{Added @racket[#:malloc-mode].}]}


@defform[(define-cstruct id/sup ([field-id type-expr field-option ...] ...)
           property ...)
         #:grammar [(id/sup _id
                            (_id _super-id))
                    (field-option (code:line #:offset offset-expr))
                    (property (code:line #:alignment alignment-expr)
                              (code:line #:malloc-mode malloc-mode-expr)
                              (code:line #:property prop-expr val-expr)
                              #:no-equal
                              #:define-unsafe)]
         #:contracts ([offset-expr exact-integer?]
                      [alignment-expr (or/c #f 1 2 4 8 16)]
                      [malloc-mode-expr (one-of/c 'raw 'atomic 'nonatomic
                                                  'atomic-interior 'interior
                                                  'stubborn 'uncollectable 'eternal)]
                      [prop-expr struct-type-property?])]{

Defines a new C struct type, but unlike @racket[_list-struct], the
resulting type deals with C structs in binary form, rather than
marshaling them to Racket values.  The syntax is similar to
@racket[define-struct], providing accessor functions for raw struct
values (which are pointer objects); the @racket[_id]
must start with @litchar{_}, at most one @racket[#:offset] can be
supplied for a field, and at most one @racket[#:alignment]
or @racket[#:malloc-mode] can be supplied. If no @racket[_super-id]
is provided, then at least one field must be specified.

The resulting bindings are as follows:

@itemize[

 @item{@racket[_id] : the new C type for this struct.}

 @item{@racket[_id]@racketidfont{-pointer}: a pointer type that should
  be used when a pointer to values of this struct are used.}

 @item{@racket[_id]@racketidfont{-pointer/null}: like
  @racket[_id]@racketidfont{-pointer}, but allowing NULL pointers (as
  represented on the Racket side by @racket[#f]).}

 @item{@racketvarfont{id}@racketidfont{?}: a predicate for the new type.}

 @item{@racketvarfont{id}@racketidfont{-tag}: the tag object that is
  used with instances.  The tag object may be the symbol form of 
  @racketvarfont{id} or a list of symbols containing the @racketvarfont{id}
  symbol and other symbols, such as the @racketvarfont{super-id} symbol.}

 @item{@racketidfont{make-}@racketvarfont{id} : a constructor, which expects
  an argument for each field.}

 @item{@racketvarfont{id}@racketidfont{-}@racket[field-id] : an accessor
  function for each @racket[field-id]; if the field has a C struct type, then
  the result of the accessor is a pointer to the field within the
  enclosing structure, rather than a  copy of the field.}

 @item{@racketidfont{set-}@racketvarfont{id}@racketidfont{-}@racket[field-id]@racketidfont{!}
  : a mutator function for each @racket[field-id].}

 @item{@racketvarfont{id}@racketidfont{-}@racket[field-id]@racketidfont{-offset}
  : the absolute offset, in bytes, of each @racket[field-id], if @racket[#:define-unsafe] is present.}

 @item{@racketidfont{unsafe-}@racketvarfont{id}@racketidfont{-}@racket[field-id]
  : an unsafe accessor function for each @racket[field-id], if @racket[#:define-unsafe] is present.}

 @item{@racketidfont{unsafe-set-}@racketvarfont{id}@racketidfont{-}@racket[field-id]@racketidfont{!}
  : an unsafe mutator function for each @racket[field-id], if @racket[#:define-unsafe] is present.}

@item{@racketvarfont{id}: structure-type information compatible with
  @racket[struct-out] or @racket[match] (but not @racket[struct] or 
  @racket[define-struct]);
  currently, this information is correct only when no @racket[super-id]
  is specified.}

 @item{@racketvarfont{id}@racketidfont{->list},
  @racketidfont{list->}@racketvarfont{id} : a function that converts a
  struct into a list of field values and vice versa.}

 @item{@racketvarfont{id}@racketidfont{->list*},
  @racketidfont{list*->}@racketvarfont{id} : like
  @racketvarfont{id}@racketidfont{->list},
  @racketidfont{list->}@racketvarfont{id}, but fields that are structs
  are recursively unpacked to lists or packed from lists.}

 @item{@racketidfont{struct:cpointer:}@racketvarfont{id}:
  only when a @racket[#:property] is specified --- a structure type that 
  corresponds to a wrapper to reflect properties (see below).}

]

Objects of the new type are actually C pointers, with a type tag that
is the symbol form of @racketvarfont{id} or a list that contains the 
symbol form of @racketvarfont{id}.  Since
structs are implemented as pointers, they can be used for a
@racket[_pointer] input to a foreign function: their address will be
used.  To make this a little safer, the corresponding cpointer type is
defined as @racket[_id]@racketidfont{-pointer}.  The @racket[_id] type
should not be used when a pointer is expected, since it will cause the
struct to be copied rather than use the pointer value, leading to
memory corruption.

Field offsets within the structure are normally computed
automatically, but the offset for a field can be specified with
@racket[#:offset]. Specifying @racket[#:offset] for a field affects
the default offsets computed for all remaining fields.

Instances of the new type are not normally Racket structure instances.
However, if at least one @racket[#:property] modifier is specified,
then struct creation and coercions from @racket[_id] variants wrap a
non-NULL C pointer representation in a Racket structure that has the
specified properties. The wrapper Racket structure also has a
@racket[prop:cpointer] property, so that wrapped C pointers can be
treated the same as unwrapped C pointers. If a @racket[super-id] is
provided and it corresponds to a C struct type with a wrapper
structure type, then the wrapper structure type is a subtype of
@racket[super-id]'s wrapper structure type. If a @racket[#:property]
modifier is specified, @racket[#:no-equal] is not specified,
and if @racket[prop:equal+hash] is not specified as any @racket[#:property],
then the @racket[prop:equal+hash] property is automatically implemented
for the wrapper structure type to use @racket[ptr-equal?].

If the first field is itself a C struct type, its tag will be used in
addition to the new tag.  This feature supports common cases of object
inheritance, where a sub-struct is made by having a first field that
is its super-struct.  Instances of the sub-struct can be considered as
instances of the super-struct, since they share the same initial
layout.  Using the tag of an initial C struct field means that the same
behavior is implemented in Racket; for example, accessors and mutators
of the super-struct can be used with the new sub-struct.  See the
example below.

Providing a @racket[super-id] is shorthand for using an initial field
named @racket[super-id] and using @racketidfont{_}@racket[super-id]
as its type.  Thus, the new struct will use
@racketidfont{_}@racket[super-id]'s tag in addition to its own tag,
meaning that instances of @racket[_id] can be used as instances of
@racketidfont{_}@racket[super-id].  Aside from the syntactic sugar,
the constructor function is different when this syntax is used:
instead of expecting a first argument that is an instance of
@racketidfont{_}@racket[super-id], the constructor will expect
arguments for each of @racketidfont{_}@racket[super-id]'s fields, in
addition for the new fields.  This adjustment of the constructor is,
again, in analogy to using a supertype with @racket[define-struct].

Structs are allocated using @racket[malloc] with the result of
@racket[malloc-mode-expr],  which default to @racket['atomic].
The default allocation of @racket['atomic] means that the
garbage collector ignores the content of a struct; thus, struct fields can hold
only non-pointer values, pointers to memory outside the GC's control,
and otherwise-reachable pointers to immobile GC-managed values (such
as those allocated with @racket[malloc] and @racket['internal] or
@racket['internal-atomic]).

As an example, consider the following C code:

@verbatim[#:indent 2]{
 typedef struct { int x; char y; } A;
 typedef struct { A a; int z; } B;

 A* makeA() {
   A *p = malloc(sizeof(A));
   p->x = 1;
   p->y = 2;
   return p;
 }

 B* makeB() {
   B *p = malloc(sizeof(B));
   p->a.x = 1;
   p->a.y = 2;
   p->z   = 3;
   return p;
 }

 char gety(A* a) {
   return a->y;
 }
}

Using the simple @racket[_list-struct], you might expect this code to
work:

@racketblock[
(define makeB
  (get-ffi-obj 'makeB "foo.so"
    (_fun ->> (_list-struct (_list-struct _int _byte) _int))))
(makeB) (code:comment @#,t{should return @racket['((1 2) 3)]})
]

The problem here is that @cpp{makeB} returns a pointer to the struct rather
than the struct itself.  The following works as expected:

@racketblock[
(define makeB
  (get-ffi-obj 'makeB "foo.so" (_fun ->> _pointer)))
(ptr-ref (makeB) (_list-struct (_list-struct _int _byte) _int))
]

As described above, @racket[_list-struct]s should be used in cases where
efficiency is not an issue.  We continue using @racket[define-cstruct], first
define a type for @cpp{A} which makes it possible to use @cpp{makeA}:

@racketblock[
(define-cstruct #,(racketidfont "_A") ([x _int] [y _byte]))
(define makeA
  (get-ffi-obj 'makeA "foo.so"
    (_fun ->> #,(racketidfont "_A-pointer")))) (code:comment @#,t{using @racketidfont{_A} is a memory-corrupting bug!})
(define a (makeA))
(list a (A-x a) (A-y a))
(code:comment @#,t{produces an @racket[A] containing @racket[1] and @racket[2]})
]

Using @cpp{gety} is also simple:

@racketblock[
(define gety
  (get-ffi-obj 'gety "foo.so"
    (_fun #,(racketidfont "_A-pointer") ->> _byte)))
(gety a) (code:comment @#,t{produces @racket[2]})
]

We now define another C struct for @cpp{B}, and expose @cpp{makeB}
using it:

@racketblock[
(define-cstruct #,(racketidfont "_B") ([a #,(racketidfont "_A")] [z _int]))
(define makeB
  (get-ffi-obj 'makeB "foo.so"
    (_fun ->> #,(racketidfont "_B-pointer"))))
(define b (makeB))
]

We can access all values of @racket[b] using a naive approach:

@racketblock[
(list (A-x (B-a b)) (A-y (B-a b)) (B-z b))
]

but this is inefficient as it allocates and copies an instance of
@cpp{A} on every access.  Inspecting the tags @racket[(cpointer-tag
b)] we can see that @cpp{A}'s tag is included, so we can simply use
its accessors and mutators, as well as any function that is defined to
take an @cpp{A} pointer:

@racketblock[
(list (A-x b) (A-y b) (B-z b))
(gety b)
]

Constructing a @cpp{B} instance in Racket requires allocating a
 temporary @cpp{A} struct:

@racketblock[
(define b (make-B (make-A 1 2) 3))
]

To make this more efficient, we switch to the alternative
@racket[define-cstruct] syntax, which creates a constructor that
expects arguments for both the super fields and the new ones:

@racketblock[
 (define-cstruct (#,(racketidfont "_B") #,(racketidfont "_A")) ([z _int]))
 (define b (make-B 1 2 3))
]

@history[#:changed "6.0.0.6" @elem{Added @racket[#:malloc-mode].}
#:changed "6.1.1.8" @elem{Added @racket[#:offset] for fields.}
#:changed "6.3.0.13" @elem{Added @racket[#:define-unsafe].}]}

@; ------------------------------------------------------------

@section{C Array Types}

@defproc[(make-array-type [type ctype?]
                          [count exact-nonnegative-integer?])
         ctype?]{

The primitive type constructor for creating new C array types. Like C
struct types, array types are new primitive types with no conversion
functions associated. When used as a function argument or return type,
array types behave like pointer types; otherwise, array types behave
like struct types (i.e., a struct with as many fields as the array has
elements), particularly when used for a field within a struct type.

Since an array is treated like a struct, @racket[cast]ing a
pointer type to an array type does not work. Instead, use
@racket[ptr-ref] with a pointer, an array type constructed with
@racket[_array], and index @racket[0] to convert a pointer to a Racket
representation that works with @racket[array-ref] and
@racket[array-set!].}


@defproc[(_array [type ctype?] [count exact-nonnegative-integer?] ...+)
         ctype?]{

Creates an array type whose Racket representation is an array that
works with @racket[array-ref] and @racket[array-set!]. The array is
not copied; the Racket representation is backed by the underlying C
representation.

Supply multiple @racket[count]s for a multidimensional array. Since C
uses row-major order for arrays, @racket[(_array _t _n _m)] is
equivalent to @racket[(_array (_array _t _m) _n)], which is different
from an array of pointers to arrays.}


@defproc[(array? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a Racket representation of a C
value via @racket[_array], @racket[#f] otherwise.}


@defproc[(array-ref [a array?] [i exact-nonnegative-integer?] ...+)
         any/c]{

Extracts an element from an array. Use multiple @racket[i] indices for
a multidimensional array access; using fewer indices than the array
dimension produces a sub-array.}


@defproc[(array-set! [a array?] 
                     [i exact-nonnegative-integer?] ...+
                     [v any/c])
         void?]{

Sets an element in an array. Use multiple @racket[i] indices for a
multidimensional array update; using fewer indices than the array
dimension sets a sub-array (i.e., @racket[v] must be an array of the
same size as the sub-array and @racket[v] is copied into the
sub-array).}


@defproc[(array-ptr [a array?]) cpointer?]{

Extracts the pointer for an array's storage.}


@defproc[(array-length [a array?]) exact-nonnegative-integer?]{

Extracts the length of an array. For a multidimensional array, the
result is still a single number; extract an element to get
a sub-array to get the length of the next dimension, and so on.}

@defproc[(array-type [a array?]) ctype?]{

Extracts the type of the array. For a multidimensional array, the
result is the ctype of the nested array.}

@defproc[(in-array [a array?]
                  [start exact-nonnegative-integer? 0]
                  [stop (or/c exact-integer? #f) #f]
                  [step (and/c exact-integer? (not/c zero?)) 1])
         sequence?]{
  Returns a sequence equivalent to @racket[a] when no optional
  arguments are supplied.

  The optional arguments @racket[start], @racket[stop], and
  @racket[step] are as in @racket[in-vector].}

@defproc[(_array/list [type ctype?] [count exact-nonnegative-integer?] ...+)
         ctype?]{

Like @racket[_array], but the Racket representation is a list (or list
of lists for a multidimensional array) of elements copied to and from
an underlying C array.}


@defproc[(_array/vector [type ctype?] [count exact-nonnegative-integer?] ...+)
         ctype?]{

Like @racket[_array], but the Racket representation is a vector (or
vector of vectors for a multidimensional array) of elements copied to
and from an underlying C array.}


@; ------------------------------------------------------------

@section{C Union Types}

@defproc[(make-union-type [type ctype?] ...+)
         ctype?]{

The primitive type constructor for creating new C union types. Like C
struct types, union types are new primitive types with no conversion
functions associated. Unions are always treated like structs.}


@defproc[(_union [type ctype?] ...+)
         ctype?]{

Creates a union type whose Racket representation is a union that
works with @racket[union-ref] and @racket[union-set!]. The union is
not copied; the Racket representation is backed by the underlying C
representation.}


@defproc[(union? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a Racket representation of a C
value via @racket[_union], @racket[#f] otherwise.}


@defproc[(union-ref [u union?] [i exact-nonnegative-integer?])
         any/c]{

Extracts a variant from a union.}


@defproc[(union-set! [u union?] 
                     [i exact-nonnegative-integer?]
                     [v any/c])
         void?]{

Sets a variant in a union..}


@defproc[(union-ptr [u array?]) cpointer?]{

Extracts the pointer for a union's storage.}


@; ------------------------------------------------------------

@section{Enumerations and Masks}

Although the constructors below are describes as procedures, they are
implemented as syntax, so that error messages can report a type name
where the syntactic context implies one.

@defproc[(_enum [symbols list?]
                [basetype ctype? _ufixint]
                [#:unknown unknown any/c (lambda (x) (error ....))])
         ctype?]{

Takes a list of symbols and generates an enumeration type.  The
enumeration maps between a symbol in the given @racket[symbols] list and
corresponding integers, counting from @racket[0].

The list @racket[symbols] can also set the values of symbols by
putting @racket['=] and an exact integer after the symbol.  For
example, the list @racket['(x y = 10 z)] maps @racket['x] to
@racket[0], @racket['y] to @racket[10], and @racket['z] to
@racket[11].

The @racket[basetype] argument specifies the base type to use.

The @racket[unknown] argument specifies the result of converting an
unknown integer from the foreign side: it can be a one-argument function
to be applied on the integer, or a value to return instead.  The default
is to throw an exception.

@examples[#:eval ffi-eval
  (code:comment "example from snappy-c.h")
  (define @#,racketidfont{_snappy_status}
    (_enum '(ok = 0
             invalid_input
             buffer_too_small)))
]}

@defproc[(_bitmask [symbols (or symbol? list?)] [basetype ctype? _uint])
         ctype?]{

Similar to @racket[_enum], but the resulting mapping translates a list
of symbols to a number and back, using @racket[bitwise-ior] on the
values of individual symbols, where A single symbol is equivalent to a
list containing just the symbol.

When a symbol does not have a given value (via @racket['=] after the
symbol in @racket[symbols]), its value is the next power of 2 greater
than the previous symbol's assignment (or @racket[1] for the first
symbol).

The default @racket[basetype] is @racket[_uint], since high bits are
often used for flags.

@examples[#:eval ffi-eval
  (code:comment "example from curl.h")
  (define @#,racketidfont{_curl_global_flag}
    (_bitmask `(CURL_GLOBAL_SSL = 1
                CURL_GLOBAL_WIN32 = 2
                CURL_GLOBAL_ALL = 3
                CURL_GLOBAL_NOTHING = 0
                CURL_GLOBAL_DEFAULT = 3
                CURL_GLOBAL_ACK_EINTR = 4)))
  (code:comment "example from XOrg")
  (define @#,racketidfont{_Modifiers}
    (_bitmask '(ShiftMask = #b0000000000001
                LockMask = #b0000000000010
                ControlMask = #b0000000000100
                Mod1Mask = #b0000000001000
                Mod2Mask = #b0000000010000
                Mod3Mask = #b0000000100000
                Mod4Mask = #b0000001000000
                Mod5Mask = #b0000010000000
                Button1Mask = #b0000100000000
                Button2Mask = #b0001000000000
                Button3Mask = #b0010000000000
                Button4Mask = #b0100000000000
                Button5Mask = #b1000000000000
                Any = #x8000)))
]}

@close-eval[ffi-eval]

