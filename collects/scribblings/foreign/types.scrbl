#lang scribble/doc
@(require "utils.ss"
          (for-label scheme/match))

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
                     [scheme-to-c (or/c #f (any/c . -> . any))]
                     [c-to-scheme (or/c #f (any/c . -> . any))])
         ctype?]{

Creates a new @tech{C type} value whose representation for foreign
code is the same as @scheme[type]'s. The given conversions functions
convert to and from the Racket representation of @scheme[type]. Either
conversion function can be @scheme[#f], meaning that the conversion
for the corresponding direction is the identity function.  If both
functions are @scheme[#f], @scheme[type] is returned.}


@defproc[(ctype? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{C type}, @scheme[#f]
otherwise.}


@defproc*[([(ctype-sizeof [type ctype?]) exact-nonnegative-integer?]
           [(ctype-alignof [type ctype?]) exact-nonnegative-integer?])]{

Returns the size or alignment of a given @scheme[type] for the current
platform.}


@defproc[(ctype->layout [type ctype?]) (flat-rec-contract rep
                                         symbol?
                                         (listof rep))]{

Returns a value to describe the eventual C representation of the
type. It can be any of the following symbols:

@schemeblock[
  'int8 'uint8 'int16 'uint16 'int32 'uint32 'int64 'uint64
  'float 'double 'bool 'void 'pointer 'fpointer 
  'bytes 'string/ucs-4 'string/utf-16
]

The result can also be a list, which describes a C struct whose
element representations are provided in order within the list.}


@defproc[(compiler-sizeof [sym symbol?]) exact-nonnegative-integer?]{

Possible values for @scheme[symbol] are @scheme['int], @scheme['char],
@scheme['short], @scheme['long], @scheme['*], @scheme['void],
@scheme['float], @scheme['double]. The result is the size of the
correspond type according to the C @cpp{sizeof} operator for the
current platform. The @scheme[compiler-sizeof] operation should be
used to gather information about the current platform, such as
defining alias type like @scheme[_int] to a known type like
@scheme[_int32].}

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

The basic integer types at various sizes. The @schemeidfont{s} or
@schemeidfont{u} prefix specifies a signed or an unsigned integer,
respectively; the ones with no prefix are signed.}

@defthing*[([_byte ctype?]
            [_sbyte ctype?]
            [_ubyte ctype?]
            [_short ctype?]
            [_sshort ctype?]
            [_ushort ctype?]
            [_int ctype?]
            [_sint ctype?]
            [_uint ctype?]
            [_word ctype?]
            [_sword ctype?]
            [_uword ctype?]
            [_long ctype?]
            [_slong ctype?]
            [_ulong ctype?])]{

Aliases for basic integer types. The @scheme[_byte] aliases correspond
to @scheme[_int8]. The @scheme[_short] and @scheme[_word] aliases
correspond to @scheme[_int16]. The @scheme[_int] aliases correspond to
@scheme[_int32]. The @scheme[_long] aliases correspond to either
@scheme[_int32] or @scheme[_int64], depending on the platform.}

@defthing*[([_fixnum ctype?]
            [_ufixnum ctype?])]{

For cases where speed matters and where you know that the integer is
small enough, the types @scheme[_fixnum] and @scheme[_ufixnum] are
similar to @scheme[_long] and @scheme[_ulong] but assume that the
quantities fit in Racket's immediate integers (i.e., not bignums).}

@defthing*[([_fixint ctype?]
            [_ufixint ctype?])]{

Like @scheme[_fixnum] and @scheme[_ufixnum], but coercions from C are
checked to be in range.}

@defthing*[([_float ctype?]
            [_double ctype?]
            [_double* ctype?])]{

The @scheme[_float] and @scheme[_double] types represent the
corresponding C types. The type @scheme[_double*] that implicitly
coerces any real number to a C @cpp{double}.}

@; ------------------------------------------------------------

@section{Other Atomic Types}

@defthing[_bool ctype?]{

Translates @scheme[#f] to a @scheme[0] @scheme[_int], and any other
value to @scheme[1].}

@defthing[_void ctype?]{

Indicates a Racket @|void-const| return value, and it cannot be used
to translate values to C. This type cannot be used for function
inputs.}

@; ------------------------------------------------------------

@section{String Types}

@subsection{Primitive String Types}

The five primitive string types correspond to cases where a C
representation matches Racket's representation without encodings.

The form @scheme[_bytes] form can be used type for Racket byte
strings, which corresponds to C's @cpp{char*} type.  In addition to
translating byte strings, @scheme[#f] corresponds to the @cpp{NULL}
pointer.

@deftogether[(
@defthing[_string/ucs-4 ctype?]
)]{

A type for Racket's native Unicode strings, which are in UCS-4 format.
These correspond to the C @cpp{mzchar*} type used by Racket. As usual, the types
treat @scheme[#f] as @cpp{NULL} and vice-versa.}


@deftogether[(
@defthing[_string/utf-16 ctype?]
)]{

Unicode strings in UTF-16 format. As usual, the types treat
@scheme[#f] as @cpp{NULL} and vice-versa.}


@defthing[_path ctype?]{

Simple @cpp{char*} strings, corresponding to Racket's paths. As usual,
the types treat @scheme[#f] as @cpp{NULL} and vice-versa.

Beware that changing the current directory via
@scheme[current-directory] does not change the OS-level current
directory as seen by foreign library functions. Paths normally should
be converted to absolute form using @scheme[path->complete-path]
(which uses the @scheme[current-directory] parameter) before passing
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
treat @scheme[#f] as @cpp{NULL} and vice-versa.}

@defthing*[([_string*/utf-8 ctype?]
            [_string*/latin-1 ctype?]
            [_string*/locale ctype?])]{

Similar to @scheme[_string/utf-8], etc., but accepting a wider range
of values: Racket byte strings are allowed and passed as is, and
Racket paths are converted using @scheme[path->bytes].}


@subsection{Variable Auto-Converting String Type}

The @scheme[_string/ucs-4] type is rarely useful when interacting with
foreign code, while using @scheme[_bytes] is somewhat unnatural, since
it forces Racket programmers to use byte strings. Using
@scheme[_string/utf-8], etc., meanwhile, may prematurely commit to a
particular encoding of strings as bytes. The @scheme[_string] type
supports conversion between Racket strings and @cpp{char*} strings
using a parameter-determined conversion.

@defthing[_string ctype?]{

Expands to a use of the @scheme[default-_string-type] parameter.  The
parameter's value is consulted when @scheme[_string] is evaluated, so
the parameter should be set before any interface definition that uses
@scheme[_string].}

@defparam[default-_string-type type ctype?]{

A parameter that determines the current meaning of @scheme[_string].
It is initially set to @scheme[_string*/utf-8].  If you change it, do
so @italic{before} interfaces are defined.}


@subsection{Other String Types}

@defthing[_file ctype?]{

Like @scheme[_path], but when values go from Racket to C,
@scheme[cleanse-path] is used on the given value.  As an output value,
it is identical to @scheme[_path].}

@defthing[_bytes/eof ctype?]{

Similar to the @scheme[_bytes] type, except that a foreign return
value of @cpp{NULL} is translated to a Racket @scheme[eof] value.}

@defthing[_string/eof ctype?]{

Similar to the @scheme[_string] type, except that a foreign return
value of @cpp{NULL} is translated to a Racket @scheme[eof] value.}

@; ------------------------------------------------------------

@section{Pointer Types}

@defthing[_pointer ctype?]{

Corresponds to Racket ``C pointer'' objects.  These pointers can have
an arbitrary Racket object attached as a type tag.  The tag is ignored
by built-in functionality; it is intended to be used by interfaces.
See @secref["foreign:tagged-pointers"] for creating pointer types that
use these tags for safety. A @scheme[#f] value is converted to
@cpp{NULL} and vice-versa.

The address referenced by a @scheme[_pointer] value must not refer to
memory managed by the garbage collector (unless the address
corresponds to a value that supports interior pointers and that is
otherwise referenced to preserve the value from garbage collection).
The reference is not traced or updated by the garbage collector.}


@defthing[_gcpointer ctype?]{

Like @scheme[_pointer], but for a value that can refer to memory
managed by the garbage collector.

Although a @scheme[_gcpointer] can reference to memory that is not
managed by the garbage collector, beware of using an address that
might eventually become managed by the garbage collector. For example,
if a reference is created by @scheme[malloc] with @scheme['raw] and
released by @scheme[free], then the @scheme[free] may allow the memory
formerly occupied by the reference to be used later by the garbage
collector.}


@deftogether[(
@defthing[_racket ctype?]
@defthing[_scheme ctype?]
)]{

A type that can be used with any Racket object; it corresponds to the
@cpp{Scheme_Object*} type of Racket's C API (see
@|InsideRacket|).  It is useful only for libraries that are aware of
Racket's C API.}


@defthing[_fpointer ctype?]{

Similar to @scheme[_pointer], except that when an @scheme[_fpointer]
is extracted from a pointer produced by @scheme[ffi-obj-ref], then a
level of indirection is skipped. A level of indirection is similarly
skipped when extracting a pointer via @scheme[get-ffi-obj]. Like
@scheme[_pointer], @scheme[_fpointer] treats @scheme[#f] as @cpp{NULL}
and vice-versa.

A type generated by @scheme[_cprocedure] builds on @scheme[_fpointer],
and normally @scheme[_cprocedure] should be used instead of
@scheme[_fpointer].}


@defproc[(_or-null [ctype ctype?]) ctype?]{

Creates a type that is like @scheme[ctype], but @scheme[#f] is
converted to @cpp{NULL} and vice-versa. The given @scheme[ctype] must
have the same C representation as @scheme[_pointer],
@scheme[_gcpointer], or @scheme[_fpointer].}


@defproc[(_gcable [ctype ctype?]) ctype?]{

Creates a type that is like @scheme[ctype], but whose base
representation is like @scheme[_gcpointer] instead of
@scheme[_pointer]. The given @scheme[ctype] must have a base
representation like @scheme[_pointer] or @scheme[_gcpointer] (and in
the later case, the result is the @scheme[ctype]).}


@; ------------------------------------------------------------

@section[#:tag "foreign:procedures"]{Function Types}

@defproc[(_cprocedure [input-types (list ctype?)]
                      [output-type ctype?]
                      [#:abi abi (or/c symbol/c #f) #f]
                      [#:atomic? atomic? any/c #f]
                      [#:save-errno save-errno (or/c #f 'posix 'windows) #f]
                      [#:wrapper wrapper (or/c #f (procedure? . -> . procedure?))
                                         #f]
                      [#:keep keep (or/c boolean? box? (any/c . -> . any/c))
                                   #t])
         any]{

A type constructor that creates a new function type, which is
specified by the given @scheme[input-types] list and @scheme[output-type].
Usually, the @scheme[_fun] syntax (described below) should be used
instead, since it manages a wide range of complicated cases.

The resulting type can be used to reference foreign functions (usually
@scheme[ffi-obj]s, but any pointer object can be referenced with this type),
generating a matching foreign callout object.  Such objects are new primitive
procedure objects that can be used like any other Racket procedure.
As with other pointer types, @scheme[#f] is treated as a @cpp{NULL}
function pointer and vice-versa.

A type created with @scheme[_cprocedure] can also be used for passing
Racket procedures to foreign functions, which will generate a foreign
function pointer that calls the given Racket procedure when it is
used.  There are no restrictions on the Racket procedure; in
particular, its lexical context is properly preserved.

The optional @scheme[abi] keyword argument determines the foreign ABI
that is used.  @scheme[#f] or @scheme['default] will use a
platform-dependent default; other possible values are
@scheme['stdcall] and @scheme['sysv] (the latter corresponds to
``cdecl'').  This is especially important on Windows, where most
system functions are @scheme['stdcall], which is not the default.

If @scheme[atomic?] is true, then when a Racket procedure is given
this procedure type and called from foreign code, then the Racket
process is put into atomic mode while evaluating the Racket procedure
body. In atomic mode, other Racket threads do not run, so the Racket
code must not call any function that potentially synchronizes with
other threads, or else it may deadlock. In addition, the Racket code
must not perform any potentially blocking operation (such as I/O), it
must not raise an uncaught exception, it must not perform any escaping
continuation jumps, and its non-tail recursion must be minimal to
avoid C-level stack overflow; otherwise, the process may crash or
misbehave.

If @scheme[save-errno] is @scheme['posix], then the value of
@as-index{@tt{errno}} is saved (specific to the current thread)
immediately after a foreign function returns. The saved value is
accessible through @scheme[saved-errno]. If @scheme[save-errno] is
@scheme['windows], then the value of
@as-index{@tt{GetLastError}}@tt{()} is saved for later use via
@scheme[saved-errno]; the @scheme['windows] option is available only
under Windows (on other platforms @scheme[saved-errno] will return
0). If @scheme[save-errno] is @scheme[#f], no error value is saved
automatically. The error-recording support provided by
@scheme[save-errno] is needed because the Racket runtime system
may otherwise preempt the current Racket thread and itself call
functions that set error values.

The optional @scheme[wrapper], if provided, is expected to be a
function that can change a callout procedure: when a callout is
generated, the wrapper is applied on the newly created primitive
procedure, and its result is used as the new function.  Thus,
@scheme[wrapper] is a hook that can perform various argument
manipulations before the foreign function is invoked, and return
different results (for example, grabbing a value stored in an
``output'' pointer and returning multiple values).  It can also be
used for callbacks, as an additional layer that tweaks arguments from
the foreign code before they reach the Racket procedure, and possibly
changes the result values too.

Sending Racket functions as callbacks to foreign code is achieved by
translating them to a foreign ``closure,'' which foreign code can call
as plain C functions.  Additional care must be taken in case the
foreign code might hold on to the callback function.  In these cases
you must arrange for the callback value to not be garbage-collected,
or the held callback will become invalid.  The optional @scheme[keep]
keyword argument is used to achieve this.  It can have the following
values: @itemize[

@item{@scheme[#t] makes the callback value stay in memory as long as
  the converted function is.  In order to use this, you need to hold
  on to the original function, for example, have a binding for it.
  Note that each function can hold onto one callback value (it is
  stored in a weak hash table), so if you need to use a function in
  multiple callbacks you will need to use one of the last two
  options below.  (This is the default, as it is fine in most cases.)}

@item{@scheme[#f] means that the callback value is not held.  This may
  be useful for a callback that is only used for the duration of the
  foreign call --- for example, the comparison function argument to
  the standard C library @tt{qsort} function is only used while
  @tt{qsort} is working, and no additional references to the
  comparison function are kept.  Use this option only in such cases,
  when no holding is necessary and you want to avoid the extra cost.}

@item{A box holding @scheme[#f] (or a callback value) --- in this case
  the callback value will be stored in the box, overriding any value
  that was in the box (making it useful for holding a single callback
  value).  When you know that it is no longer needed, you can
  ``release'' the callback value by changing the box contents, or by
  allowing the box itself to be garbage-collected.  This is can be
  useful if the box is held for a dynamic extent that corresponds to
  when the callback is needed; for example, you might encapsulate some
  foreign functionality in a Racket class or a unit, and keep the
  callback box as a field in new instances or instantiations of the
  unit.}

@item{A box holding @scheme[null] (or any list) -- this is similar to
  the previous case, except that new callback values are consed onto
  the contents of the box.  It is therefore useful in (rare) cases
  when a Racket function is used in multiple callbacks (that is, sent
  to foreign code to hold onto multiple times).}

@item{Finally, if a one-argument function is provided as
  @scheme[keep], it will be invoked with the callback value when it
  is generated.  This allows you to grab the value directly and use it
  in any way.}

]}

@defform/subs[#:literals (-> :: :)
              (_fun fun-option ... maybe-args type-spec ... -> type-spec
                    maybe-wrapper)
              ([fun-option (code:line #:abi abi-expr)
                           (code:line #:save-errno save-errno-expr)
                           (code:line #:keep keep-expr)
                           (code:line #:atomic? atomic?-expr)]
               [maybe-args code:blank
                           (code:line (id ...) ::)
                           (code:line id ::)
                           (code:line (id ... . id) ::)]
               [type-spec type-expr
                          (id : type-expr)
                          (type-expr = value-expr)
                          (id : type-expr = value-expr)]
               [maybe-wrapper code:blank
                               (code:line -> output-expr)])]{

Creates a new function type.  The @scheme[_fun] form is a convenient
syntax for the @scheme[_cprocedure] type constructor. In its simplest
form, only the input @scheme[type-expr]s and the output @scheme[type-expr] are
specified, and each types is a simple expression, which creates a
straightforward function type.

For instance,

@schemeblock[
(_fun _int _string -> _int)
]

specifies a function that receives an integer and a
string, and returns an integer.

In its full form, the @scheme[_fun] syntax provides an IDL-like
language that can be used to create a wrapper function around the
primitive foreign function.  These wrappers can implement complex
foreign interfaces given simple specifications. The full form of each
of the type specifications can include an optional label and an
expression. If a @scheme[= value-expr] is provided, then the resulting
function will be a wrapper that calculates the argument for that
position itself, meaning that it does not expect an argument for that
position.  The expression can use previous arguments if they were
labeled with @scheme[id :].  In addition, the result of a function
call need not be the value returned from the foreign call: if the
optional @scheme[output-expr] is specified, or if an expression is
provided for the output type, then this specifies an expression that
will be used as a return value.  This expression can use any of the
previous labels, including a label given for the output which can be
used to access the actual foreign return value.

In rare cases where complete control over the input arguments is needed, the
wrapper's argument list can be specified as @scheme[args], in any form (including
a ``rest'' argument).  Identifiers in this place are related to type labels, so
if an argument is there is no need to use an expression.

For example,

@schemeblock[
(_fun (n s) :: (s : _string) (n : _int) -> _int)
]

specifies a function that receives an integer and a string, but the
foreign function receives the string first.}

@defproc[(function-ptr [ptr-or-proc (or cpointer? procedure?)]
                       [fun-type ctype?])
         cpointer?]{

Casts @scheme[ptr-or-proc] to a function pointer of type @scheme[fun-type].}

@; ----------------------------------------------------------------------

@subsection[#:tag "foreign:custom-types"]{Custom Function Types}

The behavior of the @scheme[_fun] type can be customized via
@deftech{custom function types}, which are pieces of syntax that can
behave as C types and C type constructors, but they can interact with
function calls in several ways that are not possible otherwise.  When
the @scheme[_fun] form is expanded, it tries to expand each of the
given type expressions, and ones that expand to certain keyword-value
lists interact with the generation of the foreign function wrapper.
This expansion makes it possible to construct a single wrapper
function, avoiding the costs involved in compositions of higher-order
functions.

Custom function types are macros that expand to a sequence
@scheme[(_key: _val ...)], where each @scheme[_key:] is from a short list
of known keys.  Each key interacts with generated wrapper functions in
a different way, which affects how its corresponding argument is
treated:

@itemize[

 @item{@scheme[type:] specifies the foreign type that should be used, if it is
   @scheme[#f] then this argument does not participate in the foreign call.}

 @item{@scheme[expr:] specifies an expression to be used for arguments of this
   type, removing it from wrapper arguments.}

 @item{@scheme[bind:] specifies a name that is bound to the original
   argument if it is required later (e.g., @scheme[_box] converts its
   associated value to a C pointer, and later needs to refer back to
   the original box).}

 @item{@scheme[1st-arg:] specifies a name that can be used to refer to
   the first argument of the foreign call (good for common cases where
   the first argument has a special meaning, e.g., for method calls).}

 @item{@scheme[prev-arg:] similar to @scheme[1st-arg:], but refers to the
   previous argument.}

 @item{@scheme[pre:] a pre-foreign code chunk that is used to change the
   argument's value.}

 @item{@scheme[post:] a similar post-foreign code chunk.}

 @item{@scheme[keywords:] specifies keyword/value expressions that will
   be used with the surrounding @scheme[_fun] form.  (Note: the
   keyword/value sequence follows @scheme[keywords:], not parenthesized.)}
]

The @scheme[pre:] and @scheme[post:] bindings can be of the form
@scheme[(_id => _expr)] to use the existing value.  Note that if the
@scheme[pre:] expression is not @scheme[(_id => _expr)], then it means
that there is no input for this argument to the
@scheme[_fun]-generated procedure.  Also note that if a custom type is
used as an output type of a function, then only the @scheme[post:]
code is used.

Most custom types are meaningful only in a @scheme[_fun] context, and
will raise a syntax error if used elsewhere.  A few such types can be
used in non-@scheme[_fun] contexts: types which use only
@scheme[type:], @scheme[pre:], @scheme[post:], and no others.  Such
custom types can be used outside a @scheme[_fun] by expanding them
into a usage of @scheme[make-ctype], using other keywords makes this
impossible, because it means that the type has specific interaction
with a function call.


@defform[(define-fun-syntax id transformer-expr)]{

Binds @scheme[id] as a @tech{custom function type}. The type is
expanded by applying the procedure produced by
@scheme[transformer-expr] to a use of the @tech{custom function
type}.}


@defidform[_?]{

A @tech{custom function type} that is a marker for expressions that
should not be sent to the foreign function.  Use this to bind local
values in a computation that is part of an ffi wrapper interface, or
to specify wrapper arguments that are not sent to the foreign function
(e.g., an argument that is used for processing the foreign output).}


@defform/subs[#:literals (i o io)
              (_ptr mode type-expr)
              ([mode i o io])]{

Creates a C pointer type, where @scheme[mode] indicates input or
output pointers (or both).  The @scheme[mode] can be one of the
following:

@itemize[

 @item{@scheme[i] --- indicates an @italic{input} pointer argument:
  the wrapper arranges for the function call to receive a value that
  can be used with the @scheme[type] and to send a pointer to this
  value to the foreign function.  After the call, the value is
  discarded.}

 @item{@scheme[o] --- indicates an @italic{output} pointer argument:
  the foreign function expects a pointer to a place where it will save
  some value, and this value is accessible after the call, to be used
  by an extra return expression.  If @scheme[_ptr] is used in this
  mode, then the generated wrapper does not expect an argument since
  one will be freshly allocated before the call.}

 @item{@scheme[io] --- combines the above into an
  @italic{input/output} pointer argument: the wrapper gets the Racket
  value, allocates and set a pointer using this value, and then
  references the value after the call.  The ``@scheme[_ptr]'' name can
  be confusing here: it means that the foreign function expects a
  pointer, but the generated wrapper uses an actual value.  (Note that
  if this is used with structs, a struct is created when calling the
  function, and a copy of the return value is made too---which is
  inefficient, but ensures that structs are not modified by C code.)}

]

For example, the @scheme[_ptr] type can be used in output mode to create a
foreign function wrapper that returns more than a single argument.  The
following type:

@schemeblock[
(_fun (i : (_ptr o _int))
      -> (d : _double)
      -> (values d i))
]

creates a function that calls the foreign function with a fresh
integer pointer, and use the value that is placed there as a second
return value.}


@defidform[_box]{

A @tech{custom function type} similar to a @scheme[(_ptr io _type)]
argument, where the input is expected to be a box holding an
appropriate value, which is unboxed on entry and modified accordingly
on exit.}

@defform/subs[(_list mode type maybe-len)
              ([mode i o io]
               [maybe-len code:blank
                          len-expr])]{

A @tech{custom function type} that is similar to @scheme[_ptr], except
that it is used for converting lists to/from C vectors.  The optional
@scheme[maybe-len] argument is needed for output values where it is used in
the post code, and in the pre code of an output mode to allocate the
block.  In either case, it can refer to a previous binding for the
length of the list which the C function will most likely require.}

@defform[(_vector mode type maybe-len)]{

A @tech{custom function type} like @scheme[_list], except that it uses
Racket vectors instead of lists.}


@defform*[#:literals (o)
          [(_bytes o len-expr)
           _bytes]]{

A @tech{custom function type} that can be used by itself as a simple
type for a byte string as a C pointer.  Alternatively, the second form
is for a pointer return value, where the size should be explicitly
specified.

There is no need for other modes: input or input/output would be just
like @scheme[_bytes], since the string carries its size information
(there is no real need for the @scheme[o] part of the syntax, but it
is present for consistency with the above macros).}


@; ------------------------------------------------------------

@section{C Struct Types}

@defproc[(make-cstruct-type [types (listof ctype?)]) ctype?]{

The primitive type constructor for creating new C struct types.  These
types are actually new primitive types; they have no conversion
functions associated.  The corresponding Racket objects that are used
for structs are pointers, but when these types are used, the value
that the pointer @italic{refers to} is used, rather than the pointer
itself.  This value is basically made of a number of bytes that is
known according to the given list of @scheme[types] list.}


@defproc[(_list-struct [type ctype?] ...+) ctype?]{

A type constructor that builds a struct type using
@scheme[make-cstruct-type] function and wraps it in a type that
marshals a struct as a list of its components.  Note that space for
structs must to be allocated; the converter for a
@scheme[_list-struct] type immediately allocates and uses a list from
the allocated space, so it is inefficient. Use @scheme[define-cstruct]
below for a more efficient approach.}


@defform/subs[(define-cstruct id/sup ([field-id type-expr] ...))
              [(id/sup _id
                       (_id super-id))]]{

Defines a new C struct type, but unlike @scheme[_list-struct], the
resulting type deals with C structs in binary form, rather than
marshaling them to Racket values.  The syntax is similar to
@scheme[define-struct], providing accessor functions for raw struct
values (which are pointer objects).  The new type uses pointer tags to
guarantee that only proper struct objects are used.  The @scheme[_id]
must start with @litchar{_}.

The resulting bindings are as follows:

@itemize[

 @item{@scheme[_id] : the new C type for this struct.}

 @item{@scheme[_id]@schemeidfont{-pointer}: a pointer type that should
  be used when a pointer to values of this struct are used.}

 @item{@schemevarfont{id}@schemeidfont{?}: a predicate for the new type.}

 @item{@schemevarfont{id}@schemeidfont{-tag}: the tag string object that is
  used with instances.}

 @item{@schemeidfont{make-}@schemevarfont{id} : a constructor, which expects
  an argument for each type.}

 @item{@schemevarfont{id}@schemeidfont{-}@scheme[field-id] : an accessor
  function for each @scheme[field-id].}

 @item{@schemeidfont{set-}@schemevarfont{id}@schemeidfont{-}@scheme[field-id]@schemeidfont{!}
  : a mutator function for each @scheme[field-id].}

 @item{@schemevarfont{id}: structure-type information compatible with
  @scheme[struct-out] or @scheme[match] (but not @scheme[define-struct]);
  currently, this information is correct only when no @scheme[super-id]
  is specified.}

]

Objects of the new type are actually C pointers, with a type tag that
is a list that contains the string form of @schemevarfont{id}.  Since
structs are implemented as pointers, they can be used for a
@scheme[_pointer] input to a foreign function: their address will be
used.  To make this a little safer, the corresponding cpointer type is
defined as @scheme[_id]@schemeidfont{-pointer}.  The @scheme[_id] type
should not be used when a pointer is expected, since it will cause the
struct to be copied rather than use the pointer value, leading to
memory corruption.

If the first field is itself a cstruct type, its tag will be used in
addition to the new tag.  This feature supports common cases of object
inheritance, where a sub-struct is made by having a first field that
is its super-struct.  Instances of the sub-struct can be considered as
instances of the super-struct, since they share the same initial
layout.  Using the tag of an initial cstruct field means that the same
behavior is implemented in Racket; for example, accessors and mutators
of the super-cstruct can be used with the new sub-cstruct.  See the
example below.

Providing a @scheme[super-id] is shorthand for using an initial field
named @scheme[super-id] and using @schemeidfont{_}@scheme[super-id]
as its type.  Thus, the new struct will use
@schemeidfont{_}@scheme[super-id]'s tag in addition to its own tag,
meaning that instances of @scheme[_id] can be used as instances of
@schemeidfont{_}@scheme[super-id].  Aside from the syntactic sugar,
the constructor function is different when this syntax is used:
instead of expecting a first argument that is an instance of
@schemeidfont{_}@scheme[super-id], the constructor will expect
arguments for each of @schemeidfont{_}@scheme[super-id]'s fields, in
addition for the new fields.  This adjustment of the constructor is,
again, in analogy to using a supertype with @scheme[define-struct].

Note that structs are allocated as atomic blocks, which means that the
garbage collector ignores their content.  Currently, there is no safe
way to store pointers to GC-managed objects in structs (even if you
keep a reference to avoid collecting the referenced objects, a the 3m
variant's GC will invalidate the pointer's value).  Thus, only
non-pointer values and pointers to memory that is outside the GC's
control can be placed into struct fields.

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

Using the simple @scheme[_list-struct], you might expect this code to
work:

@schemeblock[
(define makeB
  (get-ffi-obj 'makeB "foo.so"
    (_fun -> (_list-struct (_list-struct _int _byte) _int))))
(makeB) (code:comment @#,t{should return @scheme['((1 2) 3)]})
]

The problem here is that @cpp{makeB} returns a pointer to the struct rather
than the struct itself.  The following works as expected:

@schemeblock[
(define makeB
  (get-ffi-obj 'makeB "foo.so" (_fun -> _pointer)))
(ptr-ref (makeB) (_list-struct (_list-struct _int _byte) _int))
]

As described above, @scheme[_list-struct]s should be used in cases where
efficiency is not an issue.  We continue using @scheme[define-cstruct], first
define a type for @cpp{A} which makes it possible to use @cpp{makeA}:

@schemeblock[
(define-cstruct #,(schemeidfont "_A") ([x _int] [y _byte]))
(define makeA
  (get-ffi-obj 'makeA "foo.so"
    (_fun -> #,(schemeidfont "_A-pointer")))) (code:comment @#,t{using @schemeidfont{_A} is a memory-corrupting bug!})
(define a (makeA))
(list a (A-x a) (A-y a))
(code:comment @#,t{produces an @scheme[A] containing @scheme[1] and @scheme[2]})
]

Using @cpp{gety} is also simple:

@schemeblock[
(define gety
  (get-ffi-obj 'gety "foo.so"
    (_fun #,(schemeidfont "_A-pointer") -> _byte)))
(gety a) (code:comment @#,t{produces @scheme[2]})
]

We now define another C struct for @cpp{B}, and expose @cpp{makeB}
using it:

@schemeblock[
(define-cstruct #,(schemeidfont "_B") ([a #,(schemeidfont "_A")] [z _int]))
(define makeB
  (get-ffi-obj 'makeB "foo.so"
    (_fun -> #,(schemeidfont "_B-pointer"))))
(define b (makeB))
]

We can access all values of @scheme[b] using a naive approach:

@schemeblock[
(list (A-x (B-a b)) (A-y (B-a b)) (B-z b))
]

but this is inefficient as it allocates and copies an instance of
@cpp{A} on every access.  Inspecting the tags @scheme[(cpointer-tag
b)] we can see that @cpp{A}'s tag is included, so we can simply use
its accessors and mutators, as well as any function that is defined to
take an @cpp{A} pointer:

@schemeblock[
(list (A-x b) (A-y b) (B-z b))
(gety b)
]

Constructing a @cpp{B} instance in Racket requires allocating a
 temporary @cpp{A} struct:

@schemeblock[
(define b (make-B (make-A 1 2) 3))
]

To make this more efficient, we switch to the alternative
@scheme[define-cstruct] syntax, which creates a constructor that
expects arguments for both the super fields ands the new ones:

@schemeblock[
 (define-cstruct (#,(schemeidfont "_B") #,(schemeidfont "_A")) ([z _int]))
 (define b (make-B 1 2 3))
]}


@; ------------------------------------------------------------

@section{Enumerations and Masks}

Although the constructors below are describes as procedures, they are
implemented as syntax, so that error messages can report a type name
where the syntactic context implies one.

@defproc[(_enum [symbols list?] [basetype ctype? _ufixint])
         ctype?]{

Takes a list of symbols and generates an enumeration type.  The
enumeration maps between a symbol in the given @scheme[symbols] list and
corresponding integers, counting from @scheme[0].

The list @scheme[symbols] can also set the values of symbols by
putting @scheme['=] and an exact integer after the symbol.  For
example, the list @scheme['(x y = 10 z)] maps @scheme['x] to
@scheme[0], @scheme['y] to @scheme[10], and @scheme['z] to
@scheme[11].

The @scheme[basetype] argument specifies the base type to use.}

@defproc[(_bitmask [symbols (or symbol? list?)] [basetype ctype? _uint])
         ctype?]{

Similar to @scheme[_enum], but the resulting mapping translates a list
of symbols to a number and back, using @scheme[bitwise-ior].  A single
symbol is equivalent to a list containing just the symbol.  The
default @scheme[basetype] is @scheme[_uint], since high bits are often
used for flags.}
