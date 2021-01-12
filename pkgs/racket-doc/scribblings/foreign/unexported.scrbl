#lang scribble/doc
@(require "utils.rkt"
          (for-label (only-in '#%foreign
                              ffi-obj ffi-obj? ffi-obj-lib ffi-obj-name
                              ctype-basetype ctype-scheme->c ctype-c->scheme
                              ffi-call ffi-callback ffi-callback?)))

@title[#:tag "foreign:c-only"]{Unexported Primitive Functions}

@declare-exporting['#%foreign]

Parts of the @racketmodname[ffi/unsafe] library are implemented by
the Racket built-in @racketmodname['#%foreign] module.  The
@racketmodname['#%foreign] module is not intended for direct use, but
it exports the following procedures (among others).

@defproc[(ffi-obj [objname bytes?]
                  [lib (or/c ffi-lib? path-string? #f)])
         ffi-obj?]{

Pulls out a foreign object from a library, returning a value
that can be used as a C pointer.  If @racket[lib] is a path or string,
then @racket[ffi-lib] is used to create a library object.}


@defproc*[([(ffi-obj? [x any/c]) boolean?]
           [(ffi-obj-lib [obj ffi-obj?]) ffi-lib?]
           [(ffi-obj-name [obj ffi-obj?]) bytes?])]{

A predicate for objects returned by @racket[ffi-obj], and accessor
functions that return its corresponding library object and name.
These values can also be used as C pointer objects.}


@defproc*[([(ctype-basetype [type ctype?]) (or/c ctype? #f)]
           [(ctype-scheme->c [type ctype?]) procedure?]
           [(ctype-c->scheme [type ctype?]) procedure?])]{

Accessors for the components of a C type object, made by
@racket[make-ctype].  The @racket[ctype-basetype] selector returns a
symbol for primitive types that names the type, a list of ctypes for
cstructs, and another ctype for user-defined ctypes.}


@defproc[(ffi-call [ptr cpointer?] [in-types (listof ctype?)] [out-type ctype?]
                   [abi (or/c #f 'default 'stdcall 'sysv) #f]
                   [save-errno? any/c #f]
                   [orig-place? any/c #f]
                   [lock-name (or/c #f string?) #f]
                   [blocking? any/c #f]
                   [varargs-after (or/c #f positive-exact-integer?) #f])
         procedure?]{

The primitive mechanism that creates Racket @tech{callout} values for
@racket[_cprocedure].  The given @racket[ptr] is wrapped in a
Racket-callable primitive function that uses the types to specify how
values are marshaled.}

@defproc[(ffi-call-maker [in-types (listof ctype?)] [out-type ctype?]
                   [abi (or/c #f 'default 'stdcall 'sysv) #f]
                   [save-errno? any/c #f]
                   [orig-place? any/c #f]
                   [lock-name (or/c #f string?) #f]
                   [blocking? any/c #f]
                   [varargs-after (or/c #f positive-exact-integer?) #f])
         (cpointer . -> . procedure?)]{

A curried variant of @racket[ffi-call] that takes the foreign-procedure pointer
separately.}


@defproc[(ffi-callback [proc procedure?] [in-types any/c] [out-type any/c]
                       [abi (or/c #f 'default 'stdcall 'sysv) #f]
                       [atomic? any/c #f]
                       [async-apply (or/c #f ((-> any) . -> . any) box?) #f]
                       [varargs-after (or/c #f positive-exact-integer?) #f])
         ffi-callback?]{

The symmetric counterpart of @racket[ffi-call].  It receives a Racket
procedure and creates a @tech{callback} object, which can also be used as a
C pointer.}

@defproc[(ffi-callback-maker [in-types any/c] [out-type any/c]
                       [abi (or/c #f 'default 'stdcall 'sysv) #f]
                       [atomic? any/c #f]
                       [async-apply (or/c #f ((-> any) . -> . any) box?) #f]
                       [varargs-after (or/c #f positive-exact-integer?) #f])
         (procedure? . -> . ffi-callback?)]{

A curried variant of @racket[ffi-callback] that takes the callback procedure
separately.}


@defproc[(ffi-callback? [v any/c]) boolean?]{

A predicate for callback values that are created by @racket[ffi-callback].
}


@defproc[(make-late-will-executor) will-executor?]{

Creates a ``late'' will executor that readies a will for a value
@scheme[_v] only if no normal will executor has a will registered for
@scheme[_v]. In addition, for the @3m[] and @CGC[] variants of Racket,
normal weak references to @scheme[_v] are cleared before a will for
@racket[_v] is readied by the late will executor, but late weak
references created by @racket[make-late-weak-box] and
@racket[make-late-weak-hasheq] are not. For the @CS[] variant of
Racket, a will is readied for @racket[_v] only when it is not reachable
from any value that has a late will; if a value @racket[_v] is
reachable from itself (i.e., through any field of @racket[_v], as
opposed to the immediate value itself), a ``late'' will for
@racket[_v] never becomes ready.

Unlike a normal will executor, if a late will executor becomes
inaccessible, the values for which it has pending wills are retained
within the late will executor's place.

A late will executor is intended for use in the implementation of
@racket[register-finalizer].}
