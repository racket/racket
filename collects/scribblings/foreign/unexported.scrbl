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
it exports the following procedures.  If you find any of these useful,
please let us know.

@defproc[(ffi-obj [objname (or/c string? bytes? symbol?)]
                  [lib (or/c ffi-lib? path-string? #f)])
         any]{

Pulls out a foreign object from a library, returning a Racket value
that can be used as a pointer.  If a name is provided instead of a
foreign-library value, @racket[ffi-lib] is used to create a library
object.}


@defproc*[([(ffi-obj? [x any/c]) boolean?]
           [(ffi-obj-lib [obj ffi-obj?]) ffi-lib?]
           [(ffi-obj-name [obj ffi-obj?]) string?])]{

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


@defproc[(ffi-call [ptr any/c] [in-types (listof ctype?)] [out-type ctype?]
                   [abi (or/c symbol/c #f) #f])
         any]{

The primitive mechanism that creates Racket ``callout'' values.  The
given @racket[ptr] (any pointer value, including @racket[ffi-obj]
values) is wrapped in a Racket-callable primitive function that uses
the types to specify how values are marshaled.

The optional @racket[abi] argument determines the foreign ABI that is
used.  @racket[#f] or @racket['default] will use a platform-dependent
default; other possible values are @racket['stdcall] and
@racket['sysv] (the latter corresponds to ``cdecl'').  This is
especially important on Windows, where most system functions are
@racket['stdcall], which is not the default.}


@defproc[(ffi-callback [proc any/c] [in-types any/c] [out-type any/c]
                       [abi (or/c symbol/c #f) #f]
                       [atomic? any/c #f])
         ffi-callback?]{

The symmetric counterpart of @racket[ffi-call].  It receives a Racket
procedure and creates a callback object, which can also be used as a
pointer.  This object can be used as a C-callable function, which
invokes @racket[proc] using the types to specify how values are
marshaled.}


@defproc[(ffi-callback? [x any/c]) boolean?]{

A predicate for callback values that are created by @racket[ffi-callback].
}
