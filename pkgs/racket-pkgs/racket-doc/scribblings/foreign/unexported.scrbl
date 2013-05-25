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

@defproc[(ffi-obj [objname (or/c string? bytes? symbol?)]
                  [lib (or/c ffi-lib? path-string? #f)])
         ffi-obj?]{

Pulls out a foreign object from a library, returning a value
that can be used as a C pointer.  If @racket[lib] is a path or string,
then @racket[ffi-lib] is used to create a library object.}


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


@defproc[(ffi-call [ptr cpointer?] [in-types (listof ctype?)] [out-type ctype?]
                   [abi (or/c #f 'default 'stdcall 'sysv) #f]
                   [save-errno? any/c]
                   [orig-place? any/c])
         procedure?]{

The primitive mechanism that creates Racket ``callout'' values for
@racket[_cprocedure].  The given @racket[ptr] is wrapped in a
Racket-callable primitive function that uses the types to specify how
values are marshaled.}


@defproc[(ffi-callback [proc any/c] [in-types any/c] [out-type any/c]
                       [abi (or/c #f 'default 'stdcall 'sysv) #f]
                       [atomic? any/c #f]
                       [async-apply (or/c #f ((-> any) . -> . any)) #f])
         ffi-callback?]{

The symmetric counterpart of @racket[ffi-call].  It receives a Racket
procedure and creates a callback object, which can also be used as a
C pointer.}


@defproc[(ffi-callback? [x any/c]) boolean?]{

A predicate for callback values that are created by @racket[ffi-callback].
}
