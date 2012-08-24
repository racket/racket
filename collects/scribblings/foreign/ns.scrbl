#lang scribble/doc
@(require scribble/manual
          (for-label racket/base
                     ffi/unsafe/nsalloc
                     ffi/unsafe/nsstring))

@title[#:tag "ns"]{Cocoa Foundation}

The @racketmodname[ffi/unsafe/nsalloc] and
@racketmodname[ffi/unsafe/nsstring] libraries provide basic
facilities for working with Cocoa and/or Mac OS X Foundation
libraries (usually along with @racket[ffi/objc]).

@; ----------------------------------------

@section{Strings}

@defmodule[ffi/unsafe/nsstring]

@defthing[_NSString ctype?]{

A type that converts between Racket strings and
@as-index{@tt{NSString}} (a.k.a. @as-index{@tt{CFString}})
values. That is, use @tt{_NSString} as a type for a foreign-function
@tt{NSString} argument or result.

The @racket[_NSString] conversion keeps a weak mapping from Racket
strings to converted strings, so that converting the same string (in
the @racket[equal?] sense) multiple times may avoid allocating
multiple @tt{NSString} objects.}


@; ----------------------------------------

@section{Allocation Pools}

@defmodule[ffi/unsafe/nsalloc]{Calling any Foundation API that
allocates requires an @tt{NSAutoreleasePool} installed. The
@racketmodname[ffi/unsafe/nsalloc] library provides a function and
shorthand syntactic form for setting up such a context. (The
@racket[_NSString] type creates an autorelease pool implicitly while
converting from/to a Racket string, however.)}

@defproc[(call-with-autorelease [thunk (-> any)]) any]{

Calls @racket[thunk] in atomic mode and with a fresh
@tt{NSAutoreleasePool} that is @tt{releas}ed after @racket[thunk]
returns.}


@defform[(with-autorelease expr)]{

A shorthand for @racket[(call-with-autorelease (lambda () expr))].}
