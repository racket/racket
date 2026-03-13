#lang scribble/manual
@(require "common.rkt")

@title[#:tag "procedure"]{Foreign Procedures}

@defform[#:kind "ffi2 type"
         (-> arg-type ...
             maybe-vararg-types
             result-type
             maybe-abi)
         #:grammar ([maybe-vararg-types (code:line #:varargs arg-type ...)
                                        ϵ]
                    [maybe-abi (code:line #:abi abi)
                               ϵ])]{

Describes the type of a procedure. The C representation of a procedure
is an address, while the Racket representation is a Racket procedure.

A @racket[->] type is normally used with @racket[ffi2-procedure] or
@racket[define-ffi2-procedure] to obtain a Racket produce that calls a
C function. That use of @racket[->] creates a @deftech{foreign callout}.

A @racket[->] type can also be used with @racket[ffi2-callback] to turn
a Racket procedure into a function callable by C as represented by a
@tech{pointer} object. That of use of @racket[->] creates a
@deftech{foreign callback}.

}

@defform[(ffi2-procedure ptr-expr arrow-type)]{

Creates a @tech{foreign callout}. The @racket[arrow-type] must be
either an immediate @racket[->] type or a type name that is defined as
an @racket[->] type.

Using @racket[ffi-procedure] is equivalent to @racket[(ffi2-cast
ptr-expr #:from ptr_t #:to arrow-type)] to convert a foreign
function pointer to a procedure that can be called from Racket.

}

@defform[(ffi2-callback proc-expr arrow-type)]{

Creates a @tech{foreign callback}. The @racket[arrow-type] must be
either an immediate @racket[->] type or a type name that is defined as
an @racket[->] type.

Using @racket[ffi2-callback] is equivalent to @racket[(ffi2-cast
proc-expr #:from arrow-type #:to ptr_t)] to convert a Racket
procedure to a function pointer to be called in a foreign context.

A foreign callback is managed by the Racket garbage collector. A
callback is always immobile in the sense of using @racket[ffi-malloc]
in @racket[#:gcable-immobile] mode, but the callback must be retained
somehow as long as it is relevant for calls from a foreign context.

}

@defform[(define-ffi2-procedure id arrow-type
           option ...)
         #:grammar ([option (code:line #:lib lib-expr)
                            (code:line #:c-id c-id)
                            (code:line #:fail fail-expr)
                            (code:line #:wrap wrap-expr)])]{

A convenience form for binding @racket[id] as a @tech{foreign
callout}. The @racket[#:lib] option must be provided.

}

@defform[(define-ffi2-definer id
           option ...)
         #:grammar ([option (code:line #:lib lib-expr)
                            (code:line #:default-fail fail-expr)
                            (code:line #:default-wrap wrap-expr)
                            (code:line #:provide)])]{

Defines @racket[id] as a definition form with the same syntax as
@racket[define-ffi2-procedure], except that a definition using
@racket[id] cannot have a @racket[#:lib] clause.

}
