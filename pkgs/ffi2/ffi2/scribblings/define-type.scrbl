#lang scribble/manual
@(require scribble/example
          "common.rkt")

@(define ffi2-eval (make-base-eval))
@examples[#:eval ffi2-eval #:hidden (require ffi2)]


@title[#:tag "define-ffi2-type"]{Defining Foreign Types}

@defform[(define-ffi2-type name parent-type
           option
           ...)
         #:grammar ([option (code:line #:tag tag)
                            (code:line #:predicate predicate-expr)
                            (code:line #:racket->c racket->c-expr)
                            (code:line #:release release-expr)
                            (code:line #:c->racket c->racket-expr)]
                    [tag identifier
                         #f])]{

Defines a type @racket[name] that is an alias or extension of
@racket[parent-type].

When @racket[parent-type] is a @racket[struct] or @racket[union] form
and no @racket[option]s are provided, then the @racket[parent-type]
affects the definitions created by @racket[define-ffi2-type]. See
@racket[struct] and @racket[union] for more information.

When @racket[parent-type] is an @racket[array] form and no
@racket[option]s are provided other than @racket[#:tag], then the
@racket[array] also affects the definitions created by
@racket[define-ffi2-type]. See @racket[array] for more information.

When @racket[parent-type] is an @deftech{immediate pointer} type and
no @racket[option]s are provided other than @racket[#:tag] and
@racket[#:predicate], then @racket[define-ffi2-type] creates a new
immediate pointer type that is a subtype of @racket[parent-type]. The
only predefined immediate pointer types are @racket[ptr_t] and
@racket[ptr_t/gcable] (and they are treated the same as a parent
type). The Racket representation of the new type is a @tech{pointer}
object. Unless @racket[tag] is supplied as @racket[#f], each pointer's
tags add @racket[tag] (if present) or @racket[name] to the end of the
tags for @racket[parent-type], which creates a @tech{pointer subtype}.
If @racket[tag] is provided as @racket[#f], Racket representation of
the new type is a generic pointer with no tags.

When @racket[parent-type] is an @tech{scalar} type and no
@racket[option]s are provided, the defined @racket[name] is a scalar
type.

The @racket[#:tag] option can be used only when @racket[parent-type]
is an immediate pointer type or @racket[array] form. In the case of an
@racket[array] form with a constant size, @racket[#:tag] cannot be
mixed with any other @racket[option] form.

In all cases, the new type @racket[name] has the same C representation
as @racket[parent-type]. The Racket representation can be adjusted via
@racket[#:racket->c] and @racket[#:c->racket] options, which often
need accompanying @racket[#:predicate] and @racket[#:release]
functions:

@itemlist[

 @item{@racket[#:predicate] provides a predicate function as the
 result of @racket[predicate-expr]. This predicate is used when checks
 are enabled for Racket values to be converted to C for the type
 @racket[name], but the predicate can be skipped on request. The
 predicate determines whether a value is suitable as an argument to a
 function provided by @racket[#:racket->c]. If @racket[#:predicate] is
 not provided, then the predicate associated with @racket[parent-type]
 is used.}

 @item{@racket[#:c->racket] provides a conversion function toward
 Racket as the result of @racket[c->racket-expr]. This converter is
 applied to a Racket representation of @racket[parent-type] as
 extracted from a C representation. The result of conversion should be
 a Racket representation for @racket[name]. If @racket[#:c->racket] is
 not provided, conversion is the identity function.}

 @item{@racket[#:release] provides a function that finalizes
 conversion from Racket to C. The function is applied to the result of
 @racket[parent-type]'s release function after the C value is
 delivered (e.g., passed in a foreign procedure call that has
 returned). For base pointer types, the release function is
 @racket[black-box], which is useful because it keeps a pointer live
 if it is subject to garbage collection. A release procedure could
 explicitly deallocate a pointer that was allocated by the
 @racket[#:racket->c] procedure, but a release function is not called
 if control somehow escapes or the current thread is forcibly
 terminated.}

 @item{@racket[#:racket->c] provides a conversion function toward C as
 the result of @racket[racket->c-expr]. This converter is applied to a
 Racket value that is supplied for a @racket[name] type. The result of
 conversion should be a Racket representation for
 @racket[parent-type]. If @racket[#:racket->c] is not provided,
 conversion is the identity function.}

]

@examples[
#:eval ffi2-eval
(define-ffi2-type percentage_t double_t
  #:predicate (lambda (v) (and (real? v) (<= 0.0 v 100.0)))
  #:racket->c (lambda (v) (/ v 100.0))
  #:c->racket (lambda (v) (* v 100.0)))
(define p (ffi2-malloc double_t))
(ffi2-set! p double_t 0.5)
(ffi2-ref p percentage_t)
(ffi2-set! p percentage_t 25.5)
(ffi2-ref p double_t)
]

@examples[
#:eval ffi2-eval
#:label #f
(define-ffi2-type percentage_box_t ptr_t
  #:predicate (lambda (bx) (percentage_t? (unbox bx)))
  #:racket->c (lambda (bx)
                (define ptr (ffi2-malloc #:gcable-immobile percentage_t))
                (ffi2-set! ptr percentage_t (unbox bx))
                ptr)
  #:c->racket (lambda (ptr)
                (box (ffi2-ref ptr percentage_t))))
(define p (ffi2-malloc #:gcable-traced ptr_t))
(ffi2-set! p percentage_box_t (box 50.5))
(ffi2-ref p percentage_box_t)
(ffi2-ref (ffi2-ref p ptr_t) double_t)
]

}

@defform[(ffi2-sizeof type)]{

Returns the number of bytes used for the C representation of
@racket[type].

}

@defform[(ffi2-offsetof type field-id)]{

Returns the number of bytes in the C representation of @racket[type]
that precede the field named @racket[field-id]. The @racket[type] must
be have the C representation of a @racket[struct] or @racket[union]
type; the result is always @racket[0] in the case of a @racket[union]
type.

}

@defform[#:kind "ffi2 type/abi"
         #:literals (else)
         (system-type-case key
           [(val ...) type/abi]
           [else type/abi])
         #:grammar ([key os
                         os*                         
                         arch
                         word])]{

Describes a type with a platform-specific representation or a
platform-specific choice of procedure ABI, enabling a compile-time
(later than expand-time) choice. The symbol form of @racket[key]
corresponds to a symbol argument to @racket[system-type], and each
@racket[val] just be a potential result: an identifier for
@racket[key]s other than @racket[word], or either @racket[32] or
@racket[64] in the case of @racket[word].

In the case of types, each right-hand side @racket[type/abi] must be a
@deftech{scalar} type, such as @racket[int_t] or @racket[float_t]. A
@racket[system-type-case] type is also scalar, since it selects among
scalar types.

}

@close-eval[ffi2-eval]
