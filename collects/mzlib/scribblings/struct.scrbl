#lang scribble/doc
@(require "common.rkt"
          scribble/eval
          (for-label mzlib/struct
                     scheme/contract
                     (only-in scheme/base
                              regexp-try-match)))

@(define struct-eval (make-base-eval))
@interaction-eval[#:eval struct-eval (require mzscheme)]
@interaction-eval[#:eval struct-eval (require mzlib/struct)]

@mzlib[#:mode title struct]

@defform[(copy-struct struct-id struct-expr 
                      (accessor-id field-expr) ...)]{

``Functional update'' for structure instances. The result of
evaluating @racket[struct-expr] must be an instance of the structure
type named by @racket[struct-id]. The result of the
@racket[copy-struct] expression is a fresh instance of
@racket[struct-id] with the same field values as the result of
@racket[struct-expr], except that the value for the field accessed by
each @racket[accessor-id] is replaced by the result of
@racket[field-expr].

The result of @racket[struct-expr] might be an instance of a sub-type
of @racket[struct-id], but the result of the @racket[copy-struct]
expression is an immediate instance of @racket[struct-id]. If
@racket[struct-expr] does not produce an instance of
@racket[struct-id], the @racket[exn:fail:contract] exception is
raised.

If any @racket[accessor-id] is not bound to an accessor of
@racket[struct-id] (according to the expansion-time information
associated with @racket[struct-id]), or if the same
@racket[accessor-id] is used twice, then a syntax error is raised.}


@defform/subs[(define-struct/properties id (field-id ...) 
                                        ((prop-expr val-expr) ...) 
                                        maybe-inspector-expr)
              ([maybe-inspector-expr code:blank
                                     expr])]{

Like @racket[define-struct] from @racketmodname[mzscheme], but
properties can be attached to the structure type. Each
@racket[prop-expr] should produce a structure-type property value, and
each @racket[val-expr] produces the corresponding value for the
property.

@examples[
#:eval struct-eval
(define-struct/properties point (x y)
  ([prop:custom-write (lambda (p port write?)
                       (fprintf port "(~a, ~a)"
                                     (point-x p)
                                     (point-y p)))]))
(display (make-point 1 2))
]}


@defform[(make-->vector struct-id)]{

Builds a function that accepts a structure type instance (matching
@racket[struct-id]) and provides a vector of the fields of the
structure type instance.}


@close-eval[struct-eval]
