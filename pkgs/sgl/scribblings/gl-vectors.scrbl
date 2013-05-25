#lang scribble/doc
@(require "common.rkt" scribble/bnf (for-syntax racket/base))

@title[#:tag "gl-vectors"]{OpenGL Vectors}

@defmodule[sgl/gl-vectors]

The @racketmodname[sgl/gl-vectors] module supports OpenGL programming
with @racket[cvector]s.  In this document and in the error messages, a
``gl-vector'' is just a @racket[cvector], while a
``gl-@nonterm{type}-vector'' is a @racket[cvector] with an appropriate
type. Use the @racketmodname[sgl/gl-vectors] module vectors instead of
a Racket @racket[cvector] directly, because they are specialized to
handle the OpenGL types correctly.

@deftogether[(
@defproc[(gl-vector? [v any/c]) boolean?]
@defproc[(gl-vector->vector [vec cvector?]) vector?]
@defproc[(gl-vector->list [vec cvector?]) list?]
@defproc[(gl-vector-length [vec cvector?]) exact-nonnegative-integer?]
@defproc[(gl-vector-ref [vec cvector?][pos exact-nonnegative-integer?]) any/v]
@defproc[(gl-vector-set! [vec cvector?][pos exact-nonnegative-integer?][v any/v]) void?]
)]{

Synonyms for @racket[cvector?], @racket[cvector->vector], @racket[cvector-length], etc.}

@(define-syntax (define-gl-vector stx)
  (syntax-case stx ()
    [(_ type)
     (let ([mk (lambda s
                 (string->symbol (apply string-append s)))]
           [type (symbol->string (syntax-e #'type))]
           [locs (lambda (l)
                   (datum->syntax #'here
                                  (list
                                   (datum->syntax #'here
                                                  (car l)
                                                  #("?" 1 1 2 1))
                                   (datum->syntax #'here
                                                  (cadr l)
                                                  #("?" 1 3 4 1)))
                                  #("?" 1 0 1 3)))])
       (with-syntax ([<type> (mk type)]
                     [<type>? (mk type "?")]
                     [gl-<type>-vector? (mk "gl-" type "-vector?")]
                     [gl-<type>-vector (mk "gl-" type "-vector")]
                     [make-gl-<type>-vector (mk "make-gl-" type "-vector")]
                     [vector->gl-<type>-vector (mk "vector->gl-" type "-vector")]
                     [list->gl-<type>-vector (mk "list->gl-" type "-vector")]
                     [gl-<type>-vector+ (mk "gl-" type "-vector+")]
                     [gl-<type>-vector- (mk "gl-" type "-vector-")]
                     [gl-<type>-vector* (mk "gl-" type "-vector*")]
                     [vectorof-<type>? (locs `(vectorof ,(mk type "?")))]
                     [listof-<type>? (locs `(listof ,(mk type "?")))])
         #'(...
            @deftogether[(
             @defproc[(gl-<type>-vector? [v any/c]) boolean?]
             @defproc[(make-gl-<type>-vector [pos exact-nonnegative-integer?]) gl-<type>-vector?]
             @defproc[(gl-<type>-vector [v <type>?] ...) gl-<type>-vector?]
             @defproc[(vector->gl-<type>-vector [v vectorof-<type>?] ...) gl-<type>-vector?]
             @defproc[(list->gl-<type>-vector [v listof-<type>?] ...) gl-<type>-vector?]
             @defproc[(gl-<type>-vector+ [vec  gl-<type>-vector?] ...+) gl-<type>-vector?]
             @defproc[(gl-<type>-vector- [vec  gl-<type>-vector?] ...+) gl-<type>-vector?]
             @defproc[(gl-<type>-vector* [x real?][vec gl-<type>-vector?]) gl-<type>-vector?]
             )]{

             Operations on vectors of @racket[<type>] elements. The @racket[gl-<type>-vector+]
             and @racket[gl-<type>-vector-] functions compute the element-by-element sum and
             difference of the given vectors, respectively. The @racket[gl-<type>-vector*] function
             multiplies each element of @racket[vec] by @racket[x].})))]))

@(define-gl-vector byte)
@(define-gl-vector ubyte)
@(define-gl-vector short)
@(define-gl-vector ushort)
@(define-gl-vector int)
@(define-gl-vector uint)
@(define-gl-vector float)
@(define-gl-vector double)
@(define-gl-vector boolean)

@defproc[(gl-vector-norm [vec gl-vector?]) real?]{

Returns the square root of the sum of the squares of the elements
of @racket[vec].}

