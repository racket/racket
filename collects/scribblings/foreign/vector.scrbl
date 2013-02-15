#lang scribble/doc
@(require "utils.rkt" 
          (only-in scribble/decode make-splice)
          scribble/racket
          (for-label racket/extflonum))

@title[#:tag "homogeneous-vectors"]{Safe Homogenous Vectors}

@defmodule[ffi/vector]

Homogenous vectors are similar to C vectors (see
@secref["foreign:cvector"]), except that they define different types
of vectors, each with a fixed element type. An exception is the
@racketidfont{u8} family of bindings, which are just aliases for
byte-string bindings; for example, @racket[make-u8vector] is an alias
for @racket[make-bytes].

@(begin
   (require (for-syntax scheme/base))
   (define-syntax (srfi-4-vector stx)
     (syntax-case stx ()
       [(_ id elem number?)
        #'(srfi-4-vector/desc id elem number? make-splice
            "Like " (racket make-vector) ", etc., but for " (racket elem) " elements.")]))
   (define-syntax (srfi-4-vector/desc stx)
     (syntax-case stx ()
       [(_ id elem as-number? extra desc ...)
        (let ([mk
               (lambda l
                 (datum->syntax
                  #'id
                  (string->symbol
                   (apply string-append
                          (map (lambda (i)
                                 (if (identifier? i)
                                     (symbol->string (syntax-e i))
                                     i))
                               l)))
                  #'id))])
          (with-syntax ([make (mk "make-" #'id "vector")]
                        [vecr (mk #'id "vector")]
                        [? (mk #'id "vector?")]
                        [length (mk #'id "vector-length")]
                        [ref (mk #'id "vector-ref")]
                        [! (mk #'id "vector-set!")]
                        [list-> (mk "list->" #'id "vector")]
                        [->list (mk #'id "vector->list")]
                        [->cpointer (mk #'id "vector->cpointer")]
                        [_vec (mk "_" #'id "vector")])
            #`(let-syntax ([number? (make-element-id-transformer
                                     (lambda (stx)
                                       #'(racket as-number?)))])
              (list
               (defproc* ([(make [len exact-nonnegative-integer?]) ?]
                          [(vecr [val number?] (... ...)) ?]
                          [(? [v any/c]) boolean?]
                          [(length [vec ?]) exact-nonnegative-integer?]
                          [(ref [vec ?] [k exact-nonnegative-integer?]) number?]
                          [(! [vec ?] [k exact-nonnegative-integer?] [val number?]) void?]
                          [(list-> [lst (listof number?)]) ?]
                          [(->list [vec ?]) (listof number?)]
                          [(->cpointer [vec ?]) cpointer?])
                 desc ...
                 (extra
                  (list
                   " The " (racket ->cpointer)
                   " function extracts a plain pointer to the underlying array.")))
               ;; Big pain: make up relatively-correct source locations
               ;; for pieces in the _vec definition:
               (defform* [#,(datum->syntax
                             #'_vec
                             (cons #'_vec
                                   (let loop ([l '(mode maybe-len)]
                                              [col (+ (syntax-column #'_vec)
                                                      (syntax-span #'_vec)
                                                      1)]
                                              [pos (+ (syntax-position #'_vec)
                                                      (syntax-span #'_vec)
                                                      1)])
                                     (if (null? l)
                                         null
                                         (let ([span (string-length (symbol->string (car l)))])
                                           (cons (datum->syntax
                                                  #'_vec
                                                  (car l)
                                                  (list (syntax-source #'_vec)
                                                        (syntax-line #'_vec)
                                                        col
                                                        pos
                                                        span))
                                                 (loop (cdr l)
                                                       (+ col 1 span)
                                                       (+ pos 1 span)))))))
                             (list (syntax-source #'_vec)
                                   (syntax-line #'_vec)
                                   (sub1 (syntax-column #'vec))
                                   (sub1 (syntax-position #'vec))
                                   10))
                           _vec]
                 "Like " (racket _cvector) ", but for vectors of "
                 (racket elem) " elements.")))))])))


@srfi-4-vector/desc[u8 _uint8 byte? (lambda (x) (make-splice null))]{

Like @racket[_cvector], but for vectors of @racket[_uint8] elements. These are
aliases for @racketidfont{byte} operations, where @racket[u8vector->cpointer]
is the identity function.}

@srfi-4-vector[s8 _int8 (integer-in -128 127)]
@srfi-4-vector[s16 _int16 (integer-in -32768 32767)]
@srfi-4-vector[u16 _uint16 (integer-in 0 65535)]
@srfi-4-vector[s32 _int32 (integer-in -2147483648 2147483647)]
@srfi-4-vector[u32 _uint32 (integer-in 0 4294967295)]
@srfi-4-vector[s64 _int64 (integer-in -9223372036854775808 9223372036854775807)]
@srfi-4-vector[u64 _uint64 (integer-in 0 18446744073709551615)]
@srfi-4-vector[f32 _float real?]
@srfi-4-vector[f64 _double* real?]
@srfi-4-vector[f80 _longdouble extflonum?]
