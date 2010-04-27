#lang scribble/doc
@(require "utils.ss")

@title[#:tag "homogeneous-vectors"]{Safe Homogenous Vectors}

@defmodule[ffi/vector]

Homogenous vectors are similar to C vectors (see
@secref["foreign:cvector"]), except that they define different types
of vectors, each with a hard-wired type.

An exception is the @schemeidfont{u8} family of bindings, which are
just aliases for byte-string bindings: @scheme[make-u8vector],
@scheme[u8vector]. @scheme[u8vector?], @scheme[u8vector-length],
@scheme[u8vector-ref], @scheme[u8vector-set!],
@scheme[list->u8vector], @scheme[u8vector->list].

@(begin
   (require (for-syntax scheme/base))
   (define-syntax (srfi-4-vector stx)
     (syntax-case stx ()
       [(_ id elem)
        #'(srfi-4-vector/desc id elem 
                              "Like " (scheme make-vector) ", etc., but for " (scheme elem) " elements.")]))
   (define-syntax (srfi-4-vector/desc stx)
     (syntax-case stx ()
       [(_ id elem . desc)
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
            #`(begin
               (defproc* ([(make [len exact-nonnegative-integer?]) ?]
                          [(vecr [val number?] (... ...)) ?]
                          [(? [v any/c]) boolean?]
                          [(length [vec ?]) exact-nonnegative-integer?]
                          [(ref [vec ?][k exact-nonnegative-integer?]) number?]
                          [(! [vec ?][k exact-nonnegative-integer?][val number?]) void?]
                          [(list-> [lst (listof number?)]) ?]
                          [(->list [vec ?]) (listof number?)]
                          [(->cpointer [vec ?]) cpointer?])
                 . desc)
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
                 "Like " (scheme _cvector) ", but for vectors of " (scheme elem) " elements."))))])))


@srfi-4-vector/desc[u8 _uint8]{

Like @scheme[_cvector], but for vectors of @scheme[_byte] elements. These are
aliases for @schemeidfont{byte} operations.}

@srfi-4-vector[s8 _int8]
@srfi-4-vector[s16 _int16]
@srfi-4-vector[u16 _uint16]
@srfi-4-vector[s32 _int32]
@srfi-4-vector[u32 _uint32]
@srfi-4-vector[s64 _int64]
@srfi-4-vector[u64 _uint64]
@srfi-4-vector[f32 _float]
@srfi-4-vector[f64 _double*]

