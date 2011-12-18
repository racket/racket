#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label scheme unstable/hash))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/hash))

@title{Hash Tables}
@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defmodule[unstable/hash]

This module provides tools for manipulating hash tables.

@defproc[(hash-union [h0 (and/c hash? hash-can-functional-set?)]
                     [h hash?] ...
                     [#:combine combine
                                (-> any/c any/c any/c)
                                (lambda _ (error 'hash-union ....))]
                     [#:combine/key combine/key
                                    (-> any/c any/c any/c any/c)
                                    (lambda (k a b) (combine a b))])
         (and/c hash? hash-can-functional-set?)]{

Computes the union of @racket[h0] with each hash table @racket[h] by functional
update, adding each element of each @racket[h] to @racket[h0] in turn.  For each
key @racket[k] and value @racket[v], if a mapping from @racket[k] to some value
@racket[v0] already exists, it is replaced with a mapping from @racket[k] to
@racket[(combine/key k v0 v)].

@defexamples[
#:eval the-eval
(hash-union (make-immutable-hash '([1 . one]))
            (make-immutable-hash '([2 . two]))
            (make-immutable-hash '([3 . three])))
(hash-union (make-immutable-hash '([1 . (one uno)] [2 . (two dos)]))
            (make-immutable-hash '([1 . (ein une)] [2 . (zwei deux)]))
            #:combine/key (lambda (k v1 v2) (append v1 v2)))
]

}

@defproc[(hash-union! [h0 (and/c hash? hash-mutable?)]
                      [h hash?] ...
                      [#:combine combine
                                 (-> any/c any/c any/c)
                                 (lambda _ (error 'hash-union ....))]
                      [#:combine/key combine/key
                                     (-> any/c any/c any/c any/c)
                                     (lambda (k a b) (combine a b))])
         void?]{

Computes the union of @racket[h0] with each hash table @racket[h] by mutable
update, adding each element of each @racket[h] to @racket[h0] in turn.  For each
key @racket[k] and value @racket[v], if a mapping from @racket[k] to some value
@racket[v0] already exists, it is replaced with a mapping from @racket[k] to
@racket[(combine/key k v0 v)].

@defexamples[
#:eval the-eval
(define h (make-hash))
h
(hash-union! h (make-immutable-hash '([1 . (one uno)] [2 . (two dos)])))
h
(hash-union! h
             (make-immutable-hash '([1 . (ein une)] [2 . (zwei deux)]))
             #:combine/key (lambda (k v1 v2) (append v1 v2)))
h
]

}

@(close-eval the-eval)
