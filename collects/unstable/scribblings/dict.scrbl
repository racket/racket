#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/dict))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/dict unstable/dict))

@title{Dictionaries}

@defmodule[unstable/dict]

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

This module provides tools for manipulating dictionary values.

@defproc[(dict-empty? [d dict?]) boolean?]{

Reports whether @racket[d] is empty (has no keys).

@defexamples[
#:eval the-eval
(dict-empty? '())
(dict-empty? '([1 . one] [2 . two]))
]

}

@defproc[(dict-union [d0 (and/c dict? dict-can-functional-set?)]
                     [d dict?] ...
                     [#:combine combine
                                (-> any/c any/c any/c)
                                (lambda _ (error 'dict-union ...))]
                     [#:combine/key combine/key
                                    (-> any/c any/c any/c any/c)
                                    (lambda (k a b) (combine a b))])
         (and/c dict? dict-can-functional-set?)]{

Computes the union of @racket[d0] with each dictionary @racket[d] by functional
update, adding each element of each @racket[d] to @racket[d0] in turn.  For each
key @racket[k] and value @racket[v], if a mapping from @racket[k] to some value
@racket[v0] already exists, it is replaced with a mapping from @racket[k] to
@racket[(combine/key k v0 v)].

@defexamples[
#:eval the-eval
(dict-union '([1 . one]) '([2 . two]) '([3 . three]))
(dict-union '([1 . (one uno)] [2 . (two dos)])
            '([1 . (ein une)] [2 . (zwei deux)])
            #:combine/key (lambda (k v1 v2) (append v1 v2)))
]

}

@defproc[(dict-union! [d0 (and/c dict? dict-mutable?)]
                      [d dict?] ...
                      [#:combine combine
                                 (-> any/c any/c any/c)
                                 (lambda _ (error 'dict-union! ...))]
                      [#:combine/key combine/key
                                     (-> any/c any/c any/c any/c)
                                     (lambda (k a b) (combine a b))])
         void?]{

Computes the union of @racket[d0] with each dictionary @racket[d] by mutable
update, adding each element of each @racket[d] to @racket[d0] in turn.  For each
key @racket[k] and value @racket[v], if a mapping from @racket[k] to some value
@racket[v0] already exists, it is replaced with a mapping from @racket[k] to
@racket[(combine/key k v0 v)].

@defexamples[
#:eval the-eval
(define d (make-hash))
d
(dict-union! d '([1 . (one uno)] [2 . (two dos)]))
d
(dict-union! d
             '([1 . (ein une)] [2 . (zwei deux)])
             #:combine/key (lambda (k v1 v2) (append v1 v2)))
d
]

}

@(close-eval the-eval)
