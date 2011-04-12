#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/dict))

@title{Dictionaries}

@defmodule[unstable/dict]

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

This module provides tools for manipulating dictionary values.

@defproc[(dict-empty? [d dict?]) boolean?]{

Reports whether @scheme[d] is empty (has no keys).

@defexamples[
#:eval (eval/require 'racket/dict 'unstable/dict)
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

Computes the union of @scheme[d0] with each dictionary @scheme[d] by functional
update, adding each element of each @scheme[d] to @scheme[d0] in turn.  For each
key @scheme[k] and value @scheme[v], if a mapping from @scheme[k] to some value
@scheme[v0] already exists, it is replaced with a mapping from @scheme[k] to
@scheme[(combine/key k v0 v)].

@defexamples[
#:eval (eval/require 'racket/dict 'unstable/dict)
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

Computes the union of @scheme[d0] with each dictionary @scheme[d] by mutable
update, adding each element of each @scheme[d] to @scheme[d0] in turn.  For each
key @scheme[k] and value @scheme[v], if a mapping from @scheme[k] to some value
@scheme[v0] already exists, it is replaced with a mapping from @scheme[k] to
@scheme[(combine/key k v0 v)].

@defexamples[
#:eval (eval/require 'racket/dict 'unstable/dict)
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
