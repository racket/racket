#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label scheme unstable/hash))

@title{Hash Tables}

@defmodule[unstable/hash]

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

This module provides tools for manipulating hash tables.

@section{Hash Table Lookup}

@defproc[(hash-ref/check [h hash?] [k (lambda (k) (hash-has-key? h k))])
         any/c]{

Looks up key @scheme[k] in hash table @scheme[h].  Raises a contract error if
@scheme[h] has no entry for @scheme[k].  Equivalent to @scheme[(hash-ref h k)],
except for the specific exception value raised.

@defexamples[
#:eval (eval/require 'unstable/hash)
(hash-ref/check (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 2)
]

}

@defproc[(hash-ref/identity [h hash?] [k any/c]) any/c]{

Looks up key @scheme[k] in hash table @scheme[h].  Returns @scheme[k] if
@scheme[h] has no entry for @scheme[k].  Equivalent to
@scheme[(hash-ref h k (lambda () k))].

@defexamples[
#:eval (eval/require 'unstable/hash)
(hash-ref/identity (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 2)
(hash-ref/identity (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 4)
]

}

@defproc[(hash-ref/default [h hash?] [k any/c] [v any/c]) any/c]{

Looks up key @scheme[k] in hash table @scheme[h].  Returns @scheme[v] if
@scheme[h] has no entry for @scheme[k].  Equivalent to
@scheme[(hash-ref h k (lambda () v))].

@defexamples[
#:eval (eval/require 'unstable/hash)
(hash-ref/default (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 2 'other)
(hash-ref/default (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 4 'other)
]

}

@defproc[(hash-ref/failure [h hash?] [k any/c] [f (-> any/c)]) any/c]{

Looks up key @scheme[k] in hash table @scheme[h].  Returns the result of
applying @scheme[f] (in tail position) if @scheme[h] has no entry for
@scheme[k].  Equivalent to @scheme[(hash-ref h k f)].

@defexamples[
#:eval (eval/require 'unstable/hash)
(hash-ref/failure (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 2 gensym)
(hash-ref/failure (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 4 gensym)
]

}

@section{Hash Table Accessors}

@defproc[(hash-equal? [h hash?]) boolean?]{

Reports whether @scheme[h] maps keys according to @scheme[equal?].

@defexamples[
#:eval (eval/require 'unstable/hash)
(hash-equal? #hash())
(hash-equal? #hasheq())
(hash-equal? #hasheqv())
]

}

@defproc[(hash-has-key? [h hash?] [k any/c]) boolean?]{

Reports whether @scheme[h] has an entry for @scheme[k].  This function is
re-exported from @schememodname[scheme/base].  In versions of PLT Scheme before
@scheme[hash-has-key?] was implemented, this module provides its own definition.

@defexamples[
#:eval (eval/require 'unstable/hash)
(hash-has-key? (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 2)
(hash-has-key? (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 4)
]

}

@defproc[(hash-domain [h hash?]) list?]{

Produces the domain of a hash table as a list of keys.

@defexamples[
#:eval (eval/require 'unstable/hash)
(hash-domain (make-immutable-hash '([1 . one] [2 . two] [3 . three])))
]

}

@defproc[(hash-range [h hash?]) list?]{

Produces the range of a hash table as a list of values.

@defexamples[
#:eval (eval/require 'unstable/hash)
(hash-range (make-immutable-hash '([1 . one] [2 . two] [3 . three])))
]

}

@section{Hash Table Combinations}

@defproc[(hash-union [h0 (and/c hash? hash-can-functional-set?)]
                     [h hash?] ...
                     [#:combine combine
                                (-> any/c any/c any/c)
                                (lambda _ (error 'hash-union ...))]
                     [#:combine/key combine/key
                                    (-> any/c any/c any/c any/c)
                                    (lambda (k a b) (combine a b))])
         (and/c hash? hash-can-functional-set?)]{

Computes the union of @scheme[h0] with each hash table @scheme[h] by functional
update, adding each element of each @scheme[h] to @scheme[h0] in turn.  For each
key @scheme[k] and value @scheme[v], if a mapping from @scheme[k] to some value
@scheme[v0] already exists, it is replaced with a mapping from @scheme[k] to
@scheme[(combine/key k v0 v)].

@defexamples[
#:eval (eval/require 'unstable/hash)
(hash-union (make-immutable-hash '([1 . one])) (make-immutable-hash '([2 . two])) (make-immutable-hash '([3 . three])))
(hash-union (make-immutable-hash '([1 . (one uno)] [2 . (two dos)]))
            (make-immutable-hash '([1 . (ein une)] [2 . (zwei deux)]))
            #:combine/key (lambda (k v1 v2) (append v1 v2)))
]

}

@defproc[(hash-union! [h0 (and/c hash? hash-mutable?)]
                      [h hash?] ...
                      [#:combine combine
                                 (-> any/c any/c any/c)
                                 (lambda _ (error 'hash-union ...))]
                      [#:combine/key combine/key
                                     (-> any/c any/c any/c any/c)
                                     (lambda (k a b) (combine a b))])
         void?]{

Computes the union of @scheme[h0] with each hash table @scheme[h] by mutable
update, adding each element of each @scheme[h] to @scheme[h0] in turn.  For each
key @scheme[k] and value @scheme[v], if a mapping from @scheme[k] to some value
@scheme[v0] already exists, it is replaced with a mapping from @scheme[k] to
@scheme[(combine/key k v0 v)].

@defexamples[
#:eval (eval/require 'unstable/hash)
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
