#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme unstable/cce/hash))

@title[#:style 'quiet #:tag "cce-hash"]{Hash Tables}

@defmodule[unstable/cce/hash]

This module provides tools for manipulating hash tables.

@section{Hash Table Construction}

@defform/subs[
(hash immutable-hash-type [key-expr value-expr] ...)
[(immutable-hash-type code:blank #:eq #:eqv #:equal)]
]{

Produces an immutable hash table based on the given comparison, defaulting to
@scheme[#:equal], and mapping the result of each @scheme[key-expr] to the result
of each @scheme[value-expr].

@defexamples[
#:eval (evaluator 'unstable/cce/hash)
(hash ['one 1] ['two 2])
(hash #:eq ['one 1] ['two 2])
(hash #:eqv ['one 1] ['two 2])
(hash #:equal ['one 1] ['two 2])
]

}

@defform/subs[
(hash! mutable-hash-spec [key-expr value-expr] ...)
[(mutable-hash-spec (code:line mutable-hash-type mutable-hash-weak)
                    (code:line mutable-hash-weak mutable-hash-type))
 (mutable-hash-type code:blank #:eq #:eqv #:equal)
 (mutable-hash-weak code:blank #:weak)]
]{

Produces a mutable hash table based on the given comparison and weakness
specification, defaulting to @scheme[#:equal] and not @scheme[#:weak], and
mapping the result of each @scheme[key-expr] to the result of each
@scheme[value-expr].

@defexamples[
#:eval (evaluator 'unstable/cce/hash)
(hash! ['one 1] ['two 2])
(hash! #:eq ['one 1] ['two 2])
(hash! #:eqv #:weak ['one 1] ['two 2])
(hash! #:weak #:equal ['one 1] ['two 2])
]

}

@section{Hash Table Lookup}

@defproc[(hash-ref/check [h hash?] [k (lambda (k) (hash-has-key? h k))])
         any/c]{

Looks up key @scheme[k] in hash table @scheme[h].  Raises a contract error if
@scheme[h] has no entry for @scheme[k].  Equivalent to @scheme[(hash-ref h k)],
except for the specific exception value raised.

@defexamples[
#:eval (evaluator 'unstable/cce/hash)
(hash-ref/check (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 2)
]

}

@defproc[(hash-ref/identity [h hash?] [k any/c]) any/c]{

Looks up key @scheme[k] in hash table @scheme[h].  Returns @scheme[k] if
@scheme[h] has no entry for @scheme[k].  Equivalent to
@scheme[(hash-ref h k (lambda () k))].

@defexamples[
#:eval (evaluator 'unstable/cce/hash)
(hash-ref/identity (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 2)
(hash-ref/identity (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 4)
]

}

@defproc[(hash-ref/default [h hash?] [k any/c] [v any/c]) any/c]{

Looks up key @scheme[k] in hash table @scheme[h].  Returns @scheme[v] if
@scheme[h] has no entry for @scheme[k].  Equivalent to
@scheme[(hash-ref h k (lambda () v))].

@defexamples[
#:eval (evaluator 'unstable/cce/hash)
(hash-ref/default (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 2 'other)
(hash-ref/default (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 4 'other)
]

}

@defproc[(hash-ref/failure [h hash?] [k any/c] [f (-> any/c)]) any/c]{

Looks up key @scheme[k] in hash table @scheme[h].  Returns the result of
applying @scheme[f] (in tail position) if @scheme[h] has no entry for
@scheme[k].  Equivalent to @scheme[(hash-ref h k f)].

@defexamples[
#:eval (evaluator 'unstable/cce/hash)
(hash-ref/failure (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 2 gensym)
(hash-ref/failure (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 4 gensym)
]

}

@section{Hash Table Accessors}

@defproc[(hash-equal? [h hash?]) boolean?]{

Reports whether @scheme[h] maps keys according to @scheme[equal?].

@defexamples[
#:eval (evaluator 'unstable/cce/hash)
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
#:eval (evaluator 'unstable/cce/hash)
(hash-has-key? (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 2)
(hash-has-key? (make-immutable-hash '([1 . one] [2 . two] [3 . three])) 4)
]

}

@defproc[(hash-domain [h hash?]) list?]{

Produces the domain of a hash table as a list of keys.

@defexamples[
#:eval (evaluator 'unstable/cce/hash)
(hash-domain (make-immutable-hash '([1 . one] [2 . two] [3 . three])))
]

}

@defproc[(hash-range [h hash?]) list?]{

Produces the range of a hash table as a list of values.

@defexamples[
#:eval (evaluator 'unstable/cce/hash)
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
#:eval (evaluator 'unstable/cce/hash)
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
#:eval (evaluator 'unstable/cce/hash)
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
