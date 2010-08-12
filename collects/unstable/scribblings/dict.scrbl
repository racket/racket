#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/dict))

@title{Dictionaries}

@defmodule[unstable/dict]

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

This module provides tools for manipulating dictionary values.

@section{Dictionary Constructors}

@defproc[(empty-dict [#:mutable? mutable? boolean? weak?]
                     [#:weak? weak? boolean? #f]
                     [#:compare compare (or/c 'eq 'eqv 'equal) equal])
         hash?]{

Constructs an empty hash table based on the behavior specified by
@scheme[mutable?], @scheme[weak?], and @scheme[compare].

@defexamples[
#:eval (eval/require 'racket/dict 'unstable/dict)
(empty-dict)
(empty-dict #:mutable? #t)
(empty-dict #:weak? #t)
(empty-dict #:compare 'eqv)
]

}

@defproc[(make-dict [d dict?]
                    [#:mutable? mutable? boolean? weak?]
                    [#:weak? weak? boolean? #f]
                    [#:compare compare (or/c 'eq 'eqv 'equal) equal])
         hash?]{

Converts a given dictionary @scheme[d] to a hash table based on the behavior
specified by @scheme[mutable?], @scheme[weak?], and @scheme[compare].

@defexamples[
#:eval (eval/require 'racket/dict 'unstable/dict)
(make-dict '([1 . one] [2 . two]))
(make-dict '([1 . one] [2 . two]) #:mutable? #t)
(make-dict '([1 . one] [2 . two]) #:weak? #t)
(make-dict '([1 . one] [2 . two]) #:compare 'eqv)
]

}

@defproc[(custom-dict [equiv? (-> any/c any/c any/c)]
                      [hash-primary (-> any/c exact-integer?) (lambda (x) 0)]
                      [hash-secondary (-> any/c exact-integer?) (lambda (x) 0)]
                      [#:mutable? mutable? boolean? weak?]
                      [#:weak? weak? boolean? #f])
         dict?]{

Constructs a dictionary based on custom comparison and optional hash functions.
Given no hash functions, the dictionary defaults to a degenerate hash function
and is thus essentially equivalent to a list-based dictionary.

@defexamples[
#:eval (eval/require 'racket/dict 'unstable/dict)
(define table (custom-dict = add1 sub1 #:mutable? #t))
(dict-set! table 1 'one)
(dict-set! table 2 'two)
(for/list ([(key val) (in-dict table)])
  (cons key val))
]

}

@section{Dictionary Lookup}

@defproc[(dict-ref/check [d dict?] [k (lambda (k) (dict-has-key? d k))])
         any/c]{

Looks up key @scheme[k] in dictionary @scheme[d].  Raises a contract error if
@scheme[d] has no entry for @scheme[k].  Equivalent to @scheme[(dict-ref d k)],
except for the specific exception value raised.

@defexamples[
#:eval (eval/require 'racket/dict 'unstable/dict)
(dict-ref/check '([1 . one] [2 . two] [3 . three]) 2)
]

}

@defproc[(dict-ref/identity [d dict?] [k any/c]) any/c]{

Looks up key @scheme[k] in dictionary @scheme[d].  Returns @scheme[k] if
@scheme[d] has no entry for @scheme[k].  Equivalent to
@scheme[(dict-ref d k (lambda () k))].

@defexamples[
#:eval (eval/require 'racket/dict 'unstable/dict)
(dict-ref/identity '([1 . one] [2 . two] [3 . three]) 2)
(dict-ref/identity '([1 . one] [2 . two] [3 . three]) 4)
]

}

@defproc[(dict-ref/default [d dict?] [k any/c] [v any/c]) any/c]{

Looks up key @scheme[k] in dictionary @scheme[d].  Returns @scheme[v] if
@scheme[d] has no entry for @scheme[k].  Equivalent to
@scheme[(dict-ref d k (lambda () v))].

@defexamples[
#:eval (eval/require 'racket/dict 'unstable/dict)
(dict-ref/default '([1 . one] [2 . two] [3 . three]) 2 'other)
(dict-ref/default '([1 . one] [2 . two] [3 . three]) 4 'other)
]

}

@defproc[(dict-ref/failure [d dict?] [k any/c] [f (-> any/c)]) any/c]{

Looks up key @scheme[k] in dictionary @scheme[d].  Returns the result of
applying @scheme[f] (in tail position) if @scheme[d] has no entry for
@scheme[k].  Equivalent to @scheme[(dict-ref d k f)].

@defexamples[
#:eval (eval/require 'racket/dict 'unstable/dict)
(dict-ref/failure '([1 . one] [2 . two] [3 . three]) 2 gensym)
(dict-ref/failure '([1 . one] [2 . two] [3 . three]) 4 gensym)
]

}

@section{Dictionary Accessors}

@defproc[(dict-empty? [d dict?]) boolean?]{

Reports whether @scheme[d] is empty (has no keys).

@defexamples[
#:eval (eval/require 'racket/dict 'unstable/dict)
(dict-empty? '())
(dict-empty? '([1 . one] [2 . two]))
]

}

@defproc[(dict-has-key? [d dict?] [k any/c]) boolean?]{

Reports whether @scheme[d] has an entry for @scheme[k].

@defexamples[
#:eval (eval/require 'racket/dict 'unstable/dict)
(dict-has-key? '([1 . one] [2 . two] [3 . three]) 2)
(dict-has-key? '([1 . one] [2 . two] [3 . three]) 4)
]

}

@section{Dictionary Combinations}

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

@section{Dictionary Structure Properties}

@defproc[(wrapped-dict-property
          [#:unwrap unwrap (-> (and/c dict? pred) dict?)]
          [#:wrap wrap (-> dict? (and/c dict? pred)) (lambda (x) x)]
          [#:predicate pred (-> any/c boolean?) (lambda (x) #t)]
          [#:mutable? mutable? boolean? weak?]
          [#:weak? mutable? boolean? #f]
          [#:functional? functional? boolean? #t])
         vector?]{

Produces a value appropriate for @scheme[prop:dict] for a derived dictionary
type recognized by @scheme[pred].  Dictionaries constructed from this property
will extract a nested dictionary using @scheme[unwrap] and will produce a
wrapped dictionary during functional update using @scheme[wrap].

@defexamples[
#:eval (eval/require 'racket/dict 'unstable/dict)
(define-struct table [dict]
  #:transparent
  #:property prop:dict
  (wrapped-dict-property
   #:unwrap (lambda (d) (table-dict d))
   #:wrap (lambda (d) (make-table d))
   #:predicate (lambda (d) (table? d))))
(dict? (make-table '([1 . one] [2 . two])))
(dict-ref (make-table '([1 . one] [2 . two])) 1)
(dict-set (make-table '([1 . one] [2 . two])) 3 'three)
]

}

@section{Contracted Dictionaries}

This library re-provides @scheme[dict/c] from
@schememodname[unstable/contract].
