#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme unstable/cce/set))

@title[#:style 'quiet #:tag "cce-set"]{Sets}

@defmodule[unstable/cce/set]

This module provides tools for representing finite sets.

@section{Set Constructors}

@defproc[(set [#:mutable? mutable? boolean? weak?]
              [#:weak? weak? boolean? #f]
              [#:compare compare (or/c 'eq 'eqv 'equal) 'equal]
              [x any/c] ...)
         set?]{

Produces a hash table-based set using the hash table properties described by
any keyword arguments, and the given values as elements of the set.

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(set 1 2 3)
(set #:mutable? #t 1 2 3)
(set #:weak? #t 1 2 3)
(set #:compare 'eqv 1 2 3)
]

}

@defproc[(empty-set [#:mutable? mutable? boolean? weak?]
                    [#:weak? weak? boolean? #f]
                    [#:compare compare (or/c 'eq 'eqv 'equal) 'equal])
         set?]{

Produces an empty hash table-based set using the hash table properties described
by any keyword arguments.

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(empty-set)
(empty-set #:mutable? #t)
(empty-set #:weak? #t)
(empty-set #:compare 'eqv)
]

}

@defproc[(list->set [#:mutable? mutable? boolean? weak?]
                    [#:weak? weak? boolean? #f]
                    [#:compare compare (or/c 'eq 'eqv 'equal) 'equal]
                    [lst list?])
         set?]{

Produces a hash table-based set using the hash table properties described by
any keyword arguments, with the elements of the given list as the elements of
the set.

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(list->set '(1 2 3))
(list->set #:mutable? #t '(1 2 3))
(list->set #:weak? #t '(1 2 3))
(list->set #:compare 'eqv '(1 2 3))
]

}

@defproc[(custom-set [#:compare compare (-> any/c any/c any/c)]
                     [#:hash hash (-> any/c exact-integer?) (lambda (x) 0)]
                     [#:hash2 hash2 (-> any/c exact-integer?) (lambda (x) 0)]
                     [#:mutable? mutable? boolean? weak?]
                     [#:weak? weak? boolean? #f]
                     [elem any/c] ...)
         set?]{

Produces a custom hash table-based set using the given equality predicate
@scheme[equiv?] and optional hash functions @scheme[hash-primary] and
@scheme[hash-secondary].  If no hash functions are given, they default to a
degenerate hash function, resulting in an effectively list-based set.  The set
is populated with the given @scheme[elem] values.

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(define singularity
  (custom-set 'one 'two 'three
              #:mutable? #t
              #:compare (lambda (a b) #t)))
(set->list singularity)
(set-insert! singularity 'four)
(set->list singularity)
(set-remove! singularity 'zero)
(set->list singularity)
]

}

@section{Set Accessors}

@defproc[(set-contains? [s set?] [x any/c]) boolean?]{

Reports whether @scheme[s] contains @scheme[x].

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(set-contains? (set 1 2 3) 1)
(set-contains? (set 1 2 3) 4)
]

}

@defproc[(set-empty? [s set?]) boolean?]{

Reports whether a set is empty.

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(set-empty? '())
(set-empty? '((1 . one)))
]

}

@defproc[(set-count [s set?]) exact-nonnegative-integer?]{

Reports the number of elements in @scheme[s].

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(set-count (set))
(set-count (set 1 2 3))
]

}

@defproc[(set=? [a set?] [b set?]) boolean?]{

Reports whether two sets contain the same elements.

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(set=? (set 1) (set 1 2 3))
(set=? (set 1 2 3) (set 1))
(set=? (set 1 2 3) (set 1 2 3))
]

}

@defproc[(subset? [a set?] [b set?]) boolean?]{

Reports whether @scheme[b] contains all of the elements of @scheme[a].

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(subset? (set 1) (set 1 2 3))
(subset? (set 1 2 3) (set 1))
(subset? (set 1 2 3) (set 1 2 3))
]

}

@defproc[(proper-subset? [a set?] [b set?]) boolean?]{

Reports whether @scheme[b] contains all of the elements of @scheme[a], and at
least one element not in @scheme[a].

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(proper-subset? (set 1) (set 1 2 3))
(proper-subset? (set 1 2 3) (set 1))
(proper-subset? (set 1 2 3) (set 1 2 3))
]

}

@defproc[(set->list [s set?]) list?]{

Produces a list containing the elements of @scheme[s].

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(set->list (set 1 2 3))
]

}

@defproc[(in-set [s set?]) sequence?]{

Produces a sequence iterating over the elements of the set.

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(for/list ([x (in-set (set 1 2 3))]) x)
]

}

@section{Set Updaters}

@defproc[(set-insert [s set?] [x any/c]) set?]{

Produces a new version of @scheme[s] containing @scheme[x].

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(set-insert (set 1 2 3) 4)
(set-insert (set 1 2 3) 1)
]

}

@defproc[(set-remove [s set?] [x any/c]) set?]{

Produces a new version of @scheme[s] that does not contain @scheme[x].

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(set-remove (set 1 2 3) 1)
(set-remove (set 1 2 3) 4)
]

}

@defproc[(set-insert! [s set?] [x any/c]) void?]{

Mutates @scheme[s] to contain @scheme[x].

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(define s (set #:mutable? #t 1 2 3))
s
(set-insert! s 4)
s
(set-insert! s 1)
s
]

}

@defproc[(set-remove! [s set?] [x any/c]) void?]{

Mutates @scheme[x] so as not to contain @scheme[x].

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(define s (set #:mutable? #t 1 2 3))
s
(set-remove! s 1)
s
(set-remove! s 4)
s
]

}

@defproc[(set-union [s0 (and/c set? set-can-insert?)] [s set?] ...) set?]{

Produces a new version of @scheme[s0] containing all the elements in each
@scheme[s].

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(set-union (set 1 2) (set 1 3) (set 2 3))
]

}

@defproc[(set-intersection [s0 (and/c set? set-can-remove?)] [s set?] ...)
         set?]{

Produces a new version of @scheme[s0] containing only those elements found in
every @scheme[s].

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(set-intersection (set 1 2 3) (set 1 2) (set 2 3))
]

}

@defproc[(set-difference [s0 (and/c set? set-can-remove?)] [s set?] ...) set?]{

Produces a new version of @scheme[s0] containing only those elements not found
in any @scheme[s].

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(set-difference (set 1 2 3) (set 1) (set 3))
]

}

@defproc[(set-exclusive-or [s0 (and/c set? set-can-insert? set-can-remove?)]
                           [s set?] ...)
         set?]{

Produces a new version of @scheme[s0] containing only those elements found in
@scheme[s0] and each @scheme[s] an odd number of times.

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(set-exclusive-or (set 1) (set 1 2) (set 1 2 3))
]

}

@section{Set Predicates}

@defproc[(set? [x any/c]) boolean?]{

Recognizes sets.  A @deftech{set} is either a @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{dictionary} or a structure with the
@scheme[prop:set] property.

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(set? '(1 2))
(set? '((1 . one) (2 . two)))
]

}

@deftogether[(
@defproc[(set-can-insert? [s set?]) boolean?]
@defproc[(set-can-remove? [s set?]) boolean?]
@defproc[(set-can-insert!? [s set?]) boolean?]
@defproc[(set-can-remove!? [s set?]) boolean?]
)]{

Report whether @scheme[s] supports @scheme[set-insert], @scheme[set-remove],
@scheme[set-insert!], or @scheme[set-remove!], respectively.

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(define functional-set (set 1 2 3))
(set-can-insert? functional-set)
(set-can-remove? functional-set)
(set-can-insert!? functional-set)
(set-can-remove!? functional-set)
(define imperative-set (set #:mutable? #t 1 2 3))
(set-can-insert? imperative-set)
(set-can-remove? imperative-set)
(set-can-insert!? imperative-set)
(set-can-remove!? imperative-set)
]

}

@section{Structures as Sets}

@defthing[prop:set struct-type-property?]{

Property for structurs as @tech{sets}.  Its value must be a vector of 7
elements, as follows:

@itemlist[

@item{a binary function implementing @scheme[set-contains?],}

@item{a binary function implementing @scheme[set-insert!], or @scheme[#f] if not
supported,}

@item{a binary function implementing @scheme[set-insert], or @scheme[#f] if not
supported,}

@item{a binary function implementing @scheme[set-remove!], or @scheme[#f] if not
supported,}

@item{a binary function implementing @scheme[set-remove], or @scheme[#f] if
not supported,}

@item{a unary function implementing @scheme[set-count],}

@item{and a unary function implementing @scheme[in-set].}

]

@defexamples[
#:eval (evaluator 'unstable/cce/set)
(define (never-contains? set elem) #f)
(define (never-insert! set elem) (error 'set-insert! "always empty!"))
(define (never-insert set elem) (error 'set-insert "always empty!"))
(define (never-remove! set elem) (void))
(define (never-remove set elem) set)
(define (always-zero set) 0)
(define (no-elements set) null)

(define-struct always-empty []
  #:transparent
  #:property prop:set
  (vector never-contains?
          never-insert!
          never-insert
          never-remove!
          never-remove
          always-zero
          no-elements))

(set? (make-always-empty))
(set-contains? (make-always-empty) 1)
(set-insert! (make-always-empty) 2)
(set-insert (make-always-empty) 3)
(set-remove (make-always-empty) 4)
(set-remove! (make-always-empty) 5)
(set-count (make-always-empty))
(for ([x (in-set (make-always-empty))])
  (printf "~s\n" x))
]

}
