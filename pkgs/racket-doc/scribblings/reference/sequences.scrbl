#lang scribble/doc
@(require "mz.rkt"
          scribble/scheme
          (for-syntax racket/base)
          (for-label racket/generator
                     racket/generic
                     compatibility/mlist
                     syntax/stx))

@(define (info-on-seq where what)
   @margin-note{See @secref[where] for information on using @|what| as
                sequences.})

@(define (for-element-reachability what)
   @elem{See @racket[for] for information on the reachability of @|what| elements
         during an iteration.})

@title[#:style 'toc #:tag "sequences+streams"]{Sequences and Streams}

@tech{Sequences} and @tech{streams} abstract over iteration of elements in a
collection. Sequences allow iteration with @racket[for] macros or with sequence
operations such as @racket[sequence-map]. Streams are functional sequences that
can be used either in a generic way or a stream-specific way. @tech{Generators}
are closely related stateful objects that can be converted to a sequence and
vice-versa.

@local-table-of-contents[]

@; ======================================================================
@section[#:tag "sequences"]{Sequences}

@(define sequence-evaluator
   (let ([evaluator (make-base-eval)])
     (evaluator '(require racket/generic racket/list racket/stream racket/sequence
                          racket/contract racket/dict))
     evaluator))

@guideintro["sequences"]{sequences}

A @deftech{sequence} encapsulates an ordered collection of values.
The elements of a sequence can be extracted with one of the
@racket[for] syntactic forms, with the procedures returned by
@racket[sequence-generate], or by converting the sequence into a
@tech{stream}.

The sequence datatype overlaps with many other datatypes.  Among
built-in datatypes, the sequence datatype includes the following:

@itemize[

 @item{exact nonnegative integers (see below)}

 @item{strings (see @secref["strings"])}

 @item{byte strings (see @secref["bytestrings"])}

 @item{lists (see @secref["pairs"])}

 @item{mutable lists (see @secref["mpairs"])}

 @item{vectors (see @secref["vectors"])}

 @item{flvectors (see @secref["flvectors"])}

 @item{fxvectors (see @secref["fxvectors"])}

 @item{hash tables (see @secref["hashtables"])}

 @item{dictionaries (see @secref["dicts"])}

 @item{sets (see @secref["sets"])}

 @item{input ports (see @secref["ports"])}

 @item{streams (see @secref["streams"])}

]

An @tech{exact number} @racket[_k] that is a non-negative
@tech{integer} acts as a sequence similar to @racket[(in-range _k)],
except that @racket[_k] by itself is not a @tech{stream}.

Custom sequences can be defined using structure type properties.  The
easiest method to define a custom sequence is to use the
@racket[gen:stream] @tech{generic interface}. Streams are a suitable
abstraction for data structures that are directly iterable.  For
example, a list is directly iterable with @racket[first] and
@racket[rest]. On the other hand, vectors are not directly iterable:
iteration has to go through an index. For data structures that are not
directly iterable, the @deftech{iterator} for the data structure can
be defined to be a stream (e.g., a structure containing the index of a
vector).

For example, unrolled linked lists (represented as a list of vectors)
themselves do not fit the stream abstraction, but have index-based
iterators that can be represented as streams:

@examples[#:eval sequence-evaluator
  (struct unrolled-list-iterator (idx lst)
    #:methods gen:stream
    [(define (stream-empty? iter)
       (define lst (unrolled-list-iterator-lst iter))
       (or (null? lst)
           (and (>= (unrolled-list-iterator-idx iter)
                    (vector-length (first lst)))
                (null? (rest lst)))))
     (define (stream-first iter)
       (vector-ref (first (unrolled-list-iterator-lst iter))
                   (unrolled-list-iterator-idx iter)))
     (define (stream-rest iter)
       (define idx (unrolled-list-iterator-idx iter))
       (define lst (unrolled-list-iterator-lst iter))
       (if (>= idx (sub1 (vector-length (first lst))))
           (unrolled-list-iterator 0 (rest lst))
           (unrolled-list-iterator (add1 idx) lst)))])

  (define (make-unrolled-list-iterator ul)
    (unrolled-list-iterator 0 (unrolled-list-lov ul)))

  (struct unrolled-list (lov)
    #:property prop:sequence
    make-unrolled-list-iterator)

  (define ul1 (unrolled-list '(#(cracker biscuit) #(cookie scone))))
  (for/list ([x ul1]) x)
]

The @racket[prop:sequence] property provides more flexibility in
specifying iteration, such as when a pre-processing step is needed to
prepare the data for iteration.  The @racket[make-do-sequence]
function creates a sequence given a thunk that returns procedures to
implement a sequence, and the @racket[prop:sequence] property can be
associated with a structure type to implement its implicit conversion
to a sequence.

For most sequence types, extracting elements from a sequence has no
side-effect on the original sequence value; for example, extracting
the sequence of elements from a list does not change the list.  For
other sequence types, each extraction implies a side effect; for
example, extracting the sequence of bytes from a port causes the bytes
to be read from the port. @elemtag["sequence-state"]{A} sequence's state may either span all uses
of the sequence, as for a port, or it may be confined to each distinct
time that a sequence is @deftech{initiate}d by a @racket[for] form,
@racket[sequence->stream], @racket[sequence-generate], or
@racket[sequence-generate*]. Concretely, the thunk passed to
@racket[make-do-sequence] is called to @tech{initiate} the sequence
each time the sequence is used. Accordingly, different sequences behave
differently when they are @tech{initiate}d multiple times.

@examples[#:eval sequence-evaluator
          #:label #f
          (define (double-initiate s1)
            (code:comment "initiate the sequence twice")
            (define-values (more?.1 next.1) (sequence-generate s1))
            (define-values (more?.2 next.2) (sequence-generate s1))
            (code:comment "alternate fetching from sequence via the two initiations")
            (list (next.1) (next.2) (next.1) (next.2)))

          (double-initiate (open-input-string "abcdef"))
          (double-initiate (list 97 98 99 100))
          (double-initiate (in-naturals 97))]

Also, subsequent elements in a sequence may be ``consumed'' just by calling the
first result of @racket[sequence-generate], even if the second
result is never called.

@examples[#:eval sequence-evaluator
          #:label #f
          (define (double-initiate-and-use-more? s1)
            (code:comment "initiate the sequence twice")
            (define-values (more?.1 next.1) (sequence-generate s1))
            (define-values (more?.2 next.2) (sequence-generate s1))
            (code:comment "alternate fetching from sequence via the two initiations")
            (code:comment "but this time call `more?` in between")
            (list (next.1) (more?.1) (next.2) (more?.2)
                  (next.1) (more?.1) (next.2) (more?.2)))

          (double-initiate-and-use-more? (open-input-string "abcdef"))]

In this example, the state embedded in the first call to @racket[sequence-generate]
``takes'' the @racket[98] just by virtue of the invocation of @racket[_more?.1].

Individual elements of a sequence typically correspond to single
values, but an element may also correspond to multiple values.  For
example, a hash table generates two values---a key and its value---for
each element in the sequence.

@; ----------------------------------------------------------------------
@subsection{Sequence Predicate and Constructors}

@defproc[(sequence? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] can be used as a @tech{sequence},
  @racket[#f] otherwise.

@examples[#:eval sequence-evaluator
  (sequence? 42)
  (sequence? '(a b c))
  (sequence? "word")
  (sequence? #\x)]}

@defproc*[([(in-range [end real?]) stream?]
           [(in-range [start real?] [end real?] [step real? 1]) stream?])]{
  Returns a sequence (that is also a @tech{stream}) whose elements are
  numbers.  The single-argument case @racket[(in-range end)] is
  equivalent to @racket[(in-range 0 end 1)].  The first number in the
  sequence is @racket[start], and each successive element is generated
  by adding @racket[step] to the previous element.  The sequence stops
  before an element that would be greater or equal to @racket[end] if
  @racket[step] is non-negative, or less or equal to @racket[end] if
  @racket[step] is negative.  @speed[in-range "number"]


  @examples[#:label "Example: gaussian sum" #:eval sequence-evaluator
    (for/sum ([x (in-range 10)]) x)]


  @examples[#:label "Example: sum of even numbers" #:eval sequence-evaluator
    (for/sum ([x (in-range 0 100 2)]) x)]

  When given zero as @racket[step], @racket[in-range] returns an infinite
  sequence. It may also return infinite sequences when @racket[step] is a very
  small number, and either @racket[step] or the sequence elements are
  floating-point numbers.
}

@defproc[(in-inclusive-range [start real?] [end real?] [step real? 1]) stream?]{

  Similar to @racket[in-range], but the sequence stopping condition is changed so that
  the last element is allowed to be equal to @racket[end]. @speed[in-inclusive-range "number"]

  @examples[#:eval sequence-evaluator
    (sequence->list (in-inclusive-range 7 11))
    (sequence->list (in-inclusive-range 7 11 2))
    (sequence->list (in-inclusive-range 7 10 2))
  ]

  @history[#:added "8.0.0.13"]
}


@defproc[(in-naturals [start exact-nonnegative-integer? 0]) stream?]{
  Returns an infinite sequence (that is also a @tech{stream}) of exact
  integers starting with @racket[start], where each element is one
  more than the preceding element.  @speed[in-naturals "integer"]

  @examples[#:eval sequence-evaluator
    (for/list ([k (in-naturals)]
               [x (in-range 10)])
      (list k x))]
}


@defproc[(in-list [lst list?]) stream?]{
  Returns a sequence (that is also a @tech{stream}) that is equivalent
  to using @racket[lst] directly as a sequence.
  @info-on-seq["pairs" "lists"]
  @speed[in-list "list"]
  @for-element-reachability["list"]

  @examples[#:eval sequence-evaluator
    (for/list ([x (in-list '(3 1 4))])
      `(,x ,(* x x)))]

@history[#:changed "6.7.0.4" @elem{Improved element-reachability guarantee for lists in @racket[for].}]}


@defproc[(in-mlist [mlst mlist?]) sequence?]{
  Returns a sequence equivalent to @racket[mlst]. Although the
  expectation is that @racket[mlst] is @tech{mutable list}, @racket[in-mlist]
  initially checks only whether @racket[mlst] is a @tech{mutable pair} or @racket[null],
  since it could change during iteration.
  @info-on-seq["mpairs" "mutable lists"]
  @speed[in-mlist "mutable list"]

  @examples[#:eval sequence-evaluator
    (for/list ([x (in-mlist (mcons "RACKET" (mcons "LANG" '())))])
      (string-length x))]
}

@defproc[(in-vector [vec vector?]
                    [start exact-nonnegative-integer? 0]
                    [stop (or/c exact-integer? #f) #f]
                    [step (and/c exact-integer? (not/c zero?)) 1])
         sequence?]{
  Returns a sequence equivalent to @racket[vec] when no optional
  arguments are supplied.

  @info-on-seq["vectors" "vectors"]

  The optional arguments @racket[start], @racket[stop], and
  @racket[step] are analogous to @racket[in-range], except that a
  @racket[#f] value for @racket[stop] is equivalent to
  @racket[(vector-length vec)].  That is, the first element in the
  sequence is @racket[(vector-ref vec start)], and each successive
  element is generated by adding @racket[step] to index of the
  previous element.  The sequence stops before an index that would be
  greater or equal to @racket[end] if @racket[step] is non-negative,
  or less or equal to @racket[end] if @racket[step] is negative.

  If @racket[start] is not a valid index, then the
  @exnraise[exn:fail:contract], except when @racket[start], @racket[stop], and
  @racket[(vector-length vec)] are equal, in which case the result is an
  empty sequence.

  @examples[#:eval sequence-evaluator
            (for ([x (in-vector (vector 1) 1)]) x)
            (eval:error (for ([x (in-vector (vector 1) 2)]) x))
            (for ([x (in-vector (vector) 0 0)]) x)
            (for ([x (in-vector (vector 1) 1 1)]) x)]

  If @racket[stop] is not in [-1, @racket[(vector-length vec)]],
  then the @exnraise[exn:fail:contract].

  If @racket[start] is less than
  @racket[stop] and @racket[step] is negative, then the
  @exnraise[exn:fail:contract].  Similarly, if @racket[start]
  is more than @racket[stop] and @racket[step] is positive, then the
  @exnraise[exn:fail:contract].

  @speed[in-vector "vector"]

  @examples[#:eval sequence-evaluator
    (define (histogram vector-of-words)
      (define a-hash (make-hash))
      (for ([word (in-vector vector-of-words)])
        (hash-set! a-hash word (add1 (hash-ref a-hash word 0))))
      a-hash)
    (histogram #("hello" "world" "hello" "sunshine"))]
}

@defproc[(in-string [str string?]
                    [start exact-nonnegative-integer? 0]
                    [stop (or/c exact-integer? #f) #f]
                    [step (and/c exact-integer? (not/c zero?)) 1])
         sequence?]{
  Returns a sequence equivalent to @racket[str] when no optional
  arguments are supplied.

  @info-on-seq["strings" "strings"]

  The optional arguments @racket[start], @racket[stop], and
  @racket[step] are as in @racket[in-vector].

  @speed[in-string "string"]

  @examples[#:eval sequence-evaluator
    (define (line-count str)
      (for/sum ([ch (in-string str)])
        (if (char=? #\newline ch) 1 0)))
    (line-count "this string\nhas\nthree \nnewlines")]
}

@defproc[(in-bytes [bstr bytes?]
                   [start exact-nonnegative-integer? 0]
                   [stop (or/c exact-integer? #f) #f]
                   [step (and/c exact-integer? (not/c zero?)) 1])
         sequence?]{
  Returns a sequence equivalent to @racket[bstr] when no optional
  arguments are supplied.

  @info-on-seq["bytestrings" "byte strings"]

  The optional arguments @racket[start], @racket[stop], and
  @racket[step] are as in @racket[in-vector].

  @speed[in-bytes "byte string"]

  @examples[#:eval sequence-evaluator
    (define (has-eof? bs)
      (for/or ([ch (in-bytes bs)])
        (= ch 0)))
    (has-eof? #"this byte string has an \0embedded zero byte")
    (has-eof? #"this byte string does not")]
}

@defproc[(in-port [r (input-port? . -> . any/c) read]
                  [in input-port? (current-input-port)])
         sequence?]{
  Returns a sequence whose elements are produced by calling @racket[r]
  on @racket[in] until it produces @racket[eof].}

@defproc[(in-input-port-bytes [in input-port?]) sequence?]{
  Returns a sequence equivalent to @racket[(in-port read-byte in)].}

@defproc[(in-input-port-chars [in input-port?]) sequence?]{
  Returns a sequence whose elements are read as characters from
  @racket[in] (equivalent to @racket[(in-port read-char in)]).}

@defproc[(in-lines [in input-port? (current-input-port)]
                   [mode (or/c 'linefeed 'return 'return-linefeed 'any 'any-one) 'any])
         sequence?]{
  Returns a sequence equivalent to
  @racket[(in-port (lambda (p) (read-line p mode)) in)].  Note that
  the default mode is @racket['any], whereas the default mode of
  @racket[read-line] is @racket['linefeed].}

@defproc[(in-bytes-lines [in input-port? (current-input-port)]
                         [mode (or/c 'linefeed 'return 'return-linefeed 'any 'any-one) 'any])
         sequence?]{
  Returns a sequence equivalent to
  @racket[(in-port (lambda (p) (read-bytes-line p mode)) in)].  Note
  that the default mode is @racket['any], whereas the default mode of
  @racket[read-bytes-line] is @racket['linefeed].}

@defproc*[([(in-hash [hash hash?]) sequence?]
           [(in-hash [hash hash?] [bad-index-v any/c]) sequence?])]{
  Returns a sequence equivalent to @racket[hash], except when @racket[bad-index-v]
  is supplied.

  Like @racket[hash-map], iteration via @racket[in-hash] can adapt to
  certain modifications to a mutable hash table while a traversal is
  in progress. Keys removes or remapped by the traversing
  thread have no immediate adverse affects; the change does not affect
  a traversal if the key has been seen already, otherwise the
  traversal skips a deleted key or uses the remapped key's new value.

  Other concurrent modifications, including key removal by a different
  thread, can lead to skipped entries or an exception if an expected
  entry key was removed before its key or value could be fetched.
  If @racket[bad-index-v] is supplied, then @racket[bad-index-v] is
  returned as both the key and the value in the case that the
  @racket[hash] is modified concurrently so that iteration does not have a
  @tech{valid hash index}. Providing @racket[bad-index-v] is particularly
  useful when iterating through a hash table with weakly held keys, since
  entries can be removed asynchronously (i.e., after @racket[in-hash] has
  committed to another iteration, but before it can access the entry for the
  next iteration).

  @examples[
    (define table (hash 'a 1 'b 2))
    (for ([(key value) (in-hash table)])
      (printf "key: ~a value: ~a\n" key value))]

  @info-on-seq["hashtables" "hash tables"]

  @history[#:changed "7.0.0.10" @elem{Added the optional @racket[bad-index-v] argument.}
           #:changed "8.18.0.11" @elem{Strengthened the guarantees about traversal with
                                       same-thread modifications to a mutable hash table.}]}

@defproc*[([(in-hash-keys [hash hash?]) sequence?]
           [(in-hash-keys [hash hash?] [bad-index-v any/c]) sequence?])]{
  Returns a sequence whose elements are the keys of @racket[hash], using
  @racket[bad-index-v] in the same way as @racket[in-hash], and with
  concurrent-modification guarantees analogous to those of @racket[in-hash].

  @examples[
    (define table (hash 'a 1 'b 2))
    (for ([key (in-hash-keys table)])
      (printf "key: ~a\n" key))]

  @history[#:changed "7.0.0.10" @elem{Added the optional @racket[bad-index-v] argument.}
           #:changed "8.18.0.11" @elem{Strengthened the guarantees about traversal with
                                       same-thread modifications to a mutable hash table.}]}

@defproc*[([(in-hash-values [hash hash?]) sequence?]
           [(in-hash-values [hash hash?] [bad-index-v any/c]) sequence?])]{
  Returns a sequence whose elements are the values of @racket[hash], using
  @racket[bad-index-v] in the same way as @racket[in-hash], and with
  concurrent-modification guarantees analogous to those of @racket[in-hash].

  @examples[
    (define table (hash 'a 1 'b 2))
    (for ([value (in-hash-values table)])
      (printf "value: ~a\n" value))]

  @history[#:changed "7.0.0.10" @elem{Added the optional @racket[bad-index-v] argument.}
           #:changed "8.18.0.11" @elem{Strengthened the guarantees about traversal with
                                       same-thread modifications to a mutable hash table.}]}

@defproc*[([(in-hash-pairs [hash hash?]) sequence?]
           [(in-hash-pairs [hash hash?] [bad-index-v any/c]) sequence?])]{
  Returns a sequence whose elements are pairs, each containing a key
  and its value from @racket[hash] (as opposed to using @racket[hash]
  directly as a sequence to get the key and value as separate values
  for each element).

  The @racket[bad-index-v] argument, if supplied, is used in the same
  way as by @racket[in-hash]. When an invalid index is encountered,
  the pair in the sequence with have @racket[bad-index-v] as both its
  @racket[car] and @racket[cdr]. The concurrent-modification
  guarantees for @racket[in-hash-pairs] are analogous to those of @racket[in-hash].

  @examples[
    (define table (hash 'a 1 'b 2))
    (for ([key+value (in-hash-pairs table)])
      (printf "key and value: ~a\n" key+value))]

  @history[#:changed "7.0.0.10" @elem{Added the optional @racket[bad-index-v] argument.}
           #:changed "8.18.0.11" @elem{Strengthened the guarantees about traversal with
                                       same-thread modifications to a mutable hash table.}]}

@deftogether[(
@defproc[(in-mutable-hash
          [hash (and/c hash? (not/c immutable?) hash-strong?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-mutable-hash
          [hash (and/c hash? (not/c immutable?) hash-strong?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-mutable-hash-keys
          [hash (and/c hash? (not/c immutable?) hash-strong?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-mutable-hash-keys
          [hash (and/c hash? (not/c immutable?) hash-strong?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-mutable-hash-values
          [hash (and/c hash? (not/c immutable?) hash-strong?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-mutable-hash-values
          [hash (and/c hash? (not/c immutable?) hash-strong?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-mutable-hash-pairs
          [hash (and/c hash? (not/c immutable?) hash-strong?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-mutable-hash-pairs
          [hash (and/c hash? (not/c immutable?) hash-strong?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-immutable-hash
          [hash (and/c hash? immutable?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-immutable-hash
          [hash (and/c hash? immutable?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-immutable-hash-keys
          [hash (and/c hash? immutable?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-immutable-hash-keys
          [hash (and/c hash? immutable?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-immutable-hash-values
          [hash (and/c hash? immutable?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-immutable-hash-values
          [hash (and/c hash? immutable?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-immutable-hash-pairs
          [hash (and/c hash? immutable?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-immutable-hash-pairs
          [hash (and/c hash? immutable?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-weak-hash
          [hash (and/c hash? hash-weak?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-weak-hash
          [hash (and/c hash? hash-weak?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-weak-hash-keys
          [hash (and/c hash? hash-weak?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-weak-hash-keys
          [hash (and/c hash? hash-weak?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-weak-hash-values
          [hash (and/c hash? hash-weak?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-weak-hash-keys
          [hash (and/c hash? hash-weak?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-weak-hash-pairs
          [hash (and/c hash? hash-weak?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-weak-hash-pairs
          [hash (and/c hash? hash-weak?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-ephemeron-hash
          [hash (and/c hash? hash-ephemeron?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-ephemeron-hash
          [hash (and/c hash? hash-ephemeron?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-ephemeron-hash-keys
          [hash (and/c hash? hash-ephemeron?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-ephemeron-hash-keys
          [hash (and/c hash? hash-ephemeron?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-ephemeron-hash-values
          [hash (and/c hash? hash-ephemeron?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-ephemeron-hash-keys
          [hash (and/c hash? hash-ephemeron?)] [bad-index-v any/c])
	  sequence?]
@defproc[(in-ephemeron-hash-pairs
          [hash (and/c hash? hash-ephemeron?)])
	  sequence?]
@defproc[#:link-target? #f
         (in-ephemeron-hash-pairs
          [hash (and/c hash? hash-ephemeron?)] [bad-index-v any/c])
	  sequence?]
)]{
   Sequence constructors for specific kinds of hash tables.
   These may perform better than the analogous @racket[in-hash]
   forms.

   @history[#:added "6.4.0.6"
            #:changed "7.0.0.10" @elem{Added the optional @racket[bad-index-v] argument.}
            #:changed "8.0.0.10" @elem{Added @schemeidfont{ephemeron} variants.}]
}


@defproc[(in-directory [dir (or/c #f path-string?) #f]
                       [use-dir? ((and/c path? complete-path?) . -> . any/c)
                                 (lambda (dir-path) #t)])
         (sequence/c path?)]{
  Returns a sequence that produces all of the paths for files,
  directories, and links within @racket[dir], except for the
  contents of any directory for which @racket[use-dir?] returns
  @racket[#f]. If @racket[dir] is not
  @racket[#f], then every produced path starts with @racket[dir] as
  its prefix.  If @racket[dir] is @racket[#f], then paths in and
  relative to the current directory are produced.

  An @racket[in-directory] sequence traverses nested subdirectories
  recursively (filtered by @racket[use-dir?]).
  To generate a sequence that includes only the immediate
  content of a directory, use the result of @racket[directory-list] as
  a sequence.

  The immediate content of each directory is reported as sorted by
  @racket[path<?], and the content of a subdirectory is reported
  before subsequent paths within the directory.

  @examples[
    (eval:alts (current-directory (path-only (collection-file-path "main.rkt" "info")))
               (void))
    (eval:alts (for/list ([f (in-directory)])
                  f)
               (map string->path '("compiled"
                                   "compiled/main_rkt.dep"
                                   "compiled/main_rkt.zo"
                                   "main.rkt")))
    (eval:alts (for/list ([f (in-directory "compiled")])
                 f)
               (map string->path '("compiled/main_rkt.dep"
                                   "compiled/main_rkt.zo")))
    (eval:alts (for/list ([f (in-directory #f (lambda (p)
                                                (not (regexp-match? #rx"compiled" p))))])
                  f)
               (map string->path '("compiled" "main.rkt")))
  ]

@history[#:changed "6.0.0.1" @elem{Added @racket[use-dir?] argument.}
         #:changed "6.6.0.4" @elem{Added guarantee of sorted results.}]}


@defproc*[([(in-producer [producer procedure?])
            sequence?]
           [(in-producer [producer procedure?] [stop any/c] [arg any/c] ...)
            sequence?])]{
  Returns a sequence that contains values from sequential calls to
  @racket[producer], which would usually use some state to do its work.

  If a @racket[stop] value is not given, the sequence goes on
  infinitely, and therefore it is common to use it with a finite sequence
  or using @racket[#:break] etc.  If a @racket[stop] value is given, it
  is used to identify a value that marks the end of the sequence (and
  the @racket[stop] value is not included in the sequence);
  @racket[stop] can be a predicate that is applied to the results of
  @racket[producer], or it can be a value that is tested against the
  result of with @racket[eq?].  (The @racket[stop] argument must be a
  predicate if the stop value is itself a function or if
  @racket[producer] returns multiple values.)

  If additional @racket[arg]s are specified, they are passed to every
  call to @racket[producer].

  @examples[
    (define (counter)
      (define n 0)
      (lambda ([d 1]) (set! n (+ d n)) n))
    (for/list ([x (in-producer (counter))] [y (in-range 4)]) x)
    (for/list ([x (in-producer (counter))] #:break (= x 5)) x)
    (for/list ([x (in-producer (counter) 5)]) x)
    (for/list ([x (in-producer (counter) 5 1/2)]) x)
    (for/list ([x (in-producer read eof (open-input-string "1 2 3"))]) x)]
}

@defproc[(in-value [v any/c]) sequence?]{
  Returns a sequence that produces a single value: @racket[v].

  This form is mostly useful for @racket[let]-like bindings in forms
  such as @racket[for*/list]---but a @racket[#:do] clause form, added
  more recently, covers many of the same uses.
}

@defproc[(in-indexed [seq sequence?]) sequence?]{
  Returns a sequence where each element has two values: the value
  produced by @racket[seq], and a non-negative exact integer starting
  with @racket[0].  The elements of @racket[seq] must be
  single-valued.
  
  @(examples
    #:eval sequence-evaluator
    (for ([(ch i) (in-indexed "hello")])
      (printf "The char at position ~a is: ~a\n" i ch)))
}

@defproc[(in-sequences [seq sequence?] ...) sequence?]{
  Returns a sequence that is made of all input sequences, one after
  the other. Each @racket[seq] is @tech{initiate}d only after the
  preceding @racket[seq] is exhausted. If a single @racket[seq] is
  provided, then @racket[seq] is returned; otherwise, the elements of
  each @racket[seq] must all have the same number of values.}

@defproc[(in-cycle [seq sequence?] ...) sequence?]{
  Similar to @racket[in-sequences], but the sequences are repeated in
  an infinite cycle, where each @racket[seq] is @tech{initiate}d
  afresh in each iteration. Beware that if no @racket[seq]s are
  provided or if all @racket[seq]s become empty, then the sequence
  produced by @racket[in-cycle] never returns when an element is
  demanded---or even when the sequence is @tech{initiate}d, if all
  @racket[seq]s are initially empty.}

@defproc[(in-parallel [seq sequence?] ...) sequence?]{
  Returns a sequence where each element has as many values as the
  number of supplied @racket[seq]s; the values, in order, are the
  values of each @racket[seq].  The elements of each @racket[seq] must
  be single-valued.}

@defproc[(in-values-sequence [seq sequence?]) sequence?]{
  Returns a sequence that is like @racket[seq], but it combines
  multiple values for each element from @racket[seq] as a list of
  elements.}

@defproc[(in-values*-sequence [seq sequence?]) sequence?]{
  Returns a sequence that is like @racket[seq], but when an element of
  @racket[seq] has multiple values or a single list value, then the
  values are combined in a list. In other words,
  @racket[in-values*-sequence] is like @racket[in-values-sequence],
  except that non-list, single-valued elements are not wrapped in a
  list.
}

@defproc[(stop-before [seq sequence?] [pred (any/c . -> . any)])
         sequence?]{
  Returns a sequence that contains the elements of @racket[seq] (which
  must be single-valued), but only until the last element for which
  applying @racket[pred] to the element produces @racket[#t], after
  which the sequence ends.
}

@defproc[(stop-after [seq sequence?] [pred (any/c . -> . any)])
         sequence?]{
  Returns a sequence that contains the elements of @racket[seq] (which
  must be single-valued), but only until the element (inclusive) for
  which applying @racket[pred] to the element produces @racket[#t],
  after which the sequence ends.
}

@defproc[(make-do-sequence
          [thunk (or/c (-> (values (any/c . -> . any)
                                   (any/c . -> . any/c)
                                   any/c
                                   (or/c (any/c . -> . any/c) #f)
                                   (or/c (any/c ... . -> . any/c) #f)
                                   (or/c (any/c any/c ... . -> . any/c) #f)))
                       (-> (values (any/c . -> . any)
                                   (or/c (any/c . -> . any/c) #f)
                                   (any/c . -> . any/c)
                                   any/c
                                   (or/c (any/c . -> . any/c) #f)
                                   (or/c (any/c ... . -> . any/c) #f)
                                   (or/c (any/c any/c ... . -> . any/c) #f))))])
         sequence?]{
  Returns a sequence whose elements are generated according to @racket[thunk].

  The sequence is @tech{initiate}d when @racket[thunk] is called.
  The initiated sequence is defined in
  terms of a @defterm{position}, which is initialized to @racket[_init-pos],
  and the @defterm{element}, which may consist of multiple values.

  The @racket[thunk] procedure must return either six or seven values.
  However, use @racket[initiate-sequence] to return these multiple values,
  as opposed to listing the values directly.

  If @racket[thunk] returns six values:
  @itemize[
    @item{The first result is a @racket[_pos->element] procedure that
      takes the current position and returns the value(s) for the
      current element.}
    @item{The second result is a @racket[_next-pos] procedure that
      takes the current position and returns the next position.}
    @item{The third result is a @racket[_init-pos] value, which is the initial position.}
    @item{The fourth result is a @racket[_continue-with-pos?] function
      that takes the current position and returns a true result if the
      sequence includes the value(s) for the current position, and
      false if the sequence should end instead of including the
      value(s). Alternatively, @racket[_continue-with-pos?] can be @racket[#f] to
      indicate that the sequence should always include the current
      value(s). This function is checked on each position before
      @racket[_pos->element] is used.}
    @item{The fifth result is a @racket[_continue-with-val?] function
      that is like @racket[_continue-with-pos?], but it takes the current element
      value(s) as arguments instead of the current position.  Alternatively, @racket[_continue-with-val?]
      can be @racket[#f] to indicate that the sequence
      should always include the value(s) at the current position.}
    @item{The sixth result is a @racket[_continue-after-pos+val?]
      procedure that takes both the current position and the current
      element value(s) and determines whether the sequence ends after
      the current element is already included in the sequence.
      Alternatively, @racket[_continue-after-pos+val?] can be @racket[#f] to indicate
      that the sequence can always continue after the current
      value(s).}]

  If @racket[thunk] returns seven values, the first result is still
  the @racket[_pos->element] procedure.
  However, the second result is now an @racket[_early-next-pos]
      procedure that is described further below. Alternatively,
      @racket[_early-next-pos] can be @racket[#f], which is equivalent
      to the identity function.
  Other results' positions are shifted by one,
  so the third result is now @racket[_next-pos], and
  the fourth result is now @racket[_init-pos], etc.

  The @racket[_early-next-pos] procedure takes the current position and returns an updated position.
  This updated position is used for @racket[_next-pos] and
  @racket[_continue-after-pos+val?], but not with
  @racket[_continue-with-pos?] (which uses the original current
  position). The intent of @racket[_early-next-pos] is to support a
  sequence where the position must be incremented to avoid keeping a
  value reachable while a loop processes the sequence value, so
  @racket[_early-next-pos] is applied just after
  @racket[_pos->element]. The @racket[_continue-after-pos+val?] function
  needs to be @racket[#f] to avoid retaining values to supply to that function.

  Each of the procedures listed above is called only once per
  position.  Among the procedures @racket[_continue-with-pos?], @racket[_continue-with-val?], and @racket[_continue-after-pos+val?], as soon as one of the
  procedures returns @racket[#f], the sequence ends, and none are
  called again.  Typically, one of the functions determines the end
  condition, and @racket[#f] is used in place of the other two
  functions.

@history[#:changed "6.7.0.4" @elem{Added support for the optional second result.}]}


@defthing[prop:sequence struct-type-property?]{

  Associates a procedure to a structure type that takes an instance of
  the structure and returns a sequence.  If @racket[v] is an instance
  of a structure type with this property, then @racket[(sequence? v)]
  produces @racket[#t].

  Using a pre-existing sequence:

  @examples[
    (struct my-set (table)
      #:property prop:sequence
      (lambda (s)
        (in-hash-keys (my-set-table s))))
    (define (make-set . xs)
      (my-set (for/hash ([x (in-list xs)])
                (values x #t))))
    (for/list ([c (make-set 'celeriac 'carrot 'potato)])
      c)]

  Using @racket[make-do-sequence]:

  @let-syntax[([car (make-element-id-transformer
                     (lambda (id) #'@racketidfont{car}))])
    @examples[
      (require racket/sequence)
      (struct train (car next)
        #:property prop:sequence
        (lambda (t)
          (make-do-sequence
           (lambda ()
             (initiate-sequence
              #:pos->element train-car
              #:next-pos train-next
              #:init-pos t
              #:continue-with-pos? (lambda (t) t))))))
      (for/list ([c (train 'engine
                           (train 'boxcar
                                  (train 'caboose
                                         #f)))])
        c)]]}

@; ----------------------------------------------------------------------
@subsection{Sequence Conversion}

@defproc[(sequence->stream [seq sequence?]) stream?]{
  Converts a sequence to a @tech{stream}, which supports the
  @racket[stream-first] and @racket[stream-rest] operations. Creation
  of the stream eagerly @tech{initiates} the sequence, but the stream
  lazily draws elements from the sequence, caching each element so
  that @racket[stream-first] produces the same result each time is
  applied to a stream.

  If extracting an element from @racket[seq] involves a side-effect,
  then the effect is performed each time that either
  @racket[stream-first] or @racket[stream-rest] is first used to
  access or skip an element.

  Note that a @elemref["sequence-state"]{sequence itself can have
  state}, so multiple calls to @racket[sequence->stream] on the same
  @racket[seq] are not necessarily independent.

  @examples[
  #:eval sequence-evaluator
  (define inport (open-input-bytes (bytes 1 2 3 4 5)))
  (define strm (sequence->stream inport))
  (stream-first strm)
  (stream-first (stream-rest strm))
  (stream-first strm)

  (define strm2 (sequence->stream inport))
  (stream-first strm2)
  (stream-first (stream-rest strm2))
 ]}

@defproc[(sequence-generate [seq sequence?])
         (values (-> boolean?) (-> any))]{
  @tech{Initiates} a sequence and returns two thunks to extract
  elements from the sequence.  The first checks if more elements are
  available by reading and caching the next element (which may be multiple
  values) if none is currently cached, returning @racket[#t] if successful.
  The second retrieves the cached element if available; otherwise it reads
  the next element from the sequence directly; if no more elements are
  available, the @exnraise[exn:fail:contract].

  Note that a @elemref["sequence-state"]{sequence itself can have
  state}, so multiple calls to @racket[sequence-generate] on the same
  @racket[seq] are not necessarily independent.

  @examples[
  #:eval sequence-evaluator
  (define inport (open-input-bytes (bytes 1 2 3 4 5 6 7 8 9 10)))
  (define-values (more? get) (sequence-generate inport))
  (more?)
  (get)
  (more?)
  (sequence-ref inport 0)
  (get)
  (more?)
  (for/first ([i inport]) i)
  (get)

  (define-values (more2? get2) (sequence-generate inport))
  (more?)
  (list (get2) (get))
  (more2?)
  (more2?)
  (list (get) (get2))
  (more?)
  (more2?)
  (more?)
  (more2?)
 ]}

@defproc[(sequence-generate* [seq sequence?])
         (values (or/c list? #f)
                 (-> (values (or/c list? #f) procedure?)))]{
  Like @racket[sequence-generate], but avoids state (aside from any
  inherent in the sequence) by returning a list of values for the
  sequence's first element---or @racket[#f] if the sequence is
  empty---and a thunk to continue with the sequence; the result of the
  thunk is the same as the result of @racket[sequence-generate*], but
  for the second element of the sequence, and so on. If the thunk is
  called when the element result is @racket[#f] (indicating no further
  values in the sequence), the @exnraise[exn:fail:contract].}

@; ----------------------------------------------------------------------
@subsection[#:tag "more-sequences"]{Additional Sequence Operations}

@note-lib[racket/sequence]

@defthing[empty-sequence sequence?]{
  A sequence with no elements.}

@defproc[(sequence->list [s sequence?]) list?]{
  Returns a list whose elements are the elements of @racket[s], each
  of which must be a single value.  If @racket[s] is infinite, this
  function does not terminate.}

@defproc[(sequence-length [s sequence?])
         exact-nonnegative-integer?]{
  Returns the number of elements of @racket[s] by extracting and
  discarding all of them.  If @racket[s] is infinite, this function
  does not terminate.}

@defproc[(sequence-ref [s sequence?] [i exact-nonnegative-integer?])
         any]{
  Returns the @racket[i]th element of @racket[s] (which may be
  multiple values).}

@defproc[(sequence-tail [s sequence?] [i exact-nonnegative-integer?])
         sequence?]{
  Returns a sequence equivalent to @racket[s], except that the first
  @racket[i] elements are omitted.

  In case @tech[#:key "initiate"]{initiating} @racket[s] involves a
  side effect, the sequence @racket[s] is not @tech{initiate}d until
  the resulting sequence is @tech{initiate}d, at which point the first
  @racket[i] elements are extracted from the sequence.
}

@defproc[(sequence-append [s sequence?] ...)
         sequence?]{
  Returns a sequence that contains all elements of each sequence in
  the order they appear in the original sequences.  The new sequence
  is constructed lazily.

  If all given @racket[s]s are @tech{streams}, the result is also a
  @tech{stream}.
}

@defproc[(sequence-map [f procedure?]
                       [s sequence?])
         sequence?]{
  Returns a sequence that contains @racket[f] applied to each element
  of @racket[s].  The new sequence is constructed lazily.

  If @racket[s] is a @tech{stream}, then the result is also a
  @tech{stream}.
}

@defproc[(sequence-andmap [f (-> any/c ... boolean?)]
                          [s sequence?])
         boolean?]{
  Returns @racket[#t] if @racket[f] returns a true result on every
  element of @racket[s].  If @racket[s] is infinite and @racket[f]
  never returns a false result, this function does not terminate.
}

@defproc[(sequence-ormap [f (-> any/c ... boolean?)]
                         [s sequence?])
         boolean?]{
  Returns @racket[#t] if @racket[f] returns a true result on some
  element of @racket[s].  If @racket[s] is infinite and @racket[f]
  never returns a true result, this function does not terminate.
}

@defproc[(sequence-for-each [f (-> any/c ... any)]
                            [s sequence?])
         void?]{
  Applies @racket[f] to each element of @racket[s].  If @racket[s] is
  infinite, this function does not terminate.
}

@defproc[(sequence-fold [f (-> any/c any/c ... any/c)]
                        [i any/c]
                        [s sequence?])
         any/c]{
  Folds @racket[f] over each element of @racket[s] with @racket[i] as
  the initial accumulator.  If @racket[s] is infinite, this function
  does not terminate. The @racket[f] function takes the accumulator as
  its first argument and the next sequence element as its second.
}

@defproc[(sequence-count [f procedure?] [s sequence?])
         exact-nonnegative-integer?]{
  Returns the number of elements in @racket[s] for which @racket[f]
  returns a true result.  If @racket[s] is infinite, this function
  does not terminate.
}

@defproc[(sequence-filter [f (-> any/c ... boolean?)]
                          [s sequence?])
         sequence?]{
  Returns a sequence whose elements are the elements of @racket[s] for
  which @racket[f] returns a true result.  Although the new sequence
  is constructed lazily, if @racket[s] has an infinite number of
  elements where @racket[f] returns a false result in between two
  elements where @racket[f] returns a true result, then operations on
  this sequence will not terminate during the infinite sub-sequence.

  If @racket[s] is a @tech{stream}, then the result is also a
  @tech{stream}.
}

@defproc[(sequence-add-between [s sequence?] [e any/c])
         sequence?]{
  Returns a sequence whose elements are the elements of @racket[s],
  but with @racket[e] between each pair of elements in @racket[s].
  The new sequence is constructed lazily.

  If @racket[s] is a @tech{stream}, then the result is also a
  @tech{stream}.

  @examples[#:eval sequence-evaluator
    (let* ([all-reds (in-cycle '("red"))]
           [red-and-blues (sequence-add-between all-reds "blue")])
      (for/list ([n (in-range 10)]
                 [elt red-and-blues])
        elt))

    (for ([text (sequence-add-between '("veni" "vidi" "duci") ", ")])
      (display text))
    ]
}

@defproc[(sequence/c [#:min-count min-count (or/c #f exact-nonnegative-integer?) #f]
                     [elem/c contract?] ...)
         contract?]{

Wraps a @tech{sequence},
obligating it to produce elements with as many values as there are @racket[elem/c] contracts,
and obligating each value to satisfy the corresponding @racket[elem/c].  The
result is not guaranteed to be the same kind of sequence as the original value;
for instance, a wrapped list is not guaranteed to satisfy @racket[list?].

If @racket[min-count] is a number, the stream is required to have at least that many elements in it.

@examples[
#:eval sequence-evaluator
(define/contract predicates
  (sequence/c (-> any/c boolean?))
  (in-list (list integer?
                 string->symbol)))
(eval:error
 (for ([P predicates])
   (printf "~s\n" (P "cat"))))
(define/contract numbers&strings
  (sequence/c number? string?)
  (in-dict (list (cons 1 "one")
                 (cons 2 "two")
                 (cons 3 'three))))
(eval:error
 (for ([(N S) numbers&strings])
   (printf "~s: ~a\n" N S)))
(define/contract a-sequence
  (sequence/c #:min-count 2 char?)
  "x")
(eval:error
 (for ([x a-sequence]
       [i (in-naturals)])
   (printf "~a is ~a\n" i x)))
]

}

@subsubsection{Additional Sequence Constructors and Functions}

@defproc[(in-syntax [stx syntax?]) sequence?]{
  Produces a sequence whose elements are the successive subparts of
  @racket[stx].
  Equivalent to @racket[(stx->list lst)].
  @speed[in-syntax "syntax"]

@examples[#:eval sequence-evaluator
(for/list ([x (in-syntax #'(1 2 3))])
  x)]

@history[#:added "6.3"]}

@defproc[(in-slice [length exact-positive-integer?] [seq sequence?])
         sequence?]{
  Returns a sequence whose elements are lists with the first @racket[length]
  elements of @racket[seq], then the next @racket[length] and so on.

  @examples[#:eval sequence-evaluator
  (for/list ([e (in-slice 3 (in-range 8))]) e)
  ]
  @history[#:added "6.3"]
}

@defproc[(initiate-sequence
          [#:pos->element pos->element (any/c . -> . any)]
          [#:early-next-pos early-next-pos (or/c (any/c . -> . any) #f) #f]
          [#:next-pos next-pos (any/c . -> . any/c)]
          [#:init-pos init-pos any/c]
          [#:continue-with-pos? continue-with-pos? (or/c (any/c . -> . any/c) #f) #f]
          [#:continue-with-val? continue-with-val? (or/c (any/c ... . -> . any/c) #f) #f]
          [#:continue-after-pos+val? continue-after-pos+val? (or/c (any/c any/c ... . -> . any/c) #f) #f])
         (values (any/c . -> . any)
                 (or/c (any/c . -> . any) #f)
                 (any/c . -> . any/c)
                 any/c
                 (or/c (any/c . -> . any/c) #f)
                 (or/c (any/c ... . -> . any/c) #f)
                 (or/c (any/c any/c ... . -> . any/c) #f))]{
  Returns values suitable for the thunk argument in @racket[make-do-sequence].
  See @racket[make-do-sequence] for the meaning of each argument.

  @examples[#:eval sequence-evaluator
    (define (in-alt-list xs)
      (make-do-sequence
       (λ ()
         (initiate-sequence
          #:pos->element car
          #:next-pos (λ (xs) (cdr (cdr xs)))
          #:init-pos xs
          #:continue-with-pos? pair?
          #:continue-after-pos+val? (λ (xs _) (pair? (cdr xs)))))))
    (sequence->list (in-alt-list '(1 2 3 4 5 6)))
    (sequence->list (in-alt-list '(1 2 3 4 5 6 7)))
  ]
  @history[#:added "8.10.0.5"]
}


@; ======================================================================
@section[#:tag "streams"]{Streams}

A @deftech{stream} is a kind of @tech{sequence} that supports
functional iteration via @racket[stream-first] and
@racket[stream-rest].  The @racket[stream-cons] form constructs a lazy
stream, but plain lists can be used as streams, and functions such as
@racket[in-range] and @racket[in-naturals] also create streams.

@note-lib[racket/stream]

@defproc[(stream? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] can be used as a @tech{stream},
  @racket[#f] otherwise.
}

@defproc[(stream-empty? [s stream?]) boolean?]{
  Returns @racket[#t] if @racket[s] has no elements, @racket[#f]
  otherwise.
}

@defproc[(stream-first [s (and/c stream? (not/c stream-empty?))]) any]{
  Returns the value(s) of the first element in @racket[s].
}

@defproc[(stream-rest [s (and/c stream? (not/c stream-empty?))]) stream?]{
  Returns a stream that is equivalent to @racket[s] without its first
  element.
}

@defform*[[(stream-cons first-expr rest-expr)
           (stream-cons #:eager first-expr rest-expr)
           (stream-cons first-expr #:eager rest-expr)
           (stream-cons #:eager first-expr #:eager rest-expr)]]{

  Produces a stream whose first element is determined by
  @racket[first-expr] and whose rest is determined by
  @racket[rest-expr].

  If @racket[first-expr] is not preceded by @racket[#:eager], then
  @racket[first-expr] is not evaluated immediately. Instead,
  @racket[stream-first] on the result stream forces the evaluation of
  @racket[first-expr] (once) to produce the first element of the
  stream. If evaluating @racket[first-expr] raises an exception or
  tries to force itself, then an @exnraise[exn:fail:contract], and
  future attempts to force evaluation will trigger another exception.

  If @racket[rest-expr] is not preceded by @racket[#:eager], then
  @racket[rest-expr] is not evaluated immediately. Instead,
  @racket[stream-rest] on the result stream produces another stream
  that is like the one produced by @racket[(stream-lazy rest-expr)].

  The first element of the stream as produced by @racket[first-expr]
  can be multiple values. The @racket[rest-expr] must produce a stream
  when it is evaluated, otherwise the @exnraise[exn:fail:contract?].

  @history[#:changed "8.0.0.12" @elem{Added @racket[#:eager] options.}
           #:changed "8.8.0.7" @elem{Changed to allow multiple values.}]}

@defform*[[(stream-lazy stream-expr)
           (stream-lazy #:who who-expr stream-expr)]]{

 Similar to @racket[(delay stream-expr)], but the result is a stream
 instead of a @tech{promise}, and @racket[stream-expr] must produce a
 stream when it is eventually forced. The stream produced by
 @racket[stream-lazy] has the same content as the stream produced by
 @racket[stream-expr]; that is, operations like @racket[stream-first]
 on the result stream will force @racket[stream-expr] and retry on its
 result.

 If evaluating @racket[stream-expr] raises an exception or tries to
 force itself, then an @exnraise[exn:fail:contract], and future
 attempts to force evaluation will trigger another exception.

 If @racket[who-expr] is provided, it is evaluated when constructing
 the delayed stream. If @racket[stream-expr] later produces a value
 that is not a stream, and if @racket[who-expr] produced a symbol
 value, then the symbol is used for the error message.

 @history[#:added "8.0.0.12"]}

@defproc[(stream-force [s stream?]) stream?]{

 Forces the evaluation of a delayed stream from @racket[stream-lazy],
 from the @racket[stream-rest] of a @racket[stream-cons], etc.,
 returning the forced stream. If @racket[s] is not a delayed stream,
 then @racket[s] is returned.

 Normally, @racket[stream-force] is not needed, because operations
 like @racket[stream-first], @racket[stream-rest], and
 @racket[stream-empty?] force a delayed stream as needed. In rare
 cases, @racket[stream-force] can be useful to reveal the underlying
 implementation of a stream (e.g., a stream that is an instance of a
 structure type that has the @racket[prop:stream] property).

 @history[#:added "8.0.0.12"]}

@defform[#:literals (values)
         (stream elem-expr ...)
         #:grammar ([elem-expr (values single-expr ...)
                               single-expr])]{
  A shorthand for nested @racket[stream-cons]es ending with
  @racket[empty-stream]. As a match pattern, @racket[stream]
  matches a stream with as many elements as @racket[elem-expr]s,
  and each element must match the corresponding @racket[elem-expr] pattern.
  The pattern @racket[elem-expr] can be @racket[(values single-expr ...)], which matches against
  multiple valued elements in the stream.

  @history[#:changed "8.8.0.7" @elem{Changed to allow multiple values.}]
}

@defform[(stream* elem-expr ... tail-expr)]{
  A shorthand for nested @racket[stream-cons]es, but the @racket[tail-expr]
  must produce a stream when it is forced, and that stream is used as the rest of the stream instead of
  @racket[empty-stream]. Similar to @racket[list*] but for streams.
  As a match pattern, @racket[stream*] is similar to a @racket[stream] pattern,
  but the @racket[tail-expr] pattern matches the ``rest'' of the stream after the last @racket[elem-expr].

@history[#:added "6.3"
         #:changed "8.0.0.12" @elem{Changed to delay @racket[rest-expr] even
                                    if zero @racket[expr]s are provided.}
         #:changed "8.8.0.7" @elem{Changed to allow multiple values.}]
}

@defproc[(in-stream [s stream?]) sequence?]{
  Returns a sequence that is equivalent to @racket[s].
  @speed[in-stream "streams"]
  @for-element-reachability["stream"]

@history[#:changed "6.7.0.4" @elem{Improved element-reachability guarantee for streams in @racket[for].}]}

@defthing[empty-stream stream?]{
  A stream with no elements.
}

@defproc[(stream->list [s stream?]) list?]{
  Returns a list whose elements are the elements of @racket[s], each
  of which must be a single value.  If @racket[s] is infinite, this
  function does not terminate.
}

@defproc[(stream-length [s stream?])
         exact-nonnegative-integer?]{
  Returns the number of elements of @racket[s].  If @racket[s] is
  infinite, this function does not terminate.

  In the case of lazy streams, this function forces evaluation only of
  the sub-streams, and not the stream's elements.
}

@defproc[(stream-ref [s stream?] [i exact-nonnegative-integer?])
         any]{
  Returns the @racket[i]th element of @racket[s] (which may be
  multiple values).
}

@defproc[(stream-tail [s stream?] [i exact-nonnegative-integer?])
         stream?]{
  Returns a stream equivalent to @racket[s], except that the first
  @racket[i] elements are omitted.

  In case extracting elements from @racket[s] involves a side effect,
  they will not be extracted until the first element is extracted from
  the resulting stream.
}

@defproc[(stream-take [s stream?] [i exact-nonnegative-integer?])
         stream?]{
  Returns a stream of the first @racket[i] elements of @racket[s].
}

@defproc[(stream-append [s stream?] ...)
         stream?]{
  Returns a stream that contains all elements of each stream in the
  order they appear in the original streams.  The new stream is
  constructed lazily, while the last given stream is used in the tail
  of the result.
}

@defproc[(stream-map [f procedure?]
                     [s stream?])
         stream?]{
  Returns a stream that contains @racket[f] applied to each element of
  @racket[s].  The new stream is constructed lazily.
}

@defproc[(stream-andmap [f (-> any/c ... boolean?)]
                        [s stream?])
         boolean?]{
  Returns @racket[#t] if @racket[f] returns a true result on every
  element of @racket[s].  If @racket[s] is infinite and @racket[f]
  never returns a false result, this function does not terminate.
}

@defproc[(stream-ormap [f (-> any/c ... boolean?)]
                       [s stream?])
         boolean?]{
  Returns @racket[#t] if @racket[f] returns a true result on some
  element of @racket[s].  If @racket[s] is infinite and @racket[f]
  never returns a true result, this function does not terminate.
}

@defproc[(stream-for-each [f (-> any/c ... any)]
                          [s stream?])
         void?]{
  Applies @racket[f] to each element of @racket[s].  If @racket[s] is
  infinite, this function does not terminate.
}

@defproc[(stream-fold [f (-> any/c any/c ... any/c)]
                      [i any/c]
                      [s stream?])
         any/c]{
  Folds @racket[f] over each element of @racket[s] with @racket[i] as
  the initial accumulator.  If @racket[s] is infinite, this function
  does not terminate. The @racket[f] function takes the accumulator as
  its first argument and the next stream element as its second.
}

@defproc[(stream-count [f procedure?] [s stream?])
         exact-nonnegative-integer?]{
  Returns the number of elements in @racket[s] for which @racket[f]
  returns a true result.  If @racket[s] is infinite, this function
  does not terminate.
}

@defproc[(stream-filter [f (-> any/c ... boolean?)]
                          [s stream?])
         stream?]{
  Returns a stream whose elements are the elements of @racket[s] for
  which @racket[f] returns a true result.  Although the new stream is
  constructed lazily, if @racket[s] has an infinite number of elements
  where @racket[f] returns a false result, then operations on this
  stream will not terminate during the infinite sub-stream.
}

@defproc[(stream-add-between [s stream?] [e any/c])
         stream?]{
  Returns a stream whose elements are the elements of @racket[s], but
  with @racket[e] between each pair of elements in @racket[s].  The
  new stream is constructed lazily.
}

@deftogether[(@defform[(for/stream (for-clause ...) body-or-break ... body)]
              @defform[(for*/stream (for-clause ...) body-or-break ... body)])]{
  Iterates like @racket[for/list] and @racket[for*/list], respectively, but the
  results are lazily collected into a @tech{stream} instead of a list.

  Unlike most @racket[for] forms, these forms are evaluated lazily, so each
  @racket[body] will not be evaluated until the resulting stream is forced. This
  allows @racket[for/stream] and @racket[for*/stream] to iterate over infinite
  sequences, unlike their finite counterparts.

  @examples[#:eval sequence-evaluator
    (for/stream ([i '(1 2 3)]) (* i i))
    (stream->list (for/stream ([i '(1 2 3)]) (* i i)))
    (stream-ref (for/stream ([i '(1 2 3)]) (displayln i) (* i i)) 1)
    (stream-ref (for/stream ([i (in-naturals)]) (* i i)) 25)
    (stream-ref (for/stream ([i (in-naturals)]) (values i (add1 i))) 10)
  ]

  @history[#:added "6.3.0.9"
           #:changed "8.8.0.7" @elem{Changed to allow multiple values.}]
}

@defthing[gen:stream any/c]{
  Associates three methods to a structure type to implement the
  @tech{generic interface} (see @secref["struct-generics"]) for
  streams.

  To supply method implementations, the @racket[#:methods] keyword
  should be used in a structure type definition. The following three
  methods must be implemented:

  @itemize[
    @item{@racket[_stream-empty?] : accepts one argument}
    @item{@racket[_stream-first] : accepts one argument}
    @item{@racket[_stream-rest] : accepts one argument}
  ]

  @examples[#:eval sequence-evaluator
    (struct list-stream (v)
      #:methods gen:stream
      [(define (stream-empty? stream)
         (empty? (list-stream-v stream)))
       (define (stream-first stream)
         (first (list-stream-v stream)))
       (define (stream-rest stream)
         (list-stream (rest (list-stream-v stream))))])

    (define l1 (list-stream '(1 2)))
    (stream? l1)
    (stream-first l1)
  ]

  @history[#:changed "8.7.0.5"
           @elem{Added a check so that omitting any of
                 @racket[_stream-empty?], @racket[_stream-first], and @racket[_stream-rest]
                 is now a syntax error.}]

}

@defthing[prop:stream struct-type-property?]{
  A structure type property used to define custom
  extensions to the stream API. Using the @racket[prop:stream] property
  is discouraged; use the @racket[gen:stream] @tech{generic interface}
  instead. Accepts a vector of three procedures taking the same arguments
  as the methods in @racket[gen:stream].
}

@defproc[(stream/c [c contract?]) contract?]{
Returns a contract that recognizes streams. All elements of the stream must match
@racket[c].

If the @racket[c] argument is a flat contract or a chaperone contract, then the
result will be a chaperone contract. Otherwise, the result will be an
impersonator contract.

When an @racket[stream/c] contract is applied to a stream, the result is not
@racket[eq?] to the input. The result will be either a @tech{chaperone} or
@tech{impersonator} of the input depending on the type of contract.

Contracts on streams are evaluated lazily by necessity (since streams may be
infinite). Contract violations will not be raised until the value in violation
is retrieved from the stream. As an exception to this rule, streams that are
lists are checked immediately, as if @racket[c] had been used with
@racket[listof].

If a contract is applied to a stream, and that stream is subsequently used as
the tail of another stream (as the second parameter to @racket[stream-cons]),
the new elements will not be checked with the contract, but the tail's elements
will still be enforced.

@history[#:added "6.1.1.8"]}

@close-eval[sequence-evaluator]

@; ======================================================================
@section{Generators}

A @deftech{generator} is a procedure that returns a sequence of
values, incrementing the sequence each time that the generator is
called. In particular, the @racket[generator] form implements a
generator by evaluating a body that calls @racket[yield] to return
values from the generator.

@defmodule[racket/generator]

@(define generator-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/generator))
     the-eval))

@defproc[(generator? [v any/c]) boolean?]{
  Return @racket[#t] if @racket[v] is a @tech{generator},
  @racket[#f] otherwise.
}

@defform/subs[(generator formals body ...+)
              ([formals (id ...)
                        (id ...+ . rest-id)
                        rest-id])]{
  Creates a @tech{generator}, where @racket[formals] specify the arguments.
  Keyword and optional arguments are not supported. This is the same as the
  @racket[formals] of a single @racket[case-lambda] clause.

  For the first call to a generator, the arguments are bound to the
  @racket[formals] and evaluation of @racket[body] starts. During the
  @tech{dynamic extent} of @racket[body], the generator can return
  immediately using the @racket[yield] function. A second call to the
  generator resumes at the @racket[yield] call, producing the
  arguments of the second call as the results of the @racket[yield],
  and so on. The eventual results of @racket[body] are supplied to an
  implicit final @racket[yield]; after that final @racket[yield],
  calling the generator again returns the same values, but all such
  calls must provide 0 arguments to the generator.

  @examples[#:eval generator-eval
    (define g (generator ()
                (let loop ([x '(a b c)])
                  (if (null? x)
                      0
                      (begin
                        (yield (car x))
                        (loop (cdr x)))))))
    (g)
    (g)
    (g)
    (g)
    (g)]}

@defproc[(yield [v any/c] ...) any]{
  Returns @racket[v]s from a generator, saving the point of execution
  inside a generator (i.e., within the @tech{dynamic extent} of a
  @racket[generator] body) to be resumed by the next call to the
  generator. The results of @racket[yield] are the arguments that are
  provided to the next call of the generator.

  When not in the @tech{dynamic extent} of a @racket[generator],
  @racket[infinite-generator], or @racket[in-generator] body,
  @racket[yield] raises @racket[exn:fail:contract].

  @examples[#:eval generator-eval
    (define my-generator (generator () (yield 1) (yield 2 3 4)))
    (my-generator)
    (my-generator)]

  @examples[#:eval generator-eval
    (define pass-values-generator
      (generator ()
        (let* ([from-user (yield 2)]
               [from-user-again (yield (add1 from-user))])
          (yield from-user-again))))

    (pass-values-generator)
    (pass-values-generator 5)
    (pass-values-generator 12)]}

@defform[(infinite-generator body ...+)]{
  Like @racket[generator], but repeats evaluation of the
  @racket[body]s when the last @racket[body] completes without
  implicitly @racket[yield]ing.

  @examples[#:eval generator-eval
    (define welcome
      (infinite-generator
        (yield 'hello)
        (yield 'goodbye)))
    (welcome)
    (welcome)
    (welcome)
    (welcome)]}

@defform/subs[(in-generator maybe-arity body ...+)
              ([maybe-arity code:blank
                            (code:line #:arity arity-k)])]{
  Produces a @tech{sequence} that encapsulates the @tech{generator}
  formed by @racket[(generator () body ...)]. The values produced by
  the generator form the elements of the sequence, except for the last
  value produced by the generator (i.e., the values produced by
  returning).

  @examples[#:eval generator-eval
    (for/list ([i (in-generator
                    (let loop ([x '(a b c)])
                      (when (not (null? x))
                        (yield (car x))
                        (loop (cdr x)))))])
      i)]

  If @racket[in-generator] is used immediately with a @racket[for] (or
  @racket[for/list], etc.) binding's right-hand side, then its result
  arity (i.e., the number of values in each element of the sequence)
  can be inferred. Otherwise, if the generator produces multiple
  values for each element, its arity should be declared with an
  @racket[#:arity arity-k] clause; the @racket[arity-k] must be a
  literal, exact, non-negative integer.

  @examples[#:eval generator-eval
    (eval:error
     (let ([g (in-generator
               (let loop ([n 3])
                 (unless (zero? n) (yield n (add1 n)) (loop (sub1 n)))))])
       (let-values ([(not-empty? next) (sequence-generate g)])
         (let loop () (when (not-empty?) (next) (loop))) 'done)))
    (let ([g (in-generator #:arity 2
              (let loop ([n 3])
                (unless (zero? n) (yield n (add1 n)) (loop (sub1 n)))))])
      (let-values ([(not-empty? next) (sequence-generate g)])
        (let loop () (when (not-empty?) (next) (loop))) 'done))]

  To use an existing generator as a sequence, use @racket[in-producer]
  with a stop-value known for the generator:

  @examples[#:label #f #:eval generator-eval
    (define abc-generator (generator ()
                           (for ([x '(a b c)])
                              (yield x))))
    (for/list ([i (in-producer abc-generator (void))])
      i)
    (define my-stop-value (gensym))
    (define my-generator (generator ()
                           (let loop ([x (list 'a (void) 'c)])
                             (if (null? x)
                                 my-stop-value
                                 (begin
                                   (yield (car x))
                                   (loop (cdr x)))))))
    (for/list ([i (in-producer my-generator my-stop-value)])
      i)]
}


@defproc[(generator-state [g generator?]) symbol?]{
  Returns a symbol that describes the state of the generator.

  @itemize[
    @item{@racket['fresh] --- The generator has been freshly created and
          has not been called yet.}
    @item{@racket['suspended] --- Control within the generator has been
          suspended due to a call to @racket[yield].  The generator can
          be called.}
    @item{@racket['running] --- The generator is currently executing.}
    @item{@racket['done] --- The generator has executed its entire
          body and will continue to produce the same result as from
          the last call.}]

  @examples[#:eval generator-eval
    (define my-generator (generator () (yield 1) (yield 2)))
    (generator-state my-generator)
    (my-generator)
    (generator-state my-generator)
    (my-generator)
    (generator-state my-generator)
    (my-generator)
    (generator-state my-generator)

    (define introspective-generator (generator () ((yield 1))))
    (introspective-generator)
    (introspective-generator
     (lambda () (generator-state introspective-generator)))
    (generator-state introspective-generator)
    (introspective-generator)]}

@defproc[(sequence->generator [s sequence?]) (-> any)]{
  Converts a @tech{sequence} to a @tech{generator}. The generator
  returns the next element of the sequence each time the generator is
  invoked, where each element of the sequence must be a single
  value. When the sequence ends, the generator returns @|void-const|
  as its final result.}

@defproc[(sequence->repeated-generator [s sequence?]) (-> any)]{
  Like @racket[sequence->generator], but when @racket[s] has no
  further values, the generator starts the sequence again (so that the
  generator never stops producing values).}

@close-eval[generator-eval]
