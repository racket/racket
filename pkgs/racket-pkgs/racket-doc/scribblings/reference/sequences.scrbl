#lang scribble/doc
@(require "mz.rkt" scribble/scheme
          (for-syntax racket/base)
          (for-label racket/generator
                     racket/generic
                     compatibility/mlist))

@(define (info-on-seq where what)
   @margin-note{See @secref[where] for information on using @|what| as
                sequences.})

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
     (evaluator '(require racket/generic racket/list racket/stream racket/sequence))
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
to be read from the port. A sequence's state may either span all uses
of the sequence, as for a port, or it may be confined to each distinct
time that a sequence is @deftech{initiate}d by a @racket[for] form,
@racket[sequence->stream], @racket[sequence-generate], or
@racket[sequence-generate*]. Concretely, the thunk passed to
@racket[make-do-sequence] is called to @tech{initiate} the sequence
each time the sequence is used.

Individual elements of a sequence typically correspond to single
values, but an element may also correspond to multiple values.  For
example, a hash table generates two values---a key and its value---for
each element in the sequence.

@; ----------------------------------------------------------------------
@subsection{Sequence Predicate and Constructors}

@defproc[(sequence? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] can be used as a @tech{sequence},
  @racket[#f] otherwise.

@interaction[#:eval sequence-evaluator
  (sequence? 42)
  (sequence? '(a b c))
  (sequence? "word")
  (sequence? #\x)]}

@defproc*[([(in-range [end number?]) stream?]
           [(in-range [start number?] [end number?] [step number? 1]) stream?])]{
  Returns a sequence (that is also a @tech{stream}) whose elements are
  numbers.  The single-argument case @racket[(in-range end)] is
  equivalent to @racket[(in-range 0 end 1)].  The first number in the
  sequence is @racket[start], and each successive element is generated
  by adding @racket[step] to the previous element.  The sequence stops
  before an element that would be greater or equal to @racket[end] if
  @racket[step] is non-negative, or less or equal to @racket[end] if
  @racket[step] is negative.  @speed[in-range "number"]

  Example: gaussian sum
  @interaction[#:eval sequence-evaluator
    (for/sum ([x (in-range 10)]) x)]

  Example: sum of even numbers
  @interaction[#:eval sequence-evaluator
    (for/sum ([x (in-range 0 100 2)]) x)]
}


@defproc[(in-naturals [start exact-nonnegative-integer? 0]) stream?]{
  Returns an infinite sequence (that is also a @tech{stream}) of exact
  integers starting with @racket[start], where each element is one
  more than the preceding element.  @speed[in-naturals "integer"]

  @interaction[#:eval sequence-evaluator
    (for/list ([k (in-naturals)]
               [x (in-range 10)])
      (list k x))]
}


@defproc[(in-list [lst list?]) stream?]{
  Returns a sequence (that is also a @tech{stream}) that is equivalent
  to using @racket[lst] directly as a sequence.
  @info-on-seq["pairs" "lists"]
  @speed[in-list "list"]

  @interaction[#:eval sequence-evaluator
    (for/list ([x (in-list '(3 1 4))])
      `(,x ,(* x x)))]
}

@defproc[(in-mlist [mlst mlist?]) sequence?]{
  Returns a sequence equivalent to @racket[mlst].
  @info-on-seq["mpairs" "mutable lists"]
  @speed[in-mlist "mutable list"]

  @interaction[#:eval sequence-evaluator
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

  If @racket[start] is not a valid index, or @racket[stop] is not in
  [-1, @racket[(vector-length vec)]] then the
  @exnraise[exn:fail:contract].  If @racket[start] is less than
  @racket[stop] and @racket[step] is negative, then the
  @exnraise[exn:fail:contract:mismatch].  Similarly, if @racket[start]
  is more than @racket[stop] and @racket[step] is positive, then the
  @exnraise[exn:fail:contract:mismatch].

  @speed[in-vector "vector"]

  @interaction[#:eval sequence-evaluator
    (define (histogram vector-of-words)
      (define a-hash (hash))
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

  @interaction[#:eval sequence-evaluator
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

  @interaction[#:eval sequence-evaluator
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

@defproc[(in-hash [hash hash?]) sequence?]{
  Returns a sequence equivalent to @racket[hash].

  @examples[
    (define table (hash 'a 1 'b 2))
    (for ([(key value) (in-hash table)])
      (printf "key: ~a value: ~a\n" key value))]

  @info-on-seq["hashtables" "hash tables"]
}

@defproc[(in-hash-keys [hash hash?]) sequence?]{
  Returns a sequence whose elements are the keys of @racket[hash].

  @examples[
    (define table (hash 'a 1 'b 2))
    (for ([key (in-hash-keys table)])
      (printf "key: ~a\n" key))]
}

@defproc[(in-hash-values [hash hash?]) sequence?]{
  Returns a sequence whose elements are the values of @racket[hash].

  @examples[
    (define table (hash 'a 1 'b 2))
    (for ([value (in-hash-values table)])
      (printf "value: ~a\n" value))]
}

@defproc[(in-hash-pairs [hash hash?]) sequence?]{
  Returns a sequence whose elements are pairs, each containing a key
  and its value from @racket[hash] (as opposed to using @racket[hash]
  directly as a sequence to get the key and value as separate values
  for each element).

  @examples[
    (define table (hash 'a 1 'b 2))
    (for ([key+value (in-hash-pairs table)])
      (printf "key and value: ~a\n" key+value))]
}

@defproc[(in-directory [dir (or/c #f path-string?) #f]) sequence?]{
  Returns a sequence that produces all of the paths for files,
  directories, and links within @racket[dir].  If @racket[dir] is not
  @racket[#f], then every produced path starts with @racket[dir] as
  its prefix.  If @racket[dir] is @racket[#f], then paths in and
  relative to the current directory are produced.

  An @racket[in-directory] sequence traverses nested subdirectories
  recursively. To generate a sequence that includes only the immediate
  content of a directory, use the result of @racket[directory-list] as
  a sequence.
}

@defproc*[([(in-producer [producer procedure?])
            sequence?]
           [(in-producer [producer procedure?] [stop any/c] [arg any/c] ...)
            sequence?])]{
  Returns a sequence that contains values from sequential calls to
  @racket[producer], which would usually use some state to do its work.

  If a @racket[stop] value is not given, the sequence goes on
  infinitely, and therefore it common to use it with a finite sequence
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
  Returns a sequence that produces a single value: @racket[v].  This
  form is mostly useful for @racket[let]-like bindings in forms such
  as @racket[for*/list].
}

@defproc[(in-indexed [seq sequence?]) sequence?]{
  Returns a sequence where each element has two values: the value
  produced by @racket[seq], and a non-negative exact integer starting
  with @racket[0].  The elements of @racket[seq] must be
  single-valued.
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
          [thunk (-> (values (any/c . -> . any)
                             (any/c . -> . any/c)
                             any/c
                             (or/c (any/c . -> . any/c) #f)
                             (or/c (() () #:rest list? . ->* . any/c) #f)
                             (or/c ((any/c) () #:rest list? . ->* . any/c) #f)))])
         sequence?]{
  Returns a sequence whose elements are generated by the procedures
  and initial value returned by the thunk, which is called to
  @tech{initiate} the sequence.  The initiated sequence is defined in
  terms of a @defterm{position}, which is initialized to the third
  result of the thunk, and the @defterm{element}, which may consist of
  multiple values.

  The @racket[thunk] results define the generated elements as follows:
  @itemize[
    @item{The first result is a @racket[_pos->element] procedure that
      takes the current position and returns the value(s) for the
      current element.}
    @item{The second result is a @racket[_next-pos] procedure that
      takes the current position and returns the next position.}
    @item{The third result is the initial position.}
    @item{The fourth result is a @racket[_continue-with-pos?] function
      that takes the current position and returns a true result if the
      sequence includes the value(s) for the current position, and
      false if the sequence should end instead of including the
      value(s). Alternatively, the fourth result can be @racket[#f] to
      indicate that the sequence should always include the current
      value(s).}
    @item{The fifth result is a @racket[_continue-with-val?] function
      that is like the fourth result, but it takes the current element
      value(s) instead of the current position.  Alternatively, the
      fifth result can be @racket[#f] to indicate that the sequence
      should always include the value(s) at the current position.}
    @item{The sixth result is a @racket[_continue-after-pos+val?]
      procedure that takes both the current position and the current
      element value(s) and determines whether the sequence ends after
      the current element is already included in the sequence.
      Alternatively, the sixth result can be @racket[#f] to indicate
      that the sequence can always continue after the current
      value(s).}]

  Each of the procedures listed above is called only once per
  position.  Among the last three procedures, as soon as one of the
  procedures returns @racket[#f], the sequence ends, and none are
  called again.  Typically, one of the functions determines the end
  condition, and @racket[#f] is used in place of the other two
  functions.
}

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
      (define-struct train (car next)
        #:property prop:sequence
        (lambda (t)
          (make-do-sequence
           (lambda ()
             (values train-car train-next t
                     (lambda (t) t)
                     (lambda (v) #t)
                     (lambda (t v) #t))))))
      (for/list ([c (make-train 'engine
                                (make-train 'boxcar
                                            (make-train 'caboose
                                                        #f)))])
        c)]]}

@; ----------------------------------------------------------------------
@subsection{Sequence Conversion}

@defproc[(sequence->stream [seq sequence?]) stream?]{
  Coverts a sequence to a @tech{stream}, which supports the
  @racket[stream-first] and @racket[stream-rest] operations. Creation
  of the stream eagerly @tech{initiates} the sequence, but the stream
  lazily draws elements from the sequence, caching each element so
  that @racket[stream-first] produces the same result each time is
  applied to a stream.

  If extracting an element from @racket[seq] involves a side-effect,
  then the effect is performed each time that either
  @racket[stream-first] or @racket[stream-rest] is first used to
  access or skip an element.
}

@defproc[(sequence-generate [seq sequence?])
         (values (-> boolean?) (-> any))]{
  @tech{Initiates} a sequence and returns two thunks to extract
  elements from the sequence.  The first returns @racket[#t] if more
  values are available for the sequence.  The second returns the next
  element (which may be multiple values) from the sequence; if no more
  elements are available, the @exnraise[exn:fail:contract].
}

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
@subsection[#:tag "more-sequences"]{Sequence Combinations}

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
  Returns @racket[#f] if @racket[s] has no elements, @racket[#f]
  otherwise.
}

@defproc[(stream-first [s (and/c stream? (not/c stream-empty?))]) any]{
  Returns the value(s) of the first element in @racket[s].
}

@defproc[(stream-rest [s (and/c stream? (not/c stream-empty?))]) stream?]{
  Returns a stream that is equivalent to @racket[s] without its first
  element.
}

@defform[(stream-cons first-expr rest-expr)]{

  Produces a lazy stream for which @racket[stream-first] forces the
  evaluation of @racket[first-expr] to produce the first element of
  the stream, and @racket[stream-rest] forces the evaluation of
  @racket[rest-expr] to produce a stream for the rest of the returned
  stream.

  The first element of the stream as produced by @racket[first-expr]
  must be a single value. The @racket[rest-expr] must produce a stream
  when it is evaluated, otherwise the @exnraise[exn:fail:contract?].
}

@defform[(stream expr ...)]{
  A shorthand for nested @racket[stream-cons]es ending with
  @racket[empty-stream].
}

@defproc[(in-stream [s stream?]) sequence?]{
  Returns a sequence that is equivalent to @racket[s].
  @speed[in-stream "streams"]
}

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
  does not terminate.
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
  where @racket[f] returns a false result in between two elements
  where @racket[f] returns a true result, then operations on this
  stream will not terminate during the infinite sub-stream.
}

@defproc[(stream-add-between [s stream?] [e any/c])
         stream?]{
  Returns a stream whose elements are the elements of @racket[s], but
  with @racket[e] between each pair of elements in @racket[s].  The
  new stream is constructed lazily.
}

@defthing[gen:stream any/c]{
  Associates three methods to a structure type to implement the
  @tech{generic interface} (see @secref["struct-generics"]) for
  streams.

  To supply method implementations, the @racket[#:methods] keyword
  should be used in a structure type definition. The following three
  methods should be implemented:

  @itemize[
    @item{@racket[stream-empty?] : accepts one argument}
    @item{@racket[stream-first] : accepts one argument}
    @item{@racket[stream-rest] : accepts one argument}
  ]

  @examples[#:eval sequence-evaluator
    (define-struct list-stream (v)
      #:methods gen:stream
      [(define (stream-empty? stream)
         (empty? (list-stream-v stream)))
       (define (stream-first stream)
         (first (list-stream-v stream)))
       (define (stream-rest stream)
         (rest (list-stream-v stream)))])

    (define l1 (list-stream '(1 2)))
    (stream? l1)
    (stream-first l1)
  ]
}

@defthing[prop:stream struct-type-property?]{
  A deprecated structure type property used to define custom
  extensions to the stream API. Use @racket[gen:stream] instead.
  Accepts a vector of three procedures taking the same arguments as
  the methods in @racket[gen:stream].
}

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

@defform[(generator formals body ...+)]{
  Creates a @tech{generator}, where @racket[formals] is like the
  @racket[formals] of @racket[case-lambda] (i.e., the
  @racket[_kw-formals] of @racket[lambda] restricted to non-optional
  and non-keyword arguments).

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
  @racket[yield] raises @racket[exn:fail] after evaluating its
  @racket[expr]s.

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
  formed by @racket[(generator () body ...+)]. The values produced by
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
    (let ([g (in-generator
              (let loop ([n 3])
                (unless (zero? n) (yield n (add1 n)) (loop (sub1 n)))))])
      (let-values ([(not-empty? next) (sequence-generate g)])
        (let loop () (when (not-empty?) (next) (loop))) 'done))
    (let ([g (in-generator #:arity 2
              (let loop ([n 3])
                (unless (zero? n) (yield n (add1 n)) (loop (sub1 n)))))])
      (let-values ([(not-empty? next) (sequence-generate g)])
        (let loop () (when (not-empty?) (next) (loop))) 'done))]

  To use an existing generator as a sequence, use @racket[in-producer]
  with a stop-value known for the generator:

  @interaction[#:eval generator-eval
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
