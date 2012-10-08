#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "for"]{Iterations and Comprehensions}

The @racket[for] family of syntactic forms support iteration over
@defterm{sequences}. Lists, vectors, strings, byte strings, input
ports, and hash tables can all be used as sequences, and constructors
like @racket[in-range] offer even more kinds of sequences.

Variants of @racket[for] accumulate iteration results in different
ways, but they all have the same syntactic shape. Simplifying for
now, the syntax of @racket[for] is

@specform[
(for ([id sequence-expr] ...)
  body ...+)
]{}

A @racket[for] loop iterates through the sequence produced by the
@racket[_sequence-expr]. For each element of the sequence,
@racket[for] binds the element to @racket[_id], and then it evaluates
the @racket[_body]s for side effects.

@examples[
(for ([i '(1 2 3)])
  (display i))
(for ([i "abc"])
  (printf "~a..." i))
(for ([i 4])
  (display i))
]

The @racket[for/list] variant of @racket[for] is more Racket-like. It
accumulates @racket[_body] results into a list, instead of
evaluating @racket[_body] only for side effects. In more
technical terms, @racket[for/list] implements a @defterm{list
comprehension}.

@examples[
(for/list ([i '(1 2 3)])
  (* i i))
(for/list ([i "abc"])
  i)
(for/list ([i 4])
  i)
]

The full syntax of @racket[for] accommodates multiple sequences to
iterate in parallel, and the @racket[for*] variant nests the
iterations instead of running them in parallel. More variants of
@racket[for] and @racket[for*] accumulate @racket[_body] results
in different ways.  In all of these variants, predicates that prune
iterations can be included along with bindings.

Before details on the variations of @racket[for], though, it's best to
see the kinds of sequence generators that make interesting examples.

@section[#:tag "sequences"]{Sequence Constructors}

The @racket[in-range] function generates a sequence of numbers, given
an optional starting number (which defaults to @racket[0]), a number
before which the sequence ends, and an optional step (which defaults
to @racket[1]). Using a non-negative integer @racket[_k] directly as 
a sequence is a shorthand for @racket[(in-range _k)].

@examples[
(for ([i 3])
  (display i))
(for ([i (in-range 3)])
  (display i))
(for ([i (in-range 1 4)])
  (display i))
(for ([i (in-range 1 4 2)])
  (display i))
(for ([i (in-range 4 1 -1)])
  (display i))
(for ([i (in-range 1 4 1/2)])
  (printf " ~a " i))
]

The @racket[in-naturals] function is similar, except that the
starting number must be an exact non-negative integer (which defaults
to @racket[0]), the step is always @racket[1], and there is no upper
limit. A @racket[for] loop using just @racket[in-naturals] will never
terminate unless a body expression raises an exception or otherwise
escapes.

@examples[
(for ([i (in-naturals)])
  (if (= i 10)
      (error "too much!")
      (display i)))
]

The @racket[stop-before] and @racket[stop-after] functions construct
a new sequence given a sequence and a predicate. The new sequence is
like the given sequence, but truncated either immediately before or
immediately after the first element for which the predicate returns
true.

@examples[
(for ([i (stop-before "abc def"
                      char-whitespace?)])
  (display i))
]

Sequence constructors like @racket[in-list], @racket[in-vector] and
@racket[in-string] simply make explicit the use of a list, vector, or
string as a sequence. Along with @racket[in-range],
these constructors raise an exception when given the
wrong kind of value, and since they otherwise avoid a run-time
dispatch to determine the sequence type, they enable more efficient
code generation; see @secref["for-performance"] for more information.

@examples[
(for ([i (in-string "abc")])
  (display i))
(for ([i (in-string '(1 2 3))])
  (display i))
]

@refdetails["sequences"]{sequences}

@section{@racket[for] and @racket[for*]}

A more complete syntax of @racket[for] is

@specform/subs[
(for (clause ...)
  body ...+)
([clause [id sequence-expr]
         (code:line #:when boolean-expr)
         (code:line #:unless boolean-expr)])
]{}

When multiple @racket[[_id _sequence-expr]] clauses are provided
in a @racket[for] form, the corresponding sequences are traversed in
parallel:

@interaction[
(for ([i (in-range 1 4)]
      [chapter '("Intro" "Details" "Conclusion")])
  (printf "Chapter ~a. ~a\n" i chapter))
]

With parallel sequences, the @racket[for] expression stops iterating
when any sequence ends. This behavior allows @racket[in-naturals],
which creates an infinite sequence of numbers, to be used for
indexing:

@interaction[
(for ([i (in-naturals 1)]
      [chapter '("Intro" "Details" "Conclusion")])
  (printf "Chapter ~a. ~a\n" i chapter))
]

The @racket[for*] form, which has the same syntax as @racket[for],
nests multiple sequences instead of running them in parallel:

@interaction[
(for* ([book '("Guide" "Reference")]
       [chapter '("Intro" "Details" "Conclusion")])
  (printf "~a ~a\n" book chapter))
]

Thus, @racket[for*] is a shorthand for nested @racket[for]s in the
same way that @racket[let*] is a shorthand for nested @racket[let]s.

The @racket[#:when _boolean-expr] form of a @racket[_clause] is
another shorthand. It allows the @racket[_body]s to evaluate only
when the @racket[_boolean-expr] produces a true value:

@interaction[
(for* ([book '("Guide" "Reference")]
       [chapter '("Intro" "Details" "Conclusion")]
       #:when (not (equal? chapter "Details")))
  (printf "~a ~a\n" book chapter))
]

A @racket[_boolean-expr] with @racket[#:when] can refer to any of the
preceding iteration bindings. In a @racket[for] form, this scoping
makes sense only if the test is nested in the iteration of the
preceding bindings; thus, bindings separated by @racket[#:when] are
mutually nested, instead of in parallel, even with @racket[for].

@interaction[
(for ([book '("Guide" "Reference" "Notes")]
      #:when (not (equal? book "Notes"))
      [i (in-naturals 1)]
      [chapter '("Intro" "Details" "Conclusion" "Index")]
      #:when (not (equal? chapter "Index")))
  (printf "~a Chapter ~a. ~a\n" book i chapter))
]

An @racket[#:unless] clause is analogus to a @racket[#:when] clause, but
the @racket[_body]s evaluate only when the @racket[_boolean-expr]
produces a false value.

@section{@racket[for/list] and @racket[for*/list]}

The @racket[for/list] form, which has the same syntax as @racket[for],
evaluates the @racket[_body]s to obtain values that go into a
newly constructed list:

@interaction[
(for/list ([i (in-naturals 1)]
           [chapter '("Intro" "Details" "Conclusion")])
  (string-append (number->string i) ". " chapter))
]

A @racket[#:when] clause in a @racket[for-list] form prunes the result
list along with evaluations of the @racket[_body]s:

@interaction[
(for/list ([i (in-naturals 1)]
           [chapter '("Intro" "Details" "Conclusion")]
           #:when (odd? i))
  chapter)
]

This pruning behavior of @racket[#:when] is more useful with
@racket[for/list] than @racket[for]. Whereas a plain @racket[when]
form normally suffices with @racket[for], a @racket[when] expression
form in a @racket[for/list] would cause the result list to contain
@|void-const|s instead of omitting list elements.

The @racket[for*/list] form is like @racket[for*], nesting multiple
iterations:

@interaction[
(for*/list ([book '("Guide" "Ref.")]
            [chapter '("Intro" "Details")])
  (string-append book " " chapter))
]

A @racket[for*/list] form is not quite the same thing as nested
@racket[for/list] forms. Nested @racket[for/list]s would produce a
list of lists, instead of one flattened list. Much like
@racket[#:when], then, the nesting of @racket[for*/list] is more
useful than the nesting of @racket[for*].

@section{@racket[for/vector] and @racket[for*/vector]}

The @racket[for/vector] form can be used with the same syntax as the
@racket[for/list] form, but the evaluated @racket[_body]s go into a
newly-constructed vector instead of a list:

@interaction[
(for/vector ([i (in-naturals 1)]
             [chapter '("Intro" "Details" "Conclusion")])
  (string-append (number->string i) ". " chapter))
]

The @racket[for*/vector] form behaves similarly, but the iterations are
nested as in @racket[for*].

The @racket[for/vector] and @racket[for*/vector] forms also allow the
length of the vector to be constructed to be supplied in advance.  The
resulting iteration can be performed more efficiently than plain
@racket[for/vector] or @racket[for*/vector]:

@interaction[
(let ([chapters '("Intro" "Details" "Conclusion")])
  (for/vector #:length (length chapters) ([i (in-naturals 1)]
                                          [chapter chapters])
    (string-append (number->string i) ". " chapter)))
]

If a length is provided, the iteration stops when the vector is filled
or the requested iterations are complete, whichever comes first.  If
the provided length exceeds the requested number of iterations, then
the remaining slots in the vector are initialized to the default
argument of @racket[make-vector].

@section{@racket[for/and] and @racket[for/or]}

The @racket[for/and] form combines iteration results with
@racket[and], stopping as soon as it encounters @racket[#f]:

@interaction[
(for/and ([chapter '("Intro" "Details" "Conclusion")])
  (equal? chapter "Intro"))
]

The @racket[for/or] form combines iteration results with @racket[or],
stopping as soon as it encounters a true value:

@interaction[
(for/or ([chapter '("Intro" "Details" "Conclusion")])
  (equal? chapter "Intro"))
]

As usual, the @racket[for*/and] and @racket[for*/or] forms provide the
same facility with nested iterations.

@section{@racket[for/first] and @racket[for/last]}

The @racket[for/first] form returns the result of the first time that
the @racket[_body]s are evaluated, skipping further iterations.
This form is most useful with a @racket[#:when] clause.

@interaction[
(for/first ([chapter '("Intro" "Details" "Conclusion" "Index")]
            #:when (not (equal? chapter "Intro")))
  chapter)
]

If the @racket[_body]s are evaluated zero times, then the result
is @racket[#f].

The @racket[for/last] form runs all iterations, returning the value of
the last iteration (or @racket[#f] if no iterations are run):

@interaction[
(for/last ([chapter '("Intro" "Details" "Conclusion" "Index")]
            #:when (not (equal? chapter "Index")))
  chapter)
]

As usual, the @racket[for*/first] and @racket[for*/last] forms provide
the same facility with nested iterations:

@interaction[
(for*/first ([book '("Guide" "Reference")]
             [chapter '("Intro" "Details" "Conclusion" "Index")]
             #:when (not (equal? chapter "Intro")))
  (list book chapter))

(for*/last ([book '("Guide" "Reference")]
            [chapter '("Intro" "Details" "Conclusion" "Index")]
            #:when (not (equal? chapter "Index")))
  (list book chapter))
]

@section[#:tag "for/fold"]{@racket[for/fold] and @racket[for*/fold]}

The @racket[for/fold] form is a very general way to combine iteration
results. Its syntax is slightly different than the syntax of
@racket[for], because accumulation variables must be declared at the
beginning:

@racketblock[
(for/fold ([_accum-id _init-expr] ...)
          (_clause ...)
  _body ...+)
]

In the simple case, only one @racket[[_accum-id _init-expr]] is
provided, and the result of the @racket[for/fold] is the final value
for @racket[_accum-id], which starts out with the value of
@racket[_init-expr]. In the @racket[_clause]s and
@racket[_body]s, @racket[_accum-id] can be referenced to get its
current value, and the last @racket[_body] provides the value of
@racket[_accum-id] for the next iteration.

@examples[
(for/fold ([len 0])
          ([chapter '("Intro" "Conclusion")])
  (+ len (string-length chapter)))
(for/fold ([prev #f])
          ([i (in-naturals 1)]
           [chapter '("Intro" "Details" "Details" "Conclusion")]
           #:when (not (equal? chapter prev)))
  (printf "~a. ~a\n" i chapter)
  chapter)
]

When multiple @racket[_accum-id]s are specified, then the last
@racket[_body] must produce multiple values, one for each
@racket[_accum-id]. The @racket[for/fold] expression itself produces
multiple values for the results.

@examples[
(for/fold ([prev #f]
           [counter 1])
          ([chapter '("Intro" "Details" "Details" "Conclusion")]
           #:when (not (equal? chapter prev)))
  (printf "~a. ~a\n" counter chapter)
  (values chapter
          (add1 counter)))
]

@section{Multiple-Valued Sequences}

In the same way that a function or expression can produce multiple
values, individual iterations of a sequence can produce multiple
elements.  For example, a hash table as a sequence generates two
values for each iteration: a key and a value.

In the same way that @racket[let-values] binds multiple results to
multiple identifiers, @racket[for] can bind multiple sequence elements
to multiple iteration identifiers:

@margin-note{While @racket[let] must be changed to @racket[let-values]
             to bind multiple identifiers, @racket[for] simply allows a
             parenthesized list of identifiers instead of a single
             identifier in any clause.}

@interaction[
(for ([(k v) #hash(("apple" . 1) ("banana" . 3))])
  (printf "~a count: ~a\n" k v))
]

This extension to multiple-value bindings works for all @racket[for]
variants. For example, @racket[for*/list] nests iterations, builds a
list, and also works with multiple-valued sequences:

@interaction[
(for*/list ([(k v) #hash(("apple" . 1) ("banana" . 3))]
            [(i) (in-range v)])
  k)
]


@section{Breaking an Iteration}

An even more complete syntax of @racket[for] is

@specform/subs[
(for (clause ...)
  body-or-break ... body)
([clause [id sequence-expr]
         (code:line #:when boolean-expr)
         (code:line #:unless boolean-expr)
         break]
 [body-or-break body break]
 [break  (code:line #:break boolean-expr)
         (code:line #:final boolean-expr)])
]{}

That is, a @racket[#:break] or @racket[#:final] clause can
be included among the binding clauses and body of the iteration. Among
the binding clauses, @racket[#:break] is like @racket[#:unless]
but when its @racket[_boolean-expr] is true, all sequences within the
@racket[for] are stopped. Among the @racket[_body]s,
@racket[#:break] has the same effect on sequences when its
@racket[_boolean-expr] is true, and it also prevents later
@racket[_body]s from evaluation in the current iteration.

For example, while using @racket[#:when] between clauses effectively
skips later sequences as well as the body,

@interaction[
(for ([book '("Guide" "Story" "Reference")]
      #:unless (equal? book "Story")
      [chapter '("Intro" "Details" "Conclusion")])
  (printf "~a ~a\n" book chapter))
]

using @racket[#:break] causes the entire @racket[for] iteration
to terminate:

@interaction[
(for ([book '("Guide" "Story" "Reference")]
      #:break (equal? book "Story")
      [chapter '("Intro" "Details" "Conclusion")])
  (printf "~a ~a\n" book chapter))
(for* ([book '("Guide" "Story" "Reference")]
       [chapter '("Intro" "Details" "Conclusion")])
  #:break (and (equal? book "Story")
               (equal? chapter "Conclusion"))
  (printf "~a ~a\n" book chapter))
]

A @racket[#:final] clause is similar to @racket[#:break],
but it does not immediately terminate the iteration. Instead, it
allows at most one more element to be drawn for each sequence and at
most one more evaluation of the @racket[_body]s.


@interaction[
(for* ([book '("Guide" "Story" "Reference")]
       [chapter '("Intro" "Details" "Conclusion")])
  #:final (and (equal? book "Story")
               (equal? chapter "Conclusion"))
  (printf "~a ~a\n" book chapter))
(for ([book '("Guide" "Story" "Reference")]
      #:final (equal? book "Story")
      [chapter '("Intro" "Details" "Conclusion")])
  (printf "~a ~a\n" book chapter))
]

@section[#:tag "for-performance"]{Iteration Performance}

Ideally, a @racket[for] iteration should run as fast as a loop that
you write by hand as a recursive-function invocation. A hand-written
loop, however, is normally specific to a particular kind of data, such
as lists. In that case, the hand-written loop uses selectors like
@racket[car] and @racket[cdr] directly, instead of handling all forms
of sequences and dispatching to an appropriate iterator.

The @racket[for] forms can provide the performance of hand-written
loops when enough information is apparent about the sequences to
iterate. Specifically, the clause should have one of the following
@racket[_fast-clause] forms:

@racketgrammar[
fast-clause [id fast-seq]
            [(id) fast-seq]
            [(id id) fast-indexed-seq]
            [(id ...) fast-parallel-seq]
]

@racketgrammar[
#:literals [in-range in-naturals in-list in-vector in-string in-bytes in-value stop-before stop-after]
fast-seq (in-range expr)
         (in-range expr expr)
         (in-range expr expr expr)
         (in-naturals)
         (in-naturals expr)
         (in-list expr)
         (in-vector expr)
         (in-string expr)
         (in-bytes expr)
         (in-value expr)
         (stop-before fast-seq predicate-expr)
         (stop-after fast-seq predicate-expr)
]

@racketgrammar[
#:literals [in-indexed stop-before stop-after]
fast-indexed-seq (in-indexed fast-seq)
                  (stop-before fast-indexed-seq predicate-expr)
                  (stop-after fast-indexed-seq predicate-expr)
]

@racketgrammar[
#:literals [in-parallel stop-before stop-after]
fast-parallel-seq (in-parallel fast-seq ...)
                  (stop-before fast-parallel-seq predicate-expr)
                  (stop-after fast-parallel-seq predicate-expr)
]

@examples[
(time (for ([i (in-range 100000)])
        (for ([elem (in-list '(a b c d e f g h))]) (code:comment @#,elem{fast})
          (void))))
(time (for ([i (in-range 100000)])
        (for ([elem '(a b c d e f g h)])           (code:comment @#,elem{slower})
          (void))))
(time (let ([seq (in-list '(a b c d e f g h))])
        (for ([i (in-range 100000)])
          (for ([elem seq])                        (code:comment @#,elem{slower})
            (void)))))
]

The grammars above are not complete, because the set of syntactic
patterns that provide good performance is extensible, just like the
set of sequence values. The documentation for a sequence constructor
should indicate the performance benefits of using it directly in
a @racket[for] @racket[_clause].

@refdetails["for"]{iterations and comprehensions}
