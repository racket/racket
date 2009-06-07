#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "for"]{Iterations and Comprehensions}

The @scheme[for] family of syntactic forms support iteration over
@defterm{sequences}. Lists, vectors, strings, byte strings, input
ports, and hash tables can all be used as sequences, and constructors
like @scheme[in-range] offer even more kinds of sequences.

Variants of @scheme[for] accumulate iteration results in different
ways, but they all have the same syntactic shape. Simplifying for
now, the syntax of @scheme[for] is

@specform[
(for ([id sequence-expr] ...)
  body ...+)
]{}

A @scheme[for] loop iterates through the sequence produced by the
@scheme[_sequence-expr]. For each element of the sequence,
@scheme[for] binds the element to @scheme[_id], and then it evaluates
the @scheme[_body]s for side effects.

@examples[
(for ([i '(1 2 3)])
  (display i))
(for ([i "abc"])
  (printf "~a..." i))
]

The @scheme[for/list] variant of @scheme[for] is more Scheme-like. It
accumulates @scheme[_body] results into a list, instead of
evaluating @scheme[_body] only for side effects. In more
technical terms, @scheme[for/list] implements a @defterm{list
comprehension}.

@examples[
(for/list ([i '(1 2 3)])
  (* i i))
(for/list ([i "abc"])
  i)
]

The full syntax of @scheme[for] accommodates multiple sequences to
iterate in parallel, and the @scheme[for*] variant nests the
iterations instead of running them in parallel. More variants of
@scheme[for] and @scheme[for*] accumulate @scheme[_body] results
in different ways.  In all of these variants, predicates that prune
iterations can be included along with bindings.

Before details on the variations of @scheme[for], though, it's best to
see the kinds of sequence generators that make interesting examples.

@section[#:tag "sequences"]{Sequence Constructors}

The @scheme[in-range] function generates a sequence of numbers, given
an optional starting number (which defaults to @scheme[0]), a number
before which the sequences ends, and an optional step (which defaults
to @scheme[1]).

@examples[
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

The @scheme[in-naturals] function is similar, except that the
starting number must be an exact non-negative integer (which defaults
to @scheme[0]), the step is always @scheme[1], and there is no upper
limit. A @scheme[for] loop using just @scheme[in-naturals] will never
terminate unless a body expression raises an exception or otherwise
escapes.

@examples[
(for ([i (in-naturals)])
  (if (= i 10)
      (error "too much!")
      (display i)))
]

The @scheme[stop-before] and @scheme[stop-after] functions construct
a new sequence given a sequence and a predicate. The new sequence is
like the given sequence, but truncated either immediately before or
immediately after the first element for which the predicate returns
true.

@examples[
(for ([i (stop-before "abc def"
                      char-whitespace?)])
  (display i))
]

Sequence constructors like @scheme[in-list], @scheme[in-vector] and
@scheme[in-string] simply make explicit the use of a list, vector, or
string as a sequence. Since they raise an exception when given the
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

@section{@scheme[for] and @scheme[for*]}

A more complete syntax of @scheme[for] is

@specform/subs[
(for (clause ...)
  body ...+)
([clause [id sequence-expr]
         (code:line #:when boolean-expr)])
]{}

When multiple @scheme[[_id _sequence-expr]] clauses are provided
in a @scheme[for] form, the corresponding sequences are traversed in
parallel:

@interaction[
(for ([i (in-range 1 4)]
      [chapter '("Intro" "Details" "Conclusion")])
  (printf "Chapter ~a. ~a\n" i chapter))
]

With parallel sequences, the @scheme[for] expression stops iterating
when any sequence ends. This behavior allows @scheme[in-naturals],
which creates an infinite sequence of numbers, to be used for
indexing:

@interaction[
(for ([i (in-naturals 1)]
      [chapter '("Intro" "Details" "Conclusion")])
  (printf "Chapter ~a. ~a\n" i chapter))
]

The @scheme[for*] form, which has the same syntax as @scheme[for],
nests multiple sequences instead of running them in parallel:

@interaction[
(for* ([book '("Guide" "Reference")]
       [chapter '("Intro" "Details" "Conclusion")])
  (printf "~a ~a\n" book chapter))
]

Thus, @scheme[for*] is a shorthand for nested @scheme[for]s in the
same way that @scheme[let*] is a shorthand for nested @scheme[let]s.

The @scheme[#:when _boolean-expr] form of a @scheme[_clause] is
another shorthand. It allows the @scheme[_body]s to evaluate only
when the @scheme[_boolean-expr] produces a true value:

@interaction[
(for* ([book '("Guide" "Reference")]
       [chapter '("Intro" "Details" "Conclusion")]
       #:when (not (equal? chapter "Details")))
  (printf "~a ~a\n" book chapter))
]

A @scheme[_boolean-expr] with @scheme[#:when] can refer to any of the
preceding iteration bindings. In a @scheme[for] form, this scoping
makes sense only if the test is nested in the iteration of the
preceding bindings; thus, bindings separated by @scheme[#:when] are
mutually nested, instead of in parallel, even with @scheme[for].

@interaction[
(for ([book '("Guide" "Reference" "Notes")]
      #:when (not (equal? book "Notes"))
      [i (in-naturals 1)]
      [chapter '("Intro" "Details" "Conclusion" "Index")]
      #:when (not (equal? chapter "Index")))
  (printf "~a Chapter ~a. ~a\n" book i chapter))
]

@section{@scheme[for/list] and @scheme[for*/list]}

The @scheme[for/list] form, which has the same syntax as @scheme[for],
evaluates the @scheme[_body]s to obtain values that go into a
newly constructed list:

@interaction[
(for/list ([i (in-naturals 1)]
           [chapter '("Intro" "Details" "Conclusion")])
  (string-append (number->string i) ". " chapter))
]

A @scheme[#:when] clause in a @scheme[for-list] form prunes the result
list along with evaluations of the @scheme[_body]s:

@interaction[
(for/list ([i (in-naturals 1)]
           [chapter '("Intro" "Details" "Conclusion")]
           #:when (odd? i))
  chapter)
]

This pruning behavior of @scheme[#:when] is more useful with
@scheme[for/list] than @scheme[for]. Whereas a plain @scheme[when]
form normally suffices with @scheme[for], a @scheme[when] expression
form in a @scheme[for/list] would cause the result list to contain
@|void-const|s instead of omitting list elements.

The @scheme[for*/list] is like @scheme[for*], nesting multiple
iterations:

@interaction[
(for*/list ([book '("Guide" "Ref.")]
            [chapter '("Intro" "Details")])
  (string-append book " " chapter))
]

A @scheme[for*/list] form is not quite the same thing as nested
@scheme[for/list] forms. Nested @scheme[for/list]s would produce a
list of lists, instead of one flattened list. Much like
@scheme[#:when], then, the nesting of @scheme[for*/list] is more
useful than the nesting of @scheme[for*].

@section{@scheme[for/and] and @scheme[for/or]}

The @scheme[for/and] form combines iteration results with
@scheme[and], stopping as soon as it encounters @scheme[#f]:

@interaction[
(for/and ([chapter '("Intro" "Details" "Conclusion")])
  (equal? chapter "Intro"))
]

The @scheme[for/or] form combines iteration results with @scheme[or],
stopping as soon as it encounters a true value:

@interaction[
(for/or ([chapter '("Intro" "Details" "Conclusion")])
  (equal? chapter "Intro"))
]

As usual, the @scheme[for*/and] and @scheme[for*/or] forms provide the
same facility with nested iterations.

@section{@scheme[for/first] and @scheme[for/last]}

The @scheme[for/first] form returns the result of the first time that
the @scheme[_body]s are evaluated, skipping further iterations.
This form is most useful with a @scheme[#:when] clause.

@interaction[
(for/first ([chapter '("Intro" "Details" "Conclusion" "Index")]
            #:when (not (equal? chapter "Intro")))
  chapter)
]

If the @scheme[_body]s are evaluated zero times, then the result
is @scheme[#f].

The @scheme[for/last] form runs all iterations, returning the value of
the last iteration (or @scheme[#f] if no iterations are run):

@interaction[
(for/last ([chapter '("Intro" "Details" "Conclusion" "Index")]
            #:when (not (equal? chapter "Index")))
  chapter)
]

As usual, the @scheme[for*/first] and @scheme[for*/last] forms provide
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

@section[#:tag "for/fold"]{@scheme[for/fold] and @scheme[for*/fold]}

The @scheme[for/fold] form is a very general way to combine iteration
results. Its syntax is slightly different than the syntax of
@scheme[for], because accumulation variables must be declared at the
beginning:

@schemeblock[
(for/fold ([_accum-id _init-expr] ...)
          (_clause ...)
  _body ...+)
]

In the simple case, only one @scheme[[_accum-id _init-expr]] is
provided, and the result of the @scheme[for/fold] is the final value
for @scheme[_accum-id], which starts out with the value of
@scheme[_init-expr]. In the @scheme[_clause]s and
@scheme[_body]s, @scheme[_accum-id] can be referenced to get its
current value, and the last @scheme[_body] provides the value of
@scheme[_accum-id] for the next iteration.

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

When multiple @scheme[_accum-id]s are specified, then the last
@scheme[_body] must produce multiple values, one for each
@scheme[_accum-id]. The @scheme[for/fold] expression itself produces
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

In the same way that @scheme[let-values] binds multiple results to
multiple identifiers, @scheme[for] can bind multiple sequence elements
to multiple iteration identifiers:

@margin-note{While @scheme[let] must be changed to @scheme[let-values]
             to bind multiple identifier, @scheme[for] simply allows a
             parenthesized list of identifiers instead of a single
             identifier in any clause.}

@interaction[
(for ([(k v) #hash(("apple" . 1) ("banana" . 3))])
  (printf "~a count: ~a\n" k v))
]

This extension to multiple-value bindings works for all @scheme[for]
variants. For example, @scheme[for*/list] nests iterations, builds a
list, and also works with multiple-valued sequences:

@interaction[
(for*/list ([(k v) #hash(("apple" . 1) ("banana" . 3))]
            [(i) (in-range v)])
  k)
]


@section[#:tag "for-performance"]{Iteration Performance}

Ideally, a @scheme[for] iteration should run as fast as a loop that
you write by hand as a recursive-function invocation. A hand-written
loop, however, is normally specific to a particular kind of data, such
as lists. In that case, the hand-written loop uses selectors like
@scheme[car] and @scheme[cdr] directly, instead of handling all forms
of sequences and dispatching to an appropriate iterator.

The @scheme[for] forms can provide the performance of hand-written
loops when enough information is apparent about the sequences to
iterate. Specifically, the clause should have one of the following
@scheme[_fast-clause] forms:

@schemegrammar[
fast-clause [id fast-seq]
            [(id) fast-seq]
            [(id id) fast-indexed-seq]
            [(id ...) fast-parallel-seq]
]

@schemegrammar[
#:literals [in-range in-naturals in-list in-vector in-string in-bytes in-value stop-before stop-after]
fast-seq (in-range expr expr)
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

@schemegrammar[
#:literals [in-indexed stop-before stop-after]
fast-indexed-seq (in-indexed fast-seq)
                  (stop-before fast-indexed-seq predicate-expr)
                  (stop-after fast-indexed-seq predicate-expr)
]

@schemegrammar[
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
a @scheme[for] @scheme[_clause].

@refdetails["for"]{iterations and comprehensions}
