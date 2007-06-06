#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:derived-syntax"]{Derived Syntactic Forms}

@section[#:tag "mz:for"]{Iterations and Comprehensions: @scheme[for], @scheme[for/list], ...}

@guideintro["guide:for"]{iterations and comprehensions}

@defform[(for (for-clause ...) . body)]{

Iteratively evaluates @scheme[body]. The @scheme[for-clause]s
introduce bindings whose scope inculdes @scheme[body] and that
determine the number of times that @scheme[body] is evaluated.

In the simple case, each @scheme[for-clause] has the form

@specsubform[[id seq-expr]]{}

In this case, the @scheme[_seq-expr]s are evaluated left-to-right, and
each must produce a sequence value (see @secref["mz:sequences"]).  The
@scheme[for] form iterates by drawing an element from each sequence;
if any sequence is empty, then the iteration stops, and @|void-const|
is the result of the @scheme[for] expression. Otherwise a location is
created for each @scheme[_id] to hold the corresponding element. The
@scheme[_id]s are then bound in the @scheme[body], which is evaluated,
and whose result(s) is(are) ignored. Iteration continues with the next
element in each sequence and with fresh locations for each
@scheme[_id]. Zero @scheme[for-clause]s is equivalent to a single
@scheme[for-clause] that binds an unreferenced @scheme[_id] to a
sequence containing one element. All of the @scheme[_id]s must be
distinct according to @scheme[bound-identifier=?].

If any @scheme[for-clause] has the form

@specsubform[(code:line #:when guard-expr)]{}

then only the preceding clauses (containing no @scheme[#:when])
determine iteration as above, and the @scheme[body] is effectively
wrapped as

@schemeblock[
(when guard-expr
  (for (for-clause ...) . body))
]

using the remaining @scheme[for-clauses].

@examples[
(for ([i '(1 2 3)]
      [j "abc"]
      #:when (odd? i)
      [k #2(#t #f)])
  (display (list i j k)))
(for ()
  (display "here"))
(for ([i '()])
  (error "doesn't get here"))
]}

@defform[(for/list (for-clause ...) . body)]{ Iterates like
@scheme[for], but that the last expression of @scheme[body] must
produce a single value, and the result of the @scheme[for/list]
expression is a list of the results in order.

@examples[
(for/list ([i '(1 2 3)]
           [j "abc"]
           #:when (odd? i)
           [k #2(#t #f)])
  (list i j k))
(for/list () 'any)
(for/list ([i '()])
  (error "doesn't get here"))
]}

@defform[(for/and (for-clause ...) . body)]{ Iterates like
@scheme[for], but when last expression of @scheme[body] produces
@scheme[#f], then iteration terminates, and the result of the
@scheme[for/and] expression is @scheme[#f]. If the @scheme[body]
is never evaluated, then the result of the @scheme[for/and]
expression is @scheme[#t]. Otherwise, the result is the (single)
result from the last evaluation of @scheme[body].

@examples[
(for/and ([i '(1 2 3 "x")])
  (i . < . 3))
(for/and ([i '(1 2 3 4)])
  i)
(for/and ([i '()])
  (error "doesn't get here"))
]}

@defform[(for/or (for-clause ...) . body)]{ Iterates like
@scheme[for], but when last expression of @scheme[body] produces
a value other than @scheme[#f], then iteration terminates, and
the result of the @scheme[for/or] expression is the same
(single) value. If the @scheme[body] is never evaluated, then the
result of the @scheme[for/or] expression is
@scheme[#f]. Otherwise, the result is @scheme[#f].

@examples[
(for/or ([i '(1 2 3 "x")])
  (i . < . 3))
(for/or ([i '(1 2 3 4)])
  i)
(for/or ([i '()])
  (error "doesn't get here"))
]}

@defform[(for/first (for-clause ...) . body)]{ Iterates like
@scheme[for], but after @scheme[body] is evaluated the first
time, then the iteration terminates, and the @scheme[for/first]
result is the (single) result of @scheme[body]. If the
@scheme[body] is never evaluated, then the result of the
@scheme[for/first] expression is @scheme[#f].

@examples[
(for/first ([i '(1 2 3 "x")]
            #:when (even? i))
   (number->string i))
(for/first ([i '()])
  (error "doesn't get here"))
]}

@defform[(for/last (for-clause ...) . body)]{ Iterates like
@scheme[for], but the @scheme[for/last] result is the (single)
result of of the last evaluation of @scheme[body]. If the
@scheme[body] is never evaluated, then the result of the
@scheme[for/last] expression is @scheme[#f].

@examples[
(for/last ([i '(1 2 3 4 5)]
            #:when (even? i))
   (number->string i))
(for/last ([i '()])
  (error "doesn't get here"))
]}

@defform[(for/fold ([accum-id init-expr] ...) (for-clause ...). body)]{

Iterates like @scheme[for]. Before iteration starts, the
@scheme[init-expr]s are evaluated to produce initial accumulator
values. At the start of each out iteration, a location is generated
for each @scheme[accum-id], and the correspinding current accumulator
value is placed into the location. The last expression in
@scheme[body] must produce as many values as @scheme[accum-id]s, and
those values become the current accumulator values. When iteration
terminates, the result of the @scheme[fold/for] expression is(are) the
accumulator value(s).

@examples[
(for/fold ([sum 0]
           [rev-roots null])
          ([i '(1 2 3 4)])
  (values (+ sum i) (cons (sqrt i) rev-roots)))
]}

@defform[(for* (for-clause ...) . body)]{
Like @scheme[for], but with an implicit @scheme[#:when #t] between
each pair of @scheme[for-clauses], so that all sequence iterations are
nested.

@examples[
(for* ([i '(1 2)]
       [j "ab"])
  (display (list i j)))
]}

@defform[(for*/list (for-clause ...) . body)]{
Like @scheme[for/list], but with the implicit nesting of @scheme[for*].
@examples[
(for*/list ([i '(1 2)]
            [j "ab"])
  (list i j))
]}

@defform[(for*/and (for-clause ...) . body)]{
Like @scheme[for/and], but with the implicit nesting of @scheme[for*].}

@defform[(for*/or (for-clause ...) . body)]{
Like @scheme[for/or], but with the implicit nesting of @scheme[for*].}

@defform[(for*/first (for-clause ...) . body)]{
Like @scheme[for/first], but with the implicit nesting of @scheme[for*].}

@defform[(for*/last (for-clause ...) . body)]{
Like @scheme[for/last], but with the implicit nesting of @scheme[for*].}

@defform[(for*/fold ([accum-id init-expr] ...) (for-clause ...) . body)]{
Like @scheme[for/fold], but with the implicit nesting of @scheme[for*].}

@defform[(for-values (for-values-clause ...) . body)]{ Like
@scheme[for], but each @scheme[for-values-clause] has one of the
following two forms:

 @specsubform[[(id ...) seq-expr]]{ The sequence produced by
 @scheme[_seq-expr] must return as many values for each iteration as
 @scheme[id]s, and the values are placed in the locations generated
 for the @scheme[id]s.}

 @specsubform[(code:line #:when guard-expr)]{As in @scheme[for].}

@examples[
(for-values ([(i j) #hash(("a" . 1) ("b" . 20))])
  (display (list i j)))
]}

@defform[(for/list-values (for-values-clause ...) . body)]{ Like
@scheme[for/list], but with multiple-value clauses like
@scheme[for-values].}

@defform[(for/and-values (for-values-clause ...) . body)]{ Like
@scheme[for-and], but with multiple-value clauses like
@scheme[for-values].}

@defform[(for/or-values (for-values-clause ...) . body)]{ Like
@scheme[for/or], but with multiple-value clauses like
@scheme[for-values].}

@defform[(for/first-values (for-values-clause ...) . body)]{ Like
@scheme[for/first], but with multiple-value clauses like
@scheme[for-values].}

@defform[(for/last-values (for-values-clause ...) . body)]{ Like
@scheme[for/last], but with multiple-value clauses like
@scheme[for-values].}

@defform[(for/fold-values ([id expr] ...) (for-values-clause ...) . body)]{ Like
@scheme[for/fold], but with multiple-value clauses like
@scheme[for-values].}

@defform[(for*-values (for-values-clause ...) . body)]{
Like @scheme[for-values], but with the implicit nesting of @scheme[for*].}

@defform[(for*/list-values (for-values-clause ...) . body)]{
Like @scheme[for/list-values], but with the implicit nesting of @scheme[for*].}

@defform[(for*/and-values (for-values-clause ...) . body)]{
Like @scheme[for/and-values], but with the implicit nesting of @scheme[for*].}

@defform[(for*/or-values (for-values-clause ...) . body)]{
Like @scheme[for/or-values], but with the implicit nesting of @scheme[for*].}

@defform[(for*/first-values (for-values-clause ...) . body)]{
Like @scheme[for/first-values], but with the implicit nesting of @scheme[for*].}

@defform[(for*/last-values (for-values-clause ...) . body)]{
Like @scheme[for/last-values], but with the implicit nesting of @scheme[for*].}

@defform[(for*/fold-values ([id expr] ...) (for-values-clause ...) . body)]{
Like @scheme[for/fold-values], but with the implicit nesting of @scheme[for*].}
