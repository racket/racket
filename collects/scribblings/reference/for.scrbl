#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]
@require[(lib "for.ss")]

@title[#:tag "mz:for"]{Iterations and Comprehensions: @scheme[for], @scheme[for/list], ...}

@guideintro["guide:for"]{iterations and comprehensions}

The PLT Scheme iteration forms are based on SRFI-42
@cite[#:key "srfi-42"
      #:title "SRFI-42: Eager Comprehensions"
      #:author "Sebastian Egner"
      #:location "http://srfi.schemers.org/srfi-42/"
      #:date "2003"].

@section{Iteration and Comprehension Forms}

@defform/subs[(for (for-clause ...) . body)
              ([for-clause [id seq-expr]
                           [(id ...) seq-expr]
                           (code:line #:when guard-expr)])]{

Iteratively evaluates @scheme[body]. The @scheme[for-clause]s
introduce bindings whose scope inculdes @scheme[body] and that
determine the number of times that @scheme[body] is evaluated.

In the simple case, each @scheme[for-clause] has one of its first two
forms, where @scheme[[id seq-expr]] is a shorthand for @scheme[[(id
...) seq-expr]].  In this simple case, the @scheme[seq-expr]s are
evaluated left-to-right, and each must produce a sequence value (see
@secref["mz:sequences"]).

The @scheme[for] form iterates by drawing an element from each
sequence; if any sequence is empty, then the iteration stops, and
@|void-const| is the result of the @scheme[for] expression. Otherwise
a location is created for each @scheme[id] to hold the values of each
element; the sequence produced by a @scheme[seq-expr] must return as
many values for each iteration as corresponding @scheme[id]s.

The @scheme[id]s are then bound in the @scheme[body], which is
evaluated, and whose results are ignored. Iteration continues with the
next element in each sequence and with fresh locations for each
@scheme[id].

A @scheme[for] form with zero @scheme[for-clause]s is equivalent to a
single @scheme[for-clause] that binds an unreferenced @scheme[id] to
a sequence containing a single element. All of the @scheme[id]s must
be distinct according to @scheme[bound-identifier=?].

If any @scheme[for-clause] has the form @scheme[#:when guard-expr],
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
(for ([(i j) #hash(("a" . 1) ("b" . 20))])
  (display (list i j)))
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
terminates, the results of the @scheme[fold/for] expression are the
accumulator values.

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

@;------------------------------------------------------------------------
@section{Deriving New Iteration Forms}

@defform[(for/fold/derived orig-datum
           ([accum-id init-expr] ...) (for-clause ...) . body)]{
Like @scheme[fold/fold], but the extra @scheme[orig-datum] is used as the source for all syntax errors.
}

@defform[(for*/fold/derived orig-datum
           ([accum-id init-expr] ...) (for-clause ...) . body)]{
Like @scheme[fold*/fold], but the extra @scheme[orig-datum] is used as the source for all syntax errors.
}

@defform[(define-sequence-syntax id
           expr-transform-expr
           clause-transform-expr)]{

Defines @scheme[id] as syntax. An @scheme[(id . _rest)] form is
treated specially when used to generate a sequence in a
@scheme[_clause] of @scheme[for] (or one of its variants). In that
case, the procedure result of @scheme[clause-transform-expr] is called
to transform the clause.

When @scheme[id] is used in any other expression position, the result
of @scheme[expr-transform-expr] is used; if it is an identifier
@scheme[_other-id], then any use of @scheme[id] is converted to a use
of @scheme[_other-id]; otherwise,@scheme[expr-transform-expr] must
produce a procedure that is used as a macro transformer.


When the @scheme[clause-transform-expr] transformer is used, it is
given a @scheme[_clause] as an argument, where the clause's form is
normalized so that the left-hand side is a parenthesized sequence of
identifiers. The right-hand side is of the form @scheme[(id . _rest)].
The result can be either @scheme[#f], to indicate that the forms
should not be treated specially (perhaps because the number of bound
identifiers is inconsistent with the @scheme[(id . _rest)] form), or a
new @scheme[_clause] to to replace the given one. The new clause might
use @scheme[:do-in].}

@defform[(:do-in ([(outer-id ...) outer-expr] ...)
                 outer-check
                 ([loop-id loop-expr] ...)
                 pos-guard
                 ([(inner-id ...) inner-expr] ...)
                 pre-guard
                 post-guard
                 (loop-arg ...))]{

A form that can only be used as a @scheme[_seq-expr] in a
@scheme[_clause] of @scheme[for] (or one of its variants).

Within a @scheme[for], the pieces of the @scheme[:do-in] form are
spliced into the iteration essentially as follows:

@schemeblock[
(let-values ([(outer-id ...) outer-expr] ...)
  outer-check
  (let loop ([loop-id loop-expr] ...)
    (if pos-guard
        (let-values ([(inner-id ...) inner-expr] ...)
          (if pre-guard
              (let _body-bindings
                   (loop loop-arg ...))
              _done-expr))
         _done-expr)))
]

where @scheme[_body-bindings] and @scheme[_done-expr] are from the
context of the @scheme[:do-in] use. The actual @scheme[loop] binding
and call has additional loop arguments to support iterations in
parallel with the @scheme[:do-in] form, and the other pieces are
similarly accompanied by pieces form parallel iterations.}
