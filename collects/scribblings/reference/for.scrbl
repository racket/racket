#lang scribble/doc
@(require "mz.ss")

@title[#:tag "for"]{Iterations and Comprehensions: @scheme[for], @scheme[for/list], ...}

@guideintro["for"]{iterations and comprehensions}

The @scheme[for] iteration forms are based on SRFI-42
@cite["SRFI-42"].


@section{Iteration and Comprehension Forms}

@defform/subs[(for (for-clause ...) body ...+)
              ([for-clause [id seq-expr]
                           [(id ...) seq-expr]
                           (code:line #:when guard-expr)])
              #:contracts ([seq-expr sequence?])]{

Iteratively evaluates @scheme[body]. The @scheme[for-clause]s
introduce bindings whose scope includes @scheme[body] and that
determine the number of times that @scheme[body] is evaluated.

In the simple case, each @scheme[for-clause] has one of its first two
forms, where @scheme[[id seq-expr]] is a shorthand for @scheme[[(id)
seq-expr]].  In this simple case, the @scheme[seq-expr]s are evaluated
left-to-right, and each must produce a sequence value (see
@secref["sequences"]).

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
  (for (for-clause ...) body ...+))
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

@defform[(for/list (for-clause ...) body ...+)]{ Iterates like
@scheme[for], but that the last expression in the @scheme[body]s must
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

@deftogether[(
@defform*[((for/vector (for-clause ...) body ...)
           (for/vector #:length length-expr (for-clause ...) body ...))]
@defform*[((for*/vector (for-clause ...) body ...)
           (for*/vector #:length length-expr (for-clause ...) body ...))])]{

Iterates like @scheme[for] or @scheme[for*], but last expression in
the @scheme[body]s must produce a single value, which is placed in the
corresponding slot of a vector whose length is the number of
iterations.  The optional @scheme[length-expr], if present, may allow
the computation to be performed more efficiently by pre-allocating a
vector of the given length.  It is an error if evaluating the given
@scheme[length-expr] does not produce a valid length for a vector that
matches the number of iterations performed by the loop.}

@deftogether[(
@defform[(for/hash (for-clause ...) body ...+)]
@defform[(for/hasheq (for-clause ...) body ...+)]
@defform[(for/hasheqv (for-clause ...) body ...+)]
)]{

Like @scheme[for/list], but the result is an immutable @tech{hash
table}; @scheme[for/hash] creates a table using @scheme[equal?] to
distinguish keys, @scheme[for/hasheq] produces a table using
@scheme[eq?], and @scheme[for/hasheqv] produces a table using
@scheme[eqv?]. The last expression in the @scheme[body]s must return
two values: a key and a value to extend the hash table accumulated by
the iteration.

@examples[
(for/hash ([i '(1 2 3)])
  (values i (number->string i)))
]}


@defform[(for/and (for-clause ...) body ...+)]{ Iterates like
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

@defform[(for/or (for-clause ...) body ...+)]{ Iterates like
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


@defform[(for/lists (id ...) (for-clause ...) body ...+)]{

Similar to @scheme[for/list], but the last @scheme[body] expression
should produce as many values as given @scheme[id]s, and the result is
as many lists as supplied @scheme[id]s. The @scheme[id]s are bound to
the lists accumulated so far in the @scheme[for-clause]s and
@scheme[body]s.}


@defform[(for/first (for-clause ...) body ...+)]{ Iterates like
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

@defform[(for/last (for-clause ...) body ...+)]{ Iterates like
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

@defform[(for/fold ([accum-id init-expr] ...) (for-clause ...) . body)]{

Iterates like @scheme[for]. Before iteration starts, the
@scheme[init-expr]s are evaluated to produce initial accumulator
values. At the start of each iteration, a location is generated
for each @scheme[accum-id], and the corresponding current accumulator
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

@defform[(for* (for-clause ...) body ...+)]{
Like @scheme[for], but with an implicit @scheme[#:when #t] between
each pair of @scheme[for-clauses], so that all sequence iterations are
nested.

@examples[
(for* ([i '(1 2)]
       [j "ab"])
  (display (list i j)))
]}

@deftogether[(
@defform[(for*/list (for-clause ...) body ...+)]
@defform[(for*/lists (id ...) (for-clause ...) body ...+)]
@defform[(for*/hash (for-clause ...) body ...+)]
@defform[(for*/hasheq (for-clause ...) body ...+)]
@defform[(for*/hasheqv (for-clause ...) body ...+)]
@defform[(for*/and (for-clause ...) body ...+)]
@defform[(for*/or (for-clause ...) body ...+)]
@defform[(for*/first (for-clause ...) body ...+)]
@defform[(for*/last (for-clause ...) body ...+)]
@defform[(for*/fold ([accum-id init-expr] ...) (for-clause ...) body ...+)]
)]{

Like @scheme[for/list], etc., but with the implicit nesting of
@scheme[for*].

@examples[
(for*/list ([i '(1 2)]
            [j "ab"])
  (list i j))
]}

@;------------------------------------------------------------------------
@section{Deriving New Iteration Forms}

@defform[(for/fold/derived orig-datum
           ([accum-id init-expr] ...) (for-clause ...) body ...+)]{
Like @scheme[for/fold], but the extra @scheme[orig-datum] is used as the source for all syntax errors.
}

@defform[(for*/fold/derived orig-datum
           ([accum-id init-expr] ...) (for-clause ...) body ...+)]{
Like @scheme[for*/fold], but the extra @scheme[orig-datum] is used as the source for all syntax errors.
}

@defform[(define-sequence-syntax id
           expr-transform-expr
           clause-transform-expr)
         #:contracts
         ([expr-transform-expr (or/c (-> identifier?)
                                     (syntax? . -> . syntax?))]
          [clause-transform-expr (syntax? . -> . syntax?)])]{

Defines @scheme[id] as syntax. An @scheme[(id . _rest)] form is
treated specially when used to generate a sequence in a
@scheme[_clause] of @scheme[for] (or one of its variants). In that
case, the procedure result of @scheme[clause-transform-expr] is called
to transform the clause.

When @scheme[id] is used in any other expression position, the result
of @scheme[expr-transform-expr] is used. If it is a procedure of zero
arguments, then the result must be an identifier @scheme[_other-id],
and any use of @scheme[id] is converted to a use of
@scheme[_other-id]. Otherwise,@scheme[expr-transform-expr] must
produce a procedure (of one argument) that is used as a macro
transformer.

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
                   (if post-guard
                       (loop loop-arg ...)
                       _done-expr))
              _done-expr))
         _done-expr)))
]

where @scheme[_body-bindings] and @scheme[_done-expr] are from the
context of the @scheme[:do-in] use. The identifiers bound by the
@scheme[for] clause are typically part of the @scheme[([(inner-id ...)
inner-expr] ...)] section.

The actual @scheme[loop] binding and call has additional loop
arguments to support iterations in parallel with the @scheme[:do-in]
form, and the other pieces are similarly accompanied by pieces from
parallel iterations.}


@section{Do Loops}

@defform/subs[(do ([id init-expr step-expr-maybe] ...)
                  (stop?-expr finish-expr ...)
                expr ...+)
              ([step-expr-maybe code:blank
                                step-expr])]{

Iteratively evaluates the @scheme[expr]s for as long as
@scheme[stop?-expr] returns @scheme[#f].

To initialize the loop, the @scheme[init-expr]s are evaluated in order
and bound to the corresponding @scheme[id]s. The @scheme[id]s are
bound in all expressions within the form other than the
@scheme[init-expr]s.

After the @scheme[id]s are bound, then @scheme[stop?-expr] is
evaluated. If it produces @scheme[#f], each @scheme[expr] is evaluated
for its side-effect. The @scheme[id]s are then effectively updated
with the values of the @scheme[step-expr]s, where the default
@scheme[step-expr] for @scheme[id] is just @scheme[id]; more
precisely, iteration continues with fresh locations for the
@scheme[id]s that are initialized with the values of the corresponding
@scheme[step-expr]s.

When @scheme[stop?-expr] produces a true value, then the
@scheme[finish-expr]s are evaluated in order, and the last one is
evaluated in tail position to produce the overall value for the
@scheme[do] form. If no @scheme[finish-expr] is provided, the value of
the @scheme[do] form is @|void-const|.}
