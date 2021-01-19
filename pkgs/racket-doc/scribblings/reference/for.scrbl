#lang scribble/doc
@(require "mz.rkt"
          (for-label syntax/for-body))

@title[#:tag "for"]{Iterations and Comprehensions: @racket[for], @racket[for/list], ...}

@guideintro["for"]{iterations and comprehensions}

@(define for-eval (make-base-eval))
@(for-eval '(require (for-syntax racket/base)))

The @racket[for] iteration forms are based on SRFI-42
@cite["SRFI-42"].

@section{Iteration and Comprehension Forms}

@defform/subs[(for (for-clause ...) body-or-break ... body)
              ([for-clause [id seq-expr]
                           [(id ...) seq-expr]
                           (code:line #:when guard-expr)
                           (code:line #:unless guard-expr)
                           break-clause]
               [break-clause (code:line #:break guard-expr)
                             (code:line #:final guard-expr)]
               [body-or-break body
                              break-clause])
              #:contracts ([seq-expr sequence?])]{

Iteratively evaluates @racket[body]s. The @racket[for-clause]s
introduce bindings whose scope includes @racket[body] and that
determine the number of times that @racket[body] is evaluated.
A @racket[break-clause] either among the @racket[for-clause]s
or @racket[body]s stops further iteration.

In the simple case, each @racket[for-clause] has one of its first two
forms, where @racket[[id seq-expr]] is a shorthand for @racket[[(id)
seq-expr]].  In this simple case, the @racket[seq-expr]s are evaluated
left-to-right, and each must produce a sequence value (see
@secref["sequences"]).

The @racket[for] form iterates by drawing an element from each
sequence; if any sequence is empty, then the iteration stops, and
@|void-const| is the result of the @racket[for] expression. Otherwise
a location is created for each @racket[id] to hold the values of each
element; the sequence produced by a @racket[seq-expr] must return as
many values for each iteration as corresponding @racket[id]s.

The @racket[id]s are then bound in the @racket[body], which is
evaluated, and whose results are ignored. Iteration continues with the
next element in each sequence and with fresh locations for each
@racket[id].

A @racket[for] form with zero @racket[for-clause]s is equivalent to a
single @racket[for-clause] that binds an unreferenced @racket[id] to
a sequence containing a single element. All of the @racket[id]s must
be distinct according to @racket[bound-identifier=?].

If any @racket[for-clause] has the form @racket[#:when guard-expr],
then only the preceding clauses (containing no @racket[#:when] or @racket[#:unless])
determine iteration as above, and the @racket[body] is effectively
wrapped as

@racketblock[
(when guard-expr
  (for (for-clause ...) body ...+))
]

using the remaining @racket[for-clauses]. A @racket[for-clause] of
the form @racket[#:unless guard-expr] corresponds to the same transformation
with @racket[unless] in place of @racket[when].

A @racket[#:break guard-expr] clause is similar to a
@racket[#:unless guard-expr] clause, but when @racket[#:break]
avoids evaluation of the @racket[body]s, it also effectively ends all
sequences within the @racket[for] form.  A @racket[#:final
guard-expr] clause is similar to @racket[#:break guard-expr], but
instead of immediately ending sequences and skipping the
@racket[body]s, it allows at most one more element from each later
sequence and at most one more evaluation of the following
@racket[body]s. Among the @racket[body]s, besides stopping the
iteration and preventing later @racket[body] evaluations, a
@racket[#:break guard-expr] or @racket[#:final guard-expr]
clause starts a new internal-definition context.

In the case of @tech{list} and @tech{stream} sequences, the
@racket[for] form itself does not keep each element reachable. If a
list or stream produced by a @racket[seq-expr] is otherwise
unreachable, and if the @racket[for] body can no longer reference an
@racket[id] for a list element, then the element is subject to
@tech{garbage collection}. The @racket[make-do-sequence] sequence
constructor supports additional sequences that behave like lists and
streams in this way.

If a @racket[seq-expr] is a quoted literal list, vector, exact integer,
string, byte string, immutable hash, or expands to such a literal,
then it may be treated as if a sequence transformer such as
@racket[in-list] was used, unless the @racket[seq-expr] has a true
value for the @indexed-racket['for:no-implicit-optimization] syntax
property; in most cases this improves performance.

@examples[
(for ([i '(1 2 3)]
      [j "abc"]
      #:when (odd? i)
      [k #2(#t #f)])
  (display (list i j k)))
(for ([(i j) #hash(("a" . 1) ("b" . 20))])
  (display (list i j)))
(for ([i '(1 2 3)]
      [j "abc"]
      #:break (not (odd? i))
      [k #2(#t #f)])
  (display (list i j k)))
(for ([i '(1 2 3)]
      [j "abc"]
      #:final (not (odd? i))
      [k #2(#t #f)])
  (display (list i j k)))
(for ([i '(1 2 3)]
      [j "abc"]
      [k #2(#t #f)])
  #:break (not (or (odd? i) k))
  (display (list i j k)))
(for ()
  (display "here"))
(for ([i '()])
  (error "doesn't get here"))
]

@history[#:changed "6.7.0.4" @elem{Added support for the optional second result.}
         #:changed "7.8.0.11" @elem{Added support for implicit optimization.}]}

@defform[(for/list (for-clause ...) body-or-break ... body)]{ Iterates like
@racket[for], but that the last expression in the @racket[body]s must
produce a single value, and the result of the @racket[for/list]
expression is a list of the results in order.
When evaluation of a @racket[body] is skipped due to a @racket[#:when]
or @racket[#:unless] clause, the result list includes no corresponding
element.

@examples[
(for/list ([i '(1 2 3)]
           [j "abc"]
           #:when (odd? i)
           [k #2(#t #f)])
  (list i j k))
(for/list ([i '(1 2 3)]
           [j "abc"]
           #:break (not (odd? i))
           [k #2(#t #f)])
  (list i j k))
(for/list () 'any)
(for/list ([i '()])
  (error "doesn't get here"))
]}

@defform/subs[(for/vector maybe-length (for-clause ...) body-or-break ... body)
              ([maybe-length (code:line)
                             (code:line #:length length-expr)
                             (code:line #:length length-expr #:fill fill-expr)])
              #:contracts ([length-expr exact-nonnegative-integer?])]{

Iterates like @racket[for/list], but results are accumulated into
a vector instead of a list. 

If the optional @racket[#:length] clause is specified, the result of
@racket[length-expr] determines the length of the result vector.  In
that case, the iteration can be performed more efficiently, and it
terminates when the vector is full or the requested number of
iterations have been performed, whichever comes first. If
@racket[length-expr] specifies a length longer than the number of
iterations, then the remaining slots of the vector are initialized to
the value of @racket[fill-expr], which defaults to @racket[0] (i.e.,
the default argument of @racket[make-vector]).

@examples[
(for/vector ([i '(1 2 3)]) (number->string i))
(for/vector #:length 2 ([i '(1 2 3)]) (number->string i))
(for/vector #:length 4 ([i '(1 2 3)]) (number->string i))
(for/vector #:length 4 #:fill "?" ([i '(1 2 3)]) (number->string i))
]

The @racket[for/vector] form may allocate a vector and mutate it after
each iteration of @racket[body], which means that capturing a
continuation during @racket[body] and applying it multiple times may
mutate a shared vector.}


@deftogether[(
@defform[(for/hash (for-clause ...) body-or-break ... body)]
@defform[(for/hasheq (for-clause ...) body-or-break ... body)]
@defform[(for/hasheqv (for-clause ...) body-or-break ... body)]
)]{

Like @racket[for/list], but the result is an immutable @tech{hash
table}; @racket[for/hash] creates a table using @racket[equal?] to
distinguish keys, @racket[for/hasheq] produces a table using
@racket[eq?], and @racket[for/hasheqv] produces a table using
@racket[eqv?]. The last expression in the @racket[body]s must return
two values: a key and a value to extend the hash table accumulated by
the iteration.

@examples[
(for/hash ([i '(1 2 3)])
  (values i (number->string i)))
]}


@defform[(for/and (for-clause ...) body-or-break ... body)]{ Iterates like
@racket[for], but when last expression of @racket[body] produces
@racket[#f], then iteration terminates, and the result of the
@racket[for/and] expression is @racket[#f]. If the @racket[body]
is never evaluated, then the result of the @racket[for/and]
expression is @racket[#t]. Otherwise, the result is the (single)
result from the last evaluation of @racket[body].

@examples[
(for/and ([i '(1 2 3 "x")])
  (i . < . 3))
(for/and ([i '(1 2 3 4)])
  i)
(for/and ([i '(1 2 3 4)])
  #:break (= i 3)
  i)
(for/and ([i '()])
  (error "doesn't get here"))
]}

@defform[(for/or (for-clause ...) body-or-break ... body)]{ Iterates like
@racket[for], but when last expression of @racket[body] produces
a value other than @racket[#f], then iteration terminates, and
the result of the @racket[for/or] expression is the same
(single) value. If the @racket[body] is never evaluated, then the
result of the @racket[for/or] expression is
@racket[#f]. Otherwise, the result is @racket[#f].

@examples[
(for/or ([i '(1 2 3 "x")])
  (i . < . 3))
(for/or ([i '(1 2 3 4)])
  i)
(for/or ([i '()])
  (error "doesn't get here"))
]}

@deftogether[(
@defform[(for/sum (for-clause ...) body-or-break ... body)]
)]{

Iterates like @racket[for], but each result of the last @racket[body]
is accumulated into a result with @racket[+].

@examples[
(for/sum ([i '(1 2 3 4)]) i)
]}


@deftogether[(
@defform[(for/product (for-clause ...) body-or-break ... body)]
)]{

Iterates like @racket[for], but each result of the last @racket[body]
is accumulated into a result with @racket[*].

@examples[
(for/product ([i '(1 2 3 4)]) i)
]}


@defform[(for/lists (id ... maybe-result)
                    (for-clause ...)
           body-or-break ... body)
         #:grammar
         ([maybe-result (code:line) (code:line #:result result-expr)])]{

Similar to @racket[for/list], but the last @racket[body] expression
should produce as many values as given @racket[id]s.
The @racket[id]s are bound to
the reversed lists accumulated so far in the @racket[for-clause]s and
@racket[body]s.

If a @racket[result-expr] is provided, it is used as with @racket[for/fold]
when iteration terminates;
otherwise, the result is as many lists as supplied @racket[id]s.

The scope of @racket[id] bindings is the same as for accumulator
identifiers in @racket[for/fold]. Mutating a @racket[id] affects the
accumulated lists, and mutating it in a way that produces a non-list
can cause a final @racket[reverse] for each @racket[id] to fail.

@examples[
(for/lists (l1 l2 l3)
           ([i '(1 2 3)]
            [j "abc"]
            #:when (odd? i)
            [k #(#t #f)])
  (values i j k))
(for/lists (acc)
           ([x '(tvp tofu seitan tvp tofu)]
            #:unless (member x acc))
  x)
(for/lists (firsts seconds #:result (list firsts seconds))
           ([pr '((1 . 2) (3 . 4) (5 . 6))])
  (values (car pr) (cdr pr)))
]

@history[
 #:changed "7.1.0.2" @elem{Added the @racket[#:result] form.}
 ]}


@defform[(for/first (for-clause ...) body-or-break ... body)]{ Iterates like
@racket[for], but after @racket[body] is evaluated the first
time, then the iteration terminates, and the @racket[for/first]
result is the (single) result of @racket[body]. If the
@racket[body] is never evaluated, then the result of the
@racket[for/first] expression is @racket[#f].

@examples[
(for/first ([i '(1 2 3 "x")]
            #:when (even? i))
   (number->string i))
(for/first ([i '()])
  (error "doesn't get here"))
]}

@defform[(for/last (for-clause ...) body-or-break ... body)]{ Iterates like
@racket[for], but the @racket[for/last] result is the (single)
result of the last evaluation of @racket[body]. If the
@racket[body] is never evaluated, then the result of the
@racket[for/last] expression is @racket[#f].

@examples[
(for/last ([i '(1 2 3 4 5)]
            #:when (even? i))
   (number->string i))
(for/last ([i '()])
  (error "doesn't get here"))
]}

@defform/subs[(for/fold ([accum-id init-expr] ... maybe-result) (for-clause ...)
                body-or-break ... body)
              ([maybe-result (code:line)
                             (code:line #:result result-expr)])]{

Iterates like @racket[for]. Before iteration starts, the
@racket[init-expr]s are evaluated to produce initial accumulator
values. At the start of each iteration, a location is generated
for each @racket[accum-id], and the corresponding current accumulator
value is placed into the location. The last expression in
@racket[body] must produce as many values as @racket[accum-id]s, and
those values become the current accumulator values. When iteration
terminates, if a @racket[result-expr] is provided then the result of the
 @racket[for/fold] is the result of evaluating @racket[result-expr]
 (with @racket[accum-id]s in scope and bound to their final values),
 otherwise the results of the @racket[for/fold] expression are the
 accumulator values.

@examples[
(for/fold ([sum 0]
           [rev-roots null])
          ([i '(1 2 3 4)])
  (values (+ sum i) (cons (sqrt i) rev-roots)))

(for/fold ([acc '()]
           [seen (hash)]
           #:result (reverse acc))
          ([x (in-list '(0 1 1 2 3 4 4 4))])
  (cond
    [(hash-ref seen x #f)
     (values acc seen)]
    [else (values (cons x acc)
                  (hash-set seen x #t))]))
]

The binding and evaluation order of @racket[accum-id]s and
@racket[init-expr]s does not follow the textual, left-to-right order
relative to the @racket[for-clause]s . Instead, the sequence
expressions in @racket[for-clause]s that determine the outermost
iteration are evaluated first, the associated identifiers are bound,
and then the @racket[init-expr]s are evaluated and the
@racket[accum-id]s are bound. One consequence is that the
@racket[accum-id]s are not bound in @racket[for-clause]s for the
outermost initialization. Another consequence is that when a
@racket[accum-id] is used as a @racket[for-clause] binding for the
outermost iteration, the @racket[for-clause] binding is shadowed in
the loop body (even though, syntactically, a @racket[for-clause] is
closer to the body). A fresh variable for each @racket[accum-id] (at a
fresh location) is bound to in each nested iteration created by a
later group for @racket[for-clause]s (after a @racket[#:when] or
@racket[#:unless], for example).

@history[#:changed "6.11.0.1" @elem{Added the @racket[#:result] form.}]
}

@(define for/foldr-eval ((make-eval-factory '(racket/promise racket/sequence racket/stream))))
@defform[(for/foldr ([accum-id init-expr] ... accum-option ...)
                    (for-clause ...)
           body-or-break ... body)
         #:grammar ([accum-option (code:line #:result result-expr)
                                  #:delay
                                  (code:line #:delay-as delayed-id)
                                  (code:line #:delay-with delayer-id)])]{

Like @racket[for/fold], but analogous to @racket[foldr] rather than
@racket[foldl]: the given sequences are still iterated in the same order, but
the loop body is evaluated in reverse order. Evaluation of a @racket[for/foldr]
expression uses space proportional to the number of iterations it performs, and
all elements produced by the given sequences are retained until backwards
evaluation of the loop body begins (assuming the element is, in fact,
referenced in the body).

@(examples
  #:eval for/foldr-eval
  (define (in-printing seq)
    (sequence-map (lambda (v) (println v) v) seq))
  (eval:check (for/foldr ([acc '()])
                         ([v (in-printing (in-range 1 4))])
                (println v)
                (cons v acc))
              '(1 2 3)))

Furthermore, unlike @racket[for/fold], the @racket[accum-id]s are not bound
within @racket[_guard-expr]s or @racket[_body-or-break] forms that appear before
a @racket[_break-clause].

While the aforementioned limitations make @racket[for/foldr] less generally
useful than @racket[for/fold], @racket[for/foldr] provides the additional
capability to iterate lazily via the @racket[#:delay], @racket[#:delay-as], and
@racket[#:delay-with] options, which can mitigate many of @racket[for/foldr]'s
disadvantages. If at least one such option is specified, the loop body is given
explicit control over when iteration continues: by default, each
@racket[accum-id] is bound to a @tech{promise} that, when forced, produces the
@racket[accum-id]'s current value.

In this mode, iteration does not continue until one such promise is forced,
which triggers any additional iteration necessary to produce a value. If the
loop body is lazy in its @racket[accum-id]s---that is, it returns a value
without forcing any of them---then the loop (or any of its iterations) will
produce a value before iteration has completely finished. If a reference to at
least one such promise is retained, then forcing it will resume iteration from
the point at which it was suspended, even if control has left the dynamic extent
of the loop body.

@(examples
  #:eval for/foldr-eval
  (eval:check (for/foldr ([acc '()] #:delay)
                         ([v (in-range 1 4)])
                (printf "--> ~v\n" v)
                (begin0
                  (cons v (force acc))
                  (printf "<-- ~v\n" v)))
              '(1 2 3))
  (define resume
    (for/foldr ([acc '()] #:delay)
               ([v (in-range 1 5)])
      (printf "--> ~v\n" v)
      (begin0
        (cond
          [(= v 1) (force acc)]
          [(= v 2) acc]
          [else    (cons v (force acc))])
        (printf "<-- ~v\n" v))))
  (eval:check (force resume) '(3 4)))

This extra control over iteration order allows @racket[for/foldr] to both
consume and construct infinite sequences, so long as it is at least sometimes
lazy in its accumulators.

@margin-note/ref{
  See also @racket[for/stream] for a more convenient (albeit less flexible) way
  to lazily transform infinite sequences. (Internally, @racket[for/stream] is
  defined in terms of @racket[for/foldr].)}

@(examples
  #:eval for/foldr-eval
  (define squares (for/foldr ([s empty-stream] #:delay)
                             ([n (in-naturals)])
                    (stream-cons (* n n) (force s))))
  (stream->list (stream-take squares 10)))

The suspension introduced by the @racket[#:delay] option does not ordinarily
affect the loop's eventual return value, but if @racket[#:delay] and
@racket[#:result] are combined, the @racket[accum-id]s will be delayed in the
scope of the @racket[result-expr] in the same way they are delayed within the
loop body. This can be used to introduce an additional layer of suspension
around the evaluation of the entire loop, if desired.

@(examples
  #:eval for/foldr-eval
  (define evaluated-yet? #f)
  (for/foldr ([acc (set! evaluated-yet? #t)] #:delay) ()
    (force acc))
  (eval:check evaluated-yet? #t))
@(examples
  #:eval for/foldr-eval
  #:label #f
  (define evaluated-yet? #f)
  (define start
    (for/foldr ([acc (set! evaluated-yet? #t)] #:delay #:result acc) ()
      (force acc)))
  (eval:check evaluated-yet? #f)
  (force start)
  (eval:check evaluated-yet? #t))

If the @racket[#:delay-as] option is provided, then @racket[delayed-id] is
bound to an additional promise that returns the values of all @racket[accum-id]s
at once. When multiple @racket[accum-id]s are provided, forcing this promise can
be slightly more efficient than forcing the promises bound to the
@racket[accum-id]s individually.

If the @racket[#:delay-with] option is provided, the given @racket[delayer-id]
is used to suspend nested iterations (instead of the default, @racket[delay]).
A form of the shape @racket[(delayer-id _recur-expr)] is constructed and placed
in expression position, where @racket[_recur-expr] is an expression that, when
evaluated, will perform the next iteration and return its result (or results).
Sensible choices for @racket[delayer-id] include @racket[lazy],
@racket[delay/sync], @racket[delay/thread], or any of the other promise
constructors from @racketmodname[racket/promise], as well as @racket[thunk] from
@racketmodname[racket/function]. However, beware that choices such as
@racket[thunk] or @racket[delay/name] may evaluate their subexpression multiple
times, which can lead to nonsensical results for sequences that have state, as
the state will be shared between all evaluations of the @racket[_recur-expr].

If multiple @racket[accum-id]s are given, the @racket[#:delay-with] option is
provided, and @racket[delayer-id] is not bound to one of @racket[delay],
@racket[lazy], @racket[delay/strict], @racket[delay/sync],
@racket[delay/thread], or @racket[delay/idle], the @racket[accum-id]s will not
be bound at all, even within the loop body. Instead, the @racket[#:delay-as]
option must be specified to access the accumulator values via
@racket[delayed-id].

@history[#:added "7.3.0.3"]}
@(close-eval for/foldr-eval)

@defform[(for* (for-clause ...) body-or-break ... body)]{
Like @racket[for], but with an implicit @racket[#:when #t] between
each pair of @racket[for-clauses], so that all sequence iterations are
nested.

@examples[
(for* ([i '(1 2)]
       [j "ab"])
  (display (list i j)))
]}

@deftogether[(
@defform[(for*/list (for-clause ...) body-or-break ... body)]
@defform[(for*/lists (id ... maybe-result) (for-clause ...) 
           body-or-break ... body)]
@defform[(for*/vector maybe-length (for-clause ...) body-or-break ... body)]
@defform[(for*/hash (for-clause ...) body-or-break ... body)]
@defform[(for*/hasheq (for-clause ...) body-or-break ... body)]
@defform[(for*/hasheqv (for-clause ...) body-or-break ... body)]
@defform[(for*/and (for-clause ...) body-or-break ... body)]
@defform[(for*/or (for-clause ...) body-or-break ... body)]
@defform[(for*/sum (for-clause ...) body-or-break ... body)]
@defform[(for*/product (for-clause ...) body-or-break ... body)]
@defform[(for*/first (for-clause ...) body-or-break ... body)]
@defform[(for*/last (for-clause ...) body-or-break ... body)]
@defform[(for*/fold ([accum-id init-expr] ... maybe-result) (for-clause ...)
           body-or-break ... body)]
@defform[(for*/foldr ([accum-id init-expr] ... accum-option ...)
                     (for-clause ...)
           body-or-break ... body)]
)]{

Like @racket[for/list], etc., but with the implicit nesting of
@racket[for*].

@examples[
(for*/list ([i '(1 2)]
            [j "ab"])
  (list i j))
]

@history[#:changed "7.3.0.3" @elem{Added the @racket[for*/foldr] form.}]}

@;------------------------------------------------------------------------
@section{Deriving New Iteration Forms}

@defform[(for/fold/derived orig-datum
           ([accum-id init-expr] ... maybe-result) (for-clause ...)
           body-or-break ... body)]{

Like @racket[for/fold], but the extra @racket[orig-datum] is used as the
source for all syntax errors.

A macro that expands to @racket[for/fold/derived] should typically use
@racket[split-for-body] to handle the possibility of macros and other
definitions mixed with keywords like @racket[#:break].

@mz-examples[#:eval for-eval
(require (for-syntax syntax/for-body))
(define-syntax (for/digits stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (with-syntax ([original stx]
                   [((pre-body ...) (post-body ...))
                    (split-for-body stx #'(body ... tail-expr))])
       #'(let-values
             ([(n k)
               (for/fold/derived
                   original ([n 0] [k 1])
                 clauses
                 pre-body ...
                 (values (+ n (* (let () post-body ...) k)) (* k 10)))])
           n))]))

@code:comment{If we misuse for/digits, we can get good error reporting}
@code:comment{because the use of orig-datum allows for source correlation:}
(eval:error
 (for/digits
     [a (in-list '(1 2 3))]
     [b (in-list '(4 5 6))]
   (+ a b)))

(for/digits
    ([a (in-list '(1 2 3))]
     [b (in-list '(2 4 6))])
  (+ a b))


@code:comment{Another example: compute the max during iteration:}
(define-syntax (for/max stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (with-syntax ([original stx]
                   [((pre-body ...) (post-body ...))
                    (split-for-body stx #'(body ... tail-expr))])
       #'(for/fold/derived original
           ([current-max -inf.0])
           clauses
           pre-body ...
           (define maybe-new-max (let () post-body ...))
           (if (> maybe-new-max current-max)
               maybe-new-max
               current-max)))]))
(for/max ([n '(3.14159 2.71828 1.61803)]
          [s '(-1      1       1)])
  (* n s))
]

@history[#:changed "6.11.0.1" @elem{Added the @racket[#:result] form.}]}

@defform[(for*/fold/derived orig-datum
           ([accum-id init-expr] ... maybe-result) (for-clause ...)
           body-or-break ... body)]{
Like @racket[for*/fold], but the extra @racket[orig-datum] is used as the source for all syntax errors.

@mz-examples[#:eval for-eval
(require (for-syntax syntax/for-body))
(define-syntax (for*/digits stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (with-syntax ([original stx]
                   [((pre-body ...) (post-body ...))
                    (split-for-body stx #'(body ... tail-expr))])
       #'(let-values
             ([(n k)
               (for*/fold/derived original ([n 0] [k 1])
                 clauses
                 pre-body ...
                 (values (+ n (* (let () post-body ...) k)) (* k 10)))])
           n))]))

(eval:error
 (for*/digits
     [ds (in-list '((8 3) (1 1)))]
     [d (in-list ds)]
   d))

(for*/digits
    ([ds (in-list '((8 3) (1 1)))]
     [d (in-list ds)])
  d)
]

@history[#:changed "6.11.0.1" @elem{Added the @racket[#:result] form.}]}

@deftogether[(
@defform[(for/foldr/derived orig-datum
           ([accum-id init-expr] ... accum-option ...) (for-clause ...)
           body-or-break ... body)]
@defform[(for*/foldr/derived orig-datum
           ([accum-id init-expr] ... accum-option ...) (for-clause ...)
           body-or-break ... body)]
)]{

Like @racket[for/foldr] and @racket[for*/foldr], but the extra
@racket[orig-datum] is used as the source for all syntax errors as in
@racket[for/fold/derived] and @racket[for*/fold/derived].

@history[#:added "7.3.0.3"]}

@defform[(define-sequence-syntax id
           expr-transform-expr
           clause-transform-expr)
         #:contracts
         ([expr-transform-expr (or/c (-> identifier?)
                                     (syntax? . -> . syntax?))]
          [clause-transform-expr (syntax? . -> . syntax?)])]{

Defines @racket[id] as syntax. An @racket[(id . _rest)] form is
treated specially when used to generate a sequence in a
@racket[_for-clause] of @racket[for] (or one of its variants). In that
case, the procedure result of @racket[clause-transform-expr] is called
to transform the clause.

When @racket[id] is used in any other expression position, the result
of @racket[expr-transform-expr] is used. If it is a procedure of zero
arguments, then the result must be an identifier @racket[_other-id],
and any use of @racket[id] is converted to a use of
@racket[_other-id]. Otherwise, @racket[expr-transform-expr] must
produce a procedure (of one argument) that is used as a macro
transformer.

When the @racket[clause-transform-expr] transformer is used, it is
given a @racket[_for-clause] as an argument, where the clause's form is
normalized so that the left-hand side is a parenthesized sequence of
identifiers. The right-hand side is of the form @racket[(id . _rest)].
The result can be either @racket[#f], to indicate that the forms
should not be treated specially (perhaps because the number of bound
identifiers is inconsistent with the @racket[(id . _rest)] form), or a
new @racket[_for-clause] to replace the given one. The new clause might
use @racket[:do-in]. To protect identifiers in the result of 
@racket[clause-transform-expr], use @racket[for-clause-syntax-protect]
instead of @racket[syntax-protect].

@mz-examples[#:eval for-eval
(define (check-nat n)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'in-digits "exact-nonnegative-integer?" n)))
(define-sequence-syntax in-digits
  (lambda () #'in-digits/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(d) (_ nat)]
       #'[(d)
          (:do-in
            ([(n) nat])
            (check-nat n)
            ([i n])
            (not (zero? i))
            ([(j d) (quotient/remainder i 10)])
            #t
            #t
            [j])]]
      [_ #f])))

(define (in-digits/proc n)
  (for/list ([d (in-digits n)]) d))

(for/list ([d (in-digits 1138)]) d)

(map in-digits (list 137 216))
]}

@defform[(:do-in ([(outer-id ...) outer-expr] ...)
                 outer-check
                 ([loop-id loop-expr] ...)
                 pos-guard
                 ([(inner-id ...) inner-expr] ...)
                 pre-guard
                 post-guard
                 (loop-arg ...))]{

A form that can only be used as a @racket[_seq-expr] in a
@racket[_for-clause] of @racket[for] (or one of its variants).

Within a @racket[for], the pieces of the @racket[:do-in] form are
spliced into the iteration essentially as follows:

@racketblock[
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

where @racket[_body-bindings] and @racket[_done-expr] are from the
context of the @racket[:do-in] use. The identifiers bound by the
@racket[for] clause are typically part of the @racket[([(inner-id ...)
inner-expr] ...)] section.

Beware that @racket[_body-bindings] and @racket[_done-expr] can
contain arbitrary expressions, potentially including @racket[set!] on
@racket[outer-id] or @racket[inner-id] identifiers if they are visible
in the original @racket[for] form, so beware of depending on such
identifiers in @racket[post-guard] and @racket[loop-arg].

The actual @racket[loop] binding and call has additional loop
arguments to support iterations in parallel with the @racket[:do-in]
form, and the other pieces are similarly accompanied by pieces from
parallel iterations.

For an example of @racket[:do-in], see @racket[define-sequence-syntax].}

@defproc[(for-clause-syntax-protect [stx syntax?]) syntax?]{

Provided @racket[for-syntax]: Like @racket[syntax-protect], but allows
the @racket[for] expander to @tech{disarm} the result syntax object,
and arms the pieces of a clause instead of the entire syntax object.

Use this function to protect the result of a
@racket[_clause-transform-expr] that is bound by
@racket[define-sequence-syntax].}


@section{Do Loops}

@defform/subs[(do ([id init-expr step-expr-maybe] ...)
                  (stop?-expr finish-expr ...)
                expr ...)
              ([step-expr-maybe code:blank
                                step-expr])]{

Iteratively evaluates the @racket[expr]s for as long as
@racket[stop?-expr] returns @racket[#f].

To initialize the loop, the @racket[init-expr]s are evaluated in order
and bound to the corresponding @racket[id]s. The @racket[id]s are
bound in all expressions within the form other than the
@racket[init-expr]s.

After the @racket[id]s have been bound, the @racket[stop?-expr] is
evaluated. If it produces @racket[#f], each @racket[expr] is evaluated
for its side-effect. The @racket[id]s are then effectively updated
with the values of the @racket[step-expr]s, where the default
@racket[step-expr] for @racket[id] is just @racket[id]; more
precisely, iteration continues with fresh locations for the
@racket[id]s that are initialized with the values of the corresponding
@racket[step-expr]s.

When @racket[stop?-expr] produces a true value, then the
@racket[finish-expr]s are evaluated in order, and the last one is
evaluated in tail position to produce the overall value for the
@racket[do] form. If no @racket[finish-expr] is provided, the value of
the @racket[do] form is @|void-const|.}

@close-eval[for-eval]
