#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:derived-syntax" #:style 'toc]{Derived Syntactic Forms}

@local-table-of-contents[]


@;------------------------------------------------------------------------
@section[#:tag "mz:cond"]{Conditionals: @scheme[cond]}

@defform/subs[#:literals (else =>)
              (cond cond-clause ...)
              ([cond-clause [test-expr then-expr ...+]
                            [else then-expr ...+]
                            [test-expr => proc-expr]
                            [test-expr]])]{

A @scheme[cond-clause] that starts with @scheme[else] must be the last
@scheme[cond-clause].

If no @scheme[cond-clause]s are present, the result is @|void-const|.

If only a @scheme[[else then-expr ...+]] is present, then the
@scheme[then-expr]s are evaluated. The results from all but the last
@scheme[then-expr] are ignored. The results of the last
@scheme[then-expr], which is in tail position with respect to the
@scheme[cond] form, provides the result for the whole @scheme[cond]
form.

Otherwise, the first @scheme[test-expr] is evaluated. If it produces
@scheme[#f], then the result is the same as a @scheme[cond] form with
the remaining @scheme[cond-clause]s, in tail position with respect to
the original @scheme[cond] form. Otherwise, evaluation depends on the
form of the @scheme[cond-clause]:

@specsubform[[test-expr then-expr ...+]]{The @scheme[then-expr]s are
evaluated in order, and the results from all but the last
@scheme[then-expr] are ignored. The results of the last
@scheme[then-expr], which is in tail position with respect to the
@scheme[cond] form, provides the result for the whole @scheme[cond]
form.}

@specsubform[#:literals (=>) [test-expr => proc-expr]]{The @scheme[proc-expr] is
evaluated, and it must produce a procedure that accepts on argument,
otherwise the @exnraise[exn:fail:contract]. The procedure is applied
to the result of @scheme[test-expr] in tail position with respect to
the @scheme[cond] expression.}

@specsubform[[test-expr]]{The result of the @scheme[test-expr] is
returned as the result of the @scheme[cond] form. The
@scheme[test-expr] is not in tail position.}

@examples[
(cond)
(cond
  [else 5])
(cond
 [(positive? -5) (error "doesn't get here")]
 [(zero? -5) (error "doesn't get here, either")]
 [(positive? 5) 'here])
(cond
 [(member 2 '(1 2 3)) => (lambda (l) (map - l))])
(cond
 [(member 2 '(1 2 3))])
]}

@;------------------------------------------------------------------------
@section{Boolean Combination: @scheme[and] and @scheme[or]}

@defform[(and expr ...)]{

If no @scheme[expr]s are provided, then result is @scheme[#f].

If a single @scheme[expr] is provided, then it is in tail position, so
the results of the @scheme[and] expression are the results of the
@scheme[expr].

Otherwise, the first @scheme[expr] is evaluated. If it produces
@scheme[#f], the result of the @scheme[and] expression is
@scheme[#f]. Otherwise, the result is the same as an @scheme[and]
expression with the remaining @scheme[expr]s in tail position with
respect to the original @scheme[and] form.

@examples[
(and)
(and 1)
(and (values 1 2))
(and #f (error "doesn't get here"))
(and #t 5)
]}

@defform[(or expr ...)]{

If no @scheme[expr]s are provided, then result is @scheme[#t].

If a single @scheme[expr] is provided, then it is in tail position, so
the results of the @scheme[and] expression are the results of the
@scheme[expr].

Otherwise, the first @scheme[expr] is evaluated. If it produces a
value other than @scheme[#f], that result is the result of the
@scheme[or] expression. Otherwise, the result is the same as an
@scheme[or] expression with the remaining @scheme[expr]s in tail
position with respect to the original @scheme[or] form.

@examples[
(or)
(or 1)
(or (values 1 2))
(or 5 (error "doesn't get here"))
(or #f 5)
]}


@;------------------------------------------------------------------------
@section{Guarded Evaluation: @scheme[when] and @scheme[unless]}

@defform[(when test-expr expr ...)]{

Evaluates the @scheme[text-expr]. If the result is any value other
than @scheme[#f], the @scheme[expr]s are evaluated, and the results
are ignored. No @scheme[expr] is in tail position with respect to the
@scheme[when] form.

@examples[
(when (positive? -5)
  (display "hi"))
(when (positive? 5)
  (display "hi")
  (display " there"))
]}

@defform[(unless test-expr expr ...)]{

Equivalent to @scheme[(when (not test-expr) expr ...)].

@examples[
(unless (positive? 5)
  (display "hi"))
(unless (positive? -5)
  (display "hi")
  (display " there"))
]}

@;------------------------------------------------------------------------
@include-section["define-struct.scrbl"]

@;------------------------------------------------------------------------
@include-section["for.scrbl"]
