#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "values"]{Multiple Values}

See @secref["values-model"] for general information about multiple
result values. In addition to @scheme[call-with-values] (described in
this section), the @scheme[let-values], @scheme[let*-values],
@scheme[letrec-values], and @scheme[define-values] forms (among
others) create continuations that receive multiple values.

@defproc[(values [v any/c] ...) any]{

Returns the given @scheme[v]s. That is, @scheme[values] returns as
provided arguments.

@examples[
(values 1)
(values 1 2 3)
(values)
]}

@defproc[(call-with-values [generator (-> any)] [receiver procedure?]) any]{

Calls @scheme[generator], and passes the values that
@scheme[generator] produces as arguments to @scheme[receiver]. Thus,
@scheme[call-with-values] creates a continuation that accepts any
number of values that @scheme[receiver] can accept. The
@scheme[receiver] procedure is called in tail position with respect to
the @scheme[call-with-values] call.

@examples[
(call-with-values (lambda () (values 1 2)) +)
(call-with-values (lambda () 1) (lambda (x y) (+ x y)))
]}
