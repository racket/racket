#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@interaction-eval[(require (rename (lib "etc.ss") lambda opt-lambda))]
@interaction-eval[(require (only (lib "etc.ss") keyword-apply
                                                make-keyword-procedure))]

@title[#:tag "mz:procedures"]{Procedures}

@defproc[(procedure? [v any/c]) boolean]{ Returns @scheme[#t] if
@scheme[v] is a procedure, @scheme[#f] otherwise.}


@defproc[(apply [proc procedure?] [v any/c] ... [lst list?]) any]{

@guideintro["guide:apply"]{@scheme[apply]}

Applies @scheme[proc] using the content of @scheme[(list* v ... lst)]
as the (by-position) arguments. The given @scheme[proc] must accept as
many arguments as the number of @scheme[v]s plus length of
@scheme[lst], and it must not require any keyword arguments;
otherwise, the @exnraise[exn:fail:contract]. The given @scheme[proc]
is called in tail position with respect to the @scheme[apply] call.

@examples[
(apply + '(1 2 3))
(apply + 1 2 '(3))
(apply + '())
]}


@defproc[(keyword-apply [proc procedure?] 
                        [kw-lst (listof (cons/c keyword? any/c))] 
                        [v any/c] ... 
                        [lst list?])
         any]{

@guideintro["guide:apply"]{@scheme[keyword-apply]}

Like @scheme[apply], but @scheme[kw-lst] supplies by-keyword arguments
in addition to the by-position arguments of the @scheme[v]s and
@scheme[lst]. The given @scheme[kw-lst] must be sorted using
@scheme[keyword<] on the @scheme[car] of each pair in the list, and no
keyword can appear twice in the @scheme[car]s of @scheme[kw-lst]. The
given @scheme[proc] must accept all of the keywords in
@scheme[kw-lst], and it must not require any other keywords;
otherwise, the @exnraise[exn:fail:contract].

@defexamples[
(define (f x #:y y #:z [z 10])
  (list x y z))
(keyword-apply f '((#:y . 2)) '(1))
(keyword-apply f '((#:y . 2) (#:z . 3)) '(1))
]}

@defproc[(procedure-arity [proc procedure?]) 
         (or/c exact-nonnegative-integer?
               arity-at-least?
               (listof 
                (or/c exact-nonnegative-integer?
                      arity-at-least?)))]{

Returns information about the number of arguments accepted by
@scheme[proc] when called without keyword arguments. The result
@scheme[_a] is one of the following:

@itemize{

  @item{An exact non-negative integer, which means that the procedure
        always accepts exactly @scheme[_a] arguments.}

 @item{A @scheme[arity-at-least] instance, which means that the
       procedure takes @scheme[(arity-at-least-value _a)] or more
       arguments.}

 @item{A list containing integers and @scheme[arity-at-least]
       instances, which means that the procedure takes any number of
       arguments that can match one of the elements of @scheme[_a].}

}

If a procedure requires at least one keyword argument, then
@scheme[procedure-arity] returns @scheme['()] for the procedure.

@examples[
(procedure-arity cons)
(procedure-arity list)
(arity-at-least? (procedure-arity list))
(arity-at-least-value (procedure-arity list))
(arity-at-least-value (procedure-arity (lambda (x . y) x)))
(procedure-arity (case-lambda [(x) 0] [(x y) 1]))
]}

@defproc[(procedure-arity-includes? [proc procedure?] [k exact-nonnegative-integer?])
         boolean?]{
Returns @scheme[#t] if the procedure can accept @scheme[k] arguments
when no keyword arguments are supplied, @scheme[#f] otherwise.

@examples[
(procedure-arity-includes? cons 2)
(procedure-arity-includes? display 3)
]}

@defproc[(keyword-procedure-arity [proc procedure?]) 
         (values
          (listof keyword?)
          (or/c (listof keyword?) 
                false/c)
          (or/c 
           exact-nonnegative-integer?
           arity-at-least?
           (listof 
            (or/c 
             exact-nonnegative-integer?
             arity-at-least?))))]{

Similar to @scheme[keyword-procedure-arity], but for the case
that keywords are supplied to @scheme[proc]. The first result is
a list of keywords (sorted by @scheme[keyword<]) that are
required when applying @scheme[proc]. The second result is a list
of accepted keyword (sorted by @scheme[keyword<]), or @scheme[#f]
to mean that any keyword is accepted. The last result is as for
@scheme[procedure-arity] for the by-position arguments of
@scheme[proc], except that it is not @scheme['()] when keywords
are required.}

@defproc[(make-keyword-procedure
          [proc (((listof (cons/c keyword? any/c))) list? . ->* . any)])
         procedure?]{

Returns a procedure that accepts any number of arguments and all
keyword arguments (without requiring any keyword arguments). The
resulting procedure calls @scheme[proc], supplying to @scheme[proc]
all keyword arguments given in the original application as a list of
keyword--value pairs, sorted by @scheme[keyword<] on the keywords. All
by-position arguments supplied in the original application are
supplied to @scheme[proc] after the list for keyword arguments.

@defexamples[
(define show
  (make-keyword-procedure (lambda (kw-args . rest)
                            (list kw-args rest))))

(show 1)
(show #:init 0 1 2 3 #:extra 4)
]}

@defstruct[arity-at-least ([value nonnegative-exact-integer?])]{
This structure type is used for the result of @scheme[procedure-arity].}
