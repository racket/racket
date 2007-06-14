#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

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
                        [kw-lst (listof keyword?)]
                        [kw-val-lst list?]
                        [v any/c] ... 
                        [lst list?])
         any]{

@guideintro["guide:apply"]{@scheme[keyword-apply]}

Like @scheme[apply], but @scheme[kw-lst] and @scheme[kw-val-lst]
supply by-keyword arguments in addition to the by-position arguments
of the @scheme[v]s and @scheme[lst]. The given @scheme[kw-lst] must be
sorted using @scheme[keyword<], and no keyword can appear twice in
@scheme[kw-lst], otherwise, the @exnraise[exn:fail:contract]. The
given @scheme[kw-val-lst] must have the same length as
@scheme[kw-lst], otherwise, the @exnraise[exn:fail:contract]. The
given @scheme[proc] must accept all of the keywords in
@scheme[kw-lst], it must not require any other keywords, and it must
accept as many by-position arguments as supplied via the @scheme[v]s
and @scheme[lst]; otherwise, the @exnraise[exn:fail:contract].

@defexamples[
(define (f x #:y y #:z [z 10])
  (list x y z))
(keyword-apply f '(#:y) '(2) '(1))
(keyword-apply f '(#:y #:z) '(2 3) '(1))
]}

@defproc[(procedure-arity [proc procedure?]) 
         (or/c exact-nonnegative-integer?
               arity-at-least?
               (listof 
                (or/c exact-nonnegative-integer?
                      arity-at-least?)))]{

Returns information about the number of by-position arguments accepted
by @scheme[proc]. The result @scheme[_a] is one of the following:

@itemize{

  @item{An exact non-negative integer, which means that the procedure
        accepts @scheme[_a] arguments, only.}

 @item{A @scheme[arity-at-least] instance, which means that the
       procedure accepts @scheme[(arity-at-least-value _a)] or more
       arguments.}

 @item{A list containing integers and @scheme[arity-at-least]
       instances, which means that the procedure accepts any number of
       arguments that can match one of the elements of @scheme[_a].}

}

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

@defproc[(procedure-keywords [proc procedure?])
         (values
          (listof keyword?)
          (or/c (listof keyword?) 
                false/c))]{

Returns information about the keyword arguments required and accepted
by a procedure. The first result is a list of keywords (sorted by
@scheme[keyword<]) that are required when applying @scheme[proc]. The
second result is a list of accepted keywords (sorted by
@scheme[keyword<]), or @scheme[#f] to mean that any keyword is
accepted. When the second result is a list, every element in the first
list is also in the second list.

@examples[
(procedure-keywords +)
(procedure-keywords (lambda (#:tag t #:mode m) t))
(procedure-keywords (lambda (#:tag t #:mode [m #f]) t))
]}

@defproc[(make-keyword-procedure
          [proc (((listof keyword?) list?) list? . ->* . any)]
          [plain-proc procedure? (lambda args (apply proc null null args))])
         procedure?]{

Returns a procedure that accepts all keyword arguments (without
requiring any keyword arguments).

When the result is called with keyword arguments, then @scheme[proc]
is called; the first argument is a list of keywords sorted by
@scheme[keyword<], the second argument is a parllel list containing a
value for each keyword, and the remaining arguments are the
by-position arguments.

When the result is called without keyword arguments, then
@scheme[plain-proc] is called. Furthermore, @scheme[procedure-arity]
obtains its result frmo @scheme[plain-proc].

@defexamples[
(define show
  (make-keyword-procedure (lambda (kws kw-args . rest)
                            (list kws kw-args rest))))

(show 1)
(show #:init 0 1 2 3 #:extra 4)
]}

@defstruct[arity-at-least ([value nonnegative-exact-integer?])]{
This structure type is used for the result of @scheme[procedure-arity].}
