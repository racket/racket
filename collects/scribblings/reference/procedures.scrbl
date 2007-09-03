#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "procedures"]{Procedures}

@defproc[(procedure? [v any/c]) boolean]{ Returns @scheme[#t] if
@scheme[v] is a procedure, @scheme[#f] otherwise.}


@defproc[(apply [proc procedure?] [v any/c] ... [lst list?]) any]{

@guideintro["apply"]{@scheme[apply]}

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

@guideintro["apply"]{@scheme[keyword-apply]}

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
         arity?]{

Returns information about the number of by-position arguments accepted
by @scheme[proc]. See also @scheme[arity?].}

@defproc[(arity? [v any/c]) boolean?]{

A valid arity is one of the following:

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

@defproc[(procedure-reduce-arity [proc procedure?]
                                 [arity arity?])
         procedure?]{

Returns a procedure that is the same as @scheme[proc] (including
the same name returned by @scheme[object-name]), but that accepts
only arguments consistent with @scheme[arity]. In particular,
when @scheme[procedure-arity] is applied to the generated
procedure, it returns a value that is @scheme[equal?] to
@scheme[arity].

If the @scheme[arity] specification allows arguments that are not
in @scheme[(procedure-arity proc)], the @exnraise[exn:fail:contract].}

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

This structure type is used for the result of @scheme[procedure-arity].
See also @scheme[arity?].}


@defthing[prop:procedure struct-type-property?]{

A @tech{structure type property} to indentify structure types whose
instances can be applied as procedures. In particular, when
@scheme[procedure?] is applied to the instance, the result will be
@scheme[#t], and when an instance is used in the function position of
an application expression, a procedure is extracted from the instance
and used to complete the procedure call.

If the @scheme[prop:procedure] property value is an integer, it
designates a field within the structure that should contain a
procedure. The integer must be between @scheme[0] (inclusive) and the
number of non-automatic fields in the structure type (exclusive, not
counting supertype fields). The designated field must also be
specified as immutable, so that after an instance of the structure is
created, its procedure cannot be changed. (Otherwise, the arity and
name of the instance could change, and such mutations are generally
not allowed for procedures.) When the instance is used as the
procedure in an application expression, the value of the designated
field in the instance is used to complete the procedure call. (This
procedure can be another structure that acts as a procedure; the
immutability of procedure fields disallows cycles in the procedure
graph, so that the procedure call will eventually continue with a
non-structure procedure.) That procedure receives all of the arguments
from the application expression. The procedure's name (see
@scheme[object-name]) and arity (see @scheme[procedure-arity]) are also
used for the name and arity of the structure. If the value in the
designated field is not a procedure, then the instance behaves like
@scheme[(case-lambda)] (i.e., a procedure which does not accept any
number of arguments).

Providing an integer @scheme[proc-spec] argument to
@scheme[make-struct-type] is the same as both supplying the value with
the @scheme[prop:procedure] property and designating the field as
immutable (so that a property binding or immutable designation is
redundant and disallowed).

@examples[
(define-struct annotated-proc ([base #:immutable] note)
               #:property prop:procedure (struct-field-index base))
(define plus1 (make-annotated-proc
                (lambda (x) (+ x 1))
                "adds 1 to its argument"))
(procedure? plus1)
(annotated-proc? plus1)
(plus1 10)
(annotated-proc-note plus1)
]

When the @scheme[prop:procedure] value is a procedure, it should
accept at least one argument. When an instance of the structure is
used in an application expression, the property-value procedure is
called with the instance as the first argument. The remaining
arguments to the property-value procedure are the arguments from the
application expression. Thus, if the application expression contained
five arguments, the property-value procedure is called with six
arguments. The name of the instance (see @scheme[object-name]) is
unaffected by the property-value procedure, but the instance's arity
is determined by subtracting one from every possible argument count of
the property-value procedure. If the property-value procedure cannot
accept at least one argument, then the instance behaves like
@scheme[(case-lambda)].

Providing a procedure @scheme[proc-spec] argument to
@scheme[make-struct-type] is the same as supplying the value with the
@scheme[prop:procedure] property (so that a specific property binding
is disallowed).

@examples[
(define-struct fish (weight color)
               #:property 
               prop:procedure  
               (lambda (f n) (set-fish-weight! f (+ n (fish-weight f)))))
(define wanda (make-fish 12 'red))
(fish? wanda)
(procedure? wanda)
(fish-weight wanda)
(for-each wanda '(1 2 3))
(fish-weight wanda)
]

If a structure type generates procedure instances, then subtypes of
the type also generate procedure instances. The instances behave the
same as instances of the original type. When a @scheme[prop:procedure]
property or non-@scheme[#f] @scheme[proc-spec] is supplied to
@scheme[make-struct-type] with a supertype that already behaves as a
procedure, the @exnraise[exn:fail:contract].}

@defproc[(procedure-struct-type? [type struct-type?]) boolean?]{

Returns @scheme[#t] if instances of the structure type represented by
@scheme[type] are procedures (according to @scheme[procedure?]),
@scheme[#f] otherwise.}

