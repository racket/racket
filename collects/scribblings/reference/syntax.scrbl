#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@define[cvt (schemefont "CVT")]

@title[#:tag "syntax" #:style 'toc]{Core Syntactic Forms}

This section describes the core syntax forms that apear in a fully
expanded expression, plus a many closely-related non-core forms.

@local-table-of-contents[]

@subsubsub*section{Notation}

Each syntactic form is described by a BNF-like notation that describes
a combination of (syntax-wrapped) pairs, symbols, and other data (not
a sequence of characters). These grammatical specifications are shown
as follows:

@specsubform[(#, @schemekeywordfont{some-form} id ...)]

Within such specifications,

@itemize{

 @item{@scheme[...] indicates zero or more
       repetitions of the preceding datum.}

 @item{@scheme[...+] indicates one or
       more repetitions of the preceding datum.}

 @item{italic meta-identifiers play the role of non-terminals; in
       particular,

      @itemize{

        @item{a meta-identifier that ends in @scheme[_id] stands for an
              identifier.}

        @item{a meta-identifier that ends in @scheme[_keyword] stands
              for a keyword.}

        @item{a meta-identifier that ends with @scheme[_expr] stands
              for a sub-form that is expanded as an expression.}

        @item{A meta-identifier that ends with @scheme[_body] stands
              for a sub-form that is expanded in an
              internal-definition context (see
              @secref["intdef-body"]).}

              }} }

@;------------------------------------------------------------------------
@section[#:tag "quote"]{Literals: @scheme[quote] and @scheme[#%datum]}

@guideintro["quote"]{@scheme[quote]}

@defform[(quote datum)]{

Produces a constant value corresponding to @scheme[datum] (i.e., the
actual representation of the program fragment) without its
@tech{lexical information} or source location.

@examples[
(eval:alts (#,(schemekeywordfont "quote") x) 'x)
(eval:alts (#,(schemekeywordfont "quote") (+ 1 2)) '(+ 1 2))
]

}

@defform[(#%datum . datum)]{

Expands to @scheme[(#,(schemekeywordfont "quote") datum)]. See also @secref["expand-steps"]
for information on how the expander introduces @schemeidfont{#%datum}
identifiers.

@examples[
(#%datum . 10)
(#%datum . x)
]
}

@;------------------------------------------------------------------------
@section[#:tag "#%expression"]{Expression Wrapper: @scheme[#%expression]}

@defform[(#%expression expr)]{

Produces the same result as @scheme[expr]. The only use of
@scheme[#%expression] is to force the parsing of a form as an
expression.

@examples[
(#%expression (+ 1 2))
(#%expression (define x 10))
]}

@;------------------------------------------------------------------------
@section[#:tag "#%top"]{Variable References and @scheme[#%top]}

@defform/none[id]{

Refers to a module-level or local binding, when @scheme[id] is
not bound as a transformer (see @secref["expansion"]). At run-time,
the reference evaluates to the value in the location associated with
the binding.

When the expander encounters an @scheme[id] that is not bound by a
module-level or local binding, it converts the expression to @scheme[(#,
@schemeidfont{#%top} . id)] giving @schemeidfont{#%top} the lexical
context of the @scheme[id]; typically, that context refers to
@scheme[#%top]. See also @secref["expand-steps"].

@examples[
(define x 10)
x
(let ([x 5]) x)
((lambda (x) x) 2)
]}

@defform[(#%top . id)]{

Refers to a top-level definition that could bind @scheme[id], even if
@scheme[id] has a local binding in its context. Such references are
disallowed anywhere within a @scheme[module] form.  See also
@secref["expand-steps"] for information on how the expander
introduces @schemeidfont{#%top} identifiers.

@examples[
(define x 12)
(let ([x 5]) (#%top . x))
]}

@;------------------------------------------------------------------------
@section{Locations: @scheme[#%variable-reference]}

@defform*[#:literals (#%top)
          [(#%variable-reference id)
           (#%variable-reference (#%top . id))]]{

Produces an opaque value representing the location of @scheme[id],
which must be bound as a @tech{top-level variable} or
@tech{module-level variable}.

The result is useful only to low-level extensions; see
@secref["inside-mzscheme"].}

@;------------------------------------------------------------------------
@section[#:tag "application"]{Procedure Applications and @scheme[#%app]}

@section-index{evaluation order}

@guideintro["application"]{procedure applications}

@defform/none[(proc-expr arg ...)]{

Applies a procedure, when @scheme[proc-expr] is not an
identifier that has a transformer binding (see
@secref["expansion"]).

More precisely, the expander converts this form to @scheme[(#,
@schemeidfont{#%app} proc-expr arg ...)], giving @schemeidfont{#%app}
the lexical context that is associated with the original form (i.e.,
the pair that combines @scheme[proc-expr] and its
arguments). Typically, the lexical context of the pair indicates the
procedure-application @scheme[#%app] that is described next. See also
@secref["expand-steps"].

@examples[
(+ 1 2)
((lambda (x #:arg y) (list y x)) #:arg 2 1)
]}

@defform[(#%app proc-expr arg ...)]{

Applies a procedure. Each @scheme[arg] is one of the following:

 @specsubform[arg-expr]{The resulting value is a non-keyword
                        argument.}

 @specsubform[(code:line keyword arg-expr)]{The resulting value is a
              keyword argument using @scheme[keyword]. Each
              @scheme[keyword] in the application must be distinct.}

The @scheme[proc-expr] and @scheme[_arg-expr]s are evaluated in order,
left to right. If the result of @scheme[proc-expr] is a procedure that
accepts as many arguments as non-@scheme[_keyword]
@scheme[_arg-expr]s, if it accepts arguments for all of the
@scheme[_keyword]s in the application, and if all required
keyword-based arguments are represented among the @scheme[_keyword]s
in the application, then the procedure is called with the values of
the @scheme[arg-expr]s. Otherwise, the @exnraise[exn:fail:contract].

The continuation of the procedure call is the same as the continuation
of the application expression, so the results of the procedure are the
results of the application expression.

The relative order of @scheme[_keyword]-based arguments matters only
for the order of @scheme[_arg-expr] evaluations; the arguments are
associated with argument variables in the applied procedure based on
the @scheme[_keyword]s, and not their positions. The other
@scheme[_arg-expr] values, in contrast, are associated with variables
according to their order in the application form.

See also @secref["expand-steps"] for information on how the
expander introduces @schemeidfont{#%app} identifiers.

@examples[
(#%app + 1 2)
(#%app (lambda (x #:arg y) (list y x)) #:arg 2 1)
(#%app cons)
]}

@defform[(#%plain-app proc-expr arg-expr ...)]{
Like @scheme[#%app], but without support for keyword arguments.
}

@;------------------------------------------------------------------------
@section[#:tag "lambda"]{Procedure Expressions: @scheme[lambda] and @scheme[case-lambda]}

@guideintro["lambda"]{procedure expressions}

@defform/subs[(lambda gen-formals body ...+)
              ([formals (id ...)
                        (id ...+ . rest-id)
                        rest-id]
               [gen-formals formals
                            (arg ...)
                            (arg ...+ . rest-id)]
               [arg id
                    [id default-expr]
                    (code:line keyword id)
                    (code:line keyword [id default-expr])])]{

Produces a procedure. The @scheme[gen-formals] determines the number of
arguments that the procedure accepts. It is either a simple
@scheme[formals] or one of the extended forms.

A simple @scheme[_formals] has one of the following three forms:

@specsubform[(id ...)]{ The procedure accepts as many non-keyword
       argument values as the number of @scheme[id]s. Each @scheme[id]
       is associated with an argument value by position.}

@specsubform[(id ...+ . rest-id)]{ The procedure accepts any number of
       non-keyword arguments greater or equal to the number of
       @scheme[id]s. When the procedure is applied, the @scheme[id]s
       are associated with argument values by position, and all
       leftover arguments are placed into a list that is associated to
       @scheme[rest-id].}

@specsubform[rest-id]{ The procedure accepts any number of non-keyword
       arguments. All arguments are placed into a list that is
       associated with @scheme[rest-id].}

Besides the three @scheme[formals] forms, a @scheme[gen-formals] can be
a sequence of @scheme[arg]s optionally ending with a
@scheme[rest-id]:

@specsubform[(arg ...)]{ Each @scheme[arg] has the following
       four forms:

        @specsubform[id]{Adds one to both the minimum and maximum
        number of non-keyword arguments accepted by the procedure. The
        @scheme[id] is associated with an actual argument by
        position.}

        @specsubform[[id default-expr]]{Adds one to the maximum number
        of non-keyword arguments accepted by the procedure. The
        @scheme[id] is associated with an actual argument by position,
        and if no such argument is provided, the @scheme[default-expr]
        is evaluated to produce a value associated with @scheme[id].
        No @scheme[arg] with a @scheme[default-expr] can appear
        before an @scheme[id] without a @scheme[default-expr] and
        without a @scheme[keyword].}

       @specsubform[(code:line keyword id)]{The procedure requires a
       keyword-based argument using @scheme[keyword]. The @scheme[id]
       is associated with a keyword-based actual argument using
       @scheme[keyword].}

       @specsubform[(code:line keyword [id default-expr])]{The
       procedure accepts a keyword-based using @scheme[keyword]. The
       @scheme[id] is associated with a keyword-based actual argument
       using @scheme[keyword], if supplied in an application;
       otherwise, the @scheme[default-expr] is evaluated to obtain a
       value to associate with @scheme[id].}

      The position of a @scheme[_keyword] @scheme[arg] in
      @scheme[gen-formals] does not matter, but each specified
      @scheme[_keyword] must be distinct.}

@specsubform[(arg ...+ . rest-id)]{ Like the previous case, but
       the procedure accepts any number of non-keyword arguments
       beyond its minimum number of arguments. When more arguments are
       provided than non-@scheme[_keyword] arguments among the
       @scheme[arg]s, the extra arguments are placed into a
       list that is associated to @scheme[rest-id].}

The @scheme[gen-formals] identifiers are bound in the @scheme[body]s. When
the procedure is applied, a new location is created for each
identifier, and the location is filled with the associated argument
value.

If any identifier appears in the @scheme[body]s that is not one of the
identifiers in @scheme[gen-formals], then it refers to the same location
that it would if it appeared in place of the @scheme[lambda]
expression. (In other words, variable reference is lexically scoped.)

When multiple identifiers appear in a @scheme[gen-formals], they must be
distinct according to @scheme[bound-identifier=?].

If the procedure procedure by @scheme[lambda] is applied to fewer or
more by-position or arguments than it accepts, to by-keyword arguments
that it does not accept, or without required by-keyword arguments, then
the @exnraise[exn:fail:contract].

The last @scheme[body] expression is in tail position with respect to
the procedure body.

@examples[
((lambda (x) x) 10)
((lambda (x y) (list y x)) 1 2)
((lambda (x [y 5]) (list y x)) 1 2)
(let ([f (lambda (x #:arg y) (list y x))])
 (list (f 1 #:arg 2)
       (f #:arg 2 1)))
]}

When compiling a @scheme[lambda] or @scheme[case-lambda] expression,
Scheme looks for a @indexed-scheme['method-arity-error] property
attached to the expression (see @secref["stxprops"]). If it is
present with a true value, and if no case of the procedure accepts
zero arguments, then the procedure is marked so that an
@scheme[exn:fail:contract:arity] exception involving the procedure
will hide the first argument, if one was provided. (Hiding the first
argument is useful when the procedure implements a method, where the
first argument is implicit in the original source). The property
affects only the format of @scheme[exn:fail:contract:arity]
exceptions, not the result of @scheme[procedure-arity].


@defform[(case-lambda [formals body ...+] ...)]{

Produces a procedure. Each @scheme[[forms body ...+]]
clause is analogous to a single @scheme[lambda] procedure; applying
the @scheme[case-lambda]-generated procedure is the same as applying a
procedure that corresponds to one of the clauses---the first procedure
that accepts the given number of arguments. If no corresponding
procedure accepts the given number of arguments, the
@exnraise[exn:fail:contract].

Note that a @scheme[case-lambda] clause supports only
@scheme[formals], not the more general @scheme[_gen-formals] of
@scheme[lambda]. That is, @scheme[case-lambda] does not directly
support keyword and optional arguments.

@examples[
(let ([f (case-lambda
          [() 10]
          [(x) x]
          [(x y) (list y x)]
          [r r])])
  (list (f)
        (f 1)
        (f 1 2)
        (f 1 2 3)))
]}

@defform[(#%plain-lambda formals body ...+)]{
Like @scheme[lambda], but without support for keyword or optional arguments.
}

@;------------------------------------------------------------------------
@section[#:tag "let"]{Local Binding: @scheme[let], @scheme[let*], @scheme[letrec], ...}

@guideintro["let"]{local binding}

@defform*[[(let ([id val-expr] ...) body ...+)
           (let proc-id ([id init-expr] ...) body ...+)]]{

The first form evaluates the @scheme[val-expr]s left-to-right, creates
a new location for each @scheme[id], and places the values into the
locations. It then evaluates the @scheme[body]s, in which the
@scheme[id]s are bound. The last @scheme[body] expression is in
tail position with respect to the @scheme[let] form. The @scheme[id]s
must be distinct according to @scheme[bound-identifier=?].

@examples[
(let ([x 5]) x)
(let ([x 5])
  (let ([x 2]
        [y x])
    (list y x)))
]

The second form evaluates the @scheme[init-expr]s; the resulting
values become arguments in an application of a procedure
@scheme[(lambda (id ...) body ...+)], where @scheme[proc-id] is bound
within the @scheme[body]s to the procedure itself.}

@examples[
(let fac ([n 10])
  (if (zero? n)
      1
      (* n (fac (sub1 n)))))
]

@defform[(let* ([id val-expr] ...) body ...+)]{

Similar to @scheme[let], but evaluates the @scheme[val-expr]s one by
one, creating a location for each @scheme[id] as soon as the value is
available. The @scheme[id]s are bound in the remaining @scheme[val-expr]s
as well as the @scheme[body]s, and the @scheme[id]s need not be
distinct; later bindings shadow earlier bindings.

@examples[
(let ([x 1]
      [y (+ x 1)])
  (list y x))
]}

@defform[(letrec ([id val-expr] ...) body ...+)]{

Similar to @scheme[let], but the locations for all @scheme[id]s are
created first and filled with @|undefined-const|, and all
@scheme[id]s are bound in all @scheme[val-expr]s as well as the
@scheme[body]s. The @scheme[id]s must be distinct according to
@scheme[bound-identifier=?].

@examples[
(letrec ([is-even? (lambda (n)
                     (or (zero? n)
                         (is-odd? (sub1 n))))]
         [is-odd? (lambda (n)
                    (or (= n 1)
                        (is-even? (sub1 n))))])
  (is-odd? 11))
]}

@defform[(let-values ([(id ...) val-expr] ...) body ...+)]{ Like
@scheme[let], except that each @scheme[val-expr] must produce as many
values as corresponding @scheme[id]s, otherwise the
@exnraise[exn:fail:contract]. A separate location is created for each
@scheme[id], all of which are bound in the @scheme[body]s.

@examples[
(let-values ([(x y) (quotient/remainder 10 3)])
  (list y x))
]}

@defform[(let*-values ([(id ...) val-expr] ...) body ...+)]{ Like
@scheme[let*], except that each @scheme[val-expr] must produce as many
values as corresponding @scheme[id]s. A separate location is created
for each @scheme[id], all of which are bound in the later
@scheme[val-expr]s and in the @scheme[body]s.

@examples[
(let*-values ([(x y) (quotient/remainder 10 3)]
              [(z) (list y x)])
  z)
]}

@defform[(letrec-values ([(id ...) val-expr] ...) body ...+)]{ Like
@scheme[letrec], except that each @scheme[val-expr] must produce as
many values as corresponding @scheme[id]s. A separate location is
created for each @scheme[id], all of which are initialized to
@|undefined-const| and bound in all @scheme[val-expr]s
and in the @scheme[body]s.

@examples[
(letrec-values ([(is-even? is-odd?)
                 (values
                   (lambda (n)
                     (or (zero? n)
                         (is-odd? (sub1 n))))
                   (lambda (n)
                     (or (= n 1)
                         (is-even? (sub1 n)))))])
  (is-odd? 11))
]}

@defform[(let-syntax ([id trans-expr] ...) body ...+)]{

Creates a @tech{transformer binding} (see
@secref["transformer-model"]) of each @scheme[id] with the value of
@scheme[trans-expr], which is an expression at @tech{phase level} 1
relative to the surrounding context. (See @secref["id-model"] for
information on @tech{phase levels}.)

Each @scheme[id] is bound in the @scheme[body]s, and not in other
@scheme[trans-expr]s.}

@defform[(letrec-syntax ([id trans-expr] ...) body ...+)]{

Like @scheme[let-syntax], except that each @scheme[id] is also bound
within all @scheme[trans-expr]s.}

@defform[(let-syntaxes ([(id ...) trans-expr] ...) body ...+)]{

Like @scheme[let-syntax], but each @scheme[trans-expr] must produce as
many values as corresponding @scheme[id]s, each of which is bound to
the corresponding value.}

@defform[(letrec-syntaxes ([(id ...) trans-expr] ...) body ...+)]{

Like @scheme[let-syntax], except that each @scheme[id] is also bound
within all @scheme[trans-expr]s.}

@defform[(letrec-syntaxes+values ([(trans-id ...) trans-expr] ...)
                                 ([(val-id ...) val-expr] ...)
            body ...+)]{

Combines @scheme[letrec-syntaxes] with @scheme[letrec-values]: each
@scheme[trans-id] and @scheme[val-id] is bound in all
@scheme[trans-expr]s and @scheme[val-expr]s.}

@;------------------------------------------------------------------------
@section[#:tag "if"]{Conditionals: @scheme[if], @scheme[cond], @scheme[and], and @scheme[or]}

@guideintro["conditionals"]{conditionals}

@defform[(if test-expr then-expr else-expr)]{

Evaluates @scheme[test-expr]. If it produces any value other than
@scheme[#f], then @scheme[then-expr] is evaluated, and its results are
the result for the @scheme[if] form. Otherwise, @scheme[else-expr] is
evaluated, and its results are the result for the @scheme[if]
form. The @scheme[then-expr] and @scheme[else-expr] are in tail
position with respect to the @scheme[if] form.

@examples[
(if (positive? -5) (error "doesn't get here") 2)
(if (positive? 5) 1 (error "doesn't get here"))
]}

@defform/subs[#:literals (else =>)
              (cond cond-clause ...)
              ([cond-clause [test-expr then-expr ...+]
                            [else then-expr ...+]
                            [test-expr => proc-expr]
                            [test-expr]])]{

@guideintro["cond"]{@scheme[cond]}

A @scheme[cond-clause] that starts with @scheme[else] must be the last
@scheme[cond-clause].

If no @scheme[cond-clause]s are present, the result is @|void-const|.

If only a @scheme[[else then-expr ...+]] is present, then the
@scheme[then-expr]s are evaluated. The results from all but the last
@scheme[then-expr] are ignored. The results of the last
@scheme[then-expr], which is in tail position with respect to the
@scheme[cond] form, are the results for the whole @scheme[cond]
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


@defidform[else]{

Recognized special ly withing forms like @scheme[cond]. An
@scheme[else] form as an expression is a syntax error.}


@defform[(and expr ...)]{

@guideintro["and+or"]{@scheme[and]}

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

@guideintro["and+or"]{@scheme[or]}

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
@section[#:tag "case"]{Dispatch: @scheme[case]}

@defform/subs[#:literals (else)
              (case val-expr case-clause ...)
              ([case-clause [(datum ...) then-expr ...+]
                            [else then-expr ...+]])]{

Evaluates @scheme[val-expr] and uses the result to select a
@scheme[case-clause]. The selected clause is the first one with a
@scheme[datum] whose @scheme[quote]d form is @scheme[eqv?] to the
result of @scheme[val-expr]. If no such @scheme[datum] is present, the
@scheme[else] @scheme[case-clause] is selected; if no @scheme[else]
@scheme[case-clause] is present, either, then the result of the
@scheme[case] form is @|void-const|.

For the selected @scheme[case-clause], the results of the last
@scheme[then-expr], which is in tail position with respect to the
@scheme[cond] form, are the results for the whole @scheme[cond] form.

A @scheme[case-clause] that starts with @scheme[else] must be the last
@scheme[case-clause].

@examples[
(case (+ 7 5)
 [(1 2 3) 'small]
 [(10 11 12) 'big])
(case (- 7 5)
 [(1 2 3) 'small]
 [(10 11 12) 'big])
]
@def+int[
(define (classify c)
  (case (char-general-category c)
   [(ll lu lt ln lo) "letter"]
   [(nd nl no) "number"]
   [else "other"]))
(classify #\A)
(classify #\1)
(classify #\!)
]}

@;------------------------------------------------------------------------
@section[#:tag "define"]{Definitions: @scheme[define], @scheme[define-syntax], ...}

@guideintro["define"]{definitions}

@defform*/subs[[(define id expr)
                (define (head args) body ...+)]
                ([head id
                       (head args)]
                 [args (code:line arg ...)
                       (code:line arg ... #, @schemeparenfont{.} rest-id)]
                 [arg arg-id
                      [arg-id default-expr]
                      (code:line keyword arg-id)
                      (code:line keyword [arg-id default-expr])])]{

The first form @tech{bind}s @scheme[id] to the result of
@scheme[expr], and the second form @tech{bind}s @scheme[id] to a
procedure. In the second case, the generation procedure is
@scheme[(#,cvt (head args) body ...+)], using the @|cvt| meta-function
defined as follows:

@schemeblock[
(#,cvt (id . _gen-formals) . _datum)   = (lambda _gen-formals . _datum)
(#,cvt (head . _gen-formals) . _datum) = (lambda _gen-formals expr)
                                         #, @elem{if} (#,cvt head . _datum) = expr
]

At the top level, the top-level binding @scheme[id] is created after
evaluating @scheme[expr], if it does not exist already, and the
top-level mapping of @scheme[id] (in the @techlink{namespace} linked
with the compiled definition) is set to the binding at the same time.

@defexamples[
(define x 10)
x
]
@def+int[
(define (f x)
  (+ x 1))
(f 10)
]

@def+int[
(define ((f x) [y 20])
  (+ x y))
((f 10) 30)
((f 10))
]
}

@defform[(define-values (id ...) expr)]{

Evaluates the @scheme[expr], and @tech{bind}s the results to the
@scheme[id]s, in order, if the number of results matches the number of
@scheme[id]s; if @scheme[expr] produces a different number of results,
the @exnraise[exn:fail:contract].

At the top level, the top-level binding for each @scheme[id] is
created after evaluating @scheme[expr], if it does not exist already,
and the top-level mapping of each @scheme[id] (in the
@techlink{namespace} linked with the compiled definition) is set to
the binding at the same time.

@defexamples[
(define-values () (values))
(define-values (x y z) (values 1 2 3))
z
]
}


@defform*[[(define-syntax id expr)
           (define-syntax (head args) body ...+)]]{

The first form creates a @tech{transformer binding} (see
@secref["transformer-model"]) of @scheme[id] with the value of
@scheme[expr], which is an expression at @tech{phase level} 1 relative
to the surrounding context. (See @secref["id-model"] for
information on @tech{phase levels}.)

The second form is a shorthand the same as for @scheme[define]; it
expands to a definition of the first form where the @scheme[expr] is a
@scheme[lambda] form.}


@defform[(define-syntaxes (id ...) expr)]{

Like @scheme[define-syntax], but creates a @tech{transformer binding}
for each @scheme[id].  The @scheme[expr] should produce as many values
as @scheme[id]s, and each value is bound to the corresponding
@scheme[id].}


@defform*[[(define-for-syntax id expr)
           (define-for-syntax (head args) body ...+)]]{

Like @scheme[define], except that the binding is at @tech{phase level}
1 instead of @tech{phase level} 0 relative to its context. The
expression for the binding is also at @tech{phase level} 1. (See
@secref["id-model"] for information on @tech{phase levels}.)}

@defform[(define-values-for-syntax (id ...) expr)]{

Like @scheme[define-for-syntax], but @scheme[expr] must produce as
many value as supplied @scheme[id]s, and all of the @scheme[id]s are
bound (at @tech{phase level} 1).}

@;------------------------------------------------------------------------
@section[#:tag "begin"]{Sequencing: @scheme[begin], @scheme[begin0], and @scheme[begin-for-syntax]}

@guideintro["begin"]{@scheme[begin] and @scheme[begin0]}

@defform*[[(begin form ...)
           (begin expr ...+)]]{

The first form applies when @scheme[begin] appears at the top level,
at module level, or in an internal-definition position (before any
expression in the internal-definition sequence). In that case, the
@scheme[begin] form is equivalent to splicing the @scheme[form]s into
the enclosing context.

The second form applies for @scheme[begin] in an expression position.
In that case, the @scheme[expr]s are evaluated in order, and the
results are ignored for all but the last @scheme[expr]. The last
@scheme[expr] is in tail position with respect to the @scheme[begin]
form.

@examples[
(begin
  (define x 10)
  x)
(+ 1 (begin
       (printf "hi\n")
       2))
(let-values ([(x y) (begin
                      (values 1 2 3)
                      (values 1 2))])
 (list x y))
]}

@defform[(begin0 expr body ...+)]{

Evaluates the @scheme[expr], then evaluates the @scheme[body]s,
ignoring the @scheme[body] results. The results of the @scheme[expr]
are the results of the @scheme[begin0] form, but the @scheme[expr] is
in tail position only if no @scheme[body]s are present.

@examples[
(begin0
  (values 1 2)
  (printf "hi\n"))
]}

@defform[(begin-for-syntax form ...)]{

Allowed only in a @tech{top-level context} or @tech{module context}.
Each @scheme[form] is partially expanded (see
@secref["partial-expansion"]) to determine one of the following
classifications:

@itemize{

 @item{@scheme[define] or @scheme[define-values] form: converted to
       a @scheme[define-for-syntax] form.}

 @item{@scheme[require] form: converted to a
       @scheme[require-for-syntax] form.}

 @item{@scheme[require-for-template] form: converted to a
       @scheme[require].}

 @item{expression form @scheme[_expr]: converted to
       @scheme[(define-values () (begin _expr (values)))], which
       effectively evaluates the expression at expansion time and, in
       the case of a @tech{module context}, preserves the expression
       for future @tech{visit}s of the module.}

}

}

@;------------------------------------------------------------------------
@section[#:tag "when+unless"]{Guarded Evaluation: @scheme[when] and @scheme[unless]}

@guideintro["when+unless"]{@scheme[when] and @scheme[unless]}

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
@section[#:tag "set!"]{Assignment: @scheme[set!] and @scheme[set!-values]}

@guideintro["set!"]{@scheme[set!]}

@defform[(set! id expr)]{

If @scheme[id] has a @tech{transformer binding} to an
@tech{assignment transformer}, as produced by
@scheme[make-set!-transformer], then this form is expanded by calling
the assignment transformer with the full expressions. If @scheme[id]
has a @tech{transformer binding} to a @tech{rename transformer} as
produced by @scheme[make-rename-transformer], then this form is
expanded by replacing @scheme[id] with the one provided to
@scheme[make-rename-transformer].

Otherwise, evaluates @scheme[expr] and installs the result into the
location for @scheme[id], which must be bound as a local variable or
defined as a @tech{top-level variable} or @tech{module-level
variable}. If @scheme[id] refers to an imported binding, a syntax
error is reported.  If @scheme[id] refers to a @tech{top-level
variable} that has not been defined, the @exnraise[exn:fail:contract].

@defexamples[
(define x 12)
(set! x (add1 x))
x
(let ([x 5])
  (set! x (add1 x))
  x)
(set! i-am-not-defined 10)
]}

@defform[(set!-values (id ...) expr)]{

Assuming that all @scheme[id]s refer to variables, this form evaluates
@scheme[expr], which must produce as many values as supplied
@scheme[id]s.  The location of each @scheme[id] is filled wih to the
corresponding value from @scheme[expr] in the same way as for
@scheme[set!].

@examples[
(let ([a 1]
      [b 2])
  (set!-values (a b) (values b a))
  (list a b))
]

More generally, the @scheme[set!-values] form is expanded to

@schemeblock[
(let-values ([(_tmp-id ...) expr])
  (set! id _tmp-id) ...)
]

which triggers further expansion if any @scheme[id] has a transformer
binding to an @tech{assignment transformer}.}

@;------------------------------------------------------------------------
@include-section["for.scrbl"]

@;------------------------------------------------------------------------
@section[#:tag "wcm"]{Continuation Marks: @scheme[with-continuation-mark]}

@defform[(with-continuation-mark key-expr val-expr result-expr)]{

The @scheme[key-expr], @scheme[mark-expr], and @scheme[result-expr]
expressions are evaluated in order. After @scheme[key-expr] is
evaluated to obtain a key and @scheme[mark-expr] is evaluated to
obtain a mark, the key is mapped to the mark in the current
continuation's initial frame. If the frame already has a mark for the
key, it is replaced. Finally, the @scheme[result-expr] is evaluated;
the continuation for evaluating @scheme[result-expr] is the
continuation of the @scheme[with-continuation-mark] expression (so the
result of the @scheme[resultbody-expr] is the result of the
@scheme[with-continuation-mark] expression, and @scheme[result-expr]
is in tail position for the @scheme[with-continuation-mark]
expression).

@moreref["contmarks"]{continuation marks}}

@;------------------------------------------------------------------------
@section[#:tag "quasiquote"]{Quasiquoting: @scheme[quasiquote], @scheme[unquote], and @scheme[unquote-splicing]}

@defform[(quasiquote datum)]{

The same as @scheme[(quote datum)] if @scheme[datum] does not include
@scheme[(unquote _expr)] or @scheme[(unquote-splicing _expr)]. An
@scheme[(unquote _expr)] expression escapes from the quote, however,
and the result of the @scheme[_expr] takes the place of the
@scheme[(unquote _expr)] form in the @scheme[quasiquote] result. An
@scheme[(unquote-splicing _expr)] similarly escapes, but the
@scheme[_expr] must produce a list, and its elements are spliced as
multiple values place of the @scheme[(unquote-splicing _expr)], which
must appear as the @scheme[car] or a quoted pair.

@examples[
(eval:alts (#,(scheme quasiquote) (0 1 2)) `(0 1 2))
(eval:alts (#,(scheme quasiquote) (0 (#,(scheme unquote) (+ 1 2)) 4)) `(1 ,(+ 1 2) 4))
(eval:alts (#,(scheme quasiquote) (0 (#,(scheme unquote-splicing) (list 1 2)) 4)) `(1 ,@(list 1 2) 4))
]

A @scheme[quasiquote], @scheme[unquote], or @scheme[unquote-splicing]
form is typically abbreviated with @litchar{`}, @litchar{,}, or
@litchar[",@"], respectively. See also @secref["parse-quote"].

@examples[
`(0 1 2)
`(1 ,(+ 1 2) 4)
`(1 ,@(list 1 2) 4)
]

A @scheme[quasiquote] form within the original @scheme[datum]
increments the level of quasiquotation: within the @scheme[quasiquote]
form, each @scheme[unquote] or @scheme[unquote-splicing] is preserved,
but a further nested @scheme[unquote] or @scheme[unquote-splicing]
escapes.  Multiple nestings of @scheme[quasiquote] require multiple
nestings of @scheme[unquote] or @scheme[unquote-splicing] to escape.

@examples[
`(1 `,(+ 1 ,(+ 2 3)) 4)
`(1 ```,,@,,@(list (+ 1 2)) 4)
]

The @scheme[quasiquote] form allocates only as many fresh cons cells,
vectors, and boxes as are needed without analyzing @scheme[unquote]
and @scheme[unquote-splicing] expressions. For example, in

@schemeblock[
`(,1 2 3)
]

a single tail @scheme['(2 3)] is used for every evaluation of the
@scheme[quasiquote] expression.

}

@defidform[unquote]{

See @scheme[quasiquote], where @scheme[unquote] is recognized as an
escape. An @scheme[unquote] form as an expression is a syntax error.}

@defidform[unquote-splicing]{

See @scheme[quasiquote], where @scheme[unquote-splicing] is recognized as an
escape. An @scheme[unquote-splicing] form as an expression is a syntax error.}

@;------------------------------------------------------------------------
@section{Syntax Quoting: @scheme[quote-syntax]}

@defform[(quote-syntax datum)]{

Produces a @tech{syntax object} that preserves the @tech{lexical
information} and source-location information attached to
@scheme[datum] at expansion time.

@examples[
(syntax? (quote-syntax x))
]
}

@;------------------------------------------------------------------------
@section[#:tag "module"]{Modules: @scheme[module], ...}

@defform[(module id require-spec form ...)]{

Declares a module named by combining @scheme[(#,(scheme quote) id)]
with @scheme[(current-module-name-prefix)] if the latter is not
@scheme[#f], or named @scheme[(#,(scheme quote) id)] otherwise.

The @scheme[require-spec] must be as for @scheme[require], and it
supplies the initial bindings for the body @scheme[form]s. That is, it
is treated like a @scheme[(require require-spec)] prefix on
@scheme[form], where @scheme[require] is the preimitive
@scheme[require] form.

If a single @scheme[form] is provided, then it is partially expanded
in a @tech{module-begin context}. If the expansion leads to
@scheme[#%module-begin], then the body of the @scheme[#%module-begin]
is the body of the module. If partial expansion leads to any other
primitive form, then the form is wrapped with
@schemeidfont{#%module-begin} using the lexical context of the module
body; this identifier must be bound by the initial
@scheme[require-spec] import, and its expansion must produce a
@scheme[#%module-begin] to supply the module body. Finally, if
multiple @scheme[form]s are provided, they are wrapped with
@schemeidfont{#%module-begin}, as in the case where a single
@scheme[form] does not expand to @scheme[#%module-begin].

After such wrapping, if any, and before any expansion, an
@indexed-scheme['enclosing-module-name] property is attached to the
@schemeidfont{#%module-begin} syntax object (see
@secref["stxprops"]); the property's value is a symbol
corresponding to @scheme[id].

Each @scheme[form] is partially expanded (see
@secref["partial-expansion"]) in a @tech{module context}. Further
action depends on the shape of the form:

@itemize{

 @item{If it is a @scheme[begin] form, so the sub-forms are flattened
  out into the module's body and immediately processed in place of the
  @scheme[begin].}

 @item{If it is a @scheme[define-syntaxes] or
  @scheme[define-values-for-syntax] form, then the right-hand side is
  evaluated (in @tech{phase} 1), and the binding is immediately
  installed for further partial expansion within the module.}

 @item{If the form is a @scheme[require], @scheme[require-for-syntax],
   or @scheme[require-for-template] form, bindings are introduced
   immediately, and the imported modules are @tech{instantiate}d or
   @tech{visit}ed as appropriate.}

 @item{If the form is a @scheme[provide] or
   @scheme[provide-for-syntax] form, then it is recorded for
   processing after the rest of the body.}

 @item{If the form is a @scheme[define-values] form, then the binding
   is installed immediately, but the right-hand expression is not
   expanded further.}

 @item{Similarly, if the form is an expression, it is
   not expanded further.}

}

After all @scheme[form]s have been partially expanded this way, then
the remaining expression forms (including those on the right-hand side
of a definition) are expanded in an expression context.

The scope of all imported identifiers covers the entire module body,
as does the scope of any identifier defined within the module body.
The ordering of syntax definitions does not affect the scope of the
syntax names; a transformer for @scheme[A] can produce expressions
containing @scheme[B], while the transformer for @scheme[B] produces
expressions containing @scheme[A], regardless of the order of
declarations for @scheme[A] and @scheme[B]. However, a syntactic form
that produces syntax definitions must be defined before it is used.

No identifier can be imported or defined more than once at any
@tech{phase level}. Every exported identifier must be imported or
defined. No expression can refer to a @tech{top-level variable}.

The evaluation of a @scheme[module] form does not evaluate the
expressions in the body of the module. Evaluation merely declares a
module, whose full name depends both on @scheme[id] and
@scheme[(current-module-name-prefix)].

The module body is executed only when the module is explicitly
@techlink{instantiate}d via @scheme[require],
@scheme[require-for-syntax], @scheme[require-for-template], or
@scheme[dynamic-require]. On invocation, expressions and definitions
are evaluated in order as they appear within the module; accessing a
@tech{module-level variable} before it is defined signals a run-time
error, just like accessing an undefined global variable.

See also @secref["module-eval-model"] and @secref["mod-parse"].}

@defform[(#%module-begin form ...)]{

Legal only in a @tech{module begin context}, and handled by the
@scheme[module] form.}

@;------------------------------------------------------------------------
@section[#:tag "require"]{Importing: @scheme[require], @scheme[require-for-syntax], @scheme[require-for-template]}

@section-index["modules" "imports"]

@defform/subs[#:literals (only only-rename prefix except rename lib file planet)
              (require require-spec ...)
              ([require-spec module-path
                             (only require-spec id-maybe-renamed ...)
                             (except require-spec id ...)
                             (prefix require-spec prefix-id)
                             (rename require-spec [orig-id bind-id] ...)]
               [module-path id
                            rel-string
                            (lib rel-string)
                            (file string)
                            (planet rel-string (user-string pkg-string vers ...))]
               [id-maybe-renamed id
                                 [orig-id bind-id]]
               [vers nat
                     (nat nat)
                     (= nat)
                     (+ nat)
                     (- nat)])]{

In a @tech{top-level context}, @scheme[require] instantiates modules
(see @secref["module-eval-model"]). In a @tech{module context},
@scheme[require] @tech{visits} modules (see @secref["mod-parse"]). In
both contexts, @scheme[require] introduces bindings into a
@tech{namespace} or a module (see @secref["intro-binding"]). A
@scheme[require] form in a @tech{expression context} or
@tech{internal-definition context} is a syntax error.

A @scheme[require-spec] designates a particular set of identifiers to
be bound in the importing context. Each identifier is mapped to a
particular export of a particular module; the identifier to bind may
be different from the symbolic name of the originally exported
identifier.

 @specsubform[module-path]{ Imports all exported bindings from the
  named module, using the export identifiers as the local identifiers.
  (See below for information on @scheme[module-path].)}

 @specsubform[#:literals (only) (only require-spec id-maybe-renamed ...)]{
  Like @scheme[require-spec], but constrained to those exports for
  which the identifiers to bind match @scheme[id-maybe-renamed]: as
  @scheme[id] or as @scheme[orig-id] in @scheme[[orig-id bind-id]]. If
  the @scheme[id] of @scheme[orig-id] of any @scheme[id-maybe-renamed]
  is not in the set that @scheme[require-spec] describes, a syntax
  error is reported.}

 @specsubform[#:literals (except) (except require-spec id ...)]{ Like
  @scheme[require-spec], but omitting those exports for which
  @scheme[id]s are the identifiers to bind; if any @scheme[id] is not
  in the set that @scheme[require-spec] describes, a syntax error is
  reported.}

 @specsubform[#:literals (prefix) (prefix require-spec prefix-id)]{
  Like @scheme[require-spec], but adjusting each identifier to be bound
  by prefixing it with @scheme[prefix-id].}

 @specsubform[#:literals (rename)
              (rename require-spec [orig-id bind-id] ...)]{
  Like @scheme[require-spec], but replacing the identifier to
  bind @scheme[orig-id] with @scheme[bind-id]; if any
  @scheme[orig-id] is not in the set that @scheme[require-spec]
  describes, a syntax error is reported.}

A @scheme[module-path] identifies a module, either through a concrete
name in the form of an identifier, or through an indirect name that
can trigger automatic loading of the module declaration. Except for
the @scheme[id] case below, the actual resolution is up to the current
@tech{module name resolver} (see
@scheme[current-module-name-resolver]), and the description below
corresponds to the default @tech{module name resolver}.

 @specsubform[id]{ Refers to a module previously declared with the name
  @scheme[id].}

 @specsubform[rel-string]{A path relative to the containing source (as
 determined by @scheme[current-load-relative-directory] or
 @scheme[current-directory]).  Regardless of the current platform,
 @scheme[rel-string] is always parsed as a Unix-format relative path:
 @litchar{/} is the path delimiter (multiple adjacent @litchar{/}s are
 treated as a single delimiter), @litchar{..} accesses the parent
 directory, and @litchar{.} accesses the current directory. The path
 cannot be empty or contain a leading or trailing slash.}

 @specsubform[#:literals (lib) (lib rel-string)]{Like the plain
 @scheme[rel-string] case, but @scheme[rel-string] must contain at
 least two path elements. All path elements up to the last one form a
 @tech{collection} path, which is used to locate the relevant
 directory (not relative to the containing source), and the last path
 element refers to a file. See @secref["collects"] for more
 information.}

 @specsubform[#:literals (file) (file string)]{Similar to the plain
 @scheme[rel-string] case, but @scheme[string] is a path (possibly
 absolute) using the current platform's path conventions.}

 @specsubform[#:literals(planet)
              (planet rel-string (user-string pkg-string vers ...))]{
 Specifies a library available via the @PLaneT server.}

No identifier can be bound multiple times in a given @tech{phase
level} by an import, unless all of the bindings refer to the same
original definition in the same module.  In a @tech{module context},
an identifier can be either imported or defined for a given
@tech{phase level}, but not both.}

@defform[(require-for-syntax require-spec ...)]{
Like @scheme[require], but @tech{instantiate}s a module at phase 1
(see @secref["module-eval-model"]) in a @tech{top-level context} or
@tech{module context}, and introduces bindings at phase level 1 (see
@secref["intro-binding"] and @secref["mod-parse"]).
}

@defform[(require-for-template require-spec ...)]{ Like
@scheme[require], but without @tech{instantiation} (see
@secref["module-eval-model"]) in a @tech{top-level context}, and
introduces bindings at phase level -1 (see @secref["intro-binding"]
and @secref["mod-parse"]).
}

@defform[(require-for-label require-spec ...)]{ Like @scheme[require],
but without implying @tech{instantiation} (see
@secref["module-eval-model"]) at all, and introducing bindings only in
the @tech{label phase level}. Such bindings are accessible via
@scheme[identifier-label-binding] and
@scheme[free-label-identifier=?].}

@;------------------------------------------------------------------------
@section[#:tag "provide"]{Exporting: @scheme[provide] and @scheme[provide-for-syntax]}

@section-index["modules" "exports"]

@defform/subs[#:literals (protect all-defined all-from rename except prefix)
              (provide protected-provide-spec ...)
              ([protected-provide-spec provide-spec
                                       (protect provide-spec)]
               [provide-spec id
                             (all-defined)
                             (all-from module-path)
                             (rename [orig-id export-id] ...)
                             (except provide-spec id ...)
                             (prefix provide-spec prefix-id)])]{

Declares exports from a module. A @scheme[provide] form must appear in
a @tech{module context} or a @tech{module-begin context}.

A @scheme[provide-spec] indicates one or more bindings to provide,
specifying for each an export symbol that can be different from
the symbolic form of the identifier bound within the module:

 @specsubform[id]{ Exports @scheme[id], which must be @tech{bound}
 within the module (i.e., either defined or imported), at @tech{phase
 level} 0 using the symbolic form of @scheme[id] as the external
 name.}

 @specsubform[#:literals (all-defined) (all-defined)]{ Exports all
 identifiers that are defined at @tech{phase level} 0 within the
 exporting module. The external name for each identifier is the
 symbolic form of the identifier; note that this can lead to an
 illegal multiple export for a single symbolic name if different
 defined identifiers use the same symbolic name.}

 @specsubform[#:literals (all-from) (all-from module-path)]{ Exports
 all identifiers that are imported into the exporting module at
 @tech{phase level} 0 using a @scheme[require-spec] built on
 @scheme[module-path] (see @secref["require"]). The symbolic name
 for export is derived from the name that is bound within the module,
 as opposed to the symbolic name of the export from
 @scheme[module-path].}

 @specsubform[#:literals (rename) (rename [orig-id export-id] ...)]{
 Exports each @scheme[orig-id], which must be @tech{bound} within the
 module at @tech{phase level} 0.  The symbolic name for each export is
 @scheme[export-id] instead @scheme[orig-d].}

 @specsubform[#:literals (except) (except provide-spec id ...)]{ Like
 @scheme[provide-spec], but omitting the export of each binding with
 external name @scheme[id]. If @scheme[id] is not specified as an
 export by @scheme[provide-spec], a syntax error is reported.}

 @specsubform[#:literals (prefix) (prefix provide-spec prefix-id)]{
 Like @scheme[provide-spec], but with each symbolic export name from
 @scheme[provide-spec] prefixed with @scheme[prefix-id].}

If @scheme[provide] wraps a @scheme[provide-spec], then the exports of
the @scheme[provide-spec] are protected; see
@secref["modprotect"]. The @scheme[provide-spec] must
specify only bindings that are defined within the exporting module.

Each export specified within a module must have a distinct symbolic
export name, though the same binding can be specified with the
multiple symbolic names.}

@defform[(provide-for-syntax protected-provide-spec ...)]{Like
@scheme[provide], but for @tech{phase level} 1.}

@defform[(provide-for-label protected-provide-spec ...)]{Like
@scheme[provide], but for the @tech{label phase level}.}
