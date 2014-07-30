#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-advanced))

@title[#:tag "advanced"]{Advanced Student}

@declare-exporting[lang/htdp-advanced]

@racketgrammar*+qq[
#:literals (define define-struct define-datatype lambda λ cond else if and or require lib planet
            local let let* letrec time begin begin0 set! delay shared recur when case match unless
             ; match
             _ cons list list* struct vector box
            check-expect check-random check-satisfied check-within check-member-of check-range check-error)
(check-expect check-random check-satisfied check-within check-error check-member-of check-range require)
[program (code:line def-or-expr ...)]
[def-or-expr definition
             expr
             test-case
             library-require]
[definition (define (name variable ...) expr)
            (define name expr)
            (define-struct name (name ...))
            (define-datatype name (name name ...) ...)]
[expr (begin expr expr ...)
      (begin0 expr expr ...)
      (set! variable expr)
      (delay expr)
      (lambda (variable ...) expr)
      (λ (variable ...) expr)
      (local [definition ...] expr)
      (letrec ([name expr] ...) expr)
      (shared ([name expr] ...) expr)
      (let ([name expr] ...) expr)
      (let name ([name expr] ...) expr)
      (let* ([name expr] ...) expr)
      (recur name ([name expr] ...) expr)
      (code:line (expr expr ...))
      (cond [expr expr] ... [expr expr])
      (cond [expr expr] ... [else expr])
      (case expr [(choice choice ...) expr] ... 
                 [(choice choice ...) expr])
      (case expr [(choice choice ...) expr] ... 
                 [else expr])
      (match expr [pattern expr] ...)
      (if expr expr expr)
      (when expr expr)
      (unless expr expr)
      (and expr expr expr ...)
      (or expr expr expr ...)
      (time expr)
      (code:line name)
      (code:line @#,elem{@racketvalfont{'}@racket[_quoted]})
      (code:line @#,elem{@racketvalfont{`}@racket[_quasiquoted]})
      number
      boolean
      string
      character]
[choice (code:line name)
        number]
[pattern _
         name
         number
         true
         false
         string
         character
         @#,elem{@racketvalfont{'}@racket[_quoted]}
         @#,elem{@racketvalfont{`}@racket[_quasiquoted-pattern]}
         (cons pattern pattern)
         (list pattern ...)
         (list* pattern ...)
         (struct id (pattern ...))
         (vector pattern ...)
         (box pattern)]
[quasiquoted-pattern name
                     number
                     string
                     character
                     (quasiquoted-pattern ...)
                     @#,elem{@racketvalfont{'}@racket[_quasiquoted-pattern]}
                     @#,elem{@racketvalfont{`}@racket[_quasiquoted-pattern]}
                     @#,elem{@racketfont[","]@racket[_pattern]}
                     @#,elem{@racketfont[",@"]@racket[_pattern]}]
]


@prim-nonterms[("advanced") define define-struct]

@prim-variables[("advanced") empty true false .. ... .... ..... ......]

@; ----------------------------------------------------------------------
@section[#:tag "advanced-syntax"]{Syntax for Advanced}

In Advanced, @racket[set!] can be used to mutate variables, and
@racket[define-struct]'s structures are mutatable. @racket[define] and
@racket[lambda] can define functions of zero arguments, and function calls can
invoke functions of zero arguments.


@defform[(lambda (variable ...) expression)]{

Creates a function that takes as many arguments as given @racket[variable]s,
and whose body is @racket[expression].}

@defform[(λ (variable ...) expression)]{

The Greek letter @racket[λ] is a synonym for @racket[lambda].}

@defform/none[(expression expression ...)]{

Calls the function that results from evaluating the first
@racket[expression]. The value of the call is the value of function's body when
every instance of @racket[name]'s variables are replaced by the values of the
corresponding @racket[expression]s.

The function being called must come from either a definition appearing before the
function call, or from a @racket[lambda] expression. The number of argument
@racket[expression]s must be the same as the number of arguments expected by
the function.}

@; ----------------------------------------------------------------------


@defform[(define-datatype dataype-name [variant-name field-name ...] ...)]{

A short-hand for defining a group of related structures. The following @racket[define-datatype]:

@racketblock[
 (define-datatype datatype-name
   [variant-name field-name (unsyntax @racketidfont{...})]
   (unsyntax @racketidfont{...}))
]
is equivalent to:
@racketblock[
 (define ((unsyntax @racket[datatype-name])? x)
   (or ((unsyntax @racket[variant-name])? x) (unsyntax @racketidfont{...})))
 (define-struct variant-name (field-name (unsyntax @racketidfont{...})))
 (unsyntax @racketidfont{...})
]}



@defform[(begin expression expression ...)]{

Evaluates the @racket[expression]s in order from left to right. The value of
the @racket[begin] expression is the value of the last @racket[expression].}



@defform[(begin0 expression expression ...)]{

Evaluates the @racket[expression]s in order from left to right. The value of
the @racket[begin] expression is the value of the first @racket[expression].}



@defform[(set! variable expression)]{

Evaluates @racket[expression], and then mutates the @racket[variable]
to have @racket[expression]'s value. The @racket[variable] must be defined 
by @racket[define], @racket[letrec], @racket[let*], or @racket[let].}


@defform[(delay expression)]{

Produces a ``promise'' to evaluate @racket[expression]. The @racket[expression]
is not evaluated until the promise is forced with @racket[force]; when
the promise is forced, the result is recorded, so that any further
@racket[force] of the promise immediately produces the remembered value.}



@defform[(shared ([name expression] ...) expression)]{

Like @racket[letrec], but when an @racket[expression] next to an @racket[id]
is a @racket[cons], @racket[list], @racket[vector], quasiquoted
expression, or @racketidfont{make-}@racket[_struct-name] from a
@racket[define-struct], the @racket[expression] can refer directly to any
@racket[name], not just @racket[name]s defined earlier. Thus,
@racket[shared] can be used to create cyclic data structures.}


@; ----------------------------------------------------------------------


@defform[(recur name ([name expression] ...) expression)]{

A short-hand syntax for recursive loops. The first @racket[name] corresponds to
the name of the recursive function. The @racket[name]s in the parenthesis are
the function's arguments, and each corresponding @racket[expression] is a
value supplied for that argument in an initial starting call of the
function. The last @racket[expression] is the body of the function.

More precisely, the following @racket[recur]: 

@racketblock[
(recur func-name ([arg-name arg-expression] (unsyntax @racketidfont{...}))
  body-expression)
]

is equivalent to:

@racketblock[
(local [(define (func-name arg-name (unsyntax @racketidfont{...})) body-expression)]
  (func-name arg-expression (unsyntax @racketidfont{...})))
]}


@defform/none[(let name ([name expression] ...) expression)]{

An alternate syntax for @racket[recur].}


@; ----------------------------------------------------------------------


@defform[(case expression [(choice ...) expression] ... [(choice ...) expression])]{

A @racket[case] form contains one or more clauses. Each clause contains a
choices (in parentheses)---either numbers or names---and an answer
@racket[expression]. The initial @racket[expression] is evaluated, and its
value is compared to the choices in each clause, where the lines are considered
in order. The first line that contains a matching choice provides an answer
@racket[expression] whose value is the result of the whole @racket[case]
expression. Numbers match with the numbers in the choices, and symbols match
with the names. If none of the lines contains a matching choice, it is an
error.}

@defform/none[#:literals (case else)
              (case expression [(choice ...) expression] ... [else expression])]{

This form of @racket[case] is similar to the prior one, except that the final
@racket[else] clause is taken if no clause contains a choice matching the value
of the initial @racket[expression].}

@; ----------------------------------------------------------------------


@defform[(match expression [pattern expression] ...)]{

A @racket[match] form contains one or more clauses that are surrounded by
square brackets. Each clause contains a pattern---a description of a value---and
an answer @racket[expression].  The initial @racket[expression] is evaluated,
and its value is matched against the pattern in each clause, where the clauses are
considered in order. The first clause that contains a matching pattern provides
an answer @racket[expression] whose value is the result of the whole
@racket[match] expression. This @racket[expression] may reference identifiers
defined in the matching pattern. If none of the clauses contains a matching
pattern, it is an error.}

@; ----------------------------------------------------------------------


@defform[(when test-expression body-expression)]{

If @racket[test-expression] evaluates to @racket[true], the result of the
@racket[when] expression is the result of evaluating the 
@racket[body-expression], otherwise the result is @racket[(void)] and the 
@racket[body-expression] is not evaluated. If the result of evaluating the
@racket[test-expression] is neither @racket[true] nor @racket[false], it is an
error.}

@defform[(unless test-expression body-expression)]{

Like @racket[when], but the @racket[body-expression] is evaluated when the
@racket[test-expression] produces @racket[false] instead of @racket[true].}


@section[#:tag "advanced-common-syntax"]{Common Syntaxes}

The following syntaxes behave the same in the @emph{Advanced}
level as they did in the @secref["intermediate-lam"] level.


@(intermediate-forms lambda
                     local
                     letrec
                     let*
                     let
                     time
                     define
                     define-struct)


@(define-forms/normal define)

@(prim-forms ("advanced")
             define 
             lambda
             define-struct 
             @{In Advanced, @racket[define-struct] introduces one additional function:
              @itemize[
               @item{@racketidfont{set-}@racket[_structure-name]@racketidfont{-}@racket[_field-name]@racketidfont{!}
                : takes an instance of the structure and a value, and
                mutates the instance's field to the given value.}]}
             define-wish
             cond
             else
             if
             and 
             or
             check-expect
             check-random
	     check-satisfied
             check-within
             check-error
             check-member-of
             check-range
             require
             true false
             #:with-beginner-function-call #f)

@; ----------------------------------------

@section[#:tag "advanced-pre-defined"]{Pre-Defined Functions}

@(require (submod lang/htdp-advanced procedures))
@(render-sections (docs) #'here "htdp-advanced")

@;prim-op-defns['(lib "htdp-advanced.rkt" "lang") #'here '()]
