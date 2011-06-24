#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-advanced))

@(define-syntax-rule (bdl intm-define intm-lambda)
   (begin
    (require (for-label lang/htdp-intermediate-lambda))
    (define intm-define (scheme define))
    (define intm-lambda (scheme lambda))))
@(bdl intm-define intm-lambda)

@(define-syntax-rule (bd intm-define-struct intm-local intm-letrec intm-let intm-let* intm-time)
   (begin
    (require (for-label lang/htdp-intermediate))
    (define intm-define (scheme define))
    (define intm-define-struct (scheme define-struct))
    (define intm-local (scheme local))
    (define intm-letrec (scheme letrec))
    (define intm-let (scheme let))
    (define intm-let* (scheme let*))
    (define intm-time (scheme time))))
@(bd intm-define-struct intm-local intm-letrec intm-let intm-let* intm-time)

@(define-syntax-rule (bbd beg-define beg-define-struct beg-cond beg-if beg-and beg-or beg-check-expect beg-require)
   (begin
    (require (for-label lang/htdp-beginner))
    (define beg-define (scheme define))
    (define beg-define-struct (scheme define-struct))
    (define beg-cond (scheme cond))
    (define beg-if (scheme if))
    (define beg-and (scheme and))
    (define beg-or (scheme or))
    (define beg-check-expect (scheme check-expect))
    (define beg-require (scheme require))))
@(bbd beg-define beg-define-struct beg-cond beg-if beg-and beg-or beg-check-expect beg-require)


@title[#:style 'toc #:tag "advanced"]{Advanced Student}

@declare-exporting[lang/htdp-advanced]

@racketgrammar*+qq[
#:literals (define define-struct define-datatype lambda λ cond else if and or empty true false require lib planet
            local let let* letrec time begin begin0 set! delay shared recur when case match unless
             ; match
             _ cons list list* struct vector box
            check-expect check-within check-error)
(check-expect check-within check-error require)
[program (code:line def-or-expr ...)]
[def-or-expr definition
             expr
             test-case
             library-require]
[definition (define (id id id ...) expr)
            (define id expr)
            (define-struct id (id ...))
            (define-datatype id (id id ...) ...)]
[expr (begin expr expr ...)
      (begin0 expr expr ...)
      (set! id expr)
      (delay expr)
      (lambda (id ...) expr)
      (λ (id ...) expr)
      (local [definition ...] expr)
      (letrec ([id expr] ...) expr)
      (shared ([id expr] ...) expr)
      (let ([id expr] ...) expr)
      (let id ([id expr] ...) expr)
      (let* ([id expr] ...) expr)
      (recur id ([id expr] ...) expr)
      (code:line (expr expr ...) (code:comment @#,seclink["advanced-call"]{function call}))
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
      empty
      (code:line id (code:comment @#,seclink["intermediate-id"]{identifier}))
      (code:line prim-op (code:comment @#,seclink["advanced-prim-ops"]{primitive operation}))
      (code:line @#,elem{@racketvalfont{'}@racket[_quoted]} (code:comment @#,seclink["beginner-abbr-quote"]{quoted value}))
      (code:line @#,elem{@racketvalfont{`}@racket[_quasiquoted]} (code:comment @#,seclink["beginner-abbr-quasiquote"]{quasiquote}))
      number
      true
      
      false
      string
      character]
[choice (code:line id (code:comment @#,t{treated as a symbol}))
        number]
[pattern _
         empty
         id
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
[quasiquoted-pattern id
                     number
                     string
                     character
                     (quasiquoted-pattern ...)
                     @#,elem{@racketvalfont{'}@racket[_quasiquoted-pattern]}
                     @#,elem{@racketvalfont{`}@racket[_quasiquoted-pattern]}
                     @#,elem{@racketfont[","]@racket[_pattern]}
                     @#,elem{@racketfont[",@"]@racket[_pattern]}]
]

@|prim-nonterms|

@prim-ops['(lib "htdp-advanced.rkt" "lang") #'here]

@; ----------------------------------------------------------------------

@section[#:tag "advanced-define"]{@racket[define]}

@deftogether[(
@defform[(define (id id ...) expr)]
@defform/none[#:literals (define)
              (define id expr)]
)]{

The same as Intermediate with Lambda's @|intm-define|, except that a
function is allowed to accept zero arguments.}

@; ----------------------------------------------------------------------

@section[#:tag "advanced-define-struct"]{@racket[define-struct]}

@defform[(define-struct structid (fieldid ...))]{

The same as Intermediate's @|intm-define-struct|, but defines an
additional set of operations:

@itemize[

 @item{@racketidfont{set-}@racket[structid]@racketidfont{-}@racket[fieldid]@racketidfont{!}
       : takes an instance of the structure and a value, and changes
       the instance's field to the given value.}

]}

@; ----------------------------------------------------------------------

@section[#:tag "advanced-define-datatype"]{@racket[define-datatype]}

@defform[(define-datatype datatypeid [variantid fieldid ...] ...)]{

A short-hand for defining a group of related structures. A @racket[define-datatype] form
@racketblock[
 (define-datatype datatypeid
   [variantid fieldid (unsyntax @racketidfont{...})]
   (unsyntax @racketidfont{...}))
]
is equivalent to
@racketblock[
 (define ((unsyntax @racket[datatypeid])? x)
   (or ((unsyntax @racket[variantid])? x) (unsyntax @racketidfont{...})))
 (define-struct variantid (fieldid (unsyntax @racketidfont{...})))
 (unsyntax @racketidfont{...})
]}

@; ----------------------------------------------------------------------

@section[#:tag "advanced-lambda"]{@racket[lambda]}

@deftogether[(
@defform[(lambda (id ...) expr)]
@defform[(λ (id ...) expr)]
)]{

The same as Intermediate with Lambda's @|intm-lambda|, except that a
function is allowed to accept zero arguments.}

@; ----------------------------------------------------------------------

@section[#:tag "advanced-call"]{Function Calls}

@defform/none[(expr expr ...)]{

A function call in Advanced is the same as an Intermediate with Lambda
@seclink["intermediate-lambda-call"]{function call}, except that zero
arguments are allowed.}

@defform[(#%app expr expr ...)]{

A function call can be written with @racket[#%app], though it's
practically never written that way.}

@; ----------------------------------------------------------------------

@section{@racket[begin]}

@defform[(begin expr expr ...)]{

Evaluates the @racket[expr]s in order from left to right. The value of
the @racket[begin] expression is the value of the last @racket[expr].}

@; ----------------------------------------------------------------------

@section{@racket[begin0]}

@defform[(begin0 expr expr ...)]{

Evaluates the @racket[expr]s in order from left to right. The value of
the @racket[begin] expression is the value of the first @racket[expr].}

@; ----------------------------------------------------------------------

@section{@racket[set!]}

@defform[(set! id expr)]{

Evaluates @racket[expr], and then changes the definition @racket[id]
to have @racket[expr]'s value. The @racket[id] must be defined or
bound by @racket[letrec], @racket[let], or @racket[let*].}

@; ----------------------------------------------------------------------

@section{@racket[delay]}

@defform[(delay expr)]{

Produces a ``promise'' to evaluate @racket[expr]. The @racket[expr] is
not evaluated until the promise is forced through the @racket[force]
operator; when the promise is forced, the result is recorded, so that
any further @racket[force] of the promise always produces the
remembered value.}

@; ----------------------------------------------------------------------

@section{@racket[shared]}

@defform[(shared ([id expr] ...) expr)]{

Like @racket[letrec], but when an @racket[expr] next to an @racket[id]
is a @racket[cons], @racket[list], @racket[vector], quasiquoted
expression, or @racketidfont{make-}@racket[_structid] from a
@racket[define-struct], the @racket[expr] can refer directly to any
@racket[id], not just @racket[id]s defined earlier. Thus,
@racket[shared] can be used to create cyclic data structures.}

@; ----------------------------------------------------------------------

@section[#:tag "advanced-let"]{@racket[let]}

@defform*[[(let ([id expr] ...) expr)
           (let id ([id expr] ...) expr)]]{

The first form of @racket[let] is the same as Intermediate's
@|intm-let|.

The second form is equivalent to a @racket[recur] form.}


@; ----------------------------------------------------------------------

@section{@racket[recur]}

@defform[(recur id ([id expr] ...) expr)]{

A short-hand recursion construct. The first @racket[id] corresponds to
the name of the recursive function. The parenthesized @racket[id]s are
the function's arguments, and each corresponding @racket[expr] is a
value supplied for that argument in an initial starting call of the
function. The last @racket[expr] is the body of the function.

More precisely, a @racket[recur] form 

@racketblock[
(recur func-id ([arg-id arg-expr] (unsyntax @racketidfont{...}))
  body-expr)
]

is equivalent to

@racketblock[
((local [(define (func-id arg-id (unsyntax @racketidfont{...}))
           body-expr)]
   func-id)
  arg-expr (unsyntax @racketidfont{...}))
]}

@; ----------------------------------------------------------------------

@section{@racket[case]}

@defform[(case expr [(choice ...) expr] ... [(choice ...) expr])]{

A @racket[case] form contains one or more ``lines'' that are
surrounded by parentheses or square brackets. Each line contains a
sequence of choices---numbers and names for symbols---and an answer
@racket[expr]. The initial @racket[expr] is evaluated, and the
resulting value is compared to the choices in each line, where the
lines are considered in order. The first line that contains a matching
choice provides an answer @racket[expr] whose value is the result of
the whole @racket[case] expression. If none of the lines contains a
matching choice, it is an error.}

@defform/none[#:literals (case else)
              (case expr [(choice ...) expr] ... [else expr])]{

This form of @racket[case] is similar to the prior one, except that
the final @racket[else] clause is always taken if no prior line
contains a choice matching the value of the initial @racket[expr]. In
other words, so there is no possibility to ``fall off the end'' of
the @racket[case] form.}

@; ----------------------------------------------------------------------

@section{@racket[match]}

@defform[(match expr [pattern expr] ...)]{

A @racket[match] form contains one or more ``lines'' that are
surrounded by parentheses or square brackets. Each line contains a
pattern---a description of a value---and an answer @racket[expr].
The initial @racket[expr] is evaluated, and the resulting value
is matched against the pattern in each line, where the lines are
considered in order. The first line that contains a matching pattern
provides an answer @racket[expr] whose value is the result of the
whole @racket[match] expression. This @racket[expr] may reference
identifiers bound in the matching pattern. If none of the lines
contains a matching pattern, it is an error.}

@; ----------------------------------------------------------------------

@section{@racket[when] and @racket[unless]}

@defform[(when expr expr)]{

The first @racket[expr] (known as the ``test'' expression) is
evaluated. If it evaluates to @racket[true], the result of the
@racket[when] expression is the result of evaluating the second
@racket[expr], otherwise the result is @racket[(void)] and the second
@racket[expr] is not evaluated. If the result of evaluating the test
@racket[expr] is neither @racket[true] nor @racket[false], it is an
error.}

@defform[(unless expr expr)]{

Like @racket[when], but the second @racket[expr] is evaluated when the
first @racket[expr] produces @racket[false] instead of @racket[true].}

@; ----------------------------------------

@section[#:tag "advanced-prim-ops"]{Primitive Operations}

@prim-op-defns['(lib "htdp-advanced.rkt" "lang") #'here '()]

@; ----------------------------------------------------------------------

@section[#:tag "advanced-unchanged"]{Unchanged Forms}

@deftogether[(
@defform[(local [definition ...] expr)]
@defform[(letrec ([id expr-for-let] ...) expr)]
@defform[(let* ([id expr-for-let] ...) expr)]
)]{

The same as Intermediate's @|intm-local|, @|intm-letrec|, and
@|intm-let*|.}


@deftogether[(
@defform[(cond [expr expr] ... [expr expr])]
@defidform[else]
)]{

The same as Beginning's @|beg-cond|, except that @racket[else] can be
used with @racket[case].}



@defform[(if expr expr expr)]{

The same as Beginning's @|beg-if|.}

@deftogether[(
@defform[(and expr expr expr ...)]
@defform[(or expr expr expr ...)]
)]{

The same as Beginning's @|beg-and| and @|beg-or|.}


@defform[(time expr)]{

The same as Intermediate's @|intm-time|.}


@deftogether[(
@defform[(check-expect expr expr)]
@defform[(check-within expr expr expr)]
@defform*[[(check-error expr expr)
           (check-error expr)]]
@defform[(check-member-of expr expr expr ...)]
@defform[(check-range expr expr expr)]
)]{

The same as Beginning's @|beg-check-expect|, etc.}



@deftogether[(
@defthing[empty empty?]
@defthing[true boolean?]
@defthing[false boolean?]
)]{

Constants for the empty list, true, and false.}

@defform[(require module-path)]{

The same as Beginning's @|beg-require|.}
