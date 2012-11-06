#lang scribble/doc
@(require "common.rkt"
          scribble/eval
          (for-label mzlib/compat))

@(define compat-eval (make-base-eval))

@interaction-eval[#:eval compat-eval (require mzlib/compat)]

@mzlib[#:mode title compat]

The @racketmodname[mzlib/compat] library defines a number of
procedures and syntactic forms that are commonly provided by other
Scheme implementations. Most of the procedures are aliases for
@racketmodname[mzscheme] procedures.

@deftogether[(
@defproc[(=? [n number?] ...+) boolean?]
@defproc[(<? [n real?] ...+) boolean?]
@defproc[(>? [n real?] ...+) boolean?]
@defproc[(<=? [n real?] ...+) boolean?]
@defproc[(>=? [n real?] ...+) boolean?]
)]{

Same as @racket[=], @racket[<], etc.}

@deftogether[(
@defproc[(1+ [n number?]) number?]
@defproc[(1- [n number?]) number?]
)]{

Same as @racket[add1] and @racket[sub1].}


@defproc[(gentmp [base (or/c string? symbol?) "g"]) symbol?]{

Same as @racket[gensym].}


@defproc[(flush-output-port [o output-port? (current-output-port)]) void?]{

Same as @racket[flush-output].}

@defproc[(real-time) exact-integer?]{

Same as @racket[current-milliseconds].}


@defproc[(atom? [v any/c]) any]{

Same as @racket[(not (pair? v))] (which does not actually imply an
atomic value).}


@defform*[[(define-structure (name-id field-id ...))
           (define-structure (name-id field-id ...) 
                             ((init-field-id init-expr) ...))]]{

Like @racket[define-struct], except that the @racket[name-id] is moved
inside the parenthesis for fields. In addition,
@racket[init-field-id]s can be specified with automatic initial-value
expression.

The @racket[init-field-id]s do not have corresponding arguments for
the @racketidfont{make-}@racket[name-id] constructor. Instead, each
@racket[init-field-id]'s @racket[init-expr] is evaluated to obtain the
field's value when the constructor is called. The @racket[field-id]s
are bound in @racket[init-expr]s, but not other
@racket[init-field-id]s.

@examples[
#:eval compat-eval
(define-structure (add left right) ([sum (+ left right)]))
(add-sum (make-add 3 6))
]}

@deftogether[(
@defproc[(getprop [sym symbol?][property symbol?][default any/c #f]) any/c]
@defproc[(putprop [sym symbol?][property symbol?][value any/c]) void?]
)]{

The @racket[getprop] function gets a property value associated with
@racket[sym].  The @racket[property] argument names the property to be
found. If the property is not found, @racket[default] is returned.

The properties obtained with @racket[getprop] are the ones installed
with @racket[putprop].}


@defproc[(new-cafe [eval-handler (any/c . -> . any) #f]) any]{

Emulates Chez Scheme's @racket[new-cafe] by installing
@racket[eval-handler] into the @racket[current-eval] parameter while
running @racket[read-eval-print]. In addition, @racket[current-exit]
is set to escape from the call to @racket[new-cafe].}


@close-eval[compat-eval]
