#lang scribble/doc
@(require "common.rkt"
          scribble/eval
          (for-label mzlib/compat))

@(define compat-eval (make-base-eval))

@interaction-eval[#:eval compat-eval (require mzlib/compat)]

@mzlib[#:mode title compat]

The @schememodname[mzlib/compat] library defines a number of
procedures and syntactic forms that are commonly provided by other
Scheme implementations. Most of the procedures are aliases for
@schememodname[mzscheme] procedures.

@deftogether[(
@defproc[(=? [n number?] ...+) boolean?]
@defproc[(<? [n real?] ...+) boolean?]
@defproc[(>? [n real?] ...+) boolean?]
@defproc[(<=? [n real?] ...+) boolean?]
@defproc[(>=? [n real?] ...+) boolean?]
)]{

Same as @scheme[=], @scheme[<], etc.}

@deftogether[(
@defproc[(1+ [n number?]) number?]
@defproc[(1- [n number?]) number?]
)]{

Same as @scheme[add1] and @scheme[sub1].}


@defproc[(gentmp [base (or/c string? symbol?) "g"]) symbol?]{

Same as @scheme[gensym].}


@defproc[(flush-output-port [o output-port? (current-output-port)]) void?]{

Same as @scheme[flush-output].}

@defproc[(real-time) exact-integer?]{

Same as @scheme[current-milliseconds].}


@defproc[(atom? [v any/c]) any]{

Same as @scheme[(not (pair? v))] (which does not actually imply an
atomic value).}


@defform*[[(define-structure (name-id field-id ...))
           (define-structure (name-id field-id ...) 
                             ((init-field-id init-expr) ...))]]{

Like @scheme[define-struct], except that the @scheme[name-id] is moved
inside the parenthesis for fields. In addition,
@scheme[init-field-id]s can be specified with automatic initial-value
expression.

The @scheme[init-field-id]s do not have corresponding arguments for
the @schemeidfont{make-}@scheme[name-id] constructor. Instead, each
@scheme[init-field-id]'s @scheme[init-expr] is evaluated to obtain the
field's value when the constructor is called. The @scheme[field-id]s
are bound in @scheme[init-expr]s, but not other
@scheme[init-field-id]s.

@examples[
#:eval compat-eval
(define-structure (add left right) ([sum (+ left right)]))
(add-sum (make-add 3 6))
]}

@deftogether[(
@defproc[(getprop [sym symbol?][property symbol?][default any/c #f]) any/c]
@defproc[(putprop [sym symbol?][property symbol?][value any/c]) void?]
)]{

The @scheme[getprop] function gets a property value associated with
@scheme[sym].  The @scheme[property] argument names the property to be
found. If the property is not found, @scheme[default] is returned.

The properties obtained with @scheme[getprop] are the ones installed
with @scheme[putprop].}


@defproc[(new-cafe [eval-handler (any/c . -> . any) #f]) any]{

Emulates Chez Scheme's @scheme[new-cafe] by installing
@scheme[eval-handler] into the @scheme[current-eval] parameter while
running @scheme[read-eval-print]. In addition, @scheme[current-exit]
is set to escape from the call to @scheme[new-cafe].}

