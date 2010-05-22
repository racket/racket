#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme unstable/cce/syntax))

@title[#:style 'quiet #:tag "cce-syntax"]{Syntax Objects}

@defmodule[unstable/cce/syntax]

This module provides tools for macro transformers.

@section{Contracts}

@defproc[(syntax-datum/c [datum/c any/c]) flat-contract?]{

Recognizes syntax objects @scheme[stx] such that @scheme[(syntax->datum stx)]
satisfies @scheme[datum/c].

}

@defproc[(syntax-listof/c [elem/c any/c]) flat-contract?]{

Recognizes syntax objects @scheme[stx] such that @scheme[(syntax->list stx)]
satisfies @scheme[(listof elem/c)].

}

@defproc[(syntax-list/c [elem/c any/c] ...) flat-contract?]{

Recognizes syntax objects @scheme[stx] such that @scheme[(syntax->list stx)]
satisfies @scheme[(list/c elem/c ...)].

}

@section{Syntax Lists}

@defform[(syntax-list template ...)]{

This form constructs a list of syntax objects based on the given templates.  It
is equivalent to @scheme[(syntax->list (syntax (template ...)))].

@defexamples[
#:eval (evaluator 'unstable/cce/syntax)
(with-syntax ([(x ...) (syntax (1 2 3))]) (syntax-list x ...))
]

}

@defproc[(syntax-map [f (-> syntax? A)] [stx syntax?]) (listof A)]{

Performs @scheme[(map f (syntax->list stx))].

@defexamples[
#:eval (evaluator 'unstable/cce/syntax)
(syntax-map syntax-e #'(a (b c) d))
]

}

@section{Syntax Conversions}

@defproc[(to-syntax [datum any/c]
                    [#:stx stx (or/c false/c syntax?) #f]
                    [#:src src src/c stx]
                    [#:ctxt ctxt (or/c false/c syntax?) stx]
                    [#:prop prop (or/c false/c syntax?) stx]
                    [#:cert cert (or/c false/c syntax?) stx])
         syntax?]{

A wrapper for @scheme[datum->syntax] with keyword arguments.

The "master" keyword @scheme[#:stx] sets all attributes from a single syntax
object, defaulting to @scheme[#f] for unadorned syntax objects.

The individual keywords @scheme[#:src], @scheme[#:ctxt], @scheme[#:prop], and
@scheme[#:cert] override @scheme[#:stx] for individual syntax object
attributes.  They control source src information, lexical context
information, syntax object properties, and syntax certificates, respectively.

@defexamples[
#:eval (evaluator 'unstable/cce/syntax)
(define blank-stx (to-syntax 'car))
blank-stx
(syntax-e blank-stx)
(free-identifier=? blank-stx #'car)
(define full-stx (to-syntax 'car #:stx #'here))
full-stx
(syntax-e full-stx)
(free-identifier=? full-stx #'car)
(define partial-stx (to-syntax 'car #:ctxt #'here))
partial-stx
(syntax-e partial-stx)
(free-identifier=? partial-stx #'car)
]

}

@defproc[(to-datum [x any/c]) (not/c syntax?)]{

A wrapper for @scheme[syntax->datum].  Produces @scheme[(syntax->datum x)] if
@scheme[x] is a syntax object and @scheme[x] otherwise.

@defexamples[
#:eval (evaluator 'unstable/cce/syntax)
(to-datum #'(a b c))
(to-datum (list #'a #'b #'c))
]

}

@section{Source Locations [Deprecated]}

@subsection{Source Location Representations}

@defthing[src/c flat-contract?]{

This contract recognizes various representations of source locations, including
@scheme[srcloc] structures and those accepted by @scheme[datum->syntax]: syntax
objects, source location lists, source location vectors, and @scheme[#f].

}

@deftogether[(
@defproc[(src->srcloc [loc src/c] ...) srcloc?]
@defproc[(src->syntax [loc src/c] ...) syntax?]
@defproc[(src->list [loc src/c] ...)
         (list/c any/c
                 (or/c exact-positive-integer? #f)
                 (or/c exact-nonnegative-integer? #f)
                 (or/c exact-nonnegative-integer? #f)
                 (or/c exact-positive-integer? #f))]
@defproc[(src->vector [loc src/c] ...)
         (vector/c any/c
                   (or/c exact-positive-integer? #f)
                   (or/c exact-nonnegative-integer? #f)
                   (or/c exact-nonnegative-integer? #f)
                   (or/c exact-positive-integer? #f))]
)]{

These functions combine multiple source locations and convert them to a specific
format.  If all provided source locations come from the same source, the result
is a source location from the same source that spans all the lines, columns, and
positions included in the originals.  If no source locations are provided, or
locations from different sources are provided, the result is a source location
with no information (@scheme[#f] for source, line, column, position, and span).

@defexamples[
#:eval (evaluator 'unstable/cce/syntax)
(src->srcloc (datum->syntax #f null (list 'source 2 3 4 5)))
(src->syntax (make-srcloc 'source 2 3 4 5))
(src->list (list 'source 2 3 4 5) (vector 'source 6 7 8 9))
(src->vector)
]

}

@defproc[(src-known? [loc src/c]) boolean?]{

Reports whether @scheme[loc] has any non-@scheme[#f] fields.

@defexamples[
#:eval (evaluator 'unstable/cce/syntax)
(src-known? (list #f #f #f #f #f))
(src-known? (list 'source #f #f #f #f))
(src-known? (list 'source 1 2 3 4))
]

}

@subsection{Syntax Object Source Locations}

@deftogether[(
@defproc[(syntax-source-directory [stx syntax?]) (or/c path? #f)]
@defproc[(syntax-source-file-name [stx syntax?]) (or/c path? #f)]
)]{

These produce the directory and file name, respectively, of the path with which
@scheme[stx] is associated, or @scheme[#f] if @scheme[stx] is not associated
with a path.

@defexamples[
#:eval (evaluator 'unstable/cce/syntax)
(define loc
  (list (build-path "/tmp" "dir" "somewhere.ss")
        #f #f #f #f))
(define stx1 (datum->syntax #f 'somewhere loc))
(syntax-source-directory stx1)
(syntax-source-file-name stx1)
(define stx2 (datum->syntax #f 'nowhere #f))
(syntax-source-directory stx2)
(syntax-source-directory stx2)
]

}

@deftogether[(
@defproc[(syntax-source-planet-package [stx syntax?])
         (or/c (list/c string?
                       string?
                       exact-nonnegative-integer?
                       exact-nonnegative-integer?)
                #f)]
@defproc[(syntax-source-planet-package-owner [stx syntax?]) (or/c string? #f)]
@defproc[(syntax-source-planet-package-name [stx syntax?]) (or/c string? #f)]
@defproc[(syntax-source-planet-package-major [stx syntax?])
         (or/c exact-nonnegative-integer? #f)]
@defproc[(syntax-source-planet-package-minor [stx syntax?])
         (or/c exact-nonnegative-integer? #f)]
@defproc[(syntax-source-planet-package-symbol
          [stx syntax?]
          [text (or/c text? #f) #f])
         (or/c symbol? #f)]
)]{

These functions extract the planet package with which @scheme[stx] is
associated, if any, based on its source location information and the currently
installed set of planet packages.  They produce, respectively, the planet
package s-expression, its owner, name, major version number, minor version
number, or a symbol corresponding to a @scheme[planet] module path.  They each
produce @scheme[#f] if @scheme[stx] is not associated with a planet package.

@defexamples[
#:eval (evaluator 'unstable/cce/syntax)
(define loc
  (list (build-path (current-directory) "file.ss")
        #f #f #f #f))
(define stx (datum->syntax #f 'stx loc))
(syntax-source-planet-package stx)
(syntax-source-planet-package-owner stx)
(syntax-source-planet-package-name stx)
(syntax-source-planet-package-major stx)
(syntax-source-planet-package-minor stx)
(syntax-source-planet-package-symbol stx)
(syntax-source-planet-package-symbol stx "there")
]

}

@defproc[(make-planet-path [stx syntax?] [id (or/c identifier? #f)]) syntax?]{

Constructs a syntax object representing a require spec for the planet package
from which @scheme[stx] arises, with suffix @scheme[id] (if any).

@defexamples[
#:eval (evaluator 'unstable/cce/syntax)
(define loc
  (list (build-path (current-directory) "file.ss")
        #f #f #f #f))
(define stx (datum->syntax #f 'stx loc))
(make-planet-path stx #f)
(make-planet-path stx #'there)
]

}

@section{Macro Transformers}

@defproc[(redirect-transformer [id identifier?]) (-> syntax? syntax?)]{

Constructs a function that behaves like a rename transformer; it does not
cooperate with @scheme[syntax-local-value] like a rename transformer does, but
unlike a rename transformer it may be used as a function to transform a syntax
object referring to one identifier into a syntax object referring to another.

@defexamples[
#:eval (evaluator 'unstable/cce/syntax)
((redirect-transformer #'x) #'a)
((redirect-transformer #'y) #'(a b c))
]

}

@defproc[(head-expand [stx syntax?] [stop-list (listof identifier?)]) syntax?]{

This function performs head expansion on @scheme[stx].  In other words, it uses
@scheme[local-expand] to expand @scheme[stx] until its head identifier is a core
form (a member of @scheme[(full-kernel-form-identifier-list)]) or a member of
@scheme[stop-list], or until it can not be expanded further (e.g. due to error).

It is equivalent to @scheme[(local-expand stx (syntax-local-context) (append
stop-ids (full-kernel-form-identifier-list) #f))].

}

@defproc[(full-kernel-form-identifier-list) (listof identifier?)]{

This function produces the full list of identifiers that may be found in fully
expanded code produced by @scheme[expand], @scheme[local-expand], and related
functions.  It is similar to @scheme[kernel-form-identifier-list], except that
in prior versions of PLT Scheme that excluded module top-level forms from the
list, this function includes them.

@defexamples[
#:eval (evaluator 'unstable/cce/syntax)
(full-kernel-form-identifier-list)
]

}

@defproc[(trampoline-transformer
          [f (-> (-> syntax? void?) (-> syntax? syntax?) syntax? syntax?)])
         (-> syntax? syntax?)]{

Produces a transformer that can emit multiple results during macro expansion, to
be spliced together via @scheme[begin].  This can be useful for compound
expansion that relies on transformer definitions, as well as on expansion state
that is difficult to marshall.

Specifically, @scheme[f] is invoked with three arguments.  The first is the
function used to emit intermediate results (other than the last one).  The
second applies the @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{syntax mark} used for the entire
expansion; @scheme[syntax-local-introduce] will not be reliable during this
process.  The third is the syntax object to expand.

@defexamples[
#:eval (evaluator '(for-syntax unstable/cce/syntax))
(define-syntax magic-begin
  (trampoline-transformer
   (lambda (emit intro stx)
     (syntax-case stx ()
       [(_ term ...)
        (let loop ([terms (syntax->list #'(term ...))])
          (cond
           [(null? terms) #'(begin)]
           [(null? (cdr terms)) (car terms)]
           [else
            (printf "Presto: ~s!\n"
                    (syntax->datum (car terms)))
            (emit (car terms))
            (loop (cdr terms))]))]))))
(magic-begin
 (define x 1)
 (define y 2)
 (+ x y))
]

}

@defproc[(quote-transformer [x any/c]) syntax?]{

Produces a syntax object representing an expression that reconstructs @scheme[x]
when executed, including faithfully reconstructing any syntax objects contained
in @scheme[x].  Note that @scheme[quote] normally converts syntax objects to
non-syntax data, and @scheme[quote-syntax] does the opposite.

@defexamples[
#:eval (evaluator '(for-syntax unstable/cce/syntax))
(define-for-syntax x (list 1 #'(2 3) 4))
(define-syntax (the-many-faces-of-x stx)
  (with-syntax ([x x] [qx (quote-transformer x)])
   #'(list (quote x)
           (quote-syntax x)
           qx)))
(the-many-faces-of-x)
]

}

@section{Syntax Errors}

@defthing[current-syntax (parameter/c (or/c syntax? false/c))]{
A parameter that may be used to store the current syntax object being
transformed.  It is not used by the expander; you have to assign to it yourself.
This parameter is used by @scheme[syntax-error], below.  It defaults to
@scheme[#f].
}

@defproc[(syntax-error [stx syntax?] [fmt string?] [arg any/c] ...) none/c]{
Raises a syntax error based on the locations of @scheme[(current-syntax)] and
@scheme[stx], with @scheme[(format fmt arg ...)] as its message.
@defexamples[
#:eval (evaluator 'unstable/cce/syntax)
(define stx #'(a b c))
(parameterize ([current-syntax #f])
  (syntax-error stx "~s location" 'general))
(parameterize ([current-syntax stx])
  (syntax-error (car (syntax-e stx)) "~s location" 'specific))
]
}

@section{Pattern Bindings}

This package re-exports @scheme[with-syntax*] from
@schememodname[unstable/syntax].
