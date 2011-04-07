#lang scribble/manual
@(require scribble/struct
          scribble/decode
          scribble/eval
	  "utils.rkt"
          (for-label racket/base
                     racket/contract
                     syntax/kerncase
                     unstable/syntax))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/syntax))
@(the-eval '(require (for-syntax racket/base unstable/syntax)))

@title[#:tag "syntax"]{Syntax}

@defmodule[unstable/syntax]

@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@;{----}

@defform[(with-temporaries (temp-id ...) . body)]{

Evaluates @racket[body] with each @racket[temp-id] bound as a pattern
variable to a freshly generated identifier.

@examples[#:eval the-eval
  (with-temporaries (x) #'(lambda (x) x))
]
}

@defproc[(generate-n-temporaries [n exact-nonnegative-integer?])
         (listof identifier?)]{

Generates a list of @racket[n] fresh identifiers.
}

@;{----}

@margin-note{This binding was added by Vincent St-Amour.}
@defproc[(format-unique-id [lctx (or/c syntax? #f)]
                    	   [#:source src (or/c syntax? #f) #f]
                    	   [#:props props (or/c syntax? #f) #f]
                    	   [#:cert cert (or/c syntax? #f) #f]
                    	   [fmt string?]
                    	   [v (or/c string? symbol? identifier? keyword? char? number?)] ...)
         identifier?]{
Like @racket[format-id], but returned identifiers are guaranteed to be unique.
}

@;{----}

@addition{Sam Tobin-Hochstadt}

@defproc[(syntax-map [f (-> syntax? A)] [stxl syntax?] ...) (listof A)]{
Performs @racket[(map f (syntax->list stxl) ...)].

@examples[#:eval the-eval
(syntax-map syntax-e #'(a b c))]
}

@addition[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defform[(syntax-list template ...)]{

This form constructs a list of syntax objects based on the given templates.  It
is equivalent to @scheme[(syntax->list (syntax (template ...)))].

@defexamples[
#:eval (eval/require '(for-syntax racket/base unstable/syntax) 'unstable/syntax)
(with-syntax ([(x ...) (syntax (1 2 3))]) (syntax-list x ...))
]

}

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
#:eval (eval/require '(for-syntax racket/base unstable/syntax) 'unstable/syntax)
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

@section{Syntax Object Source Locations}

@deftogether[(
@defproc[(syntax-source-directory [stx syntax?]) (or/c path? #f)]
@defproc[(syntax-source-file-name [stx syntax?]) (or/c path? #f)]
)]{

These produce the directory and file name, respectively, of the path with which
@scheme[stx] is associated, or @scheme[#f] if @scheme[stx] is not associated
with a path.

@defexamples[
#:eval (eval/require '(for-syntax racket/base unstable/syntax) 'unstable/syntax)
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

@section{Macro Transformers}

@defproc[(redirect-transformer [id identifier?]) (-> syntax? syntax?)]{

Constructs a function that behaves like a rename transformer; it does not
cooperate with @scheme[syntax-local-value] like a rename transformer does, but
unlike a rename transformer it may be used as a function to transform a syntax
object referring to one identifier into a syntax object referring to another.

@defexamples[
#:eval (eval/require '(for-syntax racket/base unstable/syntax) 'unstable/syntax)
((redirect-transformer #'x) #'a)
((redirect-transformer #'y) #'(a b c))
]

}

@defproc[(head-expand [stx syntax?]
                      [stop-list (listof identifier?) null]
                      [intdef-ctx (or/c internal-definitions-context?
                                        (non-empty-listof internal-definitions-context?)
                                        #f)])
         syntax?]{

This function performs head expansion on @scheme[stx].  In other words, it uses
@scheme[local-expand] to expand @scheme[stx] until its head identifier is a core
form (a member of @scheme[(kernel-form-identifier-list)]) or a member of
@scheme[stop-list], or until it can not be expanded further (e.g. due to error).

It is equivalent to @scheme[(local-expand stx (syntax-local-context) (append
stop-ids (kernel-form-identifier-list) intdef-ctx))].

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
#:eval (eval/require '(for-syntax racket/base unstable/syntax) 'unstable/syntax)
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
#:eval (eval/require '(for-syntax racket/base unstable/syntax) 'unstable/syntax)
(define-for-syntax x (list 1 #'(2 3) 4))
(define-syntax (the-many-faces-of-x stx)
  (with-syntax ([x x] [qx (quote-transformer x)])
   #'(list (quote x)
           (quote-syntax x)
           qx)))
(the-many-faces-of-x)
]

}

@close-eval[the-eval]
