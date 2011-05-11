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

@margin-note{This binding was added by Vincent St-Amour.}
@defproc[(format-unique-id [lctx (or/c syntax? #f)]
                    	   [fmt string?]
                    	   [v (or/c string? symbol? identifier? keyword? char? number?)] ...
                    	   [#:source src (or/c syntax? #f) #f]
                    	   [#:props props (or/c syntax? #f) #f]
                    	   [#:cert cert (or/c syntax? #f) #f])
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

@close-eval[the-eval]
