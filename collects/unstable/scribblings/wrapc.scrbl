#lang scribble/manual
@(require scribble/struct scribble/decode scribble/eval "utils.rkt"
          (for-label racket/base racket/contract unstable/wrapc racket/syntax))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/contract (for-syntax racket/base unstable/wrapc)))

@title[#:tag "wrapc"]{Contracts for macro subexpressions}

This library provides a procedure @racket[wrap-expr/c] for applying
contracts to macro subexpressions.

@defmodule[unstable/wrapc]

@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defproc[(wrap-expr/c [contract-expr syntax?]
                      [expr syntax?]
                      [#:positive pos-blame
                                  (or/c syntax? string? module-path-index?
                                        'from-macro 'use-site 'unknown)
                                  'use-site]
                      [#:negative neg-blame
                                  (or/c syntax? string? module-path-index?
                                        'from-macro 'use-site 'unknown)
                                  'from-macro]
                      [#:name expr-name
                              (or/c identifier? symbol? string? #f) #f]
                      [#:macro macro-name
                               (or/c identifier? symbol? string? #f) #f]
                      [#:context context (or/c syntax? #f) (current-syntax-context)])
         syntax?]{

Returns a syntax object representing an expression that applies the
contract represented by @racket[contract-expr] to the value produced
by @racket[expr].

The contract's positive blame represents the obligations of the
expression being wrapped. The negative blame represents the
obligations of the macro imposing the contract---the ultimate caller
of @racket[wrap-expr/c]. By default, the positive blame is taken as
the module currently being expanded, and the negative blame is
inferred from the definition site of the macro (itself inferred from
the @racket[context] argument). But both blame locations can be
overridden.

Positive and negative blame locations are determined from
@racket[pos-blame] and @racket[neg-blame], respectively, as follows:
@itemize[
@item{If the argument is a string, it is used directly as the blame
label.}
@item{If the argument is syntax, its source location is used
to produce the blame label.}
@item{If the argument is a module path index, its resolved module path
is used.}
@item{If the argument is @racket['from-macro], the macro is inferred
from either the @racket[macro-name] argument (if @racket[macro-name]
is an identifier) or the @racket[context] argument, and the module
where it is @emph{defined} is used as the negative blame location. If
neither an identifier @racket[macro-name] nor a @racket[context]
argument is given, the location is @racket["unknown"].}
@item{If the argument is @racket['use-site], the module being
expanded is used.}
@item{If the argument is @racket['unknown], the blame label is
@racket["unknown"].}
]

The @racket[macro-name] argument is used to determine the macro's
binding, if it is an identifier. If @racket[expr-name] is given,
@racket[macro-name] is also included in the contract error message. If
@racket[macro-name] is omitted or @racket[#f], but @racket[context] is
a syntax object, then @racket[macro-name] is determined from
@racket[context].

If @racket[expr-name] is not @racket[#f], it is used in the contract's
error message to describe the expression the contract is applied to.

The @racket[context] argument is used, when necessary, to infer the
macro name for the negative blame party and the contract error
message. The @racket[context] should be either an identifier or a
syntax pair with an identifer in operator position; in either case,
that identifier is taken as the macro ultimately requesting the
contract wrapping.

@examples[#:eval the-eval
(define-syntax (myparameterize1 stx)
  (syntax-case stx ()
    [(_ ((p v)) body)
     (with-syntax ([cp (wrap-expr/c
                        #'parameter? #'p
                        #:name "the parameter argument"
                        #:context stx)])
       #'(parameterize ((cp v)) body))]))
(myparameterize1 ((current-input-port
                   (open-input-string "(1 2 3)")))
  (read))
(myparameterize1 (('whoops 'something))
  'whatever)

(module mod racket
  (require (for-syntax unstable/wrapc))
  (define-syntax (app stx)
    (syntax-case stx ()
      [(app f arg)
       (with-syntax ([cf (wrap-expr/c
                          #'(-> number? number?)
                          #'f
                          #:name "the function argument"
                          #:context stx)])
         #'(cf arg))]))
  (provide app))
(require 'mod)
(app add1 5)
(app add1 'apple)
(app (lambda (x) 'pear) 5)
]
}

@close-eval[the-eval]
