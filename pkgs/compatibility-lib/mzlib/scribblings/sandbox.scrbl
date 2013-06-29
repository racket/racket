#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/sandbox
                     (only-in racket/sandbox make-module-evaluator)))

@(begin
   (define-syntax-rule (bind id)
     (begin
       (require (for-label racket/sandbox))
       (define id (racket make-evaluator))))
   (bind racket-make-evaluator))

@mzlib[#:mode title sandbox]

@deprecated[@racketmodname[racket/sandbox]]{}

The @racketmodname[mzlib/sandbox] library mostly re-exports
@racketmodname[racket/sandbox], but it provides a slightly different
@racket[make-evaluator] function.

The library re-exports the following bindings:

@racketblock[
sandbox-init-hook
sandbox-reader
sandbox-input
sandbox-output
sandbox-error-output
sandbox-propagate-breaks
sandbox-coverage-enabled
sandbox-namespace-specs
sandbox-override-collection-paths
sandbox-security-guard
sandbox-path-permissions
sandbox-network-guard
sandbox-make-inspector
sandbox-eval-limits
kill-evaluator
break-evaluator
set-eval-limits
put-input
get-output
get-error-output
get-uncovered-expressions
call-with-limits
with-limits
exn:fail:resource?
exn:fail:resource-resource
]

@defproc*[([(make-evaluator [language (or/c module-path? 
                                            (one-of/c 'r5rs 'beginner 'beginner-abbr 
                                                      'intermediate 'intermediate-lambda 'advanced)
                                            (list/c (one-of/c 'special) symbol?)
                                            (list/c (one-of/c 'special) symbol?)
                                            (cons/c (one-of/c 'begin) list?))]
                            [requires (or/c (cons/c 'begin list?)
                                            (listof (or/c module-path? path?)))]
                            [input-program any/c] ...)
            (any/c . -> . any)]
           [(make-evaluator [module-decl (or/c syntax? pair?)])
            (any/c . -> . any)])]{

Like @racket-make-evaluator or @racket[make-module-evaluator], but
with several differences:

@itemize[

 @item{The @racket[language] argument can be one of a fixed set of
       symbols: @racket['r5rs], etc. They are converted by adding a
       @racket[(list 'special ....)] wrapper.}

 @item{If @racket[requires] starts with @racket['begin], then each
       element in the remainder of the list is effectively evaluated
       as a prefix to the program. Otherwise, it corresponds to the
       @racket[#:requires] argument of @|racket-make-evaluator|.}

 @item{For each of @racket[language] and @racket[requires] that starts
       with @racket['begin], the expressions are inspected to find
       top-level @racket[require] forms (using symbolic equality to
       detect @racket[require]), and the @racket[require]d modules are
       added to the @racket[#:allow] list for @|racket-make-evaluator|.}

]}
