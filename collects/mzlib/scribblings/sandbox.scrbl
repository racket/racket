#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/sandbox
                     (only-in scheme/sandbox make-module-evaluator)))

@(begin
  (define-syntax-rule (bind id)
    (begin
     (require (for-label scheme/sandbox))
     (define id (scheme make-evaluator))))
  (bind scheme-make-evaluator))

@mzlib[#:mode title sandbox]

The @schememodname[mzlib/sandbox] library mostly re-exports
@schememodname[scheme/sandbox], but it provides a slightly different
@scheme[make-evaluator] function.

The library re-exports the following bindings:

@schemeblock[
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

Like @scheme-make-evaluator or @scheme[make-module-evaluator], but
with several differences:

@itemize[

 @item{The @scheme[language] argument can be one of a fixed set of
       symbols: @scheme['r5rs], etc. They are converted by adding a
       @scheme[(list 'special ....)] wrapper.}

 @item{If @scheme[requires] starts with @scheme['begin], then each
       element in the remainder of the list is effectively evaluated
       as a prefix to the program. Otherwise, it corresponds to the
       @scheme[#:requires] argument of @|scheme-make-evaluator|.}

 @item{For each of @scheme[language] and @scheme[requires] that starts
       with @scheme['begin], the expressions are inspected to find
       top-level @scheme[require] forms (using symbolic equality to
       detect @scheme[require]), and the @scheme[require]d modules are
       added to the @scheme[#:allow] list for @|scheme-make-evaluator|.}

]}
