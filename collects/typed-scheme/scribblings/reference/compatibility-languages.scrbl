#lang scribble/manual

@begin[(require "../utils.rkt" scribble/eval scriblib/footnote
                racket/sandbox)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])
                           (only-in racket/base for)
                           racket/list srfi/14
                           version/check))]

@(define the-eval (make-base-eval))
@(the-eval '(require (except-in typed/racket #%top-interaction #%module-begin)))
@(define the-top-eval (make-base-eval))
@(the-top-eval '(require (except-in typed/racket #%module-begin)))

@(define-syntax-rule (ex . args)
   (examples #:eval the-top-eval . args))


@title{Compatibility Languages}

@(defmodulelang*/no-declare (typed/scheme typed/scheme/base typed-scheme))
Typed versions of the @racketmod[scheme] and @racketmod[scheme/base]
languages. The @racketmod[typed-scheme] language is equivalent to the
@racketmod[typed/scheme/base] language.

@(declare-exporting typed/scheme/base typed/scheme typed-scheme
                    #:use-sources 
                    (typed-scheme/typed-scheme
                     typed-scheme/base-env/prims
                     typed-scheme/base-env/extra-procs
                     typed-scheme/base-env/base-types
                     typed-scheme/base-env/base-types-extra))
