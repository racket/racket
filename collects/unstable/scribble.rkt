#lang racket/base

(require scribble/manual unstable/sandbox unstable/planet
         (for-syntax racket/base unstable/planet-syntax))

(define-for-syntax (make-planet-paths stx ids)
  (map (lambda (id) (make-planet-path stx id)) (syntax->list ids)))

(define-syntax (defmodule/this-package stx)

  (define (make-defmodule opt-name locals others)
    (quasisyntax/loc stx
      (defmodule
        #,(make-planet-path stx opt-name)
        #:use-sources
        [#,@(make-planet-paths stx locals) #,@others])))

  (syntax-case stx ()
    [(_) (make-defmodule #f #'() #'())]
    [(_ name) (make-defmodule #'name #'() #'())]
    [(_ #:use-sources [local ...] [other ...])
     (make-defmodule #f #'(local ...) #'(other ...))]
    [(_ name #:use-sources [local ...] [other ...])
     (make-defmodule #'name #'(local ...) #'(other ...))]))

(define-syntax (defmodule*/no-declare/this-package stx)

  (define (make-defmodule*/no-declare local-mods other-mods)
    (quasisyntax/loc stx
      (defmodule*/no-declare
        [#,@(make-planet-paths stx local-mods) #,@other-mods])))

  (syntax-case stx ()
    [(_ [local-mod ...] [other-mod ...])
     (make-defmodule*/no-declare #'(local-mod ...) #'(other-mod ...))]))

(define-syntax (declare-exporting/this-package stx)

  (define (make-declare-exporting local-mods other-mods local-srcs other-srcs)
    (quasisyntax/loc stx
      (declare-exporting
       #,@(make-planet-paths stx local-mods) #,@other-mods
       #:use-sources
       [#,@(make-planet-paths stx local-srcs) #,@other-srcs])))

  (syntax-case stx ()
    [(_ [local-mod ...] [other-mod ...])
     (make-declare-exporting #'(local-mod ...) #'(other-mod ...) #'() #'())]
    [(_ [local-mod ...] [other-mod ...]
        #:use-sources
        [local-src ...] [other-src ...])
     (make-declare-exporting #'(local-mod ...) #'(other-mod ...)
                             #'(local-src ...) #'(other-src ...))]))

(define-syntax (schememodname/this-package stx)

  (define (make-schememodname id/f)
    (quasisyntax/loc stx
      (schememodname #,(make-planet-path stx id/f))))

  (syntax-case stx ()
    [(_) (make-schememodname #f)]
    [(_ path) (make-schememodname #'path)]))

(provide defmodule/this-package
         defmodule*/no-declare/this-package
         schememodname/this-package
         declare-exporting/this-package
         this-package-version-symbol
         this-package-in
         make-scribble-evaluator
         make-scribble-module-evaluator)
