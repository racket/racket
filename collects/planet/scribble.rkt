#lang racket/base

(provide
  this-package-in
  racketmod/this-package
  racketmodname/this-package
  racketmodlink/this-package
  defmodule/this-package
  defmodulelang/this-package
  defmodulereader/this-package
  defmodule*/this-package
  defmodulelang*/this-package
  defmodulereader*/this-package
  defmodule*/no-declare/this-package
  defmodulelang*/no-declare/this-package
  defmodulereader*/no-declare/this-package
  declare-exporting/this-package)

(require
  scribble/manual
  planet/version
  (for-label
    racket/base)
  (for-syntax
    racket/base
    syntax/parse
    planet/syntax))

(define-syntax-rule (define-syntaxes-with [name ...] body ...)
  (define-syntaxes [name ...] (let () body ... (values name ...))))

(define-syntaxes-with
  [ racketmod/this-package
    racketmodname/this-package
    racketmodlink/this-package
    defmodule/this-package
    defmodulelang/this-package
    defmodulereader/this-package
    defmodule*/this-package
    defmodulelang*/this-package
    defmodulereader*/this-package
    defmodule*/no-declare/this-package
    defmodulelang*/no-declare/this-package
    defmodulereader*/no-declare/this-package
    declare-exporting/this-package ]

  (define-syntax-class id/this-package
    #:attributes [planet-id]
    (pattern (~and src (~datum main))
      #:attr planet-id
      (datum->syntax
        #'src
        (syntax-source-planet-package-symbol #'src #f)
        #'src))
    (pattern suffix:id
      #:attr planet-id
      (datum->syntax
        #'suffix
        (syntax-source-planet-package-symbol #'suffix #'suffix)
        #'suffix)))

  (define-splicing-syntax-class (maybe-option kw)
    #:attributes [(option 1)]
    (pattern (~seq
               (~and key:keyword
                 (~fail #:unless (eq? (syntax-e #'key) kw)))
               arg:expr)
      #:attr (option 1) (list #'key #'arg))
    (pattern (~seq) #:attr (option 1) (list)))

  (define-splicing-syntax-class maybe-sources
    #:attributes [(option 1)]
    (pattern (~seq #:use-sources [src:id/this-package ...])
      #:attr (option 1) (list #'#:use-sources #'[(planet src.planet-id) ...]))
    (pattern (~seq) #:attr (option 1) (list)))

  (define racketmod/this-package
    (syntax-parser
      [(_ filename lang:id/this-package . body)
       #:declare filename (maybe-option '#:file)
       (with-syntax ([spec (syntax/loc #'lang
                             (code:line planet lang.planet-id))])
         #'(racketmod filename.option ... spec . body))]))

  (define racketmodname/this-package
    (syntax-parser #:literals [unsyntax]
      [(~and orig (_ (unsyntax e:expr)))
       #'(racketmodname
           (unsyntax `(planet ,(make-planet-symbol (quote-syntax orig) e))))]
      [(_ suffix:id/this-package)
       #'(racketmodname (planet suffix.planet-id))]))

  (define racketmodlink/this-package
    (syntax-parser
      [(_ suffix:id/this-package . body)
       #'(racketmodlink (planet suffix.planet-id) . body)]))

  (define defmodule/this-package
    (syntax-parser
      [(_ req-form suffix:id/this-package src:maybe-sources . body)
       #:declare req-form (maybe-option '#:require-form)
       #'(defmodule
           req-form.option ...
           (planet suffix.planet-id)
           src.option ...
           . body)]))

  (define defmodule*/this-package
    (syntax-parser
      [(_ req-form [suffix:id/this-package ...] src:maybe-sources . body)
       #:declare req-form (maybe-option '#:require-form)
       #'(defmodule*
           req-form.option ...
           [(planet suffix.planet-id) ...]
           src.option ...
           . body)]))

  (define defmodule*/no-declare/this-package
    (syntax-parser
      [(_ req-form [suffix:id/this-package ...] . body)
       #:declare req-form (maybe-option '#:require-form)
       #'(defmodule*/no-declare
           req-form.option ...
           [(planet suffix.planet-id) ...]
           . body)]))

  (define defmodulelang/this-package
    (syntax-parser
      [(_ suffix:id/this-package #:module-paths [path:id/this-package ...]
         src:maybe-sources
         . body)
       #'(defmodulelang (racket (code:line planet suffix.planet-id))
           #:module-paths [(planet path.planet-id) ...]
           src.option ...
           . body)]
      [(_ suffix:id/this-package src:maybe-sources . body)
       #'(defmodulelang (racket (code:line planet suffix.planet-id))
           #:module-paths [(planet suffix.planet-id)]
           src.option ...
           . body)]))

  (define defmodulelang*/this-package
    (syntax-parser
      [(_ [suffix:id/this-package ...] #:module-paths [path:id/this-package ...]
         src:maybe-sources
         . body)
       #'(defmodulelang* [(racket (code:line planet suffix.planet-id)) ...]
           #:module-paths [(planet path.planet-id) ...]
           src.option ...
           . body)]
      [(_ [suffix:id/this-package ...] src:maybe-sources . body)
       #'(defmodulelang* [(racket (code:line planet suffix.planet-id)) ...]
           #:module-paths [(planet suffix.planet-id) ...]
           src.option ...
           . body)]))

  (define defmodulelang*/no-declare/this-package
    (syntax-parser
      [(_ [suffix:id/this-package ...] #:module-paths [path:id/this-package ...]
         . body)
       #'(defmodulelang*/no-declare
           [(racket (code:line planet suffix.planet-id)) ...]
           #:module-paths [(planet path.planet-id) ...]
           . body)]
      [(_ [suffix:id/this-package ...] . body)
       #'(defmodulelang*/no-declare
           [(racket (code:line planet suffix.planet-id)) ...]
           #:module-paths [(planet suffix.planet-id) ...]
           . body)]))

  (define defmodulereader/this-package
    (syntax-parser
      [(_ suffix:id/this-package src:maybe-sources . body)
       #'(defmodulereader (planet suffix.planet-id) src.option ... . body)]))

  (define defmodulereader*/this-package
    (syntax-parser
      [(_ [suffix:id/this-package ...] src:maybe-sources . body)
       #'(defmodulereader* [(planet suffix.planet-id) ...] src.option ...
           . body)]))

  (define defmodulereader*/no-declare/this-package
    (syntax-parser
      [(_ [suffix:id/this-package ...] . body)
       #'(defmodulereader*/no-declare [(planet suffix.planet-id) ...] . body)]))

  (define declare-exporting/this-package
    (syntax-parser
      [(_ suffix:id/this-package ... src:maybe-sources)
       #'(declare-exporting (planet suffix.planet-id) ... src.option ...)])))
