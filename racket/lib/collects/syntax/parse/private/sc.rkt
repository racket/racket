#lang racket/base
(require (for-syntax racket/base
                     racket/lazy-require)
         "keywords.rkt")

;; keep and keep as abs. path -- lazy-loaded macros produce references to this
;; must be required via *absolute module path* from any disappearing module
;; (so for consistency etc, require absolutely from all modules)
(require syntax/parse/private/residual)

(begin-for-syntax
 (lazy-require
  ;; load macro transformers lazily via identifier
  ;; This module path must also be absolute (not sure why,
  ;; but it definitely breaks on relative module path).
  [syntax/parse/private/parse-aux
   (id:define-syntax-class
    id:define-splicing-syntax-class
    id:define-integrable-syntax-class
    id:syntax-parse
    id:syntax-parser
    id:define/syntax-parse
    id:syntax-parser/template
    id:parser/rhs
    id:define-eh-alternative-set)]))
;; FIXME: workaround for phase>0 bug in racket/runtime-path (and thus lazy-require)
;; Without this, dependencies don't get collected.
(require racket/runtime-path (for-meta 2 '#%kernel))
(define-runtime-module-path-index _unused_ 'syntax/parse/private/parse-aux)

(provide define-syntax-class
         define-splicing-syntax-class
         define-integrable-syntax-class
         syntax-parse
         syntax-parser
         define/syntax-parse

         (except-out (all-from-out "keywords.rkt")
                     ~reflect
                     ~splicing-reflect
                     ~eh-var)
         attribute
         this-syntax

         syntax-parser/template
         parser/rhs
         define-eh-alternative-set)

(define-syntaxes (define-syntax-class
                  define-splicing-syntax-class
                  define-integrable-syntax-class
                  syntax-parse
                  syntax-parser
                  define/syntax-parse
                  syntax-parser/template
                  parser/rhs
                  define-eh-alternative-set)
  (let ([tx (lambda (get-id)
              (lambda (stx)
                (syntax-case stx ()
                  [(_ . args)
                   (datum->syntax stx (cons (get-id) #'args) stx)])))])
    (values 
     (tx id:define-syntax-class)
     (tx id:define-splicing-syntax-class)
     (tx id:define-integrable-syntax-class)
     (tx id:syntax-parse)
     (tx id:syntax-parser)
     (tx id:define/syntax-parse)
     (tx id:syntax-parser/template)
     (tx id:parser/rhs)
     (tx id:define-eh-alternative-set))))
