#lang racket/base
(require racket/lazy-require
         "keywords.rkt")

;; keep and keep as abs. path -- lazy-loaded macros produce references to this
;; must be required via *absolute module path* from any disappearing module
;; (so for consistency etc, require absolutely from all modules)
(require syntax/parse/private/residual)

(lazy-require-syntax
 ;; load macro transformers lazily via identifier
 ;; This module path must also be absolute (not sure why,
 ;; but it definitely breaks on relative module path).
 [syntax/parse/private/parse
  (define-syntax-class
   define-splicing-syntax-class
   define-integrable-syntax-class
   syntax-parse
   syntax-parser
   define/syntax-parse
   syntax-parser/template
   parser/rhs
   define-eh-alternative-set)])

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
