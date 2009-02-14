#lang scheme/base

(require (only-in "extra-typed.ss" symbolic-compare?))

(provide (all-defined-out))
             
;; FIXME: handle for-template and for-label
;; extract-provided-vars : syntax -> (listof syntax[identifier])
(define (extract-provided-vars stx)
  (syntax-case* stx (rename struct all-from all-from-except all-defined-except) symbolic-compare?
    [identifier
     (identifier? (syntax identifier))
     (list (syntax identifier))]
    
    [(rename local-identifier export-identifier) 
     (list (syntax local-identifier))]
    
    ;; why do I even see this?!?
    [(struct struct-identifier (field-identifier ...))
     null]
    
    [(all-from module-name) null] 
    [(all-from-except module-name identifier ...)
     null]
    [(all-defined-except identifier ...)
     (syntax->list #'(identifier ...))]
    [_ 
     null]))


;; trim-require-prefix : syntax -> syntax
(define (trim-require-prefix require-spec)
  (syntax-case* require-spec (only prefix all-except prefix-all-except rename) symbolic-compare?
    [(only module-name identifer ...)
     (syntax module-name)]
    [(prefix identifier module-name) 
     (syntax module-name)]
    [(all-except module-name identifer ...)
     (syntax module-name)]
    [(prefix-all-except module-name identifer ...)
     (syntax module-name)]
    [(rename module-name local-identifer exported-identifer)
     (syntax module-name)]
    [_ require-spec]))