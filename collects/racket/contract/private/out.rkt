#lang racket/base
(require (for-syntax racket/base
                     racket/provide-transform)
         "provide.rkt")
(provide contract-out)

(define-syntax contract-out
  (make-provide-pre-transformer
   (lambda (stx modes)
     ;; For now, only work in the base phase of the `contract-out'
     ;; binding. To generalize, we'll need to parameterize `true-provide'
     ;; over the phase where it should match, shift references, and
     ;; shift lifts (by wrapping them with `begin-for-syntax'es).
     (unless (or (null? modes)
                 (and (= 1 (length modes))
                      (zero? (car modes))))
       (raise-syntax-error #f
                           "allowed only in relative phase-level 0"
                           stx))
     
     ;; check for syntax errors
     (true-provide/contract stx #t 'contract-out)
     
     (syntax-case stx ()
       [(_ . args)
        (syntax-local-lift-module-end-declaration 
         #`(provide/contract . args))])
     
     #`(combine-out))))
