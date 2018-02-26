#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../namespace/core.rkt")

(provide stop-ids->all-stop-ids
         module-expand-stop-ids)

;; ----------------------------------------

(define (stop-ids->all-stop-ids stop-ids phase)
  (cond
   [(null? stop-ids) stop-ids]
   [else
    (define p-core-stx (syntax-shift-phase-level core-stx phase))
    (cond
     [(and (= 1 (length stop-ids))
           (free-identifier=? (car stop-ids)
                              (datum->syntax p-core-stx 'module*)
                              phase
                              phase))
      stop-ids]
     [else (append stop-ids
                   (for/list ([sym (in-list auto-stop-syms)])
                     (datum->syntax p-core-stx sym)))])]))

(define auto-stop-syms '(begin quote set! lambda case-lambda let-values letrec-values
                         if begin0 with-continuation-mark letrec-syntaxes+values
                         #%app #%expression #%top #%variable-reference))

;; ----------------------------------------

(define (module-expand-stop-ids phase)
  (define p-core-stx (syntax-shift-phase-level core-stx phase))
  (for/list ([sym (in-list module-stop-syms)])
    (datum->syntax p-core-stx sym)))

(define module-stop-syms (append auto-stop-syms
                                 '(define-values define-syntaxes begin-for-syntax
                                    #%require #%provide module module* #%declare
                                    #%stratified-body)))
