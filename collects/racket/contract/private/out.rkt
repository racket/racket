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
     ;; lifts need to go after lifts for `provide's:
     (define lifts null)
     ;; use `provide/contract' expander:
     (let ([expanded (true-provide/contract stx (lambda (stx)
                                                  (set! lifts (cons stx lifts))))])
       ;; pull out `provide's to return, and lift
       ;; the rest as a module-end declaration:
       (define-values (decls provides)
         (let loop ([expanded expanded])
           (syntax-case expanded (begin provide)
             [(begin expr ...)
              (let ([boths
                     (map (lambda (e)
                            (define-values (ds ps) (loop e))
                            (cons ds ps))
                          (syntax->list #'(expr ...)))])
                (values (apply append (map car boths))
                        (apply append (map cdr boths))))]
             [(provide p ...)
              (values null (syntax->list #'(p ...)))]
             [else
              (values (list expanded) null)])))
       (for ([decl (in-list decls)])
         (syntax-local-lift-module-end-declaration decl))
       (for ([decl (in-list (reverse lifts))])
         (syntax-local-lift-module-end-declaration decl))
       #`(combine-out #,@provides)))))
