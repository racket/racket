#lang racket/base

(require racket/list
         racket/syntax
         "private/unit/exptime/signature.rkt"
         "private/unit/exptime/unit-infer.rkt"
         "private/unit/exptime/util.rkt")

(provide unit-static-signatures
         unit-static-init-dependencies
         signature-members)

(define (unit-static-signatures name err-stx)
  (parameterize ((current-syntax-context err-stx))
    (let ((ui (lookup-def-unit name)))
      (values (apply list (unit-info-import-sig-ids ui))
              (apply list (unit-info-export-sig-ids ui))))))

(define (unit-static-init-dependencies name err-stx)
  (parameterize ((current-syntax-context err-stx))
    (let ((ui (lookup-def-unit name)))
      (unit-info-deps ui))))

(define (signature-members name err-stx)
  (parameterize ((current-syntax-context err-stx))
    (let ([s (lookup-signature name)])
      (define intro (make-signature-member-introducer s name))
      (values 
       ;; extends: 
       (and (pair? (cdr (siginfo-names (signature-siginfo s))))
            (intro (cadr (siginfo-names (signature-siginfo s)))))
       ;; vars
       (map intro (signature-vars s))
       ;; defined vars
       (map intro (append-map car (signature-val-defs s)))
       ;; defined stxs
       (map intro (append-map car (signature-stx-defs s)))))))
