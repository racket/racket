#lang racket/base

(require "private/unit-syntax.rkt"
         "private/unit-compiletime.rkt")

(provide unit-static-signatures
         signature-members)

(define (unit-static-signatures name err-stx)
  (parameterize ((error-syntax err-stx))
    (let ((ui (lookup-def-unit name)))
      (values (apply list (unit-info-import-sig-ids ui))
              (apply list (unit-info-export-sig-ids ui))))))

(define (signature-members name err-stx)
  (parameterize ((error-syntax err-stx))
    (let ([s (lookup-signature name)])
      (values 
       ;; extends: 
       (and (pair? (cdr (siginfo-names (signature-siginfo s))))
            (cadr (siginfo-names (signature-siginfo s))))
       ;; vars
       (apply list (signature-vars s))
       ;; defined vars
       (apply list (apply append (map car (signature-val-defs s))))
       ;; defined stxs
       (apply list (apply append (map car (signature-stx-defs s))))))))
