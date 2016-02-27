#lang racket/base

(require "private/unit-syntax.rkt"
         "private/unit-compiletime.rkt")

(provide unit-static-signatures
         unit-static-init-dependencies
         signature-members)

(define (unit-static-signatures name err-stx)
  (parameterize ((error-syntax err-stx))
    (let ((ui (lookup-def-unit name)))
      (values (apply list (unit-info-import-sig-ids ui))
              (apply list (unit-info-export-sig-ids ui))))))

(define (unit-static-init-dependencies name err-stx)
  (parameterize ((error-syntax err-stx))
    (let ((ui (lookup-def-unit name)))
      (unit-info-deps ui))))

(define (signature-members name err-stx)
  (parameterize ((error-syntax err-stx))
    (let ([s (lookup-signature name)])
      (define intro
        (make-relative-introducer name
                                  (car (siginfo-names (signature-siginfo s)))))
      (values 
       ;; extends: 
       (and (pair? (cdr (siginfo-names (signature-siginfo s))))
            (intro (cadr (siginfo-names (signature-siginfo s)))))
       ;; vars
       (map intro
            (apply list (signature-vars s)))
       ;; defined vars
       (map intro (apply list (apply append (map car (signature-val-defs s)))))
       ;; defined stxs
       (map intro (apply list (apply append (map car (signature-stx-defs s)))))))))
