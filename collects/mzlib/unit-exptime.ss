(module unit-exptime mzscheme
  (require "private/unit-syntax.ss"
           "private/unit-compiletime.ss")

  (provide unit-static-signatures
           signature-members)

  (define (unit-static-signatures name err-stx)
    (parameterize ((error-syntax err-stx))
      (let ((ui (lookup-def-unit name)))
        (values (apply list-immutable (unit-info-import-sig-ids ui))
                (apply list-immutable (unit-info-export-sig-ids ui))))))

  (define (signature-members name err-stx)
    (parameterize ((error-syntax err-stx))
      (let ([s (lookup-signature name)])
        (values 
         ;; extends: 
         (and (pair? (cdr (siginfo-names (signature-siginfo s))))
              (cadr (siginfo-names (signature-siginfo s))))
         ;; vars
         (apply list-immutable (signature-vars s))
         ;; defined vars
         (apply list-immutable (apply append (map car (signature-val-defs s))))
         ;; defined stxs
         (apply list-immutable (apply append (map car (signature-stx-defs s)))))))))
