#lang racket/base

(require racket/port racket/contract)

(provide (all-defined-out))

(struct contract-profile
  (total-time n-samples n-contract-samples
   ;; (pairof blame? profile-sample)
   ;; samples taken while a contract was running
   live-contract-samples
   ;; (listof blame?)
   ;; all the blames that were observed during sampling
   all-blames
   ;; profile?
   ;; regular time profile
   regular-profile))

(define (samples-time samples)
  (for/sum ([s (in-list samples)])
    (cadr s)))

(define output-file-prefix "tmp-contract-profile-")


;; for testing. don't generate output files
(define dry-run? (make-parameter #f))

(define-syntax-rule (with-output-to-report-file file body ...)
  (if (dry-run?)
      (parameterize ([current-output-port (open-output-nowhere)])
        body ...)
      (with-output-to-file file
        #:exists 'replace
        (lambda () body ...))))

;; for debugging
(define (print-blame b)
  (printf (string-append "#<blame positive=~a\n"
                         "        negative=~a\n"
                         "        contract=~a\n"
                         "        value=~a\n"
                         "        source=~a>\n")
          (blame-positive b) (blame-negative b)
          (blame-contract b) (blame-value b) (blame-source b)))
