
(module shared mzscheme
  (require-for-syntax (lib "stx.ss" "syntax")
		      (lib "kerncase.ss" "syntax")
		      (lib "struct.ss" "syntax")
		      "include.ss")

  (provide shared)

  (define undefined (letrec ([x x]) x))
  (require (rename mzscheme the-cons cons))

  (define-syntax shared
    (lambda (stx)
      (define make-check-cdr #f)
      ;; Include the implementation.
      ;; See private/shared-body.ss.
      (include (build-path "private" "shared-body.ss")))))
