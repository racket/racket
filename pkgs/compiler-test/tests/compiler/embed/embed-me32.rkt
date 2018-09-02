#lang racket/base
(require syntax/modread)

;; Read and run a `#lang racket/base` program

(parameterize ([current-module-declare-name
                (make-resolved-module-path 'dynamic-module)])
  (eval
   (check-module-form
    (with-module-reading-parameterization
      (lambda ()
        (read-syntax #f (open-input-string "#lang racket/base (define x 32) (provide x)"))))
    'ignored
    #f)))

(printf "This is ~a.\n" (dynamic-require ''dynamic-module 'x))
