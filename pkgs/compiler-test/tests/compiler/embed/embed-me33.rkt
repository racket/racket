#lang racket/base
(require syntax/modread)

;; Read and run a `#lang at-exp racket/base` program

(parameterize ([current-module-declare-name
                (make-resolved-module-path 'dynamic-module)])
  (eval
   (check-module-form
    (with-module-reading-parameterization
      (lambda ()
        (read-syntax #f (open-input-string "#lang at-exp racket/base @define[x]{33} (provide x)"))))
    'ignored
    #f)))

(printf "This is ~a.\n" (dynamic-require ''dynamic-module 'x))
