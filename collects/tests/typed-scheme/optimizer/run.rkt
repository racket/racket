#lang racket
(require racket/runtime-path)

;; since Typed Scheme's optimizer does source to source transformations,
;; we compare the expansion of automatically optimized and hand optimized
;; modules
(define (read-and-expand file)
  (syntax->datum
   (parameterize ([current-namespace (make-base-namespace)])
     (with-handlers
         ([exn:fail? (lambda (exn)
                       (printf "~a\n" (exn-message exn))
                       #'#f)])
       (expand (with-input-from-file file read-syntax))))))

(define (test gen)
  (let-values (((base name _) (split-path gen)))
    (or (regexp-match ".*~" name) ; we ignore backup files
        (equal? (read-and-expand gen)
                (read-and-expand (build-path base "../hand-optimized/" name)))
        (begin (printf "~a failed\n\n" name)
               #f))))

(define-runtime-path here ".")

(let ((n-failures
       (if (> (vector-length (current-command-line-arguments)) 0)
           (if (test (format "generic/~a.rkt"
                             (vector-ref (current-command-line-arguments) 0)))
               0 1)
           (for/fold ((n-failures 0))
               ((gen (in-directory (build-path here "generic"))))
             (+ n-failures (if (test gen) 0 1))))))
  (unless (= n-failures 0)
    (error (format "~a tests failed." n-failures))))
