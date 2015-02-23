#lang racket
(require racket/runtime-path
         file/gunzip)

(define-runtime-path here ".")

(define files '("input.txt" "dynamic-input.txt"))

(define (copy-input)
  (unless (file-exists? (build-path here "dynamic-input.txt"))
    (gunzip (build-path here "dynamic-input.txt.gz")
            (lambda (file archive-supplied?)
              (build-path here "dynamic-input.txt"))))
  (for ([file (in-list files)])
    (define src (build-path here file))
    (define dest (build-path (current-directory) file))
    (unless (equal? (simplify-path src #t) (simplify-path dest #t))
      (make-file-or-directory-link src dest))))
(define (remove-input)
  (for ([file (in-list files)])
    (define src (build-path here file))
    (define dest (build-path (current-directory) file))
    (unless (equal? (simplify-path src #t) (simplify-path dest #t))
      (delete-file dest))))

(provide copy-input remove-input)
