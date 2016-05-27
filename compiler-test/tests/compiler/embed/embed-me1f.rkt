#lang scheme/base

(require scheme/runtime-path)

;; Check that relative paths are preserved:
(define-runtime-path f1 "embed-me1f1.rktl")
(define-runtime-path f2 "sub/embed-me1f2.rktl")

(with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
  (lambda () (parameterize ([current-namespace (make-base-namespace)])
               (load f1)))
  #:exists 'append)
