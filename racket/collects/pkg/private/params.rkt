#lang racket/base
(require racket/path
         setup/dirs)

(provide (all-defined-out))

(define current-pkg-scope
  (make-parameter 'user (lambda (p)
                          (if (path? p)
                              (simple-form-path p)
                              p))))
(define current-pkg-scope-version
  (make-parameter (get-installation-name)))
(define current-pkg-lookup-version
  (make-parameter (version)))
(define current-pkg-error
  (make-parameter (lambda args (apply error 'pkg args))))
(define current-no-pkg-db
  (make-parameter #f))
(define current-pkg-catalogs
  (make-parameter #f))

(define current-pkg-download-cache-dir
  (make-parameter #f))
(define current-pkg-download-cache-max-files
  (make-parameter #f))
(define current-pkg-download-cache-max-bytes
  (make-parameter #f))

(define current-pkg-trash-max-packages
  (make-parameter #f))
(define current-pkg-trash-max-seconds
  (make-parameter #f))

