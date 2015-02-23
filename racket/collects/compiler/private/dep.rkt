#lang racket/base
(require setup/main-collects
         racket/string)

(provide external-dep?
         indirect-dep?
         collects-relative-dep?
         dep->path
         dep->module-path
         dep->encoded-path)

(define (external-dep? s)
  (define (ext? s) (and (pair? s) (eq? 'ext (car s))))
  (or (ext? s)
      (and (indirect-dep? s)
           (ext? (cdr s)))))

(define (indirect-dep? s)
  (and (pair? s) (eq? 'indirect (car s))))

(define (collects-relative-dep? s)
  (let ([s (dep->encoded-path s)])
    (and (pair? s)
         (eq? 'collects (car s)))))

(define (dep->path s)
  (main-collects-relative->path (dep->encoded-path s)))

(define (dep->module-path s)
  ;; Assumes `collects-relative-dep?`
  (define path-strs (map bytes->string/utf-8 (cdr (dep->encoded-path s))))
  `(lib ,(string-join path-strs "/")))

(define (dep->encoded-path s)
  (let* ([s (if (indirect-dep? s)
                (cdr s)
                s)]
         [s (if (external-dep? s)
                (cdr s)
                s)])
    s))

