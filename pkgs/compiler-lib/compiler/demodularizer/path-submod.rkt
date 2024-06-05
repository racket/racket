#lang racket/base

(provide path/submod-join
         path/submod-path
         path/submod-submod
         path/submod->module-path
         path/submod->resolved-module-path)

(define (path/submod-join p submod)
  (if (null? submod)
      p
      (cons p submod)))

;; returns a path or symbol
(define (path/submod-path p)
  (if (pair? p)
      (car p)
      p))

;; returns alist of symbols, possibly an empty list
(define (path/submod-submod p)
  (if (pair? p)
      (cdr p)
      null))

(define (path/submod->module-path p)
  (define path (path/submod-path p))
  (define submod (path/submod-submod p))
  (define m-path (if (path? path) path `(file ,path)))
  (if (null? submod)
      m-path
      `(submod ,m-path ,@submod)))

(define (path/submod->resolved-module-path path/submod)
  (define raw-path (path/submod-path path/submod))
  (define submod (path/submod-submod path/submod))
  (define path (if (string? raw-path) (string->path raw-path) raw-path))
  (make-resolved-module-path (if (null? submod)
                                 path
                                 (cons path submod))))
