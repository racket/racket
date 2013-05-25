#lang racket/base
(require (for-syntax racket/base)
         (for-syntax racket/list)
         (for-syntax "labels.rkt")
         (for-syntax "util.rkt")
         (for-syntax "elim-letrec.rkt")
         (for-syntax "anormal.rkt")
         (for-syntax "elim-callcc.rkt")
         (for-syntax "defun.rkt")
         "lang-api.rkt")

(provide (rename-out [lang-module-begin #%module-begin])
         (all-from-out "lang-api.rkt"))

(define-for-syntax anormalize (make-anormal-term elim-letrec-term))

(define-syntax lang-module-begin 
  (make-lang-module-begin 
   make-labeling
   (make-module-case
    (make-define-case
     (lambda (stx)
       (define anf-stx (anormalize stx))
       (define no-callcc-stx (elim-callcc anf-stx))
       (define defun-stx (defun no-callcc-stx))
       defun-stx)))))
