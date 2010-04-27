#lang scheme/base
(require (for-syntax scheme/base)
         (for-syntax mzlib/etc)
         (for-syntax mzlib/list)
         (for-syntax "lang/labels.ss")
         (for-syntax "lang/util.ss")
         (for-syntax "lang/elim-letrec.ss")
         (for-syntax "lang/anormal.ss")
         (for-syntax "lang/elim-callcc.ss")
         (for-syntax "lang/defun.ss")
         "lang/lang-api.ss")

(provide (rename-out [lang-module-begin #%module-begin])
         (all-from-out "lang/lang-api.ss"))

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
