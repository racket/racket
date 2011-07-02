#lang racket/base

(require scribble/manual)

(provide teachpack
         beginner-require)

(define (teachpack tp . name)
  (apply title #:tag tp
         `(,@name ": " ,(filepath (format "~a.ss" tp))
           ,(index (format "~a teachpack" tp)))))

(define-syntax-rule (def-req beg-require)
  (begin
    (require (for-label lang/htdp-beginner))
    (define beg-require (racket require))))
(def-req beginner-require)
