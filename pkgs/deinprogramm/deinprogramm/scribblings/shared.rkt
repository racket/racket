#lang scheme/base

(require scribble/manual)

(provide teachpack)

(define (teachpack tp . name)
  (apply title #:tag tp
         `(,@name ": " ,(filepath (format "~a.ss" tp))
           ,(index (format "~a-Teachpack" tp)))))
