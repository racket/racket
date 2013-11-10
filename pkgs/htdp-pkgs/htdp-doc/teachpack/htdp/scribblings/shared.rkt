#lang racket/base

(require scribble/manual 
         scribble/core
         scribble/html-properties)

(provide teachpack
         beginner-require)

(define (teachpack #:svg? [svg? #f] tp . name)
  (apply title #:tag tp
         #:style (if svg? 
                     (style #f (list (render-convertible-as '(svg-bytes png-bytes))))
                     #f)
         `(,@name ": " ,(filepath (format "~a.rkt" tp))
           ,(index (format "~a teachpack" tp)))))

(define-syntax-rule (def-req beg-require)
  (begin
    (require (for-label lang/htdp-beginner))
    (define beg-require (racket require))))
(def-req beginner-require)
