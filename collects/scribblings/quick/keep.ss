#lang scheme/base

(require scribble/struct
         scheme/class)

(provide keep-file)

(define (keep-file file)
  (make-delayed-element
   (lambda (render part ri)
     (send render install-file file)
     null)
   (lambda () 0)
   (lambda () (make-element #f (list)))))
