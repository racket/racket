#lang scheme/base

(require scribble/struct
         scheme/class)

(provide keep-file)

(define (keep-file file)
  (make-render-element
   (make-element #f (list))
   null
   (lambda (r s i) (send r install-file file))))
