#lang scheme/base

(require (only-in syntax/module-reader wrap-read-all)
         "../private/readtable.ss")
(provide (rename-out [*read read]
                     [*read-syntax read-syntax]))

(define (*read in)
  (wrap in read))

(define (*read-syntax src in)
  (wrap in (lambda (in)
             (read-syntax src in))))

(define (wrap in read)
  (with-r6rs-reader-parameters
   (lambda ()
     (wrap-read-all 'r6rs in read))))
