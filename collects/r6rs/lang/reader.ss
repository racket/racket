#lang scheme/base

(require (only-in syntax/module-reader wrap-read-all)
         "../private/readtable.ss")
(provide (rename-out [*read read]
                     [*read-syntax read-syntax]))

(define (*read in)
  (wrap in read #f #f #f #f #f))

(define (*read-syntax src in modpath line col pos)
  (wrap in (lambda (in)
             (read-syntax src in))
        modpath src line col pos))

(define (wrap in read modpath src line col pos)
  (with-r6rs-reader-parameters
   (lambda ()
     (wrap-read-all 'r6rs in read modpath src line col pos))))
