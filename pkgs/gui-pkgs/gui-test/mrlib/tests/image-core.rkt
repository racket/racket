#lang racket/base

(require rackunit 
         mrlib/image-core
         (only-in racket/gui/base make-bitmap))

;; just check there is no error
(check-equal? (begin (un/cache-image (make-bitmap 1 1) #t)
                     (void))
              (void))