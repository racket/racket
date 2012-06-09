#lang racket

(require 2htdp/universe)

(with-handlers ((exn:fail:contract? 
                 (lambda (x) 
                   (unless (regexp-match "make-package" (exn-message x))
                     (raise x)))))
  (make-package 1 2 3))

(with-handlers ((exn:fail:contract? 
                 (lambda (x) 
                   (unless (regexp-match "make-bundle" (exn-message x))
                     (raise x)))))
  (make-bundle 1 2))

