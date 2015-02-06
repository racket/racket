#lang racket/load

(define-signature a^ ())

(define-unit a
  (import)
  (export a^))

(define-unit u
  (import a^)
  (export)
  (init-depend a^))

(define later-init-exn?
  (lambda (x)
    (and (exn? x)
         (regexp-match? #rx"depends on initialization of later unit"
                        (exn-message x)))))
                 

(with-handlers ([later-init-exn? void])
  (eval '(define-compound-unit/infer x 
          (import)
          (export)
          (link
           (() u A)
           (([A : a^]) a)))))

(define-compound-unit/infer x
  (import)
  (export)
  (link
   (([A : a^]) a)
   (() u A)))



(define-compound-unit/infer uc
  (import a^)
  (export)
  (link
   (() u)))

(with-handlers ([later-init-exn? void])
  (eval '(define-compound-unit/infer xc 
          (import)
          (export)
          (link
           (() uc A)
           (([A : a^]) a)))))

(define-compound-unit/infer xc 
  (import)
  (export)
  (link
   (([A : a^]) a)
   (() uc A)))
